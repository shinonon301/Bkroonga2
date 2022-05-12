unit IndexMail;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
	ShellAPI, System.JSON, System.SyncObjs, NetEncoding, mylogger, BeckyAPI,
	System.Generics.Collections, System.Zip, System.RegularExpressions, groongarequest,
	bgconsole;

(*
	[フォルダのインデクシング]
		・idxファイルを読み込む
		・DBから mailid,messageid,inreplytoのリストを読み込む
		・idxにあって、DBにないmessageidのメールをインデクシングする、
		  または、idx&DBにあってinreplytoが異なったらインデクシングする(ただしoutboxフォルダ関係の場合はインデクシングしない)
			・新規の場合
			  普通にインデクシングする
			・同じMessageIdが存在した場合
			  Mailsのinreplytoを書き換え、MailId、mdateを更新する
		・idxになくてDBにある場合、MailIdsからメールIDを削除し、Mailsのmdateを更新する
		  →ここではMailsからは削除しない
		・インデクシングする形式
		  load Mails
		  [
			{
			  _key:"idxのmessage-id",
			  subject:"メールのsubject",
			  date:"idxのdate",
			  mdate:"現在日時",
			  from:"メールのfrom",
			  to:"メールのto",
			  cc:"メールのcc",
			  bcc:"メールのbcc",
			  inreplyto:"idxのin-reply-to"
			  body:"textパートの本文"
			  quote:"textパートの引用文"
			  attach:"添付ファイル"
			  mailid:[
				{
				  dir:"メールIDから?の手前まで",
				  idx:メールIDの?のあとの16進数を数値化,
				  msgid:"idxのmessage-id",   ←逆検索できるように
				  attr:["idxからメールAttribute(inbox,outbox,trash,flag,tome,ccme)"]
				}
			  ]
			}
		  ]
	[メールのパース方法]
		・先頭から読んでいって、Content-Type,Content-Transfer-Encoding,
		  Content-Dispositonがあったら控える
		・空行を読んだら、マルチパートならboundaryを読み込む
			→マルチパートなし
			  bodyを取得する
			→マルチパートあり
			  Content-Type,Content-Transfer-Encoding,Content-Dispositionを読んで繰り返し
		・bodyについて
			・base64,quoted-printableは先にデコードする
			・text/*のときはcharsetを見てiso-2022-jp,euc-jpならsjisにデコードしてから、
			  utf-8にエンコードする
			・それ以外はfilenameを取得して、拡張子ごとにxdoc2txtを駆使してbodyを得る
				・zipはファイル名の羅列をbodyとする
				・xdoc2txtの対応外ファイルはファイル名をbodyとする
			・ファイル名がないtext/*はbodyかquoteに入れる
				・quoteはBecky!の引用符号がある行を使う
			・ファイル名があるパートはファイル名とともにattachに追記していく
	[その他]
		・folder.iniに[Bkroonga2]セクションを作り、フォルダIDを保存しておく。
		  フォルダのインデクシングをする際、フォルダ名の変更を検知したら、MailIdsの
		  _keyを全て書き換える(古い_keyを削除して新しい_key)
		・Mailsにmailidが空で、かつdateがあって(dateがない場合はin-reply-toにのみ記載が
		  あるメール)、かつmdateが1日以上前のメールがあったら削除する。1日猶予を与えている
		  のは、他のフォルダで同じmessage-idのメールが見つかったときの登録コストをなくすため
          いずれにせよ、MailIdsから消えているメールは検索されない。
*)

type
	TGrnFolIndex = record
		id: DWORD;          // MailIdsの_id
		mailid: DWORD;		// MailIdsのidx
		msgid: String;      // MailIdsのmsgid
		irt: String;		// MailIdsのmsgid.inreplyto
	end;
	TUpdateProc = record
		kind: Integer;
		i: Integer;
		j: Integer;
	end;
	TIndexMail = class(TThread)
	private
		FQueue: TQueue<String>;		// フォルダ移動履歴キュー。他スレッドから書き込まれる
		Flst: TStringList;	 	  	// フォルダ移動履歴。FQueueから更新される。これを元にインデックスが行われる
		ArndLst: TStringList;		// フォルダ巡回
		FMutex: TMutex;				// Flstを排他保護するMutex(たぶん必要ないはず)
		fcnt: Integer;				// インデックスを開始するタイマー
		fallcnt: Integer;			// 全インデックスを開始するタイマー
		fgrnport: Integer;
		grnreq: TGroongaRequest;
		procedure CheckQueue;
		function GetTempFilename(ext: AnsiString): AnsiString;
		procedure SetMessageText(s: String);
		function RawTrimLeft(s: RawByteString): RawByteString;
		function IsQuote(s: String): Boolean;
		function DivideBodyQuote(body: UTF8String; var quote: UTF8String): UTF8String;
		function GetLine(var str: RawByteString; headerflg: Boolean): RawByteString;
		function GetLineParam(line, param: String): String;
		procedure DelFile(fn: AnsiString);
		procedure BodyOut(mails: TJSONObject; body: RawByteString; ctype, cenc, cdisp: String);
		function Xdoc(src, ext: AnsiString; ctype: String): RawByteString;
		function UpdateFolder(fol: String): Integer;
		procedure DeleteEmptyMails;
		procedure CheckEmptyDirs;
		procedure ReadIndexed(fol: String; var idx: TArray<TGrnFolIndex>);
		procedure AddMailParam(json: TJSONObject; idx: TIdxLine);
		procedure IndexingMail(fol: String; idxline: TIdxLine);
		procedure CorrectMails(idx: TIdxLine);
		procedure DeleteMailIdsFromMails(msgid: String; id: DWORD);
		procedure AddMailIdsToMails(id: DWORD; msgid: String);
		function GetMailIdListFromMails(msgid: String): TJSONArray;
		procedure DeleteFolderAllMailIds(fol: String);
		procedure ChangeFolderMailIds(oldfol, newfol: String);
		procedure ChangeFolderOld2New(oldfol, newfol: String);
		function IsIgnoreMbx(fol: String): Boolean;
		function IsIgnoreFolder(fol: String): Boolean;
		procedure UpdateAllFolder;
		procedure UpdateAllFolderDig(fol: String);
		function IsIndexingFolder(fol: String): Boolean;
		function GetDirAge(fol: String): DWORD;
		procedure SetDirAge(fol: String; age: DWORD);
        function Wait100ms(cnt: Integer): Boolean;
	public
		constructor Create(CreateSuspended: Boolean; grnport: Integer = 10083);
		destructor Destroy; override;
		procedure Execute; override;
		procedure AddFol(fol: String);
		procedure Pending;
		procedure ParseMail(src: RawByteString; json: TJsonObject);
	end;

const
	MaxText = (128*1024);
	MaxAttach = (256*1024);

var
	indexing: TIndexMail;

implementation

{ TIndexMail }

constructor TIndexMail.Create(CreateSuspended: Boolean; grnport: Integer = 10083);
begin
	logger.info(self.ClassName, 'Create');
	fcnt := 0;
	fallcnt := 0;
	fgrnport := grnport;
	inherited Create(CreateSuspended);
	self.FreeOnTerminate := False;
end;

destructor TIndexMail.Destroy;
begin
	logger.info(self.ClassName, 'Destroy');
	if Assigned(FMutex) then FreeAndNil(FMutex);
	if Assigned(ArndLst) then FreeAndNil(ArndLst);
	if Assigned(Flst) then FreeAndNil(Flst);
	if Assigned(FQueue) then FreeAndNil(FQueue);
	if Assigned(grnreq) then FreeAndNil(grnreq);
	inherited;
end;

procedure TIndexMail.Execute;
begin
	try
		logger.info(self.ClassName, 'Execute');
		grnreq := TGroongaRequest.Create(fgrnport);
		FQueue := TQueue<String>.Create;
		Flst := TStringList.Create;
		Flst.Duplicates := dupIgnore;
		ArndLst := TStringList.Create;
		ArndLst.Duplicates := dupIgnore;
		FMutex := TMutex.Create(False);
		while not self.Terminated do begin
			CheckQueue;
			if (fcnt >= 30) and (Flst.Count > 0) then begin
				UpdateFolder(Flst[0]);
				FMutex.Acquire;
				fallcnt := 0;
				FMutex.Release;
				Flst.Delete(0);
				if Wait100ms(1) then Break;
			end else begin
				if fcnt < 100 then begin
					FMutex.Acquire;
					fcnt := fcnt + 1;
					FMutex.Release;
				end;
				if Wait100ms(1) then Break;
			end;
			if Flst.Count = 0 then begin
				if fallcnt < 6000 then begin
					FMutex.Acquire;
					fallcnt := fallcnt + 1;
					FMutex.Release;
					if fallcnt >= 300 then begin	// 1分(600)だと定期チェックと同期してしまうかもしれないのでとりあえず30秒にしている
						if ArndLst.Count = 0 then begin
							DeleteEmptyMails;
							if Wait100ms(10) then Break;
							CheckEmptyDirs;
							if Wait100ms(10) then Break;
							UpdateAllFolder;
							if Wait100ms(10) then Break;
						end else begin
							UpdateFolder(ArndLst[0]);
							ArndLst.Delete(0);
							if Wait100ms(2) then Break;
							if ArndLst.Count = 0 then begin
								FMutex.Acquire;
								fallcnt := 0;
								FMutex.Release;
							end;
						end;
					end;
				end;
			end else begin
				FMutex.Acquire;
				fallcnt := 0;
                FMutex.Release;
			end;
		end;
		logger.info(self.ClassName, 'Execute Loop End');
		self.ReturnValue := 0;
	except on E: Exception do
		logger.error(self.ClassName, Format('Execute %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.AddFol(fol: String);
begin
	logger.info(self.ClassName, 'AddFol '+fol);
	FMutex.Acquire;
	try
		FQueue.Enqueue(fol);
	finally
		FMutex.Release;
	end;
end;

procedure TIndexMail.Pending;
begin
	logger.debug(self.ClassName, 'Pending');
	FMutex.Acquire;
	try
		fcnt := 0;
		fallcnt := 0;
	finally
		FMutex.Release;
	end;
end;

procedure TIndexMail.CheckQueue;
var
	s: String;
begin
	logger.verbose(self.ClassName, 'CheckQueue');
	// キューの中身をチェックしてFlstに追加する
	while FQueue.Count > 0 do begin
		FMutex.Acquire;
		try
			s := FQueue.Extract;
		finally
			FMutex.Release;
		end;
		logger.debug(self.ClassName, 'fqueextract '+s);
		Flst.Add(s);
	end;
end;

procedure TIndexMail.SetMessageText(s: String);
begin
	try
		//logger.verbose(self.ClassName, 'SetMessageText '+s);
		bka.SetMessageText(hwndMain, AnsiString(Format('%s: %s', [AppName, s])));
	except on E: Exception do
		logger.error(self.ClassName, Format('SetMessageText %s %s', [E.Message, E.StackTrace]));
	end;
end;

function TIndexMail.GetTempFilename(ext: AnsiString): AnsiString;
var
	f: File;
	fn, path: AnsiString;
begin
	Result := '';
	try
		fn := bka.GetTempFileName(ext);
		if fn <> '' then begin
			AssignFile(f, fn);
			try
				Rewrite(f);
				CloseFile(f);
				Result := fn;
				Exit;
			except
				// ファイル作成できなかったらWinapiで再チャレンジ
			end;
		end;
		SetLength(path, MAX_PATH+2);
		GetTempPathA(MAX_PATH, PAnsiChar(path));
		SetLength(fn, MAX_PATH+2);
		GetTempFileNameA(PAnsiChar(path), PAnsiChar(ext), 0, PAnsiChar(fn));
		fn := PAnsiChar(fn);
		fn := fn + '.' + ext;
		AssignFile(f, fn);
		Rewrite(f);
		CloseFile(f);
		Result := fn;
		Exit;
	except on E: Exception do
		logger.error(self.ClassName, Format('GetTempFilename(%s:%s) %s', [ext, fn, E.Message]));
	end;
end;

function TIndexMail.GetLine(var str: RawByteString;
  headerflg: Boolean): RawByteString;
var
	i, st, ed: Integer;
	line: RawByteString;
begin
	try
		Result := '';
		for i := 1 to Length(str)-1 do begin
			if str[i] = #$0a then begin
				ed := i;
				while (Ord(str[ed]) <= $20) and (ed > 0) do ed := ed - 1;
				if ed > 0 then
					Result := RawByteString(Copy(str, 1, ed))
				else
					Result := '';
				str := RawByteString(Copy(str, i+1, Length(str)));
				if headerflg and (Result <> '') and (Length(str) > 1) then begin
					if (str[1] = #$20) or (str[1] = #9) then begin
						line := GetLine(str, headerflg);
						st := 1;
						while (st < (Length(line) - 1)) and (Ord(line[st]) <= $20) and (Ord(line[st]) <> $1b) do st := st + 1;
						if st >= Length(line)-1 then Exit;
						Result := Result + #$20 + RawByteString(Copy(line, st, Length(line)));
					end;
				end;
				Exit;
			end;
		end;
		Result := str;
		str := '';
	except on E: Exception do
		logger.error(self.ClassName, Format('GetLine %s %s', [E.Message, E.StackTrace]));
	end;
end;

function TIndexMail.GetLineParam(line, param: String): String;
var
	m: TMatch;
begin
	try
		Result := '';
		m := TRegEx.Match(line, param+'=\"(.+)\"', [roIgnoreCase]);
		if m.Success then begin
			Result := m.Groups.Item[1].Value;
			Exit;
		end;
		m := TRegEx.Match(line, param+'=(.+)\s*$', [roIgnoreCase]);
		if m.Success then begin
			Result := m.Groups.Item[1].Value;
			Exit;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('GetLineParam %s %s', [E.Message, E.StackTrace]));
	end;
end;

function TIndexMail.RawTrimLeft(s: RawByteString): RawByteString;
var
	i, st: Integer;
begin
	try
		if Length(s) < 1 then begin
			Result := '';
			Exit;
		end;
		i := 1;
		while (i < Length(s)) and (Ord(s[i]) <= $20) and (Ord(s[i]) <> $1b) do i := i + 1;
		Result := Copy(s, i, Length(s));
	except on E: Exception do
		logger.error(self.ClassName, Format('RawTrimLeft %s %s', [E.Message, E.StackTrace]));
	end;
end;

function TIndexMail.IsQuote(s: String): Boolean;
var
	i: Integer;
begin
	try
		Result := False;
		for i := 1 to Length(s) do
			if (Ord(s[i]) <= $20) or (s[i] = '　') then begin
				continue
			end else begin
				if (s[i] = '>') or (s[i] = '|') or (s[i] = '}') or (s[i] = ':')
					or (s[i] = ')') or (s[i] = '$') or (s[i] = '%') or (s[i] = '〉')
					or (s[i] = '》') or (s[i] = '＞') or (s[i] = '≫') or (s[i] = '｜')
					or (s[i] = '｝') then
						Result := True;
				Exit;
			end;
	except on E: Exception do
		logger.error(self.ClassName, Format('IsQuote %s %s', [E.Message, E.StackTrace]));
	end;
end;

function TIndexMail.DivideBodyQuote(body: UTF8String;
  var quote: UTF8String): UTF8String;
var
	line: UTF8String;
begin
	try
		Result := '';
		while Length(body) > 0 do begin
			line := GetLine(RawByteString(body), False);
			if IsQuote(line) then begin
				if Length(quote) < MaxText then
					quote := quote + line
			end else begin
				if Length(Result) < MaxText then
					Result := Result + line;
			end;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('DivideBodyQuote %s %s', [E.Message, E.StackTrace]));
	end;
end;

function TIndexMail.Xdoc(src, ext: AnsiString; ctype: String): RawByteString;
var
	fn: AnsiString;
	fs: TFileStream;
	xdocproc: TBgconsole;
	ifil: AnsiString;
	m: TMatch;
	body: String;
begin
	try
		logger.debug(self.ClassName, Format('Xdoc srclen=%d,ctype=%s', [Length(src), ctype]));
		Result := '';
		if Length(src) = 0 then Exit;
		if not FileExists(ExtractFilePath(MyDllFileName) + 'xdoc2txt.exe') then begin
			if (ext = 'htm') or (ext = 'html') then begin
				m := TRegEx.Match(src, '<meta.+?content="(.+?)".*?>', [roIgnoreCase]);
				if m.Success then begin
					ctype := m.Groups.Item[1].Value;
				end;
				if TRegEx.IsMatch(ctype, 'charset', [roIgnoreCase]) then begin
					if TRegEx.IsMatch(ctype, '2022', [roIgnoreCase]) then begin
						src := bka.ISO_2022_JP(src, False);
					end else if TRegEx.IsMatch(ctype, 'euc', [roIgnoreCase]) then begin
						src := bka.EUC_JP(src, False);
					end;
				end;
				if TRegEx.IsMatch(ctype, 'charset', [roIgnoreCase]) and (not TRegEx.IsMatch(ctype, 'utf-8', [roIgnoreCase])) then begin
					src := bka.UTF_8(src, True);
				end;
				SetCodePage(RawByteString(src), CP_UTF8, False);
				body := src;
				body := TRegEx.Replace(body, '<body.*?>(.*)</body', '\1', [roIgnoreCase, roSingleLine]);
				body := TRegEx.Replace(body, '<style.+?</style.*?>', '', [roIgnoreCase, roSingleLine]);
				body := TRegEx.Replace(body, '<a\s+(.+?)>', '【\1】', [roIgnoreCase, roSingleLine]);
				body := TRegEx.Replace(body, '<.+?>', '', [roIgnoreCase, roSingleLine]);
				Result := UTF8String(body);
			end else if ext = 'txt' then begin
				src := bka.UTF_8(Copy(src, 1, MaxText), True);
				SetCodePage(RawByteString(src), CP_UTF8, False);
				Result := UTF8String(src);
			end;
			Exit;
		end;
		fn := GetTempFileName(ext);
		try
			fs := TFileStream.Create(fn, fmCreate);
			fs.Write(src[1], Length(src));
		finally
			fs.Free;
		end;
		try
			ifil := '';
			if (ext = 'vsd') or (ext = 'vsdx') then ifil := '-i ';
			xdocproc := TBgconsole.Create(ExtractFilePath(MyDllFileName) + 'xdoc2txt.exe',
				'-8 '+ifil+'-r=0 -o=0 "'+fn+'"',
				True, 30000, MaxAttach);
			if xdocproc.WaitFor = 0 then begin
				Result := xdocproc.ResStr;
				SetCodePage(Result, CP_UTF8, False);
			end;
		finally
			if Assigned(xdocproc) then FreeAndNil(xdocproc);
			if FileExists(fn) then DelFile(fn);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('Xdoc %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.ParseMail(src: RawByteString; json: TJsonObject);
var
	ctype, cenc, cdisp, head, msgid: String;
	body, line, headstr: RawByteString;
	boundary: TArray<String>;
	charset: RawByteString;
	m: TMatch;
	mc: TMatchCollection;
	//contents: TJsonArray;	// jsonに追加するので、jsonが開放されるときに同時に開放される
	mainflg, passflg, mpaltflg: Boolean;
	p: Integer;
begin
	try
		logger.debug(self.ClassName, Format('ParseMail src=%s', [Copy(src, 1, 64)]));
		// メールヘッダ処理
		ctype := '';
		cenc := '';
		cdisp := '';
		headstr := '';
		boundary := [];
		while Length(src) > 0 do begin
			line := GetLine(src, True);
			headstr := headstr + line + #13#10;
			if line = '' then begin
				head := AnsiString(bka.MIMEHeader(headstr, charset, False));
				if head = '' then begin
					logger.error(self.ClassName, 'ParseMail Head1 No Header');
					json.Free;
					json := TJsonObject.Create;
					Exit;
				end;
				m := TRegEx.Match(head, '^subject:\s*(.+)$', [roIgnoreCase, roMultiLine]);
				if m.Success then
					json.AddPair('subject', Trim(m.Groups.Item[1].Value));
				m := TRegEx.Match(head, '^from:\s*(.+)$', [roIgnoreCase, roMultiLine]);
				if m.Success then
					json.AddPair('from', Trim(m.Groups.Item[1].Value));
				m := TRegEx.Match(head, '^to:\s*(.+)$', [roIgnoreCase, roMultiLine]);
				if m.Success then
					json.AddPair('to', Trim(m.Groups.Item[1].Value));
				m := TRegEx.Match(head, '^cc:\s*(.+)$', [roIgnoreCase, roMultiLine]);
				if m.Success then
					json.AddPair('cc', Trim(m.Groups.Item[1].Value));
				m := TRegEx.Match(head, '^bcc:\s*(.+)$', [roIgnoreCase, roMultiLine]);
				if m.Success then
					json.AddPair('bcc', Trim(m.Groups.Item[1].Value));
				if not json.TryGetValue<String>('_key', msgid) then begin
					m := TRegEx.Match(head, '^message-id:\s*(<.+?>)', [roIgnoreCase, roMultiLine]);
					if m.Success then
						json.AddPair('_key', Trim(m.Groups.Item[1].Value));
				end;
				if not Assigned(json.GetValue('inreplyto')) then begin
					m := TRegEx.Match(head, '^in-reply-to:\s*(<.+?>)', [roIgnoreCase, roMultiLine]);
					if m.Success then begin
						json.AddPair('inreplyto', Trim(m.Groups.Item[1].Value));
					end else begin
						m := TRegEx.Match(head, '^references:\s*(.+)$', [roIgnoreCase, roMultiLine]);
						if m.Success then begin
							mc := TRegEx.Matches(m.Groups.Item[1].Value, '<.+?>');
							if mc.Count > 0 then begin
								//strirt := Trim(mc.Item[mc.Count - 1].Value);
								json.AddPair('inreplyto', Trim(mc.Item[mc.Count - 1].Value));
							end;
						end;
					end;
				end;
				m := TRegEx.Match(head, '^content-type:\s*(.+)$', [roIgnoreCase, roMultiLine]);
				if m.Success then
					ctype := Trim(m.Groups.Item[1].Value);
				m := TRegEx.Match(head, '^content-transfer-encoding:\s*(.+)$', [roIgnoreCase, roMultiLine]);
				if m.Success then
					cenc := Trim(m.Groups.Item[1].Value);
				m := TRegEx.Match(head, '^content-disposition:\s*(.+)$', [roIgnoreCase, roMultiLine]);
				if m.Success then
					cdisp := Trim(m.Groups.Item[1].Value);
				break;
			end;
		end;
		if not json.TryGetValue<String>('_key', msgid) then begin
			logger.error(self.ClassName, 'missing _key');
			json.Free;
			json := TJsonObject.Create;
			Exit;
		end;
		//contents := TJsonArray.Create;
		json.AddPair('body', '');
		json.AddPair('quote', '');
		json.AddPair('attach', '');
		// メール本文処理
		if TRegEx.IsMatch(ctype, 'multipart/', [roIgnoreCase]) and TRegEx.IsMatch(ctype, 'boundary=', [roIgnoreCase]) then begin
			// マルチパートメール
			logger.debug(self.ClassName, Format('ParseMail MultipartMail(%s)', [msgid]));
			mainflg := False;
			mpaltflg := TRegEx.IsMatch(ctype, '/alternative', [roIgnoreCase]);
			boundary := boundary + [GetLineParam(ctype, 'boundary')];
			while Length(src) > 0 do begin
				p := Pos(RawByteString('--'+boundary[Length(boundary)-1]), src);
				if p > 0 then begin
					src := RawTrimLeft(Copy(src, p + Length(RawByteString('--'+boundary[Length(boundary)-1])), Length(src)));
					if Copy(src, 1, 2) = '--' then begin
						if Length(boundary) > 1 then begin
							boundary := Copy(boundary, 0, Length(boundary) - 1);
							mpaltflg := False;
							continue;
						end else begin
							Break;
						end;
					end;
				end else begin
					Break;
				end;
				head := '';
				headstr := '';
				while Length(src) > 0 do begin
					line := GetLine(src, True);
					headstr := headstr + line + #13#10;
					if line = '' then begin
						// マルチパートヘッダを読み込む
						passflg := False;
						head := AnsiString(bka.MIMEHeader(headstr, charset, False));
						if head = '' then begin
							logger.error(self.ClassName, 'ParseMail Head2 No Header');
							json.Free;
							json := TJsonObject.Create;
							Exit;
						end;
						ctype := '';
						cenc := '';
						cdisp := '';
						m := TRegEx.Match(head, '^content-type:\s*(.+)$', [roIgnoreCase, roMultiLine]);
						if m.Success then
							ctype := Trim(m.Groups.Item[1].Value);
						m := TRegEx.Match(head, '^content-transfer-encoding:\s*(.+)$', [roIgnoreCase, roMultiLine]);
						if m.Success then
							cenc := Trim(m.Groups.Item[1].Value);
						m := TRegEx.Match(head, '^content-disposition:\s*(.+)$', [roIgnoreCase, roMultiLine]);
						if m.Success then
							cdisp := Trim(m.Groups.Item[1].Value);
						if TRegEx.IsMatch(ctype, 'multipart/', [roIgnoreCase]) and TRegEx.IsMatch(ctype, 'boundary=', [roIgnoreCase]) then begin
							boundary := boundary + [GetLineParam(ctype, 'boundary')];
							mainflg := False;
							mpaltflg := TRegEx.IsMatch(ctype, '/alternative', [roIgnoreCase]);
						end else begin
							p := Pos(RawByteString('--'+boundary[Length(boundary)-1]), src);
							if (not mainflg) and (TRegEx.IsMatch(ctype, 'text/', [roIgnoreCase])) then begin
								// 最初の text/* のパートは必ず本文として読み込む
								mainflg := True;
								passflg := False;
							end else if mainflg and (TRegEx.IsMatch(ctype, 'text/', [roIgnoreCase])) and mpaltflg then begin
								// multipart/alternativeで、かつ、本文を読み込んだあとのtextパートはパスする
								passflg := True;
							end else begin
								// そうでないときは必ず読み込む
								passflg := False;
							end;
							if p > 0 then begin
								body := RawTrimLeft(Copy(src, 1, p - 1));
								src := RawTrimLeft(Copy(src, p, Length(src)));
							end else begin
								body := RawTrimLeft(src);
								src := '';
							end;
							if not passflg then begin
								BodyOut(json, body, ctype, cenc, cdisp);
							end;
						end;
						Break;
					end;
				end;
			end;
		end else begin
			logger.debug(self.ClassName, Format('ParseMail PlaneMail(%s)', [msgid]));
			body := RawTrimLeft(src);
			BodyOut(json, body, ctype, cenc, cdisp);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('ParseMail %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.BodyOut(mails: TJSONObject;
  body: RawByteString; ctype, cenc, cdisp: String);
var
	strm1: TMemoryStream;
	tmpfn1, tmpfn2, fileext, fname: AnsiString;
	fn: RawByteString;
	quote: UTF8String;
	zip: TZipFile;
	i: Integer;
	cnt: TJsonObject;
	s: String;
begin
	try
		logger.debug(self.ClassName, Format('BodyOut %s', [ctype]));
		fname := '';
		quote := '';
		if TRegEx.IsMatch(cenc, 'base64', [roIgnoreCase]) then begin
			strm1 := TMemoryStream.Create;
			tmpfn1 := GetTempFileName('b64');
			tmpfn2 := GetTempFilename('txt');
			try
				strm1.WriteBuffer(body[1], Length(body));
				strm1.SaveToFile(String(tmpfn1));
				if FileExists(String(tmpfn1)) then begin
					bka.B64Convert(tmpfn2, tmpfn1, False);
				end;
				strm1.Clear;
				if FileExists(String(tmpfn2)) then begin
					strm1.LoadFromFile(String(tmpfn2));
					SetLength(body, strm1.Size);
					strm1.ReadBuffer(body[1], strm1.Size);
				end else begin
					body := '';
				end;
			finally
				strm1.Free;
				if FileExists(String(tmpfn1)) then DelFile(String(tmpfn1));
				if FileExists(String(tmpfn2)) then DelFile(String(tmpfn2));
			end;
		end else if TRegEx.IsMatch(cenc, 'quoted', [roIgnoreCase]) then begin
			strm1 := TMemoryStream.Create;
			tmpfn1 := GetTempFilename('txt');
			tmpfn2 := GetTempFilename('txt');
			try
				strm1.WriteBuffer(body[1], Length(body));
				strm1.SaveToFile(tmpfn1);
				if FileExists(tmpfn1) then begin
					bka.QPConvert(tmpfn2, tmpfn1, False);
				end;
				strm1.Clear;
				if FileExists(tmpfn2) then begin
					strm1.LoadFromFile(tmpfn2);
					SetLength(body, strm1.Size);
					strm1.ReadBuffer(body[1], strm1.Size);
				end else begin
					body := '';
				end;
			finally
				strm1.Free;
				if FileExists(tmpfn1) then DelFile(tmpfn1);
				if FileExists(tmpfn2) then DelFile(tmpfn2);
			end;
		end;
		if TRegEx.IsMatch(ctype, 'text/', [roIgnoreCase]) and (not TRegEx.IsMatch(ctype, 'name=', [roIgnoreCase])) then begin
			// textパート
			if TRegEx.IsMatch(ctype, 'text/htm', [roIgnoreCase]) then begin
				body := Xdoc(body, 'htm', ctype);
			end else begin
				if TRegEx.IsMatch(ctype, 'charset', [roIgnoreCase]) then begin
					if TRegEx.IsMatch(ctype, '2022', [roIgnoreCase]) then begin
						body := bka.ISO_2022_JP(body, False);
					end else if TRegEx.IsMatch(ctype, 'euc', [roIgnoreCase]) then begin
						body := bka.EUC_JP(body, False);
					end;
				end;
				if TRegEx.IsMatch(ctype, 'charset', [roIgnoreCase]) and (not TRegEx.IsMatch(ctype, 'utf-8', [roIgnoreCase])) then begin
					body := bka.UTF_8(body, True);
				end;
				SetCodepage(body, CP_UTF8, False);
				// text/plainのときは本文と引用を分ける
				body := DivideBodyQuote(body, quote);
			end;
			SetCodepage(body, CP_UTF8, False);
			if body <> '' then begin
				s := mails.GetValue('body').Value + body;
				mails.RemovePair('body');
				mails.AddPair('body', s);
			end;
			if quote <> '' then begin
				s := mails.GetValue('quote').Value + quote;
				mails.RemovePair('quote');
				mails.AddPair('quote', s);
			end;
		end else begin
			// 添付ファイル
			tmpfn1 := GetLineParam(ctype, 'name');
			if (tmpfn1 = '') or (ExtractFileExt(tmpfn1) = '') then begin
				tmpfn1 := GetLineParam(cdisp, 'filename');
				if tmpfn1 = '' then tmpfn1 := GetLineParam(ctype, 'name');
			end;
			fname := tmpfn1;
			fileext := LowerCase(ExtractFileExt(tmpfn1));
			while Copy(fileext, 1, 1) = '.' do fileext := Copy(fileext, 2, Length(fileext));
			logger.debug(self.ClassName, Format('%s %s %s', [cdisp, fname, fileext]));
			if TRegEx.IsMatch(ctype, 'image/', [roIgnoreCase]) then begin
				if fname = '' then fname := 'imagefile';
				body := UTF8String(ExtractFileName(tmpfn1)) + #13#10;
			end else if (TRegEx.IsMatch(ctype, 'message/rfc', [roIgnoreCase])) or (fileext = 'eml') then begin
				body := Xdoc(body, 'eml', ctype);
				if fname <> '' then body := UTF8String(ExtractFileName(fname)) + #13#10 + body;
			end else if (fileext = 'zip') then begin
				zip := TZipFile.Create;
				strm1 := TMemoryStream.Create;
				try
					try
						zip.Encoding := TEncoding.Default;
						strm1.WriteBuffer(body[1], Length(body));
						zip.Open(strm1, zmRead);
						body := UTF8String(ExtractFileName(fname)) + #13#10;
						for i := 0 to zip.FileCount-1 do begin
							fn := bka.UTF_8(zip.FileName[i], True);
							SetCodePage(fn, CP_UTF8, False);
							body := body + fn + ' ' + UTF8String(IntToStr(zip.FileInfo[i].UncompressedSize)) + #13#10;
						end;
					except on E: Exception do
						body := UTF8String(ExtractFileName(fname)) + #13#10;
					end;
				finally
					strm1.Free;
					zip.Free;
				end;
			end else if (fileext = 'rtf') or (fileext = 'doc') or (fileext = 'xls') or (fileext = 'ppt')
				or (fileext = 'sxw') or (fileext = 'sxc') or (fileext = 'sxi') or (fileext = 'sxd')
				or (fileext = 'odt') or (fileext = 'ods') or (fileext = 'odp') or (fileext = 'odg')
				or (fileext = 'jaw') or (fileext = 'jtw') or (fileext = 'jbw') or (fileext = 'juw')
				or (fileext = 'jfw') or (fileext = 'jvw') or (fileext = 'jtd') or (fileext = 'jtt')
				or (fileext = 'oas') or (fileext = 'oa2') or (fileext = 'oa3') or (fileext = 'bun')
				or (fileext = 'wj2') or (fileext = 'wj3') or (fileext = 'wk3') or (fileext = 'wk4')
				or (fileext = '123') or (fileext = 'wri') or (fileext = 'pdf') or (fileext = 'mht')
				or (fileext = 'html') or (fileext = 'htm') or (fileext = 'txt')
				or (fileext = 'docx') or (fileext = 'xlsx') or (fileext = 'pptx') or (fileext = 'vsd')
				or (fileext = 'docm') or (fileext = 'xlsm') or (fileext = 'xlsxm') or (fileext = 'pptm')
				or (fileext = 'vsdx') then begin
					// 本当はin演算子を使えばもっと見やすいんだろうけど、古いコードをそのまま流用
					body := UTF8String(ExtractFileName(fname)) + #13#10 + Xdoc(body, fileext, ctype);
			end else if fname = '' then begin
				// ここまで来てファイル名がなかったら取り込まない
				Exit;
			end else begin
				body := UTF8String(ExtractFileName(fname)) + #13#10;
			end;
			SetCodepage(body, CP_UTF8, False);
			if body <> '' then begin
				s := mails.GetValue('attach').Value + body;
				mails.RemovePair('attach');
				mails.AddPair('attach', s);
			end;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('BodyOut %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.DelFile(fn: AnsiString);
begin
	try
		if FileExists(fn) then begin
			try
				DeleteFile(fn);
                logger.verbose(self.ClassName, Format('DelFile %s', [fn]));
			except
			end;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('DelFile %s %s %s', [fn, E.Message, E.StackTrace]));
	end;
end;

function TIndexMail.UpdateFolder(fol: String): Integer;
var
	idx: TArray<TIdxLine>;
	idxed: TArray<TGrnFolIndex>;
	proc: TArray<TUpdateProc>;
	i, j: integer;
	folname: AnsiString;
	tmpstr: String;
	p: TUpdateProc;
	age: DWORD;
begin
    Result := 0;
	try
		logger.info(self.ClassName, 'UpdateFolder '+fol);
		if not IsIndexingFolder(fol) then begin
			logger.warn(self.ClassName, Format('UpdateFolder - not index (%s)', [fol]));
			Exit;
		end;
		// フォルダ名変更を検査
		tmpstr := bka.ReadFolIni(fol, 'FolderName');
		if (tmpstr <> '') and (tmpstr <> ExcludeTrailingPathDelimiter(fol)) then
			ChangeFolderMailIds(tmpstr, ExcludeTrailingPathDelimiter(fol));
		bka.WriteFolIni(fol, 'FolderName', ExcludeTrailingPathDelimiter(fol));
		// folder.idxの更新を検査
		age := FileAge(bka.DataFolder+fol+'folder.idx');
		if GetDirAge(fol) = age then begin
			logger.info(self.ClassName, Format('UpdateFolder - not update age (%s)', [fol]));
			Exit;
		end;
	    Result := 1;
		// folder.idxを読み込む
		bka.ReadIdx(fol, idx);
		// インデックス済みのリストを読む
		ReadIndexed(fol, idxed);
		logger.info(self.ClassName, Format('UpdateFolder folder.idx=%d,indexed=%d', [Length(idx), Length(idxed)]));
		i := 0;
		j := 0;
		SetLength(proc, 0);
		while i < Length(idx) do begin
			//logger.debug(self.ClassName, Format('UpdateFolder Loop [%d,%d]', [i, j]));
			if j >= Length(idxed) then begin
				// インデックス済みリストがなくなった
				p.kind := 0; p.i := i; p.j := j; proc := proc + [p];
				Inc(i);
			end else if idx[i].dwMsgID = idxed[j].mailid then begin
				// 既にインデックス済みの同じIDがあった
				if idx[i].strMsgId = idxed[j].msgid then begin
					// 同じメッセージIDなら、InReplyToが異なっていたら書き換える(Outboxフォルダの場合は変えない)
					if (LowerCase(idx[i].strInreplyto) <> LowerCase(idxed[j].irt)) and (Pos('!outbox', LowerCase(fol)) = 0) then begin
						p.kind := 1; p.i := i; p.j := j; proc := proc + [p];
					end;
				end else begin
					// 違うメッセージIDなら、インデックス済みのメッセージIDからMailIdを削除して、新しいMailIdをインデックスする
					p.kind := 2; p.i := i; p.j := j; proc := proc + [p];
					p.kind := 0; p.i := i; p.j := j; proc := proc + [p];
				end;
				// i,jを進める
				Inc(i);
				Inc(j);
			end else if idx[i].dwMsgID < idxed[j].mailid then begin
				// 未インデックスのIDがあった
				p.kind := 0; p.i := i; p.j := j; proc := proc + [p];
				Inc(i);
			end else begin
				// インデックス済みでなくなったIDがあった
				p.kind := 2; p.i := i; p.j := j; proc := proc + [p];
				// jを進める
				Inc(j);
			end;
		end;
		while j < Length(idxed) do begin
			// インデックス済みでなくなったIDがあった
			p.kind := 2; p.i := i; p.j := j; proc := proc + [p];
			// jを進める
			Inc(j);
		end;
		if Terminated then Exit;
		if Length(proc) > 0 then begin
			i := 0;
			folname := bka.GetFolderDisplayName(fol);
			logger.verbose(self.ClassName, Format('UpdateFolder %s proclen=%d', [fol, Length(proc)]));
			try
				while i < Length(proc) do begin
					logger.verbose(self.ClassName, Format('UpdateFolder procloop [%d,%d,%d]', [proc[i].kind, proc[i].i, proc[i].j]));
					SetMessageText(Format('%s [%d/%d]', [folname, i+1, Length(proc)]));
					if proc[i].kind = 0 then
						IndexingMail(fol, idx[proc[i].i])
					else if proc[i].kind = 1 then
						CorrectMails(idx[proc[i].i])
					else if proc[i].kind = 2 then
						DeleteMailIdsFromMails(idxed[proc[i].j].msgid, idxed[proc[i].j].id);
					Inc(i);
					Sleep(1);
					if (proc[i].kind = 0) and Terminated then break;
				end;
			finally
				SetMessageText(Format('%s Finished.', [folname]));
			end;
		end;
		if Terminated then Exit;
		SetDirAge(fol, age);
	except on E: Exception do
		logger.error(self.ClassName, Format('UpdateFolder %s %s', [E.Message, E.StackTrace]));
	end;
end;

function TIndexMail.Wait100ms(cnt: Integer): Boolean;
var
	i: Integer;
begin
	try
        logger.verbose(self.ClassName, Format('Wait100ms %d', [cnt]));
		Result := False;
		for i := 1 to (cnt * 10) do begin
			if self.Terminated then begin
				Result := True;
				Exit;
			end;
			Sleep(10);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('Wait100ms %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.DeleteEmptyMails;
var
	param: TArray<String>;
	dt: Double;
begin
	try
		logger.debug(self.ClassName, 'DeleteEmptyMails');
		dt := bka.DateTimeTotime_t(Now - 1.0);
		param := ['table Mails', 'filter mailid==[]&&date>0&&mdate<'+FloatToStr(dt)];
		grnreq.command('delete', param);
	except on E: Exception do
		logger.error(self.ClassName, Format('DeleteEmptyMails %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.CheckEmptyDirs;
var
	param: TArray<String>;
	fols: TArray<String>;
	cnt: Integer;
	i: Integer;
begin
	try
		logger.debug(self.ClassName, 'CheckEmptyDirs');
		param := ['table Dirs', 'limit 99999', 'output_columns _key'];
		grnreq.command('select', param);
		cnt := grnreq.GetSelectCount;
		for i := 0 to cnt-1 do
			fols := fols + [IncludeTrailingPathDelimiter(bka.Slash2Yen(grnreq.GetSelectValue(i, 0).Value))];
		for i := 0 to Length(fols)-1 do begin
			//logger.debug(self.ClassName, fols[i]);
			if not IsIndexingFolder(fols[i]) then
				DeleteFolderAllMailIds(fols[i]);
			if Terminated then Exit;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('CheckEmptyDirs %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.ReadIndexed(fol: String; var idx: TArray<TGrnFolIndex>);
var
	id: TGrnFolIndex;
	param: TArray<String>;
	i, cnt: Integer;
begin
	try
		logger.debug(self.ClassName, 'ReadIndexed '+fol);
		SetLength(idx, 0);
		param := ['table MailIds',
			'output_columns _id,idx,msgid._key,msgid.inreplyto',
			'filter dir=="'+bka.Yen2Slash(ExcludeTrailingPathDelimiter(fol))+'"',
			MAX_LIMIT, 'sortby idx'];
		grnreq.command('select', param);
		cnt := grnreq.GetSelectCount;
		logger.debug(self.ClassName, Format('%s cnt=%d', ['ReadIndexed', cnt]));
		for i := 0 to cnt-1 do begin
			id.id := StrToInt64Def(grnreq.GetSelectValue(i, 0).Value, 0);
			id.mailid := StrToInt64Def(grnreq.GetSelectValue(i, 1).Value, 0);
			id.msgid := grnreq.GetSelectValue(i, 2).value;
			id.irt := grnreq.GetSelectValue(i, 3).value;
			//logger.debug(self.ClassName, Format('%s %d %d %s %s', ['ReadIndexed', id.id, id.mailid, id.msgid, id.irt]));
			idx := idx + [id];
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('ReadIndexed %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.AddMailParam(json: TJSONObject; idx: TIdxLine);
begin
	try
		json.AddPair('_key', idx.strMsgId);
		json.AddPair('inreplyto', idx.strInreplyto);
		json.AddPair('date', TJSONNumber.Create(idx.tSend));
		json.AddPair('mdate', TJSONNumber.Create(bka.DateTimeTotime_t(Now)));
	except on E: Exception do
		logger.error(self.ClassName, Format('AddMailParam %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.IndexingMail(fol: String; idxline: TIdxLine);
var
	mailid: String;
	src: RawByteString;
	json: TJSONArray;   // Mails Array
	mid: TJSONObject;   // MailIds
	selcnt: DWORD;
	selfrom: String;
begin
	try
		mailid := fol+'?'+LowerCase(IntToHex(idxline.dwMsgID, 1));
		logger.info(self.ClassName, 'IndexingMail '+mailid);
		if not TRegEx.Match(idxline.strMsgId, '<.+>').Success then begin
			logger.warn(self.ClassName, Format('IndexingMail invalid Message-Id(%s)', [idxline.strMsgId]));
			Exit;
		end;
		grnreq.command('select', ['table Mails', 'output_columns _id,from', 'filter _key=="'+idxline.strMsgId+'"']);
		selcnt := grnreq.GetSelectCount;
		if selcnt > 0 then selfrom := grnreq.GetSelectValue(0,1).AsType<String> else selfrom := '';
		json := TJSONArray.Create;
		try
			mid := TJSONObject.Create;	// Freeし忘れ注意
			mid.AddPair('dir', bka.Yen2Slash(ExcludeTrailingPathDelimiter(fol)));
			mid.AddPair('idx', TJSONNumber.Create(idxline.dwMsgID));
			mid.AddPair('msgid', idxline.strMsgId);
			mid.AddPair('attr', TJSONArray.Create);
			if Pos('!inbox', LowerCase(fol)) > 0 then mid.GetValue<TJSONArray>('attr').Add('inbox');
			if Pos('!outbox', LowerCase(fol)) > 0 then mid.GetValue<TJSONArray>('attr').Add('outbox');
			if Pos('!trash', LowerCase(fol)) > 0 then mid.GetValue<TJSONArray>('attr').Add('trash');
			if (idxline.dwStatus and MESSAGE_FLAG) <> 0 then mid.GetValue<TJSONArray>('attr').Add('flag');
			if (idxline.dwStatus and MESSAGE_TOME) <> 0 then mid.GetValue<TJSONArray>('attr').Add('tome');
			if (idxline.dwStatus and MESSAGE_CCME) <> 0 then mid.GetValue<TJSONArray>('attr').Add('ccme');
			mid.AddPair('priority', TJSONNumber.Create(idxline.nPriority));
			mid.AddPair('color', TJSONNumber.Create(idxline.nColor));
			if (selcnt = 0) or ((selcnt > 0) and (selfrom = '')) then begin
				// 同じ Message-Id のメールがない or あっても本文の登録がないので新規登録
				src := bka.GetSource(mailid);
				if Length(src) > 0 then begin
					json.Add(TJSONObject.Create);
					AddMailParam(json.Items[0] as TJSONObject, idxline);
					ParseMail(src, json.Items[0] as TJSONObject);
					(json.Items[0] as TJSONObject).AddPair('mailid', TJSONArray.Create(mid));
					if (json.Items[0] as TJSONObject).GetValue<String>('attach') <> '' then
						mid.GetValue<TJSONArray>('attr').Add('attach');
					//logger.info(self.ClassName, json.ToString);
					grnreq.commandjson('load', ['table Mails'], json);
					//logger.info('', grnreq.Result.ToString);
				end else begin
					// メールソースが取れなかったら異常として何もせずに返る
					logger.error(self.ClassName, 'IndexingMail: Unable to get mail source');
					FreeAndNil(mid);
				end;
			end else begin
				// MailsのメッセージIDにメールIDを追加する
				json.add(mid);
				grnreq.commandjson('load', ['table MailIds', 'output_ids yes'], json);
				if grnreq.GetLoadCount > 0 then
					AddMailIdsToMails(grnreq.GetLoadId, idxline.strMsgId);
			end;
		finally
			FreeAndNil(json);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('IndexingMail %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.CorrectMails(idx: TIdxLine);
var
	json: TJSONArray;
begin
	try
		logger.info(self.ClassName, 'CorrectMails '+idx.strMsgId);
		json := TJSONArray.Create;
		try
			json.Add(TJSONObject.Create);
			AddMailParam(json.Items[0] as TJSONObject, idx);
			grnreq.commandjson('load', ['table Mails'], json);
		finally
			FreeAndNil(json);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('CorrectMails %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.DeleteMailIdsFromMails(msgid: String;
  id: DWORD);
var
	json: TJSONArray;
begin
	try
		logger.info(self.ClassName, Format('DeleteMailIdsFromMails %s %d', [msgid, id]));
		// groonga に vector から指定の要素を削除する関数があれば楽なのに…
		grnreq.command('delete', ['table MailIds', 'id '+IntToStr(id)]);
		json := TJSONArray.Create(TJSONObject.Create(TJSONPair.Create('_key', msgid)));
		try
			(json.Items[0] as TJSONObject).AddPair('mailid', GetMailIdListFromMails(msgid));
			(json.Items[0] as TJSONObject).AddPair('mdate', TJSONNumber.Create(bka.DateTimeTotime_t(Now)));
			grnreq.commandjson('load', ['table Mails'], json);
		finally
			FreeAndNil(json);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('DeleteMailIdsFromMails %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.AddMailIdsToMails(id: DWORD; msgid: String);
var
	json: TJSONArray;
begin
	try
		logger.info(self.ClassName, Format('AddMailIdsFromMails %d %s', [id, msgid]));
		// groonga に vector に要素を追加する関数があれば楽なのに…
		json := TJSONArray.Create(TJSONObject.Create(TJSONPair.Create('_key', msgid)));
		try
			(json.Items[0] as TJSONObject).AddPair('mailid', GetMailIdListFromMails(msgid).Add(id));
			(json.Items[0] as TJSONObject).AddPair('mdate', TJSONNumber.Create(bka.DateTimeTotime_t(Now)));
			grnreq.commandjson('load', ['table Mails'], json);
		finally
			FreeAndNil(json);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('AddMailIdsFromMails %s %s', [E.Message, E.StackTrace]));
	end;
end;

function TIndexMail.GetMailIdListFromMails(msgid: String): TJSONArray;
var
	j: Integer;
begin
	try
		logger.info(self.ClassName, 'GetMailIdListFromMails '+msgid);
		Result := TJSONArray.Create;
		grnreq.command('select', ['table Mails', 'filter _key=="'+msgid+'"', 'output_columns mailid._id,mailid.msgid']);
		if grnreq.GetSelectCount > 0 then
			for j := 0 to TJSONArray(grnreq.GetSelectValue(0, 0)).Count-1 do
				if TJSONArray(grnreq.GetSelectValue(0, 1)).Items[j].Value <> '' then
					Result.Add(TJSONArray(grnreq.GetSelectValue(0, 0)).Items[j].GetValue<DWORD>);
	except on E: Exception do
		logger.error(self.ClassName, Format('GetMailIdListFromMails %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.DeleteFolderAllMailIds(fol: String);
var
	i, max: Integer;
	folname: AnsiString;
	fmsgid: TArray<String>;
	fid: TArray<DWORD>;
begin
	try
		// folのMailIdsを全て削除する
		logger.info(self.ClassName, 'DeleteFolderAllMailIds '+fol);
		grnreq.command('select', ['table MailIds',
			'output_columns _id,msgid._key',
			'filter dir=="'+bka.Yen2Slash(ExcludeTrailingPathDelimiter(fol))+'"',
			MAX_LIMIT, 'sortby _id']);
		max := grnreq.GetSelectCount;
		if max > 0 then begin
			folname := bka.GetFolderDisplayName(fol);
			for i := 0 to max-1 do begin
				fmsgid := fmsgid + [grnreq.GetSelectValue(i,1).AsType<String>];
				fid := fid + [grnreq.GetSelectValue(i,0).AsType<DWORD>];
			end;
			for i := 0 to max-1 do begin
				SetMessageText(Format('%s [%d/%d]', [folname, i+1, max]));
				DeleteMailIdsFromMails(fmsgid[i], fid[i]);
			end;
			SetMessageText(Format('%s Finished.', [folname]));
			grnreq.command('delete', ['table Dirs', 'key '+bka.Yen2Slash(ExcludeTrailingPathDelimiter(fol))]);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('DeleteFolderAllMailIds %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.ChangeFolderMailIds(oldfol, newfol: String);
var
	i: Integer;
	oldfols: TArray<String>;
	s: String;
begin
	try
		// oldfolからnewfolにMailIdsのdirsを変更する
		logger.info(self.ClassName, Format('ChangeFolderMailIds %s->%s', [oldfol, newfol]));
		ChangeFolderOld2New(oldfol, newfol);
		grnreq.command('select', ['table Dirs',
			'query _key:^'+bka.Yen2Slash(ExcludeTrailingPathDelimiter(oldfol))+'/',
			MAX_LIMIT, 'output_columns _key']);
		//logger.debug(self.ClassName, grnreq.Result.ToString);
		for i := 0 to grnreq.GetSelectCount-1 do
			oldfols := oldfols + [ExcludeTrailingPathDelimiter(bka.Slash2Yen(grnreq.GetSelectValue(i, 0).AsType<String>))];
		for s in oldfols do begin
			ChangeFolderOld2New(s, StringReplace(s, oldfol, newfol, [rfReplaceAll, rfIgnoreCase]));
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('ChangeFolderMailIds %s %s', [E.Message, E.StackTrace]));
	end;
end;


procedure TIndexMail.ChangeFolderOld2New(oldfol, newfol: String);
var
	i, len: Integer;
	json: TJSONArray;
	nnewfol: String;
begin
	try
		grnreq.command('select', ['table MailIds',
			'filter dir=="'+bka.Yen2Slash(ExcludeTrailingPathDelimiter(oldfol))+'"',
			MAX_LIMIT, 'output_columns _id']);
		len := grnreq.GetSelectCount;
		json := TJSONArray.Create;
		try
			if len > 0 then begin
				SetMessageText(Format('Change %s->%s', [bka.GetFolderDisplayName(oldfol), bka.GetFolderDisplayName(newfol)]));
				logger.info(self.ClassName, Format('ChangeFolderOld2New %d(%s->%s)', [len, oldfol, newfol]));
				nnewfol := bka.Yen2Slash(ExcludeTrailingPathDelimiter(newfol));
				for i := 0 to len-1 do begin
					json.Add(TJSONObject.Create
						.AddPair('_id', TJSONNumber.Create(grnreq.GetSelectValue(i, 0).AsType<DWORD>))
						.AddPair('dir', nnewfol));
				end;
				grnreq.commandjson('load', ['table MailIds'], json);
				SetMessageText(Format('Finished to change %s', [bka.GetFolderDisplayName(newfol)]));
			end;
			grnreq.command('delete', ['table Dirs', 'key '+bka.Yen2Slash(ExcludeTrailingPathDelimiter(oldfol))]);
		finally
			FreeAndNil(json);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('ChangeFolderOld2New %s %s', [E.Message, E.StackTrace]));
	end;
end;

function TIndexMail.IsIgnoreMbx(fol: String): Boolean;
var
	m: TMatch;
	i: Integer;
begin
	try
		Result := True;
		if Length(gconf.IgnoreMbx) > 0 then begin
			m := TRegEx.Match(fol, '([\dA-Za-z]+\.mb)\\', [roIgnoreCase]);
			for i := 0 to Length(gconf.IgnoreMbx)-1 do
				if m.Groups[1].Value = gconf.IgnoreMbx[i] then Exit;
		end;
		Result := False;
	except on E: Exception do
		logger.error(self.ClassName, Format('IsIgnoreMbx %s %s', [E.Message, E.StackTrace]));
	end;
end;

function TIndexMail.IsIgnoreFolder(fol: String): Boolean;
var
	i: Integer;
begin
	try
		Result := True;
		fol := LowerCase(fol);
		if gconf.bTrash and (Pos('!trash', fol) > 0) then Exit;
		if gconf.bOutbox and (Pos('!outbox', fol) > 0) then Exit;
		if gconf.bInbox and (Pos('!inbox', fol) > 0) then Exit;
		if Length(gconf.Ignore) > 0 then begin
			for i := 0 to Length(gconf.Ignore)-1 do
				if Pos(LowerCase(gconf.Ignore[i]), LowerCase(fol)) > 0 then Exit;
		end;
		Result := False;
	except on E: Exception do
		logger.error(self.ClassName, Format('IsIgnoreFolder %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.UpdateAllFolder;
var
	srec, srecmb: TSearchRec;
	fol: String;
begin
	try
		logger.info(self.ClassName, 'UpdateAllFolder');
		// 全フォルダをUpdate
		if FindFirst(bka.DataFolder+'*.mb', faDirectory, srec) = 0 then begin
			repeat
				if Pos('.mb.bak', LowerCase(srec.Name)) = 0 then
					UpdateAllFolderDig(bka.DataFolder+srec.Name+'\');
			until (FindNext(srec) <> 0) or self.Terminated;
			FindClose(srec);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('UpdateAllFolder %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.UpdateAllFolderDig(fol: String);
var
	srec: TSearchRec;
	name: String;
begin
	try
        Sleep(10);
		if FindFirst(fol+'*', faDirectory, srec) = 0 then begin
			repeat
				if (Pos('.', srec.Name) <> 1) and (Pos('#', srec.Name) <> 1) then begin
					name := Copy(fol+srec.Name+'\', Length(bka.DataFolder)+1, MAX_PATH);
					logger.debug(self.ClassName, 'UpdateAllFolderDig '+name);
					if FileExists(bka.DataFolder+name+'folder.idx') then
						if IsIndexingFolder(name) then begin
							ArndLst.Add(name);
							//AddFol(name);
						end;
					UpdateAllFolderDig(fol+srec.Name+'\');
				end;
			until (FindNext(srec) <> 0) or self.Terminated;
			FindClose(srec);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('UpdateAllFolderDig %s %s', [E.Message, E.StackTrace]));
	end;
end;

function TIndexMail.IsIndexingFolder(fol: String): Boolean;
var
	mb, str: AnsiString;
	m: TMatch;
	v, lfol: String;
begin
	try
		Result := False;
		lfol := LowerCase(fol);
		if Pos('!xreminder', lfol) > 0 then Exit;
		if Pos('!agent', lfol) > 0 then Exit;
		if (Pos('!outbox', lfol) > 0) and (Pos('!draft', lfol) = 0) and (Pos('!sent', lfol) = 0) then Exit;
		if Pos('#tml', lfol) > 0 then Exit;
		if Pos('.mb.bak', lfol) > 0 then Exit;
		if IsIgnoreMbx(fol) then Exit;
		if bka.IsIMAPAccount(fol) then Exit;
		if IsIgnoreFolder(fol) then Exit;
		if not bka.CanRW(bka.DataFolder+fol+'folder.ini') then begin
			//MessageBeep(MB_ICONERROR);
			logger.error(self.ClassName, Format('IsIndexingFolder Cannot R/W (%s)', [fol]));
			Exit;
		end;
		if not FileExists(bka.DataFolder+fol+'folder.idx') then begin
			logger.warn(self.ClassName, Format('IsIndexingFolder Dont found folder.idx (%s)', [fol]));
			Exit;
		end;
		Result := True;
	except on E: Exception do
		logger.error(self.ClassName, Format('IsIndexingFolder %s %s', [E.Message, E.StackTrace]));
	end;
end;

function TIndexMail.GetDirAge(fol: String): DWORD;
var
	param: TArray<String>;
begin
	try
		param := ['table Dirs',
			'output_columns _id,age',
			'filter _key=="'+bka.Yen2Slash(ExcludeTrailingPathDelimiter(fol))+'"'];
		grnreq.command('select', param);
		if grnreq.GetSelectCount > 0 then
			Result := grnreq.GetSelectValue(0, 1).AsType<DWORD>
		else
			Result := 0;
	except on E: Exception do
		logger.error(self.ClassName, Format('GetDirAge %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TIndexMail.SetDirAge(fol: String; age: DWORD);
var
	param: TArray<String>;
	json: TJSONArray;
begin
	try
		json := TJSONArray.Create;
		try
			json.Add(TJSONObject.Create.
				AddPair('_key', bka.Yen2Slash(ExcludeTrailingPathDelimiter(fol))).
				AddPair('age', TJSONNumber.Create(age)));
			grnreq.commandjson('load', ['table Dirs'], json);
		finally
			FreeAndNil(json);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('SetDirAge %s %s', [E.Message, E.StackTrace]));
	end;
end;

end.

