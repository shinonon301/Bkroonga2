unit GroongaRequest;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
	IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
	ShellAPI, System.JSON, NetEncoding, mylogger, BeckyAPI, System.StrUtils;

type
	TConfParams = record
		grnexe: String;
		grndbdir: String;
		grnport: Integer;
		hit1: Integer;
				maxhit: Integer;
		bTrash: Boolean;
		bOutbox: Boolean;
		bInbox: Boolean;
		Ignore: TArray<String>;
				IgnoreMbx: TArray<String>;
		end;
	TGroongaRequest = class(TObject)
	private
		fPort: Integer;
		fResult: TJSONObject;
		flogname: String;
		procedure ResultFree;
	public
		constructor Create(port: Integer);
		destructor Destroy; override;
		property Result: TJSONObject read fResult;
		function command(cmd: String; param: TArray<String> = [];
			fastflg: Boolean = False): TJSONObject;
		function commandjson(cmd: String; param: TArray<String> = [];
			json: TJSONArray = nil; fastflg: Boolean = False): TJSONObject;
		function param2str(param: TArray<String>): String;
		function GetSelectCount: DWORD;
		function GetSelectValue(idx, colidx: Integer): TJSONValue;
		function GetResBool: Boolean;
		function GetLoadCount: DWORD;
		function GetLoadId: DWORD;
		function CheckShutdown: Boolean;
		procedure CreateDB;
	end;

const
	MAX_LIMIT = 'limit 9999999';

var
	gconf: TConfParams;

implementation

{ TGroongaRequest }

constructor TGroongaRequest.Create(port: Integer);
begin
	flogname := self.ClassName +'['+ IntToHex(GetCurrentThreadId, 8) +']';
	logger.info(self.flogname, 'Create');
	fport := port;
	fResult := nil;
end;

destructor TGroongaRequest.Destroy;
begin
	logger.info(self.flogname, 'Destroy');
	if Assigned(fResult) then FreeAndNil(fResult);
	inherited;
end;

function TGroongaRequest.param2str(param: TArray<String>): String;
var
	i: Integer;
	name, value: String;
begin
	try
		Result := '';
		if Length(param) > 0 then begin
			for i := 0 to Length(param)-1 do begin
				name := Copy(param[i], 1, Pos(' ', param[i])-1);
				value := Copy(param[i], Pos(' ', param[i])+1, Length(param[i]));
				if Result = '' then Result := '?' else Result := Result + '&';
				Result := Result +
					TNetEncoding.URL.Encode(UTF8String(name)) +
					'=' +
					TNetEncoding.URL.Encode(UTF8String(value));
			end;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('param2str %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TGroongaRequest.ResultFree;
begin
	if Assigned(fResult) then FreeAndNil(fResult);
end;

function TGroongaRequest.CheckShutdown: Boolean;
var
	http: TIdHTTP;
begin
	http := TIdHTTP.Create;
	try
		http.ConnectTimeout := 500;
		http.ReadTimeout := 500;
		try
			http.Request.Connection := 'close';
			http.Get('http://127.0.0.1:'+IntToStr(fport)+'/d/shutdown');
			Result := True;
		except on E: Exception do
			Result := False;
		end;
		try
			http.Disconnect(False);
		except
			;
		end;
	finally
		http.Free;
	end;
end;

function TGroongaRequest.command(cmd: String; param: TArray<String> = [];
	fastflg: Boolean = False): TJSONObject;
begin
	Result := commandjson(cmd, param, nil, fastflg);
end;

function TGroongaRequest.commandjson(cmd: String; param: TArray<String> = [];
	json: TJSONArray = nil; fastflg: Boolean = False): TJSONObject;
var
	res, poststream: TStringStream;
	http: TIdHTTP;
	paramstr, errfn: String;
	i: Integer;
	outf: TextFile;
begin
	try
		// Groongaにコマンドを投げてfResultに返す
		// fResultは、TGroongaRequestインスタンス内で、次回command/commandjsonを
		// 実行するまでが有効期間なので注意
		Result := nil;
		ResultFree;
		paramstr := param2str(param);
		logger.debug(self.ClassName, Format('command %s %s', [cmd, paramstr]));
		http := TIdHTTP.Create;
		if fastflg then begin
			http.ConnectTimeout := 1000;
			http.ReadTimeout := 60000;
		end else begin
			http.ConnectTimeout := 3000;
			http.ReadTimeout := 60000;
		end;
		res := TStringStream.Create('', TEncoding.UTF8);
		try
			http.HandleRedirects := True;
			// エラーで例外を起こさないでエラー内容を返すようにするため
			http.HTTPOptions := http.HTTPOptions + [hoNoProtocolErrorException, hoWantProtocolErrorContent];
			if not Assigned(json) then begin
				logger.verbose(self.flogname, Format('%s [%s] %s', ['command', cmd, paramstr]));
				http.Get('http://127.0.0.1:'+IntToStr(fport)+'/d/'+cmd+paramstr, res);
			end else begin
				http.Request.Connection := 'close';
				http.Request.ContentType := 'application/json';
				poststream := TStringStream.Create(json.ToString, TEncoding.UTF8);
				logger.verbose(self.flogname,
					Format('%s [%s] %s json(%d)=%s',
					['command', cmd, paramstr, poststream.Size, Copy(json.ToString, 1, 512)]));
				try
					http.Post('http://127.0.0.1:'+IntToStr(fport)+'/d/'+cmd+paramstr, poststream, res);
				finally
					poststream.Free;
				end;
			end;
			logger.debug(self.flogname,
				Format('command %s finish [%s] res(max512b)=%s',
				[cmd, Copy(paramstr, 1, 32), Copy(res.DataString, 1, 512)]));
			fResult := TJSONObject.ParseJSONValue(res.DataString) as TJSONObject;
			try
				if fResult.GetValue<TJSONObject>('header').FindValue('error') <> nil then begin
					logger.error(self.ClassName, Format('command %s %s', [cmd, fResult.ToString]));
					errfn := ExtractFilePath(IniFileName);
					if cmd = 'select' then
						errfn := errfn + 'errorselect.txt'
					else if cmd = 'load' then
						errfn := errfn + 'errorload.txt'
					else
						errfn := errfn + 'errorjson.txt';
					if FileExists(errfn) then begin
						AssignFile(outf, errfn, CP_UTF8);
						try
							Rewrite(outf);
							Writeln(outf, Format('command %s %s', [cmd, paramstr]));
							if Assigned(json) then Writeln(outf, json.ToString);
							Writeln(outf, res.DataString);
						finally
							CloseFile(outf);
						end;
					end;
				end;
			except on E: Exception do
				;
			end;
			try
				http.Disconnect(False);
			except
				;
			end;
			Result := fResult;
		finally
			if Assigned(res) then res.Free;
			if Assigned(http) then http.Free;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('commandjson %s %s', [E.Message, e.StackTrace]));
	end;
end;

function TGroongaRequest.GetSelectCount: DWORD;
begin
	try
		Result := fResult.GetValue<TJSONObject>('body').GetValue<DWORD>('n_hits');
	except
		Result := 0;
	end;
end;

function TGroongaRequest.GetSelectValue(idx, colidx: Integer): TJSONValue;
var
	i: Integer;
	tmp: String;
begin
	Result := nil;
	try
		if idx < GetSelectCount then
			if colidx < fResult.GetValue<TJSONObject>('body').GetValue<TJSONArray>('columns').Count then
				Result := fResult.GetValue<TJSONObject>('body').GetValue<TJSONArray>('records').Items[idx].AsType<TJSONArray>.Items[colidx];
	except
		Result := nil;
	end;
end;

function TGroongaRequest.GetResBool: Boolean;
begin
	Result := False;
	try
		if fResult.GetValue('body').AsType<Boolean> then
			Result := True;
	except
		Result := False;
	end;
end;

function TGroongaRequest.GetLoadCount: DWORD;
begin
	try
		Result := fResult.GetValue<TJSONObject>('body').GetValue<DWORD>('n_loaded_records');
	except
		Result := 0;
	end;
end;

function TGroongaRequest.GetLoadId: DWORD;
var
	ary: TJSONArray;
begin
	if fResult.GetValue<TJSONObject>('body').TryGetValue<TJSONArray>('loaded_ids', ary) then
		Result := ary.Items[0].AsType<DWORD>
	else
		Result := 0;
end;

procedure TGroongaRequest.CreateDB;
begin
	{
	MailIds→Becky!のメールIDのテーブル。1つのMessageIdのメールが複数のフォルダに存在することがある
	├Mails→MessageIdを_keyとしたテーブル
	└Tags→メール属性のテーブル(inbox,outbox,trash,flag,tome,ccme)。MailIdsに紐づく
	}
	command('object_exist', ['name Bigram']);
	if GetResBool then command('table_remove', ['name Bigram', 'dependent yes']);
	command('object_exist', ['name Dirs']);
	if GetResBool then command('table_remove', ['name Dirs', 'dependent yes']);
	command('object_exist', ['name Tags']);
	if GetResBool then command('table_remove', ['name Tags', 'dependent yes']);
	command('object_exist', ['name Mails']);
	if GetResBool then command('table_remove', ['name Mails', 'dependent yes']);
	command('object_exist', ['name MailIds']);
	if GetResBool then command('table_remove', ['name MailIds', 'dependent yes']);
	command('table_create', ['name Mails', 'flags TABLE_HASH_KEY', 'key_type ShortText']);
	command('table_create', ['name Bigram', 'flags TABLE_PAT_KEY', 'key_type ShortText', 'normalizer NormalizerAuto', 'default_tokenizer TokenBigram']);
	command('table_create', ['name MailIds', 'flags TABLE_NO_KEY']);
	command('table_create', ['name Dirs', 'flags TABLE_PAT_KEY', 'key_type ShortText']);
	command('table_create', ['name Tags', 'flags TABLE_HASH_KEY', 'key_type ShortText']);
	command('column_create', ['table MailIds', 'name dir', 'flags COLUMN_SCALAR', 'type Dirs']);
	command('column_create', ['table MailIds', 'name idx', 'flags COLUMN_SCALAR', 'type UInt32']);
	command('column_create', ['table MailIds', 'name msgid', 'flags COLUMN_SCALAR', 'type Mails']);
	command('column_create', ['table MailIds', 'name attr', 'flags COLUMN_VECTOR', 'type Tags']);
	command('column_create', ['table MailIds', 'name priority', 'flags COLUMN_SCALAR', 'type UInt32']);
	command('column_create', ['table MailIds', 'name color', 'flags COLUMN_SCALAR', 'type UInt32']);
	command('column_create', ['table Mails', 'name mailid', 'flags COLUMN_VECTOR', 'type MailIds']);
	command('column_create', ['table Mails', 'name subject', 'flags COLUMN_SCALAR', 'type ShortText']);
	command('column_create', ['table Mails', 'name date', 'flags COLUMN_SCALAR', 'type Time']);
	command('column_create', ['table Mails', 'name mdate', 'flags COLUMN_SCALAR', 'type Time']);
	command('column_create', ['table Mails', 'name from', 'flags COLUMN_SCALAR', 'type ShortText']);
	command('column_create', ['table Mails', 'name to', 'flags COLUMN_SCALAR', 'type ShortText']);
	command('column_create', ['table Mails', 'name cc', 'flags COLUMN_SCALAR', 'type ShortText']);
	command('column_create', ['table Mails', 'name bcc', 'flags COLUMN_SCALAR', 'type ShortText']);
	command('column_create', ['table Mails', 'name inreplyto', 'flags COLUMN_SCALAR', 'type Mails']);
	command('column_create', ['table Mails', 'name body', 'flags COLUMN_SCALAR', 'type LongText']);
	command('column_create', ['table Mails', 'name quote', 'flags COLUMN_SCALAR', 'type LongText']);
	command('column_create', ['table Mails', 'name attach', 'flags COLUMN_SCALAR', 'type LongText']);
	command('column_create', ['table Dirs', 'name age', 'flags COLUMN_SCALAR', 'type UInt32']);
	command('column_create', ['table Tags', 'name index', 'flags COLUMN_INDEX', 'type MailIds', 'source attr']);		// MailIdsからTagsをqueryするために必要
	command('column_create', ['table Mails', 'name index', 'flags COLUMN_INDEX', 'type MailIds', 'source msgid']);		// MailIdsからMailsをqueryするために必要
	command('column_create', ['table Dirs', 'name index', 'flags COLUMN_INDEX', 'type MailIds', 'source dir']);			// MailIdsからDirsをqueryするために必要
	command('column_create', ['table MailIds', 'name revindex', 'flags COLUMN_INDEX', 'type Mails', 'source mailid']);	// MailsからMailIdsをqueryするために必要(多分使わない)
	command('column_create', ['table Bigram', 'name mailsindex', 'flags COLUMN_INDEX|WITH_POSITION|WITH_SECTION', 'type Mails', 'source body,attach,quote,subject,from,to,cc,bcc']);
	//command('column_create', ['table Bigram', 'name dirsindex', 'flags COLUMN_INDEX|WITH_POSITION|WITH_SECTION', 'type Dirs', 'source _key']);
end;

end.

