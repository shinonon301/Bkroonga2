unit Bkroonga2Main;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Winapi.ShellAPI, mylogger, BeckyAPI, bgconsole,
	Vcl.StdCtrls, System.JSON, System.Generics.Collections, groongarequest,
	Vcl.ExtCtrls, System.RegularExpressions, REST.Json, System.Zip, IndexMail, IniFiles,
	Registry, Vcl.CheckLst, System.StrUtils, Clipbrd,
	Bkroonga2Search, Vcl.ComCtrls, System.ImageList, Vcl.ImgList;

type
	TBkroonga2MainForm = class(TForm)
		Timer: TTimer;
		BtnParse: TButton;
		OpenDialog: TOpenDialog;
		EditQuery: TEdit;
		BtnQuery: TButton;
		Panel1: TPanel;
		LabelVer: TLabel;
		LabelGrnVer: TLabel;
		LabelDirCnt: TLabel;
		LabelMailCnt: TLabel;
		Label1: TLabel;
		EditGrnExe: TEdit;
		BtnGrnExe: TButton;
		GroupBox1: TGroupBox;
		CBTrash: TCheckBox;
		CBOutbox: TCheckBox;
		CBInbox: TCheckBox;
		Label2: TLabel;
		EditFolderName: TEdit;
		Label3: TLabel;
		GroupBox2: TGroupBox;
		BtnOK: TButton;
		BtnCancel: TButton;
		CLBMbx: TCheckListBox;
		BtnOpenBackup: TButton;
		LVLog: TListView;
		IMLVIcon: TImageList;
    Label4: TLabel;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure TimerTimer(Sender: TObject);
		procedure BtnParseClick(Sender: TObject);
		procedure BtnQueryClick(Sender: TObject);
		procedure EditQueryKeyPress(Sender: TObject; var Key: Char);
		procedure BtnOKClick(Sender: TObject);
		procedure BtnCancelClick(Sender: TObject);
		procedure BtnGrnExeClick(Sender: TObject);
		procedure BtnOpenBackupClick(Sender: TObject);
	    procedure LVLogData(Sender: TObject; Item: TListItem);
		procedure LVLogSelectItem(Sender: TObject; Item: TListItem;
		  Selected: Boolean);
		procedure FormShow(Sender: TObject);
	private
		{ Private 宣言 }
		grnproc: TBgconsole;
		grnreq: TGroongaRequest;
		backupproc: TBgconsole;
        Logs: TStringList;
		function FormatJson(json: String): String;
		procedure IniLoad;
		procedure IniSave;
		procedure MemoLogAdd(s: String);
		procedure FreeGrnObjects;
		procedure EnumMbox;
		function GetCLBMbxName(idx: Integer): String;
		function GetDefaultIgnoreMbx: TArray<String>;
		procedure MakeDumpBat;
		procedure MakeRestoreBat;
		procedure ShowLastLog;
	public
		{ Public 宣言 }
		procedure StartGroonga;
		procedure OpenSearchForm;
		procedure UpdateParams;
	end;

const
	DBName = 'bkroonga.db';

var
	Bkroonga2MainForm: TBkroonga2MainForm;

implementation

{$R *.dfm}

function TBkroonga2MainForm.FormatJson(json: String): String;
var
	tmp: TJsonObject;
begin
	tmp := TJsonObject.ParseJSONValue(json) as TJsonObject;
	Result := TJson.Format(tmp);
	FreeAndNil(tmp);
end;

procedure TBkroonga2MainForm.FormCreate(Sender: TObject);
begin
    Logs := TStringList.Create;
	IniLoad;
	if not FileExists(IniFilename) then IniSave;
end;

procedure TBkroonga2MainForm.FormDestroy(Sender: TObject);
begin
    IniSave;
	FreeGrnObjects;
    FreeAndNil(Logs);
end;

procedure TBkroonga2MainForm.FormShow(Sender: TObject);
begin
	ShowLastLog;
end;

procedure TBkroonga2MainForm.FreeGrnObjects;
begin
	if Assigned(Bkroonga2SearchForm) then begin
		if Bkroonga2SearchForm.Showing then begin
			Bkroonga2SearchForm.ClearAllQuery;
			Bkroonga2SearchForm.Close;
		end else begin
			Bkroonga2SearchForm.ClearAllQuery;
		end;
		Application.ProcessMessages;
		FreeAndNil(Bkroonga2SearchForm);
	end;
	if Assigned(indexing) then begin
		indexing.Terminate;
		//Application.ProcessMessages;
		indexing.WaitFor;
		FreeAndNil(indexing);
	end;
	if Assigned(grnproc) then begin
		grnreq.command('shutdown');
		//Application.ProcessMessages;
		grnproc.Waitfor;
		FreeAndNil(grnreq);
		FreeAndNil(grnproc);
	end;
end;

procedure TBkroonga2MainForm.UpdateParams;
begin
	if Assigned(grnreq) then begin
		grnreq.command('select', ['table Dirs', 'limit 0']);
		LabelDirCnt.Caption := Format('Dir Count=%d', [grnreq.GetSelectCount]);
		grnreq.command('select', ['table MailIds', 'limit 0']);
		LabelMailCnt.Caption := Format('Mail Count=%d', [grnreq.GetSelectCount]);
	end;
	EditGrnExe.Text := gconf.grnexe;
	CBTrash.Checked := gconf.bTrash;
	CBOutbox.Checked := gconf.bOutbox;
	CBInbox.Checked := gconf.bInbox;
	EditFolderName.Text := String.Join('/', gconf.Ignore);
	EnumMbox;
end;

procedure TBkroonga2MainForm.IniLoad;
var
	ini: TIniFile;
begin
	ini := TIniFile.Create(IniFilename);
	with ini do begin
		gconf.grnexe := ReadString(AppName, 'GroongaExe', '');
		gconf.grndbdir := ReadString(AppName, 'DBDir',
			IncludeTrailingPathDelimiter(ExtractFilePath(IniFileName))+'bkroonga2db');
        gconf.grnport := ReadInteger(AppName, 'GroongaPort', 10083);
		gconf.bTrash := ReadBool(AppName, 'bTrash', True);
		gconf.bOutbox := ReadBool(AppName, 'bOutbox', False);
		gconf.bInbox := ReadBool(AppName, 'bInbox', False);
		gconf.Ignore := TRegEx.Split(ReadString(AppName, 'Ignore', ''), '/');
		if ValueExists(AppName, 'IgnoreMailbox') then
			gconf.IgnoreMbx := TRegEx.Split(ReadString(AppName, 'IgnoreMailbox', ''), '/')
		else
			gconf.IgnoreMbx := GetDefaultIgnoreMbx;
		Free;
	end;
end;

procedure TBkroonga2MainForm.IniSave;
var
	ini: TIniFile;
begin
	ini := TIniFile.Create(IniFilename);
	with ini do begin
		WriteString(AppName, 'GroongaExe', gconf.grnexe);
		WriteString(AppName, 'DBDir', gconf.grndbdir);
        WriteInteger(AppName, 'GroongaPort', gconf.grnport);
		WriteBool(AppName, 'bTrash', gconf.bTrash);
		WriteBool(AppName, 'bOutbox', gconf.bOutbox);
		WriteBool(AppName, 'bInbox', gconf.bInbox);
		WriteString(AppName, 'Ignore', String.Join('/', gconf.Ignore));
		WriteString(AppName, 'IgnoreMailbox', String.Join('/', gconf.IgnoreMbx));
		Free;
	end;
end;

procedure TBkroonga2MainForm.LVLogData(Sender: TObject; Item: TListItem);
var
	idx: Integer;
	s: String;
begin
	idx := Item.Index;
	if (idx >= 0) and (idx < Logs.Count) then begin
		s := Logs[idx];
		if Pos('[VERB ]', s) > 0 then
			Item.ImageIndex := 0
		else if Pos('[DEBUG]', s) > 0 then
			Item.ImageIndex := 1
		else if Pos('[INFO ]', s) > 0 then
			Item.ImageIndex := 2
		else if Pos('[WARN ]', s) > 0 then
			Item.ImageIndex := 3
		else if Pos('[ERROR]', s) > 0 then
			Item.ImageIndex := 4
		else if Pos('[FATAL]', s) > 0 then
			Item.ImageIndex := 5
		else
			Item.ImageIndex := -1;
		Item.SubItems.Clear;
		Item.SubItems.Add(Logs[idx]);
	end;
end;

procedure TBkroonga2MainForm.LVLogSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
	if Selected then
		if Item.SubItems.Count > 0 then
            Clipboard.AsText := Item.SubItems[0];
end;

procedure TBkroonga2MainForm.MemoLogAdd(s: String);
var
	s0, s1: String;
	ss: TArray<String>;
begin
	while Logs.Count > 999 do Logs.Delete(0);
	if Pos(#10, s) > 0 then begin
		ss := Trim(s).Split([#10]);
		for s0 in ss do begin
			if s0 <> '' then Logs.Add(s0);
		end;
	end else begin
		Logs.Add(Trim(s));
	end;
	LVLog.Items.Count := Logs.Count;
	if self.Showing and LVLog.Showing then ShowLastLog;
end;

procedure TBkroonga2MainForm.OpenSearchForm;
begin
	if Assigned(Bkroonga2SearchForm) then begin
		Bkroonga2SearchForm.Show;
		Bkroonga2SearchForm.CBQuery.SetFocus;
	end;
end;

procedure TBkroonga2MainForm.ShowLastLog;
begin
	if LVLog.Items.Count > 0 then begin
		LVLog.Items[LVLog.Items.Count-1].MakeVisible(True);
        LVLog.Refresh;
	end;
end;

procedure TBkroonga2MainForm.StartGroonga;
begin
	if (gconf.grnexe = '') or not(FileExists(gconf.grnexe)) then begin
		if FileExists('C:\groonga\bin\groonga.exe') then
			gconf.grnexe := 'C:\groonga\bin\groonga.exe'
		else if FileExists('C:\Program Files\groonga\bin\groonga.exe') then
			gconf.grnexe := 'C:\Program Files\groonga\bin\groonga.exe'
		else if FileExists('C:\Program Files (x86)\groonga\bin\groonga.exe') then
			gconf.grnexe := 'C:\Program Files (x86)\groonga\bin\groonga.exe';
		if (gconf.grnexe = '') or not(FileExists(gconf.grnexe)) then begin
			MessageBox(0, 'groonga.exeの場所を指定してください', AppName, MB_OK or MB_ICONINFORMATION);
			if OpenDialog.Execute(self.Handle) then
				gconf.grnexe := OpenDialog.FileName;
		end;
	end;
	if (gconf.grnexe <> '') and FileExists(gconf.grnexe) then begin
		IniSave;
		if not(DirectoryExists(gconf.grndbdir)) then
			ForceDirectories(gconf.grndbdir);
		if not(FileExists(IncludeTrailingPathDelimiter(gconf.grndbdir) + DBName)) then begin
			MessageBox(0, 'データベースファイルを作成します', AppName, MB_OK or MB_ICONINFORMATION);
			grnproc := TBgconsole.Create(gconf.grnexe, '-n "' + IncludeTrailingPathDelimiter(gconf.grndbdir) + DBName +'" quit');
			grnproc.WaitFor;
			FreeAndNil(grnproc);
			if not FileExists(ExtractFilePath(MyDllFileName) + 'xdoc2txt.exe') then
				MessageBox(0,
					PChar('xdoc2txt を導入すると添付ファイルも全文高速検索の対象にできます'#10'詳細は付属ドキュメントを参照してください'),
					AppName, MB_OK or MB_ICONINFORMATION);
		end;
		try
			try
				grnreq := TGroongaRequest.Create(gconf.grnport);
				grnreq.CheckShutdown;
				FreeAndNil(grnreq);
			except on E: Exception do
			end;
			while True do begin
				// なんかhttpポートが使われていても起動は成功しちゃうらしい(マジで！？)
				// なので↓の工夫は意味無し
				grnproc := TBgconsole.Create(gconf.grnexe,
					'-d --protocol http -p '+IntToStr(gconf.grnport)+
					' --bind-address 127.0.0.1 --default-command-version 3'+
					' --default-request-timeout 60 "' + IncludeTrailingPathDelimiter(gconf.grndbdir) + DBName + '"');
				while True do begin
					if grnproc.Running then Break;
					Sleep(10);
				end;
				Sleep(100);
				if grnproc.Running then Break;
				grnproc.WaitFor;
				FreeAndNil(grnproc);
				Inc(gconf.grnport);
				if gconf.grnport >= 10093 then Exit;
			end;
			grnreq := TGroongaRequest.Create(gconf.grnport);
			grnreq.command('status');
			LabelVer.Caption := Format('Bkroonga2 Ver. %s',
				[szFileVersion]);
			LabelGrnVer.Caption := Format('Groonga Ver. %s',
				[grnreq.Result.GetValue<TJSONObject>('body').GetValue<String>('version')]);
			//MemologAdd(grnver);
			grnreq.command('plugin_register', ['name functions/string']);
			grnreq.command('plugin_register', ['name functions/vector']);
			grnreq.command('plugin_register', ['name functions/time']);
			grnreq.command('object_exist', ['name Mails']);
			if not grnreq.GetResBool then begin
				//MessageBox(0, 'データベース構造を作成します', AppName, MB_OK or MB_ICONINFORMATION);
				grnreq.CreateDB;
			end;
			indexing := TIndexMail.Create(False, gconf.grnport);
			Bkroonga2SearchForm := TBkroonga2SearchForm.Create(self);
			Bkroonga2SearchForm.SetGroongaHTTPPort(gconf.grnport);
            Bkroonga2SearchForm.GetDefaultComponentPos;
		except
			grnproc := nil;
		end;
	end;
end;

procedure TBkroonga2MainForm.TimerTimer(Sender: TObject);
begin
	try
		if Assigned(GlobalLogQueue) then begin
			while GlobalLogQueue.Count > 0 do
				MemologAdd(GlobalLogQueue.Extract);
		end;
		if EditQuery.Focused then indexing.Pending;
		if Assigned(backupproc) then begin
			if backupproc.Running = False then begin
				logger.info(self.ClassName, 'backupproc end');
				backupproc.WaitFor;
				FreeAndNil(backupproc);
				StartGroonga;
			end;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('TimerTimer %s %s', [E.Message, E.StackTrace]));
	end;
end;

procedure TBkroonga2MainForm.BtnCancelClick(Sender: TObject);
begin
	Close;
end;

procedure TBkroonga2MainForm.BtnGrnExeClick(Sender: TObject);
begin
	if OpenDialog.Execute(self.Handle) then
		EditGrnExe.Text := OpenDialog.FileName;
end;

procedure TBkroonga2MainForm.BtnOKClick(Sender: TObject);
var
	i: Integer;
begin
	if gconf.grnexe <> EditGrnExe.Text then begin
		if MessageBox(hwndMain, 'Groongaを再起動します', '確認', MB_OKCANCEL or MB_ICONEXCLAMATION) <> ID_OK then begin
			Close;
			Exit;
		end;
		FreeGrnObjects;
	end;
	gconf.grnexe := EditGrnExe.Text;
	gconf.bTrash := CBTrash.Checked;
	gconf.bOutbox := CBOutbox.Checked;
	gconf.bInbox := CBInbox.Checked;
	gconf.Ignore := TRegEx.Split(EditFolderName.Text, '\/');
	SetLength(gconf.IgnoreMbx, 0);
	for i := 0 to CLBMbx.Items.Count-1 do
		if CLBMbx.Checked[i] then
			gconf.IgnoreMbx := gconf.IgnoreMbx + [GetCLBMbxName(i)];
	IniSave;
	if not Assigned(Bkroonga2SearchForm) then
		StartGroonga;
	Close;
end;

procedure TBkroonga2MainForm.BtnOpenBackupClick(Sender: TObject);
var
	ans: Integer;
begin
	if not Assigned(grnproc) then begin
		MessageBox(0, 'Groongaが起動していません', 'エラー', MB_ICONERROR or MB_OK);
		Exit;
	end;
	if not FileExists(ExtractFilePath(gconf.grnexe)+'lz4.exe') then begin
		MessageBox(0, 'lz4.exeが見つかりません', 'エラー', MB_ICONERROR or MB_OK);
		Exit;
	end;
	if not DirectoryExists(ExtractFilePath(IniFileName)+'bkroonga2bak') then begin
		ans := MessageBox(0,
			'DBバックアップ用フォルダを、プラグインフォルダ内に作成します'#10'よろしいですか？',
			'確認', MB_YESNO or MB_DEFBUTTON2 or MB_ICONEXCLAMATION);
		if ans <> ID_YES then Exit;
		ForceDirectories(ExtractFilePath(IniFileName)+'bkroonga2bak');
	end;
	if DirectoryExists(ExtractFilePath(IniFileName)+'bkroonga2bak') then begin
		MakeDumpBat;
		MakeRestoreBat;
		ShellExecute(0, 'open', PChar(ExtractFilePath(IniFileName)+'bkroonga2bak'), nil, nil, SW_SHOWNORMAL);
	end;
end;

procedure TBkroonga2MainForm.BtnParseClick(Sender: TObject);
var
	src: RawByteString;
	json: TJsonObject;
begin
	indexing.Pending;
	src := bka.GetSource(bka.GetCurrentMail);
	if src = '' then Exit;
	json := TJsonObject.Create;
	try
		indexing.ParseMail(src, json);
		MemologAdd(TJson.Format(json));
	finally
		FreeAndNil(json);
	end;
end;

procedure TBkroonga2MainForm.BtnQueryClick(Sender: TObject);
var
	cmd, tmp: String;
	param: TArray<String>;
	m: TMatchCollection;
	i: Integer;
begin
	indexing.Pending;
	tmp := Trim(EditQuery.Text) + ' ';
	cmd := Trim(Copy(tmp, 1, Pos(' ',tmp)-1));
	if cmd = '' then Exit;
	tmp := Copy(tmp, Pos(' ',tmp)+1, Length(tmp));
	m := TRegEx.Matches(tmp, '(\S+?)=(\S+)');
	for i := 0 to m.Count-1 do begin
		param := param + [m.Item[i].Groups.Item[1].Value + ' ' + m.Item[i].Groups.Item[2].Value];
	end;
	grnreq.command(cmd, param);
	MemoLogAdd(FormatJson(grnreq.Result.ToString));
end;

procedure TBkroonga2MainForm.EditQueryKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #13 then begin
		BtnQuery.Click;
		Key := #0;
	end;
end;

procedure TBkroonga2MainForm.EnumMbox;
var
	srec: TSearchRec;
	s: String;
	i, j: Integer;
begin
	if FindFirst(bka.DataFolder+'*.mb*', faDirectory, srec) = 0 then begin
		CLBMbx.Clear;
		repeat
			s := bka.ReadMailboxIni(srec.Name+'\', 'Account', 'Name');
			CLBMbx.Items.Add(Format('[%s] (%s)',
				[s, srec.Name]));
		until FindNext(srec) <> 0;
		FindClose(srec);
	end;
	for j := 0 to CLBMbx.Items.Count-1 do begin
		CLBMbx.Checked[j] := False;
		for i := 0 to Length(gconf.IgnoreMbx)-1 do
			if GetCLBMbxName(j) = gconf.IgnoreMbx[i] then
				CLBMbx.Checked[j] := True;
	end;
end;

function TBkroonga2MainForm.GetCLBMbxName(idx: Integer): String;
var
	m: TMatch;
begin
	Result := '';
	if (idx >= 0) and (idx < CLBMbx.Items.Count) then begin
		m := TRegEx.Match(CLBMbx.Items[idx], '\s\((.+\.mb)');
		if m.Success then
			Result := m.Groups[1].Value;
	end;
end;

function TBkroonga2MainForm.GetDefaultIgnoreMbx: TArray<String>;
var
	srec: TSearchRec;
begin
	SetLength(Result, 0);
	if FindFirst(bka.DataFolder+'*.mb.bak', faDirectory, srec) = 0 then begin
		repeat
			Result := Result + [ReplaceText(srec.Name, '.bak', '')];
		until FindNext(srec) <> 0;
		FindClose(srec);
	end;
end;

procedure TBkroonga2MainForm.MakeDumpBat;
var
	s: String;
	f: TextFile;
begin
	AssignFile(f, ExtractFilePath(IniFileName)+'bkroonga2bak\DBバックアップ作成.bat', CP_UTF8);
	Rewrite(f);
	Writeln(f, 'chcp 65001');
	s := gconf.grnexe+' "'+IncludeTrailingPathDelimiter(gconf.grndbdir) + DBName + '" dump | '+
		ExtractFilePath(gconf.grnexe)+'lz4.exe -9 - "'+
		ExtractFilePath(IniFileName)+'bkroonga2bak\dbbak%date:~0,4%%date:~5,2%%date:~8,2%.lz4"';
	Writeln(f, s);
	CloseFile(f);
end;

procedure TBkroonga2MainForm.MakeRestoreBat;
var
	s: String;
	f: TextFile;
begin
	AssignFile(f, ExtractFilePath(IniFileName)+'bkroonga2bak\DBバックアップリストア(lz4ファイルをドロップ).bat', CP_UTF8);
	Rewrite(f);
	Writeln(f, 'chcp 65001');
	Writeln(f, 'if EXIST "%1" (goto CONT) else goto END');
	Writeln(f, ':CONT');
	Writeln(f, 'del "'+IncludeTrailingPathDelimiter(gconf.grndbdir)+'bkroonga.*"');
	s := ExtractFilePath(gconf.grnexe)+'lz4.exe -d "%1" - | '+
		gconf.grnexe+' -n "'+IncludeTrailingPathDelimiter(gconf.grndbdir) + DBName + '"';
	Writeln(f, s);
	Writeln(f, ':END');
	CloseFile(f);
	AssignFile(f, ExtractFilePath(IniFileName)+'bkroonga2bak\！いずれもBeckyを終了して実行すること！.txt', CP_UTF8);
	Rewrite(f);
	Writeln(f, 'DBバックアップ作成は、規模にもよりますがおそらく数分で完了します。');
	Writeln(f, 'DBバックアップリストアは、規模にもよりますがおそらく数十分かかりますので、');
	Writeln(f, '諦めないで待ってください。');
	Writeln(f, 'いずれもBecky!を終了して行ってください。');
	CloseFile(f);
end;

end.
