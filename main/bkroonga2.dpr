library bkroonga2;

{ DLL のメモリ管理に関する重要なメモ: パラメータまたは関数結果として文字列を渡す
	手続きまたは関数を DLL がエクスポートする場合は、ShareMem をライブラリの
	uses 句およびプロジェクトの uses 句 ([プロジェクト｜ソースの表示] を選択) の
	最初に記載する必要があります。これは、
	DLL との間で渡されるすべての文字列に当てはまります。レコードやクラスに
	ネストされているものも同様です。ShareMem は共有メモリ マネージャ BORLNDMM.DLL に対するインターフェイス
	ユニットです。この DLL は作成対象の DLL と一緒に配置する必要が
	あります。BORLNDMM.DLL を使用しないようにするには、PChar 型または ShortString 型の
	パラメータを使って文字列情報を渡します。}

{$R *.res}

uses
	System.SysUtils,
	BeckyAPI,
	Winapi.Windows,
	System.IniFiles,
	Winapi.messages,
	Vcl.Controls,
	Vcl.Graphics,
	Vcl.Forms,
	System.Generics.Collections,
	System.AnsiStrings,
	mylogger,
	Bkroonga2Main in 'Bkroonga2Main.pas' {Bkroonga2MainForm},
	IndexMail in 'IndexMail.pas';

var
	ExitSave: Pointer;
	BeforeFolder: AnsiString;
	BeforeRetrieve: AnsiString;
	CmdSearch: DWORD;

const
	BKID_FOLDERPROPERTY = 32935;
	AppName = 'Bkroonga2';

procedure IndexingPending;
begin
	if Assigned(indexing) then indexing.Pending;
end;

procedure IndexingAddFol(fol: String);
begin
	if Assigned(indexing) then begin
		indexing.Pending;
		if fol <> '' then indexing.AddFol(fol);
	end;
end;


/////////////////////////////////////////////////////////////////////////////////////////////
// Callbacks from Becky!

////////////////////////////////////////////////////////////////////////
// Called when menu is intialized.
procedure CmdProc(wnd: HWND; lp: LPARAM); stdcall;
begin
	try
		logger.debug('CmdProc', Format('%x %x', [wnd, lp]));
		if Exiting then Exit;
		if LOWORD(lp) = CmdSearch then
			if Assigned(Bkroonga2MainForm) then
				Bkroonga2MainForm.OpenSearchForm;
	except
		on E: Exception do MessageBox(0, PChar('CmdProc Exception'#10+E.Message), AppName, MB_OK);
	end;
end;

function CmdUIProc(wnd: HWND; lp: LPARAM):DWORD; stdcall;
begin
	Result := 0;
	try
		logger.debug('CmdUIProc', Format('%x %x', [wnd, lp]));
		if Exiting then Exit;
	except
		on E: Exception do MessageBox(0, PChar('CmdUIProc Exception'#10+E.Message), AppName, MB_OK);
	end;
end;

////////////////////////////////////////////////////////////////////////
// Called when the program is started and the main window is created.
function BKC_OnStart: Integer; stdcall;
begin
	logger.info('BKC_OnStart', '');
	Exiting := False;
	BeforeRetrieve := '';
	BeforeFolder := '';
	try
		bka.GetWindowHandles(hwndMain, hwndTree, hwndList, hwndView);
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnStart Exception'#10+E.Message), AppName, MB_OK);
	end;
	if Assigned(Bkroonga2MainForm) then
		Bkroonga2MainForm.StartGroonga
	else
        MessageBox(0, 'なぜかBkroonga2が起動していません', 'エラー', MB_ICONEXCLAMATION or MB_OK);
	// Always return 0.
	Result := 0;
end;

////////////////////////////////////////////////////////////////////////
// Called when the main window is closing.
function BKC_OnExit: Integer; stdcall;
begin
	try
		logger.info('BKC_OnExit', '');
		Exiting := True;
		if Assigned(Bkroonga2MainForm) then begin
			if Bkroonga2MainForm.Showing then
				Bkroonga2MainForm.Close;
			FreeAndNil(Bkroonga2MainForm);
		end;
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnExit Exception'#10+E.Message), AppName, MB_OK);
	end;
	// Return -1 if you don't want to quit.
	Result := 0;
end;

function BKC_OnMenuInit(Wnd: HWND; hndMenu: HMENU; nType: Integer):
	Integer; stdcall;
var
	hSubMenu: HMENU;
begin
	try
		logger.debug('BKC_OnMenuInit', Format('%x %x %d', [Wnd, hndMenu, nType]));
		case nType of
			BKC_MENU_MAIN: begin
					{
						Sample of adding menu items
						HMENU hSubMenu = GetSubMenu(hMenu, 4);
						// Define CmdProc as "void WINAPI CmdProc(HWND, LPARAM)"
						UINT nID = bka.RegisterCommand("Information about this Command", nType,CmdProc);
						AppendMenu(hSubMenu, MF_STRING, nID, "&Menu item");
					}
					Bkroonga2MainForm := TBkroonga2MainForm.Create(nil);
					hSubMenu := GetSubMenu(hndMenu, 4);
					AppendMenu(hSubMenu, MF_SEPARATOR, 0, nil);
					CmdSearch := bka.RegisterCommand('Bkroonga2検索', nType, CmdProc);
					AppendMenu(hSubMenu, MF_STRING, CmdSearch, 'Bkroonga2検索');
				end;
			BKC_MENU_LISTVIEW: begin
				end;
			BKC_MENU_TREEVIEW: begin
				end;
			BKC_MENU_MSGVIEW: begin
				end;
			BKC_MENU_MSGEDIT: begin
				end;
			BKC_MENU_COMPOSE: begin
				end;
			BKC_MENU_COMPEDIT: begin
				end;
			BKC_MENU_COMPREF: begin
				end;
			else begin
				end;
		end;
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnMenuInit Exception'#10+E.Message), AppName, MB_OK);
	end;
	// Always return 0.
	Result := 0;
end;

////////////////////////////////////////////////////////////////////////
// Called when a folder is opened.
function BKC_OnOpenFolder(lpFolderID: PAnsiChar): Integer; stdcall;
var
	tmp: AnsiString;
begin
	Result := 0;
	try
		tmp := AnsiString(lpFolderID);
		logger.debug('BKC_OnOpenFolder', String(tmp));
		if Exiting then Exit;
		IndexingAddFol(String(BeforeFolder));
		BeforeFolder := tmp;
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnOpenFolder Exception'#10+E.Message), AppName, MB_OK);
	end;
	// Always return 0.
	Result := 0;
end;

////////////////////////////////////////////////////////////////////////
// Called when a mail is selected.
function BKC_OnOpenMail(lpMailID: PAnsiChar): Integer; stdcall;
begin
	Result := 0;
	try
		logger.debug('BKC_OnOpenMail', String(AnsiString(lpMailID)));
		if Exiting then Exit;
		IndexingPending;
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnOpenMail Exception'#10+E.Message), AppName, MB_OK);
	end;
	// Always return 0.
	Result := 0;
end;

////////////////////////////////////////////////////////////////////////
// Called every minute.
function BKC_OnEveryMinute: Integer; stdcall;
begin
	Result := 0;
	try
		logger.info('BKC_OnEveryMinute', '');
		if Exiting then Exit;
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnEveryMinute Exception'#10+E.Message), AppName, MB_OK);
	end;

	// Always return 0.
	Result := 0;
end;

////////////////////////////////////////////////////////////////////////
// Called when a compose windows is opened.
function BKC_OnOpenCompose(Wnd: HWND; nMode: Integer {See COMPOSE_MODE_* in BeckyApi.h}): Integer; stdcall;
begin
	Result := 0;
	try
		logger.debug('BKC_OnOpenCompose', Format('%x %d', [Wnd, nMode]));
		if Exiting then Exit;
		IndexingPending;
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnOpenCompose Exception'#10+E.Message), AppName, MB_OK);
	end;
	// Always return 0.
	Result := 0;
end;

////////////////////////////////////////////////////////////////////////
// Called when the composing message is saved.
function BKC_OnOutgoing(Wnd: HWND; nMode: Integer {0:SaveToOutbox, 1:SaveToDraft, 2:SaveToReminder*/}): Integer; stdcall;
begin
	Result := 0;
	try
		logger.debug('BKC_OnOutgoing', Format('%x %d', [Wnd, nMode]));
		if Exiting then Exit;
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnOutGoing Exception'#10+E.Message), AppName, MB_OK);
	end;
	// Return -1 if you do not want to send it yet.
	Result := 0;
end;

////////////////////////////////////////////////////////////////////////
// Called when a key is pressed.
function BKC_OnKeyDispatch(Wnd: HWND; nKey: Integer {virtual key code};
	nShift: Integer{Shift state. 0x40=Shift, 0x20=Ctrl, 0x60=Shift+Ctrl, 0xfe=Alt}): Integer; stdcall;
begin
	Result := 0;
	try
		logger.debug('BKC_OnKeyDispatch', Format('%x %d %d', [Wnd, nKey, nShift]));
		if Exiting then Exit;
		IndexingPending;
		//ThrowTimerPending;
		if Assigned(Bkroonga2MainForm) and (nKey = Ord('E')) and (nShift = $20) then begin
			PostMessage(hwndMain, WM_COMMAND, CmdSearch, 0);
			//Bkroonga2MainForm.OpenSearchForm;
			Result := 1;
		end;
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnKeyDispatch Exception'#10+E.Message), AppName, MB_OK);
	end;
	// Return TRUE if you want to suppress subsequent command associated to this key.
end;

////////////////////////////////////////////////////////////////////////
// Called when a message is retrieved and saved to a folder
function BKC_OnRetrieve(lpMessage: PAnsiChar {Message source}; lpMailID: PAnsiChar {Mail ID}): Integer; stdcall;
var
	tmp: AnsiString;
begin
	Result := 0;
	try
		logger.debug('BKC_OnRetrieve', Format('%s', [AnsiString(lpMailID)]));
		if Exiting then Exit;
		tmp := ExtractFilePath(PAnsiChar(lpMailID));
		if BeforeRetrieve <> tmp then
			IndexingAddFol(String(tmp))
		else
			IndexingPending;
		BeforeRetrieve := tmp;
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnRetrieve Exception'#10+E.Message), AppName, MB_OK);
	end;
	// Always return 0.
	Result := 0;
end;

////////////////////////////////////////////////////////////////////////
// Called when a message is spooled
function BKC_OnSend(lpMessage: PAnsiChar {Message source}): Integer; stdcall;
begin
	Result := 0;
	try
		logger.debug('BKC_OnSend', Format('len=%d', [System.SysUtils.StrLen(lpMessage)]));
		if Exiting then Exit;
		IndexingPending;
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnSend Exception'#10+E.Message), AppName, MB_OK);
	end;
	// Always return 0.
	Result := 0;
end;

////////////////////////////////////////////////////////////////////////
// Called when all messages are retrieved
function BKC_OnFinishRetrieve(nNumber: Integer {Number of messages}): Integer; stdcall;
begin
	logger.debug('BKC_OnFinishRetrieve', Format('%d', [nNumber]));
	// Always return 0.
	BeforeRetrieve := '';
	Result := 0;
end;

////////////////////////////////////////////////////////////////////////
// Called when plug-in setup is needed.
function BKC_OnPlugInSetup(Wnd: HWND): Integer; stdcall;
begin
	Result := 0;
	try
		logger.info('BKC_OnPlugInSetup', Format('%x', [Wnd]));
		if Exiting then Exit;
		if Assigned(Bkroonga2MainForm) then begin
			IndexingPending;
			Bkroonga2MainForm.Position := poScreenCenter;
            Bkroonga2MainForm.UpdateParams;
			Bkroonga2MainForm.Show;
		end;
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnPlugInSetup Exception'#10+E.Message), AppName, MB_OK);
	end;
	Result := 1;
end;

////////////////////////////////////////////////////////////////////////
// Called when plug-in information is being retrieved.
function BKC_OnPlugInInfo(lpPlugInInfo: PBkPlugInInfo): Integer; stdcall;
begin
		{  You MUST specify at least szPlugInName and szVendor.
			 otherwise Becky! will silently ignore your plug-in.
		with lpPlugInInfo^ do begin
			szPlugInName := 'Becky! Hoge Hoge plug-in';
			szVendor := 'RimArts, Inc.';
			szVersion := '1.0';
			szDescription := 'Enables Becky! to do Hoge.';
		end;
		}
	try
		logger.debug('BKC_OnPlugInInfo', '');
		with lpPlugInInfo^ do begin
			System.SysUtils.StrLCopy(szPlugInName, PAnsiChar(szProductName), 80);
			System.SysUtils.StrLCopy(szVendor, PAnsiChar(szCompanyName), 80);
			System.SysUtils.StrLCopy(szVersion, PAnsiChar(szFileVersion), 80);
			System.SysUtils.StrLCopy(szDescription, PAnsiChar(szFileDescription), 80);
		end;
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnPlugInSetup Exception'#10+E.Message), AppName,MB_OK);
	end;
	// Always return 0.
	Result := 0;
end;

function BKC_OnDragDrop(lpTgt, lpSrc: PAnsiChar; nCount, dropEffect: Integer): Integer; stdcall;
var
	tgt, src: AnsiString;
	i: Integer;
begin
	{
		lpTgt:  A folder ID of the target folder.
						You can assume it is a root mailbox, if the string
						contains only one '\' character.
		lpSrc:  Either a folder ID or mail IDs. Multiple mail IDs are
						separated by '\n' (0x0a).
						You can assume it is a folder ID, if the string
						doesn't contain '?' character.
		nCount  Number of items to be dropped.
						It can be more than one, if you drop mail items.
		dropEffect: Type of drag and drop operation
						1: Copy
						2: Move
						4: Link (Used for filtering setup in Becky!)
	}
	// If you want to cancel the default drag and drop action,
	// return -1;
	// Do not assume the default action (copy, move, etc.) is always
	// processed, because other plug-ins might cancel the operation.
	Result := 0;
	try
		logger.debug('BKC_OnDragDrop', Format('%s<-%s %d %d', [AnsiString(lpTgt), AnsiString(lpSrc), nCount, dropEffect]));
		if Exiting then Exit;
		tgt := PAnsiChar(lpTgt);
		IndexingAddFol(String(tgt));
		src := AnsiString(PAnsiChar(lpSrc));
		for i := 1 to Length(src)-1 do begin
			if src[i] = '?' then begin
				IndexingAddFol(Copy(src, 1, i-1));
			end;
		end;
	except
		on E: Exception do MessageBox(0, PChar('BKC_OnDragDrop Exception'#10+E.Message), AppName,MB_OK);
	end;
	Result := 0;
end;

exports
	BKC_OnStart,			// index 1,
	BKC_OnExit,				// index 2,
	BKC_OnMenuInit,			// index 3,
	BKC_OnOpenFolder,		// index 4,
	BKC_OnOpenMail,			// index 5,
	BKC_OnEveryMinute,		// index 6,
	BKC_OnOpenCompose,		// index 7,
	BKC_OnOutgoing,			// index 8,
	BKC_OnKeyDispatch,		// index 9,
	BKC_OnRetrieve,			// index 10,
	BKC_OnSend,				// index 11,
	BKC_OnFinishRetrieve,	// index 12,
	BKC_OnPlugInSetup,		// index 13,
	BKC_OnPlugInInfo,		// index 14,
	BKC_OnDragDrop;			// index 15;

// DLLの終了処理
procedure LibExit;
begin
	if Assigned(logger) then begin
		logger.Terminate;
		logger.WaitFor;
		FreeAndNil(logger);
	end;
	if Assigned(bka) then
		FreeAndNil(bka);
	ExitProc := ExitSave;
end;

// DLLの初期化処理
var
	szFileName: array[0..(MAX_PATH+2)] of AnsiChar;
	VerInfoSize, dwHandle, uLen: DWORD;
	pVerInfo, pBuf: Pointer;
	LangId, CharSetId, LogFn, tmpStr: AnsiString;
	llev, lsize, lhist: Integer;
    f: File;
begin
	bka := TBeckyAPI.Create;
	ExitSave := ExitProc;
	ExitProc := @LibExit;
	Exiting := False;
	if bka.InitAPI = False then Halt;

	// バージョン情報からプラグイン情報を抽出する
	// デフォルトのプラグイン情報
	szProductName := 'Product Name';
	szCompanyName := 'Company Name';
	szFileVersion := '1.0';
	szFileDescription := 'Description';
	// DLLファイルのバージョン情報からそれぞれの文字列を得る
	GetModuleFileNameA(hInstance, szFileName, MAX_PATH);
	MyDllFileName := Trim(String(szFileName));
	if DirectoryExists(bka.DataFolder+'PlugIns') then begin
		IniFileName := bka.DataFolder + 'PlugIns\' + ExtractFileName(ChangeFileExt(MyDllFileName, '.ini'));
	end else begin
		IniFileName := bka.DataFolder + ExtractFileName(ChangeFileExt(MyDllFileName, '.ini'));
	end;
	if (not FileExists(IniFileName)) and FileExists(ChangeFileExt(MyDllFileName, '.ini')) then
		CopyFile(PChar(IniFileName), PChar(ChangeFileExt(MyDllFileName, '.ini')), True);
	LogFn := ChangeFileExt(IniFilename, '.log');
	llev := Ord(LOG_DEBUG);
	lsize := 1000000;
    lhist := 7;
	if FileExists(IniFileName) then begin
		SetLength(tmpStr, MAX_PATH+4);
		GetPrivateProfileStringA(PAnsiChar(AppName), PAnsiChar('LogFile'), '', PAnsiChar(tmpStr), MAX_PATH, PAnsiChar(AnsiString(IniFilename)));
		if Trim(tmpStr) <> '' then begin
			LogFn := Trim(tmpStr);
			try
				if not FileExists(LogFn) then begin
					if Trim(ExtractFilePath(LogFn)) = '' then LogFn := ExtractFilePath(IniFileName) + LogFn;
					if not DirectoryExists(ExtractFilePath(LogFn)) then ForceDirectories(ExtractFilePath(LogFn));
					if not FileExists(LogFn) then begin
						AssignFile(f, LogFn);
						Rewrite(f);
						CloseFile(f);
					end;
				end;
			except on E: Exception do
				LogFn := ChangeFileExt(IniFileName, '.log');
			end;
		end;
		SetLength(tmpStr, 256);
		GetPrivateProfileStringA(PAnsiChar(AppName), PAnsiChar('LogLevel'), PAnsiChar(''), PAnsiChar(tmpStr), 255, PAnsiChar(AnsiString(IniFilename)));
		llev := StrToIntDef(Trim(tmpstr), -1);
		if llev < 0 then llev := Ord(LOG_DEBUG);
		SetLength(tmpStr, 256);
		GetPrivateProfileStringA(PAnsiChar(AppName), PAnsiChar('LogSize'), PAnsiChar(''), PAnsiChar(tmpStr), 255, PAnsiChar(AnsiString(IniFilename)));
		lsize := StrToIntDef(Trim(tmpstr), -1);
		if lsize < 0 then lsize := 1000000;
		SetLength(tmpStr, 256);
		GetPrivateProfileStringA(PAnsiChar(AppName), PAnsiChar('LogHistory'), PAnsiChar(''), PAnsiChar(tmpStr), 255, PAnsiChar(AnsiString(IniFilename)));
		lhist := StrToIntDef(Trim(tmpstr), -1);
		if lhist < 0 then lhist := 7;
	end;
	logger := TMyLogger.Create(LogFn, TLogLevel(llev), LOG_INFO, lsize, lhist);
	VerInfoSize := GetFileVersionInfoSizeA(szFileName, dwHandle);
	if VerInfoSize <> 0 then begin
		pVerInfo := AllocMem(VerInfoSize);
		try
			if GetFileVersionInfoA(szFileName, dwHandle, VerInfoSize, pVerInfo) then begin
				if VerQueryValue(pVerInfo, '\VarFileInfo\Translation', pBuf, uLen) then begin
					LangId := IntToHex(word(pBuf^), 4);
					Inc(pbyte(pBuf), 2);
					CharSetId := IntToHex(word(pBuf^), 4);
					// バージョン情報の「製品名」からszPlugInNameを得る
					tmpStr := '';
					if VerQueryValueA(pVerInfo, PAnsiChar('\StringFileInfo\'+LangId+CharSetId+'\ProductName'), pBuf, uLen) then begin
						if uLen <> 0 then begin
							SetLength(tmpStr, uLen);
							System.SysUtils.StrLCopy(PAnsiChar(tmpStr), pBuf, uLen);
							tmpStr := Trim(PAnsiChar(tmpStr));
						end;
					end;
					if tmpStr <> '' then
						szProductName := tmpstr + #0;
					// バージョン情報の「会社名」からszVendorを得る
					tmpStr := '';
					if VerQueryValueA(pVerInfo, PAnsiChar('\StringFileInfo\'+LangId+CharSetId+'\CompanyName'), pBuf, uLen) then begin
						if uLen <> 0 then begin
							SetLength(tmpStr, uLen);
							System.SysUtils.StrLCopy(PAnsiChar(tmpStr), pBuf, uLen);
							tmpStr := Trim(PAnsiChar(tmpStr));
						end;
					end;
					if tmpStr <> '' then
						szCompanyName := tmpstr + #0;
					// バージョン情報の「製品バージョン」からszVersionを得る
					tmpStr := '';
					if VerQueryValueA(pVerInfo, PAnsiChar('\StringFileInfo\'+LangId+CharSetId+'\ProductVersion'), pBuf, uLen) then begin
						if uLen <> 0 then begin
							SetLength(tmpStr, uLen);
							System.SysUtils.StrLCopy(PAnsiChar(tmpStr), pBuf, uLen);
							tmpStr := Trim(PAnsiChar(tmpStr));
						end;
					end;
					if tmpStr <> '' then
						szFileVersion := tmpstr + #0;
					// バージョン情報の「説明」からszDescriptionを得る
					tmpStr := '';
					if VerQueryValueA(pVerInfo, PAnsiChar('\StringFileInfo\'+LangId+CharSetId+'\FileDescription'), pBuf, uLen) then begin
						if uLen <> 0 then begin
							SetLength(tmpStr, uLen);
							System.SysUtils.StrLCopy(PAnsiChar(tmpStr), pBuf, uLen);
							tmpStr := Trim(PAnsiChar(tmpStr));
						end;
					end;
					if tmpStr <> '' then
						szFileDescription := tmpstr + #0;
				end;
			end;
		finally
			FreeMem(pVerInfo);
		end;
	end;

end.

