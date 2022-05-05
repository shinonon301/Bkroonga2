///////////////////////////////////////////////////////////////////////////
// Becky! API class library for embarcadero Delphi Community Edition
//
// You can modify and redistribute this file without any permission.
//
// Original file is made by RimArts Inc.
// Translated and Modify function by A.Shinoda (shinonon) shinonon@jcom.home.ne.jp

unit BeckyAPI;

{$X+}
{$Z2}

interface

uses
	Winapi.Windows, System.SysUtils, Winapi.Messages, Vcl.Forms,
	System.Generics.Collections, System.Generics.Defaults, System.Classes,
	System.AnsiStrings, System.SyncObjs, System.RegularExpressions, mylogger;

///////////////////////////////////////////////////////////////////////////
//
// Constants for BKA_GetWindowHandles
//
const
	BKC_MENU_MAIN		= 0;
	BKC_MENU_LISTVIEW	= 1;
	BKC_MENU_TREEVIEW	= 2;
	BKC_MENU_MSGVIEW 	= 3;
	BKC_MENU_MSGEDIT	= 4;
	BKC_MENU_COMPOSE	= 10;
	BKC_MENU_COMPEDIT	= 11;
	BKC_MENU_COMPREF	= 12;

///////////////////////////////////////////////////////////////////////////
//
// Constants for BKC_ONSEND
//
const
	BKC_ONSEND_ERROR		= -1;
	BKC_ONSEND_PROCESSED	= -2;

///////////////////////////////////////////////////////////////////////////
//
// Constants for BKA_GetStatus
//
const
	MESSAGE_READ			= $00000001;
	MESSAGE_FORWARDED		= $00000002;
	MESSAGE_REPLIED			= $00000004;
	MESSAGE_ATTACHMENT		= $00000008;
	MESSAGE_PARTIAL			= $00000100;
	MESSAGE_REDIRECT		= $00000200;
	MESSAGE_FLAG			= $00001000;
	MESSAGE_TOME			= $00010000;
	MESSAGE_CCME			= $00020000;

///////////////////////////////////////////////////////////////////////////
//
// Constants for BKC_OnOpenCompose
//
const
	COMPOSE_MODE_COMPOSE1	= 0;
	COMPOSE_MODE_COMPOSE2	= 1;
	COMPOSE_MODE_COMPOSE3	= 2;
	COMPOSE_MODE_TEMPLATE	= 3;
	COMPOSE_MODE_REPLY1		= 5;
	COMPOSE_MODE_REPLY2		= 6;
	COMPOSE_MODE_REPLY3		= 7;
	COMPOSE_MODE_FORWARD1	= 10;
	COMPOSE_MODE_FORWARD2	= 11;
	COMPOSE_MODE_FORWARD3	= 12;

///////////////////////////////////////////////////////////////////////////
//
// Constants for BKA_RegisterUICallback
//
const
	BKMENU_CMDUI_DISABLED	=	1;
	BKMENU_CMDUI_CHECKED	=	2;

///////////////////////////////////////////////////////////////////////////
//
// Plugin Information Struct
//
type
	TBkPlugInInfo = record
		szPlugInName: array[0..79] of AnsiChar;		// Name of the plug-in
		szVendor: array[0..79] of AnsiChar;			// Name of the vendor
		szVersion: array[0..79] of AnsiChar;		// Version string
		szDescription: array[0..79] of AnsiChar;	// Short description about this plugin
	end;
	PBkPlugInInfo = ^TBkPlugInInfo;

///////////////////////////////////////////////////////////////////////////
//
// Callback Functions
//

type
	TBKA_CB_CmdProc = procedure(hw: HWND; lp: LPARAM); stdcall;
	TBKA_CB_CmdUIProc = function(hw: HWND; lp: LPARAM): Longword; stdcall;

///////////////////////////////////////////////////////////////////////////
//
// Function prototype.
//

type
	TBKA_GetVersion = function: PAnsiChar; stdcall;
	TBKA_Command = procedure(Wnd: HWND; lpCmd: PAnsiChar); stdcall;
	TBKA_GetWindowHandles = function(var lphMain, lphTree, lphList,
		lphView: HWND): LongBool; stdcall;
	TBKA_RegisterCommand = function(lpszComment: PAnsiChar; nTarget: Integer;
		lpCallback: TBKA_CB_CmdProc): Longword; stdcall;
	TBKA_RegisterUICallback = function(nID: Longword;
		lpCallBack: TBKA_CB_CmdUIProc): Longword; stdcall;
	TBKA_GetDataFolder = function: PAnsiChar; stdcall;
	TBKA_GetTempFolder = function: PAnsiChar; stdcall;
	TBKA_GetTempFileName = function(lpType: PAnsiChar): PAnsiChar; stdcall;
	TBKA_GetCurrentMailBox = function: PAnsiChar; stdcall;
	TBKA_SetCurrentMailBox = procedure(lpMailBox: PAnsiChar); stdcall;
	TBKA_GetCurrentFolder = function: PAnsiChar; stdcall;
	TBKA_SetCurrentFolder = procedure(lpFolderID: PAnsiChar); stdcall;
	TBKA_GetFolderDisplayName = function(lpFolderID: PAnsiChar): PAnsiChar;
		stdcall;
	TBKA_SetMessageText = procedure(Wnd: HWND; lpszMsg: PAnsiChar); stdcall;
	TBKA_GetCurrentMail = function: PAnsiChar; stdcall;
	TBKA_SetCurrentMail = procedure(lpMailID: PAnsiChar); stdcall;
	TBKA_GetNextMail = function(nStart: Integer; lpszMailID: PAnsiChar;
		nBuf: Integer; bSelected: LongBool): Integer; stdcall;
	TBKA_SetSel = procedure(lpMailID: PAnsiChar; bSel: LongBool); stdcall;
	TBKA_AppendMessage = function(lpFolderID, lpszData: PAnsiChar): LongBool;
		stdcall;
	TBKA_MoveSelectedMessages = function(lpFolderID: PAnsiChar; bCopy: LongBool):
		LongBool; stdcall;
	TBKA_GetStatus = function(lpMailID: PAnsiChar): DWORD; stdcall;
	TBKA_ComposeMail = function(lpURL: PAnsiChar): HWND; stdcall;
	TBKA_GetCharSet = function(lpMailID, pszCharSet: PAnsiChar; nBuf: Integer):
		Integer; stdcall;
	TBKA_GetSource = function(lpMailID: PAnsiChar): PAnsiChar; stdcall;
	TBKA_SetSource = procedure(lpMailID, lpSource: PAnsiChar); stdcall;
	TBKA_GetHeader = function(lpMailID: PAnsiChar): PAnsiChar; stdcall;
	TBKA_GetText = function(lpszMimeType: PAnsiChar; nBuf: Integer): PAnsiChar;
		stdcall;
	TBKA_SetText = procedure(nMode: Integer; lpText: PAnsiChar); stdcall;
	TBKA_GetSpecifiedHeader = procedure(lpHeader, lpszData: PAnsiChar;
		nBuf: Integer); stdcall;
	TBKA_SetSpecifiedHeader = procedure(lpHeader, lpszData: PAnsiChar);
		stdcall;
	TBKA_CompGetCharSet = function(Wnd: HWND; lpszCharSet: PAnsiChar;
		nBuf: Integer): Integer; stdcall;
	TBKA_CompGetSource = function(Wnd: HWND): PAnsiChar; stdcall;
	TBKA_CompSetSource = procedure(Wnd: HWND; lpSource: PAnsiChar); stdcall;
	TBKA_CompGetHeader = function(Wnd: HWND): PAnsiChar; stdcall;
	TBKA_CompGetSpecifiedHeader = procedure(Wnd: HWND; lpHeader, lpszData: PAnsiChar;
		nBuf: Integer); stdcall;
	TBKA_CompSetSpecifiedHeader = procedure(Wnd: HWND; lpHeader, lpszData: PAnsiChar);
		stdcall;
	TBKA_CompGetText = function(Wnd: HWND; lpszMimeType: PAnsiChar;
		nBuf: Integer): PAnsiChar; stdcall;
	TBKA_CompSetText = procedure(Wnd: HWND; nMode: Integer;
		lpText: PAnsiChar); stdcall;
	TBKA_CompAttachFile = procedure(Wnd: HWND; lpAttachFile,
		lpMimeType: PAnsiChar); stdcall;
	TBKA_Alloc = function(dwSize: DWORD): Pointer; stdcall;
	TBKA_ReAlloc = function(lpVoid: Pointer; dwSize: DWORD): Pointer;
		stdcall;
	TBKA_Free = procedure(lpVoid: Pointer);
	TBKA_ISO_2022_JP = function(lpSrc: PAnsiChar; bEncode: LongBool): PAnsiChar;
		stdcall;
	TBKA_ISO_2022_KR = function(lpSrc: PAnsiChar; bEncode: LongBool): PAnsiChar;
		stdcall;
	TBKA_HZ_GB2312 = function(lpSrc: PAnsiChar; bEncode: LongBool): PAnsiChar;
		stdcall;
	TBKA_ISO_8859_2 = function(lpSrc: PAnsiChar; bEncode: LongBool): PAnsiChar;
		stdcall;
	TBKA_EUC_JP = function(lpSrc: PAnsiChar; bEncode: LongBool): PAnsiChar;
		stdcall;
	TBKA_UTF_7 = function(lpSrc: PAnsiChar; bEncode: LongBool): PAnsiChar; stdcall;
	TBKA_UTF_8 = function(lpSrc: PAnsiChar; bEncode: LongBool): PAnsiChar; stdcall;
	TBKA_B64Convert = function(lpszOutFile, lpszInFile: PAnsiChar;
		bEncode: LongBool): LongBool; stdcall;
	TBKA_QPConvert = function(lpszOutFile, lpszInFile: PAnsiChar;
		bEncode: LongBool): LongBool; stdcall;
	TBKA_MIMEHeader = function(lpszIn, lpszCharSet: PAnsiChar; nBuf: Integer;
		bEncode: LongBool): PAnsiChar; stdcall;
	TBKA_SerializeRcpts = function(lpAddresses: PAnsiChar): PAnsiChar; stdcall;
	TBKA_Connect = function(bConnect: LongBool): LongBool; stdcall;

///////////////////////////////////////////////////////////////////////////
//
// Function Address & Other Variables
//

TIdxLine = record
	dwBodyPtr: DWORD;		    // このメールアイテムのbmfファイル中の先頭からの位置
	dwMsgID: DWORD;				// このメールアイテムをフォルダ中でユニークに識別する為のDWORD値
	dwFileName: DWORD;			// bmfファイルのファイル名部分
	strSubject: String;			// メールの件名
	strFrom: String;			// メールの差出人
	strTo: String;				// メールの宛先
	strMsgId: String;			// メールのMessage-Idフィールド
	strInreplyto: String;		// メールの参照先のMessage-Id
	tSend: Int64;				// メールの送信日時（C言語のtime_t値）（Dateフィールドより取得）
	tRecv: Int64;				// メールの配信日時（C言語のtime_t値）（Received フィールドより取得）
	tDnld: Int64;				// メールの受信日時（C言語のtime_t値）（受信時に決定）
	dwSize: DWORD;				// メールのサイズ（バイト数）
	dwStatus: DWORD;			// メールのステータスフラグ
	nColor: Integer;			// カラーラベルのCOLORREF値
	nPriority: Integer;			// ５段階の重要度
	dwParentID: DWORD;			// スレッド表示の際の親アイテムのdwMsgID
	strCharSet: String;			// このメールのキャラクタセット（空でも可）
	strTemp: String;			// テンポラリ文字列（内容は不定、通常空）
	strExtAtch: String;			// (v2.05より）添付ファイルを別ファイルに保存している場合、
	strCc: String;				// メールのCc
	nTemp: Integer;				// 何に使われているのかわからん
end;

TIdxLineCompare = class(TComparer<TIdxLine>)
	public
		function Compare(const Left, Right: TIdxLine): Integer; override;
end;

TBeckyAPI = class
	private
		_GetVersion					: TBKA_GetVersion;
		_Command					: TBKA_Command;
		_GetWindowHandles			: TBKA_GetWindowHandles;
		_RegisterCommand			: TBKA_RegisterCommand;
		_RegisterUICallback			: TBKA_RegisterUICallback;
		_GetDataFolder				: TBKA_GetDataFolder;
		_GetTempFolder				: TBKA_GetTempFolder;
		_GetTempFileName			: TBKA_GetTempFileName;
		_GetCurrentMailBox			: TBKA_GetCurrentMailBox;
		_SetCurrentMailBox			: TBKA_SetCurrentMailBox;
		_GetCurrentFolder			: TBKA_GetCurrentFolder;
		_SetCurrentFolder			: TBKA_SetCurrentFolder;
		_GetFolderDisplayName		: TBKA_GetFolderDisplayName;
		_SetMessageText				: TBKA_SetMessageText;
		_GetCurrentMail				: TBKA_GetCurrentMail;
		_SetCurrentMail				: TBKA_SetCurrentMail;
		_GetNextMail				: TBKA_GetNextMail;
		_SetSel						: TBKA_SetSel;
		_AppendMessage				: TBKA_AppendMessage;
		_MoveSelectedMessages		: TBKA_MoveSelectedMessages;
		_GetStatus					: TBKA_GetStatus;
		_ComposeMail				: TBKA_ComposeMail;
		_GetCharSet					: TBKA_GetCharSet;
		_GetSource					: TBKA_GetSource;
		_SetSource					: TBKA_SetSource;
		_GetHeader					: TBKA_GetHeader;
		_GetText					: TBKA_GetText;
		_SetText					: TBKA_SetText;
		_GetSpecifiedHeader			: TBKA_GetSpecifiedHeader;
		_SetSpecifiedHeader			: TBKA_SetSpecifiedHeader;
		_CompGetCharSet				: TBKA_CompGetCharSet;
		_CompGetSource				: TBKA_CompGetSource;
		_CompSetSource				: TBKA_CompSetSource;
		_CompGetHeader				: TBKA_CompGetHeader;
		_CompGetSpecifiedHeader		: TBKA_CompGetSpecifiedHeader;
		_CompSetSpecifiedHeader		: TBKA_CompSetSpecifiedHeader;
		_CompGetText				: TBKA_CompGetText;
		_CompSetText				: TBKA_CompSetText;
		_CompAttachFile				: TBKA_CompAttachFile;
		_AllocMem					: TBKA_Alloc;
		_ReAllocMem					: TBKA_ReAlloc;
		_FreeMem					: TBKA_Free;
		_ISO_2022_JP				: TBKA_ISO_2022_JP;
		_ISO_2022_KR				: TBKA_ISO_2022_KR;
		_HZ_GB2312					: TBKA_HZ_GB2312;
		_ISO_8859_2					: TBKA_ISO_8859_2;
		_EUC_JP						: TBKA_EUC_JP;
		_UTF_7						: TBKA_UTF_7;
		_UTF_8						: TBKA_UTF_8;
		_B64Convert					: TBKA_B64Convert;
		_QPConvert					: TBKA_QPConvert;
		_MIMEHeader					: TBKA_MIMEHeader;
		_SerializeRcpts				: TBKA_SerializeRcpts;
		_Connect					: TBKA_Connect;
		Bias: TDateTime;
		function isInitialized: Boolean;
		function EnterCriticalSection: Boolean;
		procedure LeaveCriticalSection;
	protected
		_BKA_hLib					: THandle;	// Handle of Becky Module
	public
		DataFolder: String;
		function ParseIdxLine(idxline: RawByteString): TIdxLine;
		function ReadIdx(fol: String; var idxary: TArray<TIdxLine>): Integer;
		function CanRW(fn: TFileName): Boolean;
		function ReadMailboxIni(fol, sect, key: AnsiString): AnsiString;
		function ReadFolIni(fol, key: AnsiString): AnsiString;
		procedure WriteFolIni(fol, key, data: AnsiString);
		function IsIMAPAccount(fol: String): Boolean;
		function time_tToDateTime(tm: Double): TDateTime;
		function DateTimeTotime_t(dt: TDateTime): Double;
		function Yen2Slash(s: String): String;
		function Slash2Yen(s: String): String;
		constructor Create;
		destructor Destroy;
		function InitAPI: Boolean;
		function GetVersion: RawByteString;
		procedure Command(wnd: HWND; cmd: RawByteString);
		function GetWindowHandles(var hMain, hTree, hList, hView: HWND): LongBool;
		function RegisterCommand(sComment: RawByteString; nTarget: Integer; lpCallback: TBKA_CB_CmdProc): Longword;
		function RegisterUICallback(nId: Longword; lpCallback: TBKA_CB_CmdUIProc): Longword;
		function GetDataFolder: RawByteString;
		function GetTempFolder: RawByteString;
		function GetTempFileName(sType: RawByteString): RawByteString;
		function GetCurrentMailBox: RawByteString;
		procedure SetCurrentMailBox(sMailBox: RawByteString);
		function GetCurrentFolder: RawByteString;
		procedure SetCurrentFolder(sFolderId: RawByteString);
		function GetFolderDisplayName(sFolderId: RawByteString): RawByteString;
		procedure SetMessageText(wnd: HWND; sMsg: RawByteString);
		function GetCurrentMail: RawByteString;
		procedure SetCurrentMail(sMailId: RawByteString);
		function GetNextMail(nStart: Integer; var sMailId: RawByteString; bSel: LongBool): Integer;
		procedure SetSel(sMailId: RawByteString; bSel: LongBool);
		function AppendMessage(sFolderId, sData: RawByteString): LongBool;
		function MoveSelectedMessages(sFolderId: RawByteString; bCopy: LongBool): LongBool;
		function GetStatus(sMailId: RawByteString): DWORD;
		function ComposeMail(sURL: RawByteString): HWND;
		function GetCharSet(sMailId: RawByteString; var sCharSet: RawByteString): Integer;
		function GetSource(sMailId: RawByteString): RawByteString;
		procedure SetSource(sMailId, sSource: RawByteString);
		function GetHeader(sMailId: RawByteString): RawByteString;
		function GetText(var sMimeType: RawByteString): RawByteString;
		procedure SetText(nMode: Integer; sText: RawByteString);
		procedure GetSpecifiedHeader(sHeader: RawByteString; var sData: RawByteString);
		procedure SetSpecifiedHeader(sHeader, sData: RawByteString);
		function CompGetCharSet(wnd: HWND; var sCharSet: RawByteString): Integer;
		function CompGetSource(wnd: HWND): RawByteString;
		procedure CompSetSource(wnd: HWND; sSource: RawByteString);
		function CompGetHeader(wnd: HWND): RawByteString;
		procedure CompGetSpecifiedHeader(wnd: HWND; sHeader: RawByteString; var sData: RawByteString);
		procedure CompSetSpecifiedHeader(wnd: HWND; sHeader, sData: RawByteString);
		function CompGetText(wnd: HWND; var sMimeType: RawByteString): RawByteString;
		procedure CompSetText(wnd: HWND; nMode: Integer; sText: RawByteString);
		procedure CompAttachFile(wnd: HWND; sAttachFile, sMimeType: RawByteString);
		function AllocMem(dwSize: DWORD): Pointer;
		function ReAllocMem(lpVoid: Pointer; dwSize: DWORD): Pointer;
		procedure FreeMem(lpVoid: Pointer);
		function ISO_2022_JP(sSrc: RawByteString; bEncode: LongBool): RawByteString;
		function ISO_2022_KR(sSrc: RawByteString; bEncode: LongBool): RawByteString;
		function HZ_GB2312(sSrc: RawByteString; bEncode: LongBool): RawByteString;
		function ISO_8859_2(sSrc: RawByteString; bEncode: LongBool): RawByteString;
		function EUC_JP(sSrc: RawByteString; bEncode: LongBool): RawByteString;
		function UTF_7(sSrc: RawByteString; bEncode: LongBool): RawByteString;
		function UTF_8(sSrc: RawByteString; bEncode: LongBool): RawByteString;
		function B64Convert(sOutFile, sInFile: RawByteString; bEncode: LongBool): LongBool;
		function QPConvert(sOutFile, sInFile: RawByteString; bEncode: LongBool): LongBool;
		function MIMEHeader(sIn: RawByteString; var sCharSet: RawByteString; bEncode: LongBool): RawByteString;
		function SerializeRcpts(sAddresses: RawByteString): RawByteString;
		function Connect(bConnect: LongBool): LongBool;
end;

const
	AppName = 'Bkroonga2';

var
	bka: TBeckyAPI;
	IniFileName: AnsiString; // Ini file to save your plugin settings.
	szProductName, szCompanyName, szFileVersion, szFileDescription: AnsiString;
	hwndMain, hwndTree, hwndList, hwndView: HWND;
	MyDllFileName: String;
	Exiting: Boolean;
	mtx: TMutex;

implementation

function TBeckyAPI.InitAPI: Boolean;
begin
	if _BKA_hLib <> 0 then begin
		Result := True;
		Exit;
	end;

	Result := False;
	//_BKA_hLib := LoadLibrary('B2.exe');
	_BKA_hLib := GetModuleHandle(nil);
	if _BKA_hLib = 0 then Exit;

	_GetVersion				:= TBKA_GetVersion(GetProcAddress(_BKA_hLib, 'BKA_GetVersion'));
	_Command				:= TBKA_Command(GetProcAddress(_BKA_hLib, 'BKA_Command'));
	_GetWindowHandles		:= TBKA_GetWindowHandles(GetProcAddress(_BKA_hLib, 'BKA_GetWindowHandles'));
	_RegisterCommand		:= TBKA_RegisterCommand(GetProcAddress(_BKA_hLib, 'BKA_RegisterCommand'));
	_RegisterUICallback		:= TBKA_RegisterUICallback(GetProcAddress(_BKA_hLib, 'BKA_RegisterUICallback'));
	_GetDataFolder			:= TBKA_GetDataFolder(GetProcAddress(_BKA_hLib, 'BKA_GetDataFolder'));
	_GetTempFolder			:= TBKA_GetTempFolder(GetProcAddress(_BKA_hLib, 'BKA_GetTempFolder'));
	_GetTempFileName		:= TBKA_GetTempFileName(GetProcAddress(_BKA_hLib, 'BKA_GetTempFileName'));
	_GetCurrentMailBox		:= TBKA_GetCurrentMailBox(GetProcAddress(_BKA_hLib, 'BKA_GetCurrentMailBox'));
	_SetCurrentMailBox		:= TBKA_SetCurrentMailBox(GetProcAddress(_BKA_hLib, 'BKA_SetCurrentMailBox'));
	_GetCurrentFolder		:= TBKA_GetCurrentFolder(GetProcAddress(_BKA_hLib, 'BKA_GetCurrentFolder'));
	_SetCurrentFolder		:= TBKA_SetCurrentFolder(GetProcAddress(_BKA_hLib, 'BKA_SetCurrentFolder'));
	_GetFolderDisplayName	:= TBKA_GetFolderDisplayName(GetProcAddress(_BKA_hLib, 'BKA_GetFolderDisplayName'));
	_SetMessageText			:= TBKA_SetMessageText(GetProcAddress(_BKA_hLib, 'BKA_SetMessageText'));
	_GetCurrentMail			:= TBKA_GetCurrentMail(GetProcAddress(_BKA_hLib, 'BKA_GetCurrentMail'));
	_SetCurrentMail			:= TBKA_SetCurrentMail(GetProcAddress(_BKA_hLib, 'BKA_SetCurrentMail'));
	_GetNextMail			:= TBKA_GetNextMail(GetProcAddress(_BKA_hLib, 'BKA_GetNextMail'));
	_SetSel					:= TBKA_SetSel(GetProcAddress(_BKA_hLib, 'BKA_SetSel'));
	_AppendMessage			:= TBKA_AppendMessage(GetProcAddress(_BKA_hLib, 'BKA_AppendMessage'));
	_MoveSelectedMessages	:= TBKA_MoveSelectedMessages(GetProcAddress(_BKA_hLib, 'BKA_MoveSelectedMessages'));
	_GetStatus				:= TBKA_GetStatus(GetProcAddress(_BKA_hLib, 'BKA_GetStatus'));
	_ComposeMail			:= TBKA_ComposeMail(GetProcAddress(_BKA_hLib, 'BKA_ComposeMail'));
	_GetCharSet				:= TBKA_GetCharSet(GetProcAddress(_BKA_hLib, 'BKA_GetCharSet'));
	_GetSource				:= TBKA_GetSource(GetProcAddress(_BKA_hLib, 'BKA_GetSource'));
	_SetSource				:= TBKA_SetSource(GetProcAddress(_BKA_hLib, 'BKA_SetSource'));
	_GetHeader				:= TBKA_GetHeader(GetProcAddress(_BKA_hLib, 'BKA_GetHeader'));
	_GetText				:= TBKA_GetText(GetProcAddress(_BKA_hLib, 'BKA_GetText'));
	_SetText				:= TBKA_SetText(GetProcAddress(_BKA_hLib, 'BKA_SetText'));
	_GetSpecifiedHeader		:= TBKA_GetSpecifiedHeader(GetProcAddress(_BKA_hLib, 'BKA_GetSpecifiedHeader'));
	_SetSpecifiedHeader		:= TBKA_SetSpecifiedHeader(GetProcAddress(_BKA_hLib, 'BKA_SetSpecifiedHeader'));
	_CompGetCharSet			:= TBKA_CompGetCharSet(GetProcAddress(_BKA_hLib, 'BKA_CompGetCharSet'));
	_CompGetSource			:= TBKA_CompGetSource(GetProcAddress(_BKA_hLib, 'BKA_CompGetSource'));
	_CompSetSource			:= TBKA_CompSetSource(GetProcAddress(_BKA_hLib, 'BKA_CompSetSource'));
	_CompGetHeader			:= TBKA_CompGetHeader(GetProcAddress(_BKA_hLib, 'BKA_CompGetHeader'));
	_CompGetSpecifiedHeader	:= TBKA_CompGetSpecifiedHeader(GetProcAddress(_BKA_hLib, 'BKA_CompGetSpecifiedHeader'));
	_CompSetSpecifiedHeader	:= TBKA_CompSetSpecifiedHeader(GetProcAddress(_BKA_hLib, 'BKA_CompSetSpecifiedHeader'));
	_CompGetText			:= TBKA_CompGetText(GetProcAddress(_BKA_hLib, 'BKA_CompGetText'));
	_CompSetText			:= TBKA_CompSetText(GetProcAddress(_BKA_hLib, 'BKA_CompSetText'));
	_CompAttachFile			:= TBKA_CompAttachFile(GetProcAddress(_BKA_hLib, 'BKA_CompAttachFile'));
	_AllocMem				:= TBKA_Alloc(GetProcAddress(_BKA_hLib, 'BKA_Alloc'));
	_ReAllocMem				:= TBKA_ReAlloc(GetProcAddress(_BKA_hLib, 'BKA_ReAlloc'));
	_FreeMem				:= TBKA_Free(GetProcAddress(_BKA_hLib, 'BKA_Free'));
	_ISO_2022_JP			:= TBKA_ISO_2022_JP(GetProcAddress(_BKA_hLib, 'BKA_ISO_2022_JP'));
	_ISO_2022_KR			:= TBKA_ISO_2022_KR(GetProcAddress(_BKA_hLib, 'BKA_ISO_2022_KR'));
	_HZ_GB2312				:= TBKA_HZ_GB2312(GetProcAddress(_BKA_hLib, 'BKA_HZ_GB2312'));
	_ISO_8859_2				:= TBKA_ISO_8859_2(GetProcAddress(_BKA_hLib, 'BKA_ISO_8859_2'));
	_EUC_JP					:= TBKA_EUC_JP(GetProcAddress(_BKA_hLib, 'BKA_EUC_JP'));
	_UTF_7					:= TBKA_UTF_7(GetProcAddress(_BKA_hLib, 'BKA_UTF_7'));
	_UTF_8					:= TBKA_UTF_8(GetProcAddress(_BKA_hLib, 'BKA_UTF_8'));
	_B64Convert				:= TBKA_B64Convert(GetProcAddress(_BKA_hLib, 'BKA_B64Convert'));
	_QPConvert				:= TBKA_QPConvert(GetProcAddress(_BKA_hLib, 'BKA_QPConvert'));
	_MIMEHeader				:= TBKA_MIMEHeader(GetProcAddress(_BKA_hLib, 'BKA_MIMEHeader'));
	_SerializeRcpts			:= TBKA_SerializeRcpts(GetProcAddress(_BKA_hLib, 'BKA_SerializeRcpts'));
	_Connect				:= TBKA_Connect(GetProcAddress(_BKA_hLib, 'BKA_Connect'));

	if (not Assigned(_GetVersion)) or
		(not Assigned(_Command)) or
		(not Assigned(_GetWindowHandles)) or
		(not Assigned(_RegisterCommand)) or
		(not Assigned(_RegisterUICallback)) or
		(not Assigned(_GetDataFolder)) or
		(not Assigned(_GetTempFolder)) or
		(not Assigned(_GetTempFileName)) or
		(not Assigned(_GetCurrentMailBox)) or
		(not Assigned(_SetCurrentMailBox)) or
		(not Assigned(_GetCurrentFolder)) or
		(not Assigned(_SetCurrentFolder)) or
		(not Assigned(_GetFolderDisplayName)) or
		(not Assigned(_SetMessageText)) or
		(not Assigned(_GetCurrentMail)) or
		(not Assigned(_SetCurrentMail)) or
		(not Assigned(_GetNextMail)) or
		(not Assigned(_SetSel)) or
		(not Assigned(_AppendMessage)) or
		(not Assigned(_MoveSelectedMessages)) or
		(not Assigned(_GetStatus)) or
		(not Assigned(_ComposeMail)) or
		(not Assigned(_GetCharSet)) or
		(not Assigned(_GetSource)) or
		(not Assigned(_SetSource)) or
		(not Assigned(_GetHeader)) or
		(not Assigned(_GetText)) or
		(not Assigned(_SetText)) or
		(not Assigned(_GetSpecifiedHeader)) or
		(not Assigned(_SetSpecifiedHeader)) or
		(not Assigned(_CompGetCharSet)) or
		(not Assigned(_CompGetSource)) or
		(not Assigned(_CompSetSource)) or
		(not Assigned(_CompGetHeader)) or
		(not Assigned(_CompGetSpecifiedHeader)) or
		(not Assigned(_CompSetSpecifiedHeader)) or
		(not Assigned(_CompGetText)) or
		(not Assigned(_CompSetText)) or
		(not Assigned(_CompAttachFile)) or
		(not Assigned(_AllocMem)) or
		(not Assigned(_ReAllocMem)) or
		(not Assigned(_FreeMem)) or
		(not Assigned(_ISO_2022_JP)) or
		(not Assigned(_ISO_2022_KR)) or
		(not Assigned(_HZ_GB2312)) or
		(not Assigned(_ISO_8859_2)) or
		(not Assigned(_EUC_JP)) or
		(not Assigned(_UTF_7)) or
		(not Assigned(_UTF_8)) or
		(not Assigned(_B64Convert)) or
		(not Assigned(_QPConvert)) or
		(not Assigned(_MIMEHeader)) or
		(not Assigned(_SerializeRcpts)) or
		(not Assigned(_Connect)) then begin
		_GetVersion := nil;
		_Command := nil;
		_GetWindowHandles := nil;
		_RegisterCommand := nil;
		_GetDataFolder := nil;
		_GetTempFolder := nil;
		_GetTempFileName := nil;
		_GetCurrentMailBox := nil;
		_SetCurrentMailBox := nil;
		_GetCurrentFolder := nil;
		_SetCurrentFolder := nil;
		_GetFolderDisplayName := nil;
		_SetMessageText := nil;
		_GetCurrentMail := nil;
		_SetCurrentMail := nil;
		_GetNextMail := nil;
		_SetSel := nil;
		_AppendMessage := nil;
		_MoveSelectedMessages := nil;
		_GetStatus := nil;
		_ComposeMail := nil;
		_GetCharSet := nil;
		_GetSource := nil;
		_SetSource := nil;
		_GetHeader := nil;
		_GetText := nil;
		_SetText := nil;
		_GetSpecifiedHeader := nil;
		_SetSpecifiedHeader := nil;
		_CompGetCharSet := nil;
		_CompGetSource := nil;
		_CompSetSource := nil;
		_CompGetHeader := nil;
		_CompGetSpecifiedHeader := nil;
		_CompSetSpecifiedHeader := nil;
		_CompGetText := nil;
		_CompSetText := nil;
		_CompAttachFile := nil;
		_AllocMem := nil;
		_ReAllocMem := nil;
		_FreeMem := nil;
		_ISO_2022_JP := nil;
		_ISO_2022_KR := nil;
		_HZ_GB2312 := nil;
		_ISO_8859_2 := nil;
		_EUC_JP := nil;
		_UTF_7 := nil;
		_UTF_8 := nil;
		_B64Convert := nil;
		_QPConvert := nil;
		_MIMEHeader := nil;
		_SerializeRcpts := nil;
		_Connect := nil;
		//FreeLibrary(_BKA_hLib);
		Result := False;
	end else begin
		Result := True;
		DataFolder := IncludeTrailingPathDelimiter(self._GetDataFolder);
	end;
end;

constructor TBeckyAPI.Create;
var
	TimeZoneInformation: TTimeZoneInformation;
begin
	_BKA_hLib := 0;
	mtx := TMutex.Create();
	GetTimeZoneInformation(TimeZoneInformation);
	Bias := (365*70+19) - TimeZoneInformation.Bias / (60*24);
end;

function TBeckyAPI.DateTimeTotime_t(dt: TDateTime): Double;
begin
	Result := (dt - Bias) * (24*60*60);
end;

destructor TBeckyAPI.Destroy;
begin
	//if _BKA_hLib <> 0 then
	//	FreeLibrary(_BKA_hLib);
	_BKA_hLib := 0;

	_GetVersion := nil;
	_Command := nil;
	_GetWindowHandles := nil;
	_RegisterCommand := nil;
	_GetDataFolder := nil;
	_GetTempFolder := nil;
	_GetTempFileName := nil;
	_GetCurrentMailBox := nil;
	_SetCurrentMailBox := nil;
	_GetCurrentFolder := nil;
	_SetCurrentFolder := nil;
	_GetFolderDisplayName := nil;
	_SetMessageText := nil;
	_GetCurrentMail := nil;
	_SetCurrentMail := nil;
	_GetNextMail := nil;
	_SetSel := nil;
	_AppendMessage := nil;
	_MoveSelectedMessages := nil;
	_GetStatus := nil;
	_ComposeMail := nil;
	_GetCharSet := nil;
	_GetSource := nil;
	_SetSource := nil;
	_GetHeader := nil;
	_GetText := nil;
	_SetText := nil;
	_GetSpecifiedHeader := nil;
	_SetSpecifiedHeader := nil;
	_CompGetCharSet := nil;
	_CompGetSource := nil;
	_CompSetSource := nil;
	_CompGetHeader := nil;
	_CompGetSpecifiedHeader := nil;
	_CompSetSpecifiedHeader := nil;
	_CompGetText := nil;
	_CompSetText := nil;
	_CompAttachFile := nil;
	_AllocMem := nil;
	_ReAllocMem := nil;
	_FreeMem := nil;
	_ISO_2022_JP := nil;
	_ISO_2022_KR := nil;
	_HZ_GB2312 := nil;
	_ISO_8859_2 := nil;
	_EUC_JP := nil;
	_UTF_7 := nil;
	_UTF_8 := nil;
	_B64Convert := nil;
	_QPConvert := nil;
	_MIMEHeader := nil;
	_SerializeRcpts := nil;
	_Connect := nil;

	FreeAndNil(mtx);
end;

function TBeckyAPI.IsIMAPAccount(fol: String): Boolean;
begin
	Result := (ReadMailboxIni(fol, 'Account', 'Protocol') = '1');
end;

function TBeckyAPI.isInitialized: Boolean;
begin
	Result := (_BKA_hLib <> 0);
end;

function TBeckyAPI.EnterCriticalSection: Boolean;
begin
	// 排他制御をかけると稀にデッドロックを起こしてしまう
	// Becky!側でスレッドセーフにしていると信じて排他は特に行わない
	Result := True;
	//mtx.Acquire;
end;

procedure TBeckyAPI.LeaveCriticalSection;
begin
	//mtx.Release;
end;

function TBeckyAPI.MIMEHeader(sIn: RawByteString; var sCharSet: RawByteString;
  bEncode: LongBool): RawByteString;
var
    pc: PAnsiChar;
begin
	//logger.verbose('TBeckyAPI', 'MIMEHeader '+IntToStr(Integer(bEncode))+','+Copy(sIn, 1, 16));
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		SetLength(sCharSet, 256);
		pc := self._MIMEHeader(PAnsiChar(sIn), PAnsiChar(sCharSet), 255, bEncode);
		Result := PAnsiChar(pc);
		sCharSet := PAnsiChar(sCharSet);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.MoveSelectedMessages(sFolderId: RawByteString;
  bCopy: LongBool): LongBool;
begin
	logger.verbose('TBeckyAPI', 'MoveSelectedMessages '+sFolderId);
	Result := False;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		Result := self._MoveSelectedMessages(PAnsiChar(AnsiString(sFolderId)), bCopy);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.ParseIdxLine(idxline: RawByteString): TIdxLine;
var
	rs, tmp1, tmp2, tmp3: RawByteString;
	cp: Word;
	i, st, idx: Integer;
begin
	st := 1;
	idx := 0;
	//logger.debug('TBeckyAPI', ' '+Copy(idxline, 1, 16));
	for i := 1 to Length(idxline) do begin
		if idxline[i] = #$01 then begin
			rs := Copy(idxline, st, i - st);
			st := i + 1;
			case idx of
				0:	Result.dwBodyPtr := StrToIntDef('$'+rs, 0);
				1:	Result.dwMsgID := StrToIntDef('$'+rs, 0);
				2:	Result.dwFileName := StrToIntDef('$'+rs, 0);
				3:	tmp1 := rs;
				4:	tmp2 := rs;
				5:	tmp3 := rs;
				6:	Result.strMsgId := rs;
				7:	Result.strInreplyto := rs;
				8:	Result.tSend := StrToInt64Def('$'+rs, 0);
				9:	Result.tRecv := StrToInt64Def('$'+rs, 0);
				10:	Result.tDnld := StrToInt64Def('$'+rs, 0);
				11: Result.dwSize := StrToIntDef('$'+rs, 0);
				12: Result.dwStatus := StrToIntDef('$'+rs, 0);
				13: Result.nColor := StrToIntDef('$'+rs, 0);
				14: Result.nPriority := StrToIntDef('$'+rs, 0);
				15: Result.dwParentID := StrToIntDef('$'+rs, 0);
				16: Result.strCharSet := String(rs);
				17: Result.strTemp := String(rs);
				18: Result.strExtAtch := String(rs);
				19: begin
					if Length(rs) > 0 then begin
						if AnsiPos('utf', AnsiLowerCase(Copy(rs, 1, Pos(';', rs)-1))) > 0 then
							SetCodePage(tmp1, CP_UTF8, False)
						else
							if AnsiPos('utf', AnsiLowerCase(Result.strCharSet)) > 0 then
								SetCodePage(tmp1, CP_UTF8, False)
							else
								SetCodePage(tmp1, 932, False);	// CP_932
						rs := Copy(rs, Pos(';', rs)+1, Length(rs));
						if AnsiPos('utf', AnsiLowerCase(Copy(rs, 1, Pos(';', rs)-1))) > 0 then
							SetCodePage(tmp2, CP_UTF8, False)
						else
							if AnsiPos('utf', AnsiLowerCase(Result.strCharSet)) > 0 then
								SetCodePage(tmp2, CP_UTF8, False)
							else
								SetCodePage(tmp2, 932, False);	// CP_932
						rs := Copy(rs, Pos(';', rs)+1, Length(rs));
						if AnsiPos('utf', AnsiLowerCase(rs)) > 0 then
							SetCodePage(tmp3, CP_UTF8, False)
						else
							if AnsiPos('utf', AnsiLowerCase(Result.strCharSet)) > 0 then
								SetCodePage(tmp3, CP_UTF8, False)
							else
								SetCodePage(tmp3, 932, False);	// CP_932
					end else begin
						if AnsiPos('utf', AnsiLowerCase(Result.strCharSet)) > 0 then begin
							SetCodePage(tmp1, CP_UTF8, False);
							SetCodePage(tmp2, CP_UTF8, False);
							SetCodePage(tmp3, CP_UTF8, False);
						end else begin
							SetCodePage(tmp1, 932, False);  // CP_932
							SetCodePage(tmp2, 932, False);  // CP_932
							SetCodePage(tmp3, 932, False);  // CP_932
						end;
					end;
					Result.strSubject := tmp1;
					Result.strFrom := tmp2;
					Result.strTo := tmp3;
				end;
				20: Result.strCc := String(rs);
				else	;
			end;
			idx := idx + 1;
			if idx > 20 then break;
		end;
	end;
end;

function TBeckyAPI.ReadFolIni(fol, key: AnsiString): AnsiString;
var
	inifn: String;
begin
	try
		Result := '';
		inifn := IncludeTrailingPathDelimiter(DataFolder+fol)+'folder.ini';
		if not FileExists(inifn) then begin
			logger.error(self.ClassName, 'FolIni Not Exists('+inifn+') in ReadFolIni');
			Exit;
		end;
		SetLength(Result, 256);
		GetPrivateProfileStringA(PAnsiChar(AppName), PAnsiChar(key), '', PAnsiChar(Result), 255, PAnsiChar(AnsiString(inifn)));
		Result := Trim(PAnsiChar(Result));
	except
		on E: Exception do begin
			logger.error(self.ClassName, 'ReadFolIni '+E.Message+E.StackTrace);
		end;
	end;
end;

function TBeckyAPI.ReadIdx(fol: String; var idxary: TArray<TIdxLine>): Integer;
var
	fn: String;
	f: TFileStream;
	s: RawByteString;
	i, st, ed, idx: Integer;
	line: RawByteString;
begin
	logger.debug('TBeckyAPI', 'ReadIdx '+fol);
	Result := 0;
	try
		fn := IncludeTrailingPathDelimiter(DataFolder + fol) + 'folder.idx';
		if not FileExists(fn) then Exit;
		f := TFileStream.Create(fn, fmOpenRead);
	except on E: Exception do
		Exit;
	end;
	try
		SetLength(s, f.Size);
		f.ReadBuffer(s[1], f.Size);
	finally
		f.Free;
	end;
	st := 1;
	idx := 0;
	SetLength(idxary, 0);
	for i := 1 to Length(s) do begin
		if Ord(s[i]) = $a then begin
			while((Ord(s[st]) <= $20) and (st < Length(s)) and (st < i)) do st := st + 1;
			ed := i;
			while((Ord(s[ed]) <= $20) and (ed > st)) do ed := ed - 1;
			line := RawByteString(Copy(s, st, ed - st + 1));
			if idx >= 1 then begin
				idxary := idxary + [ParseIdxLine(line)];
			end;
			st := i + 1;
			idx := idx + 1;
		end;
	end;
	TArray.Sort<TIdxLine>(idxary, TIdxLineCompare.Create);
	Result := idx - 1;
end;

function TBeckyAPI.ReadMailboxIni(fol, sect, key: AnsiString): AnsiString;
var
	mb: AnsiString;
	m: TMatch;
begin
	logger.verbose('TBeckyAPI', 'ReadMailboxIni '+fol+','+sect+','+key);
	m := TRegEx.Match(fol, '([\dA-Za-z]+\.mb.*?)\\', [roIgnoreCase]);
	if m.Success then begin
		SetLength(Result, 256);
		mb := AnsiString(IncludeTrailingPathDelimiter(DataFolder + m.Groups.Item[1].Value) + 'Mailbox.ini');
		GetPrivateProfileStringA(PAnsiChar(AnsiString(sect)), PAnsiChar(AnsiString(key)), PAnsiChar(''), PAnsiChar(Result), 255, PAnsiChar(mb));
		Result := Trim(PAnsiChar(Result));
	end;
end;

function TBeckyAPI.Yen2Slash(s: String): String;
begin
	Result := StringReplace(s, '\', '/', [rfReplaceAll]);
end;

function TBeckyAPI.Slash2Yen(s: String): String;
begin
	Result := StringReplace(s, '/', '\', [rfReplaceAll]);
end;

function TBeckyAPI.time_tToDateTime(tm: Double): TDateTime;
begin
	Result := tm / (24*60*60) + Bias;
end;

procedure TBeckyAPI.WriteFolIni(fol, key, data: AnsiString);
var
	inifn: String;
begin
	try
		inifn := IncludeTrailingPathDelimiter(DataFolder + fol) + 'folder.ini';
		if not FileExists(inifn) then begin
			logger.error(self.ClassName, 'FolIni Not Exists('+inifn+') in WriteFolIni');
			Exit;
		end;
		WritePrivateProfileStringA(PAnsiChar(AppName), PAnsiChar(key), PAnsiChar(data), PAnsiChar(AnsiString(inifn)));
	except
		on E: Exception do begin
			logger.error(self.ClassName, 'WriteFolIni '+E.Message+E.StackTrace);
		end;
	end;
end;

function TBeckyAPI.CanRW(fn: TFileName): Boolean;
var
	tmpf: File;
	dirn, tmpfn: TFileName;
	i: Integer;
begin
	Result := False;
	if FileExists(fn) then begin
		AssignFile(tmpf, fn);
		try
			try
				FileMode := 2;
				Reset(tmpf);
			except
				Exit;
			end;
		finally
			try
				CloseFile(tmpf);
			except
                ;
			end;
		end;
		Result := True;
	end;
	if DirectoryExists(ExcludeTrailingPathDelimiter(fn)) then begin
		tmpfn := ExcludeTrailingPathDelimiter(fn) + '\tmpf';
		i := 0;
		while FileExists(tmpfn) do begin
			tmpfn := tmpfn + '0';
			i := i + 1;
			if i > 10 then Exit;
		end;
		try
			try
				AssignFile(tmpf, tmpfn);
				Rewrite(tmpf);
				CloseFile(tmpf);
			except
				Exit;
			end;
		finally
			try
				DeleteFile(tmpfn);
			except
                ;
			end;
		end;
		Result := True;
		Exit;
	end;
end;

function TBeckyAPI.ISO_2022_JP(sSrc: RawByteString;
  bEncode: LongBool): RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'ISO_2022_JP '+IntToStr(Integer(bEncode))+','+Copy(sSrc, 1, 16));
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._ISO_2022_JP(PAnsiChar(AnsiString(sSrc)), bEncode);
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.ISO_2022_KR(sSrc: RawByteString;
  bEncode: LongBool): RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'ISO_2022_KR '+IntToStr(Integer(bEncode))+','+Copy(sSrc, 1, 16));
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._ISO_2022_KR(PAnsiChar(AnsiString(sSrc)), bEncode);
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.ISO_8859_2(sSrc: RawByteString;
  bEncode: LongBool): RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'ISO_8859_2 '+IntToStr(Integer(bEncode))+','+Copy(sSrc, 1, 16));
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._ISO_8859_2(PAnsiChar(AnsiString(sSrc)), bEncode);
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.QPConvert(sOutFile, sInFile: RawByteString;
  bEncode: LongBool): LongBool;
begin
	logger.verbose('TBeckyAPI', 'QPConvert '+IntToStr(Integer(bEncode))+','+sInFile+'->'+sOutFile);
	Result := False;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		Result := self._QPConvert(PAnsiChar(AnsiString(sOutFile)), PAnsiChar(AnsiString(sInFile)), bEncode);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.ReAllocMem(lpVoid: Pointer; dwSize: DWORD): Pointer;
begin
	logger.verbose('TBeckyAPI', 'ReAllocMem');
	Result := nil;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		Result := self._ReAllocMem(lpVoid, dwSize);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.RegisterCommand(sComment: RawByteString;
  nTarget: Integer; lpCallback: TBKA_CB_CmdProc): Longword;
begin
	logger.verbose('TBeckyAPI', 'RegisterCommand '+sComment);
	Result := 0;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		Result := self._RegisterCommand(PAnsiChar(AnsiString(sComment)), nTarget, lpCallback);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.RegisterUICallback(nId: Longword;
  lpCallback: TBKA_CB_CmdUIProc): Longword;
begin
	logger.verbose('TBeckyAPI', 'RegisterUICallback');
	Result := 0;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		Result := self._RegisterUICallback(nId, lpCallback);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.SerializeRcpts(
  sAddresses: RawByteString): RawByteString;
var
    pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'SerializeRcpts');
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._SerializeRcpts(PAnsiChar(AnsiString(sAddresses)));
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.SetCurrentFolder(sFolderId: RawByteString);
begin
	logger.verbose('TBeckyAPI', 'SetCurrentFolder '+sFolderId);
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		self._SetCurrentFolder(PAnsiChar(AnsiString(sFolderId)));
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.SetCurrentMail(sMailId: RawByteString);
begin
	logger.verbose('TBeckyAPI', 'SetCurrentMail '+sMailId);
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		self._SetCurrentMail(PAnsiChar(AnsiReplaceText(AnsiString(sMailId), '/', '\')));
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.SetCurrentMailBox(sMailBox: RawByteString);
begin
	logger.verbose('TBeckyAPI', 'SetCurrentMailBox '+sMailBox);
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.SetMessageText(wnd: HWND; sMsg: RawByteString);
begin
	logger.verbose('TBeckyAPI', 'SetMessageText '+IntToHEx(wnd,8)+','+sMsg);
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		self._SetMessageText(wnd, PAnsiChar(AnsiString(sMsg)));
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.SetSel(sMailId: RawByteString; bSel: LongBool);
begin
	logger.verbose('TBeckyAPI', 'SetSel '+sMailId+','+IntToStr(Integer(bSel)));
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		self._SetSel(PAnsiChar(AnsiReplaceText(AnsiString(sMailId), '/', '\')), bSel);
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.SetSource(sMailId, sSource: RawByteString);
begin
	logger.verbose('TBeckyAPI', 'SetSource '+sMailId);
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		self._SetSource(PAnsiChar(AnsiReplaceText(AnsiString(sMailId), '/', '\')), PAnsiChar(AnsiString(sSource)));
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.SetSpecifiedHeader(sHeader, sData: RawByteString);
begin
	logger.verbose('TBeckyAPI', 'SetSpecifiedHeader '+sHeader);
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		self._SetSpecifiedHeader(PAnsiChar(AnsiString(sHeader)), PAnsiChar(AnsiString(sData)));
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.SetText(nMode: Integer; sText: RawByteString);
begin
	logger.verbose('TBeckyAPI', 'SetText');
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		self._SetText(nMode, PAnsiChar(AnsiString(sText)));
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.UTF_7(sSrc: RawByteString;
  bEncode: LongBool): RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'UTF_7 '+IntToStr(Integer(bEncode))+','+Copy(sSrc, 1, 16));
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._UTF_7(PAnsiChar(AnsiString(sSrc)), bEncode);
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.UTF_8(sSrc: RawByteString;
  bEncode: LongBool): RawByteString;
var
    pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'UTF_8 '+IntToStr(Integer(bEncode))+','+Copy(sSrc, 1, 16));
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._UTF_8(PAnsiChar(AnsiString(sSrc)), bEncode);
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.AllocMem(dwSize: DWORD): Pointer;
begin
	logger.verbose('TBeckyAPI', 'AllocMem');
	Result := nil;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		Result := self._AllocMem(dwSize);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.AppendMessage(sFolderId, sData: RawByteString): LongBool;
begin
	logger.verbose('TBeckyAPI', 'AppendMessage '+sFolderId);
	Result := False;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		Result := self._AppendMessage(PAnsiChar(AnsiString(sFolderId)), PAnsiChar(AnsiString(sData)));
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.B64Convert(sOutFile, sInFile: RawByteString;
  bEncode: LongBool): LongBool;
begin
	logger.verbose('TBeckyAPI', 'B64Convert '+IntToStr(Integer(bEncode))+','+sInFile+'->'+sOutFile);
	Result := False;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		Result := self._B64Convert(PAnsiChar(AnsiString(sOutFile)), PAnsiChar(AnsiString(sInFile)), bEncode);
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.Command(wnd: HWND; cmd: RawByteString);
begin
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		self._Command(wnd, PAnsiChar(AnsiString(cmd)));
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.CompAttachFile(wnd: HWND; sAttachFile,
  sMimeType: RawByteString);
begin
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		self._CompAttachFile(wnd, PAnsiChar(AnsiString(sAttachFile)), PAnsiChar(AnsiString(sMimeType)));
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.CompGetCharSet(wnd: HWND; var sCharSet: RawByteString): Integer;
begin
	Result := 0;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		SetLength(sCharSet, 256);
		Result := self._CompGetCharSet(wnd, PAnsiChar(sCharSet), 256);
		sCharSet := PAnsiChar(sCharSet);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.CompGetHeader(wnd: HWND): RawByteString;
var
	pc: PAnsiChar;
begin
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._CompGetHeader(wnd);
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.CompGetSource(wnd: HWND): RawByteString;
var
	pc: PAnsiChar;
begin
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._CompGetSource(wnd);
		Result := PAnsiChar(pc);
        self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.CompGetSpecifiedHeader(wnd: HWND; sHeader: RawByteString;
  var sData: RawByteString);
begin
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		SetLength(sData, 4096);
		self._CompGetSpecifiedHeader(wnd, PAnsiChar(AnsiString(sHeader)), PAnsiChar(sData), 4096);
		sData := PAnsiChar(sData);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.CompGetText(wnd: HWND; var sMimeType: RawByteString): RawByteString;
var
	pc: PAnsiChar;
begin
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		SetLength(sMimeType, 256);
		pc := self._CompGetText(wnd, PAnsiChar(sMimeType), 256);
		Result := PAnsiChar(pc);
		sMimeType := PAnsiChar(sMimeType);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.ComposeMail(sURL: RawByteString): HWND;
begin
	Result := 0;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		Result := self._ComposeMail(PAnsiChar(AnsiString(sURL)));
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.CompSetSource(wnd: HWND; sSource: RawByteString);
begin
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		self._CompSetSource(wnd, PAnsiChar(AnsiString(sSource)));
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.CompSetSpecifiedHeader(wnd: HWND; sHeader,
  sData: RawByteString);
begin
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		self._CompSetSpecifiedHeader(wnd, PAnsiChar(AnsiString(sHeader)), PAnsiChar(AnsiString(sData)));
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.CompSetText(wnd: HWND; nMode: Integer;
  sText: RawByteString);
begin
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		self._CompSetText(wnd, nMode, PAnsiChar(AnsiString(sText)));
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.Connect(bConnect: LongBool): LongBool;
begin
	logger.verbose('TBeckyAPI', 'Connect '+IntToStr(Integer(bConnect)));
	Result := False;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		Result := self._Connect(bConnect);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.EUC_JP(sSrc: RawByteString;
  bEncode: LongBool): RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'EUC_JP '+IntToStr(Integer(bEncode))+','+Copy(sSrc, 1, 16));
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._EUC_JP(PAnsiChar(AnsiString(sSrc)), bEncode);
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.FreeMem(lpVoid: Pointer);
begin
	logger.verbose('TBeckyAPI', 'FreeMem');
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		self._FreeMem(lpVoid);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.GetCharSet(sMailId: RawByteString; var sCharSet: RawByteString): Integer;
begin
	logger.verbose('TBeckyAPI', 'GetCharSet '+sMailId);
	Result := 0;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		SetLength(sCharSet, 256);
		Result := self._GetCharSet(PAnsiChar(AnsiReplaceText(AnsiString(sMailId), '/', '\')), PAnsiChar(sCharSet), 256);
		sCharSet := PAnsiChar(sCharSet);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.GetCurrentFolder: RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'GetCurrentFolder');
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._GetCurrentFolder;
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.GetCurrentMail: RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'GetCurrentMail');
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._GetCurrentMail;
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
	Result := AnsiReplaceText(Result, '\', '/');
end;

function TBeckyAPI.GetCurrentMailBox: RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'GetCurrentMailBox');
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._GetCurrentMailBox;
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.GetDataFolder: RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'GetDataFolder');
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._GetDataFolder;
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.GetFolderDisplayName(
  sFolderId: RawByteString): RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'GetFolderDisplayName '+sFolderId);
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._GetFolderDisplayName(PAnsiChar(AnsiString(sFolderId)));
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
	Result := AnsiReplaceText(Result, '\', '/');
end;

function TBeckyAPI.GetHeader(sMailId: RawByteString): RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'GetHeader '+sMailId);
	Result := '';
	if not isInitialized then Exit;
	sMailId := AnsiReplaceText(AnsiString(sMailId), '/', '\');
	EnterCriticalSection;
	try
		pc := self._GetHeader(PAnsiChar(sMailId));
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.GetNextMail(nStart: Integer; var sMailId: RawByteString;
  bSel: LongBool): Integer;
begin
	logger.verbose('TBeckyAPI', 'GetNextMail');
	Result := 0;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		SetLength(sMailId, 1024);
		Result := self._GetNextMail(nStart, PAnsiChar(sMailId), 1024, bSel);
		sMailId := AnsiReplaceText(PAnsiChar(sMailId), '\', '/');
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.GetSource(sMailId: RawByteString): RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'GetSource '+sMailId);
	Result := '';
	if not isInitialized then Exit;
	sMailId := AnsiReplaceText(AnsiString(sMailId), '/', '\');
	EnterCriticalSection;
	try
		pc := self._GetSource(PAnsiChar(sMailId));
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

procedure TBeckyAPI.GetSpecifiedHeader(sHeader: RawByteString; var sData: RawByteString);
begin
	logger.verbose('TBeckyAPI', 'GetSpecifiedHeader '+sHeader);
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		SetLength(sData, 1024);
		self._GetSpecifiedHeader(PAnsiChar(AnsiString(sHeader)), PAnsiChar(sData), 1024);
		sData := PAnsiChar(sData);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.GetStatus(sMailId: RawByteString): DWORD;
begin
	logger.verbose('TBeckyAPI', 'GetStatus '+sMailId);
	Result := 0;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		Result := self._GetStatus(PAnsiChar(AnsiReplaceText(AnsiString(sMailId), '/', '\')));
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.GetTempFileName(sType: RawByteString): RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'GetTempFileName '+sType);
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._GetTempFileName(PAnsiChar(AnsiString(sType)));
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.GetTempFolder: RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'GetTempFolder');
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._GetTempFolder;
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.GetText(var sMimeType: RawByteString): RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'GetText');
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		SetLength(sMimeType, 256);
		pc := self._GetText(PAnsiChar(AnsiString(sMimeType)), 256);
		Result := PAnsiChar(pc);
		sMimeType := PAnsiChar(sMimeType);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.GetVersion: RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'GetVersion');
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._GetVersion;
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.GetWindowHandles(var hMain, hTree, hList, hView: HWND): LongBool;
begin
	logger.verbose('TBeckyAPI', 'GetWindowHandles');
	Result := False;
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		Result := _GetWindowHandles(hMain, hTree, hList, hView);
	finally
		LeaveCriticalSection;
	end;
end;

function TBeckyAPI.HZ_GB2312(sSrc: RawByteString;
  bEncode: LongBool): RawByteString;
var
	pc: PAnsiChar;
begin
	logger.verbose('TBeckyAPI', 'HZ_GB2312 '+IntToStr(Integer(bEncode))+','+Copy(sSrc, 1, 16));
	Result := '';
	if not isInitialized then Exit;
	EnterCriticalSection;
	try
		pc := self._HZ_GB2312(PAnsiChar(AnsiString(sSrc)), bEncode);
		Result := PAnsiChar(pc);
		self._FreeMem(pc);
	finally
		LeaveCriticalSection;
	end;
end;

{ TIdxLineCompare }

function TIdxLineCompare.Compare(const Left, Right: TIdxLine): Integer;
begin
	Result := Left.dwMsgID - Right.dwMsgID;
end;

end.

