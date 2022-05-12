unit Bkroonga2Search;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, TypInfo, Vcl.Menus,
	System.ImageList, Vcl.ImgList, Vcl.Grids, Vcl.OleCtrls, SHDocVw, System.Generics.Collections,
	System.RegularExpressions, System.JSON, System.SyncObjs, System.StrUtils, System.Generics.Defaults,
	IniFiles, mylogger, BeckyAPI, groongarequest, Vcl.Buttons;

type
	TGrnQueryResult = record
		mailid_id: DWORD;
		msgid_id: DWORD;
		mailid: DWORD;
		inreplyto: DWORD;
		haschild: Boolean;
		expanded: Boolean;
		level: Integer;
		date: TDateTime;
		subject: String;
		from: String;
		dir: String;
		snippet: String;
		attr: TArray<String>;
	end;
	TGrnReq = record
		qstage: Integer;
		snippetidx: DWORD;
		cmd: String;
		param: TArray<String>;
	end;
	TGrnReqResult = record
		qstage: Integer;
		snippetidx: DWORD;
		resjson: String;
	end;
	TDrilldownRecord = record
		key: String;
		value: Integer;
	end;
	TLVSortType = (SortThread, SortSubject, SortFrom, SortDate, SortDir);
	TLVSortDirection = (SortForward, SortReverse);
	TSearchFormCompPos = record
		RBtoRight: Integer;
		GB2Left: Double;
		GB3Left: Double;
		GB1_2: Double;
		GB1_3: Double;
		GB2_2: Double;
		GB3_2: Double;
		GB3_3: Double;
	end;
	TGrnRequestThread = class(TThread)
	private
		grnreq: TGroongaRequest;
		fgrnport: Integer;
		que: TQueue<TGrnReq>;
		fresque: TQueue<TGrnReqResult>;
		running: Boolean;
	public
		constructor Create(grnport: Integer; resque: TQueue<TGrnReqResult>);
		destructor Destroy; override;
		procedure Execute; override;
		procedure AddRequest(req: TGrnReq);
	end;
	TBkroonga2SearchForm = class(TForm)
		PanelTop: TPanel;
		PanelTree: TPanel;
		PanelTreeIn: TPanel;
		PanelFrom: TPanel;
		PanelBottom: TPanel;
		LabelQuery: TLabel;
		PanelMonth: TPanel;
		PanelMonthIn: TPanel;
		Splitter1: TSplitter;
		Splitter2: TSplitter;
		LabelHit: TLabel;
		CBQuery: TComboBox;
		RBAnd: TRadioButton;
		RBOr: TRadioButton;
		BtnQuery: TButton;
		BtnQuit: TButton;
		GroupBox1: TGroupBox;
		GroupBox2: TGroupBox;
		GroupBox3: TGroupBox;
		CBAttach: TCheckBox;
		CBIgnoreInbox: TCheckBox;
		CBIgnoreOutbox: TCheckBox;
		CBIgnoreTrash: TCheckBox;
		LVMail: TListView;
		ILMail: TImageList;
		CBMailbox: TCheckBox;
		CBBody: TCheckBox;
		CBQuote: TCheckBox;
		CBHeader: TCheckBox;
		CBFlag: TCheckBox;
		CBNewMail: TCheckBox;
		CBCcme: TCheckBox;
		CBTome: TCheckBox;
		PopupFrom: TPopupMenu;
		WBMail: TWebBrowser;
		TVFolder: TTreeView;
		Timer: TTimer;
		DGMonth: TDrawGrid;
		SBShowTree: TSpeedButton;
		SBOnlyFolder: TSpeedButton;
		SBShowMonth: TSpeedButton;
		SBZeroMonth: TSpeedButton;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormResize(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure WndProc(var Msg: TMessage); override;
		procedure Splitter1Moved(Sender: TObject);
		procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer;
			var Accept: Boolean);
		procedure BtnQueryClick(Sender: TObject);
		procedure BtnQuitClick(Sender: TObject);
		procedure LVMailClick(Sender: TObject);
		procedure LVMailData(Sender: TObject; Item: TListItem);
		procedure LVMailDblClick(Sender: TObject);
		procedure LVMailKeyPress(Sender: TObject; var Key: Char);
		procedure LVMailChange(Sender: TObject; Item: TListItem; Change: TItemChange);
		procedure LVMailColumnClick(Sender: TObject; Column: TListColumn);
		procedure LVMailSelectItem(Sender: TObject; Item: TListItem;
			Selected: Boolean);
		procedure PanelFromClick(Sender: TObject);
		procedure PanelFromMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure PopupFromClick(Sender: TObject);
		procedure TimerTimer(Sender: TObject);
		procedure DGMonthDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
		procedure DGMonthClick(Sender: TObject);
		procedure DGMonthMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure TVFolderMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure TVFolderCustomDrawItem(Sender: TCustomTreeView;
			Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
		procedure CBBodyClick(Sender: TObject);
		procedure CBQueryKeyPress(Sender: TObject; var Key: Char);
		procedure SBShowTreeClick(Sender: TObject);
		procedure SBShowMonthClick(Sender: TObject);
		procedure SBOnlyFolderClick(Sender: TObject);
		procedure SBZeroMonthClick(Sender: TObject);
		procedure DGMonthMouseWheelDown(Sender: TObject; Shift: TShiftState;
			MousePos: TPoint; var Handled: Boolean);
		procedure DGMonthMouseWheelUp(Sender: TObject; Shift: TShiftState;
			MousePos: TPoint; var Handled: Boolean);
    procedure TVFolderChange(Sender: TObject; Node: TTreeNode);
	private
		{ Private 宣言 }
		FirstShow: Boolean;
		fgrnport: Integer;
		grnreq: TGroongaRequest;
		qstage: Integer;	// 0=Hits,1=最初の10件+mon+dirs,2=次の10件,3=次の10件,…
		qhits: Integer;
		MonthList: TArray<TDrilldownRecord>;
		LVThread: TArray<Integer>;
		LVItem: TArray<Integer>;
		QItem: TArray<TGrnQueryResult>;
		FromList: TArray<TDrilldownRecord>;
		TVFolName: TArray<String>;
		MyWndMsg: DWORD;
		TimerPrevQstr: String;
		TimerCnt: Integer;
		PrevDGSel: Integer;
		LVSortType: TLVSortType;
		LVSortDir: TLVSortDirection;
		snippethtml: UTF8String;
		CompPos: TSearchFormCompPos;
		ShowMonth: Boolean;
		ShowTree: Boolean;
		ZeroMonth: Boolean;
		OnlyFolder: Boolean;
		TreeWidth: Integer;
		procedure IniLoad;
		procedure IniSave;
		procedure StartQuery;
		procedure ClearAllQuery;
		procedure AddHistory(s: String);
		function MakeQstr(var sni: TArray<String>): String;
		function MakeQueryParam(stage: Integer): TArray<String>;
		function MakeMatchColumns: String;
		procedure QuerySend(qstage: Integer; param: TArray<String>);
		function GetQueryHitCount(json: TJSONObject): Integer;
		function GetQueryRecordCount(json: TJSONObject): Integer;
		function GetQueryColumnLength(json: TJSONObject): Integer;
		function ParseQueryColumn(json: TJSONObject; colidx: Integer): TJSONObject;
		function ParseQueryRecord(json: TJSONObject; idx, colidx: Integer): TJSONValue;
		function GetDrilldownDtCount(json: TJSONObject): Integer;
		function ParseDrilldownDtRecord(json: TJSONObject; idx: Integer): TDrilldownRecord;
		function GetDrilldownDirCount(json: TJSONObject): Integer;
		function ParseDrilldownDirRecord(json: TJSONObject; idx: Integer): TDrilldownRecord;
		function GetDrilldownFromCount(json: TJSONObject): Integer;
		function ParseDrilldownFromRecord(json: TJSONObject; idx: Integer): TDrilldownRecord;
		procedure ReceiveSnippet(idx: Integer; json: TJSONObject);
		procedure ReceiveFirstQuery(stage: Integer; json: TJSONObject);
		procedure ReceiveQuery(stage: Integer; json: TJSONObject);
		procedure LVMailClear;
		procedure AddQItem(qi: TGrnQueryResult);
		procedure LVItemSort(renew: Boolean);
		function GetLVQItem(idx: Integer): TGrnQueryResult;
		function ParseNumberJSONArray(j: TJSONArray): TArray<DWORD>;
		function ParseStringJSONArray(j: TJSONArray): TArray<String>;
		procedure TVFolderChangeCount(tn: TTreeNode; cnt, cntchild: Integer);
		function GetTVFolderText(tn: TTreeNode): String;
		function GetFromListStr(s: String): String;
		procedure GetNameAndAddr(var name, adrs: String; src: String);
		function mymbstok(str: String; startidx: Integer; tok: String): Integer;
		function mymbschr(str: String; startidx: Integer; chr: String): Integer;
		procedure SetFromCaption(s: String);
		procedure PanelFromColorChange;
		procedure DGMonthUnselect;
		procedure SetLVSort(tp: TLVSortType);
		procedure SetSnippetHTMLText;
		function ChangeShowMonth(sh: Boolean): Boolean;
		function ChangeShowTree(sh: Boolean): Boolean;
		function ChangeZeroMonth(flg: Boolean): Boolean;
		function ChangeOnlyFolder(flg: Boolean): Boolean;
		function GetMonthFirst(dt: TDateTime): TDateTime;
		function GetMonthNext(dt: TDateTime): TDateTime;
	public
		{ Public 宣言 }
		grnreqthread: TGrnRequestThread;
		resque: TQueue<TGrnReqResult>;
		procedure SetGroongaHTTPPort(grnport: Integer);
		procedure GetDefaultComponentPos;
		procedure OnNotifyResque(Sender: TObject;
			const item: TGrnReqResult; Action: TCollectionNotification);
	end;

const
    AppIni = 'SearchForm';

var
	Bkroonga2SearchForm: TBkroonga2SearchForm;

implementation

{$R *.dfm}

{ TGrnRequestThread }

constructor TGrnRequestThread.Create(grnport: Integer; resque: TQueue<TGrnReqResult>);
begin
	inherited Create(False);
	logger.info(self.ClassName, 'Create');
	self.FreeOnTerminate := False;
	running := False;
	fgrnport := grnport;
	fresque := resque;
end;

destructor TGrnRequestThread.Destroy;
begin
	logger.info(self.ClassName, 'Destroy');
	if Assigned(grnreq) then FreeAndNil(grnreq);
	if Assigned(que) then FreeAndNil(que);
	inherited;
end;

procedure TGrnRequestThread.Execute;
var
	req: TGrnReq;
	res: TGrnReqResult;
begin
	logger.info(self.ClassName, 'Execute');
	que := TQueue<TGrnReq>.Create;
	que.Clear;
	grnreq := TGroongaRequest.Create(fgrnport);
	running := True;
	while not Terminated do begin
		if que.Count <= 0 then begin
			Sleep(100);
		end else begin
			req := que.Dequeue;
			grnreq.command(req.cmd, req.param);
			res.qstage := req.qstage;
			res.snippetidx := req.snippetidx;
			res.resjson := grnreq.Result.ToString;
			fresque.Enqueue(res);
			Sleep(100);
		end;
	end;
end;

procedure TGrnRequestThread.AddRequest(req: TGrnReq);
begin
	logger.debug(self.ClassName, 'AddRequest');
	if running then que.Enqueue(req);
end;

{ TBkroonga2SearchForm }

procedure TBkroonga2SearchForm.FormCreate(Sender: TObject);
begin
	logger.info(self.ClassName, 'Create');
	MyWndMsg := RegisterWindowMessage('Bkroonga2SearchFormUniqueMessage');
	TimerCnt := 0;
	TimerPrevQstr := '';
	PrevDGSel := 0;
	LVSortType := SortThread;
	FirstShow := True;
end;

procedure TBkroonga2SearchForm.FormDestroy(Sender: TObject);
begin
	logger.info(self.ClassName, 'Destroy');
	if Assigned(grnreqthread) then begin
		grnreqthread.Terminate;
		grnreqthread.WaitFor;
		FreeAndNil(grnreqthread);
	end;
	if Assigned(resque) then FreeAndNil(resque);
	if Assigned(grnreq) then FreeAndNil(grnreq);
end;

procedure TBkroonga2SearchForm.FormShow(Sender: TObject);
begin
	logger.debug(self.ClassName, 'Show');
	if FirstShow then begin
		FirstShow := False;
		IniLoad;
        ClearAllQuery;
		SetSnippetHTMLText;
		SetLVSort(LVSortType);
	end;
end;

procedure TBkroonga2SearchForm.FormResize(Sender: TObject);
begin
	logger.verbose(self.ClassName, 'Resize');
	if FirstShow = False then begin
		RBAnd.Left := Width - CompPos.RBtoRight;
		RBOr.Left := RBAnd.Left;
		BtnQuery.Left := RBAnd.Left + RBAnd.Width + 4;
		BtnQuit.Left := BtnQuery.Left + BtnQuery.Width + 4;
		CBQuery.Width := RBAnd.Left - CBQuery.Left - 8;
		GroupBox2.Left := Round(Width * CompPos.GB2Left);
		GroupBox1.Width := GroupBox2.Left - GroupBox1.Left - 6;
		GroupBox3.Left := Round(Width * CompPos.GB3Left);
		GroupBox2.Width := GroupBox3.Left - GroupBox2.Left - 6;
		GroupBox3.Width := Width - GroupBox3.Left - 24;
		PanelFrom.Left := GroupBox3.Left;
		PanelFrom.Width := GroupBox3.Width;
		CBBody.Left := Round(GroupBox1.Width * CompPos.GB1_2);
		CBAttach.Left := CBBody.Left;
		CBHeader.Left := Round(GroupBox1.Width * CompPos.GB1_3);
		CBFlag.Left := Round(GroupBox2.Width * CompPos.GB2_2);
		CBNewMail.Left := CBFlag.Left;
		CBIgnoreOutbox.Left := Round(GroupBox3.Width * CompPos.GB3_2);
		CBIgnoreTrash.Left := Round(GroupBox3.Width * CompPos.GB3_3);
	end;
end;

procedure TBkroonga2SearchForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
	logger.debug(self.ClassName, 'CloseQuery');
	IniSave;
	CanClose := True;
end;

procedure TBkroonga2SearchForm.GetDefaultComponentPos;
begin
	CompPos.RBtoRight := Width - RBAnd.Left;
	CompPos.GB2Left := GroupBox2.Left / Width;
	CompPos.GB3Left := GroupBox3.Left / Width;
	CompPos.GB1_2 := CBBody.Left / GroupBox1.Width;
	CompPos.GB1_3 := CBHeader.Left / GroupBox1.Width;
	CompPos.GB2_2 := CBFlag.Left / GroupBox2.Width;
	CompPos.GB3_2 := CBIgnoreOutbox.Left / GroupBox3.Width;
	CompPos.GB3_3 := CBIgnoreTrash.Left / GroupBox3.Width;
end;

procedure TBkroonga2SearchForm.WndProc(var Msg: TMessage);
var
	res: TGrnReqResult;
	json: TJSONObject;
begin
	if Msg.Msg = MyWndMsg then begin
		while resque.Count > 0 do begin
			res := resque.Dequeue;
			json := TJSONObject.ParseJSONValue(res.resjson) as TJSONObject;
			try
				if res.qstage < 0 then begin
					// snippetの受信
					logger.debug(self.ClassName, Format('OnNotifyResque idx=%d', [res.snippetidx]));
					ReceiveSnippet(res.snippetidx, json);
				end else if res.qstage = 0 then begin
					// Hitsとdrilldownの受信
					logger.debug(self.ClassName, Format('OnNotifyResque stage=%d', [res.qstage]));
					ReceiveFirstQuery(res.qstage, json);
				end else if res.qstage > 0 then begin
					// Query結果の受信
					logger.debug(self.ClassName, Format('OnNotifyResque stage=%d', [res.qstage]));
					ReceiveQuery(res.qstage, json);
				end;
			finally
				FreeAndNil(json);
			end;
		end;
	end;
	inherited;
end;

procedure TBkroonga2SearchForm.BtnQueryClick(Sender: TObject);
begin
	logger.debug(self.ClassName, 'BtnQueryClick');
	ClearAllQuery;
	StartQuery;
end;

procedure TBkroonga2SearchForm.BtnQuitClick(Sender: TObject);
begin
	logger.debug(self.ClassName, 'BtnQuit');
	Self.Close;
end;

procedure TBkroonga2SearchForm.SetGroongaHTTPPort(grnport: Integer);
begin
	fgrnport := grnport;
	grnreq := TGroongaRequest.Create(fgrnport);
	resque := TQueue<TGrnReqResult>.Create;
	resque.OnNotify := self.OnNotifyResque;
	grnreqthread := TGrnRequestThread.Create(fgrnport, resque);
end;

procedure TBkroonga2SearchForm.CBBodyClick(Sender: TObject);
begin
	logger.debug(self.ClassName, 'CheckBoxClick '+Sender.UnitName);
	BtnQuery.Click;
end;

procedure TBkroonga2SearchForm.CBQueryKeyPress(Sender: TObject;
  var Key: Char);
begin
	if Key = #13 then begin
		BtnQuery.Click;
		Key := #0;
	end;
end;

procedure TBkroonga2SearchForm.DGMonthDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
    qi: TGrnQueryResult;
	r: TRect;
	h, w, v: Integer;
	s0, s1, s2: String;
	cnv: TCanvas;
	dt: TDateTime;
begin
	try
		cnv := (Sender as TDrawGrid).Canvas;
		if not Assigned(cnv) then Exit;
		if ACol >= Length(MonthList) then Exit;
		dt := bka.time_tToDateTime(StrToFloatDef(MonthList[ACol].key, 0));
		s0 := FormatDateTime('yyyy年', dt);
		s1 := FormatDateTime('m月', dt);
		s2 := IntToStr(MonthList[ACol].value);
		if gdSelected in State then begin
			cnv.Brush.Color := clHighlight;
		end else begin
			cnv.Brush.Color := clWindow;
		end;
		if Assigned(LVMail.Selected) then begin
			qi := GetLVQItem(LVMail.Selected.Index);
			if (qi.date >= GetMonthFirst(dt)) and (qi.date < GetMonthNext(dt)) then
				cnv.Brush.Color := clYellow;
		end;
		cnv.Brush.Style := bsSolid;
		cnv.FillRect(Rect);
		v := StrToIntDef(s2, 0);
		if v > 100 then v := 100;
		if v > 0 then begin
			if gdSelected in State then begin
				cnv.Brush.Color := $00ffd0d0;
			end else begin
				cnv.Brush.Color := $00ffa0a0;
			end;
			r.Left := Rect.Left + 4;
			r.Right := Rect.Right - 4;
			r.Top := Rect.Bottom - ((Rect.Bottom - Rect.Top) * v div 100);
			r.Bottom := Rect.Bottom;
			cnv.FillRect(r);
		end;
		cnv.Brush.Style := bsClear;
		h := cnv.TextHeight('Q');
		r.Left := Rect.Left;
		r.Right := Rect.Right;
		r.Top := Rect.Top;
		r.Bottom := Rect.Top+h;
		w := ((r.Right - r.Left) - cnv.TextWidth(s0)) div 2;
		cnv.TextRect(r, r.Left + w, Rect.Top, s0);
		r.Top := r.Top + h;
		r.Bottom := r.Bottom + h;
		w := ((r.Right - r.Left) - cnv.TextWidth(s1)) div 2;
		cnv.TextRect(r, r.Left + w, Rect.Top+h, s1);
		r.Top := r.Top + h;
		r.Bottom := r.Bottom + h;
		w := ((r.Right - r.Left) - cnv.TextWidth(s2)) div 2;
		v := StrToIntDef(s2, 0);
		if v = 0 then
			cnv.Font.Color := clGray
		else if v < 25 then
			cnv.Font.Color := clWindowText
		else if v < 50 then
			cnv.Font.Color := $00000040
		else if v < 75 then
			cnv.Font.Color := $00000080
		else if v < 100 then
			cnv.Font.Color := $000000c0
		else
			cnv.Font.Color := $000000ff;
		cnv.TextRect(r, r.Left + w, Rect.Top+h*2, s2);
		cnv.Font.Color := clWindowText;
	except on E: Exception do
		logger.error(self.ClassName, Format('DGMonthDrawCell %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.DGMonthMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if (Button = mbRight) and (DGMonth.Selection.Left >= 0) then begin
		DGMonthUnselect;
		StartQuery;
	end;
end;

procedure TBkroonga2SearchForm.DGMonthMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
	PostMessage(DGMonth.Handle, WM_HSCROLL, $1, 0);
	Handled := True;
end;

procedure TBkroonga2SearchForm.DGMonthMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
	PostMessage(DGMonth.Handle, WM_HSCROLL, $0, 0);
	Handled := True;
end;

procedure TBkroonga2SearchForm.TVFolderChange(Sender: TObject;
  Node: TTreeNode);
begin
	logger.debug(self.ClassName, 'TVFolderChange');
	StartQuery;
end;

procedure TBkroonga2SearchForm.TVFolderCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
	qi: TGrnQueryResult;
	cnv: TCanvas;
	r, r0: TRect;
	m: TMatch;
	v, i, imgidx: Integer;
	bmp: TBitmap;
begin
	try
		DefaultDraw := False;
		cnv := Sender.Canvas;
		if Assigned(LVMail.Selected) then begin
			qi := GetLVQItem(LVMail.Selected.Index);
			i := Integer(Node.Data);
			if (i >= 0) and (i < Length(TVFolName)) and (qi.level >= 0) then
				if LowerCase(TVFolName[i]) = LowerCase(qi.dir) then
					cnv.Brush.Color := clYellow;
		end;
		m := TRegEx.Match(Node.Text, ' \- (\d+)');
		if m.Success then
			v := StrToIntDef(m.Groups.Item[1].Value, 0)
		else
			v := 0;
		if v > 100 then v := 100;
		cnv.Brush.Style := bsSolid;
		if v > 0 then begin
			r := Node.DisplayRect(False);
			r.Left := r.Left + ((r.Right - r.Left) * v div 100);
			cnv.FillRect(r);
			if cdsSelected in State then begin
				cnv.Brush.Color := $00ffd0d0;
			end else begin
				cnv.Brush.Color := $00ffa0a0;
			end;
			r := Node.DisplayRect(False);
			r.Right := r.Left + ((r.Right - r.Left) * v div 100);
			r.Top := r.Bottom - 2;
			cnv.FillRect(r);
		end else begin
			r := Node.DisplayRect(False);
			cnv.FillRect(r);
		end;
		cnv.Brush.Style := bsClear;
		r := Node.DisplayRect(True);
		r.Left := r.Left - 16 - (4*Node.Level);
        cnv.Font.Color := clWindowText;
		cnv.TextRect(r, r.Left, r.Top, Node.Text);
		r0 := r;
		r0.Left := r0.Left - ILMail.Width;
		r0.Right := r0.Left + ILMail.Width;
		if cdsSelected in State then
			imgidx := Node.SelectedIndex
		else
			imgidx := Node.ImageIndex;
		ILMail.Draw(cnv, r0.Left, r0.Top, imgidx);
		r0.Left := r0.Left - (ILMail.Width div 2);
		r0.Right := r0.Left + (ILMail.Width div 2);
		if Node.HasChildren then begin
			bmp := TBitmap.Create;
			if Node.Expanded then
				ILMail.GetBitmap(4, bmp)
			else
				ILMail.GetBitmap(3, bmp);
			cnv.StretchDraw(r0, bmp);
			bmp.Free;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('TVFolderCustomDrawItem %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.TVFolderMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if (Button = mbRight) and (TVFolder.Items.Count > 0) and (TVFolder.Selected <> nil) then begin
		TVFolder.Selected := nil;
        StartQuery;
	end;
end;

procedure TBkroonga2SearchForm.LVMailChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
	if Change = ctState then LVMailClick(self);
end;

procedure TBkroonga2SearchForm.LVMailClick(Sender: TObject);
var
	i: Integer;
	li: TListItem;
	param: TArray<String>;
	qres: TGrnQueryResult;
	req: TGrnReq;
	qstr, ocstr: String;
	sni: TArray<String>;
begin
	try
		logger.verbose(self.ClassName, 'LVMailClick');
		li := LVMail.Selected;
		if Assigned(li) then begin
			qres := GetLVQItem(li.Index);
			if qres.level < 0 then Exit;
			if qres.snippet = '' then begin
				qstr := MakeQstr(sni);
				ocstr := ReplaceText(ReplaceText(MakeMatchColumns, 'msgid.', ''), '||', '+');
				for i := 0 to Length(sni)-1 do
					if i < 5 then
						ocstr := ocstr + Format(',"%s","<span class=k%d>","</span>"',[sni[i], i+1]);
				ocstr := 'snippet('+ocstr+',{"width":120,"max_n_results":5,"html_escape":true,"normalizer":"NormalizerAuto"})';
				param := ['table Mails',
					'match_columns '+ReplaceText(MakeMatchColumns, 'msgid.', ''),
					'query '+qstr,
					'filter _id=='+IntToStr(qres.msgid_id),
					'output_columns '+ocstr];
				req.qstage := -1;
				req.snippetidx := li.Index;
				req.cmd := 'select';
				req.param := param;
				grnreqthread.AddRequest(req);
			end else begin
				ReceiveSnippet(li.Index, nil);
			end;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('LVMailClick %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.LVMailColumnClick(Sender: TObject;
  Column: TListColumn);
var
	i, idx, idx0: Integer;
begin
	try
		logger.debug(self.ClassName, Format('LVMailColumnClick %d-%s', [Column.Index, Column.Caption]));
		idx := -1;
		idx0 := -1;
		if Assigned(LVMail.Selected) then begin
			i := LVMail.Selected.Index;
			if LVSortType = SortThread then begin
				if (i >= 0) and (i < Length(LVThread)) then begin
					idx := LVThread[i];
					if QItem[idx].level > 0 then begin
						while (i > 0) and (QItem[LVThread[i]].level > 0) do Dec(i);
						if (i >= 0) and (i < Length(LVThread)) then idx0 := LVThread[i];
					end;
				end;
			end else begin
				if (i >= 0) and (i < Length(LVItem)) then
					idx := LVItem[i];
			end;
		end;
		if Column.Index = 0 then begin
			if LVSortType = SortThread then
				LVSortDir := TLVSortDirection((Integer(LVSortDir) + 1) and 1)
			else
				LVSortDir := SortForward;
			SetLVSort(SortThread)
		end else if Column.Index = 1 then begin
			if LVSortType = SortSubject then
				LVSortDir := TLVSortDirection((Integer(LVSortDir) + 1) and 1)
			else
				LVSortDir := SortForward;
			SetLVSort(SortSubject)
		end else if Column.Index = 2 then begin
			if LVSortType = SortFrom then
				LVSortDir := TLVSortDirection((Integer(LVSortDir) + 1) and 1)
			else
				LVSortDir := SortForward;
			SetLVSort(SortFrom)
		end else if Column.Index = 3 then begin
			if LVSortType = SortDate then
				LVSortDir := TLVSortDirection((Integer(LVSortDir) + 1) and 1)
			else
				LVSortDir := SortForward;
			SetLVSort(SortDate)
		end else begin
			if LVSortType = SortDir then
				LVSortDir := TLVSortDirection((Integer(LVSortDir) + 1) and 1)
			else
				LVSortDir := SortForward;
			SetLVSort(SortDir);
		end;
		if idx >= 0 then begin
			if LVSortType = SortThread then begin
				for i := 0 to Length(LVThread)-1 do
					if LVThread[i] = idx then begin
						LVMail.ItemIndex := i;
						LVMail.Items[i].MakeVisible(False);
						LVMailClick(Self);
						Break;
					end;
				if (LVMail.Selected = nil) and (idx0 >= 0) then
					for i := 0 to Length(LVThread)-1 do
						if LVThread[i] = idx0 then begin
							LVMail.ItemIndex := i;
							LVMail.Items[i].MakeVisible(False);
							LVMailClick(Self);
							Break;
						end;
			end else begin
				for i := 0 to Length(LVItem)-1 do
					if LVItem[i] = idx then begin
						LVMail.ItemIndex := i;
						LVMail.Items[i].MakeVisible(False);
						LVMailClick(Self);
						Break;
					end;
			end;
		end;
		LVMail.Repaint;
	except on E: Exception do
		logger.error(self.ClassName, Format('LVMailColumnClick %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.IniLoad;
var
	ini: TIniFile;
begin
	logger.debug(self.ClassName, 'IniLoad');
	ini := TIniFile.Create(IniFileName);
	with ini do begin
		Left := ReadInteger(AppIni, 'WindowX', Left);
		if (Left < 0) or (Left > 9999) then Left := 0;
		Top := ReadInteger(AppIni, 'WindowY', Top);
		if (Top < 0) or (Top > 9999) then Top := 0;
		Width := ReadInteger(AppIni, 'Width', Width);
		if Width < self.Constraints.MinWidth then Width := self.Constraints.MinWidth;
		Height := ReadInteger(AppIni, 'Height', Height);
		if Height < self.Constraints.MinHeight then Height := self.Constraints.MinHeight;
		WBMail.Height := ReadInteger(AppIni, 'WBHeight', WBMail.Height);
		TreeWidth := ReadInteger(AppIni, 'TVWidth', PanelTree.Width);
		LVMail.Column[0].Width := ReadInteger(AppIni, 'LV0Width', LVMail.Column[0].Width);
		LVMail.Column[1].Width := ReadInteger(AppIni, 'LV1Width', LVMail.Column[1].Width);
		LVMail.Column[2].Width := ReadInteger(AppIni, 'LV2Width', LVMail.Column[2].Width);
		LVMail.Column[3].Width := ReadInteger(AppIni, 'LV3Width', LVMail.Column[3].Width);
		LVMail.Column[4].Width := ReadInteger(AppIni, 'LV4Width', LVMail.Column[4].Width);
		CBMailbox.Checked := ReadBool(AppIni, 'bMailbox', True);
		CBBody.Checked := ReadBool(AppIni, 'bBody', True);
		CBHeader.Checked := ReadBool(AppIni, 'bHeader', True);
		CBQuote.Checked := ReadBool(AppIni, 'bQuote', True);
		CBAttach.Checked := ReadBool(AppIni, 'bAttach', True);
		CBToMe.Checked := ReadBool(AppIni, 'bToMe', False);
		CBCcMe.Checked := ReadBool(AppIni, 'bToCc', False);
		CBFlag.Checked := ReadBool(AppIni, 'bFlag', False);
		CBNewmail.Checked := ReadBool(AppIni, 'bNewmail', False);
		CBIgnoreInbox.Checked := ReadBool(AppIni, 'bIgnoreInbox', False);
		CBIgnoreOutbox.Checked := ReadBool(AppIni, 'bIgnoreOutbox', False);
		CBIgnoreTrash.Checked := ReadBool(AppIni, 'bIgnoreTrash', True);
		ShowMonth := ReadBool(AppIni, 'ShowMonth', True);
		ShowTree := ReadBool(AppIni, 'ShowTree', True);
		ZeroMonth := ReadBool(AppIni, 'ZeroMonth', False);
		OnlyFolder := ReadBool(AppIni, 'OnlyFolder', False);
		LVSortType := TLVSortType(ReadInteger(AppIni, 'SortType', 0));
		LVSortDir := TLVSortDirection(ReadInteger(AppIni, 'SortDir', 0));
		Free;
	end;
	WBMail.Align := alBottom;
	if FileExists(ExtractFilePath(IniFileName)+'bkroonga2.hst') then
		CBQuery.Items.LoadFromFile(ExtractFilePath(IniFileName)+'bkroonga2.hst');
	ChangeShowMonth(ShowMonth);
	ChangeShowTree(ShowTree);
	ChangeZeroMonth(ZeroMonth);
	ChangeOnlyFolder(OnlyFolder);
end;

procedure TBkroonga2SearchForm.IniSave;
var
	ini: TIniFile;
begin
	logger.debug(self.ClassName, 'IniSave');
	ini := TIniFile.Create(IniFileName);
	with ini do begin
		WriteInteger(AppIni, 'WindowX', Left);
		WriteInteger(AppIni, 'WindowY', Top);
		WriteInteger(AppIni, 'Width', Width);
		WriteInteger(AppIni, 'Height', Height);
		WriteInteger(AppIni, 'WBHeight', WBMail.Height);
		WriteInteger(AppIni, 'TVWidth', TreeWidth);
		WriteInteger(AppIni, 'LV0Width', LVMail.Column[0].Width);
		WriteInteger(AppIni, 'LV1Width', LVMail.Column[1].Width);
		WriteInteger(AppIni, 'LV2Width', LVMail.Column[2].Width);
		WriteInteger(AppIni, 'LV3Width', LVMail.Column[3].Width);
		WriteInteger(AppIni, 'LV4Width', LVMail.Column[4].Width);
		WriteBool(AppIni, 'bMailbox', CBMailbox.Checked);
		WriteBool(AppIni, 'bBody', CBBody.Checked);
		WriteBool(AppIni, 'bHeader', CBHeader.Checked);
		WriteBool(AppIni, 'bQuote', CBQuote.Checked);
		WriteBool(AppIni, 'bAttach', CBAttach.Checked);
		WriteBool(AppIni, 'bToMe', CBToMe.Checked);
		WriteBool(AppIni, 'bToCc', CBCcMe.Checked);
		WriteBool(AppIni, 'bFlag', CBFlag.Checked);
		WriteBool(AppIni, 'bNewmail', CBNewmail.Checked);
		WriteBool(AppIni, 'bIgnoreInbox', CBIgnoreInbox.Checked);
		WriteBool(AppIni, 'bIgnoreOutbox', CBIgnoreOutbox.Checked);
		WriteBool(AppIni, 'bIgnoreTrash', CBIgnoreTrash.Checked);
		WriteBool(AppIni, 'ShowMonth', ShowMonth);
		WriteBool(AppIni, 'ShowTree', ShowTree);
		WriteBool(AppIni, 'ZeroMonth', ZeroMonth);
		WriteBool(AppIni, 'OnlyFolder', OnlyFolder);
		WriteInteger(AppIni, 'SortType', Integer(LVSortType));
		WriteInteger(AppIni, 'SortDir', Integer(LVSortDir));
		Free;
	end;
	CBQuery.Items.SaveToFile(ExtractFilePath(IniFileName)+'bkroonga2.hst');
end;

function TBkroonga2SearchForm.MakeQstr(var sni: TArray<String>): String;
var
	tmp, joinstr: String;
begin
	try
		// sniはsnippetのquery作成用。通常検索時は使わない
		logger.verbose(self.ClassName, Format('MakeQstr %s', [Trim(CBQuery.Text)]));
        SetLength(sni, 0);
		tmp := Trim(CBQuery.Text);
		if tmp = '' then Exit;
		joinstr := ' ';
		if RBOr.Checked then joinstr := ' OR ';
		Result := '';
		while tmp <> '' do begin
			if Result = '' then begin
				joinstr := '';
			end else begin
				if RBOr.Checked then joinstr := ' OR ' else joinstr := ' ';
			end;
			if Pos('"', tmp) = 1 then begin
				tmp := Copy(tmp, 2, Length(tmp));
				if Pos('"', tmp) > 0 then begin
					Result := Result + joinstr + Copy(tmp, 1, Pos('"',tmp)-1);
					sni := sni + [Copy(tmp, 1, Pos('"',tmp)-1)];
					tmp := Trim(Copy(tmp, Pos('"',tmp)+1, Length(tmp)));
				end else begin
					Result := Result + joinstr + Trim(tmp);
					sni := sni + [Trim(tmp)];
					tmp := '';
				end;
			end else begin
				if Pos(' ', tmp) > 0 then begin
					Result := Result + joinstr + Copy(tmp, 1, Pos(' ', tmp)-1);
					sni := sni + [Copy(tmp, 1, Pos(' ', tmp)-1)];
					tmp := Trim(Copy(tmp, Pos(' ',tmp)+1, Length(tmp)));
				end else if Pos('　', tmp) > 0 then begin
					Result := Result + joinstr + Copy(tmp, 1, Pos('　', tmp)-1);
					sni := sni + [Copy(tmp, 1, Pos('　', tmp)-1)];
					tmp := Trim(Copy(tmp, Pos('　',tmp)+1, Length(tmp)));
				end else begin
					Result := Result + joinstr + Trim(tmp);
					sni := sni + [Trim(tmp)];
					tmp := '';
				end;
			end;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('MakeQstr %s %s', [E.Message, e.StackTrace]));
	end;
end;

function TBkroonga2SearchForm.MakeMatchColumns: String;
begin
	Result := '';
	if CBBody.Checked then Result := Result + 'msgid.body||';
	if CBQuote.Checked then Result := Result + 'msgid.quote||';
	if CBAttach.Checked then Result := Result + 'msgid.attach||';
	if CBHeader.Checked then Result := Result + 'msgid.subject||msgid.from||msgid.to||msgid.cc||msgid.bcc||';
	Result := Copy(Result, 1, Length(Result)-2);
	if Result = '' then begin
		Result := 'msgid.body';
        CBBody.Checked := True;
	end;
end;

function TBkroonga2SearchForm.MakeQueryParam(stage: Integer): TArray<String>;
var
	qs, fil, tmp, name, adrs: String;
	sni: TArray<String>;
	gsel: TGridRect;
	tn: TTreeNode;
	i: Integer;
begin
	try
		logger.debug(self.ClassName, Format('MakeQueryParam %d', [stage]));
		Result := ['table MailIds'];
		Result := Result + ['match_columns '+MakeMatchColumns];
		qs := MakeQstr(sni);
		if qs = '' then begin
			logger.debug(self.ClassName, 'MakeQueryParam Exit because query empty');
			SetLength(Result, 0);
			Exit;
		end;
		Result := Result + [
			'query '+qs,
			'sortby -msgid.date',
			'output_columns _id,msgid._id,msgid.subject,msgid.from,msgid.date,dir,idx,attr,msgid.inreplyto._id'
		];
		fil := '';
		if CBNewMail.Checked then
			fil := fil + ' && msgid.inreplyto==""';
		if CBTome.Checked then fil := fil + ' && attr@"tome"';
		if CBCcme.Checked then fil := fil + ' && attr@"ccme"';
		if CBFlag.Checked then fil := fil + ' && attr@"flag"';
		if CBIgnoreInbox.Checked then fil := fil + ' &! attr@"inbox"';
		if CBIgnoreOutbox.Checked then fil := fil + ' &! attr@"outbox"';
		if CBIgnoreTrash.Checked then fil := fil + ' &! attr@"trash"';
		gsel := DGMonth.Selection;
		if (gsel.Left >= 0) and (gsel.Right >= 0) and (gsel.Left < Length(MonthList)) and (gsel.Right < Length(MonthList)) then begin
			fil := fil +
				' && msgid.date>='+FloatToStr(bka.DateTimeTotime_t(GetMonthFirst(bka.time_tToDateTime(StrToFloatDef(MonthList[gsel.Right].key, 0)))))+
				' && msgid.date<'+FloatToStr(bka.DateTimeTotime_t(GetMonthNext(bka.time_tToDateTime(StrToFloatDef(MonthList[gsel.Left].key, 0)))));
		end;
		if CBMailbox.Checked then
			fil := fil + ' && dir@^"'+ExcludeTrailingPathDelimiter(bka.GetCurrentMailBox)+'"';
		tn := TVFolder.Selected;
		if Assigned(tn) then begin
			i := Integer(tn.Data);
			if (i >= 0) and (i < Length(TVFolName)) then
				if not OnlyFolder then begin
					fil := fil + ' && dir@^"'+TVFolName[i]+'"';
				end else begin
					fil := fil + ' && dir=="'+TVFolName[i]+'"';
				end;
		end;
		if (PanelFrom.Tag >= 0) and (PanelFrom.Tag < Length(FromList)) then begin
			tmp := FromList[PanelFrom.Tag].key;
			GetNameAndAddr(name, adrs, tmp);
			fil := fil + ' && msgid.from@"'+adrs+'"';
		end;
		if stage <= 0 then begin
			// Hits＋月々のドリルダウン＋dirsのドリルダウンを取得
			Result := Result + [
				'limit 0',
				'offset 0'
			];
			if not((gsel.Left >= 0) and (gsel.Right >= 0) and (gsel.Left < Length(MonthList)) and (gsel.Right < Length(MonthList))) then
				Result := Result + [
					'columns[dt].stage filtered',
					'columns[dt].type Time',
					'columns[dt].flags COLUMN_SCALAR',
					'columns[dt].value time_classify_month(msgid.date)',
					'drilldowns[ddt].keys dt',
					'drilldowns[ddt].sort_keys -_key',
					'drilldowns[ddt].output_columns _key,_nsubrecs',
					'drilldowns[ddt].limit 9999'];
			if (tn = nil) then
				Result := Result + [
					'drilldowns[ddir].keys dir',
					'drilldowns[ddir].sort_keys _key',
					'drilldowns[ddir].output_columns _key,_nsubrecs',
					'drilldowns[ddir].limit 9999'];
			if not((PanelFrom.Tag >= 0) and (PanelFrom.Tag < Length(FromList))) then
				Result := Result + [
					'drilldowns[dfrom].keys msgid.from',
					'drilldowns[dfrom].sort_keys -_nsubrecs',
					'drilldowns[dfrom].output_columns _key,_nsubrecs',
					'drilldowns[dfrom].limit 100'];
		end else if stage >= 1 then begin
			// 10件ずつ取得
			Result := Result + [
				'limit 10',
				'offset '+IntToStr((stage - 1) * 10)
			];
		end;
		fil := Trim(fil);
		if Copy(fil, 1, 2) = '&&' then fil := Trim(Copy(fil, 3, Length(fil)));
		if Copy(fil, 1, 2) = '&!' then fil := 'all_records()'+fil;
		if fil <> '' then Result := Result + ['filter '+fil];
	except on E: Exception do
		logger.error(self.ClassName, Format('MakeQueryParam %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.OnNotifyResque(Sender: TObject;
	const item: TGrnReqResult; Action: TCollectionNotification);
begin
	// TWebBrowserを別スレッドから操作しようとすると固まってしまうので、
	// Formのメインスレッドで動作させるようにPostMessageで通知するようにした
	PostMessage(self.Handle, MyWndMsg, 0, 0);
end;

procedure TBkroonga2SearchForm.StartQuery;
begin
	logger.debug(self.ClassName, 'StartQuery');
	if Trim(CBQuery.Text) = '' then begin
        ClearAllQuery;
		Exit;
	end;
	qstage := 0;
	qhits := 0;
	LabelHit.Caption := 'HIT: ...';
	TimerPrevQstr := Trim(CBQuery.Text);
	AddHistory(TimerPrevQstr);
	TimerCnt := 0;
	QuerySend(qstage, MakeQueryParam(qstage));
	IniSave;
end;

procedure TBkroonga2SearchForm.ClearAllQuery;
begin
	LVMailClear;
	TVFolder.Items.Clear;
	SetLength(MonthList, 0);
	SetLength(FromList, 0);
	PanelFrom.Tag := -1;
	DGMonth.ColCount := 0;
	DGMonthUnselect;
	WBMail.Navigate('about:blank');
	LabelHit.Caption := 'HIT:';
end;

procedure TBkroonga2SearchForm.AddHistory(s: String);
begin
	try
		if s = '' then Exit;    // 空文字は履歴に追記しない
		if CBQuery.Items.Count > 0 then begin
			if CBQuery.Items[0] = s then Exit;
		end;
		CBQuery.Items.Insert(0, s);
		while CBQuery.Items.Count > 1000 do
			CBQuery.Items.Delete(CBQuery.Items.Count-1);
	except on E: Exception do
		logger.error(self.ClassName, Format('AddHistory %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.QuerySend(qstage: Integer; param: TArray<String>);
var
	req: TGrnReq;
begin
	try
		if Length(param) = 0 then begin
            ClearAllQuery;
			Exit;
		end;
		logger.debug(self.ClassName, Format('QuerySend stage=%d param=%s', [qstage, String.join(',', param)]));
		req.qstage := qstage;
		req.snippetidx := 0;
		req.cmd := 'select';
		req.param := param;
		grnreqthread.AddRequest(req);
	except on E: Exception do
		logger.error(self.ClassName, Format('QuerySend %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.ReceiveSnippet(idx: Integer;
  json: TJSONObject);
var
	i: Integer;
	snistr: String;
	jary: TJSONArray;
	s: UTF8String;
	doc: Variant;
	qi: TGrnQueryResult;
begin
	try
		snistr := '';
		qi := GetLVQItem(idx);
		if qi.level < 0 then Exit;
		if (json = nil) then begin
			logger.debug(self.ClassName, Format('ReceiveSnippet %d(cached) %X', [idx, GetCurrentThreadId]));
			snistr := qi.snippet;
		end else begin
			if (idx >= 0) and (idx < Length(QItem)) then begin
				//logger.info(self.ClassName, Format('ReceiveSnippet %d %X %s', [idx, GetCurrentThreadId, json.ToString]));
				logger.info(self.ClassName, Format('ReceiveSnippet %d %X', [idx, GetCurrentThreadId]));
				jary := json.GetValue<TJSONObject>('body').
					GetValue<TJSONArray>('records').Items[0].AsType<TJSONArray>.
					Items[0].AsType<TJSONArray>;
				for i := 0 to jary.Count-1 do
					if i = 0 then
						snistr := jary.Items[i].Value
					else
						snistr := snistr + '<span class=dot>‥‥</span>' + jary.Items[i].Value;
				QItem[idx].snippet := snistr;
			end else begin
				logger.info(self.ClassName, Format('ReceiveSnippet %d %X but drop', [idx, GetCurrentThreadId]));
                Exit;
			end;
		end;
		s := snippethtml + UTF8String(snistr) + UTF8String('</body></html>');
		doc := WBMail.Document;
		doc.Clear;
		doc.Write(s);
		doc.Close;
	except on E: Exception do
		logger.error(self.ClassName, Format('ReceiveSnippet %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.PanelFromClick(Sender: TObject);
var
	i: Integer;
	mi: TMenuItem;
begin
	try
		logger.debug(self.ClassName, 'PanelFromClick');
		PopupFrom.Items.Clear;
		if Length(FromList) > 0 then begin
			for i := 0 to Length(FromList)-1 do begin
				mi := TMenuItem.Create(PopupFrom);
				mi.Caption := GetFromListStr(FromList[i].key+#9+IntToStr(FromList[i].value));
				mi.Tag := i;
				mi.OnClick := PopupFromClick;
				PopupFrom.Items.Add(mi);
			end;
			PopupFrom.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('PanelFromClick %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.PanelFromMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	try
		if (Button = mbRight) and (PanelFrom.Tag >= 0) then begin
			PanelFrom.Tag := -1;
			SetFromCaption('');
			PanelFromColorChange;
			StartQuery;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('PanelFromMouseDown %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.PopupFromClick(Sender: TObject);
var
	s: String;
	name, adrs: String;
begin
	try
		logger.debug(self.ClassName, 'PopupFromClick');
		PanelFrom.Tag := (Sender as TMenuItem).Tag;
		s := (Sender as TMenuItem).Caption;
		GetNameAndAddr(name, adrs, Copy(s, 1, AnsiPos(#9, s)-1));
		SetFromCaption(Trim(name+' <'+adrs+'>'));
		PanelFromColorChange;
		StartQuery;
	except on E: Exception do
		logger.error(self.ClassName, Format('PopupFromClick %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.TimerTimer(Sender: TObject);
begin
	try
		if TimerPrevQstr <> Trim(CBQuery.Text) then begin
			TimerPrevQstr := Trim(CBQuery.Text);
			TimerCnt := 10;
		end;
		if TimerCnt > 0 then begin
			Dec(TimerCnt);
			if TimerCnt = 0 then
				BtnQuery.Click;
		end else begin
			TimerCnt := 0;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('TimerTimer %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.DGMonthClick(Sender: TObject);
var
	dgsel: Integer;
begin
	try
		logger.debug(self.ClassName, Format('DGMonthClick %d-%d', [DGMonth.Selection.Left, DGMonth.Selection.Right]));
		dgsel := DGMonth.Selection.Left * 10000 + DGMonth.Selection.Right;
		if PrevDGSel <> dgsel then begin
			PrevDGSel := dgsel;
			StartQuery;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('DGMonthClick %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.TVFolderChangeCount(tn: TTreeNode; cnt,
  cntchild: Integer);
var
	m: TMatch;
begin
	try
		m := TRegEx.Match(tn.Text, '^(.+ \- )(\d+)\s*\((\d+)\)');
		if m.Success then
			tn.Text := Format('%s%d (%d)',
				[m.Groups[1].Value,
					StrToInt(m.Groups[2].Value) + cnt,
					StrToInt(m.Groups[3].Value) + cntchild])
		else
			tn.Text := tn.Text + '!';
	except on E: Exception do
		logger.error(self.ClassName, Format('TVFolderChangeCount %s %s', [E.Message, e.StackTrace]));
	end;
end;

function TBkroonga2SearchForm.GetTVFolderText(tn: TTreeNode): String;
var
	m: TMatch;
begin
	try
		m := TRegEx.Match(tn.Text, '^(.+) \- ');
		if m.Success then
			Result := m.Groups[1].Value
		else
			Result := '';
	except on E: Exception do
		logger.error(self.ClassName, Format('GetTVFolderText %s %s', [E.Message, e.StackTrace]));
	end;
end;

function TBkroonga2SearchForm.GetFromListStr(s: String): String;
var
	name, adrs: String;
begin
	try
		GetNameAndAddr(name, adrs, Copy(s, 1, Pos(#9,s)-1));
		Result := Trim(name+' <'+adrs+'>'#9+Copy(s, Pos(#9, s)+1,MaxInt));
	except on E: Exception do
		logger.error(self.ClassName, Format('GetFromListStr %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.GetNameAndAddr(var name, adrs: String;
  src: String);
var
	lpsz: String;
	lpszidx, lpszAddridx, lpszTmpidx: Integer;
	bq: Boolean;
	m: TMatch;
begin
	try
		// Parse an e-mail address to name and address.
		// e.g. Name <mail@address> --> "Name" and "mail@address"
		// BkCommon.cppからの移植
		logger.debug(self.ClassName, Format('GetNameAndAddr %s', [src]));
		name := '';
		adrs := '';
		lpszidx := 1;
		lpsz := Trim(src);
		if Length(lpsz) = 0 then Exit;
		bq := False;
		lpszAddridx := lpszidx;
		while True do begin
			if Copy(lpsz, lpszAddridx, 1) = '"' then begin
				bq := not bq;
			end else if (Copy(lpsz, lpszAddridx, 1) = '<') and (not bq) then begin
				break;
			end;
			lpszAddridx := lpszAddridx + 1;
			if lpszAddridx >= Length(lpsz) then Break;
		end;
		if lpszAddridx < Length(lpsz) then begin
			lpszAddridx := lpszAddridx + 1;
			lpszTmpidx := mymbstok(lpsz, lpszAddridx, '>');
			if lpszTmpidx > 0 then begin
				adrs := Trim(Copy(lpsz, lpszAddridx, lpszTmpidx - lpszAddridx - 1));
			end else begin
				adrs := '';
			end;
			if Copy(lpsz, lpszidx, 1) <> '<' then begin
				if Copy(lpsz, lpszidx, 1) = '"' then begin
					lpszidx := lpszidx + 1;
					lpszTmpidx := mymbstok(lpsz, lpszidx, '"');
				end else begin
					lpszTmpidx := mymbstok(lpsz, lpszidx, '<');
				end;
				if lpszTmpidx > 0 then begin
					name := Trim(Copy(lpsz, lpszidx, lpszTmpidx - lpszidx - 1));
				end else begin
					name := '';
				end;
			end;
		end else begin
			lpszAddridx := mymbschr(lpsz, lpszidx, '(');
			if lpszAddridx > 0 then begin
				lpszAddridx := lpszAddridx + 1;
				lpszTmpidx := mymbstok(lpsz, lpszAddridx, ')');
				if lpszTmpidx > 0 then begin
					name := Trim(Copy(lpsz, lpszAddridx, lpszTmpidx - lpszAddridx - 1));
				end else begin
					name := '';
				end;
				lpszTmpidx := mymbstok(lpsz, lpszidx, '(');
				if lpszTmpidx > 0 then begin
					adrs := Trim(Copy(lpsz, lpszidx, lpszTmpidx - lpszidx - 1));
				end else begin
					adrs := '';
				end;
			end else begin
				name := '';
				adrs := Trim(lpsz);
			end;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('GetNameAndAddr %s %s', [E.Message, e.StackTrace]));
	end;
end;

function TBkroonga2SearchForm.mymbschr(str: String; startidx: Integer;
  chr: String): Integer;
begin
	Result := Pos(chr, Copy(str, startidx, MaxInt));
	if Result > 0 then Result := Result + startidx - 1;
end;

function TBkroonga2SearchForm.mymbstok(str: String; startidx: Integer;
  tok: String): Integer;
begin
	Result := Pos(tok, Copy(str, startidx, MaxInt));
	if Result > 0 then Result := Result + startidx;
end;

procedure TBkroonga2SearchForm.SBShowMonthClick(Sender: TObject);
begin
	logger.debug(self.ClassName, 'SBShowMonthClick');
	ShowMonth := ChangeShowMonth(not ShowMonth);
end;

procedure TBkroonga2SearchForm.SBShowTreeClick(Sender: TObject);
begin
	logger.debug(self.ClassName, 'SBShowTreeClick');
	ShowTree := ChangeShowTree(not ShowTree);
end;

procedure TBkroonga2SearchForm.SBZeroMonthClick(Sender: TObject);
begin
	logger.debug(self.ClassName, 'SBZeroMonthClick');
	ZeroMonth := ChangeZeroMonth(not ZeroMonth);
	if DGMonth.ColCount > 0 then StartQuery;
end;

procedure TBkroonga2SearchForm.SBOnlyFolderClick(Sender: TObject);
begin
	logger.debug(self.ClassName, 'SBOnlyFolderClick');
	OnlyFolder := ChangeOnlyFolder(not OnlyFolder);
	if TVFolder.SelectionCount > 0 then StartQuery;
end;

procedure TBkroonga2SearchForm.SetFromCaption(s: String);
var
	maxw: Integer;
	name, adrs, tmp: String;
begin
	try
		logger.debug(self.ClassName, 'SetFromCaption '+s);
		if s <> '' then begin
			maxw := (PanelFrom.Width - 8);
			if Canvas.TextWidth(s) >= maxw then begin
				GetNameAndAddr(name, adrs, s);
				while Length(name) > 0 do begin
					tmp := name+'... <'+adrs+'>';
					if Canvas.TextWidth(tmp) <= maxw then begin
						PanelFrom.Caption := tmp;
						Exit;
					end;
					name := Copy(name, 1, Length(name)-1);
				end;
				PanelFrom.Caption := '<'+adrs+'>';
			end else begin
				PanelFrom.Caption := s;
			end;
		end else begin
			PanelFrom.Caption := '差出人絞り込みなし';
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('SetFromCaption %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.PanelFromColorChange;
begin
	try
		if PanelFrom.Tag >= 0 then begin
			if PanelFrom.Color <> clHighlight then
				PanelFrom.Color := clHighlight;
		end else begin
			if PanelFrom.Color <> clBtnFace then
				PanelFrom.Color := clBtnFace;
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('PanelFromColorChange %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.DGMonthUnselect;
var
	r: TGridRect;
begin
	try
		r.Left := -1;
		r.Right := -1;
		r.Top := 0;
		r.Bottom := 0;
		DGMonth.Selection := r;
		PrevDGSel := DGMonth.Selection.Left * 10000 + DGMonth.Selection.Right;
		DGMonth.Refresh;
	except on E: Exception do
		logger.error(self.ClassName, Format('DGMonthUnselect %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.ReceiveFirstQuery(stage: Integer;
  json: TJSONObject);
var
	i, cnt, val, lev: Integer;
	dt: TDateTime;
	r: TGridRect;
	s, ss, fol: String;
	m: TMatch;
	tn, ctn: TTreeNode;
	folary: TArray<String>;
	ddres, tmpddres: TDrilldownRecord;
	prevddt: TDateTime;
begin
	try
		qhits := GetQueryHitCount(json);
		logger.info(self.ClassName, Format('ReceiveFirstQuery hits=%d', [qhits]));
		logger.debug(self.ClassName, json.ToString);
		LVMailClear;
		LabelHit.Caption := Format('HIT: %d (%d)', [qhits, Length(QItem)]);
		cnt := GetDrilldownDtCount(json);
		logger.debug(self.ClassName, Format('ReceiveFirstQuery Drilldown Dt hits=%d', [cnt]));
		if cnt >= 0 then begin
			SetLength(MonthList, 0);
			prevddt := 0.0;
			for i := 0 to cnt-1 do begin
				ddres := ParseDrilldownDtRecord(json, i);
				dt := bka.time_tToDateTime(StrToFloatDef(ddres.key, 0.0));
				if ZeroMonth and (prevddt <> 0.0) then begin
					while (prevddt - dt) >= 32 do begin
						prevddt := prevddt - 1.0;
						prevddt := EncodeDate(StrToInt(FormatDateTime('yyyy', prevddt)), StrToInt(FormatDateTime('mm', prevddt)), 1);
						tmpddres.key := FloatToStr(bka.DateTimeTotime_t(prevddt));
						tmpddres.value := 0;
                        MonthList := MonthList + [tmpddres];
					end;
				end;
				MonthList := MonthList + [ddres];
				prevddt := dt;
			end;
			DGMonth.ColCount := Length(MonthList);
			DGMonthUnselect;
		end;
		cnt := GetDrilldownDirCount(json);
		logger.debug(self.ClassName, Format('ReceiveFirstQuery Drilldown Dir hits=%d', [cnt]));
		if cnt >= 0 then begin
			TVFolder.Items.Clear;
			TVFolder.Items.BeginUpdate;
			try
				SetLength(TVFolName, 0);
				for i := 0 to cnt-1 do begin
					fol := ParseDrilldownDirRecord(json, i).key;
					folary := TRegEx.Split(fol, '\/');
					s := bka.GetFolderDisplayName(bka.Slash2Yen(fol+'/'));
					val := ParseDrilldownDirRecord(json, i).value;
					logger.debug(self.ClassName, Format('ReceiveFirstQuery Drilldown Dir [%s %d]', [String.Join('/', folary), val]));
					m := TRegEx.Match(s, '\[(.+?)\]\s+(.+)$');
					if m.Success then begin
						// TOPを探す
						ss := m.Groups.Item[1].Value;
						tn := TVFolder.TopItem;
						lev := 1;
						while True do begin
							if tn = nil then begin
								tn := TVFolder.Items.AddChild(nil, ss + ' - 0 (0)');
								TVFolName := TVFolName + [String.Join('/', folary, 0, lev)];
								tn.Data := Pointer(Length(TVFolName)-1);
								TVFolderChangeCount(tn, 0, val);
								tn.ImageIndex := 10;
								tn.SelectedIndex := 10;
								Break;
							end else if Pos(ss + ' ', tn.Text) = 1 then begin
								TVFolderChangeCount(tn, 0, val);
								Break;
							end else begin
								tn := tn.getNextSibling;
							end;
						end;
						s := m.Groups.Item[2].Value;
						while s <> '' do begin
							if Pos('/', s) = 0 then begin
								ss := s;
								s := '';
							end else begin
								ss := Copy(s, 1, Pos('/', s)-1);
								s := Trim(Copy(s, Pos('/', s)+1, Length(s)));
							end;
							ctn := tn.getFirstChild;
							Inc(lev);
							while True do begin
								if ctn = nil then begin
									tn := TVFolder.Items.AddChild(tn, ss + ' - 0 (0)');
									TVFolName := TVFolName + [String.Join('/', folary, 0, lev)];
									tn.Data := Pointer(Length(TVFolName)-1);
									if s <> '' then
										TVFolderChangeCount(tn, 0, val)
									else
										TVFolderChangeCount(tn, val, 0);
									if Pos('!inbox', LowerCase(fol)) > 0 then begin
										tn.ImageIndex := 19;
										tn.SelectedIndex := 18;
									end else if Pos('!sent', LowerCase(fol)) > 0 then begin
										tn.ImageIndex := 25;
										tn.SelectedIndex := 24;
									end else if Pos('!draft', LowerCase(fol)) > 0 then begin
										tn.ImageIndex := 23;
										tn.SelectedIndex := 22;
									end else if Pos('!trash', LowerCase(fol)) > 0 then begin
										tn.ImageIndex := 29;
										tn.SelectedIndex := 28;
									end else begin
										tn.ImageIndex := 31;
										tn.SelectedIndex := 30;
									end;
									Break;
								end else if Pos(ss + ' ', ctn.Text) = 1 then begin
									tn := ctn;
									if s <> '' then
										TVFolderChangeCount(tn, 0, val)
									else
										TVFolderChangeCount(tn, val, 0);
									Break;
								end else begin
									ctn := ctn.getNextSibling;
								end;
							end;
						end;
					end;
				end;
				TVFolder.FullExpand;
			finally
				TVFolder.Items.EndUpdate;
			end;
		end;
		cnt := GetDrilldownFromCount(json);
		logger.debug(self.ClassName, Format('ReceiveFirstQuery Drilldown From hits=%d', [cnt]));
		if cnt >= 0 then begin
			SetLength(FromList, 0);
			for i := 0 to cnt-1 do begin
				FromList := FromList + [ParseDrilldownFromRecord(json, i)];
				//logger.info(self.ClassName, FromList[i].key);
			end;
		end;
		if qhits > 0 then
			QuerySend(stage + 1, MakeQueryParam(stage + 1));
	except on E: Exception do
		logger.error(self.ClassName, Format('ReceiveFirstQuery %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.ReceiveQuery(stage: Integer;
  json: TJSONObject);
var
	i, j: Integer;
	qres: TGrnQueryResult;
	colname: String;
begin
	try
		logger.debug(self.ClassName, Format('ReceiveQuery %d', [stage]));
		for i := 0 to GetQueryRecordCount(json)-1 do begin
			for j := 0 to GetQueryColumnLength(json)-1 do begin
				colname := ParseQueryColumn(json, j).GetValue<String>('name');
				qres.haschild := False;
				qres.expanded := False;
				qres.level := 0;
				if colname = '_id' then begin
					qres.mailid_id := ParseQueryRecord(json, i, j).AsType<DWORD>;
				end else if colname = 'msgid._id' then begin
					qres.msgid_id := ParseQueryRecord(json, i, j).AsType<DWORD>;
				end else if colname = 'msgid.subject' then begin
					qres.subject := ParseQueryRecord(json, i, j).AsType<String>;
				end else if colname = 'msgid.from' then begin
					qres.from := ParseQueryRecord(json, i, j).AsType<String>;
				end else if colname = 'msgid.date' then begin
					qres.date := bka.time_tToDateTime(ParseQueryRecord(json, i, j).AsType<Double>);
				end else if colname = 'dir' then begin
					qres.dir := ParseQueryRecord(json, i, j).AsType<String>;
				end else if colname = 'idx' then begin
					qres.mailid := ParseQueryRecord(json, i, j).AsType<DWORD>;
				end else if colname = 'attr' then begin
					qres.attr := ParseStringJSONArray(ParseQueryRecord(json, i, j).AsType<TJSONArray>);
				end else if colname = 'msgid.inreplyto._id' then begin
					qres.inreplyto := ParseQueryRecord(json, i, j).AsType<DWORD>;
				end;
			end;
			AddQItem(qres);
		end;
		LVItemSort(False);
		LabelHit.Caption := Format('HIT: %d (%d)', [qhits, Length(QItem)]);
		LVMail.Refresh;
		if (Length(QItem) < qhits) and (Length(QItem) < 1000) and (self.Showing) then begin
			QuerySend(stage+1, MakeQueryParam(stage+1));
			LabelHit.Caption := LabelHit.Caption + '...';
		end;
		if (stage = 1) and (LVMail.Items.Count > 0) then begin
			//LVMail.SetFocus;
			LVMail.ItemIndex := 0;
			LVMail.Selected.MakeVisible(True);
			LVMailClick(self);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('ReceiveQuery %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.LVMailClear;
begin
	try
		LVMail.Clear;
		SetLength(LVItem, 0);
		SetLength(QItem, 0);
	except on E: Exception do
		logger.error(self.ClassName, Format('LVMailClear %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.AddQItem(qi: TGrnQueryResult);
var
	i, j, lv: Integer;
	added: Boolean;
begin
	try
		if LVSortType = SortThread then begin
			added := False;
			i := 0;
			while i < Length(LVItem) do begin
				if QItem[LVItem[i]].msgid_id = qi.inreplyto then begin
					// 親が見つかったときはその親にぶら下げる
					if QItem[LVItem[i]].haschild then begin
						j := i + 1;
						while j < Length(LVItem) do begin
							if QItem[LVItem[j]].level <> (QItem[LVItem[j]].level + 1) then Break;
							Inc(j);
						end;
						qi.level := QItem[LVItem[i]].level + 1;
						QItem := QItem + [qi];
						LVItem := Copy(LVItem, 0, j) + [Length(QItem)-1] + Copy(LVItem, j, Length(LVItem));
						Exit;
					end else begin
						QItem[LVItem[i]].haschild := True;
						qi.level := QItem[LVItem[i]].level + 1;
						QItem := QItem + [qi];
						LVItem := Copy(LVItem, 0, i+1) + [Length(QItem)-1] + Copy(LVItem, i+1, Length(LVItem));
						Exit;
					end;
				end else if qi.msgid_id = QItem[LVItem[i]].inreplyto then begin
					// 子が見つかったときはその子の親になる
					// 子は複数いる場合がある
					// でも、なんかまだ足りない気がする？？
					if not added then begin
						qi.haschild := True;
						qi.level := QItem[LVItem[i]].level;
						lv := qi.level;
					end else begin
						lv := QItem[LVItem[i]].level;
					end;
					QItem[LVItem[i]].level := QItem[LVItem[i]].level + 1;
					QItem[LVItem[i]].haschild := False;
					j := i + 1;
					while j < Length(LVItem) do begin
						if QItem[LVItem[j]].level <= lv then Break;
						QItem[LVItem[j]].level := QItem[LVItem[j]].level + 1;
						QItem[LVItem[j]].haschild := False;
						Inc(j);
					end;
					if not added then begin
						QItem := QItem + [qi];
						LVItem := Copy(LVItem, 0, i) + [Length(QItem)-1] + Copy(LVItem, i, Length(LVItem));
						added := True;
						i := j + 1;
					end else begin
						i := j;
					end;
				end else begin
					Inc(i);
				end;
			end;
			if added then Exit;
		end;
		QItem := QItem + [qi];
		LVItem := LVItem + [Length(QItem)-1];
	except on E: Exception do
		logger.error(self.ClassName, Format('AddQItem %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.LVItemSort(renew: Boolean);
var
	i: Integer;
	tmpq: TArray<TGrnQueryResult>;
	comp: IComparer<Integer>;
begin
	try
		if LVSortType = SortThread then begin
			if renew then begin
				tmpq := Copy(QItem, 0, Length(QItem)+1);
				LVMailClear;
				for i := 0 to Length(tmpq)-1 do begin
					tmpq[i].haschild := False;
					tmpq[i].expanded := False;
					tmpq[i].level := 0;
					AddQItem(tmpq[i]);
				end;
			end;
			SetLength(LVThread, 0);
			for i := 0 to Length(LVItem)-1 do begin
				if LVSortDir = SortForward then begin
					if QItem[LVItem[i]].level = 0 then
						LVThread := LVThread + [LVItem[i]];
				end else begin
					LVThread := LVThread + [LVItem[i]];
				end;
			end;
			LVMail.Items.Count := Length(LVThread);
		end else begin
			if renew then begin
				SetLength(LVItem, 0);
				for i := 0 to Length(QItem)-1 do
					LVItem := LVItem + [i];
			end;
			if LVSortType = SortSubject then begin
				comp := TDelegatedComparer<Integer>.Create(
					function(const Left, Right: Integer): Integer
					begin
						Result := CompareStr(QItem[Left].subject, QItem[Right].subject);
						if LVSortDir = SortReverse then
							Result := -Result;
						if Result = 0 then
							Result := Left - Right;
					end
				);
				TArray.Sort<Integer>(LVItem, comp);
			end else if LVSortType = SortFrom then begin
				comp := TDelegatedComparer<Integer>.Create(
					function(const Left, Right: Integer): Integer
					begin
						Result := CompareStr(QItem[Left].from, QItem[Right].from);
						if LVSortDir = SortReverse then
							Result := -Result;
						if Result = 0 then
							Result := Left - Right;
					end
				);
				TArray.Sort<Integer>(LVItem, comp);
			end else if LVSortType = SortDate then begin
				comp := TDelegatedComparer<Integer>.Create(
					function(const Left, Right: Integer): Integer
					begin
						// Dateだけはデフォルト新しい順
						Result := Round((QItem[Right].date - QItem[Left].date) * (60*60*24));
						if LVSortDir = SortReverse then
							Result := -Result;
						if Result = 0 then
							Result := Left - Right;
					end
				);
				TArray.Sort<Integer>(LVItem, comp);
			end else if LVSortType = SortDir then begin
				comp := TDelegatedComparer<Integer>.Create(
					function(const Left, Right: Integer): Integer
					begin
						Result := CompareStr(QItem[Left].dir, QItem[Right].dir);
						if LVSortDir = SortReverse then
							Result := -Result;
						if Result = 0 then
							Result := Left - Right;
					end
				);
				TArray.Sort<Integer>(LVItem, comp);
			end;
			LVMail.Items.Count := Length(LVItem);
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('LVItemSort %s %s', [E.Message, e.StackTrace]));
	end;
end;

function TBkroonga2SearchForm.GetLVQItem(idx: Integer): TGrnQueryResult;
begin
	try
		Result.level := -1;
		if LVSortType <> SortThread then begin
			if (idx < 0) or (idx >= Length(LVItem)) then Exit;
			if (LVItem[idx] < 0) or (LVItem[idx] >= Length(QItem)) then Exit;
			Result := QItem[LVItem[idx]];
		end else begin
			if (idx < 0) or (idx >= Length(LVThread)) then Exit;
			if (LVThread[idx] < 0) or (LVThread[idx] >= Length(QItem)) then Exit;
			Result := QItem[LVThread[idx]];
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('GetLVQItem %s %s', [E.Message, e.StackTrace]));
	end;
end;

function TBkroonga2SearchForm.GetMonthFirst(dt: TDateTime): TDateTime;
begin
	try
		Result := EncodeDate(StrToInt(FormatDateTime('yyyy', dt)),
			StrToInt(FormatDateTime('m', dt)), 1);
	except on E: Exception do begin
			logger.error(self.ClassName, Format('GetMonthFirst %s %s %f', [E.Message, e.StackTrace, dt]));
			Result := 0.0;
		end;
	end;
end;

function TBkroonga2SearchForm.GetMonthNext(dt: TDateTime): TDateTime;
begin
	try
		Result := EncodeDate(StrToInt(FormatDateTime('yyyy', GetMonthFirst(dt) + 32)),
			StrToInt(FormatDateTime('m', GetMonthFirst(dt) + 32)), 1);
	except on E: Exception do begin
			logger.error(self.ClassName, Format('GetMonthNext %s %s %f', [E.Message, e.StackTrace, dt]));
			Result := 0.0;
	    end;
	end;
end;

procedure TBkroonga2SearchForm.LVMailData(Sender: TObject;
  Item: TListItem);
var
	qi: TGrnQueryResult;
	dtstr, tmp: String;
	i: Integer;
	m: TMatch;
begin
	try
		logger.verbose(self.ClassName, 'LVMailData');
		qi := GetLVQItem(Item.Index);
		if qi.level < 0 then Exit;
		if MatchStr('attach', qi.attr) then
			Item.ImageIndex := 1
		else
			Item.ImageIndex := 0;
		if (qi.haschild) and (LVSortType = SortThread) then begin
			if qi.expanded then
				Item.StateIndex := 4
			else
				Item.StateIndex := 3;
		end else begin
			Item.StateIndex := -1;
		end;
		Item.SubItems.Clear;
		if (qi.level = 0) or (LVSortType <> SortThread) then begin
			Item.SubItems.Add(qi.subject);
		end else begin
			tmp := '';
			for i := 0 to qi.level-2 do tmp := tmp + '　';
			tmp := tmp + '└';
			if Item.Selected then tmp := tmp + qi.subject;
			Item.SubItems.Add(tmp);
		end;
		Item.SubItems.Add(qi.from);
		DateTimeToString(dtstr, 'yyyy/mm/dd hh:nn:ss', qi.date);
		Item.SubItems.Add(dtstr);
		if CBMailbox.Checked then begin
			m := TRegEx.Match(qi.dir, '\.mb\/(.+)$', [roIgnoreCase]);
			if m.Success then
				Item.SubItems.Add(m.Groups[1].Value)
			else
				Item.SubItems.Add(qi.dir);
		end else begin
			Item.SubItems.Add(bka.GetFolderDisplayName(IncludeTrailingPathDelimiter(bka.Slash2Yen(qi.dir))));
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('LVMailData %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.LVMailDblClick(Sender: TObject);
var
	qi: TGrnQueryResult;
begin
	try
		logger.debug(self.ClassName, 'LVMailDblClick');
		if Assigned(LVMail.Selected) then begin
			qi := GetLVQItem(LVMail.Selected.Index);
			if qi.level < 0 then Exit;
			//logger.info(self.ClassName, qi.dir+'/?'+IntToHex(qi.mailid, 1));
			bka.SetCurrentMail(AnsiString(qi.dir)+'/?'+IntToHex(qi.mailid, 1));
		end;
	except on E: Exception do
		logger.error(self.ClassName, Format('LVMailDblClick %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.LVMailKeyPress(Sender: TObject;
  var Key: Char);
begin
	if Key = #13 then begin
		LVMailDblClick(Sender);
		Key := #0;
	end;
end;

procedure TBkroonga2SearchForm.LVMailSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
    qi: TGrnQueryResult;
	i, idx: Integer;
	s: String;
	dt: TDateTime;
    tn: TTreeNode;
begin
	try
		if Selected and Assigned(LVMail.Selected) then begin
			qi := GetLVQItem(LVMail.Selected.Index);
			s := LowerCase(qi.dir);
			for i := 0 to TVFolder.Items.Count-1 do begin
				idx := Integer(TVFolder.Items[i].Data);
				if (idx >= 0) and (i < Length(TVFolName)) then
					if LowerCase(TVFolName[idx]) = s then begin
						//TVFolder.Items[i].MakeVisible;    // MakeVisibleだと横スクロールも伴ってうざい
						tn := TVFolder.Items[i];
						while tn.Parent <> nil do begin
							tn := tn.Parent;
							tn.Expand(False);
						end;
						for idx := 0 to TVFolder.Items.Count-1 do begin
							if TVFolder.Items[i].IsVisible then Break;
							if TVFolder.Items[idx].IsVisible then
								TVFolder.TopItem := TVFolder.Items[idx];
						end;
						Break;
                    end;
			end;
			{
			// 単にスクロールさせたいだけだけど、以下のやり方だと選択されちゃう
			for i := 0 to Length(MonthList)-1 do begin
				dt := bka.time_tToDateTime(StrToFloatDef(MonthList[i].key, 0));
				if (qi.date >= GetMonthFirst(dt)) and (qi.date < GetMonthNext(dt)) then begin
					DGMonth.Col := i;
					Break;
				end;
			end;
			}
		end;
		TVFolder.Refresh;
		DGMonth.Refresh;
	except on E: Exception do
		logger.error(self.ClassName, Format('LVMailSelectItem %s %s', [E.Message, e.StackTrace]));
	end;
end;

function TBkroonga2SearchForm.GetQueryHitCount(json: TJSONObject): Integer;
begin
	try
		Result := json.GetValue<TJSONObject>('body').
			GetValue<Integer>('n_hits');
	except on E: Exception do
		Result := 0;
	end;
end;

function TBkroonga2SearchForm.GetQueryRecordCount(
  json: TJSONObject): Integer;
begin
	try
		Result := json.GetValue<TJSONObject>('body').
			GetValue<TJSONArray>('records').Count;
	except on E: Exception do
		Result := 0;
	end;
end;

function TBkroonga2SearchForm.GetQueryColumnLength(
  json: TJSONObject): Integer;
begin
	try
		Result := json.GetValue<TJSONObject>('body').
			GetValue<TJSONArray>('columns').Count;
	except on E: Exception do
		Result := 0;
	end;
end;

function TBkroonga2SearchForm.GetDrilldownDtCount(
  json: TJSONObject): Integer;
begin
	try
		Result := json.GetValue<TJSONObject>('body').
			GetValue<TJSONObject>('drilldowns').
			GetValue<TJSONObject>('ddt').
			GetValue<Integer>('n_hits');
	except on E: Exception do
		Result := -1;
	end;
end;

function TBkroonga2SearchForm.GetDrilldownDirCount(
  json: TJSONObject): Integer;
begin
	try
		Result := json.GetValue<TJSONObject>('body').
			GetValue<TJSONObject>('drilldowns').
			GetValue<TJSONObject>('ddir').
			GetValue<Integer>('n_hits');
	except on E: Exception do
		Result := -1;
	end;
end;

function TBkroonga2SearchForm.GetDrilldownFromCount(
  json: TJSONObject): Integer;
begin
	try
		Result := json.GetValue<TJSONObject>('body').
			GetValue<TJSONObject>('drilldowns').
			GetValue<TJSONObject>('dfrom').
			GetValue<Integer>('n_hits');
	except on E: Exception do
		Result := -1;
	end;
end;

function TBkroonga2SearchForm.ParseQueryColumn(json: TJSONObject;
  colidx: Integer): TJSONObject;
begin
	try
		Result := json.GetValue<TJSONObject>('body').
			GetValue<TJSONArray>('columns').Items[colidx] as TJSONObject;
	except on E: Exception do
		Result := nil;
	end;
end;

function TBkroonga2SearchForm.ParseQueryRecord(json: TJSONObject; idx,
  colidx: Integer): TJSONValue;
begin
	try
		Result := (json.GetValue<TJSONObject>('body').
			GetValue<TJSONArray>('records').Items[idx] as TJSONArray).
			Items[colidx];
	except on E: Exception do
		Result := nil;
	end;
end;

function TBkroonga2SearchForm.ParseDrilldownDtRecord(json: TJSONObject;
  idx: Integer): TDrilldownRecord;
var
	jary: TJSONArray;
begin
	try
		jary := json.GetValue<TJSONObject>('body').
			GetValue<TJSONObject>('drilldowns').
			GetValue<TJSONObject>('ddt').
			GetValue<TJSONArray>('records').Items[idx] as TJSONArray;
		Result.key := jary.Items[0].Value;
		Result.value := jary.Items[1].GetValue<Integer>;
	except on E: Exception do
		begin
			Result.key := '0.0';
			Result.value := 0;
		end;
	end;
end;

function TBkroonga2SearchForm.ParseDrilldownDirRecord(json: TJSONObject;
  idx: Integer): TDrilldownRecord;
var
	jary: TJSONArray;
begin
	try
		jary := json.GetValue<TJSONObject>('body').
			GetValue<TJSONObject>('drilldowns').
			GetValue<TJSONObject>('ddir').
			GetValue<TJSONArray>('records').Items[idx] as TJSONArray;
		Result.key := jary.Items[0].Value;
		Result.value := jary.Items[1].GetValue<Integer>;
	except on E: Exception do
		begin
			Result.key := '';
			Result.value := 0;
		end;
	end;
end;

function TBkroonga2SearchForm.ParseDrilldownFromRecord(json: TJSONObject;
  idx: Integer): TDrilldownRecord;
var
	jary: TJSONArray;
begin
	try
		jary := json.GetValue<TJSONObject>('body').
			GetValue<TJSONObject>('drilldowns').
			GetValue<TJSONObject>('dfrom').
			GetValue<TJSONArray>('records').Items[idx] as TJSONArray;
		Result.key := jary.Items[0].Value;
		Result.value := jary.Items[1].GetValue<Integer>;
	except on E: Exception do
		begin
			Result.key := '0.0';
			Result.value := 0;
		end;
	end;
end;

function TBkroonga2SearchForm.ParseNumberJSONArray(
  j: TJSONArray): TArray<DWORD>;
var
	i: Integer;
begin
	try
		Result := [];
		for i := 0 to j.Count-1 do
			Result := Result + [j.Items[i].AsType<DWORD>];
	except on E: Exception do
		logger.error(self.ClassName, Format('ParseNumberJSONArray %s %s', [E.Message, e.StackTrace]));
	end;
end;

function TBkroonga2SearchForm.ParseStringJSONArray(
  j: TJSONArray): TArray<String>;
var
	i: Integer;
begin
	try
		Result := [];
		for i := 0 to j.Count-1 do
			Result := Result + [j.Items[i].AsType<String>];
	except on E: Exception do
		logger.error(self.ClassName, Format('ParseStringJSONArray %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.SetLVSort(tp: TLVSortType);
var
	i: Integer;
	s: String;
begin
	try
		logger.debug(self.ClassName, 'SetLVSort '+IntToStr(Integer(tp)));
		for i := 0 to LVMail.Columns.Count-1 do begin
			s := LVMail.Columns.Items[i].Caption;
			if (Copy(s, 1, 1) = '▽') or (Copy(s, 1, 1) = '△') or (Copy(s, 1, 1) = '＞') then
				LVMail.Columns.Items[i].Caption := Copy(s, 2, Length(s));
		end;
		LVSortType := tp;
		LVItemSort(True);
		s := '';
		if tp = SortThread then begin
			if LVSortDir = SortForward then s := '＞' else s := '▽';
			i := 0
		end else if tp = SortSubject then begin
			if LVSortDir = SortForward then s := '▽' else s := '△';
			i := 1
		end else if tp = SortFrom then begin
			if LVSortDir = SortForward then s := '▽' else s := '△';
			i := 2
		end else if tp = SortDate then begin
			if LVSortDir = SortForward then s := '△' else s := '▽'; // Dateだけはデフォルト=新しい順
			i := 3
		end else begin
			if LVSortDir = SortForward then s := '▽' else s := '△';
			i := 4;
		end;
		LVMail.Columns.Items[i].Caption := s + LVMail.Columns.Items[i].Caption;
	except on E: Exception do
		logger.error(self.ClassName, Format('SetLVSort %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.SetSnippetHTMLText;
var
	fn: String;
	sl: TStringList;
begin
	try
		fn := ExtractFilePath(IniFileName) + 'snippethtml.txt';
		try
			if FileExists(fn) then begin
				sl := TStringList.Create;
				try
					sl.LoadFromFile(fn, TEncoding.UTF8);
					snippethtml := UTF8String(sl.Text);
					Exit;
				finally
					sl.Free;
				end;
			end;
		except on E: Exception do
			;
		end;
		snippethtml := UTF8String(
			'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+
			'<html lang="ja"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8">'+
			'<style type="text/css">body{font-size:12px;margin:0px;padding:0px;}'+
			'span.k1{background-color:rgb(255,255,128);color:black;font-weight:bold;}'+
			'span.k2{background-color:rgb(131,139,253);color:white;font-weight:bold;}'+
			'span.k3{background-color:rgb(220,146,235);color:black;font-weight:bold;}'+
			'span.k4{background-color:rgb(90,133,90);color:white;font-weight:bold;}'+
			'span.k5{background-color:rgb(136,110,136);color:white;font-weight:bold;}'+
			'span.dot{color:gray;}</style></head><body>');
	except on E: Exception do
		logger.error(self.ClassName, Format('SetSnippetHTMLText %s %s', [E.Message, e.StackTrace]));
	end;
end;

procedure TBkroonga2SearchForm.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
	Accept := ShowTree;
end;

procedure TBkroonga2SearchForm.Splitter1Moved(Sender: TObject);
begin
	if ShowTree then TreeWidth := PanelTree.Width;
end;

function TBkroonga2SearchForm.ChangeShowMonth(sh: Boolean): Boolean;
begin
	try
		if sh then begin
			DGMonth.Visible := True;
			PanelMonth.Height := 64;
			SBShowMonth.ImageIndex := 5;
			SBZeroMonth.Visible := True;
		end else begin
			DGMonth.Visible := False;
			PanelMonth.Height := 16;
			SBShowMonth.ImageIndex := 4;
			SBZeroMonth.Visible := False;
		end;
		Result := sh;
	except on E: Exception do
		logger.error(self.ClassName, Format('ChangeShowMonth %s %s', [E.Message, e.StackTrace]));
	end;
end;

function TBkroonga2SearchForm.ChangeShowTree(sh: Boolean): Boolean;
begin
	try
		if sh then begin
			PanelTree.Constraints.MinWidth := 100;
			PanelTree.Width := TreeWidth;
			TVFolder.Visible := True;
			SBShowTree.ImageIndex := 33;
			SBOnlyFolder.Visible := True;
		end else begin
			if ShowTree then TreeWidth := PanelTree.Width;
			TVFolder.Visible := False;
			PanelTree.Constraints.MinWidth := 0;
			PanelTree.Width := 16;
			SBShowTree.ImageIndex := 3;
			SBOnlyFolder.Visible := False;
		end;
		Result := sh;
	except on E: Exception do
		logger.error(self.ClassName, Format('ChangeShowTree %s %s', [E.Message, e.StackTrace]));
	end;
end;

function TBkroonga2SearchForm.ChangeZeroMonth(flg: Boolean): Boolean;
begin
	try
		if flg then begin
			SBZeroMonth.ImageIndex := 37;
			SBZeroMonth.Hint := '0件の月を表示する';
		end else begin
			SBZeroMonth.ImageIndex := 36;
			SBZeroMonth.Hint := '0件の月は表示しない';
		end;
		Result := flg;
	except on E: Exception do
		logger.error(self.ClassName, Format('ChangeZeroMonth %s %s', [E.Message, e.StackTrace]));
	end;
end;

function TBkroonga2SearchForm.ChangeOnlyFolder(flg: Boolean): Boolean;
begin
	try
		if flg then begin
			SBOnlyFolder.ImageIndex := 34;
			SBOnlyFolder.Hint := '選択したフォルダのみ';
		end else begin
			SBOnlyFolder.ImageIndex := 35;
			SBOnlyFolder.Hint := '選択したフォルダ以下を含む';
		end;
		Result := flg;
	except on E: Exception do
		logger.error(self.ClassName, Format('ChangeOnlyFolder %s %s', [E.Message, e.StackTrace]));
	end;
end;

end.
