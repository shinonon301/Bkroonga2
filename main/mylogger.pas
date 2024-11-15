unit mylogger;

interface

uses
	System.SysUtils, System.Classes, System.Generics.Collections, Winapi.Windows;

const
	LogDefaultSize = Integer(1000000);
	LogMinSize = Integer(100000);
	LogDefaultHistory = Integer(7);
	LogMaxHistory = Integer(99);
type
	TLogLevel = (LOG_VERBOSE, LOG_DEBUG, LOG_INFO, LOG_WARN, LOG_ERROR, LOG_FATAL);
	TMyLogger = class(TThread)
	private
		FLogfn: String;
		FQueue: TQueue<String>;
		FMaxsize: Integer;
		FMaxhist: Integer;
		FLogLevel: TLogLevel;						// ログファイルに出力するログレベル(デフォルト LOG_DEBUG)
		FGlobalLogLevel: TLogLevel;		// GlobalLogに出力する(リアルタイムにモニタする)ログレベル(デフォルト LOG_INFO)
		function nowstr: String;
		procedure AddGlobalLog(msg: String);
	public
		constructor Create(logfn: String; loglev: TLogLevel = LOG_DEBUG;
			gloglev: TLogLevel = LOG_WARN; maxsize: Integer = LogDefaultSize;
			maxhist: Integer = LogDefaultHistory);
		destructor Destroy; override;
		procedure Execute; override;
		procedure SetLogLevel(lev: TLogLevel);
		procedure SetGlobalLogLevel(lev: TLogLevel);
		procedure verbose(name, msg: String);
		procedure putlog(lev: TLogLevel; name, msg: String);
		procedure debug(name, msg: String);
		procedure info(name, msg: String);
		procedure warn(name, msg: String);
		procedure error(name, msg: String);
		procedure fatal(name, msg: String);
	end;

var
	logger: TMyLogger;
	LogLevelMsg: array[0..5] of String = ('VERB ', 'DEBUG', 'INFO ', 'WARN ', 'ERROR', 'FATAL');
	// 外部でこのGlobalLogQueueを参照すればリアルタイムにログをモニタできる
	// ただ、適切にこのキューからログを取らないといけない(256件以上は溜まらない)
	GlobalLogQueue: TQueue<String>;

implementation

{ TMyLogger }

constructor TMyLogger.Create(logfn: String; loglev: TLogLevel = LOG_DEBUG;
	gloglev: TLogLevel = LOG_WARN; maxsize: Integer = LogDefaultSize;
	maxhist: Integer = LogDefaultHistory);
begin
	FLogfn := logfn;
	FMaxsize := maxsize;
	if FMaxsize < (LogMinSize) then FMaxsize := LogMinSize; // 100kB以下にはしない
	FMaxhist := maxhist;
	if FMaxhist < 0 then FMaxhist := 0;
	if FMaxhist > LogMaxHistory then FMaxhist := LogMaxHistory;
	inherited Create(False);
	SetLogLevel(loglev);
	SetGlobalLogLevel(gloglev);
	self.FreeOnTerminate := False;
end;

destructor TMyLogger.Destroy;
begin
	FreeAndNil(FQueue);
	FreeAndNil(GlobalLogQueue);
	inherited;
end;

procedure TMyLogger.Execute;
var
	s: String;
	us: UTF8String;
	f: TFileStream;
	histflg: Boolean;
	i: Integer;
begin
	FQueue := TQueue<String>.Create;
	FQueue.Clear;
	GlobalLogQueue := TQueue<String>.Create;
	GlobalLogQueue.Clear;
	while not self.Terminated do begin
		if FQueue.Count <= 0 then begin
			Sleep(10);
		end else begin
			while FQueue.Count > 0 do begin
				if self.Terminated then break;
				s := FQueue.Extract;
				if FLogfn = '' then continue;
				try
					if FileExists(FLogfn) then begin
						f := TFileStream.Create(FLogfn, fmOpenReadWrite or fmShareDenyWrite)
					end else begin
						continue;
						//f := TFileStream.Create(FLogfn, fmCreate or fmShareDenyWrite);
					end;
					histflg := False;
					try
						f.Position := f.Size;
						us := UTF8String(s) + #13#10;
						f.WriteBuffer(us[1], Length(us));
						if f.Size >= FMaxsize then histflg := True;
					finally
						f.Free;
					end;
					if histflg then begin
						if FileExists(FLogfn+'.'+IntToStr(FMaxhist)) then
							System.Sysutils.DeleteFile(FLogfn+'.'+IntToStr(FMaxhist));
						for i := FMaxhist downto 2 do begin
							if FileExists(FLogfn+'.'+IntToStr(i-1)) then begin
								CopyFile(PWideChar(FLogfn+'.'+IntToStr(i-1)), PWideChar(FLogfn+'.'+IntToStr(i)), False);
								System.Sysutils.DeleteFile(FLogfn+'.'+IntToStr(i-1));
							end;
						end;
						if FileExists(FLogfn) then
							CopyFile(PWideChar(FLogfn), PWideChar(FLogfn+'.1'), False);
						f := TFileStream.Create(FLogfn, fmCreate);
						f.Free;
					end;
				except on E: Exception do
					//MessageBox(0, PChar(E.Message), nil, MB_OK);
				end;
			end;
		end;
	end;
	self.ReturnValue := 0;
end;

function TMyLogger.nowstr: String;
begin
	Result := FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', Now);
end;

procedure TMyLogger.putlog(lev: TLogLevel; name, msg: String);
var
	s: String;
begin
	if (FLogLevel <= lev) and (Ord(lev) >= Low(LogLevelMsg)) and (Ord(lev) <= High(LogLevelMsg)) then begin
		s := Format('%s [%s][%s] %s', [nowstr, LogLevelMsg[Ord(lev)], name, msg]);
		if (FLogfn <> '') and (FileExists(FLogfn)) then
			FQueue.Enqueue(s);
		if FGlobalLogLevel <= lev then
			AddGlobalLog(s);
	end;
end;

procedure TMyLogger.SetGlobalLogLevel(lev: TLogLevel);
begin
	FGlobalLogLevel := lev;
end;

procedure TMyLogger.SetLogLevel(lev: TLogLevel);
begin
	FLogLevel := lev;
end;

procedure TMyLogger.verbose(name, msg: String);
begin
	putlog(LOG_VERBOSE, name, msg);
end;

procedure TMyLogger.debug(name, msg: String);
begin
	putlog(LOG_DEBUG, name, msg);
end;

procedure TMyLogger.info(name, msg: String);
begin
	putlog(LOG_INFO, name, msg);
end;

procedure TMyLogger.warn(name, msg: String);
begin
	putlog(LOG_WARN, name, msg);
end;

procedure TMyLogger.error(name, msg: String);
begin
	putlog(LOG_ERROR, name, msg);
end;

procedure TMyLogger.fatal(name, msg: String);
begin
	putlog(LOG_FATAL, name, msg);
end;

procedure TMyLogger.AddGlobalLog(msg: String);
begin
	if GlobalLogQueue.Count < 256 then
		GlobalLogQueue.Enqueue(msg);
end;


end.

