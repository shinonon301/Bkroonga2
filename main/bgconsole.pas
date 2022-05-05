unit bgconsole;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, mylogger;

type
	TBgconsole = class(TThread)
	private
		fExefn: String;
		fCmdline: String;
		fResult: RawByteString;
		fFlagResult: Boolean;
		fTimeout: Integer;
		fMaxlen: Integer;
        fRunning: Boolean;
	public
		property ResStr: RawByteString read fResult;
        property Running: Boolean read fRunning;
		constructor Create(exefn, cmdline: String;
			flagresult: Boolean = False;
			timeoutmsec: Integer = 0;
			maxlength: Integer = 0);
		destructor Destroy; override;
		procedure Execute; override;
	end;

implementation

{ TBgconsole }

constructor TBgconsole.Create(exefn, cmdline: String;
	flagresult: Boolean = False;
	timeoutmsec: Integer = 0;
	maxlength: Integer = 0);
begin
	inherited Create(False);
	logger.info(self.ClassName, Format('Create %s', [exefn]));
	fExefn := exefn;
	fCmdline := cmdline;
	fResult := '';
	fFlagResult := flagresult;
	fTimeout := timeoutmsec;
	fMaxlen := maxlength;
	fRunning := False;
	self.freeOnTerminate := False;
end;

destructor TBgconsole.Destroy;
begin
	logger.info(self.ClassName, 'Destroy');
	inherited;
end;

procedure TBgconsole.Execute;
const
	BUFFER_SIZE = 4096;
	WAIT_FOR_READY = 1000;
	WAIT_FOR_RUN = 1;
var
	hReadPipe, hWritePipe: THandle;
	hStdInReadPipe, hStdInWritePipe, hStdInWritePipeDup: THandle;
	hErrReadPipe, hErrWritePipe: THandle;
	sa: TSecurityAttributes;
	StartupInfo: TStartupInfo;
	ProcessInfo: TProcessInformation;
	debug: _DEBUG_EVENT;
	dwStdOut, dwStdErr, dwRet, dwContinueStatus: DWord;
	dwDebugEventCodePrev, dwExceptionCodePrev,  dwExceptionFlagsPrev, dwExceptionInformation0Prev, dwExceptionInformation1Prev: DWORD;
	cmdline: String;
	abyBuffer: RawByteString;
	srec: TSearchRec;
	p, tocnt: Integer;
	//debugproc: Boolean;
	//debugbreak: Boolean;
	bsuccess: Boolean;
	errmsg: String;
begin
	//if not FileExists(fexefn) then begin
	//	Terminate;
	//	Exit;
	//end;
	ZeroMemory(@sa, sizeof(TSecurityAttributes));
	with sa do
	begin
		nLength := sizeof(TSecurityAttributes);
		lpSecurityDescriptor := nil;
		bInheritHandle := true;
	end;

	cmdline := AnsiString(fExefn) + ' ' + AnsiString(fCmdline);
	hReadPipe := 0; hWritePipe := 0;
	hErrReadPipe := 0; hErrWritePipe := 0;

	CreatePipe(hStdInReadPipe, hStdInWritePipe, @sa, BUFFER_SIZE);
	DuplicateHandle(GetCurrentProcess(), hStdInWritePipe, GetCurrentProcess(),
									@hStdInWritePipeDup, 0, false, DUPLICATE_SAME_ACCESS);
	CloseHandle(hStdInWritePipe);
	CreatePipe(hReadPipe, hWritePipe, @sa, BUFFER_SIZE);
	try
		CreatePipe(hErrReadPipe, hErrWritePipe, @sa, BUFFER_SIZE);
		try
			ZeroMemory(@StartupInfo, sizeof(TStartupInfo));
			with StartupInfo do
			begin
				cb := sizeof(TStartupInfo);
				dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
				// これがないと DOS 窓が表示されてしまう
				wShowWindow := SW_HIDE;
				// 標準 IO にパイプの端っこを指定してやる
				hStdInput := hStdInReadPipe;
				hStdOutput := hWritePipe;
				hStdError  := hErrWritePipe;
			end;
			// コンソールアプリ起動
			//debugbreak := False;
			logger.info(self.ClassName, 'CreateProcess '+fExefn+' '+fCmdline);
			//debugproc := True;
			// デバッグプロセスにすると異常終了を検知して終了できるようになるらしいけど、
			// ものすごく起動が遅くなるので取りやめ
			//bsuccess := CreateProcess(nil, PChar(cmdline), @sa, nil, true,
			//  	CREATE_NEW_PROCESS_GROUP or DETACHED_PROCESS or DEBUG_PROCESS or DEBUG_ONLY_THIS_PROCESS,
			//	//CREATE_NEW_PROCESS_GROUP or DETACHED_PROCESS or DEBUG_PROCESS or DEBUG_ONLY_THIS_PROCESS,
			//	//CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS or DEBUG_PROCESS or DEBUG_ONLY_THIS_PROCESS,
			//	nil, nil{PChar(ExtractFilePath(sCommandLine))}, StartupInfo, ProcessInfo);
			//if not bsuccess then begin
			//	debugproc := False;
			bsuccess := CreateProcess(nil, PChar(cmdline), @sa, nil, true,
					CREATE_NEW_PROCESS_GROUP or DETACHED_PROCESS,
					nil, nil{PChar(ExtractFilePath(sCommandLine))}, StartupInfo, ProcessInfo);
			//end;
			if bsuccess then begin
				// 入力待ちになるまで待ってから，
				//if not debugproc then begin
					WaitForInputIdle(ProcessInfo.hProcess, WAIT_FOR_READY);
				//end else begin
				//	while (WaitForInputIdle(ProcessInfo.hProcess, 1) = WAIT_TIMEOUT) do begin
				//		if WaitForDebugEvent(debug, 1) then begin
				//			dwContinueStatus := DBG_CONTINUE;
				//			if debug.dwDebugEventCode = EXCEPTION_DEBUG_EVENT then begin
				//				break;
				//			end else if debug.dwDebugEventCode = EXIT_PROCESS_DEBUG_EVENT then begin
				//				break;
				//			end;
				//			if not(ContinueDebugEvent(debug.dwProcessId, debug.dwThreadId, dwContinueStatus)) then
				//				break;
				//		end else begin
				//			sleep(1);
				//		end;
				//	end;
				//end;

				// 入力を与え終わった
				CloseHandle(hStdInWritePipeDup);

				SetLength(abyBuffer, BUFFER_SIZE+2);
				SetLength(fresult, 0);
				try
					ReturnValue := 0;
					tocnt := 0;
					fRunning := True;
					while True do begin
						(*if debugproc then begin
							if WaitForDebugEvent(debug, 1) then begin
								dwContinueStatus := DBG_CONTINUE;
								if debug.dwDebugEventCode = EXCEPTION_DEBUG_EVENT then begin
									if (debug.Exception.ExceptionRecord.ExceptionCode <> EXCEPTION_SINGLE_STEP)
										and (debug.Exception.ExceptionRecord.ExceptionCode <> EXCEPTION_BREAKPOINT) then begin
										if (dwDebugEventCodePrev = EXCEPTION_DEBUG_EVENT)
											and (dwExceptionCodePrev = debug.Exception.ExceptionRecord.ExceptionCode)
											and (dwExceptionFlagsPrev = debug.Exception.ExceptionRecord.ExceptionFlags)
											and (dwExceptionInformation0Prev = debug.Exception.ExceptionRecord.ExceptionInformation[0])
											and (dwExceptionInformation1Prev = debug.Exception.ExceptionRecord.ExceptionInformation[1]) then begin
												debugbreak := True;
											end;
										dwDebugEventCodePrev := EXCEPTION_DEBUG_EVENT;
										dwExceptionCodePrev := debug.Exception.ExceptionRecord.ExceptionCode;
										dwExceptionFlagsPrev := debug.Exception.ExceptionRecord.ExceptionFlags;
										dwExceptionInformation0Prev := debug.Exception.ExceptionRecord.ExceptionInformation[0];
										dwExceptionInformation1Prev := debug.Exception.ExceptionRecord.ExceptionInformation[1];
										dwContinueStatus := DBG_EXCEPTION_NOT_HANDLED;
									end;
								end else if debug.dwDebugEventCode = EXIT_PROCESS_DEBUG_EVENT then begin
									if debug.dwProcessId = ProcessInfo.dwProcessId then
										debugbreak := True;
								end;
								if not(ContinueDebugEvent(debug.dwProcessId, debug.dwThreadId, dwContinueStatus)) then
									debugbreak := True;
							end;
						end;*)
						dwRet := WaitForSingleObject(ProcessInfo.hProcess, WAIT_FOR_RUN);
						tocnt := tocnt + WAIT_FOR_RUN;
						// 標準出力パイプの内容を調べる
						PeekNamedPipe(hReadPipe, nil, 0, nil, @dwStdOut, nil);
						if (dwStdOut > 0) then begin
							tocnt := 0;
							// 内容が存在すれば、読み取る
							ReadFile(hReadPipe, abyBuffer[1], BUFFER_SIZE, dwStdOut, nil);
							if fflagresult then begin
								p := Length(fresult);
								SetLength(fresult, p + dwStdOut);
								Move(abyBuffer[1], fresult[p+1], dwStdOut);
							end;
						end;
						// 同様にエラー出力の処理
						PeekNamedPipe(hErrReadPipe, nil, 0, nil, @dwStdErr, nil);
						if (dwStdErr > 0) then begin
							// エラー出力は読み捨てる
							ReadFile(hErrReadPipe, abyBuffer[1], BUFFER_SIZE, dwStdErr, nil);
						end;
						if (dwRet = WAIT_OBJECT_0) then Break;
						//if debugbreak then Break;
						if (ftimeout > 0) and (tocnt > ftimeout) then begin
							ReturnValue := -1;
							Break;
						end;
						if (fMaxlen > 0) and (Length(fresult) >= fMaxlen) then Break;
					end;
					if fflagresult then
						fresult := PAnsiChar(fresult);  // NULL文字を削除する
				finally
					CloseHandle(ProcessInfo.hProcess);
					CloseHandle(ProcessInfo.hThread);
					CloseHandle(hStdInReadPipe);
				end;
			end
			else begin
				SetLength(errmsg, 1024);
				FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError, 0, @errmsg[1], 1024, nil);
				logger.error(self.ClassName, 'CreateProcess Error '+fExefn+' '+fCmdline);
				raise Exception.Create('CreateProcess Error ('+fExefn+','+errmsg+')' );
			end;
		finally
			CloseHandle(hErrReadPipe);
			CloseHandle(hErrWritePipe);
		end;
	finally
		CloseHandle(hReadPipe);
		CloseHandle(hWritePipe);
		fRunning := False;
	end;
end;

end.
