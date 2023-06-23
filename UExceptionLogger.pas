unit UExceptionLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , UStackTrace
  ,{$IFDEF CPU64}lnfodwrf{$ELSE}CustomLineInfo{$ENDIF}
  , Forms
  {$if FPC_FULlVERSION>=30002}
  {$ifopt D+}
  {$IFNDEF CPU64}, lineinfo{$ENDIF}
  {$ENDIF}
  // enable Debugging - Display line info... (-gl)
  {$endif}
  ;

type
  TThreadSynchronizeEvent = procedure (AObject: TObject; Method: TThreadMethod) of object;

  { TExceptionLogger }

  TExceptionLogger = class(TComponent)
  private
    FStartTime: Cardinal;
    FExtraInfo: TStringList;
    FMaxCallStackDepth: Integer;
    FLogFileName: string;
    FIgnoreList: TStringList;
    FBasicData: TStringList;
    FStackTrace: TStackTrace;
    FLastException: Exception;
    FExceptionSender: TObject;
    FLoggerLastError: string;
    FOnThreadSynchronize: TThreadSynchronizeEvent;
    procedure ThreadSynchronize(AObject: TObject; Method: TThreadMethod);
    function GetProgramUpTime: string;
    procedure SetMaxCallStackDepth(const AValue: Integer);
    function FormatBasicDataReport(ABasicData: TStringList): TStringList;
    procedure PrepareReport;
    procedure ShowForm;
    procedure SaveBugReportToFile;
    procedure CollectReportBasicData(AStore: TStringList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HandleException(Sender: TObject; E: Exception);

    function StackTraceAsStringList: TStringList;
    procedure LogToFile(ADataList: TStringList);
    procedure AddExtraInfo(const AFieldName, AValue: string);
    procedure SkipExceptionNextTime;
  published
    property LogFileName: string read FLogFileName write FLogFileName;
    property MaxCallStackDepth: Integer read FMaxCallStackDepth write SetMaxCallStackDepth;
    property OnThreadSynchronize: TThreadSynchronizeEvent read FOnThreadSynchronize
      write FOnThreadSynchronize;
  end;

procedure Register;

resourcestring
  // Log titles
  SExceptionClass = 'Exception class';
  SExceptionMessage = 'Exception message';
  SExeName = 'Executable';
  SApplicationTitle = 'App. title';
  SReportTime = 'Date/time';
  // Operating system info
  SOperatingSystem = 'Operating system';
  SUserName = 'user name';
  SCurrentDiskFreeSpaceSize = 'free disk space';
  // Time info
  SProgramUpTime = 'program up time';
  // Process Info
  SProcessID = 'Process ID';
  SThreadID = 'Thread ID';
  SVersion = 'Version';
  SCompiledDate = 'Compiled date';
  SBuildTarget = 'Build target';
  SLCLVersion = 'LCL version';
  SWidgetSet = 'Widget set';
  // Form Titles
  SExceptionInfo = 'Exception info';
  SCallStack = 'Call stack';
  SGeneral = 'General';
  SErrorOccured = 'Error occured during program execution:';
  STerminate = 'Exit program';
  SClose = 'Continue';
  SDetails = 'Details';
  SIgnoreNextTime = 'Skip this exception next';

  // Stack
  SIndex = 'Index';
  SAddress = 'Address';
  SLine = 'Line';
  SClass = 'Class';
  SProcedureMethod = 'Procedure/method';
  SUnit = 'Unit';
  SExceptionHandlerCannotBeSynchronized = 'Exception handler cannot be synchronized with main thread.';

  SErrorReportFileNotCrearted = 'Report file not created';

var
  exceptionLogger: TExceptionLogger;

implementation

uses
  UExceptionForm
  , lelVersionSupport
  , usysinfo
  , uappinfo
  ;


procedure Register;
begin
  RegisterComponents('Samples', [TExceptionLogger]);
end;

{ TExceptionLogger }

constructor TExceptionLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIgnoreList := TStringList.Create;
  FBasicData := TStringList.Create;
  FStackTrace := TStackTrace.Create;
  FExtraInfo := TStringList.Create;
  FStartTime := SysUtils.GetTickCount64;
  MaxCallStackDepth := 20;
  Application.OnException := @HandleException;
  Application.Flags := Application.Flags - [AppNoExceptionMessages];
  OnThreadSynchronize := @ThreadSynchronize;
end;

destructor TExceptionLogger.Destroy;
begin
  if Assigned(FBasicData) then
    FreeAndNil(FBasicData);
  FreeAndNil(FExtraInfo);
  FreeAndNil(FStackTrace);
  FreeAndNil(FIgnoreList);
  inherited Destroy;
end;

procedure TExceptionLogger.CollectReportBasicData(AStore: TStringList);
  procedure SubAddLine(const AName, AValue: string);
  begin
    AStore.Add(Format('%s=%s',[AName, AValue]));
  end;
  procedure SubAddExtraInfo;
  var
    i: integer;
  begin
    for i := 0 to FExtraInfo.Count - 1 do
    begin
      SubAddLine(FExtraInfo.Names[i], FExtraInfo.ValueFromIndex[i]);
    end;
  end;
begin
  SubAddLine(SReportTime,FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now));
  // OS Info
  SubAddLine(SOperatingSystem, GetOsVersionInfo);
  SubAddLine(SUserName, GetCurrentUserName);
  SubAddLine(SProgramUpTime, GetProgramUpTime);
  SubAddLine(SCurrentDiskFreeSpaceSize, GetCurrentDiskFreeSpaceSize);
  // Process Info
  SubAddLine(SProcessID,IntToStr(GetProcessID));
  {$IFNDEF DARWIN}
  SubAddLine(SThreadID,IntToStr(GetThreadID));
  {$ENDIF}
  // App Info
  SubAddLine(SExeName, ExtractFileName(Application.ExeName));
  SubAddLine(SApplicationTitle, Application.Title);
  SubAddLine(SVersion, GetAppVersion);
  // Compile time info
  SubAddLine(SCompiledDate, lelVersionSupport.GetCompiledDate);
  SubAddLine(SBuildTarget, lelVersionSupport.GetTargetInfo);
  SubAddLine(SLCLVersion, lelVersionSupport.GetLCLVersion);
  SubAddLine(SWidgetSet, lelVersionSupport.GetWidgetSet);
  // Exception Info
  SubAddLine(SExceptionClass, FLastException.ClassName);
  SubAddLine(SExceptionMessage, FLastException.Message);
  // Custom Extra Info
  SubAddExtraInfo;
  AStore.Add(EmptyStr);
end;

function TExceptionLogger.StackTraceAsStringList: TStringList;
var
  i: integer;
  stackFrame: TStackFrameInfo;
  Line: string;
  rowNo, address, method, source, lineNo: string;
begin
  Result := TStringList.Create;
  for I := 0 to FStackTrace.Count - 1 do
  begin
    stackFrame := TStackFrameInfo(FStackTrace[I]);
    method := stackFrame.FunctionName;
    if stackFrame.FunctionClassName <> EmptyStr then
      method := stackFrame.FunctionClassName + '.' + stackFrame.FunctionName;
    source := EmptyStr;
    if stackFrame.Source <> EmptyStr then
      source := 'in ' + stackFrame.Source;
    if stackFrame.LineNumber = 0 then
      source := EmptyStr;
    rowNo := Format('%0.2d', [stackFrame.Index]);
    address := IntToHex(stackFrame.Address, 8);
    lineNo := EmptyStr;
    if stackFrame.LineNumber > 0 then
      lineNo := Format('(%d)', [stackFrame.LineNumber]);
    Line := Format('%s: %s   %s %s %s',[rowNo, address, method, source, lineNo]);
    Result.Add(Line);
  end;
  Result.Add(EmptyStr);
  Result.Add(EmptyStr);
end;

procedure TExceptionLogger.LogToFile(ADataList: TStringList);
var
  LogFile: TFileStream;
  Buffer: string;
begin
  Buffer := ADataList.Text;

  if FileExists(FLogFileName) then
    LogFile := TFileStream.Create(UTF8Decode(FLogFileName), fmOpenReadWrite)
  else
    LogFile := TFileStream.Create(UTF8Decode(FLogFileName), fmCreate);
  with LogFile do
  try
    Seek(0, soFromEnd);
    if Length(Buffer) > 0 then
      Write(Buffer[1], Length(Buffer));
  finally
    LogFile.Free;
  end;
end;

procedure TExceptionLogger.AddExtraInfo(const AFieldName, AValue: string);
begin
  FExtraInfo.Add(Format('%s=%s', [AFieldName, AValue]));
end;

procedure TExceptionLogger.SkipExceptionNextTime;
begin
  if not Assigned(FLastException) then
    Exit;
  FIgnoreList.Add(FLastException.ClassName);
end;

procedure TExceptionLogger.HandleException(Sender: TObject; E: Exception);
begin
  BackTraceStrFunc := {$ifdef CPU64}@DwarfBackTraceStr{$ELSE}@StabBackTraceStr{$ENDIF};
  FStackTrace.GetExceptionBackTrace;
  FLastException := E;
  FExceptionSender := Sender;
  if (MainThreadID <> ThreadID) then
  begin
    if Assigned(FOnThreadSynchronize) then
      FOnThreadSynchronize(Sender, @ShowForm)
    else
      raise Exception.Create(SExceptionHandlerCannotBeSynchronized);
  end
  else
    ShowForm;
end;

procedure TExceptionLogger.PrepareReport;
begin
  FBasicData.Clear;
  CollectReportBasicData(FBasicData);
  FStackTrace.GetInfo;
end;

procedure TExceptionLogger.ShowForm;
var
  biFormatted: TStringList;
begin
  if FIgnoreList.IndexOf(FLastException.ClassName) <> -1 then
    Exit;

  Application.DisableIdleHandler;
  try
    if FExceptionSender is TThread then
      TThread.Synchronize(TThread(FExceptionSender), @PrepareReport)
    else
      PrepareReport;

    SaveBugReportToFile;

    with TExceptionForm.Create(Application) do
    try
      Logger := Self;
      try
        biFormatted := FormatBasicDataReport(FBasicData);
        SetBasicInfo(biFormatted);
      finally
        biFormatted.free;
      end;
      LoadStackTraceToListView(FStackTrace);
      SetLoggerError(FLoggerLastError);
      lblErrorText.Caption := FLastException.Message;
      ShowModal;
    finally
      Free;
    end;
  finally
    Application.EnableIdleHandler;
  end;
end;

procedure TExceptionLogger.SaveBugReportToFile;
var
  basicDataReport, stackTraces: TStringList;
begin
  basicDataReport := FormatBasicDataReport(FBasicData);
  stackTraces := StackTraceAsStringList;
  try
    if FLogFileName <> EmptyStr then
    try
      LogToFile(basicDataReport);
      LogToFile(stackTraces);
    except
      on E: Exception do
         FLoggerLastError := SErrorReportFileNotCrearted
         + '. ' + E.Message;
    end;
  finally
    basicDataReport.Free;
    stackTraces.Free;
  end;
end;

procedure TExceptionLogger.SetMaxCallStackDepth(const AValue: Integer);
begin
  FMaxCallStackDepth := AValue;
  FStackTrace.MaxDepth := AValue;
end;

function TExceptionLogger.FormatBasicDataReport(ABasicData: TStringList
  ): TStringList;
var
  fieldName, fieldValue: string;
  i: Integer;
begin
  Result := TStringList.Create;
  if not Assigned(ABasicData) then
    Exit;
  if ABasicData.Count = 0 then
    Exit;
  for i := 0 to ABasicData.Count - 1 do
  begin
    if ABasicData.Names[i] <> EmptyStr then
    begin
      fieldName:=ABasicData.Names[i];
      fieldValue:=ABasicData.ValueFromIndex[i];
      Result.Add(Format('%-19s: %s', [fieldName, fieldValue]));
    end
    else
      Result.Add(ABasicData[i]);
  end;
end;


procedure TExceptionLogger.ThreadSynchronize(AObject: TObject;
  Method: TThreadMethod);
begin
  if AObject is TThread then TThread.Synchronize(TThread(AObject), Method)
    else raise Exception.Create(SExceptionHandlerCannotBeSynchronized);
end;

function TExceptionLogger.GetProgramUpTime: string;
const
  SECOND = 1000;
  MINUTE = 60 * SECOND;
  HOUR = 60 * MINUTE;
  DAY = 24 * HOUR;
var
  delta: Cardinal;
  days, hours, minutes, seconds, ms: int64;
begin
  delta := SysUtils.GetTickCount64 - FStartTime;
  days := delta div DAY;
  hours := (delta - days*DAY) div HOUR;
  minutes := (delta - days*DAY - hours * HOUR) div MINUTE;
  seconds:= (delta - days*DAY - hours * HOUR - minutes*MINUTE) div SECOND;
  ms := delta - days*DAY - hours * HOUR - minutes*MINUTE - seconds*SECOND;
  Result := Format('%ddays %dhours %dmin %dsec %dms',[days, hours, minutes,
    seconds, ms]);
end;


initialization

exceptionLogger := TExceptionLogger.Create(Application);

{$IFOPT D+}
  //disables "optimizations" when converting stack to string (in unit lineinfo)
  AllowReuseOfLineInfoData:=false;
{$endif}


end.

