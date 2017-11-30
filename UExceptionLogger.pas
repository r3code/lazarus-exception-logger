unit UExceptionLogger;

{$H+}

interface

uses
  Classes, SysUtils, UStackTrace, CustomLineInfo, Forms
  // http://wiki.lazarus.freepascal.org/Show_Application_Title,_Version,_and_Company
  // FPC 3.0 fileinfo reads exe resources as long as you register the appropriate units
  , fileinfo
  , winpeimagereader {need this for reading exe info}
  , elfreader {needed for reading ELF executables}
  , machoreader {needed for reading MACH-O executables}       
  {$if FPC_FULlVERSION>=30002}
  {$ifopt D+}
  , lineinfo
  {$ENDIF}
  // enable Debugging - Display line info... (-gl)
  {$endif}
  ;

type
  TThreadSynchronizeEvent = procedure (AObject: TObject; Method: TThreadMethod) of object;

  { TExceptionLogger }

  TExceptionLogger = class(TComponent)
  private
    FStartTime: Int64;
    FExtraInfo: TStringList;
    FMaxCallStackDepth: Integer;
    FLogFileName: string;
    FIgnoreList: TStringList;
    FBasicData: TStringList;
    FStackTrace: TStackTrace;
    FLastException: Exception;
    FExceptionSender: TObject;
    FOnThreadSynchronize: TThreadSynchronizeEvent;
    procedure ThreadSynchronize(AObject: TObject; Method: TThreadMethod);
    function GetAppVersion: string;
    function GetProgramUpTime: string;
    procedure SetMaxCallStackDepth(const AValue: Integer);
    function FormatBasicDataReport(ABasicData: TStringList): TStringList;
    procedure PrepareReport;
    procedure MakeReport;
    procedure ShowForm;
  public
    procedure LoadDetails;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    function CollectReportBasicData: TStringList;
    function CollectStackTrace: TStringList;
    procedure LogToFile(ADataList: TStringList);
    procedure ShowReportForm;
    procedure AddExtraInfo(const AFieldName, AValue: string);
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
  SIgnoreNextTime = 'Next time ignore this exception';

  // Stack
  SIndex = 'Index';
  SAddress = 'Address';
  SLine = 'Line';
  SClass = 'Class';
  SProcedureMethod = 'Procedure/method';
  SUnit = 'Unit';
  SExceptionHandlerCannotBeSynchronized = 'Exception handler cannot be synchronized with main thread.';

implementation

uses
  UExceptionForm, VersionSupport, usysinfo;

const
  LOG_LINE_FORMAT = '%0.2d: %s %s in %s(%d)';

procedure Register;
begin
  RegisterComponents('Samples', [TExceptionLogger]);
end;

{ TExceptionLogger }

constructor TExceptionLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIgnoreList := TStringList.Create;
  FStackTrace := TStackTrace.Create;
  FExtraInfo := TStringList.Create;
  FStartTime := SysUtils.GetTickCount64;
  MaxCallStackDepth := 20;
  Application.OnException := @ExceptionHandler;
  Application.Flags := Application.Flags - [AppNoExceptionMessages];
  OnThreadSynchronize := @ThreadSynchronize;
end;

destructor TExceptionLogger.Destroy;
begin
  FExtraInfo.Free;
  FStackTrace.Free;
  FIgnoreList.Free;
  inherited Destroy;
end;

function TExceptionLogger.CollectReportBasicData(): TStringList;
  procedure SubAddLine(const AName, AValue: string);
  begin
    Result.Add(Format('%s=%s',[AName, AValue]));
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
  Result := TStringList.Create;
  SubAddLine(SReportTime,FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now));
  // OS Info
  SubAddLine(SOperatingSystem, GetOsVersionInfo);
  SubAddLine(SProgramUpTime, GetProgramUpTime);
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
  SubAddLine(SCompiledDate, VersionSupport.GetCompiledDate);
  SubAddLine(SBuildTarget, VersionSupport.GetTargetInfo);
  SubAddLine(SLCLVersion, VersionSupport.GetLCLVersion);
  SubAddLine(SWidgetSet, VersionSupport.GetWidgetSet);
  // Exception Info
  SubAddLine(SExceptionClass, FLastException.ClassName);
  SubAddLine(SExceptionMessage, FLastException.Message);
  // Custom Extra Info
  SubAddExtraInfo;
end;

function TExceptionLogger.CollectStackTrace: TStringList;
var
  i: integer;
  stackFrame: TStackFrameInfo;
  Line: string;
begin
  Result := TStringList.Create;
  for I := 0 to FStackTrace.Count - 1 do
  begin
    stackFrame := TStackFrameInfo(FStackTrace[I]);
    Line := Format(LOG_LINE_FORMAT,
      [stackFrame.Index, IntToHex(stackFrame.Address, 8),
      stackFrame.FunctionName, stackFrame.Source, stackFrame.LineNumber]);
    Result.Add(Line);
  end;
end;

procedure TExceptionLogger.LogToFile(ADataList: TStringList);
var
  LogFile: TFileStream;
  Buffer: string;
begin
  Buffer := ADataList.Text;

  if FileExists(FLogFileName) then
    LogFile := TFileStream.Create(UTF8Decode(FLogFileName), fmOpenReadWrite)
    else LogFile := TFileStream.Create(UTF8Decode(FLogFileName), fmCreate);
  with LogFile do
  try
    Seek(0, soFromEnd);
    if Length(Buffer) > 0 then
      Write(Buffer[1], Length(Buffer));
  finally
    LogFile.Free;
  end;
end;


procedure TExceptionLogger.ShowReportForm;
begin
  if not ExceptionForm.Visible then ExceptionForm.ShowModal;
end;

procedure TExceptionLogger.AddExtraInfo(const AFieldName, AValue: string);
begin
  FExtraInfo.Add(Format('%s=%s', [AFieldName, AValue]));
end;

procedure TExceptionLogger.ExceptionHandler(Sender: TObject; E: Exception);
begin
  BackTraceStrFunc := @StabBackTraceStr;
  FStackTrace.GetExceptionBackTrace;
  FLastException := E;
  FExceptionSender := Sender;
  PrepareReport;
  if (MainThreadID <> ThreadID) then begin
    if Assigned(FOnThreadSynchronize) then
      FOnThreadSynchronize(Sender, @ShowForm)
      else raise Exception.Create(SExceptionHandlerCannotBeSynchronized);
  end else ShowForm;
end;

procedure TExceptionLogger.MakeReport;
var
  basicData, basicDataReport, stackTraces: TStringList;
begin
  FStackTrace.GetInfo;
  if FIgnoreList.IndexOf(FLastException.ClassName) <> -1 then
    Exit;
  basicDataReport := FormatBasicDataReport(FBasicData);
  stackTraces := CollectStackTrace;
  try
    if FLogFileName <> '' then begin
      LogToFile(basicDataReport);
      LogToFile(stackTraces);
    end;
    ExceptionForm.SetBasicInfo(basicDataReport);
    ExceptionForm.LoadStackTraceToListView(FStackTrace);
    ShowReportForm;
  finally
    basicDataReport.Free;
    stackTraces.Free;
  end;
  if ExceptionForm.CheckBoxIgnore.Checked then
    FIgnoreList.Add(FLastException.ClassName);
end;

procedure TExceptionLogger.ShowForm;
begin
  ExceptionForm.Logger := Self;
  ExceptionForm.LabelMessage.Caption := FLastException.Message;
  ExceptionForm.MemoExceptionInfo.Clear;
  if not ExceptionForm.Visible then ExceptionForm.ShowModal;
end;

procedure TExceptionLogger.LoadDetails;
begin
  if FExceptionSender is TThread then
    TThread.Synchronize(TThread(FExceptionSender), @MakeReport)
    else MakeReport;
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

procedure TExceptionLogger.PrepareReport;
begin
  FBasicData := CollectReportBasicData;
end;

procedure TExceptionLogger.ThreadSynchronize(AObject: TObject;
  Method: TThreadMethod);
begin
  if AObject is TThread then TThread.Synchronize(TThread(AObject), Method)
    else raise Exception.Create(SExceptionHandlerCannotBeSynchronized);
end;

function TExceptionLogger.GetAppVersion: string;
var
  FileVerInfo: TFileVersionInfo;
begin        
  Result := '';
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    try
      FileVerInfo.ReadFileInfo;
      Result := FileVerInfo.VersionStrings.Values['FileVersion'];
      Exit;
    except
      on E: EResNotFound do
        Exit;
    end;
  finally
    FileVerInfo.Free;
  end;
end;

function TExceptionLogger.GetProgramUpTime: string;
const
  SECOND = 1000;
  MINUTE = 60 * SECOND;
  HOUR = 60 * MINUTE;
  DAY = 24 * HOUR;
var
  delta: Int64;
  days, hours, minutes, seconds, ms: Integer;
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

{$IFOPT D+}
  //disables "optimizations" when converting stack to string (in unit lineinfo)
  AllowReuseOfLineInfoData:=false;
{$endif}

end.

