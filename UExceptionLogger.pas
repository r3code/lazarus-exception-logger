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
    FExtraInfo: TStringList;
    FMaxCallStackDepth: Integer;
    FLogFileName: string;
    FIgnoreList: TStringList;
    FStackTrace: TStackTrace;
    FLastException: Exception;
    FExceptionSender: TObject;
    FOnThreadSynchronize: TThreadSynchronizeEvent;
    procedure ThreadSynchronize(AObject: TObject; Method: TThreadMethod);
    function GetAppVersion: string;
    procedure SetMaxCallStackDepth(const AValue: Integer);
    procedure MakeReport;
    procedure ShowForm;
  public
    procedure LoadDetails;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    procedure CreateTextReport(Output: TStringList);
    procedure LogToFile(Report: TStringList);
    procedure LogStackTraceToFile(AStackTrace: TStackTrace);
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
  SExceptionClass = 'Exception class';
  SExceptionMessage = 'Exception message';
  SExeName = 'Executable';
  SApplicationTitle = 'App. title';
  SReportTime = 'Date/time';
  SProcessID = 'Process ID';
  SThreadID = 'Thread ID';
  SVersion = 'Version';
  SCallStack = 'Call stack';
  SGeneral = 'General';
  SErrorOccured = 'Error occured during program execution:';
  STerminate = 'Exit program';
  SClose = 'Continue';
  SDetails = 'Details';
  SIgnoreNextTime = 'Next time ignore this exception';
  SExceptionInfo = 'Exception info';
  SIndex = 'Index';
  SAddress = 'Address';
  SLine = 'Line';
  SClass = 'Class';
  SProcedureMethod = 'Procedure/method';
  SUnit = 'Unit';
  SExceptionHandlerCannotBeSynchronized = 'Exception handler cannot be synchronized with main thread.';

implementation

uses
  UExceptionForm, VersionSupport;

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

procedure TExceptionLogger.CreateTextReport(Output: TStringList);
  procedure SubAddLine(const ATitle, AValue: string);
  begin
    Output.Add(Format('%-19s: %s', [ATitle, AValue]));
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
  Output.Clear;
  SubAddLine(SReportTime,FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now));
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
  SubAddLine('Compiled date', VersionSupport.GetCompiledDate);
  SubAddLine('Build target', VersionSupport.GetTargetInfo);
  SubAddLine('LCL version', VersionSupport.GetLCLVersion);
  SubAddLine('Widget set', VersionSupport.GetWidgetSet);
  // Exception Info
  SubAddLine(SExceptionClass, FLastException.ClassName);
  SubAddLine(SExceptionMessage, FLastException.Message);
  // Custom Extra Info
  SubAddExtraInfo;
end;

procedure TExceptionLogger.LogToFile(Report: TStringList);
var
  LogFile: TFileStream;
  Buffer: string;
begin
  Buffer := Report.Text;
  if FileExists(FLogFileName) then
    LogFile := TFileStream.Create(UTF8Decode(FLogFileName), fmOpenReadWrite)
    else LogFile := TFileStream.Create(UTF8Decode(FLogFileName), fmCreate);
  with LogFile do try
    Seek(0, soFromEnd);
    if Length(Buffer) > 0 then
      Write(Buffer[1], Length(Buffer));
  finally
    LogFile.Free;
  end;
end;

procedure TExceptionLogger.LogStackTraceToFile(AStackTrace: TStackTrace);
var
  I: Integer;
  LogFile: TFileStream;
  Line: string;
begin
  if FileExists(FLogFileName) then
    LogFile := TFileStream.Create(UTF8Decode(FLogFileName), fmOpenReadWrite)
    else LogFile := TFileStream.Create(UTF8Decode(FLogFileName), fmCreate);
  with LogFile do try
    Seek(0, soFromEnd);
    for I := 0 to AStackTrace.Count - 1 do
    with TStackFrameInfo(AStackTrace[I]) do
    begin
      Line := Format('%0.2d: %s %s in %s(%d)' + LineEnding,
        [Index, IntToHex(Address, 8), FunctionName, Source, LineNumber]);
      if Length(Line) > 0 then
        Write(Line[1], Length(Line));
    end;
    Line := LineEnding;
    Write(Line[1], Length(Line));
  finally
    LogFile.Free;
  end;
end;

procedure TExceptionLogger.ShowReportForm;
begin
  ExceptionForm.LoadStackTraceToListView(FStackTrace);
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
  if (MainThreadID <> ThreadID) then begin
    if Assigned(FOnThreadSynchronize) then
      FOnThreadSynchronize(Sender, @ShowForm)
      else raise Exception.Create(SExceptionHandlerCannotBeSynchronized);
  end else ShowForm;
end;

procedure TExceptionLogger.MakeReport;
var
  Report: TStringList;
begin
  FStackTrace.GetInfo;
  if FIgnoreList.IndexOf(FLastException.ClassName) = -1 then begin
    Report := TStringList.Create;
    try
      CreateTextReport(Report);
      if FLogFileName <> '' then begin
        LogToFile(Report);
        LogStackTraceToFile(FStackTrace);
      end;
      ExceptionForm.MemoExceptionInfo.Lines.Assign(Report);
      ShowReportForm;
    finally
      Report.Free;
    end;
    if ExceptionForm.CheckBoxIgnore.Checked then
      FIgnoreList.Add(FLastException.ClassName);
  end;
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

initialization

{$IFOPT D+}
  //disables "optimizations" when converting stack to string (in unit lineinfo)
  AllowReuseOfLineInfoData:=false;
{$endif}

end.

