unit UExceptionLogger;

{$H+}

interface

uses
  {$ifdef windows}Windows,{$endif}
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
    FMaxCallStackDepth: Integer;
    FLogFileName: string;
    FOnThreadSynchronize: TThreadSynchronizeEvent;
    procedure ThreadSynchronize(AObject: TObject; Method: TThreadMethod);
    function GetAppVersion: string;
    procedure SetMaxCallStackDepth(const AValue: Integer);
    procedure MakeReport;
    procedure ShowForm;
  public
    StackTrace: TStackTrace;
    LastException: Exception;
    ExceptionSender: TObject;
    IgnoreList: TStringList;
    procedure LoadDetails;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    procedure CreateTextReport(Output: TStringList);
    procedure LogToFile(Report: TStringList);
    procedure LogStackTraceToFile(AStackTrace: TStackTrace);
    procedure ShowReportForm;
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
  UExceptionForm;

procedure Register;
begin
  RegisterComponents('Samples', [TExceptionLogger]);
end;

{ TExceptionLogger }

constructor TExceptionLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IgnoreList := TStringList.Create;
  StackTrace := TStackTrace.Create;
  MaxCallStackDepth := 20;
  Application.OnException := @ExceptionHandler;
  Application.Flags := Application.Flags - [AppNoExceptionMessages];
  OnThreadSynchronize := @ThreadSynchronize;
end;

destructor TExceptionLogger.Destroy;
begin
  StackTrace.Free;
  IgnoreList.Free;
  inherited Destroy;
end;

procedure TExceptionLogger.CreateTextReport(Output: TStringList);
  function SubFormatLine(const ATitle, AValue: string): string;
  begin
    Result := Format('%-19s: %s', [ATitle, AValue]);
  end;
begin
  with Output do begin
    Clear;
    Add(SubFormatLine(SReportTime,FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)));
    Add(SubFormatLine(SProcessID,IntToStr(GetProcessID)));
    {$IFNDEF DARWIN}
    Add(SubFormatLine(SThreadID,IntToStr(GetThreadID)));
    {$ENDIF}
    Add(SubFormatLine(SExeName, ExtractFileName(Application.ExeName)));
    Add(SubFormatLine(SApplicationTitle, Application.Title));
    Add(SubFormatLine(SVersion, GetAppVersion));
    Add(SubFormatLine(SExceptionClass, LastException.ClassName));
    Add(SubFormatLine(SExceptionMessage, LastException.Message));
  end;
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
  ExceptionForm.LoadStackTraceToListView(StackTrace);
  if not ExceptionForm.Visible then ExceptionForm.ShowModal;
end;

procedure TExceptionLogger.ExceptionHandler(Sender: TObject; E: Exception);
begin
  BackTraceStrFunc := @StabBackTraceStr;
  StackTrace.GetExceptionBackTrace;
  LastException := E;
  ExceptionSender := Sender;
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
  StackTrace.GetInfo;
  if IgnoreList.IndexOf(LastException.ClassName) = -1 then begin
    Report := TStringList.Create;
    try
      CreateTextReport(Report);
      if FLogFileName <> '' then begin
        LogToFile(Report);
        LogStackTraceToFile(StackTrace);
      end;
      ExceptionForm.MemoExceptionInfo.Lines.Assign(Report);
      ShowReportForm;
    finally
      Report.Free;
    end;
    if ExceptionForm.CheckBoxIgnore.Checked then
      IgnoreList.Add(LastException.ClassName);
  end;
end;

procedure TExceptionLogger.ShowForm;
begin
  ExceptionForm.Logger := Self;
  ExceptionForm.LabelMessage.Caption := LastException.Message;
  ExceptionForm.MemoExceptionInfo.Clear;
  if not ExceptionForm.Visible then ExceptionForm.ShowModal;
end;

procedure TExceptionLogger.LoadDetails;
begin
  if ExceptionSender is TThread then
    TThread.Synchronize(TThread(ExceptionSender), @MakeReport)
    else MakeReport;
end;

procedure TExceptionLogger.SetMaxCallStackDepth(const AValue: Integer);
begin
  FMaxCallStackDepth := AValue;
  StackTrace.MaxDepth := AValue;
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

