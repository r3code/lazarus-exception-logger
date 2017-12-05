unit usysinfo;

{$mode objfpc}{$H+}

interface

function GetOsVersionInfo: string;
function GetCurrentDiskFreeSpaceSize: string;
function GetCurrentUserName: string;

implementation

uses
  Classes
  , SysUtils
  , strutils
  , DCConvertEncoding
  , LazUTF8
  {$IF DEFINED(UNIX)}
  , BaseUnix
  , users
    {$IFDEF DARWIN}
    , MacOSAll
    {$ENDIF}
  {$ENDIF}
  {$IFDEF LCLQT}
  , qt4
  {$ENDIF}
  {$IFDEF LCLQT5}
  , qt5
  {$ENDIF}
  {$IFDEF LCLGTK2}
  , gtk2
  {$ENDIF}
  {$IFDEF WINDOWS}
  , Windows
  , JwaNative
  , JwaNtStatus
  , JwaWinType
  , DCWindows
  {$ENDIF}

  ;

{$IF DEFINED(WINDOWS)}
const
  AccessModes: array[0..2] of DWORD  = (
                GENERIC_READ,
                GENERIC_WRITE,
                GENERIC_READ or GENERIC_WRITE);
  ShareModes: array[0..4] of DWORD = (
               0,
               0,
               FILE_SHARE_READ,
               FILE_SHARE_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE);
  OpenFlags: array[0..3] of DWORD  = (
                0,
                FILE_FLAG_WRITE_THROUGH,
                FILE_FLAG_NO_BUFFERING,
                FILE_FLAG_WRITE_THROUGH or FILE_FLAG_NO_BUFFERING);

var
  CurrentDirectory: String;
{$ELSEIF DEFINED(UNIX)}
const

{$IF NOT DECLARED(O_SYNC)}
  O_SYNC   = 0;
{$ENDIF}

{$IF NOT DECLARED(O_DIRECT)}
  O_DIRECT = 0;
{$ENDIF}

  AccessModes: array[0..2] of cInt  = (
                O_RdOnly,
                O_WrOnly,
                O_RdWr);
  OpenFlags: array[0..3] of cInt  = (
                0,
                O_SYNC,
                O_DIRECT,
                O_SYNC or O_DIRECT);
{$ENDIF}

// from https://sourceforge.net/p/doublecmd/code/HEAD/tree/trunk/src/platform/udcversion.pas
function mbFileAccess(const FileName: String; Mode: Word): Boolean;
{$IFDEF WINDOWS}
const
  AccessMode: array[0..2] of DWORD  = (
                GENERIC_READ,
                GENERIC_WRITE,
                GENERIC_READ or GENERIC_WRITE);
var
  hFile: System.THandle;
  dwDesiredAccess: DWORD;
  dwShareMode: DWORD = 0;
begin
  dwDesiredAccess := AccessMode[Mode and 3];
  if Mode = fmOpenRead then // If checking Read mode no sharing mode given
    Mode := Mode or fmShareDenyNone;
  dwShareMode := ShareModes[(Mode and $F0) shr 4];
  hFile:= CreateFileW(PWideChar(UTF16LongName(FileName)), dwDesiredAccess, dwShareMode,
                      nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  Result := hFile <> INVALID_HANDLE_VALUE;
  if Result then
    FileClose(hFile);
end;
{$ELSE}
const
  AccessMode: array[0..2] of LongInt  = (
                R_OK,
                W_OK,
                R_OK or W_OK);
begin
  Result:= fpAccess(CeUtf8ToSys(FileName), AccessMode[Mode and 3]) = 0;
end;
{$ENDIF}

// from https://sourceforge.net/p/doublecmd/code/HEAD/tree/trunk/src/platform/udcversion.pas
{$IF DEFINED(UNIX)}
{en
   Reads file into strings.
   Returns @false if file not found or cannot be read.
}
function GetStringsFromFile(FileName: String; out sl: TStringList): Boolean;
begin
  Result := False;
  sl := nil;
  if mbFileAccess(FileName, fmOpenRead) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(FileName);
      Result := True;
    except
      on EFilerError do
         Exit; // Bypass
    end;
  end;
end;

{en
   Reads first line of file into a string.
   Returns @false if file not found or cannot be read.
}
function GetStringFromFile(FileName: String; out str: String): Boolean;
var
  sl: TStringList;
begin
  str := EmptyStr;
  Result := GetStringsFromFile(FileName, sl);
  if Result then
  try
    if sl.Count > 0 then
      str := sl.Strings[0];
  finally
    sl.Free;
  end;
end;

function GetOsFromLsbRelease: String;
  function TrimQuotes(const Str: String): String;
  begin
    Result:= TrimSet(Str, ['"', '''']);
  end;
var
  sl: TStringList;
begin
  Result := EmptyStr;

  if GetStringsFromFile('/etc/lsb-release', sl) then
  try
    if sl.Count > 0 then
    begin
      Result := sl.Values['DISTRIB_DESCRIPTION'];

      if Result <> EmptyStr then
        Result := TrimQuotes(Result)
      else
        Result := sl.Values['DISTRIB_ID'] +
                  sl.Values['DISTRIB_RELEASE'] +
                  sl.Values['DISTRIB_CODENAME'];
    end;
  finally
    sl.Free;
  end;
end;

function GetOsFromProcVersion: String;
var
  i: Integer;
  s: String;
begin
  Result := EmptyStr;

  if GetStringFromFile('/proc/version', s) then
  begin
    // Get first three strings separated by space.

    i := Pos(' ', s);
    if i > 0 then
      Result := Result + Copy(s, 1, i);
    Delete(s, 1, i);

    i := Pos(' ', s);
    if i > 0 then
      Result := Result + Copy(s, 1, i);
    Delete(s, 1, i);

    i := Pos(' ', s);
    if i > 0 then
      Result := Result + Copy(s, 1, i - 1);
    Delete(s, 1, i);
  end;
end;

function GetOsFromIssue: String;
begin
  if not GetStringFromFile('/etc/issue', Result) then
    Result := EmptyStr;
end;

function GetDebianVersion: String;
var
  s: String;
begin
  if GetStringFromFile('/etc/debian_version', s) then
  begin
    Result := 'Debian';
    if s <> EmptyStr then
      Result := Result + ' ' + s;
  end
  else
    Result := EmptyStr;
end;

function GetSuseVersion: String;
begin
  if GetStringFromFile('/etc/SuSE-release', Result) or
     GetStringFromFile('/etc/suse-release', Result) then
  begin
    if Result = EmptyStr then
      Result := 'Suse';
  end
  else
    Result := EmptyStr;
end;

function GetRedHatVersion: String;
begin
  if GetStringFromFile('/etc/redhat-release', Result) then
  begin
    if Result = EmptyStr then
      Result := 'RedHat';
  end
  else
    Result := EmptyStr;
end;

function GetMandrakeVersion: String;
begin
  if GetStringFromFile('/etc/mandrake-release', Result) then
  begin
    if Result = EmptyStr then
      Result := 'Mandrake';
  end
  else
    Result := EmptyStr;
end;

function GetVersionNumber: String;
var
  Info: utsname;
  I: Integer = 1;
begin
  FillChar(Info, SizeOf(Info), 0);
  fpUname(Info);
  Result := Info.release;
  while (I <= Length(Result)) and (Result[I] in ['0'..'9', '.']) do
    Inc(I);
  Result := Copy(Result, 1, I - 1);
end;

{$IFDEF DARWIN}
function GetMacOSXVersion: String;
var
  versionMajor,
  versionMinor, versionBugFix: SInt32;
begin
  Result:= EmptyStr;
  if (Gestalt(gestaltSystemVersionMajor, versionMajor) <> noErr) then Exit;
  if (Gestalt(gestaltSystemVersionMinor, versionMinor) <> noErr) then Exit;
  if (Gestalt(gestaltSystemVersionBugFix, versionBugFix) <> noErr) then Exit;
  Result:= Format('Mac OS X %d.%d.%d', [versionMajor, versionMinor, versionBugFix]);
end;
{$ENDIF}

{$ENDIF}

{$IF DEFINED(WINDOWS)}
procedure TryGetNativeSystemInfo(var SystemInfo: TSystemInfo);
type
  TGetNativeSystemInfo = procedure (var lpSystemInfo: TSystemInfo); stdcall;
var
  hLib: HANDLE;
  GetNativeSystemInfoProc: TGetNativeSystemInfo;
begin
  hLib := LoadLibrary(LPCTSTR('kernel32.dll'));
  if hLib <> 0 then
  begin
    try
      GetNativeSystemInfoProc := TGetNativeSystemInfo(GetProcAddress(hLib, 'GetNativeSystemInfo'));
      if Assigned(GetNativeSystemInfoProc) then
        GetNativeSystemInfoProc(SystemInfo)
      else
        GetSystemInfo(SystemInfo);
    finally
      FreeLibrary(hLib);
    end;
  end
  else
    GetSystemInfo(SystemInfo);
end;
{$ENDIF}

function GetOsVersionInfo: string;
{$IF DEFINED(WINDOWS)}
const
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
var
  si: SYSTEM_INFO;
  osvi: TOsVersionInfoExW;
{$ENDIF}
begin
  {$IF DEFINED(WINDOWS)}
  Result := 'Windows';

  ZeroMemory(@osvi, SizeOf(TOsVersionInfoExW));
  osvi.dwOSVersionInfoSize := SizeOf(TOsVersionInfoExW);

  if (RtlGetVersion(@osvi) = STATUS_SUCCESS) or GetVersionExW(@osvi) then
  begin
    ZeroMemory(@si, SizeOf(si));
    TryGetNativeSystemInfo(si);

    case osvi.dwPlatformId of
      VER_PLATFORM_WIN32_WINDOWS:
        case osvi.dwMajorVersion of
          4: case osvi.dwMinorVersion of
                0: Result := Result + ' 95';
               10: Result := Result + ' 98';
               90: Result := Result + ' ME';
             end;
        end;

      VER_PLATFORM_WIN32_NT:
        begin
          case osvi.dwMajorVersion of
            3: Result := Result + ' NT 3.5';
            4: Result := Result + ' NT 4';
            5: case osvi.dwMinorVersion of
                 0: Result := Result + ' 2000';
                 1: begin
                      Result := Result + ' XP';
                      if osvi.wSuiteMask = $0000 then
                        Result := Result + ' Home'
                      else if osvi.wSuiteMask = $0200 then
                        Result := Result + ' Professional';
                    end;
                 2: if (osvi.wProductType = VER_NT_WORKSTATION) and
                       (si.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
                    begin
                      Result := Result + ' XP Professional x64'
                    end
                    else if (osvi.wProductType = VER_NT_SERVER) then
                    begin
                      if osvi.wSuiteMask = $8000 then
                        Result := Result + ' Home Server'
                      else
                        Result := Result + ' Server 2003';
                    end;
               end;
            6: case osvi.dwMinorVersion of
                 0: if (osvi.wProductType = VER_NT_WORKSTATION) then
                    begin
                      Result := Result + ' Vista';
                      if osvi.wSuiteMask = $0000 then
                        Result := Result + ' Ultimate'
                      else if osvi.wSuiteMask = $0200 then
                        Result := Result + ' Home';
                    end
                    else if (osvi.wProductType = VER_NT_SERVER) then
                      Result := Result + ' Server 2008';
                 1: if (osvi.wProductType = VER_NT_WORKSTATION) then
                      Result := Result + ' 7'
                    else if (osvi.wProductType = VER_NT_SERVER) then
                      Result := Result + ' Server 2008 R2';
                 2: if (osvi.wProductType = VER_NT_WORKSTATION) then
                      Result := Result + ' 8'
                    else if (osvi.wProductType = VER_NT_SERVER) then
                      Result := Result + ' Server 2012';
                 3: if (osvi.wProductType = VER_NT_WORKSTATION) then
                      Result := Result + ' 8.1'
                    else if (osvi.wProductType = VER_NT_SERVER) then
                      Result := Result + ' Server 2012 R2';
               end;
           10: case osvi.dwMinorVersion of
                 0: if (osvi.wProductType = VER_NT_WORKSTATION) then
                    begin
                      Result := Result + ' 10';
                      if (osvi.wSuiteMask and VER_SUITE_PERSONAL <> 0) then
                        Result := Result + ' Home';
                    end
              end;
          end;
        end;
    end;

    // If something detected then add service pack number and architecture.
    if Result <> 'Windows' then
    begin
      if osvi.wServicePackMajor > 0 then
      begin
        Result := Result + ' SP' + IntToStr(osvi.wServicePackMajor);
        if osvi.wServicePackMinor > 0 then
          Result := Result + '.' + IntToStr(osvi.wServicePackMinor);
      end;

      if si.wProcessorArchitecture in [PROCESSOR_ARCHITECTURE_AMD64] then
        Result := Result + ' x86_64'
      else
        Result := Result + ' i386';
    end
    else
      Result := Result + ' Build ' + IntToStr(osvi.dwBuildNumber);
  end;
  {$ELSEIF DEFINED(UNIX)}
  // Try using linux standard base.
  Result := GetOsFromLsbRelease;

  // Try some distribution-specific files.
  if Result = EmptyStr then
    Result := GetDebianVersion;
  if Result = EmptyStr then
    Result := GetRedHatVersion;
  if Result = EmptyStr then
    Result := GetSuseVersion;
  if Result = EmptyStr then
    Result := GetMandrakeVersion;

  {$IFDEF DARWIN}
  if Result = EmptyStr then
    Result := GetMacOSXVersion;
  {$ENDIF}

  // Other methods.
  if Result = EmptyStr then
    Result := GetOsFromIssue;
  if Result = EmptyStr then
    Result := GetOsFromProcVersion;

  // Set default names.
  if Result = EmptyStr then
  begin
    {$IF DEFINED(LINUX)}
    Result := 'Linux';
    {$ELSEIF DEFINED(DARWIN)}
    Result := 'Darwin';  // MacOS
    {$ELSEIF DEFINED(FREEBSD)}
    Result := 'FreeBSD';
    {$ELSEIF DEFINED(BSD)}
    Result := 'BSD';
    {$ELSE}
    Result := 'Unix';
    {$ENDIF}
    Result += ' ' + GetVersionNumber;
  end;
  {$ENDIF}
end;

function GetCurrentDiskFreeSpaceSize: string;
const
  GB = 1024 * 1024 * 1024;
begin
  Result := Format('%d GB',[DiskFree(0) div GB]);
end;

// from http://forum.lazarus.freepascal.org/index.php/topic,23171.msg138057.html#msg138057
function GetCurrentUserName: string;
{$IFDEF WINDOWS}
const
  MaxLen = 256;
var
  Len: DWORD;
  WS: WideString;
  Res: windows.BOOL;
{$ENDIF}
begin
  Result := '';
  {$IFDEF UNIX}
  {$IF (DEFINED(LINUX)) OR (DEFINED(FREEBSD))}
   //GetUsername in unit Users, fpgetuid in unit BaseUnix
  Result := SysToUtf8(GetUserName(fpgetuid));
  {$ELSE Linux/BSD}
  Result := GetEnvironmentVariableUtf8('USER');
  {$ENDIF UNIX}
  {$ELSE}
  {$IFDEF WINDOWS}
  Len := MaxLen;
  {$IFnDEF WINCE}
  if Win32MajorVersion <= 4 then
  begin
    SetLength(Result,MaxLen);
    Res := Windows.GetuserName(@Result[1], Len);
    if Res then
    begin
      SetLength(Result,Len-1);
      Result := SysToUtf8(Result);
    end
    else SetLength(Result,0);
  end
  else
  {$ENDIF NOT WINCE}
  begin
    SetLength(WS, MaxLen-1);
    Res := Windows.GetUserNameW(@WS[1], Len);
    if Res then
    begin
      SetLength(WS, Len - 1);
      Result := Utf16ToUtf8(WS);
    end
    else SetLength(Result,0);
  end;
  {$ENDIF WINDOWS}
  {$ENDIF UNIX}
end;

end.

