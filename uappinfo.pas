unit uappinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  // http://wiki.lazarus.freepascal.org/Show_Application_Title,_Version,_and_Company
  // FPC 3.0 fileinfo reads exe resources as long as you register the appropriate units
  , fileinfo
  , winpeimagereader {need this for reading exe info}
  , elfreader {needed for reading ELF executables}
  , machoreader {needed for reading MACH-O executables}
  ;

function GetAppVersion: string;

implementation

function GetAppVersion: string;
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

end.

