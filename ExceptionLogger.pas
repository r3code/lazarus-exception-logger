{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ExceptionLogger;

interface

uses
  UStackTrace, UExceptionForm, UExceptionLogger, DCConvertEncoding, DCWindows, 
  usysinfo, VersionSupport, CustomLineInfo, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UExceptionLogger', @UExceptionLogger.Register);
end;

initialization
  RegisterPackage('ExceptionLogger', @Register);
end.
