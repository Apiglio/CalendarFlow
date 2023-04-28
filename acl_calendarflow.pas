{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit acl_calendarflow;

{$warn 5023 off : no warning about unused units}
interface

uses
  CalendarFlow, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CalendarFlow', @CalendarFlow.Register);
end;

initialization
  RegisterPackage('acl_calendarflow', @Register);
end.
