unit DbusExtension;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dbus;

procedure dbus_connection_setup_with_g_main(connection: PDbusConnection; dummy: pointer); cdecl; external 'libdbus-glib-1';

type
    { TDBUSThread }

  TDBUSThread = class (TThread)
  public
    fBus: PDBusConnection;
     procedure Execute; override;
    destructor Destroy; override;
  end;

 type
   EDbusError =  EXception;

Function CheckDbusError(Error: DBusError; RaiseException:boolean): boolean;

implementation
uses LCLProc;

function CheckDbusError(Error: DBusError; RaiseException: boolean): boolean;
begin
  Result := true;
  if dbus_error_is_set(@Error) <> 0 then
  try
    Result := false;
    if RaiseException then
       Raise EDbusError.Create('DBUS Error:' + Error.message)
    else
       DebugLn('DBUS Error:' + Error.message);
  finally
    dbus_error_free(@Error);
  end;

end;

{ TDBUSThread }

procedure TDBUSThread.Execute;
begin
  while not Terminated do
    begin
       dbus_connection_read_write_dispatch(fBus, 150);
    end;

end;

destructor TDBUSThread.Destroy;
begin
  inherited Destroy;
end;

end.

