{
This file is part of OvoPlayer
Copyright (C) 2011 Marco Caselli

OvoPlayer is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

}
{$I codegen.inc}
unit DbusExtension;

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
uses LazLoggerBase;

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

