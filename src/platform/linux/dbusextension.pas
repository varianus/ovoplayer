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

implementation

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

