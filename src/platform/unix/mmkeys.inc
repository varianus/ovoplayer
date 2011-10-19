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
uses DBUS, LCLProc, AppConsts;

type

  { TDBUSThread }

  TDBUSThread = class (TThread)
    fBus: PDBusConnection;
  public
     procedure Execute; override;
    destructor Destroy; override;

  end;

  TkeyCaptureDBUS = class (TKeyCapture)
  private
    fBus: PDBusConnection;
    SettingsActive :Boolean;
    DBUSThread : TDBUSThread;
  public
    Procedure BeginGrab; Override;
    Procedure EndGrab; Override;
  end;

{ TkeyCaptureDBUS }
const
 GNOME_SETTINGS_DAEMON_PATH       = '/org/gnome/SettingsDaemon/MediaKeys';
 GNOME_SETTINGS_DAEMON_INTERFACE  = 'org.gnome.SettingsDaemon.MediaKeys';
 GNOME_SETTINGS_DAEMON_NAME       = 'org.gnome.SettingsDaemon';


function filter_func(connection: PDBusConnection; message_: PDBusMessage; user_data: Pointer): DBusHandlerResult; cdecl;
var 
  err: DBusError;
  udi:PCHAR;
  args: DBusMessageIter;
  obj : TkeyCaptureDBUS;
  wp:TEngineCommand;
  Command :string;
begin
     dbus_error_init (@err);
     Result := DBUS_HANDLER_RESULT_NOT_YET_HANDLED;

     if booleaN(dbus_message_is_signal (message_, GNOME_SETTINGS_DAEMON_INTERFACE, 'MediaPlayerKeyPressed'))  then
        begin
           if (dbus_message_iter_init(message_, @args) = 0) then
              exit;

           if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
               exit;

           dbus_message_iter_get_basic(@args, @udi);
           dbus_message_iter_next(@args);
           dbus_message_iter_get_basic(@args, @udi);
           Command := lowercase(trim(udi));

//           then
	      begin
	        obj:= TkeyCaptureDBUS(User_Data);
	        wp := ecInvalid;
	        if Command = 'stop'     then wp := ecStop else
	        if Command = 'play'     then wp := ecPlay else
	        if Command = 'previous' then wp := ecPrevious else
	        if Command = 'next'     then wp := ecNext;
	        if (wp <> ecInvalid) and Assigned(obj.owner.OnMMKey) then
	           obj.Owner.OnMMKey(obj.Owner, wp);
	      end;
  	   Result:= DBUS_HANDLER_RESULT_HANDLED;
  	end
     else
       begin	
         Result := DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
       end;

   dbus_error_free(@err);
end;

{ TDBUSThread }

procedure TDBUSThread.Execute;
var
  msg: PDBusMessage;
  args: DBusMessageIter;
  sigvalue: Pchar;

begin
  while not Terminated do
    begin
       dbus_connection_read_write(fBus, 150);
    end;

end;

destructor TDBUSThread.Destroy;
begin
  inherited Destroy;
end;

procedure TkeyCaptureDBUS.BeginGrab;
var
  err: DBusError;
  msg: PDBusMessage;
  i:integer;
  pl : pchar = appname;
begin
  dbus_error_init(@Err);

  fBus := dbus_bus_get(DBUS_BUS_SESSION, @err);

  dbus_connection_set_exit_on_disconnect(fBus,dbus_bool_t(false));

  dbus_bus_add_match (fBus,'type=''signal'',interface=''org.gnome.SettingsDaemon.MediaKeys'',sender=''org.gnome.SettingsDaemon'',path=''/org/gnome/SettingsDaemon/MediaKeys''',@err);
  dbus_connection_add_filter (fBus, @filter_func, self, nil);

  msg:= dbus_message_new_method_call(GNOME_SETTINGS_DAEMON_NAME,GNOME_SETTINGS_DAEMON_PATH,GNOME_SETTINGS_DAEMON_INTERFACE,'GrabMediaPlayerKeys');
  dbus_message_append_args(msg, DBUS_TYPE_STRING,[@pl]);
  i:=10;
  dbus_message_append_args(msg, DBUS_TYPE_UINT32,[@i]);
  i:=1;
  dbus_connection_send(fBus,msg,@i);
  DBUSThread := TDBUSThread.Create(True);
  DBUSThread.fBus:=fBus;
  DBUSThread.Start;
end;

procedure TkeyCaptureDBUS.EndGrab;
var
  err: DBusError;
  TMPF: DBusHandleMessageFunction;
  msg: PDBusMessage;
  i:integer;
  pl : pchar = 'appname';

begin
  DBUSThread.Terminate;
  dbus_error_init(@Err);
  msg := dbus_message_new_method_call(GNOME_SETTINGS_DAEMON_NAME,GNOME_SETTINGS_DAEMON_PATH,GNOME_SETTINGS_DAEMON_INTERFACE,'ReleaseMediaPlayerKeys');
  dbus_message_append_args(msg, DBUS_TYPE_STRING,[@pl]);
  i:=10;
  dbus_message_append_args(msg, DBUS_TYPE_UINT32,[@i]);
  i:=2;
  dbus_connection_send(fBus,msg,@i);

  dbus_bus_remove_match (fBus,
 			    'type=''signal'',' +
  			    'interface=''org.gnome.SettingsDaemon.MediaKeys'',' +
  			    'sender=''org.gnome.SettingsDaemon'',' +
  			    'path=''/org/gnome/SettingsDaemon/MediaKeys''', @err);
  TMPF:= DBusHandleMessageFunction(@filter_func);
  dbus_connection_Remove_filter (fBus, TMPF, self);
end;

constructor TMultimediaKeys.Create;
begin
  KeyCapture := TkeyCaptureDBUS.Create;
  KeyCapture.Owner:= Self;
  KeyCapture.BeginGrab;
end;

destructor TMultimediaKeys.Destroy;
begin
  KeyCapture.EndGrab;
  KeyCapture.Free;
end;

