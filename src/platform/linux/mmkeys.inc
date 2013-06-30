{%MainUnit ../multimediakeys.pas}
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
uses DBUS, LCLProc, AppConsts, x,  xlib, gdk2;

type
  { TDBUSThread }

  TDBUSThread = class (TThread)
  public
    fBus: PDBusConnection;
     procedure Execute; override;
    destructor Destroy; override;

  end;

  { TkeyCaptureDBUS }

  TkeyCaptureDBUS = class (TKeyCapture)
  private
    fBus: PDBusConnection;
    fGrabbed: boolean;
    SettingsActive :Boolean;
    DBUSThread : TDBUSThread;
  protected
   function GetGrabbed: Boolean; override;
  public
    Procedure BeginGrab; Override;
    Procedure EndGrab; Override;
  end;

  { TkeyCaptureXGrab }

  TkeyCaptureXGrab = class (TKeyCapture)
  private
    SettingsActive :Boolean;
    fGrabbed: boolean;
    procedure GrabKeys(Grab: boolean);

  protected
   function GetGrabbed: Boolean; override;
  public
    display: PDisplay;
    Procedure BeginGrab; Override;
    Procedure EndGrab; Override;
  end;


const
 XF86XK_AudioPlay  = $1008FF14;   // Start playing of audio >
 XF86XK_AudioStop  = $1008FF15;   // Stop playing audio
 XF86XK_AudioPrev  = $1008FF16;   // Previous track
 XF86XK_AudioNext  = $1008FF17;   // Next track
 XF86XK_AudioPause = $1008FF31;

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

function TkeyCaptureDBUS.GetGrabbed: Boolean;
begin
  Result:=fGrabbed;
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
  fGrabbed:=Boolean(dbus_connection_send(fBus,msg,@i));
  if fGrabbed then
     begin
       DBUSThread := TDBUSThread.Create(True);
       DBUSThread.fBus:=fBus;
       DBUSThread.Start;

     end;
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

{ TkeyCaptureXGrab }
Function grab_mmkey (dsply:Pdisplay; key_code : Integer; root: TWindow):boolean;
begin
    gdk_error_trap_push;

    XGrabKey (dsply, key_code,
              0,
               (root), 1,
              GrabModeAsync, GrabModeAsync);
    XGrabKey (dsply, key_code,
              Mod2Mask,
              (root), 1,
              GrabModeAsync, GrabModeAsync);
    XGrabKey (dsply, key_code,
              Mod5Mask,
              (root), 1,
              GrabModeAsync, GrabModeAsync);
    XGrabKey (dsply, key_code,
              LockMask,
              (root), 1,
              GrabModeAsync, GrabModeAsync);
    XGrabKey (dsply, key_code,
              Mod2Mask or Mod5Mask,
              (root), 1,
              GrabModeAsync, GrabModeAsync);
    XGrabKey (dsply, key_code,
              Mod2Mask or LockMask,
              (root), 1,
              GrabModeAsync, GrabModeAsync);
    XGrabKey (dsply, key_code,
              Mod5Mask or  LockMask,
              (root), 1,
              GrabModeAsync, GrabModeAsync);
    XGrabKey (dsply, key_code,
              Mod2Mask or Mod5Mask or LockMask,
              (root), 1,
              GrabModeAsync, GrabModeAsync);

    gdk_flush;
    Result := (gdk_error_trap_pop) = 0;

end;

function Ungrab_mmkey (dsplay:Pdisplay; key_code : Integer; root: TWindow):boolean;
begin
        gdk_error_trap_push;

        XUngrabKey (dsplay, key_code, 0,  (root));
        XUngrabKey (dsplay, key_code, Mod2Mask,  (root));
        XUngrabKey (dsplay, key_code, Mod5Mask,  (root));
        XUngrabKey (dsplay, key_code, LockMask,  (root));
        XUngrabKey (dsplay, key_code, Mod2Mask or Mod5Mask,  (root));
        XUngrabKey (dsplay, key_code, Mod2Mask or LockMask,  (root));
        XUngrabKey (dsplay, key_code, Mod5Mask or LockMask,  (root));
        XUngrabKey (dsplay, key_code, Mod2Mask or Mod5Mask or LockMask,  (root));

        gdk_flush;
        Result := (gdk_error_trap_pop) = 0;

end;

function filter_mmkeys(xevent:PGdkXEvent; event:PGdkEvent; data:pointer):TGdkFilterReturn;cdecl;
var
     xev : TXAnyEvent;
     key : TXKeyEvent;
     obj : TkeyCaptureXGrab;
     wp:TEngineCommand;
begin
        xev := (PXAnyEvent(xevent)^);
        if (xev._type <> KeyPress) then
           begin
              result := GDK_FILTER_CONTINUE;
              exit;
           end;

        obj:= TkeyCaptureXGrab(Data);
        key := (PXKeyEvent(xevent)^);
        wp:= ecInvalid;

        if (XKeysymToKeycode (obj.DISPLAY, XF86XK_AudioPlay) = key.keycode) then
           wp:= ecPlay
        else if (XKeysymToKeycode (obj.DISPLAY, XF86XK_AudioPause) = key.keycode) then
           wp:= ecPause
        else if (XKeysymToKeycode (obj.DISPLAY, XF86XK_AudioStop) = key.keycode) then
           wp:= ecStop
        else if (XKeysymToKeycode (obj.DISPLAY, XF86XK_AudioPrev) = key.keycode) then
           wp:= ecPrevious
        else if (XKeysymToKeycode (obj.DISPLAY, XF86XK_AudioNext) = key.keycode) then
           wp:= ecNext;

        if wp = ecInvalid then
           begin
              result := GDK_FILTER_CONTINUE;
              exit;
           end;

        Result := GDK_FILTER_REMOVE;

        if (wp <> ecInvalid) and Assigned(obj.owner.OnMMKey) then
           obj.Owner.OnMMKey(obj.Owner, wp);


end;

procedure TkeyCaptureXGrab.GrabKeys(Grab:boolean);
var
  keycodes : array [0..4] of integer;
  screen : PScreen;
  root :TWindow;
  i, j :Integer;

begin


    keycodes[0] := XKeysymToKeycode (DISPLAY, XF86XK_AudioPlay);
    keycodes[1] := XKeysymToKeycode (DISPLAY, XF86XK_AudioStop);
    keycodes[2] := XKeysymToKeycode (DISPLAY, XF86XK_AudioPrev);
    keycodes[3] := XKeysymToKeycode (DISPLAY, XF86XK_AudioNext);
    keycodes[4] := XKeysymToKeycode (DISPLAY, XF86XK_AudioPause);

    for i := 0 to ScreenCount(display) -1  do
        begin
          screen := ScreenOfDisplay (display, i);

          if (screen <> Nil) then
             root := RootWindowOfScreen (screen);

          for j := 0 to Length(keycodes) -1 do
            if (keycodes[j] <> 0) then
               begin
                 if (grab) then
                    fGrabbed := grab_mmkey (display, keycodes[j], root)
                 else
                    ungrab_mmkey (display, keycodes[j], root);
               end;

            if (grab) and fGrabbed  then
                gdk_window_add_filter (nil, @filter_mmkeys, self)
            else
                gdk_window_remove_filter (nil, @filter_mmkeys, self);
        end;

end;

function TkeyCaptureXGrab.GetGrabbed: Boolean;
begin
  Result:=fGrabbed;
end;

procedure TkeyCaptureXGrab.BeginGrab;
begin
 display := XOpenDisplay(gdk_get_display);
 GrabKeys(true);

end;

procedure TkeyCaptureXGrab.EndGrab;
begin
  GrabKeys(False);
  XCloseDisplay(display);
end;

constructor TMultimediaKeys.Create(Mode:Integer);
begin
  fMode := Mode;
  if fMode = 0 then
    KeyCapture := TkeyCaptureDBUS.Create
  else
    KeyCapture := TkeyCaptureXGrab.Create;

  KeyCapture.Owner:= Self;
  KeyCapture.BeginGrab;
end;

destructor TMultimediaKeys.Destroy;
begin
 KeyCapture.EndGrab;
 KeyCapture.Free;
end;


