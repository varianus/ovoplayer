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
{$I ovoplayer.inc}
unit audioengine_Xine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseTypes, AudioEngine, Xine, Song, decoupler, lclproc;

  { TAudioEngineXINE }
const
  audNames : array[0..5] of PChar = ( 'null', 'alsa', 'oss', 'arts', 'esd', 'auto' );

type
  TAudioEngineXINE = class(TAudioEngine)
  private
    XineLib : Pxine_t;
    XineStream : Pxine_stream_t;
    Ao_Driver: Pxine_audio_port_t;
    queue: Pxine_event_queue_t;
    fdecoupler: TDecoupler;
  protected
    function GetMainVolume: integer; override;
    procedure SetMainVolume(const AValue: integer); override;
    function GetMaxVolume: integer; override;
    function GetSongPos: integer; override;
    procedure SetSongPos(const AValue: integer); override;
    function GetState: TEngineState; override;
    Function DoPlay(Song: TSong; offset:Integer):boolean; override;
    procedure SetMuted(const AValue: boolean);  override;
    Function GetMuted: boolean; override;
  public
    class Function GetEngineName: String; override;
    Class Function IsAvalaible(ConfigParam: TStrings): boolean; override;

    procedure PostCommand(Command: TEngineCommand; Param: integer = 0); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure Activate; override;

    procedure Pause; override;
    function Playing: boolean; override;
    function Running: boolean; override;
    procedure Seek(Seconds: integer; SeekAbsolute: boolean); override;
    procedure Stop; override;
    procedure UnPause; override;

  end;


implementation

{ TAudioEngineXINE }

procedure XineEventCB(user_data:Pointer; event: Pxine_event_t); cdecl;
var
  player: TAudioEngineXINE;
begin
  if (user_data = nil) then
    exit;

  player := TAudioEngineXINE(user_data);

     if (user_data <> NIL) then
     begin
          case event^.typ of
          XINE_EVENT_UI_PLAYBACK_FINISHED:
               begin
                  player.PostCommand(ecNext);
               end;
          end;
     end;
end;

function PPCharToStringList(items: PPChar):TStringList;
type
  PCharArray = array of PChar;

var
     i: Integer;
     list: TStringList;
begin
     i := 0;
     list := TStringList.Create;
     while (items <> nil) and (PCharArray(items)[i] <> nil) do
     begin
          list.Add(PCharArray(items)[i]);
          i := i + 1;
     end;
     result := list;
end;


constructor TAudioEngineXINE.Create;
var
//  args: array[0..0] of PAnsiChar;
//  vo_driver :xine_video_port_s;
//  ss: Pxine_stream_t;
  error : Integer;
//  lst:PPChar;
//  st:TstringList;
begin
  inherited Create;
  Loadxine();

  XineLib := xine_new();
  xine_init(XineLib);

//  lst:= xine_list_audio_output_plugins(XineLib);
//  st:= PPCharToStringList(lst);
  ao_driver := xine_open_audio_driver(XineLib, 'auto', Nil);
  XineStream := xine_stream_new(XineLib, ao_driver, nil);
  error := xine_get_error(XineStream);

  fdecoupler := TDecoupler.Create;
  fdecoupler.OnCommand := @ReceivedCommand;

end;

destructor TAudioEngineXINE.Destroy;
begin

  //if (p_mi_ev_mgr <> nil) then
  //  begin
  //  libvlc_event_detach(p_mi_ev_mgr, libvlc_MediaPlayerEndReached, @lib_vlc_player_event_hdlr, SELF);
  //  p_mi_ev_mgr := nil;
  //  end;

  xine_close(XineStream);
  xine_exit(XineLib);
  fdecoupler.Free;
  inherited Destroy;
end;

function TAudioEngineXINE.GetMainVolume: integer;
begin
  Result := -1;
  Result := xine_get_param(XineStream, XINE_PARAM_AUDIO_VOLUME);

end;

procedure TAudioEngineXINE.SetMainVolume(const AValue: integer);
begin
  if (AValue < 0) then
    exit;
  if (AValue > 200) then
    exit;
   xine_set_param(XineStream, XINE_PARAM_AUDIO_VOLUME, AValue);
end;

function TAudioEngineXINE.GetMaxVolume: integer;
begin
  Result:=255;
end;

function TAudioEngineXINE.GetSongPos: integer;
var dummy1, dummy2:Longint;
begin
  Result := -1;
  xine_get_pos_length(XineStream, Dummy1, Result, Dummy2);

end;

procedure TAudioEngineXINE.SetSongPos(const AValue: integer);
begin
 xine_play(XineStream, 0, AValue );
end;

procedure TAudioEngineXINE.Activate;
begin
  queue := xine_event_new_queue (XineStream);
  xine_event_create_listener_thread (queue, @XineEventCB, self);

end;


function TAudioEngineXINE.GetState: TEngineState;
begin
  Result := ENGINE_ON_LINE;

  case  xine_get_status (XineStream)  of
    XINE_STATUS_PLAY: Result   := ENGINE_PLAY;
    XINE_STATUS_STOP: Result   := ENGINE_STOP;
    XINE_STATUS_IDLE: Result   := ENGINE_SONG_END;

    end;
end;

procedure TAudioEngineXINE.Pause;
begin
  if (GetState = ENGINE_PLAY) then
    begin
      xine_set_param(XINEStream,XINE_PARAM_SPEED,XINE_SPEED_PAUSE);
    end
  else
    if (GetState = ENGINE_PAUSE) then
      begin
        UnPause;
      end;

end;

Function TAudioEngineXINE.DoPlay(Song: TSong; offset:Integer):boolean;
var
  hr: HRESULT;
begin
  result := false;
  // create new media
  if FileExists(Song.FullName) then
    begin
       hr:= xine_open(XINEStream, PChar(Song.FullName));
       if hr = 0 then
         exit;
    end;
  // play
  hr:= xine_play(XINEStream,0,0);
  if hr = 0 then
    exit;

  Result:= true;

end;

procedure TAudioEngineXINE.SetMuted(const AValue: boolean);
begin
  xine_set_param(XINEStream,XINE_PARAM_AUDIO_MUTE, ord(Avalue));

end;

function TAudioEngineXINE.GetMuted: boolean;
begin
  Result:=Boolean(xine_get_param(XINEStream,XINE_PARAM_AUDIO_MUTE));
end;

class function TAudioEngineXINE.GetEngineName: String;
begin
  Result:='Xine';
end;

class function TAudioEngineXINE.IsAvalaible(ConfigParam: TStrings): boolean;
var
  Mustfree: Boolean;
begin
  Mustfree:=not isXineLoaded;
  Result := False;
  try
    Loadxine;
    result:=true;
  except
    exit;
  end;
  if Mustfree then
     Freexine;
end;

procedure TAudioEngineXINE.PostCommand(Command: TEngineCommand; Param: integer);
begin
  fdecoupler.SendCommand(Command, Param);
end;

function TAudioEngineXINE.Playing: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

function TAudioEngineXINE.Running: boolean;
begin
  Result := True;
end;

procedure TAudioEngineXINE.Seek(Seconds: integer; SeekAbsolute: boolean);
var
  currpos: integer;
begin
  currpos := GetSongPos;
  if SeekAbsolute then
    SetSongPos(Seconds * 1000)
  else
    SetSongPos(currpos + Seconds * 1000);

end;

procedure TAudioEngineXINE.Stop;
begin
   xine_stop(XINEStream);
end;

procedure TAudioEngineXINE.UnPause;
begin
  if (GetState() = ENGINE_PAUSE) then
    begin
    xine_set_param(XINEStream,XINE_PARAM_SPEED,XINE_SPEED_NORMAL);
    end;
end;

initialization
  RegisterEngineClass(TAudioEngineXINE, 3, false, true);


end.
