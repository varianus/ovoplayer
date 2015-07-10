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
unit audioengine_gstreamer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseTypes,
  AudioEngine, gstreamer, Song, uriparser, decoupler, ctypes;

type

  { TAudioEngineGStreamer }

  TAudioEngineGStreamer = class(TAudioEngine)
  private
    playbin : pointer;
    bus: pointer;
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
    function Initialize: boolean; override;
    procedure Activate; override;
    procedure Pause; override;
    function Playing: boolean; override;
    function Running: boolean; override;
    procedure Seek(Seconds: integer; SeekAbsolute: boolean); override;
    procedure Stop; override;
    procedure UnPause; override;

  end;


implementation

{ TAudioEngineGStreamer }
uses  glib2;


function bus_watch_callback(bus: pointer; message:pgstmessage; user_data:pointer): boolean; cdecl;
var
 Player: TAudioEngineGStreamer;
begin

  player:=  TAudioEngineGStreamer(User_data);
  if (user_data <> NIL) then
     begin
          case message^._type of
          GST_MESSAGE_EOS:
               begin
                  player.PostCommand(ecNext);
               end;
          end;
     end;
  result:=true;

end;

function TAudioEngineGStreamer.GetMainVolume: integer;
var
  Par: TGValue ;
begin
  par.g_type:= G_TYPE_DOUBLE;
  Par.data[0].v_double :=0;
  g_object_get_property (PGObject(playbin) ,'volume', @Par);
  Result := trunc( Par.data[0].v_double * 100);


end;

procedure TAudioEngineGStreamer.SetMainVolume(const AValue: integer);
var
 Par: TGValue;
begin
  par.g_type:= G_TYPE_DOUBLE;
  Par.data[0].v_double := AValue / 100 ;
  g_object_set_property (PGObject(playbin) ,'volume', @Par);
end;

function TAudioEngineGStreamer.GetMaxVolume: integer;
begin
  Result:=100;
end;

function TAudioEngineGStreamer.GetSongPos: integer;
var
  format : DWord;
  cur: Int64;
begin
  format := GST_FORMAT_TIME;
  cur := 0;
  if GST_New_Lib then
     begin
      if gst_element_query_position (G_OBJECT(playbin), format, cur) then
         result:= cur div 1000000
      else
         result:=0;
     end
  else
   begin
   if gst_element_query_position_OLD (G_OBJECT(playbin), format, cur) then
    if (format <> GST_FORMAT_TIME) then
       result:= 0
    else
       result:= cur div 1000000;
    end;
end;

procedure TAudioEngineGStreamer.SetSongPos(const AValue: integer);
begin
  if not  gst_element_seek_simple (G_object(PlayBin),
                     GST_FORMAT_TIME,
                     GST_SEEK_FLAG_FLUSH or GST_SEEK_FLAG_KEY_UNIT, Avalue * 1000000) then

end;

procedure TAudioEngineGStreamer.Activate;
begin

  bus := gst_pipeline_get_bus (playbin);
  gst_bus_add_watch (bus, @bus_watch_callback, self);
  gst_object_unref (bus);

end;

constructor TAudioEngineGStreamer.Create;
begin
  inherited Create;
  libGST_dynamic_dll_init;

end;

destructor TAudioEngineGStreamer.Destroy;
begin
  libGST_dynamic_dll_done;
  fdecoupler.free;
  inherited Destroy;
end;

function TAudioEngineGStreamer.Initialize: boolean;
begin

  result := gst_init_check(0, nil);
  if not result then exit;

  playbin := gst_element_factory_make('playbin2', 'play');
  if (playbin = nil) then
      playbin := gst_element_factory_make('playbin', 'play');
  result := Assigned(playbin);
  if not result then exit;
  Initialized:= true;

  fdecoupler := TDecoupler.Create;
  fdecoupler.OnCommand := @ReceivedCommand;

end;

function TAudioEngineGStreamer.GetState: TEngineState;
var _State: GstElementState;
    pending : GstElementState;
begin
  pending :=0;
  gst_element_get_state(G_OBJECT(playbin), _state, pending,  nil);
  case _state of
    GST_STATE_NULL      :result := ENGINE_STOP;
    GST_STATE_READY     :result := ENGINE_ON_LINE;
    GST_STATE_PAUSED    :result := ENGINE_PAUSE;
    GST_STATE_PLAYING   :result := ENGINE_PLAY;
  end;

end;

procedure TAudioEngineGStreamer.Pause;
var
     xx :integer;
begin
  xx:= gst_element_set_state((playbin), GST_STATE_PAUSED);
end;

function TAudioEngineGStreamer.DoPlay(Song: TSong; offset: Integer): boolean;
var
 tmp:string;
 hr : integer;
begin
  result:= false;

  hr := gst_element_set_state(playbin, GST_STATE_READY);
  if hr = 0 then
    exit;

  tmp:=  g_filename_to_uri(pchar(Song.FullName),nil,nil);
  g_object_set(G_OBJECT(playbin), 'uri', pchar(tmp), nil);

  hr := gst_element_set_state(playbin, GST_STATE_PLAYING);
  if hr = 0 then
    exit;

  if offset <> 0 then
    PostCommand(ecSeek);

  result := true;

end;

procedure TAudioEngineGStreamer.SetMuted(const AValue: boolean);
var
 Par: TGValue;

begin
  par.g_type:= G_TYPE_BOOLEAN;
  Par.data[0].v_uint := ord(AValue);
  g_object_set_property (PGObject(playbin) ,'mute', @Par);

end;

function TAudioEngineGStreamer.GetMuted: boolean;
var
 Par: TGValue;
begin
  par.g_type:= G_TYPE_BOOLEAN;
  g_object_get_property (PGObject(playbin) ,'mute', @Par);
  Result :=  boolean(Par.data[0].v_uint);
end;

class function TAudioEngineGStreamer.GetEngineName: String;
begin
  Result:='GStreamer';
end;

class function TAudioEngineGStreamer.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  result := false;
  libGST_dynamic_dll_init;
  try
    result := libgst_dynamic_dll_error = '';
    libGST_dynamic_dll_done;
  Except
    exit;
  end;

end;

procedure TAudioEngineGStreamer.PostCommand(Command: TEngineCommand; Param: integer);
begin
  fdecoupler.SendCommand(Command, Param);
end;

function TAudioEngineGStreamer.Playing: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

function TAudioEngineGStreamer.Running: boolean;
begin
  result := true
end;

procedure TAudioEngineGStreamer.Seek(Seconds: integer; SeekAbsolute: boolean);
var
  currpos: integer;
begin
  currpos := GetSongPos;
  if SeekAbsolute then
    SetSongPos(Seconds * 1000)
  else
    SetSongPos(currpos + Seconds * 1000);

end;

procedure TAudioEngineGStreamer.Stop;
begin
  gst_element_set_state(G_OBJECT(playbin), GST_STATE_NULL);
end;

procedure TAudioEngineGStreamer.UnPause;
begin
  gst_element_set_state(G_OBJECT(playbin), GST_STATE_PLAYING);

end;

initialization
  RegisterEngineClass(TAudioEngineGStreamer, 5, false, true);

end.
