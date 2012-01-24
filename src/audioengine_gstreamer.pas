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

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, AudioEngine, gstreamer, Song, uriparser;

type

  { TAudioEngineGStreamer }

  TAudioEngineGStreamer = class(TAudioEngine)
  private
    playbin : pointer;
    bus: pointer;
  protected
    function GetMainVolume: integer; override;
    procedure SetMainVolume(const AValue: integer); override;
    function GetMaxVolume: integer; override;
    function GetSongPos: integer; override;
    procedure SetSongPos(const AValue: integer); override;
    function GetState: TEngineState; override;
    procedure DoPlay(Song: TSong; offset:Integer); override;
    procedure SetMuted(const AValue: boolean);  override;
    Function GetMuted: boolean; override;
    procedure ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0); override;
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

{ TAudioEngineGStreamer }
uses  glib2;


function bus_watch_callback(bus: pointer; message:pointer; user_data:pointer): boolean; cdecl;
begin
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
  format : dword;
  cur: Int64;
begin
  format := GST_FORMAT_TIME;
  cur := 0;
  if gst_element_query_position (G_OBJECT(playbin), format, cur) then
    if (format <> GST_FORMAT_TIME) then
       result:= 0
    else
       result:= cur div 1000000;
end;

procedure TAudioEngineGStreamer.SetSongPos(const AValue: integer);
begin
  if not  gst_element_seek_simple (G_object(PlayBin),
                     GST_FORMAT_TIME,
                     GST_SEEK_FLAG_FLUSH or GST_SEEK_FLAG_KEY_UNIT, Avalue * 1000000) then

end;

procedure TAudioEngineGStreamer.Activate;
begin
  libGST_dynamic_dll_init;

  gst_init_check(0, nil);
  playbin := gst_element_factory_make('playbin2', 'play');
  if (playbin = nil) then
      playbin := gst_element_factory_make('playbin', 'play');

  bus := gst_pipeline_get_bus (playbin);
  gst_bus_add_watch (bus, bus_watch_callback, self);
  gst_object_unref (bus);

end;

constructor TAudioEngineGStreamer.Create;
begin
  inherited Create;

end;

destructor TAudioEngineGStreamer.Destroy;
begin

  inherited Destroy;
end;

function TAudioEngineGStreamer.GetState: TEngineState;
var State: GstElementState;
    pending : GstElementState;
begin
  pending :=0;
  gst_element_get_state(G_OBJECT(playbin), state, pending,  nil);
  case state of
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

procedure TAudioEngineGStreamer.DoPlay(Song: TSong; offset:Integer);
var
 tmp:string;
begin
  gst_element_set_state(playbin, GST_STATE_READY);
  tmp:=  g_filename_to_uri(pchar(Song.FullName),nil,nil);
  g_object_set(G_OBJECT(playbin), 'uri', pchar(tmp), nil);
  gst_element_set_state(playbin, GST_STATE_PLAYING);

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
begin
  Result:=false;
end;

class function TAudioEngineGStreamer.GetEngineName: String;
begin
  Result:='GStreamer';
end;

procedure TAudioEngineGStreamer.ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0);
begin
  case Command of
    ecNext: if Assigned(OnSongEnd) then
        OnSongEnd(Self);
    end;
end;

class function TAudioEngineGStreamer.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  Result:= True;
end;

procedure TAudioEngineGStreamer.PostCommand(Command: TEngineCommand; Param: integer);
begin
  ReceivedCommand(Self, Command, Param);
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
begin
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
