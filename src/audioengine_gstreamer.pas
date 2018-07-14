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
  AudioEngine, Equalizer, gstreamer, Song, uriparser, decoupler, ctypes;

type

  { TAudioEngineGStreamer }

  TAudioEngineGStreamer = class(TAudioEngine)
  private
    playbin : pointer;
    equalizer, convert, sink : pointer;
    bus: pointer;
    fdecoupler: TDecoupler;
    fBandInfo: ARBandinfo;
    fActiveEQ: Boolean;
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
    class function GetEngineInfo(IsCurrent: boolean): AREngineParams; override;
    Class Function SupportEQ: boolean; override;

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
    function GetBandInfo: ARBandInfo; override;
    function getActiveEQ: boolean; override;
    procedure SetActiveEQ(AValue: boolean); override;
    function GetBandValue(Index: Integer): Double; override;
    procedure SetBandValue(Index: Integer; AValue: Double); override;
    Procedure EQApply; override;

  end;


implementation
uses  glib2,LazLoggerBase, GeneralFunc;

var
  DoubleZero: double=0.0;

function bus_watch_callback(bus: pointer; AMessage:Pointer; user_data:pointer): boolean; cdecl;
var
 Player: TAudioEngineGStreamer;
 fgstmessage_1_0 : pgstmessage_1_0 absolute AMessage;
 fgstmessage_0_10 : pgstmessage_0_10 absolute AMessage;
 msgType : guint;
begin

  player:=  TAudioEngineGStreamer(User_data);
  if (user_data <> NIL) then
     begin
//         DebugLn(inttostr(message^._type));
//         DebugLn(inttostr(sizeof(gstMiniObject)));
          if GST_Lib_1_0 then
             msgtype := fgstmessage_1_0^._type
          else
             msgtype := fgstmessage_0_10^._type;

          case msgtype of
          GST_MESSAGE_EOS:
               begin
                  player.PostCommand(ecNext);
               end;
          end;
     end;
  result:=true;

end;

{ TAudioEngineGStreamer }
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
  if GST_Lib_1_0 then
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
var
  err : TGError;
  perr : PGError;
  res : gboolean;
  nilptr :pAnsichar = nil;
  argc : integer=0;
  bin, pad, ghost_pad: pointer;

begin
  Err.code:=0;
  res := gst_init_check(argc, nilptr, perr);


  // added a check for initialized, gst_init_check sometimes retuns FALSE but everithing is ok!!
  if not res and not gst_is_initialized() then
    begin
      DebugLn('GST initialization error',' ',inttostr(err.code), err.message);
      result := res;
      exit;

    end;

  playbin := gst_element_factory_make('playbin2', 'play');
  if (playbin = nil) then
      playbin := gst_element_factory_make('playbin', 'play');

  equalizer:=gst_element_factory_make('equalizer-10bands','eq');

  convert := gst_element_factory_make ('audioconvert', 'convert');
  sink := gst_element_factory_make ('autoaudiosink', 'audio_sink');//get the audio-sink
 //   if (!equalizer || !convert || !sink)//check is all elements were created
 //   {
 //       g_printerr ("Not all elements could be created.\n");
 //       //return -1;
 //   }
   bin := gst_bin_new ('audio_sink_bin');//get new bin
   gst_bin_add_many ((bin), equalizer, [convert, sink, Nil]);//add elements to bin
   if (not (gst_element_link_many (equalizer, convert, [sink, Nil]))) then//link all elements
        writeln('Could not link all elements');

    pad := gst_element_get_static_pad (equalizer, 'sink');//set equalizer to sink
    ghost_pad := gst_ghost_pad_new ('sink', pad);//get a ghost pad to sink
    gst_pad_set_active (ghost_pad, TRUE);
    gst_element_add_pad (bin, ghost_pad);//add ghost pad to the bin
    gst_object_unref (pad);//unreference pad
    g_object_set ( (equalizer), 'band0', DoubleZero, NULL);
    g_object_set ( (equalizer), 'band1', DoubleZero, NULL);
    g_object_set ( (equalizer), 'band2', DoubleZero, NULL);
    g_object_set ( (equalizer), 'band3', DoubleZero, NULL);
    g_object_set ( (equalizer), 'band4', DoubleZero, NULL);
    g_object_set ( (equalizer), 'band5', DoubleZero, NULL);
    g_object_set ( (equalizer), 'band6', DoubleZero, NULL);
    g_object_set ( (equalizer), 'band7', DoubleZero, NULL);
    g_object_set ( (equalizer), 'band8', DoubleZero, NULL);
    g_object_set ( (equalizer), 'band9', DoubleZero, NULL);
    //* Set playbin2's audio sink to be our sink bin */
    g_object_set ((playbin), 'audio-sink', bin, NULL);

  result := Assigned(playbin);
  if not result then exit;
  Initialized:= true;

  fdecoupler := TDecoupler.Create;
  fdecoupler.OnCommand := @ReceivedCommand;

  bus := gst_pipeline_get_bus (playbin);
  gst_bus_add_watch (bus, @bus_watch_callback, self);
  gst_object_unref (bus);

  InitBands(fBandInfo);

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

class function TAudioEngineGStreamer.GetEngineInfo(IsCurrent:boolean): AREngineParams;
 var
   isAlreadyActive, isactivated: boolean;
   Major, minor, micro, nano :guint;
   BaseAddr:pointer;
   ModuleName:string;
 begin
   result := inherited GetEngineInfo(IsCurrent);
   if not IsCurrent then
      libGST_dynamic_dll_init;
   try
     gst_version(major, minor, micro, nano);
   Except
     Major:=0; Minor:=0; micro := 0; nano := 0;
   end;

   GetModuleByAddr(@gst_version,BaseAddr,ModuleName);

   SetLength(Result,2);
   result[0].Key:= 'Library';
   Result[0].Value:=ModuleName;
   result[0].Kind:=epkString;

   result[1].Key:= 'Version';
   Result[1].Value:=Format('%d.%d.%d.%d',[major, minor, micro, nano]);
   result[1].Kind:=epkString;
   if not IsCurrent then
     libGST_dynamic_dll_done();

end;

class function TAudioEngineGStreamer.SupportEQ: boolean;
begin
  Result:=true; //{ TODO 3 : Need to check if equalizer plugin is installed? }
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

function TAudioEngineGStreamer.GetBandInfo: ARBandInfo;
begin
  Result:= fBandInfo;
end;

function TAudioEngineGStreamer.getActiveEQ: boolean;
begin
  Result := fActiveEQ;
end;

procedure TAudioEngineGStreamer.SetActiveEQ(AValue: boolean);
var
  i: integer;
begin
  if AValue and not fActiveEQ then
    for i := 0 to pred(EQUALIZER_BANDS) do
      g_object_set ( (equalizer), pgchar('band'+inttostr(i)), fBandInfo[i].Value , NULL);

  if not AValue and fActiveEQ then
    for i := 0 to pred(EQUALIZER_BANDS) do
      g_object_set ( (equalizer), pgchar('band'+inttostr(i)), DoubleZero , NULL);

 fActiveEq:=AValue;
end;

function TAudioEngineGStreamer.GetBandValue(Index: Integer): Double;
begin
  g_object_get ( (equalizer), pgchar('band'+inttostr(index)), Result, NULL);
  fBandInfo[Index].Value := result;
end;

procedure TAudioEngineGStreamer.SetBandValue(Index: Integer; AValue: Double);
begin
  g_object_set ( (equalizer), pgchar('band'+inttostr(index)), AValue, NULL);
  fBandInfo[Index].value := Avalue;
end;

procedure TAudioEngineGStreamer.EQApply;
begin
  // changes are in realtime
end;



initialization
  RegisterEngineClass(TAudioEngineGStreamer, 5, false, true);

end.
