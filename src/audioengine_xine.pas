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
  Classes, SysUtils, BaseTypes, AudioEngine, Equalizer, Xine, Song, decoupler, LazLoggerBase;

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
    FBandInfo: ARBandinfo;
    FActiveEQ : Boolean;
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
    // equalizer
    function GetBandInfo: ARBandInfo; override;
    function getActiveEQ: boolean; override;
    function GetBandValue(Index: Integer): Double; override;
    procedure SetActiveEQ(AValue: boolean); override;
    procedure SetBandValue(Index: Integer; AValue: Double); override;
    Procedure EQApply; override;

  end;


implementation
uses  LazFileUtils, GeneralFunc;
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
begin
  inherited Create;
  Loadxine();

end;

destructor TAudioEngineXINE.Destroy;
begin

  if Initialized then
  begin
    xine_close(XineStream);
    xine_exit(XineLib);
    fdecoupler.Free;
  end;
  Freexine;

  inherited Destroy;
end;

function TAudioEngineXINE.Initialize: boolean;
var
  error : Integer;

begin
  XineLib := xine_new();
  result := Assigned(XineLib);
  if not result then exit;

  xine_init(XineLib);

  ao_driver := xine_open_audio_driver(XineLib, 'auto', Nil);
  XineStream := xine_stream_new(XineLib, ao_driver, nil);
  error := xine_get_error(XineStream);

  queue := xine_event_new_queue (XineStream);
  xine_event_create_listener_thread (queue, @XineEventCB, self);


  fdecoupler := TDecoupler.Create;
  fdecoupler.OnCommand := @ReceivedCommand;
  Initialized := true;
  InitBands(FBandInfo);

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
end;


function TAudioEngineXINE.GetState: TEngineState;
begin
  Result := ENGINE_ON_LINE;

  case  xine_get_status (XineStream)  of
    XINE_STATUS_PLAY: begin
                       Result   := ENGINE_PLAY;
                       if xine_get_param(XINEStream,XINE_PARAM_SPEED) = XINE_SPEED_PAUSE then
                          Result   := ENGINE_PAUSE;
                      end;

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

function TAudioEngineXINE.DoPlay(Song: TSong; offset: Integer): boolean;
var
  hr: HRESULT;
begin
  result := false;
  // create new media
  if FileExistsUTF8(Song.FullName) then
    begin
       hr:= xine_open(XINEStream, PChar(Song.FullName));
       if hr = 0 then
         exit;
    end;
  // play
  hr:= xine_play(XINEStream,0,0);
  if hr = 0 then
    exit;

  if offset <> 0 then
     Seek(offset, true);

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
    Result := isXineLoaded;
  except
    exit;
  end;
  if Mustfree then
     Freexine;
end;

class function TAudioEngineXINE.GetEngineInfo(IsCurrent:boolean): AREngineParams;
 var
   Major, minor, sub :longint;
   BaseAddr:pointer;
   ModuleName:string;
 begin
   result := inherited GetEngineInfo(IsCurrent);
   if not IsCurrent then
      Loadxine;
   try
     xine_get_version(major, minor, sub);
   Except
     Major:=0; Minor:=0; sub := 0;
   end;

   GetModuleByAddr(@xine_get_version,BaseAddr,ModuleName);

   SetLength(Result,2);
   result[0].Key:= 'Library';
   Result[0].Value:=ModuleName;
   result[0].Kind:=epkString;

   result[1].Key:= 'Version';
   Result[1].Value:=Format('%d.%d.%d',[major, minor, sub]);
   result[1].Kind:=epkString;
   if not IsCurrent then
     Freexine();

end;

class function TAudioEngineXINE.SupportEQ: boolean;
begin
  Result:=true;
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

function TAudioEngineXINE.GetBandInfo: ARBandInfo;
begin
 Result:= fBandInfo;
end;

function TAudioEngineXINE.getActiveEQ: boolean;
begin
  Result := fActiveEQ;
end;

function TAudioEngineXINE.GetBandValue(Index: Integer): Double;
begin
 // eq value range from -100 and +100
 result := xine_get_param(XineStream, XINE_PARAM_EQ_30HZ + Index) * 12 / 100;
 fBandInfo[Index].Value := result;
end;

procedure TAudioEngineXINE.SetActiveEQ(AValue: boolean);
var
  i: integer;
begin
  if AValue and not fActiveEQ then
    for i := 0 to pred(EQUALIZER_BANDS) do
      xine_set_param(XineStream, XINE_PARAM_EQ_30HZ + i, trunc(FBandInfo[i].Value/12*100));

  if not AValue and fActiveEQ then
    for i := 0 to pred(EQUALIZER_BANDS) do
      xine_set_param(XineStream, XINE_PARAM_EQ_30HZ + i, 0);

 fActiveEq:=AValue;

end;

procedure TAudioEngineXINE.SetBandValue(Index: Integer; AValue: Double);
begin
 // eq value range from -100 and +100
 xine_set_param(XineStream, XINE_PARAM_EQ_30HZ + Index, trunc(AValue/12*100));
 fBandInfo[Index].value := Avalue;
end;

procedure TAudioEngineXINE.EQApply;
begin
   // changes are in realtime
end;

initialization
  RegisterEngineClass(TAudioEngineXINE, 3, false, true);


end.
