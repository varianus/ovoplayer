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
unit audioengine_vlc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseTypes, AudioEngine, Equalizer, PasLibVlcUnit, Song, decoupler, lclproc;

type

  { TAudioEngineVLC }

  TAudioEngineVLC = class(TAudioEngine)
  private
    p_li: libvlc_instance_t_ptr;
    p_mi: libvlc_media_player_t_ptr;
    p_md: libvlc_media_t_ptr;
    p_mi_ev_mgr: libvlc_event_manager_t_ptr;
    fdecoupler: TDecoupler;
    fEqualizer: libvlc_equalizer_t_ptr;
  protected
    function GetMainVolume: integer; override;
    procedure SetMainVolume(const AValue: integer); override;
    function GetMaxVolume: integer; override;
    function GetSongPos: integer; override;
    procedure SetSongPos(const AValue: integer); override;
    function GetState: TEngineState; override;
    Function DoPlay(Song: TSong; offset:Integer):boolean;  override;
    procedure SetMuted(const AValue: boolean);  override;
    Function GetMuted: boolean; override;
  public
    class Function GetEngineName: String; override;
    Class Function IsAvalaible(ConfigParam: TStrings): boolean; override;
    class function GetEngineInfo(IsCurrent: boolean): AREngineParams;  override;
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
uses math, lazfileutils, generalfunc;

Const
   VLCMAXVOLUME = 100;

{ TAudioEngineVLC }

procedure lib_vlc_player_event_hdlr(p_event: libvlc_event_t_ptr; Data: Pointer); cdecl;
var
  player: TAudioEngineVLC;
begin
  if (Data = nil) then
    exit;
  player := TAudioEngineVLC(Data);

  case p_event^.event_type of
    libvlc_MediaPlayerEndReached: player.PostCommand(ecNext);
    end;

end;

function TAudioEngineVLC.GetMainVolume: integer;
begin
  Result := -1;
  if (p_mi = nil) then
    exit;
  Result := trunc(libvlc_audio_get_volume(p_mi) * ( 255 / VLCMAXVOLUME));

end;

procedure TAudioEngineVLC.SetMainVolume(const AValue: integer);
begin
  if (p_mi = nil) then
    exit;
  libvlc_audio_set_volume(p_mi, trunc(AValue * (VLCMAXVOLUME / 255)));
end;

function TAudioEngineVLC.GetMaxVolume: integer;
begin
  Result:= VLCMAXVOLUME;
end;

function TAudioEngineVLC.GetSongPos: integer;
begin
  Result := -1;
  if (p_mi = nil) then
    exit;
  Result := libvlc_media_player_get_time(p_mi);
end;

procedure TAudioEngineVLC.SetSongPos(const AValue: integer);
begin
  if (p_mi = nil) then
    exit;
  libvlc_media_player_set_time(p_mi, AValue);
end;

procedure TAudioEngineVLC.Activate;
begin

end;

function TAudioEngineVLC.Initialize: boolean;
const ArgsNumber = 6;
const
  args: array[0..ArgsNumber - 1] of PAnsiChar =
    ('-I',// 'dummy',
     '--quiet',
     '--no-xlib',
     '--no-video-title-show',
     '--file-caching=500',
     '--ignore-config');
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
  p_li := libvlc_new(ArgsNumber, @args);
  Result := Assigned(p_li);
  if not result then exit;

  p_mi := libvlc_media_player_new(p_li);
  Result := Assigned(p_mi);
  if not result then exit;

  p_mi_ev_mgr := libvlc_media_player_event_manager(p_mi);
  libvlc_event_attach(p_mi_ev_mgr, libvlc_MediaPlayerEndReached, @lib_vlc_player_event_hdlr, SELF);

  fEqualizer:= nil;

  Initialized:= result;
  fdecoupler := TDecoupler.Create;
  fdecoupler.OnCommand := @ReceivedCommand;

end;

constructor TAudioEngineVLC.Create;
begin
  inherited Create;
  p_li := nil; // library instance
  p_mi_ev_mgr := nil;
  libvlc_dynamic_dll_init();

  if (libvlc_dynamic_dll_error <> '') then
    begin
    raise Exception.Create(libvlc_dynamic_dll_error);
    end;

end;

destructor TAudioEngineVLC.Destroy;
begin

  if (p_mi_ev_mgr <> nil) then
    begin
    libvlc_event_detach(p_mi_ev_mgr, libvlc_MediaPlayerEndReached, @lib_vlc_player_event_hdlr, SELF);
    p_mi_ev_mgr := nil;
    end;


  if (p_mi <> nil) then
    begin
    libvlc_media_player_release(p_mi);
    p_mi := nil;
    end;

  if (p_li <> nil) then
    begin
    libvlc_release(p_li);
    p_li := nil;
    end;

  if Initialized then
     fdecoupler.Free;

  inherited Destroy;
end;

function TAudioEngineVLC.GetState: TEngineState;
begin
  Result := ENGINE_ON_LINE;

  if (p_mi = nil) then
    exit;

  case libvlc_media_player_get_state(p_mi) of
    //    libvlc_NothingSpecial: Result := enplvPlayer_NothingSpecial;
    libvlc_Opening: Result   := ENGINE_PLAY;
    libvlc_Buffering: Result := ENGINE_PLAY;
    libvlc_Playing: Result   := ENGINE_PLAY;
    libvlc_Paused: Result    := ENGINE_PAUSE;
    libvlc_Stopped: Result   := ENGINE_STOP;
    libvlc_Ended: Result     := ENGINE_SONG_END;

    end;
end;

procedure TAudioEngineVLC.Pause;
begin
  if (p_mi = nil) then
    exit;

  if (GetState = ENGINE_PLAY) then
    begin
    libvlc_media_player_pause(p_mi);
    end
  else
    if (GetState = ENGINE_PAUSE) then
      begin
      UnPause;
      end;

end;

function TAudioEngineVLC.DoPlay(Song: TSong; offset: Integer): boolean;
Var
  savedVolume: Integer;
  hr: hResult;
  ExceptionMask : TFPUExceptionMask;

begin
  // create new media
  Result := false;
//  ExceptionMask:= GetExceptionMask;
//  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);

  if FileExistsUTF8(Song.FullName) then
    begin
      p_md := libvlc_media_new_path(p_li, PAnsiChar(System.UTF8Encode(Song.FullName)));
    end
  else
    begin
      p_md := libvlc_media_new_location(p_li, PAnsiChar(System.UTF8Encode(Song.FullName)));
    end;

  try
    if not Assigned(p_md) then
      exit;

    // assign media to player
    libvlc_media_player_set_media(p_mi, p_md);

    // play
    savedVolume := libvlc_audio_get_volume(p_mi);
    libvlc_audio_set_mute(p_mi, ord(true));

    hr := libvlc_media_player_play(p_mi);
    if hr < 0 then
       exit;

    if offset <> 0 then
       Seek(offset, true);

    libvlc_audio_set_mute(p_mi, ord(false));
    libvlc_audio_set_volume(p_mi, savedVolume);
    result:= true;

  finally
    // release media
//    SetExceptionMask(ExceptionMask);
    if (p_md <> nil) then
      begin
        libvlc_media_release(p_md);
        p_md := nil;
      end;
  end;


end;

procedure TAudioEngineVLC.SetMuted(const AValue: boolean);
begin
  libvlc_audio_set_mute(p_mi, ord(AValue));
end;

function TAudioEngineVLC.GetMuted: boolean;
begin
 result:=Boolean(libvlc_audio_get_mute(p_mi));
end;

class function TAudioEngineVLC.GetEngineName: String;
begin
  Result:='VLC';
end;

class function TAudioEngineVLC.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  Result:= Check_libvlc;
end;

class function TAudioEngineVLC.GetEngineInfo(IsCurrent:boolean): AREngineParams;
 var
   isAlreadyActive, isactivated: boolean;
   BaseAddr:pointer;
   ModuleName:string;
 begin
   result := inherited GetEngineInfo(IsCurrent);
   if not IsCurrent then
      libvlc_dynamic_dll_init();

   GetModuleByAddr(libvlc_get_version, BaseAddr, ModuleName);

   SetLength(Result,2);
   result[0].Key:= 'Library';
   Result[0].Value:=ModuleName;
   result[0].Kind:=epkString;

   result[1].Key:= 'Version';
   Result[1].Value:=libvlc_dynamic_dll_version;
   result[1].Kind:=epkString;
   if not IsCurrent then
      libvlc_dynamic_dll_done();

end;


procedure TAudioEngineVLC.PostCommand(Command: TEngineCommand; Param: integer);
begin
  fdecoupler.SendCommand(Command, Param);
end;

function TAudioEngineVLC.Playing: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

function TAudioEngineVLC.Running: boolean;
begin
  Result := p_li <> nil;
  ;
end;

class function TAudioEngineVLC.SupportEQ: boolean;
begin
  Result:= true;
end;

procedure TAudioEngineVLC.Seek(Seconds: integer; SeekAbsolute: boolean);
var
  currpos: integer;
begin
  currpos := GetSongPos;
  if SeekAbsolute then
    SetSongPos(Seconds * 1000)
  else
    SetSongPos(currpos + Seconds * 1000);

end;

procedure TAudioEngineVLC.Stop;
begin
  libvlc_media_player_stop(p_mi);
end;

procedure TAudioEngineVLC.UnPause;
begin
  if (p_mi = nil) then
    exit;

  if (GetState() = ENGINE_PAUSE) then
    begin
      libvlc_media_player_set_pause(p_mi, 0);
    end;
end;

function TAudioEngineVLC.GetBandInfo: ARBandInfo;
var
  i, bCount: unsigned_t;

begin
  bCount := libvlc_audio_equalizer_get_band_count();
  SetLength(Result, bCount);
  for i := 0 to bCount -1 do
    begin
      Result[i].Value:= libvlc_audio_equalizer_get_amp_at_index(fEqualizer, i);
      Result[i].Freq:= libvlc_audio_equalizer_get_band_frequency( i);
    end;


end;

function TAudioEngineVLC.getActiveEQ: boolean;
begin
  Result := assigned(fEqualizer);
end;

procedure TAudioEngineVLC.SetActiveEQ(AValue: boolean);
begin
  if AValue then
   begin
     if not Assigned(fEqualizer) then
       fEqualizer:=libvlc_audio_equalizer_new_from_preset(0);
   end
  else
   begin
     libvlc_audio_equalizer_release(fEqualizer);
     fEqualizer:=nil;
   end;

  libvlc_media_player_set_equalizer(p_mi, fEqualizer);

end;

function TAudioEngineVLC.GetBandValue(Index: Integer): Double;
begin
  if Assigned(fEqualizer) then
    Result := libvlc_audio_equalizer_get_amp_at_index(fEqualizer, Index)
  else
    Result:=0;
end;

procedure TAudioEngineVLC.SetBandValue(Index: Integer; AValue: Double);
begin
  if Assigned(fEqualizer) then
    libvlc_audio_equalizer_set_amp_at_index(fEqualizer, AValue, Index);

end;

procedure TAudioEngineVLC.EQApply;
begin
  libvlc_media_player_set_equalizer(p_mi, fEqualizer);
end;

initialization
  RegisterEngineClass(TAudioEngineVLC, 1, false, false);


end.
