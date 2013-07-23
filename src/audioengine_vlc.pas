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
  Classes, SysUtils, BaseTypes, AudioEngine, PasLibVlcUnit, Song, decoupler, lclproc;

type

  { TAudioEngineVLC }

  TAudioEngineVLC = class(TAudioEngine)
  private
    p_li: libvlc_instance_t_ptr;
    p_mi: libvlc_media_player_t_ptr;
    p_md: libvlc_media_t_ptr;
    p_mi_ev_mgr: libvlc_event_manager_t_ptr;
    fdecoupler: TDecoupler;
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
uses math;

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
  p_mi_ev_mgr := libvlc_media_player_event_manager(p_mi);
  libvlc_event_attach(p_mi_ev_mgr, libvlc_MediaPlayerEndReached, @lib_vlc_player_event_hdlr, SELF);

end;

constructor TAudioEngineVLC.Create;
const ArgsNumber = 6;
const
  args: array[0..ArgsNumber - 1] of PAnsiChar =
    ('-I',// 'dummy',
     '--quiet',
     '--no-xlib',
     '--no-video-title-show',
     '--file-caching=500',
     '--ignore-config');
var
  ExceptionMask : TFPUExceptionMask;
begin
  inherited Create;
  p_li := nil; // library instance
  p_mi_ev_mgr := nil;
  libvlc_dynamic_dll_init();

  if (libvlc_dynamic_dll_error <> '') then
    begin
    raise Exception.Create(libvlc_dynamic_dll_error);
    end;
  ExceptionMask:= GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
  p_li := libvlc_new(ArgsNumber, @args);
  SetExceptionMask(ExceptionMask);

  p_mi := libvlc_media_player_new(p_li);

  fdecoupler := TDecoupler.Create;
  fdecoupler.OnCommand := @ReceivedCommand;

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

procedure TAudioEngineVLC.DoPlay(Song: TSong; offset:Integer);
Var
  savedVolume: Integer;
begin
  // create new media
  if FileExists(Song.FullName) then
    begin
    p_md := libvlc_media_new_path(p_li, PAnsiChar(System.UTF8Encode(Song.FullName)));
    end
  else
    begin
    p_md := libvlc_media_new_location(p_li, PAnsiChar(System.UTF8Encode(Song.FullName)));
    end;
  // assign media to player
  libvlc_media_player_set_media(p_mi, p_md);

  // play
  savedVolume := libvlc_audio_get_volume(p_mi);
  libvlc_audio_set_mute(p_mi, ord(true));

  libvlc_media_player_play(p_mi);
    if offset <> 0 then
     Seek(offset, true);

  libvlc_audio_set_mute(p_mi, ord(false));
  libvlc_audio_set_volume(p_mi, savedVolume);


  // release media
  if (p_md <> nil) then
    begin
    libvlc_media_release(p_md);
    p_md := nil;
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

procedure TAudioEngineVLC.ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0);
begin
  case Command of
    ecNext: if Assigned(OnSongEnd) then
        OnSongEnd(Self);

    ecSeek: Seek(Param, True);

    end;
end;

class function TAudioEngineVLC.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  Result:= Check_libvlc;
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

initialization
  RegisterEngineClass(TAudioEngineVLC, 1, false, false);


end.
