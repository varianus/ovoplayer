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
unit audioengine_libmpv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseTypes, AudioEngine, Song, decoupler, libmpv;

type

  { TAudioEngineLibMPV }

  TAudioEngineLibMPV = class(TAudioEngine)
  private
    fhandle : Pmpv_handle;
    fState : TEngineState;
    function GetBoolProperty(const PropertyName: string): boolean;
    procedure SetBoolProperty(const PropertyName: string; AValue: boolean);
  protected
    fdecoupler :TDecoupler;
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
    procedure ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0); override;
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
uses math, ctypes;

Const
   MPVMAXVOLUME = 100;

{ TAudioEngineLibMPV }

procedure LibMPVEvent(Data: Pointer); cdecl;
var
  player: TAudioEngineLibMPV;
begin
  if (Data = nil) then
    exit;
  player := TAudioEngineLibMPV(Data);
  player.PostCommand(ecCustom, 1);
end;

function TAudioEngineLibMPV.GetMainVolume: integer;
var
  vol : Double;
  res:integer;
begin
  res :=mpv_get_property(fhandle^,'volume',MPV_FORMAT_DOUBLE,@vol);
  Result := trunc(vol * ( 255 / MPVMAXVOLUME));
end;

procedure TAudioEngineLibMPV.SetMainVolume(const AValue: integer);
var
  vol : Double;
    res:integer;
begin
  vol := AValue * (MPVMAXVOLUME / 255);
  res := mpv_set_property(fhandle^,'volume',MPV_FORMAT_DOUBLE,@vol);

end;

function TAudioEngineLibMPV.GetMaxVolume: integer;
begin
  Result:=255;
end;

function TAudioEngineLibMPV.GetSongPos: integer;
var
   pos : double;
begin
  result := mpv_get_property(fhandle^,'time-pos',MPV_FORMAT_DOUBLE,@pos);
  Result := trunc(pos) *1000;

end;

procedure TAudioEngineLibMPV.SetSongPos(const AValue: integer);
var
   pos : double;
begin
  pos := AValue / 1000;
  mpv_set_property(fhandle^,'time-pos',MPV_FORMAT_DOUBLE,@pos);
end;

procedure TAudioEngineLibMPV.Activate;
begin
//
end;

constructor TAudioEngineLibMPV.Create;
var
   res: integer;
   flg:integer=1;
begin
  inherited Create;
  Load_libmpv(libmpv.External_library);
  fhandle := mpv_create();
  res := mpv_set_option(fhandle^,'no-video', MPV_FORMAT_FLAG,@flg);

  mpv_initialize(fhandle^);
  fdecoupler := TDecoupler.Create;
  fdecoupler.OnCommand := @ReceivedCommand;

  mpv_set_wakeup_callback(fhandle^,@LibMPVEvent, self);

end;

destructor TAudioEngineLibMPV.Destroy;
begin

  mpv_set_wakeup_callback(fhandle^,nil, self);
  mpv_terminate_destroy(fhandle^);
  Free_libmpv;
  fdecoupler.free;
  inherited Destroy;
end;

function TAudioEngineLibMPV.GetState: TEngineState;
begin
  Result := fState;
end;

procedure TAudioEngineLibMPV.Pause;
begin
  if (GetState() = ENGINE_PAUSE) then
    begin
        SetBoolProperty('pause', false);
        fState:=ENGINE_PLAY;
    end
  else if  GetState() = ENGINE_PLAY then
    begin
      SetBoolProperty('pause', true);
      fState:=ENGINE_Pause;
    end;


end;

function TAudioEngineLibMPV.DoPlay(Song: TSong; offset: Integer): boolean;
var
  Args: array of pchar;
  res: longint;
  vol : Pchar;
begin
 setlength(args,4);
 args[0] := 'loadfile';
 args[1] := pchar(song.FullName);
 args[2] := 'replace';
 args[3] := nil ;
 res:= mpv_command(fhandle^, ppchar(@args[0])) ;
 setlength(args,2);
 result := res = 0 ;
 if Result then
   fState:= ENGINE_PLAY;

end;

procedure TAudioEngineLibMPV.SetBoolProperty(const PropertyName:string; AValue: boolean);
var
  res: integer;
  p:integer;
begin
  if AValue then
    p:= 1
  else
    p:=0;
 res:=mpv_set_property(fhandle^,pchar(PropertyName),MPV_FORMAT_FLAG,@p);
end;

function TAudioEngineLibMPV.GetBoolProperty(const PropertyName:string):boolean;
var
  res: integer;
  p:integer;
begin
 res:=mpv_get_property(fhandle^,pchar(PropertyName),MPV_FORMAT_FLAG,@p);
 result := Boolean(p);
end;


procedure TAudioEngineLibMPV.SetMuted(const AValue: boolean);
begin
  SetBoolProperty('mute', AValue);
end;

function TAudioEngineLibMPV.GetMuted: boolean;
begin
  result := GetBoolProperty('mute');
end;

class function TAudioEngineLibMPV.GetEngineName: String;
begin
  Result:='LibMPV';
end;

class function TAudioEngineLibMPV.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  Result:= Check_libmpv;
end;

procedure TAudioEngineLibMPV.PostCommand(Command: TEngineCommand; Param: integer);
begin
 fdecoupler.SendCommand(Command, Param);
end;

procedure TAudioEngineLibMPV.ReceivedCommand(Sender: TObject;
  Command: TEngineCommand; Param: integer);
var
  Event: Pmpv_event;
begin
 if (Command = ecCustom) and (param=1) then
    begin
      Event := mpv_wait_event(fhandle^, 0);
      while Event^.event_id <> MPV_EVENT_NONE do
        begin
          if (Event^.event_id =  MPV_EVENT_END_FILE) and
             (Pmpv_event_end_file(Event^.data)^.reason = 0) then
            begin
             fState:= ENGINE_SONG_END;
             ReceivedCommand(self, ecNext, 0);
            end;
          Event := mpv_wait_event(fhandle^, 0);
        end;
    end
 else
    inherited ReceivedCommand(Sender, Command, Param);
end;

function TAudioEngineLibMPV.Playing: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

function TAudioEngineLibMPV.Running: boolean;
begin
 Result := GetState = ENGINE_PLAY;
end;

procedure TAudioEngineLibMPV.Seek(Seconds: integer; SeekAbsolute: boolean);
var
  currpos: integer;
begin
  currpos := GetSongPos;
  if SeekAbsolute then
    SetSongPos(Seconds * 1000)
  else
    SetSongPos(currpos + Seconds * 1000);

end;

procedure TAudioEngineLibMPV.Stop;
var
  Args: array of pchar;
  res: longint;
begin
 setlength(args,2);
 args[0] := 'stop';
 args[3] := nil ;
 res:= mpv_command(fhandle^, ppchar(@args[0])) ;
 fState:= ENGINE_STOP;

end;

procedure TAudioEngineLibMPV.UnPause;
begin

  if (GetState() = ENGINE_PAUSE) then
    begin
      SetBoolProperty('pause',false);
      fState := ENGINE_PLAY;
    end;
end;

initialization
  RegisterEngineClass(TAudioEngineLibMPV, 5, false, true);
end.