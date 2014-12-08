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
  vol : int64;
begin
  mpv_get_property(fhandle^,'volume',MPV_FORMAT_INT64,@vol);
  Result := trunc(vol * ( 255 / MPVMAXVOLUME));
end;

procedure TAudioEngineLibMPV.SetMainVolume(const AValue: integer);
var
  vol : int64;
begin
  vol := trunc(AValue * (MPVMAXVOLUME / 255));
  mpv_set_property(fhandle^,'volume',MPV_FORMAT_INT64,@vol);

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

  mpv_terminate_destroy(fhandle^);
  Free_libmpv;
  fdecoupler.free;
  inherited Destroy;
end;

function TAudioEngineLibMPV.GetState: TEngineState;
begin
  Result := ENGINE_STOP;
end;

procedure TAudioEngineLibMPV.Pause;
begin

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

end;

procedure TAudioEngineLibMPV.SetMuted(const AValue: boolean);
begin
end;

function TAudioEngineLibMPV.GetMuted: boolean;
begin
  Result:=false;
end;

class function TAudioEngineLibMPV.GetEngineName: String;
begin
  Result:='LibMPV';
end;

class function TAudioEngineLibMPV.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
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
             ReceivedCommand(self, ecNext, 0);
          Event := mpv_wait_event(fhandle^, 0);
        end;
    end
 else
    inherited ReceivedCommand(Sender, Command, Param);
end;

function TAudioEngineLibMPV.Playing: boolean;
begin
  Result := true;
end;

function TAudioEngineLibMPV.Running: boolean;
begin
  Result := true  ;
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

end;

procedure TAudioEngineLibMPV.UnPause;
begin
end;

initialization
  RegisterEngineClass(TAudioEngineLibMPV, 5, false, false);
end.