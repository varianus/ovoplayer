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
begin
end;

procedure TAudioEngineLibMPV.Activate;
begin
//
end;

constructor TAudioEngineLibMPV.Create;
begin
  inherited Create;
  Load_libmpv(libmpv.External_library);
  fhandle := mpv_create();
  mpv_initialize(fhandle^);
  fdecoupler := TDecoupler.Create;
  fdecoupler.OnCommand := @ReceivedCommand;

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

Function TAudioEngineLibMPV.DoPlay(Song: TSong; offset:Integer):boolean;
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
// res:=mpv_get_property(fhandle^,'playlist/0/filename',MPV_FORMAT_STRING,@vol);

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
  ReceivedCommand(Self, Command, Param);
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
begin
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