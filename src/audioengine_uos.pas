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
unit AudioEngine_UOS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, decoupler, Song, BaseTypes, AudioEngine,  UOS;

type

  { TAudioEngineUOS }

  TAudioEngineUOS = class(TAudioEngine)
  private
//    StreamName: String;
    fdecoupler: TDecoupler;
    fState : TEngineState;
    UOS_Player: TUOS_Player;
//    fRate: Cardinal;
    fMuted: boolean;
    fSavedVolume: integer;
    fVolume: single;
    fStreamIndex, fDSPVol:Integer;
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
    procedure EndSong;
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
uses Math, FileUtil;
Const
   UOSMAXVOLUME = 1;
{ TAudioEngineUOS }

function TAudioEngineUOS.GetMainVolume: integer;
begin
  Result := trunc(fVolume * (255 / UOSMAXVOLUME));
end;

procedure TAudioEngineUOS.SetMainVolume(const AValue: integer);
begin

 fVolume := AValue * (UOSMAXVOLUME / 255);
 if (fState in [ENGINE_PLAY,ENGINE_PAUSE]) then
    UOS_Player.SetDSPVolumeIn(fStreamIndex,fDSPVol,fVolume,fVolume, true);

end;

function TAudioEngineUOS.GetMaxVolume: integer;
begin
  Result:= trunc(UOSMAXVOLUME * (255 / UOSMAXVOLUME));
end;

function TAudioEngineUOS.GetSongPos: integer;
begin
  if Assigned(UOS_Player) and (fState in [ENGINE_PLAY,ENGINE_PAUSE]) then
     Result := Trunc(UOS_Player.InputPositionSeconds(fStreamIndex)* 1000);
end;

procedure TAudioEngineUOS.SetSongPos(const AValue: integer);
begin
  UOS_Player.SeekSeconds(fStreamIndex, AValue /1000);
end;

procedure TAudioEngineUOS.Activate;
begin
  fStreamIndex:= -1;
end;

constructor TAudioEngineUOS.Create;
var
  ExceptionMask : TFPUExceptionMask;
begin
  inherited Create;

  {$IFDEF LINUX}
    uos_loadlib('libportaudio.so.2',
                'libsndfile.so.1',
                'libmpg123.so.0',
              nil);
  {$ENDIF LINUX}
  {$IFDEF WINDOWS}
  uos_loadlib('libportaudio-32.dll',
              'libsndfile-32.dll',
              'libmpg123-32.dll',
              nil);
  {$ENDIF LINUX}

  {$IFDEF DARWIN}
  uos_loadlib('LibPortaudio.dylib',
              'LibSndFile.dylib',
              'LibMpg123.dylib',
              nil);
  {$ENDIF DARWIN}
  fVolume:=100;
  ExceptionMask:= GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);

  fdecoupler := TDecoupler.Create;

  fdecoupler.OnCommand := @ReceivedCommand;
  UOS_Player := nil;

end;

destructor TAudioEngineUOS.Destroy;
begin
  Stop;
  UOS_UnloadLib;
  fdecoupler.Free;
  inherited Destroy;
end;

function TAudioEngineUOS.GetState: TEngineState;
begin
  Result := fState;
end;

procedure TAudioEngineUOS.Pause;
begin
  if (GetState = ENGINE_PLAY) then
    begin
       UOS_Player.Pause;
       fState:=ENGINE_PAUSE;
    end
  else
    if (GetState = ENGINE_PAUSE) then
      begin
         UOS_Player.RePlay;
         fState:=ENGINE_Play;
      end;

end;

Function TAudioEngineUOS.DoPlay(Song: TSong; offset:Integer):boolean;
var
  hr: hresult;
begin
  // create new media
  Result := false;

  if Not FileExistsUTF8(Song.FullName) then
     exit;

  if Assigned(UOS_Player) then
     begin
       if fState = ENGINE_PLAY then
       Stop;
     end;

  UOS_Player := TUOS_Player.Create(True);
  UOS_Player.Priority := tpHighest;
  UOS_Player.AddIntoDevOut(-1, -1, -1, -1, -1, -1);


  fStreamIndex:= UOS_Player.AddFromFile(pchar(Song.FullName), -1, -1, -1);
  if fStreamIndex < 0 then
     exit;
  UOS_Player.InputSetPositionEnable(fStreamIndex, 1);
  UOS_Player.EndProc:=@EndSong;

  fDSPVol := UOS_Player.AddDSPVolumeIn(fStreamIndex, 1, 1);
  UOS_Player.SetDSPVolumeIn(fStreamIndex,fDSPVol,fVolume,fVolume, true);

  UOS_Player.Play;

  fState:= ENGINE_PLAY;

  if offset <> 0 then
    Seek(offset, true);

  Result:= True;
end;

procedure TAudioEngineUOS.SetMuted(const AValue: boolean);
begin
  if AValue = fMuted then
     exit;
  if fMuted then
     begin
        fSavedVolume := GetMainVolume;
        setMainVolume(0);
        fMuted:=true;
     end
 else
     begin
        setMainVolume(fSavedVolume);
        fMuted:=False;
     end;

end;

function TAudioEngineUOS.GetMuted: boolean;
begin
 result:=fMuted;
end;

class function TAudioEngineUOS.GetEngineName: String;
begin
  Result:='UnitedOpenlibSound';
end;

procedure TAudioEngineUOS.EndSong;
Var
  oldstate: TEngineState;
begin
    oldstate:= fState;
    fState := ENGINE_SONG_END ;
    if oldstate = ENGINE_PLAY then
   PostCommand(ecNext);
end;

class function TAudioEngineUOS.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  Result := true;
  try
  {$IFDEF LINUX}
    Result := uos_loadlib('libportaudio.so.2',
                          'libsndfile.so.1',
                          'libmpg123.so.0',
                          nil) =0;
  {$ENDIF LINUX}
  {$IFDEF WINDOWS}
  Result := uos_loadlib('libportaudio-32.dll',
              'libsndfile-32.dll',
              'libmpg123-32.dll',
              nil)=0;
  {$ENDIF LINUX}

  {$IFDEF DARWIN}
  Result := uos_loadlib('LibPortaudio.dylib',
              'LibSndFile.dylib',
              'LibMpg123.dylib',
              nil)=0;
  {$ENDIF DARWIN}


  except
  end;

  try
    uos_unloadlib();
  except
  end;
end;

procedure TAudioEngineUOS.PostCommand(Command: TEngineCommand; Param: integer);
begin
  fdecoupler.SendCommand(Command, Param);
end;

function TAudioEngineUOS.Playing: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

function TAudioEngineUOS.Running: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

procedure TAudioEngineUOS.Seek(Seconds: integer; SeekAbsolute: boolean);
var
  currpos: integer;
begin
  currpos := GetSongPos;
  if SeekAbsolute then
    SetSongPos(Seconds * 1000)
  else
    SetSongPos(currpos + Seconds * 1000);

end;

procedure TAudioEngineUOS.Stop;
begin
  fState:= ENGINE_STOP;
  if Assigned(UOS_Player) then
    begin
      UOS_Player.Stop;
      UOS_Player.WaitFor;
      UOS_Player := nil;

    end;

end;

procedure TAudioEngineUOS.UnPause;
begin

  if (GetState() = ENGINE_PAUSE) then
    begin
        UOS_Player.Replay;
        fState:=ENGINE_PLAY;
    end;
end;

initialization
  RegisterEngineClass(TAudioEngineUOS, 1, false, true);


end.