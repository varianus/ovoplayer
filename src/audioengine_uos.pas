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
  Classes, SysUtils, ExtCtrls, decoupler, Process, Song, AudioEngine,  UOS;

type

  { TAudioEngineUOS }

  TAudioEngineUOS = class(TAudioEngine)
  private
    StreamName: String;
    fdecoupler: TDecoupler;
    fState : TEngineState;
    UOS_Init: TUOS_Init;
    UOS_Player: TUOS_Player;
    fRate: Cardinal;
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
    procedure DoPlay(Song: TSong; offset:Integer); override;
    procedure SetMuted(const AValue: boolean);  override;
    Function GetMuted: boolean; override;
    procedure ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0); override;
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
uses math;
{ TAudioEngineUOS }

function TAudioEngineUOS.GetMainVolume: integer;
begin
  Result := trunc(fVolume * 100);
end;

procedure TAudioEngineUOS.SetMainVolume(const AValue: integer);
begin
 fVolume:= AValue / 100;
 UOS_Player.SetDSPVolumeIn(fStreamIndex,fDSPVol,fVolume,fVolume, true);

end;

function TAudioEngineUOS.GetMaxVolume: integer;
begin
  Result:= 100;
end;

function TAudioEngineUOS.GetSongPos: integer;
begin
  if fState = ENGINE_PLAY then
     Result := Trunc(UOS_Player.InputPositionSeconds(fStreamIndex) * 1000);
end;

procedure TAudioEngineUOS.SetSongPos(const AValue: integer);
begin
  UOS_Player.SeekSeconds(fStreamIndex, AValue /1000);
end;

procedure TAudioEngineUOS.Activate;
begin

end;

constructor TAudioEngineUOS.Create;
begin
  inherited Create;
  UOS_Init := TUOS_Init.Create;
  {$IFDEF LINUX}
  UOS_Init.PA_FileName := 'libportaudio.so.2';
  UOS_Init.SF_FileName := 'libsndfile.so.1';
  IUOS_Initnit.MP_FileName := 'libmpg123.so.0';
  {$ENDIF LINUX}
  {$IFDEF WINDOWS}
  UOS_Init.PA_FileName := 'libportaudio-32.dll';
  UOS_Init.SF_FileName := 'libsndfile-32.dll';
  UOS_Init.MP_FileName := 'libmpg123-32.dll';
  {$ENDIF LINUX}

  {$IFDEF DARWIN}
  UOS_Init.PA_FileName := 'LibPortaudio.dylib';
  UOS_Init.SF_FileName := 'LibSndFile.dylib';
  UOS_Init.MP_FileName := 'LibMpg123.dylib';
  {$ENDIF DARWIN}
  UOS_Init.flag:=LoadAll;
  UOS_Init.LoadLib;
  fVolume:=100;
  fdecoupler := TDecoupler.Create;

  fdecoupler.OnCommand := @ReceivedCommand;
  UOS_Player := nil;

end;

destructor TAudioEngineUOS.Destroy;
begin
  Stop;
  UOS_Init.UnloadLib;
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

procedure TAudioEngineUOS.DoPlay(Song: TSong; offset:Integer);
Var
  savedVolume: Integer;
begin
  // create new media
  if Not FileExists(Song.FullName) then
     exit;

  if Assigned(UOS_Player) then
     begin
       if fState = ENGINE_PLAY then
          Stop;
     end;

  UOS_Player := TUOS_Player.Create(True, self);
  UOS_Player.Priority := tpHighest;
  UOS_Player.AddIntoDevOut(-1, -1, -1, -1, -1);


  fStreamIndex:= UOS_Player.AddFromFile(Song.FullName, -1, -1);
  UOS_Player.EndProc:=@EndSong;

  fDSPVol := UOS_Player.AddDSPVolumeIn(fStreamIndex, 1, 1);

  UOS_Player.Play;
  fState:= ENGINE_PLAY;

  savedVolume := 100;
  if offset <> 0 then
    Seek(offset, true);
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

procedure TAudioEngineUOS.ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0);
begin
  case Command of
    ecNext: if Assigned(OnSongEnd) then
        OnSongEnd(Self);

    ecSeek: Seek(Param, True);

    end;
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
  //  {$IFDEF LINUX}
  //  Result :=   Pa_Load('libportaudio.so.2') and
  //              sf_Load('libsndfile.so.1')  and
  //              Mp_Load('libmpg123.so.0');
  //  {$ENDIF LINUX}
  //  {$IFDEF DARWIN}
  //  Result :=   Pa_Load('LibPortaudio-32.dylib') and
  //              sf_Load('LibSndFile-32.dylib') and
  //              Mp_Load('LibMpg123-32.dylib');
  //  {$ENDIF DARWIN}
  //  {$IFDEF WINDOWS}
  //  Result :=   Pa_Load('LibPortaudio.dll') and
  //              sf_Load('LibSndFile.dll') and
  //              Mp_Load('LibMpg123.dll');
  //  {$ENDIF DARWIN}

  except
  end;

  try
     //Pa_Unload();
     //sf_Unload();
     //Mp_Unload();
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
  fState := ENGINE_STOP;
  UOS_Player.Stop;
  UOS_Player.WaitFor;
  UOS_Player := nil;
end;

procedure TAudioEngineUOS.UnPause;
begin

  if (GetState() = ENGINE_PAUSE) then
    begin
        UOS_Player.Pause;
        fState:=ENGINE_PLAY;
    end;
end;

initialization
  RegisterEngineClass(TAudioEngineUOS, 1, false, true);


end.
