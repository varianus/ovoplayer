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
unit audioengine_bass;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, BaseTypes, AudioEngine, lazdynamic_bass, Song, lclproc,
  decoupler;

type

  { TAudioEngineBASS }

  TAudioEngineBASS = class(TAudioEngine)
  private
    Plugins:      array[0..9] of HPLUGIN;
    BassInfoParam: BASS_INFO;
    Channel:      HSTREAM;
    fSavedVolume: integer;
    fMuted: boolean;
    fdecoupler: TDecoupler;
    procedure LoadPlugin(FileName: string; Index: integer; Flags: integer);
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
    Class Function IsAvalaible(ConfigParam: TStrings): boolean; override;
    class Function GetEngineName: String; override;

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
uses dl;
Const
  BASSMAXVOLUME = 1;
{ TAudioEngineBASS }

procedure PlayEndSync(SyncHandle: HSYNC; Channel, Data: DWORD;user:pointer); stdcall;
begin
  TAudioEngineBASS(User).PostCommand(ecNext);
end;

function TAudioEngineBASS.GetMainVolume: integer;
var
  tmpVol: single;
begin
  Result := -1;
  BASS_ChannelGetAttribute(Channel,BASS_ATTRIB_VOL, tmpVol);
  Result := trunc(tmpVol  * (255 / BASSMAXVOLUME));

end;

procedure TAudioEngineBASS.SetMainVolume(const AValue: integer);
begin
//  BASS_SetVolume(AValue * (BASSMAXVOLUME / 255));
    BASS_ChannelSetAttribute(Channel,BASS_ATTRIB_VOL, AValue * (BASSMAXVOLUME / 255));

end;

function TAudioEngineBASS.GetMaxVolume: integer;
begin
  Result:= BASSMAXVOLUME;
end;

function TAudioEngineBASS.GetSongPos: integer;
var
  SongPos:  DWORD;
  MilliSec: integer;
  FloatPos: FLOAT;
begin
  SongPos  := BASS_ChannelGetPosition(Channel, BASS_POS_BYTE);
  FloatPos := BASS_ChannelBytes2Seconds(Channel, SongPos);
  MilliSec := round(1000 * FloatPos);
  if MilliSec < 0 then
    MilliSec := 0;
  Result := MilliSec;
end;

procedure TAudioEngineBASS.SetSongPos(const AValue: integer);
var
  Songpos:Integer;
begin
  Songpos := AValue;
  SongPos := BASS_ChannelSeconds2Bytes(Channel, SongPos / 1000);
  BASS_ChannelSetPosition(Channel, SongPos, BASS_POS_BYTE);
end;

procedure TAudioEngineBASS.Activate;
begin
//
end;

procedure TAudioEngineBASS.LoadPlugin(FileName: string; Index: integer; Flags: integer);

var
  Info: BASS_PLUGININFO;
begin

  Plugins[Index] := BASS_PluginLoad(PChar(FileName), Flags);
  if Plugins[index] = 0  then
     begin
        DebugLn('Bass plugin error: ',dlerror());
        exit;
     end;
  Info := BASS_PluginGetInfo(Plugins[Index])^;

//  for I := 0 to info.formatc - 1 do
//    AddInfo(Info.formats[i].Name, Info.formats[i].Exts);
end;

constructor TAudioEngineBASS.Create;
var
  I: integer;
begin
  inherited Create;

  Load_BASSDLL(BASS_name);
  BASS_Init(1, 44100, 0, 0, nil);

  fMuted := false;

  for I := 0 to 9 do
     Plugins[i] := 0;
  //
  {$IFDEF WINDOWS}
  LoadPlugin('BASSWMA.DLL', 0, 0);
  LoadPlugin('BASSFLAC.DLL', 1, 0);
  LoadPlugin('BASS_AAC.DLL', 2, 0);
  LoadPlugin('BASS_APE.DLL', 3, 0);
  //LoadPlugin('BASSCD.DLL', 4, 0);
  //LoadPlugin('BASSMIDI.DLL', 5, 0);
  //LoadPlugin('BASSWV.DLL', 6, 0);
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  LoadPlugin('libbassflac.so', 1, 0);
  LoadPlugin('libbass_aac.so', 2, 0);
  LoadPlugin('libbass_ape.so', 3, 0);
  {$ENDIF LINUX}

  fdecoupler := TDecoupler.Create;
  fdecoupler.OnCommand := ReceivedCommand;

end;

destructor TAudioEngineBASS.Destroy;
begin

//  for I := 0 to FormatInfos.Count - 1 do
//    Dispose(FormatInfos.Items[i]);

  BASS_PluginFree(0);  // Unplugs all plugins
  BASS_Free;
  fdecoupler.Free;

  inherited Destroy;
end;

function TAudioEngineBASS.GetState: TEngineState;
begin
  Result := ENGINE_ON_LINE;

  case BASS_ChannelIsActive(Channel) of
    BASS_ACTIVE_PLAYING: Result   := ENGINE_PLAY;
    BASS_ACTIVE_PAUSED: Result    := ENGINE_PAUSE;
    BASS_ACTIVE_STOPPED: Result   := ENGINE_STOP;
    end;
end;

procedure TAudioEngineBASS.Pause;
begin
  BASS_ChannelPause(Channel);
end;

Function TAudioEngineBASS.DoPlay(Song: TSong; offset:Integer):boolean;
var
  Flags: DWORD;
begin
  Flags := 0;
  result:=false;
  if Channel <> 0 then
     begin
       BASS_ChannelStop(Channel);
       BASS_StreamFree(Channel);
     end;

  Channel := BASS_StreamCreateFile(False, PAnsiChar(AnsiString(Song.FullName)), 0, 0, Flags);
  if channel = 0 then
     begin
       Exit;
     end;

  BASS_ChannelSetSync(Channel, BASS_SYNC_END or BASS_SYNC_ONETIME,
    0, @PlayEndSync, Self);

  if not Paused then
    begin
      if not BASS_ChannelPlay(Channel, False) then
         exit;
      if offset <> 0 then
        Seek(offset, true);
    end;
  result:= true;
end;

procedure TAudioEngineBASS.SetMuted(const AValue: boolean);
begin
  if AValue = fMuted then
     exit;
  if fMuted then
     begin
        fSavedVolume := Trunc(BASS_GetVolume() * 10000);
        BASS_SetVolume(0);
        fMuted:=true;
     end
 else
     begin
        BASS_SetVolume(fSavedVolume  /10000);
        fMuted:=False;
     end;

end;

function TAudioEngineBASS.GetMuted: boolean;
begin
  Result:=fMuted;
end;

class function TAudioEngineBASS.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  Result:= lazdynamic_bass.isLoaded;
  if Result then
     exit;
  try
    if Load_BASSDLL(BASS_name) then
       begin
         result:= true;
         Unload_BASSDLL;
       end;
  except
    exit;
  end;
end;

class function TAudioEngineBASS.GetEngineName: String;
begin
  Result:='BASS';
end;

procedure TAudioEngineBASS.PostCommand(Command: TEngineCommand; Param: integer);
begin
  fdecoupler.SendCommand(Command, Param);
end;

function TAudioEngineBASS.Playing: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

function TAudioEngineBASS.Running: boolean;
begin
  Result := true  ;
end;

procedure TAudioEngineBASS.Seek(Seconds: integer; SeekAbsolute: boolean);
var
  currpos: integer;
begin
  currpos := GetSongPos;
  if SeekAbsolute then
    SetSongPos(Seconds * 1000)
  else
    SetSongPos(currpos + Seconds * 1000);

end;

procedure TAudioEngineBASS.Stop;
begin
  BASS_ChannelStop(Channel);

end;

procedure TAudioEngineBASS.UnPause;
begin
  BASS_ChannelPlay(Channel, False);

end;

initialization
  RegisterEngineClass(TAudioEngineBASS, 3, false, true);

end.