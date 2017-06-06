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
  RBassBand = record
    Handle: HFX;
  end;

  ARBassBands = array of RBassBand;

  TAudioEngineBASS = class(TAudioEngine)
  private
    Plugins:      array[0..9] of HPLUGIN;
    BassInfoParam: BASS_INFO;
    Channel:      HSTREAM;
    fSavedVolume: integer;
    fMuted: boolean;
    fdecoupler: TDecoupler;
    fActiveEQ : boolean;
    fBassBands: ARBassBands;
    fBandinfo: ARBandinfo;
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
    function Initialize: boolean; override;
  public
    Class Function IsAvalaible(ConfigParam: TStrings): boolean; override;
    class Function GetEngineName: String; override;
    Class Function GetEngineInfo(IsCurrent:boolean): AREngineParams; override;

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
    // equalizer
    function GetBandInfo: ARBandInfo; override;
    function getActiveEQ: boolean; override;
    function GetBandValue(Index: Integer): single; override;
    procedure SetActiveEQ(AValue: boolean); override;
    procedure SetBandValue(Index: Integer; AValue: single); override;
    Procedure EQApply; override;

  end;


implementation
uses
  dynlibs, GeneralFunc;

Const
  BASSMAXVOLUME = 1.0;
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
  if Channel = 0 then exit;
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
  Result:= Trunc(BASSMAXVOLUME * 255);
end;

function TAudioEngineBASS.GetSongPos: integer;
var
  SongPos:  DWORD;
  MilliSec: integer;
  FloatPos: FLOAT;
begin
  result := 0;
  if Channel = 0 then
     exit;
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
  if Channel = 0 then exit;
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
        DebugLn('Bass plugin error: ',GetLoadErrorStr);
        exit;
     end;
  Info := BASS_PluginGetInfo(Plugins[Index])^;

//  for I := 0 to info.formatc - 1 do
//    AddInfo(Info.formats[i].Name, Info.formats[i].Exts);
end;

function TAudioEngineBASS.Initialize:boolean;
  var
  I: integer;
begin
  result := BASS_Init(1, 44100, 0, 0, nil);

  if not Result then exit;

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

  InitBands(fBandinfo);
  SetLength(fBassBands, 10);

  fdecoupler := TDecoupler.Create;
  fdecoupler.OnCommand := ReceivedCommand;

end;

constructor TAudioEngineBASS.Create;
var
  I: integer;
begin
  inherited Create;

  Load_BASSDLL(BASS_name);

end;

destructor TAudioEngineBASS.Destroy;
begin

//  for I := 0 to FormatInfos.Count - 1 do
//    Dispose(FormatInfos.Items[i]);

  if Initialized then begin
     BASS_PluginFree(0);  // Unplugs all plugins
     BASS_Free;
     fdecoupler.Free;
  end;

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

function TAudioEngineBASS.DoPlay(Song: TSong; offset: Integer): boolean;
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

class function TAudioEngineBASS.GetEngineInfo(IsCurrent:boolean): AREngineParams;
var
  isAlreadyActive, isactivated: boolean;
  ver: LongWord;
  ByteArray: array [1..4] of byte absolute ver;
  BaseAddr:pointer;
  ModuleName:string;
begin
  result := inherited GetEngineInfo(IsCurrent);
  if not IsCurrent then
     Load_BASSDLL(lazdynamic_bass.BASS_name);
  try
    ver := BASS_GetVersion();
  Except
    ver:=0;
  end;

  GetModuleByAddr(@BASS_GetVersion,BaseAddr,ModuleName);

  SetLength(Result,2);
  result[0].Key:= 'Library';
  Result[0].Value:=ModuleName;
  result[0].Kind:=epkString;

  result[1].Key:= 'Version';
  Result[1].Value:=Format('%d.%d.%d',[ByteArray[4],ByteArray[3],ByteArray[2]]);
  result[1].Kind:=epkString;
  if not IsCurrent then
     Unload_BASSDLL;

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
  if Channel <> 0 then
  BASS_ChannelStop(Channel);
  BASS_StreamFree(Channel);
  Channel :=0;

end;

procedure TAudioEngineBASS.UnPause;
begin
  BASS_ChannelPlay(Channel, False);

end;

function TAudioEngineBASS.GetBandInfo: ARBandInfo;
begin
  Result := fBandinfo;
end;

function TAudioEngineBASS.getActiveEQ: boolean;
begin
  Result := fActiveEQ;
end;

procedure TAudioEngineBASS.SetActiveEQ(AValue: boolean);
var
  EqP: BASS_DX8_PARAMEQ;
  i: integer;
begin
   if AValue and not fActiveEQ then
       for i := 0 to 9 do
         begin
           fBassBands[i].Handle := BASS_ChannelSetFX(Channel, BASS_FX_DX8_PARAMEQ, 0);
           BASS_FXGetParameters(fBassBands[i].Handle, @EqP);
           EqP.fGain      := fBandinfo[i].Value;
           EqP.fBandwidth := 12;
           EqP.fCenter    := fBandinfo[i].Freq;
           BASS_FXSetParameters(fBassBands[i].Handle, @EqP);
        end;

   if not AValue and fActiveEQ then
     for i := 0 to 9 do
       begin
         BASS_ChannelRemoveFX(Channel, fBassBands[i].Handle);
         fBassBands[i].Handle := 0;
       end;
   fActiveEq:=AValue;
end;

function TAudioEngineBASS.GetBandValue(Index: Integer): single;
begin
  Result := fBandinfo[Index].Value;
end;

procedure TAudioEngineBASS.SetBandValue(Index: Integer; AValue: single);
begin
  fBandinfo[Index].Value:= AValue;
end;

procedure TAudioEngineBASS.EQApply;
var
  EqP: BASS_DX8_PARAMEQ;
  i:   integer;
begin
  for i := 0 to 9 do
    begin
      BASS_FXGetParameters(fBassBands[i].Handle, @EqP);
      EqP.fGain := fBandinfo[i].Value;
      BASS_FXSetParameters(fBassBands[i].Handle, @EqP);
    end;

end;

initialization
  RegisterEngineClass(TAudioEngineBASS, 3, false, true);

end.
