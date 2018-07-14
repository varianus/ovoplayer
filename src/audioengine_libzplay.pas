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
{$I backend.inc}
unit audioengine_libzplay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseTypes, AudioEngine, Equalizer, Song, decoupler, libzplay, LazLoggerBase;

type
  { TAudioEnginelibzplay }
  TAudioEnginelibzplay = class(TAudioEngine)
  private
    fhandle : ZPLAY_HANDLE;
    fState : TEngineState;
    fBandInfo: ARBandinfo;
    fActiveEq: Boolean;
    fMuted: boolean;
    fSavedVolume: integer;
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
    Class Function GetEngineInfo(IsCurrent:boolean): AREngineParams; override;
    Class Function SupportEQ: boolean; override;
    procedure PostCommand(Command: TEngineCommand; Param: integer = 0); override;
    procedure ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0); override;
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
    procedure SetActiveEQ(AValue: boolean); override;
    function GetBandValue(Index: Integer): Double; override;
    procedure SetBandValue(Index: Integer; AValue: Double); override;
    Procedure EQApply; override;

  end;


implementation
uses generalfunc;

Const
   MPVMAXVOLUME = 100;

{ TAudioEnginelibzplay }

function libzplayEvent(objptr: Pointer; user_data: pointer; msg: cardinal; param1: Cardinal; param2: Cardinal): Integer; stdcall;
var
  player: TAudioEnginelibzplay;
begin
  if (user_data = nil) then
    exit;
  player := TAudioEnginelibzplay(user_data);
//  if msg <> MsgWaveBuffer then
//    WRITELN('msg:'+inttostr(msg));
  player.PostCommand(ecCustom, ord(msg));
end;

function TAudioEnginelibzplay.GetMainVolume: integer;
var
  VolL, VolR : integer;
  res:integer;
begin
  zplay_GetPlayerVolume(fhandle,VolL, VolR);
  Result := trunc(((VolL + VolR) div 2) * ( 255 / MPVMAXVOLUME));
end;

procedure TAudioEnginelibzplay.SetMainVolume(const AValue: integer);
var
  vol : integer;
  res:integer;
begin
  vol := trunc(AValue * (MPVMAXVOLUME / 255));
  res := zplay_SetPlayerVolume(fhandle,vol, vol);

end;

function TAudioEnginelibzplay.GetMaxVolume: integer;
begin
  Result:=255;
end;

function TAudioEnginelibzplay.GetSongPos: integer;
var
   pos : TStreamTime;
begin
  zplay_GetPosition(fhandle, pos);
  Result := pos.ms;

end;

procedure TAudioEnginelibzplay.SetSongPos(const AValue: integer);
var
  pos : TStreamTime;
begin
  pos.ms := AValue ;
  zplay_Seek(fhandle,tfMillisecond, pos, smFromBeginning);
end;

procedure TAudioEnginelibzplay.Activate;
begin
//
end;

constructor TAudioEnginelibzplay.Create;
begin
  inherited Create;
  Load_libzplay(libzplay.External_library);
  fActiveEQ:= false;

end;

destructor TAudioEnginelibzplay.Destroy;
begin

  zplay_DestroyZPlay(fhandle);
  Free_libzplay;
  inherited Destroy;
end;

function TAudioEnginelibzplay.Initialize: boolean;
var
   res: integer;
begin
  fhandle := zplay_CreateZPlay();

  result := assigned(fhandle);
  if not result then exit;

  fdecoupler := TDecoupler.Create;
  fdecoupler.OnCommand := @ReceivedCommand;
  res:=zplay_SetCallbackFunc(fhandle, @libzplayEvent, MsgAll,  self);
  Initialized := true;

  InitBands(fBandInfo);

end;

function TAudioEnginelibzplay.GetState: TEngineState;
begin
  Result := fState;
end;

procedure TAudioEnginelibzplay.Pause;
begin
  if (GetState() = ENGINE_PAUSE) then
    begin
        zplay_Play(fhandle);
        fState:=ENGINE_PLAY;
    end
  else if  GetState() = ENGINE_PLAY then
    begin
      zplay_Pause(fhandle);
      fState:=ENGINE_Pause;
    end;


end;

function TAudioEnginelibzplay.DoPlay(Song: TSong; offset: Integer): boolean;
var
  res: longint;
//  vol : Pchar;
begin
//  Stop;
  zplay_Close(fhandle);
//  res := zplay_AddFile(fhandle,PAnsiChar(song.FullName), sfAutodetect);
//  res := zplay_OpenStream(fhandle,0,0,@res,1,sfAutodetect);
  res := zplay_OpenFile(fhandle,PAnsiChar(song.FullName), sfAutodetect);
  result := res = 1 ;
  if Result then
    begin
      fState:= ENGINE_PLAY;
      if offset <> 0 then
        Seek(offset, true);
      res := zplay_Play(fhandle);
    end;
// else
//   vol := zplay_GetError(fhandle);

 result := res = 1 ;

end;


procedure TAudioEnginelibzplay.SetMuted(const AValue: boolean);
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

function TAudioEnginelibzplay.GetMuted: boolean;
begin
  Result:=fMuted;
end;

class function TAudioEnginelibzplay.GetEngineName: String;
begin
  Result:='libZPlay';
end;

class function TAudioEnginelibzplay.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  Result:= Check_libzplay;
end;

class function TAudioEnginelibzplay.GetEngineInfo(IsCurrent:boolean): AREngineParams;
 var
   ver: LongWord;
   tmphandle: ZPLAY_HANDLE;
   BaseAddr:pointer;
   ModuleName:string;
 begin
   result := inherited GetEngineInfo(IsCurrent);
   if not IsCurrent then
     begin
       Load_libzplay(libzplay.External_library);

     end;
   try
     tmphandle:=zplay_CreateZPlay();
     ver:=zplay_GetVersion(tmphandle);
   Except
     ver:=0;
   end;

   GetModuleByAddr(zplay_GetVersion, BaseAddr, ModuleName);

   SetLength(Result,2);
   result[0].Key:= 'Library';
   Result[0].Value:=ModuleName;
   result[0].Kind:=epkString;

   result[1].Key:= 'Version';
   Result[1].Value:=Format('%d.%2.2d',[ver mod 100, ver div 100]);
   result[1].Kind:=epkString;
   zplay_DestroyZPlay(tmphandle);
   if not IsCurrent then
     begin
       Free_libzplay();
     end;

end;

class function TAudioEnginelibzplay.SupportEQ: boolean;
begin
  Result := true;
end;

procedure TAudioEnginelibzplay.PostCommand(Command: TEngineCommand; Param: integer);
begin
 fdecoupler.SendCommand(Command, Param);
end;

procedure TAudioEnginelibzplay.ReceivedCommand(Sender: TObject;
  Command: TEngineCommand; Param: integer);
begin
 if (Command = ecCustom) and
    ((Param and MsgStopAsync) = MsgStopAsync) then
    begin
      if fState = ENGINE_PLAY then
         begin
           fState:= ENGINE_SONG_END;
           ReceivedCommand(self, ecNext, 0);
         end;
    end
 else
    inherited ReceivedCommand(Sender, Command, Param);
end;

function TAudioEnginelibzplay.Playing: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

function TAudioEnginelibzplay.Running: boolean;
begin
 Result := GetState = ENGINE_PLAY;
end;

procedure TAudioEnginelibzplay.Seek(Seconds: integer; SeekAbsolute: boolean);
var
  currpos: integer;
begin
  currpos := GetSongPos;
  if SeekAbsolute then
    SetSongPos(Seconds * 1000)
  else
    SetSongPos(currpos + Seconds * 1000);

end;

procedure TAudioEnginelibzplay.Stop;
var
  res: longint;
begin
 res:= zplay_Stop(fhandle) ;
 fState:= ENGINE_STOP;

end;

procedure TAudioEnginelibzplay.UnPause;
begin

  if (GetState() = ENGINE_PAUSE) then
    begin
      zplay_Play(fhandle);
      fState := ENGINE_PLAY;
    end;
end;

function TAudioEnginelibzplay.GetBandInfo: ARBandInfo;
begin
  Result := fBandinfo;
end;

function TAudioEnginelibzplay.getActiveEQ: boolean;
begin
  Result := fActiveEq;
end;

procedure TAudioEnginelibzplay.SetActiveEQ(AValue: boolean);
begin
  if AValue and not fActiveEQ then
    zplay_EnableEqualizer(fhandle, 1);

  if not AValue and fActiveEQ then
    zplay_EnableEqualizer(fhandle, 0);

  fActiveEq:=AValue;
end;

function TAudioEnginelibzplay.GetBandValue(Index: Integer): Double;
var
  intValue: integer;
begin
  IntValue := zplay_GetEqualizerBandGain(fhandle, Index);
  Result := (IntValue) * 12 / 20000;
  fBandInfo[Index].Value :=  Result;
end;

procedure TAudioEnginelibzplay.SetBandValue(Index: Integer; AValue: Double);
var
  intValue: integer;
begin
  fBandInfo[Index].Value:= AValue;
  intValue:= trunc((Avalue * 20000 /12 ));
  zplay_setEqualizerBandGain(fhandle, Index, intValue);

end;

procedure TAudioEnginelibzplay.EQApply;
begin
  ///
end;


initialization
  RegisterEngineClass(TAudioEnginelibzplay, 5, false, true);
end.
