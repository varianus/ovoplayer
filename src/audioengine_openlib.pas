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
{$I codegen.inc}
{$I backend.inc}
unit AudioEngine_OpenLib;

interface

uses
  Classes, SysUtils,  decoupler, Process, Song,
  AudioEngine, basetypes, OL_Classes;
type

  { TAudioEngineOpenLib }
  TDecodingThread = class;

  TAudioEngineOpenLib = class(TAudioEngine)
  private
    StreamName: String;
    fdecoupler: TDecoupler;
    fState : TEngineState;
    DecodingThread : TDecodingThread;
    fMuted: boolean;
    fSavedVolume: integer;
    fVolume: single;
   protected
    Decoder: IOL_Decoder;
    Renderer: IOL_Renderer;
    Filter: IOL_Filter;
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
    StreamFormat: TOLStreamFormat;
    class Function GetEngineName: String; override;
    Class Function IsAvalaible(ConfigParam: TStrings): boolean; override;
    Function Initialize: boolean; override;
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


 { TDecodingThread }

 TDecodingThread = class(TThread)
 Private
   evPause: PRTLEvent;
   fPlayer : TAudioEngineOpenLib;
 protected
   procedure Execute; override;
   procedure DoTerminate; override;

 public
   constructor Create(CreateSuspended: boolean; Player:TAudioEngineOpenLib);
   destructor Destroy; override;
 end;


implementation
uses math,OL_RendererPortAudio;

{ TDecodingThread }

procedure TDecodingThread.Execute;
var
  buffer : TOLBuffer;
  OutFrames :Integer;
  err: hresult;
  wantframes :Integer;
  i: integer;

begin
  fplayer.Renderer.Start;
  repeat
    RTLeventWaitFor(evPause);
    if Terminated then
      Break;

    RTLeventSetEvent(evPause);
    wantframes := Length(buffer) div (SizeOf(TFrame) * fPlayer.StreamFormat.Channels);

    OutFrames := fPlayer.Decoder.GetBuffer(wantframes, @buffer);
    if Assigned(fPlayer.Filter) then
      fPlayer.Filter.Apply(@Buffer);
    fPlayer.Renderer.Write(OutFrames, @buffer);
    if OutFrames < wantframes then
      fPlayer.fState := ENGINE_SONG_END;


  until Terminated or (fPlayer.fState in [ENGINE_STOP,ENGINE_SONG_END]);

  fPlayer.Renderer.Stop ;
  if  fPlayer.fState = ENGINE_SONG_END then
      fPlayer.PostCommand(ecNext);

end;

procedure TDecodingThread.DoTerminate;
begin
  inherited DoTerminate;
  fPlayer.DecodingThread := nil;
end;

constructor TDecodingThread.Create(CreateSuspended: boolean; Player:TAudioEngineOpenLib);
begin
  inherited Create(CreateSuspended);
  fPlayer := Player;
  FreeOnTerminate := True;
  evPause := RTLEventCreate;
end;

destructor TDecodingThread.Destroy;
begin
  inherited Destroy;
  RTLeventdestroy(evPause);
end;

{ TAudioEngineOpenLib }

function TAudioEngineOpenLib.GetMainVolume: integer;
begin
  Result := trunc(fVolume * 100);
end;

procedure TAudioEngineOpenLib.SetMainVolume(const AValue: integer);
begin
 fVolume:= AValue / 100;
  //case CurrentSoundDecoder of
  //  csdMPG123 : mpg123_volume(StreamHandle, fVolume);
  //end;

end;

function TAudioEngineOpenLib.GetMaxVolume: integer;
begin
  Result:= 255;
end;

function TAudioEngineOpenLib.GetSongPos: integer;
begin
 if Assigned(Decoder) then
   Result := Decoder.GetSongPos
 else
   Result:= 0;
end;

procedure TAudioEngineOpenLib.SetSongPos(const AValue: integer);
begin
  Decoder.SetSongPos(AValue);
end;

procedure TAudioEngineOpenLib.Activate;
begin


end;

constructor TAudioEngineOpenLib.Create;
begin
  inherited Create;
  StreamFormat.BitRate:=44100;
  StreamFormat.Channels:=2;
  StreamFormat.Format:=ffInt16;

  fVolume:=100;
  fdecoupler := TDecoupler.Create;

  fdecoupler.OnCommand := @ReceivedCommand;

end;

destructor TAudioEngineOpenLib.Destroy;
begin
  Stop;
  fdecoupler.Free;
  inherited Destroy;
end;

function TAudioEngineOpenLib.GetState: TEngineState;
begin
  Result := fState;
end;

procedure TAudioEngineOpenLib.Pause;
begin
  if (GetState = ENGINE_PLAY) then
    begin
       RTLeventResetEvent(DecodingThread.evPause);
       fState:=ENGINE_PAUSE;
    end
  else
    if (GetState = ENGINE_PAUSE) then
      begin
         RTLeventSetEvent(DecodingThread.evPause);
         fState:=ENGINE_Play;
      end;

end;

function TAudioEngineOpenLib.DoPlay(Song: TSong; offset:Integer):boolean;
begin
  // create new media
  if Not FileExists(Song.FullName) then
     exit;

  if Assigned(DecodingThread) then
     begin
       fState:=ENGINE_STOP;
       DecodingThread.WaitFor;
     end;

  Decoder := IdentifyDecoder(Song.FullName).Create as IOL_Decoder;
  Decoder.Load();
  Decoder.StreamFormat := StreamFormat;
  Decoder.Initialize;
  Decoder.OpenFile(Song.FullName);
  Filter := nil;
  Renderer := TOL_RendererPortaudio.Create;
  Renderer.Load();
  Renderer.StreamFormat := StreamFormat;
  Renderer.Initialize;

  DecodingThread := TDecodingThread.Create(true, self);

//  WriteLn('LOAD Song:',Song.FullName);
  fState:= ENGINE_PLAY;
  RTLeventSetEvent(DecodingThread.evPause);

  FsavedVolume := 100;
  if offset <> 0 then
    Seek(offset, true);
  DecodingThread.Start;


end;

procedure TAudioEngineOpenLib.SetMuted(const AValue: boolean);
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

function TAudioEngineOpenLib.GetMuted: boolean;
begin
 result:=fMuted;
end;

class function TAudioEngineOpenLib.GetEngineName: String;
begin
  Result:='OpenSourceLibs';
end;

class function TAudioEngineOpenLib.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  Result := false;
  try

    Result :=   True;
  except
  end;

  try
  except
  end;
end;

function TAudioEngineOpenLib.Initialize: boolean;
begin
  result := true;
end;

procedure TAudioEngineOpenLib.PostCommand(Command: TEngineCommand; Param: integer);
begin
  fdecoupler.SendCommand(Command, Param);
end;

function TAudioEngineOpenLib.Playing: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

function TAudioEngineOpenLib.Running: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

procedure TAudioEngineOpenLib.Seek(Seconds: integer; SeekAbsolute: boolean);
var
  currpos: integer;
begin
  currpos := GetSongPos;
  if SeekAbsolute then
    SetSongPos(Seconds * 1000)
  else
    SetSongPos(currpos + Seconds * 1000);

end;

procedure TAudioEngineOpenLib.Stop;
begin
  fState:=ENGINE_STOP;
//  DecodingThread.Terminate;
//  DecodingThread.WaitFor;
// FreeAndNil(DecodingThread);
end;

procedure TAudioEngineOpenLib.UnPause;
begin

  if (GetState() = ENGINE_PAUSE) then
    begin
        RTLeventSetEvent(DecodingThread.evPause);
        fState:=ENGINE_PLAY;
    end;
end;

initialization
  RegisterEngineClass(TAudioEngineOpenLib, 999, false, true);


end.
