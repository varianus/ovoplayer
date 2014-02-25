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
unit AudioEngine_OpenLib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, decoupler, Process, Song,
  AudioEngine, basetypes,
  UOS_libsndfile, UOS_mpg123, UOS_portaudio;

type

  { TAudioEngineOpenLib }

  TCurrentSoundDecoder = (csdMPG123, csdSndFile);

  TDecodingThread = class;

  TAudioEngineOpenLib = class(TAudioEngine)
  private
    StreamName: String;
    fdecoupler: TDecoupler;
    fState : TEngineState;
    fdevice : PaDeviceIndex;
    CurrentSoundDecoder : TCurrentSoundDecoder;
    sfInfo : TSF_INFO;
    mpInfo : Tmpg123_frameinfo;
    pa_OutInfo : PaStreamParameters;
    StreamHandle: pointer;
    Stream_out: PaStream;
    DecodingThread : TDecodingThread;
    fRate: Cardinal;
    fMuted: boolean;
    fSavedVolume: integer;
    fVolume: single;
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


 { TDecodingThread }

 TDecodingThread = class(TThread)
 Private
   evPause: PRTLEvent;
   fOwner : TAudioEngineOpenLib;
 protected
   procedure Execute; override;

 public
   constructor Create(CreateSuspended: boolean; OWner:TAudioEngineOpenLib);
   destructor Destroy; override;
 end;


implementation
uses math;

{ TDecodingThread }

procedure TDecodingThread.Execute;
var
  OutBuf : array [0..$ffff] of Word;
  OutFrames :Size_t;
  err: hresult;
  wantframes :Size_t;
  Divider: integer;
  i: integer;
type
  TShortIntArray = array[0..$ffff] of Word;
  PShortIntArray = ^TShortIntArray;
var
  pp: PShortIntArray;

begin
  Pa_StartStream(fOwner.Stream_out);
  repeat
     RTLeventWaitFor(evPause);
     if Terminated then
        Break;

     RTLeventSetEvent(evPause);
     wantframes := Length(OutBuf) div 2;
     case  fOwner.CurrentSoundDecoder of
      csdSndFile: begin
                    OutFrames := sf_readf_short(fOwner.StreamHandle, @outbuf[0], wantframes);
                    if fOwner.fvolume <> 1 then
                     begin
                       pp := @outbuf[0];
                       for i := 0 to (Outframes * 2) - 1 do
                         pp^[i] := NtoLE(round(LEtoN(pp^[i]) * fOwner.fvolume));
                     end;
                    Divider := 1;
                  end;
      csdMPG123 : begin
                     Err := mpg123_read(fOwner.StreamHandle, @outbuf[0], wantframes, outframes);
                     Divider:= 4;
                  end;
    end;
    if OutFrames < wantframes then
       begin
         fOwner.fState := ENGINE_SONG_END;
       end;

   if OutFrames > 0 then
      Pa_WriteStream(fowner.Stream_out, @outbuf[0], outframes div Divider);

  until Terminated or (fOwner.fState in [ENGINE_STOP,ENGINE_SONG_END]);

  Pa_StopStream(fOwner.Stream_out);
  Pa_CloseStream(fOwner.Stream_out);
  if  fOwner.fState = ENGINE_SONG_END then
      fOwner.PostCommand(ecNext);

end;

constructor TDecodingThread.Create(CreateSuspended: boolean; OWner:TAudioEngineOpenLib);
begin
  inherited Create(CreateSuspended);
  fOwner := OWner;
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
  case CurrentSoundDecoder of
    csdMPG123 : mpg123_volume(StreamHandle, fVolume);
  end;

end;

function TAudioEngineOpenLib.GetMaxVolume: integer;
begin
  Result:= 255;
end;

function TAudioEngineOpenLib.GetSongPos: integer;
begin
  exit;
  case CurrentSoundDecoder of
    csdMPG123 : Result := trunc(mpg123_tell(StreamHandle) / (fRate / 1000));
    csdSndFile : Result := trunc(sf_seek(StreamHandle, 0, SEEK_CUR) / (fRate / 1000));
  end;
end;

procedure TAudioEngineOpenLib.SetSongPos(const AValue: integer);
begin
  case CurrentSoundDecoder of
    csdMPG123 : mpg123_seek(StreamHandle,trunc( AValue * (fRate / 1000)), SEEK_SET);
    csdSndFile : sf_seek(StreamHandle, trunc(AValue * (fRate / 1000)), SEEK_SET);
  end;

end;

procedure TAudioEngineOpenLib.Activate;
begin


end;

constructor TAudioEngineOpenLib.Create;
begin
  inherited Create;
  {  }
  {$IFDEF LINUX}
  Pa_Load('libportaudio.so.2');
  sf_Load('libsndfile.so.1');
  Mp_Load('libmpg123.so.0');
  {$ENDIF LINUX}
  {$IFDEF WINDOWS}
  Pa_Load('libportaudio.dll');
  sf_Load('libsndfile.dll');
  Mp_Load('libmpg123.dll');
  {$ENDIF LINUX}

  {$IFDEF DARWIN}
  Pa_Load('LibPortaudio-32.dylib');
  sf_Load('LibSndFile-32.dylib');
  Mp_Load('LibMpg123-32.dylib');
  {$ENDIF DARWIN}

  Pa_Initialize();
  fdevice := Pa_GetDefaultOutputDevice();
  mpg123_init;
  fVolume:=100;
  fdecoupler := TDecoupler.Create;

  fdecoupler.OnCommand := @ReceivedCommand;

end;

destructor TAudioEngineOpenLib.Destroy;
begin
  Stop;
  Pa_Unload();
  sf_Unload();
  Mp_Unload();

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

procedure TAudioEngineOpenLib.DoPlay(Song: TSong; offset:Integer);
Var
  savedVolume: Integer;
  err : integer;
  Fchannels, Fencoding:Integer;
begin
  // create new media
  if Not FileExistsUTF8(Song.FullName) then
     exit;

  if Assigned(DecodingThread) then
     begin
       DecodingThread.Terminate;
       FreeAndNil(DecodingThread);
     end;
  pa_OutInfo.device:= fdevice;
  StreamHandle := sf_open(Song.FullName, SFM_READ, sfInfo);
  if StreamHandle = nil then
    begin
      CurrentSoundDecoder:= csdMPG123;
      err :=0;
      StreamHandle:= mpg123_new(nil,err);
      mpg123_open(StreamHandle, pChar(Song.FullName));
      mpg123_getformat(StreamHandle, Frate, Fchannels, Fencoding);
      mpg123_format_none(StreamHandle);
      mpg123_format(StreamHandle, Frate, Fchannels, Fencoding);
      mpg123_seek_frame(StreamHandle, 0, SEEK_SET);
      pa_OutInfo.channelCount:=Fchannels;
    end
  else
    begin
      CurrentSoundDecoder:= csdSndFile;
      frate:= sfInfo.samplerate;
      pa_OutInfo.channelCount:= sfInfo.channels;
      sf_seek(StreamHandle, 0, SEEK_SET);
    end;

  pa_OutInfo.channelCount:=2;
  pa_OutInfo.sampleFormat:=paInt16;
  pa_OutInfo.hostApiSpecificStreamInfo:=nil;
  pa_OutInfo.suggestedLatency:=Pa_GetDeviceInfo(fdevice)^.defaultHighOutputLatency * 1;

  Pa_OpenStream(@Stream_out,
                nil,
                @pa_OutInfo,
                fRate,
                0,
                paClipOff, nil,self);

  DecodingThread := TDecodingThread.Create(False, self);

  fState:= ENGINE_PLAY;
  DecodingThread.Start;
  RTLeventSetEvent(DecodingThread.evPause);

  savedVolume := 100;
  if offset <> 0 then
    Seek(offset, true);
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
    {$IFDEF LINUX}
    Result :=   Pa_Load('libportaudio.so.2') and
                sf_Load('libsndfile.so.1')  and
                Mp_Load('libmpg123.so.0');
    {$ENDIF LINUX}
    {$IFDEF DARWIN}
    Result :=   Pa_Load('LibPortaudio-32.dylib') and
                sf_Load('LibSndFile-32.dylib') and
                Mp_Load('LibMpg123-32.dylib');
    {$ENDIF DARWIN}
    {$IFDEF WINDOWS}
    Result :=   Pa_Load('LibPortaudio.dll') and
                sf_Load('LibSndFile.dll') and
                Mp_Load('LibMpg123.dll');
    {$ENDIF DARWIN}

  except
  end;

  try
     Pa_Unload();
     sf_Unload();
     Mp_Unload();
  except
  end;
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
  DecodingThread.Terminate;
  DecodingThread.WaitFor;
  FreeAndNil(DecodingThread);
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
  RegisterEngineClass(TAudioEngineOpenLib, 1, false, true);


end.
