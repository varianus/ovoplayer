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
unit AudioEngine_FFMpeg;

{$mode objfpc}{$H+}

interface

uses
  lclproc,Classes, SysUtils, ExtCtrls, decoupler, song, Basetypes, AudioEngine,
  avcodec, avformat, avutil, swscale, lazdyn_portaudio, ctypes;

type

  { TAudioEngineFFMpeg }

  TDecodingThread = class;

  TAudioEngineFFMpeg = class(TAudioEngine)
  private
    StreamName: String;
    fdecoupler: TDecoupler;
    fState : TEngineState;
    fdevice : PaDeviceIndex;
    pa_OutInfo : PaStreamParameters;
    fAVFormatContext: PAVFormatContext;
    audioStream: pAVStream;
    frame : PAVFrame;
    fCodec:     PAVCodec;
    codecContext: pAVCodecContext;
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
    procedure ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0); override;
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
   fOwner : TAudioEngineFFMpeg;
   packet: TAVPacket;
 protected
   procedure Execute; override;

 public
   constructor Create(CreateSuspended: boolean; OWner:TAudioEngineFFMpeg);
   destructor Destroy; override;
 end;


implementation
uses math;

Const
   FFMPEGMAXVOLUME = 1;

{ TDecodingThread }

procedure TDecodingThread.Execute;
var
  OutBuf : array [0..$ffff] of cint16;
  OutFrames :cint;
  Res: cint;
  wantframes :cint;
  Divider: integer;
  i,j: integer;
  pError: PaError;
type
  TShortIntArray = array[0..$ffff] of cint16;
  PShortIntArray = ^TShortIntArray;
var
  pp, PP1: TShortIntArray;

  decodingPacket :  TAVPacket;
  frame: PAVFrame;
begin
  pError := Pa_StartStream(fOwner.Stream_out);
  repeat
     RTLeventWaitFor(evPause);
     if Terminated then
        Break;

     RTLeventSetEvent(evPause);
     wantframes := Length(OutBuf) div 2;
     Frame := avcodec_alloc_frame;
     Res := av_read_frame(fowner.fAVFormatContext, Packet);
     if (Res < 0 ) then
       begin
         fOwner.fState := ENGINE_SONG_END;
       end;

     decodingPacket := packet;
     if (decodingPacket.stream_index = fOwner.audioStream^.index) then
        while (decodingPacket.size > 0) do
          begin
            Res:= avcodec_decode_audio4(fOwner.codecContext, frame, @OutFrames, @decodingPacket);
            if Res > 0 then
               begin
                dec(decodingPacket.size, Res);
                inc(decodingPacket.data, Res);
               end
            else
               begin
                 decodingPacket.size := 0;
                 OutFrames := -1;
               end;
          end
     else
       OutFrames := -1;
     av_free_packet(@packet);

     if OutFrames > 0 then
       begin
//          for i := 0 to frame^.nb_samples -1 do
 //              pp[i] := pShortIntArray(frame^.data[0])^[i];
//          Pa_WriteStream(fowner.Stream_out, @pp, frame^.nb_samples);
            for i := 0 to frame^.nb_samples -1 do
                pShortIntArray(frame^.data[0])^[i] := trunc(pShortIntArray(frame^.data[0])^[i] * fOwner.fVolume);

             Pa_WriteStream(fowner.Stream_out, pShortIntArray(frame^.data[0]), frame^.nb_samples);


       end;
      avcodec_free_frame(@frame);

  until Terminated or (fOwner.fState in [ENGINE_STOP,ENGINE_SONG_END]);

  Pa_StopStream(fOwner.Stream_out);
  Pa_CloseStream(fOwner.Stream_out);
  avcodec_close(Fowner.codecContext);
  av_close_input_file(fOwner.fAVFormatContext);
  if  fOwner.fState = ENGINE_SONG_END then
      fOwner.PostCommand(ecNext);

end;

constructor TDecodingThread.Create(CreateSuspended: boolean; OWner:TAudioEngineFFMpeg);
begin
  inherited Create(CreateSuspended);
  fOwner := OWner;
  evPause := RTLEventCreate;
  av_init_packet(packet);
end;

destructor TDecodingThread.Destroy;
begin
  inherited Destroy;
  RTLeventdestroy(evPause);
end;

{ TAudioEngineFFMpeg }

function TAudioEngineFFMpeg.GetMainVolume: integer;
begin
  Result := trunc(fVolume * (255 / FFMPEGMAXVOLUME));
end;

procedure TAudioEngineFFMpeg.SetMainVolume(const AValue: integer);
begin
  fVolume := AValue * (FFMPEGMAXVOLUME / 255);

end;

function TAudioEngineFFMpeg.GetMaxVolume: integer;
begin
  Result:= 255;
end;

function TAudioEngineFFMpeg.GetSongPos: integer;
begin
  result := 100;

  //case CurrentSoundDecoder of
  //  csdMPG123 : Result := trunc(mpg123_tell(StreamHandle) / (fRate / 1000));
  //  csdSndFile : Result := trunc(sf_seek(StreamHandle, 0, SEEK_CUR) / (fRate / 1000));
  //end;
end;

procedure TAudioEngineFFMpeg.SetSongPos(const AValue: integer);
begin
  //case CurrentSoundDecoder of
  //  csdMPG123 : mpg123_seek(StreamHandle,trunc( AValue * (fRate / 1000)), SEEK_SET);
  //  csdSndFile : sf_seek(StreamHandle, trunc(AValue * (fRate / 1000)), SEEK_SET);
  //end;

end;

procedure TAudioEngineFFMpeg.Activate;
begin


end;

constructor TAudioEngineFFMpeg.Create;
begin
  inherited Create;
  {$IFDEF LINUX}
  Pa_Load('libportaudio.so.2');
  {$ENDIF LINUX}
  {$IFDEF WINDOWS}
  Pa_Load('LibPortaudio-32.dll');

  {$ENDIF LINUX}

  {$IFDEF DARWIN}
  Pa_Load('LibPortaudio-32.dylib');
  {$ENDIF DARWIN}

  avcodec_register_all();
  av_register_all();

  Pa_Initialize();
  fdevice := Pa_GetDefaultOutputDevice();

  // Initialize FFmpeg

  fVolume:=100;
  fdecoupler := TDecoupler.Create;

  fdecoupler.OnCommand := @ReceivedCommand;

end;

destructor TAudioEngineFFMpeg.Destroy;
begin
  Stop;
  Pa_Unload();
  fdecoupler.Free;
  inherited Destroy;
end;

function TAudioEngineFFMpeg.GetState: TEngineState;
begin
  Result := fState;
end;

procedure TAudioEngineFFMpeg.Pause;
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

procedure TAudioEngineFFMpeg.DoPlay(Song: TSong; offset:Integer);
Var
  savedVolume: Integer;
  err : integer;
  Fchannels, Fencoding:Integer;
  i: integer;
begin
  // create new media
  if Not FileExists(Song.FullName) then
     exit;

  if Assigned(DecodingThread) then
     begin
       DecodingThread.Terminate;
       FreeAndNil(DecodingThread);
     end;

  pa_OutInfo.device:= fdevice;

//  frame := avcodec_alloc_frame;
  fAVFormatContext :=nil;
  err := avformat_open_input(@fAVFormatContext, pchar(Song.FullName), nil, nil);

  err := avformat_find_stream_info(fAVFormatContext, nil);

// av_dump_format(fAVFormatContext, 0, pchar(Song.FullName), 0);

  for i := 0 to fAVFormatContext^.nb_streams -1 do
    if fAVFormatContext^.streams[i]^.codec^.codec_type = AVMEDIA_TYPE_AUDIO then
      begin
        audioStream:= fAVFormatContext^.streams[i];
      end;

  codecContext:= audioStream^.codec;

  fcodec := avcodec_find_decoder(codecContext^.codec_id);
  codecContext^.workaround_bugs := FF_BUG_AUTODETECT;

  err := avcodec_open2(codecContext, fcodec, nil);

  pa_OutInfo.channelCount:=codecContext^.channels;
  fRate:= codecContext^.sample_rate;
  DebugLn(IntToStr(frATE));
  pa_OutInfo.sampleFormat:=paInt16;
  pa_OutInfo.hostApiSpecificStreamInfo:=nil;
  pa_OutInfo.suggestedLatency:=Pa_GetDeviceInfo(fdevice)^.defaultHighOutputLatency * 1;

  err:=Pa_OpenStream(@Stream_out,
                nil,
                @pa_OutInfo,
                fRate,
                4092,
                paClipOff, nil,self);
  if err <> 0 then
    DebugLn(Pa_GetErrorText(err));
  DecodingThread := TDecodingThread.Create(False, self);

  fState:= ENGINE_PLAY;
  DecodingThread.Start;
  RTLeventSetEvent(DecodingThread.evPause);

  savedVolume := 100;
  if offset <> 0 then
    Seek(offset, true);
end;

procedure TAudioEngineFFMpeg.SetMuted(const AValue: boolean);
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

function TAudioEngineFFMpeg.GetMuted: boolean;
begin
 result:=fMuted;
end;

class function TAudioEngineFFMpeg.GetEngineName: String;
begin
  Result:='FFMPEG';
end;

procedure TAudioEngineFFMpeg.ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0);
begin
  case Command of
    ecNext: if Assigned(OnSongEnd) then
        OnSongEnd(Self);

    ecSeek: Seek(Param, True);

    end;
end;

class function TAudioEngineFFMpeg.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  Result := true;
  try
     {$IFDEF LINUX}
     Pa_Load('libportaudio.so.2');
     {$ENDIF LINUX}
     {$IFDEF WINDOWS}
     Pa_Load('LibPortaudio-32.dll');
     {$ENDIF LINUX}
     {$IFDEF DARWIN}
     Pa_Load('LibPortaudio-32.dylib');
     {$ENDIF DARWIN}

  except
  end;

  try
     Pa_Unload();

  except
  end;
end;

procedure TAudioEngineFFMpeg.PostCommand(Command: TEngineCommand; Param: integer);
begin
  fdecoupler.SendCommand(Command, Param);
end;

function TAudioEngineFFMpeg.Playing: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

function TAudioEngineFFMpeg.Running: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

procedure TAudioEngineFFMpeg.Seek(Seconds: integer; SeekAbsolute: boolean);
var
  currpos: integer;
begin
  currpos := GetSongPos;
  if SeekAbsolute then
    SetSongPos(Seconds * 1000)
  else
    SetSongPos(currpos + Seconds * 1000);

end;

procedure TAudioEngineFFMpeg.Stop;
begin
  DecodingThread.Terminate;
  DecodingThread.WaitFor;
  FreeAndNil(DecodingThread);
end;

procedure TAudioEngineFFMpeg.UnPause;
begin

  if (GetState() = ENGINE_PAUSE) then
    begin
        RTLeventSetEvent(DecodingThread.evPause);
        fState:=ENGINE_PLAY;
    end;
end;

initialization
  RegisterEngineClass(TAudioEngineFFMpeg, 1, false, true);


end.
