unit OL_RendererPortAudio;

{$mode objfpc}{$H+}
{$interfaces corba}
interface

uses
  Classes, SysUtils, OL_Classes, UOS_portaudio;

type

  { TOL_RendererPortaudio }

  TOL_RendererPortaudio = class(iOL_Renderer)
  private
    fdevice: PaDeviceIndex;
    pa_OutInfo: PaStreamParameters;
    Stream_out: PaStream;
    fLastError: integer;
    fStreamFormat : TOLStreamFormat;
    function Check(HR: integer): boolean; inline;
  Protected
    function GetStreamFormat: TOLStreamFormat;
    procedure SetStreamFormat(AValue: TOLStreamFormat);
  public
    function Load(LibraryName: string = ''): boolean;
    procedure UnLoad;
    procedure Start;
    procedure Stop;
    function Initialize: boolean;
    procedure Finalize;
    procedure Write(const Frames: integer; Buffer: POLBuffer);

  end;

implementation

{ TOL_RendererPortaudio }

function TOL_RendererPortaudio.Check(HR: integer): boolean;
begin
  Result := HR = 0;
  if not Result then
    fLastError := HR;
end;

function TOL_RendererPortaudio.GetStreamFormat: TOLStreamFormat;
begin
  Result := fStreamFormat;
end;

procedure TOL_RendererPortaudio.SetStreamFormat(AValue: TOLStreamFormat);
begin
  fStreamFormat := AValue;
end;

function TOL_RendererPortaudio.Load(LibraryName: string): boolean;
begin
  Result := UOS_portaudio.Pa_Load(LibraryName);
end;

procedure TOL_RendererPortaudio.UnLoad;
begin
  Pa_Terminate;
  Pa_Unload();
end;

procedure TOL_RendererPortaudio.Start;
begin
  Check(Pa_StartStream(Stream_out));
end;

procedure TOL_RendererPortaudio.Stop;
begin
  Check(Pa_StopStream(Stream_out));
end;

function TOL_RendererPortaudio.Initialize: boolean;
//var info: PPaDeviceInfo;
//  xx: PaDeviceIndex;
//  i: Integer;
begin
  Result := False;
  if not Check(Pa_Initialize()) then
    exit;

 // xx := Pa_GetDeviceCount();
 // for i := 0 to xx -1 do
 //   begin
 //    info:= Pa_GetDeviceInfo(fdevice);
 ////    WriteLn('device;', info^._name,' Hostapi ',info^.hostApi);
 //   end;

  fdevice := Pa_GetDefaultOutputDevice();


  pa_OutInfo.device := fdevice;
  pa_OutInfo.channelCount := fStreamFormat.Channels;
  pa_OutInfo.sampleFormat := paInt16;
  pa_OutInfo.hostApiSpecificStreamInfo := nil;
  pa_OutInfo.suggestedLatency := Pa_GetDeviceInfo(fdevice)^.defaultHighOutputLatency * 1;

  if not Check(Pa_OpenStream(@Stream_out, nil, @pa_OutInfo, fStreamFormat.BitRate, 512, paClipOff, nil, self)) then
    exit;

  Result := True;

end;

procedure TOL_RendererPortaudio.Finalize;
begin
  if Assigned(Stream_out) then
    Pa_CloseStream(@Stream_out);
  Pa_Terminate;
end;

procedure TOL_RendererPortaudio.Write(const Frames: integer; Buffer: POLBuffer);
begin
  Check(Pa_WriteStream(Stream_out, @buffer[0], Frames));
//  WriteLn('WRITE Frame:',Frames, 'Last Error:', fLastError);
end;

end.
