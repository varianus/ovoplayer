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
unit audioengine_mf;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, AudioEngine, Mediafoundation, Song, lclproc;

type

  TAudioEngineMediaFoundation =class;

  { TMFEventHandler }

  TMFEventHandler = class(TInterfacedObject, IMFAsyncCallback)
  public
    TheEngine: TAudioEngineMediaFoundation;
    function GetParameters(out pdwFlags: DWord; out pdwQueue: DWord): HResult; stdcall;
    function Invoke(const pAsyncResult: IMFAsyncResult): HResult; stdcall;

  end;

  { TAudioEngineMediaFoundation }

  TAudioEngineMediaFoundation = class(TAudioEngine)
  private
    fSavedVolume: integer;
    fMuted: boolean;

    pSession: ImfMediaSession;
    pSource: IMFMediaSource;
    EventHandler: TMFEventHandler;
    pClock : IMFCLOCK;
//    pClock : IMFPresentationCLOCK;
    procedure Release;
  protected
    function GetMainVolume: integer; override;
    procedure SetMainVolume(const AValue: integer); override;
    function GetMaxVolume: integer; override;
    function GetSongPos: integer; override;
    procedure SetSongPos(const AValue: integer); override;
    function GetState: TEngineState; override;
    procedure DoPlay(Song: TSong; offset: integer); override;
    procedure SetMuted(const AValue: boolean); override;
    function GetMuted: boolean; override;
    procedure ReceivedCommand(Sender: TObject; Command: TEngineCommand;
      Param: integer = 0); override;
  public
    class function IsAvalaible(ConfigParam: TStrings): boolean; override;
    class function GetEngineName: string; override;

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

uses Windows, Math;

{ TAudioEngineMediaFoundation }

function GetBasicAudioVolume(Value: integer): integer;
begin
  Result := Round(Power(10, Value / 2500) * 10001 - 1);
end;

function SetBasicAudioVolume(Value: integer): integer;
begin
  Result := Round(Log10((Value + 1) / 10000) * 2500);
end;


{ TMFEventHandler }

function TMFEventHandler.GetParameters(out pdwFlags: DWord;
  out pdwQueue: DWord): HResult; stdcall;
begin

end;

function TMFEventHandler.Invoke(const pAsyncResult: IMFAsyncResult): HResult;
  stdcall;
var
  Event: IMFMediaEvent;
  hr: HresulT;
  meType : MediaEventType;
begin
  hr := TheEngine.pSession.EndGetEvent(pAsyncResult, Event);
  hr :=  TheEngine.pSession.BeginGetEvent(self as IMFAsyncCallback, nil);
  hr := Event.GetType(meType);
  if meType = 211 then
    theEngine.PostCommand(ecNext);



end;


function TAudioEngineMediaFoundation.GetMainVolume: integer;
begin
  Result := -1;
  //AudioControl.get_Volume( Result) ;
  Result := GetBasicAudioVolume(Result);
end;

procedure TAudioEngineMediaFoundation.SetMainVolume(const AValue: integer);
begin
  //  AudioControl.put_Volume( SetBasicAudioVolume(Avalue));
end;

function TAudioEngineMediaFoundation.GetMaxVolume: integer;
begin
  Result := 10000;
end;

function TAudioEngineMediaFoundation.GetSongPos: integer;
var
 dpo,  dpo1: MFTIME;
 hr : hresult;
begin
 if not assigned(pClock) then exit;
 dpo := 0;
 hr := pClock.GetCorrelatedTime(0,dpo,dpo1);
 result := dpo div 10000;
end;

procedure TAudioEngineMediaFoundation.SetSongPos(const AValue: integer);
var
  varStart : PROPVARIANT;
begin
  varStart.vt := 20;
  varStart.Something := AValue * 10000;
  pSession.Start(GUID_NULL, varStart);

  //  PositionControl.put_CurrentPosition( AValue /1000 );
end;

procedure TAudioEngineMediaFoundation.Activate;
var
  hr: HRESULT;
begin
  if Assigned(pSession) then
    begin
    Release;
    end;

  hr := MFCreateMediaSession(nil, pSession);
  if hr <> 0 then
    raise Exception.Create('Error');


end;

constructor TAudioEngineMediaFoundation.Create;
begin
  inherited Create;
  MFStartup(MF_VERSION, MFSTARTUP_FULL);

  EventHandler := TMFEventHandler.Create;

  fMuted := False;

end;

procedure TAudioEngineMediaFoundation.Release;
begin
  pSession.Shutdown;
  pSession := nil;
end;

destructor TAudioEngineMediaFoundation.Destroy;
begin

  MFShutdown;
  Release;
  EventHandler.Free;

  inherited Destroy;
end;

function TAudioEngineMediaFoundation.GetState: TEngineState;
  //var
  //  State: TFilter_State;
begin

  Result := ENGINE_ON_LINE;
  //  MediaControl.GetState(200,State);

  //case State of
  //  State_Running: Result   := ENGINE_PLAY;
  //  State_Paused: Result    := ENGINE_PAUSE;
  //  State_Stopped: Result   := ENGINE_STOP;
  //  end;
end;

procedure TAudioEngineMediaFoundation.Pause;
begin
  pSession.Pause;
end;

procedure TAudioEngineMediaFoundation.DoPlay(Song: TSong; offset: integer);
var
  Hr: HRESULT;
  ObjectType: MF_Object_type;
  Source: IUnknown;
  pResolver: IMFSourceResolver;
  varStart: PropVariant;
var
  pTop: IMFTopology;
  pPD: IMFPresentationDescriptor;
  pactivate: IMFActivate;
  i, cSourceStreams: DWORD;
  srcNode, dstNode: IMFTopologyNode;
  fSelected: longbool;
  sd : IMFStreamDescriptor;
  intF : IMFAsyncCallback;
begin

  Activate;

  hr := MFCreateSourceResolver(pResolver);
  hr := MFCreateTopology(pTop);
  hr := MFCreateAudioRendererActivate(pactivate);
  hr := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE, srcNode);
  hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE, dstNode);

  hr := pResolver.CreateObjectFromURL(PWideChar(WideString(song.FullName)),
    MF_RESOLUTION_MEDIASOURCE,
    nil,
    ObjectType,
    Source);

  hr := Source.QueryInterface(IID_IMFMediaSource, pSource);
  hr := pSource.CreatePresentationDescriptor(pPD);

  hr := ppd.GetStreamDescriptorByIndex(0, fSelected, sd);
{
          if (FAILED(pd->GetStreamDescriptorByIndex(0, &selected, &sd))) return NULL;
          if (FAILED(sd->GetMediaTypeHandler(&typeHandler))) return NULL;
          if (FAILED(typeHandler->GetMajorType(&majorType))) return NULL;
          if (majorType != MFMediaType_Audio) return NULL;               }

  hr := srcNode.SetUnknown(MF_TOPONODE_SOURCE, psource);
  hr := srcNode.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR, ppd);
  hr := srcNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR, sd);

  hr := ptop.AddNode(srcNode);
  hr := dstNode.SetObject(pactivate);
  hr := dstNode.SetUINT32(MF_TOPONODE_STREAMID, 0);
  hr := dstNode.SetUINT32(MF_TOPONODE_NOSHUTDOWN_ON_REMOVE, longword(TRUE));
  hr := ptop.AddNode(dstNode);
  hr := srcNode.ConnectOutput(0, dstNode, 0);
  hr := psession.SetTopology(0, ptop);
  EventHandler.TheEngine := self;

  varStart.vt := 0;
  varStart.Something := 0;
  pSession.Start(GUID_NULL, varStart);

  hr := pSession.QueryInterface( IID_IMFAsyncCallback, intF);
  pSession.BeginGetEvent(intF, nil);

  Seek(offset, True);
  hr := pSession.GetClock(pClock);


end;

procedure TAudioEngineMediaFoundation.SetMuted(const AValue: boolean);
begin
  if AValue = fMuted then
    exit;
  if fMuted then
    begin
    fSavedVolume := GetMainVolume;
    setMainVolume(0);
    fMuted := True;
    end
  else
    begin
    setMainVolume(fSavedVolume);
    fMuted := False;
    end;

end;

function TAudioEngineMediaFoundation.GetMuted: boolean;
begin
  Result := fMuted;
end;

procedure TAudioEngineMediaFoundation.ReceivedCommand(Sender: TObject;
  Command: TEngineCommand; Param: integer = 0);
begin
  case Command of
    ecNext: if Assigned(OnSongEnd) then
        OnSongEnd(Self);

    ecSeek: Seek(Param, True);

    end;
end;

class function TAudioEngineMediaFoundation.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  Result := True;
  //try
  //   if Load_DShowDLL(DShow_name) then
  //      begin
  //        result:= true;
  //        Unload_DShowDLL;
  //      end;
  //except
  //  exit;
  //end;
end;

class function TAudioEngineMediaFoundation.GetEngineName: string;
begin
  Result := 'MediaFoundation';
end;

procedure TAudioEngineMediaFoundation.PostCommand(Command: TEngineCommand;
  Param: integer);
begin
  ReceivedCommand(Self, Command, Param);
end;

function TAudioEngineMediaFoundation.Playing: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

function TAudioEngineMediaFoundation.Running: boolean;
begin
  Result := True;
end;

procedure TAudioEngineMediaFoundation.Seek(Seconds: integer; SeekAbsolute: boolean);
var
  currpos: integer;
begin
  currpos := GetSongPos;
  if SeekAbsolute then
    SetSongPos(Seconds * 1000)
  else
    SetSongPos(currpos + Seconds * 1000);

end;

procedure TAudioEngineMediaFoundation.Stop;
begin
  pSession.Stop;
end;

procedure TAudioEngineMediaFoundation.UnPause;
begin
  //  pSession.Start();;

end;

initialization
  RegisterEngineClass(TAudioEngineMediaFoundation, 4, False, True);

end.
