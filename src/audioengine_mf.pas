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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AudioEngine, Mediafoundation, Basetypes, Song, decoupler, lclproc;

type

  TAudioEngineMediaFoundation = class;

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
    fDecoupler: TDecoupler;

    Source: IUnknown;
    pVolume: IMFSimpleAudioVolume;
    pSession: ImfMediaSession;
    pSource: IMFMediaSource;
    EventHandler: TMFEventHandler;
    pClock: IMFPresentationCLOCK;
    procedure Release;
  protected
    function GetMainVolume: integer; override;
    procedure SetMainVolume(const AValue: integer); override;
    function GetMaxVolume: integer; override;
    function GetSongPos: integer; override;
    procedure SetSongPos(const AValue: integer); override;
    function GetState: TEngineState; override;
    Function DoPlay(Song: TSong; offset:Integer):boolean; override;
    procedure SetMuted(const AValue: boolean); override;
    function GetMuted: boolean; override;
  public
    class function IsAvalaible(ConfigParam: TStrings): boolean; override;
    class function GetEngineName: string; override;

    procedure PostCommand(Command: TEngineCommand; Param: integer = 0); override;
    constructor Create; override;
    destructor Destroy; override;
    Function Initialize: boolean; override;
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

const
  MFMAXVOLUME = 1;

{ TMFEventHandler }
function TMFEventHandler.GetParameters(out pdwFlags: DWord; out pdwQueue: DWord): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TMFEventHandler.Invoke(const pAsyncResult: IMFAsyncResult): HResult;
  stdcall;
var
  xEvent: IMFMediaEvent;
  hr, hrstatus: HresulT;
  meType: MediaEventType;
begin
  Result := S_OK;
  xevent := nil;
  if not Assigned(TheEngine.pSession) then
    exit;

  hr := TheEngine.pSession.EndGetEvent(pAsyncResult, xEvent);

  if SUCCEEDED(hr) then
    hr := xEvent.GetType(meType);

  if SUCCEEDED(hr) then
    begin
    if meType = 211 then
      theEngine.PostCommand(ecNext);
    if meType = 103 then
      begin
        theEngine.pVolume := nil;
        hr := MFGetService(theEngine.pSession, MR_POLICY_VOLUME_SERVICE, IID_IMFSimpleAudioVolume, theEngine.pVolume);
      end;

    end;

  hr := TheEngine.pSession.BeginGetEvent(TheEngine.EventHandler, nil);
  Result := hr;
end;

{ TAudioEngineMediaFoundation }

function TAudioEngineMediaFoundation.GetMainVolume: integer;
var
  v: single;
begin
  Result := -1;
  if not Assigned(pVolume) then
    exit;
  pVolume.GetMasterVolume(v);
  Result := trunc(V * (255 / MFMAXVOLUME));

end;

procedure TAudioEngineMediaFoundation.SetMainVolume(const AValue: integer);
begin
  if not Assigned(pVolume) then
    exit;

  pVolume.SetMasterVolume(AValue * (MFMAXVOLUME / 255));
end;

function TAudioEngineMediaFoundation.GetMaxVolume: integer;
begin
  Result := MFMAXVOLUME;
end;

function TAudioEngineMediaFoundation.GetSongPos: integer;
var
  dpo, dpo1: MFTIME;
  hr: hresult;
begin
  Result := 0;
  if not assigned(pClock) then
    exit;
  dpo := 0;
  // hr := pClock.GetTime(dpo);
  hr := pClock.GetCorrelatedTime(0, dpo, dpo1);
  Result := dpo div 10000;
end;

procedure TAudioEngineMediaFoundation.SetSongPos(const AValue: integer);
var
  varStart: PROPVARIANT;
begin
  varStart.vt := 20;
  varStart.hVal.QuadPart := int64(AValue) * 10000;
  pSession.Start(GUID_NULL, varStart);

  //  PositionControl.put_CurrentPosition( AValue /1000 );
end;

procedure TAudioEngineMediaFoundation.Activate;
var
  hr: HRESULT;
begin
  if GetState in [ENGINE_PLAY, ENGINE_PAUSE] then
     Stop;
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
  libMF_dynamic_dll_init;
end;

procedure TAudioEngineMediaFoundation.Release;
var
  xEvent: IMFMediaEvent;
  pAsyncResult: IMFAsyncResult;
begin
  if Assigned(pSession) then
    begin
    pSession.Shutdown;
    pSession := nil;
    EventHandler:= nil;
    end;
  if Assigned(pSource) then
     begin
       pSource.Shutdown;
       pSource:= nil;
     end;

end;

destructor TAudioEngineMediaFoundation.Destroy;
begin
  if Initialized then
    begin
      Release;
      MFShutdown;
      EventHandler := nil;
      if Assigned(fDecoupler) then
         fDecoupler.Free;
    end;
  libMF_dynamic_dll_Done;
  inherited Destroy;
end;

function TAudioEngineMediaFoundation.Initialize: boolean;
begin

  result := MFStartup(MF_VERSION, MFSTARTUP_FULL) = S_OK;
  Initialized := Result;
  if Result then
     begin
       fDecoupler := TDecoupler.Create;
       fdecoupler.OnCommand := @ReceivedCommand;
     end;

end;

function TAudioEngineMediaFoundation.GetState: TEngineState;
var
  EnState: MF_CLOCK_STATE;
begin

  Result := ENGINE_ON_LINE;
  if not Assigned(pClock) then
    exit;
  pClock.GetState(0, EnState);
  case EnState of
    MFCLOCK_STATE_RUNNING: Result := ENGINE_PLAY;
    MFCLOCK_STATE_PAUSED: Result := ENGINE_PAUSE;
    MFCLOCK_STATE_STOPPED: Result := ENGINE_STOP;
    end;
end;

procedure TAudioEngineMediaFoundation.Pause;
begin
  pSession.Pause;
end;

function TAudioEngineMediaFoundation.DoPlay(Song: TSong; offset: Integer
  ): boolean;
var
  Hr: HRESULT;
  ObjectType: MF_Object_type;
  pResolver: IMFSourceResolver;
  varStart: PropVariant;
var
  pTop: IMFTopology;
  pPD: IMFPresentationDescriptor;
  pactivate: IMFActivate;
  i, cSourceStreams: DWORD;
  srcNode, dstNode: IMFTopologyNode;
  fSelected: longbool;
  sd: IMFStreamDescriptor;
  intF: IMFAsyncCallback;
  xClock: IMFClock;
  tmp: IUnknown;
begin

  result := false;
  Activate;

  hr := MFCreateSourceResolver(pResolver);
  try
  if not Succeeded(Hr) then
    exit;

  hr := MFCreateTopology(pTop);
  if not Succeeded(Hr) then
    exit;

  hr := MFCreateAudioRendererActivate(pactivate);
  if not Succeeded(Hr) then
    exit;

  hr := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE, srcNode);
    if not Succeeded(Hr) then
    exit;

  hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE, dstNode);
    if not Succeeded(Hr) then
    exit;

  hr := pResolver.CreateObjectFromURL(PWideChar(WideString(song.FullName)),
        MF_RESOLUTION_MEDIASOURCE or MF_RESOLUTION_CONTENT_DOES_NOT_HAVE_TO_MATCH_EXTENSION_OR_MIME_TYPE,
    nil, ObjectType, Source);

  if not Succeeded(Hr) then
      exit;

  hr := Source.QueryInterface(IID_IMFMediaSource, pSource);
  if not Succeeded(Hr) then
    exit;

  hr := pSource.CreatePresentationDescriptor(pPD);
  if not Succeeded(Hr) then
    exit;

  hr := ppd.GetStreamDescriptorByIndex(0, fSelected, sd);
  if not Succeeded(Hr) then
    exit;

{
          if (FAILED(pd->GetStreamDescriptorByIndex(0, &selected, &sd))) return NULL;
          if (FAILED(sd->GetMediaTypeHandler(&typeHandler))) return NULL;
          if (FAILED(typeHandler->GetMajorType(&majorType))) return NULL;
          if (majorType != MFMediaType_Audio) return NULL;               }

  hr := srcNode.SetUnknown(MF_TOPONODE_SOURCE, psource);
  if not Succeeded(Hr) then
    exit;

  hr := srcNode.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR, ppd);
  if not Succeeded(Hr) then
    exit;

  hr := srcNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR, sd);
  if not Succeeded(Hr) then
    exit;

  hr := ptop.AddNode(srcNode);
  if not Succeeded(Hr) then
    exit;

  hr := dstNode.SetObject(pactivate);
  if not Succeeded(Hr) then
    exit;

  hr := dstNode.SetUINT32(MF_TOPONODE_STREAMID, 0);
  if not Succeeded(Hr) then
    exit;

  hr := dstNode.SetUINT32(MF_TOPONODE_NOSHUTDOWN_ON_REMOVE, longword(True));
  if not Succeeded(Hr) then
    exit;

  hr := ptop.AddNode(dstNode);
  if not Succeeded(Hr) then
    exit;

  hr := srcNode.ConnectOutput(0, dstNode, 0);
  if not Succeeded(Hr) then
    exit;

  hr := psession.SetTopology(0, ptop);
  if not Succeeded(Hr) then
    exit;

  varStart.vt := 0;
  varStart.hVal.QuadPart := 0;
  hr:= pSession.Start(GUID_NULL, varStart);
  if not Succeeded(Hr) then
    exit;

  hr := pSession.GetClock(xClock);
  if not Succeeded(Hr) then
    exit;

  hr := xClock.QueryInterface(IID_IMFPresentationClock, pClock);
  if not Succeeded(Hr) then
    exit;

  Seek(offset, True);
//  hr := MFGetService(pSession, MR_POLICY_VOLUME_SERVICE, IID_IMFSimpleAudioVolume, tmp);
//  pVolume := tmp as IMFSimpleAudioVolume;

  EventHandler := TMFEventHandler.Create;
  EventHandler.TheEngine := self;

  hr := pSession.BeginGetEvent(EventHandler, nil);
  if not Succeeded(Hr) then
    exit;
  result:=true;
  finally
    if not Succeeded(Hr) then
      begin
        EventHandler := nil;
        xClock := nil;
        ptop := nil;
        pactivate := nil;
        pSession := nil;
        pSource := nil;
      end;
  end;

end;

procedure TAudioEngineMediaFoundation.SetMuted(const AValue: boolean);
begin
  pVolume.SetMute(AValue);
end;

function TAudioEngineMediaFoundation.GetMuted: boolean;
var
  b: bool;
begin
  pVolume.GetMute(b);
  result := b;
end;

class function TAudioEngineMediaFoundation.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  Result := CheckMF;
end;

class function TAudioEngineMediaFoundation.GetEngineName: string;
begin
  Result := 'MediaFoundation';
end;

procedure TAudioEngineMediaFoundation.PostCommand(Command: TEngineCommand; Param: integer);
begin
  fDecoupler.SendCommand(Command, Param);
  //  ReceivedCommand(Self, Command, Param);
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
var
  varStart: PROPVARIANT;
begin
  varStart.vt := VT_EMPTY;
  pSession.Start(GUID_NULL, varStart);

end;

initialization
  RegisterEngineClass(TAudioEngineMediaFoundation, 4, False, True);

end.
