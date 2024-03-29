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

unit audioengine_DShow;

interface

uses
  Classes, SysUtils, BaseTypes, AudioEngine, ActiveX, mediadshow, Song, LazLoggerBase;

type

  { TAudioEngineDShow }

  TAudioEngineDShow = class(TAudioEngine)
  private
    fSavedVolume: integer;
    fMuted: boolean;
    GraphBuilder    : IGraphBuilder;
    MediaEvent      : IMediaEventEx;
    MediaControl    : IMediaControl;
    AudioControl    : IBasicAudio;
    PositionControl : IMediaPosition;
    function CreateAppWindow: boolean;
    procedure Release;
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
    function Initialize: boolean; override;
    procedure Activate; override;
    procedure Pause; override;
    function Playing: boolean; override;
    function Running: boolean; override;
    procedure Seek(Seconds: integer; SeekAbsolute: boolean); override;
    procedure Stop; override;
    procedure UnPause; override;

  end;


implementation
uses windows, math;
Const
  DSHOWMAXVOLUME = 10000;
var

  WindowClassAtom: ATOM; // RegisterWindowClass yields an atom if successful.
  WindowClassInfo: WNDCLASSEX; // Class info Data structure for main window.
  WinClassName: string = 'ovoplayerdsmsg';
  hwindow: HWND;

{ TAudioEngineDShow }


function LogarithmicToLinear(Value : integer) : integer;
begin
  Result := Round(Power(10, Value / 2500) * 255);
end;

function LinearToLogarithmic(Value : integer) : integer;
begin
  if Value <= 0 then
     Result := -10000
  else
     Result := Round(Log10(Value / 255) * 4000);
  if Result < -10000 then
    Result := -10000;
end;

function TAudioEngineDShow.GetMaxVolume: integer;
begin
  Result:=DSHOWMAXVOLUME;
end;

function WinProc(hw: HWND; uMsg: UINT; wp: WPARAM; lp: LPARAM): LRESULT;
  stdcall; export;
var
  user: TAudioEngineDShow;
  evCode : longint;
  param1, param2: longint;
begin
  Result := 0;
  case uMsg of
    WM_APP + 2 : begin
                 User := TAudioEngineDShow(GetWindowLongPtr(HW, GWL_USERDATA));
                  while Succeeded(user.MediaEvent.GetEvent(evCode, param1, param2, 200)) do
                    begin
                      user.MediaEvent.FreeEventParams(evCode, param1, param2);
                      case evCode of
                        $01 : User.PostCommand(ecNext);
                      end;
                    end;
                end;
    else
      Result := DefWindowProc(hw, uMsg, wp, lp);
    end; // Case
end; // WinProc


function TAudioEngineDShow.CreateAppWindow: boolean;

    function RegisterWindowClass: boolean;
    begin
      WindowClassInfo.cbSize := sizeof(WindowClassInfo);
      WindowClassInfo.Style := 0;
      WindowClassInfo.lpfnWndProc := @WinProc;
      WindowClassInfo.cbClsExtra := 0;
      WindowClassInfo.cbWndExtra := 0;
      WindowClassInfo.hInstance := HInstance;
      WindowClassInfo.hIcon := 0;
      WindowClassInfo.hCursor := 0;
      WindowClassInfo.hbrBackground := 0;
      WindowClassInfo.lpszMenuName := nil;
      WindowClassInfo.lpszClassName := PChar(WinClassName);
      WindowClassInfo.hIconSm := 0;
      WindowClassAtom := RegisterClassEx(WindowClassInfo);
      Result := WindowClassAtom <> 0;
    end; // RegisterWindowClass - Nested Function

  begin
    Result := False;

    if not RegisterWindowClass then
      exit;

    HWindow := CreateWindowEx(WS_EX_NOACTIVATE or WS_EX_TRANSPARENT,
      PChar(WinClassName), PChar(WinClassName), Ws_popup or WS_CLIPSIBLINGS, 0,
      0, 0, 0, 0, 0, HInstance, nil);
    if HWindow <> 0 then
      begin
      ShowWindow(HWindow, SW_HIDE);
      SetWindowLongPtr(HWindow,GWL_USERDATA,PtrInt(Self));
      UpdateWindow(HWindow);
      Result := True;
      end;

  end; // CreateAppWindow

function TAudioEngineDShow.GetMainVolume: integer;
begin

  Result := fSavedVolume;
  if not Assigned(AudioControl) then
    exit;

  if Succeeded(AudioControl.get_Volume( Result)) then
    result := LogarithmicToLinear(Result);
  fSavedVolume := Result;
end;

procedure TAudioEngineDShow.SetMainVolume(const AValue: integer);
begin
  fSavedVolume := AValue;
  if not Assigned(AudioControl) then
    exit;

  AudioControl.put_Volume( LinearToLogarithmic(Avalue));
end;

function TAudioEngineDShow.GetSongPos: integer;
var
  Vol: double;
begin
  PositionControl.get_CurrentPosition( vol);
  Result := trunc(vol * 1000);
end;

procedure TAudioEngineDShow.SetSongPos(const AValue: integer);
begin
  PositionControl.put_CurrentPosition( AValue /1000 );
end;

procedure TAudioEngineDShow.Activate;
begin
  if Assigned(MediaControl) then
     begin
       Release;
     end;

  if CoCreateInstance( CLSID_FilterGraph,
                       nil,
                       CLSCTX_INPROC_SERVER,
                       IID_IGraphBuilder,
                       GraphBuilder ) <> S_OK Then
    Raise exception.create('Error');

  GraphBuilder.QueryInterface( IID_IMediaControl,  MediaControl );
  GraphBuilder.QueryInterface( IID_IBasicAudio,    AudioControl );
  GraphBuilder.QueryInterface( IID_IMediaPosition, PositionControl );
  GraphBuilder.QueryInterface( IID_IMediaEventEx,  MediaEvent );
  MediaEvent.SetNotifyWindow(hwindow, WM_APP + 2, 0);

end;

constructor TAudioEngineDShow.Create;
begin
  fMuted := false;
  fSavedVolume := 0;
end;

function TAudioEngineDShow.Initialize: boolean;
begin

  Result := CreateAppWindow;
  Initialized := Result;
  if Initialized then
    Activate;
end;


Procedure TAudioEngineDShow.Release;
begin

  MediaEvent.SetNotifyWindow(0, 0, 0);
  MediaEvent      := nil;
  MediaControl    := nil;
  AudioControl    := nil;
  PositionControl := nil;
  GraphBuilder    := nil;

  inherited Destroy;
end;

destructor TAudioEngineDShow.Destroy;
begin

  Release;
  inherited Destroy;
end;

function TAudioEngineDShow.GetState: TEngineState;
var
  aState: TFilter_State;
begin

  Result := ENGINE_ON_LINE;
  if not Assigned(MediaControl) then
    exit;
  MediaControl.GetState(200,aState);

  case aState of
    State_Running: Result   := ENGINE_PLAY;
    State_Paused: Result    := ENGINE_PAUSE;
    State_Stopped: Result   := ENGINE_STOP;
    end;
end;

procedure TAudioEngineDShow.Pause;
begin
  MediaControl.Pause;
end;

Function TAudioEngineDShow.DoPlay(Song: TSong; offset:Integer):boolean;
var
  hr: HResult;
begin
  Activate;
  result := false;

  hr:= MediaControl.RenderFile( PWideChar( WideString( Song.FullName) ) );
  if not Succeeded(HR) then
    exit;

  hr:= MediaControl.Run;
  if not Succeeded(HR) then
    exit;

  Seek(offset, True);
  result:= true;
end;

procedure TAudioEngineDShow.SetMuted(const AValue: boolean);
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

function TAudioEngineDShow.GetMuted: boolean;
begin
  Result:=fMuted;
end;

class function TAudioEngineDShow.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  Result:= True;
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

class function TAudioEngineDShow.GetEngineName: String;
begin
  Result:='Direct Show';
end;

procedure TAudioEngineDShow.PostCommand(Command: TEngineCommand; Param: integer);
begin
  ReceivedCommand(Self, Command, Param);
end;

function TAudioEngineDShow.Playing: boolean;
begin
  Result := GetState = ENGINE_PLAY;
end;

function TAudioEngineDShow.Running: boolean;
begin
  Result := true  ;
end;

procedure TAudioEngineDShow.Seek(Seconds: integer; SeekAbsolute: boolean);
var
  currpos: integer;
begin
  currpos := GetSongPos;
  if SeekAbsolute then
    SetSongPos(Seconds * 1000)
  else
    SetSongPos(currpos + Seconds * 1000);

end;

procedure TAudioEngineDShow.Stop;
begin
  MediaControl.Stop;
end;

procedure TAudioEngineDShow.UnPause;
begin
  MediaControl.Run;

end;

initialization
  RegisterEngineClass(TAudioEngineDShow, 4, false, true);

end.
