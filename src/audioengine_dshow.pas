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
unit audioengine_DShow;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, AudioEngine, mediadshow, Song, lclproc;

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
    procedure DoPlay(Song: TSong; offset:Integer); override;
    procedure SetMuted(const AValue: boolean);  override;
    Function GetMuted: boolean; override;
    procedure ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0); override;
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
uses windows, math;

var

  WindowClassAtom: ATOM; // RegisterWindowClass yields an atom if successful.
  WindowClassInfo: WNDCLASSEX; // Class info Data structure for main window.
  WinClassName: string = 'ovoplayerdsmsg';
  hwindow: HWND;

{ TAudioEngineDShow }

function GetBasicAudioVolume(Value : integer) : integer;
begin
  Result := Round(Power(10,Value / 2500) * 10001 - 1);
end;

function SetBasicAudioVolume(Value : integer) : integer;
begin
  Result := Round(Log10((Value+1) / 10000) * 2500);
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
    if not RegisterWindowClass then
      begin
       exit;
      end;
    HWindow := CreateWindowEx(WS_EX_NOACTIVATE or WS_EX_TRANSPARENT,
      PChar(WinClassName), PChar(WinClassName), Ws_popup or WS_CLIPSIBLINGS, 0,
      0, 0, 0, 0, 0, HInstance, nil);
    if HWindow <> 0 then
      begin
      ShowWindow(HWindow, SW_HIDE);
      SetWindowLongPtr(HWindow,GWL_USERDATA,PtrInt(Self));
      UpdateWindow(HWindow);
      Result := True;
      Exit;
      end;

    Result := False;
  end; // CreateAppWindow

function TAudioEngineDShow.GetMainVolume: integer;
begin
  Result := -1;
  AudioControl.get_Volume( Result) ;
  result := GetBasicAudioVolume(Result);
end;

procedure TAudioEngineDShow.SetMainVolume(const AValue: integer);
begin
  AudioControl.put_Volume( SetBasicAudioVolume(Avalue));
end;

function TAudioEngineDShow.GetMaxVolume: integer;
begin
  Result:=10000;
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
  inherited Create;
  CreateAppWindow;
  fMuted := false;
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
  State: TFilter_State;
begin

  Result := ENGINE_ON_LINE;
  MediaControl.GetState(200,State);

  case State of
    State_Running: Result   := ENGINE_PLAY;
    State_Paused: Result    := ENGINE_PAUSE;
    State_Stopped: Result   := ENGINE_STOP;
    end;
end;

procedure TAudioEngineDShow.Pause;
begin
  MediaControl.Pause;
end;

procedure TAudioEngineDShow.DoPlay(Song: TSong; offset:Integer);
begin
  Activate;
  MediaControl.RenderFile( PWideChar( WideString( Song.FullName) ) );
  MediaControl.Run;
  Seek(offset, True);
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

procedure TAudioEngineDShow.ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0);
begin
  case Command of
    ecNext: if Assigned(OnSongEnd) then
        OnSongEnd(Self);

    ecSeek: Seek(Param, True);

    end;
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
