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
uses windows;

var
  WindowClassAtom: ATOM; // RegisterWindowClass yields an atom if successful.
  WindowClassInfo: WNDCLASSEX; // Class info Data structure for main window.
  WinClassName: string = 'Winamp v1.x';
  hwindow: HWND;

const
  VK_VOLUME_MUTE = 173;
  VK_VOLUME_DOWN = 174;
  VK_VOLUME_UP = 175;
  VK_MEDIA_NEXT_TRACK = 176;
  VK_MEDIA_PREV_TRACK = 177;
  VK_MEDIA_STOP = 178;
  VK_MEDIA_PLAY_PAUSE = 179;

type

  { TkeyCaptureWin }

  TkeyCaptureWin = class (TKeyCapture)
  private
    function CreateAppWindow: boolean;
  public
    Procedure BeginGrab; Override;
    Procedure EndGrab; Override;
  end;

{ TkeyCaptureWin }

function WinProc(hw: HWND; uMsg: UINT; wp: WPARAM; lp: LPARAM): LRESULT;
  stdcall; export;
var
  i: TKeyCaptureWin;
begin

  Result := 0;
  case uMsg of
    WM_HOTKEY : begin
                  i := TKeyCaptureWin(GetWindowLongPtr(HWindow,GWL_USERDATA));
                  if Assigned(i.owner.OnMMKey) then
                     i.Owner.OnMMKey(i.Owner, TEngineCommand(wp));
                end;
  else
      Result := DefWindowProc(hw, uMsg, wp, lp);
    end; // Case
end; // WinProc

function TkeyCaptureWin.CreateAppWindow: boolean;

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

procedure TkeyCaptureWin.BeginGrab;
begin
  CreateAppWindow;
  RegisterHotKey(hwindow, Longint(ecStop),     0, VK_MEDIA_STOP);
  RegisterHotKey(hwindow, Longint(ecPrevious), 0, VK_MEDIA_PREV_TRACK);
  RegisterHotKey(hwindow, Longint(ecPlay),     0, VK_MEDIA_PLAY_PAUSE);
  RegisterHotKey(hwindow, Longint(ecNext),     0, VK_MEDIA_NEXT_TRACK);
end;

procedure TkeyCaptureWin.EndGrab;
begin
  UnRegisterHotkey( hwindow, Longint(ecStop));
  UnRegisterHotkey( hwindow, Longint(ecPrevious));
  UnRegisterHotkey( hwindow, Longint(ecPlay));
  UnRegisterHotkey( hwindow, Longint(ecNext));
end;

constructor TMultimediaKeys.Create(Mode: Integer);
begin
  fMode := Mode;
  KeyCapture := TkeyCaptureWin.Create;
  KeyCapture.Owner:= Self;
  KeyCapture.BeginGrab;
end;

destructor TMultimediaKeys.Destroy;
begin
  KeyCapture.EndGrab;
  KeyCapture.Free;
end;

