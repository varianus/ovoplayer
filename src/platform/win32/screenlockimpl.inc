{%MainUnit ../../screenlock.pas}
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
uses windows, jwawinuser, JwaWtsApi32;

var
  WindowClassAtom: ATOM; // RegisterWindowClass yields an atom if successful.
  WindowClassInfo: WNDCLASSEX; // Class info Data structure for main window.
  WinClassName: string = 'Ovoplayer screen Lock';
  hwindow: HWND;


function WinProc(hw: HWND; uMsg: UINT; wp: WPARAM; lp: LPARAM): LRESULT;
  stdcall; export;
var
 obj :TScreenLockHandler;
begin

  Result := 0;
  case uMsg of
     WM_WTSSESSION_CHANGE : begin
                  obj := TScreenLockHandler(GetWindowLongPtr(HWindow,GWL_USERDATA));
                  if Assigned(obj.fBackEnd) then
                    begin
                      if (wp = $6) or (wp = $7) then
                        begin
                           if obj.fBackEnd.GetStatus = ENGINE_PLAY then
                            begin
                              obj.fBackEnd.HandleCommand(ecPause);
                              obj.fPreviousState := ENGINE_PLAY;
                            end
                           else
                             obj.fPreviousState := ENGINE_OFF_LINE;
                        end;

                      if (wp = $5) or (wp = $8) and (obj.fPreviousState = ENGINE_PLAY) then
                        obj.fBackEnd.HandleCommand(ecPlay);
                    end;
                end;

    else
      Result := DefWindowProc(hw, uMsg, wp, lp);
    end; // Case
end; // WinProc

function CreateAppWindow(obj:pointer): boolean;

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
  Result := false;

  if not RegisterWindowClass then
     exit;

  HWindow := CreateWindowEx(WS_EX_NOACTIVATE or WS_EX_TRANSPARENT,
    PChar(WinClassName), PChar(WinClassName), Ws_popup or WS_CLIPSIBLINGS, 0,
    0, 0, 0, 0, 0, HInstance, nil);

  if HWindow <> 0 then
    begin
    ShowWindow(HWindow, SW_HIDE);
    SetWindowLongPtr(HWindow,GWL_USERDATA,PtrInt(obj));
    UpdateWindow(HWindow);
    Result := True;
    end;
end; //

procedure TScreenLockHandler.SetActive(AValue: boolean);
begin
  if fActive = AValue then Exit;
  if AValue then
    Init
  else
    Done;

  fActive := AValue;

end;

constructor TScreenLockHandler.Create(BackEnd: IBackEnd);
begin
  fBackEnd := BackEnd;
  fActive := false;
end;

procedure TScreenLockHandler.Init;
begin
  CreateAppWindow(self);
  fActive := WTSRegisterSessionNotification(HWindow, NOTIFY_FOR_ALL_SESSIONS);
end;

procedure TScreenLockHandler.Done;
begin
  WTSUnRegisterSessionNotification(HWindow);
  DestroyWindow(hwindow);
  fActive := False;
end;

destructor TScreenLockHandler.Destroy;
begin
  if fActive then
    Done;
  inherited Destroy;
end;

