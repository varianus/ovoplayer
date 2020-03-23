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
{
 based on code from http://www.drbob42.com/examine/examinC5.htm
}
{$I codegen.inc}
{$I ovoplayer.inc}
unit taskbar_ext;

interface

uses
  Windows, Classes, SysUtils, shlobj, comobj, coreinterfaces, controls, Forms, udm,
  guibackend, LazUTF8;

const
      IID_ITaskbarList3: TGUID = '{ea1afb91-9e28-4b86-90e9-9e9f8a5eefaf}';
type

    { TTaskBarExtender }

    TTaskBarExtender = class (TObject, IObserver)
    private
      FInitialized: boolean;
      TaskbarList : ITaskBarList;
      TaskbarList3 : ITaskBarList3;
      FBackEnd: IBackEnd;
      FApplication: THandle;
      Buttons: array [0..2] of THUMBBUTTON;
      ImgL: TimageList;
      Procedure AddButtons;

    public
      constructor Create;
      destructor Destroy; override;
      Function Init:boolean;
      procedure Update;
      Procedure UnInit;

      procedure UpdateProperty(Kind: TChangedProperty);

      Property Initialized: boolean read FInitialized;
  end;


implementation
uses
  BaseTypes;
var
  PrevWndProc: Ptrint;
  Handler: TTaskBarExtender;

{ TTaskBarExtender }

function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam):LRESULT; stdcall;
begin
  if uMsg=WM_COMMAND then
  begin
    if HIWORD(wParam) = THBN_CLICKED then
      case LOWORD(wParam) of
        41: Handler.FBackEnd.Previous;
        42: if Handler.FBackEnd.Status = ENGINE_PLAY then
             Handler.FBackEnd.Pause
           else
             Handler.FBackEnd.Play;
        43: Handler.FBackEnd.Next;
      end;
  end;
  result:=CallWindowProc(WNDPROC(PrevWndProc), Ahwnd, uMsg, WParam, LParam);
end;

procedure TTaskBarExtender.AddButtons;
var
 i:Integer;
 res: HRESULT;
begin
   for i := 0 to 2 do
     begin
       Buttons[i].dwMask := THB_FLAGS or THB_BITMAP or THB_TOOLTIP;
       Buttons[i]. dwFlags := THBF_ENABLED  or THBF_NOBACKGROUND;
     end;

   Buttons[0].iId := 41;
   Buttons[0].iBitmap := 0;
   StrCopy(Buttons[0].szTip, PWideChar(utf8toUtf16(DM.actPrevious.Caption)));

   Buttons[1].iId := 42;
   if fBackEnd.Status = ENGINE_PLAY then
     begin
         Buttons[1].iBitmap := 2;
         StrCopy(Buttons[1].szTip, PWideChar(utf8toUtf16(DM.actPlay.Caption)));
     end
   else
     begin
         Buttons[1].iBitmap := 1;
         StrCopy(Buttons[1].szTip, PWideChar(utf8toUtf16(DM.actPause.Caption)));
     end;

   Buttons[2].iId := 43;
   Buttons[2].iBitmap := 3;
   StrCopy(Buttons[2].szTip, PWideChar(utf8toUtf16(DM.actNext.Caption)));

   res:=TaskbarList3.ThumbBarSetImageList(FApplication, imgl.Reference[imgl.Height].Handle);
   res:=TaskbarList3.ThumbBarAddButtons(FApplication, 3, @Buttons[0]);
   res:= res;
   Update;
end;

constructor TTaskBarExtender.Create;
var
 res:HRESULT;
 x, y: integer;
begin
  imgl:= nil;
  FInitialized:=false;
  if not CheckWin32Version(6,1) then
    begin
      exit;
    end;
  TaskbarList := CreateComObject(CLSID_TaskbarList) as ITaskbarList;
  res:=TaskbarList.HrInit;
  TaskbarList._AddRef;
  x:= GetSystemMetrics(SM_CXICON);
  y:=GetSystemMetrics(SM_CyICON) ;
  Imgl:= TImageList.CreateSize(x,y);
  Dm.CustomRender(ImgL, TSize.Create(x,y), [$e807,$e803, $e802, $e805]);
  res:=ord(Supports(TaskbarList, IID_ITaskbarList3, TaskbarList3));
  Handler:=self;

end;

destructor TTaskBarExtender.Destroy;
begin
  if FInitialized then
    UnInit;
  FreeAndNil(imgl);
  inherited Destroy;
end;

function TTaskBarExtender.Init:boolean;
begin
  FBackEnd:=BackEnd;
  if Application.MainFormOnTaskBar then
    FApplication := Application.MainFormHandle
  else
    FApplication := Application.Handle;

  if FApplication <> 0 then
    begin
      FInitialized:=true;
      fBackEnd.Attach(Self);
      Result := True;
      PrevWndProc := Ptrint(GetWindowLongPtr(fApplication,GWL_WNDPROC));
      SetWindowLongPtr(fApplication,GWL_WNDPROC,PtrInt(@WndCallback));
      AddButtons;
    end
  else
    Result := False;
end;

procedure TTaskBarExtender.Update;
begin
  if not Initialized then
      Init;

  TaskbarList3.ThumbBarUpdateButtons(FApplication, 3, @Buttons[0]);
end;

procedure TTaskBarExtender.UnInit;
begin
  SetWindowLongptr(fApplication,GWL_WNDPROC, PrevWndProc);
  FApplication:=0;

  fBackEnd.Remove(Self);
  FInitialized:=false;
end;

procedure TTaskBarExtender.UpdateProperty(Kind: TChangedProperty);
begin
  case kind of
  cpStatus:
    begin
      if fBackEnd.Status = ENGINE_PLAY then
        begin
             Buttons[1].iBitmap := 2;
             StrCopy(Buttons[1].szTip, PWideChar(utf8toUtf16(DM.actPause.Caption)));
        end
      else
        begin
             Buttons[1].iBitmap := 1;
             StrCopy(Buttons[1].szTip, PWideChar(utf8toUtf16(DM.actPlay.Caption)));
        end;
      Update;
    end;

  end;

end;

end.

