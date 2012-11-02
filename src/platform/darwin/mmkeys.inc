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
uses LCLProc, AppConsts;

type

  { TkeyCaptureXGrab }

  TkeyCaptureXGrab = class (TKeyCapture)
  private
    SettingsActive :Boolean;
    fGrabbed: boolean;
    procedure GrabKeys(Grab: boolean);

  protected
   function GetGrabbed: Boolean; override;
  public
    Procedure BeginGrab; Override;
    Procedure EndGrab; Override;
  end;


procedure TkeyCaptureXGrab.GrabKeys(Grab:boolean);
begin
 fGrabbed := false;
end;

function TkeyCaptureXGrab.GetGrabbed: Boolean;
begin
  Result:=fGrabbed;
end;


procedure TkeyCaptureXGrab.BeginGrab;
begin

 GrabKeys(true);

end;

procedure TkeyCaptureXGrab.EndGrab;
begin
  GrabKeys(False);

end;

constructor TMultimediaKeys.Create(Mode:Integer);
begin
  KeyCapture := TkeyCaptureXGrab.Create;

  KeyCapture.Owner:= Self;
  KeyCapture.BeginGrab;
end;

destructor TMultimediaKeys.Destroy;
begin
 KeyCapture.EndGrab;
 KeyCapture.Free;
end;


