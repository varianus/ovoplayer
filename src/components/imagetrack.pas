{
TimageTrack : a component which use image as trackbar
Copyright (C) 2011 Marco Caselli

This program is free software; you can redistribute it and/or
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
unit ImageTrack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, Graphics, Dialogs;

type

  { TImageTrack }

  TImageTrack = class(TCustomControl)
  private
    FBackGroundImage: TPicture;
    FForeGroundImage: TPicture;
    FMax: Cardinal;
    FMin: Cardinal;
    FPosition: Cardinal;
    Seeking : Boolean;
    fInternalBMP:TBitmap;
    procedure DrawInternalBMP;
    procedure SetBackGroundImage(const AValue: TPicture);
    procedure SetForeGroundImage(const AValue: TPicture);
    procedure SetMax(const AValue: Cardinal);
    procedure SetMin(const AValue: Cardinal);
    procedure SetPosition(const AValue: Cardinal);
    { Private declarations }
  protected
    Procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); Override;
    Procedure DoOnResize; Override;
    Procedure Loaded; Override;
  public
    Constructor Create(AOwner: Tcomponent); override;
  published
    Property Max: Cardinal read FMax write SetMax;
    Property Min: Cardinal read FMin write SetMin;
    Property Position: Cardinal read FPosition write SetPosition;
    Property BackGroundImage :TPicture read FBackGroundImage write SetBackGroundImage;
    Property ForeGroundImage :TPicture read FForeGroundImage write SetForeGroundImage;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MCaselli',[TImageTrack]);
end;

{ TImageTrack }

procedure TImageTrack.SetBackGroundImage(const AValue: TPicture);
begin
  if FBackGroundImage=AValue then exit;
  FBackGroundImage:=AValue;
  if fInternalBMP.Width = 0 then
     fInternalBMP.SetSize(AValue.Width, AValue.Height);

  DrawInternalBMP;
end;

procedure TImageTrack.SetForeGroundImage(const AValue: TPicture);
begin
  if FForeGroundImage=AValue then exit;
  FForeGroundImage:=AValue;
  if fInternalBMP.Width = 0 then
     fInternalBMP.SetSize(AValue.Width, AValue.Height);

  DrawInternalBMP;
end;

procedure TImageTrack.SetMax(const AValue: Cardinal);
begin
  if FMax=AValue then exit;
  if FMax <= FMin then exit;
  FMax:=AValue;
  DrawInternalBMP;
end;

procedure TImageTrack.SetMin(const AValue: Cardinal);
begin
  if FMin=AValue then exit;
  if FMin >= FMax then exit;
  FMin:=AValue;
  DrawInternalBMP;
end;

procedure TImageTrack.SetPosition(const AValue: Cardinal);
begin
  if FPosition=AValue then exit;
  FPosition:=AValue;
  if FPosition > fmax then FPosition:=FMax;
  if FPosition < FMin then FPosition:=FMin;

  DrawInternalBMP;
end;

procedure TImageTrack.DrawInternalBMP;
var
  r1: TRect;
begin
  if (FBackGroundImage = nil) or
     (FForeGroundImage = nil) then
    Inherited Paint;

  fInternalBMP.Canvas.Clear;
  r1:=Rect(0, 0, fInternalBMP.Width, fInternalBMP.Height);
  fInternalBMP.Canvas.CopyRect(r1,FBackGroundImage.Bitmap.Canvas,r1);

  r1:=Rect(0,0, trunc(fInternalBMP.width *(FPosition /FMax))-1,fInternalBMP.Height );

  fInternalBMP.Canvas.CopyRect(r1,FForeGroundImage.Bitmap.Canvas,r1);
  Repaint;
end;

procedure TImageTrack.Paint;
begin
  Canvas.StretchDraw(rect(0,0,Width, Height), fInternalBMP);
end;

procedure TImageTrack.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  newPosition: integer;
begin
  Paint;
  if ssLeft in Shift then
    begin
      Seeking     := True;
      Cursor := crHSplit;
      newPosition := round(x * Max / Width);
      Position:= newPosition;
      DrawInternalBMP;
    end
  else
    begin
      Cursor := crDefault;
      Seeking := False;
    end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TImageTrack.DoOnResize;
begin
  inherited DoOnResize;
  DrawInternalBMP;
end;

procedure TImageTrack.Loaded;
begin
  inherited Loaded;
  fInternalBMP.SetSize(FForeGroundImage.Width, FForeGroundImage.Height);
  DrawInternalBMP;
end;

constructor TImageTrack.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FMin := 0;
  FMax := 100;
  FPosition := 50;

  FBackGroundImage:= TPicture.Create;
  FForeGroundImage:= TPicture.Create;
  fInternalBMP := TBitmap.Create;
  fInternalBMP.SetSize(0,0);
  fInternalBMP.Transparent:=true;
end;

end.
