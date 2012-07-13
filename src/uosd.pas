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
unit uOSD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, song, GUIBackEnd;

type

  { TfOSD }

  TfOSD = class(THintWindow)
  private
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormShow(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure ShowAtPos(x: Integer; y: Integer);
    procedure timShowTimer(Sender: TObject);
    procedure timPaintTimer(Sender: TObject);
    procedure TitleClick(Sender: TObject);
  private
    fBitmap: TBitmap;
    MovePoint: TPoint;
    imgCover: TPicture;

    fTitle : string;
    ftrack : string;
    fAlbum : string;
    fArtist : string;

    procedure LoadFromConfig;
    procedure InternalPaint;
    {$IFDEF SUPPORT_SHAPING}
    procedure ShapeControl(AControl: TWinControl);
    {$ENDIF SUPPORT_SHAPING}
  public
    _top, _left:Integer;
    ConfigMode: boolean;

    timShow: TTimer;
    timPaint: TTimer;

    BackGroundColor: TColor;
    FontColor : TColor;
    procedure UpdateAspect;
    procedure Paint; override;
    Constructor Create(AOwner: Tcomponent); override;
    Destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  end;

var
  fOSD: TfOSD;

procedure ShowOSD(Song: TSong; Image: TPicture = nil);

procedure ShowOSDConfig;

implementation

uses AppConsts, lclproc, Math, graphutil;

procedure ShowOSD(Song: TSong; Image: TPicture);
var
  imgName: string;
begin
  if fOSD = nil then
    fOSD := TfOSD.Create(nil);

 {$IFDEF SUPPORT_SHAPING}
  fOSD.ShapeControl(fOSD);
 {$ENDIF SUPPORT_SHAPING}


  fOSD.fTitle := Song.tags.Title;
  fOSD.ftrack := Song.Tags.TrackString;
  fOSD.fAlbum := Song.Tags.Album;
  fOSD.fArtist := Song.Tags.Artist;

  fOSD.AlphaBlend := True;

  if Image <> nil then
    begin
      fosd.imgCover.Assign(Image);
    end

  else
  begin
    imgName := BackEnd.GetImageFromfolder(IncludeTrailingPathDelimiter(Song.FilePath));
    if imgName <> '' then
      fOSD.imgCover.LoadFromFile(imgName);
  end;

  fOSD.timPaint.Enabled := False;
  fOSD.timShow.Enabled := True;
  fOSD.LoadFromConfig;
  fOSD.UpdateAspect;
  fOSD.InternalPaint;
  fOSD.Show;
end;



procedure ShowOSDConfig;
var
  imgName: string;
begin
  if fOSD = nil then
    fOSD := TfOSD.Create(application);

  fOSD.fTitle := DisplayAppName;
  fOSD.fAlbum := rDragToChangePosition;
  fOSD.ftrack := '';
  fOSD.fArtist := '';

  imgName := BackEnd.Config.GetResourcesPath + 'nocover.png';

  if imgName <> '' then
    fOSD.imgCover.LoadFromFile(imgName);

  fOSD.AlphaBlendValue := 200;

  fOSD.LoadFromConfig;
  fOSD.ConfigMode := True;
  fOSD.UpdateAspect;
  fOSD.InternalPaint;

  fOSD.AlphaBlend := True;

  fOSD.Show;

end;

{ TfOSD }

procedure TfOSD.ShowAtPos(x: Integer; y: Integer);
begin
  if x + Width > Screen.DeskTopWidth then
  begin
    left := x - Width -1;
    if Left < 0 then Left := 0;
  end
  else
    left := x;

  if y + Height > Screen.DeskTopHeight then
  begin
    top := y - Height -1;
    if top < 0 then top := 0;
  end
  else
    top := y;

  Show;
end;
procedure TfOSD.timShowtimer(Sender: TObject);
begin
  timShow.Enabled := False;

  AlphaBlend := True;
  Application.ProcessMessages;
  timPaint.Enabled := True;
end;

procedure TfOSD.timPaintTimer(Sender: TObject);
begin
  if AlphaBlendValue < 50 then
  begin
    timPaint.Enabled := False;
    Close;
  end;
  timPaint.Interval:=40;
  AlphaBlendValue := AlphaBlendValue - 8;
  Application.ProcessMessages;
end;

procedure TfOSD.TitleClick(Sender: TObject);
begin
  if not ConfigMode then
    Close;
end;

procedure TfOSD.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  p: TPoint;
begin
  if ConfigMode then
  begin
    p.x := x;
    p.y := y;
    p := fOSD.ScreenToClient(tcontrol(Sender).ClientToScreen(p));
    MovePoint.X := p.X;
    MovePoint.Y := p.Y;
    fosd.Cursor := crdrag;
    Tag := 1;
  end
  else
    hide;
end;

procedure TfOSD.FormMouseEnter(Sender: TObject);
begin
  if not ConfigMode then
    AlphaBlendValue := BackEnd.Config.NotificationParam.Transparency div 2;
end;

procedure TfOSD.FormMouseLeave(Sender: TObject);
begin
  if not ConfigMode then
    AlphaBlendValue := BackEnd.Config.NotificationParam.Transparency;
end;

{$IFDEF SUPPORT_SHAPING}
procedure TfOSD.ShapeControl(AControl: TWinControl);
var
  ABitmap: TBitmap;
  ARect: TRect;
begin
  ABitmap := TBitmap.Create;
  ABitmap.Monochrome := True;
  ABitmap.Width := AControl.clientwidth;
  ABitmap.Height := AControl.clientHeight;
  with ABitmap.Canvas do
  begin
    Pen.Color := clFuchsia; // mask color
    Brush.Color := clBlack; // transparent color
    FillRect(0, 0, ABitmap.Width, ABitmap.Height);
    Brush.Color := clWhite; // mask color
    Pen.Color := clWhite; // mask color
    RoundRect(2, 2, ABitmap.Width, ABitmap.Height, 15, 15);
    FloodFill(60, 60, clWhite, fsBorder);
    ABitmap.Mask(clWhite);
  end;
  AControl.SetShape(ABitmap);
  ABitmap.Free;
end;

{$ENDIF SUPPORT_SHAPING}

procedure TfOSD.LoadFromConfig;
var
  i: integer;
begin
  timShow.Interval := BackEnd.Config.NotificationParam.TimeOut;
  timPaint.Interval:=40;
  Top := BackEnd.Config.NotificationParam.Y;
  Left := BackEnd.Config.NotificationParam.X;
  Width:=430;
  Height:=103;
  Color := BackEnd.Config.NotificationParam.BackColor;
  AlphaBlendValue := BackEnd.Config.NotificationParam.Transparency;
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TStaticText then
    begin
      TStaticText(Components[i]).color := color;
      TStaticText(Components[i]).Font.color :=
        BackEnd.Config.NotificationParam.FontColor;
    end;
end;

procedure TfOSD.UpdateAspect;
begin
  BackGroundColor := BackEnd.Config.NotificationParam.BackColor;
  AlphaBlendValue := BackEnd.Config.NotificationParam.Transparency;
  FontColor := BackEnd.Config.NotificationParam.FontColor;

end;

procedure TfOSD.InternalPaint;
var
  ARect : TRect;
  leftPos : integer;
begin
  fBitmap.SetSize(Width, Height);
  fBitmap.Canvas.Brush.Style := bsSolid;
  fBitmap.Canvas.Brush.Color := Color;
  ARect := Rect(0,0,fBitmap.width,fBitmap.height);
  fBitmap.Canvas.Frame3D(ARect, GetShadowColor(Color), GetHighLightColor(Color), 2);
  DrawGradientWindow(fBitmap.Canvas, ARect, height, Color);

  if Assigned(imgCover) then
     fBitmap.Canvas.StretchDraw(Rect(11,8,113, 97), imgCover.Graphic);

  LeftPos := 123;

  fBitmap.Canvas.Font.Assign(Font);
  fBitmap.Canvas.Font.Color:= FontColor;
  fBitmap.Canvas.Font.Style := [fsBold];
  fBitmap.Canvas.Brush.Style:=bsClear;
  fBitmap.Canvas.Font.Height := 20;
  fBitmap.Canvas.TextOut(leftPos,8, fTitle);

  fBitmap.Canvas.Font.Height := 16;
  fBitmap.Canvas.Font.Style := [];
  fBitmap.Canvas.TextOut(leftPos,33, fAlbum);
  fBitmap.Canvas.TextOut(leftPos,57, fArtist);
  fBitmap.Canvas.TextOut(leftPos,81, ftrack);

  Repaint;

end;

constructor TfOSD.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  timPaint:= TTimer.Create(self);
  timPaint.Enabled:=false;
  timPaint.OnTimer:=@timPaintTimer;
  timPaint.Interval:=40;
  timShow:= TTimer.Create(self);
  timShow.Enabled:=false;
  timShow.OnTimer:=@timShowTimer;

  imgCover:= TPicture.Create;
  BorderWidth:=1;
  fBitmap := TBitmap.Create;
  fBitmap.SetSize(Width, Height);

  OnMouseDown := @FormMouseDown;
  OnMouseMove := @FormMouseMove;
  OnMouseUp := @FormMouseUp;
  OnMouseEnter := @FormMouseEnter;
  OnMouseLeave := @FormMouseLeave;

  {$IFDEF SUPPORT_SHAPING}
    ShapeControl(Self);
  {$ENDIF SUPPORT_SHAPING}
end;

procedure TfOSD.Paint;
begin
   self.Canvas.CopyRect(rect(0,0, Width, Height), fBitmap.Canvas, rect(0,0, fBitmap.Width, fBitmap.Height));
end;

destructor TfOSD.Destroy;
begin
  timPaint.free;
  timShow.free;
  imgCover.free;
  fBitmap.free;
  inherited Destroy;
end;

procedure TfOSD.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  if (ALeft + AWidth) > Screen.DesktopWidth then
      ALeft := Screen.DesktopWidth - ( AWidth +1);

  if (ATop + AHeight) > Screen.DesktopHeight then
      ATop := Screen.DesktopHeight - ( AHeight +1);

  if ALeft < 1 then
     ALeft:= 1;

  if ATop < 1 then
     ATop:= 1;

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;


procedure TfOSD.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  punto: tpoint;
begin
  if ConfigMode then
    if (Tag = 1) then
    begin
      punto := mouse.CursorPos;
      punto.X := max(punto.X - MovePoint.X, 0);
      punto.Y := max(punto.Y - MovePoint.Y, 0);
      SetBounds(punto.x, punto.y, Width, Height);
    end;
end;

procedure TfOSD.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  Tag := 0;
  fosd.Cursor := crDefault;
end;

procedure TfOSD.FormShow(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TLabel then
      if Tlabel(Components[i]).OptimalFill then
        Tlabel(Components[i]).AdjustFontForOptimalFill;

end;

procedure TfOSD.bCloseClick(Sender: TObject);
begin
  Close;
end;

initialization
  fOSD := nil;
end.
