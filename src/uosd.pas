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
    MovePoint: TPoint;
    procedure LoadFromConfig;
    {$IFDEF SUPPORT_SHAPING}
    procedure ShapeControl(AControl: TWinControl);
    {$ENDIF SUPPORT_SHAPING}
  Protected
    procedure Paint; override;
  public
    _top, _left:Integer;
    ConfigMode: boolean;
    Album: TLabel;
    Artist: TLabel;
    imgCover: TImage;
    Title: TLabel;
    Track: TLabel;

    timShow: TTimer;
    timPaint: TTimer;

    procedure UpdateAspect;
    Constructor Create(AOwner: Tcomponent); override;
    Destructor Destroy; override;
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
    fOSD := TfOSD.Create(application);
 {$IFDEF SUPPORT_SHAPING}
  fOSD.ShapeControl(fOSD);
 {$ENDIF SUPPORT_SHAPING}

  fOSD.Title.Caption := Song.tags.Title;
  fOSD.track.Caption := Song.Tags.TrackString;
  fOSD.Album.Caption := Song.Tags.Album;
  fOSD.Artist.Caption := Song.Tags.Artist;

  fOSD.LoadFromConfig;
  fOSD.UpdateAspect;

  fOSD.AlphaBlend := True;

  if Image <> nil then
    fOSD.imgCover.Picture.Assign(Image)
  else
  begin
    imgName := BackEnd.GetImageFromfolder(IncludeTrailingPathDelimiter(Song.FilePath));
    if imgName <> '' then
      fOSD.imgCover.Picture.LoadFromFile(imgName);
  end;

  fOSD.timPaint.Enabled := False;
  fOSD.timShow.Enabled := True;

  fOSD.Show;
end;



procedure ShowOSDConfig;
var
  imgName: string;
begin
  if fOSD = nil then
    fOSD := TfOSD.Create(application);

  fOSD.Title.Caption := DisplayAppName;
  fOSD.Artist.Caption := 'Drag to change position';
  fOSD.Track.Caption := '';
  fOSD.Album.Caption := '';

  imgName := BackEnd.Config.GetResourcesPath + 'nocover.png';

  if imgName <> '' then
    fOSD.imgCover.Picture.LoadFromFile(imgName);

  fOSD.AlphaBlendValue := 200;

  fOSD.LoadFromConfig;
  fOSD.ConfigMode := True;
  fOSD.UpdateAspect;

  fOSD.AlphaBlend := True;

  fOSD.Show;

end;

{ TfOSD }

procedure TfOSD.ShowAtPos(x: Integer; y: Integer);
begin
  if x + Width > Screen.Width then
  begin
    left := x - Width;
    if Left < 0 then Left := 0;
  end
  else
    left := x;

  if y + Height > Screen.Height then
  begin
    top := y - Height;
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
var
  i: integer;
begin

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

  imgCover:= TImage.Create(Self);
  with imgCover do begin
    Left := 11;
    Height := 89;
    Top := 8;
    Width := 102;
    Anchors := [];
    BorderSpacing.Left := 1;
    BorderSpacing.Top := 1;
    BorderSpacing.Right := 1;
    BorderSpacing.Bottom := 1;
    BorderSpacing.Around := 1;
    Enabled := False;
    OnMouseDown := @FormMouseDown;
    OnMouseEnter := @FormMouseEnter;
    OnMouseLeave := @FormMouseLeave;
    OnMouseMove := @FormMouseMove;
    OnMouseUp := @FormMouseUp;
    Stretch := True;
    Transparent := True;
    Parent := self;
  end;

  Title:= TLabel.create(self);
  with Title do begin
    Parent := self;
    Left := 123;
    Height := 20;
    Top := 8;
    Width := 285;
    AutoSize := False;
    Caption := 'Title';
    Color := 11955992;
    Font.Color := clBlack;
    Font.Height := 20;
    Font.Style := [fsBold];
    ParentColor := False;
    ParentFont := False;
    Transparent := true;
    OnMouseDown := @FormMouseDown;
    OnMouseMove := @FormMouseMove;
    OnMouseUp := @FormMouseUp;
    OnMouseEnter := @FormMouseEnter;
    OnMouseLeave := @FormMouseLeave;
    OptimalFill := True;
  end;

  Album:= TLabel.create(self);
  with Album do begin
    Parent := self;
    Left := 123;
    Height := 16;
    Top := 33;
    Width := 285;
    AutoSize := False;
    Caption := 'Album';
    Color := 11955992;
    Font.Color := clBlack;
    Font.Height := 16;
    ParentColor := False;
    ParentFont := False;
    Transparent := true;
    OnMouseDown := @FormMouseDown;
    OnMouseMove := @FormMouseMove;
    OnMouseUp := @FormMouseUp;
    OnMouseEnter := @FormMouseEnter;
    OnMouseLeave := @FormMouseLeave;
    OptimalFill := True;
  end;

  Artist:= TLabel.create(self);
  with Artist do begin
    Parent := self;
    Left := 123;
    Height := 16;
    Top := 57;
    Width := 285;
    AutoSize := False;
    Caption := 'Artist';
    Color := 11955992;
    Font.Color := clBlack;
    Font.Height := 16;
    ParentColor := False;
    ParentFont := False;
    Transparent := true;
    OnMouseDown := @FormMouseDown;
    OnMouseMove := @FormMouseMove;
    OnMouseUp := @FormMouseUp;
    OnMouseEnter := @FormMouseEnter;
    OnMouseLeave := @FormMouseLeave;
    OptimalFill := True;
  end;

  Track:= TLabel.create(self);
  with Track do begin
    Parent := self;
    Left := 123;
    Height := 17;
    Top := 81;
    Width := 285;
    AutoSize := False;
    Caption := '0';
    Color := 11955992;
    Font.Color := clBlack;
    Font.Height := 18;
    ParentColor := False;
    ParentFont := False;
    Transparent := true;
    OnMouseDown := @FormMouseDown;
    OnMouseMove := @FormMouseMove;
    OnMouseUp := @FormMouseUp;
    OnMouseEnter := @FormMouseEnter;
    OnMouseLeave := @FormMouseLeave;
    OptimalFill := True;
  end;

  {$IFDEF SUPPORT_SHAPING}
    ShapeControl(Self);
  {$ENDIF SUPPORT_SHAPING}
end;

procedure TfOSD.Paint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0,0,width,height));
  DrawGradientWindow(Canvas, Rect(0,0,width,height), height, Color);

end;

destructor TfOSD.Destroy;
begin
  timPaint.free;
  timShow.free;
  Album.free;
  Artist.free;
  imgCover.free;
  Title.free;
  Track.free;

  inherited Destroy;
end;


procedure TfOSD.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  punto: tpoint;
begin
  if ConfigMode then
    if (Tag = 1) then
    begin
      if (Top >= 0) and (Left >= 0) and (Left + Width <= Screen.DesktopWidth) and
        (Top + Height <= Screen.DesktopHeight) then
      begin
        punto := mouse.CursorPos;
        punto.X := max(punto.X - MovePoint.X, 0);
        punto.Y := max(punto.Y - MovePoint.Y, 0);
        SetBounds(punto.x, punto.y, Width, Height);
      end;
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

