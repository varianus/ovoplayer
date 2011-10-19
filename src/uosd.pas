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

  TfOSD = class(TForm)
    Album: TLabel;
    Artist: TLabel;
    imgCover: TImage;
    timShow: TTimer;
    timPaint: TTimer;
    Title: TLabel;
    Track: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormShow(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure timShowTimer(Sender: TObject);
    procedure timPaintTimer(Sender: TObject);
    procedure TitleClick(Sender: TObject);
  private
    MovePoint: TPoint;
    procedure LoadFromConfig;
    {$IFDEF SUPPORT_SHAPING}
    procedure ShapeControl(AControl: TWinControl);
    {$ENDIF SUPPORT_SHAPING}
  public
    ConfigMode: boolean;
    procedure UpdateAspect;
  end;

var
  fOSD: TfOSD;

procedure ShowOSD(Song: TSong; Image: TPicture = nil);

procedure ShowOSDConfig;

implementation

uses AppConsts, lclproc, Math;

{$R *.lfm}

procedure ShowOSD(Song: TSong; Image: TPicture);
var
  imgName: string;
begin
  if fOSD = nil then
    fOSD := TfOSD.Create(application);

  fOSD.Title.Caption := Song.tags.Title;
  fOSD.track.Caption := Song.Tags.TrackString;
  fOSD.Album.Caption := Song.Tags.Album;
  fOSD.Artist.Caption := Song.Tags.Artist;
  if Image <> nil then
    fOSD.imgCover.Picture.Assign(Image)
  else
  begin
    imgName := BackEnd.GetImageFromfolder(IncludeTrailingPathDelimiter(Song.FilePath));
    if imgName <> '' then
      fOSD.imgCover.Picture.LoadFromFile(imgName);
  end;

  fOSD.LoadFromConfig;

  fOSD.AlphaBlend := True;

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

  fOSD.AlphaBlend := True;

  fOSD.Show;

end;

{ TfOSD }

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

  AlphaBlendValue := AlphaBlendValue - 8;
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

procedure TfOSD.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  fOSD := nil;
end;

procedure TfOSD.FormCreate(Sender: TObject);
begin
{$IFDEF SUPPORT_SHAPING}
  ShapeControl(Self);
{$ENDIF SUPPORT_SHAPING}

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
  Top := BackEnd.Config.NotificationParam.Y;
  Left := BackEnd.Config.NotificationParam.X;
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

