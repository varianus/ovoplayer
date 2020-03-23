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
{$I ovoplayer.inc}
unit iconloader;

interface

uses
  Classes, SysUtils, Graphics, fpimage, LCLType, IntfGraphics, GraphType, EasyLazFreeType, LazFreeTypeIntfDrawer, // LazFreeTypeFPImageDrawer,
  ImgList, Forms;

type

  { TIconRenderer }

  TIconRenderer = class
  private
    FColor: TColor;
    FFontData: TFreeTypeFont;
    FSize: integer;
    fImageSize: integer;
    procedure SetColor(AValue: TColor);
    procedure SetDefaults;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    function AddToImageList(imageList: TCustomImageList; Code: cardinal): integer; overload;
    function AddToImageList(imageList: TCustomImageList; const CodeList: array of cardinal): integer; overload;
    procedure SetSize(const AImageSize: integer; const FontSize: integer; AutoScale: boolean = True);
    property FontSize: integer read FSize;
    property ImageSize: integer read FimageSize;
    property Color: TColor read FColor write SetColor;
    function getIcon(iconCode: cardinal): TBitmap;
  end;


implementation

uses lazutf8, LCLIntf;

{ TIconRenderer }
function TIconRenderer.getIcon(iconCode: cardinal): TBitmap;
var
  image: TLazIntfImage;
  freeTypePainter: TIntfFreeTypeDrawer; //TFPImageFreeTypeDrawer;
  fontColor: TFPColor;
  utf8Value: string;
begin
  try
    Result := TBitmap.Create;
    image := TLazIntfImage.Create(0, 0, [riqfRGB, riqfAlpha]);
    freeTypePainter := TIntfFreeTypeDrawer.Create(image);
    utf8Value := UnicodetoUTF8(iconCode);
    fontColor := TColorToFPColor(FColor);
    try
      image.SetSize(FImageSize, FImageSize);
      freeTypePainter.FillPixels(colTransparent);
      freeTypePainter.DrawText(utf8Value, FFontData, FImageSize div 2, FImageSize div 2, fontColor, [ftaCenter, ftaVerticalCenter]);
      Result.LoadFromIntfImage(image);
    finally
      freeTypePainter.Free;
      image.Free;
    end;
  except
    Result := nil;
  end;
end;


procedure TIconRenderer.SetSize(const AImageSize: integer; const FontSize: integer; AutoScale: boolean = True);

begin
  FFontData.DPI := Screen.PixelsPerInch;
  if AutoScale then
  begin
    FImageSize := MulDiv(AImageSize, Screen.PixelsPerInch, 96);
    FSize := MulDiv(FontSize, Screen.PixelsPerInch, 96);
  end
  else
  begin
    FImageSize := AImageSize;
    FSize := FontSize;
  end;

  FFontData.SizeInPixels := FSize;

  SetDefaults;
end;

procedure TIconRenderer.SetDefaults;
begin
  FFontData.Hinted := False; // setting to true create strange artifact...
  FFontData.ClearType := True;
  FFontData.Quality := grqHighQuality;
  FFontData.SmallLinePadding := True;
end;

procedure TIconRenderer.SetColor(AValue: TColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;

end;

constructor TIconRenderer.Create(AStream: TStream);
begin
  FFontData := TFreeTypeFont.Create;
  FFontData.AccessFromStream(AStream, True);
end;

destructor TIconRenderer.Destroy;
begin
  FFontData.Free;
  inherited Destroy;
end;

function TIconRenderer.AddToImageList(imageList: TCustomImageList; Code: cardinal): integer;
var
  abmp: TBitmap;
begin
  abmp := getIcon(Code);
  try
    Result := imageList.Add(abmp, nil);
  finally
    abmp.Free;
  end;
end;

function TIconRenderer.AddToImageList(imageList: TCustomImageList; const CodeList: array of cardinal): integer;
var
  bmp: TBitmap;
  image: TLazIntfImage;
  freeTypePainter: TIntfFreeTypeDrawer;
  fontColor: TFPColor;
  utf8Value: string;
  i: integer;
begin
  Result := 0;
  try
    bmp := TBitmap.Create;
    image := TLazIntfImage.Create(0, 0, [riqfRGB, riqfAlpha]);
    freeTypePainter := TIntfFreeTypeDrawer.Create(image);
    fontColor := TColorToFPColor(FColor);
    try
      image.SetSize(FImageSize, FImageSize);
      for i := low(CodeList) to high(codeList) do
      begin
        utf8Value := UnicodetoUTF8(codelist[i]);
        freeTypePainter.FillPixels(colTransparent);
        freeTypePainter.DrawText(utf8Value, FFontData, FImageSize div 2, FImageSize div 2, fontColor, [ftaCenter, ftaVerticalCenter]);
        bmp.LoadFromIntfImage(image);
        imageList.Add(bmp, nil);
        Inc(Result);
      end;
    finally
      freeTypePainter.Free;
      image.Free;
      bmp.Free;
    end;
  except
    Result := -1;
  end;
end;


end.
