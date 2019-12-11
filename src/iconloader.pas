unit iconloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics, fpimage, LCLType, IntfGraphics, GraphType, EasyLazFreeType,
  LazFreeTypeIntfDrawer, imglist;


Type

  { TIconRenderer }

  TIconRenderer = class
  private
    FColor: TColor;
    FFontData:TFreeTypeFont;
    FSize: Integer;
    function getIcon(iconCode: Cardinal): TBitmap;
    procedure SetColor(AValue: TColor);
    procedure SetDefaults;
    procedure SetSize(AValue: Integer);

  public
    Constructor Create(AStream: TStream);
    destructor Destroy; override;
    function AddToImageList(imageList: TCustomImageList; Code:Cardinal): integer;
    property Size:Integer read FSize write SetSize;
    property Color:TColor read FColor write SetColor;

  end;


implementation
uses lazutf8, LCLIntf;

{ TIconRenderer }
function TIconRenderer.getIcon(iconCode: Cardinal): TBitmap;
var
  image: TLazIntfImage;
  freeTypePainter: TIntfFreeTypeDrawer;
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
      image.SetSize(FSize, FSize);
      freeTypePainter.FillPixels(colTransparent);
      freeTypePainter.DrawText(utf8Value, FFontData, FSize, FSize, fontColor, [ftaRight, ftaBottom]);
      Result.LoadFromIntfImage(image);
    finally
      freeTypePainter.Free;
      image.Free;
    end;
  except
    Result := nil;
  end;
end;


procedure TIconRenderer.SetSize(AValue: Integer);
begin
  if FSize = AValue then Exit;
  FSize := AValue;
  FFontData.SizeInPixels := FSize -1;
  SetDefaults;
end;

procedure TIconRenderer.SetDefaults;
begin
  FFontData.Hinted := false;
  FFontData.ClearType := True;
  FFontData.Quality := grqHighQuality;
  FFontData.SmallLinePadding := False;
end;

procedure TIconRenderer.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
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

function TIconRenderer.AddToImageList(imageList: TCustomImageList; Code: Cardinal): integer;
var
  abmp:TBitmap;
begin
  abmp := getIcon(Code);
  try
    Result := imageList.Add(abmp, nil);
  finally
    abmp.Free;
  end;
end;

end.

