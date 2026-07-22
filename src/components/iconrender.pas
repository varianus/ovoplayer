unit iconrender;

interface

uses
  Classes, SysUtils, Graphics, fpimage, LCLType, IntfGraphics, GraphType, EasyLazFreeType, LazFreeTypeIntfDrawer;

type

  { TIconRenderer }

  TIconRenderer = class
  private
    FColor: TColor;
    FFontData: TFreeTypeFont;
    FSize: integer;
    fImageSize: integer;
    function GetCount: integer;
    procedure SetColor(AValue: TColor);
    procedure SetDefaults;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure SetSize(const AImageSize: integer; const FontSize: integer; AutoScale: boolean = True);
    property FontSize: integer read FSize;
    property ImageSize: integer read FimageSize;
    property Color: TColor read FColor write SetColor;
    function getIcon(CharCode: cardinal; Image: TBitmap): boolean;
    function getGlyph(iconIndex: cardinal; Image: TBitmap): boolean;
    property Count: integer read GetCount;
  end;


implementation

uses lazutf8, LCLIntf;


{ TIconRenderer }

function TIconRenderer.getGlyph(iconIndex: cardinal; Image: TBitmap):boolean;
var
  IntImage: TLazIntfImage;
  freeTypePainter: TIntfFreeTypeDrawer; //TFPImageFreeTypeDrawer;
  fontColor: TFPColor;
begin
  try

    IntImage := TLazIntfImage.Create(0, 0, [riqfRGB, riqfAlpha]);
    freeTypePainter := TIntfFreeTypeDrawer.Create(IntImage);
    fontColor := TColorToFPColor(FColor);
    try
      IntImage.SetSize(FImageSize, FImageSize);
      freeTypePainter.FillPixels(colTransparent);
      freeTypePainter.DrawGlyph(iconIndex, FFontData, FImageSize div 2, FImageSize div 2, fontColor, [ftaCenter, ftaVerticalCenter]);
      Image.LoadFromIntfImage(IntImage);
       Result := True;
    finally
      freeTypePainter.Free;
      IntImage.Free;
    end;
  except
    Result := False;
  end;
end;


function TIconRenderer.getIcon(CharCode: cardinal; Image: TBitmap):boolean;
var
  IntImage: TLazIntfImage;
  freeTypePainter: TIntfFreeTypeDrawer; //TFPImageFreeTypeDrawer;
  fontColor: TFPColor;
  utf8Value: string;
begin
  try
    IntImage := TLazIntfImage.Create(0, 0, [riqfRGB, riqfAlpha]);
    freeTypePainter := TIntfFreeTypeDrawer.Create(IntImage);
    utf8Value := UnicodetoUTF8(CharCode);
    fontColor := TColorToFPColor(FColor);
    try
      IntImage.SetSize(FImageSize, FImageSize);
      freeTypePainter.FillPixels(colTransparent);
      freeTypePainter.DrawText(utf8Value, FFontData, FImageSize div 2, FImageSize div 2, fontColor, [ftaCenter, ftaVerticalCenter]);
      Image.LoadFromIntfImage(IntImage);
      REsult := true;
    finally
      freeTypePainter.Free;
      IntImage.Free;
    end;
  except
    Result := False;
  end;
end;


procedure TIconRenderer.SetSize(const AImageSize: integer; const FontSize: integer; AutoScale: boolean = True);
var
  DPI : integer;
begin
  DPI := 96;
///  FFontData.DPI := Screen.PixelsPerInch;
  FFontData.DPI := DPI;

  if AutoScale then
  begin
    FImageSize := MulDiv(AImageSize, DPI, 96);
    FSize := MulDiv(FontSize, DPI, 96);
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
  FFontData.Hinted := false; // setting to true create strange artifact...
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

function TIconRenderer.GetCount: integer;
begin
  Result := FFontData.GlyphCount;
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

end.
