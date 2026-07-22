unit FontImageList;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ImgList, Controls, Graphics, iconrender, types, lcltype, LCLIntf;

type
  TFontSource = (fsrcFileName, fsrcResource);

  TFontImageData = record
    GliphIndex: integer;
    Color: TColor;
  end;

  TFontImageDataArray = array of TFontImageData;

  { TMyFontImageList }

  { TFontImageList }

  TFontImageList = class(TCustomImageList)
  private
    FFontName: TFileName;
    FFontSource: TFontSource;
    FIsLoading: boolean;
    FGlyphs: TFontImageDataArray;
    FOnInitialize: TNotifyEvent;
    FSize: TSize;
    procedure SetFontName(AValue: TFileName);
    procedure SetFontSource(AValue: TFontSource);
    procedure SetGlyphs(AValue: TFontImageDataArray);
    procedure SetOnInitialize(AValue: TNotifyEvent);
    procedure SetSize(AValue: TSize);
  protected
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetWidth(AValue: integer);
    procedure SetHeight(AValue: integer);
    procedure RegenerateImages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteGlyphData(AStream: TStream);
    procedure ReadGliphData(AStream: TStream);
    procedure WriteData(AStream: TStream); override;
    procedure ReadData(AStream: TStream); override;
    procedure WriteAdvData(AStream: TStream); override;
    procedure ReadAdvData(AStream: TStream); override;


    property Glyphs: TFontImageDataArray read FGlyphs write SetGlyphs;
  published
    property FontName: TFileName read FFontName write SetFontName;
    property FontSource: TFontSource read FFontSource write SetFontSource;
    property OnInitialize: TNotifyEvent read FOnInitialize write SetOnInitialize;
    property Height write SetHeight;
    property Width write SetWidth;

  end;

{ Procedural functions for TFontImageDataArray manipulation }
function AddGlyph(var AArray: TFontImageDataArray; AGliphIndex: integer; AColor: TColor): integer;
procedure RemoveGlyph(var AArray: TFontImageDataArray; AIndex: integer);
procedure MoveGlyphUp(var AArray: TFontImageDataArray; AIndex: integer);
procedure MoveGlyphDown(var AArray: TFontImageDataArray; AIndex: integer);

implementation

type
  TComponentCracker = class(TComponent);

  { Procedural function implementations }

function AddGlyph(var AArray: TFontImageDataArray; AGliphIndex: integer; AColor: TColor): integer;
var
  NewGlyph: TFontImageData;
begin
  NewGlyph.GliphIndex := AGliphIndex;
  NewGlyph.Color      := AColor;

  SetLength(AArray, Length(AArray) + 1);
  AArray[Length(AArray) - 1] := NewGlyph;

  Result := Length(AArray) - 1;
end;

procedure RemoveGlyph(var AArray: TFontImageDataArray; AIndex: integer);
var
  i: integer;
begin
  if (AIndex < 0) or (AIndex >= Length(AArray)) then Exit;

  for i := AIndex to Length(AArray) - 2 do
    AArray[i] := AArray[i + 1];

  SetLength(AArray, Length(AArray) - 1);
end;

procedure MoveGlyphUp(var AArray: TFontImageDataArray; AIndex: integer);
var
  TempGlyph: TFontImageData;
begin
  if (AIndex <= 0) or (AIndex >= Length(AArray)) then Exit;

  TempGlyph      := AArray[AIndex];
  AArray[AIndex] := AArray[AIndex - 1];
  AArray[AIndex - 1] := TempGlyph;
end;

procedure MoveGlyphDown(var AArray: TFontImageDataArray; AIndex: integer);
var
  TempGlyph: TFontImageData;
begin
  if (AIndex < 0) or (AIndex >= Length(AArray) - 1) then Exit;

  TempGlyph      := AArray[AIndex];
  AArray[AIndex] := AArray[AIndex + 1];
  AArray[AIndex + 1] := TempGlyph;
end;

{ TFontImageList }

constructor TFontImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //  FIsLoading := False;
  SetLength(FGlyphs, 0);
end;

destructor TFontImageList.Destroy;
begin
  SetLength(FGlyphs, 0);
  inherited Destroy;
end;

procedure TFontImageList.Loaded;
begin
  inherited Loaded;
  FIsLoading := True;
  if Assigned(FOnInitialize) then
    FOnInitialize(self);
  FIsLoading := False;
  RegenerateImages;
end;

procedure TFontImageList.DefineProperties(Filer: TFiler);
begin
  //  TComponentCracker(Self).DefineProperties(Filer);
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('GlyphsData', ReadGliphData, WriteGlyphData, True);
end;

procedure TFontImageList.SetHeight(AValue: integer);
begin
  SetWidthHeight(Width, AValue);
  RegenerateImages;
end;

procedure TFontImageList.WriteGlyphData(AStream: TStream);
var
  Glyph: TFontImageData;
  i: integer;
begin
  AStream.WriteDWord(Length(FGlyphs));
  for i := 0 to length(FGlyphs) - 1 do
  begin
    Glyph := FGlyphs[i];
    AStream.WriteDWord(Glyph.GliphIndex);
    AStream.WriteDWord(Glyph.Color);
  end;
end;

procedure TFontImageList.ReadGliphData(AStream: TStream);
var
  GlyphCnt: Dword;
  Glyph: TFontImageData;
  i: integer;
begin
  GlyphCnt := AStream.ReadDWord;
  SetLength(FGlyphs, GlyphCnt);

  for i := 0 to GlyphCnt - 1 do
  begin
    Glyph.GliphIndex := AStream.ReadDWord;
    Glyph.Color := AStream.ReadDWord;
    FGlyphs[i] := Glyph;
  end;
end;

procedure TFontImageList.WriteData(AStream: TStream);
begin
  //Do nothing
end;

procedure TFontImageList.ReadData(AStream: TStream);
begin
  //Do nothing
end;

procedure TFontImageList.WriteAdvData(AStream: TStream);
begin
  //Do nothing
end;

procedure TFontImageList.ReadAdvData(AStream: TStream);
begin
  //Do nothing
end;

procedure TFontImageList.SetWidth(AValue: integer);
begin
  SetWidthHeight(AValue, Height);
  RegenerateImages;
end;

procedure TFontImageList.SetFontName(AValue: TFileName);
begin
  if FFontName = AValue then Exit;
  FFontName := AValue;
  RegenerateImages;
end;

procedure TFontImageList.SetFontSource(AValue: TFontSource);
begin
  if FFontSource = AValue then Exit;
  FFontSource := AValue;
  RegenerateImages;
end;

procedure TFontImageList.SetGlyphs(AValue: TFontImageDataArray);
begin
  if FGlyphs = AValue then Exit;
  FGlyphs := AValue;
  RegenerateImages;
end;

procedure TFontImageList.SetOnInitialize(AValue: TNotifyEvent);
begin
  FOnInitialize := AValue;
end;

procedure TFontImageList.SetSize(AValue: TSize);
begin
  if FSize = AValue then Exit;
  FSize := AValue;
  RegenerateImages;
end;


procedure TFontImageList.RegenerateImages;
var
  Item: TFontImageData;
  IconRenderer: TIconRenderer;
  FStream: TStream;
  bmp: TBitMap;
begin
  if FIsLoading or (csLoading in ComponentState) then Exit;

  Clear;
  RegisterResolutions([Height]);

  try
    case FFontSource of
      fsrcFileName: FStream := TFileStream.Create(FFontName, fmOpenRead + fmShareDenyNone);
      fsrcResource: FStream := TResourceStream.Create(HInstance, FFontName, RT_RCDATA);
    end;
  except
    FStream.Free;
    exit;
  end;

  try
    IconRenderer := TIconRenderer.Create(FStream);
    IconRenderer.SetSize(Height, Width, True); // Scaling occurs inside object, no need to do it here

    for item in FGlyphs do
    begin
      if IsSysColor(Item.Color) then
       IconRenderer.Color := GetSysColor(SysColorToSysColorIndex(Item.Color))
      else
        IconRenderer.Color := Item.Color;
      Bmp := TBitmap.Create;
      IconRenderer.getGlyph(Item.GliphIndex, bmp); //;then
      Add(bmp, nil);
    end;

  finally
    IconRenderer.Free;
  end;
end;


end.
