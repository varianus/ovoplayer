unit fontimagelisteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ButtonPanel, ExtCtrls, ComCtrls, ColorBox, GraphMath,
  FontImageList, iconrender, Types, Math, ComponentEditors;

type

  { TFontImageListDialog }

  TFontImageListDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    ColorBox1: TColorBox;
    ColorDialog1: TColorDialog;
    grdFont: TDrawGrid;
    grdGlyphs: TDrawGrid;
    pbTesting: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    btnAdd: TToolButton;
    btnDelete: TToolButton;
    btnUp: TToolButton;
    btnDown: TToolButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure ColorBox1Exit(Sender: TObject);
    procedure grdFontDblClick(Sender: TObject);
    procedure grdFontDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure grdGlyphsDblClick(Sender: TObject);
    procedure grdGlyphsDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure grdGlyphsSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
    procedure grdGlyphsSelection(Sender: TObject; aCol, aRow: integer);
    procedure pbTestingPaint(Sender: TObject);
  private
    FGlyphs: TFontImageDataArray;
    FSelectedChar: string;
    IconRenderer: TIconRenderer;

    intBmp: TBitmap;
    procedure Preview(Item: integer);
    procedure SetGlyphs(AValue: TFontImageDataArray);
  public
    property Glyphs: TFontImageDataArray read FGlyphs write SetGlyphs;
    constructor Create(AOwner: TComponent; const AFont: string; FontSource: TFontSource); reintroduce;
    destructor Destroy; override;
    property SelectedChar: string read FSelectedChar;

  end;

  { TFontImageListEditor }

  TFontImageListEditor = class(TComponentEditor)
  public
    function GetVerbCount: integer; override;
    function GetVerb({%H-}Index: integer): string; override;
    procedure ExecuteVerb({%H-}Index: integer); override;
  end;


implementation

uses LCLType, LCLIntf, PropEdits;

  {$R *.lfm}

type
  TSysColors = array[0..COLOR_ENDCOLORS] of TColor;

var
  DarkColors, LightColors: TSysColors;

  { TFontImageListDialog }

procedure TFontImageListDialog.grdFontDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  Code: word;
  Bmp: TBitmap;
begin
  Code := aRow * grdFont.ColCount + aCol;
  if code > IconRenderer.Count - 1 then
    exit;

  IconRenderer.Color := clWindowText;
  Bmp := TBitmap.Create;
  try
    if IconRenderer.getGlyph(code, Bmp) then
      grdFont.Canvas.Draw(aRect.Left, aRect.Top, Bmp);

  finally
    Bmp.Free;

  end;

end;

procedure TFontImageListDialog.grdGlyphsDblClick(Sender: TObject);
var
  CellRect: TRect;
  ARow, ACol: integer;
begin
  ARow := grdGlyphs.Row;
  ACol := grdGlyphs.Col;
  if (aCol = 2) and (aRow >= grdGlyphs.FixedRows) then
  begin
    // Get the pixel coordinates of the target cell
    CellRect := grdGlyphs.CellRect(aCol, aRow);

    // Position the ColorBox perfectly over the cell
    ColorBox1.Left   := grdGlyphs.Left + CellRect.Left + 2;
    ColorBox1.Top    := grdGlyphs.Top + CellRect.Top + 2;
    ColorBox1.Width  := CellRect.Width - 4;
    ColorBox1.Height := CellRect.Height; // Will automatically adapt based on control constraints

    // Load the existing color into the combo box selection
    ColorBox1.Selected := Glyphs[aRow].Color;

    // Bring it to life
    ColorBox1.Visible := True;
    ColorBox1.SetFocus;
    ColorBox1.DroppedDown := True; // Optional: instantly open the dropdown for UX
  end
  else
    ColorBox1.Visible := False// Hide it if the user clicks any other column
  ;
end;

procedure TFontImageListDialog.grdFontDblClick(Sender: TObject);
var
  Code: word;
begin
  Code := grdFont.Row * grdFont.ColCount + grdFont.Col;
  if code > IconRenderer.Count - 1 then
    exit;

  if grdGlyphs.RowCount < 1 then
    exit;
  Glyphs[grdGlyphs.Row].GliphIndex := Code;
  grdGlyphs.InvalidateRow(grdGlyphs.Row);
  Preview(grdGlyphs.Row);
end;

procedure TFontImageListDialog.btnAddClick(Sender: TObject);
var
  NewRow: integer;
begin
  NewRow := AddGlyph(FGlyphs, -1, clWindowText);
  grdGlyphs.RowCount := Length(FGlyphs);
  grdGlyphs.InvalidateRow(NewRow);
end;

procedure TFontImageListDialog.btnDeleteClick(Sender: TObject);
begin
  if grdGlyphs.Row < 0 then
    exit;
  RemoveGlyph(FGlyphs, grdGlyphs.Row);
  grdGlyphs.RowCount := Length(FGlyphs);
  grdGlyphs.Invalidate;
end;

procedure TFontImageListDialog.btnDownClick(Sender: TObject);
begin
  if (grdGlyphs.Row < 0) or (grdGlyphs.Row = Length(FGlyphs) - 1) then
    exit;

  MoveGlyphDown(FGlyphs, grdGlyphs.Row);
  grdGlyphs.Row := grdGlyphs.Row + 1;
  grdGlyphs.Invalidate;

end;

procedure TFontImageListDialog.btnUpClick(Sender: TObject);
begin
  if grdGlyphs.Row <= 0 then
    exit;

  MoveGlyphUp(FGlyphs, grdGlyphs.Row);
  grdGlyphs.Row := grdGlyphs.Row - 1;
  grdGlyphs.Invalidate;
end;

procedure TFontImageListDialog.ColorBox1Change(Sender: TObject);
var
  CurrentRow: integer;
begin
  CurrentRow := grdGlyphs.Row;
  // Save the selected color to our data structure
  Glyphs[CurrentRow].Color := ColorBox1.Selected;

  // Hide the editor and refresh the grid to paint the new color
//  ColorBox1.Visible := False;
  grdGlyphs.Invalidate;
  Preview(CurrentRow);
end;

procedure TFontImageListDialog.ColorBox1Exit(Sender: TObject);
begin
  ColorBox1.Visible := False;
end;

procedure TFontImageListDialog.grdGlyphsDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect;
  aState: TGridDrawState);
var
  Bmp: TBitmap;
begin
  if aRow >= length(Glyphs) then
    exit;
  case aCol of
    0:
      grdGlyphs.Canvas.TextOut(aRect.Left, aRect.Top, IntToStr(aRow));
    1:
    begin
      IconRenderer.SetSize(48, 46);
      IconRenderer.Color := Glyphs[aRow].Color;
      Bmp := TBitmap.Create;
      try
        if IconRenderer.getGlyph(Glyphs[aRow].GliphIndex, Bmp) then
          grdGlyphs.Canvas.Draw(aRect.Left, aRect.Top, Bmp);

      finally
        Bmp.Free;
      end;

    end;
    2: grdGlyphs.Canvas.TextOut(aRect.Left, aRect.Top, ColorToString(Glyphs[aRow].Color));
  end;
end;

procedure TFontImageListDialog.grdGlyphsSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
begin
  if aCol <> 2 then exit;

end;

procedure TFontImageListDialog.grdGlyphsSelection(Sender: TObject; aCol, aRow: integer);
begin
  Preview(ARow);

end;

procedure TFontImageListDialog.Preview(Item: integer);
var
  Bmp: TBitmap;
begin
  intBmp.SetSize(pbTesting.Width, pbTesting.Height);

  intbmp.Canvas.Brush.Color := LightColors[COLOR_WINDOW];
  intBmp.Canvas.Rectangle(0, 0, intBmp.Width div 2, intBmp.Height);

  intbmp.Canvas.Brush.Color := DarkColors[COLOR_WINDOW];
  intBmp.Canvas.Rectangle(intBmp.Width div 2, 0, intBmp.Width, intBmp.Height);


  Bmp := TBitmap.Create;
  try
    if IsSysColor(Glyphs[Item].Color) then
      IconRenderer.Color := LightColors[SysColorToSysColorIndex(Glyphs[Item].Color)]
    else
      IconRenderer.Color := Glyphs[Item].Color;

    IconRenderer.SetSize(18, 16);
    IconRenderer.getGlyph(Glyphs[Item].GliphIndex, Bmp);
    intBmp.Canvas.Draw(4, 4, Bmp);
    IconRenderer.SetSize(26, 24);
    IconRenderer.getGlyph(Glyphs[Item].GliphIndex, Bmp);
    intBmp.Canvas.Draw(4, 24, Bmp);
    IconRenderer.SetSize(34, 32);
    IconRenderer.getGlyph(Glyphs[Item].GliphIndex, Bmp);
    intBmp.Canvas.Draw(4, 52, Bmp);
    IconRenderer.SetSize(66, 64);
    IconRenderer.getGlyph(Glyphs[Item].GliphIndex, Bmp);
    intBmp.Canvas.Draw(4, 88, Bmp);

    if IsSysColor(Glyphs[Item].Color) then
      IconRenderer.Color := DarkColors[SysColorToSysColorIndex(Glyphs[Item].Color)]
    else
      IconRenderer.Color := Glyphs[Item].Color;

    IconRenderer.SetSize(18, 16);
    IconRenderer.getGlyph(Glyphs[Item].GliphIndex, Bmp);
    intBmp.Canvas.Draw(4 + intBmp.Width div 2, 4, Bmp);
    IconRenderer.SetSize(26, 24);
    IconRenderer.getGlyph(Glyphs[Item].GliphIndex, Bmp);
    intBmp.Canvas.Draw(4 + intBmp.Width div 2, 24, Bmp);
    IconRenderer.SetSize(34, 32);
    IconRenderer.getGlyph(Glyphs[Item].GliphIndex, Bmp);
    intBmp.Canvas.Draw(4 + intBmp.Width div 2, 52, Bmp);
    IconRenderer.SetSize(66, 64);
    IconRenderer.getGlyph(Glyphs[Item].GliphIndex, Bmp);
    intBmp.Canvas.Draw(4 + intBmp.Width div 2, 88, Bmp);

  finally
    Bmp.Free;
  end;


  pbTesting.Invalidate;

end;

procedure TFontImageListDialog.pbTestingPaint(Sender: TObject);
begin
  pbTesting.Canvas.Draw(0, 0, intBmp);
end;

procedure TFontImageListDialog.SetGlyphs(AValue: TFontImageDataArray);
begin
  if FGlyphs = AValue then Exit;
  FGlyphs := AValue;
  grdGlyphs.RowCount := Length(Glyphs);
end;

constructor TFontImageListDialog.Create(AOwner: TComponent; const AFont: string; FontSource: TFontSource);
var
  FStream: TStream;
begin
  inherited Create(nil);
  IntBmp := TBitmap.Create;
  intBmp.SetSize(pbTesting.Width, pbTesting.Height);
  try
    case FontSource of
      fsrcFileName: FStream := TFileStream.Create(AFont, fmOpenRead + fmShareDenyNone);
      fsrcResource: FStream := TResourceStream.Create(HInstance, AFont, PChar(10));
    end;
  except
    // Do nothing
  end;
  IconRenderer := TIconRenderer.Create(FStream);
  IconRenderer.SetSize(grdFont.DefaultRowHeight - 4, grdFont.DefaultRowHeight - 4, True); // Scaling occurs inside object, no need to do it here

  grdFont.RowCount := ceil(IconRenderer.Count / grdFont.ColCount);

end;

destructor TFontImageListDialog.Destroy;
begin
  inherited Destroy;
  IconRenderer.Free;
  intBmp.Free;
end;

{ TFontImageListEditor }

function TFontImageListEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

function TFontImageListEditor.GetVerb(Index: integer): string;
begin
  Result := 'Font Image List Editor';
end;

procedure TFontImageListEditor.ExecuteVerb(Index: integer);
var
  ImageList: TFontImageList;
  FontImageListDialog: TFontImageListDialog;
  Hook: TPropertyEditorHook;
begin
  if not (GetComponent is TFontImageList) then exit;
  ImageList := TFontImageList(GetComponent);
  GetHook(Hook);
  FontImageListDialog := TFontImageListDialog.Create(nil, ImageList.FontName, ImageList.FontSource);
  try
    FontImageListDialog.Glyphs := ImageList.Glyphs;
    if FontImageListDialog.ShowModal = mrOk then
      begin
         ImageList.Glyphs := FontImageListDialog.Glyphs;
         if Assigned(Hook) then Hook.Modified(Self);
      end;

  finally
    FontImageListDialog.Free;
  end;

end;

procedure InitializeDefaultColors;
begin
  //jcf:format=off
  Darkcolors[COLOR_SCROLLBAR]                := RGBToColor(53, 53, 53);
  Darkcolors[COLOR_BACKGROUND]               := RGBToColor(53, 53, 53);
  Darkcolors[COLOR_ACTIVECAPTION]            := RGBToColor(42, 130, 218);
  Darkcolors[COLOR_INACTIVECAPTION]          := RGBToColor(53, 53, 53);
  Darkcolors[COLOR_MENU]                     := RGBToColor(42, 42, 42);
  Darkcolors[COLOR_WINDOW]                   := RGBToColor(42, 42, 42);
  Darkcolors[COLOR_WINDOWFRAME]              := RGBToColor(20, 20, 20);
  Darkcolors[COLOR_MENUTEXT]                 := RGBToColor(245, 245, 245);
  Darkcolors[COLOR_WINDOWTEXT]               := RGBToColor(245, 245, 245);
  Darkcolors[COLOR_CAPTIONTEXT]              := RGBToColor(245, 245, 245);
  Darkcolors[COLOR_ACTIVEBORDER]             := RGBToColor(53, 53, 53);
  Darkcolors[COLOR_INACTIVEBORDER]           := RGBToColor(53, 53, 53);
  Darkcolors[COLOR_APPWORKSPACE]             := RGBToColor(53, 53, 53);
  Darkcolors[COLOR_HIGHLIGHT]                := RGBToColor(42, 130, 218);
  Darkcolors[COLOR_HIGHLIGHTTEXT]            := RGBToColor(245, 245, 245);
  Darkcolors[COLOR_BTNFACE]                  := RGBToColor(53, 53, 53);
  Darkcolors[COLOR_BTNSHADOW]                := RGBToColor(35, 35, 35);
  Darkcolors[COLOR_GRAYTEXT]                 := RGBToColor(160, 160, 160);
  Darkcolors[COLOR_BTNTEXT]                  := RGBToColor(245, 245, 245);
  Darkcolors[COLOR_INACTIVECAPTIONTEXT]      := RGBToColor(245, 245, 245);
  Darkcolors[COLOR_BTNHIGHLIGHT]             := RGBToColor(66, 66, 66);
  Darkcolors[COLOR_3DDKSHADOW]               := RGBToColor(20, 20, 20);
  Darkcolors[COLOR_3DLIGHT]                  := RGBToColor(40, 40, 40);
  Darkcolors[COLOR_INFOTEXT]                 := RGBToColor(53, 53, 53);
  Darkcolors[COLOR_INFOBK]                   := RGBToColor(245, 245, 245);
  Darkcolors[COLOR_HOTLIGHT]                 := RGBToColor(66, 66, 66);
  Darkcolors[COLOR_GRADIENTACTIVECAPTION]    := GetSysColor(COLOR_GRADIENTACTIVECAPTION);
  Darkcolors[COLOR_GRADIENTINACTIVECAPTION]  := GetSysColor(COLOR_GRADIENTINACTIVECAPTION);
  Darkcolors[COLOR_MENUHILIGHT]              := RGBToColor(66, 66, 66);
  Darkcolors[COLOR_MENUBAR]                  := RGBToColor(42, 42, 42);
  Darkcolors[COLOR_FORM]                     := RGBToColor(53, 53, 53);

  LightColors[COLOR_SCROLLBAR]               := GetSysColor(COLOR_SCROLLBAR);
  LightColors[COLOR_BACKGROUND]              := GetSysColor(COLOR_BACKGROUND);
  LightColors[COLOR_ACTIVECAPTION]           := GetSysColor(COLOR_ACTIVECAPTION);
  LightColors[COLOR_INACTIVECAPTION]         := GetSysColor(COLOR_INACTIVECAPTION);
  LightColors[COLOR_MENU]                    := GetSysColor(COLOR_MENU);
  LightColors[COLOR_WINDOW]                  := GetSysColor(COLOR_WINDOW);
  LightColors[COLOR_WINDOWFRAME]             := GetSysColor(COLOR_WINDOWFRAME);
  LightColors[COLOR_MENUTEXT]                := GetSysColor(COLOR_MENUTEXT);
  LightColors[COLOR_WINDOWTEXT]              := GetSysColor(COLOR_WINDOWTEXT);
  LightColors[COLOR_CAPTIONTEXT]             := GetSysColor(COLOR_CAPTIONTEXT);
  LightColors[COLOR_ACTIVEBORDER]            := GetSysColor(COLOR_ACTIVEBORDER);
  LightColors[COLOR_INACTIVEBORDER]          := GetSysColor(COLOR_INACTIVEBORDER);
  LightColors[COLOR_APPWORKSPACE]            := GetSysColor(COLOR_APPWORKSPACE);
  LightColors[COLOR_HIGHLIGHT]               := GetSysColor(COLOR_HIGHLIGHT);
  LightColors[COLOR_HIGHLIGHTTEXT]           := GetSysColor(COLOR_HIGHLIGHTTEXT);
  LightColors[COLOR_BTNFACE]                 := GetSysColor(COLOR_BTNFACE);
  LightColors[COLOR_BTNSHADOW]               := GetSysColor(COLOR_BTNSHADOW);
  LightColors[COLOR_GRAYTEXT]                := GetSysColor(COLOR_GRAYTEXT);
  LightColors[COLOR_BTNTEXT]                 := GetSysColor(COLOR_BTNTEXT);
  LightColors[COLOR_INACTIVECAPTIONTEXT]     := GetSysColor(COLOR_INACTIVECAPTIONTEXT);
  LightColors[COLOR_BTNHIGHLIGHT]            := GetSysColor(COLOR_BTNHIGHLIGHT);
  LightColors[COLOR_3DDKSHADOW]              := GetSysColor(COLOR_3DDKSHADOW);
  LightColors[COLOR_3DLIGHT]                 := GetSysColor(COLOR_3DLIGHT);
  LightColors[COLOR_INFOTEXT]                := GetSysColor(COLOR_INFOTEXT);
  LightColors[COLOR_INFOBK]                  := GetSysColor(COLOR_INFOBK);
  LightColors[COLOR_HOTLIGHT]                := GetSysColor(COLOR_HOTLIGHT);
  LightColors[COLOR_GRADIENTACTIVECAPTION]   := GetSysColor(COLOR_GRADIENTACTIVECAPTION);
  LightColors[COLOR_GRADIENTINACTIVECAPTION] := GetSysColor(COLOR_GRADIENTINACTIVECAPTION);
  LightColors[COLOR_MENUHILIGHT]             := GetSysColor(COLOR_MENUHILIGHT);
  LightColors[COLOR_MENUBAR]                 := GetSysColor(COLOR_MENUBAR);
  LightColors[COLOR_FORM]                    := GetSysColor(COLOR_FORM);

  //jcf:format=on
end;

initialization
  InitializeDefaultColors;

end.
