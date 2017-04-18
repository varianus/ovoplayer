unit ThemedSlider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, Graphics, LCLType, InterfaceBase;

type

  { TThemedSlider }

  TThemedSlider = class(TCustomControl)
  private
    FMax: Cardinal;
    FMin: Cardinal;
    FOnChange: TNotifyEvent;
    FPosition: Cardinal;
    fSeeking : Boolean;
    FSliderwidth: Integer;
    FStep: cardinal;
    InternalBitmap: TBitmap;
    procedure RedrawControl;
    procedure Seek(X: Integer);
    procedure SetMax(const AValue: Cardinal);
    procedure SetMin(const AValue: Cardinal);
    procedure SetPosition(const AValue: Cardinal);
    procedure SetSliderwidth(const AValue: Integer);
    procedure SetStep(const AValue: cardinal);
    { Private declarations }
  protected
    Procedure Paint; Override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    Function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);  override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
  public
    Constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;

  published
    property Align;
    property Anchors;
    property BorderWidth;
    property Enabled;
    property Step: cardinal read FStep write SetStep;
    Property Max: Cardinal read FMax write SetMax;
    Property Min: Cardinal read FMin write SetMin;
    Property Position: Cardinal read FPosition write SetPosition;
    Property Sliderwidth: Integer read FSliderwidth write SetSliderwidth;
    Property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I skinnedslider_icon.lrs}
  RegisterComponents('MCaselli',[TThemedSlider]);
end;

{ TThemedSlider }

procedure TThemedSlider.SetMax(const AValue: Cardinal);
begin
  if FMax=AValue then exit;
  FMax:=AValue;
  RedrawControl;
end;

procedure TThemedSlider.SetMin(const AValue: Cardinal);
begin
  if FMin=AValue then exit;
  FMin:=AValue;
  RedrawControl;
end;

procedure TThemedSlider.SetPosition(const AValue: Cardinal);
begin
  if fSeeking then
     exit;
  if FPosition=AValue then exit;
  FPosition:=AValue;
  RedrawControl;
end;

procedure TThemedSlider.SetSliderwidth(const AValue: Integer);
begin
  if FSliderwidth=AValue then exit;
  FSliderwidth:=AValue;
  RedrawControl;
end;

procedure TThemedSlider.SetStep(const AValue: cardinal);
begin
  if FStep=AValue then exit;
  FStep:=AValue;
end;

procedure TThemedSlider.RedrawControl;
var
  r1: TRect;
  r2: TRect;
  pos :integer;
  HalfSlider:Integer;
//  Color : DWORD;
const
  FrameWidth=1;
begin
  HalfSlider := FSliderwidth div 2;

  InternalBitmap.canvas.Brush.Color:= WidgetSet.GetSysColor(COLOR_BTNFACE);
  InternalBitmap.Canvas.FillRect(0, 0, InternalBitmap.Width, InternalBitmap.Height);

  InternalBitmap.Transparent:=true;
  InternalBitmap.TransparentMode:=tmAuto;
  r1:=rect(BorderWidth, BorderWidth, Width - BorderWidth, Height - BorderWidth);
  WidgetSet.Frame3d(InternalBitmap.canvas.Handle, r1, FrameWidth, bvLowered);
  if fMax = 0 then
     pos:=0
  else
     pos:= trunc(Width * (FPosition / (fMax - fMin)));


  if pos < 0 then pos := HalfSlider;
  if pos < HalfSlider then pos := HalfSlider;
  if pos > Width - HalfSlider then pos:= Width - HalfSlider;

  r2:=Rect(pos - HalfSlider, 1, pos + HalfSlider,  Height-1);
  if fSeeking then
     begin
       WidgetSet.DrawFrameControl(InternalBitmap.canvas.Handle, r2, DFC_BUTTON, DFCS_PUSHED+DFCS_BUTTONPUSH);
       WidgetSet.DrawEdge(InternalBitmap.canvas.Handle, r2, EDGE_SUNKEN, BF_RECT+ BF_MONO);
     end
  else
     begin
       WidgetSet.DrawFrameControl(InternalBitmap.canvas.Handle, r2, DFC_BUTTON, DFCS_BUTTONPUSH);
       WidgetSet.DrawEdge(InternalBitmap.canvas.Handle, r2, EDGE_Raised, BF_RECT+ BF_mono);
     end;

  if pos > SLIDERWIDTH then
     begin
        InternalBitmap.canvas.Brush.Color:= WidgetSet.GetSysColor(COLOR_HIGHLIGHT);
        InternalBitmap.Canvas.FillRect(R1.Left + (FrameWidth), r1.Top + FrameWidth,
                            pos - HalfSlider, r1.Bottom - FrameWidth);
     end;

  if pos < Width - SLIDERWIDTH then
     begin
        InternalBitmap.canvas.Brush.Color:= WidgetSet.GetSysColor(COLOR_WINDOW);
        InternalBitmap.Canvas.FillRect(pos + HalfSlider, r1.Top + FrameWidth,
                             Width - FrameWidth  -BorderWidth , r1.Bottom - FrameWidth);
     end;

 Invalidate;
end;

procedure TThemedSlider.Paint;
begin
  Canvas.Draw(0, 0, InternalBitmap);
  inherited Paint;
end;

procedure TThemedSlider.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if InternalBitmap = nil then
     exit;
  InternalBitmap.Width:=AWidth;
  InternalBitmap.Height:=AHeight;
  RedrawControl;
end;

function TThemedSlider.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin

  if WheelDelta > 0 then
     Position:= Position + FStep
  else
     Position:= Position - FStep;

  if Assigned(FOnChange) then
     FOnChange(self);

  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);

end;

procedure TThemedSlider.Seek(X: Integer);
var
  tmppos :Cardinal;
begin
  if x < 0 then
     x := 0;
  tmppos := Round(X / ( width / (fmax - fmin)));
  if tmppos < fmin then
     tmppos:=FMin;
  if tmppos > FMax then
     tmppos:=FMax;

  fPosition := tmppos;
  RedrawControl;

end;

procedure TThemedSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    begin
      fseeking := true;
      Cursor := crHSplit;
      Seek(X);
      if Assigned(FOnChange) then
         FOnChange(self);
      end
  else
    begin
      fseeking := False;
      Cursor := crDefault;
    end;

  inherited MouseMove(Shift, X, Y);
end;

procedure TThemedSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Button = mbLeft then
     fSeeking:=true;
  RedrawControl;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TThemedSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  fSeeking := False;
  RedrawControl;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TThemedSlider.Click;
var
  X: integer;
  p: Tpoint;
begin
  p := Mouse.CursorPos;
  x:= ScreenToClient(p).X;
  fSeeking:=true;
  Seek(x);
  fSeeking:=false;
  inherited Click;
end;

constructor TThemedSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse];
  Color:= clBtnFace;
  FMax:=100;
  FMin:=0;
  FPosition:=50;
  Width:=200;
  Height:=50;
  InternalBitmap:= TBitmap.Create;
  InternalBitmap.Height:=Height;
  InternalBitmap.Width:= Width;
  BorderWidth:=2;
  FSliderwidth:= 6;
  fseeking:=false;

  RedrawControl;
end;

destructor TThemedSlider.Destroy;
begin
  InternalBitmap.free;
  inherited Destroy;
end;

procedure TThemedSlider.EraseBackground(DC: HDC);
begin
//  inherited EraseBackground(DC);
end;

end.
