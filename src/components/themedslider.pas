unit ThemedSlider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, Graphics, LCLType, InterfaceBase;

type

  { TThemedSlider }
  TThemedSliderOrientation = (ttsHorizontal, ttsVertical);


  TThemedSlider = class(TCustomControl)
  private
    FMax: Cardinal;
    FMin: Cardinal;
    FOnChange: TNotifyEvent;
    FOrientation: TThemedSliderOrientation;
    FPosition: Cardinal;
    fSeeking : Boolean;
    FSliderwidth: Integer;
    FStep: cardinal;
    InternalBitmap: TBitmap;
    procedure RedrawControl;
    procedure Seek(X, Y: Integer);
    procedure SetMax(const AValue: Cardinal);
    procedure SetMin(const AValue: Cardinal);
    procedure SetOrientation(AValue: TThemedSliderOrientation);
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
    property Orientation : TThemedSliderOrientation read FOrientation write SetOrientation;
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

procedure TThemedSlider.SetOrientation(AValue: TThemedSliderOrientation);
var
  OldWidth: LongInt;
  OldHeight: LongInt;
begin
  if FOrientation <> AValue then
  begin
    FOrientation := AValue;
    // switch width and height, but not when loading, because we assume that the
    // lfm contains a consistent combination of Orientation and (width, height)
    if not (csLoading in ComponentState) then
    begin
      OldWidth:=Width;
      OldHeight:=Height;
      SetBounds(Left,Top,OldHeight,OldWidth);
    end;
  end;
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
  RectInnerArea: TRect;
  RectSlider: TRect;
  RectFilled: TRect;
  pos, invpos :integer;
  HalfSlider:Integer;
  SlidingDimension: integer;
//  Color : DWORD;
const
  FrameWidth=1;
begin
  HalfSlider := FSliderwidth div 2;

  InternalBitmap.canvas.Brush.Color:= WidgetSet.GetSysColor(COLOR_BTNFACE);
  InternalBitmap.Canvas.FillRect(0, 0, InternalBitmap.Width, InternalBitmap.Height);

  InternalBitmap.Transparent:=true;
  InternalBitmap.TransparentMode:=tmAuto;
  RectInnerArea:=rect(BorderWidth, BorderWidth, Width - BorderWidth, Height - BorderWidth);
  WidgetSet.Frame3d(InternalBitmap.canvas.Handle, RectInnerArea, FrameWidth, bvLowered);
  RectInnerArea.Inflate(-FrameWidth, -FrameWidth);


  case FOrientation of
    ttsHorizontal: SlidingDimension := Width;
    ttsVertical: SlidingDimension := Height;
  end;

  if fMax = 0 then
    pos:=0
  else
    pos:= trunc(SlidingDimension * (FPosition / (fMax - fMin)));

  if pos < HalfSlider then
      pos := HalfSlider;

  if pos > SlidingDimension - HalfSlider then
    pos:= SlidingDimension - HalfSlider;

  if FOrientation = ttsVertical then
     invpos := Height - pos;

  case FOrientation of
       ttsHorizontal: RectFilled:=Rect(RectInnerArea.Left , RectInnerArea.Top, pos - HalfSlider, RectInnerArea.Bottom);
       ttsVertical  : RectFilled:=Rect(RectInnerArea.left , RectInnerArea.Bottom, RectInnerArea.Right, invpos + HalfSlider);
  end;

   if pos > HalfSlider then
     begin
        InternalBitmap.canvas.Brush.Color:= WidgetSet.GetSysColor(COLOR_HIGHLIGHT);
        InternalBitmap.Canvas.FillRect(RectFilled);
     end;


  case FOrientation of
    ttsHorizontal: RectFilled:=Rect(pos + HalfSlider, RectInnerArea.Top, RectInnerArea.Right , RectInnerArea.Bottom);
    ttsVertical  : RectFilled:=Rect(RectInnerArea.left, RectInnerArea.top,  RectInnerArea.Right, RectInnerArea.top + invpos - HalfSlider);
  end;

  if pos < SlidingDimension - SLIDERWIDTH then
    begin
       InternalBitmap.canvas.Brush.Color:= WidgetSet.GetSysColor(COLOR_WINDOW);
       InternalBitmap.Canvas.FillRect(RectFilled);
    end;

  case FOrientation of
    ttsHorizontal: RectSlider:=Rect(pos - HalfSlider, BorderWidth div 2, pos + HalfSlider,  Height- (BorderWidth div 2));
    ttsVertical  : RectSlider:=Rect(BorderWidth div 2, invpos - HalfSlider,  Width- (BorderWidth div 2),  invpos + HalfSlider);
  end;

  if fSeeking then
    begin
      WidgetSet.DrawFrameControl(InternalBitmap.canvas.Handle, RectSlider, DFC_BUTTON, DFCS_PUSHED+DFCS_BUTTONPUSH);
      WidgetSet.DrawEdge(InternalBitmap.canvas.Handle, RectSlider, EDGE_SUNKEN, BF_RECT+ BF_MONO);
    end
  else
    begin
      WidgetSet.DrawFrameControl(InternalBitmap.canvas.Handle, RectSlider, DFC_BUTTON, DFCS_BUTTONPUSH);
      WidgetSet.DrawEdge(InternalBitmap.canvas.Handle, RectSlider, EDGE_Raised, BF_RECT+ BF_mono);
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

procedure TThemedSlider.Seek(X, Y: Integer);
var
  tmppos :Cardinal;
  SlidingDimension: integer;
  Refer: integer;
begin

  case FOrientation of
      ttsHorizontal: begin
                       SlidingDimension := Width;
                       Refer := x;
                     end;
      ttsVertical:  begin
                       SlidingDimension := Height;
                       Refer := y;
                     end;
    end;

  if Refer < 0 then
     Refer := 0;

  tmppos := Round(Refer / ( SlidingDimension / (fmax - fmin)));

  if tmppos < fmin then
     tmppos:=FMin;
  if tmppos > FMax then
     tmppos:=FMax;

  if FOrientation = ttsVertical then
     tmppos := FMax - tmppos;

  fPosition := tmppos;
  RedrawControl;

end;

procedure TThemedSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    begin
      fseeking := true;
      case FOrientation of
          ttsHorizontal: Cursor := crHSplit;
          ttsVertical:  Cursor := crVSplit;
        end;
      Seek(X, Y);
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
  p: Tpoint;
begin
  p := Mouse.CursorPos;
  P:= ScreenToClient(p);
  fSeeking:=true;
  Seek(p.x, p.y);
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
  FOrientation:= ttsHorizontal;
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
