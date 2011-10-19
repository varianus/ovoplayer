unit SkinnedSlider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, Graphics, LCLType, InterfaceBase;

type

  { TSkinnedSlider }

  TSkinnedSlider = class(TCustomControl)
  private
    FBackGroundImage: TPicture;
    FForeGroundImage: TPicture;
    FMax: Cardinal;
    FMin: Cardinal;
    FOnChange: TNotifyEvent;
    FPosition: Cardinal;
    fSeeking : Boolean;
    procedure SetBackGroundImage(const AValue: TPicture);
    procedure SetForeGroundImage(const AValue: TPicture);
    procedure SetMax(const AValue: Cardinal);
    procedure SetMin(const AValue: Cardinal);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetPosition(const AValue: Cardinal);
    { Private declarations }
  protected
    Procedure Paint; Override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);  override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    Constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;

  published
    property BorderWidth;
    Property Max: Cardinal read FMax write SetMax;
    Property Min: Cardinal read FMin write SetMin;
    Property Position: Cardinal read FPosition write SetPosition;
    Property BackGroundImage : TPicture read FBackGroundImage write SetBackGroundImage;
    Property ForeGroundImage : TPicture read FForeGroundImage write SetForeGroundImage;
    Property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property OnClick;

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I skinnedslider_icon.lrs}
  RegisterComponents('MCaselli',[TSkinnedSlider]);
end;

{ TSkinnedSlider }

procedure TSkinnedSlider.SetBackGroundImage(const AValue: TPicture);
begin
  if FBackGroundImage=AValue then exit;
  FBackGroundImage:=AValue;
  width:= FBackGroundImage.Width;
  Height:= FBackGroundImage.Height;
  Repaint;
end;

procedure TSkinnedSlider.SetForeGroundImage(const AValue: TPicture);
begin
  if FForeGroundImage=AValue then exit;
  FForeGroundImage:=AValue;
  Repaint;
end;

procedure TSkinnedSlider.SetMax(const AValue: Cardinal);
begin
  if FMax=AValue then exit;
  FMax:=AValue;
  Repaint;
end;

procedure TSkinnedSlider.SetMin(const AValue: Cardinal);
begin
  if FMin=AValue then exit;
  FMin:=AValue;
  Repaint;
end;

procedure TSkinnedSlider.SetOnChange(const AValue: TNotifyEvent);
begin
  if FOnChange=AValue then exit;
  FOnChange:=AValue;
end;

procedure TSkinnedSlider.SetPosition(const AValue: Cardinal);
begin
  if FPosition=AValue then exit;
  FPosition:=AValue;
  Repaint;
end;

procedure TSkinnedSlider.Paint;
var
  r1: TRect;
  r2: TRect;
  b1: TBitmap;
  pos :integer;
const
  SLIDERWIDTH = 5;
begin

  b1:= TBitmap.Create;
  b1.Height:=Height;
  b1.Width:= Width;
  b1.canvas.Brush.Color:= clForm;
  b1.Canvas.FillRect(0, 0, b1.Width, b1.Height);
  b1.Transparent:=true;
  b1.TransparentMode:=tmAuto;
  r1:=rect(0,0+SLIDERWIDTH, Width, Height-SLIDERWIDTH);

  //b1.Canvas.Draw(0,0,FBackGroundImage.Graphic);
  //r1:=Rect(0,0, trunc(Width * (FPosition / (Max - Min))), Height);
  //b1.Canvas.CopyRect(r1, FForeGroundImage.Bitmap.Canvas, r1);

  b1.canvas.Brush.Color:= clForm;
  WidgetSet.Frame3d(b1.canvas.Handle, r1, BorderWidth, bvLowered);
  b1.canvas.Brush.Color:= clForm;

//  WidgetSet.Framerect(b1.canvas.Handle, r1, b1.Canvas.Brush.Handle);

  pos:= trunc(Width * (FPosition / (Max - Min)));

  if pos < 0 then pos := SLIDERWIDTH;
  if pos < 5 then pos := SLIDERWIDTH;
  if pos > Width -5 then pos:= Width - SLIDERWIDTH;

  r2:=Rect(pos - SLIDERWIDTH, 1, pos + SLIDERWIDTH,  Height-1);
  if fSeeking then
     begin
       WidgetSet.DrawFrameControl(b1.canvas.Handle, r2, DFC_BUTTON, DFCS_PUSHED+DFCS_BUTTONPUSH);
       WidgetSet.DrawEdge(b1.canvas.Handle, r2, EDGE_Raised, BF_RECT+ BF_mono);
     end
  else
     begin
       WidgetSet.DrawFrameControl(b1.canvas.Handle, r2, DFC_BUTTON, DFCS_BUTTONPUSH);
       WidgetSet.DrawEdge(b1.canvas.Handle, r2, EDGE_SUNKEN, BF_RECT+ BF_FLAT);
     end;

  if pos > SLIDERWIDTH then
     begin
        b1.canvas.Brush.Color:= WidgetSet.GetSysColor(COLOR_HIGHLIGHT);
         b1.Canvas.FillRect(R1.Left + (BorderWidth div 2), r1.Top + BorderWidth,
                            pos - SLIDERWIDTH, r1.Bottom - BorderWidth);
     end;

  //DFCS_FLAT

  Canvas.Draw(0, 0, b1);
  b1.free;
  inherited Paint;

end;

procedure TSkinnedSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
  begin
    if ssLeft in Shift then
      begin
        fseeking := true;
        Cursor := crHSplit;
        if x < 0 then
           x := 0;
        fPosition := Round(X / ( width / (fmax - fmin)));
        if FPosition < fmin then
           FPosition:=FMin;
        if FPosition > FMax then
           FPosition:=FMax;

        invalidate;
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

procedure TSkinnedSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Button = mbLeft then
     fSeeking:=true;
  Invalidate;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSkinnedSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  fSeeking := False;
  Invalidate;
  inherited MouseUp(Button, Shift, X, Y);
end;

constructor TSkinnedSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse];
  Color:= clBtnFace;
  FMax:=100;
  FMin:=0;
  FPosition:=50;
  Width:=200;
  Height:=50;
  BorderWidth:=2;
  fseeking:=false;
  FBackGroundImage:=TPicture.Create;
  FForeGroundImage:=TPicture.Create;
end;

destructor TSkinnedSlider.Destroy;
begin
  FBackGroundImage.Free;
  FForeGroundImage.Free;

  inherited Destroy;
end;

procedure TSkinnedSlider.EraseBackground(DC: HDC);
begin
//  inherited EraseBackground(DC);
end;

end.
