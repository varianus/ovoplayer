unit iconloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics, fpimage, LCLType, IntfGraphics, GraphType, EasyLazFreeType,
  LazFreeTypeIntfDrawer;

type

procedure LoadMainIcon;
procedure LoadFont;


implementation
uses lazutf8, udm, LCLIntf;
{$R ovoplayerfont.rc}

var
  ovofont: TFreeTypeFont;

procedure LoadMainIcon;
function getIcon(iconCode: Cardinal; size: integer; aColor: TColor): TBitmap;
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
    fontColor := TColorToFPColor(aColor);
    try
      ovofont.SizeInPixels := size-1;
      ovofont.Hinted := false;
      ovofont.ClearType := True;
      ovofont.Quality := grqHighQuality;
      ovofont.SmallLinePadding := False;
      image.SetSize(size, size);
      freeTypePainter.FillPixels(colTransparent);
      freeTypePainter.DrawText(utf8Value, ovofont, size, size, fontColor, [ftaRight, ftaBottom]);
      Result.LoadFromIntfImage(image);
    finally
      freeTypePainter.Free;
      image.Free;
    end;
  except
    Result := nil;
  end;
end;
begin
 DM.ilButtons.Clear;
 dm.ilButtons.Add(getIcon($e801,22,ColorToRGB (cldefault)),nil);   //0
 dm.ilButtons.Add(getIcon($e802,22,ColorToRGB (cldefault)),nil);
 dm.ilButtons.Add(getIcon($e803,22,ColorToRGB (cldefault)),nil);
 dm.ilButtons.Add(getIcon($e804,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($f111,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($e809,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($e808,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($e807,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($e805,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($e80a,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($e818,22,clBlack),nil);  //10
 dm.ilButtons.Add(getIcon($f114,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($f115,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($e811,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($e810,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($e80d,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($e80e,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($e80f,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($e80b,22,clBlack),nil);
 dm.ilButtons.Add(getIcon($e80c,22,clBlack),nil);

 DM.ilSmall.clear;
 dm.ilSmall.Add(getIcon($e814,16,clBlack),nil);
 dm.ilSmall.Add(getIcon($e815,16,clBlack),nil);
 dm.ilSmall.Add(getIcon($e816,16,clBlack),nil);
 dm.ilSmall.Add(getIcon($e817,16,clBlack),nil);
 dm.ilSmall.Add(getIcon($e819,16,clBlack),nil);
 dm.ilSmall.Add(getIcon($e81d,16,clBlack),nil);
 dm.ilSmall.Add(getIcon($e803,16,clBlack),nil);
 dm.ilSmall.Add(getIcon($f114,16,clBlack),nil);
 dm.ilSmall.Add(getIcon($e801,16,clBlack),nil);
 dm.ilSmall.Add(getIcon($e80f,16,clBlack),nil);
 dm.ilSmall.Add(getIcon($e802,16,clBlack),nil);
 dm.ilSmall.Add(getIcon($e804,16,clBlack),nil);
 dm.ilSmall.Add(getIcon($f192,16,clBlack),nil);
 dm.ilSmall.Add(getIcon($e81a,16,clBlack),nil);


end;

procedure LoadFont;
var
  s: TResourceStream;
begin
  ovofont := TFreeTypeFont.Create;
  S := TResourceStream.Create(HInstance, 'OVOFONT', RT_RCDATA);
  ovofont.AccessFromStream(S, True);
end;



end.

