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
unit ImagesSupport;

interface

uses Graphics,
  Classes, SysUtils, Math, LCLType;

procedure ResizeBitmap(Bitmap: TBitmap; Width, Height: integer; ForcePF32: boolean = False);

implementation

procedure ResizeBitmap(Bitmap: TBitmap; Width, Height: integer; ForcePF32: boolean = False);
var
  SrcWidth, SrcHeight: integer;
  DestWidth, DestHeight: integer;
  OffsetX, OffsetY: integer;
  DestImage: TBitmap;
  Scale: double;
begin
  if assigned(Bitmap) then
  begin
    DestImage := TBitmap.Create;
    try
      SrcWidth   := Bitmap.Width;
      SrcHeight  := Bitmap.Height;
      Scale      := Min(Width / SrcWidth, Height / SrcHeight);
      DestWidth  := Round(SrcWidth * Scale);
      DestHeight := Round(SrcHeight * Scale);
      OffsetX    := (Width - DestWidth) div 2;
      OffsetY    := (Height - DestHeight) div 2;

      if ForcePF32 then
        DestImage.PixelFormat := pf32bit
      else
        DestImage.PixelFormat := Bitmap.PixelFormat;
      DestImage.SetSize(Width, Height);
      DestImage.Canvas.Brush.Color := clBlack;
      DestImage.Canvas.FillRect(Rect(0, 0, Width, Height));
      DestImage.Canvas.StretchDraw(Rect(OffsetX, OffsetY, OffsetX + DestWidth, OffsetY + DestHeight), Bitmap);
    finally
      Bitmap.Assign(DestImage);
      DestImage.Free;
    end;
  end;
end;

end.
