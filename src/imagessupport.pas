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
uses graphics,
    Classes, SysUtils, LCLType;

procedure ResizeBitmap(Bitmap: TBitmap; Width, Height: Integer; ForcePF32:boolean=false);

implementation

procedure ResizeBitmap(Bitmap: TBitmap; Width, Height: Integer; ForcePF32:boolean=false );
var
  R: TRect;
  B: TBitmap;

  WNew, HNew: integer;
begin
  if assigned(Bitmap) then begin
    B:= TBitmap.Create;
    try
      WNew := (Bitmap.Width * Height) div Bitmap.Height;
      HNew := (Width * Bitmap.Height) div Bitmap.Width;
      if (Width < WNew) then
      begin
        R.Right := Width;
        R.Bottom := HNew;
      end else
      begin
        R.Right := WNew;
        R.Bottom := Height;
      end;
  //    Writeln(format('Desired x:%d Y:%d  Source x:%d Y:%d  Result x:%d Y:%d',[Width, Height, Bitmap.Width, Bitmap.Height, R.Right, R.Bottom]));
      R.Left:= 0;
      R.Top:= 0;
      B.PixelFormat:= Bitmap.PixelFormat;
      B.Width:= R.Right;
      B.Height:= R.Bottom;
      B.Canvas.StretchDraw(R, Bitmap);
      Bitmap.Width:= R.Right;
      Bitmap.Height:= R.Bottom;
      if ForcePF32 then
        Bitmap.PixelFormat:= pf32bit;
      Bitmap.Canvas.Draw(0, 0, B);
    finally
      B.Free;
    end;
  end;
end;
end.

