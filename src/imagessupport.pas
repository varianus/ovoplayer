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
unit ImagesSupport;

{$mode objfpc}{$H+}

interface
uses graphics,
    Classes, SysUtils;

procedure ResizeBitmap(Bitmap: TBitmap; Width, Height: Integer);

implementation

procedure ResizeBitmap(Bitmap: TBitmap; Width, Height: Integer);
var
  R: TRect;
  B: TBitmap;
begin
  if assigned(Bitmap) then begin
    B:= TBitmap.Create;
    try
      if Bitmap.Width > Bitmap.Height then begin
        R.Right:= Width;
        R.Bottom:= ((Width * Bitmap.Height) div Bitmap.Width);
      end else begin
        R.Right:= ((Height * Bitmap.Width) div Bitmap.Height);
        R.Bottom:= Height;
      end;
      R.Left:= 0;
      R.Top:= 0;
      B.PixelFormat:= Bitmap.PixelFormat;
      B.Width:= r.Right;
      B.Height:= R.Bottom;
      B.Canvas.StretchDraw(R, Bitmap);
      Bitmap.Width:= r.Right;
      Bitmap.Height:= r.BOTTOM;
      Bitmap.Canvas.Draw(0, 0, B);
    finally
      B.Free;
    end;
  end;
end;
end.

