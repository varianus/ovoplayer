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
unit netprotocol;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, base64;

{
My personal implementation of a netstrings-like protocol, where
each string is prefixed by it's length.

To avoid sending binary data, length is defined as a 24 bit unsigned integer.
This 3-byte integer is then encoded in Base64, so it became a 4 byte ASCII string

}
function EncodeSize(Size:Integer):string;
function DecodeSize(Size:string):integer;

implementation
function EncodeSize(Size:Integer):string;
var
  s : RawByteString;
  v:integer;
begin
  SetLength(s,3);
  s[1] := AnsiChar((Size and $ff0000 ) shr 16);
  s[2] := AnsiChar((Size and $00ff00) shr 8);
  s[3] := AnsiChar((Size and $0000ff));
  Result := base64.EncodeStringBase64(s);
end;

function DecodeSize(Size: String): integer;
var
  s : RawByteString;
  v:integer;
begin
  try
    s:= DecodeStringBase64(Size, True);

    Result := (byte(s[1]) shl 16) or
              (byte(s[2]) shl 8) or
              (byte(s[3]));
  except
    result := -1;
  end;
end;



end.

