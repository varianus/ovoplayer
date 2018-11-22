{
This file is part of OvoTag
Copyright (C) 2011 Marco Caselli

OvoTag is free software; you can redistribute it and/or
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
{$I ovotag.inc}
unit CommonFunctions;

interface

uses
  Classes, SysUtils, LazLoggerBase;

const
  { Unicode ID }
  UNICODE_ID = #1;


function ExtractTrack(const TrackString: string): word;

function ExtractString(p: pbyte; size: cardinal; LanguageID: boolean = False): string;

function ExtractString_ANSI(p: pbyte; size: cardinal; LanguageID: boolean = False): string;
function ExtractString_UTF8(p: pbyte; size: cardinal; LanguageID: boolean = False): string;
function ExtractString_UTF16(p: pbyte; size: cardinal; LanguageID: boolean = False): string;


procedure FixTrack(const TrackString: string; const TrackNr: integer; out TrackStringFixed: string;
  out TrackNrFixed: integer); overload;

procedure FixTrack(const TrackString: WideString; const TrackNr: integer; out TrackStringFixed: WideString;
  out TrackNrFixed: integer);
  overload;

function Swap32(const Figure: DWORD): DWORD;
function DecodeChannelNumber(Channels: integer): string;

function GetContent(const Content1, Content2: string): string;
function ExtractYear(const YearString, DateString: string): string;
function ExtractGenre(const GenreString: string; offset: integer = 0): string;
function SyncSafe_Decode(const SyncDWord: DWord): DWord;
function SyncSafe_Encode(const SyncDWord: DWord): DWord;

// Bit Operations
function GetBit(const Value: DWord; const Bit: byte): boolean; inline;
function ClearBit(const Value: DWord; const Bit: byte): DWord; inline;
function SetBit(const Value: DWord; const Bit: byte): DWord; inline;
function EnableBit(const Value: DWord; const Bit: byte; const TurnOn: boolean): DWord; inline;


implementation

uses ID3v1Genres, lconvencoding, lazutf8;

{ --------------------------------------------------------------------------- }

function DecodeChannelNumber(Channels: integer): string;
begin
  case Channels of
    1: Result := 'Mono';
    2: Result := 'Stereo';
    else
      Result := Format('Multi Channel (%d)', [Channels]);
    end;
end;

function SyncSafe_Encode(const SyncDWord: DWord): DWord;
var
  tmp: DWord;
begin

  //  result:=  ((SyncDWord and $0000007f) shl 24) xor
  //            ((SyncDWord and $00003f80) shl  9) xor
  //            ((SyncDWord and $001fc000) shr  6) xor
  //            ((SyncDWord and $0fe00000) shr 21);
  tmp := SyncDWord and $FFFFFFF;
  Result := NtoBE((tmp and $7F) or ((tmp and $3F80) shl 1) or ((tmp and $1FC000) shl 2) or ((tmp and $FE00000) shl 3));

end;

function SyncSafe_Decode(const SyncDWord: DWord): DWord;
begin
  Result := ((SyncDWord and $000000ff) shl 21) xor ((SyncDWord and $0000ff00) shl 6) xor
    ((SyncDWord and $00ff0000) shr 9) xor ((SyncDWord and $ff000000) shr 24);

end;

function GetContent(const Content1, Content2: string): string;
begin
  { Get content preferring the first content }
  Result := Content1;
  if Result = '' then
    Result := Content2;
end;

{ --------------------------------------------------------------------------- }

function ExtractYear(const YearString, DateString: string): string;
begin
  { Extract year from strings }
  Result := (YearString);
  if Result = '' then
    Result := Copy((DateString), 1, 4);
end;

{ --------------------------------------------------------------------------- }

function ExtractGenre(const GenreString: string; offset: integer = 0): string;
var
  GenreNumber: integer;
begin
  { Extract genre from string }
  Result := (GenreString);
  GenreNumber := 255;
  if TryStrToInt(Result, GenreNumber) and (GenreNumber <= ID3_MaxGenreExtended) then
    Result := v1Genres[GenreNumber + offset];

  if Pos(')', Result) > 0 then
    Delete(Result, 1, LastDelimiter(')', Result));
end;

{ --------------------------------------------------------------------------- }

//function ExtractText(const SourceString: string; LanguageID: boolean): string;
//var
//  Source: string;
//  EncodingID: char;
//begin
//  { Extract significant text data from a complex field }
//  Source := SourceString;
//  Result := '';
//  if Length(Source) > 0 then
//  begin
//    EncodingID := Source[1];
//    if LanguageID then
//      Delete(Source, 1, 4)
//    else
//      Delete(Source, 1, 1);
//    Result := GetANSI(EncodingID + Source);
//  end;
//end;

function Swap32(const Figure: DWORD): DWORD;
var
  ByteArray: array [1..4] of byte absolute Figure;
begin
  Result :=
    ByteArray[1] shr 24 + ByteArray[2] shr 16 + ByteArray[3] shr 8 + ByteArray[4];
end;

procedure WideSwapEndian(PWC: PWideChar; size: integer);
begin
  while size >= sizeof(widechar) do
    begin
    PWC^ := widechar(SwapEndian(word(PWC^)));
    Inc(PWC);
    Dec(size, sizeof(widechar));
    end;
end;

function ExtractString(p: pbyte; size: cardinal; LanguageID: boolean = False): string;
var
  Encoding : byte;
  l: cardinal;
begin
  Result:= '';
  if size > 0 then
    begin
       Encoding := P^;
       dec(size);
       Inc(p);
       case Encoding of
         0   : Result := ExtractString_ANSI(p,size,LanguageID);
         1,2 : Result := ExtractString_UTF16(p,size,LanguageID);
         3   : Result := ExtractString_UTF8(p,size,LanguageID);
       end;
    end;
  l := length(Result);
  while (l > 0) and (Result[l] = #0) do
    Dec(l);
  setlength(Result, l);
end;

function ExtractString_ANSI(p: pbyte; size: cardinal; LanguageID: boolean = False): string;
var
 l: cardinal;
begin
  if LanguageID then
    begin
      l := size - 3;
      Inc(p, 3);
      if p^ = 0 then
        begin
          dec(l);
          inc(p);
        end
      else
        while (l < size) and (pbyte(p)^ <> 0) do
          begin
            Dec(l);
            inc(p);
          end;
    end;

  Result := ISO_8859_1ToUTF8(PAnsiChar(p));

end;

function ExtractString_UTF8(p: pbyte; size: cardinal; LanguageID: boolean = False): string;
var
 l: cardinal;
begin
  l := 0;
  if LanguageID then
    begin
      Inc(p, 3);
      if p^ = 0 then
        begin
          inc(l);
          inc(p);
        end
      else
       begin
        while (l < size) and (pbyte(p)^ <> 0) do
          begin
            Inc(p);
            Inc(l);
          end;

        Dec(size, l);
        Inc(p);
       end;
    end;

  while (l < size) and (pbyte(p + l)^ <> 0) do
    Inc(l);
  Result := (copy(PChar(p), 1, l));
end;

function ExtractString_UTF16(p: pbyte; size: cardinal; LanguageID: boolean = False): string;
var
  l, i: cardinal;
  OldP: pbyte;
  oldL: Cardinal;
  be: boolean;
  ws: WideString;
begin
  size := size and $fffffffe;
  l:= size;
  if LanguageID then
    begin
    if ( (pword(p)^ = $feff) or (pword(p)^ = $fffe)) then   // corruption protection
      begin
        Inc(p, 8);
        dec(l, 8);
      end
    else
      begin
        Inc(p, 3);
        dec(l, 3);
      end;

    if ( (pword(p)^ = $feff) or (pword(p)^ = $fffe)) and  // empty description
         (pword(p + 2)^ = 0) then
      begin
        Inc(p, 4);
        DEC(l, 4);
      end
    else
      begin
        OldP:=p;   // save current position for corrupted description
        OldL:=L;
        while (l >= 2) and (pword(p)^ <> 0) do
            begin
              Dec(l,2);
              inc(p,2);
            end;
          if l >=2 then
            begin
              Dec(l,2);
              inc(p,2);
            end
          else
            begin
              p := OldP;  // missing description, restore search position
              L := OldL;
            end;
        end;
    end;

  if l = 0 then
    be := False
  else
    begin
    if pword(p)^ = $feff then
      begin
        be := False;
        Inc(p, 2);
        Dec(l, 2);
      end
    else
    if pword(p)^ = $fffe then
      begin
        be := True;
        Inc(p, 2);
        Dec(l, 2);
      end
    else
      be := p^ = 2;
    end;

  L:= ((L+1) DIV 2) *2;     // Ensure an even number of byte if
  setlength(ws, l div 2);   // endian conversion is needed
  if be then
    begin
    for i := 1 to l div 2 do
      begin
        word(ws[i]) := BeToN(pword(p)^);
        Inc(p, 2);
      end;
    end
  else
    move(p^, ws[1], l);

  Result := UTF16ToUTF8(ws);

end;

procedure FixTrack(const TrackString: string; const TrackNr: integer; out TrackStringFixed: string;
  out TrackNrFixed: integer);
begin
  TrackStringFixed := TrackString;
  TrackNrFixed := TrackNr;

  if (TrackNrFixed = 0) and (TrackStringFixed <> '') then
    TrackNrFixed := ExtractTrack(TrackStringFixed)
  else
    if (TrackNrFixed <> 0) and (TrackStringFixed = '') then
      TrackStringFixed := IntToStr(TrackNrFixed);

end;

procedure FixTrack(const TrackString: WideString; const TrackNr: integer; out TrackStringFixed: WideString;
  out TrackNrFixed: integer);
begin
  TrackStringFixed := TrackString;
  TrackNrFixed := TrackNr;

  if (TrackNrFixed = 0) and (TrackStringFixed <> '') then
    TrackNrFixed := ExtractTrack(UTF8Encode(TrackStringFixed))
  else
    if (TrackNrFixed <> 0) and (TrackStringFixed = '') then
      TrackStringFixed := WideString(IntToStr(TrackNrFixed));

end;

function ExtractTrack(const TrackString: string): word;
var
  Track: string;
  Index, Value, Code: integer;
begin
  { Extract track from string }
  Track := (TrackString);
  Index := Pos('/', Track);
  if Index = 0 then
    Val(Track, Value, Code)
  else
    Val(Copy(Track, 1, Index - 1), Value, Code);
  if Code = 0 then
    Result := Value
  else
    Result := 0;
end;

function GetBit(const Value: DWord; const Bit: byte): boolean;
begin
  Result := (Value and (1 shl Bit)) <> 0;
end;

function ClearBit(const Value: DWord; const Bit: byte): DWord;
begin
  Result := Value and not (1 shl Bit);
end;

function SetBit(const Value: DWord; const Bit: byte): DWord;
begin
  Result := Value or DWord(1 shl Bit);
end;

function EnableBit(const Value: DWord; const Bit: byte; const TurnOn: boolean): DWord;
begin
  Result := (Value or DWord(1 shl Bit)) xor DWord(integer(not TurnOn) shl Bit);
end;

end.
