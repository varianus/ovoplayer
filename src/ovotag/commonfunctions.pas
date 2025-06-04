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
  Classes, SysUtils;

const
  { Unicode ID }
  UNICODE_ID = #1;


function ExtractTrack(const TrackString: string): word;

function ExtractString(p: pbyte; size: cardinal): string;  overload;
function ExtractString(Encoding: byte; p: pbyte; size: cardinal): string; overload;

function ExtractString_ANSI(p: pbyte; size: cardinal): string;
function ExtractString_UTF8(p: pbyte; size: cardinal): string;
function ExtractString_UTF16(p: pbyte; size: cardinal; Encoding: byte): string;


procedure FixTrack(const TrackString: string; const TrackNr: integer; out TrackStringFixed: string;
  out TrackNrFixed: integer); overload;

procedure FixTrack(const TrackString: widestring; const TrackNr: integer; out TrackStringFixed: widestring;
  out TrackNrFixed: integer);
  overload;

function Swap32(const Figure: DWORD): DWORD;
function DecodeChannelNumber(Channels: integer): string;

function GetContent(const Content1, Content2: string): string;
function ExtractYear(const YearString, DateString: string): string;
function isValidYear(const YearString:string):boolean;
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
    1:
      Result := 'Mono';
    2:
      Result := 'Stereo';
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
  tmp    := SyncDWord and $FFFFFFF;
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

function isValidYear(const YearString: string): boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 1 to 4 do
    if not (YearString[i] in ['0'..'9']) then
      exit;
  Result := true;
end;

function ExtractGenre(const GenreString: string; offset: integer = 0): string;
var
  GenreNumber: integer;
  TmpString, tt: string;
  DelimPos: SizeInt;
begin
  { Extract genre from string }
  Result    := '';
  TmpString := GenreString;
  while TmpString.StartsWith('(') do
  begin
    if (Length(TmpString) > 1) and (TmpString[2] = '(') then
    begin
      Delete(TmpString, 1, 1);
      Result    := Result + ' ' + TmpString;
      TmpString := '';
      break;
    end
    else
    begin
      DelimPos := Pos(')', TmpString);
      if DelimPos = 0 then //Safety check
        Break;
      GenreNumber := 255;
      tt := Copy(TmpString, 2, DelimPos - 2);
      if TryStrToInt(Copy(TmpString, 2, DelimPos - 2), GenreNumber) and (GenreNumber <= ID3_MaxGenreExtended) then
        Result := Result + ' ' + v1Genres[GenreNumber + offset]
      else
      begin  //Safety check
        Result    := Result + TmpString;
        TmpString := '';
        break;
      end;
      Delete(tmpString, 1, DelimPos);
    end;
  end;
  if Result = '' then
    if TryStrToInt(TmpString, GenreNumber) and (GenreNumber <= ID3_MaxGenreExtended) then
      Result := Result + ' ' + v1Genres[GenreNumber + offset]
    else
      Result := (Result + ' ' + TmpString)
  else
    Result := (Result + ' ' + TmpString);
  Result := trim(Result);
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

procedure WideSwapEndian(PWC: pwidechar; size: integer);
begin
  while size >= sizeof(widechar) do
  begin
    PWC^ := widechar(SwapEndian(word(PWC^)));
    Inc(PWC);
    Dec(size, sizeof(widechar));
  end;
end;

function ExtractString(p: pbyte; size: cardinal): string;
var
  Encoding: byte;
begin
  Result := '';
  if size > 0 then
  begin
    Encoding := P^;
    Dec(size);
    Inc(p);
    Result := ExtractString(Encoding, p, size);
  end;
end;

function ExtractString(Encoding: byte; p: pbyte; size: cardinal): string;
var
  l: cardinal;
begin
  Result := '';
  case Encoding of
    0:
      Result := ExtractString_ANSI(p, size);
    1, 2:
      Result := ExtractString_UTF16(p, size, Encoding);
    3:
      Result := ExtractString_UTF8(p, size);
  end;

  l := length(Result);
  while (l > 0) and (Result[l] = #0) do
    Dec(l);
  setlength(Result, l);
end;


function ExtractString_ANSI(p: pbyte; size: cardinal): string;
begin
  Result := ISO_8859_1ToUTF8(pansichar(p));
end;

function ExtractString_UTF8(p: pbyte; size: cardinal): string;
var
  l: cardinal;
begin
  l := size;
  Result := (copy(PChar(p), 1, l));
end;

function ExtractString_UTF16(p: pbyte; size: cardinal; Encoding:byte): string;
var
  l, i: cardinal;
  be: boolean;
  ws: widestring = '';
begin
  size := size and $fffffffe;
  l    := size;
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
      be := Encoding = 2;
  end;

  L := ((L + 1) div 2) * 2;     // Ensure an even number of byte if
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
  if l > 1 then
    move(p^, ws[1], l);

  Result := UTF16ToUTF8(ws);

end;

procedure FixTrack(const TrackString: string; const TrackNr: integer; out TrackStringFixed: string;
  out TrackNrFixed: integer);
begin
  TrackStringFixed := TrackString;
  TrackNrFixed     := TrackNr;

  if (TrackNrFixed = 0) and (TrackStringFixed <> '') then
    TrackNrFixed := ExtractTrack(TrackStringFixed)
  else
  if (TrackNrFixed <> 0) and (TrackStringFixed = '') then
    TrackStringFixed := IntToStr(TrackNrFixed);

end;

procedure FixTrack(const TrackString: widestring; const TrackNr: integer; out TrackStringFixed: widestring;
  out TrackNrFixed: integer);
begin
  TrackStringFixed := TrackString;
  TrackNrFixed     := TrackNr;

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

  if (Code = 0) and (Value > 0) then
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
