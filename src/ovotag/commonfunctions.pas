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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc;

const
  { Unicode ID }
  UNICODE_ID = #1;


function ExtractTrack(const TrackString: string): word;

function GetANSI(const Source: string): string;
function ExtractString(p:pbyte; size:cardinal): widestring;
procedure EncodeString(s:widestring; var e:pchar; var l:cardinal);

procedure FixTrack(const TrackString: string; const TrackNr: integer;
  out TrackStringFixed: string; out TrackNrFixed: integer); overload;

procedure FixTrack(const TrackString: WideString; const TrackNr: integer;
  out TrackStringFixed: WideString; out TrackNrFixed: integer);
  overload;

function Swap32(const Figure: DWORD): DWORD;
function DecodeChannelNumber(Channels:integer): string;

function GetContent(const Content1, Content2: string): string;
function ExtractYear(const YearString, DateString: string): string;
function ExtractGenre(const GenreString: string): string;
function ExtractText(const SourceString: string; LanguageID: boolean): string;
function SyncSafe_Decode(const SyncDWord: DWord): DWord;
function SyncSafe_Encode(const SyncDWord: DWord): DWord;

// Bit Operations
function GetBit(const Value: DWord; const Bit: byte): boolean; inline;
function ClearBit(const Value: DWord; const Bit: byte): DWord; inline;
function SetBit(const Value: DWord; const Bit: byte): DWord; inline;
function EnableBit(const Value: DWord; const Bit: byte;
  const TurnOn: boolean): DWord; inline;


implementation

{ --------------------------------------------------------------------------- }

function DecodeChannelNumber(Channels:integer): string;
begin
  case Channels of
    1: Result := 'Mono';
    2: Result := 'Stereo';
    else
       Result := Format('Multi Channel (%d)',[Channels]);
    end;
end;

function SyncSafe_Encode(const SyncDWord: DWord): DWord;
var
   b: array [0..3] of byte absolute SyncDWord;
   tmp : DWord;
begin
//
//  result:=  ((SyncDWord and $0000007f) shl 24) xor
//            ((SyncDWord and $00003f80) shl  9) xor
//            ((SyncDWord and $001fc000) shr  6) xor
//            ((SyncDWord and $0fe00000) shr 21);
tmp := SyncDWord and $FFFFFFF;
result := NtoBE((tmp and $7F) or ((tmp and $3F80) shl 1)
   or ((tmp and $1FC000) shl 2) or ((tmp and $FE00000) shl 3));

end;

function SyncSafe_Decode(const SyncDWord: DWord): DWord;
var
   b: array [0..3] of byte absolute SyncDWord;
begin
 result:=((SyncDWord and $000000ff) shl 21) xor
         ((SyncDWord and $0000ff00) shl  6) xor
         ((SyncDWord and $00ff0000) shr  9) xor
         ((SyncDWord and $ff000000) shr 24)

end;

function GetContent(const Content1, Content2: string): string;
begin
  { Get content preferring the first content }
  Result := GetANSI(Content1);
  if Result = '' then
    Result := GetANSI(Content2);
end;

{ --------------------------------------------------------------------------- }

function ExtractYear(const YearString, DateString: string): string;
begin
  { Extract year from strings }
  Result := GetANSI(YearString);
  if Result = '' then
    Result := Copy(GetANSI(DateString), 1, 4);
end;

{ --------------------------------------------------------------------------- }

function ExtractGenre(const GenreString: string): string;
begin
  { Extract genre from string }
  Result := GetANSI(GenreString);
  if Pos(')', Result) > 0 then
    Delete(Result, 1, LastDelimiter(')', Result));
end;

{ --------------------------------------------------------------------------- }

function ExtractText(const SourceString: string; LanguageID: boolean): string;
var
  Source: string;
  EncodingID: char;
begin
  { Extract significant text data from a complex field }
  Source := SourceString;
  Result := '';
  if Length(Source) > 0 then
  begin
    EncodingID := Source[1];
    if LanguageID then
      Delete(Source, 1, 4)
    else
      Delete(Source, 1, 1);
    Result := GetANSI(EncodingID + Source);
  end;
end;

function Swap32(const Figure: DWORD): DWORD;
var
  ByteArray: array [1..4] of byte absolute Figure;
begin
  Result :=
    ByteArray[1] shr 24 + ByteArray[2] shr 16 + ByteArray[3] shr 8 + ByteArray[4];
end;

function GetANSI(const Source: string): string;
var
  Index: integer;
  FirstByte, SecondByte: byte;
  UnicodeChar: widechar;
begin
  { Convert string from unicode if needed and trim spaces }
  if (Length(Source) > 0) and (Source[1] = UNICODE_ID) then
    begin
    Result := '';
    for Index := 1 to ((Length(Source) - 1) div 2) do
      begin
      FirstByte   := Ord(Source[Index * 2]);
      SecondByte  := Ord(Source[Index * 2 + 1]);
      UnicodeChar := widechar(FirstByte or (SecondByte shl 8));
      if UnicodeChar = #0 then
        break;
      if FirstByte < $FF then
        Result := Result + UnicodeChar;
      end;
    Result := Trim(Result);
    end
  else
    Result := Trim(Source);
end;

procedure EncodeString(s:widestring; var e:pchar; var l:cardinal);
var
  t:string;
begin
  t:=UTF8Encode(s);
  l:=length(t)+1;
  getmem(pointer(e),l);
  pbyte(e)^:=0;
  if l<>1 then move(t[1],(e+1)^,l-1);
end;

function ExtractString(p:pbyte; size:cardinal):widestring;
var l,i:cardinal; be:boolean;
begin
 if size<>0 then begin
  if p^=0 then begin
   result:=AnsiToUtf8(string(pchar(p)+1))
  end else if p^ in [1,2] then begin
   inc(p);
   dec(size);
   size:=size and $fffffffe;
   l:=0;
   while (l<size) and (pword(PtrUInt(p)+l)^<>0) do inc(l,2);
   if l=0 then
    be:=false
   else begin
    if pword(p)^=$feff then begin
     be:=false;
     inc(p,2);
     dec(l,2);
    end else if pword(p)^=$fffe then begin
     be:=true;
     inc(p,2);
     dec(l,2);
    end else
     be:=p^=2;
   end;
   setlength(result,l div 2);
   if be then begin
    for i:=1 to l div 2 do begin
     word(result[i]):=BeToN(pword(p)^);
     inc(p,2);
    end;
   end else
    move(p^,result[1],l);
  end else if p^=3 then begin
   inc(p);
   dec(size);
   l:=0;
   while (l<size) and (pbyte(PtrUInt(p)+l)^<>0) do inc(l);
   result:=UTF8Decode(copy(pchar(p),1,l));
  end;
 end;
 l:=length(result);
 while (l>0) and (result[l]=#0) do dec(l);
 setlength(result,l);
end;

procedure FixTrack(const TrackString: string; const TrackNr: integer;
  out TrackStringFixed: string; out TrackNrFixed: integer);
begin
  TrackStringFixed := TrackString;
  TrackNrFixed := TrackNr;

  if (TrackNrFixed = 0) and (TrackStringFixed <> '') then
    TrackNrFixed := ExtractTrack(TrackStringFixed)
  else
  if (TrackNrFixed <> 0) and (TrackStringFixed = '') then
    TrackStringFixed := IntToStr(TrackNrFixed);

end;

procedure FixTrack(const TrackString: WideString; const TrackNr: integer;
  out TrackStringFixed: WideString; out TrackNrFixed: integer);
begin
  TrackStringFixed := TrackString;
  TrackNrFixed := TrackNr;

  if (TrackNrFixed = 0) and (TrackStringFixed <> '') then
    TrackNrFixed := ExtractTrack(TrackStringFixed)
  else
  if (TrackNrFixed <> 0) and (TrackStringFixed = '') then
    TrackStringFixed := IntToStr(TrackNrFixed);

end;

function ExtractTrack(const TrackString: string): word;
var
  Track: string;
  Index, Value, Code: integer;
begin
  { Extract track from string }
  Track := GetANSI(TrackString);
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
  Result := Value or (1 shl Bit);
end;

function EnableBit(const Value: DWord; const Bit: byte; const TurnOn: boolean): DWord;
begin
  Result := (Value or (1 shl Bit)) xor (integer(not TurnOn) shl Bit);
end;

end.

