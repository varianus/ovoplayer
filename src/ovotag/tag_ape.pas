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
unit tag_APE;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseTag;

type

  { TAPEFrame }

  TAPEFrame = class(TFrameElement)
  Private
    fValue :string;
  protected
    DataType: Word;
    function GetAsString: string;  override;
    procedure SetAsString(AValue: string); override;
  public
    function ReadFromStream(AStream: TStream): boolean; override;
  end;

  { TAPETags }

  TAPETags = class(TTags)
  public
    Function GetCommonTags: TCommonTags; override;
    function ReadFromStream(AStream: TStream): boolean; override;
  end;


implementation

const
  APE_IDENTIFIER :string = 'APETAGEX';

type

  TAPEHeader = packed record
    Marker: array[1..8] of ansichar;
    versionNumber:dword;
    TagSize:DWORD;
    count:DWORD;
    flags:DWORD;
    reserved:array[1..8] of byte;
  end;
{ TAPEFrame }



function TAPEFrame.GetAsString: string;
begin
  case DataType of
    0: Result := UTF8Decode(fValue);
    1: Result := '<bytes>';
  else
    result := 'unsupported';
  end;
end;

procedure TAPEFrame.SetAsString(AValue: string);
begin
  case DataType of
    0: fValue := UTF8Encode(AValue);
  else
    raise Exception.Create('Unsupported');
  end;
end;

function TAPEFrame.ReadFromStream(AStream: TStream): boolean;
var
  fSize:Cardinal;
  Data: array of char;
  NextChar: ansiChar;
  iSep :Integer;
  fFlags: DWORD;
  tmpName:string;
begin
  Result := false;
  fSize:= (AStream.ReadDWord);
  fFlags:= (AStream.ReadDWord);

  tmpname:='';
  DataType := fFlags and $03;
  NextChar:= ansichar(AStream.ReadByte);
  while Ord(NextChar) <> 0 do
    begin
      tmpName := tmpName + NextChar;
      NextChar:= ansichar(AStream.ReadByte);
    end;
  Id := tmpName;
  SetLength(Data, fSize + 1);

  AStream.Read(Data[0], fSize);
  fValue:=string(Data) +#0;
  result:=true;
end;

{ TAPETags }

function TAPETags.GetCommonTags: TCommonTags;
begin
  Result:=inherited GetCommonTags;

  Result.Album := GetFrameValue('ALBUM');
  Result.AlbumArtist := GetFrameValue('ALBUMARTIST');
  Result.Artist := GetFrameValue('ARTIST');
  Result.Comment := GetFrameValue('COMMENT');
  Result.Genre := GetFrameValue('GENRE');
  Result.Title := GetFrameValue('TITLE');
  Result.Track := StrToIntDef(GetFrameValue('TRACK'),0);
  Result.TrackString := GetFrameValue('TRACK');
  Result.Year := GetFrameValue('YEAR');
  if Result.AlbumArtist = '' then
     Result.AlbumArtist := result.Artist;
end;

function TAPETags.ReadFromStream(AStream: TStream): boolean;
var
  fSize: cardinal;
  Data: array of char;
  header :  TAPEHeader;
  i: cardinal;
  Frame: TAPEFrame;
begin
  Clear;
  result := false;
  try
    AStream.Seek(AStream.Size - SizeOf(Header), soFromBeginning);
    AStream.Read(Header, sizeof(Header));

   if String(Header.Marker) <> APE_IDENTIFIER then
      exit;

   AStream.Seek(AStream.Size - header.TagSize,  soFromBeginning);

    for i := 0 to header.count - 1 do
    begin
      Frame := TAPEFrame.Create;
      if Frame.ReadFromStream(AStream) then
         add(Frame)
      else
        FreeAndNil(Frame);
    end;
    Result := Count > 0;

  finally
  end;
end;

end.

