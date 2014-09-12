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

  TAPEFrame = class(TFrameElement)
  Private
    fValue :string;
  protected
    DataType: Word;
    function GetAsString: string;  override;
    procedure SetAsString(const AValue: string); override;
  public
    function ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;

  { TAPETags }

  TAPETags = class(TTags)
  private
    function ImportFromID3V1(AStream: TStream): boolean;
  public
    FromV1: Boolean;
    Function GetCommonTags: TCommonTags; override;
    procedure SetCommonTags(CommonTags: TCommonTags); override;
    function ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;


implementation
uses
  tag_id3v2, id3v1genres, CommonFunctions;

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

procedure TAPEFrame.SetAsString(const AValue: string);
begin
  case DataType of
    0: fValue := UTF8Encode(AValue);
  else
    raise Exception.Create('Unsupported');
  end;
end;

function TAPEFrame.ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean;
var
  fSize:Cardinal;
  Data: array of char;
  NextChar: ansiChar;
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
  SetLength(Data, fSize+1);

  AStream.Read(Data[0], fSize);
  fValue:=string(Data) ;
  result:=true;
end;

function TAPEFrame.WriteToStream(AStream: TStream): DWord;
var
  fSize:Cardinal;
begin
  fSize:= Length(fValue);
  AStream.WriteDWord(fSize);
  AStream.WriteDWord(DataType);
  AStream.Write(ID[1],Length(id));
  AStream.WriteByte(0);
  AStream.Write(fValue[1],fSize);
  Result := 9 + Length(id) +fSize;

end;

{ TAPETags }

function TAPETags.ImportFromID3V1(AStream: TStream): boolean;
var
  V1Rec : TID3V1Record;
  Frame : TAPEFrame;
begin
  result := false;
  AStream.Seek(AStream.Size - SizeOf(V1Rec), soFromBeginning);
  AStream.Read(V1Rec,  SizeOf(V1Rec));
  if V1Rec.Header <> 'TAG' then
    exit;

  Frame := TAPEFrame.Create('ARTIST');
  Frame.AsString := trim(V1Rec.Artist);
  Add(Frame);

  Frame := TAPEFrame.Create('ALBUM');
  Frame.AsString := trim(V1Rec.Album);
  Add(Frame);

  Frame := TAPEFrame.Create('TITLE');
  Frame.AsString := trim(V1Rec.Title);
  Add(Frame);

  Frame := TAPEFrame.Create('YEAR');
  Frame.AsString := trim(V1Rec.Year);
  Add(Frame);

  if V1Rec.Genre < 147 then
    begin
      Frame := TAPEFrame.Create('GENRE');
      Frame.AsString := v1Genres[V1Rec.Genre];
      Add(Frame);
    end;

  if V1Rec.Stopper = #00 then
    begin
      Frame := TAPEFrame.Create('COMMENT');
      Frame.AsString := trim(V1Rec.Comment);
      Add(Frame);

      Frame := TAPEFrame.Create('TRACK');
      Frame.AsString := inttostr(v1rec.track);
      Add(Frame);

    end
  else
  begin
    Frame := TAPEFrame.Create('COMMENT');
    Frame.AsString := trim(V1Rec.Comment + V1Rec.stopper + char(V1Rec.track));
    Add(Frame);
  end;
  result:=true;

end;
function TAPETags.GetCommonTags: TCommonTags;
begin
  Result:=inherited GetCommonTags;

  Result.Album := GetFrameValue('ALBUM');
  Result.AlbumArtist := GetFrameValue('ALBUMARTIST');
  Result.Artist := GetFrameValue('ARTIST');
  Result.Comment := GetFrameValue('COMMENT');
  Result.Genre := GetFrameValue('GENRE');
  Result.Title := GetFrameValue('TITLE');
  Result.Track := ExtractTrack(GetFrameValue('TRACK'));
  Result.TrackString := GetFrameValue('TRACK');
  Result.Year := GetFrameValue('DATE');
  if Result.Year = '' then
     Result.Year := GetFrameValue('YEAR');

  if Result.AlbumArtist = '' then
     Result.AlbumArtist := result.Artist;
end;

procedure TAPETags.SetCommonTags(CommonTags: TCommonTags);
begin
  inherited SetCommonTags(CommonTags);
  if CommonTags.Album <> '' then
     SetFrameValue('ALBUM', CommonTags.Album, TAPEFrame);
  if CommonTags.AlbumArtist <> '' then
     SetFrameValue('ALBUMARTIST', CommonTags.AlbumArtist, TAPEFrame);
  if CommonTags.Artist <> '' then
     SetFrameValue('ARTIST', CommonTags.Artist, TAPEFrame);
  if CommonTags.Comment <> '' then
     SetFrameValue('COMMENT', CommonTags.Comment, TAPEFrame);
  if CommonTags.Genre <> '' then
     SetFrameValue('GENRE', CommonTags.Genre, TAPEFrame);
  if CommonTags.Title <> '' then
     SetFrameValue('TITLE', CommonTags.Title, TAPEFrame);
  if CommonTags.TrackString <> '' then
     SetFrameValue('TRACKNUMBER', CommonTags.TrackString, TAPEFrame);
  if CommonTags.Year <> '' then
     SetFrameValue('DATE', CommonTags.Year, TAPEFrame);

end;

function TAPETags.ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean;
var
  header :  TAPEHeader;
  i: longint;
  Frame: TAPEFrame;
  offset: Integer;
  V1Rec: TID3V1Record;
begin
  Clear;
  result := false;
  FromV1 := false;
  try

    AStream.Seek(AStream.Size - SizeOf(V1Rec), soFromBeginning);
    AStream.Read(V1Rec,  SizeOf(V1Rec));

    if V1Rec.Header <> 'TAG' then
       Offset := 0
    else
       Offset := SizeOf(V1Rec);

    AStream.Seek(AStream.Size - SizeOf(Header) - offset, soFromBeginning);
    AStream.Read(Header, sizeof(Header));

   if String(Header.Marker) <> APE_IDENTIFIER then
      begin
        if offset = SizeOf(V1rec) then
           begin
             FromV1 := ImportFromID3V1(AStream);
             result:= FromV1;
           end;
        exit;
      end;

   AStream.Seek(AStream.Size - header.TagSize - offset,  soFromBeginning);

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

function TAPETags.WriteToStream(AStream: TStream): DWord;
var
  header :  TAPEHeader;
  i: longint;
  Totsize: DWOrd;
//  HeaderPos : integer;
  MemStream : Tmemorystream;
begin

//  HeaderPos:= AStream.Position;
  header.Marker := APE_IDENTIFIER;
  header.count:= Count;
  header.versionNumber:=2000;
  header.flags := 0;
  for i := 1 to 8 do
    header.reserved[i] := $0;
  header.flags := header.flags or (1 shl 31);
  header.flags := header.flags or (1 shl 29);

  MemStream := TMemoryStream.create;
  Totsize:=0;
  for i := 0 to Count -1 do
     Totsize := Totsize + Frames[i].WriteToStream(MemStream);

  header.TagSize:=Totsize + SizeOf(header);
  AStream.Write(header, SizeOf(header));
  MemStream.Position:=0;
  AStream.CopyFrom(MemStream, MemStream.size);
  MemStream.Free;

  header.flags := 0;
  header.flags :=  header.flags or (1 shl 31);
  AStream.Write(header, SizeOf(header));
  result := SizeOf(header) * 2 + Totsize;

end;

end.

