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
unit tag_vorbis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseTag;

type

  { TVorbisFrame }

  TVorbisFrame = class(TFrameElement)
  Private
    fValue :string;
  protected
    function GetAsString: string;  override;
    procedure SetAsString(const AValue: string); override;
  public
    function ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;

  { TVorbisTags }

  TVorbisTags = class(TTags)
  private
  public
    Vendor: string;
    function ReadImageFromStream(AStream: TStream): boolean;
    Function GetCommonTags: TCommonTags; override;
    Procedure SetCommonTags(CommonTags :TCommonTags); override;
    function ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;


implementation
uses CommonFunctions;
{ TVorbisFrame }

function TVorbisFrame.GetAsString: string;
begin
  Result:=fValue;
end;

procedure TVorbisFrame.SetAsString(const AValue: string);
begin
  fValue:= AValue;
end;

function TVorbisFrame.ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean;
var
  fSize:Cardinal;
  Data: RawByteString;
  iSep :Integer;
  TmpSt:String;
begin
  Result := false;
  fSize:= AStream.ReadDWord;
  SetLength(Data, fSize);
  AStream.Read(Data[1], fSize);
  tmpst := (data);
  iSep := pos('=',tmpst);
  if iSep > 0 then
     begin
       ID :=UpperCase(Copy(tmpst, 1, iSep - 1));
       fValue := Copy(tmpst, iSep + 1, MaxInt);
       Result:=true;
     end;
end;

function TVorbisFrame.WriteToStream(AStream: TStream): DWord;
var
  fSize:Cardinal;
  Data: String;
//  iSep :Integer;
begin
  Result := 0;
  Data:= (ID+'='+fValue);
  fSize:= Length(Data);
  AStream.WriteDWord(fSize);
  AStream.Write(Data[1], fSize);
  Result:=fSize;
end;

{ TVorbisTags }

function TVorbisTags.ReadImageFromStream(AStream: TStream): boolean;
var
  img : TImageElement;
  size: DWord;
  tmpstr: array of Ansichar;
begin
  img := TImageElement.Create;
  img.PictureType:= BetoN(AStream.ReadDWord);

  Size := BetoN(AStream.ReadDWord);
  SetLength(tmpstr, size);
  AStream.read(tmpstr[0], size);
  img.MIMEType:= strpas(@tmpstr[0]);

  Size := BetoN(AStream.ReadDWord);
  SetLength(tmpstr, size);
  if size <> 0 then
     begin
       AStream.read(tmpstr[0], size);
       img.Description:=Utf8ToAnsi(strpas(@tmpstr[0]));
     end;

  AStream.ReadDWord; // width
  AStream.ReadDWord; // heigth
  AStream.ReadDWord; // Color depth
  AStream.ReadDWord; // number of color

  Size := BetoN(AStream.ReadDWord);
  img.Image.CopyFrom(AStream, Size);
  AddImage(img);
  Result:= true;
end;

function TVorbisTags.GetCommonTags: TCommonTags;
begin
  Result:=inherited GetCommonTags;

  Result.Album := GetFrameValue('ALBUM');
  Result.AlbumArtist := GetFrameValue('ALBUMARTIST');
  Result.Artist := GetFrameValue('ARTIST');
  Result.Comment := GetFrameValue('COMMENT');
  Result.Genre := GetFrameValue('GENRE');
  Result.Title := GetFrameValue('TITLE');
  Result.Track := ExtractTrack(GetFrameValue('TRACKNUMBER'));
  Result.TrackString := GetFrameValue('TRACKNUMBER');
  Result.Year := GetFrameValue('DATE');

  if Result.AlbumArtist = '' then
     Result.AlbumArtist := result.Artist;

  Result.HasImage := ImageCount > 0;
end;

procedure TVorbisTags.SetCommonTags(CommonTags: TCommonTags);
begin
  inherited SetCommonTags(CommonTags);
  if CommonTags.Album <> '' then
     SetFrameValue('ALBUM', CommonTags.Album, TVorbisFrame);
  if CommonTags.AlbumArtist <> '' then
     SetFrameValue('ALBUMARTIST', CommonTags.AlbumArtist, TVorbisFrame);
  if CommonTags.Artist <> '' then
     SetFrameValue('ARTIST', CommonTags.Artist, TVorbisFrame);
  if CommonTags.Comment <> '' then
     SetFrameValue('COMMENT', CommonTags.Comment, TVorbisFrame);
  if CommonTags.Genre <> '' then
     SetFrameValue('GENRE', CommonTags.Genre, TVorbisFrame);
  if CommonTags.Title <> '' then
     SetFrameValue('TITLE', CommonTags.Title, TVorbisFrame);
  if CommonTags.TrackString <> '' then
     SetFrameValue('TRACKNUMBER', CommonTags.TrackString, TVorbisFrame);
  if CommonTags.Year <> '' then
     SetFrameValue('DATE', CommonTags.Year, TVorbisFrame);

end;

function TVorbisTags.WriteToStream(AStream: TStream): DWord;
var
  fSize: cardinal;
//  Data: array of char;
  i: cardinal;
//  Frame: TVorbisFrame;
begin
  fSize:= Length(Vendor);

  if fSize = 0 then
     AStream.WriteDWord(FSize)
  else
    begin
      inc(fSize);
      AStream.WriteDWord(FSize);
      AStream.Write(Vendor[1], fSize);
    end;


  AStream.WriteDWord(Count);
  for i := 0 to Count - 1 do
    Frames[i].WriteTostream (AStream);

  result := SizeOf(DWord) * 2;

end;

function TVorbisTags.ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean;
var
  fSize: cardinal;
  Data: array of char;
  FrameCount: cardinal;
  i: cardinal;
  Frame: TVorbisFrame;
begin
  Clear;
  fSize := AStream.ReadDWord;
  if fsize <> 0 then
     begin
       SetLength(Data, fSize);
       AStream.Read(Data[0], fSize);
       Vendor := string(Data);
     end;

  FrameCount := AStream.ReadDWord;

  for i := 0 to FrameCount - 1 do
  begin
    Frame := TVorbisFrame.Create;
    if Frame.ReadFromStream(AStream) then
       add(Frame)
    else
      FreeAndNil(Frame);
  end;
  Result := Count > 0;
end;

end.

