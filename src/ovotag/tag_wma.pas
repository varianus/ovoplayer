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
unit tag_wma;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseTag, LCLProc;

type

  { TWMAFrame }

  TWMAFrame = class(TFrameElement)
  private
    fValue: widestring;
    fValueInt: QWord;
  public
    DataType: Word;
    function GetAsString: string; override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsWideString(AValue: Widestring);
    function ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
    property AsWideString: WideString write SetAsWideString;
  end;

  { TWMATags }

  TWMATags = class(TTags)
  public
    Function GetCommonTags: TCommonTags; override;
    Procedure SetCommonTags(CommonTags: TCommonTags); override;
    function ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;


implementation

{ TWMATags }
uses CommonFunctions, Lazutf8;

function TWMATags.GetCommonTags: TCommonTags;
begin
  Result:=inherited GetCommonTags;

  Result.Album := GetFrameValue('WM/ALBUMTITLE');
  Result.AlbumArtist := GetFrameValue('WM/ALBUMARTIST');
  Result.Artist := GetFrameValue('AUTHOR');
  Result.Comment := GetFrameValue('WM/DESCRIPTION');
  Result.Genre := GetFrameValue('WM/GENRE');
  Result.Title := GetFrameValue('TITLE');
  Result.Track := ExtractTrack(GetFrameValue('WM/TRACKNUMBER'));
  Result.TrackString := GetFrameValue('WM/TRACKNUMBER');
  Result.Year := GetFrameValue('WM/YEAR');
  if Result.AlbumArtist = '' then
     Result.AlbumArtist := Result.Artist;
end;

procedure TWMATags.SetCommonTags(CommonTags: TCommonTags);
begin
  inherited SetCommonTags(CommonTags);
  if CommonTags.Album <> '' then
     SetFrameValue('WM/ALBUMTITLE', CommonTags.Album, TWMAFrame);
  if CommonTags.AlbumArtist <> '' then
     SetFrameValue('WM/ALBUMARTIST', CommonTags.AlbumArtist, TWMAFrame);
  if CommonTags.Artist <> '' then
     SetFrameValue('AUTHOR', CommonTags.Artist, TWMAFrame);
  if CommonTags.Comment <> '' then
     SetFrameValue('WM/DESCRIPTION', CommonTags.Comment, TWMAFrame);
  if CommonTags.Genre <> '' then
     SetFrameValue('WM/GENRE', CommonTags.Genre, TWMAFrame);
  if CommonTags.Title <> '' then
     SetFrameValue('TITLE', CommonTags.Title, TWMAFrame);
  if CommonTags.TrackString <> '' then
     SetFrameValue('WM/TRACKNUMBER', CommonTags.TrackString, TWMAFrame);
  if CommonTags.Year <> '' then
     SetFrameValue('WM/YEAR', CommonTags.Year, TWMAFrame);

end;


function TWMATags.ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean;
var
  ContentCount: word;
  i: word;
  Frame: TWMAFrame;
  oldFrame: TFrameElement;
begin
  Result := False;
  ContentCount := AStream.ReadWord;
  for i := 0 to ContentCount -1 do
  begin
    Frame := TWMAFrame.Create;
    if Frame.ReadFromStream(AStream) then
      begin
        oldFrame := Self.FramesByID[frame.ID];
        if Assigned(oldFrame) then
           Remove(OldFrame);
        add(Frame)
      end
    else
      FreeAndNil(Frame);
  end;
  Result := count > 0;
end;

function TWMATags.WriteToStream(AStream: TStream): DWord;
var
  i: word;
begin
 result := 2;
 AStream.WriteWord(Count);
 for i := 0 to Count -1 do
  result := result + Frames[i].WriteToStream(AStream);
end;

{ TWMAFrame }

function TWMAFrame.GetAsString: string;
begin
  case DataType of
    0: Result := Utf16toUtf8(fValue);
    1: Result := '<bytes>';
    2..5: Result := IntToStr(fValueInt);
  end;

end;

procedure TWMAFrame.SetAsString(const AValue: string);
begin
  case DataType of
    0: fValue := Utf8ToUtf16(AValue);
    1: raise Exception.Create('Unsupported');
    2..5: fValueInt := StrToInt(AValue);
  end;

end;

procedure TWMAFrame.SetAsWideString(AValue: Widestring);
begin
 case DataType of
   0: fValue := AValue;
   1: raise Exception.Create('Unsupported');
   2..5: fValueInt := StrToInt(AValue);
 end;
end;

function TWMAFrame.ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean;
var
  DataLength: word;
  tmpValue: array of char;
begin
  DataLength := AStream.ReadWord;
  SetLength(tmpValue, DataLength);
  AStream.Read(tmpValue[0], DataLength);
  DataType := AStream.ReadWord;
  ID := Widestring(tmpValue);

  DataLength := AStream.ReadWord;

  case DataType of
    2: fValueInt := AStream.ReadDWord;
    3: fValueInt := AStream.ReadDWord;
    4: fValueInt := AStream.ReadQWord;
    5: fValueInt := AStream.ReadWord;
    else
    begin
      SetLength(tmpValue, DataLength);
      AStream.Read(tmpValue[0], DataLength);
      fValue := Widestring(tmpValue);
    end;

  end;
  result:=true;
end;

function TWMAFrame.WriteToStream(AStream: TStream): DWord;
var
  DataLength: word;
  tmpValue: UnicodeString;
begin
  tmpValue:= id +#0;
  DataLength := Length(tmpValue) * SizeOf(WideChar);
  AStream.WriteWord(DataLength);
  AStream.Write(tmpValue[1], DataLength);
  AStream.WriteWord(DataType);

  Result := 2* SizeOf(Word) + DataLength;

  case DataType of
    2: begin
         AStream.WriteWord(SizeOf(DWORD));
         AStream.WriteDWord(fValueInt);
         Result := Result + 6;
       end;
    3: begin
         AStream.WriteWord(SizeOf(DWORD));
         AStream.WriteDWord(fValueInt);
         Result := Result + 6;
       end;
    4: begin
        AStream.WriteWord(SizeOf(QWORD));
        AStream.WriteQWord(fValueInt);
        Result := Result + 10;

       end;
    5: begin
        AStream.WriteWord(SizeOf(WORD));
        AStream.WriteWord(fValueInt);
        Result := Result + 4;
       end;
    else
    begin
      tmpValue:= fValue + #0;
      DataLength := Length(tmpValue) * SizeOf(WideChar);
      AStream.WriteWord(DataLength);
      if DataLength <> 0 then
        AStream.Write(tmpValue[1], DataLength);
      Result := Result + 2 + DataLength;
    end;
 end;

end;

end.

