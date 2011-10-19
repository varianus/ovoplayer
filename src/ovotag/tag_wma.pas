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
unit tag_wma;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseTag;

type

  { TWMAFrame }

  TWMAFrame = class(TFrameElement)
  private
    fValue: widestring;
    fValueInt: QWord;
  public
    DataType: Word;
    function GetAsString: string; override;
    procedure SetAsString(AValue: string); override;
    function ReadFromStream(AStream: TStream): boolean; override;
  end;

  { TWMATags }

  TWMATags = class(TTags)
  public
    Function GetCommonTags: TCommonTags; override;
    function ReadFromStream(AStream: TStream): boolean; override;
  end;


implementation

{ TWMATags }

function TWMATags.GetCommonTags: TCommonTags;
begin
  Result:=inherited GetCommonTags;


  Result.Album := GetFrameValue('WM/ALBUMTITLE');
  Result.AlbumArtist := GetFrameValue('WM/ALBUMARTIST');
  Result.Artist := GetFrameValue('WM/AUTHOR');
  Result.Comment := GetFrameValue('WM/DESCRIPTION');
//  Result.Duration := GetFrameValue('ARTIST');
  Result.Genre := GetFrameValue('WM/GENRE');
  Result.Title := GetFrameValue('WM/TITLE');
  Result.Track := StrToIntDef(GetFrameValue('WM/TRACKNUMBER'),0);;
  Result.TrackString := GetFrameValue('WM/TRACKNUMBER');
  Result.Year := GetFrameValue('WM/YEAR');
end;

function TWMATags.ReadFromStream(AStream: TStream): boolean;
var
  ContentCount: word;
  i: word;
  Frame: TWMAFrame;
begin
  Result := False;
  ContentCount := AStream.ReadWord;
  for i := 0 to ContentCount -1 do
  begin
    Frame := TWMAFrame.Create;
    if Frame.ReadFromStream(AStream) then
      add(Frame)
    else
      FreeAndNil(Frame);
  end;
  Result := count > 0;
end;

{ TWMAFrame }

function TWMAFrame.GetAsString: string;
begin
  case DataType of
    0: Result := fValue;
    1: Result := '<bytes>';
    2..5: Result := IntToStr(fValueInt);
  end;

end;

procedure TWMAFrame.SetAsString(AValue: string);
begin
  case DataType of
    0: fValue := AValue;
    1: raise Exception.Create('Unsupported');
    2..5: fValueInt := StrToInt(AValue);
  end;

end;

function TWMAFrame.ReadFromStream(AStream: TStream): boolean;
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

end.

