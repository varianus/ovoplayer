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
unit song;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basetag;

type
  TTagKind = (tkNone, tkAlbum, tkAlbumArtist, tkArtist, tkSong, tkYear, tkGenre);

type
  { TSong }

  PSong = ^TSong;

  TSong = class
  private
    FExtraProperty: TObject;
    FFullName:  string;
    FName:      string;
    fFilePath:  string;
    FFileName:  string;
    FTagLoaded: boolean;
    FTags:      TCommonTags;
    function GetTags: TCommonTags;
    function GetTitle: string;
    procedure SetExtraProperty(AValue: TObject);
    procedure SetFullName(const AValue: string);
    procedure SetName(const AValue: string);
    procedure SetTagLoaded(const AValue: boolean);
  public
    constructor Create(aFileName: string);
    destructor Destroy; override;
    procedure SetTags(const AValue: TCommonTags);
    Function LoadTags: boolean;
  public
    property FilePath: string read FFilePath;
    property FileName: TFileName read FFileName;
    property Title: string read GetTitle;
    property Tags: TCommonTags read GetTags;
    property TagLoaded: boolean read FTagLoaded write SetTagLoaded;
    property FullName: string read FFullName write SetFullName;
    property ExtraProperty :TObject read FExtraProperty write SetExtraProperty;
  end;

function TagValue(Tags: TCommonTags; Kind: TTagKind): string;

implementation

{ TSong }
uses AudioTag;

function TagValue(Tags: TCommonTags; Kind: TTagKind): string;
begin
  case Kind of
    tkAlbum: Result  := Tags.Album;
    tkAlbumArtist: Result := Tags.AlbumArtist;
    tkArtist: Result := Tags.Artist;
    tkSong: Result   := Tags.Title;
    tkYear: Result   := Tags.Year;
    tkGenre: Result  := Tags.Genre;
    tkNone: Result   := '';

    end;
end;

function TSong.GetTitle: string;
begin
  if fTagLoaded and (FTags.Title <> '') then
    Result := FTags.Title
  else
    Result := FFileName;
end;

procedure TSong.SetExtraProperty(AValue: TObject);
begin
  if FExtraProperty=AValue then Exit;
  FExtraProperty:=AValue;
end;

function TSong.GetTags: TCommonTags;
begin
  if not FTagLoaded then
    AudioTag.LoadTags(Self);

  Result := FTags;

end;

procedure TSong.SetFullName(const AValue: string);
begin
  if FFullName = AValue then
    exit;
  FFullName := AValue;
end;

procedure TSong.SetName(const AValue: string);
begin
  if FName = AValue then
    exit;
  FName := AValue;
end;

procedure TSong.SetTagLoaded(const AValue: boolean);
begin
  if FTagLoaded = AValue then
    exit;
  FTagLoaded := aValue;
end;

procedure TSong.SetTags(const AValue: TCommonTags);
begin
  FTags := AValue;
end;

function TSong.LoadTags: boolean;
begin
  Result := AudioTag.LoadTags(Self);

end;

constructor TSong.Create(aFileName: string);
begin
  FFilePath  := extractfilepath(aFileName);
  FFileName  := ExtractFileName(AfileName);
  FFullName  := aFileName;
  FTags.ID   := -1;
  FTagLoaded := False;
  FExtraProperty := nil;
end;

destructor TSong.Destroy;
begin
  if Assigned(FExtraProperty) then
     FExtraProperty.Free;
  inherited Destroy;
end;

end.