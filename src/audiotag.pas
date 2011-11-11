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
{$I ovoplayer.inc}
unit AudioTag;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Song,
  file_flac, file_mp3, file_wma, file_ogg,
  baseTag;

const
  Mp3FileMask: string    = '*.mpa;*.mp1;*.mp2;*.mp3;';
  MPPFileMask: string    = '*.mp+;*.mpc;';
  FlacFileMask: string   = '*.flac;';
  MonkeyFileMask: string = '*.mac;*.ape;';
  WMAFileMask: string    = '*.wma;';
  OGGFileMask: string    = '*.ogg;';
  WAVFileMask: string    = '*.wav;';

var
  SupportedExtension: string;


type
  TSongFormat = (sfUnsupported, sfAAC, sfAC3, sfAPE, sfCDA, sfFLAC, sfMP3, sfMonkey, sfOGG, sfWMA);


function LoadTags(var Song: Tsong): boolean;
function ExtractTags(FileName: string): TCommonTags; overload;
function ExtractTags(FileTags: TTagReader): TCommonTags; overload;
function GetFileTagsObject(FileName: string): TTagReader;
function IdentifyKind(FileName: string): TSongFormat;

implementation

function LoadTags(var Song: Tsong): boolean;
begin
  Song.SetTags(ExtractTags(Song.FullName));
  Song.TagLoaded := True;
  Result := Song.Tags.Title = '';

end;

function GetFileTagsObject(FileName: string): TTagReader;
  var
    kind: TSongFormat;
  begin
    kind := IdentifyKind(FileName);
    Result := nil;
    case kind of
      sfMP3:  Result := TMP3Reader.Create(FileName);
      sfWMA:  Result := TWMAReader.Create(FileName);
      sfFLAC: Result := TFlacReader.Create(FileName);
      sfOGG: Result := TOGGReader.Create(FileName);
    else
      Result:= TTagReader.Create(FileName); // FallBack
    end;
  end;


function IdentifyKind(FileName: string): TSongFormat;
var
  ext: string;
begin
  ext    := lowercase(ExtractFileExt(Filename));

  if Pos(ext, Mp3FileMask) > 0 then
     Result := sfMP3
  else
  if Pos(ext, FlacFileMask) > 0 then
     Result := sfFLAC
  else
  if Pos(ext, WMAFileMask) > 0 then
     Result := sfWMA
  else
  if Pos(ext, OGGFileMask) > 0 then
     Result := sfOGG
  else
     Result := sfUnsupported;

end;

function ExtractTags(FileTags: TTagReader): TCommonTags;
begin
  if FileTags = nil  then
     exit;

  Result := FileTags.GetCommonTags;
end;


function ExtractTags(FileName: string): TCommonTags;
var
  InteTags: TTagReader;
begin
  InteTags:= GetFileTagsObject(FileName);
  result := ExtractTags(InteTags);
  freeAndNil(InteTags);
end;

initialization
  SupportedExtension := Mp3FileMask
                      + OGGFileMask
                      + FlacFileMask
                      + WMAFileMask
                      ;
end.
