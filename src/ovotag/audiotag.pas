{
This file is part of OvoTag
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
{$I ovotag.inc}
unit AudioTag;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Song,
  baseTag;

function SupportedExtension: string;

Procedure RegisterTagReader(const Extensions: string;
                            const TagReader: TTagReaderClass);

function LoadTags(Song: TSong): boolean;
function ExtractTags(FileName: string): TCommonTags; overload;
function ExtractTags(FileTags: TTagReader): TCommonTags; overload;
function GetFileTagsObject(FileName: string): TTagReader;
function IdentifyKind(FileName: string): TTagReaderClass;

implementation
uses strutils, LazFileUtils, file_Dummy;

type
  RTagReader = record
    Extensions : String;
    TagReader : TTagReaderClass;
  end;

var
  AReaderList : array of RTagReader;

function SupportedExtension: string;
var
  i:Integer;
begin
  result:= '';
  for i := Low(AReaderList) to High(AReaderList) do
     result:= Result + AReaderList[i].Extensions;

end;

procedure RegisterTagReader(const Extensions: string;
  const TagReader: TTagReaderClass);
var
  tr: RTagReader;
begin
  tr.Extensions:=Extensions;
  tr.TagReader:=TagReader;
  SetLength(AReaderList, Length(AReaderList) +1);
  AReaderList[High(AReaderList)] := tr;
end;

function LoadTags(Song: Tsong): boolean;
begin
  Result := FileExistsUTF8(Song.FullName);
  if result and not (song.TagLoaded) then
     begin
       Song.SetTags(ExtractTags(Song.FullName));
       Song.TagLoaded := True;
       Result := Song.Tags.Title = '';
     end;

end;

function GetFileTagsObject(FileName: string): TTagReader;
var
  kind: TTagReaderClass;
begin
  kind := IdentifyKind(FileName);
  Result:= kind.Create(FileName); // FallBack
end;

function IdentifyKind(FileName: string): TTagReaderClass;
var
  ext: string;
  i : Integer;
begin
  Result := TDummyReader;
  if AnsiStartsStr('HTTP:\\', UpperCase(FileName)) or
     AnsiStartsStr('MMS:\\', UpperCase(FileName)) then
     begin
       exit;
     end;

  ext := lowercase(ExtractFileExt(Filename));

  for i := Low(AReaderList) to High(AReaderList) do
     if Pos(ext, AReaderList[i].Extensions) > 0 then
        begin
           result := AReaderList[i].TagReader;
           exit;
        end;
end;

function ExtractTags(FileTags: TTagReader): TCommonTags;
begin
  Result := Default(TCommonTags);
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
  InteTags.free;
end;

initialization
  setlength(AReaderList, 0);
  RegisterTagReader(DummyFileMask, TDummyReader);
finalization
  setlength(AReaderList, 0);
end.
