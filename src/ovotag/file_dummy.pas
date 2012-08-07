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
unit file_Dummy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AudioTag, baseTag, tag_Dummy;

const
  DummyFileMask: string    = '*.aac;';

type

  { TDummyReader }

  TDummyReader = class(TTagReader)
  private
    fTags: TTags;
    FSampleRate: integer;
    FSamples: int64;
  protected
    function GetDuration: int64; override;
    function GetTags: TTags; override;
  public
    function LoadFromFile(AFileName: Tfilename): boolean; override;
    function SaveToFile(AFileName: Tfilename): boolean; override;
  end;

implementation

{ TDummyReader }

function TDummyReader.GetDuration: int64;
begin
  Result := 0;
end;

function TDummyReader.GetTags: TTags;
begin
  Result := fTags;
end;

function TDummyReader.LoadFromFile(AFileName: Tfilename): boolean;
begin
  Result := false;
  fTags := TDummyTags.Create;
end;

function TDummyReader.SaveToFile(AFileName: Tfilename): boolean;
begin
  Result:=False;
end;

initialization
  RegisterTagReader(DummyFileMask, TDummyReader);
end.

