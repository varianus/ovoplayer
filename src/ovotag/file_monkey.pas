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
unit file_Monkey;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AudioTag, baseTag, tag_APE;

const
  MonkeyFileMask: string    = '*.ape;';

type

  { TMonkeyReader }

  TMonkeyReader = class(TTagReader)
  private
    fTags: TAPETags;
    FSampleRate: integer;
    FSamples: int64;
  protected
    function GetDuration: int64; override;
    function GetTags: TTags; override;
  public
    function LoadFromFile(FileName: Tfilename): boolean; override;
  end;

implementation

{ TMonkeyReader }

function TMonkeyReader.GetDuration: int64;
begin
  if (FSampleRate > 0) then
  begin
    Result := trunc((FSamples / FSampleRate) * 1000);
  end
  else
  begin
    Result := 0;
  end;
end;

function TMonkeyReader.GetTags: TTags;
begin
  Result := fTags;
end;

function TMonkeyReader.LoadFromFile(FileName: Tfilename): boolean;
var
  fStream: TFileStream;
  Start: byte;
  i:Integer;

begin
  Result := inherited LoadFromFile(FileName);
  fStream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
  try
    ftags := TAPETags.Create;
    Result := ftags.ReadFromStream(fStream);
  finally
    fstream.Free;
  end;

end;

initialization
  RegisterTagReader(MonkeyFileMask, TMonkeyReader);

end.

