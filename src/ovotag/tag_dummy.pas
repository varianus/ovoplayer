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
unit tag_Dummy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseTag;

type
  { TDummyTags }

  TDummyTags = class(TTags)
  public
    Function GetCommonTags: TCommonTags; override;
    procedure SetCommonTags(CommonTags: TCommonTags); override;
    function ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;


implementation
{ TDummyTags }

function TDummyTags.GetCommonTags: TCommonTags;
begin
  Result:=inherited GetCommonTags;
end;

procedure TDummyTags.SetCommonTags(CommonTags: TCommonTags);
begin
  inherited SetCommonTags(CommonTags);
end;

function TDummyTags.ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean;
begin
  Clear;
  result := false;
end;

function TDummyTags.WriteToStream(AStream: TStream): DWord;
begin
  Result := AStream.size;
end;

end.

