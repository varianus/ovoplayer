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
unit tag_MP4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseTag;

type
  { TMp4Tags }

  { TMp4Frame }

  TMp4Frame =  class(TFrameElement)
  Private
    fValue :string;
  protected
    function GetAsString: string;  override;
    procedure SetAsString(AValue: string); override;
  public
    function ReadFromStream(AStream: TStream): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;

  TMp4Tags = class(TTags)
  public
    Function GetCommonTags: TCommonTags; override;
    procedure SetCommonTags(CommonTags: TCommonTags); override;
    function ReadFromStream(AStream: TStream): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;


implementation

{ TMp4Frame }

function TMp4Frame.GetAsString: string;
begin

end;

procedure TMp4Frame.SetAsString(AValue: string);
begin
  //
end;

function TMp4Frame.ReadFromStream(AStream: TStream): boolean;
begin

end;

function TMp4Frame.WriteToStream(AStream: TStream): DWord;
begin
   result := 0;
end;

{ TMp4Tags }

function TMp4Tags.GetCommonTags: TCommonTags;
begin
  Result:=inherited GetCommonTags;
end;

procedure TMp4Tags.SetCommonTags(CommonTags: TCommonTags);
begin
  inherited SetCommonTags(CommonTags);
end;

function TMp4Tags.ReadFromStream(AStream: TStream): boolean;
var
  frame: TMp4Frame;
begin
  Clear;
  result := false;

  Frame := TMp4Frame.Create();
  frame.Tagger := self;




end;

function TMp4Tags.WriteToStream(AStream: TStream): DWord;
begin
  Result := AStream.size;
end;

end.

