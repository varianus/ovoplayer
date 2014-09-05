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
    fFlags: DWord;
    Data: RawByteString;
    fSize: Integer;
  protected
    function GetAsString: string;  override;
    procedure SetAsString(const AValue: string); override;
    function GetSize: DWord; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;

  TMp4Tags = class(TTags)
  public
    Function GetCommonTags: TCommonTags; override;
    procedure SetCommonTags(CommonTags: TCommonTags); override;
    function ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;


implementation

{ TMp4Frame }
uses
  file_Mp4, CommonFunctions;

const
  knowntagCount = 10;
  knowntag: array [0..knowntagCount-1] of string = (
 #169+'nam', #169+'cmt', #169+'day', #169'ART', #169+'trk',
 #169+'alb', #169+'gen',  'gnre',   'trkn', 'aART'
 );

function TMp4Frame.GetAsString: string;
begin
  if (fFlags and $00ffffff) = 1 then
    Result := (Data);

  if (fFlags and $00ffffff) = 0 then
    begin
      if fSize = 24 then
         Result := Inttostr(BEtoN(PDWord(@data[1])^))
      else
         Result := Inttostr(BEtoN(PWord(@data[1])^));
    end;


end;

procedure TMp4Frame.SetAsString(const AValue: string);
begin
  //
end;

function TMp4Frame.GetSize: DWord;
begin
  Result:= fSize;
end;

constructor TMp4Frame.Create;
begin
  inherited Create;
end;

destructor TMp4Frame.Destroy;
begin
  SetLength(Data, 0);
  inherited Destroy;
end;

function TMp4Frame.ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean;
var
  Atom: TMp4Atom;
  wName: AtomName;
begin
  SetLength(Data, 0);
  Atom := TMp4Atom(ExtInfo);
  fSize := BEtoN(AStream.ReadDWord);
  AStream.Read(wName, 4);
  fflags := BEtoN(AStream.ReadDWord);
  AStream.ReadDWord;
  SetLength(Data, fSize - 16 + 1);
  AStream.Read(Data[1], fSize - 16+1 );
  Result:= true;

end;

function TMp4Frame.WriteToStream(AStream: TStream): DWord;
begin
   result := 0;
end;

{ TMp4Tags }

function TMp4Tags.GetCommonTags: TCommonTags;
var
  tmpst: string;
  xInt: Longint;
begin
// #169+'nam', #169+'cmt', #169+'day', #169'ART', #169+'trk',
// #169+'alb', #169+'gen',  'gnre',   'trkn'

  Result:=inherited GetCommonTags;
  result.Album:= GetFrameValue(#169+'alb');
  result.Artist:= GetFrameValue(#169+'ART');
  result.AlbumArtist:= GetFrameValue('aART');
  if result.AlbumArtist = '' then
    result.AlbumArtist := result.Artist;
  result.Comment:= GetFrameValue(#169+'cmt');
  result.Title:= GetFrameValue(#169+'nam');
  result.Genre := '';
  tmpst := GetFrameValue(#169+'gen');
  if tmpst <> '' then
     result.Genre:= ExtractGenre(tmpst, -1);

  if result.Genre = '' then
    result.Genre:= ExtractGenre(GetFrameValue('gnre'), -1);

  result.Year:= ExtractYear('',GetFrameValue(#169+'day'));
  Result.TrackString := GetFrameValue(#169+'trk');
  tmpst:= GetFrameValue('trkn');
  if not TryStrToInt(tmpst, Xint) then
     result.track:= ExtractTrack(GetFrameValue(#169+'trk'))
  else
     result.track := Xint;
end;

procedure TMp4Tags.SetCommonTags(CommonTags: TCommonTags);
begin
  inherited SetCommonTags(CommonTags);
end;

function TMp4Tags.ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean;
var
  frame: TMp4Frame;
  i, j : integer;
  ilst,CurrAtom : TMp4Atom;
begin
  Clear;
  result := false;
  ilst := TMp4Atom(ExtInfo);
  if Assigned(ilst) then
     for i := 0 to ilst.children.Count -1 do
       begin
           CurrAtom:= Tmp4Atom(ilst.children[i]);
           for j:= 0 to knowntagCount -1 do
             if CurrAtom.Name = knowntag[j] then
               begin
                AStream.Seek(CurrAtom.Offset + 8, soFromBeginning);
                Frame:= TMp4Frame.Create;
                Frame.Tagger :=self;
                frame.id := CurrAtom.Name;
                frame.ReadFromStream(AStream, CurrAtom);
                Add(Frame);
               end;
       end;
  result:= true;
end;

function TMp4Tags.WriteToStream(AStream: TStream): DWord;
begin
  Result := AStream.size;
end;

end.

