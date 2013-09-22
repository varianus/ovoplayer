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
unit file_Mp4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AudioTag, baseTag, tag_Mp4;

const
  Mp4FileMask: string    = '*.aac;*.m4a';

type
  RAtomHeader = record
    Size: dword;
    Name: array [0..3] of char;
  end;

type

  { TMp4Reader }

  TMp4Reader = class(TTagReader)
  private
    fTags: TTags;
    FSampleRate: integer;
    FSamples: int64;
    function FindAtom(Stream: TFileStream; AtomName: string;
      var Atom: RAtomHeader): boolean;
  protected
    function GetDuration: int64; override;
    function GetTags: TTags; override;
  public
    function LoadFromFile(AFileName: Tfilename): boolean; override;
    function SaveToFile(AFileName: Tfilename): boolean; override;
  end;


implementation

{ TMp4Reader }

function TMp4Reader.GetDuration: int64;
begin
  Result := 0;
end;

function TMp4Reader.GetTags: TTags;
begin
  Result := fTags;
end;

function TMp4Reader.FindAtom(Stream:TFileStream; AtomName:string; var Atom: RAtomHeader ): boolean;
var
  Found:boolean;
  fAtom :RAtomHeader;
  Transferred:Integer;

begin
  Result:= false;
  found:= false;
  repeat
    Transferred:= Stream.Read(fAtom, SizeOf(RAtomHeader));
    fAtom.Size:= BEtoN(fAtom.Size);
    if fAtom.Name = AtomName then
       begin
         Result:= true;
         Atom:= Fatom;
         Found:= true;
       end
    else
      Stream.Seek(fAtom.Size -8, soCurrent);

  until (fatom.size= 0) or found or (Transferred < SizeOf(RAtomHeader));

end;

function TMp4Reader.LoadFromFile(AFileName: Tfilename): boolean;
var
  fStream: TFileStream;
  Transferred: DWord;
  Atom:RAtomHeader;
  vers: DWord;

begin
  Result := inherited LoadFromFile(AFileName);
  fStream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
  fTags := TMp4Tags.Create;

  while FindAtom(fStream, 'moov', atom) do
     while FindAtom(fStream, 'udta', atom) do
        while FindAtom(fStream, 'meta', atom) do
          begin
          Vers := fStream.ReadDWord;
          if FindAtom(fStream, 'ilst', atom) then
           begin
             fTags.ReadFromStream(fStream);
           end;

          end;

  fstream.Free;


end;

function TMp4Reader.SaveToFile(AFileName: Tfilename): boolean;
begin
  Result:=False;
end;

initialization

  RegisterTagReader(Mp4FileMask, TMP4Reader);
end.

