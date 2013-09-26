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
  Mp4FileMask: string    = '*.aac;*.m4a;';

type
  AtomName = array [0..3] of char;

  RAtomHeader = record
    Size: dword;
    Name: AtomName;
  end;


type
  { TMp4Atom }

  TMp4Atom = class;

  { TMP4AtomList }

  TMP4AtomList = class(TFPList)
  private
  public
    function Find(Name0: AtomName='';Name1: AtomName='';Name2: AtomName='';Name3: AtomName=''): TMp4Atom;
    constructor Create(Stream: TStream); overload;
    constructor Create;  overload;

    Destructor Destroy; override;
  end;

  TMp4Atom = class
    Public
      Offset: int64;
      AtomLength: int64;
      children : TMP4AtomList;
      Name: string;
      function Find(Name0: AtomName='';Name1: AtomName='';Name2: AtomName='';Name3: AtomName='') :TMp4Atom;
      function FindAll(AName:AtomName; Recursive:boolean=false) :TMp4AtomList;
      constructor Create(Stream: TStream);
      Destructor Destroy; override;
  end;

  { TMp4Reader }

  TMp4Reader = class(TTagReader)
  private
    fTags: TTags;
    FSampleRate: integer;
    FSamples: int64;
    fDuration : int64;
    //function FindAtom(Stream: TFileStream; AtomName: string; var Atom: RAtomHeader): boolean;
  protected
    function GetDuration: int64; override;
    function GetTags: TTags; override;
  public
    function LoadFromFile(AFileName: Tfilename): boolean; override;
    function SaveToFile(AFileName: Tfilename): boolean; override;
  end;


implementation

{ TMp4Atom }
const
   containers: array [0..10] of string = (
    'moov', 'udta', 'mdia', 'meta', 'ilst',
    'stbl', 'minf', 'moof', 'traf', 'trak',
    'stsd');

   knowntag: array [0..8] of string = (
    #a9'nam', #a9'cmt', #9'day', #a9'ART', #a9'trk',
    #a9'alb', #a9'gen',  'gnre',   'trkn'
    );

  numContainers= 11;

{ TMP4AtomList }

constructor TMP4AtomList.Create(Stream: TStream);
var
  end_:Int64;
  Atom : TMp4Atom;
begin
  Inherited Create;
  end_ := Stream.Size;
  Stream.Seek(0, soFromBeginning);
  while Stream.Position +8 <= end_ do
    begin
      Atom := TMp4Atom.Create(Stream);
      Add(Atom);
      if Atom.AtomLength = 0 then
        Break;

    end;


end;

destructor TMP4AtomList.Destroy;
var
  i: integer;
begin
  for i := 0 to Count -1 do
    TMp4Atom(Items[i]).Free;
  Clear;
  inherited Destroy;
end;

function TMP4AtomList.Find(Name0: AtomName='';Name1: AtomName='';Name2: AtomName='';Name3: AtomName=''): TMp4Atom;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
    if (TMp4Atom(Items[i]).name = name0) then
      begin
         result:= TMp4Atom(Items[i]).find(Name1,name2,name3);
         exit;
      end;

  result := nil;
end;

function TMp4Atom.Find(Name0: AtomName='';Name1: AtomName='';Name2: AtomName='';Name3: AtomName=''): TMp4Atom;
var
  i: Integer;
begin
  if name0 = '' then
    begin
     result := self;
     exit;
  end;
  for i := 0 to children.Count -1 do
    if TMp4Atom(children[i]).name = name0 then
      begin
         result:= TMp4Atom(children[i]).find(Name1,name2,name3);
         exit;
      end;

  result := nil;
end;

function TMp4Atom.FindAll(AName: AtomName; Recursive: boolean): TMp4AtomList;
var
  i,j: Integer;
  tmpList : TMp4AtomList;
begin
  Result := TMP4AtomList.Create;
  for  i := 0 to children.count -1 do
    begin
    if (TMp4Atom(children[i]).name = Aname) then
      result.add(children[i]);

    if (recursive) then
      begin
        TmpList := TMp4Atom(children[i]).findall(Aname, recursive);
        for j := 0 to tmpList.Count -1 do
          Result.Add(tmpList[j]);
        tmpList.Free;
      end;
  end;
end;


constructor TMp4Atom.Create(Stream: TStream);
var
  Header: RAtomHeader;
  Transferred: Integer;
  i: Integer;
  Child : TMp4Atom;
  longLength: Int64;
begin
  offset := Stream.Position;
  Transferred := Stream.Read(Header, SizeOf(Header));
  if Transferred < 8 then
    begin
      AtomLength:=0;
      Stream.Seek(0, soFromEnd);
      exit;
    end;

  AtomLength:=BEtoN(Header.Size);

  if (AtomLength = 1) then
    begin
      longLength:=beton(Stream.ReadQWord);
      if (longLength >= 8)  and (longLength <= $FFFFFFFF) then
          // The atom has a 64-bit AtomLength, but it's actually a 32-bit value
          AtomLength := longLength
      else
        begin
          //debug("MP4: 64-bit atoms are not supported");
          AtomLength := 0;
          Stream.seek(0, soFromEnd);
          exit;
        end;
  end;

  if (AtomLength < 8) then
    begin
//    debug("MP4: Invalid atom size");
       AtomLength := 0;
       Stream.seek(0, soFromEnd);
       exit;
  end;

  Name := Header.Name;
  children:= TMP4AtomList.Create;

  for  i := 0 to  numContainers -1 do
     begin
     if (name = containers[i]) then
       begin
         if (name = 'meta') then
           stream.seek(4, soFromCurrent)
         else if (name = 'stsd') then
           stream.seek(8, soFromCurrent);

        while (Stream.Position < offset + AtomLength) do
          begin
            child := TMP4Atom.Create(Stream);
            children.Add(child);
            if (child.AtomLength = 0) then
              exit;
          end;
         exit;
       end;
     end;

   stream.seek(offset + AtomLength, soFromBeginning);
end;

constructor TMp4AtomList.Create;
begin
  clear;
end;

destructor TMp4Atom.Destroy;
var
  i : Integer;
begin
  for i := 0 to children.Count -1 do
     TMp4Atom(children[i]).Free;
  children.Clear;

end;


{ TMp4Reader }

function TMp4Reader.GetDuration: int64;
begin
  Result := fDuration;
end;

function TMp4Reader.GetTags: TTags;
begin
  Result := fTags;
end;

function TMp4Reader.LoadFromFile(AFileName: Tfilename): boolean;
var
  fStream: TFileStream;
  Transferred: DWord;
  AtomList, traks:TMP4AtomList;
  vers: DWord;
  i: integer;
  moov, trak, mdhd, ilst: TMp4Atom;
  Data: array of byte;
  version:byte;
  unit_, length_:int64;
begin
  Result := inherited LoadFromFile(AFileName);
  fStream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
  fTags := TMp4Tags.Create;
  AtomList:=TMP4AtomList.Create(fStream);
  moov := AtomList.Find('moov');
  trak:= moov.Find('trak');
  mdhd:=trak.find('mdia','mdhd');
  SetLength(Data, mdhd.AtomLength);
  fStream.Seek(mdhd.Offset, soFromBeginning);
  Transferred := fStream.Read(Data[0], mdhd.AtomLength);
  Version := Data[8];
  if version = 1 then
    begin
       unit_ := beton(pInt64(@data[28])^) * 1000;
       length_ := beton(pInt64(@data[36])^) * 1000;
       fDuration:= length_ div unit_;

    end
  else
    begin
      unit_ := beton(PInteger(@data[20])^) * 1000;
      length_ := beton(PInteger(@data[24])^) * 1000;
      fDuration:= length_ div unit_;
    end ;

  ilst := AtomList.find('moov', 'udta', 'meta', 'ilst');
  if Assigned(ilst) then
     for i := 0 to ilst.children.Count -1 do
       begin

       end;


  AtomList.Free;
  SetLength(Data, 0);

  fstream.Free;

end;

function TMp4Reader.SaveToFile(AFileName: Tfilename): boolean;
begin
  Result:=False;
end;

initialization

  RegisterTagReader(Mp4FileMask, TMP4Reader);
end.

