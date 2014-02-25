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
  Classes, lazutf8classes, SysUtils, AudioTag, baseTag, tag_APE;

const
  MonkeyFileMask: string    = '*.ape;';

type

  { TMonkeyReader }

  TMonkeyReader = class(TTagReader)
  private
    fTags: TTags;
    FSampleRate: integer;
    FSamples: int64;
  protected
    function GetDuration: int64; override;
    function GetTags: TTags; override;
  public
    function isUpdateable: boolean; override;
    function LoadFromFile(AFileName: Tfilename): boolean; override;
    function SaveToFile(AFileName: Tfilename): boolean; override;
  end;

implementation

uses tag_id3v2;

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

function TMonkeyReader.isUpdateable: boolean;
begin
  Result:=true;
end;

function TMonkeyReader.LoadFromFile(AFileName: Tfilename): boolean;
var
  fStream: TFileStreamUTF8;
  tempTags : TTags;
  HaveID3V2 :boolean;

begin
  Result := inherited LoadFromFile(AFileName);
  fStream := TFileStreamUTF8.Create(fileName, fmOpenRead or fmShareDenyNone);
  try
    tempTags := TID3Tags.Create;
    HaveID3V2 := tempTags.ReadFromStream(fStream);
    fStream.Position:=0;
    ftags := TAPETags.Create;
    Result := ftags.ReadFromStream(fStream);
    if (not result) and HaveID3V2 then
       begin
         fTags.free;
         fTags := tempTags;
       end;
  finally
    fstream.Free;
  end;

end;

function TMonkeyReader.SaveToFile(AFileName: Tfilename): boolean;
var
  SourceStream: TFileStreamUTF8;
  DestStream: TFileStreamUTF8;
  v1rec : TID3V1Record;
  offset : cardinal;
  header : TAPEHeader;
begin
  Result:=inherited SaveToFile(AFileName);

  SourceStream := TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyNone);
  DestStream := TFileStreamUTF8.Create(AFileName, fmCreate or fmOpenReadWrite or fmShareDenyNone);

  try
    SourceStream.Seek(SourceStream.Size - SizeOf(TID3V1Record), soFromBeginning);
    SourceStream.Read(V1Rec,  SizeOf(TID3V1Record));

    if V1Rec.Header <> 'TAG' then
       Offset := 0
    else
       Offset := SizeOf(V1Rec);

    SourceStream.Seek(SourceStream.Size - SizeOf(TAPEHeader) - offset, soFromBeginning);
    SourceStream.Read(Header, sizeof(TAPEHeader));

    if String(Header.Marker) = APE_IDENTIFIER then
      begin
        Offset := offset + header.TagSize;
        SourceStream.Seek(SourceStream.Size - SizeOf(Header) - offset, soFromBeginning);
        SourceStream.Read(Header, sizeof(Header));
        if String(Header.Marker) = APE_IDENTIFIER then
           Offset := offset + sizeof(Header);
      end;

   SourceStream.Position := 0;
   DestStream.CopyFrom(SourceStream, SourceStream.Size - offset);
   Tags.WriteToStream(DestStream);
   Result := True;
  finally
    SourceStream.Free;
    DestStream.Free;
  end;


end;

initialization
  RegisterTagReader(MonkeyFileMask, TMonkeyReader);

end.

