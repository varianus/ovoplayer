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
unit file_ogg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseTag, tag_Vorbis;

type

  { TFlacReader }

  TOGGReader = class(TTagReader)
  private
    fTags: TVorbisTags;
    FSampleRate: integer;
    FSamples: int64;
  protected
    function GetDuration: int64; override;
    function GetTags: TTags; override;
  public
    function LoadFromFile(FileName: Tfilename): boolean; override;
  end;

implementation

{ TFlacReader }
const
  OGG_IDENTIFIER :string = 'OggS';

const
  BLOCK_TYPE_STREAMINFO = 0;
  BLOCK_TYPE_PADDING = 1;
  BLOCK_TYPE_APPLICATION = 2;
  BLOCK_TYPE_SEEKTABLE = 3;
  BLOCK_TYPE_VORBIS_COMMENT = 4;
  BLOCK_TYPE_CUESHEET = 5;
  BLOCK_TYPE_PICTURE = 6;
  VORBIS_PARAMETERS_ID = #1 + 'vorbis';
  VORBIS_TAG_ID = #3 + 'vorbis';

type

  TOggHeader = packed record
    Marker: array[1..4] of char;
    stream_structure_version:byte;
    header_type_flag:byte;
    absolute_granule_position:int64;
    stream_serial_number:cardinal;
    page_sequence_no:cardinal;
    page_checksum:cardinal;
    page_segments:byte;
  end;


  { Vorbis parameter header }
  VorbisHeader = packed record
    ID: array [1..7] of char;                          { Always #1 + "vorbis" }
    BitstreamVersion: array [1..4] of byte;        { Bitstream version number }
    ChannelMode: byte;                                   { Number of channels }
    SampleRate: integer;                                   { Sample rate (hz) }
    BitRateMaximal: integer;                           { Bit rate upper limit }
    BitRateNominal: integer;                               { Nominal bit rate }
    BitRateMinimal: integer;                           { Bit rate lower limit }
    BlockSize: byte;                   { Coded size for small and long blocks }
    StopFlag: byte;                                                { Always 1 }
  end;

  { Vorbis tag data }
  VorbisTag = packed record
    ID:     array [1..7] of char;                          { Always #3 + "vorbis" }
//    Fields: integer;                                   { Number of tag fields }
  end;



function TOGGReader.GetDuration: int64;
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

function TOGGReader.GetTags: TTags;
begin
  Result := fTags;
end;

function TOGGReader.LoadFromFile(FileName: Tfilename): boolean;
var
  fStream: TFileStream;
  Header: TOggHeader;
  Start: byte;
  BlockHeader: VorbisHeader;
  TagHeader: VorbisTag;
  BlockLength: integer;
  BlockType: integer;
  i:Integer;

begin
  Result := inherited LoadFromFile(FileName);
  fStream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
  try
    fStream.Read(Header, sizeof(Header));
    if String(Header.Marker) <> OGG_IDENTIFIER then
      exit;

    for i := 1 to Header.page_segments do
       fStream.ReadByte;

  fStream.Read(BlockHeader, SizeOf(BlockHeader));
  if BlockHeader.ID <> VORBIS_PARAMETERS_ID then
     exit;

  fStream.Read(Header, sizeof(Header));
  for i := 1 to Header.page_segments do
     fStream.ReadByte;

  fStream.Read(TagHeader, SizeOf(TagHeader));
  if TagHeader.ID <> VORBIS_TAG_ID then
     exit;

  ftags := TVorbisTags.Create;
  ftags.ReadFromStream(fStream);

  finally
    fstream.Free;
  end;

end;

end.

