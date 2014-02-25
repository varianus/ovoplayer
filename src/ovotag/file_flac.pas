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
unit file_flac;

{$mode objfpc}{$H+}

interface

uses
  Classes,  lazutf8classes,SysUtils, AudioTag, baseTag, tag_Vorbis;

const
  FlacFileMask: string   = '*.flac;';

type

  { TFlacReader }

  TFlacReader = class(TTagReader)
  private
    fTags: TVorbisTags;
    FSampleRate: integer;
    FSamples: int64;
    FChannels:    byte;
    FBitsPerSample: byte;
    fMediaProperty: TMediaProperty;
  protected
    function GetDuration: int64; override;
    function GetTags: TTags; override;
    Function DumpInfo: TMediaProperty; override;
  public
    function LoadFromFile(AFileName: Tfilename): boolean; override;
    function SaveToFile(AFileName: Tfilename): boolean; override;
    function isUpdateable: boolean; override;

//
  end;

implementation
uses
  CommonFunctions;
{ TFlacReader }
const
  FLAC_IDENTIFIER = 'fLaC';

  //BLOCK_TYPE_STREAMINFO = 0;
  //BLOCK_TYPE_PADDING = 1;
  //BLOCK_TYPE_APPLICATION = 2;
  //BLOCK_TYPE_SEEKTABLE = 3;
  BLOCK_TYPE_VORBIS_COMMENT = 4;
  //BLOCK_TYPE_CUESHEET = 5;
  BLOCK_TYPE_PICTURE = 6;

type
  TMetaDataBlockHeader = array[1..4] of byte;

  TFlacHeader = packed record
    Marker: array[1..4] of char;
    MetaDataBlockHeader: TMetaDataBlockHeader;
    StreamInfo: array[1..18] of byte;
    MD5Sign: array[1..16] of byte;
  end;


function TFlacReader.GetDuration: int64;
begin
  if (FSampleRate > 0) and (FSamples > 0) then
  begin
    Result := trunc((FSamples / FSampleRate) * 1000);
  end
  else
  begin
    Result := 1;
  end;
end;

function TFlacReader.GetTags: TTags;
begin
  Result := fTags;
end;

function TFlacReader.DumpInfo: TMediaProperty;
begin
  Result:= fMediaProperty;
end;


function TFlacReader.LoadFromFile(AFileName: Tfilename): boolean;
var
  fStream: TFileStreamUTF8;
  Header: TFlacHeader;
  BlockHeader: TMetaDataBlockHeader;
  BlockLength: integer;
  BlockType: integer;

begin
  Result := inherited LoadFromFile(AFileName);
  try
    fStream := TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyNone);
    fStream.Read(Header, sizeof(Header));
    if Header.Marker <> FLAC_IDENTIFIER then
      exit;
    with Header do
    begin
      FChannels := (StreamInfo[13] shr 1 and $7 + 1);
      FSampleRate := (StreamInfo[11] shl 12 or StreamInfo[12] shl 4 or StreamInfo[13] shr 4);
      FBitsPerSample := (StreamInfo[13] and 1 shl 4 or StreamInfo[14] shr 4 + 1);
      FSamples := (StreamInfo[15] shl 24 or StreamInfo[16] shl 16 or StreamInfo[17] shl 8 or StreamInfo[18]);
    end;

    if (Header.MetaDataBlockHeader[1] and $80) <> 0 then
      exit;

    repeat
      fStream.Read(BlockHeader, SizeOf(BlockHeader));
      BlockType := (BlockHeader[1] and $7F);
      BlockLength := (BlockHeader[2] shl 16) or (BlockHeader[3] shl 8) or
        (BlockHeader[4]);
      case BlockType of
        BLOCK_TYPE_VORBIS_COMMENT:
        begin
          if not assigned(fTags) then
             ftags := TVorbisTags.Create;
          ftags.ReadFromStream(fStream);
        end;
        BLOCK_TYPE_PICTURE:
        begin
          if not assigned(fTags) then
             ftags := TVorbisTags.Create;
          ftags.ReadImageFromStream(fStream);
        end;

        else
          fStream.Seek(BlockLength, soFromCurrent);
      end;
    until (BlockHeader[1] and $80) <> 0;

  finally
    fMediaProperty.Bitrate := Round(((fStream.Size - fStream.Position) / 1000) * 8 / getDuration);
    fMediaProperty.Sampling:= FSampleRate;
    fMediaProperty.ChannelMode:= DecodeChannelNumber(FChannels);
    fstream.Free;
  end;

end;

function TFlacReader.SaveToFile(AFileName: Tfilename): boolean;
var
  SourceStream: TFileStreamUTF8;
  DestStream: TFileStreamUTF8;
  Header: TFlacHeader;
  BlockHeader: TMetaDataBlockHeader;
  BlockLength: integer;
  BlockType: integer;
  MemoryStream : TmemoryStream;
//  HaveTags :boolean;

begin
  inherited SaveToFile(AFilename);
  SourceStream := TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyNone);
  DestStream := TFileStreamUTF8.Create(AFileName, fmCreate or fmOpenReadWrite or fmShareDenyNone);
  Result :=false;
  SourceStream.Read(Header, sizeof(Header));
  if Header.Marker <> FLAC_IDENTIFIER then
    exit;

  if (Header.MetaDataBlockHeader[1] and $80) <> 0 then
    exit;
//  HaveTags:= false;
  DestStream.Write(Header, sizeof(Header));
  repeat
    SourceStream.Read(BlockHeader, SizeOf(BlockHeader));
    BlockType := (BlockHeader[1] and $7F);
    BlockLength := (BlockHeader[2] shl 16) or (BlockHeader[3] shl 8) or
      (BlockHeader[4]);
    case BlockType of
      BLOCK_TYPE_VORBIS_COMMENT:
      begin
//        HaveTags := true;
        SourceStream.Seek(BlockLength, soFromCurrent);
        MemoryStream := TMemoryStream.Create;
        fTags.WriteToStream(MemoryStream);
        MemoryStream.Position:= 0;
        BlockLength:=  MemoryStream.Size;
        BlockHeader[2] := (BlockLength and $ff0000) shr 16;
        BlockHeader[3] := (BlockLength and $00ff00) shr 8;
        BlockHeader[4] := (BlockLength and $0000ff);
        DestStream.Write(BlockHeader, SizeOf(BlockHeader));
        DestStream.CopyFrom(MemoryStream, BlockLength);
        MemoryStream.Free;
      end;

      else
        begin
          DestStream.Write(BlockHeader, SizeOf(BlockHeader));
          DestStream.CopyFrom(SourceStream, BlockLength);
        end;
    end;
  until (BlockHeader[1] and $80) <> 0;

  //if not HaveTags then
  //  begin
  //    BlockHeader[1] := BLOCK_TYPE_VORBIS_COMMENT;
  //    MemoryStream := TMemoryStream.Create;
  //    fTags.WriteToStream(MemoryStream);
  //    MemoryStream.Position:= 0;
  //    BlockLength:=  MemoryStream.Size;
  //    BlockHeader[2] := (BlockLength and $ff0000) shr 16;
  //    BlockHeader[3] := (BlockLength and $00ff00) shr 8;
  //    BlockHeader[4] := (BlockLength and $0000ff);
  //    DestStream.Write(BlockHeader, SizeOf(BlockHeader));
  //    DestStream.CopyFrom(MemoryStream, BlockLength);
  //    MemoryStream.Free;
  //  end;

  DestStream.CopyFrom(SourceStream, SourceStream.Size - SourceStream.Position);

  SourceStream.Free;
  DestStream.Free;
  Result :=True;
end;

function TFlacReader.isUpdateable: boolean;
begin
  Result := true;
end;

initialization
  RegisterTagReader(FlacFileMask, TFlacReader);

end.

