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
unit file_Wave;

interface

uses
  Classes, SysUtils, AudioTag, baseTag, tag_Dummy;

const
  WaveFileMask: string = '*.wav;';

type
  WAVHeader = record
    { RIFF file header }
    RIFFHeader: array [1..4] of char;   { Must be "RIFF" }
    FileSize: integer;                { Must be "RealFileSize - 8" }
    WAVEHeader: array [1..4] of char;   { Must be "WAVE" }
  end;

  ChunkHeader = record  { Format information }
    ChunkName: array [1..4] of char;
    ChunkSize: cardinal;               { Format size }
  end;

  FmtHeader = record
    FormatID: word;                   { Format type code }
    ChannelNumber: word;                   { Number of channels }
    SampleRate: integer;                { Sample rate (hz) }
    BytesPerSecond: integer;               { Bytes/second }
    BlockAlign: word;                   { Block alignment }
    BitsPerSample: word;                   { Bits/sample }
  end;

  { TWaveReader }

  TWaveReader = class(TTagReader)
  private
    fTags: TDummyTags;
    fMediaProperty: TMediaProperty;
    fFileSize: int64;
    fmt: FmtHeader;
    DataPos: int64;
    SampleNumber: cardinal;
  protected
    function GetDuration: int64; override;
    function GetTags: TTags; override;
    function DumpInfo: TMediaProperty; override;
  public
    function LoadFromFile(AFileName: Tfilename): boolean; override;
    function SaveToFile(AFileName: Tfilename): boolean; override;
  end;

implementation

uses
  CommonFunctions;

{ TWaveReader }

function TWaveReader.GetDuration: int64;
begin
  Result := 0;
  if (SampleNumber = 0) and (fmt.BytesPerSecond > 0) then
    Result := trunc((ffilesize - DataPos) / fmt.BytesPerSecond) * 1000;

  if (SampleNumber > 0) and (fmt.SampleRate > 0) then
    Result := trunc(SampleNumber / (fmt.SampleRate * fmt.ChannelNumber * (fmt.BitsPerSample / 8))) * 1000;

end;

function TWaveReader.GetTags: TTags;
begin
  Result := fTags;
end;

function TWaveReader.DumpInfo: TMediaProperty;
begin
  Result := fMediaProperty;
end;

function HeaderIsValid(const WAVData: WAVHeader): boolean;
begin
  Result := True;
  { Header validation }
  if WAVData.RIFFHeader <> 'RIFF' then
    Result := False;
  if WAVData.WAVEHeader <> 'WAVE' then
    Result := False;
end;


function TWaveReader.LoadFromFile(AFileName: Tfilename): boolean;
var
  fStream: TFileStream;
  WaveH: WAVHeader;
  Found: boolean;
  Chunk: ChunkHeader;

  function FindChunk(aChunk: string): boolean;
  begin
    Result := False;
    repeat
      // read next chunk
      fStream.Read(Chunk.ChunkName, SizeOf(Chunk.ChunkName));

      if Chunk.ChunkName <> aChunk then
        begin
          fStream.Read(Chunk.ChunkSize, SizeOf(Chunk.ChunkSize));
          fStream.Seek(Chunk.ChunkSize, soCurrent);
        end
      else
        Result := True;

    until (Result) or (fStream.Position >= fFileSize);
  end;

begin
  Result := inherited LoadFromFile(AFileName);
  fTags := TDummyTags.Create;
  try
    fStream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
    fFileSize := fStream.Size;
    fStream.Read(WaveH, SizeOf(WaveH));

    if HeaderIsValid(WaveH) then
    begin

      if FindChunk('fmt ') then
        begin
          fStream.Read(Chunk.ChunkSize, SizeOf(Chunk.ChunkSize));
          fStream.Read(fmt, SizeOf(fmt));
        end
      else
        Initialize(fmt);

      if FindChunk('data') then
        begin
          fStream.Read(Chunk.ChunkSize, SizeOf(Chunk.ChunkSize));
          DataPos := fStream.Position;
          SampleNumber := Chunk.ChunkSize;
        end;
      fMediaProperty.ChannelMode := DecodeChannelNumber(fmt.ChannelNumber);
      fMediaProperty.BitRate := fmt.BytesPerSecond * 8 div 1000;
      fMediaProperty.Sampling := fmt.SampleRate;
    end;


  finally
    fStream.Free;
  end;

end;

function TWaveReader.SaveToFile(AFileName: Tfilename): boolean;
begin
  Result := False;
end;

initialization
  RegisterTagReader(WaveFileMask, TWaveReader);
end.

