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
  WaveFileMask: string    = '*.wav;';

type
  WAVHeader = record
    { RIFF file header }
    RIFFHeader:    array [1..4] of char;   { Must be "RIFF" }
    FileSize:      integer;                { Must be "RealFileSize - 8" }
    WAVEHeader:    array [1..4] of char;   { Must be "WAVE" }
    { Format information }
    FormatHeader:  array [1..4] of char;   { Must be "fmt " }
    FormatSize:    cardinal;               { Format size }
    FormatID:      word;                   { Format type code }
    ChannelNumber: word;                   { Number of channels }
    SampleRate:    integer;                { Sample rate (hz) }
    BytesPerSecond: integer;               { Bytes/second }
    BlockAlign:    word;                   { Block alignment }
    BitsPerSample: word;                   { Bits/sample }
    DataHeader:    array [1..4] of char;   { Can be "data" }
    SampleNumber:  cardinal;               { Number of samples (optional) }
  end;

  { TWaveReader }

  TWaveReader = class(TTagReader)
  private
    fTags: TDummyTags;
    fMediaProperty: TMediaProperty;
    header: WAVHeader;
    fFileSize: int64;
  protected
    function GetDuration: int64; override;
    function GetTags: TTags; override;
    Function DumpInfo: TMediaProperty; override;
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
  result := 0;
  if (header.SampleNumber = 0) and (header.BytesPerSecond > 0) then
    Result := trunc((FFileSize - SizeOf(header)) / header.BytesPerSecond) * 1000;

  if (header.SampleNumber > 0) and (header.SampleRate > 0) then
    Result := trunc(header.SampleNumber / header.SampleRate) *1000;

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
  if WAVData.FormatHeader <> 'fmt ' then
    Result := False;
  if WAVData.ChannelNumber = 0 then
    Result := False;
end;


function TWaveReader.LoadFromFile(AFileName: Tfilename): boolean;
var
  fStream: TFileStream;
begin
  Result := inherited LoadFromFile(AFileName);
  fTags := TDummyTags.Create;
  try
    fStream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
    fFileSize:= fStream.Size;
    fStream.Read(Header, SizeOf(Header));
    if fFileSize > (Header.FormatSize + 24) then
      begin
        fStream.Seek(Header.FormatSize + 24, soFromBeginning);
        Header.SampleNumber := fStream.ReadDWord;
      end;

    if HeaderIsValid(Header) then
      begin
        fMediaProperty.ChannelMode:=DecodeChannelNumber(header.ChannelNumber);
        fMediaProperty.BitRate:= Header.BytesPerSecond * 8 div 1000;
        fMediaProperty.Sampling:= Header.SampleRate;
      end;


  finally
    fStream.Free;
  end;



end;

function TWaveReader.SaveToFile(AFileName: Tfilename): boolean;
begin
  Result:=False;
end;

initialization
  RegisterTagReader(WaveFileMask, TWaveReader);
end.

