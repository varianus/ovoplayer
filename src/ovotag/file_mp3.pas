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
unit file_mp3;

{$mode objfpc}

interface

uses
  Classes, SysUtils, audiotag, basetag, tag_id3v2;

const
  Mp3FileMask: string    = '*.mpa;*.mp1;*.mp2;*.mp3;';

type

  { TMP3Reader }
  VBRData = record
    Found: boolean;                                { True if VBR header found }
    ID: array [1..4] of char;                   { Header ID: "Xing" or "VBRI" }
    Frames: integer;                                 { Total number of frames }
    Bytes: integer;                                   { Total number of bytes }
    Scale: byte;                                         { VBR scale (1..100) }
    VendorID: string;                                { Vendor ID (if present) }
  end;

  { MPEG frame header data}
  FrameData = record
    Found: boolean;                                     { True if frame found }
    Position: integer;                           { Frame position in the file }
    Size: word;                                          { Frame size (bytes) }
    Xing: boolean;                                     { True if Xing encoder }
    Data: array [1..4] of byte;                 { The whole frame header data }
    VersionID: byte;                                        { MPEG version ID }
    LayerID: byte;                                            { MPEG layer ID }
    ProtectionBit: boolean;                        { True if protected by CRC }
    BitRateID: word;                                            { Bit rate ID }
    SampleRateID: word;                                      { Sample rate ID }
    PaddingBit: boolean;                               { True if frame padded }
    PrivateBit: boolean;                                  { Extra information }
    ModeID: byte;                                           { Channel mode ID }
    ModeExtensionID: byte;             { Mode extension ID (for Joint Stereo) }
    CopyrightBit: boolean;                        { True if audio copyrighted }
    OriginalBit: boolean;                            { True if original media }
    EmphasisID: byte;                                           { Emphasis ID }
  end;

  TMP3Reader = class(TTagReader)
  private
    fTags: TID3Tags;
    FVBR: VBRData;
    FFrame: FrameData;
    FMPEGStart: int64;
    FMPEGEnd: int64;
    fMediaProperty: TMediaProperty;
  protected
    function GetTags: TTags; override;
    function GetDuration: int64; override;
    Function DumpInfo: TMediaProperty; override;
  public
    function LoadFromFile(FileName: Tfilename): boolean; override;
  end;

implementation

{ TMP3Reader }

const
  { Table for bit rates }
  MPEG_BIT_RATE: array [0..3, 0..3, 0..15] of word =
    (
    { For MPEG 2.5 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0)),
    { Reserved }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
    { For MPEG 2 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0)),
    { For MPEG 1 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384, 0),
    (0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 0))
    );

  { Sample rate codes }
  MPEG_SAMPLE_RATE_LEVEL_3 = 0;                                     { Level 3 }
  MPEG_SAMPLE_RATE_LEVEL_2 = 1;                                     { Level 2 }
  MPEG_SAMPLE_RATE_LEVEL_1 = 2;                                     { Level 1 }
  MPEG_SAMPLE_RATE_UNKNOWN = 3;                               { Unknown value }

  { Table for sample rates }
  MPEG_SAMPLE_RATE: array [0..3, 0..3] of word =
    (
    (11025, 12000, 8000, 0),                                   { For MPEG 2.5 }
    (0, 0, 0, 0),                                                  { Reserved }
    (22050, 24000, 16000, 0),                                    { For MPEG 2 }
    (44100, 48000, 32000, 0)                                     { For MPEG 1 }
    );

  { Channel mode codes }
  MPEG_CM_STEREO = 0;                                                { Stereo }
  MPEG_CM_JOINT_STEREO = 1;                                    { Joint Stereo }
  MPEG_CM_DUAL_CHANNEL = 2;                                    { Dual Channel }
  MPEG_CM_MONO = 3;                                                    { Mono }
  MPEG_CM_UNKNOWN = 4;
  { Channel mode names }
  MPEG_CM_MODE: array [0..4] of string =
      ('Stereo', 'Joint Stereo', 'Dual Channel', 'Mono', 'Unknown');


  { MPEG version codes }
  MPEG_VERSION_2_5 = 0;                                            { MPEG 2.5 }
  MPEG_VERSION_UNKNOWN = 1;                                 { Unknown version }
  MPEG_VERSION_2 = 2;                                                { MPEG 2 }
  MPEG_VERSION_1 = 3;
  MAX_MPEG_FRAME_LENGTH = 1729;                      { Max. MPEG frame length }
  MIN_MPEG_BIT_RATE = 8;                                { Min. bit rate value }
  { MPEG layer codes }
  MPEG_LAYER_UNKNOWN = 0;                                     { Unknown layer }
  MPEG_LAYER_III = 1;                                             { Layer III }
  MPEG_LAYER_II = 2;                                               { Layer II }
  MPEG_LAYER_I = 3;

  { VBR header ID for Xing/FhG }
  VBR_ID_XING = 'Xing';                                         { Xing VBR ID }
  VBR_ID_FHG = 'VBRI';                                           { FhG VBR ID }


{ --------------------------------------------------------------------------- }
function IsFrameHeader(const HeaderData: array of byte): boolean;
begin
  { Check for valid frame header }
  if ((HeaderData[0] and $FF) <> $FF) or ((HeaderData[1] and $E0) <> $E0) or
    (((HeaderData[1] shr 3) and 3) = 1) or (((HeaderData[1] shr 1) and 3) = 0) or
    ((HeaderData[2] and $F0) = $F0) or ((HeaderData[2] and $F0) = 0) or
    (((HeaderData[2] shr 2) and 3) = 3) or ((HeaderData[3] and 3) = 2) then
    Result := False
  else
    Result := True;
end;

{ --------------------------------------------------------------------------- }

procedure DecodeHeader(const HeaderData: array of byte; var Frame: FrameData);
begin
  { Decode frame header data }
  Move(HeaderData, Frame.Data, SizeOf(Frame.Data));
  Frame.VersionID := (HeaderData[1] shr 3) and 3;
  Frame.LayerID := (HeaderData[1] shr 1) and 3;
  Frame.ProtectionBit := (HeaderData[1] and 1) <> 1;
  Frame.BitRateID := HeaderData[2] shr 4;
  Frame.SampleRateID := (HeaderData[2] shr 2) and 3;
  Frame.PaddingBit := ((HeaderData[2] shr 1) and 1) = 1;
  Frame.PrivateBit := (HeaderData[2] and 1) = 1;
  Frame.ModeID := (HeaderData[3] shr 6) and 3;
  Frame.ModeExtensionID := (HeaderData[3] shr 4) and 3;
  Frame.CopyrightBit := ((HeaderData[3] shr 3) and 1) = 1;
  Frame.OriginalBit := ((HeaderData[3] shr 2) and 1) = 1;
  Frame.EmphasisID := HeaderData[3] and 3;
end;

{ --------------------------------------------------------------------------- }

function ValidFrameAt(const Index: word; Data: array of byte): boolean;
var
  HeaderData: array [1..4] of byte;
begin
  { Check for frame at given position }
  HeaderData[1] := Data[Index];
  HeaderData[2] := Data[Index + 1];
  HeaderData[3] := Data[Index + 2];
  HeaderData[4] := Data[Index + 3];
  if IsFrameHeader(HeaderData) then
    Result := True
  else
    Result := False;
end;

{ --------------------------------------------------------------------------- }

function GetCoefficient(const Frame: FrameData): byte;
begin
  { Get frame size coefficient }
  if Frame.VersionID = MPEG_VERSION_1 then
    if Frame.LayerID = MPEG_LAYER_I then
      Result := 48
    else
      Result := 144
  else
  if Frame.LayerID = MPEG_LAYER_I then
    Result := 24
  else if Frame.LayerID = MPEG_LAYER_II then
    Result := 144
  else
    Result := 72;
end;

function GetBitRate(const Frame: FrameData): Dword;
begin
  { Get bit rate }
  Result := MPEG_BIT_RATE[Frame.VersionID, Frame.LayerID, Frame.BitRateID];
end;


function GetSampleRate(const Frame: FrameData): Dword;
begin
  { Get sample rate }
  Result := MPEG_SAMPLE_RATE[Frame.VersionID, Frame.SampleRateID];
end;

function GetPadding(const Frame: FrameData): byte;
begin
  { Get frame padding }
  if Frame.PaddingBit then
    if Frame.LayerID = MPEG_LAYER_I then
      Result := 4
    else
      Result := 1
  else
    Result := 0;
end;

function GetFrameLength(const Frame: FrameData): Dword;
var
  Coefficient, BitRate, SampleRate, Padding: word;
begin
  { Calculate MPEG frame length }
  Coefficient := GetCoefficient(Frame);
  BitRate := GetBitRate(Frame);
  SampleRate := GetSampleRate(Frame);
  Padding := GetPadding(Frame);
  Result := Trunc(Coefficient * BitRate * 1000 / SampleRate) + Padding;
end;

{ --------------------------------------------------------------------------- }

function IsXing(const Index: word; Data: array of byte): boolean;
begin
  { Get true if Xing encoder }
  Result :=
    (Data[Index] = 0) and (Data[Index + 1] = 0) and (Data[Index + 2] = 0) and
    (Data[Index + 3] = 0) and (Data[Index + 4] = 0) and (Data[Index + 5] = 0);
end;

{ --------------------------------------------------------------------------- }

function GetXingInfo(const Index: word; Data: array of byte): VBRData;
begin
  { Extract Xing VBR info at given position }
  FillChar(Result, SizeOf(Result), 0);
  Result.Found := True;
  Result.Frames :=
    Data[Index + 8] * $1000000 + Data[Index + 9] * $10000 +
    Data[Index + 10] * $100 + Data[Index + 11];
  Result.Bytes :=
    Data[Index + 12] * $1000000 + Data[Index + 13] * $10000 +
    Data[Index + 14] * $100 + Data[Index + 15];
  Result.Scale := Data[Index + 119];
  { Vendor ID can be not present }
  Result.VendorID :=
    Chr(Data[Index + 120]) + Chr(Data[Index + 121]) + Chr(Data[Index + 122]) +
    Chr(Data[Index + 123]) + Chr(Data[Index + 124]) + Chr(Data[Index + 125]) +
    Chr(Data[Index + 126]) + Chr(Data[Index + 127]);
end;

{ --------------------------------------------------------------------------- }

function GetFhGInfo(const Index: word; Data: array of byte): VBRData;
begin
  { Extract FhG VBR info at given position }
  FillChar(Result, SizeOf(Result), 0);
  Result.Found := True;
  Result.Scale := Data[Index + 9];
  Result.Bytes :=
    Data[Index + 10] * $1000000 + Data[Index + 11] * $10000 +
    Data[Index + 12] * $100 + Data[Index + 13];
  Result.Frames :=
    Data[Index + 14] * $1000000 + Data[Index + 15] * $10000 +
    Data[Index + 16] * $100 + Data[Index + 17];
end;

{ --------------------------------------------------------------------------- }

function FindVBR(const Index: word; Data: array of byte): VBRData;
begin
  { Check for VBR header at given position }
  FillChar(Result, SizeOf(Result), 0);
  if Chr(Data[Index]) + Chr(Data[Index + 1]) + Chr(Data[Index + 2]) +
  Chr(Data[Index + 3]) = VBR_ID_XING then
    Result := GetXingInfo(Index, Data);
  if Chr(Data[Index]) + Chr(Data[Index + 1]) + Chr(Data[Index + 2]) +
  Chr(Data[Index + 3]) = VBR_ID_FHG then
    Result := GetFhGInfo(Index, Data);
end;

{ --------------------------------------------------------------------------- }

function GetVBRDeviation(const Frame: FrameData): byte;
begin
  { Calculate VBR deviation }
  if Frame.VersionID = MPEG_VERSION_1 then
    if Frame.ModeID <> MPEG_CM_MONO then
      Result := 36
    else
      Result := 21
  else
  if Frame.ModeID <> MPEG_CM_MONO then
    Result := 21
  else
    Result := 13;
end;

{ --------------------------------------------------------------------------- }

function FindFrame(const Data: array of byte; var VBR: VBRData): FrameData;
var
  HeaderData: array [1..4] of byte;
  Iterator, VBRIdx: integer;
begin
  { Search for valid frame }
  FillChar(Result, SizeOf(Result), 0);
  Move(Data, HeaderData, SizeOf(HeaderData));
  for Iterator := 0 to SizeOf(Data) - MAX_MPEG_FRAME_LENGTH do
  begin
    { Decode data if frame header found }
    if IsFrameHeader(HeaderData) then
    begin
      DecodeHeader(HeaderData, Result);
      { Check for next frame and try to find VBR header }
      VBRIdx := Iterator + GetFrameLength(Result);
      if (VBRIdx < SizeOf(Data)) and ValidFrameAt(VBRIdx, Data) then
      begin
        Result.Found := True;
        Result.Position := Iterator;
        Result.Size := GetFrameLength(Result);
        Result.Xing := IsXing(Iterator + SizeOf(HeaderData), Data);
        VBR := FindVBR(Iterator + GetVBRDeviation(Result), Data);
        break;
      end;
    end;
    { Prepare next data block }
    HeaderData[1] := HeaderData[2];
    HeaderData[2] := HeaderData[3];
    HeaderData[3] := HeaderData[4];
    HeaderData[4] := Data[Iterator + SizeOf(HeaderData)];
  end;
end;


function TMP3Reader.GetTags: TTags;
begin
  Result := fTags;
end;

function TMP3Reader.GetDuration: int64;
var
  MPEGSize: int64;
begin
  { Calculate song duration }
  if FFrame.Found then
  begin
    if (FVBR.Found) and (FVBR.Frames > 0) then
      Result := trunc(FVBR.Frames * GetCoefficient(FFrame) *  8 / GetSampleRate(FFrame) * 1000)
    else
    begin
      MPEGSize := FMPEGEnd - FMPEGStart;
      Result := trunc((MPEGSize - FFrame.Position) /
        GetBitRate(FFrame) * 8);
    end;
  end
  else
    Result := 0;

end;

function TMP3Reader.DumpInfo: TMediaProperty;
begin
  Result := fMediaProperty;
end;

function TMP3Reader.LoadFromFile(FileName: Tfilename): boolean;
const
  SizeOfData = MAX_MPEG_FRAME_LENGTH * 2;
var
  fStream: TFileStream;
  Data: array [1..SizeOfData] of byte;
  Transferred: DWord;

begin
  Result := inherited LoadFromFile(FileName);

  try
    fStream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
    ftags := TID3Tags.Create;
    fTags.ReadFromStream(fStream);

    Transferred := fStream.Read(Data[1], SizeOfData);
    FFrame := FindFrame(Data, FVBR);
    if (not FFrame.Found) and (Transferred = SizeOfData) then
      repeat
        Transferred := fStream.Read(Data[1], SizeOfData);
        FFrame := FindFrame(Data, FVBR);
      until (FFrame.Found) or (Transferred < SizeOfData);
    if FFrame.Found then
    begin
      FFrame.Position := fStream.Position + FFrame.Position;
      FMPEGStart := FFrame.Position;
      FMPEGEnd := fStream.Size;
      fMediaProperty.BitRate:= getbitrate(fframe);
      fMediaProperty.Sampling:= GetSampleRate(fframe);
      fMediaProperty.ChannelMode:=MPEG_CM_MODE[FFrame.ModeID];
    end;
  finally
    fStream.Free;
  end;
end;

initialization
  RegisterTagReader(Mp3FileMask, TMP3Reader);
end.

