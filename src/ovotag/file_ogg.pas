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
unit file_ogg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AudioTag, baseTag, tag_Vorbis;

const
  OGGFileMask: string    = '*.ogg;';

type

  { TOGGReader }

  TOGGReader = class(TTagReader)
  private
    fTags: TVorbisTags;
    FSampleRate: integer;
    FSamples: int64;
  protected
    function GetDuration: int64; override;
    function GetTags: TTags; override;
  public
    function LoadFromFile(AFileName: Tfilename): boolean; override;
    function SaveToFile(AFileName: Tfilename): boolean; override;
  end;

implementation

{ TOGGReader }
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

const
  { CRC table for checksum calculating }
   CRC_TABLE: array [0..$FF] of cardinal = (
     $00000000, $04C11DB7, $09823B6E, $0D4326D9, $130476DC, $17C56B6B,
     $1A864DB2, $1E475005, $2608EDB8, $22C9F00F, $2F8AD6D6, $2B4BCB61,
     $350C9B64, $31CD86D3, $3C8EA00A, $384FBDBD, $4C11DB70, $48D0C6C7,
     $4593E01E, $4152FDA9, $5F15ADAC, $5BD4B01B, $569796C2, $52568B75,
     $6A1936C8, $6ED82B7F, $639B0DA6, $675A1011, $791D4014, $7DDC5DA3,
     $709F7B7A, $745E66CD, $9823B6E0, $9CE2AB57, $91A18D8E, $95609039,
     $8B27C03C, $8FE6DD8B, $82A5FB52, $8664E6E5, $BE2B5B58, $BAEA46EF,
     $B7A96036, $B3687D81, $AD2F2D84, $A9EE3033, $A4AD16EA, $A06C0B5D,
     $D4326D90, $D0F37027, $DDB056FE, $D9714B49, $C7361B4C, $C3F706FB,
     $CEB42022, $CA753D95, $F23A8028, $F6FB9D9F, $FBB8BB46, $FF79A6F1,
     $E13EF6F4, $E5FFEB43, $E8BCCD9A, $EC7DD02D, $34867077, $30476DC0,
     $3D044B19, $39C556AE, $278206AB, $23431B1C, $2E003DC5, $2AC12072,
     $128E9DCF, $164F8078, $1B0CA6A1, $1FCDBB16, $018AEB13, $054BF6A4,
     $0808D07D, $0CC9CDCA, $7897AB07, $7C56B6B0, $71159069, $75D48DDE,
     $6B93DDDB, $6F52C06C, $6211E6B5, $66D0FB02, $5E9F46BF, $5A5E5B08,
     $571D7DD1, $53DC6066, $4D9B3063, $495A2DD4, $44190B0D, $40D816BA,
     $ACA5C697, $A864DB20, $A527FDF9, $A1E6E04E, $BFA1B04B, $BB60ADFC,
     $B6238B25, $B2E29692, $8AAD2B2F, $8E6C3698, $832F1041, $87EE0DF6,
     $99A95DF3, $9D684044, $902B669D, $94EA7B2A, $E0B41DE7, $E4750050,
     $E9362689, $EDF73B3E, $F3B06B3B, $F771768C, $FA325055, $FEF34DE2,
     $C6BCF05F, $C27DEDE8, $CF3ECB31, $CBFFD686, $D5B88683, $D1799B34,
     $DC3ABDED, $D8FBA05A, $690CE0EE, $6DCDFD59, $608EDB80, $644FC637,
     $7A089632, $7EC98B85, $738AAD5C, $774BB0EB, $4F040D56, $4BC510E1,
     $46863638, $42472B8F, $5C007B8A, $58C1663D, $558240E4, $51435D53,
     $251D3B9E, $21DC2629, $2C9F00F0, $285E1D47, $36194D42, $32D850F5,
     $3F9B762C, $3B5A6B9B, $0315D626, $07D4CB91, $0A97ED48, $0E56F0FF,
     $1011A0FA, $14D0BD4D, $19939B94, $1D528623, $F12F560E, $F5EE4BB9,
     $F8AD6D60, $FC6C70D7, $E22B20D2, $E6EA3D65, $EBA91BBC, $EF68060B,
     $D727BBB6, $D3E6A601, $DEA580D8, $DA649D6F, $C423CD6A, $C0E2D0DD,
     $CDA1F604, $C960EBB3, $BD3E8D7E, $B9FF90C9, $B4BCB610, $B07DABA7,
     $AE3AFBA2, $AAFBE615, $A7B8C0CC, $A379DD7B, $9B3660C6, $9FF77D71,
     $92B45BA8, $9675461F, $8832161A, $8CF30BAD, $81B02D74, $857130C3,
     $5D8A9099, $594B8D2E, $5408ABF7, $50C9B640, $4E8EE645, $4A4FFBF2,
     $470CDD2B, $43CDC09C, $7B827D21, $7F436096, $7200464F, $76C15BF8,
     $68860BFD, $6C47164A, $61043093, $65C52D24, $119B4BE9, $155A565E,
     $18197087, $1CD86D30, $029F3D35, $065E2082, $0B1D065B, $0FDC1BEC,
     $3793A651, $3352BBE6, $3E119D3F, $3AD08088, $2497D08D, $2056CD3A,
     $2D15EBE3, $29D4F654, $C5A92679, $C1683BCE, $CC2B1D17, $C8EA00A0,
     $D6AD50A5, $D26C4D12, $DF2F6BCB, $DBEE767C, $E3A1CBC1, $E760D676,
     $EA23F0AF, $EEE2ED18, $F0A5BD1D, $F464A0AA, $F9278673, $FDE69BC4,
     $89B8FD09, $8D79E0BE, $803AC667, $84FBDBD0, $9ABC8BD5, $9E7D9662,
     $933EB0BB, $97FFAD0C, $AFB010B1, $AB710D06, $A6322BDF, $A2F33668,
     $BCB4666D, $B8757BDA, $B5365D03, $B1F740B4);

procedure CalculateCRC(var CRC: cardinal; const Data; Size: cardinal);
var
  Buffer: ^byte;
  Index:  cardinal;
begin
  { Calculate CRC through data }
  Buffer := Addr(Data);
  for Index := 1 to Size do
    begin
    CRC := (CRC shl 8) xor CRC_TABLE[((CRC shr 24) and $FF) xor Buffer^];
    Inc(Buffer);
    end;
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

function TOGGReader.SaveToFile(AFileName: Tfilename): boolean;
var
  SourceStream: TFileStream;
  DestStream: TFileStream;
  MemoryStream : TmemoryStream;
  Header: TOggHeader;
  Start: byte;
  FrameCount, dw: DWORD;
  BlockHeader: VorbisHeader;
  TagHeader: VorbisTag;
  i:Integer;
  crc, savepos, offs, OldSize : DWORD;
  LacingArray: array [1..$FF] of byte;
    procedure SetLacingValues(const NewTagSize: integer);
    var
      Index, Position, Value: integer;
      Buffer: array [1..$FF] of byte;
    begin
      { Set new lacing values for the second Ogg page }
      Position := 1;
      Value    := 0;
      for Index := header.page_segments downto 1 do
        begin
        if LacingArray[Index] < $FF then
          begin
          Position := Index;
          Value    := 0;
          end;
        Inc(Value, LacingArray[Index]);
        end;
      Value := Value + NewTagSize - OldSize;// - SizeOf(Header) - SizeOf(TagHeader) - header.page_segments);
      { Change lacing values at the beginning }
      for Index := 1 to Value div $FF do
        Buffer[Index] := $FF;
      Buffer[(Value div $FF) + 1] := Value mod $FF;
      if Position < header.page_segments then
        for Index := Position + 1 to header.page_segments do
          Buffer[Index - Position + (Value div $FF) + 1] :=
            LacingArray[Index];
      header.page_segments:= header.page_segments - Position + (Value div $FF) + 1;
      for Index := 1 to header.page_segments do
        LacingArray[Index] := Buffer[Index];
    end;

begin
  Result := inherited SaveToFile(AFileName);
  SourceStream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
  DestStream := TFileStream.Create(AFileName, fmCreate or fmOpenReadWrite or fmShareDenyNone);
  try
    SourceStream.Read(Header, sizeof(Header));
    DestStream.Write(Header, sizeof(Header));

    for i := 1 to Header.page_segments do
        begin
          Start := SourceStream.ReadByte;
          DestStream.WriteByte(Start);
        end;

    SourceStream.Read(BlockHeader, SizeOf(BlockHeader));
    DestStream.Write(BlockHeader, sizeof(BlockHeader));

    SourceStream.Read(Header, SizeOf(Header));
    savepos:=DestStream.Position;

    Header.page_checksum:=0;
    crc := 0;

    offs:=0;
    for i := 1 to Header.page_segments do
        begin
          Start := SourceStream.ReadByte;
          Offs:= offs + Start;
          LacingArray[i]:=Start;
        end;

    SourceStream.Read(TagHeader, SizeOf(TagHeader));
    oldSize:=0;

    dw := SourceStream.ReadDWord;
    OldSize:= OldSize+ Dw + SizeOf(DWord);

    SourceStream.Seek(dw, soFromCurrent);
    FrameCount := SourceStream.ReadDWord;
    OldSize:= OldSize+ SizeOf(DWord);

    for i := 0 to FrameCount - 1 do
    begin
      dw := SourceStream.ReadDWord;
      SourceStream.Seek(dw, soFromCurrent);
      OldSize:= OldSize+ Dw + SizeOf(DWord);
    end;

    MemoryStream := TMemoryStream.Create;

    fTags.WriteToStream(MemoryStream);

    MemoryStream.Position:=0;

    SetLacingValues(MemoryStream.Size);

    DestStream.Write(Header, sizeof(Header));
    offs:= 0;
    for i := 1 to Header.page_segments do
        begin
          Offs:= offs + LacingArray[i];
          DestStream.WriteByte(LacingArray[i]);
        end;
    DestStream.Write(TagHeader, sizeof(TagHeader));

    DestStream.CopyFrom(MemoryStream, MemoryStream.Size);
    DestStream.CopyFrom(SourceStream, SourceStream.Size - SourceStream.Position);

    DestStream.Seek(savepos, soFromBeginning);

    MemoryStream.Position:=0;
    MemoryStream.CopyFrom(DestStream, SizeOf(Header) + Header.page_segments+ offs);

    crc:=0;
    MemoryStream.Position:=0;
    CalculateCRC(crc, MemoryStream.Memory^, MemoryStream.Size);

    DestStream.Seek(savepos, soFromBeginning);
    Header.page_checksum:=crc;
    DestStream.Write(Header, sizeof(Header));

  finally
    MemoryStream.free;
    SourceStream.free;
    DestStream.free;
  end;

end;

function TOGGReader.LoadFromFile(AFileName: Tfilename): boolean;
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
  Result := inherited LoadFromFile(AFileName);
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

initialization
  RegisterTagReader(OGGFileMask, TOGGReader);

end.

