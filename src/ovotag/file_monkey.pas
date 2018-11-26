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

interface

uses
  Classes, lazutf8classes, SysUtils, AudioTag, baseTag, tag_APE;

const
  MonkeyFileMask: string = '*.ape;';

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

type
  { Real structure of Monkey's Audio header }
  // common header for all versions
  APE_HEADER = packed record
    cID: array[0..3] of byte;        // should equal 'MAC '
    nVersion: word;                 // version number * 1000 (3.81 = 3810)
  end;
  // old header for <= 3.97
  APE_HEADER_OLD = packed record
    nCompressionLevel,               // the compression level
    nFormatFlags,                    // any format flags (for future use)
    nChannels: word;                 // the number of channels (1 or 2)
    nSampleRate,                     // the sample rate (typically 44100)
    nHeaderBytes,                    // the bytes after the MAC header that compose the WAV header
    nTerminatingBytes,               // the bytes after that raw data (for extended info)
    nTotalFrames,                    // the number of frames in the file
    nFinalFrameBlocks: longword;     // the number of samples in the final frame
    nInt: integer;
  end;
  // new header for >= 3.98
  APE_HEADER_NEW = packed record
    nCompressionLevel: word;       // the compression level (see defines I.E. COMPRESSION_LEVEL_FAST)
    nFormatFlags: word;      // any format flags (for future use) Note: NOT the same flags as the old header!
    nBlocksPerFrame: longword;    // the number of audio blocks in one frame
    nFinalFrameBlocks: longword;    // the number of audio blocks in the final frame
    nTotalFrames: longword;    // the total number of frames
    nBitsPerSample: word;      // the bits per sample (typically 16)
    nChannels: word;      // the number of channels (1 or 2)
    nSampleRate: longword;    // the sample rate (typically 44100)
  end;
  // data descriptor for >= 3.98
  APE_DESCRIPTOR = packed record
    padded: word;                   // padding/reserved (always empty)
    nDescriptorBytes,                // the number of descriptor bytes (allows later expansion of this header)
    nHeaderBytes,                  // the number of header APE_HEADER bytes
    nSeekTableBytes,                // the number of bytes of the seek table
    nHeaderDataBytes,                // the number of header data bytes (from original file)
    nAPEFrameDataBytes,             // the number of bytes of APE frame data
    nAPEFrameDataBytesHigh,           // the high order number of APE frame data bytes
    nTerminatingDataBytes: longword;  // the terminating data of the file (not including tag data)
    cFileMD5: array[0..15] of byte;  // the MD5 hash of the file (see notes for usage... it's a littly tricky)
  end;

Const   { Compression level codes }
   //MONKEY_COMPRESSION_FAST       = 1000;  { Fast (poor) }
   //MONKEY_COMPRESSION_NORMAL     = 2000;  { Normal (good) }
   //MONKEY_COMPRESSION_HIGH       = 3000;  { High (very good) }
   MONKEY_COMPRESSION_EXTRA_HIGH = 4000;  { Extra high (best) }
   //MONKEY_COMPRESSION_INSANE     = 5000;  { Insane }
   //MONKEY_COMPRESSION_BRAINDEAD = 6000; { BrainDead }

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
  Result := True;
end;

function TMonkeyReader.LoadFromFile(AFileName: Tfilename): boolean;
var
  fStream: TFileStreamUTF8;
  tempTags: TTags;
  HaveID3V2: boolean;
  BlocksPerFrame: Integer;

  APE: APE_HEADER;     // common header
  APE_OLD: APE_HEADER_OLD; // old header   <= 3.97
  APE_NEW: APE_HEADER_NEW; // new header   >= 3.98
  APE_DESC: APE_DESCRIPTOR; // extra header >= 3.98

begin
  Result := inherited LoadFromFile(AFileName);
  fStream := TFileStreamUTF8.Create(fileName, fmOpenRead or fmShareDenyNone);
  try
    tempTags := TID3Tags.Create;
    HaveID3V2 := tempTags.ReadFromStream(fStream);
    fStream.Position := 0;
    ftags := TAPETags.Create;
    Result := ftags.ReadFromStream(fStream);
    if (not Result) and HaveID3V2 then
    begin
      fTags.Free;
      fTags := tempTags;
    end
    else
        tempTags.free;
    if HaveID3V2 then
      fStream.Seek(TID3Tags(tempTags).Size, soFromBeginning)
    else
      fStream.Seek(0, soFromBeginning);

    FillByte(APE, sizeof(APE), 0);
    if (fStream.Read(APE, sizeof(APE)) = sizeof(APE)) and (StrLComp(@APE.cID[0], 'MAC ', 4) = 0) then
    begin
      if APE.nVersion >= 3980 then
      begin
        FillByte(APE_DESC, sizeof(APE_DESC), 0);
        if (fStream.Read(APE_DESC, sizeof(APE_DESC)) = sizeof(APE_DESC)) then
        begin
          // seek past description header
          if APE_DESC.nDescriptorBytes <> 52 then
            fStream.Seek(APE_DESC.nDescriptorBytes - 52, soFromCurrent);
          // load new ape_header
          if APE_DESC.nHeaderBytes > sizeof(APE_NEW) then
            APE_DESC.nHeaderBytes := sizeof(APE_NEW);
          fillchar(APE_NEW, sizeof(APE_NEW), 0);
          if (longword(fStream.Read(APE_NEW, APE_DESC.nHeaderBytes)) = APE_DESC.nHeaderBytes) then
          begin
            // based on MAC SDK 3.98a1 (APEinfo.h)
            FSampleRate := APE_NEW.nSampleRate;
            if APE_NEW.nTotalFrames > 0 then
            begin
              FSamples :=
                int64(APE_NEW.nBlocksPerFrame) * int64(APE_NEW.nTotalFrames - 1) +
                int64(APE_NEW.nFinalFrameBlocks);
            end;
          end;
        end;
      end
      else
      begin
        // Old Monkey <= 3.97
        fillchar(APE_OLD, sizeof(APE_OLD), 0);
        if (fStream.Read(APE_OLD, sizeof(APE_OLD)) = sizeof(APE_OLD)) then
        begin
          FSampleRate := APE_OLD.nSampleRate;
          // based on MAC_SDK_397 (APEinfo.cpp)
          if (APE.nVersion >= 3950) then
            BlocksPerFrame := 73728 * 4
          else if (APE.nVersion >= 3900) or ((APE.nVersion >= 3800) and
            (APE_OLD.nCompressionLevel = MONKEY_COMPRESSION_EXTRA_HIGH)) then
            BlocksPerFrame := 73728
          else
            BlocksPerFrame := 9216;
          // calculate total uncompressed samples
          if APE_OLD.nTotalFrames > 0 then
          begin
            FSamples :=
              int64(APE_OLD.nTotalFrames - 1) * int64(BlocksPerFrame) +
              int64(APE_OLD.nFinalFrameBlocks);
          end;
        end;
      end;

    end;

  finally
    fstream.Free;
  end;

end;

function TMonkeyReader.SaveToFile(AFileName: Tfilename): boolean;
var
  SourceStream: TFileStreamUTF8;
  DestStream: TFileStreamUTF8;
  v1rec: TID3V1Record;
  offset: cardinal;
  header: TAPEHeader;
begin
  Result := inherited SaveToFile(AFileName);

  SourceStream := TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyNone);
  DestStream := TFileStreamUTF8.Create(AFileName, fmCreate or fmOpenReadWrite or fmShareDenyNone);

  try
    SourceStream.Seek(SourceStream.Size - SizeOf(TID3V1Record), soFromBeginning);
    SourceStream.Read(V1Rec, SizeOf(TID3V1Record));

    if V1Rec.Header <> 'TAG' then
      Offset := 0
    else
      Offset := SizeOf(V1Rec);

    SourceStream.Seek(SourceStream.Size - SizeOf(TAPEHeader) - offset, soFromBeginning);
    SourceStream.Read(Header, sizeof(TAPEHeader));

    if string(Header.Marker) = APE_IDENTIFIER then
    begin
      Offset := offset + header.TagSize;
      SourceStream.Seek(SourceStream.Size - SizeOf(Header) - offset, soFromBeginning);
      SourceStream.Read(Header, sizeof(Header));
      if string(Header.Marker) = APE_IDENTIFIER then
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
