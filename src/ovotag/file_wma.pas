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
unit file_wma;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AudioTag, basetag, tag_wma, LCLProc;

const
  WMAFileMask: string = '*.wma;';

type
  TObjectHeader = packed record
    ObjectID: TGuid;
    ObjectSize: QWord;
  end;

  TWMAHeader = packed record
    ObjectHeader: TObjectHeader;
    ObjectCount: DWord;
    Reserved: word;
  end;

  TWMAFileProperty = packed record
    FileHeader: TObjectHeader;
    CreationDate: Qword;
    DataPacketsCount: QWORD;
    PlayDuration: QWORD;
    SendDuration: QWORD;
    Preroll: QWORD;
    Flags: DWORD;
    MinDataPacketSize: DWORD;
    MaxDataPacketSize: DWORD;
    MaxBitrate: DWORD;
  end;

  TWMAAudioProperty = packed record
    CodecID: word;
    Channels: word;
    Samples: DWORD;
    BytePerSecond: DWORD;
    BlockAlignment: word;
    BitsPerSamples: word;
    CodecDataSize: word;
  end;

  TWMAStreamProperty = packed record
    //  StreamHeader : TObjectHeader;
    StreamType: TGuid;
    ErrorCorrectionType: TGuid;
    TimeOffset: QWORD;
    DataLength: DWORD;
    ErrorCorrectionLength: DWORD;
    Flags: word;
    Reserved: DWORD;
    AudioProperty: TWMAAudioProperty;
  end;

  TWMAContentDescription = packed record
    //   StreamHeader : TObjectHeader;
    TitleLength: word;
    AuthorLength: word;
    CopyrightLength: word;
    DescriptionLength: word;
    RatingLength: word;
  end;
  { TWMAReader }

  TWMAReader = class(TTagReader)
  private
    fTags: TWmaTags;
    fDuration: int64;
    fBitRate: integer;
//    fBPM: integer;
    fChannels: integer;
    fSampling: integer;
    fChannelMode: string;
    fObjectCount: integer;
    fHaveContentDescription: boolean;
    fHaveExtendedDescription: boolean;
    procedure ImportContentDescription(AStream: TStream;
      ContentDescription: TWMAContentDescription);
  protected
    function GetTags: TTags; override;
    function GetDuration: int64; override;
    function DumpInfo: TMediaProperty; override;
  public
    function isUpdateable: boolean; override;
    function LoadFromFile(AFileName: Tfilename): boolean; override;
    function SaveToFile(AFileName: Tfilename): boolean; override;
  end;

implementation
uses CommonFunctions;
const

  WMA_HEADER_ID: TGUID = '{75B22630-668E-11CF-A6D9-00AA0062CE6C}';
  WMA_CONTENT_DESCRIPTION: TGUID = '{75B22633-668E-11CF-A6D9-00AA0062CE6C}';
  WMA_EXTENDED_CONTENT_DESCRIPTION: TGUID = '{D2D0A440-E307-11D2-97F0-00A0C95EA850}';
  WMA_FILE_PROPERTIES: TGUID = '{8CABDCA1-A947-11CF-8EE4-00C00C205365}';
  WMA_STREAM_PROPERTIES: TGUID = '{B7DC0791-A9B7-11CF-8EE6-00C00C205365}';

{ TWMAReader }

function TWMAReader.GetTags: TTags;
begin
  Result := fTags;
end;

function TWMAReader.GetDuration: int64;
begin
  Result := fDuration;
end;

function TWMAReader.DumpInfo: TMediaProperty;
begin
  Result := inherited DumpInfo;
  Result.BitRate:= fBitRate;
  Result.ChannelMode:= fChannelMode;
end;

function TWMAReader.isUpdateable: boolean;
begin
  Result := True;
end;

procedure TWMAReader.ImportContentDescription(AStream: TStream;
  ContentDescription: TWMAContentDescription);
var
  tmpValue: array of char;
  Frame: TWMAFrame;
begin

  if ContentDescription.TitleLength > 0 then
  begin
    SetLength(tmpValue, ContentDescription.TitleLength);
    AStream.Read(tmpValue[0], ContentDescription.TitleLength);
    if fTags.FramesByID['Title'] = nil then
    begin
      Frame := TWMAFrame.Create('Title');
      Frame.DataType := 0;
      Frame.AsString := trim(WideString(tmpValue));
      fTags.Add(Frame);
    end;
  end;

  if ContentDescription.AuthorLength > 0 then
  begin
    SetLength(tmpValue, ContentDescription.AuthorLength);
    AStream.Read(tmpValue[0], ContentDescription.AuthorLength);
    if fTags.FramesByID['Author'] = nil then
    begin
      Frame := TWMAFrame.Create('Author');
      Frame.DataType := 0;
      Frame.AsString := trim(WideString(tmpValue));
      fTags.Add(Frame);
    end;
  end;

  if ContentDescription.CopyrightLength > 0 then
  begin
    SetLength(tmpValue, ContentDescription.CopyrightLength);
    AStream.Read(tmpValue[0], ContentDescription.CopyrightLength);
  end;
  if ContentDescription.DescriptionLength > 0 then
  begin
    SetLength(tmpValue, ContentDescription.DescriptionLength);
    AStream.Read(tmpValue[0], ContentDescription.DescriptionLength);
  end;
  if ContentDescription.RatingLength > 0 then
  begin
    SetLength(tmpValue, ContentDescription.RatingLength);
    AStream.Read(tmpValue[0], ContentDescription.RatingLength);
  end;
 SetLength(tmpValue, 0);
end;

function TWMAReader.LoadFromFile(AFileName: Tfilename): boolean;
var
  fStream: TFileStream;
  Header: TWMaHeader;
  SubObjHeader: TObjectHeader;
  FileProperty: TWMAFileProperty;
  StreamProperty: TWMAStreamProperty;
  ContentDescription: TWMAContentDescription;
//  AudioProperty :TWMAAudioProperty;
//  CurrPos: int64;
  i: integer;
begin
  Result := inherited LoadFromFile(AFileName);

  try
    fStream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);

    fStream.ReadBuffer(Header, SizeOf(Header));
    fTags := TWMATags.Create;

    if not IsEqualGUID(Header.ObjectHeader.ObjectID, WMA_HEADER_ID) then
      exit;
    fObjectCount := Header.ObjectCount;
    fHaveContentDescription := False;
    fHaveExtendedDescription := False;
    for i := 0 to Header.ObjectCount -1 do
    begin
      fStream.Read(SubObjHeader, SizeOf(SubObjHeader));
      if IsEqualGUID(SubObjHeader.ObjectID, WMA_CONTENT_DESCRIPTION) then
      begin
        fStream.Read(ContentDescription, SizeOf(TWMAContentDescription));
        ImportContentDescription(fStream, ContentDescription);
        fHaveContentDescription := True;
      end
      else
      if IsEqualGUID(SubObjHeader.ObjectID, WMA_FILE_PROPERTIES) then
      begin
        fStream.Read(FileProperty, SizeOf(TWMAFileProperty));
        fDuration := (FileProperty.PlayDuration div 10000) - FileProperty.Preroll;
      end
      else
      if IsEqualGUID(SubObjHeader.ObjectID, WMA_STREAM_PROPERTIES) then
      begin
        fStream.Read(StreamProperty, SizeOf(TWMAStreamProperty));
        fChannels := StreamProperty.AudioProperty.Channels;
        fChannelMode:= DecodeChannelNumber(fChannels);
        fSampling:= StreamProperty.AudioProperty.Samples;
        fBitRate:= (StreamProperty.AudioProperty.BytePerSecond * 8) div 1000;
        fStream.Seek(StreamProperty.AudioProperty.CodecDataSize +
          StreamProperty.ErrorCorrectionLength, soFromcurrent);

        { TODO : Fix types };
      end
      else
      if IsEqualGUID(SubObjHeader.ObjectID, WMA_EXTENDED_CONTENT_DESCRIPTION) then
      begin
        fHaveExtendedDescription := True;
        fTags.ReadFromStream(fStream);
      end
      else
      begin
        fStream.Seek(SubObjHeader.ObjectSize - SizeOf(SubObjHeader), soFromCurrent);
      end;
    end;

  finally
    fStream.Free;
  end;
  Result := (Tags <> nil) and (tags.Count > 0);

end;

function TWMAReader.SaveToFile(AFileName: Tfilename): boolean;
var
  SourceStream: TFileStream;
  DestStream: TFileStream;
  MemoryStream : TmemoryStream;

  Header: TWMaHeader;
  SubObjHeader: TObjectHeader;
//  FileProperty: TWMAFileProperty;
//  StreamProperty: TWMAStreamProperty;
  ContentDescription: TWMAContentDescription;
  NewSize: word;
  fOrigCount : word;
  i: integer;
begin
  Result := inherited SaveToFile(AFileName);

  try
    DebugLn(FileName,'-->',AFileName);
    SourceStream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
    DestStream := TFileStream.Create(AFileName, fmCreate or fmOpenReadWrite or fmShareDenyNone);

    SourceStream.ReadBuffer(Header, SizeOf(Header));
    fOrigCount := Header.ObjectCount;
    if fHaveContentDescription then
       dec(Header.ObjectCount);

    if Not fHaveExtendedDescription then
       inc(Header.ObjectCount);

    NewSize:= Header.ObjectHeader.ObjectSize;
    DestStream.WriteBuffer(Header, SizeOf(Header));

    for i := 0 to fOrigCount -1 do
    begin
      SourceStream.Read(SubObjHeader, SizeOf(SubObjHeader));
      if IsEqualGUID(SubObjHeader.ObjectID, WMA_CONTENT_DESCRIPTION) then
      begin
       SourceStream.Read(ContentDescription, SizeOf(TWMAContentDescription));
       SourceStream.Seek(ContentDescription.AuthorLength +
                         ContentDescription.CopyrightLength +
                         ContentDescription.DescriptionLength +
                         ContentDescription.RatingLength +
                         ContentDescription.TitleLength, soFromCurrent);

       NewSize := NewSize - SubObjHeader.ObjectSize;
      end
      else
      if IsEqualGUID(SubObjHeader.ObjectID, WMA_EXTENDED_CONTENT_DESCRIPTION) then
      begin
        MemoryStream := TMemoryStream.Create;
        SourceStream.Seek(SubObjHeader.ObjectSize - SizeOf(SubObjHeader), soFromCurrent);
        fTags.WriteToStream(MemoryStream);
        MemoryStream.Position:= 0;
        NewSize :=  NewSize - SubObjHeader.ObjectSize + MemoryStream.Size + SizeOf(SubObjHeader);
        SubObjHeader.ObjectSize := MemoryStream.Size + SizeOf(SubObjHeader);
        DestStream.Write(SubObjHeader, SizeOf(SubObjHeader));
        DestStream.CopyFrom(MemoryStream, MemoryStream.Size);
        MemoryStream.free;
      end
      else
      begin
        DestStream.Write(SubObjHeader, SizeOf(SubObjHeader));
        DestStream.CopyFrom(SourceStream, SubObjHeader.ObjectSize - SizeOf(SubObjHeader));
      end;
    end;

    if Not fHaveExtendedDescription then
       NewSize :=  NewSize + fTags.WriteToStream(DestStream) + SizeOf(SubObjHeader);

    DestStream.CopyFrom(SourceStream, SourceStream.Size - SourceStream.Position);
    DestStream.Seek(0, soFromBeginning);
    Header.ObjectHeader.ObjectSize:= NewSize;
    DestStream.WriteBuffer(Header, SizeOf(Header));


  finally
    SourceStream.free;
    DestStream.free;
  end;
end;

initialization
  RegisterTagReader(WMAFileMask, TWMAReader);
end.
