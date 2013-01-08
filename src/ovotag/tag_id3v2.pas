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
unit tag_id3v2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basetag;

type

  TID3V1Record = record
    Header: array [1..3] of char;
    Title: array [1..30] of char;
    Artist: array [1..30] of char;
    Album: array [1..30] of char;
    Year: array [1..4] of char;
    Comment: array [1..28] of char;
    Stopper: char;
    Track: byte;
    Genre: byte;
  end;

  TID3Tags = class;

  { TID3Frame }

  TID3Frame = class(TFrameElement)
  private
    fSize: DWord;
    fFlags: DWord;
    Data: array of Ansichar;
    function IsValid: boolean;
  protected
    function GetSize: DWord; override;
  public
    destructor Destroy; override;
    function GetAsString: string; override;
    procedure SetAsString(AValue: string); override;
    function ReadFromStream(AStream: TStream): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;


  { TID3Tags }

  TID3Tags = class(TTags)
  private
    fSize: integer;
    procedure DecodeFrameToImage(Frame: TID3Frame; Image: TImageElement);
    function GetBestMatch(Index1, Index2: integer; NewFrame: boolean): string;
    function ImportFromID3V1(AStream: TStream): boolean;
  public
    Version: word;
    FromV1: boolean;
  public
    property Size: integer read fSize;
    function GetCommonTags: TCommonTags; override;
    procedure SetCommonTags(CommonTags: TCommonTags); override;
    function ReadFromStream(AStream: TStream): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;


implementation

uses CommonFunctions, ID3v1Genres, lazutf8;

type

  { TID3Frame }
  TID3Header = packed record
    Marker: array[0..2] of ansichar;
    Version: word;
    Flags: byte;
    size: dword;
  end;

  TID3FrameHeader = record
    ID: array [0..3] of char;
    Size: DWord;
    Flags: word;
  end;

  TID3FrameHeaderOld = record
    ID: array [0..2] of char;
    Size: array [0..2] of byte;
  end;

const
  ID3_HEADER_MARKER = 'ID3';
  ID3_FOOTER_MARKER = '3DI';

  TAG_VERSION_2_2 = 2;
  TAG_VERSION_2_3 = 3;
  TAG_VERSION_2_4 = 4;
  ID3V2_FRAME_COUNT = 18;

  { Names of supported tag frames (ID3v2.3.x & ID3v2.4.x) }
  ID3V2_KNOWNFRAME: array [1..ID3V2_FRAME_COUNT, boolean] of string = (
    ('TIT2', 'TT2'),
    ('TPE1', 'TP1'),
    ('TALB', 'TAL'),
    ('TRCK', 'TRK'),
    ('TYER', 'TYE'),
    ('TCON', 'TCO'),
    ('COMM', 'COM'),
    ('TCOM', 'TCM'),
    ('TENC', 'TEN'),
    ('TCOP', 'TCR'),
    ('TLAN', 'TLA'),
    ('WXXX', 'WXX'),
    ('TDRC', 'TOR'),
    ('TOPE', 'TOA'),
    ('TIT1', 'TT1'),
    ('TOAL', 'TOT'),
    ('TSIZ', 'TSI'),
    ('TPE2', 'TP2')
    );

{ TID3Tags }

function TID3Tags.GetBestMatch(Index1, Index2: integer; NewFrame: boolean): string;
begin
  Result := GetFrameValue(ID3V2_KNOWNFRAME[Index1, NewFrame]);
  if Result = '' then
    Result := GetFrameValue(ID3V2_KNOWNFRAME[Index2, NewFrame]);

end;

function TID3Tags.ImportFromID3V1(AStream: TStream): boolean;
var
  V1Rec: TID3V1Record;
  Frame: TID3Frame;
begin
  fSize := 0;
  Result := False;
  AStream.Seek(AStream.Size - SizeOf(V1Rec), soFromBeginning);
  AStream.Read(V1Rec, SizeOf(V1Rec));
  if V1Rec.Header <> 'TAG' then
    exit;

  version := TAG_VERSION_2_3;
  if trim(V1Rec.Artist) <> '' then
  begin
    Frame := TID3Frame.Create('TPE1');
    Frame.Tagger := Self;
    frame.fFlags := 0;
    Frame.AsString := trim(V1Rec.Artist);
    Add(Frame);
  end;

  if trim(V1Rec.Album) <> '' then
  begin
    Frame := TID3Frame.Create('TALB');
    Frame.Tagger := Self;
    frame.fFlags := 0;
    Frame.AsString := trim(V1Rec.Album);
    Add(Frame);
  end;

  if trim(V1Rec.Title) <> '' then
  begin
    Frame := TID3Frame.Create('TIT2');
    Frame.Tagger := Self;
    frame.fFlags := 0;
    Frame.AsString := trim(V1Rec.Title);
    Add(Frame);
  end;

  if trim(V1Rec.Year) <> '' then
  begin
    Frame := TID3Frame.Create('TYER');
    Frame.Tagger := Self;
    frame.fFlags := 0;
    Frame.AsString := trim(V1Rec.Year);
    Add(Frame);
  end;

  if V1Rec.Genre <= ID3_MaxGenreExtended then
  begin
    Frame := TID3Frame.Create('TCON');
    Frame.Tagger := Self;
    frame.fFlags := 0;
    Frame.AsString := v1Genres[V1Rec.Genre];
    Add(Frame);
  end;

  if V1Rec.Stopper = #00 then
  begin
    if trim(V1Rec.Comment) <> '' then
    begin
      Frame := TID3Frame.Create('COMM');
      Frame.Tagger := Self;
      frame.fFlags := 0;
      Frame.AsString := trim(V1Rec.Comment);
      Add(Frame);
    end;

    Frame := TID3Frame.Create('TRCK');
    Frame.Tagger := Self;
    frame.fFlags := 0;
    Frame.AsString := IntToStr(v1rec.track);
    Add(Frame);

  end
  else
  begin
    if trim(V1Rec.Comment + V1Rec.stopper + char(V1Rec.track)) <> '' then
    begin
      Frame := TID3Frame.Create('COMM');
      Frame.Tagger := Self;
      frame.fFlags := 0;
      Frame.AsString := trim(V1Rec.Comment + V1Rec.stopper + char(V1Rec.track));
      Add(Frame);
    end;
  end;
  Result := True;

end;

function TID3Tags.GetCommonTags: TCommonTags;
var
  UseOldTag: boolean;
begin
  Result := inherited GetCommonTags;
  UseOldTag := (Version <= TAG_VERSION_2_2) and not FromV1;

  Result.Artist := GetBestMatch(2, 14, UseOldTag);
  Result.Title := GetBestMatch(1, 15, UseOldTag);
  Result.Album := GetBestMatch(3, 16, UseOldTag);
  Result.Year := GetBestMatch(5, 13, UseOldTag);
  Result.AlbumArtist := GetContent(GetFrameValue(ID3V2_KNOWNFRAME[18, UseOldTag]),
    Result.Artist);
  Result.Track := ExtractTrack(GetFrameValue(ID3V2_KNOWNFRAME[4, UseOldTag]));
  Result.TrackString := GetFrameValue(ID3V2_KNOWNFRAME[4, UseOldTag]);
  Result.Comment := GetFrameValue(ID3V2_KNOWNFRAME[7, UseOldTag]);
  Result.Genre := ExtractGenre(GetFrameValue(ID3V2_KNOWNFRAME[6, UseOldTag]));
  Result.HasImage := ImageCount > 0;

end;

procedure TID3Tags.SetCommonTags(CommonTags: TCommonTags);
var
  UseOldTag: boolean;
begin
  inherited SetCommonTags(CommonTags);
  if Version = 0  then
     Version := TAG_VERSION_2_3;

  UseOldTag := (Version <= TAG_VERSION_2_2) and not FromV1;
  SetFrameValue(ID3V2_KNOWNFRAME[2, UseOldTag], CommonTags.Artist, TID3Frame);
  SetFrameValue(ID3V2_KNOWNFRAME[1, UseOldTag], CommonTags.Title, TID3Frame);
  SetFrameValue(ID3V2_KNOWNFRAME[3, UseOldTag], CommonTags.Album, TID3Frame);
  SetFrameValue(ID3V2_KNOWNFRAME[5, UseOldTag], CommonTags.Year, TID3Frame);
  SetFrameValue(ID3V2_KNOWNFRAME[18, UseOldTag], CommonTags.AlbumArtist, TID3Frame);
  SetFrameValue(ID3V2_KNOWNFRAME[4, UseOldTag], CommonTags.TrackString, TID3Frame);
  SetFrameValue(ID3V2_KNOWNFRAME[7, UseOldTag], CommonTags.Comment, TID3Frame);
  SetFrameValue(ID3V2_KNOWNFRAME[6, UseOldTag], CommonTags.Genre, TID3Frame);

end;

procedure TID3Tags.DecodeFrameToImage(Frame: TID3Frame; Image: TImageElement);
var
  wData: PChar;
  wDatasize: Dword;
begin
  image.FrameRef := Frame;
  wData := PChar(Frame.Data);
  wDatasize := Frame.fSize;
  Inc(wData);
  Dec(wDatasize);
  image.MIMEType := pAnsiChar(wData);
  Inc(wData, Length(image.MIMEType) + 1);
  Dec(wDataSize, Length(image.MIMEType) + 1);

  if Version > TAG_VERSION_2_2 then
  begin
    Image.PictureType := pByte(wData)^;
    Inc(wData);
    Dec(wDatasize);
  end;

  image.Description := pAnsiChar(wData);
  Inc(wData, Length(image.Description) + 1);
  Dec(wDataSize, Length(image.Description) + 1);

  Image.Image.WriteBuffer(wData[0], wDatasize);
  image.Image.Position := 0;
end;

function TID3Tags.WriteToStream(AStream: TStream): Dword;
var
  header: TID3Header;
  tmpSize: integer;
  i: integer;
  pos: integer;
  HeadSize : integer;
begin
  pos := AStream.Position;
  header.Marker := ID3_HEADER_MARKER;
  Header.Version := Version;
  if Version >= TAG_VERSION_2_3 then
     HeadSize:= 10
  else
    HeadSize:= 6;
  header.Flags := 0;
  tmpSize := 0;

  for i := 0 to Count - 1 do
  begin
    tmpSize := tmpSize + Frames[i].Size + HeadSize;
  end;

  header.size := SyncSafe_Encode(TmpSize);
  Astream.Write(header, SizeOf(header));

  for i := 0 to Count - 1 do
  begin
    Frames[i].WriteToStream(AStream);
  end;
  Result := tmpSize + 10;
end;

function TID3Tags.ReadFromStream(AStream: TStream): boolean;
var
  header: TID3Header;
  Transferred: DWord;
  Frame: TID3Frame;
  Image: TImageElement;
  Stop: boolean;
begin
  Result := False;
  FromV1 := False;
  ;
  AStream.Read(header, SizeOf(header));
  if header.Marker <> ID3_HEADER_MARKER then
  begin
    FromV1 := ImportFromID3V1(AStream);
    Result := FromV1;
    exit;
  end;

  Version := header.Version;
  fSize := SyncSafe_Decode(header.size);
  Stop := False;
  if (Version in [TAG_VERSION_2_2..TAG_VERSION_2_4]) and (fSize > 0) then
    while (AStream.Position < (fSize + SizeOf(header))) and not stop do
    begin
      Frame := TID3Frame.Create;
      Frame.Tagger := self;
      if Frame.ReadFromStream(AStream) then
      begin
        Add(Frame);
        if (Frame.ID = 'APIC') or (Frame.ID = 'PIC') then
        begin
          image := TImageElement.Create;
          DecodeFrameToImage(Frame, Image);
          AddImage(Image);
        end;
      end
      else
      begin
        FreeAndNil(Frame);
        Stop := True;
      end;
    end;
  Result := Count > 0;

end;

{ TID3Frame }

function TID3Frame.GetAsString: string;
begin
  case
     ID[1] of
     'T', 'W' : Result := ExtractString(pByte(Data), size);
     'C' : Result := ExtractText(string(Data), true);
  else
    Result := '?';
  end;
end;


procedure TID3Frame.SetAsString(AValue: string);
begin
  if copy(ID, 1, 3) = 'COM' then
      Avalue := Space(3) + AValue;

  Avalue:= UTF8toSys(AValue);
  fSize := Length(AValue);
  if fSize = 0 then
  begin
    SetLength(Data, 0);
    exit;
  end;
  inc(fSize,2);

  SetLength(Data, fSize);
  Data[0] := #00;
  StrPCopy(@(Data[1]), AValue);
  Data[fSize-1] := #00;

end;

function TID3Frame.IsValid: boolean;
var
  C: char;
begin
  Result := False;
  if length(Id) < 3 then
    exit;   // Corruption protection

  for C in ID do
    if not (C in ['A'..'Z', '0'..'9']) then
      Exit;   // Corruption protection

  Result := True;
end;

function TID3Frame.GetSize: DWord;
begin
  Result := fSize;
end;

destructor TID3Frame.Destroy;
begin
  self.Tagger := nil;
  inherited Destroy;
end;


function TID3Frame.WriteToStream(AStream: TStream): DWord;
var
  Header: TID3FrameHeader;
  HeaderOld: TID3FrameHeaderOld;
  tmpStr: PChar;
  tmpL: DWord;
  DataSize : DWord;
  headsize : integer;

begin
  if fSize = 0 then
     exit;

  if TID3Tags(Tagger).Version >= TAG_VERSION_2_3 then
  begin
    Header.ID := ID;
    if TID3Tags(Tagger).Version >= TAG_VERSION_2_4 then
      header.Size := SyncSafe_Encode(fSize)
    else
      Header.Size := NtoBE(fSize);
    Header.Flags := fFlags;
    headsize:= 10;
    AStream.Write(Header, headsize);
  end
  else
  begin
    HeaderOld.ID := ID;
    DataSize := fSize;
    HeaderOld.Size[0] := DataSize and $ff0000 shr 16;
    HeaderOld.Size[1] := DataSize and $00ff00 shr 8;
    HeaderOld.Size[2] := DataSize and $0000ff;
    headsize:= 6;
    AStream.Write(HeaderOld, headsize);
  end;
  AStream.Write(Data[0], fSize);
  Result := fSize + headsize;
end;

function TID3Frame.ReadFromStream(AStream: TStream): boolean;
var
  Header: TID3FrameHeader;
  HeaderOld: TID3FrameHeaderOld;
  DataSize: Dword;
begin
  Result := False;
  if TID3Tags(Tagger).Version < TAG_VERSION_2_3 then
  begin
    AStream.Read(HeaderOld, SizeOf(HeaderOld));
    id := string(HeaderOld.ID);
    fFlags := 0;
    if not IsValid then
      exit; // Corruption protection
    DataSize := HeaderOld.Size[0] shl 16 + HeaderOld.Size[1] shl 8 + HeaderOld.Size[2];
  end
  else
  begin
    AStream.Read(Header, 10);
    id := string(Header.ID);
    if not IsValid then
      exit; // Corruption protection
    fFlags := Header.Flags;
    if TID3Tags(Tagger).Version >= TAG_VERSION_2_4 then
      DataSize := SyncSafe_Decode(Header.Size)
    else
      DataSize := {Swap32}BEToN(Header.Size);
  end;

  if DataSize > TID3Tags(Tagger).size then
    exit; // Corruption protection

  SetLength(Data, DataSize + 1);
  FillByte(Data[0], DataSize + 1, 0);
  AStream.Read(Data[0], DataSize);
  fSize := DataSize;
  Result := True;
end;
end.
