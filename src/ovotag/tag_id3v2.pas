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

interface

uses
  Classes, SysUtils, basetag;

type

  TID3V1Record = packed record
    Header: array [1..3] of AnsiChar;
    Title: array [1..30] of AnsiChar;
    Artist: array [1..30] of AnsiChar;
    Album: array [1..30] of AnsiChar;
    Year: array [1..4] of AnsiChar;
    Comment: array [1..28] of AnsiChar;
    Stopper: AnsiChar;
    Track: byte;
    Genre: byte;
  end;

  TID3Tags = class;

  { TID3Frame }

  TID3Frame = class(TFrameElement)
  private
    fSize: DWord;
    fFlags: DWord;
    Data: RawByteString;
    function IsValid: boolean;
  protected
    function GetSize: DWord; override;
  public
    destructor Destroy; override;
    function GetAsString: string; override;
    procedure SetAsString(const AValue: string); override;
    function ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;


  { TID3Tags }

  TID3Tags = class(TTags)
  private
    fTotalSize: DWORD;
    procedure DecodeFrameToImage(Frame: TID3Frame; Image: TImageElement);
    function GetBestMatch(Index1, Index2: integer; NewFrame: boolean): string;
    function ImportFromID3V1(AStream: TStream): boolean;
  public
    Version: word;
    FromV1: boolean;
  public
    property Size: DWORD read fTotalSize;
    function GetCommonTags: TCommonTags; override;
    procedure SetCommonTags(CommonTags: TCommonTags); override;
    function ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean; override;
    function WriteToStream(AStream: TStream): DWord; override;
  end;


implementation

uses CommonFunctions, ID3v1Genres, lazutf8, LazUTF16;

type

  { TID3Frame }
  TID3Header = packed record
    Marker: array[0..2] of ansichar;
    Version: word;
    Flags: byte;
    size: dword;
  end;

  TID3FrameHeader = packed record
    ID: array [0..3] of char;
    Size: DWord;
    Flags: word;
  end;

  TID3FrameHeaderOld = packed record
    ID: array [0..2] of ansichar;
    Size: array [0..2] of byte;
  end;

const
  ID3_HEADER_MARKER = 'ID3';
//  ID3_FOOTER_MARKER = '3DI';

  TAG_VERSION_2_2 = 2;
  TAG_VERSION_2_3 = 3;
  TAG_VERSION_2_4 = 4;
  ID3V2_FRAME_COUNT = 18;

  { Names of supported tag frames (ID3v2.3.x & ID3v2.4.x) }
  ID3V2_KNOWNFRAME: array [1..ID3V2_FRAME_COUNT, boolean] of ansistring = (
    ('TIT2', 'TT2'),    // 1
    ('TPE1', 'TP1'),
    ('TALB', 'TAL'),
    ('TRCK', 'TRK'),
    ('TYER', 'TYE'),    // 5
    ('TCON', 'TCO'),
    ('COMM', 'COM'),
    ('TCOM', 'TCM'),
    ('TENC', 'TEN'),
    ('TCOP', 'TCR'),    //10
    ('TLAN', 'TLA'),
    ('WXXX', 'WXX'),
    ('TDRC', 'TOR'),
    ('TOPE', 'TOA'),
    ('TIT1', 'TT1'),    //15
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
  fTotalSize := 0;
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
    Frame := TID3Frame.Create('TDRC');
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
  Result.Title  := GetBestMatch(1, 15, UseOldTag);
  Result.Album  := GetBestMatch(3, 16, UseOldTag);
  Result.Year   := GetBestMatch(13, 5, UseOldTag);
  Result.AlbumArtist := GetContent(GetFrameValue(ID3V2_KNOWNFRAME[18, UseOldTag]), Result.Artist);
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
  SetFrameValue(ID3V2_KNOWNFRAME[13, UseOldTag], CommonTags.Year, TID3Frame);
  SetFrameValue(ID3V2_KNOWNFRAME[18, UseOldTag], CommonTags.AlbumArtist, TID3Frame);
  SetFrameValue(ID3V2_KNOWNFRAME[4, UseOldTag], CommonTags.TrackString, TID3Frame);
  SetFrameValue(ID3V2_KNOWNFRAME[7, UseOldTag], CommonTags.Comment, TID3Frame);
  SetFrameValue(ID3V2_KNOWNFRAME[6, UseOldTag], CommonTags.Genre, TID3Frame);

end;

procedure TID3Tags.DecodeFrameToImage(Frame: TID3Frame; Image: TImageElement);
var
  wData: PChar;
  wDatasize: Dword;
  Encoding: byte;
begin
  image.FrameRef := Frame;
  wData := PChar((Frame.Data));
  wDatasize := Frame.fSize;
  Encoding := pByte(wData)^;
  Inc(wData);
  Dec(wDatasize);
  image.MIMEType := pAnsiChar(wData);
  Inc(wData, Length(image.MIMEType) + 1);
  Dec(wDataSize, Length(image.MIMEType) + 1);

  if Version > TAG_VERSION_2_2 then
  begin
    Image.PictureType := pByte(wData)^;
    Inc(wData, 1);
    Dec(wDatasize, 1);
  end;

  if (pWord(wData)^ = $fffe)  or (pWord(wData)^ = $fefF) then
  begin
    Inc(wData, 2);
    Dec(wDatasize, 2);
  end;

  if (encoding = 0) or (encoding = 3) then  //ISO-8859-1 or UTF-8
  begin
      image.Description := pAnsiChar(wData);
      Inc(wData, Length(image.Description) + 1 );
      Dec(wDataSize, Length(image.Description) + 1 );
    end
  else  // UTF-16
    begin
      image.Description := pWideChar(wData);
      Inc(wData, Length(image.Description) *2  + 2);
      Dec(wDataSize, Length(image.Description) *2  + 2);
    end;

  Image.Image.WriteBuffer(wData[0], wDatasize);
  image.Image.Position := 0;
end;

function TID3Tags.WriteToStream(AStream: TStream): Dword;
var
  header: TID3Header;
  tmpSize: DWORD;
  i: integer;
  HeadSize : integer;
//  CurrPos: int64;
begin
//  CurrPos := AStream.Position;
  header.Marker := ID3_HEADER_MARKER;
  Header.Version := Version;
  if Version >= TAG_VERSION_2_3 then
     HeadSize:= 10
  else
    HeadSize:= 6;
  header.Flags := 0;
  tmpSize := 0;

  // Remove empty frames
  for i := Count - 1 downto 0 do
  begin
    if Frames[i].Size = 0 then
      Remove(Frames[i]);
  end;

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

function TID3Tags.ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean;
var
  header: TID3Header;
//  Transferred: DWord;
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
  fTotalSize := SyncSafe_Decode(header.size);
  Stop := False;
  if (Version in [TAG_VERSION_2_2..TAG_VERSION_2_4]) and (fTotalSize > 0) then
    while (AStream.Position < (fTotalSize + SizeOf(header))) and not stop do
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
  if Result then
    fTotalSize := fTotalSize +SizeOf(header);

end;

{ TID3Frame }

function TID3Frame.GetAsString: string;
begin
  case
     ID[1] of
     'T', 'W' : Result := ExtractString(pbyte(@Data[1]), size);
     'C' : Result :=      ExtractString(pbyte(@Data[1]), size, True);
  else
    Result := '<unknown>';
  end;
end;


procedure TID3Frame.SetAsString(Const AValue: string);
var
  xValue: UTf8String;
  wValue: UnicodeString;
  NeedLanguage: Boolean;
  LanguageOffset:integer;
begin
  xValue := (AValue);

  NeedLanguage := copy(ID, 1, 3) = 'COM';
  if NeedLanguage then
     LanguageOffset:= 3
  else
     LanguageOffset:= 0;

  fSize := Length(xValue);
  if fSize = 0 then
  begin
    SetLength(Data, 0);
    exit;
  end;

  if TID3Tags(Tagger).Version >= TAG_VERSION_2_4 then
     begin
       inc(fSize,3 + LanguageOffset);
       SetLength(Data, fSize);
       Data[1] := #03;
       if NeedLanguage then
         begin
           Data[2]:=#32;
           Data[3]:=#32;
           Data[4]:=#32;
         end;
       StrPCopy(@(Data[2+LanguageOffset]), xValue);
       Data[fSize-1] := #00;
       Data[fSize] := #00;
     end
  else
     begin
       wvalue := UTF8ToUTF16(xValue);
       fSize:= Length(wValue) * sizeof( UnicodeChar ) + 5 + LanguageOffset;
       SetLength(Data, fSize);
       Data[1] := #01;
       if NeedLanguage then
         begin
           Data[2]:=#32;
           Data[3]:=#32;
           Data[4]:=#32;
         end;

       Data[2+LanguageOffset] := #$FF;
       Data[3+LanguageOffset] := #$FE;

      Move(pbyte(wValue)^, pbyte(@Data[4+LanguageOffset])^, Length(wValue) * SizeOf(UnicodeChar));
//       StrLCopy(@(Data[4+LanguageOffset]), pChar(wValue), Length(wValue) * SizeOf(WideChar));
       Data[fSize-1] := #00;
       Data[fSize] := #00;

     end;

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
  SetLength(Data, 0);
  inherited Destroy;
end;


function TID3Frame.WriteToStream(AStream: TStream): DWord;
var
  Header: TID3FrameHeader;
  HeaderOld: TID3FrameHeaderOld;
//  tmpStr: PChar;
//  tmpL: DWord;
  DataSize : DWord;
  headsize : DWord;

begin
  Result := 0;

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
  AStream.Write(Data[1], fSize);
  Result := fSize + headsize;
end;

function TID3Frame.ReadFromStream(AStream: TStream;ExtInfo:pointer=nil): boolean;
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
  FillByte(Data[1], DataSize + 1, 0);
  AStream.Read(Data[1], DataSize);
  fSize := DataSize;
  Result := True;
end;
end.

