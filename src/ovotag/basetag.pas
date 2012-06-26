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
unit basetag;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type
  TMediaProperty = Record
    BitRate : Integer;
    BPM : Integer;
    Sampling : Integer;
    ChannelMode : string;
  end;

  TIDFields = (idAlbum, idAlbumArtist, idArtist, idComment, idGenre,
    idTitle, idTrack, idYear);
  TIDFieldsSet = set of TIDFields;

  { TCommonTags }
  TCommonTags = record
    ID: integer;
    FileName: string;
    Album: string;
    AlbumArtist: string;
    Artist: string;
    Comment: string;
    Duration: Int64;
    Genre: string;
    Title: string;
    Track: Dword;
    TrackString: string;
    Year: string;
    HasImage:boolean;
  end;

  ACommonTags = Array of TCommonTags;

  operator = (t1 : TCommonTags; t2 : TCommonTags) b : boolean;

type

  { TFrameElement }
  TFrameElementClass = class of TFrameElement;
  TTags = class;

  TFrameElement = class
  Private
    fId:String;
  Protected
    function GetSize: DWord; virtual;
    function GetID: string; virtual;
    procedure SetID(AValue: string);
    function GetAsString: string; virtual; abstract;
    procedure SetAsString(AValue: string); virtual; abstract;
  public
    Tagger : TTags;
    constructor Create; virtual; Overload;
    constructor Create(ID:String); virtual; Overload;
    destructor Destroy; override;
    function ReadFromStream(AStream:TStream):boolean; virtual; abstract;
    function WriteToStream(AStream:TStream):DWord; virtual; abstract;

  public
    Property Size: DWord read GetSize;
    property ID: string read GetID Write SetID;
    property AsString: string read GetAsString write SetAsString;

  end;

  { TImageElement }

  TImageElement = class (TFrameElement)
  private
    FFrameRef: TFrameElement;
    FImage: TStream;
    procedure SetFrameRef(AValue: TFrameElement);
    procedure SetImage(AValue: TStream);
  protected
    function GetAsString: string;  override;
    procedure SetAsString(AValue: string); override;
  Public
    Description: String;
    MIMEType: String;
    PictureType: Integer;

    constructor Create; override;
    destructor Destroy; override;
    function ReadFromStream(AStream:TStream):boolean; override;
    Property FrameRef: TFrameElement read FFrameRef write SetFrameRef;
    Property Image: TStream read FImage write SetImage;
  end;

  { TTags }

  TTags = class
  private
    FramesList: TFPObjectList;
    ImagesList: TFPObjectList;
    function GetCount: integer;
    function GetFrameByID(ID: string): TFrameElement;
    function GetFrameByIndex(Index: integer): TFrameElement;
    function GetImageByID(ID: string): TImageElement;
    function GetImageByIndex(Index: integer): TImageElement;
    function GetImageCount: integer;
  protected
    Function GetCommonTags: TCommonTags; virtual;
    Procedure SetCommonTags(CommonTags :TCommonTags); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ReadFromStream(AStream:TStream):boolean; virtual; abstract;
    function WriteToStream(AStream:TStream):DWord; virtual; abstract;
    Procedure Add(Frame: TFrameElement);
    Procedure Remove(Frame: TFrameElement);
    Procedure AddImage(Image: TImageElement);
    Procedure Clear;
    function GetFrameValue(ID: String): String;
    procedure SetFrameValue(const ID:String;const Value:String; FrameClass: TFrameElementClass);
  public
    property Count: integer read GetCount;
    property ImageCount: integer read GetImageCount;
    property Frames[Index: integer]: TFrameElement read GetFrameByIndex;
    property FramesByID[ID: string]: TFrameElement read GetFrameByID;  default;
    property Images[Index: integer]: TImageElement read GetImageByIndex;
    property ImagesByID[ID: string]: TImageElement read GetImageByID;

  end;

  { TTagReader }
  TTagReaderClass = class of TTagReader;

  TTagReader = class
  private
      FFileName: String;
  Protected
    function GetTags: TTags; virtual; abstract;
    function GetDuration: Int64; virtual;
    Function DumpInfo: TMediaProperty; virtual;
  public
    Constructor Create(FileName: TfileName); overload;
    Constructor Create; overload;
    destructor Destroy; override;
    Function GetCommonTags: TCommonTags;
    Procedure SetCommonTags(CommonTags: TCommonTags);
    function LoadFromFile(AFileName: Tfilename): boolean; virtual;
    function SaveToFile(AFileName: Tfilename): boolean; virtual;
    function UpdateFile: boolean; virtual;
  public
    Property FileName : string Read FFileName;
    property Tags: TTags read GetTags;
    Property MediaProperty: TMediaProperty read DumpInfo;
  end;

Procedure ClearTags(var Tags:TcommonTags); inline;
function GetTagByID(Tags:TcommonTags; Field: TIDFields):string; inline;
Procedure SetTagByID(var Tags:TcommonTags; Field: TIDFields; Value :string); inline;

implementation
uses FileUtil;

operator = (t1 : TCommonTags; t2 : TCommonTags) b : boolean;
begin
 result := (t1.ID          = t2.ID ) and
           (t1.FileName    = t2.FileName) and
           (t1.TrackString = t2.TrackString) and
           (t1.Track       = t2.Track) and
           (t1.Title       = t2.Title) and
           (t1.Album       = t2.Album) and
           (t1.AlbumArtist = t2.AlbumArtist) and
           (t1.Artist      = t2.Artist) and
           (t1.Genre       = t2.Genre) and
           (t1.Year        = t2.Year) and
           (t1.Duration    = t2.Duration);
end;

procedure ClearTags(var Tags:TcommonTags);
begin
  Tags.ID           := 0;
  Tags.FileName     := '';
  Tags.TrackString  := '';
  Tags.Track        := 0;
  Tags.Title        := '';
  Tags.Album        := '';
  Tags.AlbumArtist  := '';
  Tags.Artist       := '';
  Tags.Genre        := '';
  Tags.Year         := '';
  Tags.Duration     := 0;

end;

function GetTagByID(Tags:TcommonTags; Field: TIDFields):string;
begin
  case Field of
    idAlbum       : Result := Tags.Album;
    idAlbumArtist : Result := Tags.AlbumArtist;
    idArtist      : Result := Tags.Artist;
    idComment     : Result := Tags.Comment;
    idGenre       : Result := Tags.Genre;
    idTitle       : Result := Tags.Title;
    idTrack       : Result := Tags.TrackString;
    idYear        : Result := Tags.Year;
  end;
end;

Procedure SetTagByID(var Tags:TcommonTags; Field: TIDFields; Value :string);
begin
    case Field of
    idAlbum       : Tags.Album := Value;
    idAlbumArtist : Tags.AlbumArtist := Value;
    idArtist      : Tags.Artist := Value;
    idComment     : Tags.Comment := Value;
    idGenre       : Tags.Genre := Value;
    idTitle       : Tags.Title := Value;
    idTrack       : Tags.TrackString := Value;
    idYear        : Tags.Year := Value;
  end;

end;

{ TImageElement }

procedure TImageElement.SetFrameRef(AValue: TFrameElement);
begin
  if FFrameRef=AValue then Exit;
  FFrameRef:=AValue;
end;

procedure TImageElement.SetImage(AValue: TStream);
begin
  if FImage=AValue then Exit;
  FImage:=AValue;
end;

function TImageElement.GetAsString: string;
begin
  Result:= '';
end;

procedure TImageElement.SetAsString(AValue: string);
begin
end;

function TImageElement.ReadFromStream(AStream: TStream): boolean;
begin
  Result:=False;
end;

constructor TImageElement.Create;
begin
  inherited Create;
  FImage := TMemoryStream.Create;
end;

destructor TImageElement.Destroy;
begin
  FreeAndNil(fImage);
  inherited Destroy;
end;

{ TTagReader }

function TTagReader.GetDuration: Int64;
begin
  Result:=0;
end;

constructor TTagReader.Create(FileName: TfileName);
begin
  Create;
  LoadFromFile(FileName);
end;

constructor TTagReader.Create;
begin
//
end;

destructor TTagReader.Destroy;
begin
  if Tags <> nil then
     tags.free;

  inherited Destroy;
end;

function TTagReader.DumpInfo: TMediaProperty;
begin
  with Result do
   begin
     BitRate :=0;
     BPM := 0;
     Sampling := 0;
     ChannelMode := '';
   end;

end;

function TTagReader.GetCommonTags: TCommonTags;
begin
  Result := Tags.GetCommonTags;
  Result.FileName := FFileName;
  Result.Duration:= GetDuration;
  if Result.Title = '' then
     Result.Title := ChangeFileExt(ExtractFileName(FFileName) ,'');
end;

procedure TTagReader.SetCommonTags(CommonTags: TCommonTags);
begin
  Tags.SetCommonTags(CommonTags);
end;

function TTagReader.LoadFromFile(AFileName: Tfilename): boolean;
begin
  FFileName:=AFileName;
  Result:=false;
end;

function TTagReader.SaveToFile(AFileName: Tfilename): boolean;
begin
  Result := false;
end;

function TTagReader.UpdateFile: boolean;
var
  SaveFile : string;
begin
//  SaveFile:=  ExtractFileNameOnly(FileName) + '.~.'+ ExtractFileExt(FileName);
  SaveFile:=  ChangeFileExt(FileName, '.~.'+ ExtractFileExt(FileName));

  result := SaveToFile(SaveFile);

  if Result then
     result := DeleteFile(FileName);

  result := RenameFile( SaveFile, FileName);

end;

{ TTags }

function TTags.GetFrameByID(ID: string): TFrameElement;
var
  i:Integer;
  fid: Integer;
begin
  fid:= -1;
  for i:=0 to FramesList.Count -1 do
    begin
      if UpperCase(TFrameElement(FramesList[i]).ID) = UpperCase(id) then
         begin
           fid:=i;
           break;
         end;
    end;
  if fid <> -1 then
     result := GetFrameByIndex(fid)
  else
     result := nil;
end;

function TTags.GetImageCount: integer;
begin
  Result := ImagesList.Count;
end;

function TTags.GetCount: integer;
begin
  Result := FramesList.Count;
end;

function TTags.GetFrameByIndex(Index: integer): TFrameElement;
begin
  Result := TFrameElement(FramesList[Index]);
end;

function TTags.GetImageByID(ID: string): TImageElement;
var
  i:Integer;
  fid: Integer;
begin
  fid:= -1;
  for i:=0 to FramesList.Count -1 do
    begin
      if UpperCase(TImageElement(FramesList[i]).ID) = UpperCase(id) then
         begin
           fid:=i;
           break;
         end;
    end;
  if fid <> -1 then
     result := GetImageByIndex(fid)
  else
     result := nil;
end;

function TTags.GetImageByIndex(Index: integer): TImageElement;
begin
   Result := TImageElement(ImagesList[Index]);
end;

constructor TTags.Create;
begin
  FramesList := TFPObjectList.Create(True);
  ImagesList := TFPObjectList.Create(True);
end;

destructor TTags.Destroy;
begin
  FreeAndNil(FramesList);
  FreeAndNil(ImagesList);
  inherited Destroy;

end;

procedure TTags.Add(Frame: TFrameElement);
begin
  FramesList.Add(Frame);
end;

procedure TTags.Remove(Frame: TFrameElement);
begin
  FramesList.Remove(Frame);
end;

procedure TTags.AddImage(Image: TImageElement);
begin
  ImagesList.Add(Image);
end;

procedure TTags.Clear;
begin
  FramesList.Clear;
  ImagesList.Clear;
end;

function TTags.GetCommonTags: TCommonTags;
begin
  with result do
    begin
      ID:=-1;
      FileName:='';
      Album := '';
      AlbumArtist:= '';
      Artist:= '';
      Comment:= '';
      Duration:= 0;
      Genre:= '';
      Title:= '';
      Track:= 0;
      TrackString:= '';
      Year:= '';
      HasImage:=False;
    end;
end;

procedure TTags.SetCommonTags(CommonTags: TCommonTags);
begin
  //
end;

procedure TTags.SetFrameValue(const ID:String;const Value:String; FrameClass: TFrameElementClass);
var
  Element : TFrameElement;
begin
  Element := FramesByID[ID];
  if Element = nil then
     begin
       Element := FrameClass.Create(ID);
       Element.Tagger:=self;
       Add(Element);
     end;

  Element.AsString := Value;
end;


function TTags.GetFrameValue(ID:String):String;
var
  Element : TFrameElement;
begin
  Element := FramesByID[ID];
  if Element = nil then
     result := ''
  else
     Result := Element.AsString;
end;

{ TFrameElement }

procedure TFrameElement.SetID(AValue: string);
begin
  if fId <> AValue then
     fId:= AValue;
end;

function TFrameElement.GetSize: DWord;
begin
  Result := 0;
end;

function TFrameElement.GetID: string;
begin
  Result:=fId;
end;

constructor TFrameElement.Create;
begin
 //
end;

constructor TFrameElement.Create(ID: String);
begin
  fId := ID;
end;

destructor TFrameElement.Destroy;
begin
  //
end;

end.

