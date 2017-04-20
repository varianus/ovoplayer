unit playlistbuilder;

{$mode DELPHI}{$H+}

interface

uses
 Classes, LazUTF8Classes, SysUtils, GeneralFunc, fgl, FPJSON, jsonparser, fpjsonrtti;

type

 EditorKind =(ekText,ekDate,ekRating,ekNumber);

  FieldRec = record
    Id : integer;
    FieldName : string;
    FieldLabel : string;
    Kind : EditorKind;
  end;

  { TPlayListBuilder }

  { TFieldFilter }
{$M+}
  TFieldFilter = class
  private
    FFieldID: integer;
    FFloatValue: Double;
    FIdx: integer;
    FIntegerValue: int64;
    FKind: EditorKind;
    FStringValue: string;
    FTestIndex: integer;

    function GetFilterDate: string;
    function GetFilterNumber: string;
    function GetFilterRating: string;
    function GetFilterText: string;
    procedure SetFieldID(AValue: integer);
    procedure SetKind(AValue: EditorKind);
    procedure SetStringValue(AValue: string);
    procedure SetTestIndex(AValue: integer);

  public
    function isExecutable: boolean;
    function GetFilter: string;
    Function AsInteger:integer;
    Function AsDate:TDate;
  published
    property TestIndex : integer read FTestIndex write SetTestIndex;
    property Value : string read FStringValue write SetStringValue;
    property Kind : EditorKind read FKind write SetKind;
    Property FieldID : integer read FFieldID write SetFieldID;

  end;


  TPlayListBuilder = class (TFPGObjectList<TFieldFilter>)
  private
    FName: string;
    FSongLimit: integer;
    FSortAscending: boolean;
    FSortFieldID: integer;
    function GetExecutable: boolean;
    function GetFilter: string;
    function GetSortClause: string;
    procedure SetName(AValue: string);
    procedure SetSongLimit(AValue: integer);
    procedure SetSortAscending(AValue: boolean);
    procedure SetSortFieldID(AValue: integer);
  public
   //
    constructor Create;
    Destructor Destroy; override;
   //
    //Procedure ToJson(FileName:TfileName);
    //procedure FromJson(FileName: TfileName);

    function ToJson(): TJSONObject;
    procedure FromJson(json: TJSONObject);

    Property Filter : string read GetFilter;
    property isExecutable: boolean read GetExecutable;
    Property SortClause : string read GetSortClause;


  published
    property Name: string read FName write SetName;
    property SongLimit: integer read FSongLimit write SetSongLimit;
    property SortFieldID: integer read FSortFieldID write SetSortFieldID;
    property SortAscending: boolean read FSortAscending write SetSortAscending;

    //
  end;

  { TPlaylistContainer }

  TPlaylistContainer = class
  private
    FFileName:TFileName;
    JSONArray: TJSONArray;
    Stream: TFileStreamUTF8;
  public
    procedure Save;
    function GetPlaylists(List: TStrings): integer;
    Function GetByName(PlaylistName:string): TJSONObject;
    Function GetByIndex(Index:integer): TJSONObject;
    procedure SetByIndex(Index:integer; Json: TJSONObject);
    Procedure Delete(Index:integer);
  public
    class Procedure CreateDefaultFileIfMissing(FileName:TFileName);
    constructor Create(FileName:TFileName);
    Destructor Destroy; override;

  end;


ResourceString
  // Diplay label for fields
  RS_Filename     = 'File Name';
  RS_TrackString  = 'Track String';
  RS_Track        = 'Track';
  RS_Title        = 'Title';
  RS_Album        = 'Album';
  RS_Artist       = 'Artist';
  RS_AlbumArtist  = 'Album Artist';
  RS_Genre        = 'Genre';
  RS_year         = 'Year';
  RS_Duration     = 'Duration';
  RS_Playcount    = 'Play count';
  RS_Rating       = 'Rating';
  RS_LastPlay     = 'Date Last Played';
  RS_Added        = 'Date Added';
  RS_FileSize     = 'File Size';
  RS_FileDate     = 'File Date';

  RS_Random       = 'Random';


  RS_EqualTo      = 'equal to';
  RS_NotEqualTo   = 'not equal to';
  RS_BiggerThan   = 'bigger than';
  RS_NotRated     = 'not rated';
  RS_Is           = 'is';
  RS_IsNot        = 'is not';
  RS_Contains     = 'contains';
  RS_NotContains  = 'not contains';
  RS_IsEmpty      = 'is empty';
  RS_IsNotEmpty   = 'is not empty';
  RS_LessThan     = 'less than';

  RS_Between      = 'between';

  RS_On           = 'on';
  RS_NotOn        = 'not on';
  RS_Before       = 'before';
  RS_After        = 'after';
  RS_InTheLast    = 'in the last';
  RS_NotInTheLast = 'not in the last';

// Titles of standardPlaylist
  RS_Name25_Random_songs = '25 Random tracks';
  RS_NameAllsongs = 'All tracks';
  RS_NameMostplayed = 'Most played';
  RS_NameNeverplayed = 'Never played';
  RS_NameLongesttimefromlastplay = 'Longest time from last play';
  RS_NameNewersongs = 'Newer tracks';

const
   FieldCount = 16;
type
   TFieldArray = array [0..FieldCount-1] of FieldRec;
const
   FieldArray : TFieldArray =  (
   (ID : 1; FieldName : 'Filename'; FieldLabel : RS_Filename; Kind: ekText),
   (ID : 2; FieldName : 'TrackString'; FieldLabel :RS_TrackString; Kind: ekText),
   (ID : 3; FieldName : 'Track'; FieldLabel : RS_Track; Kind: ekNumber),
   (ID : 4; FieldName : 'Title'; FieldLabel : RS_Title; Kind: ekText),
   (ID : 5; FieldName : 'Album'; FieldLabel : RS_Album; Kind: ekText),
   (ID : 6; FieldName : 'Artist'; FieldLabel : RS_Artist ; Kind: ekText),
   (ID : 7; FieldName : 'AlbumArtist'; FieldLabel : RS_AlbumArtist; Kind: ekText ),
   (ID : 8; FieldName : 'Genre'; FieldLabel : RS_Genre; Kind: ekText),
   (ID : 9; FieldName : 'year'; FieldLabel : RS_year;  Kind: ekNumber),
   (ID :10; FieldName : 'Duration'; FieldLabel : RS_Duration; Kind: ekNumber),
   (ID :11; FieldName : 'Playcount'; FieldLabel : RS_Playcount; Kind: ekNumber ),
   (ID :12; FieldName : 'Rating'; FieldLabel : RS_Rating;  Kind: EKRating),
   (ID :13; FieldName : 'LastPlay'; FieldLabel : RS_LastPlay; Kind: EkDate),
   (ID :14; FieldName : 'Added'; FieldLabel : RS_Added; Kind: EkDate),
   (ID :15; FieldName : 'FileSize'; FieldLabel : RS_FileSize;  Kind: ekNumber),
   (ID :16; FieldName : 'FileDate'; FieldLabel :RS_FileDate; Kind: EkDate)
   );

  DefaultPlayList =
    '[{"Name":"%0:s","SongLimit":25,"SortAscending":true,"SortFieldID":-1,"Filter'
  +'s":[]},{"Name":"%1:s","SongLimit":-1,"SortAscending":false,"SortFieldID":-1'
  +',"Filters":[]},{"Name":"%2:s","SongLimit":50,"SortAscending":false,"SortFie'
  +'ldID":11,"Filters":[]},{"Name":"%3:s","SongLimit":100,"SortAscending":true,'
  +'"SortFieldID":-1,"Filters":[{"FieldID":11,"Kind":"ekText","TestIndex":0,"Va'
  +'lue":"0"}]},{"Name":"%4:s","SongLimit":25,"SortAscending":true,"SortFieldID'
  +'":13,"Filters":[{"FieldID":11,"Kind":"ekText","TestIndex":2,"Value":"0"}]},'
  +'{"Name":"%5:s","SongLimit":100,"SortAscending":false,"SortFieldID":14,"Filt'
  +'ers":[{"FieldID":11,"Kind":"ekText","TestIndex":0,"Value":"0"}]}]';


Function FindIndexByID(const ID: Integer): Integer;

implementation
//{$R defaultplaylist.rc}


function FindIndexByID(const ID:Integer): Integer;
var
  i: integer;
begin
  for i := 0 to FieldCount -1 do
    begin
      if FieldArray[i].Id = id then
        begin
          result:= i;
          exit;
        end;
    end;
 Result:= -1;

end;

{ TPlaylistContainer }

procedure TPlaylistContainer.Save;
begin
 Stream.position := 0;
 JSONArray.DumpJSON(Stream);
end;

function TPlaylistContainer.GetPlaylists(List: TStrings): integer;
var
  i: integer;
begin
  Result := 0;
  List.Clear;
  for i := 0 to JSONArray.Count -1 do
     begin
       List.AddObject(JSONArray[i].GetPath('Name').AsString, Pointer(PtrUInt(i)));
     end;
  Result := List.Count;
end;

function TPlaylistContainer.GetByName(PlaylistName: string): TJSONObject;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to JSONArray.Count -1 do
     begin
       if JSONArray[i].GetPath('Name').AsString = PlaylistName then
         Result := TJSONObject(JSONArray[i]);
     end;
end;

function TPlaylistContainer.GetByIndex(Index: integer): TJSONObject;
begin
  if (Index > -1) and (index < JSONArray.Count) then
    Result := TJSONObject(JSONArray[Index])
  else
    Result := nil;
end;

procedure TPlaylistContainer.SetByIndex(Index: integer; Json: TJSONObject);
begin
  if Index = -1 then
    JSONArray.Add(Json)
  else
    begin
      JSONArray[index] := Json;
    end;
end;

procedure TPlaylistContainer.Delete(Index: integer);
begin
  JSONArray.Delete(Index);
end;

class procedure TPlaylistContainer.CreateDefaultFileIfMissing(
  FileName: TFileName);
var
  FStream: TFileStreamUTF8;
  str:string;
begin
  if not FileExists(FileName) then
    begin
      fStream:= TFileStreamUTF8.Create(FileNAme, fmOpenWrite + fmCreate);
      str := Format(DefaultPlayList, [
                             RS_Name25_Random_songs,
                             RS_NameAllsongs,
                             RS_NameMostplayed,
                             RS_NameNeverplayed,
                             RS_NameLongesttimefromlastplay,
                             RS_NameNewersongs
                             ]);
      fStream.WriteBuffer(str[1], Length(str));
     fStream.free;
    end;
end;


constructor TPlaylistContainer.Create(FileName:TFileName);
begin
  FFileName:= FileName;
  Stream:= TFileStreamUTF8.Create(FFileNAme, fmOpenReadWrite);
  JSONArray := TJSONArray(GetJSON(Stream, True));

end;

destructor TPlaylistContainer.Destroy;
begin
  inherited Destroy;
  JSONArray.Free;
  Stream.Free;
end;


function TFieldFilter.GetFilterText: string;
var
  op : string;
  theValue: string;
  NeedWildcards: boolean;
begin
  result:='';
  case TestIndex of
    0: begin op := 'like';     NeedWildcards:= true;  end; // contains
    1: begin op := 'not like'; NeedWildcards:= true;  end; // not contains
    2: begin op := '=';        NeedWildcards:= false; end; // is
    3: begin op := '<>';       NeedWildcards:= false; end; // is not
    4: begin op := '=';        NeedWildcards:= false; end; // is empty
    5: begin op := '<>';       NeedWildcards:= false; end; // is not empty
  else
    exit;
  end;

  if TestIndex < 4 then
     begin
     if  FStringValue = EmptyStr then
        exit;
     end;

  if NeedWildcards then
     theValue :=  QuotedStr('%'+FStringValue+'%')
  else
     theValue :=  QuotedStr(FStringValue);

  result := format(' %s %s %s',[FieldArray[FIdx].FieldName, op, theValue]);

end;

procedure TFieldFilter.SetFieldID(AValue: integer);
begin
  if FFieldID=AValue then Exit;
  FFieldID:=AValue;
  FIdx:= FindIndexByID(FFieldID);
end;

procedure TFieldFilter.SetKind(AValue: EditorKind);
begin
  if FKind=AValue then Exit;
  FKind:=AValue;
end;

procedure TFieldFilter.SetStringValue(AValue: string);
begin
  if FStringValue=AValue then Exit;
  FStringValue:=AValue;
end;

procedure TFieldFilter.SetTestIndex(AValue: integer);
begin
  if FTestIndex=AValue then Exit;
  FTestIndex:=AValue;
end;

function TFieldFilter.GetFilterNumber: string;
var
  op : string;
begin
  result:='';

  case TestIndex of
    0: begin op := '=';  end; // equal to
    1: begin op := '<>'; end; // not equal to
    2: begin op := '>';  end; // bigger than
    3: begin op := '<';  end; // less than
  else
    exit;
  end;

  result := format(' %s %s %s',[FieldArray[FIdx].FieldName, op, FStringValue]);

end;

function TFieldFilter.GetFilterDate: string;
var
  op : string;
begin
  result:='';

  case TestIndex of
    0: begin op := '=';  end; // equal to
    1: begin op := '<>'; end; // not equal to
    2: begin op := '<';  end; // bigger than
    3: begin op := '>';  end; // less than
  else
    exit;
  end;

  result := format(' %s %s %s',[FieldArray[FIdx].FieldName, op, FStringValue]);

end;


function TFieldFilter.GetFilterRating: string;
var
  op : string;
  TheValue: string;
begin
  result:='';

  case TestIndex of
    0: begin op := '=';  end; // equal to
    1: begin op := '<>'; end; // not equal to
    2: begin op := '>';  end; // bigger than
    3: begin op := '<';  end; // less than
    4: begin op := 'is null';  end; // equal to

  else
    exit;
  end;

  if TestIndex = 4 then
    TheValue := ''
  else
    TheValue := FStringValue;


  result := format(' %s %s %s',[FieldArray[FIdx].FieldName, op, TheValue]);

end;

function TFieldFilter.GetFilter: string;
begin
 Result := EmptyStr;
 if FIdx  < 0 then exit;
 case  FieldArray[FIdx].Kind of
   ekText : result := GetFilterText;
   ekDate : result := GetFilterDate;
   ekNumber : result := GetFilterNumber;
   ekRating : result := GetFilterRating;

 end;
end;

function TFieldFilter.AsInteger: integer;
begin
  if not TryStrToInt(FStringValue,Result) then
     Result:=0;

end;

function TFieldFilter.AsDate: TDate;
var
  tmp : Integer;
  dbl:double;
begin
  if not TryStrToInt(FStringValue,tmp) then
     Result:=0
  else
     begin
        dbl := tmp;
        Result := TDate(dbl);

     end;
end;

function TFieldFilter.isExecutable: boolean;
begin
 Result := False;
 if FIdx < 0 then exit;
 case  FieldArray[FIdx].Kind of
   ekText : result := (TestIndex > 3) or
                      ((TestIndex < 4) and (FStringValue <> EmptyStr) )   ;

   ekDate : Result := AsDate > 0;
   ekNumber : Result := True;
   ekRating : result := True;
 end;
end;

{ TPlayListBuilder }

procedure TPlayListBuilder.SetSongLimit(AValue: integer);
begin
  if FSongLimit=AValue then Exit;
  FSongLimit:=AValue;
end;

procedure TPlayListBuilder.SetSortAscending(AValue: boolean);
begin
  if FSortAscending=AValue then Exit;
  FSortAscending:=AValue;
end;

function TPlayListBuilder.GetFilter: string;
var
  i :integer;
begin
 Result:= '1=1'; // dummy test to simplify AND logic

 for i := 0 to Count -1 do
   begin
     Result := Result + ' AND ' + Items[i].GetFilter;
   end;
end;

function TPlayListBuilder.GetExecutable: boolean;
var
  i: integer;
begin
 Result := true;
  for i := 0 to Count -1 do
    Result := Result and Items[i].isExecutable;

end;

function TPlayListBuilder.GetSortClause: string;
begin
  if SortFieldID = -1 then
    Result := ' random() '
  else
    begin
       Result := FieldArray[FindIndexByID(SortFieldID)].FieldName;
       if not FSortAscending then
         Result := result + ' desc ';
    end;

  if SongLimit > 0 then
    Result:= Result + ' Limit ' + inttostr(SongLimit);


end;

procedure TPlayListBuilder.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TPlayListBuilder.SetSortFieldID(AValue: integer);
begin
  if FSortFieldID=AValue then Exit;
  FSortFieldID:=AValue;
end;

//procedure TPlayListBuilder.ToJson(FileName: TfileName);
//var
//  i: integer;
//  Streamer  : TJSONStreamer;
//  JSONString : string;
//  JSONOnject : TJSONObject;
//  JSONArray: TJSONArray;
//  Stream: TFileStreamUTF8;
//begin
//  Streamer := TJSONStreamer.Create(nil);
//  Streamer.Options:= Streamer.Options + [jsoUseFormatString];
//  JSONOnject := Streamer.ObjectToJSON(Self);
//  JSONArray := TJSONArray.Create;
//
//  for i := 0 to Count -1 do
//     JSONArray.Add(Streamer.ObjectToJSON(Items[i]));
//
//  JSONOnject.Add('Filters',JSONArray);
//
//  Stream := TFileStreamUTF8.Create(FileName, fmOpenWrite + fmCreate);
//
//  JSONOnject.DumpJSON(Stream);
//  Stream.Free;
//
//  Streamer.Free;
//  JSONOnject.Free;
//
//end;
//
//procedure TPlayListBuilder.FromJson(FileName: TfileName);
//var
//  i: integer;
//  DeStreamer  : TJSONDeStreamer;
//  JSONString : string;
//  JSONOnject : TJSONObject;
//  JSONArray: TJSONArray;
//  Stream: TFileStreamUTF8;
//  item: TFieldFilter;
//begin
//  Stream:= TFileStreamUTF8.Create(FileName, fmOpenRead);
//  JSONOnject := TJSONObject(GetJSON(Stream, True));
//
//  DeStreamer := TJSONDeStreamer.Create(nil);
//  DeStreamer.JSONToObject(JSONOnject, self);
//
//  Clear;
//  JSONArray := nil;
//  JSONArray := JSONOnject.Get('Filters',JSONArray);
//
//  for i := 0 to JSONArray.Count -1 do
//     begin
//       Item := TFieldFilter.Create;
//       DeStreamer.JSONToObject(JSONArray.Objects[i],item);
//       Add(item);
//     end;
//  Stream.Free;
//  DeStreamer.Free;
//  JSONOnject.Free;
//end;


constructor TPlayListBuilder.Create;
begin
  Inherited Create(True);
end;

destructor TPlayListBuilder.Destroy;
begin
  inherited Destroy;
end;

function TPlayListBuilder.ToJson(): TJSONObject;
var
  i: integer;
  Streamer  : TJSONStreamer;
  JSONString : string;
  JSONOnject : TJSONObject;
  JSONArray: TJSONArray;
  Stream: TFileStreamUTF8;
begin
  Streamer := TJSONStreamer.Create(nil);
  Streamer.Options:= Streamer.Options + [jsoUseFormatString];
  JSONOnject := Streamer.ObjectToJSON(Self);

  JSONArray := TJSONArray.Create;

  for i := 0 to Count -1 do
     JSONArray.Add(Streamer.ObjectToJSON(Items[i]));

  JSONOnject.Add('Filters',JSONArray);

  Result := JSONOnject;

  Streamer.Free;
end;

procedure TPlayListBuilder.FromJson(json: TJSONObject);
var
  i: integer;
  DeStreamer  : TJSONDeStreamer;
  JSONArray: TJSONArray;
  Stream: TFileStreamUTF8;
  item: TFieldFilter;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  DeStreamer.JSONToObject(json, self);

  Clear;
  JSONArray := nil;
  JSONArray := json.Get('Filters',JSONArray);

  for i := 0 to JSONArray.Count -1 do
     begin
       Item := TFieldFilter.Create;
       DeStreamer.JSONToObject(JSONArray.Objects[i],item);
       Add(item);
     end;
  DeStreamer.Free;

end;

end.

