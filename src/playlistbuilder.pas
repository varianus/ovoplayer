unit playlistbuilder;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, GeneralFunc, fgl, FPJSON, fpjsonrtti;

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
    FStringValue: string;
    FTestIndex: integer;

    function GetFilterNumber: string;
    function GetFilterRating: string;
    function GetFilterText: string;
    procedure SetFieldID(AValue: integer);
    procedure SetFloatValue(AValue: Double);
    procedure SetIntegerValue(AValue: int64);
    procedure SetStringValue(AValue: string);
    procedure SetTestIndex(AValue: integer);

  public
    function isExecutable: boolean;
    function GetFilter: string;
  published
    property TestIndex : integer read FTestIndex write SetTestIndex;
    property StringValue : string read FStringValue write SetStringValue;
    property IntegerValue: int64 read FIntegerValue write SetIntegerValue;
    property FloatValue : Double read FFloatValue write SetFloatValue;
    Property FieldID : integer read FFieldID write SetFieldID;

  end;
  TIntPlayListBuilder = specialize TFPGObjectList<TFieldFilter>;

  TPlayListBuilder = class (TIntPlayListBuilder)
  private
    FSongLimit: integer;
    FSortFieldID: integer;
    function GetExecutable: boolean;
    function GetFilter: string;
    function GetSortClause: string;
    procedure SetSongLimit(AValue: integer);
    procedure SetSortFieldID(AValue: integer);
  public
   //
    constructor Create;
    Destructor Destroy; override;
   //
    Procedure ToJson(FileName:TfileName);

    Property Filter : string read GetFilter;
    property isExecutable: boolean read GetExecutable;
    Property SortClause : string read GetSortClause;

  published
    property SongLimit: integer read FSongLimit write SetSongLimit;
    property SortFieldID: integer read FSortFieldID write SetSortFieldID;
    //


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

const
   FieldCount = 16;
   FieldArray : array [0..FieldCount-1] of FieldRec =  (
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

Procedure SortFields;

Function FindIndexByID(const ID: Integer): Integer;

implementation

function MyCompare (const Item1, Item2: integer): Integer;
  begin
    result := CompareText(FieldArray[item1].FieldLabel, FieldArray[item2].FieldLabel);
  end;

procedure SortFields;
type
  myArr = specialize TSortArray<FieldRec>;
begin
  myArr.Sort(FieldArray, @MyCompare);
end;

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


function TFieldFilter.GetFilterText: string;
var
  op : string;
  Value: string;
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
     if StringValue = EmptyStr then
        exit;
     end;

  if NeedWildcards then
     Value :=  QuotedStr('%'+StringValue+'%')
  else
     Value :=  QuotedStr(StringValue);

  result := format(' %s %s %s',[FieldArray[FIdx].FieldName, op, Value]);

end;

procedure TFieldFilter.SetFieldID(AValue: integer);
begin
  if FFieldID=AValue then Exit;
  FFieldID:=AValue;
  FIdx:= FindIndexByID(FFieldID);
end;

procedure TFieldFilter.SetFloatValue(AValue: Double);
begin
  if FFloatValue=AValue then Exit;
  FFloatValue:=AValue;
end;

procedure TFieldFilter.SetIntegerValue(AValue: int64);
begin
  if FIntegerValue=AValue then Exit;
  FIntegerValue:=AValue;
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
  Value: string;
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

  Value := IntToStr(IntegerValue);

  result := format(' %s %s %s',[FieldArray[FIdx].FieldName, op, Value]);

end;

function TFieldFilter.GetFilterRating: string;
var
  op : string;
  Value: string;
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
    Value := ''
  else
    Value := IntToStr(IntegerValue + 1);


  result := format(' %s %s %s',[FieldArray[FIdx].FieldName, op, Value]);

end;

function TFieldFilter.GetFilter: string;
begin
 Result := EmptyStr;
 if FIdx  < 0 then exit;
 case  FieldArray[FIdx].Kind of
   ekText : result := GetFilterText;
   ekDate : ;
   ekNumber : result := GetFilterNumber;
   ekRating : result := GetFilterRating;

 end;
end;

function TFieldFilter.isExecutable: boolean;
begin
 Result := False;
 if FIdx < 0 then exit;
 case  FieldArray[FIdx].Kind of
   ekText : result := (TestIndex > 3) or
                      ((TestIndex < 4) and (StringValue <> EmptyStr) )   ;

   ekDate : ;
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
    Result := FieldArray[FindIndexByID(SortFieldID)].FieldName;

  if SongLimit > 0 then
    Result:= Result + ' Limit ' + inttostr(SongLimit);


end;

procedure TPlayListBuilder.SetSortFieldID(AValue: integer);
begin
  if FSortFieldID=AValue then Exit;
  FSortFieldID:=AValue;
end;

procedure TPlayListBuilder.ToJson(FileName: TfileName);
var
  i: integer;
  Streamer  : TJSONStreamer;
  JSONString : string;
  JSONOnject : TJSONObject;
  JSONArray: TJSONArray;
  f: Text;
begin
  Streamer := TJSONStreamer.Create(nil);

  JSONOnject := Streamer.ObjectToJSON(Self);
  JSONArray := TJSONArray.Create;

  for i := 0 to Count -1 do
     JSONArray.Add(Streamer.ObjectToJSON(Items[i]));

  JSONOnject.Add('Filters',JSONArray);

  JSONString := JSONOnject.AsJSON;
  AssignFile(f,FileName);
  Rewrite(f);
  WriteLn(f,JSONString);
  CloseFile(f);
  Streamer.Free;
  JSONOnject.Free;

end;

constructor TPlayListBuilder.Create;
begin
  Inherited Create(True);
end;

destructor TPlayListBuilder.Destroy;
begin
  inherited Destroy;
end;

end.

