{
This file is part of OvoPlayer
Copyright (C) 2011 Marco Caselli

OvoPlayer is free software; you can redistribute it and/or
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
{$I ovoplayer.inc}
unit PlayList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Song, AudioTag;

type

  TplSortField = (stNone, stTitle, StAlbum, stArtist, stDuration,  stTrack, stGenre,
                  stYear, stAlbumArtist, stFileName);

  TplSortDirection = (sdplAscending, sdplDiscending);
  TplRepeat = (rptNone, rptTrack, rptAlbum, rptPlayList);

  { TPlayList }
  TPlayListSortCompare = function (Item1, Item2: Pointer): Integer of object;

  TPlayList = class(TList)
  private
    FItemIndex: integer;
    FRepeatMode: TplRepeat;
    FSortField: TplSortField;
    fUpdating:  boolean;
    fSortDirection : TplSortDirection;
    fSavedPointer :Pointer;
    function CheckItem(Item: integer): integer;
    function GetCurrentItem: TSong;
    function GetItem(Index: integer): TSong;
    function GetTotalTime: double;
    function MyCompare(p1, p2: Pointer): integer;
    procedure SetItem(Index: integer; const AValue: TSong);
    procedure SetItemIndex(const AValue: integer);
    procedure SetRepeatMode(const AValue: TplRepeat);
    procedure SetSortDirection(const AValue: TplSortDirection);
    procedure SetSortField(const AValue: TplSortField);
  public
    Constructor Create;
    function EnqueueFile(FileName: TFileName): integer;
    function Add(ASong: TSong): integer;
    procedure Delete(Index: integer);
    procedure Clear;  override;
    function Next: TSong;
    function Previous: TSong;
    procedure Swap(Item1, Item2: integer);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure LoadAllTags;
    procedure Shuffle;
    Procedure Sort(Field: TplSortField; Direction:TplSortDirection); overload;
    Procedure Sort; overload;
    procedure PushPos;
    procedure PopPos;
    Property RepeatMode: TplRepeat read FRepeatMode write SetRepeatMode;
    property SortField: TplSortField Read FSortField Write SetSortField;
    property SortDirection : TplSortDirection read FSortDirection write SetSortDirection;
    property CurrentItem: TSong read GetCurrentItem;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Songs[Index: integer]: TSong read GetItem write SetItem; default;
    property TotalTime: double read GetTotalTime;
  end;


implementation
uses
  FPCAdds, math;

Procedure IntQuickSort(FList: PPointerList; L, R : Longint;
                     Compare: TPlayListSortCompare);
var
  I, J : Longint;
  P, Q : Pointer;
begin
 repeat
   I := L;
   J := R;
   P := FList^[ (L + R) div 2 ];
   repeat
     while Compare(P, FList^[i]) > 0 do
       I := I + 1;
     while Compare(P, FList^[J]) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := FList^[I];
       Flist^[I] := FList^[J];
       FList^[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   if L < J then
     intQuickSort(FList, L, J, Compare);
   L := I;
 until I >= R;
end;

function TPlayList.MyCompare(p1, p2: Pointer): integer;
var S1: TSong absolute p1;
    S2: TSong absolute p2;
begin
  case FSortField of
    stTitle      : result:= CompareStr(s1.Title,s2.Title);
    StAlbum      : result:= CompareStr(s1.Tags.Album, s2.Tags.Album);
    StArtist     : result:= CompareStr(s1.Tags.Artist, s2.Tags.AlbumArtist);
    stDuration   : result:= CompareValue(s1.Tags.Duration, s2.Tags.Duration);
    stTrack : begin
                 result:= CompareStr(s1.Tags.Album, s2.Tags.Album);
                 if result = 0 then
                    result:= CompareValue(s1.Tags.Track, s2.Tags.Track);
              end;
    stGenre       : result:= CompareStr(s1.tags.Genre,s2.tags.Genre);
    stYear        : result:= CompareStr(s1.tags.Year,s2.tags.Year);
    stAlbumArtist : result:= CompareStr(s1.tags.AlbumArtist,s2.tags.AlbumArtist);
    stFileName    : result:= CompareStr(s1.FileName, s2.FileName);
  else
    result:=0;
  end;

  if fSortDirection = sdplDiscending then
     Result := Result * -1;
end;


function TPlayList.EnqueueFile(FileName: TFileName): integer;
var
  Song: TSong;
begin
  if FileExists(FileName) then
     begin
      song   := TSong.Create(FileName);
      Result := Add(Song);
     end
  else
    Result := -1;
end;

function TPlayList.Add(ASong: TSong): integer;
begin
  Result := inherited Add(Pointer(ASong));
end;

procedure TPlayList.Delete(Index: integer);
begin
  if (Songs[Index]) <> nil then
    Songs[Index].Free;

  inherited Delete(Index);

  if index < FItemIndex then
    Dec(FItemIndex);
end;

procedure TPlayList.Clear;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    Songs[i].Free;

  inherited Clear;
  ItemIndex:= -1;
  fSavedPointer:= nil;
end;

function TPlayList.GetTotalTime: double;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + Songs[i].Tags.Duration;
end;

function TPlayList.GetItem(Index: integer): TSong;
begin
  Result := TSong(inherited Get(Index));
end;

procedure TPlayList.SetItem(Index: integer; const AValue: TSong);
begin
  Put(Index, Pointer(AValue));
end;

procedure TPlayList.SetItemIndex(const AValue: integer);
begin
  if FItemIndex = AValue then
    exit;

  if Count = 0 then
    FItemIndex := -1
  else
    FItemIndex := AValue;

end;

procedure TPlayList.SetRepeatMode(const AValue: TplRepeat);
begin
  if FRepeatMode=AValue then exit;
  FRepeatMode:=AValue;
end;

procedure TPlayList.SetSortDirection(const AValue: TplSortDirection);
begin
  if FSortDirection=AValue then exit;
  FSortDirection:=AValue;
end;

procedure TPlayList.SetSortField(const AValue: TplSortField);
begin
  if FSortField=AValue then exit;
     FSortField:=AValue;

end;

constructor TPlayList.Create;
begin
  inherited Create;
  fSavedPointer:=nil;;
end;

function TPlayList.GetCurrentItem: TSong;
begin
  if ItemIndex <> -1 then
    Result := TSong(Items[ItemIndex])
  else
    Result := nil;
end;

function TPlayList.Next: TSong;
var
  Album: string;
begin
  case RepeatMode of
  rptTrack:
     begin
       Result := CurrentItem;
     end;
  rptAlbum:
     begin
       Album := CurrentItem.Tags.Album;
       repeat
        if ItemIndex < Count - 1 then
          ItemIndex := ItemIndex + 1
        else
          ItemIndex := 0;

       until CurrentItem.Tags.Album = Album;
       Result := CurrentItem;
     end;

  rptPlayList:
    begin
      if ItemIndex < Count - 1 then
        ItemIndex := ItemIndex + 1
      else
        begin
        if Count = 0 then
          ItemIndex := -1
        else
          ItemIndex := 0;
        end;
      Result := CurrentItem;
    end;

  rptNone:
    begin
      if ItemIndex < Count - 1 then
         ItemIndex := ItemIndex + 1
      else
         ItemIndex := -1;
      Result := CurrentItem;
    end;

  end;

end;

function TPlayList.Previous: TSong;
begin
  if ItemIndex < 1 then
    ItemIndex := Count - 1
  else
    ItemIndex := ItemIndex - 1;

  Result := CurrentItem;
end;

procedure TPlayList.BeginUpdate;
begin
  fUpdating := True;
end;

procedure TPlayList.EndUpdate;
begin
  fUpdating := False;
end;

Function TPlayList.CheckItem(Item: integer): integer;
begin
 if item > count then
    result:=count
 else
   if item < 0 then
      result :=0
   else
      result := item;

end;

procedure TPlayList.Swap(Item1, Item2: integer);
var
  Q:pointer;
begin
  item1:= CheckItem(item1);
  item2:= CheckItem(item2);
  if item1 = item2 then
     exit;
  q:= List^[Item1];
  List^[Item1]:= List^[Item2];
  List^[Item2]:=Q;

end;

procedure TPlayList.LoadAllTags;
var
  i:     integer;
  ASong: Tsong;
begin
  for i := 0 to Count - 1 do
    begin
    ASong := Songs[i];
    if not ASong.TagLoaded then
      LoadTags(ASong);
    end;
end;

procedure TPlayList.Shuffle;
var
  randomIndex: integer;
  cnt: integer;
  p: Pointer;
begin
  p:= List^[FItemIndex];
  Randomize;
  for cnt := 0 to Count - 1 do
    begin
      randomIndex := Random(-cnt + Count) ;
      Swap(cnt, cnt + randomIndex);
    end;

  FItemIndex:=IndexOf(p);

end;

procedure TPlayList.Sort;
var
 p: Pointer;
begin
  p:= List^[FItemIndex];
  IntQuickSort(Self.List, 0, count-1, @MyCompare);
  FItemIndex:=IndexOf(p);
end;

procedure TPlayList.PushPos;
begin
 fSavedPointer := List^[FItemIndex];
end;

procedure TPlayList.PopPos;
begin
 if fSavedPointer <> nil then
    FItemIndex:=IndexOf(fSavedPointer);
end;

procedure TPlayList.Sort(Field: TplSortField; Direction:TplSortDirection);
begin
  FSortField := Field;
  fSortDirection := Direction;
  Sort;
end;

end.