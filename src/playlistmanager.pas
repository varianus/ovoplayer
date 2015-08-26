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
unit PlayListManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Playlist, AudioTag, MediaLibrary, lazutf8classes;

const
  XSPF_VERSION = 'http://ovoplayer.altervista.org/ovoplayer/0/1/';

type

  { TPlayListManager }
  TPlayListType = (pltUnknown, pltXSPF, pltM3U, pltBSPL, pltWPL, pltASX, pltPLS);

const
  TPlayListTypeDesc: array [pltUnknown..pltPLS] of string =
    ('Unknown',
    'XML Shareable Playlist Format',
    'M3U Playlist',
    'BSPlayer Playlist',
    'Windows Media Player Playlist',
    'Windows Media ASX Playlist',
    'PLS Playlist');

type

  TPlayListManager = class
  private
    fSavedTime: integer;
    function ProcessPath(FileName: string; CurPlayList: string): string;
  public
    function GetPlayListType(FileName: string): TPlayListType;
    function ImportFromDirectory(const Directory: string; Recursive: boolean;
      var Playlist: TPlaylist): integer;

    function ImportFromStrings(const Strings: TStrings;
      var Playlist: TPlaylist): integer;

    function ImportFromStringArray(const Strings: Array of string;
      var Playlist: TPlaylist): integer;

    function ImportFromMediaLibrary(const Lib: TMediaLibrary;
      var Playlist: TPlaylist; const Filter: string = '';
      order: string = ''): integer;

    function ImportFromWPL(const FileName: TFileName; var Playlist: TPlaylist): integer;
    function ImportFromXSPF(const FileName: TFileName; var Playlist: TPlaylist): integer;
    function ImportFromM3U(const FileName: TFileName; var Playlist: TPlaylist): integer;
    function ImportFromPLS(const FileName: TFileName; var Playlist: TPlaylist): integer;
    function ImportFromASX(const FileName: TFileName; var Playlist: TPlaylist): integer;
    function ImportFromBSP(const FileName: TFileName; var Playlist: TPlaylist): integer;

    function ExportToJson(var Playlist: TPlaylist): TStringList;

    function LoadPlayList(const FileName: TFileName; var Playlist: TPlaylist): integer;

    procedure SaveToXSPF(const FileName: TFileName; var Playlist: TPlaylist;
      CurrentTime: integer = -1);
  public
    property SavedTime: integer read fSavedTime default 0;

  end;

implementation

uses
  DOM, XMLRead, XMLWrite, URIParser, IniFiles, fileutil, LazFileUtils,
  filesSupport, CustomSong, LCLProc, basetag, fpjson, jsonparser, fpjsonrtti;

const
  //  GenDelims  = [':', '/', '?', '#', '[', ']', '@'];
  SubDelims = ['!', '$', '&', '''', '(', ')', '*', '+', ',', ';', '='];
  ALPHA = ['A'..'Z', 'a'..'z'];
  DIGIT = ['0'..'9'];
  Unreserved = ALPHA + DIGIT + ['-', '.', '_', '~'];
  ValidPathChars = Unreserved + SubDelims + ['@', ':', '/'];

function Escape(const s: string; const Allowed: TSysCharSet): string;
var
  i, L: integer;
  P: PChar;
begin
  L := Length(s);
  for i := 1 to Length(s) do
    if not (s[i] in Allowed) then
      Inc(L, 2);
  if L = Length(s) then
  begin
    Result := s;
    Exit;
  end;

  SetLength(Result, L);
  P := @Result[1];
  for i := 1 to Length(s) do
  begin
    if not (s[i] in Allowed) then
    begin
      P^ := '%';
      Inc(P);
      StrFmt(P, '%.2x', [Ord(s[i])]);
      Inc(P);
    end
    else
      P^ := s[i];
    Inc(P);
  end;
end;

function Unescape(const s: string): string;
var
  i, RealLength: integer;
  P: PChar;

  function HexValue(c: char): integer; inline;
  begin
    case c of
      '0'..'9': Result := Ord(c) - Ord('0');
      'A'..'F': Result := Ord(c) - (Ord('A') - 10);
      'a'..'f': Result := Ord(c) - (Ord('a') - 10);
      else
        Result := 0;
    end;
  end;

begin
  SetLength(Result, Length(s));
  i := 1;
  P := PChar(Result);  { use PChar to prevent numerous calls to UniqueString }
  RealLength := 0;
  while i <= Length(s) do
  begin
    if s[i] = '%' then
    begin
      P[RealLength] := Chr(HexValue(s[i + 1]) shl 4 or HexValue(s[i + 2]));
      Inc(i, 3);
    end
    else
    begin
      P[RealLength] := s[i];
      Inc(i);
    end;
    Inc(RealLength);
  end;
  SetLength(Result, RealLength);
end;


function TPlayListManager.ImportFromXSPF(const FileName: TFileName;
  var Playlist: TPlaylist): integer;
var
  XMLDoc: TXMLDocument;
  Root, Node: TDOMNode;
  //  list: TDOMNodeList;
  i: integer;
  fName, RealName: string;
  IntValue: integer;

  function FindNode(x: TDOMNode; Name: string): TDOMNode;
  var
    j: integer;
  begin
    Result := nil;
    for j := 0 to x.ChildNodes.Count - 1 do
    begin
      if UpperCase(x.ChildNodes.Item[j].NodeName) = UpperCase(Name) then
      begin
        Result := x.ChildNodes.Item[j];
      end;
    end;
  end;

begin
  Result := 0;
  if Playlist = nil then
    exit;

  ReadXMLFile(XMLDoc, FileName);
  if XMLDoc = nil then
    exit;
  ;

  Root := FindNode(XMLDoc.DocumentElement, 'tracklist');
  for i := 0 to Root.ChildNodes.Count - 1 do
  begin
    Node := FindNode(Root.ChildNodes.Item[i], 'location');
    fname := Node.FirstChild.NodeValue;
    if Length(fname) > MAX_PATH then
      Continue;
    URIToFilename(Unescape(fname), RealName);
    Playlist.EnqueueFile(RealName);
  end;

  Result := i;

  Root := FindNode(XMLDoc.DocumentElement, 'extension');
  for i := 0 to Root.ChildNodes.Count - 1 do
  begin
    Node := FindNode(Root, 'currentItem');
    if node <> nil then
    begin
      fname := Node.FirstChild.NodeValue;
      IntValue := StrToIntDef(fName, -1);
      Playlist.ItemIndex := IntValue;
    end;

    Node := FindNode(Root, 'currentTime');
    if node <> nil then
    begin
      fname := Node.FirstChild.NodeValue;
      IntValue := StrToIntDef(fName, -1) div 1000;
      fSavedTime := IntValue;
    end;

  end;

  XMLDoc.Free;
end;

function TPlayListManager.ImportFromWPL(const FileName: TFileName;
  var Playlist: TPlaylist): integer;
var
  XMLDoc: TXMLDocument;
  Root: TDOMNode;
  i: integer;
  fName: string;

  function FindNode(x: TDOMNode; Name: string): TDOMNode;
  var
    j: integer;
  begin
    Result := nil;
    for j := 0 to x.ChildNodes.Count - 1 do
    begin
      if UpperCase(x.ChildNodes.Item[j].NodeName) = UpperCase(Name) then
      begin
        Result := x.ChildNodes.Item[j];
      end;
    end;
  end;

begin
  Result := 0;
  if Playlist = nil then
    exit;

  ReadXMLFile(XMLDoc, FileName);
  Root := FindNode(XMLDoc.DocumentElement, 'body');
  Root := FindNode(root, 'seq');

  for i := 0 to Root.ChildNodes.Count - 1 do
  begin
    if Root.ChildNodes.Item[i].NodeName <> 'media' then
      Continue;
    fname := Root.ChildNodes.Item[i].Attributes.GetNamedItem('src').NodeValue;
    if Length(fname) > MAX_PATH then
      Continue;
    Inc(Result);
    URIToFilename(Unescape(fname), fname);
    Playlist.EnqueueFile(fName);
  end;

  XMLDoc.Free;
end;


function TPlayListManager.ImportFromASX(const FileName: TFileName;
  var Playlist: TPlaylist): integer;
var
  XMLDoc: TXMLDocument;
  Root, Node: TDOMNode;
  i: integer;
  fName: string;

  function FindNode(x: TDOMNode; Name: string): TDOMNode;
  var
    j: integer;
  begin
    Result := nil;
    for j := 0 to x.ChildNodes.Count - 1 do
    begin
      if UpperCase(x.ChildNodes.Item[j].NodeName) = UpperCase(Name) then
      begin
        Result := x.ChildNodes.Item[j];
      end;
    end;
  end;

begin
  Result := 0;
  if Playlist = nil then
    exit;

  ReadXMLFile(XMLDoc, FileName);
  Root := XMLDoc.DocumentElement;

  for i := 0 to Root.ChildNodes.Count - 1 do
  begin
    Node := FindNode(Root.ChildNodes.Item[i], 'ref');
    fname := Node.Attributes.GetNamedItem('href').NodeValue;
    if Length(fname) > MAX_PATH then
      Continue;
    URIToFilename(Unescape(fname), fname);
    Playlist.EnqueueFile(fName);
  end;

  Result := i;

  XMLDoc.Free;
end;


function TPlayListManager.ProcessPath(FileName: string; CurPlayList: string): string;
begin
  FileName := Trim(FileName);
  if not ((Copy(UpperCase(FileName), 1, 7) = 'HTTP://') or
    (Copy(UpperCase(FileName), 1, 6) = 'FTP://')) then
  begin
    if CurPlaylist[Length(CurPlaylist)] <> PathDelim then
      CurPlaylist := CurPlaylist + PathDelim; // changed the next IF to protect UNCs  KWL
    if (Copy(FileName, 1, 1) = PathDelim) and (Copy(FileName, 2, 1) <> PathDelim) then
      FileName := Copy(CurPlaylist, 1, 2) + FileName
    else
    if Copy(FileName, 2, 2) <> ':\' then
      FileName := CurPlaylist + FileName;
  end;
  Result := FileName;
end;

function TPlayListManager.ImportFromM3U(const FileName: TFileName;
  var Playlist: TPlaylist): integer;
var
  f: textfile;
  s: string;
  p: string;
begin
  Playlist.Clear;
  p := ExtractFilePath(FileName);
  assignfile(f, FileName);
  reset(f);
  Result := 0;
  while EOF(f) <> True do
  begin
    readln(f, s);
    s := trim(s);
    if uppercase(copy(s, 0, 7)) = '#EXTINF' then
    begin
      readln(f, s);
      Inc(Result);
      Playlist.EnqueueFile(ProcessPath(s, p));
    end
    else
    begin
      if uppercase(copy(s, 0, 7)) <> '#EXTM3U' then
      begin
        Inc(Result);
        Playlist.EnqueueFile(ProcessPath(s, p));
      end;
    end;
  end;
  closefile(f);
end;

function TPlayListManager.ImportFromBSP(const FileName: TFileName;
  var Playlist: TPlaylist): integer;
var
  f: TextFile;
  s: string;
  p: string;
begin
  Playlist.Clear;
  p := ExtractFilePath(FileName);
  assignfile(f, FileName);
  reset(f);
  Result := 0;
  while not EOF(f) do
  begin
    Inc(Result);
    readln(f, s);
    s := trim(s);
    Playlist.EnqueueFile(ProcessPath(s, p));
  end;
  closefile(f);
end;

function TPlayListManager.ExportToJson(var Playlist: TPlaylist): TStringList;
var
  FObj: TJSONObject;
  FArr: TJSONArray;
  FSongObj: TJSONObject;
  FSong: TCustomSong;
  FTags: TCommonTags;
  i: integer;

begin
  Result := TStringList.Create;
  Fobj:= TJSONObject.Create;
  Fobj.Add('count',Playlist.Count);
  Fobj.Add('current',Playlist.ItemIndex);
  Farr := TJSONArray.Create;
  for i := 0 to Playlist.Count -1 do
    begin
      FsongObj:= TJSONObject.Create;
      FSong := TCustomSong(Playlist.Items[i]);
      FTags := FSong.Tags;
      FSongObj.Add('id',i+1);
      FSongObj.Add('Title',FTags.Title);
      FSongObj.Add('Album',FTags.Album);
      FSongObj.Add('AlbumArtist',FTags.AlbumArtist);
      FSongObj.Add('Artist',FTags.Artist);
      FSongObj.Add('Comment',FTags.Comment);
      FSongObj.Add('Duration',FTags.Duration);
      FSongObj.Add('Genre',FTags.Genre);
      FSongObj.Add('TrackString',FTags.TrackString);
      FSongObj.Add('Year',FTags.Year);
      FArr.Add(FSongObj);
    end;
  FObj.Add('items',FArr);

  Result.Text :=FObj.AsJSON;

end;


function TPlayListManager.ImportFromPLS(const FileName: TFileName;
  var Playlist: TPlaylist): integer;
var
  s: string;
  i: integer;
  p: string;
  ini: TMemIniFile;
  cnt: integer;
begin
  Playlist.Clear;
  p := ExtractFilePath(FileName);
  ini := TMemIniFile.Create(FileName);
  try
    cnt := ini.ReadInteger('playlist', 'NumberOfEntries', 0);
    for i := 1 to cnt do
    begin
      s := ini.ReadString('playlist', 'File' + IntToStr(i), '');
      if s <> '' then
        Playlist.EnqueueFile(ProcessPath(s, p));
    end;
    Result := cnt;
  finally
    ini.Free;
  end;
end;


function TPlayListManager.LoadPlayList(const FileName: TFileName;
  var Playlist: TPlaylist): integer;
var
  plType: TPlayListType;
begin
  plType := GetPlayListType(FileName);
  case plType of
    pltXSPF: ImportFromXSPF(FileName, Playlist);
    pltM3U: ImportFromM3U(FileName, Playlist);
    pltPLS: ImportFromPLS(FileName, Playlist);
    pltASX: ImportFromASX(FileName, Playlist);
    pltBSPL: ImportFromBSP(FileName, Playlist);
    pltWPL: ImportFromWPL(FileName, Playlist);
  end;
  Result := Playlist.Count;

end;

procedure TPlayListManager.SaveToXSPF(const FileName: TFileName;
  var Playlist: TPlaylist; CurrentTime: integer = -1);
var
  XMLDoc: TXMLDocument;
  Root, Node: TDOMelement;
  TrackNode, PropNode: TDOMelement;
  i: integer;
  Str: TFileStreamUTF8;

begin
  XMLDoc := TXMLDocument.Create;
  Root := XMLDoc.CreateElement('playlist');
  root.SetAttribute('version', '1');
  root.SetAttribute('xmlns', 'http://xspf.org/ns/0/');
  XMLDoc.AppendChild(root);
  Node := XMLDoc.CreateElement('trackList');
  root.AppendChild(Node);

  TrackNode := XMLDoc.CreateElement('extension');
  TrackNode.SetAttribute('application', XSPF_VERSION);
  Root.AppendChild(TrackNode);
  if CurrentTime <> -1 then
  begin
    PropNode := XMLDoc.CreateElement('currentItem');
    propnode.TextContent := IntToStr(Playlist.ItemIndex);
    TrackNode.AppendChild(PropNode);
    PropNode := XMLDoc.CreateElement('currentTime');
    propnode.TextContent := IntToStr(CurrentTime);
    TrackNode.AppendChild(PropNode);
  end;


  for i := 0 to Playlist.Count - 1 do
  begin
    TrackNode := XMLDoc.CreateElement('track');
    node.AppendChild(TrackNode);

    PropNode := XMLDoc.CreateElement('location');
    propnode.TextContent := FilenameToURI(Escape(Playlist[i].FullName, ValidPathChars));
    TrackNode.AppendChild(PropNode);

    PropNode := XMLDoc.CreateElement('title');
    propnode.TextContent := Playlist[i].Tags.Title;
    TrackNode.AppendChild(PropNode);

  end;

  Str := TFileStreamUTF8.Create(FileName, fmCreate);
  WriteXMLFile(XMLDoc, Str);
  Str.Free;
  XMLDoc.Free;

end;

function TPlayListManager.GetPlayListType(FileName: string): TPlayListType;
var
  f: textfile;
  s, fe: string;
begin
  Result := pltUnknown;
  if FileExistsUTF8(FileName) then
  begin
    assignfile(f, FileName);
    reset(f);
    repeat
      readln(f, s);
    until (trim(s) <> '') or EOF(f);

    if trim(lowercase(s)) = '<?xml version="1.0"?>' then
      Result := pltXSPF
    else
    if trim(s) = '#EXTM3U' then
      Result := pltM3U
    else if copy(s, 0, 4) = 'BSPL' then
      Result := pltBSPL
    else if lowercase(copy(s, 0, 5)) = '<?wpl' then
      Result := pltWPL
    else if lowercase(copy(s, 0, 4)) = '<ASX' then
      Result := pltASX
    else
    begin
      fe := ansilowercase(extractfileext(FileName));
      if fe = '.xspf' then
        Result := pltXSPF
      else if fe = '.m3u' then
        Result := pltM3U
      else if fe = '.bsl' then
        Result := pltBSPL
      else if fe = '.wpl' then
        Result := pltWPL
      else if fe = '.lap' then
        Result := pltPLS
      else if fe = '.pls' then
        Result := pltPLS
      else if fe = '.asx' then
        Result := pltASX
      else if fe = '.wax' then
        Result := pltASX;

    end;
  end;
  closefile(f);

end;

function TPlayListManager.ImportFromDirectory(const Directory: string;
  Recursive: boolean; var Playlist: TPlaylist): integer;
var
  list: TStringList;
begin
  PlayList.Clear;
  List := TStringList.Create;
  BuildFileList(IncludeTrailingPathDelimiter(Directory) + AudioTag.SupportedExtension,
    faAnyFile, List, Recursive);

  Result := ImportFromStrings(list, Playlist);
  list.Free;
end;

function TPlayListManager.ImportFromStrings(const Strings: TStrings;
  var Playlist: TPlaylist): integer;
var
  i: integer;
begin
  PlayList.BeginUpdate;
  for I := 0 to Strings.Count - 1 do
    PlayList.EnqueueFile(Strings[i]);
  PlayList.EndUpdate;
  Result := Strings.Count;
end;

function TPlayListManager.ImportFromStringArray(const Strings: array of string;
  var Playlist: TPlaylist): integer;
var
  i: integer;
  cnt: integer;
begin
  cnt := 0;
  PlayList.BeginUpdate;
  for I := 0 to high(Strings) do
     begin
      PlayList.EnqueueFile(Strings[i]);
      inc(cnt);
     end;
  PlayList.EndUpdate;
  Result := cnt;
end;


function TPlayListManager.ImportFromMediaLibrary(const Lib: TMediaLibrary;
  var Playlist: TPlaylist; const Filter: string = '';
  order: string = ''): integer;
var
  Tags: TCommonTags;
begin
  Result := 0;
  lib.ReadBegin(Filter, Order);
  while not Lib.ReadComplete do
  begin
    Inc(Result);
    Tags := Lib.ReadItem;
    Playlist.EnqueueFile(tags.FileName);
    lib.NextItem;
  end;
  lib.ReadEnd;

end;


end.

