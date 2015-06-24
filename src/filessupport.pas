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
unit FilesSupport;

{$mode objfpc}{$H+}

interface

uses Classes;
type
 TFileInfo = record
   Size: int64;
   CreationDate: TDatetime;
   ModifyDate: TDateTime;
 end;

 TFileInfoObject = class
   public info: TFileInfo;
 end;


Function GetFileInfo(FileName: String): TFileInfo;
function BuildFileList(const Path: string; const Attr: integer; const List: TStrings; Recurring: boolean): boolean;
function BuildFolderList(const Path: string; const List: TStrings; Recurring: boolean): boolean;
Function UpperDirectory(const dir:string):string;
function GetConfigDir: string;
function strByteSize(Value: Int64): String;
function EncodeSafeFileName(const s: string): string;

implementation

uses
  SysUtils, fileutil;

const
{ common computer sizes }
KBYTE = Sizeof(Byte) shl 10;
MBYTE = KBYTE shl 10;
GBYTE = MBYTE shl 10;

BlackListedChar = ['<','>',':','"','/','\','|','?','*','%'];

function EncodeSafeFileName(const s: string): string;
var
i, L: integer;
P: PChar;
begin
  L := Length(s);
  for i := 1 to Length(s) do
    if (s[i] in BlackListedChar) then
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
    if (s[i] in BlackListedChar) then
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
  if L > (MAX_PATH -4) then
    Result := Copy(Result,1, MAX_PATH -4);
end;

function DecodeSafeFileName(const s: string): string;
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

function strByteSize(Value: int64): String;

 function FltToStr(F: Extended): String;
 begin
   Result:=FloatToStrF(F,ffNumber,6,2);
 end;

begin
 if Value > GBYTE then
    Result:=FltTostr(Value / GBYTE)+' GB'
 else
   if Value > MBYTE then
     Result:=FltToStr(Value / MBYTE)+' MB'
   else
     if Value > KBYTE then
        Result:=FltTostr(Value / KBYTE)+' KB'
     else
        Result:=FltTostr(Value) +' Bytes';
end;

// Derived from "Like" by Michael Winter
function StrMatches(const Substr, S: string; const Index: SizeInt = 1): boolean;
var
  StringPtr:  PChar;
  PatternPtr: PChar;
  StringRes:  PChar;
  PatternRes: PChar;
begin
  Result := SubStr = '*';

  if Result or (S = '') then
    Exit;

  StringPtr  := PChar(@S[Index]);
  PatternPtr := PChar(SubStr);
  StringRes  := nil;
  PatternRes := nil;

  repeat
    repeat
      case PatternPtr^ of
        #0:
          begin
          Result := StringPtr^ = #0;
          if Result or (StringRes = nil) or (PatternRes = nil) then
            Exit;

          StringPtr  := StringRes;
          PatternPtr := PatternRes;
          Break;
          end;
        '*':
          begin
          Inc(PatternPtr);
          PatternRes := PatternPtr;
          Break;
          end;
        '?':
          begin
          if StringPtr^ = #0 then
            Exit;
          Inc(StringPtr);
          Inc(PatternPtr);
          end;
        else
          begin
          if StringPtr^ = #0 then
            Exit;
          if StringPtr^ <> PatternPtr^ then
            begin
            if (StringRes = nil) or (PatternRes = nil) then
              Exit;
            StringPtr  := StringRes;
            PatternPtr := PatternRes;
            Break;
            end
          else
            begin
            Inc(StringPtr);
            Inc(PatternPtr);
            end;
          end;
        end;
    until False;

    repeat
      case PatternPtr^ of
        #0:
          begin
          Result := True;
          Exit;
          end;
        '*':
          begin
          Inc(PatternPtr);
          PatternRes := PatternPtr;
          end;
        '?':
          begin
          if StringPtr^ = #0 then
            Exit;
          Inc(StringPtr);
          Inc(PatternPtr);
          end;
        else
          begin
          repeat
            if StringPtr^ = #0 then
              Exit;
            if StringPtr^ = PatternPtr^ then
              Break;
            Inc(StringPtr);
          until False;
          Inc(StringPtr);
          StringRes := StringPtr;
          Inc(PatternPtr);
          Break;
          end;
        end;
    until False;
  until False;
end;


function IsFileNameMatch(FileName: string; const Mask: string; const CaseSensitive: boolean): boolean;
begin
  Result := True;
  {$IFDEF MSWINDOWS}
  if (Mask = '') or (Mask = '*') or (Mask = '*.*') then
    Exit;
  if Pos('.', FileName) = 0 then
    FileName := FileName + '.';  // file names w/o extension match '*.'
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  if (Mask = '') or (Mask = '*') then
    Exit;
  {$ENDIF UNIX}
  if CaseSensitive then
    Result := StrMatches(Mask, FileName)
  else
    Result := StrMatches(AnsiUpperCase(Mask), AnsiUpperCase(FileName));
end;

procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: boolean = True);
var
  I, L: integer;
  Left: string;
begin
  Assert(List <> nil);
  List.BeginUpdate;
    try
    List.Clear;
    L := Length(Sep);
    I := Pos(Sep, S);
    while I > 0 do
      begin
      Left := LeftStr(S, I - 1);
      if (Left <> '') or AllowEmptyString then
        List.Add(Left);
      Delete(S, 1, I + L - 1);
      I := Pos(Sep, S);
      end;
    if S <> '' then
      List.Add(S);
    finally
    List.EndUpdate;
    end;
end;

function GetFileInfo(FileName: String): TFileInfo;
begin
  Result.Size:= FileSize(FileName);
  result.CreationDate:= FileDateToDateTime(FileAgeUTF8(FileName));
end;

function BuildFileList(const Path: string; const Attr: integer; const List: TStrings; Recurring: boolean): boolean;
var
  SearchRec: TSearchRec;
  IndexMask: integer;
  MaskList:  TStringList;
  Masks, Directory: string;
  info :TFileInfoObject;
begin
  Assert(List <> nil);
  MaskList := TStringList.Create;
    try
    {* extract the Directory *}
    Directory := ExtractFileDir(Path);

    {* files can be searched in the current directory *}
    if Directory <> '' then
      begin
      Directory := IncludeTrailingPathDelimiter(Directory);
      {* extract the FileMasks portion out of Path *}
      Masks     := copy(Path, Length(Directory) + 1, Length(Path));
      end
    else
      Masks := Path;

    {* put the Masks into TStringlist *}
    StrToStrings(Masks, ';', MaskList, False);

    {* search all files in the directory *}
    Result := FindFirstUTF8(Directory + AllFilesMask, faAnyFile, SearchRec) = 0;

    List.BeginUpdate;
      try
      while Result do
        begin
        {* if the filename matches any mask then it is added to the list *}
        if Recurring and ((searchrec.Attr and faDirectory) <> 0) and (SearchRec.Name <> '.') and
          (SearchRec.Name <> '..') then
          BuildFileList(IncludeTrailingPathDelimiter(Directory + SearchRec.Name) + masks,
            Attr, list, Recurring);

        for IndexMask := 0 to MaskList.Count - 1 do
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
            ((SearchRec.Attr and Attr) = (SearchRec.Attr and faAnyFile)) and
            IsFileNameMatch(SearchRec.Name, MaskList.Strings[IndexMask], False) then
            begin
              info := TFileInfoObject.Create;
              info.info.Size:= SearchRec.Size;
              info.info.ModifyDate:= FileDateToDateTime(SearchRec.Time);
              List.AddObject(SysToUTF8(Directory + SearchRec.Name), info);
            Break;
            end;

        case FindNextUTF8(SearchRec) of
          0: ;
          2: //ERROR_NO_MORE_FILES:
            Break;
          else
            Result := False;
          end;
        end;
      finally
      FindCloseUTF8(SearchRec);
      List.EndUpdate;
      end;
    finally
    MaskList.Free;
    end;
end;

function BuildFolderList(const Path: string; const List: TStrings; Recurring: boolean): boolean;
var
  SearchRec: TSearchRec;
  Directory: string;
begin
  Assert(List <> nil);
  {* extract the Directory *}
  Directory := ExtractFileDir(Path);

  {* files can be searched in the current directory *}
  if Directory <> '' then
    begin
       Directory := IncludeTrailingPathDelimiter(Directory);
    end;

  {* search all files in the directory *}
  Result := FindFirstUTF8(Directory + AllFilesMask, faDirectory, SearchRec) = 0;

  List.BeginUpdate;
  try
    while Result do
      begin
        if (SearchRec.Name <> '.') and
           (SearchRec.Name <> '..') and
           ((SearchRec.Attr and faDirectory) = faDirectory)  then
          begin
          List.Add(Directory + SearchRec.Name);
          end;

      case FindNextUTF8(SearchRec) of
        0: ;
        2: //ERROR_NO_MORE_FILES:
          Break;
        else
          Result := False;
        end;
      end;
  finally
    FindCloseUTF8(SearchRec);
    List.EndUpdate;
  end;
end;


function GetConfigDir: string;
var
  Path: string;
begin
  Path := GetAppConfigDirUTF8(False);
  ForceDirectoriesUTF8(Path);
  Result := IncludeTrailingPathDelimiter(Path);

end;

function UpperDirectory(const dir: string): string;
var
  DirStart:Integer;
  lDir:String;
begin
   ldir:=ChompPathDelim(Dir);
   DirStart:=Length(ldir);
   while (DirStart>1) and (Ldir[DirStart]<>PathDelim) do
         dec(DirStart);
   Result:=AppendPathDelim(Copy(dir,1,DirStart));
end;

end.
