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
{$I codegen.inc}
unit netsupport;

interface

uses
  Classes, SysUtils, base64, BaseTypes, basetag,
  netprotocol;

type

  TSizeMode = (smByte, smUTF8Char);

  RConnectionCfg = record
    SizeMode :TSizeMode;
  end;

function EncodeSize(Size:Integer):string;
function DecodeSize(Size:string):integer;

function EncodeImageSize(Width, Height: integer):string;
Procedure DecodeImageSize(Size:string; Out Width, Height: integer);

Function EncodeString(S:String; ConnectionCfg: RConnectionCfg): string;
Function EncodeStream(inStream:TStream):string;

function BuildCommand(Category: string; Command: string; Param:string=''; IPCSeparator:boolean=false):string;
Function SplitCommand(ACommand:string): RExternalCommand;

Function EncodeMetaData(Tags:TCommonTags; ConnectionCfg: RConnectionCfg): String;
Function DecodeMetaData(var StringTags:string; ConnectionCfg: RConnectionCfg): TCommonTags;

Function ExtractField(var StringTags:String; ConnectionCfg: RConnectionCfg):String;


implementation
uses strutils, LazUTF8;

function EncodeSize(Size:Integer):string;
var
  s : RawByteString = '';
begin
  SetLength(s,3);
  s[1] := AnsiChar((Size and $ff0000 ) shr 16);
  s[2] := AnsiChar((Size and $00ff00) shr 8);
  s[3] := AnsiChar((Size and $0000ff));
  Result := base64.EncodeStringBase64(s);
end;

function DecodeSize(Size: String): integer;
var
  s : RawByteString;
begin
  try
    s:= DecodeStringBase64(Size, True);

    Result := (byte(s[1]) shl 16) or
              (byte(s[2]) shl 8) or
              (byte(s[3]));
  except
    result := -1;
  end;
end;

function EncodeImageSize(Width, Height: integer): string;
begin
  Result := format('%dx%d', [Width, Height]);
end;

Procedure DecodeImageSize(Size: string; out Width, Height: integer);
var
  pX: integer;
begin
  Width:=-1;
  Height:=-1;
  if Length(Size) = 0 then
    exit;

  try
    pX:=pos('x', Size);
    if pX = 0 then
      exit;
    Width:=StrToInt(Copy(Size,1,pX-1));
    Height:=StrToInt(Copy(Size,pX+1, Length(Size) -pX));
  Except
  end;
end;

Function EncodeString(S:String; ConnectionCfg: RConnectionCfg): string;
begin
  case ConnectionCfg.SizeMode of
    smUTF8Char :   Result := EncodeSize(UTF8Length(s))+s;
    smByte :   Result := EncodeSize(Length(s))+s;
  end;
end;

function BuildCommand(Category: string; Command: string; Param: string;
  IPCSeparator: boolean): string;
begin
  Result := Category +
            CATEGORY_COMMAND_SEPARATOR+
            Command;

  if Param <> '' then
     Result := Result +
               CATEGORY_PARAM_SEPARATOR +
               Param;
  if IPCSeparator then
     Result := Result + IPC_SEPARATOR;
end;

function SplitCommand(ACommand: string): RExternalCommand;
var
  pColon, pEqual: integer;
  cmdlength: integer;
begin
  Result.Category:=EmptyStr;
  Result.Param:=EmptyStr;
  cmdlength := Length(ACommand);

  if (cmdlength >0 ) and (ACommand[cmdlength] = IPC_SEPARATOR) then
     dec(cmdlength);

  pColon:= pos(CATEGORY_COMMAND_SEPARATOR,ACommand);
  //Size of category is 3 char
  if pColon < 4 then
    Result.Command:=ACommand
  else
    begin
      Result.Category:=copy(ACommand, 1,pColon-1);
      pEqual:= PosEx(CATEGORY_PARAM_SEPARATOR,ACommand,pColon+1);
      if pEqual < 1 then
        Result.Command:=Copy(ACommand,pColon+1, cmdlength)
      else
        begin
           Result.Command:=copy(ACommand, pColon+1,pEqual -pColon -1);
           Result.Param:= copy(ACommand, pEqual+1, cmdlength);
        end;
    end;
end;

Function ExtractField(var StringTags:String; ConnectionCfg: RConnectionCfg):String;
var
  size: integer;
begin
  size:= DecodeSize(Copy(StringTags,1,4));

  case ConnectionCfg.SizeMode of
    smByte:
      begin
        Result := copy(StringTags, 5, size);
        inc(size,4);
        Delete(StringTags, 1, size);
      end;
    smUTF8Char:
      begin
        Result := UTF8copy(StringTags, 5, size);
        inc(size,4);
        UTF8Delete(StringTags, 1, size);
      end;
  end;

end;

function EncodeMetaData(Tags: TCommonTags; ConnectionCfg: RConnectionCfg): String;
begin
  result := EncodeString(   // is this encoding really needed ??? Maybe only for check
                EncodeString(inttostr(Tags.Index),ConnectionCfg)+
                EncodeString(inttostr(Tags.id),ConnectionCfg)+
                EncodeString(Tags.FileName,ConnectionCfg)+
                EncodeString(Tags.Album,ConnectionCfg)+
                EncodeString(Tags.AlbumArtist,ConnectionCfg)+
                EncodeString(Tags.Artist,ConnectionCfg)+
                EncodeString(Tags.Comment,ConnectionCfg)+
                EncodeString(Inttostr(Tags.Duration),ConnectionCfg)+
                EncodeString(Tags.Genre,ConnectionCfg)+
                EncodeString(Tags.Title,ConnectionCfg)+
                EncodeString(Tags.TrackString,ConnectionCfg)+
                EncodeString(Tags.Year,ConnectionCfg),
                ConnectionCfg
           );
end;

function DecodeMetaData(var StringTags: string; ConnectionCfg: RConnectionCfg): TCommonTags;
begin
  Delete(StringTags, 1, 4);
  Result.Index       := StrToInt64Def(ExtractField(StringTags, ConnectionCfg),-1);
  Result.ID          := StrToInt64Def(ExtractField(StringTags, ConnectionCfg),0);
  Result.FileName    := ExtractField(StringTags, ConnectionCfg);
  Result.Album       := ExtractField(StringTags, ConnectionCfg);
  Result.AlbumArtist := ExtractField(StringTags, ConnectionCfg);
  Result.Artist      := ExtractField(StringTags, ConnectionCfg);
  Result.Comment     := ExtractField(StringTags, ConnectionCfg);
  Result.Duration    := StrToInt64Def(ExtractField(StringTags, ConnectionCfg),0);
  Result.Genre       := ExtractField(StringTags, ConnectionCfg);
  Result.Title       := ExtractField(StringTags, ConnectionCfg);
  Result.TrackString := ExtractField(StringTags, ConnectionCfg);
  Result.Year        := ExtractField(StringTags, ConnectionCfg);

end;

Function EncodeStream(inStream:TStream):string;
var
  Encoder: TBase64EncodingStream;
  Outstream : TStringStream;
begin
  Result := EmptyStr;

  if not Assigned(inStream) then
    exit;

  if inStream.Size = 0 then
    exit;

  Outstream:=TStringStream.Create('');
  try
    Encoder := TBase64EncodingStream.Create(Outstream);
    try
      inStream.Position:=0;
      Encoder.CopyFrom(inStream, inStream.Size);
    finally
      Encoder.Free;
      end;
    Result:=Outstream.DataString;
  finally
    Outstream.free;
  end;

end;




end.



