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
{$I backend.inc}
unit OL_DecoderMPG123;

interface


uses
  Classes, SysUtils, dynlibs, OL_Classes, GeneralFunc;

type
  { TOL_DecoderMP123 }

  TOL_DecoderMP123 = class(iOL_Decoder)
  private
    StreamHandle: pointer;
    fLastError: integer;
    fStreamFormat: TOLStreamFormat;
    function Check(HR: integer): boolean;
 protected
    function GetSongPos: int64;
    procedure SetSongPos(AValue: int64);
    function GetStreamFormat: TOLStreamFormat;
    procedure SetStreamFormat(AValue: TOLStreamFormat);
    Function Name: string;
  public
    function Load(LibraryName: string = ''): boolean;
    Function GetVersion: TOLVersion;
    procedure UnLoad;
    function Initialize: boolean;
    procedure Finalize;
    function OpenFile(FileName: TfileName): boolean;
    procedure Close;
    function GetBuffer(const Frames: integer; Buffer: POLBuffer): NativeUInt;
  end;

implementation

uses UOS_mpg123;


{ TOL_DecoderMP123 }

function TOL_DecoderMP123.Check(HR: integer): boolean;
begin
  Result := HR = MPG123_OK;
  if not Result then
    begin
    fLastError := HR;
  //  WriteLn('Err:', mpg123_strerror(StreamHandle));
    end;
end;
function TOL_DecoderMP123.GetSongPos: int64;
begin
 Result := trunc(mpg123_tell(StreamHandle) / (fStreamFormat.BitRate / 1000));
end;
procedure TOL_DecoderMP123.SetSongPos(AValue: int64);
begin
  Check(mpg123_seek(StreamHandle,trunc( AValue * (fStreamFormat.BitRate / 1000)), 0{SEEK_SET}));
//  WriteLn('Seek POS', AValue, 'Last Error:', fLastError);
end;

function TOL_DecoderMP123.GetStreamFormat: TOLStreamFormat;
begin
  Result := fStreamFormat;
end;

procedure TOL_DecoderMP123.SetStreamFormat(AValue: TOLStreamFormat);
begin
  fStreamFormat := AValue;
end;

function TOL_DecoderMP123.Name: string;
begin
  Result := 'MPG123';
end;


function TOL_DecoderMP123.Load(LibraryName: string): boolean;
begin
  UOS_mpg123.mp_load(LibraryName);
end;

function TOL_DecoderMP123.GetVersion: TOLVersion;
var
  BaseAddr:pointer;
  ModuleName:string;
begin
  If mp_IsLoaded then
    begin
      GetModuleByAddr(mpg123_new, BaseAddr, ModuleName);
      Result.LibraryName := ModuleName;
      Result.LibraryVersion := '';
    end;
end;

procedure TOL_DecoderMP123.UnLoad;
begin

  UOS_mpg123.mp_unload();
end;

function TOL_DecoderMP123.Initialize: boolean;
begin
  mpg123_init;
end;

procedure TOL_DecoderMP123.Finalize;
begin
  mpg123_exit;
end;

function TOL_DecoderMP123.OpenFile(FileName: TfileName): boolean;
begin

  Result := False;
  StreamHandle := mpg123_new(nil, fLastError);
  if not Check(fLastError) then
    exit;

  if not Check(mpg123_open(StreamHandle, PChar(FileName))) then
    exit;

  if not Check(mpg123_getformat(StreamHandle, fStreamFormat.BitRate, fStreamFormat.Channels, Longint(fStreamFormat.Format))) then
    exit;

  if not Check(mpg123_format_none(StreamHandle)) then
    exit;

  if not Check(mpg123_format(StreamHandle, fStreamFormat.BitRate, fStreamFormat.Channels, Longint(fStreamFormat.Format))) then
    exit;

  if not Check(mpg123_seek_frame(StreamHandle, 0, 0{SEEK_SET})) then
    exit;

  Result := True;
end;

procedure TOL_DecoderMP123.Close;
begin
  mpg123_close(StreamHandle);
  mpg123_delete(StreamHandle);
end;

function TOL_DecoderMP123.GetBuffer(const Frames: integer; Buffer: POLBuffer): NativeUInt;
begin
  if Check(mpg123_read(StreamHandle, Buffer, Frames * SizeOf(TFrame) * fStreamFormat.Channels, Result)) then
    Result := Result div (SizeOf(TFrame) * fStreamFormat.Channels)
  else
    Result := 0;
//  WriteLn('Read want:',Frames,'  result:',result, 'Last Error:', fLastError);
end;

initialization
  RegisterDecoder('*.mp3;', TOL_DecoderMP123);
end.
