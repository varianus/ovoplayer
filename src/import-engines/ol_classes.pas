unit OL_Classes;

{$mode objfpc}{$H+}
{$interfaces corba}
interface

uses
  Classes, SysUtils;

type
  TFrame = word;
  TOLBuffer = array[0..$ffff] of TFrame;
  POLBuffer = ^TOLBuffer;

  TFrameFormat = (ffNone, ffInt16, ffInt32, ffFloat32);

  TOLStreamFormat = record
    BitRate: integer;
    Channels: integer;
    Format: TFrameFormat;
  end;

  { IOL_Decoder }

  IOL_Decoder = interface
    function GetSongPos: int64;
    procedure SetSongPos(AValue: int64);
    function Load(LibraryName: string = ''): boolean;
    function GetStreamFormat: TOLStreamFormat;
    procedure SetStreamFormat(AValue: TOLStreamFormat);
    procedure UnLoad;
    function Initialize: boolean;
    procedure Finalize;
    function OpenFile(FileName: TfileName): boolean;
    function GetBuffer(const Frames: integer; Buffer: POLBuffer): NativeUInt;
    Property SongPos: int64 read GetSongPos write SetSongPos;
    Property StreamFormat: TOLStreamFormat read GetStreamFormat write SetStreamFormat;
    Procedure Free;
    procedure Close;
  end;

  { IOL_Renderer }

  IOL_Renderer = interface
    function GetStreamFormat: TOLStreamFormat;
    function Load(LibraryName: string = ''): boolean;
    procedure SetStreamFormat(AValue: TOLStreamFormat);
    procedure UnLoad;
    function Initialize: boolean;
    procedure Finalize;
    procedure Start;
    procedure Stop;
    procedure Write(const Frames: integer; Buffer: POLBuffer);
    Property StreamFormat: TOLStreamFormat read GetStreamFormat write SetStreamFormat;
    Procedure Free;
  end;

  IOL_Filter = interface
    procedure Apply(buffer: POLBuffer);
    Procedure Free;
  end;



function SupportedExtension: string;
procedure RegisterDecoder(const Extensions: string; const Decoder: TClass);
function IdentifyDecoder(FileName: string): TClass;

implementation

uses StrUtils, OL_DecoderDummy;

{ TOL_Player }


type
  RDecoder = record
    Extensions: string;
    Decoder: TClass;
  end;

var
  ADecoderList: array of RDecoder;

function SupportedExtension: string;
var
  i: integer;
begin
  Result := '';
  for i := Low(ADecoderList) to High(ADecoderList) do
    Result := Result + ADecoderList[i].Extensions;

end;

procedure RegisterDecoder(const Extensions: string; const Decoder: TClass);
var
  tr: RDecoder;
begin
  tr.Extensions := Extensions;
  tr.Decoder := Decoder;
  SetLength(ADecoderList, Length(ADecoderList) + 1);
  ADecoderList[High(ADecoderList)] := tr;
end;

function IdentifyDecoder(FileName: string): TClass;
var
  ext: string;
  i: integer;
begin
  Result := TOL_DecoderDummy;
  if AnsiStartsStr('HTTP:\\', UpperCase(FileName)) or
    AnsiStartsStr('MMS:\\', UpperCase(FileName)) then
    exit;

  ext := lowercase(ExtractFileExt(Filename));

  for i := Low(ADecoderList) to High(ADecoderList) do
    if Pos(ext, ADecoderList[i].Extensions) > 0 then
    begin
      Result := ADecoderList[i].Decoder;
      exit;
    end;
end;

initialization
  RegisterDecoder('', TOL_DecoderDummy);

end.


