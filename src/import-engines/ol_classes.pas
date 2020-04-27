unit OL_Classes;

{$mode objfpc}{$H+}
{$interfaces corba}
interface

uses
  Classes, SysUtils;

type
  TFrame = smallint;
  TOLBuffer = array of TFrame;
  POLBuffer = ^TOLBuffer;

  TFrameFormat = (ffNone, ffInt16, ffInt32, ffFloat32);

  TOLStreamFormat = record
    BitRate: integer;
    Channels: integer;
    Format: TFrameFormat;
  end;

  TOLVersion = record
    LibraryName: string;
    LibraryVersion: string;
  end;

  { IOL_Decoder }
  IOL_Module = interface
    function Load(LibraryName: string = ''): boolean;
    function Initialize: boolean;
    procedure Finalize;
    procedure UnLoad;
    Procedure Free;
    Function GetVersion: TOLVersion;
    Function Name: string;
  end;

  IOL_Decoder = interface(IOL_Module)
  //property get/set
    function GetSongPos: int64;
    procedure SetSongPos(AValue: int64);
    function GetStreamFormat: TOLStreamFormat;
    procedure SetStreamFormat(AValue: TOLStreamFormat);
  //methods
    procedure Close;
    function OpenFile(FileName: TfileName): boolean;
    function GetBuffer(const Frames: integer; const Buffer: POLBuffer): NativeUInt;
  //property
    Property SongPos: int64 read GetSongPos write SetSongPos;
    Property StreamFormat: TOLStreamFormat read GetStreamFormat write SetStreamFormat;

  end;

  { IOL_Renderer }

  IOL_Renderer = interface(IOL_Module)
    function GetStreamFormat: TOLStreamFormat;
    procedure SetStreamFormat(AValue: TOLStreamFormat);
    procedure Start;
    procedure Stop;
    procedure Write(const Frames: integer; Buffer: POLBuffer);
    Property StreamFormat: TOLStreamFormat read GetStreamFormat write SetStreamFormat;
  end;

  IOL_Filter = interface(IOL_Module)
    procedure Apply(const Frames: integer; buffer: POLBuffer );
  end;

  { IOL_FilterVolume }

  IOL_FilterVolume = interface(IOL_Filter)
    function GetStreamFormat: TOLStreamFormat;
    function GetVolume: Integer;
    procedure SetStreamFormat(AValue: TOLStreamFormat);
    procedure SetVolume(AValue: Integer);
    Property Volume: Integer read GetVolume write SetVolume;
    Property StreamFormat: TOLStreamFormat read GetStreamFormat write SetStreamFormat;
  end;

  type
  RDecoder = record
    Extensions: string;
    Decoder: TClass;
  end;

ADecoderList = array of RDecoder;

function SupportedExtension: string;
function DecoderList: ADecoderList; inline;
procedure RegisterDecoder(const Extensions: string; const Decoder: TClass);
function IdentifyDecoder(FileName: string): TClass;

implementation

uses StrUtils, OL_DecoderDummy;

{ TOL_Player }

var
  fDecoderList: ADecoderList;

function SupportedExtension: string;
var
  i: integer;
begin
  Result := '';
  for i := Low(fDecoderList) to High(fDecoderList) do
    Result := Result + fDecoderList[i].Extensions;

end;

function DecoderList: ADecoderList;
begin
  Result := fDecoderList;
end;

procedure RegisterDecoder(const Extensions: string; const Decoder: TClass);
var
  tr: RDecoder;
begin
  tr.Extensions := Extensions;
  tr.Decoder := Decoder;
  SetLength(fDecoderList, Length(fDecoderList) + 1);
  fDecoderList[High(fDecoderList)] := tr;
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

  for i := Low(fDecoderList) to High(fDecoderList) do
    if Pos(ext, fDecoderList[i].Extensions) > 0 then
    begin
      Result := fDecoderList[i].Decoder;
      exit;
    end;
end;

initialization
  RegisterDecoder('', TOL_DecoderDummy);

end.


