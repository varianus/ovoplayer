unit OL_DecoderOpus;

{$mode objfpc}{$H+}
{$interfaces corba}
interface


uses
  Classes, SysUtils, ctypes, OL_Classes, uos_OpusFile, uos_opus, GeneralFunc;

type
  { TOL_DecoderOpus }

  TOL_DecoderOpus = class(iOL_Decoder)
  private
    StreamHandle: pointer;
    fStreamFormat : TOLStreamFormat;
    fLastError: integer;
    function Check(HR: integer): boolean; inline;
    function GetVersion: TOLVersion;
  protected
    function GetStreamFormat: TOLStreamFormat;
    procedure SetStreamFormat(AValue: TOLStreamFormat);
    function GetSongPos: int64;
    procedure SetSongPos(AValue: int64);
    Function Name: string;
  public
    function Load(LibraryName: string = ''): boolean;
    procedure UnLoad;
    function Initialize: boolean;
    procedure Finalize;
    function OpenFile(FileName: TfileName): boolean;
    procedure Close;
    function GetBuffer(const Frames: integer; const Buffer: POLBuffer): NativeUInt;
  end;

implementation

{ TOL_DecoderOpus }

function TOL_DecoderOpus.Check(HR: integer): boolean;
begin
  Result := HR = OPUS_OK;
  if not Result then
    fLastError := HR;
end;

function TOL_DecoderOpus.GetStreamFormat: TOLStreamFormat;
begin
  Result := fStreamFormat;
end;

procedure TOL_DecoderOpus.SetStreamFormat(AValue: TOLStreamFormat);
begin
  fStreamFormat := AValue;
end;

function TOL_DecoderOpus.GetSongPos: int64;
begin
  if Assigned( StreamHandle) then
  Result := trunc(op_pcm_tell(StreamHandle) / (fStreamFormat.BitRate / 1000));
end;

procedure TOL_DecoderOpus.SetSongPos(AValue: int64);
begin
  op_pcm_seek(StreamHandle, trunc(AValue * (fStreamFormat.BitRate / 1000)));
end;

function TOL_DecoderOpus.Name: string;
begin
  Result := 'Opus';
end;

function TOL_DecoderOpus.Load(LibraryName: string): boolean;
begin
 // Result := UOS_Opus.op_Load(LibraryName);
  Result := uos_OpusFile.of_Load(LibraryName);
end;

procedure TOL_DecoderOpus.UnLoad;
begin


  uos_OpusFile.of_Unload();

end;

function TOL_DecoderOpus.Initialize: boolean;
begin
  Result := True;
end;

procedure TOL_DecoderOpus.Finalize;
begin

end;

function TOL_DecoderOpus.OpenFile(FileName: TfileName): boolean;
begin

  Result := False;
  StreamHandle := op_test_file(PChar(FileName),fLastError);
  if fLastError <> 0 then
    exit;

  if not Check(op_test_open(StreamHandle)) then
    exit;
  if op_link_count(StreamHandle) <> 1 then
    exit;

  fStreamFormat.Channels := 2;
  fStreamFormat.BitRate := 48000;
  Result := True;
end;

procedure TOL_DecoderOpus.Close;
begin
  op_free(StreamHandle);
end;

function TOL_DecoderOpus.GetBuffer(const Frames: integer; const Buffer: POLBuffer): NativeUInt;
begin
  Result := op_read_stereo(StreamHandle, Pointer(Buffer), Frames div fStreamFormat.Channels) ;
  //  Result := Result * fStreamFormat.Channels;
end;


function TOL_DecoderOpus.GetVersion: TOLVersion;
var
  BaseAddr:pointer;
  ModuleName:string;
begin
  If op_IsLoaded and of_IsLoaded then
    begin
      GetModuleByAddr(op_read_stereo, BaseAddr, ModuleName);
      Result.LibraryName := ModuleName;
      Result.LibraryVersion :='1';
    end;
end;

initialization
  RegisterDecoder('*.opus;', TOL_DecoderOpus);
end.
