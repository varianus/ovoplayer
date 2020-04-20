unit OL_DecoderSndFile;

{$mode objfpc}{$H+}
{$interfaces corba}
interface


uses
  Classes, SysUtils, ctypes, OL_Classes, UOS_libsndfile, GeneralFunc;

type
  { TOL_DecoderSndFile }

  TOL_DecoderSndFile = class(iOL_Decoder)
  private
    StreamHandle: pointer;
    fStreamFormat : TOLStreamFormat;
    fLastError: integer;
    sfInfo: TSF_INFO;
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
    function GetBuffer(const Frames: integer; Buffer: POLBuffer): NativeUInt;
  end;

implementation

{ TOL_DecoderSndFile }

function TOL_DecoderSndFile.Check(HR: integer): boolean;
begin
  Result := HR = SF_ERR_NO_ERROR;
  if not Result then
    fLastError := HR;
end;

function TOL_DecoderSndFile.GetStreamFormat: TOLStreamFormat;
begin
  Result := fStreamFormat;
end;

procedure TOL_DecoderSndFile.SetStreamFormat(AValue: TOLStreamFormat);
begin
  fStreamFormat := AValue;
end;

function TOL_DecoderSndFile.GetSongPos: int64;
begin
  Result := trunc(sf_seek(StreamHandle, 0, SEEK_CUR) / (fStreamFormat.BitRate / 1000));
end;

procedure TOL_DecoderSndFile.SetSongPos(AValue: int64);
begin
  sf_seek(StreamHandle, trunc(AValue * (fStreamFormat.BitRate / 1000)), SEEK_SET);
end;

function TOL_DecoderSndFile.Name: string;
begin
  Result := 'LibSndFile';
end;

function TOL_DecoderSndFile.Load(LibraryName: string): boolean;
begin
  Result := UOS_libsndfile.sf_Load(LibraryName);
end;

procedure TOL_DecoderSndFile.UnLoad;
begin

  UOS_libsndfile.sf_Unload();
end;

function TOL_DecoderSndFile.Initialize: boolean;
begin
  Result := True;
end;

procedure TOL_DecoderSndFile.Finalize;
begin

end;

function TOL_DecoderSndFile.OpenFile(FileName: TfileName): boolean;
begin

  Result := False;
  StreamHandle := sf_open_native(PChar(FileName), SFM_READ, @sfInfo);
  if not Assigned(StreamHandle) then
    exit;

  if not Check(sf_seek(StreamHandle, 0, SEEK_SET)) then
    exit;
  Result := True;
end;

procedure TOL_DecoderSndFile.Close;
begin
  sf_close(StreamHandle);
end;

function TOL_DecoderSndFile.GetBuffer(const Frames: integer; Buffer: POLBuffer): NativeUInt;
begin
  Result := sf_readf_short(StreamHandle, pcshort(Buffer), Frames);
end;


function TOL_DecoderSndFile.GetVersion: TOLVersion;
var
  BaseAddr:pointer;
  ModuleName:string;
begin
  If sf_IsLoaded then
    begin
      GetModuleByAddr(sf_version_string, BaseAddr, ModuleName);
      Result.LibraryName := ModuleName;
      Result.LibraryVersion := sf_version_string();
    end;
end;

initialization
  RegisterDecoder('*.flac;*.ogg;*.wav;*.vorbis', TOL_DecoderSndFile);
end.
