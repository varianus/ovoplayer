unit OL_DecoderDummy;

{$mode objfpc}{$H+}
{$interfaces corba}
interface


uses
  Classes, SysUtils, ctypes, OL_Classes;

type
  { TOL_DecoderDummy }

  TOL_DecoderDummy = class(iOL_Decoder)
  protected
    function GetSongPos: int64;
    procedure SetSongPos(AValue: int64);
    function GetStreamFormat: TOLStreamFormat;
    procedure SetStreamFormat(AValue: TOLStreamFormat);
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

{ TOL_DecoderDummy }

function TOL_DecoderDummy.GetSongPos: int64;
begin
  Result := 0;
end;

procedure TOL_DecoderDummy.SetSongPos(AValue: int64);
begin

end;

function TOL_DecoderDummy.GetStreamFormat: TOLStreamFormat;
begin

end;

procedure TOL_DecoderDummy.SetStreamFormat(AValue: TOLStreamFormat);
begin

end;

function TOL_DecoderDummy.Load(LibraryName: string): boolean;
begin
  Result := True;
end;

procedure TOL_DecoderDummy.UnLoad;
begin

end;

function TOL_DecoderDummy.Initialize: boolean;
begin
  Result := True;
end;

procedure TOL_DecoderDummy.Finalize;
begin

end;

function TOL_DecoderDummy.OpenFile(FileName: TfileName): boolean;
begin

  Result := False;

end;

procedure TOL_DecoderDummy.Close;
begin

end;

function TOL_DecoderDummy.GetBuffer(const Frames: integer; Buffer: POLBuffer): NativeUInt;
begin
  Result := 0;
end;

end.