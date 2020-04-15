unit OL_FilterVolume;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OL_Classes;

Type

  { TOL_FilterVolume }

  TOL_FilterVolume = Class(IOL_FilterVolume)
  private
    fVolume : Integer;
    fStreamFormat: TOLStreamFormat;
  protected
    function GetVolume: Integer;
    procedure SetVolume(AValue: Integer);
    function GetStreamFormat: TOLStreamFormat;
    procedure SetStreamFormat(AValue: TOLStreamFormat);
  Public
    procedure Apply(const Frames: integer; buffer: POLBuffer );

  end;

implementation


{ TOL_FilterVolume }

function TOL_FilterVolume.GetVolume: Integer;
begin
  Result := fVolume;
end;

procedure TOL_FilterVolume.SetVolume(AValue: Integer);
begin
  if FVolume = AValue then Exit;
  FVolume := AValue;
end;

function TOL_FilterVolume.GetStreamFormat: TOLStreamFormat;
begin
  Result := fStreamFormat;
end;

procedure TOL_FilterVolume.SetStreamFormat(AValue: TOLStreamFormat);
begin
  fStreamFormat := AValue;
end;

procedure TOL_FilterVolume.Apply(const Frames: integer; buffer: POLBuffer );
var
  FloatVol: double;
  x: Integer;
begin

  FloatVol := Sqr(fVolume / 255);
  for x := 0 to (Frames * fStreamFormat.Channels )- 1 do
  begin
    buffer^[x] := trunc(buffer^[x] * FloatVol);
 //    This to avoid distortion
    if buffer^[x] < (-32760) then
      buffer^[x] := -32760;
    if buffer^[x] > (32760) then
      buffer^[x] := 32760;

  end;


end;

end.

