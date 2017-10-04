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

unit equalizerband;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls,
  ThemedSlider;

type

  { TfrEqualizer }
  TBandChanged= procedure (Sender: TObject; const BandNo:integer; const Value: single) of object;


  TfrEqualizer = class(TFrame)
    lbBand: TLabel;
    lbValue: TLabel;
    slValue: TThemedSlider;
    procedure slValueChange(Sender: TObject);
  private
    fNormalize: Double;
    fBandNo: integer;
    FOnBandChanged: TBandChanged;
    function GetValue: Double;
    procedure SetOnBandChanged(AValue: TBandChanged);
    procedure SetValue(AValue: Double);
  public
    Procedure InitBand(BandNo:Integer; Const Band:string; const Value: Double; const Min,Max: Double);
    property OnBandChanged: TBandChanged read FOnBandChanged write SetOnBandChanged;
    property Value: Double read GetValue write SetValue;
  end;

implementation

{$R *.lfm}

{ TfrEqualizer }


procedure TfrEqualizer.slValueChange(Sender: TObject);
var
  calcValue: single;
begin
  calcValue := (slValue.Position /100) - fNormalize ;
  lbValue.Caption:= format('%2.2f Db',[calcValue]);
  if Assigned(FOnBandChanged) then
    FOnBandChanged(self, fBandNo, calcValue);
end;

procedure TfrEqualizer.SetOnBandChanged(AValue: TBandChanged);
begin
  if FOnBandChanged=AValue then Exit;
  FOnBandChanged:=AValue;
end;

function TfrEqualizer.GetValue: Double;
begin
  result := (slValue.Position /100) - fNormalize ;
end;

procedure TfrEqualizer.SetValue(AValue: Double);
begin
  slValue.Position:= trunc((AValue + fNormalize)*100);
  lbValue.Caption:= format('%2.2f Db',[AValue]);
end;

procedure TfrEqualizer.InitBand(BandNo:Integer;const Band: string; const Value: Double; const Min,
  Max: Double);
begin
  fBandNo:= BandNo;
  lbBand.Caption:= Band;
  lbValue.Caption:= format('%2.2f Db',[Value]);
  fNormalize:= abs(Min);
  slValue.Min := trunc(0);
  slValue.Max := trunc((max + fNormalize)*100);
  slValue.Position:= trunc((Value + fNormalize)*100);

end;

end.

