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
    fNormalize: single;
    fBandNo: integer;
    FOnBandChanged: TBandChanged;
    procedure SetOnBandChanged(AValue: TBandChanged);
  public
    Procedure InitBand(BandNo:Integer; Const Band:string; const Value: single; const Min,Max: single);
    property OnBandChanged: TBandChanged read FOnBandChanged write SetOnBandChanged;
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

procedure TfrEqualizer.InitBand(BandNo:Integer;const Band: string; const Value: single; const Min,
  Max: single);
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

