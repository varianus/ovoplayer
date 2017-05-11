unit equalizerband;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls,
  ThemedSlider;

type

  { TfrEqualizer }

  TfrEqualizer = class(TFrame)
    lbBand: TLabel;
    lbValue: TLabel;
    slValue: TThemedSlider;
    procedure slValueChange(Sender: TObject);
  private
    fNormalize: single;
    fBandNo: integer;
  public
    Procedure InitBand(BandNo:Integer; Const Band:string; const Value: single; const Min,Max: single);
  end;

implementation

{$R *.lfm}

{ TfrEqualizer }


procedure TfrEqualizer.slValueChange(Sender: TObject);
begin
  lbValue.Caption:= format('%2.2f Db',[(slValue.Position /100) - fNormalize ] );
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

