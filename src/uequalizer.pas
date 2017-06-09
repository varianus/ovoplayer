unit uequalizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls, equalizerband, fgl, AudioEngine;

type

  { TfEqualizer }
  TEQBandList = class(specialize TFPGObjectList<TfrEqualizer>);

  TfEqualizer = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbEnableEq: TCheckBox;
    lbMessage: TLabel;
    pnlHeader: TPanel;
    pnlContainer: TPanel;
    procedure cbEnableEqClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Eq: IEqualizer;
    EQBandList: TEQBandList;
    procedure BandChanged(Sender: TObject; const BandNo:integer; const Value: single);
  public

  end;

var
  fEqualizer: TfEqualizer;

implementation
uses GUIBackEnd;

{$R *.lfm}

const
  EQCounter = 10;

{ TfEqualizer }

procedure TfEqualizer.FormCreate(Sender: TObject);
var
  i:integer;
  EQBand: TfrEqualizer;
  BandInfo: ARBandinfo;
begin

  if not  Supports(BackEnd.AudioEngine, IEqualizer, Eq) then
    begin
      lbMessage.Caption:='UNSUPPORTED ENGINE';
      pnlContainer.Enabled:=false;
      pnlHeader.Enabled:=false;
      exit;
    end;

  BandInfo := Eq.BandInfo;
  EQBandList:= TEQBandList.Create(false);

  pnlContainer.DisableAlign;
  for i := 0 to length(BandInfo)-1 do
  begin
    EQBand:= TfrEqualizer.Create(pnlContainer);
    EQBandList.Add(EQBand);
    EQBand.Name:='eq'+IntToStr(i);
    EQBand.Parent:= pnlContainer;
    EQBand.OnBandChanged:=@BandChanged;
    EQBand.Align := alLeft;
    EQBand.Left := i* EQBand.Width;
    // Some engines allow a range greater than -12/+12 db, this should be supported by all engines...
    EQBand.InitBand(i, FloatToStr(BandInfo[i].freq), BandInfo[i].Value, -12,+12);
  end;
  pnlContainer.EnableAlign;


end;

procedure TfEqualizer.cbEnableEqClick(Sender: TObject);
begin
  Eq.ActiveEQ:=cbEnableEq.Checked;

end;

procedure TfEqualizer.BandChanged(Sender: TObject; const BandNo: integer;
  const Value: single);
begin
  eq.BandValue[BandNo]:= Value;
  eq.EQApply;
end;

end.

