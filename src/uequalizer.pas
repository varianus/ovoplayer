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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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

  eq.ActiveEQ:=true;
  BandInfo := Eq.BandInfo;
  EQBandList:= TEQBandList.Create(false);

  pnlContainer.DisableAlign;
  for i := 1 to length(BandInfo) do
  begin
    EQBand:= TfrEqualizer.Create(pnlContainer);
    EQBandList.Add(EQBand);
    EQBand.Name:='eq'+IntToStr(i);
    EQBand.Parent:= pnlContainer;
    EQBand.OnBandChanged:=@BandChanged;
    EQBand.Align := alLeft;
    EQBand.Left := i* EQBand.Width;
    EQBand.InitBand(i, FloatToStr(BandInfo[i].freq), BandInfo[i-1].Value, -12,+12);
  end;
  pnlContainer.EnableAlign;


end;

procedure TfEqualizer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    eq.ActiveEQ:=false;
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

