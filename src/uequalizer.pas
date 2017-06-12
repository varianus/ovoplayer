unit uequalizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls, equalizerband, fgl, AudioEngine, Equalizer;

type

  { TfEqualizer }
  TEQBandList = class(specialize TFPGObjectList<TfrEqualizer>);

  TfEqualizer = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbEnableEq: TCheckBox;
    cbPreset: TComboBox;
    lbMessage: TLabel;
    pnlHeader: TPanel;
    pnlContainer: TPanel;
    StaticText1: TStaticText;
    procedure cbEnableEqClick(Sender: TObject);
    procedure cbPresetChange(Sender: TObject);
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
      lbMessage.Visible:=True;
      pnlContainer.Enabled:=false;
      pnlHeader.Enabled:=false;
      exit;
    end;

  for i := 0 to pred(PRESET_COUNT) do
    begin
      cbPreset.Items.Add(ARPreset[i].Name);
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

procedure TfEqualizer.cbPresetChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to pred(EQCounter) do
    begin
      eq.BandValue[i] := ARPreset[cbPreset.ItemIndex].Values[i];
      EQBandList[i].Value:= ARPreset[cbPreset.ItemIndex].Values[i];
    end;
  Eq.EQApply;
end;

procedure TfEqualizer.BandChanged(Sender: TObject; const BandNo: integer;
  const Value: single);
begin
  eq.BandValue[BandNo]:= Value;
  eq.EQApply;
end;

end.

