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
{$I ovoplayer.inc}
unit uequalizer;

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
    procedure InitBands(BandInfo: ARBandinfo);
    procedure PresetToScreen(Preset: Integer);
  public

  end;

var
  fEqualizer: TfEqualizer;

implementation
uses GUIBackEnd, AppConsts;

{$R *.lfm}

{ TfEqualizer }

procedure TfEqualizer.FormCreate(Sender: TObject);
var
  i:integer;
begin

  if (not BackEnd.AudioEngine.SupportEQ) or
    not Supports(BackEnd.AudioEngine, IEqualizer, Eq) then
    begin
      lbMessage.Caption := rUnsupportedEngine;
      lbMessage.Visible:=True;
      pnlContainer.Enabled:=false;
      pnlHeader.Enabled:=false;
      exit;
    end;

  for i := 0 to pred(PRESET_COUNT) do
    begin
      cbPreset.Items.Add(ARPreset[i].Name);
    end;

  EQBandList:= TEQBandList.Create(false);

  if Eq.ActiveEQ then
    begin
      InitBands(eq.BandInfo);
    end;

  cbEnableEq.Checked:= BackEnd.EngineParam.ActiveEQ;
  cbPreset.ItemIndex:= BackEnd.EngineParam.EQPreset;

end;

Procedure TfEqualizer.InitBands( BandInfo: ARBandinfo);
var
  i: integer;
  EQBand: TfrEqualizer;
begin
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
    EQBand.InitBand(i, FloatToStr(BandInfo[i].freq), BandInfo[i].Value, -12, +12);
  end;
  pnlContainer.EnableAlign;

end;

procedure TfEqualizer.cbEnableEqClick(Sender: TObject);
var
  BandInfo: ARBandinfo;
begin
  Eq.ActiveEQ:=cbEnableEq.Checked;

  BackEnd.EngineParam.ActiveEQ := Eq.ActiveEQ;
  if Eq.getActiveEQ and (EQBandList.Count = 0) then
    begin
      InitBands(eq.BandInfo);
    end;
end;

procedure TfEqualizer.cbPresetChange(Sender: TObject);

begin
  PresetToScreen(cbPreset.ItemIndex);
  ApplyPreset(eq, cbPreset.ItemIndex);
  BackEnd.EngineParam.EQPreset:= cbPreset.ItemIndex;

end;

Procedure TfEqualizer.PresetToScreen(Preset:Integer);
var
 i: integer;
begin
 for i := 0 to pred(EQCounter) do
   begin
     EQBandList[i].Value := ARPreset[Preset].Values[i];
   end;
end;

procedure TfEqualizer.BandChanged(Sender: TObject; const BandNo: integer;
  const Value: single);
begin
  eq.BandValue[BandNo]:= Value;
  eq.EQApply;
end;

end.

