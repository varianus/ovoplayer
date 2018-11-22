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
{$I codegen.inc}
{$I ovoplayer.inc}
unit ufrfield;

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ComCtrls,
  Spin, ExtCtrls, EditBtn, playlistbuilder;

type

  { TfrField }

  TfrField = class(TFrame)
    bMinus: TBitBtn;
    cbFieldName: TComboBox;
    dtpDate: TDateEdit;
    Panel1: TPanel;
    testDate: TComboBox;
    testNumber: TComboBox;
    edtText: TEdit;
    seNumber: TSpinEdit;
    testRating: TComboBox;
    cbRating: TComboBox;
    TestText: TComboBox;
    pnlMain: TPanel;
    pnlText: TPanel;
    pnlDate: TPanel;
    pnlNumber: TPanel;
    pnlRating: TPanel;
    procedure bMinusClick(Sender: TObject);
    procedure cbFieldNameChange(Sender: TObject);
    procedure dtpDateChange(Sender: TObject);
    procedure signalChange(Sender: TObject);
    procedure testDateChange(Sender: TObject);
    procedure testRatingChange(Sender: TObject);
    procedure TestTextChange(Sender: TObject);
  private
    FIdx : Integer;
    fChanged : boolean;
    FisChanged: boolean;
    procedure SetisChanged(AValue: boolean);
  public
    FieldFilter : TFieldFilter;
    procedure ShowOnlyPanel(Panel: TPanel);
    constructor Create(TheOwner: TComponent); override;
    Function isChanged: boolean;
    procedure UpdateFromFilter;
    procedure UpdateFilter;
  end;

implementation

{$R *.lfm}
uses
  ucustomplaylist;


{ TfrField }

procedure TfrField.bMinusClick(Sender: TObject);
var
  cp: TfCustomPlayList;
begin
  cp:=TfCustomPlayList(Owner.owner);
  cp.DeleteField(self);
  Application.ReleaseComponent(self);

end;

procedure TfrField.ShowOnlyPanel(Panel:TPanel);
var i: integer;
begin
  For i := 0 to pnlMain.ControlCount -1 do
    pnlMain.Controls[i].Visible:= false;

  if Assigned(Panel) then
     Panel.Visible:=true;
end;

procedure TfrField.cbFieldNameChange(Sender: TObject);
begin
 Fidx := cbFieldName.ItemIndex;
 if Fidx < 0 then exit;

 case  FieldArray[fidx].Kind of
   ekText : ShowOnlyPanel(pnlText);
   EkDate : ShowOnlyPanel(pnlDate);
   ekNumber : ShowOnlyPanel(pnlNumber);
   EKRating : ShowOnlyPanel(pnlRating);
 end;
 signalChange(Self);
end;

procedure TfrField.dtpDateChange(Sender: TObject);
begin
  signalChange(self);
end;

procedure TfrField.signalChange(Sender: TObject);
begin
  fChanged:=true;
  UpdateFilter;
end;

procedure TfrField.testDateChange(Sender: TObject);
begin
  dtpDate.Visible := testDate.ItemIndex < 4;
  signalChange(self);

end;

procedure TfrField.testRatingChange(Sender: TObject);
begin
 cbRating.Visible := testRating.ItemIndex > 0;
 signalChange(self);

end;

procedure TfrField.TestTextChange(Sender: TObject);
begin
 edtText.Visible := TestText.ItemIndex < 4;
 signalChange(self);
end;

constructor TfrField.Create(TheOwner: TComponent);
var
  i: integer;
begin
  inherited Create(TheOwner);
  FIdx := -1;
  for i := 0 to FieldCount -1 do
    begin
      cbFieldName.Items.Add(FieldArray[i].FieldLabel);
    end;

  with testRating do
    begin
      Items.Clear;
      Items.Add(RS_EqualTo);
      Items.Add(RS_NotEqualTo);
      Items.Add(RS_BiggerThan);
      Items.Add(RS_LessThan);
      Items.Add(RS_NotRated);
    end;

  with TestText do
    begin
      Items.Clear;
      Items.Add(RS_Contains);
      Items.Add(RS_NotContains);
      Items.Add(RS_Is);
      Items.Add(RS_IsNot);
      Items.Add(RS_IsEmpty);
      Items.Add(RS_IsNotEmpty);
    end;

  with testNumber do
    begin
      Items.Clear;
      Items.Add(RS_EqualTo);
      Items.Add(RS_NotEqualTo);
      Items.Add(RS_BiggerThan);
      Items.Add(RS_LessThan);
    end;

  with testDate do
    begin
      Items.Clear;
      Items.Add(RS_On);
      Items.Add(RS_NotOn);
      Items.Add(RS_Before);
      Items.Add(RS_After);
//      Items.Add(RS_InTheLast);
//      Items.Add(RS_NotInTheLast);
    end;


end;

procedure TfrField.SetisChanged(AValue: boolean);
begin
  FisChanged:=AValue;
  UpdateFilter;
end;

function TfrField.isChanged: boolean;
begin
  Result := fChanged;
  fChanged := false;
end;

procedure TfrField.UpdateFromFilter;
begin
  FIdx:= FindIndexByID(FieldFilter.FieldID);
  cbFieldName.ItemIndex:=fidx;
  case  FieldArray[fidx].Kind of
    ekText : begin
               ShowOnlyPanel(pnlText);
               TestText.ItemIndex:=FieldFilter.TestIndex;
               edtText.Text:=FieldFilter.Value;
             end;
    EkDate :  begin
                 ShowOnlyPanel(pnlDate);
                 testDate.ItemIndex:=FieldFilter.TestIndex;
                 dtpDate.Date:=FieldFilter.AsDate;
               end;
    ekNumber : begin
                 ShowOnlyPanel(pnlNumber);
                 testNumber.ItemIndex:=FieldFilter.TestIndex;
                 seNumber.Value:=FieldFilter.AsInteger;
               end;
    EKRating : begin
                 ShowOnlyPanel(pnlRating);
                 testRating.ItemIndex:=FieldFilter.TestIndex;
                 cbRating.ItemIndex:=FieldFilter.AsInteger;
    end;
  end;
end;

procedure TfrField.UpdateFilter;
begin
   if Not Assigned(FieldFilter) or (FIdx <0) then exit;
    FieldFilter.FieldID := FieldArray[FIdx].Id;
    case  FieldArray[fidx].Kind of
      ekText : begin
                 FieldFilter.TestIndex   := TestText.ItemIndex;
                 FieldFilter.Value       := edtText.Text;
               end;
      EkDate : begin
                 FieldFilter.TestIndex   := testDate.ItemIndex;
                 FieldFilter.Value       := inttostr(trunc(dtpDate.Date));
               end;
      ekNumber : begin
                   FieldFilter.TestIndex := testNumber.ItemIndex;
                   FieldFilter.Value     := IntToStr(seNumber.Value);
                 end;
      EKRating : begin
                   FieldFilter.TestIndex := testRating.ItemIndex;
                   FieldFilter.Value     := IntToStr(cbRating.ItemIndex);
      end;
    end;

end;

end.

