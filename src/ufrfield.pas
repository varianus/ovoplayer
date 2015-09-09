unit ufrfield;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ComCtrls,
  Spin, ExtCtrls, playlistbuilder, Types;

type

  { TfrField }

  TfrField = class(TFrame)
    bMinus: TBitBtn;
    cbFieldName: TComboBox;
    Panel1: TPanel;
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
    procedure signalChange(Sender: TObject);
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

procedure TfrField.signalChange(Sender: TObject);
begin
  fChanged:=true;
  UpdateFilter;
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
    EkDate : ShowOnlyPanel(pnlDate);
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
//      EkDate : ShowOnlyPanel(pnlDate);
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

