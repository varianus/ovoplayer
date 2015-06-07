unit ufrfield;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, ComCtrls,
  Spin, playlistbuilder, Types;

type

  { TfrField }

  TfrField = class(TFrame)
    bMinus: TBitBtn;
    cbFieldName: TComboBox;
    testNumber: TComboBox;
    edtText: TEdit;
    seNumber: TSpinEdit;
    testRating: TComboBox;
    cbRating: TComboBox;
    TestText: TComboBox;
    PageControl1: TPageControl;
    tsText: TTabSheet;
    tsDate: TTabSheet;
    tsNumber: TTabSheet;
    tsRating: TTabSheet;
    procedure bMinusClick(Sender: TObject);
    procedure cbFieldNameChange(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}
uses
  ucustomplaylist;


{ TfrField }

procedure TfrField.bMinusClick(Sender: TObject);
begin
  TfCustomPlayList(Owner.owner).Fields.Remove(Self);
end;

procedure TfrField.cbFieldNameChange(Sender: TObject);
var
  Idx : Integer;
begin
 idx :=intptr(cbFieldName.Items.Objects[cbFieldName.ItemIndex]);
 if idx < 0 then exit;

 case  FieldArray[idx].Kind of
   ekText : PageControl1.ActivePage := tsText;
   EkDate : PageControl1.ActivePage := tsDate;
   ekNumber : PageControl1.ActivePage := tsNumber;
   EKRating : PageControl1.ActivePage := tsRating;
 end;
end;

constructor TfrField.Create(TheOwner: TComponent);
var
  i: integer;
begin
  inherited Create(TheOwner);
  for i := 1 to FieldCount  do
    begin
      cbFieldName.Items.AddObject(FieldArray[i].FieldLabel, TObject(PtrInt(FieldArray[i].Id)));
    end;

end;

end.

