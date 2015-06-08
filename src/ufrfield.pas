unit ufrfield;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, ComCtrls,
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
    PageControl1: TPageControl;
    tsText: TTabSheet;
    tsDate: TTabSheet;
    tsNumber: TTabSheet;
    tsRating: TTabSheet;
    procedure bMinusClick(Sender: TObject);
    procedure cbFieldNameChange(Sender: TObject);
  private
    function GetFilterText(index: Integer): string;
  public
    constructor Create(TheOwner: TComponent); override;
    function GetFilter: string;
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
  cp.Fields.Extract(Self);
  Application.ReleaseComponent(self);

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

function TfrField.GetFilterText(index: Integer): string;
var
  op : string;
  Value: string;
  NeedWildcards: boolean;
begin
  result:='';
  case TestText.ItemIndex of
    0: begin op := '=';        NeedWildcards:= false; end;  // is
    1: begin op := '<>';       NeedWildcards:= false; end;  // is not
    2: begin op := 'like';     NeedWildcards:= true; end;  // contains
    3: begin op := 'not like'; NeedWildcards:= true; end; // not contains
  else
    exit;
  end;

  if edtText.Text = EmptyStr then
    exit;

  if NeedWildcards then
     Value :=  QuotedStr('%'+edtText.Text+'%')
  else
     Value :=  QuotedStr(edtText.Text);

  result := format(' %s %s %s',[FieldArray[index].FieldName, op, Value]);


end;

function TfrField.GetFilter: string;
var
 Idx : Integer;
begin
 idx :=intptr(cbFieldName.Items.Objects[cbFieldName.ItemIndex]);
 if idx < 0 then exit;

 case  FieldArray[idx].Kind of
   ekText : result := GetFilterText(idx);
//   EkDate :
//   ekNumber :
//   EKRating :
 end;

 if Result <> EmptyStr then
    Result := ' AND ' + result ;
end;

end.

