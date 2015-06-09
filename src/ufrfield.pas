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
    pnlMain: TPanel;
    pnlText: TPanel;
    pnlDate: TPanel;
    pnlNumber: TPanel;
    pnlRating: TPanel;
    procedure bMinusClick(Sender: TObject);
    procedure cbFieldNameChange(Sender: TObject);
  private
    function GetFilterText(index: Integer): string;
  public
    procedure ShowOnlyPanel(Panel: TPanel);
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

procedure TfrField.ShowOnlyPanel(Panel:TPanel);
var i: integer;
begin
  For i := 0 to pnlMain.ControlCount -1 do
    pnlMain.Controls[i].Visible:= false;

  if Assigned(Panel) then
     Panel.Visible:=true;
end;

procedure TfrField.cbFieldNameChange(Sender: TObject);
var
  Idx : Integer;
begin
 idx := cbFieldName.ItemIndex;
 if idx < 0 then exit;

 case  FieldArray[idx].Kind of
   ekText : ShowOnlyPanel(pnlText);
   EkDate : ShowOnlyPanel(pnlDate);
   ekNumber : ShowOnlyPanel(pnlNumber);
   EKRating : ShowOnlyPanel(pnlRating);
 end;
end;

constructor TfrField.Create(TheOwner: TComponent);
var
  i: integer;
begin
  inherited Create(TheOwner);
  for i := 0 to FieldCount -1 do
    begin
      cbFieldName.Items.Add(FieldArray[i].FieldLabel);
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
 idx := cbFieldName.ItemIndex;
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

