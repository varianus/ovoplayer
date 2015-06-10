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
    procedure signalChange(Sender: TObject);
    procedure testRatingChange(Sender: TObject);
    procedure TestTextChange(Sender: TObject);
  private
    FIdx : Integer;
    fChanged : boolean;
    FisChanged: boolean;
    function GetFilterNumber(index: Integer): string;
    function GetFilterRating(index: Integer): string;
    function GetFilterText(index: Integer): string;
    procedure SetisChanged(AValue: boolean);
  public
    procedure ShowOnlyPanel(Panel: TPanel);
    constructor Create(TheOwner: TComponent); override;
    function GetFilter: string;
    Function isExecutable: boolean;
    Function isChanged: boolean;
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

end;

function TfrField.GetFilterText(index: Integer): string;
var
  op : string;
  Value: string;
  NeedWildcards: boolean;
begin
  result:='';
  case TestText.ItemIndex of
    0: begin op := '=';        NeedWildcards:= false; end; // is
    1: begin op := '<>';       NeedWildcards:= false; end; // is not
    2: begin op := 'like';     NeedWildcards:= true;  end; // contains
    3: begin op := 'not like'; NeedWildcards:= true;  end; // not contains
    4: begin op := '=';        NeedWildcards:= false; end; // is empty
    5: begin op := '<>';       NeedWildcards:= false; end; // is not empty
  else
    exit;
  end;

  if TestText.ItemIndex < 4 then
     begin
     if edtText.Text = EmptyStr then
        exit;
     end
  else
     edtText.Text := '';

  if NeedWildcards then
     Value :=  QuotedStr('%'+edtText.Text+'%')
  else
     Value :=  QuotedStr(edtText.Text);

  result := format(' %s %s %s',[FieldArray[index].FieldName, op, Value]);

end;

procedure TfrField.SetisChanged(AValue: boolean);
begin
  if FisChanged=AValue then Exit;
  FisChanged:=AValue;
end;

function TfrField.GetFilterNumber(index: Integer): string;
var
  op : string;
  Value: string;
begin
  result:='';

  case testNumber.ItemIndex of
    0: begin op := '=';  end; // equal to
    1: begin op := '<>'; end; // not equal to
    2: begin op := '>';  end; // bigger than
    3: begin op := '<';  end; // less than
  else
    exit;
  end;

  Value := IntToStr(seNumber.Value);

  result := format(' %s %s %s',[FieldArray[index].FieldName, op, Value]);

end;

function TfrField.GetFilterRating(index: Integer): string;
var
  op : string;
  Value: string;
begin
  result:='';

  case testRating.ItemIndex of
    0: begin op := '=';  end; // equal to
    1: begin op := '<>'; end; // not equal to
    2: begin op := '>';  end; // bigger than
    3: begin op := '<';  end; // less than
    4: begin op := 'is null';  end; // equal to

  else
    exit;
  end;

  if testRating.ItemIndex = 4 then
    Value := ''
  else
    Value := IntToStr(cbRating.ItemIndex + 1);


  result := format(' %s %s %s',[FieldArray[index].FieldName, op, Value]);

end;

function TfrField.GetFilter: string;
begin
 Result := EmptyStr;
 if Fidx < 0 then exit;
 case  FieldArray[Fidx].Kind of
   ekText : result := GetFilterText(Fidx);
   ekDate : ;
   ekNumber : result := GetFilterNumber(FIdx);
   ekRating : result := GetFilterRating(FIdx);

 end;

// if Result <> EmptyStr then
//    Result := ' AND ' + Result ;
end;

function TfrField.isExecutable: boolean;
begin
 Result := False;
 if Fidx < 0 then exit;
 case  FieldArray[Fidx].Kind of
   ekText : result := (TestText.ItemIndex > 3) or
                      ((TestText.ItemIndex < 4) and (edtText.Text <> EmptyStr) )   ;

   ekDate : ;
   ekNumber : Result := True;
   ekRating : result := True;
 end;
end;

function TfrField.isChanged: boolean;
begin
  Result := fChanged;
  fChanged := false;
end;

end.

