unit ucustomplaylist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, Buttons, StdCtrls, Spin, ufrfield, fgl, PlaylistBuilder,
  GUIBackEnd;

type

  { TfCustomPlayList }
  TFieldContainer = specialize TFPGObjectList<TfrField>;

  TfCustomPlayList = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    bPlus: TBitBtn;
    ButtonPanel1: TButtonPanel;
    ckRandom: TCheckBox;
    ckLimit: TCheckBox;
    Panel1: TPanel;
    Panel2: TPanel;
    sbFieldContainer: TScrollBox;
    seLimits: TSpinEdit;
    procedure bPlusClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    fDefaultIndex: integer;
  public
    Fields: TFieldContainer;
    Function AddField:TfrField;
  end;

var
  fCustomPlayList: TfCustomPlayList;

implementation

{$R *.lfm}

{ TfCustomPlayList }

procedure TfCustomPlayList.FormCreate(Sender: TObject);
var
  i: integer;
begin
  // Sort field names based on label.
  // Doing it here should sort the already translated
  SortFields;
  //Find at wich index is the title field, used as a default
  fDefaultIndex:=0;
  for i := 0 to FieldCount -1 do
    if FieldArray[i].Id = 4 then
      begin
         fDefaultIndex:=i;
         Break;
      end;
  Fields := TFieldContainer.Create(true);
  AddField;
  seLimits.AnchorToNeighbour(akLeft, 10, ckLimit);
end;

procedure TfCustomPlayList.OKButtonClick(Sender: TObject);
var
  filters, sorts: string;
  i: integer;
begin
  Filters:= '1=1'; // dummy test
  for i := 0 to Fields.Count -1 do
    Filters := Filters + Fields[i].GetFilter;

  if ckRandom.Checked then
    Sorts := ' random() '
  else
    Sorts := ' id ';
  if ckLimit.Checked then
    sorts:= sorts + ' Limit ' + inttostr(seLimits.Value);


  BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList,
        Filters, sorts );

end;

procedure TfCustomPlayList.bPlusClick(Sender: TObject);
begin
  AddField;

end;

function TfCustomPlayList.AddField: TfrField;
begin
  Result:= TfrField.Create(sbFieldContainer);
  Result.Name := 'FRA'+IntToStr(PtrUInt(result));
  Result.Align:=alTop;
  Fields.Add(Result);
  Result.Parent := sbFieldContainer;
  result.Top:=sbFieldContainer.Height;
  result.Show;
  // initialize with some value
  result.cbFieldName.ItemIndex:=fDefaultIndex;
  result.ShowOnlyPanel(result.pnlText);
  result.TestText.ItemIndex:=0;

end;

end.

