unit ucustomplaylist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, Buttons, StdCtrls, Spin, ufrfield, fgl, PlaylistBuilder,
  GUIBackEnd, MediaLibrary, AppConsts, GeneralFunc;

type

  { TfCustomPlayList }
  TEditorsContainer = specialize TFPGObjectList<TfrField>;

  TfCustomPlayList = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    bPlus: TBitBtn;
    ButtonPanel1: TButtonPanel;
    ckRandom: TCheckBox;
    ckLimit: TCheckBox;
    lbFilterResults: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    sbFieldContainer: TScrollBox;
    seLimits: TSpinEdit;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure bPlusClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    fDefaultIndex: integer;
    PlayListBuilder: TPlayListBuilder;
    function ComposeFilter: String;
  public
    Editors: TEditorsContainer;
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
  PlayListBuilder := TPlayListBuilder.Create;

  // Sort field names based on label.
  // Doing it here should sort the already translated ones
  SortFields;
  //Find at wich index is the title field, used as a default
  fDefaultIndex:=0;
  for i := 0 to FieldCount -1 do
    if FieldArray[i].Id = 4 then
      begin
         fDefaultIndex:=i;
         Break;
      end;
  Editors := TEditorsContainer.Create(true);
  AddField;
  seLimits.AnchorToNeighbour(akLeft, 10, ckLimit);
end;

procedure TfCustomPlayList.OKButtonClick(Sender: TObject);
var
  filters, sorts: string;
  i: integer;
begin
  Filters:= '1=1'; // dummy test
  for i := 0 to Editors.Count -1 do
    Filters := Filters + ' AND '+ Editors[i].FieldFilter.GetFilter;

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

procedure TfCustomPlayList.ApplicationProperties1Idle(Sender: TObject;
  var Done: Boolean);
var
  i: integer;
  Executable : boolean;
  PlayListData: RFilterInfo;
  isChanged: boolean;
begin
  Executable := true;
  isChanged:= false;
  for i := 0 to PlayListBuilder.Count -1 do
    Executable:=Executable and PlayListBuilder[i].isExecutable;

  ButtonPanel1.OKButton.Enabled:=Executable;

  if Executable then
    begin
      for i := 0 to Editors.Count -1 do
        isChanged:=isChanged or Editors[i].isChanged;
      if isChanged then
        begin
          PlayListData := BackEnd.mediaLibrary.FilterInfo(ComposeFilter);
          lbFilterResults.Caption:= Format(rMatchingItems,[PlayListData.Count,
                                                          TimeToStr(PlayListData.TotalTime / MSecsPerDay),
                                                          FormatByteString(PlayListData.TotalSize)]);

      end;
    end;

end;

function TfCustomPlayList.ComposeFilter: String;
var
  i: integer;

begin
  Result:=EmptyStr;
  for i := 0 to PlayListBuilder.Count -1 do
    begin
      Result := Result + ' ' + PlayListBuilder[i].GetFilter;
    end;

end;

function TfCustomPlayList.AddField: TfrField;
begin

  Result:= TfrField.Create(sbFieldContainer);
  Result.FieldFilter := TFieldFilter.Create;
  PlayListBuilder.Capacity:= 4;
  PlayListBuilder.Add(Result.FieldFilter);
  Result.Name := 'FRA'+IntToStr(PtrUInt(result));
  Result.Align:=alTop;
  Editors.Add(Result);
  Result.Parent := sbFieldContainer;
  result.Top:=sbFieldContainer.Height;
  // initialize with some value
  result.cbFieldName.ItemIndex:=fDefaultIndex;
  result.ShowOnlyPanel(result.pnlText);
  result.TestText.ItemIndex:=0;
  Result.TestTextChange(Result.TestText);
  Result.cbFieldNameChange(Result);
  Result.UpdateFilter;

end;

end.

