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
    bPlus1: TBitBtn;
    ButtonPanel1: TButtonPanel;
    cbFieldName: TComboBox;
    ckLimit: TCheckBox;
    lbSort: TLabel;
    lbFilterResults: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    sbFieldContainer: TScrollBox;
    seLimits: TSpinEdit;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure bPlus1Click(Sender: TObject);
    procedure bPlusClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    fDefaultIndex: integer;
    PlayListBuilder: TPlayListBuilder;
  public
    Editors: TEditorsContainer;
    Function AddField:TfrField;
    Procedure UpdateBuilder;
    Procedure UpdateFromBuilder(LoadFilters: boolean);
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

  cbFieldName.Items.Add(RS_Random);

  for i := 0 to FieldCount -1 do
    begin
      cbFieldName.Items.Add(FieldArray[i].FieldLabel);
    end;

  cbFieldName.ItemIndex:=0;

  Editors := TEditorsContainer.Create(true);
  AddField;
  seLimits.AnchorToNeighbour(akLeft, 10, ckLimit);
end;

procedure TfCustomPlayList.OKButtonClick(Sender: TObject);
var
  i: integer;
begin

  BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList,
        PlayListBuilder.Filter, PlayListBuilder.SortClause );

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
  isChanged:= false;
  Executable:= PlayListBuilder.isExecutable;

  ButtonPanel1.OKButton.Enabled:=Executable;

  if Executable then
    begin
      for i := 0 to Editors.Count -1 do
        isChanged:=isChanged or Editors[i].isChanged;
      if isChanged then
        begin
          PlayListData := BackEnd.mediaLibrary.FilterInfo(PlayListBuilder.Filter);
          lbFilterResults.Caption:= Format(rMatchingItems,[PlayListData.Count,
                                                          TimeToStr(PlayListData.TotalTime / MSecsPerDay),
                                                          FormatByteString(PlayListData.TotalSize)]);

      end;
    end;

end;

procedure TfCustomPlayList.bPlus1Click(Sender: TObject);
begin
  PlayListBuilder.ToJson('testfile.json');
end;

function TfCustomPlayList.AddField: TfrField;
begin

  Result:= TfrField.Create(sbFieldContainer);
  Result.FieldFilter := TFieldFilter.Create;
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

procedure TfCustomPlayList.UpdateBuilder;
begin
  if ckLimit.Checked then
     PlayListBuilder.SongLimit := seLimits.Value
  else
     PlayListBuilder.SongLimit := -1;

  if cbFieldName.ItemIndex = 0 then
    PlayListBuilder.SortFieldID:= -1
  else
    PlayListBuilder.SortFieldID:= FieldArray[cbFieldName.ItemIndex-1].Id;

end;

procedure TfCustomPlayList.UpdateFromBuilder(LoadFilters: boolean);
var
  i:integer;
  _Frame: TfrField;
begin
  if PlayListBuilder.SongLimit >0  then
     begin
       seLimits.Value := PlayListBuilder.SongLimit;
       ckLimit.Checked:= true;;
     end
  else
     begin
      seLimits.Value := 50;
      ckLimit.Checked:= False;
     end;

  if cbFieldName.ItemIndex = 0 then
    PlayListBuilder.SortFieldID:= -1
  else
    PlayListBuilder.SortFieldID:= FieldArray[cbFieldName.ItemIndex-1].Id;

  if LoadFilters then
      for i := 0 to PlayListBuilder.Count -1 do
        begin
         _Frame:= TfrField.Create(sbFieldContainer);
         _Frame.FieldFilter := PlayListBuilder[i];
         _Frame.UpdateFromFilter;
         _Frame.Name := 'FRA'+IntToStr(PtrUInt(_Frame));
         _Frame.Align:=alTop;
         Editors.Add(_Frame);
         _Frame.Parent := sbFieldContainer;
         _Frame.Top:=sbFieldContainer.Height;
        end;

end;

end.

