unit ucustomplaylist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, Buttons, ufrfield, fgl, PlaylistBuilder, GUIBackEnd;

type

  { TfCustomPlayList }
  TFieldContainer = specialize TFPGObjectList<TfrField>;

  TfCustomPlayList = class(TForm)
    bPlus: TBitBtn;
    ButtonPanel1: TButtonPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    sbFieldContainer: TScrollBox;
    procedure bPlusClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

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
begin
  Fields := TFieldContainer.Create(true);
  AddField;
end;

procedure TfCustomPlayList.OKButtonClick(Sender: TObject);
var
  filters: string;
  i: integer;
begin
  Filters:= '1=1'; // dummy test
  for i := 0 to Fields.Count -1 do
    Filters := Filters + Fields[i].GetFilter;

  BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList,
        Filters, ' random()');

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

end;

end.

