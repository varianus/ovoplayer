unit ucustomplaylist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, Buttons, ufrfield, fgl, PlaylistBuilder;

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

