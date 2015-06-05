unit ucustomplaylist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, ufrfield, fgl;

type

  { TfCustomPlayList }
  TFieldContainer = specialize TFPGObjectList<TfrField>;

  TfCustomPlayList = class(TForm)
    ButtonPanel1: TButtonPanel;
    Panel1: TPanel;
    sbFieldContainer: TScrollBox;
    procedure FormCreate(Sender: TObject);
  private
    Fields: TFieldContainer;
  public
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

function TfCustomPlayList.AddField: TfrField;
begin
  Result:= TfrField.Create(sbFieldContainer);
  result.Align:=altop;
  Fields.Add(Result);
  Result.Parent := sbFieldContainer;
end;

end.

