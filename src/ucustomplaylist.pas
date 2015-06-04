unit ucustomplaylist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel;

type

  { TfCustomPlayList }

  TfCustomPlayList = class(TForm)
    ButtonPanel1: TButtonPanel;
    Panel1: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fCustomPlayList: TfCustomPlayList;

implementation

{$R *.lfm}

end.

