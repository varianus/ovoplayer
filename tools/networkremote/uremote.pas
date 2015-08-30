unit uremote;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    tbConn: TToggleBox;
    procedure tbConnChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.tbConnChange(Sender: TObject);
begin
  if tbConn.Checked then
     tbConn.Caption:= 'Disconnect'
  else
     tbConn.Caption := 'Connect';

end;

end.

