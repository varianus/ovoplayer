unit ufrfield;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons;

type

  { TfrField }

  TfrField = class(TFrame)
    bPlus: TBitBtn;
    bMinus: TBitBtn;
    cbFieldName: TComboBox;
    cbTest: TComboBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

