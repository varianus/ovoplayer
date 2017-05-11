unit uequalizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, equalizerband, fgl;

type

  { TfEqualizer }
  TEQBandList = class(specialize TFPGObjectList<TfrEqualizer>);

  TfEqualizer = class(TForm)
    ButtonPanel1: TButtonPanel;
    pnlHeader: TPanel;
    pnlContainer: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    EQBandList: TEQBandList;
  public


  end;

var
  fEqualizer: TfEqualizer;

implementation

{$R *.lfm}

const
  EQCounter = 10;

{ TfEqualizer }

procedure TfEqualizer.FormCreate(Sender: TObject);
var
  i:integer;
  EQBand: TfrEqualizer;
begin

  EQBandList:= TEQBandList.Create(false);

  for i := 1 to EQCounter do
  begin
    EQBand:= TfrEqualizer.Create(pnlContainer);
    EQBandList.Add(EQBand);
    EQBand.Name:='eq'+IntToStr(i);
    EQBand.Parent:= pnlContainer;
    EQBand.Align:= alLeft;
  end;

  for i:= 0 to pred(EQCounter) do
    EQBandList[i].InitBand(i,inttostr(i*10), i, -12,+12);


end;

end.

