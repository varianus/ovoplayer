unit uscanresult;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ButtonPanel, GUIBackEnd, MediaLibrary;

type

  { TfScanResult }

  TfScanResult = class(TForm)
    ButtonPanel1: TButtonPanel;
    StringGrid1: TStringGrid;
  private

  public
    Procedure Load(Status: TScannedStatus);
  end;

var
  fScanResult: TfScanResult;

implementation

{$R *.lfm}

{ TfScanResult }

procedure TfScanResult.Load(Status: TScannedStatus);
var
  r: RScannedItem;
  c: integer;
begin
  c:= 0;

  for r in BackEnd.mediaLibrary.ScanResult do
    if r.Status = Status then
      inc(c);

  StringGrid1.RowCount := c;
  c:=0;
  for r in BackEnd.mediaLibrary.ScanResult do
    if r.Status = Status then
      begin
       StringGrid1.Cells[0,c]:=r.FileName;
       inc(c)
      end;

end;

end.

