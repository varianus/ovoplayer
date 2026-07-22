{
This file is part of OvoPlayer
Copyright (C) 2011 Marco Caselli

OvoPlayer is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

}
{$I codegen.inc}
{$I ovoplayer.inc}
unit uscanresult;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ButtonPanel, GUIBackEnd, MediaLibrary, uDM;

type

  { TfScanResult }

  TfScanResult = class(TForm)
    ButtonPanel1: TButtonPanel;
    ResultGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private

  public
    Procedure Load(Status: TScannedStatus);
  end;

var
  fScanResult: TfScanResult;

implementation

{$R *.lfm}

{ TfScanResult }

procedure TfScanResult.FormCreate(Sender: TObject);
begin
  ButtonPanel1.CancelButton.Images := DM.ilButtons;
  ButtonPanel1.CancelButton.ImageIndex := 26;

end;

procedure TfScanResult.Load(Status: TScannedStatus);
var
  r: RScannedItem;
  c: integer;
begin
  c:= 0;

  for r in BackEnd.mediaLibrary.ScanResult do
    if r.Status = Status then
      inc(c);

  ResultGrid.RowCount := c+1;
  c:=1;
  for r in BackEnd.mediaLibrary.ScanResult do
    if r.Status = Status then
      begin
       ResultGrid.Cells[0,c]:=r.FileName;
       inc(c)
      end;

end;

end.

