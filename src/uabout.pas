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
{$I ovoplayer.inc}
unit uAbout;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TfAbout }

  TfAbout = class(TForm)
    bClose: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    lVersion: TLabel;
    lHomePage: TLabel;
    procedure bCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lHomePageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lHomePageMouseEnter(Sender: TObject);
    procedure lHomePageMouseLeave(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fAbout: TfAbout;

implementation

uses AppConsts, lclintf;
{$R *.lfm}

{ TfAbout }

procedure TfAbout.FormShow(Sender: TObject);
var
  i:Integer;
begin
  lVersion.caption := AppVersion;
  for i := 0 to ComponentCount -1 do
     if Components[i] is TLabel then
       if Tlabel (Components[i]).OptimalFill then
          Tlabel (Components[i]).AdjustFontForOptimalFill;


end;

procedure TfAbout.lHomePageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  OpenURL(TLabel(Sender).Caption);
end;

procedure TfAbout.lHomePageMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsUnderLine];
  TLabel(Sender).Font.Color := clRed;
  TLabel(Sender).Cursor := crHandPoint;
end;

procedure TfAbout.lHomePageMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [];
  TLabel(Sender).Font.Color := clBlue;
  TLabel(Sender).Cursor := crDefault;
end;

procedure TfAbout.bCloseClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

end.
