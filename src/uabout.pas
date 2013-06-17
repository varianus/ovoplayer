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
  StdCtrls, Buttons, uLicense;

type

  { TfAbout }

  TfAbout = class(TForm)
    bClose: TBitBtn;
    bLicense: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbSVNRev: TLabel;
    lbFPCVersion: TLabel;
    lbBuildDate: TLabel;
    lVersion: TLabel;
    lHomePage: TLabel;
    lbLazVersion: TLabel;
    procedure bCloseClick(Sender: TObject);
    procedure bLicenseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lHomePageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lHomePageMouseEnter(Sender: TObject);
    procedure lHomePageMouseLeave(Sender: TObject);
  private
    LicenseForm : TfLicense;
    procedure LicenseDestroy(Sender: TObject);
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
  lVersion.caption     := AppVersion;
  lbFPCVersion.Caption := fpcVersion;
  lbLazVersion.Caption := lazVersion;
  lbBuildDate.Caption  := BuildDate;
  lbSVNRev.Caption     := ovoRevision;

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

procedure TfAbout.bLicenseClick(Sender: TObject);
begin
  if Not assigned(LicenseForm) then
     begin
       LicenseForm:= TfLicense.Create(Self);
       LicenseForm.OnDestroy:=@LicenseDestroy;
       LicenseForm.Show;
     end
  else
    FreeAndNil(LicenseForm);
end;

procedure TfAbout.LicenseDestroy(Sender: TObject);
begin
  LicenseForm := nil;
end;


end.

