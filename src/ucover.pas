unit uCover;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons;

type

  { TfCover }

  TfCover = class(TForm)
    btnZoomIn: TBitBtn;
    btnZoomOut: TBitBtn;
    btnZoomReset: TBitBtn;
    ImageCover: TImage;
    pnlSize: TPanel;
    ScrollBox1: TScrollBox;
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnZoomResetClick(Sender: TObject);
  private
    CurrHeight, CurrWidth : integer;
  public
    Procedure SetSize(imageHeigth, imageWidth: integer);
  end;

var
  fCover: TfCover;

implementation
{$R *.lfm}
uses
  math;
Const
  zoomRatio = 1.5;

{ TfCover }

procedure TfCover.btnZoomInClick(Sender: TObject);
begin
  SetSize(round(CurrHeight * zoomRatio), round(CurrWidth * zoomRatio));
end;

procedure TfCover.btnZoomOutClick(Sender: TObject);
begin
  SetSize(round(CurrHeight * (1/zoomRatio)), round(CurrWidth * (1/zoomRatio)));
end;

procedure TfCover.btnZoomResetClick(Sender: TObject);
begin
  SetSize(ImageCover.Picture.Height,ImageCover.Picture.Width);
end;

procedure TfCover.SetSize(imageHeigth, imageWidth: integer);
begin
  ClientHeight := min(Screen.Height, imageHeigth + pnlSize.Height + HorzScrollBar.Size);
  ClientWidth := min(Screen.Width, imageWidth + VertScrollBar.size);
  ImageCover.Height := imageHeigth;
  ImageCover.Width := imageWidth;
  CurrHeight:= imageHeigth;
  CurrWidth := imageWidth;
end;

end.

