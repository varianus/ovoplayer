unit uCover;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls;

type

  { TfCover }

  TfCover = class(TForm)
    btnZoomIn: TBitBtn;
    btnZoomOut: TBitBtn;
    btnZoomReset: TBitBtn;
    ImageCover: TImage;
    pnlSize: TPanel;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnZoomResetClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);
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

procedure TfCover.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfCover.FormShow(Sender: TObject);
begin
  SetSize(CurrHeight, CurrWidth);
end;

procedure TfCover.ScrollBox1Resize(Sender: TObject);
begin
  SetSize(CurrHeight, CurrWidth);
end;

procedure TfCover.SetSize(imageHeigth, imageWidth: integer);
begin
  CurrHeight:= imageHeigth;
  CurrWidth := imageWidth;

  ImageCover.Height := imageHeigth;
  ImageCover.Width := imageWidth;
  if imageHeigth <= ScrollBox1.ClientHeight then
    ImageCover.Top:= (ScrollBox1.ClientHeight - imageHeigth) div 2
  else
    ImageCover.Top:= 0;
  if imageWidth <= ScrollBox1.ClientWidth then
    ImageCover.left:= (ScrollBox1.ClientWidth - imageWidth) div 2
  else
    ImageCover.left:= 0;

end;

end.

