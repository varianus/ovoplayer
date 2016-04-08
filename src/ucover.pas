unit uCover;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, ExtDlgs;

type

  { TfCover }

  TfCover = class(TForm)
    btnSaveAs: TBitBtn;
    btnFitToScreen: TBitBtn;
    btnZoomIn: TBitBtn;
    btnZoomOut: TBitBtn;
    btnZoomReset: TBitBtn;
    ImageCover: TImage;
    pnlSize: TPanel;
    SavePictureDialog1: TSavePictureDialog;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    procedure btnFitToScreenClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
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

procedure TfCover.btnSaveAsClick(Sender: TObject);
begin
  if SavePictureDialog1.Execute then
    ImageCover.Picture.SaveToFile(SavePictureDialog1.FileName, ExtractFileExt(SavePictureDialog1.FileName));
end;

procedure TfCover.btnFitToScreenClick(Sender: TObject);
begin
  SetSize(ScrollBox1.ClientHeight, ScrollBox1.ClientWidth);
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

  StatusBar1.SimpleText:= format('%d x %d (%d%%)',[ImageCover.Picture.Width, ImageCover.Picture.Height,trunc((imageWidth/ImageCover.Picture.Width ) * 100)]);
end;

end.

