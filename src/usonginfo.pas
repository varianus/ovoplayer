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
unit uSongInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, Spin, ComCtrls, Buttons, AudioTag,
  ImageTrack, BaseTag ;

type

  { TfSongInfo }

  TfSongInfo = class(TForm)
    bNext: TBitBtn;
    bPrevious: TBitBtn;
    ButtonPanel1: TButtonPanel;
    edAlbum: TEdit;
    edAlbumArtist: TEdit;
    edArtist: TEdit;
    edGenre: TEdit;
    edTitle: TEdit;
    GroupBox1: TGroupBox;
    ImageTrack: TImageTrack;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    leLastPlayed: TLabel;
    leFileName: TLabel;
    leAdded: TLabel;
    leSize: TLabel;
    leSampling: TLabel;
    leDuration: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    leBPM: TLabel;
    leBitRate: TLabel;
    leChannels: TLabel;
    lbFiles: TListBox;
    lePlayCount: TLabel;
    meComment: TMemo;
    pnlMoveSelection: TPanel;
    pcSongInfo: TPageControl;
    seTrack: TSpinEdit;
    seYear: TSpinEdit;
    tsMediaProperty: TTabSheet;
    tsTags: TTabSheet;

    procedure bNextClick(Sender: TObject);
    procedure bPreviousClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure lbFilesSelectionChange(Sender: TObject; User: boolean);
    procedure OKButtonClick(Sender: TObject);
  private
    FList : TStringList;
    Procedure LoadFromFile(FileName:TFileName);
    procedure LoadFromFileInfo(FileName: TFileName);
    procedure LoadFromLibrary(ID: Integer);
    procedure LoadFromMediaProperty(MediaProperty: TMediaProperty);
    procedure LoadFromTags(Tags: TCommonTags);
  public
    constructor Create(Aowner: Tcomponent); override;
    Destructor Destroy; override;
    Procedure InitFromList(FileNameS:TStrings);
    procedure InitFromFile(FileName: TFileName);
  end;

var
  fSongInfo: TfSongInfo;

implementation
{$R *.lfm}
uses AppConsts, FilesSupport, GUIBackEnd, MediaLibrary;

{ TfSongInfo }

procedure TfSongInfo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfSongInfo.FormShow(Sender: TObject);
var
  i:Integer;
begin
  for i := 0 to ComponentCount -1 do
     if Components[i] is TLabel then
       if Tlabel (Components[i]).OptimalFill then
          Tlabel (Components[i]).AdjustFontForOptimalFill;

end;

procedure TfSongInfo.lbFilesSelectionChange(Sender: TObject; User: boolean);
var
  ts: TTabSheet;
begin
 if lbFiles.SelCount = 1 then
    begin
       ts:= pcSongInfo.ActivePage;
       LoadFromFile(FList[lbFiles.ItemIndex]);
       tsMediaProperty.TabVisible:= true;
       pcSongInfo.ActivePage := ts;
    end
 else
    begin
      pcSongInfo.ActivePage:= tsTags;
      tsMediaProperty.TabVisible:= False;
    end;
end;

procedure TfSongInfo.OKButtonClick(Sender: TObject);
begin
    Close;
end;

constructor TfSongInfo.Create(Aowner: Tcomponent);
begin
  inherited Create(Aowner);
  FList:= TStringList.Create;
end;

destructor TfSongInfo.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;

end;

procedure TfSongInfo.CancelButtonClick(Sender: TObject);
begin
    Close;
end;

procedure TfSongInfo.bPreviousClick(Sender: TObject);
begin
  if lbFiles.ItemIndex > 0 then
     lbFiles.ItemIndex:=lbFiles.ItemIndex -1;
end;

procedure TfSongInfo.bNextClick(Sender: TObject);
begin
 if lbFiles.ItemIndex < lbFiles.Count -1 then
    lbFiles.ItemIndex:=lbFiles.ItemIndex + 1;
end;


procedure TfSongInfo.LoadFromFile(FileName: TFileName);
var
  FileObject : TTagReader;
  ID :Integer;
begin
  FileObject := GetFileTagsObject(FileName);
  LoadFromTags(ExtractTags(FileObject));
  LoadFromMediaProperty(FileObject.MediaProperty);
  LoadFromFileInfo(FileName);
  ID := backEnd.mediaLibrary.IDFromFullName(FileName);
  if id <> -1 then
     LoadFromLibrary(ID);
  leFileName.Caption := FileName;

end;

procedure TfSongInfo.LoadFromMediaProperty(MediaProperty: TMediaProperty);
begin
  leBitRate.Caption := format('%d Kbps',[MediaProperty.BitRate]);
  leBPM.Caption := IntTostr(MediaProperty.BPM);
  leChannels.Caption := MediaProperty.ChannelMode;
  leSampling.Caption := format('%d Hz',[MediaProperty.Sampling]);
end;

procedure TfSongInfo.LoadFromFileInfo(FileName: TFileName);
var
  info:TFileInfo;
begin
  info:=GetFileInfo(FileName);
  leSize.Caption := strByteSize(info.Size);
end;

procedure TfSongInfo.LoadFromLibrary(ID: Integer);
var
  Info: TExtendedInfo;
begin
  info := BackEnd.mediaLibrary.InfoFromID(ID);
  lePlayCount.Caption := IntToStr(info.PlayCount);
  leAdded.Caption := DateTimeToStr(Info.Added);
  if Info.LastPlay <> 0 then
     leLastPlayed.Caption := DateTimeToStr(Info.LastPlay)
  else
     leLastPlayed.Caption := rNever;


end;

procedure TfSongInfo.LoadFromTags(Tags: TCommonTags);
var int:Integer;
begin
  leFileName.Caption := Tags.FileName;
  edArtist.Caption := Tags.Artist;
  edAlbum.Caption := Tags.Album;
  edAlbumArtist.Caption := Tags.AlbumArtist;
  edGenre.Caption := Tags.Genre;
  edTitle.Caption := Tags.Title;
  int:=0;
  TryStrToInt(Tags.Year,int);
  seYear.Value := int;

  seTrack.Value := Tags.Track;

end;

procedure TfSongInfo.InitFromFile(FileName: TFileName);
begin
  LoadFromFile(FileName);
  lbFiles.Visible:=false;
  bNext.Visible:=false;
  bPrevious.Visible:=false;
end;


procedure TfSongInfo.InitFromList(FileNameS: TStrings);
var i:Integer;
begin
  FList.clear;
  lbFiles.items.clear;
  FLIST.Assign(FileNames);
  for i := 0 to FList.Count - 1 do
     begin
       lbFiles.Items.Add(ExtractFileName(FList[i]));
     end;

  if lbFiles.Count > 0 then
     lbFiles.Selected[0]:= true;
end;


end.
