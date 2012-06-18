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
  StdCtrls, ButtonPanel, Spin, ComCtrls, Buttons, AudioTag, types,
  ImageTrack, BaseTag, contnrs, FilesSupport, MediaLibrary;

type

  RSongInfo = record
    FileName: string;
    Tags: TCommonTags;
    MediaProperty: TMediaProperty;
    FileInfo: TFileInfo;
    ExtendedInfo: TExtendedInfo;
    ID: integer;
  end;

  ASongInfo = array of RSongInfo;

  { TfSongInfo }

  TfSongInfo = class(TForm)
    bNext: TBitBtn;
    bPrevious: TBitBtn;
    ButtonPanel1: TButtonPanel;
    cbAlbum: TCheckBox;
    cbAlbumArtist: TCheckBox;
    cbGenre: TCheckBox;
    cbComment: TCheckBox;
    cbYear: TCheckBox;
    cbTitle: TCheckBox;
    cbArtist: TCheckBox;
    cbTrack: TCheckBox;
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
    procedure lbFilesMouseLeave(Sender: TObject);
    procedure lbFilesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure lbFilesSelectionChange(Sender: TObject; User: boolean);
    procedure OKButtonClick(Sender: TObject);
  private
    fHint: THintWindow;
    fTagList: ASongInfo;
    fCount: integer;
    procedure LoadFromFile(FileName: TFileName; var Info: RSongInfo);
    procedure ShowFileInfo(Info: TFileInfo);
    procedure ShowLibraryInfo(Info: TExtendedInfo);
    procedure ShowMediaProperty(MediaProperty: TMediaProperty);
    procedure ShowTags(Tags: TCommonTags);
  public
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
    procedure InitFromList(FileNameS: TStrings);
    procedure InitFromFile(FileName: TFileName);
  end;

var
  fSongInfo: TfSongInfo;

implementation

{$R *.lfm}
uses AppConsts, GUIBackEnd;

{ TfSongInfo }

procedure TfSongInfo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  fHint.Free;
end;

procedure TfSongInfo.FormShow(Sender: TObject);
var
  i: integer;
begin
  if lbFiles.Count > 0 then
     lbFiles.Selected[0] := true;

  lbFilesSelectionChange(lbFiles, true);

  fHint := THintWindow.Create(Self);
  fHint.HideInterval := 3000;
  fHint.AutoHide := True;
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TLabel then
      if Tlabel(Components[i]).OptimalFill then
        Tlabel(Components[i]).AdjustFontForOptimalFill;

end;

procedure TfSongInfo.lbFilesMouseLeave(Sender: TObject);
begin
  fHint.hide;
end;

procedure TfSongInfo.lbFilesMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  item: integer;
  rec: TRect;
  p: Tpoint;
begin
  item := lbFiles.ItemAtPos(point(x, y), True);
  if (item > -1) then
  begin
    rec := fHint.CalcHintRect(Width, lbFiles.Items[item], nil);
    if rec.right > lbFiles.Width then
    begin
      p.x := x;
      p.y := y;
      p := lbFiles.ClientToScreen(p);
      OffsetRect(rec, p.x, p.y);
      fHint.ActivateHint(rec, lbFiles.Items[item]);
    end
    else
      fHint.hide;
  end;
end;

procedure TfSongInfo.lbFilesSelectionChange(Sender: TObject; User: boolean);
var
  ts: TTabSheet;
begin
  if lbFiles.SelCount = 1 then
  begin
    ts := pcSongInfo.ActivePage;
    leFileName.Caption := fTagList[lbFiles.ItemIndex].FileName;
    ShowTags(fTagList[lbFiles.ItemIndex].Tags);
    ShowFileInfo(fTagList[lbFiles.ItemIndex].FileInfo);
    ShowMediaProperty(fTagList[lbFiles.ItemIndex].MediaProperty);
    ShowLibraryInfo(fTagList[lbFiles.ItemIndex].ExtendedInfo);
    tsMediaProperty.TabVisible := True;
    pcSongInfo.ActivePage := ts;
  end
  else
  begin
    pcSongInfo.ActivePage := tsTags;
    tsMediaProperty.TabVisible := False;
  end;
end;

procedure TfSongInfo.OKButtonClick(Sender: TObject);
begin
  Close;
end;

constructor TfSongInfo.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
end;

destructor TfSongInfo.Destroy;
begin
  SetLength(fTagList, 0);
  inherited Destroy;

end;

procedure TfSongInfo.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfSongInfo.bPreviousClick(Sender: TObject);
begin
  if lbFiles.ItemIndex > 0 then
    lbFiles.ItemIndex := lbFiles.ItemIndex - 1;
end;

procedure TfSongInfo.bNextClick(Sender: TObject);
begin
  if lbFiles.ItemIndex < lbFiles.Count - 1 then
    lbFiles.ItemIndex := lbFiles.ItemIndex + 1;
end;


procedure TfSongInfo.LoadFromFile(FileName: TFileName; var Info: RSongInfo);
var
  FileObject: TTagReader;
  ID: integer;
begin
  Info.FileName := FileName;
  FileObject := GetFileTagsObject(FileName);
  info.Tags := ExtractTags(FileObject);
  info.MediaProperty := FileObject.MediaProperty;
  info.ID := backEnd.mediaLibrary.IDFromFullName(FileName);
  info.FileInfo := GetFileInfo(FileName);
  if id <> -1 then
    info.ExtendedInfo := BackEnd.mediaLibrary.InfoFromID(info.ID);

end;

procedure TfSongInfo.ShowMediaProperty(MediaProperty: TMediaProperty);
begin
  leBitRate.Caption := format('%d Kbps', [MediaProperty.BitRate]);
  leBPM.Caption := IntToStr(MediaProperty.BPM);
  leChannels.Caption := MediaProperty.ChannelMode;
  leSampling.Caption := format('%d Hz', [MediaProperty.Sampling]);
end;

procedure TfSongInfo.ShowFileInfo(Info: TFileInfo);
begin

  leSize.Caption := strByteSize(info.Size);
end;

procedure TfSongInfo.ShowLibraryInfo(Info: TExtendedInfo);
begin
  lePlayCount.Caption := IntToStr(info.PlayCount);
  leAdded.Caption := DateTimeToStr(Info.Added);
  if Info.LastPlay <> 0 then
    leLastPlayed.Caption := DateTimeToStr(Info.LastPlay)
  else
    leLastPlayed.Caption := rNever;

end;

procedure TfSongInfo.ShowTags(Tags: TCommonTags);
var
  int: integer;
begin
  leFileName.Caption := Tags.FileName;
  edArtist.Caption := Tags.Artist;
  edAlbum.Caption := Tags.Album;
  edAlbumArtist.Caption := Tags.AlbumArtist;
  edGenre.Caption := Tags.Genre;
  edTitle.Caption := Tags.Title;
  int := 0;
  TryStrToInt(Tags.Year, int);
  seYear.Value := int;
  seTrack.Value := Tags.Track;

end;

procedure TfSongInfo.InitFromFile(FileName: TFileName);
var
  st: TStringList;
begin
  st := TStringList.Create;
  st.Add(FileName);
  InitFromList(st);
  st.Free;

//  lbFiles.Visible := False;
  bNext.Visible := False;
  bPrevious.Visible := False;
end;


procedure TfSongInfo.InitFromList(FileNameS: TStrings);
var
  i: integer;
begin
  lbFiles.items.Clear;
  fCount := FileNameS.Count;
  SetLength(fTagList, fCount);

  for i := 0 to fCount - 1 do
  begin
    LoadFromFile(FileNameS[i], fTagList[i]);
    lbFiles.Items.Add(ExtractFileName(FileNameS[i]));
  end;
end;


end.
