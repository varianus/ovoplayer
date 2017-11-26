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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, ComCtrls, Buttons,
  {$IFDEF SUPPORT_LISTBOX_HINT}
  LCLIntf,
  {$ENDIF}

  AudioTag, BaseTag, FilesSupport, ExtendedInfo, MediaLibrary, GeneralFunc;

type

  RSongInfo = record
    FileName: string;
    TagReader: TTagReader;
    Tags: TCommonTags;
    Modified: TIDFieldsSet;
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
    edAlbum: TEdit;
    edAlbumArtist: TEdit;
    edArtist: TEdit;
    edGenre: TComboBox;
    edTitle: TEdit;
    GroupBox1: TGroupBox;
    imgCover: TImage;
    laArtist: TLabel;
    laAlbum: TLabel;
    laTitle: TLabel;
    laAlbumArtist: TLabel;
    laGenre: TLabel;
    laComment: TLabel;
    laYear: TLabel;
    laTrack: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    leFileName: TEdit;
    leLastPlayed: TLabel;
    leAdded: TLabel;
    leSize: TLabel;
    leSampling: TLabel;
    leDuration: TLabel;
    leBitRate: TLabel;
    leChannels: TLabel;
    lbFiles: TListBox;
    lePlayCount: TLabel;
    meComment: TMemo;
    pnlMoveSelection: TPanel;
    pcSongInfo: TPageControl;
    edTrack: TEdit;
    edYear: TEdit;
    tsMediaProperty: TTabSheet;
    tsTags: TTabSheet;

    procedure bNextClick(Sender: TObject);
    procedure bPreviousClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure edAlbumArtistChange(Sender: TObject);
    procedure edAlbumChange(Sender: TObject);
    procedure edArtistChange(Sender: TObject);
    procedure edGenreChange(Sender: TObject);
    procedure edTitleChange(Sender: TObject);
    procedure edTrackChange(Sender: TObject);
    procedure edYearChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbFilesMouseLeave(Sender: TObject);
    procedure lbFilesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure lbFilesSelectionChange(Sender: TObject; User: boolean);
    procedure meCommentChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    fUpdating: boolean;
    {$IFDEF SUPPORT_LISTBOX_HINT}
    fHint: THintWindow;
    {$ENDIF}
    fTagList: ASongInfo;
    fOriginalTag: ACommonTags;
    fCount: integer;
    fCombinedTags: TCommonTags;
    fCombinedUpdateable: boolean;
    fCombinedFlags: TIDFieldsSet;
    fCombinedModified: TIDFieldsSet;

    procedure CheckModified(Field: TIDFields; AText: string);
    procedure CombineTags;
    procedure LoadFromFile(FileName: TFileName; var Info: RSongInfo);
    procedure ShowFileInfo(Info: TFileInfo);
    procedure ShowLibraryInfo(Info: TExtendedInfo);
    procedure ShowMediaProperty(MediaProperty: TMediaProperty);
    procedure ShowTags(Tags: TCommonTags; Modified: TIDFieldsSet);
    procedure ShowCombinedTags;
    procedure UpdateHiglighting(Modified: TIDFieldsSet);
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
  uses AppConsts, GUIBackEnd, ID3v1Genres;

{ TfSongInfo }

procedure TfSongInfo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;

  {$IFDEF SUPPORT_LISTBOX_HINT}
  fHint.Free;
  {$ENDIF}

end;

procedure TfSongInfo.FormCreate(Sender: TObject);
var
  i: integer;
begin
  edGenre.Items.BeginUpdate;
  for i := 0 to ID3_MaxGenreExtended do
    edGenre.Items.Add(v1Genres[i]);
  edGenre.Items.EndUpdate;
end;

procedure TfSongInfo.FormDestroy(Sender: TObject);
var
  i:Integer;
begin
  for i := 0 to lbFiles.Count -1 do
    begin
      if Assigned(fTagList[i].TagReader) then
         begin
           FreeAndNil(fTagList[i].ExtendedInfo);
           fTagList[i].TagReader.Free;
         end;
    end;
end;

procedure TfSongInfo.FormShow(Sender: TObject);
//var
//  i: integer;
begin
  {$IFDEF SUPPORT_LISTBOX_HINT}
  fHint := THintWindow.Create(Self);
  fHint.HideInterval := 3000;
  fHint.AutoHide := True;
  {$ENDIF}
  //for i := 0 to ComponentCount - 1 do
  //  if Components[i] is TLabel then
  //    if Tlabel(Components[i]).OptimalFill then
  //      Tlabel(Components[i]).AdjustFontForOptimalFill;

end;

procedure TfSongInfo.lbFilesMouseLeave(Sender: TObject);
begin
  {$IFDEF SUPPORT_LISTBOX_HINT}
  fHint.hide;
  {$ENDIF}
end;

procedure TfSongInfo.lbFilesMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
{$IFDEF SUPPORT_LISTBOX_HINT}
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
{$ELSE}
begin
end;
{$ENDIF}

procedure TfSongInfo.lbFilesSelectionChange(Sender: TObject; User: boolean);
var
  ts: TTabSheet;
begin
  if lbFiles.SelCount = 1 then
  begin
    ts := pcSongInfo.ActivePage;
    leFileName.Caption := fTagList[lbFiles.ItemIndex].FileName;
    ShowTags(fTagList[lbFiles.ItemIndex].Tags, fTagList[lbFiles.ItemIndex].Modified);
    ShowFileInfo(fTagList[lbFiles.ItemIndex].FileInfo);
    ShowMediaProperty(fTagList[lbFiles.ItemIndex].MediaProperty);
    ShowLibraryInfo(fTagList[lbFiles.ItemIndex].ExtendedInfo);
    tsMediaProperty.TabVisible := True;
    pcSongInfo.ActivePage := ts;
  end
  else
  begin
    pcSongInfo.ActivePage := tsTags;
    CombineTags;
    ShowCombinedTags;
    tsMediaProperty.TabVisible := False;
  end;
end;

procedure TfSongInfo.OKButtonClick(Sender: TObject);
var
  i: integer;
  idx: integer;
  currpos: integer;
  MustRestart: boolean;
begin
  for i := 0 to lbFiles.Count - 1 do
    if (fTagList[i].Modified <> []) and fTagList[i].TagReader.isUpdateable then
    begin
       idx := BackEnd.PlayList.FindByName(fTagList[i].FileName);
       MustRestart:= (idx = BackEnd.PlayList.ItemIndex) and BackEnd.AudioEngine.Playing; // if song is playing
       if MustRestart then
          begin
            currpos:=BackEnd.AudioEngine.Position div 1000;
            BackEnd.AudioEngine.Stop;
          end
       else
         Currpos :=0;

       fTagList[i].TagReader.SetCommonTags(fTagList[i].Tags);
       fTagList[i].TagReader.UpdateFile;
        if MustRestart then
           begin
             BackEnd.AudioEngine.Play(BackEnd.PlayList.Songs[idx], currpos);
           end;

        if (idx <> -1) then  // if song is in playlist
           begin
              BackEnd.PlayList.Songs[idx].SetTags(fTagList[i].Tags);
              BackEnd.SignalPlayListChange;
           end;


        if fTagList[i].ID <> -1 then  // if song is in media library
          begin
             BackEnd.mediaLibrary.Update(fTagList[i].ID, fTagList[i].Tags, fTagList[i].FileInfo);
          end;
    end;
  Close;
end;

procedure TfSongInfo.CombineTags;
var
  i: integer;
  First: boolean;
begin
  fCombinedUpdateable:= false;
  fCombinedFlags := [];
  fCombinedModified := [];
  first:=true;
  for i := 0 to lbFiles.Count - 1 do
  begin
    if not lbFiles.Selected[i] then
      Continue;
    fCombinedModified := fCombinedModified + fTagList[i].Modified;
    fCombinedUpdateable:= fCombinedUpdateable or fTagList[i].TagReader.isUpdateable;
    if First then
    begin
      fCombinedTags := fTagList[i].Tags;
      First := False;
    end
    else
    begin
      if not fTagList[i].TagReader.isUpdateable then
         Continue;
      if fTagList[i].Tags.Album <> fCombinedTags.Album then
        Include(fCombinedFlags, idAlbum);
      if fTagList[i].Tags.AlbumArtist <> fCombinedTags.AlbumArtist then
        Include(fCombinedFlags, idAlbumArtist);
      if fTagList[i].Tags.Artist <> fCombinedTags.Artist then
        Include(fCombinedFlags, idArtist);
      if fTagList[i].Tags.Comment <> fCombinedTags.Comment then
        Include(fCombinedFlags, idComment);
      if fTagList[i].Tags.Genre <> fCombinedTags.Genre then
        Include(fCombinedFlags, idGenre);
      if fTagList[i].Tags.Title <> fCombinedTags.Title then
        Include(fCombinedFlags, idTitle);
      if fTagList[i].Tags.TrackString <> fCombinedTags.TrackString then
        Include(fCombinedFlags, idTrack);
      if fTagList[i].Tags.Year <> fCombinedTags.Year then
        Include(fCombinedFlags, idYear);
    end;

  end;

end;

constructor TfSongInfo.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
end;

destructor TfSongInfo.Destroy;
begin
  SetLength(fTagList, 0);
  SetLength(fOriginalTag, 0);
  inherited Destroy;

end;

procedure TfSongInfo.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfSongInfo.edAlbumArtistChange(Sender: TObject);
begin
  CheckModified(idAlbumArtist, edAlbumArtist.Caption);
end;

procedure TfSongInfo.edAlbumChange(Sender: TObject);
begin
  CheckModified(idAlbum, edAlbum.Caption);
end;

procedure TfSongInfo.edArtistChange(Sender: TObject);
begin
  CheckModified(idArtist, edArtist.Caption);
end;

procedure TfSongInfo.edGenreChange(Sender: TObject);
begin
  CheckModified(idGenre, edGenre.Caption);
end;

procedure TfSongInfo.edTitleChange(Sender: TObject);
begin
  CheckModified(idTitle, edTitle.Caption);
end;

procedure TfSongInfo.edTrackChange(Sender: TObject);
begin
  CheckModified(idTrack, edTrack.Caption);
end;

procedure TfSongInfo.edYearChange(Sender: TObject);
begin
  CheckModified(idYear, edYear.Caption);
end;

procedure TfSongInfo.FormActivate(Sender: TObject);
begin
  if lbFiles.Count > 0 then
    lbFiles.Selected[0] := True;

  lbFilesSelectionChange(lbFiles, True);

end;

procedure TfSongInfo.meCommentChange(Sender: TObject);
begin
  CheckModified(idComment, meComment.Lines.Text);
end;


procedure TfSongInfo.CheckModified(Field: TIDFields; AText: string);
var
  i: integer;
  SelCount: integer;
begin
  if fUpdating then
    exit;
  SelCount := 0;
  for i := 0 to lbFiles.Count - 1 do
  begin
    if lbFiles.Selected[i] then
    begin
      Inc(SelCount);

      if aText = rMultipleValue then
        exit;

      if not fTagList[i].TagReader.isUpdateable then
         continue;

      if (aText <> GetTagByID(fOriginalTag[i], Field)) then
      begin
        Include(fTagList[i].Modified, Field);
        SetTagByID(fTagList[i].Tags, Field, aText);
      end
      else
      begin
        Exclude(fTagList[i].Modified, Field);
        SetTagByID(fTagList[i].Tags, Field, aText);
      end;
    end;
  end;
  if SelCount > 1 then
  begin
    CombineTags;
    ShowCombinedTags;
  end
  else
    ShowTags(fTagList[lbFiles.ItemIndex].Tags, fTagList[lbFiles.ItemIndex].Modified);
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
begin
  Info.FileName := FileName;
  info.TagReader := GetFileTagsObject(FileName);
  info.Tags := ExtractTags(info.TagReader);
  info.MediaProperty := info.TagReader.MediaProperty;
  info.ID := backEnd.mediaLibrary.IDFromFullName(FileName);
  info.FileInfo := GetFileInfo(FileName);
  if info.ID <> -1 then
    info.ExtendedInfo := BackEnd.mediaLibrary.InfoFromID(info.ID);

end;

procedure TfSongInfo.ShowMediaProperty(MediaProperty: TMediaProperty);
begin
  leBitRate.Caption := format('%d Kbps', [MediaProperty.BitRate]);
  leChannels.Caption := MediaProperty.ChannelMode;
  leSampling.Caption := format('%d Hz', [MediaProperty.Sampling]);
end;

procedure TfSongInfo.ShowFileInfo(Info: TFileInfo);
begin

  leSize.Caption := strByteSize(info.Size);
end;

procedure TfSongInfo.ShowLibraryInfo(Info: TExtendedInfo);
begin
  if Not Assigned(info) then
   exit;

  lePlayCount.Caption := IntToStr(info.PlayCount);
  leAdded.Caption := DateTimeToStr(Info.Added);
  if Info.LastPlay <> 0 then
    leLastPlayed.Caption := DateTimeToStr(Info.LastPlay)
  else
    leLastPlayed.Caption := rNever;

end;

procedure TfSongInfo.ShowTags(Tags: TCommonTags; Modified: TIDFieldsSet);
begin
  if fTagList[lbFiles.ItemIndex].TagReader.isUpdateable then
    begin
       edAlbum.ReadOnly := false;
       edAlbumArtist.ReadOnly := false;
       edArtist.ReadOnly := false;
       meComment.ReadOnly := false;
       edTitle.ReadOnly := false;
       edGenre.ReadOnly := false;
       edTrack.ReadOnly := false;
       edYear.ReadOnly := false;
    end
  else
    begin
       edAlbum.ReadOnly := true;
       edAlbumArtist.ReadOnly := true;
       edArtist.ReadOnly := true;
       meComment.ReadOnly := true;
       edTitle.ReadOnly := true;
       edGenre.ReadOnly := true;
       edTrack.ReadOnly := true;
       edYear.ReadOnly := true;
    end;
  fUpdating := True;
  leFileName.Caption := Tags.FileName;
  leDuration.Caption := FormatTimeRange(Tags.Duration);
  edArtist.Caption := Tags.Artist;
  edAlbum.Caption := Tags.Album;
  edAlbumArtist.Caption := Tags.AlbumArtist;
  edGenre.Caption := Tags.Genre;
  edTitle.Caption := Tags.Title;
  edYear.Caption := Tags.Year;
  edTrack.Caption := Tags.TrackString;
  meComment.Lines.Text := Tags.Comment;
  UpdateHiglighting(Modified);
  if Tags.HasImage then
     begin
       fTagList[lbFiles.ItemIndex].TagReader.Tags.Images[0].Image.Position:=0;
       fTagList[lbFiles.ItemIndex].TagReader.Tags.Images[0].Image.Position:=0;
       imgCover.Picture.LoadFromStream(fTagList[lbFiles.ItemIndex].TagReader.Tags.Images[0].image);
     end
  else
     imgCover.Picture.Clear;
  fUpdating := False;

end;


procedure TfSongInfo.ShowCombinedTags;
begin
  if fCombinedUpdateable then
    begin
       edAlbum.ReadOnly := False;
       edAlbumArtist.ReadOnly := False;
       edArtist.ReadOnly := False;
       meComment.ReadOnly := False;
       edGenre.ReadOnly := False;
       edTitle.ReadOnly := False;
       edTrack.ReadOnly := False;
       edYear.ReadOnly := False;
    end
  else
    begin
       edAlbum.ReadOnly := true;
       edAlbumArtist.ReadOnly := true;
       edArtist.ReadOnly := true;
       meComment.ReadOnly := true;
       edGenre.ReadOnly := true;
       edTitle.ReadOnly := true;
       edTrack.ReadOnly := true;
       edYear.ReadOnly := true;
    end;

  imgCover.Picture.Clear;

  fUpdating := True;
  leFileName.Caption := rMultipleValue;

  if idAlbum in fCombinedFlags then
    edAlbum.Caption := rMultipleValue
  else
    edAlbum.Caption := fCombinedTags.Album;

  if idAlbumArtist in fCombinedFlags then
    edAlbumArtist.Caption := rMultipleValue
  else
    edAlbumArtist.Caption := fCombinedTags.AlbumArtist;

  if idArtist in fCombinedFlags then
    edArtist.Caption := rMultipleValue
  else
    edArtist.Caption := fCombinedTags.Artist;

  if idComment in fCombinedFlags then
    meComment.Lines.Text := rMultipleValue
  else
    meComment.Lines.Text := fCombinedTags.Comment;

  if idGenre in fCombinedFlags then
    edGenre.Caption := rMultipleValue
  else
    edGenre.Caption := fCombinedTags.Genre;

  if idTitle in fCombinedFlags then
    edTitle.Caption := rMultipleValue
  else
    edTitle.Caption := fCombinedTags.Title;

  if idTrack in fCombinedFlags then
    edTrack.Caption := rMultipleValue
  else
    edTrack.Caption := fCombinedTags.TrackString;

  if idYear in fCombinedFlags then
    edYear.Caption := rMultipleValue
  else
    edYear.Caption := fCombinedTags.Year;

  UpdateHiglighting(fCombinedModified);
  fUpdating := False;
end;

procedure TfSongInfo.UpdateHiglighting(Modified: TIDFieldsSet);
begin

  if idAlbum in Modified then
  begin
    edAlbum.Font.Style := [fsBold];
    laAlbum.Font.Style := [fsBold];
  end
  else
  begin
    edAlbum.Font.Style := [];
    laAlbum.Font.Style := [];
  end;

  if idAlbumArtist in Modified then
  begin
    edAlbumArtist.Font.Style := [fsBold];
    laAlbumArtist.Font.Style := [fsBold];
  end
  else
  begin
    edAlbumArtist.Font.Style := [];
    laAlbumArtist.Font.Style := [];
  end;

  if idArtist in Modified then
  begin
    edArtist.Font.Style := [fsBold];
    laArtist.Font.Style := [fsBold];
  end
  else
  begin
    edArtist.Font.Style := [];
    laArtist.Font.Style := [];
  end;
  if idComment in Modified then
  begin
    meComment.Font.Style := [fsBold];
    laComment.Font.Style := [fsBold];
  end
  else
  begin
    meComment.Font.Style := [];
    laComment.Font.Style := [];
  end;
  if idGenre in Modified then
  begin
    edGenre.Font.Style := [fsBold];
    laGenre.Font.Style := [fsBold];
  end
  else
  begin
    edGenre.Font.Style := [];
    laGenre.Font.Style := [];
  end;

  if idTitle in Modified then
  begin
    edTitle.Font.Style := [fsBold];
    laTitle.Font.Style := [fsBold];
  end
  else
  begin
    edTitle.Font.Style := [];
    laTitle.Font.Style := [];
  end;

  if idTrack in Modified then
  begin
    edTrack.Font.Style := [fsBold];
    laTrack.Font.Style := [fsBold];
  end
  else
  begin
    edTrack.Font.Style := [];
    laTrack.Font.Style := [];
  end;

  if idYear in Modified then
  begin
    edYear.Font.Style := [fsBold];
    laYear.Font.Style := [fsBold];
  end
  else
  begin
    edYear.Font.Style := [];
    laYear.Font.Style := [];
  end;

end;

procedure TfSongInfo.InitFromFile(FileName: TFileName);
var
  st: TStringList;
begin
  st := TStringList.Create;
  st.Add(FileName);
  InitFromList(st);
  st.Free;

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
  SetLength(fOriginalTag, fCount);

  for i := 0 to fCount - 1 do
  begin
    LoadFromFile(FileNameS[i], fTagList[i]);
    fOriginalTag[i] := fTagList[i].Tags;
    lbFiles.Items.Add((ExtractFileName(FileNameS[i])));
  end;


end;


end.
