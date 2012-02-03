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
unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, types, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ComCtrls, Menus, ExtCtrls, Buttons, StdCtrls, Song, uOSD, playlist,
  AudioEngine, GUIBackEnd, Config, ThemedSlider, VirtualTrees,
  DefaultTranslator, Grids, EditBtn, ActnList;

type
  TSortFields = record
    F1: TTagKind;
    F2: TTagKind;
    F3: TTagKind;
  end;

const
  GroupCount = 4;
  ArrayGroup: array  [0 .. GroupCount -1] of TSortFields =
    ((F1: tkAlbumArtist; F2: tkAlbum; F3: tkSong),
     (F1: tkArtist;      F2: tkAlbum; F3: tkSong),
     (F1: tkYear;        F2: tkAlbum; F3: tkSong),
     (F1: tkGenre;       F2: tkAlbum; F3: tkSong));

type
  { TfMainForm }


  TfMainForm = class(TForm)
    actShowPLMediainfo: TAction;
    actShowAbout: TAction;
    ActShowPreferences: TAction;
    ActionList: TActionList;
    Artist:     TLabel;
    cbGroupBy:  TComboBox;
    edtFilter: TLabeledEdit;
    ePath: TDirectoryEdit;
    gbStats: TGroupBox;
    lbTime:     TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    mnuRestore: TMenuItem;
    MenuItem34: TMenuItem;
    mnuRefreshItems: TMenuItem;
    mnuEnqueueItems: TMenuItem;
    mnuPlayItems: TMenuItem;
    mnuFileInfo: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem31: TMenuItem;
    mnuInfo: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    mnuCollection: TMenuItem;
    MenuItem22: TMenuItem;
    mnuRemovePlaylist: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem4:  TMenuItem;
    MenuItem5:  TMenuItem;
    MenuItem6:  TMenuItem;
    MenuItem7:  TMenuItem;
    MenuItem8:  TMenuItem;
    MenuItem9:  TMenuItem;
    Panel1: TPanel;
    PlaylistMenu: TPopupMenu;
    pmdirectories: TPopupMenu;
    PlaylistTree: TTreeView;
    pnCollection: TPopupMenu;
    pnHeaderPlaylist: TPopupMenu;
    RepeatMenu: TPopupMenu;
    btnCloseCollectionStat: TSpeedButton;
    slVolume: TThemedSlider;
    sgStats: TStringGrid;
    btnFilterCancel: TSpeedButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    TrackBar: TThemedSlider;
    tsPlayList: TTabSheet;
    tbDirectory: TToolBar;
    ToolButton10: TToolButton;
    btnBackDir: TToolButton;
    btnForwardDir: TToolButton;
    tbRepeat: TToolButton;
    btnUpDir: TToolButton;
    btnHomeDir: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    TrayMenu:   TPopupMenu;
    Splitter1:  TSplitter;
    ToolButton5: TToolButton;
    Track:      TLabel;
    imgCover:   TImage;
    Album:      TLabel;
    Timer:      TTimer;
    Title:      TLabel;
    MainMenu:   TMainMenu;
    mnuLoadPlaylist: TMenuItem;
    mnuSavePlaylist: TMenuItem;
    mnuFile:    TMenuItem;
    pnlPlayInfo: TPanel;
    pnlPlaylist: TPanel;
    pcMain:     TPageControl;
    pnlCollection: TPanel;
    pnlMain:    TPanel;
    pnlControl: TPanel;
    ProgressBar: TProgressBar;
    tbMediaControl:   TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    TrayIcon:   TTrayIcon;
    tsCollection: TTabSheet;
    tsDirectory:  TTabSheet;
    lvPlayList: TVirtualStringTree;
    CollectionTree: TVirtualStringTree;
    FilesTree: TVirtualStringTree;
    procedure actShowAboutExecute(Sender: TObject);
    procedure actShowPLMediainfoExecute(Sender: TObject);
    procedure ActShowPreferencesExecute(Sender: TObject);
    procedure BackEndSongStart(Sender: TObject);
    procedure btnBackDirClick(Sender: TObject);
    procedure btnFilterCancelClick(Sender: TObject);
    procedure btnForwardDirClick(Sender: TObject);
    procedure btnHomeDirClick(Sender: TObject);
    procedure cbGroupByChange(Sender: TObject);
    procedure CollectionTreeDblClick(Sender: TObject);
    procedure CollectionTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: integer);
    procedure CollectionTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure edtFilterChange(Sender: TObject);
    procedure ePathAcceptDirectory(Sender: TObject; var Value: String);
    procedure ePathEditingDone(Sender: TObject);
    procedure FilesTreeDblClick(Sender: TObject);
    procedure FilesTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure FilesTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvPlayListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure lvPlayListDblClick(Sender: TObject);
    procedure lvPlayListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: integer);
    procedure lvPlayListGetPopupMenu(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
      var AskParent: Boolean; var xPopupMenu: TPopupMenu);
    procedure lvPlayListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure lvPlayListHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo
      );

    procedure lvPlayListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure lvPlayListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure lvPlayListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure lvPlayListPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure lvPlayListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure mnuRestoreClick(Sender: TObject);
    procedure mnuEnqueueItemsClick(Sender: TObject);
    procedure mnuFileInfoClick(Sender: TObject);
    procedure mnuInfoClick(Sender: TObject);
    procedure mnuPlayItemsClick(Sender: TObject);
    procedure mnuRemovePlaylistClick(Sender: TObject);
    procedure PlaylistMenuPopup(Sender: TObject);
    procedure pmdirectoriesPopup(Sender: TObject);
    procedure pnCollectionPopup(Sender: TObject);
    procedure pnHeaderPlaylistPopup(Sender: TObject);
    procedure slVolumeChange(Sender: TObject);
    procedure btnCloseCollectionStatClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure btnUpDirClick(Sender: TObject);
    procedure tbRepeatClick(Sender: TObject);
    procedure ToolButton14Click(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure TrackBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure TrackBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure TrayIconDblClick(Sender: TObject);
    procedure TrayIconMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PlaylistTreeDblClick(Sender: TObject);
    procedure tsDirectoryShow(Sender: TObject);
  private
    SeekIng:      boolean;
    fSourceNode, fTargetNode: PVirtualNode;
    CurrentCover: string;
    SortFields:   TSortFields;
    CurrentPath : string;
    PathHistory : TstringList;
    PathIndex : Integer;
    procedure ClearPanelInfo;
    procedure CollectionHandler(Enqueue: boolean);
    function FindNode(Index: Cardinal): PVirtualNode;
    procedure LoadDir(Path: string);
    procedure LoadTree;
    procedure MediaLibraryScanComplete(Sender: TObject; _Added, _Updated, _Removed, _Failed: integer);
    procedure OnLoaded(Sender: TObject);
    procedure OnMenuItemClick(Sender: TObject);
    procedure PlayListChange(Sender: TObject);
    procedure MediaLibraryScanBegin(Sender: TObject);
    procedure AdaptSize;
    function PrepareFields: string;
    function PrepareFilter: string;
    function PrepareImportFilter(Node: PVirtualNode): string;
    procedure ReloadPlayList;
    procedure OnEngineCommand(Sender: Tobject; Command : TEngineCommand);
    procedure OnExternalCommand(Sender: Tobject; Command : String);
    procedure RemoveSelectionFromPlaylist;

  public
    { public declarations }
  end;

var
  fMainForm: TfMainForm;

implementation

{$R *.lfm}
uses AppConsts, lclType, AudioTag, LCLProc, FilesSupport,
     uConfig, uMiniPlayer, uSongInfo, uAbout, baseTag;

type

  TSortRow = record
    Kind:      TTagKind;
    FieldName: string;
    ImageIndex: integer;
  end;

const
  SortArray: array [tkAlbum..tkGenre] of TSortRow =
    ((Kind: tkAlbum; FieldName: 'Album'; ImageIndex: 1),
    (Kind: tkAlbumArtist; FieldName: 'AlbumArtist'; ImageIndex: 0),
    (Kind: tkArtist; FieldName: 'Artist'; ImageIndex: 0),
    (Kind: tkSong; FieldName: 'Title'; ImageIndex: 2),
    (Kind: tkYear; FieldName: 'Year'; ImageIndex: 3),
    (Kind: tkGenre; FieldName: 'Genre'; ImageIndex: 4)
    );

type

  PNodeData = ^TNodeData;
  TNodeData = record
    ID:      integer;
    Caption: string;
    Kind:    TTagKind;
  end;

  PFileData = ^TFileData;
  TFileData = record
    FullPath: string;
    isDir:    boolean;
  end;

{ TfMainForm }
procedure TfMainForm.CollectionTreeDblClick(Sender: TObject);
begin
  CollectionHandler(true);
end;

procedure TfMainForm.CollectionHandler(Enqueue:boolean);
var
  Node:     PVirtualNode;
  nodeData: PNodeData;
  aSong:    TSong;

begin
  Node := CollectionTree.GetFirstSelected;
  if Node = nil then
    exit;

  if not Enqueue then
     BackEnd.actPlayListClear.Execute;

  nodeData := CollectionTree.GetNodeData(Node);
  case nodeData^.Kind of
    tkSong:
      begin
      aSong := TSong.Create(BackEnd.mediaLibrary.FullNameFromID(nodeData^.ID));
      BackEnd.PlayList.Add(ASong);
      end
    else
      BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList, PrepareImportFilter(Node));
    end;
  OnLoaded(lvPlayList);

  if not Enqueue and (BackEnd.PlayList.Count > 0) then
     begin
       BackEnd.PlayList.ItemIndex:=0;
       BackEnd.actPlay.Execute;
     end;
end;

procedure TfMainForm.CollectionTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: integer);
var
  NodeData: PNodeData;
begin
  NodeData   := Sender.GetNodeData(Node);
  ImageIndex := SortArray[NodeData^.Kind].ImageIndex;
end;

procedure TfMainForm.CollectionTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData: PNodeData;
begin
  NodeData := CollectionTree.GetNodeData(Node);
  CellText := NodeData^.Caption;
  if CellText = '' then
     CellText := rEmptyTag;
end;

procedure TfMainForm.edtFilterChange(Sender: TObject);
begin
  LoadTree;
end;

procedure TfMainForm.ePathAcceptDirectory(Sender: TObject; var Value: String);
begin
 LoadDir(Value);
end;

procedure TfMainForm.ePathEditingDone(Sender: TObject);
begin
  LoadDir(ePath.Directory);
end;

procedure TfMainForm.FilesTreeDblClick(Sender: TObject);
var
  Data: PFileData;
  Node: PVirtualNode;
begin
  node := FilesTree.GetFirstSelected;
  if node = nil then
     exit;

  Data:=FilesTree.GetNodeData(Node);
  if Data^.isDir then
     LoadDir(Data^.FullPath)
  else
     begin
       BackEnd.PlayList.EnqueueFile(Data^.FullPath);
       ReloadPlayList;
     end;

end;

procedure TfMainForm.FilesTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data:PFileData;
begin
  Data:=FilesTree.GetNodeData(Node);
  if (Data^.isDir) then
    ImageIndex := 7
  else
    ImageIndex := 2;

end;

procedure TfMainForm.FilesTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data:PFileData;
begin
  Data:=FilesTree.GetNodeData(Node);
  CellText := ExtractFileName(Data^.FullPath);
end;

procedure TfMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if BackEnd.Config.InterfaceParam.MinimizeOnClose then
    begin
      Application.ShowMainForm:=False;
      CloseAction:=caHide;
    end
  else
    begin
      CloseAction:=caFree;
    end;

end;

procedure TfMainForm.BackEndSongStart(Sender: TObject);
var
  Song:    Tsong;
  imgName: string;
  id:integer;
  f:TTagReader;
  imgLoaded : boolean;

begin
  pnlPlayInfo.visible:=true;
  song := BackEnd.PlayList.CurrentItem;
  lvPlayList.ScrollIntoView(FindNode(BackEnd.PlayList.ItemIndex), true);

  Title.Caption     := Song.tags.Title;
  track.Caption     := Song.Tags.TrackString;
  Album.Caption     := Song.Tags.Album;
  Artist.Caption    := Song.Tags.Artist;
  TrackBar.Max      := Song.Tags.Duration;
  TrackBar.Position := BackEnd.AudioEngine.Position;
  TrayIcon.Hint     := Song.tags.Title + LineEnding + Song.Tags.Artist;

  if TrayIcon.Hint = '' then
     TrayIcon.Hint := Song.FileName;

  imgloaded := false;
  if Song.Tags.HasImage then
     begin
       CurrentCover := '';
       f := GetFileTagsObject(Song.Tags.FileName);
       f.Tags.Images[0].image.Position:=0;

       f.Tags.Images[0].image.Position:=0;
       try
          imgCover.Picture.LoadFromStream(f.Tags.Images[0].image);
          imgloaded:= true;
       Except
       end;
       f.Free;
     end;

  if not imgLoaded then
     begin
      imgName := BackEnd.GetImageFromfolder(IncludeTrailingPathDelimiter(Song.FilePath));
      if (imgName <> '') and (imgName <> CurrentCover) then
        begin
          imgCover.Picture.LoadFromFile(imgName);
          CurrentCover := imgName;
        end;
     end;

  if BackEnd.Config.NotificationParam.Kind = npkOSD then
    ShowOSD(BackEnd.PlayList.CurrentItem, imgCover.Picture);

  if BackEnd.Config.NotificationParam.Kind = npkNotifications then
    begin
    TrayIcon.BalloonTimeout := BackEnd.Config.NotificationParam.TimeOut;
    TrayIcon.BalloonTitle   := Song.tags.Title;
    TrayIcon.BalloonHint    := Song.Tags.Album + LineEnding + Song.Tags.Artist + LineEnding +
      Song.Tags.TrackString;
    if trim(TrayIcon.BalloonHint) = '' then
         TrayIcon.BalloonHint := Song.FileName;

    TrayIcon.ShowBalloonHint;
    end;

  id:= BackEnd.mediaLibrary.IDFromFullName(BackEnd.PlayList.CurrentItem.FullName);
  if id > -1 then
     BackEnd.mediaLibrary.SetSongPlayed(ID);

end;

procedure TfMainForm.ActShowPreferencesExecute(Sender: TObject);
var
  TheForm: TfConfig;
begin
  TheForm := TfConfig.Create(Self);
  TheForm.Show;
end;

procedure TfMainForm.actShowAboutExecute(Sender: TObject);
var
  theForm : TfAbout;
begin
  theForm:= TfAbout.create(application);
  theForm.ShowModal;
end;

procedure TfMainForm.actShowPLMediainfoExecute(Sender: TObject);
var
  info : TfSongInfo;
  Node: PVirtualNode;
  fileList: TStringList;
begin
  Node:=lvPlayList.GetFirstSelected;
  if node = nil then
     exit;
  if lvPlayList.SelectedCount = 1 then
      begin
        info := TfSongInfo.Create(Application);
        info.InitFromFile(BackEnd.PlayList[Node^.Index].FullName);
        Info.show;
       end
  else
     begin
       fileList := TStringList.Create;
       try
          Node:=lvPlayList.GetFirstSelected;
          while node <> nil do
            begin
              fileList.Add(BackEnd.PlayList[Node^.Index].FullName);
              Node:=lvPlayList.GetNextSelected(Node);
            end;
         info := TfSongInfo.Create(Application);
         info.InitFromList(FileList);
         Info.show;

       finally
         FreeAndNil(FileList);
       end;

    end;
end;

procedure TfMainForm.btnBackDirClick(Sender: TObject);
begin
  dec(PathIndex);
  if PathIndex < 0 then
     PathIndex := 0;

  LoadDir(PathHistory[PathIndex]);
end;

procedure TfMainForm.btnFilterCancelClick(Sender: TObject);
begin
  edtFilter.Clear;
end;

procedure TfMainForm.btnForwardDirClick(Sender: TObject);
begin
  inc(PathIndex);
  if PathIndex > PathHistory.Count -1 then
     PathIndex := PathHistory.Count -1 ;

  LoadDir(PathHistory[PathIndex]);

end;

procedure TfMainForm.btnHomeDirClick(Sender: TObject);
begin
  LoadDir(GetUserDir);
end;

procedure TfMainForm.cbGroupByChange(Sender: TObject);
begin
  SortFields.F1 := ArrayGroup[cbGroupBy.ItemIndex].F1;
  SortFields.F2 := ArrayGroup[cbGroupBy.ItemIndex].F2;
  SortFields.F3 := ArrayGroup[cbGroupBy.ItemIndex].F3;
  BackEnd.Config.InterfaceParam.GroupBy := cbGroupBy.ItemIndex;
  LoadTree;
end;

function TfMainForm.PrepareFields: string;
begin
  Result := format('%s, %s, %s', [SortArray[SortFields.F1].FieldName, SortArray[SortFields.F2].FieldName,
    SortArray[SortFields.F3].FieldName]);

end;

function TfMainForm.PrepareFilter: string;
var
  filter: string;
begin
  filter:= edtFilter.Text;
  if filter <> '' then
    Result := format(' %0:s like ''%%%3:s%%'''
                + ' or %1:s like ''%%%3:s%%'''
                + ' or %2:s like ''%%%3:s%%''',
               [SortArray[SortFields.F1].FieldName,
                SortArray[SortFields.F2].FieldName,
                SortArray[SortFields.F3].FieldName,
                filter])
  else
    Result := '';

end;


function TfMainForm.PrepareImportFilter(Node: PVirtualNode): string;
var
  NodeData: PNodeData;
  wrkNode:  PVirtualNode;
begin
  Result  := '0=0';
  wrkNode := Node;
  repeat
    NodeData := CollectionTree.GetNodeData(wrkNode);
    Result   := Result + format(' and %s = %s ', [SortArray[NodeData^.Kind].FieldName,
      QuotedStr(NodeData^.Caption)]);
    wrkNode  := wrkNode^.Parent;

  until wrkNode^.Index = 0;

end;


procedure TfMainForm.LoadTree;
var
  tags:     TCommonTags;
  L1Key, L2Key: string;
  NodeData: PNodeData;
  L1Node, L2Node, L3Node: PVirtualNode;

const
  FakeValue: string = '!"£$%&/()=?=)(/&%$£"!';
begin
  CollectionTree.Clear;
  L1Key := FakeValue;
  L2Key := FakeValue;

  BackEnd.mediaLibrary.ReadBegin(PrepareFields, PrepareFilter);

  while not BackEnd.mediaLibrary.ReadComplete do
    begin
    Tags := BackEnd.mediaLibrary.ReadItem;
//    BackEnd.mediaLibrary.ReadItem(tags);
    if UpperCase(TagValue(Tags, SortFields.F1)) <> L1Key then
       begin
         L1Node   := CollectionTree.AddChild(nil);
         L1Key    := UpperCase(TagValue(Tags, SortFields.F1));
         NodeData := CollectionTree.GetNodeData(L1Node);
         NodeData^.Caption := TagValue(Tags, SortFields.F1);
         NodeData^.Kind := SortFields.F1;
         L2Key    := FakeValue;
       end;

    if UpperCase(TagValue(Tags, SortFields.F2)) <> L2Key then
       begin
         L2Node   := CollectionTree.AddChild(L1Node);
         L2Key    := UpperCase(TagValue(Tags, SortFields.F2));
         NodeData := CollectionTree.GetNodeData(L2Node);
         NodeData^.Caption := TagValue(Tags, SortFields.F2);
         NodeData^.Kind := SortFields.F2;
       end;

    L3Node   := CollectionTree.AddChild(L2Node);
    NodeData := CollectionTree.GetNodeData(L3Node);
    NodeData^.Caption := TagValue(Tags, SortFields.F3);
    NodeData^.Kind := SortFields.F3;
    NodeData^.ID := tags.ID;
    Finalize(Tags);
    BackEnd.mediaLibrary.NextItem;
    end;
end;

procedure TfMainForm.MediaLibraryScanComplete(Sender: TObject; _Added, _Updated, _Removed, _Failed : integer);
begin
  sgStats.Clean;
  sgStats.RowCount := 4;
  sgStats.Cells[0,0] := rAddedTrack;   sgStats.Cells[1,0] := IntToStr(_Added);
  sgStats.Cells[0,1] := rUpdatedTrack; sgStats.Cells[1,1] := IntToStr(_Updated);
  sgStats.Cells[0,2] := rRemovedTrack; sgStats.Cells[1,2] := IntToStr(_Removed);
  sgStats.Cells[0,3] := rFailedTrack;  sgStats.Cells[1,3] := IntToStr(_Failed);

  gbStats.Visible := true;
  Application.ProcessMessages;
  LoadTree;

end;

procedure TfMainForm.FormCreate(Sender: TObject);
var
  tmpIcon : TIcon;
  tmpSize : TSize;
begin

  PathHistory := TStringList.Create;
  PathHistory.Duplicates := dupIgnore;

  TrackBar.Position:= 0;

  SortFields.F1 := tkAlbumArtist;
  SortFields.F2 := tkAlbum;
  SortFields.F3 := tkSong;

  CurrentPath:=EmptyStr;
  ClearPanelInfo;

  BackEnd.OnPlayListChange := @PlayListChange;
  BackEnd.AudioEngine.OnSongStart := @BackEndSongStart;

  BackEnd.OnPlayListLoad  := @OnLoaded;
  BackEnd.OnEngineCommand := @OnEngineCommand;
  BackEnd.OnExternalCommand := @OnExternalCommand;

  BackEnd.mediaLibrary.OnScanComplete := @MediaLibraryScanComplete;
  BackEnd.mediaLibrary.OnScanStart:=@MediaLibraryScanBegin;

  slVolume.Position := BackEnd.Config.EngineParam.Volume;
  slVolume.Max:= BackEnd.AudioEngine.MaxVolume;

  CollectionTree.NodeDataSize := SizeOf(TNodeData);
  FilesTree.NodeDataSize := SizeOf(TFileData);

  lvPlayList.NodeDataSize := SizeOf(PSong);

  case TplRepeat(BackEnd.Config.PlayListParam.RepeatMode) of
    rptNone : BackEnd.actRepeatNone.Checked := true;
    rptTrack : BackEnd.actRepeatTrack.Checked := true;
    rptAlbum : BackEnd.actRepeatAlbum.Checked := true;
    rptPlayList : BackEnd.actRepeatAll.Checked := true;
  end;

  try
    if FileExists(Backend.Config.ConfigDir + LASTPLAYLISTNAME) then
       BackEnd.Manager.ImportFromXSPF(Backend.Config.ConfigDir + LASTPLAYLISTNAME, BackEnd.PlayList)
    else
       BackEnd.PlayList.clear;
    OnLoaded(self);
    if (BackEnd.Manager.SavedTime <> 0) and
       BackEnd.Config.PlayListParam.Restart and
       (BackEnd.PlayList.CurrentItem <> nil) then
      begin
         BackEnd.AudioEngine.Play(BackEnd.PlayList.CurrentItem, BackEnd.Manager.SavedTime);
         Application.ProcessMessages;
         lvPlayList.ScrollIntoView(FindNode(BackEnd.PlayList.ItemIndex), true);
      end;
  except
     BackEnd.PlayList.clear;
  end;

  if BackEnd.Config.InterfaceParam.ShowTrayIcon then
    begin
       tmpIcon:=TIcon.Create;
       tmpIcon.LoadFromResourceName(HINSTANCE,'MAINICON');
       tmpSize.cx:=TrayIcon.Canvas.Width;
       tmpSize.cy:=TrayIcon.Canvas.Height;
       tmpIcon.Current:=tmpIcon.GetBestIndexForSize(tmpsize);
       TrayIcon.Icon.Transparent:=true;
       TrayIcon.Icon.Assign(tmpIcon);
       tmpIcon.free;
    end;

  if BackEnd.Config.InterfaceParam.ShowTrayIcon then
     begin
       Application.ShowMainForm := True;
       TrayIcon.Visible := True;
     end
  else
     begin
       Application.ShowMainForm := False;
       TrayIcon.Visible := false;
     end;

  slVolume.Position := BackEnd.AudioEngine.MainVolume;

  lvPlayList.Header.Options := lvPlayList.Header.Options + [hoAutoResize];
  lvPlayList.Header.AutoSizeIndex := 2;

  cbGroupBy.ItemIndex := BackEnd.Config.InterfaceParam.GroupBy;
  cbGroupBy.OnChange(self);
end;

procedure TfMainForm.FormDestroy(Sender: TObject);
begin
  PathHistory.Free;
end;

procedure TfMainForm.FormHide(Sender: TObject);
begin
  Application.ShowMainForm:=False;
end;

procedure TfMainForm.FormResize(Sender: TObject);
begin
  AdaptSize;
end;

procedure TfMainForm.FormShow(Sender: TObject);
begin
  lvPlayList.ScrollIntoView(FindNode(BackEnd.PlayList.ItemIndex), False);

  if (BackEnd.AudioEngine = nil) or (BackEnd.AudioEngine.GetEngineName = 'dummy') then
     begin
       ShowMessage(rMissingConfig);
       ActShowPreferences.Execute;
     end;

end;

procedure TfMainForm.lvPlayListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if Node^.Index mod 2 <> 0 then
    begin
    TargetCanvas.Brush.Color := $00f2f2f2;
    TargetCanvas.FillRect(CellRect);
    end;
end;

procedure TfMainForm.lvPlayListDblClick(Sender: TObject);
var
  index: integer;
  node: PVirtualNode;
begin
  node := lvPlayList.GetFirstSelected;
  if Node = nil Then
    exit;
  index := Node^.Index;
  BackEnd.PlayList.ItemIndex := index;
  BackEnd.AudioEngine.Play(BackEnd.PlayList.CurrentItem);
  lvPlayList.Invalidate;
end;

procedure TfMainForm.lvPlayListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: integer);
begin
  if (Column = 1) and (Node^.Index = BackEnd.PlayList.ItemIndex) then
    ImageIndex := 6
  else
    ImageIndex := -1;
end;

procedure TfMainForm.lvPlayListGetPopupMenu(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
  var AskParent: Boolean; var xPopupMenu: TPopupMenu);
begin
  if lvPlayList.Header.InHeader(P)  then
     begin
      xPopupMenu := pnHeaderPlaylist;
     end
  else
     xPopupMenu := PlaylistMenu;
end;

procedure TfMainForm.lvPlayListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  ASong: TSong;
begin

  ASong := BackEnd.PlayList.Songs[node^.Index];
  if ASong = nil then
    exit;

  case Column of
    1: CellText := IntToStr(node^.Index + 1);
    2: CellText := ASong.Title;
    3: CellText := ASong.Tags.Album;
    4: CellText := TimeToStr(ASong.Tags.Duration / MSecsPerDay);
    5: CellText := ASong.Tags.TrackString;
    6: CellText := ASong.Tags.Genre;
    7: CellText := ASong.Tags.Year;
    8: CellText := ASong.Tags.AlbumArtist;
    9: CellText := ASong.FileName;
   10: CellText := '5';
  end;

end;

procedure TfMainForm.lvPlayListHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
var
  SortField: TplSortField;
  Direction:TplSortDirection;
begin
  if HitInfo.Column < 2 then exit;

  if HitInfo.Button = mbLeft then
    begin
      with Sender do
      begin
        if SortColumn <> HitInfo.Column then
        begin
          SortColumn := HitInfo.Column;
          SortDirection := sdAscending;
        end
        else
        case SortDirection of
          sdAscending:
            SortDirection := sdDescending;
          sdDescending:
            SortColumn := NoColumn;
        end;
      end;
    end
  else
    exit;

  case HitInfo.Column  of
    2 : SortField := stTitle;
    3 : SortField := StAlbum;
    4 : SortField := stDuration;
    5 : SortField := stTrack;
    6 : SortField := stGenre ;
    7 : SortField := stYear;
    8 : SortField := stAlbumArtist;
    9 : SortField := stFileName;
  else
    SortField:= stNone;
  end;

  if Sender.SortDirection = sdAscending then
    Direction:=sdplAscending
  else
    Direction:=sdplDiscending;

  BackEnd.PlayList.Sort(SortField, Direction);
  PlayListChange(Self);

end;

procedure TfMainForm.lvPlayListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if ssLeft in Shift then
     fSourceNode := lvPlayList.GetNodeAt(x, y - lvPlayList.Header.Height);

end;

procedure TfMainForm.lvPlayListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if fSourceNode = nil then
    exit;

  if not (ssLeft in shift) then
    exit;

  fTargetNode := lvPlayList.GetNodeAt(x, y - lvPlayList.Header.Height );

  if (fTargetNode = nil) or
     (fTargetNode = fSourceNode) then
    exit;

  BackEnd.PlayList.PushPos;
  BackEnd.PlayList.Swap(fTargetNode^.Index, fSourceNode^.Index);
  BackEnd.PlayList.PopPos;
  lvPlayList.ClearSelection;
  lvPlayList.Selected[fTargetNode] := true;

  lvPlayList.InvalidateNode(fTargetNode);
  lvPlayList.InvalidateNode(fSourceNode);
  lvPlayList.repaint;
  fSourceNode := fTargetNode;

end;

procedure TfMainForm.lvPlayListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin

  fSourceNode := nil;

end;

procedure TfMainForm.lvPlayListPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
begin
  if Node^.Index = BackEnd.PlayList.ItemIndex then
    begin
    TargetCanvas.Font.Style := [fsUnderline];
    end;
end;

procedure TfMainForm.PlayListChange(Sender: TObject);
begin
  ReloadPlayList;
  Invalidate;
end;

procedure TfMainForm.MediaLibraryScanBegin(Sender: TObject);
begin
  sgStats.Clean;
  sgStats.RowCount := 1;
  sgstats.cells[0,0]:=rBeginCollectionScan;
  gbStats.Visible:=True;
  Application.ProcessMessages;
end;

procedure TfMainForm.RemoveSelectionFromPlaylist;
var
  node: PVirtualNode;
begin
  Node:=lvPlayList.GetPreviousSelected(nil);
  while node <> nil do
    begin
      BackEnd.PlayList.Delete(Node^.Index);
      Node:=lvPlayList.GetPreviousSelected(Node);
    end;
   OnLoaded(nil);
end;


procedure TfMainForm.lvPlayListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    VK_DELETE:
       RemoveSelectionFromPlaylist;
    end;

end;

procedure TfMainForm.mnuRestoreClick(Sender: TObject);
begin
  Show;
end;

procedure TfMainForm.mnuEnqueueItemsClick(Sender: TObject);
begin
    CollectionHandler(true);
end;

procedure TfMainForm.mnuFileInfoClick(Sender: TObject);
var
  info : TfSongInfo;
  Node: PVirtualNode;
  Data : PFileData;
begin
  Node:=FilesTree.GetFirstSelected;
  if node = nil then
     exit;

  Data := FilesTree.GetNodeData(Node);
  if (Data <> nil) and (not Data^.isDir) then
    begin
      info := TfSongInfo.Create(Application);
      info.InitFromFile(Data^.FullPath);
      Info.show;
    end;
end;

procedure TfMainForm.mnuInfoClick(Sender: TObject);
var
  info : TfSongInfo;
  Node: PVirtualNode;
  Data : PNodeData;
  fileList: TStringList;
begin
  Node:=CollectionTree.GetFirstSelected;
  if node = nil then
     exit;
  if CollectionTree.SelectedCount = 1 then
     begin
  Data := CollectionTree.GetNodeData(Node);
  if (Data <> nil) and (Data^.Kind = tkSong) then
    begin
      info := TfSongInfo.Create(Application);
      info.InitFromFile(BackEnd.mediaLibrary.FullNameFromID(Data^.ID));
      Info.show;
          end;
     end
  else
     begin
       fileList := TStringList.Create;
       try
          Node:=CollectionTree.GetFirstSelected;
          while node <> nil do
            begin
              Data := CollectionTree.GetNodeData(Node);
              if (Data <> nil) and (Data^.Kind = tkSong) then
                 fileList.Add(BackEnd.mediaLibrary.FullNameFromID(Data^.ID));
              Node:=CollectionTree.GetNextSelected(Node);
            end;
         info := TfSongInfo.Create(Application);
         info.InitFromList(FileList);
         Info.show;

       finally
         FreeAndNil(FileList);
       end;

    end;
end;

procedure TfMainForm.mnuPlayItemsClick(Sender: TObject);
begin
    CollectionHandler(False);
end;

procedure TfMainForm.mnuRemovePlaylistClick(Sender: TObject);
begin
  RemoveSelectionFromPlaylist;
end;

procedure TfMainForm.PlaylistMenuPopup(Sender: TObject);
begin
  if lvPlayList.GetFirstSelected = nil then
     exit;
end;

procedure TfMainForm.pmdirectoriesPopup(Sender: TObject);
var
//  info : TfSongInfo;
  Node: PVirtualNode;
  Data : PFileData;
begin
  Node:=FilesTree.GetFirstSelected;
  if node = nil then
     exit;
  Data := CollectionTree.GetNodeData(Node);
  if (Data <> nil) then
    begin
      if not Data^.isDir then
         mnuFileInfo.Visible := true
      else
         mnuFileInfo.Visible := False;
    end;

end;

procedure TfMainForm.pnCollectionPopup(Sender: TObject);
var
  Node: PVirtualNode;
  Data : PNodeData;
begin
  Node:=CollectionTree.GetFirstSelected;
  if node = nil then
     exit;
  Data := CollectionTree.GetNodeData(Node);
  if (Data <> nil) then
    case Data^.Kind of
       tkSong : mnuInfo.Visible := true;
    else
       mnuInfo.Visible := False;
    end;

end;

procedure TfMainForm.OnMenuItemClick(Sender: TObject);
begin
  with TMenuItem(Sender), lvPlayList.Header.Columns.Items[Tag] do
  begin
    if Checked then
      Options := Options - [coVisible]
    else
      Options := Options + [coVisible];

  end;
  lvPlayList.Invalidate;
end;

procedure TfMainForm.pnHeaderPlaylistPopup(Sender: TObject);
var
  col :integer;
  item : TMenuItem;
begin
  pnHeaderPlaylist.Items.Clear;
  for col := 1 to lvPlayList.Header.Columns.Count - 1 do
    begin
      item := TMenuItem.Create(pnHeaderPlaylist);
      item.Caption:=lvPlayList.Header.Columns[col].Text;
      item.Tag := Col;
      item.Checked := coVisible in lvPlayList.Header.Columns[col].Options;
      item.OnClick := @OnMenuItemClick;
      pnHeaderPlaylist.Items.Add(item);
    end;

end;

procedure TfMainForm.slVolumeChange(Sender: TObject);
begin
  BackEnd.AudioEngine.MainVolume := slVolume.Position;
//
end;

procedure TfMainForm.btnCloseCollectionStatClick(Sender: TObject);
begin
  sgStats.Clean;
  sgStats.RowCount := 1;
  gbStats.Visible := false;
end;

procedure TfMainForm.TimerTimer(Sender: TObject);
begin

  if (BackEnd.AudioEngine.State = ENGINE_PLAY)  then
     begin
       if not seeking then
          TrackBar.Position := BackEnd.AudioEngine.Position;
     end;

  lbTime.Caption := TimeToStr(BackEnd.AudioEngine.Position / MSecsPerDay);

end;

procedure TfMainForm.btnUpDirClick(Sender: TObject);
begin
  LoadDir(UpperDirectory(CurrentPath));
end;

procedure TfMainForm.tbRepeatClick(Sender: TObject);
begin
  //
  RepeatMenu.PopUp;
end;

procedure TfMainForm.ToolButton14Click(Sender: TObject);
begin
  raise exception.create('aaa');
end;

procedure TfMainForm.TrackBarChange(Sender: TObject);
begin
  BackEnd.AudioEngine.Position := TrackBar.Position;
end;

procedure TfMainForm.TrackBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  newPosition: integer;
begin
  exit;
  if ssLeft in Shift then
    begin
      Seeking     := True;
      TrackBar.Cursor := crHSplit;
      newPosition := Round(x * TrackBar.Max / TrackBar.ClientWidth);
      TrackBar.Position := newPosition;
      BackEnd.AudioEngine.Position := newPosition;
    end
  else
    begin
    TrackBar.Cursor := crDefault;
    Seeking := False;
    end;

end;

procedure TfMainForm.TrackBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  SeekIng := False;
end;

procedure TfMainForm.TrayIconDblClick(Sender: TObject);
begin
  Show;
end;

procedure TfMainForm.TrayIconMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt : Tpoint;
begin
  if Button = mbMiddle then
    begin
      pt:=TrayIcon.GetPosition;
      if fMiniPlayer.Visible then
         fMiniPlayer.Hide
      else
        fMiniPlayer.ShowAtPos(pt.x,pt.y);
    end;
end;

procedure TfMainForm.PlaylistTreeDblClick(Sender: TObject);
var
  item: TTreeNode;
begin
  item := PlaylistTree.Selected;
  if item = nil then
     exit;
  case item.StateIndex of
     1: BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList,
        'playcount = 0');
     2: BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList,
        '', 'added limit 50');
     3: BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList,
        '', 'playcount desc limit 50');
     4: BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList,
        '','random() limit 50');
     5: BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList,
        '');
  end;
  ReloadPlayList;
end;

procedure TfMainForm.tsDirectoryShow(Sender: TObject);
begin
  if CurrentPath = EmptyStr then
     begin
       LoadDir(GetUserDir);
     end;
end;

procedure TfMainForm.LoadDir(Path:string);
var
  DirList: TstringList;
  FileList :TstringList;
  i: integer;
  node: PVirtualNode;
  data :PFileData;
begin
  DirList:=TStringList.Create;
  FileList:=TStringList.Create;
  CurrentPath:=IncludeTrailingPathDelimiter(Path);
  try
    FilesTree.Clear;
    BuildFolderList(CurrentPath, DirList, False);
    DirList.Sort;
    BuildFileList(IncludeTrailingPathDelimiter(CurrentPath) + AudioTag.SupportedExtension,
                   faAnyFile, FileList, False);
    FileList.Sort;
    for i := 0 to DirList.Count -1 do
      begin
        node:=FilesTree.AddChild(Nil);
        Data:=FilesTree.GetNodeData(Node);
        Data^.FullPath:=DirList[i];
        data^.isDir:=True;;
      end;

    for i := 0 to FileList.Count -1 do
      begin
        node:=FilesTree.AddChild(Nil);
        Data:=FilesTree.GetNodeData(Node);
        Data^.FullPath:=FileList[i];
        data^.isDir:=False;
      end;

  finally
    DirList.Free;;
    FileList.Free;
  end;
  ePath.Directory := Path;
  if PathHistory.IndexOf(Path) < 0 then
  PathIndex := PathHistory.Add(Path);

end;

procedure TfMainForm.OnLoaded(Sender: TObject);
begin
  ReloadPlayList;
end;

procedure TfMainForm.ReloadPlayList;
var
  I:     integer;
  Node:  PVirtualNode;
  ASong: PSong;
begin

  lvPlayList.BeginUpdate;
  lvPlayList.Clear;
  for i := 0 to BackEnd.PlayList.Count - 1 do
    begin
      node  := lvPlayList.AddChild(nil);
      ASong := lvPlayList.getNodeData(Node);
      ASong := PSong(BackEnd.PlayList.Songs[i]);
      if i = BackEnd.PlayList.ItemIndex then
         lvPlayList.Selected[Node]:=true;
    end;

  lvPlayList.EndUpdate;
  AdaptSize;

  lvPlayList.Invalidate;

end;

procedure TfMainForm.ClearPanelInfo;
var
  lFile: string;
begin
  Title.Caption     := rNotPlaying;
  Album.Caption     := '';
  Artist.Caption    := '';
  Track.Caption     := '';
  TrackBar.Position := 0;
  lbTime.caption    := TimeToStr(0);
  lFile := backend.Config.GetResourcesPath + 'logo.png';
  if not FileExists(lFile) then
    DebugLn('[TfMainForm.ClearPanelInfo] File not found: ' + lFile)
  else
    imgCover.Picture.LoadFromFile(lFile);
end;

procedure TfMainForm.OnEngineCommand(Sender: Tobject; Command: TEngineCommand);
begin
  case Command of
     ecStop : ClearPanelInfo;
  end;
end;

procedure TfMainForm.OnExternalCommand(Sender: Tobject; Command: String);
begin
  if Command = 'activate' then
     Show;
end;

procedure TfMainForm.AdaptSize;
begin
  lvPlayList.Header.AutoFitColumns(False, smaAllColumns);
end;

Function TfMainForm.FindNode(Index: cardinal): PVirtualNode;
var wrkNode: PVirtualNode;
begin
  result := nil;
  wrkNode := lvPlayList.GetFirst;
  while wrkNode <> nil do
    if wrkNode^.Index = index then
       begin
          Result := wrkNode ;
          exit;
       end
    else
      wrkNode := lvPlayList.GetNext(wrkNode);

end;

end.