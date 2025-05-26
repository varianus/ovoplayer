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
unit uMain;

interface

uses
  Classes, types, SysUtils, lazFileUtils, Forms, Controls, Graphics,
  Dialogs, ComCtrls, Menus, ExtCtrls, Buttons, StdCtrls, Song, CustomSong, uOSD,
  BaseTypes, GUIBackEnd, Config, MediaLibrary, coreinterfaces,
  DefaultTranslator, Grids, EditBtn, ActnList,
  LCLIntf, Lmessages,
  {$IFDEF MPRIS2} Mpris2,{$ENDIF}
  {$IFDEF NOTIFYDBUS} notification,{$ENDIF}
  {$IFDEF TASKBAR_EXTENSION}taskbar_ext,{$ENDIF}
  {$IFDEF NETWORK_INTF}
      {$ifdef Network_ss} netinf,{$ENDIF}
      {$ifdef Network_Ws} netintfws,{$ENDIF}
  {$ENDIF}
  {$IFDEF MULTIMEDIA_KEYS}MultimediaKeys, {$ENDIF}
  {$IFDEF SCREEN_LOCK}screenlock,{$ENDIF}
  ucover, ucustomplaylist, playlistbuilder, netprotocol,
  GuiConfig, uequalizer, uscanresult, ThemedSlider, LCLType,
  MultiRowSelection;

type
  TSortFields = record
    F1: TTagKind;
    F2: TTagKind;
    F3: TTagKind;
  end;

const
  GroupCount = 4;
  ArrayGroup: array  [0 .. GroupCount - 1] of TSortFields =
    ((F1: tkAlbumArtist; F2: tkAlbum; F3: tkTrack),
    (F1: tkArtist; F2: tkAlbum; F3: tkTrack),
    (F1: tkYear; F2: tkAlbum; F3: tkTrack),
    (F1: tkGenre; F2: tkAlbum; F3: tkTrack));

type
  { TfMainForm }

  TMusicTreeNode = class(TTreeNode)
  public
    ID: integer;
    Kind: TTagKind;
    EmptyTag: boolean;
  end;

  TFileTreeNode = class(TTreeNode)
  public
    FullPath: string;
    isDir: boolean;
  end;

  { TPlayListTreeNode }

  TPlayListTreeNode = class(TTreeNode)
  public
    FullPath: string;
    Automatic: boolean;
  end;

  TfMainForm = class;
  { TPlaylistGuiParam }

  TPlaylistGuiParam = class(TConfigParam)
  private
    FForm: TfMainForm;
  protected
    procedure InternalSave; override;
  public
    procedure Load; override;
    constructor Create(aOwner: TConfig; Form: TfMainForm); reintroduce;
  end;

  { TMainFormParam }
  TMainFormParam = class(TConfigParam)
  private
    FActivePage: integer;
    FHeight: integer;
    FLeft: integer;
    FLeftPanelVisible: boolean;
    FTop: integer;
    FWidth: integer;
    FForm: TfMainForm;
    procedure SetActivePage(AValue: integer);
    procedure SetHeight(AValue: integer);
    procedure SetLeft(AValue: integer);
    procedure SetLeftPanelVisible(AValue: boolean);
    procedure SetTop(AValue: integer);
    procedure SetWidth(AValue: integer);
  protected
    procedure InternalSave; override;
  public
    property Height: integer read FHeight write SetHeight;
    property Width: integer read FWidth write SetWidth;
    property Top: integer read FTop write SetTop;
    property Left: integer read FLeft write SetLeft;
    property LeftPanelVisible: boolean read FLeftPanelVisible write SetLeftPanelVisible;
    property ActivePage: integer read FActivePage write SetActivePage;
    procedure Load; override;
    constructor Create(aOwner: TConfig; Form: TfMainForm); reintroduce;
  end;


  TfMainForm = class(TForm, IObserver)
    actSortTrack: TAction;
    actShowEqualizer: TAction;
    ActShowPreferencesInterface: TAction;
    ActShowPreferencesEngine: TAction;
    ActShowPreferencesMediaLibrary: TAction;
    actShowLeft: TAction;
    actShowPLMediainfo: TAction;
    actShowAbout: TAction;
    ActShowPreferences: TAction;
    ActionList: TActionList;
    Artist: TLabel;
    cbGroupBy: TComboBox;
    FilesTree: TTreeView;
    MainMenu1: TMainMenu;
    MenuItem20: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem62: TMenuItem;
    mnuColumns: TMenuItem;
    MenuItem51: TMenuItem;
    mnuRating0: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    mnuEnqueuePlaylist: TMenuItem;
    mnuPlayPlaylist: TMenuItem;
    mnuDeletePlaylist: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    mnuNewPlayList: TMenuItem;
    mnuEditPlaylist: TMenuItem;
    MenuItem58: TMenuItem;
    mnuSeparator: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    pcCenter: TPageControl;
    Panel1: TPanel;
    pmPlaylists: TPopupMenu;
    RateStars: TImageList;
    MenuItem21: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    mnuRating: TMenuItem;
    mnuRating1: TMenuItem;
    mnuRating5: TMenuItem;
    mnuRating4: TMenuItem;
    mnuRating3: TMenuItem;
    mnuRating2: TMenuItem;
    sgPlayList: TStringGrid;
    slVolume: TThemedSlider;
    edtFilter: TLabeledEdit;
    ePath: TDirectoryEdit;
    gbStats: TGroupBox;
    lbTime: TLabel;
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
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    pnlTimePos: TPanel;
    pnlLeft: TPanel;
    PlaylistMenu: TPopupMenu;
    pmdirectories: TPopupMenu;
    PlaylistTree: TTreeView;
    pnCollection: TPopupMenu;
    pnHeaderPlaylist: TPopupMenu;
    RepeatMenu: TPopupMenu;
    btnCloseCollectionStat: TSpeedButton;
    sgStats: TStringGrid;
    btnFilterCancel: TSpeedButton;
    bMute: TToolButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    tsPlayer: TTabSheet;
    tsWelcome: TTabSheet;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    TrackBar: TThemedSlider;
    tvCollection: TTreeView;
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
    TrayMenu: TPopupMenu;
    Splitter1: TSplitter;
    ToolButton5: TToolButton;
    imgCover: TImage;
    Album: TLabel;
    Timer: TTimer;
    Title: TLabel;
    MainMenu: TMainMenu;
    mnuLoadPlaylist: TMenuItem;
    mnuSavePlaylist: TMenuItem;
    mnuFile: TMenuItem;
    pnlPlayInfo: TPanel;
    pnlPlaylist: TPanel;
    pcMain: TPageControl;
    pnlCollection: TPanel;
    pnlMain: TPanel;
    pnlControl: TPanel;
    ProgressBar: TProgressBar;
    tbMediaControl: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    TrayIcon: TTrayIcon;
    tsCollection: TTabSheet;
    tsDirectory: TTabSheet;
    procedure actShowAboutExecute(Sender: TObject);
    procedure actShowEqualizerExecute(Sender: TObject);
    procedure actShowLeftExecute(Sender: TObject);
    procedure actShowPLMediainfoExecute(Sender: TObject);
    procedure ActShowPreferencesExecute(Sender: TObject);
    procedure ActShowPreferencesMediaLibraryExecute(Sender: TObject);
    procedure actSortTrackExecute(Sender: TObject);
    procedure BackEndSongStart(Sender: TObject);
    procedure btnBackDirClick(Sender: TObject);
    procedure btnFilterCancelClick(Sender: TObject);
    procedure btnForwardDirClick(Sender: TObject);
    procedure btnHomeDirClick(Sender: TObject);
    procedure AfterFullStart(Data: PtrInt);
    procedure cbGroupByChange(Sender: TObject);
    procedure CollectionTreeDblClick(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure ePathAcceptDirectory(Sender: TObject; var Value: string);
    procedure ePathEditingDone(Sender: TObject);
    procedure FilesTreeCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure FilesTreeDblClick(Sender: TObject);
    procedure FilesTreeGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure FilesTreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure FilesTreeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure imgCoverDblClick(Sender: TObject);
    procedure mnuRating0Click(Sender: TObject);
    procedure mnuRatingClick(Sender: TObject);
    procedure MenuItem59Click(Sender: TObject);
    procedure MenuItem60Click(Sender: TObject);
    procedure mnuColumnsClick(Sender: TObject);
    procedure mnuDeletePlaylistClick(Sender: TObject);
    procedure mnuEnqueuePlaylistClick(Sender: TObject);
    procedure mnuPlayPlaylistClick(Sender: TObject);
    procedure mnuNewPlayListClick(Sender: TObject);
    procedure mnuEditPlaylistClick(Sender: TObject);
    procedure mnuRestoreClick(Sender: TObject);
    procedure mnuEnqueueItemsClick(Sender: TObject);
    procedure mnuFileInfoClick(Sender: TObject);
    procedure mnuInfoClick(Sender: TObject);
    procedure mnuPlayItemsClick(Sender: TObject);
    procedure mnuRemovePlaylistClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure PlaylistTreeCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    //    procedure PlaylistMenuPopup(Sender: TObject);
    procedure pmdirectoriesPopup(Sender: TObject);
    procedure pmPlaylistsPopup(Sender: TObject);
    procedure pnCollectionPopup(Sender: TObject);
    procedure pnHeaderPlaylistPopup(Sender: TObject);
    procedure sgPlayListClick(Sender: TObject);
    procedure sgPlayListColRowMoved(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure sgPlayListContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
    procedure sgPlayListDblClick(Sender: TObject);
    procedure sgPlayListDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure sgPlayListDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
    procedure sgPlayListDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure sgPlayListHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure sgPlayListHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure sgPlayListHeaderSizing(Sender: TObject; const IsColumn: boolean; const aIndex, aSize: integer);
    procedure sgPlayListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure sgPlayListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure sgPlayListMouseLeave(Sender: TObject);
    procedure sgPlayListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure sgPlayListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure sgPlayListPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure sgPlayListResize(Sender: TObject);
    procedure sgStatsClick(Sender: TObject);
    procedure sgStatsPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure slVolumeChange(Sender: TObject);
    procedure btnCloseCollectionStatClick(Sender: TObject);
    procedure slVolumeClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure btnUpDirClick(Sender: TObject);
    procedure tbRepeatClick(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure TrackBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure TrackBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure TrackDblClick(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure TrayIconMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PlaylistTreeDblClick(Sender: TObject);
    procedure tsDirectoryShow(Sender: TObject);
    procedure tsPlayListShow(Sender: TObject);
    procedure tvCollectionCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure tvCollectionDblClick(Sender: TObject);
    procedure tvCollectionGetImageIndex(Sender: TObject; Node: TTreeNode);
  private
    SeekIng: boolean;
    fSourceIndex, fTargetIndex: integer;
    CurrentCover: string;
    SortFields: TSortFields;
    CurrentPath: string;
    PathHistory: TStringList;
    PathIndex: integer;
    PlaylistSelected: TRowsSelection;
    MovingSelection: TMovingSelection;
    FAnchor: integer;
    fColumnsWidth: array of integer;
    RatingBack, RatingFront: TBitmap;
    Quitting: boolean;
    LoadedPlaylist: boolean;
    TrayMenuActive: boolean;
    PlaylistContainer: TPlaylistContainer;
    {$IFDEF MPRIS2}
    Mpris: TMpris2;
    {$ENDIF MPRIS}
    {$IFDEF NOTIFYDBUS}
    Notifier: TNotificationClient;
    MyNotification: RNotification;
    {$ENDIF NOTIFYDBUS}
    {$IFDEF TASKBAR_EXTENSION}
    Mytaskbar_ext: TTaskBarExtender;
    {$ENDIF}
    {$IFDEF NETWORK_INTF}
    MyNetIntf: TNetIntf;
    {$ENDIF}
    {$IFDEF MULTIMEDIA_KEYS}
    fMultimediaKeys: TMultimediaKeys;
    {$ENDIF}
    {$IFDEF SCREEN_LOCK}
    fscreenlock: TScreenLockHandler;
    {$ENDIF}

    FPlaylistGuiParam: TPlaylistGuiParam;
    FMainFormParam: TMainFormParam;

    function AdjustPos(pt: tpoint): Tpoint;
    function ColumnSize(aCol: integer): integer;
    procedure ClearPanelInfo;
    procedure CollectionHandler(Enqueue: boolean);
    procedure FileSystemHandler(Enqueue: boolean);
    procedure LoadColumnsMenu(BaseItem: TMenuItem);
    procedure OnConfigDone(Sender: TObject; ChangedConf: boolean);
    procedure PlayListHandler(Enqueue: boolean);
    procedure LoadDir(Path: string);
    procedure LoadTree;
    procedure MediaLibraryScanComplete(Sender: TObject; _Added, _Updated, _Removed, _Failed: integer);
    procedure MoveSelection(KeyDirection: TMovingSelection; Shift: TShiftState; Row: integer);
    procedure OnLoaded(Sender: TObject);
    procedure OnMenuItemClick(Sender: TObject);
    procedure PlayListChange(Sender: TObject);
    procedure MediaLibraryScanBegin(Sender: TObject);
    procedure AdaptSize(Recalculate: boolean = True);
    function PrepareFields: string;
    function PrepareFilter: string;
    function PrepareImportFilter(Node: TMusicTreeNode): string;
    procedure ReloadPlayList;
    procedure OnEngineCommand(Sender: TObject; Command: TEngineCommand);
    procedure OnExternalCommand(Sender: TObject; Command: RExternalCommand; var Handled: boolean);
    procedure SaveConfig(Sender: TObject);
    procedure ReadConfig(Sender: TObject);
    procedure RemoveSelectionFromPlaylist;
    procedure ScrollIntoView;
    procedure LoadAutomaticPlaylist;
    procedure ShowNotification;
    procedure UpdateProperty(Kind: TChangedProperty);
    procedure UpdateTree(Sender: TObject);
  public
    { public declarations }
    procedure LoadLastPlaylist;
    procedure CheckWelcomeMode;

  end;

var
  fMainForm: TfMainForm;

implementation

{$R *.lfm}

uses AppConsts, AudioTag, LazLoggerBase, FilesSupport,
  uConfig, uMiniPlayer, uSongInfo, uAbout, baseTag,
  Math, udm, lazutf8, GeneralFunc;

type

  THackGrid = class(TStringGrid)
  public
    property Gcache;
  end;

  TSortRow = record
    Kind: TTagKind;
    FieldName: string;
    ImageIndex: integer;
  end;

const
  SortArray: array [tkAlbum..tkTracK] of TSortRow =
    ((Kind: tkAlbum; FieldName: 'Album'; ImageIndex: 1),
    (Kind: tkAlbumArtist; FieldName: 'AlbumArtist'; ImageIndex: 0),
    (Kind: tkArtist; FieldName: 'Artist'; ImageIndex: 0),
    (Kind: tkSong; FieldName: 'Title'; ImageIndex: 2),
    (Kind: tkYear; FieldName: 'Year'; ImageIndex: 3),
    (Kind: tkGenre; FieldName: 'Genre'; ImageIndex: 4),
    (Kind: tkTracK; FieldName: '(case track when 0 then coalesce(trackstring,0) else track end)'; ImageIndex: -1)
    //    (Kind: tkRating; FieldName: 'Rating'; ImageIndex: 5)
    );

{ TPlaylistGuiParam }

procedure TPlaylistGuiParam.InternalSave;
var
  tmpSt: TStringList;
  i: integer;
begin
  tmpSt := TStringList.Create;
  try
    for i := 0 to fform.sgPlayList.Columns.Count - 1 do
      tmpst.Add(IntToStr(fform.sgPlayList.Columns[i].Index) + '=' +
        fform.sgPlayList.Columns[i].Title.Caption + ';' +
        IntToStr(fform.sgPlayList.Columns[i].Tag) + ';' +
        BoolToStr(fform.sgPlayList.Columns[i].Visible, 'Y', 'N') + ';' +
        IntToStr(fform.sgPlayList.Columns[i].Width) + ';'
        );
    Owner.SaveCustomParams('PlayListGrid', tmpSt);

  finally
    tmpSt.Free;
  end;
  Owner.Dirty := True;
end;

procedure TPlaylistGuiParam.Load;
const
  Base = 'PlayListGrid';
var
  tmpSt: TStringList;
  info: TStringList;
  i, j: integer;
  Col: integer;
begin
  tmpSt := TStringList.Create;
  info := TStringList.Create;
  try
    Owner.ReadCustomParams(Base, tmpSt);
    for i := 0 to tmpSt.Count - 1 do
    begin
      info.Clear;
      info.StrictDelimiter := True;
      info.Delimiter := ';';
      info.DelimitedText := tmpSt[i];
      Col := StrToInt(info[1]);
      for j := 0 to fform.sgPlayList.Columns.Count - 1 do
        if fform.sgPlayList.Columns[j].Tag = col then
        begin
          Col := j;
          break;
        end;
      FForm.sgPlayList.Columns[Col].Index := StrToInt(tmpSt.Names[i]);
      FForm.sgPlayList.Columns[col].Visible := Info[2] = 'Y';
      FForm.sgPlayList.Columns[Col].Width := StrToInt(info[3]);
    end;
  except
    // if problem loading columns size, remove that info from config
    // needed on 0.5 -> 1.0 upgrade
    Owner.RemoveSection(Base);
  end;
  tmpSt.Free;
  info.Free;

end;

constructor TPlaylistGuiParam.Create(aOwner: TConfig; Form: TfMainForm);
begin
  FForm := Form;
  inherited Create(aOwner);
end;

{ TMainFormParam }

procedure TMainFormParam.SetActivePage(AValue: integer);
begin
  if FActivePage = AValue then Exit;
  FActivePage := AValue;
  Dirty := True;
end;

procedure TMainFormParam.SetHeight(AValue: integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;
  Dirty := True;
end;

procedure TMainFormParam.SetLeft(AValue: integer);
begin
  if FLeft = AValue then Exit;
  FLeft := AValue;
  Dirty := True;
end;

procedure TMainFormParam.SetLeftPanelVisible(AValue: boolean);
begin
  if FLeftPanelVisible = AValue then Exit;
  FLeftPanelVisible := AValue;
  Dirty := True;
end;

procedure TMainFormParam.SetTop(AValue: integer);
begin
  if FTop = AValue then Exit;
  FTop := AValue;
  Dirty := True;
end;

procedure TMainFormParam.SetWidth(AValue: integer);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
  Dirty := True;
end;

procedure TMainFormParam.InternalSave;
const
  Base = 'MainForm';
begin
  Owner.Inifile.WriteInteger(Base, 'Height', fHeight);
  Owner.Inifile.WriteInteger(Base, 'Width', fWidth);
  Owner.Inifile.WriteInteger(Base, 'Top', fTop);
  Owner.Inifile.WriteInteger(Base, 'Left', fLeft);
  Owner.Inifile.WriteInteger(Base, 'ActivePage', FActivePage);
  Owner.Inifile.WriteBool(Base, 'LeftPanelVisible', FLeftPanelVisible);
end;

procedure TMainFormParam.Load;
const
  Base = 'MainForm';
begin
  fHeight := Owner.Inifile.ReadInteger(Base, 'Height', FForm.Height);
  fWidth := Owner.Inifile.ReadInteger(Base, 'Width', FForm.Width);
  fTop := Owner.Inifile.ReadInteger(Base, 'Top', FForm.Top);
  fLeft := Owner.Inifile.ReadInteger(Base, 'Left', FForm.Left);
  fActivePage := Owner.Inifile.ReadInteger(Base, 'ActivePage', 0);
  fLeftPanelVisible := Owner.Inifile.ReadBool(Base, 'LeftPanelVisible', True);

end;

constructor TMainFormParam.Create(aOwner: TConfig; Form: TfMainForm);
begin
  FForm := Form;
  inherited Create(aOwner);
end;


{ TfMainForm }
procedure TfMainForm.CollectionTreeDblClick(Sender: TObject);
begin
  CollectionHandler(True);
end;

procedure TfMainForm.CollectionHandler(Enqueue: boolean);
var
  Node: TMusicTreeNode;
  aSong: TCustomSong;
begin
  Node := TMusicTreeNode(tvCollection.GetFirstMultiSelected);
  while Node <> nil do
  begin
    if not Enqueue then
      BackEnd.PlayList.Clear;

    case node.Kind of
      tkSong:
      begin
        aSong := TCustomSong.Create(BackEnd.mediaLibrary.FullNameFromID(Node.ID));
        BackEnd.PlayList.Add(ASong);
      end
      else
        BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList, PrepareImportFilter(Node), PrepareFields);
    end;
    Node := TMusicTreeNode(node.GetNextMultiSelected);
  end;

  OnLoaded(sgPlayList);

  if not Enqueue and (BackEnd.PlayList.Count > 0) then
  begin
    BackEnd.PlayList.ItemIndex := 0;
    BackEnd.Play;
  end;
end;

procedure TfMainForm.FileSystemHandler(Enqueue: boolean);
var
  Node: TFileTreeNode;
  aSong: TCustomSong;
begin
  Node := TFileTreeNode(FilesTree.GetFirstMultiSelected);
  while Node <> nil do
  begin
    if not Enqueue then
      BackEnd.PlayList.Clear;

    if node.isDir then
      BackEnd.Manager.ImportFromDirectory(Node.FullPath, True, BackEnd.PlayList)
    else
    begin
      aSong := TCustomSong.Create(Node.FullPath);
      BackEnd.PlayList.Add(ASong);
    end;
    Node := TFileTreeNode(node.GetNextMultiSelected);
  end;

  OnLoaded(sgPlayList);

  if not Enqueue and (BackEnd.PlayList.Count > 0) then
  begin
    BackEnd.PlayList.ItemIndex := 0;
    BackEnd.Play;
  end;
end;

procedure TfMainForm.PlayListHandler(Enqueue: boolean);
var
  playlistbuilder: TPlayListBuilder;
  item: TPlayListTreeNode;
begin
  item := TPlayListTreeNode(PlaylistTree.Selected);
  if item = nil then
    exit;
  if not Enqueue then
    BackEnd.PlayList.Clear;
  playlistbuilder := TPlayListBuilder.Create;
  try
    playlistbuilder.FromJson(PlaylistContainer.GetByName(item.FullPath));
    BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList,
      PlayListBuilder.Filter, PlayListBuilder.SortClause);

  finally
    playlistbuilder.Free;
  end;
  ReloadPlayList;
end;

procedure TfMainForm.edtFilterChange(Sender: TObject);
begin
  LoadTree;
end;

procedure TfMainForm.ePathAcceptDirectory(Sender: TObject; var Value: string);
begin
  LoadDir(Value);
end;

procedure TfMainForm.ePathEditingDone(Sender: TObject);
begin
  if (PathIndex + 1) < PathHistory.Count then
    PathHistory.Capacity := PathIndex;
  LoadDir(ePath.Directory);

end;

procedure TfMainForm.FilesTreeCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TFileTreeNode;
end;

procedure TfMainForm.FilesTreeDblClick(Sender: TObject);
var
  Node: TFileTreeNode;
begin
  Node := TFileTreeNode(FilesTree.Selected);
  if Node = nil then
    exit;

  if Node.isDir then
    LoadDir(Node.FullPath)
  else
  begin
    BackEnd.PlayList.EnqueueFile(Node.FullPath);
    ReloadPlayList;
  end;

end;

procedure TfMainForm.FilesTreeGetImageIndex(Sender: TObject; Node: TTreeNode);
var
  myNode: TFileTreeNode;
begin
  myNode := TFileTreeNode(Node);
  if myNode = nil then
    exit;

  if (myNode.isDir) then
    myNode.ImageIndex := 7
  else
    myNode.ImageIndex := 2;

end;

procedure TfMainForm.FilesTreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
var
  myNode: TFileTreeNode;
begin
  myNode := TFileTreeNode(Node);
  if myNode = nil then
    exit;
  myNode.SelectedIndex := myNode.ImageIndex;
end;

procedure TfMainForm.FilesTreeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  Node: TTreeNode;
begin
  if (ssCtrl in Shift) and (UpperCase(char(key)) = 'A' )then
  begin
    Node := FilesTree.Items.GetFirstNode;
    while Assigned(Node) do
    begin
      Node.Selected := True;
      Node := Node.GetNext;
    end;
  end;
end;

procedure TfMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (GuiConfigObj.InterfaceParam.MinimizeOnClose) and not Quitting then
  begin
    Application.ShowMainForm := False;
    {$IFDEF TASKBAR_EXTENSION}
    if Mytaskbar_ext.Initialized then
      Mytaskbar_ext.UnInit;
    {$ENDIF}
    CloseAction := caHide;
  end
  else
  begin
    BackEnd.SaveState;
    SaveConfig(nil);
    CloseAction := caFree;
  end;

end;

procedure TfMainForm.BackEndSongStart(Sender: TObject);
var
  Song: Tsong;
  imgName: string;
  id: integer;
  f: TTagReader;
  imgLoaded: boolean;
  //  tmp:Tmemorystream;
begin
  Application.ProcessMessages;
  pnlPlayInfo.Visible := True;
  song := BackEnd.PlayList.CurrentItem;
  // mcmcmcmc scrollinto
  ScrollIntoView;

  Title.Caption := Song.tags.Title;
  //  track.Caption     := Song.Tags.TrackString;
  Album.Caption := Song.Tags.Album;
  Artist.Caption := Song.Tags.Artist;
  TrackBar.Max := Song.Tags.Duration;
  TrackBar.Position := Lo(BackEnd.Position);
  TrayIcon.Hint := Song.tags.Title + LineEnding + Song.Tags.Artist;
  Caption := unicodestring(Song.tags.Title + ' - ' + Song.Tags.Artist);

  if TrayIcon.Hint = LineEnding then
  begin
    TrayIcon.Hint := Song.FileName;
    Caption := Song.FileName;
  end;

  imgloaded := False;
  if Song.Tags.HasImage then
  begin
    CurrentCover := '';
    f := GetFileTagsObject(Song.Tags.FileName);
    try
      f.Tags.Images[0].image.Position := 0;
      if f.Tags.Images[0].image.size > 0 then
      begin
        imgCover.Picture.LoadFromStream(f.Tags.Images[0].image);
        imgCover.Hint := rEmbedded;
        imgloaded := True;
      end;
    except
    end;
    f.Free;
  end;

  if not imgLoaded then
  begin
    imgName := BackEnd.GetImageFromfolder(IncludeTrailingPathDelimiter(Song.FilePath));
    if (imgName <> '') and (imgName <> CurrentCover) then
    begin
      imgCover.Picture.LoadFromFile(imgName);
      imgCover.Hint := imgName;
      CurrentCover := imgName;
    end;
  end;

  ShowNotification;


  id := BackEnd.mediaLibrary.IDFromFullName(BackEnd.PlayList.CurrentItem.FullName);
  if id > -1 then
    BackEnd.mediaLibrary.SetSongPlayed(ID);

  ScrollIntoView;
  Application.ProcessMessages;

end;

procedure TfMainForm.ShowNotification;
var
  ASong: TSong;
  tmpstr: string;
begin
  ASong := BackEnd.PlayList.CurrentItem;
  if not Assigned(ASong) then
    exit;

  if GuiConfigObj.NotificationParam.Kind = npkOSD then
    ShowOSD(ASong, imgCover.Picture);

  if GuiConfigObj.NotificationParam.Kind = npkNotifications then
  begin
    {$IFDEF NOTIFYDBUS}
    MyNotification.Summary := ASong.tags.Title;
    //  MyNotification.IconName:='audio-x-generic';
    MyNotification.IconName := BackEnd.GetCoverURL;
    MyNotification.Body := ASong.Tags.Album + LineEnding + ASong.Tags.Artist + LineEnding + ASong.Tags.TrackString;
    MyNotification.TimeOut := NOTIFY_EXPIRES_DEFAULT;
    MyNotification.Urgency := NOTIFY_URGENCY_NORMAL;
    Notifier := TNotificationClient.Create;
    try
      Notifier.init(DisplayAppName);
      Notifier.ShowNotification(MyNotification);
      Notifier.UnInit;
    finally
      Notifier.Free;
    end;
    {$ELSE}// Use standard balloon hint on other widgetset
    TrayIcon.BalloonTimeout := GuiConfigObj.NotificationParam.TimeOut;
    TrayIcon.BalloonTitle := ASong.tags.Title;
    tmpstr := UTF8Encode(aSong.Tags.Album + LineEnding + ASong.Tags.Artist + LineEnding +
      ASong.Tags.TrackString);

    TrayIcon.BalloonHint := tmpstr;
    if trim(TrayIcon.BalloonHint) = '' then
      TrayIcon.BalloonHint := UTF8ToSys(ASong.FileName);
    ;

    TrayIcon.ShowBalloonHint;
    {$ENDIF}

  end;

end;

procedure TfMainForm.UpdateProperty(Kind: TChangedProperty);
begin
  case kind of
    cpVolume, cpMute: begin
      slVolume.Position := BackEnd.Volume;
      dm.actmute.Checked := backend.muted;
      if backend.muted then
        dm.actMute.ImageIndex := 18
      else
        dm.actMute.ImageIndex := 19;
    end;

    cpClosing: begin
      Quitting := True;
      LCLIntf.PostMessage(Self.Handle, LM_CLOSEQUERY, 0, 0);
    end;
    cpPlayPos: begin
      if not seeking then
        TrackBar.Position := BackEnd.Position;
      lbTime.Caption := FormatTimeRange(BackEnd.Position);
    end;
    cpLooping: dm.changeRepeatMode(BackEnd.Looping, False);
  end;

end;

procedure TfMainForm.OnConfigDone(Sender: TObject; ChangedConf: boolean);
begin
  if not ChangedConf then
    exit;

  {$IFDEF NETWORK_INTF}
  if GuiConfigObj.NetRemoteParam.Enabled then
  begin
    if not Assigned(MyNetIntf) then
    begin
      MyNetIntf := TNetIntf.Create;
      MyNetIntf.OnlyLAN := GuiConfigObj.NetRemoteParam.OnlyLAN;
      MyNetIntf.Port := GuiConfigObj.NetRemoteParam.Port;
      MyNetIntf.Activate(BackEnd);
    end;
  end
  else
  if Assigned(MyNetIntf) then
    FreeAndNil(MyNetIntf);

  if Assigned(MyNetIntf) then
    MyNetIntf.Port := GuiConfigObj.NetRemoteParam.Port;
  {$ENDIF}
end;

procedure TfMainForm.ActShowPreferencesExecute(Sender: TObject);
begin
  ShowConfigurationEditor(@OnConfigDone);
end;

procedure TfMainForm.ActShowPreferencesMediaLibraryExecute(Sender: TObject);
begin
  ShowConfigurationEditor(@OnConfigDone, cpMediaLibrary);
end;

procedure TfMainForm.actSortTrackExecute(Sender: TObject);
begin
  case TAction(sender).ActionComponent.Tag of
    1: BackEnd.PlayList.SortField := stTitle;
    2: BackEnd.PlayList.SortField := stTrack;
    3: BackEnd.PlayList.SortField := stDuration;
  end;

  BackEnd.PlayList.SortDirection := sdplAscending;

  if PlaylistSelected.isMultiselection then
    begin
      BackEnd.PlayList.Sort(PlaylistSelected.FirstSelected, PlaylistSelected.LastSelected);
    end
  else
    begin
      BackEnd.PlayList.Sort(0, BackEnd.PlayList.Count -1);
    end;
end;

procedure TfMainForm.actShowAboutExecute(Sender: TObject);
var
  theForm: TfAbout;
begin
  theForm := TfAbout.Create(application);
  theForm.ShowModal;
end;

procedure TfMainForm.actShowEqualizerExecute(Sender: TObject);
var
  theForm: TfEqualizer;
begin
  theForm := TfEqualizer.Create(application);
  try
    theForm.ShowModal;
  finally
    theForm.Free;
  end;
end;

procedure TfMainForm.actShowLeftExecute(Sender: TObject);
begin
  pnlLeft.Visible := actShowLeft.Checked;
  FMainFormParam.LeftPanelVisible := actShowLeft.Checked;
end;

procedure TfMainForm.actShowPLMediainfoExecute(Sender: TObject);
var
  info: TfSongInfo;
  Index: integer;
  fileList: TStringList;
begin
  Index := PlaylistSelected.FirstSelected;
  if Index < 0 then
    exit;

  fileList := TStringList.Create;
  try
    while Index > -1 do
    begin
      fileList.Add(BackEnd.PlayList[Index].FullName);
      Index := PlaylistSelected.NextSelected(Index);
    end;

    info := TfSongInfo.Create(Application);
    if fileList.Count > 1 then
      info.InitFromList(FileList)
    else
      info.InitFromFile(FileList[0]);
    info.OnUpdate := @UpdateTree;
    Info.Show;

  finally
    FreeAndNil(FileList);
  end;
end;

procedure TfMainForm.btnBackDirClick(Sender: TObject);
begin
  Dec(PathIndex);
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
  Inc(PathIndex);
  if PathIndex > PathHistory.Count - 1 then
    PathIndex := PathHistory.Count - 1;

  LoadDir(PathHistory[PathIndex]);

end;

procedure TfMainForm.btnHomeDirClick(Sender: TObject);
begin
  LoadDir(GetUserDir);
end;

procedure TfMainForm.AfterFullStart(Data: PtrInt);
begin
  {$IFDEF TASKBAR_EXTENSION}
  if not Mytaskbar_ext.Initialized then
    Mytaskbar_ext.Init;
  {$ENDIF}
end;

procedure TfMainForm.cbGroupByChange(Sender: TObject);
begin
  SortFields.F1 := ArrayGroup[cbGroupBy.ItemIndex].F1;
  SortFields.F2 := ArrayGroup[cbGroupBy.ItemIndex].F2;
  SortFields.F3 := ArrayGroup[cbGroupBy.ItemIndex].F3;
  GuiConfigObj.InterfaceParam.GroupBy := cbGroupBy.ItemIndex;
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
  filter := edtFilter.Text;
  if filter <> '' then
    Result := format(' %0:s like %3:s' + ' or %1:s like %3:s' + ' or %2:s like %3:s' + ' or Title like %3:s',
      [SortArray[SortFields.F1].FieldName,
      SortArray[SortFields.F2].FieldName,
      SortArray[SortFields.F3].FieldName,
      QuotedStr('%' + filter + '%')])
  else
    Result := '';

end;


function TfMainForm.PrepareImportFilter(Node: TMusicTreeNode): string;
var
  wrkNode: TMusicTreeNode;
  wrkValue: string;
begin
  Result := '0=0';
  wrkNode := Node;
  repeat
    if wrkNode.EmptyTag then
      wrkValue := EmptyStr
    else
      wrkValue := wrkNode.Text;
    Result := Result + format(' and %s = %s ', [SortArray[wrkNode.Kind].FieldName,
      QuotedStr(wrkValue)]);
    wrkNode := TMusicTreeNode(wrkNode.Parent);
  until (wrkNode = nil);

end;


procedure TfMainForm.LoadTree;
var
  tags: TCommonTags;
  L1Key, L2Key: string;
  L1Item, L2Item, L3Item: TMusicTreeNode;
const
  FakeValue: string = '!"£$%&/()=?=)(/&%$£"!';
begin
  tvCollection.BeginUpdate;
  try
    TVCollection.items.Clear;
    L1Key := FakeValue;
    L2Key := FakeValue;

    BackEnd.mediaLibrary.ReadBegin(PrepareFilter, PrepareFields);

    while not BackEnd.mediaLibrary.ReadComplete do
    begin
      Tags := BackEnd.mediaLibrary.ReadItem;
      if UpperCase(TagValue(Tags, SortFields.F1)) <> L1Key then
      begin
        L1Item := TMusicTreeNode(TVCollection.Items.Add(nil, TagValue(Tags, SortFields.F1)));
        L1Key := UpperCase(TagValue(Tags, SortFields.F1));
        L1Item.EmptyTag := L1Item.Text = EmptyStr;
        if L1Item.EmptyTag then L1Item.Text := rEmptyTag;
        L1Item.Kind := SortFields.F1;
        L2Key := FakeValue;
      end;

      if UpperCase(TagValue(Tags, SortFields.F2)) <> L2Key then
      begin
        L2Item := TMusicTreeNode(TVCollection.Items.AddChild(L1Item, TagValue(Tags, SortFields.F2)));
        L2Key := UpperCase(TagValue(Tags, SortFields.F2));
        L2Item.EmptyTag := L2Item.Text = EmptyStr;
        if L2Item.EmptyTag then L2Item.Text := rEmptyTag;
        L2Item.Kind := SortFields.F2;
      end;

      L3Item := TMusicTreeNode(TVCollection.Items.AddChild(L2Item, TagValue(Tags, tkSong)));
      L3Item.Kind := tkSong;
      L3Item.ID := tags.ID;
      Finalize(Tags);
      BackEnd.mediaLibrary.NextItem;
    end;

  finally
    tvCollection.EndUpdate;
  end;
  tvCollection.Repaint;

end;

procedure TfMainForm.MediaLibraryScanComplete(Sender: TObject; _Added, _Updated, _Removed, _Failed: integer);
begin
  //debugln(DateTimeToStr(now), ' - ' ,'Stop');
  sgStats.Clean;
  sgStats.RowCount := 4;
  sgStats.Cells[0, 0] := rAddedTrack;
  sgStats.Cells[1, 0] := IntToStr(_Added);
  sgStats.Cells[0, 1] := rUpdatedTrack;
  sgStats.Cells[1, 1] := IntToStr(_Updated);
  sgStats.Cells[0, 2] := rRemovedTrack;
  sgStats.Cells[1, 2] := IntToStr(_Removed);
  sgStats.Cells[0, 3] := rFailedTrack;
  sgStats.Cells[1, 3] := IntToStr(_Failed);

  gbStats.Visible := True;
  Application.ProcessMessages;
  LoadTree;

end;

procedure TfMainForm.CheckWelcomeMode;
begin
  if (BackEnd.PlayListCount = 0) and
    (BackEnd.mediaLibrary.IsEmpty) then
    pcCenter.ActivePage := tsWelcome
  else
    pcCenter.ActivePage := tsPlayer;
end;

procedure TfMainForm.FormCreate(Sender: TObject);
var
  tmpIcon: TIcon;
  tmpSize: TSize;
begin
  GuiConfigObj := TGuiConfig.Create(BackEnd.Config);

  TrayMenuActive := False;
  Quitting := False;
  PlaylistSelected := TRowsSelection.Create;
  PlaylistContainer := nil;
  PathHistory := TStringList.Create;
  PathHistory.Duplicates := dupIgnore;

  TrackBar.Position := 0;

  SortFields.F1 := tkAlbumArtist;
  SortFields.F2 := tkAlbum;
  SortFields.F3 := tkSong;

  CurrentPath := EmptyStr;
  ClearPanelInfo;

  //Cache bitmap used for rating column painting
  RatingBack := TBitmap.Create;
  RatingFront := TBitmap.Create;
  RateStars.GetBitmap(1, RatingBack);
  RateStars.GetBitmap(0, RatingFront);

  BackEnd.OnPlayListChange := @PlayListChange;
  BackEnd.AudioEngine.OnSongStart := @BackEndSongStart;
  BackEnd.OnSaveInterfaceState := @SaveConfig;

  BackEnd.OnPlayListLoad := @OnLoaded;
  BackEnd.OnEngineCommand := @OnEngineCommand;
  BackEnd.OnExternalCommand := @OnExternalCommand;

  BackEnd.mediaLibrary.OnScanComplete := @MediaLibraryScanComplete;
  BackEnd.mediaLibrary.OnScanStart := @MediaLibraryScanBegin;

  ReadConfig(Self);

  CheckWelcomeMode;

  slVolume.Max := 255;//BackEnd.AudioEngine.MaxVolume;
  slVolume.Position := BackEnd.Volume;

  case TplRepeat(BackEnd.PlayListParam.RepeatMode) of
    rptNone: dm.actRepeatNone.Checked := True;
    rptTrack: dm.actRepeatTrack.Checked := True;
    rptAlbum: dm.actRepeatAlbum.Checked := True;
    rptPlayList: dm.actRepeatAll.Checked := True;
  end;

  if GuiConfigObj.InterfaceParam.ShowTrayIcon then
  begin
    tmpIcon := TIcon.Create;
    tmpIcon.LoadFromResourceName(HINSTANCE, 'MAINICON');
    try // on some linux DE/WS combination this could fail
      tmpSize.cx := TrayIcon.Canvas.Width;
      tmpSize.cy := TrayIcon.Canvas.Height;
    except //fallback
      tmpSize.cx := 32;
      tmpSize.cy := 32;
    end;

    tmpIcon.Current := tmpIcon.GetBestIndexForSize(tmpsize);
    TrayIcon.Icon.Transparent := True;
    TrayIcon.Icon.Assign(tmpIcon);
    tmpIcon.Free;
  end;

  if GuiConfigObj.InterfaceParam.ShowTrayIcon then
  begin
    Application.ShowMainForm := True;
    TrayIcon.Visible := True;
  end
  else
  begin
    Application.ShowMainForm := False;
    TrayIcon.Visible := False;
  end;

  slVolume.Position := BackEnd.Volume;
  BackEnd.Attach(Self);

  cbGroupBy.ItemIndex := GuiConfigObj.InterfaceParam.GroupBy;
  cbGroupBy.OnChange(self);
  SetLength(fColumnsWidth, 0);

  {$IFDEF MPRIS2}
  if GuiConfigObj.InterfaceParam.EnableSoundMenu then
  begin
    Mpris := TMpris2.Create;
    Mpris.Activate(BackEnd);
  end;
  {$ENDIF MPRIS}

  {$IFDEF TASKBAR_EXTENSION}
  Mytaskbar_ext := TTaskBarExtender.Create;
  {$ENDIF}

  {$IFDEF NETWORK_INTF}
  if GuiConfigObj.NetRemoteParam.Enabled then
  begin
    MyNetIntf := TNetIntf.Create;
    MyNetIntf.Port := GuiConfigObj.NetRemoteParam.Port;
    MyNetIntf.Activate(BackEnd);
  end;
  {$ENDIF}
  {$IFDEF MULTIMEDIA_KEYS}
  if GuiConfigObj.InterfaceParam.CaptureMMKeys then
    fMultimediaKeys := TMultimediaKeys.Create(GuiConfigObj.InterfaceParam.CaptureMMkeysMode, BackEnd);
  {$ENDIF}

  {$IFDEF SCREEN_LOCK}
  if GuiConfigObj.InterfaceParam.PauseWhenLocked then
  begin
    fscreenlock := TScreenLockHandler.Create(BackEnd);
    fscreenlock.Init;
  end;
  {$ENDIF}

  try
    if not FileExists(BackEnd.Config.GetPlaylistsPath + 'playlist.opl') then
    ;

  except
    on e: Exception do

  end;

  LoadedPlaylist := False;
  BackEnd.AutoSendPosEvents(True);
  OnLoaded(self);

end;

procedure TfMainForm.LoadLastPlaylist;
begin

  try
    if FileExistsUTF8(Backend.Config.ConfigDir + LASTPLAYLISTNAME) then
      BackEnd.Manager.ImportFromXSPF(Backend.Config.ConfigDir + LASTPLAYLISTNAME, BackEnd.PlayList)
    else
      BackEnd.PlayList.Clear;

    OnLoaded(Self);

    if (BackEnd.Manager.SavedTime <> 0) and
      BackEnd.PlayListParam.Restart and
      (BackEnd.PlayList.CurrentItem <> nil) then
    begin
      Application.ProcessMessages;
      BackEnd.AudioEngine.Play(BackEnd.PlayList.CurrentItem, BackEnd.Manager.SavedTime);
      ScrollIntoView;
      Application.ProcessMessages;
    end;
  except
    on e: Exception do
    begin
      DebugLn(e.Message);
      BackEnd.PlayList.Clear;
    end;
  end;

end;

procedure TfMainForm.FormDestroy(Sender: TObject);
begin

  {$IFDEF MPRIS2}
  if Assigned(Mpris) then
    Mpris.Deactivate;
  {$ENDIF MPRIS}
  {$IFDEF TASKBAR_EXTENSION}
  Mytaskbar_ext.UnInit;
  Mytaskbar_ext.Free;
  {$ENDIF}

  {$IFDEF NETWORK_INTF}
  if Assigned(MyNetIntf) then
  begin
    MyNetIntf.DeActivate;
    MyNetIntf.Free;
  end;
  {$ENDIF}

  {$IFDEF MULTIMEDIA_KEYS}
  if Assigned(fMultimediaKeys) then
    FreeAndNil(fMultimediaKeys);
  {$ENDIF}

  {$IFDEF SCREEN_LOCK}
  if Assigned(fscreenlock) then
    FreeAndNil(fscreenlock);
  {$ENDIF}


  if Assigned(FPlaylistGuiParam) then
    FreeAndNil(FPlaylistGuiParam);

  if Assigned(FMainFormParam) then
    FreeAndNil(FMainFormParam);

  if Assigned(fMiniPlayer) then
    FreeAndNil(fMiniPlayer);

  if Assigned(PlaylistContainer) then
    FreeAndNil(PlaylistContainer);

  PathHistory.Free;
  PlaylistSelected.ClearAll;
  PlaylistSelected.Size:=0;
  PlaylistSelected.Free;
  RatingBack.Free;
  RatingFront.Free;
  DM.Free;

  GuiConfigObj.Free;

  FreeBackEnd;

end;

procedure TfMainForm.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  BackEnd.Manager.ImportFromStringArray(FileNames, BackEnd.PlayList);
  BackEnd.SignalPlayListChange;
end;

procedure TfMainForm.FormHide(Sender: TObject);
begin
  Application.ShowMainForm := False;
end;

procedure TfMainForm.FormResize(Sender: TObject);
begin
  //AdaptSize;
  FMainFormParam.Width := Width;
  FMainFormParam.Height := Height;
end;

procedure TfMainForm.FormShow(Sender: TObject);
begin
  Application.ShowMainForm := False;
  ScrollIntoView;

  if (BackEnd.AudioEngine = nil) or (BackEnd.AudioEngine.GetEngineName = 'dummy') then
  begin
    ShowMessage(rMissingConfig);
    ActShowPreferences.Execute;
  end;
  if not LoadedPlaylist then
  begin
    LoadedPlaylist := True;
    LoadLastPlaylist;
  end;
  Application.QueueAsyncCall(@AfterFullStart, 0);
end;

procedure TfMainForm.imgCoverDblClick(Sender: TObject);
var
  coverForm: TfCover;
begin
  coverForm := TfCover.Create(Self);

  coverForm.ImageCover.Picture.Assign(imgCover.Picture);
  coverForm.Show;
  coverForm.SetSize(coverForm.ScrollBox1.Height, coverForm.ScrollBox1.Width);

end;

procedure TfMainForm.mnuRating0Click(Sender: TObject);
begin
  BackEnd.PlayList.CurrentItem.Rating := TMenuItem(Sender).tag * 2;
end;

procedure TfMainForm.mnuRatingClick(Sender: TObject);
begin
  if not assigned(BackEnd.PlayList.CurrentItem) then exit;
  case (BackEnd.PlayList.CurrentItem.Rating div 2) of
    1: mnuRating1.Checked := True;
    2: mnuRating2.Checked := True;
    3: mnuRating3.Checked := True;
    4: mnuRating4.Checked := True;
    5: mnuRating5.Checked := True;
    else
      mnuRating0.Checked := True;

  end;
end;

procedure TfMainForm.MenuItem59Click(Sender: TObject);
begin
  FileSystemHandler(True);
end;

procedure TfMainForm.MenuItem60Click(Sender: TObject);
begin
  FileSystemHandler(False);
end;

procedure TfMainForm.mnuColumnsClick(Sender: TObject);
begin
  LoadColumnsMenu(mnuColumns);
end;

procedure TfMainForm.mnuDeletePlaylistClick(Sender: TObject);
var
  item: TPlayListTreeNode;
begin
  item := TPlayListTreeNode(PlaylistTree.Selected);
  if item = nil then
    exit;
  if MessageDlg(rDeletePlaylistConfirmation, mtWarning, mbYesNo, 0) = mrYes then
  begin
    PlaylistContainer.Delete(item.Index);
    PlaylistContainer.Save;
    LoadAutomaticPlaylist;
  end;
end;

procedure TfMainForm.mnuEnqueuePlaylistClick(Sender: TObject);
begin
  PlayListHandler(True);
end;

procedure TfMainForm.mnuPlayPlaylistClick(Sender: TObject);
begin
  PlayListHandler(False);
end;

procedure TfMainForm.mnuNewPlayListClick(Sender: TObject);
begin
  with TfCustomPlayList.Create(self) do
  begin
    Container := PlaylistContainer;
    Index := -1;
    if showmodal = mrOk then
      LoadAutomaticPlaylist;

  end;
end;

procedure TfMainForm.mnuEditPlaylistClick(Sender: TObject);
var
  item: TPlayListTreeNode;
begin
  item := TPlayListTreeNode(PlaylistTree.Selected);
  if item = nil then
    exit;
  with TfCustomPlayList.Create(self) do
  begin
    Container := PlaylistContainer;
    Index := item.Index;
    if LoadFromJson(PlaylistContainer.GetByName(item.FullPath)) then
    begin
      if showmodal = mrOk then
        LoadAutomaticPlaylist;
    end
    else
      Free;
  end;
end;

procedure TfMainForm.PlayListChange(Sender: TObject);
begin
  ReloadPlayList;
  CheckWelcomeMode;
  Invalidate;
end;

procedure TfMainForm.MediaLibraryScanBegin(Sender: TObject);
begin
  sgStats.Clean;
  sgStats.RowCount := 1;
  sgstats.cells[0, 0] := rBeginCollectionScan;
  gbStats.Visible := True;
  Application.ProcessMessages;
  //debugln(DateTimeToStr(now), ' - ' ,'Begin');
end;

procedure TfMainForm.RemoveSelectionFromPlaylist;
var
  Index, PrevIndex: integer;
begin
  // Work in reverse order. Removing an item on playlist shift up remaining ones..
  index := PlaylistSelected.LastSelected;
  while Index > -1 do
  begin
    Previndex := PlaylistSelected.PreviousSelected(index);
    BackEnd.PlayList.Delete(Index);
    Index := Previndex;
  end;

  OnLoaded(sgPlayList);
  BackEnd.Notify(cpStatus);
end;

procedure TfMainForm.ScrollIntoView;
var
  aRow, iTopRow, visRows, lastRow: integer;
begin
  // sgplaylist.Row:= BackEnd.PlayList.ItemIndex +1;
  // above code trigger a Click event in grid. This was breaking
  // code for editing Rating columns

  ARow := BackEnd.PlayList.ItemIndex + 1;
  if (ARow > -1) and (ARow < sgplaylist.RowCount) then
  begin
    Arow := Arow + 1;
    iTopRow := sgplaylist.TopRow;
    visRows := sgplaylist.VisibleRowCount;
    lastRow := iTopRow + visRows - 1;
    if ARow <= iTopRow then
      sgplaylist.TopRow := ARow - 1
    else
    if ARow > lastRow then
      sgplaylist.TopRow := ARow - visRows;
  end;
end;

procedure TfMainForm.LoadAutomaticPlaylist;
var
  AutoPlayList: TStringList;
  i: integer;
  Node, BaseNode: TPlayListTreeNode;
  plName: string;
begin
  PlaylistTree.Items.Clear;
  BaseNode := TPlayListTreeNode(PlaylistTree.Items.Add(nil, rAutomaticPlaylist));
  BaseNode.FullPath := EmptyStr;

  AutoPlayList := TStringList.Create;

  try

    if not Assigned(PlaylistContainer) then
    begin
      TPlaylistContainer.CreateDefaultFileIfMissing(BackEnd.Config.GetPlaylistsPath + 'playlist.opl');
      PlaylistContainer := TPlaylistContainer.Create(BackEnd.Config.GetPlaylistsPath + 'playlist.opl');

    end;
    PlaylistContainer.GetPlaylists(AutoPlayList);


    //    BuildFileList(BackEnd.Config.GetPlaylistsPath+'*'+CustomPlaylistExtension,faAnyFile, AutoPlayList,False);
    for i := 0 to AutoPlayList.Count - 1 do
    begin
      plname := DecodeSafeFileName(ExtractFileNameOnly(AutoPlayList[i]));
      node := TPlayListTreeNode(PlaylistTree.Items.AddChild(BaseNode, plName));
      node.FullPath := AutoPlayList[i];
      Node.Automatic := True;
    end;

  finally
    AutoPlayList.Free;
  end;

  BaseNode.Expand(True);
end;

procedure TfMainForm.SaveConfig(Sender: TObject);
begin
  if Assigned(FMainFormParam) then
    FMainFormParam.Save;

  if Assigned(FPlaylistGuiParam) then
    FPlaylistGuiParam.Save;

end;

procedure TfMainForm.ReadConfig(Sender: TObject);
begin
  fMainFormParam := TMainFormParam.Create(BackEnd.Config, self);

  Height := fMainFormParam.Height;
  Width := fMainFormParam.Width;
  Top := fMainFormParam.Top;
  Left := fMainFormParam.Left;

  actShowLeft.Checked := not fMainFormParam.LeftPanelVisible;
  actShowLeft.Execute;
  pcMain.ActivePageIndex := fMainFormParam.ActivePage;

  FPlaylistGuiParam := TPlaylistGuiParam.Create(BackEnd.Config, Self);

end;

procedure TfMainForm.mnuRestoreClick(Sender: TObject);
begin
  ShowOnTop;
  Show;
end;

procedure TfMainForm.UpdateTree(Sender: TObject);
begin
  LoadTree;
end;


procedure TfMainForm.mnuEnqueueItemsClick(Sender: TObject);
begin
  CollectionHandler(True);
end;

procedure TfMainForm.mnuFileInfoClick(Sender: TObject);
var
  info: TfSongInfo;
  Node: TFileTreeNode;
  fileList: TStringList;
begin
  Node := TFileTreeNode(FilesTree.GetFirstMultiSelected);
  if node = nil then
    exit;

  if (not Node.isDir) then
  begin
    info := TfSongInfo.Create(Application);
    info.InitFromFile(Node.FullPath);
    info.OnUpdate := @UpdateTree;
    Info.Show;
  end
  else
  begin
    fileList := TStringList.Create;
    try
      Node := TFileTreeNode(FilesTree.GetFirstMultiSelected);
      while node <> nil do
      begin
        if not Node.isDir then
          fileList.Add(Node.FullPath);
        Node := TFileTreeNode(Node.GetNextMultiSelected);
      end;
      info := TfSongInfo.Create(Application);
      info.InitFromList(FileList);
      info.OnUpdate := @UpdateTree;
      Info.Show;
    finally
      FreeAndNil(FileList);
    end;
  end;
end;


procedure TfMainForm.mnuInfoClick(Sender: TObject);
var
  info: TfSongInfo;
  Node: TMusicTreeNode;
  fileList: TStringList;
begin

  Node := TMusicTreeNode(tvCollection.GetFirstMultiSelected);
  if node = nil then
    exit;

  if tvCollection.SelectionCount = 1 then
  begin
    if (Node.Kind = tkSong) then
    begin
      info := TfSongInfo.Create(Application);
      info.InitFromFile(BackEnd.mediaLibrary.FullNameFromID(Node.ID));
      info.OnUpdate := @UpdateTree;
      Info.Show;
    end;
    if (Node.Kind = tkAlbum) then
    begin
      fileList := TStringList.Create;
      try
        Node := TMusicTreeNode(Node.GetFirstChild);
        while node <> nil do
        begin
          if (node.Kind = tkSong) then
            fileList.Add(BackEnd.mediaLibrary.FullNameFromID(Node.ID));
          Node := TMusicTreeNode(Node.GetNextSibling);
        end;
        info := TfSongInfo.Create(Application);
        info.InitFromList(FileList);
        Info.Show;
        info.OnUpdate := @UpdateTree;

      finally
        FreeAndNil(FileList);
      end;
    end;

  end
  else
  begin
    fileList := TStringList.Create;
    try
      Node := TMusicTreeNode(tvCollection.GetFirstMultiSelected);
      while node <> nil do
      begin
        if (node.Kind = tkSong) then
          fileList.Add(BackEnd.mediaLibrary.FullNameFromID(Node.ID));
        Node := TMusicTreeNode(Node.GetNextMultiSelected);
      end;
      info := TfSongInfo.Create(Application);
      info.InitFromList(FileList);
      Info.Show;

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

procedure TfMainForm.pcMainChange(Sender: TObject);
begin
  FMainFormParam.ActivePage := pcMain.ActivePageIndex;
end;

procedure TfMainForm.PlaylistTreeCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TPlayListTreeNode;
end;

procedure TfMainForm.pmdirectoriesPopup(Sender: TObject);
var
  Node: TFileTreeNode;
begin
  Node := TFileTreeNode(FilesTree.Selected);
  if (Node <> nil) then
  begin
    mnuFileInfo.Visible := not Node.isDir;
    mnuSeparator.Visible := not Node.isDir;
  end;

end;

procedure TfMainForm.pmPlaylistsPopup(Sender: TObject);
var
  item: TPlayListTreeNode;
  MasterItem: boolean;
begin
  item := TPlayListTreeNode(PlaylistTree.Selected);
  if item = nil then
    exit;
  MasterItem := item.FullPath = EmptyStr;

  mnuPlayPlaylist.Visible := not MasterItem;
  MenuItem58.Visible := not MasterItem;
  mnuEditPlaylist.Visible := not MasterItem;
  mnuDeletePlaylist.Visible := not MasterItem;

end;

procedure TfMainForm.pnCollectionPopup(Sender: TObject);
var
  Node: TMusicTreeNode;
begin
  Node := TMusicTreeNode(tvCollection.GetFirstMultiSelected);
  if node = nil then
    exit;
  case Node.Kind of
    tkSong, tkAlbum: mnuInfo.Visible := True;
    else
      mnuInfo.Visible := False;
  end;

end;

procedure TfMainForm.OnMenuItemClick(Sender: TObject);
begin
  with TMenuItem(Sender), sgPlayList.Columns[TMenuItem(Sender).Tag] do
  begin
    Visible := not Checked;
    ColumnSize(TMenuItem(Sender).Tag);
  end;

  sgPlayList.Invalidate;
  AdaptSize;

  SaveConfig(self);
end;

// This function is adapted from Grids.pas
function TfMainForm.ColumnSize(aCol: integer): integer;
var
  i, W: integer;
  Ts: TSize;
  TmpCanvas: TCanvas;
  C: TGridColumn;
  ASong: TSong;
  txt: string;
  TrueCol: integer;
begin

  if (aCol < 0) or (aCol > sgPlayList.ColCount - 1) then
    Exit;

  tmpCanvas := GetWorkingCanvas(sgPlayList.Canvas);

  C := sgPlayList.Columns[(aCol)];

  try
    W := 0;
    for i := sgPlayList.TopRow to (sgPlayList.TopRow + sgPlayList.VisibleRowCount - 2) do
    begin

      if C <> nil then
      begin
        if i < sgPlayList.FixedRows then
          tmpCanvas.Font := C.Title.Font
        else
          tmpCanvas.Font := C.Font;
      end
      else if i < sgPlayList.FixedRows then
        tmpCanvas.Font := sgPlayList.TitleFont
      else
        tmpCanvas.Font := Font;

      ASong := BackEnd.PlayList.Songs[i];
      //        if ASong = nil then
      //          DebugLn('nessuno');

      TrueCol := sgPlayList.Columns[aCol].Tag;

      case TrueCol of
        0: Txt := IntToStr(i);
        1: Txt := ASong.Tags.Title;
        2: Txt := ASong.Tags.Album;
        3: Txt := ASong.Tags.Artist;
        4: Txt := FormatTimeRange(ASong.Tags.Duration);
        5: Txt := ASong.Tags.TrackString;
        6: Txt := ASong.Tags.Genre;
        7: Txt := ASong.Tags.Year;
        8: Txt := ASong.Tags.AlbumArtist;
        9: Txt := ASong.FileName;
        10: Txt := 'WWWWWW';
      end;
      if txt = '' then
        txt := '.';
      Ts := TmpCanvas.TextExtent(txt);

      if Ts.Cx > W then
        W := Ts.Cx;
    end;
    if C.Title.Caption <> '' then
    begin
      Ts := TmpCanvas.TextExtent(C.Title.Caption);
      if Ts.Cx > W then
        W := Ts.Cx;
    end;
  finally
    if tmpCanvas <> Canvas then
      FreeWorkingCanvas(tmpCanvas);
  end;

  if W = 0 then
    W := sgPlayList.DefaultColWidth
  else
    W := W + 8;

  Result := W;

end;

procedure TfMainForm.LoadColumnsMenu(BaseItem: TMenuItem);
var
  col: integer;
  item: TMenuItem;
begin
  BaseItem.Clear;
  for col := 0 to sgPlayList.Columns.Count - 1 do
  begin
    item := TMenuItem.Create(BaseItem);
    item.Caption := sgPlayList.Columns[col].Title.Caption;
    item.Tag := Col;
    item.Checked := sgPlayList.Columns[col].Visible;
    item.OnClick := @OnMenuItemClick;
    BaseItem.Add(item);
  end;

end;

procedure TfMainForm.pnHeaderPlaylistPopup(Sender: TObject);
begin
  LoadColumnsMenu(pnHeaderPlaylist.Items);
end;

procedure TfMainForm.sgPlayListClick(Sender: TObject);
var
  ACol, ARow: integer;
  p: Tpoint;
  Rating: integer;
  r1: Trect;
begin
  p := sgPlayList.ScreenToControl(Mouse.CursorPos);
  sgPlayList.MouseToCell(p.x, p.y, ACol, ARow);
  if (ARow > 0) and (aCol = 11) and (BackEnd.PlayList[ARow - 1].ID <> -1) then
  begin
    R1 := sgPlayList.CellRect(ACol, ARow);
    Rating := trunc(((p.x - R1.Left) * 10) / RateStars.Width);
    BackEnd.PlayList[ARow - 1].Rating := rating;
    BackEnd.PlayList[ARow - 1].TmpRating := -1;
    BackEnd.mediaLibrary.SetRating(BackEnd.PlayList[ARow - 1].ID, Rating);
    sgPlayList.InvalidateCell(ACol, ARow);
  end;
end;

procedure TfMainForm.sgPlayListColRowMoved(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if IsColumn then exit;

  BackEnd.PlayList.PushPos;
  BackEnd.PlayList.Swap(sIndex - 1, tIndex - 1);
  sgPlayList.Cells[1, sIndex] := IntToStr(sIndex);
  sgPlayList.Cells[1, tIndex] := IntToStr(tIndex);
  BackEnd.PlayList.PopPos;

end;

procedure TfMainForm.sgPlayListContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
var
  Zone: TGridZone;
  AbsPos: TPoint;
begin
  Zone := sgPlayList.MouseToGridZone(MousePos.x, MousePos.Y);
  AbsPos := sgPlayList.ClientToScreen(MousePos);
  case zone of
    gzFixedCols, gzFixedRows: pnHeaderPlaylist.PopUp(AbsPos.x, AbsPos.Y);
    gzNormal: PlaylistMenu.PopUp(AbsPos.x, AbsPos.Y);
  end;

end;

procedure TfMainForm.sgPlayListDblClick(Sender: TObject);
begin
  BackEnd.Play(sgPlayList.Row - 1);
  BackEnd.Notify(cpStatus);
  sgPlayList.Invalidate;
end;

procedure TfMainForm.sgPlayListDragDrop(Sender, Source: TObject; X, Y: integer);
begin
  if Source = tvCollection then
    CollectionHandler(True)
  else
  if Source = FilesTree then
    FileSystemHandler(True);

end;

procedure TfMainForm.sgPlayListDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
begin
  Accept := (Source = tvCollection) or (Source = FilesTree);
end;

procedure TfMainForm.sgPlayListDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  aBmp: TBitmap;
  ASong: TCustomSong;
  Txt: string;
  r1, R2: Trect;
  ts: TTextStyle;
  CurrRating: integer;
  TrueCol: integer;
begin
  if (ACol = 0) and (Arow > 0) and (ARow = BackEnd.PlayList.ItemIndex + 1) then
  begin
    aBmp := TBitmap.Create;
    case BackEnd.AudioEngine.State of
      ENGINE_PAUSE: dm.ilSmall.GetBitmap(10, aBmp);
      ENGINE_STOP: dm.ilSmall.GetBitmap(11, aBmp);
      ENGINE_PLAY: dm.ilSmall.GetBitmap(6, aBmp);
      else
        dm.ilSmall.GetBitmap(12, aBmp)
    end;


    if aBmp = nil then exit;
    sgPlayList.Canvas.Draw(arect.Left, aRect.Top, aBmp);
    abmp.Free;
    exit;
  end;

  if aRow = 0 then
    exit;

  ASong := BackEnd.PlayList.Songs[Arow - 1];
  if ASong = nil then exit;
  ASong.LoadTags;

  if aCol = 0 then
    TrueCol := -1
  else
    TrueCol := sgPlayList.Columns[aCol - 1].Tag;

  case TrueCol of
    0: Txt := IntToStr(Arow);
    1: Txt := ASong.Title;
    2: Txt := ASong.Tags.Album;
    3: Txt := ASong.Tags.Artist;
    4: Txt := FormatTimeRange(ASong.Tags.Duration, True);
    5: Txt := ASong.Tags.TrackString;
    6: Txt := ASong.Tags.Genre;
    7: Txt := ASong.Tags.Year;
    8: Txt := ASong.Tags.AlbumArtist;
    9: Txt := ASong.FileName;
    10: Txt := '';
    else
      txt := '';
  end;

  sgPlayList.Canvas.FillRect(aRect);
  ts := sgPlayList.Canvas.textStyle;
  //  if Acol > sgPlayList.FixedCols then
  //      ts.Alignment:=sgPlayList.Columns[Acol -sgPlayList.FixedCols].Alignment;
  ts.Clipping := True;

  //   sgPlayList.Canvas.TextOut(aRect.left, arect.top, txt);;
  if ts.Alignment = taRightJustify then
    aRect.Right := aRect.Right - 3;
  if ts.Alignment = taLeftJustify then
    aRect.Left := aRect.Left + 3;

  if (TrueCol <> 10) or (ASong.ID = -1) then
    sgPlayList.Canvas.TextRect(aRect, aRect.left, arect.top, txt, ts)
  else
  begin
    sgPlayList.Canvas.Draw(arect.left, arect.top, RatingBack);
    CurrRating := ASong.TmpRating;
    if CurrRating = -1 then
      CurrRating := ASong.Rating;
    ASong.TmpRating := -1;
    r1 := Rect(0,
      0,
      trunc(RatingBack.Width * (CurrRating / 10)) - 1,
      RatingBack.Height);
    r2 := r1;
    Types.OffsetRect(R2, aRect.Left, aRect.top);
    sgPlayList.Canvas.CopyRect(r2, RatingFront.Canvas, r1);
  end;

end;

procedure TfMainForm.sgPlayListHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
var
  SortField: TplSortField;
  Direction: TplSortDirection;
  i: integer;
  TrueCol: integer;
begin

  if not IsColumn then
    exit;

  if Index = 0 then
    TrueCol := -1
  else
    TrueCol := sgPlayList.Columns[Index - 1].Tag;

  if TrueCol < 1 then exit;

  case TrueCol of
    1: SortField := stTitle;
    2: SortField := StAlbum;
    3: SortField := stArtist;
    4: SortField := stDuration;
    5: SortField := stTrack;
    6: SortField := stGenre;
    7: SortField := stYear;
    8: SortField := stAlbumArtist;
    9: SortField := stFileName;
    10: SortField := stRating;
    else
      SortField := stNone;
  end;

  if SortField <> BackEnd.PlayList.SortField then
    Direction := sdplAscending
  else
    case BackEnd.PlayList.SortDirection of
      sdplAscending:
        Direction := sdplDiscending;
      sdplDiscending:
        Direction := sdplAscending;
    end;

  for i := 0 to sgPlayList.Columns.Count - 1 do
    if i = (Index - 1) then
    begin
      if Direction = sdplDiscending then
        sgPlayList.Columns[i].Title.ImageIndex := 14
      else
        sgPlayList.Columns[i].Title.ImageIndex := 15;
    end
    else
      sgPlayList.Columns[i].Title.ImageIndex := -1;


  BackEnd.PlayList.Sort(SortField, Direction);
  PlayListChange(Self);

end;

procedure TfMainForm.sgPlayListHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  SetLength(fColumnsWidth, 0);
  sgPlayList.Invalidate;
end;

procedure TfMainForm.sgPlayListHeaderSizing(Sender: TObject; const IsColumn: boolean; const aIndex, aSize: integer);
var
  i: integer;
  diffs: integer;
  VisibleCount: integer;
  Steps: integer;
  Remains: integer;
  LastVisible: integer;
  h: ThackGrid;
begin
  if not IsColumn then
    exit;

  h := THackGrid(sgPlayList);

  if Length(fColumnsWidth) = 0 then
  begin
    SetLength(fColumnsWidth, sgPlayList.Columns.Count);
    for i := 0 to sgPlayList.Columns.Count - 1 do
      fColumnsWidth[i] := sgPlayList.Columns[i].Width;
  end;


  Diffs := (fColumnsWidth[aIndex - sgPlayList.FixedCols] - aSize);
  if Diffs = 0 then
    exit;

  VisibleCount := 0;
  LastVisible := aIndex - sgPlayList.FixedCols;
  for i := aindex to sgPlayList.Columns.Count - 1 do
    if sgPlayList.Columns[i].Visible then
      Inc(VisibleCount);

  for i := 0 to sgPlayList.Columns.Count - 1 do
    if sgPlayList.Columns[i].Visible then
      LastVisible := I;

  Dec(VisibleCount);

  if VisibleCount = -1 then
  begin
    sgPlayList.BeginUpdate;
    //   sgPlayList.Columns[LastVisible].Width := fColumnsWidth[LastVisible];
    sgPlayList.Columns[LastVisible].Width :=
      sgPlayList.Columns[LastVisible].Width + (h.GCache.ClientWidth - h.GCache.GridWidth);
    sgPlayList.EndUpdate(True);
    exit;
  end;

  sgPlayList.Columns[aIndex - sgPlayList.FixedCols].Width := ASize;

  if VisibleCount = 0 then
  begin
    sgPlayList.Columns[LastVisible].Width := fColumnsWidth[LastVisible] + Diffs;
    exit;
  end;

  sgPlayList.BeginUpdate;

  Steps := diffs div VisibleCount;
  remains := diffs mod VisibleCount;

  for i := aindex + 1 to sgPlayList.Columns.Count - 1 do
    if sgPlayList.Columns[i].Visible then
      sgPlayList.Columns[i].Width := fColumnsWidth[i] + Steps;

  for i := (aindex + 1) to (aindex + Remains) do
    if sgPlayList.Columns[i].Visible then
      if diffs > 0 then
        sgPlayList.Columns[i].Width := sgPlayList.Columns[i].Width + 1
      else
        sgPlayList.Columns[i].Width := sgPlayList.Columns[i].Width - 1;

  if h.GCache.GridWidth < h.GCache.ClientWidth then
    sgPlayList.Columns[LastVisible].Width :=
      sgPlayList.Columns[LastVisible].Width + (h.GCache.ClientWidth - h.GCache.GridWidth);

  sgPlayList.EndUpdate(True);

end;

procedure TfMainForm.MoveSelection(KeyDirection: TMovingSelection; Shift: TShiftState; Row: integer);
var
  Displacement: integer;
begin
  Displacement := 0;
  if not (ssShift in Shift) then
    MovingSelection := msNone;

  case KeyDirection of
    msUp: Displacement := 1;
    msDown: Displacement := -1;
  end;

  if (MovingSelection = msNone) and PlaylistSelected.isMultiselection and (Displacement <> 0) then
  begin
    PlaylistSelected.ClearAll;
    PlaylistSelected[row + Displacement] := True;
  end;

  if (ssShift in Shift) then
  begin
    if MovingSelection = msNone then
      MovingSelection := KeyDirection;

    if MovingSelection = KeyDirection then
      PlaylistSelected[Row - Displacement] := True
    else
    begin
      PlaylistSelected[Row] := False;
      if (Row - Displacement) = FAnchor then
        MovingSelection := msNone;
    end;
  end
  else
  if not (ssCtrl in Shift) then
  begin
    PlaylistSelected.ClearAll;
    PlaylistSelected[Row - Displacement] := True;
    MovingSelection := msNone;
    FAnchor := Row - Displacement;
  end;

end;

procedure TfMainForm.sgPlayListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  CurrRow: integer;
  h: THackGrid;
begin
  CurrRow := sgPlayList.Row - 1;

  case key of
    VK_DELETE:
      RemoveSelectionFromPlaylist;
    VK_SPACE:
      if (ssCtrl in Shift) then
        PlaylistSelected[CurrRow] := not PlaylistSelected[CurrRow]
      else
        BackEnd.PlayPause;
    VK_A:
      if (ssCtrl in Shift) then
        PlaylistSelected.SelectAll;
    VK_UP:
      MoveSelection(msUp, Shift, CurrRow);
    VK_DOWN:
      MoveSelection(msDown, Shift, CurrRow);
    VK_PRIOR:
      if (ssShift in Shift) then
      begin
        h := THackGrid(sgPlayList);
        CurrRow := h.GCache.FullVisibleGrid.Top - 1;
        PlaylistSelected.ClearAll;
        PlaylistSelected.SelectRange(FAnchor, FAnchor, CurrRow);
      end;
    VK_NEXT:
      if (ssShift in Shift) then
      begin
        h := THackGrid(sgPlayList);
        CurrRow := h.GCache.FullVisibleGrid.Bottom + 1;
        PlaylistSelected.ClearAll;
        PlaylistSelected.SelectRange(FAnchor, FAnchor, CurrRow);
      end;
    VK_HOME:
      if (ssShift in Shift) then
      begin
        CurrRow := 0;
        PlaylistSelected.ClearAll;
        PlaylistSelected.SelectRange(FAnchor, FAnchor, CurrRow);
      end;
    VK_END:
      if (ssShift in Shift) then
      begin
        CurrRow := PlaylistSelected.size;
        PlaylistSelected.ClearAll;
        PlaylistSelected.SelectRange(FAnchor, FAnchor, CurrRow);
      end;
  end;


  sgPlaylist.invalidate;
end;

procedure TfMainForm.sgPlayListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ACol, ARow: integer;
begin
  sgPlayList.MouseToCell(X, Y, ACol, ARow);
  if ACol = 11 then
    exit;

  if (button = mbLeft) and (not (ssShift in Shift) and not (ssCtrl in Shift)) then
  begin
    MovingSelection := msNone;
    fSourceIndex := ARow;
    FAnchor := ARow - 1;
    PlaylistSelected.ClearAll;
  end;

  if (Button = mbRight) then
  begin
    if not (PlaylistSelected.isMultiselection) then
    begin
      PlaylistSelected.ClearAll;
      PlaylistSelected[ARow - 1] := True;
    end;
  end
  else
  if (ARow > 0) then
    PlaylistSelected[ARow - 1] := not PlaylistSelected[ARow - 1];

  if (FAnchor <> -1) and (ssShift in Shift) then
  begin
    PlaylistSelected.ClearAll;
    PlaylistSelected.SelectRange(FAnchor, fAnchor, ARow - 1);
  end;

  sgPlayList.Invalidate;

end;

procedure TfMainForm.sgPlayListMouseLeave(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to BackEnd.PlayList.Count - 1 do
    BackEnd.PlayList[i].TmpRating := -1;
  sgPlayList.invalidate;
end;

procedure TfMainForm.sgPlayListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  ACol, ARow: integer;
  R1: TRect;
  Rating: integer;
begin

  sgPlayList.MouseToCell(x, y, ACol, ARow);
  if (ARow > 0) and (aCol = 11) and (BackEnd.PlayList[ARow - 1].ID <> -1) then
  begin
    R1 := sgPlayList.CellRect(ACol, ARow);
    Rating := trunc(((x - R1.Left) * 10) / RateStars.Width);
    BackEnd.PlayList[ARow - 1].TmpRating := rating;
    //       sgPlayList.InvalidateCell(ACol, ARow);
    sgPlayList.Invalidate;
  end;

  if fSourceIndex < 1 then
    exit;

  if not (ssLeft in Shift) and (not (ssShift in Shift) and not (ssCtrl in Shift)) then
    exit;

  if (Arow < 1) or
    (Arow = fSourceIndex) then
    exit;
  fTargetIndex := ARow;

  sgPlayList.MoveColRow(False, fTargetIndex, fSourceIndex);

  // sgPlayList.Selection := rect(fTargetIndex, 0, fTargetIndex, 1);

  sgPlayList.Repaint;
  fSourceIndex := fTargetIndex;

end;

procedure TfMainForm.sgPlayListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
//var
//   mycol, myrow : Integer;
begin
  fSourceIndex := -1;
  fTargetIndex := -1;

end;

procedure TfMainForm.sgPlayListPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
begin
  if aRow = 0 then exit;
  if ARow = BackEnd.PlayList.ItemIndex + 1 then
    sgPlaylist.Canvas.Font.Style := [fsUnderline];


  sgPlayList.Canvas.Font.Color := clWindowText;  // colore di default
  sgPlayList.Canvas.Brush.Color := clWindow;

  if PlaylistSelected[ARow - 1] then
  begin        // Will only color selected rows
    sgPlayList.Canvas.Font.Color := clHighlightText;
    sgPlayList.Canvas.Brush.Color := clHighlight;
  end;

end;

procedure TfMainForm.sgPlayListResize(Sender: TObject);
begin
  AdaptSize(False);
end;

procedure TfMainForm.sgStatsClick(Sender: TObject);
var
  ACol, ARow: integer;
  p: Tpoint;
  theform: TfScanResult;
begin
  p := sgStats.ScreenToControl(Mouse.CursorPos);
  sgStats.MouseToCell(p.x, p.y, ACol, ARow);

  if (aCol = 1) and (sgStats.Cells[aCol, aRow] <> '0') then
  begin
    theform := TfScanResult.Create(self);
    Theform.Load(TScannedStatus(arow));
    theform.Show;
  end;

end;

procedure TfMainForm.sgStatsPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
begin
  if (aCol = 1) and (sgStats.Cells[aCol, aRow] <> '0') then
    if gdHot in aState then
      sgStats.Canvas.Font.Style := [fsUnderline, fsBold]
    else
      sgStats.Canvas.Font.Style := [fsUnderline];
end;

procedure TfMainForm.slVolumeChange(Sender: TObject);
begin
  //  DebugLn('TfMainForm.slVolumeChange','->',IntToStr(slVolume.Position));
  BackEnd.SetVolume(slVolume.Position);

end;

procedure TfMainForm.btnCloseCollectionStatClick(Sender: TObject);
begin
  sgStats.Clean;
  sgStats.RowCount := 1;
  gbStats.Visible := False;
end;

procedure TfMainForm.slVolumeClick(Sender: TObject);
begin
  BackEnd.SetVolume(slVolume.Position);
end;

procedure TfMainForm.TimerTimer(Sender: TObject);
begin
  //if (BackEnd.AudioEngine.State = ENGINE_PLAY)  then
  //begin
  //  if not seeking then
  //    TrackBar.Position := BackEnd.Position;
  //end;

  //lbTime.Caption := TimeToStr(BackEnd.Position / MSecsPerDay);
end;

procedure TfMainForm.btnUpDirClick(Sender: TObject);
begin
  LoadDir(UpperDirectory(CurrentPath));
end;

procedure TfMainForm.tbRepeatClick(Sender: TObject);
begin

  RepeatMenu.PopUp;
end;

procedure TfMainForm.TrackBarChange(Sender: TObject);
begin
  Backend.Position := TrackBar.Position;
end;

procedure TfMainForm.TrackBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  newPosition: integer;
begin
  exit;
  if ssLeft in Shift then
  begin
    Seeking := True;
    TrackBar.Cursor := crHSplit;
    newPosition := Round(x * TrackBar.Max / TrackBar.ClientWidth);
    TrackBar.Position := newPosition;
	Backend.Position := newPosition;
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

procedure TfMainForm.TrackDblClick(Sender: TObject);
var
  info: TfSongInfo;
begin
  if not assigned(BackEnd.PlayList.CurrentItem) then
    exit;

  info := TfSongInfo.Create(Application);
  info.InitFromFile(BackEnd.PlayList.CurrentItem.FullName);
  info.Show;
end;

procedure TfMainForm.TrayIconClick(Sender: TObject);
begin
  //  ShowNotification;
end;

procedure TfMainForm.TrayIconDblClick(Sender: TObject);
begin
  if Visible then
  begin
    Hide;
    {$IFDEF TASKBAR_EXTENSION}
    if Mytaskbar_ext.Initialized then
      Mytaskbar_ext.UnInit;
    {$ENDIF}
  end
  else
  begin
    Show;
    BringToFront;
    {$IFDEF TASKBAR_EXTENSION}
    Mytaskbar_ext.Update;
    {$ENDIF}
  end;
end;

function TfMainForm.AdjustPos(pt: tpoint): Tpoint;
begin

  Result.x := pt.x;

  if pt.y < Screen.Monitors[0].WorkareaRect.Top then
    Result.y := Screen.Monitors[0].WorkareaRect.Top
  else
  if pt.y > Screen.Monitors[0].WorkareaRect.bottom then
    Result.y := Screen.Monitors[0].WorkareaRect.bottom
  else
    Result.y := pt.y;

end;


procedure TfMainForm.TrayIconMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  pt: Tpoint;
begin
  if button = mbMiddle then
  begin
    if not Assigned(fMiniPlayer) then
      fMiniPlayer := TfMiniPlayer.Create(Self);

    if fMiniPlayer.Visible then
      fMiniPlayer.Hide
    else
    begin
      pt := TrayIcon.GetPosition;
      fMiniPlayer.ShowAtPos(pt.x, pt.y);
    end;
  end;

  {$IFDEF WINDOWS}
  if Button = mbRight then
  begin
    SetForegroundWindow(TrayIcon.Handle);
    PostMessage(TrayIcon.Handle, 0, 0, 0);
    TrayMenu.PopUp(x, y);
  end;
  {$ENDIF}
  {$IFDEF LINUX}
  if (Button = mbRight) then
    if not TrayMenuActive then
    begin
      pt := AdjustPos(TrayIcon.GetPosition); //Mouse.CursorPos;
      TrayMenuActive := True;
      TrayMenu.PopUp(pt.x, pt.y);
    end
    else
      TrayMenuActive := False;

  {$ENDIF}

end;

procedure TfMainForm.PlaylistTreeDblClick(Sender: TObject);
var
  item: TPlayListTreeNode;
begin
  item := TPlayListTreeNode(PlaylistTree.Selected);
  if item = nil then
    exit;
  if item.FullPath <> EmptyStr then
    if item.Automatic then
      PlayListHandler(True);
  ReloadPlayList;
end;

procedure TfMainForm.tsDirectoryShow(Sender: TObject);
begin
  if CurrentPath = EmptyStr then
    LoadDir(GetUserDir);
end;

procedure TfMainForm.tsPlayListShow(Sender: TObject);
begin
  LoadAutomaticPlaylist;
end;

procedure TfMainForm.tvCollectionCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TMusicTreeNode;
end;

procedure TfMainForm.tvCollectionDblClick(Sender: TObject);
begin
  CollectionHandler(True);
end;

procedure TfMainForm.tvCollectionGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  node.ImageIndex := SortArray[TMusicTreeNode(Node).Kind].ImageIndex;
  node.SelectedIndex := node.ImageIndex;

end;

procedure TfMainForm.LoadDir(Path: string);
var
  DirList: TStringList;
  FileList: TStringList;
  i: integer;
  Node: TFileTreeNode;
begin
  DirList := TStringList.Create;
  FileList := TStringList.Create;
  FileList.OwnsObjects := True;
  CurrentPath := IncludeTrailingPathDelimiter(Path);
  try
    FilesTree.Items.Clear;
    BuildFolderList(CurrentPath, DirList);
    DirList.Sort;
    BuildFileList(IncludeTrailingPathDelimiter(CurrentPath) + AudioTag.SupportedExtension,
      faAnyFile, FileList, False);
    FileList.Sort;
    for i := 0 to DirList.Count - 1 do
    begin
      node := TFileTreeNode(FilesTree.items.AddChild(nil, ExtractFileName(DirList[i])));
      node.FullPath := DirList[i];
      node.isDir := True;
      ;
    end;

    for i := 0 to FileList.Count - 1 do
    begin
      node := TFileTreeNode(FilesTree.items.AddChild(nil, ExtractFileName(FileList[i])));
      node.FullPath := FileList[i];
      node.isDir := False;
    end;

  finally
    DirList.Free;
    ;
    FileList.Free;
  end;
  ePath.Directory := Path;

  if PathHistory.IndexOf(Path) < 0 then
    PathIndex := PathHistory.Add(Path);

end;

procedure TfMainForm.OnLoaded(Sender: TObject);
begin
  Backend.Notify(cpPlaylist);
  ReloadPlayList;
  CheckWelcomeMode;

end;

procedure TfMainForm.ReloadPlayList;
var
  oldtop: integer;
begin

  oldtop := sgPlayList.TopRow;
  sgPlayList.Clear;
  sgPlayList.RowCount := BackEnd.PlayList.Count + 1;
  PlaylistSelected.Size := sgPlayList.RowCount - 1;
  PlaylistSelected.Clearall;

  AdaptSize;
  sgPlayList.TopRow := oldtop;
  sgPlayList.Invalidate;
  //  Application.ProcessMessages;

end;

procedure TfMainForm.ClearPanelInfo;
var
  lFile: string;
begin
  Title.Caption := rNotPlaying;
  Album.Caption := '';
  Artist.Caption := '';
  //  Track.Caption     := '';
  TrackBar.Position := 0;
  lbTime.Caption := FormatTimeRange(0);
  lFile := backend.Config.GetResourcesPath + 'logo.png';
  if not FileExists(lFile) then
    DebugLn('[TfMainForm.ClearPanelInfo] File not found: ' + lFile)
  else
    imgCover.Picture.LoadFromFile(lFile);
  Caption := DisplayAppName;
end;

procedure TfMainForm.OnEngineCommand(Sender: TObject; Command: TEngineCommand);
begin
  case Command of
    ecStop: ClearPanelInfo;
    ecPause, ecPlay: sgPlayList.InvalidateRow(BackEnd.PlayList.ItemIndex + 1);
  end;
end;

procedure TfMainForm.OnExternalCommand(Sender: TObject; Command: RExternalCommand; var Handled: boolean);
begin
  if Command.Category = CATEGORY_APP then
    case Command.command of
      COMMAND_ACTIVATE:
      begin
        //if WindowState = wsMinimized then
        //   WindowState := wsNormal;
        // FormStyle := fsNormal;
        Handled := True;
        ShowOnTop;
        Show;
      end;
    end;

  if Command.Category = CATEGORY_ACTION then
    case Command.command of
      COMMAND_SEEK_P:
      begin
        dm.actSkipForward.Execute;
        Handled := True;
      end;
      COMMAND_SEEK_M:
      begin
        dm.actSkipBackward.Execute;
        Handled := True;
      end;
    end;

end;

procedure TfMainForm.AdaptSize(Recalculate: boolean = True);
var
  i: integer;
  ColWidths: array of integer = nil;
  TotalSize: integer;
  diffs: integer;
  VisibleCount: integer;
  HighP, LowP: integer;
  Steps: integer;
  Remains: integer;
  h: ThackGrid;
  oldTop: integer;
begin
  oldtop := sgPlayList.TopRow;
  HighP := 0;
  LowP := 0;
  TotalSize := 0;
  VisibleCount := 0;
  SetLength(ColWidths, sgPlayList.Columns.Count);
  if Recalculate then
  begin
    for I := 0 to sgPlayList.Columns.Count - 1 do
      if sgPlayList.Columns[i].Visible then
      begin
        ColWidths[i] := ColumnSize(i) + 3;
        Inc(VisibleCount);
        Inc(TotalSize, ColWidths[i]);
        if sgPlayList.Columns[i].SizePriority > 0 then
          Inc(HighP)
        else
          Inc(LowP);
      end
      else
        ColWidths[i] := 0;
  end
  else
    for I := 0 to sgPlayList.Columns.Count - 1 do
      if sgPlayList.Columns[i].Visible then
      begin
        ColWidths[i] := sgPlayList.Columns[i].Width;
        Inc(VisibleCount);
        Inc(TotalSize, ColWidths[i]);
        if sgPlayList.Columns[i].SizePriority > 0 then
          Inc(HighP)
        else
          Inc(LowP);

      end
      else
        ColWidths[i] := 0;

  h := THackGrid(sgPlayList);


  Diffs := h.gcache.ClientWidth - h.gcache.FixedWidth - TotalSize - 1;

  Steps := diffs div HighP;// VisibleCount;
  remains := abs(diffs mod HighP);// VisibleCount;

  for I := 0 to sgPlayList.Columns.Count - 1 do
    if sgPlayList.Columns[i].Visible and (sgPlayList.Columns[i].SizePriority > 0) then
      ColWidths[i] := ColWidths[i] + Steps;

  i := 0;
  while (i < sgPlayList.Columns.Count) and (remains > 0) do
  begin
    if sgPlayList.Columns[i].Visible and not (sgPlayList.Columns[i].SizePriority > 0) then
    begin
      if diffs > 0 then
        ColWidths[i] := ColWidths[i] + 1
      else
        ColWidths[i] := ColWidths[i] - 1;
      Dec(Remains);
    end;
    Inc(i);
  end;

  for I := 0 to sgPlayList.Columns.Count - 1 do
    if sgPlayList.Columns[i].Visible then
      sgPlayList.Columns[i].Width := ColWidths[i];
  sgPlayList.TopRow := oldtop;
end;

end.

