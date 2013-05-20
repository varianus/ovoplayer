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
  AudioEngine, GUIBackEnd, Config, MediaLibrary,
  DefaultTranslator, Grids, EditBtn, ActnList, customdrawncontrols,
  customdrawn_common, customdrawn_ovoplayer, ucover;

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

  TMusicTreeNode = class ( TTreeNode)
    public
      ID:      integer;
      Kind:    TTagKind;
  end;

  TFileTreeNode = class ( TTreeNode)
    public
      FullPath: string;
      isDir:    boolean;
  end;

  TDWordArray = array [0..$FFFFFF] of Dword;
  PDWordArray = ^TDWordArray;

  TMovingSelection = (msNone, msUp, msDown);

  { TRowsSelection }

  TRowsSelection = class (TObject)
    Private
      fArray: PDWordArray;
      fDwordSize: integer;
      fsize:integer;
      function GetSelected(Index: integer): boolean;
      procedure SetSelected(Index: integer; AValue: boolean);
    public
      Constructor Create;
      Procedure ClearAll;
      Procedure SelectAll;
      Procedure SetSize(Size:Integer);
      function  FirstSelected : integer;
      function  NextSelected(index:Integer=-1) : integer;
      Function  isMultiselection:boolean;
      property Selected[Index:integer]:boolean read GetSelected write SetSelected; default;
      Procedure SelectRange(Var Anchor: Integer;OldRow, NewRow: integer);
  end;


  TfMainForm = class(TForm)
    actShowLeft: TAction;
    actShowPLMediainfo: TAction;
    actShowAbout: TAction;
    ActShowPreferences: TAction;
    ActionList: TActionList;
    Artist:     TLabel;
    cbGroupBy:  TComboBox;
    FilesTree: TTreeView;
    RateStars: TImageList;
    MenuItem21: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    slVolume: TCDTrackBar;
    sgPlayList: TStringGrid;
    TrackBar: TCDTrackBar;
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
    sgStats: TStringGrid;
    btnFilterCancel: TSpeedButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
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
    procedure actShowAboutExecute(Sender: TObject);
    procedure actShowLeftExecute(Sender: TObject);
    procedure actShowPLMediainfoExecute(Sender: TObject);
    procedure ActShowPreferencesExecute(Sender: TObject);
    procedure BackEndSongStart(Sender: TObject);
    procedure btnBackDirClick(Sender: TObject);
    procedure btnFilterCancelClick(Sender: TObject);
    procedure btnForwardDirClick(Sender: TObject);
    procedure btnHomeDirClick(Sender: TObject);
    procedure cbGroupByChange(Sender: TObject);
    procedure CollectionTreeDblClick(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure ePathAcceptDirectory(Sender: TObject; var Value: String);
    procedure ePathEditingDone(Sender: TObject);
    procedure FilesTreeCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure FilesTreeDblClick(Sender: TObject);
    procedure FilesTreeGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure imgCoverDblClick(Sender: TObject);
    procedure mnuRestoreClick(Sender: TObject);
    procedure mnuEnqueueItemsClick(Sender: TObject);
    procedure mnuFileInfoClick(Sender: TObject);
    procedure mnuInfoClick(Sender: TObject);
    procedure mnuPlayItemsClick(Sender: TObject);
    procedure mnuRemovePlaylistClick(Sender: TObject);
//    procedure PlaylistMenuPopup(Sender: TObject);
    procedure pmdirectoriesPopup(Sender: TObject);
    procedure pnCollectionPopup(Sender: TObject);
    procedure pnHeaderPlaylistPopup(Sender: TObject);
    procedure sgPlayListColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure sgPlayListContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure sgPlayListDblClick(Sender: TObject);
    procedure sgPlayListDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure sgPlayListHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure sgPlayListHeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure sgPlayListHeaderSizing(sender: TObject; const IsColumn: boolean;
      const aIndex, aSize: Integer);
    procedure sgPlayListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgPlayListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sgPlayListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sgPlayListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sgPlayListPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure sgPlayListResize(Sender: TObject);
    procedure slVolumeChange(Sender: TObject);
    procedure btnCloseCollectionStatClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure btnUpDirClick(Sender: TObject);
    procedure tbRepeatClick(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure TrackBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure TrackBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure TrackDblClick(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure TrayIconMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PlaylistTreeDblClick(Sender: TObject);
    procedure tsDirectoryShow(Sender: TObject);
    procedure tvCollectionCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure tvCollectionDblClick(Sender: TObject);
    procedure tvCollectionGetImageIndex(Sender: TObject; Node: TTreeNode);
  private
    SeekIng:      boolean;
    fSourceIndex, fTargetIndex: Integer;
    CurrentCover: string;
    SortFields:   TSortFields;
    CurrentPath : string;
    PathHistory : TstringList;
    PathIndex : Integer;
    sortColumn : integer;
    PlaylistSelected: TRowsSelection;
    MovingSelection : TMovingSelection;
    FAnchor: integer;
    fColumnsWidth : array of integer;
    RatingBack, RatingFront:TBitmap;
    Function ColumnSize(aCol: Integer):Integer;
    procedure ClearPanelInfo;
    procedure CollectionHandler(Enqueue: boolean);
    procedure LoadDir(Path: string);
    procedure LoadTree;
    procedure MediaLibraryScanComplete(Sender: TObject; _Added, _Updated, _Removed, _Failed: integer);
    procedure MoveSelection(KeyDirection: TMovingSelection; Shift: TShiftState; Row:Integer);
    procedure OnLoaded(Sender: TObject);
    procedure OnMenuItemClick(Sender: TObject);
    procedure PlayListChange(Sender: TObject);
    procedure MediaLibraryScanBegin(Sender: TObject);
    procedure AdaptSize(Recalculate: boolean = true);
    function PrepareFields: string;
    function PrepareFilter: string;
    function PrepareImportFilter(Node: TMusicTreeNode): string;
    procedure ReloadPlayList;
    procedure OnEngineCommand(Sender: Tobject; Command : TEngineCommand);
    procedure OnExternalCommand(Sender: Tobject; Command : String);
    procedure SaveConfig(Sender: TObject);
    procedure ReadConfig(Sender: TObject);
    procedure RemoveSelectionFromPlaylist;
    procedure ScrollIntoView;
  public
    { public declarations }
  end;

var
  fMainForm: TfMainForm;

implementation

{$R *.lfm}
uses AppConsts, lclType, AudioTag, LCLProc, FilesSupport,
     uConfig, uMiniPlayer, uSongInfo, uAbout, baseTag,
     Math;

type

  THackGrid = Class(TStringGrid)
    public
      property Gcache;
  end;

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

{ TRowsSelection }

function TRowsSelection.GetSelected(Index: integer): boolean;
var
  DWordIndex, BitIndex:Integer;
begin
  if fDwordSize < 1 then
     begin
       result := false;
       exit;
     end;
  DivMod(Index, 32, DWordIndex, BitIndex);
  Result :=  (fArray^[DWordIndex] and DWORD(1 shl (BitIndex))) <> 0;
//  DebugLn('Get ', inttostr(fArray^[DWordIndex]), ' ', BoolToStr(Result, true), ' ', IntToStr(index) );
end;

procedure TRowsSelection.SetSelected(Index: integer; AValue: boolean);
var
  DWordIndex, BitIndex:Integer;
begin
    if fDwordSize < 1 then
     begin
       exit;
     end;
  DivMod(Index, 32, DWordIndex, BitIndex);
  if AValue  then
     fArray^[DWordIndex] := fArray^[DWordIndex] or DWORD(1 shl (BitIndex))
  else
     fArray^[DWordIndex] := fArray^[DWordIndex] and not DWORD(1 shl (BitIndex));
//  DebugLn('Set ', inttostr(fArray^[DWordIndex]), ' ', BoolToStr(AValue, true), ' ', IntToStr(index) );
end;

constructor TRowsSelection.Create;
begin
  fDwordSize:=-1;
  fArray:=nil;
end;

procedure TRowsSelection.ClearAll;
var i :integer;
begin
  for i := 0 to fDwordSize -1 do
    fArray^[i] := DWord($0);
end;

procedure TRowsSelection.SelectAll;
var i :integer;
begin
  for i := 0 to fDwordSize -1 do
    fArray^[i] := DWord(not $0);
end;

procedure TRowsSelection.SetSize(Size: Integer);
begin
  fDwordSize:= ceil(Size / 32);
  ReAllocMem(fArray, fDwordSize * SizeOf(DWORD));
  fsize:=Size;
  ClearAll;
end;

function TRowsSelection.FirstSelected: integer;
const
  fSkippableValue= $00000000;
var
  DWordIndex, BitIndex:Integer;
  tmpDWord: integer;

begin
  Result   := -1;
  BitIndex := 0;
  for DWordIndex := 0 to fDwordSize -1 do
    begin
      if fArray^[DWordIndex] = fSkippableValue then
         Continue;
      tmpDWord:= fArray^[DWordIndex];
      BitIndex := 0;
      while  (tmpDWord and 1) = 0 do
        begin
         tmpDWord:=tmpDWord shr 1;
         inc(BitIndex);
        end;
      Result := DWordIndex * 32 + BitIndex;
      exit;
    end;


end;

function TRowsSelection.NextSelected(index:Integer=-1): integer;
var i:integer;
begin
   Result := -1;
   if index = -1 then
      begin
         Result := FirstSelected;
         exit;
      end;

   for i := (index+1) to fsize -1 do
     if GetSelected(i) then
        begin
          Result:= i;
          exit;
        end;
end;

function TRowsSelection.isMultiselection: boolean;
var
  idx : integer;
begin
  idx := FirstSelected;
  result := not(NextSelected(idx) = -1);

end;

procedure TRowsSelection.SelectRange(Var Anchor: Integer;OldRow, NewRow: integer);
var
  dir: integer;
  sel: boolean;
begin
  if OldRow = NewRow then
    exit;
  if Anchor = -1 then
    Anchor:=OldRow;
  dir:=Sign(NewRow - OldRow);
  if Sign(Anchor - OldRow) <> Sign(Anchor - NewRow) then
    while OldRow <> Anchor do begin
      SetSelected(OldRow,False);
      Inc(OldRow, dir);
    end;
  sel:=Abs(Anchor - OldRow) < Abs(Anchor - NewRow);
  while OldRow <> NewRow do begin
    SetSelected(OldRow, sel);
    Inc(OldRow, dir);
  end;
  SetSelected(NewRow, true);
end;

{ TfMainForm }
procedure TfMainForm.CollectionTreeDblClick(Sender: TObject);
begin
  CollectionHandler(true);
end;

procedure TfMainForm.CollectionHandler(Enqueue:boolean);
var
  Node:     TMusicTreeNode;
  aSong:    TSong;

begin
  Node := TMusicTreeNode(tvCollection.GetFirstMultiSelected);
  while Node <> nil do
  begin
    if not Enqueue then
       BackEnd.actPlayListClear.Execute;

    case node.Kind of
      tkSong:
        begin
        aSong := TSong.Create(BackEnd.mediaLibrary.FullNameFromID(Node.ID));
        BackEnd.PlayList.Add(ASong);
        end
      else
        BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList, PrepareImportFilter(Node));
      end;
   Node:= TMusicTreeNode(node.GetNextMultiSelected);
  end;

  OnLoaded(sgPlayList);

  if not Enqueue and (BackEnd.PlayList.Count > 0) then
     begin
       BackEnd.PlayList.ItemIndex:=0;
       BackEnd.actPlay.Execute;
     end;
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
  if (PathIndex+1) < PathHistory.Count then
     PathHistory.Capacity:= PathIndex;
  LoadDir(ePath.Directory);

end;

procedure TfMainForm.FilesTreeCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass:= TFileTreeNode;
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

procedure TfMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if BackEnd.Config.InterfaceParam.MinimizeOnClose then
    begin
      Application.ShowMainForm:=False;
      CloseAction:=caHide;
    end
  else
    begin
      BackEnd.SaveState;
      SaveConfig(nil);
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
// mcmcmcmc scrollinto
  ScrollIntoView;

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
          imgCover.Hint:= rEmbedded;
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
          imgCover.Hint:=imgName;
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

  ScrollIntoView;

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

procedure TfMainForm.actShowLeftExecute(Sender: TObject);
begin
  pcmain.Visible:= actShowLeft.checked;
end;

procedure TfMainForm.actShowPLMediainfoExecute(Sender: TObject);
var
  info : TfSongInfo;
  Index: Integer;
  fileList: TStringList;
begin
  Index:= PlaylistSelected.FirstSelected;
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

    Info.show;

  finally
    FreeAndNil(FileList);
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


function TfMainForm.PrepareImportFilter(Node: TMusicTreeNode): string;
var
  wrkNode:  TMusicTreeNode;
begin
  Result  := '0=0';
  wrkNode := Node;
  repeat
    Result   := Result + format(' and %s = %s ', [SortArray[wrkNode.Kind].FieldName,
      QuotedStr(wrkNode.Text)]);
    wrkNode  := TMusicTreeNode(wrkNode.Parent);
  until (wrkNode = nil) or (wrkNode.Index = 0);

end;


procedure TfMainForm.LoadTree;
var
  tags:     TCommonTags;
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

    BackEnd.mediaLibrary.ReadBegin(PrepareFields, PrepareFilter);

    while not BackEnd.mediaLibrary.ReadComplete do
      begin
        Tags := BackEnd.mediaLibrary.ReadItem;
        if UpperCase(TagValue(Tags, SortFields.F1)) <> L1Key then
           begin
             L1Item   := TMusicTreeNode(TVCollection.Items.Add(nil, TagValue(Tags, SortFields.F1)));
             L1Key    := UpperCase(TagValue(Tags, SortFields.F1));
             if L1Item.Text = EmptyStr then L1Item.Text := rEmptyTag;
             L1Item.Kind := SortFields.F1;
             L2Key    := FakeValue;
           end;

        if UpperCase(TagValue(Tags, SortFields.F2)) <> L2Key then
           begin
             L2Item   := TMusicTreeNode(TVCollection.Items.AddChild(L1Item, TagValue(Tags, SortFields.F2)));
             L2Key    := UpperCase(TagValue(Tags, SortFields.F2));
             if L2Item.Text = EmptyStr then L2Item.Text := rEmptyTag;
             L2Item.Kind := SortFields.F2;
           end;

        L3Item   := TMusicTreeNode(TVCollection.Items.AddChild(L2Item, TagValue(Tags, SortFields.F3)));
        L3Item.Kind := SortFields.F3;
        L3Item.ID := tags.ID;
        Finalize(Tags);
        BackEnd.mediaLibrary.NextItem;
      end;

    finally
    tvCollection.EndUpdate;
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
  PlaylistSelected := TRowsSelection.Create;
  PathHistory := TStringList.Create;
  PathHistory.Duplicates := dupIgnore;

  TrackBar.Position:= 0;

  SortFields.F1 := tkAlbumArtist;
  SortFields.F2 := tkAlbum;
  SortFields.F3 := tkSong;

  CurrentPath:=EmptyStr;
  ClearPanelInfo;

  //Cache bitmap used for rating column painting
  RatingBack := TBitmap.Create;
  RatingFront := TBitmap.Create;
  RateStars.GetBitmap(1, RatingBack);
  RateStars.GetBitmap(0, RatingFront);

  BackEnd.OnPlayListChange := @PlayListChange;
  BackEnd.AudioEngine.OnSongStart := @BackEndSongStart;
  BackEnd.OnSaveInterfaceState:= @SaveConfig;

  BackEnd.OnPlayListLoad  := @OnLoaded;
  BackEnd.OnEngineCommand := @OnEngineCommand;
  BackEnd.OnExternalCommand := @OnExternalCommand;

  BackEnd.mediaLibrary.OnScanComplete := @MediaLibraryScanComplete;
  BackEnd.mediaLibrary.OnScanStart:=@MediaLibraryScanBegin;

  slVolume.Position := BackEnd.Config.EngineParam.Volume;
  slVolume.Max:= BackEnd.AudioEngine.MaxVolume;

  ReadConfig(Self);

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
         ScrollIntoView;
         Application.ProcessMessages;
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

  cbGroupBy.ItemIndex := BackEnd.Config.InterfaceParam.GroupBy;
  cbGroupBy.OnChange(self);
  SetLength(fColumnsWidth, 0);

end;

procedure TfMainForm.FormDestroy(Sender: TObject);
begin
  PathHistory.Free;
  PlaylistSelected.Free;
  RatingBack.Free;
  RatingFront.Free;

end;

procedure TfMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  BackEnd.Manager.ImportFromStringArray(FileNames, BackEnd.PlayList);
  BackEnd. SignalPlayListChange;
end;

procedure TfMainForm.FormHide(Sender: TObject);
begin
  Application.ShowMainForm:=False;
end;

procedure TfMainForm.FormResize(Sender: TObject);
begin
  //AdaptSize;
end;

procedure TfMainForm.FormShow(Sender: TObject);
begin
   ScrollIntoView;

  if (BackEnd.AudioEngine = nil) or (BackEnd.AudioEngine.GetEngineName = 'dummy') then
     begin
       ShowMessage(rMissingConfig);
       ActShowPreferences.Execute;
     end;

end;

procedure TfMainForm.imgCoverDblClick(Sender: TObject);
var
  coverForm: TfCover;
begin
  coverForm := TfCover.Create(Self);
  try
    coverForm.ImageCover.Picture.Assign(imgCover.Picture);
    coverForm.SetSize(imgCover.Picture.Height,imgCover.Picture.Width);
    coverForm.ShowModal;
  finally
     coverForm.free;
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
  Index: Integer;
begin
  index := PlaylistSelected.FirstSelected;
  While Index > -1 do
    begin
      BackEnd.PlayList.Delete(Index);
      index:= PlaylistSelected.NextSelected(index);
    end;

  OnLoaded(sgPlayList);
end;

procedure TfMainForm.ScrollIntoView;
begin
  sgplaylist.Row:= BackEnd.PlayList.ItemIndex +1;
end;
procedure TfMainForm.SaveConfig(Sender: TObject);
var
  tmpSt: TStringList;
  i: integer;
begin
  tmpSt := TStringList.Create;
  try
    for i := 1 to sgPlayList.Columns.Count -1 do
      begin
        tmpst.Add(Inttostr(i)+'='+
                  sgPlayList.Columns[i].Title.Caption+';'+
                  IntToStr(sgPlayList.Columns[i].Index)+';'+
                  BoolToStr(sgPlayList.Columns[i].Visible,'Y','N')+';'+
                  IntToStr(sgPlayList.Columns[i].Width)+';'
                  );
      end;
    BackEnd.Config.SaveCustomParams('PlayListGrid', tmpSt);

    tmpSt.clear;
    tmpSt.Values['Height']:= IntToStr(Height);
    tmpSt.Values['Width']:= IntToStr(Width);
    tmpSt.Values['Top']:= IntToStr(Top);
    tmpSt.Values['Left']:= IntToStr(Left);
    tmpSt.Values['LeftPanelVisible']:= BoolToStr(actShowLeft.checked, true);
    tmpSt.Values['ActivePage']:= IntToStr(pcMain.ActivePageIndex);
    BackEnd.Config.SaveCustomParams('MainForm', tmpSt);

  finally
    tmpSt.Free;
  end;


end;

procedure TfMainForm.ReadConfig(Sender: TObject);
var
  tmpSt: TStringList;
  info: TStringList;
  i: integer;
  Col: integer;
  Fase :Integer;
const
  SectionPlayListGrid = 'PlayListGrid';
  SectionMainForm = 'MainForm';

begin
  tmpSt := TStringList.Create;
  info  := TStringList.Create;
  try
    Fase := 1;
    BackEnd.Config.ReadCustomParams(SectionPlayListGrid, tmpSt);
    for i := 1 to tmpSt.Count -1 do
      begin
        info.Clear;
        info.StrictDelimiter := true;
        info.Delimiter := ';';
        info.DelimitedText := tmpSt[i];
        Col := StrToint(tmpSt.Names[i]);
        sgPlayList.Columns[Col].Index:= StrToint(info[1]);
        sgPlayList.Columns[col].visible := Info[2] = 'Y';
        sgPlayList.Columns[Col].Width:= StrToint(info[3]);

      end;
    Fase := 2;
    tmpSt.Clear;
    BackEnd.Config.ReadCustomParams(SectionMainForm, tmpSt);

    Height := StrToIntDef(tmpSt.Values['Height'], Height);
    Width := StrToIntDef(tmpSt.Values['Width'], Width);
    Top := StrToIntDef(tmpSt.Values['Top'], Top);
    Left := StrToIntDef(tmpSt.Values['Left'], Left);

    actShowLeft.checked := not (StrToBoolDef(tmpSt.Values['LeftPanelVisible'], true));
    actShowLeft.Execute;
    pcMain.ActivePageIndex := StrToIntDef(tmpSt.Values['ActivePage'], 0);


  Except
    // if problem loading columns size, remove that info from config
    // needed on 0.5 -> 1.0 upgrade
    if Fase = 1 then BackEnd.Config.RemoveSection(SectionPlayListGrid);
    if Fase = 2 then BackEnd.Config.RemoveSection(SectionMainForm);
  end;
  tmpSt.free;
  info.free;

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
      Info.show;
    end;

  fileList := TStringList.Create;
  try
     Node:= TFileTreeNode(FilesTree.GetFirstMultiSelected);
     while node <> nil do
       begin
         if not Node.isDir then
            fileList.Add(Node.FullPath);
         Node:=TFileTreeNode(Node.GetNextMultiSelected);
       end;
    info := TfSongInfo.Create(Application);
    info.InitFromList(FileList);
    Info.show;

  finally
    FreeAndNil(FileList);
  end;

end;


procedure TfMainForm.mnuInfoClick(Sender: TObject);
var
  info : TfSongInfo;
  Node: TMusicTreeNode;
  fileList: TStringList;
begin

  Node:= TMusicTreeNode(tvCollection.GetFirstMultiSelected);
  if node = nil then
     exit;

  if tvCollection.SelectionCount = 1 then
     begin
      if (Node.Kind = tkSong) then
      begin
        info := TfSongInfo.Create(Application);
        info.InitFromFile(BackEnd.mediaLibrary.FullNameFromID(Node.ID));
        Info.show;
      end;
     end
  else
     begin
       fileList := TStringList.Create;
       try
          Node:= TMusicTreeNode(tvCollection.GetFirstMultiSelected);
          while node <> nil do
            begin
              if (node.Kind = tkSong) then
                 fileList.Add(BackEnd.mediaLibrary.FullNameFromID(Node.ID));
              Node:=TMusicTreeNode(Node.GetNextMultiSelected);
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

procedure TfMainForm.pmdirectoriesPopup(Sender: TObject);
var
  Node: TFileTreeNode;
begin
  Node := TFileTreeNode(FilesTree.Selected);
  if (Node <> nil) then
    begin
      if not Node.isDir then
         mnuFileInfo.Visible := true
      else
         mnuFileInfo.Visible := False;
    end;

end;

procedure TfMainForm.pnCollectionPopup(Sender: TObject);
var
  Node: TMusicTreeNode;
begin
  Node := TMusicTreeNode(tvCollection.GetFirstMultiSelected);
  if node = nil then
     exit;
  case Node.Kind of
       tkSong : mnuInfo.Visible := true;
  else
       mnuInfo.Visible := False;
   end;

end;

procedure TfMainForm.OnMenuItemClick(Sender: TObject);
begin
  with TMenuItem(Sender), sgPlayList.Columns[TMenuItem(Sender).Tag] do
  begin
    Visible:= not Checked;
    ColumnSize(TMenuItem(Sender).Tag);
  end;

  sgPlayList.Invalidate;
  AdaptSize;

  SaveConfig(self);
end;

// This function is adapted from Grids.pas
function TfMainForm.ColumnSize(aCol: Integer):Integer;
var
  i,W: Integer;
  Ts: TSize;
  TmpCanvas: TCanvas;
  C: TGridColumn;
  ASong:TSong;
  txt:string;
begin
  with sgPlayList do
  begin
      if (aCol<0) or (aCol>ColCount-1) then
        Exit;

      tmpCanvas := GetWorkingCanvas(Canvas);

      C := Columns[(aCol)];

      try
        W:=0;
        for i := TopRow to (TopRow + VisibleRowCount-2) do begin

          if C<>nil then begin
            if i<FixedRows then
              tmpCanvas.Font := C.Title.Font
            else
              tmpCanvas.Font := C.Font;
          end else begin
            if i<FixedRows then
              tmpCanvas.Font := TitleFont
            else
              tmpCanvas.Font := Font;
          end;

          ASong := BackEnd.PlayList.Songs[i];
          case aCol of
             0: Txt := IntToStr(i);
             1: Txt := ASong.Tags.Title;
             2: Txt := ASong.Tags.Album;
             3: Txt := ASong.Tags.Artist;
             4: Txt := TimeToStr(ASong.Tags.Duration / MSecsPerDay);
             5: Txt := ASong.Tags.TrackString;
             6: Txt := ASong.Tags.Genre;
             7: Txt := ASong.Tags.Year;
             8: Txt := ASong.Tags.AlbumArtist;
             9: Txt := ASong.FileName;
            10: Txt := 'WWWWW';
          end;
          Ts := TmpCanvas.TextExtent(txt);

          if Ts.Cx>W then
            W := Ts.Cx;
        end;
        if C.Title.Caption <> '' then
          begin
            Ts := TmpCanvas.TextExtent(C.Title.Caption);
            if Ts.Cx > W then
               W:= Ts.Cx;
          end;
      finally
        if tmpCanvas<>Canvas then
          FreeWorkingCanvas(tmpCanvas);
      end;

      if W=0 then
        W := DefaultColWidth
      else
        W := W + 8;

      Result := W;

  end;
end;


procedure TfMainForm.pnHeaderPlaylistPopup(Sender: TObject);
var
  col :integer;
  item : TMenuItem;
begin
  pnHeaderPlaylist.Items.Clear;
  for col := 1 to sgPlayList.Columns.Count - 1 do
    begin
      item := TMenuItem.Create(pnHeaderPlaylist);
      item.Caption:=sgPlayList.Columns[col].Title.Caption;
      item.Tag := Col;
      item.Checked := sgPlayList.Columns[col].Visible;
      item.OnClick := @OnMenuItemClick;
      pnHeaderPlaylist.Items.Add(item);
    end;

end;

procedure TfMainForm.sgPlayListColRowMoved(Sender: TObject; IsColumn: Boolean;
  sIndex, tIndex: Integer);
begin
  if IsColumn then exit;

  BackEnd.PlayList.PushPos;
  BackEnd.PlayList.Swap(sIndex -1, tIndex -1);
  sgPlayList.Cells[1, sIndex]:= IntToStr(sIndex);
  sgPlayList.Cells[1, tIndex]:= IntToStr(tIndex);
  BackEnd.PlayList.PopPos;


end;

procedure TfMainForm.sgPlayListContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Zone: TGridZone;
  AbsPos :TPoint;
begin
  Zone := sgPlayList.MouseToGridZone(MousePos.x, MousePos.Y);
  AbsPos:= sgPlayList.ClientToScreen(MousePos);
  case zone of
     gzFixedCols, gzFixedRows: pnHeaderPlaylist.PopUp(AbsPos.x, AbsPos.Y);
     gzNormal : PlaylistMenu.PopUp(AbsPos.x, AbsPos.Y);
  end;

end;

procedure TfMainForm.sgPlayListDblClick(Sender: TObject);
var
  index: integer;
begin
  index := sgPlayList.Row;
  BackEnd.PlayList.ItemIndex := index - 1;
  BackEnd.AudioEngine.Play(BackEnd.PlayList.CurrentItem);
  sgPlayList.Invalidate;

end;

procedure TfMainForm.sgPlayListDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  aBmp, ABmp2: TBitmap;
  ASong : TSong;
  Txt: String;
  r1,R2: Trect;
  ts: TTextStyle;
begin
  if (ACol = 0) and (Arow > 0) and (ARow = BackEnd.PlayList.ItemIndex + 1) then
    begin
      aBmp := TBitmap.Create;
      case BackEnd.AudioEngine.State of
         ENGINE_PAUSE: BackEnd.ilSmall.GetBitmap(10, aBmp);
         ENGINE_STOP : BackEnd.ilSmall.GetBitmap(11, aBmp);
         ENGINE_PLAY : BackEnd.ilSmall.GetBitmap(6, aBmp);
      else
          BackEnd.ilSmall.GetBitmap(12, aBmp)
      end;


      if aBmp = nil then exit;
      sgPlayList.Canvas.Draw(arect.Left, aRect.Top, aBmp);
      abmp.free;
      exit;
    end;

   if aRow = 0 then
      exit;

    ASong := BackEnd.PlayList.Songs[Arow -1];
    if ASong = nil then exit;
    case aCol of
         1: Txt := IntToStr(Arow);
         2: Txt := ASong.Title;
         3: Txt := ASong.Tags.Album;
         4: Txt := ASong.Tags.Artist;
         5: Txt := TimeToStr(ASong.Tags.Duration / MSecsPerDay);
         6: Txt := ASong.Tags.TrackString;
         7: Txt := ASong.Tags.Genre;
         8: Txt := ASong.Tags.Year;
         9: Txt := ASong.Tags.AlbumArtist;
        10: Txt := ASong.FileName;
        11: Txt := '';
        else txt := '';
    end;

   sgPlayList.Canvas.FillRect(aRect);
   ts:= sgPlayList.Canvas.textStyle;
 //  if Acol > sgPlayList.FixedCols then
 //      ts.Alignment:=sgPlayList.Columns[Acol -sgPlayList.FixedCols].Alignment;
   ts.Clipping:= true;

//   sgPlayList.Canvas.TextOut(aRect.left, arect.top, txt);;
   if ts.Alignment = taRightJustify then
      aRect.Right:= aRect.Right -3;
   if ts.Alignment = taLeftJustify then
      aRect.Left:= aRect.Left +3;

   if (aCol <> 11) or not assigned(ASong.ExtraProperty)  then
      sgPlayList.Canvas.TextRect(aRect, aRect.left, arect.top, txt, ts)
   else
      begin
        sgPlayList.Canvas.Draw(arect.left, arect.top, RatingBack);
        r1:=Rect(0,
                 0,
                 trunc(RatingBack.width *(TExtendedInfo(ASong.ExtraProperty).Rating /10))-1,
                 RatingBack.height);
        r2:=r1;
        OffsetRect(R2,aRect.Left, aRect.top);
        sgPlayList.Canvas.CopyRect(r2,RatingFront.Canvas,r1);
      end;

end;

procedure TfMainForm.sgPlayListHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
var
  SortField: TplSortField;
  Direction: TplSortDirection;
  i: integer;
begin

  if not IsColumn then
    exit;

  if Index < 2 then exit;

  case Index  of
    2 : SortField := stTitle;
    3 : SortField := StAlbum;
    4 : SortField := stArtist;
    5 : SortField := stDuration;
    6 : SortField := stTrack;
    7 : SortField := stGenre ;
    8 : SortField := stYear;
    9 : SortField := stAlbumArtist;
   10 : SortField := stFileName;
  else
    SortField:= stNone;
  end;

  if SortField <> BackEnd.PlayList.SortField then
    Direction := sdplAscending
  else
    case  BackEnd.PlayList.SortDirection of
      sdplAscending:
        Direction := sdplDiscending;
      sdplDiscending:
        Direction := sdplAscending;
    end;

  for i := 0 to sgPlayList.Columns.Count -1 do
    if i = Index then
       begin
         if Direction = sdplDiscending then
           sgPlayList.Columns[i].Title.ImageIndex:= 8
         else
           sgPlayList.Columns[i].Title.ImageIndex:= 9;
       end
    else
       sgPlayList.Columns[i].Title.ImageIndex:= -1;


  BackEnd.PlayList.Sort(SortField, Direction);
  PlayListChange(Self);

end;

procedure TfMainForm.sgPlayListHeaderSized(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  SetLength(fColumnsWidth, 0);
  sgPlayList.Invalidate;
end;

procedure TfMainForm.sgPlayListHeaderSizing(sender: TObject;
  const IsColumn: boolean; const aIndex, aSize: Integer);
var
  i            : integer;
  diffs        : integer;
  VisibleCount : integer;
  Steps        : integer;
  Remains      : integer;
  LastVisible  : Integer;
  h: ThackGrid;

begin
  if not IsColumn then
    exit;

  h:= THackGrid(sgPlayList);

  if Length(fColumnsWidth) = 0 then
     begin
       SetLength(fColumnsWidth, sgPlayList.Columns.Count);
       for i := 0 to sgPlayList.Columns.Count -1 do
          fColumnsWidth[i] := sgPlayList.Columns[i].Width;
     end;


  Diffs := (fColumnsWidth[aIndex - sgPlayList.FixedCols] - aSize);
  IF Diffs = 0 then
    exit;

  VisibleCount:=0;
  LastVisible := aIndex - sgPlayList.FixedCols;
  for i := aindex  to sgPlayList.Columns.Count -1 do
    if sgPlayList.Columns[i].visible then
       Begin
         inc(VisibleCount);
       end;

  for i := 0  to sgPlayList.Columns.Count -1 do
    if sgPlayList.Columns[i].visible then
       Begin
         LastVisible := I;
       end;

  dec(VisibleCount);

  IF VisibleCount = -1 then
     begin
       sgPlayList.BeginUpdate;
    //   sgPlayList.Columns[LastVisible].Width := fColumnsWidth[LastVisible];
    sgPlayList.Columns[LastVisible].Width :=
        sgPlayList.Columns[LastVisible].Width + (h.GCache.ClientWidth- h.GCache.GridWidth);
       sgPlayList.EndUpdate(true);
       exit;
     end;

  sgPlayList.Columns[aIndex - sgPlayList.FixedCols].Width := ASize;

  if VisibleCount = 0 then
     begin
       sgPlayList.Columns[LastVisible].Width := fColumnsWidth[LastVisible] + Diffs;
       exit
     end;

  sgPlayList.BeginUpdate;

  Steps := diffs div VisibleCount;
  remains := diffs mod VisibleCount;

  for i := aindex +1  to sgPlayList.Columns.Count -1 do
     if sgPlayList.Columns[i].Visible then
       begin
         sgPlayList.Columns[i].Width := fColumnsWidth[i] + Steps;
       end;

  for i := (aindex + 1) to (aindex + Remains) do
     if sgPlayList.Columns[i].Visible then
       begin
         if diffs > 0 then
            sgPlayList.Columns[i].Width := sgPlayList.Columns[i].Width + 1
         else
            sgPlayList.Columns[i].Width := sgPlayList.Columns[i].Width - 1
       end;

  if h.GCache.GridWidth < h.GCache.ClientWidth then
     begin
       sgPlayList.Columns[LastVisible].Width :=
           sgPlayList.Columns[LastVisible].Width + (h.GCache.ClientWidth- h.GCache.GridWidth);
     end;

  sgPlayList.EndUpdate(true);

end;

procedure TfMainForm.MoveSelection(KeyDirection:TMovingSelection; Shift:TShiftState; Row:Integer);
var
  Displacement : Integer;

begin
  Displacement:=0;
  if not (ssShift in Shift) then
     MovingSelection := msNone;

  case KeyDirection of
     msUp: Displacement:=1;
     msDown:Displacement:=-1;
  end;

  if (MovingSelection = msNone) and PlaylistSelected.isMultiselection and (Displacement <> 0) then
     begin
        PlaylistSelected.ClearAll;
        PlaylistSelected[row+Displacement] := true;
     end;

  if (ssShift in Shift) then
     begin
      if MovingSelection = msNone then
         MovingSelection:= KeyDirection;

      if MovingSelection = KeyDirection then
         PlaylistSelected[Row - Displacement] := true
      else
        begin
           PlaylistSelected[Row] := False;
           if (Row - Displacement) = FAnchor then
            MovingSelection := msNone
        end;
     end
  else
    if not (ssCtrl in Shift) then
       begin
         PlaylistSelected.ClearAll;
         PlaylistSelected[Row - Displacement] := true;
         MovingSelection:= msNone;
         FAnchor:= Row -Displacement;
       end;


end;

procedure TfMainForm.sgPlayListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  CurrRow: integer;
begin
  CurrRow := sgPlayList.Row -1;

  case key of
    VK_DELETE:
       RemoveSelectionFromPlaylist;
    VK_SPACE:
       if (ssCtrl in Shift) then
          PlaylistSelected[CurrRow] := not PlaylistSelected[CurrRow];
    VK_UP :
       MoveSelection(msUp, Shift, CurrRow);
    VK_DOWN :
       MoveSelection(msDown, Shift, CurrRow);
    end;

  sgPlaylist.invalidate;
end;

procedure TfMainForm.sgPlayListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow : Integer;
  idx :integer;
begin
  sgPlayList.MouseToCell(X, Y, ACol, ARow);

  if (button = mbLeft) and (not (ssShift in Shift) and not (ssCtrl in Shift)) then
    begin
      MovingSelection:= msNone;
      fSourceIndex := ARow;
      FAnchor:=ARow - 1;
      PlaylistSelected.ClearAll;
    end;

  if (Button = mbRight)  then
     begin
       if not (PlaylistSelected.isMultiselection) then
          begin
             PlaylistSelected.ClearAll;
             PlaylistSelected[ARow - 1] := true;
          end;
     end
  else
     if (ARow > 0) then
        PlaylistSelected[ARow - 1] := not PlaylistSelected[ARow - 1 ];

  if (FAnchor <> -1) and  (ssShift in Shift) then
     begin
       PlaylistSelected.ClearAll;
       PlaylistSelected.SelectRange(FAnchor, fAnchor, ARow -1);
     end;

  sgPlayList.Invalidate;


end;

procedure TfMainForm.sgPlayListMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  ACol, ARow : Integer;
  R1: TRect;
  Rating: Integer;
begin

  sgPlayList.MouseToCell(x, y, ACol, ARow);
  if (ARow > 0) and (aCol = 11) and assigned(BackEnd.PlayList[ARow-1].ExtraProperty) then
     begin
       R1 := sgPlayList.CellRect(ACol, ARow);
       Rating := trunc(((x - R1.Left) * 10) / RateStars.Width) ;

       TExtendedInfo(BackEnd.PlayList[ARow-1].ExtraProperty).Rating:=rating;
       sgPlayList.InvalidateCell(ACol, ARow);
     end;

  if fSourceIndex < 1 then
    exit;

  if not (ssLeft in Shift) and (not (ssShift in Shift) and not (ssCtrl in Shift)) then
    exit;

  if (Arow < 1) or
     (Arow  = fSourceIndex) then
    exit;
  fTargetIndex:= ARow;

  sgPlayList.MoveColRow(false, fTargetIndex, fSourceIndex);

 // sgPlayList.Selection := rect(fTargetIndex, 0, fTargetIndex, 1);

  sgPlayList.Repaint;
  fSourceIndex := fTargetIndex;

end;

procedure TfMainForm.sgPlayListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//var
//   mycol, myrow : Integer;

begin
  fSourceIndex := -1;
  fTargetIndex := -1;

end;

procedure TfMainForm.sgPlayListPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if aRow = 0 then exit;
  if ARow = BackEnd.PlayList.ItemIndex + 1 then
    begin
      sgPlaylist.Canvas.Font.Style := [fsUnderline];
    end;


  sgPlayList.Canvas.Font.Color := clWindowText;  // colore di default
  sgPlayList.Canvas.Brush.Color := clWindow;

   if PlaylistSelected[ARow - 1] then
     begin        // Will only color selected rows
       sgPlayList.Canvas.Font.Color :=  clHighlightText;
       sgPlayList.Canvas.Brush.Color := clHighlight;
     end;

end;

procedure TfMainForm.sgPlayListResize(Sender: TObject);
begin
  AdaptSize(False);
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

procedure TfMainForm.TrackDblClick(Sender: TObject);
var
  info : TfSongInfo;
begin
  if not assigned(BackEnd.PlayList.CurrentItem) then
     exit;

  info := TfSongInfo.Create(Application);
  info.InitFromFile(BackEnd.PlayList.CurrentItem.FullName);
  info.Show;
end;

procedure TfMainForm.TrayIconClick(Sender: TObject);
begin
  if Visible then
    Hide
  else
  begin
    Show;
    BringToFront;
  end;
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
  Limit : Integer;
  strLimit: string;
begin
  item := PlaylistTree.Selected;
  if item = nil then
     exit;
  Limit := BackEnd.Config.PlayListParam.LimitTrack;
  if limit > 0 then
     strLimit:= ' limit ' + IntToStr(Limit)
  else
     strLimit:= '';

  case item.StateIndex of
     1: BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList,
        'playcount = 0', 'random()'  + strLimit);
     2: BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList,
        '', 'added' + strLimit);
     3: BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList,
        '', 'playcount desc ' + strLimit);
     4: BackEnd.Manager.ImportFromMediaLibrary(BackEnd.mediaLibrary, BackEnd.PlayList,
        '','random()' + strLimit);
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

procedure TfMainForm.tvCollectionCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass:= TMusicTreeNode;
end;

procedure TfMainForm.tvCollectionDblClick(Sender: TObject);
begin
  CollectionHandler(true);
end;

procedure TfMainForm.tvCollectionGetImageIndex(Sender: TObject; Node: TTreeNode
  );
begin
  node.ImageIndex := SortArray[TMusicTreeNode(Node).Kind].ImageIndex;
  node.SelectedIndex:= node.ImageIndex;

end;

procedure TfMainForm.LoadDir(Path:string);
var
  DirList: TstringList;
  FileList :TstringList;
  i: integer;
  Node: TFileTreeNode;
begin
  DirList:=TStringList.Create;
  FileList:=TStringList.Create;
  CurrentPath:=IncludeTrailingPathDelimiter(Path);
  try
    FilesTree.Items.Clear;
    BuildFolderList(CurrentPath, DirList, False);
    DirList.Sort;
    BuildFileList(IncludeTrailingPathDelimiter(CurrentPath) + AudioTag.SupportedExtension,
                   faAnyFile, FileList, False);
    FileList.Sort;
    for i := 0 to DirList.Count -1 do
      begin
        node:=TFileTreeNode(FilesTree.items.AddChild(Nil, ExtractFileName(DirList[i])));
        node.FullPath:=DirList[i];
        node.isDir:=True;;
      end;

    for i := 0 to FileList.Count -1 do
      begin
        node:=TFileTreeNode(FilesTree.items.AddChild(nil, ExtractFileName(FileList[i])));
        node.FullPath:=FileList[i];
        node.isDir:=False;
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
//var
  //I:     integer;
//  ASong: TSong;
begin


  sgPlayList.Clear;
  sgPlayList.RowCount:=BackEnd.PlayList.Count+1 ;
  PlaylistSelected.SetSize(sgPlayList.RowCount -1);
  PlaylistSelected.Clearall;


  //for i := 1 to BackEnd.PlayList.Count  do
  //  begin
  //    ASong := BackEnd.PlayList.Songs[i-1];
  //    sgPlayList.Cells[ 1,i]:= IntToStr(i);
  //    sgPlayList.Cells[ 2,i]:= ASong.Tags.Title;
  //    sgPlayList.Cells[ 3,i]:= ASong.Tags.Album;
  //    sgPlayList.Cells[ 4,i]:= TimeToStr(ASong.Tags.Duration / MSecsPerDay);
  //    sgPlayList.Cells[ 5,i]:= ASong.Tags.TrackString;
  //    sgPlayList.Cells[ 6,i]:= ASong.Tags.Genre;
  //    sgPlayList.Cells[ 7,i]:= ASong.Tags.Year;
  //    sgPlayList.Cells[ 8,i]:= ASong.Tags.AlbumArtist;
  //    sgPlayList.Cells[ 9,i]:= ASong.FileName;
  //    sgPlayList.Cells[10,i]:= '5';
  //    end;
  AdaptSize;
  sgPlayList.Repaint;


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
     ecPause, ecPlay: sgPlayList.InvalidateRow(BackEnd.PlayList.ItemIndex+1);
  end;
end;

procedure TfMainForm.OnExternalCommand(Sender: Tobject; Command: String);
begin
  if Command = 'activate' then
     Show;
end;

procedure TfMainForm.AdaptSize(Recalculate: boolean =true);
var
  i:integer;
  ColWidths : array of integer;
  TotalSize : Integer;
  diffs     : integer;
  VisibleCount : integer;
  Steps        : integer;
  Remains      : integer;
  h: ThackGrid;
begin
  TotalSize:= 0;
  VisibleCount:=0;
  SetLength(ColWidths, sgPlayList.Columns.Count);
  if Recalculate then
     begin
      for I := 0 to sgPlayList.Columns.Count - 1 do
         if sgPlayList.Columns[i].Visible then
           begin
             ColWidths[i] := ColumnSize(i);
             inc(VisibleCount);
             inc(TotalSize, ColWidths[i])
           end
         else
           ColWidths[i] := 0;
     end
  else
    begin
      for I := 0 to sgPlayList.Columns.Count - 1 do
         if sgPlayList.Columns[i].Visible then
           begin
             ColWidths[i] :=  sgPlayList.Columns[i].Width;
             inc(VisibleCount);
             inc(TotalSize, ColWidths[i])
           end
         else
           ColWidths[i] := 0;
    end;

  h:= THackGrid(sgPlayList);

  Diffs := h.gcache.ClientWidth - h.gcache.FixedWidth  - TotalSize -1;

  Steps := diffs div VisibleCount;
  remains := diffs mod VisibleCount;

  for I := 0 to sgPlayList.Columns.Count - 1 do
     if sgPlayList.Columns[i].Visible then
       begin
         ColWidths[i] := ColWidths[i] + Steps;
       end;

  for I := 0 to Remains - 2 do
     if sgPlayList.Columns[i].Visible then
       begin
         if diffs > 0 then
            ColWidths[i] := ColWidths[i] + 1
         else
            ColWidths[i] := ColWidths[i] - 1
       end;

  for I := 0 to sgPlayList.Columns.Count - 1 do
     if sgPlayList.Columns[i].Visible then
       begin
         sgPlayList.Columns[i].Width := ColWidths[i];
       end;
end;

end.