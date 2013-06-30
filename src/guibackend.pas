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
unit GUIBackEnd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ActnList, Controls, Dialogs, Forms, LResources,
  BaseTypes, CoreInterfaces,
  PopupNotifier, PlayList, AudioEngine, AudioEngine_dummy,
  PlayListManager, MediaLibrary, basetag, Song,
  MultimediaKeys, Config, UniqueInstance;

type

  TOnEngineCommand = procedure(Sender: Tobject; Command : TEngineCommand) of object;
  TOnExternalCommand = procedure(Sender: Tobject; Command : String) of object;

  { TBackEnd }

  TBackEnd = class(TDataModule, IBackEnd)
    actExit: TAction;
    actDummy: TAction;
    actRestart: TAction;
    actRepeatNone: TAction;
    actRepeatAll: TAction;
    actRepeatAlbum: TAction;
    actMute: TAction;
    actRemoveMissing: TAction;
    actRepeatTrack: TAction;
    actSkipBackward: TAction;
    actSkipForward: TAction;
    actPlayListClear:    TAction;
    actPlayListAddFiles: TAction;
    actImportDirectory: TAction;
    actPlayListShuffle: TAction;
    actRescanCollection: TAction;
    actPause:    TAction;
    actPrevious: TAction;
    actNext:     TAction;
    actStop:     TAction;
    actPlay:     TAction;
    actPlaylistSave: TAction;
    actPlaylistLoad: TAction;
    ActionList:  TActionList;
    ApplicationProperties: TApplicationProperties;
    ilButtons:   TImageList;
    ilSmall:     TImageList;
    ilNone: TImageList;
    OpenDialogPlaylist: TOpenDialog;
    OpenDialogFiles: TOpenDialog;
    SaveDialogPlayList: TSaveDialog;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    UniqueInstanceI: TUniqueInstance;
    procedure actExitExecute(Sender: TObject);
    procedure actMuteExecute(Sender: TObject);
    procedure actPlayListAddFilesExecute(Sender: TObject);
    procedure actPlayListClearExecute(Sender: TObject);
    procedure actImportDirectoryExecute(Sender: TObject);
    procedure actPlayListShuffleExecute(Sender: TObject);
    procedure actPlaylistLoadExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actPauseExecute(Sender: TObject);
    procedure actPlayExecute(Sender: TObject);
    procedure actRepeatTrackExecute(Sender: TObject);
    procedure actPreviousExecute(Sender: TObject);
    procedure actRemoveMissingExecute(Sender: TObject);
    procedure actRepeatAlbumExecute(Sender: TObject);
    procedure actRepeatAllExecute(Sender: TObject);
    procedure actRepeatNoneExecute(Sender: TObject);
    procedure actRescanCollectionExecute(Sender: TObject);
    procedure actPlaylistSaveExecute(Sender: TObject);
    procedure actRestartExecute(Sender: TObject);
    procedure actSkipBackwardExecute(Sender: TObject);
    procedure actSkipForwardExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure ApplicationPropertiesDropFiles(Sender: TObject;
      const FileNames: array of String);
    procedure ApplicationPropertiesException(Sender: TObject; E: Exception);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure UniqueInstanceIOtherInstance(Sender: TObject; ParamCount: Integer;
      Parameters: array of String);
  private
    { private declarations }
    FOnEngineCommand: TOnEngineCommand;
    FOnExternalCommand: TOnExternalCommand;
    FOnPlayListChange: TNotifyEvent;
    FOnPlayListLoad:   TNotifyEvent;
    fMultimediaKeys:   TMultimediaKeys;
    FOnSaveInterfaceState: TNotifyEvent;
    ObserverList : TInterfaceList;
    procedure AudioEngineSongEnd(Sender: TObject);
    procedure onMultimediaKeys(Sender: TObject; Command: TEngineCommand);
    procedure PlaylistOnSongAdd(Sender: Tobject; Index: Integer; ASong: TSong);
    procedure SetOnEngineCommand(const AValue: TOnEngineCommand);
    procedure SetOnExternalCommand(const AValue: TOnExternalCommand);
    procedure SetOnPlayListChange(const AValue: TNotifyEvent);
    procedure SetOnPlayListLoad(const AValue: TNotifyEvent);
    procedure SetOnSaveInterfaceState(AValue: TNotifyEvent);

  public
    PlayList: TPlaylist;
    Manager: TPlayListManager;
    AudioEngine: TAudioEngine;
    mediaLibrary: TMediaLibrary;
    Config: TConfig;
  // IBackEnd methods
    function GetLooping: TplRepeat;
    function GetPosition: int64;
    function GetStatus: TEngineState;
    function GetVolume: cardinal;
    procedure SetLooping(AValue: TplRepeat);
    procedure SetPosition(AValue: int64);
    procedure SetStatus(AValue: TEngineState);
    procedure SetVolume(AValue: cardinal);


    Procedure Play;
    Procedure Stop;
    Procedure Pause;
    Procedure UnPause;
    Procedure Next;
    Procedure Previous;
    Procedure Quit;
    Procedure OpenURI(URI: String);
    Function GetMetadata: TCommonTags;
    Procedure Attach(observer: iObserver);
    Procedure Remove(observer: iObserver);
    procedure Seek(AValue: int64);
    Procedure Notify(Kind:  TChangedProperty);
 //
    function GetImageFromfolder(Path: string): string;
    procedure HandleCommand(Command: TEngineCommand; Param: integer);
    procedure SaveState;
    procedure SignalPlayListChange;
    property OnSaveInterfaceState: TNotifyEvent read FOnSaveInterfaceState write SetOnSaveInterfaceState;
    property OnPlayListChange: TNotifyEvent read FOnPlayListChange write SetOnPlayListChange;
    property OnPlayListLoad: TNotifyEvent read FOnPlayListLoad write SetOnPlayListLoad;
    property OnEngineCommand :TOnEngineCommand read FOnEngineCommand write SetOnEngineCommand;
    property OnExternalCommand :TOnExternalCommand read FOnExternalCommand write SetOnExternalCommand;

  end;

var
  fBackEnd: TBackEnd;


function BackEnd: TBackEnd;

implementation

{$R *.lfm}
uses LCLProc, strutils,  FilesSupport, AudioTag, AppConsts, GeneralFunc;

{ TBackEnd }

function BackEnd: TBackEnd;
begin
  if fBackEnd = nil then
    fBackEnd := TBackEnd.Create(Application);

  Result := fBackEnd;
end;

procedure TBackEnd.DataModuleCreate(Sender: TObject);
var
  Engine : TAudioEngineClass;
begin
  Application.OnException := @ApplicationPropertiesException;
  Config := TConfig.Create;

  Manager  := TPlayListManager.Create;
  Playlist := TPlayList.Create;
  PlayList.RepeatMode :=  TplRepeat(Config.PlayListParam.RepeatMode);
  mediaLibrary := TMediaLibrary.Create;
  engine := nil;
  if Config.EngineParam.EngineKind <> '' then
     Engine := GetEngineByName(Config.EngineParam.EngineKind);

  if Engine = nil then
    begin
      Engine := GetBestEngine;
      if Engine = nil then
         engine:=TAudioEngineDummy
      else
        Config.EngineParam.EngineKind := Engine.getEngineNAme;;
    end;

  if not Engine.IsAvalaible(Config.EngineSubParams) then
       engine:=TAudioEngineDummy;

  AudioEngine := Engine.Create;
  AudioEngine.OnSongEnd := @AudioEngineSongEnd;
  AudioEngine.Activate;
  PlayList.OnSongAdd:=@PlaylistOnSongAdd;

  if Config.InterfaceParam.CaptureMMKeys then
    begin
      fMultimediaKeys := TMultimediaKeys.Create(Config.InterfaceParam.CaptureMMkeysMode);
      fMultimediaKeys.OnMMKey := @OnMultimediaKeys;
    end;


  UniqueInstanceI:= TUniqueInstance.Create(Self);
  with UniqueInstanceI do
    begin
      Identifier := AppNameServerID;
      UpdateInterval := 500;
      OnOtherInstance := @UniqueInstanceIOtherInstance;
      Enabled := True;
      Loaded;
    end;

end;

procedure TBackEnd.SaveState;
begin
  Manager.SaveToXSPF(Config.ConfigDir + 'lastplaylist.xspf', PlayList, AudioEngine.Position);
  BackEnd.Config.EngineParam.Volume := AudioEngine.MainVolume;

end;

procedure TBackEnd.DataModuleDestroy(Sender: TObject);
begin
  try
    SaveState;
    if Assigned(FOnSaveInterfaceState) then
       FOnSaveInterfaceState(Sender);
  except
  end;
  fMultimediaKeys.Free;
  Manager.Free;
  PlayList.Free;
  AudioEngine.Free;
  mediaLibrary.Free;
  Config.Free;

end;

procedure TBackEnd.UniqueInstanceIOtherInstance(Sender: TObject;
  ParamCount: Integer; Parameters: array of String);
var
  i:integer;
  tempstr: string;
  idx: integer;
  tempparam: string;
begin
  //
  if ParamCount > 0 then
    for i:= 0 to ParamCount - 1 do
      begin
         if AnsiStartsStr('action:', Parameters[i]) then
           begin
             tempstr:=copy(Parameters[i], 8, Length(Parameters[i]));
             if tempstr = 'seek+'    then
                actSkipForward.Execute   else
             if tempstr = 'seek-'    then
                actSkipBackward.Execute  else
             if tempstr = 'play'     then
                Play                     else
             if tempstr = 'stop'     then
                Stop                     else
             if tempstr = 'pause'    then
                Pause                    else
             if tempstr = 'next'     then
                Next                     else
             if tempstr = 'previous' then
                Previous                 else
             if tempstr = 'close' then
                Quit;

             if Assigned(FOnExternalCommand) then
                FOnExternalCommand(Self, tempstr);

           end;

         if AnsiStartsStr('file:', Parameters[i]) then
           begin
             tempstr:=copy(Parameters[i], 6, 2);
             tempparam:=copy(Parameters[i], 8, Length(Parameters[i]));
             if tempstr = 'e=' then
                begin
                  idx := PlayList.EnqueueFile(tempparam);
                  SignalPlayListChange;
                end else
             if tempstr = 'p=' then
                begin
                  OpenUri(tempparam)
                end  else
             if tempstr = 'x=' then
                begin
                  idx := PlayList.EnqueueFile(tempparam);
                  PlayList.ItemIndex:=idx;
                  AudioEngine.Play(PlayList.CurrentItem);
                  SignalPlayListChange;
                end;

           end;
      end;

end;


function TBackEnd.GetImageFromfolder(Path: string): string;
const
  CountName = 6;
  CoverName : array [0..CountName - 1] of string =
     ('cover', 'front','folder', 'cd', 'cov', 'art');

  CountExt = 4;
  CoverExt : array [0..CountExt - 1] of string =
     ('png', 'jpg', 'jpeg', 'gif');

var
  FileList: TStringList;
  N, E :Integer;
  AllExt:string;
begin
  Result := '';
  E:=0;
  while (E <= CountExt - 1) and (result = EmptyStr) do
   begin
     n:=0;
     while (N <= CountName - 1) and (result = EmptyStr) do
       begin
         if FileExists(Path + CoverName[N] + '.' + CoverExt[E]) then
            Result:=path + CoverName[N] + '.' + CoverExt[E];
         inc(N);
       end;
     inc(E);
    end;

  if Result = '' then
     begin
       FileList := TStringList.Create;
       AllExt:='';
       for E := 0 to CountExt -1 do
         AllExt:= AllExt + '*.' + CoverExt[E]+';';

       try
       BuildFileList(IncludeTrailingPathDelimiter(Path) + AllExt,
         faAnyFile, FileList, false);
       FileList.Sort;
       if FileList.Count > 0 then
         Result := FileList[0];
       finally
         FileList.Free;
       end;
     end;

  if Result = '' then
    Result := Config.GetResourcesPath + 'nocover.png';
end;


procedure TBackEnd.onMultimediaKeys(Sender: TObject; Command: TEngineCommand);
begin
  HandleCommand(Command, 0);
end;

procedure TBackEnd.SetOnEngineCommand(const AValue: TOnEngineCommand);
begin
  if FOnEngineCommand=AValue then exit;
  FOnEngineCommand:=AValue;
end;

procedure TBackEnd.SetOnExternalCommand(const AValue: TOnExternalCommand);
begin
  if FOnExternalCommand=AValue then exit;
  FOnExternalCommand:=AValue;
end;

procedure TBackEnd.SetOnPlayListChange(const AValue: TNotifyEvent);
begin
  if FOnPlayListChange = AValue then
    exit;
  FOnPlayListChange := AValue;
end;

procedure TBackEnd.SetOnPlayListLoad(const AValue: TNotifyEvent);
begin
  if FOnPlayListLoad = AValue then
    exit;
  FOnPlayListLoad := AValue;
end;

procedure TBackEnd.SetOnSaveInterfaceState(AValue: TNotifyEvent);
begin
  if FOnSaveInterfaceState=AValue then Exit;
  FOnSaveInterfaceState:=AValue;
end;

function TBackEnd.GetLooping: TplRepeat;
begin
  if Assigned(PlayList) then
     Result := PlayList.RepeatMode;
end;

function TBackEnd.GetPosition: int64;
begin
  Result := AudioEngine.Position;
end;

function TBackEnd.GetStatus: TEngineState;
begin
  if Assigned(AudioEngine) then
     Result := AudioEngine.State;
end;

function TBackEnd.GetVolume: cardinal;
begin
  Result:= AudioEngine.MainVolume;
end;

procedure TBackEnd.SetLooping(AValue: TplRepeat);
begin
 PlayList.RepeatMode:= AValue;
 Notify(cpLooping);
end;

procedure TBackEnd.SetPosition(AValue: int64);
begin
  AudioEngine.Position := AValue;
  Notify(cpPosition);
end;

procedure TBackEnd.SetStatus(AValue: TEngineState);
begin
  // ??
end;

procedure TBackEnd.SetVolume(AValue: cardinal);
begin
  AudioEngine.MainVolume := AValue;
  Notify(cpVolume);
end;

procedure TBackEnd.Play;
begin
  if AudioEngine.State = ENGINE_PAUSE then
    AudioEngine.UnPause
  else
    begin
      if PlayList.Count = 0 then
         exit;

      if PlayList.ItemIndex = -1 then
        PlayList.ItemIndex := 0;

      PlayList.CurrentItem.Tags;
      AudioEngine.Play(PlayList.CurrentItem);
    end;

 if Assigned(FOnEngineCommand) then
     FOnEngineCommand(AudioEngine, ecPlay);
 Notify(cpStatus);
end;

procedure TBackEnd.Stop;
begin
  AudioEngine.Stop;
  if Assigned(FOnEngineCommand) then
     FOnEngineCommand(AudioEngine, ecStop);
  Notify(cpStatus);
end;

procedure TBackEnd.Pause;
begin
  AudioEngine.Pause;
  if Assigned(FOnEngineCommand) then
      FOnEngineCommand(AudioEngine, ecPause);
  Notify(cpStatus);
end;

procedure TBackEnd.UnPause;
begin
  Play;
  Notify(cpStatus);
end;

procedure TBackEnd.Next;
begin
  AudioEngine.Play(PlayList.Next);
  SignalPlayListChange;

  if Assigned(FOnEngineCommand) then
     FOnEngineCommand(AudioEngine, ecNext);

  Notify(cpStatus);
end;

procedure TBackEnd.Previous;
begin
  AudioEngine.Play(PlayList.Previous);
  SignalPlayListChange;

  if Assigned(FOnEngineCommand) then
    FOnEngineCommand(AudioEngine, ecPrevious);
  Notify(cpStatus);
end;

procedure TBackEnd.Quit;
begin
  SaveState;
  if Assigned(FOnSaveInterfaceState) then
     FOnSaveInterfaceState(Self);

  Application.Terminate;

end;

procedure TBackEnd.OpenURI(URI: String);
var
  idx:Integer;
begin
  PlayList.Clear;
  SignalPlayListChange;
  idx := PlayList.EnqueueFile(URI);
  PlayList.ItemIndex:=idx;
  AudioEngine.Play(PlayList.CurrentItem);

end;

function TBackEnd.GetMetadata: TCommonTags;
begin
  If Assigned(PlayList.CurrentItem) then
     Result:=PlayList.CurrentItem.Tags;
end;

procedure TBackEnd.Seek(AValue: int64);
begin
  AudioEngine.Seek(AValue,False);
  Notify(cpPosition);
end;

procedure TBackEnd.Attach(observer: iObserver);
begin
  if not Assigned(ObserverList) then
     ObserverList := TInterfaceList.create;
  ObserverList.Add(observer);

end;

procedure TBackEnd.Remove(observer: iObserver);
begin
  Remove(observer);
  if ObserverList.Count = 0 then
    FreeAndNil(ObserverList);

end;

procedure TBackEnd.Notify(Kind: TChangedProperty);
var
  i:integer;
begin
  if Assigned(ObserverList) then
     for i := 0 to ObserverList.Count -1 do
       IObserver(ObserverList[i]).Update(Kind);
end;

procedure TBackEnd.SignalPlayListChange;
begin
  if Assigned(OnPlayListChange) then
    OnPlayListChange(PlayList);

end;

procedure TBackEnd.actPlaylistLoadExecute(Sender: TObject);
begin
  if OpenDialogPlaylist.Execute then
    begin
    PlayList.clear;
    Manager.LoadPlayList(OpenDialogPlaylist.FileName, PlayList);
    PlayList.LoadAllTags;
    if Assigned(OnPlayListLoad) then
      OnPlayListLoad(self);

    if (Manager.SavedTime <> 0) and
        Config.PlayListParam.Restart and
       (PlayList.CurrentItem <> nil) then
      begin
         AudioEngine.Play(PlayList.CurrentItem, Manager.SavedTime);
      end;

    end;

end;

procedure TBackEnd.actPlayListClearExecute(Sender: TObject);
begin
  Stop;
  PlayList.Clear;
  SignalPlayListChange;
end;

procedure TBackEnd.actImportDirectoryExecute(Sender: TObject);
begin
 SelectDirectoryDialog.FileName := Config.GeneralParam.LastImportFolder;
 if SelectDirectoryDialog.Execute then
   begin
     Manager.ImportFromDirectory(SelectDirectoryDialog.FileName,true, PlayList);
     Config.GeneralParam.LastImportFolder := SelectDirectoryDialog.FileName;
   end;
 SignalPlayListChange;
end;

procedure TBackEnd.actPlayListShuffleExecute(Sender: TObject);
begin
  PlayList.Shuffle;
  SignalPlayListChange;
end;

procedure TBackEnd.actPlayListAddFilesExecute(Sender: TObject);
var
  i: integer;
begin
  OpenDialogFiles.Filter := format(rAllFiles, [supportedExtension]);

  if not OpenDialogFiles.Execute then
    exit;

  for i := 0 to OpenDialogFiles.Files.Count - 1 do
    PlayList.EnqueueFile(OpenDialogFiles.Files[i]);

  SignalPlayListChange;

end;

procedure TBackEnd.actExitExecute(Sender: TObject);
begin
  Quit;
end;

procedure TBackEnd.actMuteExecute(Sender: TObject);
begin
  if actMute.Checked then
     begin
       AudioEngine.Muted := False;
       actMute.ImageIndex := 19;
     end
  else
     begin
       AudioEngine.Muted := true;
       actMute.ImageIndex := 18;
     end;
  actMute.Checked := not actMute.Checked;
  Notify(cpVoLume);
end;

procedure TBackEnd.actNextExecute(Sender: TObject);
begin
  Next;
  Notify(cpStatus);
end;

procedure TBackEnd.actPauseExecute(Sender: TObject);
begin
  Pause;
  Notify(cpStatus);
end;

procedure TBackEnd.actPlayExecute(Sender: TObject);
begin
  Play;
  Notify(cpStatus);
end;

procedure TBackEnd.actPreviousExecute(Sender: TObject);
begin
 Previous;
 Notify(cpStatus);
end;

procedure TBackEnd.actRemoveMissingExecute(Sender: TObject);
begin
  mediaLibrary.RemoveMissing;
end;

procedure TBackEnd.actRepeatTrackExecute(Sender: TObject);
begin
  PlayList.RepeatMode := rptTrack;
  actRepeatTrack.Checked := true;
  Config.PlayListParam.RepeatMode := Ord(PlayList.RepeatMode);
  Notify(cpLooping);
end;

procedure TBackEnd.actRepeatAlbumExecute(Sender: TObject);
begin
  PlayList.RepeatMode := rptAlbum;
  actRepeatAlbum.Checked:=true;
  Config.PlayListParam.RepeatMode := Ord(PlayList.RepeatMode);
  Notify(cpLooping);
end;

procedure TBackEnd.actRepeatAllExecute(Sender: TObject);
begin
  PlayList.RepeatMode := rptPlayList;
   actRepeatAll.Checked:=true;
   Config.PlayListParam.RepeatMode := Ord(PlayList.RepeatMode);
   Notify(cpLooping);
end;

procedure TBackEnd.actRepeatNoneExecute(Sender: TObject);
begin
  PlayList.RepeatMode := rptNone;
  actRepeatNone.Checked:=true;
  Config.PlayListParam.RepeatMode := Ord(PlayList.RepeatMode);
  Notify(cpLooping);
end;

procedure TBackEnd.actRescanCollectionExecute(Sender: TObject);

begin
  MediaLibrary.Scan(Config.MediaLibraryParam.LibraryPaths);
end;

procedure TBackEnd.actPlaylistSaveExecute(Sender: TObject);
begin
  if not SaveDialogPlayList.Execute then
    exit;

  Manager.SaveToXSPF(SaveDialogPlayList.FileName, PlayList, AudioEngine.Position);

end;

procedure TBackEnd.actRestartExecute(Sender: TObject);
begin
  Restart(Application);
end;

procedure TBackEnd.actSkipBackwardExecute(Sender: TObject);
begin
  BackEnd.AudioEngine.Seek(-10, False);
  if Assigned(FOnEngineCommand) then
     FOnEngineCommand(AudioEngine, ecSeek);

end;

procedure TBackEnd.actSkipForwardExecute(Sender: TObject);
begin
  BackEnd.AudioEngine.Seek(+10, False);
  if Assigned(FOnEngineCommand) then
     FOnEngineCommand(AudioEngine, ecSeek);

end;

procedure TBackEnd.actStopExecute(Sender: TObject);
begin
  Stop;
end;

procedure TBackEnd.PlaylistOnSongAdd(Sender: Tobject; Index: Integer; ASong : TSong);
var
  ID: Integer;
  ExtendedInfo: TExtendedInfo;
begin
  ID := mediaLibrary.IDFromFullName(ASong.FullName);
  if ID = -1 then
    exit;
  ExtendedInfo := mediaLibrary.InfoFromID(ID);
  ASong.ExtraProperty := ExtendedInfo;
end;

procedure TBackEnd.ApplicationPropertiesDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  st:TstringList;
  i :Integer;
begin
  st:=TStringList.Create ;
  try
    for i:= Low(FileNames) to High(FileNames) do
      if pos ('.' + ExtractFileExt(FileNames[i]) + ';', SupportedExtension) > 0 then
         st.Add(FileNames[i]);

    Manager.ImportFromStrings(st, PlayList);
  finally
    st.free;
  end;
end;
{
procedure DumpExceptionCallStack(E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
  func,
  source : shortstring;
  hs     : string[32];
  line   : longint;
  Success : boolean;
  Msg: TstringList;
begin
  msg:= TstringList.create;
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc (ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc (Frames[I] );
  msg.Text:=Report;
  msg.SaveToFile('ovoplayer.err');
  msg.free;
  DumpStack;
end;

}
procedure DumpExceptionCallStack(e:exception);
var
  f: System.Text;
  afileName:String;
begin
  afileName:='ovoplayer.err';
  if (aFileName <> EmptyStr)  then
  begin
    AssignFile(f, UTF8ToSys(aFileName));
    {$PUSH}{$I-}
    if not FileExists(aFileName) then
      Rewrite(f)
    else
      Append(f);
    {$POP}
    if (TextRec(f).mode <> fmClosed) and (IOResult = 0) then
    begin
      WriteLn(f, '--------------- ',
                 FormatDateTime('dd-mm-yyyy, hh:nn:ss', SysUtils.Now),
                 ' ---------------');
      WriteLn(f, '| DC v', AppVersion, ' Rev. ', AppVersion);
//                 ' -- ', TargetCPU + '-' + TargetOS + '-' + TargetWS);
//      if WSVersion <> EmptyStr then
//        WriteLn(f, '| ', OSVersion, ' -- ', WSVersion)
//      else
//        WriteLn(f, '| ', OSVersion);

        if Assigned(e) then
          WriteLn(f, 'Unhandled exception: ',
                     Exception(e).ClassName, ': ',
                     Exception(e).Message)
        else
          WriteLn(f, 'Unhandled exception');
        WriteLn(f, '  Stack trace:');

        System.DumpExceptionBackTrace(f);


      // Make one empty line.
      WriteLn(f);

      CloseFile(f);
    end;
  end;
end;


procedure TBackEnd.ApplicationPropertiesException(Sender: TObject; E: Exception
  );
begin
  DumpExceptionCallStack(e);
  Halt; // End of program execution
end;

procedure TBackEnd.AudioEngineSongEnd(Sender: TObject);
begin
  Next;
end;

procedure TBackEnd.HandleCommand(Command: TEngineCommand; Param: integer);
begin
  case command of
    ecNext: Next;
    ecPrevious: Previous;
    ecPlay: if AudioEngine.State = ENGINE_PLAY then
        Pause
      else
        Play;
    ecStop: Stop;
    ecSeek: SetPosition(Param);
    end;
end;


initialization
  fBackEnd := nil;
end.