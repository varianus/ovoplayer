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
unit uDM;

interface

uses
  Classes, SysUtils, LazFileUtils, ActnList, Controls, Dialogs, Forms, LResources,
  singleinstance, strutils,
  BaseTypes, CoreInterfaces,
  AudioEngine, LazUTF8,  LazLogger,
  PlayListManager, MediaLibrary, FilesSupport, netprotocol;

type

  { TDM }

  TDM = class(TDataModule)
    actExit: TAction;
    actDummy: TAction;
    actFastScan: TAction;
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
    procedure actExitExecute(Sender: TObject);
    procedure actFastScanExecute(Sender: TObject);
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
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure SaveDialogPlaylistTypeChange(Sender: TObject);
    procedure ServerReceivedParams(Sender: TBaseSingleInstance; aParams: TStringList );
    // -- --
  private
    procedure DebugLnHook(Sender: TObject; S: string; var Handled: Boolean);
  public
    procedure ChangeRepeatMode(Mode: TplRepeat; Notify: boolean=true);
  end;

var
  DM: TDM;

implementation

{$R *.lfm}
uses  AudioTag, AppConsts, GeneralFunc, GUIBackEnd,
  netsupport;

{ TDM }
// for traceback porpouse
var
  f: text;

procedure TDM.ServerReceivedParams(Sender: TBaseSingleInstance;
  aParams: TStringList);
var
  I: Integer;
  Command: RExternalCommand;
begin
  for I := 0 to aParams.Count -1 do
    begin
      Command := SplitCommand(aParams[i]);
      if  (Command.Category = CATEGORY_NONE) then
        begin
          if not AnsiStartsText(Command.command,'--') and FileExists(Command.Command) then
           begin
             Command.Category:=CATEGORY_FILE;
             Command.Param:= Command.Command;
             Command.Command:=COMMAND_ENQUEUE;
           end;
        end;

       BackEnd.HandleExternalCommand(Command);
    end;
end;

procedure TDM.DataModuleCreate(Sender: TObject);
var
  ParamList: TStringList;
  i: Integer;
begin
  Application.SingleInstance.OnServerReceivedParams := @ServerReceivedParams;
  IF ParamCount > 0  then
   try
     ParamList := TStringList.Create;
     for i := 1 to ParamCount do
       ParamList.Add(ParamStr(I));
     ServerReceivedParams(Application.SingleInstance, ParamList);
   finally
     FreeAndNil(ParamList);
   end;



end;

procedure TDM.DataModuleDestroy(Sender: TObject);
begin
  Application.SingleInstance.OnServerReceivedParams:= nil;
end;

procedure TDM.SaveDialogPlaylistTypeChange(Sender: TObject);
var
  NewExt:string;
begin
  case SaveDialogPlayList.FilterIndex of
    1 : NewExt:='.xspf';
    2 : NewExt:='.m3u8';
    3 : NewExt:='.m3u';
  end;
  ChangeFileExt(SaveDialogPlayList.FileName,'newExt');
end;

procedure TDM.DebugLnHook(Sender: TObject; S: string; var Handled: Boolean);
begin
 if (TextRec(f).mode <> fmClosed) and (IOResult = 0) then
    WriteLn(f, S);
end;

procedure TDM.actPlaylistLoadExecute(Sender: TObject);
begin
  if OpenDialogPlaylist.Execute then
    begin
    Backend.PlayList.clear;
    Backend.Manager.LoadPlayList(OpenDialogPlaylist.FileName, Backend.PlayList);
    Backend.PlayList.LoadAllTags;
    if Assigned(Backend.OnPlayListLoad) then
      Backend.OnPlayListLoad(self);

    if (Backend.Manager.SavedTime <> 0) and
        Backend.PlayListParam.Restart and
       (Backend.PlayList.CurrentItem <> nil) then
      begin
         Backend.AudioEngine.Play(Backend.PlayList.CurrentItem, Backend.Manager.SavedTime);
      end;

    end;

end;

procedure TDM.actPlayListClearExecute(Sender: TObject);
begin
  Backend.Stop;
  Backend.PlayList.Clear;
  Backend.SignalPlayListChange;
end;

procedure TDM.actImportDirectoryExecute(Sender: TObject);
begin
 SelectDirectoryDialog.FileName := Backend.GeneralParam.LastImportFolder;
 if SelectDirectoryDialog.Execute then
   begin
     BackEnd.PlayList.Clear;
     Backend.Manager.ImportFromDirectory(SelectDirectoryDialog.FileName,true, Backend.PlayList);
     Backend.GeneralParam.LastImportFolder := SelectDirectoryDialog.FileName;
   end;
 Backend.SignalPlayListChange;
end;

procedure TDM.actPlayListShuffleExecute(Sender: TObject);
begin
  Backend.PlayList.Shuffle;
  Backend.SignalPlayListChange;
end;

procedure TDM.actPlayListAddFilesExecute(Sender: TObject);
var
  i: integer;
begin
  OpenDialogFiles.Filter := format(rAllFiles, [supportedExtension])+'|All Files|*.*';

  if not OpenDialogFiles.Execute then
    exit;

  for i := 0 to OpenDialogFiles.Files.Count - 1 do
    Backend.PlayList.EnqueueFile((OpenDialogFiles.Files[i]));

  Backend.SignalPlayListChange;

end;

procedure TDM.actExitExecute(Sender: TObject);
begin
  Backend.Quit;
end;

procedure TDM.actFastScanExecute(Sender: TObject);
begin
  Backend.MediaLibrary.Scan(Backend.MediaLibraryParam.LibraryPaths, false);
end;

procedure TDM.actMuteExecute(Sender: TObject);
begin
  actMute.Checked := not actMute.Checked;

  if actMute.Checked then
     begin
       actMute.ImageIndex := 18;
       Backend.Muted := true;
     end
  else
     begin
       actMute.ImageIndex := 19;
       Backend.Muted := false;
     end;

end;

procedure TDM.actNextExecute(Sender: TObject);
begin
  Backend.Next;
end;

procedure TDM.actPauseExecute(Sender: TObject);
begin
  Backend.Pause;
end;

procedure TDM.actPlayExecute(Sender: TObject);
begin
  Backend.Play;
end;

procedure TDM.actPreviousExecute(Sender: TObject);
begin
 Backend.Previous;
end;

procedure TDM.actRemoveMissingExecute(Sender: TObject);
begin
  Backend.mediaLibrary.RemoveMissing;
end;


procedure TDM.ChangeRepeatMode(Mode: TplRepeat; Notify:boolean=true);
begin
  Backend.PlayList.RepeatMode := Mode;
  case Mode of
    rptTrack : actRepeatTrack.Checked:=true;
    rptAlbum : actRepeatAlbum.Checked:=true;
    rptNone : actRepeatNone.Checked:=true;
    rptPlayList : actRepeatAll.Checked:=true;
  end;
  Backend.PlayListParam.RepeatMode := Ord(Backend.PlayList.RepeatMode);
  if Notify then
    Backend.Notify(cpLooping);
end;

procedure TDM.actRepeatTrackExecute(Sender: TObject);
begin
  ChangeRepeatMode(rptTrack);
end;

procedure TDM.actRepeatAlbumExecute(Sender: TObject);
begin
  ChangeRepeatMode(rptAlbum);
end;

procedure TDM.actRepeatAllExecute(Sender: TObject);
begin
  ChangeRepeatMode(rptPlayList);
end;

procedure TDM.actRepeatNoneExecute(Sender: TObject);
begin
  ChangeRepeatMode(rptNone);
end;

procedure TDM.actRescanCollectionExecute(Sender: TObject);

begin
  Backend.MediaLibrary.Scan(Backend.MediaLibraryParam.LibraryPaths);
end;

procedure TDM.actPlaylistSaveExecute(Sender: TObject);
var
  z :integer;
begin
  if not SaveDialogPlayList.Execute then
    exit;
 z:= SaveDialogPlayList.FilterIndex;
  case SaveDialogPlayList.FilterIndex of
    1   : Backend.Manager.SaveToXSPF(SaveDialogPlayList.FileName, Backend.PlayList, Backend.AudioEngine.Position);
    2,3 : Backend.Manager.SaveToM3U(SaveDialogPlayList.FileName, Backend.PlayList, Backend.AudioEngine.Position);
  end;

end;

procedure TDM.actRestartExecute(Sender: TObject);
begin
  Restart(Application);
end;

procedure TDM.actSkipBackwardExecute(Sender: TObject);
begin
  BackEnd.AudioEngine.Seek(-10, False);
  if Assigned(Backend.OnEngineCommand) then
     Backend.OnEngineCommand(Backend.AudioEngine, ecSeek);

end;

procedure TDM.actSkipForwardExecute(Sender: TObject);
begin
  BackEnd.AudioEngine.Seek(+10, False);
  if Assigned(Backend.OnEngineCommand) then
     Backend.OnEngineCommand(Backend.AudioEngine, ecSeek);

end;

procedure TDM.actStopExecute(Sender: TObject);
begin
  Backend.Stop;
end;

procedure TDM.ApplicationPropertiesDropFiles(Sender: TObject;
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

    Backend.Manager.ImportFromStrings(st, Backend.PlayList);
  finally
    st.free;
  end;
end;


procedure TDM.ApplicationPropertiesException(Sender: TObject; E: Exception
  );
var
  AfileName: string;
begin
  AFileName:= GetConfigDir+'ovoplayer.err';
  AssignFile(f, UTF8ToSys(aFileName));
  {$PUSH}{$I-}
  if not FileExistsUTF8(aFileName) then
    Rewrite(f)
  else
    Append(f);
  {$POP}

  DebugLogger.OnDebugLn:= @DebugLnHook;
  debugln('=============================================');
  debugln('| ', FormatDateTime('dd-mm-yyyy, hh:nn:ss', SysUtils.Now));
  debugln('| ', AppName, ' v', AppVersion, ' Rev. ', ovoRevision);
  if Assigned(e) then
    debugln('| TApplication.HandleException ', Exception(e).ClassName,' ', Exception(e).Message)
  else
    debugln('| TApplication.HandleException Strange Exception ');

  DumpExceptionBackTrace;

  DebugLogger.OnDebugLn:=nil;
  Flush(F);
  CloseFile(F);

end;

procedure TDM.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  Application.SingleInstance.ServerCheckMessages;
end;

initialization

end.
