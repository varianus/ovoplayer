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
unit uDM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ActnList, Controls, Dialogs, Forms, LResources,
  BaseTypes, CoreInterfaces,
  AudioEngine,
  PlayListManager, MediaLibrary, UniqueInstance;

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
    UniqueInstanceI: TUniqueInstance;
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
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure UniqueInstanceIOtherInstance(Sender: TObject; ParamCount: Integer;
      Parameters: array of String);
  public

  end;

var
  DM: TDM;

implementation

{$R *.lfm}
uses LCLProc, strutils, AudioTag, AppConsts, GeneralFunc, GUIBackEnd;


{ TDM }


procedure TDM.DataModuleCreate(Sender: TObject);
begin
  Application.OnException := @ApplicationPropertiesException;

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

procedure TDM.DataModuleDestroy(Sender: TObject);
begin
  UniqueInstanceI.free;
end;

procedure TDM.UniqueInstanceIOtherInstance(Sender: TObject;
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
                Backend.Play                     else
             if tempstr = 'stop'     then
                Backend.Stop                     else
             if tempstr = 'pause'    then
                Backend.Pause                    else
             if tempstr = 'next'     then
                Backend.Next                     else
             if tempstr = 'previous' then
                Backend.Previous                 else
             if tempstr = 'quit' then
                Backend.Quit;

             if Assigned(Backend.OnExternalCommand) then
                Backend.OnExternalCommand(Self, tempstr);

           end;

         if AnsiStartsStr('file:', Parameters[i]) then
           begin
             tempstr:=copy(Parameters[i], 6, 2);
             tempparam:=copy(Parameters[i], 8, Length(Parameters[i]));
             if tempstr = 'e=' then
                begin
                  idx := Backend.PlayList.EnqueueFile(tempparam);
                  Backend.SignalPlayListChange;
                end else
             if tempstr = 'p=' then
                begin
                  Backend.OpenUri(tempparam)
                end  else
             if tempstr = 'x=' then
                begin
                  idx := Backend.PlayList.EnqueueFile(tempparam);
                  Backend.PlayList.ItemIndex:=idx;
                  Backend.AudioEngine.Play(Backend.PlayList.CurrentItem);
                  Backend.SignalPlayListChange;
                end;

           end;
      end;

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
        Backend.Config.PlayListParam.Restart and
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
 SelectDirectoryDialog.FileName := Backend.Config.GeneralParam.LastImportFolder;
 if SelectDirectoryDialog.Execute then
   begin
     Backend.Manager.ImportFromDirectory(SelectDirectoryDialog.FileName,true, Backend.PlayList);
     Backend.Config.GeneralParam.LastImportFolder := SelectDirectoryDialog.FileName;
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
  Backend.MediaLibrary.Scan(Backend.Config.MediaLibraryParam.LibraryPaths, false);
end;

procedure TDM.actMuteExecute(Sender: TObject);
begin
  actMute.Checked := not actMute.Checked;

  if actMute.Checked then
     begin
       actMute.ImageIndex := 18;
       Backend.Mute;
     end
  else
     begin
       actMute.ImageIndex := 19;
       Backend.UnMute;
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

procedure TDM.actRepeatTrackExecute(Sender: TObject);
begin
  Backend.PlayList.RepeatMode := rptTrack;
  actRepeatTrack.Checked := true;
  Backend.Config.PlayListParam.RepeatMode := Ord(Backend.PlayList.RepeatMode);
  Backend.Notify(cpLooping);
end;

procedure TDM.actRepeatAlbumExecute(Sender: TObject);
begin
  Backend.PlayList.RepeatMode := rptAlbum;
  actRepeatAlbum.Checked:=true;
  Backend.Config.PlayListParam.RepeatMode := Ord(Backend.PlayList.RepeatMode);
  Backend.Notify(cpLooping);
end;

procedure TDM.actRepeatAllExecute(Sender: TObject);
begin
  Backend.PlayList.RepeatMode := rptPlayList;
  actRepeatAll.Checked:=true;
  Backend.Config.PlayListParam.RepeatMode := Ord(Backend.PlayList.RepeatMode);
  Backend.Notify(cpLooping);
end;

procedure TDM.actRepeatNoneExecute(Sender: TObject);
begin
  Backend.PlayList.RepeatMode := rptNone;
  actRepeatNone.Checked:=true;
  Backend.Config.PlayListParam.RepeatMode := Ord(Backend.PlayList.RepeatMode);
  Backend.Notify(cpLooping);
end;

procedure TDM.actRescanCollectionExecute(Sender: TObject);

begin
  Backend.MediaLibrary.Scan(Backend.Config.MediaLibraryParam.LibraryPaths);
end;

procedure TDM.actPlaylistSaveExecute(Sender: TObject);
begin
  if not SaveDialogPlayList.Execute then
    exit;

  Backend.Manager.SaveToXSPF(SaveDialogPlayList.FileName, Backend.PlayList, Backend.AudioEngine.Position);

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
    if not FileExistsUTF8(aFileName) then
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


procedure TDM.ApplicationPropertiesException(Sender: TObject; E: Exception
  );
begin
  DumpExceptionCallStack(e);
  Halt; // End of program execution
end;

initialization
end.
