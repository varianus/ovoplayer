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
  Classes, SysUtils, FileUtil,
  BaseTypes, CoreInterfaces,  forms,
  PlayList, AudioEngine, AudioEngine_dummy,
  PlayListManager, MediaLibrary, basetag, Song,
  MultimediaKeys, Config;

type

  TOnEngineCommand = procedure(Sender: Tobject; Command : TEngineCommand) of object;
  TOnExternalCommand = procedure(Sender: Tobject; Command : String) of object;

  { TBackEnd }

  TBackEnd = class(TInterfacedObject, IBackEnd)
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

    Constructor Create;
    Destructor Destroy; override;

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
    procedure HandleCommand(Command: TEngineCommand; Param: integer);

    function GetImageFromfolder(Path: string): string;
    procedure SaveState;
    procedure SignalPlayListChange;
    property OnSaveInterfaceState: TNotifyEvent read FOnSaveInterfaceState write SetOnSaveInterfaceState;
    property OnPlayListChange: TNotifyEvent read FOnPlayListChange write SetOnPlayListChange;
    property OnPlayListLoad: TNotifyEvent read FOnPlayListLoad write SetOnPlayListLoad;
    property OnEngineCommand :TOnEngineCommand read FOnEngineCommand write SetOnEngineCommand;
    property OnExternalCommand :TOnExternalCommand read FOnExternalCommand write SetOnExternalCommand;

  end;


function BackEnd: TBackEnd;

implementation

uses LCLProc, FilesSupport, AudioTag, AppConsts;

var
  fBackEnd: TBackEnd;


{ TBackEnd }

function BackEnd: TBackEnd;
begin
  if fBackEnd = nil then
    fBackEnd := TBackEnd.Create;

  Result := fBackEnd;
end;

constructor TBackEnd.Create;
var
  Engine : TAudioEngineClass;
begin
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
      fMultimediaKeys := TMultimediaKeys.Create(Config.InterfaceParam.CaptureMMkeysMode, self);
    end;

end;

procedure TBackEnd.SaveState;
begin
  Manager.SaveToXSPF(Config.ConfigDir + 'lastplaylist.xspf', PlayList, AudioEngine.Position);
  Config.EngineParam.Volume := AudioEngine.MainVolume;

end;

destructor TBackEnd.Destroy;
begin

  try
    SaveState;
    if Assigned(FOnSaveInterfaceState) then
       FOnSaveInterfaceState(Self);
  except
  end;
  fMultimediaKeys.Free;
  Manager.Free;
  PlayList.Free;
  AudioEngine.Free;
  mediaLibrary.Free;
  Config.Free;

  Inherited Destroy;
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
  ObserverList.Remove(observer);
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

procedure TBackEnd.AudioEngineSongEnd(Sender: TObject);
begin
  Next;
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
