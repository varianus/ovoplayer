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
{$I backend.inc}
unit GUIBackEnd;
interface

uses
  Classes, SysUtils, LazFileUtils,
  BaseTypes, CoreInterfaces,
  audioengine, audioengine_dummy, AudioEngine_MPlayer,
  {$IFDEF GSTREAMER} gstreamer, audioengine_gstreamer, {$ENDIF}
  {$IFDEF VLC} PasLibVlcUnit, audioengine_vlc, {$ENDIF}
  {$IFDEF XINE} xine, audioengine_Xine,{$ENDIF}
  {$IFDEF BASS} lazdynamic_bass, audioengine_bass,{$ENDIF}
  {$IFDEF DSHOW} mediadshow, audioengine_dshow,{$ENDIF}
  {$IFDEF MEDIAFOUNDATION} mediafoundation, audioengine_mf,{$ENDIF}
  {$IF defined(OPENSOURCELIB) OR defined(UOS)}  uos_libsndfile, uos_mpg123, uos_portaudio,{$ENDIF}
  {$IFDEF OPENSOURCELIB}audioengine_OpenLib,{$ENDIF}
  {$IFDEF UOS} UOS,audioengine_UOS,{$ENDIF}
  {$IFDEF FFMPEG} ffmpeg, audioengine_FFMPEG,{$ENDIF}
  {$IFDEF LIBMPV} libmpv, audioengine_libmpv,{$ENDIF}
  extctrls,

  PlayList, PlayListManager, MediaLibrary, basetag, CustomSong,
  Config, Equalizer, backendconfig, fgl,
  FpTimer;

type

  TOnEngineCommand = procedure(Sender: Tobject; Command : TEngineCommand) of object;
  TOnExternalCommand = procedure(Sender: Tobject; Command : RExternalCommand; var Handled:boolean) of object;

  { TBackEnd }
  TObserverlist = specialize TFPGList<IObserver>;

  TBackEnd = class(TObject, IBackEnd)
  private
    { private declarations }
    FOnEngineCommand: TOnEngineCommand;
    FOnExternalCommand: TOnExternalCommand;
    FOnPlayListChange: TNotifyEvent;
    FOnPlayListLoad:   TNotifyEvent;

    FOnSaveInterfaceState: TNotifyEvent;
    ObserverList : TObserverlist;
    IntTimer: TTimer;

    fEngineParam: TEngineParam;
    fGeneralParam: TGeneralParam;
    FMediaLibraryParam: TMediaLibraryParam;
    fPlayListParam: TPlayListParam;
    fEqualizerParam :TEqualizerParam;

    procedure AudioEngineSongEnd(Sender: TObject);
    function GetMute: boolean;
    procedure PlaylistOnSongAdd(Sender: Tobject; Index: Integer; ASong: TCustomSong);
    procedure SetMute(AValue: boolean);
    procedure SetOnEngineCommand(const AValue: TOnEngineCommand);
    procedure SetOnExternalCommand(const AValue: TOnExternalCommand);
    procedure SetOnPlayListChange(const AValue: TNotifyEvent);
    procedure SetOnPlayListLoad(const AValue: TNotifyEvent);
    procedure SetOnSaveInterfaceState(AValue: TNotifyEvent);
    procedure TimerEvent(Sender: TObject);

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
    Function GetCoverURL: String;
    Function GetCover(Width: integer=-1; Height:Integer=-1): String;


    Procedure Play(Index:integer=-1);
    Procedure Stop;
    Procedure Pause;
    Procedure UnPause;
    Procedure Next;
    Procedure Previous;
    Procedure Quit;
    Function GetMetadata(Index:integer=-1): TCommonTags;
    Procedure OpenURI(URI: String);
    procedure Seek(AValue: int64);
    Function PlayListCount : integer;
    Function GetCurrentSongIndex : integer;
    Procedure AutoSendPosEvents(Active: boolean);

    Procedure Attach(observer: iObserver);
    Procedure Remove(observer: iObserver);

    Procedure Notify(Kind:  TChangedProperty);
    procedure HandleCommand(Command: TEngineCommand; Param: integer);
    function HandleExternalCommand(Command: RExternalCommand):boolean;

    function GetImageFromfolder(Path: string; LoadDefault:boolean=true): string;
    procedure SaveState;
    procedure SignalPlayListChange;

    property OnSaveInterfaceState: TNotifyEvent read FOnSaveInterfaceState write SetOnSaveInterfaceState;
    property OnPlayListChange: TNotifyEvent read FOnPlayListChange write SetOnPlayListChange;
    property OnPlayListLoad: TNotifyEvent read FOnPlayListLoad write SetOnPlayListLoad;
    property OnEngineCommand :TOnEngineCommand read FOnEngineCommand write SetOnEngineCommand;
    property OnExternalCommand :TOnExternalCommand read FOnExternalCommand write SetOnExternalCommand;
    Property Status: TengineState read GetStatus write SetStatus;
    Property Position: int64 read GetPosition write SetPosition;
    property Looping: TplRepeat read  GetLooping write SetLooping;
    Property Volume : cardinal read GetVolume write SetVolume;
    Property Muted : boolean read GetMute write SetMute;

    property MediaLibraryParam: TMediaLibraryParam read FMediaLibraryParam;
    property PlayListParam:     TPlayListParam read fPlayListParam;
    property EngineParam:       TEngineParam read fEngineParam;
    property GeneralParam:      TGeneralParam read fGeneralParam;
    property EqualizerParam:    TEqualizerParam read fEqualizerParam;

  end;


function BackEnd: TBackEnd;
Procedure FreeBackend;

implementation

uses Graphics, LazLoggerBase, FilesSupport, AudioTag, AppConsts, ExtendedInfo, uriparser,
     NetProtocol, NetSupport, ImagesSupport;

var
  fBackEnd: TBackEnd;

const
  RCommandString : array [TEngineCommand] of string =
                  ('', COMMAND_STOP, COMMAND_PREVIOUS, COMMAND_PLAYPAUSE, COMMAND_NEXT,
                   COMMAND_PAUSE, COMMAND_SEEK, '');


{ TBackEnd }

function BackEnd: TBackEnd;
begin
  if fBackEnd = nil then
    fBackEnd := TBackEnd.Create;

  Result := fBackEnd;
end;

procedure FreeBackend;
var
  i: integer;
begin
  try
  if Assigned(fBackEnd.ObserverList) then
     for i := 0 to fBackEnd.ObserverList.Count -1 do
        fBackEnd.remove(IObserver(fBackEnd.ObserverList[i]));
  except
    // ignore exception on observer destroy
  end;
  fBackEnd.Free;
  fBackEnd := nil;

end;


constructor TBackEnd.Create;
var
  Engine : TAudioEngineClass;
  Function EngineCreation:boolean;
   begin
     try
       AudioEngine := Engine.Create;
       if not AudioEngine.Initialize then
           begin
             FreeAndNil(AudioEngine);
             SetEngineFailed(Engine);
             Engine:= nil;
             result := False;
           end
       else
         Result:= true;
     except
       FreeAndNil(AudioEngine);
       SetEngineFailed(Engine);
       Engine:= nil;
       result := False;
     end;
   end;

begin
  IntTimer := nil;
  Config := TConfig.Create;
  fMediaLibraryParam := TMediaLibraryParam.Create(Config);
  fPlayListParam     := TPlayListParam.Create(Config);
  fEngineParam       := TEngineParam.Create(Config);
  fGeneralParam      := TGeneralParam.Create(Config);
  fEqualizerParam    := TEqualizerParam.Create(Config);

  Manager  := TPlayListManager.Create;
  Playlist := TPlayList.Create;
  PlayList.RepeatMode :=  TplRepeat(PlayListParam.RepeatMode);
  mediaLibrary := TMediaLibrary.Create;
  engine := nil;
  if EngineParam.EngineKind <> '' then
     begin
       Engine := GetEngineByName(EngineParam.EngineKind);
       if Assigned(Engine) then
          if not Engine.IsAvalaible(EngineParam.EngineSubParams) then
             engine := nil
          else
            EngineCreation;
     end;

  while not assigned(engine) do
    begin
      Engine := GetBestEngine;
      if Engine = nil then  // no more engine to try ...
        engine:=TAudioEngineDummy
      else
        EngineParam.EngineKind := Engine.getEngineName;

      if not Engine.IsAvalaible(EngineParam.EngineSubParams) then
         begin
           SetEngineFailed(Engine);
           Engine:= nil;
         end
      else
        EngineCreation;
    end;

  if AudioEngine.SupportEQ then
    begin
      AudioEngine.SetActiveEQ(EngineParam.ActiveEQ);
      // if equalizer is active, try to load active preset
      if EngineParam.ActiveEQ then
        begin
          if (EngineParam.EQPreset > 0) and (EngineParam.EQPreset < EqualizerParam.Count) then
            ApplyPreset(AudioEngine, EqualizerParam.Preset[EngineParam.EQPreset])
          else
            begin
              // If something wrong happened loading presets disable equalizer,
              // better than access violation or playing crap...
              EngineParam.EQPreset :=0;
              EngineParam.ActiveEQ := false;
              AudioEngine.SetActiveEQ(EngineParam.ActiveEQ);
            end;
        end;
    end;

  AudioEngine.OnSongEnd := @AudioEngineSongEnd;
  AudioEngine.MainVolume:= EngineParam.Volume;
  PlayList.OnSongAdd:=@PlaylistOnSongAdd;

end;

procedure TBackEnd.SaveState;
begin
  Manager.SaveToXSPF(Config.ConfigDir + 'lastplaylist.xspf', PlayList, AudioEngine.Position);
  EngineParam.Volume := AudioEngine.MainVolume;
  EngineParam.ActiveEQ := AudioEngine.getActiveEQ;

end;

destructor TBackEnd.Destroy;
begin
  if Assigned(IntTimer) then
    begin
   //   IntTimer.StopTimer;
      IntTimer.Enabled:= false;
      IntTimer.Free;
    end;
  try
    SaveState;
    if Assigned(FOnSaveInterfaceState) then
       FOnSaveInterfaceState(Self);
  except
  end;

  EqualizerParam.Dirty := true;

  Manager.Free;
  PlayList.Free;
  AudioEngine.Free;
  mediaLibrary.Free;

  fMediaLibraryParam.Free;
  fPlayListParam.Free;
  fEngineParam.Free;
  fGeneralParam.Free;
  fEqualizerParam.Free;
  Config.Free;

  Inherited Destroy;
end;



function TBackEnd.GetImageFromfolder(Path: string; LoadDefault:boolean): string;
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
       FileList.OwnsObjects := true;
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

  if (Result = '') and LoadDefault then
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
    Result := PlayList.RepeatMode
  else
    Result := rptNone;
end;

function TBackEnd.GetPosition: int64;
begin
  Result := AudioEngine.Position;
end;

function TBackEnd.GetStatus: TEngineState;
begin
  if Assigned(AudioEngine) then
     Result := AudioEngine.State
  else
     Result := ENGINE_OFF_LINE;

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
//  DebugLn('TBackend.setvolume','->',IntToStr(AValue));
  AudioEngine.MainVolume := AValue;
  Notify(cpVolume);
end;

function TBackEnd.GetCoverURL: String;
var
  Picture: TPicture;
  imgLoaded : boolean;
  Song: TCustomSong;
  f: TTagReader;
begin
  result := '';
  Song :=  PlayList.CurrentItem;
  if not assigned(song) then exit;

  imgloaded := false;
  if Song.Tags.HasImage then
     begin
       f := GetFileTagsObject(Song.Tags.FileName);
       f.Tags.Images[0].image.Position:=0;
       Picture:= tpicture.Create;
       try
         Picture.LoadFromStream(f.Tags.Images[0].image);
         result :=GetTempDir(true)+'ovoplayer-tmp-cover'+'.png';
         Picture.SaveToFile(result);
         imgLoaded:= true;
       except
       end;
       Picture.free;
       f.Free;
     end;

  if not imgLoaded then
     begin
      result := BackEnd.GetImageFromfolder(IncludeTrailingPathDelimiter(Song.FilePath));
     end;

  if Result <> '' then
     result := FilenameToURI(result);

end;

function TBackEnd.GetCover(Width: integer=-1; Height:Integer=-1): String;
var
  imgLoaded : boolean;
  Song: TCustomSong;
  img: TPicture;
  tmpStream: TMemoryStream;

  f: TTagReader;
  FileName: string;

begin
  result := '';
  Song :=  PlayList.CurrentItem;
  if not assigned(song) then exit;

  img := TPicture.Create;

  imgLoaded := false;
  if Song.Tags.HasImage then
     begin
       f := GetFileTagsObject(Song.Tags.FileName);
       try
         img.LoadFromStream(f.Tags.Images[0].image);
         imgLoaded:= true;
       except
       end;
       f.free;
     end;

  if not imgLoaded then
     begin
      FileName := BackEnd.GetImageFromfolder(IncludeTrailingPathDelimiter(Song.FilePath), false);
      if FileName <> '' then
        begin
          Img.LoadFromFile(FileName);
          imgLoaded := true;
        end;
     end;

  if not imgLoaded then
    begin
      img.free;
      exit
    end;

  try
    if (Width <> -1) and (Height <> -1) then
      ResizeBitmap(img.Bitmap, Width, Height);

    tmpStream := TMemoryStream.Create;
    try
//      img.SaveToStreamWithFileExt(tmpStream, '.jpg');
      img.SaveToStream(tmpStream);
    //  DebugLn('STSize: '+IntToStr(tmpStream.Size));
      Result := 'data:'+img.Graphic.MimeType+';base64, '+ EncodeStream(tmpStream);
    //  DebugLn('IMGSize: '+inttostr(length(result)));

    finally
      img.free;
      tmpStream.Free;
    end;


  Except
  end;

end;

procedure TBackEnd.Play(Index:integer=-1);
begin
  if (AudioEngine.State = ENGINE_PAUSE) and
     ((index = -1) or (Index =  PlayList.ItemIndex)) then
    AudioEngine.UnPause
  else
    begin
      if PlayList.Count = 0 then
         exit;

      if Index <> -1 then
        PlayList.ItemIndex:= index;

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

  Notify(cpClosing);

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

function TBackEnd.GetMetadata(Index:integer=-1): TCommonTags;
begin
  Result := Default(TCommonTags);

  If (index<1) then
    begin
      if Assigned(PlayList.CurrentItem) then
         Result:=PlayList.CurrentItem.Tags
    end
  else
    if (Index <= PlayList.Count) and (PlayList.Count > 0) then
       Result:= TCustomSong(PlayList.Items[Index-1]).Tags;
  result.Index := Index;
end;

procedure TBackEnd.Seek(AValue: int64);
begin
  AudioEngine.Seek(AValue,False);
  Notify(cpPosition);
end;

function TBackEnd.PlayListCount: integer;
begin
  Result:=PlayList.Count;
end;

function TBackEnd.GetCurrentSongIndex: integer;
begin
  Result := PlayList.ItemIndex + 1;
end;

procedure TBackEnd.Attach(observer: iObserver);
begin
  if not Assigned(ObserverList) then
     ObserverList := TObserverlist.create;
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
       IObserver(ObserverList[i]).UpdateProperty(Kind);
end;

procedure TBackEnd.SignalPlayListChange;
begin
  if Assigned(OnPlayListChange) then
    OnPlayListChange(PlayList);

end;

procedure TBackEnd.TimerEvent(Sender: TObject);
begin
  if AudioEngine.State = ENGINE_PLAY then
    Notify(cpPlayPos);
end;


procedure TBackEnd.AutoSendPosEvents(Active: boolean);
begin
  if Active then
    begin
      if not Assigned(IntTimer) then
        begin
          IntTimer := TTimer.Create(nil);
          IntTimer.Interval:= 500;
          IntTimer.OnTimer:=@TimerEvent;
          IntTimer.Enabled:=true;
        end;
//      IntTimer.StartTimer;
        IntTimer.Enabled:=true;
    end
  Else
    begin
      if Assigned(IntTimer) then
        begin
//          IntTimer.StopTimer;
            IntTimer.Enabled:=False;
        end;
    end;

end;

procedure TBackEnd.AudioEngineSongEnd(Sender: TObject);
begin
  Next;
end;

function TBackEnd.GetMute: boolean;
begin
  Result := AudioEngine.Muted;
end;

procedure TBackEnd.PlaylistOnSongAdd(Sender: Tobject; Index: Integer;
  ASong: TCustomSong);
var
  ID: Integer;
//  ExtendedInfo: TExtendedInfo;
begin
  ID := mediaLibrary.IDFromFullName(ASong.FullName);
  if ID = -1 then
     exit;
  mediaLibrary.AddInfoToSong(Id, ASong);

//
//  if ID = -1 then
//    exit;
//  ExtendedInfo := mediaLibrary.InfoFromID(ID);

end;

procedure TBackEnd.SetMute(AValue: boolean);
begin
  if AValue = AudioEngine.Muted then
    exit;
  AudioEngine.Muted:= AValue;
  Notify(cpMute);
end;

procedure TBackEnd.HandleCommand(Command: TEngineCommand; Param: integer);
var
  tmpCommand: RExternalCommand ;
begin

  tmpCommand.Category := CATEGORY_ACTION;
  tmpCommand.Command := RCommandString[Command];
  tmpCommand.Param := inttostr(Param);
  HandleExternalCommand(tmpCommand);

end;

function TBackEnd.HandleExternalCommand(Command: RExternalCommand):boolean;
var
  Handled: boolean;
  idx: integer;
begin
  Handled:=false;

  if Command.Category = CATEGORY_ACTION then
    case Command.Command of
       COMMAND_PLAY     : begin

                            Play(StrToIntDef(Command.Param, -1));
                            Handled := true; end;
       COMMAND_STOP     : begin Stop; Handled := true; end;
       COMMAND_PAUSE    : begin Pause; Handled := true; end;
       COMMAND_PLAYPAUSE: begin
                            if Status = ENGINE_PLAY then
                              Pause
                            else
                              Play;
                            Handled := true;
                          end;
       COMMAND_NEXT     : begin Next; Handled := true; end;
       COMMAND_PREVIOUS : begin Previous; Handled := true; end;
       COMMAND_MUTE     : begin Muted:= true; Handled := true; end;
       COMMAND_UNMUTE   : begin Muted:= false; Handled := true; end;
       COMMAND_SETVOL   : begin
                              Volume :=StrToIntDef(Command.Param, Volume);
                              Handled := true;
                          end;
       COMMAND_SEEK     : position := (StrToIntDef(Command.Param, Position));

       COMMAND_LOOPING  : Looping := TplRepeat(StrToIntDef(Command.Param, ord(Looping)));

    end;

  if Command.Category = CATEGORY_APP then
    case Command.Command of
       COMMAND_QUIT     : begin Quit; Handled := true; end;
    end;

  if Command.Category = CATEGORY_FILE then
     case command.Command of
       COMMAND_ENQUEUE :
                  begin
                    idx := PlayList.EnqueueFile(Command.Param);
                    Handled:=true;
                    SignalPlayListChange;
                  end;
       COMMAND_CLEAR_AND_PLAY :
                  begin
                    OpenUri(Command.Param);
                    Handled:=true;
                  end;
       COMMAND_ENQUEUE_AND_PLAY :
                  begin
                    idx := PlayList.EnqueueFile(Command.Param);
                    PlayList.ItemIndex:=idx;
                    AudioEngine.Play(PlayList.CurrentItem);
                    Handled:=true;
                    SignalPlayListChange;
                  end;
     end;
  // in not handled by backend, forward event
  if not Handled and Assigned(FOnExternalCommand) then
     FOnExternalCommand(Self, Command, Handled);

  Result := handled;
end;


initialization
  fBackEnd := nil;
end.
