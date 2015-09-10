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
unit Config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, inifiles;

const
  npkOSD = 2;
  npkNotifications = 1;

type

  TNotificationParam = record
    Kind: integer;
    X:    integer;
    Y:    integer;
    BackColor: TColor;
    FontColor: TColor;
    Transparency: integer;
    TimeOut: integer;
  end;

  TMediaLibraryParam = record
    LibraryPaths: TStringList;
    CheckOnStart: boolean;
  end;

  TInterfaceParam = record
    ShowTrayIcon: boolean;
    MinimizeOnClose: boolean;
    GroupBy: Integer;
    CaptureMMKeys: boolean;
    CaptureMMkeysMode: Integer;
    EnableSoundMenu: boolean;
  end;

  TPlayListParam = record
    Restart: boolean;
    RepeatMode:Integer;
  end;

  TEngineParam = record
    EngineKind : string;
    Volume : Integer;
  end;

  TGeneralParam = record
    LastImportFolder: String;
  end;

  TNetRemoteParam = record
    Enabled:boolean;
    Port: integer;
  end;

  { TConfig }


  TConfig = class
  private
    ConfigFile:    string;
    fConfigDir:    string;
    FNeedRestart: boolean;
    ResourcesPath: string;
    fIniFiles:     TMemIniFile;
    function ReadColor(const Section, Ident: string; const Default: TColor): TColor;
    procedure SetNeedRestart(AValue: boolean);
    procedure WriteStringS(Section: string; BaseName: string; Values: TStrings);
    function ReadStrings(Section: string; Name: string; Values: TStrings): integer;
    procedure WriteColor(const Section, Ident: string; const Value: TColor);
  public
    NotificationParam: TNotificationParam;
    MediaLibraryParam: TMediaLibraryParam;
    InterfaceParam:    TInterfaceParam;
    PlayListParam:     TPlayListParam;
    EngineParam:       TEngineParam;
    GeneralParam:      TGeneralParam;
    NetRemoteParam:    TNetRemoteParam;
    EngineSubParams:   TStringList;
    constructor Create;
    procedure ReadConfig;
    procedure SaveConfig;
    Procedure ReadSubParams(EngineKind:String='');
    procedure SaveSubParams(EngineKind:String='');
    Procedure ReadCustomParams(const Section:string; Params:TStrings);
    procedure SaveCustomParams(const Section:string; Params:TStrings);
    procedure RemoveSection(const Section:string);

    function GetResourcesPath: string;
    function GetPlaylistsPath: string;
    Property ConfigDir: string read fConfigDir;
    Property NeedRestart:boolean read FNeedRestart write SetNeedRestart;
    procedure Flush;
    destructor Destroy; override;
  end;

implementation

{ TConfig }
uses
  AppConsts, FilesSupport
{$ifdef Darwin}
  , MacOSAll
{$endif}  ;

const
  SectionUnix    = 'UNIX';
  IdentResourcesPath = 'ResourcesPath';

const
 {$ifdef UNIX}
  DefaultDirectory = '/usr/share/ovoplayer/';
 {$endif}

 {$ifdef DARWIN}
  BundleResourcesDirectory = '/Contents/Resources/';
 {$endif}

  SectionGeneral = 'General';

constructor TConfig.Create;
begin
  ConfigFile := GetAppConfigFile(False {$ifdef NEEDCFGSUBDIR} , true{$ENDIF} );
  fConfigDir :=  GetConfigDir;
  fIniFiles  := TMemIniFile.Create(ConfigFile, False);
  MediaLibraryParam.LibraryPaths := TStringList.Create;
  EngineSubParams:= TStringList.Create;
  ForceDirectories(GetPlaylistsPath);
  ReadConfig;

end;

destructor TConfig.Destroy;
begin
  SaveConfig;
  fIniFiles.UpdateFile;
  MediaLibraryParam.LibraryPaths.Free;
  EngineSubParams.Free;
  fIniFiles.Free;
  inherited Destroy;
end;

procedure TConfig.SaveConfig;
begin
  // MEDIA LIBRARY
  WriteStringS('MediaLibraryPaths', 'Path', MediaLibraryParam.LibraryPaths);
  fIniFiles.WriteBool('MediaLibrary', 'CheckOnStart', MediaLibraryParam.CheckOnStart);

  // NOTIFICATION
  fIniFiles.WriteInteger('Notification', 'Kind', NotificationParam.Kind);
  WriteColor('Notification', 'BackColor', NotificationParam.BackColor);
  WriteColor('Notification', 'FontColor', NotificationParam.FontColor);
  fIniFiles.WriteInteger('Notification', 'TimeOut', NotificationParam.TimeOut);
  fIniFiles.WriteInteger('Notification', 'X', NotificationParam.X);
  fIniFiles.WriteInteger('Notification', 'Y', NotificationParam.Y);
  fIniFiles.WriteInteger('Notification', 'Transparency', NotificationParam.Transparency);

  // INTERFACE
  fIniFiles.WriteBool('Interface', 'MinimizeOnClose', InterfaceParam.MinimizeOnClose);
  fIniFiles.WriteBool('Interface', 'ShowTrayIcon', InterfaceParam.ShowTrayIcon);
  fIniFiles.WriteInteger('Interface', 'GroupBy', InterfaceParam.GroupBy);
  fIniFiles.WriteBool('Interface', 'CaptureMMKeys', InterfaceParam.CaptureMMKeys);
  fIniFiles.WriteInteger('Interface', 'CaptureMMKeysMode', InterfaceParam.CaptureMMkeysMode);
  fIniFiles.WriteBool('Interface', 'EnableSoundMenu', InterfaceParam.EnableSoundMenu);

  // PLAYLIST
  fIniFiles.WriteBool('PlayList', 'Restart', PlayListParam.Restart);
  fIniFiles.WriteInteger('PlayList', 'RepeatMode', PlayListParam.RepeatMode);

  // ENGINE
  fIniFiles.WriteString('AudioEngine', 'Kind', EngineParam.EngineKind);
  fIniFiles.WriteInteger('AudioEngine', 'Volume', EngineParam.Volume);
  fIniFiles.WriteInteger('AudioEngine', 'Volume', EngineParam.Volume);

  // NETREMOTE
  fIniFiles.WriteBool('NetRemote', 'Enabled', NetRemoteParam.Enabled);
  fIniFiles.WriteInteger('NetRemote', 'Port', NetRemoteParam.Port);

  //GENERAL
  fIniFiles.WriteString('General', 'LastFolder', GeneralParam.LastImportFolder);
  fIniFiles.WriteString(SectionUnix, IdentResourcesPath, ResourcesPath);
  SaveSubParams;

end;

procedure TConfig.ReadSubParams(EngineKind:String='');
begin
  if EngineKind = '' then
     EngineKind := EngineParam.EngineKind;

  EngineSubParams.Clear;
  fIniFiles.ReadSectionValues('AudioEngine.'+ EngineKind, EngineSubParams)
end;

procedure TConfig.SaveSubParams(EngineKind:String='');
var
  Section:string;
  i :Integer;
begin
  if EngineKind = '' then
     EngineKind := EngineParam.EngineKind;

  Section:= 'AudioEngine.'+ EngineKind;

  for i := 0 to EngineSubParams.Count -1 do
    begin
       fIniFiles.WriteString(Section, EngineSubParams.Names[i], EngineSubParams.ValueFromIndex[i]);
    end;

end;

procedure TConfig.ReadCustomParams(const Section:string; Params: TStrings);
begin
  Params.Clear;
  fIniFiles.ReadSectionValues(Section, Params)
end;

procedure TConfig.SaveCustomParams(const Section:string; Params: TStrings);
var
  i :Integer;
begin
for i := 0 to Params.Count -1 do
  begin
     fIniFiles.WriteString(Section, Params.Names[i], Params.ValueFromIndex[i]);
  end;
end;

procedure TConfig.RemoveSection(const Section: string);
begin
  fIniFiles.EraseSection(Section);
end;


procedure TConfig.ReadConfig;
begin
  // MEDIA LIBRARY
  ReadStrings('MediaLibraryPaths', 'Path', MediaLibraryParam.LibraryPaths);
  MediaLibraryParam.CheckOnStart := fIniFiles.ReadBool('MediaLibrary', 'CheckOnStart', False);

  // NOTIFICATION
  NotificationParam.Kind := fIniFiles.ReadInteger('Notification', 'Kind', 2);
  NotificationParam.BackColor := ReadColor('Notification', 'BackColor', $00B66F18);
  NotificationParam.FontColor := ReadColor('Notification', 'FontColor', $00000000);
  NotificationParam.TimeOut := fIniFiles.ReadInteger('Notification', 'TimeOut', 3000);
  NotificationParam.X := fIniFiles.ReadInteger('Notification', 'X', 100);
  NotificationParam.Y := fIniFiles.ReadInteger('Notification', 'Y', 100);
  NotificationParam.Transparency := fIniFiles.ReadInteger('Notification', 'Transparency', 230);

  // INTERFACE
  InterfaceParam.MinimizeOnClose := fIniFiles.ReadBool('Interface', 'MinimizeOnClose', True);
  InterfaceParam.ShowTrayIcon := fIniFiles.ReadBool('Interface', 'ShowTrayIcon', True);
  InterfaceParam.GroupBy := fIniFiles.ReadInteger('Interface', 'GroupBy', 0);
  InterfaceParam.CaptureMMKeys := fIniFiles.ReadBool('Interface', 'CaptureMMKeys', False);
  InterfaceParam.CaptureMMkeysMode := fIniFiles.ReadInteger('Interface', 'CaptureMMkeysMode', 0);
  InterfaceParam.EnableSoundMenu := fIniFiles.ReadBool('Interface', 'EnableSoundMenu', True);

  // PLAYLIST
  PlayListParam.Restart :=  fIniFiles.ReadBool('PlayList', 'Restart', true);
  PlayListParam.RepeatMode :=  fIniFiles.ReadInteger('PlayList', 'RepeatMode', 0);

  // ENGINE
  EngineParam.EngineKind := fIniFiles.ReadString('AudioEngine', 'Kind', '');
  EngineParam.Volume := fIniFiles.ReadInteger('AudioEngine', 'Volume', 50);
  ReadSubParams;

  // NETREMOTE
  NetRemoteParam.Enabled := fIniFiles.ReadBool('NetRemote', 'Enabled', False);
  NetRemoteParam.Port := fIniFiles.ReadInteger('NetRemote', 'Port', 6860);

  //GENERAL
  GeneralParam.LastImportFolder := fIniFiles.ReadString('General', 'LastFolder', GetUserDir);

{$ifdef WINDOWS}
  ResourcesPath := fIniFiles.ReadString(SectionUnix, IdentResourcesPath, ExtractFilePath(
    ExtractFilePath(ParamStr(0))));
{$else}
  {$ifndef DARWIN}
  ResourcesPath := fIniFiles.ReadString(SectionUnix, IdentResourcesPath, DefaultResourceDirectory);
  {$endif}
{$endif}
end;

procedure TConfig.WriteStringS(Section: string; BaseName: string;
  Values: TStrings);
var
  i: integer;
begin
  fIniFiles.EraseSection(Section);
  for i := 0 to Values.Count - 1 do
    begin
    fIniFiles.WriteString(Section, BaseName + IntToStr(i), Values[i]);
    end;
end;

function TConfig.ReadStrings(Section: string; Name: string; Values: TStrings): integer;
var
  strs: TStringList;
  i:    integer;
begin
  Values.Clear;
  Strs := TStringList.Create;
  fIniFiles.ReadSectionValues(Section, Strs);
  for i := 0 to strs.Count - 1 do
    Values.Add(strs.ValueFromIndex[i]);
  strs.Free;
  Result := Values.Count;

end;

function TConfig.ReadColor(const Section, Ident: string; const Default: TColor): TColor;
var
  tmpString: string;
begin
  tmpString := fIniFiles.ReadString(Section, Ident, IntToHex(Default, 8));
  if not TryStrToInt(tmpString, Result) then
    Result := Default;
end;

procedure TConfig.SetNeedRestart(AValue: boolean);
begin
  if FNeedRestart=AValue then Exit;
  FNeedRestart:=AValue;
end;

procedure TConfig.WriteColor(const Section, Ident: string; const Value: TColor);
begin
  fIniFiles.WriteString(Section, Ident, '$' + IntToHex(Value, 8));
end;

procedure TConfig.Flush;
begin
  fIniFiles.UpdateFile;
end;

function TConfig.GetResourcesPath: string;
{$ifdef DARWIN}
var
  pathRef:   CFURLRef;
  pathCFStr: CFStringRef;
  pathStr:   shortstring;
{$endif}
begin
{$ifdef UNIX}
{$ifdef DARWIN}
  pathRef   := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + BundleResourcesDirectory;
{$else}
  Result := ResourcesPath;
{$endif}
{$endif}

{$ifdef WINDOWS}
  Result := ExtractFilePath(ExtractFilePath(ParamStr(0))) + ResourceSubDirectory + PathDelim;
{$endif}

end;

function TConfig.GetPlaylistsPath: string;
begin
  Result := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(fConfigDir)+'playlists');
end;

end.
