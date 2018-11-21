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

interface

uses
  Classes, SysUtils, inifiles, fgl;

type
  TConfig = class;

  { TConfigParam }
  TConfigParam = class(tobject)
  private
    FDirty: boolean;
    fOwner: TConfig;
    procedure SetDirty(AValue: boolean);
  Protected
    Procedure InternalSave; virtual; abstract;
  public
    property Dirty: boolean read FDirty write SetDirty;
    property Owner: TConfig read fOwner;
    Constructor Create(aOwner:TConfig); virtual;
    Destructor Destroy; override;
    Procedure Save;
    Procedure Load; virtual; abstract;
  end;

  { TConfig }

  TConfigList= specialize TFPGObjectList<TConfigParam>;

  TConfig = class
  private
    fDirty:        boolean;
    ConfigFile:    string;
    fConfigDir:    string;
    FNeedRestart:  boolean;
    fIniFiles:     TMemIniFile;
    fConfigList:  TConfigList;
    ResourcesPath: string;
    procedure Attach(cfgobject: TConfigParam);
    procedure Remove(cfgobject: TConfigParam);
    procedure SetDirty(AValue: boolean);
    procedure SetNeedRestart(AValue: boolean);
  public
    property Inifile: TMemIniFile read fIniFiles;
    Property Dirty: boolean read FDirty write SetDirty;
    constructor Create;
    procedure ReadConfig;
    procedure SaveConfig;
    Procedure ReadCustomParams(const Section:string; Params:TStrings);
    procedure SaveCustomParams(const Section:string; Params:TStrings);
    procedure RemoveSection(const Section:string);
    function GetResourcesPath: string;
    function GetPlaylistsPath: string;
    procedure WriteStringS(Section: string; BaseName: string; Values: TStrings);
    function ReadStrings(Section: string; Name: string; Values: TStrings): integer;

    Property ConfigDir: string read fConfigDir;
    Property NeedRestart:boolean read FNeedRestart write SetNeedRestart;
    procedure Flush;
    destructor Destroy; override;
  end;

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

implementation

{ TConfig }
uses
  AppConsts, FilesSupport
{$ifdef Darwin}
  , MacOSAll
{$endif}  ;

{ TConfigParam }

procedure TConfigParam.SetDirty(AValue: boolean);
begin
  if FDirty=AValue then Exit;
  FDirty:=AValue;
  if FDirty then
    fOwner.Dirty:=true;
end;

constructor TConfigParam.Create(aOwner: TConfig);
begin
  fOwner := AOwner;
  fOwner.Attach(Self);
  FDirty:=False;
end;

destructor TConfigParam.Destroy;
begin
  fOwner.Remove(Self);

  inherited Destroy;
end;

procedure TConfigParam.Save;
begin
  if FDirty then
    InternalSave;
  FDirty:=false;
end;


constructor TConfig.Create;
begin
  fDirty:= False;
  fConfigList:= TConfigList.Create(false);

  ConfigFile := GetAppConfigFile(False {$ifdef NEEDCFGSUBDIR} , true{$ENDIF} );
  fConfigDir :=  GetConfigDir;
  fIniFiles  := TMemIniFile.Create(ConfigFile, False);
  {$ifdef WINDOWS}
  ResourcesPath := ExtractFilePath(ParamStr(0));
  {$else}
   {$ifndef DARWIN}
  ResourcesPath := DefaultResourceDirectory;
   {$endif}
 {$endif}

  ForceDirectories(GetPlaylistsPath);
  ReadConfig;

end;

destructor TConfig.Destroy;
begin
  SaveConfig;

  fConfigList.Free;

  fIniFiles.Free;
  inherited Destroy;
end;

procedure TConfig.SaveConfig;
var
  i: integer;
begin

  for i := 0 to Pred(fConfigList.Count) do
    if fConfigList[i].Dirty then
       begin
         fConfigList[i].Save;
         FDirty:= true;
       end;
  if fDirty then
    begin
      IniFile.WriteString(SectionUnix, IdentResourcesPath, ResourcesPath);
      fIniFiles.UpdateFile;
    end;

  fDirty := false;

end;

procedure TConfig.ReadConfig;
var
  i: integer;
begin
  {$ifdef WINDOWS}
    ResourcesPath := IniFile.ReadString(SectionUnix, IdentResourcesPath, ExtractFilePath(ExtractFilePath(ParamStr(0))));
    {$else}
     {$ifndef DARWIN}
    ResourcesPath := IniFile.ReadString(SectionUnix, IdentResourcesPath, DefaultResourceDirectory);
     {$endif}
  {$endif}

  for i := 0 to Pred(fConfigList.Count) do
  begin
     fConfigList[i].Load;
  end;
end;


procedure TConfig.Attach(cfgobject: TConfigParam);
begin
  fConfigList.Add(cfgobject);
  cfgobject.Load;
end;

procedure TConfig.Remove(cfgobject: TConfigParam);
begin
  cfgobject.Save;
  fConfigList.Remove(cfgobject);
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

procedure TConfig.SetNeedRestart(AValue: boolean);
begin
  if FNeedRestart=AValue then Exit;
  FNeedRestart:=AValue;
end;

procedure TConfig.SetDirty(AValue: boolean);
begin
  if FDirty=AValue then Exit;
  FDirty:=AValue;
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
