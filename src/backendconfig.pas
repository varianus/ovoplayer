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
unit BackendConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles, Config;

{ TMediaLibraryParam }
type

  TMediaLibraryParam = Class(TConfigParam)
  private
    FCheckOnStart: boolean;
    FLibraryPaths: TStringList;
    procedure SetCheckOnStart(AValue: boolean);
    procedure SetLibraryPaths(AValue: TStringList);
  protected
    Procedure InternalSave; override;
  public
    property LibraryPaths: TStringList read FLibraryPaths write SetLibraryPaths;
    property CheckOnStart: boolean read FCheckOnStart write SetCheckOnStart;
    Constructor Create(aOwner:TConfig); override;
    Destructor Destroy; override;
    Procedure Load; override;
  end;

  { TPlayListParam }

  TPlayListParam = Class(TConfigParam)
  private
   FRepeatMode: Integer;
   FRestart: boolean;
   procedure SetRepeatMode(AValue: Integer);
   procedure SetRestart(AValue: boolean);
  protected
    Procedure InternalSave; override;
  public
   property Restart: boolean read FRestart write SetRestart;
   property RepeatMode:Integer read FRepeatMode write SetRepeatMode;
   Procedure Load; override;
  end;

  { TEngineParam }

  TEngineParam = Class(TConfigParam)
  private
    FActiveEQ: boolean;
    FEngineKind: string;
    fEngineSubParams: TStringList;
    FEQPreset: integer;
    FVolume: Integer;
    procedure ReadSubParams(AEngineKind: String='');
    procedure SaveSubParams(AEngineKind: String='');
    procedure SetActiveEQ(AValue: boolean);
    procedure SetEngineKind(AValue: string);
    procedure SetEQPreset(AValue: integer);
    procedure SetVolume(AValue: Integer);
  protected
    Procedure InternalSave; override;
  public
    Constructor Create(aOwner:TConfig); override;
    Destructor Destroy; override;
    property EngineSubParams:   TStringList read fEngineSubParams;
    property EngineKind : string read FEngineKind write SetEngineKind;
    property Volume : Integer read FVolume write SetVolume;
    property ActiveEQ: boolean read FActiveEQ write SetActiveEQ;
    property EQPreset: integer read FEQPreset write SetEQPreset;
    Procedure Load; override;
  end;

  { TGeneralParam }

  TGeneralParam = Class(TConfigParam)
  private
    FLastImportFolder: String;
    ResourcesPath: string;
    procedure SetLastImportFolder(AValue: String);
  protected
    Procedure InternalSave; override;
  public
    property LastImportFolder: String read FLastImportFolder write SetLastImportFolder;
    Procedure Load; override;
  end;


implementation

{ TGeneralParam }

procedure TGeneralParam.SetLastImportFolder(AValue: String);
begin
  if FLastImportFolder=AValue then Exit;
  FLastImportFolder:=AValue;
  Dirty:= true;
end;

procedure TGeneralParam.InternalSave;
begin
  owner.IniFile.WriteString('General', 'LastFolder', LastImportFolder);
  owner.IniFile.WriteString(SectionUnix, IdentResourcesPath, ResourcesPath);

end;

procedure TGeneralParam.Load;
begin
  fLastImportFolder := owner.IniFile.ReadString('General', 'LastFolder', GetUserDir);
  {$ifdef WINDOWS}
  ResourcesPath := owner.IniFile.ReadString(SectionUnix, IdentResourcesPath, ExtractFilePath(
    ExtractFilePath(ParamStr(0))));
  {$else}
   {$ifndef DARWIN}
  ResourcesPath := owner.IniFile.ReadString(SectionUnix, IdentResourcesPath, DefaultResourceDirectory);
   {$endif}
 {$endif}

end;


{ TPlayListParam }

procedure TPlayListParam.SetRepeatMode(AValue: Integer);
begin
  if FRepeatMode=AValue then Exit;
  FRepeatMode:=AValue;
  Dirty:= true;

end;

procedure TPlayListParam.SetRestart(AValue: boolean);
begin
  if FRestart=AValue then Exit;
  FRestart:=AValue;
  Dirty:= true;

end;

procedure TPlayListParam.InternalSave;
begin
  owner.IniFile.WriteBool('PlayList', 'Restart', Restart);
  owner.IniFile.WriteInteger('PlayList', 'RepeatMode', RepeatMode);

end;

procedure TPlayListParam.Load;
begin
  fRestart := owner.IniFile.ReadBool('PlayList', 'Restart', true);
  fRepeatMode := owner.IniFile.ReadInteger('PlayList', 'RepeatMode', 0);

end;

{ TMediaLibraryParam }

procedure TMediaLibraryParam.SetCheckOnStart(AValue: boolean);
begin
  if FCheckOnStart=AValue then Exit;
  FCheckOnStart:=AValue;
  Dirty:= true;
end;

procedure TMediaLibraryParam.SetLibraryPaths(AValue: TStringList);
begin
  if FLibraryPaths=AValue then Exit;
  FLibraryPaths:=AValue;
  Dirty:= true;

end;

constructor TMediaLibraryParam.Create(aOwner: TConfig);
begin
  FLibraryPaths := TStringList.Create;
  inherited Create(aOwner);
end;

destructor TMediaLibraryParam.Destroy;
begin
  inherited;
  LibraryPaths.Free;
end;

procedure TMediaLibraryParam.InternalSave;
begin
  Owner.WriteStringS('MediaLibraryPaths', 'Path', LibraryPaths);
  Owner.IniFile.WriteBool('MediaLibrary', 'CheckOnStart', CheckOnStart);

end;

procedure TMediaLibraryParam.Load;
begin
  Owner.ReadStrings('MediaLibraryPaths', 'Path', FLibraryPaths);
  fCheckOnStart := Owner.IniFile.ReadBool('MediaLibrary', 'CheckOnStart', False);
end;

{ TEngineParam }

procedure TEngineParam.SetEngineKind(AValue: string);
begin
  if FEngineKind=AValue then Exit;
  FEngineKind:=AValue;
  Dirty:= true;
end;

procedure TEngineParam.SetActiveEQ(AValue: boolean);
begin
  if FActiveEQ=AValue then Exit;
  FActiveEQ:=AValue;
  Dirty:= true;

end;

procedure TEngineParam.SetEQPreset(AValue: integer);
begin
  if FEQPreset=AValue then Exit;
  FEQPreset:=AValue;
  Dirty:= true;

end;

procedure TEngineParam.SetVolume(AValue: Integer);
begin
  if FVolume=AValue then Exit;
  FVolume:=AValue;
  Dirty:= true;
end;

constructor TEngineParam.Create(aOwner: TConfig);
begin
  fEngineSubParams:= TStringList.Create;
  inherited Create(aOwner);
end;

destructor TEngineParam.Destroy;
begin
  inherited Destroy;
  fEngineSubParams.Free;
end;

procedure TEngineParam.InternalSave;
begin
  owner.IniFile.WriteString('AudioEngine', 'Kind', EngineKind);
  owner.IniFile.WriteInteger('AudioEngine', 'Volume', Volume);
  owner.IniFile.WriteBool('AudioEngine', 'ActiveEQ', ActiveEQ);
  owner.IniFile.WriteInteger('AudioEngine', 'EQPreset', EQPreset);
  SaveSubParams;
end;

procedure TEngineParam.Load;
begin
  EngineKind := owner.IniFile.ReadString('AudioEngine', 'Kind', '');
  Volume := owner.IniFile.ReadInteger('AudioEngine', 'Volume', 50);
  ActiveEQ := owner.IniFile.ReadBool('AudioEngine', 'ActiveEQ', False);
  EQPreset := owner.IniFile.ReadInteger('AudioEngine', 'EQPreset', 0);
  ReadSubParams;
end;

procedure TEngineParam.ReadSubParams(AEngineKind:String='');
begin
  if AEngineKind = '' then
     AEngineKind := EngineKind;

  EngineSubParams.Clear;
  Owner.IniFile.ReadSectionValues('AudioEngine.'+ AEngineKind, EngineSubParams)
end;

procedure TEngineParam.SaveSubParams(AEngineKind:String='');
var
  Section:string;
  i :Integer;
begin
  if AEngineKind = '' then
     AEngineKind := EngineKind;

  Section:= 'AudioEngine.'+ AEngineKind;

  for i := 0 to EngineSubParams.Count -1 do
    begin
       owner.IniFile.WriteString(Section, EngineSubParams.Names[i], EngineSubParams.ValueFromIndex[i]);
    end;
end;



end.

