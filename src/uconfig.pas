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
unit uConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, ButtonPanel, ColorBox, Menus, Spin, EditBtn,
  GUIBackEnd, uOSD, AudioEngine, LCLProc, ValEdit, BaseTypes, Grids;

type
    { TfConfig }

  TConfigPage = (cpNone, cpEngine,cpMediaLibrary, cpOSD, cpGUI);

  TfConfig = class(TForm)
    bAddDir:    TButton;
    bRemoveDir: TButton;
    bRescanLibrary: TButton;
    bRestart: TButton;
    ButtonPanel: TButtonPanel;
    cbCaptureMMKeys: TCheckBox;
    cbEnableSoundMenu: TCheckBox;
    cbScanOnStart: TCheckBox;
    cbTrayVisible: TCheckBox;
    cbMinimizeOnClose: TCheckBox;
    cbRestart: TCheckBox;
    colorBackground: TColorBox;
    ColorFont:  TColorBox;
    EngineInfo: TValueListEditor;
    lbRestart: TLabel;
    FontDialog1: TFontDialog;
    GroupBox1:  TGroupBox;
    Label1:     TLabel;
    Label2:     TLabel;
    Label3:     TLabel;
    Label4:     TLabel;
    Label5: TLabel;
    lbMLPath:   TListBox;
    OpenDialog1: TOpenDialog;
    pnlRestart: TPanel;
    pcConfig:   TPageControl;
    rgKeyCaptureMode: TRadioGroup;
    rgOSDKind:  TRadioGroup;
    rgAudioEngine: TRadioGroup;
    sbEngine:   TSpeedButton;
    sbInterface: TSpeedButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    seLimit: TSpinEdit;
    sbNotification: TSpeedButton;
    sbPlayList: TSpeedButton;
    sbLibrary:  TSpeedButton;
    tsInterface: TTabSheet;
    tbTransparency: TTrackBar;
    tsOSD:      TTabSheet;
    tsEngine:   TTabSheet;
    tsMediaLibrary: TTabSheet;
    tsPlaylist: TTabSheet;
    EngineParamsEditor: TValueListEditor;
    procedure bAddDirClick(Sender: TObject);
    procedure bRemoveDirClick(Sender: TObject);
    procedure bRescanLibraryClick(Sender: TObject);
    procedure bRestartClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure cbCaptureMMKeysClick(Sender: TObject);
    procedure colorBackgroundChange(Sender: TObject);
    procedure ColorFontChange(Sender: TObject);
    procedure EngineParamsEditorButtonClick(Sender: TObject; aCol, aRow: Integer
      );
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure rgAudioEngineClick(Sender: TObject);
    procedure rgOSDKindClick(Sender: TObject);
    procedure sbEngineClick(Sender: TObject);
    procedure sbInterfaceClick(Sender: TObject);
    procedure sbLibraryClick(Sender: TObject);
    procedure sbPlayListClick(Sender: TObject);
    procedure sbNotificationClick(Sender: TObject);
    procedure tbTransparencyChange(Sender: TObject);
    procedure tsEngineShow(Sender: TObject);
    procedure tsInterfaceShow(Sender: TObject);
    procedure tsMediaLibraryShow(Sender: TObject);
    procedure tsOSDHide(Sender: TObject);
    procedure tsOSDShow(Sender: TObject);
    procedure tsPlaylistShow(Sender: TObject);
  private
    procedure ConfigToMap;
  public
    procedure MapToConfig;
    destructor Destroy; override;
  end;



Procedure ShowConfigurationEditor(Page:TConfigPage=cpNone);

implementation
{$R *.lfm}
uses udm, GeneralFunc;
var
  fConfig: TfConfig;

{ TfConfig }

procedure TfConfig.sbEngineClick(Sender: TObject);
begin
  pcConfig.ActivePage := tsEngine;
end;

procedure TfConfig.sbInterfaceClick(Sender: TObject);
begin
    pcConfig.ActivePage := tsInterface;
end;

procedure TfConfig.OKButtonClick(Sender: TObject);
begin
  if Assigned(FOSD) and fOSD.Visible then
    begin
      BackEnd.Config.NotificationParam.X := fOsd.left;
      BackEnd.Config.NotificationParam.Y := fOsd.top;
      FreeAndNil(fOSD);
    end;

  MapToConfig;
  BackEnd.Config.SaveConfig;
  BackEnd.Config.Flush;
  Close;
end;

procedure TfConfig.rgAudioEngineClick(Sender: TObject);
var
  engineParams: AREngineParams;
  Engine : TAudioEngineClass;
  i: integer;
  isCurrent: boolean;
  tmpValue: string;
begin
  if rgAudioEngine.Items[rgAudioEngine.ItemIndex] <> BackEnd.AudioEngine.GetEngineName then
     begin
       isCurrent:= False;
       BackEnd.Config.NeedRestart:= true;
       pnlRestart.Visible:= true;
     end
  else
    begin
      isCurrent:= True;
      BackEnd.Config.NeedRestart:= false;
      pnlRestart.Visible:= false;
    end;
  Engine := EngineArray[rgAudioEngine.ItemIndex].Engine;
  EngineParams := Engine.GetEngineParams;

  if Length(engineParams) > 0 then
     begin
       EngineParamsEditor.Visible:=True;
       EngineParamsEditor.Clear;
       BackEnd.Config.ReadSubParams(Engine.GetEngineName);
       for i := 0 to Length(engineParams) -1 do
         begin
           tmpValue :=BackEnd.Config.EngineSubParams.Values[engineParams[i].Key];
           if tmpValue = '' then
             tmpValue:=engineParams[i].Value;
           EngineParamsEditor.Values[engineParams[i].Key]:=tmpValue;
           if engineParams[i].Kind = epkFileName then
             EngineParamsEditor.ItemProps[engineParams[i].Key].EditStyle:=esEllipsis;

        // very dirty hack, shame on me
           EngineParamsEditor.Strings.Objects[i]:=TObject(IntPtr(ord(EngineParams[i].Kind)));
         end;
     end
  else
  EngineParamsEditor.Visible:=false;

  EngineParams := Engine.GetEngineInfo(isCurrent);

  if Length(engineParams) > 0 then
     begin
       EngineInfo.Visible:=True;
       EngineInfo.Clear;
       for i := 0 to Length(engineParams) -1 do
         begin
           EngineInfo.Values[engineParams[i].Key]:=engineParams[i].Value;
         end;
     end
  else
  EngineInfo.Visible:=false;


end;

procedure TfConfig.rgOSDKindClick(Sender: TObject);
begin
  if rgOSDKind.ItemIndex = 2 then
     begin
        if Assigned(fOSD) then
          ShowOSDConfig;
     end
  else
     FreeAndNil(fOSD);
end;

procedure TfConfig.bAddDirClick(Sender: TObject);
begin
  if dm.SelectDirectoryDialog.Execute then
    lbMLPath.Items.Add(dm.SelectDirectoryDialog.FileName);
end;

procedure TfConfig.bRemoveDirClick(Sender: TObject);
begin
  if lbMLPath.ItemIndex > -1 then
     lbMLPath.Items.Delete(lbMLPath.ItemIndex);
end;

procedure TfConfig.bRescanLibraryClick(Sender: TObject);
begin
//  BackEnd.MediaLibrary.Scan(BackEnd.Config.MediaLibraryParam.LibraryPaths);
    BackEnd.MediaLibrary.Scan(lbMLPath.Items);
end;

procedure TfConfig.bRestartClick(Sender: TObject);
begin
  if Assigned(FOSD) and fOSD.Visible then
    begin
      BackEnd.Config.NotificationParam.X := fOsd.left;
      BackEnd.Config.NotificationParam.Y := fOsd.top;
      FreeAndNil(fOSD);
    end;

  MapToConfig;
  BackEnd.Config.SaveConfig;
  BackEnd.Config.Flush;
  Restart(Application);
end;

procedure TfConfig.CancelButtonClick(Sender: TObject);
begin
  if Assigned(Fosd) and fOSD.Visible then
    begin
      FreeAndNil(fOSD);
    end;
  Close;
end;

procedure TfConfig.cbCaptureMMKeysClick(Sender: TObject);
begin
  rgKeyCaptureMode.Enabled := cbCaptureMMKeys.Checked;
end;

procedure TfConfig.colorBackgroundChange(Sender: TObject);
begin
  BackEnd.Config.NotificationParam.BackColor := colorBackground.Selected;
  if Assigned(Fosd) and fOSD.Visible then
    fosd.UpdateAspect;
end;

procedure TfConfig.ColorFontChange(Sender: TObject);
begin
  BackEnd.Config.NotificationParam.FontColor := ColorFont.Selected;
  if Assigned(Fosd) and fOSD.Visible then
    fosd.UpdateAspect;

end;

procedure TfConfig.EngineParamsEditorButtonClick(Sender: TObject; aCol,
  aRow: Integer);
begin

 if  EngineParamKind(PtrInt(EngineParamsEditor.Strings.Objects[Arow-1])) = epkFileName  then
     if OpenDialog1.Execute then
       EngineParamsEditor.Cells[aCol, aRow] := OpenDialog1.FileName;
end;

procedure TfConfig.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TfConfig.FormCreate(Sender: TObject);
var
  i,j: integer;
  tmpstr: string;
begin
  rgAudioEngine.Items.clear;
  tmpstr := '';
  for i:= low(EngineArray) to High(EngineArray) do
    begin
      tmpstr:= tmpstr + EngineArray[i].Name + sLineBreak;
    end;
  rgAudioEngine.Items.Text:=tmpstr;
  j:= -1;
  for i:= 0 to rgAudioEngine.items.count -1 do
    begin
       if not(rgAudioEngine.Controls[i] is TRadioButton) or not rgAudioEngine.Controls[i].Visible then
          continue;
      inc(j);

      if EngineArray[j].ForceSelection then
         TRadioButton(rgAudioEngine.Controls[i]).Enabled:=true
      else
         TRadioButton(rgAudioEngine.Controls[i]).Enabled := EngineArray[j].Engine.IsAvalaible(nil);
      end;

end;


procedure TfConfig.FormShow(Sender: TObject);
var
  i:Integer;
begin
{  for i := 0 to ComponentCount -1 do
     if Components[i] is TLabel then
       if Tlabel (Components[i]).OptimalFill then
          Tlabel (Components[i]).AdjustFontForOptimalFill;
 }
  pcConfig.ShowTabs:=false;
//  sbInterface.click;
  ConfigToMap;
  {$IFDEF ASKMMKEYSMODE}
    rgKeyCaptureMode.Visible:=True;
  {$ELSE}
    rgKeyCaptureMode.Visible:=False;
  {$ENDIF ASKMMKEYSMODE}

end;


procedure TfConfig.sbLibraryClick(Sender: TObject);
begin
  pcConfig.ActivePage := tsMediaLibrary;
end;

procedure TfConfig.sbPlayListClick(Sender: TObject);
begin
  pcConfig.ActivePage := tsPlaylist;
end;

procedure TfConfig.sbNotificationClick(Sender: TObject);
begin
  pcConfig.ActivePage := tsOSD;
end;

procedure TfConfig.tbTransparencyChange(Sender: TObject);
begin
  BackEnd.Config.NotificationParam.Transparency := tbTransparency.Position;
  if Assigned(Fosd) and fOSD.Visible then
    fosd.UpdateAspect;

end;

procedure TfConfig.tsEngineShow(Sender: TObject);
begin
  sbEngine.Down:=true;
end;

procedure TfConfig.tsInterfaceShow(Sender: TObject);
begin
  sbInterface.Down:=true;
end;

procedure TfConfig.tsMediaLibraryShow(Sender: TObject);
begin
  sbLibrary.Down:=true;
end;

procedure TfConfig.tsOSDHide(Sender: TObject);
begin
  if Assigned(Fosd) and fOSD.Visible then
    begin
      BackEnd.Config.NotificationParam.X := fosd.left;
      BackEnd.Config.NotificationParam.Y := fosd.top;
      FreeAndNil(fOSD);
    end;
end;

procedure TfConfig.tsOSDShow(Sender: TObject);
begin
  sbNotification.Down:=true;
  if rgOSDKind.ItemIndex = 2 then
     ShowOSDConfig;
end;

procedure TfConfig.tsPlaylistShow(Sender: TObject);
begin
  sbPlayList.Down:=true;
end;

procedure TfConfig.MapToConfig;
begin
  // MEDIA LIBRARY
  BackEnd.Config.MediaLibraryParam.LibraryPaths.Assign(lbMLPath.Items);
  BackEnd.Config.MediaLibraryParam.CheckOnStart := cbScanOnStart.Checked;

  // NOTIFICATION
  BackEnd.Config.NotificationParam.Kind           := rgOSDKind.ItemIndex;
  BackEnd.Config.NotificationParam.BackColor      := ColorBackground.Selected;
  BackEnd.Config.NotificationParam.FontColor      := ColorFont.Selected;
  BackEnd.Config.NotificationParam.Transparency   := tbTransparency.Position;

  // INTERFACE
  BackEnd.Config.InterfaceParam.MinimizeOnClose   := cbMinimizeOnClose.checked;
  BackEnd.Config.InterfaceParam.ShowTrayIcon      := cbTrayVisible.checked;
  BackEnd.Config.InterfaceParam.CaptureMMKeys     := cbCaptureMMKeys.checked;
  BackEnd.Config.InterfaceParam.CaptureMMkeysMode := rgKeyCaptureMode.ItemIndex;
  BackEnd.Config.InterfaceParam.EnableSoundMenu   := cbEnableSoundMenu.Checked;

  // PLAYLIST
  BackEnd.Config.PlayListParam.LimitTrack         := seLimit.Value;
  BackEnd.Config.PlayListParam.Restart            := cbRestart.Checked;

  // ENGINE
  BackEnd.Config.EngineParam.EngineKind           := rgAudioEngine.Items[rgAudioEngine.ItemIndex];

  //GENERAL
  if EngineParamsEditor.Visible then
    BackEnd.Config.EngineSubParams.Assign(EngineParamsEditor.Strings);
end;

destructor TfConfig.Destroy;
begin
  inherited Destroy;
  fConfig := nil;
end;

procedure TfConfig.ConfigToMap;
begin
  // MEDIA LIBRARY
  lbMLPath.Items.Assign(BackEnd.Config.MediaLibraryParam.LibraryPaths);
  cbScanOnStart.Checked     := BackEnd.Config.MediaLibraryParam.CheckOnStart;

  // NOTIFICATION
  rgOSDKind.ItemIndex       := BackEnd.Config.NotificationParam.Kind;
  ColorBackground.Selected  := BackEnd.Config.NotificationParam.BackColor;
  ColorFont.Selected        := BackEnd.Config.NotificationParam.FontColor;
  tbTransparency.Position   := BackEnd.Config.NotificationParam.Transparency;

  // INTERFACE
  cbMinimizeOnClose.checked  := BackEnd.Config.InterfaceParam.MinimizeOnClose;
  cbTrayVisible.checked      := BackEnd.Config.InterfaceParam.ShowTrayIcon;
  cbCaptureMMKeys.checked    := BackEnd.Config.InterfaceParam.CaptureMMKeys;
  rgKeyCaptureMode.ItemIndex := BackEnd.Config.InterfaceParam.CaptureMMkeysMode;
  cbEnableSoundMenu.Checked  := BackEnd.Config.InterfaceParam.EnableSoundMenu;

  // PLAYLIST
  seLimit.Value             := BackEnd.Config.PlayListParam.LimitTrack;
  cbRestart.Checked         := BackEnd.Config.PlayListParam.Restart;

  // ENGINE
  rgAudioEngine.ItemIndex   := rgAudioEngine.Items.IndexOf(Backend.Config.EngineParam.EngineKind);

  if EngineParamsEditor.Visible then
    EngineParamsEditor.Strings.Assign(BackEnd.Config.EngineSubParams);

  //GENERAL
  pnlRestart.visible := Backend.Config.NeedRestart;
end;

Procedure ShowConfigurationEditor(Page:TConfigPage=cpNone);
begin
  if not Assigned(fConfig) then
    fConfig := TfConfig.Create(Application);
  case page of
       cpNone: ;
       cpEngine: fConfig.pcConfig.ActivePage := fConfig.tsEngine;
       cpMediaLibrary: fConfig.pcConfig.ActivePage := fConfig.tsMediaLibrary;
       cpOSD: fConfig.pcConfig.ActivePage := fConfig.tsOSD;
       cpGUI: fConfig.pcConfig.ActivePage := fConfig.tsInterface;
  end;

  fConfig.Show;
end;

Initialization
 fConfig := Nil;

end.
