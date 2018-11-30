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
unit uConfig;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, ButtonPanel, ColorBox, Menus, Spin, EditBtn,
  GUIBackEnd, uOSD, AudioEngine, LazLoggerBase, ValEdit, BaseTypes, AppConsts, Grids;

type
    { TfConfig }

  TConfigPage = (cpNone, cpEngine,cpMediaLibrary, cpOSD, cpGUI);

  TOnConfigDone = procedure (Sender: TObject; Changed: boolean) of object;

  TfConfig = class(TForm)
    bAddDir: TButton;
    bRemoveDir: TButton;
    bRescanLibrary: TButton;
    bRestart: TButton;
    ButtonPanel: TButtonPanel;
    cbCaptureMMKeys: TCheckBox;
    cbEnableSoundMenu: TCheckBox;
    cbMinimizeOnClose: TCheckBox;
    cbRestart: TCheckBox;
    cbScanOnStart: TCheckBox;
    cbTrayVisible: TCheckBox;
    cbNetRemote: TCheckBox;
    colorBackground: TColorBox;
    ColorFont: TColorBox;
    EngineInfoView: TValueListEditor;
    EngineParamsEditor: TValueListEditor;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbWarning: TLabel;
    lbMLPath: TListBox;
    lbRestart: TLabel;
    FontDialog1: TFontDialog;
    GroupBox1:  TGroupBox;
    OpenDialog1: TOpenDialog;
    pnlOSDParams: TPanel;
    pnlNetwork: TPanel;
    pcConfig: TPageControl;
    pnlRestart: TPanel;
    rgAudioEngine: TRadioGroup;
    rgKeyCaptureMode: TRadioGroup;
    rgOSDKind: TRadioGroup;
    sbEngine:   TSpeedButton;
    sbInterface: TSpeedButton;
    sbNetRemote: TSpeedButton;
    sbNotification: TSpeedButton;
    sbLibrary:  TSpeedButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    sePort: TSpinEdit;
    tbTransparency: TTrackBar;
    tsEngine: TTabSheet;
    tsInterface: TTabSheet;
    tsMediaLibrary: TTabSheet;
    tsNetRemote: TTabSheet;
    tsOSD: TTabSheet;
    procedure bAddDirClick(Sender: TObject);
    procedure bRemoveDirClick(Sender: TObject);
    procedure bRescanLibraryClick(Sender: TObject);
    procedure bRestartClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure cbCaptureMMKeysClick(Sender: TObject);
    procedure cbNetRemoteChange(Sender: TObject);
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
    procedure sbNetRemoteClick(Sender: TObject);
    procedure sbNotificationClick(Sender: TObject);
    procedure sePortChange(Sender: TObject);
    procedure tbTransparencyChange(Sender: TObject);
    procedure tsEngineShow(Sender: TObject);
    procedure tsInterfaceShow(Sender: TObject);
    procedure tsMediaLibraryShow(Sender: TObject);
    procedure tsNetRemoteShow(Sender: TObject);
    procedure tsOSDHide(Sender: TObject);
    procedure tsOSDShow(Sender: TObject);
  private
    FSaved: boolean;
    FOnConfigDone: TOnConfigDone;
    OldEngine: Integer;

    procedure ConfigToMap;
    procedure SetOnConfigDone(AValue: TOnConfigDone);
  public
    procedure MapToConfig;
    destructor Destroy; override;
    Property OnConfigDone: TOnConfigDone read FOnConfigDone write SetOnConfigDone;
  end;



Procedure ShowConfigurationEditor(CallBack:TOnConfigDone=nil; Page:TConfigPage=cpNone);

implementation
{$R *.lfm}
uses udm, GeneralFunc, GuiConfig;
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
      GuiConfigObj.NotificationParam.X := fOsd.left;
      GuiConfigObj.NotificationParam.Y := fOsd.top;
      FreeAndNil(fOSD);
    end;
  FSaved:=true;
  MapToConfig;
  BackEnd.Config.SaveConfig;
  BackEnd.Config.Flush;
  Close;
end;

procedure TfConfig.rgAudioEngineClick(Sender: TObject);
var
  engineParams, EngineInfo: AREngineParams;
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

  if rgAudioEngine.ItemIndex <> OldEngine then
    begin
      if EngineArray[oldEngine].Engine.GetEngineParamsCount > 0 then
        begin
//mcmcmcmcmcmcmcmcmcm
//          BackEnd.Config.SaveSubParams(EngineArray[OldEngine].Engine.GetEngineName);
          SetLength(engineParams,0);
        end;

    end;

  OldEngine := rgAudioEngine.ItemIndex;

  if Engine.GetEngineParamsCount > 0 then
     begin
       EngineParams := Engine.GetEngineParams;
       EngineParamsEditor.Visible:=True;
       EngineParamsEditor.Clear;
   //mcmcmcmcmcmcmcmcmc
   //  BackEnd.Config.ReadSubParams(Engine.GetEngineName);
       for i := 0 to Length(engineParams) -1 do
         begin
           tmpValue :=BackEnd.EngineParam.EngineSubParams.Values[engineParams[i].Key];
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

  EngineInfo := Engine.GetEngineInfo(isCurrent);

  if Length(EngineInfo) > 0 then
     begin
       EngineInfoView.Visible:=True;
       EngineInfoView.Clear;
       for i := 0 to Length(EngineInfo) -1 do
         begin
           EngineInfoView.Values[EngineInfo[i].Key]:=EngineInfo[i].Value;
         end;
     end
  else
  EngineInfoView.Visible:=false;


end;

procedure TfConfig.rgOSDKindClick(Sender: TObject);
begin
  if rgOSDKind.ItemIndex = 2 then
     begin
   //     if Assigned(fOSD) then
          ShowOSDConfig;
          pnlOSDParams.Enabled := true;
     end
  else
    begin
      FreeAndNil(fOSD);
      pnlOSDParams.Enabled := False;
    end;
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
      GuiConfigObj.NotificationParam.X := fOsd.left;
      GuiConfigObj.NotificationParam.Y := fOsd.top;
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
  FSaved:=False;
  Close;
end;

procedure TfConfig.cbCaptureMMKeysClick(Sender: TObject);
begin
  rgKeyCaptureMode.Enabled := cbCaptureMMKeys.Checked;
end;

procedure TfConfig.cbNetRemoteChange(Sender: TObject);
begin
  pnlNetwork.Enabled:= cbNetRemote.Checked;
end;

procedure TfConfig.colorBackgroundChange(Sender: TObject);
begin
  GuiConfigObj.NotificationParam.BackColor := colorBackground.Selected;
  if Assigned(Fosd) and fOSD.Visible then
    fosd.UpdateAspect;
end;

procedure TfConfig.ColorFontChange(Sender: TObject);
begin
  GuiConfigObj.NotificationParam.FontColor := ColorFont.Selected;
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
  if Assigned(FOnConfigDone) then
    FOnConfigDone(Self, FSaved);
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
//var
//  i:Integer;
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
  pnlNetwork.Enabled:=cbNetRemote.Checked;

  {$IFNDEF NETWORK_INTF}
  sbNetRemote.Visible:=False;
  {$ENDIF}
end;

procedure TfConfig.sbLibraryClick(Sender: TObject);
begin
  pcConfig.ActivePage := tsMediaLibrary;
end;

procedure TfConfig.sbNetRemoteClick(Sender: TObject);
begin
  pcConfig.ActivePage := tsNetRemote;
end;

procedure TfConfig.sbNotificationClick(Sender: TObject);
begin
  pcConfig.ActivePage := tsOSD;
end;

procedure TfConfig.sePortChange(Sender: TObject);
begin
  if sePort.Value < 1024 then
     lbWarning.Caption:= rLowPortWarning
  else
     lbWarning.Caption:= '';
end;

procedure TfConfig.tbTransparencyChange(Sender: TObject);
begin
  GuiConfigObj.NotificationParam.Transparency := tbTransparency.Position;
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

procedure TfConfig.tsNetRemoteShow(Sender: TObject);
begin
  sbNetRemote.Down:=true;
end;

procedure TfConfig.tsOSDHide(Sender: TObject);
begin
  if Assigned(Fosd) and fOSD.Visible then
    begin
      GuiConfigObj.NotificationParam.X := fosd.left;
      GuiConfigObj.NotificationParam.Y := fosd.top;
      FreeAndNil(fOSD);
    end;
end;

procedure TfConfig.tsOSDShow(Sender: TObject);
begin
  sbNotification.Down:=true;
  if rgOSDKind.ItemIndex = 2 then
     ShowOSDConfig;
end;

procedure TfConfig.MapToConfig;
begin
  // MEDIA LIBRARY
  BackEnd.MediaLibraryParam.LibraryPaths.Assign(lbMLPath.Items);
  BackEnd.MediaLibraryParam.CheckOnStart   := cbScanOnStart.Checked;

  // NOTIFICATION
  GuiConfigObj.NotificationParam.Kind           := rgOSDKind.ItemIndex;
  GuiConfigObj.NotificationParam.BackColor      := ColorBackground.Selected;
  GuiConfigObj.NotificationParam.FontColor      := ColorFont.Selected;
  GuiConfigObj.NotificationParam.Transparency   := tbTransparency.Position;

  // INTERFACE
  GuiConfigObj.InterfaceParam.MinimizeOnClose   := cbMinimizeOnClose.checked;
  GuiConfigObj.InterfaceParam.ShowTrayIcon      := cbTrayVisible.checked;
  GuiConfigObj.InterfaceParam.CaptureMMKeys     := cbCaptureMMKeys.checked;
  GuiConfigObj.InterfaceParam.CaptureMMkeysMode := rgKeyCaptureMode.ItemIndex;
  GuiConfigObj.InterfaceParam.EnableSoundMenu   := cbEnableSoundMenu.Checked;

  // PLAYLIST
  BackEnd.PlayListParam.Restart            := cbRestart.Checked;

  // ENGINE
  BackEnd.EngineParam.EngineKind           := rgAudioEngine.Items[rgAudioEngine.ItemIndex];

  // NETREMOTE
  {$IFDEF NETWORK_INTF}
  GuiConfigObj.NetRemoteParam.Enabled           := cbNetRemote.checked;
  GuiConfigObj.NetRemoteParam.Port              := sePort.Value;
  {$ENDIF NETWORK_INTF}

  //GENERAL
  if EngineParamsEditor.Visible then
    BackEnd.EngineParam.EngineSubParams.Assign(EngineParamsEditor.Strings)
  else
    BackEnd.EngineParam.EngineSubParams.Clear;
end;

destructor TfConfig.Destroy;
begin
  inherited Destroy;
  fConfig := nil;
end;

procedure TfConfig.ConfigToMap;
begin
  // MEDIA LIBRARY
  lbMLPath.Items.Assign(BackEnd.MediaLibraryParam.LibraryPaths);
  cbScanOnStart.Checked      := BackEnd.MediaLibraryParam.CheckOnStart;

  // NOTIFICATION
  rgOSDKind.ItemIndex        := GuiConfigObj.NotificationParam.Kind;
  ColorBackground.Selected   := GuiConfigObj.NotificationParam.BackColor;
  ColorFont.Selected         := GuiConfigObj.NotificationParam.FontColor;
  tbTransparency.Position    := GuiConfigObj.NotificationParam.Transparency;

  // INTERFACE
  cbMinimizeOnClose.checked  := GuiConfigObj.InterfaceParam.MinimizeOnClose;
  cbTrayVisible.checked      := GuiConfigObj.InterfaceParam.ShowTrayIcon;
  cbCaptureMMKeys.checked    := GuiConfigObj.InterfaceParam.CaptureMMKeys;
  rgKeyCaptureMode.ItemIndex := GuiConfigObj.InterfaceParam.CaptureMMkeysMode;
  cbEnableSoundMenu.Checked  := GuiConfigObj.InterfaceParam.EnableSoundMenu;

  // PLAYLIST
  cbRestart.Checked          := BackEnd.PlayListParam.Restart;

  // ENGINE
  OldEngine:= rgAudioEngine.Items.IndexOf(Backend.EngineParam.EngineKind);
  rgAudioEngine.ItemIndex    := OldEngine;


  if EngineParamsEditor.Visible then
    EngineParamsEditor.Strings.Assign(BackEnd.EngineParam.EngineSubParams);

  {$IFDEF NETWORK_INTF}
  // NETREMOTE
  cbNetRemote.Checked       := GuiConfigObj.NetRemoteParam.Enabled;
  sePort.Value              := GuiConfigObj.NetRemoteParam.Port;
  {$ENDIF NETWORK_INTF}
  //GENERAL
  pnlRestart.visible := Backend.Config.NeedRestart;
end;

procedure TfConfig.SetOnConfigDone(AValue: TOnConfigDone);
begin
  if FOnConfigDone=AValue then Exit;
  FOnConfigDone:=AValue;
end;

Procedure ShowConfigurationEditor(CallBack:TOnConfigDone=nil; Page:TConfigPage=cpNone);
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
  fConfig.OnConfigDone:= CallBack;
end;

Initialization
 fConfig := Nil;

end.
