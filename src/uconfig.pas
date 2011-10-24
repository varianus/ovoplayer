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
  GUIBackEnd, uOSD;

type

  { TfConfig }

  TfConfig = class(TForm)
    bAddDir:    TButton;
    bRemoveDir: TButton;
    bRescanLibrary: TButton;
    ButtonPanel: TButtonPanel;
    cbScanOnStart: TCheckBox;
    cbTrayVisible: TCheckBox;
    cbMinimizeOnClose: TCheckBox;
    cbRestart: TCheckBox;
    colorBackground: TColorBox;
    ColorFont:  TColorBox;
    MPlayerPath: TFileNameEdit;
    FontDialog1: TFontDialog;
    GroupBox1:  TGroupBox;
    Label1:     TLabel;
    Label2:     TLabel;
    Label3:     TLabel;
    Label4:     TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbMLPath:   TListBox;
    PageControl1: TPageControl;
    pcConfig:   TPageControl;
    rgOSDKind:  TRadioGroup;
    rgAudioEngine: TRadioGroup;
    sbEngine:   TSpeedButton;
    sbInterface: TSpeedButton;
    seLimit: TSpinEdit;
    SpeedButton2: TSpeedButton;
    sbPlayList: TSpeedButton;
    sbLibrary:  TSpeedButton;
    tsNone: TTabSheet;
    tsMPlayer: TTabSheet;
    tsInterface: TTabSheet;
    tbTransparency: TTrackBar;
    tsOSD:      TTabSheet;
    tsEngine:   TTabSheet;
    tsMediaLibrary: TTabSheet;
    tsPlaylist: TTabSheet;
    procedure bAddDirClick(Sender: TObject);
    procedure bRescanLibraryClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure colorBackgroundChange(Sender: TObject);
    procedure ColorFontChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure rgOSDKindClick(Sender: TObject);
    procedure sbEngineClick(Sender: TObject);
    procedure sbInterfaceClick(Sender: TObject);
    procedure sbLibraryClick(Sender: TObject);
    procedure sbPlayListClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure tbTransparencyChange(Sender: TObject);
    procedure tsOSDHide(Sender: TObject);
    procedure tsOSDShow(Sender: TObject);
  private
    procedure ConfigToMap;
  public
    procedure MapToConfig;
  end;

var
  fConfig: TfConfig;

implementation
uses audioengine_vlc, AudioEngine_MPlayer, audioengine_Xine, audioengine_bass;
{$R *.lfm}

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
    fOSD.Close;

    end;

  MapToConfig;
  BackEnd.Config.SaveConfig;
  BackEnd.Config.Flush;
  Close;
end;

procedure TfConfig.rgOSDKindClick(Sender: TObject);
begin
  if rgOSDKind.ItemIndex = 2 then
     begin
        if fOSD <> nil then
          ShowOSDConfig;
     end
  else
    fOSD.free;
end;

procedure TfConfig.bAddDirClick(Sender: TObject);
begin
  if BackEnd.SelectDirectoryDialog.Execute then
    lbMLPath.Items.Add(BackEnd.SelectDirectoryDialog.FileName);
end;

procedure TfConfig.bRescanLibraryClick(Sender: TObject);
begin
  BackEnd.MediaLibrary.Scan(BackEnd.Config.MediaLibraryParam.LibraryPaths);
end;

procedure TfConfig.Button1Click(Sender: TObject);
begin
  if FontDialog1.Execute then
    begin

    end;
end;

procedure TfConfig.CancelButtonClick(Sender: TObject);
begin
  if Assigned(Fosd) and fOSD.Visible then
    begin
      fOSD.Close;
    end;
  Close;
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

procedure TfConfig.FormCreate(Sender: TObject);
begin
  TRadioButton(rgAudioEngine.Controls[0]).Enabled := TAudioEngineVLC.IsAvalaible(nil);
  TRadioButton(rgAudioEngine.Controls[1]).Enabled := TAudioEngineMPlayer.IsAvalaible(nil);
  TRadioButton(rgAudioEngine.Controls[2]).Enabled := TAudioEngineXINE.IsAvalaible(nil);
  TRadioButton(rgAudioEngine.Controls[3]).Enabled := TAudioEngineBASS.IsAvalaible(nil);
end;

procedure TfConfig.FormShow(Sender: TObject);
var
  i:Integer;
begin
  for i := 0 to ComponentCount -1 do
     if Components[i] is TLabel then
       if Tlabel (Components[i]).OptimalFill then
          Tlabel (Components[i]).AdjustFontForOptimalFill;

  pcConfig.ShowTabs:=false;
  sbInterface.click;
  ConfigToMap;

end;

procedure TfConfig.sbLibraryClick(Sender: TObject);
begin
  pcConfig.ActivePage := tsMediaLibrary;
end;

procedure TfConfig.sbPlayListClick(Sender: TObject);
begin
  pcConfig.ActivePage := tsPlaylist;
end;

procedure TfConfig.SpeedButton2Click(Sender: TObject);
begin
  pcConfig.ActivePage := tsOSD;
end;

procedure TfConfig.tbTransparencyChange(Sender: TObject);
begin
  BackEnd.Config.NotificationParam.Transparency := tbTransparency.Position;
  if Assigned(Fosd) and fOSD.Visible then
    fosd.UpdateAspect;

end;

procedure TfConfig.tsOSDHide(Sender: TObject);
begin
  if Assigned(Fosd) and fOSD.Visible then
    begin
      BackEnd.Config.NotificationParam.X := fosd.left;
      BackEnd.Config.NotificationParam.Y := fosd.top;
      fOSD.Close;
    end;
end;

procedure TfConfig.tsOSDShow(Sender: TObject);
begin
  if rgOSDKind.ItemIndex = 2 then
     ShowOSDConfig;
end;

procedure TfConfig.MapToConfig;
begin
  // MEDIA LIBRARY
  BackEnd.Config.MediaLibraryParam.LibraryPaths.Assign(lbMLPath.Items);
  BackEnd.Config.MediaLibraryParam.CheckOnStart := cbScanOnStart.Checked;

  // NOTIFICATION
  BackEnd.Config.NotificationParam.Kind         := rgOSDKind.ItemIndex;
  BackEnd.Config.NotificationParam.BackColor    := ColorBackground.Selected;
  BackEnd.Config.NotificationParam.FontColor    := ColorFont.Selected;
  BackEnd.Config.NotificationParam.Transparency := tbTransparency.Position;

  // INTERFACE
  BackEnd.Config.InterfaceParam.MinimizeOnClose := cbMinimizeOnClose.checked;
  BackEnd.Config.InterfaceParam.ShowTrayIcon    := cbTrayVisible.checked;

  // PLAYLIST
  BackEnd.Config.PlayListParam.LimitTrack       := seLimit.Value;
  BackEnd.Config.PlayListParam.Restart          := cbRestart.Checked;

  // ENGINE
  BackEnd.Config.EngineParam.EngineKind         := rgAudioEngine.ItemIndex;

  //GENERAL
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
  cbMinimizeOnClose.checked := BackEnd.Config.InterfaceParam.MinimizeOnClose;
  cbTrayVisible.checked := BackEnd.Config.InterfaceParam.ShowTrayIcon;

  // PLAYLIST
  seLimit.Value             := BackEnd.Config.PlayListParam.LimitTrack;
  cbRestart.Checked         := BackEnd.Config.PlayListParam.Restart;

  // ENGINE
  rgAudioEngine.ItemIndex   := BackEnd.Config.EngineParam.EngineKind;

  //GENERAL
end;

end.
