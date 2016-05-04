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
unit GuiConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Config, Graphics;

const
  npkOSD = 2;
  npkNotifications = 1;

type

  TNotificationParam = record
    Kind: integer;
    X: integer;
    Y: integer;
    BackColor: TColor;
    FontColor: TColor;
    Transparency: integer;
    TimeOut: integer;
  end;

  TInterfaceParam = record
    ShowTrayIcon: boolean;
    MinimizeOnClose: boolean;
    GroupBy: Integer;
    CaptureMMKeys: boolean;
    CaptureMMkeysMode: Integer;
    EnableSoundMenu: boolean;
  end;


 {$IFDEF NETWORK_INTF}
  TNetRemoteParam = record
    Enabled: boolean;
    Port: integer;
  end;
 {$EndIf}


  { TGuiConfig }

  TGuiConfig = class
  private
    FConfig: TConfig;
  public
    NotificationParam: TNotificationParam;
    InterfaceParam:    TInterfaceParam;
   {$IFDEF NETWORK_INTF}
    NetRemoteParam: TNetRemoteParam;
   {$ENDIF}

    constructor Create(Config: TConfig);
    procedure ReadConfig;
    procedure SaveConfig;

  end;
var

 GuiConfigObj : TGuiConfig;

implementation


{ TGuiConfig }

constructor TGuiConfig.Create(Config: TConfig);
begin
  FConfig := Config;
end;

procedure TGuiConfig.ReadConfig;
var
  TmpSt: TStringList;
begin

  TmpSt := TStringList.Create;
  FConfig.ReadCustomParams('Notification', TmpSt);
  // NOTIFICATION
  NotificationParam.Kind := StrToIntDef(TmpSt.Values['Kind'], 2);
  NotificationParam.BackColor := StringToColorDef(TmpSt.Values['BackColor'], $00B66F18);
  NotificationParam.FontColor := StringToColorDef(TmpSt.Values['FontColor'], $00000000);
  NotificationParam.TimeOut := StrToIntDef(TmpSt.Values['TimeOut'], 3000);
  NotificationParam.X := StrToIntDef(TmpSt.Values['X'], 100);
  NotificationParam.Y := StrToIntDef(TmpSt.Values['Y'], 100);
  NotificationParam.Transparency := StrToIntDef(TmpSt.Values['Transparency'], 230);

  // INTERFACE
  FConfig.ReadCustomParams('Interface', TmpSt);
  InterfaceParam.MinimizeOnClose := StrToBoolDef(TmpSt.Values['MinimizeOnClose'], True);
  InterfaceParam.ShowTrayIcon := StrToBoolDef(TmpSt.Values['ShowTrayIcon'], True);
  InterfaceParam.GroupBy := StrToIntDef(TmpSt.Values['GroupBy'], 0);
  InterfaceParam.CaptureMMKeys := StrToBoolDef(TmpSt.Values['CaptureMMKeys'], False);
  InterfaceParam.CaptureMMkeysMode := StrToIntDef(TmpSt.Values['CaptureMMkeysMode'], 0);
  InterfaceParam.EnableSoundMenu := StrToBoolDef(TmpSt.Values['EnableSoundMenu'], True);

  // NETREMOTE
  {$IFDEF NETWORK_INTF}
  FConfig.ReadCustomParams('NetRemote', TmpSt);

  NetRemoteParam.Enabled := StrToBoolDef(TmpSt.Values['Enabled'], False);
  NetRemoteParam.Port :=StrToIntDef(TmpSt.Values['Port'], 6860);
  {$ENDIF}


end;

procedure TGuiConfig.SaveConfig;
var
  TmpSt: TStringList;
begin

  TmpSt := TStringList.Create;
  // NOTIFICATION
  TmpSt.Values['Kind'] := IntToStr(NotificationParam.Kind);
  TmpSt.Values['BackColor'] := ColorToString(NotificationParam.BackColor);
  TmpSt.Values['FontColor'] := ColorToString(NotificationParam.FontColor);
  TmpSt.Values['TimeOut'] := IntToStr(NotificationParam.TimeOut);
  TmpSt.Values['X'] := IntToStr(NotificationParam.X);
  TmpSt.Values['Y'] := IntToStr(NotificationParam.Y);
  TmpSt.Values['Transparency'] := IntToStr(NotificationParam.Transparency);
  fConfig.SaveCustomParams('Notification', tmpSt);

  // INTERFACE
  TmpSt.Clear;
  TmpSt.Values['MinimizeOnClose'] := BoolToStr(InterfaceParam.MinimizeOnClose);
  TmpSt.Values['ShowTrayIcon'] := BoolToStr(InterfaceParam.ShowTrayIcon);
  TmpSt.Values['GroupBy'] := IntToStr(InterfaceParam.GroupBy);
  TmpSt.Values['CaptureMMKeys'] := BoolToStr(InterfaceParam.CaptureMMKeys);
  TmpSt.Values['CaptureMMKeysMode'] := IntToStr(InterfaceParam.CaptureMMkeysMode);
  TmpSt.Values['EnableSoundMenu'] := BoolToStr(InterfaceParam.EnableSoundMenu);
  fConfig.SaveCustomParams('Notification', tmpSt);

  {$IFDEF NETWORK_INTF}
  // NETREMOTE
  TmpSt.Clear;
  TmpSt.Values['Enabled']:= BoolToStr(NetRemoteParam.Enabled);
  TmpSt.Values['Port'] := IntToStr( NetRemoteParam.Port);
  {$ENDIf}

  fConfig.SaveCustomParams('NetRemote', tmpSt);

  TmpSt.Free;
end;

end.
