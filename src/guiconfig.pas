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
unit GuiConfig;

interface

uses
  Classes, SysUtils, Config, Graphics;

const
  npkOSD = 2;
  npkNotifications = 1;

type

  { TNotificationParam }

  TNotificationParam = Class(TConfigParam)
  private
    FBackColor: TColor;
    FFontColor: TColor;
    FKind: integer;
    FTimeOut: integer;
    FTransparency: integer;
    FX: integer;
    FY: integer;
    procedure SetBackColor(AValue: TColor);
    procedure SetFontColor(AValue: TColor);
    procedure SetKind(AValue: integer);
    procedure SetTimeOut(AValue: integer);
    procedure SetTransparency(AValue: integer);
    procedure SetX(AValue: integer);
    procedure SetY(AValue: integer);
  protected
    Procedure InternalSave; override;
  public
    Property Kind: integer read FKind write SetKind;
    Property X: integer read FX write SetX;
    Property Y: integer read FY write SetY;
    Property BackColor: TColor read FBackColor write SetBackColor;
    Property FontColor: TColor read FFontColor write SetFontColor;
    Property Transparency: integer read FTransparency write SetTransparency;
    Property TimeOut: integer read FTimeOut write SetTimeOut;
    Procedure Load; override;
  end;

  { TInterfaceParam }

  TInterfaceParam = Class(TConfigParam)
  private
    FCaptureMMKeys: boolean;
    FCaptureMMkeysMode: Integer;
    FEnableSoundMenu: boolean;
    FGroupBy: Integer;
    FMinimizeOnClose: boolean;
    FPauseWhenLocked: boolean;
    FShowTrayIcon: boolean;
    procedure SetCaptureMMKeys(AValue: boolean);
    procedure SetCaptureMMkeysMode(AValue: Integer);
    procedure SetEnableSoundMenu(AValue: boolean);
    procedure SetGroupBy(AValue: Integer);
    procedure SetMinimizeOnClose(AValue: boolean);
    procedure SetPauseWhenLocked(AValue: boolean);
    procedure SetShowTrayIcon(AValue: boolean);
  protected
    Procedure InternalSave; override;
  public
    Property ShowTrayIcon: boolean read FShowTrayIcon write SetShowTrayIcon;
    Property MinimizeOnClose: boolean read FMinimizeOnClose write SetMinimizeOnClose;
    Property GroupBy: Integer read FGroupBy write SetGroupBy;
    Property CaptureMMKeys: boolean read FCaptureMMKeys write SetCaptureMMKeys;
    Property CaptureMMkeysMode: Integer read FCaptureMMkeysMode write SetCaptureMMkeysMode;
    Property EnableSoundMenu: boolean read FEnableSoundMenu write SetEnableSoundMenu;
    Property PauseWhenLocked: boolean read FPauseWhenLocked write SetPauseWhenLocked;
    Procedure Load; override;

  end;


 {$IFDEF NETWORK_INTF}
  { TNetRemoteParam }
  TNetRemoteParam = Class(TConfigParam)
  private
    FEnabled: boolean;
    FOnlyLAN: boolean;
    FPort: integer;
    procedure SetEnabled(AValue: boolean);
    procedure SetOnlyLAN(AValue: boolean);
    procedure SetPort(AValue: integer);
  protected
    Procedure InternalSave; override;
  public
    Property Enabled: boolean read FEnabled write SetEnabled;
    Property OnlyLAN: boolean read FOnlyLAN write SetOnlyLAN;
    Property Port: integer read FPort write SetPort;
    Procedure Load; override;
  end;
 {$EndIf}

 { TGuiConfig }

 TGuiConfig = class
 private
   fNotificationParam: TNotificationParam;
   FInterfaceParam: TInterfaceParam;
   {$IFDEF NETWORK_INTF}
   fNetRemoteParam: TNetRemoteParam;
   {$ENDIF}
 public
   property NotificationParam: TNotificationParam read FNotificationParam;
   property InterfaceParam: TInterfaceParam read FInterfaceParam;
   {$IFDEF NETWORK_INTF}
   property NetRemoteParam: TNetRemoteParam read FNetRemoteParam;
   {$ENDIF}
   constructor Create(Config: TConfig);
   Destructor Destroy; override;
 end;

var
 GuiConfigObj :TGuiConfig;

implementation


{ TGuiConfig }

constructor TGuiConfig.Create(Config: TConfig);
begin
  fNotificationParam:= TNotificationParam.Create(Config);
  FInterfaceParam:= TInterfaceParam.Create(Config);
  {$IFDEF NETWORK_INTF}
  fNetRemoteParam:= TNetRemoteParam.Create(Config);
  {$ENDIF}

end;

destructor TGuiConfig.Destroy;
begin
  inherited Destroy;
  fNotificationParam.Free;
  FInterfaceParam.Free;
  {$IFDEF NETWORK_INTF}
  fNetRemoteParam.Free;
  {$ENDIF}

end;

{ TGuiConfig }

{$IFDEF NETWORK_INTF}

{ TNetRemoteParam }

procedure TNetRemoteParam.SetEnabled(AValue: boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  Dirty := True;
end;

procedure TNetRemoteParam.SetOnlyLAN(AValue: boolean);
begin
  if FOnlyLAN = AValue then Exit;
  FOnlyLAN := AValue;
  Dirty := True;
end;

procedure TNetRemoteParam.SetPort(AValue: integer);
begin
  if FPort=AValue then Exit;
  FPort:=AValue;
  Dirty := True;
end;

procedure TNetRemoteParam.InternalSave;
const
  Base = 'NetRemote';
begin
  owner.IniFile.WriteBool(Base, 'Enabled', Enabled);
  owner.IniFile.WriteInteger(Base, 'Port', Port);
  owner.IniFile.WriteBool(Base, 'OnlyLAN', OnlyLAN);
end;

procedure TNetRemoteParam.Load;
const
  Base = 'NetRemote';
begin
  FEnabled := owner.IniFile.ReadBool(Base , 'Enabled', False);
  FPort    := owner.IniFile.ReadInteger(Base , 'Port', 6860);
  FOnlyLAN := owner.IniFile.ReadBool(Base , 'OnlyLAN', true);
end;
{$EndIf}

{ TInterfaceParam }

procedure TInterfaceParam.SetCaptureMMKeys(AValue: boolean);
begin
  if FCaptureMMKeys=AValue then Exit;
  FCaptureMMKeys:=AValue;
  Dirty := True;
end;

procedure TInterfaceParam.SetCaptureMMkeysMode(AValue: Integer);
begin
  if FCaptureMMkeysMode=AValue then Exit;
  FCaptureMMkeysMode:=AValue;
  Dirty := True;
end;

procedure TInterfaceParam.SetEnableSoundMenu(AValue: boolean);
begin
  if FEnableSoundMenu=AValue then Exit;
  FEnableSoundMenu:=AValue;
  Dirty := True;
end;

procedure TInterfaceParam.SetGroupBy(AValue: Integer);
begin
  if FGroupBy=AValue then Exit;
  FGroupBy:=AValue;
  Dirty := True;
end;

procedure TInterfaceParam.SetMinimizeOnClose(AValue: boolean);
begin
  if FMinimizeOnClose=AValue then Exit;
  FMinimizeOnClose:=AValue;
  Dirty := True;
end;

procedure TInterfaceParam.SetPauseWhenLocked(AValue: boolean);
begin
  if FPauseWhenLocked = AValue then Exit;
  FPauseWhenLocked := AValue;
  Dirty := True;
end;

procedure TInterfaceParam.SetShowTrayIcon(AValue: boolean);
begin
  if FShowTrayIcon=AValue then Exit;
  FShowTrayIcon:=AValue;
  Dirty := True;
end;

procedure TInterfaceParam.InternalSave;
const
  Base = 'Interface';
begin
  owner.IniFile.WriteBool(Base, 'MinimizeOnClose', MinimizeOnClose);
  owner.IniFile.WriteBool(Base, 'ShowTrayIcon', ShowTrayIcon);
  owner.IniFile.WriteBool(Base, 'EnableSoundMenu', EnableSoundMenu);
  owner.IniFile.WriteBool(Base, 'CaptureMMKeys', CaptureMMKeys);
  owner.IniFile.WriteInteger(Base, 'CaptureMMKeysMode', CaptureMMkeysMode);
  owner.IniFile.WriteInteger(Base, 'GroupBy', GroupBy);
  owner.IniFile.WriteBool(Base, 'PauseWhenLocked', PauseWhenLocked);

end;

procedure TInterfaceParam.Load;
const
  Base = 'Interface';
begin
  fMinimizeOnClose   := owner.IniFile.ReadBool(Base , 'MinimizeOnClose', True);
  fShowTrayIcon      := owner.IniFile.ReadBool(Base , 'ShowTrayIcon', True);
  fEnableSoundMenu   := owner.IniFile.ReadBool(Base , 'EnableSoundMenu', True);
  fCaptureMMKeys     := owner.IniFile.ReadBool(Base , 'CaptureMMKeys', False);
  fCaptureMMkeysMode := owner.IniFile.ReadInteger(Base , 'CaptureMMkeysMode', 0);
  fGroupBy           := owner.IniFile.ReadInteger(Base , 'GroupBy', 0);
  FPauseWhenLocked   := owner.IniFile.ReadBool(Base , 'PauseWhenLocked', False);

end;

{ TNotificationParam }

procedure TNotificationParam.SetBackColor(AValue: TColor);
begin
  if FBackColor=AValue then Exit;
  FBackColor:=AValue;
  Dirty := True;
end;

procedure TNotificationParam.SetFontColor(AValue: TColor);
begin
  if FFontColor=AValue then Exit;
  FFontColor:=AValue;
  Dirty := True;
end;

procedure TNotificationParam.SetKind(AValue: integer);
begin
  if FKind=AValue then Exit;
  FKind:=AValue;
  Dirty := True;
end;

procedure TNotificationParam.SetTimeOut(AValue: integer);
begin
  if FTimeOut=AValue then Exit;
  FTimeOut:=AValue;
  Dirty := True;
end;

procedure TNotificationParam.SetTransparency(AValue: integer);
begin
  if FTransparency=AValue then Exit;
  FTransparency:=AValue;
  Dirty := True;
end;

procedure TNotificationParam.SetX(AValue: integer);
begin
  if FX=AValue then Exit;
  FX:=AValue;
  Dirty := True;
end;

procedure TNotificationParam.SetY(AValue: integer);
begin
  if FY=AValue then Exit;
  FY:=AValue;
  Dirty := True;
end;

procedure TNotificationParam.InternalSave;
const
  Base = 'Notification';
begin
  owner.IniFile.WriteInteger(Base, 'Kind', Kind);
  owner.IniFile.WriteString(Base, 'BackColor', ColorToString(BackColor));
  owner.IniFile.WriteString(Base, 'FontColor', ColorToString(FontColor));
  owner.IniFile.WriteInteger(Base, 'TimeOut', TimeOut);
  owner.IniFile.WriteInteger(Base, 'X', X);
  owner.IniFile.WriteInteger(Base, 'Y', Y);
  owner.IniFile.WriteInteger(Base, 'Transparency', Transparency);
end;

procedure TNotificationParam.Load;
const
  Base = 'Notification';
begin
  fKind         := owner.IniFile.ReadInteger(Base , 'Kind', 2);
  fBackColor    := StringToColorDef(owner.IniFile.ReadString(Base,'BackColor',''), $00B66F18);
  fFontColor    := StringToColorDef(owner.IniFile.ReadString(Base,'FontColor',''), $00000000);
  fTimeOut      := owner.IniFile.ReadInteger(Base , 'TimeOut', 3000);
  fX            := owner.IniFile.ReadInteger(Base , 'X', 100);
  fY            := owner.IniFile.ReadInteger(Base , 'Y', 100);
  fTransparency := owner.IniFile.ReadInteger(Base , 'Transparency', 230);
end;

end.
