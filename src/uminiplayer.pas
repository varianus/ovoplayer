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
unit uMiniPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, GUIBackEnd, ThemedSlider;

type

  { TfMiniPlayer }

  TfMiniPlayer = class(TForm)
    Panel1: TPanel;
    tbPlayControls: TToolBar;
    Timer: TTimer;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    TrackBar: TThemedSlider;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  public
    procedure ShowAtPos(x: Integer; y: Integer);
  end; 

var
  fMiniPlayer: TfMiniPlayer;

implementation

{$R *.lfm}
uses BaseTypes;

{ TfMiniPlayer }

procedure TfMiniPlayer.ShowAtPos(x: Integer; y: Integer);
begin
  if x + Width > Screen.Monitors[0].WorkareaRect.Left then
  begin
    left := x - Width;
    if Left < 0 then Left := 0;
  end
  else
    left := x;

  if y + Height >  Screen.Monitors[0].WorkareaRect.Top then
  begin
    top := y - Height;
    if top < 0 then top := 0;
  end
  else
    top := y;

  Show;
end;

procedure TfMiniPlayer.FormShow(Sender: TObject);
begin
  Timer.Enabled:=true;
end;

procedure TfMiniPlayer.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Timer.Enabled:=false;
end;

procedure TfMiniPlayer.FormCreate(Sender: TObject);
begin
  Timer.Enabled:=false;
end;

procedure TfMiniPlayer.FormHide(Sender: TObject);
begin
  Timer.Enabled:=false;
end;

procedure TfMiniPlayer.FormMouseLeave(Sender: TObject);
begin
  hide;
end;

procedure TfMiniPlayer.TimerTimer(Sender: TObject);
begin
 if BackEnd.Status = ENGINE_PLAY then
    begin
      TrackBar.Max := BackEnd.PlayList.CurrentItem.Tags.Duration;
      TrackBar.Position := BackEnd.AudioEngine.Position;
    end;
end;

procedure TfMiniPlayer.TrackBarChange(Sender: TObject);
begin
  BackEnd.AudioEngine.Position := TrackBar.Position;
end;

end.
