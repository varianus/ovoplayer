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
unit AudioEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Song;

type
  TEngineState = (ENGINE_STOP,
    ENGINE_PLAY,
    ENGINE_PAUSE,
    ENGINE_SONG_END,
    ENGINE_ON_LINE);

  TEngineCommand = (ecInvalid, ecStop, ecPrevious, ecPlay, ecNext, ecPause, ecSeek);


  TAudioEngineClass = class of TAudioEngine;
  { TAudioEngine }

  TAudioEngine = class
  private
    FOnSongEnd:   TNotifyEvent;
    FOnSongStart: TNotifyEvent;
    FPaused:      boolean;
  protected
    function GetMainVolume: integer; virtual; abstract;
    function GetMaxVolume: integer; virtual; abstract;
    procedure SetMainVolume(const AValue: integer); virtual; abstract;
    function GetSongPos: integer; virtual; abstract;
    procedure SetOnSongEnd(const AValue: TNotifyEvent);
    procedure SetOnSongStart(const AValue: TNotifyEvent);
    procedure SetPaused(const AValue: boolean);
    procedure SetSongPos(const AValue: integer); virtual; abstract;
    function GetState: TEngineState; virtual; abstract;
    procedure DoPlay(Song: TSong; offset:Integer); virtual; abstract;
    procedure ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0); virtual; abstract;
    procedure SetMuted(const AValue: boolean); virtual; abstract;
    Function GetMuted: boolean; virtual; abstract;
    Function GetEngineName: string; virtual; abstract;
  public
    Class Function IsAvalaible(ConfigParam: TStrings): boolean; virtual; abstract;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Pause; virtual;
    procedure Activate; virtual; abstract;
    procedure Play(Song: TSong; offset:Integer=0); virtual;
    function Playing: boolean; virtual; abstract;
    function Running: boolean; virtual; abstract;
    procedure Seek(Seconds: integer; SeekAbsolute: boolean); virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure UnPause; virtual; abstract;
    procedure PostCommand(Command: TEngineCommand; Param: integer = 0); virtual; abstract;
  public
    property EngineName: string read GetEngineName;
    property Muted: boolean read GetMuted write SetMuted;
    property MainVolume: integer read GetMainVolume write SetMainVolume;
    property OnSongEnd: TNotifyEvent read FOnSongEnd write SetOnSongEnd;
    property OnSongStart: TNotifyEvent read FOnSongStart write SetOnSongStart;
    property Paused: boolean read FPaused write SetPaused;
    property Position: integer read GetSongPos write SetSongPos;
    property State: TEngineState read GetState;
    Property MaxVolume: Integer Read GetMaxVolume;
  end;

implementation

var
  EngFormat: TFormatSettings;

{ TAudioEngine }

procedure TAudioEngine.SetOnSongEnd(const AValue: TNotifyEvent);
begin
  if FOnSongEnd = AValue then
    exit;
  FOnSongEnd := AValue;
end;

procedure TAudioEngine.SetOnSongStart(const AValue: TNotifyEvent);
begin
  if FOnSongStart = AValue then
    exit;
  FOnSongStart := AValue;
end;

procedure TAudioEngine.SetPaused(const AValue: boolean);
begin
  if AValue then
    Pause
  else
    UnPause;

end;

constructor TAudioEngine.Create;
begin
  inherited Create;
end;


destructor TAudioEngine.Destroy;
begin
  inherited Destroy;
end;

procedure TAudioEngine.Pause;
begin

end;

procedure TAudioEngine.Play(Song: TSong; offset:Integer=0);
begin
  if Song = nil then
     exit;

  if not FileExists( Song.FullName) then
     begin
       PostCommand(ecNext);
       exit;
     end;

  DoPlay(song, offset);
  if Assigned(FOnSongStart) then
    FOnSongStart(self);
end;

end.
