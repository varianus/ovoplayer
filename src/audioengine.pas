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
  Classes, SysUtils, ExtCtrls, Song, BaseTypes;

type

  TAudioEngineClass = class of TAudioEngine;

  RAudioEngine = record
    Engine : TAudioEngineClass;
    Name: String;
    Priority : Integer;
    ForceSelection:boolean;
    _Experimental : boolean;
  end;

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
    Function DoPlay(Song: TSong; offset:Integer):boolean; virtual; abstract;
    procedure ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0); virtual;
    procedure SetMuted(const AValue: boolean); virtual; abstract;
    Function GetMuted: boolean; virtual; abstract;
  public
    class Function GetEngineName: string; virtual; abstract;
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
    property Muted: boolean read GetMuted write SetMuted;
    property MainVolume: integer read GetMainVolume write SetMainVolume;
    property OnSongEnd: TNotifyEvent read FOnSongEnd write SetOnSongEnd;
    property OnSongStart: TNotifyEvent read FOnSongStart write SetOnSongStart;
    property Paused: boolean read FPaused write SetPaused;
    property Position: integer read GetSongPos write SetSongPos;
    property State: TEngineState read GetState;
    Property MaxVolume: Integer Read GetMaxVolume;
  end;

Procedure RegisterEngineClass(const EngineClass: TAudioEngineClass;
                              const Priority:Integer;
                              const ForceSelection: boolean = false;
                              const _Experimental:boolean = false);

Function GetEngineByName(const Name: string): TAudioEngineClass;

Function GetBestEngine: TAudioEngineClass;

var
  EngineArray : array of RAudioEngine;

implementation
uses FileUtil;

procedure RegisterEngineClass(const EngineClass: TAudioEngineClass;
                              const Priority: Integer;
                              const ForceSelection: boolean = false;
                              const _Experimental:boolean = false);
var
  EngineRecord : RAudioEngine;
begin
  SetLength(EngineArray, Length(EngineArray) + 1);
  EngineRecord.Engine := EngineClass;
  EngineRecord.Name := Engineclass.GetEngineName;
  EngineRecord.Priority := Priority;
  EngineRecord.ForceSelection:=ForceSelection;
  EngineRecord._Experimental := _Experimental;
  EngineArray[High(EngineArray)] := EngineRecord;
end;

function GetEngineByName(const Name: string): TAudioEngineClass;
var
  i:integer;
begin
  result:=nil;
  for i := Low(EngineArray) to High(EngineArray) do
    if SameText(EngineArray[i].Name, Name) then
       begin
         Result:= EngineArray[i].Engine;
         break;
       end;

end;

procedure SortEngines(var Vals:Array of RAudioEngine);
var i,j,k:Integer;
    Hold:RAudioEngine;
    ACount:Integer;
begin
  ACount := Length(Vals);
  for i := 1 to ACount - 1 do
    begin
      Hold:=Vals[i];
      j := i;
      k := j - 1;
      while ((j > 0) and (Vals[k].Priority > Hold.Priority)) do
        begin
          Vals[j] := Vals[k];
          dec(j);
          dec(k);
        end;
      Vals[j] := Hold;
    end;
end;

function GetBestEngine: TAudioEngineClass;
var
  i:integer;
begin
  SortEngines(EngineArray);
  result:=nil;
  for i := Low(EngineArray) to High(EngineArray) do
    if EngineArray[i].Engine.isAvalaible(nil) then
       begin
         result := EngineArray[i].Engine;
         break;
       end;

end;

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

procedure TAudioEngine.ReceivedCommand(Sender: TObject;
  Command: TEngineCommand; Param: integer);
begin
  case Command of
    ecNext: if Assigned(fOnSongEnd) then
        fOnSongEnd(Self);

    ecSeek: Seek(Param, True);

    end;
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
 //
end;

procedure TAudioEngine.Play(Song: TSong; offset:Integer=0);
begin
  if Song = nil then
     exit;

  if not FileExistsUTF8( Song.FullName) then
     begin
       PostCommand(ecNext);
       exit;
     end;

  if DoPlay(song, offset) then
     begin
       if Assigned(FOnSongStart) then
         FOnSongStart(self);
     end
  else
    begin
      PostCommand(ecNext);
    end;
end;

initialization
  setlength(EngineArray, 0);
Finalization
  setlength(EngineArray, 0);
end.
