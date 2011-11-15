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
unit audioengine_dummy;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, AudioEngine, Song;

type

  { TAudioEnginedummy }

  TAudioEnginedummy = class(TAudioEngine)
  private
  protected
    function GetMainVolume: integer; override;
    procedure SetMainVolume(const AValue: integer); override;
    function GetMaxVolume: integer; override;
    function GetSongPos: integer; override;
    procedure SetSongPos(const AValue: integer); override;
    function GetState: TEngineState; override;
    procedure DoPlay(Song: TSong; offset:Integer); override;
    procedure SetMuted(const AValue: boolean);  override;
    Function GetMuted: boolean; override;
    procedure ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0); override;
  public
    class Function GetEngineName: String; override;
    Class Function IsAvalaible(ConfigParam: TStrings): boolean; override;

    procedure PostCommand(Command: TEngineCommand; Param: integer = 0); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure Activate; override;
    procedure Pause; override;
    function Playing: boolean; override;
    function Running: boolean; override;
    procedure Seek(Seconds: integer; SeekAbsolute: boolean); override;
    procedure Stop; override;
    procedure UnPause; override;

  end;


implementation

{ TAudioEnginedummy }

function TAudioEnginedummy.GetMainVolume: integer;
begin
  Result := 100;

end;

procedure TAudioEnginedummy.SetMainVolume(const AValue: integer);
begin

end;

function TAudioEnginedummy.GetMaxVolume: integer;
begin
  Result:=100;
end;

function TAudioEnginedummy.GetSongPos: integer;
begin
  Result := 0;
end;

procedure TAudioEnginedummy.SetSongPos(const AValue: integer);
begin
end;

procedure TAudioEnginedummy.Activate;
begin
//
end;

constructor TAudioEnginedummy.Create;
begin
  inherited Create;

end;

destructor TAudioEnginedummy.Destroy;
begin

  inherited Destroy;
end;

function TAudioEnginedummy.GetState: TEngineState;
begin
  Result := ENGINE_ON_LINE;
end;

procedure TAudioEnginedummy.Pause;
begin

end;

procedure TAudioEnginedummy.DoPlay(Song: TSong; offset:Integer);
begin
end;

procedure TAudioEnginedummy.SetMuted(const AValue: boolean);
begin
end;

function TAudioEnginedummy.GetMuted: boolean;
begin
  Result:=false;
end;

class function TAudioEnginedummy.GetEngineName: String;
begin
  Result:='dummy';
end;

procedure TAudioEnginedummy.ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer = 0);
begin
  case Command of
    ecNext: if Assigned(OnSongEnd) then
        OnSongEnd(Self);
    end;
end;

class function TAudioEnginedummy.IsAvalaible(ConfigParam: TStrings): boolean;
begin
  Result:= True;
end;

procedure TAudioEnginedummy.PostCommand(Command: TEngineCommand; Param: integer);
begin
  ReceivedCommand(Self, Command, Param);
end;

function TAudioEnginedummy.Playing: boolean;
begin
  Result := False;
end;

function TAudioEnginedummy.Running: boolean;
begin
  Result := true  ;
end;

procedure TAudioEnginedummy.Seek(Seconds: integer; SeekAbsolute: boolean);
begin
end;

procedure TAudioEnginedummy.Stop;
begin
end;

procedure TAudioEnginedummy.UnPause;
begin
end;

initialization
  RegisterEngineClass(TAudioEnginedummy, 99, false, false);
end.
