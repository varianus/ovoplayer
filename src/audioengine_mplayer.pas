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
unit AudioEngine_MPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Process, UTF8Process, Song, BaseTypes, AudioEngine;

type

  { TAudioEngineMPlayer }

  TAudioEngineMPlayer = class(TAudioEngine)
  private
    fMainVolume: integer;
    FPaused: boolean;
    fPlayerProcess: TProcessUTF8;
    fPlayerState: TEngineState;
    FPlayRunningI: boolean;
    fPosition: integer;
    fTimer: TTimer;
    fMuted: boolean;
    ExePath: string;
    Params:  string;
    procedure RunAndPlay(Filename: String);
    procedure SetPaused(const AValue: boolean);
    procedure SendMPlayerCommand(Cmd: string);
    procedure TimerEvent(Sender: TObject);
  protected
    function GetMainVolume: integer; override;
    procedure SetMainVolume(const AValue: integer); override;
    function GetMaxVolume: integer; override;
    function GetSongPos: integer; override;
    procedure SetSongPos(const AValue: integer); override;
    Function DoPlay(Song: TSong; offset:Integer):boolean;override;
    function GetState: TEngineState; override;
    procedure SetMuted(const AValue: boolean);  override;
    Function GetMuted: boolean; override;
    // see: mplayer -input cmdlist and http://www.mplayerhq.hu/DOCS/tech/slave.txt

  public
    class Function GetEngineName: String; override;
    Class Function IsAvalaible(ConfigParam: TStrings): boolean; override;
    Class Function GetEngineParams: AREngineParams; override;

    constructor Create; override;
    procedure Activate; override;
    function Initialize: boolean; override;
    destructor Destroy; override;
    procedure Pause; override;
    function Playing: boolean; override;
    procedure PostCommand(Command: TEngineCommand; Param: integer); override;
    function Running: boolean; override;
    procedure Seek(Seconds: integer; SeekAbsolute: boolean); override;
    procedure Stop; override;
    procedure UnPause; override;

  end;

implementation

uses strutils, FileUtil, LazFileUtils, lclproc;
Const
  MPLAYERMAXVOLUME = 100;


const
  TIMEPOSOUT = 'A: ';
  EXITING = 'Exiting...';

  fMPlayerCommand = 'mplayer';


var
  EngFormat: TFormatSettings;
  fProgramPath:string;

{ TAudioEngineMPlayer }

procedure TAudioEngineMPlayer.TimerEvent(Sender: TObject);
var
  NoMoreOutput: boolean;

  procedure DoStuffForProcess;
  var
    Buffer:     ansistring;
    BytesAvailable: DWord;
    BytesRead:  longint;
    ProcessStr: string;
    cmdPos:     integer;
    tmpPos:     double;
  begin
    if Running then
      begin
      BytesAvailable := fPlayerProcess.Output.NumBytesAvailable;
      BytesRead      := 0;
      while BytesAvailable > 0 do
        begin
        SetLength(Buffer, BytesAvailable);
        BytesRead  := fPlayerProcess.OutPut.Read(Buffer[1], BytesAvailable);
        ProcessStr := copy(Buffer, 1, BytesRead);
     //   debugln(ProcessStr);

        if AnsiStartsStr(TIMEPOSOUT,ProcessStr) then
          begin
            ProcessStr := trim(Copy(ProcessStr, 3, 7));
            if not TryStrToFloat(ProcessStr, tmpPos, EngFormat) then
              fPosition := 0
            else
              fPosition := trunc(tmpPos * 1000);
          end;
        BytesAvailable := fPlayerProcess.Output.NumBytesAvailable;
        NoMoreOutput   := False;
        end;
      SetLength(Buffer, 0);
      end
  end;

begin
  DoStuffForProcess;
  repeat
    NoMoreOutput := True;
    DoStuffForProcess;
  until noMoreOutput or (not Running);

  if not Running then
    begin
      fTimer.enabled := false;
      PostCommand(ecNext,0);
    end;

end;

function TAudioEngineMPlayer.GetMainVolume: integer;
begin
  Result := trunc(fMainVolume *  ( 255 / MPLAYERMAXVOLUME)) ;
end;

function TAudioEngineMPlayer.GetSongPos: integer;
begin
  Result := trunc(fPosition);
end;

procedure TAudioEngineMPlayer.SetMainVolume(const AValue: integer);
begin
  if AValue = fMainVolume then
    exit;
  fMainVolume := trunc(AValue * MPLAYERMAXVOLUME /255);
  SendMPlayerCommand('volume ' + IntToStr(fMainVolume) + ' 1');
end;

function TAudioEngineMPlayer.GetMaxVolume: integer;
begin
  Result:= MPLAYERMAXVOLUME;
end;


procedure TAudioEngineMPlayer.SetPaused(const AValue: boolean);
begin
  if FPaused = AValue then
    exit;
  FPaused := AValue;
  if Running then
    begin
    SendMPlayerCommand('pause');
    if FPaused then
       fPlayerState := ENGINE_PAUSE
    else
       fPlayerState := ENGINE_PLAY
    end;
end;

procedure TAudioEngineMPlayer.SetSongPos(const AValue: integer);
begin
  Seek(AValue, True);
end;

procedure TAudioEngineMPlayer.SendMPlayerCommand(Cmd: string);
const
  LineEnding = #10;
begin
  if Cmd = '' then
    exit;
  if not Running then
    exit;
  if Cmd[length(Cmd)] <> LineEnding then
    Cmd := Cmd + LineEnding;
  fPlayerProcess.Input.Write(Cmd[1], length(Cmd));
end;

constructor TAudioEngineMPlayer.Create;
begin
  inherited Create;
  fMainVolume := 127;
  fTimer := TTimer.Create(nil);
  fTimer.Enabled:=false;
  fTimer.Interval :=150;
  fTimer.OnTimer  := @TimerEvent;

end;

procedure TAudioEngineMPlayer.Pause;
begin
  SetPaused(True);
end;

procedure TAudioEngineMPlayer.Stop;
begin
  SendMPlayerCommand('stop');
  fPlayerProcess.Terminate(0);
end;

function TAudioEngineMPlayer.DoPlay(Song: TSong; offset: Integer): boolean;
begin
  if not Assigned(Song) then
    exit;

  Activate;

  Params := StringReplace(Song.FullName, '\', '/', [rfReplaceall]);
  if not Running then
     RunAndPlay(Params)
  else
     SendMPlayerCommand('loadfile "' + Params + '"');

  fPlayerState := ENGINE_PLAY;
  FPlayRunningI := True;

  if offset <> 0 then
    Seek(offset, true);

  fTimer.Enabled:=true;

  Result := fPlayerProcess.Running;

end;
procedure TAudioEngineMPlayer.Activate;
begin

end;

function TAudioEngineMPlayer.Initialize: boolean;
begin
  ExePath := fProgramPath;
  if not FilenameIsAbsolute(ExePath) then
    ExePath := FindDefaultExecutablePath(ExePath);
  Result := FileExistsUTF8(ExePath);
  Initialized := result;

end;

procedure TAudioEngineMPlayer.RunAndPlay(Filename:String);
begin
  if Running and Paused then
    begin
    Paused := False;
    exit;
    end;

  if Playing then
    exit;

  FPlayRunningI := True;
  fPlayerProcess := TProcessUTF8.Create(nil);
  Params := ' -slave -nofs -nomouseinput -noquiet -vc null -vo null ';// -nofontconfig '; //  -priority abovenormal -really-quiet -identify
  Params := Params + ' -volume ' + IntToStr(Self.MainVolume) + ' -softvol -softvol-max 255';
  fPlayerProcess.Options := fPlayerProcess.Options + [poUsePipes, poNoConsole, poStderrToOutPut];
  fPlayerProcess.CommandLine :=ExePath + ' ' + Params + ' "' +Filename+'"';
  fPlayerProcess.Execute;

end;

function TAudioEngineMPlayer.GetState: TEngineState;
begin
  Result := fPlayerState;
end;

procedure TAudioEngineMPlayer.SetMuted(const AValue: boolean);
begin
  if AValue = fMuted then
     exit;
  if fMuted then
     begin
        SendMPlayerCommand('mute 1');
        fMuted:=true;
     end
 else
     begin
        SendMPlayerCommand('mute 0');
        fMuted:=true;
     end;

end;

function TAudioEngineMPlayer.GetMuted: boolean;
begin
  Result := fMuted;
end;

class function TAudioEngineMPlayer.GetEngineName: String;
begin
  Result:='MPlayer';
end;

class function TAudioEngineMPlayer.IsAvalaible(ConfigParam: TStrings): boolean;
var
  AProcess : TProcessUTF8;
  APath :string;
begin
  Result:= false;
  if Assigned(ConfigParam) then
    begin
      APath:= ConfigParam.Values['Path'];
      if trim(APath) = '' then
         APath := fMPlayerCommand;
    end
  else
    APath := fMPlayerCommand;;

  AProcess := TProcessUTF8.Create(nil);
  AProcess.Options := AProcess.Options + [poUsePipes, poNoConsole];

  try
    if APath = '' then
       begin
          result:= false;
          exit;
       end;
    AProcess.CommandLine:= APath;
    fProgramPath:=APath;
    try
      AProcess.Execute;
      Result:=true;
    Except
      Result := false;
    end;

  finally
    if AProcess.Running then
       AProcess.Terminate(0);
    AProcess.free;
  end;

end;

class function TAudioEngineMPlayer.GetEngineParams: AREngineParams;
var
  tmpName: string;
begin
  SetLength(Result,1);
  Result[0].Key:='Path';

  tmpname :=  FindFilenameOfCmd(fMPlayerCommand+GetExeExt);
  if tmpName = '' then
    tmpname :=  FindFilenameOfCmd(fMPlayerCommand+'2'+GetExeExt);
  Result[0].Value:=tmpName;

  Result[0].Kind:=epkFileName;

end;

procedure TAudioEngineMPlayer.PostCommand(Command: TEngineCommand;
  Param: integer);
begin
  ReceivedCommand(Self, Command, Param);
end;


procedure TAudioEngineMPlayer.UnPause;
begin
  SetPaused(False);
end;

function TAudioEngineMPlayer.Running: boolean;
begin
  Result := (fPlayerProcess <> nil) and fPlayerProcess.Running;
end;

function TAudioEngineMPlayer.Playing: boolean;
begin
  Result := (fPlayerProcess <> nil) and fPlayerProcess.Running and (Not Paused);
end;

procedure TAudioEngineMPlayer.Seek(Seconds: integer; SeekAbsolute: boolean);
var
  st: string;
begin
  if Running then
  begin
    st := 'seek ' + IntToStr(Seconds div 1000);
    if SeekAbsolute then
      st := st + ' 2'
    else
      st := st + ' 0';
    SendMPlayerCommand(st);
  end;
end;

destructor TAudioEngineMPlayer.Destroy;
begin
  SendMPlayerCommand('quit');
  if Running then
     fPlayerProcess.Terminate(0);

  inherited Destroy;
end;

initialization
  EngFormat := DefaultFormatSettings;
  EngFormat.DecimalSeparator := '.';
  EngFormat.ThousandSeparator := ',';

  RegisterEngineClass(TAudioEngineMPlayer, 10, true, false);

end.
