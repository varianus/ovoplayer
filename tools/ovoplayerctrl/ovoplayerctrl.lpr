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
program ovoplayerctrl;
{$IFDEF WINDOWS}
  {$DEFINE CONSOLEHACK}

  {$IFNDEF CONSOLEHACK}
     {$apptype console}
  {$ENDIF}
{$ENDIF}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, appconsts, netprotocol, netsupport, CustApp,  lclproc,
  { you can add units after this }
  {$IFDEF CONSOLEHACK}
  windows,  JwaWinUser,
  {$ENDIF}
  singleinstance, SimpleSingleInstance;

type

  { TOvoPlayerCtrl }
  TCommandType = (ctFile, ctAction);

  TOvoPlayerCtrl = class(TCustomApplication)
  private
    ShortOptions:string;
    LongOptions:TStringList;

    function PostCommand(Category:string ; Command:string; Parameter:string=''): Boolean;
    Procedure AddOptions(ShortOption:string; LongOption:string); overload;
    Procedure AddOptions(LongOption:string); overload;
    procedure WriteVersion;
    Procedure NeedConsole;
    Procedure DoneConsole;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TOvoPlayerCtrl }
{$IFDEF CONSOLEHACK}
  function AttachConsole(dwProcessId: DWORD): BOOL; stdcall; external kernel32 name 'AttachConsole';
  function GetConsoleWindow(): HWND; stdcall; external kernel32 name 'GetConsoleWindow';

  procedure SendKeyInput(Flag: DWORD; Key: Word);
  var
    Input: TInput;
  begin
    FillChar(Input, SizeOf(Input), 0);
    Input.type_ := INPUT_KEYBOARD;
    Input.ki.dwFlags := Flag;
    Input.ki.wVk := Key;

    SendInput(1, @Input, SizeOf(Input));
  end;
{$ENDIF}

  var
  BaseServerId:string = 'SI_';
  AppNameServerID :string  = 'ovoplayer_exe';
  AppVersion : string = {$I ..\..\src\version.inc};

function TOvoPlayerCtrl.PostCommand(Category:string; Command:string;Parameter:string=''): Boolean;
var
  Inst: TSimpleSingleInstance;
begin
  inst := TSimpleSingleInstance.Create(nil);
  try
    Inst.ID := BaseServerId + AppNameServerID;

    Result := inst.Start = siClient;
    if Result then
      begin
        inst.ClientPostString(BuildCommand(Category, Command, Parameter, false));
      end
    else
     begin
       NeedConsole;
       WriteLn('Cannot find ovoplayer instance');
       DoneConsole;
     end;
  finally
    inst.Free;
  end;
end;

procedure TOvoPlayerCtrl.AddOptions(ShortOption: string; LongOption: string);
begin
  ShortOptions:=ShortOptions+ShortOption;
  AddOptions(LongOption);
end;

procedure TOvoPlayerCtrl.AddOptions(LongOption: string);
begin
  LongOptions.Add(LongOption);
end;


procedure TOvoPlayerCtrl.DoRun;
var
  ErrorMsg: String;
  i:Integer;
const
  MediaControlCount = 8;
var
  MediaControl : array [0..MediaControlCount-1] of string =
                                   ('pause', 'play', 'playpause', 'stop',
                                    'next', 'previous',
                                    'seek+', 'seek-');

  MediaControlCommand : array [0..MediaControlCount-1] of string =
                                   (COMMAND_PAUSE, COMMAND_PLAY, COMMAND_PLAYPAUSE, COMMAND_STOP,
                                    COMMAND_NEXT, COMMAND_PREVIOUS,
                                    COMMAND_SEEK_P, COMMAND_SEEK_M);
begin
  // quick check parameters
  AddOptions('h','help');
  AddOptions('v','version');
  AddOptions('p:','playsong:');
  AddOptions('e:','enqueue:');
  AddOptions('x:','enqplay:');
  AddOptions('q','quit');
  AddOptions('d:','debug-log:');


  for i := 0 to MediaControlCount -1 do
     AddOptions(MediaControl[i]);

  ErrorMsg:=CheckOptions(ShortOptions, LongOptions, true);
  if ErrorMsg<>'' then begin
    NeedConsole;
    Writeln(ErrorMsg);
    DoneConsole;
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help')  or (paramcount =0) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('v','version') then begin
    WriteVersion;
  end;

  for i := 0 to MediaControlCount -1 do
     if HasOption(MediaControl[i]) then
        PostCommand(CATEGORY_ACTION,MediaControlCommand[i]);

  if HasOption('e','enqueue') then
     PostCommand(CATEGORY_FILE, COMMAND_ENQUEUE, GetOptionValue('e','enqueue'));

  if HasOption('p','playsong') then
     PostCommand(CATEGORY_FILE, COMMAND_CLEAR_AND_PLAY , GetOptionValue('p','playsong'));

  if HasOption('x','enqplay') then
     PostCommand(CATEGORY_FILE, COMMAND_ENQUEUE_AND_PLAY, GetOptionValue('x','enqplay'));

  if HasOption('q','quit') then
     PostCommand(CATEGORY_APP, COMMAND_QUIT);


  // stop program loop
  Terminate;
end;

constructor TOvoPlayerCtrl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ShortOptions:= '';
  LongOptions:= TStringList.Create;
  StopOnException:=True;
end;

destructor TOvoPlayerCtrl.Destroy;
begin
  LongOptions.Free;
  inherited Destroy;
end;

procedure TOvoPlayerCtrl.WriteVersion;
begin
  NeedConsole;
  writeln('ovoplayerctrl ' + AppVersion);
  writeln('This application act as a "remote control" for Ovoplayer application');
  writeln;
  writeln('Copyright (C) 2011, Marco Caselli <marcocas@gmail.com>');
  writeln('License GPLv2: http://www.gnu.org/licenses/old-licenses/gpl-2.0.html');
  DoneConsole;
end;

procedure TOvoPlayerCtrl.NeedConsole;
var
 h: HRESULT;
begin
  {$IFDEF CONSOLEHACK}
  h:= GetStdHandle(STD_OUTPUT_HANDLE);
  if h <> 0 then
    begin
      Freeconsole;
      AttachConsole(DWORD(-1));
    end;
  h:= GetStdHandle(STD_OUTPUT_HANDLE);

  if h = 0 then
     AllocConsole;

  rewrite(system.Output);
  reset(system.input);
  WriteLn();
  {$ENDIF CONSOLEHACK}
end;

procedure TOvoPlayerCtrl.DoneConsole;
begin
  {$IFDEF CONSOLEHACK}
     WriteLn();
     Close(system.Output);
     close(system.Input);
     if GetForegroundWindow = GetConsoleWindow() then
        begin
          SendKeyInput(0,$0d);
          SendKeyInput(KEYEVENTF_KEYUP,$0d);
        end;
     FreeConsole;
  {$ENDIF}
end;

procedure TOvoPlayerCtrl.WriteHelp;
begin
  NeedConsole;
  writeln('ovoplayerctrl ' + AppVersion);
  writeln('This application act as a "remote control" for Ovoplayer application');
  writeln('Usage: ovoplayerctrl [option]');
  writeln;
  writeln('--next' + sLineBreak +
          '    ' + 'Skip to next song in playlist');
  writeln('--previous' + sLineBreak +
          '    ' + 'Skip to previous song in playlist');
  writeln('--pause' + sLineBreak +
          '    ' + 'Pause current song');
  writeln('--play' + sLineBreak +
          '    ' + 'Resume current song');
  writeln('--playpause' + sLineBreak +
          '    ' + 'Pause current song if playing or resume current song if paused');
  writeln('--stop' + sLineBreak +
          '    ' + 'Stop playing');
  writeln('--seek+' + sLineBreak +
          '    ' + 'Skip forward 10 second in current song');
  writeln('--seek-' + sLineBreak +
          '    ' + 'Skip backward 10 second in current song');
  writeln('-e <filename>, --enqueue=<filename>' + sLineBreak +
          '    ' + 'Enqueue song to current playlist');
  writeln('-p <filename>, --playsong=<filename>' + sLineBreak +
          '    ' + 'Play song and clear current playlist');
  writeln('-x <filename>, --enqplay=<filename>' + sLineBreak +
          '    ' + 'Enqueue current playlist AND begin play a song');
  writeln('-q , --quit' + sLineBreak +
          '    ' + 'Close ovoplayer');
  DoneConsole;

end;

var
  Application: TOvoPlayerCtrl;

{$R *.res}

begin
  Application:=TOvoPlayerCtrl.Create(nil);
  Application.Run;
  Application.Free;
end.
