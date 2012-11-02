program ovoplayerctrl;
{$apptype console}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
   SimpleIPC;

type

  { TOvoPlayerCtrl }
  TCommandType = (ctFile, ctAction);

  TOvoPlayerCtrl = class(TCustomApplication)
  private
    ShortOptions:string;
    LongOptions:TStringList;

    function PostCommand(CommandType:TCommandType; Command:string; Parameter:string=''): Boolean;
    Procedure AddOptions(ShortOption:string; LongOption:string); overload;
    Procedure AddOptions(LongOption:string); overload;
    procedure WriteVersion;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TOvoPlayerCtrl }
var
  BaseServerId:string = 'tuniqueinstance_';
  AppNameServerID :string  = 'ovoplayer';
  AppVersion : string = {$I ..\..\src\version.inc};
  Separator:string = '|';
function TOvoPlayerCtrl.PostCommand(CommandType:TCommandType;Command:string;Parameter:string=''): Boolean;
var
  TempStr: String;
begin
  with TSimpleIPCClient.Create(nil) do
  try
    ServerId := BaseServerId + AppNameServerID;
    Result := ServerRunning;
    if Result then
      begin
        case CommandType of
          ctAction : TempStr := 'action:'+lowercase(Command) + Separator;
          ctFile : TempStr := 'file:'+lowercase(Command) +'='+Parameter+ Separator;
        end;

        Active := True;
        SendStringMessage(TempStr);
      end;
  finally
    Free;
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
  MediaControlCount = 7;
var
  MediaControl : array [0..MediaControlCount-1] of string =
                                   ('pause', 'play', 'stop',
                                    'next', 'previous',
                                    'seek+', 'seek-');
begin
  // quick check parameters
  AddOptions('h','help');
  AddOptions('v','version');
  AddOptions('p:','playsong:');
  AddOptions('e:','enqueue:');
  AddOptions('x:','enqplay:');

  for i := 0 to MediaControlCount -1 do
     AddOptions(MediaControl[i]);

  ErrorMsg:=CheckOptions(ShortOptions, LongOptions);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  if HasOption('v','version') then begin
    WriteVersion;
  end;

  for i := 0 to MediaControlCount -1 do
     if HasOption(MediaControl[i]) then
        PostCommand(ctAction,MediaControl[i]);

  if HasOption('e','enqueue') then
     PostCommand(ctFile, 'e', GetOptionValue('e','enqueue'));

  if HasOption('p','playsong') then
     PostCommand(ctFile, 'p', GetOptionValue('p','playsong'));

  if HasOption('x','enqplay') then
     PostCommand(ctFile, 'x', GetOptionValue('x','enqplay'));


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
  { add your help code here }
  writeln('ovoplayerctrl ' + AppVersion);
  writeln('This application act as a "remote control" for Ovoplayer application');
  writeln;
  writeln('Copyright (C) 2011, Marco Caselli <marcocas@gmail.com>');
  writeln('License GPLv2: http://www.gnu.org/licenses/old-licenses/gpl-2.0.html');
end;
procedure TOvoPlayerCtrl.WriteHelp;
begin
  writeln('ovoplayerctrl ' + AppVersion);
  writeln('This application act as a "remote control" for Ovoplayer application');
//  writeln('Copyright (C) 2011, Marco Caselli <marcocas@gmail.com>');
//  writeln('License GPLv2: http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
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

end;

var
  Application: TOvoPlayerCtrl;

{$R *.res}

begin
  Application:=TOvoPlayerCtrl.Create(nil);
  Application.Run;
  Application.Free;
end.

