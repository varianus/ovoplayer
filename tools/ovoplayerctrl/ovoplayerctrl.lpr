program ovoplayerctrl;
{$apptype gui}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
   SimpleIPC, AppConsts;

type

  { TOvoPlayerCtrl }

  TOvoPlayerCtrl = class(TCustomApplication)
  private
    ShortOptions:string;
    LongOptions:TStringList;

    function PostCommand(Command:string): Boolean;
    Procedure AddOptions(ShortOption:string; LongOption:string); overload;
    Procedure AddOptions(LongOption:string); overload;
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
  Separator:string = '|';

function TOvoPlayerCtrl.PostCommand(Command:string): Boolean;
var
  TempStr: String;
begin
  with TSimpleIPCClient.Create(nil) do
  try
    ServerId := BaseServerId + AppNameServerID;
    Result := ServerRunning;
    if Result then
      begin
        TempStr := 'action:'+lowercase(Command) + Separator;
        Active := True;
        SendStringMessage(TempStr);
      end;
  finally
    Free;
  end;
end;

procedure TOvoPlayerCtrl.AddOptions(ShortOption: string; LongOption: string);
begin
  ShortOptions:=ShortOptions+ShortOptions;
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
  AddOptions('p:','playsong:');
  AddOptions('e:','enqueue:');

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

  for i := 0 to MediaControlCount -1 do
     if HasOption(MediaControl[i]) then
        PostCommand(MediaControl[i]);


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

procedure TOvoPlayerCtrl.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TOvoPlayerCtrl;

{$R *.res}

begin
  Application:=TOvoPlayerCtrl.Create(nil);
  Application.Run;
  Application.Free;
end.

