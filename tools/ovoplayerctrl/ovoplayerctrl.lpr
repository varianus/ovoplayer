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
    function InstanceRunning: Boolean;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TOvoPlayerCtrl }
const
  BaseServerId = 'tuniqueinstance_';
  Separator = '|';

function TOvoPlayerCtrl.InstanceRunning: Boolean;
var
  TempStr: String;
  i: Integer;

begin
  with TSimpleIPCClient.Create(nil) do
  try
    ServerId := BaseServerId + AppNameServerID;
    Result := ServerRunning;
    if Result then
      begin
        TempStr := '';
        for i := 1 to ParamCount do
          TempStr := TempStr + 'action:'+lowercase(ParamStr(i)) + Separator;
        Active := True;
        SendStringMessage(ParamCount, TempStr);
      end;
  finally
    Free;
  end;
end;


procedure TOvoPlayerCtrl.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
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

  InstanceRunning;

  // stop program loop
  Terminate;
end;

constructor TOvoPlayerCtrl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TOvoPlayerCtrl.Destroy;
begin
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

