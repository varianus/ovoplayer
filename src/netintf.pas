unit NetIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,types, BaseTypes, coreinterfaces, TcpIpServer, TcpIpClient, sockets, NullInterfacedObject;

type

  { TWebIntf }
  TTCPRemoteDaemon = class;
  TTCPRemoteThrd = class;

  TNetIntf = class(TNullInterfacedobject, IObserver)
  private
    fBackEnd: IBackEnd;
    DaemonThread : TTCPRemoteDaemon;
  public
    function Activate(BackEnd: IBackEnd): boolean;
    procedure DeActivate;
    procedure UpdateProperty(Kind: TChangedProperty);
    constructor Create;
    destructor Destroy; override;
  end;

  TTCPRemoteDaemon = class(TThread)
  private
    Sock:TTcpIpServerSocket;
    fnet:TNetIntf;
  public
    Constructor Create(net:TNetIntf);
    Destructor Destroy; override;
    procedure Execute; override;
  end;

  TTCPRemoteThrd = class(TThread)
  private
    Sock:TTcpIpClientSocket;
    CSock: TSocket;
    fnet:TNetIntf;
  public
    Constructor Create (hsock:tSocket; net:TNetIntf);
    procedure Execute; override;
  end;

implementation

{ TEchoDaemon }

Constructor TTCPRemoteDaemon.Create(net:TNetIntf);
begin
  inherited create(false);
  fnet:=net;
  sock:=TTcpIpServerSocket.Create(5500);
  FreeOnTerminate:=true;
end;

Destructor TTCPRemoteDaemon.Destroy;
begin
  Sock.free;
end;

procedure TTCPRemoteDaemon.Execute;
var
  ClientSock:TSocket;
begin
  with sock do
    begin

     // setLinger(true,10000);
      bind;
      listen;
      repeat
        if terminated then break;
        ClientSock:=accept;
        if lastError=0 then TTCPRemoteThrd.create(ClientSock, fnet);
      until false;
    end;
end;

{ TEchoThrd }

Constructor TTCPRemoteThrd.Create(Hsock:TSocket; net:TNetIntf);
begin
  inherited create(false);
  fnet:= net;
  Csock := Hsock;
  FreeOnTerminate:=true;
end;

procedure TTCPRemoteThrd.Execute;
var
  s: ansistring;
  DataSize: integer;
  Unk: boolean;
begin
  sock:=TTcpIpClientSocket.create(CSock);
  try
    with sock do
      begin
        repeat
          if terminated then break;
          if sock.CanRead(60000) then
            begin
              if (lastError<>0) then break;
              DataSize := Sock.Waiting;
              SetLength(S,DataSize);
              Sock.Read(s[1], DataSize);
              unk := false;
              case s of
                'p' : fnet.fBackEnd.Play;
                'u' : fnet.fBackEnd.pause;
                'n' : fnet.fBackEnd.Next;
                'x' : ;
                '':;
              else
                unk:= true;
              end;
             if unk then
               Sock.WriteStr('BOH??')
             else
               Sock.WriteStr(Inttostr(fnet.fBackEnd.GetPosition));

             if lastError<>0 then break;

            end;
        until false;
      end;
  finally
    Sock.Free;
  end;
end;

{ TWebIntf }

function TNetIntf.Activate(BackEnd: IBackEnd): boolean;
begin
  fBackEnd := BackEnd;

  DaemonThread := TTCPRemoteDaemon.Create(self);

  if not Assigned(fBackEnd) then
    exit;

  fBackEnd.Attach(Self);
  Result := True;


end;

procedure TNetIntf.DeActivate;
begin
  fBackEnd.Remove(Self);
end;

procedure TNetIntf.UpdateProperty(Kind: TChangedProperty);
begin

end;

constructor TNetIntf.Create;
begin

end;

destructor TNetIntf.Destroy;
begin
  DeActivate;
  inherited Destroy;

end;

end.

