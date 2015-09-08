unit NetIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, BaseTypes, coreinterfaces, TcpIpServer, TcpIpClient, sockets,
  NullInterfacedObject, netprotocol,lclproc;

type

  { TWebIntf }
  TTCPRemoteDaemon = class;

  TNetIntf = class(TNullInterfacedobject)
  private
    fBackEnd: IBackEnd;
    DaemonThread: TTCPRemoteDaemon;
  public
    function Activate(BackEnd: IBackEnd): boolean;
    procedure DeActivate;
    constructor Create;
    destructor Destroy; override;
  end;

  TTCPRemoteDaemon = class(TThread)
  private
    Sock: TTcpIpServerSocket;
    fnet: TNetIntf;
  public
    constructor Create(net: TNetIntf);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TTCPRemoteThrd }

  TTCPRemoteThrd = class(TNullInterfacedThread, IObserver)
  private
    Sock: TTcpIpClientSocket;
    CSock: TSocket;
    fnet: TNetIntf;
    Data: string;
    DataSize: integer;
  private
    procedure SyncRunner;
  public
    procedure UpdateProperty(Kind: TChangedProperty);
    constructor Create(hsock: TSocket; net: TNetIntf);
    procedure Execute; override;
    Destructor Destroy; override;
  end;

implementation
uses GeneralFunc;

{ TEchoDaemon }

constructor TTCPRemoteDaemon.Create(net: TNetIntf);
begin
  inherited Create(False);
  fnet := net;
  sock := TTcpIpServerSocket.Create(5500);
  FreeOnTerminate := True;
end;

destructor TTCPRemoteDaemon.Destroy;
begin
  Sock.Free;
end;

procedure TTCPRemoteDaemon.Execute;
var
  ClientSock: TSocket;
begin
  with sock do
    begin

    // setLinger(true,10000);
    bind;
    listen;
    repeat
      if terminated then
        break;
      ClientSock := accept;
      if lastError = 0 then
        TTCPRemoteThrd.Create(ClientSock, fnet);
    until False;
    end;
end;

{ TEchoThrd }

procedure TTCPRemoteThrd.SyncRunner;
var
  Command : RExternalCommand;
  Item: integer;

begin
  Command := SplitCommand(Data);
  if not  fnet.fBackEnd.HandleExternalCommand(Command) then
    begin
      if Command.Category = CATEGORY_REQUEST then
        begin
          case Command.Command of
            INFO_ENGINE_STATE: sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_ENGINE_STATE, IntToStr(ord(fnet.fBackEnd.GetStatus)))));
            INFO_METADATA: begin
                             item := StrToInt64Def(Command.Param, -1);
                             sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata(item)))));
                           end;
            INFO_POSITION : sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_POSITION, IntToStr(fnet.fBackEnd.GetPosition))));
            INFO_VOLUME: Sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_VOLUME, IntToStr(fnet.fBackEnd.GetVolume))));
            INFO_PLAYLISTCOUNT: Sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_PLAYLISTCOUNT, IntToStr(fnet.fBackEnd.PlayListCount))));
            INFO_COVER : sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_COVER, fnet.fBackEnd.GetCoverURL)));
          end;
        end;
    end;
end;

procedure TTCPRemoteThrd.UpdateProperty(Kind: TChangedProperty);
var
  tmpstr: string;
begin
    case kind of
    cpStatus:
      begin
        if fnet.fBackEnd.GetStatus = ENGINE_PLAY then
          begin
            tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata()));
            Sock.WriteStr(EncodeString(tmpstr));
          end;
        tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_ENGINE_STATE, IntToStr(ord(fnet.fBackEnd.GetStatus)));
      end;
    cpVolume: tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_VOLUME, IntToStr(fnet.fBackEnd.GetVolume));
    cpPosition: tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_POSITION, IntToStr(fnet.fBackEnd.GetPosition));
    cpMetadata: tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata()));

    end;
  Sock.WriteStr(EncodeString(tmpstr));
end;

constructor TTCPRemoteThrd.Create(hsock: TSocket; net: TNetIntf);
begin
  inherited Create(False);
  fnet := net;
  Csock := Hsock;
  fnet.fBackEnd.Attach(self);
  FreeOnTerminate := True;
end;

procedure TTCPRemoteThrd.Execute;
var
  w: integer;
begin
  sock := TTcpIpClientSocket.Create(CSock);
    try
    with sock do
      begin
        repeat
          if terminated then
            break;
          if sock.CanRead(60000) then
            begin
              if (lastError <> 0) then
                Break;
              if waiting = 0 then
                Break;
              SetLength(Data, 4);
              Sock.Read(Data[1], 4);
              DataSize:= DecodeSize(Data);
              if DataSize < 0 then
                break;
              SetLength(Data, DataSize);
              Sock.Read(Data[1], DataSize);
              Synchronize(@SyncRunner);

              if lastError <> 0 then
                break;

            end;
        until False;
      end;
    finally
      Sock.Free;
    end;
end;

destructor TTCPRemoteThrd.Destroy;
begin
  fnet.fBackEnd.Remove(Self);
  inherited Destroy;
end;

{ TWebIntf }

function TNetIntf.Activate(BackEnd: IBackEnd): boolean;
begin
  fBackEnd := BackEnd;

  DaemonThread := TTCPRemoteDaemon.Create(self);

  if not Assigned(fBackEnd) then
    exit;

  Result := True;

end;

procedure TNetIntf.DeActivate;
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
