unit NetIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, BaseTypes, coreinterfaces, TcpIpServer, TcpIpClient, sockets,
  NullInterfacedObject, netprotocol,lclproc;

type

  { TWebIntf }
  TTCPRemoteDaemon = class;

  { TNetIntf }

  TNetIntf = class(TNullInterfacedobject)
  private
    FActivated: boolean;
    fBackEnd: IBackEnd;
    DaemonThread: TTCPRemoteDaemon;
    FPort: integer;
    procedure SetPort(AValue: integer);
  public
    function Activate(BackEnd: IBackEnd): boolean;
    procedure DeActivate;
    constructor Create;
    destructor Destroy; override;
    property Port: integer read FPort write SetPort;
    property Activated: boolean read FActivated;
  end;

  { TTCPRemoteDaemon }

  TTCPRemoteDaemon = class(TThread)
  private
    Sock: TTcpIpServerSocket;
    fnet: TNetIntf;
  protected

  public
    constructor Create(net: TNetIntf);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Terminate;
  end;

  { TTCPRemoteThrd }

  TTCPRemoteThrd = class(TNullInterfacedThread, IObserver)
  private
    FKeepOpen: boolean;
    Sock: TTcpIpClientSocket;
    CSock: TSocket;
    fnet: TNetIntf;
    Data: string;
    DataSize: integer;
    procedure SetKeepOpen(AValue: boolean);
  private
    procedure SyncRunner;
    property KeepOpen: boolean read FKeepOpen write SetKeepOpen;
  public
    procedure UpdateProperty(Kind: TChangedProperty);
    constructor Create(hsock: TSocket; net: TNetIntf);
    procedure Execute; override;
    Destructor Destroy; override;
  end;

implementation
uses GeneralFunc;

{ TEchoDaemon }

procedure TTCPRemoteDaemon.Terminate;
begin
  Sock.Socket.StopAccepting(False);
  Sock.Free;
  inherited Terminate;
end;

constructor TTCPRemoteDaemon.Create(net: TNetIntf);
begin
  inherited Create(False);
  fnet := net;
  sock := TTcpIpServerSocket.Create('', net.FPort);
  FreeOnTerminate := True;
end;

destructor TTCPRemoteDaemon.Destroy;
begin
//  Sock.free;
  Inherited Destroy();
end;

procedure TTCPRemoteDaemon.Execute;
var
  ClientSock: TSocket;
begin
  sock.bind;
  sock.listen;
  repeat
    ClientSock := sock.accept;
    if (not Terminated) and (Sock.lastError = 0) then
      TTCPRemoteThrd.Create(ClientSock, fnet);
  until terminated;
end;

{ TEchoThrd }

procedure TTCPRemoteThrd.SetKeepOpen(AValue: boolean);
begin
  if FKeepOpen=AValue then Exit;

  //if AValue then
  //  fnet.fBackEnd.Attach(self)
  //else
  //  fnet.fBackEnd.Remove(self);

  FKeepOpen:=AValue;

end;

procedure TTCPRemoteThrd.SyncRunner;
var
  Command : RExternalCommand;
  Item: integer;
  i: integer;
  fPlaylist: String;

begin
  Command := SplitCommand(Data);
  if Command.Category = CATEGORY_CONFIG then
    begin
      Case Command.Command of
        COMMAND_KEEP : KeepOpen := true;
        COMMAND_PIN: ;
      end;

      exit;
    end;

 if not  fnet.fBackEnd.HandleExternalCommand(Command) then
    begin
      if Command.Category = CATEGORY_REQUEST then
        begin
          case Command.Command of
            INFO_ENGINE_STATE: sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_ENGINE_STATE, IntToStr(ord(fnet.fBackEnd.Status)))));
            INFO_METADATA: begin
                             item := StrToInt64Def(Command.Param, -1);
                             sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata(item)))));
                           end;
            INFO_POSITION : sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_POSITION, IntToStr(fnet.fBackEnd.Position))));
            INFO_VOLUME: Sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_VOLUME, IntToStr(fnet.fBackEnd.Volume))));
            INFO_PLAYLISTCOUNT: Sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_PLAYLISTCOUNT, IntToStr(fnet.fBackEnd.PlayListCount))));
            INFO_COVERURL : sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_COVERURL, fnet.fBackEnd.GetCoverURL)));
            INFO_COVERIMG : sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_COVERIMG, fnet.fBackEnd.GetCover)));
            INFO_FULLPLAYLIST : begin
                                 fPlaylist:=EncodeString(IntToStr(fnet.fBackEnd.PlayListCount));
                                 for i := 1 to fnet.fBackEnd.PlayListCount  do
                                    fPlaylist:= fPlaylist+EncodeMetaData(fnet.fBackEnd.GetMetadata(i));
                                 Sock.WriteStr(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_FULLPLAYLIST, fPlaylist)));
                                end;
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
        if fnet.fBackEnd.Status = ENGINE_PLAY then
          begin
            tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata()));
            Sock.WriteStr(EncodeString(tmpstr));
          end;
        tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_ENGINE_STATE, IntToStr(ord(fnet.fBackEnd.Status)));
      end;
    cpVolume: tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_VOLUME, IntToStr(fnet.fBackEnd.Volume));
    cpPosition: tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_POSITION, IntToStr(fnet.fBackEnd.Position));
    cpMetadata: tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata()));
    cpClosing:  tmpstr:= BuildCommand(CATEGORY_APP, COMMAND_CLOSE);
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
  // temporary hack 
  KeepOpen := true;
  sock := TTcpIpClientSocket.Create(CSock);
    try
    with sock do
      begin
        repeat
          if terminated then
            begin
               DebugLn('GOT KILLED:', IntToStr(LastError));
               Break;
            end;

          if sock.CanRead(60000) then
            begin
              if (lastError <> 0) then
                begin
                   DebugLn('GOT NET ERROR:', IntToStr(LastError));
                   Break;
                end;
              w:=Waiting;
              if w = 0 then
                begin
                   DebugLn('GOT NO DATA:', IntToStr(LastError));
                   Break;
                end;
              SetLength(Data, 4);
              Sock.Read(Data[1], 4);
              DataSize:= DecodeSize(Data);
              if DataSize < 0 then
                begin
                   DebugLn('GOT INVALID DATA SIZE', IntToStr(LastError));
                   Break;
                end;
              SetLength(Data, DataSize);
              Sock.Read(Data[1], DataSize);
              Synchronize(@SyncRunner);

              if lastError <> 0 then
                begin
                   DebugLn('GOT NET ERROR ON REPLY:', IntToStr(LastError));
                   Break;
                end;
            end;
        until (not FKeepOpen) and (w = 0);
        DebugLn('DISconnected');
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

procedure TNetIntf.SetPort(AValue: integer);
begin
  if FPort=AValue then Exit;
  FPort:=AValue;
  if Assigned(DaemonThread) then
    begin
      DeActivate;
      Activate(fBackEnd);
    end;
end;

function TNetIntf.Activate(BackEnd: IBackEnd): boolean;
begin
  fBackEnd := BackEnd;

  DaemonThread := TTCPRemoteDaemon.Create(self);

  if not Assigned(fBackEnd) then
    exit;

  Result := True;
  FActivated:= True;

end;

procedure TNetIntf.DeActivate;
begin
  if Assigned(DaemonThread) then
    begin
      DaemonThread.Terminate;
//      DaemonThread.Free;
      FActivated:=false;
    end;
end;

constructor TNetIntf.Create;
begin
  FPort := 6860;
  FActivated := False;
  DaemonThread := nil;
end;

destructor TNetIntf.Destroy;
begin
  DeActivate;
  inherited Destroy;

end;

end.
