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
{$I codegen.inc}
unit NetIntfws;

interface

uses
  Classes, SysUtils, BaseTypes, coreinterfaces, TcpIpServer, TcpIpWebSocket, sockets,
  netprotocol,netsupport, ssockets, sslsockets, LazLoggerBase, opensslsockets;

type

  { TWebIntf }
  TTCPRemoteDaemon = class;

  { TNetIntf }

  TNetIntf = class
  private
    FActivated: boolean;
    fBackEnd: IBackEnd;
    DaemonThread: TTCPRemoteDaemon;
    FCertificate: TFileName;
    FOnlyLocalhost: boolean;
    FPort: integer;
    FPrivateKey: TFileName;
    FUseSSL: boolean;
    procedure RestartIfActive;
    procedure SetCertificate(AValue: TFileName);
    procedure SetOnlyLocalhost(AValue: boolean);
    procedure SetPort(AValue: integer);
    procedure SetPrivateKey(AValue: TFileName);
    procedure SetUseSSL(AValue: boolean);
  public
    function Activate(BackEnd: IBackEnd): boolean;
    procedure DeActivate;
    constructor Create;
    destructor Destroy; override;
    procedure GetSocketHandler(Sender: TObject; out AHandler: TSocketHandler);
    property OnlyLocalhost: boolean read FOnlyLocalhost write SetOnlyLocalhost;
    property Port: integer read FPort write SetPort;
    property UseSSL:boolean read FUseSSL write SetUseSSL;
    property Certificate: TFileName read FCertificate write SetCertificate;
    property PrivateKey: TFileName read FPrivateKey write SetPrivateKey;
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

  { TRemoteHandler }

  TRemoteHandler = class(IObserver)
  private
    FKeepOpen: boolean;
    Sock: TTcpIpWebSocket;
    CSock: TSocket;
    fnet: TNetIntf;
    Data: string;
    DataSize: integer;
    ConnectionCfg: RConnectionCfg;
    procedure SetKeepOpen(AValue: boolean);
    procedure MessageHandler(ws: TTcpIpWebSocket; const Message: string);
  private
    property KeepOpen: boolean read FKeepOpen write SetKeepOpen;
  public
    procedure UpdateProperty(Kind: TChangedProperty);
    constructor Create(hsock: TSocket; net: TNetIntf);
    destructor Destroy; override;
  end;

implementation

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
  sock := TTcpIpServerSocket.Create(specialize IfThen<string>(Net.FOnlyLocalhost, '127.0.0.1', '0.0.0.0'), net.FPort);
  FreeOnTerminate := True;
end;

destructor TTCPRemoteDaemon.Destroy;
begin
  inherited Destroy();
end;

procedure TTCPRemoteDaemon.Execute;
var
  ClientSock: TSocket;
  ws: TRemoteHandler;
begin
  sock.bind;
  sock.listen;
  repeat
    ClientSock := sock.accept;
    if (not Terminated) and (Sock.lastError = 0) then
    try
      ws := TRemoteHandler.Create(ClientSock, fnet);
    except
    end;
  until terminated;

end;

{ TEchoThrd }

procedure TRemoteHandler.SetKeepOpen(AValue: boolean);
begin
  if FKeepOpen = AValue then Exit;

  //if AValue then
  //  fnet.fBackEnd.Attach(self)
  //else
  //  fnet.fBackEnd.Remove(self);

  FKeepOpen := AValue;

end;

procedure TRemoteHandler.MessageHandler(ws: TTcpIpWebSocket; const Message: string);
var
  Command: RExternalCommand;
  Item: integer;
  i: integer;
  fPlaylist: string;
  H, W: integer;
begin
  Data    := copy(message, 5, Length(Message));
  Command := SplitCommand(Data);
  if Command.Category = CATEGORY_CONFIG then
    case Command.Command of
      COMMAND_KEEP: KeepOpen := True;
      COMMAND_PIN: ;
      COMMAND_WANTPOS: begin
        if Command.Param = '1' then
          fnet.fBackEnd.AutoSendPosEvents(True);
        if Command.Param = '0' then
          fnet.fBackEnd.AutoSendPosEvents(False);
      end;
      COMMAND_SIZEMODE: begin
        if Command.Param = '1' then
          ConnectionCfg.SizeMode := smUTF8Char;
        if Command.Param = '0' then
          ConnectionCfg.SizeMode := smByte;
      end;
    end;

  if not fnet.fBackEnd.HandleExternalCommand(Command) then
    if Command.Category = CATEGORY_REQUEST then
      case Command.Command of
        INFO_ENGINE_STATE: sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_ENGINE_STATE, IntToStr(Ord(fnet.fBackEnd.Status))), ConnectionCfg));
        INFO_METADATA: begin
          item := StrToInt64Def(Command.Param, -1);
          sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata(item), ConnectionCfg)), ConnectionCfg));
        end;
        INFO_POSITION: sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_POSITION, IntToStr(fnet.fBackEnd.Position)), ConnectionCfg));
        INFO_VOLUME: sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_VOLUME, IntToStr(fnet.fBackEnd.Volume)), ConnectionCfg));
        INFO_PLAYLISTCOUNT: sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_PLAYLISTCOUNT, IntToStr(fnet.fBackEnd.PlayListCount)), ConnectionCfg));
        INFO_COVERURL: sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_COVERURL, fnet.fBackEnd.GetCoverURL), ConnectionCfg));
        INFO_COVERIMG: begin
          DecodeImageSize(Command.Param, W, H);
          sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_COVERIMG, fnet.fBackEnd.GetCover(W, H)), ConnectionCfg));
        end;
        INFO_PLAYLISTINDEX: sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_PLAYLISTINDEX, IntToStr(fnet.fBackEnd.GetCurrentSongIndex)), ConnectionCfg));
        INFO_FULLPLAYLIST: begin
          fPlaylist := EncodeString(IntToStr(fnet.fBackEnd.PlayListCount), ConnectionCfg);
          for i := 1 to fnet.fBackEnd.PlayListCount do
            fPlaylist := fPlaylist + EncodeMetaData(fnet.fBackEnd.GetMetadata(i), ConnectionCfg);
          sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_FULLPLAYLIST, fPlaylist), ConnectionCfg));
        end;
        INFO_LOOPING: sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_LOOPING, IntToStr(Ord(fnet.fBackEnd.GetLooping()))), ConnectionCfg));
        INFO_MUTE: sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_MUTE, IntToStr(Ord(fnet.fBackEnd.GetMute()))), ConnectionCfg));
        else
          sock.WriteString(EncodeString(BuildCommand(CATEGORY_ERROR, INFO_MESSAGE, format('UNKNOWN COMMAND %s %s', [Command.Category, Command.Command])), ConnectionCfg));
      end;
end;

procedure TRemoteHandler.UpdateProperty(Kind: TChangedProperty);
var
  tmpstr: string;
begin
  tmpstr := EmptyStr;
  case kind of
    cpStatus:
    begin
      if fnet.fBackEnd.Status = ENGINE_PLAY then
      begin
        tmpstr := BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata(), ConnectionCfg));
        sock.WriteString(EncodeString(tmpstr, ConnectionCfg));
      end;
      tmpstr := BuildCommand(CATEGORY_INFORMATION, INFO_ENGINE_STATE, IntToStr(Ord(fnet.fBackEnd.Status)));
      sock.WriteString(EncodeString(tmpstr, ConnectionCfg));
      tmpstr := BuildCommand(CATEGORY_INFORMATION, INFO_PLAYLISTINDEX, IntToStr(fnet.fBackEnd.GetCurrentSongIndex));
    end;
    cpVolume: tmpstr   := BuildCommand(CATEGORY_INFORMATION, INFO_VOLUME, IntToStr(fnet.fBackEnd.Volume));
    cpPosition,
    cpPlayPos: tmpstr  := BuildCommand(CATEGORY_INFORMATION, INFO_POSITION, IntToStr(fnet.fBackEnd.Position));
    cpMetadata: tmpstr := BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata(), ConnectionCfg));
    cpClosing: tmpstr  := BuildCommand(CATEGORY_APP, COMMAND_CLOSE);
    cpPlayList: tmpstr := BuildCommand(CATEGORY_APP, INFO_PLAYLISTCHANGE);
    cpLooping: tmpstr  := BuildCommand(CATEGORY_INFORMATION, INFO_LOOPING, IntToStr(Ord(fnet.fBackEnd.GetLooping())));
    cpMute: tmpstr     := BuildCommand(CATEGORY_INFORMATION, INFO_MUTE, IntToStr(Ord(fnet.fBackEnd.GetMute())));
  end;
  if tmpstr <> EmptyStr then
    sock.WriteString(EncodeString(tmpstr, ConnectionCfg));
end;

constructor TRemoteHandler.Create(hsock: TSocket; net: TNetIntf);
begin
  inherited Create;
  fnet  := net;
  Csock := Hsock;
  Sock:= TTcpIpWebSocket.Create(CSock, @(net.getsockethandler));
  sock.OnText := @MessageHandler;
  ConnectionCfg.SizeMode := smByte;

  if sock.Listen then
    fnet.fBackEnd.Attach(self);

end;


destructor TRemoteHandler.Destroy;
begin
  Sock.Free;
  fnet.fBackEnd.Remove(Self);
  inherited Destroy;
end;

{ TWebIntf }

procedure TNetIntf.RestartIfActive;
begin
  if Assigned(DaemonThread) then
  begin
    DeActivate;
    Activate(fBackEnd);
  end;

end;

procedure TNetIntf.SetPort(AValue: integer);
begin
  if FPort = AValue then Exit;
  FPort := AValue;
  RestartIfActive;
end;

procedure TNetIntf.SetPrivateKey(AValue: TFileName);
begin
  if FPrivateKey = AValue then Exit;
  FPrivateKey := AValue;
  RestartIfActive;
end;

procedure TNetIntf.SetUseSSL(AValue: boolean);
begin
  if FUseSSL = AValue then Exit;
  FUseSSL := AValue;
  RestartIfActive;
end;

procedure TNetIntf.SetOnlyLocalhost(AValue: boolean);
begin
  if FOnlyLocalhost = AValue then Exit;
  FOnlyLocalhost := AValue;
  if Assigned(DaemonThread) then
  RestartIfActive;
end;

procedure TNetIntf.SetCertificate(AValue: TFileName);
begin
  if FCertificate = AValue then Exit;
  FCertificate := AValue;
  RestartIfActive;
end;

function TNetIntf.Activate(BackEnd: IBackEnd): boolean;
begin
  fBackEnd := BackEnd;

  DaemonThread := TTCPRemoteDaemon.Create(self);
  Result     := Assigned(fBackEnd);
  FActivated := Result;

end;

procedure TNetIntf.DeActivate;
begin
  if Assigned(DaemonThread) then
  begin
    DaemonThread.Terminate;
    //      DaemonThread.Free;
    FActivated := False;
  end;
end;

procedure TNetIntf.GetSocketHandler(Sender: TObject; out AHandler: TSocketHandler);
var
  S: TSSLSocketHandler;
begin
  if FUseSSL then
  begin
    S := TSSLSocketHandler.GetDefaultHandler;
//    s.CertificateData := WebSocketServer.CertificateData;
    s.CertificateData.PrivateKey.FileName := FPrivateKey;
    s.CertificateData.Certificate.FileName := FCertificate;
    AHandler := s;
  end
  else
    AHandler := TSocketHandler.Create;
end;

constructor TNetIntf.Create;
begin
  FPort      := 6860;
  FOnlyLocalhost := True;
  FActivated := False;
  DaemonThread := nil;
end;

destructor TNetIntf.Destroy;
begin
  if FActivated then
    DeActivate;
  inherited Destroy;

end;

end.
