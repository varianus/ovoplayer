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
  Classes, SysUtils, BaseTypes, coreinterfaces, netprotocol, netsupport, opensslsockets, sslbase, sslsockets, ssockets,
  LazLoggerBase, fpcustwsserver, fpwebsocketserver, fpwebsocket;

type

  TRemoteHandler = class;

  { TNetIntf }
  TNetIntf = class
  private
    FActivated: boolean;
    fBackEnd: IBackEnd;
    FUseSSL: boolean;
    WebSocketServer: TWebSocketServer;
    Handler: TRemoteHandler;
    FOnlyLocalhost: boolean;
    FPort: integer;
    procedure GetSocketHandler(Sender: TObject; const _UseSSL: boolean; out AHandler: TSocketHandler);
    procedure SetOnlyLocalhost(AValue: boolean);
    procedure SetPort(AValue: integer);
    procedure SetUseSSL(AValue: boolean);
  public
    function Activate(BackEnd: IBackEnd): boolean;
    procedure DeActivate;
    constructor Create;
    destructor Destroy; override;
    property OnlyLocalhost: boolean read FOnlyLocalhost write SetOnlyLocalhost;
    property Port: integer read FPort write SetPort;
    property UseSSL: boolean read FUseSSL write SetUseSSL;
    property Activated: boolean read FActivated;
  end;

  { TTCPRemoteDaemon }

  { TRemoteHandler }

  TRemoteHandler = class(IObserver)
  private
    FKeepOpen: boolean;
    fnet: TNetIntf;
    Data: string;
    ConnectionCfg: RConnectionCfg;
    procedure SetKeepOpen(AValue: boolean);
    procedure DoMessageReceived(Sender: TObject; const aMessage: TWSMessage);
  private
    property KeepOpen: boolean read FKeepOpen write SetKeepOpen;
  public
    procedure UpdateProperty(Kind: TChangedProperty);
    constructor Create(Net: TNetIntf);
    destructor Destroy; override;
  end;

implementation

{ TEchoDaemon }

procedure TRemoteHandler.SetKeepOpen(AValue: boolean);
begin

end;

procedure TRemoteHandler.DoMessageReceived(Sender: TObject; const aMessage: TWSMessage);
var
  Command: RExternalCommand;
  Connection: TWSConnection;
  Item: integer;
  i: integer;
  fPlaylist: string;
  H, W: integer;
begin
  Connection := Sender as TWSConnection;
  Data    := copy(aMessage.AsString, 5, Length(aMessage.AsString));
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
        INFO_ENGINE_STATE: Connection.Send(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_ENGINE_STATE, IntToStr(Ord(fnet.fBackEnd.Status))), ConnectionCfg));
        INFO_METADATA: begin
          item := StrToInt64Def(Command.Param, -1);
          Connection.Send(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata(item), ConnectionCfg)), ConnectionCfg));
        end;
        INFO_POSITION: Connection.Send(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_POSITION, IntToStr(fnet.fBackEnd.Position)), ConnectionCfg));
        INFO_VOLUME: Connection.Send(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_VOLUME, IntToStr(fnet.fBackEnd.Volume)), ConnectionCfg));
        INFO_PLAYLISTCOUNT: Connection.Send(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_PLAYLISTCOUNT, IntToStr(fnet.fBackEnd.PlayListCount)), ConnectionCfg));
        INFO_COVERURL: Connection.Send(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_COVERURL, fnet.fBackEnd.GetCoverURL), ConnectionCfg));
        INFO_COVERIMG: begin
          DecodeImageSize(Command.Param, W, H);
          Connection.Send(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_COVERIMG, fnet.fBackEnd.GetCover(W, H)), ConnectionCfg));
        end;
        INFO_PLAYLISTINDEX: Connection.Send(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_PLAYLISTINDEX, IntToStr(fnet.fBackEnd.GetCurrentSongIndex)), ConnectionCfg));
        INFO_FULLPLAYLIST: begin
          fPlaylist := EncodeString(IntToStr(fnet.fBackEnd.PlayListCount), ConnectionCfg);
          for i := 1 to fnet.fBackEnd.PlayListCount do
            fPlaylist := fPlaylist + EncodeMetaData(fnet.fBackEnd.GetMetadata(i), ConnectionCfg);
          Connection.Send(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_FULLPLAYLIST, fPlaylist), ConnectionCfg));
        end;
        INFO_LOOPING: Connection.Send(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_LOOPING, IntToStr(Ord(fnet.fBackEnd.GetLooping()))), ConnectionCfg));
        INFO_MUTE: Connection.Send(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_MUTE, IntToStr(Ord(fnet.fBackEnd.GetMute()))), ConnectionCfg));
        else
          Connection.Send(EncodeString(BuildCommand(CATEGORY_ERROR, INFO_MESSAGE, format('UNKNOWN COMMAND %s %s', [Command.Category, Command.Command])), ConnectionCfg));
      end;
end;

procedure TRemoteHandler.UpdateProperty(Kind: TChangedProperty);
var
  tmpstr: string;
begin
  case kind of
    cpStatus:
    begin
      if fnet.fBackEnd.Status = ENGINE_PLAY then
      begin
        tmpstr := BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata(), ConnectionCfg));
        fnet.WebSocketServer.BroadcastMessage(EncodeString(tmpstr, ConnectionCfg));
      end;
      tmpstr := BuildCommand(CATEGORY_INFORMATION, INFO_ENGINE_STATE, IntToStr(Ord(fnet.fBackEnd.Status)));
      fnet.WebSocketServer.BroadcastMessage(EncodeString(tmpstr, ConnectionCfg));
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
  fnet.WebSocketServer.BroadcastMessage(EncodeString(tmpstr, ConnectionCfg));
end;

constructor TRemoteHandler.Create(Net: TNetIntf);
begin
  inherited Create;
  fnet := net;
  ConnectionCfg.SizeMode := smByte;
  fnet.fBackEnd.Attach(self);
end;


destructor TRemoteHandler.Destroy;
begin
  fnet.fBackEnd.Remove(Self);
  inherited Destroy;
end;

{ TWebIntf }

procedure TNetIntf.SetPort(AValue: integer);
begin
  if FPort = AValue then Exit;
  FPort := AValue;
  if Assigned(WebSocketServer) then
  begin
    DeActivate;
    Activate(fBackEnd);
  end;
end;

procedure TNetIntf.SetUseSSL(AValue: boolean);
begin
  if FUseSSL = AValue then Exit;
  FUseSSL := AValue;
  if Assigned(WebSocketServer) then
  begin
    DeActivate;
    Activate(fBackEnd);
  end;
end;

procedure TNetIntf.SetOnlyLocalhost(AValue: boolean);
begin
  if FOnlyLocalhost = AValue then Exit;
  FOnlyLocalhost := AValue;
  if Assigned(WebSocketServer) then
  begin
    DeActivate;
    Activate(fBackEnd);
  end;
end;

procedure TNetIntf.GetSocketHandler(Sender: TObject; const _UseSSL: boolean; out AHandler: TSocketHandler);
var
  S: TSSLSocketHandler;
  CK: TCertAndKey;
begin
  if _UseSSL then
  begin
    AHandler := TSSLSocketHandler.GetDefaultHandler;
    with TSSLSocketHandler(AHandler) do
    begin
      CertGenerator.HostName := 'localhost';
      SSLType := stTLSv1_2;
    end;
  end
  else
    AHandler := TSocketHandler.Create;
end;

function TNetIntf.Activate(BackEnd: IBackEnd): boolean;
begin
  fBackEnd := BackEnd;
  Handler  := TRemoteHandler.Create(self);

  WebSocketServer      := TWebSocketServer.Create(nil);
  WebSocketServer.Port := FPort;
  WebSocketServer.Host := specialize IfThen<string>(FOnlyLocalhost, '127.0.0.1', '0.0.0.0');

  if fUseSSL then
    begin
    WebSocketServer.UseSSL := True;
    //  WebSocketServer.CertificateData.HostName:='ovoplayer';
    //  WebSocketServer.CertificateData.PrivateKey.FileName := 'C:\source\ovoplayer\bin\cert\ovoplayer.key';
    // WebSocketServer.CertificateData.Certificate.FileName := 'C:\source\ovoplayer\bin\cert\ovoplayer.crt';
    end;

  WebSocketServer.OnGetSocketHandler := @GetSocketHandler;
  WebSocketServer.OnMessageReceived := @Handler.DoMessageReceived;
  //  WebSocketServer.OnControlReceived:=@FChat.DoControlReceived;
  //  WebSocketServer.OnDisconnect:=@FChat.DoDisconnect;
  WebSocketServer.ThreadedAccept := True;
  WebSocketServer.ThreadMode := wtmThread;
  Result     := Assigned(fBackEnd);
  FActivated := Result;
  WebSocketServer.Active := True;

end;

procedure TNetIntf.DeActivate;
begin
  if Assigned(WebSocketServer) then
  begin
    WebSocketServer.Free;
    FActivated := False;
  end;
end;

constructor TNetIntf.Create;
begin
  FPort      := 6860;
  FOnlyLocalhost := True;
  FActivated := False;
  WebSocketServer := nil;
end;

destructor TNetIntf.Destroy;
begin
  if FActivated then
    DeActivate;
  inherited Destroy;

end;

end.
