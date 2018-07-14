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

unit NetIntfws;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseTypes, coreinterfaces, TcpIpServer, TcpIpWebSocket, sockets,
  NullInterfacedObject, netprotocol,netsupport, LazLoggerBase;

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

  { TRemoteHandler }

  TRemoteHandler = class(TNullInterfacedObject, IObserver)
  private
    FKeepOpen: boolean;
    Sock: TTcpIpWebSocket;
    CSock: TSocket;
    fnet: TNetIntf;
    Data: string;
    DataSize: integer;
    ConnectionCfg: RConnectionCfg;
    procedure SetKeepOpen(AValue: boolean);
    procedure MessageHandler(ws: TTcpIpWebSocket; const Message: String);
  private
    property KeepOpen: boolean read FKeepOpen write SetKeepOpen;
  public
    procedure UpdateProperty(Kind: TChangedProperty);
    constructor Create(hsock: TSocket; net: TNetIntf);
    Destructor Destroy; override;
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
  sock := TTcpIpServerSocket.Create('', net.FPort);
  FreeOnTerminate := True;
end;

destructor TTCPRemoteDaemon.Destroy;
begin
  Inherited Destroy();
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
      begin
        try
         ws:= TRemoteHandler.Create(ClientSock, fnet);
        except
        end;


      end;
  until terminated;

end;

{ TEchoThrd }

procedure TRemoteHandler.SetKeepOpen(AValue: boolean);
begin
  if FKeepOpen=AValue then Exit;

  //if AValue then
  //  fnet.fBackEnd.Attach(self)
  //else
  //  fnet.fBackEnd.Remove(self);

  FKeepOpen:=AValue;

end;

procedure TRemoteHandler.MessageHandler(ws: TTcpIpWebSocket; const Message:String);
var
  Command : RExternalCommand;
  Item: integer;
  i: integer;
  fPlaylist: String;
  H, W: integer;

begin
  Data:= copy(message, 5, Length(Message));
  Command := SplitCommand(data);
  if Command.Category = CATEGORY_CONFIG then
    begin
      Case Command.Command of
        COMMAND_KEEP : KeepOpen := true;
        COMMAND_PIN: ;
        COMMAND_WANTPOS: begin
                           if Command.Param = '1' then
                              fnet.fBackEnd.AutoSendPosEvents(true);
                           if Command.Param = '0' then
                              fnet.fBackEnd.AutoSendPosEvents(False);
                         end;
        COMMAND_SIZEMODE: begin
                            if Command.Param = '1' then
                              ConnectionCfg.SizeMode:=smUTF8Char;
                            if Command.Param = '0' then
                              ConnectionCfg.SizeMode:=smByte;
                          end;
      end;
    end;

 if not  fnet.fBackEnd.HandleExternalCommand(Command) then
    begin
      if Command.Category = CATEGORY_REQUEST then
        begin
          case Command.Command of
            INFO_ENGINE_STATE: sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_ENGINE_STATE, IntToStr(ord(fnet.fBackEnd.Status))),ConnectionCfg));
            INFO_METADATA: begin
                             item := StrToInt64Def(Command.Param, -1);
                             sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata(item),ConnectionCfg)),ConnectionCfg));
                           end;
            INFO_POSITION : sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_POSITION, IntToStr(fnet.fBackEnd.Position)),ConnectionCfg));
            INFO_VOLUME: sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_VOLUME, IntToStr(fnet.fBackEnd.Volume)),ConnectionCfg));
            INFO_PLAYLISTCOUNT: sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_PLAYLISTCOUNT, IntToStr(fnet.fBackEnd.PlayListCount)),ConnectionCfg));
            INFO_COVERURL : sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_COVERURL, fnet.fBackEnd.GetCoverURL),ConnectionCfg));
            INFO_COVERIMG :  begin
                                DecodeImageSize(Command.Param, W, H);
                                sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_COVERIMG, fnet.fBackEnd.GetCover(W,H)),ConnectionCfg));
                             end;
            INFO_PLAYLISTINDEX : sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_PLAYLISTINDEX, IntToStr(fnet.fBackEnd.GetCurrentSongIndex)),ConnectionCfg));
            INFO_FULLPLAYLIST : begin
                                 fPlaylist:=EncodeString(IntToStr(fnet.fBackEnd.PlayListCount),ConnectionCfg);
                                 for i := 1 to fnet.fBackEnd.PlayListCount  do
                                    fPlaylist:= fPlaylist+EncodeMetaData(fnet.fBackEnd.GetMetadata(i),ConnectionCfg);
                                 sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_FULLPLAYLIST, fPlaylist),ConnectionCfg));
                                end;
            INFO_LOOPING : sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_LOOPING, inttostr(ord(fnet.fBackEnd.GetLooping()))),ConnectionCfg));
            INFO_MUTE : sock.WriteString(EncodeString(BuildCommand(CATEGORY_INFORMATION, INFO_MUTE, inttostr(ord(fnet.fBackEnd.GetMute()))),ConnectionCfg));
          end;
        end;
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
            tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata(),ConnectionCfg));
            sock.WriteString(EncodeString(tmpstr,ConnectionCfg));
          end;
        tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_ENGINE_STATE, IntToStr(ord(fnet.fBackEnd.Status)));
        sock.WriteString(EncodeString(tmpstr,ConnectionCfg));
        tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_PLAYLISTINDEX, IntToStr(fnet.fBackEnd.GetCurrentSongIndex));
      end;
    cpVolume: tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_VOLUME, IntToStr(fnet.fBackEnd.Volume));
    cpPosition,
    cpPlayPos : tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_POSITION, IntToStr(fnet.fBackEnd.Position));
    cpMetadata: tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_METADATA, EncodeMetaData(fnet.fBackEnd.GetMetadata(),ConnectionCfg));
    cpClosing:  tmpstr:= BuildCommand(CATEGORY_APP, COMMAND_CLOSE);
    cpPlayList: tmpstr:= BuildCommand(CATEGORY_APP, INFO_PLAYLISTCHANGE);
    cpLooping: tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_LOOPING, inttostr(ord(fnet.fBackEnd.GetLooping())));
    cpMute: tmpstr:= BuildCommand(CATEGORY_INFORMATION, INFO_MUTE, inttostr(ord(fnet.fBackEnd.GetMute())));
    end;
  sock.WriteString(EncodeString(tmpstr,ConnectionCfg));
end;

constructor TRemoteHandler.Create(hsock: TSocket; net: TNetIntf);
begin
  inherited Create;
  fnet := net;
  Csock := Hsock;
  Sock:= TTcpIpWebSocket.Create(CSock);
  sock.OnText:=@MessageHandler;
  ConnectionCfg.SizeMode:=smByte;

  if sock.Listen then
    fnet.fBackEnd.Attach(self)
  else
    raise exception.create('Cannot listen on WS');

end;


destructor TRemoteHandler.Destroy;
begin
  Sock.Free;
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
  Result := Assigned(fBackEnd);
  FActivated:= Result;

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
  if FActivated then
     DeActivate;
  inherited Destroy;

end;

end.
