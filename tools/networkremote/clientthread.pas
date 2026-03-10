unit ClientThread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  netprotocol, netsupport, TcpIpClient;
type

  TClientReceiveEvent = procedure(Sender: TObject;
    const AData: string) of object;

  TClientThread = class(TThread)
  private
    Data: string;
    FOnReceive: TClientReceiveEvent;
    FSocket: TTcpIpClientSocket;
  protected
    procedure DoReceive;
  public
    constructor Create(ASocket: TTcpIpClientSocket);
    procedure Execute; override;
    property OnReceive: TClientReceiveEvent read FOnReceive write FOnReceive;
  end;

implementation

constructor TClientThread.Create(ASocket: TTcpIpClientSocket);
begin
  FreeOnTerminate := True;
  FSocket := ASocket;
  inherited Create(False);
end;

procedure TClientThread.Execute;
var
  DataSize: integer;
  ReadCnt, Remains: integer;
begin
  while (not Terminated) do
  begin
    if (FSocket.LastError <> 0) then
      exit;

    if FSocket.Socket.Closing then
      Break;
    if not FSocket.CanRead(2000) then
      Continue;
    SetLength(Data, 4);
    FSocket.Read(Data[1], 4);
    DataSize := DecodeSize(Data);
    SetLength(Data, DataSize);
    Remains := DataSize;
    repeat
    try
      ReadCnt := FSocket.Read(Data[DataSize - Remains + 1], Remains);
      Remains := Remains - ReadCnt;
    except
      Remains := 0;
    end;
    until (Remains = 0) or (FSocket.LastError <> 0);

    if DataSize < 1 then
      Exit;
    Synchronize(@DoReceive);
  end;
end;

procedure TClientThread.DoReceive;
begin
  if Assigned(FOnReceive) then
    FOnReceive(Self, Data);
end;


end.

