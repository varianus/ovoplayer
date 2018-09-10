(*
  Useful classes for TCP/IP communication.
  Copyright (c) 2013 by Silvio Clecio, Gilson Nunes Rodrigues and Waldir Paim
  Copyright (c) 2017 by Marco Caselli

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit tcpipwebsocket;

{$mode objfpc}{$H+}

interface

uses
  Classes, TcpIpBase, TcpIpUtils, TcpIpClient, SysUtils, URIParser, DynQueue, sslsockets;

type

  { TTcpIpWebSocket }
  TTcpIpWebSocket = class;
  TWSReadyState = (rsConnecting, rsOpen, rsClosing, rsClosed);

  TTcpWSText = procedure(ws: TTcpIpWebSocket; const Data: string) of object;
  TTcpWSBinary = procedure(ws: TTcpIpWebSocket; const Data: TStream) of object;


  { TcpipListenThread }

  TcpipListenThread = class(TThread)
  private
    FWebSocket: TTcpIpWebSocket;
    stream: TDynamicDataQueue;
    procedure SendText;
    procedure SendBinary;
  public
    constructor Create(websocket: TTcpIpWebSocket);
    procedure Close; overload;
    procedure Execute; override;
    destructor Destroy; override;
  end;

  TTcpIpWebSocket = class
  private
    FHandShakeDone: boolean;
    FonBinary: TTcpWSBinary;
    FOnGetSocketHandler: TOnGetSocketHandler;
    FOnText: TTcpWSText;
    fResourceName: string;
    fssl: boolean;
    key: string;
    fURI: TURI;
    fOrigin: string;
    fListener: TcpipListenThread;
    IntSocket: TTcpIpClientSocket;
    FReadyState: TWSReadyState;

    procedure Output(b: byte; const Data; len: int64; Mask: boolean = False);
    function ReadHandShake: boolean;
    function ReadSwitchHeader: boolean;
    procedure SendHandShake;
    procedure SendSwitchHeader;
    procedure SetonBinary(AValue: TTcpWSBinary);
    procedure SetOnGetSocketHandler(AValue: TOnGetSocketHandler);
    procedure SetOnText(AValue: TTcpWSText);

  public
    property OnText: TTcpWSText read FOnText write SetOnText;
    property onBinary: TTcpWSBinary read FonBinary write SetonBinary;
    function Listen: boolean;
    function Connect: boolean;
    function Write(const ABuffer; ACount: longint): longint;
    function WriteString(const AString: string): longint;
    function WriteSTream(const AStream: TStream; ACount: longint): longint;
    procedure Close;
    constructor Create(const URL: string; origin: string);
    constructor Create(const ASocket: longint);
    destructor Destroy; override;
  end;

const
  {:Constants section defining close codes}
  {:Normal valid closure, connection purpose was fulfilled}
  wsCloseNormal = 1000;
  {:Endpoint is going away (like server shutdown) }
  wsCloseShutdown = 1001;
  {:Protocol error }
  wsCloseErrorProtocol = 1002;
  {:Unknown frame data type or data type application cannot handle }
  wsCloseErrorData = 1003;
  {:Reserved }
  wsCloseReserved1 = 1004;
  {:Close received by peer but without any close code. This close code MUST NOT be sent by application. }
  wsCloseNoStatus = 1005;
  {:Abnotmal connection shutdown close code. This close code MUST NOT be sent by application. }
  wsCloseErrorClose = 1006;
  {:Received text data are not valid UTF-8. }
  wsCloseErrorUTF8 = 1007;
  {:Endpoint is terminating the connection because it has received a message that violates its policy. Generic error. }
  wsCloseErrorPolicy = 1008;
  {:Too large message received }
  wsCloseTooLargeMessage = 1009;
  {:Client is terminating the connection because it has expected the server to negotiate one or more extension, but the server didn't return them in the response message of the WebSocket handshake }
  wsCloseClientExtensionError = 1010;
  {:Server is terminating the connection because it encountered an unexpected condition that prevented it from fulfilling the request }
  wsCloseErrorServerRequest = 1011;
  {:Connection was closed due to a failure to perform a TLS handshake. This close code MUST NOT be sent by application. }
  wsCloseErrorTLS = 1015;


function EncodeHashToBase64(const s: string): string;

implementation

uses
  strutils, base64, sha1;

const
  WS_SALT_V13 = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

  {:Constants section defining what kind of data are sent from one pont to another}
  {:Continuation frame }
  wsCodeContinuation = $0;
  {:Text frame }
  wsCodeText = $1;
  {:Binary frame }
  wsCodeBinary = $2;
  {:Close frame }
  wsCodeClose = $8;
  {:Ping frame }
  wsCodePing = $9;
  {:Frame frame }
  wsCodePong = $A;

{ TTcpIpWebSocket }

function EncodeHashToBase64(const s: string): string;

var
  Digest: TSHA1Digest;
  Outstream: TStringStream;
  Encoder: TBase64EncodingStream;
begin
  Outstream := TStringStream.Create('');
  try
    Encoder := TBase64EncodingStream.Create(outstream);
    try
      Digest := SHA1String(s);
      Encoder.Write(digest, sizeof(Digest));
    finally
      Encoder.Free;
    end;
    Result := Outstream.DataString;
  finally
    Outstream.Free;
  end;
end;

function EncodeBufferBase64(const s; Len:integer): string;

var
  Outstream: TStringStream;
  Encoder: TBase64EncodingStream;
begin
  Outstream := TStringStream.Create('');
  try
    Encoder := TBase64EncodingStream.Create(outstream);
    try
      Encoder.Write(s, len);
    finally
      Encoder.Free;
    end;
    Result := Outstream.DataString;
  finally
    Outstream.Free;
  end;
end;

{ TcpipListenThread }

procedure TcpipListenThread.SendText;
var
  Data: RawByteString; //UTF8
begin
  SetLength(Data, stream.Size);
  stream.Pop(PByteArray(Data)^, stream.Size);
  SetCodePage(Data, CP_UTF8);
  if Assigned(FWebSocket.FOnText) then
    FWebSocket.FOnText(FWebSocket, Data);

end;

procedure TcpipListenThread.SendBinary;
var
  wstream: TMemoryStream;
begin
  wstream := TMemoryStream.Create;
  stream.Pop(wstream, stream.Size);
  wstream.Position := 0;
  if Assigned(FWebSocket.onBinary) then
    FWebSocket.FonBinary(FWebSocket, wstream);

  Wstream.Free;

end;

constructor TcpipListenThread.Create(websocket: TTcpIpWebSocket);
begin
  inherited Create(False);
  FWebSocket := websocket;
  FreeOnTerminate := True;
end;

procedure TcpipListenThread.Close;
begin
  FWebSocket.IntSocket.Socket.Close;
  inherited Terminate;
end;

procedure TcpipListenThread.Execute;
type
  TState = (stStart, stNext, stPayload16, stPayload64, stMask, stData);
var
  b, opcode: byte;
  L16:word;
  L64: int64;
  closecode: word;
  state: TState;
  fin, havemask: boolean;
  payloadLength: int64;
  pos: integer;
  mask: array[0..3] of byte;
  Data: RawByteString;

  procedure EndMask;
  begin
    if (payloadLength > 0) or (opcode = wsCodeClose) then
    begin
      state := stData;
      pos := 0;
    end
    else
      state := stStart;
  end;

begin
  state := stStart;
  pos := 0;
  payloadLength := 0;
  opcode := 0;
  fin := False;
  closecode := 0;
  havemask := False;

  stream := TDynamicDataQueue.Create;
  try
    while not terminated and (FWebSocket.FReadyState = rsOpen) do
    begin
      if (FWebSocket.IntSocket.Read(b, 1) <> 1) then
        break;

      fin := (b and $80) <> 0;
      if (b and $70) <> 0 then
          Exit; // reserved
       opcode := b and $0F;
       closecode := 0;

      if (FWebSocket.IntSocket.Read(b, 1) <> 1) then
         break;

      havemask := (b and $80) = $80;
      payloadLength := b and $7F;

      if (payloadLength = 126) then
        begin
          if (FWebSocket.IntSocket.Read(L16, 2) <> 2) then
             break;
          payloadLength:=BEtoN(L16);
        end;
      if (payloadLength = 127) then
        begin
          if (FWebSocket.IntSocket.Read(L64, 2) <> 2) then
             break;
          payloadLength:=BEtoN(L64);
        end;

      if havemask then
        begin
          if (FWebSocket.IntSocket.Read(mask, 4) <> 4) then
             break;
        end;
      pos:=0;
      while not terminated and (payloadlength > 0)  do
        begin
          if (FWebSocket.IntSocket.Read(b, 1) <> 1) then
               break;
          if havemask then
             b := b xor mask[pos mod 4];
          case opcode of
            wsCodeClose: begin
                           closecode := closecode shl 8 or b;
                           break;
                         end;
          else
             stream.Push(b, 1);
          end;

          Dec(payloadLength);
          Inc(pos);
       end;

       if (payloadLength = 0) then
          begin
            if fin and (opcode <> wsCodeContinuation) and (FWebSocket.FReadyState = rsOpen) then
            begin
              case opcode of
                wsCodeClose:
                begin
                  SetLength(Data, stream.Size);
                  stream.Pop(PByteArray(Data)^, stream.Size);
                  FWebSocket.Output($80+wsCodeClose, Data, Length(Data));
                  FWebSocket.FReadyState := rsClosing;
                  Break;
                end;
                wsCodeText:
                begin
                  Synchronize(@SendText);
                end;
                wsCodePing:
                begin
                  SetLength(Data, stream.Size);
                  stream.Pop(PByteArray(Data)^, stream.Size);
                  FWebSocket.Output(wsCodePong, Data, Length(Data));

                end;
                wsCodePong:
                begin
                  SetLength(Data, stream.Size);
                  stream.Pop(pAnsiChar(Data)^, stream.Size);
                end;

                wsCodeBinary:
                begin
                  Synchronize(@sendBinary);
                end;
              end;
              stream.Clear;
            end;
          end;
        end;
  finally
    stream.Free;
  end;
  if FWebSocket.FReadyState = rsOpen then // remotely closed
    FWebSocket.Close;

end;

destructor TcpipListenThread.Destroy;
begin
  inherited Destroy;
end;


procedure TTcpIpWebSocket.SendSwitchHeader;
var
  HttpResponse: TStringList;
  wrkstr: string;
begin
  HttpResponse := TStringList.Create;
  try
    HttpResponse.LineBreak := #13#10;
    HttpResponse.NameValueSeparator := ':';
    HttpResponse.Add('HTTP/1.1 101 Switching Protocols');
    HttpResponse.Values['Upgrade'] := ' websocket';
    HttpResponse.Values['Connection'] := ' upgrade';
    wrkstr := EncodeHashToBase64(key + WS_SALT_V13);

    HttpResponse.Values['Sec-WebSocket-Accept'] := wrkstr;
    HttpResponse.Add('');
    wrkstr := HttpResponse.Text;
    IntSocket.WriteStr(wrkstr);
    FHandShakeDone := True;
  finally
    HttpResponse.Free;
  end;
end;

procedure TTcpIpWebSocket.SetonBinary(AValue: TTcpWSBinary);
begin
  if FonBinary = AValue then
    Exit;
  FonBinary := AValue;
end;

procedure TTcpIpWebSocket.SetOnGetSocketHandler(AValue: TOnGetSocketHandler);
begin
  if FOnGetSocketHandler=AValue then Exit;
  FOnGetSocketHandler:=AValue;
end;

procedure TTcpIpWebSocket.SetOnText(AValue: TTcpWSText);
begin
  if FOnText = AValue then
    Exit;
  FOnText := AValue;
end;

procedure TTcpIpWebSocket.SendHandShake;
var
  HttpResponse: TStringList;
  wrkstr: string;
  g: TGUID;
begin
  HttpResponse := TStringList.Create;
  try
    HttpResponse.LineBreak := #13#10;
    HttpResponse.NameValueSeparator := ':';
    HttpResponse.Add('GET ' + fUri.Document + ' HTTP/1.1');
    HttpResponse.Values['Upgrade'] := ' websocket';
    HttpResponse.Values['origin'] := fOrigin;
    HttpResponse.Values['Connection'] := ' upgrade';
    CreateGUID(G);
    key := EncodeBufferBase64(g, 16);
    HttpResponse.Values['Sec-WebSocket-Key'] := key;
    HttpResponse.Values['Sec-WebSocket-Version'] := '13';
    HttpResponse.Add('');
    wrkstr := HttpResponse.Text;
    IntSocket.WriteStr(wrkstr);
  finally
    HttpResponse.Free;
  end;
end;

function DecodeStringBase64(const s:string;strict:boolean=false):rawbytestring;

var
  SD : String;
  Instream,
  Outstream : TmemoryStream;
  Decoder   : TBase64DecodingStream;
begin
  SD:=S;
  while Length(Sd) mod 4 > 0 do
    SD := SD + '=';
  Instream:=TStringStream.Create(SD);
  try
    Outstream:=TStringStream.Create('');
    try
      if strict then
        Decoder:=TBase64DecodingStream.Create(Instream,bdmStrict)
      else
        Decoder:=TBase64DecodingStream.Create(Instream,bdmMIME);
      try
         Outstream.CopyFrom(Decoder,Decoder.Size);
         Setlength(result, outstream.size);
         Outstream.Position := 0;
         Move(Outstream.Memory,Result[1], Outstream.Size);
      finally
        Decoder.Free;
        end;
    finally
     Outstream.Free;
     end;
  finally
    Instream.Free;
    end;
end;

function TTcpIpWebSocket.ReadHandShake: boolean;
var
  Buf: array [0..8192 - 1] of char;
  cnt: integer;
  HttpRequest: TStringList;
  wrkstr: string;
  wrkbuf: rawbytestring;

begin
  Result := False;
  cnt := IntSocket.Read(Buf, 8192);
  if cnt = 0 then
    exit;
  HttpRequest := TStringList.Create;
  try
    HttpRequest.LineBreak := #13#10;
    HttpRequest.NameValueSeparator := ':';
    HttpRequest.SetText(Buf);
    if HttpRequest.Count < 5 then
      exit;
    wrkstr := HttpRequest[0];
    if ((Pos('GET ', Uppercase(wrkstr)) = 0) or
      (Pos(' HTTP/1.1', Uppercase(wrkstr)) = 0)) then
      exit;

    Copy2SpaceDel(wrkstr);
    fResourceName := Copy2Space(wrkstr);

    wrkstr := trim(HttpRequest.Values['sec-websocket-key']);
    if wrkstr = '' then
      exit;
    wrkbuf := DecodeStringBase64(wrkstr);
    cnt:= length(wrkbuf);
    if (cnt = 16) then
      key := trim(wrkstr)
    else
      exit;

    if (LowerCase(trim(HttpRequest.Values['Upgrade'])) <> LowerCase('websocket')) or
      (pos('upgrade', LowerCase(trim(HttpRequest.Values['Connection']))) = 0) then
      exit;

    Result := True;
  finally
    HttpRequest.Free;
  end;

end;

function TTcpIpWebSocket.ReadSwitchHeader: boolean;
var
  Buf: array [0..8192 - 1] of char;
  cnt: integer;
  HttpRequest: TStringList;
  wrkstr: string;

begin
  Result := False;
  cnt := IntSocket.Read(Buf, 8192);
  if cnt = 0 then
    exit;
  HttpRequest := TStringList.Create;
  try
    HttpRequest.LineBreak := #13#10;
    HttpRequest.NameValueSeparator := ':';
    HttpRequest.SetText(Buf);
    if HttpRequest.Count < 5 then
      exit;
    wrkstr := HttpRequest[0];
    if Pos('HTTP/1.1 101 SWITCHING PROTOCOLS', Uppercase(wrkstr)) = 0 then
      exit;

    wrkstr := HttpRequest.Values['Sec-WebSocket-Accept'];
    if wrkstr = '' then
      exit;

    if EncodeHashToBase64(key + WS_SALT_V13) <> trim(wrkstr) then
      exit;

    if (LowerCase(trim(HttpRequest.Values['Upgrade'])) <> LowerCase('websocket')) or
      (pos('upgrade', LowerCase(trim(HttpRequest.Values['Connection']))) = 0) then
      exit;

    Result := True;
  finally
    HttpRequest.Free;
  end;

end;


procedure TTcpIpWebSocket.Output(b: byte; const Data; len: int64; Mask: boolean = False);
var
  lenarray: array[0..7] of byte absolute len;
  d: cardinal;
  p: Pointer;
  g: TGUID;
  bitMask: byte;
begin
  if FReadyState <> rsOpen then
     exit;
  Intsocket.Write(b, 1);
  if mask then
    bitMask := $80
  else
    bitMask := $00;

  if len < 126 then
  begin
    b := len or bitMask;
    Intsocket.Write(b, 1);
  end
  else
  if len < High(word) then
  begin
    b := 126 or bitMask;
    Intsocket.Write(b, 1);
    Intsocket.Write(lenarray[1], 1);
    Intsocket.Write(lenarray[0], 1);
  end
  else
  begin
    b := 127 or bitMask;
    Intsocket.Write(b, 1);
    Intsocket.Write(lenarray[7], 1);
    Intsocket.Write(lenarray[6], 1);
    Intsocket.Write(lenarray[5], 1);
    Intsocket.Write(lenarray[4], 1);
    Intsocket.Write(lenarray[3], 1);
    Intsocket.Write(lenarray[2], 1);
    Intsocket.Write(lenarray[1], 1);
    Intsocket.Write(lenarray[0], 1);
  end;

  if Mask then
  begin
    CreateGUID(g); // entropy
    Intsocket.Write(g.D1, SizeOf(g.D1));
    p := @Data;
    while len >= 4 do
    begin
      d := cardinal(p^) xor g.D1;
      Intsocket.Write(d, SizeOf(d));
      Inc(NativeInt(p), 4);
      Dec(len, 4);
    end;
    if len > 0 then
    begin
      Move(p^, d, len);
      d := d xor g.D1;
      Intsocket.Write(d, len);
    end;
  end
  else
    Intsocket.Write(Data, len);

end;


function TTcpIpWebSocket.Write(const ABuffer; ACount: longint): longint;
begin
  Output($80 or wsCodeBinary, ABuffer, ACount);
end;

function TTcpIpWebSocket.WriteString(const AString: string): longint;
begin
  Output($80 or wsCodeText, AString[1], Length(AString));
end;

function TTcpIpWebSocket.WriteSTream(const AStream: TStream; ACount: longint): longint;
begin
  //  Output($80 or wsCodeBinary, AStream[1], ACount);
end;


function TTcpIpWebSocket.Listen: boolean;
begin
  Result:= false;
  FReadyState := rsConnecting;
  if not FHandShakeDone then
  begin
    if ReadHandShake then
      SendSwitchHeader
    else
      exit;
  end;
  result:= true;
  FReadyState := rsOpen;
  fListener := TcpipListenThread.Create(Self);
  fListener.Start;
end;

function TTcpIpWebSocket.Connect: boolean;
begin
  result:= false;
  FReadyState := rsConnecting;
  if not FHandShakeDone then
  begin
     SendHandShake;
     if not ReadSwitchHeader then
      exit;
  end;
  Result:= true;
  FReadyState := rsOpen;
  fListener := TcpipListenThread.Create(Self);
  fListener.Start;
end;

procedure TTcpIpWebSocket.Close;
begin
  if FReadyState = rsOpen then
  begin
    FReadyState := rsClosing;
    Output($80+wsCodeClose,'',0);
    FReadyState := rsClosed;
  end;
  Self.Free;
end;

constructor TTcpIpWebSocket.Create(const URL: string; origin: string);
begin
  fURI := URIParser.ParseURI(URL);
  if fUri.protocol = 'ws' then
  begin
    Fssl := False;
    if fUri.port = 0 then
      fUri.port := 80;
  end
  else
  if fUri.protocol = 'wss' then
  begin
    Fssl := True;
    if fUri.port = 0 then
      fUri.port := 443;
  end;
  fOrigin := origin;
  IntSocket :=TTcpIpClientSocket.Create(fURI.Host, fUri.Port);
end;

constructor TTcpIpWebSocket.Create(const ASocket: longint);
begin
  IntSocket := TTcpIpClientSocket.Create(ASocket);
end;

destructor TTcpIpWebSocket.Destroy;
begin
  if Assigned(fListener) then
    fListener.Terminate;
  IntSocket.Free;
  inherited Destroy;
end;

end.
