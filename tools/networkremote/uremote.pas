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
unit uremote;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, ComCtrls, Buttons, tcpipwebsocket,  ClientThread,
  netprotocol, netsupport, BaseTypes, basetag, uriparser;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    cbEcho: TCheckBox;
    ComboBox1: TComboBox;
    edAlbum: TEdit;
    edAlbumArtist: TEdit;
    edArtist: TEdit;
    edGenre: TEdit;
    Edit1: TEdit;
    edtServer: TEdit;
    edTitle: TEdit;
    gbCommonTags: TGroupBox;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    meComment: TMemo;
    memoReceived: TMemo;
    memoSent: TMemo;
    Panel1: TPanel;
    seTrack: TSpinEdit;
    seYear: TSpinEdit;
    Status: TShape;
    tbConn1: TButton;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure tbConn1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure TrackBar1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  protected
    procedure DoClientReceive(Sender: ttcpipwebsocket; const AData: string);
  private
    Seeking: boolean;
    FClient: TTcpIpWebSocket;
    FThread: TClientThread;
    OutCfg: RConnectionCfg;
    InCfg: RConnectionCfg;
    fConnected: boolean;
    procedure BeginGrab;
    function CreateAppWindow: boolean;
    procedure DecodeImage(s: string);
    procedure Echo(Sent: boolean; const Message: string);
    procedure EndGrab;
    procedure HandleCommand(Command: TEngineCommand; Param: integer);
    procedure SetConnected(AValue: boolean);
    procedure TagsToMap(Tags: TCommonTags);
    procedure DecodePlaylist(s: string);
  public
    property Connected: boolean read FConnected write SetConnected;

  end;

var
  Form1: TForm1;

implementation

uses base64, Windows;

  {$R *.lfm}

var
  WindowClassAtom: ATOM; // RegisterWindowClass yields an atom if successful.
  WindowClassInfo: WNDCLASSEX; // Class info Data structure for main window.
  WinClassName: string = 'Winamp v1.x';
  hwindow: HWND;

const
  VK_MEDIA_NEXT_TRACK = 176;
  VK_MEDIA_PREV_TRACK = 177;
  VK_MEDIA_STOP = 178;
  VK_MEDIA_PLAY_PAUSE = 179;

function WinProc(hw: HWND; uMsg: UINT; wp: WPARAM; lp: LPARAM): LRESULT;
  stdcall; export;
begin

  Result := 0;
  case uMsg of
    WM_HOTKEY: Form1.HandleCommand(TEngineCommand(wp), 0);
    else
      Result := DefWindowProc(hw, uMsg, wp, lp);
  end; // Case
end; // W

{ TForm1 }


const
  RCommandString: array [TEngineCommand] of string =
    ('', COMMAND_STOP, COMMAND_PREVIOUS, COMMAND_PLAYPAUSE, COMMAND_NEXT,
    COMMAND_PAUSE, COMMAND_SEEK, '');

procedure TForm1.Echo(Sent: boolean; const Message: string);
begin
  if cbEcho.Checked then
    if Sent then
      memoSent.Lines.Add(Message)
    else
      memoReceived.Lines.Add(Message);
end;

procedure TForm1.HandleCommand(Command: TEngineCommand; Param: integer);
var
  tmpCommand: RExternalCommand;
  s: string;
begin

  tmpCommand.Category := CATEGORY_ACTION;
  tmpCommand.Command := RCommandString[Command];
  tmpCommand.Param := '';
  s := EncodeString(BuildCommand(tmpCommand), OutCfg);
  echo(True, s);
  FClient.WriteString(s);

end;

procedure TForm1.SetConnected(AValue: boolean);

var
  s: String;
begin
  if FConnected = AValue then Exit;
  FConnected := AValue;
  if fConnected then
  begin
    s := EncodeString(BuildCommand(CATEGORY_REQUEST, INFO_ENGINE_STATE), OutCfg);
    echo(True, s);
    FClient.WriteString(s);
    s := EncodeString(BuildCommand(CATEGORY_REQUEST, INFO_METADATA), OutCfg);
    echo(True, s);
    FClient.WriteString(s);
    BeginGrab;
    tbConn1.Caption    := 'Disconnect';
    Status.Brush.Color := clGreen;
  end
  else
  begin
    EndGrab;
    tbConn1.Caption    := 'Connect';
    Status.Brush.Color := clWhite;
  end;
end;

function TForm1.CreateAppWindow: boolean;

  function RegisterWindowClass: boolean;
  begin
    WindowClassInfo.cbSize := sizeof(WindowClassInfo);
    WindowClassInfo.Style := 0;
    WindowClassInfo.lpfnWndProc := @WinProc;
    WindowClassInfo.cbClsExtra := 0;
    WindowClassInfo.cbWndExtra := 0;
    WindowClassInfo.hInstance := HInstance;
    WindowClassInfo.hIcon := 0;
    WindowClassInfo.hCursor := 0;
    WindowClassInfo.hbrBackground := 0;
    WindowClassInfo.lpszMenuName := nil;
    WindowClassInfo.lpszClassName := PChar(WinClassName);
    WindowClassInfo.hIconSm := 0;
    WindowClassAtom := RegisterClassEx(WindowClassInfo);
    Result := WindowClassAtom <> 0;
  end; // RegisterWindowClass - Nested Function

begin
  Result := False;

  if not RegisterWindowClass then
    exit;

  HWindow := CreateWindowEx(WS_EX_NOACTIVATE or WS_EX_TRANSPARENT,
    PChar(WinClassName), PChar(WinClassName), Ws_popup or WS_CLIPSIBLINGS, 0,
    0, 0, 0, 0, 0, HInstance, nil);

  if HWindow <> 0 then
  begin
    ShowWindow(HWindow, SW_HIDE);
    SetWindowLongPtr(HWindow, GWL_USERDATA, PtrInt(Self));
    UpdateWindow(HWindow);
    Result := True;
  end;
end; // CreateAppWindow

procedure TForm1.BeginGrab;
begin
  CreateAppWindow;
  RegisterHotKey(hwindow, longint(ecStop), 0, VK_MEDIA_STOP);
  RegisterHotKey(hwindow, longint(ecPrevious), 0, VK_MEDIA_PREV_TRACK);
  RegisterHotKey(hwindow, longint(ecPlay), 0, VK_MEDIA_PLAY_PAUSE);
  RegisterHotKey(hwindow, longint(ecNext), 0, VK_MEDIA_NEXT_TRACK);

end;

procedure TForm1.EndGrab;
begin
  UnRegisterHotkey(hwindow, longint(ecStop));
  UnRegisterHotkey(hwindow, longint(ecPrevious));
  UnRegisterHotkey(hwindow, longint(ecPlay));
  UnRegisterHotkey(hwindow, longint(ecNext));

  DestroyWindow(hwindow);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  s: string;
begin
  s := EncodeString(BuildCommand(CATEGORY_REQUEST, INFO_POSITION), OutCfg);
  echo(True, s);
  fClient.WriteString(s);

end;

procedure TForm1.TrackBar1Change(Sender: TObject);
//var
//  s:string;
begin
  //if not Seeking then exit;
  //s:=EncodeString(BuildCommand(CATEGORY_ACTION, COMMAND_SEEK, inttostr(TrackBar1.Position)), OutCfg);
  //echo(true, s);
  //fClient.WriteString(s);
end;

procedure TForm1.TrackBar1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  Seeking := True;
  Timer1.Enabled := False;
end;

procedure TForm1.TrackBar1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  Seeking := True;
  Timer1.Enabled := True;
end;

procedure TForm1.DoClientReceive(Sender: ttcpipwebsocket; const AData: string);
var
  r: RExternalCommand;
  tags: TCommonTags;
  s: string;
begin
  echo(False, AData);
  s := Copy(Adata, 5, Length(AData));
  r := SplitCommand(s);

  if (r.Category = CATEGORY_INFORMATION) then
    case r.Command of
      INFO_METADATA: begin
        tags := DecodeMetaData(r.Param, InCfg);
        TagsToMap(tags);
      end;
      INFO_POSITION: ;//if not seeking then
      //  TrackBar1.Position:=StrToInt(r.Param);


      INFO_COVERURL: if URIToFilename(r.param, s) then
          image1.Picture.LoadFromFile(s);
      INFO_COVERIMG: if r.Param <> '' then
          DecodeImage(r.Param);


      INFO_ENGINE_STATE: begin
        ComboBox1.ItemIndex := StrToIntDef(r.Param, -1);
        case TEngineState(StrToInt(r.Param)) of
          ENGINE_PLAY: Timer1.Enabled := True;
          else
            Timer1.Enabled := False;
        end;
      end;

      INFO_FULLPLAYLIST: DecodePlaylist(r.Param);

    end;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FClient.WriteString(EncodeSize(Length(Edit1.Caption)) + edit1.Caption);
  echo(True, EncodeSize(Length(Edit1.Caption)) + edit1.Caption);
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  s: string;
begin
  s := EncodeString(BuildCommand(CATEGORY_ACTION, (Sender as TButton).Caption), OutCfg);
  echo(True, s);
  FClient.WriteString(s);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Connected then
    FClient.Free//    FThread.Terminate;
  //    FThread.WaitFor;
  ;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OutCfg.SizeMode := smByte;
  InCfg.SizeMode  := smByte;
  fConnected      := False;
  if ParamCount > 0 then
    if ParamStr(1).StartsWith('ws', True) then
      edtServer.Text := ParamStr(1);
end;

procedure TForm1.tbConn1Click(Sender: TObject);
var
  s: string;
begin
  if not Connected then
  begin
    //       FClient := TTcpIpWebSocket.Create('127.0.0.1', 6860);
    try
      FClient := TTcpIpWebSocket.Create(edtServer.Text, 'ovonetremote');
      FClient.OnText := @DoClientReceive;
      if not FClient.Connect then
      begin
        FClient.Free;
        exit;
      end;

    except
      On e: Exception do
      begin
        FClient.Free;
        exit;
      end;
    end;
    Connected := True;
  end
  else
  begin
    //   FThread.Terminate;
    //   FThread.WaitFor;
    try
      FClient.Close;
      FClient.Free;
    except
    end;
    Connected := False;
  end;

end;

procedure TForm1.TagsToMap(Tags: TCommonTags);
var
  i: integer;
begin
  //  leFileName.Caption := Tags.FileName;
  edArtist.Caption := Tags.Artist;
  edAlbum.Caption  := Tags.Album;
  edAlbumArtist.Caption := Tags.AlbumArtist;
  edGenre.Caption  := Tags.Genre;
  edTitle.Caption  := Tags.Title;
  meComment.Lines.Clear;
  meComment.Lines.Add(Tags.Comment);

  i := 0;
  TryStrToInt(Tags.Year, i);
  seYear.Value := i;

  i := 0;
  TryStrToInt(Tags.TrackString, i);

  seTrack.Value := i;

  //TrackBar1.Max:= Tags.Duration;
end;

procedure TForm1.DecodePlaylist(s: string);
var
  tmp: string;
  cnt: integer;
  i: integer;
  tags: TCommonTags;
begin
  tmp := ExtractField(s, InCfg);
  // delete(s,1,4);
  cnt := StrToInt(tmp);
  ListBox1.Clear;
  try
    for i := 0 to cnt - 1 do
    begin
      tags := DecodeMetaData(s, incfg);
      ListBox1.Items.Add(IntToStr(i) + '=' + Tags.Title);
    end;

  except
  end;

end;

procedure TForm1.DecodeImage(s: string);
var
  DecodedStream: TMemoryStream;
  EncodedStream: TStringStream;
  Decoder: TBase64DecodingStream;
begin
  EncodedStream := TStringStream.Create(S);
  DecodedStream := TMemoryStream.Create;
  Decoder := TBase64DecodingStream.Create(EncodedStream);
  DecodedStream.CopyFrom(Decoder, Decoder.Size);
  DecodedStream.Position := 0;
  Image1.Picture.LoadFromStream(DecodedStream);

  DecodedStream.Free;
  EncodedStream.Free;
  Decoder.Free;
end;

end.
