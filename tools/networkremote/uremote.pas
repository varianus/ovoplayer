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
  ExtCtrls, Spin, ComCtrls, CustomDrawnControls, myhello, TcpIpClient,
  netprotocol, netsupport, BaseTypes, basetag, uriparser;

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
    ComboBox1: TComboBox;
    edAlbum: TEdit;
    edAlbumArtist: TEdit;
    edArtist: TEdit;
    edGenre: TEdit;
    Edit1: TEdit;
    edTitle: TEdit;
    gbCommonTags: TGroupBox;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
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
    tbConn1: TToggleBox;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure tbConnChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrackBar1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    procedure DoClientReceive(Sender: TObject; const AData: string);
  private
    Seeking: boolean;
    FClient: TTcpIpClientSocket;
    FThread: TClientThread;
    OutCfg : RConnectionCfg;
    InCfg : RConnectionCfg;
    procedure DecodeImage(s: string);
    procedure TagsToMap(Tags:TCommonTags);
    procedure DecodePlaylist(s: string);
  public

  end;

var
  Form1: TForm1;

implementation
uses base64;

{$R *.lfm}

constructor TClientThread.Create(ASocket: TTcpIpClientSocket);
begin
  FreeOnTerminate := True;
  FSocket := ASocket;
  inherited Create(False);
end;

procedure TClientThread.Execute;
var
  DataSize: Integer;
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
    DataSize:= DecodeSize(Data);
    SetLength(Data, DataSize);
    Remains := DataSize;
    Repeat
       ReadCnt:=FSocket.Read(Data[DataSize-Remains+1], Remains);
       Remains:=Remains-ReadCnt;
    until Remains = 0;

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


{ TForm1 }

procedure TForm1.tbConnChange(Sender: TObject);
var
  s:string;
begin
  if tbConn1.Checked then
     begin
       tbConn1.Caption:= 'Disconnect';
       FClient := TTcpIpClientSocket.Create('127.0.0.1', 6860);
       FThread := TClientThread.Create(FClient);
       FThread.OnReceive := @DoClientReceive;
       s:=EncodeString(BuildCommand(CATEGORY_REQUEST, INFO_ENGINE_STATE), OutCfg);
       memoSent.lines.Add(s);
       FClient.WriteStr(s);
       s:=EncodeString(BuildCommand(CATEGORY_REQUEST, INFO_METADATA), OutCfg);
       memoSent.lines.Add(s);
       FClient.WriteStr(s);
     end
  else
     begin
       FThread.Terminate;
       FThread.WaitFor;
       FClient.Free;
       tbConn1.Caption:= 'Connect';
     end;

end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  s:string;
begin
  s:=EncodeString(BuildCommand(CATEGORY_REQUEST, INFO_POSITION), OutCfg);
  memoSent.lines.Add(s);
  fClient.WriteStr(s);

end;

procedure TForm1.TrackBar1Change(Sender: TObject);
var
  s:string;
begin
  if not Seeking then exit;
  s:=EncodeString(BuildCommand(CATEGORY_ACTION, COMMAND_SEEK, inttostr(TrackBar1.Position)), OutCfg);
  memoSent.lines.Add(s);
  fClient.WriteStr(s);
end;

procedure TForm1.TrackBar1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Seeking:= true;
  Timer1.Enabled:=false;
end;

procedure TForm1.TrackBar1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Seeking:= true;
  Timer1.Enabled:=true;
end;

procedure TForm1.DoClientReceive(Sender: TObject; const AData: string);
var
  r : RExternalCommand;
  tags: TCommonTags;
  s: string;
begin
  memoReceived.lines.add(AData);
  r:= SplitCommand(AData);

  if (r.Category = CATEGORY_INFORMATION) then
   case r.Command of
     INFO_METADATA : begin
                       tags := DecodeMetaData(r.Param, InCfg);
                       TagsToMap(tags);
                    end;
     INFO_POSITION : begin
                       if not seeking then
                          TrackBar1.Position:=StrToInt(r.Param);
                    end;

     INFO_COVERURL : begin
                    if URIToFilename(r.param,s) then
                       image1.Picture.LoadFromFile(s);
                  end;
     INFO_COVERIMG : begin
                     if r.Param <> ''then
                       DecodeImage(r.Param);

                  end;


     INFO_ENGINE_STATE : begin
                           ComboBox1.ItemIndex:=StrToIntDef(r.Param,-1);
                           case TEngineState(StrToInt(r.Param)) of
                             ENGINE_PLAY: Timer1.Enabled:=true;
                           else
                             Timer1.Enabled:=false;
                           end;
                         end;

     INFO_FULLPLAYLIST :begin
                           DecodePlaylist( r.Param);
                        end;

   end;


end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FClient.WriteStr(EncodeSize(Length(Edit1.caption)) + edit1.caption);
  memoSent.Lines.Add(EncodeSize(Length(Edit1.caption)) + edit1.caption);
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  s :string;
begin
  s:=EncodeString(BuildCommand(CATEGORY_ACTION, (sender as tbutton).caption), OutCfg);
  memoSent.lines.Add(s);
  FClient.WriteStr(s);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if tbConn1.Checked then
    begin
       FThread.Terminate;
       FThread.WaitFor;
       FClient.Free;

    end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OutCfg.SizeMode:=smByte;
  InCfg.SizeMode:=smByte;
end;

procedure TForm1.TagsToMap(Tags:TCommonTags);
var
  i: integer;
begin
//  leFileName.Caption := Tags.FileName;
  edArtist.Caption := Tags.Artist;
  edAlbum.Caption := Tags.Album;
  edAlbumArtist.Caption := Tags.AlbumArtist;
  edGenre.Caption := Tags.Genre;
  edTitle.Caption := Tags.Title;
  meComment.Lines.Clear;
  meComment.Lines.Add(Tags.Comment);

  i := 0;
  TryStrToInt(Tags.Year, i);
  seYear.Value := i;

  i := 0;
  TryStrToInt(Tags.TrackString, i);

  seTrack.Value := i;

  TrackBar1.Max:= Tags.Duration;
end;

procedure TForm1.DecodePlaylist(s: string);
var
  tmp:string;
  cnt: integer;
  i: integer;
  tags: TCommonTags;
begin
  tmp:= ExtractField(s, InCfg);
 // delete(s,1,4);
  cnt:= StrToInt(tmp);
  ListBox1.Clear;
  try
  for i := 0 to cnt -1 do
    begin
        tags:=DecodeMetaData(s, incfg);
        ListBox1.Items.Add(inttostr(i)+'='+Tags.Title);
    end;

  except
  end;

end;

procedure TForm1.DecodeImage(s: string);
var
  DecodedStream: TMemoryStream;
  EncodedStream: TStringStream;
  Decoder: TBase64DecodingStream;
  Output: string;
begin
  EncodedStream := TStringStream.Create(S);
  DecodedStream := TMemoryStream.Create;
  Decoder       := TBase64DecodingStream.Create(EncodedStream);
  DecodedStream.CopyFrom(Decoder, Decoder.Size);
  DecodedStream.Position:=0;
  Image1.Picture.LoadFromStream(DecodedStream);

  DecodedStream.Free;
  EncodedStream.Free;
  Decoder.Free;
end;

end.

