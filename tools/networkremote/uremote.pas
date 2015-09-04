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
  ExtCtrls, Spin, TcpIpClient, netprotocol, BaseTypes, basetag;

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
    edAlbum: TEdit;
    edAlbumArtist: TEdit;
    edArtist: TEdit;
    edGenre: TEdit;
    Edit1: TEdit;
    Edit2: TEdit;
    edTitle: TEdit;
    gbCommonTags: TGroupBox;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    meComment: TMemo;
    Memo1: TMemo;
    seTrack: TSpinEdit;
    seYear: TSpinEdit;
    tbConn: TToggleBox;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure tbConnChange(Sender: TObject);
  protected
    procedure DoClientReceive(Sender: TObject; const AData: string);
  private
    FClient: TTcpIpClientSocket;
    FThread: TClientThread;
    procedure TagsToMap(Tags:TCommonTags);
  public

  end;

var
  Form1: TForm1;

implementation

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
    FSocket.Read(Data[1], DataSize);
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
begin
  if tbConn.Checked then
     begin
       tbConn.Caption:= 'Disconnect';
       FClient := TTcpIpClientSocket.Create('localhost', 5500);
       FThread := TClientThread.Create(FClient);
       FThread.OnReceive := @DoClientReceive;
     end
  else
     begin
       FThread.Terminate;
       FThread.WaitFor;
       FClient.Free;
       tbConn.Caption:= 'Connect';
     end;

end;

procedure TForm1.DoClientReceive(Sender: TObject; const AData: string);
var
  r : RExternalCommand;
  tags: TCommonTags;
begin
  memo1.lines.add(AData);
  r:= SplitCommand(AData);

  if (r.Category = CATEGORY_INFORMATION) and (r.Command = INFO_METADATA) then
    begin
      tags := DecodeMetaData(r.Param);
      TagsToMap(tags);
    end;



end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FClient.WriteStr(EncodeSize(Length(Edit1.caption)) + edit1.caption);
  Edit2.Caption:=EncodeSize(Length(Edit1.caption)) + edit1.caption;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if tbConn.Checked then
    begin
       FThread.Terminate;
       FThread.WaitFor;
       FClient.Free;

    end;

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
  seYear.Value := i;

  seTrack.Value := i;

end;


end.

