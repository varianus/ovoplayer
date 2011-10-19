unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, basetag;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    edAlbum: TEdit;
    edAlbumArtist: TEdit;
    edArtist: TEdit;
    edGenre: TEdit;
    edTitle: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    leBitRate: TLabel;
    leBPM: TLabel;
    leChannels: TLabel;
    leDuration: TLabel;
    leFileName: TLabel;
    leSampling: TLabel;
    leSize: TLabel;
    meComment: TMemo;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    seTrack: TSpinEdit;
    seYear: TSpinEdit;
    procedure ReadTag(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}
uses file_flac, file_wma, file_mp3, file_ogg;

{ TForm1 }

procedure TForm1.ReadTag(Sender: TObject);
var
  i: Integer;
  TagReader: TTagReader;
  CommonTags: TCommonTags;
  FileStream: TfileStream;
begin
  memo1.clear;
  if not OpenDialog1.Execute then exit;

  case (Sender as TButton).Tag of
    1: TagReader:= TFlacReader.Create;
    2: TagReader:= TWMAReader.Create;
    3: TagReader:= TMP3Reader.Create;
    4: TagReader:= TOGGReader.Create;
  end;

  TagReader.LoadFromFile(OpenDialog1.FileName);
  CommonTags:= TagReader.GetCommonTags;
  for i:= 0 to TagReader.Tags.Count -1 do
    begin
      Memo1.Lines.Add( TagReader.Tags.Frames[i].ID+ ' ---> '+  TagReader.Tags.Frames[i].AsString) ;
    end;

 leFileName.Caption :=CommonTags.FileName ;
 leDuration.Caption := TimeToStr(CommonTags.Duration / MSecsPerDay) ;
 leBitRate.Caption := format('%d Kbps',[TagReader.MediaProperty.BitRate]);
 leBPM.Caption := IntTostr(TagReader.MediaProperty.BPM);
 leChannels.Caption := TagReader.MediaProperty.ChannelMode;
 leSampling.Caption := format('%d Hz',[TagReader.MediaProperty.Sampling]);

 leFileName.Caption := CommonTags.FileName;
 edArtist.Caption := CommonTags.Artist;
 edAlbum.Caption := CommonTags.Album;
 edAlbumArtist.Caption := CommonTags.AlbumArtist;
 edGenre.Caption := CommonTags.Genre;
 edTitle.Caption := CommonTags.Title;
 i:=0;
 TryStrToInt(CommonTags.Year,i);
 seYear.Value := i;

 seTrack.Value := CommonTags.Track;


 if TagReader.Tags.ImageCount > 0 then
   begin
      TagReader.Tags.Images[0].Image.Position:= 0;
      FileStream:= TFileStream.Create('c:\xxxxx.jpg', fmCreate);
      FileStream.CopyFrom(TagReader.Tags.Images[0].Image, TagReader.Tags.Images[0].Image.Size);
      TagReader.Tags.Images[0].Image.Position:= 0;
      Image1.Picture.LoadFromStream(TagReader.Tags.Images[0].Image);
      FileStream.Free;
   end;

  TagReader.Free;
end;

end.

