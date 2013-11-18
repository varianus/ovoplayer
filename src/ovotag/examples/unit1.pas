unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, AudioTag, basetag;

type

  { TForm1 }

  TForm1 = class(TForm)
    bOpenFile: TButton;
    bSave: TButton;
    bSave1: TButton;
    edAlbum: TEdit;
    edAlbumArtist: TEdit;
    edArtist: TEdit;
    edGenre: TEdit;
    edTitle: TEdit;
    gbCommonTags: TGroupBox;
    gbMediaInfo: TGroupBox;
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
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    seTrack: TSpinEdit;
    seYear: TSpinEdit;
    procedure bSave1Click(Sender: TObject);
    procedure bSaveAsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ReadTag(Sender: TObject);
  private
    TagReader: TTagReader;
    CommonTags: TCommonTags;
    procedure MapToTags;
    procedure MediaInfoToMap;
    procedure TagsToMap;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
uses file_flac, file_wma, file_mp3, file_ogg, file_monkey;

{ TForm1 }

procedure TForm1.ReadTag(Sender: TObject);
var
  i: integer;
  tagClass: TTagReaderClass;
  tmpsr: string;
begin
  memo1.Clear;

  if not OpenDialog1.Execute then
    exit;

  if Assigned(TagReader) then
    TagReader.Free;

  tagClass := IdentifyKind(OpenDialog1.FileName);
  TagReader := tagClass.Create;

  TagReader.LoadFromFile(OpenDialog1.FileName);
  CommonTags := TagReader.GetCommonTags;
  for i := 0 to TagReader.Tags.Count - 1 do
  begin
    tmpsr := TagReader.Tags.Frames[i].ID + ' ---> ' + TagReader.Tags.Frames[i].AsString;
    Memo1.Lines.Add(tmpsr);
  end;

  MediaInfoToMap;
  TagsToMap;

end;

procedure TForm1.MediaInfoToMap;
begin
  leDuration.Caption := TimeToStr(CommonTags.Duration / MSecsPerDay);
  leBitRate.Caption := format('%d Kbps', [TagReader.MediaProperty.BitRate]);
  leBPM.Caption := IntToStr(TagReader.MediaProperty.BPM);
  leChannels.Caption := TagReader.MediaProperty.ChannelMode;
  leSampling.Caption := format('%d Hz', [TagReader.MediaProperty.Sampling]);

end;

procedure TForm1.MapToTags;
begin
  CommonTags.FileName    := leFileName.Caption;
  CommonTags.Artist      := edArtist.Caption;
  CommonTags.Album       := edAlbum.Caption;
  CommonTags.AlbumArtist := edAlbumArtist.Caption;
  CommonTags.Genre       := edGenre.Caption;
  CommonTags.Title       := edTitle.Caption;
  CommonTags.Year        := IntTostr(seYear.Value);
  CommonTags.Track       := seTrack.Value;
end;

procedure TForm1.TagsToMap;
var
  i: integer;
  ImageStream: TMemoryStream;
begin
  leFileName.Caption := CommonTags.FileName;
  edArtist.Caption := CommonTags.Artist;
  edAlbum.Caption := CommonTags.Album;
  edAlbumArtist.Caption := CommonTags.AlbumArtist;
  edGenre.Caption := CommonTags.Genre;
  edTitle.Caption := CommonTags.Title;
  meComment.Lines.Clear;
  meComment.Lines.Add(CommonTags.Comment);

  i := 0;
  TryStrToInt(CommonTags.Year, i);
  seYear.Value := i;

  seTrack.Value := CommonTags.Track;

  if TagReader.Tags.ImageCount > 0 then
  begin
    TagReader.Tags.Images[0].Image.Position := 0;
    ImageStream := TMemoryStream.Create;
    ImageStream.CopyFrom(TagReader.Tags.Images[0].Image,
      TagReader.Tags.Images[0].Image.Size);
    TagReader.Tags.Images[0].Image.Position := 0;
    Image1.Picture.LoadFromStream(TagReader.Tags.Images[0].Image);
    ImageStream.Free;
  end;

end;

procedure TForm1.bSaveAsClick(Sender: TObject);
begin
  if TagReader = nil then
    exit;

  if not TagReader.isUpdateable then
    begin
      ShowMessage('Not implemented for this file format!');
      exit;
    end;
  SaveDialog1.FileName := ExtractFileNameOnly(TagReader.GetCommonTags.FileName) +
                          '.new' +
                          ExtractFileExt(TagReader.GetCommonTags.FileName);

  if not SaveDialog1.Execute then
    exit;

  MapToTags;
  TagReader.SetCommonTags(CommonTags);
  TagReader.SaveToFile(SaveDialog1.FileName);

end;

procedure TForm1.bSave1Click(Sender: TObject);
begin
  if not TagReader.isUpdateable then
    begin
      ShowMessage('Not implemented for this file format!');
      exit;
    end;

  MapToTags;
  TagReader.SetCommonTags(CommonTags);
  TagReader.UpdateFile;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TagReader := nil;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 if Assigned(TagReader) then
    TagReader.Free;

end;

end.
