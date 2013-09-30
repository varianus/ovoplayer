program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1,
  basetag,
  tag_vorbis, file_ogg, file_flac,
  tag_wma, file_wma,
  tag_id3v2, file_mp3, file_monkey, tag_ape, file_mp4, tag_mp4;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

