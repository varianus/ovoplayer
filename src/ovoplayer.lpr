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
program ovoplayer;

{$I ovoplayer.inc}
{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, song, PlayList, PlayListManager, AudioEngine_MPlayer,
  FilesSupport, AudioTag, MediaLibrary, GeneralFunc, audioengine,
  virtualtreeview_package, audioengine_bass, PasLibVlcUnit, xine, decoupler,
  MultimediaKeys, uConfig, uOSD, Config, AppConsts, uMiniPlayer, uSongInfo,
  GUIBackEnd, uAbout, audioengine_xine, audioengine_vlc, audioengine_dummy,
  mcaselli, uniqueinstance_package, CommonFunctions,
  file_flac, file_mp3, file_wma, tag_wma, tag_vorbis, tag_id3v2, basetag,
  file_ogg, gstreamer, lazdynamic_bass, UniqueInstanceRaw,
  DefaultTranslator;

{$R *.res}
begin
  if not isAppRunning then
   begin
      Application.Title:='OVO Player';
      Application.Title:=DisplayAppName;
      Application.Initialize;
      Application.CreateForm(TfMainForm, fMainForm);
      Application.CreateForm(TfMiniPlayer, fMiniPlayer);
      Application.Run;
   end;
end.
