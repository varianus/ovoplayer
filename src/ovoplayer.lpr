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
  Forms, uMain, PlayList, PlayListManager,
  FilesSupport, MediaLibrary, GeneralFunc, decoupler,
  MultimediaKeys, uConfig, uOSD, Config, AppConsts, uMiniPlayer, uSongInfo,
  GUIBackEnd, uAbout,
  customdrawndrawers, customdrawn_ovoplayer,
  // Audio Engines
  audioengine, audioengine_dummy, AudioEngine_MPlayer,
  {$IFDEF GSTREAMER} gstreamer, audioengine_gstreamer, {$ENDIF}
  {$IFDEF VLC} PasLibVlcUnit, audioengine_vlc, {$ENDIF}
  {$IFDEF XINE} xine, audioengine_Xine,{$ENDIF}
  {$IFDEF BASS} lazdynamic_bass, audioengine_bass,{$ENDIF}
  {$IFDEF DSHOW} mediadshow, audioengine_dshow,{$ENDIF}
  {$IFDEF MEDIAFOUNDATION} mediafoundation, audioengine_mf,{$ENDIF}
  {$IFDEF OPENSOURCELIB} lazdyn_libsndfile, uos, lazdyn_mpg123, lazdyn_portaudio, audioengine_OpenLib,{$ENDIF}
  {$IFDEF UOS} UOS, uos_libsndfile, uos_mpg123, uos_portaudio, audioengine_UOS,{$ENDIF}
  {$IFDEF FFMPEG} audioengine_FFMPEG,{$ENDIF}
  // ovotag
  song, AudioTag, basetag, file_flac, file_mp3, file_wma,
  tag_wma, tag_vorbis, tag_id3v2, file_ogg, file_monkey, tag_ape,
  file_Wave,  tag_Dummy,
  id3v1genres,
  //
  UniqueInstanceRaw, CommonFunctions, file_Dummy,
   DefaultTranslator, ulicense, uCover;

{$R *.res}
begin
  defaultstyle := dsExtra2;

  CheckRestarting(Application);

  if not isAppRunning(Application) then
   begin
      Application.Title:='OVO Player';
      Application.Title:=DisplayAppName;
      Application.Initialize;
      Application.CreateForm(TfMainForm, fMainForm);
      fMainForm.show;
      Application.CreateForm(TfMiniPlayer, fMiniPlayer);
      Application.Run;
   end;
end.
