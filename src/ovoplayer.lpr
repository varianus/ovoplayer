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
{$DEFINE EXTRA}
program ovoplayer;

{$I ovoplayer.inc}
{$I backend.inc}  // needed here to add units to project
{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
    {$IFDEF UseCThreads}
  cthreads,
    {$ENDIF}
  clocale,
  {$ENDIF}

  Interfaces, // this includes the LCL widgetset
  Forms, singleinstance,
  // general functions
  AppConsts, GeneralFunc, decoupler, FilesSupport,
  CommonFunctions,
  // Core player objects
  NullInterfacedObject,
  CustomSong,
  PlayList, PlayListManager,
  ExtendedInfo, MediaLibrary,
  MultimediaKeys,  Config,
  BaseTypes, coreinterfaces, GUIBackEnd,

  // Audio Engines
  audioengine, audioengine_dummy, AudioEngine_MPlayer,
  {$IFDEF GSTREAMER} gstreamer, audioengine_gstreamer, {$ENDIF}
  {$IFDEF VLC} PasLibVlcUnit, audioengine_vlc, {$ENDIF}
  {$IFDEF XINE} xine, audioengine_Xine,{$ENDIF}
  {$IFDEF BASS} lazdynamic_bass, audioengine_bass,{$ENDIF}
  {$IFDEF DSHOW} mediadshow, audioengine_dshow,{$ENDIF}
  {$IFDEF MEDIAFOUNDATION} mediafoundation, audioengine_mf,{$ENDIF}
  {$IFDEF OPENSOURCELIB} uos_libsndfile, uos_mpg123, uos_portaudio, audioengine_OpenLib,{$ENDIF}
  {$IFDEF UOS} UOS, uos_libsndfile, uos_mpg123, uos_portaudio, audioengine_UOS,{$ENDIF}
  {$IFDEF FFMPEG} ffmpeg, audioengine_FFMPEG,{$ENDIF}
  {$IFDEF LIBMPV} libmpv, audioengine_libmpv,{$ENDIF}

  // Platform related
  {$IFDEF MPRIS2} mpris2,{$ENDIF}
  {$IFDEF NOTIFYDBUS}notification,{$ENDIF}
  {$IFDEF TASKBAR_EXTENSION}taskbar_ext,{$ENDIF}

  //Other Features

  {$IFDEF NETWORK_INTF}NetIntf,{$ENDIF}

  // ovotag
  song, AudioTag, basetag,
  tag_vorbis, file_flac, file_ogg,
  id3v1genres, tag_id3v2, file_mp3,
  tag_wma,file_wma,
  file_monkey, tag_ape,
  tag_MP4, file_Mp4,
  tag_Dummy, file_Wave, file_Dummy,

    // Application forms and related
  Udm, uMain, uAbout, ulicense, uConfig, uOSD, uMiniPlayer, uSongInfo, uCover,
  lazlogger, customdrawndrawers, customdrawn_ovoplayer, DefaultTranslator,
  customdrawn, ucustomplaylist, ufrfield, playlistbuilder, netprotocol,
  ImagesSupport, netsupport, guiconfig, SimpleSingleInstance;

{$R *.res}
begin

  defaultstyle := dsExtra2;

  CheckRestarting(Application);
  // needed to output exception to a file
  Application.Flags := Application.Flags + [appNoExceptionMessages];

  Application.SingleInstanceClass:= DefaultSingleInstanceClass;
  Application.SingleInstanceEnabled:= True;
  Application.Title:='OvoPlayer';

  Application.Initialize;

 // WriteLn(Application.SingleInstance.StartResult);

  if Application.SingleInstance.StartResult <> siClient then
    begin
      Application.CreateForm(TDM, dm);
      Application.CreateForm(TfMainForm, fMainForm);
      Application.Run;
    end
  else
    begin
     Application.Free;
   end;
end.
