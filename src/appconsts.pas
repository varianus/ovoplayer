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
{$I ovoplayer.inc}
unit AppConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLVersion;

var
   AppNameServerID :string  = 'ovoplayer';

{$i ovorevision.inc}

const
  DisplayAppName = 'OvoPlayer';
  AppVersion = {$i version.inc};
  BuildDate = {$I %DATE%};
  lazVersion  = lcl_version;         // Lazarus version (major.minor.micro)
//  lazRevision = RevisionStr;         // Lazarus SVN revision
  fpcVersion  = {$I %FPCVERSION%};   // FPC version (major.minor.micro)
  TargetCPU   = {$I %FPCTARGETCPU%}; // Target CPU of FPC
  TargetOS    = {$I %FPCTARGETOS%};  // Target Operating System of FPC


  AppName  = 'ovoplayer';
  DefaultResourceDirectory = '/usr/share/' + AppName + '/';
  ResourceSubDirectory     = 'Resources';
  LastPlaylistName = 'lastplaylist.xspf';

  MediaLibraryName = 'medialibrary.db';

  MimeTypes : array [0..8] of string =(
    'application/x-ogg',
    'application/ogg',
    'audio/x-vorbis+ogg',
    'audio/x-scpls',
    'audio/x-mp3',
    'audio/x-mpeg',
    'audio/mpeg',
    'audio/x-mpegurl',
    'audio/x-flac');


Resourcestring
  rEmptyTag = '<none>';
  rMissing = '<missing>';
  rNotPlaying = 'Not Playing';
  rAllFiles     = 'All Files(%0:s)|%0:s';
  rEmbedded  = 'Embedded in file';

  rBeginCollectionScan  = 'Scanning Collection';
  rAddedTrack   = 'Added:';
  rRemovedTrack = 'Removed:';
  rFailedTrack  = 'Failed:';
  rUpdatedTrack = 'Updated:';
  rNever        = 'Never';
  rMissingConfig = 'Can''t activate Audio Engine. Please review configuration';
  rMultipleValue = '<Multiple Value>';
  rDragToChangePosition = 'Drag to change position';


implementation

Function GetAppName : String;
begin
  Result:=AppName;
end;

initialization
 OnGetApplicationName := @GetAppName;


end.