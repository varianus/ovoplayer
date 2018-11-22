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
{$I codegen.inc}
{$I backend.inc}
unit BaseTypes;

interface
type
  TEngineState = (
    ENGINE_STOP,
    ENGINE_PLAY,
    ENGINE_PAUSE,
    ENGINE_SONG_END,
    ENGINE_ON_LINE,
    ENGINE_OFF_LINE);

  TEngineCommand = (ecInvalid, ecStop, ecPrevious, ecPlay, ecNext, ecPause, ecSeek, ecCustom);

  TplSortField = (stNone, stTitle, StAlbum, stArtist, stDuration,  stTrack, stGenre,
                  stYear, stAlbumArtist, stFileName, stRating);

  TplSortDirection = (sdplAscending, sdplDiscending);
  TplRepeat = (rptNone, rptTrack, rptAlbum, rptPlayList);

  EngineParamKind = (epkString, epkInteger, epkFloat, epkPath, epkFileName);

  REngineParams = record
    Key : string;
    Value: String;
    Kind:EngineParamKind;
  end;

  AREngineParams = array of REngineParams;

  RExternalCommand = record
    Category: string;
    Command: string;
    Param: string;
  end;

implementation

end.

