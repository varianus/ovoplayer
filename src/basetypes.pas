unit BaseTypes;

{$mode objfpc}{$H+}

interface
type
  TEngineState = (ENGINE_STOP,
    ENGINE_PLAY,
    ENGINE_PAUSE,
    ENGINE_SONG_END,
    ENGINE_ON_LINE);

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

implementation

end.
