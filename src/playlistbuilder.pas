unit playlistbuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;



type

 EditorKind =(ekText,EkDate,EKRating,ekNumber);

FieldRec = record
  Id : integer;
  FieldName : string;
  FieldLabel : string;
  Kind : EditorKind;

end;

ResourceString
  // Diplay label for fields
  RS_Filename     = 'File Name';
  RS_TrackString  = 'Track String';
  RS_Track        = 'Track';
  RS_Title        = 'Title';
  RS_Album        = 'Album';
  RS_Artist       = 'Artist';
  RS_AlbumArtist  = 'Album Artist';
  RS_Genre        = 'Genre';
  RS_year         = 'Year';
  RS_Duration     = 'Duration';
  RS_Playcount    = 'Play count';
  RS_Rating       = 'Rating';
  RS_LastPlay     = 'Date Last Played';
  RS_Added        = 'Date Added';
  RS_FileSize     = 'File Size';
  RS_FileDate     = 'File Date';

const
   FieldCount = 16;
   FieldArray : array [1..FieldCount] of FieldRec =  (
   (ID : 1; FieldName : 'Filename'; FieldLabel : RS_Filename; Kind: ekText),
   (ID : 2; FieldName : 'TrackString'; FieldLabel :RS_TrackString; Kind: ekText),
   (ID : 3; FieldName : 'Track'; FieldLabel : RS_Track; Kind: ekNumber),
   (ID : 4; FieldName : 'Title'; FieldLabel : RS_Title; Kind: ekText),
   (ID : 5; FieldName : 'Album'; FieldLabel : RS_Album; Kind: ekText),
   (ID : 6; FieldName : 'Artist'; FieldLabel : RS_Artist ; Kind: ekText),
   (ID : 7; FieldName : 'AlbumArtist'; FieldLabel : RS_AlbumArtist; Kind: ekText ),
   (ID : 8; FieldName : 'Genre'; FieldLabel : RS_Genre; Kind: ekText),
   (ID : 9; FieldName : 'year'; FieldLabel : RS_year;  Kind: ekNumber),
   (ID :10; FieldName : 'Duration'; FieldLabel : RS_Duration; Kind: ekNumber),
   (ID :11; FieldName : 'Playcount'; FieldLabel : RS_Playcount; Kind: ekNumber ),
   (ID :12; FieldName : 'Rating'; FieldLabel : RS_Rating;  Kind: EKRating),
   (ID :13; FieldName : 'LastPlay'; FieldLabel : RS_LastPlay; Kind: EkDate),
   (ID :14; FieldName : 'Added'; FieldLabel : RS_Added; Kind: EkDate),
   (ID :15; FieldName : 'FileSize'; FieldLabel : RS_FileSize;  Kind: ekNumber),
   (ID :16; FieldName : 'FileDate'; FieldLabel :RS_FileDate; Kind: EkDate)
   );

implementation

end.

