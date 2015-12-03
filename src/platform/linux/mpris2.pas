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

{ This unit is mostly a pascal port of mpris-interface.c
  from gnome-player project
 }


{$I ovoplayer.inc}
unit mpris2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DBUS, ctypes, BaseTypes, coreinterfaces;

type

  { TMpris2 }

  TMpris2 = class(TInterfacedobject, IObserver)
  private
    err: DBusError;
    mpris_connection: PDBusConnection;
    ret: cint;
    fBackEnd: IBackEnd;
    procedure mpris_send_signal_PlaybackStatus;
    procedure mpris_send_signal_Seeked;
    procedure mpris_send_signal_Updated_Metadata;
    procedure mpris_send_signal_VolumeChanged;
  public
    function Activate(BackEnd: IBackEnd): boolean;
    procedure DeActivate;
    procedure UpdateProperty(Kind: TChangedProperty);

    constructor Create;
    destructor Destroy; override;

  end;

implementation

uses LCLProc, AppConsts, BaseTag, uMain, URIParser, DbusExtension;

const
  MyTrue: dword = 1;
  MyFalse: dword = 0;


const
  Introspect_xml =
    '<!DOCTYPE node PUBLIC ''-//freedesktop//DTD D-BUS Object Introspection 1.0//EN''' + #10 +
    ' ''http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd''>' + #10 + '<node>' + #10 +
    '	<interface name=''org.freedesktop.DBus.Introspectable''>' + #10 + '		<method name=''Introspect''>' + #10 +
    '			<arg name=''data'' direction=''out'' type=''s''/>' + #10 + '		</method>' + #10 + '	</interface>' + #10 +
    '	<interface name=''org.freedesktop.DBus.Properties''>' + #10 + '		<method name=''Get''>' + #10 +
    '			<arg name=''interface'' direction=''in'' type=''s''/>' + #10 + '			<arg name=''property'' direction=''in'' type=''s''/>' +
    #10 + '			<arg name=''value'' direction=''out'' type=''v''/>' + #10 + '		</method>' + #10 + '		<method name=''GetAll''>' +
    #10 + '			<arg name=''interface'' direction=''in'' type=''s''/>' + #10 + '			<arg name=''properties'' direction=''out'' type=''a{sv}''/>' +
    #10 + '		</method>' + #10 + '7	</interface>' + #10 + '	<interface name=''org.mpris.MediaPlayer2''>' + #10 +
    '		<method name=''Raise''>' + #10 + '		</method>' + #10 + '		<method name=''Quit''>' + #10 + '		</method>' +
    #10 + '		<property name=''CanQuit'' type=''b'' access=''read'' />' + #10 + '		<property name=''Fullscreen'' type=''b'' access=''readwrite'' />' +
    #10 + '		<property name=''CanRaise'' type=''b'' access=''read'' />' + #10 + '		<property name=''HasTrackList'' type=''b'' access=''read''/>' +
    #10 + '		<property name=''Identity'' type=''s'' access=''read''/>' + #10 + '		<property name=''DesktopEntry'' type=''s'' access=''read''/>' +
    #10 + '		<property name=''SupportedUriSchemes'' type=''as'' access=''read''/>' + #10 +
    '		<property name=''SupportedMimeTypes'' type=''as'' access=''read''/>' + #10 + '	</interface>' + #10 +
    '	<interface name=''org.mpris.MediaPlayer2.Player''>' + #10 + '		<method name=''Next''/>' + #10 +
    '		<method name=''Previous''/>' + #10 + '		<method name=''Pause''/>' + #10 + '		<method name=''PlayPause''/>' +
    #10 + '		<method name=''Stop''/>' + #10 + '		<method name=''Play''/>' + #10 + '		<method name=''Seek''>' +
    #10 + '			<arg direction=''in'' name=''Offset'' type=''x''/>' + #10 + '		</method>' + #10 + '		<method name=''SetPosition''>' +
    #10 + '			<arg direction=''in'' name=''TrackId'' type=''o''/>' + #10 + '			<arg direction=''in'' name=''Position'' type=''x''/>' +
    #10 + '		</method>' + #10 + '		<method name=''OpenUri''>' + #10 + '			<arg direction=''in'' name=''Uri'' type=''s''/>' +
    #10 + '		</method>' + #10 + '		<signal name=''Seeked''>' + #10 + '			<arg name=''Position'' type=''x''/>' +
    #10 + '		</signal>' + #10 + '		<property name=''PlaybackStatus'' type=''s'' access=''read''/>' + #10 +
    '		<property name=''LoopStatus'' type=''s'' access=''readwrite''/>' + #10 + '		<property name=''Rate'' type=''d'' access=''readwrite''/>' +
    #10 + '		<property name=''Shuffle'' type=''b'' access=''readwrite''/>' + #10 +
    '		<property name=''Metadata'' type=''a{sv}'' access=''read''>' + #10 + '		</property>' + #10 +
    '		<property name=''Volume'' type=''d'' access=''readwrite''/>' + #10 + '		<property name=''Position'' type=''x'' access=''read''/>' +
    #10 + '		<property name=''MinimumRate'' type=''d'' access=''read''/>' + #10 +
    '		<property name=''MaximumRate'' type=''d'' access=''read''/>' + #10 + '		<property name=''CanGoNext'' type=''b'' access=''read''/>' +
    #10 + '		<property name=''CanGoPrevious'' type=''b'' access=''read''/>' + #10 + '		<property name=''CanPlay'' type=''b'' access=''read''/>' +
    #10 + '		<property name=''CanPause'' type=''b'' access=''read''/>' + #10 + '		<property name=''CanSeek'' type=''b'' access=''read''/>' +
    #10 + '		<property name=''CanControl'' type=''b'' access=''read''/>' + #10 + '	</interface>' + #10 +
    '	<interface name=''org.mpris.MediaPlayer2.TrackList''>' + #10 + '		<method name=''GetTracksMetadata''>' +
    #10 + '			<arg direction=''in'' name=''TrackIds'' type=''ao''/>' + #10 + '			<arg direction=''out'' name=''Metadata'' type=''aa{sv}''>' +
    #10 + '			</arg>' + #10 + '		</method>' + #10 + '		<method name=''AddTrack''>' + #10 +
    '			<arg direction=''in'' name=''Uri'' type=''s''/>' + #10 + '			<arg direction=''in'' name=''AfterTrack'' type=''o''/>' +
    #10 + '			<arg direction=''in'' name=''SetAsCurrent'' type=''b''/>' + #10 + '		</method>' + #10 +
    '		<method name=''RemoveTrack''>' + #10 + '			<arg direction=''in'' name=''TrackId'' type=''o''/>' + #10 +
    '		</method>' + #10 + '		<method name=''GoTo''>' + #10 + '			<arg direction=''in'' name=''TrackId'' type=''o''/>' +
    #10 + '		</method>' + #10 + '		<signal name=''TrackListReplaced''>' + #10 + '			<arg name=''Tracks'' type=''ao''/>' +
    #10 + '			<arg name=''CurrentTrack'' type=''o''/>' + #10 + '		</signal>' + #10 + '		<signal name=''TrackAdded''>' +
    #10 + '			<arg name=''Metadata'' type=''a{sv}''>' + #10 + '			</arg>' + #10 + '			<arg name=''AfterTrack'' type=''o''/>' +
    #10 + '		</signal>' + #10 + '		<signal name=''TrackRemoved''>' + #10 + '			<arg name=''TrackId'' type=''o''/>' +
    #10 + '		</signal>' + #10 + '		<signal name=''TrackMetadataChanged''>' + #10 + '			<arg name=''TrackId'' type=''o''/>' +
    #10 + '			<arg name=''Metadata'' type=''a{sv}''>' + #10 + '			</arg>' + #10 + '		</signal>' + #10 +
    '		<property name=''Tracks'' type=''ao'' access=''read''/>' + #10 + '		<property name=''CanEditTracks'' type=''b'' access=''read''/>' +
    #10 + '	</interface>' + #10 + '</node>';


const
  BUS_NAME = 'org.mpris.MediaPlayer2.ovoplayer';


{ TMpris2 }

procedure append_metadata_array(messageIter: PDBusMessageIter; Backend: IBackend);
var
  array_, dict, dict_entry, dict_val, variant_array: DBusMessageIter;
  property_: PChar;
  i_val: int64;
  s_val: PChar;
  i: integer;
  tags : TCommonTags;
  tmp: string;

begin
  Tags:= Backend.GetMetadata;
  dbus_message_iter_open_container(messageIter, DBUS_TYPE_VARIANT, 'a{sv}', @array_);
  dbus_message_iter_open_container(@array_, DBUS_TYPE_ARRAY, '{sv}', @dict);

  dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
  property_ := 'mpris:length';
  dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
  dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 'x', @dict_val);
  i_val := Tags.Duration * 1000;
  dbus_message_iter_append_basic(@dict_val, DBUS_TYPE_INT64, @i_val);
  dbus_message_iter_close_container(@dict_entry, @dict_val);
  dbus_message_iter_close_container(@dict, @dict_entry);

  dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
  property_ := ('mpris:trackid');
  dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);

  s_val := PChar('/org/mpris/MediaPlayer2/Track/' + IntToStr(tags.Track));

  dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 'o', @dict_val);
  dbus_message_iter_append_basic(@dict_val, DBUS_TYPE_OBJECT_PATH, @s_val);
  dbus_message_iter_close_container(@dict_entry, @dict_val);
  dbus_message_iter_close_container(@dict, @dict_entry);

  dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
  property_ := ('xesam:url');
  dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
  s_val := PChar(FilenameToURI(Tags.FileName));
  dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 's', @dict_val);
  dbus_message_iter_append_basic(@dict_val, DBUS_TYPE_STRING, @s_val);
  dbus_message_iter_close_container(@dict_entry, @dict_val);
  dbus_message_iter_close_container(@dict, @dict_entry);

  dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
  property_ := ('xesam:title');
  dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
  s_val := PChar(tags.title);
  dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 's', @dict_val);
  dbus_message_iter_append_basic(@dict_val, DBUS_TYPE_STRING, @s_val);
  dbus_message_iter_close_container(@dict_entry, @dict_val);
  dbus_message_iter_close_container(@dict, @dict_entry);


  dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
  property_ := ('xesam:artist');
  dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
  if tags.Artist = '' then
    s_val := ('Unknown Artist')
  else
    s_val := PChar(tags.Artist);
  dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 'as', @dict_val);
  dbus_message_iter_open_container(@dict_val, DBUS_TYPE_ARRAY, 's', @variant_array);
  dbus_message_iter_append_basic(@variant_array, DBUS_TYPE_STRING, @s_val);
  dbus_message_iter_close_container(@dict_val, @variant_array);
  dbus_message_iter_close_container(@dict_entry, @dict_val);
  dbus_message_iter_close_container(@dict, @dict_entry);

  dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
  property_ := ('mpris:artUrl');
  dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
  s_val := PChar(backend.GetCoverURL);
  dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 's', @dict_val);
  dbus_message_iter_append_basic(@dict_val, DBUS_TYPE_STRING, @s_val);
  dbus_message_iter_close_container(@dict_entry, @dict_val);
  dbus_message_iter_close_container(@dict, @dict_entry);

  dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
  property_ := ('xesam:album');
  dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
  if tags.Album = '' then
    s_val := ('Unknown Album')
  else
    s_val := PChar(tags.Album);
  dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 's', @dict_val);
  dbus_message_iter_append_basic(@dict_val, DBUS_TYPE_STRING, @s_val);
  dbus_message_iter_close_container(@dict_entry, @dict_val);
  dbus_message_iter_close_container(@dict, @dict_entry);

  dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
  property_ := ('xesam:albumArtist');
  dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
  if tags.Album = '' then
    s_val := ('Unknown Album')
  else
    s_val := PChar(tags.AlbumArtist);
  dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 's', @dict_val);
  dbus_message_iter_append_basic(@dict_val, DBUS_TYPE_STRING, @s_val);
  dbus_message_iter_close_container(@dict_entry, @dict_val);
  dbus_message_iter_close_container(@dict, @dict_entry);


  dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
  property_ := ('xesam:trackNumber');
  dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
  i := tags.Track;
  dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 'i', @dict_val);
  dbus_message_iter_append_basic(@dict_val, DBUS_TYPE_INT32, @i);
  dbus_message_iter_close_container(@dict_entry, @dict_val);
  dbus_message_iter_close_container(@dict, @dict_entry);

  dbus_message_iter_close_container(@array_, @dict);
  dbus_message_iter_close_container(messageIter, @array_);
end;

function mpris_filter_func(connection: PDBusConnection; message_: PDBusMessage; user_data: Pointer): DBusHandlerResult; cdecl;
const
  path1 = '/org/mpris/MediaPlayer2';
var
  s: PChar;
  s_val: PChar;
  x_val: int64;
  b_val: dword;
  d_Val: double;

  Error: PDBusError;
  xml: string;
  reply_message: PDBusMessage;
  sub0, sub1, sub2, sub3, sub4: DBusMessageIter;
  message_type: cint;
  mpris_connection: PDBusConnection;
  dontplaynext: boolean;
  interface_, Property_: PChar;
  BackEnd: IBackEnd;
  i: integer;
begin

  mpris_connection := TMpris2(user_data).mpris_connection;
  BackEnd := TMpris2(user_data).fBackEnd;
  message_type := dbus_message_get_type(message_);
  s_val := dbus_message_get_path(message_);
  if (strcomp(dbus_message_get_path(message_), path1) = 0) then
    begin
    //            gm_log(verbose, G_LOG_LEVEL_DEBUG, 'Path matched %s', dbus_message_get_path(message_));

    if (message_type = DBUS_MESSAGE_TYPE_SIGNAL) then
      begin
      if (strcomp(dbus_message_get_member(message_), 'Add') = 0) then
        begin
        dbus_error_init(@error);
        if (dbus_message_get_args(message_, @error, DBUS_TYPE_STRING, [@s_val, DBUS_TYPE_INVALID])) > 0 then
          begin
          if (strlen(s_val) > 0) then
            begin
            //     g_idle_add(add_to_playlist_and_play, g_strdup(s));
            end;
          end
        else
          begin
          dbus_error_free(@error);
          end;
        Result := DBUS_HANDLER_RESULT_HANDLED;
        exit;
        end;

      end
    else
      if (message_type = DBUS_MESSAGE_TYPE_METHOD_CALL) then
        begin
        if (dbus_message_is_method_call(message_, 'org.freedesktop.DBus.Introspectable', 'Introspect')) > 0 then
          begin
          xml := Introspect_xml;
          s_val := PChar(xml);
          reply_message := dbus_message_new_method_return(message_);
          dbus_message_append_args(reply_message,
            DBUS_TYPE_STRING, [@s_val, DBUS_TYPE_INVALID]);
          dbus_connection_send(mpris_connection, reply_message, nil);
          dbus_message_unref(reply_message);
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;
          end;

        if (dbus_message_is_method_call(message_, 'org.freedesktop.DBus.Properties', 'GetAll')) > 0 then
          begin
          reply_message := dbus_message_new_method_return(message_);
          dbus_message_iter_init_append(reply_message, @sub0);
          dbus_message_iter_open_container(@sub0, DBUS_TYPE_ARRAY, '{sv}', @sub1);

          // org.mpris.MediaPlayer2 properties
          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          b_val := MyTrue;
          property_ := 'CanQuit';
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'b', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_BOOLEAN, @b_val);
          dbus_message_iter_close_container(@sub2, @sub3);
          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          b_val := myTrue;
          s := 'CanRaise';
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @s);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'b', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_BOOLEAN, @b_val);
          dbus_message_iter_close_container(@sub2, @sub3);
          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          b_val := myFalse;
          s := 'CanSetFullscreen';
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @s);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'b', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_BOOLEAN, @b_val);
          dbus_message_iter_close_container(@sub2, @sub3);
          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          b_val := myFalse;
          s := 'Fullscreen';
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @s);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'b', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_BOOLEAN, @b_val);
          dbus_message_iter_close_container(@sub2, @sub3);
          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          b_val := MyFalse;
          s := 'HasTrackList';
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @s);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'b', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_BOOLEAN, @b_val);
          dbus_message_iter_close_container(@sub2, @sub3);
          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          s_val := PChar(appname);
          s := 'Identity';
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @s);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 's', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_STRING, @s_val);
          dbus_message_iter_close_container(@sub2, @sub3);
          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          s := 'DesktopEntry';
          s_val := PChar(appname);
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @s);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 's', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_STRING, @s_val);
          dbus_message_iter_close_container(@sub2, @sub3);
          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          s := 'SupportedMimeTypes';
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @s);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'as', @sub3);
          dbus_message_iter_open_container(@sub3, DBUS_TYPE_ARRAY, 's', @sub4);

          for i := 0 to 8 do
              s_val :=pchar(mimetypes[i]);
              dbus_message_iter_append_basic(@sub4, DBUS_TYPE_STRING, @s_val);

          dbus_message_iter_close_container(@sub3, @sub4);
          dbus_message_iter_close_container(@sub2, @sub3);
          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          s := 'PlaybackStatus';
          case backend.Status of
            engine_play: s_val := ('Playing');
            engine_pause: s_val := ('Paused');
            else
              s_val := ('Stopped');
            end;
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @s);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 's', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_STRING, @s_val);
          dbus_message_iter_close_container(@sub2, @sub3);
          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          property_ := ('Volume');
          d_val := BackEnd.Volume / 255;
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'd', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_DOUBLE, @d_val);
          dbus_message_iter_close_container(@sub2, @sub3);

          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          property_ := ('Rate');
          d_val := 1.0;
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'd', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_DOUBLE, @d_val);
          dbus_message_iter_close_container(@sub2, @sub3);

          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          property_ := ('MinimumRate');
          d_val := 1.0;
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'd', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_DOUBLE, @d_val);
          dbus_message_iter_close_container(@sub2, @sub3);

          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          property_ := ('MaximumRate');
          d_val := 1.0;
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'd', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_DOUBLE, @d_val);
          dbus_message_iter_close_container(@sub2, @sub3);

          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          property_ := ('Position');
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'x', @sub3);
          x_val := BackEnd.Position * 1000000;
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_INT64, @x_val);
          dbus_message_iter_close_container(@sub2, @sub3);

          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          property_ := ('CanGoNext');
          b_val := MyTrue;
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'b', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_BOOLEAN, @b_val);
          dbus_message_iter_close_container(@sub2, @sub3);

          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          property_ := ('CanGoPrevious');
          b_val := MyTrue;
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'b', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_BOOLEAN, @b_val);
          dbus_message_iter_close_container(@sub2, @sub3);

          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          property_ := ('CanPlay');
          b_val := MyTrue;
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'b', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_BOOLEAN, @b_val);
          dbus_message_iter_close_container(@sub2, @sub3);

          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          property_ := ('CanPause');
          b_val := MyTrue;
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'b', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_BOOLEAN, @b_val);
          dbus_message_iter_close_container(@sub2, @sub3);

          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          property_ := ('CanSeek');
          b_val := MyTrue;
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'b', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_BOOLEAN, @b_val);
          dbus_message_iter_close_container(@sub2, @sub3);

          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          property_ := ('CanControl');
          b_val := MyTrue;
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'b', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_BOOLEAN, @b_val);
          dbus_message_iter_close_container(@sub2, @sub3);

          dbus_message_iter_close_container(@sub1, @sub2);

          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          property_ := ('CanEditTracks');
          b_val := MyFalse;
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'b', @sub3);
          dbus_message_iter_append_basic(@sub3, DBUS_TYPE_BOOLEAN, @b_val);
          dbus_message_iter_close_container(@sub2, @sub3);

          dbus_message_iter_close_container(@sub1, @sub2);

(*                    dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
                    property_ := ('Tracks');
                    dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
                    dbus_message_iter_open_container(@sub2, DBUS_TYPE_VARIANT, 'ao', @sub3);
                    dbus_message_iter_open_container(@sub3, DBUS_TYPE_ARRAY, 'o', @sub4);

                    gtk_tree_model_get_iter_first(GTK_TREE_MODEL(playliststore), @localiter);
                    if (gtk_list_store_iter_is_valid(playliststore, @localiter)) {
                        do {
                            gtk_tree_model_get(GTK_TREE_MODEL(playliststore), @localiter, ADD_ORDER_COLUMN, @i, -1);
                            s_val := _printf('/org/mpris/MediaPlayer2/Track/%i', i);
                            dbus_message_iter_append_basic(@sub4, DBUS_TYPE_OBJECT_PATH, @s_val);
                            g_free(s_val);
                        } while (gtk_tree_model_iter_next(GTK_TREE_MODEL(playliststore), @localiter));
                    }

                    dbus_message_iter_close_container(@sub3, @sub4);
                    dbus_message_iter_close_container(@sub2, @sub3);

                    dbus_message_iter_close_container(@sub1, @sub2);        *)


          dbus_message_iter_open_container(@sub1, DBUS_TYPE_DICT_ENTRY, nil, @sub2);
          property_ := ('Metadata');
          dbus_message_iter_append_basic(@sub2, DBUS_TYPE_STRING, @property_);
          append_metadata_array(@sub2, BackEnd);
          dbus_message_iter_close_container(@sub1, @sub2);
          dbus_message_iter_close_container(@sub0, @sub1);

          dbus_connection_send(mpris_connection, reply_message, nil);
          dbus_message_unref(reply_message);
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;
          end;

        if (dbus_message_is_method_call(message_, 'org.mpris.MediaPlayer2', 'Raise')) > 0 then
          begin
          fMainForm.Show;
          reply_message := dbus_message_new_method_return(message_);
          dbus_connection_send(mpris_connection, reply_message, nil);
          dbus_message_unref(reply_message);
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;
          end;
        if (dbus_message_is_method_call(message_, 'org.mpris.MediaPlayer2', 'Quit')) > 0 then
          begin
          reply_message := dbus_message_new_method_return(message_);
          dbus_connection_send(mpris_connection, reply_message, nil);
          dbus_message_unref(reply_message);
          BackEnd.Quit;
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;
          end;
        // org.mpris.MediaPlayer2.Player Methods
        if (dbus_message_is_method_call(message_, 'org.mpris.MediaPlayer2.Player', 'Pause')) > 0 then
          begin
          BackEnd.Pause;
          reply_message := dbus_message_new_method_return(message_);
          dbus_connection_send(mpris_connection, reply_message, nil);
          dbus_message_unref(reply_message);
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;
          end;
        if (dbus_message_is_method_call(message_, 'org.mpris.MediaPlayer2.Player', 'PlayPause')) > 0 then
          begin
          if BackEnd.Status = ENGINE_PLAY then
            BackEnd.Pause
          else
            BackEnd.Play;

          reply_message := dbus_message_new_method_return(message_);
          dbus_connection_send(mpris_connection, reply_message, nil);
          dbus_message_unref(reply_message);
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;
          end;
        if (dbus_message_is_method_call(message_, 'org.mpris.MediaPlayer2.Player', 'Stop')) > 0 then
          begin
          BackEnd.Stop;
          reply_message := dbus_message_new_method_return(message_);
          dbus_connection_send(mpris_connection, reply_message, nil);
          dbus_message_unref(reply_message);
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;
          end;
        if (dbus_message_is_method_call(message_, 'org.mpris.MediaPlayer2.Player', 'Play')) > 0 then
          begin
          BackEnd.Play;
          reply_message := dbus_message_new_method_return(message_);
          dbus_connection_send(mpris_connection, reply_message, nil);
          dbus_message_unref(reply_message);
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;
          end;
        if (dbus_message_is_method_call(message_, 'org.mpris.MediaPlayer2.Player', 'Next')) > 0 then
          begin
          BackEnd.Next;
          reply_message := dbus_message_new_method_return(message_);
          dbus_connection_send(mpris_connection, reply_message, nil);
          dbus_message_unref(reply_message);
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;
          end;
        if (dbus_message_is_method_call(message_, 'org.mpris.MediaPlayer2.Player', 'Previous')) > 0 then
          begin
          BackEnd.Previous;
          reply_message := dbus_message_new_method_return(message_);
          dbus_connection_send(mpris_connection, reply_message, nil);
          dbus_message_unref(reply_message);
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;
          end;
        if (dbus_message_is_method_call(message_, 'org.mpris.MediaPlayer2.Player', 'Seek')) > 0 then
          begin
          dbus_error_init(@error);
          if (dbus_message_get_args(message_, @error, DBUS_TYPE_INT64, [@x_val, DBUS_TYPE_INVALID])) > 0 then
            begin
            BackEnd.Seek(x_val div 1000000);
            end
          else
            begin
            dbus_error_free(@error);
            end;
          reply_message := dbus_message_new_method_return(message_);
          dbus_connection_send(mpris_connection, reply_message, nil);
          dbus_message_unref(reply_message);
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;
          end;

        if (dbus_message_is_method_call(message_, 'org.mpris.MediaPlayer2.Player', 'SetPosition')) > 0 then
          begin
          dbus_error_init(@error);
          if (dbus_message_get_args(message_, @error, DBUS_TYPE_OBJECT_PATH, [@s_val, DBUS_TYPE_INT64, @x_val, DBUS_TYPE_INVALID]) > 0) then
            begin
            BackEnd.Position := x_val div 1000000;
            end
          else
            begin
            dbus_error_free(@error);
            end;
          reply_message := dbus_message_new_method_return(message_);
          dbus_connection_send(mpris_connection, reply_message, nil);
          dbus_message_unref(reply_message);
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;
          end;


        if (dbus_message_is_method_call(message_, 'org.mpris.MediaPlayer2.Player', 'OpenUri')) > 0 then
          begin

          dbus_error_init(@error);
          if (dbus_message_get_args(message_, @error, DBUS_TYPE_STRING, [@s, DBUS_TYPE_INVALID]) > 0) then
            begin
            if (strlen(s) > 0) then
              begin
              BackEnd.OpenURI(string(s));
              end;

            end
          else
            begin
            dbus_error_free(@error);
            end;
          reply_message := dbus_message_new_method_return(message_);
          dbus_connection_send(mpris_connection, reply_message, nil);
          dbus_message_unref(reply_message);
          Result := DBUS_HANDLER_RESULT_HANDLED;
          exit;
          end;


        if (dbus_message_is_method_call(message_, 'org.freedesktop.DBus.Properties', 'Set')) > 0 then
          begin
          dbus_message_iter_init(message_, @sub0);
          if (dbus_message_iter_get_arg_type(@sub0) = DBUS_TYPE_STRING) then
            begin
            dbus_message_iter_get_basic(@sub0, @interface_);
            if (dbus_message_iter_next(@sub0) and dbus_message_iter_get_arg_type(@sub0) = DBUS_TYPE_STRING) then
              begin
              dbus_message_iter_get_basic(@sub0, @property_);

              if (strcomp(property_, 'Volume') = 0) then
                begin
                dbus_message_iter_next(@sub0);
                // this is a variant, so we have to open its container
                dbus_message_iter_recurse(@sub0, @sub1);
                dbus_message_iter_get_basic(@sub1, @d_val);
                BackEnd.Volume := (trunc(d_val * 255));
                end;
              end;
            reply_message := dbus_message_new_method_return(message_);
            dbus_connection_send(mpris_connection, reply_message, nil);
            dbus_message_unref(reply_message);
            Result := DBUS_HANDLER_RESULT_HANDLED;
            exit;
            end;
          end;

        if (dbus_message_is_method_call(message_, 'org.freedesktop.DBus.Properties', 'Get')) > 0 then
          begin

          dbus_message_iter_init(message_, @sub0);
          if (dbus_message_iter_get_arg_type(@sub0) = DBUS_TYPE_STRING) then
            begin
            dbus_message_iter_get_basic(@sub0, @interface_);
            if (dbus_message_iter_next(@sub0) > 0) and (dbus_message_iter_get_arg_type(@sub0) = DBUS_TYPE_STRING) then
              begin
              dbus_message_iter_get_basic(@sub0, @property_);
              if (strcomp(property_, 'PlaybackStatus') = 0) then
                begin
                case backend.Status of
                  engine_play: s_val := ('Playing');
                  engine_pause: s_val := ('Paused');
                  else
                    s_val := ('Stopped');
                  end;

                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                dbus_message_iter_open_container(@sub0, DBUS_TYPE_VARIANT, 's', @sub1);
                dbus_message_iter_append_basic(@sub1, DBUS_TYPE_STRING, @s_val);
                dbus_message_iter_close_container(@sub0, @sub1);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;

              if (strcomp(property_, 'Position') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                //dbus_message_iter_open_container(@sub0, DBUS_TYPE_VARIANT, 'x', @sub1);
                x_val := BackEnd.Position * 1000000;
                dbus_message_iter_append_basic(@sub0, DBUS_TYPE_INT64, @x_val);
                //dbus_message_iter_close_container(@sub0, @sub1);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;

              if (strcomp(property_, 'Volume') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                //dbus_message_iter_open_container(@sub0, DBUS_TYPE_VARIANT, 'd', @sub1);
                d_val := backend.Volume / 255;
                dbus_message_iter_append_basic(@sub0, DBUS_TYPE_DOUBLE, @d_val);
                //dbus_message_iter_close_container(@sub0, @sub1);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;

              if (strcomp(property_, 'Rate') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                d_val := 1.0;
                dbus_message_iter_append_basic(@sub0, DBUS_TYPE_DOUBLE, @d_val);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;

              if (strcomp(property_, 'MinimumRate') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                d_val := 1.0;
                dbus_message_iter_append_basic(@sub0, DBUS_TYPE_DOUBLE, @d_val);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;

              if (strcomp(property_, 'MaximumRate') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                d_val := 1.0;
                dbus_message_iter_append_basic(@sub0, DBUS_TYPE_DOUBLE, @d_val);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;

              if (strcomp(property_, 'Identity') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                dbus_message_iter_open_container(@sub0, DBUS_TYPE_VARIANT, 's', @sub1);
                s_val := PChar(appname);
                dbus_message_iter_append_basic(@sub1, DBUS_TYPE_STRING, @s_val);
                dbus_message_iter_close_container(@sub0, @sub1);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;

              if (strcomp(property_, 'DesktopEntry') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                dbus_message_iter_open_container(@sub0, DBUS_TYPE_VARIANT, 's', @sub1);
                s_val := PChar(appname);
                dbus_message_iter_append_basic(@sub1, DBUS_TYPE_STRING, @s_val);
                dbus_message_iter_close_container(@sub0, @sub1);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;

              if (strcomp(property_, 'CanRaise') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                dbus_message_iter_open_container(@sub0, DBUS_TYPE_VARIANT, 'b', @sub1);
                b_val := myTrue;
                dbus_message_iter_append_basic(@sub1, DBUS_TYPE_BOOLEAN, @b_val);
                dbus_message_iter_close_container(@sub0, @sub1);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;

              if (strcomp(property_, 'CanQuit') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                dbus_message_iter_open_container(@sub0, DBUS_TYPE_VARIANT, 'b', @sub1);
                b_val := myTrue;
                dbus_message_iter_append_basic(@sub1, DBUS_TYPE_BOOLEAN, @b_val);
                dbus_message_iter_close_container(@sub0, @sub1);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;

              if (strcomp(property_, 'CanSeek') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                dbus_message_iter_open_container(@sub0, DBUS_TYPE_VARIANT, 'b', @sub1);
                b_val := myTrue;
                dbus_message_iter_append_basic(@sub1, DBUS_TYPE_BOOLEAN, @b_val);
                dbus_message_iter_close_container(@sub0, @sub1);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;

              if (strcomp(property_, 'CanGoNext') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                dbus_message_iter_open_container(@sub0, DBUS_TYPE_VARIANT, 'b', @sub1);
                b_val := myTrue;
                dbus_message_iter_append_basic(@sub1, DBUS_TYPE_BOOLEAN, @b_val);
                dbus_message_iter_close_container(@sub0, @sub1);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;

              if (strcomp(property_, 'CanGoPrevious') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                dbus_message_iter_open_container(@sub0, DBUS_TYPE_VARIANT, 'b', @sub1);
                b_val := myTrue;
                dbus_message_iter_append_basic(@sub1, DBUS_TYPE_BOOLEAN, @b_val);
                dbus_message_iter_close_container(@sub0, @sub1);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;

                end;

              if (strcomp(property_, 'HasTrackList') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                dbus_message_iter_open_container(@sub0, DBUS_TYPE_VARIANT, 'b', @sub1);
                b_val := MyFalse;
                dbus_message_iter_append_basic(@sub1, DBUS_TYPE_BOOLEAN, @b_val);
                dbus_message_iter_close_container(@sub0, @sub1);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;

              if (strcomp(property_, 'CanEditTracks') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                dbus_message_iter_open_container(@sub0, DBUS_TYPE_VARIANT, 'b', @sub1);
                b_val := myFalse;
                dbus_message_iter_append_basic(@sub1, DBUS_TYPE_BOOLEAN, @b_val);
                dbus_message_iter_close_container(@sub0, @sub1);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;
                                              (*
                            if (strcomp(property_, 'Tracks') = 0) then begin
                                reply_message := dbus_message_new_method_return(message_);
                                dbus_message_iter_init_append(reply_message, @sub0);
                                dbus_message_iter_open_container(@sub0, DBUS_TYPE_ARRAY, 'o', @sub1);

                                gtk_tree_model_get_iter_first(GTK_TREE_MODEL(playliststore), @localiter);
                                if (gtk_list_store_iter_is_valid(playliststore, @localiter)) then begin
                                    do then begin
                                        gtk_tree_model_get(GTK_TREE_MODEL(playliststore), @localiter, ADD_ORDER_COLUMN,
                                                           @i, -1);
                                        s_val = g_strdup_printf('/org/mpris/MediaPlayer2/Track/%i', i);
                                        dbus_message_iter_append_basic(@sub1, DBUS_TYPE_OBJECT_PATH, @s_val);
                                        g_free(s_val);
                                    end; while (gtk_tree_model_iter_next(GTK_TREE_MODEL(playliststore), @localiter));
                                end;

                                dbus_message_iter_close_container(@sub0, @sub1);
                                dbus_connection_send(mpris_connection, reply_message, nil);
                                dbus_message_unref(reply_message);
                                result := DBUS_HANDLER_RESULT_HANDLED; exit;
                            end;

                                                  *)
              if (strcomp(property_, 'Metadata') = 0) then
                begin
                reply_message := dbus_message_new_method_return(message_);
                dbus_message_iter_init_append(reply_message, @sub0);
                append_metadata_array(@sub0, BackEnd);
                dbus_connection_send(mpris_connection, reply_message, nil);
                dbus_message_unref(reply_message);
                Result := DBUS_HANDLER_RESULT_HANDLED;
                exit;
                end;
              end;

            end;
          end;

        Result := DBUS_HANDLER_RESULT_NOT_YET_HANDLED;

        end;
    end;
end;

function TMpris2.Activate(BackEnd: IBackEnd): boolean;
var
  Error: DBusError;
  Match, Path: PChar;
begin
  fBackEnd := BackEnd;
  if not Assigned(fBackEnd) then
    exit;

  dbus_error_init(@error);
  mpris_connection := dbus_bus_get(DBUS_BUS_SESSION, @error);
  dbus_connection_setup_with_g_main(mpris_connection, nil);
  if (mpris_connection = nil) then
    begin
      DebugLn(format('Failed to open connection to %s message bus: %s', ['session', error.message]));
      dbus_error_free(@error);
      Result := False;
      exit;
    end;

  match := 'type=''signal'',interface=''org.mpris.MediaPlayer2''';
  dbus_bus_add_match(mpris_connection, match, @error);
  dbus_error_free(@error);

  match := 'type=''signal'',interface=''org.mpris.MediaPlayer2.Player''';
  dbus_bus_add_match(mpris_connection, match, @error);
  dbus_error_free(@error);

  path := BUS_NAME;
  ret := dbus_bus_request_name(mpris_connection, path, 0, nil);

  dbus_connection_add_filter(mpris_connection, @mpris_filter_func, self, nil);

  dbus_connection_flush(mpris_connection);

  fBackEnd.Attach(Self);
  Result := True;

end;

procedure TMpris2.DeActivate;
begin
  if Assigned(mpris_connection) then
    begin
     dbus_connection_unref(mpris_connection);
     mpris_connection := nil;
     fBackEnd.Remove(Self);
    end;
end;

procedure TMpris2.UpdateProperty(Kind: TChangedProperty);
begin
  case kind of
    cpStatus:
      begin
        mpris_send_signal_Updated_Metadata;
        mpris_send_signal_PlaybackStatus;
      end;
    cpVolume: mpris_send_signal_VolumeChanged;
    cpPosition: mpris_send_signal_Seeked;
    cpMetadata: mpris_send_signal_Updated_Metadata;
    end;
end;

procedure TMpris2.mpris_send_signal_PlaybackStatus;
var
  message: PDBusMessage;
  iter, dict, dict_entry, dict_val: DBusMessageIter;
  path, interface_, property_, s_val: PChar;

begin
  if (mpris_connection <> nil) then
    begin
    path := ('/org/mpris/MediaPlayer2');
    interface_ := ('org.mpris.MediaPlayer2.Player');
    message := dbus_message_new_signal(path, 'org.freedesktop.DBus.Properties', 'PropertiesChanged');
    dbus_message_iter_init_append(message, @iter);
    dbus_message_iter_append_basic(@iter, DBUS_TYPE_STRING, @interface_);
    dbus_message_iter_open_container(@iter, DBUS_TYPE_ARRAY, '{sv}', @dict);
    dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
    property_ := ('PlaybackStatus');
    case fbackend.Status of
      engine_play: s_val := ('Playing');
      engine_pause: s_val := ('Paused');
      else
        s_val := ('Stopped');
      end;
    dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
    dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 's', @dict_val);
    dbus_message_iter_append_basic(@dict_val, DBUS_TYPE_STRING, @s_val);
    dbus_message_iter_close_container(@dict_entry, @dict_val);
    dbus_message_iter_close_container(@dict, @dict_entry);
    dbus_message_iter_close_container(@iter, @dict);
    dbus_message_iter_open_container(@iter, DBUS_TYPE_ARRAY, 's', @dict);
    dbus_message_iter_append_basic(@dict, DBUS_TYPE_STRING, @property_);
    dbus_message_iter_close_container(@iter, @dict);
    dbus_connection_send(mpris_connection, message, nil);
    dbus_message_unref(message);
    end;
end;

procedure TMpris2.mpris_send_signal_Updated_Metadata;
var
  message: PDBusMessage;
  iter, dict, dict_entry, dict_val: DBusMessageIter;
  path, interface_, property_, s_val: PChar;
  b_val: dword;
begin
  if (mpris_connection <> nil) then
    begin
    path := ('/org/mpris/MediaPlayer2');
    interface_ := ('org.mpris.MediaPlayer2.Player');
    message := dbus_message_new_signal(path, 'org.freedesktop.DBus.Properties', 'PropertiesChanged');
    dbus_message_iter_init_append(message, @iter);
    dbus_message_iter_append_basic(@iter, DBUS_TYPE_STRING, @interface_);
    dbus_message_iter_open_container(@iter, DBUS_TYPE_ARRAY, '{sv}', @dict);
    dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
    property_ := ('Metadata');
    dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
    append_metadata_array(@dict_entry, fBackEnd);
    dbus_message_iter_close_container(@dict, @dict_entry);

    dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
    property_ := ('CanSeek');
    dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
    dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 'b', @dict_val);
    b_val := MyTrue;
    dbus_message_iter_append_basic(@dict_val, DBUS_TYPE_BOOLEAN, @b_val);
    dbus_message_iter_close_container(@dict_entry, @dict_val);
    dbus_message_iter_close_container(@dict, @dict_entry);

    dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
    property_ := ('PlaybackStatus');
    case fbackend.Status of
      engine_play: s_val := ('Playing');
      engine_pause: s_val := ('Paused');
      else
        s_val := ('Stopped');
      end;
    dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
    dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 's', @dict_val);
    dbus_message_iter_append_basic(@dict_val, DBUS_TYPE_STRING, @s_val);
    dbus_message_iter_close_container(@dict_entry, @dict_val);
    dbus_message_iter_close_container(@dict, @dict_entry);
    dbus_message_iter_close_container(@iter, @dict);
    dbus_message_iter_open_container(@iter, DBUS_TYPE_ARRAY, 's', @dict);
    dbus_message_iter_close_container(@iter, @dict);
    dbus_connection_send(mpris_connection, message, nil);
    dbus_message_unref(message);
    end;
end;

procedure TMpris2.mpris_send_signal_Seeked;
var
  message: PDBusMessage;
  iter: DBusMessageIter;
  path, interface_: PChar;
  x_val: int64;
begin
  if (mpris_connection <> nil) then
    begin
    path := ('/org/mpris/MediaPlayer2');
    interface_ := ('org.mpris.MediaPlayer2.Player');
    message := dbus_message_new_signal(path, interface_, 'Seeked');
    dbus_message_iter_init_append(message, @iter);
    x_val := fbackend.Position * 1000000;
    dbus_message_iter_append_basic(@iter, DBUS_TYPE_INT64, @x_val);
    dbus_connection_send(mpris_connection, message, nil);
    dbus_message_unref(message);
    end;
end;

procedure TMpris2.mpris_send_signal_VolumeChanged;
var
  message: PDBusMessage;
  iter, dict, dict_entry, dict_val: DBusMessageIter;
  path, interface_, property_, s_val: PChar;
  D_val: double;
begin

  if (mpris_connection <> nil) then
    begin
    path := ('/org/mpris/MediaPlayer2');
    interface_ := ('org.mpris.MediaPlayer2.Player');
    message := dbus_message_new_signal(path, 'org.freedesktop.DBus.Properties', 'PropertiesChanged');
    dbus_message_iter_init_append(message, @iter);
    dbus_message_iter_append_basic(@iter, DBUS_TYPE_STRING, @interface_);
    dbus_message_iter_open_container(@iter, DBUS_TYPE_ARRAY, '{sv}', @dict);
    dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
    property_ := ('Volume');
    dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
    dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 'd', @dict_val);
    d_val := fBackend.Volume / 255;
    dbus_message_iter_append_basic(@dict_val, DBUS_TYPE_DOUBLE, @d_val);
    dbus_message_iter_close_container(@dict_entry, @dict_val);
    dbus_message_iter_close_container(@dict, @dict_entry);
    dbus_message_iter_close_container(@iter, @dict);
    dbus_message_iter_open_container(@iter, DBUS_TYPE_ARRAY, 's', @dict);
    dbus_message_iter_close_container(@iter, @dict);
    dbus_connection_send(mpris_connection, message, nil);
    dbus_message_unref(message);
    end;
end;

constructor TMpris2.Create;
begin
  mpris_connection := nil;
end;

destructor TMpris2.Destroy;
begin
  if Assigned(mpris_connection) then
    DeActivate;
  inherited Destroy;
end;

end.
