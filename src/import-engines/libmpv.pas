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
{$packrecords C}
unit libmpv;
interface
uses
  ctypes;


const
  {$IFDEF LINUX}
     External_libraryV1='libmpv.so.1';
     External_libraryV1='libmpv.so.2';
  {$ENDIF LINUX}
  {$IFDEF WINDOWS}
     External_libraryV1='mpv-1.dll';
     External_libraryV2='libmpv-2.dll';
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
     External_libraryV1 = 'libmpv.dylib';
     External_libraryV2 = 'libmpv.dylib';
  {$ENDIF DARWIN}


  { Pointers to basic pascal types, inserted by h2pas conversion program.}
  Type
    PLongint  = ^Longint;
    PSmallInt = ^SmallInt;
    PByte     = ^Byte;
    PWord     = ^Word;
    PDWord    = ^DWord;
    PDouble   = ^Double;

    Pmpv_handle  = ^mpv_handle;
    mpv_handle = Thandle;

  { Permission to use, copy, modify, and/or distribute this software for any
   * purpose with or without fee is hereby granted, provided that the above
   * copyright notice and this permission notice appear in all copies.
   *
   * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
    }
  {
   * Note: the client API is licensed under ISC (see above) to ease
   * interoperability with other licenses. But keep in mind that the
   * mpv core is still mostly GPLv2+. It's up to lawyers to decide
   * whether applications using this API are affected by the GPL.
   * One argument against this is that proprietary applications
   * using mplayer in slave mode is apparently tolerated, and this
   * API is basically equivalent to slave mode.
    }
 {*
   * Mechanisms provided by this API
   * -------------------------------
   *
   * This API provides general control over mpv playback. It does not give you
   * direct access to individual components of the player, only the whole thing.
   * It's somewhat equivalent to MPlayer's slave mode. You can send commands,
   * retrieve or set playback status or settings with properties, and receive
   * events.
   *
   * The API can be used in two ways:
   * 1) Internally in mpv, to provide additional features to the command line
   *    player. Lua scripting uses this. (Currently there is no plugin API to
   *    get a client API handle in external user code. It has to be a fixed
   *    part of the player at compilation time.)
   * 2) Using mpv as a library with mpv_create(). This basically allows embedding
   *    mpv in other applications.
   *
   * Event loop
   * ----------
   *
   * In general, the API user should run an event loop in order to receive events.
   * This even loop should call mpv_wait_event(), which will return once a new
   * mpv client API is available. It should also be possible to integrate client
   * API usage in other event loops (e.g. GUI toolkits) with the
   * mpv_set_wakeup_callback() function, and then polling for events by calling
   * mpv_wait_event() with a 0 timeout.
   *
   * Note that the event loop is detached from the actual player. Not calling
   * mpv_wait_event() will not stop playback. It will eventually congest the
   * event queue of your API handle, though.
   *
   * Synchronous vs. asynchronous calls
   * ----------------------------------
   *
   * The API allows both synchronous and asynchronous calls. Synchronous calls
   * have to wait until the playback core is ready, which currently can take
   * an unbounded time (e.g. if network is slow or unresponsive). Asynchronous
   * calls just queue operations as requests, and return the result of the
   * operation as events.
   *
   * Asynchronous calls
   * ------------------
   *
   * The client API includes asynchronous functions. These allow you to send
   * requests instantly, and get replies as events at a later point. The
   * requests are made with functions carrying the _async suffix, and replies
   * are returned by mpv_wait_event() (interleaved with the normal event stream).
   *
   * A 64 bit userdata value is used to allow the user to associate requests
   * with replies. The value is passed as reply_userdata parameter to the request
   * function. The reply to the request will have the reply
   * mpv_event->reply_userdata field set to the same value as the
   * reply_userdata parameter of the corresponding request.
   *
   * This userdata value is arbitrary and is never interpreted by the API. Note
   * that the userdata value 0 is also allowed, but then the client must be
   * careful not accidentally interpret the mpv_event->reply_userdata if an
   * event is not a reply. (For non-replies, this field is set to 0.)
   *
   * Currently, asynchronous calls are always strictly ordered (even with
   * synchronous calls) for each client, although that may change in the future.
   *
   * Multithreading
   * --------------
   *
   * The client API is generally fully thread-safe, unless otherwise noted.
   * Currently, there is no real advantage in using more than 1 thread to access
   * the client API, since everything is serialized through a single lock in the
   * playback core.
   *
   * Basic environment requirements
   * ------------------------------
   *
   * This documents basic requirements on the C environment. This is especially
   * important if mpv is used as library with mpv_create().
   *
   * - The LC_NUMERIC locale category must be set to "C". If your program calls
   *   setlocale(), be sure not to use LC_ALL, or if you do, reset LC_NUMERIC
   *   to its sane default: setlocale(LC_NUMERIC, "C").
   * - If a X11 based VO is used, mpv will set the xlib error handler. This error
   *   handler is process-wide, and there's no proper way to share it with other
   *   xlib users within the same process. This might confuse GUI toolkits.
   * - mpv uses some other libraries that are not library-safe, such as Fribidi
   *   (used through libass), LittleCMS, ALSA, FFmpeg, and possibly more.
   * - The FPU precision must be set at least to double precision.
   * - On Windows, mpv will call timeBeginPeriod(1).
   * - On memory exhaustion, mpv will kill the process.
   *
   * Embedding the video window
   * --------------------------
   *
   * Currently you have to get the raw window handle, and set it as "wid" option.
   * This works on X11 and win32 only. In addition, it works with a few VOs only,
   * and VOs which do not support this will just create a freestanding window.
   *
   * Both on X11 and win32, the player will fill the window referenced by the
   * "wid" option fully and letterbox the video (i.e. add black bars if the
   * aspect ratio of the window and the video mismatch).
   *
   * On OSX, embedding is not yet possible, because Cocoa makes this non-trivial.
    }
  {*
   * The version is incremented on each API change. The 16 lower bits form the
   * minor version number, and the 16 higher bits the major version number. If
   * the API becomes incompatible to previous versions, the major version
   * number is incremented. This affects only C part, and not properties and
   * options.
    }

  const
    _MPV_CLIENT_API_VERSION = $00010000;
  {*
   * Return the MPV_CLIENT_API_VERSION the mpv source has been compiled with.
    }

  var
    mpv_client_api_version : function:dword;cdecl;
  {*
   * Client context used by the client API. Every client has its own private
   * handle.
    }

  type
   mpv_error =  clong;
  {*
   * List of error codes than can be returned by API functions. 0 and positive
   * return values always mean success, negative values are always errors.
    }
const
     {*
     * No error happened (used to signal successful operation).
     * Keep in mind that many API functions returning error codes can also
     * return positive values, which also indicate success. API users can
     * hardcode the fact that ">= 0" means success.
     }
    MPV_ERROR_SUCCESS = 0;
    {*
     * The event ringbuffer is full. This means the client is choked, and can't
     * receive any events. This can happen when too many asynchronous requests
     * have been made, but not answered. Probably never happens in practice,
     * unless the mpv core is frozen for some reason, and the client keeps
     * making asynchronous requests. (Bugs in the client API implementation
     * could also trigger this, e.g. if events become "lost".)
     }
    MPV_ERROR_EVENT_QUEUE_FULL = -1;
    {*
     * Memory allocation failed.
     }
    MPV_ERROR_NOMEM = -2;
    {*
     * The mpv core wasn't configured and initialized yet. See the notes in
     * mpv_create().
     }
    MPV_ERROR_UNINITIALIZED = -3;
    {*
     * Generic catch-all error if a parameter is set to an invalid or
     * unsupported value. This is used if there is no better error code.
     }
    MPV_ERROR_INVALID_PARAMETER = -4;
    {*
     * Trying to set an option that doesn't exist.
     }
    MPV_ERROR_OPTION_NOT_FOUND = -5;
    {*
     * Trying to set an option using an unsupported MPV_FORMAT.
     }
    MPV_ERROR_OPTION_FORMAT = -6;
    {*
     * Setting the option failed. Typically this happens if the provided option
     * value could not be parsed.
     }
    MPV_ERROR_OPTION_ERROR = -7;
    {*
     * The accessed property doesn't exist.
     }
    MPV_ERROR_PROPERTY_NOT_FOUND = -8;
    {*
     * Trying to set or get a property using an unsupported MPV_FORMAT.
     }
    MPV_ERROR_PROPERTY_FORMAT = -9;
    {*
     * The property exists, but is not available. This usually happens when the
     * associated subsystem is not active, e.g. querying audio parameters while
     * audio is disabled.
     }
    MPV_ERROR_PROPERTY_UNAVAILABLE = -10;
    {*
     * Error setting or getting a property.
     }
    MPV_ERROR_PROPERTY_ERROR = -11;
    {*
     * General error when running a command with mpv_command and similar.
        }
   MPV_ERROR_COMMAND = -12;
    {*
     * Generic error on loading (usually used with mpv_event_end_file.error).
     }
    MPV_ERROR_LOADING_FAILED = -13;
    {*
     * Initializing the audio output failed.
     }
    MPV_ERROR_AO_INIT_FAILED = -14;
    {*
     * Initializing the video output failed.
     }
    MPV_ERROR_VO_INIT_FAILED = -15;
    {*
     * There was no audio or video data to play. This also happens if the
     * file was recognized, but did not contain any audio or video streams,
     * or no streams were selected.
     }
    MPV_ERROR_NOTHING_TO_PLAY = -16;
    {*
     * When trying to load the file, the file format could not be determined,
     * or the file was too broken to open it.
     }
    MPV_ERROR_UNKNOWN_FORMAT = -17;
    {*
     * Generic error for signaling that certain system requirements are not
     * fulfilled.
     }
    MPV_ERROR_UNSUPPORTED = -18;
    {*
     * The API function which was called is a stub only.
     }
    MPV_ERROR_NOT_IMPLEMENTED = -19;
    {*
     * Unspecified error.
     }
    MPV_ERROR_GENERIC = -20;

type
  Pmpv_error = ^mpv_error;

  (**
   * Data format for options and properties. The API functions to get/set
   * properties and options support multiple formats, and this enum describes
   * them.
   *)
  type
    Pmpv_format = ^mpv_format;
    mpv_format =  clong;
const
    {*
     * Invalid. Sometimes used for empty values. This is always defined to 0,
     * so a normal 0-init of mpv_format (or e.g. mpv_node) is guaranteed to set
     * this it to MPV_FORMAT_NONE (which makes some things saner as consequence).
     }
    MPV_FORMAT_NONE = 0;
    {*
     * The basic type is char*. It returns the raw property string, like
     * using ${=property} in input.conf (see input.rst).
     *
     * NULL isn't an allowed value.
     *
     * Warning: although the encoding is usually UTF-8, this is not always the
     *          case. File tags often store strings in some legacy codepage,
     *          and even filenames don't necessarily have to be in UTF-8 (at
     *          least on Linux). If you pass the strings to code that requires
     *          valid UTF-8, you have to sanitize it in some way.
     *          On Windows, filenames are always UTF-8, and libmpv converts
     *          between UTF-8 and UTF-16 when using win32 API functions. See
     *          the "Encoding of filenames" section for details.
     *
     * Example for reading:
     *
     *     char *result = NULL;
     *     if (mpv_get_property(ctx, "property", MPV_FORMAT_STRING, &result) < 0)
     *         goto error;
     *     printf("%s\n", result);
     *     mpv_free(result);
     *
     * Or just use mpv_get_property_string().
     *
     * Example for writing:
     *
     *     char *value = "the new value";
     *     // yep, you pass the address to the variable
     *     // (needed for symmetry with other types and mpv_get_property)
     *     mpv_set_property(ctx, "property", MPV_FORMAT_STRING, &value);
     *
     * Or just use mpv_set_property_string().
     *
     }
    MPV_FORMAT_STRING = 1;
    {*
     * The basic type is char*. It returns the OSD property string, like
     * using ${property} in input.conf (see input.rst). In many cases, this
     * is the same as the raw string, but in other cases it's formatted for
     * display on OSD. It's intended to be human readable. Do not attempt to
     * parse these strings.
     *
     * Only valid when doing read access. The rest works like MPV_FORMAT_STRING.
     }
    MPV_FORMAT_OSD_STRING = 2;
    {*
     * The basic type is int. The only allowed values are 0 ("no")
     * and 1 ("yes").
     *
     * Example for reading:
     *
     *     int result;
     *     if (mpv_get_property(ctx, "property", MPV_FORMAT_FLAG, &result) < 0)
     *         goto error;
     *     printf("%s\n", result ? "true" : "false");
     *
     * Example for writing:
     *
     *     int flag = 1;
     *     mpv_set_property(ctx, "property", MPV_FORMAT_FLAG, &flag);
     }
    MPV_FORMAT_FLAG = 3;
    {*
     * The basic type is int64_t.
     }
    MPV_FORMAT_INT64 = 4;
    {*
     * The basic type is double.
     }
    MPV_FORMAT_DOUBLE = 5;
    {*
     * The type is mpv_node.
     *
     * For reading, you usually would pass a pointer to a stack-allocated
     * mpv_node value to mpv, and when you're done you call
     * mpv_free_node_contents(&node).
     * You're expected not to write to the data - if you have to, copy it
     * first (which you have to do manually).
     *
     * For writing, you construct your own mpv_node, and pass a pointer to the
     * API. The API will never write to your data (and copy it if needed), so
     * you're free to use any form of allocation or memory management you like.
     *
     * Warning: when reading, always check the mpv_node.format member. For
     *          example, properties might change their type in future versions
     *          of mpv, or sometimes even during runtime.
     *
     * Example for reading:
     *
     *     mpv_node result;
     *     if (mpv_get_property(ctx, "property", MPV_FORMAT_NODE, &result) < 0)
     *         goto error;
     *     printf("format=%d\n", (int)result.format);
     *     mpv_free_node_contents(&result).
     *
     * Example for writing:
     *
     *     mpv_node value;
     *     value.format = MPV_FORMAT_STRING;
     *     value.u.string = "hello";
     *     mpv_set_property(ctx, "property", MPV_FORMAT_NODE, &value);
     }
    MPV_FORMAT_NODE = 6;
    {*
     * Used with mpv_node only. Can usually not be used directly.
     }
    MPV_FORMAT_NODE_ARRAY = 7;
    {*
     * See MPV_FORMAT_NODE_ARRAY.
     }
    MPV_FORMAT_NODE_MAP = 8;
    {*
     * A raw, untyped byte array. Only used only with mpv_node, and only in
     * some very specific situations. (Some commands use it.)
     }
    MPV_FORMAT_BYTE_ARRAY = 9;

type
    Pmpv_event_id = ^mpv_event_id;
    mpv_event_id =  Longint;
    {*
     * Generic data storage.
     *
     * If mpv writes this struct (e.g. via mpv_get_property()), you must not change
     * the data. In some cases (mpv_get_property()), you have to free it with
     * mpv_free_node_contents(). If you fill this struct yourself, you're also
     * responsible for freeing it, and you must not call mpv_free_node_contents(). }
    type
      Pmpv_node_list = ^mpv_node_list;
      mpv_byte_array = packed record
          data: pointer;
          size: csize_t;
      end;
      _u = packed record
          case integer of
           0:( string_ : Pchar);
          {* valid if format==MPV_FORMAT_STRING  }
           1:( flag_   : longint);
          {* valid if format==MPV_FORMAT_FLAG    }
           2:( double_ : double);
          {* valid if format==MPV_FORMAT_DOUBLE  }
           3:( list_   : Pmpv_node_list);
         {* valid if format==MPV_FORMAT_NODE_ARRAY
           *    or if format==MPV_FORMAT_NODE_MAP }
           4:( int64_  : int64);
          {* valid if format==MPV_FORMAT_INT64   }
           5:( byte_array:^mpv_byte_array);
          {* valid if format==MPV_FORMAT_BYTE_ARRAY}
         end;
    {*
       * Type of the data stored in this struct. This value rules what members in
       * the given union can be accessed. The following formats are currently
       * defined to be allowed in mpv_node:
       *
       *  MPV_FORMAT_STRING       (u.string)
       *  MPV_FORMAT_FLAG         (u.flag)
       *  MPV_FORMAT_INT64        (u.int64)
       *  MPV_FORMAT_DOUBLE       (u.double_)
       *  MPV_FORMAT_NODE_ARRAY   (u.list)
       *  MPV_FORMAT_NODE_MAP     (u.list)
       *  MPV_FORMAT_BYTE_ARRAY   (u.ba)
       *  MPV_FORMAT_NONE         (no member)
       *
       * If you encounter a value you don't know, you must not make any
       * assumptions about the contents of union u.
          }

      mpv_node = packed record
          u: _u;
      format: mpv_format;
          {$IFDEF CPU32}
      dummy: integer;
          {$ENDIF}
    end;
    {*
     * (see mpv_node)
      }
      Pmpv_node = ^mpv_node;
      mpv_node_array = packed array [0..100] of mpv_node;
      pmpv_node_array = ^mpv_node_array;

     {*
     * (see mpv_node)
     *}
  mpv_node_list = packed record
      (**
       * Number of entries. Negative values are not allowed.
          *)
          num : cLong;
    {*
       * MPV_FORMAT_NODE_ARRAY:
       *  values[N] refers to value of the Nth item
       *
       * MPV_FORMAT_NODE_MAP:
       *  values[N] refers to value of the Nth key/value pair
       *
       * If num > 0, values[0] to values[num-1] (inclusive) are valid.
       * Otherwise, this can be NULL.
          }
          values : pmpv_node_array;
    {*
       * MPV_FORMAT_NODE_ARRAY:
       *  unused (typically NULL), access is not allowed
       *
       * MPV_FORMAT_NODE_MAP:
       *  keys[N] refers to key of the Nth key/value pair. If num > 0, keys[0] to
       *  keys[num-1] (inclusive) are valid. Otherwise, this can be NULL.
       *  The keys are in random order. The only guarantee is that keys[N] belongs
       *  to the value values[N]. NULL keys are not allowed.
          }
          keys : PPChar;
    end;

const
    (**
     * Nothing happened. Happens on timeouts or sporadic wakeups.
     *)
    MPV_EVENT_NONE = 0;
    (**
     * Happens when the player quits. The player enters a state where it tries
     * to disconnect all clients. Most requests to the player will fail, and
     * the client should react to this and quit with mpv_destroy() as soon as
     * possible.
     *)
    MPV_EVENT_SHUTDOWN = 1;
    (**
     * See mpv_request_log_messages().
     *)
    MPV_EVENT_LOG_MESSAGE = 2;
    (**
     * Reply to a mpv_get_property_async() request.
     * See also mpv_event and mpv_event_property.
     *)
    MPV_EVENT_GET_PROPERTY_REPLY = 3;
    (**
     * Reply to a mpv_set_property_async() request.
     * (Unlike MPV_EVENT_GET_PROPERTY, mpv_event_property is not used.)
     *)
    MPV_EVENT_SET_PROPERTY_REPLY = 4;
    (**
     * Reply to a mpv_command_async() or mpv_command_node_async() request.
     * See also mpv_event and mpv_event_command.
     *)
    MPV_EVENT_COMMAND_REPLY = 5;
    (**
     * Notification before playback start of a file (before the file is loaded).
     * See also mpv_event and mpv_event_start_file.
     *)
    MPV_EVENT_START_FILE = 6;
    (**
     * Notification after playback end (after the file was unloaded).
     * See also mpv_event and mpv_event_end_file.
     *)
    MPV_EVENT_END_FILE = 7;
    (**
     * Notification when the file has been loaded (headers were read etc.), and
     * decoding starts.
     *)
    MPV_EVENT_FILE_LOADED = 8;
    (**
     * The list of video/audio/subtitle tracks was changed. (E.g. a new track
     * was found. This doesn't necessarily indicate a track switch; for this,
     * MPV_EVENT_TRACK_SWITCHED is used.)
     *
     * @deprecated This is equivalent to using mpv_observe_property() on the
     *             "track-list" property. The event is redundant, and might
     *             be removed in the far future.
     *)
    MPV_EVENT_TRACKS_CHANGED = 9;
    (**
     * A video/audio/subtitle track was switched on or off.
     *
     * @deprecated This is equivalent to using mpv_observe_property() on the
     *             "vid", "aid", and "sid" properties. The event is redundant,
     *             and might be removed in the far future.
     *)
    MPV_EVENT_TRACK_SWITCHED = 10;
    (**
     * Idle mode was entered. In this mode, no file is played, and the playback
     * core waits for new commands. (The command line player normally quits
     * instead of entering idle mode, unless --idle was specified. If mpv
     * was started with mpv_create(), idle mode is enabled by default.)
     *
     * @deprecated This is equivalent to using mpv_observe_property() on the
     *             "idle-active" property. The event is redundant, and might be
     *             removed in the far future. As a further warning, this event
     *             is not necessarily sent at the right point anymore (at the
     *             start of the program), while the property behaves correctly.
     *)
    MPV_EVENT_IDLE = 11;
    (**
     * Playback was paused. This indicates the user pause state.
     *
     * The user pause state is the state the user requested (changed with the
     * "pause" property). There is an internal pause state too, which is entered
     * if e.g. the network is too slow (the "core-idle" property generally
     * indicates whether the core is playing or waiting).
     *
     * This event is sent whenever any pause states change, not only the user
     * state. You might get multiple events in a row while these states change
     * independently. But the event ID sent always indicates the user pause
     * state.
     *
     * If you don't want to deal with this, use mpv_observe_property() on the
     * "pause" property and ignore MPV_EVENT_PAUSE/UNPAUSE. Likewise, the
     * "core-idle" property tells you whether video is actually playing or not.
     *
     * @deprecated The event is redundant with mpv_observe_property() as
     *             mentioned above, and might be removed in the far future.
     *)
    MPV_EVENT_PAUSE = 12;
    (**
     * Playback was unpaused. See MPV_EVENT_PAUSE for not so obvious details.
     *
     * @deprecated The event is redundant with mpv_observe_property() as
     *             explained in the MPV_EVENT_PAUSE comments, and might be
     *             removed in the far future.
     *)
    MPV_EVENT_UNPAUSE = 13;
    (**
     * Sent every time after a video frame is displayed. Note that currently,
     * this will be sent in lower frequency if there is no video, or playback
     * is paused - but that will be removed in the future, and it will be
     * restricted to video frames only.
     *
     * @deprecated Use mpv_observe_property() with relevant properties instead
     *             (such as "playback-time").
     *)
    MPV_EVENT_TICK = 14;
    (**
     * @deprecated This was used internally with the internal "script_dispatch"
     *             command to dispatch keyboard and mouse input for the OSC.
     *             It was never useful in general and has been completely
     *             replaced with "script-binding".
     *             This event never happens anymore, and is included in this
     *             header only for compatibility.
     *)
    MPV_EVENT_SCRIPT_INPUT_DISPATCH = 15;
    (**
     * Triggered by the script-message input command. The command uses the
     * first argument of the command as client name (see mpv_client_name()) to
     * dispatch the message, and passes along all arguments starting from the
     * second argument as strings.
     * See also mpv_event and mpv_event_client_message.
     *)
    MPV_EVENT_CLIENT_MESSAGE = 16;
    (**
     * Happens after video changed in some way. This can happen on resolution
     * changes, pixel format changes, or video filter changes. The event is
     * sent after the video filters and the VO are reconfigured. Applications
     * embedding a mpv window should listen to this event in order to resize
     * the window if needed.
     * Note that this event can happen sporadically, and you should check
     * yourself whether the video parameters really changed before doing
     * something expensive.
     *)
    MPV_EVENT_VIDEO_RECONFIG = 17;
    (**
     * Similar to MPV_EVENT_VIDEO_RECONFIG. This is relatively uninteresting,
     * because there is no such thing as audio output embedding.
     *)
    MPV_EVENT_AUDIO_RECONFIG = 18;
    (**
     * Happens when metadata (like file tags) is possibly updated. (It's left
     * unspecified whether this happens on file start or only when it changes
     * within a file.)
     *
     * @deprecated This is equivalent to using mpv_observe_property() on the
     *             "metadata" property. The event is redundant, and might
     *             be removed in the far future.
     *)
    MPV_EVENT_METADATA_UPDATE = 19;
    (**
     * Happens when a seek was initiated. Playback stops. Usually it will
     * resume with MPV_EVENT_PLAYBACK_RESTART as soon as the seek is finished.
     *)
    MPV_EVENT_SEEK = 20;
    (**
     * There was a discontinuity of some sort (like a seek), and playback
     * was reinitialized. Usually happens on start of playback and after
     * seeking. The main purpose is allowing the client to detect when a seek
     * request is finished.
     *)
    MPV_EVENT_PLAYBACK_RESTART = 21;
    (**
     * Event sent due to mpv_observe_property().
     * See also mpv_event and mpv_event_property.
     *)
    MPV_EVENT_PROPERTY_CHANGE = 22;
    (**
     * Happens when the current chapter changes.
     *
     * @deprecated This is equivalent to using mpv_observe_property() on the
     *             "chapter" property. The event is redundant, and might
     *             be removed in the far future.
     *)
    MPV_EVENT_CHAPTER_CHANGE = 23;
    (**
     * Happens if the internal per-mpv_handle ringbuffer overflows, and at
     * least 1 event had to be dropped. This can happen if the client doesn't
     * read the event queue quickly enough with mpv_wait_event(), or if the
     * client makes a very large number of asynchronous calls at once.
     *
     * Event delivery will continue normally once this event was returned
     * (this forces the client to empty the queue completely).
     *)
    MPV_EVENT_QUEUE_OVERFLOW = 24;
    (**
     * Triggered if a hook handler was registered with mpv_hook_add(), and the
     * hook is invoked. If you receive this, you must handle it, and continue
     * the hook with mpv_hook_continue().
     * See also mpv_event and mpv_event_hook.
     *)
    MPV_EVENT_HOOK = 25;

type
      pmpv_event_property = ^mpv_event_property;
      mpv_event_property = packed record
        (**
         * Name of the property.
         *)
        name: PChar;
        (**
         * Format of the data field in the same struct. See enum mpv_format.
         * This is always the same format as the requested format, except when
         * the property could not be retrieved (unavailable, or an error happened),
         * in which case the format is MPV_FORMAT_NONE.
         *)
        format: mpv_format;
        (**
         * Received property value. Depends on the format. This is like the
         * pointer argument passed to mpv_get_property().
         *
         * For example, for MPV_FORMAT_STRING you get the string with:
         *
         *    char *value = *(char ** )(event_property->data);
         *
         * Note that this is set to NULL if retrieving the property failed (the
         * format will be MPV_FORMAT_NONE).
         *)
        data: Pointer;
      end;

      (**
       * Numeric log levels. The lower the number, the more important the message is.
       * MPV_LOG_LEVEL_NONE is never used when receiving messages. The string in
       * the comment after the value is the name of the log level as used for the
       * mpv_request_log_messages() function.
       * Unused numeric values are unused, but reserved for future use.
       *)
      mpv_log_level = (
        MPV_LOG_LEVEL_NONE = 0,
        /// "no"    - disable absolutely all messages
        MPV_LOG_LEVEL_FATAL = 10,
        /// "fatal" - critical/aborting errors
        MPV_LOG_LEVEL_ERROR = 20,
        /// "error" - simple errors
        MPV_LOG_LEVEL_WARN = 30,
        /// "warn"  - possible problems
        MPV_LOG_LEVEL_INFO = 40,
        /// "info"  - informational message
        MPV_LOG_LEVEL_V = 50,
        /// "v"     - noisy informational message
        MPV_LOG_LEVEL_DEBUG = 60,
        /// "debug" - very noisy technical information
        MPV_LOG_LEVEL_TRACE = 70);
      Pmpv_log_level = ^mpv_log_level;

      pmpv_event_log_message=  ^_mpv_event_log_message;
      _mpv_event_log_message = packed record
        (**
         * The module prefix, identifies the sender of the message. As a special
         * case, if the message buffer overflows, this will be set to the string
         * "overflow" (which doesn't appear as prefix otherwise), and the text
         * field will contain an informative message.
         *)
        prefix: PChar;
        (**
         * The log level as string. See mpv_request_log_messages() for possible
         * values. The level "no" is never used here.
         *)
        level: PChar;
        (**
         * The log message. It consists of 1 line of text, and is terminated with
         * a newline character. (Before API version 1.6, it could contain multiple
         * or partial lines.)
         *)
        text: PChar;
        (**
         * The same contents as the level field, but as a numeric ID.
         * Since API version 1.6.
         *)
        log_level: mpv_log_level;
      end;

      /// Since API version 1.9.
      mpv_end_file_reason = (
        (**
         * The end of file was reached. Sometimes this may also happen on
         * incomplete or corrupted files, or if the network connection was
         * interrupted when playing a remote file. It also happens if the
         * playback range was restricted with --end or --frames or similar.
         *)
        MPV_END_FILE_REASON_EOF = 0,
        (**
         * Playback was stopped by an external action (e.g. playlist controls).
         *)
        MPV_END_FILE_REASON_STOP = 2,
        (**
         * Playback was stopped by the quit command or player shutdown.
         *)
        MPV_END_FILE_REASON_QUIT = 3,
        (**
         * Some kind of error happened that lead to playback abort. Does not
         * necessarily happen on incomplete or broken files (in these cases, both
         * MPV_END_FILE_REASON_ERROR or MPV_END_FILE_REASON_EOF are possible).
         *
         * mpv_event_end_file.error will be set.
         *)
        MPV_END_FILE_REASON_ERROR = 4,
        (**
         * The file was a playlist or similar. When the playlist is read, its
         * entries will be appended to the playlist after the entry of the current
         * file, the entry of the current file is removed, and a MPV_EVENT_END_FILE
         * event is sent with reason set to MPV_END_FILE_REASON_REDIRECT. Then
         * playback continues with the playlist contents.
         * Since API version 1.18.
         *)
        MPV_END_FILE_REASON_REDIRECT = 5);
      Pmpv_end_file_reason = ^mpv_end_file_reason;

      /// Since API version 1.108.
      _mpv_event_start_file = packed record
        (**
         * Playlist entry ID of the file being loaded now.
         *)
        playlist_entry_id: Int64;
      end;

      _mpv_event_end_file = packed record
        (**
         * Corresponds to the values in enum mpv_end_file_reason (the "int" type
         * will be replaced with mpv_end_file_reason on the next ABI bump).
         *
         * Unknown values should be treated as unknown.
         *)
        reason: Integer;
        (**
         * If reason==MPV_END_FILE_REASON_ERROR, this contains a mpv error code
         * (one of MPV_ERROR_...) giving an approximate reason why playback
         * failed. In other cases, this field is 0 (no error).
         * Since API version 1.9.
         *)
        error: Integer;
        (**
         * Playlist entry ID of the file that was being played or attempted to be
         * played. This has the same value as the playlist_entry_id field in the
         * corresponding mpv_event_start_file event.
         * Since API version 1.108.
         *)
        playlist_entry_id: Int64;
        (**
         * If loading ended, because the playlist entry to be played was for example
         * a playlist, and the current playlist entry is replaced with a number of
         * other entries. This may happen at least with MPV_END_FILE_REASON_REDIRECT
         * (other event types may use this for similar but different purposes in the
         * future). In this case, playlist_insert_id will be set to the playlist
         * entry ID of the first inserted entry, and playlist_insert_num_entries to
         * the total number of inserted playlist entries. Note this in this specific
         * case, the ID of the last inserted entry is playlist_insert_id+num-1.
         * Beware that depending on circumstances, you may observe the new playlist
         * entries before seeing the event (e.g. reading the "playlist" property or
         * getting a property change notification before receiving the event).
         * Since API version 1.108.
         *)
        playlist_insert_id: Int64;
        (**
         * See playlist_insert_id. Only non-0 if playlist_insert_id is valid. Never
         * negative.
         * Since API version 1.108.
         *)
        playlist_insert_num_entries: Integer;
      end;

      (** @deprecated see MPV_EVENT_SCRIPT_INPUT_DISPATCH for remarks
       *)
      _mpv_event_script_input_dispatch = packed record
        arg0: Integer;
        _type: PUTF8Char;
      end;

      _mpv_event_client_message = packed record
        (**
         * Arbitrary arguments chosen by the sender of the message. If num_args > 0,
         * you can access args[0] through args[num_args - 1] (inclusive). What
         * these arguments mean is up to the sender and receiver.
         * None of the valid items are NULL.
         *)
        num_args: Integer;
        args: PPChar;
      end;

      _mpv_event_hook = packed record
        (**
         * The hook name as passed to mpv_hook_add().
         *)
        name: PChar;
        (**
         * Internal ID that must be passed to mpv_hook_continue().
         *)
        id: UInt64;
      end;

      mpv_event_command = packed record
        (**
         * Result data of the command. Note that success/failure is signaled
         * separately via mpv_event.error. This field is only for result data
         * in case of success. Most commands leave it at MPV_FORMAT_NONE. Set
         * to MPV_FORMAT_NONE on failure.
         *)
        result: mpv_node;
      end;

      pmpv_event= ^mpv_event;
      mpv_event = packed record
        (**
         * One of mpv_event. Keep in mind that later ABI compatible releases might
         * add new event types. These should be ignored by the API user.
         *)
        event_id: mpv_event_id;
        (**
         * This is mainly used for events that are replies to (asynchronous)
         * requests. It contains a status code, which is >= 0 on success, or < 0
         * on error (a mpv_error value). Usually, this will be set if an
         * asynchronous request fails.
         * Used for:
         *  MPV_EVENT_GET_PROPERTY_REPLY
         *  MPV_EVENT_SET_PROPERTY_REPLY
         *  MPV_EVENT_COMMAND_REPLY
         *)
        error: Integer;
        (**
         * If the event is in reply to a request (made with this API and this
         * API handle), this is set to the reply_userdata parameter of the request
         * call. Otherwise, this field is 0.
         * Used for:
         *  MPV_EVENT_GET_PROPERTY_REPLY
         *  MPV_EVENT_SET_PROPERTY_REPLY
         *  MPV_EVENT_COMMAND_REPLY
         *  MPV_EVENT_PROPERTY_CHANGE
         *  MPV_EVENT_HOOK
         *)
        reply_userdata: UInt64;
        (**
         * The meaning and contents of the data member depend on the event_id:
         *  MPV_EVENT_GET_PROPERTY_REPLY:     mpv_event_property*
         *  MPV_EVENT_PROPERTY_CHANGE:        mpv_event_property*
         *  MPV_EVENT_LOG_MESSAGE:            mpv_event_log_message*
         *  MPV_EVENT_CLIENT_MESSAGE:         mpv_event_client_message*
         *  MPV_EVENT_START_FILE:             mpv_event_start_file* (since v1.108)
         *  MPV_EVENT_END_FILE:               mpv_event_end_file*
         *  MPV_EVENT_HOOK:                   mpv_event_hook*
         *  MPV_EVENT_COMMAND_REPLY*          mpv_event_command*
         *  other: NULL
         *
         * Note: future enhancements might add new event structs for existing or new
         *       event types.
         *)
        data: Pointer;
      end;

      (**
       * @deprecated use render.h
       *)
      mpv_sub_api = (
        (**
         * For using mpv's OpenGL renderer on an external OpenGL context.
         * mpv_get_sub_api(MPV_SUB_API_OPENGL_CB) returns mpv_opengl_cb_context*.
         * This context can be used with mpv_opengl_cb_* functions.
         * Will return NULL if unavailable (if OpenGL support was not compiled in).
         * See opengl_cb.h for details.
         *
         * @deprecated use render.h
         *)
        MPV_SUB_API_OPENGL_CB = 1);
      Pmpv_sub_api = ^mpv_sub_api;


var

(**
 * Return a string describing the error. For unknown errors, the string
 * "unknown error" is returned.
 *
 * @param error error number, see enum mpv_error
 * @return A static string describing the error. The string is completely
 *         static, i.e. doesn't need to be deallocated, and is valid forever.
 *)
  mpv_error_string : function(error:clong):Pchar;cdecl;

(**
 * General function to deallocate memory returned by some of the API functions.
 * Call this only if it's explicitly documented as allowed. Calling this on
 * mpv memory not owned by the caller will lead to undefined behavior.
 *
 * @param data A valid pointer returned by the API, or NULL.
 *)
  mpv_free : procedure(var data:pointer);cdecl;

(**
 * Return the name of this client handle. Every client has its own unique
 * name, which is mostly used for user interface purposes.
 *
 * @return The client name. The string is read-only and is valid until the
 *         mpv_handle is destroyed.
 *)
  mpv_client_name : function(var ctx:mpv_handle):Pchar;cdecl;

(**
 * Return the ID of this client handle. Every client has its own unique ID. This
 * ID is never reused by the core, even if the mpv_handle at hand gets destroyed
 * and new handles get allocated.
 *
 * IDs are never 0 or negative.
 *
 * Some mpv APIs (not necessarily all) accept a name in the form "@<id>" in
 * addition of the proper mpv_client_name(), where "<id>" is the ID in decimal
 * form (e.g. "@123"). For example, the "script-message-to" command takes the
 * client name as first argument, but also accepts the client ID formatted in
 * this manner.
 *
 * @return The client ID.
 *)
 mpv_client_id: function(var ctx:mpv_handle): Int64; cdecl;


(**
 * Create a new mpv instance and an associated client API handle to control
 * the mpv instance. This instance is in a pre-initialized state,
 * and needs to be initialized to be actually used with most other API
 * functions.
 *
 * Some API functions will return MPV_ERROR_UNINITIALIZED in the uninitialized
 * state. You can call mpv_set_property() (or mpv_set_property_string() and
 * other variants, and before mpv 0.21.0 mpv_set_option() etc.) to set initial
 * options. After this, call mpv_initialize() to start the player, and then use
 * e.g. mpv_command() to start playback of a file.
 *
 * The point of separating handle creation and actual initialization is that
 * you can configure things which can't be changed during runtime.
 *
 * Unlike the command line player, this will have initial settings suitable
 * for embedding in applications. The following settings are different:
 * - stdin/stdout/stderr and the terminal will never be accessed. This is
 *   equivalent to setting the --no-terminal option.
 *   (Technically, this also suppresses C signal handling.)
 * - No config files will be loaded. This is roughly equivalent to using
 *   --config=no. Since libmpv 1.15, you can actually re-enable this option,
 *   which will make libmpv load config files during mpv_initialize(). If you
 *   do this, you are strongly encouraged to set the "config-dir" option too.
 *   (Otherwise it will load the mpv command line player's config.)
 *   For example:
 *      mpv_set_option_string(mpv, "config-dir", "/my/path"); // set config root
 *      mpv_set_option_string(mpv, "config", "yes"); // enable config loading
 *      (call mpv_initialize() _after_ this)
 * - Idle mode is enabled, which means the playback core will enter idle mode
 *   if there are no more files to play on the internal playlist, instead of
 *   exiting. This is equivalent to the --idle option.
 * - Disable parts of input handling.
 * - Most of the different settings can be viewed with the command line player
 *   by running "mpv --show-profile=libmpv".
 *
 * All this assumes that API users want a mpv instance that is strictly
 * isolated from the command line player's configuration, user settings, and
 * so on. You can re-enable disabled features by setting the appropriate
 * options.
 *
 * The mpv command line parser is not available through this API, but you can
 * set individual options with mpv_set_property(). Files for playback must be
 * loaded with mpv_command() or others.
 *
 * Note that you should avoid doing concurrent accesses on the uninitialized
 * client handle. (Whether concurrent access is definitely allowed or not has
 * yet to be decided.)
 *
 * @return a new mpv client API handle. Returns NULL on error. Currently, this
 *         can happen in the following situations:
 *         - out of memory
 *         - LC_NUMERIC is not set to "C" (see general remarks)
 *)
  mpv_create : function:Pmpv_handle;cdecl;

(**
 * Initialize an uninitialized mpv instance. If the mpv instance is already
 * running, an error is returned.
 *
 * This function needs to be called to make full use of the client API if the
 * client API handle was created with mpv_create().
 *
 * Only the following options are required to be set _before_ mpv_initialize():
 *      - options which are only read at initialization time:
 *        - config
 *        - config-dir
 *        - input-conf
 *        - load-scripts
 *        - script
 *        - player-operation-mode
 *        - input-app-events (OSX)
 *      - all encoding mode options
 *
 * @return error code
 *)
  mpv_initialize : function(var ctx:mpv_handle):longint;cdecl;

(**
 * Disconnect and destroy the mpv_handle. ctx will be deallocated with this
 * API call.
 *
 * If the last mpv_handle is detached, the core player is destroyed. In
 * addition, if there are only weak mpv_handles (such as created by
 * mpv_create_weak_client() or internal scripts), these mpv_handles will
 * be sent MPV_EVENT_SHUTDOWN. This function may block until these clients
 * have responded to the shutdown event, and the core is finally destroyed.
 *)
  mpv_destroy : procedure(var ctx:mpv_handle);cdecl;

(**
 * @deprecated use mpv_destroy(), which has exactly the same semantics (the
 * deprecation is a mere rename)
 *
 * Since mpv client API version 1.29:
 *  If the last mpv_handle is detached, the core player is destroyed. In
 *  addition, if there are only weak mpv_handles (such as created by
 *  mpv_create_weak_client() or internal scripts), these mpv_handles will
 *  be sent MPV_EVENT_SHUTDOWN. This function may block until these clients
 *  have responded to the shutdown event, and the core is finally destroyed.
 *
 * Before mpv client API version 1.29:
 *  This left the player running. If you want to be sure that the
 *  player is terminated, send a "quit" command, and wait until the
 *  MPV_EVENT_SHUTDOWN event is received, or use mpv_terminate_destroy().
 *)
    mpv_detach_destroy : procedure(var ctx:mpv_handle);cdecl;

(**
 * Similar to mpv_destroy(), but brings the player and all clients down
 * as well, and waits until all of them are destroyed. This function blocks. The
 * advantage over mpv_destroy() is that while mpv_destroy() merely
 * detaches the client handle from the player, this function quits the player,
 * waits until all other clients are destroyed (i.e. all mpv_handles are
 * detached), and also waits for the final termination of the player.
 *
 * Since mpv_destroy() is called somewhere on the way, it's not safe to
 * call other functions concurrently on the same context.
 *
 * Since mpv client API version 1.29:
 *  The first call on any mpv_handle will block until the core is destroyed.
 *  This means it will wait until other mpv_handle have been destroyed. If you
 *  want asynchronous destruction, just run the "quit" command, and then react
 *  to the MPV_EVENT_SHUTDOWN event.
 *  If another mpv_handle already called mpv_terminate_destroy(), this call will
 *  not actually block. It will destroy the mpv_handle, and exit immediately,
 *  while other mpv_handles might still be uninitializing.
 *
 * Before mpv client API version 1.29:
 *  If this is called on a mpv_handle that was not created with mpv_create(),
 *  this function will merely send a quit command and then call
 *  mpv_destroy(), without waiting for the actual shutdown.
 *)
mpv_terminate_destroy : procedure(var ctx:mpv_handle);cdecl;

(**
 * Create a new client handle connected to the same player core as ctx. This
 * context has its own event queue, its own mpv_request_event() state, its own
 * mpv_request_log_messages() state, its own set of observed properties, and
 * its own state for asynchronous operations. Otherwise, everything is shared.
 *
 * This handle should be destroyed with mpv_destroy() if no longer
 * needed. The core will live as long as there is at least 1 handle referencing
 * it. Any handle can make the core quit, which will result in every handle
 * receiving MPV_EVENT_SHUTDOWN.
 *
 * This function can not be called before the main handle was initialized with
 * mpv_initialize(). The new handle is always initialized, unless ctx=NULL was
 * passed.
 *
 * @param ctx Used to get the reference to the mpv core; handle-specific
 *            settings and parameters are not used.
 *            If NULL, this function behaves like mpv_create() (ignores name).
 * @param name The client name. This will be returned by mpv_client_name(). If
 *             the name is already in use, or contains non-alphanumeric
 *             characters (other than '_'), the name is modified to fit.
 *             If NULL, an arbitrary name is automatically chosen.
 * @return a new handle, or NULL on error
 *)
   mpv_create_client: function(var ctx:mpv_handle; const name: PChar): Pmpv_handle; cdecl;

(**
 * This is the same as mpv_create_client(), but the created mpv_handle is
 * treated as a weak reference. If all mpv_handles referencing a core are
 * weak references, the core is automatically destroyed. (This still goes
 * through normal uninit of course. Effectively, if the last non-weak mpv_handle
 * is destroyed, then the weak mpv_handles receive MPV_EVENT_SHUTDOWN and are
 * asked to terminate as well.)
 *
 * Note if you want to use this like refcounting: you have to be aware that
 * mpv_terminate_destroy() _and_ mpv_destroy() for the last non-weak
 * mpv_handle will block until all weak mpv_handles are destroyed.
 *)
  mpv_create_weak_client: function(var ctx:mpv_handle; const name: PChar): Pmpv_handle; cdecl;

(**
 * Load a config file. This loads and parses the file, and sets every entry in
 * the config file's default section as if mpv_set_option_string() is called.
 *
 * The filename should be an absolute path. If it isn't, the actual path used
 * is unspecified. (Note: an absolute path starts with '/' on UNIX.) If the
 * file wasn't found, MPV_ERROR_INVALID_PARAMETER is returned.
 *
 * If a fatal error happens when parsing a config file, MPV_ERROR_OPTION_ERROR
 * is returned. Errors when setting options as well as other types or errors
 * are ignored (even if options do not exist). You can still try to capture
 * the resulting error messages with mpv_request_log_messages(). Note that it's
 * possible that some options were successfully set even if any of these errors
 * happen.
 *
 * @param filename absolute path to the config file on the local filesystem
 * @return error code
 *)
mpv_load_config_file : function(var ctx:mpv_handle; filename:Pchar):longint;cdecl;

(**
 * This does nothing since mpv 0.23.0 (API version 1.24). Below is the
 * description of the old behavior.
 *
 * Stop the playback thread. This means the core will stop doing anything, and
 * only run and answer to client API requests. This is sometimes useful; for
 * example, no new frame will be queued to the video output, so doing requests
 * which have to wait on the video output can run instantly.
 *
 * Suspension is reentrant and recursive for convenience. Any thread can call
 * the suspend function multiple times, and the playback thread will remain
 * suspended until the last thread resumes it. Note that during suspension, all
 * clients still have concurrent access to the core, which is serialized through
 * a single mutex.
 *
 * Call mpv_resume() to resume the playback thread. You must call mpv_resume()
 * for each mpv_suspend() call. Calling mpv_resume() more often than
 * mpv_suspend() is not allowed.
 *
 * Calling this on an uninitialized player (see mpv_create()) will deadlock.
 *
 * @deprecated This function, as well as mpv_resume(), are deprecated, and
 *             will stop doing anything soon. Their semantics were never
 *             well-defined, and their usefulness is extremely limited. The
 *             calls will remain stubs in order to keep ABI compatibility.
 *)
    mpv_suspend : procedure(var ctx:mpv_handle);cdecl;
(**
 * See mpv_suspend().
 *)
    mpv_resume : procedure(var ctx:mpv_handle);cdecl;

(**
 * Return the internal time in microseconds. This has an arbitrary start offset,
 * but will never wrap or go backwards.
 *
 * Note that this is always the real time, and doesn't necessarily have to do
 * with playback time. For example, playback could go faster or slower due to
 * playback speed, or due to playback being paused. Use the "time-pos" property
 * instead to get the playback status.
 *
 * Unlike other libmpv APIs, this can be called at absolutely any time (even
 * within wakeup callbacks), as long as the context is valid.
 *
 * Safe to be called from mpv render API threads.
 *)
    mpv_get_time_us : function(var ctx:mpv_handle):int64;cdecl;

(**
 * Frees any data referenced by the node. It doesn't free the node itself.
 * Call this only if the mpv client API set the node. If you constructed the
 * node yourself (manually), you have to free it yourself.
 *
 * If node->format is MPV_FORMAT_NONE, this call does nothing. Likewise, if
 * the client API sets a node with this format, this function doesn't need to
 * be called. (This is just a clarification that there's no danger of anything
 * strange happening in these cases.)
 *)
 mpv_free_node_contents : procedure(var node:mpv_node);cdecl;

(**
 * Set an option. Note that you can't normally set options during runtime. It
 * works in uninitialized state (see mpv_create()), and in some cases in at
 * runtime.
 *
 * Using a format other than MPV_FORMAT_NODE is equivalent to constructing a
 * mpv_node with the given format and data, and passing the mpv_node to this
 * function.
 *
 * Note: this is semi-deprecated. For most purposes, this is not needed anymore.
 *       Starting with mpv version 0.21.0 (version 1.23) most options can be set
 *       with mpv_set_property() (and related functions), and even before
 *       mpv_initialize(). In some obscure corner cases, using this function
 *       to set options might still be required (see below, and also section
 *       "Inconsistencies between options and properties" on the manpage). Once
 *       these are resolved, the option setting functions might be fully
 *       deprecated.
 *
 *       The following options still need to be set either _before_
 *       mpv_initialize() with mpv_set_property() (or related functions), or
 *       with mpv_set_option() (or related functions) at any time:
 *              - options shadowed by deprecated properties:
 *                - demuxer (property deprecated in 0.21.0)
 *                - idle (property deprecated in 0.21.0)
 *                - fps (property deprecated in 0.21.0)
 *                - cache (property deprecated in 0.21.0)
 *                - length (property deprecated in 0.10.0)
 *                - audio-samplerate (property deprecated in 0.10.0)
 *                - audio-channels (property deprecated in 0.10.0)
 *                - audio-format (property deprecated in 0.10.0)
 *              - deprecated options shadowed by properties:
 *                - chapter (option deprecated in 0.21.0)
 *                - playlist-pos (option deprecated in 0.21.0)
 *       The deprecated properties were removed in mpv 0.23.0.
 *
 * @param name Option name. This is the same as on the mpv command line, but
 *             without the leading "--".
 * @param format see enum mpv_format.
 * @param[in] data Option value (according to the format).
 * @return error code
    *)
     mpv_set_option : function(var ctx:mpv_handle; name:Pchar; format:mpv_format; data:pointer):longint;cdecl;

(**
 * Convenience function to set an option to a string value. This is like
 * calling mpv_set_option() with MPV_FORMAT_STRING.
 *
 * @return error code
 *)
    mpv_set_option_string : function(var ctx:mpv_handle; name:Pchar; data:Pchar):longint;cdecl;

(**
 * Send a command to the player. Commands are the same as those used in
 * input.conf, except that this function takes parameters in a pre-split
 * form.
 *
 * The commands and their parameters are documented in input.rst.
 *
 * Does not use OSD and string expansion by default (unlike mpv_command_string()
 * and input.conf).
 *
 * @param[in] args NULL-terminated list of strings. Usually, the first item
 *                 is the command, and the following items are arguments.
 * @return error code
 *)
  mpv_command : function(var ctx:mpv_handle; args:PPchar):longint;cdecl;

(**
 * Same as mpv_command(), but allows passing structured data in any format.
 * In particular, calling mpv_command() is exactly like calling
 * mpv_command_node() with the format set to MPV_FORMAT_NODE_ARRAY, and
 * every arg passed in order as MPV_FORMAT_STRING.
 *
 * Does not use OSD and string expansion by default.
 *
 * The args argument can have one of the following formats:
 *
 * MPV_FORMAT_NODE_ARRAY:
 *      Positional arguments. Each entry is an argument using an arbitrary
 *      format (the format must be compatible to the used command). Usually,
 *      the first item is the command name (as MPV_FORMAT_STRING). The order
 *      of arguments is as documented in each command description.
 *
 * MPV_FORMAT_NODE_MAP:
 *      Named arguments. This requires at least an entry with the key "name"
 *      to be present, which must be a string, and contains the command name.
 *      The special entry "_flags" is optional, and if present, must be an
 *      array of strings, each being a command prefix to apply. All other
 *      entries are interpreted as arguments. They must use the argument names
 *      as documented in each command description. Some commands do not
 *      support named arguments at all, and must use MPV_FORMAT_NODE_ARRAY.
 *
 * @param[in] args mpv_node with format set to one of the values documented
 *                 above (see there for details)
 * @param[out] result Optional, pass NULL if unused. If not NULL, and if the
 *                    function succeeds, this is set to command-specific return
 *                    data. You must call mpv_free_node_contents() to free it
 *                    (again, only if the command actually succeeds).
 *                    Not many commands actually use this at all.
 * @return error code (the result parameter is not set on error)
 *)
mpv_command_node : function(var ctx:mpv_handle; var args:mpv_node; var result:mpv_node):cint;cdecl;

(**
 * This is essentially identical to mpv_command() but it also returns a result.
 *
 * Does not use OSD and string expansion by default.
 *
 * @param[in] args NULL-terminated list of strings. Usually, the first item
 *                 is the command, and the following items are arguments.
 * @param[out] result Optional, pass NULL if unused. If not NULL, and if the
 *                    function succeeds, this is set to command-specific return
 *                    data. You must call mpv_free_node_contents() to free it
 *                    (again, only if the command actually succeeds).
 *                    Not many commands actually use this at all.
 * @return error code (the result parameter is not set on error)
 *)
 mpv_command_ret: function(var ctx: mpv_handle; args: PPchar; var result: mpv_node): Integer; cdecl;

(**
 * Same as mpv_command, but use input.conf parsing for splitting arguments.
 * This is slightly simpler, but also more error prone, since arguments may
 * need quoting/escaping.
 *
 * This also has OSD and string expansion enabled by default.
 *)
mpv_command_string : function(var ctx:mpv_handle; args:Pchar):longint;cdecl;

(**
 * Same as mpv_command, but run the command asynchronously.
 *
 * Commands are executed asynchronously. You will receive a
 * MPV_EVENT_COMMAND_REPLY event. This event will also have an
 * error code set if running the command failed. For commands that
 * return data, the data is put into mpv_event_command.result.
 *
 * The only case when you do not receive an event is when the function call
 * itself fails. This happens only if parsing the command itself (or otherwise
 * validating it) fails, i.e. the return code of the API call is not 0 or
 * positive.
 *
 * Safe to be called from mpv render API threads.
 *
 * @param reply_userdata the value mpv_event.reply_userdata of the reply will
 *                       be set to (see section about asynchronous calls)
 * @param args NULL-terminated list of strings (see mpv_command())
 * @return error code (if parsing or queuing the command fails)
 *)
mpv_command_async : function(var ctx:mpv_handle; reply_userdata:cuint64; args:PPchar):longint;cdecl;

(**
 * Same as mpv_command_node(), but run it asynchronously. Basically, this
 * function is to mpv_command_node() what mpv_command_async() is to
 * mpv_command().
 *
 * See mpv_command_async() for details.
 *
 * Safe to be called from mpv render API threads.
 *
 * @param reply_userdata the value mpv_event.reply_userdata of the reply will
 *                       be set to (see section about asynchronous calls)
 * @param args as in mpv_command_node()
 * @return error code (if parsing or queuing the command fails)
 *)
mpv_command_node_async : function(var ctx:mpv_handle; reply_userdata: UInt64; var args:mpv_node):cint;cdecl;

(**
 * Signal to all async requests with the matching ID to abort. This affects
 * the following API calls:
 *
 *      mpv_command_async
 *      mpv_command_node_async
 *
 * All of these functions take a reply_userdata parameter. This API function
 * tells all requests with the matching reply_userdata value to try to return
 * as soon as possible. If there are multiple requests with matching ID, it
 * aborts all of them.
 *
 * This API function is mostly asynchronous itself. It will not wait until the
 * command is aborted. Instead, the command will terminate as usual, but with
 * some work not done. How this is signaled depends on the specific command (for
 * example, the "subprocess" command will indicate it by "killed_by_us" set to
 * true in the result). How long it takes also depends on the situation. The
 * aborting process is completely asynchronous.
 *
 * Not all commands may support this functionality. In this case, this function
 * will have no effect. The same is true if the request using the passed
 * reply_userdata has already terminated, has not been started yet, or was
 * never in use at all.
 *
 * You have to be careful of race conditions: the time during which the abort
 * request will be effective is _after_ e.g. mpv_command_async() has returned,
 * and before the command has signaled completion with MPV_EVENT_COMMAND_REPLY.
 *
 * @param reply_userdata ID of the request to be aborted (see above)
 *)

 mpv_abort_async_command: procedure(var ctx: mpv_handle; reply_userdata: UInt64); cdecl;
(**
 * Set a property to a given value. Properties are essentially variables which
 * can be queried or set at runtime. For example, writing to the pause property
 * will actually pause or unpause playback.
 *
 * If the format doesn't match with the internal format of the property, access
 * usually will fail with MPV_ERROR_PROPERTY_FORMAT. In some cases, the data
 * is automatically converted and access succeeds. For example, MPV_FORMAT_INT64
 * is always converted to MPV_FORMAT_DOUBLE, and access using MPV_FORMAT_STRING
 * usually invokes a string parser. The same happens when calling this function
 * with MPV_FORMAT_NODE: the underlying format may be converted to another
 * type if possible.
 *
 * Using a format other than MPV_FORMAT_NODE is equivalent to constructing a
 * mpv_node with the given format and data, and passing the mpv_node to this
 * function. (Before API version 1.21, this was different.)
 *
 * Note: starting with mpv 0.21.0 (client API version 1.23), this can be used to
 *       set options in general. It even can be used before mpv_initialize()
 *       has been called. If called before mpv_initialize(), setting properties
 *       not backed by options will result in MPV_ERROR_PROPERTY_UNAVAILABLE.
 *       In some cases, properties and options still conflict. In these cases,
 *       mpv_set_property() accesses the options before mpv_initialize(), and
 *       the properties after mpv_initialize(). These conflicts will be removed
 *       in mpv 0.23.0. See mpv_set_option() for further remarks.
 *
 * @param name The property name. See input.rst for a list of properties.
 * @param format see enum mpv_format.
 * @param[in] data Option value.
 * @return error code
 *)
 mpv_set_property : function(var ctx:mpv_handle; name:Pchar; format:mpv_format; data:pointer):longint;cdecl;

(**
 * Convenience function to set a property to a string value.
 *
 * This is like calling mpv_set_property() with MPV_FORMAT_STRING.
 *)
    mpv_set_property_string : function(var ctx:mpv_handle; const name:Pchar; const data:Pchar):longint; cdecl;

(**
 * Set a property asynchronously. You will receive the result of the operation
 * as MPV_EVENT_SET_PROPERTY_REPLY event. The mpv_event.error field will contain
 * the result status of the operation. Otherwise, this function is similar to
 * mpv_set_property().
 *
 * Safe to be called from mpv render API threads.
 *
 * @param reply_userdata see section about asynchronous calls
 * @param name The property name.
 * @param format see enum mpv_format.
 * @param[in] data Option value. The value will be copied by the function. It
 *                 will never be modified by the client API.
 * @return error code if sending the request failed
 *)
    mpv_set_property_async : function(var ctx:mpv_handle; reply_userdata:cuint64; name:Pchar; format:mpv_format; var data:pointer):longint;cdecl;


(**
 * Read the value of the given property.
 *
 * If the format doesn't match with the internal format of the property, access
 * usually will fail with MPV_ERROR_PROPERTY_FORMAT. In some cases, the data
 * is automatically converted and access succeeds. For example, MPV_FORMAT_INT64
 * is always converted to MPV_FORMAT_DOUBLE, and access using MPV_FORMAT_STRING
 * usually invokes a string formatter.
 *
 * @param name The property name.
 * @param format see enum mpv_format.
 * @param[out] data Pointer to the variable holding the option value. On
 *                  success, the variable will be set to a copy of the option
 *                  value. For formats that require dynamic memory allocation,
 *                  you can free the value with mpv_free() (strings) or
 *                  mpv_free_node_contents() (MPV_FORMAT_NODE).
 * @return error code
 *)
mpv_get_property : function(var ctx:mpv_handle; name:Pchar; format:mpv_format; data:pointer):longint;cdecl;

(**
 * Return the value of the property with the given name as string. This is
 * equivalent to mpv_get_property() with MPV_FORMAT_STRING.
 *
 * See MPV_FORMAT_STRING for character encoding issues.
 *
 * On error, NULL is returned. Use mpv_get_property() if you want fine-grained
 * error reporting.
 *
 * @param name The property name.
 * @return Property value, or NULL if the property can't be retrieved. Free
 *         the string with mpv_free().
 *)
    mpv_get_property_string : function(var ctx:mpv_handle; name:Pchar):Pchar;cdecl;

(**
 * Return the property as "OSD" formatted string. This is the same as
 * mpv_get_property_string, but using MPV_FORMAT_OSD_STRING.
 *
 * @return Property value, or NULL if the property can't be retrieved. Free
 *         the string with mpv_free().
 *)
    mpv_get_property_osd_string : function(var ctx:mpv_handle; name:Pchar):Pchar;cdecl;
(**
 * Get a property asynchronously. You will receive the result of the operation
 * as well as the property data with the MPV_EVENT_GET_PROPERTY_REPLY event.
 * You should check the mpv_event.error field on the reply event.
 *
 * Safe to be called from mpv render API threads.
 *
 * @param reply_userdata see section about asynchronous calls
 * @param name The property name.
 * @param format see enum mpv_format.
 * @return error code if sending the request failed
 *)
mpv_get_property_async : function(var ctx:mpv_handle; reply_userdata:cuint64; name:Pchar; format:mpv_format):longint;cdecl;

(**
 * Get a notification whenever the given property changes. You will receive
 * updates as MPV_EVENT_PROPERTY_CHANGE. Note that this is not very precise:
 * for some properties, it may not send updates even if the property changed.
 * This depends on the property, and it's a valid feature request to ask for
 * better update handling of a specific property. (For some properties, like
 * ``clock``, which shows the wall clock, this mechanism doesn't make too
 * much sense anyway.)
 *
 * Property changes are coalesced: the change events are returned only once the
 * event queue becomes empty (e.g. mpv_wait_event() would block or return
 * MPV_EVENT_NONE), and then only one event per changed property is returned.
 *
 * You always get an initial change notification. This is meant to initialize
 * the user's state to the current value of the property.
 *
 * Normally, change events are sent only if the property value changes according
 * to the requested format. mpv_event_property will contain the property value
 * as data member.
 *
 * Warning: if a property is unavailable or retrieving it caused an error,
 *          MPV_FORMAT_NONE will be set in mpv_event_property, even if the
 *          format parameter was set to a different value. In this case, the
 *          mpv_event_property.data field is invalid.
 *
 * If the property is observed with the format parameter set to MPV_FORMAT_NONE,
 * you get low-level notifications whether the property _may_ have changed, and
 * the data member in mpv_event_property will be unset. With this mode, you
 * will have to determine yourself whether the property really changed. On the
 * other hand, this mechanism can be faster and uses less resources.
 *
 * Observing a property that doesn't exist is allowed. (Although it may still
 * cause some sporadic change events.)
 *
 * Keep in mind that you will get change notifications even if you change a
 * property yourself. Try to avoid endless feedback loops, which could happen
 * if you react to the change notifications triggered by your own change.
 *
 * Only the mpv_handle on which this was called will receive the property
 * change events, or can unobserve them.
 *
 * Safe to be called from mpv render API threads.
 *
 * @param reply_userdata This will be used for the mpv_event.reply_userdata
 *                       field for the received MPV_EVENT_PROPERTY_CHANGE
 *                       events. (Also see section about asynchronous calls,
 *                       although this function is somewhat different from
 *                       actual asynchronous calls.)
 *                       If you have no use for this, pass 0.
 *                       Also see mpv_unobserve_property().
 * @param name The property name.
 * @param format see enum mpv_format. Can be MPV_FORMAT_NONE to omit values
 *               from the change events.
 * @return error code (usually fails only on OOM or unsupported format)
 *)
mpv_observe_property : function(var mpv:mpv_handle; reply_userdata:cuint64; name:Pchar; format:mpv_format):longint;cdecl;

(**
 * Undo mpv_observe_property(). This will remove all observed properties for
 * which the given number was passed as reply_userdata to mpv_observe_property.
 *
 * Safe to be called from mpv render API threads.
 *
 * @param registered_reply_userdata ID that was passed to mpv_observe_property
 * @return negative value is an error code, >=0 is number of removed properties
 *         on success (includes the case when 0 were removed)
 *)
mpv_unobserve_property : function(var mpv:mpv_handle; registered_reply_userdata:cuint64):longint;cdecl;

(**
 * Return a string describing the event. For unknown events, NULL is returned.
 *
 * Note that all events actually returned by the API will also yield a non-NULL
 * string with this function.
 *
 * @param event event ID, see see enum mpv_event_id
 * @return A static string giving a short symbolic name of the event. It
 *         consists of lower-case alphanumeric characters and can include "-"
 *         characters. This string is suitable for use in e.g. scripting
 *         interfaces.
 *         The string is completely static, i.e. doesn't need to be deallocated,
 *         and is valid forever.
 *)
    mpv_event_name : function(event:mpv_event_id):Pchar;cdecl;

(**
 * Convert the given src event to a mpv_node, and set *dst to the result. *dst
 * is set to a MPV_FORMAT_NODE_MAP, with fields for corresponding mpv_event and
 * mpv_event.data/mpv_event_* fields.
 *
 * The exact details are not completely documented out of laziness. A start
 * is located in the "Events" section of the manpage.
 *
 * *dst may point to newly allocated memory, or pointers in mpv_event. You must
 * copy the entire mpv_node if you want to reference it after mpv_event becomes
 * invalid (such as making a new mpv_wait_event() call, or destroying the
 * mpv_handle from which it was returned). Call mpv_free_node_contents() to free
 * any memory allocations made by this API function.
 *
 * Safe to be called from mpv render API threads.
 *
 * @param dst Target. This is not read and fully overwritten. Must be released
 *            with mpv_free_node_contents(). Do not write to pointers returned
 *            by it. (On error, this may be left as an empty node.)
 * @param src The source event. Not modified (it's not const due to the author's
 *            prejudice of the C version of const).
 * @return error code (MPV_ERROR_NOMEM only, if at all)
 *)
 mpv_event_to_node: function (dst: Pmpv_node; src: Pmpv_event): Integer; cdecl; //new

(**
 * Enable or disable the given event.
 *
 * Some events are enabled by default. Some events can't be disabled.
 *
 * (Informational note: currently, all events are enabled by default, except
 *  MPV_EVENT_TICK.)
 *
 * Safe to be called from mpv render API threads.
 *
 * @param event See enum mpv_event_id.
 * @param enable 1 to enable receiving this event, 0 to disable it.
 * @return error code
 *)
    mpv_request_event : function(var ctx:mpv_handle; event:mpv_event_id; enable:longint):longint;cdecl;

(**
 * Enable or disable receiving of log messages. These are the messages the
 * command line player prints to the terminal. This call sets the minimum
 * required log level for a message to be received with MPV_EVENT_LOG_MESSAGE.
 *
 * @param min_level Minimal log level as string. Valid log levels:
 *                      no fatal error warn info v debug trace
 *                  The value "no" disables all messages. This is the default.
 *                  An exception is the value "terminal-default", which uses the
 *                  log level as set by the "--msg-level" option. This works
 *                  even if the terminal is disabled. (Since API version 1.19.)
 *                  Also see mpv_log_level.
 * @return error code
 *)
    mpv_request_log_messages : function(var ctx:mpv_handle; min_level:Pchar):longint;cdecl;

(**
 * Wait for the next event, or until the timeout expires, or if another thread
 * makes a call to mpv_wakeup(). Passing 0 as timeout will never wait, and
 * is suitable for polling.
 *
 * The internal event queue has a limited size (per client handle). If you
 * don't empty the event queue quickly enough with mpv_wait_event(), it will
 * overflow and silently discard further events. If this happens, making
 * asynchronous requests will fail as well (with MPV_ERROR_EVENT_QUEUE_FULL).
 *
 * Only one thread is allowed to call this on the same mpv_handle at a time.
 * The API won't complain if more than one thread calls this, but it will cause
 * race conditions in the client when accessing the shared mpv_event struct.
 * Note that most other API functions are not restricted by this, and no API
 * function internally calls mpv_wait_event(). Additionally, concurrent calls
 * to different mpv_handles are always safe.
 *
 * As long as the timeout is 0, this is safe to be called from mpv render API
 * threads.
 *
 * @param timeout Timeout in seconds, after which the function returns even if
 *                no event was received. A MPV_EVENT_NONE is returned on
 *                timeout. A value of 0 will disable waiting. Negative values
 *                will wait with an infinite timeout.
 * @return A struct containing the event ID and other data. The pointer (and
 *         fields in the struct) stay valid until the next mpv_wait_event()
 *         call, or until the mpv_handle is destroyed. You must not write to
 *         the struct, and all memory referenced by it will be automatically
 *         released by the API on the next mpv_wait_event() call, or when the
 *         context is destroyed. The return value is never NULL.
 *)
    mpv_wait_event : function(var ctx:mpv_handle; timeout:double):Pmpv_event;cdecl;

(**
 * Interrupt the current mpv_wait_event() call. This will wake up the thread
 * currently waiting in mpv_wait_event(). If no thread is waiting, the next
 * mpv_wait_event() call will return immediately (this is to avoid lost
 * wakeups).
 *
 * mpv_wait_event() will receive a MPV_EVENT_NONE if it's woken up due to
 * this call. But note that this dummy event might be skipped if there are
 * already other events queued. All what counts is that the waiting thread
 * is woken up at all.
 *
 * Safe to be called from mpv render API threads.
 *)
    mpv_wakeup : procedure(var ctx:mpv_handle);cdecl;

type
  mpv_set_wakeup_callback_cb = procedure(d: Pointer); cdecl;
  mpv_callback = procedure(d: Pointer); cdecl;

var
(**
 * Set a custom function that should be called when there are new events. Use
 * this if blocking in mpv_wait_event() to wait for new events is not feasible.
 *
 * Keep in mind that the callback will be called from foreign threads. You
 * must not make any assumptions of the environment, and you must return as
 * soon as possible (i.e. no long blocking waits). Exiting the callback through
 * any other means than a normal return is forbidden (no throwing exceptions,
 * no longjmp() calls). You must not change any local thread state (such as
 * the C floating point environment).
 *
 * You are not allowed to call any client API functions inside of the callback.
 * In particular, you should not do any processing in the callback, but wake up
 * another thread that does all the work. The callback is meant strictly for
 * notification only, and is called from arbitrary core parts of the player,
 * that make no considerations for reentrant API use or allowing the callee to
 * spend a lot of time doing other things. Keep in mind that it's also possible
 * that the callback is called from a thread while a mpv API function is called
 * (i.e. it can be reentrant).
 *
 * In general, the client API expects you to call mpv_wait_event() to receive
 * notifications, and the wakeup callback is merely a helper utility to make
 * this easier in certain situations. Note that it's possible that there's
 * only one wakeup callback invocation for multiple events. You should call
 * mpv_wait_event() with no timeout until MPV_EVENT_NONE is reached, at which
 * point the event queue is empty.
 *
 * If you actually want to do processing in a callback, spawn a thread that
 * does nothing but call mpv_wait_event() in a loop and dispatches the result
 * to a callback.
 *
 * Only one wakeup callback can be set.
 *
 * @param cb function that should be called if a wakeup is required
 * @param d arbitrary userdata passed to cb
 *)
    mpv_set_wakeup_callback : procedure(var ctx:mpv_handle; cb:mpv_callback; d:pointer);cdecl;

(**
 * Block until all asynchronous requests are done. This affects functions like
 * mpv_command_async(), which return immediately and return their result as
 * events.
 *
 * This is a helper, and somewhat equivalent to calling mpv_wait_event() in a
 * loop until all known asynchronous requests have sent their reply as event,
 * except that the event queue is not emptied.
 *
 * In case you called mpv_suspend() before, this will also forcibly reset the
 * suspend counter of the given handle.
 *)
 mpv_wait_async_requests: procedure(var ctx:mpv_handle); cdecl; //new


(**
 * A hook is like a synchronous event that blocks the player. You register
 * a hook handler with this function. You will get an event, which you need
 * to handle, and once things are ready, you can let the player continue with
 * mpv_hook_continue().
 *
 * Currently, hooks can't be removed explicitly. But they will be implicitly
 * removed if the mpv_handle it was registered with is destroyed. This also
 * continues the hook if it was being handled by the destroyed mpv_handle (but
 * this should be avoided, as it might mess up order of hook execution).
 *
 * Hook handlers are ordered globally by priority and order of registration.
 * Handlers for the same hook with same priority are invoked in order of
 * registration (the handler registered first is run first). Handlers with
 * lower priority are run first (which seems backward).
 *
 * See the "Hooks" section in the manpage to see which hooks are currently
 * defined.
 *
 * Some hooks might be reentrant (so you get multiple MPV_EVENT_HOOK for the
 * same hook). If this can happen for a specific hook type, it will be
 * explicitly documented in the manpage.
 *
 * Only the mpv_handle on which this was called will receive the hook events,
 * or can "continue" them.
 *
 * @param reply_userdata This will be used for the mpv_event.reply_userdata
 *                       field for the received MPV_EVENT_HOOK events.
 *                       If you have no use for this, pass 0.
 * @param name The hook name. This should be one of the documented names. But
 *             if the name is unknown, the hook event will simply be never
 *             raised.
 * @param priority See remarks above. Use 0 as a neutral default.
 * @return error code (usually fails only on OOM)
 *)
 mpv_hook_add: function(var ctx:mpv_handle; reply_userdata: UInt64; const name: PChar; priority: Integer): Integer; cdecl; //new

(**
 * Respond to a MPV_EVENT_HOOK event. You must call this after you have handled
 * the event. There is no way to "cancel" or "stop" the hook.
 *
 * Calling this will will typically unblock the player for whatever the hook
 * is responsible for (e.g. for the "on_load" hook it lets it continue
 * playback).
 *
 * It is explicitly undefined behavior to call this more than once for each
 * MPV_EVENT_HOOK, to pass an incorrect ID, or to call this on a mpv_handle
 * different from the one that registered the handler and received the event.
 *
 * @param id This must be the value of the mpv_event_hook.id field for the
 *           corresponding MPV_EVENT_HOOK.
 * @return error code
 *)
 mpv_hook_continue: function(var ctx:mpv_handle; id: UInt64): Integer; cdecl; //new

(**
 * Return a UNIX file descriptor referring to the read end of a pipe. This
 * pipe can be used to wake up a poll() based processing loop. The purpose of
 * this function is very similar to mpv_set_wakeup_callback(), and provides
 * a primitive mechanism to handle coordinating a foreign event loop and the
 * libmpv event loop. The pipe is non-blocking. It's closed when the mpv_handle
 * is destroyed. This function always returns the same value (on success).
 *
 * This is in fact implemented using the same underlying code as for
 * mpv_set_wakeup_callback() (though they don't conflict), and it is as if each
 * callback invocation writes a single 0 byte to the pipe. When the pipe
 * becomes readable, the code calling poll() (or select()) on the pipe should
 * read all contents of the pipe and then call mpv_wait_event(c, 0) until
 * no new events are returned. The pipe contents do not matter and can just
 * be discarded. There is not necessarily one byte per readable event in the
 * pipe. For example, the pipes are non-blocking, and mpv won't block if the
 * pipe is full. Pipes are normally limited to 4096 bytes, so if there are
 * more than 4096 events, the number of readable bytes can not equal the number
 * of events queued. Also, it's possible that mpv does not write to the pipe
 * once it's guaranteed that the client was already signaled. See the example
 * below how to do it correctly.
 *
 * Example:
 *
 *  int pipefd = mpv_get_wakeup_pipe(mpv);
 *  if (pipefd < 0)
 *      error();
 *  while (1) {
 *      struct pollfd pfds[1] = {
 *          { .fd = pipefd, .events = POLLIN },
 *      };
 *      // Wait until there are possibly new mpv events.
 *      poll(pfds, 1, -1);
 *      if (pfds[0].revents & POLLIN) {
 *          // Empty the pipe. Doing this before calling mpv_wait_event()
 *          // ensures that no wakeups are missed. It's not so important to
 *          // make sure the pipe is really empty (it will just cause some
 *          // additional wakeups in unlikely corner cases).
 *          char unused[256];
 *          read(pipefd, unused, sizeof(unused));
 *          while (1) {
 *              mpv_event *ev = mpv_wait_event(mpv, 0);
 *              // If MPV_EVENT_NONE is received, the event queue is empty.
 *              if (ev->event_id == MPV_EVENT_NONE)
 *                  break;
 *              // Process the event.
 *              ...
 *          }
 *      }
 *  }
 *
 * @deprecated this function will be removed in the future. If you need this
 *             functionality, use mpv_set_wakeup_callback(), create a pipe
 *             manually, and call write() on your pipe in the callback.
 *
 * @return A UNIX FD of the read end of the wakeup pipe, or -1 on error.
 *         On MS Windows/MinGW, this will always return -1.
 *)
 mpv_get_wakeup_pipe: function(var ctx: mpv_handle): Integer; cdecl;

(**
 * This is used for additional APIs that are not strictly part of the core API.
 * See the individual mpv_sub_api member values.
 *
 * @deprecated use render.h
 *)
 mpv_get_sub_api: function(var ctx: mpv_handle; sub_api: mpv_sub_api): Pointer; cdecl; //new

 Function Check_libmpv:boolean;
 function Load_libmpv(lib : pchar):boolean;
 procedure Free_libmpv;

implementation


  uses
    SysUtils, dynlibs;

  var
    hlib : tlibhandle;


  procedure Free_libmpv;
    begin
      FreeLibrary(hlib);
      mpv_client_api_version:=nil;
      mpv_error_string:=nil;
      mpv_free:=nil;
      mpv_client_name:=nil;
      mpv_create:=nil;
      mpv_initialize:=nil;
      mpv_detach_destroy:=nil;
      mpv_terminate_destroy:=nil;
      mpv_load_config_file:=nil;
      mpv_suspend:=nil;
      mpv_resume:=nil;
      mpv_get_time_us:=nil;
      mpv_free_node_contents:=nil;
      mpv_set_option:=nil;
      mpv_set_option_string:=nil;
      mpv_command:=nil;
      mpv_command_string:=nil;
      mpv_command_async:=nil;
      mpv_set_property:=nil;
      mpv_set_property_string:=nil;
      mpv_set_property_async:=nil;
      mpv_get_property:=nil;
      mpv_get_property_string:=nil;
      mpv_get_property_osd_string:=nil;
      mpv_get_property_async:=nil;
      mpv_observe_property:=nil;
      mpv_unobserve_property:=nil;
      mpv_event_name:=nil;
      mpv_request_event:=nil;
      mpv_request_log_messages:=nil;
      mpv_wait_event:=nil;
      mpv_wakeup:=nil;
      mpv_set_wakeup_callback:=nil;
      mpv_get_wakeup_pipe:=nil;
    end;


    function Load_libmpv(lib: pchar): boolean;
    begin

      hlib:=LoadLibrary(lib);
      Result:=  hlib<>0 ;
      if not Result then
        exit;

      pointer(mpv_client_api_version):=GetProcAddress(hlib,'mpv_client_api_version');
      pointer(mpv_error_string):=GetProcAddress(hlib,'mpv_error_string');
      pointer(mpv_free):=GetProcAddress(hlib,'mpv_free');
      pointer(mpv_client_name):=GetProcAddress(hlib,'mpv_client_name');
      pointer(mpv_create):=GetProcAddress(hlib,'mpv_create');
      pointer(mpv_initialize):=GetProcAddress(hlib,'mpv_initialize');
      pointer(mpv_detach_destroy):=GetProcAddress(hlib,'mpv_detach_destroy');
      pointer(mpv_terminate_destroy):=GetProcAddress(hlib,'mpv_terminate_destroy');
      pointer(mpv_load_config_file):=GetProcAddress(hlib,'mpv_load_config_file');
      pointer(mpv_suspend):=GetProcAddress(hlib,'mpv_suspend');
      pointer(mpv_resume):=GetProcAddress(hlib,'mpv_resume');
      pointer(mpv_get_time_us):=GetProcAddress(hlib,'mpv_get_time_us');
      pointer(mpv_free_node_contents):=GetProcAddress(hlib,'mpv_free_node_contents');
      pointer(mpv_set_option):=GetProcAddress(hlib,'mpv_set_option');
      pointer(mpv_set_option_string):=GetProcAddress(hlib,'mpv_set_option_string');
      pointer(mpv_command):=GetProcAddress(hlib,'mpv_command');
      pointer(mpv_command_string):=GetProcAddress(hlib,'mpv_command_string');
      pointer(mpv_command_async):=GetProcAddress(hlib,'mpv_command_async');
      pointer(mpv_command_node):=GetProcAddress(hlib,'mpv_command_node');
      pointer(mpv_set_property):=GetProcAddress(hlib,'mpv_set_property');
      pointer(mpv_set_property_string):=GetProcAddress(hlib,'mpv_set_property_string');
      pointer(mpv_set_property_async):=GetProcAddress(hlib,'mpv_set_property_async');
      pointer(mpv_get_property):=GetProcAddress(hlib,'mpv_get_property');
      pointer(mpv_get_property_string):=GetProcAddress(hlib,'mpv_get_property_string');
      pointer(mpv_get_property_osd_string):=GetProcAddress(hlib,'mpv_get_property_osd_string');
      pointer(mpv_get_property_async):=GetProcAddress(hlib,'mpv_get_property_async');
      pointer(mpv_observe_property):=GetProcAddress(hlib,'mpv_observe_property');
      pointer(mpv_unobserve_property):=GetProcAddress(hlib,'mpv_unobserve_property');
      pointer(mpv_event_name):=GetProcAddress(hlib,'mpv_event_name');
      pointer(mpv_request_event):=GetProcAddress(hlib,'mpv_request_event');
      pointer(mpv_request_log_messages):=GetProcAddress(hlib,'mpv_request_log_messages');
      pointer(mpv_wait_event):=GetProcAddress(hlib,'mpv_wait_event');
      pointer(mpv_wakeup):=GetProcAddress(hlib,'mpv_wakeup');
      pointer(mpv_set_wakeup_callback):=GetProcAddress(hlib,'mpv_set_wakeup_callback');
      pointer(mpv_get_wakeup_pipe):=GetProcAddress(hlib,'mpv_get_wakeup_pipe');
    end;

    function Check_libmpv: boolean;
  var
    MustFree :boolean ;
  begin
    // no error
    MustFree := hlib = 0;

    if not Load_libmpv(External_libraryV2) then
      Load_libmpv(External_libraryV1);

    Result:=false;
    // exit, report error
    if (hlib = 0) then
      begin
      end
    else
      begin
        Result:=true;
        if MustFree then
           begin
             UnloadLibrary(hlib);
             hlib := 0;
           end;
      end;

  end;

initialization
    hlib := 0;
finalization

end.
