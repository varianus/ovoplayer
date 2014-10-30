
{$mode objfpc}
unit client;
interface

{
  Automatically converted by H2Pas 1.0.0 from c:\source\ovoplayer\trunk\src\import-engines\client.tmp.h
  The following command line parameters were used:
    -e
    -p
    -s
    -S
    -D
    -P
    -v
    -w
    -o
    c:\source\ovoplayer\trunk\src\import-engines\client.pas
    c:\source\ovoplayer\trunk\src\import-engines\client.tmp.h
}

  const
    External_library='kernel32'; {Setup as you need}

  { Pointers to basic pascal types, inserted by h2pas conversion program.}
  Type
    PLongint  = ^Longint;
    PSmallInt = ^SmallInt;
    PByte     = ^Byte;
    PWord     = ^Word;
    PDWord    = ^DWord;
    PDouble   = ^Double;

  Type
  Pchar  = ^char;
  Pmpv_end_file_reason  = ^mpv_end_file_reason;
  Pmpv_error  = ^mpv_error;
  Pmpv_event  = ^mpv_event;
  Pmpv_event_client_message  = ^mpv_event_client_message;
  Pmpv_event_end_file  = ^mpv_event_end_file;
  Pmpv_event_id  = ^mpv_event_id;
  Pmpv_event_log_message  = ^mpv_event_log_message;
  Pmpv_event_property  = ^mpv_event_property;
  Pmpv_event_script_input_dispatch  = ^mpv_event_script_input_dispatch;
  Pmpv_format  = ^mpv_format;
  Pmpv_handle  = ^mpv_handle;
  Pmpv_log_level  = ^mpv_log_level;
  Pmpv_node  = ^mpv_node;
  Pmpv_node_list  = ^mpv_node_list;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}




{$ifndef MPV_CLIENT_API_H_}
{$define MPV_CLIENT_API_H_}  
{$include <stdint.h>}



  function MPV_MAKE_VERSION(major,minor : longint) : longint;  

  function MPV_CLIENT_API_VERSION : longint; { return type might be wrong }



  var
    mpv_client_api_version : function:dword;cdecl;


  type




















    Pmpv_error = ^mpv_error;
    mpv_error =  Longint;
    Const
      MPV_ERROR_SUCCESS = 0;
      MPV_ERROR_EVENT_QUEUE_FULL = -(1);
      MPV_ERROR_NOMEM = -(2);
      MPV_ERROR_UNINITIALIZED = -(3);
      MPV_ERROR_INVALID_PARAMETER = -(4);
      MPV_ERROR_OPTION_NOT_FOUND = -(5);
      MPV_ERROR_OPTION_FORMAT = -(6);
      MPV_ERROR_OPTION_ERROR = -(7);
      MPV_ERROR_PROPERTY_NOT_FOUND = -(8);
      MPV_ERROR_PROPERTY_FORMAT = -(9);
      MPV_ERROR_PROPERTY_UNAVAILABLE = -(10);
      MPV_ERROR_PROPERTY_ERROR = -(11);
      MPV_ERROR_COMMAND = -(12);
      MPV_ERROR_LOADING_FAILED = -(13);
      MPV_ERROR_AO_INIT_FAILED = -(14);
      MPV_ERROR_VO_INIT_FAILED = -(15);
      MPV_ERROR_NOTHING_TO_PLAY = -(16);
      MPV_ERROR_UNKNOWN_FORMAT = -(17);
;


  var
    mpv_error_string : function(error:longint):Pchar;cdecl;

    mpv_free : procedure(var data:pointer);cdecl;

    mpv_client_name : function(var ctx:mpv_handle):Pchar;cdecl;

    mpv_create : function:Pmpv_handle;cdecl;

    mpv_initialize : function(var ctx:mpv_handle):longint;cdecl;

    mpv_detach_destroy : procedure(var ctx:mpv_handle);cdecl;

    mpv_terminate_destroy : procedure(var ctx:mpv_handle);cdecl;

    mpv_load_config_file : function(var ctx:mpv_handle; filename:Pchar):longint;cdecl;

    mpv_suspend : procedure(var ctx:mpv_handle);cdecl;

    mpv_resume : procedure(var ctx:mpv_handle);cdecl;

    mpv_get_time_us : function(var ctx:mpv_handle):int64_t;cdecl;











  type
    Pmpv_format = ^mpv_format;
    mpv_format =  Longint;
    Const
      MPV_FORMAT_NONE = 0;
      MPV_FORMAT_STRING = 1;
      MPV_FORMAT_OSD_STRING = 2;
      MPV_FORMAT_FLAG = 3;
      MPV_FORMAT_INT64 = 4;
      MPV_FORMAT_DOUBLE = 5;
      MPV_FORMAT_NODE = 6;
      MPV_FORMAT_NODE_ARRAY = 7;
      MPV_FORMAT_NODE_MAP = 8;
;



(* error 
        int64_t int64;  /** valid if format==MPV_FORMAT_INT64  */
 in declarator_list *)





  type
    Pmpv_node = ^mpv_node;
    mpv_node = record
        _string : Pchar;
        flag : longint;
        double_ : double;
        list : Pmpv_node_list;
        format : mpv_format;
      end;





    Pmpv_node_list = ^mpv_node_list;
    mpv_node_list = record
        num : longint;
        values : Pmpv_node;
        keys : ^Pchar;
      end;


  var
    mpv_free_node_contents : procedure(var node:mpv_node);cdecl;

    mpv_set_option : function(var ctx:mpv_handle; name:Pchar; format:mpv_format; var data:pointer):longint;cdecl;

    mpv_set_option_string : function(var ctx:mpv_handle; name:Pchar; data:Pchar):longint;cdecl;

    mpv_command : function(var ctx:mpv_handle; args:PPchar):longint;cdecl;

    mpv_command_node : function(var ctx:mpv_handle; var args:mpv_node; var result:mpv_node):longint;cdecl;

    mpv_command_string : function(var ctx:mpv_handle; args:Pchar):longint;cdecl;

    mpv_command_async : function(var ctx:mpv_handle; reply_userdata:uint64_t; args:PPchar):longint;cdecl;

    mpv_command_node_async : function(var ctx:mpv_handle; reply_userdata:uint64_t; var args:mpv_node):longint;cdecl;

    mpv_set_property : function(var ctx:mpv_handle; name:Pchar; format:mpv_format; var data:pointer):longint;cdecl;

    mpv_set_property_string : function(var ctx:mpv_handle; name:Pchar; data:Pchar):longint;cdecl;

    mpv_set_property_async : function(var ctx:mpv_handle; reply_userdata:uint64_t; name:Pchar; format:mpv_format; var data:pointer):longint;cdecl;

    mpv_get_property : function(var ctx:mpv_handle; name:Pchar; format:mpv_format; var data:pointer):longint;cdecl;

    mpv_get_property_string : function(var ctx:mpv_handle; name:Pchar):Pchar;cdecl;

    mpv_get_property_osd_string : function(var ctx:mpv_handle; name:Pchar):Pchar;cdecl;

    mpv_get_property_async : function(var ctx:mpv_handle; reply_userdata:uint64_t; name:Pchar; format:mpv_format):longint;cdecl;

    mpv_observe_property : function(var mpv:mpv_handle; reply_userdata:uint64_t; name:Pchar; format:mpv_format):longint;cdecl;

    mpv_unobserve_property : function(var mpv:mpv_handle; registered_reply_userdata:uint64_t):longint;cdecl;

























  type
    Pmpv_event_id = ^mpv_event_id;
    mpv_event_id =  Longint;
    Const
      MPV_EVENT_NONE = 0;
      MPV_EVENT_SHUTDOWN = 1;
      MPV_EVENT_LOG_MESSAGE = 2;
      MPV_EVENT_GET_PROPERTY_REPLY = 3;
      MPV_EVENT_SET_PROPERTY_REPLY = 4;
      MPV_EVENT_COMMAND_REPLY = 5;
      MPV_EVENT_START_FILE = 6;
      MPV_EVENT_END_FILE = 7;
      MPV_EVENT_FILE_LOADED = 8;
      MPV_EVENT_TRACKS_CHANGED = 9;
      MPV_EVENT_TRACK_SWITCHED = 10;
      MPV_EVENT_IDLE = 11;
      MPV_EVENT_PAUSE = 12;
      MPV_EVENT_UNPAUSE = 13;
      MPV_EVENT_TICK = 14;
      MPV_EVENT_SCRIPT_INPUT_DISPATCH = 15;
      MPV_EVENT_CLIENT_MESSAGE = 16;
      MPV_EVENT_VIDEO_RECONFIG = 17;
      MPV_EVENT_AUDIO_RECONFIG = 18;
      MPV_EVENT_METADATA_UPDATE = 19;
      MPV_EVENT_SEEK = 20;
      MPV_EVENT_PLAYBACK_RESTART = 21;
      MPV_EVENT_PROPERTY_CHANGE = 22;
      MPV_EVENT_CHAPTER_CHANGE = 23;
;


  var
    mpv_event_name : function(event:mpv_event_id):Pchar;cdecl;




  type
    Pmpv_event_property = ^mpv_event_property;
    mpv_event_property = record
        name : Pchar;
        format : mpv_format;
        data : pointer;
      end;


    Pmpv_log_level = ^mpv_log_level;
    mpv_log_level =  Longint;
    Const
      MPV_LOG_LEVEL_NONE = 0;
      MPV_LOG_LEVEL_FATAL = 10;
      MPV_LOG_LEVEL_ERROR = 20;
      MPV_LOG_LEVEL_WARN = 30;
      MPV_LOG_LEVEL_INFO = 40;
      MPV_LOG_LEVEL_V = 50;
      MPV_LOG_LEVEL_DEBUG = 60;
      MPV_LOG_LEVEL_TRACE = 70;
;





  type
    Pmpv_event_log_message = ^mpv_event_log_message;
    mpv_event_log_message = record
        prefix : Pchar;
        level : Pchar;
        text : Pchar;
        log_level : mpv_log_level;
      end;





    Pmpv_end_file_reason = ^mpv_end_file_reason;
    mpv_end_file_reason =  Longint;
    Const
      MPV_END_FILE_REASON_EOF = 0;
      MPV_END_FILE_REASON_STOP = 2;
      MPV_END_FILE_REASON_QUIT = 3;
      MPV_END_FILE_REASON_ERROR = 4;
;



  type
    Pmpv_event_end_file = ^mpv_event_end_file;
    mpv_event_end_file = record
        reason : longint;
        error : longint;
      end;



    Pmpv_event_script_input_dispatch = ^mpv_event_script_input_dispatch;
    mpv_event_script_input_dispatch = record
        arg0 : longint;
        _type : Pchar;
      end;


    Pmpv_event_client_message = ^mpv_event_client_message;
    mpv_event_client_message = record
        num_args : longint;
        args : ^Pchar;
      end;





    Pmpv_event = ^mpv_event;
    mpv_event = record
        event_id : mpv_event_id;
        error : longint;
        reply_userdata : uint64_t;
        data : pointer;
      end;


  var
    mpv_request_event : function(var ctx:mpv_handle; event:mpv_event_id; enable:longint):longint;cdecl;

    mpv_request_log_messages : function(var ctx:mpv_handle; min_level:Pchar):longint;cdecl;

    mpv_wait_event : function(var ctx:mpv_handle; timeout:double):Pmpv_event;cdecl;

    mpv_wakeup : procedure(var ctx:mpv_handle);cdecl;

    mpv_set_wakeup_callback : procedure(var ctx:mpv_handle; cb:procedure (var d:pointer); var d:pointer);cdecl;

    mpv_get_wakeup_pipe : function(var ctx:mpv_handle):longint;cdecl;
{$endif}

implementation

  function MPV_MAKE_VERSION(major,minor : longint) : longint;
  begin
    MPV_MAKE_VERSION:=((major shl 16) or minor) or 0;
  end;

  function MPV_CLIENT_API_VERSION : longint; { return type might be wrong }
    begin
      MPV_CLIENT_API_VERSION:=MPV_MAKE_VERSION(1,9);
    end;

  uses
    SysUtils, dynlibs;

  var
    hlib : tlibhandle;


  procedure Freeclient;
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
      mpv_command_node:=nil;
      mpv_command_string:=nil;
      mpv_command_async:=nil;
      mpv_command_node_async:=nil;
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


  procedure Loadclient(lib : pchar);
    begin
      Freeclient;
      hlib:=LoadLibrary(lib);
      if hlib=0 then
        raise Exception.Create(format('Could not load library: %s',[lib]));

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
      pointer(mpv_command_node):=GetProcAddress(hlib,'mpv_command_node');
      pointer(mpv_command_string):=GetProcAddress(hlib,'mpv_command_string');
      pointer(mpv_command_async):=GetProcAddress(hlib,'mpv_command_async');
      pointer(mpv_command_node_async):=GetProcAddress(hlib,'mpv_command_node_async');
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


initialization
  Loadclient('client');
finalization
  Freeclient;

end.
