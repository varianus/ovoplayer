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
{ A little note about this binding.

  This is only a very small extract of Gstreamer API.
  Converting all the header into pascal would have taken a lot of time, even
  with the help of H2Pas tools.
  So I've decided to port only used functions, and most parameters are handled
  as generic pointer.
  This unit (and related audioengine_gstreamer) depends on glib2.
}

{$I ovoplayer.inc}
unit gstreamer;

{$mode objfpc}{$H+}


interface
uses glib2;

const
  GST_STATE_VOID_PENDING        = 0;
  GST_STATE_NULL                = 1;
  GST_STATE_READY               = 2;
  GST_STATE_PAUSED              = 3;
  GST_STATE_PLAYING             = 4;

  GST_SEEK_FLAG_NONE            = 0;
  GST_SEEK_FLAG_FLUSH           = (1 shl 0);
  GST_SEEK_FLAG_ACCURATE        = (1 shl 1);
  GST_SEEK_FLAG_KEY_UNIT        = (1 shl 2);
  GST_SEEK_FLAG_SEGMENT         = (1 shl 3);
  GST_SEEK_FLAG_SKIP            = (1 shl 4);

  GST_SEEK_TYPE_NONE            = 0;
  GST_SEEK_TYPE_CUR             = 1;
  GST_SEEK_TYPE_SET             = 2;
  GST_SEEK_TYPE_END             = 3;
  GST_CLOCK_TIME_NONE           = -1;
  GST_FORMAT_TIME               =  3;

  GST_MESSAGE_EOS               = (1 shl 0);

type
  gstMiniObject_0_10 =  record
    instance: TGTypeInstance;
    refcount: gint;
    flags : guint;
    _gst_reserved: gpointer;
  end;

  gstMiniObject_1_0 =  record
    _type: GType;
    refcount: gint;
    lockstate: gint;
    flags : guint;
    _gst_reserved1: gpointer;
    _gst_reserved2: gpointer;
    _gst_reserved3: gpointer;
    n_qdata : guint;
    qdata : gpointer;
  end;

  pgstmessage_0_10 = ^gstMessage_0_10;
  gstMessage_0_10 = packed record
    MiniObject: gstMiniObject_0_10;
    lock: gpointer;
    cond: gpointer;
    _type: guint;
  end;

  pgstmessage_1_0 = ^gstMessage_1_0;
  gstMessage_1_0 = packed record
    MiniObject: gstMiniObject_1_0;
    _type: guint;
    timestamp: guint64;
    dummy : pointer;
    seqnum: guint32;

  end;


type

  GstElementState = dword;
  pTimeVal = ^word;

  gst_init_check_t = function (var argc: Integer; var args: PAnsiChar; var Error:PGError):gBoolean; cdecl;
  gst_element_factory_make_t = function (const factoryname: PChar;const name: PChar): Pointer; cdecl;
  gst_bin_new_t = FUNCTION (const name: PChar): Pointer; cdecl;
  gst_is_initialized_t = FUNCTION (): gBoolean; cdecl;
  gst_bin_add_t = function (bin: Pointer;element: Pointer): Boolean; cdecl;
  gst_bin_add_many_t = procedure(bin: Pointer;element_1: Pointer;additional: array of const); cdecl;
  gst_element_link_many_t = function (element_1: Pointer;element_2: Pointer; additional: array of const): Boolean; cdecl;
  gst_element_set_state_t = function (element: Pointer;state: GstElementState): GstElementState; cdecl;
  gst_element_get_state_T = function (element: Pointer;var state: GstElementState ; var pending: GstElementState;timeout: pTimeVal): GstElementState; cdecl;
  gst_pipeline_get_bus_T  = function (pipeline: Pointer): pointer; cdecl;
  gst_object_unref_T      = function (Obj: Pointer): dword; cdecl;
  gst_element_query_position_1_0_T   = function (element: Pointer; Format:DWORD; var CURR:int64 ): boolean; cdecl;
  gst_element_query_position_0_10_T   = function (element: Pointer; var Format:DWORD; var CURR:int64 ): boolean; cdecl;
  gst_element_seek_simple_t      = function (element: pointer; format :Dword; flags :dword;  cur:Int64): boolean; cdecl;

  gst_element_seek_t      = function  (element: pointer; rate :double; format :Dword; flags :dword;
                                                         cur_type: dword;  cur:Int64;
                                                         stop_type: dword; stop:Int64): boolean; cdecl;

//  gst_bus_func = function (bus: pointer; message:pgstmessage; user_data:pointer): boolean; cdecl;

  gst_bus_add_watch_T = function (bus: pointer;func:Pointer; user_data:pointer): dword; cdecl;


var
  GST_Lib_1_0 :boolean;
  libgst_dynamic_dll_error: string;

  gst_init_check :gst_init_check_t;
  gst_element_factory_make : gst_element_factory_make_t;
  gst_bin_new : gst_bin_new_t;
  gst_bin_add : gst_bin_add_t;
  gst_bin_add_many : gst_bin_add_many_t;
  gst_element_link_many : gst_element_link_many_t;
  gst_element_set_state : gst_element_set_state_t;
  gst_element_get_state : gst_element_get_state_T;
  gst_pipeline_get_bus  : gst_pipeline_get_bus_T;
  gst_bus_add_watch     : gst_bus_add_watch_T;
  gst_object_unref      : gst_object_unref_T;
  gst_element_seek      : gst_element_seek_t;
  gst_element_seek_simple      : gst_element_seek_simple_t;
  gst_element_query_position : gst_element_query_position_1_0_T;
  gst_element_query_position_OLD : gst_element_query_position_0_10_T;
  gst_is_initialized : gst_is_initialized_t;


  procedure libGST_dynamic_dll_init;
  procedure libGST_dynamic_dll_done;
implementation
uses
  Classes, SysUtils, dynlibs;

const
{$IFDEF UNIX}
  libgst_name_1_0  = 'libgstbase-0.10.so.0';
  libgst_name_0_10 = 'libgstbase-1.0.so.0';
{$ENDIF}
{$IFDEF MSWINDOWS}
  libgst_name_1_0  = 'libgstreamer-1.0-0.dll';
  libgst_name_0_10 = 'libgstreamer-0.10-0.dll';
{$ENDIF}



var
libgst_handle: THandle;

function libGST_dll_get_proc_addr(var addr: Pointer; const name: PAnsiChar): Boolean;
begin
  addr := GetProcedureAddress(libGST_handle, name);
  Result := (addr <> NIL);
  if not Result then
  begin
    libGST_dynamic_dll_error := 'Procedure "' + name + '" not found!';
  end;
end;

procedure libGST_dynamic_dll_init();
var
  currLib: string;
begin

  if (libGST_handle <> dynlibs.NilHandle) then exit;
  currLib:=libgst_name_0_10;
  libGST_handle := DynLibs.LoadLibrary(currLib);
  GST_Lib_1_0 := true;
  // try to load previous release of GStreamer
  if (libGST_handle = dynlibs.NilHandle) and (libgst_name_1_0 <> EmptyStr) then
    begin
      currLib:=libgst_name_1_0;
      libGST_handle := DynLibs.LoadLibrary(currLib);
      GST_Lib_1_0 := false;
    end;

  // exit, report error
  if (libGST_handle = dynlibs.NilHandle) then
  begin
    libGST_dynamic_dll_error :=
      'Library not found ' + currLib + ', '+
      'GetLastError() = ' + IntToStr(GetLastOSError);
    exit;
  end;

  if not libGST_dll_get_proc_addr(pointer(gst_element_factory_make),  'gst_element_factory_make') then   exit;
  if not libGST_dll_get_proc_addr(pointer(gst_init_check),  'gst_init') then   exit;
  if not libGST_dll_get_proc_addr(pointer(gst_bin_new),  'gst_bin_new') then   exit;
  if not libGST_dll_get_proc_addr(pointer(gst_bin_add),  'gst_bin_add') then   exit;
  if not libGST_dll_get_proc_addr(pointer(gst_bin_add_many),  'gst_bin_add_many') then   exit;
  if not libGST_dll_get_proc_addr(pointer(gst_element_link_many),  'gst_element_link_many') then   exit;
  if not libGST_dll_get_proc_addr(pointer(gst_element_set_state),  'gst_element_set_state') then   exit;
  if not libGST_dll_get_proc_addr(pointer(gst_element_get_state),  'gst_element_get_state') then   exit;
  if not libGST_dll_get_proc_addr(pointer(gst_pipeline_get_bus),  'gst_pipeline_get_bus') then   exit;
  if not libGST_dll_get_proc_addr(pointer(gst_bus_add_watch),  'gst_bus_add_watch') then   exit;
  if not libGST_dll_get_proc_addr(pointer(gst_object_unref),  'gst_object_unref') then   exit;
  if GST_Lib_1_0 then
     begin
     if not libGST_dll_get_proc_addr(pointer(gst_element_query_position),  'gst_element_query_position') then   exit
     end
  else
    if not libGST_dll_get_proc_addr(pointer(gst_element_query_position_OLD),  'gst_element_query_position') then   exit;

  if not libGST_dll_get_proc_addr(pointer(gst_element_seek),  'gst_element_seek') then   exit;
  if not libGST_dll_get_proc_addr(pointer(gst_element_seek_simple),  'gst_element_seek_simple') then   exit;
  if not libGST_dll_get_proc_addr(pointer(gst_is_initialized),  'gst_is_initialized') then   exit;

end;


procedure libgst_dynamic_dll_done();
begin
  if (libgst_handle <> dynlibs.NilHandle) then FreeLibrary(libgst_handle);
  libgst_handle := dynlibs.NilHandle;
end;

initialization

  libgst_handle := dynlibs.NilHandle;

finalization

end.



