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

  This is only a very small extract of FFMPEG API.
}
{$I codegen.inc}
unit ffmpeg;

interface

uses
  ctypes;

const
  AV_NUM_DATA_POINTERS = 4;
const
  AVSEEK_FLAG_BACKWARD = 1; ///< seek backward
  AVSEEK_FLAG_BYTE     = 2; ///< seeking based on position in bytes
  AVSEEK_FLAG_ANY      = 4; ///< seek to any frame, even non-keyframes
  AVSEEK_FLAG_FRAME    = 8;

type
  TAVMediaType = (
    AVMEDIA_TYPE_UNKNOWN = -1,  ///< Usually treated as AVMEDIA_TYPE_DATA
    AVMEDIA_TYPE_VIDEO,
    AVMEDIA_TYPE_AUDIO,
    AVMEDIA_TYPE_DATA,          ///< Opaque data information usually continuous
    AVMEDIA_TYPE_SUBTITLE,
    AVMEDIA_TYPE_ATTACHMENT,    ///< Opaque data information usually sparse
    AVMEDIA_TYPE_NB
  );

  TAVCodecID = (
      AV_CODEC_ID_NONE);

  PAVCodecContext = ^TAVCodecContext;
  PPAVFormatContext = ^PAVFormatContext;
  PAVFormatContext = ^TAVFormatContext;

  PPAVStream = ^PAVStream;
  PAVStream = ^TAVStream;
  TAVStream = record
    index: cint;
    id: cint;
    codec: PAVCodecContext;
  end;



  PPAVCodec = ^PAVCodec;
  PAVCodec = ^TAVCodec;
  TAVCodec = record
  end;


  TAVCodecContext = record
    av_class: Pointer;
    log_level_offset: cint;

    codec_type: TAVMediaType;
    codec:      PAVCodec;
    codec_name: array [0..31] of AnsiChar;
    codec_id:   TAVCodecID;
  end;

  TAVFormatContext = record
       av_class: Pointer;
       iformat: Pointer;
       oformat: Pointer;
       priv_data: pointer;
       pb: Pointer;
       nb_streams: cuint;
       streams: PPAVStream;
    end;


  PPAVFrame = ^PAVFrame;
  PAVFrame = ^TAVFrame;
  TAVFrame = record
     data: array [0..AV_NUM_DATA_POINTERS - 1] of pbyte;
     linesize: array [0..AV_NUM_DATA_POINTERS - 1] of cint;
     base: array [0..AV_NUM_DATA_POINTERS - 1] of pbyte;
  end;

  PAVPacket = ^TAVPacket;
  TAVPacket = record
    pts:          cint64;
    dts:          cint64;
    data:         Pointer;
    size:         cint;
    stream_index: cint;
    flags:        cint;
    side_data: Pointer;
    side_data_elems: cint;
    duration:     cint;
    destruct:     procedure (para1: PAVPacket); cdecl;
    priv:         pointer;
    pos:          cint64;       // byte position in stream, -1 if unknown
    convergence_duration: cint64;
  end; {TAVPacket}


  PAVClass = ^TAVClass;
  TAVClass = record
    (**
     * The name of the class; usually it is the same name as the
     * context structure type to which the AVClass is associated.
     *)
    class_name: PAnsiChar;

    (**
     * A pointer to a function which returns the name of a context
     * instance ctx associated with the class.
     *)
    item_name: function(ctx: pointer): PAnsiChar; cdecl;

    (**
     * a pointer to the first option specified in the class if any or NULL
     *
     * @see av_set_default_options()
     *)
    option: Pointer;

    (**
     * LIBAVUTIL_VERSION with which this structure was created.
     * This is used to allow fields to be added without requiring major
     * version bumps everywhere.
     *)
    version: cint;
  end;



  avcodec_register_all_t = procedure (); cdecl;
  av_register_all_t = procedure ();  cdecl;
  avformat_open_input_t = function (ps: PPAVFormatContext; filename: {const} PAnsiChar; fmt: pointer; options: pointer): cint;  cdecl;
  avformat_find_stream_info_t = function (ic: PAVFormatContext; options: pointer): cint;  cdecl;
  avcodec_find_decoder_t = function (id: TAVCodecID): PAVCodec;  cdecl;
  avcodec_open2_t = function (avctx: PAVCodecContext; codec: {const} PAVCodec; options: pointer): cint;  cdecl;
  avcodec_alloc_frame_t = function (): PAVFrame;  cdecl;
  av_read_frame_t = function (s: PAVFormatContext; var pkt: TAVPacket): cint;  cdecl;
  avcodec_decode_audio4_t = function (avctx: PAVCodecContext; frame: PAVFrame; got_frame_ptr: Pcint; avpkt: PAVPacket): cint;  cdecl;
  av_free_packet_t = procedure (pkt: PAVPacket);  cdecl;
  avcodec_free_frame_t = procedure (frame: PPAVFrame);  cdecl;
  avcodec_close_t = function (avctx: PAVCodecContext): cint;  cdecl;
  avformat_close_input_t = procedure (s: PPAVFormatContext);  cdecl;
  av_init_packet_t = procedure (var pkt: TAVPacket);  cdecl;

  av_find_best_stream_t = function (ic: PAVFormatContext;
                          type_: TAVMediaType;
                          wanted_stream_nb: cint;
                          related_stream: cint;
                          decoder_ret: PPAVCodec;
                          flags: cint): cint;
                           cdecl;

  av_seek_frame_t = function (s: PAVFormatContext; stream_index: cint; timestamp: cint64; flags: cint): cint;  cdecl;

  avcodec_alloc_context3_t = function (codec: PAVCodec): PAVCodecContext;    cdecl;

  av_opt_get_int_t = function  (obj: pointer; name: {const} PAnsiChar; search_flags: cint;     out_val: Pcint64):     cint; cdecl;

//  av_frame_get_channels_t = function (frame: {const} PAVFrame): cint;    cdecl;
//  av_frame_get_sample_rate_t = function   (frame: {const} PAVFrame): cint; cdecl;


var
  avcodec_register_all      :avcodec_register_all_t;
  av_register_all           :av_register_all_t;
  avformat_open_input       :avformat_open_input_t;
  avformat_find_stream_info :avformat_find_stream_info_t;
  avcodec_find_decoder      :avcodec_find_decoder_t;
  avcodec_open2             :avcodec_open2_t;
  avcodec_alloc_frame       :avcodec_alloc_frame_t;
  av_read_frame             :av_read_frame_t;
  avcodec_decode_audio4     :avcodec_decode_audio4_t;
  av_free_packet            :av_free_packet_t;
  avcodec_free_frame        :avcodec_free_frame_t;
  avcodec_close             :avcodec_close_t;
  avformat_close_input      :avformat_close_input_t;
  av_init_packet            :av_init_packet_t;
  av_find_best_stream       :av_find_best_stream_t;
  avcodec_alloc_context3    :avcodec_alloc_context3_t;
  av_opt_get_int            :av_opt_get_int_t;
  av_seek_frame             :av_seek_frame_t;
//  av_frame_get_channels     :av_frame_get_channels_t;
//  av_frame_get_sample_rate  :av_frame_get_sample_rate_t;

procedure libFFMPEG_dynamic_dll_init;
procedure libFFMPEG_dynamic_dll_done;


var
  libFFMPEG_dynamic_dll_error : string;

implementation
uses
  Classes, SysUtils, dynlibs;

const
  av_codec = 'libavcodec.so.53';
  av_format = 'libavformat.so.53';
  av_util = 'libavutil.so.51';

var
  av_codec_handle : THandle;
  av_format_handle: THandle;
  av_util_handle: THandle;

function libFFMPEG_dll_get_proc_addr(Handle: THandle; var addr: Pointer; const name: PAnsiChar): Boolean;
  begin
    addr := GetProcedureAddress(Handle, name);
    Result := (addr <> NIL);
    if not Result then
    begin
      libFFMPEG_dynamic_dll_error := 'Procedure "' + name + '" not found!';
    end;
  end;


procedure libFFMPEG_dynamic_dll_init;
begin

  if (av_format_handle <> 0) then exit;

  av_format_handle := LoadLibrary(PAnsiChar(av_format));
  // exit, report error
  if (av_format_handle = 0) then
  begin
    libFFMPEG_dynamic_dll_error :=
      'Library not found ' + av_format + ', '+
      'GetLastError() = ' + IntToStr(GetLastOSError);
    exit;
  end;

  av_codec_handle := LoadLibrary(PAnsiChar(av_codec));
  if (av_codec_handle = 0) then
  begin
    libFFMPEG_dynamic_dll_error :=
      'Library not found ' + av_codec + ', '+
      'GetLastError() = ' + IntToStr(GetLastOSError);
    exit;
  end;

  av_util_handle := LoadLibrary(PAnsiChar(av_util));
  if (av_util_handle = 0) then
  begin
    libFFMPEG_dynamic_dll_error :=
      'Library not found ' + av_util + ', '+
      'GetLastError() = ' + IntToStr(GetLastOSError);
    exit;
  end;

  if not libFFMPEG_dll_get_proc_addr(av_codec_handle, pointer(avcodec_register_all),  'avcodec_register_all') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_codec_handle, pointer(avcodec_find_decoder),  'avcodec_find_decoder') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_codec_handle, pointer(avcodec_open2),  'avcodec_open2') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_codec_handle, pointer(avcodec_alloc_frame),  'avcodec_alloc_frame') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_codec_handle, pointer(avcodec_decode_audio4),  'avcodec_decode_audio4') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_codec_handle, pointer(av_free_packet),  'av_free_packet') then   exit;
//  if not libFFMPEG_dll_get_proc_addr(av_codec_handle, pointer(avcodec_free_frame),  'avcodec_free_frame') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_codec_handle, pointer(avcodec_close),  'avcodec_close') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_codec_handle, pointer(av_init_packet),  'av_init_packet') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_codec_handle, pointer(avcodec_alloc_context3),  'avcodec_alloc_context3') then   exit;
//  if not libFFMPEG_dll_get_proc_addr(av_codec_handle, pointer(av_frame_get_channels),  'av_frame_get_channels') then   exit;
//  if not libFFMPEG_dll_get_proc_addr(av_codec_handle, pointer(av_frame_get_sample_rate),  'av_frame_get_sample_rate') then   exit;


  if not libFFMPEG_dll_get_proc_addr(av_format_handle, pointer(av_register_all),  'av_register_all') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_format_handle, pointer(avformat_open_input),  'avformat_open_input') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_format_handle, pointer(avformat_find_stream_info),  'avformat_find_stream_info') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_format_handle, pointer(av_read_frame),  'av_read_frame') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_format_handle, pointer(avformat_close_input),  'avformat_close_input') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_format_handle, pointer(av_find_best_stream),  'av_find_best_stream') then   exit;
  if not libFFMPEG_dll_get_proc_addr(av_format_handle, pointer(av_seek_frame),  'av_seek_frame') then   exit;

  if not libFFMPEG_dll_get_proc_addr(av_util_handle, pointer(av_opt_get_int),  'av_opt_get_int') then   exit;

end;

procedure libFFMPEG_dynamic_dll_done;
begin
  if (av_codec_handle <> 0) then FreeLibrary(av_codec_handle);
  av_codec_handle := 0;
  if (av_format_handle <> 0) then FreeLibrary(av_format_handle);
  av_format_handle := 0;


end;

initialization

  av_codec_handle := 0;
  av_format_handle := 0;

end.

