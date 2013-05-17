(*
 * copyright (c) 2001 Fabrice Bellard
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

   ===

   revision 15291, 10-sep-2008

   delphi conversion of libavcodec/avformat.h

   slightly.fat.hamster@gmail.com, originally by Victor Zinetz

   24-sept-2008

   ===

 *)

unit avformat;

interface

uses
  windows,
  avcodec,
  rational,
  avutil,
  avio, log;

(* packet functions *)

(**
 * Returns the LIBAVFORMAT_VERSION_INT constant.
 *)
function avformat_version (): cardinal;
  cdecl; external dll_name;

const
  LIBAVFORMAT_VERSION_MAJOR = 52;
  LIBAVFORMAT_VERSION_MINOR = 22;
  LIBAVFORMAT_VERSION_MICRO = 1;

  LIBAVFORMAT_VERSION_INT = LIBAVFORMAT_VERSION_MAJOR shl 16 +
                            LIBAVFORMAT_VERSION_MINOR shl 8 +
                            LIBAVFORMAT_VERSION_MICRO;

  dll_name = 'avformat-52.dll';

  PKT_FLAG_KEY   = $0001;

  AVPROBE_SCORE_MAX = 100;               ///< max score, half of that is used for file extension based detection
  AVPROBE_PADDING_SIZE = 32;             ///< extra allocated bytes at the end of the probe buffer
//! demuxer will use url_fopen, no opened file should be provided by the caller
  AVFMT_NOFILE        = $0001;
  AVFMT_NEEDNUMBER    = $0002; (**< needs '%d' in filename *)
  AVFMT_SHOW_IDS      = $0008; (**< show format stream IDs numbers *)
  AVFMT_RAWPICTURE    = $0020; (**< format wants AVPicture structure for
                                      raw picture data *)
  AVFMT_GLOBALHEADER  = $0040; (**< format wants global header *)
  AVFMT_NOTIMESTAMPS  = $0080; (**< format does not need / have any timestamps *)
  AVFMT_GENERIC_INDEX = $0100; (**< use generic index building code *)
  AVFMT_TS_DISCONT    = $0200; (**< format allows timestamo discontinuities *)

  AVINDEX_KEYFRAME = $0001;

  MAX_REORDER_DELAY = 16;

  AV_PROGRAM_RUNNING = 1;

  AVFMTCTX_NOHEADER      = $0001; (**< signal that no header is present
                                         (streams are added dynamically) *)

  MAX_STREAMS = 20;

  AVFMT_NOOUTPUTLOOP = -1;
  AVFMT_INFINITEOUTPUTLOOP = 0;
  AVFMT_FLAG_GENPTS       = $0001; ///< generate pts if missing even if it requires parsing future frames
  AVFMT_FLAG_IGNIDX       = $0002; ///< ignore index
  AVFMT_FLAG_NONBLOCK     = $0004; ///< do not block when reading packets from input

  AVSEEK_FLAG_BACKWARD = 1; ///< seek backward
  AVSEEK_FLAG_BYTE     = 2; ///< seeking based on position in bytes
  AVSEEK_FLAG_ANY      = 4; ///< seek to any frame, even non keyframes

  FFM_PACKET_SIZE = 4096;

  AV_DISPOSITION_DEFAULT   = $0001;
  AV_DISPOSITION_DUB       = $0002;
  AV_DISPOSITION_ORIGINAL  = $0004;
  AV_DISPOSITION_COMMENT   = $0008;
  AV_DISPOSITION_LYRICS    = $0010;
  AV_DISPOSITION_KARAOKE   = $0020;

  FF_FDEBUG_TS        = $0001;
type
  int = integer;

  PAVPacket = ^TAVPacket;
  TAVPacket = record
    (**
     * Presentation time stamp in time_base units.
     * This is the time at which the decompressed packet will be presented
     * to the user.
     * Can be AV_NOPTS_VALUE if it is not stored in the file.
     * pts MUST be larger or equal to dts as presentation can not happen before
     * decompression, unless one wants to view hex dumps. Some formats misuse
     * the terms dts and pts/cts to mean something different, these timestamps
     * must be converted to true pts/dts before they are stored in AVPacket.
     *)
    pts: int64;
    (**
     * Decompression time stamp in time_base units.
     * This is the time at which the packet is decompressed.
     * Can be AV_NOPTS_VALUE if it is not stored in the file.
     *)
    dts: int64;
    data: PByte;
    size,
    stream_index,
    flags,
    (**
     * Duration of this packet in time_base units, 0 if unknown.
     * Equals next_pts - this_pts in presentation order.
     *)
    duration: int;
    destruct: procedure (pkt: PAVPacket); //void  (*destruct)(struct AVPacket *);
    priv: pointer;
    pos: int64;                            ///< byte position in stream, -1 if unknown
    (**
     * This is the time difference in stream timebase units from the pts of this
     * packet to the point at which the output from the decoder has converged
     * independent from the availability
     * of previous frames (that is the frames are virtually identical no matter
     * if decoding started from the very first frame or from this keyframe).
     * is AV_NOPTS_VALUE if unknown.
     * This field is not the display duration of the current packet.
     *
     * The purpose of this field is to allow seeking in streams that have no
     * keyframes in the conventional sense. It corresponds to the
     * recovery point SEI in H.264 and match_time_delta in nut. It also is
     * essential for some types of subtitle streams to ensure that all
     * subtitles are correctly displayed after seeking.
     *)
    convergence_duration: int64;
  end;

procedure av_destruct_packet_nofree (pkt: PAVPacket);
  cdecl; external dll_name;

(**
 * Default packet destructor.
 *)
procedure av_destruct_packet (pkt: PAVPacket);
  cdecl; external dll_name;
(**
 * Initialize optional fields of a packet to default values.
 *
 * @param pkt packet
 *)
procedure av_init_packet (pkt: PAVPacket);
  cdecl; external dll_name;

(**
 * Allocate the payload of a packet and initialize its fields to default values.
 *
 * @param pkt packet
 * @param size wanted payload size
 * @return 0 if OK. AVERROR_xxx otherwise.
 *)
function av_new_packet (pkt: PAVPacket; size: int): int;
  cdecl; external dll_name;
(**
 * Allocate and read the payload of a packet and initialize its fields to default values.
 *
 * @param pkt packet
 * @param size wanted payload size
 * @return >0 (read size) if OK. AVERROR_xxx otherwise.
 *)
function av_get_packet (s: PByteIOContext; pkt: PAVPacket; size: int): int;
  cdecl; external dll_name;

(**
 * @warning This is a hack - the packet memory allocation stuff is broken. The
 * packet is allocated if it was not really allocated
 *)
function av_dup_packet (pkt: PAVPacket): int;
  cdecl; external dll_name;

(**
 * Free a packet
 *
 * @param pkt packet to free
 *)
procedure av_free_packet (pkt: PAVPacket);

type
(*************************************************)
(* fractional numbers for exact pts handling *)

(**
 * the exact value of the fractional number is: 'val + num / den'.
 * num is assumed to be such as 0 <= num < den
 * @deprecated Use AVRational instead
*)
  TAVFrac = record
    val, num, den: int64;
  end; //deprecated

(*************************************************)
(* input/output formats *)

  PAVCodecTag = pointer;
  PAVFormatContext = ^TAVFormatContext;

(** this structure contains the data a format has to probe a file *)
  PAVProbeData = ^TAVProbeData;
  TAVProbeData = record
    filename: pchar;
    buf: pchar;
    buf_size: int;
  end;

  PAVFormatParameters = ^TAVFormatParameters;
  TAVFormatParameters = record
    time_base: TAVRational;
    sample_rate,
    channels,
    width,
    height: int;
    pix_fmt: TPixelFormat;
    channel: int; (**< used to select dv channel *)
    standard: pchar; (**< tv standard, NTSC, PAL, SECAM *)
//    unsigned int mpeg2ts_raw:1;  /**< force raw MPEG2 transport stream output, if possible */
//    unsigned int mpeg2ts_compute_pcr:1; /**< compute exact PCR for each transport
//                                            stream packet (only meaningful if
//                                            mpeg2ts_raw is TRUE) */
//    unsigned int initial_pause:1;       /**< do not begin to play the stream
//                                            immediately (RTSP only) */
//    unsigned int prealloced_context:1;
    union: byte;
{$if LIBAVFORMAT_VERSION_INT < (53 shl 16)}
    video_codec_id: TCodecID;
    audio_codec_id: TCodecID;
{$ifend}
  end;

  PAVOutputFormat = ^TAVOutputFormat;
  TAVOutputFormat = record
    name,
     (**
     * Descriptive name for the format, meant to be more human-readable
     * than \p name. You \e should use the NULL_IF_CONFIG_SMALL() macro
     * to define it.
     *)
    long_name,
    mime_type,
    extensions: pchar; (**< comma separated filename extensions *)
    (** size of private data so that it can be allocated in the wrapper *)
    priv_data_size: int;
    (* output support *)
    audio_codec: TCodecID; (**< default audio codec *)
    video_codec: TCodecID; (**< default video codec *)
    write_header: pointer; //int (*write_header)(struct AVFormatContext *);
    write_packet: pointer; //int (*write_packet)(struct AVFormatContext *, AVPacket *pkt);
    write_trailer: pointer; //int (*write_trailer)(struct AVFormatContext *);
    (** can use flags: AVFMT_NOFILE, AVFMT_NEEDNUMBER, AVFMT_GLOBALHEADER *)
    flags: int;
    (** currently only used to set pixel format if not YUV420P *)
    set_parameters: pointer; //int (*set_parameters)(struct AVFormatContext *, AVFormatParameters *);
    interleave_packet: pointer; //int (*interleave_packet)(struct AVFormatContext *, AVPacket *out, AVPacket *in, int flush);

    (**
     * list of supported codec_id-codec_tag pairs, ordered by "better choice first"
     * the arrays are all CODEC_ID_NONE terminated
     *)
    codec_tag: PAVCodecTag;

    subtitle_codec: TCodecID; (**< default subtitle codec *)

    (* private fields *)
    next: PAVOutputFormat;
  end;

  PAVInputFormat = ^TAVInputFormat;
  TAVInputFormat = record
    name,
    (**
     * Descriptive name for the format, meant to be more human-readable
     * than \p name. You \e should use the NULL_IF_CONFIG_SMALL() macro
     * to define it.
     *)
    long_name: pchar;
    (** size of private data so that it can be allocated in the wrapper *)
    priv_data_size: int;
    (**
     * Tell if a given file has a chance of being parsed by this format.
     * The buffer provided is guaranteed to be AVPROBE_PADDING_SIZE bytes
     * big so you do not have to check for that unless you need more.
     *)
    read_probe: pointer; //int (*read_probe)(AVProbeData *);
    (** read the format header and initialize the AVFormatContext
       structure. Return 0 if OK. 'ap' if non NULL contains
       additional paramters. Only used in raw format right
       now. 'av_new_stream' should be called to create new streams.  *)
    read_header: pointer; //int (*read_header)(struct AVFormatContext *,
                       //AVFormatParameters *ap);
    (** read one packet and put it in 'pkt'. pts and flags are also
       set. 'av_new_stream' can be called only if the flag
       AVFMTCTX_NOHEADER is used. *)
    read_packet: pointer; //int (*read_packet)(struct AVFormatContext *, AVPacket *pkt);
    (** close the stream. The AVFormatContext and AVStreams are not
       freed by this function *)
    read_close: pointer; //int (*read_close)(struct AVFormatContext *);
    (**
     * seek to a given timestamp relative to the frames in
     * stream component stream_index
     * @param stream_index must not be -1
     * @param flags selects which direction should be preferred if no exact
     *              match is available
     * @return >= 0 on success (but not necessarily the new offset)
     *)
    read_seek: pointer; //int (*read_seek)(struct AVFormatContext *,
                     //int stream_index, int64_t timestamp, int flags);
    (**
     * gets the next timestamp in stream[stream_index].time_base units.
     * @return the timestamp or AV_NOPTS_VALUE if an error occured
     *)
    read_timestamp: pointer; //int64_t (*read_timestamp)(struct AVFormatContext *s, int stream_index,
                              //int64_t *pos, int64_t pos_limit);
    (** can use flags: AVFMT_NOFILE, AVFMT_NEEDNUMBER *)
    flags: int;
    (** if extensions are   d, then no probe is done. You should
       usually not use extension format guessing because it is not
       reliable enough *)
    extensions: pchar;
    (** general purpose read only value that the format can use *)
    value: int;
    (** start/resume playing - only meaningful if using a network based format
       (RTSP) *)
    read_play: pointer; //int (*read_play)(struct AVFormatContext *);
    (** pause playing - only meaningful if using a network based format
       (RTSP) *)
    read_pause: pointer; //int (*read_pause)(struct AVFormatContext *);
    codec_tag: PAVCodecTag;

    (* private fields *)
    next: PAVInputFormat;
  end;

  TAVStreamParseType = (
    AVSTREAM_PARSE_NONE,
    AVSTREAM_PARSE_FULL,       (**< full parsing and repack *)
    AVSTREAM_PARSE_HEADERS,    (**< only parse headers, don't repack *)
    AVSTREAM_PARSE_TIMESTAMPS (**< full parsing and interpolation of timestamps for frames not starting on packet boundary *)
  );

  PAVIndexEntry = ^TAVIndexEntry;
  TAVIndexEntry = record
    pos,
    timestamp: int64;

    //int flags:2;
    //int size:30; //Yeah, trying to keep the size of this small to reduce memory requirements (it is 24 vs 32 byte due to possible 8byte align).
    union: cardinal;
    min_distance: int;         (**< min distance between this and the previous keyframe, used to avoid unneeded searching *)
  end;

(**
 * Stream structure.
 * New fields can be added to the end with minor version bumps.
 * Removal, reordering and changes to existing fields require a major
 * version bump.
 * sizeof(AVStream) must not be used outside libav*.
 *)
  PAVStream = ^TAVStream;
  TAVStream = record
    index,    (**< stream index in AVFormatContext *)
    id: int;       (**< format specific stream id *)
    codec: PAVCodecContext; (**< codec context *)
    (**
     * Real base frame rate of the stream.
     * This is the lowest frame rate with which all timestamps can be
     * represented accurately (it is the least common multiple of all
     * frame rates in the stream), Note, this value is just a guess!
     * For example if the timebase is 1/90000 and all frames have either
     * approximately 3600 or 1800 timer ticks then r_frame_rate will be 50/1.
     *)
    r_frame_rate: TAVRational;
    priv_data: pointer;
    (* internal data used in av_find_stream_info() *)
    first_dts: int64;
    (** encoding: PTS generation when outputing stream *)
    pts: TAVFrac;
    (**
     * This is the fundamental unit of time (in seconds) in terms
     * of which frame timestamps are represented. For fixed-fps content,
     * timebase should be 1/frame rate and timestamp increments should be
     * identically 1.
     *)
    time_base: TAVRational;
    pts_wrap_bits: int; (**< number of bits in pts (used for wrapping control) *)
    (* ffmpeg.c private use *)
    stream_copy: int; (**< if set, just copy stream *)
    discard: TAVDiscard; ///< selects which packets can be discarded at will and do not need to be demuxed
    //FIXME move stuff to a flags field?
    (** quality, as it has been removed from AVCodecContext and put in AVVideoFrame
     * MN: dunno if that is the right place for it *)
    quality: single;
    (**
     * Decoding: pts of the first frame of the stream, in stream time base.
     * Only set this if you are absolutely 100% sure that the value you set
     * it to really is the pts of the first frame.
     * This may be un  d (AV_NOPTS_VALUE).
     * @note The ASF header does NOT contain a correct start_time the ASF
     * demuxer must NOT set this.
     *)
    start_time,
    (**
     * Decoding: duration of the stream, in stream time base.
     * If a source file does not specify a duration, but does specify
     * a bitrate, this value will be estimates from bit rate and file size.
     *)
    duration: int64;

    language: array [0..3] of char; (** ISO 639 3-letter language code (empty string if un  d) *)

    (* av_read_frame() support *)
    need_parsing: TAVStreamParseType;
    parser: PAVCodecParserContext;

    cur_dts: int64;
    last_IP_duration: int;
    last_IP_pts: int64;
    (* av_seek_frame() support *)
    index_entries: PAVIndexEntry; (**< only used if the format does not
                                    support seeking natively *)
    nb_index_entries: int;
    index_entries_allocated_size: cardinal;
    nb_frames: int64;                 ///< number of frames in this stream if known or 0
    {$if LIBAVFORMAT_VERSION_INT < (53 shl 16)}
    unused: array [0..4] of int64;
    {$ifend}


    filename: pchar; (**< source filename of the stream *)

    disposition: integer; //**< AV_DISPOSITION_* bitfield */
    probe_data: TAVProbeData;
    pts_buffer: array [0..MAX_REORDER_DELAY] of int64;
    (**
     * sample aspect ratio (0 if unknown)
     * - encoding: Set by user.
     * - decoding: Set by libavformat.
     *)
    sample_aspect_ratio: TAVRational;
  end;

(**
 * New fields can be added to the end with minor version bumps.
 * Removal, reordering and changes to existing fields require a major
 * version bump.
 * sizeof(AVProgram) must not be used outside libav*.
 *)
  PAVProgram = ^TAVProgram;
  TAVProgram = record
    id: int;
    provider_name: pchar; ///< Network name for DVB streams
    name: pchar;          ///< Service name for DVB streams
    flags: int;
    discard: TAVDiscard;        ///< selects which program to discard and which to feed to the caller
    stream_index: PCardinal;
    nb_stream_indexes: cardinal;
  end;

  PAVChapter = ^TAVChapter;
  TAVChapter = record
    id: int;                 ///< Unique id to identify the chapter
    time_base: TAVRational;   ///< Timebase in which the start/end timestamps are specified

    start,
    _end: int64; ///< chapter start/end time in time_base units
    title: pchar; ///< chapter title
  end;

(**
 * format I/O context.
 * New fields can be added to the end with minor version bumps.
 * Removal, reordering and changes to existing fields require a major
 * version bump.
 * sizeof(AVFormatContext) must not be used outside libav*.
 *)
  PAVPacketList = ^TAVPacketList;
  TAVFormatContext = record
    av_class: PAVClass; (**< set by av_alloc_format_context *)
    (* can only be iformat or oformat, not both at the same time *)
    iformat: PAVInputFormat;
    oformat: PAVOutputFormat;
    priv_data: pointer;
    pb: PByteIOContext;
    nb_streams: cardinal;
    streams: array [0..MAX_STREAMS-1] of PAVStream;
    filename: array [0..1023] of char; (**< input or output filename *)
    (* stream info *)
    timestamp: int64;
    title: array [0..511] of char;
    author: array [0..511] of char;
    copyright: array [0..511] of char;
    comment: array [0..511] of char;
    album: array [0..511] of char;
    year: int;  (**< ID3 year, 0 if none *)
    track: int; (**< track number, 0 if none *)
    genre: array [0..31] of char; (**< ID3 genre *)

    ctx_flags: int; (**< format specific flags, see AVFMTCTX_xx *)
    (* private data for pts handling (do not modify directly) *)
    (** This buffer is only needed when packets were already buffered but
       not decoded, for example to get the codec parameters in mpeg
       streams *)
    packet_buffer: PAVPacketList;

    (** decoding: position of the first frame of the component, in
       AV_TIME_BASE fractional seconds. NEVER set this value directly:
       it is deduced from the AVStream values.  *)
    start_time: int64;
    (** decoding: duration of the stream, in AV_TIME_BASE fractional
       seconds. NEVER set this value directly: it is deduced from the
       AVStream values.  *)
    duration: int64;
    (** decoding: total file size. 0 if unknown *)
    file_size: int64;
    (** decoding: total stream bitrate in bit/s, 0 if not
       available. Never set it directly if the file_size and the
       duration are known as ffmpeg can compute it automatically. *)
    bit_rate: int;

    (* av_read_frame() support *)
    cur_st: PAVStream;
    cur_ptr: PByte;
    cur_len: int;
    cur_pkt: TAVPacket;

    (* av_seek_frame() support *)
    data_offset: int64; (** offset of the first packet *)
    index_built: int;

    mux_rate,
    packet_size,
    preload,
    max_delay: int;

    (** number of times to loop output in formats that support it *)
    loop_output: int;

    flags: int;

    loop_input: int;
    (** decoding: size of data to probe; encoding unused *)
    probesize: cardinal;

    (**
     * maximum duration in AV_TIME_BASE units over which the input should be analyzed in av_find_stream_info()
     *)
    max_analyze_duration: int;

    key: PByte;
    keylen: int;

    nb_programs: cardinal;
    programs: PAVProgram;

    (**
     * Forced video codec_id.
     * demuxing: set by user
     *)
    video_codec_id: TCodecID;
    (**
     * Forced audio codec_id.
     * demuxing: set by user
     *)
    audio_codec_id: TCodecID;
    (**
     * Forced subtitle codec_id.
     * demuxing: set by user
     *)
    subtitle_codec_id: TCodecID;

    (**
     * Maximum amount of memory in bytes to use per stream for the index.
     * If the needed index exceeds this size entries will be discarded as
     * needed to maintain a smaller size. This can lead to slower or less
     * accurate seeking (depends on demuxer).
     * Demuxers for which a full in memory index is mandatory will ignore
     * this.
     * muxing  : unused
     * demuxing: set by user
     *)
    max_index_size: cardinal;
    (**
     * Maximum ammount of memory in bytes to use for buffering frames that are
     * obtained from real-time capture devices.
     *)
    max_picture_buffer: cardinal;

    nb_chapters: cardinal;
    chapters: ^PAVChapter;
    (**
     * Flags to enable debuging.
     *)
    debug: int;
    (**
     * raw packets from the demuxer, prior to parsing and decoding.
     * This buffer is used for buffering packets until the codec can
     * be identified, as parsing cannot be done without knowing the
     * codec.
     *)
    raw_packet_buffer: PAVPacketList;
    raw_packet_buffer_end: PAVPacketList;
    packet_buffer_end: PAVPacketList;
  end;

  TAVPacketList = record
    pkt: TAVPacket;
    next: PAVPacketList;
  end;

function av_iformat_next (f: PAVInputFormat): PAVInputFormat;
  cdecl; external dll_name;
function av_oformat_next (f: PAVOutputFormat): PAVOutputFormat;
  cdecl; external dll_name;

function av_guess_image2_codec (const filename: pchar): TCodecID;
  cdecl; external dll_name;

(* XXX: use automatic init with either ELF sections or C file parser *)
(* modules *)

(* utils.c *)
procedure av_register_input_format (format: PAVInputFormat);
  cdecl; external dll_name;
procedure av_register_output_format (format: PAVOutputFormat);
  cdecl; external dll_name;
function guess_stream_format (const short_name: pchar; const filename: pchar;
                              const mime_type: pchar): PAVOutputFormat;
  cdecl; external dll_name;
function guess_format (const short_name: pchar;
                       const filename: pchar; const mime_type: pchar): PAVOutputFormat;
  cdecl; external dll_name;

(**
 * Guesses the codec id based upon muxer and filename.
 *)
function av_guess_codec (fmt: PAVOutputFormat; const short_name: pchar;
                         const filename: pchar; const mime_type: pchar; _type: TCodecType): TCodecID;
  cdecl; external dll_name;

(**
 * Send a nice hexadecimal dump of a buffer to the specified file stream.
 *
 * @param f The file stream pointer where the dump should be sent to.
 * @param buf buffer
 * @param size buffer size
 *
 * @see av_hex_dump_log, av_pkt_dump, av_pkt_dump_log
 *)
procedure av_hex_dump (f: THandle; buf: PByte; size: int);
  cdecl; external dll_name;

(**
 * Send a nice hexadecimal dump of a buffer to the log.
 *
 * @param avcl A pointer to an arbitrary struct of which the first field is a
 * pointer to an AVClass struct.
 * @param level The importance level of the message, lower values signifying
 * higher importance.
 * @param buf buffer
 * @param size buffer size
 *
 * @see av_hex_dump, av_pkt_dump, av_pkt_dump_log
 *)
procedure av_hex_dump_log (avcl: pointer; level: int; buf: PByte; size: int);
  cdecl; external dll_name;

(**
 * Send a nice dump of a packet to the specified file stream.
 *
 * @param f The file stream pointer where the dump should be sent to.
 * @param pkt packet to dump
 * @param dump_payload true if the payload must be displayed too
 *)
procedure av_pkt_dump (f: THandle; pkt: PAVPacket; dump_payload: int);
  cdecl; external dll_name;

(**
 * Send a nice dump of a packet to the log.
 *
 * @param avcl A pointer to an arbitrary struct of which the first field is a
 * pointer to an AVClass struct.
 * @param level The importance level of the message, lower values signifying
 * higher importance.
 * @param pkt packet to dump
 * @param dump_payload true if the payload must be displayed too
 *)
procedure av_pkt_dump_log (avcl: pointer; level: int; pkt: PAVPacket; dump_payload: int);
  cdecl; external dll_name;

procedure av_register_all ();
  cdecl; external dll_name;

(** codec tag <-> codec id *)
function av_codec_get_id (const tags: PAVCodecTag; tag: cardinal): TCodecID;
  cdecl; external dll_name;
function av_codec_get_tag (const tags: PAVCodecTag; id: TCodecID): cardinal;
  cdecl; external dll_name;

(* media file input *)

(**
 * finds AVInputFormat based on input format's short name.
 *)
function av_find_input_format (const short_name: pchar): PAVInputFormat;
  cdecl; external dll_name;

(**
 * Guess file format.
 *
 * @param is_opened whether the file is already opened, determines whether
 *                  demuxers with or without AVFMT_NOFILE are probed
 *)
function av_probe_input_format (pd: PAVProbeData; is_opened: int): PAVInputFormat;
  cdecl; external dll_name;

(**
 * Allocates all the structures needed to read an input stream.
 *        This does not open the needed codecs for decoding the stream[s].
 *)
function av_open_input_stream (var ic_ptr: PAVFormatContext;
                         pb: PByteIOContext; const filename: pchar;
                         fmt: PAVInputFormat; ap: PAVFormatParameters): int;
  cdecl; external dll_name;

(**
 * Open a media file as input. The codecs are not opened. Only the file
 * header (if present) is read.
 *
 * @param ic_ptr the opened media file handle is put here
 * @param filename filename to open.
 * @param fmt if non NULL, force the file format to use
 * @param buf_size optional buffer size (zero if default is OK)
 * @param ap additional parameters needed when opening the file (NULL if default)
 * @return 0 if OK. AVERROR_xxx otherwise.
 *)
function av_open_input_file (var ic_ptr: PAVFormatContext; const filename: pchar;
                       fmt: PAVInputFormat;
                       buf_size: int;
                       ap: PAVFormatParameters): int;
  cdecl; external dll_name;
(**
 * Allocate an AVFormatContext.
 * Can be freed with av_free() but do not forget to free everything you
 * explicitly allocated as well!
 *)
function av_alloc_format_context (): PAVFormatContext;
  cdecl; external dll_name;

(**
 * Read packets of a media file to get stream information. This
 * is useful for file formats with no headers such as MPEG. This
 * function also computes the real frame rate in case of mpeg2 repeat
 * frame mode.
 * The logical file position is not changed by this function;
 * examined packets may be buffered for later processing.
 *
 * @param ic media file handle
 * @return >=0 if OK. AVERROR_xxx if error.
 * @todo Let user decide somehow what information is needed so we do not waste time getting stuff the user does not need.
 *)
function av_find_stream_info (ic: PAVFormatContext): int;
  cdecl; external dll_name;

(**
 * Read a transport packet from a media file.
 *
 * This function is obsolete and should never be used.
 * Use av_read_frame() instead.
 *
 * @param s media file handle
 * @param pkt is filled
 * @return 0 if OK. AVERROR_xxx if error.
 *)
function av_read_packet (s: PAVFormatContext; pkt: PAVPacket): int;
  cdecl; external dll_name;

(**
 * Return the next frame of a stream.
 *
 * The returned packet is valid
 * until the next av_read_frame() or until av_close_input_file() and
 * must be freed with av_free_packet. For video, the packet contains
 * exactly one frame. For audio, it contains an integer number of
 * frames if each frame has a known fixed size (e.g. PCM or ADPCM
 * data). If the audio frames have a variable size (e.g. MPEG audio),
 * then it contains one frame.
 *
 * pkt->pts, pkt->dts and pkt->duration are always set to correct
 * values in AVStream.timebase units (and guessed if the format cannot
 * provided them). pkt->pts can be AV_NOPTS_VALUE if the video format
 * has B frames, so it is better to rely on pkt->dts if you do not
 * decompress the payload.
 *
 * @return 0 if OK, < 0 if error or end of file.
 *)
function av_read_frame (s: PAVFormatContext; pkt: PAVPacket): int;
  cdecl; external dll_name;

(**
 * Seek to the key frame at timestamp.
 * 'timestamp' in 'stream_index'.
 * @param stream_index If stream_index is (-1), a default
 * stream is selected, and timestamp is automatically converted
 * from AV_TIME_BASE units to the stream specific time_base.
 * @param timestamp timestamp in AVStream.time_base units
 *        or if there is no stream specified then in AV_TIME_BASE units
 * @param flags flags which select direction and seeking mode
 * @return >= 0 on success
 *)
function av_seek_frame (s: PAVFormatContext; stream_index: int; timestamp: int64; flags: int): int;
  cdecl; external dll_name;

(**
 * start playing a network based stream (e.g. RTSP stream) at the
 * current position
 *)
function av_read_play (s: PAVFormatContext): int;
  cdecl; external dll_name;

(**
 * Pause a network based stream (e.g. RTSP stream).
 *
 * Use av_read_play() to resume it.
 *)
function av_read_pause (s: PAVFormatContext): int;
  cdecl; external dll_name;

(**
 * Free a AVFormatContext allocated by av_open_input_stream.
 * @param s context to free
 *)
procedure av_close_input_stream (s: PAVFormatContext);
  cdecl; external dll_name;

(**
 * Close a media file (but not its codecs).
 *
 * @param s media file handle
 *)
procedure av_close_input_file (s: PAVFormatContext);
  cdecl; external dll_name;

(**
 * Add a new stream to a media file.
 *
 * Can only be called in the read_header() function. If the flag
 * AVFMTCTX_NOHEADER is in the format context, then new streams
 * can be added in read_packet too.
 *
 * @param s media file handle
 * @param id file format dependent stream id
 *)
function av_new_stream (s: PAVFormatContext; id: int): PAVStream;
  cdecl; external dll_name;
function av_new_program (s: PAVFormatContext; id: int): PAVProgram;
  cdecl; external dll_name;

(**
 * Add a new chapter.
 * This function is NOT part of the public API
 * and should be ONLY used by demuxers.
 *
 * @param s media file handle
 * @param id unique id for this chapter
 * @param start chapter start time in time_base units
 * @param end chapter end time in time_base units
 * @param title chapter title
 *
 * @return AVChapter or NULL if error.
 *)
function ff_new_chapter (s: PAVFormatContext; id: int; time_base: TAVRational;
                         start, _end: int64; const title: pchar): PAVChapter;
  cdecl; external dll_name;

(**
 * Set the pts for a given stream.
 *
 * @param s stream
 * @param pts_wrap_bits number of bits effectively used by the pts
 *        (used for wrap control, 33 is the value for MPEG)
 * @param pts_num numerator to convert to seconds (MPEG: 1)
 * @param pts_den denominator to convert to seconds (MPEG: 90000)
 *)
procedure av_set_pts_info (s: PAVStream; pts_wrap_bits: int;
                     pts_num, pts_den: int);
  cdecl; external dll_name;


function av_find_default_stream_index (s: PAVFormatContext): int;
  cdecl; external dll_name;

(**
 * Gets the index for a specific timestamp.
 * @param flags if AVSEEK_FLAG_BACKWARD then the returned index will correspond to
 *                 the timestamp which is <= the requested one, if backward is 0
 *                 then it will be >=
 *              if AVSEEK_FLAG_ANY seek to any frame, only keyframes otherwise
 * @return < 0 if no such timestamp could be found
 *)
function av_index_search_timestamp (st: PAVStream; timestamp: int64; flags: int): int;
  cdecl; external dll_name;

(**
 * Ensures the index uses less memory than the maximum specified in
 * AVFormatContext.max_index_size, by discarding entries if it grows
 * too large.
 * This function is not part of the public API and should only be called
 * by demuxers.
 *)
procedure ff_reduce_index (s: PAVFormatContext; stream_index: int);
  cdecl; external dll_name;

(**
 * Add a index entry into a sorted list updateing if it is already there.
 *
 * @param timestamp timestamp in the timebase of the given stream
 *)
function av_add_index_entry (st: PAVStream; pos, timestamp: int64; size, distance, flags: int): int;
  cdecl; external dll_name;
(**
 * Does a binary search using av_index_search_timestamp() and AVCodec.read_timestamp().
 * This is not supposed to be called directly by a user application, but by demuxers.
 * @param target_ts target timestamp in the time base of the given stream
 * @param stream_index stream number
 *)
function av_seek_frame_binary (s: PAVFormatContext; stream_index: int; target_ts: int64; flags: int): int;
  cdecl; external dll_name;

(**
 * Updates cur_dts of all streams based on given timestamp and AVStream.
 *
 * Stream ref_st unchanged, others set cur_dts in their native timebase
 * only needed for timestamp wrapping or if (dts not set and pts!=dts).
 * @param timestamp new dts expressed in time_base of param ref_st
 * @param ref_st reference stream giving time_base of param timestamp
 *)
procedure av_update_cur_dts (s: PAVFormatContext; ref_st: PAVStream; timestamp: int64);
  cdecl; external dll_name;

(**
 * Does a binary search using read_timestamp().
 * This is not supposed to be called directly by a user application, but by demuxers.
 * @param target_ts target timestamp in the time base of the given stream
 * @param stream_index stream number
 *)
function av_gen_search (s: PAVFormatContext; stream_index: int;
                 target_ts, pos_min, pos_max, pos_limit, ts_min, ts_max: int64;
                 flags: int; ts_ret: PInt64; read_timestamp: pointer): int64;
  cdecl; external dll_name;

(** media file output *)
function av_set_parameters (s: PAVFormatContext; ap: PAVFormatParameters): int;
  cdecl; external dll_name;

(**
 * Allocate the stream private data and write the stream header to an
 * output media file.
 *
 * @param s media file handle
 * @return 0 if OK. AVERROR_xxx if error.
 *)
function av_write_header (s: PAVFormatContext): int;
  cdecl; external dll_name;

(**
 * Write a packet to an output media file.
 *
 * The packet shall contain one audio or video frame.
 * The packet must be correctly interleaved according to the container specification,
 * if not then av_interleaved_write_frame must be used
 *
 * @param s media file handle
 * @param pkt the packet, which contains the stream_index, buf/buf_size, dts/pts, ...
 * @return < 0 if error, = 0 if OK, 1 if end of stream wanted.
 *)
function av_write_frame (s: PAVFormatContext; pkt: PAVPacket): int;
  cdecl; external dll_name;

(**
 * Writes a packet to an output media file ensuring correct interleaving.
 *
 * The packet must contain one audio or video frame.
 * If the packets are already correctly interleaved the application should
 * call av_write_frame() instead as it is slightly faster. It is also important
 * to keep in mind that completely non-interleaved input will need huge amounts
 * of memory to interleave with this, so it is preferable to interleave at the
 * demuxer level.
 *
 * @param s media file handle
 * @param pkt the packet, which contains the stream_index, buf/buf_size, dts/pts, ...
 * @return < 0 if error, = 0 if OK, 1 if end of stream wanted.
 *)
function av_interleaved_write_frame (s: PAVFormatContext; pkt: PAVPacket): int;
  cdecl; external dll_name;

(**
 * Interleave a packet per DTS in an output media file.
 *
 * Packets with pkt->destruct == av_destruct_packet will be freed inside this function,
 * so they cannot be used after it, note calling av_free_packet() on them is still safe.
 *
 * @param s media file handle
 * @param out the interleaved packet will be output here
 * @param in the input packet
 * @param flush 1 if no further packets are available as input and all
 *              remaining packets should be output
 * @return 1 if a packet was output, 0 if no packet could be output,
 *         < 0 if an error occured
 *)
function av_interleave_packet_per_dts (s: PAVFormatContext; _out: PAVPacket; pkt: PAVPacket; flush: int): int;
  cdecl; external dll_name;

(**
 * @brief Write the stream trailer to an output media file and
 *        free the file private data.
 *
 * May only be called after a successful call to av_write_header.
 *
 * @param s media file handle
 * @return 0 if OK. AVERROR_xxx if error.
 *)
function av_write_trailer (s: PAVFormatContext): int;
  cdecl; external dll_name;

procedure dump_format (ic: PAVFormatContext;
                 index: int;
                 const url: pchar;
                 is_output: int);
  cdecl; external dll_name;

(**
 * parses width and height out of string str.
 * @deprecated Use av_parse_video_frame_size instead.
 *)
function parse_image_size (width_ptr, height_ptr: PInteger; const str: pchar): int;
  cdecl; external dll_name; deprecated;

(**
 * Converts frame rate from string to a fraction.
 * @deprecated Use av_parse_video_frame_rate instead.
 *)
function parse_frame_rate (frame_rate, frame_rate_base: PInteger; const arg: pchar): int;
  cdecl; external dll_name; deprecated;

(**
 * Parses \p datestr and returns a corresponding number of microseconds.
 * @param datestr String representing a date or a duration.
 * - If a date the syntax is:
 * @code
 *  [{YYYY-MM-DD|YYYYMMDD}]{T| }{HH[:MM[:SS[.m...]]][Z]|HH[MM[SS[.m...]]][Z]}
 * @endcode
 * Time is localtime unless Z is appended, in which case it is
 * interpreted as UTC.
 * If the year-month-day part isn't specified it takes the current
 * year-month-day.
 * Returns the number of microseconds since 1st of January, 1970 up to
 * the time of the parsed date or INT64_MIN if \p datestr cannot be
 * successfully parsed.
 * - If a duration the syntax is:
 * @code
 *  [-]HH[:MM[:SS[.m...]]]
 *  [-]S+[.m...]
 * @endcode
 * Returns the number of microseconds contained in a time interval
 * with the specified duration or INT64_MIN if \p datestr cannot be
 * successfully parsed.
 * @param duration Flag which tells how to interpret \p datestr, if
 * not zero \p datestr is interpreted as a duration, otherwise as a
 * date.
 *)
function parse_date (const datestr: pchar; duration: int): int64;
  cdecl; external dll_name;

function av_gettime (): int64;
  cdecl; external dll_name;

(* ffm specific for ffserver *)

function ffm_read_write_index (fd: int): int64;
  cdecl; external dll_name;
procedure ffm_write_write_index (fd: int; pos: int64);
  cdecl; external dll_name;
procedure ffm_set_write_index (s: PAVFormatContext; pos, file_size: int64);
  cdecl; external dll_name;

(**
 * Attempts to find a specific tag in a URL.
 *
 * syntax: '?tag1=val1&tag2=val2...'. Little URL decoding is done.
 * Return 1 if found.
 *)
function find_info_tag (arg: pchar; arg_size: int; const tag1: pchar; const info: pchar): int;
  cdecl; external dll_name;

(**
 * Returns in 'buf' the path with '%d' replaced by number.

 * Also handles the '%0nd' format where 'n' is the total number
 * of digits and '%%'.
 *
 * @param buf destination buffer
 * @param buf_size destination buffer size
 * @param path numbered sequence string
 * @param number frame number
 * @return 0 if OK, -1 if format error.
 *)
function av_get_frame_filename (buf: pchar; buf_size: int;
                          const path: pchar; number: int): pchar;
  cdecl; external dll_name;

(**
 * Check whether filename actually is a numbered sequence generator.
 *
 * @param filename possible numbered sequence string
 * @return 1 if a valid numbered sequence string, 0 otherwise.
 *)
function av_filename_number_test (const filename: pchar): int;
  cdecl; external dll_name;

(**
 * Generate an SDP for an RTP session.
 *
 * @param ac array of AVFormatContexts describing the RTP streams. If the
 *           array is composed by only one context, such context can contain
 *           multiple AVStreams (one AVStream per RTP stream). Otherwise,
 *           all the contexts in the array (an AVCodecContext per RTP stream)
 *           must contain only one AVStream
 * @param n_files number of AVCodecContexts contained in ac
 * @param buff buffer where the SDP will be stored (must be allocated by
 *             the caller
 * @param size the size of the buffer
 * @return 0 if OK. AVERROR_xxx if error.
 *)
function avf_sdp_create (ac: PAVFormatContext; n_files: int; buff: pchar; size: int): int;
  cdecl; external dll_name;

function resolve_host (sin_addr: pointer; const hostname: pchar): int;
  cdecl; external dll_name;

procedure url_split (proto: pchar; proto_size: int;
               authorization: pchar; authorization_size: int;
               hostname: pchar; hostname_size: int;
               port_ptr: pinteger;
               path: pchar; path_size: int;
               const url: pchar);
  cdecl; external dll_name;

function match_ext (const filename: pchar; const extensions: pchar): int;
  cdecl; external dll_name;

implementation

procedure av_free_packet (pkt: PAVPacket);
begin
  if (pkt <> nil) and (@pkt.destruct <> nil) then
    pkt.destruct (pkt);
end;

end.
