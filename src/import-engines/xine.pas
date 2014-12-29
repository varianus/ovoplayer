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
{$mode objfpc}
unit xine;
interface
uses types, classes, ctypes;

  const
    {$IFDEF WINDOWS}
      External_library='libxine-1.dll';
    {$ELSE}
      External_library='libxine.so.2';
    {$ENDIF WINDOWS}

{$I ptypes.inc}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  var
    xine_get_version_string : function:Pchar;cdecl;
    xine_get_version : procedure(var major:longint; var minor:longint; var sub:longint);cdecl;
  { compare given version to libxine version,
     return 1 if compatible, 0 otherwise  }
    xine_check_version : function(major:longint; minor:longint; sub:longint):longint;cdecl;
  { static info - which libxine release this header came from  }

  const
    XINE_MAJOR_VERSION = 1;
    XINE_MINOR_VERSION = 1;
    XINE_SUB_VERSION = 19;
    XINE_VERSION = '1.1.19';

   XINE_VISUAL_TYPE_NONE          = 0;
   {$EXTERNALSYM XINE_VISUAL_TYPE_NONE}
   XINE_VISUAL_TYPE_X11           = 1;
   {$EXTERNALSYM XINE_VISUAL_TYPE_X11}
   XINE_VISUAL_TYPE_AA            = 2;
   {$EXTERNALSYM XINE_VISUAL_TYPE_AA}
   XINE_VISUAL_TYPE_FB            = 3;
   {$EXTERNALSYM XINE_VISUAL_TYPE_FB}
   XINE_VISUAL_TYPE_GTK           = 4;
   {$EXTERNALSYM XINE_VISUAL_TYPE_GTK}
   XINE_VISUAL_TYPE_DFB           = 5;
   {$EXTERNALSYM XINE_VISUAL_TYPE_DFB}
   XINE_VISUAL_TYPE_PM            = 6;
   {$EXTERNALSYM XINE_VISUAL_TYPE_PM}
   XINE_VISUAL_TYPE_DIRECTX	    = 7;
   {$EXTERNALSYM XINE_VISUAL_TYPE_DIRECTX}

   XINE_MASTER_SLAVE_PLAY	    = $01;
   {$EXTERNALSYM XINE_MASTER_SLAVE_PLAY}
   XINE_MASTER_SLAVE_STOP	    = $10;
   {$EXTERNALSYM XINE_MASTER_SLAVE_STOP}

   XINE_TRICK_MODE_OFF              = 0;
   {$EXTERNALSYM XINE_TRICK_MODE_OFF}
   XINE_TRICK_MODE_SEEK_TO_POSITION = 1;
   {$EXTERNALSYM XINE_TRICK_MODE_SEEK_TO_POSITION}
   XINE_TRICK_MODE_SEEK_TO_TIME     = 2;
   {$EXTERNALSYM XINE_TRICK_MODE_SEEK_TO_TIME}
   XINE_TRICK_MODE_FAST_FORWARD     = 3;
   {$EXTERNALSYM XINE_TRICK_MODE_FAST_FORWARD}
   XINE_TRICK_MODE_FAST_REWIND      = 4;
   {$EXTERNALSYM XINE_TRICK_MODE_FAST_REWIND}

   XINE_ENGINE_PARAM_VERBOSITY      = 1;
   {$EXTERNALSYM XINE_ENGINE_PARAM_VERBOSITY}

//{$IFDEF USE_INTERNAL_XINE_DEFINES}
   AUDIO_DECODER_IFACE_VERSION      = 13;
   {$EXTERNALSYM AUDIO_DECODER_IFACE_VERSION}
   AUDIO_OUT_IFACE_VERSION          = 7;
   {$EXTERNALSYM AUDIO_OUT_IFACE_VERSION}
   AO_CAP_NOCAP                     = $00000000;
   {$EXTERNALSYM AO_CAP_NOCAP}
   AO_CAP_MODE_A52                  = $00000001;
   {$EXTERNALSYM AO_CAP_MODE_A52}
   AO_CAP_MODE_AC5                  = $00000002;
   {$EXTERNALSYM AO_CAP_MODE_AC5}
   AO_CAP_MODE_MONO                 = $00000004;
   {$EXTERNALSYM AO_CAP_MODE_MONO}
   AO_CAP_MODE_STEREO               = $00000008;
   {$EXTERNALSYM AO_CAP_MODE_STEREO}
   AO_CAP_MODE_4CHANNEL             = $00000010;
   {$EXTERNALSYM AO_CAP_MODE_4CHANNEL}
   AO_CAP_MODE_5CHANNEL             = $00000020;
   {$EXTERNALSYM AO_CAP_MODE_5CHANNEL}
   AO_CAP_MODE_5_1CHANNEL           = $00000040;
   {$EXTERNALSYM AO_CAP_MODE_5_1CHANNEL}
   AO_CAP_MIXER_VOL                 = $00000080;
   {$EXTERNALSYM AO_CAP_MIXER_VOL}
   AO_CAP_PCM_VOL                   = $00000100;
   {$EXTERNALSYM AO_CAP_PCM_VOL}
   AO_CAP_MUTE_VOL                  = $00000200;
   {$EXTERNALSYM AO_CAP_MUTE_VOL}
   AO_CAP_8BITS                     = $00000400;
   {$EXTERNALSYM AO_CAP_8BITS}
   AO_PROP_MIXER_VOL                = 0;
   {$EXTERNALSYM AO_PROP_MIXER_VOL}
   AO_PROP_PCM_VOL                  = 1;
   {$EXTERNALSYM AO_PROP_PCM_VOL}
   AO_PROP_MUTE_VOL                 = 2;
   {$EXTERNALSYM AO_PROP_MUTE_VOL}
   AO_PROP_COMPRESSOR               = 3;
   {$EXTERNALSYM AO_PROP_COMPRESSOR}
   AO_PROP_DISCARD_BUFFERS          = 4;
   {$EXTERNALSYM AO_PROP_DISCARD_BUFFERS}

   AO_PROP_PAUSED                   = 5;
   {$EXTERNALSYM AO_PROP_PAUSED}
   AO_PROP_AMP                      = 6;
   {$EXTERNALSYM AO_PROP_AMP}
   AO_PROP_EQ_30HZ                  = 7;
   {$EXTERNALSYM AO_PROP_EQ_30HZ}
   AO_PROP_EQ_60HZ                  = 8;
   {$EXTERNALSYM AO_PROP_EQ_60HZ}
   AO_PROP_EQ_125HZ                 = 9;
   {$EXTERNALSYM AO_PROP_EQ_125HZ}
   AO_PROP_EQ_250HZ                 = 10;
   {$EXTERNALSYM AO_PROP_EQ_250HZ}
   AO_PROP_EQ_500HZ                 = 11;
   {$EXTERNALSYM AO_PROP_EQ_500HZ}
   AO_PROP_EQ_1000HZ                = 12;
   {$EXTERNALSYM AO_PROP_EQ_1000HZ}
   AO_PROP_EQ_2000HZ                = 13;
   {$EXTERNALSYM AO_PROP_EQ_2000HZ}
   AO_PROP_EQ_4000HZ                = 14;
   {$EXTERNALSYM AO_PROP_EQ_4000HZ}
   AO_PROP_EQ_8000HZ                = 15;
   {$EXTERNALSYM AO_PROP_EQ_8000HZ}
   AO_PROP_EQ_16000HZ               = 16;
   {$EXTERNALSYM AO_PROP_EQ_16000HZ}
   AO_CTRL_PLAY_PAUSE               = 0;
   {$EXTERNALSYM AO_CTRL_PLAY_PAUSE}
   AO_CTRL_PLAY_RESUME              = 1;
   {$EXTERNALSYM AO_CTRL_PLAY_RESUME}
   AO_CTRL_FLUSH_BUFFERS            = 2;
   {$EXTERNALSYM AO_CTRL_FLUSH_BUFFERS}
   AO_MAX_GAP                       = 15000;
   {$EXTERNALSYM AO_MAX_GAP}

   BUF_MAJOR_MASK                   = $FF000000;
   {$EXTERNALSYM BUF_MAJOR_MASK}
   BUF_DECODER_MASK                 = $00FF0000;
   {$EXTERNALSYM BUF_DECODER_MASK}

   BUF_CONTROL_BASE                 = $01000000;
   {$EXTERNALSYM BUF_CONTROL_BASE}
   BUF_CONTROL_START                = $01000000;
   {$EXTERNALSYM BUF_CONTROL_START}
   BUF_CONTROL_END                  = $01010000;
   {$EXTERNALSYM BUF_CONTROL_END}
   BUF_CONTROL_QUIT                 = $01020000;
   {$EXTERNALSYM BUF_CONTROL_QUIT}
   BUF_CONTROL_DISCONTINUITY        = $01030000;
   {$EXTERNALSYM BUF_CONTROL_DISCONTINUITY}
   BUF_CONTROL_NOP                  = $01040000;
   {$EXTERNALSYM BUF_CONTROL_NOP}
   BUF_CONTROL_AUDIO_CHANNEL        = $01050000;
   {$EXTERNALSYM BUF_CONTROL_AUDIO_CHANNEL}
   BUF_CONTROL_SPU_CHANNEL          = $01060000;
   {$EXTERNALSYM BUF_CONTROL_SPU_CHANNEL}
   BUF_CONTROL_NEWPTS               = $01070000;
   {$EXTERNALSYM BUF_CONTROL_NEWPTS}
   BUF_CONTROL_RESET_DECODER        = $01080000;
   {$EXTERNALSYM BUF_CONTROL_RESET_DECODER}
   BUF_CONTROL_HEADERS_DONE         = $01090000;
   {$EXTERNALSYM BUF_CONTROL_HEADERS_DONE}
   BUF_CONTROL_FLUSH_DECODER        = $010a0000;
   {$EXTERNALSYM BUF_CONTROL_FLUSH_DECODER}

   BUF_VIDEO_BASE                   = $02000000;
   {$EXTERNALSYM BUF_VIDEO_BASE}
   BUF_VIDEO_UNKNOWN                = $02ff0000;
   {$EXTERNALSYM BUF_VIDEO_UNKNOWN}
   BUF_VIDEO_MPEG                   = $02000000;
   {$EXTERNALSYM BUF_VIDEO_MPEG}
   BUF_VIDEO_MPEG4                  = $02010000;
   {$EXTERNALSYM BUF_VIDEO_MPEG4}
   BUF_VIDEO_CINEPAK                = $02020000;
   {$EXTERNALSYM BUF_VIDEO_CINEPAK}
   BUF_VIDEO_SORENSON_V1            = $02030000;
   {$EXTERNALSYM BUF_VIDEO_SORENSON_V1}
   BUF_VIDEO_MSMPEG4_V2             = $02040000;
   {$EXTERNALSYM BUF_VIDEO_MSMPEG4_V2}
   BUF_VIDEO_MSMPEG4_V3             = $02050000;
   {$EXTERNALSYM BUF_VIDEO_MSMPEG4_V3}
   BUF_VIDEO_MJPEG                  = $02060000;
   {$EXTERNALSYM BUF_VIDEO_MJPEG}
   BUF_VIDEO_IV50                   = $02070000;
   {$EXTERNALSYM BUF_VIDEO_IV50}
   BUF_VIDEO_IV41                   = $02080000;
   {$EXTERNALSYM BUF_VIDEO_IV41}
   BUF_VIDEO_IV32                   = $02090000;
   {$EXTERNALSYM BUF_VIDEO_IV32}
   BUF_VIDEO_IV31                   = $020a0000;
   {$EXTERNALSYM BUF_VIDEO_IV31}
   BUF_VIDEO_ATIVCR1                = $020b0000;
   {$EXTERNALSYM BUF_VIDEO_ATIVCR1}
   BUF_VIDEO_ATIVCR2                = $020c0000;
   {$EXTERNALSYM BUF_VIDEO_ATIVCR2}
   BUF_VIDEO_I263                   = $020d0000;
   {$EXTERNALSYM BUF_VIDEO_I263}
   BUF_VIDEO_RV10                   = $020e0000;
   {$EXTERNALSYM BUF_VIDEO_RV10}
   BUF_VIDEO_RGB                    = $02100000;
   {$EXTERNALSYM BUF_VIDEO_RGB}
   BUF_VIDEO_YUY2                   = $02110000;
   {$EXTERNALSYM BUF_VIDEO_YUY2}
   BUF_VIDEO_JPEG                   = $02120000;
   {$EXTERNALSYM BUF_VIDEO_JPEG}
   BUF_VIDEO_WMV7                   = $02130000;
   {$EXTERNALSYM BUF_VIDEO_WMV7}
   BUF_VIDEO_WMV8                   = $02140000;
   {$EXTERNALSYM BUF_VIDEO_WMV8}
   BUF_VIDEO_MSVC                   = $02150000;
   {$EXTERNALSYM BUF_VIDEO_MSVC}
   BUF_VIDEO_DV                     = $02160000;
   {$EXTERNALSYM BUF_VIDEO_DV}
   BUF_VIDEO_REAL                   = $02170000;
   {$EXTERNALSYM BUF_VIDEO_REAL}
   BUF_VIDEO_VP31                   = $02180000;
   {$EXTERNALSYM BUF_VIDEO_VP31}
   BUF_VIDEO_H263                   = $02190000;
   {$EXTERNALSYM BUF_VIDEO_H263}
   BUF_VIDEO_3IVX                   = $021A0000;
   {$EXTERNALSYM BUF_VIDEO_3IVX}
   BUF_VIDEO_CYUV                   = $021B0000;
   {$EXTERNALSYM BUF_VIDEO_CYUV}
   BUF_VIDEO_DIVX5                  = $021C0000;
   {$EXTERNALSYM BUF_VIDEO_DIVX5}
   BUF_VIDEO_XVID                   = $021D0000;
   {$EXTERNALSYM BUF_VIDEO_XVID}
   BUF_VIDEO_SMC                    = $021E0000;
   {$EXTERNALSYM BUF_VIDEO_SMC}
   BUF_VIDEO_RPZA                   = $021F0000;
   {$EXTERNALSYM BUF_VIDEO_RPZA}
   BUF_VIDEO_QTRLE                  = $02200000;
   {$EXTERNALSYM BUF_VIDEO_QTRLE}
   BUF_VIDEO_MSRLE                  = $02210000;
   {$EXTERNALSYM BUF_VIDEO_MSRLE}
   BUF_VIDEO_DUCKTM1                = $02220000;
   {$EXTERNALSYM BUF_VIDEO_DUCKTM1}
   BUF_VIDEO_FLI                    = $02230000;
   {$EXTERNALSYM BUF_VIDEO_FLI}
   BUF_VIDEO_ROQ                    = $02240000;
   {$EXTERNALSYM BUF_VIDEO_ROQ}
   BUF_VIDEO_SORENSON_V3            = $02250000;
   {$EXTERNALSYM BUF_VIDEO_SORENSON_V3}
   BUF_VIDEO_MSMPEG4_V1             = $02260000;
   {$EXTERNALSYM BUF_VIDEO_MSMPEG4_V1}
   BUF_VIDEO_MSS1                   = $02270000;
   {$EXTERNALSYM BUF_VIDEO_MSS1}
   BUF_VIDEO_IDCIN                  = $02280000;
   {$EXTERNALSYM BUF_VIDEO_IDCIN}
   BUF_VIDEO_PGVV                   = $02290000;
   {$EXTERNALSYM BUF_VIDEO_PGVV}
   BUF_VIDEO_ZYGO                   = $022A0000;
   {$EXTERNALSYM BUF_VIDEO_ZYGO}
   BUF_VIDEO_TSCC                   = $022B0000;
   {$EXTERNALSYM BUF_VIDEO_TSCC}
   BUF_VIDEO_YVU9                   = $022C0000;
   {$EXTERNALSYM BUF_VIDEO_YVU9}
   BUF_VIDEO_VQA                    = $022D0000;
   {$EXTERNALSYM BUF_VIDEO_VQA}
   BUF_VIDEO_GREY                   = $022E0000;
   {$EXTERNALSYM BUF_VIDEO_GREY}
   BUF_VIDEO_XXAN                   = $022F0000;
   {$EXTERNALSYM BUF_VIDEO_XXAN}
   BUF_VIDEO_WC3                    = $02300000;
   {$EXTERNALSYM BUF_VIDEO_WC3}
   BUF_VIDEO_YV12                   = $02310000;
   {$EXTERNALSYM BUF_VIDEO_YV12}
   BUF_VIDEO_SEGA                   = $02320000;
   {$EXTERNALSYM BUF_VIDEO_SEGA}
   BUF_VIDEO_RV20                   = $02330000;
   {$EXTERNALSYM BUF_VIDEO_RV20}
   BUF_VIDEO_RV30                   = $02340000;
   {$EXTERNALSYM BUF_VIDEO_RV30}
   BUF_VIDEO_MVI2                   = $02350000;
   {$EXTERNALSYM BUF_VIDEO_MVI2}
   BUF_VIDEO_UCOD                   = $02360000;
   {$EXTERNALSYM BUF_VIDEO_UCOD}
   BUF_VIDEO_WMV9                   = $02370000;
   {$EXTERNALSYM BUF_VIDEO_WMV9}
   BUF_VIDEO_INTERPLAY              = $02380000;
   {$EXTERNALSYM BUF_VIDEO_INTERPLAY}
   BUF_VIDEO_RV40                   = $02390000;
   {$EXTERNALSYM BUF_VIDEO_RV40}
   BUF_VIDEO_PSX_MDEC               = $023A0000;
   {$EXTERNALSYM BUF_VIDEO_PSX_MDEC}
   BUF_VIDEO_YUV_FRAMES             = $023B0000;
   {$EXTERNALSYM BUF_VIDEO_YUV_FRAMES}
   BUF_VIDEO_HUFFYUV                = $023C0000;
   {$EXTERNALSYM BUF_VIDEO_HUFFYUV}
   BUF_VIDEO_IMAGE                  = $023D0000;
   {$EXTERNALSYM BUF_VIDEO_IMAGE}
   BUF_VIDEO_THEORA                 = $023E0000;
   {$EXTERNALSYM BUF_VIDEO_THEORA}

   BUF_AUDIO_BASE                   = $03000000;
   {$EXTERNALSYM BUF_AUDIO_BASE}
   BUF_AUDIO_UNKNOWN                = $03ff0000;
   {$EXTERNALSYM BUF_AUDIO_UNKNOWN}
   BUF_AUDIO_A52                    = $03000000;
   {$EXTERNALSYM BUF_AUDIO_A52}
   BUF_AUDIO_MPEG                   = $03010000;
   {$EXTERNALSYM BUF_AUDIO_MPEG}
   BUF_AUDIO_LPCM_BE                = $03020000;
   {$EXTERNALSYM BUF_AUDIO_LPCM_BE}
   BUF_AUDIO_LPCM_LE                = $03030000;
   {$EXTERNALSYM BUF_AUDIO_LPCM_LE}
   BUF_AUDIO_WMAV1                  = $03040000;
   {$EXTERNALSYM BUF_AUDIO_WMAV1}
   BUF_AUDIO_DTS                    = $03050000;
   {$EXTERNALSYM BUF_AUDIO_DTS}
   BUF_AUDIO_MSADPCM                = $03060000;
   {$EXTERNALSYM BUF_AUDIO_MSADPCM}
   BUF_AUDIO_MSIMAADPCM             = $03070000;
   {$EXTERNALSYM BUF_AUDIO_MSIMAADPCM}
   BUF_AUDIO_MSGSM                  = $03080000;
   {$EXTERNALSYM BUF_AUDIO_MSGSM}
   BUF_AUDIO_VORBIS                 = $03090000;
   {$EXTERNALSYM BUF_AUDIO_VORBIS}
   BUF_AUDIO_IMC                    = $030a0000;
   {$EXTERNALSYM BUF_AUDIO_IMC}
   BUF_AUDIO_LH                     = $030b0000;
   {$EXTERNALSYM BUF_AUDIO_LH}
   BUF_AUDIO_VOXWARE                = $030c0000;
   {$EXTERNALSYM BUF_AUDIO_VOXWARE}
   BUF_AUDIO_ACELPNET               = $030d0000;
   {$EXTERNALSYM BUF_AUDIO_ACELPNET}
   BUF_AUDIO_AAC                    = $030e0000;
   {$EXTERNALSYM BUF_AUDIO_AAC}
   BUF_AUDIO_DNET                   = $030f0000;
   {$EXTERNALSYM BUF_AUDIO_DNET}
   BUF_AUDIO_VIVOG723               = $03100000;
   {$EXTERNALSYM BUF_AUDIO_VIVOG723}
   BUF_AUDIO_DK3ADPCM               = $03110000;
   {$EXTERNALSYM BUF_AUDIO_DK3ADPCM}
   BUF_AUDIO_DK4ADPCM               = $03120000;
   {$EXTERNALSYM BUF_AUDIO_DK4ADPCM}
   BUF_AUDIO_ROQ                    = $03130000;
   {$EXTERNALSYM BUF_AUDIO_ROQ}
   BUF_AUDIO_QTIMAADPCM             = $03140000;
   {$EXTERNALSYM BUF_AUDIO_QTIMAADPCM}
   BUF_AUDIO_MAC3                   = $03150000;
   {$EXTERNALSYM BUF_AUDIO_MAC3}
   BUF_AUDIO_MAC6                   = $03160000;
   {$EXTERNALSYM BUF_AUDIO_MAC6}
   BUF_AUDIO_QDESIGN1               = $03170000;
   {$EXTERNALSYM BUF_AUDIO_QDESIGN1}
   BUF_AUDIO_QDESIGN2               = $03180000;
   {$EXTERNALSYM BUF_AUDIO_QDESIGN2}
   BUF_AUDIO_QCLP                   = $03190000;
   {$EXTERNALSYM BUF_AUDIO_QCLP}
   BUF_AUDIO_SMJPEG_IMA             = $031A0000;
   {$EXTERNALSYM BUF_AUDIO_SMJPEG_IMA}
   BUF_AUDIO_VQA_IMA                = $031B0000;
   {$EXTERNALSYM BUF_AUDIO_VQA_IMA}
   BUF_AUDIO_MULAW                  = $031C0000;
   {$EXTERNALSYM BUF_AUDIO_MULAW}
   BUF_AUDIO_ALAW                   = $031D0000;
   {$EXTERNALSYM BUF_AUDIO_ALAW}
   BUF_AUDIO_GSM610                 = $031E0000;
   {$EXTERNALSYM BUF_AUDIO_GSM610}
   BUF_AUDIO_EA_ADPCM               = $031F0000;
   {$EXTERNALSYM BUF_AUDIO_EA_ADPCM}
   BUF_AUDIO_WMAV2                  = $03200000;
   {$EXTERNALSYM BUF_AUDIO_WMAV2}
   BUF_AUDIO_COOK                   = $03210000;
   {$EXTERNALSYM BUF_AUDIO_COOK}
   BUF_AUDIO_ATRK                   = $03220000;
   {$EXTERNALSYM BUF_AUDIO_ATRK}
   BUF_AUDIO_14_4                   = $03230000;
   {$EXTERNALSYM BUF_AUDIO_14_4}
   BUF_AUDIO_28_8                   = $03240000;
   {$EXTERNALSYM BUF_AUDIO_28_8}
   BUF_AUDIO_SIPRO                  = $03250000;
   {$EXTERNALSYM BUF_AUDIO_SIPRO}
   BUF_AUDIO_WMAV3                  = $03260000;
   {$EXTERNALSYM BUF_AUDIO_WMAV3}
   BUF_AUDIO_INTERPLAY              = $03270000;
   {$EXTERNALSYM BUF_AUDIO_INTERPLAY}
   BUF_AUDIO_XA_ADPCM               = $03280000;
   {$EXTERNALSYM BUF_AUDIO_XA_ADPCM}
   BUF_AUDIO_WESTWOOD               = $03290000;
   {$EXTERNALSYM BUF_AUDIO_WESTWOOD}
   BUF_AUDIO_DIALOGIC_IMA           = $032A0000;
   {$EXTERNALSYM BUF_AUDIO_DIALOGIC_IMA}
   BUF_AUDIO_NSF                    = $032B0000;
   {$EXTERNALSYM BUF_AUDIO_NSF}
   BUF_AUDIO_FLAC                   = $032C0000;
   {$EXTERNALSYM BUF_AUDIO_FLAC}
   BUF_AUDIO_DV                     = $032D0000;
   {$EXTERNALSYM BUF_AUDIO_DV}
   BUF_AUDIO_WMAV                   = $032E0000;
   {$EXTERNALSYM BUF_AUDIO_WMAV}

   BUF_SPU_BASE                     = $04000000;
   {$EXTERNALSYM BUF_SPU_BASE}
   BUF_SPU_DVD                      = $04000000;
   {$EXTERNALSYM BUF_SPU_DVD}
   BUF_SPU_TEXT                     = $04010000;
   {$EXTERNALSYM BUF_SPU_TEXT}
   BUF_SPU_CC                       = $04020000;
   {$EXTERNALSYM BUF_SPU_CC}
   BUF_SPU_DVB                      = $04030000;
   {$EXTERNALSYM BUF_SPU_DVB}
   BUF_SPU_SVCD                     = $04040000;
   {$EXTERNALSYM BUF_SPU_SVCD}
   BUF_SPU_CVD                      = $04050000;
   {$EXTERNALSYM BUF_SPU_CVD}
   BUF_SPU_OGM                      = $04060000;
   {$EXTERNALSYM BUF_SPU_OGM}

   BUF_DEMUX_BLOCK                  = $05000000;
   {$EXTERNALSYM BUF_DEMUX_BLOCK}

   BUF_FLAG_KEYFRAME                = $0001;
   {$EXTERNALSYM BUF_FLAG_KEYFRAME}
   BUF_FLAG_FRAME_START             = $0002;
   {$EXTERNALSYM BUF_FLAG_FRAME_START}
   BUF_FLAG_FRAME_END               = $0004;
   {$EXTERNALSYM BUF_FLAG_FRAME_END}
   BUF_FLAG_HEADER                  = $0008;
   {$EXTERNALSYM BUF_FLAG_HEADER}
   BUF_FLAG_PREVIEW                 = $0010;
   {$EXTERNALSYM BUF_FLAG_PREVIEW}
   BUF_FLAG_END_USER                = $0020;
   {$EXTERNALSYM BUF_FLAG_END_USER}
   BUF_FLAG_END_STREAM              = $0040;
   {$EXTERNALSYM BUF_FLAG_END_STREAM}
   BUF_FLAG_FRAMERATE               = $0080;
   {$EXTERNALSYM BUF_FLAG_FRAMERATE}
   BUF_FLAG_SEEK                    = $0100;
   {$EXTERNALSYM BUF_FLAG_SEEK}
   BUF_FLAG_SPECIAL                 = $0200;
   {$EXTERNALSYM BUF_FLAG_SPECIAL}
   BUF_FLAG_NO_VIDEO                = $0400;
   {$EXTERNALSYM BUF_FLAG_NO_VIDEO}
   BUF_FLAG_FRAMING                 = $0800;
   {$EXTERNALSYM BUF_FLAG_FRAMING}

   BUF_SPECIAL_PALETTE              = 1;
   {$EXTERNALSYM BUF_SPECIAL_PALETTE}
   BUF_SPECIAL_IDCIN_HUFFMAN_TABLE  = 2;
   {$EXTERNALSYM BUF_SPECIAL_IDCIN_HUFFMAN_TABLE}
   BUF_SPECIAL_ASPECT               = 3;
   {$EXTERNALSYM BUF_SPECIAL_ASPECT}
   BUF_SPECIAL_DECODER_CONFIG       = 4;
   {$EXTERNALSYM BUF_SPECIAL_DECODER_CONFIG}
   BUF_SPECIAL_STSD_ATOM            = 5;
   {$EXTERNALSYM BUF_SPECIAL_STSD_ATOM}
   BUF_SPECIAL_LPCM_CONFIG          = 6;
   {$EXTERNALSYM BUF_SPECIAL_LPCM_CONFIG}
   BUF_SPECIAL_SPU_DVD_SUBTYPE      = 8;

   {$EXTERNALSYM BUF_SPECIAL_SPU_DVD_SUBTYPE}
   SPU_DVD_SUBTYPE_CLUT             = 1;
   {$EXTERNALSYM SPU_DVD_SUBTYPE_CLUT}
   SPU_DVD_SUBTYPE_PACKAGE          = 2;
   {$EXTERNALSYM SPU_DVD_SUBTYPE_PACKAGE}
   SPU_DVD_SUBTYPE_SUBP_CONTROL     = 3;
   {$EXTERNALSYM SPU_DVD_SUBTYPE_SUBP_CONTROL}
   SPU_DVD_SUBTYPE_NAV              = 4;
   {$EXTERNALSYM SPU_DVD_SUBTYPE_NAV}

   BUF_SPECIAL_SPU_DVB_DESCRIPTOR   = 9;
   {$EXTERNALSYM BUF_SPECIAL_SPU_DVB_DESCRIPTOR}
   XINE_NAME_MAX                    = MaxPathLen;
   {$EXTERNALSYM XINE_NAME_MAX}
   XINE_PATH_MAX                    = MaxPathLen;
   {$EXTERNALSYM XINE_PATH_MAX}
   CONFIG_TYPE_UNKNOWN              = 0;
   {$EXTERNALSYM CONFIG_TYPE_UNKNOWN}
   CONFIG_TYPE_RANGE                = 1;
   {$EXTERNALSYM CONFIG_TYPE_RANGE}
   CONFIG_TYPE_STRING               = 2;
   {$EXTERNALSYM CONFIG_TYPE_STRING}
   CONFIG_TYPE_ENUM                 = 3;
   {$EXTERNALSYM CONFIG_TYPE_ENUM}
   CONFIG_TYPE_NUM                  = 4;
   {$EXTERNALSYM CONFIG_TYPE_NUM}
   CONFIG_TYPE_BOOL                 = 5;
   {$EXTERNALSYM CONFIG_TYPE_BOOL}

   DEMUXER_PLUGIN_IFACE_VERSION     = 21;
   {$EXTERNALSYM DEMUXER_PLUGIN_IFACE_VERSION}
   DEMUX_OK                         = 0;
   {$EXTERNALSYM DEMUX_OK}
   DEMUX_FINISHED                   = 1;
   {$EXTERNALSYM DEMUX_FINISHED}
   DEMUX_CANNOT_HANDLE              = 0;
   {$EXTERNALSYM DEMUX_CANNOT_HANDLE}
   DEMUX_CAN_HANDLE                 = 1;
   {$EXTERNALSYM DEMUX_CAN_HANDLE}

   METHOD_BY_CONTENT                = 1;
   {$EXTERNALSYM METHOD_BY_CONTENT}
   METHOD_BY_EXTENSION              = 2;
   {$EXTERNALSYM METHOD_BY_EXTENSION}
   METHOD_EXPLICIT                  = 3;
   {$EXTERNALSYM METHOD_EXPLICIT}

   DEMUX_CAP_NOCAP                  = $00000000;
   {$EXTERNALSYM DEMUX_CAP_NOCAP}
   DEMUX_CAP_AUDIOLANG              = $00000008;
   {$EXTERNALSYM DEMUX_CAP_AUDIOLANG}
   DEMUX_CAP_SPULANG                = $00000010;
   {$EXTERNALSYM DEMUX_CAP_SPULANG}

   DEMUX_OPTIONAL_UNSUPPORTED       = 0;
   {$EXTERNALSYM DEMUX_OPTIONAL_UNSUPPORTED}
   DEMUX_OPTIONAL_SUCCESS           = 1;
   {$EXTERNALSYM DEMUX_OPTIONAL_SUCCESS}
   DEMUX_OPTIONAL_DATA_AUDIOLANG    = 2;
   {$EXTERNALSYM DEMUX_OPTIONAL_DATA_AUDIOLANG}
   DEMUX_OPTIONAL_DATA_SPULANG      = 3;
   {$EXTERNALSYM DEMUX_OPTIONAL_DATA_SPULANG}

   INPUT_PLUGIN_IFACE_VERSION       = 13;
   {$EXTERNALSYM INPUT_PLUGIN_IFACE_VERSION}

   INPUT_CAP_NOCAP                  = $00000000;
   {$EXTERNALSYM INPUT_CAP_NOCAP}
   INPUT_CAP_SEEKABLE               = $00000001;
   {$EXTERNALSYM INPUT_CAP_SEEKABLE}
   INPUT_CAP_BLOCK                  = $00000002;
   {$EXTERNALSYM INPUT_CAP_BLOCK}
   INPUT_CAP_AUDIOLANG              = $00000008;
   {$EXTERNALSYM INPUT_CAP_AUDIOLANG}
   INPUT_CAP_SPULANG                = $00000010;
   {$EXTERNALSYM INPUT_CAP_SPULANG}
   INPUT_CAP_PREVIEW                = $00000040;
   {$EXTERNALSYM INPUT_CAP_PREVIEW}
   INPUT_CAP_CHAPTERS               = $00000080;
   {$EXTERNALSYM INPUT_CAP_CHAPTERS}

   INPUT_OPTIONAL_UNSUPPORTED       = 0;
   {$EXTERNALSYM INPUT_OPTIONAL_UNSUPPORTED}
   INPUT_OPTIONAL_SUCCESS           = 1;
   {$EXTERNALSYM INPUT_OPTIONAL_SUCCESS}
   INPUT_OPTIONAL_DATA_AUDIOLANG    = 2;
   {$EXTERNALSYM INPUT_OPTIONAL_DATA_AUDIOLANG}
   INPUT_OPTIONAL_DATA_SPULANG      = 3;
   {$EXTERNALSYM INPUT_OPTIONAL_DATA_SPULANG}
   INPUT_OPTIONAL_DATA_PREVIEW      = 7;
   {$EXTERNALSYM INPUT_OPTIONAL_DATA_PREVIEW}

   MAX_MRL_ENTRIES                  = 255;
   {$EXTERNALSYM MAX_MRL_ENTRIES}
   MAX_PREVIEW_SIZE                 = 4096;
   {$EXTERNALSYM MAX_PREVIEW_SIZE}

   mrl_unknown                      = $00000;
   {$EXTERNALSYM mrl_unknown}
   mrl_dvd                          = $00001;
   {$EXTERNALSYM mrl_dvd}
   mrl_vcd                          = $00002;
   {$EXTERNALSYM mrl_vcd}
   mrl_net                          = $00004;
   {$EXTERNALSYM mrl_net}
   mrl_rtp                          = $00008;
   {$EXTERNALSYM mrl_rtp}
   mrl_stdin                        = $00010;
   {$EXTERNALSYM mrl_stdin}
   mrl_cda                          = $00020;
   {$EXTERNALSYM mrl_cda}
   mrl_file                         = $00040;
   {$EXTERNALSYM mrl_file}
   mrl_file_fifo                    = $00080;
   {$EXTERNALSYM mrl_file_fifo}
   mrl_file_chardev                 = $00100;
   {$EXTERNALSYM mrl_file_chardev}
   mrl_file_directory               = $00200;
   {$EXTERNALSYM mrl_file_directory}
   mrl_file_blockdev                = $00400;
   {$EXTERNALSYM mrl_file_blockdev}
   mrl_file_normal                  = $00800;
   {$EXTERNALSYM mrl_file_normal}
   mrl_file_symlink                 = $01000;
   {$EXTERNALSYM mrl_file_symlink}
   mrl_file_sock                    = $02000;
   {$EXTERNALSYM mrl_file_sock}
   mrl_file_exec                    = $04000;
   {$EXTERNALSYM mrl_file_exec}
   mrl_file_backup                  = $08000;
   {$EXTERNALSYM mrl_file_backup}
   mrl_file_hidden                  = $10000;
   {$EXTERNALSYM mrl_file_hidden}

   PREBUFFER_PTS_OFFSET             = 12000;
   {$EXTERNALSYM PREBUFFER_PTS_OFFSET}
   DISC_STREAMSTART                 = 0;
   {$EXTERNALSYM DISC_STREAMSTART}
   DISC_RELATIVE                    = 1;
   {$EXTERNALSYM DISC_RELATIVE}
   DISC_ABSOLUTE                    = 2;
   {$EXTERNALSYM DISC_ABSOLUTE}
   DISC_STREAMSEEK                  = 3;
   {$EXTERNALSYM DISC_STREAMSEEK}

   METRONOM_AV_OFFSET               = 2;
   {$EXTERNALSYM METRONOM_AV_OFFSET}
   METRONOM_ADJ_VPTS_OFFSET         = 3;
   {$EXTERNALSYM METRONOM_ADJ_VPTS_OFFSET}
   METRONOM_FRAME_DURATION          = 4;
   {$EXTERNALSYM METRONOM_FRAME_DURATION}
   METRONOM_SPU_OFFSET              = 5;
   {$EXTERNALSYM METRONOM_SPU_OFFSET}

   CLOCK_SCR_ADJUSTABLE             = 1;
   {$EXTERNALSYM CLOCK_SCR_ADJUSTABLE}

   TEXT_PALETTE_SIZE                = 11;
   {$EXTERNALSYM TEXT_PALETTE_SIZE}
   OSD_TEXT1                        = 0*TEXT_PALETTE_SIZE;
   {$EXTERNALSYM OSD_TEXT1}
   OSD_TEXT2                        = 1*TEXT_PALETTE_SIZE;
   {$EXTERNALSYM OSD_TEXT2}
   OSD_TEXT3                        = 2*TEXT_PALETTE_SIZE;
   {$EXTERNALSYM OSD_TEXT3}
   OSD_TEXT4                        = 3*TEXT_PALETTE_SIZE;
   {$EXTERNALSYM OSD_TEXT4}
   OSD_TEXT5                        = 4*TEXT_PALETTE_SIZE;
   {$EXTERNALSYM OSD_TEXT5}
   OSD_TEXT6                        = 5*TEXT_PALETTE_SIZE;
   {$EXTERNALSYM OSD_TEXT6}
   OSD_TEXT7                        = 6*TEXT_PALETTE_SIZE;
   {$EXTERNALSYM OSD_TEXT7}
   OSD_TEXT8                        = 7*TEXT_PALETTE_SIZE;
   {$EXTERNALSYM OSD_TEXT8}
   OSD_TEXT9                        = 8*TEXT_PALETTE_SIZE;
   {$EXTERNALSYM OSD_TEXT9}
   OSD_TEXT10                       = 9*TEXT_PALETTE_SIZE;
   {$EXTERNALSYM OSD_TEXT10}

   NUMBER_OF_TEXT_PALETTES              = 4;
   {$EXTERNALSYM NUMBER_OF_TEXT_PALETTES}
   TEXTPALETTE_WHITE_BLACK_TRANSPARENT  = 0;
   {$EXTERNALSYM TEXTPALETTE_WHITE_BLACK_TRANSPARENT}
   TEXTPALETTE_WHITE_NONE_TRANSPARENT   = 1;
   {$EXTERNALSYM TEXTPALETTE_WHITE_NONE_TRANSPARENT}
   TEXTPALETTE_WHITE_NONE_TRANSLUCID    = 2;
   {$EXTERNALSYM TEXTPALETTE_WHITE_NONE_TRANSLUCID}
   TEXTPALETTE_YELLOW_BLACK_TRANSPARENT = 3;
   {$EXTERNALSYM TEXTPALETTE_YELLOW_BLACK_TRANSPARENT}

   DECODER_MAX                      = 256;
   {$EXTERNALSYM DECODER_MAX}
   PLUGIN_MAX                       = 256;
   {$EXTERNALSYM PLUGIN_MAX}
   PLUGINS_PER_TYPE                 = 10;
   {$EXTERNALSYM PLUGINS_PER_TYPE}

   CACHE_CATALOG_VERSION            = 2;
   {$EXTERNALSYM CACHE_CATALOG_VERSION}
   CACHE_CATALOG_FILE               = '.xine/catalog.cache';
   {$EXTERNALSYM CACHE_CATALOG_FILE}
   CACHE_CATALOG_DIR                = '.xine';
   {$EXTERNALSYM CACHE_CATALOG_DIR}
   POST_PLUGIN_IFACE_VERSION        = 2;
   {$EXTERNALSYM POST_PLUGIN_IFACE_VERSION}
   SPU_DECODER_IFACE_VERSION        = 14;
   {$EXTERNALSYM SPU_DECODER_IFACE_VERSION}
   SPU_OUT_IFACE_VERSION            = 1;
   {$EXTERNALSYM SPU_OUT_IFACE_VERSION}

   VIDEO_DECODER_IFACE_VERSION      = 14;
   {$EXTERNALSYM VIDEO_DECODER_IFACE_VERSION}
   VO_PROP_INTERLACED               = 0;
   {$EXTERNALSYM VO_PROP_INTERLACED}
   VO_PROP_ASPECT_RATIO             = 1;
   {$EXTERNALSYM VO_PROP_ASPECT_RATIO}
   VO_PROP_HUE                      = 2;
   {$EXTERNALSYM VO_PROP_HUE}
   VO_PROP_SATURATION               = 3;
   {$EXTERNALSYM VO_PROP_SATURATION}
   VO_PROP_CONTRAST                 = 4;
   {$EXTERNALSYM VO_PROP_CONTRAST}
   VO_PROP_BRIGHTNESS               = 5;
   {$EXTERNALSYM VO_PROP_BRIGHTNESS}
   VO_PROP_COLORKEY                 = 6;
   {$EXTERNALSYM VO_PROP_COLORKEY}
   VO_PROP_AUTOPAINT_COLORKEY       = 7;
   {$EXTERNALSYM VO_PROP_AUTOPAINT_COLORKEY}
   VO_PROP_ZOOM_X                   = 8;
   {$EXTERNALSYM VO_PROP_ZOOM_X}
   VO_PROP_PAN_SCAN                 = 9;
   {$EXTERNALSYM VO_PROP_PAN_SCAN}
   VO_PROP_TVMODE                   = 10;
   {$EXTERNALSYM VO_PROP_TVMODE}
   VO_PROP_MAX_NUM_FRAMES           = 11;
   {$EXTERNALSYM VO_PROP_MAX_NUM_FRAMES}
   VO_PROP_ZOOM_Y                   = 13;
   {$EXTERNALSYM VO_PROP_ZOOM_Y}
   VO_PROP_DISCARD_FRAMES           = 14;
   {$EXTERNALSYM VO_PROP_DISCARD_FRAMES}
   VO_NUM_PROPERTIES                = 15;
   {$EXTERNALSYM VO_NUM_PROPERTIES}

   VO_ZOOM_STEP                     = 100;
   {$EXTERNALSYM VO_ZOOM_STEP}
   VO_ZOOM_MAX                      = 400;
   {$EXTERNALSYM VO_ZOOM_MAX}
   VO_ZOOM_MIN                      = 100;
   {$EXTERNALSYM VO_ZOOM_MIN}

   OVL_PALETTE_SIZE                 = 256;
   {$EXTERNALSYM OVL_PALETTE_SIZE}
   VO_NUM_RECENT_FRAMES             = 2;
   {$EXTERNALSYM VO_NUM_RECENT_FRAMES}

   ASPECT_AUTO                      = 0;
   {$EXTERNALSYM ASPECT_AUTO}
   ASPECT_ANAMORPHIC                = 1;
   {$EXTERNALSYM ASPECT_ANAMORPHIC}
   ASPECT_FULL                      = 2;
   {$EXTERNALSYM ASPECT_FULL}
   ASPECT_DVB                       = 3;
   {$EXTERNALSYM ASPECT_DVB}
   ASPECT_SQUARE                    = 4;
   {$EXTERNALSYM ASPECT_SQUARE}
   NUM_ASPECT_RATIOS                = 5;
   {$EXTERNALSYM NUM_ASPECT_RATIOS}
   VO_TOP_FIELD                     = 1;
   {$EXTERNALSYM VO_TOP_FIELD}
   VO_BOTTOM_FIELD                  = 2;
   {$EXTERNALSYM VO_BOTTOM_FIELD}
   VO_BOTH_FIELDS                   = VO_TOP_FIELD;
   {$EXTERNALSYM VO_BOTH_FIELDS}
   VO_PREDICTION_FLAG               = 4;
   {$EXTERNALSYM VO_PREDICTION_FLAG}
   VO_CAP_COPIES_IMAGE              = $00000001;
   {$EXTERNALSYM VO_CAP_COPIES_IMAGE}
   VO_CAP_YV12                      = $00000002;
   {$EXTERNALSYM VO_CAP_YV12}
   VO_CAP_YUY2                      = $00000004;
   {$EXTERNALSYM VO_CAP_YUY2}
   VO_CAP_HUE                       = $00000010;
   {$EXTERNALSYM VO_CAP_HUE}
   VO_CAP_SATURATION                = $00000020;
   {$EXTERNALSYM VO_CAP_SATURATION}
   VO_CAP_BRIGHTNESS                = $00000040;
   {$EXTERNALSYM VO_CAP_BRIGHTNESS}
   VO_CAP_CONTRAST                  = $00000080;
   {$EXTERNALSYM VO_CAP_CONTRAST}
   VO_CAP_COLORKEY                  = $00000100;
   {$EXTERNALSYM VO_CAP_COLORKEY}
   VO_CAP_AUTOPAINT_COLORKEY        = $00000200;
   {$EXTERNALSYM VO_CAP_AUTOPAINT_COLORKEY}

   VIDEO_OUT_DRIVER_IFACE_VERSION   = 14;
   {$EXTERNALSYM VIDEO_OUT_DRIVER_IFACE_VERSION}
   MAX_OBJECTS                      = 50;
   {$EXTERNALSYM MAX_OBJECTS}
   MAX_EVENTS                       = 50;
   {$EXTERNALSYM MAX_EVENTS}
   MAX_SHOWING                      = 5;
   {$EXTERNALSYM MAX_SHOWING}

   OVERLAY_EVENT_NULL               = 0;
   {$EXTERNALSYM OVERLAY_EVENT_NULL}
   OVERLAY_EVENT_SHOW               = 1;
   {$EXTERNALSYM OVERLAY_EVENT_SHOW}
   OVERLAY_EVENT_HIDE               = 2;
   {$EXTERNALSYM OVERLAY_EVENT_HIDE}
   OVERLAY_EVENT_MENU_BUTTON        = 3;
   {$EXTERNALSYM OVERLAY_EVENT_MENU_BUTTON}
   OVERLAY_EVENT_FREE_HANDLE        = 8;
   {$EXTERNALSYM OVERLAY_EVENT_FREE_HANDLE}

   XINE_MAX_EVENT_LISTENERS         = 50;
   {$EXTERNALSYM XINE_MAX_EVENT_LISTENERS}
   XINE_MAX_EVENT_TYPES             = 100;
   {$EXTERNALSYM XINE_MAX_EVENT_TYPES}
   XINE_VERSION_CODE                = XINE_MAJOR_VERSION*10000+XINE_MINOR_VERSION*100+XINE_SUB_VERSION;
   {$EXTERNALSYM XINE_VERSION_CODE}
   XINE_LOG_MSG                     = 0;
   {$EXTERNALSYM XINE_LOG_MSG}
   XINE_LOG_PLUGIN                  = 1;
   {$EXTERNALSYM XINE_LOG_PLUGIN}
   XINE_LOG_NUM                     = 2;
   {$EXTERNALSYM XINE_LOG_NUM}
   XINE_STREAM_INFO_MAX             = 99;
   {$EXTERNALSYM XINE_STREAM_INFO_MAX}
   XINE_EVENT_PIDS_CHANGE           = $80000000;
   {$EXTERNALSYM XINE_EVENT_PIDS_CHANGE}
   PLUGIN_NONE                      = 0;
   {$EXTERNALSYM PLUGIN_NONE}
   PLUGIN_INPUT                     = 1;
   {$EXTERNALSYM PLUGIN_INPUT}
   PLUGIN_DEMUX                     = 2;
   {$EXTERNALSYM PLUGIN_DEMUX}
   PLUGIN_AUDIO_DECODER             = 3;
   {$EXTERNALSYM PLUGIN_AUDIO_DECODER}
   PLUGIN_VIDEO_DECODER             = 4;
   {$EXTERNALSYM PLUGIN_VIDEO_DECODER}
   PLUGIN_SPU_DECODER               = 5;
   {$EXTERNALSYM PLUGIN_SPU_DECODER}
   PLUGIN_AUDIO_OUT                 = 6;
   {$EXTERNALSYM PLUGIN_AUDIO_OUT}
   PLUGIN_VIDEO_OUT                 = 7;
   {$EXTERNALSYM PLUGIN_VIDEO_OUT}
   PLUGIN_POST                      = 8;
   {$EXTERNALSYM PLUGIN_POST}
   PLUGIN_MUST_PRELOAD              = 128;
   {$EXTERNALSYM PLUGIN_MUST_PRELOAD}
   PLUGIN_TYPE_MASK                 = 127;
   {$EXTERNALSYM PLUGIN_TYPE_MASK}
   MM_ACCEL_MLIB                    = $00000001;
   {$EXTERNALSYM MM_ACCEL_MLIB}
   MM_ACCEL_X86_MMX                 = $80000000;
   {$EXTERNALSYM MM_ACCEL_X86_MMX}
   MM_ACCEL_X86_3DNOW               = $40000000;
   {$EXTERNALSYM MM_ACCEL_X86_3DNOW}
   MM_ACCEL_X86_MMXEXT              = $20000000;
   {$EXTERNALSYM MM_ACCEL_X86_MMXEXT}
   MM_ACCEL_X86_SSE                 = $10000000;
   {$EXTERNALSYM MM_ACCEL_X86_SSE}
   MM_ACCEL_X86_SSE2                = $08000000;
   {$EXTERNALSYM MM_ACCEL_X86_SSE2}
   MM_ACCEL_PPC_ALTIVEC             = $04000000;
   {$EXTERNALSYM MM_ACCEL_PPC_ALTIVEC}
   MM_MMX                           = MM_ACCEL_X86_MMX;
   {$EXTERNALSYM MM_MMX}
   MM_3DNOW                         = MM_ACCEL_X86_3DNOW;
   {$EXTERNALSYM MM_3DNOW}
   MM_MMXEXT                        = MM_ACCEL_X86_MMXEXT;
   {$EXTERNALSYM MM_MMXEXT}
   MM_SSE                           = MM_ACCEL_X86_SSE;
   {$EXTERNALSYM MM_SSE}
   MM_SSE2                          = MM_ACCEL_X86_SSE2;
   {$EXTERNALSYM MM_SSE2}
   SCALEFACTOR                      = 65536;
   {$EXTERNALSYM SCALEFACTOR}
   CENTERSAMPLE                     = 128;
   {$EXTERNALSYM CENTERSAMPLE}
   T_ERROR                          = -1;
   {$EXTERNALSYM T_ERROR}
   T_EOF                            = 0;
   {$EXTERNALSYM T_EOF}
   T_EOL                            = 1;
   {$EXTERNALSYM T_EOL}
   T_SEPAR                          = 2;
   {$EXTERNALSYM T_SEPAR}
   T_M_START_1                      = 3;
   {$EXTERNALSYM T_M_START_1}
   T_M_START_2                      = 4;
   {$EXTERNALSYM T_M_START_2}
   T_M_STOP_1                       = 5;
   {$EXTERNALSYM T_M_STOP_1}
   T_M_STOP_2                       = 6;
   {$EXTERNALSYM T_M_STOP_2}
   T_EQUAL                          = 7;
   {$EXTERNALSYM T_EQUAL}
   T_QUOTE                          = 8;
   {$EXTERNALSYM T_QUOTE}
   T_STRING                         = 9;
   {$EXTERNALSYM T_STRING}
   T_IDENT                          = 10;
   {$EXTERNALSYM T_IDENT}
   T_DATA                           = 11;
   {$EXTERNALSYM T_DATA}
   T_C_START                        = 12;
   {$EXTERNALSYM T_C_START}
   T_C_STOP                         = 13;
   {$EXTERNALSYM T_C_STOP}
   T_TI_START                       = 14;
   {$EXTERNALSYM T_TI_START}
   T_TI_STOP                        = 15;
   {$EXTERNALSYM T_TI_STOP}
   T_DOCTYPE_START                  = 16;
   {$EXTERNALSYM T_DOCTYPE_START}
   T_DOCTYPE_STOP                   = 17;
   {$EXTERNALSYM T_DOCTYPE_STOP}
   XML_PARSER_CASE_INSENSITIVE      = 0;
   {$EXTERNALSYM XML_PARSER_CASE_INSENSITIVE}
   XML_PARSER_CASE_SENSITIVE        = 1;
   {$EXTERNALSYM XML_PARSER_CASE_SENSITIVE}
   XML_PARSER_OK                    = 0;
   {$EXTERNALSYM XML_PARSER_OK}
   XML_PARSER_ERROR                 = 1;
   {$EXTERNALSYM XML_PARSER_ERROR}
//{$ENDIF}

type

   { xine.h records and type definitions }
   uint8_t = cuint8;
   uint16_t = cuint16;
   uint32_t = cuint32;
   uint64_t = cuint64;

   int8_t = cint8;
   int16_t = cint16;
   int32_t = cInt32;
   int64_t = cInt64;
   pUint8_t = pcint8;

   PDisplay = Pointer;
   TDrawable = uint32_t;

   Puint32_t = ^uint32_t;
   Pxine_t = ^xine_s;
   Pxine_stream_t = ^xine_stream_s;
   Pxine_audio_port_t = ^xine_audio_port_s;
   Pxine_video_port_t = ^xine_video_port_s;
   PPxine_audio_port_t = ^Pxine_audio_port_t;
   PPxine_video_port_t = ^Pxine_video_port_t;
   Pxine_ao_driver_t = Pxine_audio_port_t;
   Pxine_vo_driver_t = Pxine_video_port_t;
   Pxine_video_frame_t = ^xine_video_frame_s;
   Pxine_audio_frame_t = ^xine_audio_frame_s;

   xine_video_frame_s = record
      vpts: int64_t;
      duration: int64_t;
      width, height: Integer;
      colorspace: Integer;
      aspect_ratio: Double;
      pos_stream: Integer;
      pos_time: Integer;
      data: Puint8_t;
      xine_frame: Pointer;
   end;
   xine_audio_frame_s = record
      vpts: int64_t;
      num_samples: Integer;
      sample_rate: Integer;
      num_channels: Integer;
      bits_per_sample: Integer;
      pos_stream: clong;
      pos_time: Integer;
      data: Puint8_t;
      xine_frame: Pointer;
   end;
   Pxine_post_t = ^xine_post_s;
   xine_post_s = record
      audio_input: ^Pxine_audio_port_t;
      video_input: ^Pxine_video_port_t;
      typ: Integer;
   end;
   xine_post_t = xine_post_s;

   Pxine_post_in_t = ^xine_post_in_s;
   Pxine_post_out_t = ^xine_post_out_s;
   xine_post_in_s = record
      name: PChar;
      typ: Integer;
      data: Pointer;
   end;
   xine_post_in_t = xine_post_in_s;
   xine_post_out_s = record
      name: PChar;
      typ: Integer;
      data: Pointer;
      rewire: function(self: Pxine_post_out_t; data: Pointer):Integer; cdecl;
   end;
   xine_post_out_t = xine_post_out_s;


   xine_log_cb_t = procedure (user_data:Pointer; section: Integer);
   xine_mrl_s = record
      origin: PChar;
      mrl: PChar;
      link: PChar;
      typ: uint32_t;
      size: clong;
   end;
   Pxine_mrl_t = ^xine_mrl_s;
   PPxine_mrl_t = ^Pxine_mrl_t;
   x11_rectangle_t = record
      x,y,w,h: Integer;
   end;
   x11_visual_s = record
      display : PDisplay;
      screen : Integer;
{$IFDEF FPC}
      d:TDrawable;
{$ELSE}
      d:Drawable;
{$ENDIF}
      user_data:Pointer;
      dest_size_cb : procedure(user_data:Pointer;
         video_width,video_height: Integer;
         video_pixel_aspect: Double;
         dest_width,dest_height:PInteger;
         dest_pixel_aspect: PDouble);
         cdecl;
      frame_output_cb : procedure (user_data:Pointer;
         video_width, video_height: Integer;
         video_pixel_aspect: Double;
         dest_x, dest_y, dest_width, dest_height: PInteger;
         dest_pixel_aspect: PDouble;
         win_x, win_y:PInteger);
         cdecl;
   end;
   x11_visual_t = x11_visual_s;
   fb_visual_s = record
      frame_output_cb : procedure (user_data:Pointer;
         video_width, video_height: Integer;
         video_pixel_aspect: Double;
         dest_x, dest_y, dest_width, dest_height: PInteger;
         dest_pixel_aspect: PDouble;
         win_x, win_y:PInteger);
         cdecl;
      user_data: Pointer;
   end;
   fb_visual_t = fb_visual_s;
   xine_health_check_s = record
      status : Integer;
      cdrom_dev, dvd_dev, msg, title, explanation : PChar;
   end;
   Pxine_health_check_t = ^xine_health_check_s;
   Pxine_cfg_entry_t = ^xine_cfg_entry_s;
   xine_config_cb_t = procedure (user_data:Pointer; entry: Pxine_cfg_entry_t);
      cdecl;
   xine_cfg_entry_s = record
      key: PChar;
      typ: Integer;
      unknown_value: PChar;
      str_value, str_default, str_sticky: PChar;
      num_value, num_default: Integer;
      range_min, range_max: Integer;
      enum_values: PPChar;
      description, help: PChar;
      exp_level: Integer;
      callback: xine_config_cb_t;
      callback_data: Pointer;
   end;

   xine_event_t = packed record
      stream: Pxine_stream_t;
      data: Pointer;
      data_length: Integer;
      typ: int32_t;
      tv: timeval;
   end;
   Pxine_event_t = ^xine_event_t;
   xine_input_data_t = record
      event: xine_event_t;
      button: uint8_t;
      x,y: uint16_t;
   end;
   Pxine_input_data_t = ^xine_input_data_t;
   xine_ui_data_t = record
      num_buttons: Integer;
      str_len: Integer;
      str: array[0..255] of Char;
   end;
   Pxine_ui_data_t = ^xine_ui_data_t;
   xine_ui_message_data_s = record
      compatibility: xine_ui_data_t;
      typ: Integer;
      explanation: Integer;
      num_parameters: Integer;
      parameters: Integer;
      message: array[0..0] of Char;
   end;
   Pxine_ui_message_data_t = ^xine_ui_message_data_s;
   xine_format_change_data_t = record
      width, height, aspect: Integer;
   end;
   Pxine_format_change_data_t = ^xine_format_change_data_t;
   xine_audio_level_data_t = record
      left, right: Integer;
   end;
   Pxine_audio_level_data_t = ^xine_audio_level_data_t;
   xine_progress_data_t = record
      description: PChar;
      percent: Integer;
   end;
   Pxine_progress_data_t = ^xine_progress_data_t;
   xine_mrl_reference_data_s = record
      alternative: Integer;
      mrl: array[0..0] of Char;
   end;
   Pxine_mrl_reference_data_t = ^xine_mrl_reference_data_s;
{$IFDEF XINE_ENABLE_EXPERIMENTAL_FEATURES}
   xine_set_v4l2_data_s = record
      inpt, channel, radio: Integer;
      frequency, transmission, framerate_numerator, framerate_denominator,
         framelines: uint32_t;
      stardard_id: uint64_t;
      colorstandard, colorsubcarrier: uint32_t;
      frame_width, frame_height: Integer;
      spare: array[0..19] of uint32_t;
      session_id: int32_t;
   end;
   Pxine_set_v4l2_data_t = ^xine_set_v4l2_data_s;
   xine_set_mpeg_data_s = record
      bitrate_vbr, bitrate_mean, bitrate_peak, gop_size, gop_closure,
         b_frames, aspect_ratio: Integer;
      spare: array[0..19] of uint32_t;
   end;
   Pxine_set_mpeg_data_t = ^xine_set_mpeg_data_s;
   xine_pvr_save_data_s = record
      mode, id: Integer;
      name: array[0..255] of Char;
   end;
   xine_pvr_realtime_s = record
      mode: Integer;
   end;
   xine_pvr_pause_s = record
      mode: Integer;
   end;
   Pxine_pvr_save_data_t = ^xine_pvr_save_data_s;
   Pxine_pvr_realtime_t = ^xine_pvr_realtime_s;
   Pxine_pvr_pause_t = ^xine_pvr_pause_s;
{$ENDIF}
   Pxine_event_queue_t = ^xine_event_queue_s;
   xine_event_listener_cb_t = procedure (user_data: Pointer;
      event: Pxine_event_t); cdecl;
   Pxine_osd_t = ^xine_osd_s;
   xine_tvsystem = ( XINE_TVSYSTEM_PAL, XINE_TVSYSTEM_NTSC );

   { non xine.h records and type definitions }

   Pxine_list_t = ^xine_list_s;
   Pmetronom_clock_t = ^metronom_clock_s;
   Pmetronom_t = ^metronom_s;
   Pplugin_catalog_t = ^plugin_catalog_s;
   Pscratch_buffer_t = ^scratch_buffer_s;
   Pinput_plugin_t = ^input_plugin_s;
   Pinput_class_t = ^input_class_s;
   Pdemux_plugin_t = ^demux_plugin_s;
   Pvo_driver_t = ^vo_driver_s;
   Pfifo_buffer_t = ^fifo_buffer_s;
   Pvideo_decoder_t = ^video_decoder_s;
   Plrb_t = ^lrb_s;
   Paudio_decoder_t = ^audio_decoder_s;
   Pspu_functions_t = ^spu_functions_s;
   Pspu_decoder_class_t = ^spu_decoder_class_s;
   Pspu_decoder_t = ^spu_decoder_s;
   Posd_renderer_t = ^osd_renderer_s;
   Pao_driver_t = ^ao_driver_s;
   Paudio_decoder_class_t = ^audio_decoder_s;
   Paudio_fifo_t = Pointer;
   Puint16_t = ^uint16_t;
   Paudio_buffer_t = ^audio_buffer_s;
   Paudio_driver_class_t = ^audio_driver_class_s;
   Pextra_info_t = ^extra_info_s;
   Pbuf_element_t = ^buf_element_s;
   Pxine_bmiheader = ^xine_bmiheader;
   Pxine_waveformatex = ^xine_waveformatex;
   Pcfg_entry_t = ^cfg_entry_s;
   Pconfig_values_t = ^config_values_s;
   Pdemux_class_t = ^demux_class_s;
   Pscr_plugin_t = ^scr_plugin_s;
   PPscr_plugin_t = ^Pscr_plugin_t;
   Posd_object_t = ^osd_object_s;
   Posd_font_t = Pointer;
   Posd_ft2context_t = Pointer;
   Pplugin_node_t = ^plugin_node_s;
   Ppost_class_t = ^post_class_s;
   Ppost_plugin_t = ^post_plugin_s;
   Ppost_video_port_t = ^post_video_port_s;
   Ppost_audio_port_t = ^post_audio_port_s;
   Pint16_t = ^int16_t;
   Pspu_info_t = ^spu_info_s;
   Pvideo_decoder_class_t = ^video_decoder_class_s;
   Pvo_frame_t = ^vo_frame_s;
   Pimg_buf_fifo_t = ^img_buf_fifo_s;
   Pvo_overlay_t = ^vo_overlay_s;
   Pvideo_overlay_instance_t = ^video_overlay_instance_s;
   Prle_elem_t = ^rle_elem_s;
   Pvo_button_t = ^vo_button_s;
   Pvideo_overlay_object_t = ^video_overlay_object_s;
   Pvideo_overlay_event_t = ^video_overlay_event_s;
   Pvo_scale_rect_t = ^vo_scale_rect_s;
   Pvo_scale_t = ^vo_scale_s;
   PPuint8_t = ^Puint8_t;
   Pvo_driver_class_t = ^vo_driver_class_s;
   Pplugin_info_t = ^plugin_info_s;
   Pint8_t = ^int8_t;
   Pxine_node_t = ^xine_node_s;
   Ppthread_mutex_t = ^pthread_mutex_t;
   Ppthread_mutexattr_t = ^pthread_mutexattr_t;
   Pxine_mutex_t = ^xine_mutex_t;
   Pxml_property_s = ^xml_property_s;
   Pxml_node_s = ^xml_node_s;
   PPxml_node_s = ^Pxml_node_s;
   Pyuv_planes_t = ^yuv_planes_s;

   xine_s = record
      config: Pconfig_values_t;
      plugin_catalog: Pplugin_catalog_t;
      demux_strategy:Integer;
      logbuffers: array[0..XINE_LOG_NUM-1] of Pscratch_buffer_t;
      verbosity: Integer;
      streams: Pxine_list_t;
      stream_lock: pthread_mutex_t;
      clock: Pmetronom_clock_t;
   end;


   audio_decoder_class_s = record
      open_plugin: function(this: Paudio_decoder_class_t;
         stream: Pxine_stream_t):Paudio_decoder_t; cdecl;
      get_identifier: function(this: Paudio_decoder_class_t):PChar; cdecl;
      get_desciption: function(this:Paudio_decoder_class_t):PChar; cdecl;
      dispose: procedure(this:Paudio_decoder_class_t); cdecl;
   end;
   audio_decoder_s = record
      decode_data: procedure(this: Paudio_decoder_t;
         buf: Pbuf_element_t); cdecl;
      reset: procedure(this: Paudio_decoder_t); cdecl;
      discontinuity: procedure(this: Paudio_decoder_t); cdecl;
      dispose: procedure(this: Paudio_decoder_t); cdecl;
      node: Pointer;
   end;
   audio_buffer_s = record
      next: Paudio_buffer_t;
      mem: ^int16_t;
      mem_size, num_frames: Integer;
      vpts: int64_t;
      frame_header_count, frame_access_unit: uint32_t;
      extra_info: Pextra_info_t;
      stream: Pxine_stream_t;
   end;
   ao_format_s = record
      bits, rate : uint32_t;
      mode: Integer;
   end;
   ao_format_t = ao_format_s;
   ao_driver_s = record
      get_capabilities : function (this:Pao_driver_t):uint32_t; cdecl;
      open : function(this:Pao_driver_t; bits,rate:uint32_t;mode:Integer):Integer;
         cdecl;
      num_channels : function (self_gen:Pao_driver_t):Integer; cdecl;
      bytes_per_frame: function (self_gen:Pao_driver_t):Integer; cdecl;
      delay: function (self_gen:Pao_driver_t):Integer; cdecl;
      get_gap_tolerance: function  (self_gen:Pao_driver_t):Integer; cdecl;
      write: function (this:Pao_driver_t; audio_data:Puint16_t;
         num_samples: uint32_t):Integer; cdecl;
      close: procedure (this:Pao_driver_t); cdecl;
      exit: procedure (this:Pao_driver_t); cdecl;
      get_property: function (this:Pao_driver_t; prop:Integer):Integer;  cdecl;
      set_property: function (this:Pao_driver_t; prop,value:Integer):Integer;
         cdecl;
      control: function (this:Pao_driver_t; cmd: Integer):Integer; cdecl;
      node: Pointer;
   end;
   xine_audio_port_s = record
      get_capabilities: function(this: Pxine_audio_port_t):uint32_t; cdecl;
      get_property: function(this:Pxine_audio_port_t;
         prop: Integer):Integer; cdecl;
      set_property: function(this:Pxine_audio_port_t;
         prop, value: Integer):Integer; cdecl;
      open: function(this: Pxine_audio_port_t; stream: Pxine_stream_t;
         bits, rate: uint32_t; mode: Integer):Integer; cdecl;
      get_buffer: function(this: Pxine_audio_port_t):Paudio_buffer_t; cdecl;
      put_buffer: procedure(this: Pxine_audio_port_t;
         buf: Paudio_buffer_t; stream: Pxine_stream_t);cdecl;
      close: procedure(this: Pxine_audio_port_t; stream: Pxine_stream_t);cdecl;
      exit: procedure(this: Pxine_audio_port_t);cdecl;
      control: function(this:Pxine_audio_port_t; cmd: Integer):Integer; cdecl;
      flush: procedure(this: Pxine_audio_port_t);cdecl;
      driver: Pao_driver_t;
      driver_lock: pthread_mutex_t;
      clock: Pmetronom_clock_t;
      xine: Pxine_t;
      streams: Pxine_list_t;
      streams_lock: pthread_mutex_t;
      audio_loop_running, audio_paused: Integer;
      audio_thread: pthread_t;
      audio_step: Integer;
      frames_per_kpts: int32_t;
      input, output: ao_format_t;
      frame_rate_factor: Double;
      resample_conf, force_rate, do_resample, gap_tolerance: Integer;
      free_fifo, out_fifo: Paudio_fifo_t;
      last_audio_vpts: int64_t;
      frame_buf: array[0..1] of Paudio_buffer_t;
      zero_space: int16_t;
      passthrough_offset: int64_t;
      flush_audio_driver, allow_full_ao_fill_gap, do_compress: Integer;
      compression_factor, compression_factor_max: Double;
   end;
   audio_driver_class_s = record
     open_plugin: function(this: Paudio_driver_class_t;
        data: Pointer): Pao_driver_t;cdecl;
     get_identifier: function(this: Paudio_driver_class_t):PChar; cdecl;
     get_description: function(this: Paudio_driver_class_t):PChar; cdecl;
     dispose: procedure(this: Paudio_driver_class_t); cdecl;
   end;

   img_buf_fifo_s = record
      first: Pvo_frame_t;
      last: Pvo_frame_t;
      num_buffers, locked_for_read: Integer;
      mutex: pthread_mutex_t;
      not_empty: pthread_cond_t;
   end;
   vo_frame_s = record
      free: procedure(vo_img: Pvo_frame_t);cdecl;
      copy: procedure(vo_img: Pvo_frame_t; src: PPuint8_t );cdecl;
      field: procedure(vo_img: Pvo_frame_t; which_field:Integer); cdecl;
      draw: function(vo_img: Pvo_frame_t; stream: Pxine_stream_t):Integer;
         cdecl;
      displayed: procedure(vo_img: Pvo_frame_t);cdecl;
      dispose: procedure(vo_img: Pvo_frame_t);cdecl;
      pts, vpts: int64_t;
      bad_frame, duration: Integer;
      base: array[0..2] of Puint8_t;
      pitches: array[0..2] of Integer;
      top_field_first, repeat_first_field: Integer;
      pan_scan_x, pan_scan_y: Integer;
      width, height, ratio, format, drawn, copy_called: Integer;
      port: Pxine_video_port_t;
      driver: Pvo_driver_t;
      next: Pvo_frame_t;
      lock_counter: Integer;
      mutex: pthread_mutex_t;
      id, is_first: Integer;
   end;
   xine_video_port_s = record
      get_capabilities: function(this: Pxine_video_port_t):uint32_t; cdecl;
      open: function(this: Pxine_video_port_t; stream: Pxine_stream_t;
         bits, rate: uint32_t; mode: Integer):Integer; cdecl;
      get_frame: function(this: Pxine_video_port_t; width, height: uint32_t;
         ratio_code, format, flags: Integer):Pvo_frame_t; cdecl;
      get_last_frame: function(this: Pxine_video_port_t):Pvo_frame_t; cdecl;
      enable_ovl: procedure(this: Pxine_video_port_t; ovl_enable: Integer);
         cdecl;
      close: procedure(this: Pxine_video_port_t; stream: Pxine_stream_t);cdecl;
      exit: procedure(this: Pxine_video_port_t);cdecl;
      get_overlay_instance: function(this: Pxine_video_port_t):
         Pvideo_overlay_instance_t; cdecl;
      flush: procedure(this: Pxine_video_port_t);cdecl;
      driver: Pvo_driver_t;
   end;
   vo_driver_s = record
      get_capabilities : function(self : Pvo_driver_t) : uint32_t; cdecl;
      alloc_frame : function(self :Pvo_driver_t): Pvo_frame_t; cdecl;
      update_frame_format : procedure (self : Pvo_driver_t; img : Pvo_frame_t;
         width, height :uint32_t; ratio_code, format, flags : Integer); cdecl;
      display_frame : procedure (self : Pvo_driver_t; img : Pvo_frame_t); cdecl;
      overlay_begin: procedure (self : Pvo_driver_t; img : Pvo_frame_t;
         changed: Integer); cdecl;
      overlay_blend : procedure (self : Pvo_driver_t; img : Pvo_frame_t;
         overlay : Pvo_overlay_t); cdecl;
      overlay_end: procedure (self : Pvo_driver_t;
         img : Pvo_frame_t); cdecl;
      get_property : function (self : Pvo_driver_t; prop : Integer) : Integer;
         cdecl;
      set_property : function (self : Pvo_driver_t;
         prop, value : Integer) : Integer; cdecl;
      get_property_min_max : function (self : Pvo_driver_t; prop: Integer;
         min,max :PInteger) : Integer; cdecl;
      gui_data_exchange : function (self : Pvo_driver_t; data_type: Integer;
         data : Pointer):Integer; cdecl;
      redraw_needed : function (self : Pvo_driver_t): Integer; cdecl;
      dispose : procedure (self : Pvo_driver_t); cdecl;
      node: Pointer;
   end;
   vo_driver_class_s = record
      open_plugin: function(this: Pvo_driver_class_t;
         visual: Pointer):Pvo_driver_class_t; cdecl;
      get_identifier: function(this: Pvo_driver_class_t):PChar; cdecl;
      get_desciption: function(this:Pvo_driver_class_t):PChar; cdecl;
      dispose: procedure(this:Pvo_driver_class_t); cdecl;
   end;
   rle_elem_s = record
      len, color: uint16_t;
   end;
   vo_overlay_s = record
      rle: Prle_elem_t;
      data_size, num_rle, x, y, width, height: Integer;
      color: array[0..OVL_PALETTE_SIZE-1] of uint32_t;
      trans: array[0..OVL_PALETTE_SIZE-1] of uint8_t;
      rgb_clut, clip_top, clip_bottom, clip_left, clip_right: Integer;
      clip_color: array[0..OVL_PALETTE_SIZE-1] of uint32_t;
      clip_trans: array[0..OVL_PALETTE_SIZE-1] of uint8_t;
      clip_rgb_clut: Integer;
   end;
   video_overlay_instance_s = record
      init: procedure(this_gen: Pvideo_overlay_instance_t);cdecl;
      dispose: procedure(this_gen: Pvideo_overlay_instance_t);cdecl;
      get_handle: function(this_gen: Pvideo_overlay_instance_t;
         object_type:Integer):int32_t; cdecl;
      free_handle: procedure(this_gen: Pvideo_overlay_instance_t;
         handle: int32_t); cdecl;
      add_event: function(this_gen: Pvideo_overlay_instance_t;
         event: Pointer):int32_t; cdecl;
      flush_events: procedure(this_gen: Pvideo_overlay_instance_t);cdecl;
      redraw_needed: function(this_gen: Pvideo_overlay_instance_t;
         vpts: int64_t):Integer; cdecl;
      multiple_overlay_blend: procedure(this_gen: Pvideo_overlay_instance_t;
         vpts: int64_t; output: Pvo_driver_t; vo_img: Pvo_frame_t;
         enabled: Integer); cdecl;
   end;

   buf_element_s = record
      next: Pbuf_element_t;
      mem, content: PChar;
      size, max_size: int32_t;
      typ: uint32_t;
      pts, disc_off: int64_t;
      input_pos, input_length: off_t;
      input_time: Integer;
      decoder_flags: uint32_t;
      decoder_info: array[0..3] of uint32_t;
      free_buffer: procedure( buf: Pbuf_element_t); cdecl;
      source: Pointer;
   end;
   spu_dvb_descriptor = record
      lang: array[0..3] of Char;
      comp_page_id: Integer;
      aux_page_id: Integer;
   end;
   palette_entry_s = record
      r,g,b: uint8_t;
   end;
   Tfifo_buffer_cb_t = procedure(fifo: Pfifo_buffer_t; buf: Pbuf_element_t;
         data_cb: Pointer);cdecl;
   fifo_buffer_s = record
      first, last: Pbuf_element_t;
      fifo_size: Integer;
      mutex: pthread_mutex_t;
      not_empty: pthread_cond_t;
      put: procedure ( fifo: Pfifo_buffer_t; buf: Pbuf_element_t); cdecl;
      get: function (fifo: Pfifo_buffer_t):Pbuf_element_t; cdecl;
      clear: procedure (fifo: Pfifo_buffer_t); cdecl;
      size: function (fifo: Pfifo_buffer_t):Integer; cdecl;
      dispose: procedure (fifo: Pfifo_buffer_t); cdecl;
      buffer_pool_alloc: function (this: Pfifo_buffer_t):Pbuf_element_t; cdecl;
      buffer_pool_try_alloc:function(this:Pfifo_buffer_t):Pbuf_element_t;cdecl;
      insert: procedure(fifo: Pfifo_buffer_t; buf: Pbuf_element_t); cdecl;
      register_put_cb: procedure(fifo: Pfifo_buffer_t; cb: Tfifo_buffer_cb_t;
        cb_data: Pointer); cdecl;
      register_get_cb: procedure(fifo: Pfifo_buffer_t; cb: Tfifo_buffer_cb_t;
        cb_data: Pointer); cdecl;
      buffer_pool_top: Pbuf_element_t;
      buffer_pool_mutex: pthread_mutex_t;
      buffer_pool_cond_not_empty: pthread_cond_t;
      buffer_pool_num_empty: Integer;
      buffer_pool_capacity: Integer;
      buffer_pool_buf_size: Integer;
      buffer_pool_base: Pointer;
      put_cb: Tfifo_buffer_cb_t;
      get_cb: Tfifo_buffer_cb_t;
      put_cb_data: Pointer;
      get_cb_data: Pointer;
   end;
   xine_bmiheader = record
      biSize: int32_t;
      biWidth: int32_t;
      biHeight: int32_t;
      biPlanes: int16_t;
      biBitCount: int16_t;
      biCompression: uint32_t;
      biSizeImage: int32_t;
      biXPelsPerMeter: int32_t;
      biYPelsPerMeter: int32_t;
      biClrUsed: int32_t;
      biClrImportant: int32_t;
   end;
   xine_waveformatex = record
      wFormatTag: int16_t;
      nChannels: int16_t;
      nSamplesPerSec: int32_t;
      nAvgBytesPerSec: int32_t;
      nBlockAlign: int32_t;
      wBitsPerSample: int16_t;
      cbSize:int16_t;
   end;

   cfg_entry_s = record
      next : Pcfg_entry_t;
      config : Pconfig_values_t;
      key : PChar;
      typ : Integer;
      unknown_value : Pchar;
      str_value : PChar;
      str_default : Pchar;
      str_sticky : PChar;
      num_value : Integer;
      num_default : Integer;
      range_min : Integer;
      range_max : Integer;
      enum_values : PPChar;
      description : PChar;
      help : PChar;
      exp_level: Integer;
      callback : xine_config_cb_t;
      callback_data : Pointer;
   end;

   config_values_s = record
         register_string : function(self : Pconfig_values_t;
            key,def_value,description,help: Pchar;
            exp_level: Integer; changed_cb :xine_config_cb_t;
            cb_data: Pointer): PChar; cdecl;
         register_range : function(self : Pconfig_values_t;
            key :PChar; def_value, min, max : Integer; description, help : PChar;
            exp_level: Integer; changed_cb :xine_config_cb_t;
            cb_data: Pointer):Integer; cdecl;
         register_enum : function (self : Pconfig_values_t;
            key :PChar; def_value: Integer; values : PPChar;
            description, help:PChar; exp_level: Integer;
            changed_cb :xine_config_cb_t; cb_data: Pointer):Integer; cdecl;
         register_num : function (self : Pconfig_values_t;
            key :PChar; def_value: Integer; description, help :PChar;
            exp_level: Integer; changed_cb :xine_config_cb_t;
            cb_data: Pointer):Integer; cdecl;
         register_bool :function (self : Pconfig_values_t;
            key :PChar; def_value: Integer; description, help :PChar;
            exp_level: Integer; changed_cb :xine_config_cb_t;
            cb_data: Pointer):Integer; cdecl;
         update_num :procedure(self : Pconfig_values_t; key: Pchar; value: Integer);
            cdecl;
         update_string :procedure (self : Pconfig_values_t; key, value: Pchar);
            cdecl;
         parse_enum : function (str :Pchar; values: PPchar): Integer; cdecl;
         lookup_entry : function (self : Pconfig_values_t;key:Pchar): Pcfg_entry_t;
            cdecl;
         unregister_callback: procedure (self : Pconfig_values_t; key : PChar);
            cdecl;
         dispose: procedure (self : Pconfig_values_t); cdecl;
         first, last, curr : Pcfg_entry_t;
         config_lock: pthread_mutex_t;
      end;

   demux_class_s = record
      open_plugin: function ( this: Pdemux_class_t; stream: Pxine_stream_t;
         input: Pinput_plugin_t): Pdemux_plugin_t; cdecl;
      get_description: function( this: Pdemux_class_t): PChar; cdecl;
      get_identifier: function( this: Pdemux_class_t): PChar; cdecl;
      get_mimetypes: function( this: Pdemux_class_t): PChar; cdecl;
      get_extensions: function( this: Pdemux_class_t): PChar; cdecl;
      dispose: procedure( this: Pdemux_class_t ); cdecl;
   end;
   demux_plugin_s = record
      send_headers: procedure ( this : Pdemux_plugin_t); cdecl;
      seek: function ( this: Pdemux_plugin_t; start_pos: off_t;
         start_time: Integer):Integer; cdecl;
      send_chunk: function ( this: Pdemux_plugin_t ): Integer; cdecl;
      dispose: procedure ( this: Pdemux_plugin_t ); cdecl;
      get_status: function ( this: Pdemux_plugin_t): Integer; cdecl;
      get_stream_length: function (this: Pdemux_plugin_t): Integer; cdecl;
      get_video_frame: function ( this: Pdemux_plugin_t; timestamp: Integer;
         width, height, ratio_code, duration, format: PInteger;
         img: Puint8_t):Integer; cdecl;
      got_video_frame_cb: procedure ( this: Pdemux_plugin_t;
         frame: Pvo_frame_t); cdecl;
      get_capabilities: function(this: Pdemux_plugin_t):uint32_t; cdecl;
      get_optional_data: function(this: Pdemux_plugin_t; data: Pointer;
         data_type: Integer):Integer; cdecl;
      demux_class: Pdemux_class_t;
   end;

   input_class_s = record
      open_plugin: function ( this: Pinput_class_t; stream: Pxine_stream_t;
         mrl: PChar): Pinput_plugin_t; cdecl;
      get_identifier: function( this: Pinput_class_t): PChar; cdecl;
      get_description: function( this: Pinput_class_t): PChar; cdecl;
      get_dir: function( this: Pinput_class_t; filename: PChar;
         nFiles: PInteger): PPxine_mrl_t; cdecl;
      get_autoplay_list: function( this: Pinput_class_t;
         num_files: Integer): PPChar; cdecl;
      dispose: procedure( this: Pinput_class_t ); cdecl;
      eject_media: function( this: Pinput_class_t ):Integer; cdecl;
   end;
   input_plugin_s = record
      get_capabilities: function(this: Pinput_plugin_t):uint32_t; cdecl;
      read: function(this: Pinput_plugin_t; buf: PChar; nlen: off_t):off_t;
         cdecl;
      read_plugin: function(this: Pinput_plugin_t; fifo: Pfifo_buffer_t;
         len: off_t): Pbuf_element_t; cdecl;
      seek: function(this: Pinput_plugin_t; offset: off_t;
         origin: Integer):off_t; cdecl;
      get_current_pos: function(this: Pinput_plugin_t):off_t; cdecl;
      get_length: function(this: Pinput_plugin_t):off_t; cdecl;
      get_blocksize: function(this: Pinput_plugin_t):uint32_t; cdecl;
      get_mrl: function(this: Pinput_plugin_t):PChar; cdecl;
      get_optional_data: function(this: Pinput_plugin_t; data: Pointer;
         data_type: Integer):Integer; cdecl;
      dispose: procedure(this: Pinput_plugin_t); cdecl;
      input_class: Pinput_class_t;
   end;

   lrb_s = record
     max_num_entries, cur_num_entries: Integer;
     newest, oldest: Pbuf_element_t;
     fifo: Pfifo_buffer_t;
   end;

   metronom_s = record
      set_audio_rate: procedure(this: Pmetronom_t; pts_per_sample: int64_t);
         cdecl;
      got_video_frame: procedure(this: Pmetronom_t; frame: Pvo_frame_t);
         cdecl;
      got_audio_samples: function(this: Pmetronom_t; pts: int64_t;
         nsamples: Integer):int64_t; cdecl;
      got_spu_packet: function(this: Pmetronom_t; pts: int64_t):int64_t;
         cdecl;
      handle_audio_discontinuity: procedure(this: Pmetronom_t; typ: Integer;
         disc_off: int64_t); cdecl;
      handle_video_discontinuity: procedure(this: Pmetronom_t; typ: Integer;
         disc_off: int64_t); cdecl;
      set_option: procedure(this: Pmetronom_t; option: Integer;
         value: int64_t); cdecl;
      get_option: function(this: Pmetronom_t; option: Integer):int64_t; cdecl;
      exit: procedure(this: Pmetronom_t); cdecl;
      stream: Pxine_stream_t;
      clock: Pmetronom_clock_t;
      pts_per_sample, video_vpts, spu_vpts, audio_vpts, vpts_offset,
         video_drift, video_drift_step: int64_t;
      audio_samples: Integer;
      audio_drift_step, av_offset: int64_t;
      lock: pthread_mutex_t;
      have_audio, video_discontinuity_count, audio_discontinuity_count,
         discontinuity_handled_count: Integer;
      video_discontinuity_reached, audio_discontinuity_reached,
         cancel: pthread_cond_t;
      force_video_jump, force_audio_jump: Integer;
      img_duration: int64_t;
      img_cpt: Integer;
      last_video_pts: int64_t;
   end;
   metronom_clock_s = record
      set_option: procedure(this: Pmetronom_clock_t; option: Integer;
         value: int64_t); cdecl;
      get_option: function(this: Pmetronom_clock_t;
         option: Integer):int64_t; cdecl;
      start_clock: procedure(this: Pmetronom_clock_t; pts: int64_t); cdecl;
      stop_clock: procedure(this: Pmetronom_clock_t); cdecl;
      resume_clock: procedure(this: Pmetronom_clock_t); cdecl;
      get_current_time: function(this: Pmetronom_clock_t):int64_t; cdecl;
      adjust_clock: procedure(this: Pmetronom_clock_t;
         desired_pts: int64_t); cdecl;
      set_speed: function(this: Pmetronom_clock_t; speed: Integer):Integer;
         cdecl;
      register_scr: function(this: Pmetronom_clock_t;
         scr: Pscr_plugin_t): Integer; cdecl;
      unregister_scr: procedure(this: Pmetronom_clock_t; scr: Pscr_plugin_t);
         cdecl;
      exit: procedure(this: Pmetronom_clock_t); cdecl;
      scr_master: Pscr_plugin_t;
      scr_list: PPscr_plugin_t;
      sync_thread: pthread_t;
      thread_running, scr_adjustable: Integer;
      speed: Integer;
      lock: pthread_mutex_t;
      cancel: pthread_cond_t;
   end;
   scr_plugin_s = record
      inteface_version: Integer;
      get_priority: function(this: Pscr_plugin_t):Integer; cdecl;
      set_speed: function(this: Pscr_plugin_t; speed: Integer):Integer;
         cdecl;
      adjust: procedure(this: Pscr_plugin_t; vpts: int64_t); cdecl;
      start: procedure(this: Pscr_plugin_t; start_vpts: int64_t); cdecl;
      get_current: function(this: Pscr_plugin_t):int64_t; cdecl;
      exit: procedure(this: Pscr_plugin_t); cdecl;
      clock: Pmetronom_clock_t;
   end;

   plugin_node_s = record
      filename: PChar;
      info: Pplugin_info_t;
      plugin_class: Pointer;
      ref: Integer;
      filesize: off_t;
      filemtime: time_t;
   end;
   plugin_catalog_s = record
      input, demux, spu, audio, video, aout, vout, post, cache: Pxine_list_t;
      audio_decoder_map : array[0..DECODER_MAX-1,0..PLUGINS_PER_TYPE-1] of
         Pplugin_node_t;
      video_decoder_map : array[0..DECODER_MAX-1,0..PLUGINS_PER_TYPE-1] of
         Pplugin_node_t;
      spu_decoder_map : array[0..DECODER_MAX-1,0..PLUGINS_PER_TYPE-1] of
         Pplugin_node_t;
      ids: array[0..PLUGIN_MAX-1] of PChar;
      lock: pthread_mutex_t;
   end;

   post_class_s = record
      open_plugin: function(this: Ppost_class_t; inputs: Integer;
         audio_target: PPxine_audio_port_t;
         video_target: PPxine_video_port_t):Ppost_plugin_t; cdecl;
      get_identifier: function(this: Ppost_class_t):PChar; cdecl;
      get_description: function(this: Ppost_class_t):PChar; cdecl;
      dispose: procedure(this: Ppost_class_t); cdecl;
   end;
   post_plugin_s = record
      xine_post: xine_post_s;
      input, output: Pxine_list_t;
      dispose: procedure(this: Ppost_plugin_t); cdecl;
      input_ids, output_ids: PPChar;
      node: Pointer;
   end;
   post_video_port_s = record
      port: xine_video_port_s;
      original_port: Pxine_video_port_t;
      frame: vo_frame_s;
      post: Ppost_plugin_t;
   end;
   post_audio_port_s = record
      port: xine_audio_port_s;
      original_port: Pxine_audio_port_t;
      post: Ppost_plugin_t;
   end;

   scratch_buffer_s = record
      scratch_printf: procedure(this: Pscratch_buffer_t; format: PChar;
         ap: Pointer); cdecl;
      get_content: function(this: Pscratch_buffer_t):PPChar; cdecl;
      dispose: procedure(this: Pscratch_buffer_t); cdecl;
      lines, ordered: PPChar;
      num_lines, cur: Integer;
   end;

   spu_decoder_class_s = record
      open_plugin: function(this: Pspu_decoder_class_t;
         stream: Pxine_stream_t):Pspu_decoder_t; cdecl;
      get_identifier: function(this: Pspu_decoder_class_t):PChar; cdecl;
      get_description: function(this: Pspu_decoder_class_t):PChar; cdecl;
      dispose: procedure(this: Pspu_decoder_class_t); cdecl;
   end;
   spu_decoder_s = record
      decode_data: procedure ( this: Pspu_decoder_t;
         buf: Pbuf_element_t); cdecl;
      reset: procedure ( this: Pspu_decoder_t); cdecl;
      discontinuity: procedure ( this: Pspu_decoder_t); cdecl;
      dispose: procedure ( this: Pspu_decoder_t); cdecl;
      get_interact_info: function (this: Pspu_decoder_t;
         data: Pointer): Integer; cdecl;
      set_button: procedure (this: Pspu_decoder_t;
         button, mode: int32_t); cdecl;
      node: Pointer;
   end;

   spu_functions_s = record
      get_capabilities: function(this: Pspu_functions_t):uint32_t;cdecl;
      connect: procedure (this: Pspu_functions_t; metronom: Pmetronom_t); cdecl;
      open: function ( this: Pspu_functions_t; bits, rate: uint32_t;
         mode: Integer): Integer; cdecl;
      write_spu_data: procedure (this: Pspu_functions_t; spu_data: Pint16_t;
         num_samples: uint32_t; pts: int64_t); cdecl;
      close: procedure (this: Pspu_functions_t); cdecl;
      exit: procedure (this: Pspu_functions_t); cdecl;
      get_property : function (self : Pspu_functions_t;
         prop : Integer) : Integer; cdecl;
      set_property : function (self : Pspu_functions_t;
         prop, value : Integer) : Integer; cdecl;
   end;
   spu_info_s = record
      interface_version: Integer;
      id: PChar;
      description: PChar;
      priority: Integer;
   end;

   video_decoder_class_s = record
      open_plugin: function(this: Pvideo_decoder_class_t;
         stream: Pxine_stream_t):Pvideo_decoder_t; cdecl;
      get_identifier: function(this: Pvideo_decoder_class_t):PChar; cdecl;
      get_desciption: function(this:Pvideo_decoder_class_t):PChar; cdecl;
      dispose: procedure(this:Pvideo_decoder_class_t); cdecl;
   end;
   video_decoder_s = record
      decode_data: procedure ( this: Pvideo_decoder_t;
         buf: Pbuf_element_t); cdecl;
      reset: procedure ( this: Pvideo_decoder_t); cdecl;
      discontinuity: procedure ( this: Pvideo_decoder_t); cdecl;
      flush: procedure ( this: Pvideo_decoder_t); cdecl;
      dispose: procedure ( this: Pvideo_decoder_t); cdecl;
      node: Pointer;
   end;

   vo_button_s = record
      typ, clip_top, clip_bottom, clip_left, clip_right, up, down, left,
         right: int32_t;
      select_color: array[0..OVL_PALETTE_SIZE] of uint32_t;
      select_trans: array[0..OVL_PALETTE_SIZE] of uint8_t;
      select_event: xine_event_t;
      active_color: array[0..OVL_PALETTE_SIZE] of uint32_t;
      active_trans: array[0..OVL_PALETTE_SIZE] of uint8_t;
      active_event: xine_event_t;
      clip_rgb_clut: int32_t;
   end;
   video_overlay_object_s = record
      handle: int32_t;
      object_type: uint32_t;
      pts: int64_t;
      overlay: Pvo_overlay_t;
      plette_type: uint32_t;
      palette: Puint32_t;
      buttonN: int32_t;
      button: array[0..31] of vo_button_s;
   end;
   video_overlay_event_s = record
      event_type: uint32_t;
      vpts: int64_t;
      vo_object: video_overlay_object_s;
   end;

   osd_object_s = record
      next: Posd_object_t;
      renderer: Posd_renderer_t;
      width, height: Integer;
      area: Puint8_t;
      display_x, display_y: Integer;
      x1, y1, x2, y2 : Integer;
      color: array[0..OVL_PALETTE_SIZE-1] of uint32_t;
      trans: array[0..OVL_PALETTE_SIZE-1] of uint8_t;
      handle: int32_t;
      font: Posd_font_t;
   end;
   xine_osd_s = record
      osd: osd_object_s;
   end;
   osd_renderer_s = record
      new_object : function (self:Posd_renderer_t;
         width,height:Integer):Posd_object_t; cdecl;
      free_object : procedure (osd_to_close:Posd_object_t); cdecl;
      show : function (osd:Posd_object_t;vpts:int64_t ):Integer; cdecl;
      hide : function (osd:Posd_object_t;vpts:int64_t ):Integer; cdecl;
      line : procedure (osd:Posd_object_t;x1,y1,x2,y2,color: Integer );cdecl;
      filled_rect : procedure (osd:Posd_object_t;x1,y1,x2,y2,color: Integer  );
         cdecl;
      set_palette : procedure (osd:Posd_object_t; color: Puint32_t;
         trans: Puint8_t );cdecl;
      set_text_palette : procedure (osd: Posd_object_t;
         palette_number,color_base : Integer );cdecl;
      get_palette : procedure (osd:Posd_object_t; color: Puint32_t;
         trans: Puint8_t ); cdecl;
      set_position : procedure (osd:Posd_object_t; x,y: Integer); cdecl;
      set_font : function (osd:Posd_object_t; fontname: PChar;
         size: Integer):Integer; cdecl;
      render_text : function (osd:Posd_object_t; x1, y1: Integer;
         text: PChar; color_base: Integer):Integer; cdecl;
      get_text_size : function (osd:Posd_object_t; text:Pchar;
         width, height: PInteger):Integer;  cdecl;
      close : procedure (self:Posd_renderer_t); cdecl;
      clear : procedure (osd:Posd_object_t ); cdecl;
      draw_bitmap: procedure (osd: Posd_object_t; bitmap: Puint8_t;
         x1, y1, width, height: Integer; palette_map: Puint8_t); cdecl;
      osd_mutex: pthread_mutex_t;
      video_overlay: Pvideo_overlay_instance_t;
      event: video_overlay_event_s;
      osds: Posd_object_t;
      fonts: Posd_font_t;
      textpalette: Integer;
      config: Pconfig_values_t;
   end;

   vo_scale_rect_s = record
      x,y,w,h: Integer;
   end;
   vo_scale_s = record
      support_zoom, scaling_disabled, delivered_width, delivered_height,
         delivered_ratio_code, displayed_xoffset, displayed_yoffset,
         displayed_width, displayed_height: Integer;
      zoom_factor_x, zoom_factor_y : Double;
      user_ratio, gui_x, gui_y, gui_width, gui_height, gui_win_x,
         gui_win_y: Integer;
      gui_pixel_aspect, video_pixel_aspect: Double;
      output_width, output_height, output_xoffset, output_yoffset,
         force_redraw: Integer;
      user_data: Pointer;
      frame_output_cb : procedure (user_data:Pointer;
         video_width, video_height: Integer;
         video_pixel_aspect: Double;
         dest_x, dest_y, dest_width, dest_height: PInteger;
         dest_pixel_aspect: PDouble;
         win_x, win_y:PInteger);
         cdecl;
      dest_size_cb : procedure(user_data:Pointer;
         video_width,video_height: Integer;
         video_pixel_aspect: Double;
         dest_width,dest_height:PInteger;
         dest_pixel_aspect: PDouble);
         cdecl;
      border: array[0..3] of vo_scale_rect_s;
      output_horizontal_position, output_vertical_position: Double;
   end;

   extra_info_s = record
      input_pos, input_length: off_t;
      input_time: Integer;
      frame_number: uint32_t;
      seek_count: Integer;
      vpts: int64_t;
      invalid, total_time: Integer;
   end;
   xine_event_queue_s = record
      events: Pxine_list_t;
      lock: pthread_mutex_t;
      new_event: pthread_cond_t;
      stream: Pxine_stream_t;
      listener_thread: ^pthread_t;
      callback: xine_event_listener_cb_t;
      user_data: Pointer;
   end;
   xine_stream_s = record
      xine: Pxine_t;
      status: Integer;
      input_plugin: Pinput_plugin_t;
      eject_class: Pinput_class_t;
      content_detection_method: Integer;
      demux_plugin: Pdemux_plugin_t;
      metronom: Pmetronom_t;
      input_pos, input_length: off_t;
      input_time: Integer;
      video_out: Pxine_video_port_t;
      video_driver: Pvo_driver_t;
      video_fifo: Pfifo_buffer_t;
      video_thread: pthread_t;
      video_decoder_plugin: Pvideo_decoder_t;
      video_decoder_streamtype, video_in_discontinuity, video_channel: Integer;
      audio_out: Pxine_audio_port_t;
      audio_fifo: Pfifo_buffer_t;
      audio_temp: Plrb_t;
      audio_thread: pthread_t;
      audio_decoder_plugin: Paudio_decoder_t;
      audio_decoder_stream_type: Integer;
      audio_track_map: array[0..49] of uint32_t;
      audio_track_map_entries: Integer;
      audio_type: uint32_t;
      audio_channel_user, audio_channel_auto: Integer;
      spu_out: Pspu_functions_t;
      spu_thread: pthread_t;
      spu_decoder_plugin: Pspu_decoder_t;
      spu_decoder_streamtype, spu_channel_user, spu_channel_auto:Integer;
      spu_channel_letterbox, spu_channel_pan_scan, spu_channel:Integer;
      frontend_lock: pthread_mutex_t;
      osd_lock: pthread_mutex_t;
      osd_renderer: Posd_renderer_t;
      stream_info: array[0..XINE_STREAM_INFO_MAX-1] of Integer;
      meta_info: array[0..XINE_STREAM_INFO_MAX-1] of PChar;
      first_frame_flag: Integer;
      first_frame_lock: pthread_mutex_t;
      first_frame_reached: pthread_cond_t;
      counter_lock: pthread_mutex_t;
      counter_changed: pthread_cond_t;
      header_count_audio, header_count_video: Integer;
      finished_count_audio, finished_count_video: Integer;
      event_queues: Pxine_list_t;
      event_queues_lock: pthread_mutex_t;
      demux_thread: pthread_t;
      demux_thread_running: Integer;
      demux_lock: pthread_mutex_t;
      demux_action_pending: Integer;
      err: Integer;
      next_video_port: Pxine_video_port_t;
      next_audio_port: Pxine_audio_port_t;
      next_video_port_lock, next_audio_port_lock: pthread_mutex_t;
      next_video_port_wired, next_audio_port_wired: pthread_cond_t;
      metronom_prebuffer: int64_t;
   end;
   xine_pids_data_t = record
      vpid, apid: Integer;
   end;

   plugin_info_t = record
      typ, api: uint8_t;
      id: PChar;
      version: uint32_t;
      special_info: Pointer;
      init: procedure(xine: Pxine_t; data: Pointer); cdecl;
   end;
   vo_info_t = record
      priority, visual_type: Integer;
   end;
   ao_info_t = record
      priority: Integer;
   end;
   decoder_info_t = record
      supported_types: Puint32_t;
      priority: Integer;
   end;
   post_info_t = record
      typ: uint32_t;
   end;

   xine_mutex_t = record
      mutex: pthread_mutex_t;
      id: array[0..79] of Char;
      locked_by: PChar;
   end;

   xine_node_s = record
      next, prev: Pxine_node_t;
      content: Pointer;
      priority: Integer;
   end;
   xine_list_s = record
      first, last, cur: Pxine_node_t;
   end;
   yuv_planes_s = record
      y,u,v: PByte;
      row_width, row_count: Cardinal;
   end;

   plugin_info_s = record
      typ, API: uint8_t;
      id: PChar;
      version: uint32_t;
      special_info: Pointer;
      init: procedure(xine: Pxine_t; data: Pointer); cdecl;
   end;
   clut_t = record
      cb, cr, y, foo: uint8_t;
   end;

   xml_property_s = record
      name, value: PChar;
      next: Pxml_property_s;
   end;
   xml_node_s = record
      name, data: PChar;
      props: Pxml_property_s;
      child, next: Pxml_node_s;
   end;
(* Const before type ignored *)


  var
      xine_new : function:Pxine_t;cdecl;
    {
     * post_init the xine engine
      }
      xine_init : procedure(self:pxine_t);cdecl;
    {
     * helper functions to find and init audio/video drivers
     * from xine's plugin collection
     *
     * id    : identifier of the driver, may be NULL for auto-detection
     * data  : special data struct for ui/driver communications, depends
     *         on driver
     * visual: video driver flavor selector, constants see below
     *
     * both functions may return NULL if driver failed to load, was not
     * found ...
     *
     * use xine_close_audio/video_driver() to close loaded drivers
     * and free resources allocated by them
      }
  (* Const before type ignored *)
      xine_open_audio_driver : function(self:Pxine_t; id:Pchar; data:pointer):Pxine_audio_port_t;cdecl;
  (* Const before type ignored *)
      xine_open_video_driver : function(var self:xine_s; id:Pchar; visual:longint; var data:pointer):Pxine_video_port_t;cdecl;
      xine_close_audio_driver : procedure(var self:xine_s; var driver:xine_audio_port_s);cdecl;
      xine_close_video_driver : procedure(var self:xine_s; var driver:xine_video_port_s);cdecl;
    { valid visual types  }

    var
      xine_exit : procedure(self:pxine_t);cdecl;
    {********************************************************************
     * stream handling                                                   *
     ******************************************************************** }
    {
     * create a new stream for media playback/access
     *
     * returns xine_stream_t* if OK,
     *         NULL on error (use xine_get_error for details)
     *
     * the only proper way to free the stream pointer returned by this
     * function is to call xine_dispose() on it. do not try to access any
     * fields in xine_stream_t, they're all private and subject to change
     * without further notice.
      }
      xine_stream_new : function(self:Pxine_T; ao:Pxine_audio_port_T; vo:Pxine_video_port_t):Pxine_stream_t;cdecl;
    {
     * Make one stream the slave of another.
     * This establishes a binary master slave relation on streams, where
     * certain operations (specified by parameter "affection") on the master
     * stream are also applied to the slave stream.
     * If you want more than one stream to react to one master, you have to
     * apply the calls in a top down way:
     *  xine_stream_master_slave(stream1, stream2, 3);
     *  xine_stream_master_slave(stream2, stream3, 3);
     * This will make stream1 affect stream2 and stream2 affect stream3, so
     * effectively, operations on stream1 propagate to stream2 and 3.
     *
     * Please note that subsequent master_slave calls on the same streams
     * will overwrite their previous master/slave setting.
     * Be sure to not mess around.
     *
     * returns 1 on success, 0 on failure
      }
      xine_stream_master_slave : function(var master:xine_stream_s; var slave:xine_stream_s; affection:longint):longint;cdecl;
    { affection is some of the following ORed together:  }
    { playing the master plays the slave  }

    const
    { slave is synced to master's speed  }
      XINE_MASTER_SLAVE_SPEED = 1 shl 2;
    {
     * open a stream
     *
     * look for input / demux / decoder plugins, find out about the format
     * see if it is supported, set up internal buffers and threads
     *
     * returns 1 if OK, 0 on error (use xine_get_error for details)
     }
var
    xine_open : function(stream:pxine_stream_t; mrl:Pchar):longint;cdecl;
  {
   * play a stream from a given position
   *
   * start_pos:  0..65535
   * start_time: milliseconds
   * if both start position parameters are != 0 start_pos will be used
   * for non-seekable streams both values will be ignored
   *
   * returns 1 if OK, 0 on error (use xine_get_error for details)
    }

    xine_play : function(stream:pxine_stream_t; start_pos:longint; start_time:longint):longint;cdecl;
  {
   * stop stream playback
   * xine_stream_s stays valid for new xine_open or xine_play
    }

  var
    xine_stop : procedure(stream:pxine_stream_t);cdecl;
  {
   * stop stream playback, free all stream-related resources
   * xine_stream_s stays valid for new xine_open
    }
    xine_close : procedure(stream:pxine_stream_t);cdecl;
  {
   * ask current/recent input plugin to eject media - may or may not work,
   * depending on input plugin capabilities
    }
    xine_eject : function(var stream:xine_stream_s):longint;cdecl;
  {
   * stop playback, dispose all stream-related resources
   * xine_stream_s no longer valid when after this
    }
    xine_dispose : procedure(var stream:xine_stream_s);cdecl;
  {
   * set/get engine parameters.
    }
    xine_engine_set_param : procedure(var self:xine_s; param:longint; value:longint);cdecl;
    xine_engine_get_param : function(var self:xine_s; param:longint):longint;cdecl;

  {
   * set/get xine stream parameters
   * e.g. playback speed, constants see below
    }

  var
    xine_set_param : procedure(stream:pxine_stream_t; param:longint; value:longint);cdecl;
    xine_get_param : function(stream:pxine_stream_t; param:longint):longint;cdecl;
  {
   * xine stream parameters
    }
  { see below                    }
  const
    XINE_PARAM_SPEED = 1;    
  { unit: 1/90000 sec            }    XINE_PARAM_AV_OFFSET = 2;    
  { -1 => auto, -2 => off        }    XINE_PARAM_AUDIO_CHANNEL_LOGICAL = 3;    
    XINE_PARAM_SPU_CHANNEL = 4;    
    XINE_PARAM_VIDEO_CHANNEL = 5;    
  { 0..100                       }    XINE_PARAM_AUDIO_VOLUME = 6;    
  { 1=>mute, 0=>unmute           }    XINE_PARAM_AUDIO_MUTE = 7;    
  { <100=>off, % compress otherw }    XINE_PARAM_AUDIO_COMPR_LEVEL = 8;    
  { 0..200, 100=>100% (default)  }    XINE_PARAM_AUDIO_AMP_LEVEL = 9;    
  { 1=>send events, 0=> don't    }    XINE_PARAM_AUDIO_REPORT_LEVEL = 10;    
  { control console output       }    XINE_PARAM_VERBOSITY = 11;    
  { unit: 1/90000 sec            }    XINE_PARAM_SPU_OFFSET = 12;    
  { disable video decoding       }    XINE_PARAM_IGNORE_VIDEO = 13;    
  { disable audio decoding       }    XINE_PARAM_IGNORE_AUDIO = 14;    
  { disable spu decoding         }    XINE_PARAM_IGNORE_SPU = 15;    
  { 0: disable, x: server port   }    XINE_PARAM_BROADCASTER_PORT = 16;    
  { unit: 1/90000 sec            }    XINE_PARAM_METRONOM_PREBUFFER = 17;    
  { equalizer gains -100..100    }    XINE_PARAM_EQ_30HZ = 18;    
  { equalizer gains -100..100    }    XINE_PARAM_EQ_60HZ = 19;    
  { equalizer gains -100..100    }    XINE_PARAM_EQ_125HZ = 20;    
  { equalizer gains -100..100    }    XINE_PARAM_EQ_250HZ = 21;    
  { equalizer gains -100..100    }    XINE_PARAM_EQ_500HZ = 22;    
  { equalizer gains -100..100    }    XINE_PARAM_EQ_1000HZ = 23;    
  { equalizer gains -100..100    }    XINE_PARAM_EQ_2000HZ = 24;    
  { equalizer gains -100..100    }    XINE_PARAM_EQ_4000HZ = 25;    
  { equalizer gains -100..100    }    XINE_PARAM_EQ_8000HZ = 26;    
  { equalizer gains -100..100    }    XINE_PARAM_EQ_16000HZ = 27;    
  { force closing audio device   }    XINE_PARAM_AUDIO_CLOSE_DEVICE = 28;    
  { 1=>mute, 0=>unmute  }    XINE_PARAM_AUDIO_AMP_MUTE = 29;    
  { 1.000.000 => normal speed    }    XINE_PARAM_FINE_SPEED = 30;    
  { send event when demux finish }    XINE_PARAM_EARLY_FINISHED_EVENT = 31;    
  { next stream only gapless swi }    XINE_PARAM_GAPLESS_SWITCH = 32;    
  { 1/10sec,0=>disable,-1=>forev }    XINE_PARAM_DELAY_FINISHED_EVENT = 33;    
  {
   * speed values for XINE_PARAM_SPEED parameter.
   *
   * alternatively, one may use XINE_PARAM_FINE_SPEED for greater
   * control of the speed value, where:
   * XINE_PARAM_SPEED / 4 <-> XINE_PARAM_FINE_SPEED / 1000000
    }
    XINE_SPEED_PAUSE = 0;    
    XINE_SPEED_SLOW_4 = 1;    
    XINE_SPEED_SLOW_2 = 2;    
    XINE_SPEED_NORMAL = 4;    
    XINE_SPEED_FAST_2 = 8;    
    XINE_SPEED_FAST_4 = 16;    
  { normal speed value for XINE_PARAM_FINE_SPEED parameter  }
    XINE_FINE_SPEED_NORMAL = 1000000;    
  { video parameters  }
  { bool                }    XINE_PARAM_VO_DEINTERLACE = $01000000;    
  { see below           }    XINE_PARAM_VO_ASPECT_RATIO = $01000001;    
  { 0..65535            }    XINE_PARAM_VO_HUE = $01000002;    
  { 0..65535            }    XINE_PARAM_VO_SATURATION = $01000003;    
  { 0..65535            }    XINE_PARAM_VO_CONTRAST = $01000004;    
  { 0..65535            }    XINE_PARAM_VO_BRIGHTNESS = $01000005;    
  { 0..65535            }    XINE_PARAM_VO_GAMMA = $0100000c;    
  { percent             }    XINE_PARAM_VO_ZOOM_X = $01000008;    
  { percent             }    XINE_PARAM_VO_ZOOM_Y = $0100000d;    
  { bool                }    XINE_PARAM_VO_PAN_SCAN = $01000009;    
  { ???                 }    XINE_PARAM_VO_TVMODE = $0100000a;    
  { readonly            }    XINE_PARAM_VO_WINDOW_WIDTH = $0100000f;    
  { readonly            }    XINE_PARAM_VO_WINDOW_HEIGHT = $01000010;    
  { crop frame pixels   }    XINE_PARAM_VO_CROP_LEFT = $01000020;    
  { crop frame pixels   }    XINE_PARAM_VO_CROP_RIGHT = $01000021;    
  { crop frame pixels   }    XINE_PARAM_VO_CROP_TOP = $01000022;    
  { crop frame pixels   }    XINE_PARAM_VO_CROP_BOTTOM = $01000023;    
    XINE_VO_ZOOM_STEP = 100;    
    XINE_VO_ZOOM_MAX = 400;    
    XINE_VO_ZOOM_MIN = -(85);    
  { possible ratios for XINE_PARAM_VO_ASPECT_RATIO  }
    XINE_VO_ASPECT_AUTO = 0;    
  { 1:1     }    XINE_VO_ASPECT_SQUARE = 1;    
  { 4:3     }    XINE_VO_ASPECT_4_3 = 2;    
  { 16:9    }    XINE_VO_ASPECT_ANAMORPHIC = 3;    
  { 2.11:1  }    XINE_VO_ASPECT_DVB = 4;    
    XINE_VO_ASPECT_NUM_RATIOS = 5;    
  { stream format detection strategies  }
  { recognize stream type first by content then by extension.  }
    XINE_DEMUX_DEFAULT_STRATEGY = 0;    
  { recognize stream type first by extension then by content.  }
    XINE_DEMUX_REVERT_STRATEGY = 1;    
  { recognize stream type by content only.                     }
    XINE_DEMUX_CONTENT_STRATEGY = 2;    
  { recognize stream type by extension only.                   }
    XINE_DEMUX_EXTENSION_STRATEGY = 3;    
  { verbosity settings  }
    XINE_VERBOSITY_NONE = 0;    
    XINE_VERBOSITY_LOG = 1;    
    XINE_VERBOSITY_DEBUG = 2;    
  {
   * snapshot function
   *
   * image format can be YUV 4:2:0 or 4:2:2
   * will copy the image data into memory that <img> points to
   * (interleaved for yuv 4:2:2 or planary for 4:2:0)
   * 
   * xine_get_current_frame() requires that <img> must be able
   * to hold the image data. Use a NULL pointer to retrieve the
   * necessary parameters for calculating the buffer size. Be
   * aware that the image can change between two successive calls
   * so you better pause the stream.
   *
   * xine_get_current_frame_s() requires to specify the buffer
   * size and it returns the needed / used size. It won't copy
   * image data into a too small buffer.
   *
   * xine_get_current_frame_alloc() takes care of allocating
   * a buffer on its own, so image data can be retrieved by
   * a single call without the need to pause the stream.
   * 
   * xine_get_current_frame_data() passes the parameters of the
   * previously mentioned functions plus further information in
   * a structure and can work like the _s or _alloc function
   * respectively depending on the passed flags.
   *
   * all functions return 1 on success, 0 failure.
    }

  var
    xine_get_current_frame : function(var stream:xine_stream_s; var width:longint; var height:longint; var ratio_code:longint; var format:longint;
      var img:uint8_t):longint;cdecl;
    xine_get_current_frame_s : function(var stream:xine_stream_s; var width:longint; var height:longint; var ratio_code:longint; var format:longint;
      var img:uint8_t; var img_size:longint):longint;cdecl;
    xine_get_current_frame_alloc : function(var stream:xine_stream_s; var width:longint; var height:longint; var ratio_code:longint; var format:longint;
      var img:Puint8_t; var img_size:longint):longint;cdecl;

  type
    Pxine_current_frame_data_t = ^xine_current_frame_data_t;
    xine_current_frame_data_t = record
        width : longint;
        height : longint;
        crop_left : longint;
        crop_right : longint;
        crop_top : longint;
        crop_bottom : longint;
        ratio_code : longint;
        interlaced : longint;
        format : longint;
        img_size : longint;
        img : Puint8_t;
      end;

  const
    XINE_FRAME_DATA_ALLOCATE_IMG = 1 shl 0;    

  var
    xine_get_current_frame_data : function(var stream:xine_stream_s; var data:xine_current_frame_data_t; flags:longint):longint;cdecl;
  { xine image formats  }

  const
    XINE_IMGFMT_YV12 = (((ord('2') shl 24) or (ord('1') shl 16)) or (ord('V') shl 8)) or ord('Y');
    XINE_IMGFMT_YUY2 = (((ord('2') shl 24) or (ord('Y') shl 16)) or (ord('U') shl 8)) or ord('Y');
    XINE_IMGFMT_XVMC = (((ord('C') shl 24) or (ord('M') shl 16)) or (ord('v') shl 8)) or ord('X');
    XINE_IMGFMT_XXMC = (((ord('C') shl 24) or (ord('M') shl 16)) or (ord('x') shl 8)) or ord('X');
  { get current xine's virtual presentation timestamp (1/90000 sec)
   * note: this is mostly internal data.
   * one can use vpts with xine_osd_show() and xine_osd_hide().
    }

  var
    xine_get_current_vpts : function(var stream:xine_stream_s):int64_t;cdecl;
  {********************************************************************
   * media processing                                                  *
   ******************************************************************** }
{$ifdef XINE_ENABLE_EXPERIMENTAL_FEATURES}
  {
   * access to decoded audio and video frames from a stream
   * these functions are intended to provide the basis for
   * re-encoding and other video processing applications
   *
   * note that the xine playback engine will block when
   * rendering to a framegrab port: to unblock the stream,
   * you must fetch the frames manually with the
   * xine_get_next_* functions.  this ensures that a
   * framegrab port is guaranteed to never miss a frame.
   *
    }
    xine_new_framegrab_video_port : function(var self:xine_s):Pxine_video_port_t;cdecl;
  { timestamp 1/90000 sec for a/v sync  }
  { XINE_IMGFMT_*  }
  { bytes from stream start  }
  { milliseconds  }
  { used internally by xine engine  }
  { frame number (may be unknown)  }

  type
    Pxine_video_frame_t = ^xine_video_frame_t;
    xine_video_frame_t = record
        vpts : int64_t;
        duration : int64_t;
        width : longint;
        height : longint;
        colorspace : longint;
        aspect_ratio : double;
        pos_stream : longint;
        pos_time : longint;
        data : Puint8_t;
        xine_frame : pointer;
        frame_number : longint;
      end;

  var
    xine_get_next_video_frame : function(var port:xine_video_port_s; var frame:xine_video_frame_t):longint;cdecl;
    xine_free_video_frame : procedure(var port:xine_video_port_s; var frame:xine_video_frame_t);cdecl;
    xine_new_framegrab_audio_port : function(var self:xine_s):Pxine_audio_port_t;cdecl;
  { timestamp 1/90000 sec for a/v sync  }
  { per channel  }
  { bytes from stream start  }
  { milliseconds  }
  { used internally by xine engine  }

  type
    Pxine_audio_frame_t = ^xine_audio_frame_t;
    xine_audio_frame_t = record
        vpts : int64_t;
        num_samples : longint;
        sample_rate : longint;
        num_channels : longint;
        bits_per_sample : longint;
        pos_stream : off_t;
        pos_time : longint;
        data : Puint8_t;
        xine_frame : pointer;
      end;

  var
    xine_get_next_audio_frame : function(var port:xine_audio_port_s; var frame:xine_audio_frame_t):longint;cdecl;
    xine_free_audio_frame : procedure(var port:xine_audio_port_s; var frame:xine_audio_frame_t);cdecl;
{$endif}
  {********************************************************************
   * post plugin handling                                              *
   ******************************************************************** }
  {
   * post effect plugin functions
   *
   * after the data leaves the decoder it can pass an arbitrary tree
   * of post plugins allowing for effects to be applied to the video
   * frames/audio buffers before they reach the output stage
    }



  {
   * initialize a post plugin
   *
   * returns xine_post_t* on success, NULL on failure
   *
   * Initializes the post plugin with the given name and connects its
   * outputs to the NULL-terminated arrays of audio and video ports.
   * Some plugins also care about the number of inputs you request
   * (e.g. mixer plugins), others simply ignore this number.
    }
(* Const before type ignored *)

  var
    xine_post_init : function(var xine:xine_s; name:Pchar; inputs:longint; var audio_target:Pxine_audio_port_t; var video_target:Pxine_video_port_t):Pxine_post_t;cdecl;
  { get a list of all available post plugins  }
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_list_post_plugins : function(var xine:xine_s):PPchar;cdecl;
  { get a list of all post plugins of one type  }
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_list_post_plugins_typed : function(var xine:xine_s; _type:longint):PPchar;cdecl;
  {
   * post plugin input/output
   *
   * These structures encapsulate inputs/outputs for post plugins
   * to transfer arbitrary data. Frontends can also provide inputs
   * and outputs and connect them to post plugins to exchange data
   * with them.
    }


  var
    xine_post_list_inputs : function(var self:xine_post_t):PPchar;cdecl;
  { get a list of all outputs of a post plugin  }
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_post_list_outputs : function(var self:xine_post_t):PPchar;cdecl;
  { retrieve one specific input of a post plugin  }
(* Const before type ignored *)
    xine_post_input : function(var self:xine_post_t; name:Pchar):Pxine_post_in_t;cdecl;
  { retrieve one specific output of a post plugin  }
(* Const before type ignored *)
    xine_post_output : function(var self:xine_post_t; name:Pchar):Pxine_post_out_t;cdecl;
  {
   * wire an input to an output
   * returns 1 on success, 0 on failure
    }
    xine_post_wire : function(var source:xine_post_out_t; var target:xine_post_in_t):longint;cdecl;
  {
   * wire a video port to a video output
   * This can be used to rewire different post plugins to the video output
   * plugin layer. The ports you hand in at xine_post_init() will already
   * be wired with the post plugin, so you need this function for
   * _re_connecting only.
   *
   * returns 1 on success, 0 on failure
    }
    xine_post_wire_video_port : function(var source:xine_post_out_t; var vo:xine_video_port_s):longint;cdecl;
  {
   * wire an audio port to an audio output
   * This can be used to rewire different post plugins to the audio output
   * plugin layer. The ports you hand in at xine_post_init() will already
   * be wired with the post plugin, so you need this function for
   * _re_connecting only.
   *
   * returns 1 on success, 0 on failure
    }
    xine_post_wire_audio_port : function(var source:xine_post_out_t; var ao:xine_audio_port_s):longint;cdecl;
  {
   * Extracts an output for a stream. Use this to rewire the outputs of streams.
    }
    xine_get_video_source : function(var stream:xine_stream_s):Pxine_post_out_t;cdecl;
    xine_get_audio_source : function(var stream:xine_stream_s):Pxine_post_out_t;cdecl;
  {
   * disposes the post plugin
   * please make sure that no other post plugin and no stream is
   * connected to any of this plugin's inputs
    }
    xine_post_dispose : procedure(var xine:xine_s; var self:xine_post_t);cdecl;
  { post plugin types  }

  const
    XINE_POST_TYPE_VIDEO_FILTER = $010000;    
    XINE_POST_TYPE_VIDEO_VISUALIZATION = $010001;    
    XINE_POST_TYPE_VIDEO_COMPOSE = $010002;    
    XINE_POST_TYPE_AUDIO_FILTER = $020000;    
    XINE_POST_TYPE_AUDIO_VISUALIZATION = $020001;    
  { post plugin data types  }
  { video port data
   * input->data is a xine_video_port_s*
   * output->data usually is a xine_video_port_s**
    }
    XINE_POST_DATA_VIDEO = 0;    
  { audio port data
   * input->data is a xine_audio_port_s*
   * output->data usually is a xine_audio_port_s**
    }
    XINE_POST_DATA_AUDIO = 1;    
  { integer data
   * input->data is a int*
   * output->data usually is a int*
    }
    XINE_POST_DATA_INT = 3;    
  { double precision floating point data
   * input->data is a double*
   * output->data usually is a double*
    }
    XINE_POST_DATA_DOUBLE = 4;    
  { parameters api (used by frontends)
   * input->data is xine_post_api_t*  (see below)
    }
    XINE_POST_DATA_PARAMETERS = 5;    
  { defines a single parameter entry.  }
  { POST_PARAM_TYPE_xxx              }
  { name of this parameter           }
  { sizeof(parameter)                }
  { offset in bytes from struct ptr  }
  { enumeration (first=0) or NULL    }
  { minimum value                    }
  { maximum value                    }
  { 0 = read/write, 1=read-only      }
  { user-friendly description        }

  type
    Pxine_post_api_parameter_t = ^xine_post_api_parameter_t;
    xine_post_api_parameter_t = record
        _type : longint;
        name : Pchar;
        size : longint;
        offset : longint;
        enum_values : PPchar;
        range_min : double;
        range_max : double;
        readonly : longint;
        description : Pchar;
      end;
  { description of parameters struct (params).  }
  { sizeof(params)      }
  { list of parameters  }

    Pxine_post_api_descr_t = ^xine_post_api_descr_t;
    xine_post_api_descr_t = record
        struct_size : longint;
        parameter : Pxine_post_api_parameter_t;
      end;
  {
     * method to set all the read/write parameters.
     * params is a struct * defined by xine_post_api_descr_t
      }
  {
     * method to get all parameters.
      }
  {
     * method to get params struct definition
      }
  {
     * method to get plugin and parameters help (UTF-8)
     * the help string must be word wrapped by the frontend.
     * it might contain \n to mark paragraph breaks.
      }

    Pxine_post_api_t = ^xine_post_api_t;
    xine_post_api_t = record
        set_parameters : function (var self:xine_post_t; var params:pointer):longint;cdecl;
        get_parameters : function (var self:xine_post_t; var params:pointer):longint;
        get_param_descr : function :Pxine_post_api_descr_t;
        get_help : function :Pchar;
      end;
  { post parameter types  }
  { terminator of parameter list        }
  const
    POST_PARAM_TYPE_LAST = 0;    
  { integer (or vector of integers)     }    POST_PARAM_TYPE_INT = 1;    
  { double (or vector of doubles)       }    POST_PARAM_TYPE_DOUBLE = 2;    
  { char (or vector of chars = string)  }    POST_PARAM_TYPE_CHAR = 3;    
  { (char *), ASCIIZ                    }    POST_PARAM_TYPE_STRING = 4;    
  { (char **) list, NULL terminated     }    POST_PARAM_TYPE_STRINGLIST = 5;    
  { integer (0 or 1)                    }    POST_PARAM_TYPE_BOOL = 6;    
  {********************************************************************
   * information retrieval                                             *
   ******************************************************************** }
  {
   * xine log functions
   *
   * frontends can display xine log output using these functions
    }

  var
    xine_get_log_section_count : function(var self:xine_s):longint;cdecl;
  { return a NULL terminated array of log sections names  }
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_get_log_names : function(var self:xine_s):PPchar;cdecl;
  { get log messages of specified section  }
(* Const before declarator ignored *)
    xine_get_log : function(var self:xine_s; buf:longint):PPchar;cdecl;
  { log callback will be called whenever something is logged  }

  var
    xine_register_log_cb : procedure(var self:xine_s; cb:xine_log_cb_t; var user_data:pointer);cdecl;
  {
   * error handling / engine status
    }
  { return last error   }
    xine_get_error : function(stream:pxine_stream_t):longint;cdecl;
  { get current xine engine status (constants see below)  }
    xine_get_status : function(stream:pxine_stream_t):longint;cdecl;
  {
   * engine status codes
    }
  { no mrl assigned  }
  const
    XINE_STATUS_IDLE = 0;    
    XINE_STATUS_STOP = 1;    
    XINE_STATUS_PLAY = 2;    
    XINE_STATUS_QUIT = 3;    
  {
   * xine error codes
    }
    XINE_ERROR_NONE = 0;    
    XINE_ERROR_NO_INPUT_PLUGIN = 1;    
    XINE_ERROR_NO_DEMUX_PLUGIN = 2;    
    XINE_ERROR_DEMUX_FAILED = 3;    
    XINE_ERROR_MALFORMED_MRL = 4;    
    XINE_ERROR_INPUT_FAILED = 5;    
  {
   * try to find out audio/spu language of given channel
   * (use -1 for current channel)
   *
   * lang must point to a buffer of at least XINE_LANG_MAX bytes
   *
   * returns 1 on success, 0 on failure
    }

  var
    xine_get_audio_lang : function(var stream:xine_stream_s; channel:longint; lang:Pchar):longint;cdecl;
    xine_get_spu_lang : function(var stream:xine_stream_s; channel:longint; lang:Pchar):longint;cdecl;
  {_x_ increasing this number means an incompatible ABI breakage!  }

  const
    XINE_LANG_MAX = 32;    
  {
   * get position / length information
   *
   * depending of the nature and system layer of the stream,
   * some or all of this information may be unavailable or incorrect
   * (e.g. live network streams may not have a valid length)
   *
   * returns 1 on success, 0 on failure (data was not updated,
   * probably because it's not known yet... try again later)
    }
  { 0..65535      }
  { milliseconds  }
  { milliseconds  }

  var
    xine_get_pos_length : function(stream:pxine_stream_t; var pos_stream:longint; var pos_time:longint; var length_time:longint):longint;cdecl;
  {
   * get information about the stream such as
   * video width/height, codecs, audio format, title, author...
   * strings are UTF-8 encoded.
   *
   * constants see below
    }
    xine_get_stream_info : function(var stream:xine_stream_s; info:longint):uint32_t;cdecl;
(* Const before type ignored *)
    xine_get_meta_info : function(var stream:xine_stream_s; info:longint):Pchar;cdecl;
  { xine_get_stream_info  }

  const
    XINE_STREAM_INFO_BITRATE = 0;    
    XINE_STREAM_INFO_SEEKABLE = 1;    
    XINE_STREAM_INFO_VIDEO_WIDTH = 2;    
    XINE_STREAM_INFO_VIDEO_HEIGHT = 3;    
  { *10000  }    XINE_STREAM_INFO_VIDEO_RATIO = 4;    
    XINE_STREAM_INFO_VIDEO_CHANNELS = 5;    
    XINE_STREAM_INFO_VIDEO_STREAMS = 6;    
    XINE_STREAM_INFO_VIDEO_BITRATE = 7;    
    XINE_STREAM_INFO_VIDEO_FOURCC = 8;    
  { codec available?  }    XINE_STREAM_INFO_VIDEO_HANDLED = 9;    
  { 1/90000 sec  }    XINE_STREAM_INFO_FRAME_DURATION = 10;    
    XINE_STREAM_INFO_AUDIO_CHANNELS = 11;    
    XINE_STREAM_INFO_AUDIO_BITS = 12;    
    XINE_STREAM_INFO_AUDIO_SAMPLERATE = 13;    
    XINE_STREAM_INFO_AUDIO_BITRATE = 14;    
    XINE_STREAM_INFO_AUDIO_FOURCC = 15;    
  { codec available?  }    XINE_STREAM_INFO_AUDIO_HANDLED = 16;    
    XINE_STREAM_INFO_HAS_CHAPTERS = 17;    
    XINE_STREAM_INFO_HAS_VIDEO = 18;    
    XINE_STREAM_INFO_HAS_AUDIO = 19;    
    XINE_STREAM_INFO_IGNORE_VIDEO = 20;    
    XINE_STREAM_INFO_IGNORE_AUDIO = 21;    
    XINE_STREAM_INFO_IGNORE_SPU = 22;    
    XINE_STREAM_INFO_VIDEO_HAS_STILL = 23;    
    XINE_STREAM_INFO_MAX_AUDIO_CHANNEL = 24;    
    XINE_STREAM_INFO_MAX_SPU_CHANNEL = 25;    
    XINE_STREAM_INFO_AUDIO_MODE = 26;    
  { for 1000 frames delivered  }    XINE_STREAM_INFO_SKIPPED_FRAMES = 27;    
  { for 1000 frames delivered  }    XINE_STREAM_INFO_DISCARDED_FRAMES = 28;    
    XINE_STREAM_INFO_VIDEO_AFD = 29;    
    XINE_STREAM_INFO_DVD_TITLE_NUMBER = 30;    
    XINE_STREAM_INFO_DVD_TITLE_COUNT = 31;    
    XINE_STREAM_INFO_DVD_CHAPTER_NUMBER = 32;    
    XINE_STREAM_INFO_DVD_CHAPTER_COUNT = 33;    
    XINE_STREAM_INFO_DVD_ANGLE_NUMBER = 34;    
    XINE_STREAM_INFO_DVD_ANGLE_COUNT = 35;    
  { possible values for XINE_STREAM_INFO_VIDEO_AFD  }
    XINE_VIDEO_AFD_NOT_PRESENT = -(1);    
    XINE_VIDEO_AFD_RESERVED_0 = 0;    
    XINE_VIDEO_AFD_RESERVED_1 = 1;    
    XINE_VIDEO_AFD_BOX_16_9_TOP = 2;    
    XINE_VIDEO_AFD_BOX_14_9_TOP = 3;    
    XINE_VIDEO_AFD_BOX_GT_16_9_CENTRE = 4;    
    XINE_VIDEO_AFD_RESERVED_5 = 5;    
    XINE_VIDEO_AFD_RESERVED_6 = 6;    
    XINE_VIDEO_AFD_RESERVED_7 = 7;    
    XINE_VIDEO_AFD_SAME_AS_FRAME = 8;    
    XINE_VIDEO_AFD_4_3_CENTRE = 9;    
    XINE_VIDEO_AFD_16_9_CENTRE = 10;    
    XINE_VIDEO_AFD_14_9_CENTRE = 11;    
    XINE_VIDEO_AFD_RESERVED_12 = 12;    
    XINE_VIDEO_AFD_4_3_PROTECT_14_9 = 13;    
    XINE_VIDEO_AFD_16_9_PROTECT_14_9 = 14;    
    XINE_VIDEO_AFD_16_9_PROTECT_4_3 = 15;    
  { xine_get_meta_info  }
    XINE_META_INFO_TITLE = 0;    
    XINE_META_INFO_COMMENT = 1;    
    XINE_META_INFO_ARTIST = 2;    
    XINE_META_INFO_GENRE = 3;    
    XINE_META_INFO_ALBUM = 4;    
  { may be full date  }    XINE_META_INFO_YEAR = 5;    
    XINE_META_INFO_VIDEOCODEC = 6;    
    XINE_META_INFO_AUDIOCODEC = 7;    
    XINE_META_INFO_SYSTEMLAYER = 8;    
    XINE_META_INFO_INPUT_PLUGIN = 9;    
    XINE_META_INFO_CDINDEX_DISCID = 10;    
    XINE_META_INFO_TRACK_NUMBER = 11;    
    XINE_META_INFO_COMPOSER = 12;    
  { post-1.1.17; taken from the list at http://age.hobba.nl/audio/mirroredpages/ogg-tagging.html on 2009-12-11  }
    XINE_META_INFO_PUBLISHER = 13;    
    XINE_META_INFO_COPYRIGHT = 14;    
    XINE_META_INFO_LICENSE = 15;    
    XINE_META_INFO_ARRANGER = 16;    
    XINE_META_INFO_LYRICIST = 17;    
    XINE_META_INFO_AUTHOR = 18;    
    XINE_META_INFO_CONDUCTOR = 19;    
    XINE_META_INFO_PERFORMER = 20;    
    XINE_META_INFO_ENSEMBLE = 21;    
    XINE_META_INFO_OPUS = 22;    
    XINE_META_INFO_PART = 23;    
    XINE_META_INFO_PARTNUMBER = 24;    
    XINE_META_INFO_LOCATION = 25;    
  { post-1.1.18.1  }
    XINE_META_INFO_DISCNUMBER = 26;    
  {********************************************************************
   * plugin management / autoplay / mrl browsing                       *
   ******************************************************************** }
  {
   * note: the pointers to strings or string arrays returned
   *       by some of these functions are pointers to statically
   *       alloced internal xine memory chunks.
   *       they're only valid between xine function calls
   *       and should never be free()d.
    }
  { file plugin: path  }
  { <type>://<location>  }
  { see below  }
  { size of this source, may be 0  }

  { mrl types  }

  const
    XINE_MRL_TYPE_unknown = 0 shl 0;    
    XINE_MRL_TYPE_dvd = 1 shl 0;    
    XINE_MRL_TYPE_vcd = 1 shl 1;    
    XINE_MRL_TYPE_net = 1 shl 2;    
    XINE_MRL_TYPE_rtp = 1 shl 3;    
    XINE_MRL_TYPE_stdin = 1 shl 4;    
    XINE_MRL_TYPE_cda = 1 shl 5;    
    XINE_MRL_TYPE_file = 1 shl 6;    
    XINE_MRL_TYPE_file_fifo = 1 shl 7;    
    XINE_MRL_TYPE_file_chardev = 1 shl 8;    
    XINE_MRL_TYPE_file_directory = 1 shl 9;    
    XINE_MRL_TYPE_file_blockdev = 1 shl 10;    
    XINE_MRL_TYPE_file_normal = 1 shl 11;    
    XINE_MRL_TYPE_file_symlink = 1 shl 12;    
    XINE_MRL_TYPE_file_sock = 1 shl 13;    
    XINE_MRL_TYPE_file_exec = 1 shl 14;    
    XINE_MRL_TYPE_file_backup = 1 shl 15;    
    XINE_MRL_TYPE_file_hidden = 1 shl 16;    
  { get a list of browsable input plugin ids  }
(* Const before type ignored *)
(* Const before declarator ignored *)

  var
    xine_get_browsable_input_plugin_ids : function(var self:xine_s):PPchar;cdecl;
  {
   * ask input plugin named <plugin_id> to return
   * a list of available MRLs in domain/directory <start_mrl>.
   *
   * <start_mrl> may be NULL indicating the toplevel domain/dir
   * returns <start_mrl> if <start_mrl> is a valid MRL, not a directory
   * returns NULL if <start_mrl> is an invalid MRL, not even a directory.
    }
(* Const before type ignored *)
(* Const before type ignored *)
    xine_get_browse_mrls : function(var self:xine_s; plugin_id:Pchar; start_mrl:Pchar; var num_mrls:longint):PPxine_mrl_t;cdecl;
  { get a list of plugins that support the autoplay feature  }
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_get_autoplay_input_plugin_ids : function(var self:xine_s):PPchar;cdecl;
  { get autoplay MRL list from input plugin named <plugin_id>  }
(* Const before type ignored *)
    xine_get_autoplay_mrls : function(var self:xine_s; plugin_id:Pchar; var num_mrls:longint):PPchar;cdecl;
  { get a list of file extensions for file types supported by xine
   * the list is separated by spaces
   *
   * the pointer returned can be free()ed when no longer used  }
    xine_get_file_extensions : function(var self:xine_s):Pchar;cdecl;
  { get a list of mime types supported by xine
   *
   * the pointer returned can be free()ed when no longer used  }
    xine_get_mime_types : function(var self:xine_s):Pchar;cdecl;
  { get the demuxer identifier that handles a given mime type
   *
   * the pointer returned can be free()ed when no longer used
   * returns NULL if no demuxer is available to handle this.  }
(* Const before type ignored *)
    xine_get_demux_for_mime_type : function(var self:xine_s; mime_type:Pchar):Pchar;cdecl;
  { get a description string for a plugin  }
(* Const before type ignored *)
(* Const before type ignored *)
    xine_get_input_plugin_description : function(var self:xine_s; plugin_id:Pchar):Pchar;cdecl;
(* Const before type ignored *)
(* Const before type ignored *)
    xine_get_demux_plugin_description : function(var self:xine_s; plugin_id:Pchar):Pchar;cdecl;
(* Const before type ignored *)
(* Const before type ignored *)
    xine_get_spu_plugin_description : function(var self:xine_s; plugin_id:Pchar):Pchar;cdecl;
(* Const before type ignored *)
(* Const before type ignored *)
    xine_get_audio_plugin_description : function(var self:xine_s; plugin_id:Pchar):Pchar;cdecl;
(* Const before type ignored *)
(* Const before type ignored *)
    xine_get_video_plugin_description : function(var self:xine_s; plugin_id:Pchar):Pchar;cdecl;
(* Const before type ignored *)
(* Const before type ignored *)
    xine_get_audio_driver_plugin_description : function(var self:xine_s; plugin_id:Pchar):Pchar;cdecl;
(* Const before type ignored *)
(* Const before type ignored *)
    xine_get_video_driver_plugin_description : function(var self:xine_s; plugin_id:Pchar):Pchar;cdecl;
(* Const before type ignored *)
(* Const before type ignored *)
    xine_get_post_plugin_description : function(var self:xine_s; plugin_id:Pchar):Pchar;cdecl;
  { get lists of available audio and video output plugins  }
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_list_audio_output_plugins : function(self:pxine_t):PPchar;cdecl;
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_list_video_output_plugins : function(var self:xine_s):PPchar;cdecl;
  { typemask is (1ULL << XINE_VISUAL_TYPE_FOO) | ...  }
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_list_video_output_plugins_typed : function(var self:xine_s; typemask:uint64_t):PPchar;cdecl;
  { get list of available demultiplexor plugins  }
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_list_demuxer_plugins : function(var self:xine_s):PPchar;cdecl;
  { get list of available input plugins  }
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_list_input_plugins : function(var self:xine_s):PPchar;cdecl;
  { get list of available subpicture plugins  }
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_list_spu_plugins : function(var self:xine_s):PPchar;cdecl;
  { get list of available audio and video decoder plugins  }
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_list_audio_decoder_plugins : function(var self:xine_s):PPchar;cdecl;
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_list_video_decoder_plugins : function(var self:xine_s):PPchar;cdecl;
  { unload unused plugins  }
    xine_plugins_garbage_collector : procedure(var self:xine_s);cdecl;
  {********************************************************************
   * visual specific gui <-> xine engine communication                 *
   ******************************************************************** }
  { new (preferred) method to talk to video driver.  }
    xine_port_send_gui_data : function(var vo:xine_video_port_s; _type:longint; var data:pointer):longint;cdecl;
  { area of that drawable to be used by video  }

  {
   * this is the visual data struct any x11 gui
   * must supply to the xine_open_video_driver call
   * ("data" parameter)
    }
  { some information about the display  }
  { Display*  }
  { drawable to display the video in/on  }
  { Drawable  }
  {
     * dest size callback
     *
     * this will be called by the video driver to find out
     * how big the video output area size will be for a
     * given video size. The ui should _not_ adjust its
     * video out area, just do some calculations and return
     * the size. This will be called for every frame, ui
     * implementation should be fast.
     * dest_pixel_aspect should be set to the used display pixel aspect.
     * NOTE: Semantics has changed: video_width and video_height
     * are no longer pixel aspect corrected. Get the old semantics
     * in the UI with
     *   *dest_pixel_aspect = display_pixel_aspect;
     *   if (video_pixel_aspect >= display_pixel_aspect)
     *     video_width  = video_width * video_pixel_aspect / display_pixel_aspect + .5;
     *   else
     *     video_height = video_height * display_pixel_aspect / video_pixel_aspect + .5;
      }
  {
     * frame output callback
     *
     * this will be called by the video driver for every frame
     * it's about to draw. ui can adapt its size if necessary
     * here.
     * note: the ui doesn't have to adjust itself to this
     * size, this is just to be taken as a hint.
     * ui must return the actual size of the video output
     * area and the video output driver will do its best
     * to adjust the video frames to that size (while
     * preserving aspect ratio and stuff).
     *    dest_x, dest_y: offset inside window
     *    dest_width, dest_height: available drawing space
     *    dest_pixel_aspect: display pixel aspect
     *    win_x, win_y: window absolute screen position
     * NOTE: Semantics has changed: video_width and video_height
     * are no longer pixel aspect corrected. Get the old semantics
     * in the UI with
     *   *dest_pixel_aspect = display_pixel_aspect;
     *   if (video_pixel_aspect >= display_pixel_aspect)
     *     video_width  = video_width * video_pixel_aspect / display_pixel_aspect + .5;
     *   else
     *     video_height = video_height * display_pixel_aspect / video_pixel_aspect + .5;
      }
  {
     * lock display callback
     *
     * this callback is called when the video driver
     * needs access to the x11 display connection
     *
     * note: to enable this you MUST use XINE_VISUAL_TYPE_X11_2
     * note: if display_lock is NULL, the fallback is used
     * note: fallback for this function is XLockDisplay(display)
      }
  {
     * unlock display callback
     *
     * this callback is called when the video driver
     * doesn't need access to the x11 display connection anymore
     *
     * note: to enable this you MUST use XINE_VISUAL_TYPE_X11_2
     * note: if display_unlock is NULL, the fallback is used
     * note: fallback for this function is XUnlockDisplay(display)
      }
type
  {
   * this is the visual data struct any xcb gui
   * must supply to the xine_open_video_driver call
   * ("data" parameter)
    }
  { some information about the display  }
  { xcb_connection_t  }
  { xcb_screen_t      }
  { window to display the video in / on  }
  { xcb_window_t  }
  {
     * dest size callback
     *
     * this will be called by the video driver to find out
     * how big the video output area size will be for a
     * given video size. The ui should _not_ adjust its
     * video out area, just do some calculations and return
     * the size. This will be called for every frame, ui
     * implementation should be fast.
     * dest_pixel_aspect should be set to the used display pixel aspect.
     * NOTE: Semantics has changed: video_width and video_height
     * are no longer pixel aspect corrected. Get the old semantics
     * in the UI with
     *   *dest_pixel_aspect = display_pixel_aspect;
     *   if (video_pixel_aspect >= display_pixel_aspect)
     *     video_width  = video_width * video_pixel_aspect / display_pixel_aspect + .5;
     *   else
     *     video_height = video_height * display_pixel_aspect / video_pixel_aspect + .5;
      }
  {
     * frame output callback
     *
     * this will be called by the video driver for every frame
     * it's about to draw. ui can adapt its size if necessary
     * here.
     * note: the ui doesn't have to adjust itself to this
     * size, this is just to be taken as a hint.
     * ui must return the actual size of the video output
     * area and the video output driver will do its best
     * to adjust the video frames to that size (while
     * preserving aspect ratio and stuff).
     *    dest_x, dest_y: offset inside window
     *    dest_width, dest_height: available drawing space
     *    dest_pixel_aspect: display pixel aspect
     *    win_x, win_y: window absolute screen position
     * NOTE: Semantics has changed: video_width and video_height
     * are no longer pixel aspect corrected. Get the old semantics
     * in the UI with
     *   *dest_pixel_aspect = display_pixel_aspect;
     *   if (video_pixel_aspect >= display_pixel_aspect)
     *     video_width  = video_width * video_pixel_aspect / display_pixel_aspect + .5;
     *   else
     *     video_height = video_height * display_pixel_aspect / video_pixel_aspect + .5;
      }

    Pxcb_visual_t = ^xcb_visual_t;
    xcb_visual_t = record
        connection : pointer;
        screen : pointer;
        window : dword;
        user_data : pointer;
        dest_size_cb : procedure (var user_data:pointer; video_width:longint; video_height:longint; video_pixel_aspect:double; var dest_width:longint; 
                      var dest_height:longint; var dest_pixel_aspect:double);cdecl;
        frame_output_cb : procedure (var user_data:pointer; video_width:longint; video_height:longint; video_pixel_aspect:double; var dest_x:longint; 
                      var dest_y:longint; var dest_width:longint; var dest_height:longint; var dest_pixel_aspect:double; var win_x:longint; 
                      var win_y:longint);
      end;
  {*************************************************
   * XINE_VO_RAW struct definitions
   ************************************************ }
  {  frame_format definitions  }

  const
    XINE_VORAW_YV12 = 1;    
    XINE_VORAW_YUY2 = 2;    
    XINE_VORAW_RGB = 4;    
  {  maximum number of overlays the raw driver can handle  }
    XINE_VORAW_MAX_OVL = 16;    
  { raw_overlay_t struct used in raw_overlay_cb callback  }
  { overlay's width and height  }
  { overlay's top-left display position  }

{$ifdef WIN32}
  {
   * this is the visual data struct any win32 gui should supply
   * (pass this to init_video_out_plugin or the xine_load_video_output_plugin
   * utility function)
    }
  { handle of window associated with primary surface  }
  { handle of windows application instance  }
  { rect of window client points translated to screen 
                           * cooridnates  }
  { is window fullscreen  }
  { window brush for background color  }
  { window brush color key  }
  {
   * constants for gui_data_exchange's data_type parameter
    }

  const
    GUI_WIN32_MOVED_OR_RESIZED = 0;    
{$endif}
  { WIN32  }
  {
   * "type" constants for xine_port_send_gui_data(...)
    }
  { Drawable data  }

  const
    XINE_GUI_SEND_DRAWABLE_CHANGED = 2;    
  { xevent *data  }
    XINE_GUI_SEND_EXPOSE_EVENT = 3;    
  { x11_rectangle_t *data  }
    XINE_GUI_SEND_TRANSLATE_GUI_TO_VIDEO = 4;    
  { int data  }
    XINE_GUI_SEND_VIDEOWIN_VISIBLE = 5;    
  { *data contains chosen visual, select a new one or change it to NULL
   * to indicate the visual to use or that no visual will work  }
  { XVisualInfo **data  }
    XINE_GUI_SEND_SELECT_VISUAL = 8;    
  { Gui is about to destroy drawable  }
    XINE_GUI_SEND_WILL_DESTROY_DRAWABLE = 9;    
  {********************************************************************
   * xine health check stuff                                           *
   ******************************************************************** }
    XINE_HEALTH_CHECK_OK = 0;    
    XINE_HEALTH_CHECK_FAIL = 1;    
    XINE_HEALTH_CHECK_UNSUPPORTED = 2;    
    XINE_HEALTH_CHECK_NO_SUCH_CHECK = 3;    
    CHECK_KERNEL = 0;    
    CHECK_MTRR = 1;    
    CHECK_CDROM = 2;    
    CHECK_DVDROM = 3;    
    CHECK_DMA = 4;    
    CHECK_X = 5;    
    CHECK_XV = 6;    
(* Const before type ignored *)
(* Const before type ignored *)

  var
    xine_health_check : function(_para1:Pxine_health_check_t; check_num:longint):Pxine_health_check_t;cdecl;
  {********************************************************************
   * configuration system                                              *
   ******************************************************************** }
  {
   * config entry data types
    }

  const
    XINE_CONFIG_TYPE_UNKNOWN = 0;    
    XINE_CONFIG_TYPE_RANGE = 1;    
    XINE_CONFIG_TYPE_STRING = 2;    
    XINE_CONFIG_TYPE_ENUM = 3;    
    XINE_CONFIG_TYPE_NUM = 4;    
    XINE_CONFIG_TYPE_BOOL = 5;    
  { For the string type (1.1.4 and later). These are stored in num_value.  }    XINE_CONFIG_STRING_IS_STRING = 0;    
    XINE_CONFIG_STRING_IS_FILENAME = 1;    
    XINE_CONFIG_STRING_IS_DEVICE_NAME = 2;    
    XINE_CONFIG_STRING_IS_DIRECTORY_NAME = 3;    


  var
    xine_config_register_string : function(var self:xine_s; key:Pchar; def_value:Pchar; description:Pchar; help:Pchar;
      exp_level:longint; changed_cb:xine_config_cb_t; var cb_data:pointer):Pchar;cdecl;
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  { XINE_CONFIG_STRING_IS_*  }
(* Const before type ignored *)
(* Const before type ignored *)
    xine_config_register_filename : function(var self:xine_s; key:Pchar; def_value:Pchar; req_type:longint; description:Pchar;
      help:Pchar; exp_level:longint; changed_cb:xine_config_cb_t; var cb_data:pointer):Pchar;cdecl;
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
    xine_config_register_range : function(var self:xine_s; key:Pchar; def_value:longint; min:longint; max:longint;
      description:Pchar; help:Pchar; exp_level:longint; changed_cb:xine_config_cb_t; var cb_data:pointer):longint;cdecl;
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
    xine_config_register_enum : function(var self:xine_s; key:Pchar; def_value:longint; values:PPchar; description:Pchar;
      help:Pchar; exp_level:longint; changed_cb:xine_config_cb_t; var cb_data:pointer):longint;cdecl;
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
    xine_config_register_num : function(var self:xine_s; key:Pchar; def_value:longint; description:Pchar; help:Pchar;
      exp_level:longint; changed_cb:xine_config_cb_t; var cb_data:pointer):longint;cdecl;
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
    xine_config_register_bool : function(var self:xine_s; key:Pchar; def_value:longint; description:Pchar; help:Pchar;
      exp_level:longint; changed_cb:xine_config_cb_t; var cb_data:pointer):longint;cdecl;
  {
   * the following functions will copy data from the internal xine_config
   * data database to the xine_cfg_entry_t *entry you provide
   *
   * they return 1 on success, 0 on failure
    }
  { get first config item  }
    xine_config_get_first_entry : function(var self:xine_s; var entry:xine_cfg_entry_s):longint;cdecl;
  { get next config item (iterate through the items)  }
    xine_config_get_next_entry : function(var self:xine_s; var entry:xine_cfg_entry_s):longint;cdecl;
  { search for a config entry by key  }
(* Const before type ignored *)
    xine_config_lookup_entry : function(var self:xine_s; key:Pchar; var entry:xine_cfg_entry_s):longint;cdecl;
  {
   * update a config entry (which was returned from lookup_entry() )
   *
   * xine will make a deep copy of the data in the entry into its internal
   * config database.
    }
(* Const before type ignored *)
    xine_config_update_entry : procedure(var self:xine_s; var entry:xine_cfg_entry_s);cdecl;
  {
   * translation of old configuration entry names
    }
(* Const before type ignored *)

  type
    Pxine_config_entry_translation_t = ^xine_config_entry_translation_t;
    xine_config_entry_translation_t = record
        old_name : Pchar;
        new_name : Pchar;
      end;
(* Const before type ignored *)

  var
    xine_config_set_translation_user : procedure(_para1:Pxine_config_entry_translation_t);cdecl;
  {
   * load/save config data from/to afile (e.g. $HOME/.xine/config)
    }
(* Const before type ignored *)
    xine_config_load : procedure(var self:xine_s; cfg_filename:Pchar);cdecl;
(* Const before type ignored *)
    xine_config_save : procedure(var self:xine_s; cfg_filename:Pchar);cdecl;
    xine_config_reset : procedure(var self:xine_s);cdecl;
  {********************************************************************
   * asynchroneous xine event mechanism                                *
   ******************************************************************** }
  {
   * to receive events you have to register an event queue with
   * the xine engine (xine_event_new_queue, see below).
   *
   * then you can either
   * 1) check for incoming events regularly (xine_event_get/wait),
   *    process them and free them using xine_event_free
   * 2) use xine_event_create_listener_thread and specify a callback
   *    which will then be called for each event
   *
   * to send events to every module listening you don't need
   * to register an event queue but simply call xine_event_send.
   *
   * front ends should listen for one of MRL_REFERENCE and MRL_REFERENCE_EXT
   * since both will be sent for compatibility reasons
    }
  { event types  }
  { frontend can e.g. move on to next playlist entry  }
  const
    XINE_EVENT_UI_PLAYBACK_FINISHED = 1;    
  { inform ui that new channel info is available  }    XINE_EVENT_UI_CHANNELS_CHANGED = 2;    
  { request title display change in ui  }    XINE_EVENT_UI_SET_TITLE = 3;    
  { message (dialog) for the ui to display  }    XINE_EVENT_UI_MESSAGE = 4;    
  { e.g. aspect ratio change during dvd playback  }    XINE_EVENT_FRAME_FORMAT_CHANGE = 5;    
  { report current audio level (l/r/mute)  }    XINE_EVENT_AUDIO_LEVEL = 6;    
  { last event sent when stream is disposed  }    XINE_EVENT_QUIT = 7;    
  { index creation/network connections  }    XINE_EVENT_PROGRESS = 8;    
  { (deprecated) demuxer->frontend: MRL reference(s) for the real stream  }    XINE_EVENT_MRL_REFERENCE = 9;    
  { number of buttons for interactive menus  }    XINE_EVENT_UI_NUM_BUTTONS = 10;    
  { the mouse pointer enter/leave a button  }    XINE_EVENT_SPU_BUTTON = 11;    
  { number of dropped frames is too high  }    XINE_EVENT_DROPPED_FRAMES = 12;    
  { demuxer->frontend: MRL reference(s) for the real stream  }    XINE_EVENT_MRL_REFERENCE_EXT = 13;    
  { report current audio amp level (l/r/mute)  }    XINE_EVENT_AUDIO_AMP_LEVEL = 14;    
  { nbc buffer status  }    XINE_EVENT_NBC_STATS = 15;    
  { input events coming from frontend  }
    XINE_EVENT_INPUT_MOUSE_BUTTON = 101;    
    XINE_EVENT_INPUT_MOUSE_MOVE = 102;    
    XINE_EVENT_INPUT_MENU1 = 103;    
    XINE_EVENT_INPUT_MENU2 = 104;    
    XINE_EVENT_INPUT_MENU3 = 105;    
    XINE_EVENT_INPUT_MENU4 = 106;    
    XINE_EVENT_INPUT_MENU5 = 107;    
    XINE_EVENT_INPUT_MENU6 = 108;    
    XINE_EVENT_INPUT_MENU7 = 109;    
    XINE_EVENT_INPUT_UP = 110;    
    XINE_EVENT_INPUT_DOWN = 111;    
    XINE_EVENT_INPUT_LEFT = 112;    
    XINE_EVENT_INPUT_RIGHT = 113;    
    XINE_EVENT_INPUT_SELECT = 114;    
    XINE_EVENT_INPUT_NEXT = 115;    
    XINE_EVENT_INPUT_PREVIOUS = 116;    
    XINE_EVENT_INPUT_ANGLE_NEXT = 117;    
    XINE_EVENT_INPUT_ANGLE_PREVIOUS = 118;    
    XINE_EVENT_INPUT_BUTTON_FORCE = 119;    
    XINE_EVENT_INPUT_NUMBER_0 = 120;    
    XINE_EVENT_INPUT_NUMBER_1 = 121;    
    XINE_EVENT_INPUT_NUMBER_2 = 122;    
    XINE_EVENT_INPUT_NUMBER_3 = 123;    
    XINE_EVENT_INPUT_NUMBER_4 = 124;    
    XINE_EVENT_INPUT_NUMBER_5 = 125;    
    XINE_EVENT_INPUT_NUMBER_6 = 126;    
    XINE_EVENT_INPUT_NUMBER_7 = 127;    
    XINE_EVENT_INPUT_NUMBER_8 = 128;    
    XINE_EVENT_INPUT_NUMBER_9 = 129;    
    XINE_EVENT_INPUT_NUMBER_10_ADD = 130;    
  { specific event types  }
    XINE_EVENT_SET_V4L2 = 200;    
    XINE_EVENT_PVR_SAVE = 201;    
    XINE_EVENT_PVR_REPORT_NAME = 202;    
    XINE_EVENT_PVR_REALTIME = 203;    
    XINE_EVENT_PVR_PAUSE = 204;    
    XINE_EVENT_SET_MPEG_DATA = 205;    
  { VDR specific event types  }
    XINE_EVENT_VDR_RED = 300;    
    XINE_EVENT_VDR_GREEN = 301;    
    XINE_EVENT_VDR_YELLOW = 302;    
    XINE_EVENT_VDR_BLUE = 303;    
    XINE_EVENT_VDR_PLAY = 304;    
    XINE_EVENT_VDR_PAUSE = 305;    
    XINE_EVENT_VDR_STOP = 306;    
    XINE_EVENT_VDR_RECORD = 307;    
    XINE_EVENT_VDR_FASTFWD = 308;    
    XINE_EVENT_VDR_FASTREW = 309;    
    XINE_EVENT_VDR_POWER = 310;    
    XINE_EVENT_VDR_CHANNELPLUS = 311;    
    XINE_EVENT_VDR_CHANNELMINUS = 312;    
    XINE_EVENT_VDR_SCHEDULE = 313;    
    XINE_EVENT_VDR_CHANNELS = 314;    
    XINE_EVENT_VDR_TIMERS = 315;    
    XINE_EVENT_VDR_RECORDINGS = 316;    
    XINE_EVENT_VDR_SETUP = 317;    
    XINE_EVENT_VDR_COMMANDS = 318;    
    XINE_EVENT_VDR_BACK = 319;    
    XINE_EVENT_VDR_USER1 = 320;    
    XINE_EVENT_VDR_USER2 = 321;    
    XINE_EVENT_VDR_USER3 = 322;    
    XINE_EVENT_VDR_USER4 = 323;    
    XINE_EVENT_VDR_USER5 = 324;    
    XINE_EVENT_VDR_USER6 = 325;    
    XINE_EVENT_VDR_USER7 = 326;    
    XINE_EVENT_VDR_USER8 = 327;    
    XINE_EVENT_VDR_USER9 = 328;    
    XINE_EVENT_VDR_VOLPLUS = 329;    
    XINE_EVENT_VDR_VOLMINUS = 330;    
    XINE_EVENT_VDR_MUTE = 331;    
    XINE_EVENT_VDR_AUDIO = 332;    
    XINE_EVENT_VDR_INFO = 333;    
    XINE_EVENT_VDR_CHANNELPREVIOUS = 334;    
    XINE_EVENT_VDR_SUBTITLES = 335;    
  { some space for further keys  }
    XINE_EVENT_VDR_SETVIDEOWINDOW = 350;    
    XINE_EVENT_VDR_FRAMESIZECHANGED = 351;    
    XINE_EVENT_VDR_SELECTAUDIO = 352;    
    XINE_EVENT_VDR_TRICKSPEEDMODE = 353;    
    XINE_EVENT_VDR_PLUGINSTARTED = 354;    
    XINE_EVENT_VDR_DISCONTINUITY = 355;    
  { events generated from post plugins  }
    XINE_EVENT_POST_TVTIME_FILMMODE_CHANGE = 400;    
  {
   * xine event struct
    }
  { event type (constants see above)  }
  { stream this event belongs to      }
  { contents depending on type  }
  { you do not have to provide this, it will be filled in by xine_event_send()  }
  { timestamp of event creation  }

  type
  {
   * UI event dynamic data - send information to/from UI.
    }
  { might be longer  }
  {
   * configuration options for plugins that can do a kind of mpeg encoding
   * note: highly experimental api :)
    }
  { mpeg2 parameters  }
  { 1 = vbr, 0 = cbr  }
  { mean (target) bitrate in kbps }
  { peak (max) bitrate in kbps  }
  { GOP size in frames  }
  { open/closed GOP  }
  { number of B frames to use  }
  { XINE_VO_ASPECT_xxx  }
  { let some spare space so we can add new fields without breaking
     * binary api compatibility.
      }

    Pxine_set_mpeg_data_t = ^xine_set_mpeg_data_t;
    xine_set_mpeg_data_t = record
        bitrate_vbr : longint;
        bitrate_mean : longint;
        bitrate_peak : longint;
        gop_size : longint;
        gop_closure : longint;
        b_frames : longint;
        aspect_ratio : longint;
        spare : array[0..19] of uint32_t;
      end;
  { 0 leave, 1 enter  }
  { button number  }

    Pxine_spu_button_t = ^xine_spu_button_t;
    xine_spu_button_t = record
        direction : longint;
        button : int32_t;
      end;
{$ifdef XINE_ENABLE_EXPERIMENTAL_FEATURES}
  { 
   * ask pvr to save (ie. do not discard) the current session
   * see comments on input_pvr.c to understand how it works.
    }
  { mode values:
     * -1 = do nothing, just set the name
     * 0 = truncate current session and save from now on
     * 1 = save from last sync point
     * 2 = save everything on current session
      }
  { name for saving, might be longer  }

    Pxine_pvr_save_data_t = ^xine_pvr_save_data_t;
    xine_pvr_save_data_t = record
        mode : longint;
        id : longint;
        name : array[0..255] of char;
      end;
  { mode values:
     * 0 = non realtime
     * 1 = realtime
      }

    Pxine_pvr_realtime_t = ^xine_pvr_realtime_t;
    xine_pvr_realtime_t = record
        mode : longint;
      end;
  { mode values:
     * 0 = playing
     * 1 = paused
      }

    Pxine_pvr_pause_t = ^xine_pvr_pause_t;
    xine_pvr_pause_t = record
        mode : longint;
      end;
{$endif}
  { event XINE_EVENT_DROPPED_FRAMES is generated if libxine detects a
   * high number of dropped frames (above configured thresholds). it can
   * be used by the front end to warn about performance problems.
    }
  { these values are given for 1000 frames delivered  }
  { (that is, divide by 10 to get percentages)  }

  type
    Pxine_dropped_frames_t = ^xine_dropped_frames_t;
    xine_dropped_frames_t = record
        skipped_frames : longint;
        skipped_threshold : longint;
        discarded_frames : longint;
        discarded_threshold : longint;
      end;
  {
   * Defined message types for XINE_EVENT_UI_MESSAGE
   * This is the mechanism to report async errors from engine.
   *
   * If frontend knows about the XINE_MSG_xxx type it may safely 
   * ignore the 'explanation' field and provide its own custom
   * dialogue to the 'parameters'.
   *
   * right column specifies the usual parameters.
    }
  { (messages to UI)    }
  const
    XINE_MSG_NO_ERROR = 0;    
  { (warning message)   }    XINE_MSG_GENERAL_WARNING = 1;    
  { (host name)         }    XINE_MSG_UNKNOWN_HOST = 2;    
  { (device name)       }    XINE_MSG_UNKNOWN_DEVICE = 3;    
  { none                }    XINE_MSG_NETWORK_UNREACHABLE = 4;    
  { (host name)         }    XINE_MSG_CONNECTION_REFUSED = 5;    
  { (file name or mrl)  }    XINE_MSG_FILE_NOT_FOUND = 6;    
  { (device/file/mrl)   }    XINE_MSG_READ_ERROR = 7;    
  { (library/decoder)   }    XINE_MSG_LIBRARY_LOAD_ERROR = 8;    
  { none                }    XINE_MSG_ENCRYPTED_SOURCE = 9;    
  { (security message)  }    XINE_MSG_SECURITY = 10;    
  { none                }    XINE_MSG_AUDIO_OUT_UNAVAILABLE = 11;    
  { (file name or mrl)  }    XINE_MSG_PERMISSION_ERROR = 12;    
  { file is empty       }    XINE_MSG_FILE_EMPTY = 13;    
  { opaque xine_event_queue_t  }

  var
    xine_event_new_queue : function(stream:pxine_stream_t):Pxine_event_queue_t;cdecl;
    xine_event_dispose_queue : procedure(queue:pxine_event_queue_t);cdecl;
  {
   * receive events (poll)
   *
   * use xine_event_free on the events received from these calls
   * when they're no longer needed
    }
    xine_event_get : function(var queue:xine_event_queue_s):Pxine_event_t;cdecl;
    xine_event_wait : function(var queue:xine_event_queue_s):Pxine_event_t;cdecl;
    xine_event_free : procedure(var event:xine_event_t);cdecl;
  {
   * receive events (callback)
   *
   * a thread is created which will receive all events from
   * the specified queue, call your callback on each of them
   * and will then free the event when your callback returns
   *
    }
(* Const before type ignored *)

  var
    xine_event_create_listener_thread : procedure(queue:pxine_event_queue_t; callback:xine_event_listener_cb_t;user_data:pointer);cdecl;
  {
   * send an event to all queues
   *
   * the event will be copied so you can free or reuse
   * *event as soon as xine_event_send returns.
    }
(* Const before type ignored *)
    xine_event_send : procedure(var stream:xine_stream_s; var event:xine_event_t);cdecl;
  {********************************************************************
   * OSD (on screen display)                                           *
   ******************************************************************** }

  const
    XINE_TEXT_PALETTE_SIZE = 11;    
    XINE_OSD_TEXT1 = 0*XINE_TEXT_PALETTE_SIZE;    
    XINE_OSD_TEXT2 = 1*XINE_TEXT_PALETTE_SIZE;    
    XINE_OSD_TEXT3 = 2*XINE_TEXT_PALETTE_SIZE;    
    XINE_OSD_TEXT4 = 3*XINE_TEXT_PALETTE_SIZE;    
    XINE_OSD_TEXT5 = 4*XINE_TEXT_PALETTE_SIZE;    
    XINE_OSD_TEXT6 = 5*XINE_TEXT_PALETTE_SIZE;    
    XINE_OSD_TEXT7 = 6*XINE_TEXT_PALETTE_SIZE;    
    XINE_OSD_TEXT8 = 7*XINE_TEXT_PALETTE_SIZE;    
    XINE_OSD_TEXT9 = 8*XINE_TEXT_PALETTE_SIZE;    
    XINE_OSD_TEXT10 = 9*XINE_TEXT_PALETTE_SIZE;    
  { white text, black border, transparent background   }
    XINE_TEXTPALETTE_WHITE_BLACK_TRANSPARENT = 0;    
  { white text, noborder, transparent background       }
    XINE_TEXTPALETTE_WHITE_NONE_TRANSPARENT = 1;    
  { white text, no border, translucid background       }
    XINE_TEXTPALETTE_WHITE_NONE_TRANSLUCID = 2;    
  { yellow text, black border, transparent background  }
    XINE_TEXTPALETTE_YELLOW_BLACK_TRANSPARENT = 3;    
  { freetype2 support compiled in      }    XINE_OSD_CAP_FREETYPE2 = $0001;    
  { unscaled overlays supp. by vo drv  }    XINE_OSD_CAP_UNSCALED = $0002;    


  var
    xine_osd_new : function(var self:xine_stream_s; x:longint; y:longint; width:longint; height:longint):Pxine_osd_t;cdecl;
    xine_osd_get_capabilities : function(var self:xine_osd_s):uint32_t;cdecl;
    xine_osd_draw_point : procedure(var self:xine_osd_s; x:longint; y:longint; color:longint);cdecl;
    xine_osd_draw_line : procedure(var self:xine_osd_s; x1:longint; y1:longint; x2:longint; y2:longint;
      color:longint);cdecl;
    xine_osd_draw_rect : procedure(var self:xine_osd_s; x1:longint; y1:longint; x2:longint; y2:longint;
      color:longint; filled:longint);cdecl;
  { x1 and y1 specifies the upper left corner of the text to be rendered  }
(* Const before type ignored *)
    xine_osd_draw_text : procedure(var self:xine_osd_s; x1:longint; y1:longint; text:Pchar; color_base:longint);cdecl;
    xine_osd_draw_bitmap : procedure(var self:xine_osd_s; var bitmap:uint8_t; x1:longint; y1:longint; width:longint;
      height:longint; var palette_map:uint8_t);cdecl;
  { for freetype2 fonts the height is the maximum height for the whole font and not
   * only for the specified text  }
(* Const before type ignored *)
    xine_osd_get_text_size : procedure(var self:xine_osd_s; text:Pchar; var width:longint; var height:longint);cdecl;
  { with freetype2 support compiled in, you can also specify a font file
     as 'fontname' here  }
(* Const before type ignored *)
    xine_osd_set_font : function(var self:xine_osd_s; fontname:Pchar; size:longint):longint;cdecl;
  { 
   * specifying encoding of texts
   *   ""   ... means current locale encoding (default)
   *   NULL ... means latin1
    }
(* Const before type ignored *)
    xine_osd_set_encoding : procedure(var self:xine_osd_s; encoding:Pchar);cdecl;
  { set position were overlay will be blended  }
    xine_osd_set_position : procedure(var self:xine_osd_s; x:longint; y:longint);cdecl;
    xine_osd_show : procedure(var self:xine_osd_s; vpts:int64_t);cdecl;
    xine_osd_show_unscaled : procedure(var self:xine_osd_s; vpts:int64_t);cdecl;
    xine_osd_hide : procedure(var self:xine_osd_s; vpts:int64_t);cdecl;
  { empty drawing area  }
    xine_osd_clear : procedure(var self:xine_osd_s);cdecl;
  {
   * set on existing text palette
   * (-1 to set used specified palette)
   *
   * color_base specifies the first color index to use for this text
   * palette. The OSD palette is then modified starting at this
   * color index, up to the size of the text palette.
   *
   * Use OSD_TEXT1, OSD_TEXT2, ... for some preassigned color indices.
   *
   * These palettes are not working well with the true type fonts.
   * First thing is that these fonts cannot have a border. So you get
   * the best results by loading a linearly blending palette from the
   * background (at index 0) to the forground color (at index 10).
    }
    xine_osd_set_text_palette : procedure(var self:xine_osd_s; palette_number:longint; color_base:longint);cdecl;
  { get palette (color and transparency)  }
    xine_osd_get_palette : procedure(var self:xine_osd_s; var color:uint32_t; var trans:uint8_t);cdecl;
(* Const before type ignored *)
(* Const before declarator ignored *)
(* Const before type ignored *)
(* Const before declarator ignored *)
    xine_osd_set_palette : procedure(var self:xine_osd_s; var color:uint32_t; var trans:uint8_t);cdecl;
  {
   * close osd rendering engine
   * loaded fonts are unloaded
   * osd objects are closed
    }
    xine_osd_free : procedure(var self:xine_osd_s);cdecl;
{.$endif}

procedure Loadxine;
function isXineLoaded:boolean;
procedure Freexine;

implementation

  uses
    SysUtils, dynlibs;

  var
    hlib : tlibhandle;

  function isXineLoaded: boolean;
  begin
    Result := Hlib <> 0;
  end;

  procedure Freexine;
    begin
      if hlib <> 0 then
         FreeLibrary(hlib);
      hlib:=0;
      xine_get_version_string:=nil;
      xine_get_version:=nil;
      xine_check_version:=nil;
      xine_new:=nil;
      xine_init:=nil;
      xine_open_audio_driver:=nil;
      xine_open_video_driver:=nil;
      xine_close_audio_driver:=nil;
      xine_close_video_driver:=nil;
      xine_exit:=nil;
      xine_stream_new:=nil;
      xine_stream_master_slave:=nil;
      xine_open:=nil;
      xine_play:=nil;
      xine_stop:=nil;
      xine_close:=nil;
      xine_eject:=nil;
      xine_dispose:=nil;
      xine_engine_set_param:=nil;
      xine_engine_get_param:=nil;
      xine_set_param:=nil;
      xine_get_param:=nil;
      xine_get_current_frame:=nil;
      xine_get_current_frame_s:=nil;
      xine_get_current_frame_alloc:=nil;
      xine_get_current_frame_data:=nil;
      xine_get_current_vpts:=nil;

      xine_post_init:=nil;
      xine_list_post_plugins:=nil;
      xine_list_post_plugins_typed:=nil;
      xine_post_list_inputs:=nil;
      xine_post_list_outputs:=nil;
      xine_post_input:=nil;
      xine_post_output:=nil;
      xine_post_wire:=nil;
      xine_post_wire_video_port:=nil;
      xine_post_wire_audio_port:=nil;
      xine_get_video_source:=nil;
      xine_get_audio_source:=nil;
      xine_post_dispose:=nil;
      xine_get_log_section_count:=nil;
      xine_get_log_names:=nil;
      xine_get_log:=nil;
      xine_register_log_cb:=nil;
      xine_get_error:=nil;
      xine_get_status:=nil;
      xine_get_audio_lang:=nil;
      xine_get_spu_lang:=nil;
      xine_get_pos_length:=nil;
      xine_get_stream_info:=nil;
      xine_get_meta_info:=nil;
      xine_get_browsable_input_plugin_ids:=nil;
      xine_get_browse_mrls:=nil;
      xine_get_autoplay_input_plugin_ids:=nil;
      xine_get_autoplay_mrls:=nil;
      xine_get_file_extensions:=nil;
      xine_get_mime_types:=nil;
      xine_get_demux_for_mime_type:=nil;
      xine_get_input_plugin_description:=nil;
      xine_get_demux_plugin_description:=nil;
      xine_get_spu_plugin_description:=nil;
      xine_get_audio_plugin_description:=nil;
      xine_get_video_plugin_description:=nil;
      xine_get_audio_driver_plugin_description:=nil;
      xine_get_video_driver_plugin_description:=nil;
      xine_get_post_plugin_description:=nil;
      xine_list_audio_output_plugins:=nil;
      xine_list_video_output_plugins:=nil;
      xine_list_video_output_plugins_typed:=nil;
      xine_list_demuxer_plugins:=nil;
      xine_list_input_plugins:=nil;
      xine_list_spu_plugins:=nil;
      xine_list_audio_decoder_plugins:=nil;
      xine_list_video_decoder_plugins:=nil;
      xine_plugins_garbage_collector:=nil;
      xine_port_send_gui_data:=nil;
      xine_health_check:=nil;
      xine_config_register_string:=nil;
      xine_config_register_filename:=nil;
      xine_config_register_range:=nil;
      xine_config_register_enum:=nil;
      xine_config_register_num:=nil;
      xine_config_register_bool:=nil;
      xine_config_get_first_entry:=nil;
      xine_config_get_next_entry:=nil;
      xine_config_lookup_entry:=nil;
      xine_config_update_entry:=nil;
      xine_config_set_translation_user:=nil;
      xine_config_load:=nil;
      xine_config_save:=nil;
      xine_config_reset:=nil;
      xine_event_new_queue:=nil;
      xine_event_dispose_queue:=nil;
      xine_event_get:=nil;
      xine_event_wait:=nil;
      xine_event_free:=nil;
      xine_event_create_listener_thread:=nil;
      xine_event_send:=nil;
      xine_osd_new:=nil;
      xine_osd_get_capabilities:=nil;
      xine_osd_draw_point:=nil;
      xine_osd_draw_line:=nil;
      xine_osd_draw_rect:=nil;
      xine_osd_draw_text:=nil;
      xine_osd_draw_bitmap:=nil;
      xine_osd_get_text_size:=nil;
      xine_osd_set_font:=nil;
      xine_osd_set_encoding:=nil;
      xine_osd_set_position:=nil;
      xine_osd_show:=nil;
      xine_osd_show_unscaled:=nil;
      xine_osd_hide:=nil;
      xine_osd_clear:=nil;
      xine_osd_set_text_palette:=nil;
      xine_osd_get_palette:=nil;
      xine_osd_set_palette:=nil;
      xine_osd_free:=nil;
    end;


  procedure Loadxine;
    begin
      Freexine;
      hlib:=LoadLibrary(External_library);
      if hlib=0 then
        raise Exception.Create(format('Could not load library: %s',[External_library]));

      pointer(xine_get_version_string):=GetProcAddress(hlib,'xine_get_version_string');
      pointer(xine_get_version):=GetProcAddress(hlib,'xine_get_version');
      pointer(xine_check_version):=GetProcAddress(hlib,'xine_check_version');
      pointer(xine_new):=GetProcAddress(hlib,'xine_new');
      pointer(xine_init):=GetProcAddress(hlib,'xine_init');
      pointer(xine_open_audio_driver):=GetProcAddress(hlib,'xine_open_audio_driver');
      pointer(xine_open_video_driver):=GetProcAddress(hlib,'xine_open_video_driver');
      pointer(xine_close_audio_driver):=GetProcAddress(hlib,'xine_close_audio_driver');
      pointer(xine_close_video_driver):=GetProcAddress(hlib,'xine_close_video_driver');
      pointer(xine_exit):=GetProcAddress(hlib,'xine_exit');
      pointer(xine_stream_new):=GetProcAddress(hlib,'xine_stream_new');
      pointer(xine_stream_master_slave):=GetProcAddress(hlib,'xine_stream_master_slave');
      pointer(xine_open):=GetProcAddress(hlib,'xine_open');
      pointer(xine_play):=GetProcAddress(hlib,'xine_play');
      pointer(xine_stop):=GetProcAddress(hlib,'xine_stop');
      pointer(xine_close):=GetProcAddress(hlib,'xine_close');
      pointer(xine_eject):=GetProcAddress(hlib,'xine_eject');
      pointer(xine_dispose):=GetProcAddress(hlib,'xine_dispose');
      pointer(xine_engine_set_param):=GetProcAddress(hlib,'xine_engine_set_param');
      pointer(xine_engine_get_param):=GetProcAddress(hlib,'xine_engine_get_param');
      pointer(xine_set_param):=GetProcAddress(hlib,'xine_set_param');
      pointer(xine_get_param):=GetProcAddress(hlib,'xine_get_param');
      pointer(xine_get_current_frame):=GetProcAddress(hlib,'xine_get_current_frame');
      pointer(xine_get_current_frame_s):=GetProcAddress(hlib,'xine_get_current_frame_s');
      pointer(xine_get_current_frame_alloc):=GetProcAddress(hlib,'xine_get_current_frame_alloc');
      pointer(xine_get_current_frame_data):=GetProcAddress(hlib,'xine_get_current_frame_data');
      pointer(xine_get_current_vpts):=GetProcAddress(hlib,'xine_get_current_vpts');
      pointer(xine_post_init):=GetProcAddress(hlib,'xine_post_init');
      pointer(xine_list_post_plugins):=GetProcAddress(hlib,'xine_list_post_plugins');
      pointer(xine_list_post_plugins_typed):=GetProcAddress(hlib,'xine_list_post_plugins_typed');
      pointer(xine_post_list_inputs):=GetProcAddress(hlib,'xine_post_list_inputs');
      pointer(xine_post_list_outputs):=GetProcAddress(hlib,'xine_post_list_outputs');
      pointer(xine_post_input):=GetProcAddress(hlib,'xine_post_input');
      pointer(xine_post_output):=GetProcAddress(hlib,'xine_post_output');
      pointer(xine_post_wire):=GetProcAddress(hlib,'xine_post_wire');
      pointer(xine_post_wire_video_port):=GetProcAddress(hlib,'xine_post_wire_video_port');
      pointer(xine_post_wire_audio_port):=GetProcAddress(hlib,'xine_post_wire_audio_port');
      pointer(xine_get_video_source):=GetProcAddress(hlib,'xine_get_video_source');
      pointer(xine_get_audio_source):=GetProcAddress(hlib,'xine_get_audio_source');
      pointer(xine_post_dispose):=GetProcAddress(hlib,'xine_post_dispose');
      pointer(xine_get_log_section_count):=GetProcAddress(hlib,'xine_get_log_section_count');
      pointer(xine_get_log_names):=GetProcAddress(hlib,'xine_get_log_names');
      pointer(xine_get_log):=GetProcAddress(hlib,'xine_get_log');
      pointer(xine_register_log_cb):=GetProcAddress(hlib,'xine_register_log_cb');
      pointer(xine_get_error):=GetProcAddress(hlib,'xine_get_error');
      pointer(xine_get_status):=GetProcAddress(hlib,'xine_get_status');
      pointer(xine_get_audio_lang):=GetProcAddress(hlib,'xine_get_audio_lang');
      pointer(xine_get_spu_lang):=GetProcAddress(hlib,'xine_get_spu_lang');
      pointer(xine_get_pos_length):=GetProcAddress(hlib,'xine_get_pos_length');
      pointer(xine_get_stream_info):=GetProcAddress(hlib,'xine_get_stream_info');
      pointer(xine_get_meta_info):=GetProcAddress(hlib,'xine_get_meta_info');
      pointer(xine_get_browsable_input_plugin_ids):=GetProcAddress(hlib,'xine_get_browsable_input_plugin_ids');
      pointer(xine_get_browse_mrls):=GetProcAddress(hlib,'xine_get_browse_mrls');
      pointer(xine_get_autoplay_input_plugin_ids):=GetProcAddress(hlib,'xine_get_autoplay_input_plugin_ids');
      pointer(xine_get_autoplay_mrls):=GetProcAddress(hlib,'xine_get_autoplay_mrls');
      pointer(xine_get_file_extensions):=GetProcAddress(hlib,'xine_get_file_extensions');
      pointer(xine_get_mime_types):=GetProcAddress(hlib,'xine_get_mime_types');
      pointer(xine_get_demux_for_mime_type):=GetProcAddress(hlib,'xine_get_demux_for_mime_type');
      pointer(xine_get_input_plugin_description):=GetProcAddress(hlib,'xine_get_input_plugin_description');
      pointer(xine_get_demux_plugin_description):=GetProcAddress(hlib,'xine_get_demux_plugin_description');
      pointer(xine_get_spu_plugin_description):=GetProcAddress(hlib,'xine_get_spu_plugin_description');
      pointer(xine_get_audio_plugin_description):=GetProcAddress(hlib,'xine_get_audio_plugin_description');
      pointer(xine_get_video_plugin_description):=GetProcAddress(hlib,'xine_get_video_plugin_description');
      pointer(xine_get_audio_driver_plugin_description):=GetProcAddress(hlib,'xine_get_audio_driver_plugin_description');
      pointer(xine_get_video_driver_plugin_description):=GetProcAddress(hlib,'xine_get_video_driver_plugin_description');
      pointer(xine_get_post_plugin_description):=GetProcAddress(hlib,'xine_get_post_plugin_description');
      pointer(xine_list_audio_output_plugins):=GetProcAddress(hlib,'xine_list_audio_output_plugins');
      pointer(xine_list_video_output_plugins):=GetProcAddress(hlib,'xine_list_video_output_plugins');
      pointer(xine_list_video_output_plugins_typed):=GetProcAddress(hlib,'xine_list_video_output_plugins_typed');
      pointer(xine_list_demuxer_plugins):=GetProcAddress(hlib,'xine_list_demuxer_plugins');
      pointer(xine_list_input_plugins):=GetProcAddress(hlib,'xine_list_input_plugins');
      pointer(xine_list_spu_plugins):=GetProcAddress(hlib,'xine_list_spu_plugins');
      pointer(xine_list_audio_decoder_plugins):=GetProcAddress(hlib,'xine_list_audio_decoder_plugins');
      pointer(xine_list_video_decoder_plugins):=GetProcAddress(hlib,'xine_list_video_decoder_plugins');
      pointer(xine_plugins_garbage_collector):=GetProcAddress(hlib,'xine_plugins_garbage_collector');
      pointer(xine_port_send_gui_data):=GetProcAddress(hlib,'xine_port_send_gui_data');
      pointer(xine_health_check):=GetProcAddress(hlib,'xine_health_check');
      pointer(xine_config_register_string):=GetProcAddress(hlib,'xine_config_register_string');
      pointer(xine_config_register_filename):=GetProcAddress(hlib,'xine_config_register_filename');
      pointer(xine_config_register_range):=GetProcAddress(hlib,'xine_config_register_range');
      pointer(xine_config_register_enum):=GetProcAddress(hlib,'xine_config_register_enum');
      pointer(xine_config_register_num):=GetProcAddress(hlib,'xine_config_register_num');
      pointer(xine_config_register_bool):=GetProcAddress(hlib,'xine_config_register_bool');
      pointer(xine_config_get_first_entry):=GetProcAddress(hlib,'xine_config_get_first_entry');
      pointer(xine_config_get_next_entry):=GetProcAddress(hlib,'xine_config_get_next_entry');
      pointer(xine_config_lookup_entry):=GetProcAddress(hlib,'xine_config_lookup_entry');
      pointer(xine_config_update_entry):=GetProcAddress(hlib,'xine_config_update_entry');
      pointer(xine_config_set_translation_user):=GetProcAddress(hlib,'xine_config_set_translation_user');
      pointer(xine_config_load):=GetProcAddress(hlib,'xine_config_load');
      pointer(xine_config_save):=GetProcAddress(hlib,'xine_config_save');
      pointer(xine_config_reset):=GetProcAddress(hlib,'xine_config_reset');
      pointer(xine_event_new_queue):=GetProcAddress(hlib,'xine_event_new_queue');
      pointer(xine_event_dispose_queue):=GetProcAddress(hlib,'xine_event_dispose_queue');
      pointer(xine_event_get):=GetProcAddress(hlib,'xine_event_get');
      pointer(xine_event_wait):=GetProcAddress(hlib,'xine_event_wait');
      pointer(xine_event_free):=GetProcAddress(hlib,'xine_event_free');
      pointer(xine_event_create_listener_thread):=GetProcAddress(hlib,'xine_event_create_listener_thread');
      pointer(xine_event_send):=GetProcAddress(hlib,'xine_event_send');
      pointer(xine_osd_new):=GetProcAddress(hlib,'xine_osd_new');
      pointer(xine_osd_get_capabilities):=GetProcAddress(hlib,'xine_osd_get_capabilities');
      pointer(xine_osd_draw_point):=GetProcAddress(hlib,'xine_osd_draw_point');
      pointer(xine_osd_draw_line):=GetProcAddress(hlib,'xine_osd_draw_line');
      pointer(xine_osd_draw_rect):=GetProcAddress(hlib,'xine_osd_draw_rect');
      pointer(xine_osd_draw_text):=GetProcAddress(hlib,'xine_osd_draw_text');
      pointer(xine_osd_draw_bitmap):=GetProcAddress(hlib,'xine_osd_draw_bitmap');
      pointer(xine_osd_get_text_size):=GetProcAddress(hlib,'xine_osd_get_text_size');
      pointer(xine_osd_set_font):=GetProcAddress(hlib,'xine_osd_set_font');
      pointer(xine_osd_set_encoding):=GetProcAddress(hlib,'xine_osd_set_encoding');
      pointer(xine_osd_set_position):=GetProcAddress(hlib,'xine_osd_set_position');
      pointer(xine_osd_show):=GetProcAddress(hlib,'xine_osd_show');
      pointer(xine_osd_show_unscaled):=GetProcAddress(hlib,'xine_osd_show_unscaled');
      pointer(xine_osd_hide):=GetProcAddress(hlib,'xine_osd_hide');
      pointer(xine_osd_clear):=GetProcAddress(hlib,'xine_osd_clear');
      pointer(xine_osd_set_text_palette):=GetProcAddress(hlib,'xine_osd_set_text_palette');
      pointer(xine_osd_get_palette):=GetProcAddress(hlib,'xine_osd_get_palette');
      pointer(xine_osd_set_palette):=GetProcAddress(hlib,'xine_osd_set_palette');
      pointer(xine_osd_free):=GetProcAddress(hlib,'xine_osd_free');
    end;


initialization
  hlib := 0;
finalization

end.