{*****************************************************************
 * Configuration file for UltraStar Deluxe 1.1
 *****************************************************************}

{* Libraries *}

{$DEFINE HaveFFmpeg}

{$IF Defined(HaveFFmpeg) and Defined(IncludeConstants)}
  av__codec = 'avcodec-54';
  LIBAVCODEC_VERSION_MAJOR   = 54;
  LIBAVCODEC_VERSION_MINOR   = 59;
  LIBAVCODEC_VERSION_RELEASE = 2;

  av__format = 'avformat-54';
  LIBAVFORMAT_VERSION_MAJOR   = 54;
  LIBAVFORMAT_VERSION_MINOR   = 29;
  LIBAVFORMAT_VERSION_RELEASE = 0;

  av__util = 'avutil-51';
  LIBAVUTIL_VERSION_MAJOR   = 51;
  LIBAVUTIL_VERSION_MINOR   = 73;
  LIBAVUTIL_VERSION_RELEASE = 100;
{$IFEND}

{$DEFINE HaveSWScale}
{$IF Defined(HaveSWScale) and Defined(IncludeConstants)}
  sw__scale = 'swscale-0';
  LIBSWSCALE_VERSION_MAJOR   = 0;
  LIBSWSCALE_VERSION_MINOR   = 10;
  LIBSWSCALE_VERSION_RELEASE = 0;
{$IFEND}

{$DEFINE HaveProjectM}
{$IF Defined(HaveProjectM) and Defined(IncludeConstants)}
  ProjectM_DataDir = 'Visuals\projectM';
  PROJECTM_VERSION_MAJOR   = 1;
  PROJECTM_VERSION_MINOR   = 10;
  PROJECTM_VERSION_RELEASE = 0;
{$IFEND}

{$UNDEF HavePortaudio}
{$IF Defined(HavePortaudio) and Defined(IncludeConstants)}
  PORTAUDIO_VERSION_MAJOR   = 19;
  PORTAUDIO_VERSION_MINOR   = 0;
  PORTAUDIO_VERSION_RELEASE = 0;
{$IFEND}

{$UNDEF HavePortmixer}

{$UNDEF HaveLibsamplerate}
{$IF Defined(HaveLibsamplerate) and Defined(IncludeConstants)}
  LIBSAMPLERATE_VERSION_MAJOR   = 0;
  LIBSAMPLERATE_VERSION_MINOR   = 1;
  LIBSAMPLERATE_VERSION_RELEASE = 3;
{$IFEND}
