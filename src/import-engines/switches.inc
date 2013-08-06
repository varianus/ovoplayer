// compiler/IDE dependent config
{$IFDEF FPC}
  {$H+} // use AnsiString instead of ShortString as String-type (default in Delphi)

  // if -dDEBUG is specified on the command-line, FPC uses some default
  // compiler-flags specified in fpc.cfg -> use -dDEBUG_MODE instead
  {$IFDEF DEBUG_MODE}
    {$DEFINE DEBUG}
  {$ENDIF}

  {$DEFINE HasInline}
{$ENDIF}


// platform dependent config
{$IF Defined(MSWINDOWS)}
  // include defines but no constants
  {$I config-win.inc}

  // enable debug-mode. For development only!
  {.$DEFINE DEBUG}
  {$IFDEF DEBUG}
    // windows apps are either GUI- or console-apps. Console-apps will open
    // an additional console-window for output. For development only!
    {$DEFINE CONSOLE}
  {$ENDIF}

  {$DEFINE HaveBASS}
  {$DEFINE UseMIDIPort}
{$ELSEIF Defined(DARWIN)}
  // include defines but no constants
  {$I config-darwin.inc}

  // enable debug-mode. For development only!
  {.$DEFINE DEBUG}
  {$DEFINE CONSOLE}
  {.$DEFINE HaveBASS}
  {$DEFINE UTF8_FILENAMES}
{$ELSEIF Defined(UNIX)}
  // include defines but no constants
  {$IF Defined(FREEBSD)}
    {$I config-freebsd.inc}
  {$ELSEIF Defined(Linux)}
    {$I config-linux.inc}
  {$IFEND}

  // use "configure --enable-debug", "make debug" or
  // the command-line parameter "-debug" instead of defining DEBUG directly
  {.$DEFINE DEBUG}
  // linux apps are always console-apps so leave this defined.
  {$DEFINE CONSOLE}
{$IFEND}

// audio config
  {$DEFINE UseFFmpegDecoder}
  {$DEFINE UseFFmpegResample}
  {$DEFINE UseFFmpegVideo}


// specify some useful defines

{$IF Defined(UseFFmpegVideo) or Defined(UseFFmpegDecoder)}
  {$DEFINE UseFFmpeg}
{$IFEND}
