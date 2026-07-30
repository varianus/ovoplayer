# Import Engines

This directory contains Pascal bindings and wrapper classes for various audio playback libraries and codecs used by OvoPlayer. These modules provide abstractions and dynamic linking to multiple audio backends, allowing OvoPlayer to support diverse audio formats and playback systems across different platforms.

## Architecture Overview

The import-engines directory is organized into several categories:

### [UOS - Universal Open Sound](https://github.com/fredvs/uos)

- **uos.pas** - The main Universal Open Sound library providing a unified interface for audio playback and format handling
- **uos_aac.pas** - AAC format decoder bindings
- **uos_libsndfile.pas** - libsndfile bindings for WAV, FLAC, and other formats
- **uos_mpg123.pas** - MPG123 bindings for MP3 format support
- **uos_opus.pas** - Opus audio codec bindings
- **uos_opusfile.pas** - OpusFile library bindings
- **uos_portaudio.pas** - PortAudio bindings for cross-platform audio output
- **uos_soundtouch.pas** - SoundTouch library bindings for tempo/pitch adjustment

### Multi-platform playback engines

- **gstreamer.pas** - GStreamer multimedia framework bindings (primarily Linux)
- **xine.pas** - Xine media engine bindings
- **PasLibVlcUnit.pas** - VLC (libvlc) library bindings
- **libmpv.pas** - libmpv media player bindings
- **lazdynamic_bass.pas** - BASS audio library bindings

### Windows only engines

- **mediadshow.pas** - DirectShow API bindings for Windows
- **mediafoundation.pas** - Media Foundation API bindings for Windows

### Experimental/incomplete engines

- **ol_classes.pas** - Base decoder and filter classes
- **ol_decoderdummy.pas** - Dummy/fallback decoder implementation
- **ol_decodermpg123.pas** - MPG123-based MP3 decoder wrapper
- **ol_decoderopus.pas** - Opus format decoder wrapper
- **ol_decodersndfile.pas** - libsndfile-based decoder wrapper
- **ol_filtervolume.pas** - Volume filter/processor
- **ol_rendererportaudio.pas** - PortAudio renderer wrapper
- **ffmpeg.pas** - FFmpeg library bindings for comprehensive audio/video format support 
- **libZplay.pas** - ZPlay audio engine (Windows only)

### Configuration & Type Definitions

- **define.inc** - Configure UOS features
- **compiler.inc** - Compiler-specific settings and conditional compilation directives

## Key Features

- **Multi-backend Support**: OvoPlayer can use different audio engines based on platform and availability
- **Dynamic Linking**: Libraries are dynamically loaded at runtime, reducing static dependencies
- **Format Support**: Comprehensive support for common audio formats (MP3, OGG Vorbis, FLAC, AAC, Opus, WAV, etc.)
- **Cross-Platform**: Abstractions handle platform differences between Linux and Windows
- **Plugin Architecture**: Decoder and filter classes allow extensible audio processing

## Usage

These modules are used internally by OvoPlayer's main audio engine to provide format support and playback capabilities. Developers working with OvoPlayer's audio system will interact with these through the main application interface rather than directly.

To add support for a new audio backend:

1. Create appropriate FFI bindings (e.g., `newlibrary.pas`)
2. Create a new TAudioEngine descendant (e.g., `audioengine_newlibrary.pas`) for interfacing the library and Ovoplayer 
3. Register the new decoder/engine in the main application, modifying `backend.inc` and `ovoplayer.lpr`

## Build Notes

- Use appropriate compiler directives in `backend.inc` to enable/disable specific backends

## License

These bindings are part of OvoPlayer and follow the same license as the main project. Note that individual libraries used have their own licenses (FFmpeg, GStreamer, libvlc, etc.) which must be respected if distributed with OvoPlayer.
