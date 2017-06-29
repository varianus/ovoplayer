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
// Master include file for OVOPlayer application  (BACKEND)
//
// Working Audio engines
// VLC             - Videolan VLC     - Stable
// BASS            - Un4Seen          - Stable
// DSHOW           - Microsoft        - Stable, windows only
// XINE            - Xine             - Stable, minor issues
// GSTREAMER       - Gstreamer        - Stable, Linux only, needs glib
// MPLAYER         - Mplayer          - Stable, minor issues
// UOS             - Various          - Stable, minor issues
// MEDIAFOUNDATION - Microsoft        - Stable, minor seeking issues, Windows Vista and later only
//
// Engines under development
// LIBMPV          - mpv.io           - Fully working, under testing

// FFMPEG          - FFMpeg           - Working, but FFMPEG changes data structures so often that I've disabled it
// OPENSOURCELIB   - Various          - Working with major issues, lighter alternative to UOS

{$IFDEF LINUX}
//
  {$DEFINE GSTREAMER} // GStreamer need glib
  {$DEFINE VLC} //
  {$DEFINE XINE} //
  {$DEFINE BASS} //
  {$DEFINE UOS} //
  {$DEFINE LIBMPV} //
  {.$DEFINE OPENSOURCELIB} //
  {.$DEFINE FFMPEG}

{$ENDIF LINUX}

{$IFDEF WINDOWS}
  {$DEFINE DSHOW} // Work only on windows
  {$DEFINE MEDIAFOUNDATION} // Work only on windows vista and higher
  {$DEFINE VLC} //
  {$DEFINE XINE} //
  {$DEFINE BASS} //
  {$DEFINE UOS} //
  {$DEFINE LIBMPV} //
  {$DEFINE LIBZPLAY} //
  {.$DEFINE GSTREAMER} // GStreamer need glib

  {.$DEFINE OPENSOURCELIB} //
  {.$DEFINE FFMPEG} //

{$ENDIF WINDOWS}

{$IFDEF DARWIN}
  {$DEFINE VLC} //
  {$DEFINE BASS} //

  {.$DEFINE OPENSOURCELIB} //
  {.$DEFINE UOS} //
  {.$DEFINE FFMPEG}

{$ENDIF DARWIN}

{$IFDEF LINUX}
  {$DEFINE NEEDCFGSUBDIR} // Need to add a sub-directory to configuration files directory
{$ENDIF LINUX}


