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

// Master include File for OVOPlayer application
// General features
{$IF FPC_FULLVERSION >= 30005}
  {$DEFINE STACKTRACEDUMP} //  Dump stack trace on exception
{$ENDIF FPC_FULLVERSION}

{$DEFINE MULTIMEDIA_KEYS} //  Handle Multimedia Keys

// Platform-dependent features
{$IFDEF LINUX}
  {$DEFINE NEEDCFGSUBDIR} // Need to add a sub-directory to configuration files directory
  {$DEFINE ASKMMKEYSMODE} // Ask for multimedia keys capture mode
  {$DEFINE MPRIS2}        // MPRIS V2 Interface
  {$DEFINE NOTIFYDBUS}    // use DBus notification
  {$DEFINE CATCH_SIGNAL}  // try to close gracefully on linux shutdown signal
{$ENDIF LINUX}  

{$IFDEF WINDOWS}
  {$DEFINE SUPPORT_SHAPING}   // Rounded border of OSD popup, seems to work only on windows
  {$DEFINE TASKBAR_EXTENSION} // Add buttons to taskbar preview window, Win7 and higher only
{$ENDIF WINDOWS}

{$IFDEF LCLWin32}
  {$DEFINE SUPPORT_LISTBOX_HINT} // GTK2 have some problems
{$ENDIF LCLWin32}

//Other Features
{$DEFINE NETWORK_INTF} // Activate network interface for remote control
{$IFDEF NETWORK_INTF}
    {.$DEFINE NETWORK_SS} // Activate network interface for remote control (simple socket)
    {$DEFINE NETWORK_WS} // Activate network interface for remote control (web socket)
{$ENDIF NETWORK_INTF}
