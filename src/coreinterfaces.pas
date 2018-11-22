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

{$I codegen.inc}
{$I backend.inc}
unit coreinterfaces;

interface
uses
  BaseTypes, basetag;

const
  SBackEnd = '{692D792E-C98A-4F0C-9422-6AFF2A92E065}';
  SSUbject = '{A75AF920-56C3-434F-8F4A-85655EA2C51E}';
  SObserver ='{03E2942D-D961-4CF2-A339-69044C763EB4}';
type

  TChangedProperty = (cpStatus, cpVolume, cpPosition, cpMetadata, cpMute,
                      cpLooping, cpCurrentItem, cpClosing, cpPlayPos,
                      cpPlaylist);

  { IBackEnd }

  IObserver = interface
    [SObserver]
   Procedure UpdateProperty(Kind: TChangedProperty);
  end;


  { ISUbject }

  ISUbject = interface
  [SSUbject]
  Procedure Attach(observer: iObserver);
  Procedure Remove(observer: iObserver);
  Procedure Notify(Kind:  TChangedProperty);
  end;

  IBackEnd = interface(ISubject)
    [SBackEnd]
 // property Get-Set
    function GetLooping: TplRepeat;
    function GetMute: boolean;
    function GetPosition: int64;
    function GetStatus: TEngineState;
    function GetVolume: cardinal;
    procedure SetLooping(AValue: TplRepeat);
    procedure SetMute(AValue: boolean);
    procedure SetPosition(AValue: int64);
    procedure SetStatus(AValue: TEngineState);
    procedure SetVolume(AValue: cardinal);
 // Procedures
    Procedure Play(Index:integer=-1);
    Procedure Stop;
    Procedure Pause;
    Procedure UnPause;
    Procedure Next;
    Procedure Previous;
    Procedure Quit;
    procedure HandleCommand(Command: TEngineCommand; Param: integer = 0);
    function HandleExternalCommand(Command: RExternalCommand):boolean;
    Procedure OpenURI(URI: String);
    procedure Seek(AValue: int64);
    Function GetMetadata(Index:integer=-1): TCommonTags;
    Function GetCoverURL: String;
    Function GetCover(Width: integer=-1; Height:Integer=-1): String;
    Function PlayListCount : integer;
    Function GetCurrentSongIndex : integer;
    Procedure AutoSendPosEvents(Active: boolean);
    // property
    Property Status: TEngineState read GetStatus write SetStatus;
    Property Position: int64 read GetPosition write SetPosition;
    property Looping: TplRepeat read  GetLooping write SetLooping;
    Property Volume : cardinal read GetVolume write SetVolume;
    Property Muted : boolean read GetMute write SetMute;
  end;


implementation

end.

