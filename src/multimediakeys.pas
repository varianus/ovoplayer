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
{$I ovoplayer.inc}
unit MultimediaKeys;

interface

uses
  Classes, SysUtils, BaseTypes, coreinterfaces;

type

  TMultimediaKeys = class;

  { TKeyCapture }

  TKeyCapture = class
  protected
    procedure BeginGrab; virtual; abstract;
    procedure EndGrab; virtual; abstract;
    function GetGrabbed: Boolean; Virtual;
  public
    Owner: TMultimediaKeys;
    Property Grabbed: Boolean read GetGrabbed;
  end;

  { TMultimediaKeys }

  TMultimediaKeys = class
  private
    fMode: integer;
    KeyCapture: TKeyCapture;
    fBackEnd: IBackEnd;
  public
    constructor Create(Mode:Integer; BackEnd: IBackEnd);
    destructor Destroy; override;
    property Mode:integer read fMode;

  end;

implementation

{ TMultimediaKeys }
{$I mmkeys.inc}

function TKeyCapture.GetGrabbed: Boolean;
begin
  result:=False;
end;


end.
