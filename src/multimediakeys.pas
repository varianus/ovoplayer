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
{$I ovoplayer.inc}
unit MultimediaKeys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AudioEngine;

type

  TMultimediaKeys = class;

  { TKeyCapture }

  TKeyCapture = class
  protected
    procedure BeginGrab; virtual; abstract;
    procedure EndGrab; virtual; abstract;
  public
    Owner: TMultimediaKeys;
  end;

  TOnMMKeys = procedure(Sender: TObject; Command: TEngineCommand) of object;

  { TMultimediaKeys }

  TMultimediaKeys = class
  private
    FOnMMKey:   TOnMMKeys;
    KeyCapture: TKeyCapture;
    procedure SetOnMMKey(const AValue: TOnMMKeys);
  public
    constructor Create;
    destructor Destroy; override;
    property OnMMKey: TOnMMKeys read FOnMMKey write SetOnMMKey;

  end;

implementation

{ TMultimediaKeys }
{$I mmkeys.inc}

procedure TMultimediaKeys.SetOnMMKey(const AValue: TOnMMKeys);
begin
  if FOnMMKey = AValue then
    exit;
  FOnMMKey := AValue;
end;


end.
