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

unit Equalizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Const
  EQCounter = 10;

type
  RBandInfo = record
    Value : Double;
    Freq: Double;
  end;
  ARBandinfo = array of RBandInfo;

  { IEqualizer }
  IEqualizer = interface
    ['{95CADD9D-8BDD-4527-8660-53537B3052F5}']
    function getActiveEQ: boolean;
    function GetBandInfo: ARBandInfo;
    function GetBandValue(Index: Integer): Double;
    procedure SetActiveEQ(AValue: boolean);
    procedure SetBandValue(Index: Integer; AValue: Double);
//
    property BandInfo: ARBandInfo read GetBandInfo;
    Property ActiveEQ: boolean read getActiveEQ write SetActiveEQ;
    Property BandValue[Index:Integer]: Double read GetBandValue write SetBandValue;
    Procedure EQApply;
  end;


  RPreset = record
    Name: string;
    Values : array [0..EQCounter -1] of double;
  end;

const
  PRESET_COUNT = 19;

var
  ARPreset : array [0..PRESET_COUNT -1] of RPreset = (
      (Name: 'default'; Values: (0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)),
      (Name: 'classical'; Values: (0.0,0.0,0.0,0.0,0.0,-4.4,-4.4,-4.4,-5.8,-6)),
      (Name: 'club'; Values: (0.0,0.0,4.8,3.3,3.3,3.3,1.9,0.0,0.0,0.0)),
      (Name: 'dance'; Values: (5.7,4.3,1.4,0.0,0.0,-3.4,-4.4,-4.3,0.0,0.0)),
      (Name: 'full bass'; Values: (-4.8,5.7,5.7,3.3,1.0,-2.4,-4.8,-6.3,-6.7,-6.7)),
      (Name: 'full bass and treble'; Values: (4.3,3.3,0.0,-4.4,-2.9,1.0,4.8,6.7,7.2,7.2)),
      (Name: 'full treble'; Values: (-5.8,-5.8,-5.8,-2.4,1.4,6.7,9.6,9.6,9.6,10.1)),
      (Name: 'headphones'; Values: (2.8,6.7,3.3,-2.0,-1.4,1.0,2.8,5.7,7.7,8.6)),
      (Name: 'large hall'; Values: (6.2,6.2,3.3,3.3,0.0,-2.9,-2.9,-2.9,0.0,0.0)),
      (Name: 'live'; Values: (-2.9,0.0,2.4,3.3,3.3,3.3,2.4,1.4,1.4,1.4)),
      (Name: 'party'; Values: (4.3,4.3,0.0,0.0,0.0,0.0,0.0,0.0,4.3,4.3)),
      (Name: 'pop'; Values: (-1.0,2.8,4.3,4.8,3.3,0.0,-1.4,-1.4,-1.0,-1.0)),
      (Name: 'reggae'; Values: (0.0,0.0,0.0,-3.4,0.0,3.8,3.8,0.0,0.0,0.0)),
      (Name: 'rock'; Values: (4.8,2.8,-3.4,-4.8,-2.0,2.4,5.3,6.7,6.7,6.7)),
      (Name: 'ska'; Values: (-1.4,-2.9,-2.4,0.0,2.4,3.3,5.3,5.7,6.7,5.8)),
      (Name: 'soft'; Values: (2.8,1.0,0.0,-1.4,0.0,2.4,4.8,5.7,6.7,7.2)),
      (Name: 'soft rock'; Values: (2.4,2.4,1.4,0.0,-2.4,-3.4,-2.0,0.0,1.4,5.3)),
      (Name: 'techno'; Values: (4.8,3.3,0.0,-3.4,-2.9,0.0,4.8,5.7,5.8,5.3)),
      (Name: 'user'; Values: (0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0))
    );

Procedure ApplyPreset(eq : IEqualizer; Preset:Integer);

implementation

Procedure ApplyPreset(eq : IEqualizer; Preset:Integer);
var
  i: integer;
begin
  for i := 0 to pred(EQCounter) do
    begin
      eq.BandValue[i] := ARPreset[Preset].Values[i];
    end;

  Eq.EQApply;
end;

end.

