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
unit Equalizer;

interface

uses
  Classes, SysUtils, config;

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

type
  ARPreset = array of RPreset;

  { TEqualizerParam }

  TEqualizerParam = Class(TConfigParam)
  private
    const Base = 'EqualizerPreset';
    procedure InitWithDefault;
  private
    fPresets: ARPreset;
    function GetCount: integer;
    function GetPreset(Index: integer): RPreset;
    procedure SetPreset(Index: integer; AValue: RPreset);
  protected
    Procedure InternalSave; override;
  public
    Property Preset[Index:integer]: RPreset read GetPreset write SetPreset; default;
    property Count: integer read GetCount;
    Procedure Load; override;
    Procedure ApplyPreset(eq : IEqualizer; Idx:Integer);
  end;

implementation

const
  PRESET_COUNT = 18;
var
  DefaultPreset : array [0..PRESET_COUNT -1] of RPreset = (
      (Name: 'flat'; Values: (0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)),
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
      (Name: 'techno'; Values: (4.8,3.3,0.0,-3.4,-2.9,0.0,4.8,5.7,5.8,5.3))
    );

procedure TEqualizerParam.ApplyPreset(eq: IEqualizer; Idx: Integer);
var
  i: integer;
begin
  for i := 0 to pred(EQCounter) do
    begin
      eq.BandValue[i] := fPresets[Idx].Values[i];
    end;

  Eq.EQApply;
end;

{ TEqualizerParam }

function TEqualizerParam.GetPreset(Index: integer): RPreset;
begin
  Result := fPresets[index];
end;

function TEqualizerParam.GetCount: integer;
begin
  Result := Length(fPresets);
end;

procedure TEqualizerParam.SetPreset(Index: integer; AValue: RPreset);
begin
  fPresets[index] := AValue;
end;

procedure TEqualizerParam.InternalSave;
var
  tmpSt: TStringList;
  i: integer;
  setting: TFormatSettings;
  Function DumpPreset(idx:integer):string;
   var
     j: Integer;
   begin
     result:='';
     for j := 0 to EQCounter -1 do
       Result:= Result + FloatToStrF(fPresets[idx].Values[j],fffixed, 99, 2, setting) +';';
   end;

begin
  tmpSt := TStringList.Create;
  Setting.DecimalSeparator := '.';
  try
    for i := 0 to pred(Length(fPresets)) do
      begin
        tmpst.Add(inttostr(i) +'='+
                  fPresets[i].Name +';'+
                  DumpPreset(i)
                  );
      end;
    Owner.SaveCustomParams(base, tmpSt);

  finally
    tmpSt.Free;
  end;
  Owner.Dirty:= true;
end;


procedure TEqualizerParam.Load;
var
  tmpSt: TStringList;
  info: TStringList;
  i, j: integer;
  tmp : double;
  r : RPreset;
  setting: TFormatSettings;
begin
  Setting.DecimalSeparator := '.';
  tmpSt := TStringList.Create;
  info  := TStringList.Create;
  try
   Owner.ReadCustomParams(Base, tmpSt);
   SetLength(fPresets,tmpSt.Count);

   if tmpSt.Count = 0 then
     begin
       InitWithDefault;
       Dirty := true;
       exit;
     end;

   for i := 0 to tmpSt.Count -1 do
     begin
       info.Clear;
       info.StrictDelimiter := true;
       info.Delimiter := ';';
       info.DelimitedText := tmpSt.ValueFromIndex[i];
       initialize(r);
       r.Name := info[0];
       for j := 1 to EQCounter do
         begin
           TryStrToFloat(info[j], tmp, setting);
           r.Values[j] := tmp;
         end;
       fPresets[i] := r;
     end;
  Except
    Owner.RemoveSection(Base);
  end;

  tmpSt.free;
  info.free;

end;

procedure TEqualizerParam.InitWithDefault;
var
  i: Integer;
begin
  SetLength(fPresets,PRESET_COUNT);
  for i := 0 to pred(PRESET_COUNT) do
    begin
      fPresets[i]:=DefaultPreset[i];
    end;
end;

end.

