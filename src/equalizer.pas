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
  PRESET_COUNT = 2;

var
  ARPreset : array [0..PRESET_COUNT -1] of RPreset = (
      (Name: 'default'; Values: (0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)),
      (Name: 'rock';    Values: (4.8,2.8,-3.4,-4.8,-2.0,2.4,5.3,6.7,6.7,6.7))
    );

implementation

end.

