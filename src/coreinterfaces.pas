unit coreinterfaces;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils;

const
  SBackEnd = '{692D792E-C98A-4F0C-9422-6AFF2A92E065}';
type

  { IBackEnd }

  IBackEnd = interface
    [SBackEnd]
 // property Get-Set
    function GetPosition: int64;
    function GetVolume: cardinal;
    procedure SetPosition(AValue: int64);
    procedure SetVolume(AValue: cardinal);
 // Procedures
    Procedure Play;
    Procedure Stop;
    Procedure Pause;
    Procedure UnPause;
    Procedure Next;
    Procedure Previous;
    Procedure Quit;
 //Property
    property Position : int64 read GetPosition write SetPosition;
    property Volume   :cardinal read GetVolume write SetVolume;
  end;


implementation

end.

