unit coreinterfaces;

{$mode objfpc}{$H+}

interface
uses
  BaseTypes;

const
  SBackEnd = '{692D792E-C98A-4F0C-9422-6AFF2A92E065}';
type

  { IBackEnd }

  IBackEnd = interface
    [SBackEnd]
 // property Get-Set
    function GetLooping: TplRepeat;
    function GetPosition: int64;
    function GetStatus: TEngineState;
    function GetVolume: cardinal;
    procedure SetLooping(AValue: TplRepeat);
    procedure SetPosition(AValue: int64);
    procedure SetStatus(AValue: TEngineState);
    procedure SetVolume(AValue: cardinal);
 // Procedures
    Procedure Play;
    Procedure Stop;
    Procedure Pause;
    Procedure UnPause;
    Procedure Next;
    Procedure Previous;
    Procedure Quit;
    Procedure OpenURI(URI: String);

 //Property
    property Position : int64 read GetPosition write SetPosition;
    property Volume   : cardinal read GetVolume write SetVolume;
    Property Status   : TEngineState read GetStatus write SetStatus;
    Property Looping  : TplRepeat read GetLooping write SetLooping;
  end;


implementation

end.

