unit coreinterfaces;

{$mode objfpc}{$H+}

interface
uses
  BaseTypes, basetag;

const
  SBackEnd = '{692D792E-C98A-4F0C-9422-6AFF2A92E065}';
  SSUbject = '{A75AF920-56C3-434F-8F4A-85655EA2C51E}';
  SObserver ='{03E2942D-D961-4CF2-A339-69044C763EB4}';
type

  TChangedProperty = (cpStatus, cpVolume, cpPosition, cpMetadata,
                      cpLooping, cpCurrentItem, cpClosing);

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
    Procedure Mute;
    Procedure UnMute;
    procedure HandleCommand(Command: TEngineCommand; Param: integer = 0);
    function HandleExternalCommand(Command: RExternalCommand):boolean;
    Procedure OpenURI(URI: String);
    procedure Seek(AValue: int64);
    Function GetMetadata(Index:integer=-1): TCommonTags;
    Function GetCoverURL: String;
    Function PlayListCount : integer;
    // property
    Property Status: TEngineState read GetStatus write SetStatus;
    Property Position: int64 read GetPosition write SetPosition;
    property Looping: TplRepeat read  GetLooping write SetLooping;
    Property Volume : cardinal read GetVolume write SetVolume;
  end;


implementation

end.

