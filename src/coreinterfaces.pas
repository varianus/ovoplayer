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
   Procedure Update(Kind: TChangedProperty);
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
    procedure HandleCommand(Command: TEngineCommand; Param: integer = 0);
    Procedure OpenURI(URI: String);
    procedure Seek(AValue: int64);
    Function GetMetadata: TCommonTags;
  end;


implementation

end.

