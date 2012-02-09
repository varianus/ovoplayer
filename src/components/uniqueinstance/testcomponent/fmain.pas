unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  UniqueInstance, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButCrashApp: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    UniqueInstance1: TUniqueInstance;
    procedure ButCrashAppClick(Sender: TObject);
    procedure UniqueInstance1OtherInstance(Sender: TObject; Count: Integer;
      Parameters: array of String);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$ifdef unix}
uses
  BaseUnix;
{$endif}

{$ifdef windows}
uses
  Windows;
{$endif}

{$R *.lfm}

{ TForm1 }

procedure TForm1.UniqueInstance1OtherInstance(Sender: TObject; Count: Integer;
  Parameters: array of String);
var
  i:Integer;
begin
  Label1.Caption:='A new instance was created. '+ IntToStr(Count)+' Parameters';
  ListBox1.Clear;
  for i := 0 to Count - 1 do
    ListBox1.Items.Add(Parameters[i]);
  BringToFront;
end;

procedure TForm1.ButCrashAppClick(Sender: TObject);
begin
  {$ifdef unix}
  FpKill(FpGetpid, 9);
  {$endif}
  {$ifdef windows}
  TerminateProcess(GetCurrentProcess, 0);
  {$endif}
end;

end.

