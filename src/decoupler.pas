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
unit decoupler;

interface

uses
  Classes, SysUtils, BaseTypes;

type

  { TDecoupler }
  TDecoupledCommand = procedure(Sender: TObject; Command: TEngineCommand; Param: integer = 0) of object;

  { TDecoupThread }
  TDecoupler = class;

  TDecoupThread = class(TThread)
  private
    procedure RunEvent;
  public
    Owner:     TDecoupler;
    WaitEvent: PRtlEvent;
    constructor Create;
    procedure Execute; override;
    destructor Destroy; override;
  end;

  TDecoupler = class
  private
    DecoupThread: TDecoupThread;
    FOnCommand: TDecoupledCommand;
    fCommand: TEngineCommand;
    fParam: integer;
    procedure SetOnCommand(const AValue: TDecoupledCommand);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendCommand(Command: TEngineCommand; Param: Integer);
    property OnCommand: TDecoupledCommand read FOnCommand write SetOnCommand;
  end;

implementation

{ TDecoupThread }

constructor TDecoupThread.Create;
begin
  inherited Create(True);
  WaitEvent := RTLEventCreate;
end;

procedure TDecoupThread.Execute;
begin
  while not Terminated do
    begin
    RtlEventWaitFor(WaitEvent);
    if assigned(Owner.FOnCommand) and not Terminated then
      Synchronize(@RunEvent);

    RTLeventResetEvent(WaitEvent);
    end;
end;

procedure TDecoupThread.RunEvent;
begin
  Owner.FOnCommand(Owner, Owner.fCommand, Owner.fParam);
end;

destructor TDecoupThread.Destroy;
begin
  RTLeventdestroy(WaitEvent);
  inherited Destroy;
end;

{ TDecoupler }

constructor TDecoupler.Create;
begin
  DecoupThread := TDecoupThread.Create;
  DecoupThread.Owner := self;
  DecoupThread.Start;
end;

destructor TDecoupler.Destroy;
begin
  DecoupThread.Terminate;
  RTLeventSetEvent(DecoupThread.WaitEvent);
  DecoupThread.Free;
  inherited Destroy;
end;

procedure TDecoupler.SetOnCommand(const AValue: TDecoupledCommand);
begin
  if FOnCommand = AValue then
    exit;
  FOnCommand := AValue;
end;


procedure TDecoupler.SendCommand(Command: TEngineCommand; Param: integer);
begin
  fCommand := Command;
  fParam   := Param;
  RTLeventSetEvent(DecoupThread.WaitEvent);
end;

end.
