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

unit SimpleSingleInstance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleIPC, singleinstance;

type

  { TSimpleSingleInstance }
  TSimpleSingleInstance = class(TBaseSingleInstance)
  private
    FDefaultMessage: string;
    FGlobal: Boolean;
    FID: string;
    FServer: TSimpleIPCServer;
    FClient: TSimpleIPCClient;
    procedure SetDefaultMessage(AValue: string);
    procedure SetGlobal(const aGlobal: Boolean);
    procedure SetID(const aID: string);
  protected
    procedure ProcessMessage(ASender: TObject);
    function GetIsClient: Boolean; override;
    function GetIsServer: Boolean; override;
    function GetStartResult: TSingleInstanceStart; override;
  public
    constructor Create(aOwner: TComponent); override;
  public
    function Start: TSingleInstanceStart; override;
    procedure Stop; override;
    procedure ServerCheckMessages; override;
    procedure ClientPostParams; override;
    procedure ClientPostString(Message: String);
  public
    // this message will be sent if there are no params in the current instance
    property DefaultMessage: string read FDefaultMessage write SetDefaultMessage;
    property ID: string read FID write SetID;
    property Global: Boolean read FGlobal write SetGlobal;
  end;

implementation

Resourcestring
  SErrSetSingleInstanceIDStarted = 'You cannot change the single instance ID when it''s been started.';
  SErrSetSingleInstanceGlobalStarted = 'You cannot change the single instance global property when it''s been started.';
  SErrStartSingleInstanceStarted = 'You cannot start single instance when it''s been already started.';
  SErrSingleInstanceNotClient = 'Current instance is not a client.';

{ TSimpleSingleInstance }

procedure TSimpleSingleInstance.SetGlobal(const aGlobal: Boolean);
begin
  if FGlobal = aGlobal then Exit;
  if Assigned(FServer) or Assigned(FClient) then
    raise ESingleInstance.Create(SErrSetSingleInstanceGlobalStarted);
  FGlobal := aGlobal;

end;

procedure TSimpleSingleInstance.SetDefaultMessage(AValue: string);
begin
  if FDefaultMessage=AValue then Exit;
  FDefaultMessage:=AValue;
end;

procedure TSimpleSingleInstance.SetID(const aID: string);
begin
  if FID = aID then Exit;
  if Assigned(FServer) or Assigned(FClient) then
    raise ESingleInstance.Create(SErrSetSingleInstanceIDStarted);
  FID := aID;

end;

function TSimpleSingleInstance.GetIsClient: Boolean;
begin
  Result:= Assigned(FClient);
end;

function TSimpleSingleInstance.GetIsServer: Boolean;
begin
  Result:= Assigned(FClient);
end;

function TSimpleSingleInstance.GetStartResult: TSingleInstanceStart;
begin
  Result:=inherited GetStartResult;
end;

constructor TSimpleSingleInstance.Create(aOwner: TComponent);
var
  xID: RawByteString;
  I: Integer;
begin
  inherited Create(aOwner);

  xID := 'SI_'+ExtractFileName(ParamStr(0));
  for I := 1 to Length(xID) do
    case xID[I] of
      'a'..'z', 'A'..'Z', '0'..'9', '_': begin end;
    else
      xID[I] := '_';
    end;
  ID := xID;

  FGlobal:= true;
  FClient := nil;
  FServer := nil;

end;

procedure TSimpleSingleInstance.ProcessMessage(ASender: TObject);
var
 stl: TStringList;
begin
 Stl := TStringList.Create;
 try
   stl.DelimitedText:= FServer.StringMessage;

   if Assigned(OnServerReceivedParams) then
      OnServerReceivedParams(Self, stl);
 finally
   stl.free;
 end;
end;

function TSimpleSingleInstance.Start: TSingleInstanceStart;
begin
  if Assigned(FServer) or Assigned(FClient) then
    raise ESingleInstance.Create(SErrStartSingleInstanceStarted);

  FClient := TSimpleIPCClient.Create(Self);
  FClient.ServerId := FID;
  if FClient.ServerRunning then
    begin
      Result := siClient;
      FClient.Active:=true;
    end
  else
    begin
      FreeAndNil(FClient);
      FServer := TSimpleIPCServer.Create(Self);
      FServer.OnMessage:= @ProcessMessage;
      FServer.Global:=FGlobal;
      FServer.ThreadTimeOut:=500;
      FServer.ServerID:=FID;
      FServer.StartServer(false);
      Result := siServer;
    end;
  SetStartResult(Result);
end;

procedure TSimpleSingleInstance.Stop;
begin
  FreeAndNil(FClient);
  FreeAndNil(FServer);
end;

procedure TSimpleSingleInstance.ServerCheckMessages;
begin
  FServer.PeekMessage(1, true);
end;

procedure TSimpleSingleInstance.ClientPostParams;
var
  xSL: TStringList;
  I: Integer;
begin
  if not Assigned(FClient) then
    raise ESingleInstance.Create(SErrSingleInstanceNotClient);

  xSL := TStringList.Create;
  try
    if ParamCount > 0 then
      begin
        for I := 1 to ParamCount do
          xSL.Add(ParamStr(I));
        FClient.SendStringMessage(xSL.DelimitedText);
      end
    else
      if DefaultMessage <> EmptyStr then
        FClient.SendStringMessage(DefaultMessage);
  finally
    xSL.Free;
  end;

end;

procedure TSimpleSingleInstance.ClientPostString(Message: String);
begin
  FClient.SendStringMessage(Message);
end;

initialization
  DefaultSingleInstanceClass:=TSimpleSingleInstance;

end.

