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
{$I ovoplayer.inc}
unit GeneralFunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AppConsts, CustApp, lclProc;

function TimeToMSec(Time: double): int64;
Function isAppRunning(Application:TCustomApplication):Boolean;
Function Restart(Application:TCustomApplication):Boolean;
Function CheckRestarting(Application:TCustomApplication):Boolean;

implementation
uses
 {$IFdef MSWindows}
  Windows,
{$ENDIF}
{$IFDEF unix}
  BaseUnix,
{$ENDIF}
 SimpleIPC, SimpleIPCWrapper,
 AsyncProcess;

Function Restart(Application:TCustomApplication):Boolean;
var
  NewProc: TAsyncProcess;
begin
  NewProc:= TAsyncProcess.Create(nil);
  try
    NewProc.CommandLine:= format('%s --restart=%d',[Application.ExeName,
                                                      GetProcessID]);

    NewProc.Execute;
  finally
    NewProc.Free;
  end;
  Application.Terminate;
  result := true;
end;

function TimeToMSec(Time: double): int64;
const
  transform = 1 / (24 * 60 * 60);
begin
  Result := trunc(Time * 1000 / transform);
end;

Function isAppRunning(Application:TCustomApplication):Boolean;
const
  BaseServerId = 'tuniqueinstance_';
//  Separator = '|';
var
  Client : TSimpleIPCClient;
  FilesInParm : TStringList;
  i: Integer;
begin
  Client := TSimpleIPCClient.Create(nil);
   with Client do
     try
       ServerId := BaseServerId + AppName;
       Result := IsServerRunning(Client);
       if result then
          begin
            Active := true;
            FilesInParm := TStringList.Create;
            Application.CheckOptions('', nil, nil, FilesInParm);
            for i := 0 to FilesinParm.Count - 1 do
              begin  
               SendStringMessage(1, 'file:x='+FilesInParm[i] +'|');
               DebugLn(FilesInParm[i]);
              end;
            SendStringMessage(1, 'action:activate|');
          end;

     finally
       Client.free;
     end;
end;

Function ProcessRunningByPID(Pid:DWORD):Boolean;
{$IFDEF WINDOWS}
  var
    ProcessHandle: THandle;
  begin
    Result:= false;
    ProcessHandle := OpenProcess(SYNCHRONIZE, false, PID);
    if ProcessHandle<>0 then
      begin
        Result:= true;
        CloseHandle(ProcessHandle);
      end;
  end;
{$ELSE}
{$IFDEF UNIX}
  begin
    Result := fpKill(PID, 0) = 0;
  end;
{$ELSE}
  begin
    DebugLn('ProcessRunningByPID not implemented for this OS. We just wait 5 seconds');
    Sleep(1000);
    Result := false;
  end;
{$ENDIF}
{$ENDIF}

function CheckRestarting(Application: TCustomApplication): Boolean;
var
  Pid: Longint;
  i:Integer;
  Alive:boolean;
begin
  result:=false;

  if not Application.HasOption(#00,'restart') then
     exit;
  if not TryStrToInt(application.GetOptionValue(#00,'restart'), pid) then
     exit;

  i:=0;
  repeat
    Alive := ProcessRunningByPID(pid);
    if Alive then
       begin
         sleep(1000);
         inc(i);
       end;
  until (i > 5) or not Alive;
  result := not alive;

end;

end.