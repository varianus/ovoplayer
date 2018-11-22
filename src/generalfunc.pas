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
unit GeneralFunc;

interface

uses
  Classes, SysUtils, AppConsts, CustApp, LazLoggerBase,
  netprotocol;

function TimeToMSec(Time: double): int64;
Function isAppRunning(Application:TCustomApplication):Boolean;
Function Restart(Application:TCustomApplication):Boolean;
Function CheckRestarting(Application:TCustomApplication):Boolean;
Function FormatTimeRange(const Time: TDateTime; ShortMode:boolean=false): string;

// Platform dependant function
procedure GetModuleByAddr(addr: pointer; var baseaddr: pointer; var filename: string);
Function ProcessRunningByPID(Pid:DWORD):Boolean;
function CompareBoolean (a, b: Boolean): Integer;

 type
  TByteStringFormat = (bsfDefault, bsfBytes, bsfKB, bsfMB, bsfGB, bsfTB);
  function FormatByteString(Bytes: UInt64; Format: TByteStringFormat = bsfDefault): string;

implementation
uses
{$DEFINE USESMODE}
  {$i generalfuncimpl.inc}
{$UNDEF USESMODE}
 SimpleIPC, Process, netsupport;

const
  OneKB = 1024;
  OneMB = OneKB * OneKB;
  OneGB = OneKB * OneMB;
  OneTB = OneKB * OneGB;

  OneHour = MinsPerHour * SecsPerMin * MSecsPerSec;


function FormatTimeRange(const Time: TDateTime; ShortMode:boolean=false): string;
begin
  if ShortMode and (Time < OneHour) then
    result:=FormatDateTime('[mm]:ss', Time / MSecsPerDay,[fdoInterval])
  else
    result:=FormatDateTime('[hh]:mm:ss', Time / MSecsPerDay,[fdoInterval]);

end;

function CompareBoolean (a, b: Boolean): Integer;
const
   BoolOrder: Array [False..True] Of Integer = (0,1); // o 1,0 se si desidera ordinare il contrario
Begin
   result := BoolOrder [a] - BoolOrder [b];
End ;

// code from David Heffernan, from http://stackoverflow.com/questions/30548940/correct-way-to-convert-size-in-bytes-to-kb-mb-gb-delphi
function FormatByteString(Bytes: UInt64; Format: TByteStringFormat = bsfDefault): string;
begin
  if Format = bsfDefault then begin
    if Bytes < OneKB then begin
      Format := bsfBytes;
    end
    else if Bytes < OneMB then begin
      Format := bsfKB;
    end
    else if Bytes < OneGB then begin
      Format := bsfMB;
    end
    else if Bytes < OneTB then begin
      Format := bsfGB;
    end
    else begin
      Format := bsfTB;
    end;
  end;

  case Format of
  bsfKB:
    Result := SysUtils.Format('%.1n KB', [Bytes / OneKB]);
  bsfMB:
    Result := SysUtils.Format('%.1n MB', [Bytes / OneMB]);
  bsfGB:
    Result := SysUtils.Format('%.1n GB', [Bytes / OneGB]);
  bsfTB:
    Result := SysUtils.Format('%.1n TB', [Bytes / OneTB]);
  else  // bsfBytes:
    Result := SysUtils.Format('%d bytes', [Bytes]);

  end;
end;

Function Restart(Application:TCustomApplication):Boolean;
var
  NewProc: TProcess;
  i: integer;
begin
  NewProc:= TProcess.Create(nil);
  try
    Newproc.InheritHandles := False;
    NewProc.CurrentDirectory:=GetCurrentDir;
    Newproc.Options := [];
    Newproc.ShowWindow := swoShow;
    Newproc.Executable:=Application.ExeName;
    NewProc.Parameters.Add(format('--restart=%d',[GetProcessID]));
    for I := 1 to GetEnvironmentVariableCount do
      NewProc.Environment.Add(GetEnvironmentString(I));

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
var
  Client : TSimpleIPCClient;
  FilesInParm : TStringList;
  i: Integer;
begin
  Client := TSimpleIPCClient.Create(nil);
   with Client do
     try
       ServerId := BaseServerId + AppName;
       Result := Client.ServerRunning;
       if result then
          begin
            Active := true;
            FilesInParm := TStringList.Create;
            Application.CheckOptions('', nil, nil, FilesInParm);
            for i := 0 to FilesinParm.Count - 1 do
              begin
               if i = 0 then // if there is more then one song, play the first one and enqueues the others
                 SendStringMessage(1, BuildCommand(CATEGORY_FILE,COMMAND_ENQUEUE_AND_PLAY,FilesInParm[i], true))
               else
                 SendStringMessage(1, BuildCommand(CATEGORY_FILE,COMMAND_ENQUEUE,FilesInParm[i], true))
              end;
            SendStringMessage(1, BuildCommand(CATEGORY_APP, COMMAND_ACTIVATE, '', True));
          end;

     finally
       Client.free;
     end;
end;

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

{$i generalfuncimpl.inc}
end.
