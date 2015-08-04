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
function CompareBoolean (a, b: Boolean): Integer;

// Platform dependant function
procedure GetModuleByAddr(addr: pointer; var baseaddr: pointer; var filename: string);
Function ProcessRunningByPID(Pid:DWORD):Boolean;

type

  { TSortArray }
  TArrayCompareFunc = function(const Item1, Item2: Integer): Integer;

  generic TSortArray<T> = class (TObject)
  private
     class procedure IntQuickSort(var Arr: Array of T; L, R: Longint;
        Compare: TArrayCompareFunc);
  public
    Class Procedure Sort (var Arr: Array of T; Compare:TArrayCompareFunc);
  end;

 type
  TByteStringFormat = (bsfDefault, bsfBytes, bsfKB, bsfMB, bsfGB, bsfTB);
  function FormatByteString(Bytes: UInt64; Format: TByteStringFormat = bsfDefault): string;

implementation
uses
{$DEFINE USESMODE}
  {$i generalfuncimpl.inc}
{$UNDEF USESMODE}
 SimpleIPC, AsyncProcess;

const
  OneKB = 1024;
  OneMB = OneKB * OneKB;
  OneGB = OneKB * OneMB;
  OneTB = OneKB * OneGB;

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
  bsfBytes:
    Result := SysUtils.Format('%d bytes', [Bytes]);
  bsfKB:
    Result := SysUtils.Format('%.1n KB', [Bytes / OneKB]);
  bsfMB:
    Result := SysUtils.Format('%.1n MB', [Bytes / OneMB]);
  bsfGB:
    Result := SysUtils.Format('%.1n GB', [Bytes / OneGB]);
  bsfTB:
    Result := SysUtils.Format('%.1n TB', [Bytes / OneTB]);
  end;
end;

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
       Result := Client.ServerRunning;
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

{ TSortArray }

class procedure TSortArray.IntQuickSort(var Arr: Array of T; L, R: Longint;
  Compare: TArrayCompareFunc);

var
  I, J, P : Longint;
  Q : T;
begin
 repeat
   I := L;
   J := R;
   P :=  (L + R) div 2 ;
   repeat
     while Compare(P, i) > 0 do
       I := I + 1;
     while Compare(P, J) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := Arr[I];
       Arr[I] := Arr[J];
       Arr[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   if L < J then
     intQuickSort(Arr, L, J, Compare);
   L := I;
 until I >= R;
end;

class procedure TSortArray.Sort(var Arr: array of T; Compare: TArrayCompareFunc);
begin
  IntQuickSort(Arr,low(arr), High(Arr), Compare);
end;

function CompareBoolean (a, b: Boolean): Integer;
const
   BoolOrder: Array [False..True] Of Integer = (0,1); // o 1,0 se si desidera ordinare il contrario
Begin
   result := BoolOrder [a] - BoolOrder [b];
End ;

{$i generalfuncimpl.inc}
end.
