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
  Classes, SysUtils, AppConsts;

function TimeToMSec(Time: double): int64;
Function isAppRunning:Boolean;

implementation
uses
  SimpleIPC, SimpleIPCWrapper;

function TimeToMSec(Time: double): int64;
const
  transform = 1 / (24 * 60 * 60);
begin
  Result := trunc(Time * 1000 / transform);
end;

Function isAppRunning:Boolean;
const
  BaseServerId = 'tuniqueinstance_';
//  Separator = '|';
var
  Client : TSimpleIPCClient;
begin
  Client := TSimpleIPCClient.Create(nil);
   with Client do
     try
       ServerId := BaseServerId + AppName;

       Result := IsServerRunning(Client);
       if result then
          begin
            Active := true;
            SendStringMessage(1, 'activate|');
          end;

     finally
       Client.free;
     end;
end;
end.
