{$IFDEF USESMODE}
  BaseUnix,
{$ELSE}  

procedure GetModuleByAddr(addr: pointer; var baseaddr: pointer; var filename: string);
var
  dlinfo: dl_info;
begin
  FillChar(dlinfo, sizeof(dlinfo), 0);
  dladdr(addr, @dlinfo);
  baseaddr:= dlinfo.dli_fbase;
  filename:= String(dlinfo.dli_fname);
  if ExtractFileName(filename) = ExtractFileName(ParamStr(0))
    then baseaddr:= nil;
end;

Function ProcessRunningByPID(Pid:DWORD):Boolean;
begin
    Result := fpKill(PID, 0) = 0;
 end;

{$ENDIF}   