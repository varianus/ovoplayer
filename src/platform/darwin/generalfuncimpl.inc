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
end;

Function ProcessRunningByPID(Pid:DWORD):Boolean;
begin
  DebugLn('ProcessRunningByPID not implemented for this OS. We just wait 5 seconds');
  Sleep(5000);
  Result := false;
 end;

{$ENDIF}   