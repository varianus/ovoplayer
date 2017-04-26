{$IFDEF USESMODE}
  Windows,
{$ELSE}  

procedure GetModuleByAddr(addr: pointer; var baseaddr: pointer; var filename: string);
var
  Tmm: TMemoryBasicInformation;
  TST: array[0..Max_Path] of Char;
begin
  if VirtualQuery(addr, @Tmm, SizeOf(Tmm)) <> sizeof(Tmm)
    then raise Exception.Create('The VirualQuery() call failed.');
  baseaddr:=Tmm.AllocationBase;
  TST[0]:= #0;
  GetModuleFileName(UIntPtr(Tmm.AllocationBase), TST, SizeOf(TST));
  filename:= String(PChar(@TST));
end;

Function ProcessRunningByPID(Pid:DWORD):Boolean;
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
{$ENDIF}
