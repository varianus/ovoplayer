unit ffmpegLib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function libav_dll_get_proc_addr(LibHandle:THandle; var addr: Pointer; const name: PAnsiChar): Boolean;

procedure libAV_dynamic_dll_init;
procedure libAV_dynamic_dll_done;

var
  libAV_dynamic_dll_error: string;

implementation
uses
  dynlibs;

const
  libavformat_name = 'avformat-52.dll';
  libavcodec_name = 'avcodec-52.dll';

var
 libavformat_handle: THandle;
 libavcodec_handle: THandle;

function libav_dll_get_proc_addr(LibHandle:THandle; var addr: Pointer; const name: PAnsiChar): Boolean;
begin
  addr := GetProcedureAddress(LibHandle, name);
  Result := (addr <> NIL);
  if not Result then
  begin
    libAV_dynamic_dll_error := 'Procedure "' + name + '" not found!';
  end;
end;

procedure libAV_dynamic_dll_init();
var
  cdir: string;
begin

  //if (libavformat_handle <> 0) then exit;
  //
  //libavformat_handle := LoadLibrary(PAnsiChar(libavformat_name));
  //libavcodec_handle := LoadLibrary(PAnsiChar(libavcodec_name));
  //
  //
  //// exit, report error
  //if (libavformat_handle = 0) then
  //begin
  //  libAV_dynamic_dll_error :=
  //    'Library not found ' + libavformat_name + ', '+
  //    'GetLastError() = ' + IntToStr(GetLastOSError);
  //  exit;
  //end;
  //if (libavcodec_handle = 0) then
  //begin
  //  libAV_dynamic_dll_error :=
  //    'Library not found ' + libavcodec_name + ', '+
  //    'GetLastError() = ' + IntToStr(GetLastOSError);
  //  exit;
  //end;
  //
  //if not libav_dll_get_proc_addr(libavformat_handle, pointer(av_register_all),  'av_register_all') then  exit;
  //if not libav_dll_get_proc_addr(libavcodec_handle, pointer(avcodec_alloc_frame),  'avcodec_alloc_frame') then  exit;
  //
end;


procedure libAV_dynamic_dll_done();
begin
  if (libavformat_handle <> 0) then FreeLibrary(libavformat_handle);
  libavformat_handle := 0;
end;

initialization

  libavformat_handle := 0;

finalization

end.


