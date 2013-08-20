///////////////////////////////////////////////////////////////////////
// Copyright (C) Andru 2007                                          //
// mail: dr.andru@gmail.com                                          //
// site: http://andru.2x4.ru                                         //
//                                                                   //
// OMEGA Engine is free software.                                    //
//                                                                   //
// You may redistribute it and/or modify it under the terms of the   //
// GNU General Public License, as published by the Free Software     //
// Foundation; either version 2 of the License, or (at your option)  //
// any later version.                                                //
//                                                                   //
// this module is distributed in the hope that it will be useful,    //
// but WITHOUT ANY WARRANTY; without even the implied warranty of    //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.              //
// See the GNU General Public License for more details.              //
//                                                                   //
// You should have received a copy of the GNU General Public License //
// along with this module.  If not, write to:                        //
// 	The Free Software Foundation, Inc.,                          //
// 	51 Franklin Street, Fifth Floor                              //
// 	Boston, MA  02110-1301, USA.                                 //
///////////////////////////////////////////////////////////////////////
unit mediadshow;
{$mode objfpc}{$H+}

interface
uses
  Windows, Activex;

const
  CLSCTX_INPROC_SERVER     = 1;
  CLSID_FilterGraph: TGUID = (D1:$E436EBB3;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));

type
  PIID = PGUID;
  TIID = TGUID;
  PCLSID = PGUID;
  TCLSID = TGUID;
  HSEMAPHORE = Longint;

  IPersist = interface(IUnknown)
    ['{0000010C-0000-0000-C000-000000000046}']
    function GetClassID(out classID: TCLSID): HResult; stdcall;
  end;

  TAM_Media_Type = record
    majortype: TGUID;
    subtype: TGUID;
    bFixedSizeSamples: BOOL;
    bTemporalCompression: BOOL;
    lSampleSize: ULONG;
    formattype: TGUID;
    pUnk: IUnknown;
    cbFormat: ULONG;
    pbFormat: Pointer;
  end;
  PAM_Media_Type = ^TAM_Media_Type;

  IBaseFilter = interface;

  TPin_Direction = (
    PINDIR_INPUT,
    PINDIR_OUTPUT
  );

  TPin_Info = record
    pFilter: IBaseFilter;
    dir: TPin_Direction;
    achName: array[0..127] of WCHAR;
  end;

  IEnumFilters = interface;
  IEnumMediaTypes = interface;

  IPin = interface(IUnknown)
    ['{56A86891-0AD4-11CE-B03A-0020AF0BA770}']
    function Connect(pReceivePin: IPin; const pmt: PAM_Media_Type): HRESULT; stdcall;
    function ReceiveConnection(pConnector: IPin; const pmt: TAM_Media_Type): HRESULT; stdcall;
    function Disconnect: HRESULT; stdcall;
    function ConnectedTo(out pPin: IPin): HRESULT; stdcall;
    function ConnectionMediaType(out pmt: TAM_Media_Type): HRESULT; stdcall;
    function QueryPinInfo(out pInfo: TPin_Info): HRESULT; stdcall;
    function QueryDirection(out pPinDir: TPin_Direction): HRESULT; stdcall;
    function QueryId(out Id: LPWSTR): HRESULT; stdcall;
    function QueryAccept(const pmt: TAM_Media_Type): HRESULT; stdcall;
    function EnumMediaTypes(out ppEnum: IEnumMediaTypes): HRESULT; stdcall;
    function QueryInternalConnections(out apPin: IPin; var nPin: ULONG): HRESULT; stdcall;
    function EndOfStream: HRESULT; stdcall;
    function BeginFlush: HRESULT; stdcall;
    function EndFlush: HRESULT; stdcall;
    function NewSegment(tStart, tStop: int64; dRate: double): HRESULT; stdcall;
  end;

  IEnumPins = interface(IUnknown)
    ['{56A86892-0AD4-11CE-B03A-0020AF0BA770}']
    function Next(cPins: ULONG; out ppPins: IPin; pcFetched: PULONG): HRESULT; stdcall;
    function Skip(cPins: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumPins): HRESULT; stdcall;
  end;

  IEnumMediaTypes = interface(IUnknown)
    ['{89C31040-846B-11CE-97D3-00AA0055595A}']
    function Next(cMediaTypes: ULONG; out ppMediaTypes: PAM_Media_Type;
      pcFetched: PULONG): HRESULT; stdcall;
    function Skip(cMediaTypes: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumMediaTypes): HRESULT; stdcall;
  end;

  IFilterGraph = interface(IUnknown)
    ['{56A8689F-0AD4-11CE-B03A-0020AF0BA770}']
    function AddFilter(pFilter: IBaseFilter; pName: PWideChar): HRESULT; stdcall;
    function RemoveFilter(pFilter: IBaseFilter): HRESULT; stdcall;
    function EnumFilters(out ppEnum: IEnumFilters): HRESULT; stdcall;
    function FindFilterByName(pName: PWideChar; out ppFilter: IBaseFilter): HRESULT; stdcall;
    function ConnectDirect(ppinOut, ppinIn: IPin; pmt: PAM_Media_Type): HRESULT; stdcall;
    function Reconnect(ppin: IPin): HRESULT; stdcall;
    function Disconnect(ppin: IPin): HRESULT; stdcall;
    function SetDefaultSyncSource: HRESULT; stdcall;
  end;

  IEnumFilters = interface(IUnknown)
    ['{56A86893-0AD4-11CE-B03A-0020AF0BA770}']
    function Next(cFilters: ULONG; out ppFilter: IBaseFilter;
      pcFetched: PULONG): HRESULT; stdcall;
    function Skip(cFilters: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumFilters): HRESULT; stdcall;
  end;

  TFilter_State = (
    State_Stopped,
    State_Paused,
    State_Running
  );

  IReferenceClock = interface;

  IMediaFilter = interface(IPersist)
    ['{56A86899-0AD4-11CE-B03A-0020AF0BA770}']
    function Stop: HRESULT; stdcall;
    function Pause: HRESULT; stdcall;
    function Run(tStart: int64): HRESULT; stdcall;
    function GetState(dwMilliSecsTimeout: DWORD; out State: TFilter_State): HRESULT; stdcall;
    function SetSyncSource(pClock: IReferenceClock): HRESULT; stdcall;
    function GetSyncSource(out pClock: IReferenceClock): HRESULT; stdcall;
  end;

  TFilterInfo = record
    achName: array[0..127] of WCHAR;
    pGraph: IFilterGraph;
  end;

  IGraphBuilder = interface(IFilterGraph)
    ['{56A868A9-0AD4-11CE-B03A-0020AF0BA770}']
    function Connect(ppinOut, ppinIn: IPin): HRESULT; stdcall;
    function Render(ppinOut: IPin): HRESULT; stdcall;
    function RenderFile(lpcwstrFile, lpcwstrPlayList: PWideChar): HRESULT; stdcall;
    function AddSourceFilter(lpcwstrFileName, lpcwstrFilterName: LPCWSTR;
        out ppFilter: IBaseFilter): HRESULT; stdcall;
    function SetLogFile(hFile: THandle): HRESULT; stdcall;
    function Abort: HRESULT; stdcall;
    function ShouldOperationContinue: HRESULT; stdcall;
  end;

  IBaseFilter = interface(IMediaFilter)
    ['{56A86895-0AD4-11CE-B03A-0020AF0BA770}']
    function EnumPins(out ppEnum: IEnumPins): HRESULT; stdcall;
    function FindPin(Id: PWideChar; out ppPin: IPin): HRESULT; stdcall;
    function QueryFilterInfo(out pInfo: TFilterInfo): HRESULT; stdcall;
    function JoinFilterGraph(pGraph: IFilterGraph; pName: PWideChar): HRESULT; stdcall;
    function QueryVendorInfo(out pVendorInfo: PWideChar): HRESULT; stdcall;
  end;

  IReferenceClock = interface(IUnknown)
    ['{56A86897-0AD4-11CE-B03A-0020AF0BA770}']
    function GetTime(out pTime: int64): HRESULT; stdcall;
    function AdviseTime(baseTime, streamTime: int64;
        hEvent: THandle; out pdwAdviseCookie: DWORD): HRESULT; stdcall;
    function AdvisePeriodic(startTime, periodTime: int64;
        hSemaphore: HSEMAPHORE; out pdwAdviseCookie: DWORD): HRESULT; stdcall;
    function Unadvise(dwAdviseCookie: DWORD): HRESULT; stdcall;
  end;

  IMediaControl = interface(IDispatch)
    ['{56A868B1-0AD4-11CE-B03A-0020AF0BA770}']
    (* IMediaControl methods *)
    function Run: HResult; stdcall;
    function Pause: HResult; stdcall;
    function Stop: HResult; stdcall;
    function GetState(msTimeout: DWORD; out pfs: TFilter_State): HResult; stdcall;
    function RenderFile(strFilename: WideString): HResult; stdcall;
    function AddSourceFilter(strFilename: WideString; out ppUnk: IDispatch): HResult; stdcall;
    function get_FilterCollection(out ppUnk: IDispatch): HResult; stdcall;
    function get_RegFilterCollection(out ppUnk: IDispatch): HResult; stdcall;
    function StopWhenReady: HResult; stdcall;
  end;

  IBasicAudio = interface(IDispatch)
    ['{56A868B3-0AD4-11CE-B03A-0020AF0BA770}']
    (* IBasicAudio methods *)
    function put_Volume(lVolume: Longint): HResult; stdcall;
    function get_Volume(out plVolume: Longint): HResult; stdcall;
    function put_Balance(lBalance: Longint): HResult; stdcall;
    function get_Balance(out plBalance: Longint): HResult; stdcall;
  end;

  IMediaPosition = interface(IDispatch)
    ['{56A868B2-0AD4-11CE-B03A-0020AF0BA770}']
    (* IMediaPosition methods *)
    function get_Duration(out plength: Double): HResult; stdcall;
    function put_CurrentPosition(llTime: Double): HResult; stdcall;
    function get_CurrentPosition(out pllTime: Double): HResult; stdcall;
    function get_StopTime(out pllTime: Double): HResult; stdcall;
    function put_StopTime(llTime: Double): HResult; stdcall;
    function get_PrerollTime(out pllTime: Double): HResult; stdcall;
    function put_PrerollTime(llTime: Double): HResult; stdcall;
    function put_Rate(dRate: double): HResult; stdcall;
    function get_Rate(out pdRate: double): HResult; stdcall;
    function CanSeekForward(out pCanSeekForward: Longint): HResult; stdcall;
    function CanSeekBackward(out pCanSeekBackward: Longint): HResult; stdcall;
  end;

  IMediaEvent = interface(IDispatch)
    ['{56A868B6-0AD4-11CE-B03A-0020AF0BA770}']
    (*** IMediaEvent methods ***)
    function GetEventHandle(out hEvent: Longint): HResult; stdcall;
    function GetEvent(out lEventCode: Longint; out lParam1, lParam2: Longint;
        msTimeout: DWORD): HResult; stdcall;
    function WaitForCompletion(msTimeout: DWORD; out pEvCode: Longint):
        HResult; stdcall;
    function CancelDefaultHandling(lEvCode: Longint): HResult; stdcall;
    function RestoreDefaultHandling(lEvCode: Longint): HResult; stdcall;
    function FreeEventParams(lEvCode: Longint; lParam1, lParam2: Longint):
        HResult; stdcall;
  end;

  IMediaEventEx = interface(IMediaEvent)
    ['{56A868C0-0AD4-11CE-B03A-0020AF0BA770}']
    (* IMediaEventEx methods *)
    function SetNotifyWindow(hwnd: HWND; lMsg: Longint; lInstanceData: Longint): HResult; stdcall;
    function SetNotifyFlags(lNoNotifyFlags: Longint): HResult; stdcall;
    function GetNotifyFlags(out lplNoNotifyFlags): HResult; stdcall;
  end;


const
  IID_IGraphBuilder  : TGUID = '{56A868A9-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IMediaControl  : TGUID = (D1:$56A868B1;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IBasicAudio    : TGUID = (D1:$56A868B3;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IMediaPosition : TGUID = (D1:$56A868B2;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IMediaEventEx  : TGUID = (D1:$56A868C0;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));

//----------------------------------------------------------------------------//
                                 IMPLEMENTATION
//----------------------------------------------------------------------------//

initialization
  CoInitialize( nil );
finalization
  CoUninitialize;

end.
