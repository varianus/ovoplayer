unit mediafoundation;

{$mode objfpc}{$H+}

interface

uses
  Windows, ActiveX;

const
  MF_SDK_VERSION = $0002;
  MF_API_VERSION = $0070;
  MF_VERSION = (MF_SDK_VERSION shl 16 or MF_API_VERSION);
  MFSTARTUP_FULL = 0;
  MF_RESOLUTION_MEDIASOURCE = $1;

  MF_TOPOLOGY_OUTPUT_NODE = 0;
  MF_TOPOLOGY_SOURCESTREAM_NODE = (MF_TOPOLOGY_OUTPUT_NODE + 1);


  IID_IMFMediaSource: TGUID = '{279a808d-aec7-40c8-9c6b-a6b492c78a66}';
  IID_IMFPresentationClock: TGUID =
    '{868CE85C-8EA9-4f55-AB82-B009A910A805}';
  IID_IMFAsyncCallback: TGUID =
    '{a27003cf-2354-4f2a-8d6a-ab7cff15437e}';

  MF_TOPONODE_SOURCE: TGUID =
    '{835c58ec-e075-4bc7-bcba-4de000df9ae6}';
  MF_TOPONODE_PRESENTATION_DESCRIPTOR: TGUID =
    '{835c58ed-e075-4bc7-bcba-4de000df9ae6}';
  MF_TOPONODE_STREAM_DESCRIPTOR: TGUID =
    '{835c58ee-e075-4bc7-bcba-4de000df9ae6}';
  MF_TOPONODE_SEQUENCE_ELEMENTID: TGUID =
    '{835c58ef-e075-4bc7-bcba-4de000df9ae6}';
  MF_TOPONODE_TRANSFORM_OBJECTID: TGUID =
    '{88dcc0c9-293e-4e8b-9aeb-0ad64cc016b0}';
  MF_TOPONODE_STREAMID: TGUID =
    '{14932f9b-9087-4bb4-8412-5167145cbe04}';
  MF_TOPONODE_NOSHUTDOWN_ON_REMOVE: TGUID =
    '{14932f9c-9087-4bb4-8412-5167145cbe04}';

type
  TOPOID = UInt64;
  UINT32 = longword;
  UINT8 = byte;

  PTopoid = ^TOPOID;

  PROPERTYKEY = packed record
    fmtid: TGuid;
    pid: DWord;
  end;
  PPROPERTYKEY = ^PROPERTYKEY;

  MF_TOPOLOGY_TYPE = cardinal;
  MF_OBJECT_TYPE = cardinal;
  MFTIME = LONGLONG;
  MF_CLOCK_STATE = integer;

  REFGUID = GUID;
  MediaEventType = DWord;

  MF_ATTRIBUTE_TYPE = record
    u32: UINT32;
    u64: UINT64;
    d: double;
  end;

  PROPVARIANT = packed record
    vt: TVarType;
    wReserved1: byte;
    wReserved2: byte;
    wReserved3: uLong;
    Something: int64;
  end;

  MF_ATTRIBUTES_MATCH_TYPE = (
    MF_ATTRIBUTES_MATCH_OUR_ITEMS = 0,
    MF_ATTRIBUTES_MATCH_THEIR_ITEMS = 1,
    MF_ATTRIBUTES_MATCH_ALL_ITEMS = 2,
    MF_ATTRIBUTES_MATCH_INTERSECTION = 3,
    MF_ATTRIBUTES_MATCH_SMALLER = 4
    );

  REFPROPVARIANT = ^PROPVARIANT;

  MFCLOCK_PROPERTIES = record
    qwCorrelationRate: UInt64;
    guidClockId: TGUID;
    dwClockFlags: DWORD;
    qwClockFrequency: UInt64;
    dwClockTolerance: DWORD;
    dwClockJitter: DWORD;
  end;

  IMFAttributes = interface(IUnknown)
    ['{2cd2d921-c447-44a7-a13c-4adabfc247e3}']
    function GetItem(const guidKey: REFGUID; var pValue: PROPVARIANT): HResult; stdcall;
    function GetItemType(const guidKey: REFGUID; out pType: MF_ATTRIBUTE_TYPE): HResult;
      stdcall;
    function CompareItem(const guidKey: REFGUID; const Value: REFPROPVARIANT;
      out pbResult: boolean): HResult; stdcall;
    function Compare(const pTheirs: IMFAttributes; const MatchType: MF_ATTRIBUTES_MATCH_TYPE;
      out pbResult: boolean): HResult;
      stdcall;
    function GetUINT32(const guidKey: REFGUID; out punValue: UINT32): HResult; stdcall;
    function GetUINT64(const guidKey: REFGUID; out punValue: UINT64): HResult; stdcall;
    function GetDouble(const guidKey: REFGUID; out pfValue: double): HResult; stdcall;
    function GetGUID(const guidKey: REFGUID; out pguidValue: TGuid): HResult; stdcall;
    function GetStringLength(const guidKey: REFGUID; out pcchLength: UINT32): HResult;
      stdcall;
    function GetString(const guidKey: REFGUID; out pwszValue: LPWSTR;
      out cchBufSize: UINT32; var pcchLength: UINT32): HResult; stdcall;
    function GetAllocatedString(const guidKey: REFGUID; out ppwszValue: LPWSTR;
      out pcchLength: UINT32): HResult; stdcall;
    function GetBlobSize(const guidKey: REFGUID; out pcbBlobSize: UINT32): HResult; stdcall;
    function GetBlob(const guidKey: REFGUID; out pBuf: UINT8; out cbBufSize: UINT32;
      var pcbBlobSize: UINT32): HResult; stdcall;
    function GetAllocatedBlob(const guidKey: REFGUID; out ppBuf: UINT8;
      out pcbSize: UINT32): HResult; stdcall;
    function GetUnknown(const guidKey: REFGUID; const riid: REFIID;
      out ppv: Pointer): HResult; stdcall;
    function SetItem(const guidKey: REFGUID; const Value: REFPROPVARIANT): HResult;
      stdcall;
    function DeleteItem(const guidKey: REFGUID): HResult; stdcall;
    function DeleteAllItems(): HResult; stdcall;
    function SetUINT32(const guidKey: REFGUID; const unValue: UINT32): HResult; stdcall;
    function SetUINT64(const guidKey: REFGUID; const unValue: UINT64): HResult; stdcall;
    function SetDouble(const guidKey: REFGUID; const fValue: double): HResult; stdcall;
    function SetGUID(const guidKey: REFGUID; const guidValue: REFGUID): HResult; stdcall;
    function SetString(const guidKey: REFGUID; const wszValue: LPCWSTR): HResult; stdcall;
    function SetBlob(const guidKey: REFGUID; const pBuf: UINT8;
      const cbBufSize: UINT32): HResult; stdcall;
    function SetUnknown(const guidKey: REFGUID; const pUnknown: IUnknown): HResult; stdcall;
    function LockStore(): HResult; stdcall;
    function UnlockStore(): HResult; stdcall;
    function GetCount(out pcItems: UINT32): HResult; stdcall;
    function GetItemByIndex(const unIndex: UINT32; const guidKey: REFGUID;
      var pValue: PROPVARIANT): HResult; stdcall;
    function CopyAllItems(const pDest: IMFAttributes): HResult; stdcall;

  end;

  IMFAsyncResult = interface(IUnknown)
    ['{ac6b7889-0740-4d51-8619-905994a55cc6}']
    function GetState(out ppunkState: IUnknown): HResult; stdcall;
    function GetStatus(): HResult; stdcall;
    function SetStatus(const hrStatus: HRESULT): HResult; stdcall;
    function GetObject(out ppObject: IUnknown): HResult; stdcall;
    function GetStateNoAddRef(): IUnknown; stdcall;
  end;

  IMFAsyncCallback = interface(IUnknown)
    ['{a27003cf-2354-4f2a-8d6a-ab7cff15437e}']
    function GetParameters(out pdwFlags: DWord; out pdwQueue: DWord): HResult; stdcall;
    function Invoke(const pAsyncResult: IMFAsyncResult): HResult; stdcall;
  end;

  IMFMediaEvent = interface(IUnknown)
    ['{DF598932-F10C-4E39-BBA2-C308F101DAA3}']
    function GetType(out pmet: MediaEventType): HResult; stdcall;
    function GetExtendedType(out pguidExtendedType: TGuid): HResult; stdcall;
    function GetStatus(out phrStatus: HRESULT): HResult; stdcall;
    function GetValue(out pvValue: PROPVARIANT): HResult; stdcall;
  end;


  IMFCollection = interface(IUnknown)
    ['{5BC8A76B-869A-46a3-9B03-FA218A66AEBE}']
    function GetElementCount(out pcElements: DWord): HResult; stdcall;
    function GetElement(const dwElementIndex: DWord;
      out ppUnkElement: IUnknown): HResult; stdcall;
    function AddElement(const pUnkElement: IUnknown): HResult; stdcall;
    function RemoveElement(const dwElementIndex: DWord;
      ppUnkElement: IUnknown): HResult; stdcall;
    function InsertElementAt(const dwIndex: DWord; const pUnknown: IUnknown): HResult;
      stdcall;
    function RemoveAllElements(): HResult; stdcall;
  end;

  IMFMediaEventGenerator = interface(IUnknown)
    ['{2CD0BD52-BCD5-4B89-B62C-EADC0C031E7D}']
    function GetEvent(const dwFlags: DWord; out ppEvent: IMFMediaEvent): HResult;
      stdcall;
    function BeginGetEvent(const pCallback: IMFAsyncCallback;
      const punkState: IUnknown): HResult; stdcall;
    function EndGetEvent(const pResult: IMFAsyncResult;
      out ppEvent: IMFMediaEvent): HResult; stdcall;
    function QueueEvent(const met: MediaEventType; const guidExtendedType: REFGUID;
      hrStatus: HRESULT; const pvValue: PROPVARIANT): HResult; stdcall;
  end;

  IMFMediaType = interface(IUnknown)
    ['{44ae0fa8-ea31-4109-8d2e-4cae4997c555}']
    function GetMajorType(out pguidMajorType: TGuid): HResult; stdcall;
    function IsCompressedFormat(out pfCompressed: boolean): HResult; stdcall;
    function IsEqual(const pIMediaType: IMFMediaType; out pdwFlags: DWord): HResult; stdcall;
    function GetRepresentation(const guidRepresentation: TGuid;
      out ppvRepresentation: Pointer): HResult; stdcall;
    function FreeRepresentation(const guidRepresentation: TGuid;
      const pvRepresentation: Pointer): HResult; stdcall;
  end;

  IMFTopologyNode = interface(IMFAttributes)
    ['{83CF873A-F6DA-4bc8-823F-BACFD55DC430}']
    function SetObject(const pObject: IUnknown): HResult; stdcall;
    function GetObject(out ppObject: IUnknown): HResult; stdcall;
    function GetNodeType(out pType: MF_TOPOLOGY_TYPE): HResult; stdcall;
    function GetTopoNodeID(out pID: TOPOID): HResult; stdcall;
    function SetTopoNodeID(const ullTopoID: TOPOID): HResult; stdcall;
    function GetInputCount(out pcInputs: DWord): HResult; stdcall;
    function GetOutputCount(out pcOutputs: DWord): HResult; stdcall;
    function ConnectOutput(const dwOutputIndex: DWord; const pDownstreamNode: IMFTopologyNode;
      const dwInputIndexOnDownstreamNode: DWord): HResult; stdcall;
    function DisconnectOutput(const dwOutputIndex: DWord): HResult; stdcall;
    function GetInput(const dwInputIndex: DWord; out ppUpstreamNode: IMFTopologyNode;
      out pdwOutputIndexOnUpstreamNode: DWord): HResult; stdcall;
    function GetOutput(const dwOutputIndex: DWord; out ppDownstreamNode: IMFTopologyNode;
      out pdwInputIndexOnDownstreamNode: DWord): HResult; stdcall;
    function SetOutputPrefType(const dwOutputIndex: DWord;
      const pType: IMFMediaType): HResult; stdcall;
    function GetOutputPrefType(const dwOutputIndex: DWord;
      out ppType: IMFMediaType): HResult; stdcall;
    function SetInputPrefType(const dwInputIndex: DWord;
      const pType: IMFMediaType): HResult; stdcall;
    function GetInputPrefType(const dwInputIndex: DWord;
      out ppType: IMFMediaType): HResult; stdcall;
    function CloneFrom(const pNode: IMFTopologyNode): HResult; stdcall;
  end;

  IMFTopology = interface(IMFAttributes)
    ['{83CF873A-F6DA-4bc8-823F-BACFD55DC433}']
    function GetTopologyID(out pID: TOPOID): HResult; stdcall;
    function AddNode(const pNode: IMFTopologyNode): HResult; stdcall;
    function RemoveNode(const pNode: IMFTopologyNode): HResult; stdcall;
    function GetNodeCount(out pwNodes: word): HResult; stdcall;
    function GetNode(const wIndex: word; out ppNode: IMFTopologyNode): HResult; stdcall;
    function Clear(): HResult; stdcall;
    function CloneFrom(const pTopology: IMFTopology): HResult; stdcall;
    function GetNodeByID(const qwTopoNodeID: TOPOID;
      out ppNode: IMFTopologyNode): HResult; stdcall;
    function GetSourceNodeCollection(out ppCollection: IMFCollection): HResult; stdcall;
    function GetOutputNodeCollection(out ppCollection: IMFCollection): HResult; stdcall;
  end;

  IMFClock = interface(IUnknown)
    ['{2eb1e945-18b8-4139-9b1a-d5d584818530}']
    function GetClockCharacteristics(out pdwCharacteristics: DWord): HResult; stdcall;
    function GetCorrelatedTime(const dwReserved: DWord; out pllClockTime: LongLong;
      out phnsSystemTime: MFTIME): HResult; stdcall;
    function GetContinuityKey(out pdwContinuityKey: Dword): HResult; stdcall;
    function GetState(const dwReserved: DWord; out peClockState: MF_CLOCK_STATE): HResult;
      stdcall;
    function GetProperties(out pClockProperties: MFCLOCK_PROPERTIES): HResult; stdcall;
  end;

  IMFMediaSession = interface(IMFMediaEventGenerator)
    ['{90377834-21D0-4dee-8214-BA2E3E6C1127}']
    function SetTopology(const dwSetTopologyFlags: Dword;
      const pTopology: IMFTopology): HResult; stdcall;
    function ClearTopologies(): HResult; stdcall;
    function Start(const pguidTimeFormat: TGUID;
      const pvarStartPosition: PROPVARIANT): HResult; stdcall;
    function Pause(): HResult; stdcall;
    function Stop(): HResult; stdcall;
    function Close(): HResult; stdcall;
    function Shutdown(): HResult; stdcall;
    function GetClock(out ppClock: IMFClock): HResult; stdcall;
    function GetSessionCapabilities(out pdwCaps: DWord): HResult; stdcall;
    function GetFullTopology(const dwGetFullTopologyFlags: DWord;
      const TopoId: TOPOID; out ppFullTopology: IMFTopology): HResult; stdcall;
  end;

  IPropertyStore = interface(IUnknown)
    ['{886d8eeb-8cf2-4446-8d02-cdba1dbdcf99}']
    function GetCount(out cProps: DWORD): HResult; stdcall;
    function GetAt(iProp: DWORD; out pkey: PPROPERTYKEY): HResult; stdcall;
    function GetValue(const key: PROPERTYKEY; out pv: TPropVariant): HResult; stdcall;
    function SetValue(const key: PROPERTYKEY; const propvar: TPropVariant): HResult;
      stdcall;
    function Commit: HResult; stdcall;
  end;

  IMFSourceResolver = interface(IUnknown)
    ['{90377834-21D0-4dee-8214-BA2E3E6C1127}']
    function CreateObjectFromURL(const pwszURLL: LPCWSTR; const dwFlags: DWord;
      const pProps: IPropertyStore; out pObjectType: MF_OBJECT_TYPE;
      out ppObject: IUnknown): HResult; stdcall;
    //function CreateObjectFromByteStream(const pByteStream: IMFByteStream;
    //  const pwszURL: LPCWSTR; const dwFlags: DWord;
    //  const pProps: IPropertyStore;
    //  out pObjectType: MF_OBJECT_TYPE;
    //  out ppObject: IUnknown): HResult; stdcall;
    function BeginCreateObjectFromURL(const pwszURL: LPCWSTR; const dwFlags: DWord;
      const pProps: IPropertyStore; out ppIUnknownCancelCookie: IUnknown;
      const pCallback: IMFAsyncCallback; const punkState: IUnknown): HResult; stdcall;
    function EndCreateObjectFromURL(const pResult: IMFAsyncResult;
      out pObjectType: MF_OBJECT_TYPE; ppObject: IUnknown): HResult; stdcall;
    //function BeginCreateObjectFromByteStream(const pByteStream: IMFByteStream;
    //  const pwszURL: LPCWSTR;
    //  const dwFlags: DWord; const pProps: IPropertyStore;
    //  out ppIUnknownCancelCookie: IUnknown;
    //  const pCallback: IMFAsyncCallback;
    //  const punkState: IUnknown): HResult;
    //  stdcall;
    function EndCreateObjectFromByteStream(const pResult: IMFAsyncResult;
      out pObjectType: MF_OBJECT_TYPE; out ppObject: IUnknown): HResult; stdcall;
    function CancelObjectCreation(const pIUnknownCancelCookie: IUnknown): HResult;
      stdcall;
  end;

  //Interface IMFMediaTypeHandler
  IMFMediaTypeHandler = interface(IUnknown)
    ['{e93dcf6c-4b07-4e1e-8123-aa16ed6eadf5}']
    function IsMediaTypeSupported(const pMediaType: IMFMediaType;
      out ppMediaType: IMFMediaType): HResult; stdcall;
    function GetMediaTypeCount(out pdwTypeCount: DWord): HResult; stdcall;
    function GetMediaTypeByIndex(const dwIndex: DWord;
      out ppType: IMFMediaType): HResult; stdcall;
    function SetCurrentMediaType(const pMediaType: IMFMediaType): HResult; stdcall;
    function GetCurrentMediaType(out ppMediaType: IMFMediaType): HResult; stdcall;
    function GetMajorType(out pguidMajorType: TGuid): HResult; stdcall;
  end;

  IMFStreamDescriptor = interface(IMFAttributes)
    ['{56c03d9c-9dbb-45f5-ab4b-d80f47c05938}']
    function GetStreamIdentifier(out pdwStreamIdentifier: DWord): HResult; stdcall;
    function GetMediaTypeHandler(out ppMediaTypeHandler: IMFMediaTypeHandler): HResult;
      stdcall;
  end;
  //Interface IMFPresentationDescriptor */
  IMFPresentationDescriptor = interface(IMFAttributes)
    ['{03cb2711-24d7-4db6-a17f-f3a7a479a536}']
    function GetStreamDescriptorCount(out pdwDescriptorCount: DWord): HResult; stdcall;
    function GetStreamDescriptorByIndex(const dwIndex: Dword; out pfSelected: Bool;
      out ppDescriptor: IMFStreamDescriptor): HResult; stdcall;
    function SelectStream(const dwDescriptorIndex: DWord): HResult; stdcall;
    function DeselectStream(const dwDescriptorIndex: DWord): HResult; stdcall;
    function Clone(out ppPresentationDescriptor: IMFPresentationDescriptor): HResult;
      stdcall;
  end;

  IMFMediaSource = interface(IMFMediaEventGenerator)
    ['{90377834-21D0-4dee-8214-BA2E3E6C1127}']
    function GetCharacteristics(out pdwCharacteristics: DWord): HResult; stdcall;
    function CreatePresentationDescriptor(out ppPresentationDescriptor:
      IMFPresentationDescriptor): HResult; stdcall;
    function Start(const pPresentationDescriptor: IMFPresentationDescriptor;
      const pguidTimeFormat: TGuid; pvarStartPosition: PROPVARIANT): HResult; stdcall;
    function Stop(): HResult; stdcall;
    function Pause(): HResult; stdcall;
    function Shutdown(): HResult; stdcall;
  end;

  IMFActivate = interface(IUnknown)
    ['{7FEE9E9A-4A89-47a6-899C-B6A53A70FB67}']
    function ActivateObject(const riid: REFIID; out ppv: Pointer): HResult; stdcall;
    function ShutdownObject(): HResult; stdcall;
    function DetachObject(): HResult; stdcall;
  end;

  IMFPresentationTimeSource = interface(IUnknown)
    ['{7FF12CCE-F76F-41c2-863B-1666C8E5E139}']
    function GetUnderlyingClock(out ppClock: IMFClock): HResult; stdcall;
  end;

  IMFPresentationClock = interface(IUnknown)
    ['{868CE85C-8EA9-4f55-AB82-B009A910A805}']
    function SetTimeSource(const pTimeSource: IMFPresentationTimeSource): HResult;
      stdcall;
    function GetTimeSource(out ppTimeSource: IMFPresentationTimeSource): HResult;
      stdcall;
    function GetTime(out phnsClockTime: MFTIME): HResult; stdcall;
    //function AddClockStateSink(const pStateSink: IMFClockStateSink): HResult; stdcall;
    //function RemoveClockStateSink(const pStateSink: IMFClockStateSink): HResult; stdcall;
    function Start(const llClockStartOffset: LongLong): HResult; stdcall;
    function Stop(): HResult; stdcall;
    function Pause(): HResult; stdcall;
  end;

function MFStartup(const Version: ULONG; const dwFlags: DWORD): HRESULT; stdcall;
function MFShutdown: HRESULT; stdcall;
function MFCreateMediaSession(const pConfiguration: IMFAttributes;
  out ppMediaSession: IMFMediaSession): HResult; stdcall;

function MFCreateSourceResolver(out ppISourceResolver: IMFSourceResolver): HResult;
  stdcall;

function MFCreateTopology(out ppTopo: IMFTopology): HResult; stdcall;
function MFCreateTopologyNode(const NodeType: MF_TOPOLOGY_TYPE;
  out ppNode: IMFTopologyNode): HResult; stdcall;
function MFCreateAudioRendererActivate(out ppActivate: IMFActivate): HResult; stdcall;

function MFCreateSystemTimeSource(out ppSystemTimeSource: IMFPresentationTimeSource): HResult;
  stdcall;
function CoInitialize(pvReserved: Pointer): HResult; stdcall;
  external 'ole32.dll' Name 'CoInitialize';
procedure CoUninitialize; stdcall; external 'ole32.dll' Name 'CoUninitialize';
function CoCreateInstance(const clsid: TCLSID; unkOuter: IUnknown; dwClsContext: longint;
  const iid: TIID; out pv): HResult;
  stdcall; external 'ole32.dll' Name 'CoCreateInstance';


//----------------------------------------------------------------------------//
implementation
//----------------------------------------------------------------------------//


function MFStartup(const Version: ULONG; const dwFlags: DWORD): HRESULT;
  stdcall; external 'Mfplat.dll' Name 'MFStartup';
function MFShutdown: HRESULT; stdcall; external 'Mfplat.dll' Name 'MFShutdown';

function MFCreateMediaSession(const pConfiguration: IMFAttributes;
  out ppMediaSession: IMFMediaSession): HResult; stdcall;
  external 'Mf.dll' Name 'MFCreateMediaSession';

function MFCreateSourceResolver(out ppISourceResolver: IMFSourceResolver): HResult;
  stdcall;
  external 'Mf.dll' Name 'MFCreateSourceResolver';

function MFCreateTopology(out ppTopo: IMFTopology): HResult; stdcall;
  external 'Mf.dll' Name 'MFCreateTopology';

function MFCreateAudioRendererActivate(out ppActivate: IMFActivate): HResult; stdcall;
  external 'Mf.dll' Name 'MFCreateAudioRendererActivate';

function MFCreateTopologyNode(const NodeType: MF_TOPOLOGY_TYPE;
  out ppNode: IMFTopologyNode): HResult; stdcall;
  external 'Mf.dll' Name 'MFCreateTopologyNode';

function MFCreateSystemTimeSource(out ppSystemTimeSource: IMFPresentationTimeSource): HResult;
  stdcall;








  external 'Mfplat.dll' Name 'MFCreateSystemTimeSource';

initialization
  CoInitialize(nil);

finalization
  CoUninitialize;

end.
