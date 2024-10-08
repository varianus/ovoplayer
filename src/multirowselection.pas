unit MultiRowSelection;

{$mode ObjFPC}{$H+}

interface

type
  TMovingSelection = (msNone, msUp, msDown);

  TRowsSelection = class(TObject)

  private
  type
    TDWordArray = array [0..$FFFFFF] of Dword;
    PDWordArray = ^TDWordArray;
  private
    fArray: PDWordArray;
    fDwordSize: integer;
    fsize: integer;
    function GetSelected(Index: integer): boolean;
    procedure SetSelected(Index: integer; AValue: boolean);
    procedure SetSize(Size: integer);
  public
    constructor Create;
    procedure ClearAll;
    procedure SelectAll;
    function FirstSelected: integer;
    function LastSelected: integer;
    function NextSelected(index: integer = -1): integer;
    function PreviousSelected(index: integer = -1): integer;
    function isMultiselection: boolean;
    function IsContiguous: boolean;
    procedure SelectRange(var Anchor: integer; OldRow, NewRow: integer);
    property Size: integer read FSize write SetSize;
    property Selected[Index: integer]: boolean read GetSelected write SetSelected; default;

  end;

implementation

uses Math;

  { TRowsSelection }

function TRowsSelection.GetSelected(Index: integer): boolean;
var
  DWordIndex, BitIndex: integer;
begin
  if fDwordSize < 1 then
  begin
    Result := False;
    exit;
  end;
  DivMod(Index, 32, DWordIndex, BitIndex);
  Result := (fArray^[DWordIndex] and DWORD(1 shl (BitIndex))) <> 0;
  //  DebugLn('Get ', inttostr(fArray^[DWordIndex]), ' ', BoolToStr(Result, true), ' ', IntToStr(index) );
end;

procedure TRowsSelection.SetSelected(Index: integer; AValue: boolean);
var
  DWordIndex, BitIndex: integer;
begin
  if fDwordSize < 1 then
    exit;

  if (Index > size - 1) or (Index < 0) then
    exit;

  DivMod(Index, 32, DWordIndex, BitIndex);
  if AValue then
    fArray^[DWordIndex] := fArray^[DWordIndex] or DWORD(1 shl (BitIndex))
  else
    fArray^[DWordIndex] := fArray^[DWordIndex] and not DWORD(1 shl (BitIndex));
  //  DebugLn('Set ', inttostr(fArray^[DWordIndex]), ' ', BoolToStr(AValue, true), ' ', IntToStr(index) );
end;

constructor TRowsSelection.Create;
begin
  fDwordSize := -1;
  fArray := nil;
end;

procedure TRowsSelection.ClearAll;
var
  i: integer;
begin
  for i := 0 to fDwordSize - 1 do
    fArray^[i] := DWord($0);
end;

procedure TRowsSelection.SelectAll;
var
  i: integer;
begin
  for i := 0 to fDwordSize - 2 do
    fArray^[i] := DWord(not $0);

  if fDwordSize > 0 then
    // Clean up unused bits so LastSelected work give right result
    fArray^[fDwordSize - 1] := not ($ffffffff shl (size mod 32));

end;

procedure TRowsSelection.SetSize(Size: integer);
begin
  fDwordSize := ceil(Size / 32);
  ReAllocMem(fArray, fDwordSize * SizeOf(DWORD));
  fsize := Size;
  ClearAll;
end;

function TRowsSelection.FirstSelected: integer;
const
  fSkippableValue = $00000000;
var
  DWordIndex, BitIndex: integer;
  tmpDWord: DWORD;
begin
  Result := -1;
  BitIndex := 0;
  for DWordIndex := 0 to fDwordSize - 1 do
  begin
    if fArray^[DWordIndex] = fSkippableValue then
      Continue;
    tmpDWord := fArray^[DWordIndex];
    BitIndex := 0;
    while (tmpDWord and 1) = 0 do
    begin
      tmpDWord := tmpDWord shr 1;
      Inc(BitIndex);
    end;
    Result := DWordIndex * 32 + BitIndex;
    exit;
  end;
end;

function TRowsSelection.LastSelected: integer;
const
  fSkippableValue = $00000000;
var
  DWordIndex, BitIndex: integer;
  tmpDWord: DWORD;
begin
  Result := -1;
  BitIndex := 31;
  for DWordIndex := fDwordSize - 1 downto 0 do
  begin
    if fArray^[DWordIndex] = fSkippableValue then
      Continue;
    tmpDWord := fArray^[DWordIndex];
    BitIndex := 31;
    while (tmpDWord and $80000000) = 0 do
    begin
      tmpDWord := tmpDWord shl 1;
      Dec(BitIndex);
    end;
    Result := DWordIndex * 32 + BitIndex;
    exit;
  end;

end;

function TRowsSelection.NextSelected(index: integer = -1): integer;
var
  i: integer;
begin
  Result := -1;
  if index = -1 then
  begin
    Result := FirstSelected;
    exit;
  end;

  for i := (index + 1) to fsize - 1 do
    if GetSelected(i) then
    begin
      Result := i;
      exit;
    end;
end;

function TRowsSelection.PreviousSelected(index: integer = -1): integer;
var
  i: integer;
begin
  Result := -1;
  if index = -1 then
  begin
    Result := LastSelected;
    exit;
  end;

  for i := index - 1 downto 0 do
    if GetSelected(i) then
    begin
      Result := i;
      exit;
    end;
end;

function TRowsSelection.isMultiselection: boolean;
var
  idx: integer;
begin
  idx := FirstSelected;
  Result := not (NextSelected(idx) = -1);

end;

function TRowsSelection.IsContiguous: boolean;
var
  idx: integer;
begin
  Result := True;
  for idx := FirstSelected to LastSelected do
  begin
    Result := Result and Selected[idx];
    if not Result then exit;
  end;

end;

procedure TRowsSelection.SelectRange(var Anchor: integer; OldRow, NewRow: integer);
var
  dir: integer;
  sel: boolean;
begin
  if OldRow = NewRow then
    exit;
  if Anchor = -1 then
    Anchor := OldRow;
  dir := Sign(NewRow - OldRow);
  if Sign(Anchor - OldRow) <> Sign(Anchor - NewRow) then
    while OldRow <> Anchor do
    begin
      SetSelected(OldRow, False);
      Inc(OldRow, dir);
    end;
  sel := Abs(Anchor - OldRow) < Abs(Anchor - NewRow);
  while OldRow <> NewRow do
  begin
    SetSelected(OldRow, sel);
    Inc(OldRow, dir);
  end;
  SetSelected(NewRow, True);
end;


end.
