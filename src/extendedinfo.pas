unit ExtendedInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TExtendedInfo }

  TExtendedInfo = Class
    Id: Integer;
    PlayCount : Integer;
    Rating :Integer;
    Added : TDateTime;
    LastPlay : TDateTime;
  public
    constructor Create;
  end;


implementation

{ TExtendedInfo }

constructor TExtendedInfo.Create;
begin
   PlayCount := -1;
   Rating := -1;
   Added := 0;
   LastPlay := 0;
end;


end.

