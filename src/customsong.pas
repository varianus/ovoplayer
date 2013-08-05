unit CustomSong;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Song;

type

  { TCustomSong }

  TCustomSong = Class (tSong)
  private
    FAdded: TDateTime;
    FId: Integer;
    FLastPlay: TDateTime;
    FPlayCount: Integer;
    FRating: Integer;
    FtmpRating: integer;
    procedure SetAdded(AValue: TDateTime);
    procedure SetId(AValue: Integer);
    procedure SetLastPlay(AValue: TDateTime);
    procedure SetPlayCount(AValue: Integer);
    procedure SetRating(AValue: Integer);
    procedure SettmpRating(AValue: integer);
  public
    constructor Create(aFileName: string); override;
  public
    Property Id: Integer read FId write SetId;
    Property PlayCount : Integer read FPlayCount write SetPlayCount;
    Property Rating :Integer read FRating write SetRating;
    Property tmpRating: integer read FtmpRating write SettmpRating;
    Property Added : TDateTime read FAdded write SetAdded;
    Property LastPlay : TDateTime read FLastPlay write SetLastPlay;

  end;

implementation

{ TCustomSong }

procedure TCustomSong.SetAdded(AValue: TDateTime);
begin
  if FAdded=AValue then Exit;
  FAdded:=AValue;
end;

procedure TCustomSong.SetId(AValue: Integer);
begin
  if FId=AValue then Exit;
  FId:=AValue;
end;

procedure TCustomSong.SetLastPlay(AValue: TDateTime);
begin
  if FLastPlay=AValue then Exit;
  FLastPlay:=AValue;
end;

procedure TCustomSong.SetPlayCount(AValue: Integer);
begin
  if FPlayCount=AValue then Exit;
  FPlayCount:=AValue;
end;

procedure TCustomSong.SetRating(AValue: Integer);
begin
  if FRating=AValue then Exit;
  FRating:=AValue;
end;

procedure TCustomSong.SettmpRating(AValue: integer);
begin
  if FtmpRating=AValue then Exit;
  FtmpRating:=AValue;
end;

constructor TCustomSong.Create(aFileName: string);
begin
  inherited Create(aFileName);
  FId:= -1;
  fPlayCount := -1;
  fRating := -1;
  fTmpRating := -1;
  fAdded := 0;
  fLastPlay := 0;
end;

end.

