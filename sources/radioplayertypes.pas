unit RadioPlayerTypes;
{===============================================================================
File:                RadioPlayerTypes.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Types and classes used in the application

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ - - - - - - - - - - - - - - - - - Enums - - - - - - - - - - - - - - - - - -  }
type
  TPlayerMessageType = (Connecting=0, Error=1, Progress=2, StreamName=3, Bitrate=4, StreamTitle=7, Channels=10, Freq=11, Other=25);

{ - - - - - - - - - - - - - - - - Error Type - - - - - - - - - - - - - - - - - }
type
  ErrorId = integer;

{ - - - - - - - - - - - - - - - - - - Forms - - - - - - - - - - - - - - - - - -}
type
  TOpenMode = (omNew, omEdit, omDelete, omNormal);

{ - - - - - - - - - - - - - - - - - Dictionary - - - - - - - - - - - - - - - - }
type
  TDictionaryType = (dkNone, dkGenre, dkRegion, dkCountry);

{ - - - - - - - - - - - - - - - - StationInfo - - - - - - - - - - - - - - - - - }
type
  TStationInfo = packed record
    Id                  : integer;
    Name                : string;
    StreamUrl           : string;
    Description         : string;
    WebpageUrl          : string;
    GenreCode           : string;
    CountryCode         : string;
  end;

{ - - - - - - - - - - - - - - - - DictionaryTable - - - - - - - - - - - - - - -}
type
  PDictionaryTable = ^TDictionaryTable;
  TDictionaryTable = record
    Id                      : integer;
    Text                    : string;
    Code                    : string;
    ParentDictionaryCode    : string;
    ParentDictionaryRowCode : string;
    Position                : integer;
  end;

{ - - - - - - - - - - - - - - - TStationNodeData  - - - - - - - - - - - - - - - }
type
  TStationNodeData = class
  protected
    FID      : integer;
    FName    : string;
    FGenre   : string;
    FCountry : string;
  public
    constructor Create(const Id: integer;
      const Name, Genre, Country: string); overload;

    property ID       : integer  read FID       write FID;
    property Name     : string   read FName     write FName;
    property Genre    : string   read FGenre    write FGenre;
    property Country  : string   read FCountry  write FCountry;
  end;

  PStationNodeRec = ^TStationNodeRec;
  TStationNodeRec =
  record
     snd : TStationNodeData;
  end;

{ - - - - - - - - - - - - - TDictionaryTableNodeData - - - - - - - - - - - - - }
type
  TDictionaryTableNodeData = class
  protected
    FName           : string;
    FTableName      : string;
    FDictionaryType : TDictionaryType;
  public
    constructor Create(const Name, TableName: string; DictionaryType: TDictionaryType); overload;

    property Name           : string read FName      write FName;
    property TableName      : string read FTableName write FTableName;
    property DictionaryType : TDictionaryType read FDictionaryType;
  end;

  PDictionaryTableNodeRec = ^TDictionaryTableNodeRec;
  TDictionaryTableNodeRec =
  record
     dtnd : TDictionaryTableNodeData;
  end;

{ - - - - - - - - - - - - - TDictionaryDetailTableNodeData - - - - - - - - - - }
type
  TDictionaryDetailTableNodeData = class
  protected
    FID        : integer;
    FText      : string;
    FCode      : string;
    FPosition  : integer;
  public
    constructor Create(const Id: integer; const Text: string; const Code: string;
      const Position: integer); overload;

    property ID       : integer read FID write FID;
    property Text     : string read FText write FText;
    property Code     : string read FCode write FCode;
    property Position : integer read FPosition write FPosition;
  end;

  PDictionaryDetailTableNodeRec = ^TDictionaryDetailTableNodeRec;
  TDictionaryDetailTableNodeRec =
  record
     ddtnd : TDictionaryDetailTableNodeData;
  end;

{ - - - - - - - - - - - - - - - - TEqualizerConfig - - - - - - - - - - - - - - }
type
  TEqualizerConfig = class
  private
    FEnabled       : boolean;
    FBandwidth     : single;
    FBand1Center   : integer;
    FBand2Center   : integer;
    FBand3Center   : integer;
    FBand4Center   : integer;
    FBand5Center   : integer;
    FBand6Center   : integer;
    FBand7Center   : integer;
    FBand8Center   : integer;
    FDefaultPreset : string;

    procedure SetBandwidth(AValue: single);
  public
    property Enabled       : boolean read FEnabled       write FEnabled;
    property Bandwidth     : single  read FBandwidth     write SetBandwidth;
    property Band1Center   : integer read FBand1Center   write FBand1Center;
    property Band2Center   : integer read FBand2Center   write FBand2Center;
    property Band3Center   : integer read FBand3Center   write FBand3Center;
    property Band4Center   : integer read FBand4Center   write FBand4Center;
    property Band5Center   : integer read FBand5Center   write FBand5Center;
    property Band6Center   : integer read FBand6Center   write FBand6Center;
    property Band7Center   : integer read FBand7Center   write FBand7Center;
    property Band8Center   : integer read FBand8Center   write FBand8Center;
    property DefaultPreset : string  read FDefaultPreset write FDefaultPreset;
  end;

{ - - - - - - - - - - - - - - - - TEqualizerPreset - - - - - - - - - - - - - - }
type
    TEqualizerPreset = class
    private
      FName      : string;
      FBand1Gain : integer;
      FBand2Gain : integer;
      FBand3Gain : integer;
      FBand4Gain : integer;
      FBand5Gain : integer;
      FBand6Gain : integer;
      FBand7Gain : integer;
      FBand8Gain : integer;

      procedure SetBand1Gain(AValue: integer);
      procedure SetBand2Gain(AValue: integer);
      procedure SetBand3Gain(AValue: integer);
      procedure SetBand4Gain(AValue: integer);
      procedure SetBand5Gain(AValue: integer);
      procedure SetBand6Gain(AValue: integer);
      procedure SetBand7Gain(AValue: integer);
      procedure SetBand8Gain(AValue: integer);
    public
      constructor Create (const AName: string;
        const ABand1Gain: integer; const ABand2Gain: integer; const ABand3Gain: integer;
        const ABand4Gain: integer; const ABand5Gain: integer; const ABand6Gain: integer;
        const ABand7Gain: integer ;const ABand8Gain: integer); overload;

      property Name      : string read FName write FName;
      property Band1Gain : integer read FBand1Gain write SetBand1Gain;
      property Band2Gain : integer read FBand2Gain write SetBand2Gain;
      property Band3Gain : integer read FBand3Gain write SetBand3Gain;
      property Band4Gain : integer read FBand4Gain write SetBand4Gain;
      property Band5Gain : integer read FBand5Gain write SetBand5Gain;
      property Band6Gain : integer read FBand6Gain write SetBand6Gain;
      property Band7Gain : integer read FBand7Gain write SetBand7Gain;
      property Band8Gain : integer read FBand8Gain write SetBand8Gain;
    end;

implementation

{ TStationNodeData }

constructor TStationNodeData.Create(const Id: integer; const Name, Genre,
  Country: string);
begin
  inherited Create;

  FID       := Id;
  FName     := Name;
  FGenre    := Genre;
  FCountry  := Country;
end;

{ TDictionaryTableNodeData }

constructor TDictionaryTableNodeData.Create(const Name, TableName: string;
  DictionaryType: TDictionaryType);
begin
  inherited Create;

  FName := Name;
  FTableName := TableName;
  FDictionaryType := DictionaryType;
end;

{ TDictionaryDetailTableNodeData }

constructor TDictionaryDetailTableNodeData.Create(const Id: integer;
  const Text: string; const Code: string; const Position: integer);
begin
  inherited Create;

  FID := Id;
  FText := Text;
  FCode := Code;
  FPosition := Position;
end;

{TEqualizerConfig }

procedure TEqualizerConfig.SetBandwidth(AValue: single);
begin
  if FBandwidth = AValue then Exit;

  if AValue < 0.1 then
    AValue := 0.1
  else if AValue >= 10 then
    AValue := 9.9;

  FBandwidth:=AValue;
end;

{ TEqualizerPreset }

constructor TEqualizerPreset.Create(const AName: string;
  const ABand1Gain: integer; const ABand2Gain: integer;
  const ABand3Gain: integer; const ABand4Gain: integer;
  const ABand5Gain: integer; const ABand6Gain: integer;
  const ABand7Gain: integer; const ABand8Gain: integer);
begin
  inherited Create;

  FName := AName ;
  FBand1Gain := ABand1Gain;
  FBand2Gain := ABand2Gain;
  FBand3Gain := ABand3Gain;
  FBand4Gain := ABand4Gain;
  FBand5Gain := ABand5Gain;
  FBand6Gain := ABand6Gain;
  FBand7Gain := ABand7Gain;
  FBand8Gain := ABand8Gain;
end;

procedure TEqualizerPreset.SetBand1Gain(AValue: integer);
begin
  if FBand1Gain = AValue then Exit;

  if AValue < -12 then
    AValue := -12
  else if AValue > 12 then
    AValue := 12;

  FBand1Gain := AValue;
end;

procedure TEqualizerPreset.SetBand2Gain(AValue: integer);
begin
  if FBand2Gain = AValue then Exit;

  if AValue < -12 then
    AValue := -12
  else if AValue > 12 then
    AValue := 12;

  FBand2Gain := AValue;
end;

procedure TEqualizerPreset.SetBand3Gain(AValue: integer);
begin
  if FBand3Gain = AValue then Exit;

  if AValue < -12 then
    AValue := -12
  else if AValue > 12 then
    AValue := 12;

  FBand3Gain := AValue;
end;

procedure TEqualizerPreset.SetBand4Gain(AValue: integer);
begin
  if FBand4Gain = AValue then Exit;

  if AValue < -12 then
    AValue := -12
  else if AValue > 12 then
    AValue := 12;

  FBand4Gain := AValue;
end;

procedure TEqualizerPreset.SetBand5Gain(AValue: integer);
begin
  if FBand5Gain = AValue then Exit;

  if AValue < -12 then
    AValue := -12
  else if AValue > 12 then
    AValue := 12;

  FBand5Gain := AValue;
end;

procedure TEqualizerPreset.SetBand6Gain(AValue: integer);
begin
  if FBand6Gain = AValue then Exit;

  if AValue < -12 then
    AValue := -12
  else if AValue > 12 then
    AValue := 12;

  FBand6Gain := AValue;
end;

procedure TEqualizerPreset.SetBand7Gain(AValue: integer);
begin
  if FBand7Gain = AValue then Exit;

  if AValue < -12 then
    AValue := -12
  else if AValue > 12 then
    AValue := 12;

  FBand7Gain := AValue;
end;

procedure TEqualizerPreset.SetBand8Gain(AValue: integer);
begin
  if FBand8Gain = AValue then Exit;

  if AValue < -12 then
    AValue := -12
  else if AValue > 12 then
    AValue := 12;

  FBand8Gain := AValue;
end;

end.

