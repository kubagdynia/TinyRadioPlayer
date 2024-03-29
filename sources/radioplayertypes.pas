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
  Classes, SysUtils, Contnrs;

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

{ - - - - - - - - - - - - - - - Import Data Status - - - - - - - - - - - - - - }
type
  TImportDataStatus = (idsStationAdded, idsStationUpdated, idsStationNotUpdatedCosTheSameData,
                       idsStationNotAddedBecauseAlreadyExists,
                       idsStationNotUpdatedBecauseNoDictionaryIten,
                       idsStationNotAddedBecauseNoDictionaryIten,
                       idsDictionaryAdded, idsDictionaryUpdated, idsDictionaryNotUpdatedCosTheSameData,
                       idsStationNotUpdatedBecauseAlreadyExists);

{ - - - - - - - - - - - - - - - - StationInfo - - - - - - - - - - - - - - - - - }
type
  TStationInfo = packed record
    Id                  : string;
    Name                : string;
    StreamUrl           : string;
    Description         : string;
    WebpageUrl          : string;
    GenreCode           : string;
    CountryCode         : string;
    RegionCode          : string;
  end;

{ - - - - - - - - - - - - - - - ApplicationInfo - - - - - - - - - - - - - - - - }
type
  TApplicationInfo = packed record
    CompanyName         : string;
    FileDescription     : string;
    FileVersion         : string;
    InternalName        : string;
    LegalCopyright      : string;
    OriginalFilename    : string;
    ProductName         : string;
    ProductVersion      : string;
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
    FID      : string;
    FName    : string;
    FGenre   : string;
    FCountry : string;
  public
    constructor Create(const Id: string;
      const Name, Genre, Country: string); overload;

    property ID       : string   read FID       write FID;
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
    FCompressorEnabled : boolean;
    FDefaultCompressorPreset: string;
    FDampEnabled : boolean;
    FDefaultDampPreset: string;

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
    property CompressorEnabled : boolean read FCompressorEnabled write FCompressorEnabled;
    property DefaultCompressorPreset: string read FDefaultCompressorPreset write FDefaultCompressorPreset;
    property DampEnabled : boolean read FDampEnabled write FDampEnabled;
    property DefaultDampPreset: string read FDefaultDampPreset write FDefaultDampPreset;
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
      constructor Create(const AName: string;
        const ABand1Gain: integer; const ABand2Gain: integer;
        const ABand3Gain: integer; const ABand4Gain: integer;
        const ABand5Gain: integer; const ABand6Gain: integer;
        const ABand7Gain: integer; const ABand8Gain: integer); overload;

      property Name      : string  read FName      write FName;
      property Band1Gain : integer read FBand1Gain write SetBand1Gain;
      property Band2Gain : integer read FBand2Gain write SetBand2Gain;
      property Band3Gain : integer read FBand3Gain write SetBand3Gain;
      property Band4Gain : integer read FBand4Gain write SetBand4Gain;
      property Band5Gain : integer read FBand5Gain write SetBand5Gain;
      property Band6Gain : integer read FBand6Gain write SetBand6Gain;
      property Band7Gain : integer read FBand7Gain write SetBand7Gain;
      property Band8Gain : integer read FBand8Gain write SetBand8Gain;
    end;

{ - - - - - - - - - - - - - - - - TCompressorPreset - - - - - - - - - - - - - - }
type
    TCompressorPreset = class
    private
      FName      : string;
      FGain      : single;
      FThreshold : single;
      FRatio     : single;
      FAttack    : single;
      FRelease   : single;
    public
      property Name      : string  read FName      write FName;
      property Gain      : single  read FGain      write FGain;
      property Threshold : single  read FThreshold write FThreshold;
      property Ratio     : single  read FRatio     write FRatio;
      property Attack    : single  read FAttack    write FAttack;
      property Release   : single  read FRelease   write FRelease;
    end;

{ - - - - - - - - - - - - - - - - - TDampPreset - - - - - - - - - - - - - - - }
type
    TDampPreset = class
    private
      FName      : string;
      FTarget    : single;
      FQuiet     : single;
      FRate      : single;
      FGain      : single;
      FDelay     : single;
    public
      property Name      : string  read FName      write FName;
      property Target    : single  read FTarget    write FTarget;
      property Quiet     : single  read FQuiet     write FQuiet;
      property Rate      : single  read FRate      write FRate;
      property Gain      : single  read FGain      write FGain;
      property Delay     : single  read FDelay     write FDelay;
    end;

{ - - - - - - - - - - - - - - - - - TStation - - - - - - - - - - - - - - - - - }
type
    TStation = class(TCollectionItem)
    private
      FID          : string;
      FName        : string;
      FStreamUrl   : string;
      FDescription : string;
      FWebpageUrl  : string;
      FGenreCode   : string;
      FCountryCode : string;
      FRegionCode  : string;
    public
      constructor Create(const AID: string;
        const AName: string; const AStreamUrl: string;
        const ADescription: string; const AWebpageUrl: string;
        const AGenreCode: string; const ACountryCode: string;
        const ARegionCode: string); overload;
    published
      property A_ID           : string read FID          write FID;
      property B_Name         : string read FName        write FName;
      property C_StreamUrl    : string read FStreamUrl   write FStreamUrl;
      property D_Description  : string read FDescription write FDescription;
      property E_WebpageUrl   : string read FWebpageUrl  write FWebpageUrl;
      property F_GenreCode    : string read FGenreCode   write FGenreCode;
      property G_CountryCode  : string read FCountryCode write FCountryCode;
      property H_RegionCode   : string read FRegionCode  write FRegionCode;
    end;

{ - - - - - - - - - - - - - - - - - TDictionary - - - - - - - - - - - - - - - - }
type
    TDictionary = class(TPersistent)
    const
      PropNameName         = 'A_Name';
      PropCodeName         = 'B_Code';
      PropDescriptionName  = 'C_Description';
      PropDetailsName      = 'D_Details';
    private
      FName        : string;
      FCode        : string;
      FDescription : string;
      FDetails     : TObjectList;
    public
      constructor Create(const AName: string;
        const ACode: string; const ADescription: string); overload;
      constructor Create(const AName: string;
        const ACode: string; const ADescription: string;
        const ADetails: TObjectList); overload;
      destructor Destroy; override;
    published
      property A_Name         : string      read FName        write FName;
      property B_Code         : string      read FCode        write FCode;
      property C_Description  : string      read FDescription write FDescription;
      property D_Details      : TObjectList read FDetails     write FDetails;
    end;

{ - - - - - - - - - - - - - - - - TDictionaryDetail - - - - - - - - - - - - - - }
type
    TDictionaryDetail = class(TPersistent)
    const
      PropTextName         = 'A_Text';
      PropCodeName         = 'B_Code';
      PropPositionName     = 'C_Position';
      PropChildName        = 'D_Child';
    private
      FText        : string;
      FCode        : string;
      FPosition    : integer;
      FChild       : TDictionary;
    public
      constructor Create(const AText: string;
        const ACode: string; const APosition: integer); overload;
      destructor Destroy; override;
    published
      property A_Text         : string      read FText        write FText;
      property B_Code         : string      read FCode        write FCode;
      property C_Position     : integer     read FPosition    write FPosition;
      property D_Child        : TDictionary read FChild       write FChild;
    end;

{ - - - - - - - - - - - - - - - - TExportImportDto - - - - - - - - - - - - - - }
type
    TExportImportDto = class(TPersistent)
    const
      PropDateName         = 'A_Date';
      PropStationsName     = 'B_Stations';
      PropDictionariesName = 'C_Dictionaries';
    private
      FDate         : string;
      FStations     : TObjectList;
      FDictionaries : TObjectList;
    public
      destructor Destroy; override;
    published
      property A_Date          : string      read FDate         write FDate;
      property B_Stations      : TObjectList read FStations     write FStations;
      property C_Dictionaries  : TObjectList read FDictionaries write FDictionaries;
    end;

{ - - - - - - - - - - - - - - TImportDataNodeData - - - - - - - - - - - - - - - }
type
  TImportDataNodeData = class
  protected
    FName             : string;
    FStatus           : string;
    FImportDataStatus : TImportDataStatus;
  public
    constructor Create(const Name, Status: string; ImportDataStatus: TImportDataStatus); overload;

    property Name             : string read FName   write FName;
    property Status           : string read FStatus write FStatus;
    property ImportDataStatus : TImportDataStatus read FImportDataStatus write FImportDataStatus;
  end;

  PImportDataNodeRec = ^TImportDataNodeRec;
  TImportDataNodeRec =
  record
     idnd : TImportDataNodeData;
  end;

{ **************************************************************************** }
{ **************************************************************************** }

implementation

{ TStationNodeData }

constructor TStationNodeData.Create(const Id: string; const Name, Genre,
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

{ TStation }

constructor TStation.Create(const AID: string; const AName: string;
  const AStreamUrl: string; const ADescription: string;
  const AWebpageUrl: string; const AGenreCode: string;
  const ACountryCode: string; const ARegionCode: string);
begin
  FID := AID;
  FName := AName;
  FStreamUrl := AStreamUrl;
  FDescription := ADescription;
  FWebpageUrl := AWebpageUrl;
  FGenreCode := AGenreCode;
  FCountryCode := ACountryCode;
  FRegionCode := ARegionCode;
end;

{ TDictionary }

constructor TDictionary.Create(const AName: string;
  const ACode: string; const ADescription: string);
begin
  FName := AName;
  FCode := ACode;
  FDescription := ADescription;
end;

constructor TDictionary.Create(const AName: string;
  const ACode: string; const ADescription: string; const ADetails: TObjectList);
begin
  FName := AName;
  FCode := ACode;
  FDescription := ADescription;
  FDetails := ADetails;
end;

destructor TDictionary.Destroy;
begin
  if Assigned(FDetails) then
    FDetails.Free;

  inherited Destroy;
end;

{ TDictionaryDetail }

constructor TDictionaryDetail.Create(const AText: string; const ACode: string;
  const APosition: integer);
begin
  FText := AText;
  FCode := ACode;
  FPosition := APosition;
end;

destructor TDictionaryDetail.Destroy;
begin
  if Assigned(FChild) then
    FChild.Free;

  inherited Destroy;
end;

{ TExportImportDto }

destructor TExportImportDto.Destroy;
begin
  if Assigned(FStations) then
    FStations.Free;
  if Assigned(FDictionaries) then
    FDictionaries.Free;

  inherited Destroy;
end;

{ TImportDataNodeData }

constructor TImportDataNodeData.Create(const Name, Status: string;
  ImportDataStatus: TImportDataStatus);
begin
  inherited Create;

  FName := Name;
  FStatus := Status;
  FImportDataStatus := ImportDataStatus;
end;

end.

