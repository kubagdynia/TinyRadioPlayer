unit ExportImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fpjsonrtti, contnrs, typinfo, rttiutils,
  RadioPlayerTypes;

type
  TImportStationEvent = procedure(AStationInfo: TStationInfo; AImportDataStatus: TImportDataStatus) of object;

type

  { TExportImport }

  TExportImport = class (TObject)
  private
    FOnImportStation: TImportStationEvent;

    function SerializeObjectToJSON(AObject: TObject): string;
    function GetFormatedJsonString(var exportImportDto: TExportImportDto): string;

    procedure SaveStringToFile(Str: string; FilePath: string);
    function LoadFileToString(FilePath: string): string;

    function ParseImportFile(const FilePath: string): TExportImportDto;
    procedure ParseStations(var dto: TExportImportDto; const jData: TJSONData);
    procedure ParseDictionaries(var dto: TExportImportDto; const jData: TJSONData);
    function ParseDictionary(const jData: TJSONData): TDictionary;

    procedure ImportStations(const dto: TExportImportDto);

    procedure JSONToObject(const JSON: TJSONData; AObject: TObject);
    procedure JSONToObject(const JSON: TJSONObject; AObject: TObject);
    procedure DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo; PropData: TJSONData);
  public
    constructor Create(); overload;
    destructor Destroy; override;

    function ExportToJsonFile(FilePath: string;
      ExportStations: boolean = true; ExportDictionaries: boolean = true): ErrorId;
    procedure ImportFromJsonFile(FilePath: string;
      ExportStations: boolean = true; ExportDictionaries: boolean = true);

    property OnImportStation: TImportStationEvent read FOnImportStation write FOnImportStation;
  end;


implementation

uses
  Repository, TRPErrors, Helpers, Consts;

constructor TExportImport.Create();
begin

end;

destructor TExportImport.Destroy;
begin
  inherited Destroy;
end;

function TExportImport.ExportToJsonFile(FilePath: string;
  ExportStations: boolean = true; ExportDictionaries: boolean = true): ErrorId;
var
  err: ErrorId;
  content: string;
  exportImportDto: TExportImportDto;
  stationList: TObjectList;
  dictionaryList: TObjectList;
begin
  err := ERR_OK;

  try

    if ExportStations then
      err := TRepository.GetAllStations(stationList);

    if (ExportDictionaries) and (err = ERR_OK) then
      err := TRepository.GetAllDictionaries(dictionaryList);

    if err = ERR_OK then
    begin
      exportImportDto := TExportImportDto.Create;
      try
        // prepare data
        exportImportDto.A_Date := DateTimeToStr(Now);

        if ExportStations then
          exportImportDto.B_Stations := stationList;

        if ExportDictionaries then
          exportImportDto.C_Dictionaries := dictionaryList;

        // and save
        content := GetFormatedJsonString(exportImportDto);
        SaveStringToFile(content, FilePath);
      finally
        FreeAndNil(exportImportDto);
      end;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'ExportToJsonFile', E);
        err := ERR_EXPORT_TO_JSON_FILE;
      end;
  end;

  Result := err;
end;

procedure TExportImport.ImportFromJsonFile(FilePath: string;
  ExportStations: boolean = true; ExportDictionaries: boolean = true);
var
  exportImportDto: TExportImportDto;
begin
  exportImportDto := ParseImportFile(FilePath);
  try
    if ExportStations then
      ImportStations(exportImportDto);

  finally
    exportImportDto.Free;
  end;
end;

procedure TExportImport.ImportStations(const dto: TExportImportDto);
var
  i: integer;
  station: TStation;
  err: ErrorId;
  isExists: boolean;
  stationInfo: TStationInfo;
  stationId: string;
begin
  if (dto = nil) or (dto.B_Stations = nil) or (dto.B_Stations.Count = 0) then
    exit;

  err := ERR_OK;

  for i := 0 to dto.B_Stations.Count - 1 do
  begin
    station := dto.B_Stations[i] as TStation;

    isExists := false;
    err := TRepository.IsStationExists(station.A_ID, isExists);

    if isExists then
    begin
      // Update station
      with stationInfo do
      begin
        Id := station.A_ID;
        Name := station.B_Name;
        StreamUrl := station.C_StreamUrl;
        Description := station.D_Description;
        WebpageUrl := station.E_WebpageUrl;
        GenreCode := station.F_GenreCode;
        CountryCode := station.G_CountryCode;
        RegionCode := station.H_RegionCode;
      end;

      err := TRepository.UpdateStation(stationInfo);

      // call event
      if Assigned(OnImportStation) then
      begin
        if (err = ERR_OK) then
          OnImportStation(stationInfo, idsStationUpdated)
        else if (err = ERR_DB_DATA_ARE_THE_SAME_STATION_UPDATE_IS_NOT_NEEDED) then
          OnImportStation(stationInfo, idsStationNotUpdatedCosTheSameData)
        else if (err = ERR_DICTIONARY_NOT_ROW_EXISTS) then
          OnImportStation(stationInfo, idsStationNotUpdatedBecauseNoDictionaryIten)
        else if (err = ERR_DB_STATION_ALREADY_EXISTS) then
          OnImportStation(stationInfo, idsStationNotUpdatedBecauseAlreadyExists);
      end;

    end else
    begin
      // Add station
      with stationInfo do
      begin
        Name := station.B_Name;
        StreamUrl := station.C_StreamUrl;
        Description := station.D_Description;
        WebpageUrl := station.E_WebpageUrl;
        GenreCode := station.F_GenreCode;
        CountryCode := station.G_CountryCode;
        RegionCode := station.H_RegionCode;
      end;

      err := TRepository.AddStation(stationInfo, stationId);

      // call event
      if Assigned(OnImportStation) then
      begin
        if (err = ERR_OK) then
          OnImportStation(stationInfo, idsStationAdded)
        else if (err = ERR_DICTIONARY_NOT_ROW_EXISTS) then
          OnImportStation(stationInfo, idsStationNotAddedBecauseNoDictionaryIten)
        else if (err = ERR_DB_STATION_ALREADY_EXISTS) then
          OnImportStation(stationInfo, idsStationNotAddedBecauseAlreadyExists)
      end;

    end;

  end;


end;

function TExportImport.ParseImportFile(const FilePath: string): TExportImportDto;
var
  strContent: string;
  jData: TJSONData;
  exportImportDto: TExportImportDto;
  jsEnum: TJSONEnum;
begin
  exportImportDto := nil;

  strContent := LoadFileToString(FilePath);

  // create from string
  jData := GetJSON(strContent);
  try
    exportImportDto := TExportImportDto.Create;

    for TJSONEnum(jsEnum) in jData do
    begin
      // Date
      if (jsEnum.Value.JSONType = jtString) and (jsEnum.Key = TExportImportDto.PropDateName) then
        exportImportDto.A_Date :=  jsEnum.Value.AsString
      else if (jsEnum.Value.JSONType = jtArray) then
      begin
        // Stations
        if (jsEnum.Key = TExportImportDto.PropStationsName) then
          ParseStations(exportImportDto, jsEnum.Value)
        // Dictionaries
        else if (jsEnum.Key = TExportImportDto.PropDictionariesName) then
          ParseDictionaries(exportImportDto, jsEnum.Value);
      end;
    end;
  finally
    jData.Free;
  end;

  Result := exportImportDto;
end;

procedure TExportImport.ParseStations(var dto: TExportImportDto;
  const jData: TJSONData);
var
  jsoDeserialize: TJSONDeStreamer;
  coll: TCollection;
  i: integer;
  station: TStation;
  stationList: TObjectList;
begin
  if not Assigned(dto) then
    Exit;

  jsoDeserialize := TJSONDeStreamer.Create(nil);
  try
    coll := TCollection.Create(TStation);
    try
      jsoDeserialize.JSONToCollection(jData, coll);

      stationList := TObjectList.Create(true);

      for i := 0 to coll.Count - 1 do
      begin
        station := coll.Items[i] as TStation;

        if Assigned(station) then
          stationList.Add(
            TStation.Create(
              station.A_ID,
              station.B_Name,
              station.C_StreamUrl,
              station.D_Description,
              station.E_WebpageUrl,
              station.F_GenreCode,
              station.G_CountryCode,
              station.H_RegionCode
            )
          );

      end;

      dto.B_Stations := stationList;

    finally
      coll.Free;
    end;
  finally
    jsoDeserialize.Free;
  end;

end;

procedure TExportImport.ParseDictionaries(var dto: TExportImportDto;
  const jData: TJSONData);
var
  jsoDeserialize: TJSONDeStreamer;
  dictionary: TDictionary;
  dictionaryList: TObjectList;
  jsEnum: TJSONEnum;
begin
  if not Assigned(dto) then
    Exit;

  jsoDeserialize := TJSONDeStreamer.Create(nil);
  try

    dictionaryList := TObjectList.Create(true);

    for TJSONEnum(jsEnum) in jData do
    begin
      if (jsEnum.Value.JSONType = jtObject)  then
      begin
        dictionary := ParseDictionary(jsEnum.Value);
        dictionaryList.Add(dictionary);
      end;

    end;

    dto.C_Dictionaries := dictionaryList;

    TRepository.ImportDictionaries(dto);

  finally
    jsoDeserialize.Free;
  end;
end;

function TExportImport.ParseDictionary(const jData: TJSONData): TDictionary;
var
  dictionary: TDictionary;
  detail: TDictionaryDetail;
  jsEnum, jsEnum2, jsEnum3: TJSONEnum;
begin
  Result := nil;

  if jData = nil then
    exit;

  dictionary := TDictionary.Create();
  JSONToObject(jData, dictionary);

  for TJSONEnum(jsEnum) in jData do // iterate over dictionary to find detail items
  begin
    if (jsEnum.Value.JSONType = jtArray) and (jsEnum.Key = TDictionary.PropDetailsName)  then
    begin

      if not Assigned(dictionary.D_Details) then
        dictionary.D_Details := TObjectList.Create(true);

      for TJSONEnum(jsEnum2) in jsEnum.Value do // iterate over dictionary details
      begin
        detail := TDictionaryDetail.Create;
        JSONToObject(jsEnum2.Value, detail);

        for TJSONEnum(jsEnum3) in jsEnum2.Value do // iterate over detail items to find child object
          if (jsEnum3.Value.JSONType = jtObject) and (jsEnum3.Key = TDictionaryDetail.PropChildName) then // child object
            detail.D_Child := ParseDictionary(jsEnum3.Value);

        dictionary.D_Details.Add(detail);
      end;
    end;
  end;

  result := dictionary;
end;

function TExportImport.SerializeObjectToJSON(AObject: TObject): string;
var
  jsoSerialize: TJSONStreamer;
begin
  jsoSerialize := TJSONStreamer.Create(nil);

  try
    Result := jsoSerialize.ObjectToJSONString(AObject);
  finally
    jsoSerialize.Free;
  end;

end;

procedure TExportImport.SaveStringToFile(Str: string; FilePath: string);
var
  f: TMemoryStream;
begin
  f := TMemoryStream.Create;
  try
    f.WriteBuffer(Pointer(Str)^, Length(Str));
    f.Position := 0;
    f.SaveToFile(FilePath);
  finally
   f.Free;
  end;
end;

function TExportImport.LoadFileToString(FilePath: string): string;
var
  f: TMemoryStream;
begin
  Result := EmptyStr;

  f := TMemoryStream.Create;
  try
    f.LoadFromFile(FilePath);
    Result := StrPas(f.Memory);
  finally
   f.Free;
  end;
end;

function TExportImport.GetFormatedJsonString(var exportImportDto: TExportImportDto): string;
var
  strContent: string;
  jData: TJSONData;
begin
  strContent := SerializeObjectToJSON(exportImportDto);
  jData := GetJSON(strContent);
  try
    Result := jData.FormatJSON;
  finally
    jData.Free;
  end;
end;

procedure TExportImport.JSONToObject(const JSON: TJSONData; AObject: TObject);
begin
  JSONToObject(JSON as TJSONObject, AObject);
end;

procedure TExportImport.JSONToObject(const JSON: TJSONObject; AObject: TObject);
var
  i, j: Integer;
  pil: TPropInfoList;
begin
  pil := TPropInfoList.Create(AObject, tkProperties);

  try
    for i := 0 to pil.Count-1 do
    begin
      J := JSON.IndexOfName(pil.Items[i]^.Name, true);
      if (J <> -1) then
        DoRestoreProperty(AObject, pil.Items[i], JSON.Items[j]);
    end;

  finally
    FreeAndNil(pil);
  end;

end;

procedure TExportImport.DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo;
  PropData: TJSONData);
var
  pi: PPropInfo;
  ti: PTypeInfo;
  i: Integer;
  js: TJSONStringType;
begin
  pi := PropInfo;
  ti := PropInfo^.PropType;

  case ti^.Kind of
    tkInteger:
      SetOrdProp(AObject, pi, PropData.AsInteger);
    tkInt64:
      SetOrdProp(AObject, pi, PropData.AsInt64);
    tkFloat:
      SetFloatProp(AObject, pi, PropData.AsFloat);
    tkEnumeration:
      begin
        if (PropData.JSONType = jtNumber) then
          i := PropData.AsInteger
        else if PropData.JSONType = jtString then
          i := GetEnumValue(ti, PropData.AsString);

        SetOrdProp(AObject, pi, i);
      end;
    tkSString,
    tkLString,
    tkAString:
      SetStrProp(AObject, pi, PropData.AsString);
    tkWString:
      SetWideStrProp(AObject, pi, PropData.AsUnicodeString);
    tkWChar:
      begin
        js := PropData.AsString;
        if (js <> '') then
          SetOrdProp(AObject, pi, Ord(js[1]));
      end;
    tkBool:
      SetOrdProp(AObject, pi, Ord(PropData.AsBoolean));
    tkQWord:
      SetOrdProp(AObject, pi, Trunc(PropData.AsFloat));
    tkUString:
      SetUnicodeStrProp(AObject, pi, PropData.AsUnicodeString);
    tkChar,
    tkUChar:
      begin
        js := PropData.AsString;
        if (js <> '') then
          SetOrdProp(AObject, pi, Ord(js[1]));
      end;
  end;
end;

end.

