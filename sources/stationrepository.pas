unit StationRepository;
{===============================================================================
File:                StationRepository.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Database operations related to station data management

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioPlayerTypes, VirtualTrees;

type

  { TStationRepository }

  TStationRepository = class (TObject)
  private

  protected

  public
    constructor Create; overload;
    destructor Destroy; override;

    function AddStation(const AStationName: string; const AStreamUrl: string;
      out AStationId: string): ErrorId;
    function AddStation(const AStationName: string; const AStreamUrl: string;
      const ADescription: string; const AWebpageUrl: string;
      const AGenreCode: string; const ACountryCode: string; const ARegionCode: string;
      out AStationId: string): ErrorId;
    function AddStation(AStationInfo: TStationInfo; out AStationId: string): ErrorId;

    function UpdateStation(StationInfo: TStationInfo): ErrorId;

    function DeleteStation(StationId: string): ErrorId;

    function IsStationExists(StationName: string; ExcludeStationId: string;
      out IsExists: boolean): ErrorId;

    function DoesAnyStationUseTheGivenItemOfTheDictionary(
      DictionaryType: TDictionaryType; DictionaryRowCode: string;
      out ItemIsUsed: boolean): ErrorId;

    function UpdateStationDictionaryCode(DictionaryType: TDictionaryType;
      OldCode: string; NewCode: string): ErrorId;

    function LoadStation(var StationInfo: TStationInfo; const StationId: string): ErrorId;
    function LoadStations(var VstList: TVirtualStringTree; const Text: string): ErrorId;

    function GetSelectedStationId(var VstList: TVirtualStringTree): string;
  end;

implementation

uses
  Dialogs, ZDataset, Consts, Helpers, Repository, TRPErrors;

{ TStationRepository }

constructor TStationRepository.Create;
begin
  inherited Create;

  // Code here
end;

destructor TStationRepository.Destroy;
begin
  // Code here

  inherited Destroy;
end;

function TStationRepository.AddStation(const AStationName: string;
  const AStreamUrl: string; out AStationId: string): ErrorId;
begin
  Result := AddStation(AStationName, AStreamUrl, EMPTY_STR, EMPTY_STR, EMPTY_STR, EMPTY_STR, EMPTY_STR, AStationId);
end;

function TStationRepository.AddStation(const AStationName: string;
  const AStreamUrl: string; const ADescription: string; const AWebpageUrl: string;
  const AGenreCode: string; const ACountryCode: string; const ARegionCode: string;
  out AStationId: string): ErrorId;
var
  err: ErrorId;
  stationInfo: TStationInfo;
begin
  err := ERR_OK;

  with stationInfo do
    begin
      Id := EMPTY_STR;
      Name := AStationName;
      StreamUrl := AStreamUrl;
      Description := ADescription;
      WebpageUrl := AWebpageUrl;
      GenreCode := AGenreCode;
      CountryCode := ACountryCode;
      RegionCode := ARegionCode;
    end;

  err := AddStation(stationInfo, AStationId);

  Result := err;
end;

function TStationRepository.AddStation(AStationInfo: TStationInfo; out AStationId: string): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  dateNow: integer;
  isExists: boolean;
begin
  err := ERR_OK;

  try
    err := IsStationExists(AStationInfo.Name, EMPTY_STR, isExists);

    if (err = ERR_OK) and isExists then
      err := ERR_DB_STATION_ALREADY_EXISTS;

    if err = ERR_OK then
    begin
      AStationId := TRepository.GetNewDbTableKeyAsGUID;
      dateNow := GetUnixTimestamp();

      query := TZQuery.Create(nil);
      try
        query.Connection := TRepository.GetDbConnection;

        query.SQL.Add(
          'INSERT INTO ' + DB_TABLE_STATIONS +
          ' (ID, Name, StreamUrl, Description, WebpageUrl, GenreCode, CountryCode, RegionCode, Created, Modified) ' +
          'VALUES(:ID,:Name,:StreamUrl,:Description,:WebpageUrl,:GenreCode,:CountryCode,:RegionCode,:Created,:Modified);'
        );

        query.Params.ParamByName('ID').AsString := AStationId;
        query.Params.ParamByName('Name').AsString := Trim(AStationInfo.Name);
        query.Params.ParamByName('StreamUrl').AsString := Trim(AStationInfo.StreamUrl);

        if (Trim(AStationInfo.Description) <> EMPTY_STR) then
          query.Params.ParamByName('Description').AsString := Trim(AStationInfo.Description);

        if (Trim(AStationInfo.WebpageUrl) <> EMPTY_STR) then
          query.Params.ParamByName('WebpageUrl').AsString := Trim(AStationInfo.WebpageUrl);

        if (Trim(AStationInfo.GenreCode) <> EMPTY_STR) then
          query.Params.ParamByName('GenreCode').AsString := Trim(AStationInfo.GenreCode);

        if (Trim(AStationInfo.CountryCode) <> EMPTY_STR) then
          query.Params.ParamByName('CountryCode').AsString := Trim(AStationInfo.CountryCode);
        if (Trim(AStationInfo.RegionCode) <> EMPTY_STR) then
          query.Params.ParamByName('RegionCode').AsString := Trim(AStationInfo.RegionCode);

        query.Params.ParamByName('Created').AsInteger := dateNow;
        query.Params.ParamByName('Modified').AsInteger := dateNow;

        query.ExecSQL;

      finally
        query.Free;
      end;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'AddStation', E);
        err := ERR_DB_ADD_STATION;
      end;
  end;

  Result := err;
end;

function TStationRepository.UpdateStation(StationInfo: TStationInfo): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  isExists: boolean;
begin
  err := ERR_OK;

  try

    err := IsStationExists(StationInfo.Name, StationInfo.Id, isExists);

    if (err = ERR_OK) and isExists then
      err := ERR_DB_STATION_ALREADY_EXISTS;

    if err = ERR_OK then
    begin
      query := TZQuery.Create(nil);
      try
        query.Connection := TRepository.GetDbConnection;

        query.SQL.Add(
          'UPDATE ' + DB_TABLE_STATIONS + ' SET ' +
          '  Name = :StationName, StreamUrl = :StreamUrl, Description = :Description, ' +
          '  WebpageUrl = :WebpageUrl, GenreCode = :GenreCode, CountryCode = :CountryCode, ' +
          '  Modified = :Modified ' +
          'WHERE ID = :StationId;');

        query.ParamByName('StationName').AsString := StationInfo.Name;
        query.ParamByName('StreamUrl').AsString := StationInfo.StreamUrl;
        query.ParamByName('Description').AsString := StationInfo.Description;
        query.ParamByName('WebpageUrl').AsString := StationInfo.WebpageUrl;
        query.ParamByName('GenreCode').AsString := StationInfo.GenreCode;
        query.ParamByName('CountryCode').AsString := StationInfo.CountryCode;
        query.ParamByName('Modified').AsInteger := GetUnixTimestamp();
        query.ParamByName('StationId').AsString := StationInfo.Id;

        query.ExecSQL;

      finally
        query.Free;
      end;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'UpdateStation', E);
        err := ERR_DB_UPDATE_STATION;
      end;
  end;

  Result := err;
end;

function TStationRepository.UpdateStationDictionaryCode(
  DictionaryType: TDictionaryType; OldCode: string; NewCode: string): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  columnName: string;
begin
  err := ERR_OK;

  try
    if (Trim(OldCode) = EMPTY_STR) or (Trim(NewCode) = EMPTY_STR) or (Trim(NewCode) = Trim(OldCode)) then
    begin
      Result := err;
      Exit;
    end;

    if not(DictionaryType in [dkCountry, dkGenre]) then
    begin
      Result := err;
      Exit;
    end;

    query := TZQuery.Create(nil);
    try

      if (DictionaryType = dkCountry) then
        columnName := 'CountryCode'
      else if (DictionaryType = dkGenre) then
        columnName := 'GenreCode'
      else begin
        Result := ERR_DB_UPDATE_STATION_DICTIONARY_CODE;
        Exit;
      end;

      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'UPDATE ' + DB_TABLE_STATIONS + ' SET ' + columnName + ' = :NewCode ' +
        'WHERE ' + columnName + ' = :OldCode;');

      query.ParamByName('NewCode').AsString := NewCode;
      query.ParamByName('OldCode').AsString := OldCode;

      query.ExecSQL;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'UpdateStationDictionaryCode', E);
        err := ERR_DB_UPDATE_STATION_DICTIONARY_CODE;
      end;
  end;

  Result := err;
end;

function TStationRepository.DeleteStation(StationId: string): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
begin
  err := ERR_OK;

  try
    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add('DELETE FROM ' + DB_TABLE_STATIONS + ' WHERE ID = :StationId;');

      query.ParamByName('StationId').AsString := StationId;

      query.ExecSQL;
    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'DeleteStation', E);
        err := ERR_DB_DELETE_STATION;
      end;
  end;

  Result := err;
end;

function TStationRepository.IsStationExists(StationName: string; ExcludeStationId: string;
  out IsExists: boolean): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
begin
  err := ERR_OK;

  try

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      if (ExcludeStationId <> EMPTY_STR) then
      begin
        query.SQL.Add('SELECT EXISTS(SELECT 1 FROM ' + DB_TABLE_STATIONS + ' WHERE ID <> :Id AND UPPER(Name) = UPPER(:Name)) AS IsExist;');
        query.ParamByName('Id').AsString := ExcludeStationId;
      end
      else
        query.SQL.Add('SELECT EXISTS(SELECT 1 FROM ' + DB_TABLE_STATIONS + ' WHERE UPPER(Name) = UPPER(:Name)) AS IsExist;');

      query.ParamByName('Name').AsString := StationName;

      query.Open;

      if query.RecordCount = 1 then
        IsExists := query.Fields[0].AsBoolean;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'IsStationExists', E);
        err := ERR_DB_IS_STATION_EXISTS;
      end;
  end;

  Result := err;
end;

function TStationRepository.DoesAnyStationUseTheGivenItemOfTheDictionary(
  DictionaryType: TDictionaryType; DictionaryRowCode: string; out
  ItemIsUsed: boolean): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  columnName: string;
begin
  err := ERR_OK;

  try

    if not(DictionaryType in [dkCountry, dkGenre]) then
    begin
      Result := err;
      ItemIsUsed := false;
      Exit;
    end;

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      if (DictionaryType = dkCountry) then
        columnName := 'CountryCode'
      else if (DictionaryType = dkGenre) then
        columnName := 'GenreCode'
      else begin
        Result := err;
        ItemIsUsed := false;
        Exit;
      end;

      query.SQL.Add('SELECT EXISTS(SELECT 1 FROM ' + DB_TABLE_STATIONS +
        ' WHERE ' + columnName + ' = UPPER(:Code)) AS IsExist;');

      query.ParamByName('Code').AsString := DictionaryRowCode;

      query.Open;

      if query.RecordCount = 1 then
        ItemIsUsed := query.Fields[0].AsBoolean
      else
        err := ERR_DB_DOES_ANY_STATION_USE_ITEM_OF_DICTIONARY;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'DoesAnyStationUseTheGivenItemOfTheDictionary', E);
        err := ERR_DB_DOES_ANY_STATION_USE_ITEM_OF_DICTIONARY;
      end;
  end;

  Result := err;
end;

function TStationRepository.LoadStation(var StationInfo: TStationInfo;
  const StationId: string): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
begin
  err := ERR_OK;

  try

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'SELECT ' +
        '  S.ID, S.Name, S.StreamUrl, S.Description, S.WebpageUrl, S.GenreCode, S.CountryCode ' +
        'FROM ' + DB_TABLE_STATIONS + ' S ' +
        'WHERE S.ID = :StationId;');

      query.ParamByName('StationId').AsString := StationId;

      query.Open;

      if (not query.EOF) and (query.RecordCount = 1) then
      begin
        with StationInfo do
        begin
          Id := query.FieldByName('ID').AsString;
          Name := query.FieldByName('Name').AsString;
          StreamUrl := query.FieldByName('StreamUrl').AsString;
          Description := query.FieldByName('Description').AsString;
          WebpageUrl := query.FieldByName('WebpageUrl').AsString;
          GenreCode := query.FieldByName('GenreCode').AsString;
          CountryCode := query.FieldByName('CountryCode').AsString;
        end;
      end;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'LoadStation', E);
        err := ERR_DB_LOAD_STATION;
      end;
  end;

  Result := err;
end;

function TStationRepository.LoadStations(var VstList: TVirtualStringTree;
  const Text: string): ErrorId;
var
  i: integer;
  query: TZQuery;
  err: ErrorId;

  sortColumns: Integer;
  orderBy: string;

  node: PVirtualNode;
  data: PStationNodeRec;

  textList: TStringList;

  selectedId: string;
begin
  err := ERR_OK;

  try

    node := VstList.GetFirstSelected();

    if (node <> nil) then
    begin
      data := VstList.GetNodeData(node);
      selectedId := data^.snd.ID;
    end else
      selectedId := EMPTY_STR;

    // Determine hot to sort
    if VstList.Header.SortColumn >= 0 then
      sortColumns := VstList.Header.SortColumn
    else
      sortColumns := 0;

    orderBy := ' ORDER BY ';
    case sortColumns of
      0: orderBy := orderBy + 'UPPER(S.Name)';
      1: orderBy := orderBy + 'UPPER(DRG.Text)';
      2: orderBy := orderBy + 'UPPER(DRC.Text)'
      else
        orderBy := 'UPPER(S.Name)';
    end;

    case VstList.Header.SortDirection of
      sdAscending: orderBy := orderBy + ' ASC;';
      sdDescending: orderBy := orderBy + ' DESC;';
    end;

    textList := TStringList.Create;
    textList.Sorted := true;
    textList.Duplicates := dupIgnore;
    Split(' ', Text, textList);

    query := TZQuery.Create(nil);
    try

      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'SELECT ' +
        '  S.ID, S.Name, S.GenreCode, DRG.Text AS GenreText, S.CountryCode, DRC.Text AS CountryText ' +
        'FROM ' + DB_TABLE_STATIONS + ' S ' +

        'LEFT JOIN ' + DB_TABLE_DICTIONARY + ' DG ON ' +
        '  DG.Code = ''' + DICTIONARY_GENRE_CODE + ''' ' +
        'LEFT JOIN ' + DB_TABLE_DICTIONARY_ROW + ' DRG ON ' +
        '  DRG.DictionaryID = DG.ID AND UPPER(DRG.Code) = UPPER(S.GenreCode) ' +

        'LEFT JOIN ' + DB_TABLE_DICTIONARY + ' DC ON ' +
        '  DC.Code = ''' + DICTIONARY_COUNTRY_CODE + ''' ' +
        'LEFT JOIN ' + DB_TABLE_DICTIONARY_ROW + ' DRC ON ' +
        '  DRC.DictionaryID = DC.ID AND DRC.Code = S.CountryCode ' +

        IIF(textList.Count > 0,
        'WHERE (' +
        '       (' + CreateMultipleStatements('UPPER(S.Name) LIKE :Text%d', 0, textList.Count, 'OR') + ') OR ' +
        '       (' + CreateMultipleStatements('UPPER(DRG.Text) LIKE :Text%d', 0, textList.Count, 'OR') + ') OR ' +
        '       (' + CreateMultipleStatements('UPPER(DRC.Text) LIKE :Text%d', 0, textList.Count, 'OR') + ')' +
        ')', '')
      );

      for i := 0 to textList.Count - 1 do
        query.ParamByName('Text' + IntToStr(i)).AsString := '%' + UpperCase(Trim(textList[i])) + '%';

      query.SQL.Add(orderBy);

      query.Open;

      VstList.Clear;
      VstList.RootNodeCount := query.RecordCount;
      VstList.ReinitNode(VstList.RootNode, True);

      VstList.BeginUpdate;

      node := nil;
      try
        while not query.EOF do
        begin
          if node = nil then
            node := VstList.GetFirst
          else
            node := VstList.GetNext(node);

          data := VstList.GetNodeData(node);

          data^.snd := TStationNodeData.Create(
            query.FieldByName('ID').AsString,
            query.FieldByName('Name').AsString,
            query.FieldByName('GenreText').AsString,
            query.FieldByName('CountryText').AsString
          );

          if (selectedId <> EMPTY_STR) and (query.FieldByName('ID').AsString = selectedId) then
            VstList.Selected[node] := true;

          query.Next;
        end;
      finally
        Finalize(node^);
      end;

      VstList.EndUpdate;

    finally
      query.Free;
      textList.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'LoadStations', E);
        err := ERR_DB_LOAD_STATIONS;
      end;
  end;

  Result := err;
end;

function TStationRepository.GetSelectedStationId(var VstList: TVirtualStringTree): string;
var
  node: PVirtualNode;
  data: PStationNodeRec;
begin
  Result := EMPTY_STR;

  node := VstList.GetFirstSelected;

  if Node <> nil then
    data := VstList.GetNodeData(node)
  else
    Exit;

  Result := data^.snd.ID;
end;

end.

