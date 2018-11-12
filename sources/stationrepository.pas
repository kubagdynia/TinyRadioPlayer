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

    function AddStation(const StationName: string; const StreamUrl: string;
      out StationId: integer): ErrorId;
    function AddStation(const StationName: string; const StreamUrl: string;
      const Description: string; const WebpageUrl: string;
      const GenreCode: string; const CountryCode: string;
      out StationId: integer): ErrorId;

    function LoadStations(var VstList: TVirtualStringTree; const Text: string): ErrorId;
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

function TStationRepository.AddStation(const StationName: string;
  const StreamUrl: string; out StationId: integer): ErrorId;
begin
  Result := AddStation(StationName, StreamUrl, EMPTY_STR, EMPTY_STR, EMPTY_STR, EMPTY_STR, StationId);
end;

function TStationRepository.AddStation(const StationName: string;
  const StreamUrl: string; const Description: string; const WebpageUrl: string;
  const GenreCode: string; const CountryCode: string; out StationId: integer): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  dateNow: integer;
begin
  err := ERR_OK;

  try
    StationId := TRepository.GetNewDbTableKey(DB_TABLE_STATIONS);
    dateNow := GetUnixTimestamp();

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'INSERT INTO ' + DB_TABLE_STATIONS +
        ' (ID, Name, StreamUrl, Description, WebpageUrl, GenreCode, CountryCode, Created, Modified) ' +
        'VALUES(:ID,:Name,:StreamUrl,:Description,:WebpageUrl,:GenreCode,:CountryCode,:Created,:Modified);'
      );

      query.Params.ParamByName('ID').AsInteger := StationId;
      query.Params.ParamByName('Name').AsString := StationName;
      query.Params.ParamByName('StreamUrl').AsString := StreamUrl;

      if (Description <> EMPTY_STR) then
        query.Params.ParamByName('Description').AsString := Description;

      if (WebpageUrl <> EMPTY_STR) then
        query.Params.ParamByName('WebpageUrl').AsString := WebpageUrl;

      if (GenreCode <> EMPTY_STR) then
        query.Params.ParamByName('GenreCode').AsString := GenreCode;

      if (CountryCode <> EMPTY_STR) then
        query.Params.ParamByName('CountryCode').AsString := CountryCode;

      query.Params.ParamByName('Created').AsInteger := dateNow;
      query.Params.ParamByName('Modified').AsInteger := dateNow;

      query.ExecSQL;

    finally
      query.Free;
    end;
  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'AddDatabaseStation', E);
        err := ERR_DB_ADD_STATION;
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
begin
  err := ERR_OK;

  try

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
        '  DRG.DictionaryID = DG.ID AND DRG.Code = S.GenreCode ' +

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
            query.FieldByName('ID').AsInteger,
            query.FieldByName('Name').AsString,
            query.FieldByName('GenreText').AsString,
            query.FieldByName('CountryText').AsString
          );

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

end.

