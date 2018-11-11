unit StationRepository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZConnection, RadioPlayerTypes;

type

  { TStationRepository }

  TStationRepository = class (TObject)
  private

  protected

  public
    constructor Create; overload;
    destructor Destroy; override;

    procedure DoSomething(ShowThisText: string);

    function AddStation(const StationName: string; const StreamUrl: string;
      out StationId: integer): ErrorId;
    function AddStation(const StationName: string; const StreamUrl: string;
      const Description: string; const WebpageUrl: string;
      const GenreCode: string; const CountryCode: string;
      out StationId: integer): ErrorId;
  end;

implementation

uses Dialogs, ZDataset, Consts, Helpers, Repository, TRPErrors;

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

procedure TStationRepository.DoSomething(ShowThisText: string);
var
  query: TZQuery;
  nextId: Integer;
begin
  query := TZQuery.Create(nil);
  try
    query.Connection := TRepository.GetDbConnection;

    query.SQL.Add('SELECT coalesce(MAX(ID),0)+1 FROM ' + DB_TABLE_STATIONS + ';');

    query.Open;

    if query.RecordCount = 1 then
      nextId := query.Fields[0].AsInteger;

    ShowMessage(IntToStr(nextId));
  finally
    query.Free;
  end;

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

end.

