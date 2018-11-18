unit DictionaryRepository;
{===============================================================================
File:                DictionaryRepository.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Dictionary management

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioPlayerTypes;

type

  { TDictionaryRepository }

  TDictionaryRepository = class (TObject)
  private

  protected

  public
    constructor Create; overload;
    destructor Destroy; override;

    // Add Dictionary
    function AddDictionary(const Name: string; const Code: string;
      const Description: string; out DictionaryId: integer): ErrorId;
    function AddDictionary(const Name: string; const Code: string;
      out DictionaryId: integer): ErrorId;

    // Add Dictionary Row
    function AddDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryId: integer; const ParentDictionaryId: integer;
      out DictionaryRowId: integer): ErrorId;
    function AddDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryId: integer;
      out DictionaryRowId: integer): ErrorId;
  end;

implementation

uses
  ZDataset, Consts, Helpers, Repository, TRPErrors;

{ TDictionaryRepository }

constructor TDictionaryRepository.Create;
begin
  inherited Create;

  // Code here
end;

destructor TDictionaryRepository.Destroy;
begin
  // Code here

  inherited Destroy;
end;

function TDictionaryRepository.AddDictionary(const Name: string;
  const Code: string; const Description: string; out DictionaryId: integer): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  dateNow: integer;
begin
  err := ERR_OK;

  try
    DictionaryId := TRepository.GetNewDbTableKey(DB_TABLE_DICTIONARY);
    dateNow := GetUnixTimestamp();

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'INSERT INTO ' + DB_TABLE_DICTIONARY +
        ' (ID, Name, Code, Description, Created, Modified) ' +
        'VALUES(:ID,:Name,:Code,:Description,:Created,:Modified);'
      );

      query.Params.ParamByName('ID').AsInteger := DictionaryId;
      query.Params.ParamByName('Name').AsString := Name;
      query.Params.ParamByName('Code').AsString := Code;

      if Description <> EMPTY_STR then
        query.Params.ParamByName('Description').AsString := Description;

      query.Params.ParamByName('Created').AsInteger := dateNow;
      query.Params.ParamByName('Modified').AsInteger := dateNow;

      query.ExecSQL;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'AddDatabaseDictionary', E);
        err := ERR_DB_ADD_DICTIONARY;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.AddDictionary(const Name: string;
  const Code: string; out DictionaryId: integer): ErrorId;
begin
  Result := AddDictionary(Name, Code, EMPTY_STR, DictionaryId);
end;

function TDictionaryRepository.AddDictionaryRow(const Text: string;
  const Code: string; const Position: integer; const DictionaryId: integer;
  const ParentDictionaryId: integer; out DictionaryRowId: integer): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
begin
  err := ERR_OK;

  try
    DictionaryRowId := TRepository.GetNewDbTableKey(DB_TABLE_DICTIONARY_ROW);

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'INSERT INTO ' + DB_TABLE_DICTIONARY_ROW +
        ' (ID, DictionaryID, Text, Code, Position, ParentDictionaryRowID) ' +
        'VALUES(:ID,:DictionaryID,:Text,:Code,:Position,:ParentDictionaryRowID);'
      );

      query.Params.ParamByName('ID').AsInteger := DictionaryRowId;
      query.Params.ParamByName('DictionaryID').AsInteger := DictionaryId;
      query.Params.ParamByName('Text').AsString := Text;
      query.Params.ParamByName('Code').AsString := Code;
      query.Params.ParamByName('Position').AsInteger := Position;

      if ParentDictionaryId <> EMPTY_INT then
        query.Params.ParamByName('ParentDictionaryRowID').AsInteger := ParentDictionaryId;

      query.ExecSQL;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'AddDatabaseDictionaryRow', E);
        err := ERR_DB_ADD_DICTIONARY_ROW;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.AddDictionaryRow(const Text: string;
  const Code: string; const Position: integer; const DictionaryId: integer; out
  DictionaryRowId: integer): ErrorId;
begin
  Result := AddDictionaryRow(Text, Code, Position, DictionaryId, EMPTY_INT, DictionaryRowId);
end;

end.

