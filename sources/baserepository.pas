unit BaseRepository;
{===============================================================================
File:                BaseRepository.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Database management, the base repository class

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZConnection, RadioPlayerTypes;

type

  { TBaseRepository }

  TBaseRepository = class abstract(TObject)
  private
    FConnection: TZConnection;

    function Connect(const DBName: string): ErrorId;
    function Disconnect: ErrorId;
    procedure SetDBSettings;
  protected
    function GetDBPath: string; virtual; abstract;
    function CreateDB: ErrorId; virtual;

    function CreateDDL: ErrorId; virtual;
    function CreateDML: ErrorId; virtual; abstract;

    function GetUnixTimestamp: Integer;

    property Connection: TZConnection read FConnection;
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;

    function GetNewTableKey(const TableName: string): integer;
    function GetDBName: string; virtual; abstract;

    function ConnectDB(DBName: string = '';
      const ApplicationPath: string = ''): ErrorId; virtual;
    function DisconnectDB: ErrorId;
    function IsConnected : boolean;

    function AddStation(const StationName: string; const StreamUrl: string; out StationId: integer): ErrorId;
    function AddStation(const StationName: string; const StreamUrl: string;
      const Description: string; const WebpageUrl: string;
      const GenreCode: string; const CountryCode: string;
      out StationId: integer): ErrorId;

    function AddDictionary(const Name: string; const Code: string;
      const Description: string; out DictionaryId: integer): ErrorId;
    function AddDictionary(const Name: string; const Code: string; out DictionaryId: integer): ErrorId;

    function AddDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryId: integer; const ParentDictionaryId: integer;
      out DictionaryRowId: integer): ErrorId;
    function AddDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryId: integer;
      out DictionaryRowId: integer): ErrorId;
  end;

implementation

uses
  Forms, ZDataset, TRPErrors, Helpers, Consts, dateutils;

constructor TBaseRepository.Create;
begin
  inherited Create;
end;

destructor TBaseRepository.Destroy;
begin
  inherited Destroy;
end;

// Retrieves a new key for the table
function TBaseRepository.GetNewTableKey(const TableName: string): integer;
var
  query: TZQuery;
begin
  // Default is returned with a vicious key
  Result := INVALID_INT;

  try
    query := TZQuery.Create(nil);
    try
      query.Connection := FConnection;

      // sql query
      query.SQL.Add('SELECT coalesce(MAX(ID),0)+1 FROM ' + TableName + ';');

      query.Open;

      if query.RecordCount = 1 then
        Result := query.Fields[0].AsInteger;

    finally
      query.Free;
    end;
  except
    on E: Exception do
    begin
      LogException(
        GetLanguageItem('ErrorMessage.UnspecifiedError'), ClassName,
          'GetNewTableKey', E);

      Result := INVALID_INT;

      E.Message :=
        GetLanguageItem('ErrorMessage.General') + NEW_LINE + E.Message;

      Application.ShowException(E);
    end;
  end;
end;

// Connect to the database
function TBaseRepository.ConnectDB(DBName: string = '';
  const ApplicationPath: string = ''): ErrorId;
var
  fullPath: string;
  err: ErrorId;
begin
  err := ERR_OK;

  if DBName = EMPTY_STR then
    DBName := GetDBName;

  if err = ERR_OK then
  begin
    if FileExists(DBName) then
      fullPath := DBName
    else if ApplicationPath <> EmptyStr then
      fullPath := ConcatPaths([ApplicationPath, GetDBPath, DBName])
    else
      fullPath := ConcatPaths([GetApplicationPath, GetDBPath, DBName]);

    // Checking if the database exists if not, create it and open
    err := Connect(fullPath);
  end;

  Result := err;

end;

// Disconnect from the database
function TBaseRepository.DisconnectDB: ErrorId;
begin
  Result := Disconnect;
end;

function TBaseRepository.IsConnected: boolean;
begin
  if FConnection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

function TBaseRepository.Connect(const DBName: string): ErrorId;
var
  err: ErrorId;
  filePath: string;
begin
  err := ERR_OK;

  if DBName = EMPTY_STR then
    err := ERR_DB_NO_DATABASE_NAME;

  if err = ERR_OK then
  begin
    err := Disconnect;

    if err = ERR_OK then
    begin
      try
        FConnection := TZConnection.Create(nil);
        FConnection.Protocol := 'sqlite-3';
        FConnection.LibraryLocation :=
          ConcatPaths([GetApplicationPath, LIB_PATH,
          {$IFDEF MSWINDOWS}
          'sqlite3.dll'
          {$ENDIF}
          {$IFDEF LINUX}
          'libsqlite3.so'
          {$ENDIF}
          {$IFDEF MACOS}
          'sqlite3.dylib'
          {$ENDIF}
          ]);

        // Try connect to database
        if not FileExists(DBName) then
        begin
          // check if directory exists, if not create it
          filePath := ExtractFilePath(DBName);
          if not DirectoryExists(filePath) then
            if not CreateDir (filePath) then
            begin
              Result := ERR_DB_CREATE_DIR;
              Exit;
            end;

          FConnection.Database := DBName;
          FConnection.Connect;
          CreateDB;
        end else
        begin
          FConnection.Database := DBName;
          FConnection.Connect;
          SetDBSettings;
        end;

      except
        on E: Exception do
        begin
          LogException(EmptyStr, ClassName, 'Connect', E);
          //
          err := ERR_DB_CONNECT_ERROR;
        end;
      end;

    end;
  end;

  Result := err;
end;

function TBaseRepository.Disconnect: ErrorId;
var
  err: ErrorId;
begin
  err := ERR_OK;

  try
    if FConnection <> nil then
    begin
      if FConnection.Connected then
        FConnection.Disconnect;

      FConnection.Free;
    end;
  except
    on E: Exception do
    begin
      LogException(EmptyStr, ClassName, 'Disconnect', E);
      err := ERR_DB_DISCONNECT_ERROR;
    end;
  end;

  Result := err;

end;

procedure TBaseRepository.SetDBSettings;
begin
  // The Boolean synchronous value controls whether or not the
  // library will wait for disk writes to be fully written to disk
  // before continuing. In typical use the library may spend a lot of
  // time just waiting on the file system.
  // Setting "PRAGMA synchronous=OFF" can make a major speed difference.
  FConnection.ExecuteDirect('PRAGMA synchronous = NORMAL;');

  // The temp_store values specifies the type of database back-end to use
  // for temporary files.
  // The choices are DEFAULT (0), FILE (1), and MEMORY (2).
  // The use of a memory database for temporary tables can produce
  // signifigant savings. DEFAULT specifies the compiled-in default,
  // which is FILE unless the source has been modified.
  FConnection.ExecuteDirect('PRAGMA temp_store = MEMORY;');

  // The default behavior of the LIKE operator is to ignore case for
  // .SCII characters
  FConnection.ExecuteDirect('PRAGMA case_sensitive_like = OFF;');
end;

function TBaseRepository.CreateDB: ErrorId;
var
  query: TZQuery;
  err: ErrorId;
begin
  err := ERR_OK;

  try
    query := TZQuery.Create(nil);
    try
      // Database settings

      // The Boolean synchronous value controls whether or not the
      // library will wait for disk writes to be fully written to disk
      // before continuing. In typical use the library may spend a lot of
      // time just waiting on the file system.
      // Setting "PRAGMA synchronous=OFF" can make a major speed difference.
      FConnection.ExecuteDirect('PRAGMA synchronous = OFF;');

      // The temp_store values specifies the type of database back-end to use
      // for temporary files.
      // The choices are DEFAULT (0), FILE (1), and MEMORY (2).
      // The use of a memory database for temporary tables can produce
      // signifigant savings. DEFAULT specifies the compiled-in default,
      // which is FILE unless the source has been modified.
      FConnection.ExecuteDirect('PRAGMA temp_store = MEMORY;');

      // The default behavior of the LIKE operator is to ignore case for
      // .SCII characters
      FConnection.ExecuteDirect('PRAGMA case_sensitive_like = OFF;');

      // Unless already in a transaction, each SQL statement has a new
      // transaction started for it. This is very expensive, since it requires
      // reopening, writing to, and closing the journal file for each statement.
      // This can be avoided by wrapping sequences of SQL statements with
      // BEGIN TRANSACTION; and END TRANSACTION; statements.
      // This speedup is also obtained for statements which don't alter
      // the database.
      // The keyword COMMIT is a synonym for END TRANSACTION.
      FConnection.ExecuteDirect('BEGIN TRANSACTION;');

      err := CreateDDL;

      if err = ERR_OK then
        err := CreateDML;

      if (err = ERR_OK) and (not FConnection.ExecuteDirect('COMMIT;')) then
        err := ERR_DB_CREATE_ERROR;

      if err <> ERR_OK then
        FConnection.ExecuteDirect('ROLLBACK;');
    finally
      query.Free;
    end;
  except
    on E: Exception do
    begin
      LogException(EmptyStr, ClassName, 'CreateDB', E);
      FConnection.ExecuteDirect('ROLLBACK;');
      err := ERR_DB_CREATE_ERROR;
    end;
  end;

  Result := err;
end;

function TBaseRepository.CreateDDL: ErrorId;
var
  query: TZQuery;
  err: ErrorId;

  procedure ExecuteQuery(AQuery: string);
  begin
    query.SQL.Add(AQuery);
    query.ExecSQL;
    query.SQL.Clear;
  end;

begin
  err := ERR_OK;

  try
    query := TZQuery.Create(nil);
    try
      query.Connection := FConnection;

      // Stations
      ExecuteQuery(
        'CREATE TABLE ' + DB_TABLE_STATIONS + ' (' +
        'ID INTEGER PRIMARY KEY NOT NULL, ' +
        'Name VARCHAR NOT NULL, ' +
        'StreamUrl VARCHAR NOT NULL, ' +
        'Description TEXT NULL, ' +
        'WebpageUrl VARCHAR NULL, ' +
        'GenreCode VARCHAR NULL, ' +
        'CountryCode VARCHAR NULL, ' +
        'Created INTEGER NOT NULL, ' +
        'Modified INTEGER NULL);');

      ExecuteQuery(
        'CREATE INDEX ' + DB_TABLE_STATIONS +'_GenreCode_Index ' +
        'ON ' + DB_TABLE_STATIONS + '(GenreCode);' );
      ExecuteQuery(
        'CREATE INDEX ' + DB_TABLE_STATIONS + '_CountryCode_Index ' +
        'ON ' + DB_TABLE_STATIONS + '(CountryCode);' );

      // Dictionary
      ExecuteQuery(
        'CREATE TABLE ' + DB_TABLE_DICTIONARY + ' (' +
        'ID INTEGER PRIMARY KEY NOT NULL, ' +
        'Name VARCHAR NOT NULL, ' +
        'Code VARCHAR NOT NULL, ' +
        'Description VARCHAR NULL, ' +
        'Created INTEGER NOT NULL, ' +
        'Modified INTEGER NOT NULL);');

      ExecuteQuery(
        'CREATE INDEX ' + DB_TABLE_DICTIONARY + '_Code_Index ' +
        'ON ' + DB_TABLE_DICTIONARY + '(Code);' );

      // Dictionary Row
      ExecuteQuery(
        'CREATE TABLE ' + DB_TABLE_DICTIONARY_ROW + ' (' +
        'ID INTEGER PRIMARY KEY NOT NULL, ' +
        'DictionaryID INTEGER NOT NULL, ' +
        'Text VARCHAR NOT NULL, ' +
        'Code VARCHAR NOT NULL, ' +
        'Position INTEGER NULL, ' +
        'ParentDictionaryRowID INTEGER NULL);');

      ExecuteQuery(
        'CREATE INDEX ' + DB_TABLE_DICTIONARY_ROW + '_DictionaryID_Index ' +
        'ON ' + DB_TABLE_DICTIONARY_ROW + '(DictionaryID);' );
      ExecuteQuery(
        'CREATE INDEX ' + DB_TABLE_DICTIONARY_ROW + '_Code_Index ' +
        'ON ' + DB_TABLE_DICTIONARY_ROW + '(Code);' );

    finally
      query.Free;
    end;

  except
    on E: Exception do
    begin
      LogException(EmptyStr, ClassName, 'CreateDDL', E);
      err := ERR_DB_CREATE_DDL_ERROR;
    end;
  end;

  Result := err;
end;

function TBaseRepository.GetUnixTimestamp: Integer;
begin
  Result := DateTimeToUnix(LocalTimeToUniversal(Now));
end;

function TBaseRepository.AddStation(const StationName: string;
  const StreamUrl: string; out StationId: integer): ErrorId;
begin
  Result := AddStation(StationName, StreamUrl, EMPTY_STR, EMPTY_STR, EMPTY_STR, EMPTY_STR, StationId);
end;

function TBaseRepository.AddStation(const StationName: string;
  const StreamUrl: string; const Description: string; const WebpageUrl: string;
  const GenreCode: string; const CountryCode: string; out StationId: integer): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  dateNow: integer;
begin
  err := ERR_OK;

  try
    StationId := GetNewTableKey(DB_TABLE_STATIONS);
    dateNow := GetUnixTimestamp();

    query := TZQuery.Create(nil);
    try
      query.Connection := Connection;

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

function TBaseRepository.AddDictionary(const Name: string; const Code: string;
  const Description: string; out DictionaryId: integer): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  dateNow: integer;
begin
  err := ERR_OK;

  try
    DictionaryId := GetNewTableKey(DB_TABLE_DICTIONARY);
    dateNow := GetUnixTimestamp();

    query := TZQuery.Create(nil);
    try
      query.Connection := Connection;

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

function TBaseRepository.AddDictionary(const Name: string; const Code: string;
  out DictionaryId: integer): ErrorId;
begin
  Result := AddDictionary(Name, Code, EMPTY_STR, DictionaryId);
end;

function TBaseRepository.AddDictionaryRow(const Text: string;
  const Code: string; const Position: integer; const DictionaryId: integer;
  const ParentDictionaryId: integer; out DictionaryRowId: integer): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
begin
  err := ERR_OK;

  try
    DictionaryRowId := GetNewTableKey(DB_TABLE_DICTIONARY_ROW);

    query := TZQuery.Create(nil);
    try
      query.Connection := Connection;

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

function TBaseRepository.AddDictionaryRow(const Text: string;
  const Code: string; const Position: integer; const DictionaryId: integer; out
  DictionaryRowId: integer): ErrorId;
begin
  Result := AddDictionaryRow(Text, Code, Position, DictionaryId, EMPTY_INT, DictionaryRowId);
end;

end.


