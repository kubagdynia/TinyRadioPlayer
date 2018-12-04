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
  Classes, SysUtils, StdCtrls, VirtualTrees, RadioPlayerTypes, Consts;

type

  { TDictionaryRepository }

  TDictionaryRepository = class (TObject)
  private
    FDictionary: array [Low(TDictionaryKind)..High(TDictionaryKind)] of TList;

    // Determines whether the first position should be empty
    FFirstBlank: boolean;

    function ClearDictionary(FreeAndNilDictionary: boolean = false): ErrorId;
  protected
    function GetDictionaryName(DictionaryKind: TDictionaryKind): string;

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

    function LoadDictionary(DictionaryKind: TDictionaryKind;
      SkipIfLoaded: boolean = true; SortDirection: TSortDirection = sdAscending): ErrorId;
    function ClearDictionary: ErrorId;

    function AddDictionaryItemsToComboBox(var ComboBox: TComboBox;
      DictionaryKind: TDictionaryKind; FirstBlank: boolean): ErrorId;
  end;

implementation

uses
  ZDataset, Helpers, Repository, TRPErrors;

{ TDictionaryRepository }

function TDictionaryRepository.GetDictionaryName(DictionaryKind: TDictionaryKind): string;
begin
  Result := DICTIONARY_NAMES[DictionaryKind];
end;

constructor TDictionaryRepository.Create;
var
  dictionaryKind: TDictionaryKind;
begin
  inherited Create;

  for dictionaryKind := Low(FDictionary) to High(FDictionary) do
    FDictionary[dictionaryKind] := TList.Create;
end;

destructor TDictionaryRepository.Destroy;
begin
  ClearDictionary(true);

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
        LogException(EMPTY_STR, ClassName, 'AddDictionary', E);
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
        LogException(EMPTY_STR, ClassName, 'AddDictionaryRow', E);
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

function TDictionaryRepository.LoadDictionary(DictionaryKind: TDictionaryKind;
  SkipIfLoaded: boolean = true; SortDirection: TSortDirection = sdAscending): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  sortDir: string;
  dictionaryTable: PDictionaryTable;
begin
  err := ERR_OK;

  try

    if (SkipIfLoaded) and (FDictionary[DictionaryKind] <> nil) and (FDictionary[DictionaryKind].Count > 0) then
    begin
      Result := err;
      Exit;
    end;

    case SortDirection of
      sdAscending: sortDir := 'ASC';
      sdDescending: sortDir := 'DESC';
    end;

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'SELECT dr.ID, dr.Text, dr.Code FROM ' + DB_TABLE_DICTIONARY + ' d ' +
        'INNER JOIN ' + DB_TABLE_DICTIONARY_ROW + ' dr ON dr.DictionaryID = d.ID ' +
        'WHERE d.Code = :DictionaryCode ' +
        'ORDER BY dr.Position, UPPER(dr.Text) ' + sortDir
      );

      query.Params.ParamByName('DictionaryCode').AsString := GetDictionaryName(DictionaryKind);

      query.Open;

      while not query.EOF do
      begin
        // create a new record
        New(dictionaryTable);

        dictionaryTable^.Id:= query.FieldByName('ID').AsInteger;
        dictionaryTable^.Text := query.FieldByName('Text').AsString;
        dictionaryTable^.Code := query.FieldByName('Code').AsString;

        // adds record to the list
        FDictionary[DictionaryKind].Add(dictionaryTable);

        query.Next;
      end;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'LoadDictionary', E);
        err := ERR_DB_LOAD_DICTIONARY;
      end;
  end;

  Result := err;

end;


function TDictionaryRepository.AddDictionaryItemsToComboBox(
  var ComboBox: TComboBox; DictionaryKind: TDictionaryKind; FirstBlank: boolean): ErrorId;
var
  err: ErrorId;
  i: integer;
  selectedItem: integer;
  dictionaryTable: PDictionaryTable;
begin
   err := ERR_OK;

   try

     if FDictionary[DictionaryKind] <> nil then
     begin

       // Get index of selected item in a ComboBox to mark the same position
       // after loading
       selectedItem := ComboBox.ItemIndex;

       ComboBox.Items.BeginUpdate;

       ComboBox.Clear;

       FFirstBlank := FirstBlank;

       if FirstBlank then
         ComboBox.Items.Add(EMPTY_STR);

       for i := 0 to FDictionary[dictionaryKind].Count - 1 do
       begin
         dictionaryTable := FDictionary[dictionaryKind].Items[i];
         ComboBox.Items.Add(dictionaryTable^.Text);
       end;

       ComboBox.Items.EndUpdate;

       if selectedItem < 0 then
         ComboBox.ItemIndex := 0
       else
         ComboBox.ItemIndex := selectedItem;

       if ComboBox.ItemIndex = EMPTY_INT then
         ComboBox.ItemIndex := ComboBox.Items.Count - 1;

     end;

   except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'AddDictionaryItemsToComboBox', E);
        err := ERR_ADD_DICTIONARY_ITEMS_TO_COMBOBOX;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.ClearDictionary: ErrorId;
begin
  Result := ClearDictionary(false);
end;

// Free the dictionary and its all items
function TDictionaryRepository.ClearDictionary(FreeAndNilDictionary: boolean = false): ErrorId;
var
  err: ErrorId;
  i: integer;
  dictionaryKind: TDictionaryKind;
begin
  err := ERR_OK;

  try

    for dictionaryKind := Low(FDictionary) to High(FDictionary) do
    begin
      if FDictionary[dictionaryKind] <> nil then
      begin
        for i := FDictionary[dictionaryKind].Count - 1 downto 0 do
          Dispose(PDictionaryTable(FDictionary[dictionaryKind].Items[i]));

        FDictionary[dictionaryKind].Clear;

        if FreeAndNilDictionary then
          FreeAndNil(FDictionary[dictionaryKind]);
      end;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'ClearDictionary', E);
        err := ERR_DB_CLEAR_DICTIONARY;
      end;
  end;

  Result := err;
end;

end.

