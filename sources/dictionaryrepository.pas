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
  Classes, SysUtils, StdCtrls, VirtualTrees, RadioPlayerTypes, Consts, contnrs;

type

  { TDictionaryRepository }

  TDictionaryRepository = class (TObject)
  private
    FDictionary: array [Low(TDictionaryType)..High(TDictionaryType)] of TList;

    // Determines whether the first position should be empty
    FFirstBlank: boolean;

    function ClearDictionary(AFreeAndNilDictionary: boolean = false;
      ADictionaryType: TDictionaryType = TDictionaryType.dkNone): ErrorId;

    // Exists
    function DictionaryRowExists(DictionaryId: integer; Code: string; Text: string;
      ParentDictionaryRowId: integer = EMPTY_INT;
      ExcludeDictionaryRowId: integer = EMPTY_INT): boolean;

    function IsDictionaryRowUsedAsAParent(DictionaryRowId: integer): boolean;

    function RefreshDictionary(ADictionaryType: TDictionaryType): ErrorId;
  protected

  public
    constructor Create; overload;
    destructor Destroy; override;

    // Add Dictionary
    function AddDictionary(const Name: string; const Code: string;
      const Description: string; out DictionaryId: integer;
      const ParentCode: string = EMPTY_STR): ErrorId;
    function AddDictionary(const Name: string; const Code: string;
      out DictionaryId: integer; const ParentCode: string = EMPTY_STR): ErrorId;

    // Get Dictionary
    function GetDictionaryName(DictionaryType: TDictionaryType): string;
    function GetDictionaryType(DictionaryCode: string): TDictionaryType;
    function GetLocalizedDictionaryName(DictionaryType: TDictionaryType): string;
    function GetDictionaryId(DictionaryType: TDictionaryType;
      out DictionaryId: integer; out DictionaryParentId: integer): ErrorId;
    function GetDictionaryTypeByDictionaryRowId(DictionaryRowId: integer;
      out DictionaryType: TDictionaryType): ErrorId;
    function GetParentDictionaryType(DictionaryType: TDictionaryType;
      out ParentDictionaryType: TDictionaryType): ErrorId;

    function IsDictionaryItemUsed(DictionaryRowId: integer;
      DictionaryType: TDictionaryType; out IsUsed: boolean): ErrorId;

    // Get Dictionary Row
    function GetDictionaryRowId(DictionaryId: integer; Code: string;
      out DictionaryRowId: integer): ErrorId;
    function GetDictionaryRowCode(DictionaryRowId: integer;
      out Code: string): ErrorId;

    // Add Dictionary Row
    function AddDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryId: integer; const ParentDictionaryId: integer;
      out DictionaryRowId: integer): ErrorId;
    function AddDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryId: integer;
      out DictionaryRowId: integer): ErrorId;
    function AddDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryCode: string; const ParentDictionaryCode: string;
      out DictionaryRowId: integer): ErrorId;

    // Update Dictionary Row
    function UpdateDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryCode: string; const ParentDictionaryCode: string;
      DictionaryRowId: integer): ErrorId;

    // Delete Dictionary Row
    function DeleteDictionaryRow(DictionaryRowId: integer): ErrorId;

    // Load Dictionary
    function LoadDictionary(DictionaryType: TDictionaryType;
      SkipIfLoaded: boolean = true; SortDirection: TSortDirection = sdAscending): ErrorId;
    function ClearDictionary: ErrorId;

    function LoadDictionaryNames(var VstList: TVirtualStringTree; SelectFirst: boolean = false): ErrorId;
    function LoadDictionaryDetails(var VstList: TVirtualStringTree;
      DictionaryType: TDictionaryType;
      ParentDictionaryType: TDictionaryType = TDictionaryType.dkNone;
      ParentDictionaryRowCode: string = EMPTY_STR;
      LastUsedDictionaryRowId: integer = EMPTY_INT): ErrorId;

    // ComboBox
    function AddDictionaryItemsToComboBox(var ComboBox: TComboBox;
      DictionaryType: TDictionaryType; FirstBlank: boolean): ErrorId;
    function FindAnItemInTheComboBox(var ComboBox: TComboBox; Code: string): ErrorId;
    function GetDictionaryCodeFromSelectedItem(var ComboBox: TComboBox;
      out DictionaryCode: string): ErrorId;
    function GetDictionaryCodeFromSelectedItem(var ComboBox: TComboBox;
      out DictionaryCode: string; out ParentDictionaryCode: string): ErrorId;

    // Get All
    function GetAllDictionaries(out ADictionaryList : TObjectList): ErrorId;

    function ImportDictionaries(var dto: TExportImportDto): ErrorId;

    // Exists
    function DictionaryRowExists(DictionaryType: TDictionaryType; Code: string): boolean;
    function DictionaryRowExists(DictionaryType: TDictionaryType; Code: string; ParentCode: string): boolean;
  end;

implementation

uses
  ZDataset, Helpers, Repository, TRPErrors;

{ TDictionaryRepository }

constructor TDictionaryRepository.Create;
var
  dictionaryType: TDictionaryType;
begin
  inherited Create;

  for dictionaryType := Low(FDictionary) to High(FDictionary) do
  begin
    if dictionaryType = TDictionaryType.dkNone then
      continue;

    FDictionary[dictionaryType] := TList.Create;
  end;
end;

destructor TDictionaryRepository.Destroy;
begin
  ClearDictionary(true);

  inherited Destroy;
end;

function TDictionaryRepository.AddDictionary(const Name: string;
  const Code: string; const Description: string; out DictionaryId: integer;
  const ParentCode: string = EMPTY_STR): ErrorId;
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
        ' (ID, Name, Code, ParentCode, Description, Created, Modified) ' +
        'VALUES(:ID,:Name,:Code,:ParentCode,:Description,:Created,:Modified);'
      );

      query.Params.ParamByName('ID').AsInteger := DictionaryId;
      query.Params.ParamByName('Name').AsString := Name;
      query.Params.ParamByName('Code').AsString := Code;

      if ParentCode <> EMPTY_STR then
        query.Params.ParamByName('ParentCode').AsString := ParentCode
      else
        query.Params.ParamByName('ParentCode').Clear;

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
  const Code: string; out DictionaryId: integer;
  const ParentCode: string = EMPTY_STR): ErrorId;
begin
  Result := AddDictionary(Name, Code, EMPTY_STR, DictionaryId, ParentCode);
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

function TDictionaryRepository.AddDictionaryRow(const Text: string;
  const Code: string; const Position: integer; const DictionaryCode: string;
  const ParentDictionaryCode: string; out DictionaryRowId: integer): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  dictionaryType: TDictionaryType;
  dictionaryId: integer;
  dictionaryParentId: integer;
  dictionaryParentRowId: integer;
  rowExists: boolean;
begin
  err := ERR_OK;

  try

    dictionaryType := GetDictionaryType(DictionaryCode);

    err := TRepository.GetDictionaryId(dictionaryType,
      dictionaryId, dictionaryParentId);

    if err <> ERR_OK then
    begin
      Result := err;
      Exit;
    end;

    dictionaryParentRowId := EMPTY_INT;

    if (ParentDictionaryCode <> EMPTY_STR) and (dictionaryParentId <> EMPTY_INT) then
      err := TRepository.GetDictionaryRowId(dictionaryParentId,
        ParentDictionaryCode, dictionaryParentRowId);

    if err <> ERR_OK then
    begin
      Result := err;
      Exit;
    end;

    rowExists := DictionaryRowExists(dictionaryId, Code, Text, dictionaryParentRowId);

    if rowExists then
      err := ERR_DICTIONARY_ROW_EXISTS;

    if err <> ERR_OK then
    begin
      Result := err;
      Exit;
    end;

    DictionaryRowId := TRepository.GetNewDbTableKey(DB_TABLE_DICTIONARY_ROW);

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'INSERT INTO ' + DB_TABLE_DICTIONARY_ROW +
        ' (ID, DictionaryID, Text, Code, Position, ParentDictionaryRowID) ' +
        'VALUES(:ID,:DictionaryID,:Text,UPPER(:Code),:Position,:ParentDictionaryRowID);'
      );

      query.Params.ParamByName('ID').AsInteger := DictionaryRowId;
      query.Params.ParamByName('DictionaryID').AsInteger := dictionaryId;
      query.Params.ParamByName('Text').AsString := Text;
      query.Params.ParamByName('Code').AsString := Code;
      query.Params.ParamByName('Position').AsInteger := Position;

      if dictionaryParentRowId = EMPTY_INT then
        query.Params.ParamByName('ParentDictionaryRowID').Clear
      else
        query.Params.ParamByName('ParentDictionaryRowID').AsInteger := dictionaryParentRowId;

      query.ExecSQL;

      err := RefreshDictionary(dictionaryType);

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

function TDictionaryRepository.UpdateDictionaryRow(const Text: string;
  const Code: string; const Position: integer; const DictionaryCode: string;
  const ParentDictionaryCode: string; DictionaryRowId: integer): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  dictionaryType: TDictionaryType;
  dictionaryId: integer;
  dictionaryParentId: integer;
  dictionaryParentRowId: integer;
  rowExists: boolean;
  oldCode: string;
begin
  err := ERR_OK;

  try

    dictionaryType := GetDictionaryType(DictionaryCode);

    err := TRepository.GetDictionaryId(dictionaryType,
      dictionaryId, dictionaryParentId);

    if err <> ERR_OK then
    begin
      Result := err;
      Exit;
    end;

    dictionaryParentRowId := EMPTY_INT;

    if (ParentDictionaryCode <> EMPTY_STR) and (dictionaryParentId <> EMPTY_INT) then
      err := TRepository.GetDictionaryRowId(dictionaryParentId,
        ParentDictionaryCode, dictionaryParentRowId);

    if err <> ERR_OK then
    begin
      Result := err;
      Exit;
    end;

    rowExists := DictionaryRowExists(dictionaryId, Code, Text, dictionaryParentRowId, DictionaryRowId);

    if rowExists then
      err := ERR_DICTIONARY_ROW_EXISTS;

    if err <> ERR_OK then
    begin
      Result := err;
      Exit;
    end;

    err := GetDictionaryRowCode(DictionaryRowId, oldCode);

    if err <> ERR_OK then
    begin
      Result := err;
      Exit;
    end;

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'UPDATE ' + DB_TABLE_DICTIONARY_ROW +
        ' SET DictionaryID = :DictionaryID, Text = :Text, Code = UPPER(:Code), ' +
        ' Position = :Position, ParentDictionaryRowID = :ParentDictionaryRowID ' +
        'WHERE ID = :ID;'
      );

      query.Params.ParamByName('ID').AsInteger := DictionaryRowId;
      query.Params.ParamByName('DictionaryID').AsInteger := dictionaryId;
      query.Params.ParamByName('Text').AsString := Text;
      query.Params.ParamByName('Code').AsString := Code;
      query.Params.ParamByName('Position').AsInteger := Position;

      if dictionaryParentRowId = EMPTY_INT then
        query.Params.ParamByName('ParentDictionaryRowID').Clear
      else
        query.Params.ParamByName('ParentDictionaryRowID').AsInteger := dictionaryParentRowId;

      query.ExecSQL;

      TRepository.UpdateStationDictionaryCode(dictionaryType, oldCode, Code);

      err := RefreshDictionary(dictionaryType);

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'UpdateDictionaryRow', E);
        err := ERR_DB_UPDATE_DICTIONARY_ROW;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.DeleteDictionaryRow(DictionaryRowId: integer): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  dictionaryType: TDictionaryType;
  isUsed: boolean;
begin
  err := ERR_OK;

  try

    err := GetDictionaryTypeByDictionaryRowId(DictionaryRowId, dictionaryType);

    if err <> ERR_OK then
    begin
      Result := err;
      Exit;
    end;

    err := IsDictionaryItemUsed(DictionaryRowId, dictionaryType, isUsed);

    if err <> ERR_OK then
    begin
      Result := err;
      Exit;
    end;

    if isUsed then
    begin
      Result := ERR_DICTIONARY_ROW_IS_USED_BY_OTHER_DATA;
      Exit;
    end;

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'DELETE FROM ' + DB_TABLE_DICTIONARY_ROW + ' WHERE ID = :ID;'
      );

      query.Params.ParamByName('ID').AsInteger := DictionaryRowId;

      query.ExecSQL;

      err := RefreshDictionary(dictionaryType);

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'DeleteDictionaryRow', E);
        err := ERR_DB_DELETE_DICTIONARY_ROW;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.LoadDictionary(DictionaryType: TDictionaryType;
  SkipIfLoaded: boolean = true; SortDirection: TSortDirection = sdAscending): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  sortDir: string;
  dictionaryTable: PDictionaryTable;
begin
  err := ERR_OK;

  try

    if (SkipIfLoaded) and (FDictionary[DictionaryType] <> nil) and (FDictionary[DictionaryType].Count > 0) then
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
        'SELECT dr.ID, dr.Text, dr.Code, d.ParentCode AS ParentDictionaryCode, ' +
        '  drParent.Code AS ParentDictionaryRowCode, dr.Position FROM ' + DB_TABLE_DICTIONARY + ' d ' +
        'INNER JOIN ' + DB_TABLE_DICTIONARY_ROW + ' dr ON dr.DictionaryID = d.ID ' +
        'LEFT JOIN ' + DB_TABLE_DICTIONARY_ROW + ' drParent ON drParent.ID = dr.ParentDictionaryRowId ' +
        'WHERE d.Code = :DictionaryCode ' +
        'ORDER BY dr.Position, UPPER(dr.Text) ' + sortDir
      );

      query.Params.ParamByName('DictionaryCode').AsString := GetDictionaryName(DictionaryType);

      query.Open;

      while not query.EOF do
      begin
        // create a new record
        New(dictionaryTable);

        dictionaryTable^.Id:= query.FieldByName('ID').AsInteger;
        dictionaryTable^.Text := query.FieldByName('Text').AsString;
        dictionaryTable^.Code := query.FieldByName('Code').AsString;
        dictionaryTable^.ParentDictionaryCode := query.FieldByName('ParentDictionaryCode').AsString;
        dictionaryTable^.ParentDictionaryRowCode := query.FieldByName('ParentDictionaryRowCode').AsString;
        dictionaryTable^.Position := query.FieldByName('Position').AsInteger;

        // adds record to the list
        FDictionary[DictionaryType].Add(dictionaryTable);

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
  var ComboBox: TComboBox; DictionaryType: TDictionaryType; FirstBlank: boolean): ErrorId;
var
  err: ErrorId;
  i: integer;
  selectedItem: integer;
  dictionaryTable: PDictionaryTable;
begin
   err := ERR_OK;

   try

     if FDictionary[DictionaryType] <> nil then
     begin

       // Load dictionary details if it needed
       LoadDictionary(DictionaryType);

       // Get index of selected item in a ComboBox to mark the same position
       // after loading
       selectedItem := ComboBox.ItemIndex;

       ComboBox.Items.BeginUpdate;

       ComboBox.Clear;

       FFirstBlank := FirstBlank;

       if FirstBlank then
         ComboBox.Items.Add(EMPTY_STR);

       for i := 0 to FDictionary[DictionaryType].Count - 1 do
       begin
         dictionaryTable := FDictionary[DictionaryType].Items[i];
         ComboBox.Items.AddObject(dictionaryTable^.Text, TObject(dictionaryTable));
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

function TDictionaryRepository.FindAnItemInTheComboBox(var ComboBox: TComboBox;
  Code: string): ErrorId;
var
  err: ErrorId;
  i: integer;
begin
  err := ERR_OK;

  try
    for i := 0 to ComboBox.Items.Count - 1 do
    begin
      if (ComboBox.Items.Objects[i] <> nil) and (PDictionaryTable(ComboBox.Items.Objects[i])^.Code = Code) then
      begin
        ComboBox.ItemIndex := i;
        Break;
      end;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'FindAnItemInTheComboBox', E);
        err := ERR_FIND_ITEM_IN_COMBOBOX;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.GetDictionaryCodeFromSelectedItem(
  var ComboBox: TComboBox; out DictionaryCode: string): ErrorId;
var
  parentDictionaryCode: string;
begin
  Result := GetDictionaryCodeFromSelectedItem(ComboBox, DictionaryCode, parentDictionaryCode);
end;

function TDictionaryRepository.GetDictionaryCodeFromSelectedItem(
  var ComboBox: TComboBox; out DictionaryCode: string;
  out ParentDictionaryCode: string): ErrorId;
var
  err: ErrorId;
  selectedItem: integer;
begin
  err := ERR_OK;

  try
    if (ComboBox = nil) or (ComboBox.Items = nil) or (ComboBox.Items.Count = 0) then
      err := ERR_GET_CODE_FROM_SELECTED_ITEM;

    if err = ERR_OK then
    begin
      selectedItem := ComboBox.ItemIndex;

      if selectedItem < 0 then
        err := ERR_GET_CODE_FROM_SELECTED_ITEM;
    end;

    if err = ERR_OK then
    begin
      if ComboBox.Items.Objects[selectedItem] = nil then
      begin
        DictionaryCode := EMPTY_STR;
        ParentDictionaryCode := EMPTY_STR;
      end
      else
      begin
        DictionaryCode := PDictionaryTable(ComboBox.Items.Objects[selectedItem])^.Code;
        ParentDictionaryCode := PDictionaryTable(ComboBox.Items.Objects[selectedItem])^.ParentDictionaryRowCode;
      end;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'GetDictionaryCodeFromSelectedItem', E);
        err := ERR_GET_CODE_FROM_SELECTED_ITEM;
      end;
  end;

  Result := err;

end;

function TDictionaryRepository.GetAllDictionaries(out
  ADictionaryList: TObjectList): ErrorId;
var
  query: TZQuery;
  err: ErrorId;
  dictionaryList: TObjectList;
  dictionary, childDictionary: TDictionary;
  dictionaryDetail, childDictionaryDetail: TDictionaryDetail;
  prevPropCode, prevPropDetailCode: string;
  propName, propCode, propDescription: string;
  propDetailText, propDetailCode: string;
  propDetailPosition: integer;
begin
  err := ERR_OK;

  try
    dictionaryList := TObjectList.Create(true); // true means that objects will be released when the list is destroyed

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'SELECT ' +
        // dictionary
        '  d.Name, d.Code, d.Description, ' +

        // dictionary details
        '  dd.Text DetailText, dd.Code DetailCode, dd.Position DetailPosition, ' +

        // child dictionary
        '  cd.Name ChildName, cd.Code ChildCode, cd.Description ChildDescription, ' +

        // child dictionary details
        '  cdd.Text ChildDetailText, cdd.Code ChildDetailCode, cdd.Position ChildDetailPosition ' +

        'FROM ' + DB_TABLE_DICTIONARY + ' d ' +
        'LEFT JOIN ' + DB_TABLE_DICTIONARY + ' cd ON cd.ParentCode = d.Code ' +
        'LEFT JOIN ' + DB_TABLE_DICTIONARY_ROW + ' dd ON dd.DictionaryID = d.ID ' +
        'LEFT JOIN ' + DB_TABLE_DICTIONARY_ROW + ' cdd ON cdd.ParentDictionaryRowID = dd.ID ' +
        'WHERE d.ParentCode IS NULL ' +
        'ORDER BY UPPER(d.Name), dd.Position, UPPER(dd.Text), cdd.Position, UPPER(cdd.Text)');

      query.Open;

      while not query.EOF do  // iterate over dictionary
      begin

        propName := query.FieldByName('Name').AsString;
        propCode := query.FieldByName('Code').AsString;
        propDescription := query.FieldByName('Description').AsString;

        if (prevPropCode <> propCode) then
        begin
          prevPropCode := propCode;

          dictionary := TDictionary.Create(
            propName, propCode,propDescription);

          if (query.FieldByName('DetailCode').AsString <> EMPTY_STR) then
            dictionary.D_Details := TObjectList.Create(true);

          while ((not query.EOF) and (prevPropCode = query.FieldByName('Code').AsString)) do // iterate over dictionary details
          begin
            propDetailText := query.FieldByName('DetailText').AsString;
            propDetailCode := query.FieldByName('DetailCode').AsString;
            propDetailPosition := query.FieldByName('DetailPosition').AsInteger;

            prevPropDetailCode := propDetailCode;

            if (propDetailCode <> EMPTY_STR) then
            begin
              dictionaryDetail := TDictionaryDetail.Create(
                propDetailText, propDetailCode, propDetailPosition);

              if (query.FieldByName('ChildCode').AsString <> EMPTY_STR) then
              begin
                childDictionary := TDictionary.Create(
                  query.FieldByName('ChildName').AsString,
                  query.FieldByName('ChildCode').AsString,
                  query.FieldByName('ChildDescription').AsString);

                if (query.FieldByName('ChildDetailCode').AsString <> EMPTY_STR) then
                begin
                  childDictionary.D_Details := TObjectList.Create(true);

                  while ((not query.EOF) and (prevPropDetailCode = query.FieldByName('DetailCode').AsString)) do
                  begin
                    childDictionaryDetail := TDictionaryDetail.Create(
                      query.FieldByName('ChildDetailText').AsString,
                      query.FieldByName('ChildDetailCode').AsString,
                      query.FieldByName('ChildDetailPosition').AsInteger);

                    childDictionary.D_Details.Add(childDictionaryDetail);

                    query.Next;
                  end;

                  if not query.EOF then
                    query.Prior;

                end;

                dictionaryDetail.D_Child := childDictionary;
              end;

              dictionary.D_Details.Add(dictionaryDetail);
            end;

            query.Next;
          end;

          if not query.EOF then
            query.Prior;

          dictionaryList.Add(dictionary);
        end;

        prevPropCode := propCode;

        query.Next;
      end;

      ADictionaryList := dictionaryList;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'GetAllDictionaries', E);
        err := ERR_GET_ALL_DICTIONARIES;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.ImportDictionaries(var dto: TExportImportDto): ErrorId;
var
  err: ErrorId;
  i: integer;
  dictionary: TDictionary;
  str: string;
begin
  err := ERR_OK;

  if (dto = nil) or (dto.C_Dictionaries = nil) then
    exit;

  try

    for i := 0 to dto.C_Dictionaries.Count - 1 do
    begin
      dictionary := dto.C_Dictionaries[i] as TDictionary;
      str := dictionary.A_Name;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'ImportDictionaries', E);
        err := ERR_GET_ALL_DICTIONARIES;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.DictionaryRowExists(DictionaryId: integer;
  Code: string; Text: string; ParentDictionaryRowId: integer = EMPTY_INT;
  ExcludeDictionaryRowId: integer = EMPTY_INT): boolean;
var
  query: TZQuery;
begin
  try

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      if ParentDictionaryRowId = EMPTY_INT then
        query.SQL.Add(
          'SELECT COUNT(1) AS Count FROM ' + DB_TABLE_DICTIONARY_ROW +
          ' WHERE (-1 = :ID OR ID <> :ID) AND DictionaryID = :DictionaryID AND (UPPER(Code) = UPPER(:Code)' +
          ' OR UPPER(TEXT) = UPPER(:Text));'
        )
      else
      begin
        query.SQL.Add(
          'SELECT COUNT(1) AS Count FROM ' + DB_TABLE_DICTIONARY_ROW +
          ' WHERE (-1 = :ID OR ID <> :ID) AND DictionaryID = :DictionaryID AND (UPPER(Code) = UPPER(:Code)' +
          ' OR UPPER(TEXT) = UPPER(:Text)) AND ParentDictionaryRowID = :ParentDictionaryRowId;'
        );
        query.Params.ParamByName('ParentDictionaryRowId').AsInteger := ParentDictionaryRowId;
      end;

      query.Params.ParamByName('ID').AsInteger := ExcludeDictionaryRowId;
      query.Params.ParamByName('DictionaryID').AsInteger := DictionaryId;
      query.Params.ParamByName('Code').AsString := Code;
      query.Params.ParamByName('Text').AsString := Text;

      query.Open;

      Result := query.FieldByName('Count').AsInteger > 0;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      RaiseErrorMessage(ERR_CHECKING_IF_DICTIONARY_ROW_EXISTS, ClassName, 'DictionaryRowExists');
  end;
end;

function TDictionaryRepository.DictionaryRowExists(
  DictionaryType: TDictionaryType; Code: string): boolean;
var
  dictionaryId: integer;
  dictionaryParentId: integer;
begin
  try

    GetDictionaryId(DictionaryType, dictionaryId, dictionaryParentId);
    Result := DictionaryRowExists(dictionaryId, Code, EMPTY_STR);

  except
    on E: Exception do
      RaiseErrorMessage(ERR_CHECKING_IF_DICTIONARY_ROW_EXISTS, ClassName, 'DictionaryRowCodeExists');
  end;

end;

function TDictionaryRepository.DictionaryRowExists(
  DictionaryType: TDictionaryType; Code: string; ParentCode: string): boolean;
var
  query: TZQuery;
  dictionaryId: integer;
  dictionaryParentId: integer;
begin
  try

    GetDictionaryId(DictionaryType, dictionaryId, dictionaryParentId);

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'SELECT COUNT(1) AS Count FROM ' + DB_TABLE_DICTIONARY_ROW +
        ' WHERE DictionaryID = :DictionaryID AND Code = :Code AND ParentDictionaryRowID = ( ' +
        '   SELECT ID FROM ' + DB_TABLE_DICTIONARY_ROW +
        '   WHERE DictionaryID = :DictionaryParentId AND Code = :ParentCode limit 1)'
      );

      query.Params.ParamByName('DictionaryID').AsInteger := dictionaryId;
      query.Params.ParamByName('Code').AsString := Code;
      query.Params.ParamByName('DictionaryParentId').AsInteger := dictionaryParentId;
      query.Params.ParamByName('ParentCode').AsString := ParentCode;

      query.Open;

      Result := query.FieldByName('Count').AsInteger > 0;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      RaiseErrorMessage(ERR_CHECKING_IF_DICTIONARY_ROW_EXISTS, ClassName, 'DictionaryRowCodeExists');
  end;
end;

function TDictionaryRepository.IsDictionaryRowUsedAsAParent(
  DictionaryRowId: integer): boolean;
var
  query: TZQuery;
begin
  try

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'SELECT COUNT(1) AS Count FROM ' + DB_TABLE_DICTIONARY_ROW +
        ' WHERE ParentDictionaryRowID = :ParentDictionaryRowId;'
      );

      query.Params.ParamByName('ParentDictionaryRowId').AsInteger := DictionaryRowId;

      query.Open;

      Result := query.FieldByName('Count').AsInteger > 0;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      RaiseErrorMessage(ERR_IS_DICTIONARY_ROW_USED_AS_A_PARENT, ClassName, 'DictionaryRowExists');
  end;
end;

function TDictionaryRepository.RefreshDictionary(
  ADictionaryType: TDictionaryType): ErrorId;
begin
  Result := ClearDictionary(false, ADictionaryType);
  Result := LoadDictionary(ADictionaryType, false);
end;

function TDictionaryRepository.GetParentDictionaryType(DictionaryType: TDictionaryType;
  out ParentDictionaryType: TDictionaryType): ErrorId;
var
  err: ErrorId;
  query: TZQuery;
begin
  err := ERR_OK;

  try

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'SELECT ParentCode FROM ' + DB_TABLE_DICTIONARY + ' WHERE Code = :DictionaryCode;'
      );

      query.Params.ParamByName('DictionaryCode').AsString := GetDictionaryName(DictionaryType);

      query.Open;

      if query.RecordCount = 1 then
        ParentDictionaryType := GetDictionaryType(query.FieldByName('ParentCode').AsString)
      else
        err := ERR_GET_PARENT_DICTIONARY_KIND;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'GetParentDictionary', E);
        err := ERR_GET_PARENT_DICTIONARY_KIND;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.IsDictionaryItemUsed(DictionaryRowId: integer;
  DictionaryType: TDictionaryType; out IsUsed: boolean): ErrorId;
var
  err: ErrorId;
  query: TZQuery;
  dictionaryRowCode: string;
  itemIsUsed: boolean;
begin
  err := ERR_OK;

  Result := err;
  IsUsed := false;

  try
    err := GetDictionaryRowCode(DictionaryRowId, dictionaryRowCode);

    if err <> ERR_OK then
    begin
      Result := err;
      Exit;
    end;

    if IsDictionaryRowUsedAsAParent(DictionaryRowId) then
    begin
      IsUsed := true;
      Exit;
    end;

    err := TRepository.DoesAnyStationUseTheGivenItemOfTheDictionary(
      DictionaryType, dictionaryRowCode, itemIsUsed);

    if err <> ERR_OK then
    begin
      Result := err;
      Exit;
    end;

    if itemIsUsed then
    begin
      IsUsed := true;
      Exit;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'IsDictionaryItemUsed', E);
        err := ERR_IS_DICTIONARY_ITEM_USED;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.ClearDictionary: ErrorId;
begin
  Result := ClearDictionary(false);
end;

function TDictionaryRepository.LoadDictionaryNames(
  var VstList: TVirtualStringTree; SelectFirst: boolean = false): ErrorId;
var
  err: ErrorId;
  dictionaryType: TDictionaryType;

  node: PVirtualNode;
  data: PDictionaryTableNodeRec;

  dictionaryName: string;
  dictionaryLocalizedName: string;
begin
  err := ERR_OK;

  try

    // Reinit Virtual String Tree
    VstList.Clear;
    VstList.RootNodeCount := Length(FDictionary) - 1; // take away one because we don't need dkNone dictionary item
    VstList.ReinitNode(VstList.RootNode, True);

    VstList.BeginUpdate;

    node := nil;

    for dictionaryType := Low(FDictionary) to High(FDictionary) do
    begin
      if dictionaryType = TDictionaryType.dkNone then
        continue;

      if node = nil then
        node := VstList.GetFirst
      else
        node := VstList.GetNext(node);

      data := VstList.GetNodeData(node);

      dictionaryName := GetDictionaryName(dictionaryType);
      dictionaryLocalizedName := GetLocalizedDictionaryName(dictionaryType);

      data^.dtnd := TDictionaryTableNodeData.Create(dictionaryLocalizedName, dictionaryName, dictionaryType);
    end;

    // Sort items
    VstList.SortTree(0, VstList.Header.SortDirection, false);

    VstList.EndUpdate;

    // Select first node
    if SelectFirst then
      VstList.Selected[VstList.GetFirst] := True;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'LoadAllDictionaryNames', E);
        err := ERR_LOAD_DICTIONARY_NAMES;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.LoadDictionaryDetails(
  var VstList: TVirtualStringTree; DictionaryType: TDictionaryType;
  ParentDictionaryType: TDictionaryType = TDictionaryType.dkNone;
  ParentDictionaryRowCode: string = EMPTY_STR;
  LastUsedDictionaryRowId: integer = EMPTY_INT): ErrorId;
var
  i: integer;
  err: ErrorId;
  dictionaryTable: PDictionaryTable;

  node: PVirtualNode;
  data: PDictionaryDetailTableNodeRec;
  parentCode: string;

  rootNodeCount: integer;
begin
  err := ERR_OK;

  try

    if FDictionary[DictionaryType] <> nil then
    begin

      parentCode := GetDictionaryName(ParentDictionaryType);

      // Load dictionary details if it needed
      LoadDictionary(DictionaryType);

      // Reinit Virtual String Tree
      VstList.Clear;

      //TODO: need to be refactoring
      rootNodeCount := 0;
      for i := 0 to FDictionary[DictionaryType].Count - 1 do
      begin
        dictionaryTable := FDictionary[DictionaryType].Items[i];

        if (parentCode <> EMPTY_STR) and (dictionaryTable^.ParentDictionaryCode = parentCode) and
           (ParentDictionaryRowCode <> dictionaryTable^.ParentDictionaryRowCode) then
          Continue;

        Inc(rootNodeCount)
      end;

      VstList.RootNodeCount := rootNodeCount;
      VstList.ReinitNode(VstList.RootNode, True);

      VstList.BeginUpdate;

      node := nil;

      i := FDictionary[DictionaryType].Count;

      for i := 0 to FDictionary[DictionaryType].Count - 1 do
      begin
        dictionaryTable := FDictionary[DictionaryType].Items[i];

        if (parentCode <> EMPTY_STR) and (dictionaryTable^.ParentDictionaryCode = parentCode) and
           (ParentDictionaryRowCode <> dictionaryTable^.ParentDictionaryRowCode) then
          Continue;

        if node = nil then
          node := VstList.GetFirst
        else
          node := VstList.GetNext(node);

        data := VstList.GetNodeData(node);

        data^.ddtnd := TDictionaryDetailTableNodeData.Create(dictionaryTable^.Id,
          dictionaryTable^.Text, dictionaryTable^.Code, dictionaryTable^.Position);

        if (dictionaryTable^.Id = LastUsedDictionaryRowId) then
          VstList.Selected[node] := true;

      end;

      // Sort items
      VstList.SortTree(0, VstList.Header.SortDirection, false);

      VstList.EndUpdate;

    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'LoadDictionaryDetails', E);
        err := ERR_LOAD_DICTIONARY_DETAILS;
      end;
  end;

  Result := err;
end;

// Free the dictionary and its all items
function TDictionaryRepository.ClearDictionary(AFreeAndNilDictionary: boolean = false;
  ADictionaryType: TDictionaryType = TDictionaryType.dkNone): ErrorId;
var
  err: ErrorId;
  i: integer;
  dictionaryType: TDictionaryType;
begin
  err := ERR_OK;

  try

    for dictionaryType := Low(FDictionary) to High(FDictionary) do
    begin
      if (ADictionaryType in [TDictionaryType.dkNone, dictionaryType]) and (FDictionary[dictionaryType] <> nil) then
      begin
        for i := FDictionary[dictionaryType].Count - 1 downto 0 do
          Dispose(PDictionaryTable(FDictionary[dictionaryType].Items[i]));

        FDictionary[dictionaryType].Clear;

        if AFreeAndNilDictionary then
          FreeAndNil(FDictionary[dictionaryType]);
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

function TDictionaryRepository.GetDictionaryName(DictionaryType: TDictionaryType): string;
begin
  Result := DICTIONARY_NAMES[DictionaryType];
end;

function TDictionaryRepository.GetLocalizedDictionaryName(
  DictionaryType: TDictionaryType): string;
begin
  Result := GetLanguageItem('DictionaryTables.' + GetDictionaryName(DictionaryType));
end;

function TDictionaryRepository.GetDictionaryId(DictionaryType: TDictionaryType;
  out DictionaryId: integer; out DictionaryParentId: integer): ErrorId;
var
  err: ErrorId;
  query: TZQuery;
begin
  err := ERR_OK;

  try

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'SELECT ID, ' +
        '  COALESCE((SELECT ID FROM ' + DB_TABLE_DICTIONARY + ' dParent WHERE dParent.Name = d.ParentCode), :EmptyInt) AS ParentID ' +
        'FROM ' + DB_TABLE_DICTIONARY + ' d ' +

        ' WHERE Code = :DictionaryCode;'
      );

      query.Params.ParamByName('DictionaryCode').AsString := GetDictionaryName(DictionaryType);
      query.Params.ParamByName('EmptyInt').AsInteger := EMPTY_INT;

      query.Open;

      if query.RecordCount = 1 then
      begin
        DictionaryId := query.FieldByName('ID').AsInteger;
        DictionaryParentId := query.FieldByName('ParentID').AsInteger;;
      end
      else
        err := ERR_GET_DICTIONARY_ID;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'GetDictionaryId', E);
        err := ERR_GET_DICTIONARY_ID;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.GetDictionaryTypeByDictionaryRowId(
  DictionaryRowId: integer; out DictionaryType: TDictionaryType): ErrorId;
var
  err: ErrorId;
  query: TZQuery;
begin
  err := ERR_OK;

  try

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'SELECT d.Code FROM ' + DB_TABLE_DICTIONARY + ' d ' +
        'INNER JOIN ' + DB_TABLE_DICTIONARY_ROW + ' r ON r.DictionaryID = d.ID ' +
        'WHERE r.ID = :DictionaryRowId;'
      );

      query.Params.ParamByName('DictionaryRowId').AsInteger := DictionaryRowId;

      query.Open;

      if query.RecordCount = 1 then
        DictionaryType := GetDictionaryType(query.FieldByName('Code').AsString)
      else
        DictionaryType := TDictionaryType.dkNone;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'GetDictionaryTypeByDictionaryRowId', E);
        err := ERR_GET_DICTIONARY_ID_BY_ROW_ID;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.GetDictionaryRowId(DictionaryId: integer;
  Code: string; out DictionaryRowId: integer): ErrorId;
var
  err: ErrorId;
  query: TZQuery;
begin
  err := ERR_OK;

  try

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'SELECT ID FROM ' + DB_TABLE_DICTIONARY_ROW +
        ' WHERE DictionaryID = :DictionaryID AND Code = :Code;'
      );

      query.Params.ParamByName('DictionaryID').AsInteger := DictionaryId;
      query.Params.ParamByName('Code').AsString := Code;

      query.Open;

      if query.RecordCount = 1 then
        DictionaryRowId := query.FieldByName('ID').AsInteger
      else if query.RecordCount = 0 then
        DictionaryRowId := EMPTY_INT
      else
        err := ERR_GET_DICTIONARY_ROW_ID;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'GetDictionaryRowId', E);
        err := ERR_GET_DICTIONARY_ROW_ID;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.GetDictionaryRowCode(DictionaryRowId: integer;
  out Code: string): ErrorId;
var
  err: ErrorId;
  query: TZQuery;
begin
  err := ERR_OK;

  try

    Code := EMPTY_STR;

    query := TZQuery.Create(nil);
    try
      query.Connection := TRepository.GetDbConnection;

      query.SQL.Add(
        'SELECT Code FROM ' + DB_TABLE_DICTIONARY_ROW +
        ' WHERE ID = :DictionaryRowId;'
      );

      query.Params.ParamByName('DictionaryRowId').AsInteger := DictionaryRowId;

      query.Open;

      if query.RecordCount = 1 then
        Code := query.FieldByName('CODE').AsString
      else if query.RecordCount = 0 then
        Code := EMPTY_STR
      else
        err := ERR_GET_DICTIONARY_ROW_CODE;

    finally
      query.Free;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'GetDictionaryRowCode', E);
        err := ERR_GET_DICTIONARY_ROW_CODE;
      end;
  end;

  Result := err;
end;

function TDictionaryRepository.GetDictionaryType(DictionaryCode: string): TDictionaryType;
var
  dictionaryType: TDictionaryType;
begin

  if (Trim(DictionaryCode) = EMPTY_STR) then
  begin
    Result := TDictionaryType.dkNone;
    exit;
  end;

  for dictionaryType := Low(FDictionary) to High(FDictionary) do
    if GetDictionaryName(dictionaryType) = DictionaryCode then
    begin
      Result := dictionaryType;
      exit;
    end;

  RaiseErrorMessage(ERR_IN_DETERMINING_TYPE_OF_DICTIONARY, ClassName, 'GetDictionaryType');
end;

end.

