unit Repository;
{===============================================================================
File:                Repository.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Repository wrapper

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ZConnection, RadioPlayerTypes, MainRepository,
  BaseRepository, VirtualTrees, Consts, contnrs;

type

  { TRepository }

  TRepository = class sealed (TObject)
  private
    class var FMainRepo: TBaseRepository;

  public
    class procedure ConnectToMainRepository();
    class procedure DisconnectFromMainRepository();

    class function GetDbConnection: TZConnection;
    class function GetNewDbTableKey(const TableName: string): integer;
    class function GetNewDbTableKeyAsGUID: string;

    // Stations
    class function AddStation(const StationName: string; const StreamUrl: string;
      out StationId: string): ErrorId;
    class function AddStation(const StationName: string; const StreamUrl: string;
      const Description: string; const WebpageUrl: string;
      const GenreCode: string; const CountryCode: string; const RegionCode: string;
      out StationId: string): ErrorId;
    class function AddStation(StationInfo: TStationInfo; out StationId: string): ErrorId;
    class function UpdateStation(StationInfo: TStationInfo): ErrorId;
    class function DeleteStation(StationId: string): ErrorId;
    class function LoadStations(var VstList: TVirtualStringTree; const Text: string): ErrorId;
    class function LoadStation(var StationInfo: TStationInfo; const StationId: string): ErrorId;
    class function GetSelectedStationId(var VstList: TVirtualStringTree): string;
    class function DoesAnyStationUseTheGivenItemOfTheDictionary(
      DictionaryType: TDictionaryType; DictionaryRowCode: string;
      out ItemIsUsed: boolean): ErrorId;
    class function UpdateStationDictionaryCode(DictionaryType: TDictionaryType;
      OldCode: string; NewCode: string): ErrorId;
    class function GetAllStations(out AStationList : TObjectList): ErrorId;

    // Dictionary
    class function AddDictionary(const Name: string; const Code: string;
      const Description: string; out DictionaryId: integer): ErrorId;
    class function AddDictionary(const Name: string; const Code: string;
      out DictionaryId: integer): ErrorId;
    class function AddDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryId: integer; const ParentDictionaryId: integer;
      out DictionaryRowId: integer): ErrorId;
    class function AddDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryId: integer;
      out DictionaryRowId: integer): ErrorId;
    class function AddDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryCode: string; const ParentDictionaryCode: string;
      out DictionaryRowId: integer): ErrorId;
    class function UpdateDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryCode: string; const ParentDictionaryCode: string;
      DictionaryRowId: integer): ErrorId;
    class function DeleteDictionaryRow(DictionaryRowId: integer): ErrorId;
    class function GetDictionaryName(DictionaryType: TDictionaryType): string;
    class function GetLocalizedDictionaryName(DictionaryType: TDictionaryType): string;
    class function LoadDictionary(DictionaryType: TDictionaryType;
      SkipIfLoaded: boolean = true; SortDirection: TSortDirection = sdAscending): ErrorId;
    class function ClearDictionary: ErrorId;
    class function AddDictionaryItemsToComboBox(var ComboBox: TComboBox;
      DictionaryType: TDictionaryType; FirstBlank: boolean): ErrorId;
    class function FindAnItemInTheComboBox(var ComboBox: TComboBox; Code: string): ErrorId;
    class function GetDictionaryCodeFromSelectedItem(var ComboBox: TComboBox;
      out DictionaryCode: string): ErrorId;
    class function GetDictionaryCodeFromSelectedItem(var ComboBox: TComboBox;
      out DictionaryCode: string; out ParentDictionaryCode: string): ErrorId;
    class function LoadDictionaryNames(var VstList: TVirtualStringTree;
      SelectFirst: boolean = false): ErrorId;
    class function LoadDictionaryDetails(var VstList: TVirtualStringTree;
      DictionaryType: TDictionaryType;
      ParentDictionaryType: TDictionaryType = TDictionaryType.dkNone;
      ParentDictionaryRowCode: string = EMPTY_STR;
      LastUsedDictionaryRowId: integer = EMPTY_INT): ErrorId;
    class function GetDictionaryTypeByDictionaryRowId(DictionaryRowId: integer;
      out DictionaryType: TDictionaryType): ErrorId;
    class function GetParentDictionaryType(DictionaryType: TDictionaryType;
      out ParentDictionaryType: TDictionaryType): ErrorId;
    class function GetDictionaryId(DictionaryType: TDictionaryType;
      out DictionaryId: integer; out DictionaryParentId: integer): ErrorId;
    class function GetDictionaryRowId(DictionaryId: integer; Code: string;
      out DictionaryRowId: integer): ErrorId;
    class function GetDictionaryRowCode(DictionaryRowId: integer;
      out Code: string): ErrorId;
    class function GetAllDictionaries(out ADictionaryList: TObjectList): ErrorId;
  end;

implementation

uses
  TRPErrors;

{ TRepository }

class procedure TRepository.ConnectToMainRepository();
var
  err: ErrorId;
begin
  FMainRepo := TMainRepository.Create;
  err := FMainRepo.ConnectDB();
  ShowErrorMessage(err);
end;

class procedure TRepository.DisconnectFromMainRepository();
begin
  if Assigned(FMainRepo) then
  begin
    FMainRepo.DisconnectDB();
    FreeAndNil(FMainRepo);
  end;

end;

class function TRepository.GetDbConnection: TZConnection;
begin
  Result := FMainRepo.Connection;
end;

class function TRepository.GetNewDbTableKey(const TableName: string): integer;
begin
  Result := FMainRepo.GetNewTableKey(TableName);
end;

class function TRepository.GetNewDbTableKeyAsGUID: string;
begin
  Result := FMainRepo.GetNewTableKeyAsGUID;
end;

class function TRepository.AddStation(const StationName: string;
  const StreamUrl: string; out StationId: string): ErrorId;
begin
  Result := FMainRepo.StationRepo.AddStation(StationName, StreamUrl, StationId);
end;

class function TRepository.AddStation(const StationName: string;
  const StreamUrl: string; const Description: string; const WebpageUrl: string;
  const GenreCode: string; const CountryCode: string; const RegionCode: string;
  out StationId: string): ErrorId;
begin
  Result := FMainRepo.StationRepo.AddStation(StationName, StreamUrl, Description,
    WebpageUrl, GenreCode, CountryCode, RegionCode, StationId);
end;

class function TRepository.AddStation(StationInfo: TStationInfo; out StationId: string): ErrorId;
begin
  Result := FMainRepo.StationRepo.AddStation(StationInfo, StationId);
end;

class function TRepository.UpdateStation(StationInfo: TStationInfo): ErrorId;
begin
  Result := FMainRepo.StationRepo.UpdateStation(StationInfo);
end;

class function TRepository.DeleteStation(StationId: string): ErrorId;
begin
  Result := FMainRepo.StationRepo.DeleteStation(StationId);
end;

class function TRepository.LoadStations(var VstList: TVirtualStringTree;
  const Text: string): ErrorId;
begin
  Result := FMainRepo.StationRepo.LoadStations(VstList, Text);
end;

class function TRepository.LoadStation(var StationInfo: TStationInfo;
  const StationId: string): ErrorId;
begin
  Result := FMainRepo.StationRepo.LoadStation(StationInfo, StationId);
end;

class function TRepository.GetSelectedStationId(var VstList: TVirtualStringTree): string;
begin
  Result := FMainRepo.StationRepo.GetSelectedStationId(VstList);
end;

class function TRepository.DoesAnyStationUseTheGivenItemOfTheDictionary(
  DictionaryType: TDictionaryType; DictionaryRowCode: string; out
  ItemIsUsed: boolean): ErrorId;
begin
  Result := FMainRepo.StationRepo.DoesAnyStationUseTheGivenItemOfTheDictionary(
    DictionaryType, DictionaryRowCode, ItemIsUsed);
end;

class function TRepository.UpdateStationDictionaryCode(
  DictionaryType: TDictionaryType; OldCode: string; NewCode: string): ErrorId;
begin
  Result := FMainRepo.StationRepo.UpdateStationDictionaryCode(DictionaryType, OldCode, NewCode);
end;

class function TRepository.GetAllStations(out AStationList: TObjectList): ErrorId;
begin
  Result := FMainRepo.StationRepo.GetAllStations(AStationList);
end;

class function TRepository.AddDictionary(const Name: string;
  const Code: string; const Description: string; out DictionaryId: integer): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.AddDictionary(Name, Code, Description, DictionaryId);
end;

class function TRepository.AddDictionary(const Name: string;
  const Code: string; out DictionaryId: integer): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.AddDictionary(Name, Code, DictionaryId);
end;

class function TRepository.AddDictionaryRow(const Text: string; const Code: string;
  const Position: integer; const DictionaryId: integer;
  const ParentDictionaryId: integer; out DictionaryRowId: integer): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.AddDictionaryRow(Text, Code, Position, DictionaryId,
    ParentDictionaryId, DictionaryRowId);
end;

class function TRepository.AddDictionaryRow(const Text: string; const Code: string;
  const Position: integer; const DictionaryId: integer; out
  DictionaryRowId: integer): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.AddDictionaryRow(Text, Code, Position, DictionaryId,
    DictionaryRowId);
end;

class function TRepository.AddDictionaryRow(const Text: string;
  const Code: string; const Position: integer; const DictionaryCode: string;
  const ParentDictionaryCode: string; out DictionaryRowId: integer): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.AddDictionaryRow(Text, Code, Position,
    DictionaryCode, parentDictionaryCode, DictionaryRowId);
end;

class function TRepository.UpdateDictionaryRow(const Text: string;
  const Code: string; const Position: integer; const DictionaryCode: string;
  const ParentDictionaryCode: string; DictionaryRowId: integer): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.UpdateDictionaryRow(Text, Code, Position,
    DictionaryCode, parentDictionaryCode, DictionaryRowId);
end;

class function TRepository.DeleteDictionaryRow(DictionaryRowId: integer): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.DeleteDictionaryRow(DictionaryRowId);
end;

class function TRepository.GetDictionaryName(DictionaryType: TDictionaryType): string;
begin
  Result := FMainRepo.DictionaryRepo.GetDictionaryName(DictionaryType);
end;

class function TRepository.GetLocalizedDictionaryName(
  DictionaryType: TDictionaryType): string;
begin
  Result := FMainRepo.DictionaryRepo.GetLocalizedDictionaryName(DictionaryType);
end;

class function TRepository.LoadDictionary(DictionaryType: TDictionaryType;
  SkipIfLoaded: boolean = true; SortDirection: TSortDirection = sdAscending): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.LoadDictionary(DictionaryType, SkipIfLoaded, SortDirection);
end;

class function TRepository.ClearDictionary: ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.ClearDictionary;
end;

class function TRepository.AddDictionaryItemsToComboBox(
  var ComboBox: TComboBox; DictionaryType: TDictionaryType; FirstBlank: boolean): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.AddDictionaryItemsToComboBox(
    ComboBox, DictionaryType, FirstBlank);
end;

class function TRepository.FindAnItemInTheComboBox(var ComboBox: TComboBox;
  Code: string): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.FindAnItemInTheComboBox(ComboBox, Code);
end;

class function TRepository.GetDictionaryCodeFromSelectedItem(
  var ComboBox: TComboBox; out DictionaryCode: string): ErrorId;
begin
  Result :=
    FMainRepo.DictionaryRepo.GetDictionaryCodeFromSelectedItem(ComboBox, DictionaryCode);
end;

class function TRepository.GetDictionaryCodeFromSelectedItem(
  var ComboBox: TComboBox; out DictionaryCode: string; out
  ParentDictionaryCode: string): ErrorId;
begin
  Result :=
    FMainRepo.DictionaryRepo.GetDictionaryCodeFromSelectedItem(ComboBox, DictionaryCode, ParentDictionaryCode);
end;

class function TRepository.LoadDictionaryNames(
  var VstList: TVirtualStringTree; SelectFirst: boolean = false): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.LoadDictionaryNames(VstList, SelectFirst);
end;

class function TRepository.LoadDictionaryDetails(
  var VstList: TVirtualStringTree; DictionaryType: TDictionaryType;
  ParentDictionaryType: TDictionaryType = TDictionaryType.dkNone;
  ParentDictionaryRowCode: string = EMPTY_STR;
  LastUsedDictionaryRowId: integer = EMPTY_INT): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.LoadDictionaryDetails(VstList,
    DictionaryType, ParentDictionaryType, ParentDictionaryRowCode,
    LastUsedDictionaryRowId);
end;

class function TRepository.GetDictionaryTypeByDictionaryRowId(
  DictionaryRowId: integer; out DictionaryType: TDictionaryType): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.GetDictionaryTypeByDictionaryRowId(DictionaryRowId, DictionaryType);
end;

class function TRepository.GetParentDictionaryType(
  DictionaryType: TDictionaryType; out ParentDictionaryType: TDictionaryType): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.GetParentDictionaryType(DictionaryType, ParentDictionaryType);
end;

class function TRepository.GetDictionaryId(DictionaryType: TDictionaryType;
  out DictionaryId: integer; out DictionaryParentId: integer): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.GetDictionaryId(DictionaryType,
    DictionaryId, DictionaryParentId);
end;

class function TRepository.GetDictionaryRowId(DictionaryId: integer;
  Code: string; out DictionaryRowId: integer): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.GetDictionaryRowId(DictionaryId, Code, DictionaryRowId);
end;

class function TRepository.GetDictionaryRowCode(DictionaryRowId: integer; out
  Code: string): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.GetDictionaryRowCode(DictionaryRowId, Code);
end;

class function TRepository.GetAllDictionaries(out ADictionaryList: TObjectList): ErrorId;
begin
  REsult := FMainRepo.DictionaryRepo.GetAllDictionaries(ADictionaryList);
end;

initialization
  TRepository.ConnectToMainRepository();

finalization
  TRepository.DisconnectFromMainRepository();

end.

