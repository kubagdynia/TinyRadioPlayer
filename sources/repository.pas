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
  BaseRepository, VirtualTrees;

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

    // Stations
    class function AddStation(const StationName: string; const StreamUrl: string;
      out StationId: integer): ErrorId;
    class function AddStation(const StationName: string; const StreamUrl: string;
      const Description: string; const WebpageUrl: string;
      const GenreCode: string; const CountryCode: string;
      out StationId: integer): ErrorId;
    class function AddStation(StationInfo: TStationInfo; out StationId: integer): ErrorId;
    class function UpdateStation(StationInfo: TStationInfo): ErrorId;
    class function DeleteStation(StationId: integer): ErrorId;
    class function LoadStations(var VstList: TVirtualStringTree; const Text: string): ErrorId;
    class function LoadStation(var StationInfo: TStationInfo; const StationId: integer): ErrorId;
    class function GetSelectedStationId(var VstList: TVirtualStringTree): integer;

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
    class function LoadDictionary(DictionaryKind: TDictionaryKind;
      SkipIfLoaded: boolean = true; SortDirection: TSortDirection = sdAscending): ErrorId;
    class function ClearDictionary: ErrorId;
    class function AddDictionaryItemsToComboBox(var ComboBox: TComboBox;
      DictionaryKind: TDictionaryKind; FirstBlank: boolean): ErrorId;
    class function FindAnItemInTheComboBox(var ComboBox: TComboBox; Code: string): ErrorId;
    class function GetDictionaryCodeFromSelectedItem(var ComboBox: TComboBox;
      out DictionaryCode: string): ErrorId;
    class function LoadDictionaryNames(var VstList: TVirtualStringTree): ErrorId;
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

class function TRepository.AddStation(const StationName: string;
  const StreamUrl: string; out StationId: integer): ErrorId;
begin
  Result := FMainRepo.StationRepo.AddStation(StationName, StreamUrl, StationId);
end;

class function TRepository.AddStation(const StationName: string;
  const StreamUrl: string; const Description: string; const WebpageUrl: string;
  const GenreCode: string; const CountryCode: string; out StationId: integer): ErrorId;
begin
  Result := FMainRepo.StationRepo.AddStation(StationName, StreamUrl, Description,
    WebpageUrl, GenreCode, CountryCode, StationId);
end;

class function TRepository.AddStation(StationInfo: TStationInfo; out StationId: integer): ErrorId;
begin
  Result := FMainRepo.StationRepo.AddStation(StationInfo, StationId);
end;

class function TRepository.UpdateStation(StationInfo: TStationInfo): ErrorId;
begin
  Result := FMainRepo.StationRepo.UpdateStation(StationInfo);
end;

class function TRepository.DeleteStation(StationId: integer): ErrorId;
begin
  Result := FMainRepo.StationRepo.DeleteStation(StationId);
end;

class function TRepository.LoadStations(var VstList: TVirtualStringTree;
  const Text: string): ErrorId;
begin
  Result := FMainRepo.StationRepo.LoadStations(VstList, Text);
end;

class function TRepository.LoadStation(var StationInfo: TStationInfo;
  const StationId: integer): ErrorId;
begin
  Result := FMainRepo.StationRepo.LoadStation(StationInfo, StationId);
end;

class function TRepository.GetSelectedStationId(var VstList: TVirtualStringTree): integer;
begin
  Result := FMainRepo.StationRepo.GetSelectedStationId(VstList);
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

class function TRepository.LoadDictionary(DictionaryKind: TDictionaryKind;
  SkipIfLoaded: boolean = true; SortDirection: TSortDirection = sdAscending): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.LoadDictionary(DictionaryKind, SkipIfLoaded, SortDirection);
end;

class function TRepository.ClearDictionary: ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.ClearDictionary;
end;

class function TRepository.AddDictionaryItemsToComboBox(
  var ComboBox: TComboBox; DictionaryKind: TDictionaryKind; FirstBlank: boolean): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.AddDictionaryItemsToComboBox(
    ComboBox, DictionaryKind, FirstBlank);
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

class function TRepository.LoadDictionaryNames(
  var VstList: TVirtualStringTree): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.LoadDictionaryNames(VstList);
end;

initialization
  TRepository.ConnectToMainRepository();

finalization
  TRepository.DisconnectFromMainRepository();

end.

