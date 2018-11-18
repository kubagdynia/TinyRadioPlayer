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
  Classes, SysUtils, ZConnection, RadioPlayerTypes, MainRepository,
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
    class function LoadStations(var VstList: TVirtualStringTree; const Text: string): ErrorId;

    // Dictionary
    class function AddDictionary(const Name: string; const Code: string;
      const Description: string; out DictionaryId: integer): ErrorId;
    class function AddDictionary(const Name: string; const Code: string;
      out DictionaryId: integer): ErrorId;
    function AddDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryId: integer; const ParentDictionaryId: integer;
      out DictionaryRowId: integer): ErrorId;
    function AddDictionaryRow(const Text: string; const Code: string;
      const Position: integer; const DictionaryId: integer;
      out DictionaryRowId: integer): ErrorId;
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

class function TRepository.LoadStations(var VstList: TVirtualStringTree;
  const Text: string): ErrorId;
begin
  Result := FMainRepo.StationRepo.LoadStations(VstList, Text);
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

function TRepository.AddDictionaryRow(const Text: string; const Code: string;
  const Position: integer; const DictionaryId: integer;
  const ParentDictionaryId: integer; out DictionaryRowId: integer): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.AddDictionaryRow(Text, Code, Position, DictionaryId,
    ParentDictionaryId, DictionaryRowId);
end;

function TRepository.AddDictionaryRow(const Text: string; const Code: string;
  const Position: integer; const DictionaryId: integer; out
  DictionaryRowId: integer): ErrorId;
begin
  Result := FMainRepo.DictionaryRepo.AddDictionaryRow(Text, Code, Position, DictionaryId,
    DictionaryRowId);
end;

initialization
  TRepository.ConnectToMainRepository();

finalization
  TRepository.DisconnectFromMainRepository();

end.

