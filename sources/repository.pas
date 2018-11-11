unit Repository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZConnection, MainRepository, BaseRepository;

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

    class procedure DoSomething(ShowThisText: string);
  end;

implementation

uses
  RadioPlayerTypes, TRPErrors;

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

class procedure TRepository.DoSomething(ShowThisText: string);
begin
  FMainRepo.StationRepo.DoSomething(ShowThisText);
end;

initialization
  TRepository.ConnectToMainRepository();

finalization
  TRepository.DisconnectFromMainRepository();

end.

