unit Repository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MainRepository, BaseRepository;

var
  MainRepo: TBaseRepository;

implementation

uses
  RadioPlayerTypes, TRPErrors;

// Local functions that can only be used inside this unit
procedure ConnectToMainRepository(); forward;
procedure DisconnectFromMainRepository(); forward;

procedure ConnectToMainRepository();
var
  err: ErrorId;
begin
  MainRepo := TMainRepository.Create;
  err := MainRepo.ConnectDB();
  ShowErrorMessage(err);
end;

procedure DisconnectFromMainRepository();
begin
  if Assigned(MainRepo) then
  begin
    MainRepo.DisconnectDB();
    FreeAndNil(MainRepo);
  end;
end;

initialization
  ConnectToMainRepository();

finalization
  DisconnectFromMainRepository();


end.

