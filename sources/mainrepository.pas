unit MainRepository;
{===============================================================================
File:                MainRepository.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Database management, the main database

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioPlayerTypes, BaseRepository;

type

  TMainRepository = class (TBaseRepository)
  private

  protected
    function GetDBPath: string; override;
    function CreateDML: ErrorId; override;
  public
    constructor Create; override;

    function GetDBName: string; override;
  end;

implementation

uses
  Consts, ZDataset, TRPErrors, Helpers, dateutils;

// Constructor
constructor TMainRepository.Create;
begin
  inherited Create;
end;

// Function returns the path to the database
function TMainRepository.GetDBPath: string;
begin
  Result := DB_PATH;
end;

function TMainRepository.CreateDML: ErrorId;
var
  err: ErrorId;
  stationId: integer;
begin
  err := ERR_OK;

  try
    err := AddStation('Radio Kaszebe', 'http://stream3.nadaje.com:8048', stationId);
  except
    on E: Exception do
    begin
      LogException(EmptyStr, ClassName, 'CreateDML', E);
      err := ERR_DB_CREATE_DML_ERROR;
    end;
  end;

  Result := err;
end;

function TMainRepository.GetDBName: string;
begin
  Result := DB_MAIN_DB_FILE_NAME;
end;

end.

