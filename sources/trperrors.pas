unit TRPErrors;
{===============================================================================
File:                TRPErrors.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Errors management. Definition of Error Codes

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioPlayerTypes;

{ - - - - - - - - - Definition of Error Codes - - - - - - - - - - - - - - - -  }
const
  ERR_OK                                    = $00000000;
  ERR_UNSPECIFIED_ERROR                     = $00000001;
  ERR_DB_NO_DATABASE_NAME                   = $00000002;
  ERR_DB_CONNECT_ERROR                      = $00000003;
  ERR_DB_DISCONNECT_ERROR                   = $00000004;
  ERR_DB_CREATE_ERROR                       = $00000005;
  ERR_DB_CREATE_DDL_ERROR                   = $00000006;
  ERR_DB_CREATE_DML_ERROR                   = $00000007;
  ERR_DB_CREATE_DIR                         = $00000008;
  ERR_DB_ADD_STATION                        = $00000009;


  function ShowErrorMessage(Err: ErrorId): boolean;
  function GetErrorMessage(Err: ErrorId): String;

implementation

uses
  Dialogs, Helpers;

function ShowErrorMessage(Err: ErrorId): boolean;
begin
  Result := false;

  if Err <> ERR_OK then
  begin
    Result := true;
    ShowMessage(GetErrorMessage(Err));
  end;
end;

function GetErrorMessage(Err: ErrorId): String;
begin
  case Err of
    ERR_UNSPECIFIED_ERROR:
      Result := GetLanguageItem('ErrorMessage.UnspecifiedError',
             'An unspecified error occurred!');
    else
      Result := GetLanguageItem('ErrorMessage.UnspecifiedError',
        'An unspecified error occurred!');
  end;
end;

end.
