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
  Classes, SysUtils, RadioPlayerTypes, Consts;

{ - - - - - - - - - Definition of Error Codes - - - - - - - - - - - - - - - -  }
const
  ERR_OK                                    = $00000000;
  ERR_UNSPECIFIED_ERROR                     = $00000001;

  // Database core
  ERR_DB_NO_DATABASE_NAME                   = $00000011;
  ERR_DB_CONNECT_ERROR                      = $00000012;
  ERR_DB_DISCONNECT_ERROR                   = $00000013;
  ERR_DB_CREATE_ERROR                       = $00000014;
  ERR_DB_CREATE_DDL_ERROR                   = $00000015;
  ERR_DB_CREATE_DML_ERROR                   = $00000016;
  ERR_DB_CREATE_DIR                         = $00000017;

  // Station Repository
  ERR_DB_ADD_STATION                        = $00000031;
  ERR_DB_LOAD_STATIONS                      = $00000032;
  ERR_DB_LOAD_STATION                       = $00000033;
  ERR_DB_UPDATE_STATION                     = $00000034;
  ERR_DB_DELETE_STATION                     = $00000035;
  ERR_DB_IS_STATION_EXISTS                  = $00000036;
  ERR_DB_STATION_ALREADY_EXISTS             = $00000037;

  // Dictionary Repository
  ERR_DB_ADD_GENRE                          = $00000101;
  ERR_DB_ADD_COUNTRY                        = $00000102;
  ERR_DB_ADD_DICTIONARY                     = $00000103;
  ERR_DB_ADD_DICTIONARY_ROW                 = $00000104;
  ERR_DB_LOAD_DICTIONARY                    = $00000105;
  ERR_DB_CLEAR_DICTIONARY                   = $00000106;
  ERR_ADD_DICTIONARY_ITEMS_TO_COMBOBOX      = $00000107;
  ERR_FIND_ITEM_IN_COMBOBOX                 = $00000108;
  ERR_GET_CODE_FROM_SELECTED_ITEM           = $00000109;
  ERR_LOAD_DICTIONARY_NAMES                 = $0000010A;
  ERR_LOAD_DICTIONARY_DETAILS               = $0000010B;

  procedure ShowErrorMessage(const Err: ErrorId;
    const AClassName: string = EMPTY_STR;
    const AMethodName: string = EMPTY_STR);

  procedure ShowWarningMessage(const Err: ErrorId);

  procedure ShowInfoMessage(const Err: ErrorId);

  function GetErrorMessage(const Err: ErrorId): String;

implementation

uses
  Dialogs, Helpers, Log;

procedure ShowErrorMessage(const Err: ErrorId;
  const AClassName: string = EMPTY_STR;
  const AMethodName: string = EMPTY_STR);
var
  errorMessage: string;
begin
  if Err <> ERR_OK then
  begin
    errorMessage := GetErrorMessage(Err);

    if (Trim(AClassName) <> EMPTY_STR) or (Trim(AMethodName) <> EMPTY_STR) then
      TLog.LogError(
        Format('Code[%d] %s', [err, errorMessage]),
        AClassName, AMethodName);

    MessageDlg(
      GetLanguageItem('ErrorMessage.General', 'Error!'),
      errorMessage, TMsgDlgType.mtError, [mbOK], 0);
  end;
end;

procedure ShowWarningMessage(const Err: ErrorId);
var
  errorMessage: string;
begin
  if Err <> ERR_OK then
  begin
    errorMessage := GetErrorMessage(Err);

    MessageDlg(
      GetLanguageItem('WarningMessage.General', 'Warning!'),
      errorMessage, TMsgDlgType.mtWarning, [mbOK], 0);
  end;
end;

procedure ShowInfoMessage(const Err: ErrorId);
var
  errorMessage: string;
begin
  if Err <> ERR_OK then
  begin
    errorMessage := GetErrorMessage(Err);

    MessageDlg(
      GetLanguageItem('InformationMessage.General', 'Information!'),
      errorMessage, TMsgDlgType.mtInformation, [mbOK], 0);
  end;

end;

function GetErrorMessage(const Err: ErrorId): String;
begin
  case Err of
    ERR_UNSPECIFIED_ERROR:
      Result := GetLanguageItem('ErrorMessage.UnspecifiedError',
             'An unspecified error occurred!');
    ERR_DB_STATION_ALREADY_EXISTS:
      Result := GetLanguageItem('StationDetail.Error.StationAlreadyExists',
             'A station with this name already exists!');
    else
      Result := GetLanguageItem('ErrorMessage.UnspecifiedError',
        'An unspecified error occurred!');
  end;
end;

end.

