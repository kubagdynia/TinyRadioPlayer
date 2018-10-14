unit Errors;
{===============================================================================
File:                Errors.pas

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


  procedure ShowErrorMessage(Err: ErrorId);
  function GetErrorMessage(Err: ErrorId): String;

implementation

uses
  Dialogs, Helpers;

procedure ShowErrorMessage(Err: ErrorId);
begin
  ShowMessage(GetErrorMessage(Err));
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

