unit Consts;
{===============================================================================
File:                Consts.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Constants used in the application

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  EMPTY_INT         = -1;
  EMPTY_STR         = '';
  NEW_LINE          = #13#10;

  {$IFDEF WINDOWS}
  LIB_PATH = 'data/lib/';
  LANGUAGE_PATH = 'data/lang/';
  {$ELSE}
  LIB_PATH = 'data/lib/';
  LANGUAGE_PATH = 'data/lang/';
  {$ENDIF}

  LOG_NAME = 'Application.log';

  SETTINGS_FILE = 'settings.trp';

{ - - - - - - - - - - - - - - - - - Language - - - - - - - - - - - - - - - - - }
  LANGUAGE_FILE_PREFIX = 'language-';
  LANGUAGE_FILE_EXTENSION = '.xml';
  LANGUAGE_FILE_PATTERN = LANGUAGE_FILE_PREFIX + '*' + LANGUAGE_FILE_EXTENSION;
  DEFAULT_LANGUAGE = 'en';
  USE_OS_LANGUAGE_INSTEAD_DEFAULT = true;

implementation

end.

