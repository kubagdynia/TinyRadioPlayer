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
  Classes, SysUtils, Graphics;

const
  EMPTY_INT         = -1;
  EMPTY_STR         = '';
  NEW_LINE          = #13#10;
  INVALID_INT       = -1;

  {$IFDEF WINDOWS}
  LIB_PATH = 'data\lib\';
  LANGUAGE_PATH = 'data\lang\';
  DB_PATH = 'data\db\';
  {$ELSE}
  LIB_PATH = 'data/lib/';
  LANGUAGE_PATH = 'data/lang/';
  DB_PATH = 'data/db/';
  {$ENDIF}

  LOG_NAME = 'Application.log';

  SETTINGS_FILE = 'settings.trp';

{ - - - - - - - - - - - - - - - - - Language - - - - - - - - - - - - - - - - - }
  LANGUAGE_FILE_PREFIX = 'language-';
  LANGUAGE_FILE_EXTENSION = '.xml';
  LANGUAGE_FILE_PATTERN = LANGUAGE_FILE_PREFIX + '*' + LANGUAGE_FILE_EXTENSION;
  DEFAULT_LANGUAGE = 'en';
  USE_OS_LANGUAGE_INSTEAD_DEFAULT = true;

{ - - - - - - - - - - - - - - - - - Database - - - - - - - - - - - - - - - - - }
  DB_TABLE_STATIONS = 'Stations';
  DB_MAIN_DB_FILE_NAME = 'database.trp';

{ - - - - - - - - - - - - - - - - - Grid Colors - - - - - - - - - - - - - - - - }
GridLineColor: TColor = (250 or (250 shl 8) or (250 shl 16)); // RGB(250, 250, 250);
GridLineColorCurrentlyPlaying: TColor = (199 or (237 shl 8) or (252 shl 16)); // RGB(255, 187, 187);

implementation

end.

