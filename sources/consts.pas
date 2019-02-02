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
  Classes, SysUtils, Graphics, RadioPlayerTypes;

const
  EMPTY_INT         = -1;
  EMPTY_STR         = '';
  NEW_LINE          = #13#10;
  INVALID_INT       = -1;

  DATE_TIME_FORMAT  = 'yyyy.mm.dd hh:nn:ss';

  {$IFDEF WINDOWS}
  LIB_PATH = 'data\lib\';
  LANGUAGE_PATH = 'data\lang\';
  DB_PATH = 'data\db\';
  SKINS_PATH = 'data\skins\';
  {$ELSE}
  LIB_PATH = 'data/lib/';
  LANGUAGE_PATH = 'data/lang/';
  DB_PATH = 'data/db/';
  SKINS_PATH = 'data/skins/';
  {$ENDIF}

  LOG_NAME = 'Application.log';

  SETTINGS_FILE = 'settings.trp';

{ - - - - - - - - - - - - - - - - - Language - - - - - - - - - - - - - - - - - }
  LANGUAGE_FILE_PREFIX      = 'language-';
  LANGUAGE_FILE_EXTENSION   = '.xml';
  LANGUAGE_FILE_PATTERN     = LANGUAGE_FILE_PREFIX + '*' + LANGUAGE_FILE_EXTENSION;
  DEFAULT_LANGUAGE          = 'en';
  USE_OS_LANGUAGE_INSTEAD_DEFAULT = true;

{ - - - - - - - - - - - - - - - - - - Skins - - - - - - - - - - - - - - - - - - }
  SKIN_FILE_EXTENSION       = '.zip';
  DEFAULT_SKIN              = 'skin';
  SKIN_FILE_PATTERN         = '*' + SKIN_FILE_EXTENSION;

{ - - - - - - - - - - - - - - - - - Database - - - - - - - - - - - - - - - - - }
  DB_TABLE_STATIONS         = 'Stations';
  DB_TABLE_GENRES           = 'Genres';
  DB_TABLE_COUNTRIES        = 'Countries';
  DB_TABLE_DICTIONARY       = 'Dictionary';
  DB_TABLE_DICTIONARY_ROW   = 'DictionaryRow';
  DB_MAIN_DB_FILE_NAME      = 'database.db';

{ - - - - - - - - - - - - - - - - - Dictionary - - - - - - - - - - - - - - - - }
  DICTIONARY_GENRE_CODE     = 'Genre';
  DICTIONARY_REGION_CODE    = 'Region';
  DICTIONARY_COUNTRY_CODE   = 'Country';
  DICTIONARY_NAMES: ARRAY[Low(TDictionaryType)..High(TDictionaryType)] of string = (EMPTY_STR, DICTIONARY_GENRE_CODE, DICTIONARY_REGION_CODE, DICTIONARY_COUNTRY_CODE);

{ - - - - - - - - - - - - - - - - - Grid Colors - - - - - - - - - - - - - - - - }
GridLineColor: TColor = (250 or (250 shl 8) or (250 shl 16)); // RGB(250, 250, 250);
GridLineColorCurrentlyPlaying: TColor = (199 or (237 shl 8) or (252 shl 16)); // RGB(255, 187, 187);

{ - - - - - - - - - - - - - - - - - - Colors - - - - - - - - - - - - - - - - - }
ColorBlue: TColor = (220 or (234 shl 8) or (248 shl 16));
ColorGreen: TColor = (220 or (245 shl 8) or (193 shl 16));
ColorRed: TColor = (249 or (226 shl 8) or (225 shl 16));
ColorBlack: TColor = (0 or (0 shl 8) or (0 shl 16));

implementation

end.

