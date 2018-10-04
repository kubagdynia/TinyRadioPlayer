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
  {$ELSE}
  LIB_PATH = 'data/lib/';
  {$ENDIF}

implementation

end.

