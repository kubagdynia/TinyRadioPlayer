unit Helpers;
{===============================================================================
File:                Helpers.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Useful functions and procedures

================================================================================}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Consts, LCLType;

var
  ApplicationPath: string = EMPTY_STR;

  MainWindowHandle: LCLType.HWND;

  function GetApplicationPath: string;
  function IIF(cond: Boolean; t, f: Variant): Variant;


implementation

// This function returns the application main path
function GetApplicationPath: string;
begin
  if ApplicationPath = EMPTY_STR then
    ApplicationPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

  Result := ApplicationPath;

end;

// Inline if
function IIF(cond: Boolean; t, f: Variant): Variant;
begin
  if cond then
    Result := t
  else
    Result := f;
end;

end.

