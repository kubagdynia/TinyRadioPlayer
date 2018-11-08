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
  Classes, SysUtils, Graphics, Consts, LCLType;

var
  ApplicationPath: string = EMPTY_STR;

  MainWindowHandle: LCLType.HWND;

  function GetApplicationPath: string;
  function IIF(cond: Boolean; t, f: Variant): Variant;
  function GetLanguageItem(const Item: string;
    const DefaultValue: string = EMPTY_STR): string;
  procedure ReloadLanguageItems();

  procedure LogException(const Text: string;
    const NameOfTheClass: string = EMPTY_STR; const NameOfTheMethod: string = EMPTY_STR;
    const E: Exception = nil);

  function MixingColors(const MyColor1, MyColor2: TColor;
    const Proportion1, Proportion2: integer): TColor;

implementation

uses
  Language, Log;

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

// Get language item
function GetLanguageItem(const Item: string; const DefaultValue: string): string;
begin
  Result := Language.TLanguage.Get(item, defaultValue);
end;

// Reload language items from the disk
procedure ReloadLanguageItems();
begin
  Language.TLanguage.ReloadLanguageItems();
end;

// Log exception
procedure LogException(const Text: string; const NameOfTheClass: string;
  const NameOfTheMethod: string; const E: Exception);
begin
  TLog.LogException(Text, NameOfTheClass, NameOfTheMethod, E);
end;

// Mixing two colors together
function MixingColors(const MyColor1, MyColor2: TColor;
  const Proportion1, Proportion2: integer): TColor;
var
  color: Longint;
  r1, g1, b1, r2, g2, b2: Byte;
begin
  color  := ColorToRGB(MyColor1);
  RedGreenBlue(color, r1, g1, b1);

  color  := ColorToRGB(MyColor2);
  RedGreenBlue(color, r2, g2, b2);

  r1 := Round((r1 * Proportion1 + r2 * Proportion2) / (Proportion1 + Proportion2));
  g1 := Round((g1 * Proportion1 + g2 * Proportion2) / (Proportion1 + Proportion2));
  b1 := Round((b1 * Proportion1 + b2 * Proportion2) / (Proportion1 + Proportion2));
  result := (r1 or (g1 shl 8) or (b1 shl 16));
end;

end.

