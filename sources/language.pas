unit Language;
{===============================================================================
File:                Language.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Language management

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Consts;

type

  TLanguage = class sealed (TObject)
  private
    // class variables and methods belongs to the class, not to the instance
    class var LanguageHashmap: TStringList;
    class var LanguageInfoLanguage: string;
    class var LanguageInfoAuthor: string;
    class var LanguageInfoVersion: string;
    class var LanguageInfoDate: string;
    class var LanguageInfoUrl: string;
    class var LanguageInfoEmail: string;
    class var LangName: string;

    class procedure GetLanguageResources(LanguageName: string);
    class function GetLanguageFileName(UseDefaultLanguage: Boolean = false): string;
    class function GetLanguageFilePath(LanguageName: string): string;
  public
    // static methods
    class function Get(const Item: string;
      const DefaultValue: string = EMPTY_STR): string;
    class procedure Reload();
    class function GetOSLanguage(): string;
  end;

implementation

uses
  laz2_DOM, laz2_XMLRead, Helpers, gettext;

//  Get language item or default value
class function TLanguage.Get(const Item: string; const DefaultValue: string): string;
begin
  // If there is no items list just create a list and load data
  if not Assigned(LanguageHashmap) then
  begin
    LanguageHashmap := TStringList.Create;
    GetLanguageResources(GetLanguageFileName());
  end;

  // Get language item
  Result := LanguageHashmap.Values[Item];

  // Return default value if language item is empty
  if Result = EMPTY_STR then
    Result := DefaultValue;
end;

// Reload language data from the disk
class procedure TLanguage.Reload();
begin
  if (LangName = EMPTY_STR) then
    Exit;

  // If there is no items list, create it, otherwise just clear it
  if not Assigned(LanguageHashmap) then
    LanguageHashmap := TStringList.Create
  else
    LanguageHashmap.Clear;

  GetLanguageResources(LangName);
end;

// Get language resources from the xml file
class procedure TLanguage.GetLanguageResources(LanguageName: string);
var
  xmlNode: TDOMNode;
  xmlDoc: TXMLDocument;
  languagePath: string;
  i: Integer;
begin
  try
    LangName := LanguageName;

    languagePath := GetLanguageFilePath(LanguageName);

    // If the file does not exist, set it to the default
    if not FileExists(languagePath) then
    begin
      LangName := GetLanguageFileName(True);
      languagePath := GetLanguageFilePath(LangName);
    end;

    // Read in xml file from disk
    ReadXMLFile(xmlDoc, languagePath);

    xmlNode := xmlDoc.DocumentElement.FindNode('Items');
    if Assigned(xmlNode) then
    begin
      // Language Info Section
      if xmlNode.Attributes.GetNamedItem('language') <> nil then
        LanguageInfoLanguage := xmlNode.Attributes.GetNamedItem('language').NodeValue;

      if xmlNode.Attributes.GetNamedItem('author') <> nil then
        LanguageInfoAuthor := xmlNode.Attributes.GetNamedItem('author').NodeValue;

      if xmlNode.Attributes.GetNamedItem('version') <> nil then
        LanguageInfoVersion := xmlNode.Attributes.GetNamedItem('version').NodeValue;

      if xmlNode.Attributes.GetNamedItem('date') <> nil then
        LanguageInfoDate := xmlNode.Attributes.GetNamedItem('date').NodeValue;

      if xmlNode.Attributes.GetNamedItem('url') <> nil then
        LanguageInfoUrl := xmlNode.Attributes.GetNamedItem('url').NodeValue;

      if xmlNode.Attributes.GetNamedItem('email') <> nil then
        LanguageInfoEmail := xmlNode.Attributes.GetNamedItem('email').NodeValue;

      // Language Items Section
      with xmlNode.ChildNodes do
      begin
        try
          for i := 0 to (Count - 1) do
          begin
            if Item[i].NodeName = 'Item' then
            begin
              if (Item[i].Attributes.GetNamedItem('name') <> nil) and
                (Item[i].FirstChild <> nil) then
              begin
                LanguageHashmap.Add(
                  Item[i].Attributes.GetNamedItem('name').NodeValue +
                    '=' + Item[i].FirstChild.NodeValue);
              end;
            end;
          end;
        finally
          Free;
        end;
      end;

    end;

  finally
    // Finally, free the xml document
    xmlDoc.Free;
  end;
end;

// Get language file name
class function TLanguage.GetLanguageFileName(UseDefaultLanguage: Boolean = false): string;
begin
  if UseDefaultLanguage then
    Result := LANGUAGE_FILE_PREFIX + DEFAULT_LANGUAGE
  else
    Result := LANGUAGE_FILE_PREFIX +
      IIF(USE_OS_LANGUAGE_INSTEAD_DEFAULT, GetOSLanguage(), DEFAULT_LANGUAGE);
end;

class function TLanguage.GetLanguageFilePath(LanguageName: string): string;
begin
  Result := ConcatPaths([GetApplicationPath,
    LANGUAGE_PATH, LanguageName + LANGUAGE_FILE_EXTENSION]);
end;

// Returns the system language
class function TLanguage.GetOSLanguage(): string;
var
  l, fbl: string;
begin
  GetLanguageIDs(l, fbl);
  Result := fbl;
end;

initialization

finalization
  // Free at the end
  TLanguage.LanguageHashmap.Free;

end.

