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
  Classes, SysUtils, Consts, LazMethodList;

type

  TLanguage = class sealed (TObject)
  private
    // class variables and methods belongs to the class, not to the instance
    class var FLanguageHashmap: TStringList;
    class var FLanguageInfoLanguage: string;
    class var FLanguageInfoAuthor: string;
    class var FLanguageInfoVersion: string;
    class var FLanguageInfoDate: string;
    class var FLanguageInfoUrl: string;
    class var FLanguageInfoEmail: string;

    class var FLanguageChangeEventList: TMethodList;

    class procedure Initialize();
    class procedure Finalize();

    class function GetLanguageResources(const LanguageName: string): Boolean;
    class function GetLanguageFileName(const UseDefaultLanguage: Boolean = false): string;
    class function GetLanguageFilePath(const LanguageName: string): string;

    class procedure SearchAllLanguageFiles();
  public
    class var LanguageFiles: TStringList;
    class var CurrentLangName: string;

    // static methods
    class function Get(const Item: string;
      const DefaultValue: string = EMPTY_STR): string;
    class procedure ReloadLanguageItems();
    class procedure ChangeLanguage(const LanguageName: string);

    class function GetOSLanguage(): string;

    // events
    class procedure RegisterLanguageChangeEvent(const ANotification: TNotifyEvent);
    class procedure UnregisterLanguageChangeEvent(const ANotification: TNotifyEvent);
  end;

implementation

uses
  laz2_DOM, laz2_XMLRead, Helpers, gettext, FileUtil, TRPSettings;

//  Get language item or default value
class function TLanguage.Get(const Item: string; const DefaultValue: string): string;
begin
  // Get language item
  Result := FLanguageHashmap.Values[Item];

  // Return default value if language item is empty
  if Result = EMPTY_STR then
    Result := DefaultValue;
end;

// Reload language data from the disk
class procedure TLanguage.ReloadLanguageItems();
begin
  if (CurrentLangName = EMPTY_STR) then
    Exit;

  GetLanguageResources(CurrentLangName);
end;

// Fired up when language unit is initialization
class procedure TLanguage.Initialize();
begin
  FLanguageChangeEventList := TMethodList.Create;

  SearchAllLanguageFiles();
  GetLanguageResources(GetLanguageFileName());
end;

// Fired up when language unit is finalization
class procedure TLanguage.Finalize();
begin
  // Free at the end
  if Assigned(FLanguageChangeEventList) then
    FLanguageChangeEventList.Free;

  if Assigned(FLanguageHashmap) then
    FLanguageHashmap.Free;

  if Assigned(LanguageFiles) then
    LanguageFiles.Free;
end;

// Get language resources from the xml file
class function TLanguage.GetLanguageResources(const LanguageName: string): Boolean;
var
  xmlNode: TDOMNode;
  xmlDoc: TXMLDocument;
  languagePath: string;
  i: Integer;
begin
  Result := false;

  try
    CurrentLangName := LanguageName;

    languagePath := GetLanguageFilePath(LanguageName);

    // If the file does not exist, set it to the default
    if not FileExists(languagePath) then
    begin
      CurrentLangName := GetLanguageFileName(True);
      languagePath := GetLanguageFilePath(CurrentLangName);
    end;

    // Read in xml file from disk
    ReadXMLFile(xmlDoc, languagePath);

    xmlNode := xmlDoc.DocumentElement.FindNode('Items');
    if Assigned(xmlNode) then
    begin
      // Language Info Section
      if xmlNode.Attributes.GetNamedItem('language') <> nil then
        FLanguageInfoLanguage := xmlNode.Attributes.GetNamedItem('language').NodeValue;

      if xmlNode.Attributes.GetNamedItem('author') <> nil then
        FLanguageInfoAuthor := xmlNode.Attributes.GetNamedItem('author').NodeValue;

      if xmlNode.Attributes.GetNamedItem('version') <> nil then
        FLanguageInfoVersion := xmlNode.Attributes.GetNamedItem('version').NodeValue;

      if xmlNode.Attributes.GetNamedItem('date') <> nil then
        FLanguageInfoDate := xmlNode.Attributes.GetNamedItem('date').NodeValue;

      if xmlNode.Attributes.GetNamedItem('url') <> nil then
        FLanguageInfoUrl := xmlNode.Attributes.GetNamedItem('url').NodeValue;

      if xmlNode.Attributes.GetNamedItem('email') <> nil then
        FLanguageInfoEmail := xmlNode.Attributes.GetNamedItem('email').NodeValue;

       // If there is no items list, create it, otherwise just clear it
      if not Assigned(FLanguageHashmap) then
        FLanguageHashmap := TStringList.Create
      else
        FLanguageHashmap.Clear;

      // Language Items Section
      with xmlNode.ChildNodes do
      begin
        try
          for i := 0 to Count - 1 do
          begin
            if Item[i].NodeName = 'Item' then
            begin
              if (Item[i].Attributes.GetNamedItem('name') <> nil) and
                (Item[i].FirstChild <> nil) then
              begin
                FLanguageHashmap.Add(
                  Item[i].Attributes.GetNamedItem('name').NodeValue +
                    '=' + Item[i].FirstChild.NodeValue);
              end;
            end;
          end;
        finally
          Free;
        end;
      end;
      Result := true;
    end;

  finally
    // Finally, free the xml document
    xmlDoc.Free;
  end;
end;

// Get language file name
class function TLanguage.GetLanguageFileName(const UseDefaultLanguage: Boolean = false): string;
var
  languageName: string;
begin
  // try to get the language name from the settings
  if UseDefaultLanguage then
    languageName := LANGUAGE_FILE_PREFIX + DEFAULT_LANGUAGE
  else
    languageName := TTRPSettings.GetValue('SelectedLanguage', EMPTY_STR);

  if languageName = EMPTY_STR then
    languageName := LANGUAGE_FILE_PREFIX +
      IIF(USE_OS_LANGUAGE_INSTEAD_DEFAULT, GetOSLanguage(), DEFAULT_LANGUAGE);

  TTRPSettings.SetValue('SelectedLanguage', languageName);

  Result := languageName;
end;

// Get language file path
class function TLanguage.GetLanguageFilePath(const LanguageName: string): string;
begin
  Result := ConcatPaths([GetApplicationPath,
    LANGUAGE_PATH, LanguageName + LANGUAGE_FILE_EXTENSION]);
end;

// Search the names of all loanguage files
class procedure TLanguage.SearchAllLanguageFiles();
var
  files: TStringList;
  i: Integer;
begin
  if not Assigned(LanguageFiles) then
    LanguageFiles := TStringList.Create
  else
    LanguageFiles.Clear;

  files :=
    FindAllFiles(ConcatPaths([GetApplicationPath, LANGUAGE_PATH]), LANGUAGE_FILE_PATTERN, false);

  try
    for i := 0 to Pred(files.Count) do
      LanguageFiles.Add(ChangeFileExt(ExtractFileName(files[i]), EMPTY_STR));
  finally
    files.Free;
  end;
end;

// Change the language and if the change is successful, call the language change event
class procedure TLanguage.ChangeLanguage(const LanguageName: string);
begin
  if (GetLanguageResources(LanguageName)) then
  begin
    TTRPSettings.SetValue('SelectedLanguage', LanguageName);
    FLanguageChangeEventList.CallNotifyEvents(nil);
  end;
end;

// Returns the system language
class function TLanguage.GetOSLanguage(): string;
var
  l, fbl: string;
begin
  GetLanguageIDs(l, fbl);
  Result := fbl;
end;

// Add a language change event to the event list. Will be fired up when the language changes.
class procedure TLanguage.RegisterLanguageChangeEvent(const ANotification: TNotifyEvent);
begin
  if not Assigned(FLanguageChangeEventList) then
    Exit;

  FLanguageChangeEventList.Add(TMethod(ANotification));
end;

// Remove a language change event from the event list
class procedure TLanguage.UnregisterLanguageChangeEvent(const ANotification: TNotifyEvent);
begin
  if not Assigned(FLanguageChangeEventList) then
    Exit;

  FLanguageChangeEventList.Remove(TMethod(ANotification));
end;

initialization
  TLanguage.Initialize();

finalization
  TLanguage.Finalize();

end.

