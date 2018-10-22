unit TRPSettings;
{===============================================================================
File:                Log.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Application settings

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Consts, RadioPlayerTypes;

type

  { TTRPSettings }

  TTRPSettings = class sealed (TObject)
  private
    // class variables and methods belongs to the class, not to the instance

    class var SettingsHashmap: TStringList;
    // if its true, the settings should be saved before exit
    class var IsUpdated: Boolean;

    class procedure CreateDefaultSettings;
    class function GetSettingsFilePath(): string;
    class procedure Initialize();
    class procedure Finalize();
  public
    // static methods
    class function GetValue(const Item: string;
      const DefaultValue: string = EMPTY_STR; const AddIfNoExists: boolean = false): string;
    class function GetValue(const Item: string;
      const DefaultValue: integer = EMPTY_INT; const AddIfNoExists: boolean = false): integer;
    class function SetValue(const Item: string; const Value: string): ErrorId;
    class function SetValue(const Item: string; const Value: integer): ErrorId;
    class procedure SaveSettings;
    class procedure LoadSettings;
  end;

implementation

uses
  laz2_DOM, laz2_XMLRead, laz2_XMLWrite, TRPErrors;

class procedure TTRPSettings.CreateDefaultSettings;
begin
  SetValue('Volume', 100);
  SaveSettings();
end;

class function TTRPSettings.GetSettingsFilePath(): string;
begin
  Result := ConcatPaths([IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))), SETTINGS_FILE]);
end;

// Fired up when settings unit is initialization
class procedure TTRPSettings.Initialize();
begin
  IsUpdated := false;
  LoadSettings;
end;

// Fired up when settings unit is finalization
class procedure TTRPSettings.Finalize();
begin
  if Assigned(SettingsHashmap) then
  begin
    // if the settings have beed changed, save them to a file
    if IsUpdated then
      SaveSettings;

    // and free at the end
    SettingsHashmap.Free;
  end;
end;

class function TTRPSettings.GetValue(const Item: string;
  const DefaultValue: string; const AddIfNoExists: boolean = false): string;
begin
  // Get an item
  Result := SettingsHashmap.Values[Item];

  // Return default value if item is empty
  if Result = EMPTY_STR then
  begin
    if AddIfNoExists then
      SetValue(Item, DefaultValue);

    Result := DefaultValue;
  end;
end;

class function TTRPSettings.GetValue(const Item: string;
  const DefaultValue: integer; const AddIfNoExists: boolean = false): integer;
begin
  Result := StrToInt(GetValue(Item, IntToStr(DefaultValue), AddIfNoExists));
end;

class function TTRPSettings.SetValue(const Item: string; const Value: string): ErrorId;
var
  existingItemValue: string;
begin
  existingItemValue := GetValue(Item, EMPTY_STR);

  if (existingItemValue <> Value) then
  begin
    SettingsHashmap.Values[Item] := Value;
    IsUpdated := true;
  end;

  Result := ERR_OK;

end;

class function TTRPSettings.SetValue(const Item: string; const Value: integer): ErrorId;
begin
  Result := SetValue(Item, IntToStr(Value));
end;

// Saving data to a file
class procedure TTRPSettings.SaveSettings;
var
  i: integer;
  xmlDoc: TXMLDocument;
  xmlRootNode, xmlElementNode: TDOMNode;
  itemsCount: integer;
begin

  // Create a xml document
  xmlDoc := TXMLDocument.Create;
  try
    // Create a root node
    xmlRootNode := xmlDoc.CreateElement('Settings');
    xmlDoc.Appendchild(xmlRootNode);

    // Create items
    itemsCount := SettingsHashmap.Count;
    if itemsCount > 0 then
      for i := 0 to itemsCount - 1 do
      begin
        // Add an item
        xmlElementNode := xmlDoc.CreateElement('Item');
        // Add attribute
        TDOMElement(xmlElementNode).SetAttribute('name', SettingsHashmap.Names[i]);
        xmlRootNode.Appendchild(xmlElementNode);
        // Add value
        xmlElementNode.AppendChild(xmlDoc.CreateTextNode(
          SettingsHashmap.ValueFromIndex[i]));
      end;

    // Save XML file
    WriteXMLFile(xmlDoc, GetSettingsFilePath());

  finally
    // Finally, free the xml document
    xmlDoc.Free;
  end;

end;

// Load settings from a file
class procedure TTRPSettings.LoadSettings;
var
  xmlNode: TDOMNode;
  xmlDoc: TXMLDocument;
  i: Integer;
  settingsPath: string;
begin
  if not Assigned(SettingsHashmap) then
    SettingsHashmap := TStringList.Create;

  settingsPath := GetSettingsFilePath();
  if not FileExists(settingsPath) then
  begin
    CreateDefaultSettings();
    Exit;
  end;

  // Read in xml file from disk
  ReadXMLFile(xmlDoc, settingsPath);
  try
    xmlNode := xmlDoc.FirstChild;
    if Assigned(xmlNode) then
    begin
      // Items section
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
                 SettingsHashmap.Add(
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

  IsUpdated := false;
end;

initialization
  TTRPSettings.Initialize();

finalization
  TTRPSettings.Finalize();

end.

