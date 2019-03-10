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
  Classes, SysUtils, Consts, RadioPlayerTypes, fgl,
  laz2_DOM;

const
  NameAttribute = 'name';
  ItemNode = 'Item';
  GroupNode = 'Group';

type

  TDictionaryOfStringList = specialize TFPGMap<string, TStringList>;

  { TTRPSettings }

  TTRPSettings = class sealed (TObject)
  private
    // class variables and methods belongs to the class, not to the instance

    class var SettingsHashmap: TStringList;

    class var SettingsGroups: TDictionaryOfStringList;

    // if its true, the settings should be saved before exit
    class var IsUpdated: Boolean;

    class procedure CreateDefaultSettings;
    class function GetSettingsFilePath(): string;
    class procedure Initialize();
    class procedure Finalize();

    class procedure AddSettingsItem(Node: TDOMNode; Target: TStringList);
    class procedure AddGroupSettings(Node: TDOMNode);
  public
    // Get value
    class function GetValue(const Item: string;
      const DefaultValue: string = EMPTY_STR; const AddIfNoExists: boolean = false): string;
    class function GetValue(const Item: string;
      const DefaultValue: integer = EMPTY_INT; const AddIfNoExists: boolean = false): integer;
    class function GetValue(const Item: string;
      const DefaultValue: single = EMPTY_INT; const AddIfNoExists: boolean = false): single;
    class function GetValue(const Item: string;
      const DefaultValue: boolean = false; const AddIfNoExists: boolean = false): boolean;

    // Get group value
    class function GetGroupValue(const Item: string; const GroupName: string;
      const DefaultValue: string = EMPTY_STR; const AddIfNoExists: boolean = false): string;
    class function GetGroupValue(const Item: string; const GroupName: string;
      const DefaultValue: integer = EMPTY_INT; const AddIfNoExists: boolean = false): integer;
    class function GetGroupValue(const Item: string; const GroupName: string;
      const DefaultValue: single = EMPTY_INT; const AddIfNoExists: boolean = false): single;
    class function GetGroupValue(const Item: string; const GroupName: string;
      const DefaultValue: boolean = false; const AddIfNoExists: boolean = false): boolean;

    // Set value
    class function SetValue(const Item: string; const Value: string): ErrorId;
    class function SetValue(const Item: string; const Value: integer): ErrorId;
    class function SetValue(const Item: string; const Value: single): ErrorId;
    class function SetValue(const Item: string; const Value: boolean): ErrorId;

    // Set group value
    class function SetGroupValue(const Item: string; const GroupName: string;
      const Value: string): ErrorId;
    class function SetGroupValue(const Item: string; const GroupName: string;
      const Value: integer): ErrorId;
    class function SetGroupValue(const Item: string; const GroupName: string;
      const Value: single): ErrorId;
    class function SetGroupValue(const Item: string; const GroupName: string;
      const Value: boolean): ErrorId;

    class procedure SaveSettings;
    class procedure LoadSettings;
  end;

implementation

uses
  laz2_XMLRead, laz2_XMLWrite, TRPErrors, Helpers;

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
var
  i: integer;
  groupCount: integer;
begin
  if Assigned(SettingsHashmap) then
  begin
    // if the settings have beed changed, save them to a file
    if IsUpdated then
      SaveSettings;

    // and free at the end
    SettingsHashmap.Free;
  end;

  if Assigned(SettingsGroups) then
  begin
    groupCount := SettingsGroups.Count - 1;
    for i := 0 to groupCount do
      SettingsGroups.Data[i].Free;

    FreeAndNil(SettingsGroups);
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

class function TTRPSettings.GetValue(const Item: string;
  const DefaultValue: single; const AddIfNoExists: boolean): single;
begin
  Result := StrToFloat(GetValue(Item, FloatToStr(DefaultValue), AddIfNoExists));
end;

class function TTRPSettings.GetValue(const Item: string;
  const DefaultValue: boolean; const AddIfNoExists: boolean): boolean;
begin
  Result := FullStrToBool(GetValue(Item, BoolToFullStr(DefaultValue), AddIfNoExists));
end;

class function TTRPSettings.GetGroupValue(const Item: string;
  const GroupName: string; const DefaultValue: string;
  const AddIfNoExists: boolean): string;
var
  groupIndex: integer;
begin
  if (Trim(GroupName) = EMPTY_STR) or (Trim(Item) = EMPTY_STR) then
  begin
    if DefaultValue = EMPTY_STR then
      Result := EMPTY_STR
    else
      Result := DefaultValue;

    Exit;
  end;

  groupIndex := SettingsGroups.IndexOf(GroupName);

  if (groupIndex = EMPTY_INT) then
  begin
    if AddIfNoExists then
      SetGroupValue(Item, GroupName, DefaultValue);

    Result := DefaultValue;
    Exit;
  end;

  // Get an item
  Result := SettingsGroups[GroupName].Values[Item];

  // Return default value if item is empty
  if Result = EMPTY_STR then
  begin
    if AddIfNoExists then
      SetGroupValue(Item, GroupName, DefaultValue);

    Result := DefaultValue;
  end;

end;

class function TTRPSettings.GetGroupValue(const Item: string;
  const GroupName: string; const DefaultValue: integer;
  const AddIfNoExists: boolean): integer;
begin
  Result := StrToInt(GetGroupValue(Item, GroupName, IntToStr(DefaultValue), AddIfNoExists));
end;

class function TTRPSettings.GetGroupValue(const Item: string;
  const GroupName: string; const DefaultValue: single;
  const AddIfNoExists: boolean): single;
begin
  Result := StrToFloat(GetGroupValue(Item, GroupName, FloatToStr(DefaultValue), AddIfNoExists));
end;

class function TTRPSettings.GetGroupValue(const Item: string;
  const GroupName: string; const DefaultValue: boolean;
  const AddIfNoExists: boolean): boolean;
begin
  Result := FullStrToBool(GetGroupValue(Item, GroupName, BoolToFullStr(DefaultValue), AddIfNoExists));
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

class function TTRPSettings.SetValue(const Item: string; const Value: single): ErrorId;
begin
  Result := SetValue(Item, FloatToStr(Value));
end;

class function TTRPSettings.SetValue(const Item: string; const Value: boolean): ErrorId;
begin
  Result := SetValue(Item, BoolToFullStr(Value));
end;

class function TTRPSettings.SetGroupValue(const Item: string;
  const GroupName: string; const Value: string): ErrorId;
var
  err: ErrorId;
  groupIndex: integer;
  hashmap: TStringList;
  existingItemValue: string;
begin
  err := ERR_OK;

  try
    if (Trim(GroupName) = EMPTY_STR) or (Trim(Item) = EMPTY_STR) or (Trim(Value) = EMPTY_STR) then
    begin
      Result := ERR_SET_GROUP_VALUE;
      Exit;
    end;

    groupIndex := SettingsGroups.IndexOf(GroupName);

    if groupIndex = EMPTY_INT then
    begin
      // If the group does not exist yet, create it and add the item
      hashmap := TStringList.Create;
      hashmap.Values[Item] := Value;
      SettingsGroups.Add(groupName, hashmap);
      IsUpdated := true;
    end
    else
    begin
      existingItemValue := SettingsGroups[GroupName].Values[Item];

      if (existingItemValue <> Value) then
      begin
        SettingsGroups[GroupName].Values[Item] := Value;
        IsUpdated := true;
      end;
    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'SetGroupValue', E);
        err := ERR_SET_GROUP_VALUE;
      end;
  end;

  Result := err;
end;

class function TTRPSettings.SetGroupValue(const Item: string;
  const GroupName: string; const Value: integer): ErrorId;
begin
  Result := SetGroupValue(Item, GroupName, IntToStr(Value));
end;

class function TTRPSettings.SetGroupValue(const Item: string;
  const GroupName: string; const Value: single): ErrorId;
begin
  Result := SetGroupValue(Item, GroupName, FloatToStr(Value));
end;

class function TTRPSettings.SetGroupValue(const Item: string;
  const GroupName: string; const Value: boolean): ErrorId;
begin
  Result := SetGroupValue(Item, GroupName, BoolToFullStr(Value));
end;

// Saving data to a file
class procedure TTRPSettings.SaveSettings;
var
  i, j: integer;
  xmlDoc: TXMLDocument;
  xmlRootNode, xmlElementNode, xmlElementNode2: TDOMNode;
  itemsCount: integer;
  groupsCount: integer;
  groupName: string;
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
        xmlElementNode := xmlDoc.CreateElement(ItemNode);
        // Add attribute
        TDOMElement(xmlElementNode).SetAttribute(NameAttribute, SettingsHashmap.Names[i]);
        xmlRootNode.Appendchild(xmlElementNode);
        // Add value
        xmlElementNode.AppendChild(xmlDoc.CreateTextNode(
          SettingsHashmap.ValueFromIndex[i]));
      end;

    // Create a group of items
    groupsCount := SettingsGroups.Count;
    if (Assigned(SettingsGroups)) and (groupsCount > 0) then
    begin
      for i := 0 to groupsCount - 1 do
      begin
        groupName := SettingsGroups.Keys[i];

        // Create group
        xmlElementNode := xmlDoc.CreateElement(GroupNode);
        TDOMElement(xmlElementNode).SetAttribute(NameAttribute, groupName);
        xmlRootNode.Appendchild(xmlElementNode);

        // Create items
        itemsCount := SettingsGroups[groupName].Count;
        if itemsCount > 0 then
          for j := 0 to itemsCount - 1 do
          begin
            // Add an item
            xmlElementNode2 := xmlDoc.CreateElement(ItemNode);
            // Add attribute
            TDOMElement(xmlElementNode2).SetAttribute(NameAttribute, SettingsGroups[groupName].Names[j]);
            xmlElementNode.Appendchild(xmlElementNode2);
            // Add value
            xmlElementNode2.AppendChild(xmlDoc.CreateTextNode(
              SettingsGroups[groupName].ValueFromIndex[j]));
          end;
      end;
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

  if not Assigned(SettingsGroups) then
    SettingsGroups := TDictionaryOfStringList.Create;

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
            if Item[i].NodeName = ItemNode then
               AddSettingsItem(Item[i], SettingsHashmap);

            if Item[i].NodeName = GroupNode then
              AddGroupSettings(Item[i]);

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

class procedure TTRPSettings.AddSettingsItem(Node: TDOMNode; Target: TStringList);
begin
  if (Node = nil) or (Node.FirstChild = nil) then Exit;

  if Node.Attributes.GetNamedItem(NameAttribute) <> nil then
  begin
    Target.Values[Node.Attributes.GetNamedItem(NameAttribute).NodeValue] := Node.FirstChild.NodeValue;
  end;
end;

class procedure TTRPSettings.AddGroupSettings(Node: TDOMNode);
var
  i: integer;
  groupName: string;
  groupIndex: integer;
  hashmap: TStringList;
begin
  groupIndex := SettingsGroups.IndexOf(groupName);

  if groupIndex <> EMPTY_INT then Exit;

  groupName := Node.Attributes.GetNamedItem(NameAttribute).NodeValue;

  if Node.ChildNodes.Count > 0 then
  begin
    hashmap := TStringList.Create;

    with Node.ChildNodes do
    begin

      try
        for i := 0 to (Count - 1) do
        begin
          if Item[i].NodeName = ItemNode then
            AddSettingsItem(Item[i], hashmap);
        end;
      finally
        Free;
      end;

    end;

    SettingsGroups.Add(groupName, hashmap);
  end;

end;

initialization
  TTRPSettings.Initialize();

finalization
  TTRPSettings.Finalize();

end.

