unit Skins;
{===============================================================================
File:                Skins.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Skins management

================================================================================}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics, Controls, Consts, LazMethodList, zipper;

type

  { TBitmapItem }
  PBitmapItem = ^TBitmapItem;
  TBitmapItem = record
    Bitmap: TBitmap;
    Name: string;
  end;

  { TSkinData }

  TSkinData = class
  private
    FSkinItemsHashmap: TStringList;
    FBitmapList: TStrings;
    procedure ClearBitmapList;
  public
    constructor Create(); overload;
    destructor Destroy; override;

    function AddItem(Name: string; Value: string): integer;
    function AddBitmap(Name: string; Bitmap: TBitmap): integer;
    function AddBitmap(Name: string; var AStream: TStream): integer;

    function GetStringItem(Name: string): string;
    function GetColorItem(Name: string): TColor;
    function GetBitmapItem(Name: string): TBitmap;

    procedure ClearItems;
  end;

  TOnSkinCustomStreamEvent = Procedure(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry) of object;
  TSkinLoadedEvent = Procedure(Sender : TObject; var ASkinData: TSkinData) of object;

  { TSkins }

  TSkins = class sealed (TObject)
  private
    class var FOnSkinCreateStream: TOnSkinCustomStreamEvent;
    class var FOnSkinDoneStream: TOnSkinCustomStreamEvent;

    class var FOnSkinLoaded: TSkinLoadedEvent;

    class var FSkinChangeEventList: TMethodList;

    class var FSkinData: TSkinData;

    class procedure Initialize();
    class procedure Finalize();

    class function GetSkinResources(const SkinName: string): Boolean;
    class function GetSkinFileName(const UseDefaultSkin: Boolean = false): string;
    class function GetSkinFilePath(const SkinName: string): string;

    class procedure SearchForAllSkinsFiles();
    procedure zipFileCloseInputStream(Sender: TObject; var AStream: TStream);
    class procedure zipFileCreateStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    class procedure zipFileDoneStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure zipFileOpenInputStream(Sender: TObject; var AStream: TStream);
  public
    class var SkinFiles: TStringList;
    class var CurrentSkinName: string;

    // static methods
    class procedure ChangeSkin(const SkinName: string);
    class procedure LoadSkin();
    class function GetBitmapItem(Name: string): TBitmap;
    class function GetStringItem(Name: string): string;
    class function GetColorItem(Name: string): TColor;

    // events
    class procedure RegisterSkinChangeEvent(const ANotification: TNotifyEvent);
    class procedure UnregisterSkinChangeEvent(const ANotification: TNotifyEvent);

    class property OnSkinCreateStream : TOnSkinCustomStreamEvent Read FOnSkinCreateStream Write FOnSkinCreateStream;
    class property OnSkinDoneStream : TOnSkinCustomStreamEvent Read FOnSkinDoneStream Write FOnSkinDoneStream;
    class property OnSkinLoaded : TSkinLoadedEvent Read FOnSkinLoaded Write FOnSkinLoaded;
  end;

implementation

uses
  laz2_DOM, laz2_XMLRead, Helpers, FileUtil, TRPSettings, TRPErrors;

{ TSkinData }

constructor TSkinData.Create();
begin
  FSkinItemsHashmap := TStringList.Create;
  FBitmapList := TStringList.Create;
end;

destructor TSkinData.Destroy;
begin
  ClearBitmapList;
  FreeAndNil(FBitmapList);

  if Assigned(FSkinItemsHashmap) then
    FSkinItemsHashmap.Free;

  inherited Destroy;
end;

function TSkinData.AddItem(Name: string; Value: string): integer;
begin
  Result := FSkinItemsHashmap.Add(Name + '=' + Value);
end;

function TSkinData.AddBitmap(Name: string; Bitmap: TBitmap): integer;
var
  bitmapItem: PBitmapItem;
  stream: TMemoryStream;
  bitmapToSave: TBitmap;
begin
  stream := TMemoryStream.Create;
  try
    Bitmap.SaveToStream(stream);

    stream.Position := 0;

    bitmapToSave := TBitmap.Create;
    bitmapToSave.LoadFromStream(stream);
  finally
    stream.Free;
  end;

  New(bitmapItem);

  bitmapItem^.Name := Name;
  bitmapItem^.Bitmap := bitmapToSave;

  Result := FBitmapList.AddObject(Name, TObject(bitmapItem));
end;

function TSkinData.AddBitmap(Name: string; var AStream: TStream): integer;
var
  picture: TPicture;
begin
  AStream.Position := 0;

  picture := TPicture.Create;
  try
    picture.LoadFromStream(AStream);
    Result := AddBitmap(Name, picture.Bitmap);
  finally
    picture.Free;
  end;

  AStream.Position := 0;
end;

function TSkinData.GetBitmapItem(Name: string): TBitmap;
var
  index: integer;
  bitmapItem: PBitmapItem;
  bitmap: TBitmap;
begin
  index := FBitmapList.IndexOf(Name);

  if index = EMPTY_INT then
    RaiseErrorMessage(ERR_CANT_LOAD_SKIN_ITEMS, ClassName, 'GetBitmapItem');

  bitmapItem := PBitmapItem(FBitmapList.Objects[index]);
  bitmap := bitmapItem^.Bitmap;

  Result := bitmap;
end;

function TSkinData.GetStringItem(Name: string): string;
begin
  if Name = EMPTY_STR then
    Result := EMPTY_STR
  else
  begin
    Result := FSkinItemsHashmap.Values[Name];
  end;
end;

function TSkinData.GetColorItem(Name: string): TColor;
begin
  Result := StrToColor(GetStringItem(Name));
end;

procedure TSkinData.ClearItems;
begin
  FSkinItemsHashmap.Clear;
  ClearBitmapList;
end;

procedure TSkinData.ClearBitmapList;
var
  i: integer;
begin
  if Assigned(FBitmapList) then
  begin
    for i := FBitmapList.Count - 1 downto 0 do
    begin
      FreeAndNil(PBitmapItem(FBitmapList.Objects[i])^.Bitmap);
      Dispose(PBitmapItem(FBitmapList.Objects[i]));
    end;
    FBitmapList.Clear;
  end;
end;

{ TSkins }

// Fired up when skins unit is initialization
class procedure TSkins.Initialize();
begin
  FSkinChangeEventList := TMethodList.Create;

  FSkinData := TSkinData.Create;

  SearchForAllSkinsFiles();
end;

// Fired up when skins unit is finalization
class procedure TSkins.Finalize();
begin
  // Free at the end
  if Assigned(SkinFiles) then
    SkinFiles.Free;

  if (Assigned(FSkinData)) then
    FSkinData.Free;

  if Assigned(FSkinChangeEventList) then
    FSkinChangeEventList.Free;
end;

// Get skin resources from the zip file
class function TSkins.GetSkinResources(const SkinName: string): Boolean;
var
  zipFile: TUnZipper;
  items: TStringList;
begin
  Result := false;

  CurrentSkinName := SkinName;

  items := TStringList.Create;
  try
    // Search icon
    items.Add('icoSearch.png');

    // Bottom function panel
    items.Add('btnPlay.png');
    items.Add('btnPause.png');
    items.Add('btnPrev.png');
    items.Add('btnStop.png');
    items.Add('btnNext.png');
    items.Add('btnRec.png');
    items.Add('btnOpen.png');

    // Station List Popup Menu
    items.Add('btnAdd.png');
    items.Add('btnEdit.png');
    items.Add('btnDelete.png');

    // xml
    items.Add('skin.xml');

    zipFile := TUnZipper.Create;
    try
      zipFile.FileName := GetSkinFilePath(SkinName);
      zipFile.OnOpenInputStream := @zipFileOpenInputStream;
      zipFile.OnCreateStream := @zipFileCreateStream;
      zipFile.OnDoneStream := @zipFileDoneStream;
      zipFile.OnCloseInputStream := @zipFileCloseInputStream;
      zipFile.UnZipFiles(items);
    finally
      FreeAndNil(zipFile);
    end;

  finally
    FreeAndNil(items);
  end;

  Result := true;
end;

// Get skin file name
class function TSkins.GetSkinFileName(const UseDefaultSkin: Boolean): string;
var
  skinName: string;
begin
  // try to get the skin name from the settings
  if UseDefaultSkin then
    skinName := DEFAULT_SKIN
  else
    skinName := TTRPSettings.GetValue('SelectedSkin', EMPTY_STR);

  if skinName = EMPTY_STR then
    skinName := DEFAULT_SKIN;

  TTRPSettings.SetValue('SelectedSkin', skinName);

  Result := skinName;
end;

// Get skin file path
class function TSkins.GetSkinFilePath(const SkinName: string): string;
begin
  Result := ConcatPaths([GetApplicationPath,
    SKINS_PATH, SkinName + SKIN_FILE_EXTENSION]);
end;

// Search the names of all skins files
class procedure TSkins.SearchForAllSkinsFiles();
var
  files: TStringList;
  i: Integer;
begin
  if not Assigned(SkinFiles) then
    SkinFiles := TStringList.Create
  else
    SkinFiles.Clear;

  files :=
    FindAllFiles(ConcatPaths([GetApplicationPath, SKINS_PATH]), SKIN_FILE_PATTERN, false);

  try
    for i := 0 to Pred(files.Count) do
      SkinFiles.Add(RemoveFileExtension(ExtractFileName(files[i])));
  finally
    files.Free;
  end;
end;

class procedure TSkins.zipFileCreateStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream := TMemoryStream.Create;

  if Assigned(OnSkinCreateStream) then
    OnSkinCreateStream(Sender, AStream, AItem);
end;

class procedure TSkins.zipFileDoneStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var
  xmlNode: TDOMNode;
  xmlDoc: TXMLDocument;
  i: integer;
begin

  if AItem.DiskFileName = 'skin.xml' then
  begin

    AStream.Position := 0;

    // Load xml file from stream
    ReadXMLFile(xmlDoc, AStream);
    try
      xmlNode := xmlDoc.FirstChild;
      if Assigned(xmlNode) then
      begin
        // Skin items
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
                  FSkinData.AddItem(
                    Item[i].Attributes.GetNamedItem('name').NodeValue,
                    Item[i].FirstChild.NodeValue);
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

  end
  else
  begin
    // Load bitmaps
    FSkinData.AddBitmap(RemoveFileExtension(AItem.DiskFileName), AStream);

    if Assigned(OnSkinDoneStream) then
      OnSkinDoneStream(Sender, AStream, AItem);
  end;

  Astream.Free;
end;

procedure TSkins.zipFileOpenInputStream(Sender: TObject; var AStream: TStream);
begin
  FSkinData.ClearItems;

end;

procedure TSkins.zipFileCloseInputStream(Sender: TObject; var AStream: TStream);
begin
  if Assigned(OnSkinLoaded) then
    OnSkinLoaded(Sender, FSkinData);
end;

// Change the skin and if the change is successful, call the skin change event
class procedure TSkins.ChangeSkin(const SkinName: string);
begin

  if (GetSkinResources(SkinName)) then
  begin
    TTRPSettings.SetValue('SelectedSkin', SkinName);
    FSkinChangeEventList.CallNotifyEvents(nil);
  end;
end;

class procedure TSkins.LoadSkin();
begin
  GetSkinResources(GetSkinFileName());
end;

class function TSkins.GetBitmapItem(Name: string): TBitmap;
begin
  Result := FSkinData.GetBitmapItem(Name);
end;

class function TSkins.GetStringItem(Name: string): string;
begin
  Result := FSkinData.GetStringItem(Name);
end;

class function TSkins.GetColorItem(Name: string): TColor;
begin
  Result := FSkinData.GetColorItem(Name);
end;

// Add a skin change event to the event list. Will be fired up when the skin changes.
class procedure TSkins.RegisterSkinChangeEvent(const ANotification: TNotifyEvent);
begin
  if not Assigned(FSkinChangeEventList) then
    Exit;

  FSkinChangeEventList.Add(TMethod(ANotification));
end;

// Remove a skin change event from the event list
class procedure TSkins.UnregisterSkinChangeEvent(
  const ANotification: TNotifyEvent);
begin
  if not Assigned(FSkinChangeEventList) then
    Exit;

  FSkinChangeEventList.Remove(TMethod(ANotification));
end;

initialization
  TSkins.Initialize();

finalization
  TSkins.Finalize();

end.

