unit ImportFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BCPanel, BCLabel, BCButton, Forms, Controls,
  Graphics, Dialogs, ActnList, ComCtrls, StdCtrls, BaseFormUnit,
  RadioPlayerTypes, Helpers, VirtualTrees, Consts;

type

  { TImportForm }

  TImportForm = class(TBaseForm)
    btnChooseAFile: TBCButton;
    btnImport: TBCButton;
    cbImportDictionaries: TCheckBox;
    cbImportStations: TCheckBox;
    edtFilePath: TEdit;
    gbImportStatus: TGroupBox;
    gbFile: TGroupBox;
    gbToImported: TGroupBox;
    lblImportStatus: TLabel;
    procedure btnChooseAFileClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure cbImportDictionariesChange(Sender: TObject);
    procedure cbImportStationsChange(Sender: TObject);
    procedure edtFilePathChange(Sender: TObject);
  private
    procedure OnImportStation(AStationInfo: TStationInfo; AImportDataStatus: TImportDataStatus);
    procedure ImportEnabled;

    procedure InitVstImportStatusList;
    procedure AddImportStatus(AName: string; AStatus: string; AImportDataStatus: TImportDataStatus);

    procedure VSTImportStatusListBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure VSTImportStatusListCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTImportStatusListFocusChanging(Sender: TBaseVirtualTree;
      OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure VSTImportStatusListFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VSTImportStatusListGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTImportStatusListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VSTImportStatusListHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
  protected
    procedure LoadLanguages; override;
    procedure LoadSkins; override;
  public
    VSTImportStatusList: TVirtualStringTree;
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AOpenMode: TOpenMode); overload;
    destructor Destroy; override;
  end;

var
  ImportForm: TImportForm;

implementation

uses
  Language, ExportImport, TRPErrors, TRPSettings;

{$R *.lfm}

{ TImportForm }

constructor TImportForm.Create(AOwner: TComponent);
begin
  Create(AOwner, omNormal);
end;

constructor TImportForm.Create(AOwner: TComponent; AOpenMode: TOpenMode);
begin
  inherited Create(AOwner, AOpenMode);

  InitVstImportStatusList;
end;

destructor TImportForm.Destroy;
begin
  inherited Destroy;
end;

procedure TImportForm.LoadLanguages;
begin
  inherited LoadLanguages;

  Self.Caption := GetLanguageItem('ImportData.WindowName', 'Import data');
  lblTitle.Caption := GetLanguageItem('ImportData.Title', 'Import data from json file');
  btnImport.Caption := GetLanguageItem('ImportData.Button.Import', 'Import');
  gbFile.Caption := GetLanguageItem('ImportData.GroupBox.File', 'Choose a file');
  gbImportStatus.Caption := GetLanguageItem('ImportData.GroupBox.ImportStatus', 'Import status');
  gbToImported.Caption := GetLanguageItem('ImportData.GroupBox.ToImported', 'Select data to import');
  cbImportStations.Caption := GetLanguageItem('ImportData.GroupBox.ToImported.Stations', 'Stations');
  cbImportDictionaries.Caption := GetLanguageItem('ImportData.GroupBox.ToImported.Dictionaries', 'Dictionaries')
end;

procedure TImportForm.LoadSkins;
begin
  inherited LoadSkins;
end;

procedure TImportForm.btnChooseAFileClick(Sender: TObject);
var
  openDialog: TOpenDialog;
begin
  openDialog := TOpenDialog.Create(Self);
  try
    openDialog.Title :=
      GetLanguageItem('ImportData.Dialog.Title', 'Select file to import');

    openDialog.InitialDir := GetCurrentDir;
    openDialog.Filter :=
      TTRPSettings.GetGroupValue('FileFilter', 'ExportData', 'JSON files|*.json|All|*.*', true);
    openDialog.DefaultExt :=
      TTRPSettings.GetGroupValue('DefaultFileExtension', 'ExportData', 'json', true);
    openDialog.FileName :=
      TTRPSettings.GetGroupValue('DefaultFileName', 'ExportData', 'trpData', true);
    openDialog.FilterIndex := 1;

    if openDialog.Execute then
    begin
      edtFilePath.Text := openDialog.FileName;
    end;
  finally
    openDialog.Free;
  end;

end;

procedure TImportForm.btnImportClick(Sender: TObject);
var
  exportImport: TExportImport;
begin
  if Trim(edtFilePath.Text) <> EmptyStr then
  begin
    VSTImportStatusList.Clear;

    exportImport := TExportImport.Create();
    try
      exportImport.OnImportStation := @OnImportStation;

      exportImport.ImportFromJsonFile(edtFilePath.Text,
        cbImportStations.Checked, cbImportDictionaries.Checked);

    finally
      exportImport.Free;
    end;
  end;

end;

procedure TImportForm.cbImportDictionariesChange(Sender: TObject);
begin
  ImportEnabled;
end;

procedure TImportForm.cbImportStationsChange(Sender: TObject);
begin
  ImportEnabled;
end;

procedure TImportForm.edtFilePathChange(Sender: TObject);
begin
  ImportEnabled;
end;

procedure TImportForm.OnImportStation(AStationInfo: TStationInfo; AImportDataStatus: TImportDataStatus);
begin
  if AImportDataStatus = idsStationAdded then
    AddImportStatus(AStationInfo.Name, 'Added', AImportDataStatus)
  else if AImportDataStatus = idsStationUpdated then
    AddImportStatus(AStationInfo.Name, 'Updated', AImportDataStatus);
end;

procedure TImportForm.ImportEnabled;
begin
  btnImport.Enabled := (cbImportStations.Checked or cbImportDictionaries.Checked) and (Trim(edtFilePath.Text) <> EmptyStr);
end;

procedure TImportForm.InitVstImportStatusList;
begin
  VSTImportStatusList := TVirtualStringTree.Create(gbImportStatus);

  VSTImportStatusList.Name := 'DictionaryTablesList';
  VSTImportStatusList.Parent := gbImportStatus;
  VSTImportStatusList.Align := alClient;
  VSTImportStatusList.DefaultNodeHeight := 20;
  VSTImportStatusList.SelectionCurveRadius := 0;
  VSTImportStatusList.TabOrder := 0;
  VSTImportStatusList.Width := 350;

  // Events
  VSTImportStatusList.OnBeforeItemErase := @VSTImportStatusListBeforeItemErase;
  VSTImportStatusList.OnCompareNodes := @VSTImportStatusListCompareNodes;
  VSTImportStatusList.OnFocusChanging := @VSTImportStatusListFocusChanging;
  VSTImportStatusList.OnFreeNode := @VSTImportStatusListFreeNode;
  VSTImportStatusList.OnGetText := @VSTImportStatusListGetText;
  VSTImportStatusList.OnGetNodeDataSize := @VSTImportStatusListGetNodeDataSize;
  VSTImportStatusList.OnHeaderClick:=@VSTImportStatusListHeaderClick;

  // Add colums
  VSTImportStatusList.Header.Columns.Add.Text := 'Name';
  VSTImportStatusList.Header.Columns[0].Width := VSTImportStatusList.Width div 2;

  VSTImportStatusList.Header.Columns.Add.Text := 'Status';
  VSTImportStatusList.Header.Columns[1].Width := VSTImportStatusList.Width div 2;

  // Visibility of header columns
  VSTImportStatusList.Header.Options := [hoVisible, hoColumnResize, hoHotTrack, hoOwnerDraw, hoShowHint, hoShowImages, hoShowSortGlyphs];
  VSTImportStatusList.Header.Height := 20;

  // Options
  VSTImportStatusList.TreeOptions.AnimationOptions := [toAnimatedToggle];

  VSTImportStatusList.TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes];
  VSTImportStatusList.TreeOptions.MiscOptions := [toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toWheelPanning];
  VSTImportStatusList.TreeOptions.PaintOptions := [toHideFocusRect, toThemeAware, toUseBlendedImages];
  VSTImportStatusList.TreeOptions.SelectionOptions := [toDisableDrawSelection, toExtendedFocus, toFullRowSelect, toMiddleClickSelect, toRightClickSelect]; // toCenterScrollIntoView
  VSTImportStatusList.TreeOptions.StringOptions := [toSaveCaptions];

  VSTImportStatusList.ScrollBarOptions.ScrollBars := ssVertical;

  // Colors of nodes
  VSTImportStatusList.Colors.FocusedSelectionColor := MixingColors(clHighlight, clWindow, 70, 20);
  VSTImportStatusList.Colors.FocusedSelectionBorderColor := MixingColors(clHighlight, clWindow, 70, 20);
  VSTImportStatusList.Colors.HotColor := clWhite;
  VSTImportStatusList.Colors.UnfocusedSelectionColor := MixingColors(clHighlight, clWindow, 60, 40);
  VSTImportStatusList.Colors.UnfocusedSelectionBorderColor := MixingColors(clHighlight, clWindow, 60, 40);

  // Set the sort order
  VSTImportStatusList.Header.SortDirection := sdAscending;
  VSTImportStatusList.Header.SortColumn := 0;

  VSTImportStatusList.Show;

end;

procedure TImportForm.AddImportStatus(AName: string; AStatus: string; AImportDataStatus: TImportDataStatus);
var
  node: PVirtualNode;
  data: PImportDataNodeRec;
begin
  VSTImportStatusList.BeginUpdate;

  node := VSTImportStatusList.AddChild(nil);
  data := VSTImportStatusList.GetNodeData(node);

  data^.idnd := TImportDataNodeData.Create(AName, AStatus, AImportDataStatus);

  VSTImportStatusList.EndUpdate;
end;

// Called when the background of a node is about to be erased
procedure TImportForm.VSTImportStatusListBeforeItemErase(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  const ItemRect: TRect; var ItemColor: TColor;
  var EraseAction: TItemEraseAction);
begin
  // Coloring every second line
  if (Odd(Node^.Index)) and (not Sender.Selected[Node]) then
  begin
    ItemColor := GridLineColor;
    EraseAction := eaColor;
  end;
end;

// Sort and search support event
procedure TImportForm.VSTImportStatusListCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  data1: PImportDataNodeRec;
  data2: PImportDataNodeRec;
begin
  data1 := Sender.GetNodeData(Node1);
  data2 := Sender.GetNodeData(Node2);

  if (not Assigned(data1) or (data1^.idnd = nil)) or
     (not Assigned(data2) or (data2^.idnd = nil)) then
    Result := 0
  else begin
    case Column of
      0: Result := CompareText(data1^.idnd.Name, data2^.idnd.Name);
      1: Result := CompareText(data1^.idnd.Status, data2^.idnd.Status);
    end;
  end;
end;

// Triggered after focus Changing
procedure TImportForm.VSTImportStatusListFocusChanging(
  Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
  NewColumn: TColumnIndex; var Allowed: Boolean);
begin

end;

// Triggered when a node is about to be freed
procedure TImportForm.VSTImportStatusListFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  data: PImportDataNodeRec;
begin
  data := Sender.GetNodeData(Node);

  if not Assigned(data) then
    Exit;

  if data^.idnd <> nil then
    data^.idnd.Free;

  Finalize(data^);
end;

// This will be called every time when we needs to know about the text of a
// spectfic node and column
procedure TImportForm.VSTImportStatusListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  data: PImportDataNodeRec;
begin
  Node^.States := Node^.States + [vsOnFreeNodeCallRequired];
  // A handler for the OnGetText event is always needed as it provides
  // the tree with the string data to display.
  // Note that we are always using WideString
  data := Sender.GetNodeData(Node);

  if Assigned(data) and (data^.idnd <> nil) then
  begin
    case Column of
      0: CellText := data^.idnd.Name;
      1: CellText := data^.idnd.Status;
    end;
  end;
end;

// Triggered when access to a node's data happens the first time but the actual
// data size is not set yet.
procedure TImportForm.VSTImportStatusListGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TImportDataNodeRec);
end;

// Triggered when user clicks on a header button. Set the current SortColumn
// and SortDirection
procedure TImportForm.VSTImportStatusListHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  // We determine the sort direction but only if click on the same column
  if (Sender.SortColumn = HitInfo.Column) then
    if Sender.SortDirection = sdAscending then
      Sender.SortDirection := sdDescending
    else
      Sender.SortDirection := sdAscending;

  // show SortGlyph
  Sender.SortColumn := HitInfo.Column;

  // sorting
  Sender.Treeview.SortTree(HitInfo.Column, Sender.SortDirection, True);
end;



end.

