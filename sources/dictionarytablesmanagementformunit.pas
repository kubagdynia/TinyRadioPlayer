unit DictionaryTablesManagementFormUnit;
{===============================================================================
File:                StationDetailFormUnit.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Dictionary management

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BCPanel, BCLabel, BCButton, Forms, Controls,
  Graphics, Dialogs, ActnList, StdCtrls, BaseFormUnit, VirtualTrees,
  RadioPlayerTypes;

type

  { TDictionaryTablesManagementForm }

  TDictionaryTablesManagementForm = class(TBaseForm)
    AddDetailAction: TAction;
    btnAdd: TBCButton;
    btnEdit: TBCButton;
    btnDelete: TBCButton;
    CenterPanel: TBCPanel;
    CenterTopPanel: TBCPanel;
    CenterMainPanel: TBCPanel;
    cboParentTablesList: TComboBox;
    RightPanel: TBCPanel;
    LeftPanel: TBCPanel;
    procedure AddDetailActionExecute(Sender: TObject);
    procedure cboParentTablesListChange(Sender: TObject);
  private
    procedure InitVstDictionaryTablesList;
    procedure InitVstDictionaryDetailsList;
    procedure LoadDictionaryTables;
    procedure VSTDictionaryDetailsListBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure VSTDictionaryDetailsListCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTDictionaryDetailsListFocusChanging(Sender: TBaseVirtualTree;
      OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure VSTDictionaryDetailsListFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VSTDictionaryDetailsListGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTDictionaryDetailsListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VSTDictionaryDetailsListHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);

    procedure VSTDictionaryTablesListBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure VSTDictionaryTablesListCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTDictionaryTablesListFocusChanging(Sender: TBaseVirtualTree;
      OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure VSTDictionaryTablesListFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VSTDictionaryTablesListGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTDictionaryTablesListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VSTDictionaryTablesListHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);

    procedure LoadDictionaryDetailsList(VSTNode: PVirtualNode);
  protected
    procedure LoadLanguages; override;
    procedure LoadSkins; override;
  public
    VSTDictionaryTablesList: TVirtualStringTree;
    VSTDictionaryDetailsList: TVirtualStringTree;

    constructor Create(AOwner: TComponent); override;


  end;

var
  DictionaryTablesManagementForm: TDictionaryTablesManagementForm;

implementation

uses
  Language, Helpers, Consts, Repository, Skins, DictionaryDetailFormUnit;

{$R *.lfm}

{ TDictionaryTablesManagementForm }

constructor TDictionaryTablesManagementForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, omNormal);

  InitVstDictionaryTablesList;
  InitVstDictionaryDetailsList;

  LoadDictionaryTables;
end;

procedure TDictionaryTablesManagementForm.cboParentTablesListChange(
  Sender: TObject);
begin
  // Load details based on tables list
  LoadDictionaryDetailsList(VSTDictionaryTablesList.GetFirstSelected);
end;

procedure TDictionaryTablesManagementForm.AddDetailActionExecute(Sender: TObject);
var
  mr: TModalResult;
begin
  if not Assigned(DictionaryDetailForm) then
  begin
    DictionaryDetailForm := TDictionaryDetailForm.Create(Self, TOpenMode.omNew);
    try
      mr := DictionaryDetailForm.ShowModal;
    finally
      FreeAndNil(DictionaryDetailForm);
    end;
  end;
end;

procedure TDictionaryTablesManagementForm.InitVstDictionaryTablesList;
begin

  VSTDictionaryTablesList := TVirtualStringTree.Create(LeftPanel);

  VSTDictionaryTablesList.Name := 'DictionaryTablesList';
  VSTDictionaryTablesList.Parent := LeftPanel;
  VSTDictionaryTablesList.Align := alClient;
  VSTDictionaryTablesList.DefaultNodeHeight := 20;
  VSTDictionaryTablesList.SelectionCurveRadius := 0;
  VSTDictionaryTablesList.TabOrder := 0;
  VSTDictionaryTablesList.Width := 146;

  VSTDictionaryTablesList.OnBeforeItemErase := @VSTDictionaryTablesListBeforeItemErase;
  VSTDictionaryTablesList.OnCompareNodes := @VSTDictionaryTablesListCompareNodes;
  VSTDictionaryTablesList.OnFocusChanging := @VSTDictionaryTablesListFocusChanging;
  VSTDictionaryTablesList.OnFreeNode := @VSTDictionaryTablesListFreeNode;
  VSTDictionaryTablesList.OnGetText := @VSTDictionaryTablesListGetText;
  VSTDictionaryTablesList.OnGetNodeDataSize := @VSTDictionaryTablesListGetNodeDataSize;
  VSTDictionaryTablesList.OnHeaderClick := @VSTDictionaryTablesListHeaderClick;

  // Add colums
  VSTDictionaryTablesList.Header.Columns.Add.Text :=
    GetLanguageItem('DictionaryTablesManagement.Column.DictionaryName', 'Dictionary Name');
  VSTDictionaryTablesList.Header.Columns[0].Width := VSTDictionaryTablesList.Width;

  // Visibility of header columns
  VSTDictionaryTablesList.Header.Options := [
    hoVisible,
    hoColumnResize,
    hoHotTrack,
    hoOwnerDraw,
    hoShowHint,
    hoShowImages,
    hoShowSortGlyphs];
  VSTDictionaryTablesList.Header.Height := 20;

  // Options
  VSTDictionaryTablesList.TreeOptions.AnimationOptions := [
    toAnimatedToggle];

  VSTDictionaryTablesList.TreeOptions.AutoOptions := [
    toAutoDropExpand,
    toAutoScroll,
    toAutoScrollOnExpand,
    toAutoTristateTracking,
    toAutoDeleteMovedNodes];

  VSTDictionaryTablesList.TreeOptions.MiscOptions := [
    toCheckSupport,
    toFullRepaintOnResize,
    toGridExtensions,
    toInitOnSave,
    toWheelPanning];

  VSTDictionaryTablesList.TreeOptions.PaintOptions := [
    toHideFocusRect,
    toThemeAware,
    toUseBlendedImages];

  VSTDictionaryTablesList.TreeOptions.SelectionOptions := [
    toDisableDrawSelection,
    toExtendedFocus,
    toFullRowSelect,
    toMiddleClickSelect,
    toRightClickSelect,
    toCenterScrollIntoView];

  VSTDictionaryTablesList.TreeOptions.StringOptions := [
    toSaveCaptions];

  VSTDictionaryTablesList.ScrollBarOptions.ScrollBars := ssVertical;

  // Colors of nodes
  VSTDictionaryTablesList.Colors.FocusedSelectionColor :=
    MixingColors(clHighlight, clWindow, 70, 20);
  VSTDictionaryTablesList.Colors.FocusedSelectionBorderColor :=
    MixingColors(clHighlight, clWindow, 70, 20);
  VSTDictionaryTablesList.Colors.HotColor := clWhite;
  VSTDictionaryTablesList.Colors.UnfocusedSelectionColor :=
    MixingColors(clHighlight, clWindow, 60, 40);
  VSTDictionaryTablesList.Colors.UnfocusedSelectionBorderColor :=
    MixingColors(clHighlight, clWindow, 60, 40);

  // Set the sort order
  VSTDictionaryTablesList.Header.SortDirection := sdAscending;
  VSTDictionaryTablesList.Header.SortColumn := 0;

  VSTDictionaryTablesList.Show;
end;

procedure TDictionaryTablesManagementForm.InitVstDictionaryDetailsList;
begin
  VSTDictionaryDetailsList := TVirtualStringTree.Create(LeftPanel);

  VSTDictionaryDetailsList.Name := 'DictionaryDetailsList';
  VSTDictionaryDetailsList.Parent := CenterMainPanel;
  VSTDictionaryDetailsList.Align := alClient;
  VSTDictionaryDetailsList.DefaultNodeHeight := 20;
  VSTDictionaryDetailsList.SelectionCurveRadius := 0;
  VSTDictionaryDetailsList.TabOrder := 0;
  VSTDictionaryDetailsList.Width := 342;

  VSTDictionaryDetailsList.OnBeforeItemErase := @VSTDictionaryDetailsListBeforeItemErase;
  VSTDictionaryDetailsList.OnCompareNodes:=@VSTDictionaryDetailsListCompareNodes;
  VSTDictionaryDetailsList.OnFocusChanging:=@VSTDictionaryDetailsListFocusChanging;
  VSTDictionaryDetailsList.OnFreeNode:=@VSTDictionaryDetailsListFreeNode;
  VSTDictionaryDetailsList.OnGetText:=@VSTDictionaryDetailsListGetText;
  VSTDictionaryDetailsList.OnGetNodeDataSize:=@VSTDictionaryDetailsListGetNodeDataSize;
  VSTDictionaryDetailsList.OnHeaderClick:=@VSTDictionaryDetailsListHeaderClick;

  // Add colums
  VSTDictionaryDetailsList.Header.Columns.Add.Text :=
    GetLanguageItem('DictionaryTablesManagement.Column.DictionaryDetailName', 'Name');
  VSTDictionaryDetailsList.Header.Columns[0].Width := VSTDictionaryDetailsList.Width * 2;

  /// Visibility of header columns
  VSTDictionaryDetailsList.Header.Options := [
    hoVisible,
    hoColumnResize,
    hoHotTrack,
    hoOwnerDraw,
    hoShowHint,
    hoShowImages,
    hoShowSortGlyphs];
  VSTDictionaryDetailsList.Header.Height := 20;

  // Options
  VSTDictionaryDetailsList.TreeOptions.AnimationOptions := [
    toAnimatedToggle];

  VSTDictionaryDetailsList.TreeOptions.AutoOptions := [
    toAutoDropExpand,
    toAutoScroll,
    toAutoScrollOnExpand,
    toAutoTristateTracking,
    toAutoDeleteMovedNodes];

  VSTDictionaryDetailsList.TreeOptions.MiscOptions := [
    toCheckSupport,
    toFullRepaintOnResize,
    toGridExtensions,
    toInitOnSave,
    toWheelPanning];

  VSTDictionaryDetailsList.TreeOptions.PaintOptions := [
    toHideFocusRect,
    toThemeAware,
    toUseBlendedImages];

  VSTDictionaryDetailsList.TreeOptions.SelectionOptions := [
    toDisableDrawSelection,
    toExtendedFocus,
    toFullRowSelect,
    toMiddleClickSelect,
    toRightClickSelect]; // toCenterScrollIntoView

  VSTDictionaryDetailsList.TreeOptions.StringOptions := [
    toSaveCaptions];

  VSTDictionaryDetailsList.ScrollBarOptions.ScrollBars := ssVertical;

  // Colors of nodes
  VSTDictionaryDetailsList.Colors.FocusedSelectionColor :=
    MixingColors(clHighlight, clWindow, 70, 20);
  VSTDictionaryDetailsList.Colors.FocusedSelectionBorderColor :=
    MixingColors(clHighlight, clWindow, 70, 20);
  VSTDictionaryDetailsList.Colors.HotColor := clWhite;
  VSTDictionaryDetailsList.Colors.UnfocusedSelectionColor :=
    MixingColors(clHighlight, clWindow, 60, 40);
  VSTDictionaryDetailsList.Colors.UnfocusedSelectionBorderColor :=
    MixingColors(clHighlight, clWindow, 60, 40);

  // Set the sort order
  VSTDictionaryDetailsList.Header.SortDirection := sdAscending;
  VSTDictionaryDetailsList.Header.SortColumn := 0;

  VSTDictionaryDetailsList.Show;
end;

// Load dictionary tables and adds them to the VST
procedure TDictionaryTablesManagementForm.LoadDictionaryTables;
begin
  TRepository.LoadDictionaryNames(VSTDictionaryTablesList, true);

  // Load details based on tables list
  LoadDictionaryDetailsList(VSTDictionaryTablesList.GetFirstSelected);
end;

// Called when the background of a node is about to be erased
procedure TDictionaryTablesManagementForm.VSTDictionaryDetailsListBeforeItemErase
  (Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
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
procedure TDictionaryTablesManagementForm.VSTDictionaryDetailsListCompareNodes(
  Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
var
  data1: PDictionaryDetailTableNodeRec;
  data2: PDictionaryDetailTableNodeRec;
begin
  data1 := Sender.GetNodeData(Node1);
  data2 := Sender.GetNodeData(Node2);

  if (not Assigned(data1) or (data1^.ddtnd = nil)) or
     (not Assigned(data2) or (data2^.ddtnd = nil)) then
    Result := 0
  else begin
    case Column of
      0: Result := CompareText(data1^.ddtnd.Text, data2^.ddtnd.Text);
    end;
  end;
end;

// Load dictionary table detail list after focus Changing
procedure TDictionaryTablesManagementForm.VSTDictionaryDetailsListFocusChanging(
  Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
  NewColumn: TColumnIndex; var Allowed: Boolean);
begin

end;

// Triggered when a node is about to be freed
procedure TDictionaryTablesManagementForm.VSTDictionaryDetailsListFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  data: PDictionaryDetailTableNodeRec;
begin
  data := Sender.GetNodeData(Node);

  if not Assigned(data) then
    Exit;

  if data^.ddtnd <> nil then
    data^.ddtnd.Free;

  Finalize(data^);
end;

// Triggered when access to a node's data happens the first time but the actual
// data size is not set yet.
procedure TDictionaryTablesManagementForm.VSTDictionaryDetailsListGetNodeDataSize
  (Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TDictionaryDetailTableNodeRec);
end;

// This will be called every time when we needs to know about the text of a
// spectfic node and column
procedure TDictionaryTablesManagementForm.VSTDictionaryDetailsListGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  data: PDictionaryDetailTableNodeRec;
begin
  Node^.States := Node^.States + [vsOnFreeNodeCallRequired];
  // A handler for the OnGetText event is always needed as it provides
  // the tree with the string data to display.
  // Note that we are always using WideString
  data := Sender.GetNodeData(Node);

  if Assigned(data) and (data^.ddtnd <> nil) then
  begin
    case Column of
      0: CellText := data^.ddtnd.Text;
    end;
  end;
end;

// Triggered when user clicks on a header button. Set the current SortColumn
// and SortDirection
procedure TDictionaryTablesManagementForm.VSTDictionaryDetailsListHeaderClick(
  Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
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

// Called when the background of a node is about to be erased
procedure TDictionaryTablesManagementForm.VSTDictionaryTablesListBeforeItemErase
  (Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
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
procedure TDictionaryTablesManagementForm.VSTDictionaryTablesListCompareNodes(
  Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
var
  data1: PDictionaryTableNodeRec;
  data2: PDictionaryTableNodeRec;
begin
  data1 := Sender.GetNodeData(Node1);
  data2 := Sender.GetNodeData(Node2);

  if (not Assigned(data1) or (data1^.dtnd = nil)) or
     (not Assigned(data2) or (data2^.dtnd = nil)) then
    Result := 0
  else begin
    case Column of
      0: Result := CompareText(data1^.dtnd.Name, data2^.dtnd.Name);
    end;
  end;

end;

// Load dictionary table detail list after focus Changing
procedure TDictionaryTablesManagementForm.VSTDictionaryTablesListFocusChanging(
  Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
  NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  LoadDictionaryDetailsList(NewNode);
end;

// Triggered when a node is about to be freed
procedure TDictionaryTablesManagementForm.VSTDictionaryTablesListFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  data: PDictionaryTableNodeRec;
begin
  data := Sender.GetNodeData(Node);

  if not Assigned(data) then
    Exit;

  if data^.dtnd <> nil then
    data^.dtnd.Free;

  Finalize(data^);
end;

// Triggered when access to a node's data happens the first time but the actual
// data size is not set yet.
procedure TDictionaryTablesManagementForm.VSTDictionaryTablesListGetNodeDataSize
  (Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TDictionaryTableNodeRec);
end;

// This will be called every time when we needs to know about the text of a
// spectfic node and column
procedure TDictionaryTablesManagementForm.VSTDictionaryTablesListGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  data: PDictionaryTableNodeRec;
begin
  Node^.States := Node^.States + [vsOnFreeNodeCallRequired];
  // A handler for the OnGetText event is always needed as it provides
  // the tree with the string data to display.
  // Note that we are always using WideString
  data := Sender.GetNodeData(Node);

  if Assigned(data) and (data^.dtnd <> nil) then
  begin
    case Column of
      0: CellText := data^.dtnd.Name;
    end;
  end;
end;

// Triggered when user clicks on a header button. Set the current SortColumn
// and SortDirection
procedure TDictionaryTablesManagementForm.VSTDictionaryTablesListHeaderClick(
  Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
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

// Load dictionary details list
procedure TDictionaryTablesManagementForm.LoadDictionaryDetailsList(
  VSTNode: PVirtualNode);
var
  data: PDictionaryTableNodeRec;
  dictionaryType: TDictionaryType;
  parentDictionaryType: TDictionaryType;
  parentDictionaryRowCode: string;
begin
  if VSTNode <> nil then
    data := VSTDictionaryTablesList.GetNodeData(VSTNode)
  else
    Exit;

  dictionaryType := data^.dtnd.DictionaryType;

  TRepository.GetParentDictionaryType(dictionaryType, parentDictionaryType);

  CenterTopPanel.Visible := parentDictionaryType <> TDictionaryType.dkNone;

  if (CenterTopPanel.Visible) and (cboParentTablesList.Items.Count = 0) then
    TRepository.AddDictionaryItemsToComboBox(cboParentTablesList, parentDictionaryType, false);

  TRepository.GetDictionaryCodeFromSelectedItem(cboParentTablesList, parentDictionaryRowCode);

  TRepository.LoadDictionaryDetails(VSTDictionaryDetailsList, dictionaryType,
    parentDictionaryType, parentDictionaryRowCode);
end;

procedure TDictionaryTablesManagementForm.LoadLanguages;
begin
  inherited LoadLanguages;

  Self.Caption := GetLanguageItem('DictionaryTablesManagement.WindowName', 'Dictionary Tables');
  lblTitle.Caption := GetLanguageItem('DictionaryTablesManagement.Title', 'Add, edit or delete dictionary tables');
end;

procedure TDictionaryTablesManagementForm.LoadSkins;
begin
  inherited LoadSkins;

  btnAdd.Glyph.Assign(TSkins.GetBitmapItem('btnAdd'));
  btnEdit.Glyph.Assign(TSkins.GetBitmapItem('btnEdit'));
  btnDelete.Glyph.Assign(TSkins.GetBitmapItem('btnDelete'));
end;

end.

