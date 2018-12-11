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
  Graphics, Dialogs, ActnList, BaseFormUnit, VirtualTrees, RadioPlayerTypes;

type

  { TDictionaryTablesManagementForm }

  TDictionaryTablesManagementForm = class(TBaseForm)
    CenterPanel: TBCPanel;
    RightPanel: TBCPanel;
    LeftPanel: TBCPanel;
  private
    procedure InitVstDictionaryTablesList;
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

  public
    VSTDictionaryTablesList: TVirtualStringTree;

    constructor Create(AOwner: TComponent); override;


  end;

var
  DictionaryTablesManagementForm: TDictionaryTablesManagementForm;

implementation

uses
  Language, Helpers, Consts;

{$R *.lfm}

{ TDictionaryTablesManagementForm }

constructor TDictionaryTablesManagementForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, omNormal);

  InitVstDictionaryTablesList;
end;

procedure TDictionaryTablesManagementForm.InitVstDictionaryTablesList;
begin

  VSTDictionaryTablesList := TVirtualStringTree.Create(LeftPanel);

  VSTDictionaryTablesList.Name := 'DictionaryTablesList';
  VSTDictionaryTablesList.Parent := LeftPanel;
  VSTDictionaryTablesList.Align := alClient;
  VSTDictionaryTablesList.DefaultNodeHeight := 20;
  VSTDictionaryTablesList.SelectionCurveRadius := 5;
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
    GetLanguageItem('DictionaryTable.Column.Table', 'Table');
  VSTDictionaryTablesList.Header.Columns[0].Width := VSTDictionaryTablesList.Width - 20;

  /// Visibility of header columns
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

end.

