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

  public
    VSTDictionaryTablesList: TVirtualStringTree;

    constructor Create(AOwner: TComponent); override;


  end;

var
  DictionaryTablesManagementForm: TDictionaryTablesManagementForm;

implementation

uses
  Language, Helpers;

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


  // Add colums
  VSTDictionaryTablesList.Header.Columns.Add.Text :=
    GetLanguageItem('DictionaryTable.Column.Table', 'Table');
  VSTDictionaryTablesList.Header.Columns[0].Width := VSTDictionaryTablesList.Width - 5;

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

end.

