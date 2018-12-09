unit MainFormUnit;
{===============================================================================
File:                MainFormUnit.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Main Form

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BCButton, BGRAFlashProgressBar, BCLabel, BCPanel,
  Forms, Controls, Graphics, Dialogs, LCLType, StdCtrls, ExtCtrls, Menus,
  Helpers, RadioPlayer, RadioPlayerTypes, VirtualTrees, ImgList, ActnList,
  CTRPTextScroll, CTRPTrackBar, zipper, OpenStationUrlFormUnit, Skins, Consts;

type

  { TMainForm }

  TMainForm = class(TForm)
    DeleteStationAction: TAction;
    EditStationAction: TAction;
    AddStationAction: TAction;
    btnSearch: TBCButton;
    miColumnSettings: TMenuItem;
    miShowBothScrollBars: TMenuItem;
    miSpaceLine: TMenuItem;
    miDeleteStation: TMenuItem;
    miEditStation: TMenuItem;
    miAddStation: TMenuItem;
    miShowStationName: TMenuItem;
    miShowGenre: TMenuItem;
    miShowCountry: TMenuItem;
    miSkins: TMenuItem;
    OpenUrlAction: TAction;
    MainActionList: TActionList;
    btnRec: TBCButton;
    btnPrev: TBCButton;
    btnNext: TBCButton;
    btnOpen: TBCButton;
    btnStop: TBCButton;
    btnPause: TBCButton;
    MainPanel: TBCPanel;
    PeakmeterPanel: TBCPanel;
    PopupMenuStationList: TPopupMenu;
    SearchPanel: TBCPanel;
    BottomFunctionPanel: TBCPanel;
    TopInfoPanel: TBCPanel;
    edtSearch: TEdit;
    MainMenu1: TMainMenu;
    miLanguage: TMenuItem;
    miSettings: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    StationListPanel: TPanel;
    pbLeftLevelMeter: TBGRAFlashProgressBar;
    btnPlay: TBCButton;
    pbRightLevelMeter: TBGRAFlashProgressBar;
    Timer1: TTimer;
    SearchTimer: TTimer;
    procedure AddStationActionExecute(Sender: TObject);
    procedure BottomFunctionPanelResize(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure DeleteStationActionExecute(Sender: TObject);
    procedure EditStationActionExecute(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure miShowBothScrollBarsClick(Sender: TObject);
    procedure miShowVstColumnClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure OpenUrlActionExecute(Sender: TObject);
    procedure PeakmeterPanelResize(Sender: TObject);
    procedure PopupMenuStationListPopup(Sender: TObject);
    procedure RadioPlayerRadioPlay(Sender: TObject);
    procedure RadioPlayerRadioPlayerTags(AMessage: string; APlayerMessageType: TPlayerMessageType);
    procedure SearchTimerTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure LoadLoanguages;
    procedure LoadSettings;
    procedure LoadSkin;
    procedure AddLanguageItems;
    procedure AddSkinItems;
    procedure OnLanguageChange(Sender: TObject);
    procedure CreateVstStationList;
    procedure miLanguageItemClick(Sender: TObject);
    procedure miSkinsItemClick(Sender: TObject);
    procedure TextScrollMouseEnter(Sender: TObject);
    procedure TextScrollMouseLeave(Sender: TObject);
    procedure SkinDoneStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure SkinLoaded(Sender: TObject;
      var ASkinData: TSkinData);
    procedure VolumeTrackBarPositionChange(ASender: TObject; APosition: integer);
    procedure VstStationListAfterColumnWidthTracking(Sender: TVTHeader;
      Column: TColumnIndex);
    procedure VstStationListGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);

    procedure VstStationListHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
    procedure VstStationListBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure VstStationListFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VstStationListCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VstStationListGetImageIndexEx(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure VstStationListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VstStationListDblClick(Sender: TObject);
    procedure VstStationListKeyPress(Sender: TObject; var Key: char);
    procedure VstStationListFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);

    procedure StationDetailManagement(OpenMode: TOpenMode; DropFileName: string = EMPTY_STR);
  public
    RadioPlayer: TRadioPlayer;

    VstStationList: TVirtualStringTree;

    TextScroll: TCTRPTextScroll;
    VolumeTrackBar: TCTRPTrackBar;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Language, TRPSettings, Repository, StationDetailFormUnit;

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MainWindowHandle := Handle;

  RadioPlayer := TRadioPlayer.Create;
  RadioPlayer.OnRadioPlayerTags := @RadioPlayerRadioPlayerTags;
  RadioPlayer.OnRadioPlay := @RadioPlayerRadioPlay;

  CreateVstStationList;

  // Create volume control
  VolumeTrackBar := TCTRPTrackBar.Create(Self);
  VolumeTrackBar.Parent := BottomFunctionPanel;
  VolumeTrackBar.Width := 130;
  VolumeTrackBar.Top := 2;
  VolumeTrackBar.Left := VolumeTrackBar.Parent.Width - VolumeTrackBar.Width - 2;
  VolumeTrackBar.Anchors := [akBottom];
  VolumeTrackBar.Position := 100;
  VolumeTrackBar.OnPositionChange := @VolumeTrackBarPositionChange;
  VolumeTrackBar.Show;
  VolumeTrackBar.Tag := VolumeTrackBar.Width;

  // Create text scroll
  TextScroll := TCTRPTextScroll.Create(Self);
  TextScroll.Parent := TopInfoPanel;

  TextScroll.Lines.TextScrollLine1.BackgroundColor := RGBToColor(0, 175, 240);
  TextScroll.Lines.TextScrollLine1.Border.BorderLeft.BorderColor := RGBToColor(0, 175, 240);
  TextScroll.Lines.TextScrollLine1.Border.BorderLeft.BorderWidth := 2;
  TextScroll.Lines.TextScrollLine1.Border.BorderTop.BorderColor := RGBToColor(0, 175, 240);
  TextScroll.Lines.TextScrollLine1.Border.BorderTop.BorderWidth := 2;
  TextScroll.Lines.TextScrollLine1.Border.BorderRight.BorderColor := RGBToColor(0, 175, 240);
  TextScroll.Lines.TextScrollLine1.Border.BorderRight.BorderWidth := 2;
  TextScroll.Lines.TextScrollLine1.ScrollText := 'Tiny Radio Player';

  TextScroll.Lines.TextScrollLine2.BackgroundColor := RGBToColor(0, 175, 240);
  TextScroll.Lines.TextScrollLine2.Border.BorderLeft.BorderColor := RGBToColor(0, 175, 240);
  TextScroll.Lines.TextScrollLine2.Border.BorderLeft.BorderWidth := 2;
  TextScroll.Lines.TextScrollLine2.Border.BorderRight.BorderColor := RGBToColor(0, 175, 240);
  TextScroll.Lines.TextScrollLine2.Border.BorderRight.BorderWidth := 2;
  TextScroll.Lines.TextScrollLine2.Border.BorderBottom.BorderColor := RGBToColor(0, 175, 240);
  TextScroll.Lines.TextScrollLine2.Border.BorderBottom.BorderWidth := 2;
  TextScroll.Lines.TextScrollLine2.ScrollText := 'ver. 0.1';

  TextScroll.OnMouseEnter := @TextScrollMouseEnter;
  TextScroll.OnMouseLeave := @TextScrollMouseLeave;

  TextScroll.Align := alClient;
  //TextScroll.PopupMenu := pmRedTextScroll;

  LoadSettings;
  LoadSkin;
  LoadLoanguages;
  AddLanguageItems;
  AddSkinItems;

  TLanguage.RegisterLanguageChangeEvent(@OnLanguageChange);

  TRepository.LoadStations(VstStationList, edtSearch.Text);

  // Calculating position of function buttons and volume
  btnPrev.Tag := (BottomFunctionPanel.Width div 2) - btnPrev.Left;
  btnPlay.Tag := (BottomFunctionPanel.Width div 2) - btnPlay.Left;
  btnPause.Tag := (BottomFunctionPanel.Width div 2) - btnPause.Left;
  btnStop.Tag := (BottomFunctionPanel.Width div 2) - btnStop.Left;
  btnNext.Tag := (BottomFunctionPanel.Width div 2) - btnNext.Left;
  btnRec.Tag := (BottomFunctionPanel.Width div 2) - btnRec.Left;
  btnOpen.Tag := (BottomFunctionPanel.Width div 2) - btnOpen.Left;

  MainForm.Width := TTRPSettings.GetValue('MainForm.Width', 485);
  MainForm.Height := TTRPSettings.GetValue('MainForm.Height', 516);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(RadioPlayer);

  FreeAndNil(TextScroll);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  TTRPSettings.SetValue('MainForm.Width', MainForm.Width);
  TTRPSettings.SetValue('MainForm.Height', MainForm.Height);
end;

procedure TMainForm.miShowBothScrollBarsClick(Sender: TObject);
var
  menuItem: TMenuItem;
begin
  if Sender is TMenuItem then
  begin
    menuItem := TMenuItem(Sender);

    menuItem.Checked := not menuItem.Checked;

    if menuItem.Checked then
    begin
      VstStationList.ScrollBarOptions.ScrollBars := ssBoth;
      TTRPSettings.SetValue('StationList.ScrollBars.Both', 'true');
    end
    else
    begin
      VstStationList.ScrollBarOptions.ScrollBars := ssVertical;
      TTRPSettings.SetValue('StationList.ScrollBars.Both', 'false');
    end;
  end;
end;

procedure TMainForm.miShowVstColumnClick(Sender: TObject);
var
  menuItem: TMenuItem;
  columnVisibility: string;
begin
   if Sender is TMenuItem then
   begin
     menuItem := TMenuItem(Sender);

     menuItem.Checked := not menuItem.Checked;

     if menuItem.Checked then
        VstStationList.Header.Columns[menuItem.Tag].Options := VstStationList.Header.Columns[menuItem.Tag].Options + [coVisible]
     else
       VstStationList.Header.Columns[menuItem.Tag].Options := VstStationList.Header.Columns[menuItem.Tag].Options - [coVisible];

     columnVisibility := string(IIF(menuItem.Checked, 'true', 'false'));
     case menuItem.Tag of
       0: TTRPSettings.SetValue('StationList.ColumnVisibility.StationName', columnVisibility);
       1: TTRPSettings.SetValue('StationList.ColumnVisibility.Genre', columnVisibility);
       2: TTRPSettings.SetValue('StationList.ColumnVisibility.Country', columnVisibility);
     end;

   end;
end;

procedure TMainForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.OpenUrlActionExecute(Sender: TObject);
begin
  if not Assigned(OpenStationUrlForm) then
  begin
    OpenStationUrlForm := TOpenStationUrlForm.Create(Self);
    try
     if OpenStationUrlForm.ShowModal = mrOK then
     begin
       RadioPlayer.PlayURL(OpenStationUrlForm.Url, VolumeTrackBar.Position);
     end;
    finally
      FreeAndNil(OpenStationUrlForm);
    end;
  end;
end;

procedure TMainForm.PeakmeterPanelResize(Sender: TObject);
var
  panelCenter: integer;
begin
  panelCenter := PeakmeterPanel.Width div 2;

  pbLeftLevelMeter.Width := panelCenter - 2;
  pbRightLevelMeter.Left := panelCenter + 2;
  pbRightLevelMeter.Width := panelCenter - 2;

end;

procedure TMainForm.PopupMenuStationListPopup(Sender: TObject);
var
  stationSelected: boolean;
begin
  stationSelected := VstStationList.GetFirstSelected() <> nil;

  miEditStation.Enabled := stationSelected;
  miDeleteStation.Enabled := stationSelected;
end;

procedure TMainForm.edtSearchChange(Sender: TObject);
begin
  SearchTimer.Enabled := False;
  SearchTimer.Enabled := True;
end;

procedure TMainForm.BottomFunctionPanelResize(Sender: TObject);
var
  panelCenter: integer;
  buttonCount: byte;
  buttonWidth: byte;
  buttonSpace: byte;
  button7: integer;
begin
  buttonCount := 7;
  buttonWidth := 23;
  buttonSpace := 2;

  panelCenter := BottomFunctionPanel.Width div 2;

  button7 := round( panelCenter + (buttonWidth * (buttonCount / 2) + ((buttonCount / 2) * buttonSpace)  ) );

  if (VolumeTrackBar.Left - 5 < button7) then
    button7 := VolumeTrackBar.Left - 5;

  if button7 <  buttonCount * buttonWidth + ((buttonCount - 1) * buttonSpace) then
   button7 := buttonCount * buttonWidth + ((buttonCount - 1) * buttonSpace);

  VolumeTrackBar.Left := BottomFunctionPanel.Width - VolumeTrackBar.Width - 3;

  if (VolumeTrackBar.Left < button7) or ((VolumeTrackBar.Left > button7) and (VolumeTrackBar.Width < VolumeTrackBar.Tag))  then
  begin
    VolumeTrackBar.Left := button7;
    VolumeTrackBar.Width := BottomFunctionPanel.Width - button7 - 3;
  end else
    VolumeTrackBar.Width := VolumeTrackBar.Tag;

  if VolumeTrackBar.Width > VolumeTrackBar.Tag then
  begin
    VolumeTrackBar.Width := VolumeTrackBar.Tag;
    VolumeTrackBar.Left := BottomFunctionPanel.Width - VolumeTrackBar.Width - 3;
  end;

  btnPrev.Left := button7 - (7 * buttonWidth) - (6 * buttonSpace);
  btnPlay.Left := button7 - (6 * buttonWidth) - (5 * buttonSpace);
  btnPause.Left := button7 - (5 * buttonWidth) - (4 * buttonSpace);
  btnStop.Left := button7 - (4 * buttonWidth) - (3 * buttonSpace);
  btnNext.Left := button7 - (3 * buttonWidth) - (2 * buttonSpace);
  btnRec.Left := button7 - (2 * buttonWidth) - buttonSpace;
  btnOpen.Left := button7 - buttonWidth;

end;

procedure TMainForm.AddStationActionExecute(Sender: TObject);
begin
  StationDetailManagement(TOpenMode.omNew);
end;

procedure TMainForm.EditStationActionExecute(Sender: TObject);
begin
  if VstStationList.GetFirstSelected() = nil then
    Exit;

  StationDetailManagement(TOpenMode.omEdit);
end;

procedure TMainForm.DeleteStationActionExecute(Sender: TObject);
begin
  if VstStationList.GetFirstSelected() = nil then
    Exit;

  StationDetailManagement(TOpenMode.omDelete);
end;

procedure TMainForm.btnOpenClick(Sender: TObject);
begin
  OpenUrlActionExecute(Self);
end;

procedure TMainForm.btnPlayClick(Sender: TObject);
begin
  //RadioPlayer.PlayURL(edtStreamUrl.Caption, VolumeTrackBar.Position);
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
  RadioPlayer.Stop();
end;

procedure TMainForm.RadioPlayerRadioPlay(Sender: TObject);
begin

end;

procedure TMainForm.RadioPlayerRadioPlayerTags(AMessage: string;
  APlayerMessageType: TPlayerMessageType);
var
  buffPercent: byte;
begin
  case APlayerMessageType of
    Connecting: begin
      TextScroll.Lines.TextScrollLine1.ScrollText := 'Connecting';
    end;
    Error: begin
      TextScroll.Lines.TextScrollLine1.ScrollText := 'Idle: ' + AMessage;
    end;
    Progress: begin
      // Buffering progress
      buffPercent := StrToInt(AMessage);
      if buffPercent < 95 then
      begin

        TextScroll.ProgressBarValue := buffPercent;
        if not TextScroll.ProgressBarVisible then
          TextScroll.ProgressBarVisible := false;

      end else
        TextScroll.ProgressBarVisible := false;

    end;
    StreamName: begin
      TextScroll.Lines.TextScrollLine2.ScrollText := AMessage;
    end;
    Bitrate: begin
      // bitrate
    end;
    StreamTitle: begin
      // title name, song name
      TextScroll.Lines.TextScrollLine1.ScrollText := AMessage;
    end;
    Other: begin
      TextScroll.Lines.TextScrollLine2.ScrollText := AMessage;
    end;
  end;
end;

procedure TMainForm.SearchTimerTimer(Sender: TObject);
begin
  SearchTimer.Enabled := False;
  TRepository.LoadStations(VstStationList, edtSearch.Text);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
  level: integer;
begin

  if RadioPlayer.ChannelIsActiveAndPlaying then
  begin
    level := integer(RadioPlayer.ChannelGetLevel);

    pbLeftLevelMeter.Value := MulDiv(100, LoWord(level), 32768);
    pbRightLevelMeter.Value := MulDiv(100, HiWord(level), 32768);
  end
  else if (pbLeftLevelMeter.Value <> 0) or (pbRightLevelMeter.Value <> 0) then
  begin
    pbLeftLevelMeter.Value := 0;
    pbRightLevelMeter.Value := 0;
  end;
end;

procedure TMainForm.LoadLoanguages;
begin
  miFile.Caption := GetLanguageItem('MainMenu.File', 'File');
  miExit.Caption := GetLanguageItem('MainMenu.File.Exit', 'Exit');
  miSettings.Caption := GetLanguageItem('MainMenu.Settings', 'Settings');
  miLanguage.Caption := GetLanguageItem('MainMenu.Settings.Language', 'Language');
  miSkins.Caption := GetLanguageItem('MainMenu.Settings.Skins', 'Skins');

  VstStationList.Header.Columns[0].Text :=
    GetLanguageItem('MainForm.StationList.StationName', 'Station Name');
  VstStationList.Header.Columns[1].Text :=
    GetLanguageItem('MainForm.StationList.Genre', 'Genre');
  VstStationList.Header.Columns[2].Text :=
    GetLanguageItem('MainForm.StationList.Country', 'Country');

  miShowStationName.Caption :=
    GetLanguageItem('MainForm.StationList.PopupMenu.ShowStationName', 'Show Station Name');
  miShowGenre.Caption :=
    GetLanguageItem('MainForm.StationList.PopupMenu.ShowGenre', 'Show Genre');
  miShowCountry.Caption :=
    GetLanguageItem('MainForm.StationList.PopupMenu.ShowCountry', 'Show Country');
  miShowBothScrollBars.Caption :=
    GetLanguageItem('MainForm.StationList.PopupMenu.ShowBothScrollBars', 'Show Both Scroll Bars');
  miColumnSettings.Caption :=
    GetLanguageItem('MainForm.StationList.PopupMenu.ColumnSettings', 'Column Settings');

  miAddStation.Caption :=
    GetLanguageItem('MainForm.StationList.PopupMenu.AddStation', 'Add Station');
  miEditStation.Caption :=
    GetLanguageItem('MainForm.StationList.PopupMenu.EditStation', 'Edit Station');
  miDeleteStation.Caption :=
    GetLanguageItem('MainForm.StationList.PopupMenu.DeleteStation', 'Delete Station');
end;

procedure TMainForm.LoadSettings;
begin
  VolumeTrackBar.Position := TTRPSettings.GetValue('Volume', 100);
end;

procedure TMainForm.LoadSkin;
begin
  TSkins.OnSkinDoneStream := @SkinDoneStream;
  TSkins.OnSkinLoaded := @SkinLoaded;
  TSkins.LoadSkin();
end;

procedure TMainForm.AddLanguageItems;
var
  i: integer;
  subItem: TMenuItem;
begin
  for i := 0 to Pred(TLanguage.LanguageFiles.Count) do
  begin
    subItem := TMenuItem.Create(miLanguage);
    subItem.Caption := TLanguage.LanguageFiles[i];
    subItem.Tag := i;
    subItem.OnClick := @miLanguageItemClick;
    subItem.Checked := TLanguage.CurrentLangName = TLanguage.LanguageFiles[i];
    miLanguage.Add(subItem);
  end;
end;

procedure TMainForm.AddSkinItems;
var
  i: integer;
  subItem: TMenuItem;
begin
  for i := 0 to Pred(TSkins.SkinFiles.Count) do
  begin
    subItem := TMenuItem.Create(miSkins);
    subItem.Caption := TSkins.SkinFiles[i];
    subItem.Tag := i;
    subItem.OnClick:= @miSkinsItemClick;
    subItem.Checked := TSkins.CurrentSkinName = TSkins.SkinFiles[i];
    miSkins.Add(subItem);
  end;
end;

procedure TMainForm.OnLanguageChange(Sender: TObject);
begin
  LoadLoanguages;
end;

procedure TMainForm.CreateVstStationList;
begin

  VstStationList := TVirtualStringTree.Create(Self);

  VstStationList.Name := 'StationList';
  VstStationList.Parent := MainPanel;
  VstStationList.Align := alClient;
  VstStationList.DefaultNodeHeight := 20;
  VstStationList.SelectionCurveRadius := 5;
  VstStationList.TabOrder := 0;

  VstStationList.PopupMenu := PopupMenuStationList;

  // Events
  VstStationList.OnBeforeItemErase := @VstStationListBeforeItemErase;
  VstStationList.OnHeaderClick := @VstStationListHeaderClick;
  VstStationList.OnFreeNode := @VstStationListFreeNode;
  VstStationList.OnCompareNodes := @VstStationListCompareNodes;
  VstStationList.OnGetImageIndexEx := @VstStationListGetImageIndexEx;
  VstStationList.OnGetText := @VstStationListGetText;
  VstStationList.OnDblClick := @VstStationListDblClick;
  VstStationList.OnKeyPress := @VstStationListKeyPress;
  VstStationList.OnFocusChanged := @VstStationListFocusChanged;
  VstStationList.OnGetNodeDataSize := @VstStationListGetNodeDataSize;
  VstStationList.OnAfterColumnWidthTracking := @VstStationListAfterColumnWidthTracking;

  // Add columns
  VstStationList.Header.Columns.Add.Text :=
    GetLanguageItem('MainForm.StationList.StationName', 'Station Name');
  VstStationList.Header.Columns[0].Width := TTRPSettings.GetValue('StationList.ColumnWidth.StationName', 240);
  VstStationList.Header.Columns.Add.Text :=
    GetLanguageItem('MainForm.StationList.Genre', 'Genre');
  VstStationList.Header.Columns[1].Width := TTRPSettings.GetValue('StationList.ColumnWidth.Genre', 120);
  VstStationList.Header.Columns.Add.Text :=
    GetLanguageItem('MainForm.StationList.Country', 'Country');
  VstStationList.Header.Columns[2].Width := TTRPSettings.GetValue('StationList.ColumnWidth.Country', 90);

  // Columns visibility
  if (TTRPSettings.GetValue('StationList.ColumnVisibility.StationName', 'true') = 'false') then
  begin
    VstStationList.Header.Columns[0].Options := VstStationList.Header.Columns[0].Options - [coVisible];
    miShowStationName.Checked := false;
  end;
  if (TTRPSettings.GetValue('StationList.ColumnVisibility.Genre', 'true') = 'false') then begin
    VstStationList.Header.Columns[1].Options := VstStationList.Header.Columns[1].Options - [coVisible];
    miShowGenre.Checked := false;
  end;
  if (TTRPSettings.GetValue('StationList.ColumnVisibility.Country', 'true') = 'false') then
  begin
    VstStationList.Header.Columns[2].Options := VstStationList.Header.Columns[2].Options - [coVisible];
    miShowCountry.Checked := false;
  end;

  // Visibility of the header columns
  VstStationList.Header.Options := [hoVisible, hoColumnResize, hoHotTrack, hoOwnerDraw, hoShowHint, hoShowImages, hoShowSortGlyphs];
  VstStationList.Header.Height := 20;

  // Options
  VstStationList.TreeOptions.AnimationOptions := [toAnimatedToggle];
  VstStationList.TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes];
  VstStationList.TreeOptions.MiscOptions := [toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toWheelPanning];
  VstStationList.TreeOptions.PaintOptions := [toHideFocusRect,  toThemeAware, toUseBlendedImages];
  VstStationList.TreeOptions.SelectionOptions := [toDisableDrawSelection, toExtendedFocus, toFullRowSelect, toMiddleClickSelect, toRightClickSelect];  // toCenterScrollIntoView
  VstStationList.TreeOptions.StringOptions := [toSaveCaptions];

  // Nodes color
  VstStationList.Colors.FocusedSelectionColor := MixingColors(clHighlight,clWindow,70,20);
  VstStationList.Colors.FocusedSelectionBorderColor := MixingColors(clHighlight,clWindow,70,20);
  VstStationList.Colors.HotColor := clWhite;
  VstStationList.Colors.UnfocusedSelectionColor := MixingColors(clHighlight,clWindow,60,40);
  VstStationList.Colors.UnfocusedSelectionBorderColor := MixingColors(clHighlight,clWindow,60,40);

  // Sort direction
  VstStationList.Header.SortDirection :=
    IIF(TTRPSettings.GetValue('StationList.SortDirection', 'ASC') = 'ASC', TSortDirection.sdAscending, TSortDirection.sdDescending);
  VstStationList.Header.SortColumn := TTRPSettings.GetValue('StationList.SortColumn', 0);

  // Scroll bars
  if (TTRPSettings.GetValue('StationList.ScrollBars.Both', 'false') = 'true') then
  begin
    VstStationList.ScrollBarOptions.ScrollBars := ssBoth;
    miShowBothScrollBars.Checked := true;
  end
  else
  begin
    VstStationList.ScrollBarOptions.ScrollBars := ssVertical;
    miShowBothScrollBars.Checked := false;
  end;

  VstStationList.Show;
end;

procedure TMainForm.TextScrollMouseEnter(Sender: TObject);
begin
  TextScroll.Lines.TextScrollLine1.BackgroundColor := RGBToColor(0, 168, 229);
  TextScroll.Lines.TextScrollLine2.BackgroundColor := RGBToColor(0, 168, 229);
end;

procedure TMainForm.TextScrollMouseLeave(Sender: TObject);
begin
  TextScroll.Lines.TextScrollLine1.BackgroundColor := RGBToColor(0, 175, 240);
  TextScroll.Lines.TextScrollLine2.BackgroundColor := RGBToColor(0, 175, 240);
end;

procedure TMainForm.VolumeTrackBarPositionChange(ASender: TObject;
  APosition: integer);
begin
  RadioPlayer.Volume(VolumeTrackBar.Position);
  TTRPSettings.SetValue('Volume', VolumeTrackBar.Position);
end;

procedure TMainForm.miLanguageItemClick(Sender: TObject);
var
  i: integer;
  mi : TMenuItem;
begin
  if Sender is TMenuItem then
  begin
    // mark the selected item and deselect all others
    mi := TMenuItem(Sender);
    for i := 0 to Pred(mi.Parent.Count) do
      mi.Parent.Items[i].Checked := mi.Parent.Items[i] = mi;

    TLanguage.ChangeLanguage(mi.Caption);
  end;
end;

procedure TMainForm.miSkinsItemClick(Sender: TObject);
var
  i: integer;
  mi : TMenuItem;
begin
  if Sender is TMenuItem then
  begin
    // mark the selected item and deselect all others
    mi := TMenuItem(Sender);
    for i := 0 to Pred(mi.Parent.Count) do
      mi.Parent.Items[i].Checked := mi.Parent.Items[i] = mi;

    TSkins.ChangeSkin(mi.Caption);
  end;
end;

// Called when the background of a node is about to be erased
procedure TMainForm.VstStationListBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  Data: PStationNodeRec;
begin
  if (not Sender.Selected[Node]) then
  begin
    Data := Sender.GetNodeData(Node);

    // Colorize line with currently playing station
    if (Data^.snd.ID = RadioPlayer.CurrentStationId) then
    begin
      ItemColor := GridLineColorCurrentlyPlaying;
      EraseAction := eaColor;
    end
    else
    // Coloring every second line
    if (Odd(Node^.Index)) then
    begin
      ItemColor := GridLineColor;
      EraseAction := eaColor;
    end;
  end;
end;

// Triggered when user clicks on a header button. Set the current SortColumn
// and SortDirection
procedure TMainForm.VstStationListHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  // We determine the sort direction but only if click on the same column
  if (Sender.SortColumn = HitInfo.Column) then
  begin
    if Sender.SortDirection = sdAscending then
      Sender.SortDirection := sdDescending
    else
      Sender.SortDirection := sdAscending;

    TTRPSettings.SetValue('StationList.SortDirection',
      string(IIF(Sender.SortDirection = TSortDirection.sdAscending, 'ASC', 'DESC')));
  end;

  TTRPSettings.SetValue('StationList.SortColumn', HitInfo.Column);

  // show SortGlyph
  Sender.SortColumn := HitInfo.Column;

  // sorting
  Sender.Treeview.SortTree(HitInfo.Column, Sender.SortDirection, True);
end;

// Triggered when a node is about to be freed
procedure TMainForm.VstStationListFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PStationNodeRec;
begin
  Data := Sender.GetNodeData(Node);

  if not Assigned(Data) then
    Exit;

  if Data^.snd <> nil then
    Data^.snd.Free;

  Finalize(Data^);
end;

// Sort and search support event
procedure TMainForm.VstStationListCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PStationNodeRec;
  Data2: PStationNodeRec;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  if (not Assigned(Data1) or (Data1^.snd = nil)) or
     (not Assigned(Data2) or (Data2^.snd = nil)) then
    Result := 0
  else begin
    case Column of
      0: Result := CompareText(Data1^.snd.Name, Data2^.snd.Name);
      1: Result := CompareText(Data1^.snd.Genre, Data2^.snd.Genre);
      2: Result := CompareText(Data1^.snd.Country, Data2^.snd.Country);
    end;
  end;
end;

// Called when the background of a node is about to be erased
procedure TMainForm.VstStationListGetImageIndexEx(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
var
  Data: PStationNodeRec;
begin
  case Kind of
    ikNormal, ikSelected:
      begin
        if Column = 0 then
        begin
          Data := Sender.GetNodeData(Node);
          if not Assigned(Data) then
            Exit;

          // Take play image
          if (Data^.snd.ID = RadioPlayer.CurrentStationID) then
            ImageIndex := 0;
        end;
      end;
  end;
end;

// This will be called every time when we needs to know about the text of a
// spectfic node and column
procedure TMainForm.VstStationListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PStationNodeRec;
begin
  Node^.States := Node^.States +[vsOnFreeNodeCallRequired];
  // A handler for the OnGetText event is always needed as it provides
  // the tree with the string data to display.
  // Note that we are always using WideString
  Data := Sender.GetNodeData(Node);

  if Assigned(Data) and (Data^.snd <> nil) then
  begin
    case Column of
      0: CellText := Data^.snd.Name;
      1: CellText := Data^.snd.Genre;
      2: CellText := Data^.snd.Country;
    end;
  end;
end;

// Triggered when double clicked
procedure TMainForm.VstStationListDblClick(Sender: TObject);
begin
  RadioPlayer.PlayStation(
    RadioPlayer.GetSelectedStationId(VstStationList),
    VolumeTrackBar.Position);
end;

// Triggered when key pressed
procedure TMainForm.VstStationListKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = Char(VK_RETURN)) then
  begin
    // set #0 to eliminate Ding sound
    Key := #0;
  end
end;

// Triggered when the node focus is about to change
procedure TMainForm.VstStationListFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  {with TVirtualStringTree(Sender) do
  begin
    if OldNode <> nil then
      NodeHeight[OldNode] := DefaultNodeHeight;
    if NewNode <> nil then
      NodeHeight[NewNode] := round(DefaultNodeHeight * 1.1);
  end;}
end;

procedure TMainForm.StationDetailManagement(OpenMode: TOpenMode; DropFileName: string = EMPTY_STR);
var
  mr: TModalResult;
begin
  if not Assigned(StationDetailForm) then
  begin
    case OpenMode of
      { NEW }
      TOpenMode.omNew: begin
        StationDetailForm := TStationDetailForm.Create(Self, OpenMode, EMPTY_INT, DropFileName);
        try
          mr := StationDetailForm.ShowModal;

          if mr = mrOK then
            TRepository.LoadStations(VstStationList, edtSearch.Text);
        finally
          FreeAndNil(StationDetailForm);
        end;
      end;

      { EDIT }
      TOpenMode.omEdit: begin
        StationDetailForm := TStationDetailForm.Create(Self, OpenMode,
          TRepository.GetSelectedStationId(VstStationList), DropFileName);
        try
          mr := StationDetailForm.ShowModal;

          if mr = mrOK then
            TRepository.LoadStations(VstStationList, edtSearch.Text);

        finally
          FreeAndNil(StationDetailForm);
        end;
      end;

      { DELETE }
      TOpenMode.omDelete: begin
        StationDetailForm := TStationDetailForm.Create(Self, OpenMode,
          TRepository.GetSelectedStationId(VstStationList), DropFileName);
        try
          mr := StationDetailForm.ShowModal;

          if mr = mrOK then
            TRepository.LoadStations(VstStationList, edtSearch.Text);

        finally
          FreeAndNil(StationDetailForm);
        end;
      end;

    end;
  end;
end;

// Set columns width
procedure TMainForm.VstStationListAfterColumnWidthTracking(Sender: TVTHeader;
  Column: TColumnIndex);
begin
  case Column of
    0: TTRPSettings.SetValue('StationList.ColumnWidth.StationName', Sender.Columns[Column].Width);
    1: TTRPSettings.SetValue('StationList.ColumnWidth.Genre', Sender.Columns[Column].Width);
    2: TTRPSettings.SetValue('StationList.ColumnWidth.Country', Sender.Columns[Column].Width);
  end;
end;

procedure TMainForm.SkinDoneStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var
  picture: TPicture;
begin
  // Using TPicture to avoid black background instead of transparent
  picture := TPicture.Create;
  try
    picture.LoadFromStream(AStream);

    case StringToCaseSelect(AItem.DiskFileName,
          ['icoStations.png','icoFavorites.png','icoRecords.png',
          'icoScheduled.png', 'icoEqualizer.png', 'icoOptions.png',
          'icoHistory.png', 'icoSearch.png', 'icoPlayStation.png', 'btnAdd.png',
          'btnEdit.png', 'btnDelete.png', 'btnPlay.png', 'btnPause.png',
          'btnPrev.png', 'btnStop.png', 'btnNext.png', 'btnRec.png',
          'btnOpen.png']) of
       7: btnSearch.Glyph.Assign(picture.Bitmap);
       9: miAddStation.Bitmap.Assign(picture.Bitmap);
       10: miEditStation.Bitmap.Assign(picture.Bitmap);
       11: miDeleteStation.Bitmap.Assign(picture.Bitmap);
       12: btnPlay.Glyph.Assign(picture.Bitmap);
       13: btnPause.Glyph.Assign(picture.Bitmap);
       14: btnPrev.Glyph.Assign(picture.Bitmap);
       15: btnStop.Glyph.Assign(picture.Bitmap);
       16: btnNext.Glyph.Assign(picture.Bitmap);
       17: btnRec.Glyph.Assign(picture.Bitmap);
       18: btnOpen.Glyph.Assign(picture.Bitmap);
    end;

  finally
    picture.Free;
  end;
end;

procedure TMainForm.SkinLoaded(Sender: TObject; var ASkinData: TSkinData);
begin
  VolumeTrackBar.Track.TrackBackground.BackgroundColor :=
    ASkinData.GetColorItem('VolumeTrackBar.Track.TrackBackground.BackgroundColor');

  VolumeTrackBar.Thumb.ThumbFontColor :=
    ASkinData.GetColorItem('VolumeTrackBar.Thumb.ThumbFontColor');
end;

// Triggered when access to a node's data happens the first time but the actual
// data size is not set yet.
procedure TMainForm.VstStationListGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TStationNodeRec);
end;

end.

