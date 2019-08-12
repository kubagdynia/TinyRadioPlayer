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
  Classes, SysUtils, FileUtil, BCButton, BGRAFlashProgressBar, BCPanel,
  Forms, Controls, Graphics, Dialogs, LCLType, StdCtrls,
  ExtCtrls, Menus, Helpers, RadioPlayer, RadioPlayerTypes, VirtualTrees,
  ImgList, ActnList, CTRPTextScroll, CTRPTrackBar, zipper,
  OpenStationUrlFormUnit, Skins, Consts, CTRPCustomDrawn, ExportImport;

type

  { TMainForm }
  TMainForm = class(TForm)
    AboutAction: TAction;
    ExportStationsAction: TAction;
    miLine1: TMenuItem;
    miExportStations: TMenuItem;
    miAbout: TMenuItem;
    OpenEqualizerAction: TAction;
    miEqualizer: TMenuItem;
    miCopyTitleToClipboard: TMenuItem;
    miSearchOnYoutube: TMenuItem;
    miSpace1: TMenuItem;
    miStationInfo: TMenuItem;
    NextAction: TAction;
    PopupMenuTextScroll: TPopupMenu;
    PrevAction: TAction;
    btnStop: TBCButton;
    StopAction: TAction;
    PlayAction: TAction;
    BCPanel1: TBCPanel;
    OpenDictionaryTablesAction: TAction;
    DeleteStationAction: TAction;
    EditStationAction: TAction;
    AddStationAction: TAction;
    miDictionaryTables: TMenuItem;
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
    MainPanel: TBCPanel;
    PeakmeterPanel: TBCPanel;
    PopupMenuStationList: TPopupMenu;
    SearchPanel: TBCPanel;
    BottomFunctionPanel: TBCPanel;
    TopInfoPanel: TBCPanel;
    MainMenu1: TMainMenu;
    miLanguage: TMenuItem;
    miSettings: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    StationListPanel: TBCPanel;
    pbLeftLevelMeter: TBGRAFlashProgressBar;
    btnPlay: TBCButton;
    pbRightLevelMeter: TBGRAFlashProgressBar;
    Timer1: TTimer;
    procedure AboutActionExecute(Sender: TObject);
    procedure AddStationActionExecute(Sender: TObject);
    procedure BottomFunctionPanelResize(Sender: TObject);
    procedure DeleteStationActionExecute(Sender: TObject);
    procedure ExportStationsActionExecute(Sender: TObject);
    procedure MainPanelResize(Sender: TObject);
    procedure miCopyTitleToClipboardClick(Sender: TObject);
    procedure miSearchOnYoutubeClick(Sender: TObject);
    procedure miStationInfoClick(Sender: TObject);
    procedure NextActionExecute(Sender: TObject);
    procedure OpenDictionaryTablesActionExecute(Sender: TObject);
    procedure EditStationActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure miShowBothScrollBarsClick(Sender: TObject);
    procedure miShowVstColumnClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure OpenEqualizerActionExecute(Sender: TObject);
    procedure OpenUrlActionExecute(Sender: TObject);
    procedure PeakmeterPanelResize(Sender: TObject);
    procedure PlayActionExecute(Sender: TObject);
    procedure PopupMenuStationListPopup(Sender: TObject);
    procedure PrevActionExecute(Sender: TObject);
    procedure RadioPlayerRadioPlay(Sender: TObject);
    procedure RadioPlayerRadioPlayerTags(AMessage: string; APlayerMessageType: TPlayerMessageType);
    procedure StopActionExecute(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure LoadLoanguages;
    procedure LoadSettings;
    procedure LoadSkin;
    procedure AddMenuLanguageItems;
    procedure AddMenuSkinItems;
    procedure OnLanguageChange(Sender: TObject);
    procedure CreateVstStationList;
    procedure miLanguageItemClick(Sender: TObject);
    procedure miSkinsItemClick(Sender: TObject);
    procedure SearchEditDelayChange(ASender: TObject; AText: string);
    procedure TextScrollMouseEnter(Sender: TObject);
    procedure TextScrollMouseLeave(Sender: TObject);
    procedure SkinDoneStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure SkinLoaded(Sender: TObject;
      var ASkinData: TSkinData);
    procedure VolumeTrackBarPositionChange(ASender: TObject; APosition: integer);
    procedure VstStationListAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
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
    procedure VstStationListHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure VstStationListKeyPress(Sender: TObject; var Key: char);
    procedure VstStationListFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);

    procedure StationDetailManagement(OpenMode: TOpenMode; DropFileName: string = EMPTY_STR);

    procedure RadioActions(Sender: TObject);

    procedure TextScrollSetDefaultValues;
  public
    RadioPlayer: TRadioPlayer;

    VstStationList: TVirtualStringTree;

    TextScroll: TCTRPTextScroll;
    VolumeTrackBar: TCTRPTrackBar;

    SearchEdit: TCTRPEdit;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Language, TRPSettings, Repository, StationDetailFormUnit, DictionaryTablesManagementFormUnit,
  LCLIntf, Clipbrd, EqualizerFormUnit, AboutFormUnit;

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MainWindowHandle := Handle;

  RadioPlayer := TRadioPlayer.Create;
  RadioPlayer.OnRadioPlayerTags := @RadioPlayerRadioPlayerTags;
  RadioPlayer.OnRadioPlay := @RadioPlayerRadioPlay;

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
  TextScrollSetDefaultValues;
  TextScroll.OnMouseEnter := @TextScrollMouseEnter;
  TextScroll.OnMouseLeave := @TextScrollMouseLeave;

  TextScroll.Align := alClient;
  TextScroll.PopupMenu := PopupMenuTextScroll;

  LoadSettings;

  CreateVstStationList;

  // Create SearchEdit
  SearchEdit := TCTRPEdit.Create(Self);
  SearchEdit.Parent := SearchPanel;
  SearchEdit.Left := 293;
  SearchEdit.Top := 8;
  SearchEdit.Width := 184;
  SearchEdit.Height := 23;
  SearchEdit.Anchors := [akTop, akRight];
  SearchEdit.OnDelayChange := @SearchEditDelayChange;

  LoadSkin;

  LoadLoanguages;

  AddMenuLanguageItems;
  AddMenuSkinItems;

  TLanguage.RegisterLanguageChangeEvent(@OnLanguageChange);

  TRepository.LoadStations(VstStationList, SearchEdit.Text);

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

procedure TMainForm.OpenEqualizerActionExecute(Sender: TObject);
begin
  if not Assigned(EqualizerForm) then
  begin
    EqualizerForm := TEqualizerForm.Create(Self, RadioPlayer);
    try
      if EqualizerForm.ShowModal = mrOK then
      begin

      end;
    finally
      FreeAndNil(EqualizerForm);
    end;
  end;
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

procedure TMainForm.PlayActionExecute(Sender: TObject);
begin
  RadioActions(Sender);
end;

procedure TMainForm.PrevActionExecute(Sender: TObject);
begin
  RadioActions(Sender);
end;

procedure TMainForm.NextActionExecute(Sender: TObject);
begin
  RadioActions(Sender);
end;

procedure TMainForm.StopActionExecute(Sender: TObject);
begin
  RadioPlayer.Stop();
end;

procedure TMainForm.RadioActions(Sender: TObject);
var
  node: PVirtualNode;
begin
  if Sender is TAction then
  begin

    node := VstStationList.GetFirstSelected();

    if node = nil then
      node := VstStationList.GetFirst()
    else if Sender = NextAction then
       node := VstStationList.GetNext(node)
    else if Sender = PrevAction then
       node := VstStationList.GetPrevious(node);

    if node <> nil then
      VstStationList.Selected[node] := true;

    RadioPlayer.PlayStation(
      RadioPlayer.GetSelectedStationId(VstStationList),
      VolumeTrackBar.Position);
  end;
end;

procedure TMainForm.TextScrollSetDefaultValues;
var
  appInfo: TApplicationInfo;
begin
  appInfo := GetApplicationInfo;

  TextScroll.Lines.TextScrollLine1.ScrollText := appInfo.ProductName;
  TextScroll.Lines.TextScrollLine2.ScrollText := appInfo.FileVersion;
end;

procedure TMainForm.PopupMenuStationListPopup(Sender: TObject);
var
  stationSelected: boolean;
begin
  stationSelected := VstStationList.GetFirstSelected() <> nil;

  miEditStation.Enabled := stationSelected;
  miDeleteStation.Enabled := stationSelected;
end;

procedure TMainForm.BottomFunctionPanelResize(Sender: TObject);
var
  panelCenter: integer;
  buttonCount: byte;
  buttonWidth: byte;
  buttonSpace: byte;
  button7: integer;
begin
  buttonCount := 6;
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

  btnPrev.Left := button7 - (6 * buttonWidth) - (5 * buttonSpace);
  btnPlay.Left := button7 - (5 * buttonWidth) - (4 * buttonSpace);
  btnStop.Left := button7 - (4 * buttonWidth) - (3 * buttonSpace);
  btnNext.Left := button7 - (3 * buttonWidth) - (2 * buttonSpace);
  btnRec.Left := button7 - (2 * buttonWidth) - buttonSpace;
  btnOpen.Left := button7 - buttonWidth;

end;

procedure TMainForm.AddStationActionExecute(Sender: TObject);
begin
  StationDetailManagement(TOpenMode.omNew);
end;

procedure TMainForm.AboutActionExecute(Sender: TObject);
begin
  if not Assigned(AboutForm) then
  begin
    AboutForm := TAboutForm.Create(Self);
    try
      AboutForm.ShowModal;
    finally
      FreeAndNil(AboutForm);
    end;

  end;
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

procedure TMainForm.ExportStationsActionExecute(Sender: TObject);
var
  exportImport: TExportImport;
  saveDialog: TSaveDialog;
begin

  saveDialog := TSaveDialog.Create(self);
  try
    saveDialog.Title :=
      GetLanguageItem('ExportStations.Dialog.Title', 'Export the stations list to a file');

    saveDialog.InitialDir := GetCurrentDir;
    saveDialog.Filter := 'JSON files|*.json|All|*.*';
    saveDialog.DefaultExt := 'json';
    saveDialog.FileName := 'TRPStations';
    saveDialog.FilterIndex := 1;

    if saveDialog.Execute then
    begin
      exportImport := TExportImport.Create();
      try
        exportImport.ExportToJsonFile(saveDialog.Filename);
      finally
        exportImport.Free;
      end;
    end;
  finally
    saveDialog.Free;
  end;

end;

procedure TMainForm.MainPanelResize(Sender: TObject);
begin
  VstStationList.Left := 1;
  VstStationList.Top := 1;
  VstStationList.Width := MainPanel.Width - 2;
  VstStationList.Height := MainPanel.Height - 2;
end;

procedure TMainForm.miCopyTitleToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := TextScroll.Lines.TextScrollLine1.ScrollText;
end;

procedure TMainForm.miSearchOnYoutubeClick(Sender: TObject);
begin
  OpenURL(
    Format(YOUTUBE_SEARCH_PATH,
      [TextScroll.Lines.TextScrollLine1.ScrollText]));
end;

procedure TMainForm.miStationInfoClick(Sender: TObject);
begin
  if Assigned(StationDetailForm) or RadioPlayer.NoCurrentStationLoaded then Exit;

  StationDetailForm := TStationDetailForm.Create(Self, omNormal,
    RadioPlayer.CurrentStationId);
  try

    if StationDetailForm.ShowModal = mrOK then
    begin

    end;

  finally
    FreeAndNil(StationDetailForm);
  end;
end;

procedure TMainForm.OpenDictionaryTablesActionExecute(Sender: TObject);
var
  mr: TModalResult;
begin
  if not Assigned(DictionaryTablesManagementForm) then
  begin
    DictionaryTablesManagementForm := TDictionaryTablesManagementForm.Create(Self);
    try
      mr := DictionaryTablesManagementForm.ShowModal;


    finally
      FreeAndNil(DictionaryTablesManagementForm);
    end;

  end;
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
  miExportStations.Caption := GetLanguageItem('MainMenu.File.ExportStations', 'Export Stations');
  miExit.Caption := GetLanguageItem('MainMenu.File.Exit', 'Exit');
  miSettings.Caption := GetLanguageItem('MainMenu.Settings', 'Settings');
  miLanguage.Caption := GetLanguageItem('MainMenu.Settings.Language', 'Language');
  miSkins.Caption := GetLanguageItem('MainMenu.Settings.Skins', 'Skins');
  miDictionaryTables.Caption := GetLanguageItem('MainMenu.Settings.DictionaryTables', 'Dictionary Tables');
  miEqualizer.Caption := GetLanguageItem('MainMenu.Settings.Equalizer', 'Equalizer');
  miAbout.Caption := GetLanguageItem('MainMenu.About', 'About');

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

  miStationInfo.Caption :=
    GetLanguageItem('MainForm.TopTextScroll.PopupMenu.StationInfo', 'Station Info');
  miSearchOnYoutube.Caption :=
    GetLanguageItem('MainForm.TopTextScroll.PopupMenu.SearchOnYoutube', 'Search on YouTube');
  miCopyTitleToClipboard.Caption :=
    GetLanguageItem('MainForm.TopTextScroll.PopupMenu.CopyTitleToClipboard', 'Copy Title to Clipboard');
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

procedure TMainForm.AddMenuLanguageItems;
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

procedure TMainForm.AddMenuSkinItems;
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
  // Requires correctly loaded settings

  VstStationList := TVirtualStringTree.Create(Self);

  VstStationList.Name := 'StationList';
  VstStationList.Parent := MainPanel;

  VstStationList.Align := alCustom;

  VstStationList.DefaultNodeHeight := 20;
  VstStationList.SelectionCurveRadius := 0;
  VstStationList.TabOrder := 0;

  VstStationList.BorderStyle := bsNone;

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
  VstStationList.OnHeaderDrawQueryElements := @VstStationListHeaderDrawQueryElements;
  VstStationList.OnAdvancedHeaderDraw := @VstStationListAdvancedHeaderDraw;

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
  VstStationList.TreeOptions.PaintOptions := [toHideFocusRect, toThemeAware, toUseBlendedImages];
  VstStationList.TreeOptions.SelectionOptions := [toDisableDrawSelection, toExtendedFocus, toFullRowSelect, toMiddleClickSelect, toRightClickSelect];  // toCenterScrollIntoView
  VstStationList.TreeOptions.StringOptions := [toSaveCaptions];

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
end;

procedure TMainForm.TextScrollMouseEnter(Sender: TObject);
begin
  TextScroll.Lines.TextScrollLine1.BackgroundColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.BackgroundColor.OnMouseEnter');
  TextScroll.Lines.TextScrollLine2.BackgroundColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.BackgroundColor.OnMouseEnter');

  TextScroll.Lines.TextScrollLine1.FontColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.FontColor.OnMouseEnter');
  TextScroll.Lines.TextScrollLine2.FontColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.FontColor.OnMouseEnter');

  TextScroll.Lines.TextScrollLine1.Border.BorderLeft.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.Border.BorderLeft.BorderColor.OnMouseEnter');
  TextScroll.Lines.TextScrollLine1.Border.BorderTop.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.Border.BorderTop.BorderColor.OnMouseEnter');
  TextScroll.Lines.TextScrollLine1.Border.BorderRight.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.Border.BorderRight.BorderColor.OnMouseEnter');
  TextScroll.Lines.TextScrollLine1.Border.BorderBottom.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.Border.BorderBottom.BorderColor.OnMouseEnter');

  TextScroll.Lines.TextScrollLine2.Border.BorderLeft.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.Border.BorderLeft.BorderColor.OnMouseEnter');
  TextScroll.Lines.TextScrollLine2.Border.BorderTop.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.Border.BorderTop.BorderColor.OnMouseEnter');
  TextScroll.Lines.TextScrollLine2.Border.BorderRight.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.Border.BorderRight.BorderColor.OnMouseEnter');
  TextScroll.Lines.TextScrollLine2.Border.BorderBottom.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.Border.BorderBottom.BorderColor.OnMouseEnter');
end;

procedure TMainForm.TextScrollMouseLeave(Sender: TObject);
begin
  TextScroll.Lines.TextScrollLine1.BackgroundColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.BackgroundColor');
  TextScroll.Lines.TextScrollLine2.BackgroundColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.BackgroundColor');

  TextScroll.Lines.TextScrollLine1.FontColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.FontColor');
  TextScroll.Lines.TextScrollLine2.FontColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.FontColor');

  TextScroll.Lines.TextScrollLine1.Border.BorderLeft.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.Border.BorderLeft.BorderColor');
  TextScroll.Lines.TextScrollLine1.Border.BorderTop.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.Border.BorderTop.BorderColor');
  TextScroll.Lines.TextScrollLine1.Border.BorderRight.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.Border.BorderRight.BorderColor');
  TextScroll.Lines.TextScrollLine1.Border.BorderBottom.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.Border.BorderBottom.BorderColor');

  TextScroll.Lines.TextScrollLine2.Border.BorderLeft.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.Border.BorderLeft.BorderColor');
  TextScroll.Lines.TextScrollLine2.Border.BorderTop.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.Border.BorderTop.BorderColor');
  TextScroll.Lines.TextScrollLine2.Border.BorderRight.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.Border.BorderRight.BorderColor');
  TextScroll.Lines.TextScrollLine2.Border.BorderBottom.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.Border.BorderBottom.BorderColor');
end;

procedure TMainForm.VolumeTrackBarPositionChange(ASender: TObject;
  APosition: integer);
begin
  RadioPlayer.Volume(VolumeTrackBar.Position);
  TTRPSettings.SetValue('Volume', VolumeTrackBar.Position);
end;

procedure TMainForm.VstStationListAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  if hpeBackground in Elements then
  begin
    PaintInfo.TargetCanvas.Brush.Color := TSkins.GetColorItem('StationList.Header.BackgroundColor');
    PaintInfo.TargetCanvas.FillRect(PaintInfo.PaintRectangle);
  end;
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

procedure TMainForm.SearchEditDelayChange(ASender: TObject; AText: string);
begin
  TRepository.LoadStations(VstStationList, AText);
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
      ItemColor := TSkins.GetColorItem('StationList.Grid.CurrentlyPlayingStationColor');
      EraseAction := eaColor;
    end
    else
    // Coloring every second line
    if (Odd(Node^.Index)) then
    begin
      ItemColor := TSkins.GetColorItem('StationList.Grid.Every2ndLineColor');
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

procedure TMainForm.VstStationListHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  // if there is no column assigned in the paint info, elements for the background painting are requested;
  // this event fires later for plates as well, but we want to paint only the background by ourselves
  //if not Assigned(PaintInfo.Column) then
    Elements := [hpeBackground];
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
        StationDetailForm := TStationDetailForm.Create(Self, OpenMode, EMPTY_STR, DropFileName);
        try
          mr := StationDetailForm.ShowModal;

          if mr = mrOK then
            TRepository.LoadStations(VstStationList, SearchEdit.Text);
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
            TRepository.LoadStations(VstStationList, SearchEdit.Text);

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
            TRepository.LoadStations(VstStationList, SearchEdit.Text);

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
begin
  // moved to SkinLoaded
end;

procedure TMainForm.SkinLoaded(Sender: TObject; var ASkinData: TSkinData);
begin
  // VolumeTrackBar
  VolumeTrackBar.Track.TrackBackground.BackgroundColor :=
    ASkinData.GetColorItem('VolumeTrackBar.Track.TrackBackground.BackgroundColor');
  VolumeTrackBar.Track.TrackBorder.BorderTop.BorderColor :=
    ASkinData.GetColorItem('VolumeTrackBar.Track.TrackBorder.BorderTop.BorderColor');
  VolumeTrackBar.Track.TrackBorder.BorderRight.BorderColor :=
    ASkinData.GetColorItem('VolumeTrackBar.Track.TrackBorder.BorderRight.BorderColor');
  VolumeTrackBar.Track.TrackBorder.BorderBottom.BorderColor :=
    ASkinData.GetColorItem('VolumeTrackBar.Track.TrackBorder.BorderBottom.BorderColor');
  VolumeTrackBar.Track.TrackBorder.BorderLeft.BorderColor :=
    ASkinData.GetColorItem('VolumeTrackBar.Track.TrackBorder.BorderLeft.BorderColor');
  VolumeTrackBar.Thumb.ThumbFontColor :=
    ASkinData.GetColorItem('VolumeTrackBar.Thumb.ThumbFontColor');

  // Panels
  SearchPanel.Background.Color := ASkinData.GetColorItem('SearchPanel.BackgroundColor');
  StationListPanel.Background.Color := ASkinData.GetColorItem('StationListPanel.BackgroundColor');
  MainPanel.Background.Color := ASkinData.GetColorItem('MainPanel.BackgroundColor');

  // PeakmeterPanel
  PeakmeterPanel.Background.Color := ASkinData.GetColorItem('PeakmeterPanel.BackgroundColor');
  PeakmeterPanel.Border.Color := ASkinData.GetColorItem('PeakmeterPanel.BorderColor');

  // BottomFunctionPanel
  BottomFunctionPanel.Background.Gradient1.StartColor :=
    ASkinData.GetColorItem('BottomFunctionPanel.Background.Gradient1.StartColor');
  BottomFunctionPanel.Background.Gradient1.EndColor :=
    ASkinData.GetColorItem('BottomFunctionPanel.Background.Gradient1.EndColor');

  // TextScroll
  // Line 1
  TextScroll.Lines.TextScrollLine1.BackgroundColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.BackgroundColor');
  TextScroll.Lines.TextScrollLine1.FontColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.FontColor');
  TextScroll.Lines.TextScrollLine1.Border.BorderLeft.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.Border.BorderLeft.BorderColor');
  TextScroll.Lines.TextScrollLine1.Border.BorderLeft.BorderWidth :=
    TSkins.GetIntItem('TextScroll.Lines.TextScrollLine1.Border.BorderLeft.BorderWidth');
  TextScroll.Lines.TextScrollLine1.Border.BorderTop.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.Border.BorderTop.BorderColor');
  TextScroll.Lines.TextScrollLine1.Border.BorderTop.BorderWidth :=
    TSkins.GetIntItem('TextScroll.Lines.TextScrollLine1.Border.BorderTop.BorderWidth');
  TextScroll.Lines.TextScrollLine1.Border.BorderRight.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.Border.BorderRight.BorderColor');
  TextScroll.Lines.TextScrollLine1.Border.BorderRight.BorderWidth :=
    TSkins.GetIntItem('TextScroll.Lines.TextScrollLine1.Border.BorderRight.BorderWidth');
  TextScroll.Lines.TextScrollLine1.Border.BorderBottom.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine1.Border.BorderBottom.BorderColor');
  TextScroll.Lines.TextScrollLine1.Border.BorderBottom.BorderWidth :=
    TSkins.GetIntItem('TextScroll.Lines.TextScrollLine1.Border.BorderBottom.BorderWidth');
  // Line 2
  TextScroll.Lines.TextScrollLine2.BackgroundColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.BackgroundColor');
  TextScroll.Lines.TextScrollLine2.FontColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.FontColor');
  TextScroll.Lines.TextScrollLine2.Border.BorderLeft.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.Border.BorderLeft.BorderColor');
  TextScroll.Lines.TextScrollLine2.Border.BorderLeft.BorderWidth :=
    TSkins.GetIntItem('TextScroll.Lines.TextScrollLine2.Border.BorderLeft.BorderWidth');
  TextScroll.Lines.TextScrollLine2.Border.BorderTop.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.Border.BorderTop.BorderColor');
  TextScroll.Lines.TextScrollLine2.Border.BorderTop.BorderWidth :=
    TSkins.GetIntItem('TextScroll.Lines.TextScrollLine2.Border.BorderTop.BorderWidth');
  TextScroll.Lines.TextScrollLine2.Border.BorderRight.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.Border.BorderRight.BorderColor');
  TextScroll.Lines.TextScrollLine2.Border.BorderRight.BorderWidth :=
    TSkins.GetIntItem('TextScroll.Lines.TextScrollLine2.Border.BorderRight.BorderWidth');
  TextScroll.Lines.TextScrollLine2.Border.BorderBottom.BorderColor :=
    TSkins.GetColorItem('TextScroll.Lines.TextScrollLine2.Border.BorderBottom.BorderColor');
  TextScroll.Lines.TextScrollLine2.Border.BorderBottom.BorderWidth :=
    TSkins.GetIntItem('TextScroll.Lines.TextScrollLine2.Border.BorderBottom.BorderWidth');

  // StationList
  VstStationList.Colors.FocusedSelectionColor :=
    TSkins.GetColorItem('StationList.Grid.FocusedSelectionColor');
  VstStationList.Colors.FocusedSelectionBorderColor :=
    TSkins.GetColorItem('StationList.Grid.FocusedSelectionBorderColor');
  VstStationList.Font.Color :=
    TSkins.GetColorItem('StationList.Grid.FontColor');
  VstStationList.Color :=
    TSkins.GetColorItem('StationList.Grid.BackgroundColor');
  VstStationList.Colors.UnfocusedSelectionColor :=
    TSkins.GetColorItem('StationList.Grid.UnfocusedSelectionColor');
  VstStationList.Colors.UnfocusedSelectionBorderColor :=
    TSkins.GetColorItem('StationList.Grid.UnfocusedSelectionBorderColor');

  // SearchEdit
  SearchEdit.Color := TSkins.GetColorItem('SearchEdit.Color');
  SearchEdit.Font.Color := TSkins.GetColorItem('SearchEdit.FontColor');

  // LeftLevelMeter
  pbLeftLevelMeter.BackgroundColor := TSkins.GetColorItem('LeftLevelMeter.BackgroundColor');
  pbLeftLevelMeter.Color := TSkins.GetColorItem('LeftLevelMeter.Color');
  // RightLevelMeter
  pbRightLevelMeter.BackgroundColor := TSkins.GetColorItem('RightLevelMeter.BackgroundColor');
  pbRightLevelMeter.Color := TSkins.GetColorItem('RightLevelMeter.Color');

  // Bitmaps
  miAddStation.Bitmap.Assign(ASkinData.GetBitmapItem('btnAdd'));
  miEditStation.Bitmap.Assign(ASkinData.GetBitmapItem('btnEdit'));
  miDeleteStation.Bitmap.Assign(ASkinData.GetBitmapItem('btnDelete'));
  btnPlay.Glyph.Assign(ASkinData.GetBitmapItem('btnPlay'));
  btnPrev.Glyph.Assign(ASkinData.GetBitmapItem('btnPrev'));
  btnStop.Glyph.Assign(ASkinData.GetBitmapItem('btnStop'));
  btnNext.Glyph.Assign(ASkinData.GetBitmapItem('btnNext'));
  btnRec.Glyph.Assign(ASkinData.GetBitmapItem('btnRec'));
  btnOpen.Glyph.Assign(ASkinData.GetBitmapItem('btnOpen'));
end;

// Triggered when access to a node's data happens the first time but the actual
// data size is not set yet.
procedure TMainForm.VstStationListGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TStationNodeRec);
end;

end.

