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
  Classes, SysUtils, FileUtil, BCButton, BGRAFlashProgressBar, BCLabel, Forms,
  Controls, Graphics, Dialogs, LCLType, StdCtrls, ExtCtrls, Menus, Helpers,
  RadioPlayer, RadioPlayerTypes;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnStop: TBCButton;
    MainMenu1: TMainMenu;
    miLanguage: TMenuItem;
    miSettings: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    pbLeftLevelMeter: TBGRAFlashProgressBar;
    btnPlay: TBCButton;
    edtStreamUrl: TEdit;
    lblInfo1: TLabel;
    lblInfo2: TLabel;
    pbRightLevelMeter: TBGRAFlashProgressBar;
    sbVolume: TScrollBar;
    Timer1: TTimer;
    procedure btnPlayClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure RadioPlayerRadioPlay(Sender: TObject);
    procedure RadioPlayerRadioPlayerTags(AMessage: string; APlayerMessageType: TPlayerMessageType);
    procedure sbVolumeChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure miLanguageItemClick(Sender: TObject);
  private
    procedure LoadLoanguages;
    procedure LoadSettings;
    procedure AddLanguageItems;
    procedure OnLanguageChange(Sender: TObject);
  public
    RadioPlayer: TRadioPlayer;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Language, TRPSettings, Repository;

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MainWindowHandle := Handle;

  RadioPlayer := TRadioPlayer.Create;
  RadioPlayer.OnRadioPlayerTags := @RadioPlayerRadioPlayerTags;
  RadioPlayer.OnRadioPlay := @RadioPlayerRadioPlay;

  LoadSettings;
  LoadLoanguages;
  AddLanguageItems;

  TLanguage.RegisterLanguageChangeEvent(@OnLanguageChange);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(RadioPlayer);
end;

procedure TMainForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnPlayClick(Sender: TObject);
begin
  RadioPlayer.PlayURL(edtStreamUrl.Text, sbVolume.Position);
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
  RadioPlayer.Stop();
  TRepository.DoSomething('Hello World from repo');

end;

procedure TMainForm.RadioPlayerRadioPlay(Sender: TObject);
begin

end;

procedure TMainForm.RadioPlayerRadioPlayerTags(AMessage: string;
  APlayerMessageType: TPlayerMessageType);
begin
  case APlayerMessageType of
    Connecting: begin
      lblInfo1.Caption := 'Connecting';
    end;
    Error: begin
      lblInfo1.Caption := 'Idle: ' + AMessage;
    end;
    Progress: begin
      // Buffering progress
    end;
    StreamName: begin
      lblInfo2.Caption := AMessage;
    end;
    Bitrate: begin
      // bitrate
    end;
    StreamTitle: begin
      // title name, song name
      lblInfo1.Caption := AMessage;
    end;
    Other: begin
      lblInfo2.Caption := AMessage;
    end;
  end;
end;

procedure TMainForm.sbVolumeChange(Sender: TObject);
begin
  RadioPlayer.Volume(sbVolume.Position);
  TTRPSettings.SetValue('Volume', sbVolume.Position);
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
  btnPlay.Caption := GetLanguageItem('Test.Button.Play', 'Play');
  btnStop.Caption := GetLanguageItem('Test.Button.Stop', 'Stop');

  miFile.Caption := GetLanguageItem('MainMenu.File', 'File');
  miExit.Caption := GetLanguageItem('MainMenu.File.Exit', 'Exit');
  miSettings.Caption := GetLanguageItem('MainMenu.Settings', 'Settings');
  miLanguage.Caption := GetLanguageItem('MainMenu.Settings.Language', 'Language');
end;

procedure TMainForm.LoadSettings;
begin
  sbVolume.Position := TTRPSettings.GetValue('Volume', 100);
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

procedure TMainForm.OnLanguageChange(Sender: TObject);
begin
  LoadLoanguages;
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

end.

