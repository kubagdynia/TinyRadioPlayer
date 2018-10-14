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
  Controls, Graphics, Dialogs, LCLType, StdCtrls, ExtCtrls, Helpers,
  RadioPlayer, RadioPlayerTypes;

type

  TMainForm = class(TForm)
    btnStop: TBCButton;
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
    procedure RadioPlayerRadioPlay(Sender: TObject);
    procedure RadioPlayerRadioPlayerTags(AMessage: string; APlayerMessageType: TPlayerMessageType);
    procedure sbVolumeChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure LoadLoanguages;
    procedure LoadSettings;
  public
    RadioPlayer: TRadioPlayer;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Language, TRPSettings;

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MainWindowHandle := Handle;

  RadioPlayer := TRadioPlayer.Create;
  RadioPlayer.OnRadioPlayerTags := @RadioPlayerRadioPlayerTags;
  RadioPlayer.OnRadioPlay := @RadioPlayerRadioPlay;

  LoadSettings;
  LoadLoanguages;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(RadioPlayer);
end;

procedure TMainForm.btnPlayClick(Sender: TObject);
begin
  RadioPlayer.PlayURL(edtStreamUrl.Text, sbVolume.Position);
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
  level: DWORD;
begin

  if RadioPlayer.ChannelIsActiveAndPlaying then
  begin
    level := RadioPlayer.ChannelGetLevel;

    pbLeftLevelMeter.Value := MulDiv(100, LoWord(level), 32768);
    pbRightLevelMeter.Value := MulDiv(100, HiWord(level), 32768);
  end else if (pbLeftLevelMeter.Value <> 0) or (pbRightLevelMeter.Value <> 0) then
  begin
    pbLeftLevelMeter.Value := 0;
    pbRightLevelMeter.Value := 0;
  end;
end;

procedure TMainForm.LoadLoanguages;
begin
  btnPlay.Caption := GetLanguageItem('Test.Button.Play', 'Play');
  btnStop.Caption := GetLanguageItem('Test.Button.Stop', 'Stop');
end;

procedure TMainForm.LoadSettings;
begin
  sbVolume.Position := TTRPSettings.GetValue('Volume', 100);
end;

end.

