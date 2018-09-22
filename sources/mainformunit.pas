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
  RadioPlayer;

type

  { TMainForm }

  TMainForm = class(TForm)
    pbLeftLevelMeter: TBGRAFlashProgressBar;
    btnPlay: TBCButton;
    edtStreamUrl: TEdit;
    lblInfo1: TLabel;
    lblInfo2: TLabel;
    pbRightLevelMeter: TBGRAFlashProgressBar;
    sbVolume: TScrollBar;
    Timer1: TTimer;
    procedure btnPlayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RadioPlayerRadioPlay(Sender: TObject);
    procedure RadioPlayerRadioPlayerTags(AMsg: string; AMsgNbr: byte);
    procedure sbVolumeChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public
    RadioPlayer: TRadioPlayer;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btnPlayClick(Sender: TObject);
begin
  if (btnPlay.Caption = 'Play')
  then
  begin
    RadioPlayer.PlayURL(edtStreamUrl.Text, sbVolume.Position);
    btnPlay.Caption := 'Stop';
  end else
  begin
    RadioPlayer.Stop();
    btnPlay.Caption := 'Play';
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MainWindowHandle := Handle;

  RadioPlayer := TRadioPlayer.Create;
  RadioPlayer.OnRadioPlayerTags := @RadioPlayerRadioPlayerTags;
  RadioPlayer.OnRadioPlay := @RadioPlayerRadioPlay;

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(RadioPlayer);
end;

procedure TMainForm.RadioPlayerRadioPlay(Sender: TObject);
begin

end;

procedure TMainForm.RadioPlayerRadioPlayerTags(AMsg: string; AMsgNbr: byte);
begin
  case AMsgNbr of
    0: begin
         lblInfo1.Caption := 'Connecting';
       end;
    1: begin
         lblInfo1.Caption := 'Idle: ' + AMsg;
       end;
    2: begin
         // Buffering progress
       end;
    3: begin
         // station name
         lblInfo2.Caption := AMsg;
       end;
    4: begin
         // bitrate
       end;
    7: begin
         // title name, song name
         lblInfo1.Caption := AMsg;
       end;
    8: begin
         lblInfo2.Caption := AMsg; // ICY ok
       end;
  end;
end;

procedure TMainForm.sbVolumeChange(Sender: TObject);
begin
  RadioPlayer.Volume(sbVolume.Position);
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
  end;
end;

end.

