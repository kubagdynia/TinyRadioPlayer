unit EqualizerFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BCPanel, BCLabel, BCButton, Forms, Controls,
  Graphics, Dialogs, ActnList, ExtCtrls, StdCtrls, ComCtrls, BaseFormUnit,
  RadioPlayer;

type

  { TEqualizerForm }

  TEqualizerForm = class(TBaseForm)
    cbOnOff: TCheckBox;
    GroupBox1: TGroupBox;
    cboPresets: TComboBox;
    lblPos2: TLabel;
    lblPos1: TLabel;
    lblPos7: TLabel;
    lblPos8: TLabel;
    lblPos6: TLabel;
    lblGain1: TLabel;
    lblGain5: TLabel;
    lblGain3: TLabel;
    lblGain2: TLabel;
    lblGain4: TLabel;
    lblEqBand4Center: TLabel;
    lblEqBand5Center: TLabel;
    lblEqBand6Center: TLabel;
    lblEqBand7Center: TLabel;
    lblEqBand8Center: TLabel;
    lblPos5: TLabel;
    lblPos4: TLabel;
    lblPos3: TLabel;
    lblPresets: TLabel;
    lblEqBand3Center: TLabel;
    lblEqBand1Center: TLabel;
    lblEqBand2Center: TLabel;
    tbEqBand1Gain: TTrackBar;
    tbEqBand2Gain: TTrackBar;
    tbEqBand3Gain: TTrackBar;
    tbEqBand4Gain: TTrackBar;
    tbEqBand5Gain: TTrackBar;
    tbEqBand6Gain: TTrackBar;
    tbEqBand7Gain: TTrackBar;
    tbEqBand8Gain: TTrackBar;
    procedure cbOnOffChange(Sender: TObject);
    procedure cboPresetsChange(Sender: TObject);
    procedure tbEqBand1GainChange(Sender: TObject);
    procedure tbEqBand2GainChange(Sender: TObject);
    procedure tbEqBand3GainChange(Sender: TObject);
    procedure tbEqBand4GainChange(Sender: TObject);
    procedure tbEqBand5GainChange(Sender: TObject);
    procedure tbEqBand6GainChange(Sender: TObject);
    procedure tbEqBand7GainChange(Sender: TObject);
    procedure tbEqBand8GainChange(Sender: TObject);
  private
    FRadioPlayer: TRadioPlayer;

    FChangeEventsEnabled: boolean;

    procedure ConfigEqualizer;
    procedure ConfigEqualizerLabel(BandCenter: integer; BandCenterLabel: TLabel);
    procedure ConfigEqualizerValues(PresetName: string);
    procedure AddPresetNames;

    procedure SetLabelsPOsition(ATrackBar: TTrackBar; ALabel: TLabel);
  protected
    procedure LoadLanguages; override;
    procedure LoadSkins; override;
  public
    constructor Create(AOwner: TComponent; ARadioPlayer: TRadioPlayer); overload;
    destructor Destroy; override;
  end;

var
  EqualizerForm: TEqualizerForm;

implementation

uses
  TRPSettings;

{$R *.lfm}

{ TEqualizerForm }

constructor TEqualizerForm.Create(AOwner: TComponent; ARadioPlayer: TRadioPlayer);
begin
  FChangeEventsEnabled := false;

  inherited Create(AOwner);

  btnCancel.Visible := false;
  lblTitle.Visible := false;

  FRadioPlayer := ARadioPlayer;

  AddPresetNames;

  cbOnOff.Checked :=  FRadioPlayer.EqualizerConfig.Enabled;

  ConfigEqualizer;
  ConfigEqualizerValues(cboPresets.Text);

  FChangeEventsEnabled := true;
end;

destructor TEqualizerForm.Destroy;
begin
  inherited Destroy;
end;

procedure TEqualizerForm.cbOnOffChange(Sender: TObject);
begin
  if FChangeEventsEnabled then
  begin
    if cbOnOff.Checked then
      FRadioPlayer.EqualizerEnable
    else
      FRadioPlayer.EqualizerDisable;
  end;

  GroupBox1.Enabled := cbOnOff.Checked;

  lblPresets.Enabled := cbOnOff.Checked;
  cboPresets.Enabled := cbOnOff.Checked;
end;

procedure TEqualizerForm.cboPresetsChange(Sender: TObject);
begin
  if not FChangeEventsEnabled then exit;

  FRadioPlayer.EqualizerConfig.DefaultPreset := cboPresets.Text;
  ConfigEqualizerValues(cboPresets.Text);
end;

procedure TEqualizerForm.tbEqBand1GainChange(Sender: TObject);
begin
  SetLabelsPOsition(tbEqBand1Gain, lblPos1);

  if not FChangeEventsEnabled then exit;

  FRadioPlayer.UpdateEqualizerPreset(cboPresets.Text, 1, tbEqBand1Gain.Position);
end;

procedure TEqualizerForm.tbEqBand2GainChange(Sender: TObject);
begin
  SetLabelsPOsition(tbEqBand2Gain, lblPos2);

  if not FChangeEventsEnabled then exit;

  FRadioPlayer.UpdateEqualizerPreset(cboPresets.Text, 2, tbEqBand2Gain.Position);
end;

procedure TEqualizerForm.tbEqBand3GainChange(Sender: TObject);
begin
  SetLabelsPOsition(tbEqBand3Gain, lblPos3);

  if not FChangeEventsEnabled then exit;

  FRadioPlayer.UpdateEqualizerPreset(cboPresets.Text, 3, tbEqBand3Gain.Position);
end;

procedure TEqualizerForm.tbEqBand4GainChange(Sender: TObject);
begin
  SetLabelsPOsition(tbEqBand4Gain, lblPos4);

  if not FChangeEventsEnabled then exit;

  FRadioPlayer.UpdateEqualizerPreset(cboPresets.Text, 4, tbEqBand4Gain.Position);
end;

procedure TEqualizerForm.tbEqBand5GainChange(Sender: TObject);
begin
  SetLabelsPOsition(tbEqBand5Gain, lblPos5);

  if not FChangeEventsEnabled then exit;

  FRadioPlayer.UpdateEqualizerPreset(cboPresets.Text, 5, tbEqBand5Gain.Position);
end;

procedure TEqualizerForm.tbEqBand6GainChange(Sender: TObject);
begin
  SetLabelsPOsition(tbEqBand6Gain, lblPos6);

  if not FChangeEventsEnabled then exit;

  FRadioPlayer.UpdateEqualizerPreset(cboPresets.Text, 6, tbEqBand6Gain.Position);
end;

procedure TEqualizerForm.tbEqBand7GainChange(Sender: TObject);
begin
  SetLabelsPOsition(tbEqBand7Gain, lblPos7);

  if not FChangeEventsEnabled then exit;

  FRadioPlayer.UpdateEqualizerPreset(cboPresets.Text, 7, tbEqBand7Gain.Position);
end;

procedure TEqualizerForm.tbEqBand8GainChange(Sender: TObject);
begin
  SetLabelsPOsition(tbEqBand8Gain, lblPos8);

  if not FChangeEventsEnabled then exit;

  FRadioPlayer.UpdateEqualizerPreset(cboPresets.Text, 8, tbEqBand8Gain.Position);
end;

procedure TEqualizerForm.SetLabelsPOsition(ATrackBar: TTrackBar; ALabel: TLabel);
var
  positions: integer;
  heightTrackBar: integer;
begin
  heightTrackBar := ATrackBar.Height - ATrackBar.Top - 4;
  positions := ATrackBar.Max - ATrackBar.Min + 1;

  ALabel.Caption := IntToStr(ATrackBar.Position);

  ALabel.Left := ATrackBar.Left - ALabel.Width;

  ALabel.Top :=  round((heightTrackBar / 2) - (ATrackBar.Position * (heightTrackBar div positions)) + ATrackBar.Top);
end;

procedure TEqualizerForm.ConfigEqualizer;
begin
  ConfigEqualizerLabel(FRadioPlayer.EqualizerConfig.Band1Center, lblEqBand1Center);
  ConfigEqualizerLabel(FRadioPlayer.EqualizerConfig.Band2Center, lblEqBand2Center);
  ConfigEqualizerLabel(FRadioPlayer.EqualizerConfig.Band3Center, lblEqBand3Center);
  ConfigEqualizerLabel(FRadioPlayer.EqualizerConfig.Band4Center, lblEqBand4Center);
  ConfigEqualizerLabel(FRadioPlayer.EqualizerConfig.Band5Center, lblEqBand5Center);
  ConfigEqualizerLabel(FRadioPlayer.EqualizerConfig.Band6Center, lblEqBand6Center);
  ConfigEqualizerLabel(FRadioPlayer.EqualizerConfig.Band7Center, lblEqBand7Center);
  ConfigEqualizerLabel(FRadioPlayer.EqualizerConfig.Band8Center, lblEqBand8Center);
end;

procedure TEqualizerForm.ConfigEqualizerLabel(BandCenter: integer; BandCenterLabel: TLabel);
var
  khz: string = 'kHz';
  hz: string = 'Hz';
  threshold: integer = 1000;
begin
  if BandCenter >= threshold then
    BandCenterLabel.Caption := IntToStr(BandCenter div threshold) + khz
  else
    BandCenterLabel.Caption := IntToStr(BandCenter) + hz;
end;

procedure TEqualizerForm.ConfigEqualizerValues(PresetName: string);
begin
  tbEqBand1Gain.Position := FRadioPlayer.EqualizerPresets[PresetName].Band1Gain;
  tbEqBand2Gain.Position := FRadioPlayer.EqualizerPresets[PresetName].Band2Gain;
  tbEqBand3Gain.Position := FRadioPlayer.EqualizerPresets[PresetName].Band3Gain;
  tbEqBand4Gain.Position := FRadioPlayer.EqualizerPresets[PresetName].Band4Gain;
  tbEqBand5Gain.Position := FRadioPlayer.EqualizerPresets[PresetName].Band5Gain;
  tbEqBand6Gain.Position := FRadioPlayer.EqualizerPresets[PresetName].Band6Gain;
  tbEqBand7Gain.Position := FRadioPlayer.EqualizerPresets[PresetName].Band7Gain;
  tbEqBand8Gain.Position := FRadioPlayer.EqualizerPresets[PresetName].Band8Gain;
end;

procedure TEqualizerForm.AddPresetNames;
var
  i: integer;
  presetsCount: integer;
  presetName: string;
begin
  cboPresets.Items.BeginUpdate;

  cboPresets.Clear;

  presetsCount := FRadioPlayer.EqualizerPresets.Count;

  for i := 0 to presetsCount - 1 do
  begin
    presetName := FRadioPlayer.EqualizerPresets.Keys[i];
    cboPresets.Items.Add(presetName);

    if presetName = FRadioPlayer.EqualizerConfig.DefaultPreset then
      cboPresets.ItemIndex := i;
  end;

  cboPresets.Items.EndUpdate;
end;

procedure TEqualizerForm.LoadLanguages;
begin
  inherited LoadLanguages;
end;

procedure TEqualizerForm.LoadSkins;
begin
  inherited LoadSkins;
end;

end.

