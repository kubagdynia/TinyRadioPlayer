unit EqualizerFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BCPanel, BCLabel, BCButton, Forms, Controls,
  Graphics, Dialogs, ActnList, ExtCtrls, StdCtrls, ComCtrls, BaseFormUnit,
  RadioPlayer;

type

  TEqualizerChangeEvent = procedure(ASender: TObject; AEnabled: boolean; ABand1Pos: integer) of object;

  { TEqualizerForm }

  TEqualizerForm = class(TBaseForm)
    CheckBox1: TCheckBox;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblEq8000: TLabel;
    lblEq125: TLabel;
    lblEq1000: TLabel;
    tbEq125: TTrackBar;
    tbEq1000: TTrackBar;
    tbEq8000: TTrackBar;
    procedure CheckBox1Change(Sender: TObject);
  private
    FOnEqualizerChange: TEqualizerChangeEvent;
    FRadioPlayer: TRadioPlayer;

    procedure AddPresetNames;
  protected
    procedure LoadLanguages; override;
    procedure LoadSkins; override;
  public
    constructor Create(AOwner: TComponent; ARadioPlayer: TRadioPlayer); overload;
    destructor Destroy; override;

    property OnEqualizerChange: TEqualizerChangeEvent read FOnEqualizerChange write FOnEqualizerChange;
  end;

var
  EqualizerForm: TEqualizerForm;

implementation

{$R *.lfm}

{ TEqualizerForm }

constructor TEqualizerForm.Create(AOwner: TComponent; ARadioPlayer: TRadioPlayer);
begin
  inherited Create(AOwner);

  btnCancel.Visible := false;
  lblTitle.Visible := false;

  FRadioPlayer := ARadioPlayer;

  AddPresetNames;

end;

destructor TEqualizerForm.Destroy;
begin
  inherited Destroy;
end;

procedure TEqualizerForm.CheckBox1Change(Sender: TObject);
begin
  GroupBox1.Enabled := CheckBox1.Checked;

  if Assigned(OnEqualizerChange) then
    OnEqualizerChange(Self, GroupBox1.Enabled, tbEq8000.Position);
end;

procedure TEqualizerForm.AddPresetNames;
var
  i: integer;
  presetsCount: integer;
begin
  ComboBox1.Items.BeginUpdate;

  ComboBox1.Clear;

  presetsCount := FRadioPlayer.EqualizerPresets.Count;

  for i := 0 to presetsCount - 1 do
  begin
    ComboBox1.Items.Add(FRadioPlayer.EqualizerPresets.Keys[i]);
  end;

  ComboBox1.Items.EndUpdate;


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

