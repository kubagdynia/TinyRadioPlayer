unit EqualizerFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BCPanel, BCLabel, BCButton, Forms, Controls,
  Graphics, Dialogs, ActnList, ExtCtrls, StdCtrls, ComCtrls, BaseFormUnit;

type

  TEqualizerChangeEvent = procedure(ASender: TObject; AEnabled: boolean; ABand1Pos: integer) of object;

  { TEqualizerForm }

  TEqualizerForm = class(TBaseForm)
    CheckBox1: TCheckBox;
    GroupBox1: TGroupBox;
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
  protected
    procedure LoadLanguages; override;
    procedure LoadSkins; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property OnEqualizerChange: TEqualizerChangeEvent read FOnEqualizerChange write FOnEqualizerChange;
  end;

var
  EqualizerForm: TEqualizerForm;

implementation

{$R *.lfm}

{ TEqualizerForm }

constructor TEqualizerForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  btnCancel.Visible := false;
  lblTitle.Visible := false;
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

procedure TEqualizerForm.LoadLanguages;
begin
  inherited LoadLanguages;
end;

procedure TEqualizerForm.LoadSkins;
begin
  inherited LoadSkins;
end;

end.

