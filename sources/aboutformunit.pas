unit AboutFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BCPanel, BCLabel, BCButton, Forms, Controls,
  Graphics, Dialogs, ActnList, StdCtrls, BaseFormUnit;

type

  { TAboutForm }

  TAboutForm = class(TBaseForm)
    Label1: TLabel;
  private

  protected
    procedure LoadLanguages; override;
    procedure LoadSkins; override;

  public
    constructor Create(AOwner: TComponent); override;

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.LoadLanguages;
begin
  inherited LoadLanguages;
end;

procedure TAboutForm.LoadSkins;
begin
  inherited LoadSkins;
end;

constructor TAboutForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  btnCancel.Visible := false;
  //lblTitle.Visible := false;
end;

end.

