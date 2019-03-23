unit OpenStationUrlFormUnit;
{===============================================================================
File:                OpenStationUrlFormUnit.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Open Url

================================================================================}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BCLabel, BCButton, Forms, Controls,
  Graphics, Dialogs, ActnList, StdCtrls, BaseFormUnit;

type

  TOpenStationUrlForm = class(TBaseForm)
    edtUrl: TEdit;
    procedure btnOkClick(Sender: TObject);
  private
    function GetUrl: string;
  protected
    procedure LoadLanguages; override;
    procedure LoadSkins; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Url: string read GetUrl;
  end;

var
  OpenStationUrlForm: TOpenStationUrlForm;

implementation

uses
  TRPSettings, Helpers, RadioPlayerTypes;

{$R *.lfm}

constructor TOpenStationUrlForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, omNormal);

  edtUrl.Text := TTRPSettings.GetValue('LastOpenedStationUrl', EmptyStr);
end;

destructor TOpenStationUrlForm.Destroy;
begin
  inherited Destroy;
end;

procedure TOpenStationUrlForm.btnOkClick(Sender: TObject);
begin
  TTRPSettings.SetValue('LastOpenedStationUrl', edtUrl.Text);
  inherited;
end;

function TOpenStationUrlForm.GetUrl: string;
begin
  Result := edtUrl.Text;
end;

procedure TOpenStationUrlForm.LoadLanguages;
begin
  inherited LoadLanguages;

  Self.Caption := GetLanguageItem('OpenURL.Name', 'Open URL');

  lblTitle.Caption := GetLanguageItem('OpenURL.Title', 'Enter the URL to open:');

  btnOK.Caption := GetLanguageItem('Button.Open', 'Open');
end;

procedure TOpenStationUrlForm.LoadSkins;
begin
  inherited LoadSkins;
end;

end.

