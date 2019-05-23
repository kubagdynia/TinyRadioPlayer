unit AboutFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BCPanel, BCLabel, BCButton, Forms, Controls,
  Graphics, Dialogs, ActnList, StdCtrls, BaseFormUnit;

type

  { TAboutForm }

  TAboutForm = class(TBaseForm)
    lblPoweredByValue: TLabel;
    lblPoweredBy: TLabel;
    lblPoweredByValue2: TLabel;
    lblSourceCodeValue: TLabel;
    lblSourceCode: TLabel;
    lblLicenseValue: TLabel;
    lblLicense: TLabel;
    lblVersion: TLabel;
    lblFileVersion: TLabel;
    lblVersionValue: TLabel;
    lblFileVersionValue: TLabel;
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

uses RadioPlayerTypes, Helpers;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.LoadLanguages;
begin
  inherited LoadLanguages;

  Self.Caption := GetLanguageItem('About.WindowName', 'About');

  lblVersion.Caption := GetLanguageItem('About.Version', 'Version');
  lblFileVersion.Caption := GetLanguageItem('About.FileVersion', 'File version');
  lblLicense.Caption := GetLanguageItem('About.License', 'License');
  lblSourceCode.Caption := GetLanguageItem('About.SourceCode', 'Source code');
  lblPoweredBy.Caption := GetLanguageItem('About.PoweredBy', 'Powered by');
end;

procedure TAboutForm.LoadSkins;
begin
  inherited LoadSkins;
end;

constructor TAboutForm.Create(AOwner: TComponent);
var
  appInfo: TApplicationInfo;
begin
  inherited Create(AOwner);

  btnCancel.Visible := false;

  appInfo := GetApplicationInfo;

  lblTitle.Caption := appInfo.ProductName;

  lblVersionValue.Caption := appInfo.ProductVersion;
  lblFileVersionValue.Caption := appInfo.FileVersion;
  lblLicenseValue.Caption := appInfo.LegalCopyright;

end;

end.

