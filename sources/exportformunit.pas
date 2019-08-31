unit ExportFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BCPanel, BCLabel, BCButton, Forms, Controls,
  Graphics, Dialogs, ActnList, ComCtrls, StdCtrls, BaseFormUnit,
  RadioPlayerTypes, Helpers;

type

  { TExportForm }

  TExportForm = class(TBaseForm)
    btnExport: TBCButton;
    btnChooseAFile: TBCButton;
    cbExportStations: TCheckBox;
    cbExportDictionaries: TCheckBox;
    edtFilePath: TEdit;
    gbExportStatus: TGroupBox;
    gbFile: TGroupBox;
    gbToExported: TGroupBox;
    lblExportStatus: TLabel;
    procedure btnExportClick(Sender: TObject);
    procedure btnChooseAFileClick(Sender: TObject);
    procedure cbExportDictionariesChange(Sender: TObject);
    procedure cbExportStationsChange(Sender: TObject);
    procedure edtFilePathChange(Sender: TObject);
  private
    procedure ExportEnabled;
  protected
    procedure LoadLanguages; override;
    procedure LoadSkins; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AOpenMode: TOpenMode); overload;
    destructor Destroy; override;
  end;

var
  ExportForm: TExportForm;

implementation

uses
  Language, ExportImport, TRPErrors, TRPSettings;

{$R *.lfm}

{ TExportForm }

constructor TExportForm.Create(AOwner: TComponent);
begin
  Create(AOwner, omNormal);
end;

constructor TExportForm.Create(AOwner: TComponent; AOpenMode: TOpenMode);
begin
  inherited Create(AOwner, AOpenMode);
end;

destructor TExportForm.Destroy;
begin
  inherited Destroy;
end;

procedure TExportForm.btnChooseAFileClick(Sender: TObject);
var
  saveDialog: TSaveDialog;
begin
  saveDialog := TSaveDialog.Create(Self);
  try
    saveDialog.Title :=
      GetLanguageItem('ExportData.Dialog.Title', 'Select file to save data');

    saveDialog.InitialDir := GetCurrentDir;
    saveDialog.Filter :=
      TTRPSettings.GetGroupValue('FileFilter', 'ExportData', 'JSON files|*.json|All|*.*', true);
    saveDialog.DefaultExt :=
      TTRPSettings.GetGroupValue('DefaultFileExtension', 'ExportData', 'json', true);
    saveDialog.FileName :=
      TTRPSettings.GetGroupValue('DefaultFileName', 'ExportData', 'trpData', true);
    saveDialog.FilterIndex := 1;

    if saveDialog.Execute then
    begin
      edtFilePath.Text := saveDialog.FileName;
    end;
  finally
    saveDialog.Free;
  end;
end;

procedure TExportForm.cbExportDictionariesChange(Sender: TObject);
begin
  ExportEnabled;
end;

procedure TExportForm.cbExportStationsChange(Sender: TObject);
begin
  ExportEnabled;
end;

procedure TExportForm.edtFilePathChange(Sender: TObject);
begin
  ExportEnabled;
end;

procedure TExportForm.ExportEnabled;
begin
  btnExport.Enabled := (cbExportStations.Checked or cbExportDictionaries.Checked) and (Trim(edtFilePath.Text) <> EmptyStr);
end;

procedure TExportForm.btnExportClick(Sender: TObject);
var
  exportImport: TExportImport;
  exportStatus: ErrorId;
begin
  if Trim(edtFilePath.Text) <> EmptyStr then
  begin
    exportImport := TExportImport.Create();
    try
      exportStatus := exportImport.ExportToJsonFile(edtFilePath.Text,
        cbExportStations.Checked, cbExportDictionaries.Checked);

      if exportStatus = ERR_OK then
        lblExportStatus.Caption :=
          GetLanguageItem('ExportData.ExportStatus.Success', 'SUCCESS')
      else
        lblExportStatus.Caption :=
          GetLanguageItem('ExportData.ExportStatus.Failed', 'FAILED')

    finally
      exportImport.Free;
    end;
  end;
end;

procedure TExportForm.LoadLanguages;
begin
  inherited LoadLanguages;

  Self.Caption := GetLanguageItem('ExportData.WindowName', 'Export data');
  lblTitle.Caption := GetLanguageItem('ExportData.Title', 'Export data to json file');
  btnExport.Caption := GetLanguageItem('ExportData.Button.Export', 'Export');
  gbFile.Caption := GetLanguageItem('ExportData.GroupBox.File', 'Choose a file');
  gbExportStatus.Caption := GetLanguageItem('ExportData.GroupBox.ExportStatus', 'Export status');
  gbToExported.Caption := GetLanguageItem('ExportData.GroupBox.ToExported', 'Select data to export');
  cbExportStations.Caption := GetLanguageItem('ExportData.GroupBox.ToExported.Stations', 'Stations');
  cbExportDictionaries.Caption := GetLanguageItem('ExportData.GroupBox.ToExported.Dictionaries', 'Dictionaries');
end;

procedure TExportForm.LoadSkins;
begin
  inherited LoadSkins;
end;

end.

