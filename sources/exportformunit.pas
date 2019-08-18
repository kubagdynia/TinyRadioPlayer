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
    btnExport: TButton;
    procedure btnExportClick(Sender: TObject);
  private

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
  Language, ExportImport;

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

procedure TExportForm.btnExportClick(Sender: TObject);
var
  exportImport: TExportImport;
  saveDialog: TSaveDialog;
begin
  saveDialog := TSaveDialog.Create(Self);
  try
    saveDialog.Title :=
      GetLanguageItem('ExportStations.Dialog.Title', 'Export station list');

    saveDialog.InitialDir := GetCurrentDir;
    saveDialog.Filter := 'JSON files|*.json|All|*.*';
    saveDialog.DefaultExt := 'json';
    saveDialog.FileName := 'TRPStations';
    saveDialog.FilterIndex := 1;

    if saveDialog.Execute then
    begin
      exportImport := TExportImport.Create();
      try
        exportImport.ExportToJsonFile(saveDialog.Filename);
      finally
        exportImport.Free;
      end;
    end;
  finally
    saveDialog.Free;
  end;
end;

procedure TExportForm.LoadLanguages;
begin
  inherited LoadLanguages;
end;

procedure TExportForm.LoadSkins;
begin
  inherited LoadSkins;
end;

end.

