unit DictionaryDetailFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BCPanel, BCLabel, BCButton, Forms, Controls,
  Graphics, Dialogs, ActnList, StdCtrls, RadioPlayerTypes, BaseFormUnit;

type

  { TDictionaryDetailForm }

  TDictionaryDetailForm = class(TBaseForm)
    cboParentDictionaryTable: TComboBox;
    edtDictionaryItemName: TEdit;
    edtDictionaryItemCode: TEdit;
    lblDictionaryItemName: TLabel;
    lblDictionaryItemCode: TLabel;
    lblParentDictionaryTable: TLabel;
  private
    DictionaryType: TDictionaryType;
    ParentDictionaryType: TDictionaryType;
    ParentDictionaryRowCode: string;
    DictionaryDetailTableNodeData: TDictionaryDetailTableNodeData;
  protected
    procedure LoadLanguages; override;
    procedure LoadSkins; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AOpenMode: TOpenMode;
      ADictionaryType: TDictionaryType;
      AParentDictionaryType: TDictionaryType;
      AparentDictionaryRowCode: string;
      ADictionaryDetailTableNodeData: TDictionaryDetailTableNodeData); overload;
    destructor Destroy; override;
  end;

var
  DictionaryDetailForm: TDictionaryDetailForm;

implementation

uses
  TRPErrors, Language, Helpers, Repository;

{$R *.lfm}

constructor TDictionaryDetailForm.Create(AOwner: TComponent);
begin
  RaiseErrorMessage(ERR_INCORRECT_CONSTRUCTOR, ClassName, 'Create');
end;

constructor TDictionaryDetailForm.Create(AOwner: TComponent; AOpenMode: TOpenMode;
  ADictionaryType: TDictionaryType;
  AParentDictionaryType: TDictionaryType;
  AparentDictionaryRowCode: string;
  ADictionaryDetailTableNodeData: TDictionaryDetailTableNodeData);
begin
  DictionaryType := ADictionaryType;
  ParentDictionaryType := AParentDictionaryType;
  ParentDictionaryRowCode := AparentDictionaryRowCode;
  DictionaryDetailTableNodeData := ADictionaryDetailTableNodeData;

  inherited Create(AOwner, AOpenMode);

  if (DictionaryDetailTableNodeData <> nil) and (AOpenMode <> TOpenMode.omNew) then
  begin
    edtDictionaryItemName.Text := DictionaryDetailTableNodeData.Text;
    edtDictionaryItemCode.Text := DictionaryDetailTableNodeData.Code;
  end;

  if ParentDictionaryType <> TDictionaryType.dkNone then
  begin
    TRepository.AddDictionaryItemsToComboBox(cboParentDictionaryTable, ParentDictionaryType, false);
    TRepository.FindAnItemInTheComboBox(cboParentDictionaryTable, ParentDictionaryRowCode);
  end;
end;

destructor TDictionaryDetailForm.Destroy;
begin
  inherited Destroy;
end;

procedure TDictionaryDetailForm.LoadLanguages;
begin
  inherited LoadLanguages;

  lblTitle.Caption := TRepository.GetLocalizedDictionaryName(DictionaryType);
  lblParentDictionaryTable.Caption := GetLanguageItem('DictionaryTablesManagement.Detail.ParentDictionary');
  lblDictionaryItemName.Caption := GetLanguageItem('DictionaryTablesManagement.Detail.DictionaryItemName');
  lblDictionaryItemCode.Caption := GetLanguageItem('DictionaryTablesManagement.Detail.DictionaryItemCode');

  case OpenMode of
    omNew:
    begin
      Self.Caption := GetLanguageItem('DictionaryTablesManagement.Detail.AddNewDictionaryItem');
      btnOk.Caption := GetLanguageItem('DictionaryTablesManagement.Detail.Save');
    end;

    omEdit:
    begin
      Self.Caption := GetLanguageItem('DictionaryTablesManagement.Detail.EditDictionaryItem');
      btnOk.Caption := GetLanguageItem('DictionaryTablesManagement.Detail.Save');
    end;

    omDelete:
    begin
      Self.Caption := GetLanguageItem('DictionaryTablesManagement.Detail.DeleteDictionaryItem');
      btnOk.Caption := GetLanguageItem('DictionaryTablesManagement.Detail.Delete');
    end;
  end;
end;

procedure TDictionaryDetailForm.LoadSkins;
begin
  inherited LoadSkins;

  // Set form layout only if there is no parent table, in other case do nothing
  // because everything has already been set
  if ParentDictionaryType = TDictionaryType.dkNone then
  begin
    cboParentDictionaryTable.Visible := false;
    lblParentDictionaryTable.Visible := false;

    lblDictionaryItemCode.Top := lblDictionaryItemName.Top;
    edtDictionaryItemCode.Top := edtDictionaryItemName.Top;

    lblDictionaryItemName.Top := lblParentDictionaryTable.Top;
    edtDictionaryItemName.Top := cboParentDictionaryTable.Top;

    Self.Constraints.MinHeight := 210;
    Self.Height := Self.Constraints.MinHeight;
  end;

  case FOpenMode of
    TOpenMode.omNew:
    begin
      // Default at the begining set to false
      btnOK.Enabled := False;
    end;
    TOpenMode.omEdit:
    begin
      // Default at the begining set to false
      btnOK.Enabled := False;
    end;
    TOpenMode.omDelete:
    begin
      // Default at the begining set to true
      btnOK.Enabled := True;
      edtDictionaryItemCode.Enabled := false;
      edtDictionaryItemName.Enabled := false;
    end;
  end;
end;

end.

