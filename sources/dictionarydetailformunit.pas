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
    edtDictionaryItemPosition: TEdit;
    edtDictionaryItemName: TEdit;
    edtDictionaryItemCode: TEdit;
    lblDictionaryItemPosition: TLabel;
    lblDictionaryItemName: TLabel;
    lblDictionaryItemCode: TLabel;
    lblParentDictionaryTable: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure edtDictionaryItemPositionChange(Sender: TObject);
    procedure edtDictionaryItemPositionKeyPress(Sender: TObject; var Key: char);
    procedure ValidateEnteredData(Sender: TObject);
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
  LCLType, TRPErrors, Language, Helpers, Repository;

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
    edtDictionaryItemPosition.Text := IntToStr(DictionaryDetailTableNodeData.Position);
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
  lblDictionaryItemPosition.Caption := GetLanguageItem('DictionaryTablesManagement.Detail.DictionaryItemPosition');

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

    lblDictionaryItemPosition.Top := lblDictionaryItemCode.Top;
    edtDictionaryItemPosition.Top := edtDictionaryItemCode.Top;

    lblDictionaryItemCode.Top := lblDictionaryItemName.Top;
    edtDictionaryItemCode.Top := edtDictionaryItemName.Top;

    lblDictionaryItemName.Top := lblParentDictionaryTable.Top;
    edtDictionaryItemName.Top := cboParentDictionaryTable.Top;

    Self.Constraints.MinHeight := 265;
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
      edtDictionaryItemPosition.Enabled := false;
    end;
  end;
end;

procedure TDictionaryDetailForm.ValidateEnteredData(Sender: TObject);
var
  v: integer;
begin
  btnOk.Enabled := (Trim(edtDictionaryItemName.Text) <> EmptyStr) and
    (Trim(edtDictionaryItemCode.Text) <> EmptyStr) and
    (TryStrToInt(edtDictionaryItemPosition.Text, v)) and (v >= 0);
end;

procedure TDictionaryDetailForm.btnOkClick(Sender: TObject);
var
  dictionaryRowId: integer;
  err: ErrorId;
begin
  case FOpenMode of
    TOpenMode.omNew:
      err := TRepository.AddDictionaryRow(
        edtDictionaryItemName.Text,
        edtDictionaryItemCode.Text,
        StrToInt(edtDictionaryItemPosition.Text),
        TRepository.GetDictionaryName(DictionaryType),
        ParentDictionaryRowCode,
        dictionaryRowId);

    TOpenMode.omEdit:
      err := TRepository.UpdateDictionaryRow(
        edtDictionaryItemName.Text,
        edtDictionaryItemCode.Text,
        StrToInt(edtDictionaryItemPosition.Text),
        TRepository.GetDictionaryName(DictionaryType),
        ParentDictionaryRowCode,
        DictionaryDetailTableNodeData.ID);

    TOpenMode.omDelete:
    begin

    end;
  end;

  if err <> ERR_OK then
    ShowWarningMessage(err)
  else
    inherited;
end;

procedure TDictionaryDetailForm.edtDictionaryItemPositionChange(Sender: TObject);
var
  v: integer;
begin
  if (not TryStrToInt(edtDictionaryItemPosition.Text, v)) and
     (edtDictionaryItemPosition.Text <> EmptyStr ) then
    edtDictionaryItemPosition.Text  := '1'; // default value

  ValidateEnteredData(Sender);
end;

procedure TDictionaryDetailForm.edtDictionaryItemPositionKeyPress(
  Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', Char(VK_BACK), Char(VK_DELETE)]) then Key := #0;
end;

end.

