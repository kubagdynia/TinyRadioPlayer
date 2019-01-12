unit DictionaryDetailFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  RadioPlayerTypes, BaseFormUnit;

type

  { TDictionaryDetailForm }

  TDictionaryDetailForm = class(TBaseForm)
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
  DictionaryDetailForm: TDictionaryDetailForm;

implementation

uses
  TRPErrors, Language, Helpers;

{$R *.lfm}

constructor TDictionaryDetailForm.Create(AOwner: TComponent);
begin
  RaiseErrorMessage(ERR_INCORRECT_CONSTRUCTOR, ClassName, 'Create');
end;

constructor TDictionaryDetailForm.Create(AOwner: TComponent; AOpenMode: TOpenMode);
begin
  inherited Create(AOwner, AOpenMode);

end;

destructor TDictionaryDetailForm.Destroy;
begin
  inherited Destroy;
end;

procedure TDictionaryDetailForm.LoadLanguages;
begin
  inherited LoadLanguages;

  case OpenMode of
    omNew:
    begin
      Self.Caption := GetLanguageItem('DictionaryTablesManagement.Detail.AddNewDictionaryItem');
    end;

    omEdit:
    begin
      Self.Caption := GetLanguageItem('DictionaryTablesManagement.Detail.EditDictionaryItem');
    end;

    omDelete:
    begin
      Self.Caption := GetLanguageItem('DictionaryTablesManagement.Detail.DeleteDictionaryItem');
    end;
  end;
end;

procedure TDictionaryDetailForm.LoadSkins;
begin
  inherited LoadSkins;
end;

end.

