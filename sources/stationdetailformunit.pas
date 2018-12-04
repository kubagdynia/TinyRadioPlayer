unit StationDetailFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BCPanel, BCLabel, BCButton, Forms, Controls,
  Graphics, Dialogs, ActnList, StdCtrls, BaseFormUnit, Consts, RadioPlayerTypes;

type

  { TStationDetailForm }

  TStationDetailForm = class(TBaseForm)
    btnWWWOpen: TBCButton;
    cboGenre: TComboBox;
    cboCountry: TComboBox;
    edtStationName: TEdit;
    edtStreamUrl: TEdit;
    edtWebpageUrl: TEdit;
    lblCountry: TLabel;
    lblGenre: TLabel;
    lblDescription: TLabel;
    lblStationName: TLabel;
    lblStreamUrl: TLabel;
    lblWebpageUrl: TLabel;
    mmoDescription: TMemo;
    procedure MainPanelResize(Sender: TObject);
  private

  protected
    procedure LoadLanguages; override;
    procedure LoadSkins; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AOpenMode: TOpenMode;
      StationId: integer = EMPTY_INT; DroppedFileName: string = EMPTY_STR); overload;
    destructor Destroy; override;
  end;

var
  StationDetailForm: TStationDetailForm;

implementation

uses
  Helpers, Repository, TRPErrors;

{$R *.lfm}

{ TStationDetailForm }

constructor TStationDetailForm.Create(AOwner: TComponent);
begin
  Create(AOwner, omNormal);
end;

constructor TStationDetailForm.Create(AOwner: TComponent; AOpenMode: TOpenMode;
  StationId: integer; DroppedFileName: string);
var
  err: ErrorId;
begin
  inherited Create(AOwner, AOpenMode);

  err := TRepository.LoadDictionary(TDictionaryKind.dkGenre);

  if err = ERR_OK then
    err := TRepository.LoadDictionary(TDictionaryKind.dkCountry);

  if err = ERR_OK then
    err := TRepository.AddDictionaryItemsToComboBox(cboGenre, TDictionaryKind.dkGenre, true);

  if err = ERR_OK then
    err := TRepository.AddDictionaryItemsToComboBox(cboCountry, TDictionaryKind.dkCountry, true);

end;

destructor TStationDetailForm.Destroy;
begin
  inherited Destroy;
end;

procedure TStationDetailForm.MainPanelResize(Sender: TObject);
var
  itemWidth: integer;
  spaceBetweenItems: integer;
  leftSpace: integer;
  rightSpace: integer;
begin
  if Sender is TCustomBCPanel then
  begin
    spaceBetweenItems := 10;
    leftSpace := 16;
    rightSpace := 13;

    itemWidth := (TCustomBCPanel(Sender).Width - leftSpace - spaceBetweenItems - rightSpace) div 2;

    cboGenre.Left := leftSpace;
    cboGenre.Width := itemWidth;

    lblCountry.Left := cboGenre.Left + cboGenre.Width + spaceBetweenItems;
    cboCountry.Left := lblCountry.Left;
    cboCountry.Width := itemWidth;
  end;
end;

procedure TStationDetailForm.LoadLanguages;
begin
  inherited LoadLanguages;

  Self.Caption := GetLanguageItem('StationDetail.WindowName', 'Station Detail');

  case OpenMode of
    omNew:
    begin
      lblTitle.Caption := GetLanguageItem('StationDetail.TitleNew', 'Add new station');
      btnOk.Caption := GetLanguageItem('Button.Save', 'Save');
    end;

    omEdit:
    begin
      lblTitle.Caption := GetLanguageItem('StationDetail.TitleEdit', 'Edit station');
      btnOk.Caption := GetLanguageItem('Button.Save', 'Save');
    end;

    omDelete:
    begin
      lblTitle.Caption := GetLanguageItem('StationDetail.TitleDelete', 'Delete station');
      btnOk.Caption := GetLanguageItem('Button.Delete', 'Delete');
    end;
  end;
end;

procedure TStationDetailForm.LoadSkins;
begin
  inherited LoadSkins;
end;

end.

