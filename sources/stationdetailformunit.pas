unit StationDetailFormUnit;
{===============================================================================
File:                StationDetailFormUnit.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Station details management (add, edit and delete)

================================================================================}

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
    procedure btnOkClick(Sender: TObject);
    procedure btnWWWOpenClick(Sender: TObject);
    procedure MainPanelResize(Sender: TObject);
    procedure ValidateEnteredData(Sender: TObject);
  private
    FStationId: string;

    procedure LoadStationData(AStationId: string);

    function AddStation: ErrorId;
    function UpdateStation: ErrorId;
    function DeleteStation: ErrorId;

    function ConfirmBeforeDeletingTheStation: boolean;
  protected
    procedure LoadLanguages; override;
    procedure LoadSkins; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AOpenMode: TOpenMode;
      StationId: string = EMPTY_STR; DroppedFileName: string = EMPTY_STR); overload;
    destructor Destroy; override;
  end;

var
  StationDetailForm: TStationDetailForm;

implementation

uses
  Helpers, Repository, TRPErrors, LCLIntf;

{$R *.lfm}

{ TStationDetailForm }

constructor TStationDetailForm.Create(AOwner: TComponent);
begin
  Create(AOwner, omNormal);
end;

constructor TStationDetailForm.Create(AOwner: TComponent; AOpenMode: TOpenMode;
  StationId: string; DroppedFileName: string);
var
  err: ErrorId;
begin
  inherited Create(AOwner, AOpenMode);

  FStationId := StationId;

  err := TRepository.LoadDictionary(TDictionaryType.dkGenre);

  if err = ERR_OK then
    err := TRepository.LoadDictionary(TDictionaryType.dkCountry);

  if err = ERR_OK then
    err := TRepository.AddDictionaryItemsToComboBox(cboGenre, TDictionaryType.dkGenre, true);

  if err = ERR_OK then
    err := TRepository.AddDictionaryItemsToComboBox(cboCountry, TDictionaryType.dkCountry, true);

  case AOpenMode of
    omNew:
    begin
      btnOk.Enabled := false;
    end;

    omEdit:
    begin
      LoadStationData(FStationId);
      btnOk.Enabled := false;
    end;

    omDelete:
    begin
      LoadStationData(FStationId);
      btnOk.Enabled := true;
      edtStationName.Enabled := false;
      edtStreamUrl.Enabled := false;
      edtWebpageUrl.Enabled := false;
      mmoDescription.Enabled := false;
      cboGenre.Enabled := false;
      cboCountry.Enabled := false;
    end;

    omNormal:
    begin
      LoadStationData(FStationId);
      btnOk.Enabled := true;
      btnCancel.Visible := false;
      edtStationName.Enabled := false;
      edtStreamUrl.Enabled := false;
      edtWebpageUrl.Enabled := false;
      mmoDescription.Enabled := false;
      cboGenre.Enabled := false;
      cboCountry.Enabled := false;
    end;

  end;

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

procedure TStationDetailForm.btnWWWOpenClick(Sender: TObject);
begin
  if Trim(edtWebpageUrl.Text) = EMPTY_STR then
    Exit;

  OpenUrl(edtWebpageUrl.Text);
end;

procedure TStationDetailForm.btnOkClick(Sender: TObject);
var
  err: ErrorId;
begin
  err := ERR_OK;

  Case FOpenMode of
    omNew: err := AddStation;

    omEdit: err := UpdateStation;

    omDelete:
      if ConfirmBeforeDeletingTheStation then
        err := DeleteStation;
  end;

  // error handling
  case err of
    ERR_OK,
    ERR_DB_DATA_ARE_THE_SAME_STATION_UPDATE_IS_NOT_NEEDED: inherited;

    // if error exists show error message
    ERR_DB_ADD_STATION,
    ERR_DB_UPDATE_STATION,
    ERR_DB_DELETE_STATION: ShowErrorMessage(err, ClassName, 'btnOkClick');

    ERR_DB_STATION_ALREADY_EXISTS: ShowInfoMessage(err);

    else ShowErrorMessage(err, ClassName, 'btnOkClick');
  end;
end;

procedure TStationDetailForm.LoadLanguages;
begin
  inherited LoadLanguages;

  Self.Caption := GetLanguageItem('StationDetail.WindowName', 'Station Detail');

  lblStationName.Caption := GetLanguageItem('StationDetail.StationName', 'Station Name');
  lblStreamUrl.Caption := GetLanguageItem('StationDetail.StreamUrl', 'Stream Url');
  lblWebpageUrl.Caption := GetLanguageItem('StationDetail.Webpage', 'Webpage');
  lblDescription.Caption := GetLanguageItem('StationDetail.Description', 'Description');
  lblGenre.Caption := GetLanguageItem('StationDetail.Genre', 'Genre');
  lblCountry.Caption := GetLanguageItem('StationDetail.Country', 'Country');
  btnWWWOpen.Caption := GetLanguageItem('StationDetail.Go', 'Go');

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

    omNormal:
    begin
      lblTitle.Caption := EMPTY_STR;
    end;
  end;
end;

procedure TStationDetailForm.LoadSkins;
begin
  inherited LoadSkins;
end;

procedure TStationDetailForm.LoadStationData(AStationId: string);
var
  stationInfo: TStationInfo;
  err: ErrorId;
begin
  err := TRepository.LoadStation(stationInfo, AStationId);

  if err = ERR_OK then
  begin
    edtStationName.Text := stationInfo.Name;
    edtStreamUrl.Text := stationInfo.StreamUrl;
    edtWebpageUrl.Text := stationInfo.WebpageUrl;
    mmoDescription.Text := stationInfo.Description;

    TRepository.FindAnItemInTheComboBox(cboGenre, stationInfo.GenreCode);
    TRepository.FindAnItemInTheComboBox(cboCountry, stationInfo.CountryCode);
  end;

end;

function TStationDetailForm.AddStation: ErrorId;
var
  err: ErrorId;
  stationInfo: TStationInfo;
  gCode: string;
  cCode: string;
  rCode: string;
  stationId: string;
begin
  err := ERR_OK;

  try

    err := TRepository.GetDictionaryCodeFromSelectedItem(cboGenre, gCode);
    err := TRepository.GetDictionaryCodeFromSelectedItem(cboCountry, cCode, rCode);

    if err = ERR_OK then
    begin

      with stationInfo do
      begin
        Id := EMPTY_STR;
        Name := edtStationName.Text;
        StreamUrl := edtStreamUrl.Text;
        Description := mmoDescription.Text;
        WebpageUrl := edtWebpageUrl.Text;
        GenreCode := gCode;
        CountryCode := cCode;
        RegionCode := rCode;
      end;

      err := TRepository.AddStation(stationInfo, stationId);

    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'AddStation', E);
        err := ERR_DB_ADD_STATION;
      end;
  end;

  Result := err;
end;

function TStationDetailForm.UpdateStation: ErrorId;
var
  err: ErrorId;
  stationInfo: TStationInfo;
  gCode: string;
  cCode: string;
  rCode: string;
begin
  err := ERR_OK;

  try

    err := TRepository.GetDictionaryCodeFromSelectedItem(cboGenre, gCode);
    err := TRepository.GetDictionaryCodeFromSelectedItem(cboCountry, cCode, rCode);

    if err = ERR_OK then
    begin

      with stationInfo do
      begin
        Id := FStationId;
        Name := edtStationName.Text;
        StreamUrl := edtStreamUrl.Text;
        Description := mmoDescription.Text;
        WebpageUrl := edtWebpageUrl.Text;
        GenreCode := gCode;
        CountryCode := cCode;
        RegionCode := rCode;
      end;

      err := TRepository.UpdateStation(stationInfo);

    end;

  except
    on E: Exception do
      begin
        LogException(EMPTY_STR, ClassName, 'UpdateStation', E);
        err := ERR_DB_UPDATE_STATION;
      end;
  end;

  Result := err;
end;

function TStationDetailForm.DeleteStation: ErrorId;
var
  err: ErrorId;
begin
  err := TRepository.DeleteStation(FStationId);
  Result := err;
end;

function TStationDetailForm.ConfirmBeforeDeletingTheStation: boolean;
begin
  Result := QuestionDlg(
    GetLanguageItem('Dialog.Question', 'Question'),
    GetLanguageItem('StationDetail.BeforeDeleteQuestion', 'Are you sure you want to delete this station?'),
    mtConfirmation,
    [mrYes, GetLanguageItem('Dialog.Answer.Yes', 'Yes'), mrNo, GetLanguageItem('Dialog.Answer.No', 'No'), 'IsDefault'],
    0) = mrYes;
end;

procedure TStationDetailForm.ValidateEnteredData(Sender: TObject);
begin
  btnOk.Enabled := (Trim(edtStationName.Text) <> EMPTY_STR) and (Trim(edtStreamUrl.Text) <> EMPTY_STR);

  btnWWWOpen.Enabled := Trim(edtWebpageUrl.Text) <> EMPTY_STR;
end;

end.

