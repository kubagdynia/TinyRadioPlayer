unit StationDetailFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, BaseFormUnit,
  Consts, RadioPlayerTypes;

type

  { TStationDetailForm }

  TStationDetailForm = class(TBaseForm)
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
  Helpers;

{$R *.lfm}

{ TStationDetailForm }

constructor TStationDetailForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, omNormal);
end;

constructor TStationDetailForm.Create(AOwner: TComponent; AOpenMode: TOpenMode;
  StationId: integer; DroppedFileName: string);
begin
  inherited Create(AOwner, AOpenMode);


end;

destructor TStationDetailForm.Destroy;
begin
  inherited Destroy;
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

