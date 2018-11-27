unit BaseFormUnit;
{===============================================================================
File:                BaseFormUnit.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Base form

================================================================================}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BCPanel, BCLabel, BCButton, Forms, Controls,
  Graphics, Dialogs, ActnList, RadioPlayerTypes;

type

  { TBaseForm }

  TBaseForm = class(TForm)
    CloseFormAction: TAction;
    BaseFormActionList: TActionList;
    btnCancel: TBCButton;
    btnOk: TBCButton;
    lblTitle: TBCLabel;
    BottomPanel: TBCPanel;
    MainPanel: TBCPanel;
    TitlePanel: TBCPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure CloseFormActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetSkinsPath: string;
    procedure CloseFormByCancel;
  protected
    FOpenMode: TOpenMode;

    procedure ModeNew; virtual;
    procedure ModeEdit; virtual;
    procedure ModeDelete; virtual;
    procedure ModeNormal; virtual;
    procedure LoadLanguages; virtual;
    procedure LoadSkins; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; OpenMode: TOpenMode); overload;
    destructor Destroy; override;

    property SkinsPath: string read GetSkinsPath;
  end;

implementation

uses
  Helpers, Consts;

{$R *.lfm}

{ TBaseForm }

constructor TBaseForm.Create(AOwner: TComponent);
begin
  Create(AOwner, omNormal);
end;

constructor TBaseForm.Create(AOwner: TComponent; OpenMode: TOpenMode);
begin
  inherited Create(AOwner);

  FOpenMode := OpenMode;

  case FOpenMode of
    omNew: ModeNew;
    omEdit: ModeEdit;
    omDelete: ModeDelete;
    omNormal: ModeNormal;
  end;

  // Load resources
  LoadLanguages;
  LoadSkins;
end;

destructor TBaseForm.Destroy;
begin
  inherited Destroy;
end;

procedure TBaseForm.CloseFormActionExecute(Sender: TObject);
begin
  CloseFormByCancel;
end;

procedure TBaseForm.btnCancelClick(Sender: TObject);
begin
  CloseFormByCancel;
end;

procedure TBaseForm.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TBaseForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Release form when closing
  CloseAction := caFree;
end;

procedure TBaseForm.FormCreate(Sender: TObject);
begin

end;

procedure TBaseForm.FormDestroy(Sender: TObject);
begin

end;

procedure TBaseForm.FormResize(Sender: TObject);
begin
  // Need to refresh after resize main form because on the linux we can se
  // distortion on the TBCPanel
  TitlePanel.Refresh;
  BottomPanel.Refresh;
  MainPanel.Refresh;
end;

procedure TBaseForm.FormShow(Sender: TObject);
begin

end;

function TBaseForm.GetSkinsPath: string;
begin
  Result := GetApplicationPath + SKINS_PATH;
end;

procedure TBaseForm.CloseFormByCancel;
begin
  ModalResult := mrCancel;
end;

procedure TBaseForm.ModeNew;
begin
  TitlePanel.Background.Gradient1.StartColor := RGBToColor(245, 250, 255);
  TitlePanel.Background.Gradient1.EndColor := RGBToColor(230, 240, 250);
  TitlePanel.Background.Gradient1EndPercent := 50;
  TitlePanel.Background.Gradient2.StartColor := RGBToColor(220, 230, 244);
  TitlePanel.Background.Gradient2.EndColor := RGBToColor(221, 233, 247);
end;

procedure TBaseForm.ModeEdit;
begin
  TitlePanel.Background.Gradient1.StartColor := ColorGreen;
  TitlePanel.Background.Gradient1.EndColor := ColorGreen;
  TitlePanel.Background.Gradient1EndPercent := 100;
end;

procedure TBaseForm.ModeDelete;
begin
  TitlePanel.Background.Gradient1.StartColor := ColorRed;
  TitlePanel.Background.Gradient1.EndColor := ColorRed;
  TitlePanel.Background.Gradient1EndPercent := 100;
end;

procedure TBaseForm.ModeNormal;
begin
  // Nothing for now
end;

procedure TBaseForm.LoadLanguages;
begin
  btnCancel.Caption := GetLanguageItem('Button.Cancel', 'Cancel');
end;

procedure TBaseForm.LoadSkins;
begin
  // Nothing for now (overriden in child class)
end;

end.

