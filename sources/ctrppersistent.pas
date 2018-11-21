unit CTRPPersistent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, BGRABitmapTypes;

type

  TOnPropertyChange = procedure(ASender: TObject; AData: PtrInt) of object;

  { TCTRPProperty }

  TCTRPProperty = class(TPersistent)
  private
    { Private declarations }
    FOnChange: TOnPropertyChange;
    FCallChangeEnable: boolean;

    procedure SetCallChangeEnable(const AValue: boolean);
  protected
    { Protected declarations }
    FControl: TControl;

    procedure Change(AData: PtrInt = 0); virtual;
    procedure OnChangeProc(ASender: TObject; AData: PtrInt); virtual;
  public
    { Public declarations }
    constructor Create(AControl: TControl); virtual;

    procedure Assign(Source: TPersistent); override;
    procedure Assign(Source: TPersistent; ACallChange: boolean); overload; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;

    property Control: TControl read FControl;
    property CallChangeEnable: boolean read FCallChangeEnable write SetCallChangeEnable;
    property OnChange: TOnPropertyChange read FOnChange write FOnChange;
  end;

  { TCTRPSpacingProperty }

  TCTRPSpacingProperty = class(TCTRPProperty)
  private
    { Private declarations }

    // space from the left side
    FLeft: TSpacingSize;
    // space from the top side
    FTop: TSpacingSize;
    // space from the right side
    FRight: TSpacingSize;
    // space from the bottom side
    FBottom: TSpacingSize;
    // space on each side, will be added to the other spaces
    FAround: TSpacingSize;

    FDefault: PControlBorderSpacingDefault;

    procedure SetLeft(AValue: TSpacingSize);
    procedure SetTop(AValue: TSpacingSize);
    procedure SetRight(AValue: TSpacingSize);
    procedure SetBottom(AValue: TSpacingSize);
    procedure SetAround(AValue: TSpacingSize);
    function IsLeftStored: boolean;
    function IsTopStored: boolean;
    function IsRightStored: boolean;
    function IsBottomStored: boolean;
    function IsAroundStored: boolean;
  public
    { Public declarations }
    constructor Create(AControl: TControl); override;
    constructor Create(AControl: TControl; ADefault: PControlBorderSpacingDefault);
      overload;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent; ACallChange: boolean); override;
    function IsEqual(Spacing: TCTRPSpacingProperty): boolean;

    procedure GetSpaceAround(var SpaceAround: TRect);
    function GetSideSpace(Kind: TAnchorKind): integer; // Around+GetSpace
    function GetSpace(Kind: TAnchorKind): integer;
  published
    { Published properties }
    property Left: TSpacingSize read FLeft write SetLeft stored IsLeftStored;
    property Top: TSpacingSize read FTop write SetTop stored IsTopStored;
    property Right: TSpacingSize read FRight write SetRight stored IsRightStored;
    property Bottom: TSpacingSize read FBottom write SetBottom stored IsBottomStored;
    property Around: TSpacingSize read FAround write SetAround stored IsAroundStored;
  end;

  { TCTRPBorderProperty }

  TCTRPBorderProperty = class(TCTRPProperty)
  const
    DEFAULT_BORDER_WIDTH = 0;
    DEFAULT_BORDER_COLOR = clBlack;
    DEFAULT_BORDER_ALPHA = 255;
  private
    { Private declarations }
    FBorderWidth: byte;
    FBorderColor: TColor;
    FBorderAlpha: byte;
    FBorderBGRAColor: TBGRAPixel;
    procedure SetBorderWidth(AValue: byte);
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderAlpha(AValue: byte);
    procedure CreateBorderBGRAColor;
  protected
    { Protected declarations }
  public
    { Public Declarations }
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent; ACallChange: boolean); override;
    function IsEqual(ABorder: TCTRPBorderProperty): boolean;
    function IsFullyTransparent: boolean;

    property BorderBGRAColor: TBGRAPixel read FBorderBGRAColor;
  published
    { Published properties }
    property BorderWidth: byte read FBorderWidth write SetBorderWidth default
      DEFAULT_BORDER_WIDTH;
    property BorderColor: TColor read FBorderColor write SetBorderColor default
      DEFAULT_BORDER_COLOR;
    property BorderAlpha: byte read FBorderAlpha write SetBorderAlpha default
      DEFAULT_BORDER_ALPHA;
  end;

  { TCTRPBordersProperty }

  TCTRPBordersProperty = class(TCTRPProperty)
  private
    { Private declarations }
    FBorderLeft: TCTRPBorderProperty;
    FBorderTop: TCTRPBorderProperty;
    FBorderRight: TCTRPBorderProperty;
    FBorderBottom: TCTRPBorderProperty;
    procedure SetBorderLeft(AValue: TCTRPBorderProperty);
    procedure SetBorderTop(AValue: TCTRPBorderProperty);
    procedure SetBorderRight(AValue: TCTRPBorderProperty);
    procedure SetBorderBottom(AValue: TCTRPBorderProperty);

    procedure FBorderChange(ASender: TObject; AData: PtrInt);
  protected
    { Protected declarations }
  public
    { Public Declarations }
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent; ACallChange: boolean); override;
    function IsEqual(ABorders: TCTRPBordersProperty): boolean;
  published
    { Published properties }
    property BorderLeft: TCTRPBorderProperty read FBorderLeft write SetBorderLeft;
    property BorderTop: TCTRPBorderProperty read FBorderTop write SetBorderTop;
    property BorderRight: TCTRPBorderProperty read FBorderRight write SetBorderRight;
    property BorderBottom: TCTRPBorderProperty read FBorderBottom write SetBorderBottom;
  end;

  { TCTRPBackgroundProperty }

  TCTRPBackgroundProperty = class(TCTRPProperty)
  private
    { Private declarations }
    FBackgroundColor: TColor;
    FBackgroundAlpha: byte;
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBackgroundAlpha(AValue: byte);
  protected
    { Protected declarations }
  public
    { Public Declarations }
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent; ACallChange: boolean); overload;
    function IsEqual(ABackground: TCTRPBackgroundProperty): boolean;
  published
    { Published properties }
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property BackgroundAlpha: byte read FBackgroundAlpha write SetBackgroundAlpha;
  end;

implementation

{ TCTRPProperty }

constructor TCTRPProperty.Create(AControl: TControl);
begin
  FCallChangeEnable := True;
  FControl := AControl;

  inherited Create;
end;

procedure TCTRPProperty.Assign(Source: TPersistent);
begin
  Assign(Source, True);
end;

procedure TCTRPProperty.AssignTo(Dest: TPersistent);
begin
  Dest.Assign(Self);
end;

procedure TCTRPProperty.SetCallChangeEnable(const AValue: boolean);
begin
  if FCallChangeEnable = AValue then
    exit;

  FCallChangeEnable := AValue;
end;

procedure TCTRPProperty.Change(AData: PtrInt);
begin
  if (CallChangeEnable) and (Assigned(FOnChange)) then
    FOnChange(Self, AData);
end;

procedure TCTRPProperty.OnChangeProc(ASender: TObject; AData: PtrInt);
begin
  Change(AData);
end;

{ TCTRPSpacingProperty }

constructor TCTRPSpacingProperty.Create(AControl: TControl);
begin
  Self.Create(AControl, nil);
end;

constructor TCTRPSpacingProperty.Create(AControl: TControl;
  ADefault: PControlBorderSpacingDefault);
begin
  FDefault := ADefault;
  if FDefault = nil then
  begin
    FLeft := 0;
    FRight := 0;
    FTop := 0;
    FBottom := 0;
    FAround := 0;
  end
  else
  begin
    FLeft := FDefault^.Left;
    FRight := FDefault^.Right;
    FTop := FDefault^.Top;
    FBottom := FDefault^.Bottom;
    FAround := FDefault^.Around;
  end;

  inherited Create(AControl);
end;

destructor TCTRPSpacingProperty.Destroy;
begin
  inherited Destroy;
end;

procedure TCTRPSpacingProperty.Assign(Source: TPersistent; ACallChange: boolean);
var
  srcSpacing: TCTRPSpacingProperty;
begin
  if Source is TCTRPSpacingProperty then
  begin
    srcSpacing := TCTRPSpacingProperty(Source);
    if IsEqual(srcSpacing) then
      Exit;

    FAround := srcSpacing.Around;
    FBottom := srcSpacing.Bottom;
    FLeft := srcSpacing.Left;
    FRight := srcSpacing.Right;
    FTop := srcSpacing.Top;

    if ACallChange then
      Change;
  end
  else
    inherited Assign(Source);
end;

function TCTRPSpacingProperty.IsEqual(Spacing: TCTRPSpacingProperty): boolean;
begin
  Result := (Spacing <> nil) and
    (FAround = Spacing.Around) and (FBottom = Spacing.Bottom) and
    (FLeft = Spacing.Left) and (FRight = Spacing.Right) and (FTop = Spacing.Top);
end;

procedure TCTRPSpacingProperty.GetSpaceAround(var SpaceAround: TRect);
begin
  SpaceAround.Left := Left + Around;
  SpaceAround.Top := Top + Around;
  SpaceAround.Right := Right + Around;
  SpaceAround.Bottom := Bottom + Around;
end;

function TCTRPSpacingProperty.GetSideSpace(Kind: TAnchorKind): integer;

begin
  Result := Around + GetSpace(Kind);
end;

function TCTRPSpacingProperty.GetSpace(Kind: TAnchorKind): integer;
begin
  case Kind of
    akLeft: Result := Left;
    akTop: Result := Top;
    akRight: Result := Right;
    akBottom: Result := Bottom;
  end;
end;

procedure TCTRPSpacingProperty.SetLeft(AValue: TSpacingSize);
begin
  if FLeft = AValue then
    Exit;
  FLeft := AValue;

  Change;
end;

procedure TCTRPSpacingProperty.SetTop(AValue: TSpacingSize);
begin
  if FTop = AValue then
    Exit;
  FTop := AValue;

  Change;
end;

procedure TCTRPSpacingProperty.SetRight(AValue: TSpacingSize);
begin
  if FRight = AValue then
    Exit;
  FRight := AValue;

  Change;
end;

procedure TCTRPSpacingProperty.SetBottom(AValue: TSpacingSize);
begin
  if FBottom = AValue then
    Exit;
  FBottom := AValue;

  Change;
end;

procedure TCTRPSpacingProperty.SetAround(AValue: TSpacingSize);
begin
  if FAround = AValue then
    Exit;
  FAround := AValue;

  Change;
end;

function TCTRPSpacingProperty.IsLeftStored: boolean;
begin
  if FDefault = nil then
    Result := FLeft <> 0
  else
    Result := FLeft <> FDefault^.Left;
end;

function TCTRPSpacingProperty.IsTopStored: boolean;
begin
  if FDefault = nil then
    Result := FTop <> 0
  else
    Result := FTop <> FDefault^.Top;
end;

function TCTRPSpacingProperty.IsRightStored: boolean;
begin
  if FDefault = nil then
    Result := FRight <> 0
  else
    Result := FRight <> FDefault^.Right;
end;

function TCTRPSpacingProperty.IsBottomStored: boolean;
begin
  if FDefault = nil then
    Result := FBottom <> 0
  else
    Result := FBottom <> FDefault^.Bottom;
end;

function TCTRPSpacingProperty.IsAroundStored: boolean;
begin
  if FDefault = nil then
    Result := FAround <> 0
  else
    Result := FAround <> FDefault^.Around;
end;

{ TCTRPBorderProperty }

constructor TCTRPBorderProperty.Create(AControl: TControl);
begin
  inherited Create(AControl);

  FBorderWidth := DEFAULT_BORDER_WIDTH;
  FBorderColor := DEFAULT_BORDER_COLOR;
  FBorderAlpha := DEFAULT_BORDER_ALPHA;
  CreateBorderBGRAColor;
end;

destructor TCTRPBorderProperty.Destroy;
begin
  inherited Destroy;
end;

procedure TCTRPBorderProperty.Assign(Source: TPersistent; ACallChange: boolean);
var
  srcBorder: TCTRPBorderProperty;
begin
  if Source is TCTRPBorderProperty then
  begin
    srcBorder := TCTRPBorderProperty(Source);
    if IsEqual(srcBorder) then
      Exit;

    FBorderWidth := srcBorder.BorderWidth;
    FBorderColor := srcBorder.BorderColor;
    FBorderAlpha := srcBorder.BorderAlpha;
    CreateBorderBGRAColor;

    if ACallChange then
      Change;
  end
  else
    inherited Assign(Source);
end;

// Check whether an object is equal to stored border
function TCTRPBorderProperty.IsEqual(ABorder: TCTRPBorderProperty): boolean;
begin
  Result := (ABorder <> nil) and
    (FBorderWidth = ABorder.BorderWidth) and
    (FBorderColor = ABorder.FBorderColor) and
    (FBorderAlpha = ABorder.FBorderAlpha);
end;

function TCTRPBorderProperty.IsFullyTransparent: boolean;
begin
  Result := FBorderAlpha = 0;
end;

procedure TCTRPBorderProperty.SetBorderWidth(AValue: byte);
begin
  if FBorderWidth = AValue then
    Exit;
  FBorderWidth := AValue;

  Change;
end;

procedure TCTRPBorderProperty.SetBorderColor(AValue: TColor);
begin
  if FBorderColor = AValue then
    Exit;
  FBorderColor := AValue;
  CreateBorderBGRAColor;

  Change;
end;

procedure TCTRPBorderProperty.SetBorderAlpha(AValue: byte);
begin
  if FBorderAlpha = AValue then
    Exit;
  FBorderAlpha := AValue;
  CreateBorderBGRAColor;

  Change;
end;

procedure TCTRPBorderProperty.CreateBorderBGRAColor;
begin
  FBorderBGRAColor := ColorToBGRA(FBorderColor, FBorderAlpha);
end;

{ TCTRPBordersProperty }

constructor TCTRPBordersProperty.Create(AControl: TControl);
begin
  inherited Create(AControl);

  FBorderLeft := TCTRPBorderProperty.Create(AControl);
  FBorderTop := TCTRPBorderProperty.Create(AControl);
  FBorderRight := TCTRPBorderProperty.Create(AControl);
  FBorderBottom := TCTRPBorderProperty.Create(AControl);

  FBorderLeft.OnChange := @OnChangeProc;
  FBorderTop.OnChange := @OnChangeProc;
  FBorderRight.OnChange := @OnChangeProc;
  FBorderBottom.OnChange := @OnChangeProc;
end;

destructor TCTRPBordersProperty.Destroy;
begin
  FreeAndNil(FBorderBottom);
  FreeAndNil(FBorderRight);
  FreeAndNil(FBorderTop);
  FreeAndNil(FBorderLeft);

  inherited Destroy;
end;

procedure TCTRPBordersProperty.Assign(Source: TPersistent; ACallChange: boolean);
var
  srcBorders: TCTRPBordersProperty;
begin
  if Source is TCTRPBordersProperty then
  begin
    srcBorders := TCTRPBordersProperty(Source);
    if IsEqual(srcBorders) then
      Exit;

    FBorderLeft.Assign(srcBorders.BorderLeft, False);
    FBorderTop.Assign(srcBorders.BorderTop, False);
    FBorderRight.Assign(srcBorders.BorderRight, False);
    FBorderBottom.Assign(srcBorders.BorderBottom, False);

    if ACallChange then
      Change;
  end
  else
    inherited Assign(Source);
end;

// Check whether an object is equal to stored borders
function TCTRPBordersProperty.IsEqual(ABorders: TCTRPBordersProperty): boolean;
begin
  Result := (ABorders <> nil) and
    (FBorderLeft.IsEqual(ABorders.BorderLeft)) and
    (FBorderTop.IsEqual(ABorders.BorderTop)) and
    (FBorderRight.IsEqual(ABorders.BorderRight)) and
    (FBorderBottom.IsEqual(ABorders.BorderBottom));
end;

procedure TCTRPBordersProperty.SetBorderLeft(AValue: TCTRPBorderProperty);
begin
  FBorderLeft.Assign(AValue);
end;

procedure TCTRPBordersProperty.SetBorderTop(AValue: TCTRPBorderProperty);
begin
  FBorderTop.Assign(AValue);
end;

procedure TCTRPBordersProperty.SetBorderRight(AValue: TCTRPBorderProperty);
begin
  FBorderRight.Assign(AValue);
end;

procedure TCTRPBordersProperty.SetBorderBottom(AValue: TCTRPBorderProperty);
begin
  FBorderBottom.Assign(AValue);
end;

procedure TCTRPBordersProperty.FBorderChange(ASender: TObject; AData: PtrInt);
begin
  Change(AData);
end;

{ TCTRPBackgroundProperty }

constructor TCTRPBackgroundProperty.Create(AControl: TControl);
begin
  inherited Create(AControl);
end;

destructor TCTRPBackgroundProperty.Destroy;
begin
  inherited Destroy;
end;

procedure TCTRPBackgroundProperty.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor = AValue then
    Exit;
  FBackgroundColor := AValue;

  Change;
end;

procedure TCTRPBackgroundProperty.SetBackgroundAlpha(AValue: byte);
begin
  if FBackgroundAlpha = AValue then
    Exit;
  FBackgroundAlpha := AValue;

  Change;
end;

procedure TCTRPBackgroundProperty.Assign(Source: TPersistent; ACallChange: boolean);
var
  srcBackground: TCTRPBackgroundProperty;
begin
  if Source is TCTRPBackgroundProperty then
  begin
    srcBackground := TCTRPBackgroundProperty(Source);
    if IsEqual(srcBackground) then
      Exit;

    FBackgroundColor := srcBackground.BackgroundColor;
    FBackgroundAlpha := srcBackground.BackgroundAlpha;

    if ACallChange then
      Change;
  end
  else
    inherited Assign(Source);
end;

function TCTRPBackgroundProperty.IsEqual(ABackground: TCTRPBackgroundProperty): boolean;
begin
  Result := (ABackground <> nil) and
    (FBackgroundColor = ABackground.BackgroundColor) and
    (FBackgroundAlpha = ABackground.BackgroundAlpha);
end;

end.

