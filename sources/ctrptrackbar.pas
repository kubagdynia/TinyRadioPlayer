unit CTRPTrackBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, LMessages, BGRABitmap,
  BGRABitmapTypes, CTRPPersistent, CTRPCommon;

type
  TTrackBarOrientation = (tbHorizontal, tbVertical);

type
  TOnPositionChangeEvent = procedure(ASender: TObject; APosition: integer) of object;

type

  { TCTRPTrackProperty }

  TCTRPTrackProperty = class(TCTRPProperty)
  private
    { Private declarations }
    FTrackSpacing: TCTRPSpacingProperty;
    FTrackBorder: TCTRPBordersProperty;
    FTrackBackground: TCTRPBackgroundProperty;
    procedure SetTrackBackground(AValue: TCTRPBackgroundProperty);
    procedure SetTrackBorder(AValue: TCTRPBordersProperty);
    procedure SetTrackSpacing(AValue: TCTRPSpacingProperty);
  protected
    { Protected declarations }
  public
    { Public Declarations }
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent; ACallChange: boolean); override;
    function IsEqual(ATrackProperty: TCTRPTrackProperty): boolean;
  published
    { Published properties }
    property TrackSpacing: TCTRPSpacingProperty read FTrackSpacing write SetTrackSpacing;
    property TrackBorder: TCTRPBordersProperty read FTRackBorder write SetTrackBorder;
    property TrackBackground: TCTRPBackgroundProperty
      read FTrackBackground write SetTrackBackground;
  end;

type

  { TCTRPThumbProperty }

  TCTRPThumbProperty = class(TCTRPProperty)
  const
    DEFAULT_THUMB_WIDTH = 20;
    DEFAULT_THUMB_HEIGHT = 16;
    DEFAULT_DRAW_POSITION = True;
    DEFAULT_THUMB_FONT_COLOR = ({r} 84 or ({g} 84 shl 8) or ({b} 84 shl 16)); // Dark gray
    DEFAULT_THUMB_FONT_HEIGHT = 11;
    DEFAULT_THUMB_FONT_STYLE = [fsBold];
  private
    { Private declarations }
    FThumbWidth: integer;
    FThumbHeight: integer;
    FThumbBorder: TCTRPBordersProperty;
    FThumbBackground: TCTRPBackgroundProperty;
    FThumbFontColor: TColor;
    FThumbFontHeight: integer;
    FThumbFontStyle: TFontStyles;

    // Whether draw position on thumb
    FDrawPosition: boolean;

    procedure SetDrawPosition(AValue: boolean);
    procedure SetThumbWidth(AValue: integer);
    procedure SetThumbHeight(AValue: integer);
    procedure SetThumbBackground(AValue: TCTRPBackgroundProperty);
    procedure SetThumbBorder(AValue: TCTRPBordersProperty);
    procedure SetThumbFontColor(AValue: TColor);
    procedure SetThumbFontHeight(AValue: integer);
    procedure SetThumbFontStyle(AValue: TFontStyles);
  protected
    { Protected declarations }
  public
    { Public Declarations }
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent; ACallChange: boolean); override;
    function IsEqual(AThumbProperty: TCTRPThumbProperty): boolean;
  published
    { Published properties }
    property ThumbWidth: integer read FThumbWidth write SetThumbWidth default
      DEFAULT_THUMB_WIDTH;
    property ThumbHeight: integer read FThumbHeight write SetThumbHeight default
      DEFAULT_THUMB_HEIGHT;
    property ThumbBorder: TCTRPBordersProperty read FThumbBorder write SetThumbBorder;
    property ThumbBackground: TCTRPBackgroundProperty
      read FThumbBackground write SetThumbBackground;
    property DrawPosition: boolean read FDrawPosition write SetDrawPosition;
    property ThumbFontColor: TColor read FThumbFontColor write SetThumbFontColor
      default DEFAULT_THUMB_FONT_COLOR;
    property ThumbFontHeight: integer read FThumbFontHeight write SetThumbFontHeight
      default DEFAULT_THUMB_FONT_HEIGHT;
    property ThumbFontStyle: TFontStyles read FThumbFontStyle write SetThumbFontStyle
      default DEFAULT_THUMB_FONT_STYLE;
  end;

type

  { TCTRPTrackBar }

  TCTRPTrackBar = class(TGraphicControl, ICTRPControl)
  private
    { Private declarations }
    FBitmap: TBGRABitmap; // bitmap where the control wil be drawn

    FOrientation: TTrackBarOrientation;
    FPosition: integer;
    FMax: integer;
    FMin: integer;

    FTrackBarWasClicked: boolean;

    // Track
    FTrackRect: TRect;
    FTrack: TCTRPTrackProperty;

    // Thumb
    FThumbRect: TRect;
    FThumbPosition: integer;
    FThumb: TCTRPThumbProperty;

    // Background
    FBackground: TCTRPBackgroundProperty;

    // Events
    FOnPositionChangeEvent: TOnPositionChangeEvent;
    FOnPositionChangeByUserEvent: TOnPositionChangeEvent;

    procedure SetOrientation(const AValue: TTrackBarOrientation);
    procedure SetPosition(const AValue: integer);
    procedure SetMax(const AValue: integer);
    procedure SetMin(const AValue: integer);
    procedure SetBackground(const AValue: TCTRPBackgroundProperty);
    procedure SetTrack(const AValue: TCTRPTrackProperty);
    procedure SetThumb(const AValue: TCTRPThumbProperty);
    procedure SetRttiPropertiesToIgnore(const AValue: string);
    function GetRttiPropertiesToIgnore: string;

    function ThumbMinPosition: integer;
    function ThumbMaxPosition: integer;

    function UpdatePosition: boolean;
    procedure UpdateThumbPosition;
    procedure DoOnPositionChange;
    procedure DoOnPositionChangeByUser;
    procedure ChangePosition(const AValueToBeAdded: integer);

    procedure RecalcTrackRect;
    procedure RecalcThumbRect;
    procedure RecalcThumbPosition;

    procedure CheckMinMaxPosition;

    procedure FThumbChange(ASender: TObject; AData: PtrInt);
    procedure FTrackChange(ASender: TObject; AData: PtrInt);
    procedure FBackgroundChange(ASender: TObject; AData: PtrInt);
  protected
    { Protected declarations }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    procedure Paint; override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure SetDefaultValues;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure DefaultSkin;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure RecalcControl(AInvalidate: boolean = True);
    procedure DefaultSkin2;

  published
    { Published declarations }
    property Enabled;

    property Orientation: TTrackBarOrientation
      read FOrientation write SetOrientation default tbHorizontal;
    property Position: integer read FPosition write SetPosition default 3;
    property Max: integer read FMax write SetMax default 10;
    property Min: integer read FMin write SetMin default 0;
    // Trackbar
    property Track: TCTRPTrackProperty read FTrack write SetTrack;
    // Thumb
    property Thumb: TCTRPThumbProperty read FThumb write SetThumb;
    // Background
    property Background: TCTRPBackgroundProperty read FBackground write SetBackground;

    property OnPositionChange: TOnPositionChangeEvent
      read FOnPositionChangeEvent write FOnPositionChangeEvent;
    property OnPositionChangeByUser: TOnPositionChangeEvent
      read FOnPositionChangeByUserEvent write FOnPositionChangeByUserEvent;

    property RttiPropertiesToIgnore: string
      read GetRttiPropertiesToIgnore write SetRttiPropertiesToIgnore;

  end;

implementation

{ TCTRPTrackProperty }

constructor TCTRPTrackProperty.Create(AControl: TControl);
begin
  inherited Create(AControl);

  FTrackSpacing := TCTRPSpacingProperty.Create(AControl);
  FTrackBorder := TCTRPBordersProperty.Create(AControl);
  FTrackBackground := TCTRPBackgroundProperty.Create(AControl);

  FTrackSpacing.OnChange := @OnChangeProc;
  FTrackBorder.OnChange := @OnChangeProc;
  FTrackBackground.OnChange := @OnChangeProc;
end;

destructor TCTRPTrackProperty.Destroy;
begin
  FreeAndNil(FTrackSpacing);
  FreeAndNil(FTrackBorder);
  FreeAndNil(FTrackBackground);

  inherited Destroy;
end;

procedure TCTRPTrackProperty.Assign(Source: TPersistent; ACallChange: boolean);
var
  srcTrack: TCTRPTrackProperty;
begin
  if Source is TCTRPTrackProperty then
  begin
    srcTrack := TCTRPTrackProperty(Source);
    if IsEqual(srcTrack) then
      Exit;

    FTrackSpacing.Assign(srcTrack.TrackSpacing, False);
    FTrackBorder.Assign(srcTrack.TrackBorder, False);
    FTrackBackground.Assign(srcTrack.TrackBackground, False);

    if ACallChange then
      Change;
  end
  else
    inherited Assign(Source);
end;

// Check whether an object is equal to stored track
function TCTRPTrackProperty.IsEqual(ATrackProperty: TCTRPTrackProperty): boolean;
begin
  Result := (ATrackProperty <> nil) and
    (FTrackSpacing.IsEqual(ATrackProperty.TrackSpacing)) and
    (FTrackBorder.IsEqual(ATrackProperty.TrackBorder)) and
    (FTrackBackground.IsEqual(ATrackProperty.TrackBackground));
end;

procedure TCTRPTrackProperty.SetTrackSpacing(AValue: TCTRPSpacingProperty);
begin
  if FTrackSpacing.IsEqual(AValue) then
    Exit;
  FTrackSpacing.Assign(AValue);

  Change();
end;

procedure TCTRPTrackProperty.SetTrackBorder(AValue: TCTRPBordersProperty);
begin
  if FTrackBorder.IsEqual(AValue) then
    Exit;
  FTrackBorder.Assign(AValue);

  Change;
end;

procedure TCTRPTrackProperty.SetTrackBackground(AValue: TCTRPBackgroundProperty);
begin
  if FTrackBackground.IsEqual(AValue) then
    Exit;
  FTrackBackground.Assign(AValue);

  Change;
end;

{ TCTRPThumbProperty }

constructor TCTRPThumbProperty.Create(AControl: TControl);
begin
  inherited Create(AControl);

  FThumbWidth := DEFAULT_THUMB_WIDTH;
  FThumbHeight := DEFAULT_THUMB_HEIGHT;

  FDrawPosition := DEFAULT_DRAW_POSITION;

  FThumbFontColor := DEFAULT_THUMB_FONT_COLOR;
  FThumbFontHeight := DEFAULT_THUMB_FONT_HEIGHT;
  FThumbFontStyle := DEFAULT_THUMB_FONT_STYLE;

  FThumbBorder := TCTRPBordersProperty.Create(AControl);
  FThumbBackground := TCTRPBackgroundProperty.Create(AControl);

  FThumbBorder.OnChange := @OnChangeProc;
  FThumbBackground.OnChange := @OnChangeProc;
end;

destructor TCTRPThumbProperty.Destroy;
begin
  FreeAndNil(FThumbBorder);
  FreeAndNil(FThumbBackground);

  inherited Destroy;
end;

procedure TCTRPThumbProperty.Assign(Source: TPersistent; ACallChange: boolean);
var
  srcThumb: TCTRPThumbProperty;
begin
  if Source is TCTRPThumbProperty then
  begin
    srcThumb := TCTRPThumbProperty(Source);
    if IsEqual(srcThumb) then
      Exit;

    FThumbWidth := srcThumb.ThumbWidth;
    FThumbHeight := srcThumb.ThumbHeight;
    FThumbBorder.Assign(srcThumb.ThumbBorder, False);
    FThumbBackground.Assign(srcThumb.ThumbBackground, False);

    if ACallChange then
      Change;
  end
  else
    inherited Assign(Source);
end;

// Check whether an object is equal to stored thumb
function TCTRPThumbProperty.IsEqual(AThumbProperty: TCTRPThumbProperty): boolean;
begin
  Result := (AThumbProperty <> nil) and
    (FDrawPosition = AThumbProperty.DrawPosition) and
    (FThumbWidth = AThumbProperty.ThumbWidth) and
    (FThumbHeight = AThumbProperty.ThumbHeight) and
    (FThumbBorder.IsEqual(AThumbProperty.ThumbBorder)) and
    (FThumbBackground.IsEqual(AThumbProperty.ThumbBackground)) and
    (FThumbFontColor = AThumbProperty.ThumbFontColor) and
    (FThumbFontHeight = AThumbProperty.ThumbFontHeight) and
    (FThumbFontStyle = AThumbProperty.ThumbFontStyle);
end;

procedure TCTRPThumbProperty.SetDrawPosition(AValue: boolean);
begin
  if FDrawPosition = AValue then
    Exit;
  FDrawPosition := AValue;

  Change;
end;

procedure TCTRPThumbProperty.SetThumbWidth(AValue: integer);
begin
  if FThumbWidth = AValue then
    Exit;
  FThumbWidth := AValue;

  Change;
end;

procedure TCTRPThumbProperty.SetThumbHeight(AValue: integer);
begin
  if FThumbHeight = AValue then
    Exit;
  FThumbHeight := AValue;

  Change;
end;

procedure TCTRPThumbProperty.SetThumbBorder(AValue: TCTRPBordersProperty);
begin
  if FThumbBorder.IsEqual(AValue) then
    Exit;
  FThumbBorder.Assign(AValue);

  Change;
end;

procedure TCTRPThumbProperty.SetThumbFontColor(AValue: TColor);
begin
  if FThumbFontColor = AValue then
    Exit;
  FThumbFontColor := AValue;

  Change;
end;

procedure TCTRPThumbProperty.SetThumbFontHeight(AValue: integer);
begin
  if FThumbFontHeight = AValue then
    Exit;
  FThumbFontHeight := AValue;

  Change;
end;

procedure TCTRPThumbProperty.SetThumbFontStyle(AValue: TFontStyles);
var
  i: integer;
begin
  if FThumbFontStyle = AValue then
    Exit;
  FThumbFontStyle := AValue;

  Change;
end;

procedure TCTRPThumbProperty.SetThumbBackground(AValue: TCTRPBackgroundProperty);
begin
  if FThumbBackground.IsEqual(AValue) then
    Exit;
  FThumbBackground.Assign(AValue);

  Change;
end;

{ TCTRPTrackBar }

constructor TCTRPTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBitmap := nil;

  FTrackBarWasClicked := False;

  // Track
  FTrack := TCTRPTrackProperty.Create(Self);
  FTrack.OnChange := @FTrackChange;

  // Thumb
  FThumb := TCTRPThumbProperty.Create(Self);
  FThumb.OnChange := @FThumbChange;

  // Background
  FBackground := TCTRPBackgroundProperty.Create(Self);

  FBackground.OnChange := @FBackgroundChange;

  SetDefaultValues;
  DefaultSkin;
end;

destructor TCTRPTrackBar.Destroy;
begin
  FreeAndNil(FTrack);
  FreeAndNil(FThumb);
  FreeAndNil(FBackground);

  if Assigned(FBitmap) then
    FreeAndNil(FBitmap);

  inherited Destroy;
end;

procedure TCTRPTrackBar.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TCTRPTrackBar.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
end;

procedure TCTRPTrackBar.FBackgroundChange(ASender: TObject; AData: PtrInt);
begin
  RecalcControl;
end;

procedure TCTRPTrackBar.FThumbChange(ASender: TObject; AData: PtrInt);
begin
  RecalcControl;
end;

procedure TCTRPTrackBar.FTrackChange(ASender: TObject; AData: PtrInt);
begin
  RecalcControl;
end;

procedure TCTRPTrackBar.SetOrientation(const AValue: TTrackBarOrientation);
begin
  if FOrientation = AValue then
    Exit;
  FOrientation := AValue;

  RecalcControl;
end;

procedure TCTRPTrackBar.SetPosition(const AValue: integer);
begin
  if (FPosition = AValue) or (AValue < FMin) or (AValue > FMax) then
    Exit;
  FPosition := AValue;

  RecalcControl;

  DoOnPositionChange;
end;

procedure TCTRPTrackBar.SetMax(const AValue: integer);
begin
  if (FMax = AValue) or (FMax < FMin) then
    Exit;
  FMax := AValue;

  RecalcControl;
end;

procedure TCTRPTrackBar.SetMin(const AValue: integer);
begin
  if (FMin = AValue) or (FMin > FMax) then
    Exit;
  FMin := AValue;

  RecalcControl;
end;

procedure TCTRPTrackBar.SetBackground(const AValue: TCTRPBackgroundProperty);
begin
  FBackground.Assign(AValue);
end;

procedure TCTRPTrackBar.SetTrack(const AValue: TCTRPTrackProperty);
begin
  FTrack.Assign(AValue);
end;

procedure TCTRPTrackBar.SetThumb(const AValue: TCTRPThumbProperty);
begin
  FThumb.Assign(AValue);
end;

procedure TCTRPTrackBar.SetRttiPropertiesToIgnore(const AValue: string);
begin

end;

function TCTRPTrackBar.GetRttiPropertiesToIgnore: string;
begin
  Result := 'position';
end;

function TCTRPTrackBar.ThumbMinPosition: integer;
begin
  case FOrientation of
    tbHorizontal: Result := FTrackRect.Left;
    tbVertical: Result := FTrackRect.Top;
  end;
end;

function TCTRPTrackBar.ThumbMaxPosition: integer;
begin
  case FOrientation of
    tbHorizontal: Result := FTrackRect.Right;
    tbVertical: Result := FTrackRect.Bottom;
  end;
end;

function TCTRPTrackBar.UpdatePosition: boolean;
var
  oldPosition: integer;
begin
  // protection against dividing by zero
  if (ThumbMaxPosition - ThumbMinPosition) * (FMax - FMin) = 0 then
    Exit;

  oldPosition := FPosition;

  FPosition := round(FMin + (FThumbPosition - ThumbMinPosition) /
    (ThumbMaxPosition - ThumbMinPosition) * (FMax - FMin));

  CheckMinMaxPosition;

  Result := (FPosition <> oldPosition);
end;

procedure TCTRPTrackBar.UpdateThumbPosition;
begin
  // protection against dividing by zero
  if (FMax - FMin) = 0 then
    Exit;

  FThumbPosition := ThumbMinPosition + round(
    (ThumbMaxPosition - ThumbMinPosition) * ((FPosition - FMin) / (FMax - FMin)));
end;

procedure TCTRPTrackBar.DoOnPositionChange;
begin
  if Assigned(OnPositionChange) then
    OnPositionChange(Self, FPosition);
end;

procedure TCTRPTrackBar.DoOnPositionChangeByUser;
begin
  if Assigned(OnPositionChangeByUser) then
    OnPositionChangeByUser(Self, FPosition);
end;

procedure TCTRPTrackBar.ChangePosition(const AValueToBeAdded: integer);
var
  oldPosition: integer;
begin
  oldPosition := FPosition;

  FPosition := FPosition + AValueToBeAdded;
  CheckMinMaxPosition;

  if FPosition <> oldPosition then
  begin
    RecalcControl;

    DoOnPositionChange;
    DoOnPositionChangeByUser;
  end;
end;

procedure TCTRPTrackBar.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  PreferredWidth := 100;
  PreferredHeight := 33;
end;

procedure TCTRPTrackBar.Paint;
var
  tx, ty: integer;
begin
  tx := ClientWidth;
  ty := ClientHeight;

  // If the bitmap size is diffrent that client size just free it
  if Assigned(FBitmap) and ((FBitmap.Width <> tx) or (FBitmap.Height <> ty)) then
    FreeAndNil(FBitmap);

  // If the bitmap is not assigned create it, otherwise fill it transparent color
  if not Assigned(FBitmap) then
    FBitmap := TBGRABitmap.Create(tx, ty)
  else
    FBitmap.FillTransparent;

  // if background color is not fully transparent then fill background
  if FBackground.BackgroundAlpha <> 0 then
    FBitmap.Fill(
      ColorToBGRA(FBackground.BackgroundColor, FBackground.BackgroundAlpha));

  // Draw track
  with FTrack.TrackBorder do
  begin
    // Left border
    if (BorderLeft.BorderWidth > 0) and (not BorderLeft.IsFullyTransparent) then
    begin
      if BorderLeft.BorderWidth = 1 then
        FBitmap.DrawVertLine(FTrackRect.Left, FTrackRect.Top,
          FTrackRect.Bottom - BorderBottom.BorderWidth,
          BorderLeft.BorderBGRAColor)
      else
        FBitmap.Rectangle(
          FTrackRect.Left, FTrackRect.Top,
          FTrackRect.Left + BorderLeft.BorderWidth,
          FTrackRect.Bottom - BorderBottom.BorderWidth,
          BorderLeft.BorderBGRAColor, BorderLeft.BorderBGRAColor, dmSet
          );
    end;

    // Top border
    if (BorderTop.BorderWidth > 0) and (not BorderTop.IsFullyTransparent) then
    begin
      if BorderTop.BorderWidth = 1 then
        FBitmap.DrawHorizLine(FTrackRect.Left + BorderLeft.BorderWidth,
          FTrackRect.Top, FTrackRect.Right,
          BorderTop.BorderBGRAColor)
      else
        FBitmap.Rectangle(
          FTrackRect.Left + BorderLeft.BorderWidth, FTrackRect.Top,
          FTrackRect.Right, FTrackRect.Top + BorderTop.BorderWidth,
          BorderTop.BorderBGRAColor, BorderTop.BorderBGRAColor, dmSet
          );
    end;

    // Right border
    if (BorderRight.BorderWidth > 0) and (not BorderRight.IsFullyTransparent) then
    begin
      if BorderRight.BorderWidth = 1 then
        FBitmap.DrawVertLine(FTrackRect.Right, FTrackRect.Top +
          BorderTop.BorderWidth, FTrackRect.Bottom,
          BorderRight.BorderBGRAColor)
      else
        FBitmap.Rectangle(
          FTrackRect.Right + 1, FTrackRect.Top + BorderTop.BorderWidth,
          FTrackRect.Right - BorderRight.BorderWidth + 1, FTrackRect.Bottom + 1,
          BorderRight.BorderBGRAColor, BorderRight.BorderBGRAColor, dmSet
          );
    end;

    // Bottom border
    if (BorderBottom.BorderWidth > 0) and (not BorderBottom.IsFullyTransparent) then
    begin
      if BorderBottom.BorderWidth = 1 then
        FBitmap.DrawHorizLine(FTrackRect.Left, FTrackRect.Bottom,
          FTrackRect.Right - BorderRight.BorderWidth,
          BorderBottom.BorderBGRAColor)
      else
        FBitmap.Rectangle(
          FTrackRect.Left, FTrackRect.Bottom + 1,
          FTrackRect.Right - BorderRight.BorderWidth + 1,
          FTrackRect.Bottom - BorderBottom.BorderWidth + 1,
          BorderBottom.BorderBGRAColor, dmSet
          );
    end;

    FBitmap.FillRect(
      FTrackRect.Left + BorderLeft.BorderWidth,
      FTrackRect.Top + BorderTop.BorderWidth,
      FTrackRect.Right - BorderRight.BorderWidth + 1,
      FTrackRect.Bottom - BorderBottom.BorderWidth + 1,
      ColorToBGRA(FTrack.TrackBackground.BackgroundColor,
      FTrack.TrackBackground.BackgroundAlpha),
      dmSet);
  end;

  // Draw thumb
  with FThumb.ThumbBorder do
  begin
    // Left border
    if (BorderLeft.BorderWidth > 0) and (not BorderLeft.IsFullyTransparent) then
    begin
      if BorderLeft.BorderWidth = 1 then
        FBitmap.DrawVertLine(FThumbRect.Left, FThumbRect.Top,
          FThumbRect.Bottom - BorderBottom.BorderWidth,
          BorderLeft.BorderBGRAColor)
      else
        FBitmap.Rectangle(
          FThumbRect.Left, FThumbRect.Top,
          FThumbRect.Left + BorderLeft.BorderWidth,
          FThumbRect.Bottom - BorderBottom.BorderWidth,
          BorderLeft.BorderBGRAColor, BorderLeft.BorderBGRAColor, dmSet
          );
    end;

    // Top border
    if (BorderTop.BorderWidth > 0) and (not BorderTop.IsFullyTransparent) then
    begin
      if BorderTop.BorderWidth = 1 then
        FBitmap.DrawHorizLine(FThumbRect.Left + BorderLeft.BorderWidth,
          FThumbRect.Top, FThumbRect.Right,
          BorderTop.BorderBGRAColor)
      else
        FBitmap.Rectangle(
          FThumbRect.Left + BorderLeft.BorderWidth, FThumbRect.Top,
          FThumbRect.Right, FThumbRect.Top + BorderTop.BorderWidth,
          BorderTop.BorderBGRAColor, BorderTop.BorderBGRAColor, dmSet
          );
    end;

    // Right border
    if (BorderRight.BorderWidth > 0) and (not BorderRight.IsFullyTransparent) then
    begin
      if BorderRight.BorderWidth = 1 then
        FBitmap.DrawVertLine(FThumbRect.Right, FThumbRect.Top +
          BorderTop.BorderWidth, FThumbRect.Bottom,
          BorderRight.BorderBGRAColor)
      else
        FBitmap.Rectangle(
          FThumbRect.Right + 1, FThumbRect.Top + BorderTop.BorderWidth,
          FThumbRect.Right - BorderRight.BorderWidth + 1, FThumbRect.Bottom + 1,
          BorderRight.BorderBGRAColor, BorderRight.BorderBGRAColor, dmSet
          );
    end;

    // Bottom border
    if (BorderBottom.BorderWidth > 0) and (not BorderBottom.IsFullyTransparent) then
    begin
      if BorderBottom.BorderWidth = 1 then
        FBitmap.DrawHorizLine(FThumbRect.Left, FThumbRect.Bottom,
          FThumbRect.Right - BorderRight.BorderWidth,
          BorderBottom.BorderBGRAColor)
      else
        FBitmap.Rectangle(
          FThumbRect.Left, FThumbRect.Bottom + 1,
          FThumbRect.Right - BorderRight.BorderWidth + 1,
          FThumbRect.Bottom - BorderBottom.BorderWidth + 1,
          BorderBottom.BorderBGRAColor, dmSet
          );
    end;

    FBitmap.FillRect(
      FThumbRect.Left + BorderLeft.BorderWidth,
      FThumbRect.Top + BorderTop.BorderWidth,
      FThumbRect.Right - BorderRight.BorderWidth + 1,
      FThumbRect.Bottom - BorderBottom.BorderWidth + 1,
      ColorToBGRA(FThumb.ThumbBackground.BackgroundColor,
      FThumb.ThumbBackground.BackgroundAlpha),
      dmSet);

    if FThumb.DrawPosition then
    begin
      FBitmap.FontHeight := FThumb.ThumbFontHeight;
      FBitmap.FontStyle := FThumb.FThumbFontStyle;
      FBitmap.TextRect(FThumbRect,
        IntToStr(Position), taCenter, tlCenter,
        ColorToBGRA(FThumb.ThumbFontColor));
    end;

  end;


  //FBitmap.Rectangle(FTrackRect, BGRA(0, 0, 0, 6), dmSet);


  // Move bitmap to the canvas
  FBitmap.Draw(Canvas, 0, 0, False);
end;

procedure TCTRPTrackBar.RecalcControl(AInvalidate: boolean = True);
begin
  if csLoading in ComponentState then
    Exit;

  RecalcTrackRect;
  RecalcThumbPosition;
  RecalcThumbRect;
  UpdateThumbPosition;

  if AInvalidate then
    Invalidate;
end;

procedure TCTRPTrackBar.RecalcTrackRect;
var
  tx, ty: integer;
  trackSpaceAround: TRect;
begin
  tx := ClientWidth;
  ty := ClientHeight;

  // get track space around
  FTRack.TrackSpacing.GetSpaceAround(trackSpaceAround);

  case Orientation of
    tbHorizontal:
    begin
      FTrackRect :=
        Rect(trackSpaceAround.Left, trackSpaceAround.Top, tx -
        trackSpaceAround.Right, ty - trackSpaceAround.Bottom);
    end;
  end;
end;

procedure TCTRPTrackBar.RecalcThumbRect;
var
  dx, dy: integer;
  trackVerticalCenter: integer;
begin
  case Orientation of
    tbHorizontal:
    begin
      trackVerticalCenter := FTrackRect.Top +
        ((FTrackRect.Bottom - FTrackRect.Top) div 2);
      dx := Thumb.ThumbWidth div 2;
      dy := Thumb.ThumbHeight div 2;
      FThumbRect := Rect(FThumbPosition - dx, trackVerticalCenter -
        dy, FThumbPosition + dx, trackVerticalCenter + dy);
    end;
  end;
end;

procedure TCTRPTrackBar.RecalcThumbPosition;
begin
  if FThumbPosition > ThumbMaxPosition then
    FThumbPosition := ThumbMaxPosition;
  if FThumbPosition < ThumbMinPosition then
    FThumbPosition := ThumbMinPosition;
end;

procedure TCTRPTrackBar.CheckMinMaxPosition;
begin
  if FPosition > FMax then
    FPosition := FMax
  else
  if FPosition < FMin then
    FPosition := FMin;
end;

{$HINTS off}// don't need any hints that we don't use a variable
procedure TCTRPTrackBar.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin

end;

{$HINTS ON}

procedure TCTRPTrackBar.SetDefaultValues;
begin
  // Sets the bounds of the control initially, when it is created.
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, 150, 33);

  // Set thumb min value
  FMin := 0;
  // Set thumb max value
  FMax := 100;
  // Set thumb value
  FPosition := 0;
  // Set reack bar orientation
  FOrientation := tbHorizontal;
end;

procedure TCTRPTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  positionChanged: boolean;
begin
  if ssLeft in Shift then
  begin
    FTrackBarWasClicked := True;

    case FOrientation of
      tbHorizontal: FThumbPosition := X;
      tbVertical: FThumbPosition := Y;
    end;

    positionChanged := UpdatePosition;
    RecalcControl;

    if positionChanged then
    begin
      DoOnPositionChange;
      DoOnPositionChangeByUser;
    end;
  end;

  inherited;
end;

procedure TCTRPTrackBar.MouseMove(Shift: TShiftState; X, Y: integer);
var
  positionChanged: boolean;
begin
  if (ssLeft in Shift) and (FTrackBarWasClicked) then
  begin
    if Orientation = tbHorizontal then
      FThumbPosition := X
    else
      FThumbPosition := Y;

    positionChanged := UpdatePosition;
    RecalcControl;

    if positionChanged then
    begin
      DoOnPositionChange;
      DoOnPositionChangeByUser;
    end;
  end;

  inherited;
end;

procedure TCTRPTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FTrackBarWasClicked := False;
  inherited;
end;

function TCTRPTrackBar.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean;
begin
  Result := inherited;
  ChangePosition(1);
end;

function TCTRPTrackBar.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean;
begin
  Result := inherited;
  ChangePosition(-1);
end;

procedure TCTRPTrackBar.Resize;
begin
  inherited Resize;

  RecalcControl;
end;

procedure TCTRPTrackBar.Loaded;
begin
  inherited Loaded;
end;

procedure TCTRPTrackBar.DefaultSkin;
begin
  // Set call change enable to false to protect before calling Change event

  Background.CallChangeEnable := False;
  Background.BackgroundAlpha := 0;
  Background.CallChangeEnable := True;

  Track.CallChangeEnable := False;
  with Track.TrackSpacing do
  begin
    Left := 12;
    Right := 12;
    Top := 13;
    Bottom := 13;
  end;
  Track.TrackBorder.BorderLeft.BorderWidth := 1;
  Track.TrackBorder.BorderLeft.BorderColor := RGBToColor(160, 160, 160);
  Track.TrackBorder.BorderTop.BorderWidth := 1;
  Track.TrackBorder.BorderTop.BorderColor := RGBToColor(160, 160, 160);
  Track.TrackBorder.BorderRight.BorderWidth := 1;
  Track.TrackBorder.BorderRight.BorderColor := RGBToColor(255, 255, 255);
  Track.TrackBorder.BorderBottom.BorderWidth := 1;
  Track.TrackBorder.BorderBottom.BorderColor := RGBToColor(255, 255, 255);
  Track.TrackBackground.BackgroundColor := RGBToColor(168, 221, 255);
  Track.TrackBackground.BackgroundAlpha := 255;
  Track.CallChangeEnable := True;

  Thumb.CallChangeEnable := False;
  Thumb.ThumbBorder.BorderLeft.BorderWidth := 1;
  Thumb.ThumbBorder.BorderLeft.BorderColor := RGBToColor(255, 255, 255);
  Thumb.ThumbBorder.BorderTop.BorderWidth := 1;
  Thumb.ThumbBorder.BorderTop.BorderColor := RGBToColor(255, 255, 255);
  Thumb.ThumbBorder.BorderRight.BorderWidth := 1;
  Thumb.ThumbBorder.BorderRight.BorderColor := RGBToColor(160, 160, 160);
  Thumb.ThumbBorder.BorderBottom.BorderWidth := 1;
  Thumb.ThumbBorder.BorderBottom.BorderColor := RGBToColor(160, 160, 160);
  Thumb.ThumbBackground.BackgroundColor := RGBToColor(206, 206, 206);
  Thumb.ThumbBackground.BackgroundAlpha := 255;
  Thumb.CallChangeEnable := True;
end;

procedure TCTRPTrackBar.DefaultSkin2;
begin
  Background.CallChangeEnable := False;
  Background.BackgroundAlpha := 0;
  Background.CallChangeEnable := True;

  Track.CallChangeEnable := False;
  with Track.TrackSpacing do
  begin
    Left := 12;
    Right := 12;
    Top := 13;
    Bottom := 13;
  end;
  Track.TrackBorder.BorderLeft.BorderWidth := 1;
  Track.TrackBorder.BorderLeft.BorderColor := RGBToColor(160, 160, 160);
  Track.TrackBorder.BorderTop.BorderWidth := 1;
  Track.TrackBorder.BorderTop.BorderColor := RGBToColor(160, 160, 160);
  Track.TrackBorder.BorderRight.BorderWidth := 1;
  Track.TrackBorder.BorderRight.BorderColor := RGBToColor(255, 255, 255);
  Track.TrackBorder.BorderBottom.BorderWidth := 1;
  Track.TrackBorder.BorderBottom.BorderColor := RGBToColor(255, 255, 255);
  Track.TrackBackground.BackgroundColor := RGBToColor(170, 255, 160);
  Track.TrackBackground.BackgroundAlpha := 255;
  Track.CallChangeEnable := True;

  Thumb.CallChangeEnable := False;
  Thumb.ThumbBorder.BorderLeft.BorderWidth := 1;
  Thumb.ThumbBorder.BorderLeft.BorderColor := RGBToColor(255, 255, 255);
  Thumb.ThumbBorder.BorderTop.BorderWidth := 1;
  Thumb.ThumbBorder.BorderTop.BorderColor := RGBToColor(255, 255, 255);
  Thumb.ThumbBorder.BorderRight.BorderWidth := 1;
  Thumb.ThumbBorder.BorderRight.BorderColor := RGBToColor(160, 160, 160);
  Thumb.ThumbBorder.BorderBottom.BorderWidth := 1;
  Thumb.ThumbBorder.BorderBottom.BorderColor := RGBToColor(160, 160, 160);
  Thumb.ThumbBackground.BackgroundColor := RGBToColor(255, 172, 206);
  Thumb.ThumbBackground.BackgroundAlpha := 255;
  Thumb.CallChangeEnable := True;

  RecalcControl;
end;

end.

