unit CTRPTextScroll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, LMessages, ExtCtrls,
  BGRABitmap, BGRABitmapTypes, CTRPPersistent, CTRPCommon;

type

  { TCTRPTextScrollLineProperty }

  TCTRPTextScrollLineProperty = class(TCTRPProperty)
  const
    DEFAULT_BACKGROUND_COLOR = ({r} 0 or ({g} 0 shl 8) or ({b} 0 shl 16)); // Black
    DEFAULT_FONT_COLOR = ({r} 255 or ({g} 255 shl 8) or ({b} 255 shl 16)); // Black
  private
    { Private declarations }
    FBorder: TCTRPBordersProperty;
    FBackgroundColor: TColor;
    FFontColor: TColor;
    FScrollText: string;
    FBlinkTimer: TTimer;

    procedure SetBorder(AValue: TCTRPBordersProperty);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetFontColor(AValue: TColor);
    procedure SetScrollText(AValue: string);
    procedure BlinkTimerOnTimer(Sender: TObject);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent; ACallChange: boolean); override;
    function IsEqual(ALineProperty: TCTRPTextScrollLineProperty): boolean;
  published
    { Published declarations }
    property Border: TCTRPBordersProperty read FBorder write SetBorder;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor
      default DEFAULT_BACKGROUND_COLOR;
    property FontColor: TColor read FFontColor write SetFontColor
      default DEFAULT_FONT_COLOR;
    property ScrollText: string read FScrollText write SetScrollText;
  end;

  { TCTRPTextScrollLinesProperty }

  TCTRPTextScrollLinesProperty = class(TCTRPProperty)
  const
    DEFAULT_END_PERCENT = 50;
  private
    { Private declarations }
    FTextScrollLine1: TCTRPTextScrollLineProperty;
    FTextScrollLine2: TCTRPTextScrollLineProperty;
    FTextScrollLine1EndPercent: byte;
    procedure SetTextScrollLine1Property(AValue: TCTRPTextScrollLineProperty);
    procedure SetTextScrollLine2Property(AValue: TCTRPTextScrollLineProperty);
    procedure SetTextScrollLine1EndPercent(AValue: byte);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent; ACallChange: boolean); override;
    function IsEqual(ALinesProperty: TCTRPTextScrollLinesProperty): boolean;
  published
    { Published declarations }
    property TextScrollLine1: TCTRPTextScrollLineProperty
      read FTextScrollLine1 write SetTextScrollLine1Property;
    property TextScrollLine2: TCTRPTextScrollLineProperty
      read FTextScrollLine2 write SetTextScrollLine2Property;
    property TextScrollLine1EndPercent: byte
      read FTextScrollLine1EndPercent write SetTextScrollLine1EndPercent
      default DEFAULT_END_PERCENT;
  end;

  TCTRPTextScroll = class(TGraphicControl, ICTRPControl)
  const
    DEFAULT_SCROLL_SPEED_LINE = 500; // MS
  private
    { Private declarations }
    FTextScrollLines: TCTRPTextScrollLinesProperty;
    FBmp: TBGRABitmap;

    FLine1Rect: TRect;
    FLine2Rect: TRect;

    FProgressBarVisible: boolean;
    FProgressBarValue: integer;

    FFontSize, FDefaultFontSize: integer;

    FTimer: TTimer;

    procedure SetRttiPropertiesToIgnore(const AValue: string);
    function GetRttiPropertiesToIgnore: string;
    procedure SetTextScrollLines(AValue: TCTRPTextScrollLinesProperty);

    procedure SetProgressBarVisible(AValue: boolean);
    procedure SetProgressBarValue(AValue: integer);

    procedure FTextScrollLinesChange(ASender: TObject; AData: PtrInt);
    procedure FTimerTimer(Sender: TObject);
  protected
    { Protected declarations }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: Boolean); override;
    procedure Paint; override;
    procedure ReclcLinesRect;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property RttiPropertiesToIgnore: string
      read GetRttiPropertiesToIgnore write SetRttiPropertiesToIgnore;
    property Lines: TCTRPTextScrollLinesProperty
      read FTextScrollLines write SetTextScrollLines;
    property ProgressBarVisible: boolean
      read FProgressBarVisible write SetProgressBarVisible;
    property ProgressBarValue: integer
      read FProgressBarValue write SetProgressBarValue;

    property Constraints;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Hint;
    property Visible;
    property Align;
    property Anchors;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnStartDock;
    property OnStartDrag;
    property DragCursor;
    property DragKind;
    property DragMode;
  end;

implementation

uses typinfo;

{ TCTRPTextScrollLineProperty }

constructor TCTRPTextScrollLineProperty.Create(AControl: TControl);
begin
  inherited Create(AControl);

  FBorder := TCTRPBordersProperty.Create(AControl);
  FBorder.OnChange := @OnChangeProc;

  FBackgroundColor := DEFAULT_BACKGROUND_COLOR;
  FFontColor := DEFAULT_FONT_COLOR;

  // Create timer to be able to blinking new text
  FBlinkTimer := TTimer.Create(AControl);
  FBlinkTimer.Interval := 200;
  FBlinkTimer.OnTimer := @BlinkTimerOnTimer;
  FBlinkTimer.Tag := 0;
  FBlinkTimer.Enabled := False;
end;

destructor TCTRPTextScrollLineProperty.Destroy;
begin
  FreeAndNil(FBlinkTimer);
  FreeAndNil(FBorder);

  inherited Destroy;
end;


function TCTRPTextScrollLineProperty.IsEqual(
  ALineProperty: TCTRPTextScrollLineProperty): boolean;
begin
  Result := (ALineProperty <> nil) and
    (FBorder.IsEqual(ALineProperty.Border)) and
    (FBackgroundColor = ALineProperty.BackgroundColor) and
    (FScrollText = ALineProperty.ScrollText);
end;

procedure TCTRPTextScrollLineProperty.Assign(Source: TPersistent; ACallChange: boolean);
var
  srcLine: TCTRPTextScrollLineProperty;
begin
  if Source is TCTRPTextScrollLineProperty then
  begin
    srcLine := TCTRPTextScrollLineProperty(Source);
    if IsEqual(srcLine) then
      Exit;

    FBorder.Assign(srcLine.Border, False);
    FBackgroundColor := srcLine.BackgroundColor;
    FScrollText := srcLine.ScrollText;

    if ACallChange then
      Change;
  end
  else
    inherited Assign(Source);
end;

procedure TCTRPTextScrollLineProperty.BlinkTimerOnTimer(Sender: TObject);
begin

end;

procedure TCTRPTextScrollLineProperty.SetBorder(AValue: TCTRPBordersProperty);
begin
  FBorder.Assign(AValue);
end;

procedure TCTRPTextScrollLineProperty.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor = AValue then Exit;
  FBackgroundColor := AValue;

  Change;
end;

procedure TCTRPTextScrollLineProperty.SetFontColor(AValue: TColor);
begin
  if FFontColor = AValue then Exit;
  FFontColor := AValue;

  Change;
end;

procedure TCTRPTextScrollLineProperty.SetScrollText(AValue: string);
begin
  if FScrollText = AValue then Exit;
  FScrollText := AValue;

  Change;
end;

{ TCTRPTextScrollLinesProperty }

constructor TCTRPTextScrollLinesProperty.Create(AControl: TControl);
begin
  FTextScrollLine1 := TCTRPTextScrollLineProperty.Create(AControl);
  FTextScrollLine2 := TCTRPTextScrollLineProperty.Create(AControl);

  FTextScrollLine1.OnChange := @OnChangeProc;
  FTextScrollLine2.OnChange := @OnChangeProc;

  FTextScrollLine1EndPercent := DEFAULT_END_PERCENT;

  // Default background color
  FTextScrollLine1.BackgroundColor := RGBToColor(55, 55, 55);
  FTextScrollLine2.BackgroundColor := RGBToColor(26, 26, 26);

  inherited Create(AControl);
end;

destructor TCTRPTextScrollLinesProperty.Destroy;
begin
  FreeAndNil(FTextScrollLine1);
  FreeAndNil(FTextScrollLine2);

  inherited Destroy;
end;

procedure TCTRPTextScrollLinesProperty.Assign(Source: TPersistent;
  ACallChange: boolean);
var
  srcLines: TCTRPTextScrollLinesProperty;
begin
  if Source is TCTRPTextScrollLinesProperty then
  begin
    srcLines := TCTRPTextScrollLinesProperty(Source);
    if IsEqual(srcLines) then
      Exit;

    FTextScrollLine1EndPercent := srcLines.TextScrollLine1EndPercent;
    FTextScrollLine1.Assign(srcLines.TextScrollLine1, False);
    FTextScrollLine2.Assign(srcLines.TextScrollLine2, False);

    if ACallChange then
      Change;
  end
  else
    inherited Assign(Source);
end;

function TCTRPTextScrollLinesProperty.IsEqual(
  ALinesProperty: TCTRPTextScrollLinesProperty): boolean;
begin
  Result := (ALinesProperty <> nil) and
    (FTextScrollLine1.IsEqual(ALinesProperty.TextScrollLine1)) and
    (FTextScrollLine2.IsEqual(ALinesProperty.TextScrollLine2)) and
    (FTextScrollLine1EndPercent = ALinesProperty.TextScrollLine1EndPercent);
end;

procedure TCTRPTextScrollLinesProperty.SetTextScrollLine1Property(
  AValue: TCTRPTextScrollLineProperty);
begin
  FTextScrollLine1.Assign(AValue);
end;

procedure TCTRPTextScrollLinesProperty.SetTextScrollLine2Property(
  AValue: TCTRPTextScrollLineProperty);
begin
  FTextScrollLine2.Assign(AValue);
end;

procedure TCTRPTextScrollLinesProperty.SetTextScrollLine1EndPercent(AValue: byte);
begin
  if FTextScrollLine1EndPercent = AValue then
    Exit;

  FTextScrollLine1EndPercent := AValue;

  if FTextScrollLine1EndPercent > 100 then
    FTextScrollLine1EndPercent := 100
  else if FTextScrollLine1EndPercent < 0 then
    FTextScrollLine1EndPercent := 0;

  Change;
end;

{ TCTRPTextScroll }

constructor TCTRPTextScroll.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, 34);

  FBmp := nil;

  FTextScrollLines := TCTRPTextScrollLinesProperty.Create(Self);

  FProgressBarVisible := false;
  FProgressBarValue := 0;

  FDefaultFontSize := 12;
  FFontSize := FDefaultFontSize;


  ReclcLinesRect;

  FTextScrollLines.OnChange := @FTextScrollLinesChange;

  // Create timer that can support scroll message
  FTimer := TTimer.Create(Self);
  FTimer.Interval := DEFAULT_SCROLL_SPEED_LINE;
  FTimer.OnTimer := @FTimerTimer;
  FTimer.Enabled := False;

end;

destructor TCTRPTextScroll.Destroy;
begin
  FreeAndNil(FTimer);
  FreeAndNil(FTextScrollLines);
  FreeAndNil(FBmp);
  inherited Destroy;
end;

procedure TCTRPTextScroll.FTextScrollLinesChange(ASender: TObject; AData: PtrInt
  );
begin
  ReclcLinesRect;
  Invalidate;
end;

procedure TCTRPTextScroll.FTimerTimer(Sender: TObject);
begin
  if csDesigning in ComponentState then
    Exit;

  Invalidate;
end;

procedure TCTRPTextScroll.SetRttiPropertiesToIgnore(const AValue: string);
begin

end;

function TCTRPTextScroll.GetRttiPropertiesToIgnore: string;
begin
  Result := '';
end;

procedure TCTRPTextScroll.SetTextScrollLines(AValue: TCTRPTextScrollLinesProperty);
begin
  if FTextScrollLines.IsEqual(AValue) then
    Exit;
  FTextScrollLines.Assign(AValue, False);

  Invalidate;
end;

procedure TCTRPTextScroll.SetProgressBarVisible(AValue: boolean);
begin
  if FProgressBarVisible = AValue then
    Exit;
  FProgressBarVisible := AValue;

  Invalidate;
end;

procedure TCTRPTextScroll.SetProgressBarValue(AValue: integer);
begin
  if FProgressBarValue = AValue then
    Exit;
  FProgressBarValue := AValue;

  if FProgressBarVisible then
    Invalidate;
end;

procedure TCTRPTextScroll.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth  := 100;
  PreferredHeight := 33;
end;

procedure TCTRPTextScroll.Paint;
var
  tx, ty: integer;
  textStyle: TTextStyle;
begin
  //inherited Paint;
  tx := ClientWidth;
  ty := ClientHeight;

  with textStyle do
  begin
    Alignment := TAlignment.taCenter;
    Layout := TTextLayout.tlCenter;
    Wordbreak := False;
    ShowPrefix := False;
    Clipping := False;
    SingleLine := True;
  end;

  // Recalc lines rect if component size has changed
  if tx <> (FLine1Rect.Right - FLine1Rect.Left +
    Lines.TextScrollLine1.Border.BorderLeft.BorderWidth +
    Lines.TextScrollLine1.Border.BorderRight.BorderWidth) then ReclcLinesRect;

  if Assigned(FBmp) and ((FBmp.Width <> tx) or (FBmp.Height <> ty)) then
    FreeAndNil(FBmp);

  if not Assigned(FBmp) then
    FBmp := TBGRABitmap.Create(tx, ty)
  else
    FBmp.FillTransparent;

  FBmp.Rectangle(FLine1Rect,
    ColorToBGRA(Lines.TextScrollLine1.BackgroundColor),
    ColorToBGRA(Lines.TextScrollLine1.BackgroundColor), dmSet);

  FBmp.FontHeight := FFontSize;
  FBmp.FontAntialias := false;
  FBmp.FontQuality := fqSystemClearType;
  FBmp.FontName := 'Verdana';//'Arial';//'Verdana';

  // Left
  if Lines.TextScrollLine1.Border.BorderLeft.BorderWidth > 0 then
    FBmp.Rectangle(0, 0, Lines.TextScrollLine1.Border.BorderLeft.BorderWidth, FLine1Rect.Bottom,
      ColorToBGRA(Lines.TextScrollLine1.Border.BorderLeft.BorderColor, Lines.TextScrollLine1.Border.BorderLeft.BorderAlpha),
      ColorToBGRA(Lines.TextScrollLine1.Border.BorderLeft.BorderColor, Lines.TextScrollLine1.Border.BorderLeft.BorderAlpha),
      dmSet);

  // Top
  if Lines.TextScrollLine1.Border.BorderTop.BorderWidth > 0 then
    FBmp.Rectangle(Lines.TextScrollLine1.Border.BorderLeft.BorderWidth, 0, Self.Width, Lines.TextScrollLine1.Border.BorderTop.BorderWidth,
      ColorToBGRA(Lines.TextScrollLine1.Border.BorderTop.BorderColor, Lines.TextScrollLine1.Border.BorderTop.BorderAlpha),
      ColorToBGRA(Lines.TextScrollLine1.Border.BorderTop.BorderColor, Lines.TextScrollLine1.Border.BorderTop.BorderAlpha),
      dmSet);

  // Right
  if Lines.TextScrollLine1.Border.BorderRight.BorderWidth > 0 then
    FBmp.Rectangle(FLine1Rect.Right, FLine1Rect.Top, Self.Width, FLine1Rect.Bottom + Lines.TextScrollLine1.Border.BorderBottom.BorderWidth,
      ColorToBGRA(Lines.TextScrollLine1.Border.BorderRight.BorderColor, Lines.TextScrollLine1.Border.BorderRight.BorderAlpha),
      ColorToBGRA(Lines.TextScrollLine1.Border.BorderRight.BorderColor, Lines.TextScrollLine1.Border.BorderRight.BorderAlpha),
      dmSet);

  // Bottom
  if Lines.TextScrollLine1.Border.BorderBottom.BorderWidth > 0 then
    FBmp.Rectangle(0, FLine1Rect.Bottom, FLine1Rect.Right, FLine1Rect.Bottom + Lines.TextScrollLine1.Border.BorderBottom.BorderWidth,
      ColorToBGRA(Lines.TextScrollLine1.Border.BorderBottom.BorderColor, Lines.TextScrollLine1.Border.BorderBottom.BorderAlpha),
      ColorToBGRA(Lines.TextScrollLine1.Border.BorderBottom.BorderColor, Lines.TextScrollLine1.Border.BorderBottom.BorderAlpha),
      dmSet);

  // Draw text line 1
  FBmp.TextRect(FLine1Rect, FLine1Rect.Left, FLine2Rect.Top, Lines.TextScrollLine1.ScrollText, textStyle,
    ColorToBGRA(Lines.TextScrollLine1.FontColor));

  // PREPARE 2nd line
  FBmp.Rectangle(FLine2Rect,
    ColorToBGRA(Lines.TextScrollLine2.BackgroundColor),
    ColorToBGRA(Lines.TextScrollLine2.BackgroundColor), dmSet);

  // Left
  if Lines.TextScrollLine2.Border.BorderLeft.BorderWidth > 0 then
    FBmp.Rectangle(0, FLine2Rect.Top - Lines.TextScrollLine2.Border.BorderTop.BorderWidth, Lines.TextScrollLine2.Border.BorderLeft.BorderWidth, FLine2Rect.Bottom,
      ColorToBGRA(Lines.TextScrollLine2.Border.BorderLeft.BorderColor, Lines.TextScrollLine2.Border.BorderLeft.BorderAlpha),
      ColorToBGRA(Lines.TextScrollLine2.Border.BorderLeft.BorderColor, Lines.TextScrollLine2.Border.BorderLeft.BorderAlpha),
      dmSet);

  // Top
  if Lines.TextScrollLine2.Border.BorderTop.BorderWidth > 0 then
    FBmp.Rectangle(Lines.TextScrollLine2.Border.BorderLeft.BorderWidth, FLine2Rect.Top - Lines.TextScrollLine2.Border.BorderTop.BorderWidth, Self.Width, FLine2Rect.Top,
      ColorToBGRA(Lines.TextScrollLine2.Border.BorderTop.BorderColor, Lines.TextScrollLine2.Border.BorderTop.BorderAlpha),
      ColorToBGRA(Lines.TextScrollLine2.Border.BorderTop.BorderColor, Lines.TextScrollLine2.Border.BorderTop.BorderAlpha),
      dmSet);

  // Right
  if Lines.TextScrollLine2.Border.BorderRight.BorderWidth > 0 then
    FBmp.Rectangle(FLine2Rect.Right, FLine2Rect.Top, Self.Width, FLine2Rect.Bottom + Lines.TextScrollLine2.Border.BorderBottom.BorderWidth,
      ColorToBGRA(Lines.TextScrollLine2.Border.BorderRight.BorderColor, Lines.TextScrollLine2.Border.BorderRight.BorderAlpha),
      ColorToBGRA(Lines.TextScrollLine2.Border.BorderRight.BorderColor, Lines.TextScrollLine2.Border.BorderRight.BorderAlpha),
      dmSet);

  // Bottom
  if Lines.TextScrollLine2.Border.BorderBottom.BorderWidth > 0 then
    FBmp.Rectangle(0, FLine2Rect.Bottom, FLine2Rect.Right, FLine2Rect.Bottom + Lines.TextScrollLine2.Border.BorderBottom.BorderWidth,
      ColorToBGRA(Lines.TextScrollLine2.Border.BorderBottom.BorderColor, Lines.TextScrollLine2.Border.BorderBottom.BorderAlpha),
      ColorToBGRA(Lines.TextScrollLine2.Border.BorderBottom.BorderColor, Lines.TextScrollLine2.Border.BorderBottom.BorderAlpha),
      dmSet);

  // Draw text line 2
  FBmp.TextRect(Rect( FLine2Rect.Left-50, FLine2Rect.Top, FLine2Rect.Right+50, FLine2Rect.Bottom),
    FLine2Rect.Left, FLine2Rect.Top, Lines.TextScrollLine2.ScrollText, textStyle, ColorToBGRA(Lines.TextScrollLine2.FontColor));

  // Draw progress bar
  if ProgressBarVisible and (FProgressBarValue > 0) then
  begin
    FBmp.Rectangle(
      FLine2Rect.Left, FLine2Rect.Bottom - 5,
      Round(((FLine2Rect.Right - FLine2Rect.Left) / 100) * FProgressBarValue), FLine2Rect.Bottom,
      BGRA(0, 0, 0, 30), BGRA(0, 0, 0, 30), dmDrawWithTransparency);
  end;

  FBmp.Draw(Canvas, 0, 0, False);

end;

procedure TCTRPTextScroll.ReclcLinesRect;
begin
  FLine1Rect := Rect( 0, 0, Self.Width,
    Round(Self.Height * (Lines.TextScrollLine1EndPercent / 100)));

  // Inflate rect
  with FLine1Rect do
  begin
    Inc(Left, Lines.TextScrollLine1.Border.BorderLeft.BorderWidth);
    Inc(Top, Lines.TextScrollLine1.Border.BorderTop.BorderWidth);
    Dec(Right, Lines.TextScrollLine1.Border.BorderRight.BorderWidth);
    Dec(Bottom, Lines.TextScrollLine1.Border.BorderBottom.BorderWidth);
  end;

  FLine2Rect := Rect( 0,
    Round(Self.Height * (Lines.TextScrollLine1EndPercent / 100)), Self.Width,
    Round(Self.Height * (Lines.TextScrollLine1EndPercent / 100)) +
      Round(Self.Height * (Lines.TextScrollLine1EndPercent / 100)));

  // Inflate rect
  with FLine2Rect do
  begin
    Inc(Left, Lines.TextScrollLine2.Border.BorderLeft.BorderWidth);
    Inc(Top, Lines.TextScrollLine2.Border.BorderTop.BorderWidth);
    Dec(Right, Lines.TextScrollLine2.Border.BorderRight.BorderWidth);
    Dec(Bottom, Lines.TextScrollLine2.Border.BorderBottom.BorderWidth);
  end;

end;

function TCTRPTextScroll.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);

  if WheelDelta > 0 then
    Inc(FFontSize)
  else
    Dec(FFontSize);

  Invalidate;
end;

procedure TCTRPTextScroll.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbMiddle then
    FFontSize := FDefaultFontSize;

  Invalidate;
end;

procedure TCTRPTextScroll.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  // do nothing
end;



end.

