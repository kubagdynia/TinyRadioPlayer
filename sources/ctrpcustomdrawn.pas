unit CTRPCustomDrawn;
{===============================================================================
File:                CTRPCustomDrawn.pas

Application Name:    Tiny Radio Player

Created:             2019 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Based on BGRACustomDrawn

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FPCanvas, Graphics, Controls, Math, LazUTF8, ExtCtrls,
  { CustomDrawn }
  CustomDrawnControls, CustomDrawnDrawers, CustomDrawn_Common,
  { BGRABitmap }
  BGRABitmap, BGRABitmapTypes;

type

  TNotifyTextEvent = procedure(ASender: TObject; AText: string) of object;

  { TCTRPEdit }

  TCTRPEdit = class(TCDEdit)
  private
    FOnDelayChange: TNotifyTextEvent;
    DelayChangeTimer: TTimer;
    procedure DelayChangeTimerTimer(Sender: TObject);
  protected
    function GetDelayChangeInterval: Cardinal;
    procedure SetDelayChangeInterval(Value: Cardinal);
    procedure DoChange; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnDelayChange: TNotifyTextEvent read FOnDelayChange write FOnDelayChange;
    property DelayChangeInterval: Cardinal read GetDelayChangeInterval write SetDelayChangeInterval default 500;
  end;

  { TCTRPDrawer }

  TCTRPDrawer = class(TCDDrawerCommon)
  private
    FRandSeed: integer;

    function MixingColors(const MyColor1, MyColor2: TColor;
      const Proportion1, Proportion2: integer): TColor;
    function InvertColor(const Color: TColor): TColor;
  protected
    procedure AssignFont(Bitmap: TBGRABitmap; Font: TFont);
  public
    constructor Create; override;

    { General }
    function GetMeasures(AMeasureID: integer): integer; override;
    procedure DrawTickmark(ADest: TFPCustomCanvas; ADestPos: TPoint;
      AState: TCDControlState); override;
    function DPIAdjustment(const AValue: integer): integer;

    { Edit }
    procedure DrawEditBackground(ADest: TCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; {%H-}AStateEx: TCDEditStateEx); override;
    procedure DrawEditFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; {%H-}AStateEx: TCDEditStateEx); override;
    procedure DrawCaret(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    procedure DrawEdit(ADest: TCanvas; ASize: TSize; AState: TCDControlState;
      AStateEx: TCDEditStateEx); override;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{$IFDEF FPC}procedure Register;
begin
  RegisterComponents('CTRP Custom Drawn', [TCTRPEdit]);
end;
{$ENDIF}

{ TCTRPEdit }

constructor TCTRPEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  DelayChangeTimer := TTimer.Create(Self);
  DelayChangeTimer.Interval := 500;
  DelayChangeTimer.Enabled := False;
  DelayChangeTimer.OnTimer := @DelayChangeTimerTimer;
end;

destructor TCTRPEdit.Destroy;
begin
  FreeAndNil(DelayChangeTimer);

  inherited Destroy;
end;

procedure TCTRPEdit.DelayChangeTimerTimer(Sender: TObject);
begin
  if Sender is TTimer then
    TTimer(Sender).Enabled := False;

  if Assigned(OnDelayChange) then
    FOnDelayChange(Self, Text);
end;

function TCTRPEdit.GetDelayChangeInterval: Cardinal;
begin
  Result := DelayChangeTimer.Interval;
end;

procedure TCTRPEdit.SetDelayChangeInterval(Value: Cardinal);
begin
  if (Value <> DelayChangeTimer.Interval) then
  begin
    DelayChangeTimer.Interval := Value;
  end;
end;

procedure TCTRPEdit.DoChange;
begin
  inherited DoChange;

  DelayChangeTimer.Enabled := False;
  DelayChangeTimer.Enabled := True;
end;

{ TCTRPDrawer }

constructor TCTRPDrawer.Create;
begin
  inherited Create;

  Randomize;
  FRandSeed := RandSeed;
end;

function TCTRPDrawer.MixingColors(const MyColor1, MyColor2: TColor;
  const Proportion1, Proportion2: integer): TColor;
var
  color: Longint;
  r1, g1, b1, r2, g2, b2: Byte;
begin
  color  := ColorToRGB(MyColor1);
  RedGreenBlue(color, r1, g1, b1);

  color  := ColorToRGB(MyColor2);
  RedGreenBlue(color, r2, g2, b2);

  r1 := Round((r1 * Proportion1 + r2 * Proportion2) / (Proportion1 + Proportion2));
  g1 := Round((g1 * Proportion1 + g2 * Proportion2) / (Proportion1 + Proportion2));
  b1 := Round((b1 * Proportion1 + b2 * Proportion2) / (Proportion1 + Proportion2));
  result := (r1 or (g1 shl 8) or (b1 shl 16));
end;

function TCTRPDrawer.InvertColor(const Color: TColor): TColor;
var
  r, g, b: Byte;
begin
  RedGreenBlue(Color, r, g, b);
  result := ((255 - r) or ((255 - g) shl 8) or ((255 - b) shl 16));
end;

procedure TCTRPDrawer.AssignFont(Bitmap: TBGRABitmap; Font: TFont);
begin
  Bitmap.FontName := Font.Name;
  Bitmap.FontStyle := Font.Style;
  Bitmap.FontHeight := Font.Height;
  Bitmap.FontQuality := fqSystemClearType;
end;

function TCTRPDrawer.GetMeasures(AMeasureID: integer): integer;
begin
  case AMeasureID of
    TCDEDIT_LEFT_TEXT_SPACING: Result := 3;
    TCDEDIT_RIGHT_TEXT_SPACING: Result := 3;
    TCDEDIT_TOP_TEXT_SPACING: Result := 3;
    TCDEDIT_BOTTOM_TEXT_SPACING: Result := 3;

    TCDCHECKBOX_SQUARE_HALF_HEIGHT: Result :=
        Floor(GetMeasures(TCDCHECKBOX_SQUARE_HEIGHT) / 2);
    TCDCHECKBOX_SQUARE_HEIGHT: Result := DPIAdjustment(13);

    TCDCOMBOBOX_DEFAULT_HEIGHT: Result := 21;

    TCDRADIOBUTTON_CIRCLE_HEIGHT: Result := DPIAdjustment(13);

    TCDSCROLLBAR_BUTTON_WIDTH: Result := 17;
    TCDSCROLLBAR_LEFT_SPACING: Result := 17;
    TCDSCROLLBAR_RIGHT_SPACING: Result := 17;
    TCDSCROLLBAR_LEFT_BUTTON_POS: Result := 0;
    TCDSCROLLBAR_RIGHT_BUTTON_POS: Result := -17;

    TCDTRACKBAR_LEFT_SPACING: Result := 9;
    TCDTRACKBAR_RIGHT_SPACING: Result := 9;
    TCDTRACKBAR_TOP_SPACING: Result := 5;
    TCDTRACKBAR_FRAME_HEIGHT: Result := DPIAdjustment(17);

    TCDLISTVIEW_COLUMN_LEFT_SPACING: Result := 10;
    TCDLISTVIEW_COLUMN_RIGHT_SPACING: Result := 10;
    TCDLISTVIEW_COLUMN_TEXT_LEFT_SPACING: Result := 5;
    TCDLISTVIEW_LINE_TOP_SPACING: Result := 3;
    TCDLISTVIEW_LINE_BOTTOM_SPACING: Result := 3;

    TCDTOOLBAR_ITEM_SPACING: Result := 2;
    TCDTOOLBAR_ITEM_ARROW_WIDTH: Result := 7;
    TCDTOOLBAR_ITEM_BUTTON_DEFAULT_WIDTH: Result := 23;
    TCDTOOLBAR_ITEM_ARROW_RESERVED_WIDTH: Result := 35 - 23;
    TCDTOOLBAR_ITEM_SEPARATOR_DEFAULT_WIDTH: Result := 8;
    TCDTOOLBAR_DEFAULT_HEIGHT: Result := 26;

    TCDCTABCONTROL_CLOSE_TAB_BUTTON_WIDTH: Result := 10;
    TCDCTABCONTROL_CLOSE_TAB_BUTTON_EXTRA_SPACING: Result := 10;
    else
      Result := 0;
  end;
end;

procedure TCTRPDrawer.DrawTickmark(ADest: TFPCustomCanvas; ADestPos: TPoint;
  AState: TCDControlState);
begin
  inherited DrawTickmark(ADest, ADestPos, AState);
end;

function TCTRPDrawer.DPIAdjustment(const AValue: integer): integer;
begin
  { Adjustment that works under Windows }
  Result := ScaleY(AValue, 96);
end;

procedure TCTRPDrawer.DrawEditBackground(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  Bitmap: TBGRABitmap;
  ccc, cc2, cc3, cc4: TBGRAPixel;
begin
  ccc := ColorToBGRA(AStateEx.RGBColor);
  cc2 := ColorToBGRA(MixingColors(AStateEx.RGBColor, clWhite, 80, 20));
  cc3 := ColorToBGRA(MixingColors(AStateEx.RGBColor, clBlack, 80, 20));
  cc4 := ColorToBGRA(MixingColors(AStateEx.RGBColor, clBlack, 50, 50));

  Bitmap := TBGRABitmap.Create(ASize.cx, ASize.cy);

  if csfEnabled in AState then
  begin
    if csfHasFocus in AState then
      { Focused }
      Bitmap.Fill(cc2)
    else
      { Normal }
      Bitmap.Fill(ccc)
  end
  else
    { Disabled }
    Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, cc3, cc4, dmSet);

  Bitmap.Draw(TCanvas(ADest), ADestPos.x, ADestPos.y, True);
  Bitmap.Free;
end;

procedure TCTRPDrawer.DrawEditFrame(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  Bitmap: TBGRABitmap;
  ccc, cc3, cc4: TBGRAPixel;
begin
  ccc := ColorToBGRA(AStateEx.RGBColor);
  cc3 := ColorToBGRA(MixingColors(AStateEx.RGBColor, clBlack, 50, 50));
  cc4 := ColorToBGRA(MixingColors(AStateEx.RGBColor, clBlack, 60, 40));

  Bitmap := TBGRABitmap.Create(ASize.cx, ASize.cy);

  if csfEnabled in AState then
  begin
    if csfHasFocus in AState then
    begin
      { Focused }
      Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy, ccc, dmSet);
      Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, cc3, dmSet);
    end
    else
    begin
      { Normal }
      Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy, cc4, dmSet);
      Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, cc3, dmSet);
    end;
  end
  else
  begin
    { Disabled }
    Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy, cc4, dmSet);
    Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, cc3, dmSet);
  end;

  Bitmap.Draw(TCanvas(ADest), ADestPos.x, ADestPos.y, False);
  Bitmap.Free;
end;

procedure TCTRPDrawer.DrawCaret(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  lTextTopSpacing, lCaptionHeight, lLineHeight, lLineTop: Integer;
  lControlText, lTmpText: string;
  lTextBottomSpacing, lCaretPixelPos: Integer;
begin
  if not AStateEx.CaretIsVisible then Exit;

  if AStateEx.Lines.Count = 0 then
    lControlText := ''
  else
    lControlText := AStateEx.Lines.Strings[AStateEx.CaretPos.Y];

  lCaptionHeight := GetMeasuresEx(ADest, TCDCONTROL_CAPTION_HEIGHT, AState, AStateEx);
  lTextBottomSpacing := GetMeasures(TCDEDIT_BOTTOM_TEXT_SPACING);
  lTextTopSpacing := GetMeasures(TCDEDIT_TOP_TEXT_SPACING);
  lLineHeight := ADest.TextHeight(cddTestStr) + 2;
  lLineHeight := Min(ASize.cy-lTextBottomSpacing, lLineHeight);
  lLineTop := lTextTopSpacing + AStateEx.CaretPos.Y * lLineHeight;

  lTmpText := UTF8Copy(lControlText, AStateEx.VisibleTextStart.X, AStateEx.CaretPos.X-AStateEx.VisibleTextStart.X+1);
  lTmpText :=  VisibleText(lTmpText, AStateEx.PasswordChar);
  lCaretPixelPos := ADest.TextWidth(lTmpText) + GetMeasures(TCDEDIT_LEFT_TEXT_SPACING) + AStateEx.LeftTextMargin;
  ADest.Pen.Color := AStateEx.Font.Color;
  ADest.Pen.Style := psSolid;
  ADest.Line(lCaretPixelPos, lLineTop, lCaretPixelPos, lLineTop+lCaptionHeight);
end;

procedure TCTRPDrawer.DrawEdit(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  lVisibleText, lControlText: TCaption;
  lSelLeftPos, lSelLeftPixelPos, lSelLength, lSelRightPos: integer;
  lTextWidth, lLineHeight, lLineTop: integer;
  lControlTextLen: PtrInt;
  lTextLeftSpacing, lTextTopSpacing, lTextBottomSpacing: integer;
  lTextColor: TColor;
  i, lVisibleLinesCount: integer;
  lMixedFontColor, lBackgroundColor: TColor;
begin
  // Prepare colors
  lMixedFontColor := MixingColors(AStateEx.Font.Color, AStateEx.RGBColor, 50, 50);
  lBackgroundColor := ColorToBGRA(MixingColors(AStateEx.RGBColor, clWhite, 80, 20));

  // Background
  DrawEditBackground(ADest, Point(0, 0), ASize, AState, AStateEx);

  // General text configurations which apply to all lines
  // Configure the text color
  if csfEnabled in AState then
    lTextColor := lMixedFontColor
  else
    lTextColor := lMixedFontColor;

  if csfHasFocus in AState then
    lTextColor := AStateEx.Font.Color;

  ADest.Brush.Style := bsClear;
  ADest.Font.Assign(AStateEx.Font);
  ADest.Font.Color := lTextColor;
  lTextLeftSpacing := GetMeasures(TCDEDIT_LEFT_TEXT_SPACING);
  lTextTopSpacing := GetMeasures(TCDEDIT_TOP_TEXT_SPACING);
  lTextBottomSpacing := GetMeasures(TCDEDIT_BOTTOM_TEXT_SPACING);

  lLineHeight := ADest.TextHeight(cddTestStr) + 2;
  lLineHeight := Min(ASize.cy - lTextBottomSpacing, lLineHeight);

  // Fill this to be used in other parts
  AStateEx.LineHeight := lLineHeight;
  AStateEx.FullyVisibleLinesCount := ASize.cy - lTextTopSpacing - lTextBottomSpacing;
  AStateEx.FullyVisibleLinesCount := AStateEx.FullyVisibleLinesCount div lLineHeight;
  AStateEx.FullyVisibleLinesCount := Min(AStateEx.FullyVisibleLinesCount, AStateEx.Lines.Count);

  // Calculate how many lines to draw
  if AStateEx.Multiline then
    lVisibleLinesCount := AStateEx.FullyVisibleLinesCount + 1
  else
    lVisibleLinesCount := 1;

  lVisibleLinesCount := Min(lVisibleLinesCount, AStateEx.Lines.Count);

  // Now draw each line
  for i := 0 to lVisibleLinesCount - 1 do
  begin
    lControlText := AStateEx.Lines.Strings[AStateEx.VisibleTextStart.Y + i];
    lControlText := VisibleText(lControlText, AStateEx.PasswordChar);
    lControlTextLen := UTF8Length(lControlText);
    lLineTop := lTextTopSpacing + i * lLineHeight;

    // The text
    ADest.Pen.Style := psClear;
    ADest.Brush.Style := bsClear;

    // ToDo: Implement multi-line selection
    if (AStateEx.SelLength = 0) or (AStateEx.SelStart.Y <> AStateEx.VisibleTextStart.Y + i) then
    begin
      lVisibleText := UTF8Copy(lControlText, AStateEx.VisibleTextStart.X, lControlTextLen);
      ADest.TextOut(lTextLeftSpacing, lLineTop, lVisibleText);
    end
    // Text and Selection
    else
    begin
      lSelLeftPos := AStateEx.SelStart.X;
      if AStateEx.SelLength < 0 then
        lSelLeftPos := lSelLeftPos + AStateEx.SelLength;

      lSelRightPos := AStateEx.SelStart.X;
      if AStateEx.SelLength > 0 then
        lSelRightPos := lSelRightPos + AStateEx.SelLength;

      lSelLength := AStateEx.SelLength;
      if lSelLength < 0 then
        lSelLength := lSelLength * -1;

      // Text left of the selection
      lVisibleText := UTF8Copy(lControlText, AStateEx.VisibleTextStart.X,
        lSelLeftPos - AStateEx.VisibleTextStart.X + 1);
      ADest.TextOut(lTextLeftSpacing, lLineTop, lVisibleText);
      lSelLeftPixelPos := ADest.TextWidth(lVisibleText) + lTextLeftSpacing;

      // The selection background
      lVisibleText := UTF8Copy(lControlText, lSelLeftPos + 1, lSelLength);
      lTextWidth := ADest.TextWidth(lVisibleText);
      ADest.Brush.Color := lMixedFontColor;
      ADest.Brush.Style := bsSolid;
      ADest.Rectangle(Bounds(lSelLeftPixelPos, lLineTop, lTextWidth, lLineHeight));
      ADest.Brush.Style := bsClear;

      // The selection text
      ADest.Font.Color := InvertColor(AStateEx.Font.Color);
      ADest.TextOut(lSelLeftPixelPos, lLineTop, lVisibleText);
      lSelLeftPixelPos := lSelLeftPixelPos + lTextWidth;

      // Text right of the selection
      ADest.Brush.Color := lBackgroundColor;
      ADest.Font.Color := lTextColor;
      lVisibleText := UTF8Copy(lControlText, lSelLeftPos + lSelLength + 1, lControlTextLen);
      ADest.TextOut(lSelLeftPixelPos, lLineTop, lVisibleText);
    end;
  end;

  // And the caret
  DrawCaret(ADest, Point(0, 0), ASize, AState, AStateEx);

  // In the end the frame, because it must be on top of everything
  DrawEditFrame(ADest, Point(0, 0), ASize, AState, AStateEx);
end;

initialization
  RegisterDrawer(TCTRPDrawer.Create, dsCommon);

end.

