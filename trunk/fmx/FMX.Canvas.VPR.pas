unit FMX.Canvas.VPR;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Vectorial Polygon Rasterizer for FMX.
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2011
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I VPR.INC}

{
 --- FMX.Canvas.VPR ---

 See README.TXT for more info about FMX.Canvas.VPR.
 See VPR.INC for available conditional symbols.

 Current limitation of FMX:

  - no polygon fill mode support (non-zero / even-odd);
  - no 'square' line cap style;
  - no brush opacity property;
  - no hinting/cleartype/weight/kerning/letter-spacing/word-spacing font properties;
  - only 'wrap' and 'flip x' bitmap tile modes;
  - linear flattening of cubic beziers instead of a more optimal recursive scheme;
  - no proper support for flattening paths to polypolygons.

 Current limitations of the GDI+ and/or D2D canvases:

  - line thickness is truncated to nearest integer;
  - matrix property does not reflect actual matrix after invoking MultyMat;
  - no gamma correction;
  - no perfect antialiasing.

 Current limitations of the VPR canvas:

  - incomplete clipping support;
  - no support for bkGrab and bkResource brush kinds;
  - only supports integer font sizes (GDI limitation);
  - menus are painted incorrectly (GDI blending problem?);
  - VPR currently uses the SVG definition for spacing between dashed lines with
    round or squared caps (which is inconsistent with GDI+.)
}

procedure SetVPRDefault;

implementation

uses
  Winapi.Windows, FMX.Types, System.Types, System.Classes, System.SysUtils,
  System.UITypes, System.Math, FMX.VPR, FMX.VPR.Polygons, FMX.VPR.Blend;

type
  { TBitmapHelper }

  TBitmapHelper = class helper for TBitmap
  private
    function GetPixelLinear(X, Y, wX, wY: Integer): TAlphaColor; overload; inline;
    function GetPixelLinearWrap(const X, Y: Single): TAlphaColor;
    function GetPixelLinearMirror(const X, Y: Single): TAlphaColor;
  public
    function GetPixelNearest(const X, Y: Single): TAlphaColor;
    function GetPixelLinear(const X, Y: Single): TAlphaColor; overload;
  end;

  { TPathDataHelper }

  TPathDataHelper = class helper for TPathData
  public
    function FlattenToPolyPolygon(var PP: TPolyPolygon; const Flatness: Single = 0.25): TPointF;
  end;

  { TCanvasVPRSaveState }

  TCanvasVPRSaveState = class(TCanvasSaveState)
  private
    FClipRect: TRectF;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Assign(Source: TPersistent); override;
  end;

  TGetPixel = function(const X, Y: Single): TAlphaColor of object;

  { TCanvasVPR }

  TCanvasVPR = class(TCanvas)
  private
    FBitmapInfo: TBitmapInfo;
    FBufferBitmap: THandle;
    FClipRect: TRectF;
    FLinearGradientSlope: Single;
    FFillColor: TAlphaColor;
    FGradientStart: TPointF;
    FGradientStop: TPointF;
    FFillScaleX: Single;
    FFillScaleY: Single;
    FFillBitmap: TBitmap;
    FGetPixel: TGetPixel;
    FOpacity: Single;
    FFontHandle: HFONT;
    FBrush: TBrush;
    FInverse: TMatrix;
    FGradientLUT: array [Byte] of TAlphaColor;
    function CreateSaveState: TCanvasSaveState; override;
    function GetPixelMirror(const X, Y: Single): TAlphaColor;
    function GetPixelWrap(const X, Y: Single): TAlphaColor;
    function GetPixelStretch(const X, Y: Single): TAlphaColor;
    procedure PrepareGradientLUT;
  protected
    function GetRenderCallback: TRenderSpanEvent;
    procedure ClearOpaque(const Span: TValueSpan; DstY: Integer); virtual;
    procedure RenderOpaque(const Span: TValueSpan; DstY: Integer); virtual;
    procedure RenderOpaqueLCD(const Span: TValueSpan; DstY: Integer);
    procedure RenderLinearGradient(const Span: TValueSpan; DstY: Integer); virtual;
    procedure RenderRadialGradient(const Span: TValueSpan; DstY: Integer); virtual;
    procedure RenderBitmap(const Span: TValueSpan; DstY: Integer); virtual;
    procedure RenderGrab(const Span: TValueSpan; DstY: Integer); virtual;
    procedure RenderResource(const Span: TValueSpan; DstY: Integer); virtual;
    procedure PrepareFill(const ARect: TRectF; ABrush: TBrush; AOpacity: Single);
    procedure FillPolyPolygonLCD(var PP: TPolyPolygon; const AOpacity: Single); virtual;
    procedure FillPolygon(const Polygon: TPolygon; const AOpacity: Single); override;
    procedure DrawPolygon(const Polygon: TPolygon; const AOpacity: Single); override;
    procedure DrawPolyPolygon(const PP: TPolyPolygon; const AOpacity: Single); virtual;
    procedure FillPolyPolygon(var PP: TPolyPolygon; const AOpacity: Single); virtual;
    procedure FontChanged(Sender: TObject); override;
    class function GetBitmapScanline(Bitmap: TBitmap; Y: integer): PAlphaColorArray; override;
    { Bitmaps }
    procedure UpdateBitmapHandle(ABitmap: TBitmap); override;
    procedure DestroyBitmapHandle(ABitmap: TBitmap); override;
    procedure FreeBuffer; override;
    function TransformedClipRect: TRectF;
    function GetEndStyle: TEndStyle;
    function GetJoinStyle: TJoinStyle;
  public
    constructor CreateFromWindow(const AParent: THandle; const AWidth, AHeight: integer); override;
    constructor CreateFromBitmap(const ABitmap: TBitmap); override;
    constructor CreateFromPrinter(const APrinter: TAbstractPrinter); override;
    destructor Destroy; override;
    { buffer }
    procedure ResizeBuffer(const AWidth, AHeight: Integer); override;
    procedure FlushBufferRect(const X, Y: integer; const Context; const ARect: TRectF); override;
    procedure Clear(const Color: TAlphaColor); override;
    procedure ClearRect(const ARect: TRectF; const AColor: TAlphaColor = 0); override;
    { matrix }
    procedure SetMatrix(const M: TMatrix); override;
    procedure MultyMatrix(const M: TMatrix); override;
    { clipping }
    procedure SetClipRects(const ARects: array of TRectF); override;
    procedure IntersectClipRect(const ARect: TRectF); override;
    procedure ExcludeClipRect(const ARect: TRectF); override;
    procedure ResetClipRect; override;
    { drawing }
    procedure DrawLine(const APt1, APt2: TPointF; const AOpacity: Single); override;
    procedure FillRect(const ARect: TRectF; const XRadius, YRadius: Single;
      const ACorners: TCorners; const AOpacity: Single;
      const ACornerType: TCornerType = TCornerType.ctRound); override;
    procedure DrawRect(const ARect: TRectF; const XRadius, YRadius: Single;
      const ACorners: TCorners; const AOpacity: Single;
      const ACornerType: TCornerType = TCornerType.ctRound); override;
    procedure FillEllipse(const ARect: TRectF; const AOpacity: Single); override;
    procedure DrawEllipse(const ARect: TRectF; const AOpacity: Single); override;
    function LoadFontFromStream(AStream: TStream): boolean; override;
    procedure FillText(const ARect: TRectF; const AText: String;
      const WordWrap: boolean; const AOpacity: Single;
      const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.taCenter); override;
    procedure MeasureText(var ARect: TRectF;
      const AText: String; const WordWrap: boolean;
      const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.taCenter); override;
    function TextToPath(Path: TPathData; const ARect: TRectF;
      const AText: String; const WordWrap: boolean;
      const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.taCenter): boolean; override;
    function PtInPath(const APoint: TPointF; const APath: TPathData): boolean; override;
    procedure FillPathLCD(const APath: TPathData; const AOpacity: Single);
    procedure FillPath(const APath: TPathData; const AOpacity: Single); override;
    procedure DrawPath(const APath: TPathData; const AOpacity: Single); override;
    procedure DrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF;
      const AOpacity: Single; const HighSpeed: boolean = false); override;
    procedure DrawThumbnail(const ABitmap: TBitmap; const Width, Height: Single); override;
  end;

type
  PRGBTriple = ^TRGBTriple;
  TRGBTriple = packed record
    B, G, R: Byte;
  end;

  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [0..0] of TRGBTriple;

  TMakeAlphaProcLCD = procedure(Coverage: PSingleArray; AlphaValues: PByteArray;
    Count: Integer; Color: TAlphaColor);

{$IFDEF CPUX64}
function DivMod(Dividend, Divisor: Integer; var Remainder: Integer): Integer;
asm
        mov       rax,rcx
        mov       r9,rdx
        cdq
        idiv      r9
        mov       dword ptr [r8], edx
end;
{$ELSE}
function DivMod(Dividend, Divisor: Integer; var Remainder: Integer): Integer;
asm
        push      edx
        cdq
        idiv      dword ptr [esp]
        add       esp,$04
        mov       dword ptr [ecx], edx
end;
{$ENDIF}

procedure MakeAlphaNonZeroLCD(Coverage: PSingleArray; AlphaValues: PByteArray;
  Count: Integer; Color: TAlphaColor);
var
  I: Integer;
  M, V: Cardinal;
  Last: Single;
begin
{$IFNDEF CLEARTYPEHQ}
  M := Color shr 24 * 86;  // 86 = 258 / 3
{$ELSE}
  M := Color shr 24 * 29;
{$ENDIF}

  Last := Infinity;
  V := 0;
  AlphaValues[0] := 0;
  AlphaValues[1] := 0;
  for I := 0 to Count - 1 do
  begin
    if PInteger(@Last)^ <> PInteger(@Coverage[I])^ then
    begin
      Last := Coverage[I];
      V := Abs(Round(Last * $1000));
      if V > $1000 then V := $1000;
{$IFDEF GAMMACORRECTION}
      V := GAMMA_TABLE[V];
{$ENDIF}
      V := V * M shr 20;
    end;
    Inc(AlphaValues[I], V);
    Inc(AlphaValues[I + 1], V);
    AlphaValues[I + 2] := V;
  end;
  AlphaValues[Count + 2] := 0;
  AlphaValues[Count + 3] := 0;
end;

procedure MakeAlphaEvenOddLCD(Coverage: PSingleArray; AlphaValues: PByteArray;
  Count: Integer; Color: TAlphaColor);
var
  I: Integer;
  M, V: Cardinal;
  Last: Single;
begin
  //M := Color shr 24 * 86;  // 86 = 258 / 3
  M := Color shr 24 * 29;

  Last := Infinity;
  V := 0;
  AlphaValues[0] := 0;
  AlphaValues[1] := 0;
  for I := 0 to Count - 1 do
  begin
    if PInteger(@Last)^ <> PInteger(@Coverage[I])^ then
    begin
      Last := Coverage[I];
      V := Abs(Round(Last * $1000));
      V := V and $1fff;
      if V >= $1000 then V := V xor $1fff;
{$IFDEF GAMMACORRECTION}
      V := GAMMA_TABLE[V];
{$ENDIF}
      V := V * M shr 20;
    end;
    Inc(AlphaValues[I], V);
    Inc(AlphaValues[I + 1], V);
    AlphaValues[I + 2] := V;
  end;
  AlphaValues[Count + 2] := 0;
  AlphaValues[Count + 3] := 0;
end;


procedure MakeAlphaNonZeroLCD2(Coverage: PSingleArray; AlphaValues: PByteArray;
  Count: Integer; Color: TAlphaColor);
var
  I: Integer;
begin
  MakeAlphaNonZeroLCD(Coverage, AlphaValues, Count, Color);
  AlphaValues[Count + 2] := AlphaValues[Count] + AlphaValues[Count + 1];
  AlphaValues[Count + 3] := AlphaValues[Count + 1];
  for I := Count + 1 downto 2 do
  begin
{$IFDEF CLEARTYPESMOOTH}
    AlphaValues[I] := AlphaValues[I]*2 - AlphaValues[I - 1] + AlphaValues[I - 2]*2;
{$ELSE}
    AlphaValues[I] := AlphaValues[I] + AlphaValues[I - 1] + AlphaValues[I - 2];
{$ENDIF}
  end;
  AlphaValues[1] := AlphaValues[0] + AlphaValues[1];
  AlphaValues[0] := AlphaValues[0];
end;

procedure MakeAlphaEvenOddLCD2(Coverage: PSingleArray; AlphaValues: PByteArray;
  Count: Integer; Color: TAlphaColor);
var
  I: Integer;
begin
  MakeAlphaEvenOddLCD(Coverage, AlphaValues, Count, Color);
  AlphaValues[Count + 2] := (AlphaValues[Count] + AlphaValues[Count + 1]) div 3;
  AlphaValues[Count + 3] := AlphaValues[Count + 1] div 3;
  for I := Count + 1 downto 2 do
  begin
    AlphaValues[I] := (AlphaValues[I] + AlphaValues[I - 1] + AlphaValues[I - 2]) div 3;
  end;
  AlphaValues[1] := (AlphaValues[0] + AlphaValues[1]) div 3;
  AlphaValues[0] := AlphaValues[0] div 3;
end;

procedure CombineLineLCD(Weights: PRGBTripleArray; Dst: PAlphaColorArray; Color: TAlphaColor; Count: Integer);
var
  I: Integer;
{$IFDEF PUREPASCAL}
  C: TColorRec absolute Color;
  W: PRGBTriple;
{$ENDIF}
begin
  for I := 0 to Count - 1 do
  begin
{$IFDEF PUREPASCAL}
    W := @Weights[I];
    with PColorRec(@Dst[I])^ do
    begin
      R := (C.R - R) * W.R div 255 + R;
      G := (C.G - G) * W.G div 255 + G;
      B := (C.B - B) * W.B div 255 + B;
    end;
{$ELSE}
    BlendRGB(Color, PColor(@Weights[I])^, Dst[I]);
{$ENDIF}
  end;
end;

function TransformPoint(const P: TPointF; const M: TMatrix): TPointF;
begin
  Result.X := P.X * M.M[0].V[0] + P.Y * M.M[1].V[0] + M.M[2].V[0];
  Result.Y := P.X * M.M[0].V[1] + P.Y * M.M[1].V[1] + M.M[2].V[1];
end;

function TransformPolygon(const P: TPolygon; const M: TMatrix): TPolygon;
var
  I: Integer;
begin
  SetLength(Result, Length(P));
  for I := Low(P) to High(P) do
    Result[I] := TransformPoint(P[I], M);
end;

procedure TransformPolyPolygon(var P: TPolyPolygon; const M: TMatrix);
var
  I: Integer;
begin
  for I := Low(P) to High(P) do
    P[I] := TransformPolygon(P[I], M);
end;

function MakePolyPolygon(const P: TPolygon): TPolyPolygon;
var
  I, J, K, L: Integer;
begin
  J := 0;
  K := 0;
  L := 0;
  for I := Low(P) to High(P) do
  begin
    Inc(K);
    if (P[I].X = $ffff) and (P[I].Y = $ffff) then
    begin
      SetLength(Result, L + 1);
      Result[L] := Copy(P, J, K - 1);
      K := 0;
      Inc(L);
      J := I + 1;
    end;
  end;
  if J = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := Copy(P);
  end;
end;

function RectToPolygon(const R: TRectF): TPolygon;
begin
  SetLength(Result, 4);
  Result[0] := PointF(R.Left, R.Top);
  Result[1] := PointF(R.Right, R.Top);
  Result[2] := PointF(R.Right, R.Bottom);
  Result[3] := PointF(R.Left, R.Bottom);
end;

function EllipseToPolygon(const R: TRectF): TPolygon;
begin
  SetLength(Result, 4);
  Result[0] := PointF(R.Left, R.Top);
  Result[1] := PointF(R.Right, R.Top);
  Result[2] := PointF(R.Right, R.Bottom);
  Result[3] := PointF(R.Left, R.Bottom);
end;


function Interpolate(C1, C2, C3, C4: TAlphaColor; wX, wY: Integer): TAlphaColor; inline;
begin
  Result := CombineReg(CombineReg(C1, C2, wX), CombineReg(C3, C4, wX), wY);
end;

{ TBitmapHelper }

function TBitmapHelper.GetPixelLinear(X, Y, wX, wY: Integer): TAlphaColor;
var
  C1, C2, C3, C4: TAlphaColor;
begin
  Inc(X, Y * Width);
  C1 := StartLine[X];
  C2 := StartLine[X + 1];
  Inc(X, Width);
  C3 := StartLine[X];
  C4 := StartLine[X + 1];
  Result := Interpolate(C1, C2, C3, C4, wX, wY);
end;

function TBitmapHelper.GetPixelLinear(const X, Y: Single): TAlphaColor;
var
  iX, iY: Integer;
  wX, wY: Integer;
begin
  iX := Floor(X);
  iY := Floor(Y);
  wX := Round((X - iX) * 255) xor 255;
  wY := Round((Y - iY) * 255) xor 255;
  Result := GetPixelLinear(iX, iY, wX, wY);
end;

function TBitmapHelper.GetPixelLinearWrap(const X, Y: Single): TAlphaColor;
var
  iX, iY: Integer;
  wX, wY: Integer;
  X1, X2, Y1, Y2: Integer;
  X_safe, Y_safe: Boolean;
  C1, C2, C3, C4: TAlphaColor;
begin
  X1 := Floor(X);
  Y1 := Floor(Y);
  wX := Round((X - X1) * 255) xor 255;
  wY := Round((Y - Y1) * 255) xor 255;
  X_safe := X1 < Width - 1;
  Y_safe := Y1 < Height - 1;
  if X_safe and Y_safe then
  begin
    Result := GetPixelLinear(X1, Y1, wX, wY);
  end
  else
  begin
    X2 := IfThen(X_safe, X1 + 1);
    Y2 := IfThen(Y_safe, Y1 + 1);
    C1 := GetPixelNearest(X1, Y1);
    C2 := GetPixelNearest(X2, Y1);
    C3 := GetPixelNearest(X1, Y2);
    C4 := GetPixelNearest(X2, Y2);
    Result := Interpolate(C1, C2, C3, C4, wX, wY);
  end;
end;

function TBitmapHelper.GetPixelLinearMirror(const X, Y: Single): TAlphaColor;
var
  wX, wY: Integer;
  X1, X2, Y1, Y2: Integer;
  X_safe, Y_safe: Boolean;
  C1, C2, C3, C4: TAlphaColor;
begin
  X1 := Floor(X);
  Y1 := Floor(Y);
  wX := Round((X - X1) * 255) xor 255;
  wY := Round((Y - Y1) * 255) xor 255;
  X_safe := X1 < Width - 1;
  Y_safe := Y1 < Height - 1;
  if X_safe and Y_safe then
  begin
    Result := GetPixelLinear(X1, Y1, wX, wY);
  end
  else
  begin
    X2 := IfThen(X_safe, X1 + 1, X1 - 1);
    Y2 := IfThen(Y_safe, Y1 + 1, Y1 - 1);
    C1 := GetPixelNearest(X1, Y1);
    C2 := GetPixelNearest(X2, Y1);
    C3 := GetPixelNearest(X1, Y2);
    C4 := GetPixelNearest(X2, Y2);
    Result := Interpolate(C1, C2, C3, C4, wX, wY);
  end;
end;

function TBitmapHelper.GetPixelNearest(const X, Y: Single): TAlphaColor;
begin
  Result := StartLine[Round(X) + Round(Y) * Width];
end;

function TransformClipRect(const R: TRectF; const Matrix: TMatrix): TRectF;
var
  P1, P2, P3, P4: TPointF;
begin
  with R do
  begin
    P1 := TransformPoint(PointF(Left, Top), Matrix);
    P2 := TransformPoint(PointF(Right, Top), Matrix);
    P3 := TransformPoint(PointF(Right, Bottom), Matrix);
    P4 := TransformPoint(PointF(Left, Bottom), Matrix);
  end;
  Result.Left := Min(Min(P1.X, P2.X), Min(P3.X, P4.X));
  Result.Top := Min(Min(P1.Y, P2.Y), Min(P3.Y, P4.Y));
  Result.Right := Max(Max(P1.X, P2.X), Max(P3.X, P4.X));
  Result.Bottom := Max(Max(P1.Y, P2.Y), Max(P3.Y, P4.Y));
end;

procedure AffineTransform(
  const Dst: TCanvas; DstRect: TRectF; //DstClip: TRectF;
  const Src: TBitmap; SrcRect: TRectF; const Matrix: TMatrix;
  const Opacity: Single = 1; HighSpeed: Boolean = False);
var
  P: TPointF;
  DstClip: TRectF;
  X, Y: Integer;
  M: TMatrix;
  PDst: PAlphaColorArray;
  C: TAlphaColor;
  SrcX, SrcY, Sx, Sy: Single;
  W, flX, flY: Integer;
  GetPixel: TGetPixel;
begin
  if HighSpeed then
    GetPixel := Src.GetPixelNearest
  else
    GetPixel := Src.GetPixelLinear;

  DstClip := TransformClipRect(DstRect, Matrix);
  DstClip.Left := Max(DstClip.Left, 0);
  DstClip.Top := Max(DstClip.Top, 0);
  DstClip.Right := Min(DstClip.Right, Dst.Width - 1);
  DstClip.Bottom := Min(DstClip.Bottom, Dst.Height - 1);

  M := Matrix;
  InvertMatrix(M);
  W := Round(Opacity * 255);

  Sx := (SrcRect.Right - SrcRect.Left) / (DstRect.Right - DstRect.Left);
  Sy := (SrcRect.Bottom - SrcRect.Top) / (DstRect.Bottom - DstRect.Top);
  for Y := Floor(DstClip.Top) to Ceil(DstClip.Bottom) do
  begin
    PDst := @PAlphaColorArray(Dst.BufferBits)[Y * Dst.Width];
    for X := Round(DstClip.Left) to Round(DstClip.Right) do
    begin
      P := TransformPoint(PointF(X, Y), M);
      SrcX := (P.X - DstRect.Left) * Sx;
      SrcY := (P.Y - DstRect.Top) * Sy;
      flX := Floor(SrcX);
      flY := Floor(SrcY);
      if (flX > SrcRect.Left) and (flX < SrcRect.Right - 1)
        and (flY > SrcRect.Top) and (flY < SrcRect.Bottom - 1) then
        begin
          C := GetPixel(SrcX, SrcY);
          BlendMemEx(C, PDst[X], W);
        end;
    end;
  end;
end;

{ TCanvasVPR }

procedure TCanvasVPR.Clear(const Color: TAlphaColor);
var
  P: TPolygon;
  R: TRectF;
begin
  FFillColor := Color;
  R := RectF(0, 0, FWidth, FHeight);
  P := RectToPolygon(R);
  TransformPolygon(P, Matrix);
  RenderPolygon(P, FClipRect, ClearOpaque);
end;

procedure TCanvasVPR.ClearOpaque(const Span: TValueSpan; DstY: Integer);
var
  Scanline: PAlphaColorArray;
begin
  Scanline := PAlphaColorArray(Integer(FBufferBits) + DstY * Width * SizeOf(TAlphaColor));
  FillLongWord(@ScanLine[Span.X1], Span.X2 - Span.X1 + 1, FFillColor);
end;

procedure TCanvasVPR.ClearRect(const ARect: TRectF; const AColor: TAlphaColor);
var
  P: TPolygon;
  R: TRectF;
begin
  FFillColor := AColor;
  IntersectRect(R, ARect, FClipRect);
  P := RectToPolygon(R);
  P := TransformPolygon(P, Matrix);
  R := RectF(0, 0, FWidth, FHeight);
  RenderPolygon(P, R, ClearOpaque);
//  RenderPolygon(P, R, RenderOpaque);
end;

constructor TCanvasVPR.CreateFromBitmap(const ABitmap: TBitmap);
begin
  inherited;
  FBitmap := ABitmap;
  UpdateBitmapHandle(FBitmap);
  FBufferBits := ABitmap.StartLine;
  ResetClipRect;
end;

constructor TCanvasVPR.CreateFromPrinter(const APrinter: TAbstractPrinter);
begin
  // unsupported
  inherited;
end;

constructor TCanvasVPR.CreateFromWindow(const AParent: THandle; const AWidth,
  AHeight: integer);
begin
  FBuffered := true;
  inherited;
  ResetClipRect;
end;

function TCanvasVPR.CreateSaveState: TCanvasSaveState;
begin
  Result := TCanvasVPRSaveState.Create;
end;

destructor TCanvasVPR.Destroy;
begin
  DeleteObject(FFontHandle);
  inherited;
end;

procedure TCanvasVPR.DestroyBitmapHandle(ABitmap: TBitmap);
begin
  if (ABitmap.HandleExists(Self)) then
  begin
    FBitmaps.Remove(ABitmap);
    ABitmap.RemoveFreeNotify(Self);
    ABitmap.HandleRemove(Self);
  end;
end;

procedure TCanvasVPR.DrawBitmap(const ABitmap: TBitmap; const SrcRect,
  DstRect: TRectF; const AOpacity: Single; const HighSpeed: boolean);
begin
  UpdateBitmapHandle(ABitmap);
  if not ABitmap.HandleExists(Self) then
    Exit;
  AffineTransform(Self, DstRect, ABitmap, SrcRect,Matrix, AOpacity, HighSpeed);
end;

procedure TCanvasVPR.DrawEllipse(const ARect: TRectF; const AOpacity: Single);
var
  Path: TPathData;
begin
  Path := TPathData.Create;
  try
    Path.AddEllipse(ARect);
    DrawPath(Path, AOpacity);
  finally
    Path.Free;
  end;
end;

procedure TCanvasVPR.DrawLine(const APt1, APt2: TPointF;
  const AOpacity: Single);
var
  P: TPolygon;
begin
  SetLength(P, 2);
  P[0] := APt1;
  P[1] := APt2;
  DrawPolygon(P, AOpacity);
end;

procedure TCanvasVPR.DrawPath(const APath: TPathData; const AOpacity: Single);
var
  PP: TPolyPolygon;
begin
  APath.FlattenToPolyPolygon(PP);
  DrawPolyPolygon(PP, AOpacity);
end;

procedure TCanvasVPR.DrawPolygon(const Polygon: TPolygon; const AOpacity: Single);
var
  PP: TPolyPolygon;
begin
  if (FStrokeThickness = 0) or (AOpacity = 0) or (Stroke.Kind = TBrushKind.bkNone) then Exit;

  PrepareFill(PolygonBounds(Polygon), Stroke, AOpacity);
  if StrokeDash = TStrokeDash.sdSolid then
    PP := PolyLineFS(Polygon, False, FStrokeThickness, GetJoinStyle, GetEndStyle)
  else
    PP := DashedPolyLineFS(Polygon, FDash, FDashOffset, False, FStrokeThickness, GetJoinStyle, GetEndStyle);
  TransformPolyPolygon(PP, Matrix);
  RenderPolyPolygon(PP, FClipRect, GetRenderCallback());
end;

procedure TCanvasVPR.DrawPolyPolygon(const PP: TPolyPolygon;
  const AOpacity: Single);
var
  I: Integer;
begin
  for I := 0 to High(PP) do
    DrawPolygon(PP[I], AOpacity);
end;

procedure TCanvasVPR.DrawRect(const ARect: TRectF; const XRadius,
  YRadius: Single; const ACorners: TCorners; const AOpacity: Single;
  const ACornerType: TCornerType);
var
  Path: TPathData;
begin
  Path := TPathData.Create;
  try
    Path.AddRectangle(ARect, XRadius, YRadius, ACorners, ACornerType);
    DrawPath(Path, AOPacity);
  finally
    Path.Free;
  end;
end;

procedure TCanvasVPR.DrawThumbnail(const ABitmap: TBitmap; const Width,
  Height: Single);
var
  R: TRectF;
begin
  R := RectF(0, 0, Width, Height);
  AffineTransform(Self, R, ABitmap, R, Matrix);
end;

procedure TCanvasVPR.ExcludeClipRect(const ARect: TRectF);
begin
  // unsupported
end;

procedure TCanvasVPR.FillEllipse(const ARect: TRectF; const AOpacity: Single);
var
  Path: TPathData;
begin
  Path := TPathData.Create;
  try
    Path.AddEllipse(ARect);
    FillPath(Path, AOpacity);
  finally
    Path.Free;
  end;
end;

procedure TCanvasVPR.FillPath(const APath: TPathData; const AOpacity: Single);
var
  PP: TPolyPolygon;
begin
  APath.FlattenToPolyPolygon(PP);
  if Length(PP) > 0 then
  begin
    FillPolyPolygon(PP, AOpacity);
  end;
end;

procedure TCanvasVPR.FillPathLCD(const APath: TPathData;
  const AOpacity: Single);
var
  PP: TPolyPolygon;
begin
  APath.FlattenToPolyPolygon(PP);
  if Length(PP) > 0 then
  begin
{$IFDEF CLEARTYPE}
    FillPolyPolygonLCD(PP, AOpacity);
{$ELSE}
    FillPolyPolygon(PP, AOpacity);
{$ENDIF}
  end;
end;

procedure TCanvasVPR.FillPolygon(const Polygon: TPolygon; const AOpacity: Single);
var
  P: TPolygon;
begin
  if (AOpacity = 0) or (Fill.Kind = TBrushKind.bkNone) then Exit;

  PrepareFill(PolygonBounds(Polygon), Fill, AOpacity);
  P := TransformPolygon(Polygon, Matrix);
  RenderPolygon(P, FClipRect, GetRenderCallback());
end;

procedure TCanvasVPR.FillPolyPolygonLCD(var PP: TPolyPolygon;
  const AOpacity: Single);
var
  Clip: TRectF;
  M: TMatrix;
begin
  if (AOpacity = 0) or (Fill.Kind = TBrushKind.bkNone) then Exit;

  PrepareFill(PolygonBounds(PP), Fill, AOpacity);
  M := Matrix;
  M.M[0].V[0] := M.M[0].V[0] * 3;
  M.M[1].V[0] := M.M[1].V[0] * 3;
  M.M[2].V[0] := M.M[2].V[0] * 3;
  TransformPolyPolygon(PP, M);
  Clip := RectF(FClipRect.Left * 3, FClipRect.Top, FClipRect.Right * 3, FClipRect.Bottom);
  RenderPolyPolygon(PP, Clip, RenderOpaqueLCD);
end;

procedure TCanvasVPR.FillPolyPolygon(var PP: TPolyPolygon;
  const AOpacity: Single);
begin
  if (AOpacity = 0) or (Fill.Kind = TBrushKind.bkNone) then Exit;

  PrepareFill(PolygonBounds(PP), Fill, AOpacity);
  TransformPolyPolygon(PP, Matrix);
  RenderPolyPolygon(PP, FClipRect, GetRenderCallback());
end;

procedure TCanvasVPR.FillRect(const ARect: TRectF; const XRadius,
  YRadius: Single; const ACorners: TCorners; const AOpacity: Single;
  const ACornerType: TCornerType);
var
  Path: TPathData;
begin
  Path := TPathData.Create;
  try
    Path.AddRectangle(ARect, XRadius, YRadius, ACorners, ACornerType);
    FillPath(Path, AOPacity);
  finally
    Path.Free;
  end;
end;

procedure TCanvasVPR.FillText(const ARect: TRectF; const AText: String;
  const WordWrap: boolean; const AOpacity: Single; const Flags: TFillTextFlags;
  const ATextAlign, AVTextAlign: TTextAlign);
var
  Path: TPathData;
begin
  Path := TPathData.Create;
  try
    TextToPath(Path, ARect, AText, WordWrap, ATextAlign, AVTextAlign);
{$IFDEF CLEARTYPE}
    FillPathLCD(Path, AOpacity)
{$ELSE}
    FillPath(Path, AOpacity);
{$ENDIF}
  finally
    Path.Free;
  end;
end;

procedure TCanvasVPR.FlushBufferRect(const X, Y: integer; const Context;
  const ARect: TRectF);
var
  R: Winapi.Windows.TRect;
  DstDC: THandle;
begin
  DstDC := THandle(Context);
  if DstDC = 0 then
    Exit;

  R := System.Classes.Rect(Trunc(ARect.Left), Trunc(ARect.Top), Trunc(ARect.Right) + 1,
    Trunc(ARect.Bottom) + 1);

  Winapi.Windows.BitBlt(DstDC, X + R.Left, y + R.Top, R.Right - R.Left,
    R.Bottom - R.Top, FBufferHandle, R.Left, R.Top, SRCCOPY);
end;

procedure TCanvasVPR.FontChanged(Sender: TObject);
var
  LF: TLogFont;
begin
  LF := Default(TLogFont);
  DeleteObject(FFontHandle);
  with LF do
  begin
    lfHeight := -Round(Font.Size);
    lfWidth := 0;
    lfEscapement := 0;
    lfOrientation := 0;
    if TFontStyle.fsBold in Font.Style then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;
    lfItalic := Byte(TFontStyle.fsItalic in Font.Style);
    lfUnderline := Byte(TFontStyle.fsUnderline in Font.Style);
    lfStrikeOut := Byte(TFontStyle.fsStrikeOut in Font.Style);
{$WARNINGS OFF}
    StrPLCopy(lfFaceName, UTF8ToString(Font.Family), Length(lfFaceName) - 1);
{$WARNINGS ON}
    lfCharSet := DEFAULT_CHARSET;
    lfQuality := DEFAULT_QUALITY;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfPitchAndFamily := DEFAULT_PITCH;
  end;
  FFontHandle := CreateFontIndirect(LF);
end;

procedure TCanvasVPR.FreeBuffer;
begin
  if FBuffered then
  begin
    if FBufferHandle = 0 then
      Exit;
    if FBufferHandle <> 0 then
      DeleteDC(FBufferHandle);
    FBufferHandle := 0;
    if FBufferBitmap <> 0 then
      DeleteObject(FBufferBitmap);
    FBufferBitmap := 0;
  end;
end;

class function TCanvasVPR.GetBitmapScanline(Bitmap: TBitmap;
  y: integer): PAlphaColorArray;
begin
  if (y >= 0) and (y < Bitmap.Height) and (Bitmap.StartLine <> nil) then
    Result := @PAlphaColorArray(Bitmap.StartLine)[(y) * Bitmap.Width]
  else
    Result := nil;
end;

function TCanvasVPR.GetEndStyle: TEndStyle;
begin
  case StrokeCap of
    TStrokeCap.scFlat: Result := esButt;
    TStrokeCap.scRound: Result := esRound;
  end;
end;

function TCanvasVPR.GetJoinStyle: TJoinStyle;
begin
  case StrokeJoin of
    TStrokeJoin.sjMiter: Result := jsMiter;
    TStrokeJoin.sjBevel: Result := jsBevel;
    TStrokeJoin.sjRound: Result := jsRound;
  end;
end;

function TCanvasVPR.GetRenderCallback: TRenderSpanEvent;
begin
  Result := nil;
  case FBrush.Kind of
    TBrushKind.bkSolid:
      Result := RenderOpaque;
    TBrushKind.bkGradient:
      if FBrush.Gradient.Style = TGradientStyle.gsLinear then
        Result := RenderLinearGradient
      else
        Result := RenderRadialGradient;
    TBrushKind.bkBitmap:
      Result := RenderBitmap;
    TBrushKind.bkGrab:
      {Result := RenderGrab};
    TBrushKind.bkResource:
      {Result := RenderResource};
  end;
end;

procedure TCanvasVPR.IntersectClipRect(const ARect: TRectF);
begin
  IntersectRect(FClipRect, FClipRect, TransformClipRect(ARect, Matrix));
end;

function TCanvasVPR.LoadFontFromStream(AStream: TStream): boolean;
begin
  Result := False;
end;

procedure TCanvasVPR.MeasureText(var ARect: TRectF; const AText: String;
  const WordWrap: boolean; const Flags: TFillTextFlags; const ATextAlign,
  AVTextAlign: TTextAlign);
begin
  ARect := FMX.VPR.Polygons.MeasureText(FFontHandle, ARect, AText, WordWrap, ATextAlign, AVTextAlign);
end;

procedure TCanvasVPR.MultyMatrix(const M: TMatrix);
begin
  FMatrix := MatrixMultiply(M, FMatrix);
end;

procedure TCanvasVPR.PrepareFill(const ARect: TRectF; ABrush: TBrush; AOpacity: Single);
var
  P1, P2: TPointF;
begin
  FBrush := ABrush;
  FFillColor := 0;
  FOpacity := AOpacity;
  with ABrush do
  begin
    case Kind of
      TBrushKind.bkNone: FFillColor := 0;
      TBrushKind.bkSolid: FFillColor := ABrush.Color;
      TBrushKind.bkGrab:
        begin
        end;
      TBrushKind.bkBitmap:
        begin
          FInverse := Matrix;
          InvertMatrix(FInverse);
          FFillBitmap := Fill.Bitmap.Bitmap;
          FFillScaleX := 1/FFillBitmap.Width;
          FFillScaleY := 1/FFillBitmap.Height;
          case Fill.Bitmap.WrapMode of
            TWrapMode.wmTile: FGetPixel := GetPixelWrap;
            TWrapMode.wmTileOriginal: FGetPixel := GetPixelMirror;
            TWrapMode.wmTileStretch:
              begin
                FGetPixel := GetPixelStretch;
                FFillScaleX := FFillBitmap.Width/ARect.Width;
                FFillScaleY := FFillBitmap.Height/ARect.Height;
              end;
          end;
        end;
      TBrushKind.bkGradient:
        begin
          case Gradient.Style of
            TGradientStyle.gsLinear:
              begin
                P1 := Gradient.StartPosition.Point;
                P2 := Gradient.StopPosition.Point;
                P1.X := ARect.Left + P1.X * ARect.Width;
                P1.Y := ARect.Top + P1.Y * ARect.Height;
                P2.X := ARect.Left + P2.X * ARect.Width;
                P2.Y := ARect.Top + P2.Y * ARect.Height;
                P1 := TransformPoint(P1, Matrix);
                P2 := TransformPoint(P2, Matrix);
                FGradientStart := P1;
                FGradientStop := P2;

                FLinearGradientSlope := IfThen(P1.X <> P2.X, (P1.Y - P2.Y) / (P1.X - P2.X), Infinity);
              end;
            TGradientStyle.gsRadial:
              begin
                FGradientStart := ARect.CenterPoint;
                FFillScaleX := 2/ARect.Width;
                FFillScaleY := 2/ARect.Height;
                FInverse := Matrix;
                InvertMatrix(FInverse);
              end;
          end;
{$IFDEF GRADIENTLUT}
          PrepareGradientLUT;
{$ENDIF}
        end;
    end;
  end;
  FFillColor := (FFillColor and $ffffff) or (Ceil(AOpacity * (FFillColor shr 24)) shl 24);
end;

procedure TCanvasVPR.PrepareGradientLUT;
const
  DIV255 = 1/255;
var
  I: Integer;
begin
  for I := 0 to 255 do
  begin
    FGradientLUT[I] := FBrush.Gradient.InterpolateColor(I * DIV255);
  end;
end;

function TCanvasVPR.PtInPath(const APoint: TPointF;
  const APath: TPathData): boolean;
begin
  Result := False;
end;

function TCanvasVPR.GetPixelWrap(const X, Y: Single): TAlphaColor;
var
  wX, wY: Single;
begin
  wX := X - Floor(X);
  wY := Y - Floor(Y);
  wX := wX * FFillBitmap.Width;
  wY := wY * FFillBitmap.Height;
  Result := FFillBitmap.GetPixelLinearWrap(wX, wY);
end;

function TCanvasVPR.GetPixelMirror(const X, Y: Single): TAlphaColor;
var
  wX, wY: Single;
  flX, flY: NativeInt;
begin
  flX := Floor(X);
  flY := Floor(Y);
  wX := X - flX;
  wY := Y - flY;
  if Odd(flX) then wX := 1 - wX;
  // N: apparently wmTileOriginal means "flip X"
  // if Odd(flY) then wY := 1 - wY;
  wX := wX * FFillBitmap.Width;
  wY := wY * FFillBitmap.Height;
  Result := FFillBitmap.GetPixelLinearMirror(wX, wY);
end;

function TCanvasVPR.GetPixelStretch(const X, Y: Single): TAlphaColor;
begin
  Result := FFillBitmap.GetPixelLinearWrap(X, Y);
end;

procedure TCanvasVPR.RenderBitmap(const Span: TValueSpan; DstY: Integer);
var
  X: Integer;
  Scanline: PAlphaColorArray;
  R, V: Single;
  C: TAlphaColor;
  W: Integer;
  ValPtr: PSingle;
  P: TPointF;
begin
  Scanline := PAlphaColorArray(Integer(FBufferBits) + DstY * Width * SizeOf(TAlphaColor));
  ValPtr := @Span.Values[0];
  for X := Span.X1 to Span.X2 do
  begin
    P := TransformPoint(PointF(X, DstY), FInverse);
    P.X := P.X * FFillScaleX;
    P.Y := P.Y * FFillScaleY;
    C := FGetPixel(P.X, P.Y);

    V := Abs(ValPtr^);
    if V > 1 then V := 1;
    W := Round(V * $1000);
{$IFDEF GAMMACORRECTION}
    W := GAMMA_TABLE[W];
{$ENDIF}
    W := Round(W * FOpacity) shr 4;

    BlendMemEx(C, Scanline[X], W);
    // N: make opaque to avoid blending problem with UpdateLayeredWindow in FMX
    Scanline[X] := Scanline[X] or $ff000000;
    Inc(ValPtr);
  end;
end;

procedure TCanvasVPR.RenderGrab(const Span: TValueSpan; DstY: Integer);
begin
  // TBA
end;

procedure TCanvasVPR.RenderLinearGradient(const Span: TValueSpan;
  DstY: Integer);
var
  P1, P2: TPointF;
  Offset, X1, X2: Single;
  X, W, M: Integer;
  ValPtr: PSingle;
  V, RX: Single;
  C: TAlphaColor;
  Scanline: PAlphaColorArray;
begin
  P1 := FGradientStart;
  P2 := FGradientStop;
  { Horizontal gradient }
  if FLinearGradientSlope = Infinity then
  begin
    Offset := (DstY - P1.Y) / (P2.Y - P1.Y);
    FFillColor := FBrush.Gradient.InterpolateColor(Offset);
    RenderOpaque(Span, DstY);
  end
  else
  begin
    X1 := P1.X - (DstY - P1.Y) * FLinearGradientSlope;
    X2 := P2.X - (DstY - P2.Y) * FLinearGradientSlope;

    ValPtr := @Span.Values[0];
    Scanline := PAlphaColorArray(Integer(FBufferBits) + DstY * Width * SizeOf(TAlphaColor));

    RX := 1/(X2 - X1);
    Offset := (Span.X1 - X1) * RX;
    for X := Span.X1 to Span.X2 do
    begin
{$IFDEF GRADIENTLUT}
      M := Round(Offset * 255);
      if M < 0 then M := 0;
      if M > 255 then M := 255;
      C := FGradientLUT[M];
{$ELSE}
      C := FBrush.Gradient.InterpolateColor(Offset);
{$ENDIF}
      V := Abs(ValPtr^);
      if V > 1 then V := 1;
      W := Round(V * $1000);
{$IFDEF GAMMACORRECTION}
      W := GAMMA_TABLE[W];
{$ENDIF}
      W := Round(W * FOpacity) shr 4;
      BlendMemEx(C, Scanline[X], W);
      // N: make opaque to avoid blending problem with UpdateLayeredWindow in FMX
      Scanline[X] := Scanline[X] or $ff000000;
      Inc(ValPtr);
      Offset := Offset + RX;
    end;
  end;
end;

procedure TCanvasVPR.RenderRadialGradient(const Span: TValueSpan; DstY: Integer);
var
  X: Integer;
  Scanline: PAlphaColorArray;
  R, V: Single;
  C: TAlphaColor;
  W: Integer;
  ValPtr: PSingle;

  function GetRadius(X, Y: Integer): Single;
  var
    P: TPointF;
    Dx, Dy: Single;
  begin
    P := TransformPoint(PointF(X, Y), FInverse);
    Dx := (FGradientStart.X - P.X) * FFillScaleX;
    Dy := (FGradientStart.Y - P.Y) * FFillScaleY;
    Result := Hypot(Dx, Dy);
  end;

begin
  Scanline := PAlphaColorArray(Integer(FBufferBits) + DstY * Width * SizeOf(TAlphaColor));
  ValPtr := @Span.Values[0];
  for X := Span.X1 to Span.X2 do
  begin
    R := GetRadius(X, DstY);
    if R > 1 then R := 1;
    R := 1 - R;
{$IFDEF GRADIENTLUT}
    W := Round(R * 255);
    C := FGradientLUT[W];
{$ELSE}
    C := FBrush.Gradient.InterpolateColor(R);
{$ENDIF}
    V := Abs(ValPtr^);
    if V > 1 then V := 1;
    W := Round(V * $1000);
{$IFDEF GAMMACORRECTION}
    W := GAMMA_TABLE[W];
{$ENDIF}
    W := Round(W * FOpacity) shr 4;
    BlendMemEx(C, Scanline[X], W);
    // N: make opaque to avoid blending problem with UpdateLayeredWindow in FMX
    Scanline[X] := Scanline[X] or $ff000000;
    Inc(ValPtr);
  end;
end;



procedure TCanvasVPR.RenderResource(const Span: TValueSpan; DstY: Integer);
begin
  // TBA
end;

procedure MakeAlphaNonZeroUP(Coverage: PSingleArray; AlphaValues: PAlphaColorArray;
  Count: Integer; Color: TAlphaColor);
var
  I: Integer;
  M, V, C: Cardinal;
  Last: Single;
begin
  M := Color shr 24 * $1010;
  C := Color and $00ffffff;

  Last := Infinity;
  V := 0;
  for I := 0 to Count - 1 do
  begin
    if PInteger(@Last)^ <> PInteger(@Coverage[I])^ then
    begin
      Last := Coverage[I];
      V := Abs(Round(Last * $1000));
      if V >= $1000 then V := $1000;
{$IFDEF GAMMACORRECTION}
      V := GAMMA_TABLE[V];
{$ENDIF}
      V := (V * M and $ff000000) or C;
    end;
    AlphaValues[I] := V;
  end;
end;

{$IFDEF USESTACKALLOC}
{$W+}
{$ENDIF}
procedure TCanvasVPR.RenderOpaque(const Span: TValueSpan; DstY: Integer);
var
  AlphaValues: PAlphaColorArray;
  Count: Integer;
  Scanline: PAlphaColorArray;
begin
  Count := Span.X2 - Span.X1 + 1;
{$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc(Count * SizeOf(TAlphaColor));
{$ELSE}
  GetMem(AlphaValues, Count * SizeOf(TAlphaColor));
{$ENDIF}
  //FFillProcUnpacked(Span.Values, AlphaValues, Count, FColor);
  MakeAlphaNonZeroUP(Span.Values, AlphaValues, Count, FFillColor);

  Scanline := PAlphaColorArray(Integer(FBufferBits) + DstY * Width * SizeOf(TAlphaColor));

  BlendLine(@AlphaValues[0], @ScanLine[Span.X1], Count);
{$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
{$ELSE}
  FreeMem(AlphaValues);
{$ENDIF}
end;
{$IFDEF USESTACKALLOC}
{$W-}
{$ENDIF}

procedure TCanvasVPR.RenderOpaqueLCD(const Span: TValueSpan; DstY: Integer);
const
  PADDING = 5;
var
  AlphaValues: PByteArray;
  Count: Integer;
  X1, Offset: Integer;
  Scanline: PAlphaColorArray;
//const
//  MakeAlpha: array [TPolyFillMode] of TMakeAlphaProcLCD = (MakeAlphaEvenOddLCD2, MakeAlphaNonZeroLCD2);
begin
  Scanline := PAlphaColorArray(Integer(FBufferBits) + DstY * Width * SizeOf(TAlphaColor));

  Count := Span.X2 - Span.X1 + 1;

  X1 := DivMod(Span.X1, 3, Offset);

  // Left Padding + Right Padding + Filter Width = 2 + 2 + 2 = 6
  GetMem(AlphaValues, Count + 6 + PADDING);
  AlphaValues[0] := 0;
  AlphaValues[1] := 0;
  if (X1 > 0) then
  begin
    Dec(X1);
    Inc(Offset, 3);
    AlphaValues[2] := 0;
    AlphaValues[3] := 0;
    AlphaValues[4] := 0;
  end;

  Dec(Offset, 1);
  //MakeAlpha[FFillMode](Span.Values, PByteArray(@AlphaValues[PADDING]), Count, FColor);
  {$IFDEF CLEARTYPEHQ}
  MakeAlphaNonZeroLCD2
  {$ELSE}
  MakeAlphaNonZeroLCD
  {$ENDIF}(Span.Values, PByteArray(@AlphaValues[PADDING]), Count, FFillColor);
  Inc(Count);
  CombineLineLCD(@AlphaValues[PADDING - Offset], PAlphaColorArray(@Scanline[X1]), FFillColor, (Count + Offset + 2) div 3);
  FreeMem(AlphaValues);
end;

procedure TCanvasVPR.ResetClipRect;
begin
  FClipRect := RectF(0, 0, Width, Height);
end;

procedure TCanvasVPR.ResizeBuffer(const AWidth, AHeight: integer);
begin
  if (AWidth = FWidth) and (AHeight = FHeight) then
    Exit;
  FreeBuffer;
  FWidth := AWidth;
  FHeight := AHeight;
  if FWidth <= 0 then
    FWidth := 1;
  if FHeight <= 0 then
    FHeight := 1;
  FResized := true;
  if FBuffered then
  begin
    { Initialization }
    with FBitmapInfo.bmiHeader do
    begin
      biSize := SizeOf(TBitmapInfoHeader);
      biPlanes := 1;
      biBitCount := 32;
      biCompression := BI_RGB;
      biWidth := AWidth;
      if biWidth <= 0 then
        biWidth := 1;
      biHeight := -AHeight;
      if biHeight >= 0 then
        biHeight := -1;
    end;
    { Create new DIB }
    FBufferBitmap := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS,
      Pointer(FBufferBits), 0, 0);
    if FBufferBits = nil then
      raise Exception.Create('Can''t allocate the DIB handle ' + IntToStr(AWidth) + 'x' + IntToStr(AHeight));

    FBufferHandle := CreateCompatibleDC(0);
    if FBufferHandle = 0 then
    begin
      DeleteObject(FBufferBitmap);
      FBufferHandle := 0;
      FBufferBits := nil;
      raise Exception.Create('Can''t create compatible DC');
    end;

    if SelectObject(FBufferHandle, FBufferBitmap) = 0 then
    begin
      DeleteDC(FBufferHandle);
      DeleteObject(FBufferBitmap);
      FBufferBitmap := 0;
      FBufferHandle := 0;
      FBufferBits := nil;
      raise Exception.Create('Can''t select an object into DC');
    end;

  end;
end;

procedure TCanvasVPR.SetClipRects(const ARects: array of TRectF);
var
  I: Integer;
begin
  if Length(ARects) = 0 then Exit;
  FClipRect := TransformClipRect(ARects[0], Matrix);
  for I := 1 to High(ARects) do
    FClipRect := UnionRect(FClipRect, TransformClipRect(ARects[I], Matrix));
  IntersectRect(FClipRect, FClipRect, RectF(0, 0, Width, Height));
end;

procedure TCanvasVPR.SetMatrix(const M: TMatrix);
begin
  FMatrix := M;
end;

function TCanvasVPR.TextToPath(Path: TPathData; const ARect: TRectF;
  const AText: String; const WordWrap: boolean; const ATextAlign,
  AVTextAlign: TTextAlign): boolean;
begin
  FMX.VPR.Polygons.TextToPath(FFontHandle, Path, ARect, AText, WordWrap, ATextAlign, AVTextAlign);
  Result := True;
end;

function TCanvasVPR.TransformedClipRect: TRectF;
begin
  Result := TransformClipRect(FClipRect, Matrix);
end;

procedure TCanvasVPR.UpdateBitmapHandle(ABitmap: TBitmap);
begin
  { update bitmap to GDI+ bitmap }
  if ABitmap = nil then
    Exit;
  if ABitmap.IsEmpty then
    Exit;
  { create - if need }
  if not ABitmap.HandleExists(Self) then
  begin
    ABitmap.HandleAdd(Self);
    ABitmap.HandlesNeedUpdate[Self] := False;
    ABitmap.AddFreeNotify(Self);
    FBitmaps.Add(ABitmap);
  end;
end;


{ TPathDataHelper }

const
  BezierTolerance = 0.25;

function Flatness(P1, P2, P3, P4: TPointF): Single;
begin
  Result :=
    Abs(P1.X + P3.X - 2*P2.X) +
    Abs(P1.Y + P3.Y - 2*P2.Y) +
    Abs(P2.X + P4.X - 2*P3.X) +
    Abs(P2.Y + P4.Y - 2*P3.Y);
end;

procedure BezierCurve(P1, P2, P3, P4: TPointF; var Polygon: TPolygon);

  procedure AddPoint(P: TPointF);
  var
    L: Integer;
  begin
    L := Length(Polygon);
    SetLength(Polygon, L + 1);
    Polygon[L] := P;
  end;

  procedure Recurse(const P1, P2, P3, P4: TPointF);
  var
    P12, P23, P34, P123, P234, P1234: TPointF;
  begin
    if Flatness(P1, P2, P3, P4) < BezierTolerance then
    begin
      AddPoint(P1);
    end
    else
    begin
      P12.X   := (P1.X + P2.X) * 0.5;
      P12.Y   := (P1.Y + P2.Y) * 0.5;
      P23.X   := (P2.X + P3.X) * 0.5;
      P23.Y   := (P2.Y + P3.Y) * 0.5;
      P34.X   := (P3.X + P4.X) * 0.5;
      P34.Y   := (P3.Y + P4.Y) * 0.5;
      P123.X  := (P12.X + P23.X) * 0.5;
      P123.Y  := (P12.Y + P23.Y) * 0.5;
      P234.X  := (P23.X + P34.X) * 0.5;
      P234.Y  := (P23.Y + P34.Y) * 0.5;
      P1234.X := (P123.X + P234.X) * 0.5;
      P1234.Y := (P123.Y + P234.Y) * 0.5;

      Recurse(P1, P12, P123, P1234);
      Recurse(P1234, P234, P34, P4);
    end;
  end;

begin
  Recurse(P1, P2, P3, P4);
  AddPoint(P4);
end;

function TPathDataHelper.FlattenToPolyPolygon(var PP: TPolyPolygon;
  const Flatness: Single): TPointF;
var
  I, J, K: Integer;
  SP, CurPoint: TPointF;
  F, S: Single;
  Bounds, R: TRectF;
  P: TPathPoint;
  Polygon: ^TPolygon;
begin
  Result := PointF(0, 0);
  SetLength(PP, 0);
  if Count > 0 then
  begin
    Bounds := GetBounds;
    R := Bounds;
    FitRect(R, RectF(0, 0, 100, 100));
    S := Min(RectWidth(Bounds) * 0.01, RectHeight(Bounds) * 0.01);

    I := 0;
    Polygon := nil;
    while I < Count do
    begin
      P := Points[I];
      case P.Kind of
        TPathPointKind.ppMoveTo:
          begin
            SetLength(PP, Length(PP) + 1);
            Polygon := @PP[High(PP)];
            SetLength(Polygon^, 1);
            Polygon^[High(Polygon^)] := P.Point;
            CurPoint := P.Point;
            SP := CurPoint;
          end;
        TPathPointKind.ppLineTo:
          begin
            SetLength(Polygon^, Length(Polygon^) + 1);
            Polygon^[High(Polygon^)] := P.Point;
            CurPoint := P.Point;
          end;
        TPathPointKind.ppCurveTo:
          begin
            BezierCurve(CurPoint, Points[I].Point, Points[I + 1].Point, Points[I + 2].Point, Polygon^);
            Inc(I, 2);
            CurPoint := Points[I].Point;
          end;
        TPathPointKind.ppClose:
          begin
            SetLength(Polygon^, Length(Polygon^) + 1);
            Polygon^[High(Polygon^)] := SP;
          end;
      end;
      inc(I);
    end;
    with GetBounds do
      Result := PointF(Abs(Right - Left), Abs(Bottom - Top));
  end;
end;

{ TCanvasVPRSaveState }

procedure TCanvasVPRSaveState.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCanvasVPR then
    FClipRect := TCanvasVPR(Source).FClipRect;
end;

procedure TCanvasVPRSaveState.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TCanvasVPR then
    TCanvasVPR(Dest).FClipRect := FClipRect;
end;

procedure SetVPRDefault;
begin
  GlobalUseDirect2D := False;
  DefaultCanvasClass := TCanvasVPR;
end;

initialization
{$IFNDEF DISABLEAUTOINITIALIZE}
  SetVPRDefault;
{$ENDIF}

end.
