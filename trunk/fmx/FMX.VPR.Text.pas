unit FMX.VPR.Text;

interface

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

{$I VPR.INC}

uses
  Winapi.Windows, System.Types, FMX.VPR.Polygons, FMX.Types;

procedure TextToPath(Font: HFONT; Path: TPathData;
  const ARect: TRectF; const Text: WideString; WordWrap: Boolean;
  HorzAlign, VertAlign: TTextAlign); overload;
function MeasureText(Font: HFONT; const ARect: TRectF; const Text: WideString;
  WordWrap: Boolean; HorzAlign, VertAlign: TTextAlign): TRectF; overload;

var
  UseHinting: Boolean = {$IFDEF NOHINTING}False{$ELSE}True{$ENDIF};

// stretching factor when calling GetGlyphOutline()
const
  HORZSTRETCH = 16;

implementation

type
  PBezierVertex = ^TBezierVertex;
  TBezierVertex = record
    case Integer of
      0: (Point: TPointF; ControlPoints: array [0..1] of TPointF);
      1: (Points: array [0..2] of TPointF);
  end;

const
  GGO_UNHINTED = $0100;
  GGODefaultFlags: array [Boolean] of Integer = (GGO_NATIVE or GGO_UNHINTED, GGO_NATIVE);

const
  FixedOne = $10000;

  VertFlip_mat2: tmat2 = (
    eM11: (fract: 0; Value: {$IFNDEF NOHORIZONTALHINTING}1{$ELSE}HORZSTRETCH{$ENDIF});
    eM12: (fract: 0; Value: 0);
    eM21: (fract: 0; Value: 0);
    eM22: (fract: 0; Value: -1);
  );

function QuadToBezier(Q0, Q1, Q2: TPointF): TBezierVertex;
// Q-spline to Bezier curve:
// B0 = Q0
// B1 = (Q0 + 2*Q1) / 3
// B2 = (Q0 + 2*Q2) / 3
begin
  with Result do
  begin
    Points[0] := Q0;
    Points[1].X := (Q0.X + 2*Q1.X) / 3;
    Points[1].Y := (Q0.Y + 2*Q1.Y) / 3;
    Points[2].X := (Q0.X + 2*Q2.X) / 3;
    Points[2].Y := (Q0.Y + 2*Q2.Y) / 3;
  end;
end;

procedure OffsetVertex(var Vertex: TBezierVertex; const Dx, Dy: Single);
begin
  Vertex.Points[0].X := Vertex.Points[0].X + Dx;
  Vertex.Points[0].Y := Vertex.Points[0].Y + Dy;
  Vertex.Points[1].X := Vertex.Points[1].X + Dx;
  Vertex.Points[1].Y := Vertex.Points[1].Y + Dy;
  Vertex.Points[2].X := Vertex.Points[2].X + Dx;
  Vertex.Points[2].Y := Vertex.Points[2].Y + Dy;
end;

function PointFXtoPointF(const Point: tagPointFX): TPointF;
begin
  Result.X := Point.X.Value + Point.X.Fract / FixedOne;
  Result.Y := Point.Y.Value + Point.Y.Fract / FixedOne;
end;


{$IFDEF USESTACKALLOC}
{$W+}
{$ENDIF}
procedure GlyphOutlineToPath(Handle: HDC; Path: TPathData; DstX, DstY: Single;
  const Glyph: Integer; out Metrics: TGlyphMetrics);
var
  J, K, S: Integer;
  Res: DWORD;
  PGlyphMem, PBuffer: PTTPolygonHeader;
  PPCurve: PTTPolyCurve;

  P: TPointF;
  V: TBezierVertex;
  Q0, Q1, Q2: TPointF;

  procedure AddToBezierCurve;
  begin
    V := QuadToBezier(Q0, Q2, Q1);
    OffsetVertex(V, DstX, DstY);
    Path.CurveTo(V.Points[0], V.Points[1], V.Points[2]);
  end;

begin
  Res := GetGlyphOutlineW(Handle, Glyph, GGODefaultFlags[UseHinting], Metrics, 0, nil, VertFlip_mat2);
  if not Assigned(Path) then Exit;

  PGlyphMem := StackAlloc(Res);
  PBuffer := PGlyphMem;

  Res := GetGlyphOutlineW(Handle, Glyph, GGODefaultFlags[UseHinting], Metrics, Res, PBuffer, VertFlip_mat2);

  if (Res = GDI_ERROR) or (PBuffer^.dwType <> TT_POLYGON_TYPE) then
  begin
    StackFree(PGlyphMem);
    Exit;
  end;

  K := 0;
    while Res > 0 do
    begin
      S := PBuffer.cb - SizeOf(TTTPolygonHeader);
      NativeInt(PPCurve) := NativeInt(PBuffer) + SizeOf(TTTPolygonHeader);
      Q0 := PointFXtoPointF(PBuffer.pfxStart);
      Q2 := Q0;

      Path.MoveTo(PointF(Q0.X + DstX, Q0.Y + DstY));
      while S > 0 do
      begin
        case PPCurve.wType of
          TT_PRIM_LINE:
            begin
              Q1 := Q0;
              AddToBezierCurve;

              P := Q0;
              P.X := P.X + DstX;
              P.Y := P.Y + DstY;
              for J := 0 to PPCurve.cpfx - 1 do
              begin
                P := PointFXtoPointF(PPCurve.apfx[J]);
                P.X := P.X + DstX;
                P.Y := P.Y + DstY;
                Path.LineTo(P);
              end;

              Q0 := PointFXtoPointF(PPCurve.apfx[PPCurve.cpfx - 1]);
              Q2 := Q0;
            end;
          TT_PRIM_QSPLINE:
            begin

              for J := 0 to PPCurve.cpfx - 2 do
              begin
                Q1 := PointFXtoPointF(PPCurve.apfx[J]);
                AddToBezierCurve;

                if J < PPCurve.cpfx - 2 then
                with PointFXtoPointF(PPCurve.apfx[J + 1]) do
                  begin
                    Q0.x := (Q1.x + x) * 0.5;
                    Q0.y := (Q1.y + y) * 0.5;
                  end
                else
                  Q0 := PointFXtoPointF(PPCurve.apfx[J + 1]);

                Q2 := Q1;
              end;
            end;
        end;
        K := (PPCurve.cpfx - 1) * SizeOf(TPointFX) + SizeOf(TTPolyCurve);
        Dec(S, K);
        Inc(NativeInt(PPCurve), K);
      end;

      Dec(NativeInt(PPCurve), K);
      if PPCurve.wType = TT_PRIM_QSPLINE then
      begin
        Q1 := PointFXtoPointF(PPCurve.apfx[PPCurve.cpfx - 1]);
        AddToBezierCurve;
      end;

      Path.ClosePath;

      Dec(Res, PBuffer.cb);
      Inc(NativeInt(PBuffer), PBuffer.cb);
    end;

  StackFree(PGlyphMem);
end;
{$IFDEF USESTACKALLOC}
{$W-}
{$ENDIF}


procedure TextToPath(DC: HDC; Path: TPathData; var ARect: TRectF;
  const Text: WideString; WordWrap: Boolean; HorzAlign, VertAlign: TTextAlign); overload;
const
  CHAR_CR = 10;
  CHAR_NL = 13;
  CHAR_SP = 32;
var
  GlyphMetrics: TGlyphMetrics;
  TextMetric: TTextMetric;
  I, J, TextLen: Integer;
  CharValue: Integer;
  X, Y, XMax: Single;
  S: WideString;

  procedure NewLine;
  begin
    X := ARect.Left{$IFDEF NOHORIZONTALHINTING}*HORZSTRETCH{$ENDIF};
    Y := Y + TextMetric.tmHeight;
  end;

  function MeasureTextX(const S: WideString): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to Length(S) do
    begin
      CharValue := Ord(S[I]);
      GetGlyphOutlineW(DC, CharValue, GGODefaultFlags[UseHinting], GlyphMetrics, 0, nil, VertFlip_mat2);
      Inc(Result, GlyphMetrics.gmCellIncX);
    end;
  end;

  procedure TestNewLine(X: Single);
  begin
    if X > ARect.Right{$IFDEF NOHORIZONTALHINTING}*HORZSTRETCH{$ENDIF} then
      NewLine;
  end;

begin
  GetTextMetrics(DC, TextMetric);

  TextLen := Length(Text);
  X := ARect.Left {$IFDEF NOHORIZONTALHINTING}*HORZSTRETCH{$ENDIF};
  Y := ARect.Top + TextMetric.tmAscent;
  XMax := X;
  for I := 1 to TextLen do
  begin
    CharValue := Ord(Text[I]);
    if CharValue <= 32 then
    begin
      case CharValue of
        CHAR_CR: X := ARect.Left;
        CHAR_NL: Y := Y + TextMetric.tmHeight;
        CHAR_SP:
          begin
            GetGlyphOutlineW(DC, CharValue, GGODefaultFlags[UseHinting], GlyphMetrics, 0, nil, VertFlip_mat2);
            X := X + GlyphMetrics.gmCellIncX;

            if WordWrap then
            begin
              J := I + 1;
              while (J <= TextLen) and ([Ord(Text[J])] * [CHAR_CR, CHAR_NL, CHAR_SP] = []) do
                Inc(J);
              S := Copy(Text, I + 1, J - I - 1);
              TestNewLine(X + MeasureTextX(S));
            end;
          end;
      end;
    end
    else
    begin
      TestNewLine(X);

      GlyphOutlineToPath(DC, Path, X, Y, CharValue, GlyphMetrics);
      X := X + GlyphMetrics.gmCellIncX;
      if X > XMax then XMax := X;
    end;
  end;
  Y := Y + TextMetric.tmHeight - TextMetric.tmAscent;

{$IFNDEF NOHORIZONTALHINTING}
  ARect := RectF(ARect.Left, ARect.Top, XMax, Y);
{$ELSE}
  ARect := RectF(ARect.Left, ARect.Top, XMax{$IFDEF NOHORIZONTALHINTING}/HORZSTRETCH{$ENDIF}, Y);
  if Assigned(Path) then
    Path.Scale(1/HORZSTRETCH, 1);
{$ENDIF}
end;

function MeasureText(DC: HDC;
  const ARect: TRectF; const Text: WideString;
  WordWrap: Boolean; HorzAlign, VertAlign: TTextAlign): TRectF; overload;
begin
  Result := ARect;
  TextToPath(DC, nil, Result, Text, WordWrap, HorzAlign, VertAlign);

  case HorzAlign of
    TTextAlign.taCenter: OffsetRect(Result, (((ARect.Left + ARect.Right) - (Result.Left + Result.Right)) * 0.5), 0);
    TTextAlign.taTrailing: OffsetRect(Result, ARect.Right - Result.Right, 0);
  end;
  case VertAlign of
    TTextAlign.taCenter: OffsetRect(Result, 0, ((ARect.Top + ARect.Bottom) - (Result.Top + Result.Bottom)) * 0.5);
    TTextAlign.taTrailing: OffsetRect(Result, 0, ARect.Bottom - Result.Bottom);
  end;
  Result.Left := Round(Result.Left);
  Result.Top := Round(Result.Top);
  Result.Right := Round(Result.Right);
  Result.Bottom := Round(Result.Bottom);
end;

function MeasureText(Font: HFONT; const ARect: TRectF; const Text: WideString;
  WordWrap: Boolean; HorzAlign, VertAlign: TTextAlign): TRectF; overload;
var
  DC: HDC;
begin
  DC := GetDC(0);
  try
    SelectObject(DC, Font);
    Result := MeasureText(DC, ARect, Text, WordWrap, HorzAlign, VertAlign);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TextToPath(Font: HFONT; Path: TPathData; const ARect: TRectF;
  const Text: WideString; WordWrap: Boolean; HorzAlign, VertAlign: TTextAlign); overload;
var
  DC: HDC;
  R: TRectF;
begin
  DC := GetDC(0);
  try
    SelectObject(DC, Font);
    R := MeasureText(DC, ARect, Text, WordWrap, HorzAlign, VertAlign);
    TextToPath(DC, Path, R, Text, WordWrap, HorzAlign, VertAlign);
  finally
    ReleaseDC(0, DC);
  end;
end;

end.
