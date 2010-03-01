unit GR32_PathsEx;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Windows, Classes, Math, GR32, GR32_Polygons, GR32_Resamplers, GR32_Transforms
  { GR32_PolygonsEx };

const
  GGO_UNHINTED = $0100;
  
type
  { TCustomShape }
  TCustomShape = class(TPersistent)
  private
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    function AsPolygon: TPolygon32;
    procedure AppendToPolygon(Polygon: TPolygon32); virtual; abstract;
  end;


  { TCustomCurve }
  TCustomCurve = class(TCustomShape)
  public
    procedure Delete; virtual;
  end;


  { TBezierCurve }

  PBezierVertex = ^TBezierVertex;
  TBezierVertex = record
    case Integer of
      0: (Point: TFloatPoint; ControlPoints: array [0..1] of TFloatPoint);
      1: (Points: array [0..2] of TFloatPoint);
  end;

  TBezierSegment = record
    P1, P2, P3, P4: TFloatPoint;
  end;

  TArrayOfBezierVertex = array of TBezierVertex;
  TArrayOfArrayOfBezierVertex = array of TArrayOfBezierVertex;

  TBezierCurve = class(TCustomCurve)
  private
    FVertices: TArrayOfArrayOfBezierVertex;
    FClosed: Boolean;
  public
    constructor Create;
    procedure Delete; override;
    procedure NewContour;
    procedure AddVertex(const V: TBezierVertex);
    procedure AppendToPolygon(Polygon: TPolygon32); override;
    property Vertices: TArrayOfArrayOfBezierVertex read FVertices write FVertices;
    property Closed: Boolean read FClosed write FClosed;
  end;

  { TSplineCurve }
  TSplineCurve = class(TCustomShape)
  private
    FPoints: TArrayOfArrayOfFloatPoint;
    FKernel: TCustomKernel;
    procedure SetKernel(const Value: TCustomKernel);
  public
    constructor Create;
    procedure NewContour;
    procedure AddPoint(const Point: TFloatPoint);
    procedure AppendToPolygon(Polygon: TPolygon32); override;
    property Points: TArrayOfArrayOfFloatPoint read FPoints write FPoints;
    property Kernel: TCustomKernel read FKernel write SetKernel;
  end;

  { TEllipse }
  TEllipse = class(TCustomShape)
  private
    FBoundsRect: TFloatRect;
  public
    procedure AppendToPolygon(Polygon: TPolygon32); override;
    property BoundsRect: TFloatRect read FBoundsRect write FBoundsRect;
  end;

const
  Epsilon = 1;

var
  BezierTolerance: Single = 0.25;

function SqrDistance(const A, B: TFloatPoint): TFloat;
function DotProduct(const A, B: TFloatPoint): TFloat;
function AddPoints(const A, B: TFloatPoint): TFloatPoint;
function SubPoints(const A, B: TFloatPoint): TFloatPoint;

{ Bezier curve routines }
function BezierCurveToPolygonX(const Vertices: TArrayOfBezierVertex;
  Closed: Boolean = True): TArrayOfFixedPoint; overload;
function BezierCurveToPolygonF(const Vertices: TArrayOfBezierVertex;
  Closed: Boolean = True): TArrayOfFloatPoint; overload;

procedure OffsetVertex(var Vertex: TBezierVertex; const Dx, Dy: TFloat);
function ZeroVertex(const Point: TFloatPoint): TBezierVertex;
function BezierSegment(const Vertices: TArrayOfBezierVertex; Index: Integer): TBezierSegment;

{ Text routines }
procedure DrawText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  OutlineColor, FillColor: TColor32; Transformation: TTransformation = nil); overload;
procedure DrawText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  OutlineColor: TColor32; Filler: TCustomPolygonFiller;
  Transformation: TTransformation = nil); overload;
procedure DrawTextOutline(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  Color: TColor32; Transformation: TTransformation = nil);
procedure FillText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  Filler: TCustomPolygonFiller; Transformation: TTransformation = nil);
function TextToPolygon(Dst: TBitmap32; DstX, DstY: TFloat; const Text: WideString): TPolygon32;
function TextToPolygonF(Dst: TBitmap32; DstX, DstY: TFloat; const Text: WideString): TArrayOfArrayOfFloatPoint;

// function EllipseToPolygon
function MakeCurve(const Points: TArrayOfFloatPoint; Kernel: TCustomKernel;
  Closed: Boolean; StepSize: Integer): TArrayOfFixedPoint; overload;


var
  GGODefaultFlags: Integer = GGO_NATIVE or GGO_UNHINTED;

implementation

uses
  SysUtils, Types, GR32_LowLevel;

function AddPoints(const A, B: TFloatPoint): TFloatPoint;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

function SubPoints(const A, B: TFloatPoint): TFloatPoint;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

// Returns square of the distance between points A and B.
function SqrDistance(const A, B: TFloatPoint): TFloat;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;

function DotProduct(const A, B: TFloatPoint): TFloat;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;

// Returns a point on the line from A to B perpendicular to C.
function PointOnLine(const A, B, C: TFloatPoint): TFloatPoint;
var
  dx, dy, r: Single;
begin
  dx := B.X - A.X;
  dy := B.Y - A.Y;
  r := ((C.X - A.X) * dx + (C.Y - A.Y) * dy) / (Sqr(dx) + Sqr(dy));
  if InRange(r, 0, 1) then
  begin
    Result.X := A.X + r * dx;
    Result.Y := A.Y + r * dy;
  end
  else
  begin
    if SqrDistance(A, C) < SqrDistance(B, C) then
      Result := A
    else
      Result := B;
  end;
end;

function Flatness(P1, P2, P3, P4: TFloatPoint): Single;
begin
  Result :=
    Abs(P1.X + P3.X - 2*P2.X) +
    Abs(P1.Y + P3.Y - 2*P2.Y) +
    Abs(P2.X + P4.X - 2*P3.X) +
    Abs(P2.Y + P4.Y - 2*P3.Y);
end;


procedure OffsetVertex(var Vertex: TBezierVertex; const Dx, Dy: TFloat);
begin
  Vertex.Points[0].X := Vertex.Points[0].X + Dx;
  Vertex.Points[0].Y := Vertex.Points[0].Y + Dy;
  Vertex.Points[1].X := Vertex.Points[1].X + Dx;
  Vertex.Points[1].Y := Vertex.Points[1].Y + Dy;
  Vertex.Points[2].X := Vertex.Points[2].X + Dx;
  Vertex.Points[2].Y := Vertex.Points[2].Y + Dy;
end;


function ZeroVertex(const Point: TFloatPoint): TBezierVertex;
begin
  Result.Points[0] := Point;
  Result.Points[1] := Point;
  Result.Points[2] := Point;
end;

function BezierSegment(const Vertices: TArrayOfBezierVertex; Index: Integer): TBezierSegment;
begin
  if Index = High(Vertices) then
  begin
    Result.P1 := Vertices[Index].Points[0];
    Result.P2 := Vertices[Index].Points[2];
    Result.P3 := Vertices[0].Points[1];
    Result.P4 := Vertices[0].Points[0];
  end
  else
  begin
    Result.P1 := Vertices[Index].Points[0];
    Result.P2 := Vertices[Index].Points[2];
    Result.P3 := Vertices[Index + 1].Points[1];
    Result.P4 := Vertices[Index + 1].Points[0];
  end;
end;

procedure TBezierCurve.AddVertex(const V: TBezierVertex);
var
  H, L: Integer;
begin
  H := High(FVertices);
  L := Length(FVertices[H]);
  SetLength(FVertices[H], L + 1);
  FVertices[H][L] := V;
end;

function BezierCurveToPolygonX(const Vertices: TArrayOfBezierVertex;
  Closed: Boolean = True): TArrayOfFixedPoint;
var
  I, Index: Integer;

  procedure Recurse(const P1, P2, P3, P4: TFloatPoint);
  var
    P12, P23, P34, P123, P234, P1234: TFloatPoint;
  begin
    if Flatness(P1, P2, P3, P4) < BezierTolerance then
    begin
      if High(Result) < Index then
        SetLength(Result, Length(Result) * 2);
      Result[Index] := FixedPoint(P4);
      Inc(Index);
    end
    else
    begin
      P12.X   := (P1.X + P2.X) * 1/2;
      P12.Y   := (P1.Y + P2.Y) * 1/2;
      P23.X   := (P2.X + P3.X) * 1/2;
      P23.Y   := (P2.Y + P3.Y) * 1/2;
      P34.X   := (P3.X + P4.X) * 1/2;
      P34.Y   := (P3.Y + P4.Y) * 1/2;
      P123.X  := (P12.X + P23.X) * 1/2;
      P123.Y  := (P12.Y + P23.Y) * 1/2;
      P234.X  := (P23.X + P34.X) * 1/2;
      P234.Y  := (P23.Y + P34.Y) * 1/2;
      P1234.X := (P123.X + P234.X) * 1/2;
      P1234.Y := (P123.Y + P234.Y) * 1/2;

      Recurse(P1, P12, P123, P1234);
      Recurse(P1234, P234, P34, P4);
    end;
  end;

begin
  Index := 0;
  SetLength(Result, 8);
  Result[Index] := FixedPoint(Vertices[0].Point);
  Inc(Index);

  for I := 0 to High(Vertices) + Ord(Closed) - 1 do
    with BezierSegment(Vertices, I) do
      Recurse(P1, P2, P3, P4);
  SetLength(Result, Index);
end;

function BezierCurveToPolygonF(const Vertices: TArrayOfBezierVertex;
  Closed: Boolean = True): TArrayOfFloatPoint;
var
  I, Index: Integer;

  procedure Recurse(const P1, P2, P3, P4: TFloatPoint);
  var
    P12, P23, P34, P123, P234, P1234: TFloatPoint;
  begin
    if Flatness(P1, P2, P3, P4) < BezierTolerance then
    begin
      if High(Result) < Index then
        SetLength(Result, Length(Result) * 2);
      Result[Index] := P4;
      Inc(Index);
    end
    else
    begin
      P12.X   := (P1.X + P2.X) * 1/2;
      P12.Y   := (P1.Y + P2.Y) * 1/2;
      P23.X   := (P2.X + P3.X) * 1/2;
      P23.Y   := (P2.Y + P3.Y) * 1/2;
      P34.X   := (P3.X + P4.X) * 1/2;
      P34.Y   := (P3.Y + P4.Y) * 1/2;
      P123.X  := (P12.X + P23.X) * 1/2;
      P123.Y  := (P12.Y + P23.Y) * 1/2;
      P234.X  := (P23.X + P34.X) * 1/2;
      P234.Y  := (P23.Y + P34.Y) * 1/2;
      P1234.X := (P123.X + P234.X) * 1/2;
      P1234.Y := (P123.Y + P234.Y) * 1/2;

      Recurse(P1, P12, P123, P1234);
      Recurse(P1234, P234, P34, P4);
    end;
  end;

begin
  Index := 0;
  SetLength(Result, 8);
  for I := 0 to High(Vertices) + Ord(Closed) - 1 do
    with BezierSegment(Vertices, I) do
      Recurse(P1, P2, P3, P4);
  SetLength(Result, Index);
end;


procedure TBezierCurve.AppendToPolygon(Polygon: TPolygon32);
var
  I, J: Integer;
begin
  J := High(Polygon.Points);
  for I := 0 to High(FVertices) do
  begin
    Polygon.NewLine;
    Polygon.Points[I + J] := BezierCurveToPolygonX(FVertices[I], Closed);
  end;
end;

procedure TBezierCurve.Delete;
begin
  FVertices := nil;
end;

procedure TBezierCurve.NewContour;
begin
  SetLength(FVertices, Length(FVertices) + 1);
end;

{ TCustomShape }

procedure TCustomShape.AssignTo(Dest: TPersistent);
begin
  if Dest is TPolygon32 then
  begin
    TPolygon32(Dest).Points := nil;
    TPolygon32(Dest).Normals := nil;
    AppendToPolygon(TPolygon32(Dest));
  end;
end;

function TCustomShape.AsPolygon: TPolygon32;
begin
  Result := TPolygon32.Create;
  AppendToPolygon(Result);
end;

//============================================================================//
// Text routines
//============================================================================//

const
  Identity_mat2: tmat2 = (
    eM11: (fract: 0; Value: 1);
    eM12: (fract: 0; Value: 0);
    eM21: (fract: 0; Value: 0);
    eM22: (fract: 0; Value: 1);
  );

  VertFlip_mat2: tmat2 = (
    eM11: (fract: 0; Value: 1);
    eM12: (fract: 0; Value: 0);
    eM21: (fract: 0; Value: 0);
    eM22: (fract: 0; Value: -1);
  );

function QuadToBezier(Q0, Q1, Q2: TFixedPoint): TBezierVertex;
// Q-spline to Bezier curve:
// B0 = Q0
// B1 = (Q0 + 2*Q1) / 3
// B2 = (Q0 + 2*Q2) / 3
begin
  with Result do
  begin
    Points[0] := FloatPoint(Q0);
    Points[1].X := (Q0.X + 2*Q1.X) / (3 * FixedOne);
    Points[1].Y := (Q0.Y + 2*Q1.Y) / (3 * FixedOne);
    Points[2].X := (Q0.X + 2*Q2.X) / (3 * FixedOne);
    Points[2].Y := (Q0.Y + 2*Q2.Y) / (3 * FixedOne);
  end;
end;

function PointFXtoFixedPoint(const Point: tagPointFX): TFixedPoint;
begin
  Result.X := Point.X.Value shl 16 or Point.X.Fract;
  Result.Y := Point.Y.Value shl 16 or Point.Y.Fract;
end;

function GlyphOutlineToBezierCurve(Dst: TBitmap32; DstX, DstY: TFloat;
  const Glyph: Integer; out Metrics: TGlyphMetrics): TBezierCurve;
var
  I, J, K, S, Res: Integer;
  Code: LongWord;
  PGlyphMem, PBuffer: PTTPolygonHeader;
  PPCurve: PTTPolyCurve;

  Handle: HDC;
  BezierCurve: TBezierCurve;
  P: TFloatPoint;
  V: TBezierVertex;
  Q0, Q1, Q2: TFixedPoint;

  procedure AddToBezierCurve;
  begin
    V := QuadToBezier(Q0, Q2, Q1);
    OffsetVertex(V, DstX, DstY);
    BezierCurve.AddVertex(V);
  end;

begin
  Dst.UpdateFont;
  Handle := Dst.Handle;

  Res := GetGlyphOutlineW(Handle, Glyph, GGODefaultFlags, Metrics, 0, nil, VertFlip_mat2);

  PGlyphMem := StackAlloc(Res);
  PBuffer := PGlyphMem;

  Res := GetGlyphOutlineW(Handle, Glyph, GGODefaultFlags, Metrics, Res, PBuffer, VertFlip_mat2);

  if (Res = GDI_ERROR) or (PBuffer^.dwType <> TT_POLYGON_TYPE) then
  begin
    Result := nil;
    StackFree(PGlyphMem);
    Exit;
  end;

  BezierCurve := TBezierCurve.Create;
  try
    while Res > 0 do
    begin
      BezierCurve.NewContour;

      S := PBuffer.cb - SizeOf(TTTPolygonHeader);
      Integer(PPCurve) := Integer(PBuffer) + SizeOf(TTTPolygonHeader);
      Q0 := PointFXtoFixedPoint(PBuffer.pfxStart);
      Q2 := Q0;

      while S > 0 do
      begin
        case PPCurve.wType of
          TT_PRIM_LINE:
            begin
              Q1 := Q0;
              AddToBezierCurve;

              P := FloatPoint(TFixedPoint(Q0));
              P.X := P.X + DstX;
              P.Y := P.Y + DstY;
              BezierCurve.AddVertex(ZeroVertex(P));
              for J := 0 to PPCurve.cpfx - 1 do
              begin
                P := FloatPoint(TFixedPoint(PPCurve.apfx[J]));
                P.X := P.X + DstX;
                P.Y := P.Y + DstY;
                BezierCurve.AddVertex(ZeroVertex(P));
              end;

              Q0 := PointFXtoFixedPoint(PPCurve.apfx[PPCurve.cpfx - 1]);
              Q2 := Q0;
            end;
          TT_PRIM_QSPLINE:
            begin

              for J := 0 to PPCurve.cpfx - 2 do
              begin
                Q1 := PointFXtoFixedPoint(PPCurve.apfx[J]);
                AddToBezierCurve;

                if J < PPCurve.cpfx - 2 then
                with PointFXtoFixedPoint(PPCurve.apfx[J + 1]) do
                  begin
                    Q0.x := (Q1.x + x) div 2;
                    Q0.y := (Q1.y + y) div 2;
                  end
                else
                  Q0 := PointFXtoFixedPoint(PPCurve.apfx[J + 1]);

                Q2 := Q1;
              end;
            end;
        end;
        K := (PPCurve.cpfx - 1) * SizeOf(TPointFX) + SizeOf(TTPolyCurve);
        Dec(S, K);
        Inc(Integer(PPCurve), K);
      end;

      Dec(Integer(PPCurve), K);
      if PPCurve.wType = TT_PRIM_QSPLINE then
      begin
        Q1 := PointFXtoFixedPoint(PPCurve.apfx[PPCurve.cpfx - 1]);
        AddToBezierCurve;
      end;

      Dec(Res, PBuffer.cb);
      Inc(Integer(PBuffer), PBuffer.cb);
    end;

  except
    BezierCurve.Free;
  end;
  Result := BezierCurve;
  StackFree(PGlyphMem);
end;

function TextToPolygon(Dst: TBitmap32; DstX, DstY: TFloat; const Text: WideString): TPolygon32;
var
  I, TextLen: Integer;
  CharValue: Integer;
  B: TBezierCurve;
  Metrics: TGlyphMetrics;
  TextMetric: TTextMetric;
  DC: HDC;
  X, Y: TFloat;
begin
  DC := Dst.Handle;
  Dst.UpdateFont;
  SelectObject(DC, Dst.Font.Handle);
  GetTextMetrics(DC, TextMetric);

  TextLen := Length(Text);

  // Initialize polygon
  Result := TPolygon32.Create;
  Result.Antialiased := True;
  Result.AntialiasMode := am4times;
  Result.FillMode := pfWinding;

  X := DstX;
  Y := DstY + TextMetric.tmAscent;
  for I := 1 to TextLen do
  begin
    CharValue := Ord(Text[I]);
    if CharValue <= 31 then
    begin
      case CharValue of
        10: X := DstX;
        13: Y := Y + TextMetric.tmHeight;
      end;
    end
    else
    begin
      B := GlyphOutlineToBezierCurve(Dst, X, Y, CharValue, Metrics);
      if Assigned(B) then
      try
        B.AppendToPolygon(Result);
      finally
        B.Free;
      end;
      X := X + Metrics.gmCellIncX;
    end;
  end;
end;

function TextToPolygonF(Dst: TBitmap32; DstX, DstY: TFloat; const Text: WideString): TArrayOfArrayOfFloatPoint;
var
  I, J, K, TextLen: Integer;
  CharValue: Integer;
  B: TBezierCurve;
  Metrics: TGlyphMetrics;
  TextMetric: TTextMetric;
  DC: HDC;
  X, Y: TFloat;
begin
  DC := Dst.Handle;
  Dst.UpdateFont;
  SelectObject(DC, Dst.Font.Handle);
  GetTextMetrics(DC, TextMetric);

  TextLen := Length(Text);

  X := DstX;
  Y := DstY;

  K := 0;
  Y := Y + TextMetric.tmAscent;
  for I := 1 to TextLen do
  begin
    CharValue := Ord(Text[I]);
    if CharValue <= 31 then
    begin
      case CharValue of
        10: X := DstX;
        13: Y := Y + TextMetric.tmHeight;
      end;
    end
    else
    begin
      B := GlyphOutlineToBezierCurve(Dst, X, Y, CharValue, Metrics);
      if Assigned(B) then
      try
        for  J := 0 to High(B.Vertices) do
        begin
          SetLength(Result, K + 1);
          Result[K] := BezierCurveToPolygonF(B.Vertices[J]);
          Inc(K);
        end;
      finally
        B.Free;
      end;
      X := X + Metrics.gmCellIncX;
    end;
  end;
end;



procedure RenderText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  OutlineColor, FillColor: TColor32; FillCallback: TFillLineEvent;
  Transformation: TTransformation = nil);
var
  P: TPolygon32;
begin
  P := TextToPolygon(Dst, X, Y, Text);
  try
    if Assigned(FillCallback) then
      P.Draw(Dst, OutlineColor, FillCallback, Transformation)
    else
      P.Draw(Dst, OutlineColor, FillColor, Transformation);
  finally
    P.Free;
  end;
end;

procedure DrawText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  OutlineColor, FillColor: TColor32; Transformation: TTransformation = nil); overload;
begin
  RenderText(Dst, X, Y, Text, OutlineColor, FillColor, nil, Transformation);
end;

procedure DrawText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  OutlineColor: TColor32; Filler: TCustomPolygonFiller;
  Transformation: TTransformation = nil); overload;
begin
  RenderText(Dst, X, Y, Text, OutlineColor, 0, Filler.FillLine, Transformation);
end;

procedure DrawTextOutline(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  Color: TColor32; Transformation: TTransformation = nil);
begin
  RenderText(Dst, X, Y, Text, Color, 0, nil, Transformation);
end;

procedure FillText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  Filler: TCustomPolygonFiller; Transformation: TTransformation = nil);
begin
  RenderText(Dst, X, Y, Text, 0, 0, Filler.FillLine, Transformation);
end;


//============================================================================//

// SplineToPolygon
function MakeCurve(const Points: TArrayOfFloatPoint; Kernel: TCustomKernel;
  Closed: Boolean; StepSize: Integer): TArrayOfFixedPoint; overload;
var
  I, J, F, H, Index, LastIndex, Steps, R: Integer;
  K, V, W, dx, dy, X, Y: Single;
  Filter: TFilterMethod;
  WrapProc: TWrapProc;
  PPoint: PFixedPoint;
const
  WRAP_PROC: array[Boolean] of TWrapProc = (Clamp, Wrap);
begin
  WrapProc := Wrap_PROC[Closed];
  Filter := Kernel.Filter;
  R := Ceil(Kernel.GetWidth);
  H := High(Points);

  LastIndex := H - Ord(not Closed);
  Steps := 0;
  for I := 0 to LastIndex do
  begin
    Index := WrapProc(I + 1, H);
    dx := Points[Index].X - Points[I].X;
    dy := Points[Index].Y - Points[I].Y;
    Inc(Steps, Floor(Hypot(dx, dy) / StepSize) + 1);
  end;

  SetLength(Result, Steps);
  PPoint := @Result[0];

  for I := 0 to LastIndex do
  begin
    Index := WrapProc(I + 1, H);
    dx := Points[Index].X - Points[I].X;
    dy := Points[Index].Y - Points[I].Y;
    Steps := Floor(Hypot(dx, dy) / StepSize);
    if Steps > 0 then
    begin
      K := 1 / Steps;
      for J := 0 to Steps do
      begin
        X := 0; Y := 0;
        V := J * K;
        for F := -R to R do
        begin
          Index := WrapProc(I - F, H);
          W := Filter(F + V);
          X := X + W * Points[Index].X;
          Y := Y + W * Points[Index].Y;
        end;
        PPoint^ := FixedPoint(X, Y);
        Inc(PPoint);
      end;
    end;
  end;
end;

function MakeCurve(const Points: TArrayOfFixedPoint; Kernel: TCustomKernel;
  Closed: Boolean; StepSize: Integer): TArrayOfFixedPoint; overload;
var
  I, J, F, H, Index, LastIndex, Steps, R: Integer;
  K, V, W, dx, dy, X, Y: Single;
  Filter: TFilterMethod;
  WrapProc: TWrapProc;
  PPoint: PFixedPoint;
const
  WRAP_PROC: array[Boolean] of TWrapProc = (Clamp, Wrap);
begin
  WrapProc := WRAP_PROC[Closed];
  Filter := Kernel.Filter;
  R := Ceil(Kernel.GetWidth);
  H := High(Points);

  LastIndex := H - Ord(not Closed);
  Steps := 0;
  for I := 0 to LastIndex do
  begin
    Index := WrapProc(I + 1, H);
    dx := Points[Index].X - Points[I].X;
    dy := Points[Index].Y - Points[I].Y;
    Inc(Steps, Floor(Hypot(dx, dy) / StepSize) + 1);
  end;

  SetLength(Result, Steps);
  PPoint := @Result[0];

  for I := 0 to LastIndex do
  begin
    Index := WrapProc(I + 1, H);
    dx := Points[Index].X - Points[I].X;
    dy := Points[Index].Y - Points[I].Y;
    Steps := Floor(Hypot(dx, dy) / StepSize);
    if Steps > 0 then
    begin
      K := 1 / Steps;
      for J := 0 to Steps do
      begin
        X := 0; Y := 0;
        V := J * K;
        for F := -R to R do
        begin
          Index := WrapProc(I - F, H);
          W := Filter(F + V);
          X := X + W * Points[Index].X;
          Y := Y + W * Points[Index].Y;
        end;
        PPoint^ := FixedPoint(X, Y);
        Inc(PPoint);
      end;
    end;
  end;
end;

{ TCustomCurve }

procedure TCustomCurve.Delete;
begin
//
end;

{ TSplineCurve }

procedure TSplineCurve.AddPoint(const Point: TFloatPoint);
var
  H, L: Integer;
begin
  H := High(Points);
  L := Length(Points[H]);
  SetLength(Points[H], L + 1);
  Points[H][L] := Point;
end;

procedure TSplineCurve.AppendToPolygon(Polygon: TPolygon32);
var
  I, H: Integer;
begin
  H := High(Polygon.Points);
  for I := 0 to High(FPoints) do
  begin
    Polygon.NewLine;
    Inc(H);
    Polygon.Points[H] := MakeCurve(FPoints[I], FKernel, True, 2);
  end;
end;

constructor TSplineCurve.Create;
begin
  FKernel := TCubicKernel.Create;
  NewContour;
end;

procedure TSplineCurve.NewContour;
begin
  SetLength(FPoints, Length(FPoints) + 1);
end;

procedure TSplineCurve.SetKernel(const Value: TCustomKernel);
begin
  if Assigned(FKernel) then
    FKernel.Free;
  FKernel := Value;
end;

{ TEllipse }

procedure TEllipse.AppendToPolygon(Polygon: TPolygon32);
begin

end;

end.
