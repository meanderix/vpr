unit FMX.VPR;

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

uses
  System.Types, FMX.Types;

type
  PPolyPolygon = ^TPolyPolygon;
  TPolyPolygon = array of TPolygon;

  PSingleArray = ^TSingleArray;
  TSingleArray = array [0..0] of Single;
  PInteger = ^Integer;

  TSpanMode = (smUnpacked, smPacked);

  PValueSpan = ^TValueSpan;
  TValueSpan = record
    X1, X2: Integer;
    Values: PSingleArray;
  end;

  TRenderSpanEvent = procedure(const Span: TValueSpan; DstY: Integer) of object;
  TRenderSpanProc = procedure(Data: Pointer; const Span: TValueSpan; DstY: Integer);

  { TPolygonRenderer }
  TPolygonRenderer = class
  protected
    procedure RenderSpan(const Span: TValueSpan; DstY: Integer); virtual; abstract;
  end;

procedure RenderPolyPolygon(const Points: TPolyPolygon;
  const ClipRect: TRectF; const RenderProc: TRenderSpanProc; Data: Pointer = nil); overload;
procedure RenderPolygon(const Points: TPolygon;
  const ClipRect: TRectF; const RenderProc: TRenderSpanProc; Data: Pointer = nil); overload;
procedure RenderPolyPolygon(const Points: TPolyPolygon;
  const ClipRect: TRectF; const RenderProc: TRenderSpanEvent); overload;
procedure RenderPolygon(const Points: TPolygon;
  const ClipRect: TRectF; const RenderProc: TRenderSpanEvent); overload;

implementation

uses
  Winapi.Windows, System.SysUtils, System.Math, FMX.VPR.Polygons;

type
  TArrayOfValueSpan = array of TValueSpan;

  PValueSpanArray = ^TValueSpanArray;
  TValueSpanArray = array [0..0] of TValueSpan;

  PLineSegment = ^TLineSegment;
  TLineSegment = array [0..1] of TPointF;
  TArrayOfLineSegment = array of TLineSegment;

  PLineSegmentArray = ^TLineSegmentArray;
  TLineSegmentArray = array [0..0] of TLineSegment;

  PScanLine = ^TScanLine;
  TScanLine = record
    Segments: PLineSegmentArray;
    Count: Integer;
    Y: Integer;
    X1, X2: Integer;
    SpanData: PSingleArray;
  end;
  TScanLines = array of TScanLine;
  PScanLineArray = ^TScanLineArray;
  TScanLineArray = array [0..0] of TScanLine;

procedure IntegrateSegment(var P1, P2: TPointF; Values: PSingleArray);
var
  X1, X2, I: Integer;
  Dx, Dy, DyDx, Sx, Y, fracX1, fracX2: Single;
begin
  X1 := _Round(P1.X);
  X2 := _Round(P2.X);
  if X1 = X2 then
  begin
    Values[X1] := Values[X1] + 0.5 * (P2.X - P1.X) * (P1.Y + P2.Y);
  end
  else
  begin
    fracX1 := P1.X - X1;
    fracX2 := P2.X - X2;

    Dx := P2.X - P1.X;
    Dy := P2.Y - P1.Y;
    DyDx := Dy/Dx;

    if X1 < X2 then
    begin
      Sx := 1 - fracX1;
      Y := P1.Y + Sx * DyDx;
      Values[X1] := Values[X1] + 0.5 * (P1.Y + Y) * Sx;
      for I := X1 + 1 to X2 - 1 do
      begin
        Values[I] := Values[I] + (Y + DyDx * 0.5);     // N: Sx = 1
        Y := Y + DyDx;
      end;

      Sx := fracX2;
      Values[X2] := Values[X2] + 0.5 * (Y + P2.Y) * Sx;
    end
    else // X1 > X2
    begin
      Sx := fracX1;
      Y := P1.Y - Sx * DyDx;
      Values[X1] := Values[X1] - 0.5 * (P1.Y + Y) * Sx;
      for I := X1 - 1 downto X2 + 1 do
      begin
        Values[I] := Values[I] - (Y - DyDx * 0.5);    // N: Sx = -1
        Y := Y - DyDx;
      end;
      Sx := 1 - fracX2;
      Values[X2] := Values[X2] - 0.5 * (Y + P2.Y) * Sx;
    end;
  end;
end;

{$IFDEF CPUX86}
// Aligned SSE2 version -- Credits: Sanyin <prevodilac@hotmail.com>
procedure CumSum(Values: PSingleArray; Count: Integer);
asm
        MOV     ECX,EDX
        CMP     ECX,2       // if count < 2, exit
        JL      @END
        CMP     ECX,32      // if count < 32, avoid SSE2 overhead
        JL      @SMALL

{--- align memory ---}
        PUSH    EBX
        PXOR    XMM4,XMM4
        MOV     EBX,EAX
        AND     EBX,15       // get aligned count
        JZ      @ENDALIGNING // already aligned
        ADD     EBX,-16
        NEG     EBX          // get bytes to advance
        JZ      @ENDALIGNING // already aligned

        MOV     ECX,EBX
        SAR     ECX,2        // div with 4 to get cnt
        SUB     EDX,ECX

        ADD     EAX,4
        DEC     ECX
        JZ      @SETUPLAST   // one element

@ALIGNINGLOOP:
        FLD     DWORD PTR [EAX-4]
        FADD    DWORD PTR [EAX]
        FSTP    DWORD PTR [EAX]
        ADD     EAX,4
        DEC     ECX
        JNZ     @ALIGNINGLOOP

@SETUPLAST:
        MOVUPS  XMM4,[EAX-4]
        PSLLDQ  XMM4,12
        PSRLDQ  XMM4,12

@ENDALIGNING:
        POP     EBX
        PUSH    EBX
        MOV     ECX,EDX
        SAR     ECX,2
@LOOP:
        MOVAPS  XMM0,[EAX]
        PXOR    XMM5,XMM5
        PCMPEQD XMM5,XMM0
        PMOVMSKB EBX,XMM5
        CMP     EBX,$0000FFFF
        JNE     @NORMAL
        PSHUFD  XMM0,XMM4,0
        JMP     @SKIP

@NORMAL:
        ADDPS   XMM0,XMM4
        PSHUFD  XMM1,XMM0,$e4
        PSLLDQ  XMM1,4
        PSHUFD  XMM2,XMM1,$90
        PSHUFD  XMM3,XMM1,$40
        ADDPS   XMM2,XMM3
        ADDPS   XMM1,XMM2
        ADDPS   XMM0,XMM1

        PSHUFLW XMM4,XMM0,$E4
        PSRLDQ  XMM4,12

@SKIP:
        PREFETCHNTA [eax+16*16*2]
        MOVAPS  [EAX],XMM0
        ADD     EAX,16
        SUB     ECX,1
        JNZ     @LOOP
        POP     EBX
        MOV     ECX,EDX
        SAR     ECX,2
        SHL     ECX,2
        SUB     EDX,ECX
        MOV     ECX,EDX
        JZ      @END

@LOOP2:
        FLD     DWORD PTR [EAX-4]
        FADD    DWORD PTR [EAX]
        FSTP    DWORD PTR [EAX]
        ADD     EAX,4
        DEC     ECX
        JNZ     @LOOP2
        JMP     @END

@SMALL:
        MOV     ECX,EDX
        ADD     EAX,4
        DEC     ECX
@LOOP3:
        FLD     DWORD PTR [EAX-4]
        FADD    DWORD PTR [EAX]
        FSTP    DWORD PTR [EAX]
        ADD     EAX,4
        DEC     ECX
        JNZ     @LOOP3
@END:
end;
{$ELSE}
procedure CumSum(Values: PSingleArray; Count: Integer);
var
  I: Integer;
  V: Single;
begin
  V := Values[0];
  for I := 1 to Count - 1 do
  begin
    if PInteger(@Values[I])^ <> 0 then
      V := V + Values[I];
    Values[I] := V;
  end;
end;
{$ENDIF}

procedure ExtractSingleSpan(const ScanLine: TScanLine; out Span: TValueSpan;
  SpanData: PSingleArray);
var
  I, X: Integer;
  P: PPointF;
  S: PLineSegment;
  fracX: Single;
  Points: PPointArray;
  N: Integer;
begin
  N := ScanLine.Count * 2;
  Points := @ScanLine.Segments[0];
  Span.X1 := High(Integer);
  Span.X2 := Low(Integer);

  for I := 0 to N - 1 do
  begin
    P := @Points[I];
    X := _Round(P.X);
    if X < Span.X1 then Span.X1 := X;
    if P.Y = 1 then
    begin
      fracX := P.X - X;
      if Odd(I) then
      begin
        SpanData[X] := SpanData[X] + (1 - fracX); Inc(X);
        SpanData[X] := SpanData[X] + fracX;
      end
      else
      begin
        SpanData[X] := SpanData[X] - (1 - fracX); Inc(X);
        SpanData[X] := SpanData[X] - fracX;
      end;
    end;
    if X > Span.X2 then Span.X2 := X;
  end;

  CumSum(@SpanData[Span.X1], Span.X2 - Span.X1 + 1);

  for I := 0 to ScanLine.Count - 1 do
  begin
    S := @ScanLine.Segments[I];
    IntegrateSegment(S[0], S[1], SpanData);
  end;

  Span.Values := @SpanData[Span.X1];
end;

procedure AddSegment(const X1, Y1, X2, Y2: Single; var ScanLine: TScanLine);
var
  V1, V2: Integer;
  S: PLineSegment;
begin
  if (Y1 = 0) and (Y2 = 0) then Exit;  {** needed for proper clipping }
  with ScanLine do
  begin
    S := @Segments[Count];
    Inc(Count);
  end;

  S[0].X := X1;
  S[0].Y := Y1;
  S[1].X := X2;
  S[1].Y := Y2;

  V1 := Round(X1);
  V2 := Round(X2);
  if V1 < V2 then
  begin
    ScanLine.X1 := Min(V1, ScanLine.X1);
    ScanLine.X2 := Max(V2, ScanLine.X2);
  end
  else
  begin
    ScanLine.X1 := Min(V2, ScanLine.X1);
    ScanLine.X2 := Max(V1, ScanLine.X2);
  end;
end;

procedure DivideSegment(var P1, P2: TPointF; const ScanLines: PScanLineArray);
var
  Y, Y1, Y2: Integer;
  k, X: Single;
begin
  Y1 := _Round(P1.Y);
  Y2 := _Round(P2.Y);

  if Y1 = Y2 then
  begin
    AddSegment(P1.X, P1.Y - Y1, P2.X, P2.Y - Y1, ScanLines[Y1]);
  end
  else
  begin
    k := (P2.X - P1.X) / (P2.Y - P1.Y);
    if Y1 < Y2 then
    begin
      X := P1.X + (Y1 + 1 - P1.Y) * k;
      AddSegment(P1.X, P1.Y - Y1, X, 1, ScanLines[Y1]);
      for Y := Y1 + 1 to Y2 - 1 do
      begin
        AddSegment(X, 0, X + k, 1, ScanLines[Y]);
        X := X + k;
      end;
      AddSegment(X, 0, P2.X, P2.Y - Y2, ScanLines[Y2]);
    end
    else
    begin
      X := P1.X + (Y1 - P1.Y) * k;
      AddSegment(P1.X, P1.Y - Y1, X, 0, ScanLines[Y1]);
      for Y := Y1 - 1 downto Y2 + 1 do
      begin
        AddSegment(X, 1, X - k, 0, ScanLines[Y]);
        X := X - k
      end;
      AddSegment(X, 1, P2.X, P2.Y - Y2, ScanLines[Y2]);
    end;
  end;
end;

procedure BuildScanLines(const Points: TPolygon;
  out ScanLines: TScanLines);
var
  I, J, N, J0, J1, Y, YMin, YMax: Integer;
  PScanLines: PScanLineArray;
begin
  N := Length(Points);
  if N <= 2 then Exit;

  YMin := High(Integer);
  YMax := Low(Integer);
  for I := 0 to N - 1 do
  begin
    Y := _Round(Points[I].Y);
    if YMin > Y then YMin := Y;
    if YMax < Y then YMax := Y;
  end;

  SetLength(ScanLines, YMax - YMin + 2);
  PScanLines := @ScanLines[-YMin];

  {** compute array sizes for each scanline }
  J0 := _Round(Points[0].Y);
  for I := 1 to N - 1 do
  begin
    J1 := J0;
    J0 := _Round(Points[I].Y);
    if J0 <= J1 then
    begin
      Inc(PScanLines[J0].Count);
      Dec(PScanLines[J1 + 1].Count);
    end
    else
    begin
      Inc(PScanLines[J1].Count);
      Dec(PScanLines[J0 + 1].Count);
    end;
  end;

  {** allocate memory }
  J := 0;
  for I := 0 to High(ScanLines) do
  begin
    Inc(J, ScanLines[I].Count);
    GetMem(ScanLines[I].Segments, J * SizeOf(TLineSegment));
    ScanLines[I].Count := 0;
    ScanLines[I].Y := YMin + I;
    ScanLines[I].X1 := High(Integer);
    ScanLines[I].X2 := Low(Integer);
  end;

  for I := 0 to N - 2 do
  begin
    DivideSegment(Points[I], Points[I + 1], PScanLines);
  end;
end;

procedure MergeScanLines(const Src: TScanLines; var Dst: TScanLines);
var
  Temp: TScanLines;
  I, J, K, SrcCount, DstCount: Integer;
begin
  if Length(Src) = 0 then Exit;
  SetLength(Temp, Length(Src) + Length(Dst));

  I := 0;
  J := 0;
  K := 0;
  while (I <= High(Src)) and (J <= High(Dst)) do
  begin
    if Src[I].Y = Dst[J].Y then
    begin
      SrcCount := Src[I].Count;
      DstCount := Dst[J].Count;
      Temp[K].X1 := Min(Src[I].X1, Dst[J].X1);
      Temp[K].X2 := Max(Src[I].X2, Dst[J].X2);
      Temp[K].Count := SrcCount + DstCount;
      Temp[K].Y := Src[I].Y;
      GetMem(Temp[K].Segments, Temp[K].Count * SizeOf(TLineSegment));

      Move(Src[I].Segments[0], Temp[K].Segments[0], SrcCount * SizeOf(TLineSegment));
      Move(Dst[J].Segments[0], Temp[K].Segments[SrcCount], DstCount * SizeOf(TLineSegment));
      FreeMem(Src[I].Segments);
      FreeMem(Dst[J].Segments);
      Inc(I);
      Inc(J);
    end
    else if Src[I].Y < Dst[J].Y then
    begin
      Temp[K] := Src[I];
      Inc(I);
    end
    else
    begin
      Temp[K] := Dst[J];
      Inc(J);
    end;
    Inc(K);
  end;
  while I <= High(Src) do
  begin
    Temp[K] := Src[I];
    Inc(I); Inc(K);
  end;
  while J <= High(Dst) do
  begin
    Temp[K] := Dst[J];
    Inc(J); Inc(K);
  end;
  Dst := Copy(Temp, 0, K);
end;

procedure RenderScanline(var ScanLine: TScanLine;
  RenderProc: TRenderSpanProc; Data: Pointer; SpanData: PSingleArray; X1, X2: Integer);
var
  Span: TValueSpan;
begin
  if ScanLine.Count > 0 then
  begin
    ExtractSingleSpan(ScanLine, Span, SpanData);
    if Span.X1 < X1 then Span.X1 := X1;
    if Span.X2 > X2 then Span.X2 := X2;
    if Span.X2 < Span.X1 then Exit;

    RenderProc(Data, Span, ScanLine.Y);
    FillLongWord(@SpanData[Span.X1], Span.X2 - Span.X1 + 1, 0);
  end;
end;

procedure RenderPolyPolygon(const Points: TPolyPolygon;
  const ClipRect: TRectF; const RenderProc: TRenderSpanProc; Data: Pointer);
var
  ScanLines, Temp: TScanLines;
  I: Integer;
  Poly: TPolygon;
  SavedRoundMode: TFPURoundingMode;
  CX1, CX2: Integer;
  SpanData: PSingleArray;
begin
  if (Length(Points) = 0) or (not Assigned(RenderProc)) then Exit;

  SavedRoundMode := SetRoundMode(rmDown);
  try
    Poly := ClosePolygon(ClipPolygon(Points[0], ClipRect));
    BuildScanLines(Poly, ScanLines);
    for I := 1 to High(Points) do
    begin
      Poly := ClosePolygon(ClipPolygon(Points[I], ClipRect));
      BuildScanLines(Poly, Temp);
      MergeScanLines(Temp, ScanLines);
      Temp := nil;
    end;

    CX1 := Round(ClipRect.Left);
    CX2 := -Round(-ClipRect.Right) - 1;

    I := CX2 - CX1 + 4;
    GetMem(SpanData, I * SizeOf(Single));
    FillLongWord(SpanData, I, 0);

    for I := 0 to High(ScanLines) do
    begin
      if ScanLines[I].Y >= ClipRect.Bottom then Exit;
      RenderScanline(ScanLines[I], RenderProc, Data, @SpanData[-CX1 + 1], CX1, CX2);
      FreeMem(ScanLines[I].Segments);
    end;

    FreeMem(SpanData);
  finally
    SetRoundMode(SavedRoundMode);
  end;
end;

procedure RenderPolygon(const Points: TPolygon;
  const ClipRect: TRectF; const RenderProc: TRenderSpanProc; Data: Pointer);
var
  H: TPolyPolygon;
begin
  SetLength(H, 1);
  H[0] := Points;
  RenderPolyPolygon(H, ClipRect, RenderProc, Data);
end;

procedure RenderPolyPolygon(const Points: TPolyPolygon;
  const ClipRect: TRectF; const RenderProc: TRenderSpanEvent);
begin
  with TMethod(RenderProc) do
    RenderPolyPolygon(Points, ClipRect, Code, Data);
end;

procedure RenderPolygon(const Points: TPolygon;
  const ClipRect: TRectF; const RenderProc: TRenderSpanEvent);
begin
  with TMethod(RenderProc) do
    RenderPolygon(Points, ClipRect, Code, Data);
end;

end.
