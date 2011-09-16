unit FMX.VPR.Polygons;

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

uses
  Winapi.Windows, System.Types, System.Classes, FMX.Types, FMX.VPR;

type
  TJoinStyle = (jsMiter, jsBevel, jsRound);
  TEndStyle = (esButt, esSquare, esRound);

function PolyPolylineFS(const Points: TPolyPolygon;
  Closed: Boolean = False; StrokeWidth: Single = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: Single = 4.0): TPolyPolygon;
function PolylineFS(const Points: TPolygon;
  Closed: Boolean = False; StrokeWidth: Single = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: Single = 4.0): TPolyPolygon;
function DashedPolylineFS(const Points: TPolygon;
  const DashArray: TDashArray; DashOffset: Single = 0;
  Closed: Boolean = False; StrokeWidth: Single = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: Single = 4.0): TPolyPolygon;
function BuildDashedLine(const Points: TPolygon; const DashArray: TDashArray;
  DashOffset: Single = 0; Scale: Single = 1): TPolyPolygon;

function PolygonBounds(const Points: TPolygon): TRectF; overload;
function PolygonBounds(const PP: TPolyPolygon): TRectF; overload;
function ClipPolygon(const Points: TPolygon; const ClipRect: TRectF): TPolygon;
function ClosePolygon(const Points: TPolygon): TPolygon;

procedure TextToPath(Font: HFONT; Path: TPathData;
  const ARect: TRectF; const Text: WideString; WordWrap: Boolean;
  HorzAlign, VertAlign: TTextAlign); overload;
function MeasureText(Font: HFONT; const ARect: TRectF; const Text: WideString;
  WordWrap: Boolean; HorzAlign, VertAlign: TTextAlign): TRectF; overload;

function _Round(var X: Single): Integer;
{$IFDEF CPUX86}
function Round(const X: Single): Integer;
{$ENDIF}
function Hypot(const X, Y: Single): Single;

function StackAlloc(Size: Integer): Pointer; register;
procedure StackFree(P: Pointer); register;

var
  UseHinting: Boolean = {$IFDEF NOHINTING}False{$ELSE}True{$ENDIF};

// stretching factor when calling GetGlyphOutline()
const
  HORZSTRETCH = 16;

implementation

uses
  System.Math;

function Hypot(const X, Y: Single): Single;
{$IFDEF PUREPASCAL}
begin
  Result := Sqrt(Sqr(X) + Sqr(Y));
{$ELSE}
asm
{$IFDEF CPUX86}
        FLD     X
        FMUL    ST,ST
        FLD     Y
        FMUL    ST,ST
        FADDP   ST(1),ST
        FSQRT
        FWAIT
{$ENDIF}
{$IFDEF CPUX64}
        MULSS   XMM0, XMM0
        MULSS   XMM1, XMM1
        ADDSS   XMM0, XMM1
        SQRTSS  XMM0, XMM0
{$ENDIF}
{$ENDIF}
end;

{$IFDEF CPUX86}
function _Round(var X: Single): Integer;
asm
        fld       [X]
        fistp     dword ptr [esp-4]
        mov       eax,[esp-4]
end;

function Round(const X: Single): Integer;
asm
        fld       X
        fistp     dword ptr [esp-4]
        mov       eax,[esp-4]
end;
{$ELSE}
function _Round(var X: Single): Integer;
begin
  Result := Round(X);
end;
{$ENDIF}

const
{$HINTS OFF}
  TWOPI = 6.283185308;
{$HINTS ON}

{$IFDEF CPUX86}
{ StackAlloc allocates a 'small' block of memory from the stack by
  decrementing SP.  This provides the allocation speed of a local variable,
  but the runtime size flexibility of heap allocated memory.  }
function StackAlloc(Size: Integer): Pointer; register;
asm
  POP   ECX          { return address }
  MOV   EDX, ESP
  ADD   EAX, 3
  AND   EAX, not 3   // round up to keep ESP dword aligned
  CMP   EAX, 4092
  JLE   @@2
@@1:
  SUB   ESP, 4092
  PUSH  EAX          { make sure we touch guard page, to grow stack }
  SUB   EAX, 4096
  JNS   @@1
  ADD   EAX, 4096
@@2:
  SUB   ESP, EAX
  MOV   EAX, ESP     { function result = low memory address of block }
  PUSH  EDX          { save original SP, for cleanup }
  MOV   EDX, ESP
  SUB   EDX, 4
  PUSH  EDX          { save current SP, for sanity check  (sp = [sp]) }
  PUSH  ECX          { return to caller }
end;

{ StackFree pops the memory allocated by StackAlloc off the stack.
- Calling StackFree is optional - SP will be restored when the calling routine
  exits, but it's a good idea to free the stack allocated memory ASAP anyway.
- StackFree must be called in the same stack context as StackAlloc - not in
  a subroutine or finally block.
- Multiple StackFree calls must occur in reverse order of their corresponding
  StackAlloc calls.
- Built-in sanity checks guarantee that an improper call to StackFree will not
  corrupt the stack. Worst case is that the stack block is not released until
  the calling routine exits. }
procedure StackFree(P: Pointer); register;
asm
  POP   ECX                     { return address }
  MOV   EDX, DWORD PTR [ESP]
  SUB   EAX, 8
  CMP   EDX, ESP                { sanity check #1 (SP = [SP]) }
  JNE   @@1
  CMP   EDX, EAX                { sanity check #2 (P = this stack block) }
  JNE   @@1
  MOV   ESP, DWORD PTR [ESP+4]  { restore previous SP  }
@@1:
  PUSH  ECX                     { return to caller }
end;
{$ENDIF}

{$IFDEF CPUX64}
function StackAlloc(Size: Integer): Pointer; register;
begin
  GetMem(Result, Size);
end;

procedure StackFree(P: Pointer); register;
begin
  FreeMem(P);
end;

(*
function StackAlloc(Size: Integer): Pointer; register;
asm
        MOV       RAX, RCX
        POP       RCX          { return address }
        MOV       RDX, RSP
        ADD       RAX, 3
        AND       RAX, NOT 3   // round up to keep ESP dword aligned
        CMP       RAX, 4092
        JLE       @@2
@@1:
        SUB       RSP, 4092
        PUSH      RAX          { make sure we touch guard page, to grow stack }
        SUB       RAX, 4096
        JNS       @@1
        ADD       RAX, 4096
@@2:
        SUB       RSP, RAX
        MOV       RAX, RSP     { function result = low memory address of block }
        PUSH      RDX          { save original SP, for cleanup }
        MOV       RDX, RSP
        SUB       RDX, 8
        PUSH      RDX          { save current SP, for sanity check  (sp = [sp]) }
        PUSH      RCX          { return to caller }
end;

procedure StackFree(P: Pointer); register;
asm
        MOV       RAX, RCX
        POP       RCX                     { return address }
        MOV       RDX, QWORD PTR [RSP]
        SUB       RAX, 8
        CMP       RDX, RSP                { sanity check #1 (SP = [SP]) }
        JNE       @@1
        CMP       RDX, RAX                { sanity check #2 (P = this stack block) }
        JNE       @@1
        MOV       RSP, QWORD PTR [RSP+8]  { restore previous SP  }
@@1:
        PUSH      RCX                     { return to caller }
end;
*)
{$ENDIF}

function ClosePolygon(const Points: TPolygon): TPolygon;
var
  L: Integer;
  P1, P2: TPointF;
begin
  L := Length(Points);
  Result := Points;
  if L <= 1 then
    Exit;

  P1 := Result[0];
  P2 := Result[L - 1];
  if (P1.X = P2.X) and (P1.Y = P2.Y) then
    Exit;

  SetLength(Result, L+1);
  Move(Result[0], Points[0], L*SizeOf(TPointF));
  Result[L] := P1;
end;

function InterpolateX(X: Single; const P1, P2: TPointF): TPointF;
var
  W: Single;
begin
  W := (X - P1.X) / (P2.X - P1.X);
  Result.X := X;
  Result.Y := P1.Y + W * (P2.Y - P1.Y);
end;

function InterpolateY(Y: Single; const P1, P2: TPointF): TPointF;
var
  W: Single;
begin
  W := (Y - P1.Y) / (P2.Y - P1.Y);
  Result.Y := Y;
  Result.X := P1.X + W * (P2.X - P1.X);
end;

function GetCode(const P: TPointF; const R: TRectF): Integer; inline;
begin
  Result := Ord(P.X >= R.Left) or
    (Ord(P.X <= R.Right) shl 1) or
    (Ord(P.Y >= R.Top) shl 2) or
    (Ord(P.Y <= R.Bottom) shl 3);
end;

function ClipPolygon(const Points: TPolygon; const ClipRect: TRectF): TPolygon;
type
  TInterpolateProc = function(X: Single; const P1, P2: TPointF): TPointF;
  TByteArray = array [0..0] of Byte;
  PByteArray = ^TByteArray;
const
  SAFEOVERSIZE = 5;
  POPCOUNT: array [0..15] of Integer =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4);
var
  I, J, K, L, N: Integer;
  X, Y, Z, Code, Count: Integer;
  Codes: PByteArray;
  NextIndex: PIntegerArray;
  Temp: PPointArray;
label
  ExitProc;

  procedure AddPoint(Index: Integer; const P: TPointF);
  begin
    Temp[K] := P;
    Codes[K] := GetCode(P, ClipRect);
    Inc(K);
    Inc(Count);
  end;

  function ClipEdges(Mask: Integer; V: Single; Interpolate: TInterpolateProc): Boolean;
  var
    I, NextI, StopIndex: Integer;
  begin
    I := 0;
    while (I < K) and (Codes[I] and Mask = 0) do Inc(I);

    Result := I = K;
    if Result then { all points outside }
    begin
      ClipPolygon := nil;
      Result := True;
      Exit;
    end;

    StopIndex := I;
    repeat
      NextI := NextIndex[I];

      if Codes[NextI] and Mask = 0 then  { inside -> outside }
      begin
        NextIndex[I] := K;
        NextIndex[K] := K + 1;
        AddPoint(I, Interpolate(V, Temp[I], Temp[NextI]));

        while Codes[NextI] and Mask = 0 do
        begin
          Dec(Count);
          Codes[NextI] := 0;
          I := NextI;
          NextI := NextIndex[I];
        end;
        { outside -> inside }
        NextIndex[K] := NextI;
        AddPoint(I, Interpolate(V, Temp[I], Temp[NextI]));
      end;

      I := NextI;
    until I = StopIndex;
  end;

begin
  N := Length(Points);
{$IFDEF USESTACKALLOC}
  Codes := StackAlloc(N * SAFEOVERSIZE);
{$ELSE}
  GetMem(Codes, N * SAFEOVERSIZE);
{$ENDIF}
  X := 15;
  Y := 0;
  for I := 0 to N - 1 do
  begin
    Code := GetCode(Points[I], ClipRect);
    Codes[I] := Code;
    X := X and Code;
    Y := Y or Code;
  end;
  if X = 15 then { all points inside }
  begin
    Result := Points;
  end
  else if Y <> 15 then { all points outside }
  begin
    Result := nil;
  end
  else
  begin
    Count := N;
    Z := Codes[N - 1];
    for I := 0 to N - 1 do
    begin
      Code := Codes[I];
      Inc(Count, POPCOUNT[Z xor Code]);
      Z := Code;
    end;
{$IFDEF USESTACKALLOC}
    Temp := StackAlloc(Count * SizeOf(TPointF));
    NextIndex := StackAlloc(Count * SizeOf(TPointF));
{$ELSE}
    GetMem(Temp, Count * SizeOf(TPointF));
    GetMem(NextIndex, Count * SizeOf(TPointF));
{$ENDIF}

    Move(Points[0], Temp[0], N * SizeOf(TPointF));
    for I := 0 to N - 2 do NextIndex[I] := I + 1;
    NextIndex[N - 1] := 0;

    Count := N;
    K := N;
    if X and 1 = 0 then if ClipEdges(1, ClipRect.Left, InterpolateX) then goto ExitProc;
    if X and 2 = 0 then if ClipEdges(2, ClipRect.Right, InterpolateX) then goto ExitProc;
    if X and 4 = 0 then if ClipEdges(4, ClipRect.Top, InterpolateY) then goto ExitProc;
    if X and 8 = 0 then if ClipEdges(8, ClipRect.Bottom, InterpolateY) then goto ExitProc;

    SetLength(Result, Count);

    { start with first point inside the clipping rectangle }
    I := 0;
    while Codes[I] = 0 do
      I := NextIndex[I];

    J := I;
    L := 0;
    repeat
      Result[L] := Temp[I];
      Inc(L);
      I := NextIndex[I];
    until I = J;

ExitProc:
{$IFDEF USESTACKALLOC}
    StackFree(NextIndex);
    StackFree(Temp);
{$ELSE}
    FreeMem(NextIndex);
    FreeMem(Temp);
{$ENDIF}
  end;
{$IFDEF USESTACKALLOC}
  StackFree(Codes);
{$ELSE}
  FreeMem(Codes);
{$ENDIF}
end;

function PolygonBounds(const Points: TPolygon): TRectF;
var
  I, N: Integer;
begin
  N := Length(Points);
  if N = 0 then
  begin
    Result := Default(TRectF);
    Exit;
  end;

  Result.Left := Points[0].X;
  Result.Top := Points[0].Y;
  Result.Right := Points[0].X;
  Result.Bottom := Points[0].Y;
  for I := 1 to N - 1 do
  begin
    Result.Left := Min(Result.Left, Points[I].X);
    Result.Right := Max(Result.Right, Points[I].X);
    Result.Top := Min(Result.Top, Points[I].Y);
    Result.Bottom := Max(Result.Bottom, Points[I].Y);
  end;
end;

function PolygonBounds(const PP: TPolyPolygon): TRectF;
var
  I: Integer;
begin
  Result := PolygonBounds(PP[0]);
  for I := 1 to High(PP) do
  begin
    Result := UnionRect(Result, PolygonBounds(PP[I]));
  end;
end;

function BuildArc(const P: TPointF; a1, a2, r: Single; Steps: Integer): TPolygon; overload;
var
  I, N: Integer;
  a, da, dx, dy: Single;
begin
  SetLength(Result, Steps);
  N := Steps - 1;
  da := (a2 - a1) / N;
  a := a1;
  for I := 0 to N do
  begin
    SinCos(a, dy, dx);
    Result[I].X := P.X + dx * r;
    Result[I].Y := P.Y + dy * r;
    a := a + da;
  end;
end;

function BuildArc(const P: TPointF; a1, a2, r: Single): TPolygon; overload;
const
  MINSTEPS = 6;
var
  Steps: Integer;
begin
  Steps := Max(MINSTEPS, Round(Sqrt(Abs(r)) * Abs(a2 - a1)));
  Result := BuildArc(P, a1, a2, r, Steps);
end;

function BuildNormals(const Points: TPolygon): TPolygon;
var
  I, Count, NextI: Integer;
  dx, dy, f: Single;
begin
  Count := Length(Points);
  SetLength(Result, Count);

  I := 0;
  NextI := 1;

  while I < Count do
  begin
    if NextI >= Count then NextI := 0;

    dx := Points[NextI].X - Points[I].X;
    dy := Points[NextI].Y - Points[I].Y;
    if (dx <> 0) or (dy <> 0) then
    begin
      f := 1/Sqrt(Sqr(dx) + Sqr(dy));//HypotRcp(dx, dy);
      dx := dx * f;
      dy := dy * f;
    end;

    Result[I].X := dy;
    Result[I].Y := -dx;

    Inc(I);
    Inc(NextI);
  end;
end;

function Grow(const Points: TPolygon; const Normals: TPolygon;
  const Delta: Single; JoinStyle: TJoinStyle; Closed: Boolean; MiterLimit: Single): TPolygon; overload;
const
  BUFFSIZEINCREMENT = 128;
var
  I, L, H: Integer;
  ResSize, BuffSize: integer;
  PX, PY, D, RMin: Single;
  A, B: TPointF;

  procedure AddPoint(const LongDeltaX, LongDeltaY: Single);
  begin
    if ResSize = BuffSize then
    begin
      inc(BuffSize, BUFFSIZEINCREMENT);
      SetLength(Result, BuffSize);
    end;
    with Result[ResSize] do
    begin
      X := PX + LongDeltaX;
      Y := PY + LongDeltaY;
    end;
    inc(resSize);
  end;

  procedure AddMitered(const X1, Y1, X2, Y2: Single);
  var
    R, CX, CY: Single;
  begin
    CX := X1 + X2;
    CY := Y1 + Y2;
    R := X1 * CX + Y1 * CY;
    if R < RMin then
    begin
      AddPoint(D * X1, D * Y1);
      AddPoint(D * X2, D * Y2);
    end
    else
    begin
      R := D / R;
      AddPoint(CX * R, CY * R)
    end;
  end;

  procedure AddBevelled(const X1, Y1, X2, Y2: Single);
  var
    R: Single;
  begin
    R := X1 * Y2 - X2 * Y1;
    if R * D <= 0 then
    begin
      AddMitered(X1, Y1, X2, Y2);
    end
    else
    begin
      AddPoint(D * X1, D * Y1);
      AddPoint(D * X2, D * Y2);
    end;
  end;

  procedure AddRoundedJoin(const X1, Y1, X2, Y2: Single);
  var
    R, a1, a2, da: Single;
    Arc: TPolygon;
    arcLen: integer;
  begin
    R := X1 * Y2 - X2 * Y1;
    if R * D <= 0 then
    begin
      AddMitered(X1, Y1, X2, Y2);
    end
    else
    begin
      a1 := ArcTan2(Y1, X1);
      a2 := ArcTan2(Y2, X2);
      da := a2 - a1;
      if da > Pi then
        a2 := a2 - TWOPI
      else if da < -Pi then
        a2 := a2 + TWOPI;
      Arc := BuildArc(PointF(PX, PY), a1, a2, D);

      arcLen := length(Arc);
      if resSize + arcLen >= BuffSize then
      begin
        inc(BuffSize, arcLen);
        SetLength(Result, BuffSize);
      end;
      Move(Arc[0], Result[resSize], Length(Arc) * SizeOf(TPointF));
      inc(resSize, arcLen);
    end;
  end;

  procedure AddJoin(const X, Y, X1, Y1, X2, Y2: Single);
  begin
    PX := X;
    PY := Y;
    case JoinStyle of
      jsMiter: AddMitered(A.X, A.Y, B.X, B.Y);
      jsBevel: AddBevelled(A.X, A.Y, B.X, B.Y);
      jsRound: AddRoundedJoin(A.X, A.Y, B.X, B.Y);
    end;
  end;

begin
  Result := nil;

  if Length(Points) <= 1 then Exit;

  D := Delta;
  RMin := 2/Sqr(MiterLimit);

  H := High(Points) - Ord(not Closed);
  while (H >= 0) and (Normals[H].X = 0) and (Normals[H].Y = 0) do Dec(H);

{** all normals zeroed => Exit }
  if H < 0 then Exit;

  L := 0;
  while (Normals[L].X = 0) and (Normals[L].Y = 0) do Inc(L);

  if Closed then
    A := Normals[H]
  else
    A := Normals[L];

  ResSize := 0;
  BuffSize := BUFFSIZEINCREMENT;
  SetLength(Result, BuffSize);

  for I := L to H do
  begin
    B := Normals[I];
    if (B.X = 0) and (B.Y = 0) then Continue;

    with Points[I] do AddJoin(X, Y, A.X, A.Y, B.X, B.Y);
    A := B;
  end;
  if not Closed then
    with Points[High(Points)] do AddJoin(X, Y, A.X, A.Y, A.X, A.Y);

  SetLength(Result, resSize);
end;

function Grow(const Points: TPolygon;
  const Delta: Single; JoinStyle: TJoinStyle; Closed: Boolean;
  MiterLimit: Single): TPolygon; overload;
var
  Normals: TPolygon;
begin
  Normals := BuildNormals(Points);
  Result := Grow(Points, Normals, Delta, JoinStyle, Closed, MiterLimit);
end;

function ReversePolygon(const Points: TPolygon): TPolygon;
var
  I, L: Integer;
begin
  L := Length(Points);
  SetLength(Result, L);
  Dec(L);
  for I := 0 to L do
    Result[I] := Points[L - I];
end;

function BuildLineEnd(const P, N: TPointF; const W: Single; EndStyle: TEndStyle): TPolygon;
var
  a1, a2: Single;
begin
  case EndStyle of
    esButt:
      begin
        Result := nil;
      end;
    esSquare:
      begin
        SetLength(Result, 2);
        Result[0].X := P.X + (N.X - N.Y) * W;
        Result[0].Y := P.Y + (N.Y + N.X) * W;
        Result[1].X := P.X - (N.X + N.Y) * W;
        Result[1].Y := P.Y - (N.Y - N.X) * W;
      end;
    esRound:
      begin
        a1 := ArcTan2(N.Y, N.X);
        a2 := ArcTan2(-N.Y, -N.X);
        if a2 < a1 then a2 := a2 + TWOPI;
        Result := BuildArc(P, a1, a2, W);
      end;
  end;
end;

function BuildPolyline(const Points: TPolygon; StrokeWidth: Single;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle; MiterLimit: Single): TPolygon;
var
  L, H: Integer;
  Normals: TPolygon;
  P1, P2, E1, E2: TPolygon;
  V: Single;
  P: PPointF;
begin
  V := StrokeWidth * 0.5;
  Normals := BuildNormals(Points);
  P1 := Grow(Points, Normals, V, JoinStyle, False, MiterLimit);
  P2 := ReversePolygon(Grow(Points, Normals, -V, JoinStyle, False, MiterLimit));

  H := High(Points) - 1;
  while (H >= 0) and (Normals[H].X = 0) and (Normals[H].Y = 0) do Dec(H);
  if H < 0 then
  begin
    // only one point => any normal will do
    H := 0;
    SetLength(Normals, 1);
    Normals[0].X := 0;
    Normals[0].Y := 1;
  end;
  L := 0;
  while (Normals[L].X = 0) and (Normals[L].Y = 0) do Inc(L);

  E1 := BuildLineEnd(Points[0], Normals[L], -V, EndStyle);
  E2 := BuildLineEnd(Points[High(Points)], Normals[H], V, EndStyle);

  SetLength(Result, Length(P1) + Length(P2) + Length(E1) + Length(E2));
  P := @Result[0];
  Move(E1[0], P^, Length(E1) * SizeOf(TPointF)); Inc(P, Length(E1));
  Move(P1[0], P^, Length(P1) * SizeOf(TPointF)); Inc(P, Length(P1));
  Move(E2[0], P^, Length(E2) * SizeOf(TPointF)); Inc(P, Length(E2));
  Move(P2[0], P^, Length(P2) * SizeOf(TPointF));
end;

function PolyPolylineFS(const Points: TPolyPolygon;
  Closed: Boolean = False; StrokeWidth: Single = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: Single = 4.0): TPolyPolygon;
var
  I: Integer;
  P1, P2: TPolygon;
  Dst: TPolyPolygon;
  Normals: TPolygon;
begin
  if Closed then
  begin
    SetLength(Dst, Length(Points)*2);
    for I := 0 to High(Points) do
    begin
      Normals := BuildNormals(Points[I]);
      P1 := Grow(Points[I], Normals, StrokeWidth * 0.5, JoinStyle, true, MiterLimit);
      P2 := Grow(Points[I], Normals, -StrokeWidth * 0.5, JoinStyle, true, MiterLimit);
      Dst[I * 2] := P1;
      Dst[I * 2 + 1] := ReversePolygon(P2);
    end;
  end
  else
  begin
    SetLength(Dst, Length(Points));
    for I := 0 to High(Points) do
      Dst[I] := BuildPolyline(Points[I], StrokeWidth, JoinStyle, EndStyle, MiterLimit);
  end;

  Result := Dst;
end;

function PolylineFS(const Points: TPolygon;
  Closed: Boolean = False; StrokeWidth: Single = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: Single = 4.0): TPolyPolygon;
var
  P: TPolyPolygon;
begin
  SetLength(P, 1);
  P[0] := Points;
  Result := PolyPolylineFS(P, Closed, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
end;

//============================================================================//

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


function BuildDashedLine(const Points: TPolygon; const DashArray: TDashArray;
  DashOffset: Single = 0; Scale: Single = 1): TPolyPolygon;
var
  I, J, DashIndex: Integer;
  Offset, dx, dy, d, v: Single;

  procedure AddPoint(X, Y: Single);
  var
    K: Integer;
  begin
    K := Length(Result[J]);
    SetLength(Result[J], K + 1);
    Result[J][K].X := X;
    Result[J][K].Y := Y;
  end;

begin
  DashIndex := 0;
  Offset := 0;
  DashOffset := DashArray[0] - DashOffset;

  v := 0;
  for I := 0 to High(DashArray) do v := v + DashArray[I];
  while DashOffset < 0 do DashOffset := DashOffset + v;
  while DashOffset >= v do DashOffset := DashOffset - v;

  while DashOffset - DashArray[DashIndex] > 0 do
  begin
    DashOffset := DashOffset - DashArray[DashIndex];
    Inc(DashIndex);
  end;
  DashOffset := DashOffset * Scale;

  J := 0;
  // N: second dimension might not be zero by default!
  SetLength(Result, 1, 0);
  if not Odd(DashIndex) then
    AddPoint(Points[0].X, Points[0].Y);
  for I := 1 to High(Points) do
  begin
    dx := Points[I].X - Points[I - 1].X;
    dy := Points[I].Y - Points[I - 1].Y;
    d := Hypot(dx, dy);
    if d = 0 then Continue;
    dx := dx / d;
    dy := dy / d;
    Offset := Offset + d;
    while Offset > DashOffset do
    begin
      v := Offset - DashOffset;
      AddPoint(Points[I].X - v * dx, Points[I].Y - v * dy);
      DashIndex := (DashIndex + 1) mod Length(DashArray);
      DashOffset := DashOffset + DashArray[DashIndex] * Scale;
      if Odd(DashIndex) then
      begin
        Inc(J);
        SetLength(Result, J + 1);
      end;
    end;
    if not Odd(DashIndex) then
      AddPoint(Points[I].X, Points[I].Y);
  end;
  if Length(Result[J]) = 0 then SetLength(Result, Length(Result) - 1);
end;

function DashedPolylineFS(const Points: TPolygon;
  const DashArray: TDashArray; DashOffset: Single = 0;
  Closed: Boolean = False; StrokeWidth: Single = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: Single = 4.0): TPolyPolygon;
var
  PP: TPolyPolygon;
  I: Integer;
begin
  PP := BuildDashedLine(Points, DashArray, DashOffset, StrokeWidth);
  SetLength(Result, Length(PP));
  for I := 0 to High(PP) do
    Result[I] := BuildPolyline(PP[I], StrokeWidth, JoinStyle, EndStyle, MiterLimit);
end;

end.
