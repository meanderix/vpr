{
Anti-Grain Geometry - Version 2.0
Copyright (C) 2002-2004 Maxim Shemanarev (McSeem)

Permission to copy, use, modify, sell and distribute this software 
is granted provided this copyright notice appears in all copies. 
This software is provided "as is" without express or implied
warranty, and with no claim as to its suitability for any purpose.
}

unit Agg;

interface

uses Types, Math, SysUtils;

type
  // Span types
  TSpanType = (stRGB24, stRGBA32);

  Int8    = ShortInt;
  Int8U   = Byte;
  Int16   = SmallInt;
  Int16U  = Word;
  Int32   = Integer;
  Int32U  = LongWord;

  PPByte  = ^PByte;
  PInt8U  = ^Int8U;
  PPInt8U = ^PInt8U;
  PInt16U = ^Int16U;

  TFillingRule = (frFillNonZero, frFillEvenOdd);

  TByteOrder = (boRgb, boBgr);
  TFillScanLine = procedure (X, Y, Len: integer; Ptr: PByte) of object;

  TRGBA8 = record
    R: Int8U;
    G: Int8U;
    B: Int8U;
    A: Int8U;
  end;
  
  function RGBA8: TRGBA8; overload;
  function RGBA8(R, G, B: LongWord; A: LongWord = 255): TRGBA8; overload;
  function RGBA8(APacked: LongWord; Order: TByteOrder): TRGBA8; overload;

  function Gradient(var Self: TRGBA8; C: TRGBA8; K: Double): TRGBA8;
  function Pre(var Self: TRGBA8): TRGBA8;
  function Opacity(var Self: TRGBA8): Double; overload;
  procedure Opacity(var Self: TRGBA8; A: Double); overload;

type

  TRenderingBuffer = class(TObject)
  private
    FBuf: PByte;
    FHeight: LongWord;
    FMaxHeight: LongWord;
    FRows: PPByte;
    FStride: Integer;
    FWidth: LongWord;
  public
    constructor Create(Buf: PByte; Width, Height: LongWord; Stride: Integer);
    destructor Destroy; override;
    function AbsStride: LongWord;
    procedure Attach(Buf: PByte; Width, Height: LongWord; Stride: Integer);
    function Inbox(X, Y: Integer): Boolean;
    function Row(Y: LongWord): PByte;
    property Buf: PByte read FBuf;
    property Height: LongWord read FHeight;
    property Stride: Integer read FStride;
    property Width: LongWord read FWidth;
  end;
  
  TScanLine = class(TObject)
  private
    FCounts: PInt16U;
    FCovers: PInt8U;
    FCurCount: PInt16U;
    FCurStartPtr: PPInt8U;
    FDx: Integer;
    FDy: Integer;
    FLastX: Integer;
    FLastY: Integer;
    FMaxLen: LongWord;
    FMinX: Integer;
    FNumSpans: LongWord;
    FStartPtrs: PPInt8U;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCell(X, Y: Integer; Cover: LongWord);
    procedure AddSpan(X, Y: Integer; Len, Cover: LongWord);
    function BaseX: Integer;
    function IsReady(Y: Integer): Integer;
    procedure Reset(MinX, MaxX: Integer; Dx: Integer = 0; Dy: Integer = 0);
    procedure ResetSpans;
    function Y: Integer;
    property NumSpans: LongWord read FNumSpans;
  end;
  
  TScanLineIter = class(TObject)
  private
    FCovers: PInt8U;
    FCurCount: PInt16U;
    FCurStartPtr: PPInt8U;
  public
    constructor Create(SL: TScanLine);
    function Covers: PInt8U;
    function Next: Integer;
    function NumPix: Integer;
  end;

  TSpanRGB24 = class;
  TSpanRGBA32 = class;

  TRenderer = class(TObject)
  private
    FOnFillScanline: TFillScanLine;
    FRBuf: TRenderingBuffer;
    FSpan: TSpanRGB24;
    FSpanType: TSpanType;
    function GetPixel(const X, Y: Integer): TRGBA8;
    procedure SetPixel(const X, Y: Integer; const Value: TRGBA8);
    procedure SetSpanType(const Value: TSpanType);
  public
    constructor Create(RBuf: TRenderingBuffer); virtual;
    destructor Destroy; override;
    procedure Clear(const C: TRGBA8);
    procedure Render(SL: TScanLine); overload;
    procedure Render(SL: TScanLine; const C: TRGBA8); overload;
    property Pixels[const X, Y: Integer]: TRGBA8 read GetPixel write SetPixel;
    property RBuf: TRenderingBuffer read FRBuf;
    property OnFillScanline: TFillScanLine read FOnFillScanline write
            FOnFillScanline;
    property SpanType: TSpanType read FSpanType write SetSpanType;
  end;
  
  TSpanRGB24 = class(TObject)
  public
    class function Get(Ptr: PByte; X: Integer): TRGBA8; virtual;
    class procedure HLine(Ptr: PByte; X: Integer; Count: LongWord; const C: TRGBA8); virtual;
    class procedure Render(Ptr: PByte; X: Integer; Count: LongWord; Covers:
            PByte; const C: TRGBA8); virtual;
  end;

  TSpanRGBA32 = class(TSpanRGB24)
  public
    class function Get(Ptr: PByte; X: Integer): TRGBA8; override;
    class procedure HLine(Ptr: PByte; X: Integer; Count: LongWord; const C:
            TRGBA8); override;
    class procedure Render(Ptr: PByte; X: Integer; Count: LongWord; Covers:
            PByte; const C: TRGBA8); override;
  end;


const

  PolyBaseShift = 8;
  PolyBaseSize = 1 shl PolyBaseShift;
  PolyBaseMask = PolyBaseSize - 1;

type

  // We don't use <object>s because it's not recommended
  // Instead, we use records and a set of functions to simulate C++ structure
  PPPCell = ^PPCell;
  PPCell = ^PCell;
  PCell = ^TCell;
  TCell = record
    X: Int16;
    Y: Int16;
    PackedCoord: Integer;
    Cover: Integer;
    Area: Integer;
  end;
  
  procedure SetCell(var Self: TCell; CX, CY, C, A: Integer);
  procedure SetCellCoord(var Self: TCell; CX, CY: Integer);
  procedure SetCellCover(var Self: TCell; C, A: Integer);
  procedure AddCellCover(var Self: TCell; C, A: Integer);

const

  NotClosed = 1;
  SortRequired = 2;

  CellBlockShift = 12;
  CellBlockSize  = 1 shl CellBlockShift;
  CellBlockMask  = CellBlockSize - 1;
  CellBlockPool  = 256;
  CellBlockLimit = 1024;


const

  AAShift   = 8;
  AANum     = 1 shl AAShift;
  AAMask    = AANum - 1;
  AA2Num    = AANum * 2;
  AA2Mask   = AA2Num - 1;


  function PolyCoord(C: Double): Integer;

implementation

function PolyCoord(C: Double): Integer;
begin
  Result := Trunc(C * PolyBaseSize);
end;

{ TRGBA8 }

function RGBA8: TRGBA8;
begin
end;

function RGBA8(R, G, B: LongWord; A: LongWord = 255): TRGBA8;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;

function RGBA8(APacked: LongWord; Order: TByteOrder): TRGBA8;
begin
  Result.A := 255;
  Result.G := (APacked shr 8) and $FF;
  case Order of
    boRgb:
      begin
        Result.R := (APacked shr 16) and $FF;
        Result.B := APacked and $FF;
      end;
    boBgr:
      begin
        Result.R := APacked and $FF;
        Result.B := (APacked shr 16) and $FF;
      end;
  end;
end;

function Gradient(var Self: TRGBA8; C: TRGBA8; K: Double): TRGBA8;
var
  IK: Integer;
begin
  IK := Trunc(K * 256);
  with Result do
  begin
    R := Int8U(Self.R + (((C.R - Self.R) * IK) shr 8));
    G := Int8U(Self.G + (((C.G - Self.G) * IK) shr 8));
    B := Int8U(Self.B + (((C.B - Self.B) * IK) shr 8));
    A := Int8U(Self.A + (((C.A - Self.A) * IK) shr 8));
  end;
end;

function Pre(var Self: TRGBA8): TRGBA8;
begin
  Result := RGBA8(
    (Self.R * Self.A) shr 8,
    (Self.G * Self.A) shr 8,
    (Self.B * Self.A) shr 8,
    Self.A
  );
end;

function Opacity(var Self: TRGBA8): Double;
begin
  Result := Self.A / 255.0;
end;

procedure Opacity(var Self: TRGBA8; A: Double);
begin
  if A < 0.0 then
    A := 0.0;
  if A > 1.0 then
    A := 1.0;
  Self.A := Trunc(A * 255.0);
end;

{ TRenderingBuffer }

{
******************************* TRenderingBuffer *******************************
}
constructor TRenderingBuffer.Create(Buf: PByte; Width, Height: LongWord;
        Stride: Integer);
begin
  inherited Create;
  
  Attach(Buf, Width, Height, Stride);
end;

destructor TRenderingBuffer.Destroy;
begin
  FreeMem(FRows);

  inherited Destroy;
end;

function TRenderingBuffer.AbsStride: LongWord;
begin
  Result := Abs(FStride);
end;

procedure TRenderingBuffer.Attach(Buf: PByte; Width, Height: LongWord; Stride:
        Integer);
var
  RowPtr: PByte;
  Rows: PPByte;
begin
  FBuf := Buf;
  FWidth := Width;
  FHeight := Height;
  FStride := Stride;
  if Height > FMaxHeight then
  begin
    FreeMem(FRows);
    GetMem(FRows, Height * SizeOf(PByte));
    FMaxHeight := Height;
  end;

  RowPtr := FBuf;
  
  if Stride < 0 then
    RowPtr := PByte(Integer(FBuf) - Integer(Height - 1) * Stride * SizeOf(Byte));
  
  Rows := FRows;
  
  while Height <> 0 do
  begin
    Rows^ := RowPtr;
    Inc(Rows);
    Inc(RowPtr, Stride);
  
    Dec(Height);
  end;
end;

function TRenderingBuffer.Inbox(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < Integer(FWidth)) and (Y < Integer(FHeight));
end;

function TRenderingBuffer.Row(Y: LongWord): PByte;
begin
  Result := PPByte(LongWord(FRows) + Y * SizeOf(PByte))^;
end;

{ TScanLine }

{
********************************** TScanLine ***********************************
}
constructor TScanLine.Create;
begin
  inherited Create;
  
  FLastX := $7FFF;
  FLastY := $7FFF;
end;

destructor TScanLine.Destroy;
begin
  FreeMem(FCounts);
  FreeMem(FStartPtrs);
  FreeMem(FCovers);

  inherited Destroy;
end;

procedure TScanLine.AddCell(X, Y: Integer; Cover: LongWord);
begin
  Dec(X, FMinX);
  PInt8U(Integer(FCovers) + X * SizeOf(Int8U))^ := Int8U(Cover);
  if X = FLastX + 1 then
    Inc(FCurCount^)
  else
  begin
    Inc(FCurCount);
    FCurCount^ := 1;
    Inc(FCurStartPtr);
    FCurStartPtr^ := PInt8U(Integer(FCovers) + X * SizeOf(Int8U));
    Inc(FNumSpans);
  end;
  FLastX := X;
  FLastY := Y;
end;

procedure TScanLine.AddSpan(X, Y: Integer; Len, Cover: LongWord);
begin
  Dec(X, FMinX);
  
  FillChar(PInt8U(Integer(FCovers) + X * SizeOf(Int8U))^, Len, Byte(Cover));
  if X = FLastX + 1 then
    Inc(FCurCount^, Int16U(Len))
  else
  begin
    Inc(FCurCount);
    FCurCount^ := Int16U(Len);
    Inc(FCurStartPtr);
    FCurStartPtr^ := PInt8U(Integer(FCovers) + X * SizeOf(Int8U));
    Inc(FNumSpans);
  end;
  FLastX := X + Integer(Len) - 1;
  FLastY := Y;
end;

function TScanLine.BaseX: Integer;
begin
  Result := FMinX + FDx;
end;

function TScanLine.IsReady(Y: Integer): Integer;
begin
  Result := Ord((FNumSpans <> 0) and ((Y xor FLastY) <> 0));
end;

procedure TScanLine.Reset(MinX, MaxX: Integer; Dx: Integer = 0; Dy: Integer =
        0);
var
  MaxLen: LongWord;
begin
  MaxLen := MaxX - MinX + 2;
  if MaxLen > FMaxLen then
  begin
    FreeMem(FCounts);
    FreeMem(FStartPtrs);
    FreeMem(FCovers);
    GetMem(FCovers, MaxLen * SizeOf(Int8U));
    GetMem(FStartPtrs, MaxLen * SizeOf(PInt8U));
    GetMem(FCounts, MaxLen * SizeOf(Int16U));
    FMaxLen := MaxLen;
  end;
  FDx           := Dx;
  FDy           := Dy;
  FLastX        := $7FFF;
  FLastY        := $7FFF;
  FMinX         := MinX;
  FCurCount     := FCounts;
  FCurStartPtr  := FStartPtrs;
  FNumSpans     := 0;
end;

procedure TScanLine.ResetSpans;
begin
  FLastX        := $7FFF;
  FLastY        := $7FFF;
  FCurCount     := FCounts;
  FCurStartPtr  := FStartPtrs;
  FNumSpans     := 0;
end;

function TScanLine.Y: Integer;
begin
  Result := FLastY + FDy;
end;

{ TRenderer }

{
********************************** TRenderer ***********************************
}
constructor TRenderer.Create(RBuf: TRenderingBuffer);
begin
  inherited Create;
  FSpan := nil;
  SpanType := stRGB24;
  FRBuf := RBuf;
end;

destructor TRenderer.Destroy;
begin
  FSpan.Free;
  
  inherited Destroy;
end;

procedure TRenderer.Clear(const C: TRGBA8);
var
  Y: LongWord;
begin
  for Y := 0 to FRBuf.Height - 1 do
    FSpan.HLine(FRBuf.Row(Y), 0, FRBuf.Width, C);
end;

function TRenderer.GetPixel(const X, Y: Integer): TRGBA8;
begin
  if FRBuf.Inbox(X, Y) then
    Result := FSpan.Get(FRBuf.Row(Y), X)
  else
    Result := RGBA8(0, 0, 0);
end;

procedure TRenderer.Render(SL: TScanLine);
var
  NumSpans: LongWord;
  BaseX: Integer;
  Span: TScanLineIter;
  X: Integer;
  Covers: PInt8U;
  NumPix: Integer;
begin
  if (SL.Y < 0) or (SL.Y >= Integer(FRBuf.Height)) or
     (not Assigned(FOnFillScanline)) then
    Exit;

  NumSpans := SL.NumSpans;
  BaseX := SL.BaseX;
  Span := TScanLineIter.Create(SL);

  try
    repeat
      Dec(NumSpans);

      X := Span.Next + BaseX;
      Covers := Span.Covers;
      NumPix := Span.NumPix;
      if X < 0 then
      begin
        Inc(NumPix, X);
        if NumPix <= 0 then
          Continue;
        Dec(Covers, X);
        X := 0;
      end;
      if X + NumPix >= Integer(FRBuf.Width) then
      begin
        NumPix := Integer(FRBuf.Width) - X;
        if NumPix <= 0 then
          Continue;
      end;
      FOnFillScanLine(X, SL.Y, NumPix, PByte(Covers));
    until NumSpans = 0;
  finally
    Span.Free;
  end;
end;

procedure TRenderer.Render(SL: TScanLine; const C: TRGBA8);
var
  NumSpans: LongWord;
  BaseX: Integer;
  Row: PByte;
  Span: TScanLineIter;
  X: Integer;
  Covers: PInt8U;
  NumPix: Integer;
begin
  if (SL.Y < 0) or (SL.Y >= Integer(FRBuf.Height)) then
    Exit;

  NumSpans := SL.NumSpans;
  BaseX := SL.BaseX;
  Row := FRBuf.Row(SL.Y);
  Span := TScanLineIter.Create(SL);
  
  try
    repeat
      Dec(NumSpans);
  
      X := Span.Next + BaseX;
      Covers := Span.Covers;
      NumPix := Span.NumPix;
      if X < 0 then
      begin
        Inc(NumPix, X);
        if NumPix <= 0 then
          Continue;
        Dec(Covers, X);
        X := 0;
      end;
      if X + NumPix >= Integer(FRBuf.Width) then
      begin
        NumPix := Integer(FRBuf.Width) - X;
        if NumPix <= 0 then
          Continue;
      end;
      FSpan.Render(Row, X, NumPix, PByte(Covers), C);
    until NumSpans = 0;
  finally
    Span.Free;
  end;
end;

procedure TRenderer.SetPixel(const X, Y: Integer; const Value: TRGBA8);
begin
  if FRBuf.Inbox(X, Y) then
    FSpan.HLine(FRBuf.Row(Y), X, 1, Value);
end;

{ TScanLineIter }

{
******************************** TScanLineIter *********************************
}
constructor TScanLineIter.Create(SL: TScanLine);
begin
  inherited Create;
  
  FCovers := SL.FCovers;
  FCurCount := SL.FCounts;
  FCurStartPtr := SL.FStartPtrs;
end;

function TScanLineIter.Covers: PInt8U;
begin
  Result := FCurStartPtr^;
end;

function TScanLineIter.Next: Integer;
begin
  Inc(FCurCount);
  Inc(FCurStartPtr);
  Result := (Integer(FCurStartPtr^) - Integer(FCovers)) div SizeOf(Int8U);
end;

function TScanLineIter.NumPix: Integer;
begin
  Result := FCurCount^;
end;

{ TCell }

procedure SetCell(var Self: TCell; CX, CY, C, A: Integer);
begin
  with Self do
  begin
    X := Int16(CX);
    Y := Int16(CY);
    PackedCoord := (CY shl 16) + CX;
    Cover := C;
    Area := A;
  end;
end;

procedure SetCellCoord(var Self: TCell; CX, CY: Integer);
begin
  with Self do
  begin
    X := Int16(CX);
    Y := Int16(CY);
    PackedCoord := (CY shl 16) + CX;
  end;
end;

procedure SetCellCover(var Self: TCell; C, A: Integer);
begin
  with Self do
  begin
    Cover := C;
    Area := A;
  end;
end;

procedure AddCellCover(var Self: TCell; C, A: Integer);
begin
  with Self do
  begin
    Inc(Cover, C);
    Inc(Area, A);
  end;
end;


{ TSpanRGB24 }

{
********************************** TSpanRGB24 **********************************
}
class function TSpanRGB24.Get(Ptr: PByte; X: Integer): TRGBA8;
var
  P: PByte;
begin
  P := PByte(Integer(Ptr) + (X + X + X) * SizeOf(Byte));
  with Result do
  begin
    B := P^;
    Inc(P);
    G := P^;
    Inc(P);
    R := P^;
    //Inc(P);
    A := 255;
  end;
end;

class procedure TSpanRGB24.HLine(Ptr: PByte; X: Integer; Count: LongWord; const C:
        TRGBA8);
var
  P: PByte;
begin
  P := PByte(Integer(Ptr) + (X + X + X) * SizeOf(Byte));
  repeat
    P^ := C.B;
    Inc(P);
    P^ := C.G;
    Inc(P);
    P^ := C.R;
    Inc(P);
  
    Dec(Count);
  until Count = 0;
end;

class procedure TSpanRGB24.Render(Ptr: PByte; X: Integer; Count: LongWord;
        Covers: PByte; const C: TRGBA8);
var
  P: PByte;
  R, G, B, Alpha: Integer;
begin
  P := PByte(Integer(Ptr) + (X + X + X) * SizeOf(Byte));
  repeat
    Alpha := Covers^ * C.A;
    Inc(Covers);
    B := PByte(Integer(P) + 0 * SizeOf(Byte))^;
    G := PByte(Integer(P) + 1 * SizeOf(Byte))^;
    R := PByte(Integer(P) + 2 * SizeOf(Byte))^;
    P^ := (((C.B - B) * Alpha) + (B shl 16)) shr 16;
    Inc(P);
    P^ := (((C.G - G) * Alpha) + (G shl 16)) shr 16;
    Inc(P);
    P^ := (((C.R - R) * Alpha) + (R shl 16)) shr 16;
    Inc(P);

    Dec(Count);
  until Count = 0;
end;


{ TSpanRGBA32 }

{
********************************* TSpanRGBA32 **********************************
}
class function TSpanRGBA32.Get(Ptr: PByte; X: Integer): TRGBA8;
var
  P: PByte;
begin
  P := PByte(Integer(Ptr) + (X shl 2) * SizeOf(Byte));
  with Result do
  begin
    R := P^;
    Inc(P);
    G := P^;
    Inc(P);
    B := P^;
    Inc(P);
    A := P^;
  end;
end;

class procedure TSpanRGBA32.HLine(Ptr: PByte; X: Integer; Count: LongWord;
        const C: TRGBA8);
var
  P: PByte;
begin
  P := PByte(Integer(Ptr) + (X shl 2) * SizeOf(Byte));
  repeat
    P^ := C.R;
    Inc(P);
    P^ := C.G;
    Inc(P);
    P^ := C.B;
    Inc(P);
    P^ := C.A;
    Inc(P);
  
    Dec(Count);
  until Count = 0;
end;

class procedure TSpanRGBA32.Render(Ptr: PByte; X: Integer; Count: LongWord;
        Covers: PByte; const C: TRGBA8);
var
  P: PByte;
  Alpha, R, G, B, A: Integer;
begin
  P := PByte(Integer(Ptr) + (X shl 2) * SizeOf(Byte));
  repeat
    Alpha := Covers^ * C.A;
    Inc(Covers);
    R := PByte(Integer(P) + 0 * SizeOf(Byte))^;
    G := PByte(Integer(P) + 1 * SizeOf(Byte))^;
    B := PByte(Integer(P) + 2 * SizeOf(Byte))^;
    A := PByte(Integer(P) + 3 * SizeOf(Byte))^;
    P^ := (((C.R - R) * Alpha) + (R shl 16)) shr 16;
    Inc(P);
    P^ := (((C.G - G) * Alpha) + (G shl 16)) shr 16;
    Inc(P);
    P^ := (((C.B - B) * Alpha) + (B shl 16)) shr 16;
    Inc(P);
    P^ := (((C.A - A) * Alpha) + (A shl 16)) shr 16;
    Inc(P);
  
    Dec(Count);
  until Count = 0;
end;


procedure TRenderer.SetSpanType(const Value: TSpanType);
begin
  FSpanType := Value;
  if assigned(FSpan) then
    FreeAndNil(FSpan);
  case FSpanType of
    stRGB24:  FSpan := TSpanRGB24.Create;
    stRGBA32: FSpan := TSpanRGBA32.Create;
  end;
end;


end.
