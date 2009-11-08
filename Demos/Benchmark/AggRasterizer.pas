{
Anti-Grain Geometry - Version 2.0
Copyright (C) 2002-2004 Maxim Shemanarev (McSeem)

Permission to copy, use, modify, sell and distribute this software 
is granted provided this copyright notice appears in all copies. 
This software is provided "as is" without express or implied
warranty, and with no claim as to its suitability for any purpose.
}
unit AggRasterizer;

interface

uses Agg, Math;

type
  TVertexSource = class(TObject)
  public
    function AddVertex(var x, y: integer): Integer; virtual; abstract;
    procedure Rewind(PathId: integer); virtual; abstract;
  end;

  TOutLine = class(TObject)
  private
    FCells: PPCell;
    FCloseX: Integer;
    FCloseY: Integer;
    FCurBlock: LongWord;
    FCurCell: TCell;
    FCurCellPtr: PCell;
    FCurX: Integer;
    FCurY: Integer;
    FFlags: LongWord;
    FMaxBlocks: LongWord;
    FMaxX: Integer;
    FMaxY: Integer;
    FMinX: Integer;
    FMinY: Integer;
    FNumBlocks: LongWord;
    FNumCells: LongWord;
    FSortedCells: PPCell;
    FSortedSize: LongWord;
    procedure AddCurCell;
    procedure AllocateBlock;
    function GetCells: PPCell;
    class procedure QSortCells(Start: PPCell; Num: LongWord);
    procedure RenderLine(X1, Y1, X2, Y2: Integer);
    procedure RenderScanLine(EY, X1, Y1, X2, Y2: Integer);
    procedure SetCurCell(X, Y: Integer);
    procedure SortCells;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LineTo(X, Y: Integer);
    procedure MoveTo(X, Y: Integer);
    procedure Reset;
    property Cells: PPCell read GetCells;
    property MaxX: Integer read FMaxX;
    property MaxY: Integer read FMaxY;
    property MinX: Integer read FMinX;
    property MinY: Integer read FMinY;
    property NumCells: LongWord read FNumCells;
  end;

  TRasterizer = class(TObject)
  private
    FFillingRule: TFillingRule;
    FGamma: array[0..255] of Int8U;
    FOutLine: TOutLine;
    FScanLine: TScanLine;
    function GetMaxX: Integer;
    function GetMaxY: Integer;
    function GetMinX: Integer;
    function GetMinY: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPath(VS: TVertexSource; PathID: integer);
    function CalculateAlpha(Area: Integer): LongWord;
    procedure Gamma(G: Double); overload;
    procedure Gamma(G: PInt8U); overload;
    function HitTest(Tx, Ty: Integer): Boolean;
    procedure LineTo(X, Y: Integer);
    procedure LineToD(X, Y: Double);
    procedure MoveTo(X, Y: Integer);
    procedure MoveToD(X, Y: Double);
    procedure Render(R: TRenderer; Dx: Integer = 0; Dy: Integer = 0); overload;
    procedure Render(R: TRenderer; const C: TRGBA8; Dx: Integer = 0; Dy:
            Integer = 0); overload;
    procedure Reset;
    property FillingRule: TFillingRule read FFillingRule write FFillingRule;
    property MaxX: Integer read GetMaxX;
    property MaxY: Integer read GetMaxY;
    property MinX: Integer read GetMinX;
    property MinY: Integer read GetMinY;
  end;



implementation

procedure SwapCells(A, B: PPCell);
var
  Temp: PCell;
begin
  Temp := A^;
  A^ := B^;
  B^ := Temp;
end;

function LessThan(A, B: PPCell): Boolean;
begin
  Result := A^.PackedCoord < B^.PackedCoord;
end;


{
********************************* TRasterizer **********************************
}
{ TRasterizer }

const
  DefaultGamma: array[0..255] of Int8U = (
    0,  0,  1,  1,  2,  2,  3,  4,  4,  5,  5,  6,  7,  7,  8,  8,
    9, 10, 10, 11, 11, 12, 13, 13, 14, 14, 15, 16, 16, 17, 18, 18,
    19, 19, 20, 21, 21, 22, 22, 23, 24, 24, 25, 25, 26, 27, 27, 28,
    29, 29, 30, 30, 31, 32, 32, 33, 34, 34, 35, 36, 36, 37, 37, 38,
    39, 39, 40, 41, 41, 42, 43, 43, 44, 45, 45, 46, 47, 47, 48, 49,
    49, 50, 51, 51, 52, 53, 53, 54, 55, 55, 56, 57, 57, 58, 59, 60,
    60, 61, 62, 62, 63, 64, 65, 65, 66, 67, 68, 68, 69, 70, 71, 71,
    72, 73, 74, 74, 75, 76, 77, 78, 78, 79, 80, 81, 82, 83, 83, 84,
    85, 86, 87, 88, 89, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
    100,101,101,102,103,104,105,106,107,108,109,110,111,112,114,115,
    116,117,118,119,120,121,122,123,124,126,127,128,129,130,131,132,
    134,135,136,137,139,140,141,142,144,145,146,147,149,150,151,153,
    154,155,157,158,159,161,162,164,165,166,168,169,171,172,174,175,
    177,178,180,181,183,184,186,188,189,191,192,194,195,197,199,200,
    202,204,205,207,209,210,212,214,215,217,219,220,222,224,225,227,
    229,230,232,234,236,237,239,241,242,244,246,248,249,251,253,255
  );

constructor TRasterizer.Create;
begin
  inherited Create;

  Move(DefaultGamma, FGamma, SizeOf(FGamma));
  FOutLine := TOutLine.Create;
  FScanLine := TScanLine.Create;
end;

destructor TRasterizer.Destroy;
begin
  FScanLine.Free;
  FOutLine.Free;
  inherited;
end;

procedure TRasterizer.AddPath(VS: TVertexSource; PathID: integer);
begin
  // TODO -cMM: TRasterizer.AddPath default body inserted
end;

function TRasterizer.CalculateAlpha(Area: Integer): LongWord;
var
  Cover: Integer;
begin
  //Cover := Area shr (PolyBaseShift * 2 + 1 - AAShift);
  asm
    MOV     EAX, Area
    SAR     EAX, (PolyBaseShift * 2 + 1 - AAShift)
    MOV     Cover, EAX
  end;
  if Cover < 0 then
    Cover := -Cover;
  if FFillingRule = frFillEvenOdd then
  begin
    Cover := Cover and AA2Mask;
    if Cover > AANum then
      Cover := AA2Num - Cover;
  end;
  if Cover > AAMask then
    Cover := AAMask;
  Result := Cover;
end;

procedure TRasterizer.Gamma(G: Double);
var
  I: LongWord;
begin
  for I := 0 to 255 do
    FGamma[I] := Trunc(Power(I / 255, G) * 255.0);
end;

procedure TRasterizer.Gamma(G: PInt8U);
begin
  Move(G^, FGamma, SizeOf(FGamma));
end;

function TRasterizer.GetMaxX: Integer;
begin
  Result := FOutLine.MaxX;
end;

function TRasterizer.GetMaxY: Integer;
begin
  Result := FOutLine.MaxY;
end;

function TRasterizer.GetMinX: Integer;
begin
  Result := FOutLine.MinX;
end;

function TRasterizer.GetMinY: Integer;
begin
  Result := FOutLine.MinY;
end;

function TRasterizer.HitTest(Tx, Ty: Integer): Boolean;
var
  Cells: PPCell;
  CurCell, StartCell: PCell;
  X, Y, Cover, Alpha, Area, Coord: Integer;
begin
  Cells := FOutLine.Cells;
  if FOutLine.NumCells = 0 then
  begin
    Result := False;
    Exit;
  end;
  
  Cover := 0;
  CurCell := Cells^;
  Inc(Cells);
  while True do
  begin
    StartCell := CurCell;

    Coord := CurCell^.PackedCoord;
    X := CurCell^.X;
    Y := CurCell^.Y;

    if Y > Ty then
    begin
      Result := False;
      Exit;
    end;
  
    Area := StartCell^.Area;
    Inc(Cover, StartCell^.Cover);
  
    CurCell := Cells^;
    Inc(Cells);
    while Assigned(CurCell) do
    begin
      if CurCell^.PackedCoord <> Coord then
        Break;
      Inc(Area, CurCell^.Area);
      Inc(Cover, CurCell^.Cover);
  
      CurCell := Cells^;
      Inc(Cells);
    end;
  
    if Area <> 0 then
    begin
      Alpha := CalculateAlpha((Cover shl (PolyBaseShift + 1)) - Area);
      if (Alpha <> 0) and (Tx = X) and (Ty = Y) then
      begin
        Result := True;
        Exit;
      end;
      Inc(X);
    end;
  
    if not Assigned(CurCell) then
      Break;
  
    if CurCell^.X > X then
    begin
      Alpha := CalculateAlpha(Cover shl (PolyBaseShift + 1));
      if (Alpha <> 0) and (Ty = Y) and (Tx >= X) and (Tx <= CurCell^.X) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  Result := False;
end;

procedure TRasterizer.LineTo(X, Y: Integer);
begin
  FOutLine.LineTo(X, Y);
end;

procedure TRasterizer.LineToD(X, Y: Double);
begin
  FOutLine.LineTo(PolyCoord(X), PolyCoord(Y));
end;

procedure TRasterizer.MoveTo(X, Y: Integer);
begin
  FOutLine.MoveTo(X, Y);
end;

procedure TRasterizer.MoveToD(X, Y: Double);
begin
  FOutLine.MoveTo(PolyCoord(X), PolyCoord(Y));
end;

procedure TRasterizer.Render(R: TRenderer; Dx: Integer = 0; Dy: Integer = 0);
var
  Cells: PPCell;
  X, Y, Cover, Alpha, Area, Coord: Integer;
  CurCell, StartCell: PCell;
begin
  Cells := FOutLine.Cells;
  if FOutLine.NumCells = 0 then
    Exit;
  
  FScanLine.Reset(FOutLine.MinX, FOutLine.MaxX, Dx, Dy);

  Cover := 0;
  CurCell := Cells^;
  Inc(Cells);
  while True do
  begin
    StartCell := CurCell;
  
    Coord := CurCell^.PackedCoord;
    X := CurCell^.X;
    Y := CurCell^.Y;

    Area := StartCell^.Area;
    Inc(Cover, StartCell^.Cover);
  
    CurCell := Cells^;
    Inc(Cells);
    while Assigned(CurCell) do
    begin
      if CurCell^.PackedCoord <> Coord then
        Break;
      Inc(Area, CurCell^.Area);
      Inc(Cover, CurCell^.Cover);

      CurCell := Cells^;
      Inc(Cells);
    end;

    if Area <> 0 then
    begin
      Alpha := CalculateAlpha((Cover shl (PolyBaseShift + 1)) - Area);
      if Alpha <> 0 then
      begin
        if FScanLine.IsReady(Y) <> 0 then
        begin
          R.Render(FScanLine);
          FScanLine.ResetSpans;
        end;
        FScanLine.AddCell(X, Y, FGamma[Alpha]);
      end;
      Inc(X);
    end;
  
    if not Assigned(CurCell) then
      Break;
  
    if CurCell^.X > X then
    begin
      Alpha := CalculateAlpha(Cover shl (PolyBaseShift + 1));
      if Alpha <> 0 then
      begin
        if FScanLine.IsReady(Y) <> 0 then
        begin
          R.Render(FScanLine);
          FScanLine.ResetSpans;
        end;
        FScanLine.AddSpan(X, Y, CurCell^.X - X, FGamma[Alpha]);
      end;
    end;
  end;
  
  if FScanLine.NumSpans <> 0 then
    R.Render(FScanLine);
end;

procedure TRasterizer.Render(R: TRenderer; const C: TRGBA8; Dx: Integer = 0;
        Dy: Integer = 0);
var
  Cells: PPCell;
  X, Y, Cover, Alpha, Area, Coord: Integer;
  CurCell, StartCell: PCell;
begin
  Cells := FOutLine.Cells;
  if FOutLine.NumCells = 0 then
    Exit;

  FScanLine.Reset(FOutLine.MinX, FOutLine.MaxX, Dx, Dy);
  
  Cover := 0;
  CurCell := Cells^;
  Inc(Cells);
  while True do
  begin
    StartCell := CurCell;
  
    Coord := CurCell^.PackedCoord;
    X := CurCell^.X;
    Y := CurCell^.Y;
  
    Area := StartCell^.Area;
    Inc(Cover, StartCell^.Cover);
  
    CurCell := Cells^;
    Inc(Cells);
    while Assigned(CurCell) do
    begin
      if CurCell^.PackedCoord <> Coord then
        Break;
      Inc(Area, CurCell^.Area);
      Inc(Cover, CurCell^.Cover);
  
      CurCell := Cells^;
      Inc(Cells);
    end;
  
    if Area <> 0 then
    begin
      Alpha := CalculateAlpha((Cover shl (PolyBaseShift + 1)) - Area);
      if Alpha <> 0 then
      begin
        if FScanLine.IsReady(Y) <> 0 then
        begin
          R.Render(FScanLine, C);
          FScanLine.ResetSpans;
        end;
        FScanLine.AddCell(X, Y, FGamma[Alpha]);
      end;
      Inc(X);
    end;

    if not Assigned(CurCell) then
      Break;
  
    if CurCell^.X > X then
    begin
      Alpha := CalculateAlpha(Cover shl (PolyBaseShift + 1));
      if Alpha <> 0 then
      begin
        if FScanLine.IsReady(Y) <> 0 then
        begin
          R.Render(FScanLine, C);
          FScanLine.ResetSpans;
        end;
        FScanLine.AddSpan(X, Y, CurCell^.X - X, FGamma[Alpha]);
      end;
    end;
  end;
  
  if FScanLine.NumSpans <> 0 then
    R.Render(FScanLine, C);
end;

procedure TRasterizer.Reset;
begin
  FOutLine.Reset;
end;

{ TOutLine }

{
*********************************** TOutLine ***********************************
}
constructor TOutLine.Create;
begin
  inherited Create;

  FMinX := $7FFFFFFF;
  FMinY := $7FFFFFFF;
  FMaxX := -$7FFFFFFF;
  FMaxY := -$7FFFFFFF;
  FFlags := SortRequired;

  SetCell(FCurCell, $7FFF, $7FFF, 0, 0);
end;

destructor TOutLine.Destroy;
var
  Ptr: PPCell;
begin
  FreeMem(FSortedCells);
  if FNumBlocks <> 0 then
  begin
    Ptr := PPCell(LongWord(FCells) + (FNumBlocks - 1) * SizeOf(PCell));
    while FNumBlocks <> 0 do
    begin
      FreeMem(Ptr^);
      Dec(Ptr);
  
      Dec(FNumBlocks);
    end;
    FreeMem(FCells);
  end;
  
  inherited Destroy;
end;

procedure TOutLine.AddCurCell;
begin
  if FCurCell.Area or FCurCell.Cover <> 0 then
  begin
    if FNumCells and CellBlockMask = 0 then
    begin
      if FNumBlocks >= CellBlockLimit then
        Exit;
      AllocateBlock;
    end;
    FCurCellPtr^ := FCurCell;
    Inc(FCurCellPtr);
    Inc(FNumCells);
  end;
end;

procedure TOutLine.AllocateBlock;
var
  NewCells: PPCell;
begin
  if FCurBlock >= FNumBlocks then
  begin
    if FNumBlocks >= FMaxBlocks then
    begin
      GetMem(NewCells, (FMaxBlocks + CellBlockPool) * SizeOf(PCell));
      if Assigned(FCells) then
      begin
        Move(FCells^, NewCells^, FMaxBlocks * SizeOf(PCell));
        FreeMem(FCells);
      end;
      FCells := NewCells;
      Inc(FMaxBlocks, CellBlockPool);
    end;
    GetMem(PPCell(LongWord(FCells) + FNumBlocks * SizeOf(PCell))^,
      LongWord(CellBlockSize) * SizeOf(TCell));
    Inc(FNumBlocks);
  end;
  FCurCellPtr := PPCell(LongWord(FCells) + FCurBlock * SizeOf(PCell))^;
  Inc(FCurBlock);
end;

function TOutLine.GetCells: PPCell;
begin
  if FFlags and NotClosed <> 0 then
  begin
    LineTo(FCloseX, FCloseY);
    FFlags := FFlags and not NotClosed;
  end;
  
  //Perform sort only the first time.
  if FFlags and SortRequired <> 0 then
  begin
    AddCurCell;
    if FNumCells = 0 then
    begin
      Result := nil;
      Exit;
    end;
    SortCells;
    FFlags := FFlags and not SortRequired;
  end;
  Result := FSortedCells;
end;

procedure TOutLine.LineTo(X, Y: Integer);
var
  C: Integer;
begin
  if (FFlags and SortRequired <> 0) and (((FCurX xor X) or (FCurY xor Y)) <> 0) then
  begin
    //C := FCurX shr PolyBaseShift;
    asm
      MOV   EAX, Self
      MOV   EAX, [EAX].TOutLine.FCurX
      SAR   EAX, PolyBaseShift
      MOV   C, EAX
    end;
    if C < FMinX then FMinX := C;
    Inc(C);
    if C > FMaxX then FMaxX := C;

    //C := X shr PolyBaseShift;
    asm
      MOV   EAX, X
      SAR   EAX, PolyBaseShift
      MOV   C, EAX
    end;
    if C < FMinX then FMinX := C;
    Inc(C);
    if C > FMaxX then FMaxX := C;

    RenderLine(FCurX, FCurY, X, Y);
    FCurX := X;
    FCurY := Y;
    FFlags := FFlags or NotClosed;
  end;
end;

procedure TOutLine.MoveTo(X, Y: Integer);
begin
  if FFlags and SortRequired = 0 then
    Reset;
  if FFlags and NotClosed <> 0 then
    LineTo(FCloseX, FCloseY);
  //SetCurCell(X shr PolyBaseShift, Y shr PolyBaseShift);
  asm
    MOV       ECX, Y
    SAR       ECX, PolyBaseShift
    MOV       EDX, X
    SAR       EDX, PolyBaseShift
    MOV       EAX, EBX
    CALL      TOutLine.SetCurCell
  end;
  FCurX   := X;
  FCloseX := X;
  FCurY   := Y;
  FCloseY := Y;
end;

class procedure TOutLine.QSortCells(Start: PPCell; Num: LongWord);
  
  const
    QSortThreshold = 9;
  var
    Stack: array[0..79] of PPCell;
    Top: ^PPCell;
    Limit, Base, I, J, Pivot: PPCell;
    Len: Integer;
  
begin
  Limit := PPCell(LongWord(Start) + Num * SizeOf(PCell));
  Base  := Start;
  Top   := @Stack[0];
  
  while True do
  begin
    Len := (Integer(Limit) - Integer(Base)) div SizeOf(PCell);

    if Len > QSortThreshold then
    begin
      // we use Base + (Len div 2) as the pivot
      Pivot := PPCell(Integer(Base) + (Len div 2) * SizeOf(PCell));
      SwapCells(Base, Pivot);
  
      I := PPCell(Integer(Base) + SizeOf(PCell));
      J := PPCell(Integer(Limit) - SizeOf(PCell));
  
      // now ensure that I^ <= Base^ <= J^
      if LessThan(J, I) then
        SwapCells(I, J);
  
      if LessThan(Base, I) then
        SwapCells(Base, I);
  
      if LessThan(J, Base) then
        SwapCells(Base, J);
  
      while True do
      begin
        repeat Inc(I) until not LessThan(I, Base);
        repeat Dec(J) until not LessThan(Base, J);

        if Integer(I) > Integer(J) then
          Break;
  
        SwapCells(I, J);
      end;
  
      SwapCells(Base, J);
  
      // now, push the largest sub-array
      if Integer(J) - Integer(Base) > Integer(Limit) - Integer(I) then
      begin
        PPPCell(Integer(Top) + 0 * SizeOf(PPCell))^ := Base;
        PPPCell(Integer(Top) + 1 * SizeOf(PPCell))^ := J;
        Base := I;
      end
      else
      begin
        PPPCell(Integer(Top) + 0 * SizeOf(PPCell))^ := I;
        PPPCell(Integer(Top) + 1 * SizeOf(PPCell))^ := Limit;
        Limit := J;
      end;
      Inc(Top, 2);
    end
    else
    begin
      // the sub-array is small, perform insertion sort
      J := Base;
      I := PPCell(Integer(J) + SizeOf(PCell));
  
      while Integer(I) < Integer(Limit) do
      begin
        while LessThan(PPCell(Integer(J) + SizeOf(PCell)), J) do
        begin
          SwapCells(PPCell(Integer(J) + SizeOf(PCell)), J);
          if J = Base then
            Break;
          Dec(J);
        end;
        J := I;
        Inc(I);
      end;
  
      if Integer(Top) > Integer(@Stack[0]) then
      begin
        Dec(Top, 2);
        Base := PPPCell(Integer(Top) + 0 * SizeOf(PPCell))^;
        Limit := PPPCell(Integer(Top) + 1 * SizeOf(PPCell))^;
      end
      else
        Break;
    end;
  end;
end;

procedure TOutLine.RenderLine(X1, Y1, X2, Y2: Integer);
var
  EY1, EY2, FY1, FY2, Dx, Dy, XFrom, XTo, P, Rem, AMod, Lift, Delta, First, Incr, EX, TwoFx, Area: Integer;
begin
  //EY1 := Y1 shr PolyBaseShift;
  asm
    MOV       EAX, Y1
    SAR       EAX, PolyBaseShift
    MOV       EY1, EAX
  end;
  //EY2 := Y2 shr PolyBaseShift;
  asm
    MOV       EAX, Y2
    SAR       EAX, PolyBaseShift
    MOV       EY2, EAX
  end;
  FY1 := Y1 and PolyBaseMask;
  FY2 := Y2 and PolyBaseMask;
  
  if EY1      < FMinY then    FMinY := EY1;
  if EY1 + 1  > FMaxY then    FMaxY := EY1 + 1;
  if EY2      < FMinY then    FMinY := EY2;
  if EY2 + 1  > FMaxY then    FMaxY := EY2 + 1;
  
  Dx := X2 - X1;
  Dy := Y2 - Y1;
  
  //everything is on a single scanline
  if EY1 = EY2 then
  begin
    RenderScanLine(EY1, X1, FY1, X2, FY2);
    Exit;
  end;
  
  //Vertical line - we have to calculate start and end cells,
  //and then - the common values of the area and coverage for
  //all cells of the line. We know exactly there's only one
  //cell, so, we don't have to call RenderScanline().
  Incr := 1;
  if Dx = 0 then
  begin
    //EX := X1 shr PolyBaseShift;
    asm
      MOV       EAX, X1
      SAR       EAX, PolyBaseShift
      MOV       EX, EAX
    end;
    TwoFx := (X1 - (EX shl PolyBaseShift)) shl 1;
  
    First := PolyBaseSize;
    if Dy < 0 then
    begin
      First := 0;
      Incr  := -1;
    end;
  
    //XFrom := X1;

    //RenderScanline(EY1, XFrom, FY1, XFrom, First);
    Delta := First - FY1;
    AddCellCover(FCurCell, Delta, TwoFx * Delta);

    Inc(EY1, Incr);
    SetCurCell(EX, EY1);
  
    Delta := First + First - PolyBaseSize;
    Area := TwoFx * Delta;
    while EY1 <> EY2 do
    begin
      //RenderScanline(EY1, XFrom, PolyBaseSize - First, XFrom, First);
      SetCellCover(FCurCell, Delta, Area);
      Inc(EY1, Incr);
      SetCurCell(EX, EY1);
    end;
    //RenderScanline(EY1, XFrom, PolyBaseSize - First, XFrom, FY2);
    Delta := FY2 - PolyBaseSize + First;
    AddCellCover(FCurCell, Delta, TwoFx * Delta);
    Exit;
  end;
  
  //ok, we have to render several scanlines
  P     := (PolyBaseSize - FY1) * Dx;
  First := PolyBaseSize;

  if Dy < 0 then
  begin
    P     := FY1 * Dx;
    First := 0;
    Incr  := -1;
    Dy    := -Dy;
  end;
  
  Delta := P div Dy;
  AMod  := P mod Dy;
  
  if AMod < 0 then
  begin
    Dec(Delta);
    Inc(AMod, Dy);
  end;
  
  XFrom := X1 + Delta;
  RenderScanLine(EY1, X1, FY1, XFrom, First);
  
  Inc(EY1, Incr);
  //SetCurCell(XFrom shr PolyBaseShift, EY1);
  asm
    MOV       EDX, XFrom
    SAR       EDX, PolyBaseShift
    MOV       ECX, EY1
    MOV       EAX, EBX
    CALL      TOutLine.SetCurCell
  end;
  
  if EY1 <> EY2 then
  begin
    P     := PolyBaseSize * Dx;
    Lift  := P div Dy;
    Rem   := P mod Dy;
  
    if Rem < 0 then
    begin
      Dec(Lift);
      Inc(Rem, Dy);
    end;
    Dec(AMod, Dy);
  
    while EY1 <> EY2 do
    begin
      Delta := Lift;
      Inc(AMod, Rem);
      if AMod >= 0 then
      begin
        Dec(AMod, Dy);
        Inc(Delta);
      end;
  
      XTo := XFrom + Delta;
      RenderScanLine(EY1, XFrom, PolyBaseSize - First, XTo, First);
      XFrom := XTo;
  
      Inc(EY1, Incr);
      //SetCurCell(XFrom shr PolyBaseShift, EY1);
      asm
        MOV     EDX, XFrom
        SAR     EDX, PolyBaseShift
        MOV     ECX, EY1
        MOV     EAX, EBX
        CALL    TOutLine.SetCurCell
      end;
    end;
  end;

  RenderScanLine(EY1, XFrom, PolyBaseSize - First, X2, FY2);
end;

procedure TOutLine.RenderScanLine(EY, X1, Y1, X2, Y2: Integer);
var
  EX1, EX2, FX1, FX2, Delta, P, First, Dx, Incr, Lift, AMod, Rem: Integer;
begin
  //EX1 := X1 shr PolyBaseShift;
  asm
    MOV       EAX, X1
    SAR       EAX, PolyBaseShift
    MOV       EX1, EAX
  end;
  //EX2 := X2 shr PolyBaseShift;
  asm
    MOV       EAX, X2
    SAR       EAX, PolyBaseShift
    MOV       EX2, EAX
  end;
  FX1 := X1 and PolyBaseMask;
  FX2 := X2 and PolyBaseMask;

  //trivial case. Happens often
  if Y1 = Y2 then
  begin
    SetCurCell(EX2, EY);
    Exit;
  end;
  
  //everything is located in a single cell.  That is easy!
  if EX1 = EX2 then
  begin
    Delta := Y2 - Y1;
    AddCellCover(FCurCell, Delta, (FX1 + FX2) * Delta);
    Exit;
  end;
  
  //ok, we'll have to render a run of adjacent cells on the same
  //scanline...
  P     := (PolyBaseSize - FX1) * (Y2 - Y1);
  First := PolyBaseSize;
  Incr  := 1;
  
  Dx := X2 - X1;

  if Dx < 0 then
  begin
    P     := FX1 * (Y2 - Y1);
    First := 0;
    Incr  := -1;
    Dx    := -Dx;
  end;
  
  Delta := P div Dx;
  AMod  := P mod Dx;
  
  if AMod < 0 then
  begin
    Dec(Delta);
    Inc(AMod, Dx);
  end;
  
  AddCellCover(FCurCell, Delta, (FX1 + First) * Delta);
  
  Inc(EX1, Incr);
  SetCurCell(EX1, EY);
  Inc(Y1, Delta);

  if EX1 <> EX2 then
  begin
    P     := PolyBaseSize * (Y2 - Y1 + Delta);
    Lift  := P div Dx;
    Rem   := P mod Dx;
  
    if Rem < 0 then
    begin
      Dec(Lift);
      Inc(Rem, Dx);
    end;
  
    Dec(AMod, Dx);
  
    while EX1 <> EX2 do
    begin
      Delta := Lift;
      Inc(AMod, Rem);
      if AMod >= 0 then
      begin
        Dec(AMod, Dx);
        Inc(Delta);
      end;

      AddCellCover(FCurCell, Delta, PolyBaseSize * Delta);
      Inc(Y1, Delta);
      Inc(EX1, Incr);
      SetCurCell(EX1, EY);
    end;
  end;
  
  Delta := Y2 - Y1;
  AddCellCover(FCurCell, Delta, (FX2 + PolyBaseSize - First) * Delta);
end;

procedure TOutLine.Reset;
begin
  FNumCells := 0;
  FCurBlock := 0;
  SetCell(FCurCell, $7FFF, $7FFF, 0, 0);
  FFlags := FFlags or SortRequired;
  FFlags := FFlags and not NotClosed;
  FMinX := $7FFFFFFF;
  FMinY := $7FFFFFFF;
  FMaxX := -$7FFFFFFF;
  FMaxY := -$7FFFFFFF;
end;

procedure TOutLine.SetCurCell(X, Y: Integer);
begin
  if FCurCell.PackedCoord <> (Y shl 16) + X then
  begin
    AddCurCell;
    SetCell(FCurCell, X, Y, 0, 0);
  end;
end;

procedure TOutLine.SortCells;
var
  SortedPtr, BlockPtr: PPCell;
  CellPtr: PCell;
  NB, I: LongWord;
begin
  if FNumCells = 0 then
    Exit;
  
  if FNumCells > FSortedSize then
  begin
    FreeMem(FSortedCells);
    FSortedSize := FNumCells;
    GetMem(FSortedCells, (FNumCells + 1) * SizeOf(PCell));
  end;
  
  SortedPtr := FSortedCells;
  BlockPtr := FCells;
  
  NB := FNumCells shr CellBlockShift;
  
  while NB <> 0 do
  begin
    Dec(NB);
  
    CellPtr := BlockPtr^;
    Inc(BlockPtr);
    I := CellBlockSize;
    while I <> 0 do
    begin
      Dec(I);
  
      SortedPtr^ := CellPtr;
      Inc(SortedPtr);
      Inc(CellPtr);
    end;
  end;
  
  CellPtr := BlockPtr^;
  //Inc(BlockPtr);
  I := FNumCells and CellBlockMask;
  while I <> 0 do
  begin
    Dec(I);
  
    SortedPtr^ := CellPtr;
    Inc(SortedPtr);
    Inc(CellPtr);
  end;
  PPCell(LongWord(FSortedCells) + FNumCells * SizeOf(PCell))^ := nil;
  QSortCells(FSortedCells, FNumCells);
end;



end.
