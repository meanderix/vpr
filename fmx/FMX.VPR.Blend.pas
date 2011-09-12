unit FMX.VPR.Blend;

interface

uses
  System.UITypes, System.Math;

function BlendRegEx(F, B, M: TAlphaColor): TAlphaColor;
procedure BlendMemEx(F: TAlphaColor; var B:TAlphaColor; M: TAlphaColor);
function CombineReg(X, Y, W: TAlphaColor): TAlphaColor;
procedure CombineMem(F: TAlphaColor; var B: TAlphaColor; W: TAlphaColor);
procedure BlendRGB(F, W: TAlphaColor; var B: TAlphaColor);
procedure BlendLine(Src, Dst: PAlphaColor; Count: Integer);

const
  // gamma correction exponent
  DEFAULT_GAMMA = 1.7;

var
  GAMMA_TABLE: array [0..$1000] of Word;

implementation

var
  AlphaTable: Pointer;
  bias_ptr: Pointer;
  alpha_ptr: Pointer;

procedure GenAlphaTable;
var
  I: Integer;
  L: Longword;
  P: ^Longword;
begin
  GetMem(AlphaTable, 257 * 8 * SizeOf(Cardinal));
  alpha_ptr := Pointer(Cardinal(AlphaTable) and $FFFFFFF8);
  if Cardinal(alpha_ptr) < Cardinal(AlphaTable) then
    alpha_ptr := Pointer(Integer(alpha_ptr) + 8);
  P := alpha_ptr;
  for I := 0 to 255 do
  begin
    L := I + I shl 16;
    P^ := L;
    Inc(P);
    P^ := L;
    Inc(P);
    P^ := L;
    Inc(P);
    P^ := L;
    Inc(P);
  end;
  bias_ptr := Pointer(Integer(alpha_ptr) + $80 * 4 * SizeOf(Cardinal));
end;

procedure FreeAlphaTable;
begin
  FreeMem(AlphaTable);
end;

{ Gamma / Pixel Shape Correction table }

procedure SetGamma(Gamma: Single);
var
  i: Integer;
begin
//  for i := 0 to 255 do
//    GAMMA_TABLE[i] := Round(255 * Power(i / 255, Gamma));
  for i := 0 to $1000 do
    GAMMA_TABLE[i] := Round($1000 * Power(i / $1000, Gamma));
end;

type
  TAlphaColorRec = packed record B, G, R, A: Byte; end;

//----------------------------------------------------------------------------//
// pure pascal blending routines
//----------------------------------------------------------------------------//
{$IFDEF PUREPASCAL}
function BlendRegEx(F, B, M: TAlphaColor): TAlphaColor;
begin
// TBA
end;

procedure BlendMemEx(F: TAlphaColor; var B:TAlphaColor; M: TAlphaColor);
begin
  M := TAlphaColorRec(F).A * M;
  TAlphaColorRec(B).R := M * (TAlphaColorRec(F).R - TAlphaColorRec(B).R) div (255*255) + TAlphaColorRec(B).R;
  TAlphaColorRec(B).G := M * (TAlphaColorRec(F).G - TAlphaColorRec(B).G) div (255*255) + TAlphaColorRec(B).G;
  TAlphaColorRec(B).B := M * (TAlphaColorRec(F).B - TAlphaColorRec(B).B) div (255*255) + TAlphaColorRec(B).B;
end;

function CombineReg(X, Y, W: TAlphaColor): TAlphaColor;
begin
  TAlphaColorRec(Result).A := W * (TAlphaColorRec(X).A - TAlphaColorRec(Y).A) div 255 + TAlphaColorRec(Y).A;
  TAlphaColorRec(Result).R := W * (TAlphaColorRec(X).R - TAlphaColorRec(Y).R) div 255 + TAlphaColorRec(Y).R;
  TAlphaColorRec(Result).G := W * (TAlphaColorRec(X).G - TAlphaColorRec(Y).G) div 255 + TAlphaColorRec(Y).G;
  TAlphaColorRec(Result).B := W * (TAlphaColorRec(X).B - TAlphaColorRec(Y).B) div 255 + TAlphaColorRec(Y).B;
end;

procedure BlendLine(Src, Dst: PAlphaColor; Count: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    BlendMemEx(Src^, Dst^, 255);
    Inc(Src); Inc(Dst);
  end;
end;
{$ELSE}

//----------------------------------------------------------------------------//
// X86 blending routines
//----------------------------------------------------------------------------//
{$IFDEF CPUX86}
function BlendRegEx(F, B, M: TAlphaColor): TAlphaColor;
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // ECX <- M
  // Result := M * Fa * (Frgb - Brgb) + Brgb
        PUSH      EBX
        MOV       EBX,EAX
        SHR       EBX,24
        INC       ECX             // 255:256 range bias
        IMUL      ECX,EBX
        SHR       ECX,8
        JZ        @1

        PXOR      XMM0,XMM0
        MOVD      XMM1,EAX
        SHL       ECX,4
        MOVD      XMM2,EDX
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0
        ADD       ECX,alpha_ptr
        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[ECX]
        PSLLW     XMM2,8
        MOV       ECX,bias_ptr
        PADDW     XMM2,[ECX]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      EAX,XMM1
//        OR        EAX,$ff000000

        POP       EBX
        RET

@1:     MOV       EAX,EDX
        POP       EBX
end;

procedure BlendMemEx(F: TAlphaColor; var B: TAlphaColor; M: TAlphaColor);
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // [EDX] <- B
  // ECX <- M
  // Result := M * Fa * (Frgb - Brgb) + Brgb
        TEST      EAX,$FF000000
        JZ        @2

        PUSH      EBX
        MOV       EBX,EAX
        SHR       EBX,24
        INC       ECX             // 255:256 range bias
        IMUL      ECX,EBX
        SHR       ECX,8
        JZ        @1

        PXOR      XMM0,XMM0
        MOVD      XMM1,EAX
        SHL       ECX,4

//        MOV       EAX,[EDX]
        MOVD      XMM2,[EDX]
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0
        ADD       ECX,alpha_ptr
        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[ECX]
        PSLLW     XMM2,8
        MOV       ECX,bias_ptr
        PADDW     XMM2,[ECX]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      [EDX],XMM1

//        AND       EAX,$ff000000
//        OR        [EDX],EAX
//        OR        [EDX],$ff000000

@1:     POP       EBX

@2:
end;


function CombineReg(X, Y, W: TAlphaColor): TAlphaColor;
asm
  // EAX - Color X
  // EDX - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        MOVD      XMM1,EAX
        PXOR      XMM0,XMM0
        SHL       ECX,4

        MOVD      XMM2,EDX
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0

        ADD       ECX,alpha_ptr

        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[ECX]
        PSLLW     XMM2,8

        MOV       ECX,bias_ptr

        PADDW     XMM2,[ECX]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      EAX,XMM1
end;

procedure CombineMem(F: TAlphaColor; var B: TAlphaColor; W: TAlphaColor);
asm
  // EAX - Color X
  // [EDX] - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        JCXZ    @1

        CMP       ECX,$FF
        JZ        @2

        MOVD      XMM1,EAX
        PXOR      XMM0,XMM0

        SHL       ECX,4

        MOVD      XMM2,[EDX]
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0

        ADD       ECX,alpha_ptr

        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[ECX]
        PSLLW     XMM2,8

        MOV       ECX,bias_ptr

        PADDW     XMM2,[ECX]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      [EDX],XMM1

@1:     RET

@2:     MOV       [EDX],EAX
end;

procedure BlendRGB(F, W: TAlphaColor; var B: TAlphaColor);
asm
        PXOR      XMM2,XMM2
        MOVD      XMM0,EAX
        PUNPCKLBW XMM0,XMM2
        MOVD      XMM1,[ECX]
        PUNPCKLBW XMM1,XMM2
        BSWAP     EDX
        PSUBW     XMM0,XMM1
        MOVD      XMM3,EDX
        PUNPCKLBW XMM3,XMM2
        PMULLW    XMM0,XMM3
        MOV       EAX,bias_ptr
        PSLLW     XMM1,8
        PADDW     XMM1,[EAX]
        PADDW     XMM1,XMM0
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM2
        MOVD      [ECX],XMM1
end;

procedure BlendLine(Src, Dst: PAlphaColor; Count: Integer);
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

        TEST      ECX,ECX
        JZ        @4

        PUSH      ESI
        PUSH      EDI

        MOV       ESI,EAX
        MOV       EDI,EDX

@1:     MOV       EAX,[ESI]
        TEST      EAX,$FF000000
        JZ        @3
        CMP       EAX,$FF000000
        JNC       @2

        MOVD      XMM0,EAX
        PXOR      XMM3,XMM3
        MOVD      XMM2,[EDI]
        PUNPCKLBW XMM0,XMM3
        MOV       EAX,bias_ptr
        PUNPCKLBW XMM2,XMM3
        MOVQ      XMM1,XMM0
        PUNPCKLBW XMM1,XMM3
        PUNPCKHWD XMM1,XMM1
        PSUBW     XMM0,XMM2
        PUNPCKHDQ XMM1,XMM1
        PSLLW     XMM2,8
        PMULLW    XMM0,XMM1
        PADDW     XMM2,[EAX]
        PADDW     XMM2,XMM0
        PSRLW     XMM2,8
        PACKUSWB  XMM2,XMM3
        MOVD      EAX,XMM2

// N: make output opaque in order to avoid problem in UpdateLayeredWindow (FMX.Platform.Win)
        OR        EAX,$ff000000

@2:     MOV       [EDI],EAX

@3:     ADD       ESI,4
        ADD       EDI,4

        DEC       ECX
        JNZ       @1

        POP       EDI
        POP       ESI

@4:     RET
end;
{$ENDIF}

//----------------------------------------------------------------------------//
// X64 blending routines
//----------------------------------------------------------------------------//
{$IFDEF CPUX64}
function BlendRegEx(F, B, M: TAlphaColor): TAlphaColor;
begin
// TBA
end;

procedure BlendMemEx(F: TAlphaColor; var B:TAlphaColor; M: TAlphaColor);
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // [EDX] <- B
  // ECX <- M
  // Result := M * Fa * (Frgb - Brgb) + Brgb

        TEST      RCX,$FF000000
        JZ        @1

        MOV       R9,RCX
        SHR       R9,24
        INC       R8             // 255:256 range bias
        IMUL      R8,R9
        SHR       R8,8
        JZ        @1

        PXOR      XMM0,XMM0
        MOVD      XMM1,ECX
        SHL       R8,4
        MOVD      XMM2,[RDX]
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0
        ADD       R8,alpha_ptr
        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[R8]
        PSLLW     XMM2,8
        MOV       R8,bias_ptr
        PADDW     XMM2,[R8]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      dword ptr [RDX],XMM1
//        OR        dword ptr [RDX],$ff000000  // set opaque

@1:
end;

function CombineReg(X, Y, W: TAlphaColor): TAlphaColor;
asm
  // ECX - Color X
  // EDX - Color Y
  // E8 - Weight of X [0..255]
  // Result := W * (X - Y) + Y
        MOVD      XMM1,ECX
        PXOR      XMM0,XMM0
        SHL       R8,4

        MOVD      XMM2,EDX
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0

        ADD       R8,alpha_ptr

        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[R8]
        PSLLW     XMM2,8

        MOV       R8,bias_ptr

        PADDW     XMM2,[R8]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      EAX,XMM1
end;

procedure CombineMem(F: TAlphaColor; var B: TAlphaColor; W: TAlphaColor);
begin
// TBA
end;

procedure BlendRGB(F, W: TAlphaColor; var B: TAlphaColor);
asm
        PXOR      XMM2,XMM2
        MOVD      XMM0,ECX
        PUNPCKLBW XMM0,XMM2
        MOVD      XMM1,dword ptr [R8]
        PUNPCKLBW XMM1,XMM2
        BSWAP     EDX
        PSUBW     XMM0,XMM1
        MOVD      XMM3,EDX
        PUNPCKLBW XMM3,XMM2
        PMULLW    XMM0,XMM3
        MOV       RCX,bias_ptr
        PSLLW     XMM1,8
        PADDW     XMM1,[RCX]
        PADDW     XMM1,XMM0
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM2
        MOVD      dword ptr [R8],XMM1
end;

// TODO: add X64 basm version
procedure BlendLine(Src, Dst: PAlphaColor; Count: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    BlendMemEx(Src^, Dst^, 255);
    Inc(Src); Inc(Dst);
  end;
end;
{$ENDIF}
{$ENDIF}

initialization
  SetGamma(DEFAULT_GAMMA);
  GenAlphaTable;

finalization
  FreeAlphaTable;

end.
