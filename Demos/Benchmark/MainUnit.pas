{-------------------------------------------------------------------------------
                      Polygon Rasterizer Benchmark Tool
 -------------------------------------------------------------------------------

 Libraries included in the benchmark:

   VPR & GR32_PolygonsEx by Mattias Andersson
   <https://sourceforge.net/projects/vpr/>

   GR32_Polygons rasterizer of Graphics32
   <http://graphics32.org>

   AggPas by Milan Marusinec (pascal port of AGG by Maxim Shemanarev)
   <http://www.aggpas.org/>
   <http://graphics32.org/news/newsgroups.php?art_group=graphics32.general&article_id=8511>

   LibArt by Raph Levien
   <http://www.levien.com/libart/>
   <http://gnuwin32.sourceforge.net/packages/libart_lgpl.htm>

   Cairo Graphics by Red Hat, Inc.
   <http://www.cairographics.org/>
   <http://www.rodi.dk/programming_cairo.php>

   GDI+ by Microsoft Corporation
   <http://msdn.microsoft.com/en-us/library/ms533798(VS.85).aspx>


 Other libraries that would be interesting to include:

   OpenVG by Khronos Group
   <http://www.khronos.org/openvg/>

   AmanithVG by Mazatech S.r.l.
   <http://www.amanith.org/>

-------------------------------------------------------------------------------}

// undefine to disable library
{$DEFINE GDIPLUS}
{$DEFINE AGGPAS}
{$DEFINE AGGLITE}
{$DEFINE LIBART}
{$DEFINE CAIRO}
{$DEFINE DIRECT2D}

unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, GR32_Image, XPMan,
  GR32, GR32_Polygons, GR32_PolygonsEx, GR32_RangeBars
{$IFDEF GDIPLUS},
  GDIPAPI,
  GDIPOBJ
{$ENDIF}
{$IFDEF AGGPAS},
  Agg2D
{$ENDIF}
{$IFDEF AGGLITE},
  Agg, AggRasterizer
{$ENDIF}
{$IFDEF LIBART},
  LibArt
{$ENDIF}
{$IFDEF CAIRO},
  Cairo, CairoWin32
{$ENDIF}
{$IFDEF DIRECT2D},
  Direct2D, D2D1
{$ENDIF};

type
  TMainForm = class(TForm)
    btnBenchmark: TButton;
    cbAllRasterizers: TCheckBox;
    cbAllTests: TCheckBox;
    cbOpaque: TCheckBox;
    cbStroke: TCheckBox;
    gbResults: TGroupBox;
    gbSeed: TGaugeBar;
    Img: TImage32;
    lblSeed: TLabel;
    meResults: TMemo;
    pnlControl: TPanel;
    pnlImage: TPanel;
    pnlSeed: TPanel;
    rgRasterizer: TRadioGroup;
    rgTest: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBenchmarkClick(Sender: TObject);
    procedure gbSeedChange(Sender: TObject);
    procedure ImgPaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
    procedure rgRasterizerClick(Sender: TObject);
  public
    Polygon: TArrayOfArrayOfFloatPoint;
    TextPolygon: array [0..5] of TArrayOfArrayOfFloatPoint;
    AAMode: TAntialiasMode;
{$IFDEF AGGPAS}
    Agg2D: TAgg2D;
{$ENDIF}
{$IFDEF AGGLITE}
    RBuf: TRenderingBuffer;
    Ren: TRenderer;
    Ras: TRasterizer;
{$ENDIF}
{$IFDEF GDIPLUS}
    GPGraphics: TGPGraphics;
    GPPen: TGPPen;
    GPBrush: TGPSolidBrush;
{$ENDIF}
{$IFDEF CAIRO}
    surface: Pcairo_surface_t;
    cr: Pcairo_t;
{$ENDIF}
{$IFDEF DIRECT2D}
    d2d: TDirect2DCanvas;
{$ENDIF}
    function GetFixedPolygon: TArrayOfArrayOfFixedPoint;
    function GetIterations: Integer;
    procedure BuildPolygon;
    procedure RenderPolygon(Dst: TBitmap32);
    procedure Benchmark;
    function RandomEllipse: TArrayOfFloatPoint;
    function RandomLine: TArrayOfFloatPoint;
    function RandomLine2: TArrayOfFloatPoint;
    function RandomSpline: TArrayOfFloatPoint;
    function RandomText: TArrayOfArrayOfFloatPoint;
    procedure StopTimer;
    procedure CreateTextPolygons;
  end;

var
  MainForm: TMainForm;

const
  XSIZE = 640;
  YSIZE = 480;

implementation

{$R *.dfm}

uses
  Math, GR32_VPR, GR32_VectorUtils, GR32_System, GR32_Resamplers, GR32_LowLevel,
  GR32_Blend, GR32_PathsEx { needed for text rendering };

const
  STRINGS: array [0..5] of string = (
    'Graphics32',
    'Excellence endures!',
    'Hello World!',
    'Lorem ipsum dolor sit amet, consectetur adipisicing elit,' + #13#10 +
    'sed do eiusmod tempor incididunt ut labore et dolore magna' + #13#10 +
    'aliqua. Ut enim ad minim veniam, quis nostrud exercitation' + #13#10 +
    'ullamco laboris nisi ut aliquip ex ea commodo consequat.',
    'The quick brown fox jumps over the lazy dog.',
    'Jackdaws love my big sphinx of quartz.');
  ONE255TH : Double = 1 / 255;  

type
  TFontEntry = record
    Name: string;
    Size: Integer;
    Style: TFontStyles;
  end;

const
  FACES: array [0..5] of TFontEntry = (
    (Name: 'Trebuchet MS'; Size: 24; Style: [fsBold]),
    (Name: 'Tahoma'; Size: 20; Style: [fsItalic]),
    (Name: 'Courier New'; Size: 14; Style: []),
    (Name: 'Georgia'; Size: 8; Style: [fsItalic]),
    (Name: 'Times New Roman'; Size: 12; Style: []),
    (Name: 'Garamond'; Size: 12; Style: [])
  );

procedure SinCos(const Argument: TFloat; var Sin, Cos: TFloat);
asm
  fld     Argument
  fsincos
  fstp    edx.TFloat   // Cos
  fstp    eax.TFloat   // Sin
end;

function Ellipse(const X, Y, Rx, Ry: TFloat): TArrayOfFloatPoint;
const
  M : TFloat = 2 * System.Pi / 100;
var
  I: Integer;
  t: TFloat;
  Data : array [0..3] of TFloat;
begin
  SetLength(Result, 100);

  // first item
  Result[0].X := Rx + X;
  Result[0].Y := Y;

  // calculate complex offset
  SinCos(M, Data[0], Data[1]);
  Data[2] := Data[0];
  Data[3] := Data[1];

  // second item
  Result[1].X := Rx * Data[3] + X;
  Result[1].Y := Ry * Data[2] + Y;

  // other items
  for I := 2 to 99 do
  begin
    t := Data[3];
    Data[3] := Data[3] * Data[1] - Data[2] * Data[0];
    Data[2] := Data[2] * Data[1] + t * Data[0];

    Result[I].X := Rx * Data[3] + X;
    Result[I].Y := Ry * Data[2] + Y;
  end;
end;

function CreateLine(x1, y1, x2, y2, width: TFloat): TArrayOfFloatPoint;
var
  dx, dy, d: TFloat;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  d := Sqrt(Sqr(dx) + Sqr(dy));
  if d <> 0 then
  begin
    dx := width * (y2 - y1) / d;
    dy := width * (x2 - x1) / d;
    SetLength(Result, 4);
    Result[0] := FloatPoint(x1 - dx, y1 + dy);
    Result[1] := FloatPoint(x2 - dx, y2 + dy);
    Result[2] := FloatPoint(x2 + dx, y2 - dy);
    Result[3] := FloatPoint(x1 + dx, y1 - dy);
  end
  else
  begin
    SetLength(Result, 2);
    Result[0] := FloatPoint(x1, y1);
    Result[1] := FloatPoint(x2, y2);
  end;
end;

function MakeCurve(const Points: TArrayOfFloatPoint; Kernel: TCustomKernel;
  Closed: Boolean; StepSize: Integer): TArrayOfFloatPoint;
var
  I, J, F, H, Index, LastIndex, Steps, R: Integer;
  K, V, W, dx, dy, X, Y: TFloat;
  Filter: TFilterMethod;
  WrapProc: TWrapProc;
  PPoint: PFloatPoint;
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
      V := 0;
      for J := 0 to Steps do
      begin
        X := 0; Y := 0;
        for F := -R to R do
        begin
          Index := WrapProc(I - F, H);
          W := Filter(F + V);
          X := X + W * Points[Index].X;
          Y := Y + W * Points[Index].Y;
        end;
        PPoint^ := FloatPoint(X, Y);
        Inc(PPoint);
        V := V + K;
      end;
    end;
  end;
end;

function RandColor: TColor32;
begin
  Result := Random($FFFFFFFF) and $FF777777 + $333333;
end;


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  P: PPaintStage;
begin
  SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_HIGHEST);
  with Img.PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then Stage := PST_CUSTOM;
  end;

  P := Img.PaintStages.Add;
  P.Stage := PST_CUSTOM;
  P.Parameter := 1;

  CreateTextPolygons;
  Img.BufferOversize := 0;
  Img.Bitmap.SetSize(XSIZE, YSIZE);
  Img.Bitmap.Clear(clWhite32);

{$IFDEF AGGPAS}
  Agg2D := TAgg2D.Create;
  Agg2D.Attach32(Img.Bitmap);
  Agg2D.LineWidth(1.0);
{$ENDIF}

{$IFDEF AGGLITE}
  RBuf := TRenderingBuffer.Create(PByte(Img.Bitmap.Bits), XSIZE, YSIZE, XSIZE * SizeOf(TColor32));

  // Create the renderer and the rasterizer
  Ren := TRenderer.Create(RBuf);
  Ren.SpanType := stRGBA32;
  Ras := TRasterizer.Create;
  Ras.Gamma(1.0);

  // Setup the rasterizer
  Ras.FillingRule := frFillNonZero;
{$ENDIF}

{$IFDEF GDIPLUS}
  // initialize GDI plus
  GPGraphics := TGPGraphics.Create(Img.Bitmap.Handle);
  GPPen := TGPPen.Create(Color);
  GPPen.SetWidth(1.0);
  GPBrush := TGPSolidBrush.Create(Color);
{$ENDIF}

{$IFDEF CAIRO}
  // initialize cairo
  surface := cairo_win32_surface_create(Img.Bitmap.Handle);
  cr := cairo_create(surface);
  cairo_set_line_width(cr, 1.0);
  //cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);
{$ENDIF}
{$IFDEF DIRECT2D}
  d2d := TDirect2DCanvas.Create(Img.Bitmap.Handle, Img.Bitmap.BoundsRect);
{$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
{$IFDEF AGGPAS}
  Agg2D.Free;
{$ENDIF}
{$IFDEF AGGLITE}
  Ras.Free;
  Ren.Free;
  RBuf.Free;
{$ENDIF}
{$IFDEF GDIPLUS}
  GPGraphics.Free;
  GPPen.Free;
  GPBrush.Free;
{$ENDIF}
{$IFDEF CAIRO}
  cairo_destroy(cr);
  cairo_surface_destroy(surface);
{$ENDIF}
{$IFDEF DIRECT2D}
  d2d.Free;
{$ENDIF}
end;


procedure TMainForm.btnBenchmarkClick(Sender: TObject);
var
  I, J: Integer;
begin
  Img.Bitmap.Clear(clWhite32);
  for J := 0 to rgTest.Items.Count - 1 do
  begin
    if cbAllTests.Checked then rgTest.ItemIndex := J;
    for I := 0 to rgRasterizer.Items.Count - 1 do
    begin
    if cbAllRasterizers.Checked then rgRasterizer.ItemIndex := I;
      if (I = rgRasterizer.ItemIndex) and (J = rgTest.ItemIndex) then
      begin
        Benchmark;
      end;
    end;
  end;
end;

procedure TMainForm.Benchmark;
var
  I, Iter: Integer;
  Dst: TBitmap32;
begin
  RandSeed := gbSeed.Position;
  Screen.Cursor := crHourglass;
  Dst := Img.Bitmap;
  Iter := GetIterations;
  GlobalPerfTimer.Start;

  {$IFDEF DIRECT2D}
  if rgRasterizer.ItemIndex = 8 then
    d2d.BeginDraw;
  {$ENDIF}
  for I := 0 to Iter - 1 do
  begin
    BuildPolygon;
    RenderPolygon(Dst);
  end;
  {$IFDEF DIRECT2D}
  if rgRasterizer.ItemIndex = 8 then
    d2d.EndDraw;
  {$ENDIF}

  StopTimer;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.BuildPolygon;
begin
  SetLength(Polygon, 1);
  case rgTest.ItemIndex of
    0: Polygon[0] := RandomEllipse;
    1: Polygon[0] := RandomLine;
    2: Polygon[0] := RandomLine2;
    3: Polygon[0] := RandomSpline;
    4: Polygon := RandomText;
  end;
end;

{$IFDEF LIBART}
// N1: this is a bit different from LibArt's own callbacks, but it seems to work OK.
// N2: it should be possible to rewrite it for the even-odd fill rule as well.
procedure LibArtCallback(data: Pointer; y, start: Integer;
  steps: PArtSVPRenderAAStep; n_steps: Integer); cdecl;
var
  I: Integer;
  Dst: TBitmap32;
  Bits: PColor32Array;
  running_sum, W, C, Ca: Integer;
  X, X1, X2: Integer;
  AlphaValues: PColor32Array;
begin
  if n_steps = 0 then Exit;
  Dst := TBitmap32(data);
  Bits := Dst.ScanLine[Y];
  C := Dst.PenColor;
  Ca := C shr 24 * 257 div 255;
  C := C and $ffffff;

  AlphaValues := StackAlloc(Dst.Width * SizeOf(TColor32));

  running_sum := start - 32768;
  W := Abs(running_sum);
  if W > $ff0000 then W := $ff0000;
  W := W * Ca and $ff000000;

  if W = 0 then X := steps.x else X := 0;
  X1 := X;
  for I := 0 to n_steps - 1 do
  begin
    FillLongWord(AlphaValues[X], steps.x - X, W or C);
    X := steps.X;
    Inc(running_sum, Steps.delta);
    W := Abs(running_sum);
    if W > $ff0000 then W := $ff0000;
    W := W * Ca and $ff000000;
    Inc(Steps);
  end;
  X2 := X;
  if W <> 0 then
  begin
    FillLongWord(AlphaValues[X], Dst.Width - X, W or C);
    X2 := Dst.Width;
  end;
  BlendLine(@AlphaValues[X1], @Bits[X1], X2 - X1);
  EMMS;

  StackFree(AlphaValues);
end;
{$ENDIF}

procedure TMainForm.RenderPolygon(Dst: TBitmap32);
var
  Color: TColor32;
  Poly: TArrayOfArrayOfFixedPoint;

{$IFDEF AGGPAS}
  procedure RenderAggPas;
  var
    I, J: Integer;
  begin
    Agg2D.FillColor(TAggColor(Color));
    Agg2D.LineColor(TAggColor(Color));
    Agg2D.ResetPath;
    for J := 0 to High(Polygon) do
    begin
      Agg2D.MoveTo(Polygon[J, 0].X, Polygon[J, 0].Y);
      for I := 1 to High(Polygon[J]) do
        Agg2D.LineTo(Polygon[J, I].X, Polygon[J, I].Y);
      Agg2D.ClosePolygon;
    end;
    if cbStroke.Checked then
      Agg2D.DrawPath(AGG_StrokeOnly)
    else
      Agg2D.DrawPath(AGG_FillOnly);
  end;
{$ENDIF}

{$IFDEF AGGLITE}
  procedure RenderAGGLITE;
  var
    Bits: Pointer;
    RGBA8: TRGBA8;
    I, J: Integer;
  begin

    for J := 0 to High(Polygon) do
    begin
      Ras.MoveToD(Polygon[J, 0].X, Polygon[J, 0].Y);
      for I := 1 to High(Polygon[J]) do
        Ras.LineToD(Polygon[J, I].X, Polygon[J, I].Y);
      //Agg2D.ClosePolygon;
    end;
    with TColor32Entry(Color) do
    begin
      RGBA8.R := B;
      RGBA8.G := G;
      RGBA8.B := R;
      RGBA8.A := A;
    end;
    Ras.Render(Ren, RGBA8);
    //Ren.Clear(RGBA8(255, 255, 255));

  end;
{$ENDIF}

{$IFDEF GDIPLUS}
  procedure RenderGdiPlus;
  var
    Path: TGPGraphicsPath;
    I, J: Integer;
  begin
    Path := TGPGraphicsPath.Create;
    try
      for J := 0 to High(Polygon) do
      begin
        Path.StartFigure;
        for I := 0 to High(Polygon[J]) - 1 do
          Path.AddLine(TGPPointF(Polygon[J, I]), TGPPointF(Polygon[J, I + 1]));
        Path.CloseFigure;
      end;
      if cbStroke.Checked then
      begin
        GPPen.SetColor(Color);
        GPGraphics.DrawPath(GPPen, Path)
      end
      else
      begin
        GPBrush.SetColor(Color);
        GPGraphics.FillPath(GPBrush, Path);
      end;
    finally
      Path.Free;
    end;
  end;
{$ENDIF}

{$IFDEF LIBART}
  procedure RenderLibArt;
  var
    I, J, K, N: Integer;
    path: TArrayOfVPath;
    svp: PArtSVP;
  begin
    Dst.PenColor := Color;
    N := 0;
    for J := 0 to High(Polygon) do Inc(N, Length(Polygon[J])  + 1);
    SetLength(path, N + 1);
    K := 0;
    for J := 0 to High(Polygon) do
    begin
      N := Length(Polygon[J]);
      path[K].Code := ART_MOVETO;
      path[K].X := Polygon[J, 0].X;
      path[K].Y := Polygon[J, 0].Y;
      Inc(K);
      for I := 1 to N - 1 do
      begin
        path[K].Code := ART_LINETO;
        path[K].X := Polygon[J, I].X;
        path[K].Y := Polygon[J, I].Y;
        Inc(K);
      end;
      path[K].X := Polygon[J, 0].X;
      path[K].Y := Polygon[J, 0].Y;
      path[K].Code := ART_LINETO;
      Inc(K);
    end;
    path[K].Code := ART_END;
    svp := art_svp_from_vpath(@Path[0]);
    art_svp_render_aa(svp, 0, 0, XSIZE, YSIZE, LibArtCallback, Dst);
    art_svp_free(svp);
  end;
{$ENDIF}

{$IFDEF CAIRO}
  procedure RenderCairo;
  var
    I, J: Integer;
  begin
    with TColor32Entry(Color) do
      cairo_set_source_rgba(cr, r * ONE255TH, g * ONE255TH, b * ONE255TH, a * ONE255TH);
    cairo_new_path(cr);
    for J := 0 to High(Polygon) do
    begin
      cairo_move_to(cr, Polygon[J, 0].X, Polygon[J, 0].Y);
      for I := 1 to High(Polygon[J]) do
        cairo_line_to(cr, Polygon[J, I].X, Polygon[J, I].Y);
    end;
    cairo_close_path(cr);
    if cbStroke.Checked then
      cairo_stroke(cr)
    else
      cairo_fill(cr);
  end;
{$ENDIF}

{$IFDEF DIRECT2D}
  procedure RenderDirect2D;
  var
    Geometry: ID2D1PathGeometry;
    Sink: ID2D1GeometrySink;
    HR: HRESULT;
    I, J: Integer;
  begin
    d2d.Brush.Color := WinColor(Color);
    d2d.Brush.Handle.SetOpacity((Color and $ff000000)/$ff000000);

    D2DFactory.CreatePathGeometry(Geometry);
    HR := Geometry.Open(Sink);
    Sink.SetFillMode(D2D1_FILL_MODE_WINDING);

    for I := 0 to High(Polygon) do
    begin
      with Polygon[I][0] do
        Sink.BeginFigure(D2D1PointF(X, Y), D2D1_FIGURE_BEGIN_FILLED);
      try
        for J := 1 to High(Polygon[I]) do
          with Polygon[I][J] do
            Sink.AddLine(D2D1PointF(X, Y));
      finally
        Sink.EndFigure(D2D1_FIGURE_END_CLOSED);
      end;
    end;
    hr := Sink.Close;

    if cbStroke.Checked then
      d2d.RenderTarget.DrawGeometry(Geometry, d2d.Brush.Handle)
    else
      d2d.RenderTarget.FillGeometry(Geometry, d2d.Brush.Handle);
  end;
{$ENDIF}

begin
  Color := RandColor;
  if cbOpaque.Checked then Color := Color or $ff000000;
  case rgRasterizer.ItemIndex of
    0:
      begin
        if cbStroke.Checked then
          PolyPolylineFS(Dst, Polygon, Color, True, 1)
        else
          PolyPolygonFS(Dst, Polygon, Color, pfWinding);
      end;
    1:
      begin
        PolyPolygonFS_LCD(Dst, Polygon, Color, pfWinding);
      end;
{$IFDEF AGGPAS}
    2: RenderAggpas;
{$ENDIF}
{$IFDEF AGGLITE}
    3: RenderAGGLITE;
{$ENDIF}
{$IFDEF LIBART}
    4: RenderLibArt;
{$ENDIF}
{$IFDEF GDIPLUS}
    5:
      begin
        GPGraphics.SetSmoothingMode(SmoothingModeHighSpeed);
        RenderGdiPlus;
      end;
{$ENDIF}
{$IFDEF GDIPLUS}
    6:
      begin
        GPGraphics.SetSmoothingMode(SmoothingModeHighQuality);
        RenderGdiPlus;
      end;
{$ENDIF}
{$IFDEF CAIRO}
    7: RenderCairo;
{$ENDIF}
{$IFDEF DIRECT2D}
    8: RenderDirect2D;
{$ENDIF}
    9..13:
      begin
        Poly := GetFixedPolygon;
        if cbStroke.Checked then
          GR32_Polygons.PolyPolylineXS(Dst, Poly, Color, True)
        else
          GR32_Polygons.PolyPolygonXS(Dst, Poly, Color, GR32_Polygons.pfWinding, AAMode, nil);
      end;
  else
  end;
end;

function TMainForm.GetIterations: Integer;
const
  Iterations: array [0..4] of Integer = (5000, 20000, 20000, 5000, 2000);
begin
  Result := Iterations[rgTest.ItemIndex];
end;

function TMainForm.GetFixedPolygon: TArrayOfArrayOfFixedPoint;
var
  I, J, L, N: Integer;
begin
  L := Length(Polygon);
  SetLength(Result, L);
  for J := 0 to L - 1 do
  begin
    N := Length(Polygon[J]);
    SetLength(Result[J], N);
    for I := 0 to N - 1 do
    begin
      Result[J, I] := FixedPoint(Polygon[J, I]);
    end;
  end;
end;

procedure TMainForm.StopTimer;
var
  S1, S2, S3: string;
begin
  S3 := GlobalPerfTimer.ReadMilliseconds;
  with rgTest do S1 := Items[ItemIndex];
  with rgRasterizer do S2 := Items[ItemIndex];
  meResults.Lines.Add(Format('[%s, %s]: %s ms', [S1, S2, S3]));
end;

procedure TMainForm.rgRasterizerClick(Sender: TObject);
const
  Modes: array [0..5] of TAntialiasMode = (am32times, am16times,
    am8times, am4times, am2times, amNone);
begin
  AAMode := Modes[rgRasterizer.ItemIndex - 8];
end;

function TMainForm.RandomEllipse: TArrayOfFloatPoint;
const
  MAXRADIUS = 200;
var
  X, Y, Rx, Ry: TFloat;
begin
  X := Random(XSIZE);
  Y := Random(YSIZE);
  Rx := Random(MAXRADIUS);
  Ry := Random(MAXRADIUS);

  Result := Ellipse(X, Y, Rx, Ry);
end;

function TMainForm.RandomLine: TArrayOfFloatPoint;
begin
  Result := CreateLine(Random(XSIZE), Random(YSIZE), Random(XSIZE),
    Random(YSIZE), 5);
end;

function TMainForm.RandomLine2: TArrayOfFloatPoint;
begin
  Result := CreateLine(Random(XSIZE), Random(YSIZE), Random(XSIZE),
    Random(YSIZE), 0.5);
end;

function TMainForm.RandomSpline: TArrayOfFloatPoint;
var
  Input: TArrayOfFloatPoint;
  I: Integer;
  K: TSplineKernel;
begin
  SetLength(Input, 10);
  for I := 0 to High(Input) do
  begin
    Input[I].X := Random(XSize);
    Input[I].Y := Random(YSIze);
  end;
  K := TSplineKernel.Create;
  try
    Result := MakeCurve(Input, K, True, 3);
  finally
    K.Free;
  end;
end;

procedure TMainForm.CreateTextPolygons;
var
  I: Integer;
  Tmp: TBitmap32;
begin
  Tmp := TBitmap32.Create;
  Tmp.SetSize(5, 5);
  for I := 0 to High(STRINGS) do
  begin
    with Tmp do
    begin
      Font.Name := FACES[I].Name;
      Font.Size := FACES[I].Size;
      Font.Style := FACES[I].Style;
    end;
    TextPolygon[I] := TextToPolygonF(Tmp, -100, -20, STRINGS[I]);
  end;
  Tmp.Free;
end;

function TMainForm.RandomText: TArrayOfArrayOfFloatPoint;
var
  I, J: Integer;
  L: Integer;
  Dx, Dy: Integer;
  Src: TArrayOfArrayOfFloatPoint;
begin
  Dx := Random(XSIZE);
  Dy := Random(YSIZE);
  I := Random(Length(STRINGS));
  Src := TextPolygon[I];
  L := Length(Src);
  SetLength(Result, L);
  for J := 0 to L - 1 do
  begin
    Result[J] := Copy(Src[J]);
    for I := 0 to High(Result[J]) do
    begin
      Result[J, I].X := Result[J, I].X + Dx;
      Result[J, I].Y := Result[J, I].Y + Dy;
    end;
  end;
end;

procedure TMainForm.ImgPaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
const            
  Colors: array [0..1] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  R: TRect;
  I, J: Integer;
  TilesHorz, TilesVert: Integer;
  TileX, TileY: Integer;
  TileHeight, TileWidth: Integer;
begin
  if Img.PaintStages[StageNum].Parameter = 0 then
  begin
    TileHeight := 8;
    TileWidth := 8;

    TilesHorz := Buffer.Width div TileWidth;
    TilesVert := Buffer.Height div TileHeight;
    TileY := 0;

    for J := 0 to TilesVert do
    begin
      TileX := 0;
      for I := 0 to TilesHorz do
      begin
        R.Left := TileX;
        R.Top := TileY;
        R.Right := TileX + TileWidth;
        R.Bottom := TileY + TileHeight;
        Buffer.FillRectS(R, Colors[(I xor J) and $1]);
        Inc(TileX, TileWidth);
      end;
      Inc(TileY, TileHeight);
    end;
    R := Img.GetBitmapRect;
    InflateRect(R, 1, 1);
    Buffer.FrameRectS(R, clBlack32);
  end;
end;

procedure TMainForm.gbSeedChange(Sender: TObject);
begin
  pnlSeed.Caption := IntToStr(gbSeed.Position);
end;

end.
