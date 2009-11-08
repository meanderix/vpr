unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GR32_Image, XPMan;

type
  TForm3 = class(TForm)
    Img: TImage32;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

uses
  GR32, AGG2D, Math, GR32_Resamplers, GR32_PolygonsEx, GR32_LowLevel,
  GR32_Polygons;

function CrossProd(X1, Y1, X2, Y2: TFloat): TFloat;
begin
  Result := X1 * Y2 - Y1 * X2;
end;

function MakeCurve(const Points: TArrayOfFloatPoint; Kernel: TCustomKernel;
  Closed: Boolean): TArrayOfFloatPoint;
const
  TOLERANCE: TFloat = 20.0;
  THRESHOLD: TFloat = 0.5;
var
  I, H, R: Integer;
  Filter: TFilterMethod;
  WrapProc: TWrapProc;

  procedure AddPoint(const P: TFloatPoint);
  var
    L: Integer;
  begin
    L := Length(Result);
    SetLength(Result, L + 1);
    Result[L] := P;
  end;

  function GetPoint(I: Integer; t: TFloat = 0.0): TFloatPoint;
  var
    f, Index: Integer;
    W: TFloat;
  begin
    Result.X := 0; Result.Y := 0;
    for f := -R to R do
    begin
      Index := WrapProc(I - f, H);
      W := Filter(f + t);
      Result.X := Result.X + W * Points[Index].X;
      Result.Y := Result.Y + W * Points[Index].Y;
    end;
  end;

  procedure Recurse(I: Integer; const P1, P2: TFloatPoint; const t1, t2: TFloat);
  var
    f, Index: Integer;
    t, W: TFloat;
    P: TFloatPoint;
  begin
    AddPoint(P1);
    t := (t1 + t2) * 0.5;
    P := GetPoint(I, t);

    if (Abs(CrossProd(P1.X - P.X, P1.Y - P.Y, P.X - P2.X, P.Y - P2.Y)) > TOLERANCE) or (t2 - t1 >= THRESHOLD) then
    begin
      Recurse(I, P1, P, t1, t);
      Recurse(I, P, P2, t, t2);
    end
    else AddPoint(P);
  end;

const
  WRAP_PROC: array[Boolean] of TWrapProc = (Clamp, Wrap);
begin
  WrapProc := Wrap_PROC[Closed];
  Filter := Kernel.Filter;
  R := Ceil(Kernel.GetWidth);
  H := High(Points);

  for I := 0 to H - 1 do
  begin
    Recurse(I, GetPoint(I), GetPoint(I + 1), 0, 1);
  end;
  if Closed then
    Recurse(H, GetPoint(H), GetPoint(0), 0, 1)
  else
    AddPoint(GetPoint(H));
end;

function Ellipse(const X, Y, Rx, Ry: TFloat): TArrayOfFloatPoint;
const
  M: TFloat = 1/360*2*Pi;
var
  I: Integer;
  t: TFloat;
begin
  SetLength(Result, 360);
  for I := 0 to 359 do
  begin
    t := I * M;
    Result[I].X := Rx * Cos(t) + X;
    Result[I].Y := Ry * Sin(t) + Y;
  end;
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  PX, PY: TArrayOfFloatPoint;
  I: Integer;
  K: TCustomKernel;
  X, Y: Integer;
begin
  //Randomize;
  Img.SetupBitmap(True, $ff333333);
  SetLength(PX, 8);

  // create a set of random data points
  for I := 0 to High(PX) do
  begin
    PX[I] := FloatPoint(Random(Img.Width), Random(Img.Height));
    //PX[I] := FloatPoint((I * Img.Width)/High(PX), Random(Img.Height));
  end;
  //PX[0] := FloatPoint(0, Img.Height);
  //PX[High(PX)] := FloatPoint(Img.Width, Img.Height);

  // create interpolation kernel
  K := TGaussianKernel.Create;
  try
    // subdivide recursively and interpolate
    PY := MakeCurve(PX, K, True);
  finally
    K.Free;
  end;

  // draw result polygon
  PolygonFS(Img.Bitmap, PY, $ffcc3300, pfWinding);

  // draw data points
  for I := 0 to High(PY) do
  begin
    X := Floor(PY[I].X);
    Y := Floor(PY[I].Y);
    Img.Bitmap.FillRects(X, Y, X + 1, Y + 1, $ff00ff00);
  end;
  for I := 0 to High(PX) do
  begin
    PY := Ellipse(PX[I].X, PX[I].Y, 4, 4);
    PolygonFS(Img.Bitmap, PY, $ff000000);
    PY := Ellipse(PX[I].X, PX[I].Y, 2.75, 2.75);
    PolygonFS(Img.Bitmap, PY, $ff00ff00);
  end;
end;

end.
