unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GR32_Image, GR32_RangeBars, StdCtrls;

type
  TForm1 = class(TForm)
    Img: TImage32;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  GR32, GR32_VectorGraphics, GR32_PolygonsEx, GR32_VectorUtils;

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

procedure DrawSimplePolygon(Renderer: TPathRenderer; Cx, Cy, Rx, Ry: TFloat);
var
  I: Integer;
begin
  Renderer.MoveTo(Cx, Cy);
  for I := 0 to 240 do
  begin
    Renderer.LineTo(Cx + Rx * I / 200 * Cos(I / 8), Cy + Ry * I / 200 * Sin(I / 8));
  end;
end;

procedure PaintBitmap(Dst: TBitmap32);
var
  Renderer: TPathRenderer32;
  Polygon: TArrayOfFloatPoint;
  Dashed: TArrayOfArrayOfFloatPoint;
  I: Integer;
begin
  { (a) Fill and stroke a polygon using PolygonFS/PolylineFS }
  Polygon := Ellipse(240, 240, 200, 200);
  PolygonFS(Dst, Polygon, $ffff99cc);
  PolyLineFS(Dst, Polygon, $ffcc6699, True, 5);

  { (b) Create a rounded path with a dashed outline }
  SetLength(Polygon, 4);
  Polygon[0] := FloatPoint(400, 80);
  Polygon[1] := FloatPoint(80, 80);
  Polygon[2] := FloatPoint(400, 400);
  Polygon[3] := FloatPoint(80, 400);
  Polygon := BuildPolyline(Polygon, 50, jsRound, esRound);
  PolygonFS(Dst, Polygon, $7f66cc99);
  Dashed := BuildDashedLine(Polygon, [10, 10, 30, 10]);
  for I := 0 to High(Dashed) do
  begin
    PolylineFS(Dst, Dashed[I], clBlack32, False, 4);
  end;

  { (c) Render a path with an outline using a path renderer }
  Renderer := TPathRenderer32.Create;
  try
    Renderer.Bitmap := Dst;
    Renderer.EndStyle := esSquare;

    Renderer.BeginPath;
    Renderer.StrokeColor := clBlack32;
    Renderer.StrokeWidth := 8;
    DrawSimplePolygon(Renderer, 240, 240, 100, 100);
    Renderer.EndPath;

    Renderer.BeginPath;
    Renderer.StrokeColor := $ff6699cc;
    Renderer.StrokeWidth := 6;
    DrawSimplePolygon(Renderer, 240, 240, 100, 100);
    Renderer.EndPath;
  finally
    Dst.Changed;
    Renderer.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Img.SetupBitmap(True, clWhite32);
  PaintBitmap(Img.Bitmap);
end;

function MakeArrayOfFloatPoints(const a: array of single):
TArrayOfFloatPoint;
var
   i, len: integer;
begin
   len := length(a) div 2;
   setlength(result, len);
   if len = 0 then exit;
   for i := 0 to len -1 do
   begin
     result[i].X := a[i*2];
     result[i].Y := a[i*2 +1];
   end;
end;

procedure Test1BuildPolyline;
var
  pts: TArrayOfFloatPoint;
begin
  pts := nil;
  pts := BuildPolyline(pts, 2.0, jsMiter, esSquare, 2.0);
  //exception raised in VPR ver 1.18
  //corrected by changing line 589 to ...
  //while (H > 0) and (Normals[H].X = 0) and (Normals[H].Y = 0) do Dec(H);
end;

procedure Test2BuildPolyline;
var
  pts: TArrayOfFloatPoint;
begin
  pts := MakeArrayOfFloatPoints([10,10, 10,10]);
  pts := BuildPolyline(pts, 2.0, jsMiter, esSquare, 2.0);
  //fails to return a nil array in VPR ver 1.18
  //corrected by changing line 589 to ...
  //while (H >= 0) and (Normals[H].X = 0) and (Normals[H].Y = 0) do Dec(H);

  //also, with those changes (back to a prior state) the algorithm functions
  //properly IMHO and also removes the need for the added conditional test in
  //line 592 - 'and (L < High(Normals))'.
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  pts: TArrayOfFLoatPoint;
  linewidth: tfloat;
begin
  Test1BuildPolyLine;
  Test2BuildPolyLine;

  //rounded end is broken ...
//  pts := MakeArrayOfFloatPoints([ 100, 100,   200, 100,   200, 100 ]);
  lineWidth := 30;
//  PolylineFS(Img.Bitmap, pts, $40000099, false, lineWidth,
//  jsRound, esRound);


        //rounded end is broken ...
{         pts := MakeArrayOfFloatPoints([ 100, 100,   200, 100,   200, 100 ]);
        PolylineFS(Img.Bitmap, pts, $40000099, false, lineWidth,
jsRound, esRound); }


{         pts := MakeArrayOfFloatPoints([150,150,  300,150,  300,300, 150,300, 150,150]);
        lineWidth := 200; //joinStyle := jsBevel; endStyle := esButt;

        PolylineFS(img.Bitmap, pts, $40000099, true, lineWidth, jsBevel, esButt); }


        //an edge case problem with no easy solution, but it's still something to think about ...
        pts := MakeArrayOfFloatPoints([150,150,  260,150,  300,250,  340,150,  450,150]);
        lineWidth := 100; //joinStyle := jsRound; endStyle := esRound;
        PolylineFS(img.Bitmap, pts, $40000099, false, lineWidth, jsBevel, esRound);
end;
end.
