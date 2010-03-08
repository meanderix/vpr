unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  GR32_Image;

type
  TMainForm = class(TForm)
    Img: TImage32;
    procedure FormCreate(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  GR32, GR32_Math, GR32_VectorGraphics, GR32_PolygonsEx, GR32_VectorUtils;

procedure DrawSimplePolygon(Renderer: TPathRenderer; Cx, Cy, Rx, Ry: TFloat);
// Draw spiral shape
const
  ONE8TH : TFloat = 1 / 12;
  ONE200TH : TFloat = 1 / 400;
var
  I: Integer;
  S, C, Scale: TFloat;
begin
  Renderer.MoveTo(Cx, Cy);
  for I := 0 to 480 do
  begin
    SinCos(I * ONE8TH, S, C);
    Scale := I * ONE200TH;
    Renderer.LineTo(Cx + Rx * Scale * C, Cy + Ry * Scale * S);
  end;
end;

procedure PaintBitmap(Dst: TBitmap32);
var
  Renderer: TPathRenderer;
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
    PolylineFS(Dst, Dashed[I], clBlack32, False, 4);

  { (c) Render a path with an outline using a path renderer }
  Renderer := TPathRenderer.Create;
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
    Renderer.StrokeWidth := 5;
    DrawSimplePolygon(Renderer, 240, 240, 100, 100);
    Renderer.EndPath;
  finally
    Dst.Changed;
    Renderer.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Img.SetupBitmap(True, clWhite32);
  PaintBitmap(Img.Bitmap);
end;

end.
