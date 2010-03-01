{
GPC-Test is public domain software written by Richard B. Winston
(rbwinst@usgs.gov).
It relies on GPC (http://www.cs.man.ac.uk/~toby/alan/software/ ,
http://www.cs.man.ac.uk/~toby/alan/software/gpc.html)
and Graphics32 (http://graphics32.org).

It also uses TQRbwZoomBox2 which is a public domain custom component
included with the source code of GPC-Test.

To compile GPC-Test first, you must first install Graphics32
and then TQRbwZoomBox2 in Delphi.

Neither Graphics32 nor GPC are in the public domain. Graphics32 is covered
by the MPL 1.1 license. GPC is free for non-commercial use.

Change log:
 27 February 2010, Mattias Andersson (mattias@centaurix.com):
  - Project updated for VPR polygon rasterizer (http://vpr.sourceforge.net);
  - Removed ZoomBox and TjvFileEdit dependencies.
}

unit frmMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, GR32, GR32_Layers, GR32_PolygonsEx,
  GR32_Polygons, GPC, Mask, XPMan, GR32_VectorUtils, GR32_Image;

type
  TfrmMain = class(TForm)
    rgClipOperation: TRadioGroup;
    rgOutputType: TRadioGroup;
    Image32: TImage32;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgClipOperationClick(Sender: TObject);
    procedure rgOutputTypeClick(Sender: TObject);
    procedure Image32PaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure Image32Resize(Sender: TObject);
  private
    Polygon1: Tgpc_polygon;
    Polygon2: Tgpc_polygon;
    ResultPolygon: Tgpc_polygon;
    Tristrip: Tgpc_tristrip;
    FDrawing: Boolean;
    FShouldUpdate: Boolean;
    // Display code
    procedure DrawContours(Dst: TBitmap32);
    procedure DrawTristrip(Dst: TBitmap32);

    procedure ReadPolygon(var Polygon: Tgpc_polygon; FileName: string);
    procedure GetMinMax(Polygon: Tgpc_polygon; var FirstFound: Boolean;
      var MaxY, MinY, MaxX, MinX: TFloat);
    procedure UpdateScale;
    procedure UpdateClipOperation;
    procedure ReadFiles;
    function TriStripToPolyPolygon(
      Tristrip: Tgpc_tristrip): TArrayOfArrayOfFloatPoint;
    procedure DrawAPolygon(Dst: TBitmap32; APolygon: Tgpc_polygon;
      FillColor: TColor32; Stipple: Boolean);
    { Private declarations }
  public
    { Public declarations }
    MaxY: TFloat;
    MinY: TFloat;
    MaxX: TFloat;
    MinX: TFloat;
    ScaleX, ScaleY: TFloat;
    { convert to 'device' coordinates }
    function LPtoDP(const P: TFloatPoint): TFloatPoint;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Math, BigCanvasMethods;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  with Image32.PaintStages.Add^ do
  begin
    Stage := PST_CUSTOM;
    RunTime := True;
  end;

  ReadFiles;
  Image32.Invalidate;
end;

procedure TfrmMain.ReadFiles;
var
  Dir, FileA, FileB: string;
begin
  Dir := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
  FileA := Dir + 'britain.gpf';
  FileB := Dir + 'arrows.gpf';

  if FileExists(FileA) and FileExists(FileB) then
  begin
    ReadPolygon(Polygon1, FileA);
    ReadPolygon(Polygon2, FileB);
    UpdateClipOperation;
    UpdateScale;
  end;
end;


procedure TfrmMain.ReadPolygon(var Polygon: Tgpc_polygon; FileName: string);
const
  FFalse = 0;
var
  AFile: TextFile;
begin
  AssignFile(AFile, FileName);
  try
    Reset(AFile);
    gpc_free_polygon(@Polygon);
    gpc_read_polygon(AFile, FFalse, @Polygon);
  finally
    CloseFile(AFile);
  end;
end;

procedure TfrmMain.UpdateClipOperation;
begin
  case rgOutputType.ItemIndex of
    0:
      begin
        gpc_free_polygon(@ResultPolygon);
        gpc_polygon_clip(Tgpc_op(rgClipOperation.ItemIndex), @Polygon1, @Polygon2, @ResultPolygon);
      end;
    1:
      begin
        gpc_free_tristrip(@Tristrip);
        gpc_tristrip_clip(Tgpc_op(rgClipOperation.ItemIndex), @Polygon1, @Polygon2, @Tristrip);
      end;
  end;
  Image32.Invalidate;
end;

procedure TfrmMain.UpdateScale;
var
  FirstFound: Boolean;
  Dx, Dy: TFloat;
begin
  FirstFound := False;
  MaxY := 0;
  MinY := 0;
  MaxX := 0;
  MinX := 0;
  GetMinMax(Polygon1, FirstFound, MaxY, MinY, MaxX, MinX);
  GetMinMax(Polygon2, FirstFound, MaxY, MinY, MaxX, MinX);
  Dx := MaxX - MinX;
  Dy := MaxY - MinY;
  MinX := MinX - Dx*0.05;
  MaxX := MaxX + Dx*0.05;
  MinY := MinY - Dy*0.05;
  MaxY := MaxY + Dy*0.05;
end;

procedure TfrmMain.GetMinMax(Polygon: Tgpc_polygon; var FirstFound: Boolean;
  var MaxY, MinY, MaxX, MinX: TFloat);
var
  AVertex: Tgpc_vertex;
  VertexIndex: Integer;
  ContourIndex: Integer;
begin
  for ContourIndex := 0 to Polygon.num_contours - 1 do
  begin
    with Polygon.contour[ContourIndex] do
    begin
      for VertexIndex := 0 to num_vertices - 1 do
      begin
        AVertex := vertex[VertexIndex];
        if FirstFound then
        begin
          MinX := Min(AVertex.x, MinX);
          MaxX := Max(AVertex.x, MaxX);
          MinY := Min(AVertex.y, MinY);
          MaxY := Max(AVertex.y, MaxY);
        end
        else
        begin
          FirstFound := True;
          MinX := AVertex.x;
          MaxX := AVertex.x;
          MinY := AVertex.y;
          MaxY := AVertex.y;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.Image32PaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
begin
  Buffer.BeginUpdate;
  try
    DrawContours(Buffer);
  finally
    Buffer.EndUpdate;
  end;
end;

procedure TfrmMain.Image32Resize(Sender: TObject);
var
  W: TFloat;
begin
  W := Min(Image32.Width, Image32.Height);
  ScaleX := W / (MaxX - MinX);
  ScaleY := W / (MaxY - MinY);
end;

function TfrmMain.LPtoDP(const P: TFloatPoint): TFloatPoint;
begin
  Result.X := (P.X - MinX) * ScaleX;
  Result.Y := (MaxY - P.Y) * ScaleY;
end;

(*
function MakePolygon(APolygon: Tgpc_polygon): TArrayOfArrayOfFloatPoint;
var
  I: Integer;
  VertexIndex: Integer;
begin
  SetLength(Result, APolygon.num_contours);
  for I := 0 to APolygon.num_contours - 1 do
  begin
    with APolygon.contour[ContourIndex] do
    begin
      SetLength(Result[I], num_vertices);
      for VertexIndex := 0 to num_vertices - 1 do
      begin
        Result[I][VertexIndex] := LPtoDP(Vertex[VertexIndex]);
      end;
    end;
  end;
end;
*)

procedure TfrmMain.DrawAPolygon(Dst: TBitmap32; APolygon: Tgpc_polygon; FillColor: TColor32; Stipple: Boolean);
var
  ContourIndex: Integer;
  Points: TArrayOfFloatPoint;
  VertexIndex,i: Integer;
  AVertex: Tgpc_vertex;
  ColorUsed: TColor32;
begin
  for ContourIndex := 0 to APolygon.num_contours - 1 do
  begin
    with APolygon.contour[ContourIndex] do
    begin
      SetLength(Points, num_vertices);

      for VertexIndex := 0 to num_vertices - 1 do
      begin
        AVertex := Vertex[VertexIndex];
        Points[VertexIndex] := LPtoDP(AVertex);
      end;

      if (APolygon.hole <> nil) and (APolygon.hole[ContourIndex] = 1) then
        ColorUsed := clWhite32
      else
        ColorUsed := FillColor;

      if ColorUsed <> clTransparent32 then
        PolygonFS(Dst, Points, ColorUsed);

      if Stipple then
      begin
        Points := ClosePolygon(Points);
        PolyPolyLineFS(Dst, BuildDashedLine(Points, [10, 10]), clBlack32, False, 0.2);
      end
      else
        PolyLineFS(Dst, Points, clBlack32, True, 0.2);
    end;
  end;
end;

procedure TfrmMain.DrawContours(Dst: TBitmap32);
begin
  // draw a box around the drawing area
  DrawBigRectangle32(Dst, clBlack32, clWhite32, 1.0, 0, 0,
    Image32.ClientWidth - 1, Image32.ClientHeight - 1);

  // draw the result of the clipping operation.
  case rgOutputType.ItemIndex of
    0: DrawAPolygon(Dst, ResultPolygon, $ffcc99ff, False);
    1: DrawTristrip(Dst);
  end;

  // Draw the polygons.
  DrawAPolygon(Dst, Polygon1, clTransparent32, False);
  DrawAPolygon(Dst, Polygon2, clTransparent32, True);
end;

function TfrmMain.TriStripToPolyPolygon(Tristrip: Tgpc_tristrip): TArrayOfArrayOfFloatPoint;
var
  Count: Integer;
  V3: Tgpc_vertex;
  V2: Tgpc_vertex;
  V1: Tgpc_vertex;
  VertexIndex: Integer;
  Strip: Tgpc_vertex_list;
  StripIndex: Integer;
begin
  Count := 0;
  for StripIndex := 0 to Tristrip.num_strips - 1 do
    Inc(Count, Tristrip.Strip[StripIndex].num_vertices - 2);
  SetLength(Result, Count, 4);
  Count := 0;
  for StripIndex := 0 to Tristrip.num_strips - 1 do
  begin
    Strip := Tristrip.Strip[StripIndex];

    for VertexIndex := 0 to Strip.num_vertices - 3 do
    begin
      // Note: reverse the orientation of every second triangle in order to
      //       avoid positive/negative weights to cancel out at edges in VPR.
      if Odd(VertexIndex) then
      begin
        V1 := Strip.vertex[VertexIndex];
        V2 := Strip.vertex[VertexIndex + 1];
        V3 := Strip.vertex[VertexIndex + 2];
      end
      else
      begin
        V1 := Strip.vertex[VertexIndex];
        V3 := Strip.vertex[VertexIndex + 1];
        V2 := Strip.vertex[VertexIndex + 2];
      end;

      Result[Count][0] := LPtoDP(V1);
      Result[Count][1] := LPtoDP(V2);
      Result[Count][2] := LPtoDP(V3);
      Result[Count][3] := Result[Count][0];

      Inc(Count);
    end;
  end;
end;

procedure TfrmMain.DrawTristrip(Dst: TBitmap32);
var
  Points: TArrayOfArrayOfFloatPoint;
begin
  Points := TriStripToPolyPolygon(Tristrip);
  PolyPolygonFS(Dst, Points, $ffccff99, pfWinding);
end;

procedure TfrmMain.rgClipOperationClick(Sender: TObject);
begin
  UpdateClipOperation;
end;

procedure TfrmMain.rgOutputTypeClick(Sender: TObject);
begin
  UpdateClipOperation;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  gpc_free_polygon(@Polygon1);
  gpc_free_polygon(@Polygon2);
  gpc_free_polygon(@ResultPolygon);
  gpc_free_tristrip(@Tristrip);
end;

end.
