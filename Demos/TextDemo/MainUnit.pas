unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GR32_Image, ComCtrls, ExtCtrls, XPMan, GR32_Layers;

type
  TForm2 = class(TForm)
    FontDialog1: TFontDialog;
    Panel1: TPanel;
    Img: TImage32;
    Panel2: TPanel;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    tbGamma: TTrackBar;
    cbHinted: TCheckBox;
    PB: TPaintBox32;
    rgMethod: TRadioGroup;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbGammaChange(Sender: TObject);
    procedure cbHintedClick(Sender: TObject);
    procedure ImgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure rgMethodClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure RenderText;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  GR32, GR32_Polygons, { GR32_SVG,  }GR32_PathsEx, GR32_PolygonsEx;

const
  CRNL = #13#10;
  TSTRING =
  'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin sit amet nulla.' + CRNL +
  'Nam turpis nisl, faucibus ut, pulvinar eget, porta ac, lacus. Nam ultricies' + CRNL +
  'quam sed est. Mauris auctor nibh ut dui. Phasellus facilisis libero sit amet urna.' + CRNL +
  'Pellentesque non lorem. Donec aliquam, turpis in ornare placerat, risus justo' + CRNL +
  'rhoncus nibh, vitae commodo sem eros vitae massa. Donec tincidunt. Suspendisse' + CRNL +
  'potenti. Praesent sapien augue, fermentum in, aliquet et, vestibulum vel, neque.' + CRNL +
  'Vivamus diam. Suspendisse commodo odio non erat. Fusce ornare, ipsum et luctus' + CRNL +
  'eleifend, sapien lectus placerat ante, a posuere nibh risus nec quam. Pellentesque' + CRNL +
  'pretium. Etiam leo urna, gravida eu, pellentesque eu, imperdiet in, enim. Nam nunc.' + CRNL +
  'Quisque commodo.' + CRNL + CRNL +

  'In scelerisque. Mauris vitae magna. Curabitur tempor. Pellentesque condimentum.' + CRNL +
  'Maecenas molestie turpis sed arcu pulvinar malesuada. Morbi quis metus in leo' + CRNL +
  'vestibulum mollis. Ut libero arcu, molestie eget, tincidunt at, lobortis et,' + CRNL +
  'libero. Duis molestie venenatis magna. Nulla non ligula. Proin est. Curabitur nisl.' + CRNL +
  'Nulla facilisi. Nam dolor nulla, mollis non, tristique eu, vestibulum eget, mi.' + CRNL +
  'Donec venenatis, lacus adipiscing interdum laoreet, risus odio ullamcorper turpis,' + CRNL +
  'at feugiat pede neque ac dui.' + CRNL + CRNL +

  'Nulla quis dolor eget justo ullamcorper consectetur. Mauris in ante. Integer placerat' + CRNL +
  'dui at orci. Pellentesque at augue. Fusce a turpis. Aliquam tincidunt dolor ut augue.' + CRNL +
  'Quisque euismod mi ultrices mi. Sed pulvinar dolor sagittis mauris. Sed iaculis nisl' + CRNL +
  'sed orci. Sed massa nisl, porta a, blandit vel, ultrices quis, neque. Curabitur' + CRNL +
  'consequat urna id pede. Suspendisse sed metus.';

var
  Poly: TArrayOfArrayOfFloatPoint;

procedure TForm2.Button2Click(Sender: TObject);
begin
  if FontDialog1.Execute then
  begin
    Img.Bitmap.Font.Assign(FontDialog1.Font);
    Poly := TextToPolygonF(Img.Bitmap, 10, 10, TSTRING);    
    RenderText;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  SetGamma(0.88);
  Img.SetupBitmap(True, clWhite32);
  Img.Bitmap.Font.Name := 'Georgia';
  Img.Bitmap.Font.Size := 8;
  Img.Bitmap.Font.Style := [fsItalic];
  FontDialog1.Font.Assign(Img.Bitmap.Font);
  Poly := TextToPolygonF(Img.Bitmap, 10, 10, TSTRING);
  SetGamma(1);
  RenderText;
  PB.Buffer.SetSizeFrom(PB);
  PB.Buffer.Clear(clWhite32);
end;

procedure TForm2.ImgMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
const
  Delta = 16;
begin
  PB.Buffer.Draw(PB.Buffer.BoundsRect, Rect(X - Delta, Y - Delta, X + Delta, Y + Delta), Img.Bitmap);
  PB.Repaint;
end;

procedure TForm2.RenderText;
begin
  Img.SetupBitmap(True, clWhite32);
  case rgMethod.ItemIndex of
    0: PolyPolygonFS(Img.Bitmap, Poly, clBlack32, pfWinding);
    1: PolyPolygonFS_LCD(Img.Bitmap, Poly, clBlack32, pfWinding);
    2: PolyPolygonFS_LCD2(Img.Bitmap, Poly, clBlack32, pfWinding);
  end;
  with Img.ScreenToClient(Mouse.CursorPos) do
    ImgMouseMove(nil, [], X, Y, nil);
end;

procedure TForm2.rgMethodClick(Sender: TObject);
begin
  RenderText;
end;

procedure TForm2.tbGammaChange(Sender: TObject);
begin
  SetGamma(tbGamma.Position / 100);
  Label2.Caption := Format('(%1.2f)', [tbGamma.Position / 100]);
  RenderText;
end;

procedure TForm2.cbHintedClick(Sender: TObject);
const
  HVALUES: array [Boolean] of Integer = (GGO_NATIVE or GGO_UNHINTED, GGO_NATIVE);
begin
  GGODefaultFlags := HVALUES[cbHinted.Checked];
  Poly := TextToPolygonF(Img.Bitmap, 10, 10, TSTRING);  
  RenderText;
end;



end.