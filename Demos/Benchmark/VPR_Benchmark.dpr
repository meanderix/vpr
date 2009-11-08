program VPR_Benchmark;

uses
  FastMM4,
  FastMove,
  Forms,
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.res}
begin
  Application.Initialize;
  Application.Title := 'GR32_PolygonEx Benchmark Tool';
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
