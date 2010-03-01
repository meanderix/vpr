program VPR_Benchmark;

uses
  FastMM4,
  FastMove,
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm};

{$R *.res}
begin
  Application.Initialize;
  Application.Title := 'GR32_PolygonEx Benchmark Tool';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

end.
