program textdemo;

uses
  FastMM4,
  FastMove,
  Forms,
  MainUnit in 'MainUnit.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
