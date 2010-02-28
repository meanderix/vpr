program GpcTest;

uses
  Forms,
  frmMainUnit in 'frmMainUnit.pas' {frmMain},
  BigCanvasMethods in 'BigCanvasMethods.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
