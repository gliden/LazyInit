program Lazy;

uses
  Vcl.Forms,
  LazyMainFrm in 'LazyMainFrm.pas' {Form30},
  LazyInitLib in 'LazyInitLib.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm30, Form30);
  Application.Run;
end.
