program tab2as32;

uses
  Forms,
  Utab2asp in 'UTAB2ASP.PAS' {Tab2AspForm};

{$R *.RES}

begin
  Application.Title := 'Tab to aspect converter';
  Application.CreateForm(TTab2AspForm, Tab2AspForm);
  Application.Run;
end.
