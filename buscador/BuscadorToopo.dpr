program BuscadorToopo;

uses
  Forms,
  AlmacenamientoIndice in '..\engine\AlmacenamientoIndice.pas',
  IndiceInvertido in '..\engine\IndiceInvertido.pas',
  ListaEnFichero in '..\engine\ListaEnFichero.pas',
  PalabrasVacias in '..\engine\PalabrasVacias.pas',
  Parseador in '..\engine\Parseador.pas',
  Buscador in '..\engine\Buscador.pas',
  DiccionarioSinonimos in '..\engine\DiccionarioSinonimos.pas',
  NombresFicheros in '..\engine\NombresFicheros.pas',
  main in 'src\main.pas' {MainForm},
  VerDocumento in 'src\VerDocumento.pas' {VerDocumentoForm},
  AbrirBiblioteca in 'src\AbrirBiblioteca.pas' {AbrirBibliotecaForm},
  AcercaDelBuscador in 'src\AcercaDelBuscador.pas' {AcercaDelBuscadorForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
