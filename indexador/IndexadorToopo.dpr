program IndexadorToopo;

uses
  Forms,
  DiccionarioSinonimos in '..\engine\DiccionarioSinonimos.pas',
  AlmacenamientoIndice in '..\engine\AlmacenamientoIndice.pas',
  Indexador in '..\engine\Indexador.pas',
  IndiceInvertido in '..\engine\IndiceInvertido.pas',
  ListaEnFichero in '..\engine\ListaEnFichero.pas',
  MotorIndexacion in '..\engine\MotorIndexacion.pas',
  MotorIndexacionASCII in '..\engine\MotorIndexacionASCII.pas',
  MotorIndexacionFactory in '..\engine\MotorIndexacionFactory.pas',
  PalabrasVacias in '..\engine\PalabrasVacias.pas',
  Parseador in '..\engine\Parseador.pas',
  EstadisticasIndexacion in '..\engine\EstadisticasIndexacion.pas',
  MotorIndexacionHTML in '..\engine\MotorIndexacionHTML.pas',
  main in 'src\main.pas' {MainForm},
  AcercaDelIndexador in 'src\AcercaDelIndexador.pas' {AcercaDelIndexadorForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
