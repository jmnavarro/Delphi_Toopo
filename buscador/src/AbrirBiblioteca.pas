unit AbrirBiblioteca;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, Buscador;

type
  TAbrirBibliotecaForm = class(TForm)
    Panel3: TPanel;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    e_biblioteca: TEdit;
    b_cargar: TButton;
    e_nombre: TEdit;
    e_fecha: TEdit;
    e_longitud: TEdit;
    b_aceptar: TButton;
    Button2: TButton;
    Bevel1: TBevel;
    Panel2: TPanel;
    Bevel2: TBevel;
    Label2: TLabel;
    Image1: TImage;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure b_cargarClick(Sender: TObject);
  private
	FBuscador: TBuscador;
  public
	function AbrirBiblioteca: TBuscador;
  end;

implementation

uses FileCtrl;

{$R *.dfm}


function TAbrirBibliotecaForm.AbrirBiblioteca: TBuscador;
begin
	if ShowModal = mrOk then
		result := FBuscador
	else
	begin
		result := nil;
		if FBuscador <> nil then
			FBuscador.Free;
	end;
end;


procedure TAbrirBibliotecaForm.SpeedButton1Click(Sender: TObject);
var
	dir: string;
begin
	dir := e_biblioteca.Text;
	if SelectDirectory('Selecciona la carpeta donde están los índices de la biblioteca digital:', '', dir) then
	begin
		e_biblioteca.Text := dir;
		b_cargar.Click;
	end;
end;


procedure TAbrirBibliotecaForm.FormCreate(Sender: TObject);
begin
	FBuscador := nil;
	e_biblioteca.Text := ExtractFilePath(Application.ExeName) + '..\..\colecciones\cuentos\indices\';
end;


procedure TAbrirBibliotecaForm.b_cargarClick(Sender: TObject);
begin
	Screen.Cursor := crHourGlass;
	try
		if FBuscador <> nil then
		begin
			FBuscador.Free;
			FBuscador := nil;
		end;

		try
			FBuscador := TBuscador.Create(e_biblioteca.Text);

			e_nombre.Text := FBuscador.NombreBiblioteca;
			e_fecha.Text := FBuscador.FechaIndexacion;
			e_longitud.Text := IntToStr(FBuscador.LongitudMinimaPalabra);

			b_aceptar.Enabled := true;
		except
			MessageBox(Handle, 'No se ha podido cargar los índices de la biblioteca digital.'#10#13'Es posible que la carpeta elegida no contenga los archivos correctos o que estén corruptos.'#10#13'Intenta seleccionar otra carpeta o bien volver a generar los índices.', 'Error cargando índices', MB_ICONERROR);
			b_aceptar.Enabled := false;

			if FBuscador <> nil then
				FBuscador.Free;
			FBuscador := nil;
		end;
	finally
		Screen.Cursor := crDefault;
	end;
end;

end.
