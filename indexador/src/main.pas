unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Buttons, ComCtrls, ToolWin, StdCtrls;

type
  TMainForm = class(TForm)
	p_main: TPanel;
	p_central: TPanel;
	lista: TListView;
	Label2: TLabel;
	Bevel1: TBevel;
	b_aniadir: TButton;
	b_eliminar: TButton;
	Panel1: TPanel;
	Label1: TLabel;
	Bevel2: TBevel;
	Panel2: TPanel;
	pb: TProgressBar;
	b_indexar: TButton;
	od: TOpenDialog;
	Label4: TLabel;
	Panel3: TPanel;
    SpeedButton1: TSpeedButton;
	Label3: TLabel;
    Panel4: TPanel;
    SpeedButton2: TSpeedButton;
    e_vacias: TEdit;
    sb: TStatusBar;
    Label5: TLabel;
    Panel5: TPanel;
	SpeedButton3: TSpeedButton;
	e_carpeta: TEdit;
	e_diccionario: TEdit;
	OpenDialog1: TOpenDialog;
	Label6: TLabel;
	e_lenPalabra: TEdit;
    Button1: TButton;
    lb_estadisticas: TListBox;
    Label7: TLabel;
    e_nombre: TEdit;
    Bevel3: TBevel;
	procedure b_aniadirClick(Sender: TObject);
	procedure b_eliminarClick(Sender: TObject);
	procedure b_indexarClick(Sender: TObject);
	procedure SpeedButton2Click(Sender: TObject);
	procedure SpeedButton1Click(Sender: TObject);
	procedure SpeedButton3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
	function EstaEnLista(ruta: string): boolean;
	procedure MostrarEstadisticas(log: TStrings);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses Indexador, FileCtrl, AcercaDelIndexador;


function TMainForm.EstaEnLista(ruta: string): boolean;
var
	i: integer;
	nombre, carpeta: string;
	item: TListItem;
begin
	result := false;

	carpeta := ExtractFilePath(ruta);
	nombre :=  ExtractFileName(ruta);

	i := 0;
	while (i < lista.Items.Count) and (not result) do
	begin
		item := lista.Items[i];
		result := (item.Caption = nombre) and (item.SubItems[0] = carpeta);
		Inc(i);
	end;
end;


procedure TMainForm.MostrarEstadisticas(log: TStrings);
var
	i: integer;
begin
	lb_estadisticas.Clear;
	for i:=0 to Pred(log.Count) do
		lb_estadisticas.AddItem(log[i], nil);
end;


procedure TMainForm.b_aniadirClick(Sender: TObject);
var
	i: integer;
	carpeta, nombre: string;
	item: TListItem;
begin
	if od.Execute then
		for i:=Pred(od.Files.Count) downto 0 do
			if not EstaEnLista(od.Files[i]) then
			begin
				carpeta := ExtractFilePath(od.Files[i]);
				nombre :=  ExtractFileName(od.Files[i]);

				item := lista.Items.Add;

				item.Caption := nombre;
				item.SubItems.Add(carpeta);
			end;
end;

procedure TMainForm.b_eliminarClick(Sender: TObject);
begin
	lista.DeleteSelected;
end;

procedure TMainForm.b_indexarClick(Sender: TObject);
var
	indexador: TIndexador;
	i: integer;
	carpeta, nombre: string;
	log: TStringList;
begin
	if lista.items.count = 0 then
	begin
		MessageBox(Handle, 'No hay ningún archivo para indexar', 'Indexación', MB_ICONWARNING);
		exit;
	end;

	indexador := TIndexador.Create(e_carpeta.Text, e_nombre.Text, true);
	Screen.Cursor := crHourGlass;
	b_indexar.Hide;
	pb.Show;
	try
		indexador.FicheroVacias := e_vacias.Text;
		indexador.FicheroDiccionario := e_diccionario.Text;
		indexador.LongitudPalabraMinima := StrToInt(e_lenPalabra.Text);

		pb.Max := lista.Items.Count + 1;
		pb.Position := 0;

		for i:=0 to Pred(lista.Items.Count) do
		begin
			carpeta := lista.Items[i].SubItems[0];
			nombre  := lista.Items[i].Caption;

			sb.SimpleText := 'Indexando archivo ' + carpeta + nombre + '...';
			sb.Repaint;

			indexador.IndexarArchivo(carpeta + nombre);

			pb.Position := i+1;
		end;

		sb.SimpleText := 'Generando índices en ' + e_carpeta.Text + '...';
		sb.Repaint;

		if not indexador.GenerarIndices then
			MessageBox(Handle, PChar('Se ha producido un error intentando almacenar los índices en el disco.'#10#13'Asegúrate de que tienes permiso de escritura en la carpeta destino (y que no esté en un CD-ROM o un disquette protegido).'), 'Error generando índices', MB_ICONERROR);

		pb.Position := pb.Max;

		sb.SimpleText := '';
		sb.Repaint;

		log := TStringList.Create;
		try
			indexador.GetEstadisticasIndexacion(log);
			MostrarEstadisticas(log);
		finally
			log.Free;
		end;

		MessageBox(Handle, 'Indexación terminada correctamente.', 'Indexación', MB_ICONINFORMATION);

   finally
		pb.Hide;
		b_indexar.Show;
		Screen.Cursor := crDefault;
		indexador.Free;
   end;
end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
begin
	OpenDialog1.Title := 'Seleccionar un archivo de palabras vacías';
	if OpenDialog1.Execute then
		e_vacias.Text := OpenDialog1.FileName;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
	OpenDialog1.Title := 'Seleccionar un archivo de diccionario';
	if OpenDialog1.Execute then
		e_diccionario.Text := OpenDialog1.FileName;
end;

procedure TMainForm.SpeedButton3Click(Sender: TObject);
var dir: string;
begin
	dir := e_carpeta.Text;
	if SelectDirectory('Selecciona la carpeta donde se dejarán los índices:', '', dir) then
		e_carpeta.Text := dir;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
	e_vacias.Text := ExtractFilePath(Application.ExeName) + '..\..\colecciones\cuentos\config\vacias.txt';
	e_diccionario.Text := ExtractFilePath(Application.ExeName) + '..\..\colecciones\cuentos\config\dic.txt';
	e_carpeta.Text := ExtractFilePath(Application.ExeName) + '..\..\colecciones\cuentos\indices\';

	od.Filter := TIndexador.GetFiltroSoportados;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
	dlg: TAcercaDelIndexadorForm;
begin
	dlg := TAcercaDelIndexadorForm.Create(self);
	try
		dlg.ShowModal;
	finally
		dlg.Free;
	end;
end;

end.
