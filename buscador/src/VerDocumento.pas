unit VerDocumento;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TVerDocumentoForm = class(TForm)
	Label1: TLabel;
	lv_palabras: TListBox;
    lv_sinonimos: TListBox;
	Label2: TLabel;
	Memo1: TMemo;
	SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Bevel1: TBevel;
    Label3: TLabel;
    Label4: TLabel;
	Edit1: TEdit;
	SpeedButton3: TSpeedButton;
	procedure lv_palabrasClick(Sender: TObject);
	procedure SpeedButton1Click(Sender: TObject);
	procedure SpeedButton2Click(Sender: TObject);
	procedure SpeedButton3Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
	FSinonimos: TStrings;
	texto: PChar;
	posActual: PChar;
	ultimoTexto: string;

	procedure CargarTexto(ruta: string);
	procedure CargarSinonimos(palabra: string);
	procedure Buscar(palabra: PChar);
  public
	function Mostrar(ruta: string; palabras: string; sinonimos: TStrings): boolean;
  end;

implementation

{$R *.dfm}

uses Parseador;


function TVerDocumentoForm.Mostrar(ruta: string; palabras: string; sinonimos: TStrings): boolean;
begin
	try
		Caption := 'Ver documento encontrado - ' + ruta;
		
		FSinonimos := sinonimos;
		lv_palabras.Items.CommaText := palabras;

		Screen.Cursor := crHourGlass;
		try
			CargarTexto(ruta);
		finally
			Screen.Cursor := crDefault;
		end;

		ShowModal;

		StrDispose(texto);

		result := true;
	except
		result := false;
	end;
end;


procedure TVerDocumentoForm.CargarTexto(ruta: string);
begin
	Memo1.Lines.LoadFromFile(ruta);

	texto := StrNew(Memo1.Lines.GetText);
	TParseador.Normalizar(texto);
end;


procedure TVerDocumentoForm.lv_palabrasClick(Sender: TObject);
var
	p: string;
	lista: TListBox;
begin
	lista := Sender as TListBox;

	if lista.ItemIndex <> -1 then
	begin
		p := lista.Items[lista.ItemIndex];
		if lista = lv_palabras then
			CargarSinonimos(p);

		posActual := texto;
		Buscar(PChar(p));
	end;
end;


procedure TVerDocumentoForm.Buscar(palabra: PChar);
var
	p: PChar;
	msg: PChar;
	len: integer;
begin
	p := StrPos(posActual, palabra);
	if p = nil then
	begin
		if posActual = texto then
			msg := 'No se han encontrado esta palabra en el texto. Es posible que aparazca como un sinónimo.'
		else
			msg := 'No se han encontrado más ocurrencias de esta palabra en el texto.';
		MessageBox(Handle, msg, 'Palabra no encontrada', MB_ICONWARNING)
	end
	else
	begin
		len := StrLen(palabra);
		Memo1.SelStart := p - texto;
		Memo1.SelLength := len;
		Memo1.SetFocus;
		posActual := p + len;
	end;
end;


procedure TVerDocumentoForm.SpeedButton1Click(Sender: TObject);
begin
	if lv_palabras.ItemIndex <> -1 then
		Buscar(PChar(lv_palabras.Items[lv_palabras.ItemIndex]));
end;


procedure TVerDocumentoForm.CargarSinonimos(palabra: string);
var
	i: integer;
	copia: PChar;
	actual: array[0..63] of char;

	procedure QuitarClave(palabra: PChar);
	var p: PChar;
	begin
		p := StrScan(palabra, '=');
		if p <> nil then
			StrCopy(palabra, p+1);
	end;
begin
	lv_sinonimos.Clear;

	copia := StrNew(PChar(palabra));

	TParseador.Normalizar(copia);
	for i := Pred(FSinonimos.Count) downto 0 do
	begin
		StrPCopy(actual, FSinonimos[i]);
		QuitarClave(actual);
		TParseador.Normalizar(actual);

		if StrComp(actual, copia) = 0 then
			lv_sinonimos.AddItem(FSinonimos.Names[i], nil);
	end;

	StrDispose(copia);
end;


procedure TVerDocumentoForm.SpeedButton2Click(Sender: TObject);
begin
	if lv_sinonimos.ItemIndex <> -1 then
		Buscar(PChar(lv_sinonimos.Items[lv_sinonimos.ItemIndex]));
end;

procedure TVerDocumentoForm.SpeedButton3Click(Sender: TObject);
begin
	if ultimoTexto <> Edit1.Text then
	begin
		ultimoTexto := Edit1.Text;
		posActual := texto;
		Buscar(PChar(Edit1.Text));
	end;
end;

procedure TVerDocumentoForm.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_RETURN then
		SpeedButton3Click(Sender);
end;

end.
