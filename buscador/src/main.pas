unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, Buscador;

const
	WM_POSTOPEN = WM_APP + 1;

type
  TMainForm = class(TForm)
	Panel2: TPanel;
	lv_resultados: TListBox;
	b_buscar: TButton;
	b_abrir: TButton;
	Label2: TLabel;
	e_buscado: TEdit;
	e_consulta: TComboBox;
	b_cargar: TButton;
    Button1: TButton;
    Panel1: TPanel;
    Bevel1: TBevel;
    Image1: TImage;
    Label3: TLabel;
	procedure b_cargarClick(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
	procedure b_buscarClick(Sender: TObject);
	procedure b_abrirClick(Sender: TObject);
	procedure lv_resultadosClick(Sender: TObject);
	procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
	FBuscador: TBuscador;
	procedure WMPostOpen(var msg:TMessage); message WM_POSTOPEN;
  public
	{ Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses ShellAPI, VerDocumento, AbrirBiblioteca, AcercaDelBuscador;

{$R *.dfm}

procedure TMainForm.b_cargarClick(Sender: TObject);
var
	dlg: TAbrirBibliotecaForm;
begin
	dlg := TAbrirBibliotecaForm.Create(Self);
	try
		if FBuscador <> nil then
			FBuscador.Free;

		FBuscador := dlg.AbrirBiblioteca;

		if FBuscador <> nil then
		begin
			b_buscar.Enabled := true;
			e_consulta.Enabled := true;
			e_consulta.Color := clWhite;
			lv_resultados.Color := clWhite;

			Caption := 'Buscador Toopo - ' + FBuscador.NombreBiblioteca;
		end;
	finally
		dlg.Free;
	end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
	if FBuscador <> nil then
		FBuscador.Free;
end;

procedure TMainForm.b_buscarClick(Sender: TObject);
var
	lista: TList;
	i, total: integer;
	r: PChar;
	query: string;
begin
	if FBuscador = nil then
		exit;

	lista := TList.Create;
	Screen.Cursor := crHourGlass;
	try
		lv_resultados.Clear;
		query := e_consulta.Text;
		total := FBuscador.Buscar(query, lista);

		if query = '' then
			e_buscado.Text := '(Sin palabras de búsqueda)'
		else
			e_buscado.Text := query;

		if total > 0 then
		begin
			lv_resultados.Items.BeginUpdate;
			try
				for i:=0 to Pred(total) do
				begin
					r := lista[i];
					lv_resultados.AddItem(r, nil);
				end;
			finally
				lv_resultados.Items.EndUpdate;
			end;
		end;
	finally
		Screen.Cursor := crDefault;
		lista.Free;
	end;
end;

procedure TMainForm.b_abrirClick(Sender: TObject);
var
	dlg: TVerDocumentoForm;
begin
	if lv_resultados.ItemIndex <> -1 then
	begin
		dlg := TVerDocumentoForm.Create(Self);
		try
			dlg.Mostrar(lv_resultados.Items[lv_resultados.ItemIndex], e_buscado.Text, FBuscador.Sinonimos);
		finally
			dlg.Free;
		end;
	end;
end;

procedure TMainForm.lv_resultadosClick(Sender: TObject);
begin
	b_abrir.Enabled := lv_resultados.ItemIndex <> -1;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
	PostMessage(Handle, WM_POSTOPEN, 0, 0);
end;

procedure TMainForm.WMPostOpen(var msg:TMessage);
begin
	b_cargar.Click;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
	dlg: TAcercaDelBuscadorForm;
begin
	dlg := TAcercaDelBuscadorForm.Create(Self);
	try
		dlg.ShowModal;
	finally
		dlg.Free;
	end;
end;

end.
