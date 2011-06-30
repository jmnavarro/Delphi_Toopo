unit GeneradorIndice;

interface


uses classes;


type
	TGeneradorIndice = class(TObject)
	private
		FDocumentos: TStringList;
		FPalabras: TStringList;
		FidDocumentos: TList;
		FidPalabras: TList;

	public
		constructor Create;
		destructor Destroy; override;

	  	function AgregarDocumento(const ruta: string): integer;
		function AgregarPalabra(const palabra: string): integer;
		function AgregarRelacion(idPal, idDoc: integer): integer;

		function ProcesarListaPalabras(const ruta: string; palabras: PChar): boolean;
	end;


implementation


uses SysUtils;

constructor TGeneradorIndice.Create;
begin
	inherited Create;

  	FDocumentos := TStringList.Create;
	FDocumentos.Duplicates := dupIgnore;

	FPalabras := TStringList.Create;
	FPalabras.Duplicates := dupIgnore;

	FidDocumentos := TList.Create;
	FidPalabras := TList.Create;
end;


destructor TGeneradorIndice.Destroy;
begin
  	FDocumentos.Free;
	FPalabras.Free;

	FidDocumentos.Free;
	FidPalabras.Free;

	inherited;
end;


function TGeneradorIndice.AgregarDocumento(const ruta: string): integer;
begin
	result := FDocumentos.IndexOf(ruta);
	if result = -1 then
		result := FDocumentos.Add(ruta);
end;


function TGeneradorIndice.AgregarPalabra(const palabra: string): integer;
begin
	result := FPalabras.IndexOf(palabra);
	if result = -1 then
		result := FPalabras.Add(palabra);
end;


function TGeneradorIndice.AgregarRelacion(idPal, idDoc: integer): integer;
begin
   FidPalabras.Add(Pointer(idPal));
   result := FidDocumentos.Add(Pointer(idDoc));
end;


function TGeneradorIndice.ProcesarListaPalabras(const ruta: string; palabras: PChar): boolean;
var
	idDoc: integer;
	idPal: integer;
	ptr, ptrFin: PChar;
	palabra: array[0..63] of char;
begin
	result := true;

	idDoc := AgregarDocumento(ruta);

	ptr := palabras;
	while ptr^ <> #0 do
	begin
		ptrFin := StrScan(ptr, ' ');
		StrLCopy(palabra, ptr, ptrFin - ptr);

		idPal := AgregarPalabra(palabra);
		AgregarRelacion(idPal, idDoc);

		ptr := ptrFin + 1;
	end;
end;


end.
