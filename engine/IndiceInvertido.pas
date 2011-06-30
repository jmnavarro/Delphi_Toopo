unit IndiceInvertido;

interface


uses classes, windows;


type
	TIndiceInvertido = class(TObject)
	private
		FCarpeta: string;

		FDocumentos: TStringList;
		FPalabras: TStringList;
		FidDocumentos: TList;
		FidPalabras: TList;

		FNombre: string;
		FLongitudPalabraMinima: integer;
		FFechaGeneracion: TSystemTime;

	public
		constructor Create(const carpeta: string; const nombre: string = '');
		destructor Destroy; override;

		function AgregarDocumento(const ruta: string): integer;
		function AgregarPalabra(const palabra: string): integer;
		function AgregarRelacion(idPal, idDoc: integer): integer;

		function AgregarListaPalabras(const ruta: string; palabras: PChar): LongWord;

		function Guardar(longitudPalabraMinima: integer): boolean;
		function Leer: boolean;

		function GetIdPalabra(palabra: string): integer;
		function GetIdDocumento(documento: string): integer;

		function GetPalabra(idPalabra: integer): string;
		function GetPtrPalabra(idPalabra: integer): PChar;
		function GetDocumento(idDocumento: integer): string;
		function GetPtrDocumento(idDocumento: integer): PChar;

		function BuscarDocumentosPalabra(palabra: string; IdDocs: TList): integer;
		function BuscarPalabrasDocumento(documento: string; IdPalabras: TList): integer;

		procedure GetSizes(var sizePalabras: Longword; var sizeDocumentos: Longword; var sizeIndice: Longword);

		property Nombre: string read FNombre;
		property FechaGeneracion: TSystemTime read FFechaGeneracion;
		property LongitudPalabraMinima: integer read FLongitudPalabraMinima;
	end;


implementation


uses SysUtils, AlmacenamientoIndice;


constructor TIndiceInvertido.Create(const carpeta: string; const nombre: string = '');
begin
	inherited Create;

	FCarpeta := carpeta;
	
	FDocumentos := TStringList.Create;
	FDocumentos.Duplicates := dupIgnore;

	FPalabras := TStringList.Create;
	FPalabras.Duplicates := dupIgnore;

	FidDocumentos := TList.Create;
	FidPalabras := TList.Create;

	FNombre := nombre;
	FLongitudPalabraMinima := 0;
end;


destructor TIndiceInvertido.Destroy;
begin
	FDocumentos.Free;
	FPalabras.Free;

	FidDocumentos.Free;
	FidPalabras.Free;

	inherited;
end;


function TIndiceInvertido.AgregarDocumento(const ruta: string): integer;
begin
	result := FDocumentos.IndexOf(ruta);
	if result = -1 then
	begin
		result := FDocumentos.Add(ruta);
		if result <> -1 then
			FDocumentos.Objects[result] := TObject(result);
	end;
end;


function TIndiceInvertido.AgregarPalabra(const palabra: string): integer;
begin
	result := FPalabras.IndexOf(palabra);
	if result = -1 then
		result := FPalabras.Add(palabra);
		if result <> -1 then
			FPalabras.Objects[result] := TObject(result);
end;


function TIndiceInvertido.AgregarRelacion(idPal, idDoc: integer): integer;
begin
	FidPalabras.Add(Pointer(idPal));
	result := FidDocumentos.Add(Pointer(idDoc));
end;


function TIndiceInvertido.AgregarListaPalabras(const ruta: string; palabras: PChar): LongWord;
var
	idDoc: integer;
	idPal: integer;
	ptr, ptrFin: PChar;
	palabra: array[0..63] of char;
	actual: integer;
begin
	idDoc := AgregarDocumento(ruta);

	actual := FPalabras.Count;

	FPalabras.BeginUpdate;
	try
		ptr := palabras;
		while (ptr <> nil) and (ptr^ <> #0) do
		begin
			ptrFin := StrScan(ptr, ' ');
			if ptrFin = nil then
				StrCopy(palabra, ptr)
			else
				StrLCopy(palabra, ptr, ptrFin - ptr);

			if palabra[0] <> #0 then
			begin
				idPal := AgregarPalabra(palabra);
				AgregarRelacion(idPal, idDoc);
			end;

			ptr := ptrFin;
			if ptr <> nil then
				Inc(ptr);
		end;
	finally
		FPalabras.EndUpdate;
	end;

	result := FPalabras.Count - actual;
end;


function TIndiceInvertido.Guardar(longitudPalabraMinima: integer): boolean;
var
	a: TAlmacenamientoIndice;
begin
	if FCarpeta <> '' then
	begin
		a := TAlmacenamientoIndice.Create(FCarpeta);
		try
			FLongitudPalabraMinima := longitudPalabraMinima;
			GetLocalTime(FFechaGeneracion);

			result := a.GuardarConfiguracion(FNombre, FLongitudPalabraMinima, FFechaGeneracion);
			if result then
			begin
				result := a.GuardarDocumentos(FDocumentos);
				if result then
				begin
					result := a.GuardarPalabras(FPalabras);
					if result then
						a.GuardarIndice(FidPalabras, FidDocumentos);
				end;
			end;
		finally
			a.Free;
		end;
	end
	else
		result := false;
end;


function TIndiceInvertido.Leer: boolean;
var
	a: TAlmacenamientoIndice;
begin
	if FCarpeta <> '' then
	begin
		a := TAlmacenamientoIndice.Create(FCarpeta);
		try
			result := a.LeerConfiguracion(FNombre, FLongitudPalabraMinima, FFechaGeneracion);
			if result then
			begin
				result := a.LeerDocumentos(FDocumentos);
				if result then
				begin
					result := a.LeerPalabras(FPalabras);
					if result then
						a.LeerIndice(FidPalabras, FidDocumentos);
				end;
			end;
		finally
			a.Free;
		end;
	end
	else
		result := false;
end;


function TIndiceInvertido.GetIdPalabra(palabra: string): integer;
var
	ind: integer;
begin
	ind := FPalabras.IndexOf(palabra);
	if ind = -1 then
		result := -1
	else
		result := Integer(FPalabras.Objects[ind]);
end;


function TIndiceInvertido.GetIdDocumento(documento: string): integer;
var
	ind: integer;
begin
	ind := FDocumentos.IndexOf(documento);
	if ind = -1 then
		result := -1
	else
		result := Integer(FDocumentos.Objects[ind]);
end;


function TIndiceInvertido.GetPalabra(idPalabra: integer): string;
var
	p: PChar;
begin
	p := GetPtrPalabra(idPalabra);
	if p <> nil then
		result := p;
end;


function TIndiceInvertido.GetPtrPalabra(idPalabra: integer): PChar;
var
	ind: integer;
begin
	result := nil;
	ind := FPalabras.IndexOfObject(TObject(idPalabra));
	if ind <> -1 then
		result := PChar(FPalabras.Strings[ind]);
end;


function TIndiceInvertido.GetDocumento(idDocumento: integer): string;
var
	p: PChar;
begin
	p := GetPtrDocumento(idDocumento);
	if p <> nil then
		result := p;
end;


function TIndiceInvertido.GetPtrDocumento(idDocumento: integer): PChar;
var
	ind: integer;
begin
	result := nil;
	ind := FDocumentos.IndexOfObject(TObject(idDocumento));
	if ind <> -1 then
		result := PChar(FDocumentos.Strings[ind]);
end;


function TIndiceInvertido.BuscarDocumentosPalabra(palabra: string; IdDocs: TList): integer;
var
	i, max: integer;
	idBuscado: integer;
begin
	// primero busca el identificador de la palabra buscada
	idBuscado := GetIdPalabra(palabra);

	// si la palabra está en el índice, construír la lista de documentos donde aparece
	if idBuscado <> -1 then
	begin
		max := FIdPalabras.Count - 1;

		if max >= 0 then
		begin
			for i:=0 to max do
			begin
				if idBuscado = Integer(FIdPalabras.Items[i]) then
				begin
					IdDocs.Add(FIdDocumentos.Items[i]);
				end;
			end;
		end;
	end;

	result := IdDocs.Count;
end;


function TIndiceInvertido.BuscarPalabrasDocumento(documento: string; IdPalabras: TList): integer;
var
	i, max: integer;
	idBuscado: integer;
begin
	// primero busca el identificador del documento buscado
	idBuscado := GetIdDocumento(documento);

	// si el documento está en el índice, construir la lista de palabras que contiene
	if idBuscado <> -1 then
	begin
		max := FIdDocumentos.Count - 1;

		if max >= 0 then
		begin
			for i:=0 to max do
			begin
				if idBuscado = Integer(FIdDocumentos.Items[i]) then
				begin
					IdPalabras.Add(FIdPalabras.Items[i]);
				end;
			end;
		end;
	end;

	result := IdPalabras.Count;
end;


procedure TIndiceInvertido.GetSizes(var sizePalabras: Longword; var sizeDocumentos: Longword; var sizeIndice: Longword);
var
	a: TAlmacenamientoIndice;
begin
	a := TAlmacenamientoIndice.Create(FCarpeta);
	try
		sizePalabras := a.SizePalabras;
		sizeDocumentos := a.SizeDocumentos;
		sizeIndice := a.SizeIndice;
	finally
		a.Free;
	end;
end;


end.


