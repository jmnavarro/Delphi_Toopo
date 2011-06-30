unit Buscador;

interface

uses classes, IndiceInvertido, PalabrasVacias, DiccionarioSinonimos;

type
	TBuscador = class(TObject)
	private
		FVacias: TPalabrasVacias;
		FDiccionario: TDiccionarioSinonimos;
		FIndice: TIndiceInvertido;

		function GetNombreBiblioteca: string;
		function GetFechaIndexacion: string;
		function GetLongitudMinimaPalabra: integer;
		function GetSinonimos: TStrings;

		procedure ObtenerPalabras(texto: PChar; var palabras: TStringList);

	protected
		function CargarVacias(const carpeta: string): boolean;
		function CargarDiccionario(const carpeta: string): boolean;
		function CargarIndices(const carpeta: string): boolean;
		function LiberarIndices: boolean;

	public
		constructor Create(const carpeta: string);
		destructor Destroy; override;

		function Buscar(var query: string; var resultados: TList): integer;

		property NombreBiblioteca: string read GetNombreBiblioteca;
		property FechaIndexacion: string read GetFechaIndexacion;
		property LongitudMinimaPalabra: integer read GetLongitudMinimaPalabra;
		property Sinonimos: TStrings read GetSinonimos;
	end;


implementation

uses SysUtils, NombresFicheros, Parseador, Windows;


constructor TBuscador.Create(const carpeta: string);
var
	carpetaDatos: string;
begin
	inherited Create;

	FIndice := nil;

	carpetaDatos := IncludeTrailingPathDelimiter(carpeta);

	CargarIndices(carpetaDatos);
	CargarVacias(carpetaDatos);
	CargarDiccionario(carpetaDatos);
end;


destructor TBuscador.Destroy;
begin
	FDiccionario.Free;
	FVacias.Free;
	LiberarIndices;

	inherited;
end;


function TBuscador.CargarVacias(const carpeta: string): boolean;
begin
	result := (FVacias = nil);
	if result then
		FVacias := TPalabrasVacias.Create(carpeta + FICHERO_VACIAS);
end;


function TBuscador.CargarDiccionario(const carpeta: string): boolean;
begin
	result := (FDiccionario = nil);
	if result then
		FDiccionario := TDiccionarioSinonimos.Create(carpeta + FICHERO_DICCIONARIO);
end;


function TBuscador.CargarIndices(const carpeta: string): boolean;
begin
	result := (FIndice = nil);
	if result then
	begin
		FIndice := TIndiceInvertido.Create(carpeta);

		result := FIndice.Leer;
	end;
end;


function TBuscador.LiberarIndices: boolean;
begin
	result := (FIndice <> nil);
	if result then
		FIndice.Free;;
end;


procedure TBuscador.ObtenerPalabras(texto: PChar; var palabras: TStringList);
var
	ptr, fin, final: PChar;
	palabra: array[0..63] of char;
	copia: PChar;
	separadores: TConjuntoSeparadores;
	ignorar: boolean;
begin
	separadores := [#10, #13, #9, ' ', '.', ',', ';', ':', '?', '¿', '¡', '!', '(', ')', '[', ']', '{', '}', '+', '-', '—', '=', '/', '\', '"', '''', '“', '”', '*', '<', '>', '…', '@', '&'];

	copia := StrNew(texto);

	TParseador.Normalizar(copia);

	// quitar palabras vacias
	if FVacias <> nil then
		FVacias.QuitarPalabrasVacias(copia);

	palabras.BeginUpdate;
	try
		final := StrEnd(copia);
		ptr := copia;
		repeat
			ptr := TParseador.AvanzarMientrasCaracteres(ptr, final, separadores);
			fin := TParseador.AvanzarHastaCaracteres(ptr, final, separadores);

			if (ptr <> nil) and (ptr^ <> #0) then
			begin
				StrLCopy(palabra, ptr, fin - ptr);

				ignorar := false;

				// sustituir diccionario
				if FDiccionario <> nil then
				begin
					FDiccionario.SustituirSinonimo(palabra);
					ignorar := (palabra[0] = #0);
				end;

				if FVacias <> nil then
					ignorar := FVacias.EsPalabraVacia(palabra);

				if not ignorar then
				begin
					// aparentemente no funciona el Duplicates = dupIgnore, así que lo hago a mano
					if palabras.IndexOf(palabra) = -1 then
						palabras.Add(palabra);
				end;

				ptr := fin;
			end;
		until (ptr = nil) or (ptr^ = #0);
	finally
		palabras.EndUpdate;
	end;

	StrDispose(copia);
end;


function TBuscador.Buscar(var query: string; var resultados: TList): integer;
var
	palabras: TStringList;
	ptr: PChar;
	resultadosPorPalabra: TList;
	encontrados: TList;
	iPalabra, maxPalabra: integer;
	iEncontrados, maxEncontrados: integer;
	ruta: PChar;
	palabra: string;

	procedure ConstruirResultados;
	var
		iLista, maxLista: integer;
		iEncontrados, maxEncontrados: integer;
		listaActual: TList;
		idDoc: integer;

		function EstaEnResultados(buscar: PChar): boolean;
		var
			iRes, maxRes: integer;
		begin
			result := false;
			maxRes := resultados.Count - 1;
			iRes := 0;
			while (iRes <= maxRes) and (not result) do
			begin
				result := (resultados[iRes] = buscar);
				Inc(iRes);
			end;
		end;

		function EstaEnTodasLasListas(id: integer; ignorarNumLista: integer): boolean;
		var
			maxLista, iLista: integer;
			maxDoc, iDoc: integer;
			listaActual: TList;
			encontrado: boolean;
		begin
			result := true;

			maxLista := resultadosPorPalabra.Count - 1;
			iLista := 0;
			while (iLista <= maxLista) and result do
			begin
				if iLista <> ignorarNumLista then
				begin
					listaActual := resultadosPorPalabra[iLista];

					encontrado := false;
					maxDoc := listaActual.Count - 1;
					iDoc := 0;
					while (iDoc <= maxDoc) and (not encontrado)  do
					begin
						encontrado := (Integer(listaActual[iDoc]) = id);
						Inc(iDoc);
					end;

					// no ha encontrado el documento en esta lista: ya no está en todas
					if not encontrado then
						result := false;
				end;
				Inc(iLista);
			end;
		end;

	begin
		maxLista := resultadosPorPalabra.Count - 1;
		if maxLista >= 0 then
			for iLista:=0 to maxLista do
			begin
				listaActual := resultadosPorPalabra[iLista];
				maxEncontrados := listaActual.Count - 1;
				if maxEncontrados >= 0 then
					for iEncontrados:=0 to maxEncontrados do
					begin
						idDoc := Integer(listaActual[iEncontrados]);
						ruta := FIndice.GetPtrDocumento(idDoc);
						if (not EstaEnResultados(ruta)) and
						   EstaEnTodasLasListas(idDoc, iLista) then
						begin
							resultados.Add(ruta);
						end;
					end;
			end;
	end;
begin
	// separar las palabras de la consulta
	palabras := TStringList.Create;
	try
		GetMem(ptr, Length(query) + 3);
		StrCopy(ptr, ' ');
		StrPCopy(ptr+1, query);
		StrCat(ptr, ' ');

		ObtenerPalabras(ptr, palabras);

		FreeMem(ptr);

		query := palabras.CommaText;

		maxPalabra := palabras.Count - 1;
		if maxPalabra >= 0 then
		begin
			resultadosPorPalabra := TList.Create;
			try
				// busco los documentos de cada palabra buscada
				for iPalabra:=0 to maxPalabra do
				begin
					encontrados := TList.Create;
					palabra := palabras[iPalabra];

					FIndice.BuscarDocumentosPalabra(palabra, encontrados);

					resultadosPorPalabra.Add(encontrados);
				end;

				// si hay resultados encontrados
				maxEncontrados := resultadosPorPalabra.Count;
				if maxEncontrados > 0 then
				begin
					// fusiona todas las listas, eliminando repetidos
					ConstruirResultados;

					// libera las listas intermedias
					Dec(maxEncontrados);
					for iEncontrados:=0 to maxEncontrados do
					begin
						encontrados := resultadosPorPalabra[iEncontrados];
						encontrados.Free;
					end;
				end;

			finally
				resultadosPorPalabra.Free;
			end;
		end;
	finally
		palabras.Free;
	end;

	result := resultados.Count;
end;


function TBuscador.GetNombreBiblioteca: string;
begin
	result := FIndice.Nombre;
end;


function TBuscador.GetFechaIndexacion: string;
var f: TSystemTime;
begin
	f := FIndice.FechaGeneracion;
	result := Format('%.2d/%.2d/%.4d %.2d:%.2d', [f.wDay, f.wMonth, f.wYear, f.wHour, f.wMinute]);
end;


function TBuscador.GetLongitudMinimaPalabra: integer;
begin
	result := FIndice.LongitudPalabraMinima;
end;


function TBuscador.GetSinonimos: TStrings;
begin
	if FDiccionario <> nil then
		result := FDiccionario.Sinonimos
	else
		result := nil;
end;


end.
