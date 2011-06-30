unit AlmacenamientoIndice;

interface

uses classes, windows;

type
	TAlmacenamientoIndice = class(TObject)
	private
		FCarpeta: string;

		function GetSizeIndice: Int64;
		function GetSizePalabras: Int64;
		function GetSizeDocumentos: Int64;

		function GetSizeFichero(fich: string): Int64;
		procedure QuitarSoloLectura(fich: string);

	public
		constructor Create(const carpeta: string);

		function Vaciar: boolean;

		function GuardarDocumentos(lista: TStrings): boolean;
		function GuardarPalabras(lista: TStrings): boolean;
		function GuardarIndice(listaIdPalabras: TList; listaIdDocumentos: TList): boolean;
		function GuardarConfiguracion(nombre: string; longitudPalabraMinima: integer; var st: TSystemTime): boolean;

		function LeerDocumentos(lista: TStrings): boolean;
		function LeerPalabras(lista: TStrings): boolean;
		function LeerIndice(listaIdPalabras: TList; listaIdDocumentos: TList): boolean;
		function LeerConfiguracion(var nombre: string; var longitudPalabraMinima: integer; var fecha: TSystemTime): boolean;

		property SizeIndice: Int64 read GetSizeIndice;
		property SizePalabras: Int64 read GetSizePalabras;
		property SizeDocumentos: Int64 read GetSizeDocumentos;
	end;


implementation

uses SysUtils;

const
	FICHERO_DOCUMENTOS	= 'docs.dat';
	FICHERO_PALABRAS	= 'words.dat';
	FICHERO_INDICE		= 'index.dat';
	FICHERO_CONFIG		= 'bd.conf';

type
	TDocumentoAlmacenado = record
	idDocumento: LongWord;
		ruta: array[0..255] of char;
	end;

	TPalabraAlmacenada = record
		idPalabra: LongWord;
		palabra: array[0..63] of char;
	end;

	TIndiceInvertido = record
		idPalabra: LongWord;
		idDocumento: LongWord;
	end;


constructor TAlmacenamientoIndice.Create(const carpeta: string);
begin
	inherited Create;

	FCarpeta := carpeta;
	ForceDirectories(FCarpeta);
end;


function TAlmacenamientoIndice.Vaciar: boolean;
const
	FICHEROS: array[0..2] of PChar = (FICHERO_DOCUMENTOS, FICHERO_PALABRAS, FICHERO_INDICE);
var
	i: integer;
	ruta: string;
	h: THandle;
begin
	result := true;
	for i:=Low(FICHEROS) to High(FICHEROS) do
	begin
		ruta := FCarpeta + FICHEROS[i];

		h := CreateFile(PChar(ruta), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
		if h = INVALID_HANDLE_VALUE then
			result := false
		else
			CloseHandle(h);
	end;
end;


function TAlmacenamientoIndice.GuardarDocumentos(lista: TStrings): boolean;
var
	h: THandle;
	id, i, max: integer;
	ruta: array[0..255] of char;
	escrito: DWORD;
begin
	result := false;

	max := lista.Count - 1;
	if max >= 0 then
	begin
		QuitarSoloLectura(FCarpeta + FICHERO_DOCUMENTOS);

		h := CreateFile(PChar(FCarpeta + FICHERO_DOCUMENTOS), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
		if h <> INVALID_HANDLE_VALUE then
		begin
			for i:=0 to max do
			begin
				id := LongWord(lista.Objects[i]);
				WriteFile(h, id, sizeof(id), escrito, nil);

				ZeroMemory(@ruta, sizeof(ruta));
				StrCopy(ruta, PChar(lista.Strings[i]));
				WriteFile(h, ruta, sizeof(ruta), escrito, nil);
			end;

			CloseHandle(h);
			result := true;
		end;
	end;
end;


function TAlmacenamientoIndice.GuardarPalabras(lista: TStrings): boolean;
var
	h: THandle;
	id, i, max: integer;
	palabra: array[0..63] of char;
	escrito: DWORD;
begin
	result := false;

	max := lista.Count - 1;
	if max >= 0 then
	begin
		QuitarSoloLectura(FCarpeta + FICHERO_PALABRAS);

		h := CreateFile(PChar(FCarpeta + FICHERO_PALABRAS), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
		if h <> INVALID_HANDLE_VALUE then
		begin
			for i:=0 to max do
			begin
				id := LongWord(lista.Objects[i]);
				WriteFile(h, id, sizeof(id), escrito, nil);

				ZeroMemory(@palabra, sizeof(palabra));
				StrCopy(palabra, PChar(lista.Strings[i]));
				WriteFile(h, palabra, sizeof(palabra), escrito, nil);
			end;

			CloseHandle(h);
			result := true;
		end;
	end;
end;


function TAlmacenamientoIndice.GuardarIndice(listaIdPalabras: TList; listaIdDocumentos: TList): boolean;
var
	h: THandle;
	i, max: integer;
	idPalabra, idDocumento: LongWord;
	escrito: DWORD;
begin
	result := false;

	max := listaIdPalabras.Count - 1;
	if max >= 0 then
	begin
		QuitarSoloLectura(FCarpeta + FICHERO_INDICE);

		h := CreateFile(PChar(FCarpeta + FICHERO_INDICE), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
		if h <> INVALID_HANDLE_VALUE then
		begin
			for i:=0 to max do
			begin
				idPalabra := LongWord(listaIdPalabras[i]);
				WriteFile(h, idPalabra, sizeof(idPalabra), escrito, nil);

				idDocumento := LongWord(listaIdDocumentos[i]);
				WriteFile(h, idDocumento, sizeof(idDocumento), escrito, nil);
			end;

			CloseHandle(h);
			result := true;
		end;
	end;
end;


function TAlmacenamientoIndice.GuardarConfiguracion(nombre: string; longitudPalabraMinima: integer; var st: TSystemTime): boolean;
var
	lista: TStringList;

	function FormatearFecha(f: TSystemTime): string;
	begin
		result := Format('%.2d/%.2d/%.4d', [f.wDay, f.wMonth, f.wYear]);
	end;

	function FormatearHora(f: TSystemTime): string;
	begin
		result := Format('%.2d:%.2d:%.2d,%.3d', [f.wHour, f.wMinute, f.wSecond, f.wMilliseconds]);
	end;

	function QuitarSoloLectura(fich: string): boolean;
	var
		atr: DWORD;
	begin
		atr := GetFileAttributes(PChar(fich));
		if (atr <> $FFFFFFFF) and ((atr and FILE_ATTRIBUTE_READONLY) <> 0) then
		begin
			atr := atr and (not FILE_ATTRIBUTE_READONLY);
			result := SetFileAttributes(PChar(fich), atr);
		end
		else
			result := true;
	end;

begin
	lista := TStringList.Create;
	try
		lista.Add('nombre=' + nombre);
		lista.Add('longitudPalabraMinima=' + IntToStr(longitudPalabraMinima));
		lista.Add('fecha=' + FormatearFecha(st));
		lista.Add('hora=' + FormatearHora(st));

		result := QuitarSoloLectura(FCarpeta + FICHERO_CONFIG);
		if result then
		begin
			try
				lista.SaveToFile(FCarpeta + FICHERO_CONFIG);
				result := true;
			except
				result := false;
			end;
		end;
	finally
		lista.Free;
	end;
end;


function TAlmacenamientoIndice.LeerDocumentos(lista: TStrings): boolean;
var
	h: THandle;
	id, size: integer;
	ruta: array[0..255] of char;
	leido: DWORD;
	terminado: boolean;
begin
	result := false;

	h := CreateFile(PChar(FCarpeta + FICHERO_DOCUMENTOS), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if h <> INVALID_HANDLE_VALUE then
	begin
		size := GetFileSize(h, nil);
		leido := Round(size / (sizeof(ruta) + sizeof(id)));
		if (leido > 0) and (DWORD(lista.capacity) < leido)then
			lista.capacity := leido;

		lista.BeginUpdate;
		try
			terminado := false;
			while not terminado do
			begin
				leido := 0;
				result := ReadFile(h, id, sizeof(id), leido, nil);

				terminado := result and (leido = 0);

				if not terminado then
				begin
					leido := 0;
					result := ReadFile(h, ruta, sizeof(ruta), leido, nil);

					lista.AddObject(ruta, TObject(id));

					terminado := result and (leido = 0);
				end;
			end;
		finally
			lista.EndUpdate;
		end;

		CloseHandle(h);
		result := true;
	end;
end;


function TAlmacenamientoIndice.LeerPalabras(lista: TStrings): boolean;
var
	h: THandle;
	id, size: integer;
	palabra: array[0..63] of char;
	leido: DWORD;
	terminado: boolean;
begin
	result := false;

	h := CreateFile(PChar(FCarpeta + FICHERO_PALABRAS), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if h <> INVALID_HANDLE_VALUE then
	begin
		size := GetFileSize(h, nil);
		leido := Round(size / (sizeof(palabra) + sizeof(id)));
		if (leido > 0) and (DWORD(lista.capacity) < leido)then
			lista.capacity := leido;

		lista.BeginUpdate;
		try
			terminado := false;
			while not terminado do
			begin
				leido := 0;
				result := ReadFile(h, id, sizeof(id), leido, nil);

				terminado := result and (leido = 0);

				if not terminado then
				begin
					leido := 0;
					result := ReadFile(h, palabra, sizeof(palabra), leido, nil);

					lista.AddObject(palabra, TObject(id));

					terminado := result and (leido = 0);
				end;
			end;
		finally
			lista.EndUpdate;
		end;

		CloseHandle(h);
		result := true;
	end;
end;


function TAlmacenamientoIndice.LeerIndice(listaIdPalabras: TList; listaIdDocumentos: TList): boolean;
var
	h: THandle;
	size: integer;
	idPalabra, idDocumento: LongWord;
	leido: DWORD;
	terminado: boolean;

	procedure AjustarSize(lista: TList; len: integer);
	var
		aux: integer;
	begin
		aux := Round(size / len);
		if (aux > 0) and (DWORD(lista.capacity) < leido)then
			lista.capacity := aux;
	end;
begin
	result := false;

	h := CreateFile(PChar(FCarpeta + FICHERO_INDICE), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if h <> INVALID_HANDLE_VALUE then
	begin
		size := GetFileSize(h, nil);

		AjustarSize(listaidPalabras, sizeof(idPalabra));
		AjustarSize(listaidDocumentos, sizeof(idDocumento));

		terminado := false;
		while not terminado do
		begin
			leido := 0;
			result := ReadFile(h, idPalabra, sizeof(idPalabra), leido, nil);

			terminado := result and (leido = 0);

			if not terminado then
			begin
				leido := 0;
				result := ReadFile(h, idDocumento, sizeof(idDocumento), leido, nil);

				listaIdPalabras.Add(TObject(idPalabra));
				listaIdDocumentos.Add(TObject(idDocumento));

				terminado := result and (leido = 0);
			end;
		end;

		CloseHandle(h);
		result := true;
	end;
end;


function TAlmacenamientoIndice.LeerConfiguracion(var nombre: string; var longitudPalabraMinima: integer; var fecha: TSystemTime): boolean;
var
	lista: TStringList;

	function ComponerFecha(fecha, hora: string): TSystemTime;
	begin
		result.wDay := StrToInt(Copy(fecha, 0, 2));
		result.wMonth := StrToInt(Copy(fecha, 4, 2));
		result.wYear := StrToInt(Copy(fecha, 8, 4));

		result.wHour := StrToInt(Copy(hora, 0, 2));
		result.wMinute := StrToInt(Copy(hora, 4, 2));
		result.wSecond := StrToInt(Copy(hora, 7, 2));
		result.wMilliseconds := StrToInt(Copy(hora, 10, 3));
	end;
begin
	result := FileExists(FCarpeta + FICHERO_CONFIG);

	if result then
	begin
		lista := TStringList.Create;
		try
			lista.LoadFromFile(FCarpeta + FICHERO_CONFIG);
			nombre := lista.Values['nombre'];
			longitudPalabraMinima := StrToInt(lista.Values['longitudPalabraMinima']);
			try
				fecha := ComponerFecha(lista.Values['fecha'], lista.Values['hora']);
			except
				result := false;
			end;
		finally
			lista.Free;
		end;
	end;
end;


function TAlmacenamientoIndice.GetSizeFichero(fich: string): Int64;
var
	h: THandle;
	sizeLow, sizeHigh: DWORD;
begin
	h := CreateFile(PChar(fich), 0, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
	try
		sizeLow := GetFileSize(h, @sizeHigh);
		if (sizeLow = $FFFFFFFF) and (GetLastError <> NO_ERROR) then
			result := 0
		else
			result := (sizeHigh shl 32) or sizeLow;
	finally
		CloseHandle(h);
	end;
end;


procedure TAlmacenamientoIndice.QuitarSoloLectura(fich: string);
var atr: DWORD;
begin
	atr := GetFileAttributes(PChar(fich));
	if (atr <> $FFFFFFFF) and ((atr and FILE_ATTRIBUTE_READONLY) <> 0) then
	begin
		atr := atr and (not FILE_ATTRIBUTE_READONLY);
		SetFileAttributes(PChar(fich), atr);
	end;
end;


function TAlmacenamientoIndice.GetSizeIndice: Int64;
begin
	result := GetSizeFichero(FCarpeta + FICHERO_INDICE);
end;

function TAlmacenamientoIndice.GetSizePalabras: Int64;
begin
	result := GetSizeFichero(FCarpeta + FICHERO_PALABRAS);
end;

function TAlmacenamientoIndice.GetSizeDocumentos: Int64;
begin
	result := GetSizeFichero(FCarpeta + FICHERO_DOCUMENTOS);
end;


end.
