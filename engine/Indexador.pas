unit Indexador;

interface

uses classes, IndiceInvertido, EstadisticasIndexacion;

type
	TIndexador = class(TObject)
	private
		FCarpetaBD: string;
		FFicheroVacias: string;
		FFicheroDiccionario: string;

		FDatos: Pointer;
		FLenDatos: LongWord;
		FMap: THandle;

		FIndice: TIndiceInvertido;

		FLongitudPalabraMinima: integer;

		FEstadisticas: TEstadisticasIndexacion;

	protected
		function LeerArchivo(const archivo: string): boolean; virtual;
		procedure LiberarDatosArchivo(datos: Pointer); virtual;

	public
		constructor Create(const carpetaBD, nombreBD: string; calcularEstadisticas: boolean); virtual;
		destructor Destroy; override;

		function IndexarArchivo(const archivo: string): boolean; virtual;
		function GenerarIndices: boolean; virtual;

		procedure GetEstadisticasIndexacion(log: TStrings);

		class function GetFiltroSoportados: string;

		property CarpetaBD: string read FCarpetaBD;
		property FicheroVacias: string read FFicheroVacias write FFicheroVacias;
		property FicheroDiccionario: string read FFicheroDiccionario write FFicheroDiccionario;
		property LongitudPalabraMinima: integer read FLongitudPalabraMinima write FLongitudPalabraMinima;
	end;


//procedure IndexarCadena(ptr: PChar; fin: PChar; var resultado: PChar; var lenResultado: integer; var sizeResultado: integer);

implementation

uses Windows, MotorIndexacion, MotorIndexacionFactory, SysUtils
	{$IFDEF _DEBUG}
	, dialogs
	{$ENDIF}
	;


const
	LONGITUD_PALABRA_MINIMA = 3;


constructor TIndexador.Create(const carpetaBD, nombreBD: string; calcularEstadisticas: boolean);
begin
	inherited Create;

	FCarpetaBD := IncludeTrailingPathDelimiter(carpetaBD);
	FIndice := TIndiceInvertido.Create(FCarpetaBD, nombreBD);

	FLongitudPalabraMinima := LONGITUD_PALABRA_MINIMA;

	if calcularEstadisticas then
		FEstadisticas := TEstadisticasIndexacion.Create
	else
		FEstadisticas := nil;
end;


destructor TIndexador.Destroy;
begin
	FIndice.Free;

	if FEstadisticas <> nil then
		FEstadisticas.Free;

	inherited;
end;


function TIndexador.IndexarArchivo(const archivo: string): boolean;
var
	datos: Pointer;
	totalPalabras, lenDatos: LongWord;
	motor: TMotorIndexacion;
	t, t1: DWORD;
begin
	motor := TMotorIndexacionFactory.CreateMotorIndexacion(
											archivo, FFicheroVacias,
											FFicheroDiccionario,
											FLongitudPalabraMinima, FEstadisticas);
	if motor <> nil then
	begin
		totalPalabras := 0;

		t1 := GetTickCount;
		result := LeerArchivo(archivo);
		t := GetTickCount - t1;

		if result then
		begin
			datos := FDatos;
			lenDatos := FLenDatos;

			t1 := GetTickCount;
			result := motor.Indexar(datos, lenDatos);
			Inc(t, GetTickCount - t1);

			LiberarDatosArchivo(datos);

			if result then
			begin
{$IFDEF _DEBUG}
				ShowMessage(motor.PalabrasIndexadas);
{$ENDIF}
				t1 := GetTickCount;
				totalPalabras := FIndice.AgregarListaPalabras(archivo, motor.PalabrasIndexadas);
				Inc(t, GetTickCount - t1);
			end;
		end;

		motor.Free;

		if FEstadisticas <> nil then
			FEstadisticas.NuevoDocumentoIndexado(totalPalabras, t);
	end
	else
	begin
		// el tipo del archivo no está soportado por ningún motor de indexación
		result := true;
	end;
end;


function TIndexador.GenerarIndices: boolean;
var
	newFichero: string;
	sizeVacias, sizeDiccionario: DWORD;
	sizePalabras, sizeDocumentos, sizeInvertido: DWORD;

	function QuitarSoloLectura(fich: string): DWORD;
	var
		h: THandle;
		atr: DWORD;
	begin
		h := CreateFile(PChar(newFichero), 0, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
		result := GetFileSize(h, nil);
		CloseHandle(h);

		atr := GetFileAttributes(PChar(fich));
		if (atr <> $FFFFFFFF) and ((atr and FILE_ATTRIBUTE_READONLY) <> 0) then
		begin
			atr := atr and (not FILE_ATTRIBUTE_READONLY);
			SetFileAttributes(PChar(fich), atr);
		end;
	end;
begin
	result := FIndice.Guardar(FLongitudPalabraMinima);

	// también hay que copiar los archivos de diccionario y palabras vacías,
	// ya que serán necesarios durante la búsqueda
	if result and (FFicheroVacias <> '') then
	begin
		newFichero := FCarpetaBD + ExtractFileName(FFicheroVacias);
		sizeVacias := QuitarSoloLectura(newFichero);
		result := CopyFile(PChar(FFicheroVacias), PChar(newFichero), false);
	end
	else
		sizeVacias := 0;

	if result and (FFicheroDiccionario <> '') then
	begin
		newFichero := FCarpetaBD + ExtractFileName(FFicheroDiccionario);
		sizeDiccionario := QuitarSoloLectura(newFichero);
		result := CopyFile(PChar(FFicheroDiccionario), PChar(newFichero), false);
	end
	else
		sizeDiccionario := 0;

	if (FEstadisticas <> nil) and (FIndice <> nil) then
	begin
		FIndice.GetSizes(sizePalabras, sizeDocumentos, sizeInvertido);

		FEstadisticas.SetSizeInvertido(sizePalabras);
		FEstadisticas.SetSizePalabras(sizeDocumentos);
		FEstadisticas.SetSizeDocumentos(sizeInvertido);
		FEstadisticas.SetSizePalabrasVacias(sizeVacias);
		FEstadisticas.SetSizeDiccionario(sizeDiccionario);
	end;
end;


function TIndexador.LeerArchivo(const archivo: string): boolean;
var
	hFile: THandle;
begin
	result := false;

	hFile := CreateFile(PChar(archivo), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);

	if hFile <> INVALID_HANDLE_VALUE then
	begin
		FLenDatos := GetFileSize(hFile, nil);

		FMap := CreateFileMapping(hFile, nil, PAGE_READONLY, 0, 0, nil);

		if FMap <> 0 then
		begin
			FDatos := MapViewOfFile(FMap, FILE_MAP_READ, 0, 0, 0);

			result := (FDatos <> nil);
		end;

		CloseHandle(hFile);
	end;
end;


procedure TIndexador.LiberarDatosArchivo(datos: Pointer);
begin
	if datos <> FDatos then
		FreeMem(datos);

	FLenDatos := 0;
	UnmapViewOfFile(FDatos);
	CloseHandle(FMap);
	FMap := 0;
end;


procedure TIndexador.GetEstadisticasIndexacion(log: TStrings);
begin
	if (FEstadisticas <> nil) and (log <> nil) then
		FEstadisticas.GetLog(log);
end;


class function TIndexador.GetFiltroSoportados: string;
begin
	result := TMotorIndexacionFactory.GetFiltroSoportados;
end;

end.
