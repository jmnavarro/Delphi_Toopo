unit EstadisticasIndexacion;

interface

uses classes;

type
	TEstadisticasIndexacion = class(TObject)
	private
		FTiempoTotal: Longword;

		FNumeroDocumentos: Longword;
		FNumeroPalabrasIndexadas: Longword;
		FNumeroPalabrasVacias: Longword;
		FNumeroPalabrasDiccionario: Longword;
		FNumeroPalabrasVaciasEncontradas: Longword;
		FNumeroSinonimosEncontrados: Longword;

		FSizeInvertido: Longword;
		FSizePalabras: Longword;
		FSizeDocumentos: Longword;
		FSizePalabrasVacias: Longword;
		FSizeDiccionario: Longword;

		FMinLenPalabra: integer;
		FMaxLenPalabra: integer;
		FTotalLenPalabras: Int64;

		function GetTiempoMedioPorDocumento: LongWord;
		function GetTiempoMedioPorPalabra: LongWord;

	public
		constructor Create;

		procedure NuevoDocumentoIndexado(totalPalabras: Longword; tiempo: Longword);
		procedure CargadasPalabrasVacias(totalPalabras: Longword);
		procedure CargadoDiccionario(totalPalabras: Longword);
		procedure EncontradaPalabraVacia;
		procedure EncontradoSinonimo;
		procedure NuevaPalabraIndexada(palabra: PChar; len: integer);

		procedure SetSizeInvertido(value: LongWord);
		procedure SetSizePalabras(value: LongWord);
		procedure SetSizeDocumentos(value: LongWord);
		procedure SetSizePalabrasVacias(value: LongWord);
		procedure SetSizeDiccionario(value: Longword);

		procedure GetLog(log: TStrings);

		property TiempoMedioPorDocumento: LongWord read GetTiempoMedioPorDocumento;
		property TiempoMedioPorPalabra: LongWord read GetTiempoMedioPorPalabra;

		property SizeInvertido: LongWord read FSizeInvertido;
		property SizePalabras: LongWord read FSizePalabras;
		property SizeDocumentos: LongWord read FSizeDocumentos;
		property SizePalabrasVacias: LongWord read FSizePalabrasVacias;
		property SizeDiccionario: Longword read FSizeDiccionario;
	end;

implementation

uses SysUtils;


constructor TEstadisticasIndexacion.Create;
begin
	inherited;

	FMinLenPalabra := MaxInt;
end;

function TEstadisticasIndexacion.GetTiempoMedioPorDocumento: LongWord;
begin
	if FNumeroDocumentos > 0 then
		result := Round(FTiempoTotal/FNumeroDocumentos)
	else
		result := 0;
end;


function TEstadisticasIndexacion.GetTiempoMedioPorPalabra: LongWord;
begin
	if FNumeroPalabrasIndexadas > 0 then
		result := Round(FTiempoTotal/FNumeroPalabrasIndexadas)
	else
		result := 0;
end;


procedure TEstadisticasIndexacion.SetSizeInvertido(value: LongWord);
begin
	FSizeInvertido := value;
end;


procedure TEstadisticasIndexacion.SetSizePalabras(value: LongWord);
begin
	FSizePalabras := value;
end;


procedure TEstadisticasIndexacion.SetSizeDocumentos(value: LongWord);
begin
	FSizeDocumentos := value;
end;

procedure TEstadisticasIndexacion.SetSizePalabrasVacias(value: LongWord);
begin
	FSizePalabrasVacias := value;
end;

procedure TEstadisticasIndexacion.SetSizeDiccionario(value: Longword);
begin
	FSizeDiccionario := value;
end;

procedure TEstadisticasIndexacion.NuevoDocumentoIndexado(totalPalabras: Longword; tiempo: Longword);
begin
	Inc(FNumeroDocumentos);
	Inc(FNumeroPalabrasIndexadas, totalPalabras);
	Inc(FTiempoTotal, tiempo);
end;

procedure TEstadisticasIndexacion.CargadasPalabrasVacias(totalPalabras: Longword);
begin
	FNumeroPalabrasVacias := totalPalabras;
end;

procedure TEstadisticasIndexacion.CargadoDiccionario(totalPalabras: Longword);
begin
	FNumeroPalabrasDiccionario := totalPalabras;
end;

procedure TEstadisticasIndexacion.EncontradaPalabraVacia;
begin
	Inc(FNumeroPalabrasVaciasEncontradas);
end;

procedure TEstadisticasIndexacion.EncontradoSinonimo;
begin
	Inc(FNumeroSinonimosEncontrados);
end;

procedure TEstadisticasIndexacion.NuevaPalabraIndexada(palabra: PChar; len: integer);
begin
	if len > FMaxLenPalabra then
		FMaxLenPalabra := len;

	if len < FMinLenPalabra then
		FMinLenPalabra := len;

	Inc(FTotalLenPalabras, len);
end;

procedure TEstadisticasIndexacion.GetLog(log: TStrings);
var
	palabrasTotales: Longword;
	aux: Longword;

	function GetTiempoStr(tiempo: Longword): string;
	begin
		result := Format('%d ms.', [tiempo]);
	end;

	function GetSizeStr(size: Longword): string;
	begin
		result := Format('%d bytes.', [size]);
	end;
begin
	log.Clear;

	palabrasTotales := FNumeroPalabrasIndexadas + FNumeroPalabrasVaciasEncontradas;

	log.Add(Format('Número de documentos indexados: %d', [FNumeroDocumentos]));
	log.Add(Format('Número de palabras indexadas: %d', [FNumeroPalabrasIndexadas]));
	log.Add(Format('Número de palabras vacías: %d', [FNumeroPalabrasVacias]));
	log.Add(Format('Número de sinónimos en el diccionario: %d', [FNumeroPalabrasDiccionario]));
	log.Add(Format('Número de palabras vacías descartadas: %d (%5.2f%%)', [FNumeroPalabrasVaciasEncontradas, (FNumeroPalabrasVaciasEncontradas/palabrasTotales)*100]));
	log.Add(Format('Número de sinónimos sustituidos: %d (%5.2f%%)', [FNumeroSinonimosEncontrados, (FNumeroSinonimosEncontrados/palabrasTotales)*100]));

	log.Add(Format('Tamaño del índice invertido: %s', [GetSizeStr(FSizeInvertido)]));
	log.Add(Format('Tamaño del índice de palabras: %s', [GetSizeStr(FSizePalabras)]));
	log.Add(Format('Tamaño del índice de documentos: %s', [GetSizeStr(FSizeDocumentos)]));
	log.Add(Format('Tamaño del archivo de palabras vacías: %s', [GetSizeStr(FSizePalabrasVacias)]));
	log.Add(Format('Tamaño del diccionario de sinónimos: %s', [GetSizeStr(FSizeDiccionario)]));

	log.Add(Format('Tiempo de indexación total: %s', [GetTiempoStr(FTiempoTotal)]));
	log.Add(Format('Tiempo de indexación medio por documento: %s', [GetTiempoStr(TiempoMedioPorDocumento)]));
	log.Add(Format('Tiempo de indexación medio por palabra: %s', [GetTiempoStr(TiempoMedioPorPalabra)]));

	if FMinLenPalabra = MaxInt then
		aux := 0
	else
		aux := FMinLenPalabra;

	log.Add(Format('Longitud de la palabra más corta: %d', [aux]));
	log.Add(Format('Longitud de la palabra más larga: %d', [FMaxLenPalabra]));
	log.Add(Format('Longitud media de palabras indexadas: %d', [Round(FTotalLenPalabras/FNumeroPalabrasIndexadas)]));

end;

end.
