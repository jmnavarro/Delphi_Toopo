unit MotorIndexacion;


interface

uses PalabrasVacias, DiccionarioSinonimos, EstadisticasIndexacion, Parseador;

type
	TMotorIndexacion = class(TObject)
	private
		FVacias: TPalabrasVacias;
		FDiccionario: TDiccionarioSinonimos;
		FEstadisticas: TEstadisticasIndexacion;

		FSeparadores: TConjuntoSeparadores;

		FLongitudMinima: integer;

	protected
		FPalabrasIndexadas: PChar;

		function GetPalabrasIndexadas: PChar;

		procedure AplicarDiccionarioSinonimos(palabra: PChar; var newLen: integer);
		function EsPalabraVacia(palabra: PChar): boolean;

		function EsSeparador(const chr: char): boolean;

		property LongitudMinima: integer read FLongitudMinima;
		property Estadisticas: TEstadisticasIndexacion read FEstadisticas;
		property Separadores: TConjuntoSeparadores read FSeparadores write FSeparadores;

	public
		constructor Create(ficheroVacias, ficheroDiccionario: string; lenMinima: integer; estadisticas: TEstadisticasIndexacion = nil); virtual;
		destructor Destroy; override;

		function Indexar(const datos: Pointer; lenDatos: LongWord): boolean; virtual; abstract;

		property PalabrasIndexadas: PChar read GetPalabrasIndexadas;
	end;


implementation

uses SysUtils;



constructor TMotorIndexacion.Create(ficheroVacias, ficheroDiccionario: string; lenMinima: integer; estadisticas: TEstadisticasIndexacion = nil);
begin
	inherited Create;

	if ficheroVacias <> '' then
	begin
		FVacias := TPalabrasVacias.Create(ficheroVacias);
		if estadisticas <> nil then
			estadisticas.CargadasPalabrasVacias(FVacias.NumeroPalabras);
	end
	else
		FVacias := nil;

	if ficheroDiccionario <> '' then
	begin
		FDiccionario := TDiccionarioSinonimos.Create(ficheroDiccionario);
		if estadisticas <> nil then
			estadisticas.CargadoDiccionario(FDiccionario.NumeroPalabras);
	end
	else
		FDiccionario := nil;

	FPalabrasIndexadas := nil;

	FLongitudMinima := lenMinima;
	FEstadisticas := estadisticas;
end;


destructor TMotorIndexacion.Destroy;
begin
	if FPalabrasIndexadas <> nil then
		FreeMem(FPalabrasIndexadas);

	if FVacias <> nil then
		FVacias.Free;

	if FDiccionario <> nil then
		FDiccionario.Free;

	inherited;
end;


function TMotorIndexacion.GetPalabrasIndexadas: PChar;
begin
	result := FPalabrasIndexadas;
	Inc(result);
end;


procedure TMotorIndexacion.AplicarDiccionarioSinonimos(palabra: PChar; var newLen: integer);
begin
	if FDiccionario <> nil then
	begin
		if FDiccionario.SustituirSinonimo(palabra) then
		begin
			newLen := StrLen(palabra);
			if FEstadisticas <> nil then
				FEstadisticas.EncontradoSinonimo;
		end;
	end;
end;


function TMotorIndexacion.EsPalabraVacia(palabra: PChar): boolean;
begin
	if FVacias <> nil then
		result := FVacias.EsPalabraVacia(palabra)
	else
		result := false;

	if result and (FEstadisticas <> nil) then
		FEstadisticas.EncontradaPalabraVacia;
end;


function TMotorIndexacion.EsSeparador(const chr: char): boolean;
begin
	result := (chr in FSeparadores);
end;


end.
