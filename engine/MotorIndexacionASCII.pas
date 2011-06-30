//~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
//
// Unidad: MotorIndexacionASCII.pas
//
// Propósito:
//		La clase TMotorIndexacionASCII es un descendiente directo de TMotorIndexacion
//		por lo que permite obtener la lista de palabras a partir de un texto plano.
//		Esta clase debe crearse a partir de TMotorIndexacionFactory.
//
//
// Autor:          JM - http://www.lawebdejm.com
// Observaciones:  Creado en Delphi 6 para Todo Programación (www.iberprensa.com)
// Copyright:      Este código es de dominio público y se puede utilizar y/o
//						 mejorar siempre que SE HAGA REFERENCIA AL AUTOR ORIGINAL, ya
//						 sea a través de estos comentarios o de cualquier otro modo.
//
// Modificaciones:
//		JM		01/12/2004		Versión inicial
//		JM		15/12/2004		Refactorización "Pull Up Field": se lleva el atributo
//									"separadores" a la clase padre, rellenando su valor
//									en los constructores de las clases hijas.
//
//~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
unit MotorIndexacionASCII;

interface

uses MotorIndexacion, EstadisticasIndexacion;

type
	TMotorIndexacionASCII = class(TMotorIndexacion)
	public
		constructor Create(ficheroVacias, ficheroDiccionario: string; lenMinima: integer; estadisticas: TEstadisticasIndexacion = nil); override;

		function Indexar(const datos: Pointer; lenDatos: LongWord): boolean; override;
	end;


implementation


uses SysUtils, Parseador;


constructor TMotorIndexacionASCII.Create(ficheroVacias, ficheroDiccionario: string; lenMinima: integer; estadisticas: TEstadisticasIndexacion = nil);
begin
	Separadores := [#10, #13, #9, ' ', '.', ',', ';', ':', '?', '¿', '¡', '!', '(', ')', '[', ']', '{', '}', '+', '-', '—', '=', '/', '\', '"', '''', '“', '”', '*', '<', '>', '…', '@', '&'];
	inherited;
end;


function TMotorIndexacionASCII.Indexar(const datos: Pointer; lenDatos: LongWord): boolean;
var
	ptrIni, ptrFin: PChar;
	resultado: PChar;
	size: integer;
	lenResultado: integer;
	lenPalabra: integer;
	palabra: array[0..128] of char;
	PuntoFinal: PChar;

	procedure AmpliarResultado;
	var
		aux: PChar;
	begin
		size := size * 2;
		GetMem(aux, size);
		StrLCopy(aux, FPalabrasIndexadas, lenResultado);
		FreeMem(FPalabrasIndexadas);
		FPalabrasIndexadas := aux;
		resultado := FPalabrasIndexadas + lenResultado;
	end;

	function PalabraYaIndexada(listaPalabras: PChar; palabra: PChar; len: LongWord): boolean;
	var
		p: array[0..256] of char;
		ret: PChar;
	begin
		p[0] := ' ';
		StrLCopy(p+1, palabra, len);
		p[len+1] := ' ';
		p[len+2] := #0;

		ret := StrPos(listaPalabras, p);

		result := (ret <> nil);
	end;

begin
	result := (datos <> nil) and (lenDatos > 0);

	if result then
	begin
		PuntoFinal := PChar(LongWord(datos) + lenDatos);

		size := 256;
		GetMem(FPalabrasIndexadas, size);
		resultado := FPalabrasIndexadas;
		resultado^ := ' ';
		Inc(resultado);
		lenResultado := 1;

		ptrIni := datos;

		repeat
			if (ptrIni <> nil) and (ptrIni^ <> #0) then
			begin
				ptrIni := TParseador.AvanzarMientrasCaracteres(ptrIni, PuntoFinal, Separadores);
				ptrFin := TParseador.AvanzarHastaCaracteres(ptrIni, PuntoFinal, Separadores);

				if (ptrFin <> nil) then
				begin
					lenPalabra := ptrFin - ptrIni;

					if lenPalabra >= LongitudMinima then
					begin
						StrLCopy(palabra, ptrIni, lenPalabra);

						TParseador.Normalizar(palabra);
						AplicarDiccionarioSinonimos(palabra, lenPalabra);

						if (not EsPalabraVacia(palabra)) and
							(not PalabraYaIndexada(FPalabrasIndexadas, palabra, lenPalabra)) then
						begin
							if Estadisticas <> nil then
								Estadisticas.NuevaPalabraIndexada(palabra, lenPalabra);
								
							if lenResultado + lenPalabra + 1 > size then
							begin
								AmpliarResultado;
							end;
							StrLCopy(resultado, palabra, lenPalabra);
							Inc(resultado, lenPalabra);
							resultado^ := ' ';
							Inc(resultado);
							Inc(lenResultado, lenPalabra + 1);
						end;
					end;
					ptrIni := ptrFin + 1;
				end
				else
				begin
				end;
			end;
		until (ptrIni = nil) or (ptrIni^ = #0) or (ptrIni >= PuntoFinal);
		resultado^ := #0;
	end;
end;



end.