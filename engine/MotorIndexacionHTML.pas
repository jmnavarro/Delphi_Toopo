//~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
//
// Unidad: MotorIndexacionHTML.pas
//
// Propósito:
//		La clase TMotorIndexacionHTML es un descendiente directo de TMotorIndexacion
//		por lo que permite obtener la lista de palabras a partir de un texto en
//		formato HTML.
//		Tiene algunas limitaciones en el análisis de las etiquetas HTML, como que
//		no es capaz de extraer el texto de dentro de los atributos de las etiquetas
//		sino que solo extrae el texto entre etiquetas.
//		Por ejemplo, del siguiente HTML:
//			<b>el texto</b><img src="icono.gif" alt="descripción del icono">
//		se extraerán las palabras "el" y "texto" y el resto se ignorará ya que
//		pertenece a los atributos de las etiquetas y no al texto entre etiquetas.
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
unit MotorIndexacionHTML;

interface

uses MotorIndexacion, EstadisticasIndexacion;

type
	TMotorIndexacionHTML = class(TMotorIndexacion)
	private
		function SustituirEntidad(palabra: PChar): boolean;

	protected
		procedure IndexarCadena(ptr: PChar; fin: PChar; var resultado: PChar; var lenResultado: integer; var sizeResultado: integer); overload; virtual;
		procedure IndexarCadena(ptr: PChar; var resultado: PChar; var lenResultado: integer; var sizeResultado: integer); overload; virtual;

		procedure IndexarHead(var inicio: PChar; fin: PChar; var resultado: PChar; var lenResultado: integer; var sizeResultado: integer); virtual;
		procedure IndexarBody(var ptr: PChar; fin: PChar; var resultado: PChar; var lenResultado: integer; var sizeResultado: integer); virtual;

	public
		constructor Create(ficheroVacias, ficheroDiccionario: string; lenMinima: integer; estadisticas: TEstadisticasIndexacion = nil); override;

		function Indexar(const datos: Pointer; lenDatos: LongWord): boolean; override;
	end;


implementation


uses SysUtils, Parseador;


constructor TMotorIndexacionHTML.Create(ficheroVacias, ficheroDiccionario: string; lenMinima: integer; estadisticas: TEstadisticasIndexacion = nil);
begin
	Separadores := [#10, #13, #9, ' ', '.', ',', ';', ':', '?', '¿', '¡', '!', '(', ')', '[', ']', '{', '}', '+', '-', '—', '=', '/', '\', '"', '''', '“', '”', '*', '<', '>', '…', '@'];
	inherited;
end;


function TMotorIndexacionHTML.Indexar(const datos: Pointer; lenDatos: LongWord): boolean;
var
	ptrIni: PChar;
	PuntoFinal: PChar;
	sizeResultado: integer;
	lenResultado: integer;
begin
	result := (datos <> nil) and (lenDatos > 0);

	if result then
	begin
		PuntoFinal := PChar(LongWord(datos) + lenDatos);
		ptrIni := PChar(datos);

		sizeResultado := 256;
		GetMem(FPalabrasIndexadas, sizeResultado);
		FPalabrasIndexadas^ := ' ';
		lenResultado := 1;

		IndexarHead(ptrIni, PuntoFinal, FPalabrasIndexadas, lenResultado, sizeResultado);
		IndexarBody(ptrIni, PuntoFinal, FPalabrasIndexadas, lenResultado, sizeResultado);
	end;
end;


procedure TMotorIndexacionHTML.IndexarCadena(ptr: PChar; fin: PChar; var resultado: PChar; var lenResultado: integer; var sizeResultado: integer);
var
	ptrIni, ptrFin: PChar;
	res: PChar;
	chr: char;
	separadoresSinPC: TConjuntoSeparadores;
	lenPalabra: integer;
	palabra: array[0..63] of char;
	frase, p: PChar;

	procedure AmpliarResultado;
	var
		aux: PChar;
	begin
		sizeResultado := sizeResultado * 2;
		GetMem(aux, sizeResultado);
		StrLCopy(aux, resultado, lenResultado);
		FreeMem(resultado);
		resultado := aux;
		res := resultado + lenResultado;
	end;

	function PalabraYaIndexada(listaPalabras: PChar; palabra: PChar; len: LongWord): boolean;
	var
		p: array[0..255] of char;
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
	separadoresSinPC := Self.Separadores - [';'];

	res := resultado + lenResultado;

	ptrIni := ptr;
	repeat
		if (ptrIni <> nil) and (ptrIni^ <> #0) then
		begin
			ptrIni := TParseador.AvanzarMientrasCaracteres(ptrIni, fin, Self.Separadores);
			ptrFin := TParseador.AvanzarHastaCaracteres(ptrIni, fin, Self.Separadores);

			if ptrFin <> nil then
			begin
				lenPalabra := ptrFin - ptrIni;
				if lenPalabra >= LongitudMinima then
				begin
					StrLCopy(palabra, ptrIni, lenPalabra);
					palabra[lenPalabra] := #0;

					if StrScan(palabra, '&') <> nil then
					begin
						chr := (ptrFin+1)^;
						if not EsSeparador(chr) then
						begin
							ptrFin := TParseador.AvanzarHastaCaracteres(ptrIni, fin, separadoresSinPC);

							lenPalabra := ptrFin - ptrIni;
							GetMem(frase, lenPalabra+1);
							StrLCopy(frase, ptrIni, lenPalabra);
							frase[lenPalabra] := #0;
						end
						else
							frase := palabra;

						if SustituirEntidad(frase) then
						begin
							lenPalabra := StrLen(frase);

							p := TParseador.AvanzarHastaCaracteres(frase, StrEnd(frase), Self.Separadores);
							if p^ <> #0 then
							begin
								IndexarCadena(frase, frase + lenPalabra, resultado, lenResultado, sizeResultado);
								res := resultado + lenResultado;
								lenPalabra := -1;
							end
							else
								StrCopy(palabra, frase);
						end;

						if frase <> palabra then
							FreeMem(frase);
					end;

					if lenPalabra >= LongitudMinima then
					begin
						TParseador.Normalizar(palabra);
						AplicarDiccionarioSinonimos(palabra, lenPalabra);

						if (not EsPalabraVacia(palabra)) and
						   (not PalabraYaIndexada(resultado, palabra, lenPalabra)) then
						begin
							if lenResultado + lenPalabra + 1 >= sizeResultado then
							begin
								AmpliarResultado;
							end;
							res^ := ' ';
							Inc(res);
							StrLCopy(res, palabra, lenPalabra);
							Inc(res, lenPalabra);
							Inc(lenResultado, lenPalabra + 1);
						end;
					end;
				end;
				ptrIni := ptrFin + 1;
			end;
		end;
	until (ptrIni = nil) or (ptrIni^ = #0) or (ptrIni >= fin);
	res^ := #0;
end;


procedure TMotorIndexacionHTML.IndexarCadena(ptr: PChar; var resultado: PChar; var lenResultado: integer; var sizeResultado: integer);
begin
	IndexarCadena(ptr, StrEnd(ptr), resultado, lenResultado, sizeResultado);
end;


procedure TMotorIndexacionHTML.IndexarHead(var inicio: PChar; fin: PChar; var resultado: PChar; var lenResultado: integer; var sizeResultado: integer);

	function IndexarTitle(ptr: PChar): PChar;
	var
		tag: array[0..7] of char;
	begin
		tag[0] := #0;
		result := nil;

		while (result = nil) and (ptr^ <> #0) and (StrComp(tag, 'title') <> 0) do
		begin
			ptr := TParseador.AvanzarHastaCaracter(ptr, fin, '<');
			if ptr^ <> #0 then
			begin
				Inc(ptr);
				StrLCopy(tag, ptr, 5);
				StrLower(tag);

				if StrComp(tag, '/head') = 0 then
					result := ptr;
			end;
		end;

		// se ha encontrado title, se indexa
		if (result = nil) and (ptr^ <> #0) and (StrComp(tag, 'title') = 0) then
		begin
			ptr := StrScan(ptr, '>');
			if ptr <> nil then
			begin
				Inc(ptr);
				result := StrPos(ptr, '</');
				IndexarCadena(ptr, result, resultado, lenResultado, sizeResultado);
				Inc(result, 2);
			end;
		end;
	end;


	function IndexarMeta(ptr, name: PChar): PChar;
	var
		tag: array[0..31] of char;
		encontrado: boolean;
		espacios: TConjuntoSeparadores;
		terminacion: PChar;
		lenName: integer;
	begin
		espacios := [#10, #13, #9, ' '];

		tag[0] := #0;
		result := nil;
		encontrado := false;
		lenName := StrLen(name);

		while (result = nil) and (ptr^ <> #0) and (not encontrado) do
		begin
			ptr := TParseador.AvanzarHastaCaracter(ptr, fin, '<');
			if ptr^ <> #0 then
			begin
				Inc(ptr);
				StrLCopy(tag, ptr, 4);
				StrLower(tag);

				encontrado := (StrComp(tag, 'meta') = 0);
				if encontrado then
				begin
					ptr := TParseador.AvanzarHastaCaracter(ptr, fin, '=');
					if ptr^ <> #0 then
					begin
						Inc(ptr);
						ptr := TParseador.AvanzarMientrasCaracteres(ptr, fin, espacios);
						if ptr^ <> #0 then
						begin
							if ptr^ = '"' then
								Inc(ptr);

							StrLCopy(tag, ptr, lenName);
							StrLower(tag);

							encontrado := (StrComp(tag, name) = 0);
						end;
					end;
				end
				else if StrComp(tag, '/hea') = 0 then
					result := ptr;
			end;
		end;

		// se ha encontrado title, se indexa
		if (result = nil) and (ptr^ <> #0) and (StrComp(tag, name) = 0) then
		begin
			if ptr <> nil then
			begin
				ptr := StrPos(ptr, '=');
				if ptr <> nil then
				begin
					Inc(ptr);
					if ptr^ = '"' then
					begin
						Inc(ptr);
						terminacion := '"';
					end
					else
						terminacion := '</';

					result := StrPos(ptr, terminacion);
					IndexarCadena(ptr, result, resultado, lenResultado, sizeResultado);
					Inc(result, 2);
				end;
			end;
		end;
	end;




begin
	// del head solo se indexa las etiquetas
	//	<title>
	//	<meta name="description"
	//	<meta name="keywords"
	//	<meta name="author"
	IndexarTitle(inicio);
	IndexarMeta(inicio, 'description');
	IndexarMeta(inicio, 'keywords');
	IndexarMeta(inicio, 'author');
end;


procedure TMotorIndexacionHTML.IndexarBody(var ptr: PChar; fin: PChar; var resultado: PChar; var lenResultado: integer; var sizeResultado: integer);
var
	ptrIni, ptrFin: PChar;
	tag: array[0..15] of char;
begin
	FillChar(tag, sizeof(tag), #0);
	ptrIni := ptr;
	while (ptrIni <> nil) and (StrComp(tag, 'body') <> 0) do
	begin
		ptrIni := TParseador.AvanzarHastaCaracter(ptrIni, fin, '<');
		if (ptrIni <> nil) and (ptrIni^ <> #0) then
		begin
			Inc(ptrIni);
			StrLCopy(tag, ptrIni, 4);
			StrLower(tag);
		end;
	end;

	if (ptrIni <> nil) and (ptrIni^ <> #0) then
	begin
		ptrIni := TParseador.AvanzarHastaCaracter(ptrIni, fin, '>');
		if (ptrIni <> nil) and (ptrIni^ <> #0) then
		begin
			Inc(ptrIni);
			while (ptrIni <> nil) and (ptrIni^ <> #0) do
			begin
				ptrFin := TParseador.AvanzarHastaCaracter(ptrIni, fin, '<');
				if (ptrFin <> nil) and (ptrFin^ <> #0) then
				begin
					if ptrIni < ptrFin then
					begin
						IndexarCadena(ptrIni, ptrFin, resultado, lenResultado, sizeResultado);
					end;

					StrLCopy(tag, ptrFin, 7);
					StrLower(tag);
					if StrComp(tag, '</body>') = 0 then
						ptrIni := nil
					else
					begin
						ptrIni := TParseador.AvanzarHastaCaracter(ptrFin, fin, '>');
						if (ptrIni <> nil) and (ptrIni^ <> #0) then
							Inc(ptrIni);
					end;
				end;
			end;
		end;
	end;
end;


function TMotorIndexacionHTML.SustituirEntidad(palabra: PChar): boolean;
const
	MAX_ENTIDADES = 17;
	ENTIDADES: array[0..MAX_ENTIDADES-1] of PChar =
	(
		'nbsp',		'acute',
		'aacute',	'eacute',	'iacute',	'oacute',	'uacute',
		'Aacute',	'Eacute',	'Iacute',	'Oacute',	'Uacute',
		'ntilde',	'Ntilde',
		'iquest',	'iexcl',
		'amp'
	);
	PALABRAS: array[0..MAX_ENTIDADES-1] of PChar =
	(
		' ',		'´',
		'á',		'é',		'í',		'ó',		'ú',
		'Á',		'É',		'Í',  		'Ó',		'Ú',
		'ñ',		'Ñ',
		'¿',		'¡',
		'&'
	);
var
	entidad, final: PChar;
	i: integer;
	copia: PChar;
	enFinal: boolean;
begin
	result := false;

	copia := StrNew(palabra);

	entidad := copia;
	while entidad^ <> #0 do
	begin
		while (entidad^ <> '&') and (entidad^ <> #0) do
			Inc(entidad);

		if entidad^ <> #0 then
		begin
			Inc(entidad);
			final := entidad;

			while (final^ <> #0) and (final^ <> ';') do
				Inc(final);

			enFinal := (final^ = #0);
			if final^ <> #0 then
				final^ := #0;

			AnsiStrLower(entidad);

			i := 0;
			while i < MAX_ENTIDADES do
			begin
				if StrComp(entidad, ENTIDADES[i]) = 0 then
				begin
					if not enFinal then
						Inc(final);
					StrLCopy(palabra, copia, entidad - copia - 1);
					StrCat(palabra, PALABRAS[i]);
					StrCat(palabra, final);
					StrCopy(copia, palabra);
					result := true;
					i := MAX_ENTIDADES + 1;
				end;
				Inc(i);
			end;
			if i = MAX_ENTIDADES then
			begin
				// no encontrada, se deja tal y como está
				entidad := copia;
				while (entidad^ <> '&') and (entidad^ <> #0) do
					Inc(entidad);

				if entidad^ = '&' then
					Inc(entidad);
			end
			else
				entidad := copia;
		end;
	end;
	StrDispose(copia);
end;


end.
