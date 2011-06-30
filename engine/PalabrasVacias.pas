//~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
//
// Unidad: PalabrasVacias.pas
//
// Propósito:
//		La clase TPalabrasVacias es un descendiete directo de TListaEnFichero y
//		sirve para recuperar la lista de palabras (stoplist) que se consideran
//		supérfluas tanto en la indexación como en la búsqueda.
//		Ofrece métodos para averiguar si una palabra está en la lista (es decir:
//		saber si es palabra vacía o no) y para eliminar todas las palabras
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
//
//~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
unit PalabrasVacias;

interface

uses ListaEnFichero;

type
	TPalabrasVacias = class(TListaEnFichero)
	public
		function EsPalabraVacia(palabra: PChar): boolean;
		function QuitarPalabrasVacias(texto: PChar): integer;
	end;


implementation

uses SysUtils, Parseador;


function TPalabrasVacias.EsPalabraVacia(palabra: PChar): boolean;
begin
	result := (Lista.IndexOf(palabra) <> -1);
end;


function TPalabrasVacias.QuitarPalabrasVacias(texto: PChar): integer;
var
	ini, fin, final: PChar;
	palabra: array[0..63] of char;
begin
	result := 0;
	ini := texto;
	final := StrEnd(texto);
	repeat
		ini := TParseador.AvanzarMientrasCaracter(ini, final, ' ');
		fin := TParseador.AvanzarHastaCaracter(ini, final, ' ');

		if (ini <> nil) and (ini^ <> #0) then
		begin
			StrLCopy(palabra, ini, fin - ini);

			if EsPalabraVacia(palabra) then
			begin
				Inc(result);
				Dec(ini);
				if (fin = nil) or (fin^ = #0) then
				begin
					while ini^ <> ' ' do
						Dec(ini);
					ini^ := #0;
				end
				else
					StrLCopy(ini, fin, final - fin);
			end
			else
				ini := fin;
		end;

	until (ini = nil) or (ini^ = #0);
end;


end.
