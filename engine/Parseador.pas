unit Parseador;

interface


type
	TConjuntoSeparadores = set of char;

	TParseador = class(TObject)
	public
		class function AvanzarMientrasCaracter(ptr, fin: PChar; caracter: char): PChar;
		class function AvanzarHastaCaracter(ptr, fin: PChar; caracter: char): PChar;

		class function AvanzarMientrasCaracteres(ptr, fin: PChar; caracteres: TConjuntoSeparadores): PChar;
		class function AvanzarHastaCaracteres(ptr, fin: PChar; caracteres: TConjuntoSeparadores): PChar;

		class function AvanzarTagsYCaracteres(ptr, fin: PChar; caracteres: TConjuntoSeparadores): PChar;

		class procedure Normalizar(palabra: PChar);
	end;


implementation

uses SysUtils;


class function TParseador.AvanzarMientrasCaracter(ptr, fin: PChar; caracter: char): PChar;
var
	c: char;
begin
	result := ptr;
	if result <> nil then
	begin
		c := result^;
		while (result < fin) and (c <> #0) and (c = caracter) do
		begin
			Inc(result);
			c := result^;
		end;
	end;
end;


class function TParseador.AvanzarHastaCaracter(ptr, fin: PChar; caracter: char): PChar;
var
	c: char;
begin
	result := ptr;
	if result <> nil then
	begin
		c := result^;
		while (result < fin) and (c <> #0) and (c <> caracter) do
		begin
			Inc(result);
			c := result^;
		end;
	end;
end;


class function TParseador.AvanzarHastaCaracteres(ptr, fin: PChar; caracteres: TConjuntoSeparadores): PChar;
var c: char;
begin
	result := ptr;
	if result <> nil then
	begin
		c := result^;
		while (result < fin) and (c <> #0) and (not(c in caracteres)) do
		begin
			Inc(result);
			c := result^;
		end;
	end;
end;

class function TParseador.AvanzarMientrasCaracteres(ptr, fin: PChar; caracteres: TConjuntoSeparadores): PChar;
var
	c: char;
begin
	result := ptr;
	if result <> nil then
	begin
		c := result^;
		while (result < fin) and (c <> #0) and (c in caracteres) do
		begin
			Inc(result);
			c := result^;
		end;
	end;
end;


class function TParseador.AvanzarTagsYCaracteres(ptr, fin: PChar; caracteres: TConjuntoSeparadores): PChar;
begin
	result := ptr;
	if result <> nil then
	begin
		result := AvanzarMientrasCaracteres(result, fin, caracteres);
		while (result < fin) and (result^ = '<') and (result^ <> #0) do
		begin
			result := AvanzarHastaCaracter(result, fin, '>');
			if result^ <> #0 then
				result := AvanzarMientrasCaracteres(result+1, fin, caracteres);
		end;
	end;
end;


class procedure TParseador.Normalizar(palabra: PChar);
var
	p: PChar;
begin
	StrLower(palabra);

	p := palabra;

	while p^ <> #0 do
	begin
	if (p^ = 'á') or (p^ = 'à') or (p^ = 'ä') or (p^ = 'Á') or (p^ = 'À') or (p^ = 'Ä') then
		p^ := 'a'
	else if (p^ = 'é') or (p^ = 'è') or (p^ = 'ë') or (p^ = 'É') or (p^ = 'È') or (p^ = 'Ë') then
		p^ := 'e'
	else if (p^ = 'í') or (p^ = 'ì') or (p^ = 'ï') or (p^ = 'Í') or (p^ = 'Ì') or (p^ = 'Ï') then
		p^ := 'i'
	else if (p^ = 'ó') or (p^ = 'ò') or (p^ = 'ö') or (p^ = 'Ó') or (p^ = 'Ò') or (p^ = 'Ö') then
		p^ := 'o'
	else if (p^ = 'ú') or (p^ = 'ù') or (p^ = 'ü') or (p^ = 'Ú') or (p^ = 'Ù') or (p^ = 'Ü') then
		p^ := 'u';

	  Inc(p);
   end;
end;

end.