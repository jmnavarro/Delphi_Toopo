unit DiccionarioSinonimos;

interface

uses ListaEnFichero, classes;

type
	TDiccionarioSinonimos = class(TListaEnFichero)
	private
		function GetLista: TStrings;
	public
		function SustituirSinonimo(palabra: PChar): boolean;

		property Sinonimos: TStrings read GetLista;
	end;


implementation

uses SysUtils;


function TDiccionarioSinonimos.SustituirSinonimo(palabra: PChar): boolean;
var
	aux: string;
	copia: PChar;
	len: integer;
begin
	if palabra[0] = ' ' then
		copia := StrNew(palabra+1)
	else
		copia := StrNew(palabra);

	len := StrLen(copia);

	if copia[len-1] = ' ' then
		copia[len-1] := #0;

	aux := Lista.values[copia];
	result := (aux <> '');
	if result then
		StrCopy(palabra, PChar(aux));

	StrDispose(copia);
end;


function TDiccionarioSinonimos.GetLista: TStrings;
begin
	result := Lista;
end;

end.
