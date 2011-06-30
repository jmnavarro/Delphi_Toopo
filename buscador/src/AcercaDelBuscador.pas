unit AcercaDelBuscador;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TAcercaDelBuscadorForm = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure Label4Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
  private
    { Private declarations }
  public
	{ Public declarations }
  end;

implementation

{$R *.dfm}

uses ShellAPI;

procedure TAcercaDelBuscadorForm.Label4Click(Sender: TObject);
begin
	(Sender as TLabel).Font.Color := clPurple;
	(Sender as TLabel).Repaint;
	ShellExecute(Handle, nil, 'http://www.lawebdejm.com', nil, nil, 0);
end;

procedure TAcercaDelBuscadorForm.Label5Click(Sender: TObject);
begin
	(Sender as TLabel).Font.Color := clPurple;
	(Sender as TLabel).Repaint;
	ShellExecute(Handle, nil, 'http://www.iberprensa.com', nil, nil, 0);
end;

end.
