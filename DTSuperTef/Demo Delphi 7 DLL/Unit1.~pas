unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,IniFiles ,
  StdCtrls,
  Dialogs;

   //Supertef
 procedure ListaPOS(authorization_sh, chave_cliente: WideString; Result: PAnsiChar); stdcall; external 'supertef.dll' name 'ListaPOS';
 procedure Pagamento_Cadastro(authorization_sh, chave_cliente: WideString;
  pos_id, transaction_type, installment_count, installment_type: integer;
  amount: WideString; order_id, description: WideString;
  Result: PAnsiChar); stdcall; external 'supertef.dll' name 'Pagamento_Cadastro';
 procedure Pagamento_Detalha(authorization_sh: WideString;
  payment_uniqueid: integer; Result: PAnsiChar); stdcall; external 'supertef.dll' name 'Pagamento_Detalha';

  procedure Pagamento_Rejeita(authorization_sh: WideString;
  payment_uniqueid: Integer; Result: PAnsiChar); stdcall; external 'supertef.dll' name 'Pagamento_Rejeita';


  procedure Pagamento_payment_uniqueid(Result: PAnsiChar); stdcall;external 'supertef.dll' name 'Pagamento_payment_uniqueid';
  procedure Pagamento_payment_status(Result: PAnsiChar); stdcall; external 'supertef.dll' name 'Pagamento_payment_status';
  procedure Pagamento_payment_message(Result: PAnsiChar); stdcall;  external 'supertef.dll' name 'Pagamento_payment_message';
  procedure Pagamento_id_payment(Result: PAnsiChar); stdcall;  external 'supertef.dll' name 'Pagamento_id_payment';
  procedure Pagamento_cardholder_name(Result: PAnsiChar); stdcall; external 'supertef.dll' name 'Pagamento_cardholder_name';
  procedure Pagamento_brand(Result: PAnsiChar); stdcall; external 'supertef.dll' name 'Pagamento_brand';
  procedure Pagamento_nsu(Result: PAnsiChar); stdcall; external 'supertef.dll' name 'Pagamento_nsu';
  procedure Pagamento_authorization_code(Result: PAnsiChar); stdcall;external 'supertef.dll' name 'Pagamento_authorization_code';
  procedure Pagamento_authorization_date_time(Result: PAnsiChar); stdcall; external 'supertef.dll' name 'Pagamento_authorization_date_time';


type
  TForm1 = class(TForm)
    ButtonListaPagamento: TButton;
    ButtonCadastraPagamento: TButton;
    Edit12: TEdit;
    ButtonDetalha: TButton;
    Memo7: TMemo;
    Button1: TButton;
    procedure ButtonListaPagamentoClick(Sender: TObject);
   procedure ButtonCadastraPagamentoClick(Sender: TObject);
    procedure ButtonDetalhaClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonListaPagamentoClick(Sender: TObject);
var
  DLL: THandle;
  pResult: array [0..255] of AnsiChar;
  resultado: string;
  ArquivoIni: TIniFile;
  Secoes: TStringList;
  i: Integer;
  NomeSecao: string;
  RetornouResultado: Boolean;
  authorization_sh, chave_cliente: string;
begin
  authorization_sh := '';
  chave_cliente := '';

  DLL := LoadLibrary('supertef.dll');
  if DLL = 0 then
  begin
    ShowMessage('Erro ao carregar a DLL supertef.dll');
    Exit;
  end;

  try
    FillChar(pResult, SizeOf(pResult), 0);
    ListaPOS(authorization_sh, chave_cliente, pResult);

    if StrPas(pResult) <> 'TRUE' then
    begin
      ShowMessage(StrPas(pResult));
    end
    else
    begin
      memo7.Clear;
      ArquivoIni := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'SuperTefListaPOS.ini');
      Secoes := TStringList.Create;
      try
        // Obtém todas as seções do arquivo INI
        ArquivoIni.ReadSections(Secoes);

        // Itera sobre todas as seções
        for i := 0 to Secoes.Count - 1 do
        begin
          RetornouResultado := True;
          NomeSecao := Secoes[i];

          memo7.Lines.Add('ID: ' + ArquivoIni.ReadString(NomeSecao, 'ID', ''));
          memo7.Lines.Add('NOME: ' + ArquivoIni.ReadString(NomeSecao, 'NOME', ''));
          memo7.Lines.Add('STATUS: ' + ArquivoIni.ReadString(NomeSecao, 'STATUS', ''));
          memo7.Lines.Add('BANCO: ' + ArquivoIni.ReadString(NomeSecao, 'BANCO', ''));
          memo7.Lines.Add('--------------------------------');
          memo7.Lines.Add('');
        end;
      finally
        Secoes.Free;
        ArquivoIni.Free;
      end;
    end;
  finally
    FreeLibrary(DLL);
  end;
end;

procedure TForm1.ButtonCadastraPagamentoClick(Sender: TObject);
var
DLL : THandle;
pResult : array [0..255] of AnsiChar;
resultado: string;
authorization_sh,chave_cliente : string;

begin
 authorization_sh := '';
  chave_cliente := '';



    Pagamento_Cadastro(authorization_sh,
    chave_cliente,
    171,3,1,1,
    '10,55',
    '123',
    'teste',pResult);


    if pResult <> 'TRUE' then
   begin
   ShowMessage(pResult);
   end else
   begin
  memo7.Clear;
  pResult := '';
  Pagamento_payment_uniqueid(pResult);
  Edit12.Text :=  pResult;
  memo7.Lines.Add('payment_uniqueid: '+pResult);
  pResult := '';
  Pagamento_payment_status(pResult);
  memo7.Lines.Add('payment_status: '+pResult);
  pResult := '';
  Pagamento_payment_message(pResult);
  memo7.Lines.Add('payment_message: '+pResult);
   end;





end;

procedure TForm1.ButtonDetalhaClick(Sender: TObject);
var
DLL : THandle;
pResult : array [0..255] of AnsiChar;
resultado: string;
authorization_sh : string;
begin
 authorization_sh := '';




if Edit12.text = '' then
begin
Edit12.Focused;
ShowMessage('Campo Vazio');
exit;
end;

Pagamento_Detalha(authorization_sh,strtoint(Edit12.text),pResult);

  if pResult <> 'TRUE' then
   begin
   ShowMessage(pResult);
   end else
   begin

  memo7.Clear;
  pResult := '';
  Pagamento_payment_uniqueid(pResult);
  Edit12.Text :=  pResult;
  memo7.Lines.Add('payment_uniqueid: '+pResult);
  pResult := '';
  Pagamento_payment_status(pResult);
  memo7.Lines.Add('payment_status: '+pResult);
  pResult := '';
  Pagamento_payment_message(pResult);
  memo7.Lines.Add('payment_message: '+pResult);
  pResult := '';
  Pagamento_id_payment(pResult);
  memo7.Lines.Add('id_payment: '+pResult);
  pResult := '';
  Pagamento_cardholder_name(pResult);
  memo7.Lines.Add('cardholder_name: '+pResult);
   pResult := '';
  Pagamento_brand(pResult);
  memo7.Lines.Add('cardholder_name: '+pResult);
   pResult := '';
  Pagamento_nsu(pResult);
  memo7.Lines.Add('nsu: '+pResult);
   pResult := '';
  Pagamento_authorization_code(pResult);
  memo7.Lines.Add('authorization_code: '+pResult);
  pResult := '';
  Pagamento_authorization_date_time(pResult);
  memo7.Lines.Add('authorization_date_time: '+pResult);
   end;



end;

procedure TForm1.Button1Click(Sender: TObject);
var
DLL : THandle;
pResult : array [0..255] of AnsiChar;
resultado: string;
authorization_sh : string;
begin
 authorization_sh := '';




if Edit12.text = '' then
begin
Edit12.Focused;
ShowMessage('Campo Vazio');
exit;
end;
    Pagamento_Rejeita(authorization_sh,strtoint(Edit12.text),pResult);
  if pResult <> 'TRUE' then
   begin
   ShowMessage(pResult);
   end else
   begin
   ShowMessage('Status: '+'Sucesso - pagamento rejeitado');
   end;



end;

end.
