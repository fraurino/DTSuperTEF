unit uDemo;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
StdCtrls, Buttons, ComCtrls, ExtCtrls, MaskEdit, DTSuperTEF;

type

{ TForm1 }

TForm1 = class(TForm)
		  btnAtualizarCliente: TButton;
		  btnAtualizarPOS: TButton;
		  btnCancelarPagamento: TButton;
		  btnCriarCliente: TButton;
		  btnCriarPagamento: TButton;
		  btnCriarPOS: TButton;
		  btnDetalharCliente: TButton;
		  btnDetalharPagamento: TButton;
		  btnDetalharPOS: TButton;
		  btnExcluirPOS: TButton;
		  btnListarPagamentos: TButton;
		  btnListarPOS: TButton;
		  CBExibir_Json: TCheckBox;
		  DTSuperTEF1: TDTSuperTEF;
		  edtCliAtivo: TComboBox;
		  edtCliCNPJ: TMaskEdit;
		  edtCliContato: TEdit;
		  edtCliLimitePOS: TEdit;
		  edtCliNome: TEdit;
		  edtCliSitefBanco: TEdit;
		  edtCliSitefCNPJ: TEdit;
		  edtCliSitefEmpresa: TEdit;
		  edtPayDescricao: TEdit;
		  edtPayOrderID: TEdit;
		  edtPayParceladoTipo: TEdit;
		  edtPayParcelas: TEdit;
		  edtPayPOSId: TEdit;
		  edtPayTransType: TEdit;
		  edtPayUniqueID: TEdit;
		  edtPayValor: TEdit;
		  edtPOSId: TEdit;
		  edtPOSNome: TEdit;
		  edtToken: TEdit;
		  edtCliChave: TEdit;
		  Image1: TImage;
		  Label1: TLabel;
		  Label2: TLabel;
		  LabelCliAtivo: TLabel;
		  LabelCliCNPJ: TLabel;
		  LabelCliContato: TLabel;
		  LabelCliLimitePOS: TLabel;
		  LabelCliNome: TLabel;
		  LabelCliSitefBanco: TLabel;
		  LabelCliSitefCNPJ: TLabel;
		  LabelCliSitefEmpresa: TLabel;
		  LabelPayDescricao: TLabel;
		  LabelPayOrderID: TLabel;
		  LabelPayParceladoTipo: TLabel;
		  LabelPayParcelas: TLabel;
		  LabelPayPOSId: TLabel;
		  LabelPayTransType: TLabel;
		  LabelPayUniqueID: TLabel;
		  LabelPayValor: TLabel;
		  LabelPOSId: TLabel;
		  LabelPOSNome: TLabel;
		  listarclientes: TButton;
MemoLog: TMemo;
PageControl1: TPageControl;
Panel1: TPanel;
tsRelatorios: TTabSheet;
tsPagamento: TTabSheet;
tsPOS: TTabSheet;
tsClientes: TTabSheet;
procedure btnAtualizarClienteClick(Sender: TObject);
procedure btnAtualizarPOSClick(Sender: TObject);
procedure btnCancelarPagamentoClick(Sender: TObject);
procedure btnCriarClienteClick(Sender: TObject);
procedure btnCriarPagamentoClick(Sender: TObject);
procedure btnCriarPOSClick(Sender: TObject);
procedure btnDetalharClienteClick(Sender: TObject);
procedure btnDetalharPagamentoClick(Sender: TObject);
procedure btnDetalharPOSClick(Sender: TObject);
procedure btnExcluirPOSClick(Sender: TObject);
procedure btnListarPOSClick(Sender: TObject);
procedure CBExibir_JsonClick(Sender: TObject);
procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
procedure FormCreate(Sender: TObject);
procedure listarclientesClick(
Sender: TObject);
private

procedure LimparMemoLog;
procedure AdicionarLinhaLog(const ATexto: string);
procedure AdicionarSeparador;
procedure LogListaClientes(AListaClientes: TListaClientes);
procedure LogCliente(ACliente: TCliente; const ATitulo: string = 'CLIENTE');
procedure LogPagamento(APagamento: TPagamento; const ATitulo: string = 'PAGAMENTO');
procedure LogPOS(APOS: TPOS; const ATitulo: string = 'POS');
procedure LogListaPOS(AListaPOS: TListaPOS);
procedure LogExcluirPOSResponse(AResponse: TExcluirPOSResponse);
procedure LogRejeitarPagamentoResponse(AResponse: TRejeitarPagamentoResponse);
procedure LogPaymentOrder(APaymentOrder: TPaymentOrder; const AIndentacao: string = '  ');
procedure LogPaymentData(APaymentData: TPaymentData; const AIndentacao: string = '  ');

public
//DTSuperTEF1  : TDTSuperTEF;

end;

var
Form1: TForm1;


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.listarclientesClick(Sender: TObject);
var
  ListaClientes: TListaClientes;

begin
  LimparMemoLog;

  DTSuperTEF1.Token:= edtToken.text;

  try
    ListaClientes := DTSuperTEF1.ListarClientes;
    try
      LogListaClientes(ListaClientes);
    finally
      ListaClientes.Free;
    end;
  except
    on E: Exception do
    begin
      AdicionarLinhaLog('ERRO AO LISTAR CLIENTES:');
      AdicionarLinhaLog(E.Message);
    end;
  end;

  MemoLog.SelStart := 0;

end;

procedure TForm1.btnCriarClienteClick(Sender: TObject);
var
  vAtivo: Integer;
  Cliente: TCliente;
begin
  LimparMemoLog;

  if edtCliAtivo.ItemIndex = 0 then
    vAtivo := 1
  else
    vAtivo := 0;

  DTSuperTEF1.Token := edtToken.Text;

  try
    Cliente := DTSuperTEF1.CriarCliente(
      vAtivo,
      edtCliCNPJ.Text,
      edtCliNome.Text,
      edtCliContato.Text,
      StrToIntDef(edtCliLimitePOS.Text, 1),
      edtCliSitefEmpresa.Text,
      edtCliSitefCNPJ.Text,
      edtCliSitefBanco.Text
    );

    try
      LogCliente(Cliente, 'CLIENTE CRIADO');
      // Salva a chave do cliente criado para uso posterior
      edtCliChave.Text := Cliente.Chave;
    finally
      Cliente.Free;
    end;
  except
    on E: Exception do
    begin
      AdicionarLinhaLog('ERRO AO CRIAR CLIENTE:');
      AdicionarLinhaLog(E.Message);
    end;
  end;

  // Rola para o topo do memo
  MemoLog.SelStart := 0;

end;

procedure TForm1.btnCriarPagamentoClick(Sender: TObject);
var
  Pagamento: TPagamento;
begin
  LimparMemoLog;

  DTSuperTEF1.Token := edtToken.Text;

  try
    Pagamento := DTSuperTEF1.CriarPagamento(
      edtCliChave.Text,
      StrToIntDef(edtPayPOSId.Text, 0),
      edtPayTransType.Text,
      StrToIntDef(edtPayParcelas.Text, 1),
      StrToIntDef(edtPayParceladoTipo.Text, 1),
      StrToFloatDef(edtPayValor.Text, 0),
      edtPayOrderID.Text,
      edtPayDescricao.Text
    );

    try
      LogPagamento(Pagamento, 'PAGAMENTO CRIADO');
      // Salva o Unique ID do pagamento criado para uso posterior
      edtPayUniqueID.Text := Pagamento.PaymentUniqueid.ToString;
    finally
      Pagamento.Free;
    end;
  except
    on E: Exception do
    begin
      AdicionarLinhaLog('ERRO AO CRIAR PAGAMENTO:');
      AdicionarLinhaLog(E.Message);
    end;
  end;

  MemoLog.SelStart := 0;
end;


procedure TForm1.btnCriarPOSClick(Sender: TObject);
var
  POS: TPOS;
begin
  LimparMemoLog;
  DTSuperTEF1.Token := edtToken.Text;
  try
    POS := DTSuperTEF1.CriarPOS(  edtCliChave.Text,   edtPOSNome.Text );
    try
      LogPOS(POS, 'POS CRIADO');
      edtPOSId.Text := POS.Id.ToString;
    finally
      POS.Free;
    end;
  except
    on E: Exception do
    begin
      AdicionarLinhaLog('ERRO AO CRIAR POS:');
      AdicionarLinhaLog(E.Message);
    end;
  end;
  MemoLog.SelStart := 0;
end;


procedure TForm1.btnAtualizarClienteClick(Sender: TObject);
var
  vAtivo: Integer;
  Cliente: TCliente;
begin
  LimparMemoLog;

  if edtCliAtivo.ItemIndex = 0 then
    vAtivo := 1
  else
    vAtivo := 0;

  DTSuperTEF1.Token := edtToken.Text;

  try
    Cliente := DTSuperTEF1.AtualizarCliente(
      edtCliChave.Text,
      vAtivo,
      edtCliCNPJ.Text,
      edtCliNome.Text,
      edtCliContato.Text,
      StrToIntDef(edtCliLimitePOS.Text, 1),
      edtCliSitefEmpresa.Text,
      edtCliSitefCNPJ.Text,
      edtCliSitefBanco.Text
    );

    try
      LogCliente(Cliente, 'CLIENTE ATUALIZADO');
    finally
      Cliente.Free;
    end;
  except
    on E: Exception do
    begin
      AdicionarLinhaLog('ERRO AO ATUALIZAR CLIENTE:');
      AdicionarLinhaLog(E.Message);
    end;
  end;

  MemoLog.SelStart := 0;

end;

procedure TForm1.btnAtualizarPOSClick(Sender: TObject);
var
  POS: TPOS;
begin
  LimparMemoLog;

  DTSuperTEF1.Token := edtToken.Text;

  try
    POS := DTSuperTEF1.AtualizarPOS(
      StrToIntDef(edtPOSId.Text, 0),
      edtCliChave.Text,
      edtPOSNome.Text
    );

    try
      LogPOS(POS, 'POS ATUALIZADO');
    finally
      POS.Free;
    end;
  except
    on E: Exception do
    begin
      AdicionarLinhaLog('ERRO AO ATUALIZAR POS:');
      AdicionarLinhaLog(E.Message);
    end;
  end;

  MemoLog.SelStart := 0;
end;

procedure TForm1.btnCancelarPagamentoClick(Sender: TObject);
var
  Response: TRejeitarPagamentoResponse;
begin
  LimparMemoLog;

  DTSuperTEF1.Token := edtToken.Text;

  try
    Response := DTSuperTEF1.RejeitarPagamento(StrToIntDef(edtPayUniqueID.Text, 0));
    try
      LogRejeitarPagamentoResponse(Response);
    finally
      Response.Free;
    end;
  except
    on E: Exception do
    begin
      AdicionarLinhaLog('ERRO AO CANCELAR PAGAMENTO:');
      AdicionarLinhaLog(E.Message);
    end;
  end;

  MemoLog.SelStart := 0;
end;


procedure TForm1.btnDetalharClienteClick(Sender: TObject);
var
  Cliente: TCliente;
begin
  LimparMemoLog;

  DTSuperTEF1.Token := edtToken.Text;

  try
    Cliente := DTSuperTEF1.DetalharCliente(edtCliChave.Text);
    try
      LogCliente(Cliente, 'DETALHES DO CLIENTE');
    finally
      Cliente.Free;
    end;
  except
    on E: Exception do
    begin
      AdicionarLinhaLog('ERRO AO DETALHAR CLIENTE:');
      AdicionarLinhaLog(E.Message);
    end;
  end;

  MemoLog.SelStart := 0;

end;

procedure TForm1.btnDetalharPagamentoClick(Sender: TObject);
var
  Pagamento: TPagamento;
begin
  LimparMemoLog;

  DTSuperTEF1.Token := edtToken.Text;

  try
    Pagamento := DTSuperTEF1.DetalharPagamento(StrToIntDef(edtPayUniqueID.Text, 0));
    try
      LogPagamento(Pagamento, 'DETALHES DO PAGAMENTO');
    finally
      Pagamento.Free;
    end;
  except
    on E: Exception do
    begin
      AdicionarLinhaLog('ERRO AO DETALHAR PAGAMENTO:');
      AdicionarLinhaLog(E.Message);
    end;
  end;

  MemoLog.SelStart := 0;
end;

procedure TForm1.btnDetalharPOSClick(Sender: TObject);
var
  POS: TPOS;
begin
  LimparMemoLog;

  DTSuperTEF1.Token := edtToken.Text;

  try
    POS := DTSuperTEF1.DetalharPOS(StrToIntDef(edtPOSId.Text, 0));
    try
      LogPOS(POS, 'DETALHES DO POS');
    finally
      POS.Free;
    end;
  except
    on E: Exception do
    begin
      AdicionarLinhaLog('ERRO AO DETALHAR POS:');
      AdicionarLinhaLog(E.Message);
    end;
  end;

  MemoLog.SelStart := 0;
end;

procedure TForm1.btnExcluirPOSClick(Sender: TObject);
var
  Response: TExcluirPOSResponse;
begin
  LimparMemoLog;

  DTSuperTEF1.Token := edtToken.Text;

  try
    Response := DTSuperTEF1.ExcluirPOS(
      StrToIntDef(edtPOSId.Text, 0),
      edtCliChave.Text
    );

    try
      LogExcluirPOSResponse(Response);
    finally
      Response.Free;
    end;
  except
    on E: Exception do
    begin
      AdicionarLinhaLog('ERRO AO EXCLUIR POS:');
      AdicionarLinhaLog(E.Message);
    end;
  end;

  MemoLog.SelStart := 0;
end;


procedure TForm1.btnListarPOSClick(Sender: TObject);
var
  ListaPOS: TListaPOS;
begin
  LimparMemoLog;

  DTSuperTEF1.Token := edtToken.Text;

  try
    ListaPOS := DTSuperTEF1.ListarPOS(edtCliChave.Text);
    try
      LogListaPOS(ListaPOS);
    finally
      ListaPOS.Free;
    end;
  except
    on E: Exception do
    begin
      AdicionarLinhaLog('ERRO AO LISTAR POS:');
      AdicionarLinhaLog(E.Message);
    end;
  end;

  MemoLog.SelStart := 0;
end;

procedure TForm1.CBExibir_JsonClick(Sender: TObject);
begin
  MemoLog.Visible := CBExibir_Json.Checked;
end;




procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 //DTSuperTEF1.free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 //DTSuperTEF1 := TDTSuperTEF.Create(self);
end;

procedure TForm1.LimparMemoLog;
begin
    MemoLog.Lines.Clear;
end;

procedure TForm1.AdicionarLinhaLog(const ATexto: string);
begin
   MemoLog.Lines.Add(ATexto);
end;

procedure TForm1.AdicionarSeparador;
begin
   AdicionarLinhaLog('=====================================');
end;

procedure TForm1.LogListaClientes(AListaClientes: TListaClientes);
var
  I: Integer;
begin
  AdicionarLinhaLog('=== LISTA DE CLIENTES ===');
  AdicionarLinhaLog('Total: ' + AListaClientes.Total.ToString);
  AdicionarLinhaLog('Por Página: ' + AListaClientes.PerPage.ToString);
  AdicionarLinhaLog('Página Atual: ' + AListaClientes.CurrentPage.ToString);
  AdicionarLinhaLog('Última Página: ' + AListaClientes.LastPage.ToString);
  AdicionarLinhaLog('Até: ' + AListaClientes.ToPage.ToString);
  AdicionarLinhaLog('Registros encontrados: ' + AListaClientes.Data.Count.ToString);
  AdicionarSeparador;

  for I := 0 to AListaClientes.Data.Count - 1 do
  begin
    LogCliente(AListaClientes.Data[I], 'CLIENTE #' + (I + 1).ToString);
  end;
end;

procedure TForm1.LogCliente(ACliente: TCliente; const ATitulo: string);
begin
  AdicionarLinhaLog('=== ' + ATitulo + ' ===');
    AdicionarLinhaLog('ID: ' + ACliente.Id.ToString);
    AdicionarLinhaLog('Status: ' + BoolToStr(ACliente.Status, True));
    AdicionarLinhaLog('Ativo: ' + ACliente.Ativo.ToString);
    AdicionarLinhaLog('CNPJ/CPF: ' + ACliente.CNPJ_CPF);
    AdicionarLinhaLog('Nome Empresa: ' + ACliente.NomeEmpresa);
    AdicionarLinhaLog('Contato: ' + ACliente.Contato);
    AdicionarLinhaLog('Limite POS: ' + ACliente.LimitePOS.ToString);
    AdicionarLinhaLog('Sitef Empresa: ' + ACliente.SitefEmpresa);
    AdicionarLinhaLog('Sitef CNPJ/CPF: ' + ACliente.SitefCNPJ_CPF);
    AdicionarLinhaLog('Sitef Banco: ' + ACliente.SitefBanco);
    AdicionarLinhaLog('Chave: ' + ACliente.Chave);
    AdicionarLinhaLog('Softhouse ID: ' + ACliente.SofthouseId.ToString);
    AdicionarLinhaLog('Criado em: ' + ACliente.CreatedAt);
    AdicionarLinhaLog('Atualizado em: ' + ACliente.UpdatedAt);
    AdicionarSeparador;
end;

procedure TForm1.LogPagamento(APagamento: TPagamento; const ATitulo: string);
begin
  AdicionarLinhaLog('=== ' + ATitulo + ' ===');
  AdicionarLinhaLog('Payment Unique ID: ' + APagamento.PaymentUniqueid.ToString);
  AdicionarLinhaLog('Criado em: ' + APagamento.CreatedAt);
  AdicionarLinhaLog('Status Pagamento: ' + APagamento.PaymentStatus.ToString);
  AdicionarLinhaLog('Mensagem Pagamento: ' + APagamento.PaymentMessage);
  AdicionarLinhaLog('');
  LogPaymentOrder(APagamento.PaymentOrder);
  AdicionarLinhaLog('');
  LogPaymentData(APagamento.PaymentData);
  AdicionarSeparador;
end;

procedure TForm1.LogPOS(APOS: TPOS; const ATitulo: string);
begin
  AdicionarLinhaLog('=== ' + ATitulo + ' ===');
  AdicionarLinhaLog('ID: ' + APOS.Id.ToString);
  AdicionarLinhaLog('Status: ' + APOS.Status.ToString);
  AdicionarLinhaLog('Banco: ' + APOS.Banco);
  AdicionarLinhaLog('Chave: ' + APOS.Chave);
  AdicionarLinhaLog('Cliente ID: ' + APOS.ClienteId.ToString);
  AdicionarLinhaLog('Nome: ' + APOS.Nome);
  AdicionarLinhaLog('Criado em: ' + APOS.CreatedAt);
  AdicionarLinhaLog('Atualizado em: ' + APOS.UpdatedAt);
  AdicionarLinhaLog('Data Ativação: ' + APOS.DateAtivacao);
  AdicionarLinhaLog('Deletado em: ' + APOS.DeletedAt);
  AdicionarSeparador;
end;

procedure TForm1.LogListaPOS(AListaPOS: TListaPOS);
var
  I: Integer;
begin
  AdicionarLinhaLog('=== LISTA DE POS ===');
  AdicionarLinhaLog('Total: ' + AListaPOS.Total.ToString);
  AdicionarLinhaLog('Por Página: ' + AListaPOS.PerPage.ToString);
  AdicionarLinhaLog('Página Atual: ' + AListaPOS.CurrentPage.ToString);
  AdicionarLinhaLog('Última Página: ' + AListaPOS.LastPage.ToString);
  AdicionarLinhaLog('Até: ' + AListaPOS.ToPage.ToString);
  AdicionarLinhaLog('Registros encontrados: ' + AListaPOS.Data.Count.ToString);
  AdicionarSeparador;

  for I := 0 to AListaPOS.Data.Count - 1 do
  begin
    LogPOS(AListaPOS.Data[I], 'POS #' + (I + 1).ToString);
  end;
end;

procedure TForm1.LogExcluirPOSResponse(AResponse: TExcluirPOSResponse);
begin
  AdicionarLinhaLog('=== EXCLUSÃO DE POS ===');
  AdicionarLinhaLog('Mensagem: ' + AResponse.Message);
  AdicionarSeparador;
end;

procedure TForm1.LogRejeitarPagamentoResponse(
		  AResponse: TRejeitarPagamentoResponse);
begin
  AdicionarLinhaLog('=== REJEIÇÃO DE PAGAMENTO ===');
  AdicionarLinhaLog('Status: ' + BoolToStr(AResponse.Status, True));
  AdicionarLinhaLog('Mensagem: ' + AResponse.Message);
  AdicionarLinhaLog('');
  LogPagamento(AResponse.Data, 'DADOS DO PAGAMENTO REJEITADO');
end;

procedure TForm1.LogPaymentOrder(APaymentOrder: TPaymentOrder;
		  const AIndentacao: string);
begin
  AdicionarLinhaLog(AIndentacao + 'PAYMENT ORDER:');
  AdicionarLinhaLog(AIndentacao + '  POS ID: ' + APaymentOrder.PosId.ToString);
  AdicionarLinhaLog(AIndentacao + '  Tipo Parcelamento: ' + APaymentOrder.InstallmentType.ToString);
  AdicionarLinhaLog(AIndentacao + '  Tipo Transação: ' + APaymentOrder.TransactionType);
  AdicionarLinhaLog(AIndentacao + '  Qtd Parcelas: ' + APaymentOrder.InstallmentCount.ToString);
  AdicionarLinhaLog(AIndentacao + '  Valor: ' + FormatFloat('#,##0.00', APaymentOrder.Amount));
  AdicionarLinhaLog(AIndentacao + '  Order ID: ' + APaymentOrder.OrderId);
  AdicionarLinhaLog(AIndentacao + '  Descrição: ' + APaymentOrder.Description);
end;

procedure TForm1.LogPaymentData(APaymentData: TPaymentData;
		  const AIndentacao: string);
begin
  AdicionarLinhaLog(AIndentacao + 'PAYMENT DATA:');
  AdicionarLinhaLog(AIndentacao + '  POS ID: ' + APaymentData.PosId.ToString);
  AdicionarLinhaLog(AIndentacao + '  ID Payment: ' + APaymentData.IdPayment);
  AdicionarLinhaLog(AIndentacao + '  Nome Portador: ' + APaymentData.CardholderName);
  AdicionarLinhaLog(AIndentacao + '  Bandeira: ' + APaymentData.Brand);
  AdicionarLinhaLog(AIndentacao + '  NSU: ' + APaymentData.NSU);
  AdicionarLinhaLog(AIndentacao + '  Código Autorização: ' + APaymentData.AuthorizationCode);
  AdicionarLinhaLog(AIndentacao + '  Data/Hora Autorização: ' + APaymentData.AuthorizationDateTime);
  AdicionarLinhaLog(AIndentacao + '  Banco Adquirente: ' + APaymentData.AcquirerBanco);
  AdicionarLinhaLog(AIndentacao + '  CNPJ Adquirente: ' + APaymentData.AcquirerCNPJ);
end;


end.

