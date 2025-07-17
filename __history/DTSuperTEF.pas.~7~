unit DTSuperTEF;

interface

uses
  System.SysUtils, System.Classes, System.JSON, REST.Client, REST.Types;

type
  TDTSuperTEF = class(TComponent)
  private
    FBaseURL: string;
    FToken: string;
    FClient: TRESTClient;
    FRequest: TRESTRequest;
    FResponse: TRESTResponse;

    procedure PrepareRequest(const AResource: string; AMethod: TRESTRequestMethod);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetToken(const AToken: string);

    // === Clientes ===
    function CriarCliente(
      Ativo: Integer;
      const CNPJ_CPF, NomeEmpresa, Contato: string;
      LimitePOS: Integer;
      const SitefEmpresa, SitefCNPJ_CPF, SitefBanco: string
    ): TJSONObject;

    function ListarClientes: TJSONObject;

    function DetalharCliente(const Chave: string): TJSONObject;

    function AtualizarCliente(
      const Chave: string;
      Ativo: Integer;
      const CNPJ_CPF, NomeEmpresa, Contato: string;
      LimitePOS: Integer;
      const SitefEmpresa, SitefCNPJ_CPF, SitefBanco: string
    ): TJSONObject;

    // === POS ===
    function CriarPOS(const ClienteChave: string; Status: Integer; const Nome: string): TJSONObject;

    function ListarPOS(const ClienteChave: string): TJSONObject;

    function DetalharPOS(APOSId: Integer): TJSONObject;

    function AtualizarPOS(APOSId: Integer; const ClienteChave: string; Status: Integer; const Nome: string): TJSONObject;

    function ExcluirPOS(APOSId: Integer; const ClienteChave: string): TJSONObject;

    // === Pagamentos ===
    function CriarPagamento(
      const ClienteChave: string;
      POSId: Integer;
      const TransactionType: string;
      InstallmentCount, InstallmentType: Integer;
      Amount: Integer;
      const OrderID, Description: string
    ): TJSONObject;

    function ListarPagamentos: TJSONObject;

    function DetalharPagamento(UniqueId: Integer): TJSONObject;

  published
    property BaseURL: string read FBaseURL write FBaseURL;
    property Token: string read FToken write FToken;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DT Inovacao', [TDTSuperTEF]);
end;

{ TDTSuperTEF }

constructor TDTSuperTEF.Create(AOwner: TComponent);
begin
  inherited;
  FBaseURL := 'https://api.supertef.com.br/api';
  FClient := TRESTClient.Create(FBaseURL);
  FRequest := TRESTRequest.Create(nil);
  FResponse := TRESTResponse.Create(nil);

  FRequest.Client := FClient;
  FRequest.Response := FResponse;
end;

destructor TDTSuperTEF.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  FClient.Free;
  inherited;
end;

procedure TDTSuperTEF.PrepareRequest(const AResource: string; AMethod: TRESTRequestMethod);
begin
  FClient.BaseURL := FBaseURL;
  FRequest.Resource := AResource;
  FRequest.Method := AMethod;
  FRequest.Params.Clear;
  FRequest.AddParameter('Authorization', 'Bearer ' + FToken, pkHTTPHEADER, [poDoNotEncode]);
  FRequest.Body.ClearBody;
end;

procedure TDTSuperTEF.SetToken(const AToken: string);
begin
  FToken := AToken;
end;

// === CLIENTES ===

function TDTSuperTEF.CriarCliente(Ativo: Integer; const CNPJ_CPF, NomeEmpresa, Contato: string; LimitePOS: Integer;
  const SitefEmpresa, SitefCNPJ_CPF, SitefBanco: string): TJSONObject;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('ativo', TJSONNumber.Create(Ativo));
    JSON.AddPair('cnpj_cpf', CNPJ_CPF);
    JSON.AddPair('nome_empresa', NomeEmpresa);
    JSON.AddPair('contato', Contato);
    JSON.AddPair('limite_pos', TJSONNumber.Create(LimitePOS));
    JSON.AddPair('sitef_empresa', SitefEmpresa);
    JSON.AddPair('sitef_cnpj_cpf', SitefCNPJ_CPF);
    JSON.AddPair('sitef_banco', SitefBanco);

    PrepareRequest('clientes', rmPOST);
    FRequest.AddBody(JSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;
    Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.ListarClientes: TJSONObject;
begin
  PrepareRequest('clientes', rmGET);
  FRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
end;

function TDTSuperTEF.DetalharCliente(const Chave: string): TJSONObject;
begin
  PrepareRequest('clientes/' + Chave, rmGET);
  FRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
end;

function TDTSuperTEF.AtualizarCliente(const Chave: string; Ativo: Integer; const CNPJ_CPF, NomeEmpresa, Contato: string;
  LimitePOS: Integer; const SitefEmpresa, SitefCNPJ_CPF, SitefBanco: string): TJSONObject;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('ativo', TJSONNumber.Create(Ativo));
    JSON.AddPair('cnpj_cpf', CNPJ_CPF);
    JSON.AddPair('nome_empresa', NomeEmpresa);
    JSON.AddPair('contato', Contato);
    JSON.AddPair('limite_pos', TJSONNumber.Create(LimitePOS));
    JSON.AddPair('sitef_empresa', SitefEmpresa);
    JSON.AddPair('sitef_cnpj_cpf', SitefCNPJ_CPF);
    JSON.AddPair('sitef_banco', SitefBanco);

    PrepareRequest('clientes/' + Chave, rmPUT);
    FRequest.AddBody(JSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;
    Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  finally
    JSON.Free;
  end;
end;

// === POS ===

function TDTSuperTEF.CriarPOS(const ClienteChave: string; Status: Integer; const Nome: string): TJSONObject;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('cliente_chave', ClienteChave);
    JSON.AddPair('status', TJSONNumber.Create(Status));
    JSON.AddPair('nome', Nome);

    PrepareRequest('pos', rmPOST);
    FRequest.AddBody(JSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;
    Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.ListarPOS(const ClienteChave: string): TJSONObject;
begin
  PrepareRequest('pos?cliente_chave=' + ClienteChave, rmGET);
  FRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
end;

function TDTSuperTEF.DetalharPOS(APOSId: Integer): TJSONObject;
begin
  PrepareRequest('pos/' + APOSId.ToString, rmGET);
  FRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
end;

function TDTSuperTEF.AtualizarPOS(APOSId: Integer; const ClienteChave: string; Status: Integer; const Nome: string): TJSONObject;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('cliente_chave', ClienteChave);
    JSON.AddPair('status', TJSONNumber.Create(Status));
    JSON.AddPair('nome', Nome);

    PrepareRequest('pos/' + APOSId.ToString, rmPUT);
    FRequest.AddBody(JSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;
    Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.ExcluirPOS(APOSId: Integer; const ClienteChave: string): TJSONObject;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('cliente_chave', ClienteChave);

    PrepareRequest('pos/' + APOSId.ToString, rmDELETE);
    FRequest.AddBody(JSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;
    Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  finally
    JSON.Free;
  end;
end;

// === PAGAMENTOS ===

function TDTSuperTEF.CriarPagamento(const ClienteChave: string; POSId: Integer; const TransactionType: string;
  InstallmentCount, InstallmentType: Integer; Amount: Integer; const OrderID, Description: string): TJSONObject;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('cliente_chave', ClienteChave);
    JSON.AddPair('pos_id', TJSONNumber.Create(POSId));
    JSON.AddPair('transaction_type', TransactionType);
    JSON.AddPair('installment_count', TJSONNumber.Create(InstallmentCount));
    JSON.AddPair('installment_type', TJSONNumber.Create(InstallmentType));
    JSON.AddPair('amount', TJSONNumber.Create(Amount));
    JSON.AddPair('order_id', OrderID);
    JSON.AddPair('description', Description);

    PrepareRequest('pagamentos', rmPOST);
    FRequest.AddBody(JSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;
    Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.ListarPagamentos: TJSONObject;
begin
  PrepareRequest('pagamentos', rmGET);
  FRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
end;

function TDTSuperTEF.DetalharPagamento(UniqueId: Integer): TJSONObject;
begin
  PrepareRequest('pagamentos/by-uniqueid/' + UniqueId.ToString, rmGET);
  FRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
end;

end.

