unit DTSuperTEF;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.StrUtils,
  REST.Client, REST.Types, System.Generics.Collections;

type
  // === CLASSES DE RETORNO TIPADAS ===

  // Cliente
  TCliente = class
  private
    FStatus: Boolean;
    FAtivo: Integer;
    FCNPJ_CPF: string;
    FNomeEmpresa: string;
    FContato: string;
    FLimitePOS: Integer;
    FSitefEmpresa: string;
    FSitefCNPJ_CPF: string;
    FSitefBanco: string;
    FChave: string;
    FSofthouseId: Integer;
    FUpdatedAt: string;
    FCreatedAt: string;
    FId: Integer;
  public
    property Status: Boolean read FStatus write FStatus;
    property Ativo: Integer read FAtivo write FAtivo;
    property CNPJ_CPF: string read FCNPJ_CPF write FCNPJ_CPF;
    property NomeEmpresa: string read FNomeEmpresa write FNomeEmpresa;
    property Contato: string read FContato write FContato;
    property LimitePOS: Integer read FLimitePOS write FLimitePOS;
    property SitefEmpresa: string read FSitefEmpresa write FSitefEmpresa;
    property SitefCNPJ_CPF: string read FSitefCNPJ_CPF write FSitefCNPJ_CPF;
    property SitefBanco: string read FSitefBanco write FSitefBanco;
    property Chave: string read FChave write FChave;
    property SofthouseId: Integer read FSofthouseId write FSofthouseId;
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;
    property CreatedAt: string read FCreatedAt write FCreatedAt;
    property Id: Integer read FId write FId;
  end;

  TListaClientes = class
  private
    FTotal: Integer;
    FPerPage: Integer;
    FCurrentPage: Integer;
    FLastPage: Integer;
    FTo: Integer;
    FData: TObjectList<TCliente>;
  public
    constructor Create;
    destructor Destroy; override;
    property Total: Integer read FTotal write FTotal;
    property PerPage: Integer read FPerPage write FPerPage;
    property CurrentPage: Integer read FCurrentPage write FCurrentPage;
    property LastPage: Integer read FLastPage write FLastPage;
    property ToPage: Integer read FTo write FTo;
    property Data: TObjectList<TCliente> read FData;
  end;

  // POS
  TPOS = class
  private
    FStatus: Integer;
    FBanco: string;
    FChave: string;
    FClienteId: Integer;
    FCreatedAt: string;
    FUpdatedAt: string;
    FDateAtivacao: string;
    FDeletedAt: string;
    FId: Integer;
    FNome: string;
  public
    property Status: Integer read FStatus write FStatus;
    property Banco: string read FBanco write FBanco;
    property Chave: string read FChave write FChave;
    property ClienteId: Integer read FClienteId write FClienteId;
    property CreatedAt: string read FCreatedAt write FCreatedAt;
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;
    property DateAtivacao: string read FDateAtivacao write FDateAtivacao;
    property DeletedAt: string read FDeletedAt write FDeletedAt;
    property Id: Integer read FId write FId;
    property Nome: string read FNome write FNome;
  end;

  TListaPOS = class
  private
    FTotal: Integer;
    FPerPage: Integer;
    FCurrentPage: Integer;
    FLastPage: Integer;
    FTo: Integer;
    FData: TObjectList<TPOS>;
  public
    constructor Create;
    destructor Destroy; override;
    property Total: Integer read FTotal write FTotal;
    property PerPage: Integer read FPerPage write FPerPage;
    property CurrentPage: Integer read FCurrentPage write FCurrentPage;
    property LastPage: Integer read FLastPage write FLastPage;
    property ToPage: Integer read FTo write FTo;
    property Data: TObjectList<TPOS> read FData;
  end;

  TExcluirPOSResponse = class
  private
    FMessage: string;
  public
    property Message: string read FMessage write FMessage;
  end;

  // Pagamentos
  TPaymentOrder = class
  private
    FPosId: Integer;
    FInstallmentType: Integer;
    FTransactionType: string;
    FInstallmentCount: Integer;
    FAmount: Extended;
    FOrderId: string;
    FDescription: string;
  public
    property PosId: Integer read FPosId write FPosId;
    property InstallmentType: Integer read FInstallmentType write FInstallmentType;
    property TransactionType: string read FTransactionType write FTransactionType;
    property InstallmentCount: Integer read FInstallmentCount write FInstallmentCount;
    property Amount: Extended read FAmount write FAmount;
    property OrderId: string read FOrderId write FOrderId;
    property Description: string read FDescription write FDescription;
  end;

  TPaymentData = class
  private
    FPosId: Integer;
    FIdPayment: string;
    FCardholderName: string;
    FBrand: string;
    FNSU: string;
    FAuthorizationCode: string;
    FAuthorizationDateTime: string;
    FAcquirerBanco: string;
    FAcquirerCNPJ: string;
  public
    property PosId: Integer read FPosId write FPosId;
    property IdPayment: string read FIdPayment write FIdPayment;
    property CardholderName: string read FCardholderName write FCardholderName;
    property Brand: string read FBrand write FBrand;
    property NSU: string read FNSU write FNSU;
    property AuthorizationCode: string read FAuthorizationCode write FAuthorizationCode;
    property AuthorizationDateTime: string read FAuthorizationDateTime write FAuthorizationDateTime;
    property AcquirerBanco: string read FAcquirerBanco write FAcquirerBanco;
    property AcquirerCNPJ: string read FAcquirerCNPJ write FAcquirerCNPJ;
  end;

  TPagamento = class
  private
    FPaymentUniqueid: Integer;
    FCreatedAt: string;
    FPaymentStatus: Integer;
    FPaymentMessage: string;
    FPaymentOrder: TPaymentOrder;
    FPaymentData: TPaymentData;
  public
    constructor Create;
    destructor Destroy; override;
    property PaymentUniqueid: Integer read FPaymentUniqueid write FPaymentUniqueid;
    property CreatedAt: string read FCreatedAt write FCreatedAt;
    property PaymentStatus: Integer read FPaymentStatus write FPaymentStatus;
    property PaymentMessage: string read FPaymentMessage write FPaymentMessage;
    property PaymentOrder: TPaymentOrder read FPaymentOrder;
    property PaymentData: TPaymentData read FPaymentData;
  end;

  TListaPagamentos = class
  private
    FTotal: Integer;
    FPerPage: Integer;
    FCurrentPage: Integer;
    FLastPage: Integer;
    FTo: Integer;
    FData: TObjectList<TPagamento>;
  public
    constructor Create;
    destructor Destroy; override;
    property Total: Integer read FTotal write FTotal;
    property PerPage: Integer read FPerPage write FPerPage;
    property CurrentPage: Integer read FCurrentPage write FCurrentPage;
    property LastPage: Integer read FLastPage write FLastPage;
    property ToPage: Integer read FTo write FTo;
    property Data: TObjectList<TPagamento> read FData;
  end;

  TRejeitarPagamentoResponse = class
  private
    FStatus: Boolean;
    FMessage: string;
    FData: TPagamento;
  public
    constructor Create;
    destructor Destroy; override;
    property Status: Boolean read FStatus write FStatus;
    property Message: string read FMessage write FMessage;
    property Data: TPagamento read FData;
  end;

  // === COMPONENTE PRINCIPAL ===
  TDTSuperTEF = class(TComponent)
  private
    FBaseURL: string;
    FToken: string;
    FClient: TRESTClient;
    FRequest: TRESTRequest;
    FResponse: TRESTResponse;

    procedure PrepareRequest(const AResource: string; AMethod: TRESTRequestMethod);
    function URLEncode(const S: string): string;

    // Métodos de conversão JSON para objetos tipados
    function JSONToCliente(AJSON: TJSONObject): TCliente;
    function JSONToListaClientes(AJSON: TJSONObject): TListaClientes;
    function JSONToPOS(AJSON: TJSONObject): TPOS;
    function JSONToListaPOS(AJSON: TJSONObject): TListaPOS;
    function JSONToExcluirPOSResponse(AJSON: TJSONObject): TExcluirPOSResponse;
    function JSONToPagamento(AJSON: TJSONObject): TPagamento;
    function JSONToListaPagamentos(AJSON: TJSONObject): TListaPagamentos;
    function JSONToRejeitarPagamentoResponse(AJSON: TJSONObject): TRejeitarPagamentoResponse;
    function JSONToPaymentOrder(AJSON: TJSONObject): TPaymentOrder;
    function JSONToPaymentData(AJSON: TJSONObject): TPaymentData;

    function GetJSONStringValue(AJSON: TJSONObject; const AName: string): string;
    function GetJSONIntValue(AJSON: TJSONObject; const AName: string): Integer;
    function GetJSONBoolValue(AJSON: TJSONObject; const AName: string): Boolean;
    function GetJSONFloatValue(AJSON: TJSONObject; const AName: string): Extended;

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
    ): TCliente;

    function ListarClientes: TListaClientes;

    function DetalharCliente(const Chave: string): TCliente;

    function AtualizarCliente(
      const Chave: string;
      Ativo: Integer;
      const CNPJ_CPF, NomeEmpresa, Contato: string;
      LimitePOS: Integer;
      const SitefEmpresa, SitefCNPJ_CPF, SitefBanco: string
    ): TCliente;

    // === POS ===
    function CriarPOS(const ClienteChave: string; const Nome: string): TPOS;

    function ListarPOS(const ClienteChave: string): TListaPOS;

    function DetalharPOS(APOSId: Integer): TPOS;

    function AtualizarPOS(APOSId: Integer; const ClienteChave: string; const Nome: string): TPOS;

    function ExcluirPOS(APOSId: Integer; const ClienteChave: string): TExcluirPOSResponse;

    // === Pagamentos ===
    function CriarPagamento(
      const ClienteChave: string;
      POSId: Integer;
      const TransactionType: string;
      InstallmentCount, InstallmentType: Integer;
      Amount: Extended;
      const OrderID, Description: string
    ): TPagamento;

    function ListarPagamentos(
      const aClienteChave : string;
      const aPOSId        : Integer;
      const aOrderID      : string;
      const aPage         : Integer;
      const aDataInicial  : TDateTime;
      const aDataFinal    : TDateTime): TListaPagamentos;

    function DetalharPagamento(UniqueId: Integer): TPagamento;

    function RejeitarPagamento(UniqueId: Integer): TRejeitarPagamentoResponse;

    // === Métodos de compatibilidade (retornam JSON como antes) ===
    function CriarClienteJSON(
      Ativo: Integer;
      const CNPJ_CPF, NomeEmpresa, Contato: string;
      LimitePOS: Integer;
      const SitefEmpresa, SitefCNPJ_CPF, SitefBanco: string
    ): TJSONObject;

    function ListarClientesJSON: TJSONObject;
    function DetalharClienteJSON(const Chave: string): TJSONObject;
    function AtualizarClienteJSON(
      const Chave: string;
      Ativo: Integer;
      const CNPJ_CPF, NomeEmpresa, Contato: string;
      LimitePOS: Integer;
      const SitefEmpresa, SitefCNPJ_CPF, SitefBanco: string
    ): TJSONObject;

    function CriarPOSJSON(const ClienteChave: string; const Nome: string): TJSONObject;
    function ListarPOSJSON(const ClienteChave: string): TJSONObject;
    function DetalharPOSJSON(APOSId: Integer): TJSONObject;
    function AtualizarPOSJSON(APOSId: Integer; const ClienteChave: string; const Nome: string): TJSONObject;
    function ExcluirPOSJSON(APOSId: Integer; const ClienteChave: string): TJSONObject;

    function CriarPagamentoJSON(
      const ClienteChave: string;
      POSId: Integer;
      const TransactionType: string;
      InstallmentCount, InstallmentType: Integer;
      Amount: Extended;
      const OrderID, Description: string
    ): TJSONObject;

    function ListarPagamentosJSON(
      const aClienteChave : string;
      const aPOSId        : Integer;
      const aOrderID      : string;
      const aPage         : Integer;
      const aDataInicial  : TDateTime;
      const aDataFinal    : TDateTime): TJSONObject;

    function DetalharPagamentoJSON(UniqueId: Integer): TJSONObject;
    function RejeitarPagamentoJSON(UniqueId: Integer): TJSONObject;

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

{ TListaClientes }

constructor TListaClientes.Create;
begin
  inherited;
  FData := TObjectList<TCliente>.Create(True);
end;

destructor TListaClientes.Destroy;
begin
  FData.Free;
  inherited;
end;

{ TListaPOS }

constructor TListaPOS.Create;
begin
  inherited;
  FData := TObjectList<TPOS>.Create(True);
end;

destructor TListaPOS.Destroy;
begin
  FData.Free;
  inherited;
end;

{ TPagamento }

constructor TPagamento.Create;
begin
  inherited;
  FPaymentOrder := TPaymentOrder.Create;
  FPaymentData := TPaymentData.Create;
end;

destructor TPagamento.Destroy;
begin
  FPaymentOrder.Free;
  FPaymentData.Free;
  inherited;
end;

{ TListaPagamentos }

constructor TListaPagamentos.Create;
begin
  inherited;
  FData := TObjectList<TPagamento>.Create(True);
end;

destructor TListaPagamentos.Destroy;
begin
  FData.Free;
  inherited;
end;

{ TRejeitarPagamentoResponse }

constructor TRejeitarPagamentoResponse.Create;
begin
  inherited;
  FData := TPagamento.Create;
end;

destructor TRejeitarPagamentoResponse.Destroy;
begin
  FData.Free;
  inherited;
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

function TDTSuperTEF.URLEncode(const S: string): string;
var
  I: Integer;
  Ch: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    Ch := S[I];
    if Ch in [' ', '!', '*', '''', '(', ')', ';', ':', '@', '&', '=', '+', '$',
          ',', '/', '?', '%', '#', '[', ']'] then
      Result := Result + '%' + IntToHex(Ord(Ch), 2)
    else
      Result := Result + Ch;
  end;
end;

// === Métodos auxiliares para conversão JSON ===

function TDTSuperTEF.GetJSONStringValue(AJSON: TJSONObject; const AName: string): string;
var
  JSONValue: TJSONValue;
begin
  Result := '';
  if Assigned(AJSON) then
  begin
    JSONValue := AJSON.GetValue(AName);
    if Assigned(JSONValue) and not (JSONValue is TJSONNull) then
      Result := JSONValue.Value;
  end;
end;

function TDTSuperTEF.GetJSONIntValue(AJSON: TJSONObject; const AName: string): Integer;
var
  JSONValue: TJSONValue;
begin
  Result := 0;
  if Assigned(AJSON) then
  begin
    JSONValue := AJSON.GetValue(AName);
    if Assigned(JSONValue) and not (JSONValue is TJSONNull) then
      Result := StrToIntDef(JSONValue.Value, 0);
  end;
end;

function TDTSuperTEF.GetJSONBoolValue(AJSON: TJSONObject; const AName: string): Boolean;
var
  JSONValue: TJSONValue;
begin
  Result := False;
  if Assigned(AJSON) then
  begin
    JSONValue := AJSON.GetValue(AName);
    if Assigned(JSONValue) and not (JSONValue is TJSONNull) then
      Result := (JSONValue is TJSONTrue) or (UpperCase(JSONValue.Value) = 'TRUE');
  end;
end;

function TDTSuperTEF.GetJSONFloatValue(AJSON: TJSONObject; const AName: string): Extended;
var
  JSONValue: TJSONValue;
begin
  Result := 0;
  if Assigned(AJSON) then
  begin
    JSONValue := AJSON.GetValue(AName);
    if Assigned(JSONValue) and not (JSONValue is TJSONNull) then
      Result := StrToFloatDef(JSONValue.Value, 0);
  end;
end;

function TDTSuperTEF.JSONToCliente(AJSON: TJSONObject): TCliente;
begin
  Result := TCliente.Create;
  Result.Status := GetJSONBoolValue(AJSON, 'status');
  Result.Ativo := GetJSONIntValue(AJSON, 'ativo');
  Result.CNPJ_CPF := GetJSONStringValue(AJSON, 'cnpj_cpf');
  Result.NomeEmpresa := GetJSONStringValue(AJSON, 'nome_empresa');
  Result.Contato := GetJSONStringValue(AJSON, 'contato');
  Result.LimitePOS := GetJSONIntValue(AJSON, 'limite_pos');
  Result.SitefEmpresa := GetJSONStringValue(AJSON, 'sitef_empresa');
  Result.SitefCNPJ_CPF := GetJSONStringValue(AJSON, 'sitef_cnpj_cpf');
  Result.SitefBanco := GetJSONStringValue(AJSON, 'sitef_banco');
  Result.Chave := GetJSONStringValue(AJSON, 'chave');
  Result.SofthouseId := GetJSONIntValue(AJSON, 'softhouse_id');
  Result.UpdatedAt := GetJSONStringValue(AJSON, 'updated_at');
  Result.CreatedAt := GetJSONStringValue(AJSON, 'created_at');
  Result.Id := GetJSONIntValue(AJSON, 'id');
end;

function TDTSuperTEF.JSONToListaClientes(AJSON: TJSONObject): TListaClientes;
var
  DataArray: TJSONArray;
  I: Integer;
  ClienteJSON: TJSONObject;
begin
  Result := TListaClientes.Create;
  Result.Total := GetJSONIntValue(AJSON, 'total');
  Result.PerPage := GetJSONIntValue(AJSON, 'per_page');
  Result.CurrentPage := GetJSONIntValue(AJSON, 'current_page');
  Result.LastPage := GetJSONIntValue(AJSON, 'last_page');
  Result.ToPage := GetJSONIntValue(AJSON, 'to');

  DataArray := AJSON.GetValue('data') as TJSONArray;
  if Assigned(DataArray) then
  begin
    for I := 0 to DataArray.Count - 1 do
    begin
      ClienteJSON := DataArray.Items[I] as TJSONObject;
      Result.Data.Add(JSONToCliente(ClienteJSON));
    end;
  end;
end;

function TDTSuperTEF.JSONToPOS(AJSON: TJSONObject): TPOS;
begin
  Result := TPOS.Create;
  Result.Status := GetJSONIntValue(AJSON, 'status');
  Result.Banco := GetJSONStringValue(AJSON, 'banco');
  Result.Chave := GetJSONStringValue(AJSON, 'chave');
  Result.ClienteId := GetJSONIntValue(AJSON, 'cliente_id');
  Result.CreatedAt := GetJSONStringValue(AJSON, 'created_at');
  Result.UpdatedAt := GetJSONStringValue(AJSON, 'updated_at');
  Result.DateAtivacao := GetJSONStringValue(AJSON, 'date_ativacao');
  Result.DeletedAt := GetJSONStringValue(AJSON, 'deleted_at');
  Result.Id := GetJSONIntValue(AJSON, 'id');
  Result.Nome := GetJSONStringValue(AJSON, 'nome');
end;

function TDTSuperTEF.JSONToListaPOS(AJSON: TJSONObject): TListaPOS;
var
  DataArray: TJSONArray;
  I: Integer;
  POSJSON: TJSONObject;
begin
  Result := TListaPOS.Create;
  Result.Total := GetJSONIntValue(AJSON, 'total');
  Result.PerPage := GetJSONIntValue(AJSON, 'per_page');
  Result.CurrentPage := GetJSONIntValue(AJSON, 'current_page');
  Result.LastPage := GetJSONIntValue(AJSON, 'last_page');
  Result.ToPage := GetJSONIntValue(AJSON, 'to');

  DataArray := AJSON.GetValue('data') as TJSONArray;
  if Assigned(DataArray) then
  begin
    for I := 0 to DataArray.Count - 1 do
    begin
      POSJSON := DataArray.Items[I] as TJSONObject;
      Result.Data.Add(JSONToPOS(POSJSON));
    end;
  end;
end;

function TDTSuperTEF.JSONToExcluirPOSResponse(AJSON: TJSONObject): TExcluirPOSResponse;
begin
  Result := TExcluirPOSResponse.Create;
  Result.Message := GetJSONStringValue(AJSON, 'message');
end;

function TDTSuperTEF.JSONToPaymentOrder(AJSON: TJSONObject): TPaymentOrder;
begin
  Result := TPaymentOrder.Create;
  Result.PosId := GetJSONIntValue(AJSON, 'pos_id');
  Result.InstallmentType := GetJSONIntValue(AJSON, 'installment_type');
  Result.TransactionType := GetJSONStringValue(AJSON, 'transaction_type');
  Result.InstallmentCount := GetJSONIntValue(AJSON, 'installment_count');
  Result.Amount := GetJSONFloatValue(AJSON, 'amount');
  Result.OrderId := GetJSONStringValue(AJSON, 'order_id');
  Result.Description := GetJSONStringValue(AJSON, 'description');
end;

function TDTSuperTEF.JSONToPaymentData(AJSON: TJSONObject): TPaymentData;
begin
  Result := TPaymentData.Create;
  Result.PosId := GetJSONIntValue(AJSON, 'pos_id');
  Result.IdPayment := GetJSONStringValue(AJSON, 'id_payment');
  Result.CardholderName := GetJSONStringValue(AJSON, 'cardholder_name');
  Result.Brand := GetJSONStringValue(AJSON, 'brand');
  Result.NSU := GetJSONStringValue(AJSON, 'nsu');
  Result.AuthorizationCode := GetJSONStringValue(AJSON, 'authorization_code');
  Result.AuthorizationDateTime := GetJSONStringValue(AJSON, 'authorization_date_time');
  Result.AcquirerBanco := GetJSONStringValue(AJSON, 'acquirer_banco');
  Result.AcquirerCNPJ := GetJSONStringValue(AJSON, 'acquirer_cnpj');
end;

function TDTSuperTEF.JSONToPagamento(AJSON: TJSONObject): TPagamento;
var
  OrderJSON, DataJSON: TJSONObject;
begin
  Result := TPagamento.Create;
  Result.PaymentUniqueid := GetJSONIntValue(AJSON, 'payment_uniqueid');
  Result.CreatedAt := GetJSONStringValue(AJSON, 'created_at');
  Result.PaymentStatus := GetJSONIntValue(AJSON, 'payment_status');
  Result.PaymentMessage := GetJSONStringValue(AJSON, 'payment_message');

  OrderJSON := AJSON.GetValue('payment_order') as TJSONObject;
  if Assigned(OrderJSON) then
  begin
    Result.PaymentOrder.Free;
    Result.FPaymentOrder := JSONToPaymentOrder(OrderJSON);
  end;

  DataJSON := AJSON.GetValue('payment_data') as TJSONObject;
  if Assigned(DataJSON) then
  begin
    Result.PaymentData.Free;
    Result.FPaymentData := JSONToPaymentData(DataJSON);
  end;
end;

function TDTSuperTEF.JSONToListaPagamentos(AJSON: TJSONObject): TListaPagamentos;
var
  DataArray: TJSONArray;
  I: Integer;
  PagamentoJSON: TJSONObject;
begin
  Result := TListaPagamentos.Create;
  Result.Total := GetJSONIntValue(AJSON, 'total');
  Result.PerPage := GetJSONIntValue(AJSON, 'per_page');
  Result.CurrentPage := GetJSONIntValue(AJSON, 'current_page');
  Result.LastPage := GetJSONIntValue(AJSON, 'last_page');
  Result.ToPage := GetJSONIntValue(AJSON, 'to');

  DataArray := AJSON.GetValue('data') as TJSONArray;
  if Assigned(DataArray) then
  begin
    for I := 0 to DataArray.Count - 1 do
    begin
      PagamentoJSON := DataArray.Items[I] as TJSONObject;
      Result.Data.Add(JSONToPagamento(PagamentoJSON));
    end;
  end;
end;

function TDTSuperTEF.JSONToRejeitarPagamentoResponse(AJSON: TJSONObject): TRejeitarPagamentoResponse;
var
  DataJSON: TJSONObject;
begin
  Result := TRejeitarPagamentoResponse.Create;
  Result.Status := GetJSONBoolValue(AJSON, 'status');
  Result.Message := GetJSONStringValue(AJSON, 'message');

  DataJSON := AJSON.GetValue('data') as TJSONObject;
  if Assigned(DataJSON) then
  begin
    Result.Data.Free;
    Result.FData := JSONToPagamento(DataJSON);
  end;
end;

// === MÉTODOS PRINCIPAIS COM RETORNO TIPADO ===

function TDTSuperTEF.CriarCliente(Ativo: Integer; const CNPJ_CPF, NomeEmpresa, Contato: string;
  LimitePOS: Integer; const SitefEmpresa, SitefCNPJ_CPF, SitefBanco: string): TCliente;
var
  JSON, ResponseJSON: TJSONObject;
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

    ResponseJSON := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
    try
      Result := JSONToCliente(ResponseJSON);
    finally
      ResponseJSON.Free;
    end;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.ListarClientes: TListaClientes;
var
  ResponseJSON: TJSONObject;
begin
  PrepareRequest('clientes', rmGET);
  FRequest.Execute;

  ResponseJSON := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  try
    Result := JSONToListaClientes(ResponseJSON);
  finally
    ResponseJSON.Free;
  end;
end;

function TDTSuperTEF.DetalharCliente(const Chave: string): TCliente;
var
  ResponseJSON: TJSONObject;
begin
  PrepareRequest('clientes/' + Chave, rmGET);
  FRequest.Execute;

  ResponseJSON := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  try
    Result := JSONToCliente(ResponseJSON);
  finally
    ResponseJSON.Free;
  end;
end;

function TDTSuperTEF.AtualizarCliente(const Chave: string; Ativo: Integer; const CNPJ_CPF, NomeEmpresa, Contato: string;
  LimitePOS: Integer; const SitefEmpresa, SitefCNPJ_CPF, SitefBanco: string): TCliente;
var
  JSON, ResponseJSON: TJSONObject;
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

    ResponseJSON := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
    try
      Result := JSONToCliente(ResponseJSON);
    finally
      ResponseJSON.Free;
    end;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.CriarPOS(const ClienteChave: string; const Nome: string): TPOS;
var
  JSON, ResponseJSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('cliente_chave', ClienteChave);
    JSON.AddPair('status', TJSONNumber.Create(1));
    JSON.AddPair('nome', Nome);

    PrepareRequest('pos', rmPOST);
    FRequest.AddBody(JSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;

    ResponseJSON := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
    try
      Result := JSONToPOS(ResponseJSON);
    finally
      ResponseJSON.Free;
    end;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.ListarPOS(const ClienteChave: string): TListaPOS;
var
  ResponseJSON: TJSONObject;
begin
  PrepareRequest('pos?cliente_chave=' + ClienteChave, rmGET);
  FRequest.Execute;

  ResponseJSON := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  try
    Result := JSONToListaPOS(ResponseJSON);
  finally
    ResponseJSON.Free;
  end;
end;

function TDTSuperTEF.DetalharPOS(APOSId: Integer): TPOS;
var
  ResponseJSON: TJSONObject;
begin
  PrepareRequest('pos/' + APOSId.ToString, rmGET);
  FRequest.Execute;

  ResponseJSON := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  try
    Result := JSONToPOS(ResponseJSON);
  finally
    ResponseJSON.Free;
  end;
end;

function TDTSuperTEF.AtualizarPOS(APOSId: Integer; const ClienteChave: string; const Nome: string): TPOS;
var
  JSON, ResponseJSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('cliente_chave', ClienteChave);
    JSON.AddPair('status', TJSONNumber.Create(1));
    JSON.AddPair('nome', Nome);

    PrepareRequest('pos/' + APOSId.ToString, rmPUT);
    FRequest.AddBody(JSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;

    ResponseJSON := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
    try
      Result := JSONToPOS(ResponseJSON);
    finally
      ResponseJSON.Free;
    end;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.ExcluirPOS(APOSId: Integer; const ClienteChave: string): TExcluirPOSResponse;
var
  JSON, ResponseJSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('cliente_chave', ClienteChave);

    PrepareRequest('pos/' + APOSId.ToString, rmDELETE);
    FRequest.AddBody(JSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;

    ResponseJSON := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
    try
      Result := JSONToExcluirPOSResponse(ResponseJSON);
    finally
      ResponseJSON.Free;
    end;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.CriarPagamento(const ClienteChave: string; POSId: Integer; const TransactionType: string;
  InstallmentCount, InstallmentType: Integer; Amount: Extended; const OrderID, Description: string): TPagamento;
var
  JSON, ResponseJSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('cliente_chave', ClienteChave);

    if POSId = 0 then
     JSON.AddPair('pos_id', TJSONNull.Create)
    else
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

    ResponseJSON := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
    try
      Result := JSONToPagamento(ResponseJSON);
    finally
      ResponseJSON.Free;
    end;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.ListarPagamentos(
  const aClienteChave : string;
  const aPOSId        : Integer;
  const aOrderID      : string;
  const aPage         : Integer;
  const aDataInicial  : TDateTime;
  const aDataFinal    : TDateTime): TListaPagamentos;
var
  JSON, ResponseJSON : TJSONObject;
  URL  : string;
begin
  JSON := TJSONObject.Create;
  try
    // Monta os parâmetros na URL
    URL := 'pagamentos?';

    if not aClienteChave.IsEmpty then
      URL := URL + 'cliente_chave=' + URLEncode(aClienteChave) + '&';

    URL := URL + 'pos_id=' + IfThen(aPOSId = 0, '', IntToStr(aPOSId)) + '&';

    if not aOrderID.IsEmpty then
      URL := URL + 'order_id=' + URLEncode(aOrderID) + '&';

    URL := URL + 'page=' + IfThen(aPage = 0, '', IntToStr(aPage)) + '&';

    if aDataInicial > 0 then
      URL := URL + 'data_inicial=' + FormatDateTime('yyyy-mm-dd', aDataInicial) + '&';

    if aDataFinal > 0 then
      URL := URL + 'data_final=' + FormatDateTime('yyyy-mm-dd', aDataFinal) + '&';

    // Remove o último '&' se existir
    if URL.EndsWith('&') then
      Delete(URL, Length(URL), 1);

    // Prepara e executa requisição GET
    PrepareRequest(URL, rmGET);
    FRequest.Execute;

    ResponseJSON := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
    try
      Result := JSONToListaPagamentos(ResponseJSON);
    finally
      ResponseJSON.Free;
    end;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.DetalharPagamento(UniqueId: Integer): TPagamento;
var
  ResponseJSON: TJSONObject;
begin
  PrepareRequest('pagamentos/by-uniqueid/' + UniqueId.ToString, rmGET);
  FRequest.Execute;

  ResponseJSON := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  try
    Result := JSONToPagamento(ResponseJSON);
  finally
    ResponseJSON.Free;
  end;
end;

function TDTSuperTEF.RejeitarPagamento(UniqueId: Integer): TRejeitarPagamentoResponse;
var
  ResponseJSON: TJSONObject;
begin
  PrepareRequest('pagamentos/cancelar/' + UniqueId.ToString, rmPUT);
  FRequest.Execute;

  ResponseJSON := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  try
    Result := JSONToRejeitarPagamentoResponse(ResponseJSON);
  finally
    ResponseJSON.Free;
  end;
end;

// === MÉTODOS DE COMPATIBILIDADE (mantém os retornos JSON originais) ===

function TDTSuperTEF.CriarClienteJSON(Ativo: Integer; const CNPJ_CPF, NomeEmpresa, Contato: string;
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

    PrepareRequest('clientes', rmPOST);
    FRequest.AddBody(JSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;
    Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.ListarClientesJSON: TJSONObject;
begin
  PrepareRequest('clientes', rmGET);
  FRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
end;

function TDTSuperTEF.DetalharClienteJSON(const Chave: string): TJSONObject;
begin
  PrepareRequest('clientes/' + Chave, rmGET);
  FRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
end;

function TDTSuperTEF.AtualizarClienteJSON(const Chave: string; Ativo: Integer; const CNPJ_CPF, NomeEmpresa, Contato: string;
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

function TDTSuperTEF.CriarPOSJSON(const ClienteChave: string; const Nome: string): TJSONObject;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('cliente_chave', ClienteChave);
    JSON.AddPair('status', TJSONNumber.Create(1));
    JSON.AddPair('nome', Nome);

    PrepareRequest('pos', rmPOST);
    FRequest.AddBody(JSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;
    Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.ListarPOSJSON(const ClienteChave: string): TJSONObject;
begin
  PrepareRequest('pos?cliente_chave=' + ClienteChave, rmGET);
  FRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
end;

function TDTSuperTEF.DetalharPOSJSON(APOSId: Integer): TJSONObject;
begin
  PrepareRequest('pos/' + APOSId.ToString, rmGET);
  FRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
end;

function TDTSuperTEF.AtualizarPOSJSON(APOSId: Integer; const ClienteChave: string; const Nome: string): TJSONObject;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('cliente_chave', ClienteChave);
    JSON.AddPair('status', TJSONNumber.Create(1));
    JSON.AddPair('nome', Nome);

    PrepareRequest('pos/' + APOSId.ToString, rmPUT);
    FRequest.AddBody(JSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;
    Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.ExcluirPOSJSON(APOSId: Integer; const ClienteChave: string): TJSONObject;
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

function TDTSuperTEF.CriarPagamentoJSON(const ClienteChave: string; POSId: Integer; const TransactionType: string;
  InstallmentCount, InstallmentType: Integer; Amount: Extended; const OrderID, Description: string): TJSONObject;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('cliente_chave', ClienteChave);

    if POSId = 0 then
     JSON.AddPair('pos_id', TJSONNull.Create)
    else
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

function TDTSuperTEF.ListarPagamentosJSON(
  const aClienteChave : string;
  const aPOSId        : Integer;
  const aOrderID      : string;
  const aPage         : Integer;
  const aDataInicial  : TDateTime;
  const aDataFinal    : TDateTime): TJSONObject;
var
  JSON : TJSONObject;
  URL  : string;
begin
  JSON := TJSONObject.Create;
  try
    // Monta os parâmetros na URL
    URL := 'pagamentos?';

    if not aClienteChave.IsEmpty then
      URL := URL + 'cliente_chave=' + URLEncode(aClienteChave) + '&';

    URL := URL + 'pos_id=' + IfThen(aPOSId = 0, '', IntToStr(aPOSId)) + '&';

    if not aOrderID.IsEmpty then
      URL := URL + 'order_id=' + URLEncode(aOrderID) + '&';

    URL := URL + 'page=' + IfThen(aPage = 0, '', IntToStr(aPage)) + '&';

    if aDataInicial > 0 then
      URL := URL + 'data_inicial=' + FormatDateTime('yyyy-mm-dd', aDataInicial) + '&';

    if aDataFinal > 0 then
      URL := URL + 'data_final=' + FormatDateTime('yyyy-mm-dd', aDataFinal) + '&';

    // Remove o último '&' se existir
    if URL.EndsWith('&') then
      Delete(URL, Length(URL), 1);

    // Prepara e executa requisição GET
    PrepareRequest(URL, rmGET);
    FRequest.Execute;

    Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
  finally
    JSON.Free;
  end;
end;

function TDTSuperTEF.DetalharPagamentoJSON(UniqueId: Integer): TJSONObject;
begin
  PrepareRequest('pagamentos/by-uniqueid/' + UniqueId.ToString, rmGET);
  FRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
end;

function TDTSuperTEF.RejeitarPagamentoJSON(UniqueId: Integer): TJSONObject;
begin
  PrepareRequest('pagamentos/cancelar/' + UniqueId.ToString, rmPUT);
  FRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
end;

end.
