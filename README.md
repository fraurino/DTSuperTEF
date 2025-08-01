# 📘 Manual do Componente DTSuperTEF (Atualizado)

<p align="center">
  <img src="https://img.shields.io/badge/License-MIT-blue.svg" alt="License">
  <img src="https://img.shields.io/badge/Platform-Windows-lightgrey.svg" alt="Platform">
  <img src="https://img.shields.io/badge/API-REST-green.svg" alt="API">
</p>

## 🔖 Sobre o SuperTEF

O **SuperTEF** é uma solução de pagamento digital com foco em automação comercial. O componente **DTSuperTEF** permite integrar com a API SuperTEF diretamente em projetos Delphi para realizar gestão de clientes, terminais POS e transações.

---

## ✨ Recursos Implementados

### 👥 Clientes

* Criar, listar, detalhar e atualizar clientes

### 🏪 POS (Terminais)

* Criar, listar, detalhar, atualizar e excluir terminais

### 💳 Pagamentos

* Criar pagamento com suporte a parcelamento
* Listar histórico com filtros e paginação
* Detalhar e cancelar pagamento

---

## 🔧 Inicialização do Componente

Antes de usar qualquer método, defina o token de autenticação:

```pascal
DTSuperTEF1.Token := 'seu_token_aqui';
```

Esse token é obtido no portal SuperTEF, na área de configurações da API.

---

## 📄 Clientes

### Criar Cliente

```pascal
Cliente := DTSuperTEF1.CriarCliente(
  Ativo,
  CNPJ_CPF,
  NomeEmpresa,
  Contato,
  LimitePOS,
  SitefEmpresa,
  SitefCNPJ,
  SitefBanco
);
```

**Retorno:** Objeto `TCliente`

* `Id: Integer`
* `Status: Boolean`
* `Ativo: Integer`
* `CNPJ_CPF: string`
* `NomeEmpresa: string`
* `Contato: string`
* `LimitePOS: Integer`
* `SitefEmpresa: string`
* `SitefCNPJ_CPF: string`
* `SitefBanco: string`
* `Chave: string`
* `SofthouseId: Integer`
* `CreatedAt: string`
* `UpdatedAt: string`

### Listar Clientes

```pascal
ListaClientes := DTSuperTEF1.ListarClientes;
```

**Retorno:** Objeto `TListaClientes`

* `Total: Integer`
* `PerPage: Integer`
* `CurrentPage: Integer`
* `LastPage: Integer`
* `ToPage: Integer`
* `Data: TList<TCliente>`

### Detalhar Cliente

```pascal
Cliente := DTSuperTEF1.DetalharCliente(ChaveDoCliente);
```

**Retorno:** Mesmo do `CriarCliente`

### Atualizar Cliente

```pascal
Cliente := DTSuperTEF1.AtualizarCliente(...);
```

**Retorno:** Mesmo do `CriarCliente`

---

## 🏦 POS (Terminais)

### Criar POS

```pascal
POS := DTSuperTEF1.CriarPOS(ChaveDoCliente, NomePOS);
```

**Retorno:** Objeto `TPOS`

* `Id: Integer`
* `Status: Integer`
* `Banco: string`
* `Chave: string`
* `ClienteId: Integer`
* `Nome: string`
* `CreatedAt: string`
* `UpdatedAt: string`
* `DateAtivacao: string`
* `DeletedAt: string`

### Listar POS

```pascal
ListaPOS := DTSuperTEF1.ListarPOS(ChaveDoCliente);
```

**Retorno:** Objeto `TListaPOS`

* `Total, PerPage, CurrentPage, LastPage, ToPage`
* `Data: TList<TPOS>`

### Detalhar POS

```pascal
POS := DTSuperTEF1.DetalharPOS(IdPOS);
```

**Retorno:** Mesmo do `CriarPOS`

### Atualizar POS

```pascal
POS := DTSuperTEF1.AtualizarPOS(IdPOS, ChaveCliente, NovoNome);
```

**Retorno:** Mesmo do `CriarPOS`

### Excluir POS

```pascal
Response := DTSuperTEF1.ExcluirPOS(IdPOS, ChaveCliente);
```

**Retorno:** Objeto `TExcluirPOSResponse`

* `Message: string`

---

## 💰 Pagamentos

### Criar Pagamento

```pascal
Pagamento := DTSuperTEF1.CriarPagamento(
  ChaveCliente,
  IdPOS,
  TipoTransacao,       // 'CREDIT', 'DEBIT', etc.
  Parcelas,
  TipoParcelamento,    // 1=Loja, 3=Administradora
  Valor,               // Ex: 5000 para R$50,00
  OrderID,
  Descricao
);
```

**Retorno:** Objeto `TPagamento`

* `PaymentUniqueid: Integer`
* `CreatedAt: string`
* `PaymentStatus: Integer`
* `PaymentMessage: string`
* `PaymentOrder: TPaymentOrder`

  * `PosId, InstallmentType, TransactionType, InstallmentCount, Amount, OrderId, Description`
* `PaymentData: TPaymentData`

  * `PosId, IdPayment, CardholderName, Brand, NSU, AuthorizationCode, AuthorizationDateTime, AcquirerBanco, AcquirerCNPJ`

### Listar Pagamentos

```pascal
ListaPagamentos := DTSuperTEF1.ListarPagamentos(...);
```

**Retorno:** Objeto `TListaPagamentos`

* `Total, PerPage, CurrentPage, LastPage, ToPage`
* `Data: TList<TPagamento>`

### Detalhar Pagamento

```pascal
Pagamento := DTSuperTEF1.DetalharPagamento(PaymentUniqueID);
```

**Retorno:** Mesmo do `CriarPagamento`

### Cancelar/Rejeitar Pagamento

```pascal
Response := DTSuperTEF1.RejeitarPagamento(PaymentUniqueID);
```

**Retorno:** `TRejeitarPagamentoResponse`

* `Status: Boolean`
* `Message: string`
* `Data: TPagamento` (completo)

---

## 📖 Documentação Complementar

### Tipos de Transação Suportados

| Tipo   | Descrição         |
| ------ | ----------------- |
| CREDIT | Cartão de crédito |
| DEBIT  | Cartão de débito  |
| PIX    | Pagamento via Pix |

### Parcelamento

| Tipo | Forma                         |
| ---- | ----------------------------- |
| 1/2  | Parcelado pela loja           |
| 3    | Parcelado pela administradora |

---

## 🤝 Colaboradores

* Carlos - i9fast
* Gabriel - Vedas Sistemas
* Tiago - DT Inovação

---

## 🚫 Suporte

* [suporte@supertef.com.br](mailto:suporte@supertef.com.br)
* Grupo WhatsApp: [https://chat.whatsapp.com/LCOrwL3tEcDBBggRHF5E0E](https://chat.whatsapp.com/LCOrwL3tEcDBBggRHF5E0E)

---

<p align="center">
  Desenvolvido com ❤️ para a comunidade Delphi
</p>
