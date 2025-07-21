# 💳 DTSuperTEF

<p align="center">  
  <img src="https://img.shields.io/badge/License-MIT-blue.svg" alt="License">
  <img src="https://img.shields.io/badge/Platform-Windows-lightgrey.svg" alt="Platform">
  <img src="https://img.shields.io/badge/API-REST-green.svg" alt="API">
</p>

Um componente Delphi para integração com a **API SuperTEF**, oferecendo funcionalidades completas para gerenciamento de clientes, POS e transações de pagamento digital.

## 🎯 Sobre o SuperTEF

O **SuperTEF** é uma solução brasileira para automação de pagamentos digitais, oferecendo tecnologia TEF (Transferência Eletrônica de Fundos) moderna e segura para estabelecimentos comerciais. A plataforma permite processar pagamentos via cartão de crédito, débito e outras modalidades eletrônicas.

## 📋 Índice

- [Funcionalidades](#-funcionalidades)
- [Instalação](#-instalação)
- [Configuração](#-configuração)
- [Uso Básico](#-uso-básico)
- [Exemplos de Uso](#-exemplos-de-uso)
- [Documentação da API](#-documentação-da-api)
- [Contribuição](#-contribuição)
- [Licença](#-licença)

## ⚡ Funcionalidades

### 👥 Gerenciamento de Clientes
- ✅ Criar novos clientes
- ✅ Listar clientes cadastrados
- ✅ Detalhar informações específicas de cliente
- ✅ Atualizar dados do cliente

### 🏪 Gerenciamento de POS
- ✅ Criar terminais POS
- ✅ Listar terminais por cliente
- ✅ Detalhar configurações de POS
- ✅ Atualizar status e configurações
- ✅ Excluir terminais

### 💰 Processamento de Pagamentos
- ✅ Criar transações de pagamento
- ✅ Listar histórico de transações
- ✅ Detalhar informações de pagamento
- ✅ Suporte a parcelamento
- ✅ Diferentes tipos de transação

## 🔧 Instalação

### Requisitos
- **Delphi xe8** ou superior
- **Rest Client** (normalmente incluído no Delphi)
- **System.JSON** (Delphi XE8+)

### Configuração Básica

```pascal
var
  SuperTEF: TDTSuperTEF;
begin
  SuperTEF := TDTSuperTEF.Create(nil);
  try
    // Configurar URL base (opcional, já vem configurada)
    SuperTEF.BaseURL := 'https://api.supertef.com.br/api';
    
    // Definir token de autenticação
    SuperTEF.Token := 'seu_token_aqui';
    
    // Usar o componente...
    
  finally
    SuperTEF.Free;
  end;
end;
```

### Obtendo Token de Autenticação

Para obter seu token de autenticação, acesse o [portal SuperTEF](https://www.supertef.com.br) e:

1. Faça login na sua conta
2. Vá para **Configurações → API**
3. Gere um novo token
4. Copie o token gerado

## 🚀 Uso Básico

### Exemplo Completo de Uso

```pascal
procedure TForm1.ExemploCompleto;
var
  SuperTEF: TDTSuperTEF;
  Response: TJSONObject;
  ClienteChave: string;
  POSId: Integer;
begin
  SuperTEF := TDTSuperTEF.Create(nil);
  try
    SuperTEF.Token := 'seu_token_aqui';
    
    // 1. Criar Cliente
    Response := SuperTEF.CriarCliente(
      1,                          // Ativo
      '12345678901234',          // CNPJ
      'Empresa Exemplo Ltda',     // Nome
      'contato@exemplo.com',      // Contato
      5,                          // Limite POS
      'Empresa TEF',              // Sitef Empresa
      '12345678901234',          // Sitef CNPJ
      '001'                       // Sitef Banco
    );
    
    // Processar resposta...
    if Response.GetValue('success').AsBoolean then
    begin
      ClienteChave := Response.GetValue('data').GetValue('chave').AsString;
      ShowMessage('Cliente criado com sucesso! Chave: ' + ClienteChave);
    end;
    Response.Free;
    
    // 2. Criar POS
    Response := SuperTEF.CriarPOS(
      ClienteChave,              // Chave do cliente
      1,                         // Status (ativo)
      'Terminal Principal'       // Nome do terminal
    );
    
    if Response.GetValue('success').AsBoolean then
    begin
      POSId := Response.GetValue('data').GetValue('id').AsInteger;
      ShowMessage('POS criado com sucesso! ID: ' + IntToStr(POSId));
    end;
    Response.Free;
    
    // 3. Criar Pagamento
    Response := SuperTEF.CriarPagamento(
      ClienteChave,              // Chave do cliente
      POSId,                     // ID do POS
      'CREDIT',                  // Tipo de transação
      1,                         // Parcelas
      1,                         // Tipo parcelamento
      10000,                     // Valor (em centavos)
      'PEDIDO001',               // ID do pedido
      'Venda de produtos'        // Descrição
    );
    
    if Response.GetValue('success').AsBoolean then
    begin
      ShowMessage('Pagamento processado com sucesso!');
    end;
    Response.Free;
    
  finally
    SuperTEF.Free;
  end;
end;
```

## 📖 Exemplos de Uso

### 👤 Trabalhando com Clientes

```pascal
// Criar cliente
Response := SuperTEF.CriarCliente(
  1,                            // Ativo (1=ativo, 0=inativo)
  '12.345.678/0001-90',        // CNPJ/CPF
  'Loja Exemplo',              // Nome da empresa
  'contato@lojaexemplo.com',   // Contato
  10,                          // Limite de POS
  'Loja TEF',                  // Empresa no SiTef
  '12.345.678/0001-90',       // CNPJ no SiTef
  '341'                        // Código do banco
);

// Listar todos os clientes
Response := SuperTEF.ListarClientes;

// Detalhar cliente específico
Response := SuperTEF.DetalharCliente('chave_do_cliente');

// Atualizar cliente
Response := SuperTEF.AtualizarCliente(
  'chave_do_cliente',          // Chave do cliente
  1,                           // Ativo
  '12.345.678/0001-90',       // CNPJ atualizado
  'Novo Nome da Loja',         // Nome atualizado
  'novo@email.com',            // Novo contato
  15,                          // Novo limite
  'Nova Empresa TEF',          // Nova empresa SiTef
  '12.345.678/0001-90',       // CNPJ SiTef
  '237'                        // Novo banco
);
```

### 🏪 Gerenciando Terminais POS

```pascal
// Criar terminal POS
Response := SuperTEF.CriarPOS(
  'chave_do_cliente',          // Chave do cliente
  1,                           // Status (1=ativo, 0=inativo)
  'Caixa 01'                   // Nome do terminal
);

// Listar terminais de um cliente
Response := SuperTEF.ListarPOS('chave_do_cliente');

// Detalhar terminal específico
Response := SuperTEF.DetalharPOS(123); // ID do POS

// Atualizar terminal
Response := SuperTEF.AtualizarPOS(
  123,                         // ID do POS
  'chave_do_cliente',          // Chave do cliente
  1,                           // Status
  'Caixa Principal'            // Novo nome
);

// Excluir terminal
Response := SuperTEF.ExcluirPOS(123, 'chave_do_cliente');
```

### 💳 Processando Pagamentos

```pascal
// Pagamento à vista
Response := SuperTEF.CriarPagamento(
  'chave_do_cliente',          // Chave do cliente
  123,                         // ID do POS
  'CREDIT',                    // Tipo: CREDIT, DEBIT, PIX
  1,                           // Parcelas
  1,                           // Tipo parcelamento (1 ou 2=loja, 3=admin (Cliente))
  5000,                        // Valor em centavos (R$ 50,00)
  'VENDA001',                  // ID único do pedido
  'Compra de produtos'         // Descrição
);

// Pagamento parcelado
Response := SuperTEF.CriarPagamento(
  'chave_do_cliente',
  123,
  'CREDIT',
  3,                           // 3 parcelas
  2,                           // Parcelamento administradora
  15000,                       // R$ 150,00
  'VENDA002',
  'Compra parcelada'
);

// Consultar histórico de pagamentos
Response := SuperTEF.ListarPagamentos;

// Detalhar pagamento específico
Response := SuperTEF.DetalharPagamento(456); // Unique ID
```

## 📊 Tratamento de Respostas

### Estrutura de Resposta Padrão

```pascal
procedure ProcessarResposta(Response: TJSONObject);
var
  Success: Boolean;
  Message: string;
  Data: TJSONObject;
begin
  Success := Response.GetValue('success').AsBoolean;
  Message := Response.GetValue('message').AsString;
  
  if Success then
  begin
    Data := Response.GetValue('data') as TJSONObject;
    // Processar dados...
    ShowMessage('Operação realizada com sucesso!');
  end
  else
  begin
    ShowMessage('Erro: ' + Message);
  end;
end;
```

### Tratamento de Erros

```pascal
procedure TratarErros(Response: TJSONObject);
var
  ErrorCode: Integer;
  ErrorMessage: string;
begin
  if not Response.GetValue('success').AsBoolean then
  begin
    ErrorCode := Response.GetValue('error_code').AsInteger;
    ErrorMessage := Response.GetValue('message').AsString;
    
    case ErrorCode of
      400: ShowMessage('Dados inválidos: ' + ErrorMessage);
      401: ShowMessage('Token de autenticação inválido');
      403: ShowMessage('Acesso negado');
      404: ShowMessage('Recurso não encontrado');
      500: ShowMessage('Erro interno do servidor');
    else
      ShowMessage('Erro desconhecido: ' + ErrorMessage);
    end;
  end;
end;
```

## 📋 Documentação da API

### Tipos de Transação Suportados

| Tipo | Descrição |
|------|-----------|
| `CREDIT` | Cartão de crédito |
| `DEBIT` | Cartão de débito |
| `PIX` | Pagamento via PIX |
| `VOUCHER` | Vale refeição/alimentação |

### Tipos de Parcelamento

| Tipo | Descrição |
|------|-----------|
| `1 ou 2` | Parcelamento pela loja |
| `3` | Parcelamento pela administradora (Cliente) |

### Status de POS

| Status | Descrição |
|--------|-----------|
| `0` | Inativo |
| `1` | Ativo |

## 🔒 Segurança

- **Token de Autenticação**: Sempre mantenha seu token em segurança
- **HTTPS**: Todas as comunicações são feitas via HTTPS
- **Validação**: Sempre valide as respostas da API
- **Logs**: Implemente logs para auditoria das transações

## 🤝 Contribuição

Contribuições são bem-vindas! Para contribuir:

1. Faça um fork do projeto
2. Crie uma branch para sua feature (`git checkout -b feature/AmazingFeature`)
3. Commit suas mudanças (`git commit -m 'Add some AmazingFeature'`)
4. Push para a branch (`git push origin feature/AmazingFeature`)
5. Abra um Pull Request

## 📄 Licença

Este projeto está sob a licença MIT. Veja o arquivo `LICENSE` para mais detalhes.

## 🔗 Links Úteis

- [Site Oficial SuperTEF](https://www.supertef.com.br)
- [Documentação da API](https://supertef.apidog.io)


## 📞 Suporte

Para suporte técnico:
- 📧 Email: suporte@supertef.com.br
- 💬 Grupo de apoio: https://chat.whatsapp.com/LCOrwL3tEcDBBggRHF5E0E

Projeto criado por:

Carlos - i9fast
Gabriel - Vedas sistemas
Tiago - DT Inovacao

---

<p align="center">
  Desenvolvido com ❤️ para a comunidade Delphi

</p>

<p align="center">
  <a href="https://www.supertef.com.br">
    <img src="https://img.shields.io/badge/Powered%20by-SuperTEF-blue.svg" alt="Powered by SuperTEF">
  </a>
</p>