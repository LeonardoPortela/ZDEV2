interface ZIF_ACTIVE_DIRECTORY
  public .


  data AT_DADOS_SERVICO type ZWAT0002 .
  class-data AT_ACTIVE_DIRECTORY type ref to ZIF_ACTIVE_DIRECTORY .

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCE) type ref to ZIF_ACTIVE_DIRECTORY
    raising
      ZCX_ACTIVE_DIRECTORY .
  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_ACTIVE_DIRECTORY .
  methods GET_AUTENTICA
    importing
      !I_DOMINIO type ZDE_DOMINIO
      !I_USUARIO type ZDE_USUARIO
      !I_SENHA type ZDE_SENHA
    returning
      value(R_INSTANCE) type ref to ZIF_ACTIVE_DIRECTORY
    raising
      ZCX_ACTIVE_DIRECTORY .
  methods SET_HTTP
    importing
      !I_URL type STRING
      !I_JSON type STRING
    exporting
      value(E_CDATA) type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_ACTIVE_DIRECTORY
    raising
      ZCX_ACTIVE_DIRECTORY .
  methods GET_NOMES_ALEATORIOS
    importing
      !I_NOME_COMPLETO type STRING
    exporting
      value(E_NOMES) type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_ACTIVE_DIRECTORY
    raising
      ZCX_ACTIVE_DIRECTORY .
  methods GET_INFOS_USUARIO
    importing
      !I_CPF type STRING
    exporting
      !E_INFO type ZDE_RESPONSE_AD_GET_USUARIO
    returning
      value(R_INSTANCE) type ref to ZIF_ACTIVE_DIRECTORY
    raising
      ZCX_ACTIVE_DIRECTORY .
  methods SET_ADD_USUARIO
    importing
      !I_CPF type STRING
      !I_NOME_COMPLETO type STRING
      !I_PAIS type STRING
    exporting
      !E_RESPOSTA type ZDE_RESPONSE_AD_ADD_USUARIO
    returning
      value(R_INSTANCE) type ref to ZIF_ACTIVE_DIRECTORY
    raising
      ZCX_ACTIVE_DIRECTORY .
  methods SET_ATIVAR
    importing
      !I_CPF type STRING
    exporting
      !E_RESPOSTA type ZDE_RESPONSE_AD_SET_ATIVAR
    returning
      value(R_INSTANCE) type ref to ZIF_ACTIVE_DIRECTORY
    raising
      ZCX_ACTIVE_DIRECTORY .
  methods SET_BLOQUEAR
    importing
      !I_CPF type STRING
      !I_MOTIVO type STRING
      !I_DESLIGADO type STRING optional
      !I_TRANSFERIDO type STRING optional
    exporting
      !E_RESPOSTA type ZDE_RESPONSE_AD_SET_ATIVAR
    returning
      value(R_INSTANCE) type ref to ZIF_ACTIVE_DIRECTORY
    raising
      ZCX_ACTIVE_DIRECTORY .
endinterface.
