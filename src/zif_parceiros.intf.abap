interface ZIF_PARCEIROS
  public .


  data PARCEIRO type J_1BPARID .
  data NOME type NAME1_GP .
  data TIPO_PESSOA type STKZN .
  data INSC_ESTADUAL type STCD3 .
  constants ST_PESSOA_FISICA type CHAR01 value 'F' ##NO_TEXT.
  constants ST_PESSOA_JURIDICA type CHAR01 value 'J' ##NO_TEXT.
  class-data AT_PARCEIROS type ref to ZIF_PARCEIROS .
  data REGIO type REGIO .
  data DLGRP type DLGRP .
  data SCACD type SCACD .
  data TXJCD type TXJCD .
  data LAND1 type LAND1_GP .

  class-methods GET_INSTANCE
    returning
      value(PARCEIRO) type ref to ZIF_PARCEIROS .
  methods SET_PARCEIRO
    importing
      !I_PARCEIRO type J_1BPARID
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods CK_ATIVO
    importing
      !I_CK_SD type CHAR01 optional
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods CK_ATIVO_EMPRESA
    importing
      !I_EMPRESA type BUKRS
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods CK_PARCEIRO_EMP_DIFERENTE
    importing
      !I_EMPRESA type BUKRS
    exporting
      !E_J_1BBRANCH type J_1BBRANCH
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods CK_PARCEIRO_INTERCOMPANY
    importing
      !I_EMPRESA type BUKRS
    exporting
      !E_J_1BBRANCH type J_1BBRANCH
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods CK_PARCEIRO_LOCAL_NEGOCIO
    exporting
      !E_J_1BBRANCH type J_1BBRANCH
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods CK_RESTRICAO_EMBARGO
    importing
      !I_GERA_ERRO type CHAR01 default 'X'
    exporting
      !E_RESULTADO type ZDE_PES_RESULTADO_RESTRICAO
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods GET_NAME
    exporting
      !E_NAME type NAME1_GP
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS .
  methods GET_TIPO_PARCEIRO
    exporting
      !E_TIPO type CHAR01
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS .
  methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_PARCEIROS .
  methods SET_PARCEIRO_IE
    importing
      !I_INSC_ESTATUAL type STCD3
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods SET_PARCEIRO_CNPJ_CPF_IE
    importing
      !I_CNPJ type J_1BCGC optional
      !I_CPF type J_1BCPF optional
      !I_INSC_ESTATUAL type J_1BSTAINS optional
      !I_KTOKK type ZDE_KTOK_RANGE_T optional
      !I_AGNORAR_BLOQUEIO type CHAR01 default ' '
      !I_CK_IE type CHAR01 optional
      !I_FORNE_FRETE type CHAR01 optional
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods GET_ID_PARCEIRO
    exporting
      !E_PARCEIRO type J_1BPARID
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods CK_PARCEIRO_TERCEIRO
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods GET_REGIO
    exporting
      !E_REGIO type REGIO
      !E_LAND1 type LAND1_GP
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods GET_TXJCD
    exporting
      !E_TXJCD type TXJCD
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS .
  methods CK_SERVICO_FRETE
    returning
      value(R_INSTANCE) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods CK_SERVICO_FRETE_RODO
    returning
      value(R_INSTANCE) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods SET_PARCEIRO_TABELA
    importing
      !I_TABELA type ANY
    returning
      value(R_INSTANCE) type ref to ZIF_PARCEIROS .
  methods GET_DADOS_BANCARIOS
    exporting
      !E_BANCOS type MASSLFBK_T
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods CK_EMISSOR_NF_E
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS .
  methods SET_CREATE_PARTINER
    importing
      !I_DATA type DATA
      !I_FORNE_FRETE type CHAR01 optional
    exporting
      !E_OUTBOUND_FORNE type ZFIE_VENDOR
      !E_OUTBOUND_BANCO type ZFIE_BANK_T
      !E_MSG type STRING
    returning
      value(R_IF_PARCEIROS) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS
      ZCX_SHDB
      ZCX_ERROR .
  methods GET_EMAIL
    exporting
      !E_MAIL type AD_SMTPADR
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS .
  methods GET_ENDERECO
    exporting
      !E_ENDERECO type ADDR1_VAL
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS
    raising
      ZCX_PARCEIROS
      ZCX_ERROR .
  methods GET_OUTBOUND_MSG
    importing
      !I_BUKRS type BUKRS
    exporting
      !E_MSG type DATA
    returning
      value(R_PARCEIRO) type ref to ZIF_PARCEIROS .
endinterface.
