interface ZIF_SOL_MOBILE_RH
  public .


  data CK_PROCESSO_FILHO type CHAR01 .
  data CK_ERRO_LOGIN type CHAR01 .
  data SOLICITACAO type ZHCMT_PA_0011 .
  data LOGS type ZHCMT_PA_0012_T .
  constants ST_STATUS_PENDENTE type ZDE_STATUS_SOLICITACAO value '00' ##NO_TEXT.
  constants ST_STATUS_ATIVO type ZDE_STATUS_SOLICITACAO value '01' ##NO_TEXT.
  constants ST_STATUS_DESATIVADO type ZDE_STATUS_SOLICITACAO value '02' ##NO_TEXT.
  constants ST_STATUS_BLOQUEADO type ZDE_STATUS_SOLICITACAO value '03' ##NO_TEXT.

  class-methods SET_ENQUEUE
    importing
      !I_ID_SOLICITACAO type ZDE_ID_SOLICITACAO
    raising
      ZCX_SOL_MOBILE_RH .
  class-methods SET_DENQUEUE
    importing
      !I_ID_SOLICITACAO type ZDE_ID_SOLICITACAO
    raising
      ZCX_SOL_MOBILE_RH .
  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_SOL_MOBILE_RH .
  class-methods SET_LOGIN
    importing
      !I_CPF type STRING optional
      !I_PERNR type STRING optional
      !I_SENHA type STRING
    exporting
      !E_TOKEN type STRING
      !E_NOME type STRING
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  class-methods GET_VERIFICA_TOKEN
    importing
      !I_CPF type STRING
      !I_TOKEN type STRING
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  class-methods SET_DESATIVA_ACESSO
    importing
      !I_PERNR type PERSNO
      !I_DS_MOTIVO type ZDE_MOTIVO_STATUS
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  class-methods GET_MD5_HASH
    importing
      !I_TEXTO type STRING
    returning
      value(R_HASHSTRING) type STRING
    raising
      ZCX_SOL_MOBILE_RH .
  methods SET_REGISTRO
    importing
      !I_ID_SOLICITACAO type ZDE_ID_SOLICITACAO
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods GET_REGISTRO
    exporting
      !E_ZHCMT_PA_0011 type ZHCMT_PA_0011
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods GET_LOGS_REGISTRO
    exporting
      !E_ZHCMT_PA_0012 type ZHCMT_PA_0012_T
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods SET_PASSWORD
    importing
      !I_SENHA type ZDE_SENHA_30
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods GET_PASSWORD
    exporting
      !E_DS_SENHA type ZDE_SENHA_HASH_MD5
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods SET_ID_SOLICITACAO
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods SET_PERNR
    importing
      !I_PERNR type PERSNO
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods SET_ST_SOLICITACAO
    importing
      !I_ST_SOLICITACAO type ZDE_STATUS_SOLICITACAO
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods SET_ADD_ERRO_LOGIN
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods SET_CLEAR_ERRO_LOGIN
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods SET_DS_MOTIVO
    importing
      !I_DS_MOTIVO type ZDE_MOTIVO_STATUS
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods SET_GRAVAR
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods SET_VALIDAR
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods SET_ADD_LOG_MOTIVO
    exporting
      !E_ZHCMT_PA_0012 type ZHCMT_PA_0012
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods SET_LIMPAR
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH
    raising
      ZCX_SOL_MOBILE_RH .
  methods SET_ID_DISPOSITIVO
    importing
      !I_ID_DISPOSITIVO type ZDE_ID_DISPOSITIVO
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH .
  methods GET_ID_DISPOSITIVO
    exporting
      !E_ID_DISPOSITIVO type ZDE_ID_DISPOSITIVO
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH .
  methods GET_CD_VALIDACAO
    exporting
      !E_CD_VALIDACAO type ZDE_CD_VALIDACAO
    returning
      value(R_SOL_MOBILE_RH) type ref to ZIF_SOL_MOBILE_RH .
endinterface.
