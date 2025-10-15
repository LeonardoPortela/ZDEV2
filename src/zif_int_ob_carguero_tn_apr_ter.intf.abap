interface ZIF_INT_OB_CARGUERO_TN_APR_TER
  public .


  class-data AT_IF_INTEGRACAO_CARREGAR type ref to ZIF_INT_OB_CARGUERO_TN_APR_TER .
  data AT_VIAGEM type ZLEST0185 .
  constants AT_FC_PROCESSAR_VIAGEM_CARREGA type ZDE_DS_FUNCAO_PROCESSA value '/viagens/id/carregamento' ##NO_TEXT.
  data AT_NFE_TERCEIRO type ZMME0252 .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INT_OB_CARGUERO_TN_APR_TER
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  class-methods VALIDA_CHAVE_NFE
    importing
      !I_CH_REFERENCIA type ZCH_REF
    returning
      value(E_ERRO) type CHAR1 .
  class-methods GET_URL_DOCUMENTO
    importing
      !I_VIAGEM_ID type ZDE_VIAGEM_ID optional
    returning
      value(R_URL) type STRING
    raising
      ZCX_INTEGRACAO .
  methods VALIDA_ENVIO_APROVACAO
    importing
      !I_VIAGEM_ID type ZDE_VIAGEM_ID
    exporting
      value(E_ID_INTEGRACAO) type ZDE_ID_INTEGRACAO
      value(E_STATUS) type ZDE_STATUS_CARGUERO_VIAGEM
      value(E_ERRO) type CHAR1
      value(E_MSG_ERRO) type STRING
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INT_OB_CARGUERO_TN_APR_TER
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods VALIDA_REMOCAO_DOCS_CARGUERO
    importing
      !I_CH_REFERENCIA type ZCH_REF
    exporting
      !E_ERRO type CHAR01
      !E_MSG_ERRO type STRING
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INT_OB_CARGUERO_TN_APR_TER
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods APROVAR_VIAGEM
    importing
      !I_VIAGEM_ID type ZDE_VIAGEM_ID optional
      !I_DT_CARREGAMENTO type ZDE_DT_CARREGAMENTO optional
      !I_PESO_TARA type ZDE_PESO_BRUTO optional
      !I_PESO_LIQUIDO type ZDE_PESO_BRUTO optional
      !I_PESO_BRUTO type ZDE_PESO_BRUTO optional
      !I_NFE_TERCEIRO type ZMME0252
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INT_OB_CARGUERO_TN_APR_TER
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INT_OB_CARGUERO_TN_APR_TER
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INT_OB_CARGUERO_TN_APR_TER
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INT_OB_CARGUERO_TN_APR_TER
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INT_OB_CARGUERO_TN_APR_TER .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INT_OB_CARGUERO_TN_APR_TER .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INT_OB_CARGUERO_TN_APR_TER
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ID_VIAGEM
    importing
      !I_TKNUM type VTTK-TKNUM
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INT_OB_CARGUERO_TN_APR_TER .
endinterface.
