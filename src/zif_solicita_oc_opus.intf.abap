interface ZIF_SOLICITA_OC_OPUS
  public .


  class-data AT_IF_SOLICITA_OC_OPUS type ref to ZIF_SOLICITA_OC_OPUS .
  data AT_VIAGEM type ZLEST0185 .
  data AT_ORDEM_CARREGAMENTO type ZSDT0001OD .
  constants AT_FUNCAO_SOLICITA_OC_OPUS type ZDE_DS_FUNCAO_PROCESSA value '/carguero/ordemcarregamento' ##NO_TEXT.
  data AT_OC_SAP_TO_PUS_OV type ZDE_JSON_OC_SAP_TO_PUS_OV .

  class-methods GET_INSTANCE
    returning
      value(R_IF_SOLICITA_OC_OPUS) type ref to ZIF_SOLICITA_OC_OPUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SOLICITA_ORDEM_CARREGA
    importing
      !I_ZLEST0185 type ZLEST0185
      !I_ZDE_JSON_OC_SAP_TO_PUS_OV type ZDE_JSON_OC_SAP_TO_PUS_OV
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_SOLICITA_OC_OPUS) type ref to ZIF_SOLICITA_OC_OPUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_SOLICITA_OC_OPUS) type ref to ZIF_SOLICITA_OC_OPUS
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_SOLICITA_OC_OPUS) type ref to ZIF_SOLICITA_OC_OPUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_SOLICITA_OC_OPUS) type ref to ZIF_SOLICITA_OC_OPUS
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_SOLICITA_OC_OPUS) type ref to ZIF_SOLICITA_OC_OPUS .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_SOLICITA_OC_OPUS) type ref to ZIF_SOLICITA_OC_OPUS .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_SOLICITA_OC_OPUS) type ref to ZIF_SOLICITA_OC_OPUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
