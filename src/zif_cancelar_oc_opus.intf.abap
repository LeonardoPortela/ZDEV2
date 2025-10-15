interface ZIF_CANCELAR_OC_OPUS
  public .


  class-data AT_IF_CANCELAR_OC_OPUS type ref to ZIF_CANCELAR_OC_OPUS .
  data AT_VIAGEM type ZLEST0185 .
  data AT_ORDEM_CARREGAMENTO type ZSDT0001OD .
  constants AT_FUNCAO_CANCELAR_OC_OPUS type ZDE_DS_FUNCAO_PROCESSA value '/ordemcarregamento/cancelar' ##NO_TEXT.
  constants AT_SUCESS_CANCELAMENTO type CHAR10 value 'true' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_IF_CANCELAR_OC_OPUS) type ref to ZIF_CANCELAR_OC_OPUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_CANCELAR_ORDEM_CARREGA
    importing
      !I_ZLEST0185 type ZLEST0185 optional
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_CANCELADA type CHAR01
    returning
      value(R_IF_CANCELAR_OC_OPUS) type ref to ZIF_CANCELAR_OC_OPUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ORDEM_CARREGAMENTO
    importing
      !I_ID_ORDEM type ZDE_ID_ORDEM
    returning
      value(R_IF_CANCELAR_OC_OPUS) type ref to ZIF_CANCELAR_OC_OPUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_CANCELAR_OC_OPUS) type ref to ZIF_CANCELAR_OC_OPUS
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_CANCELAR_OC_OPUS) type ref to ZIF_CANCELAR_OC_OPUS .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_CANCELAR_OC_OPUS) type ref to ZIF_CANCELAR_OC_OPUS .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_CANCELAR_OC_OPUS) type ref to ZIF_CANCELAR_OC_OPUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
