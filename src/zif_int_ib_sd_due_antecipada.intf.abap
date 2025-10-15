interface ZIF_INT_IB_SD_DUE_ANTECIPADA
  public .


  class-data AT_IF_INT_IB_DUE_ANTEC type ref to ZIF_INT_IB_SD_DUE_ANTECIPADA .
  constants AT_FC_END_POINT type STRING value '' ##NO_TEXT.
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '111' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_IF_INT_IB_SD_DUE_ANTECIPADA) type ref to ZIF_INT_IB_SD_DUE_ANTECIPADA .
  methods VALIDAR_DADOS_INBOUND
    importing
      !I_DATA_INBOUND type STRING
    returning
      value(R_MSG_ERRO) type STRING .
endinterface.
