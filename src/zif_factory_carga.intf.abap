interface ZIF_FACTORY_CARGA
  public .


  class-data AT_FACTORY_CARGA type ref to ZIF_FACTORY_CARGA .
  data IA_NAME_CLASS type STRING .
  constants AT_RECEBE_GRANEL type STRING value 'ZCL_CARGA_RECEBIMENTO_GRANEL' ##NO_TEXT.
  constants AT_RECEBE_ALGODAO type STRING value 'ZCL_CARGA_RECEBIMENTO_ALGODAO' ##NO_TEXT.
  constants AT_SAIDA_OPUS_VIEW type STRING value 'ZCL_CARGA_SAIDA_OPUS' ##NO_TEXT.
  constants AT_RECEBE_GRANEL_SAIDA type STRING value 'ZCL_CARGA_SAIDA' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_FACTORY_CARGA) type ref to ZIF_FACTORY_CARGA .
  methods SET_FACTORY_OBJETO
    importing
      !I_TP_CARGA type ZDE_TP_CARGA
      !I_TP_PRODUTO type ZDE_TP_PRODUTO_CARGA
    returning
      value(R_FACTORY_CARGA) type ref to ZIF_FACTORY_CARGA .
  methods SET_FACTORY_OBJETO_ID
    importing
      !I_ID_CARGA type ZDE_ID_CARGA
    returning
      value(R_FACTORY_CARGA) type ref to ZIF_FACTORY_CARGA .
  methods GET_FACTORY_OBJETO
    returning
      value(R_CARGA) type ref to ZIF_CARGA .
  methods GET_FACTORY_OBJETO_NAME
    exporting
      value(E_NAME) type STRING
    returning
      value(R_FACTORY_CARGA) type ref to ZIF_FACTORY_CARGA .
endinterface.
