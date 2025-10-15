interface ZIF_INT_ORDEM_CARREGA_OPUS
  public .


  class-data AT_INT_ORDEM_CARREGA_OPUS type ref to ZIF_INT_ORDEM_CARREGA_OPUS .
  class-data AT_SERVICO type STRING .
  data AT_JSON type STRING .
  data AT_DATA type ZLESE0159 .
  class-data AT_RETURN type BAPIRET2 .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INT_ORDEM_CARREGA_OPUS) type ref to ZIF_INT_ORDEM_CARREGA_OPUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INT_ORDEM_CARREGA_OPUS) type ref to ZIF_INT_ORDEM_CARREGA_OPUS
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INT_ORDEM_CARREGA_OPUS) type ref to ZIF_INT_ORDEM_CARREGA_OPUS
    raising
      ZCX_INTEGRACAO .
  methods SET_INT_COMB
    returning
      value(R_IF_INTEGRACAO_COMB) type ref to ZIF_INTEGRACAO_COMB
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INT_ORDEM_CARREGA_OPUS) type ref to ZIF_INT_ORDEM_CARREGA_OPUS
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INT_ORDEM_CARREGA_OPUS) type ref to ZIF_INT_ORDEM_CARREGA_OPUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INT_ORDEM_CARREGA_OPUS) type ref to ZIF_INT_ORDEM_CARREGA_OPUS .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INT_ORDEM_CARREGA_OPUS) type ref to ZIF_INT_ORDEM_CARREGA_OPUS .
  methods SET_STATUS_ORDEM_CAR_OPUS
    returning
      value(R_IF_INT_ORDEM_CARREGA_OPUS) type ref to ZIF_INT_ORDEM_CARREGA_OPUS .
  methods POST_ORDEM_CAR_OPUS
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    changing
      value(E_DATA) type STRING optional
    returning
      value(R_IF_INT_ORDEM_CARREGA_OPUS) type ref to ZIF_INT_ORDEM_CARREGA_OPUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods MONTA_JSON
    importing
      !I_DATA type ZLESE0159
    returning
      value(E_JSON) type STRING .
  methods SET_DADOS_ORDEM_CARREGAMENTO
    importing
      !I_DATA type ZLESE0159
    returning
      value(R_IF_INT_ORDEM_CARREGA_OPUS) type ref to ZIF_INT_ORDEM_CARREGA_OPUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
