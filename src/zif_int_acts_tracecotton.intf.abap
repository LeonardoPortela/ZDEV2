interface ZIF_INT_ACTS_TRACECOTTON
  public .


  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INT_ACTS_TRACECOTTON type ref to ZIF_INT_ACTS_TRACECOTTON .
  constants AT_FC_END_POINT type STRING value '/api/clientes' ##NO_TEXT.
  class-data AT_TIPO type STRING .
  class-data AT_SERVICO type STRING .
  data AT_CD_SAI type STRING .
  data AT_LGORT type STRING .
  data AT_LOTE type STRING .
  data AT_FAZENDA type STRING .

  class-methods GET_INSTANCE
    importing
      !I_SERVICO type ZTIPOWEBSERV
    returning
      value(R_IF_INT_ACTS_TRACECOTTON) type ref to ZIF_INT_ACTS_TRACECOTTON
    raising
      ZCX_INTEGRACAO .
  methods SET_DADOS_ACTS
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_RETURN type ZDE_ACTS_TRACECOTTON_T
    returning
      value(R_IF_INT_ACTS_TRACECOTTON) type ref to ZIF_INT_ACTS_TRACECOTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INT_ACTS_TRACECOTTON) type ref to ZIF_INT_ACTS_TRACECOTTON
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INT_ACTS_TRACECOTTON) type ref to ZIF_INT_ACTS_TRACECOTTON
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INT_ACTS_TRACECOTTON) type ref to ZIF_INT_ACTS_TRACECOTTON
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INT_ACTS_TRACECOTTON) type ref to ZIF_INT_ACTS_TRACECOTTON
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INT_ACTS_TRACECOTTON) type ref to ZIF_INT_ACTS_TRACECOTTON
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INT_ACTS_TRACECOTTON) type ref to ZIF_INT_ACTS_TRACECOTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_TIPO
    importing
      value(I_TIPO) type ZTIPOWEBADM .
  methods SET_SERVICO
    importing
      !I_SERVICO type ZTIPOWEBSERV .
  methods SET_FAZENDA
    importing
      !I_FAZENDA type WERKS_D
    returning
      value(R_IF_INT_ACTS_TRACECOTTON) type ref to ZIF_INT_ACTS_TRACECOTTON .
  methods SET_LGORT
    importing
      value(I_LGORT) type LGORT_D optional
    returning
      value(R_IF_INT_ACTS_TRACECOTTON) type ref to ZIF_INT_ACTS_TRACECOTTON .
  methods SET_LOTE
    importing
      value(I_LOTE) type CHAR4 optional
    returning
      value(R_IF_INT_ACTS_TRACECOTTON) type ref to ZIF_INT_ACTS_TRACECOTTON .
  methods SET_CD_SAI
    importing
      value(I_CD_SAI) type CHAR20 optional
    returning
      value(R_IF_INT_ACTS_TRACECOTTON) type ref to ZIF_INT_ACTS_TRACECOTTON .
endinterface.
