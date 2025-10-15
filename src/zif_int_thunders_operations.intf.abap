interface ZIF_INT_THUNDERS_OPERATIONS
  public .


  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INT_THUNDERS_OPERATIONS type ref to ZIF_INT_THUNDERS_OPERATIONS .
  constants AT_FC_END_POINT type STRING value '/api/clientes' ##NO_TEXT.
  class-data AT_TIPO type STRING .
  class-data AT_SERVICO type STRING .
  class-data AT_JSON type STRING .
  class-data AT_QUERY type ZDE_HEADER_FIELD_T .
  class-data AT_SERVICE type /UI2/SERVICE_NAME .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INT_THUNDERS_OPERATIONS) type ref to ZIF_INT_THUNDERS_OPERATIONS
    raising
      ZCX_INTEGRACAO .
  methods GET_DADOS_THUNDERS_OPERATIONS
    exporting
      !I_THUNDERS type ZFIE0004_T
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_JSON type STRING
      !I_THUNDERS_OPERATIONS type ZFIE0003_T
    returning
      value(R_IF_INT_THUNDERS_OPERATIONS) type ref to ZIF_INT_THUNDERS_OPERATIONS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INT_THUNDERS_OPERATIONS) type ref to ZIF_INT_THUNDERS_OPERATIONS
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INT_THUNDERS_OPERATIONS) type ref to ZIF_INT_THUNDERS_OPERATIONS
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INT_THUNDERS_OPERATIONS) type ref to ZIF_INT_THUNDERS_OPERATIONS
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INT_THUNDERS_OPERATIONS) type ref to ZIF_INT_THUNDERS_OPERATIONS
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INT_THUNDERS_OPERATIONS) type ref to ZIF_INT_THUNDERS_OPERATIONS
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INT_THUNDERS_OPERATIONS) type ref to ZIF_INT_THUNDERS_OPERATIONS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_TIPO
    importing
      value(I_TIPO) type ZTIPOWEBADM .
  methods SET_SERVICO
    importing
      !I_SERVICO type ZTIPOWEBSERV .
  methods SET_JSON
    importing
      !E_JSON type STRING
    returning
      value(R_IF_INT_THUNDERS_OPERATIONS) type ref to ZIF_INT_THUNDERS_OPERATIONS .
  methods SET_QUERY
    importing
      !I_PARAMETRO type ZDE_HEADER_FIELD_T
    returning
      value(R_IF_INT_THUNDERS_OPERATIONS) type ref to ZIF_INT_THUNDERS_OPERATIONS .
  methods SET_SERVICE
    importing
      !I_SERVICE type /UI2/SERVICE_NAME
    returning
      value(R_IF_INT_THUNDERS_OPERATIONS) type ref to ZIF_INT_THUNDERS_OPERATIONS .
endinterface.
