interface ZIF_INT_TOKEN_DESENV
  public .


  types:
    BEGIN OF ty_token,
      login      TYPE char20,
      password   TYPE string,
      tenantcode TYPE char10,
      clientid   TYPE char10,
    END OF ty_token .
  types:
    tyt_token TYPE STANDARD TABLE OF ty_token .

  class-data AT_IF_INTEGRACAO_TOKEN type ref to ZIF_INT_TOKEN_DESENV .
  constants GC_TOKEN type /UI2/SERVICE_NAME value 'DESENV_INT_TOKEN' ##NO_TEXT.
  class-data AT_SERVICO type /UI2/SERVICE_NAME .
  data AT_STRUCT type TY_TOKEN .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_TOKEN_WS type ZAUTH_WEBSERVICE .
  data AT_BODY type STRING .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INT_TOKEN_DESENV .
  methods GET_TOKEN
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INT_TOKEN_DESENV
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INT_TOKEN_DESENV
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SEND_MSG
    exporting
      !E_ACCESS_TOKEN type STRING
      !E_TOKEN_TYPE type STRING
      !E_EXPIRES_IN type STRING
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INT_TOKEN_DESENV
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_DATA
    returning
      value(R_IF_INT_TOKEN_DESENV) type ref to ZIF_INT_TOKEN_DESENV .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INT_TOKEN_DESENV) type ref to ZIF_INT_TOKEN_DESENV .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INT_TOKEN_DESENV) type ref to ZIF_INT_TOKEN_DESENV .
  methods SET_SERVICO
    importing
      !I_SERVICO type /UI2/SERVICE_NAME .
  methods ENVIAR_DESENVOLVE
    importing
      !IT_SAIDA type TYT_TOKEN
    exporting
      !EV_RETURN_CODE type STRING
      !EV_RETURN_MSG type STRING
    returning
      value(R_RETURN_VALUE) type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
