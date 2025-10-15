interface ZIF_INT_ORG_DESENV
  public .


  types:
    BEGIN OF ty_saida,
      code        TYPE char10,
      name        TYPE char80,
      description TYPE char50,
      address     TYPE char50,
      document    TYPE char30,
      parentcode  TYPE char10,
    END OF ty_saida .
  types:
    tyt_saida TYPE STANDARD TABLE OF ty_saida .

  constants GC_SERVICE type /UI2/SERVICE_NAME value 'DESENV_INT_ENVIA_ORG' ##NO_TEXT.
  constants GC_TOKEN type /UI2/SERVICE_NAME value 'DESENV_INT_TOKEN' ##NO_TEXT.
  class-data AT_SERVICO type /UI2/SERVICE_NAME .
  data AT_STRUCT type TYT_SAIDA .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_TOKEN_WS type ZAUTH_WEBSERVICE .
  data AT_BODY type STRING .

  methods GET_INSTANCE .
  methods SET_DS_URL
    returning
      value(R_IF_INT_ORG_DESENV) type ref to ZIF_INT_ORG_DESENV .
  methods SET_XML .
  methods SET_DS_DATA
    returning
      value(R_IF_INT_ORG_DESENV) type ref to ZIF_INT_ORG_DESENV .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INT_ORG_DESENV) type ref to ZIF_INT_ORG_DESENV .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INT_ORG_DESENV) type ref to ZIF_INT_ORG_DESENV .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INT_ORG_DESENV) type ref to ZIF_INT_ORG_DESENV .
  methods SET_SERVICO
    importing
      !I_SERVICO type /UI2/SERVICE_NAME .
  methods ENVIAR_DESENVOLVE
    importing
      !IT_SAIDA type TYT_SAIDA
    exporting
      !EV_RETURN_CODE type STRING
      !EV_RETURN_MSG type STRING
    returning
      value(R_RETURN_VALUE) type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
