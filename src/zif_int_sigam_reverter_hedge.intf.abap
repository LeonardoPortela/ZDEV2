interface ZIF_INT_SIGAM_REVERTER_HEDGE
  public .


  data AT_SERVICO type /UI2/SERVICE_NAME .
  class-data AT_INSTANCE type ref to ZIF_INT_SIGAM_REVERTER_HEDGE .
  data AT_XDADOS type XSTRING .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_BODY type STRING .

  class-methods GET_INSTANCE
    returning
      value(R_OBJECT) type ref to ZIF_INT_SIGAM_REVERTER_HEDGE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  class-methods ENVIAR_SIGAM
    importing
      !I_REQUEST type ZSDE0024
    returning
      value(R_RESPO_TAB) type ZSDC0028
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_OBJECT) type ref to ZIF_INT_SIGAM_REVERTER_HEDGE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_DATA
    importing
      !I_REQUEST type STRING
    returning
      value(R_OBJECT) type ref to ZIF_INT_SIGAM_REVERTER_HEDGE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_OBJECT) type ref to ZIF_INT_SIGAM_REVERTER_HEDGE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
