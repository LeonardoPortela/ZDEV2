interface ZIF_INT_CLIENTE_TOKEN
  public .


  class-data AT_SERVICO type STRING .
  class-data AT_JSON type STRING .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INT_CLIENTE_TOKEN type ref to ZIF_INT_CLIENTE_TOKEN .
  constants AT_FC_END_POINT type STRING value '/api/clientes' ##NO_TEXT.
  class-data AT_TIPO type STRING .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INT_CLIENTE_TOKEN) type ref to ZIF_INT_CLIENTE_TOKEN
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_URL
    returning
      value(R_IF_INT_CLIENTE_TOKEN) type ref to ZIF_INT_CLIENTE_TOKEN
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ACCESS_TOKEN type STRING
      !E_TOKEN_TYPE type STRING
      value(E_EXPIRES_IN) type STRING
    returning
      value(R_IF_INT_CLIENTE_TOKEN) type ref to ZIF_INT_CLIENTE_TOKEN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_TOKEN
    returning
      value(R_IF_INT_CLIENTE_TOKEN) type ref to ZIF_INT_CLIENTE_TOKEN .
endinterface.
