interface ZIF_SHDB
  public .


  class-data INSTANCE type ref to ZIF_SHDB .
  data AT_SHDB type TAB_BDCDATA read-only .
  data AT_MSG type TAB_BDCMSGCOLL read-only .
  data AT_TRANSACTION type TCODE read-only .
  data AT_MODE type CHAR01 read-only .
  constants ST_TIPO_MODE_COM_TELA type CHAR01 value 'A' ##NO_TEXT.
  constants ST_TIPO_MODE_ERRO_TELA type CHAR01 value 'E' ##NO_TEXT.
  constants ST_TIPO_MODE_SEM_TELA_SEM_DEBU type CHAR01 value 'N' ##NO_TEXT.
  constants ST_TIPO_MODE_SEM_TELA_COM_DEBU type CHAR01 value 'P' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCIA) type ref to ZIF_SHDB
    raising
      ZCX_SHDB .
  methods GET_ERRO_GERAL
    raising
      ZCX_SHDB .
  methods GET_ERRO_GERAL_STRING
    importing
      !I_TEXTO type STRING
    raising
      ZCX_SHDB .
  methods SET_CLEAR
    returning
      value(R_INSTANCIA) type ref to ZIF_SHDB
    raising
      ZCX_SHDB .
  methods SET_TRANSACTION
    importing
      !I_TCODE type TCODE
    returning
      value(R_INSTANCIA) type ref to ZIF_SHDB
    raising
      ZCX_SHDB .
  methods SET_MODE
    importing
      !I_MODE type CHAR01 default ZIF_SHDB=>ST_TIPO_MODE_SEM_TELA_SEM_DEBU
    returning
      value(R_INSTANCIA) type ref to ZIF_SHDB
    raising
      ZCX_SHDB .
  methods SET_ADD_BDCDATA
    importing
      !I_BDCDATA type BDCDATA
    returning
      value(R_INSTANCIA) type ref to ZIF_SHDB
    raising
      ZCX_SHDB .
  methods SET_EXECUTAR
    returning
      value(R_INSTANCIA) type ref to ZIF_SHDB
    raising
      ZCX_SHDB .
  methods GET_CK_EXISTE_MSG_ERRO
    exporting
      !E_MSG type BDCMSGCOLL
      !E_MSG_TAB type TAB_BDCMSGCOLL
    returning
      value(R_INSTANCIA) type ref to ZIF_SHDB
    raising
      ZCX_SHDB .
endinterface.
