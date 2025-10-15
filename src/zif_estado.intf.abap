interface ZIF_ESTADO
  public .


  class-data AT_ESTADO type ref to ZIF_ESTADO .

  class-methods GET_INSTANCE
    returning
      value(R_IF_ESTADO) type ref to ZIF_ESTADO .
  methods GET_ID_BACEN
    importing
      !I_UF type STRING
    exporting
      !E_ID_BACEN type STRING
    returning
      value(R_IF_ESTADO) type ref to ZIF_ESTADO .
  methods GET_SIGLA_ESTADO
    importing
      !I_ID_BACEN type STRING
    exporting
      !E_UF type STRING
    returning
      value(R_IF_ESTADO) type ref to ZIF_ESTADO .
endinterface.
