interface ZIF_PESQUISA
  public .


  methods PESQUISAR
    importing
      !I_FILTROS type ANY
    exporting
      !E_REGISTROS type ANY
      !E_REGISTROS_S type ANY
    returning
      value(E_PESQUISOU) type CHAR01
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
endinterface.
