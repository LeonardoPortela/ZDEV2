interface ZIF_CARGA_AJUSTES
  public .


  methods SET_AJUSTAR_FISCAL
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA_AJUSTES
    raising
      ZCX_CARGA_AJUSTES .
  methods SET_AJUSTAR_ESTOQUE
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA_AJUSTES
    raising
      ZCX_CARGA_AJUSTES .
  methods SET_AJUSTAR_ROMANEIO_ENTRADA
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA_AJUSTES
    raising
      ZCX_CARGA_AJUSTES .
  methods SET_AJUSTAR_CUSTO_FRETE
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA_AJUSTES
    raising
      ZCX_CARGA_AJUSTES .
endinterface.
