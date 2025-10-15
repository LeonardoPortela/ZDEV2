interface ZIF_CENTRO
  public .


  class-data AT_IF_CENTRO type ref to ZIF_CENTRO .

  class-methods GET_INSTANCE
    returning
      value(R_ZIF_CENTRO) type ref to ZIF_CENTRO .
  methods GET_CENTRO_AFIXAR
    importing
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
    exporting
      !R_WERKS_AFIXAR type WERKS_D
    returning
      value(R_ZIF_CENTRO) type ref to ZIF_CENTRO
    raising
      ZCX_CARGA .
endinterface.
