interface ZIF_DEPOSITO
  public .


  class-data AT_DEPOSITO type ref to ZIF_DEPOSITO .

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCE) type ref to ZIF_DEPOSITO .
  methods GET_DEPOSITO_MATERIAL_FILIAL
    importing
      !I_MATNR type MATNR
      !I_TP_PRODUTO type ZDE_TP_PRODUTO
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_ optional
      !I_EUDR type ZEUDR optional
      !I_CENTRO_A_FIXAR type WERKS_D optional
    exporting
      !E_LGORT type LGORT_D
      !E_BRANCH type J_1BBRANC_
      !E_CENTRO_A_FIXAR type WERKS_D
    returning
      value(R_INSTANCE) type ref to ZIF_DEPOSITO
    raising
      ZCX_DEPOSITO .
endinterface.
