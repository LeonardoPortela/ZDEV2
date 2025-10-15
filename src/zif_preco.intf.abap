interface ZIF_PRECO
  public .


  class-methods GET_PRECO_PAUTA
    importing
      !I_REGIO type J_1BTXSHPF
      !I_MATNR type MATNR
      !I_INCO1 type INCO1
      !I_LAST type CHAR01 optional
    exporting
      !E_KONP type KONP
    returning
      value(R_KBETR) type KBETR_KOND
    raising
      ZCX_PRECO .
  class-methods GET_PRECO_ESTOQUE
    importing
      !I_MATNR type MATNR
      !I_BWKEY type BWKEY
      !I_VPRSV type VPRSV
      !I_WAERS type WAERS
      !I_LAST type CHAR01 optional
    returning
      value(R_PRECO) type CK_STPRS_1
    raising
      ZCX_PRECO .
endinterface.
