interface ZIF_FATURA_FRETE_AQUAVIARIO
  public .


  class-data AT_FATURA_FRETE_AQUAVIARIO type ref to ZIF_FATURA_FRETE_AQUAVIARIO .

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCE) type ref to ZIF_FATURA_FRETE_AQUAVIARIO .
  methods GET_NEW_ID_FATURA_FRETE
    exporting
      !E_ID_FATURA_FRETE type ZDE_ID_FRETE_AQUA
    returning
      value(R_INSTANCE) type ref to ZIF_FATURA_FRETE_AQUAVIARIO
    raising
      ZCX_FATURA_FRETE_AQUAVIARIO .
  methods GET_NEW_ID_FATURA_FRETE_ITEM
    exporting
      !E_ID_FATURA_FRETE_ITEM type ZDE_ID_FRETE_AQUA_NF
    returning
      value(R_INSTANCE) type ref to ZIF_FATURA_FRETE_AQUAVIARIO
    raising
      ZCX_FATURA_FRETE_AQUAVIARIO .
endinterface.
