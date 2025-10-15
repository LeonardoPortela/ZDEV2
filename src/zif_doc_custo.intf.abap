interface ZIF_DOC_CUSTO
  public .


  class-data AT_IF_DOC_CUSTO type ref to ZIF_DOC_CUSTO .

  class-methods GET_INSTANCE
    returning
      value(R_IF_DOC_CUSTO) type ref to ZIF_DOC_CUSTO
    raising
      ZCX_DOC_CUSTO .
  methods GET_CK_VALIDA_VI_VT
    importing
      !I_VTTKF type VTTKF optional
      !I_TKNUM type TKNUM optional
    returning
      value(R_IF_DOC_CUSTO) type ref to ZIF_DOC_CUSTO
    raising
      ZCX_DOC_CUSTO .
endinterface.
