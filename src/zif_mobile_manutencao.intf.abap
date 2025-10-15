interface ZIF_MOBILE_MANUTENCAO
  public .


  class-data AT_IF_MOBILE_MANUTENCAO type ref to ZIF_MOBILE_MANUTENCAO .

  class-methods GET_INSTANCE
    returning
      value(R_IF_MOBILE_MANUTENCAO) type ref to ZIF_MOBILE_MANUTENCAO .
  methods ZFI_SET_NOTA_MAN
    returning
      value(R_RETURN) type STRING .
endinterface.
