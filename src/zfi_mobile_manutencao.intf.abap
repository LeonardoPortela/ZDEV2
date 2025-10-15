interface ZFI_MOBILE_MANUTENCAO
  public .


  class-data AT_IF_MOBILE_MANUTENCAO type ref to ZFI_MOBILE_MANUTENCAO .

  class-methods GET_INSTANCE
    returning
      value(R_IF_MOBILE_MANUTENCAO) type ref to ZIF_BOLETIM_PRODUCAO .
  class-methods ZFI_GET_NOTA_MAN .
  class-methods ZFI_GET_ORDEM_MAN .
  class-methods ZFI_GET_USER .
endinterface.
