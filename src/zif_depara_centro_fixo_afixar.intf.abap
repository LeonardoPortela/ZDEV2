interface ZIF_DEPARA_CENTRO_FIXO_AFIXAR
  public .


  class-data AT_CENTRO type ref to ZIF_DEPARA_CENTRO_FIXO_AFIXAR .

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCE) type ref to ZIF_DEPARA_CENTRO_FIXO_AFIXAR .
  class-methods GET_DADOS_DEPARA
    importing
      !I_MATERIAL type MATNR optional
      !IT_MATNR type ZMMTT_MATNR optional
      !I_CENTRO_AFIXAR type WERKS_D optional
      !IT_CENTRO_AFIXAR type ZMMTT_WERKS optional
      !I_CENTRO_FIXO type WERKS_D optional
      !IT_CENTRO_FIXO type ZMMTT_WERKS optional
      !I_DEPOSITO type LGORT_D optional
      !IT_DEPOSITO type ZMMTT_LGORT optional
      !I_TIPO_PRODUTO type ZDE_TP_PRODUTO optional
      !I_EUDR type ZEUDR optional
    exporting
      !E_TABLE_DEPARA type ZMMT0017_TP
      !E_SINGLE_DEPARA type ZZMMT0017 .
endinterface.
