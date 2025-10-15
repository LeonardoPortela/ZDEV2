interface ZIF_REMESSA_ARMAZENAGEM
  public .


  class-methods GERAR_REMESSA_COM_PESAGEM_OPUS
    importing
      !I_CH_REF_ROMANEIO type ZCH_REF
      !I_LIFNR_SP type LIFNR
    returning
      value(R_DELIVERY) type BAPISHPDELIVNUMB-DELIV_NUMB .
  class-methods ESTORNAR_SAIDA_ESTOQUE
    importing
      !I_CH_REFERENCIA type ZCH_REF
    returning
      value(R_ESTORNADO) type CHAR01 .
  class-methods GERAR_SAIDA_ESTOQUE
    importing
      !I_CH_REFERENCIA type ZCH_REF
    exporting
      !E_MJAHR type MJAHR
      !E_MBLNR type MBLNR
    returning
      value(R_GEROU) type CHAR01 .
endinterface.
