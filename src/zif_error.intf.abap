interface ZIF_ERROR
  public .


  data MSGTY type SYST_MSGTY .
  data MSGNO type SYST_MSGNO .
  data MSGV1 type SYST_MSGV .
  data MSGV2 type SYST_MSGV .
  data MSGV3 type SYST_MSGV .
  data MSGV4 type SYST_MSGV .
  data MSGID type SYST_MSGID .
  data TRANSACAO type TCODE .

  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_ERROR .
  methods PUBLISHED_ERRO
    importing
      !I_MSGTY type SYST_MSGTY optional
      !I_MSGTY_DISPLAY type SYST_MSGTY optional .
  methods GET_MSG_ERRO
    returning
      value(R_MSG_TEXTO) type STRING .
endinterface.
