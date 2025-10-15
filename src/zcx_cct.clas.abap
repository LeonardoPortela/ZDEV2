class ZCX_CCT definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_ERRO_GERAL,
      msgid type symsgid value 'ZUSER',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_ERRO_GERAL .
  constants:
    begin of ZCX_AUTENTICACAO_NOT_FOUND,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '018',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_AUTENTICACAO_NOT_FOUND .
  constants:
    begin of ZCX_PAR_AUTENTICACAO_NOT_FOUND,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '039',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PAR_AUTENTICACAO_NOT_FOUND .
  constants:
    begin of ZCX_WEBSERVICE_NOT_FOUND,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '125',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_WEBSERVICE_NOT_FOUND .
  constants:
    begin of ZCX_FALHA_COMUNICACAO_WS,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '024',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_FALHA_COMUNICACAO_WS .
  constants:
    begin of ZCX_STATUS_COMUNICACAO_WS,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '025',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_STATUS_COMUNICACAO_WS .
  constants:
    begin of ZCX_FALHA_PROCESSAMENTO_WS,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '026',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_FALHA_PROCESSAMENTO_WS .
  constants:
    begin of ZCX_TIMEOUT_WS,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '027',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TIMEOUT_WS .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_400,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '126',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_400 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_401,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '127',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_401 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_403,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '128',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_403 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_404,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '129',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_404 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_422,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '130',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_422 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_500,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '131',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_500 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_503,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '132',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_503 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_OTHERS,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '133',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_OTHERS .
  constants:
    begin of ZCX_RETORNO_SISCOMEX,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '134',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_RETORNO_SISCOMEX .
  constants:
    begin of ZCX_ERRO_DESCONHECIDO_SISCOMEX,
      msgid type symsgid value 'ZCCT',
      msgno type symsgno value '135',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_DESCONHECIDO_SISCOMEX .
  data MSGTY type SYST_MSGTY .
  data MSGNO type SYST_MSGNO .
  data MSGV1 type SYST_MSGV .
  data MSGV2 type SYST_MSGV .
  data MSGV3 type SYST_MSGV .
  data MSGV4 type SYST_MSGV .
  data MSGID type SYST_MSGID .
  data TRANSACAO type TCODE .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGTY type SYST_MSGTY optional
      !MSGNO type SYST_MSGNO optional
      !MSGV1 type SYST_MSGV optional
      !MSGV2 type SYST_MSGV optional
      !MSGV3 type SYST_MSGV optional
      !MSGV4 type SYST_MSGV optional
      !MSGID type SYST_MSGID optional
      !TRANSACAO type TCODE optional .
  methods PUBLISHED_ERRO
    importing
      !I_MSGTY type SYST_MSGTY optional
      !I_MSGTY_DISPLAY type SYST_MSGTY optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CCT IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGTY = MSGTY .
me->MSGNO = MSGNO .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
me->MSGID = MSGID .
me->TRANSACAO = TRANSACAO .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD PUBLISHED_ERRO.

    DATA: P_MSGTY	        TYPE SYST_MSGTY,
          P_MSGTY_DISPLAY	TYPE SYST_MSGTY.


    IF I_MSGTY IS NOT INITIAL.
      P_MSGTY = I_MSGTY.
    ELSE.
      P_MSGTY = ME->MSGTY.
    ENDIF.

    IF I_MSGTY_DISPLAY IS NOT INITIAL.
      P_MSGTY_DISPLAY = I_MSGTY_DISPLAY.
    ELSE.
      P_MSGTY_DISPLAY = ME->MSGTY.
    ENDIF.

    IF ME->TRANSACAO IS NOT INITIAL.
      IF ME->MSGV1 IS INITIAL.
        ME->MSGV1 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV2 IS INITIAL.
        ME->MSGV2 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV3 IS INITIAL.
        ME->MSGV3 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV4 IS INITIAL.
        ME->MSGV4 = ME->TRANSACAO.
      ENDIF.
    ENDIF.

    MESSAGE ID ME->MSGID TYPE P_MSGTY NUMBER ME->MSGNO WITH ME->MSGV1 ME->MSGV2 ME->MSGV3 ME->MSGV4 DISPLAY LIKE P_MSGTY_DISPLAY.

  ENDMETHOD.
ENDCLASS.
