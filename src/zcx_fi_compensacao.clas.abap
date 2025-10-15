class ZCX_FI_COMPENSACAO definition
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
    begin of ZCX_OBG_INF_BUKRS,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '013',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_BUKRS .
  constants:
    begin of ZCX_OBG_INF_BUDAT,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '014',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_BUDAT .
  constants:
    begin of ZCX_OBG_INF_BLDAT,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '015',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_BLDAT .
  constants:
    begin of ZCX_OBG_INF_WAERS,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '016',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_WAERS .
  constants:
    begin of ZCX_OBG_INF_KURSF,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '017',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_KURSF .
  constants:
    begin of ZCX_OBG_INF_BLART,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '018',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_BLART .
  constants:
    begin of ZCX_OBG_INF_MONAT,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '019',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_MONAT .
  constants:
    begin of ZCX_ERROR_INI_COMP,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '020',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERROR_INI_COMP .
  constants:
    begin of ZCX_DOCUMENT_NOT_FOUND,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '023',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_DOCUMENT_NOT_FOUND .
  constants:
    begin of ZCX_DOCUMENT_NOT_OPEN,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '024',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_DOCUMENT_NOT_OPEN .
  constants:
    begin of ZCX_SLD_RESIDUAL_INVALID,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '025',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_SLD_RESIDUAL_INVALID .
  constants:
    begin of ZCX_TX_GER_RESID_NOT_FOUND,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '026',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_TX_GER_RESID_NOT_FOUND .
  constants:
    begin of ZCX_ERRO_BSCHL,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '027',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_BSCHL .
  constants:
    begin of ZCX_LCTO_VALOR_ZERADO,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '028',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_LCTO_VALOR_ZERADO .
  constants:
    begin of ZCX_LCTO_SEM_CHV_LCTO,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '029',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_LCTO_SEM_CHV_LCTO .
  constants:
    begin of ZCX_LCTO_SEM_TIPO_CONTA,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '030',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_LCTO_SEM_TIPO_CONTA .
  constants:
    begin of ZCX_CENARIO_NAO_PREVISTO,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '031',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_CENARIO_NAO_PREVISTO .
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



CLASS ZCX_FI_COMPENSACAO IMPLEMENTATION.


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
