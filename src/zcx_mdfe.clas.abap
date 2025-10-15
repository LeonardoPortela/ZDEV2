class ZCX_MDFE definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

*"* public components of class ZCX_MDFE
*"* do not include other source files here!!!
public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_MODELO_NOT_PREV,
      msgid type symsgid value 'ZEXC_MSG_MDFE',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_MODELO_NOT_PREV .
  constants:
    begin of ZEXC_MSG_MDFE,
      msgid type symsgid value 'ZEXC_MSG_MDFE',
      msgno type symsgno value '000',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZEXC_MSG_MDFE .
  constants:
    begin of ZCX_NF_MAT_NOT_FOUND,
      msgid type symsgid value 'ZEXC_MSG_MDFE',
      msgno type symsgno value '002',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NF_MAT_NOT_FOUND .
  constants:
    begin of ZCX_GRUPO_MERC_NOT_PARA,
      msgid type symsgid value 'ZEXC_MSG_MDFE',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_GRUPO_MERC_NOT_PARA .
  constants:
    begin of ZCX_GRUPO_MERC_NOT_INFO,
      msgid type symsgid value 'ZEXC_MSG_MDFE',
      msgno type symsgno value '004',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_GRUPO_MERC_NOT_INFO .
  constants:
    begin of ZCX_LOCAL_CARREGA,
      msgid type symsgid value 'ZEXC_MSG_MDFE',
      msgno type symsgno value '005',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_LOCAL_CARREGA .
  constants:
    begin of ZCX_LOCAL_DESCARREGA,
      msgid type symsgid value 'ZEXC_MSG_MDFE',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_LOCAL_DESCARREGA .
  data MSGV1 type MSGV1 .
  data MSGV2 type MSGV2 .
  data MSGV3 type MSGV3 .
  data MSGV4 type MSGV4 .
  data MSGTY type SYST_MSGTY .
  data MSGNO type SYST_MSGNO .
  data MSGID type SYST_MSGID .
  data TRANSACAO type TCODE .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type MSGV1 optional
      !MSGV2 type MSGV2 optional
      !MSGV3 type MSGV3 optional
      !MSGV4 type MSGV4 optional
      !MSGTY type SYST_MSGTY optional
      !MSGNO type SYST_MSGNO optional
      !MSGID type SYST_MSGID optional
      !TRANSACAO type TCODE optional .
protected section.
*"* protected components of class ZCX_MDFE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCX_MDFE
*"* do not include other source files here!!!

  methods PUBLISHED_ERRO
    importing
      !I_MSGTY type SYST_MSGTY
      !I_MSGTY_DISPLAY type SYST_MSGTY .
ENDCLASS.



CLASS ZCX_MDFE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
me->MSGTY = MSGTY .
me->MSGNO = MSGNO .
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
