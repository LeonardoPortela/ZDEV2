class ZCX_CADASTRO definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_NAO_GERA_AVISO_RECEBIMENTO,
      msgid type symsgid value 'ZCADASTRO',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_NAO_GERA_AVISO_RECEBIMENTO .
  constants:
    begin of ZCX_BLOQUEADO,
      msgid type symsgid value 'MC',
      msgno type symsgno value '601',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_BLOQUEADO .
  constants:
    begin of ZCX_NAO_GERA_MIGO,
      msgid type symsgid value 'ZNFE_DISTRI',
      msgno type symsgno value '057',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NAO_GERA_MIGO .
  constants:
    begin of ZCX_ERRO_GERAL,
      msgid type symsgid value 'ZCADASTRO',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_ERRO_GERAL .
  data MSGID type SYST_MSGID .
  data MSGNO type SYST_MSGNO .
  data MSGTY type SYST_MSGTY .
  data MSGV1 type SYST_MSGV .
  data MSGV2 type SYST_MSGV .
  data MSGV3 type SYST_MSGV .
  data MSGV4 type SYST_MSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGID type SYST_MSGID optional
      !MSGNO type SYST_MSGNO optional
      !MSGTY type SYST_MSGTY optional
      !MSGV1 type SYST_MSGV optional
      !MSGV2 type SYST_MSGV optional
      !MSGV3 type SYST_MSGV optional
      !MSGV4 type SYST_MSGV optional .
  methods PUBLISHED_ERRO
    importing
      !I_MSGTY type SYST_MSGTY
      !I_MSGTY_DISPLAY type SYST_MSGTY .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CADASTRO IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGID = MSGID .
me->MSGNO = MSGNO .
me->MSGTY = MSGTY .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
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

    P_MSGTY         = I_MSGTY.
    P_MSGTY_DISPLAY = I_MSGTY_DISPLAY.

    MESSAGE ID ME->IF_T100_MESSAGE~T100KEY-MSGID TYPE P_MSGTY NUMBER ME->IF_T100_MESSAGE~T100KEY-MSGNO WITH ME->MSGV1 ME->MSGV2 ME->MSGV3 ME->MSGV4 DISPLAY LIKE P_MSGTY_DISPLAY.

  ENDMETHOD.
ENDCLASS.
