class ZCX_ZHCM definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

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



CLASS ZCX_ZHCM IMPLEMENTATION.


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


  METHOD published_erro.

    DATA: p_msgty          TYPE syst_msgty,
          p_msgty_display  TYPE syst_msgty.


    IF i_msgty IS NOT INITIAL.
      p_msgty = i_msgty.
    ELSE.
      p_msgty = me->msgty.
    ENDIF.

    IF i_msgty_display IS NOT INITIAL.
      p_msgty_display = i_msgty_display.
    ELSE.
      p_msgty_display = me->msgty.
    ENDIF.

    IF me->transacao IS NOT INITIAL.
      IF me->msgv1 IS INITIAL.
        me->msgv1 = me->transacao.
      ENDIF.
      IF me->msgv2 IS INITIAL.
        me->msgv2 = me->transacao.
      ENDIF.
      IF me->msgv3 IS INITIAL.
        me->msgv3 = me->transacao.
      ENDIF.
      IF me->msgv4 IS INITIAL.
        me->msgv4 = me->transacao.
      ENDIF.
    ENDIF.

    MESSAGE ID me->msgid TYPE p_msgty NUMBER me->msgno WITH me->msgv1 me->msgv2 me->msgv3 me->msgv4 DISPLAY LIKE p_msgty_display.

  ENDMETHOD.
ENDCLASS.
