class ZCX_DEPOSITO definition
  public
  inheriting from ZCX_ERROR
  final
  create public .

public section.

  constants:
    begin of ZCX_CENTRO_A_FIXAR_DEPOSITO,
      msgid type symsgid value 'ZDEPOSITO',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_CENTRO_A_FIXAR_DEPOSITO .
  constants:
    begin of ZCX_CENTRO_A_FIXAR,
      msgid type symsgid value 'ZDEPOSITO',
      msgno type symsgno value '002',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'TRANSACAO',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CENTRO_A_FIXAR .
  constants:
    begin of ZCX_CENTRO_FIXO,
      msgid type symsgid value 'ZDEPOSITO',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'TRANSACAO',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CENTRO_FIXO .

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
      !MSGV4 type SYST_MSGV optional
      !TRANSACAO type TCODE optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_DEPOSITO IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGID = MSGID
MSGNO = MSGNO
MSGTY = MSGTY
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
TRANSACAO = TRANSACAO
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
