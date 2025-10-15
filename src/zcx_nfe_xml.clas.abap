class ZCX_NFE_XML definition
  public
  inheriting from ZCX_ERROR
  final
  create public .

public section.

  constants:
    begin of ZCX_NFE_NAO_LOCALIZADO,
      msgid type symsgid value 'ZNFE_DISTRI',
      msgno type symsgno value '143',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NFE_NAO_LOCALIZADO .
  constants:
    begin of ZCX_NFE_NAO_AUTORIZADA_USO,
      msgid type symsgid value 'ZNFE_DISTRI',
      msgno type symsgno value '145',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NFE_NAO_AUTORIZADA_USO .
  constants:
    begin of ZCX_NFE_CANCELADA,
      msgid type symsgid value 'ZNFE_DISTRI',
      msgno type symsgno value '144',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NFE_CANCELADA .
  constants:
    begin of ZCX_NFE_NAO_EXPORTACAO,
      msgid type symsgid value 'ZNFE_DISTRI',
      msgno type symsgno value '146',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NFE_NAO_EXPORTACAO .
  constants:
    begin of ZCX_NFE_UND_EXP_ERRADA,
      msgid type symsgid value 'ZNFE_DISTRI',
      msgno type symsgno value '147',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NFE_UND_EXP_ERRADA .

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



CLASS ZCX_NFE_XML IMPLEMENTATION.


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
