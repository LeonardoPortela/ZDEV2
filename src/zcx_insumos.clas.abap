class ZCX_INSUMOS definition
  public
  inheriting from ZCX_ERROR
  final
  create public .

public section.

  constants:
    BEGIN OF ZCX_MATERIAL_NAO_ENCONTRADO,
        MSGID TYPE SYMSGID VALUE 'ZINSUMOS',
        MSGNO TYPE SYMSGNO VALUE '002',
        ATTR1 TYPE SCX_ATTRNAME VALUE 'MSGV1',
        ATTR2 TYPE SCX_ATTRNAME VALUE '',
        ATTR3 TYPE SCX_ATTRNAME VALUE '',
        ATTR4 TYPE SCX_ATTRNAME VALUE '',
      END OF ZCX_MATERIAL_NAO_ENCONTRADO .

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



CLASS ZCX_INSUMOS IMPLEMENTATION.


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
