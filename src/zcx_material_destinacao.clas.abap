class ZCX_MATERIAL_DESTINACAO definition
  public
  inheriting from ZCX_ERROR
  final
  create public .

public section.

  constants:
    begin of ZCX_ORIGEM_NAO_PERMITIDA,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORIGEM_NAO_PERMITIDA .
  constants:
    begin of ZCX_ORIGEM_INFORMAR,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORIGEM_INFORMAR .
  constants:
    begin of ZCX_ORIGEM_VALIDA,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '003',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORIGEM_VALIDA .
  constants:
    begin of ZCX_ORIGEM_DOC_SAIDA_NAO,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '004',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORIGEM_DOC_SAIDA_NAO .
  constants:
    begin of ZCX_ORIGEM_DOC_ESTORNADO,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '005',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORIGEM_DOC_ESTORNADO .
  constants:
    begin of ZCX_TIPO_MOVIMENTO,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '006',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TIPO_MOVIMENTO .
  constants:
    begin of ZCX_EMPRESA,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '007',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_EMPRESA .
  constants:
    begin of ZCX_ORIGEM_SEM_SALDO,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '008',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_ORIGEM_SEM_SALDO .
  constants:
    begin of ZCX_NOT_FOUND_DEST,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '009',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NOT_FOUND_DEST .
  constants:
    begin of ZCX_NOT_FOUND_DEST_MAT,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NOT_FOUND_DEST_MAT .
  constants:
    begin of ZCX_ROM_SAI_NAO_FECHADO,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '011',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_ROM_SAI_NAO_FECHADO .
  constants:
    begin of ZCX_ROM_SAI_TAX_CODE,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '012',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ROM_SAI_TAX_CODE .
  constants:
    begin of ZCX_PEDIDO_NAO_PERMITIDO,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '013',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PEDIDO_NAO_PERMITIDO .
  constants:
    begin of ZCX_ORIGEM_DOC_ENTRADA_NAO,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '017',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORIGEM_DOC_ENTRADA_NAO .
  constants:
    begin of ZCX_ROM_ENT_NAO_CONFERIDO,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '018',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_ROM_ENT_NAO_CONFERIDO .
  constants:
    begin of ZCX_DOC_MATERIAL_SEM_SALDO,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '019',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_DOC_MATERIAL_SEM_SALDO .
  constants:
    begin of ZCX_ESTOQUE_ESPECIAL,
      msgid type symsgid value 'ZDESTINACAO',
      msgno type symsgno value '020',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_ESTOQUE_ESPECIAL .

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



CLASS ZCX_MATERIAL_DESTINACAO IMPLEMENTATION.


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
