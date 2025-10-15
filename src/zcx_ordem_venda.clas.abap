class ZCX_ORDEM_VENDA definition
  public
  inheriting from ZCX_ERROR
  final
  create public .

public section.

  constants:
    begin of ZCX_ORDEM_VENDA_NAO_EXISTE,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_VENDA_NAO_EXISTE .
  constants:
    begin of ZCX_ORDEM_VENDA_NAO_CO,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '002',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_VENDA_NAO_CO .
  constants:
    begin of ZCX_ORDEM_VENDA_NAO_RR,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_VENDA_NAO_RR .
  constants:
    begin of ZCX_ORDEM_VENDA_PARCEIRO,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_VENDA_PARCEIRO .
  constants:
    begin of ZCX_ORDEM_VENDA_FILIAL,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '005',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_VENDA_FILIAL .
  constants:
    begin of ZCX_ORDEM_VENDA_SEM_DCO,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_VENDA_SEM_DCO .
  constants:
    begin of ZCX_ORDEM_VENDA_CENTRO_ERRO,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '007',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_VENDA_CENTRO_ERRO .
  constants:
    begin of ZCX_ORDEM_VENDA_TIPO_FRETE,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '008',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_VENDA_TIPO_FRETE .
  constants:
    begin of ZCX_ORDEM_VENDA_TIPO_FRETE_ERR,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '009',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_VENDA_TIPO_FRETE_ERR .
  constants:
    begin of ZCX_ORDEM_VENDA_SAFRA,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_VENDA_SAFRA .
  constants:
    begin of ZCX_ARMAZEM_ERRADO,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '011',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ARMAZEM_ERRADO .
  constants:
    begin of ZCX_ORDEM_SEM_ITINERARIO,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '012',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_SEM_ITINERARIO .
  constants:
    begin of ZCX_ORDEM_ZONA_PARTIDA,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '013',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_ZONA_PARTIDA .
  constants:
    begin of ZCX_ORDEM_ZONA_CHEGADA,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '014',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_ZONA_CHEGADA .
  constants:
    begin of ZCX_PRECO_FRETE,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '015',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PRECO_FRETE .
  constants:
    begin of ZCX_PRECO_FRETE_EMPRESA,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '016',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PRECO_FRETE_EMPRESA .
  constants:
    begin of ZCX_PRECO_FRETE_MOTORISTA,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '017',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PRECO_FRETE_MOTORISTA .
  constants:
    begin of ZCX_OBRIG_ORDEM_CARREGA,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '018',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBRIG_ORDEM_CARREGA .
  constants:
    begin of ZCX_ORD_VENDA_ORD_CARREGA,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '019',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORD_VENDA_ORD_CARREGA .
  constants:
    begin of ZCX_ORDEM_AGENTE_FRETE,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '020',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_AGENTE_FRETE .
  constants:
    BEGIN OF ZCX_ORDEM_VENDA_CPT_OD,
      msgid type symsgid value 'ZODVENDA',
      msgno type symsgno value '021',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    END  OF ZCX_ORDEM_VENDA_CPT_OD.

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
  methods PUBLISHED_ERRO
    importing
      !I_MSGTY type SYST_MSGTY optional
      !I_MSGTY_DISPLAY type SYST_MSGTY optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ORDEM_VENDA IMPLEMENTATION.


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
