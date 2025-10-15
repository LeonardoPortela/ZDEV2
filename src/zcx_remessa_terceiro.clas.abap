CLASS zcx_remessa_terceiro DEFINITION
  PUBLIC
  INHERITING FROM zcx_error
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF zcx_ordem_venda_nao_existe,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_nao_existe .
    CONSTANTS:
      BEGIN OF zcx_ordem_venda_nao_co,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_nao_co .
    CONSTANTS:
      BEGIN OF zcx_ordem_venda_nao_rr,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_nao_rr .
    CONSTANTS:
      BEGIN OF zcx_ordem_venda_parceiro,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_parceiro .
    CONSTANTS:
      BEGIN OF zcx_ordem_venda_filial,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_filial .
    CONSTANTS:
      BEGIN OF zcx_ordem_venda_sem_dco,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_sem_dco .
    CONSTANTS:
      BEGIN OF zcx_ordem_venda_centro_erro,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_centro_erro .
    CONSTANTS:
      BEGIN OF zcx_ordem_venda_tipo_frete,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_tipo_frete .
    CONSTANTS:
      BEGIN OF zcx_ordem_venda_tipo_frete_err,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_tipo_frete_err .
    CONSTANTS:
      BEGIN OF zcx_ordem_venda_safra,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_safra .
    CONSTANTS:
      BEGIN OF zcx_armazem_errado,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_armazem_errado .
    CONSTANTS:
      BEGIN OF zcx_ordem_sem_itinerario,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_sem_itinerario .
    CONSTANTS:
      BEGIN OF zcx_ordem_zona_partida,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_zona_partida .
    CONSTANTS:
      BEGIN OF zcx_ordem_zona_chegada,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_zona_chegada .
    CONSTANTS:
      BEGIN OF zcx_preco_frete,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_preco_frete .
    CONSTANTS:
      BEGIN OF zcx_preco_frete_empresa,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_preco_frete_empresa .
    CONSTANTS:
      BEGIN OF zcx_preco_frete_motorista,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_preco_frete_motorista .
    CONSTANTS:
      BEGIN OF zcx_obrig_ordem_carrega,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obrig_ordem_carrega .
    CONSTANTS:
      BEGIN OF zcx_ord_venda_ord_carrega,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ord_venda_ord_carrega .
    CONSTANTS:
      BEGIN OF zcx_ordem_agente_frete,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_agente_frete .
    CONSTANTS:
      BEGIN OF zcx_erro_criar_ov_dummy,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '021',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_criar_ov_dummy .
    CONSTANTS:
      BEGIN OF zcx_erro_criar_remessa_dummy,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '022',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_criar_remessa_dummy .
    CONSTANTS:
      BEGIN OF zcx_erro_criar_transporte,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '023',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_criar_transporte .
    CONSTANTS:
      BEGIN OF zcx_erro_criar_doc_custo,
        msgid TYPE symsgid VALUE 'ZODVENDA',
        msgno TYPE symsgno VALUE '024',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_criar_doc_custo.

    METHODS constructor
      IMPORTING
        !textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous  LIKE previous OPTIONAL
        !msgid     TYPE syst_msgid OPTIONAL
        !msgno     TYPE syst_msgno OPTIONAL
        !msgty     TYPE syst_msgty OPTIONAL
        !msgv1     TYPE syst_msgv OPTIONAL
        !msgv2     TYPE syst_msgv OPTIONAL
        !msgv3     TYPE syst_msgv OPTIONAL
        !msgv4     TYPE syst_msgv OPTIONAL
        !transacao TYPE tcode OPTIONAL .
    METHODS published_erro
      IMPORTING
        !i_msgty         TYPE syst_msgty OPTIONAL
        !i_msgty_display TYPE syst_msgty OPTIONAL .
protected section.
private section.
ENDCLASS.



CLASS ZCX_REMESSA_TERCEIRO IMPLEMENTATION.


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


  METHOD published_erro.

    DATA: p_msgty	        TYPE syst_msgty,
          p_msgty_display	TYPE syst_msgty.


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
