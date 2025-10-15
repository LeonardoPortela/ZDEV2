CLASS zcx_parceiros DEFINITION
  PUBLIC
  INHERITING FROM zcx_error
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF zcx_nao_local_negocio,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_local_negocio .
    CONSTANTS:
      BEGIN OF zcx_nao_cliente,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_cliente .
    CONSTANTS:
      BEGIN OF zcx_nao_fornecedor,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_fornecedor .
    CONSTANTS:
      BEGIN OF zcx_cli_nao_contabiliza,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cli_nao_contabiliza .
    CONSTANTS:
      BEGIN OF zcx_for_nao_contabiliza,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_for_nao_contabiliza .
    CONSTANTS:
      BEGIN OF zcx_cli_marc_eliminacao,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cli_marc_eliminacao .
    CONSTANTS:
      BEGIN OF zcx_for_marc_eliminacao,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_for_marc_eliminacao .
    CONSTANTS:
      BEGIN OF zcx_cli_bloq_central,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cli_bloq_central .
    CONSTANTS:
      BEGIN OF zcx_for_bloq_central,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_for_bloq_central .
    CONSTANTS:
      BEGIN OF zcx_cli_nao_empresa,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cli_nao_empresa .
    CONSTANTS:
      BEGIN OF zcx_for_nao_empresa,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_for_nao_empresa .
    CONSTANTS:
      BEGIN OF zcx_cli_nao_contabiliza_emp,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cli_nao_contabiliza_emp .
    CONSTANTS:
      BEGIN OF zcx_for_nao_contabiliza_emp,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_for_nao_contabiliza_emp .
    CONSTANTS:
      BEGIN OF zcx_cli_marc_eliminacao_emp,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cli_marc_eliminacao_emp .
    CONSTANTS:
      BEGIN OF zcx_for_marc_eliminacao_emp,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_for_marc_eliminacao_emp .
    CONSTANTS:
      BEGIN OF zcx_nao_terceiro,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_terceiro .
    CONSTANTS:
      BEGIN OF zcx_fornecedor_servico,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_fornecedor_servico .
    CONSTANTS:
      BEGIN OF zcx_fornecedor_servico_rodo,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_fornecedor_servico_rodo .
    CONSTANTS:
      BEGIN OF zcx_cliente_nao_fornecedor,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cliente_nao_fornecedor .
    CONSTANTS:
      BEGIN OF zcx_parceiro_nao_intercompany,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_parceiro_nao_intercompany .
    CONSTANTS:
      BEGIN OF zcx_informar_cnpj_cpf,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '021',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_informar_cnpj_cpf .
    CONSTANTS:
      BEGIN OF zcx_coordenadas_bancarias,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '022',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_coordenadas_bancarias .
    CONSTANTS:
      BEGIN OF zcx_nao_emissor_nfe,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '023',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_emissor_nfe .
    CONSTANTS:
      BEGIN OF zcx_sem_cidade_idioma,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '024',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_cidade_idioma .
    CONSTANTS:
      BEGIN OF zcx_parceiro_emp_iguais,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '025',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_parceiro_emp_iguais,
      BEGIN OF zcx_cli_bloq_ordem_center,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '026', "zcx_cli_bloq_ordem_center
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cli_bloq_ordem_center,
      BEGIN OF zcx_cli_bloq_remessa_center,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '027', "zcx_cli_bloq_remessa_center
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cli_bloq_remessa_center,
      BEGIN OF zcx_cli_bloq_fatura_center,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '028', "zcx_cli_bloq_fatura_center
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cli_bloq_fatura_center,
      BEGIN OF zcx_cli_bloq_contato_central,
        msgid TYPE symsgid VALUE 'ZPARCEIROS',
        msgno TYPE symsgno VALUE '029', "zcx_cli_bloq_contato_central
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cli_bloq_contato_central      .


*        IF wa_kna1-aufsd EQ abap_true."  Bloqueio de ordem centralizado para cliente
*          msgid  = zcx_parceiros=>zcx_cli_bloq_ordem_center-msgid
*    ENDIF.
*
*    IF wa_kna1-lifsd EQ abap_true."  Bloqueio de remessa centralizado para cliente
*          msgno  = zcx_parceiros=>zcx_cli_bloq_remessa_center-msgno
*    ENDIF.
*
*    IF wa_kna1-faksd EQ abap_true. "Bloqueio centralizado de faturamento para cliente
*          msgno  = zcx_parceiros=>zcx_cli_bloq_fatura_center-msgno
*    ENDIF.
*
*    IF wa_kna1-cassd EQ abap_true."  Bloqueio de contatos central para cliente
*          msgno  = zcx_parceiros=>zcx_cli_bloq_contato_central-msgno
*    ENDIF.

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
        !i_msgty         TYPE syst_msgty
        !i_msgty_display TYPE syst_msgty .
protected section.
private section.
ENDCLASS.



CLASS ZCX_PARCEIROS IMPLEMENTATION.


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

    P_MSGTY         = I_MSGTY.
    P_MSGTY_DISPLAY = I_MSGTY_DISPLAY.

    MESSAGE ID ME->IF_T100_MESSAGE~T100KEY-MSGID TYPE P_MSGTY NUMBER ME->IF_T100_MESSAGE~T100KEY-MSGNO WITH ME->MSGV1 ME->MSGV2 ME->MSGV3 ME->MSGV4 DISPLAY LIKE P_MSGTY_DISPLAY.

  ENDMETHOD.
ENDCLASS.
