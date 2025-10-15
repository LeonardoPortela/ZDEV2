class ZCL_PEDIDO_COMPRA definition
  public
  final
  create public .

public section.

  interfaces ZIF_PESQUISA .

  constants:
    BEGIN OF zcx_ped_compra_erro_geral,
        msgid TYPE symsgid VALUE 'ZMMPED',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ped_compra_erro_geral .
  constants:
    BEGIN OF zcx_ped_compra_sem_itinerario,
        msgid TYPE symsgid VALUE 'ZMMPED',
        msgno TYPE symsgno VALUE '021',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ped_compra_sem_itinerario .
  constants:
    BEGIN OF zcx_ped_compra_zona_partida,
        msgid TYPE symsgid VALUE 'ZMMPED',
        msgno TYPE symsgno VALUE '022',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ped_compra_zona_partida .
  constants:
    BEGIN OF zcx_ped_compra_zona_chegada,
        msgid TYPE symsgid VALUE 'ZMMPED',
        msgno TYPE symsgno VALUE '023',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ped_compra_zona_chegada .
  constants:
    BEGIN OF zcx_preco_frete,
        msgid TYPE symsgid VALUE 'ZMMPED',
        msgno TYPE symsgno VALUE '024',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_preco_frete .
  constants:
    BEGIN OF zcx_preco_frete_empresa,
        msgid TYPE symsgid VALUE 'ZMMPED',
        msgno TYPE symsgno VALUE '025',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_preco_frete_empresa .
  constants:
    BEGIN OF zcx_preco_frete_motorista,
        msgid TYPE symsgid VALUE 'ZMMPED',
        msgno TYPE symsgno VALUE '026',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_preco_frete_motorista .
  constants:
    BEGIN OF zcx_obrig_ordem_carrega,
        msgid TYPE symsgid VALUE 'ZPEDCOMP',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obrig_ordem_carrega .
  constants:
    BEGIN OF zcx_ord_venda_ord_carrega,
        msgid TYPE symsgid VALUE 'ZPEDCOMP',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ord_venda_ord_carrega .
  constants:
    BEGIN OF zcx_ordem_agente_frete,
        msgid TYPE symsgid VALUE 'ZMMPED',
        msgno TYPE symsgno VALUE '027',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_agente_frete .
  constants:
    BEGIN OF zcx_ped_sem_agente_frete,
        msgid TYPE symsgid VALUE 'ZMMPED',
        msgno TYPE symsgno VALUE '028',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ped_sem_agente_frete .
  constants ST_TP_CENTRO_A_FIXAR type ZDE_TP_CENTRO_VIRTUAL value '1' ##NO_TEXT.
  constants ST_TP_CENTRO_PORTO type ZDE_TP_CENTRO_VIRTUAL value '2' ##NO_TEXT.
  constants ST_TP_CENTRO_REAL type ZDE_TP_CENTRO_VIRTUAL value ' ' ##NO_TEXT.
  constants ST_BSART_PCE type BSART value 'PCE' ##NO_TEXT.
  class-data AT_PEDIDO_COMPRA type ref to ZCL_PEDIDO_COMPRA .
  data EKKO type EKKO .
  data EKPO_T type ME_EKPO .
  data AT_EKKO type EKKO .
  data AT_EKPO type EKPO .
  data AT_EKPA type ZEKPA_TAB .
  data AT_EKPV type EKPV .
  data AT_EKET type EKET .

  class-methods GET_PEDIDO_COMPRA_CHAVE_E
    importing
      !I_LIFNR type LIFNR optional
      !I_BUKRS type BUKRS optional
      !I_WERKS type EWERK optional
      !I_MATNR type MATNR optional
      !I_LGORT type LGORT_D optional
      !I_CHARG type CHARG_D optional
      !I_EBELN type EBELN optional
      !I_EBELP type EBELP optional
      !I_BSTYP type EBSTYP optional
      !I_BSART type ESART optional
      !I_MENGE_GE type BSTMG optional
      !I_MEINS type BSTME optional
      !I_ZMMT0075 type ZDE_ZMMT0075_T optional
      !I_ABRIR_TELA type CHAR01 default 'X'
      !I_MATNR_T type EHS_MATNR_T optional
      !I_FILTRO_1 type ZDE_EKPO_HELP_SALDO_T optional
    exporting
      !R_SALDO_ITEM type ZDE_EKPO_HELP_SALDO
      !E_EKPO_T type ZDE_EKPO_HELP_SALDO_T
    returning
      value(R_EKPO) type EKPO
    exceptions
      NAO_ENCONTRADO_PEDIDO .
  class-methods GET_PEDIDO
    importing
      !I_EBELN type EBELN
    returning
      value(R_EKKO) type EKKO
    exceptions
      NAO_ACHOU_PEDIDO .
  methods GET_PEDIDO_PC
    exporting
      !E_PEDIDO_COMPRA type EKKO
    returning
      value(R_INSTANCIA) type ref to ZCL_PEDIDO_COMPRA
    raising
      ZCX_PEDIDO_COMPRA .
  methods GET_ITEM_PC
    exporting
      !E_EKPO type EKPO
      !E_EKPV type EKPV
      !E_EKET type EKET
    returning
      value(R_INSTANCIA) type ref to ZCL_PEDIDO_COMPRA .
  class-methods GERA_ERRO_GERAL
    raising
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  class-methods GERA_ERRO_GERAL_STRING
    importing
      !I_TEXTO type STRING
    raising
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  class-methods GET_PEDIDO_ITENS
    importing
      !I_EBELN type EBELN
      !I_EBELP type EBELP optional
    exporting
      !E_EKKO type EKKO
      !E_EKPO_T type ME_EKPO
    exceptions
      NAO_ACHOU_PEDIDO .
  class-methods GET_CHAVE_CONTROLE_CONF_ITEM
    importing
      !I_EBELN type EBELN
      !I_EBELP type EBELP
    exporting
      !E_WEBRE type WEBRE
      !E_KZABS type KZABS
    returning
      value(R_BSTAE) type BSTAE
    raising
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  class-methods SHOW_PEDIDO
    importing
      !I_EBELN type EBELN .
  class-methods SET_CRIAR_PEDIDO_COMPRA
    importing
      !I_BEDAT type EBDAT
      !I_EKORG type EKORG
      !I_EKGRP type BKGRP
      !I_WAERS type WAERS
      !I_BSART type ESART
      !I_ZTERM type DZTERM
      !I_LIFNR type ELIFN
      !I_BUKRS type BUKRS
      !I_LGORT type LGORT_D optional
      !I_CHARG type CHARG_D
      !I_EINDT type EINDT
      !I_MWSKZ type MWSKZ
      !I_MENGE type BSTMG
      !I_MATNR type MATNR
      !I_MEINS type BSTME
      !I_ELIKZ type ELIKZ optional
      !I_LOEKZ type ELOEK optional
      !I_WERKS type EWERK
      !I_TP_CENTRO type ZDE_TP_CENTRO_VIRTUAL default ' '
      !I_WERKS_D type EKORG optional
      !I_NET_PRICE type BAPICUREXT optional
      !I_COLLECT_NO type VBELN_VA optional
    returning
      value(R_EBELN) type EBELN
    raising
      ZCX_CADASTRO
      ZCX_PEDIDO_COMPRA_EXCEPTION
      ZCX_JOB .
  class-methods GET_VALIDA_PEDIDO_NOTA
    importing
      !I_EBELN type EBELN
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
      !I_VERF_EXISTE type CHAR01 default 'X'
    raising
      ZCX_PEDIDO_COMPRA_EXCEPTION
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CADASTRO .
  class-methods GET_PEDIDOS_APROVACAO
    returning
      value(R_PEDIDOS) type ZDE_PEDIDOS_APROVACAO_T
    raising
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  class-methods VERIF_BLOQ_PEDIDO_WAIT
    importing
      !I_EBELN type EBELN
    raising
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  class-methods GET_INSTANCE
    returning
      value(R_INSTANCIA) type ref to ZCL_PEDIDO_COMPRA .
  methods SET_PEDIDO
    importing
      !I_EBELN type EBELN
    returning
      value(R_INSTANCIA) type ref to ZCL_PEDIDO_COMPRA
    raising
      ZCX_PEDIDO_COMPRA .
  methods SET_PEDIDO_PC
    importing
      !I_EBELN type EBELN
    returning
      value(R_INSTANCIA) type ref to ZCL_PEDIDO_COMPRA .
  methods GET_TIPO_FRETE
    exporting
      !E_TIPO_FRETE type ZDE_TP_FRETE
    returning
      value(R_INSTANCIA) type ref to ZCL_PEDIDO_COMPRA
    raising
      ZCX_PEDIDO_COMPRA .
  methods GET_CENTRO
    exporting
      !E_WERKS type EWERK
      !E_PARID_WERKS type ZPARID
    returning
      value(R_INSTANCIA) type ref to ZCL_PEDIDO_COMPRA .
  class-methods GET_SALDO_PEDIDO_ITEM
    importing
      !I_EBELN type EBELN
      !I_EBELP type EBELP optional
    exporting
      !E_MEINS type MEINS
    returning
      value(R_SALDO) type BSTMG
    raising
      ZCX_PEDIDO_COMPRA .
  methods GET_CK_VALOR_FRETE
    importing
      !I_VALOR_FRETE_EMPRESA type KBETR_KOND
      !I_VALOR_FRETE_MOTORISTA type KBETR_KOND
      !I_PLACA_VEIC_TRACAO type ZPC_VEICULO
      !I_ID_ORDEM type ZDE_ID_ORDEM optional
      !I_ZLEST0181 type ZLEST0181 optional
      !I_VIAGEM_ID type ZLEST0185-VIAGEM_ID optional
      !I_PAGA_TRECHO_TERCEIRO type ZDE_CARGUERO_REQ_DATA-PAGA_TRECHO_TERCEIRO optional
    exporting
      !E_PARAMETROS_EMPRESA type STRING
      !E_PARAMETROS_MOTORISTA type STRING
      !E_VALOR_FRETE_ENTRADA type KBETR_KOND
      !E_VALOR_FRETE_MOTORISTA type KBETR_KOND
    returning
      value(R_INSTANCIA) type ref to ZCL_PEDIDO_COMPRA
    raising
      ZCX_PEDIDO_COMPRA
      ZCX_ERROR
      ZCX_CALC_FRETE .
  methods GET_TIPO_TRANSPORTE
    importing
      !I_VSART type VERSART
    exporting
      !E_TIPO_TRANSPORTE type SHTYP
    returning
      value(R_INSTANCIA) type ref to ZCL_PEDIDO_COMPRA .
  methods GET_ZONAS_TRANSPORTE
    importing
      !I_FRETE_ENTRADA type CHAR01 optional
    exporting
      !E_LZONEA type LZONEA
      !E_LZONEZ type LZONEZ
      !E_LC_COLETA type LIFNR
      !E_LC_ENTREGA type KUNNR
    returning
      value(R_INSTANCIA) type ref to ZCL_PEDIDO_COMPRA
    raising
      ZCX_PEDIDO_COMPRA .
  methods GET_PARCEIROS
    exporting
      !E_EKPA type ZEKPA_TAB
    returning
      value(R_INSTANCIA) type ref to ZCL_PEDIDO_COMPRA .
  methods GET_SOLICITACAO
    importing
      !E_SOLICITACAO type ZDE_NRO_SOL
    exporting
      value(I_PEDIDO) type EBELN
    returning
      value(R_INSTANCIA) type ref to ZCL_PEDIDO_COMPRA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PEDIDO_COMPRA IMPLEMENTATION.


  METHOD GERA_ERRO_GERAL.

    RAISE EXCEPTION TYPE ZCX_SOL_MOBILE_RH
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_SOL_MOBILE_RH=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_SOL_MOBILE_RH=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_SOL_MOBILE_RH=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_SOL_MOBILE_RH=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD GERA_ERRO_GERAL_STRING.

    DATA: LC_TEXTO TYPE C LENGTH 200.

    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_PEDIDO_COMPRA_EXCEPTION
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_PEDIDO_COMPRA_EXCEPTION=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_PEDIDO_COMPRA_EXCEPTION=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_PEDIDO_COMPRA_EXCEPTION=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_PEDIDO_COMPRA_EXCEPTION=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  method GET_CENTRO.

    R_INSTANCIA = ME.

    CLEAR: E_WERKS, E_PARID_WERKS.

    READ TABLE ME->EKPO_T INDEX 1 INTO DATA(WA_EKPO).

    E_WERKS       = WA_EKPO-WERKS.
    E_PARID_WERKS = |{ WA_EKPO-WERKS ALPHA = IN }|.

  endmethod.


  METHOD GET_CHAVE_CONTROLE_CONF_ITEM.

    CLEAR: R_BSTAE.

*Chave de controle confirmação
*Controla que categorias de confirmação estão previstas para um item de pedido; p.ex. confirmação da ordem e aviso de entrega.

    SELECT SINGLE * INTO @DATA(WA_MASSEKPO)
      FROM MASSEKPO
     WHERE EBELN EQ @I_EBELN
       AND EBELP EQ @I_EBELP.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_PEDIDO_COMPRA_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PEDIDO_COMPRA_EXCEPTION=>ZCX_PEDIDO_ITEM_NAO_LOC-MSGID
                            MSGNO = ZCX_PEDIDO_COMPRA_EXCEPTION=>ZCX_PEDIDO_ITEM_NAO_LOC-MSGNO
                            ATTR1 = CONV #( I_EBELN )
                            ATTR2 = CONV #( I_EBELP ) )
          MSGTY  = 'E'
          MSGID  = ZCX_PEDIDO_COMPRA_EXCEPTION=>ZCX_PEDIDO_ITEM_NAO_LOC-MSGID
          MSGNO  = ZCX_PEDIDO_COMPRA_EXCEPTION=>ZCX_PEDIDO_ITEM_NAO_LOC-MSGNO
          MSGV1  = CONV #( I_EBELN )
          MSGV2  = CONV #( I_EBELP ).
    ELSE.
      R_BSTAE = WA_MASSEKPO-BSTAE.
      E_WEBRE = WA_MASSEKPO-WEBRE.
      E_KZABS = WA_MASSEKPO-KZABS.
    ENDIF.

    "Tabela de Parametrização de Movimento do tipo de confirmação de chegada
    "T163G

  ENDMETHOD.


  METHOD get_ck_valor_frete.

    DATA: lc_frete       TYPE zde_info_frete,
          lc_frete_ent   TYPE zde_info_frete,
          lc_texto_valor TYPE c LENGTH 14.

    r_instancia = me.
    CLEAR: e_parametros_empresa,
           e_parametros_motorista.

    "Itinerário
*    IF me->at_ekpv-route IS INITIAL.
*      RAISE EXCEPTION TYPE zcx_pedido_compra
*        EXPORTING
*          textid = VALUE #( msgid  = zcl_pedido_compra=>zcx_ped_compra_sem_itinerario-msgid
*                            msgno  = zcl_pedido_compra=>zcx_ped_compra_sem_itinerario-msgno
*                            attr1  = CONV #( me->at_ekko-ebeln ) )
*          msgty  = 'E'
*          msgid  = zcl_pedido_compra=>zcx_ped_compra_sem_itinerario-msgid
*          msgno  = zcl_pedido_compra=>zcx_ped_compra_sem_itinerario-msgno
*          msgv1  = CONV #( me->at_ekko-ebeln ).
*    ELSE.
*      lc_frete-route = me->at_ekpv-route.
*    ENDIF.

    IF me->at_ekpv-route is NOT INITIAL.
      lc_frete-route = me->at_ekpv-route.
    ENDIF.

    " Tipo de transporte
    me->get_tipo_transporte(
       EXPORTING
         i_vsart = '01'
       IMPORTING
         e_tipo_transporte = lc_frete-shtyp
    " Zona de partida " Zona de chegada
      )->get_zonas_transporte( IMPORTING e_lzonea = lc_frete-lzonea e_lzonez = lc_frete-lzonez
      ).

    TRY .
        " Tipo do Contrato
        DATA(lc_veiculo_tracao) =
          zcl_veiculos=>zif_veiculos~get_instance(
            )->set_veiculo( i_placa = i_placa_veic_tracao
            )->get_tipo_contrato( IMPORTING e_tipo_contrato = lc_frete-add01
            )->at_veiculo.

      CATCH zcx_veiculos INTO DATA(ex_veiculo).    "
        ex_veiculo->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
        RAISE EXCEPTION TYPE zcx_pedido_compra
          EXPORTING
            textid = VALUE #( msgid  = zcl_pedido_compra=>zcx_ped_compra_erro_geral-msgid
                              msgno  = zcl_pedido_compra=>zcx_ped_compra_erro_geral-msgno
                              attr1  = CONV #( sy-msgv1 ) attr2  = CONV #( sy-msgv2 ) attr3  = CONV #( sy-msgv3 ) attr4  = CONV #( sy-msgv4 ) )
            msgty  = 'E'
            msgid  = zcl_pedido_compra=>zcx_ped_compra_erro_geral-msgid
            msgno  = zcl_pedido_compra=>zcx_ped_compra_erro_geral-msgno
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4.
    ENDTRY.

    "Material
    lc_frete-matnr = me->at_ekpo-matnr.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(t_matkl_fert)
     WHERE name EQ 'MAGGI_GR_FERTILIZANTES'
       AND LOW  EQ @me->at_ekpo-matkl.

    IF SY-SUBRC EQ 0.
      DATA(_insumos) = abap_true.
    ELSE.
      _insumos = abap_false.
    ENDIF.

    "Veículo de Tração
    lc_frete-ds_placa_trator = i_placa_veic_tracao.

    "Somente CIF valida o preço do parceiro SP da OV.
    IF me->at_ekpo-inco1 NE zif_carga=>st_tp_frete_cpt AND
       me->at_ekpo-inco1 NE zif_carga=>st_tp_frete_cfr.
*      me->at_ekpo-kvgr5 NE '002'.

      READ TABLE me->at_ekpa WITH KEY parvw = 'SP' INTO DATA(wa_empresa).

      IF sy-subrc IS NOT INITIAL AND i_zlest0181 IS NOT INITIAL.

        TRY .

            zcl_fornecedores=>zif_parceiros~get_instance(
              )->set_parceiro( i_parceiro = i_zlest0181-id_parceiro_cole
              )->get_regio( IMPORTING e_regio = DATA(uf_coleta)
              ).

            "Procurar Por Local de Negócio
            SELECT SINGLE * INTO @DATA(wa_zlest0207)
              FROM zlest0207
             WHERE bukrs  EQ @i_zlest0181-bukrs
               AND branch EQ @i_zlest0181-branch
               AND regio  EQ @uf_coleta.

            IF sy-subrc IS NOT INITIAL.
              "Procurar Sem Local de Negócio
              SELECT SINGLE * INTO @wa_zlest0207
                FROM zlest0207
               WHERE bukrs  EQ @i_zlest0181-bukrs
                 AND regio  EQ @uf_coleta.
            ENDIF.

            IF sy-subrc IS INITIAL.
              APPEND VALUE #( parvw = 'SP' lifn2 = wa_zlest0207-tdlnr ) TO me->at_ekpa.
            ENDIF.

            READ TABLE me->at_ekpa WITH KEY parvw = 'SP' INTO wa_empresa.

*-BUG 56712 - 23.04.2021 - JT - inicio
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_pedido_compra
                EXPORTING
                  textid = VALUE #( msgid  = zcl_pedido_compra=>zcx_ped_sem_agente_frete-msgid
                                    msgno  = zcl_pedido_compra=>zcx_ped_sem_agente_frete-msgno
                                     )
                  msgty  = 'E'
                  msgid  = zcl_pedido_compra=>zcx_ped_sem_agente_frete-msgid
                  msgno  = zcl_pedido_compra=>zcx_ped_sem_agente_frete-msgno .
            ENDIF.
*-BUG 56712 - 23.04.2021 - JT - fim
*          CATCH zcx_parceiros.
*            sy-subrc = 1.
        ENDTRY.

      ENDIF.

*     IF sy-subrc IS INITIAL AND ( me->at_ekpo-zkvgr3 EQ 'SIM' OR me->at_ekpo-zkvgr4 IS INITIAL ).
      IF sy-subrc IS INITIAL AND ( me->at_ekpo-zkvgr3 EQ 'C' OR me->at_ekpo-zkvgr3 = 'R' or _insumos eq abap_true ).
        " Nº do agente de frete
        lc_frete-id_agent_frete = wa_empresa-lifn2.

        TRY .

            "Valida Frete Empresa
            zcl_calc_frete=>get_valor_frete(
              EXPORTING
                i_route        = lc_frete-route           " Itinerário
                i_tdlnr        = lc_frete-id_agent_frete  " Nº do agente de frete
                i_shtyp        = lc_frete-shtyp           " Tipo de transporte
                i_lzonea       = lc_frete-lzonea    " Zona de partida
                i_lzonez       = lc_frete-lzonez    " Zona de chegada
                i_add01        = lc_frete-add01     " Suplem.1
                i_matnr        = lc_frete-matnr     " Nº do material
*               i_ordem_venda  = me->zif_ordem_venda~at_vbak-vbeln  " Nº documento de vendas e distribuição
                i_placa_trator = lc_frete-ds_placa_trator " Placa Veículo Tração
*---------------CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
                i_id_carguero          = i_zlest0181-id_carguero
                i_viagem_id            = i_viagem_id
*---------------CS2019001158 - Jaime Tassoni - 16.11.2020 - fim
              IMPORTING
                e_kbetr        = lc_frete-kbetr " Montante/porcentagem de condição no caso de não haver escala
                e_konwa        = lc_frete-konwa " Unidade de condição (moeda ou porcentagem)
                e_krech        = lc_frete-krech " Regra de cálculo de condição
                e_lzonea       = DATA(e_lzonea)
                e_lzonez       = DATA(e_lzonez)
                e_route        = DATA(e_route)
            ).

            CONCATENATE 'Itinerário: ' e_route
                        'Agente de Frete:' lc_frete-id_agent_frete
                        'Tipo de Transporte:' lc_frete-shtyp
                        'Zona de Partida:' e_lzonea
                        'Zona de Chegada:' e_lzonez
                        'Contrato:' lc_frete-add01
                        'Material:' lc_frete-matnr
                        'Pedido Compras:' me->at_ekko-ebeln
                        'Viagem Id': i_viagem_id
                        'Placa Trator:' lc_frete-ds_placa_trator
                   INTO e_parametros_empresa SEPARATED BY space.


          CATCH zcx_calc_frete INTO DATA(ex_calc_frete).

            CONCATENATE 'Itinerário: ' e_route
                        'Agente de Frete:' lc_frete-id_agent_frete
                        'Tipo de Transporte:' lc_frete-shtyp
                        'Zona de Partida:' e_lzonea
                        'Zona de Chegada:' e_lzonez
                        'Contrato:' lc_frete-add01
                        'Material:' lc_frete-matnr
                        'Pedido Compras:' me->at_ekko-ebeln
                        'Viagem Id': i_viagem_id
                        'Placa Trator:' lc_frete-ds_placa_trator
                   INTO e_parametros_empresa SEPARATED BY space.

            RAISE EXCEPTION TYPE zcx_calc_frete
              EXPORTING
                textid = VALUE #( msgid = ex_calc_frete->msgid
                                  msgno = ex_calc_frete->msgno
                                  attr1 = CONV #( ex_calc_frete->msgv1 )
                                  attr2 = CONV #( ex_calc_frete->msgv2 )
                                  attr3 = CONV #( ex_calc_frete->msgv3 )
                                  attr4 = CONV #( ex_calc_frete->msgv4 ) )
                msgid  = ex_calc_frete->msgid
                msgno  = ex_calc_frete->msgno
                msgty  = 'E'
                msgv1  = ex_calc_frete->msgv1
                msgv2  = ex_calc_frete->msgv2
                msgv3  = ex_calc_frete->msgv3
                msgv4  = ex_calc_frete->msgv4.

        ENDTRY.

        IF lc_frete-kbetr         NE i_valor_frete_empresa AND
           i_paga_trecho_terceiro IS INITIAL.

          WRITE i_valor_frete_empresa TO lc_texto_valor.
          CONDENSE lc_texto_valor NO-GAPS.

          RAISE EXCEPTION TYPE zcx_pedido_compra
            EXPORTING
              textid = VALUE #( msgid  = zcl_pedido_compra=>zcx_preco_frete-msgid
                                msgno  = zcl_pedido_compra=>zcx_preco_frete-msgno
                                attr1  = lc_texto_valor )
              msgty  = 'E'
              msgid  = zcl_pedido_compra=>zcx_preco_frete-msgid
              msgno  = zcl_pedido_compra=>zcx_preco_frete-msgno
              msgv1  = CONV #( lc_texto_valor ).
        ENDIF.

      ELSEIF i_valor_frete_empresa IS NOT INITIAL.
        "Não pode ter valor de frete empresa
        RAISE EXCEPTION TYPE zcx_pedido_compra
          EXPORTING
            textid = VALUE #( msgid  = zcl_pedido_compra=>zcx_preco_frete_empresa-msgid
                              msgno  = zcl_pedido_compra=>zcx_preco_frete_empresa-msgno )
            msgty  = 'E'
            msgid  = zcl_pedido_compra=>zcx_preco_frete_empresa-msgid
            msgno  = zcl_pedido_compra=>zcx_preco_frete_empresa-msgno.
      ENDIF.
    ENDIF.

    CLEAR: wa_empresa.
    READ TABLE me->at_ekpa WITH KEY parvw = 'SP' INTO wa_empresa.

    IF ( lc_veiculo_tracao-proprietario IS INITIAL AND i_valor_frete_motorista IS NOT INITIAL ) OR
       ( lc_veiculo_tracao-proprietario IS NOT INITIAL AND i_valor_frete_motorista IS INITIAL ) AND
       ( sy-subrc IS NOT INITIAL ).
      "Não pode ter valor de frete motorista
      RAISE EXCEPTION TYPE zcx_pedido_compra
        EXPORTING
          textid = VALUE #( msgid  = zcl_pedido_compra=>zcx_preco_frete_motorista-msgid
                            msgno  = zcl_pedido_compra=>zcx_preco_frete_motorista-msgno )
          msgty  = 'E'
          msgid  = zcl_pedido_compra=>zcx_preco_frete_motorista-msgid
          msgno  = zcl_pedido_compra=>zcx_preco_frete_motorista-msgno.
    ELSE.

      IF sy-subrc IS NOT INITIAL.
        " Nº do agente de frete
        lc_frete-id_agent_frete = lc_veiculo_tracao-proprietario.
      ELSE.
        lc_frete-id_agent_frete = wa_empresa-lifn2.
      ENDIF.

      CONCATENATE 'Itinerário: ' lc_frete-route
                  'Agente de Frete:' lc_frete-id_agent_frete
                  'Tipo de Transporte:' lc_frete-shtyp
                  'Zona de Partida:' lc_frete-lzonea
                  'Zona de Chegada:' lc_frete-lzonez
                  'Contrato:' lc_frete-add01
                  'Material:' lc_frete-matnr
                  'Pedido Compras:' me->at_ekko-ebeln
                  'Viagem Id': i_viagem_id
                  'Placa Trator:' lc_frete-ds_placa_trator
             INTO e_parametros_motorista SEPARATED BY space.

      TRY .

          "Valida Frete Motorista Saída
          zcl_calc_frete=>get_valor_frete(
            EXPORTING
              i_route        = lc_frete-route           " Itinerário
              i_tdlnr        = lc_frete-id_agent_frete  " Nº do agente de frete
              i_shtyp        = lc_frete-shtyp           " Tipo de transporte
              i_lzonea       = lc_frete-lzonea    " Zona de partida //PRODUTOR
              i_lzonez       = lc_frete-lzonez    " Zona de chegada // LOCAL ENTREGA
              i_add01        = lc_frete-add01     " Suplem.1
              i_matnr        = lc_frete-matnr     " Nº do material
*             i_ordem_venda  = me->zif_ordem_venda~at_vbak-vbeln  " Nº documento de vendas e distribuição
              i_id_ordem     = i_id_ordem
              i_placa_trator = lc_frete-ds_placa_trator " Placa Veículo Tração
*---------------CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
              i_id_carguero          = i_zlest0181-id_carguero
              i_viagem_id            = i_viagem_id
*---------------CS2019001158 - Jaime Tassoni - 16.11.2020 - fim
            IMPORTING
              e_kbetr        = lc_frete-kbetr " Montante/porcentagem de condição no caso de não haver escala
              e_konwa        = lc_frete-konwa " Unidade de condição (moeda ou porcentagem)
              e_krech        = lc_frete-krech " Regra de cálculo de condição
              e_lzonea       = e_lzonea
              e_lzonez       = e_lzonez
              e_route        = e_route
          ).

          CONCATENATE 'Itinerário: ' e_route
                      'Agente de Frete:' lc_frete-id_agent_frete
                      'Tipo de Transporte:' lc_frete-shtyp
                      'Zona de Partida:' e_lzonea
                      'Zona de Chegada:' e_lzonez
                      'Contrato:' lc_frete-add01
                      'Material:' lc_frete-matnr
                      'Pedido Compras:' me->at_ekko-ebeln
                      'Viagem Id': i_viagem_id
                      'Placa Trator:' lc_frete-ds_placa_trator
                 INTO e_parametros_motorista SEPARATED BY space.

        CATCH zcx_calc_frete INTO ex_calc_frete.

          CONCATENATE 'Itinerário: ' e_route
                      'Agente de Frete:' lc_frete-id_agent_frete
                      'Tipo de Transporte:' lc_frete-shtyp
                      'Zona de Partida:' e_lzonea
                      'Zona de Chegada:' e_lzonez
                      'Contrato:' lc_frete-add01
                      'Material:' lc_frete-matnr
                      'Pedido Compras:' me->at_ekko-ebeln
                      'Viagem Id': i_viagem_id
                      'Placa Trator:' lc_frete-ds_placa_trator
                 INTO e_parametros_motorista SEPARATED BY space.

          RAISE EXCEPTION TYPE zcx_calc_frete
            EXPORTING
              textid = VALUE #( msgid = ex_calc_frete->msgid
                                msgno = ex_calc_frete->msgno
                                attr1 = CONV #( ex_calc_frete->msgv1 )
                                attr2 = CONV #( ex_calc_frete->msgv2 )
                                attr3 = CONV #( ex_calc_frete->msgv3 )
                                attr4 = CONV #( ex_calc_frete->msgv4 ) )
              msgid  = ex_calc_frete->msgid
              msgno  = ex_calc_frete->msgno
              msgty  = 'E'
              msgv1  = ex_calc_frete->msgv1
              msgv2  = ex_calc_frete->msgv2
              msgv3  = ex_calc_frete->msgv3
              msgv4  = ex_calc_frete->msgv4.

      ENDTRY.

      "Somar valor frete entrada
      IF me->at_ekpo-zkvgr3     EQ '002' OR
         i_paga_trecho_terceiro EQ abap_true.

        READ TABLE me->at_ekpa WITH KEY parvw = 'SP' INTO wa_empresa.
        IF sy-subrc IS INITIAL.

          DATA: rgcnpj TYPE RANGE OF stcd1.

          SELECT SINGLE * INTO @DATA(wa_lfa1)
            FROM lfa1
           WHERE lifnr EQ '0000000116'.

          rgcnpj = VALUE #( sign = 'I' option = 'CP' ( low = wa_lfa1-stcd1(8) && '*' ) ).

          SELECT SINGLE * INTO @DATA(wa_zlest0181)
            FROM zlest0181
           WHERE ebeln EQ @me->at_ekko-ebeln.

          SELECT SINGLE * INTO @DATA(wa_cole)
            FROM lfa1
           WHERE lifnr EQ @wa_zlest0181-id_parceiro_cole.

          SELECT SINGLE * INTO @DATA(wa_lfa1_regio)
            FROM lfa1
           WHERE regio EQ @wa_cole-regio
             AND ktokk EQ 'ZFIC'
             AND bahns NE @space
             AND dlgrp EQ '0001'
             AND stcd1 IN @rgcnpj.

          lc_frete_ent-shtyp = 'Z021'.
          lc_frete_ent-add01 = lc_frete-add01.
          lc_frete_ent-id_agent_frete = wa_lfa1_regio-lifnr.
          lc_frete_ent-matnr = lc_frete-matnr.
          lc_frete_ent-ds_placa_trator = lc_frete-ds_placa_trator.

          me->get_zonas_transporte(
            EXPORTING
              i_frete_entrada = abap_true
            IMPORTING
              e_lzonea     = lc_frete_ent-lzonea
              e_lzonez     = lc_frete_ent-lzonez
              e_lc_coleta  = DATA(e_lc_coleta)
              e_lc_entrega = DATA(e_lc_entrega) ).

          TRY .

              zcl_itinerario=>zif_itinerario~get_instance(
                )->get_itinerario_relevante(
                EXPORTING
                  i_cod_loc_coleta  = e_lc_coleta    " Nº conta do fornecedor
                  i_cod_loc_entrega = e_lc_entrega    " Nº cliente
                IMPORTING
                  e_tvro            = DATA(e_tvro)    " Itinerários
              ).

              lc_frete_ent-route = e_tvro-route.

            CATCH zcx_itinerario INTO DATA(ex_itinerario).    "

              RAISE EXCEPTION TYPE zcx_pedido_compra
                EXPORTING
                  textid = VALUE #( msgid = ex_itinerario->msgid
                                    msgno = ex_itinerario->msgno
                                    attr1 = ex_itinerario->msgv1
                                    attr2 = ex_itinerario->msgv2
                                    attr3 = ex_itinerario->msgv3
                                    attr4 = ex_itinerario->msgv4 )
                  msgid  = ex_itinerario->msgid
                  msgno  = ex_itinerario->msgno
                  msgty  = 'E'
                  msgv1  = ex_itinerario->msgv1
                  msgv2  = ex_itinerario->msgv2
                  msgv3  = ex_itinerario->msgv3
                  msgv4  = ex_itinerario->msgv4.

          ENDTRY.

          TRY .

              zcl_calc_frete=>get_valor_frete(
                EXPORTING
                  i_route        = lc_frete_ent-route           " Itinerário
                  i_tdlnr        = lc_frete_ent-id_agent_frete  " Nº do agente de frete
                  i_shtyp        = lc_frete_ent-shtyp           " Tipo de transporte
                  i_lzonea       = lc_frete_ent-lzonea          " Zona de partida  // PRODUTOR
                  i_lzonez       = lc_frete_ent-lzonez      " Zona de chegada  // FILIAL
                  i_add01        = lc_frete_ent-add01       " Suplem.1
                  i_matnr        = lc_frete_ent-matnr       " Nº do material
*                 i_ordem_venda  = me->zif_ordem_venda~at_vbak-vbeln  " Nº documento de vendas e distribuição
                  i_id_ordem     = i_id_ordem
                  i_placa_trator = lc_frete_ent-ds_placa_trator " Placa Veículo Tração
                  i_frete_entrada = abap_true
*---------------CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
                  i_id_carguero          = i_zlest0181-id_carguero
                  i_viagem_id            = i_viagem_id
*---------------CS2019001158 - Jaime Tassoni - 16.11.2020 - fim
                IMPORTING
                  e_kbetr        = lc_frete_ent-kbetr " Montante/porcentagem de condição no caso de não haver escala
                  e_konwa        = lc_frete_ent-konwa " Unidade de condição (moeda ou porcentagem)
                  e_krech        = lc_frete_ent-krech " Regra de cálculo de condição
                  e_lzonea       = e_lzonea
                  e_lzonez       = e_lzonez
                  e_route        = e_route
                ).

              CONCATENATE 'Saída:' e_parametros_motorista
                          ' --> Entrada:'
                          'Itinerário: ' e_route
                          'Agente de Frete:' lc_frete_ent-id_agent_frete
                          'Tipo de Transporte:' lc_frete_ent-shtyp
                          'Zona de Partida:' e_lzonea
                          'Zona de Chegada:' e_lzonez
                          'Contrato:' lc_frete_ent-add01
                          'Material:' lc_frete_ent-matnr
                          'Pedido Compras:' me->at_ekko-ebeln
                          'Viagem Id': i_viagem_id
                          'Placa Trator:' lc_frete_ent-ds_placa_trator
                     INTO e_parametros_motorista SEPARATED BY space.

            CATCH zcx_calc_frete INTO ex_calc_frete.

              CONCATENATE 'Saída:' e_parametros_motorista
                          ' --> Entrada:'
                          'Itinerário: ' e_route
                          'Agente de Frete:' lc_frete_ent-id_agent_frete
                          'Tipo de Transporte:' lc_frete_ent-shtyp
                          'Zona de Partida:' e_lzonea
                          'Zona de Chegada:' e_lzonez
                          'Contrato:' lc_frete_ent-add01
                          'Material:' lc_frete_ent-matnr
                          'Pedido Compras:' me->at_ekko-ebeln
                          'Viagem Id': i_viagem_id
                          'Placa Trator:' lc_frete_ent-ds_placa_trator
                     INTO e_parametros_motorista SEPARATED BY space.

              RAISE EXCEPTION TYPE zcx_calc_frete
                EXPORTING
                  textid = VALUE #( msgid = ex_calc_frete->msgid
                                    msgno = ex_calc_frete->msgno
                                    attr1 = CONV #( ex_calc_frete->msgv1 )
                                    attr2 = CONV #( ex_calc_frete->msgv2 )
                                    attr3 = CONV #( ex_calc_frete->msgv3 )
                                    attr4 = CONV #( ex_calc_frete->msgv4 ) )
                  msgid  = ex_calc_frete->msgid
                  msgno  = ex_calc_frete->msgno
                  msgty  = 'E'
                  msgv1  = ex_calc_frete->msgv1
                  msgv2  = ex_calc_frete->msgv2
                  msgv3  = ex_calc_frete->msgv3
                  msgv4  = ex_calc_frete->msgv4.

          ENDTRY.

        ELSE.
          "Erro, não informado parceiro SP na Ordem de Venda/Pedido de Compra
          RAISE EXCEPTION TYPE zcx_pedido_compra
            EXPORTING
              textid = VALUE #( msgid  = zcl_pedido_compra=>zcx_ordem_agente_frete-msgid
                                msgno  = zcl_pedido_compra=>zcx_ordem_agente_frete-msgno
                                attr1  = me->at_ekko-ebeln )
              msgty  = 'E'
              msgid  = zcl_pedido_compra=>zcx_ordem_agente_frete-msgid
              msgno  = zcl_pedido_compra=>zcx_ordem_agente_frete-msgno
              msgv1  = CONV #( me->at_ekko-ebeln ).
        ENDIF.
      ELSE.
        lc_frete_ent-kbetr = 0.
      ENDIF.

      IF ( lc_frete-kbetr + lc_frete_ent-kbetr ) NE i_valor_frete_motorista.

        WRITE i_valor_frete_motorista TO lc_texto_valor.
        CONDENSE lc_texto_valor NO-GAPS.
        RAISE EXCEPTION TYPE zcx_pedido_compra
          EXPORTING
            textid = VALUE #( msgid  = zcl_pedido_compra=>zcx_preco_frete-msgid
                              msgno  = zcl_pedido_compra=>zcx_preco_frete-msgno
                              attr1  = lc_texto_valor )
            msgty  = 'E'
            msgid  = zcl_pedido_compra=>zcx_preco_frete-msgid
            msgno  = zcl_pedido_compra=>zcx_preco_frete-msgno
            msgv1  = CONV #( lc_texto_valor ).
      ELSE.
        e_valor_frete_motorista = lc_frete-kbetr.
        e_valor_frete_entrada   = lc_frete_ent-kbetr.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD GET_INSTANCE.

    IF AT_PEDIDO_COMPRA IS NOT BOUND.
      CREATE OBJECT AT_PEDIDO_COMPRA.
    ENDIF.

    R_INSTANCIA = AT_PEDIDO_COMPRA.

  ENDMETHOD.


  METHOD get_item_pc.

    r_instancia = me.

    e_ekpo = me->at_ekpo.
    e_ekpv = me->at_ekpv.
    e_eket = me->at_eket.

  ENDMETHOD.


  METHOD get_parceiros.

    r_instancia = me.

    e_ekpa = me->at_ekpa.

  ENDMETHOD.


  METHOD GET_PEDIDO.

    CLEAR: R_EKKO.

    SELECT SINGLE * INTO R_EKKO
      FROM EKKO
     WHERE EBELN EQ I_EBELN.

    IF SY-SUBRC IS NOT INITIAL.
      "003  Pedido &1 não Encontrado!
      MESSAGE E003 WITH I_EBELN RAISING NAO_ACHOU_PEDIDO.
    ENDIF.

  ENDMETHOD.


  METHOD get_pedidos_aprovacao.

    DATA: items          TYPE TABLE OF swwwlhead,
          leading        TYPE swr_obj_2,
          header         TYPE bapiekkol,
          texts          TYPE TABLE OF bapiekkotx,
          wa_pedidos     TYPE zde_pedidos_aprovacao,
          lc_data        TYPE c LENGTH 10,
          lc_valor       TYPE c LENGTH 14,
*---> 04/07/2023 - Migração S4 - EO
          wa_header      TYPE bapimepoheader,
          lt_text_header TYPE TABLE OF bapimepotextheader.
*<--- 04/07/2023 - Migração S4 - EO

    CLEAR r_pedidos.

    DATA(lc_user) = sy-uname.

    CALL FUNCTION 'RH_INBOX_VIEW_CREATE'
      EXPORTING
*       SEARCH_DATE       = SY-DATUM
*       READ_OBJECT_TEXT  =
*       NO_WI_SELECTION   =
*       NO_HEADER_SELECTION       =
        inbox_user        = lc_user
*       IV_DO_COMMIT      = 'X'
      TABLES
*       INBOX_VIEW        =
        wi_head           = items
*       WI_STATUS         =
*       TASK_FILTER       =
      EXCEPTIONS
        no_active_plvar   = 1
        no_tasks_found    = 2
        user_not_defined  = 3
        no_workitem_found = 4
        OTHERS            = 5.

    IF sy-subrc IS NOT INITIAL.
      zcl_pedido_compra=>gera_erro_geral( ).
    ENDIF.

    "//Delete what isn't purchase order
    DELETE items WHERE wi_rh_task NE 'TS20000166'.

    LOOP AT items INTO DATA(_item).

      CLEAR wa_pedidos.
      wa_pedidos-wi_id = _item-wi_id.
      wa_pedidos-wi_cd = _item-wi_cd.
      wa_pedidos-wi_ct = _item-wi_ct.

      "//Get purchase order number
      CALL FUNCTION 'SAP_WAPI_GET_OBJECTS'
        EXPORTING
          workitem_id      = _item-wi_id
        IMPORTING
          leading_object_2 = leading.

*---> 04/07/2023 - Migração S4 - EO
*      CALL FUNCTION 'BAPI_PO_GETDETAIL' "#EC CI_USAGE_OK[2438131]
*        EXPORTING
*          purchaseorder   = CONV char10( leading-instid )
*          header_texts    = abap_true
*        IMPORTING
*          po_header       = header
*        TABLES
*          po_header_texts = texts.

      CALL FUNCTION 'BAPI_PO_GETDETAIL1' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          purchaseorder = CONV ebeln( leading-instid )
          header_text   = abap_true
        IMPORTING
          poheader      = wa_header
        TABLES
          potextheader  = lt_text_header.
*<--- 04/07/2023 - Migração S4 - EO

*---> 04/07/2023 - Migração S4 - EO
*      LOOP AT texts INTO DATA(_text).
      LOOP AT lt_text_header INTO DATA(_text).
*<--- 04/07/2023 - Migração S4 - EO
        IF wa_pedidos-description IS INITIAL.
          wa_pedidos-description = _text-text_line.
        ELSE.
          CONCATENATE wa_pedidos-description _text-text_line INTO wa_pedidos-description SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      SHIFT leading-instid LEFT DELETING LEADING '0'.

      SELECT SUM( brtwr )
        FROM ekpo
        INTO @wa_pedidos-brtwr
       WHERE ebeln EQ @leading-instid.

      WRITE _item-wi_cd TO lc_data DD/MM/YYYY.
      wa_pedidos-wi_cd_txt = lc_data.

      WRITE wa_pedidos-brtwr TO lc_valor.
      CONDENSE lc_valor NO-GAPS.
      wa_pedidos-brtwr_txt = lc_valor.

*---> 04/07/2023 - Migração S4 - EO
*      wa_pedidos-vendor_id   = header-vendor.
*      wa_pedidos-vendor_name = header-vend_name.
*      wa_pedidos-ebeln       = header-po_number.
*      wa_pedidos-waers       = header-currency.
      SELECT SINGLE name1
        FROM lfa1
        INTO @DATA(lv_name)
        WHERE lifnr = @wa_header-vendor.

      wa_pedidos = VALUE #( BASE wa_pedidos
                            vendor_id   = wa_header-vendor
                            vendor_name = lv_name
                            ebeln       = wa_header-po_number
                            waers       = wa_header-currency ).
*<--- 04/07/2023 - Migração S4 - EO

      APPEND wa_pedidos TO r_pedidos.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_PEDIDO_COMPRA_CHAVE_E.

    CLEAR: R_EKPO.

    CALL FUNCTION 'Z_MM_GET_PEDIDO_COMPRA_PSQ'
      EXPORTING
        I_LIFNR               = I_LIFNR
        I_BUKRS               = I_BUKRS
        I_WERKS               = I_WERKS
        I_MATNR               = I_MATNR
        I_CHARG               = I_CHARG
        I_LGORT               = I_LGORT
        I_EBELN               = I_EBELN
        I_EBELP               = I_EBELP
        I_BSTYP               = I_BSTYP
        I_BSART               = I_BSART
        I_MENGE_GE            = I_MENGE_GE
        I_MEINS               = I_MEINS
        I_ZMMT0075            = I_ZMMT0075
        I_ABRIR_TELA          = I_ABRIR_TELA
        I_MATNR_T             = I_MATNR_T
        I_FILTRO_1            = I_FILTRO_1
      IMPORTING
        R_EKPO                = R_EKPO
        R_SALDO_ITEM          = R_SALDO_ITEM
        E_EKPO_T              = E_EKPO_T
      EXCEPTIONS
        NAO_ENCONTRADO_PEDIDO = 1
        OTHERS                = 2.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING NAO_ENCONTRADO_PEDIDO.
    ENDIF.

  ENDMETHOD.


  METHOD GET_PEDIDO_ITENS.

    DATA: REBELP TYPE RANGE OF EBELP,
          WEBELP LIKE LINE OF REBELP.

    CLEAR: E_EKKO, E_EKPO_T.

    IF I_EBELP IS NOT INITIAL.
      WEBELP-SIGN   = 'I'.
      WEBELP-OPTION = 'EQ'.
      WEBELP-LOW    = I_EBELP.
      WEBELP-HIGH   = I_EBELP.
      APPEND WEBELP TO REBELP.
    ENDIF.

    CALL METHOD ZCL_PEDIDO_COMPRA=>GET_PEDIDO
      EXPORTING
        I_EBELN          = I_EBELN
      RECEIVING
        R_EKKO           = E_EKKO
      EXCEPTIONS
        NAO_ACHOU_PEDIDO = 1
        OTHERS           = 2.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING NAO_ACHOU_PEDIDO.
    ENDIF.

    SELECT * INTO TABLE @DATA(IT_EKPO)
      FROM EKPO
     WHERE EBELN EQ @I_EBELN
       AND EBELP IN @REBELP.

    IF I_EBELP IS NOT INITIAL AND SY-SUBRC IS NOT INITIAL.
      MESSAGE E013 WITH I_EBELN RAISING NAO_ACHOU_PEDIDO.
    ENDIF.

    MOVE IT_EKPO[] TO E_EKPO_T.

  ENDMETHOD.


  METHOD get_pedido_pc.

    r_instancia = me.

    e_pedido_compra = me->at_ekko.

  ENDMETHOD.


  METHOD GET_SALDO_PEDIDO_ITEM.

    DATA: RG_EBELP TYPE RANGE OF EBELP.

    R_SALDO = 0.

    IF I_EBELP IS NOT INITIAL.
      RG_EBELP = VALUE #( OPTION = 'EQ' SIGN = 'I'  ( LOW = I_EBELP HIGH = I_EBELP ) ).
    ENDIF.


    SELECT O~EBELN, O~EBELP, O~MATNR, O~MEINS, O~MENGE AS MENGE_EKPO, K~MENGE
      INTO TABLE @DATA(IT_REGISTROS)
      FROM EKPO AS O
      INNER JOIN EKBE AS K ON K~EBELN EQ O~EBELN AND K~EBELP EQ O~EBELP
     WHERE O~EBELN      EQ @I_EBELN
       AND O~EBELP      IN @RG_EBELP
       AND K~VGABE      EQ '1'
       AND K~SHKZG      EQ 'S'
       AND NOT EXISTS ( SELECT * FROM MSEG AS M WHERE M~SJAHR EQ K~GJAHR AND M~SMBLN EQ K~BELNR AND M~SMBLP EQ K~BUZEI ).

    CHECK SY-SUBRC IS INITIAL.

    "Busca Quantidade dos Itens do Pedido
    DATA(IT_ITENS_PED) = IT_REGISTROS[].
    SORT IT_ITENS_PED BY EBELN EBELP.
    DELETE ADJACENT DUPLICATES FROM IT_ITENS_PED COMPARING EBELN EBELP.
    LOOP AT IT_ITENS_PED INTO DATA(WA_ITENS_PED).
      ADD WA_ITENS_PED-MENGE_EKPO TO R_SALDO.
    ENDLOOP.

    "Busca Materiais
    DATA(IT_MATERIAL) = IT_REGISTROS[].
    SORT IT_MATERIAL BY MATNR.
    DELETE ADJACENT DUPLICATES FROM IT_MATERIAL COMPARING MATNR.

    SELECT * INTO TABLE @DATA(IT_MARA)
      FROM MARA
       FOR ALL ENTRIES IN @IT_MATERIAL
     WHERE MATNR EQ @IT_MATERIAL-MATNR.

    LOOP AT IT_MARA INTO DATA(WA_MARA).

      LOOP AT IT_REGISTROS INTO DATA(WA_REGISTRO) WHERE MATNR EQ WA_MARA-MATNR.

        IF WA_REGISTRO-MEINS NE WA_MARA-MEINS.
          CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
            EXPORTING
              I_MATNR              = WA_REGISTRO-MATNR
              I_IN_ME              = WA_REGISTRO-MEINS
              I_OUT_ME             = WA_MARA-MEINS
              I_MENGE              = WA_REGISTRO-MENGE
            IMPORTING
              E_MENGE              = WA_REGISTRO-MENGE
            EXCEPTIONS
              ERROR_IN_APPLICATION = 1
              ERROR                = 2
              OTHERS               = 3.

          IF SY-SUBRC IS NOT INITIAL.
            RAISE EXCEPTION TYPE ZCX_PEDIDO_COMPRA
              EXPORTING
                TEXTID = VALUE #( MSGID = ZCX_PEDIDO_COMPRA=>ZCX_PEDIDO_ERRO_CONV_UNID-MSGID
                                  MSGNO = ZCX_PEDIDO_COMPRA=>ZCX_PEDIDO_ERRO_CONV_UNID-MSGNO
                                  ATTR1 = CONV #( WA_REGISTRO-MATNR )
                                  ATTR2 = CONV #( WA_REGISTRO-MEINS )
                                  ATTR3 = CONV #( WA_MARA-MEINS ) )
                MSGID  = ZCX_PEDIDO_COMPRA=>ZCX_PEDIDO_ERRO_CONV_UNID-MSGID
                MSGNO  = ZCX_PEDIDO_COMPRA=>ZCX_PEDIDO_ERRO_CONV_UNID-MSGNO
                MSGTY  = 'E'
                MSGV1  = CONV #( WA_REGISTRO-MATNR )
                MSGV2  = CONV #( WA_REGISTRO-MEINS )
                MSGV3  = CONV #( WA_MARA-MEINS ).
          ENDIF.
        ENDIF.

        IF WA_REGISTRO-MENGE GT R_SALDO.
          R_SALDO = 0.
        ELSE.
          SUBTRACT WA_REGISTRO-MENGE FROM R_SALDO.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_TIPO_FRETE.

    R_INSTANCIA = ME.

    READ TABLE ME->EKPO_T INDEX 1 INTO DATA(WA_EKPO).

    DATA(LC_ICOTERMO) = WA_EKPO-INCO1.
    IF LC_ICOTERMO IS INITIAL.
      LC_ICOTERMO = ME->EKKO-INCO1.
    ENDIF.

    CASE LC_ICOTERMO.
      WHEN SPACE.
      WHEN ZIF_CARGA=>ST_TP_FRETE_EXW.
      WHEN ZIF_CARGA=>ST_TP_FRETE_FCA.
      WHEN ZIF_CARGA=>ST_TP_FRETE_FAS.
      WHEN ZIF_CARGA=>ST_TP_FRETE_FOB.
      WHEN ZIF_CARGA=>ST_TP_FRETE_CFR.
      WHEN ZIF_CARGA=>ST_TP_FRETE_CIF.
      WHEN ZIF_CARGA=>ST_TP_FRETE_CPT.
      WHEN ZIF_CARGA=>ST_TP_FRETE_CIP.
      WHEN ZIF_CARGA=>ST_TP_FRETE_DAF.
      WHEN ZIF_CARGA=>ST_TP_FRETE_DES.
      WHEN ZIF_CARGA=>ST_TP_FRETE_DEQ.
      WHEN ZIF_CARGA=>ST_TP_FRETE_DDU.
      WHEN ZIF_CARGA=>ST_TP_FRETE_DDP.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE ZCX_PEDIDO_COMPRA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_PEDIDO_COMPRA=>ZCX_PEDIDO_TIPO_FRETE_ERR-MSGID MSGNO = ZCX_PEDIDO_COMPRA=>ZCX_PEDIDO_TIPO_FRETE_ERR-MSGNO )
            MSGID  = ZCX_PEDIDO_COMPRA=>ZCX_PEDIDO_TIPO_FRETE_ERR-MSGID
            MSGNO  = ZCX_PEDIDO_COMPRA=>ZCX_PEDIDO_TIPO_FRETE_ERR-MSGNO
            MSGTY  = 'E'.
    ENDCASE.

    E_TIPO_FRETE = LC_ICOTERMO.

  ENDMETHOD.


  METHOD get_tipo_transporte.

    r_instancia = me.

*   IF ( 'ZUB' CS me->at_ekko-bsart ) AND ( me->at_ekko-bsart IS NOT INITIAL ).
*      READ TABLE me->at_ekpa INTO DATA(wa_ekpa) WITH KEY parvw = 'LR'.
*      IF sy-subrc IS INITIAL.
*        SELECT SINGLE * INTO @DATA(wa_kna1_lr)
*          FROM kna1
*         WHERE kunnr EQ @wa_ekpa-lifn2.
*      ENDIF.
*
*      READ TABLE me->at_ekpa INTO wa_ekpa WITH KEY parvw = 'Z1'.
*      IF sy-subrc IS INITIAL.
*        SELECT SINGLE * INTO @DATA(wa_lfa1_z1)
*          FROM lfa1
*         WHERE lifnr EQ @wa_ekpa-lifn2.
*      ENDIF.
*
*      "Determinar Agente Frete
*      READ TABLE me->at_ekpa INTO wa_ekpa  WITH KEY parvw = 'SP'.
*      IF sy-subrc IS INITIAL.
*        SELECT SINGLE * INTO @DATA(wa_lfa1_sp)
*          FROM lfa1
*         WHERE lifnr EQ @wa_ekpa-lifn2.
*      ENDIF.
*   ENDIF.

    SELECT t~shtyp, k~vsart, t~bsart, t~auart, k~laufk
      INTO TABLE @DATA(it_zsdt0011)
      FROM zsdt0011 AS t
     INNER JOIN tvtk AS k ON k~shtyp EQ t~shtyp
     WHERE t~tp_movimento EQ 'S'
       AND t~bsart        EQ @me->at_ekko-bsart
       AND k~vsart        EQ @i_vsart.

    "Percurso Direto
    READ TABLE it_zsdt0011 INTO DATA(wa_zsdt0011) WITH KEY laufk = '4'.
    e_tipo_transporte = wa_zsdt0011-shtyp.

*    IF sy-subrc IS INITIAL.
*      IF ( wa_kna1_lr IS NOT INITIAL AND wa_lfa1_z1 IS NOT INITIAL ) AND
*         ( ( wa_kna1_lr-stcd1 IS NOT INITIAL AND wa_kna1_lr-stcd1 NE wa_lfa1_z1-stcd1 ) OR
*           ( wa_kna1_lr-stcd2 IS NOT INITIAL AND wa_kna1_lr-stcd2 NE wa_lfa1_z1-stcd2 ) ).
*        "Percurso com Transbordo
*        READ TABLE it_zsdt0011 INTO DATA(wa_zsdt0011) WITH KEY laufk = '1'.
*        e_tipo_transporte = wa_zsdt0011-shtyp.
*      ELSE.
*        "Percurso Direto
*        READ TABLE it_zsdt0011 INTO wa_zsdt0011 WITH KEY laufk = '4'.
*        e_tipo_transporte = wa_zsdt0011-shtyp.
*      ENDIF.
*   ENDIF.

  ENDMETHOD.


  METHOD GET_VALIDA_PEDIDO_NOTA.

    DATA: LC_NFE                TYPE REF TO ZCL_NFE_INBOUND,
          WA_NFE                TYPE ZIB_NFE_DIST_TER,
          LC_VALOR_TOTAL_PEDIDO TYPE BWERT,
          LC_VALOR_ITEM_NOTA    TYPE BWERT,
          LC_VALOR_ITEM_ICMS    TYPE BWERT,
          LC_VALOR_ITEM_PIS     TYPE BWERT,
          LC_VALOR_ITEM_COFINS  TYPE BWERT,
          LC_CENTRO_REAL        TYPE WERKS_D,
          LX_TEXTO              TYPE C LENGTH 15.

    "Verifica se Pedido de Compra Existe
    ZCL_PEDIDO_COMPRA=>GET_PEDIDO(
      EXPORTING
        I_EBELN          = I_EBELN    " Nº do documento de compras
      RECEIVING
        R_EKKO           = DATA(LC_EKKO)   " Cabeçalho do documento de compra
      EXCEPTIONS
        NAO_ACHOU_PEDIDO = 1
        OTHERS           = 2
    ).

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_PEDIDO_COMPRA_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = SY-MSGTY
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    CREATE OBJECT LC_NFE.

    "Verifica se o Documento Fiscal Existe
    LC_NFE->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = I_CHAVE_NFE ).

    LC_NFE->SET_INFO_SAP( ).

    LC_NFE->ZIF_CADASTRO~GET_REGISTRO( IMPORTING E_REGISTRO = WA_NFE ).

    "Documento Fiscal Está Escriturado
    IF WA_NFE-DOCNUM_NFE IS NOT INITIAL AND I_VERF_EXISTE EQ ABAP_TRUE.
      LC_NFE->FREE( ).
      CLEAR LC_NFE.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_ESCRITURADA-MSGNO
                            MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_ESCRITURADA-MSGID )
          MSGTY  = 'E'
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_ESCRITURADA-MSGNO
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_ESCRITURADA-MSGID.
    ENDIF.

    IF WA_NFE-P_EMISSOR IS INITIAL.

      IF WA_NFE-FORNE_CNPJ IS NOT INITIAL.
        LX_TEXTO = WA_NFE-FORNE_CNPJ.
      ELSE.
        LX_TEXTO = WA_NFE-FORNE_CPF.
      ENDIF.

      LC_NFE->FREE( ).
      CLEAR LC_NFE.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_FORNECEDOR_NAO_LOCALIZADO-MSGNO
                            MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_FORNECEDOR_NAO_LOCALIZADO-MSGID
                            ATTR1  = CONV #( LX_TEXTO )
                            ATTR2  = CONV #( WA_NFE-FORNE_IE ) )
          MSGTY  = 'E'
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_FORNECEDOR_NAO_LOCALIZADO-MSGNO
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_FORNECEDOR_NAO_LOCALIZADO-MSGID
          MSGV1  = CONV #( LX_TEXTO )
          MSGV2  = CONV #( WA_NFE-FORNE_IE ).
    ENDIF.

    IF WA_NFE-E_TOMADORA IS INITIAL OR WA_NFE-F_TOMADORA IS INITIAL.
      LC_NFE->FREE( ).
      CLEAR LC_NFE.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_TOMADOR_NAO_LOCALIZADO-MSGNO
                            MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_TOMADOR_NAO_LOCALIZADO-MSGID
                            ATTR1  = CONV #( WA_NFE-DESTINO_CNPJ )
                            ATTR2  = CONV #( WA_NFE-DESTINO_IE ) )
          MSGTY  = 'E'
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_TOMADOR_NAO_LOCALIZADO-MSGNO
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_TOMADOR_NAO_LOCALIZADO-MSGID
          MSGV1  = CONV #( WA_NFE-DESTINO_CNPJ )
          MSGV2  = CONV #( WA_NFE-DESTINO_IE ).
    ENDIF.

    SELECT SINGLE * INTO @DATA(WA_MASSEKKO)
      FROM MASSEKKO
     WHERE EBELN EQ @I_EBELN.

    "Empresa do Pedido não está igual a Empresa Tomadora"
    IF WA_MASSEKKO-BUKRS NE WA_NFE-E_TOMADORA.
      LC_NFE->FREE( ).
      CLEAR LC_NFE.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_EMPRESA-MSGNO
                            MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_EMPRESA-MSGID )
          MSGTY  = 'E'
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_EMPRESA-MSGNO
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_EMPRESA-MSGID.
    ENDIF.

    "Emissor do Documento Fiscal não é fornecedor e não é fornecedor da Remessa
    IF NOT ( WA_MASSEKKO-LIFNR EQ WA_NFE-P_EMISSOR OR WA_MASSEKKO-LLIEF EQ WA_NFE-P_EMISSOR ).
      LC_NFE->FREE( ).
      CLEAR LC_NFE.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_FORNECEDOR-MSGNO
                            MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_FORNECEDOR-MSGID )
          MSGTY  = 'E'
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_FORNECEDOR-MSGNO
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_FORNECEDOR-MSGID.
    ENDIF.

    "Emissor do Documento Fiscal não é fornecedor e não é fornecedor da Remessa
    IF WA_MASSEKKO-FRGRL EQ ABAP_TRUE.
      LC_NFE->FREE( ).
      CLEAR LC_NFE.
      RAISE EXCEPTION TYPE ZCX_PEDIDO_COMPRA_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGNO  = ZCX_PEDIDO_COMPRA_EXCEPTION=>ZCX_PEDIDO_NAO_LIBERADO-MSGNO
                            MSGID  = ZCX_PEDIDO_COMPRA_EXCEPTION=>ZCX_PEDIDO_NAO_LIBERADO-MSGID )
          MSGTY  = 'E'
          MSGNO  = ZCX_PEDIDO_COMPRA_EXCEPTION=>ZCX_PEDIDO_NAO_LIBERADO-MSGNO
          MSGID  = ZCX_PEDIDO_COMPRA_EXCEPTION=>ZCX_PEDIDO_NAO_LIBERADO-MSGID.
    ENDIF.

    "Busca Item de Pedido de Compra
    SELECT * INTO TABLE @DATA(IT_MASSEKPO)
      FROM MASSEKPO AS P
     WHERE EBELN EQ @WA_MASSEKKO-EBELN.

    DATA(CK_ACHOU_FILIAL_TOMADORA) = ABAP_FALSE.

    LOOP AT IT_MASSEKPO INTO DATA(WA_MASSEKPO).

      CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
        EXPORTING
          CENTRO               = WA_MASSEKPO-WERKS
        IMPORTING
          CENTRO_REAL          = LC_CENTRO_REAL
        EXCEPTIONS
          INFORMAR_CENTRO      = 1
          NAO_CENTRO_R_VIRTUAL = 2
          INFORMAR_CENTRO_OUT  = 3
          INFORMAR_CENTRO_V    = 4
          OTHERS               = 5.

      IF SY-SUBRC IS NOT INITIAL.
        LC_CENTRO_REAL = WA_MASSEKPO-WERKS.
      ENDIF.

      IF LC_CENTRO_REAL EQ WA_NFE-F_TOMADORA.
        CK_ACHOU_FILIAL_TOMADORA = ABAP_TRUE.
      ENDIF.

    ENDLOOP.

    "Filial do Pedido não está igual a Filial Tomadora"
    IF CK_ACHOU_FILIAL_TOMADORA NE ABAP_TRUE.
      LC_NFE->FREE( ).
      CLEAR LC_NFE.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_FILIAL-MSGNO
                            MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_FILIAL-MSGID )
          MSGTY  = 'E'
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_FILIAL-MSGNO
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_FILIAL-MSGID.
    ENDIF.

    "Verificar Valores do Pedido com Nota Fiscal
    "Somente Quando for Pedido Expresso, pois somente tem uma nota fiscal e todos os itens da nota fiscal estão no pedido
    "e Existe somente uma entrega.

    IF WA_MASSEKKO-BSART EQ ZCL_PEDIDO_COMPRA=>ST_BSART_PCE.

      "Somar Valor Total dos Itens
      LC_VALOR_TOTAL_PEDIDO = 0.
      LOOP AT IT_MASSEKPO INTO WA_MASSEKPO.
        IF WA_MASSEKPO-PSTYP NE '9'.
          ADD WA_MASSEKPO-NETWR TO LC_VALOR_TOTAL_PEDIDO.
        ENDIF.
      ENDLOOP.

      DATA(R_NFE_INBOUND) = LC_NFE->GET_INFO_NOTA( ).
      LC_VALOR_ITEM_NOTA   = 0.
      LC_VALOR_ITEM_ICMS   = 0.
      LC_VALOR_ITEM_PIS    = 0.
      LC_VALOR_ITEM_COFINS = 0.
      LOOP AT R_NFE_INBOUND-NFE_BASE-ITENS INTO DATA(WA_ITEM).
        ADD WA_ITEM-PROD_VLR_TOTAL_B TO LC_VALOR_ITEM_NOTA.
        ADD WA_ITEM-ICMS_VALOR       TO LC_VALOR_ITEM_ICMS.
        ADD WA_ITEM-PIS_VALOR        TO LC_VALOR_ITEM_PIS.
        ADD WA_ITEM-COF_VALOR        TO LC_VALOR_ITEM_COFINS.
      ENDLOOP.

      DATA(DIFERENCA) = LC_VALOR_TOTAL_PEDIDO - LC_VALOR_ITEM_NOTA.
      "DATA(DIFERENCA) = LC_VALOR_TOTAL_PEDIDO - ( LC_VALOR_ITEM_NOTA - LC_VALOR_ITEM_ICMS - LC_VALOR_ITEM_PIS - LC_VALOR_ITEM_COFINS ).
      IF ABS( DIFERENCA ) GT ( 10 / 100 ).

        WRITE DIFERENCA TO SY-MSGV1.
        CONDENSE SY-MSGV1 NO-GAPS.
        LC_NFE->FREE( ).
        CLEAR LC_NFE.
        RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
          EXPORTING
            TEXTID = VALUE #( MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_DIF_VALOR-MSGNO
                              MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_DIF_VALOR-MSGID
                              ATTR1  = CONV #( SY-MSGV1 ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_DIF_VALOR-MSGNO
            MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NFE_PEDIDO_DIF_VALOR-MSGID
            MSGV1  = SY-MSGV1.
      ENDIF.

    ENDIF.

    LC_NFE->FREE( ).
    CLEAR LC_NFE.

  ENDMETHOD.


  METHOD get_zonas_transporte.

    r_instancia = me.

    DATA: l_lifnr         TYPE lfa1-lifnr.

    CLEAR: e_lzonea,
           e_lzonez,
           e_lc_coleta,
           e_lc_entrega.

*---------------------------------------
* local coleta
*---------------------------------------
    READ TABLE me->at_ekpa INTO DATA(lc_pc) WITH KEY parvw = 'PR'.
    IF sy-subrc IS INITIAL.
      e_lc_coleta = lc_pc-lifn2.
      SELECT SINGLE lzone INTO @e_lzonea
        FROM lfa1
       WHERE lifnr EQ @lc_pc-lifn2.
    ENDIF.

*---------------------------------------
* local entrega
*---------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = me->at_ekpo-werks
      IMPORTING
        output = l_lifnr.

    e_lc_entrega = l_lifnr.
    SELECT SINGLE lzone INTO @e_lzonez
      FROM lfa1
     WHERE lifnr EQ @l_lifnr.

*    CASE i_frete_entrada.
*      WHEN abap_false.
*        "Local de Entrega
*        READ TABLE me->at_ekpa INTO DATA(lc_lr) WITH KEY parvw = 'LR'.
*        IF sy-subrc IS INITIAL.
*          e_lc_entrega = lc_lr-lifn2.
*          SELECT SINGLE lzone INTO @e_lzonez
*            FROM kna1
*           WHERE kunnr EQ @lc_lr-lifn2.
*        ENDIF.
*      WHEN abap_true.
*        "Filial de Entrega
*        READ TABLE me->at_ekpa INTO lc_lr WITH KEY parvw = 'AG'.
*        IF sy-subrc IS INITIAL.
*          e_lc_entrega = lc_lr-lifn2.
*          SELECT SINGLE lzone INTO @e_lzonez
*            FROM kna1
*           WHERE kunnr EQ @lc_lr-lifn2.
*        ENDIF.
*    ENDCASE.

    IF e_lzonea IS INITIAL.
      RAISE EXCEPTION TYPE zcx_pedido_compra
        EXPORTING
          textid = VALUE #( msgid  = zcl_pedido_compra=>zcx_ped_compra_zona_partida-msgid
                            msgno  = zcl_pedido_compra=>zcx_ped_compra_zona_partida-msgno
                            attr1  = CONV #( me->at_ekko-ebeln ) )
          msgty  = 'E'
          msgid  = zcl_pedido_compra=>zcx_ped_compra_zona_partida-msgid
          msgno  = zcl_pedido_compra=>zcx_ped_compra_zona_partida-msgno
          msgv1  = CONV #( me->at_ekko-ebeln ).
    ENDIF.

    IF e_lzonez IS INITIAL.
      RAISE EXCEPTION TYPE zcx_pedido_compra
        EXPORTING
          textid = VALUE #( msgid  = zcl_pedido_compra=>zcx_ped_compra_zona_chegada-msgid
                            msgno  = zcl_pedido_compra=>zcx_ped_compra_zona_chegada-msgno
                            attr1  = CONV #( me->at_ekko-ebeln ) )
          msgty  = 'E'
          msgid  = zcl_pedido_compra=>zcx_ped_compra_zona_partida-msgid
          msgno  = zcl_pedido_compra=>zcx_ped_compra_zona_partida-msgno
          msgv1  = CONV #( me->at_ekko-ebeln ).
    ENDIF.

  ENDMETHOD.


  METHOD set_criar_pedido_compra.

    DATA: it_zmm_po_zgr	      TYPE TABLE OF	zmmt_po_zgr,
          it_zmmt_po_item_zgr TYPE TABLE OF zmmt_po_item_zgr,
          wa_zmm_po_zgr	      TYPE zmmt_po_zgr,
          wa_zmmt_po_item_zgr TYPE zmmt_po_item_zgr,
          lc_lgort            TYPE lgort_d.

    CLEAR:
    it_zmm_po_zgr,
    wa_zmm_po_zgr.

    "1  Centro a Fixar
    "2  Centro Porto

    IF i_lgort IS INITIAL AND i_tp_centro IS INITIAL.
      RAISE EXCEPTION TYPE zcx_pedido_compra_exception
        EXPORTING
          textid = VALUE #( msgid = zcx_pedido_compra_exception=>zcx_deposito_centro-msgid
                            msgno = zcx_pedido_compra_exception=>zcx_deposito_centro-msgno )
          msgid  = zcx_pedido_compra_exception=>zcx_deposito_centro-msgid
          msgno  = zcx_pedido_compra_exception=>zcx_deposito_centro-msgno
          msgty  = 'E'.
    ENDIF.

    lc_lgort = i_lgort.

    CASE i_tp_centro.
      WHEN zcl_pedido_compra=>st_tp_centro_a_fixar.

        "Buscar Centro a Fixar
        SELECT SINGLE * INTO @DATA(wa_afixar)
          FROM zsdt_depara_cen
         WHERE vkorg             EQ @i_bukrs
           AND centro_real       EQ @i_werks
           AND tp_centro_virtual EQ @zcl_pedido_compra=>st_tp_centro_a_fixar.

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zcx_pedido_compra_exception
            EXPORTING
              textid    = VALUE #( msgid = zcx_pedido_compra_exception=>zcx_centro_a_fixar-msgid
                                   msgno = zcx_pedido_compra_exception=>zcx_centro_a_fixar-msgno
                                   attr1 = CONV #( i_werks )
                                   attr2 = 'ZSDT0036' )
              msgid     = zcx_pedido_compra_exception=>zcx_centro_a_fixar-msgid
              msgno     = zcx_pedido_compra_exception=>zcx_centro_a_fixar-msgno
              msgty     = 'E'
              msgv1     = CONV #( i_werks )
              msgv2     = 'ZSDT0036'
              transacao = 'ZSDT0036'.
        ENDIF.

        DATA(i_werks_pedido) = wa_afixar-centrov_1.

        IF lc_lgort IS INITIAL.

          SELECT SINGLE * INTO @DATA(wa_zmmt0017)
            FROM zmmt0017
           WHERE matnr          EQ @i_matnr
             AND centro_fixo    EQ @i_werks
             AND centro_a_fixar EQ @wa_afixar-centrov_1
             AND tp_produto     EQ @space
             AND eudr           eq @space. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940

          IF sy-subrc IS not INITIAL.
            "Centro &1 Centro a Fixar &2 e Material &3 sem Depósito! Transação &4!
            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid    = VALUE #( msgid = zcx_carga=>zcx_centro_a_fixar_deposito-msgid
                                     msgno = zcx_carga=>zcx_centro_a_fixar_deposito-msgno
                                     attr1 = CONV #( i_werks )
                                     attr2 = CONV #( wa_afixar-centrov_1 )
                                     attr3 = CONV #( i_matnr )
                                     attr4 = 'ZMM0017' )
                msgid     = zcx_carga=>zcx_centro_a_fixar_deposito-msgid
                msgno     = zcx_carga=>zcx_centro_a_fixar_deposito-msgno
                msgty     = 'E'
                msgv1     = CONV #( i_werks )
                msgv2     = CONV #( wa_afixar-centrov_1 )
                msgv3     = CONV #( i_matnr )
                msgv4     = 'ZMM0017'
                transacao = 'ZMM0017'.
          ENDIF.
          lc_lgort = wa_zmmt0017-lgort.
        ENDIF.

      WHEN zcl_pedido_compra=>st_tp_centro_porto.
        "Buscar Centro Porto
        SELECT SINGLE * INTO @wa_afixar
          FROM zsdt_depara_cen
         WHERE centro_real       EQ @i_werks
           AND tp_centro_virtual EQ @zcl_pedido_compra=>st_tp_centro_porto.

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zcx_pedido_compra_exception
            EXPORTING
              textid    = VALUE #( msgid = zcx_pedido_compra_exception=>zcx_centro_porto-msgid
                                   msgno = zcx_pedido_compra_exception=>zcx_centro_porto-msgno
                                   attr1 = CONV #( i_werks )
                                   attr2 = 'ZSDT0036' )
              msgid     = zcx_pedido_compra_exception=>zcx_centro_porto-msgid
              msgno     = zcx_pedido_compra_exception=>zcx_centro_porto-msgno
              msgty     = 'E'
              msgv1     = CONV #( i_werks )
              msgv2     = 'ZSDT0036'
              transacao = 'ZSDT0036'.
        ENDIF.

        i_werks_pedido = wa_afixar-centrov_1.

      WHEN zcl_pedido_compra=>st_tp_centro_real.

        i_werks_pedido = i_werks.

    ENDCASE.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZOBJKEY_M1'
      IMPORTING
        number                  = wa_zmm_po_zgr-obj_key
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_cadastro
        EXPORTING
          textid = VALUE #( msgid = sy-msgid
                            msgno = sy-msgno
                            attr1 = CONV #( sy-msgv1 )
                            attr2 = CONV #( sy-msgv2 )
                            attr3 = CONV #( sy-msgv3 )
                            attr4 = CONV #( sy-msgv4 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgty  = 'E'
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.

    DATA: lc_obj_key TYPE c LENGTH 9.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zmm_po_zgr-obj_key
      IMPORTING
        output = lc_obj_key.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lc_obj_key
      IMPORTING
        output = lc_obj_key.

    CONCATENATE 'PD' lc_obj_key INTO wa_zmm_po_zgr-obj_key.

    wa_zmm_po_zgr-bedat       = i_bedat.
    wa_zmm_po_zgr-ekorg       = i_ekorg.
    wa_zmm_po_zgr-ekgrp       = i_ekgrp.
    wa_zmm_po_zgr-waers       = i_waers.
    wa_zmm_po_zgr-bsart       = i_bsart.
    wa_zmm_po_zgr-zterm       = i_zterm.
    wa_zmm_po_zgr-lifnr       = i_lifnr.
    wa_zmm_po_zgr-bukrs       = i_bukrs.
    wa_zmm_po_zgr-lgort       = lc_lgort.
    wa_zmm_po_zgr-charg       = i_charg.
    wa_zmm_po_zgr-eindt       = i_eindt.
    wa_zmm_po_zgr-mwskz       = i_mwskz.
    wa_zmm_po_zgr-menge       = i_menge.
    wa_zmm_po_zgr-matnr       = i_matnr.
    wa_zmm_po_zgr-meins       = i_meins.
    wa_zmm_po_zgr-elikz       = i_elikz.
    wa_zmm_po_zgr-loekz       = i_loekz.
    wa_zmm_po_zgr-werks       = i_werks_pedido.
    wa_zmm_po_zgr-werks_d     = i_werks_d.
    wa_zmm_po_zgr-net_price   = i_net_price.
    wa_zmm_po_zgr-zst_atlz    = 'I'.
    wa_zmm_po_zgr-zid_atlz    = '8'.
    wa_zmm_po_zgr-collect_no  = i_collect_no.  "*-CS2022000332-#78064-07.06.2022-JT-inicio

    APPEND wa_zmm_po_zgr TO it_zmm_po_zgr.

    wa_zmmt_po_item_zgr-obj_key = wa_zmm_po_zgr-obj_key.
    wa_zmmt_po_item_zgr-matnr   = wa_zmm_po_zgr-matnr.
    wa_zmmt_po_item_zgr-menge   = wa_zmm_po_zgr-menge.
    APPEND wa_zmmt_po_item_zgr TO it_zmmt_po_item_zgr.

    MODIFY zmmt_po_zgr FROM TABLE it_zmm_po_zgr.
    MODIFY zmmt_po_item_zgr FROM TABLE it_zmmt_po_item_zgr.
    COMMIT WORK.

    DATA: number           TYPE tbtcjob-jobcount,
          name             TYPE tbtcjob-jobname,
          print_parameters TYPE pri_params.

    DATA(lc_user_job) = zcl_job=>get_user_job( ).

    CONCATENATE 'JOB_ENTRADA' wa_zmm_po_zgr-obj_key INTO name SEPARATED BY '_'.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = name
      IMPORTING
        jobcount         = number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc = 0.

      SUBMIT zmmr127 TO SAP-SPOOL SPOOL PARAMETERS print_parameters WITHOUT SPOOL DYNPRO VIA JOB name NUMBER number
        WITH pobjkey EQ wa_zmm_po_zgr-obj_key
        USER lc_user_job
         AND RETURN.

      IF sy-subrc = 0.
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = number
            jobname              = name
            strtimmed            = 'X'
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            OTHERS               = 8.
      ENDIF.
    ENDIF.

    "Aguardar execução do job
    zcl_job=>get_instance( )->set_key_job( i_jobname = name i_jobcount = number )->get_wait_job_exec( ).
    "BREAK-POINT.

  ENDMETHOD.


  METHOD SET_PEDIDO.

    R_INSTANCIA = ME.

    CLEAR: ME->EKKO, ME->EKPO_T[].

    SELECT SINGLE * INTO ME->EKKO
      FROM EKKO
     WHERE EBELN EQ I_EBELN.

    IF SY-SUBRC IS NOT INITIAL.
      "003  Pedido &1 não Encontrado!
      RAISE EXCEPTION TYPE ZCX_PEDIDO_COMPRA
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_PEDIDO_COMPRA=>ZCX_PEDIDO_NAO_ENCONTRADO-MSGID
                            MSGNO  = ZCX_PEDIDO_COMPRA=>ZCX_PEDIDO_NAO_ENCONTRADO-MSGNO
                            ATTR1  = I_EBELN )
          MSGTY  = 'E'
          MSGID  = ZCX_PEDIDO_COMPRA=>ZCX_PEDIDO_NAO_ENCONTRADO-MSGID
          MSGNO  = ZCX_PEDIDO_COMPRA=>ZCX_PEDIDO_NAO_ENCONTRADO-MSGNO
          MSGV1  = CONV #( I_EBELN ).
    ENDIF.

    SELECT * INTO TABLE ME->EKPO_T
      FROM EKPO
     WHERE EBELN EQ I_EBELN.

  ENDMETHOD.


  METHOD set_pedido_pc.

    r_instancia = me.

    CLEAR: me->at_ekko, me->at_ekpo, me->at_ekpa.

    SELECT SINGLE * INTO me->at_ekko
      FROM ekko
     WHERE ebeln EQ i_ebeln.

    SELECT SINGLE * INTO me->at_ekpo
      FROM ekpo
     WHERE ebeln EQ i_ebeln.

    SELECT SINGLE * INTO me->at_eket
      FROM eket
     WHERE ebeln EQ i_ebeln.

    SELECT * INTO TABLE me->at_ekpa
      FROM ekpa
     WHERE ebeln EQ i_ebeln.

    SELECT SINGLE * INTO me->at_ekpv
      FROM ekpv
     WHERE ebeln EQ i_ebeln.

  ENDMETHOD.


  METHOD SHOW_PEDIDO.

    IF I_EBELN IS NOT INITIAL.
      SET PARAMETER ID 'BES' FIELD I_EBELN.
      CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.
    ENDIF.

  ENDMETHOD.


  METHOD VERIF_BLOQ_PEDIDO_WAIT.

    DATA: I_GNAME TYPE SEQG3-GNAME,
          I_GARG  TYPE SEQG3-GARG,
          IT_ENQ  TYPE TABLE OF SEQG3.

    DATA: I_TEXTO     TYPE STRING,
          I_TEMPO     TYPE I,
          LC_TEXTO    TYPE STRING,
          LC_NUMERO   TYPE CHAR30,
          I_TEXT_WAIT TYPE STRING.

    I_GNAME = 'EKKO'.
    CONCATENATE SY-MANDT I_EBELN INTO I_GARG.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        GCLIENT               = SY-MANDT
        GNAME                 = I_GNAME
        GARG                  = I_GARG
        GUNAME                = SPACE
      TABLES
        ENQ                   = IT_ENQ
      EXCEPTIONS
        COMMUNICATION_FAILURE = 1
        SYSTEM_FAILURE        = 2
        OTHERS                = 3.

    IF SY-SUBRC IS INITIAL AND IT_ENQ[] IS NOT INITIAL.
      READ TABLE IT_ENQ INTO DATA(WA_ENQ) INDEX 1.
      CONCATENATE 'Aguarde, Pedido:' I_EBELN 'bloqueado!' WA_ENQ-GUNAME WA_ENQ-GTCODE INTO I_TEXT_WAIT SEPARATED BY SPACE.
      IF SY-BATCH NE ABAP_TRUE.
        LC_NUMERO = 0.
        LC_TEXTO  = I_TEXT_WAIT && ' Segundos: ' && LC_NUMERO.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            PERCENTAGE = 0
            TEXT       = LC_TEXTO.
      ELSE.
        MESSAGE I_TEXT_WAIT TYPE 'S'.
      ENDIF.
    ENDIF.

    CHECK IT_ENQ[] IS NOT INITIAL.

    I_TEMPO = 0.

    WHILE IT_ENQ[] IS NOT INITIAL.

      WAIT UP TO 1 SECONDS.
      ADD 1 TO I_TEMPO.

      CALL FUNCTION 'TH_REDISPATCH'.

      WRITE I_TEMPO TO LC_NUMERO.
      CONDENSE LC_NUMERO NO-GAPS.

      READ TABLE IT_ENQ INTO WA_ENQ INDEX 1.
      CONCATENATE 'Aguarde, Pedido:' I_EBELN 'bloqueado!' WA_ENQ-GUNAME WA_ENQ-GTCODE INTO I_TEXT_WAIT SEPARATED BY SPACE.
      IF SY-BATCH NE ABAP_TRUE.
        LC_TEXTO = I_TEXT_WAIT && ' Segundos: ' && LC_NUMERO.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            PERCENTAGE = 0
            TEXT       = LC_TEXTO.
      ELSE.
        MESSAGE I_TEXT_WAIT TYPE 'S'.
      ENDIF.

      CLEAR: IT_ENQ[].
      CALL FUNCTION 'ENQUEUE_READ'
        EXPORTING
          GCLIENT               = SY-MANDT
          GNAME                 = I_GNAME
          GARG                  = I_GARG
        TABLES
          ENQ                   = IT_ENQ
        EXCEPTIONS
          COMMUNICATION_FAILURE = 1
          SYSTEM_FAILURE        = 2
          OTHERS                = 3.

    ENDWHILE.

  ENDMETHOD.


  METHOD zif_pesquisa~pesquisar.

    DATA: lc_filtro TYPE zde_filtro_pedido_compra,
          it_ekko   TYPE zde_ekko_t.

    lc_filtro = i_filtros.

    SELECT k~* INTO TABLE @DATA(it_tab)
      FROM ekko AS k
     INNER JOIN ekpo AS p ON p~ebeln EQ k~ebeln
     INNER JOIN eket AS g ON g~ebeln EQ p~ebeln AND g~ebelp EQ p~ebelp
     WHERE k~bukrs IN @lc_filtro-ibukrs                     "= '0001'
       AND k~bstyp IN @lc_filtro-ibstyp "= 'F'
       AND k~bsart IN @lc_filtro-ibsart "= 'ZGR'
       AND k~ekorg IN @lc_filtro-iekorg "= 'OC01'
       AND k~ekgrp IN @lc_filtro-iekgrp "= 'G01'
       AND k~ebeln IN @lc_filtro-iebeln "= '4000051996'
       AND k~lifnr IN @lc_filtro-ilifnr "= '0000108635'
       AND k~frgrl IN @lc_filtro-ifrgrl "= ' '
       AND p~matnr IN @lc_filtro-imatnr "= '000000000000119892'
       AND p~werks IN @lc_filtro-iwerks "= 'AF11'
       AND p~ebelp IN @lc_filtro-iebelp                     "= '00010'
       AND g~charg IN @lc_filtro-icharg                     "= '2017'
       AND p~bstae IN @lc_filtro-ibstae                     "= '0002'
       AND p~mwskz IN @lc_filtro-imwskz
       AND p~lgort IN @lc_filtro-ilgort
       AND k~submi IN @lc_filtro-isubmi.  "*-CS2022000332-#78064-07.06.2022-JT-inicio

    CHECK sy-subrc IS INITIAL.

    MOVE it_tab[] TO it_ekko[].
    e_registros = it_ekko.
    e_pesquisou = abap_true.

  ENDMETHOD.


  METHOD GET_SOLICITACAO.

    R_INSTANCIA = ME.

    SELECT SINGLE EBELN
      FROM ZMMT0196
      INTO I_PEDIDO
      WHERE NRO_SOL EQ E_SOLICITACAO.

    IF SY-SUBRC IS NOT INITIAL.
      "029  Solicitação $1 não Encontrado!
      RAISE EXCEPTION TYPE ZCX_PEDIDO_COMPRA
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_PEDIDO_COMPRA=>ZCX_SOLICITACAO_NAO_ENCONTRADO-MSGID
                            MSGNO  = ZCX_PEDIDO_COMPRA=>ZCX_SOLICITACAO_NAO_ENCONTRADO-MSGNO
                            ATTR1  = E_SOLICITACAO )
          MSGTY  = 'E'
          MSGID  = ZCX_PEDIDO_COMPRA=>ZCX_SOLICITACAO_NAO_ENCONTRADO-MSGID
          MSGNO  = ZCX_PEDIDO_COMPRA=>ZCX_SOLICITACAO_NAO_ENCONTRADO-MSGNO
          MSGV1  = CONV #( E_SOLICITACAO ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
