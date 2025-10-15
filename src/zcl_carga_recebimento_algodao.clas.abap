class ZCL_CARGA_RECEBIMENTO_ALGODAO definition
  public
  inheriting from ZCL_CARGA_RECEBIMENTO_V0001
  final
  create public .

public section.

  methods ZIF_CARGA~ADD_NOTA_FISCAL
    redefinition .
  methods ZIF_CARGA~ADD_ORDEM_VENDA
    redefinition .
  methods ZIF_CARGA~GET_INFO_ALV_APRESENTACAO
    redefinition .
  methods ZIF_CARGA~GET_VALIDAR_NOTA_FISCAL
    redefinition .
  methods ZIF_CARGA~GRAVAR_REGISTRO
    redefinition .
  methods ZIF_CARGA~SET_AJUSTAR_RAT_ORDEM_VENDA
    redefinition .
  methods ZIF_CARGA~SET_CARGA
    redefinition .
  methods ZIF_CARGA~SET_EXCLUIR_ORDEM_VENDA
    redefinition .
  methods ZIF_CARGA~SET_ORDEM_CARREGAMENTO
    redefinition .
  methods ZIF_CARGA~SET_ORDEM_VENDA
    redefinition .
  methods ZIF_CARGA~SET_VOLUME_ORDEM_VENDA
    redefinition .
  methods ZIF_CARGA~VALIDAR_REGISTRO_ORDEM_VENDA
    redefinition .
  methods ZIF_CARGA~VERIF_ORDEM_CARREGAMENTO
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CARGA_RECEBIMENTO_ALGODAO IMPLEMENTATION.


  METHOD ZIF_CARGA~ADD_NOTA_FISCAL.

    R_CARGA = SUPER->ZIF_CARGA~ADD_NOTA_FISCAL( EXPORTING I_NOTA  = I_NOTA IMPORTING E_NOTA = E_NOTA ).

    ME->ZIF_CARGA~CARGA-NM_PESO_LIQUIDO = E_NOTA-NR_QUANTIDADE.
    ME->ZIF_CARGA~CARGA-QT_FARDOS       = E_NOTA-NR_FARDO.

    ME->ZIF_CARGA~SET_AJUSTAR_RAT_ORDEM_VENDA( ).

  ENDMETHOD.


  METHOD zif_carga~add_ordem_venda.

    r_instancia = me.

    IF me->zif_carga~ordem_venda[] IS INITIAL.
      DATA(lc_ck_limpo) = abap_true.
    ELSE.
      lc_ck_limpo = abap_false.
    ENDIF.

    READ TABLE me->zif_carga~ordem_venda WITH KEY nr_ordem_venda = i_ordem_venda-nr_ordem_venda ASSIGNING FIELD-SYMBOL(<fs_ordem>).
    CHECK sy-subrc IS NOT INITIAL.

    TRY .
        zcl_ordem_venda=>zif_ordem_venda~get_instance(
                 )->set_ordem_venda( i_vbeln = i_ordem_venda-nr_ordem_venda
                 )->get_tipo_frete( IMPORTING e_tipo_frete = DATA(e_tipo_frete)
                 )->get_parceiros(  IMPORTING e_vbpa       = DATA(e_vbpa)
                 )->get_item(       IMPORTING e_vbap       = DATA(e_vbap)  "*-CS2022000332-#78064-07.06.2022-JT-inicio
                 ).

        "Validações """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF lc_ck_limpo EQ abap_false.

          "Ponto de Coleta
          TRY.
              me->zif_carga~get_partiner_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda-nr_ordem_venda i_funcao_partiner = 'PC' IMPORTING e_partiner = DATA(r_partiner_pc) ).
            CATCH zcx_carga .
          ENDTRY.

          "Local de Descarga
          TRY.
              me->zif_carga~get_partiner_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda-nr_ordem_venda i_funcao_partiner = 'LR' IMPORTING e_partiner = DATA(r_partiner_lr) ).
            CATCH zcx_carga .
          ENDTRY.

          "Local de Entrega
          TRY.
              me->zif_carga~get_partiner_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda-nr_ordem_venda i_funcao_partiner = 'Z1' IMPORTING e_partiner = DATA(r_partiner_z1) ).
            CATCH zcx_carga .
          ENDTRY.

          "Caso Seja Frete CIF buscar SP
          TRY.
              me->zif_carga~get_partiner_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda-nr_ordem_venda i_funcao_partiner = 'SP' IMPORTING e_partiner = DATA(r_partiner_sp) ).
            CATCH zcx_carga .
          ENDTRY.

          "Não pode ser vinculado Ordens de Venda com Tipo de Frete Diferente
          IF e_tipo_frete NE me->zif_carga~at_tipo_frete_ordem_venda.
            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid = VALUE #( msgid = zcx_carga=>zcx_dif_ordem_tipo_frete-msgid
                                  msgno = zcx_carga=>zcx_dif_ordem_tipo_frete-msgno
                                  attr1 = CONV #( me->zif_carga~at_tipo_frete_ordem_venda ) )
                msgid  = zcx_carga=>zcx_dif_ordem_tipo_frete-msgid
                msgno  = zcx_carga=>zcx_dif_ordem_tipo_frete-msgno
                msgty  = 'E'
                msgv1  = CONV #( me->zif_carga~at_tipo_frete_ordem_venda ).
          ENDIF.

          "Parceiro Ponto de Coleta Diferente
          IF r_partiner_pc-lifnr NE me->zif_carga~carga-id_local_coleta.
            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid = VALUE #( msgid = zcx_carga=>zcx_dif_ordem_ponto_coleta-msgid
                                  msgno = zcx_carga=>zcx_dif_ordem_ponto_coleta-msgno
                                  attr1 = CONV #( me->zif_carga~carga-id_local_coleta ) )
                msgid  = zcx_carga=>zcx_dif_ordem_ponto_coleta-msgid
                msgno  = zcx_carga=>zcx_dif_ordem_ponto_coleta-msgno
                msgty  = 'E'
                msgv1  = CONV #( me->zif_carga~carga-id_local_coleta ).
          ENDIF.

          "Parceiro Local de Entrega Diferente
          IF r_partiner_lr-kunnr NE me->zif_carga~carga-id_local_descarga.
            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid = VALUE #( msgid = zcx_carga=>zcx_dif_ordem_local_entrega-msgid
                                  msgno = zcx_carga=>zcx_dif_ordem_local_entrega-msgno
                                  attr1 = CONV #( me->zif_carga~carga-id_local_descarga ) )
                msgid  = zcx_carga=>zcx_dif_ordem_local_entrega-msgid
                msgno  = zcx_carga=>zcx_dif_ordem_local_entrega-msgno
                msgty  = 'E'
                msgv1  = CONV #( me->zif_carga~carga-id_local_descarga ).
          ENDIF.

          "Parceiro Local de Entrega Diferente
          IF r_partiner_z1-lifnr NE me->zif_carga~carga-id_local_destino.
            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid = VALUE #( msgid = zcx_carga=>zcx_dif_ordem_destino-msgid
                                  msgno = zcx_carga=>zcx_dif_ordem_destino-msgno
                                  attr1 = CONV #( me->zif_carga~carga-id_local_destino ) )
                msgid  = zcx_carga=>zcx_dif_ordem_destino-msgid
                msgno  = zcx_carga=>zcx_dif_ordem_destino-msgno
                msgty  = 'E'
                msgv1  = CONV #( me->zif_carga~carga-id_local_destino ).
          ENDIF.

*-CS2022000332-#78064-07.06.2022-JT-inicio
          "Obtem ID carga da OV
          TRY.
              me->zif_carga~get_contrato_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda-nr_ordem_venda
                                                      IMPORTING e_id_contrato = DATA(l_id_contrato) ).
            CATCH zcx_carga INTO DATA(ex_carga).
              RAISE EXCEPTION TYPE zcx_carga
                EXPORTING
                  textid = VALUE #( msgid = ex_carga->msgid
                                    msgno = ex_carga->msgno
                                    attr1 = CONV #( ex_carga->msgv1 )
                                    attr2 = CONV #( ex_carga->msgv2 )
                                    attr3 = CONV #( ex_carga->msgv3 )
                                    attr4 = CONV #( ex_carga->msgv4 ) )
                  msgid  = ex_carga->msgid
                  msgno  = ex_carga->msgno
                  msgty  = 'E'
                  msgv1  = ex_carga->msgv1
                  msgv2  = ex_carga->msgv2
                  msgv3  = ex_carga->msgv3
                  msgv4  = ex_carga->msgv4.
          ENDTRY.

          "ID Contrato Diferente
          IF l_id_contrato NE me->zif_carga~carga-id_contrato.
            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid = VALUE #( msgid = zcx_carga=>zcx_dif_contrato-msgid
                                  msgno = zcx_carga=>zcx_dif_contrato-msgno )
                msgid  = zcx_carga=>zcx_dif_contrato-msgid
                msgno  = zcx_carga=>zcx_dif_contrato-msgno
                msgty  = 'E'.
          ENDIF.

          "Material Diferente
          IF e_vbap-matnr NE me->zif_carga~carga-id_produto.
            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid = VALUE #( msgid = zcx_carga=>zcx_dif_material-msgid
                                  msgno = zcx_carga=>zcx_dif_material-msgno
                                  attr1 = CONV #( me->zif_carga~carga-id_produto ) )
                msgid  = zcx_carga=>zcx_dif_material-msgid
                msgno  = zcx_carga=>zcx_dif_material-msgno
                msgty  = 'E'
                msgv1  = CONV #( me->zif_carga~carga-id_produto ).
          ENDIF.
*-CS2022000332-#78064-07.06.2022-JT-fim

        ENDIF.
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        IF me->zif_carga~ordem_venda[] IS INITIAL.
          TRY .
              me->zif_carga~set_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda-nr_ordem_venda IMPORTING e_carga = e_carga ).

*-CS2022000332-#78064-07.06.2022-JT-inicio
            CATCH zcx_carga INTO ex_carga.
              RAISE EXCEPTION TYPE zcx_carga
                EXPORTING
                  textid = VALUE #( msgid = ex_carga->msgid
                                    msgno = ex_carga->msgno
                                    attr1 = CONV #( ex_carga->msgv1 )
                                    attr2 = CONV #( ex_carga->msgv2 )
                                    attr3 = CONV #( ex_carga->msgv3 )
                                    attr4 = CONV #( ex_carga->msgv4 ) )
                  msgid  = ex_carga->msgid
                  msgno  = ex_carga->msgno
                  msgty  = 'E'
                  msgv1  = ex_carga->msgv1
                  msgv2  = ex_carga->msgv2
                  msgv3  = ex_carga->msgv3
                  msgv4  = ex_carga->msgv4.
*-CS2022000332-#78064-07.06.2022-JT-fim

            CATCH zcx_parceiros INTO DATA(ex_parceiros).    "

              RAISE EXCEPTION TYPE zcx_carga
                EXPORTING
                  textid = VALUE #( msgid = ex_parceiros->msgid
                                    msgno = ex_parceiros->msgno
                                    attr1 = CONV #( ex_parceiros->msgv1 )
                                    attr2 = CONV #( ex_parceiros->msgv2 )
                                    attr3 = CONV #( ex_parceiros->msgv3 )
                                    attr4 = CONV #( ex_parceiros->msgv4 ) )
                  msgid  = ex_parceiros->msgid
                  msgno  = ex_parceiros->msgno
                  msgty  = 'E'
                  msgv1  = ex_parceiros->msgv1
                  msgv2  = ex_parceiros->msgv2
                  msgv3  = ex_parceiros->msgv3
                  msgv4  = ex_parceiros->msgv4.
          ENDTRY.
        ELSE.

          DATA: lc_zsdt0001ov  TYPE zsdt0001ov.
          lc_zsdt0001ov-id_carga       = me->zif_carga~carga-id_carga.
          lc_zsdt0001ov-nr_ordem_venda = i_ordem_venda-nr_ordem_venda.
          APPEND lc_zsdt0001ov TO me->zif_carga~ordem_venda.

        ENDIF.

      CATCH zcx_ordem_venda INTO DATA(ex_ordem_venda).

        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = ex_ordem_venda->msgid
                              msgno = ex_ordem_venda->msgno
                              attr1 = CONV #( ex_ordem_venda->msgv1 )
                              attr2 = CONV #( ex_ordem_venda->msgv2 )
                              attr3 = CONV #( ex_ordem_venda->msgv3 )
                              attr4 = CONV #( ex_ordem_venda->msgv4 ) )
            msgid  = ex_ordem_venda->msgid
            msgno  = ex_ordem_venda->msgno
            msgty  = 'E'
            msgv1  = ex_ordem_venda->msgv1
            msgv2  = ex_ordem_venda->msgv2
            msgv3  = ex_ordem_venda->msgv3
            msgv4  = ex_ordem_venda->msgv4.

    ENDTRY.

    "I_ORDEM_VENDA  TYPE ZDE_ZSDT0001OV_ALV

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_INFO_ALV_APRESENTACAO.

    CONSTANTS: CS_LINE_COLOR_A TYPE C LENGTH 4 VALUE 'C200',
               CS_LINE_COLOR_L TYPE C LENGTH 4 VALUE 'C500',
               CS_LINE_COLOR_D TYPE C LENGTH 4 VALUE 'C601',
               CS_LINE_COLOR_R TYPE C LENGTH 4 VALUE 'C600',
               CS_LINE_COLOR_P TYPE C LENGTH 4 VALUE 'C200'.

    CONSTANTS: DS_STATUS_A TYPE STRING VALUE 'Aguardando Sol. de Aprovação',
               DS_STATUS_L TYPE STRING VALUE 'Liberado',
               DS_STATUS_D TYPE STRING VALUE 'Deletado',
               DS_STATUS_R TYPE STRING VALUE 'Reprovado',
               DS_STATUS_P TYPE STRING VALUE 'Aguardando Aprovação'.

    DATA: LC_ORDEM_VENDA_ALGO TYPE ZSDT0001OV_ALGODAO_ALV,
          LN_EDICAO           TYPE LVC_S_SCOL.

    LN_EDICAO = VALUE #( FNAME = 'QT_FARDOS' COLOR = VALUE #( COL = 5 INT = 0 INV = 0 ) ).

    R_CARGA = SUPER->ZIF_CARGA~GET_INFO_ALV_APRESENTACAO( IMPORTING E_APRESENTACAO = E_APRESENTACAO ).

    IF E_APRESENTACAO-ORDEM_CARREGA-ID_AGENT_FRETE IS INITIAL.
      E_APRESENTACAO-ORDEM_CARREGA-ID_AGENT_FRETE = E_APRESENTACAO-CARGA-ID_AGENT_FRETE.
      E_APRESENTACAO-ORDEM_CARREGA-DS_AGENT_FRETE = E_APRESENTACAO-CARGA-DS_AGENT_FRETE.
    ENDIF.

    "Informações do ALV de Ordem de Venda do Algodão

    CLEAR: E_APRESENTACAO-ORDEM_VENDA_ALGO[].

    CHECK E_APRESENTACAO-ORDEM_VENDA[] IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(IT_ZSDT0066)
      FROM ZSDT0066
       FOR ALL ENTRIES IN @E_APRESENTACAO-ORDEM_VENDA
     WHERE VBELN EQ @E_APRESENTACAO-ORDEM_VENDA-NR_ORDEM_VENDA.

    SELECT * INTO TABLE @DATA(IT_VBAP)
      FROM VBAP
       FOR ALL ENTRIES IN @E_APRESENTACAO-ORDEM_VENDA
     WHERE VBELN EQ @E_APRESENTACAO-ORDEM_VENDA-NR_ORDEM_VENDA.

    SELECT * INTO TABLE @DATA(IT_ZSDT0001OD)
      FROM ZSDT0001OD
       FOR ALL ENTRIES IN @E_APRESENTACAO-ORDEM_VENDA
     WHERE ID_ORDEM EQ @E_APRESENTACAO-ORDEM_VENDA-ID_ORDEM.

    SELECT VP~VBELN, LF~NAME1
      INTO TABLE @DATA(IT_COLE)
      FROM VBPA AS VP
      INNER JOIN LFA1 AS LF ON LF~LIFNR EQ VP~LIFNR
       FOR ALL ENTRIES IN @E_APRESENTACAO-ORDEM_VENDA
     WHERE VBELN EQ @E_APRESENTACAO-ORDEM_VENDA-NR_ORDEM_VENDA
       AND PARVW EQ 'PC'.

    SORT IT_ZSDT0066 BY VBELN.
    SORT IT_VBAP BY VBELN.
    SORT IT_ZSDT0001OD BY ID_ORDEM.
    SORT IT_COLE BY VBELN.

    LOOP AT E_APRESENTACAO-ORDEM_VENDA INTO DATA(WA_ORDEM_VENDA).
      CLEAR: LC_ORDEM_VENDA_ALGO.

      ZCL_ORDEM_VENDA=>ZIF_ORDEM_VENDA~GET_INSTANCE(
        )->SET_ORDEM_VENDA( I_VBELN = WA_ORDEM_VENDA-NR_ORDEM_VENDA
        )->GET_TIPO_FRETE( IMPORTING E_TIPO_FRETE = DATA(E_TIPO_FRETE)
        ).

      LC_ORDEM_VENDA_ALGO-QT_FARDOS         = WA_ORDEM_VENDA-QT_FARDOS.
      LC_ORDEM_VENDA_ALGO-NM_PESO_BRUTO     = WA_ORDEM_VENDA-NM_PESO_BRUTO.
      LC_ORDEM_VENDA_ALGO-NM_PESO_TARA      = WA_ORDEM_VENDA-NM_PESO_TARA.
      LC_ORDEM_VENDA_ALGO-NM_PESO_SUBTOTAL  = WA_ORDEM_VENDA-NM_PESO_SUBTOTAL.
      LC_ORDEM_VENDA_ALGO-NM_PESO_EMBALAGEM = WA_ORDEM_VENDA-NM_PESO_EMBALAGEM.
      LC_ORDEM_VENDA_ALGO-NM_PESO_LIQUIDO   = WA_ORDEM_VENDA-NM_PESO_LIQUIDO.

      READ TABLE IT_ZSDT0066 INTO DATA(WA_ZSDT0066)
      WITH KEY VBELN = WA_ORDEM_VENDA-NR_ORDEM_VENDA BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
        LC_ORDEM_VENDA_ALGO-DS_INSTRUCAO = WA_ZSDT0066-INSTRUCAO.
        LC_ORDEM_VENDA_ALGO-STATUS       = WA_ZSDT0066-STATUS.
      ENDIF.

      LC_ORDEM_VENDA_ALGO-NR_ORDEM_VENDA = WA_ORDEM_VENDA-NR_ORDEM_VENDA.
      LC_ORDEM_VENDA_ALGO-AUART          = WA_ORDEM_VENDA-TP_TIPO_ORDEM.

      READ TABLE IT_VBAP INTO DATA(WA_VBAP) WITH KEY VBELN = WA_ORDEM_VENDA-NR_ORDEM_VENDA BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
        LC_ORDEM_VENDA_ALGO-LGORT        = WA_VBAP-LGORT.
        LC_ORDEM_VENDA_ALGO-MATNR        = WA_VBAP-MATNR.
        LC_ORDEM_VENDA_ALGO-ARKTX        = WA_VBAP-ARKTX.
        LC_ORDEM_VENDA_ALGO-TOTAL_FARDOS = WA_VBAP-VOLUM.
        LC_ORDEM_VENDA_ALGO-VOLEH        = WA_VBAP-VOLEH.
      ENDIF.

      READ TABLE IT_COLE INTO DATA(WA_COLE) WITH KEY VBELN = WA_ORDEM_VENDA-NR_ORDEM_VENDA BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        LC_ORDEM_VENDA_ALGO-DS_PONTO_COLETA = WA_COLE-NAME1.
      ENDIF.

      CASE LC_ORDEM_VENDA_ALGO-STATUS.
        WHEN 'A'.
          LC_ORDEM_VENDA_ALGO-LINE_COLOR = CS_LINE_COLOR_A.
          LC_ORDEM_VENDA_ALGO-DS_STATUS  = DS_STATUS_A.
        WHEN 'L'.
          LC_ORDEM_VENDA_ALGO-LINE_COLOR = CS_LINE_COLOR_L.
          LC_ORDEM_VENDA_ALGO-DS_STATUS  = DS_STATUS_L.
        WHEN 'D'.
          LC_ORDEM_VENDA_ALGO-LINE_COLOR = CS_LINE_COLOR_D.
          LC_ORDEM_VENDA_ALGO-DS_STATUS  = DS_STATUS_D.
        WHEN 'R'.
          LC_ORDEM_VENDA_ALGO-LINE_COLOR = CS_LINE_COLOR_R.
          LC_ORDEM_VENDA_ALGO-DS_STATUS  = DS_STATUS_R.
        WHEN 'P'.
          LC_ORDEM_VENDA_ALGO-LINE_COLOR = CS_LINE_COLOR_P.
          LC_ORDEM_VENDA_ALGO-DS_STATUS  = DS_STATUS_P.
      ENDCASE.

      "Ordem de Carregamento """"""""""""""""""""""""""""""""""""""""""""""""""
      IF WA_ORDEM_VENDA-ID_ORDEM IS NOT INITIAL.
        READ TABLE IT_ZSDT0001OD INTO DATA(WA_ZSDT0001OD)
        WITH KEY ID_ORDEM = WA_ORDEM_VENDA-ID_ORDEM BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          LC_ORDEM_VENDA_ALGO-NR_ORDEM = WA_ZSDT0001OD-NR_ORDEM.
        ENDIF.
      ENDIF.

      IF ( LC_ORDEM_VENDA_ALGO-NR_ORDEM IS INITIAL ) AND
         ( ME->ZIF_CARGA~CARGA-TP_STATUS EQ ZIF_CARGA=>ST_STATUS_ABERTO OR ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_TRUE ).

        IF E_TIPO_FRETE EQ ZIF_CARGA=>ST_TP_FRETE_CIF.
          LC_ORDEM_VENDA_ALGO-NR_ORDEM = ICON_WF_LINK.
        ELSE.
          LC_ORDEM_VENDA_ALGO-NR_ORDEM = ICON_WF_UNLINK.
        ENDIF.

      ELSEIF LC_ORDEM_VENDA_ALGO-NR_ORDEM IS INITIAL.

        CLEAR: LC_ORDEM_VENDA_ALGO-NR_ORDEM.

      ENDIF.

      IF ME->ZIF_CARGA~CARGA-TP_STATUS EQ ZIF_CARGA=>ST_STATUS_ABERTO AND ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_FALSE.
        APPEND LN_EDICAO TO LC_ORDEM_VENDA_ALGO-COLOR_CELL.
        LC_ORDEM_VENDA_ALGO-ADD_BLOCO = ICON_BW_INFO_CUBE_INA.
      ELSE.
        LC_ORDEM_VENDA_ALGO-ADD_BLOCO = ICON_LOCKED.
      ENDIF.

      APPEND LC_ORDEM_VENDA_ALGO TO E_APRESENTACAO-ORDEM_VENDA_ALGO.
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_VALIDAR_NOTA_FISCAL.

    R_INSTANCE = SUPER->ZIF_CARGA~GET_VALIDAR_NOTA_FISCAL( CHANGING I_NOTA_FISCAL = I_NOTA_FISCAL ).

    IF ME->ZIF_CARGA~CARGA-TP_STATUS NE ZIF_CARGA=>ST_STATUS_CANCELADA.

      IF I_NOTA_FISCAL-NR_FARDO IS INITIAL.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_OBG_INF_QT_FARDO-MSGID MSGNO = ZCX_CARGA=>ZCX_OBG_INF_QT_FARDO-MSGNO )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_OBG_INF_QT_FARDO-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_OBG_INF_QT_FARDO-MSGID.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~GRAVAR_REGISTRO.

    ME->ZIF_CARGA~CARGA-TP_PRODUTO_CARGA = ZIF_CARGA=>ST_TP_PRODUTO_CARGA_ALGODAO.

    R_CARGA = SUPER->ZIF_CARGA~GRAVAR_REGISTRO( IMPORTING E_GRAVOU = E_GRAVOU ).

  ENDMETHOD.


  METHOD zif_carga~set_ajustar_rat_ordem_venda.

    DATA: lc_peso_liquido_fardo      TYPE p LENGTH 16 DECIMALS 10,
          lc_peso_bruto_fardo        TYPE p LENGTH 16 DECIMALS 10,
          lc_peso_embalagem_fardo    TYPE p LENGTH 16 DECIMALS 10,
          lc_peso_tara_fardo         TYPE p LENGTH 16 DECIMALS 10,
          lc_total_liquido_vinculado TYPE p LENGTH 16 DECIMALS 0,
          lc_total_bruto_vinculado   TYPE p LENGTH 16 DECIMALS 0,
          lc_peso_diferenca          TYPE p LENGTH 16 DECIMALS 0,
          lc_peso_adicionar          TYPE p LENGTH 16 DECIMALS 0,
          lc_aux_peso                TYPE p LENGTH 16 DECIMALS 0.

    "R_CARGA = SUPER->ZIF_CARGA~SET_AJUSTAR_RAT_ORDEM_VENDA( ).
    r_carga = me.

    DESCRIBE TABLE me->zif_carga~ordem_venda LINES DATA(qtd_ordens).

    READ TABLE me->zif_carga~documento_fiscal INDEX 1 INTO DATA(wa_nota_fiscal).

    CHECK sy-subrc IS INITIAL.

    "Verifica se foi Informado a Quantidade de Fardos
    CHECK wa_nota_fiscal-nr_fardo IS NOT INITIAL.

    me->zif_carga~carga-qt_fardos = wa_nota_fiscal-nr_fardo.
    DATA(ps_embalagem) = me->zif_carga~carga-nm_peso_subtotal - me->zif_carga~carga-nm_peso_liquido.
    IF ps_embalagem GT 0.
      me->zif_carga~carga-nm_peso_embalagem = ps_embalagem.
    ELSE.
      me->zif_carga~carga-nm_peso_embalagem = 0.
    ENDIF.

    "    IF QTD_ORDENS GT 1.

    " Ordem de Venda """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    lc_peso_embalagem_fardo    = me->zif_carga~carga-nm_peso_embalagem / me->zif_carga~carga-qt_fardos.
    lc_peso_liquido_fardo      = me->zif_carga~carga-nm_peso_liquido / me->zif_carga~carga-qt_fardos.
    lc_peso_tara_fardo         = me->zif_carga~carga-nm_peso_tara / me->zif_carga~carga-qt_fardos.
    lc_total_liquido_vinculado = 0.

    LOOP AT me->zif_carga~ordem_venda ASSIGNING FIELD-SYMBOL(<lc_ordem>).
      <lc_ordem>-nm_peso_liquido   = <lc_ordem>-qt_fardos * lc_peso_liquido_fardo.
      <lc_ordem>-nm_peso_embalagem = <lc_ordem>-qt_fardos * lc_peso_embalagem_fardo.
      <lc_ordem>-nm_peso_subtotal  = <lc_ordem>-nm_peso_liquido + <lc_ordem>-nm_peso_embalagem.
      <lc_ordem>-nm_peso_tara      = <lc_ordem>-qt_fardos * lc_peso_tara_fardo.
      <lc_ordem>-nm_peso_bruto     = <lc_ordem>-nm_peso_subtotal + <lc_ordem>-nm_peso_tara.
      ADD <lc_ordem>-nm_peso_liquido TO lc_total_liquido_vinculado.
    ENDLOOP.

    "Ajustar Distribuição de Peso
    lc_peso_diferenca = me->zif_carga~carga-nm_peso_liquido - lc_total_liquido_vinculado.

    IF lc_peso_diferenca NE 0.
      LOOP AT me->zif_carga~ordem_venda ASSIGNING <lc_ordem>.
        lc_peso_adicionar = ( ( <lc_ordem>-qt_fardos / me->zif_carga~carga-qt_fardos ) * lc_peso_diferenca ).
        <lc_ordem>-nm_peso_liquido   = <lc_ordem>-nm_peso_liquido  + lc_peso_adicionar.
        <lc_ordem>-nm_peso_subtotal  = <lc_ordem>-nm_peso_liquido  + <lc_ordem>-nm_peso_embalagem.
        <lc_ordem>-nm_peso_bruto     = <lc_ordem>-nm_peso_subtotal + <lc_ordem>-nm_peso_tara.
      ENDLOOP.
    ENDIF.

    " Blocos da Ordem de Venda """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT me->zif_carga~ordem_venda INTO DATA(wa_ordem).

      IF wa_ordem-qt_fardos GT 0.
        lc_peso_embalagem_fardo = wa_ordem-nm_peso_embalagem / wa_ordem-qt_fardos.
        lc_peso_liquido_fardo   = wa_ordem-nm_peso_liquido / wa_ordem-qt_fardos.
        lc_peso_bruto_fardo     = wa_ordem-nm_peso_subtotal / wa_ordem-qt_fardos.
      ELSE.
        lc_peso_embalagem_fardo = 0.
        lc_peso_liquido_fardo   = 0.
        lc_peso_bruto_fardo     = 0.
      ENDIF.

      lc_total_liquido_vinculado = 0.
      lc_total_bruto_vinculado = 0.
      LOOP AT me->zif_carga~blocos ASSIGNING FIELD-SYMBOL(<fs_blocos>) WHERE nr_ordem_venda EQ wa_ordem-nr_ordem_venda.
        <fs_blocos>-meins = 'KG'.
        lc_aux_peso = <fs_blocos>-qt_fardos * lc_peso_liquido_fardo.
        <fs_blocos>-ps_fardos_liqui = lc_aux_peso.

        lc_aux_peso = <fs_blocos>-qt_fardos * lc_peso_bruto_fardo.
        <fs_blocos>-ps_fardos_bruto = lc_aux_peso.

        ADD <fs_blocos>-ps_fardos_liqui TO lc_total_liquido_vinculado.
        ADD <fs_blocos>-ps_fardos_bruto TO lc_total_bruto_vinculado.
      ENDLOOP.

      "Ajusta Peso Líquido
      lc_peso_diferenca = wa_ordem-nm_peso_liquido - lc_total_liquido_vinculado.

      IF lc_peso_diferenca NE 0.
*        LOOP AT ME->ZIF_CARGA~BLOCOS ASSIGNING <FS_BLOCOS> WHERE NR_ORDEM_VENDA EQ WA_ORDEM-NR_ORDEM_VENDA.
*          LC_PESO_ADICIONAR = ( ( <FS_BLOCOS>-QT_FARDOS / WA_ORDEM-QT_FARDOS ) * LC_PESO_DIFERENCA ).
*          <FS_BLOCOS>-PS_FARDOS_LIQUI = <FS_BLOCOS>-PS_FARDOS_LIQUI + LC_PESO_ADICIONAR.
*        ENDLOOP.

        READ TABLE me->zif_carga~blocos ASSIGNING <fs_blocos> WITH KEY nr_ordem_venda = wa_ordem-nr_ordem_venda.
        IF sy-subrc EQ 0.
          <fs_blocos>-ps_fardos_liqui = <fs_blocos>-ps_fardos_liqui + lc_peso_diferenca.
        ENDIF.
      ENDIF.

      "Ajusta Peso Bruto
      lc_peso_diferenca = wa_ordem-nm_peso_subtotal - lc_total_bruto_vinculado.

      IF lc_peso_diferenca NE 0.
        LOOP AT me->zif_carga~blocos ASSIGNING <fs_blocos> WHERE nr_ordem_venda EQ wa_ordem-nr_ordem_venda.
          lc_peso_adicionar = ( ( <fs_blocos>-qt_fardos / wa_ordem-qt_fardos ) * lc_peso_diferenca ).
          <fs_blocos>-ps_fardos_bruto = <fs_blocos>-ps_fardos_bruto + lc_peso_adicionar.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_CARGA.

**TRY.
*CALL METHOD SUPER->ZIF_CARGA~SET_CARGA
*  EXPORTING
*    I_CARGA             =
**  IMPORTING
**    E_CARGA_RECEBIMENTO =
*  RECEIVING
*    R_CARGA             =
*    .
** CATCH ZCX_CARGA .
**ENDTRY.

    R_CARGA = ME.

    CASE ME->ZIF_CARGA~CARGA-TP_STATUS.
      WHEN ZIF_CARGA=>ST_STATUS_FECHADO.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_CARGA_FECHADA-MSGID MSGNO = ZCX_CARGA=>ZCX_CARGA_FECHADA-MSGNO )
            MSGTY  = 'E'
            MSGID  = ZCX_CARGA=>ZCX_CARGA_FECHADA-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_CARGA_FECHADA-MSGNO.
      WHEN ZIF_CARGA=>ST_STATUS_CONFERIDO.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_CARGA_CONFERIDA-MSGID MSGNO = ZCX_CARGA=>ZCX_CARGA_CONFERIDA-MSGNO )
            MSGTY  = 'E'
            MSGID  = ZCX_CARGA=>ZCX_CARGA_CONFERIDA-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_CARGA_CONFERIDA-MSGNO.
      WHEN ZIF_CARGA=>ST_STATUS_CANCELADA.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_CARGA_CANCELADA-MSGID MSGNO = ZCX_CARGA=>ZCX_CARGA_CANCELADA-MSGNO )
            MSGTY  = 'E'
            MSGID  = ZCX_CARGA=>ZCX_CARGA_CANCELADA-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_CARGA_CANCELADA-MSGNO.
    ENDCASE.

    IF ME->ZIF_CARGA~CARGA-TP_STATUS NE ZIF_CARGA=>ST_STATUS_ABERTO.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_CARGA_NAO_ABERTA-MSGID MSGNO = ZCX_CARGA=>ZCX_CARGA_NAO_ABERTA-MSGNO )
          MSGTY  = 'E'
          MSGID  = ZCX_CARGA=>ZCX_CARGA_NAO_ABERTA-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_CARGA_NAO_ABERTA-MSGNO.
    ENDIF.

    DATA(WA_CARGA) = I_CARGA.
    DATA(CP_ME_CARGA) = ME->ZIF_CARGA~CARGA.
    DATA(CP_ME_CLASSIFICACAO) = ME->ZIF_CARGA~CLASSIFICACAO.

    WA_CARGA-ID_CARGA         = ME->ZIF_CARGA~CARGA-ID_CARGA.
    WA_CARGA-ID_CLASSIFICACAO = ME->ZIF_CARGA~CLASSIFICACAO-ID_CLASSIFICACAO.
    MOVE-CORRESPONDING WA_CARGA TO ME->ZIF_CARGA~CARGA.
    MOVE-CORRESPONDING WA_CARGA TO ME->ZIF_CARGA~CLASSIFICACAO.

    IF ME->ZIF_CARGA~CARGA NE CP_ME_CARGA.
      ME->ZIF_CARGA~CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF ME->ZIF_CARGA~CLASSIFICACAO NE CP_ME_CLASSIFICACAO.
      ME->ZIF_CARGA~CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    E_CARGA_RECEBIMENTO = I_CARGA.

    READ TABLE ME->ZIF_CARGA~DOCUMENTO_FISCAL INDEX 1 INTO DATA(WA_NOTA_FISCAL).
    ME->ZIF_CARGA~CARGA-NM_PESO_DESCONTOS = 0.
    ME->ZIF_CARGA~CARGA-NM_PESO_LIQUIDO   = WA_NOTA_FISCAL-NR_QUANTIDADE.

    IF ME->ZIF_CARGA~CARGA-NM_PESO_SUBTOTAL IS NOT INITIAL AND ME->ZIF_CARGA~CARGA-NM_PESO_LIQUIDO IS NOT INITIAL.
      ME->ZIF_CARGA~CARGA-NM_PESO_EMBALAGEM = ME->ZIF_CARGA~CARGA-NM_PESO_SUBTOTAL - ME->ZIF_CARGA~CARGA-NM_PESO_LIQUIDO.
      IF ME->ZIF_CARGA~CARGA-NM_PESO_EMBALAGEM LE 0.
        ME->ZIF_CARGA~CARGA-NM_PESO_EMBALAGEM = 0.
      ENDIF.
    ENDIF.

    E_CARGA_RECEBIMENTO-NM_PESO_LIQUIDO   = ME->ZIF_CARGA~CARGA-NM_PESO_LIQUIDO.
    E_CARGA_RECEBIMENTO-NM_PESO_SUBTOTAL  = ME->ZIF_CARGA~CARGA-NM_PESO_SUBTOTAL.
    E_CARGA_RECEBIMENTO-NM_PESO_EMBALAGEM = ME->ZIF_CARGA~CARGA-NM_PESO_EMBALAGEM.

    "Local de Coleta
    IF ( ME->ZIF_CARGA~CARGA-ID_LOCAL_COLETA NE CP_ME_CARGA-ID_LOCAL_COLETA ) OR
       ( ME->ZIF_CARGA~CARGA-ID_LOCAL_COLETA IS NOT INITIAL AND E_CARGA_RECEBIMENTO-DS_LOCAL_COLETA IS INITIAL ).
      CLEAR: E_CARGA_RECEBIMENTO-DS_LOCAL_COLETA.
      SELECT SINGLE NAME1
        INTO E_CARGA_RECEBIMENTO-DS_LOCAL_COLETA
        FROM LFA1 WHERE LIFNR EQ ME->ZIF_CARGA~CARGA-ID_LOCAL_COLETA.
    ELSEIF ME->ZIF_CARGA~CARGA-ID_LOCAL_COLETA IS INITIAL.
      CLEAR: E_CARGA_RECEBIMENTO-DS_LOCAL_COLETA.
    ENDIF.

    "Local de Descarga
    IF ( ME->ZIF_CARGA~CARGA-ID_LOCAL_DESCARGA NE CP_ME_CARGA-ID_LOCAL_DESCARGA ) OR
       ( ME->ZIF_CARGA~CARGA-ID_LOCAL_DESCARGA IS NOT INITIAL AND E_CARGA_RECEBIMENTO-DS_LOCAL_DESCARGA IS INITIAL ) .
      CLEAR: E_CARGA_RECEBIMENTO-DS_LOCAL_DESCARGA.
      SELECT SINGLE NAME1
        INTO E_CARGA_RECEBIMENTO-DS_LOCAL_DESCARGA
        FROM KNA1 WHERE KUNNR EQ ME->ZIF_CARGA~CARGA-ID_LOCAL_DESCARGA.
    ELSEIF ME->ZIF_CARGA~CARGA-ID_LOCAL_DESCARGA IS INITIAL.
      CLEAR: E_CARGA_RECEBIMENTO-DS_LOCAL_DESCARGA.
    ENDIF.

    "Local de Destino
    IF ( ME->ZIF_CARGA~CARGA-ID_LOCAL_DESTINO NE CP_ME_CARGA-ID_LOCAL_DESTINO ) OR
       ( ME->ZIF_CARGA~CARGA-ID_LOCAL_DESTINO IS NOT INITIAL AND E_CARGA_RECEBIMENTO-DS_LOCAL_DESTINO IS INITIAL ).
      CLEAR: E_CARGA_RECEBIMENTO-DS_LOCAL_DESTINO.
      SELECT SINGLE NAME1
        INTO E_CARGA_RECEBIMENTO-DS_LOCAL_DESTINO
        FROM LFA1 WHERE LIFNR EQ ME->ZIF_CARGA~CARGA-ID_LOCAL_DESTINO.
    ELSEIF ME->ZIF_CARGA~CARGA-ID_LOCAL_DESTINO IS INITIAL.
      CLEAR: E_CARGA_RECEBIMENTO-DS_LOCAL_DESTINO.
    ENDIF.

    "Local de Entrega
    IF ( ME->ZIF_CARGA~CARGA-ID_LOCAL_ENTREGA NE CP_ME_CARGA-ID_LOCAL_ENTREGA ) OR
       ( ME->ZIF_CARGA~CARGA-ID_LOCAL_ENTREGA IS NOT INITIAL AND E_CARGA_RECEBIMENTO-DS_LOCAL_ENTREGA IS INITIAL ).
      CLEAR: E_CARGA_RECEBIMENTO-DS_LOCAL_ENTREGA.
      SELECT SINGLE DS_LOCAL_ENTREGA
        INTO E_CARGA_RECEBIMENTO-DS_LOCAL_ENTREGA
        FROM ZSDT0001LE WHERE ID_LOCAL_ENTREGA EQ ME->ZIF_CARGA~CARGA-ID_LOCAL_ENTREGA.
    ELSEIF ME->ZIF_CARGA~CARGA-ID_LOCAL_ENTREGA IS INITIAL.
      CLEAR: E_CARGA_RECEBIMENTO-DS_LOCAL_ENTREGA.
    ENDIF.

    "Motorista
    IF ( ME->ZIF_CARGA~CARGA-ID_MOTORISTA NE CP_ME_CARGA-ID_MOTORISTA ) OR
       ( ME->ZIF_CARGA~CARGA-ID_MOTORISTA IS NOT INITIAL AND E_CARGA_RECEBIMENTO-DS_MOTORISTA IS INITIAL ).
      CLEAR: E_CARGA_RECEBIMENTO-DS_MOTORISTA.
      SELECT SINGLE NAME1
        INTO E_CARGA_RECEBIMENTO-DS_MOTORISTA
        FROM LFA1 WHERE LIFNR EQ ME->ZIF_CARGA~CARGA-ID_MOTORISTA.
    ELSEIF ME->ZIF_CARGA~CARGA-ID_MOTORISTA IS INITIAL.
      CLEAR: E_CARGA_RECEBIMENTO-DS_MOTORISTA.
    ENDIF.

    "Proprietário do Veículo
    IF ( ME->ZIF_CARGA~CARGA-ID_PROPRIETARIO NE CP_ME_CARGA-ID_PROPRIETARIO ) OR
       ( ME->ZIF_CARGA~CARGA-ID_PROPRIETARIO IS NOT INITIAL AND E_CARGA_RECEBIMENTO-DS_PROPRIETARIO IS INITIAL ).
      CLEAR: E_CARGA_RECEBIMENTO-DS_PROPRIETARIO.
      SELECT SINGLE NAME1
        INTO E_CARGA_RECEBIMENTO-DS_PROPRIETARIO
        FROM LFA1 WHERE LIFNR EQ ME->ZIF_CARGA~CARGA-ID_PROPRIETARIO.
    ELSEIF ME->ZIF_CARGA~CARGA-ID_PROPRIETARIO IS INITIAL.
      CLEAR: E_CARGA_RECEBIMENTO-DS_PROPRIETARIO.
    ENDIF.

    "Agente de Frete
    IF ( ME->ZIF_CARGA~CARGA-ID_AGENT_FRETE NE CP_ME_CARGA-ID_AGENT_FRETE ) OR
       ( ME->ZIF_CARGA~CARGA-ID_AGENT_FRETE IS NOT INITIAL AND E_CARGA_RECEBIMENTO-DS_AGENT_FRETE IS INITIAL ).
      CLEAR: E_CARGA_RECEBIMENTO-DS_AGENT_FRETE.
      SELECT SINGLE NAME1
        INTO E_CARGA_RECEBIMENTO-DS_AGENT_FRETE
        FROM LFA1 WHERE LIFNR EQ ME->ZIF_CARGA~CARGA-ID_AGENT_FRETE.
    ELSEIF ME->ZIF_CARGA~CARGA-ID_LOCAL_COLETA IS INITIAL.
      CLEAR: E_CARGA_RECEBIMENTO-DS_AGENT_FRETE.
    ENDIF.

    "Informações alteradas na classificação
    E_CARGA_RECEBIMENTO-NR_RES_RR1_RR2 = E_CARGA_RECEBIMENTO-NR_RESULTADO_01 + E_CARGA_RECEBIMENTO-NR_RESULTADO_02.

    IF ME->ZIF_CARGA~CLASSIFICACAO-IN_SRR_ORIGEM_PARTIC EQ ABAP_FALSE.
      CLEAR: E_CARGA_RECEBIMENTO-DS_OUTRO_PARTIC,
             E_CARGA_RECEBIMENTO-ID_OUTRO_PARTIC.
    ENDIF.

    IF ( ME->ZIF_CARGA~CLASSIFICACAO-ID_OUTRO_PARTIC NE CP_ME_CLASSIFICACAO-ID_OUTRO_PARTIC ) OR
       ( ME->ZIF_CARGA~CLASSIFICACAO-ID_OUTRO_PARTIC IS NOT INITIAL AND E_CARGA_RECEBIMENTO-DS_OUTRO_PARTIC IS INITIAL ).
      CLEAR: E_CARGA_RECEBIMENTO-DS_OUTRO_PARTIC.
      SELECT SINGLE NAME1
        INTO E_CARGA_RECEBIMENTO-DS_OUTRO_PARTIC
        FROM LFA1 WHERE LIFNR EQ ME->ZIF_CARGA~CLASSIFICACAO-ID_OUTRO_PARTIC.
    ELSEIF ME->ZIF_CARGA~CLASSIFICACAO-ID_OUTRO_PARTIC IS INITIAL.
      CLEAR: E_CARGA_RECEBIMENTO-DS_OUTRO_PARTIC.
    ENDIF.

    "Classificadora
    IF ( ME->ZIF_CARGA~CLASSIFICACAO-ID_CLASSIFICADORA NE CP_ME_CLASSIFICACAO-ID_CLASSIFICADORA ) OR
       ( ME->ZIF_CARGA~CLASSIFICACAO-ID_CLASSIFICADORA IS NOT INITIAL AND E_CARGA_RECEBIMENTO-DS_CLASSIFICADORA IS INITIAL ).
      CLEAR: E_CARGA_RECEBIMENTO-DS_CLASSIFICADORA.
      SELECT SINGLE NAME1
        INTO E_CARGA_RECEBIMENTO-DS_CLASSIFICADORA
        FROM LFA1 WHERE LIFNR EQ ME->ZIF_CARGA~CLASSIFICACAO-ID_CLASSIFICADORA.
    ELSEIF ME->ZIF_CARGA~CLASSIFICACAO-ID_CLASSIFICADORA IS INITIAL.
      CLEAR: E_CARGA_RECEBIMENTO-DS_CLASSIFICADORA.
    ENDIF.

    IF ME->ZIF_CARGA~CARGA-NM_PESO_SUBTOTAL NE CP_ME_CARGA-NM_PESO_SUBTOTAL OR
       ME->ZIF_CARGA~CARGA-NM_PESO_LIQUIDO  NE CP_ME_CARGA-NM_PESO_LIQUIDO.
      ME->ZIF_CARGA~SET_AJUSTAR_RAT_DESC_GERAL( ).
    ENDIF.

    ME->ZIF_CARGA~SET_AJUSTAR_RAT_ORDEM_VENDA( ).

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_EXCLUIR_ORDEM_VENDA.

    R_CARGA = SUPER->ZIF_CARGA~SET_EXCLUIR_ORDEM_VENDA( EXPORTING I_VBELN = I_VBELN ).

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_ORDEM_CARREGAMENTO.

    R_CARGA = ME.

    E_ORDEM_CARRGAMENTO = ZCL_ORDEM_CARREGAMENTO=>BUSCA_ORDEM_CARREGAMENTO_NR(
    I_NR_SAFRA  = I_NR_SAFRA
    I_ID_BUKRS  = I_ID_BUKRS
    I_ID_BRANCH = I_ID_BRANCH
    I_NR_ORDEM  = I_NR_ORDEM ).

    "Verificar se já foi informado a ordem de carregamento para outra Ordem de Venda
    LOOP AT ME->ZIF_CARGA~ORDEM_VENDA INTO DATA(WA_ORDEM_VENDA)
      WHERE NR_ORDEM_VENDA NE I_VBELN AND ID_ORDEM EQ E_ORDEM_CARRGAMENTO-ID_ORDEM.

      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_OC_VINCULADA_OV-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_OC_VINCULADA_OV-MSGNO
                            ATTR1 = CONV #( I_NR_ORDEM )
                            ATTR2 = CONV #( WA_ORDEM_VENDA-NR_ORDEM_VENDA ) )
          MSGID  = ZCX_CARGA=>ZCX_OC_VINCULADA_OV-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_OC_VINCULADA_OV-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_NR_ORDEM )
          MSGV2  = CONV #( WA_ORDEM_VENDA-NR_ORDEM_VENDA ).

    ENDLOOP.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    R_CARGA =
    SUPER->ZIF_CARGA~SET_ORDEM_CARREGAMENTO(
       EXPORTING
         I_NR_SAFRA          = I_NR_SAFRA
         I_ID_BUKRS          = I_ID_BUKRS
         I_ID_BRANCH         = I_ID_BRANCH
         I_NR_ORDEM          = I_NR_ORDEM
         I_VBELN             = I_VBELN
       IMPORTING
         E_ORDEM_CARRGAMENTO = E_ORDEM_CARRGAMENTO
       CHANGING
         I_CARGA_ALV         = I_CARGA_ALV ).

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_ORDEM_VENDA.

    CALL METHOD SUPER->ZIF_CARGA~SET_ORDEM_VENDA
      EXPORTING
        I_ORDEM_VENDA        = I_ORDEM_VENDA
      IMPORTING
        E_CARGA              = E_CARGA
      CHANGING
        C_ZDE_ZSDT0001OV_ALV = C_ZDE_ZSDT0001OV_ALV
      RECEIVING
        R_CARGA              = R_CARGA.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_VOLUME_ORDEM_VENDA.

    R_CARGA = SUPER->ZIF_CARGA~SET_VOLUME_ORDEM_VENDA( EXPORTING I_VBELN = I_VBELN I_VOLUME = I_VOLUME ).

    SELECT SINGLE VOLUM INTO @DATA(QT_ORDEM)
      FROM VBAP
     WHERE VBELN EQ @I_VBELN.

    SELECT SUM( OV~QT_FARDOS ) INTO @DATA(QT_CARGAS)
      FROM ZSDT0001OV AS OV
     INNER JOIN ZSDT0001CG AS CG ON CG~ID_CARGA EQ OV~ID_CARGA
     WHERE OV~NR_ORDEM_VENDA EQ @I_VBELN
       AND OV~ID_CARGA       NE @ME->ZIF_CARGA~CARGA-ID_CARGA
       AND OV~ID_CARGA       NE @SPACE
       AND CG~TP_STATUS      EQ @ZIF_CARGA=>ST_STATUS_CONFERIDO
       AND CG~TP_CARGA       EQ @ZIF_CARGA=>ST_TP_CARGA_ENTRADA_FOB.

    DATA(LC_SALDO) = QT_ORDEM - QT_CARGAS.
    IF LC_SALDO LT I_VOLUME.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_OV_SALDO_VOLUME-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_OV_SALDO_VOLUME-MSGNO
                            ATTR1 = CONV #( I_VBELN )
                            ATTR2 = CONV #( I_VOLUME ) )
          MSGID  = ZCX_CARGA=>ZCX_OV_SALDO_VOLUME-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_OV_SALDO_VOLUME-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_VBELN )
          MSGV2  = CONV #( I_VOLUME ).
    ENDIF.

    READ TABLE ME->ZIF_CARGA~ORDEM_VENDA WITH KEY NR_ORDEM_VENDA = I_VBELN ASSIGNING FIELD-SYMBOL(<FS_ORDEM>).
    IF SY-SUBRC IS INITIAL.
      <FS_ORDEM>-QT_FARDOS = I_VOLUME.
      ME->ZIF_CARGA~SET_AJUSTAR_RAT_ORDEM_VENDA( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_carga~validar_registro_ordem_venda.

    DATA: e_tipo_frete  TYPE zde_tp_frete.

    LOOP AT me->zif_carga~ordem_venda INTO DATA(wa_ordem_venda).

      DATA(ordem_venda) =
      zcl_ordem_venda=>zif_ordem_venda~get_instance(
               )->set_ordem_venda( i_vbeln = wa_ordem_venda-nr_ordem_venda
               )->ck_filial_emissora_romaneio( i_branch = me->zif_carga~carga-id_branch
               )->ck_safra( i_safra = CONV #( me->zif_carga~carga-nr_safra )
               )->ck_ordem_venda_dco(  i_branch = me->zif_carga~carga-id_branch i_matnr = me->zif_carga~carga-id_produto
               )->get_tipo_frete( IMPORTING  e_tipo_frete = e_tipo_frete
               ).

      "Somente Permitido Ordem de Venda CIF, CPT ou CFR
      TRY .
          ordem_venda->ck_tipo_frete( i_tipo_frete = zif_carga=>st_tp_frete_cif ).
        CATCH zcx_ordem_venda.
          TRY .
              ordem_venda->ck_tipo_frete( i_tipo_frete = zif_carga=>st_tp_frete_cpt ).
            CATCH zcx_ordem_venda.
              TRY.
                  ordem_venda->ck_tipo_frete( i_tipo_frete = zif_carga=>st_tp_frete_cfr ).
                CATCH zcx_ordem_venda.
                  ordem_venda->ck_tipo_frete( i_tipo_frete = zif_carga=>st_tp_frete_fob ). "MM - U.S 155370 / Validação Frete OV ZMM0127 - WPP
              ENDTRY.
          ENDTRY.
      ENDTRY.

      TRY .
          CASE e_tipo_frete.
            WHEN zif_carga=>st_tp_frete_cif.

              IF wa_ordem_venda-id_ordem IS INITIAL.
                RAISE EXCEPTION TYPE zcx_carga
                  EXPORTING
                    textid = VALUE #( msgid = zcx_carga=>zcx_ordem_venda_cif_od-msgid
                                      msgno = zcx_carga=>zcx_ordem_venda_cif_od-msgno
                                      attr1 = CONV #( wa_ordem_venda-nr_ordem_venda ) )
                    msgty  = 'E'
                    msgno  = zcx_carga=>zcx_ordem_venda_cif_od-msgno
                    msgid  = zcx_carga=>zcx_ordem_venda_cif_od-msgid
                    msgv1  = CONV #( wa_ordem_venda-nr_ordem_venda ).
              ENDIF.

              IF me->zif_carga~carga-id_agent_frete IS NOT INITIAL AND me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
                DATA(ob_fornecedores) = zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = me->zif_carga~carga-id_agent_frete
                  )->ck_ativo(
                  )->ck_ativo_empresa( i_empresa = me->zif_carga~carga-id_bukrs
                  )->ck_parceiro_local_negocio(
                  ).
              ENDIF.

            WHEN zif_carga=>st_tp_frete_cpt.

              "Não Valida Agente de Frete
              IF me->zif_carga~carga-id_agent_frete IS NOT INITIAL AND me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
                ob_fornecedores = zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = me->zif_carga~carga-id_agent_frete
                  )->ck_ativo(
                  )->ck_ativo_empresa( i_empresa = me->zif_carga~carga-id_bukrs
                  )->ck_parceiro_terceiro(
                  )->ck_servico_frete(
                  )->ck_servico_frete_rodo(
                  ).
              ENDIF.

            WHEN zif_carga=>st_tp_frete_cfr.

              "Não Valida Agente de Frete
              IF me->zif_carga~carga-id_agent_frete IS NOT INITIAL AND me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
                ob_fornecedores = zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = me->zif_carga~carga-id_agent_frete
                  )->ck_ativo(
                  )->ck_ativo_empresa( i_empresa = me->zif_carga~carga-id_bukrs
                  )->ck_servico_frete(
                  )->ck_servico_frete_rodo(
                  ).
              ENDIF.

          ENDCASE.

        CATCH zcx_parceiros INTO DATA(ex_parceiros).    " .
          MESSAGE ID ex_parceiros->if_t100_message~t100key-msgid
             TYPE 'E'
           NUMBER ex_parceiros->if_t100_message~t100key-msgno
             WITH ex_parceiros->msgv1
                  ex_parceiros->msgv2
                  ex_parceiros->msgv3
                  ex_parceiros->msgv4
             INTO DATA(lc_texto).

          CONCATENATE 'Agente de Frete: ' lc_texto INTO lc_texto.
          me->zif_carga~gera_erro_geral( i_texto = lc_texto ).
      ENDTRY.

      "Verifica Ordem de Venda e Ordem de Carregamento
      zcl_atribui_rom_doctrans=>get_ck_ov_oc(
        EXPORTING
          i_nr_ordem_venda = wa_ordem_venda-nr_ordem_venda    " Documento de vendas
          i_id_ordem       = wa_ordem_venda-id_ordem    " Ordem de Carregamento
      ).


      "Ponto de Coleta
      TRY.
          me->zif_carga~get_partiner_ordem_venda( EXPORTING i_ordem_venda = wa_ordem_venda-nr_ordem_venda i_funcao_partiner = 'PC' IMPORTING e_partiner = DATA(r_partiner_pc) ).
        CATCH zcx_carga .
      ENDTRY.

      "Local de Descarga
      TRY.
          me->zif_carga~get_partiner_ordem_venda( EXPORTING i_ordem_venda = wa_ordem_venda-nr_ordem_venda i_funcao_partiner = 'LR' IMPORTING e_partiner = DATA(r_partiner_lr) ).
        CATCH zcx_carga .
      ENDTRY.

      "Local de Entrega
      TRY.
          me->zif_carga~get_partiner_ordem_venda( EXPORTING i_ordem_venda = wa_ordem_venda-nr_ordem_venda i_funcao_partiner = 'Z1' IMPORTING e_partiner = DATA(r_partiner_z1) ).
        CATCH zcx_carga .
      ENDTRY.

      "Caso Seja Frete CIF buscar SP
      TRY.
          me->zif_carga~get_partiner_ordem_venda( EXPORTING i_ordem_venda = wa_ordem_venda-nr_ordem_venda i_funcao_partiner = 'SP' IMPORTING e_partiner = DATA(r_partiner_sp) ).
        CATCH zcx_carga .
      ENDTRY.

      "Não pode ser vinculado Ordens de Venda com Tipo de Frete Diferente
      IF e_tipo_frete NE me->zif_carga~at_tipo_frete_ordem_venda.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_dif_ordem_tipo_frete-msgid
                              msgno = zcx_carga=>zcx_dif_ordem_tipo_frete-msgno
                              attr1 = CONV #( me->zif_carga~at_tipo_frete_ordem_venda ) )
            msgid  = zcx_carga=>zcx_dif_ordem_tipo_frete-msgid
            msgno  = zcx_carga=>zcx_dif_ordem_tipo_frete-msgno
            msgty  = 'E'
            msgv1  = CONV #( me->zif_carga~at_tipo_frete_ordem_venda ).
      ENDIF.

      "Parceiro Ponto de Coleta Diferente
      IF r_partiner_pc-lifnr NE me->zif_carga~carga-id_local_coleta.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_dif_ordem_ponto_coleta-msgid
                              msgno = zcx_carga=>zcx_dif_ordem_ponto_coleta-msgno
                              attr1 = CONV #( me->zif_carga~carga-id_local_coleta ) )
            msgid  = zcx_carga=>zcx_dif_ordem_ponto_coleta-msgid
            msgno  = zcx_carga=>zcx_dif_ordem_ponto_coleta-msgno
            msgty  = 'E'
            msgv1  = CONV #( me->zif_carga~carga-id_local_coleta ).
      ENDIF.

      "Parceiro Local de Entrega Diferente
      IF r_partiner_lr-kunnr NE me->zif_carga~carga-id_local_descarga.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_dif_ordem_local_entrega-msgid
                              msgno = zcx_carga=>zcx_dif_ordem_local_entrega-msgno
                              attr1 = CONV #( me->zif_carga~carga-id_local_descarga ) )
            msgid  = zcx_carga=>zcx_dif_ordem_local_entrega-msgid
            msgno  = zcx_carga=>zcx_dif_ordem_local_entrega-msgno
            msgty  = 'E'
            msgv1  = CONV #( me->zif_carga~carga-id_local_descarga ).
      ENDIF.

      "Parceiro Local de Entrega Diferente
      IF r_partiner_z1-lifnr NE me->zif_carga~carga-id_local_destino.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_dif_ordem_destino-msgid
                              msgno = zcx_carga=>zcx_dif_ordem_destino-msgno
                              attr1 = CONV #( me->zif_carga~carga-id_local_destino ) )
            msgid  = zcx_carga=>zcx_dif_ordem_destino-msgid
            msgno  = zcx_carga=>zcx_dif_ordem_destino-msgno
            msgty  = 'E'
            msgv1  = CONV #( me->zif_carga~carga-id_local_destino ).
      ENDIF.

    ENDLOOP.
    CLEAR: ordem_venda, ob_fornecedores.

  ENDMETHOD.


  METHOD ZIF_CARGA~VERIF_ORDEM_CARREGAMENTO.

    R_CARGA = SUPER->ZIF_CARGA~VERIF_ORDEM_CARREGAMENTO(  EXPORTING  I_ORDEM = I_ORDEM I_CK_VERIFICAR_CARGA = ABAP_TRUE ).

  ENDMETHOD.
ENDCLASS.
