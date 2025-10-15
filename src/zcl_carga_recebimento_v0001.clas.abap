CLASS ZCL_CARGA_RECEBIMENTO_V0001 DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES ZIF_CARGA .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES CARGA
      FOR ZIF_CARGA~CARGA .
    ALIASES CK_ALTEROU
      FOR ZIF_CARGA~CK_ALTEROU .
    ALIASES CLASSIFICACAO
      FOR ZIF_CARGA~CLASSIFICACAO .
    ALIASES CLASSIFICACAO_NOTAS
      FOR ZIF_CARGA~CLASSIFICACAO_NOTAS .
    ALIASES DOCUMENTO_FISCAL
      FOR ZIF_CARGA~DOCUMENTO_FISCAL .
    ALIASES DOCUMENTO_FISCAL_IMP_RET
      FOR ZIF_CARGA~DOCUMENTO_FISCAL_IMP_RET .
    ALIASES RESULTADO
      FOR ZIF_CARGA~RESULTADO .
    ALIASES ST_GMO_NAO_TESTADO
      FOR ZIF_CARGA~ST_GMO_NAO_TESTADO .
    ALIASES ST_GMO_NEGATIVO
      FOR ZIF_CARGA~ST_GMO_NEGATIVO .
    ALIASES ST_GMO_POSITIVO
      FOR ZIF_CARGA~ST_GMO_POSITIVO .
    ALIASES ST_INTERFACE_AVISO
      FOR ZIF_CARGA~ST_INTERFACE_AVISO .
    ALIASES ST_INTERFACE_MIGO
      FOR ZIF_CARGA~ST_INTERFACE_MIGO .
    ALIASES ST_INTERFACE_MIRO
      FOR ZIF_CARGA~ST_INTERFACE_MIRO .
    ALIASES ST_MODEL_FISCAL_ELETRONICO
      FOR ZIF_CARGA~ST_MODEL_FISCAL_ELETRONICO .
    ALIASES ST_MODEL_FISCAL_PAPEL
      FOR ZIF_CARGA~ST_MODEL_FISCAL_PAPEL .
    ALIASES ST_STATUS_ABERTO
      FOR ZIF_CARGA~ST_STATUS_ABERTO .
    ALIASES ST_STATUS_CANCELADA
      FOR ZIF_CARGA~ST_STATUS_CANCELADA .
    ALIASES ST_STATUS_CONFERIDO
      FOR ZIF_CARGA~ST_STATUS_CONFERIDO .
    ALIASES ST_STATUS_ESTORNO_BLOQUEIO
      FOR ZIF_CARGA~ST_STATUS_ESTORNO_BLOQUEIO .
    ALIASES ST_STATUS_ESTORNO_ERRO
      FOR ZIF_CARGA~ST_STATUS_ESTORNO_ERRO .
    ALIASES ST_STATUS_ESTORNO_EXECUTADO
      FOR ZIF_CARGA~ST_STATUS_ESTORNO_EXECUTADO .
    ALIASES ST_STATUS_ESTORNO_SEM
      FOR ZIF_CARGA~ST_STATUS_ESTORNO_SEM .
    ALIASES ST_STATUS_ESTORNO_SOLICITADO
      FOR ZIF_CARGA~ST_STATUS_ESTORNO_SOLICITADO .
    ALIASES ST_STATUS_FECHADO
      FOR ZIF_CARGA~ST_STATUS_FECHADO .
    ALIASES ST_TP_CARACT_CLASS_ARDIDO
      FOR ZIF_CARGA~ST_TP_CARACT_CLASS_ARDIDO .
    ALIASES ST_TP_CARACT_CLASS_AVARIADO
      FOR ZIF_CARGA~ST_TP_CARACT_CLASS_AVARIADO .
    ALIASES ST_TP_CARACT_CLASS_ESVERDEADO
      FOR ZIF_CARGA~ST_TP_CARACT_CLASS_ESVERDEADO .
    ALIASES ST_TP_CARACT_CLASS_IMPUREZA
      FOR ZIF_CARGA~ST_TP_CARACT_CLASS_IMPUREZA .
    ALIASES ST_TP_CARACT_CLASS_QUEBRADO
      FOR ZIF_CARGA~ST_TP_CARACT_CLASS_QUEBRADO .
    ALIASES ST_TP_CARACT_CLASS_UMIDADE
      FOR ZIF_CARGA~ST_TP_CARACT_CLASS_UMIDADE .
    ALIASES ST_TP_FRETE_CFR
      FOR ZIF_CARGA~ST_TP_FRETE_CFR .
    ALIASES ST_TP_FRETE_CIF
      FOR ZIF_CARGA~ST_TP_FRETE_CIF .
    ALIASES ST_TP_FRETE_CIP
      FOR ZIF_CARGA~ST_TP_FRETE_CIP .
    ALIASES ST_TP_FRETE_CPT
      FOR ZIF_CARGA~ST_TP_FRETE_CPT .
    ALIASES ST_TP_FRETE_DAF
      FOR ZIF_CARGA~ST_TP_FRETE_DAF .
    ALIASES ST_TP_FRETE_DDP
      FOR ZIF_CARGA~ST_TP_FRETE_DDP .
    ALIASES ST_TP_FRETE_DDU
      FOR ZIF_CARGA~ST_TP_FRETE_DDU .
    ALIASES ST_TP_FRETE_DEQ
      FOR ZIF_CARGA~ST_TP_FRETE_DEQ .
    ALIASES ST_TP_FRETE_DES
      FOR ZIF_CARGA~ST_TP_FRETE_DES .
    ALIASES ST_TP_FRETE_EXW
      FOR ZIF_CARGA~ST_TP_FRETE_EXW .
    ALIASES ST_TP_FRETE_FAS
      FOR ZIF_CARGA~ST_TP_FRETE_FAS .
    ALIASES ST_TP_FRETE_FCA
      FOR ZIF_CARGA~ST_TP_FRETE_FCA .
    ALIASES ST_TP_FRETE_FOB
      FOR ZIF_CARGA~ST_TP_FRETE_FOB .
    ALIASES ADD_NOTA_FISCAL
      FOR ZIF_CARGA~ADD_NOTA_FISCAL .
    ALIASES BLOQUEAR_ENTRADA
      FOR ZIF_CARGA~BLOQUEAR_ENTRADA .
    ALIASES DESBLOQUEAR_ENTRADA
      FOR ZIF_CARGA~DESBLOQUEAR_ENTRADA .
    ALIASES EXCLUIR_NOTA_FISCAL
      FOR ZIF_CARGA~EXCLUIR_NOTA_FISCAL .
    ALIASES EXCLUIR_REGISTRO
      FOR ZIF_CARGA~EXCLUIR_REGISTRO .
    ALIASES FREE
      FOR ZIF_CARGA~FREE .
    ALIASES GERA_ERRO_GERAL
      FOR ZIF_CARGA~GERA_ERRO_GERAL .
    ALIASES GET_CALCULAR_SUBTOTAL
      FOR ZIF_CARGA~GET_CALCULAR_SUBTOTAL .
    ALIASES GET_CHECK_JOB_EXECUCAO
      FOR ZIF_CARGA~GET_CHECK_JOB_EXECUCAO .
    ALIASES GET_CHECK_JOB_EXECUCAO_ESTORNO
      FOR ZIF_CARGA~GET_CHECK_JOB_EXECUCAO_ESTORNO .
    ALIASES GET_FACTORY_TP_TRANSGENIA
      FOR ZIF_CARGA~GET_FACTORY_TP_TRANSGENIA .
    ALIASES GET_INFO_ALV_APRESENTACAO
      FOR ZIF_CARGA~GET_INFO_ALV_APRESENTACAO .
    ALIASES GET_INFO_ALV_APRESENTACAO_LOG
      FOR ZIF_CARGA~GET_INFO_ALV_APRESENTACAO_LOG .
    ALIASES GET_INFO_MESSAGEM_ESTORNO
      FOR ZIF_CARGA~GET_INFO_MESSAGEM_ESTORNO .
    ALIASES GET_INFO_ORDEM_VENDA
      FOR ZIF_CARGA~GET_INFO_ORDEM_VENDA .
    ALIASES GET_INFO_PLACA
      FOR ZIF_CARGA~GET_INFO_PLACA .
    ALIASES GET_LOGS_HISTORICO
      FOR ZIF_CARGA~GET_LOGS_HISTORICO .
    ALIASES GET_MENS_INTERFACE_ENTRADA
      FOR ZIF_CARGA~GET_MENS_INTERFACE_ENTRADA .
    ALIASES GET_NEW_ID_CARGA
      FOR ZIF_CARGA~GET_NEW_ID_CARGA .
    ALIASES GET_NEW_ID_CLASSIFICAO
      FOR ZIF_CARGA~GET_NEW_ID_CLASSIFICAO .
    ALIASES GET_NEW_ID_ENTRADA_ESTOQUE
      FOR ZIF_CARGA~GET_NEW_ID_ENTRADA_ESTOQUE .
    ALIASES GET_NOTA_FORNECEDOR_IE
      FOR ZIF_CARGA~GET_NOTA_FORNECEDOR_IE .
    ALIASES GET_PARTINER_ORDEM_VENDA
      FOR ZIF_CARGA~GET_PARTINER_ORDEM_VENDA .
    ALIASES GET_PEDIDO_COMPRA
      FOR ZIF_CARGA~GET_PEDIDO_COMPRA .
    ALIASES GET_RATEIA_DESCONTOS
      FOR ZIF_CARGA~GET_RATEIA_DESCONTOS .
    ALIASES GET_REGISTRO
      FOR ZIF_CARGA~GET_REGISTRO .
    ALIASES GET_RESULT_DESC_CLASSIFICACAO
      FOR ZIF_CARGA~GET_RESULT_DESC_CLASSIFICACAO .
    ALIASES GET_ROMANEIO_ENTRADA
      FOR ZIF_CARGA~GET_ROMANEIO_ENTRADA .
    ALIASES GET_ROMANEIO_SAIDA
      FOR ZIF_CARGA~GET_ROMANEIO_SAIDA .
    ALIASES GET_TP_STATUS
      FOR ZIF_CARGA~GET_TP_STATUS .
    ALIASES GRAVAR_REGISTRO
      FOR ZIF_CARGA~GRAVAR_REGISTRO .
    ALIASES LIMPAR_REGISTRO
      FOR ZIF_CARGA~LIMPAR_REGISTRO .
    ALIASES NOVO_REGISTRO
      FOR ZIF_CARGA~NOVO_REGISTRO .
    ALIASES PESQUISAR
      FOR ZIF_CARGA~PESQUISAR .
    ALIASES SET_ABRIR
      FOR ZIF_CARGA~SET_ABRIR .
    ALIASES SET_CANCELAR
      FOR ZIF_CARGA~SET_CANCELAR .
    ALIASES SET_CARGA
      FOR ZIF_CARGA~SET_CARGA .
    ALIASES SET_CONFERIDO
      FOR ZIF_CARGA~SET_CONFERIDO .
    ALIASES SET_DENQUEUE
      FOR ZIF_CARGA~SET_DENQUEUE .
    ALIASES SET_ENQUEUE
      FOR ZIF_CARGA~SET_ENQUEUE .
    ALIASES SET_FECHAR
      FOR ZIF_CARGA~SET_FECHAR .
    ALIASES SET_GERAR_ENTRADA_ESTOQUE
      FOR ZIF_CARGA~SET_GERAR_ENTRADA_ESTOQUE .
    ALIASES SET_GERAR_ESTORNO_ESTOQUE
      FOR ZIF_CARGA~SET_GERAR_ESTORNO_ESTOQUE .
    ALIASES SET_GERAR_ROMANEIO_ENTRADA
      FOR ZIF_CARGA~SET_GERAR_ROMANEIO_ENTRADA .
    ALIASES SET_GERAR_ROMANEIO_SAIDA
      FOR ZIF_CARGA~SET_GERAR_ROMANEIO_SAIDA .
    ALIASES SET_ID_CARGA
      FOR ZIF_CARGA~SET_ID_CARGA .
    ALIASES SET_ID_CLASSIFICACAO
      FOR ZIF_CARGA~SET_ID_CLASSIFICACAO .
    ALIASES SET_LOGS_ALTERACAO
      FOR ZIF_CARGA~SET_LOGS_ALTERACAO .
    ALIASES SET_ORDEM_CARREGAMENTO
      FOR ZIF_CARGA~SET_ORDEM_CARREGAMENTO .
    ALIASES SET_ORDEM_VENDA
      FOR ZIF_CARGA~SET_ORDEM_VENDA .
    ALIASES SET_PESOS_NOTAS
      FOR ZIF_CARGA~SET_PESOS_NOTAS .
    ALIASES SET_PROCESSAR_ENTRADA
      FOR ZIF_CARGA~SET_PROCESSAR_ENTRADA .
    ALIASES SET_PROCESSAR_ESTORNO
      FOR ZIF_CARGA~SET_PROCESSAR_ESTORNO .
    ALIASES SET_REGISTRO
      FOR ZIF_CARGA~SET_REGISTRO .
    ALIASES VALIDAR_EXCLUSAO
      FOR ZIF_CARGA~VALIDAR_EXCLUSAO .
    ALIASES VALIDAR_REGISTRO
      FOR ZIF_CARGA~VALIDAR_REGISTRO .
    ALIASES VALIDA_ATRIBUTO_ALTERAVEL
      FOR ZIF_CARGA~VALIDA_ATRIBUTO_ALTERAVEL .
    ALIASES VERIF_ESTORNO_PENDENTE
      FOR ZIF_CARGA~VERIF_ESTORNO_PENDENTE .
    ALIASES VERIF_ORDEM_CARREGAMENTO
      FOR ZIF_CARGA~VERIF_ORDEM_CARREGAMENTO .
ENDCLASS.



CLASS ZCL_CARGA_RECEBIMENTO_V0001 IMPLEMENTATION.


  METHOD zif_carga~add_nota_fiscal.

    DATA: valor               TYPE i,
          lc_nota             TYPE zsdt0001nt,
          wa_nota             TYPE j_1bnfdoc,
          lc_nr_total_entrada TYPE zde_nr_quantidade,
          lc_nr_total_saida   TYPE zde_nr_quantidade,
          lc_menge            TYPE bstmg.

    DATA: ob_fornecedor TYPE REF TO zcl_fornecedores.


    r_carga = me.

    DESCRIBE TABLE me->documento_fiscal LINES DATA(qtd_notas_entrada).

    lc_nr_total_entrada = 0.

    LOOP AT me->documento_fiscal INTO DATA(wa_fiscal).
      ADD wa_fiscal-nr_quantidade TO lc_nr_total_entrada.
    ENDLOOP.

    DATA: lc_nota_fiscal TYPE zsdt0001nt.
    MOVE-CORRESPONDING i_nota TO lc_nota_fiscal.

    me->zif_carga~get_validar_nota_fiscal( CHANGING i_nota_fiscal = lc_nota_fiscal ).

    "Verificar se o Documento já foi lançado na Carga
    IF lc_nota_fiscal-id_nota IS INITIAL.

      CLEAR: me->zif_carga~ck_executar_reversao_entrada.

      READ TABLE me->documento_fiscal
      WITH KEY id_fornecedor = lc_nota_fiscal-id_fornecedor
               nr_nota       = lc_nota_fiscal-nr_nota
               nm_serie      = lc_nota_fiscal-nm_serie
      TRANSPORTING NO FIELDS.

      IF sy-subrc IS INITIAL.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_nota_fiscal_informada-msgid
                              msgno = zcx_carga=>zcx_nota_fiscal_informada-msgno
                              attr1 = CONV #( lc_nota_fiscal-nr_nota )
                              attr2 = CONV #( lc_nota_fiscal-nm_serie )
                              attr3 = CONV #( lc_nota_fiscal-id_fornecedor ) )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_nota_fiscal_informada-msgno
            msgid  = zcx_carga=>zcx_nota_fiscal_informada-msgid
            msgv1  = CONV #( lc_nota_fiscal-nr_nota )
            msgv2  = CONV #( lc_nota_fiscal-nm_serie )
            msgv3  = CONV #( lc_nota_fiscal-id_fornecedor ).
      ENDIF.
    ELSE.
      LOOP AT me->documento_fiscal TRANSPORTING NO FIELDS
        WHERE id_fornecedor EQ lc_nota_fiscal-id_fornecedor
          AND nr_nota       EQ lc_nota_fiscal-nr_nota
          AND nm_serie      EQ lc_nota_fiscal-nm_serie
          AND id_nota       NE lc_nota_fiscal-id_nota.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_nota_fiscal_informada-msgid msgno = zcx_carga=>zcx_nota_fiscal_informada-msgno
                              attr1 = CONV #( lc_nota_fiscal-nr_nota )
                              attr2 = CONV #( lc_nota_fiscal-nm_serie )
                              attr3 = CONV #( lc_nota_fiscal-id_fornecedor ) )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_nota_fiscal_informada-msgno
            msgid  = zcx_carga=>zcx_nota_fiscal_informada-msgid
            msgv1  = CONV #( lc_nota_fiscal-nr_nota )
            msgv2  = CONV #( lc_nota_fiscal-nm_serie )
            msgv3  = CONV #( lc_nota_fiscal-id_fornecedor ).
      ENDLOOP.
    ENDIF.

    MOVE-CORRESPONDING lc_nota_fiscal TO e_nota.
    IF e_nota-nr_quantidade IS NOT INITIAL AND e_nota-nr_valor IS NOT INITIAL.
      TRY .
          valor = ( e_nota-nr_valor / e_nota-nr_quantidade ) * 1000.
          e_nota-nr_preco_saca_60 = ( valor * 60 ) / 1000.
        CATCH cx_sy_arithmetic_overflow.
          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = VALUE #( msgid = zcx_carga=>zcx_erro_preco_saca-msgid msgno = zcx_carga=>zcx_erro_preco_saca-msgno )
              msgty  = 'E'
              msgno  = zcx_carga=>zcx_erro_preco_saca-msgno
              msgid  = zcx_carga=>zcx_erro_preco_saca-msgid.
      ENDTRY.
    ENDIF.

    DATA(lc_ck_nfe) = COND #( LET clet = i_nota-id_mod_fiscal IN WHEN clet EQ zif_carga=>st_model_fiscal_eletronico THEN abap_true ELSE abap_false ).

    SELECT SINGLE * INTO @DATA(wa_tipo_entrada)
      FROM zsdt0001te
     WHERE id_entrada EQ @i_nota-id_entrada
       AND id_empresa EQ @me->carga-id_bukrs
       AND ck_nfe     EQ @lc_ck_nfe.

    CASE wa_tipo_entrada-id_tp_operacao.
      WHEN zif_tp_operacao_carga_entrada=>st_compra_entrega_futura OR
           zif_tp_operacao_carga_entrada=>st_retorno_armazenagem.

        me->zif_carga~get_pedido_compra_geral(
          EXPORTING
            i_id_bukrs      = me->carga-id_bukrs
            i_id_entrada    = lc_nota_fiscal-id_entrada
            i_id_mod_fiscal = lc_nota_fiscal-id_mod_fiscal
            i_id_branch     = me->carga-id_branch
            i_nr_safra      = me->carga-nr_safra
            i_id_produto    = me->carga-id_produto
            i_id_fornecedor = lc_nota_fiscal-id_fornecedor
          IMPORTING
            e_info_pedido   = DATA(e_info_pedido)
        ).

      WHEN OTHERS.

        TRY .

            me->zif_carga~get_pedido_compra_geral(
              EXPORTING
                i_id_bukrs      = me->carga-id_bukrs
                i_id_entrada    = lc_nota_fiscal-id_entrada
                i_id_mod_fiscal = lc_nota_fiscal-id_mod_fiscal
                i_id_branch     = me->carga-id_branch
                i_nr_safra      = me->carga-nr_safra
                i_id_produto    = me->carga-id_produto
                i_id_fornecedor = lc_nota_fiscal-id_fornecedor
              IMPORTING
                e_info_pedido   = e_info_pedido
            ).

          CATCH zcx_carga INTO DATA(ex_carga).
            IF ex_carga->msgid EQ zcx_carga=>zcx_sem_pedido_compra-msgid AND
               ex_carga->msgno EQ zcx_carga=>zcx_sem_pedido_compra-msgno AND
               wa_tipo_entrada-id_tp_operacao NE zif_tp_operacao_carga_entrada=>st_compra_entrega_futura.

              TRY.


                  SELECT SINGLE * INTO @DATA(wa_zsdt0001tetn)
                    FROM zsdt0001tetn
                   WHERE id_entrada EQ @wa_tipo_entrada-id_entrada.

                  IF sy-subrc IS INITIAL.

                    "Buscar Grupo de Mercadoria
                    SELECT SINGLE * INTO @DATA(wa_zsdt0001mt)
                      FROM zsdt0001mt
                     WHERE matnr EQ @me->carga-id_produto.

                    IF sy-subrc IS INITIAL.

                      "Buscar Iva por Tipo de Nota Fiscal / Grupo de Mercadoria / Empresa
                      SELECT SINGLE * INTO @DATA(wa_zsdt0001tetniva)
                        FROM zsdt0001tetniva
                       WHERE id_empresa   EQ @me->carga-id_bukrs
                         AND id_tipo_nota EQ @wa_zsdt0001tetn-id_tipo_nota
                         AND tp_grupo_ctb EQ @wa_zsdt0001mt-tp_grupo_ctb.

                      IF sy-subrc IS INITIAL.
                        wa_tipo_entrada-id_iva = wa_zsdt0001tetniva-id_iva.
                      ENDIF.
                    ENDIF.

                  ENDIF.

*-CS2022000332-#78064-07.06.2022-JT-inicio
                  IF me->zif_carga~carga-id_contrato IS INITIAL.
                    lc_menge = 400000000.    " Quantidade do pedido
                  ELSE.
                    SELECT SINGLE quatidade
                      INTO lc_menge
                      FROM zsdt0143
                     WHERE id_contrato = me->zif_carga~carga-id_contrato.
                    IF sy-subrc <> 0.
                      lc_menge = 400000000.    " Quantidade do pedido
                    ENDIF.
                  ENDIF.
*-CS2022000332-#78064-07.06.2022-JT-fim

                  zcl_pedido_compra=>set_criar_pedido_compra(
                    EXPORTING
                      i_bedat      = lc_nota_fiscal-dt_emissao    " Data do documento de compra
                      i_ekorg      = 'OC01'    " Organização de compras
                      i_ekgrp      = 'G01'    " Grupo de compradores
                      i_waers      = 'BRL'    " Código da moeda
                      i_bsart      = 'ZGR '   " Tipo de documento de compras
                      i_zterm      = 'Z001'    " Chave de condições de pagamento
                      i_lifnr      = lc_nota_fiscal-id_fornecedor " Nº conta do fornecedor
                      i_bukrs      = me->carga-id_bukrs    " Empresa
                      i_charg      = CONV #( me->carga-nr_safra )    " Número do lote
                      i_eindt      = sy-datum    " Data de remessa do item
                      i_mwskz      = wa_tipo_entrada-id_iva    " Código do IVA
                      i_menge      = lc_menge  "400000000    " Quantidade do pedido *-CS2022000332-#78064-07.06.2022-JT-inicio
                      i_matnr      = me->carga-id_produto   " Nº do material
                      i_meins      = 'KG'    " Unidade de medida do pedido
                      i_werks      = me->carga-id_branch    " Centro
                      i_tp_centro  = zcl_pedido_compra=>st_tp_centro_a_fixar
                      i_collect_no = me->zif_carga~carga-id_contrato      "*-CS2022000332-#78064-07.06.2022-JT-inicio
                  ).

                CATCH zcx_job INTO DATA(ex_job).

                  RAISE EXCEPTION TYPE zcx_carga
                    EXPORTING
                      textid = VALUE #( msgid = ex_job->msgid msgno = ex_job->msgno
                                        attr1 = CONV #( ex_job->msgv1 )
                                        attr2 = CONV #( ex_job->msgv2 )
                                        attr3 = CONV #( ex_job->msgv3 )
                                        attr4 = CONV #( ex_job->msgv4 ) )
                      msgid  = ex_job->msgid
                      msgno  = ex_job->msgno
                      msgty  = 'E'
                      msgv1  = ex_job->msgv1
                      msgv2  = ex_job->msgv2
                      msgv3  = ex_job->msgv3
                      msgv4  = ex_job->msgv4.

                CATCH zcx_pedido_compra_exception INTO DATA(ex_pedido).

                  RAISE EXCEPTION TYPE zcx_carga
                    EXPORTING
                      textid = VALUE #( msgid = ex_pedido->msgid msgno = ex_pedido->msgno
                                        attr1 = CONV #( ex_pedido->msgv1 )
                                        attr2 = CONV #( ex_pedido->msgv2 )
                                        attr3 = CONV #( ex_pedido->msgv3 )
                                        attr4 = CONV #( ex_pedido->msgv4 ) )
                      msgid  = ex_pedido->msgid
                      msgno  = ex_pedido->msgno
                      msgty  = 'E'
                      msgv1  = ex_pedido->msgv1
                      msgv2  = ex_pedido->msgv2
                      msgv3  = ex_pedido->msgv3
                      msgv4  = ex_pedido->msgv4.

                CATCH zcx_cadastro INTO DATA(ex_cadastro).

                  RAISE EXCEPTION TYPE zcx_carga
                    EXPORTING
                      textid = VALUE #( msgid = ex_cadastro->msgid msgno = ex_cadastro->msgno
                                        attr1 = CONV #( ex_cadastro->msgv1 )
                                        attr2 = CONV #( ex_cadastro->msgv2 )
                                        attr3 = CONV #( ex_cadastro->msgv3 )
                                        attr4 = CONV #( ex_cadastro->msgv4 ) )
                      msgid  = ex_cadastro->msgid
                      msgno  = ex_cadastro->msgno
                      msgty  = 'E'
                      msgv1  = ex_cadastro->msgv1
                      msgv2  = ex_cadastro->msgv2
                      msgv3  = ex_cadastro->msgv3
                      msgv4  = ex_cadastro->msgv4.

              ENDTRY.

              WAIT UP TO 5 SECONDS.

              me->zif_carga~get_pedido_compra_geral(
                EXPORTING
                  i_id_bukrs      = me->carga-id_bukrs
                  i_id_entrada    = lc_nota_fiscal-id_entrada
                  i_id_mod_fiscal = lc_nota_fiscal-id_mod_fiscal
                  i_id_branch     = me->carga-id_branch
                  i_nr_safra      = me->carga-nr_safra
                  i_id_produto    = me->carga-id_produto
                  i_id_fornecedor = lc_nota_fiscal-id_fornecedor
                IMPORTING
                  e_info_pedido   = e_info_pedido
              ).

            ELSE.
              RAISE EXCEPTION TYPE zcx_carga
                EXPORTING
                  textid = VALUE #( msgid = ex_carga->msgid msgno = ex_carga->msgno
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
            ENDIF.
        ENDTRY.

    ENDCASE.

    lc_nota_fiscal-po_number = e_info_pedido-ekpo-ebeln.
    lc_nota_fiscal-po_item   = e_info_pedido-ekpo-ebelp.

    IF lc_nota_fiscal-id_nota IS INITIAL.

      DESCRIBE TABLE me->documento_fiscal LINES DATA(lc_item).
      ADD 1 TO lc_item.
      e_nota-id_nota = lc_item.
      e_nota-po_number = e_info_pedido-ekpo-ebeln.
      e_nota-po_item   = e_info_pedido-ekpo-ebelp.
      MOVE-CORRESPONDING e_nota TO lc_nota.
      lc_nota-id_carga = me->carga-id_carga.
      e_nota-id_carga = me->carga-id_carga.
      APPEND lc_nota TO me->documento_fiscal.
      me->zif_carga~set_gera_imposto_nota( EXPORTING i_nota = lc_nota ).

      SELECT SINGLE maktx
        INTO e_nota-ds_produto
        FROM makt WHERE matnr EQ e_nota-id_produto
         AND spras EQ sy-langu.

      "090  Nota Fiscal Incluída!
      "091  Nota Fiscal Modificada!

      IF e_nota-id_entregue_por IS NOT INITIAL.
        SELECT SINGLE name1
          INTO e_nota-ds_entregue_por
          FROM lfa1 WHERE lifnr EQ e_nota-id_entregue_por.
      ENDIF.

      "Tipo de Entrada
      SELECT SINGLE ds_entrada
        INTO e_nota-ds_entrada
        FROM zsdt0001tetx
       WHERE id_entrada EQ e_nota-id_entrada.

      MESSAGE s090.

    ELSE.
      READ TABLE me->documento_fiscal ASSIGNING FIELD-SYMBOL(<fs_nota>) WITH KEY id_nota = lc_nota_fiscal-id_nota.

      e_nota-po_number = e_info_pedido-ekpo-ebeln.
      e_nota-po_item   = e_info_pedido-ekpo-ebelp.
      MOVE-CORRESPONDING e_nota TO <fs_nota>.

      e_nota-id_carga = me->carga-id_carga.
      <fs_nota>-id_carga = me->carga-id_carga.

      me->zif_carga~set_gera_imposto_nota( EXPORTING i_nota = <fs_nota> ).

      SELECT SINGLE name1
        INTO e_nota-ds_fornecedor
        FROM lfa1 WHERE lifnr EQ e_nota-id_fornecedor.

      SELECT SINGLE maktx
        INTO e_nota-ds_produto
        FROM makt WHERE matnr EQ e_nota-id_produto
         AND spras EQ sy-langu.

      SELECT SINGLE ds_entrada
        INTO e_nota-ds_entrada
        FROM zsdt0001tetx
       WHERE id_entrada EQ e_nota-id_entrada.

      IF e_nota-id_entregue_por IS NOT INITIAL.
        SELECT SINGLE name1
          INTO e_nota-ds_entregue_por
          FROM lfa1 WHERE lifnr EQ e_nota-id_entregue_por.
      ENDIF.

      MESSAGE s091.

    ENDIF.

    DESCRIBE TABLE me->documento_fiscal LINES DATA(qtd_notas_saida).

    lc_nr_total_saida = 0.

    LOOP AT me->documento_fiscal INTO wa_fiscal.
      ADD wa_fiscal-nr_quantidade TO lc_nr_total_saida.
    ENDLOOP.

    "Recalcula Distribuição de Pesos de Desconto
    IF lc_nr_total_saida NE lc_nr_total_entrada OR
       qtd_notas_saida NE qtd_notas_entrada.

      me->zif_carga~set_ajustar_rat_desc_geral( ).

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~ADD_ORDEM_VENDA.
    R_INSTANCIA = ME.
  ENDMETHOD.


  METHOD ZIF_CARGA~BLOQUEAR_ENTRADA.

    R_CARGA = ME.

    CHECK ME->ZIF_CARGA~AT_NAO_GERAR_BLOQUEIOS EQ ABAP_FALSE.

    CALL FUNCTION 'ENQUEUE_EZMMT001'
      EXPORTING
        MODE_ZMMT_EE_ZGR = 'S'
        MANDT            = SY-MANDT
        OBJ_KEY          = I_OBJ_KEY
      EXCEPTIONS
        FOREIGN_LOCK     = 1
        SYSTEM_FAILURE   = 2
        OTHERS           = 3.

    IF SY-SUBRC IS NOT INITIAL.

      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGTY  = 'E'
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~DESBLOQUEAR_ENTRADA.

    R_CARGA = ME.

    CHECK ME->ZIF_CARGA~AT_NAO_GERAR_BLOQUEIOS EQ ABAP_FALSE.

    CALL FUNCTION 'DEQUEUE_EZMMT001'
      EXPORTING
        MODE_ZMMT_EE_ZGR = 'S'
        MANDT            = SY-MANDT
        OBJ_KEY          = I_OBJ_KEY.

  ENDMETHOD.


  METHOD zif_carga~desvincular_romaneio_carga.

    DATA:
      lc_retorno TYPE zde_opus_carga.

    DATA: ob_web_service TYPE REF TO zcl_webservice.

    AUTHORITY-CHECK OBJECT 'ZMM0127_V2' ID 'ZACAO_0001' FIELD '01'.

    IF sy-subrc IS NOT INITIAL.
      zcl_carga_recebimento_v0001=>zif_carga~gera_erro_geral( i_texto = 'Sem autorização para Desvincular romaneio!' ).
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0001 INTO @DATA(lwa_zsdt0001_saida)
     WHERE ch_referencia EQ @i_ch_referencia.

    CHECK sy-subrc EQ 0 AND lwa_zsdt0001_saida-tp_movimento EQ 'S'.

    IF lwa_zsdt0001_saida-nro_nf_prod IS INITIAL.
      zcl_carga_recebimento_v0001=>zif_carga~gera_erro_geral( i_texto = 'Romaneio de Saida não possui NF autorizada!' ).
      RETURN.
    ENDIF.


    CREATE OBJECT ob_web_service.

*----------------------------------------------------------------------
* Gerar token
*----------------------------------------------------------------------
    ob_web_service->zif_webservice~autentica_opus = abap_true.

*----------------------------------------------------------------------
* Setar variáveis
*----------------------------------------------------------------------
    TRY .
        ob_web_service->set_servico( i_servico = 'RD' ).
        ob_web_service->set_tipo( i_tipo = 'O' ).
      CATCH zcx_webservice INTO DATA(lc_exception).
    ENDTRY.

*----------------------------------------------------------------------
* Preparar dados do Envio
*----------------------------------------------------------------------
    DATA: gw_webservice TYPE zciot_webservice. "Tabela de WebServices para Viagem Adiministradora
    DATA: var_servico TYPE ztipowebserv, "Tipo de Serviço WebService
          var_tipo    TYPE ztipowebadm, "Tipo de WebService
          var_url     TYPE string. "Endereço do WebService.

    TRY.

        CLEAR: var_servico, var_tipo.

*----------------------------------------------------------------------
* Verificar variáveis setadas variáveis
*----------------------------------------------------------------------
        var_servico =  ob_web_service->get_servico( ). "Atribuir o Serviço que vai ser selecionado.
        var_tipo    =  ob_web_service->get_tipo( ).    "Atribuir o Tipo de Serviço que vai ser selecionado.

        IF NOT ( var_servico IS INITIAL ) AND NOT ( var_tipo IS INITIAL ).

          "Selecionar na tabela o serviço e tipo de serviço que vai ser utilizado.
          SELECT SINGLE * FROM zciot_webservice INTO gw_webservice WHERE servico EQ var_servico
                                                                     AND tipo    EQ var_tipo.

          IF ( sy-subrc EQ 0 ). "Caso o endereço seja encontrado.

            var_url = gw_webservice-url && '/' && i_ch_referencia.

            ob_web_service->at_url       = var_url.
            ob_web_service->at_url_token = gw_webservice-url_token.

            "O retorno é um HTTP Client.
            cl_http_client=>create_by_url(
                                           EXPORTING url    = var_url
*                                                     ssl_id = i_ssl_id
                                           IMPORTING client = DATA(var_http)
                                           EXCEPTIONS argument_not_found = 1
                                                      plugin_not_active  = 2
                                                      internal_error     = 3
                                         ).
            CASE sy-subrc.
              WHEN: 1.
                "Este erro lança uma exception de "Argumento não encontrado" Caso a URL não seja informada.
                "na passagem de parametro da CREATE_BY_URL.
                RAISE EXCEPTION TYPE zcx_carga EXPORTING textid = zcx_webservice=>argumento_nao_encontrado.
              WHEN: 2.
                "Este erro lança uma expcetion de "Plugin não ativo", quando a comunicação HTTP não esteja ativa no SAP.
                RAISE EXCEPTION TYPE zcx_carga EXPORTING textid = zcx_webservice=>plugin_nao_ativo.
              WHEN: 3.
                "Este erro lança uma exception generica "Erro Interno" do SAP.
                RAISE EXCEPTION TYPE zcx_carga EXPORTING textid = zcx_webservice=>erro_interno.
            ENDCASE.

          ELSE.
            "Quando uma URL não for encontrada no cadastro da ZLES0096 (tabela: ZCIOT_WEBSERVICE)
            "Este erro vai lançar uma exception informando ao sistema que não foi encontrado o endereço URL.
            RAISE EXCEPTION TYPE zcx_carga EXPORTING textid = zcx_webservice=>url_nao_encontrado.
          ENDIF.
        ENDIF.


*       "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
        DATA(lc_uri) = ob_web_service->get_uri(  ).

      CATCH zcx_webservice INTO lc_exception.
    ENDTRY.

*----------------------------------------------------------------------
* Enviar dados ao Legado
*----------------------------------------------------------------------
    ob_web_service->zif_webservice~abrir_conexao( i_http = var_http ).

    ob_web_service->zif_webservice~consultar(
      EXPORTING
        i_http                     = var_http
      IMPORTING
        e_code                     = DATA(l_code)
        e_reason                   = DATA(e_reason)
      RECEIVING
        e_resultado                = DATA(json_retorno)
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgty  = 'E'
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.

    CASE l_code.
      WHEN '200'.
        "Não retorna mensagem programa
      WHEN abap_false.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_sem_retorno_webservice-msgid
                              msgno = zcx_carga=>zcx_sem_retorno_webservice-msgno
                              attr1 = CONV #( lc_uri ) )
            msgid  = zcx_carga=>zcx_sem_retorno_webservice-msgid
            msgno  = zcx_carga=>zcx_sem_retorno_webservice-msgno
            msgv1  = CONV #( lc_uri ).
      WHEN OTHERS.
        IF lc_retorno-message IS NOT INITIAL.
          CONCATENATE 'OPUS:' lc_retorno-message INTO DATA(lc_texto_code) SEPARATED BY space.
        ELSE.
          CONCATENATE 'OPUS:' e_reason INTO lc_texto_code SEPARATED BY space.
        ENDIF.
        zcl_carga_recebimento_v0001=>zif_carga~gera_erro_geral( EXPORTING i_texto = lc_texto_code ).
    ENDCASE.



  ENDMETHOD.


  method zif_carga~excluir_nota_fiscal.

    data: valor    type i,
          vid_nota type zsdt0001nt-id_nota,
          wa_notas type zde_zsdt0001nt_alv.

    r_carga = me.

    clear: e_notas.

    check i_nota-id_nota is not initial.

    vid_nota = i_nota-id_nota.
    if me->zif_carga~at_manutencao eq abap_true and i_nota-id_nota eq 1.

      loop at me->documento_fiscal assigning field-symbol(<fs_notas>).

        move-corresponding <fs_notas> to wa_notas.

        add 1 to valor.

        select single name1
          into wa_notas-ds_fornecedor
          from lfa1
         where lifnr eq wa_notas-id_fornecedor.

        select single maktx
          into wa_notas-ds_produto
          from makt
         where matnr eq wa_notas-id_produto
           and spras eq sy-langu.

        append wa_notas to e_notas.

      endloop.

      raise exception type zcx_carga
        exporting
          textid = value #( msgid = zcx_carga=>zcx_first_rom_manu_exclui-msgid
                            msgno = zcx_carga=>zcx_first_rom_manu_exclui-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_first_rom_manu_exclui-msgno
          msgid  = zcx_carga=>zcx_first_rom_manu_exclui-msgid.
    endif.

    "US 143677 - transgenia por nota
    if vid_nota ne 1. " Nota 1 tem a classificacao principal
      if i_nota-id_classificacao is initial.
        read table me->classificacao_notas into data(w_fiscal)  with key id_classificacao = i_nota-nr_nota. "Antes de Gravar
        if sy-subrc = 0.
          delete me->classificacao_notas where id_classificacao = w_fiscal-id_classificacao.
        endif.
      else.
        delete me->classificacao_notas where id_classificacao = i_nota-id_classificacao."depois de gravar
      endif.
    endif.
    "US 143677 - transgenia por nota


    delete me->documento_fiscal where id_nota = i_nota-id_nota.
    delete me->documento_fiscal_imp_ret where id_nota = i_nota-id_nota.

    if sy-subrc is initial.
      valor = 1.
      loop at me->documento_fiscal assigning <fs_notas>.

        <fs_notas>-id_nota = valor.
        move-corresponding <fs_notas> to wa_notas.

        add 1 to valor.

        select single name1
          into wa_notas-ds_fornecedor
          from lfa1
         where lifnr eq wa_notas-id_fornecedor.

        select single maktx
          into wa_notas-ds_produto
          from makt
         where matnr eq wa_notas-id_produto
           and spras eq sy-langu.

        "US 143677 - transgenia por nota ( classificacao exibida na tela )
        wa_notas-in_gmo_nt                = me->classificacao-in_gmo.
        wa_notas-nr_resultado_01_nt	      = me->classificacao-nr_resultado_01.
        wa_notas-nr_resultado_02_nt	      = me->classificacao-nr_resultado_02.
        wa_notas-nr_res_rr1_rr2_nt        = me->classificacao-nr_res_rr1_rr2.
        wa_notas-in_gmo_03_nt	            = me->classificacao-in_gmo_03.
        wa_notas-in_srr_origem_partic_nt  = me->classificacao-in_srr_origem_partic.
        wa_notas-id_outro_partic_nt	      = me->classificacao-id_outro_partic.
        wa_notas-in_srr_declarado_nt      = me->classificacao-in_srr_declarado.
        wa_notas-in_teste_srr_nt          = me->classificacao-in_teste_srr.
        wa_notas-in_srr_declarado_2_nt    = me->classificacao-in_srr_declarado_2.
        wa_notas-in_teste_srr_2_nt        = me->classificacao-in_teste_srr_2.
        wa_notas-id_classificadora_nt	    = me->classificacao-id_classificadora.
        wa_notas-tp_transgenia_nt	        = me->classificacao-tp_transgenia.

        if vid_nota eq 1. " Nota 1 tem a classificacao principal
          read table me->classificacao_notas into me->classificacao with key id_classificacao = <fs_notas>-id_classificacao.
          if sy-subrc = 0.
            "classificacao exibida na tela sera a da  nota 1
            wa_notas-in_gmo_nt                = me->classificacao-in_gmo.
            wa_notas-nr_resultado_01_nt	      = me->classificacao-nr_resultado_01.
            wa_notas-nr_resultado_02_nt	      = me->classificacao-nr_resultado_02.
            wa_notas-nr_res_rr1_rr2_nt        = me->classificacao-nr_res_rr1_rr2.
            wa_notas-in_gmo_03_nt	            = me->classificacao-in_gmo_03.
            wa_notas-in_srr_origem_partic_nt  = me->classificacao-in_srr_origem_partic.
            wa_notas-id_outro_partic_nt	      = me->classificacao-id_outro_partic.
            wa_notas-in_srr_declarado_nt      = me->classificacao-in_srr_declarado.
            wa_notas-in_teste_srr_nt          = me->classificacao-in_teste_srr.
            wa_notas-in_srr_declarado_2_nt    = me->classificacao-in_srr_declarado_2.
            wa_notas-in_teste_srr_2_nt        = me->classificacao-in_teste_srr_2.
            wa_notas-id_classificadora_nt	    = me->classificacao-id_classificadora.
            wa_notas-tp_transgenia_nt	        = me->classificacao-tp_transgenia.
            vid_nota = <fs_notas>-id_nota.
            wa_notas-id_nota   = 1.
            <fs_notas>-id_nota = 1.
            me->carga-id_classificacao = <fs_notas>-id_classificacao.
            delete  me->classificacao_notas where id_classificacao = <fs_notas>-id_classificacao.
          endif.
        endif.
        "US 143677 - transgenia por nota

        append wa_notas to e_notas.

      endloop.

      loop at me->documento_fiscal into data(wa_nota).
        me->zif_carga~set_gera_imposto_nota( exporting i_nota = wa_nota ).
      endloop.

      message s092.
    endif.

  endmethod.


  METHOD ZIF_CARGA~EXCLUIR_REGISTRO.

    E_EXCLUIU = ABAP_FALSE.

    R_CARGA = ME.

  ENDMETHOD.


  METHOD ZIF_CARGA~FREE.

    R_CARGA = ME.

    IF ME->CARGA-ID_CARGA IS NOT INITIAL.
      ME->SET_DENQUEUE( I_CARGA = ME->CARGA-ID_CARGA ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~GERAR_CARGA_SAIDA.

  ENDMETHOD.


  METHOD ZIF_CARGA~GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_CARGA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_CARGA=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_CARGA=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_CARGA=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_CABECALHO_CARGA.

    R_CARGA = ME.

    SELECT SINGLE * INTO E_ZSDT0001CG FROM ZSDT0001CG WHERE ID_CARGA EQ I_ID_CARGA.
    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_CARGA_NAO_ENCONTRADA-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_CARGA_NAO_ENCONTRADA-MSGNO
                            ATTR1 = CONV #( I_ID_CARGA ) )
          MSGTY  = 'E'
          MSGNO  = ZCX_CARGA=>ZCX_CARGA_NAO_ENCONTRADA-MSGNO
          MSGID  = ZCX_CARGA=>ZCX_CARGA_NAO_ENCONTRADA-MSGID
          MSGV1  = CONV #( I_ID_CARGA ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_CALCULAR_SUBTOTAL.
    R_CARGA = ME.
    E_PESO_SUBTOTAL = I_PESO_BRUTO - I_PESO_TARA.
  ENDMETHOD.


  METHOD ZIF_CARGA~GET_CHECK_JOB_EXECUCAO.

    R_CARGA = ME.

    TRY .
        ZCL_JOB=>GET_JOB_EXECUCAO( I_JOB_NAME = ZCL_JOB=>ST_NAME_JOB_ENTRADA_ESTOQUE ).

        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_JOB_ENTRADA_EXEC-MSGID MSGNO = ZCX_CARGA=>ZCX_JOB_ENTRADA_EXEC-MSGNO )
            MSGTY  = 'E'
            MSGID  = ZCX_CARGA=>ZCX_JOB_ENTRADA_EXEC-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_JOB_ENTRADA_EXEC-MSGNO.

      CATCH ZCX_JOB.    "

    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_CHECK_JOB_EXECUCAO_ESTORNO.

    R_CARGA = ME.

    TRY .
        ZCL_JOB=>GET_JOB_EXECUCAO( I_JOB_NAME = ZCL_JOB=>ST_NAME_JOB_ESTORNO_ESTOQUE ).

        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_JOB_ESTORNO_EXEC-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_JOB_ESTORNO_EXEC-MSGNO )
            MSGTY  = 'E'
            MSGID  = ZCX_CARGA=>ZCX_JOB_ESTORNO_EXEC-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_JOB_ESTORNO_EXEC-MSGNO.

      CATCH ZCX_JOB.    "

    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_CK_CARGA_SEM_SOLIC_MANUT.

    IF ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_TRUE.

      DATA: RG_STATUS      TYPE RANGE OF ZDE_ST_SOL_AJUSTE.
      DATA: RG_SOLICITACAO TYPE RANGE OF ZDE_ID_SOL_AJUSTE.

      IF ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-ID_SOLICITACAO IS NOT INITIAL.
        RG_SOLICITACAO = VALUE #( SIGN = 'I' OPTION = 'NE'
                                   ( LOW = ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-ID_SOLICITACAO HIGH = ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-ID_SOLICITACAO ) ).
      ENDIF.

      RG_STATUS = VALUE #( SIGN = 'I' OPTION = 'EQ'
                            ( LOW = ZIF_CARGA=>ST_STATUS_MANUT_ABERTO  HIGH = ZIF_CARGA=>ST_STATUS_MANUT_ABERTO  )
                            ( LOW = ZIF_CARGA=>ST_STATUS_MANUT_ENVIADO HIGH = ZIF_CARGA=>ST_STATUS_MANUT_ENVIADO ) ).

      "2  Aprovado
      "3  Recusado
      "4  Cancelado

      SELECT SINGLE
             B~DT_SOLICITACAO,
             B~US_SOLICITACAO
        INTO @DATA(WA_ZSDT0001ACB)
        FROM ZSDT0001ACB AS B
       INNER JOIN ZSDT0001ACG  AS A ON A~ID_SOLICITACAO EQ B~ID_SOLICITACAO
       WHERE B~ID_SOLICITACAO  IN @RG_SOLICITACAO
         AND B~ID_SOLICITACAO  EQ A~ID_SOLICITACAO
         AND B~TP_STATUS       IN @RG_STATUS
         AND A~ID_CARGA_ORIGEM EQ @ME->CARGA-ID_CARGA.

      IF SY-SUBRC IS INITIAL.

        DATA: LC_DATE_EXTERNAL TYPE C LENGTH 10.

        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            DATE_INTERNAL            = WA_ZSDT0001ACB-DT_SOLICITACAO
          IMPORTING
            DATE_EXTERNAL            = LC_DATE_EXTERNAL
          EXCEPTIONS
            DATE_INTERNAL_IS_INVALID = 1
            OTHERS                   = 2.

        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_SOL_MANUT_RESERVADA-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_SOL_MANUT_RESERVADA-MSGNO
                              ATTR1 = CONV #( WA_ZSDT0001ACB-US_SOLICITACAO )
                              ATTR2 = CONV #( LC_DATE_EXTERNAL ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_SOL_MANUT_RESERVADA-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_SOL_MANUT_RESERVADA-MSGID
            MSGV1  = CONV #( WA_ZSDT0001ACB-US_SOLICITACAO )
            MSGV2  = CONV #( LC_DATE_EXTERNAL ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_CK_DOC_TRANSPORTE_EXISTE.

    SELECT SINGLE * INTO @E_VTTK
      FROM VTTK
     WHERE ID_CARGA EQ @ME->CARGA-ID_CARGA.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_CARGA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_DOC_TRANSPORTE_NAO_EXISTE-MSGID
                          MSGNO = ZCX_CARGA=>ZCX_DOC_TRANSPORTE_NAO_EXISTE-MSGNO
                          ATTR1 = CONV #( ME->CARGA-ID_CARGA ) )
        MSGTY  = 'E'
        MSGID  = ZCX_CARGA=>ZCX_DOC_TRANSPORTE_NAO_EXISTE-MSGID
        MSGNO  = ZCX_CARGA=>ZCX_DOC_TRANSPORTE_NAO_EXISTE-MSGNO
        MSGV1  = CONV #( ME->CARGA-ID_CARGA ).

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_CK_EXCLUIR_ROMANEIO_SAIDA.

    DATA: WA_ZSDT0001 TYPE ZSDT0001.

    R_INSTANCIA = ME.

    "CHECK ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_FALSE.

    ME->GET_ROMANEIO_SAIDA( EXPORTING I_ID_CARGA  = ME->CARGA-ID_CARGA IMPORTING E_ROMANEIOS = DATA(ROMANEIOS) ).

    LOOP AT ROMANEIOS INTO DATA(WA_ROMANEIO).

      "Consultar Romaneio
      IF I_CK_OPUS EQ ABAP_TRUE.
        TRY .
            DATA(LC_ROMANEIO) = ZCL_ROMANEIO=>GET_INSTANCE( ).
            LC_ROMANEIO->SET_REGISTRO( I_ID_REGISTRO = WA_ROMANEIO-CH_REFERENCIA ).
            LC_ROMANEIO->GET_REGISTRO( IMPORTING E_REGISTRO = WA_ZSDT0001 ).
            LC_ROMANEIO->GET_CONSULTA_STATUS_OPUS( IMPORTING E_STATUS = DATA(E_STATUS) ).
          CATCH ZCX_CADASTRO INTO DATA(EX_CADASTRO).

            RAISE EXCEPTION TYPE ZCX_CARGA
              EXPORTING
                TEXTID = VALUE #( MSGID = EX_CADASTRO->MSGID
                                  MSGNO = EX_CADASTRO->MSGNO
                                  ATTR1 = EX_CADASTRO->MSGV1
                                  ATTR2 = EX_CADASTRO->MSGV2
                                  ATTR3 = EX_CADASTRO->MSGV3
                                  ATTR4 = EX_CADASTRO->MSGV4 )
                MSGTY  = 'E'
                MSGID  = EX_CADASTRO->MSGID
                MSGNO  = EX_CADASTRO->MSGNO
                MSGV1  = EX_CADASTRO->MSGV1
                MSGV2  = EX_CADASTRO->MSGV2
                MSGV3  = EX_CADASTRO->MSGV3
                MSGV4  = EX_CADASTRO->MSGV4.

          CATCH ZCX_ROMANEIO INTO DATA(EX_ROMANEIO).

            RAISE EXCEPTION TYPE ZCX_CARGA
              EXPORTING
                TEXTID = VALUE #( MSGID = EX_ROMANEIO->MSGID
                                  MSGNO = EX_ROMANEIO->MSGNO
                                  ATTR1 = EX_ROMANEIO->MSGV1
                                  ATTR2 = EX_ROMANEIO->MSGV2
                                  ATTR3 = EX_ROMANEIO->MSGV3
                                  ATTR4 = EX_ROMANEIO->MSGV4 )
                MSGTY  = 'E'
                MSGID  = EX_ROMANEIO->MSGID
                MSGNO  = EX_ROMANEIO->MSGNO
                MSGV1  = EX_ROMANEIO->MSGV1
                MSGV2  = EX_ROMANEIO->MSGV2
                MSGV3  = EX_ROMANEIO->MSGV3
                MSGV4  = EX_ROMANEIO->MSGV4.

        ENDTRY.

        IF E_STATUS EQ ZIF_CARGA=>ST_STATUS_CONFERIDO OR E_STATUS EQ ZIF_CARGA=>ST_STATUS_FECHADO.

          DATA(TX_STATUS) = COND STRING( WHEN E_STATUS EQ ZIF_CARGA=>ST_STATUS_CONFERIDO THEN 'Conferido' ELSE 'Fechado' ).

          "Romaneio de Saída &MSGV1& está &MSGV2& no OPUS!
          RAISE EXCEPTION TYPE ZCX_CARGA
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ROM_STATUS_OPUS-MSGID
                                MSGNO = ZCX_CARGA=>ZCX_ROM_STATUS_OPUS-MSGNO
                                ATTR1 = WA_ZSDT0001-NR_ROMANEIO
                                ATTR2 = TX_STATUS )
              MSGTY  = 'E'
              MSGID  = ZCX_CARGA=>ZCX_ROM_STATUS_OPUS-MSGID
              MSGNO  = ZCX_CARGA=>ZCX_ROM_STATUS_OPUS-MSGNO
              MSGV1  = CONV #( WA_ZSDT0001-NR_ROMANEIO )
              MSGV2  = CONV #( TX_STATUS ).
        ENDIF.

      ENDIF.

      IF WA_ROMANEIO-ST_PROC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ROMANEIO_SAIDA_DOC-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_ROMANEIO_SAIDA_DOC-MSGNO )
            MSGTY  = 'E'
            MSGID  = ZCX_CARGA=>ZCX_ROMANEIO_SAIDA_DOC-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_ROMANEIO_SAIDA_DOC-MSGNO.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_CK_GERA_SOBRA.

    CLEAR: E_ZMMT0074.

    CHECK ME->CARGA-NM_PESO_DESCONTOS IS NOT INITIAL.

    "Verifica se Material Gera Sobra
    SELECT SINGLE * INTO @DATA(WA_SETLEAF)
      FROM SETLEAF
     WHERE SETNAME EQ 'RESIDUO'
       AND VALFROM EQ @ME->CARGA-ID_PRODUTO.

    IF SY-SUBRC IS INITIAL.
      SELECT SINGLE * INTO E_ZMMT0074
        FROM ZMMT0074
       WHERE WERKS       EQ ME->CARGA-ID_BRANCH
         AND MATNR       EQ ME->CARGA-ID_PRODUTO
         AND ENTRADA_ROM EQ 'S'.

      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID    = VALUE #( MSGID  = ZCX_CARGA=>ZCX_PARAM_SOBRA_MATERIAL-MSGID
                                 MSGNO  = ZCX_CARGA=>ZCX_PARAM_SOBRA_MATERIAL-MSGNO
                                 ATTR1  = CONV #( ME->CARGA-ID_PRODUTO ) )
            MSGTY     = 'E'
            MSGID     = ZCX_CARGA=>ZCX_PARAM_SOBRA_MATERIAL-MSGID
            MSGNO     = ZCX_CARGA=>ZCX_PARAM_SOBRA_MATERIAL-MSGNO
            MSGV1     = CONV #( ME->CARGA-ID_PRODUTO )
            TRANSACAO = 'ZMM0112'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_CK_SAIDA_AUTOMATICA.

    R_INSTANCE = ME.

    SELECT SINGLE *
      FROM SETLEAF
      INTO @DATA(WA_SETLEAF)
     WHERE SETNAME EQ 'MAGGI_SAIDA_AUT_ZMM0127'
       AND VALFROM LE @ME->CARGA-ID_BRANCH
       AND VALTO GE @ME->CARGA-ID_BRANCH.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_CARGA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_SAIDA_NAO_AUTOMATICA-MSGID MSGNO = ZCX_CARGA=>ZCX_SAIDA_NAO_AUTOMATICA-MSGNO )
        MSGTY  = 'E'
        MSGID  = ZCX_CARGA=>ZCX_SAIDA_NAO_AUTOMATICA-MSGID
        MSGNO  = ZCX_CARGA=>ZCX_SAIDA_NAO_AUTOMATICA-MSGNO.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_CK_SOL_AJUSTE_NAO_PROC.

    R_CARGA = ME.

    IF ME->ZIF_CARGA~CARGA-TP_STATUS NE ZIF_CARGA=>ST_STATUS_CONFERIDO.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_SOL_MANU_NAO_CONFE-MSGID MSGNO = ZCX_CARGA=>ZCX_SOL_MANU_NAO_CONFE-MSGNO )
          MSGTY  = 'E'
          MSGID  = ZCX_CARGA=>ZCX_SOL_MANU_NAO_CONFE-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_SOL_MANU_NAO_CONFE-MSGNO.
    ENDIF.

    IF NOT ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_PROCESSOU_AJUSTE EQ ABAP_FALSE.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_CARGA=>ZCX_SOL_MANUTENCAO_PROCESS-MSGID
                            MSGNO  = ZCX_CARGA=>ZCX_SOL_MANUTENCAO_PROCESS-MSGNO )
          MSGID  = ZCX_CARGA=>ZCX_SOL_MANUTENCAO_PROCESS-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_SOL_MANUTENCAO_PROCESS-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    "A  Solicitação Aprovada
    "R  Solicitação Recusada
    "W  Solicitação Em Espera de Aprovação
    "S  Solicitação não gera Aprovação

    IF ( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA OR
         ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_NAO_GERA ) AND
       ( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_COMERCIAL EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA OR
         ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_COMERCIAL EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_NAO_GERA ) AND
       ( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FISCAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA OR
         ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FISCAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_NAO_GERA ).
    ELSE.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_CARGA=>ZCX_SOL_MANUTENCAO_WAIT-MSGID
                            MSGNO  = ZCX_CARGA=>ZCX_SOL_MANUTENCAO_WAIT-MSGNO )
          MSGID  = ZCX_CARGA=>ZCX_SOL_MANUTENCAO_WAIT-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_SOL_MANUTENCAO_WAIT-MSGNO
          MSGTY  = 'E'.
    ENDIF.

*    IF ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_ACEITE_FILIAL EQ ABAP_TRUE AND
*       ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA.
*      DATA(CK_APROVOU_FILIAL) = ABAP_TRUE.
*    ELSEIF ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_ACEITE_FILIAL EQ ABAP_TRUE.
*      CK_APROVOU_FILIAL = ABAP_FALSE.
*    ENDIF.
*
*    IF ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_ACEITE_FISCAL EQ ABAP_TRUE AND
*       ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FISCAL EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA.
*      DATA(CK_APROVOU_FISCAL) = ABAP_TRUE.
*    ELSEIF ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_ACEITE_FISCAL EQ ABAP_TRUE.
*      CK_APROVOU_FISCAL = ABAP_FALSE.
*    ENDIF.
*
*    IF ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_ACEITE_COMERCIAL EQ ABAP_TRUE AND
*       ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_COMERCIAL EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA.
*      DATA(CK_APROVOU_COMERCIAL) = ABAP_TRUE.
*    ELSEIF ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_ACEITE_COMERCIAL EQ ABAP_TRUE.
*      CK_APROVOU_COMERCIAL = ABAP_FALSE.
*    ENDIF.
*
*    IF NOT CK_APROVOU_FILIAL    EQ ABAP_TRUE OR
*       NOT CK_APROVOU_FISCAL    EQ ABAP_TRUE OR
*       NOT CK_APROVOU_COMERCIAL EQ ABAP_TRUE.
*      RAISE EXCEPTION TYPE ZCX_CARGA
*        EXPORTING
*          TEXTID = VALUE #( MSGID  = ZCX_CARGA=>ZCX_SOL_MANUTENCAO_WAIT-MSGID
*                            MSGNO  = ZCX_CARGA=>ZCX_SOL_MANUTENCAO_WAIT-MSGNO )
*          MSGID  = ZCX_CARGA=>ZCX_SOL_MANUTENCAO_WAIT-MSGID
*          MSGNO  = ZCX_CARGA=>ZCX_SOL_MANUTENCAO_WAIT-MSGNO
*          MSGTY  = 'E'.
*    ENDIF.

  ENDMETHOD.


  method zif_carga~get_ck_sub_carac_ava_material.

    if me->carga-id_produto is initial.
      raise exception type zcx_carga
        exporting
          textid = value #( msgid = zcx_carga=>zcx_nao_inf_material-msgid
                            msgno = zcx_carga=>zcx_nao_inf_material-msgno )
          msgid  = zcx_carga=>zcx_nao_inf_material-msgid
          msgno  = zcx_carga=>zcx_nao_inf_material-msgno
          msgty  = 'E'.
    endif.

    select single * into @data(wa_mara)
      from mara
     where matnr eq @me->carga-id_produto.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_mara-matkl
      importing
        output = wa_mara-matkl.

*1  Ardido/Queimado
*2  Queimados
*3  Mofados
*4  Picados
*5  Fermentados
*6  Germinados/Imaturos/Chochos
*7  Ardidos
*8  Gessados

    case wa_mara-matkl.

      when zif_carga=>st_grupo_algodao_pluma. "Algodão

        case i_sub_carac_ava.
          when zif_carga=>st_tp_caract_sub_mof.
            e_ck_carac       = abap_false.
            e_ck_obrigatorio = abap_false.
          when zif_carga=>st_tp_caract_sub_fer.
            e_ck_carac       = abap_false.
            e_ck_obrigatorio = abap_false.
          when zif_carga=>st_tp_caract_sub_ger.
            e_ck_carac       = abap_false.
            e_ck_obrigatorio = abap_false.
          when zif_carga=>st_tp_caract_sub_ard.
            e_ck_carac       = abap_false.
            e_ck_obrigatorio = abap_false.
          when zif_carga=>st_tp_caract_sub_ges.
            e_ck_carac       = abap_false.
            e_ck_obrigatorio = abap_false.
          when others.
            e_ck_carac       = abap_false.
            e_ck_obrigatorio = abap_false.
        endcase.

      when '700170'. "Milho

        case i_sub_carac_ava.
          when zif_carga=>st_tp_caract_sub_mof.
            e_ck_carac       = abap_true.
            e_ck_obrigatorio = abap_false.
          when zif_carga=>st_tp_caract_sub_fer.
            e_ck_carac       = abap_true.
            e_ck_obrigatorio = abap_true.
          when zif_carga=>st_tp_caract_sub_ger.
            e_ck_carac       = abap_true.
            e_ck_obrigatorio = abap_false.
          when zif_carga=>st_tp_caract_sub_ard.
            e_ck_carac       = abap_true.
            e_ck_obrigatorio = abap_false.
          when zif_carga=>st_tp_caract_sub_ges.
            e_ck_carac       = abap_true.
            e_ck_obrigatorio = abap_true.
          when others.
            e_ck_carac       = abap_false.
            e_ck_obrigatorio = abap_false.
        endcase.

      when '700110'. "Soja

        case i_sub_carac_ava.
          when zif_carga=>st_tp_caract_sub_arq.
            e_ck_carac       = abap_true.
            e_ck_obrigatorio = abap_false.
          when zif_carga=>st_tp_caract_sub_que.
            e_ck_carac       = abap_true.
            e_ck_obrigatorio = abap_false.
          when zif_carga=>st_tp_caract_sub_mof.
            e_ck_carac       = abap_true.
            e_ck_obrigatorio = abap_false.
          when zif_carga=>st_tp_caract_sub_pic.
            e_ck_carac       = abap_true.
            e_ck_obrigatorio = abap_true.
          when zif_carga=>st_tp_caract_sub_fer.
            e_ck_carac       = abap_true.
            e_ck_obrigatorio = abap_true.
          when zif_carga=>st_tp_caract_sub_ger.
            e_ck_carac       = abap_true.
            e_ck_obrigatorio = abap_false.
          when zif_carga=>st_tp_caract_sub_ard. "ALRS 21/01/2025  US164152
            e_ck_carac       = abap_true.
            e_ck_obrigatorio = abap_false.
          when others.
            e_ck_carac       = abap_false.
            e_ck_obrigatorio = abap_false.
        endcase.

      when others.
        e_ck_carac       = abap_false.
        e_ck_obrigatorio = abap_false.
    endcase.



  endmethod.


  METHOD zif_carga~get_contrato_ordem_venda.

    FREE: e_id_contrato.

    r_carga = me.

    CHECK me->carga-tp_produto_carga = '02'. "Algodao

    SELECT nro_sol_ov
      INTO @DATA(l_nro_sol_ov)
      FROM zsdt0066
        UP TO 1 ROWS
     WHERE vbeln = @i_ordem_venda.
    ENDSELECT.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_ordem_venda_contrato-msgid
                            msgno = zcx_carga=>zcx_ordem_venda_contrato-msgno
                            attr1 = CONV #( i_ordem_venda ) )
          msgid  = zcx_carga=>zcx_ordem_venda_contrato-msgid
          msgno  = zcx_carga=>zcx_ordem_venda_contrato-msgno
          msgty  = 'E'
          msgv1  = CONV #( i_ordem_venda ).
    ENDIF.

    SELECT id_contrato
      INTO @DATA(l_id_contrato)
      FROM zsdt0051
        UP TO 1 ROWS
     WHERE nro_sol_ov = @l_nro_sol_ov.
    ENDSELECT.

    IF sy-subrc <> 0 OR l_id_contrato IS INITIAL.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_ordem_venda_contrato-msgid
                            msgno = zcx_carga=>zcx_ordem_venda_contrato-msgno
                            attr1 = CONV #( i_ordem_venda ) )
          msgid  = zcx_carga=>zcx_ordem_venda_contrato-msgid
          msgno  = zcx_carga=>zcx_ordem_venda_contrato-msgno
          msgty  = 'E'
          msgv1  = CONV #( i_ordem_venda ).
    ENDIF.

    e_id_contrato = l_id_contrato.

  ENDMETHOD.


  METHOD zif_carga~get_documento_ent_estornado.

    r_estornado = abap_true.

    SELECT SINGLE * INTO @DATA(wa_entrada)
      FROM zmmt_ee_zgr
     WHERE obj_key EQ @i_obj_key.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE * INTO @DATA(wa_entrada_docs)
      FROM zmmt_ee_zgr_docs
     WHERE obj_key EQ @i_obj_key.

    CHECK sy-subrc IS INITIAL.

*    IF WA_ENTRADA_DOCS-AV_VBELN IS NOT INITIAL.
*      "Verifica Aviso
*      SELECT SINGLE * INTO @DATA(WA_LIKP)
*        FROM LIKP
*       WHERE VBELN EQ @WA_ENTRADA_DOCS-AV_VBELN.
*
*      IF SY-SUBRC IS INITIAL.
*        R_ESTORNADO = ABAP_FALSE.
*      ENDIF.
*    ENDIF.

    IF wa_entrada_docs-mm_mblnr IS NOT INITIAL.
      "Verifica se Documento de Material foi Estornado
      SELECT SINGLE * INTO @DATA(wa_mkpf)
        FROM mseg
       WHERE smbln EQ @wa_entrada_docs-mm_mblnr
         AND sjahr EQ @wa_entrada_docs-mm_mjahr.

      IF sy-subrc IS NOT INITIAL.
        r_estornado = abap_false.
      ENDIF.
    ENDIF.

    IF wa_entrada_docs-ft_belnr IS NOT INITIAL.
      "Verifica se Documento Fatura foi Estornado
      SELECT SINGLE * INTO @DATA(wa_rbkp)
        FROM rbkp
       WHERE belnr EQ @wa_entrada_docs-ft_belnr
         AND gjahr EQ @wa_entrada_docs-ft_gjahr.

      IF wa_rbkp-stblg IS INITIAL AND sy-subrc IS INITIAL.
        r_estornado = abap_false.
      ENDIF.
    ENDIF.

    IF wa_entrada_docs-docnum IS NOT INITIAL.
      "Verifica se Documento Fiscal foi estornado

*-CS2021000183-#83755-19.07.2022-JT-inicio
*-verifica se é Nota Propria. Se for, o cancelamento é feito
*-apos este processo
      READ TABLE me->documento_fiscal INTO DATA(w_fiscal) WITH KEY docnum_np = wa_entrada_docs-docnum.
      IF sy-subrc <> 0.
*-CS2021000183-#83755-19.07.2022-JT-fim
        SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
          FROM j_1bnfdoc
         WHERE docnum EQ @wa_entrada_docs-docnum.

        IF sy-subrc IS INITIAL AND wa_j_1bnfdoc-cancel EQ abap_false.
          r_estornado = abap_false.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_entrada_docs-mm_mblnr_sobra IS NOT INITIAL.
      "Verifica se Documento de Material de Sobra foi Estornado
      SELECT SINGLE * INTO @DATA(wa_mkpf_sobra)
        FROM mseg
       WHERE smbln EQ @wa_entrada_docs-mm_mblnr_sobra
         AND sjahr EQ @wa_entrada_docs-mm_mjahr_sobra.

      IF sy-subrc IS NOT INITIAL.
        r_estornado = abap_false.
      ENDIF.
    ENDIF.

    "Se Todos os Documentos estão estornas remove registro
    IF r_estornado EQ abap_true.
      DELETE FROM zmmt_ee_zgr_docs  WHERE obj_key EQ @i_obj_key AND obj_key NE @space.
    ENDIF.

  ENDMETHOD.


  method zif_carga~get_factory_tp_transgenia.

*(Campo 1) IN_GMO
*(Campo 2) IN_SRR_DECLARADO
*(Campo 3) IN_SRR_DECLARADO_2
*(Campo 4) IN_TESTE_SRR_2
*(campo 5) IN_SRR_ORIGEM_PARTIC

    "US 143677 - transgenia por nota
*    move-corresponding i_classificacao to me->classificacao.
    "US 143677 - transgenia por nota

    select single * into @data(wa_mara)
      from mara
     where matnr eq @me->carga-id_produto.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_mara-matkl
      importing
        output = wa_mara-matkl.

    case wa_mara-matkl.
      when zif_carga=>st_grupo_algodao_pluma. "Algodão

        e_tp_transgenia = 'D1'.
        exit.

      when zif_carga=>st_grupo_caroco_algodao_ter. "Caroço de Algodão Adq de Terceiros

        e_tp_transgenia = 'D1'.
        exit.

      when '700300'. "Resíduo de Soja

        e_tp_transgenia = 'CO'.
        exit.

      when '700170'. "Milho

        e_tp_transgenia = 'D1'.
        exit.

      when '700110'. "Soja

*        case me->classificacao-in_gmo.
        case i_classificacao-in_gmo.

*1: Se campo 1 = NEGATIVO, demais campos devem ser marcados com bloqueio para alteração conforme abaixo:
*CONVENCIONAL (CO) Desmarcar "Negativo"
          when zif_carga=>st_gmo_negativo.
            e_tp_transgenia = 'CO'.
            exit.
*2: Se campo 1 = POSITIVO, demais campos devem ser marcados com bloqueio para alteração conforme abaixo:
*RR TESTADO (T1) Desmarcar "Positivo"
          when zif_carga=>st_gmo_positivo.
            e_tp_transgenia = 'T1'.
            exit.
        endcase.

*4: Se campo 3 = SIM, demais campos devem ser marcados com bloqueio para alteração conforme abaixo:
*RR2 DECLARADO (D2)  Desmarcar "Sim"
*        if me->classificacao-in_srr_declarado_2 eq abap_true.
        if i_classificacao-in_srr_declarado_2 eq abap_true. "US 143677 - transgenia por nota
          e_tp_transgenia = 'D2'.
          exit.
        endif.

*        case me->classificacao-in_teste_srr_2.
        case i_classificacao-in_teste_srr_2. "US 143677 - transgenia por nota
*5: Se campo 4 = POSITIVO, demais campos devem ser marcados com bloqueio para alteração conforme abaixo:
*RR2 TESTADO (T2)  Desmarcar "Positivo"
          when zif_carga=>st_gmo_positivo.
            e_tp_transgenia = 'T2'.
            exit.
*6: Se campo 4 = NEGATIVO, demais campos devem ser marcados com bloqueio para alteração conforme abaixo:
*RR1 DECLARADO (D1)  Desmarcar "Negativo"
          when zif_carga=>st_gmo_negativo.
            e_tp_transgenia = 'D1'.
            exit.
        endcase.

*5: Se campo 5 = SIM, demais campos devem ser marcados com bloqueio para alteração conforme abaixo:
*PARTICIPANTE (PA) Desmarcar "Negativo"
*        if me->classificacao-in_srr_origem_partic eq abap_true.
        if i_classificacao-in_srr_origem_partic eq abap_true. "US 143677 - transgenia por nota
          e_tp_transgenia = 'PA'.
          exit.
        endif.

      when others.
        e_tp_transgenia = space.
    endcase.

    raise exception type zcx_carga
      exporting
        textid = value #( msgid = zcx_carga=>zcx_erro_determinar_transg-msgid
                          msgno = zcx_carga=>zcx_erro_determinar_transg-msgno )
        msgty  = 'E'
        msgno  = zcx_carga=>zcx_erro_determinar_transg-msgno
        msgid  = zcx_carga=>zcx_erro_determinar_transg-msgid.

  endmethod.


  method zif_carga~get_info_alv_apresentacao.

    r_carga = me.

    data: wa_nota_alv        type zde_zsdt0001nt_alv,
          wa_ordem_alv       type zde_zsdt0001ov_alv,
          wa_pedido_alv      type zde_zsdt0001ek_alv,
          wa_solicitacao_alv type zde_zsdt0001acb_alv,
          wa_bloco_alv       type zde_zsdt0001fd_alv.

    clear: e_apresentacao.

    "Carga
    move-corresponding me->carga to e_apresentacao-carga.

    "Transgenia
    move-corresponding me->classificacao to e_apresentacao-carga.

*    *BUG 179188 soma qtde de todas as notas
    clear: e_apresentacao-carga-nr_qtde_umi,
           e_apresentacao-carga-nr_qtde_imp,
           e_apresentacao-carga-nr_qtde_ava,
           e_apresentacao-carga-nr_qtde_ard,
           e_apresentacao-carga-nr_qtde_que,
           e_apresentacao-carga-nr_qtde_esv,
           e_apresentacao-carga-nr_qtde_car.


    "Classificação
    loop at me->resultado into data(wa_resultado). " where id_classificacao eq me->carga-id_classificacao.
      case wa_resultado-tp_caracteristica.
        when zif_carga=>st_tp_caract_class_umidade.
          e_apresentacao-carga-nr_perc_umi = wa_resultado-nr_percentual_com.
*          e_apresentacao-carga-nr_qtde_umi = wa_resultado-nr_quantidade_com.
          add wa_resultado-nr_quantidade_com to e_apresentacao-carga-nr_qtde_umi.
        when zif_carga=>st_tp_caract_class_impureza.
          e_apresentacao-carga-nr_perc_imp = wa_resultado-nr_percentual_com.
*          e_apresentacao-carga-nr_qtde_imp = wa_resultado-nr_quantidade_com.
          add wa_resultado-nr_quantidade_com to e_apresentacao-carga-nr_qtde_imp.
        when zif_carga=>st_tp_caract_class_avariado.
          e_apresentacao-carga-nr_perc_ava = wa_resultado-nr_percentual_com.
*          e_apresentacao-carga-nr_qtde_ava = wa_resultado-nr_quantidade_com.
          add wa_resultado-nr_quantidade_com to e_apresentacao-carga-nr_qtde_ava.
        when zif_carga=>st_tp_caract_class_ardido.
          e_apresentacao-carga-nr_perc_ard = wa_resultado-nr_percentual_com.
*          e_apresentacao-carga-nr_qtde_ard = wa_resultado-nr_quantidade_com.
          add wa_resultado-nr_quantidade_com to e_apresentacao-carga-nr_qtde_ard.
        when zif_carga=>st_tp_caract_class_quebrado.
          e_apresentacao-carga-nr_perc_que = wa_resultado-nr_percentual_com.
*          e_apresentacao-carga-nr_qtde_que = wa_resultado-nr_quantidade_com.
          add wa_resultado-nr_quantidade_com to e_apresentacao-carga-nr_qtde_que.
        when zif_carga=>st_tp_caract_class_esverdeado.
          e_apresentacao-carga-nr_perc_esv = wa_resultado-nr_percentual_com.
*          e_apresentacao-carga-nr_qtde_esv = wa_resultado-nr_quantidade_com.
          add wa_resultado-nr_quantidade_com to e_apresentacao-carga-nr_qtde_esv.
        when zif_carga=>st_tp_caract_class_carunchado.
          e_apresentacao-carga-nr_perc_car = wa_resultado-nr_percentual_com.
*          e_apresentacao-carga-nr_qtde_car = wa_resultado-nr_quantidade_com.
          add wa_resultado-nr_quantidade_com to e_apresentacao-carga-nr_qtde_car.
      endcase.
    endloop.

    "SubCaracterística da Classificação
    loop at me->zif_carga~resultado_avariado into data(wa_resul_avariado) where id_classificacao eq me->carga-id_classificacao.
      case wa_resul_avariado-tp_sub_carac_avariado.
        when zif_carga=>st_tp_caract_sub_arq.
          e_apresentacao-carga-nr_perc_ava_arq = wa_resul_avariado-nr_percentual_com.
        when zif_carga=>st_tp_caract_sub_que.
          e_apresentacao-carga-nr_perc_ava_que = wa_resul_avariado-nr_percentual_com.
        when zif_carga=>st_tp_caract_sub_mof.
          e_apresentacao-carga-nr_perc_ava_mof = wa_resul_avariado-nr_percentual_com.
        when zif_carga=>st_tp_caract_sub_pic.
          e_apresentacao-carga-nr_perc_ava_pic = wa_resul_avariado-nr_percentual_com.
        when zif_carga=>st_tp_caract_sub_fer.
          e_apresentacao-carga-nr_perc_ava_fer = wa_resul_avariado-nr_percentual_com.
        when zif_carga=>st_tp_caract_sub_ger.
          e_apresentacao-carga-nr_perc_ava_ger = wa_resul_avariado-nr_percentual_com.
        when zif_carga=>st_tp_caract_sub_ard.
          e_apresentacao-carga-nr_perc_ava_ard = wa_resul_avariado-nr_percentual_com.
        when zif_carga=>st_tp_caract_sub_ges.
          e_apresentacao-carga-nr_perc_ava_ges = wa_resul_avariado-nr_percentual_com.
      endcase.
    endloop.

    select single name into e_apresentacao-carga-name
      from j_1bbranch
     where bukrs  eq e_apresentacao-carga-id_bukrs
       and branch eq e_apresentacao-carga-id_branch.

    if e_apresentacao-carga-id_local_coleta is not initial.
      select single name1
        into e_apresentacao-carga-ds_local_coleta
        from lfa1 where lifnr eq e_apresentacao-carga-id_local_coleta.
    endif.

    if e_apresentacao-carga-id_local_descarga is not initial.
      select single name1
        into e_apresentacao-carga-ds_local_descarga
        from kna1 where kunnr eq e_apresentacao-carga-id_local_descarga.
    endif.

    if e_apresentacao-carga-id_local_destino is not initial.
      select single name1
        into e_apresentacao-carga-ds_local_destino
        from lfa1 where lifnr eq e_apresentacao-carga-id_local_destino.
    endif.

    if e_apresentacao-carga-id_classificadora is not initial.
      select single name1
        into e_apresentacao-carga-ds_classificadora
        from lfa1 where lifnr eq e_apresentacao-carga-id_classificadora.
    endif.

    if e_apresentacao-carga-id_local_entrega is not initial.
      select single ds_local_entrega
        into e_apresentacao-carga-ds_local_entrega
        from zsdt0001le where id_local_entrega eq e_apresentacao-carga-id_local_entrega.
    endif.

    if e_apresentacao-carga-id_proprietario is not initial.
      select single name1
        into e_apresentacao-carga-ds_proprietario
        from lfa1 where lifnr eq e_apresentacao-carga-id_proprietario.
    endif.

    if e_apresentacao-carga-id_outro_partic is not initial.
      select single name1
        into e_apresentacao-carga-ds_outro_partic
        from lfa1 where lifnr eq e_apresentacao-carga-id_outro_partic.
    endif.

    if e_apresentacao-carga-id_agent_frete is not initial.
      select single name1
        into e_apresentacao-carga-ds_agent_frete
        from lfa1 where lifnr eq e_apresentacao-carga-id_agent_frete.
    endif.

    if e_apresentacao-carga-id_motorista is not initial.
      select single name1
        into e_apresentacao-carga-ds_motorista
        from lfa1 where lifnr eq e_apresentacao-carga-id_motorista.
    endif.

    if e_apresentacao-carga-id_produto is not initial.
      select single maktx
        into e_apresentacao-carga-ds_produto
        from makt
       where matnr eq e_apresentacao-carga-id_produto
         and spras eq sy-langu.
    endif.

    if e_apresentacao-carga-id_ordem is not initial.
      data(r_ordem_carrgamento) = zcl_ordem_carregamento=>busca_ordem_carregamento( i_id_ordem = e_apresentacao-carga-id_ordem ).
      e_apresentacao-carga-nr_ordem = r_ordem_carrgamento-nr_ordem.
      e_apresentacao-ordem_carrega  = r_ordem_carrgamento.
    endif.

    "Documentos Fiscais
    loop at me->documento_fiscal into data(wa_nota).

      clear: wa_nota_alv.

      move-corresponding wa_nota to wa_nota_alv.

*-CS2021000183 - 29.03.2022 - JT - inicio
      if wa_nota_alv-ent_np     = abap_true   and
         wa_nota_alv-docnum_np is not initial.
        wa_nota_alv-docnum = wa_nota_alv-docnum_np.
      endif.
*-CS2021000183 - 29.03.2022 - JT - fim

      select single name1
        into wa_nota_alv-ds_fornecedor
        from lfa1 where lifnr eq wa_nota_alv-id_fornecedor.

      wa_nota_alv-id_produto = e_apresentacao-carga-id_produto.
      wa_nota_alv-ds_produto = e_apresentacao-carga-ds_produto.

      select single ds_entrada
        into wa_nota_alv-ds_entrada
        from zsdt0001tetx where id_entrada eq wa_nota-id_entrada.

      if wa_nota_alv-id_entregue_por is not initial.
        select single name1
          into wa_nota_alv-ds_entregue_por
          from lfa1 where lifnr eq wa_nota_alv-id_entregue_por.
      endif.

      "Classificação
      loop at me->resultado into wa_resultado where id_classificacao eq wa_nota-id_classificacao.
        case wa_resultado-tp_caracteristica.
          when zif_carga=>st_tp_caract_class_umidade.
            wa_nota_alv-nr_perc_umi = wa_resultado-nr_percentual_com.
            wa_nota_alv-nr_qtde_umi = wa_resultado-nr_quantidade_com.
          when zif_carga=>st_tp_caract_class_impureza.
            wa_nota_alv-nr_perc_imp = wa_resultado-nr_percentual_com.
            wa_nota_alv-nr_qtde_imp = wa_resultado-nr_quantidade_com.
          when zif_carga=>st_tp_caract_class_avariado.
            wa_nota_alv-nr_perc_ava = wa_resultado-nr_percentual_com.
            wa_nota_alv-nr_qtde_ava = wa_resultado-nr_quantidade_com.
          when zif_carga=>st_tp_caract_class_ardido.
            wa_nota_alv-nr_perc_ard = wa_resultado-nr_percentual_com.
            wa_nota_alv-nr_qtde_ard = wa_resultado-nr_quantidade_com.
          when zif_carga=>st_tp_caract_class_quebrado.
            wa_nota_alv-nr_perc_que = wa_resultado-nr_percentual_com.
            wa_nota_alv-nr_qtde_que = wa_resultado-nr_quantidade_com.
          when zif_carga=>st_tp_caract_class_esverdeado.
            wa_nota_alv-nr_perc_esv = wa_resultado-nr_percentual_com.
            wa_nota_alv-nr_qtde_esv = wa_resultado-nr_quantidade_com.
          when zif_carga=>st_tp_caract_class_carunchado.
            wa_nota_alv-nr_perc_car = wa_resultado-nr_percentual_com.
            wa_nota_alv-nr_qtde_car = wa_resultado-nr_quantidade_com.
        endcase.
      endloop.

      "SubCaracterística da Classificação
      loop at me->zif_carga~resultado_avariado into wa_resul_avariado where id_classificacao eq me->carga-id_classificacao.
        case wa_resul_avariado-tp_sub_carac_avariado.
          when zif_carga=>st_tp_caract_sub_arq.
            wa_nota_alv-nr_perc_ava_arq = wa_resul_avariado-nr_percentual_com.
          when zif_carga=>st_tp_caract_sub_que.
            wa_nota_alv-nr_perc_ava_que = wa_resul_avariado-nr_percentual_com.
          when zif_carga=>st_tp_caract_sub_mof.
            wa_nota_alv-nr_perc_ava_mof = wa_resul_avariado-nr_percentual_com.
          when zif_carga=>st_tp_caract_sub_pic.
            wa_nota_alv-nr_perc_ava_pic = wa_resul_avariado-nr_percentual_com.
          when zif_carga=>st_tp_caract_sub_fer.
            wa_nota_alv-nr_perc_ava_fer = wa_resul_avariado-nr_percentual_com.
          when zif_carga=>st_tp_caract_sub_ger.
            wa_nota_alv-nr_perc_ava_ger = wa_resul_avariado-nr_percentual_com.
          when zif_carga=>st_tp_caract_sub_ard.
            wa_nota_alv-nr_perc_ava_ard = wa_resul_avariado-nr_percentual_com.
          when zif_carga=>st_tp_caract_sub_ges.
            wa_nota_alv-nr_perc_ava_ges = wa_resul_avariado-nr_percentual_com.
        endcase.
      endloop.

      if me->carga-tp_status ne zif_carga=>st_status_conferido and
         me->carga-tp_status ne zif_carga=>st_status_cancelada.
        "Recalcular Pois ainda não está gravado em banco como resultado (somente acontece na conferência)
        me->set_pesos_notas(
          exporting
            i_id_carga      = wa_nota_alv-id_carga   " Id. da Carga
            i_id_nota       = wa_nota_alv-id_nota    " Id. Nota Fiscal
            i_peso_subtotal = wa_nota_alv-nm_peso_subtotal  " Peso SubTotal do Caminhão
            i_peso_liquido  = wa_nota_alv-nm_peso_liquido   " Peso Líquido
            i_sem_consulta  = abap_false
          importing
            e_nota          = data(wa_saida) ).

        wa_nota_alv-nr_perc_umi = wa_saida-nr_perc_umi.
        wa_nota_alv-nr_qtde_umi = wa_saida-nr_qtde_umi.
        wa_nota_alv-nr_perc_imp = wa_saida-nr_perc_imp.
        wa_nota_alv-nr_qtde_imp = wa_saida-nr_qtde_imp.
        wa_nota_alv-nr_perc_ava = wa_saida-nr_perc_ava.
        wa_nota_alv-nr_qtde_ava = wa_saida-nr_qtde_ava.
        wa_nota_alv-nr_perc_ard = wa_saida-nr_perc_ard.
        wa_nota_alv-nr_qtde_ard = wa_saida-nr_qtde_ard.
        wa_nota_alv-nr_perc_que = wa_saida-nr_perc_que.
        wa_nota_alv-nr_qtde_que = wa_saida-nr_qtde_que.
        wa_nota_alv-nr_perc_esv = wa_saida-nr_perc_esv.
        wa_nota_alv-nr_qtde_esv = wa_saida-nr_qtde_esv.
        wa_nota_alv-nr_perc_car = wa_saida-nr_perc_car.
        wa_nota_alv-nr_qtde_car = wa_saida-nr_qtde_car.

      endif.

      append wa_nota_alv to e_apresentacao-notas.

    endloop.

    "Ordens de Venda
    loop at me->zif_carga~ordem_venda into data(wa_ordem).
      clear: wa_ordem_alv.

      move-corresponding wa_ordem to wa_ordem_alv.

      if wa_ordem-nr_ordem_venda is not initial.
        try .
            me->get_info_ordem_venda( exporting i_ordem_venda = wa_ordem-nr_ordem_venda importing e_ordem = data(r_ordem) ).
            wa_ordem_alv-tp_tipo_ordem  = r_ordem-tp_tipo_ordem.
            wa_ordem_alv-ds_tipo_ordem  = r_ordem-ds_tipo_ordem.
            wa_ordem_alv-ds_tipo_frete  = r_ordem-ds_tipo_frete.
            wa_ordem_alv-id_produto     = r_ordem-id_produto.
            wa_ordem_alv-ds_produto     = r_ordem-ds_produto.
          catch zcx_carga.    "
        endtry.
      endif.

      append wa_ordem_alv to e_apresentacao-ordem_venda.
    endloop.

    "Pedido de Compra
    loop at me->zif_carga~pedido_compra into data(wa_pedido_compra).
      clear: wa_pedido_alv.

      move-corresponding wa_pedido_compra to wa_pedido_alv.

      if wa_pedido_compra-nr_pedido_compra is not initial.
        try .
            me->zif_carga~get_info_pedido_compra(
              exporting
                i_pedido_compra = wa_pedido_compra-nr_pedido_compra
              importing
                e_pedido = data(e_pedido) ).
            wa_pedido_alv-tp_tipo_pedido = e_pedido-tp_tipo_pedido.
            wa_pedido_alv-ds_tipo_pedido = e_pedido-ds_tipo_pedido.
            wa_pedido_alv-ds_tipo_frete  = e_pedido-ds_tipo_frete.
            wa_pedido_alv-id_produto     = e_pedido-id_produto.
            wa_pedido_alv-ds_produto     = e_pedido-ds_produto.
          catch zcx_carga.
        endtry.
      endif.

      append wa_pedido_alv to e_apresentacao-pedido_compra.

    endloop.

    "Solicitação de Manutenção
    move-corresponding me->zif_carga~solicitacao_manutencao to e_apresentacao-manutencao.
    if me->zif_carga~solicitacao_manutencao is not initial.
      e_apresentacao-manutencao-tp_solicitacao_status = me->zif_carga~solicitacao_manutencao-tp_status.
    endif.

    "Solicitações de Manutenção da Carga
    loop at me->zif_carga~solicitacoes into data(wa_solicitacoes).
      clear: wa_solicitacao_alv.
      move-corresponding wa_solicitacoes to wa_solicitacao_alv.
      wa_solicitacao_alv-tp_solicitacao_status = wa_solicitacoes-tp_status.
      append wa_solicitacao_alv to e_apresentacao-solicitacoes.
    endloop.

    loop at me->zif_carga~take_up into data(wa_take_up).
      read table e_apresentacao-notas with key id_nota = wa_take_up-id_nota into wa_nota_alv.
      if sy-subrc is not initial.
        continue.
      endif.
      append value #( id_carga      = wa_take_up-id_carga
                      id_nota       = wa_take_up-id_nota
                      ds_fornecedor = wa_nota_alv-ds_fornecedor
                      id_mod_fiscal = wa_nota_alv-id_mod_fiscal
                      nr_nota       = wa_nota_alv-nr_nota
                      nm_serie      = wa_nota_alv-nm_serie
                      dt_emissao    = wa_nota_alv-dt_emissao
                      id_takeup     = wa_take_up-id_takeup
                      nu_bloco      = wa_take_up-nu_bloco
                      qt_vinculada  = wa_take_up-qt_vinculada
                      qt_fardos     = wa_take_up-qt_fardos )
                      to e_apresentacao-takeup.
    endloop.

    loop at me->zif_carga~blocos into data(wa_bloco).

      clear: wa_bloco_alv.
      move-corresponding wa_bloco to wa_bloco_alv.

      select single name1 into @wa_bloco_alv-ds_ponto_c
        from lfa1
       where lifnr eq @wa_bloco-cd_ponto_c.

      append wa_bloco_alv to e_apresentacao-blocos.

    endloop.

  endmethod.


  METHOD ZIF_CARGA~GET_INFO_ALV_APRESENTACAO_LOG.

    DATA: WA_NOTA_ALV   TYPE ZDE_ZSDT0001NT_ALV.
    DATA: WA_ORDEM_ALV  TYPE ZDE_ZSDT0001OV_ALV.
    DATA: WA_TAKEUP_ALV TYPE ZDE_ZSDT0001TK_ALV.
    DATA: WA_BLOCO_ALV TYPE ZDE_ZSDT0001FD_ALV.

    R_CARGA = ME.

    CLEAR: E_APRESENTACAO.

    SELECT SINGLE *
      INTO @DATA(WA_ZSDT0001CG_LOG)
      FROM ZSDT0001CGLG
     WHERE ID_CARGA    EQ @ME->CARGA-ID_CARGA
       AND DT_REGISTRO EQ @I_DT_REGISTRO
       AND HR_REGISTRO EQ @I_HR_REGISTRO
       AND US_REGISTRO EQ @I_US_REGISTRO.

    SELECT *
      INTO TABLE @DATA(IT_ZSDT0001NT_LOG)
      FROM ZSDT0001NTLG
     WHERE ID_CARGA    EQ @ME->CARGA-ID_CARGA
       AND DT_REGISTRO EQ @I_DT_REGISTRO
       AND HR_REGISTRO EQ @I_HR_REGISTRO
       AND US_REGISTRO EQ @I_US_REGISTRO.

    SELECT *
      INTO TABLE @DATA(IT_ZSDT0001TK_LOG)
      FROM ZSDT0001TKLG
     WHERE ID_CARGA    EQ @ME->CARGA-ID_CARGA
       AND DT_REGISTRO EQ @I_DT_REGISTRO
       AND HR_REGISTRO EQ @I_HR_REGISTRO
       AND US_REGISTRO EQ @I_US_REGISTRO.

    SELECT *
      INTO TABLE @DATA(IT_ZSDT0001OV_LOG)
      FROM ZSDT0001OVLG
     WHERE ID_CARGA    EQ @ME->CARGA-ID_CARGA
       AND DT_REGISTRO EQ @I_DT_REGISTRO
       AND HR_REGISTRO EQ @I_HR_REGISTRO
       AND US_REGISTRO EQ @I_US_REGISTRO.

    SELECT *
      INTO TABLE @DATA(IT_ZSDT0001FD_LOG)
      FROM ZSDT0001FDLG
     WHERE ID_CARGA    EQ @ME->CARGA-ID_CARGA
       AND DT_REGISTRO EQ @I_DT_REGISTRO
       AND HR_REGISTRO EQ @I_HR_REGISTRO
       AND US_REGISTRO EQ @I_US_REGISTRO.

    SELECT *
      INTO TABLE @DATA(IT_ZSDT0001CL_LOG)
      FROM ZSDT0001CLLG
     WHERE ID_CARGA    EQ @ME->CARGA-ID_CARGA
       AND DT_REGISTRO EQ @I_DT_REGISTRO
       AND HR_REGISTRO EQ @I_HR_REGISTRO
       AND US_REGISTRO EQ @I_US_REGISTRO.

    SELECT *
      INTO TABLE @DATA(IT_ZSDT0001RS_LOG)
      FROM ZSDT0001RSLG
     WHERE ID_CARGA    EQ @ME->CARGA-ID_CARGA
       AND DT_REGISTRO EQ @I_DT_REGISTRO
       AND HR_REGISTRO EQ @I_HR_REGISTRO
       AND US_REGISTRO EQ @I_US_REGISTRO.

    SELECT *
      INTO TABLE @DATA(IT_ZSDT0001RS_LOG_03)
      FROM ZSDT0001RSLG_03
     WHERE ID_CARGA    EQ @ME->CARGA-ID_CARGA
       AND DT_REGISTRO EQ @I_DT_REGISTRO
       AND HR_REGISTRO EQ @I_HR_REGISTRO
       AND US_REGISTRO EQ @I_US_REGISTRO.

    "Carga
    MOVE-CORRESPONDING WA_ZSDT0001CG_LOG TO E_APRESENTACAO-CARGA.

    READ TABLE IT_ZSDT0001CL_LOG INTO DATA(WA_ZSDT0001CL_LOG) WITH KEY ID_CLASSIFICACAO = WA_ZSDT0001CG_LOG-ID_CLASSIFICACAO.

    "Transgenia
    MOVE-CORRESPONDING WA_ZSDT0001CL_LOG TO E_APRESENTACAO-CARGA.

    IF WA_ZSDT0001CG_LOG-TP_STATUS NE ZIF_CARGA=>ST_STATUS_CONFERIDO.
      "Classificação
      LOOP AT IT_ZSDT0001RS_LOG INTO DATA(WA_RESULTADO).
        CASE WA_RESULTADO-TP_CARACTERISTICA.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_UMIDADE.
            E_APRESENTACAO-CARGA-NR_PERC_UMI = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_UMI = WA_RESULTADO-NR_QUANTIDADE_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_IMPUREZA.
            E_APRESENTACAO-CARGA-NR_PERC_IMP = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_IMP = WA_RESULTADO-NR_QUANTIDADE_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_AVARIADO.
            E_APRESENTACAO-CARGA-NR_PERC_AVA = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_AVA = WA_RESULTADO-NR_QUANTIDADE_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_ARDIDO.
            E_APRESENTACAO-CARGA-NR_PERC_ARD = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_ARD = WA_RESULTADO-NR_QUANTIDADE_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_QUEBRADO.
            E_APRESENTACAO-CARGA-NR_PERC_QUE = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_QUE = WA_RESULTADO-NR_QUANTIDADE_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_ESVERDEADO.
            E_APRESENTACAO-CARGA-NR_PERC_ESV = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_ESV = WA_RESULTADO-NR_QUANTIDADE_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_CARUNCHADO.
            E_APRESENTACAO-CARGA-NR_PERC_CAR = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_CAR = WA_RESULTADO-NR_QUANTIDADE_COM.
        ENDCASE.
      ENDLOOP.
      "SubCaracterística da Classificação
      LOOP AT IT_ZSDT0001RS_LOG_03 INTO DATA(WA_RESUL_AVARIADO).
        CASE WA_RESUL_AVARIADO-TP_SUB_CARAC_ESVERDEADO.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_ARQ.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_ARQ = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_QUE.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_QUE = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_MOF.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_MOF = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_PIC.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_PIC = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_FER.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_FER = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_GER.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_GER = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_ARD.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_ARD = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_GES.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_GES = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
        ENDCASE.
      ENDLOOP.
    ELSE.
      "Classificação
      LOOP AT IT_ZSDT0001RS_LOG INTO WA_RESULTADO WHERE ID_CLASSIFICACAO EQ WA_ZSDT0001CG_LOG-ID_CLASSIFICACAO.
        CASE WA_RESULTADO-TP_CARACTERISTICA.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_UMIDADE.
            E_APRESENTACAO-CARGA-NR_PERC_UMI = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_UMI = WA_RESULTADO-NR_QUANTIDADE_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_IMPUREZA.
            E_APRESENTACAO-CARGA-NR_PERC_IMP = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_IMP = WA_RESULTADO-NR_QUANTIDADE_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_AVARIADO.
            E_APRESENTACAO-CARGA-NR_PERC_AVA = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_AVA = WA_RESULTADO-NR_QUANTIDADE_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_ARDIDO.
            E_APRESENTACAO-CARGA-NR_PERC_ARD = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_ARD = WA_RESULTADO-NR_QUANTIDADE_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_QUEBRADO.
            E_APRESENTACAO-CARGA-NR_PERC_QUE = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_QUE = WA_RESULTADO-NR_QUANTIDADE_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_ESVERDEADO.
            E_APRESENTACAO-CARGA-NR_PERC_ESV = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_ESV = WA_RESULTADO-NR_QUANTIDADE_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_CARUNCHADO.
            E_APRESENTACAO-CARGA-NR_PERC_CAR = WA_RESULTADO-NR_PERCENTUAL_COM.
            E_APRESENTACAO-CARGA-NR_QTDE_CAR = WA_RESULTADO-NR_QUANTIDADE_COM.
        ENDCASE.
      ENDLOOP.
      "SubCaracterística da Classificação
      LOOP AT IT_ZSDT0001RS_LOG_03 INTO WA_RESUL_AVARIADO WHERE ID_CLASSIFICACAO EQ WA_ZSDT0001CG_LOG-ID_CLASSIFICACAO.
        CASE WA_RESUL_AVARIADO-TP_SUB_CARAC_ESVERDEADO.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_ARQ.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_ARQ = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_QUE.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_QUE = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_MOF.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_MOF = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_PIC.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_PIC = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_FER.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_FER = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_GER.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_GER = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_ARD.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_ARD = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_GES.
            E_APRESENTACAO-CARGA-NR_PERC_AVA_GES = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    SELECT SINGLE NAME INTO E_APRESENTACAO-CARGA-NAME
      FROM J_1BBRANCH
     WHERE BUKRS  EQ E_APRESENTACAO-CARGA-ID_BUKRS
       AND BRANCH EQ E_APRESENTACAO-CARGA-ID_BRANCH.

    IF E_APRESENTACAO-CARGA-ID_LOCAL_COLETA IS NOT INITIAL.
      SELECT SINGLE NAME1
        INTO E_APRESENTACAO-CARGA-DS_LOCAL_COLETA
        FROM LFA1 WHERE LIFNR EQ E_APRESENTACAO-CARGA-ID_LOCAL_COLETA.
    ENDIF.

    IF E_APRESENTACAO-CARGA-ID_LOCAL_DESCARGA IS NOT INITIAL.
      SELECT SINGLE NAME1
        INTO E_APRESENTACAO-CARGA-DS_LOCAL_DESCARGA
        FROM KNA1 WHERE KUNNR EQ E_APRESENTACAO-CARGA-ID_LOCAL_DESCARGA.
    ENDIF.

    IF E_APRESENTACAO-CARGA-ID_LOCAL_DESTINO IS NOT INITIAL.
      SELECT SINGLE NAME1
        INTO E_APRESENTACAO-CARGA-DS_LOCAL_DESTINO
        FROM LFA1 WHERE LIFNR EQ E_APRESENTACAO-CARGA-ID_LOCAL_DESTINO.
    ENDIF.

    IF E_APRESENTACAO-CARGA-ID_LOCAL_ENTREGA IS NOT INITIAL.
      SELECT SINGLE DS_LOCAL_ENTREGA
        INTO E_APRESENTACAO-CARGA-DS_LOCAL_ENTREGA
        FROM ZSDT0001LE WHERE ID_LOCAL_ENTREGA EQ E_APRESENTACAO-CARGA-ID_LOCAL_ENTREGA.
    ENDIF.

    IF E_APRESENTACAO-CARGA-ID_PROPRIETARIO IS NOT INITIAL.
      SELECT SINGLE NAME1
        INTO E_APRESENTACAO-CARGA-DS_PROPRIETARIO
        FROM LFA1 WHERE LIFNR EQ E_APRESENTACAO-CARGA-ID_PROPRIETARIO.
    ENDIF.

    IF E_APRESENTACAO-CARGA-ID_OUTRO_PARTIC IS NOT INITIAL.
      SELECT SINGLE NAME1
        INTO E_APRESENTACAO-CARGA-DS_OUTRO_PARTIC
        FROM LFA1 WHERE LIFNR EQ E_APRESENTACAO-CARGA-ID_OUTRO_PARTIC.
    ENDIF.

    IF E_APRESENTACAO-CARGA-ID_AGENT_FRETE IS NOT INITIAL.
      SELECT SINGLE NAME1
        INTO E_APRESENTACAO-CARGA-DS_AGENT_FRETE
        FROM LFA1 WHERE LIFNR EQ E_APRESENTACAO-CARGA-ID_AGENT_FRETE.
    ENDIF.

    IF E_APRESENTACAO-CARGA-ID_MOTORISTA IS NOT INITIAL.
      SELECT SINGLE NAME1
        INTO E_APRESENTACAO-CARGA-DS_MOTORISTA
        FROM LFA1 WHERE LIFNR EQ E_APRESENTACAO-CARGA-ID_MOTORISTA.
    ENDIF.

    IF E_APRESENTACAO-CARGA-ID_CLASSIFICADORA IS NOT INITIAL.
      SELECT SINGLE NAME1
        INTO E_APRESENTACAO-CARGA-DS_CLASSIFICADORA
        FROM LFA1 WHERE LIFNR EQ E_APRESENTACAO-CARGA-ID_CLASSIFICADORA.
    ENDIF.

    IF E_APRESENTACAO-CARGA-ID_PRODUTO IS NOT INITIAL.
      SELECT SINGLE MAKTX
        INTO E_APRESENTACAO-CARGA-DS_PRODUTO
        FROM MAKT
       WHERE MATNR EQ E_APRESENTACAO-CARGA-ID_PRODUTO
         AND SPRAS EQ SY-LANGU.
    ENDIF.

    IF E_APRESENTACAO-CARGA-ID_ORDEM IS NOT INITIAL.
      DATA(R_ORDEM_CARRGAMENTO) = ZCL_ORDEM_CARREGAMENTO=>BUSCA_ORDEM_CARREGAMENTO( I_ID_ORDEM = E_APRESENTACAO-CARGA-ID_ORDEM ).
      E_APRESENTACAO-CARGA-NR_ORDEM = R_ORDEM_CARRGAMENTO-NR_ORDEM.
      E_APRESENTACAO-ORDEM_CARREGA  = R_ORDEM_CARRGAMENTO.
    ENDIF.

    "Documentos Fiscais
    LOOP AT IT_ZSDT0001NT_LOG INTO DATA(WA_NOTA).

      CLEAR: WA_NOTA_ALV.

      MOVE-CORRESPONDING WA_NOTA TO WA_NOTA_ALV.

      SELECT SINGLE NAME1
        INTO WA_NOTA_ALV-DS_FORNECEDOR
        FROM LFA1 WHERE LIFNR EQ WA_NOTA_ALV-ID_FORNECEDOR.

      WA_NOTA_ALV-ID_PRODUTO = E_APRESENTACAO-CARGA-ID_PRODUTO.
      WA_NOTA_ALV-DS_PRODUTO = E_APRESENTACAO-CARGA-DS_PRODUTO.

      SELECT SINGLE DS_ENTRADA
        INTO WA_NOTA_ALV-DS_ENTRADA
        FROM ZSDT0001TETX WHERE ID_ENTRADA EQ WA_NOTA-ID_ENTRADA.

      IF WA_NOTA_ALV-ID_ENTREGUE_POR IS NOT INITIAL.
        SELECT SINGLE NAME1
          INTO WA_NOTA_ALV-DS_ENTREGUE_POR
          FROM LFA1 WHERE LIFNR EQ WA_NOTA_ALV-ID_ENTREGUE_POR.
      ENDIF.

      IF WA_ZSDT0001CG_LOG-TP_STATUS EQ ZIF_CARGA=>ST_STATUS_CONFERIDO.
        "Classificação
        LOOP AT IT_ZSDT0001RS_LOG INTO WA_RESULTADO WHERE ID_CLASSIFICACAO EQ WA_NOTA-ID_CLASSIFICACAO.
          CASE WA_RESULTADO-TP_CARACTERISTICA.
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_UMIDADE.
              WA_NOTA_ALV-NR_PERC_UMI = WA_RESULTADO-NR_PERCENTUAL_COM.
              WA_NOTA_ALV-NR_QTDE_UMI = WA_RESULTADO-NR_QUANTIDADE_COM.
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_IMPUREZA.
              WA_NOTA_ALV-NR_PERC_IMP = WA_RESULTADO-NR_PERCENTUAL_COM.
              WA_NOTA_ALV-NR_QTDE_IMP = WA_RESULTADO-NR_QUANTIDADE_COM.
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_AVARIADO.
              WA_NOTA_ALV-NR_PERC_AVA = WA_RESULTADO-NR_PERCENTUAL_COM.
              WA_NOTA_ALV-NR_QTDE_AVA = WA_RESULTADO-NR_QUANTIDADE_COM.
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_ARDIDO.
              WA_NOTA_ALV-NR_PERC_ARD = WA_RESULTADO-NR_PERCENTUAL_COM.
              WA_NOTA_ALV-NR_QTDE_ARD = WA_RESULTADO-NR_QUANTIDADE_COM.
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_QUEBRADO.
              WA_NOTA_ALV-NR_PERC_QUE = WA_RESULTADO-NR_PERCENTUAL_COM.
              WA_NOTA_ALV-NR_QTDE_QUE = WA_RESULTADO-NR_QUANTIDADE_COM.
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_ESVERDEADO.
              WA_NOTA_ALV-NR_PERC_ESV = WA_RESULTADO-NR_PERCENTUAL_COM.
              WA_NOTA_ALV-NR_QTDE_ESV = WA_RESULTADO-NR_QUANTIDADE_COM.
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_CARUNCHADO.
              WA_NOTA_ALV-NR_PERC_CAR = WA_RESULTADO-NR_PERCENTUAL_COM.
              WA_NOTA_ALV-NR_QTDE_CAR = WA_RESULTADO-NR_QUANTIDADE_COM.
          ENDCASE.
        ENDLOOP.
        "SubCaracterística da Classificação
        LOOP AT IT_ZSDT0001RS_LOG_03 INTO WA_RESUL_AVARIADO WHERE ID_CLASSIFICACAO EQ WA_NOTA-ID_CLASSIFICACAO.
          CASE WA_RESUL_AVARIADO-TP_SUB_CARAC_ESVERDEADO.
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_ARQ.
              WA_NOTA_ALV-NR_PERC_AVA_ARQ = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_QUE.
              WA_NOTA_ALV-NR_PERC_AVA_QUE = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_MOF.
              WA_NOTA_ALV-NR_PERC_AVA_MOF = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_PIC.
              WA_NOTA_ALV-NR_PERC_AVA_PIC = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_FER.
              WA_NOTA_ALV-NR_PERC_AVA_FER = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_GER.
              WA_NOTA_ALV-NR_PERC_AVA_GER = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_ARD.
              WA_NOTA_ALV-NR_PERC_AVA_ARD = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_GES.
              WA_NOTA_ALV-NR_PERC_AVA_GES = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
          ENDCASE.
        ENDLOOP.
      ENDIF.

      APPEND WA_NOTA_ALV TO E_APRESENTACAO-NOTAS.
    ENDLOOP.

    "Take UP's da Nota Fiscal
    LOOP AT IT_ZSDT0001TK_LOG INTO DATA(WA_ZSDT0001TK).
      CLEAR: WA_TAKEUP_ALV.
      MOVE-CORRESPONDING WA_ZSDT0001TK TO WA_TAKEUP_ALV.
      APPEND WA_TAKEUP_ALV TO E_APRESENTACAO-TAKEUP.
    ENDLOOP.

    "Blocos
    LOOP AT IT_ZSDT0001FD_LOG INTO DATA(WA_ZSDT0001FD).
      CLEAR: WA_BLOCO_ALV.
      MOVE-CORRESPONDING WA_ZSDT0001FD TO WA_BLOCO_ALV.

      SELECT SINGLE NAME1 INTO @WA_BLOCO_ALV-DS_PONTO_C
        FROM LFA1
       WHERE LIFNR EQ @WA_BLOCO_ALV-CD_PONTO_C.

      APPEND WA_BLOCO_ALV TO E_APRESENTACAO-BLOCOS.
    ENDLOOP.

    "Ordens de Venda
    LOOP AT IT_ZSDT0001OV_LOG INTO DATA(WA_ORDEM).
      CLEAR: WA_ORDEM_ALV.
      MOVE-CORRESPONDING WA_ORDEM TO WA_ORDEM_ALV.
      IF WA_ORDEM-NR_ORDEM_VENDA IS NOT INITIAL.
        TRY .
            ME->GET_INFO_ORDEM_VENDA( EXPORTING I_ORDEM_VENDA = WA_ORDEM-NR_ORDEM_VENDA IMPORTING E_ORDEM = DATA(R_ORDEM) ).
            WA_ORDEM_ALV-TP_TIPO_ORDEM  = R_ORDEM-TP_TIPO_ORDEM.
            WA_ORDEM_ALV-DS_TIPO_ORDEM  = R_ORDEM-DS_TIPO_ORDEM.
            WA_ORDEM_ALV-DS_TIPO_FRETE  = R_ORDEM-DS_TIPO_FRETE.
            WA_ORDEM_ALV-ID_PRODUTO     = R_ORDEM-ID_PRODUTO.
            WA_ORDEM_ALV-DS_PRODUTO     = R_ORDEM-DS_PRODUTO.
          CATCH ZCX_CARGA.    "
          CATCH ZCX_PARCEIROS.    "
        ENDTRY.
      ENDIF.
      APPEND WA_ORDEM_ALV TO E_APRESENTACAO-ORDEM_VENDA.
    ENDLOOP.

    LOOP AT IT_ZSDT0001TK_LOG INTO DATA(WA_TAKE_UP).
      READ TABLE E_APRESENTACAO-NOTAS WITH KEY ID_NOTA = WA_TAKE_UP-ID_NOTA INTO WA_NOTA_ALV.
      IF SY-SUBRC IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      APPEND VALUE #( ID_CARGA = WA_TAKE_UP-ID_CARGA
                      ID_NOTA = WA_TAKE_UP-ID_NOTA
                      DS_FORNECEDOR = WA_NOTA_ALV-DS_FORNECEDOR
                      ID_MOD_FISCAL = WA_NOTA_ALV-ID_MOD_FISCAL
                      NR_NOTA = WA_NOTA_ALV-NR_NOTA
                      NM_SERIE = WA_NOTA_ALV-NM_SERIE
                      DT_EMISSAO = WA_NOTA_ALV-DT_EMISSAO
                      ID_TAKEUP = WA_TAKE_UP-ID_TAKEUP
                      NU_BLOCO = WA_TAKE_UP-NU_BLOCO
                      QT_VINCULADA = WA_TAKE_UP-QT_VINCULADA ) TO E_APRESENTACAO-TAKEUP.
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_INFO_MESSAGEM_ESTORNO.

    DATA: R_MESSAGE_V1 TYPE RANGE OF SYMSGV,
          R_MESSAGE_V2 TYPE RANGE OF SYMSGV,
          R_MESSAGE_V3 TYPE RANGE OF SYMSGV,
          R_MESSAGE_V4 TYPE RANGE OF SYMSGV.

    R_CARGA = ME.

    CLEAR: E_MENSAGEM.

    IF I_MESSAGE_V1 IS NOT INITIAL.
      R_MESSAGE_V1 = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = I_MESSAGE_V1 HIGH = I_MESSAGE_V1 ) ).
    ENDIF.

    IF I_MESSAGE_V2 IS NOT INITIAL.
      R_MESSAGE_V2 = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = I_MESSAGE_V2 HIGH = I_MESSAGE_V2 ) ).
    ENDIF.

    IF I_MESSAGE_V3 IS NOT INITIAL.
      R_MESSAGE_V3 = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = I_MESSAGE_V3 HIGH = I_MESSAGE_V3 ) ).
    ENDIF.

    IF I_MESSAGE_V4 IS NOT INITIAL.
      R_MESSAGE_V4 = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = I_MESSAGE_V4 HIGH = I_MESSAGE_V4 ) ).
    ENDIF.

    SELECT SINGLE * INTO E_MENSAGEM
     FROM ZOB_MENSAGEM
    WHERE OBJ_KEY    EQ I_OBJ_KEY
      AND INTERFACE  EQ I_INTERFACE
      AND TYPE       EQ 'S'
      AND ID         EQ 'MM'
      AND NUM        EQ '899'
      AND MESSAGE_V1 IN R_MESSAGE_V1
      AND MESSAGE_V2 IN R_MESSAGE_V2
      AND MESSAGE_V3 IN R_MESSAGE_V3
      AND MESSAGE_V4 IN R_MESSAGE_V4.

    IF SY-SUBRC IS NOT INITIAL.

      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_MSG_ESTORNO_ERRO-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_MSG_ESTORNO_ERRO-MSGNO
                            ATTR1 = CONV #( I_INTERFACE )
                            ATTR2 = CONV #( I_OBJ_KEY ) )
          MSGTY  = 'E'
          MSGID  = ZCX_CARGA=>ZCX_MSG_ESTORNO_ERRO-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_MSG_ESTORNO_ERRO-MSGNO
          MSGV1  = CONV #( I_INTERFACE )
          MSGV2  = CONV #( I_OBJ_KEY ).

    ENDIF.


  ENDMETHOD.


  METHOD ZIF_CARGA~GET_INFO_ORDEM_VENDA.

    R_CARGA = ME.

    SELECT K~VBELN,
           K~AUART,
           T~BEZEI,
           D~INCO1,
           P~MATNR,
           P~CHARG,
           M~MAKTX,
           K~KVGR3,
           K~KVGR5
      INTO TABLE @DATA(IT_TAB)
      FROM VBAK AS K
     INNER JOIN VBAP  AS P ON P~VBELN EQ K~VBELN
     INNER JOIN MAKT  AS M ON M~MATNR EQ P~MATNR AND M~SPRAS EQ @SY-LANGU
     INNER JOIN TVAKT AS T ON T~AUART EQ K~AUART AND T~SPRAS EQ @SY-LANGU
     INNER JOIN VBKD  AS D ON D~VBELN EQ K~VBELN AND D~POSNR EQ '000000'
     WHERE K~VBELN = @I_ORDEM_VENDA.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_VENDA_NOT_FOUND-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_ORDEM_VENDA_NOT_FOUND-MSGNO )
          MSGID  = ZCX_CARGA=>ZCX_ORDEM_VENDA_NOT_FOUND-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_ORDEM_VENDA_NOT_FOUND-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    READ TABLE IT_TAB INDEX 1 INTO DATA(WA_TAB).

    E_ORDEM-NR_ORDEM_VENDA = WA_TAB-VBELN.
    E_ORDEM-TP_TIPO_ORDEM  = WA_TAB-AUART.
    E_ORDEM-DS_TIPO_ORDEM  = WA_TAB-BEZEI.
    E_ORDEM-DS_TIPO_FRETE  = WA_TAB-INCO1.
    E_ORDEM-ID_PRODUTO     = WA_TAB-MATNR.
    E_ORDEM-DS_PRODUTO     = WA_TAB-MAKTX.
    E_ORDEM-NR_SAFRA       = WA_TAB-CHARG.
    E_ORDEM-KVGR3          = WA_TAB-KVGR3.
    E_ORDEM-KVGR5          = WA_TAB-KVGR5.
    TRY .
        ZCL_ORDEM_VENDA=>ZIF_ORDEM_VENDA~GET_ORDEM_VENDA_SALDO( EXPORTING I_VBELN = WA_TAB-VBELN IMPORTING E_SALDO = DATA(E_SALDO) ).
      CATCH ZCX_ORDEM_VENDA.
    ENDTRY.
"MM - Tratar Dump ZMM0127 - #135174 - BG
    if E_SALDO > 999999999.
      MESSAGE 'Quantidade da OV não pode ser maior que 999.999.999' TYPE 'E'.
      exit.
    else.
        E_ORDEM-NM_SALDO_OV    = E_SALDO.
    endif.
  ENDMETHOD.


  METHOD ZIF_CARGA~GET_INFO_PEDIDO_COMPRA.

    R_CARGA = ME.

    SELECT K~EBELN AS EBELN,
           K~BUKRS AS ID_EMPRESA_SAIDA,
           K~RESWK AS ID_FILIAL_SAIDA,
           G~CHARG AS DS_SAFRA,
           K~BSART AS TIPO_PEDIDO,
           P~WERKS AS ID_LOCAL_DESTINO,
           P~MATNR AS MATNR,
           M~MAKTX AS MAKTX,
           T~BSART AS BSART,
           T~BATXT AS BATXT,
           P~INCO1 AS INCO1,
           P~ZCKFRETEENT AS ZCKFRETEENT
      INTO TABLE @DATA(IT_TAB)
      FROM EKKO AS K
     INNER JOIN EKPO  AS P ON P~EBELN EQ K~EBELN
     INNER JOIN EKET  AS G ON G~EBELN EQ P~EBELN AND G~EBELP EQ P~EBELP
     INNER JOIN MAKT  AS M ON M~MATNR EQ P~MATNR AND M~SPRAS EQ @SY-LANGU
     INNER JOIN T161T AS T ON T~BSART EQ K~BSART AND T~SPRAS EQ @SY-LANGU
     WHERE K~EBELN EQ @I_PEDIDO_COMPRA
       AND K~STATU EQ '9'.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_NAO_ENC-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_NAO_ENC-MSGNO )
          MSGID  = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_NAO_ENC-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_NAO_ENC-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    READ TABLE IT_TAB INDEX 1 INTO DATA(WA_TAB).

    E_PEDIDO-NR_PEDIDO_COMPRA    = WA_TAB-EBELN.
    E_PEDIDO-TP_TIPO_PEDIDO      = WA_TAB-BSART.
    E_PEDIDO-DS_TIPO_PEDIDO      = WA_TAB-BATXT.
    E_PEDIDO-DS_TIPO_FRETE       = WA_TAB-INCO1.
    E_PEDIDO-ID_PRODUTO          = WA_TAB-MATNR.
    E_PEDIDO-DS_PRODUTO          = WA_TAB-MAKTX.
    E_PEDIDO-ID_CENTRO_RECEBEDOR = WA_TAB-ID_LOCAL_DESTINO.
    E_PEDIDO-ID_CENTRO_SAIDA     = WA_TAB-ID_FILIAL_SAIDA.
    E_PEDIDO-NR_SAFRA            = WA_TAB-DS_SAFRA.
    E_PEDIDO-ZCKFRETEENT         = WA_TAB-ZCKFRETEENT.

    SELECT SINGLE * INTO @DATA(WA_PR)
      FROM EKPA
     WHERE EBELN EQ @WA_TAB-EBELN
       AND EBELP EQ '00000'
       AND PARVW EQ 'PR'.

    IF SY-SUBRC IS INITIAL.
      E_PEDIDO-ID_LOCAL_COLETA = WA_PR-LIFN2.
    ENDIF.

  ENDMETHOD.


  method zif_carga~get_info_placa.

    r_carga = me.

    check i_placa is not initial.

    select single * into e_zlest0002
      from zlest0002
     where pc_veiculo eq i_placa.

    if sy-subrc is not initial and i_tipo_frete eq zif_carga=>st_tp_frete_cif.

      raise exception type zcx_carga
        exporting
          textid    = value #( msgid = zcx_carga=>zcx_placa_sem_cadastro-msgid
                               msgno = zcx_carga=>zcx_placa_sem_cadastro-msgno
                               attr1 = conv #( i_placa ) )
          msgid     = zcx_carga=>zcx_placa_sem_cadastro-msgid
          msgno     = zcx_carga=>zcx_placa_sem_cadastro-msgno
          msgty     = 'E'
          msgv1     = conv #( i_placa )
          transacao = 'ZLES0003'.

    endif.

    check i_validar eq abap_true.

    "Valida Cadastro
    if sy-subrc is initial.

      if i_tipo_frete ne zif_carga=>st_tp_frete_cif and i_tracao = abap_true. "IR221460 14.02.2025
        if e_zlest0002-proprietario ne me->carga-id_proprietario and me->carga-id_proprietario is not initial.
          raise exception type zcx_carga
            exporting
              textid    = value #( msgid = zcx_carga=>zcx_erro_proprietario-msgid
                                   msgno = zcx_carga=>zcx_erro_proprietario-msgno
                                   attr1 = conv #( i_placa )
                                   attr2 = conv #( me->carga-id_proprietario ) )
              msgid     = zcx_carga=>zcx_erro_proprietario-msgid
              msgno     = zcx_carga=>zcx_erro_proprietario-msgno
              msgty     = 'E'
              msgv1     = conv #( i_placa )
              msgv2     = conv #( me->carga-id_proprietario )
              transacao = 'ZLES0003'.
        endif.
      endif.

*068  Placa &1 não é de Tração!
      if e_zlest0002-tp_veiculo ne '0' and i_tracao eq abap_true.
        raise exception type zcx_carga
          exporting
            textid    = value #( msgid = zcx_carga=>zcx_placa_nao_tracao-msgid
                                 msgno = zcx_carga=>zcx_placa_nao_tracao-msgno
                                 attr1 = conv #( i_placa ) )
            msgid     = zcx_carga=>zcx_placa_nao_tracao-msgid
            msgno     = zcx_carga=>zcx_placa_nao_tracao-msgno
            msgty     = 'E'
            msgv1     = conv #( i_placa )
            transacao = 'ZLES0003'.
      endif.

*069  Placa &1 não é de Reboque!
      if e_zlest0002-tp_veiculo ne '1' and i_tracao eq abap_false.
        raise exception type zcx_carga
          exporting
            textid    = value #( msgid = zcx_carga=>zcx_placa_nao_reboque-msgid
                                 msgno = zcx_carga=>zcx_placa_nao_reboque-msgno
                                 attr1 = conv #( i_placa ) )
            msgid     = zcx_carga=>zcx_placa_nao_reboque-msgid
            msgno     = zcx_carga=>zcx_placa_nao_reboque-msgno
            msgty     = 'E'
            msgv1     = conv #( i_placa )
            transacao = 'ZLES0003'.
      endif.

*070  Veículo &1 sem informação de Qtde de Eixo!
      if e_zlest0002-qt_eixo is initial.
        raise exception type zcx_carga
          exporting
            textid    = value #( msgid = zcx_carga=>zcx_placa_sem_qtd_eixo-msgid
                                 msgno = zcx_carga=>zcx_placa_sem_qtd_eixo-msgno
                                 attr1 = conv #( i_placa ) )
            msgid     = zcx_carga=>zcx_placa_sem_qtd_eixo-msgid
            msgno     = zcx_carga=>zcx_placa_sem_qtd_eixo-msgno
            msgty     = 'E'
            msgv1     = conv #( i_placa )
            transacao = 'ZLES0003'.
      endif.

      if i_tipo_frete eq zif_carga=>st_tp_frete_cif.

        call method zcl_webservice_tipcard=>cons_situacao_transportador
          exporting
            i_placa     = i_placa
          receiving
            e_consultas = data(e_consultas)
          exceptions
            erro        = 1
            webservice  = 2
            others      = 3.

        if sy-subrc is not initial.
          message w000(zles) with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.

        read table e_consultas index 1 into data(wa_zlest0135).
        if wa_zlest0135-ck_rntrc_ativo eq abap_false.
          sy-msgv1 = wa_zlest0135-ds_msg_transportador+000(50).
          sy-msgv2 = wa_zlest0135-ds_msg_transportador+050(50).
          sy-msgv3 = wa_zlest0135-ds_msg_transportador+100(50).
          sy-msgv4 = wa_zlest0135-ds_msg_transportador+150(50)..
          message w000(zles) with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.
      endif.

    elseif i_tipo_frete eq zif_carga=>st_tp_frete_cpt.

      call method zcl_webservice_tipcard=>cons_situacao_transportador
        exporting
          i_placa     = i_placa
          i_partiner  = me->carga-id_agent_frete
        receiving
          e_consultas = e_consultas
        exceptions
          erro        = 1
          webservice  = 2
          others      = 3.

      if sy-subrc is not initial.
        message w000(zles) with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      read table e_consultas index 1 into wa_zlest0135.
      if wa_zlest0135-ck_rntrc_ativo eq abap_false.
        sy-msgv1 = wa_zlest0135-ds_msg_transportador+000(50).
        sy-msgv2 = wa_zlest0135-ds_msg_transportador+050(50).
        sy-msgv3 = wa_zlest0135-ds_msg_transportador+100(50).
        sy-msgv4 = wa_zlest0135-ds_msg_transportador+150(50)..
        message w000(zles) with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

    endif.

  endmethod.


  METHOD ZIF_CARGA~GET_INSTANCE.

    IF ZIF_CARGA~AT_CARGA IS NOT BOUND.
      CREATE OBJECT ZIF_CARGA~AT_CARGA TYPE ZCL_CARGA_RECEBIMENTO_V0001.
      R_INSTANCE = ZIF_CARGA~AT_CARGA.
    ELSE.
      R_INSTANCE = ZIF_CARGA~AT_CARGA.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_LOGS_HISTORICO.

    DATA: WA_LOGS    TYPE ZDE_LOG_REGISTRO.

    R_CARGA = ME.

    CLEAR: E_LOGS[], E_LOGS.

    TRY.
        EXEC SQL.
          OPEN DOCUMENTOS FOR
            SELECT * FROM (
            SELECT F.DT_REGISTRO, F.HR_REGISTRO, F.US_REGISTRO
* ---> S4 Migration - 26/07/2023 - MG-5875 - RF
*              FROM SAPHANADB.ZSDT0001CGLG F
             FROM SAPHANADB.ZSDT0001CGLG F
* <--- S4 Migration - 26/07/2023 - MG-5875 - RF
             WHERE ID_CARGA = :ME->CARGA-ID_CARGA ) TT
             ORDER BY DT_REGISTRO, HR_REGISTRO
        ENDEXEC.
      CATCH CX_SY_NATIVE_SQL_ERROR INTO DATA(EXC_REF).
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ERRO_PSQ_BANCO_DADOS-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_ERRO_PSQ_BANCO_DADOS-MSGNO )
            MSGID  = ZCX_CARGA=>ZCX_ERRO_PSQ_BANCO_DADOS-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_ERRO_PSQ_BANCO_DADOS-MSGNO
            MSGTY  = 'E'.
    ENDTRY.

    DO.
      EXEC SQL.
        FETCH NEXT DOCUMENTOS INTO
        :WA_LOGS-DT_REGISTRO,
        :WA_LOGS-HR_REGISTRO,
        :WA_LOGS-US_REGISTRO
      ENDEXEC.
      IF SY-SUBRC <> 0.
        EXIT.
      ELSE.
        APPEND WA_LOGS TO E_LOGS.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE DOCUMENTOS
    ENDEXEC.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_MENS_INTERFACE_ENTRADA.

    DATA: WA_MENSAGENS LIKE LINE OF E_MENSAGENS.

    R_CARGA = ME.

    CLEAR: E_MENSAGENS.

    SELECT * INTO TABLE @DATA(IT_NOTAS)
      FROM ZSDT0001NT
     WHERE ID_CARGA        EQ @I_ID_CARGA
       AND OBJ_KEY_ENTRADA NE @SPACE.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_SEM_REGISTRO_ENTRADA-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_SEM_REGISTRO_ENTRADA-MSGNO )
          MSGTY  = 'E'
          MSGNO  = ZCX_CARGA=>ZCX_SEM_REGISTRO_ENTRADA-MSGNO
          MSGID  = ZCX_CARGA=>ZCX_SEM_REGISTRO_ENTRADA-MSGID.
    ENDIF.

    SELECT * INTO TABLE @DATA(IT_MENSAGENS)
      FROM ZOB_MENSAGEM
       FOR ALL ENTRIES IN @IT_NOTAS
     WHERE OBJ_KEY EQ @IT_NOTAS-OBJ_KEY_ENTRADA.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_SEM_REGISTROS_MSG_INTEFACE-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_SEM_REGISTROS_MSG_INTEFACE-MSGNO )
          MSGTY  = 'E'
          MSGNO  = ZCX_CARGA=>ZCX_SEM_REGISTROS_MSG_INTEFACE-MSGNO
          MSGID  = ZCX_CARGA=>ZCX_SEM_REGISTROS_MSG_INTEFACE-MSGID.
    ENDIF.

    SORT IT_MENSAGENS BY SEQ_REGISTRO.

    LOOP AT IT_MENSAGENS INTO DATA(WA_MENSAGEM).
      MOVE-CORRESPONDING WA_MENSAGEM TO WA_MENSAGENS.
      CASE WA_MENSAGENS-TYPE.
        WHEN 'S'.
          WA_MENSAGENS-ST_MENSAGEM = ICON_LED_GREEN.
        WHEN 'E'.
          WA_MENSAGENS-ST_MENSAGEM = ICON_LED_RED.
        WHEN 'W'.
          WA_MENSAGENS-ST_MENSAGEM = ICON_LED_YELLOW.
        WHEN 'I'.
          WA_MENSAGENS-ST_MENSAGEM = ICON_HINT.
      ENDCASE.
      APPEND WA_MENSAGENS TO E_MENSAGENS.
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_NAME_JOB_SAIDA_AUTOMATICA.

    R_CARGA = ME.

    CONCATENATE 'JOB_SAIDA_AUTOMATICA' ME->CARGA-ID_CARGA INTO E_NAME SEPARATED BY '_'.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_NEW_ID_CARGA.

    CLEAR: E_ID_CARGA.

    R_CARGA = ME.

    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        OBJECT           = 'ZCARGA'
      EXCEPTIONS
        FOREIGN_LOCK     = 1
        OBJECT_NOT_FOUND = 2
        SYSTEM_FAILURE   = 3
        OTHERS           = 4.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = '01'
        OBJECT                  = 'ZCARGA'
        QUANTITY                = '00000000000000000001'
        IGNORE_BUFFER           = 'X'
      IMPORTING
        NUMBER                  = E_ID_CARGA
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

* Desbloqueia o objeto de numeração
    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
      EXPORTING
        OBJECT           = 'ZCARGA'
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.


  ENDMETHOD.


  METHOD ZIF_CARGA~GET_NEW_ID_CLASSIFICAO.

    R_CARGA = ME.

    CLEAR: E_ID_CLASSIFICACAO.

    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        OBJECT           = 'ZCLASSIFIC'
      EXCEPTIONS
        FOREIGN_LOCK     = 1
        OBJECT_NOT_FOUND = 2
        SYSTEM_FAILURE   = 3
        OTHERS           = 4.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = '01'
        OBJECT                  = 'ZCLASSIFIC'
        QUANTITY                = '00000000000000000001'
        IGNORE_BUFFER           = 'X'
      IMPORTING
        NUMBER                  = E_ID_CLASSIFICACAO
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

* Desbloqueia o objeto de numeração
    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
      EXPORTING
        OBJECT           = 'ZCLASSIFIC'
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.


  ENDMETHOD.


  METHOD ZIF_CARGA~GET_NEW_ID_ENTRADA_ESTOQUE.

    CLEAR: E_ID_CARGA.

    R_CARGA = ME.

    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        OBJECT           = 'ZENTESTOQU'
      EXCEPTIONS
        FOREIGN_LOCK     = 1
        OBJECT_NOT_FOUND = 2
        SYSTEM_FAILURE   = 3
        OTHERS           = 4.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = '01'
        OBJECT                  = 'ZENTESTOQU'
        QUANTITY                = '00000000000000000001'
        IGNORE_BUFFER           = 'X'
      IMPORTING
        NUMBER                  = E_ID_CARGA
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

* Desbloqueia o objeto de numeração
    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
      EXPORTING
        OBJECT           = 'ZENTESTOQU'
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD zif_carga~get_new_id_nota_propria.

    CLEAR: e_id_nota_propria.

    r_carga = me.

    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = 'ZNTPROPRIA'
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = sy-msgid
                            msgno = sy-msgno
                            attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgty  = 'E'
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZNTPROPRIA'
        quantity                = '00000000000000000001'
        ignore_buffer           = 'X'
      IMPORTING
        number                  = e_id_nota_propria
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
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = sy-msgid
                            msgno = sy-msgno
                            attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgty  = 'E'
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.

* Desbloqueia o objeto de numeração
    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
      EXPORTING
        object           = 'ZNTPROPRIA'
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = sy-msgid
                            msgno = sy-msgno
                            attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgty  = 'E'
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_NEW_ID_SOLICITACAO_MANUT.

    CLEAR: E_ID_SOLICITACAO.

    R_CARGA = ME.

    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        OBJECT           = 'ZCARGASA'
      EXCEPTIONS
        FOREIGN_LOCK     = 1
        OBJECT_NOT_FOUND = 2
        SYSTEM_FAILURE   = 3
        OTHERS           = 4.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = '01'
        OBJECT                  = 'ZCARGASA'
        QUANTITY                = '00000000000000000001'
        IGNORE_BUFFER           = 'X'
      IMPORTING
        NUMBER                  = E_ID_SOLICITACAO
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

* Desbloqueia o objeto de numeração
    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
      EXPORTING
        OBJECT           = 'ZCARGASA'
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_NOTA_FORNECEDOR_IE.

    DATA: OBJ_FORNECEDOR TYPE REF TO ZCL_FORNECEDORES.

    CREATE OBJECT OBJ_FORNECEDOR.

    TRY .
        OBJ_FORNECEDOR->ZIF_PARCEIROS~SET_PARCEIRO_IE( I_INSC_ESTATUAL = I_STCD3 )->GET_ID_PARCEIRO( IMPORTING E_PARCEIRO = E_NOTA-ID_FORNECEDOR )->GET_NAME( IMPORTING E_NAME = E_NOTA-DS_FORNECEDOR ).
      CATCH ZCX_PARCEIROS INTO DATA(EX_PARCEIROS).

        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_IE_ERRO_FORNECEDOR-MSGID
                              MSGNO  = ZCX_CARGA=>ZCX_IE_ERRO_FORNECEDOR-MSGNO
                              ATTR1  = CONV #( I_STCD3 ) )
            MSGID  = ZCX_CARGA=>ZCX_IE_ERRO_FORNECEDOR-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_IE_ERRO_FORNECEDOR-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( I_STCD3 ).

    ENDTRY.

    SELECT SINGLE * INTO @DATA(WA_PRODUTOR_VALIDA)
      FROM ZSDT0001PD
     WHERE ID_PRODUTOR EQ @E_NOTA-ID_FORNECEDOR
       AND NR_SAFRA    EQ @ME->CARGA-NR_SAFRA
       AND ID_BUKRS    EQ @ME->CARGA-ID_BUKRS
       AND ID_BRANCH   EQ @ME->CARGA-ID_BRANCH.

    IF SY-SUBRC IS NOT INITIAL.
      "095  Fornecedor &1 Não Parametrizado para Safra/Empresa/Filial: &2/&3/&4!
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_CARGA=>ZCX_FORN_SEM_PARAMETRO-MSGID
                            MSGNO  = ZCX_CARGA=>ZCX_FORN_SEM_PARAMETRO-MSGNO
                            ATTR1  = CONV #( E_NOTA-ID_FORNECEDOR )
                            ATTR2  = CONV #( ME->CARGA-NR_SAFRA )
                            ATTR3  = CONV #( ME->CARGA-ID_BUKRS )
                            ATTR4  = CONV #( ME->CARGA-ID_BRANCH ) )
          MSGID  = ZCX_CARGA=>ZCX_FORN_SEM_PARAMETRO-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_FORN_SEM_PARAMETRO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( E_NOTA-ID_FORNECEDOR )
          MSGV2  = CONV #( ME->CARGA-NR_SAFRA )
          MSGV3  = CONV #( ME->CARGA-ID_BUKRS )
          MSGV4  = CONV #( ME->CARGA-ID_BRANCH ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_PARTINER_ORDEM_VENDA.

*    DATA: LC_FUNCAO_PARTINER  TYPE PARVW.
*
*    CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
*      EXPORTING
*        INPUT  = I_FUNCAO_PARTINER
*      IMPORTING
*        OUTPUT = LC_FUNCAO_PARTINER.

    R_CARGA = ME.

    SELECT SINGLE *
      INTO @E_PARTINER
      FROM VBPA
     WHERE VBELN = @I_ORDEM_VENDA
       AND PARVW = @I_FUNCAO_PARTINER.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_VENDA_PARTINER-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_ORDEM_VENDA_PARTINER-MSGNO
                            ATTR1 = CONV #( I_FUNCAO_PARTINER ) )
          MSGID  = ZCX_CARGA=>ZCX_ORDEM_VENDA_NOT_FOUND-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_ORDEM_VENDA_NOT_FOUND-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_FUNCAO_PARTINER ).
    ENDIF.


  ENDMETHOD.


  METHOD zif_carga~get_pedido_compra.

    r_carga = me.

    READ TABLE me->documento_fiscal WITH KEY id_nota = i_id_nota INTO DATA(wa_nota).

    me->zif_carga~get_pedido_compra_geral(
      EXPORTING
        i_id_bukrs      = me->carga-id_bukrs
        i_id_entrada    = wa_nota-id_entrada
        i_id_mod_fiscal = wa_nota-id_mod_fiscal
        i_id_branch     = me->carga-id_branch
        i_nr_safra      = me->carga-nr_safra
        i_id_produto    = me->carga-id_produto
        i_id_fornecedor = wa_nota-id_fornecedor
      IMPORTING
        e_info_pedido   = e_info_pedido
    ).

  ENDMETHOD.


  METHOD zif_carga~get_pedido_compra_geral.

    DATA: lc_pedido         TYPE REF TO zcl_pedido_compra,
          lc_filtro         TYPE zde_filtro_pedido_compra,
          it_ekko           TYPE zde_ekko_t,
          lva_lgort_tp_prod TYPE lgort_d, "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940
          lva_msg_error     TYPE c LENGTH 250. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940


    "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 --->
    " DATA: WA_ZMMT0017_TP_PRODUTO TYPE ZMMT0017,
    "   WA_ZMMT0017            TYPE ZMMT0017.
    "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 <---

    DATA: lva_msg_erro TYPE c LENGTH 200.

    r_carga = me.

    DATA(lc_ck_nfe) = COND #( LET clet = i_id_mod_fiscal IN WHEN clet EQ zif_carga=>st_model_fiscal_eletronico THEN abap_true ELSE abap_false ).

    SELECT SINGLE * INTO @DATA(wa_zsdt0001te)
      FROM zsdt0001te
     WHERE id_entrada EQ @i_id_entrada
       AND id_empresa EQ @i_id_bukrs
       AND ck_nfe     EQ @lc_ck_nfe.

    CLEAR: e_info_pedido.

    lc_pedido = NEW #( ).

    "Buscar Centro a Fixar
    SELECT SINGLE * INTO @DATA(wa_afixar)
      FROM zsdt_depara_cen
     WHERE vkorg             EQ @i_id_bukrs
       AND centro_real       EQ @i_id_branch
       AND tp_centro_virtual EQ @zcl_pedido_compra=>st_tp_centro_a_fixar.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid    = VALUE #( msgid = zcx_carga=>zcx_centro_a_fixar-msgid
                               msgno = zcx_carga=>zcx_centro_a_fixar-msgno
                               attr1 = CONV #( i_id_branch )
                               attr2 = 'ZSDT0036' )
          msgid     = zcx_carga=>zcx_centro_a_fixar-msgid
          msgno     = zcx_carga=>zcx_centro_a_fixar-msgno
          msgty     = 'E'
          msgv1     = CONV #( i_id_branch )
          msgv2     = 'ZSDT0036'
          transacao = 'ZSDT0036'.
    ENDIF.

*    SELECT SINGLE *
*     FROM tvarvc
*      INTO @DATA(vl_tp_protuto)
*     WHERE name EQ 'BUSCA_TP_PRODUTO'
*      AND low EQ @sy-uname.
    "IF ( sy-subrc IS INITIAL )

    "CLEAR: wa_zmmt0017_tp_produto, wa_zmmt0017. "*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - inicio

    TRY.
        DATA(_found_param_lgort) = abap_false.
        CLEAR: lva_msg_error .
        IF wa_zsdt0001te-id_tp_operacao EQ zif_tp_operacao_carga_entrada=>st_retorno_armazenagem.

          me->get_factory_tp_transgenia( EXPORTING i_classificacao = me->classificacao IMPORTING e_tp_transgenia = DATA(e_tp_transgenia) ).

          e_tp_transgenia = COND #( WHEN e_tp_transgenia NE 'CO' THEN 'RR' ELSE e_tp_transgenia ).
*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - inicio

*      SELECT SINGLE * INTO wa_zmmt0017_tp_produto
*        FROM zmmt0017
*       WHERE matnr          EQ i_id_produto
*         AND centro_fixo    EQ i_id_branch
*         AND centro_a_fixar EQ wa_afixar-centrov_1
*         AND tp_produto     EQ e_tp_transgenia.

          zcl_deposito=>zif_deposito~get_instance(
                      )->get_deposito_material_filial(
                      EXPORTING
                        i_matnr          = i_id_produto
                        i_tp_produto     = e_tp_transgenia
                        i_bukrs          = i_id_bukrs
                        i_branch         = i_id_branch
                        i_eudr           = space
                      IMPORTING
                        e_lgort          = lva_lgort_tp_prod ).


          IF lva_lgort_tp_prod IS NOT INITIAL.
            _found_param_lgort = abap_true.
          ENDIF.

*      SELECT SINGLE * INTO WA_ZMMT0017
*        FROM ZMMT0017
*       WHERE MATNR          EQ I_ID_PRODUTO
*         AND CENTRO_FIXO    EQ I_ID_BRANCH
*         AND CENTRO_A_FIXAR EQ WA_AFIXAR-CENTROV_1
*         AND TP_PRODUTO     EQ SPACE.

          zcl_deposito=>zif_deposito~get_instance(
                    )->get_deposito_material_filial(
                    EXPORTING
                      i_matnr          = i_id_produto
                      i_tp_produto     = ' '
                      i_bukrs          = i_id_bukrs
                      i_branch         = i_id_branch
                      i_eudr           = space
                    IMPORTING
                      e_lgort          = DATA(lva_lgort)  ).

          IF lva_lgort IS NOT INITIAL.
            _found_param_lgort = abap_true.
          ENDIF.

        ELSE.

*      SELECT SINGLE * INTO WA_ZMMT0017
*        FROM ZMMT0017
*       WHERE MATNR          EQ I_ID_PRODUTO
*         AND CENTRO_FIXO    EQ I_ID_BRANCH
*         AND CENTRO_A_FIXAR EQ WA_AFIXAR-CENTROV_1
*         AND TP_PRODUTO     EQ SPACE.

          zcl_deposito=>zif_deposito~get_instance(
                      )->get_deposito_material_filial(
                      EXPORTING
                        i_matnr          = i_id_produto
                        i_tp_produto     = ' '
                        i_bukrs          = i_id_bukrs
                        i_branch         = i_id_branch
                        i_eudr           = space
                      IMPORTING
                        e_lgort          = lva_lgort ).

          IF lva_lgort IS NOT INITIAL.
            _found_param_lgort = abap_true.
          ENDIF.

        ENDIF.
      CATCH zcx_deposito INTO DATA(zcx_deposito). " Classe de Erro de Depósito
        lva_msg_error = zcx_deposito->zif_error~get_msg_erro( ).
    ENDTRY.

*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940  - fim
    IF _found_param_lgort EQ abap_false.

      IF lva_msg_error IS NOT INITIAL. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940  - fim
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid    = VALUE #( msgid = zcx_carga=>zcx_erro_geral-msgid
                                 msgno = zcx_carga=>zcx_erro_geral-msgno
                                 attr1 = CONV #( lva_msg_error+000(050) )
                                 attr2 = CONV #( lva_msg_error+050(050) )
                                 attr3 = CONV #( lva_msg_error+100(050) )
                                 attr4 = CONV #( lva_msg_error+150(050) ) )
            msgid     = zcx_carga=>zcx_erro_geral-msgid
            msgno     = zcx_carga=>zcx_erro_geral-msgno
            msgty     = 'E'
            msgv1     = CONV #( lva_msg_error+000(050) )
            msgv2     = CONV #( lva_msg_error+050(050) )
            msgv3     = CONV #( lva_msg_error+100(050) )
            msgv4     = CONV #( lva_msg_error+150(050) ).
      ELSE.

        "Centro &1 Centro a Fixar &2 e Material &3 sem Depósito! Transação &4!
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid    = VALUE #( msgid = zcx_carga=>zcx_centro_a_fixar_deposito-msgid
                                 msgno = zcx_carga=>zcx_centro_a_fixar_deposito-msgno
                                 attr1 = CONV #( i_id_branch )
                                 attr2 = CONV #( wa_afixar-centrov_1 )
                                 attr3 = CONV #( i_id_produto )
                                 attr4 = 'ZMM0017' )
            msgid     = zcx_carga=>zcx_centro_a_fixar_deposito-msgid
            msgno     = zcx_carga=>zcx_centro_a_fixar_deposito-msgno
            msgty     = 'E'
            msgv1     = CONV #( i_id_branch )
            msgv2     = CONV #( wa_afixar-centrov_1 )
            msgv3     = CONV #( i_id_produto )
            msgv4     = 'ZMM0017'
            transacao = 'ZMM0017'.
      ENDIF.
    ENDIF.

    DATA(lc_safra_filial) = |{ i_nr_safra }_{ i_id_branch }|.

    DATA(lc_tipo_pedido) = COND #( LET id_tp_operacao = wa_zsdt0001te-id_tp_operacao IN
                                     WHEN id_tp_operacao EQ zif_tp_operacao_carga_entrada=>st_compra_entrega_futura THEN 'ZGEF'
                                     WHEN id_tp_operacao EQ zif_tp_operacao_carga_entrada=>st_retorno_armazenagem   THEN 'ZARM'
                                     ELSE 'ZGR' ).

    DATA(lc_iwerks) = COND #( LET id_tp_operacao = wa_zsdt0001te-id_tp_operacao IN
                                     WHEN id_tp_operacao EQ zif_tp_operacao_carga_entrada=>st_compra_entrega_futura THEN wa_afixar-centrov_1
                                     WHEN id_tp_operacao EQ zif_tp_operacao_carga_entrada=>st_retorno_armazenagem   THEN wa_afixar-centro_real
                                     ELSE wa_afixar-centrov_1 ).

    "Buscar Tipo de Nota Fiscal
    SELECT SINGLE * INTO @DATA(wa_zsdt0001tetn)
      FROM zsdt0001tetn
     WHERE id_entrada EQ @wa_zsdt0001te-id_entrada.

    IF sy-subrc IS INITIAL.

      "Buscar Grupo de Mercadoria
      SELECT SINGLE * INTO @DATA(wa_zsdt0001mt)
        FROM zsdt0001mt
       WHERE matnr EQ @i_id_produto.

      IF sy-subrc IS INITIAL.

        "Buscar Iva por Tipo de Nota Fiscal / Grupo de Mercadoria / Empresa
        SELECT SINGLE * INTO @DATA(wa_zsdt0001tetniva)
          FROM zsdt0001tetniva
         WHERE id_empresa   EQ @i_id_bukrs
           AND id_tipo_nota EQ @wa_zsdt0001tetn-id_tipo_nota
           AND tp_grupo_ctb EQ @wa_zsdt0001mt-tp_grupo_ctb.

        IF sy-subrc IS INITIAL.
          wa_zsdt0001te-id_iva = wa_zsdt0001tetniva-id_iva.
        ENDIF.
      ENDIF.

    ENDIF.
    "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INICIO
*    IF WA_ZMMT0017_TP_PRODUTO-LGORT IS NOT INITIAL.
*      DATA(_LGORT) = WA_ZMMT0017_TP_PRODUTO-LGORT.
*    ELSE.
*      _LGORT = WA_ZMMT0017-LGORT.
*    ENDIF.
    "
    IF lva_lgort_tp_prod IS NOT INITIAL.
      DATA(_lgort) = lva_lgort_tp_prod.
    ELSE.
      _lgort = lva_lgort.
    ENDIF.
    "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940

    lc_filtro-ibukrs = VALUE #( sign = 'I'  option = 'EQ' ( low = i_id_bukrs    high = i_id_bukrs    ) ).
    lc_filtro-ilifnr = VALUE #( sign = 'I'  option = 'EQ' ( low = i_id_fornecedor high = i_id_fornecedor ) ).
    lc_filtro-imatnr = VALUE #( sign = 'I'  option = 'EQ' ( low = i_id_produto  high = i_id_produto  ) ).
    lc_filtro-icharg = VALUE #( sign = 'I'  option = 'EQ' ( low = i_nr_safra    high = i_nr_safra    ) ( low = lc_safra_filial high = lc_safra_filial ) ).
    lc_filtro-iwerks = VALUE #( sign = 'I'  option = 'EQ' ( low = lc_iwerks   high = lc_iwerks ) ).
    lc_filtro-ibstyp = VALUE #( sign = 'I'  option = 'EQ' ( low = 'F'     high = 'F'     ) ).
    lc_filtro-ibsart = VALUE #( sign = 'I'  option = 'EQ' ( low = lc_tipo_pedido high = lc_tipo_pedido ) ).
    lc_filtro-iekorg = VALUE #( sign = 'I'  option = 'EQ' ( low = 'OC01'  high = 'OC01'  ) ).
    lc_filtro-iekgrp = VALUE #( sign = 'I'  option = 'EQ' ( low = 'G01'   high = 'G01'   ) ).
    lc_filtro-ifrgrl = VALUE #( sign = 'I'  option = 'EQ' ( low = space   high = space   ) ).
    lc_filtro-iebelp = VALUE #( sign = 'I'  option = 'EQ' ( low = '00010' high = '00010' ) ).
    lc_filtro-imwskz = VALUE #( sign = 'I'  option = 'EQ' ( low = wa_zsdt0001te-id_iva  high = wa_zsdt0001te-id_iva  ) ).
    lc_filtro-ilgort = VALUE #( sign = 'I'  option = 'EQ' ( low = _lgort     high = _lgort     ) ).

**BUG SOLTO 133324
    APPEND VALUE #( sign = 'I'  option = 'EQ'  low = '0003'  high = '0003' ) TO lc_filtro-ibstae.
    APPEND VALUE #( sign = 'I'  option = 'EQ'  low = '0004'  high = '0004' ) TO lc_filtro-ibstae.
**BUG SOLTO 133324

*-CS2022000332-#78064-07.06.2022-JT-inicio
    IF me->zif_carga~carga-id_contrato IS NOT INITIAL.
      lc_filtro-isubmi = VALUE #( sign = 'I'  option = 'EQ' ( low = me->zif_carga~carga-id_contrato high = me->zif_carga~carga-id_contrato ) ).
    ELSE.
      CLEAR: lc_filtro-isubmi[].
    ENDIF.
*-CS2022000332-#78064-07.06.2022-JT-fim

    IF lc_pedido->zif_pesquisa~pesquisar( EXPORTING i_filtros  = lc_filtro IMPORTING e_registros = it_ekko ) NE abap_true.

      DATA(lva_pedido_not_found) = abap_true.

      IF _lgort IS NOT INITIAL. ""Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940
        CLEAR: lc_filtro-ilgort.
        lc_filtro-ilgort = VALUE #( sign = 'I'  option = 'EQ' ( low = _lgort high = _lgort  ) ).  ""Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940
        IF lc_pedido->zif_pesquisa~pesquisar( EXPORTING i_filtros  = lc_filtro IMPORTING e_registros = it_ekko ) EQ abap_true.
          CLEAR: lva_pedido_not_found.
        ENDIF.
      ENDIF.

      IF lva_pedido_not_found EQ abap_true.
        CONCATENATE 'Sem Pedido Forn.:' i_id_fornecedor 'Safra:' i_nr_safra 'Produto:' i_id_produto 'Tipo:' lc_tipo_pedido
                    'Ctrl.Confirm: 0004 - Receb. Cod.Imposto:' wa_zsdt0001te-id_iva  'Grp.Compr: G01  Org.Compras: OC01'
                    'Dep.:' _lgort
           INTO lva_msg_erro SEPARATED BY space.

        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_sem_pedido_compra-msgid
                              msgno = zcx_carga=>zcx_sem_pedido_compra-msgno
                              attr1 = CONV #( lva_msg_erro+000(050) )
                              attr2 = CONV #( lva_msg_erro+050(050) )
                              attr3 = CONV #( lva_msg_erro+100(050) )
                              attr4 = CONV #( lva_msg_erro+150(050) ) )
            msgid  = zcx_carga=>zcx_sem_pedido_compra-msgid
            msgno  = zcx_carga=>zcx_sem_pedido_compra-msgno
            msgty  = 'E'
            msgv1  = CONV #( lva_msg_erro+000(050) )
            msgv2  = CONV #( lva_msg_erro+050(050) )
            msgv3  = CONV #( lva_msg_erro+100(050) )
            msgv4  = CONV #( lva_msg_erro+150(050) ).
      ENDIF.

    ENDIF.

    READ TABLE it_ekko INDEX 1 INTO e_info_pedido-ekko.

    SELECT SINGLE * INTO e_info_pedido-ekpo
      FROM ekpo
     WHERE ebeln EQ e_info_pedido-ekko-ebeln
       AND ebelp EQ '00010'.

    SELECT SINGLE * INTO e_info_pedido-eket
      FROM eket
     WHERE ebeln EQ e_info_pedido-ekko-ebeln
       AND ebelp EQ '00010'.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_PRODUTOR_FILIAL_SAFRA.

    SELECT SINGLE * INTO @DATA(WA_PRODUTOR_VALIDA)
      FROM ZSDT0001PD
     WHERE ID_PRODUTOR EQ @ID_PRODUTOR
       AND NR_SAFRA    EQ @ID_NR_SAFRA
       AND ID_BUKRS    EQ @ID_BUKRS
       AND ID_BRANCH   EQ @ID_BRANCH.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_CARGA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_FORN_SEM_PARAMETRO-MSGID
                          MSGNO = ZCX_CARGA=>ZCX_FORN_SEM_PARAMETRO-MSGNO
                          ATTR1 = CONV #( ID_PRODUTOR )
                          ATTR2 = CONV #( ID_NR_SAFRA )
                          ATTR3 = CONV #( ID_BUKRS )
                          ATTR4 = CONV #( ID_BRANCH ) )
        MSGTY  = 'E'
        MSGNO  = ZCX_CARGA=>ZCX_FORN_SEM_PARAMETRO-MSGNO
        MSGID  = ZCX_CARGA=>ZCX_FORN_SEM_PARAMETRO-MSGID
        MSGV1  = CONV #( ID_PRODUTOR )
        MSGV2  = CONV #( ID_NR_SAFRA )
        MSGV3  = CONV #( ID_BUKRS )
        MSGV4  = CONV #( ID_BRANCH ).

  ENDMETHOD.


  method zif_carga~get_rateia_descontos.

    data: pc_diferenca       type i,
          tot_nr_quantidade  type zde_zsdt0001nt_alv-nr_quantidade,
          lc_total_descontos type zde_nr_quan_class_com.

    data t_descontos type zde_zsdt0001rs_t.

    r_carga = me.

    "DATA(LC_DIDFENCA) = I_PESO_SUBTOTAL - I_PESO_LIQUIDO.

    move i_descontos[] to e_rateio[].

    loop at e_rateio assigning field-symbol(<fs_rateio>).
      clear:
      <fs_rateio>-nr_quantidade_com,
      <fs_rateio>-nr_quantidade_fis.
    endloop.

    sort e_rateio by tp_caracteristica.
    delete adjacent duplicates from e_rateio comparing tp_caracteristica.

    "Total de Desconto da Carga
    lc_total_descontos = 0.
*    *BUG 179188
    loop at i_descontos into data(wa_desconto). " where id_classificacao eq me->carga-id_classificacao or id_classificacao is initial.
      add wa_desconto-nr_quantidade_com to lc_total_descontos.
    endloop.

    if lc_total_descontos lt 0.
      raise exception type zcx_carga
        exporting
          textid = value #( msgid  = zcx_carga=>zcx_erro_descontos-msgid
                            msgno  = zcx_carga=>zcx_erro_descontos-msgno )
          msgty  = 'E'
          msgid  = zcx_carga=>zcx_erro_descontos-msgid
          msgno  = zcx_carga=>zcx_erro_descontos-msgno.
    endif.

*BUG 179188
    loop at i_descontos into wa_desconto.
      clear wa_desconto-id_classificacao.
      read table t_descontos assigning field-symbol(<fs_desconto>)
         with key tp_caracteristica = wa_desconto-tp_caracteristica.
      if sy-subrc ne 0.
        append wa_desconto to t_descontos.
      else.
        add wa_desconto-nr_quantidade_com to <fs_desconto>-nr_quantidade_com.
        add wa_desconto-nr_quantidade_fis to <fs_desconto>-nr_quantidade_fis.
      endif.
    endloop.

    data(pc_peso) = ( i_peso_liquido * 100 / me->carga-nm_peso_liquido ) * 100000.
    lc_total_descontos = 0.
    loop at e_rateio assigning <fs_rateio>.
      read table t_descontos into wa_desconto
      with key id_classificacao  = me->carga-id_classificacao
               tp_caracteristica = <fs_rateio>-tp_caracteristica.
      if sy-subrc is not initial.
        read table t_descontos into wa_desconto
        with key id_classificacao  = space
                 tp_caracteristica = <fs_rateio>-tp_caracteristica.
      endif.
      <fs_rateio>-nr_quantidade_com = ( wa_desconto-nr_quantidade_com * ( pc_peso / 100 ) ) / 100000.
      <fs_rateio>-nr_quantidade_fis = ( wa_desconto-nr_quantidade_fis * ( pc_peso / 100 ) ) / 100000.
      add <fs_rateio>-nr_quantidade_fis to lc_total_descontos.
    endloop.

    e_peso_subtotal = i_peso_liquido + lc_total_descontos.

  endmethod.


  METHOD ZIF_CARGA~GET_RECUPERAR_ENTRADA.

    R_INSTANCIA = ME.

    CHECK ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_FALSE.

    "Limpar Chaves ja estornadas
    LOOP AT ME->DOCUMENTO_FISCAL ASSIGNING FIELD-SYMBOL(<FS_NOTA>) WHERE OBJ_KEY_ENTRADA IS NOT INITIAL.
      ME->ZIF_CARGA~GET_DOCUMENTO_ENT_ESTORNADO(
         EXPORTING
           I_OBJ_KEY   = CONV #( <FS_NOTA>-OBJ_KEY_ENTRADA )
         IMPORTING
           R_ESTORNADO = DATA(R_ESTORNADO) ).

      IF R_ESTORNADO EQ ABAP_TRUE.
        CLEAR: <FS_NOTA>-OBJ_KEY_ENTRADA.
      ENDIF.
    ENDLOOP.

    LOOP AT ME->DOCUMENTO_FISCAL ASSIGNING <FS_NOTA> WHERE OBJ_KEY_ENTRADA IS INITIAL.

      "Resgatar Entrada não estornada
      SELECT * INTO TABLE @DATA(IT_ENTRADA_ESTOQUE)
        FROM ZMMT_EE_ZGR
       WHERE ID_CARGA   EQ @<FS_NOTA>-ID_CARGA
         AND ID_NOTA    EQ @<FS_NOTA>-ID_NOTA
         AND OBJ_KEY    NE @SPACE.

      LOOP AT IT_ENTRADA_ESTOQUE INTO DATA(WA_ENTRADA_ESTOQUE).

        SELECT SINGLE * INTO @DATA(WA_ZMMT_EE_ZGR_DOCS)
          FROM ZMMT_EE_ZGR_DOCS
         WHERE OBJ_KEY EQ @WA_ENTRADA_ESTOQUE-OBJ_KEY.

        IF SY-SUBRC IS INITIAL.

          "Verifica Aviso
          "IF WA_ZMMT_EE_ZGR_DOCS-AV_VBELN IS NOT INITIAL.
          "  SELECT SINGLE * INTO @DATA(WA_LIKP)
          "    FROM LIKP
          "   WHERE VBELN EQ @WA_ZMMT_EE_ZGR_DOCS-AV_VBELN.
          "
          "  IF SY-SUBRC IS INITIAL.
          "    <FS_NOTA>-OBJ_KEY_ENTRADA = WA_ZMMT_EE_ZGR_DOCS-OBJ_KEY.
          "    <FS_NOTA>-AV_VBELN        = WA_ZMMT_EE_ZGR_DOCS-AV_VBELN.
          "    <FS_NOTA>-PO_NUMBER       = WA_ENTRADA_ESTOQUE-PO_NUMBER.
          "    <FS_NOTA>-PO_ITEM         = WA_ENTRADA_ESTOQUE-PO_ITEM.
          "  ENDIF.
          "ENDIF.

          "Verifica MIGO
          IF WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR IS NOT INITIAL.
            SELECT SINGLE * INTO @DATA(WA_MKPF)
              FROM MSEG
             WHERE SMBLN EQ @WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR
               AND SJAHR EQ @WA_ZMMT_EE_ZGR_DOCS-MM_MJAHR.

            IF SY-SUBRC IS NOT INITIAL.
              <FS_NOTA>-OBJ_KEY_ENTRADA = WA_ZMMT_EE_ZGR_DOCS-OBJ_KEY.
              <FS_NOTA>-MM_MBLNR        = WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR.
              <FS_NOTA>-MM_MJAHR        = WA_ZMMT_EE_ZGR_DOCS-MM_MJAHR.
              <FS_NOTA>-PO_NUMBER       = WA_ENTRADA_ESTOQUE-PO_NUMBER.
              <FS_NOTA>-PO_ITEM         = WA_ENTRADA_ESTOQUE-PO_ITEM.
            ENDIF.
          ENDIF.

          "Verifica MIRO
          IF WA_ZMMT_EE_ZGR_DOCS-FT_BELNR IS NOT INITIAL.
            SELECT SINGLE * INTO @DATA(WA_RBKP)
              FROM RBKP
             WHERE BELNR EQ @WA_ZMMT_EE_ZGR_DOCS-FT_BELNR
               AND GJAHR EQ @WA_ZMMT_EE_ZGR_DOCS-FT_GJAHR.

            IF SY-SUBRC IS INITIAL AND WA_RBKP-STBLG IS INITIAL.
              <FS_NOTA>-OBJ_KEY_ENTRADA = WA_ZMMT_EE_ZGR_DOCS-OBJ_KEY.
              <FS_NOTA>-FT_BELNR        = WA_ZMMT_EE_ZGR_DOCS-FT_BELNR.
              <FS_NOTA>-FT_GJAHR        = WA_ZMMT_EE_ZGR_DOCS-FT_GJAHR.
              <FS_NOTA>-PO_NUMBER       = WA_ENTRADA_ESTOQUE-PO_NUMBER.
              <FS_NOTA>-PO_ITEM         = WA_ENTRADA_ESTOQUE-PO_ITEM.
            ENDIF.
          ENDIF.

          "Fiscal
          IF WA_ZMMT_EE_ZGR_DOCS-DOCNUM IS NOT INITIAL.
            SELECT SINGLE * INTO @DATA(WA_J_1BNFDOC)
              FROM J_1BNFDOC
             WHERE DOCNUM EQ @WA_ZMMT_EE_ZGR_DOCS-DOCNUM.

            IF SY-SUBRC IS INITIAL AND WA_J_1BNFDOC-CANCEL NE ABAP_TRUE.
              <FS_NOTA>-OBJ_KEY_ENTRADA = WA_ZMMT_EE_ZGR_DOCS-OBJ_KEY.
              <FS_NOTA>-DOCNUM          = WA_ZMMT_EE_ZGR_DOCS-DOCNUM.
            ENDIF.
          ENDIF.

          "Verifica MIGO Sobra
          IF WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR_SOBRA IS NOT INITIAL.
            SELECT SINGLE * INTO @DATA(WA_MKPF_SOBRA)
              FROM MSEG
             WHERE SMBLN EQ @WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR_SOBRA
               AND SJAHR EQ @WA_ZMMT_EE_ZGR_DOCS-MM_MJAHR_SOBRA.

            IF SY-SUBRC IS NOT INITIAL.
              <FS_NOTA>-OBJ_KEY_ENTRADA = WA_ZMMT_EE_ZGR_DOCS-OBJ_KEY.
              <FS_NOTA>-MM_MBLNR_SOBRA = WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR_SOBRA.
              <FS_NOTA>-MM_MJAHR_SOBRA = WA_ZMMT_EE_ZGR_DOCS-MM_MJAHR_SOBRA.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_REGISTRO.
    R_CARGA = ME.
    MOVE-CORRESPONDING ME->CARGA TO E_REGISTRO.
  ENDMETHOD.


  METHOD ZIF_CARGA~GET_RESULT_DESC_CLASSIFICACAO.

    TYPES BEGIN OF TY_MESSAGE_ERRO.
    TYPES: MESSAGE TYPE STRING.
    TYPES END OF TY_MESSAGE_ERRO.

    DATA: LO_JSON_SERIALIZER TYPE REF TO CL_TREX_JSON_SERIALIZER,
          R_CLASSIFICACAO	   TYPE ZDE_PES_RESULTADO_CLASS,
          RETORNO_ERRO       TYPE TY_MESSAGE_ERRO.

    R_CARGA = ME.

    CLEAR: E_RESULTADO.

    CHECK I_CLASSIFICACAO-PERCENTUAL IS NOT INITIAL.

    CREATE OBJECT LO_JSON_SERIALIZER EXPORTING DATA = I_CLASSIFICACAO.

    LO_JSON_SERIALIZER->SERIALIZE( ).
    DATA(JSON_INPUT) = LO_JSON_SERIALIZER->GET_DATA( ).
    REPLACE FIRST OCCURRENCE OF 'percentualimpureza' IN JSON_INPUT WITH '"percentualImpureza"'.

    DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.

    CREATE OBJECT OB_WEB_SERVICE.

    OB_WEB_SERVICE->ZIF_WEBSERVICE~AUTENTICA_OPUS = ABAP_TRUE.

    TRY .
        OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'DC' ).
      CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
    ENDTRY.

    OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'O' ).

    TRY .
        DATA(VAR_HTTP) = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
        DATA(VAR_URL)  = OB_WEB_SERVICE->GET_URI( ).
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
    ENDTRY.

    OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP ).

    OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
      EXPORTING
        I_HTTP                     = VAR_HTTP
        I_XML                      = JSON_INPUT
      IMPORTING
        E_CODE                     = DATA(E_CODE)
        E_REASON                   = DATA(E_REASON)
      RECEIVING
        E_RESULTADO                = DATA(JSON_RETORNO)
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5
    ).

*    IF sy-subrc IS NOT INITIAL.
**     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    CALL METHOD ob_web_service->zif_webservice~consultar
*      EXPORTING
*        i_http                     = var_http
*        i_xml                      = json_input
*      RECEIVING
*        e_resultado                = DATA(json_retorno)
*      EXCEPTIONS
*        http_communication_failure = 1
*        http_invalid_state         = 2
*        http_processing_failed     = 3
*        http_invalid_timeout       = 4
*        OTHERS                     = 5.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    IF E_CODE NE 200.

      E_RESULTADO-MESSAGE = E_CODE.

      CLEAR: RETORNO_ERRO.

      IF JSON_RETORNO IS NOT INITIAL.
        CALL METHOD /UI2/CL_JSON=>DESERIALIZE
          EXPORTING
            JSON = JSON_RETORNO
          CHANGING
            DATA = RETORNO_ERRO.
      ENDIF.

      IF RETORNO_ERRO-MESSAGE IS NOT INITIAL.
        E_RESULTADO-MESSAGE = RETORNO_ERRO-MESSAGE.
        ME->ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = E_RESULTADO-MESSAGE ).
      ELSE.
        CONCATENATE E_RESULTADO-MESSAGE '-' E_REASON '-' VAR_URL INTO E_RESULTADO-MESSAGE SEPARATED BY SPACE.
        ME->ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = E_RESULTADO-MESSAGE ).
      ENDIF.
    ENDIF.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = JSON_RETORNO
      CHANGING
        DATA = E_RESULTADO.

    IF E_RESULTADO-MESSAGE IS NOT INITIAL.
      ME->ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = E_RESULTADO-MESSAGE ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_ROMANEIO_ENTRADA.

    DATA: R_ID_CARGA TYPE RANGE OF ZDE_ID_CARGA,
          R_ID_NOTA  TYPE RANGE OF ZDE_ID_NOTA.

    R_CARGA = ME.

    CHECK ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_FALSE OR I_FORCE EQ ABAP_TRUE.

    R_ID_CARGA = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = I_ID_CARGA HIGH = I_ID_CARGA ) ).
    IF I_ID_NOTA IS NOT INITIAL.
      R_ID_NOTA  = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = I_ID_NOTA HIGH = I_ID_NOTA ) ).
    ENDIF.

    SELECT * INTO TABLE E_ROMANEIOS
      FROM ZSDT0001
     WHERE ID_CARGA     IN R_ID_CARGA
       AND ID_NOTA      IN R_ID_NOTA
       AND TP_MOVIMENTO EQ ZCL_ROMANEIO=>ST_TP_MOVIMENTO_ENTRADA
     ORDER BY NR_ROMANEIO.

    IF I_CANCELADOS EQ ABAP_TRUE.
      SELECT * APPENDING TABLE E_ROMANEIOS_CANCEL
        FROM ZSDT0001CA
       WHERE ID_CARGA     IN R_ID_CARGA
         AND ID_NOTA      IN R_ID_NOTA
         AND TP_MOVIMENTO EQ ZCL_ROMANEIO=>ST_TP_MOVIMENTO_ENTRADA
       ORDER BY NR_ROMANEIO.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_ROMANEIO_SAIDA.

    DATA: R_ID_CARGA TYPE RANGE OF ZDE_ID_CARGA,
          R_ID_NOTA  TYPE RANGE OF ZDE_ID_NOTA.

    "CHECK ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_FALSE.

    R_ID_CARGA = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = I_ID_CARGA HIGH = I_ID_CARGA ) ).

    R_CARGA = ME.

    SELECT * INTO TABLE E_ROMANEIOS
      FROM ZSDT0001
     WHERE ID_CARGA     IN R_ID_CARGA
       AND TP_MOVIMENTO EQ ZCL_ROMANEIO=>ST_TP_MOVIMENTO_SAIDA.

  ENDMETHOD.


  METHOD ZIF_CARGA~GET_TP_STATUS.
    R_CARGA = ME.
    E_TP_STATUS = ME->CARGA-TP_STATUS.
  ENDMETHOD.


  method zif_carga~get_validar_nota_fiscal.

    data: e_tipo      type char01,
          e_regio	    type regio,
          i_texto     type string,
          i_nf_number	type j_1bnfdoc-nfnum,
          lc_nr_nota  type c length 09.

    data: wa_zsdt0001pd type zsdt0001pd.

    r_instance = me.

    if me->carga-tp_status ne zif_carga=>st_status_cancelada.

      if i_nota_fiscal-id_entrada is initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_obg_inf_tp_entrada-msgid msgno = zcx_carga=>zcx_obg_inf_tp_entrada-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_obg_inf_tp_entrada-msgno
            msgid  = zcx_carga=>zcx_obg_inf_tp_entrada-msgid.
      endif.

      select single * into @data(wa_zsdt0001teln)
        from zsdt0001teln
       where id_entrada eq @i_nota_fiscal-id_entrada
         and nr_safra   eq @me->carga-nr_safra
         and id_bukrs   eq @me->carga-id_bukrs
         and id_branch  eq @me->carga-id_branch.

      if sy-subrc is not initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_tp_entrada_nao_permitido-msgid msgno = zcx_carga=>zcx_tp_entrada_nao_permitido-msgno
                              attr1 = conv #( i_nota_fiscal-id_entrada )
                              attr2 = conv #( me->carga-nr_safra )
                              attr3 = conv #( me->carga-id_branch ) )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_tp_entrada_nao_permitido-msgno
            msgid  = zcx_carga=>zcx_tp_entrada_nao_permitido-msgid
            msgv1  = conv #( i_nota_fiscal-id_entrada )
            msgv2  = conv #( me->carga-nr_safra )
            msgv3  = conv #( me->carga-id_branch ).
      endif.

      if i_nota_fiscal-nr_fornecedor_ie is initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_obg_inf_ie_prod-msgid msgno = zcx_carga=>zcx_obg_inf_ie_prod-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_obg_inf_ie_prod-msgno
            msgid  = zcx_carga=>zcx_obg_inf_ie_prod-msgid.
      endif.

      if i_nota_fiscal-id_fornecedor is initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_obg_inf_fornecedor-msgid msgno = zcx_carga=>zcx_obg_inf_fornecedor-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_obg_inf_fornecedor-msgno
            msgid  = zcx_carga=>zcx_obg_inf_fornecedor-msgid.
      else .
        try .

            data(ob_fornecedores) = zcl_fornecedores=>zif_parceiros~get_instance(
              )->set_parceiro( i_parceiro = i_nota_fiscal-id_fornecedor
              )->ck_ativo(
              )->ck_ativo_empresa( i_empresa = me->carga-id_bukrs
              )->ck_restricao_embargo(
              )->get_tipo_parceiro( importing e_tipo = e_tipo
              )->get_regio( importing e_regio	= e_regio ).
          catch zcx_parceiros into data(ex_parceiros).
            message id ex_parceiros->msgid type 'S'
             number ex_parceiros->msgno
               with ex_parceiros->msgv1 ex_parceiros->msgv2 ex_parceiros->msgv3 ex_parceiros->msgv4
               into i_texto.
            me->gera_erro_geral( i_texto = i_texto ).
        endtry.
      endif.

      case i_nota_fiscal-id_mod_fiscal.
        when zif_carga=>st_model_fiscal_papel.
          data(lc_ck_nfe) = abap_false.
        when zif_carga=>st_model_fiscal_eletronico.
          lc_ck_nfe = abap_true.
      endcase.

      select single * into @data(wa_entrada)
        from zsdt0001te
       where id_entrada eq @i_nota_fiscal-id_entrada
         and id_empresa eq @me->carga-id_bukrs
         and ck_nfe     eq @lc_ck_nfe.

      if sy-subrc is not initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_te_md_fiscal_sem_param-msgid msgno = zcx_carga=>zcx_te_md_fiscal_sem_param-msgno attr1 = conv #( i_nota_fiscal-id_mod_fiscal ) )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_te_md_fiscal_sem_param-msgno
            msgid  = zcx_carga=>zcx_te_md_fiscal_sem_param-msgid
            msgv1  = conv #( i_nota_fiscal-id_mod_fiscal ).
      endif.

      select single * into @data(wa_j_1baa)
        from j_1baa
       where nftype eq @wa_entrada-ct_nota.

      case i_nota_fiscal-id_mod_fiscal.
        when zif_carga=>st_model_fiscal_eletronico.

          if i_nota_fiscal-nr_chave_nfe is initial and wa_j_1baa-form is initial.
            raise exception type zcx_carga
              exporting
                textid = value #( msgid = zcx_carga=>zcx_obg_inf_chave_nfe-msgid
                                  msgno = zcx_carga=>zcx_obg_inf_chave_nfe-msgno )
                msgty  = 'E'
                msgno  = zcx_carga=>zcx_obg_inf_chave_nfe-msgno
                msgid  = zcx_carga=>zcx_obg_inf_chave_nfe-msgid.
          endif.

          if wa_j_1baa-form is initial.
            select single * into @data(wa_nfe)
              from zib_nfe_dist_ter
             where chave_nfe eq @i_nota_fiscal-nr_chave_nfe.

            if sy-subrc is not initial.
              raise exception type zcx_carga
                exporting
                  textid = value #( msgid = zcx_carga=>zcx_nfe_nao_distribuida-msgid
                                    msgno = zcx_carga=>zcx_nfe_nao_distribuida-msgno
                                    attr1 = conv #( i_nota_fiscal-nr_chave_nfe ) )
                  msgty  = 'E'
                  msgno  = zcx_carga=>zcx_nfe_nao_distribuida-msgno
                  msgid  = zcx_carga=>zcx_nfe_nao_distribuida-msgid
                  msgv1  = conv #( i_nota_fiscal-nr_chave_nfe ).
            endif.

            if wa_nfe-bukrs  ne me->carga-id_bukrs or wa_nfe-branch ne me->carga-id_branch.
              raise exception type zcx_carga
                exporting
                  textid = value #( msgid = zcx_carga=>zcx_erro_tomador_nfe-msgid
                                    msgno = zcx_carga=>zcx_erro_tomador_nfe-msgno
                                    attr1 = conv #( me->carga-id_branch ) )
                  msgty  = 'E'
                  msgno  = zcx_carga=>zcx_erro_tomador_nfe-msgno
                  msgid  = zcx_carga=>zcx_erro_tomador_nfe-msgid
                  msgv1  = conv #( me->carga-id_branch ).
            endif.

            i_nota_fiscal-nr_nota            = wa_nfe-numero.
            i_nota_fiscal-nm_serie           = wa_nfe-serie.
            i_nota_fiscal-dt_emissao         = wa_nfe-dt_emissao.
            i_nota_fiscal-dt_vencimento_form = wa_nfe-dt_emissao.
            i_nota_fiscal-nr_valor           = wa_nfe-vl_total.

            select * into table @data(it_itm)
              from zib_nfe_dist_itm
             where chave_nfe eq @i_nota_fiscal-nr_chave_nfe.

            if sy-subrc is not initial.
              raise exception type zcx_carga
                exporting
                  textid = value #( msgid = zcx_carga=>zcx_nfe_item_nao_distribuido-msgid
                                    msgno = zcx_carga=>zcx_nfe_item_nao_distribuido-msgno
                                    attr1 = conv #( i_nota_fiscal-nr_chave_nfe ) )
                  msgty  = 'E'
                  msgno  = zcx_carga=>zcx_nfe_item_nao_distribuido-msgno
                  msgid  = zcx_carga=>zcx_nfe_item_nao_distribuido-msgid
                  msgv1  = conv #( i_nota_fiscal-nr_chave_nfe ).
            endif.

            i_nota_fiscal-nr_quantidade = 0.

            loop at it_itm into data(wa_itm).

              i_nota_fiscal-cfop = wa_itm-prod_cfop.

              translate wa_itm-prod_und_comerci to upper case.

              case zcl_str=>upper( conv #( wa_itm-prod_und_comerci ) )->get( ).
                when 'KG'.
                  i_nota_fiscal-nr_quantidade = i_nota_fiscal-nr_quantidade + wa_itm-prod_qtd_comerci.
                when 'TON' or 'TO'.
                  i_nota_fiscal-nr_quantidade = i_nota_fiscal-nr_quantidade + wa_itm-prod_qtd_comerci * 1000.
                when others.
                  raise exception type zcx_carga
                    exporting
                      textid = value #( msgid = zcx_carga=>zcx_nfe_item_unidade-msgid
                                        msgno = zcx_carga=>zcx_nfe_item_unidade-msgno
                                        attr1 = conv #( wa_itm-prod_und_comerci ) )
                      msgty  = 'E'
                      msgno  = zcx_carga=>zcx_nfe_item_unidade-msgno
                      msgid  = zcx_carga=>zcx_nfe_item_unidade-msgid
                      msgv1  = conv #( wa_itm-prod_und_comerci ).
              endcase.

            endloop.
          endif.

        when others.
          if wa_j_1baa-form is not initial and wa_j_1baa-nftype ne 'ZW'.  "BUG IMPEDITIVO 82103
            raise exception type zcx_carga
              exporting
                textid = value #( msgid = zcx_carga=>zcx_nao_permitido_md_fiscal-msgid
                                  msgno = zcx_carga=>zcx_nao_permitido_md_fiscal-msgno
                                  attr1 = conv #( i_nota_fiscal-id_mod_fiscal ) )
                msgty  = 'E'
                msgno  = zcx_carga=>zcx_nao_permitido_md_fiscal-msgno
                msgid  = zcx_carga=>zcx_nao_permitido_md_fiscal-msgid
                msgv1  = conv #( i_nota_fiscal-id_mod_fiscal ).
          endif.

          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
            exporting
              input  = i_nota_fiscal-nr_nota
            importing
              output = lc_nr_nota.

          if zcl_string=>length( zcl_string=>trim( conv #( lc_nr_nota ) ) ) gt 6.
            raise exception type zcx_carga
              exporting
                textid = value #( msgid = zcx_carga=>zcx_qtd_nro_nota_fiscal-msgid
                                  msgno = zcx_carga=>zcx_qtd_nro_nota_fiscal-msgno
                                  attr1 = conv #( i_nota_fiscal-id_mod_fiscal ) )
                msgty  = 'E'
                msgno  = zcx_carga=>zcx_qtd_nro_nota_fiscal-msgno
                msgid  = zcx_carga=>zcx_qtd_nro_nota_fiscal-msgid
                msgv1  = conv #( i_nota_fiscal-id_mod_fiscal ).
          endif.

      endcase.

      if wa_entrada-ct_nota is initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_te_sem_ct_fiscal-msgid msgno = zcx_carga=>zcx_te_sem_ct_fiscal-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_te_sem_ct_fiscal-msgno
            msgid  = zcx_carga=>zcx_te_sem_ct_fiscal-msgid.
      endif.

      if wa_entrada-tp_mov_mercadoria is initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_te_sem_tp_mov_merc-msgid msgno = zcx_carga=>zcx_te_sem_tp_mov_merc-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_te_sem_tp_mov_merc-msgno
            msgid  = zcx_carga=>zcx_te_sem_tp_mov_merc-msgid.
      endif.

      if wa_entrada-id_iva is initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_te_sem_iva-msgid msgno = zcx_carga=>zcx_te_sem_iva-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_te_sem_iva-msgno
            msgid  = zcx_carga=>zcx_te_sem_iva-msgid.
      endif.

      if wa_entrada-fr_pagamento is initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_te_sem_form_pagamento-msgid msgno = zcx_carga=>zcx_te_sem_form_pagamento-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_te_sem_form_pagamento-msgno
            msgid  = zcx_carga=>zcx_te_sem_form_pagamento-msgid.
      endif.

      if wa_entrada-bl_pagamento is initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_te_sem_ch_bloqueio-msgid msgno = zcx_carga=>zcx_te_sem_ch_bloqueio-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_te_sem_ch_bloqueio-msgno
            msgid  = zcx_carga=>zcx_te_sem_ch_bloqueio-msgid.
      endif.

      if wa_entrada-bn_empresa is initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_te_sem_bnc_empresa-msgid msgno = zcx_carga=>zcx_te_sem_bnc_empresa-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_te_sem_bnc_empresa-msgno
            msgid  = zcx_carga=>zcx_te_sem_bnc_empresa-msgid.
      endif.

      case wa_entrada-tp_pessoa.
        when zif_parceiros=>st_pessoa_fisica.
          if e_tipo ne wa_entrada-tp_pessoa.
            raise exception type zcx_carga
              exporting
                textid = value #( msgid = zcx_carga=>zcx_te_somente_fisica-msgid
                                  msgno = zcx_carga=>zcx_te_somente_fisica-msgno
                                  attr1 = conv #( wa_entrada-id_entrada ) )
                msgty  = 'E'
                msgno  = zcx_carga=>zcx_te_somente_fisica-msgno
                msgid  = zcx_carga=>zcx_te_somente_fisica-msgid
                msgv1  = conv #( wa_entrada-id_entrada ).
          endif.
        when zif_parceiros=>st_pessoa_juridica.
          if e_tipo ne wa_entrada-tp_pessoa.
            raise exception type zcx_carga
              exporting
                textid = value #( msgid = zcx_carga=>zcx_te_somente_juridica-msgid
                                  msgno = zcx_carga=>zcx_te_somente_juridica-msgno
                                  attr1 = conv #( wa_entrada-id_entrada ) )
                msgty  = 'E'
                msgno  = zcx_carga=>zcx_te_somente_juridica-msgno
                msgid  = zcx_carga=>zcx_te_somente_juridica-msgid
                msgv1  = conv #( wa_entrada-id_entrada ).
          endif.
      endcase.

      if i_nota_fiscal-nr_nota is initial and wa_j_1baa-form is initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_obg_inf_nf_numero-msgid
                              msgno = zcx_carga=>zcx_obg_inf_nf_numero-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_obg_inf_nf_numero-msgno
            msgid  = zcx_carga=>zcx_obg_inf_nf_numero-msgid.
      endif.

      if i_nota_fiscal-nm_serie is initial and wa_j_1baa-form is initial.

        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_obg_inf_nf_serie-msgid
                              msgno = zcx_carga=>zcx_obg_inf_nf_serie-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_obg_inf_nf_serie-msgno
            msgid  = zcx_carga=>zcx_obg_inf_nf_serie-msgid.

      elseif wa_j_1baa-form is initial or wa_j_1baa-nftype eq 'ZW'.  "BUG IMPEDITIVO 82103

        "Verificar se Série Existe no Sigam
        try .
            zcl_api_opus=>get_series_sigam( exporting i_descr_resumida = conv #( i_nota_fiscal-nm_serie ) importing e_series = data(e_series) ).

            case i_nota_fiscal-id_mod_fiscal.
              when zif_carga=>st_model_fiscal_papel.
                read table e_series with key eletronica = abap_false transporting no fields.
              when zif_carga=>st_model_fiscal_eletronico.
                read table e_series with key eletronica = abap_true transporting no fields.
              when others.
                sy-subrc = 1.
            endcase.

            if sy-subrc is not initial.
              raise exception type zcx_carga
                exporting
                  textid = value #( msgid = zcx_carga=>zcx_serie_sigam-msgid
                                    msgno = zcx_carga=>zcx_serie_sigam-msgno
                                    attr1 = i_nota_fiscal-nm_serie )
                  msgty  = 'E'
                  msgno  = zcx_carga=>zcx_serie_sigam-msgno
                  msgid  = zcx_carga=>zcx_serie_sigam-msgid
                  msgv1  = conv #( i_nota_fiscal-nm_serie ).
            endif.

          catch zcx_error into data(ex_error).    "

            raise exception type zcx_carga
              exporting
                textid = value #( msgid = ex_error->msgid
                                  msgno = ex_error->msgno
                                  attr1 = ex_error->msgv1
                                  attr2 = ex_error->msgv2
                                  attr3 = ex_error->msgv3
                                  attr4 = ex_error->msgv4 )
                msgty  = 'E'
                msgno  = ex_error->msgno
                msgid  = ex_error->msgid
                msgv1  = ex_error->msgv1
                msgv2  = ex_error->msgv2
                msgv3  = ex_error->msgv3
                msgv4  = ex_error->msgv4.
        endtry.
      endif.

      if i_nota_fiscal-dt_emissao is initial and wa_j_1baa-form is initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_obg_inf_nf_dt_emissao-msgid
                              msgno = zcx_carga=>zcx_obg_inf_nf_dt_emissao-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_obg_inf_nf_dt_emissao-msgno
            msgid  = zcx_carga=>zcx_obg_inf_nf_dt_emissao-msgid.
      endif.

      if i_nota_fiscal-dt_emissao gt sy-datlo and i_nota_fiscal-dt_emissao gt sy-datum.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_data_emissao_nf-msgid
                              msgno = zcx_carga=>zcx_data_emissao_nf-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_data_emissao_nf-msgno
            msgid  = zcx_carga=>zcx_data_emissao_nf-msgid.
      endif.

      if i_nota_fiscal-nr_quantidade is initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_obg_inf_nf_quantidade-msgid msgno = zcx_carga=>zcx_obg_inf_nf_quantidade-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_obg_inf_nf_quantidade-msgno
            msgid  = zcx_carga=>zcx_obg_inf_nf_quantidade-msgid.
      endif.

      if i_nota_fiscal-nr_valor is initial.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_obg_inf_nf_valor_total-msgid msgno = zcx_carga=>zcx_obg_inf_nf_valor_total-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_obg_inf_nf_valor_total-msgno
            msgid  = zcx_carga=>zcx_obg_inf_nf_valor_total-msgid.
      endif.

      if i_nota_fiscal-id_mod_fiscal eq zif_carga=>st_model_fiscal_papel.
        if i_nota_fiscal-dt_vencimento_form is initial.
          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_obg_inf_dt_venc_form-msgid
                                msgno = zcx_carga=>zcx_obg_inf_dt_venc_form-msgno )
              msgty  = 'E'
              msgno  = zcx_carga=>zcx_obg_inf_dt_venc_form-msgno
              msgid  = zcx_carga=>zcx_obg_inf_dt_venc_form-msgid.
        endif.

        if i_nota_fiscal-dt_vencimento_form lt i_nota_fiscal-dt_emissao.
          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_data_formulario_venc-msgid
                                msgno = zcx_carga=>zcx_data_formulario_venc-msgno )
              msgty  = 'E'
              msgno  = zcx_carga=>zcx_data_formulario_venc-msgno
              msgid  = zcx_carga=>zcx_data_formulario_venc-msgid.
        endif.
      endif.

      if i_nota_fiscal-cfop is initial and wa_j_1baa-form is initial.

        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_obg_inf_cfop-msgid msgno = zcx_carga=>zcx_obg_inf_cfop-msgno )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_obg_inf_cfop-msgno
            msgid  = zcx_carga=>zcx_obg_inf_cfop-msgid.

      elseif wa_j_1baa-form is initial.

        select * into table @data(it_zsdt0001tecfop)
         from zsdt0001tecfop
        where id_entrada eq @i_nota_fiscal-id_entrada
          and cfop       eq @i_nota_fiscal-cfop.

        if sy-subrc is not initial.
          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_cfop_nao_permitido_te-msgid
                                msgno = zcx_carga=>zcx_cfop_nao_permitido_te-msgno
                                attr1 = conv #( i_nota_fiscal-cfop )
                                attr2 = conv #( i_nota_fiscal-id_entrada ) )
              msgty  = 'E'
              msgno  = zcx_carga=>zcx_cfop_nao_permitido_te-msgno
              msgid  = zcx_carga=>zcx_cfop_nao_permitido_te-msgid
              msgv1  = conv #( i_nota_fiscal-cfop )
              msgv2  = conv #( i_nota_fiscal-id_entrada ).
        endif.

        select single * into @data(wa_j_1bbranch)
          from j_1bbranch
         where bukrs  eq @me->carga-id_bukrs
           and branch eq @me->carga-id_branch.

        select single * into @data(wa_adrc)
          from adrc
         where addrnumber eq @wa_j_1bbranch-adrnr.

        read table it_zsdt0001tecfop with key regio = wa_adrc-region tp_pessoa = e_tipo transporting no fields.

        if sy-subrc is not initial.
          "CFOP &MSGV1& não permitido para Fornecedor Pessoa &MSGV2& Estado &MSGV3&!
          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_cfop_nao_permitido_forn-msgid
                                msgno = zcx_carga=>zcx_cfop_nao_permitido_forn-msgno
                                attr1 = conv #( i_nota_fiscal-cfop  )
                                attr2 = conv #( e_tipo  )
                                attr3 = conv #( e_regio ) )
              msgty  = 'E'
              msgno  = zcx_carga=>zcx_cfop_nao_permitido_forn-msgno
              msgid  = zcx_carga=>zcx_cfop_nao_permitido_forn-msgid
              msgv1  = conv #( i_nota_fiscal-cfop )
              msgv2  = conv #( e_tipo )
              msgv3  = conv #( e_regio ).
        endif.

      endif.

      "Se CFOP não configura de Retorno de Armazenagem mais
      "TE de Retorno de Armazenagem
      data(lc_ck_cfop_retorno_amazenagem) = zcl_cfop=>get_ck_cfop_retorno_amazem( i_cfop = i_nota_fiscal-cfop ).

      if ( lc_ck_cfop_retorno_amazenagem eq abap_false and wa_entrada-id_tp_operacao eq zif_tp_operacao_carga_entrada=>st_retorno_armazenagem ) or
         ( lc_ck_cfop_retorno_amazenagem eq abap_true  and wa_entrada-id_tp_operacao ne zif_tp_operacao_carga_entrada=>st_retorno_armazenagem ) .
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_tipo_entrada_cfop-msgid
                              msgno = zcx_carga=>zcx_tipo_entrada_cfop-msgno
                              attr1 = wa_entrada-id_entrada
                              attr2 = i_nota_fiscal-cfop )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_tipo_entrada_cfop-msgno
            msgid  = zcx_carga=>zcx_tipo_entrada_cfop-msgid
            msgv1  = conv #( wa_entrada-id_entrada )
            msgv2  = conv #( i_nota_fiscal-cfop ).
      endif.


      if ( wa_entrada-id_tp_operacao = zif_tp_operacao_carga_entrada=>st_retorno_armazenagem ) and
         ( i_nota_fiscal-id_nota is not initial ).

        if i_nota_fiscal-po_number is initial.
          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_ped_compra_retorno_obg-msgid msgno = zcx_carga=>zcx_ped_compra_retorno_obg-msgno )
              msgty  = 'E'
              msgno  = zcx_carga=>zcx_ped_compra_retorno_obg-msgno
              msgid  = zcx_carga=>zcx_ped_compra_retorno_obg-msgid.
        else.
          "Buscar Número de Pedido de Compra
          zcl_pedido_compra=>get_pedido_compra_chave_e(
            exporting
              i_lifnr               = i_nota_fiscal-id_fornecedor
              i_bukrs               = me->carga-id_bukrs
              i_werks               = me->carga-id_branch
              i_matnr               = me->carga-id_produto
              i_lgort               = 'ARMZ'
              i_charg               = conv #( me->carga-nr_safra )
              i_ebeln               = i_nota_fiscal-po_number
              i_ebelp               = i_nota_fiscal-po_item
              i_bstyp               = 'F'    " Categoria do documento de compras
              i_bsart               = 'ZARM'
              i_abrir_tela          = abap_false
            receiving
              r_ekpo                = data(r_ekpo)    " Item do documento de compras
            exceptions
              nao_encontrado_pedido = 1
              others                = 2 ).

          if sy-subrc is not initial.
            raise exception type zcx_carga
              exporting
                textid = value #( msgid = zcx_carga=>zcx_ped_compra_retorno_err-msgid
                                  msgno = zcx_carga=>zcx_ped_compra_retorno_err-msgno
                                  attr1 = i_nota_fiscal-po_number )
                msgty  = 'E'
                msgno  = zcx_carga=>zcx_ped_compra_retorno_err-msgno
                msgid  = zcx_carga=>zcx_ped_compra_retorno_err-msgid
                msgv1  = conv #( i_nota_fiscal-po_number ).
          endif.
        endif.
      endif.

      if i_nota_fiscal-id_entregue_por is not initial.
        try .
            ob_fornecedores = zcl_fornecedores=>zif_parceiros~get_instance(
              )->set_parceiro( i_parceiro = i_nota_fiscal-id_entregue_por
              )->ck_ativo(
              )->ck_ativo_empresa( i_empresa = me->carga-id_bukrs
              )->ck_restricao_embargo(
              ).

          catch zcx_parceiros into ex_parceiros.

            message id ex_parceiros->msgid type 'S'
             number ex_parceiros->msgno
               with ex_parceiros->msgv1 ex_parceiros->msgv2 ex_parceiros->msgv3 ex_parceiros->msgv4
               into i_texto.

            me->gera_erro_geral( i_texto = i_texto ).
        endtry.

      endif.

      select single * into @data(wa_produtor_valida)
        from zsdt0001pd
       where id_produtor eq @i_nota_fiscal-id_fornecedor
         and nr_safra    eq @me->carga-nr_safra
         and id_bukrs    eq @me->carga-id_bukrs
         and id_branch   eq @me->carga-id_branch.

      if sy-subrc is not initial.

        sy-msgv1 = conv #( i_nota_fiscal-id_fornecedor ).
        sy-msgv2 = conv #( me->carga-nr_safra ).
        sy-msgv3 = conv #( me->carga-id_bukrs ).
        sy-msgv4 = conv #( me->carga-id_branch ).

        if me->zif_carga~at_manutencao eq abap_false.

          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_forn_sem_parametro-msgid
                                msgno = zcx_carga=>zcx_forn_sem_parametro-msgno
                                attr1 = conv #( i_nota_fiscal-id_fornecedor )
                                attr2 = conv #( me->carga-nr_safra )
                                attr3 = conv #( me->carga-id_bukrs )
                                attr4 = conv #( me->carga-id_branch ) )
              msgty  = 'E'
              msgno  = zcx_carga=>zcx_forn_sem_parametro-msgno
              msgid  = zcx_carga=>zcx_forn_sem_parametro-msgid
              msgv1  = conv #( i_nota_fiscal-id_fornecedor )
              msgv2  = conv #( me->carga-nr_safra )
              msgv3  = conv #( me->carga-id_bukrs )
              msgv4  = conv #( me->carga-id_branch ).

        else.

          wa_zsdt0001pd-id_branch    = me->carga-id_branch.
          wa_zsdt0001pd-id_bukrs     = me->carga-id_bukrs.
          wa_zsdt0001pd-nr_safra     = me->carga-nr_safra.
          wa_zsdt0001pd-id_produtor  = i_nota_fiscal-id_fornecedor.

          try .
              zcl_fornecedores=>zif_parceiros~get_instance(
                )->set_parceiro( i_parceiro = i_nota_fiscal-id_fornecedor
                )->ck_ativo(
                )->ck_ativo_empresa( i_empresa = me->carga-id_bukrs
                ).
              insert into zsdt0001pd values wa_zsdt0001pd.
            catch zcx_parceiros into ex_parceiros.
              message id ex_parceiros->msgid type 'S'
               number ex_parceiros->msgno
                 with ex_parceiros->msgv1 ex_parceiros->msgv2 ex_parceiros->msgv3 ex_parceiros->msgv4
                 into i_texto.
              me->gera_erro_geral( i_texto = i_texto ).
          endtry.

        endif.

      endif.

      if wa_j_1baa-form is initial.
        "Verificar Tipo da Pessoa e Modal do Documento
        case e_tipo.
          when zif_parceiros=>st_pessoa_fisica.
            if ( i_nota_fiscal-id_mod_fiscal eq zif_carga=>st_model_fiscal_eletronico )
                and not ( i_nota_fiscal-nm_serie ge '890' and i_nota_fiscal-nm_serie le '999' ) .
              if i_nota_fiscal-nm_serie ne '025'. "BUG SOLTO 173001  serie 025
                raise exception type zcx_carga
                  exporting
                    textid = value #( msgid = zcx_carga=>zcx_pessoa_fis_nfe-msgid
                                      msgno = zcx_carga=>zcx_pessoa_fis_nfe-msgno )
                    msgty  = 'E'
                    msgno  = zcx_carga=>zcx_pessoa_fis_nfe-msgno
                    msgid  = zcx_carga=>zcx_pessoa_fis_nfe-msgid.
              endif.
            endif.
          when zif_parceiros=>st_pessoa_juridica.
            if i_nota_fiscal-id_mod_fiscal eq zif_carga=>st_model_fiscal_papel.
              raise exception type zcx_carga
                exporting
                  textid = value #( msgid = zcx_carga=>zcx_pessoa_jus_papel-msgid
                                    msgno = zcx_carga=>zcx_pessoa_jus_papel-msgno )
                  msgty  = 'E'
                  msgno  = zcx_carga=>zcx_pessoa_jus_papel-msgno
                  msgid  = zcx_carga=>zcx_pessoa_jus_papel-msgid.
            endif.
        endcase.

      else.
        if i_nota_fiscal-id_mod_fiscal ne zif_carga=>st_model_fiscal_eletronico  and wa_j_1baa-nftype ne 'ZW'.  "BUG IMPEDITIVO 82103
          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_nf_propria_nfe-msgid
                                msgno = zcx_carga=>zcx_nf_propria_nfe-msgno )
              msgty  = 'E'
              msgno  = zcx_carga=>zcx_nf_propria_nfe-msgno
              msgid  = zcx_carga=>zcx_nf_propria_nfe-msgid.
        endif.
      endif.

      if wa_j_1baa-form is initial and i_nota_fiscal-id_mod_fiscal eq zif_carga=>st_model_fiscal_eletronico.

        select single * into @data(wa_item)
          from zib_nfe_dist_itm
         where chave_nfe eq @i_nota_fiscal-nr_chave_nfe.

        if sy-subrc is not initial.
          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_xml_nfe_nao_recebido-msgid
                                msgno = zcx_carga=>zcx_xml_nfe_nao_recebido-msgno )
              msgty  = 'E'
              msgno  = zcx_carga=>zcx_xml_nfe_nao_recebido-msgno
              msgid  = zcx_carga=>zcx_xml_nfe_nao_recebido-msgid.
        elseif wa_item-prod_cfop ne i_nota_fiscal-cfop.
          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_xml_nfe_cfop_invalido-msgid
                                msgno = zcx_carga=>zcx_xml_nfe_cfop_invalido-msgno
                                attr1 = conv #( i_nota_fiscal-cfop  ) )
              msgty  = 'E'
              msgno  = zcx_carga=>zcx_xml_nfe_cfop_invalido-msgno
              msgid  = zcx_carga=>zcx_xml_nfe_cfop_invalido-msgid
              msgv1  = conv #( i_nota_fiscal-cfop ).
        endif.

        "Verificar Grupo de NCM da Mercadoria
        select single * into @data(wa_marc)
          from marc
         where matnr eq @me->carga-id_produto
           and werks eq @me->carga-id_branch.

        replace all occurrences of regex '[^0-9]' in wa_marc-steuc with ''.
        replace all occurrences of regex '[^0-9]' in wa_item-prod_ncm with ''.

*        if wa_marc-steuc(4) ne wa_item-prod_ncm(4).
        if wa_marc-steuc(8) ne wa_item-prod_ncm(8). "BUG SOLTO 182008

          concatenate wa_item-prod_ncm(4) '.' wa_item-prod_ncm+4(2) '.' wa_item-prod_ncm+6(2) into data(lc_ncm_cliente).

          select single * into @data(wa_t604n_maggi)
            from t604n
           where spras eq @sy-langu
             and land1 eq 'BR'
             and steuc eq @wa_marc-steuc.

          select single * into @data(wa_t604n_cliente)
            from t604n
           where spras eq @sy-langu
             and land1 eq 'BR'
             and steuc eq @lc_ncm_cliente.

          "ZCX_ERRO_NCM	Grupo de Material &MSGV1& da Nota Fiscal diferente de &MSGV2&!
          "ZCX_ERRO_NCM_DESC  Grupo de Material &MSGV1&(&MSGV2&) da Nota Fiscal diferente de &MSGV3&(&MSGV4&)!

          if wa_t604n_maggi is initial or wa_t604n_cliente is initial.
            raise exception type zcx_carga
              exporting
                textid = value #( msgid  = zcx_carga=>zcx_erro_ncm-msgid
                                  msgno  = zcx_carga=>zcx_erro_ncm-msgno
                                  attr1  = conv #( wa_item-prod_ncm(4) )
                                  attr2  = conv #( wa_marc-steuc(4) ) )
                msgid  = zcx_carga=>zcx_erro_ncm-msgid
                msgno  = zcx_carga=>zcx_erro_ncm-msgno
                msgty  = 'E'
                msgv1  = conv #( wa_item-prod_ncm(4) )
                msgv2  = conv #( wa_marc-steuc(4) ).
          else.
            raise exception type zcx_carga
              exporting
                textid = value #( msgid  = zcx_carga=>zcx_erro_ncm_desc-msgid
                                  msgno  = zcx_carga=>zcx_erro_ncm_desc-msgno
                                  attr1  = conv #( wa_item-prod_ncm(4) )
                                  attr2  = conv #( wa_t604n_cliente-text1 )
                                  attr3  = conv #( wa_marc-steuc(4) )
                                  attr4  = conv #( wa_t604n_maggi-text1 ) )
                msgid  = zcx_carga=>zcx_erro_ncm_desc-msgid
                msgno  = zcx_carga=>zcx_erro_ncm_desc-msgno
                msgty  = 'E'
                msgv1  = conv #( wa_item-prod_ncm(4) )
                msgv2  = conv #( wa_t604n_cliente-text1 )
                msgv3  = conv #( wa_marc-steuc(4) )
                msgv4  = conv #( wa_t604n_maggi-text1 ).
          endif.
        endif.

        "Valida se Chave pode ser usada
        if ( zif_carga~ck_executar_reversao_entrada ne abap_true ). "BUG 34154

          zcl_nfe_xml=>zif_nfe_xml~get_instance(
             )->set_registro( i_chave = i_nota_fiscal-nr_chave_nfe
             )->get_validar(
            exporting
              i_material       = me->carga-id_produto " Nº do material
            importing
              e_validacao      = data(e_validacao) " Estrutura de Retorno de Validação de XML de NF-e
             ).

          if e_validacao-ck_erro eq abap_true.
            me->gera_erro_geral( exporting i_texto = e_validacao-ds_messagem ) .
          elseif e_validacao-nm_qtd_itens gt 1.

            raise exception type zcx_carga
              exporting
                textid = value #( msgid  = zcx_carga=>zcx_nfe_many_rows-msgid
                                  msgno  = zcx_carga=>zcx_nfe_many_rows-msgno )
                msgid  = zcx_carga=>zcx_nfe_many_rows-msgid
                msgno  = zcx_carga=>zcx_nfe_many_rows-msgno
                msgty  = 'E'.

          endif.

        endif.

      endif.

      case i_nota_fiscal-id_mod_fiscal.
        when zif_carga=>st_model_fiscal_papel.

          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
            exporting
              input  = i_nota_fiscal-nr_nota
            importing
              output = i_nf_number.

          data(i_xblnr) = zcl_miro=>get_chave_referencia( i_nf_number  = i_nf_number i_series = i_nota_fiscal-nm_serie ).

        when zif_carga=>st_model_fiscal_eletronico.

          i_xblnr = zcl_miro=>get_chave_referencia( i_nf_number9  = conv #( i_nota_fiscal-nr_nota ) i_series = i_nota_fiscal-nm_serie ).
      endcase.

      "Verificar se Documento Fiscal já foi lançado

      if ( zif_carga~ck_executar_manutencao_entrada ne abap_true ) and
         ( zif_carga~ck_executar_reversao_entrada   ne abap_true ) and "BUG 34154
         ( me->carga-tp_status eq zif_carga=>st_status_aberto    ).

        try .
            zcl_miro=>verificar_forn_doc_fiscal(
              exporting
                i_lifnr            = i_nota_fiscal-id_fornecedor
                i_nftype           = wa_entrada-ct_nota
                i_xblnr            = i_xblnr
                i_data             = i_nota_fiscal-dt_emissao
                i_werks            = me->carga-id_branch
            ).
          catch zcx_miro_exception into data(ex_miro).    "

            if not
               ( ex_miro->msgid eq 'ZCTE_DISTRI' and ex_miro->msgno eq '186' and ex_miro->msgv3 eq i_nota_fiscal-docnum or
                 ex_miro->msgid eq 'ZCTE_DISTRI' and ex_miro->msgno eq '187' and ex_miro->msgv3 eq i_nota_fiscal-ft_belnr ).

              raise exception type zcx_carga
                exporting
                  textid = value #( msgid  = ex_miro->msgid
                                    msgno  = ex_miro->msgno
                                    attr1  = ex_miro->msgv1
                                    attr2  = ex_miro->msgv2
                                    attr3  = ex_miro->msgv3
                                    attr4  = ex_miro->msgv4 )
                  msgid  = ex_miro->msgid
                  msgno  = ex_miro->msgno
                  msgty  = 'E'
                  msgv1  = ex_miro->msgv1
                  msgv2  = ex_miro->msgv2
                  msgv3  = ex_miro->msgv3
                  msgv4  = ex_miro->msgv4.

            endif.
        endtry.
      endif.

      data(ck_alterou_nota) = abap_false.
      if me->zif_carga~at_manutencao eq abap_true.

        select single * into @data(wa_zsdt0001nt)
          from zsdt0001nt
         where id_carga eq @i_nota_fiscal-id_carga
           and id_nota  eq @i_nota_fiscal-id_nota.

        if ( wa_zsdt0001nt-nr_chave_nfe ne i_nota_fiscal-nr_chave_nfe or
             wa_zsdt0001nt-nm_serie ne i_nota_fiscal-nm_serie or
             wa_zsdt0001nt-nr_nota ne i_nota_fiscal-nr_nota or
             wa_zsdt0001nt-id_fornecedor ne i_nota_fiscal-id_fornecedor or
             wa_zsdt0001nt-id_mod_fiscal ne i_nota_fiscal-id_mod_fiscal ) and ( sy-subrc is initial ).
          ck_alterou_nota = abap_true.
        endif.

      endif.

      "Verificar se Documento está Lançado
      if me->carga-tp_status eq zif_carga=>st_status_aberto and me->zif_carga~at_manutencao eq abap_false or ck_alterou_nota eq abap_true.

        "Verificar se Documento está lançado!
        data: lc_nf_number        type  j_1bnfdoc-nfnum,
              lc_model            type  j_1bnfdoc-model,
              lc_series           type  j_1bnfdoc-series,
              lc_subseries        type  j_1bnfdoc-subser,
              lc_partner_id       type  j_1bnfdoc-parid,
              lc_partner_type     type  j_1bnfdoc-partyp,
              lc_date             type  datum,
              lc_i_nfeflag        type  j_1bnfe,
              lc_i_nfnum9         type  j_1bdocnum9,
              lc_doc_number       type  j_1bnfdoc-docnum,
              lc_nr_chave_nfe     type  zsdt0001nt-nr_chave_nfe,
              lc_id_fornecedor    type  zsdt0001nt-id_fornecedor,
              lc_nr_nota2         type  zsdt0001nt-nr_nota,
              lc_nr_fornecedor_ie type  zsdt0001nt-nr_fornecedor_ie,
              lc_docnum_np        type  zsdt0001nt-docnum_np,
              lc_id_entrada       type  zsdt0001nt-id_entrada.

        case i_nota_fiscal-id_mod_fiscal.
          when zif_carga=>st_model_fiscal_papel.
            lc_i_nfeflag = abap_false.
            lc_nf_number = i_nota_fiscal-nr_nota.
          when zif_carga=>st_model_fiscal_eletronico.
            lc_i_nfeflag = abap_true.
            lc_i_nfnum9  = i_nota_fiscal-nr_nota.
        endcase.

        lc_model        = i_nota_fiscal-id_mod_fiscal.
        lc_series       = i_nota_fiscal-nm_serie.
        lc_partner_id   = i_nota_fiscal-id_fornecedor.
        lc_partner_type = wa_j_1baa-partyp.
        lc_date         = i_nota_fiscal-dt_emissao.
*-CS2021000183-#71105-26.04.2022-JT-inicio
        lc_docnum_np        = i_nota_fiscal-docnum_np.
        lc_nr_chave_nfe     = i_nota_fiscal-nr_chave_nfe.
        lc_id_fornecedor    = i_nota_fiscal-id_fornecedor.
        lc_nr_nota2         = i_nota_fiscal-nr_nota.
        lc_nr_fornecedor_ie = i_nota_fiscal-nr_fornecedor_ie.
        lc_id_entrada       = i_nota_fiscal-id_entrada.
*-CS2021000183-#71105-26.04.2022-JT-fim

        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = lc_series
          importing
            output = lc_series.

        call function 'Z_1B_NF_DOCUMENT_SELECT_2'
          exporting
            nf_number                = lc_nf_number
            model                    = lc_model
            series                   = lc_series
            subseries                = lc_subseries
            partner_id               = lc_partner_id
            partner_type             = lc_partner_type
            date                     = lc_date
            i_nfeflag                = lc_i_nfeflag
            i_nfnum9                 = lc_i_nfnum9
*-CS2021000183-#71105-26.04.2022-JT-inicio
            i_docnum_np              = lc_docnum_np
            i_nr_chave_nfe           = lc_nr_chave_nfe
            i_id_fornecedor          = lc_id_fornecedor
            i_nr_nota2               = lc_nr_nota2
            i_nr_fornecedor_ie       = lc_nr_fornecedor_ie
            i_id_entrada             = lc_id_entrada
            i_id_bukrs               = me->carga-id_bukrs
*-CS2021000183-#71105-26.04.2022-JT-fim
          importing
            doc_number               = lc_doc_number
          exceptions
            document_not_found       = 4
            doc_with_same_year_found = 5
            doc_with_diff_year_found = 6
            too_many_documents_found = 8
            others                   = 8.

        case sy-subrc.
          when 0.
            if i_nota_fiscal-docnum eq lc_doc_number.
              data(lc_erro_nota) = abap_false.
            else.
              lc_erro_nota = abap_true.
              message e398(00) with 'Para NF informada já tem o registro fiscal nro' lc_doc_number '. Verificar com a Área Fiscal' into data(texto).
            endif.
          when 4.
          when 5.
            lc_erro_nota = abap_true.
            message e291(8b) with i_xblnr lc_partner_id into texto.
          when 6.
            lc_erro_nota = abap_true.
            message e264(8b) with i_xblnr lc_partner_id into texto.
          when 8.
            lc_erro_nota = abap_true.
        endcase.

        if lc_erro_nota eq abap_true.
          raise exception type zcx_carga
            exporting
              textid = value #( msgid  = sy-msgid
                                msgno  = sy-msgno
                                attr1  = sy-msgv1
                                attr2  = sy-msgv2
                                attr3  = sy-msgv3
                                attr4  = sy-msgv4 )
              msgid  = sy-msgid
              msgno  = sy-msgno
              msgty  = 'E'
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = sy-msgv3
              msgv4  = sy-msgv4.
        endif.

      endif.

      if zcl_string=>length( text = conv #( i_nota_fiscal-ds_observacao ) ) gt 50.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid  = zcx_carga=>zcx_erro_observacao_nota-msgid
                              msgno  = zcx_carga=>zcx_erro_observacao_nota-msgno )
            msgid  = zcx_carga=>zcx_erro_observacao_nota-msgid
            msgno  = zcx_carga=>zcx_erro_observacao_nota-msgno
            msgty  = 'E'.
      endif.

    endif.

  endmethod.


  METHOD ZIF_CARGA~GET_VERIFICA_EXISTE_SAIDA.

    DATA: ENTRADA_CARGA TYPE REF TO ZIF_CARGA,
          OB_ROMANEIO   TYPE REF TO ZCL_ROMANEIO.

    SELECT SINGLE * INTO @DATA(WA_ZSDT0001NT)
      FROM ZSDT0001NT
     WHERE DOCNUM EQ @I_DOCNUM.

    CHECK SY-SUBRC IS INITIAL.

    ENTRADA_CARGA =
    ZCL_FACTORY_CARGA=>ZIF_FACTORY_CARGA~GET_INSTANCE(
      )->SET_FACTORY_OBJETO_ID( I_ID_CARGA = ENTRADA_CARGA->CARGA-ID_CARGA
      )->GET_FACTORY_OBJETO(
      ).

    ENTRADA_CARGA->GET_ROMANEIO_SAIDA( EXPORTING I_ID_CARGA  = ENTRADA_CARGA->CARGA-ID_CARGA IMPORTING E_ROMANEIOS = DATA(ROMANEIOS) ).

    LOOP AT ROMANEIOS INTO DATA(WA_ROMANEIO).
      IF WA_ROMANEIO-ST_PROC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ROMANEIO_SAIDA_DOC-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_ROMANEIO_SAIDA_DOC-MSGNO )
            MSGTY  = 'E'
            MSGID  = ZCX_CARGA=>ZCX_ROMANEIO_SAIDA_DOC-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_ROMANEIO_SAIDA_DOC-MSGNO.
      ENDIF.
    ENDLOOP.

    IF I_EXCLUIR_ROMANEIO EQ ABAP_TRUE.
      CREATE OBJECT OB_ROMANEIO.
      LOOP AT ROMANEIOS INTO WA_ROMANEIO.
        OB_ROMANEIO->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = WA_ROMANEIO-CH_REFERENCIA ).
        OB_ROMANEIO->ZIF_CADASTRO~EXCLUIR_REGISTRO( ).
      ENDLOOP.
      CLEAR OB_ROMANEIO.
    ENDIF.

    ENTRADA_CARGA->FREE( ).

    CLEAR: ENTRADA_CARGA.

  ENDMETHOD.


  method zif_carga~gravar_registro.

    data e_id_classificacao type zsdt0001nt-id_classificacao.

    r_carga = me.

    e_gravou = abap_false.

    me->validar_registro( importing e_validou = data(e_validou)  ).

    if e_validou eq abap_true.

      if me->zif_carga~at_manutencao eq abap_false.

        if me->carga-id_carga is initial.
          me->get_new_id_carga( importing e_id_carga = data(e_id_carga) )->set_id_carga( i_id_carga = e_id_carga ).
          "US 143677 - transgenia por nota
          loop at me->classificacao_notas  assigning field-symbol(<fs_class>) .
            if <fs_class>-id_classificacao is not initial.
              read table me->documento_fiscal assigning field-symbol(<fs_nota2>)  with key nr_nota = <fs_class>-id_classificacao.
              if sy-subrc = 0.
                me->get_new_id_classificao( importing e_id_classificacao = e_id_classificacao ).
                <fs_class>-id_carga         = me->carga-id_carga.
                <fs_class>-id_classificacao = e_id_classificacao.
                <fs_nota2>-id_classificacao = e_id_classificacao.
              endif.
            endif.
          endloop.
          "US 143677 - transgenia por nota
          clear e_id_classificacao.
          if me->classificacao-id_classificacao is initial.
            me->get_new_id_classificao( importing e_id_classificacao = e_id_classificacao )->set_id_classificacao( i_id_classificacao = e_id_classificacao ).
          endif.

          loop at me->documento_fiscal assigning field-symbol(<fs_nota>) where id_carga is initial.
            <fs_nota>-id_carga = me->carga-id_carga.
            if <fs_nota>-id_classificacao is initial.
              <fs_nota>-id_classificacao = e_id_classificacao.
            endif.
          endloop.

          loop at me->documento_fiscal_imp_ret assigning field-symbol(<fs_imp_ret>).
            <fs_imp_ret>-id_carga = me->carga-id_carga.
          endloop.

*          if me->classificacao-id_classificacao is initial.
*            me->get_new_id_classificao( importing e_id_classificacao = data(e_id_classificacao) )->set_id_classificacao( i_id_classificacao = e_id_classificacao ).
*          endif.


          loop at me->resultado assigning field-symbol(<fs_resultado>) .
            <fs_resultado>-id_carga         = me->carga-id_carga.
            <fs_resultado>-id_classificacao = me->classificacao-id_classificacao.
          endloop.

          loop at me->zif_carga~resultado_avariado assigning field-symbol(<fs_resultado_avariado>) .
            <fs_resultado_avariado>-id_carga         = me->carga-id_carga.
            <fs_resultado_avariado>-id_classificacao = me->classificacao-id_classificacao.
          endloop.

          loop at me->zif_carga~blocos assigning field-symbol(<fs_bloco>).
            <fs_bloco>-id_carga = me->carga-id_carga.
          endloop.

          loop at me->zif_carga~take_up assigning field-symbol(<fs_take>).
            <fs_take>-id_carga = me->carga-id_carga.
          endloop.

          loop at me->zif_carga~ordem_venda assigning field-symbol(<fs_ordem>).
            <fs_ordem>-id_carga = me->carga-id_carga.
          endloop.

          loop at me->zif_carga~pedido_compra assigning field-symbol(<fs_pedido>).
            <fs_pedido>-id_carga = me->carga-id_carga.
          endloop.
        else.

          "US 143677 - transgenia por nota
          loop at me->classificacao_notas  assigning <fs_class> .
            if <fs_class>-id_classificacao is not initial.
              read table me->documento_fiscal assigning <fs_nota> with key nr_nota = <fs_class>-id_classificacao.
              if sy-subrc = 0.
                me->get_new_id_classificao( importing e_id_classificacao = e_id_classificacao ).
                <fs_class>-id_carga         = me->carga-id_carga.
                <fs_class>-id_classificacao = e_id_classificacao.
                <fs_nota>-id_classificacao  = e_id_classificacao.
              endif.
            endif.
          endloop.
          "US 143677 - transgenia por nota
        endif.


        me->zif_carga~set_ajustar_rat_ordem_venda( ).

        "Registra Alterações."""""""""""""""""""""""""""""""""""""""""""""""
        me->set_logs_alteracao( ).
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        delete from zsdt0001rs_03  where id_carga eq me->carga-id_carga.
        delete from zsdt0001rs     where id_carga eq me->carga-id_carga.
        delete from zsdt0001nt     where id_carga eq me->carga-id_carga.
        delete from zsdt0001tk     where id_carga eq me->carga-id_carga.
        delete from zsdt0001nt_ret where id_carga eq me->carga-id_carga.
        delete from zsdt0001cl     where id_carga eq me->carga-id_carga.
        delete from zsdt0001ov     where id_carga eq me->carga-id_carga.
        delete from zsdt0001ek     where id_carga eq me->carga-id_carga.
        delete from zsdt0001fd     where id_carga eq me->carga-id_carga.

        "Carga
        modify zsdt0001cg from me->carga.
        "Classificação da Carga
        modify zsdt0001cl from me->classificacao.
        "Classificação da Nota caso seja desmambrado
        modify zsdt0001cl from table me->classificacao_notas.
        "Nota Fiscal
        modify zsdt0001nt from table me->documento_fiscal.
        "Nota Fiscal - Impostos Retidos
        modify zsdt0001nt_ret from table me->documento_fiscal_imp_ret.
        "Nota Fiscal - Take UP's
        modify zsdt0001tk from table me->zif_carga~take_up.
        "Ordem de Venda
        modify zsdt0001ov from table me->zif_carga~ordem_venda.
        "Pedido de Compra
        modify zsdt0001ek from table me->zif_carga~pedido_compra.
        "Blocos/Fardos da Ordem ou Pedido de Compra
        modify zsdt0001fd from table me->zif_carga~blocos.
        "Resultados da(s) Classificação(ões)
        modify zsdt0001rs from table me->resultado.
        "Resultados da(s) Classificação(ões) Avariado
        modify zsdt0001rs_03 from table me->zif_carga~resultado_avariado.

      else.

        data: wa_zsdt0001acb     type zsdt0001acb,
              wa_zsdt0001acg     type zsdt0001acg,
              it_zsdt0001acl     type table of zsdt0001acl,
              wa_zsdt0001acl     type zsdt0001acl,
              it_zsdt0001ant     type table of zsdt0001ant,
              wa_zsdt0001ant     type zsdt0001ant,
              it_zsdt0001ant_ret type table of zsdt0001ant_ret,
              wa_zsdt0001ant_ret type zsdt0001ant_ret,
              it_zsdt0001aov     type table of zsdt0001aov,
              wa_zsdt0001aov     type zsdt0001aov,
              it_zsdt0001aek     type table of zsdt0001aek,
              wa_zsdt0001aek     type zsdt0001aek,
              it_zsdt0001ars     type table of zsdt0001ars,
              wa_zsdt0001ars     type zsdt0001ars,
              it_zsdt0001ars_03  type table of zsdt0001ars_03,
              wa_zsdt0001ars_03  type zsdt0001ars_03,
              it_zsdt0001atk     type table of zsdt0001atk,
              wa_zsdt0001atk     type zsdt0001atk,
              it_zsdt0001afd     type table of zsdt0001afd,
              wa_zsdt0001afd     type zsdt0001afd.

        if me->zif_carga~solicitacao_manutencao-id_solicitacao is initial.
          "Gerar Número de Solicitação
          me->zif_carga~get_new_id_solicitacao_manut( importing e_id_solicitacao = data(e_id_solicitacao) )->set_id_solic_manut( i_id_solicitacao = e_id_solicitacao ).
          me->zif_carga~solicitacao_manutencao-dt_solicitacao = sy-datlo.
          me->zif_carga~solicitacao_manutencao-hr_solicitacao = sy-timlo.
          me->zif_carga~solicitacao_manutencao-us_solicitacao = sy-uname.
        endif.

        if me->classificacao-id_classificacao is initial.
          me->get_new_id_classificao( importing e_id_classificacao = e_id_classificacao )->set_id_classificacao( i_id_classificacao = e_id_classificacao ).
          loop at me->resultado assigning <fs_resultado>.
            <fs_resultado>-id_classificacao = e_id_classificacao.
          endloop.
          loop at me->zif_carga~resultado_avariado assigning <fs_resultado_avariado>.
            <fs_resultado_avariado>-id_classificacao = e_id_classificacao.
          endloop.
        endif.

        delete from zsdt0001ars_03 where id_solicitacao eq me->zif_carga~solicitacao_manutencao-id_solicitacao.
        delete from zsdt0001acb where id_solicitacao eq me->zif_carga~solicitacao_manutencao-id_solicitacao.
        delete from zsdt0001ars where id_solicitacao eq me->zif_carga~solicitacao_manutencao-id_solicitacao.
        delete from zsdt0001ant where id_solicitacao eq me->zif_carga~solicitacao_manutencao-id_solicitacao.
        delete from zsdt0001atk where id_solicitacao eq me->zif_carga~solicitacao_manutencao-id_solicitacao.
        delete from zsdt0001ant_ret where id_solicitacao eq me->zif_carga~solicitacao_manutencao-id_solicitacao.
        delete from zsdt0001acl where id_solicitacao eq me->zif_carga~solicitacao_manutencao-id_solicitacao.
        delete from zsdt0001aov where id_solicitacao eq me->zif_carga~solicitacao_manutencao-id_solicitacao.
        delete from zsdt0001aek where id_solicitacao eq me->zif_carga~solicitacao_manutencao-id_solicitacao.
        delete from zsdt0001afd where id_solicitacao eq me->zif_carga~solicitacao_manutencao-id_solicitacao.

        move-corresponding me->carga to wa_zsdt0001acg.
        wa_zsdt0001acg-id_solicitacao  = me->zif_carga~solicitacao_manutencao-id_solicitacao.
        wa_zsdt0001acg-id_carga_origem = me->carga-id_carga.

        move-corresponding me->classificacao to wa_zsdt0001acl.
        wa_zsdt0001acl-id_solicitacao  = me->zif_carga~solicitacao_manutencao-id_solicitacao.
        wa_zsdt0001acl-id_carga_origem = me->carga-id_carga.
        append wa_zsdt0001acl to it_zsdt0001acl.

        "BUG174959 (se não tiver classificacao na nota
        loop at me->classificacao_notas  assigning <fs_class> .
          if <fs_class>-id_classificacao is not initial.
            read table me->documento_fiscal assigning <fs_nota> with key nr_nota = <fs_class>-id_classificacao.
            if sy-subrc = 0.
              me->get_new_id_classificao( importing e_id_classificacao = e_id_classificacao ).
              <fs_class>-id_carga         = me->carga-id_carga.
              <fs_class>-id_classificacao = e_id_classificacao.
              <fs_nota>-id_classificacao  = e_id_classificacao.
            endif.
          endif.
        endloop.
                                                            "BUG174959

        loop at me->classificacao_notas into data(wa_classi).
          move-corresponding wa_classi to wa_zsdt0001acl.
          wa_zsdt0001acl-id_solicitacao  = me->zif_carga~solicitacao_manutencao-id_solicitacao.
          wa_zsdt0001acl-id_carga_origem = me->carga-id_carga.
          append wa_zsdt0001acl to it_zsdt0001acl.
        endloop.

        loop at me->documento_fiscal into data(wa_nota).
          move-corresponding wa_nota to wa_zsdt0001ant.
          wa_zsdt0001ant-id_solicitacao  = me->zif_carga~solicitacao_manutencao-id_solicitacao.
          wa_zsdt0001ant-id_carga_origem = me->carga-id_carga.
          wa_zsdt0001ant-id_nota_origem  = wa_nota-id_nota.
          append wa_zsdt0001ant to it_zsdt0001ant.
        endloop.

        loop at me->zif_carga~take_up into data(wa_take_up).
          move-corresponding wa_take_up to wa_zsdt0001atk.
          wa_zsdt0001atk-id_solicitacao   = me->zif_carga~solicitacao_manutencao-id_solicitacao.
          wa_zsdt0001atk-id_carga_origem  = me->carga-id_carga.
          wa_zsdt0001atk-id_nota_origem   = wa_take_up-id_nota.
          wa_zsdt0001atk-id_takeup_origem = wa_take_up-id_takeup.
          append wa_zsdt0001atk to it_zsdt0001atk.
        endloop.

        loop at me->documento_fiscal_imp_ret into data(wa_nota_ret).
          move-corresponding wa_nota_ret to wa_zsdt0001ant_ret.
          wa_zsdt0001ant_ret-id_solicitacao  = me->zif_carga~solicitacao_manutencao-id_solicitacao.
          wa_zsdt0001ant_ret-id_carga_origem = me->carga-id_carga.
          wa_zsdt0001ant_ret-id_nota_origem  = wa_nota_ret-id_nota.
          append wa_zsdt0001ant_ret to it_zsdt0001ant_ret.
        endloop.

        loop at me->zif_carga~ordem_venda into data(wa_ordem).
          move-corresponding wa_ordem to wa_zsdt0001aov.
          wa_zsdt0001aov-id_solicitacao = me->zif_carga~solicitacao_manutencao-id_solicitacao.
          wa_zsdt0001aov-id_carga_origem = me->carga-id_carga.
          append wa_zsdt0001aov to it_zsdt0001aov.
        endloop.

        loop at me->zif_carga~pedido_compra into data(wa_pedido).
          move-corresponding wa_pedido to wa_zsdt0001aek.
          wa_zsdt0001aek-id_solicitacao = me->zif_carga~solicitacao_manutencao-id_solicitacao.
          wa_zsdt0001aek-id_carga_origem = me->carga-id_carga.
          append wa_zsdt0001aek to it_zsdt0001aek.
        endloop.

        loop at me->zif_carga~blocos into data(wa_blocos).
          move-corresponding wa_blocos to wa_zsdt0001afd.
          wa_zsdt0001afd-id_solicitacao  = me->zif_carga~solicitacao_manutencao-id_solicitacao.
          wa_zsdt0001afd-id_carga_origem = me->carga-id_carga.
          append wa_zsdt0001afd to it_zsdt0001afd.
        endloop.

        loop at me->resultado into data(wa_resultado).
          move-corresponding wa_resultado to wa_zsdt0001ars.
          wa_zsdt0001ars-id_solicitacao  = me->zif_carga~solicitacao_manutencao-id_solicitacao.
          wa_zsdt0001ars-id_carga_origem = me->carga-id_carga.
          append wa_zsdt0001ars to it_zsdt0001ars.
        endloop.

        loop at me->zif_carga~resultado_avariado into data(wa_resultado_avariado).
          move-corresponding wa_resultado_avariado to wa_zsdt0001ars_03.
          wa_zsdt0001ars_03-id_solicitacao  = me->zif_carga~solicitacao_manutencao-id_solicitacao.
          wa_zsdt0001ars_03-id_carga_origem = me->carga-id_carga.
          append wa_zsdt0001ars_03 to it_zsdt0001ars_03.
        endloop.

        "Solicitação de Manutenção
        modify zsdt0001acb from me->zif_carga~solicitacao_manutencao.
        "Carga
        modify zsdt0001acg from wa_zsdt0001acg.
        "Classificação
        modify zsdt0001acl from table it_zsdt0001acl.
        "Nota Fiscal
        modify zsdt0001ant from table it_zsdt0001ant.
        "Nota Fiscal - Impostos Retidos
        modify zsdt0001ant_ret from table it_zsdt0001ant_ret.
        "Nota Fiscal - Take UP
        modify zsdt0001atk from table it_zsdt0001atk.
        "Ordem de Venda
        modify zsdt0001aov from table it_zsdt0001aov.
        "Pedido de Compra
        modify zsdt0001aek from table it_zsdt0001aek.
        "Resultados da(s) Classificação(ões)
        modify zsdt0001ars from table it_zsdt0001ars.
        "Resultados da(s) Classificação(ões) Avariado
        modify zsdt0001ars_03 from table it_zsdt0001ars_03.
        "Fardos de Algodão
        modify zsdt0001afd from table it_zsdt0001afd.

      endif.

      commit work.
      e_gravou = abap_true.
      me->ck_alterou = abap_false.
      message s072.

    endif.

  endmethod.


  METHOD ZIF_CARGA~LIMPAR_REGISTRO.

    R_CARGA = ME.

    CLEAR: ME->CARGA,
           ME->CLASSIFICACAO,
           ME->RESULTADO,
           ME->ZIF_CARGA~RESULTADO_AVARIADO,
           ME->DOCUMENTO_FISCAL,
           ME->CLASSIFICACAO_NOTAS,
           ME->ZIF_CARGA~ORDEM_VENDA,
           ME->ZIF_CARGA~PEDIDO_COMPRA,
           ME->ZIF_CARGA~TAKE_UP,
           ME->ZIF_CARGA~BLOCOS,
           ME->ZIF_CARGA~AT_NAO_GERAR_BLOQUEIOS,
           ME->ZIF_CARGA~AT_TIPO_FRETE_ORDEM_VENDA,
           ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO,
           ME->ZIF_CARGA~AT_MANUTENCAO,
           ME->ZIF_CARGA~SOLICITACOES,
           ME->ZIF_CARGA~CK_DIGITADO_UMIDADE,
           ME->ZIF_CARGA~CK_DIGITADO_IMPUREZA,
           ME->ZIF_CARGA~CK_DIGITADO_ARDIDO,
           ME->ZIF_CARGA~CK_DIGITADO_AVARIADO,
           ME->ZIF_CARGA~CK_DIGITADO_QUEBRADO,
           ME->ZIF_CARGA~CK_DIGITADO_ESVERDEADO,
           ME->ZIF_CARGA~CK_DIGITADO_CARUNCHADO.

  ENDMETHOD.


  METHOD zif_carga~novo_registro.

    r_carga = me.

    me->limpar_registro( ).

    "Teste Amaggi RR1 e RR2 - Não Testado
    me->classificacao-in_gmo = zif_carga=>st_gmo_nao_testado.
    "Resultado 0
    me->classificacao-nr_resultado_01 = 0.
    "Resultado 0
    me->classificacao-nr_resultado_02 = 0.
    "Soma RR1 e RR2
    me->classificacao-nr_res_rr1_rr2  = 0.
    "Outro Participante - Não
    me->classificacao-in_srr_origem_partic = abap_false.
    "RR Declado (RR1) - Não
    me->classificacao-in_srr_declarado     = abap_false.
    "Intacta Declarado (RR2) - Não
    me->classificacao-in_srr_declarado_2   = abap_false.
    "Teste Monsanto (RR2) - Não Testado
    me->classificacao-in_teste_srr_2       = zif_carga=>st_gmo_nao_testado.

    me->carga-tp_frete                     = zif_carga=>st_tp_frete_fob.
    me->carga-tp_carga                     = zif_carga=>st_tp_carga_entrada_fob.

* CS2021000183 Parte 1 - Classificação automática - US 74975 - BG - INICIO
    IF me->carga-id_carga = ''.
 "     me->get_info_alv_apresentacao( IMPORTING e_apresentacao = DATA(e_carga) ).
"      e_carga_recebimento = e_carga-carga.

      SELECT * FROM zsdt0001rsw INTO TABLE @DATA(it_zsdt0001rsw) WHERE werks = @i_id_branch.

      LOOP AT it_zsdt0001rsw     INTO DATA(wa_resultado).

        CASE wa_resultado-tp_caracteristica.
          WHEN zif_carga=>st_tp_caract_class_umidade.
            e_carga_recebimento-nr_perc_umi = wa_resultado-nr_percentual_com.
            e_carga_recebimento-nr_qtde_umi = wa_resultado-nr_quantidade_com.
            ME->ZIF_CARGA~CK_DIGITADO_UMIDADE    = ABAP_TRUE.
          WHEN zif_carga=>st_tp_caract_class_impureza.
            e_carga_recebimento-nr_perc_imp = wa_resultado-nr_percentual_com.
            e_carga_recebimento-nr_qtde_imp = wa_resultado-nr_quantidade_com.
            ME->ZIF_CARGA~CK_DIGITADO_IMPUREZA   = ABAP_TRUE.
          WHEN zif_carga=>st_tp_caract_class_avariado.
            e_carga_recebimento-nr_perc_ava = wa_resultado-nr_percentual_com.
            e_carga_recebimento-nr_qtde_ava = wa_resultado-nr_quantidade_com.
            ME->ZIF_CARGA~CK_DIGITADO_AVARIADO   = ABAP_TRUE.
          WHEN zif_carga=>st_tp_caract_class_ardido.
            e_carga_recebimento-nr_perc_ard = wa_resultado-nr_percentual_com.
            e_carga_recebimento-nr_qtde_ard = wa_resultado-nr_quantidade_com.
            ME->ZIF_CARGA~CK_DIGITADO_ARDIDO     = ABAP_TRUE.
          WHEN zif_carga=>st_tp_caract_class_quebrado.
            e_carga_recebimento-nr_perc_que = wa_resultado-nr_percentual_com.
            e_carga_recebimento-nr_qtde_que = wa_resultado-nr_quantidade_com.
            ME->ZIF_CARGA~CK_DIGITADO_QUEBRADO   = ABAP_TRUE.
          WHEN zif_carga=>st_tp_caract_class_esverdeado.
            e_carga_recebimento-nr_perc_esv = wa_resultado-nr_percentual_com.
            e_carga_recebimento-nr_qtde_esv = wa_resultado-nr_quantidade_com.
             ME->ZIF_CARGA~CK_DIGITADO_ESVERDEADO = ABAP_TRUE.
          WHEN zif_carga=>st_tp_caract_class_carunchado.
            e_carga_recebimento-nr_perc_car = wa_resultado-nr_percentual_com.
            e_carga_recebimento-nr_qtde_car = wa_resultado-nr_quantidade_com.
            ME->ZIF_CARGA~CK_DIGITADO_CARUNCHADO = ABAP_TRUE.
        ENDCASE.

      ENDLOOP.

      SELECT * FROM zsdt0001rs_03w INTO TABLE @DATA(it_zsdt0001rs_03w) WHERE werks = @i_id_branch.

      LOOP AT it_zsdt0001rs_03w     INTO DATA(wa_resul_avariado).

        CASE wa_resul_avariado-tp_sub_carac_avariado.
          WHEN zif_carga=>st_tp_caract_sub_arq.
            e_carga_recebimento-nr_perc_ava_arq = wa_resul_avariado-nr_percentual_com.
          WHEN zif_carga=>st_tp_caract_sub_que.
            e_carga_recebimento-nr_perc_ava_que = wa_resul_avariado-nr_percentual_com.
          WHEN zif_carga=>st_tp_caract_sub_mof.
            e_carga_recebimento-nr_perc_ava_mof = wa_resul_avariado-nr_percentual_com.
          WHEN zif_carga=>st_tp_caract_sub_pic.
            e_carga_recebimento-nr_perc_ava_pic = wa_resul_avariado-nr_percentual_com.
          WHEN zif_carga=>st_tp_caract_sub_fer.
            e_carga_recebimento-nr_perc_ava_fer = wa_resul_avariado-nr_percentual_com.
          WHEN zif_carga=>st_tp_caract_sub_ger.
            e_carga_recebimento-nr_perc_ava_ger = wa_resul_avariado-nr_percentual_com.
          WHEN zif_carga=>st_tp_caract_sub_ard.
            e_carga_recebimento-nr_perc_ava_ard = wa_resul_avariado-nr_percentual_com.
          WHEN zif_carga=>st_tp_caract_sub_ges.
            e_carga_recebimento-nr_perc_ava_ges = wa_resul_avariado-nr_percentual_com.
        ENDCASE.

      ENDLOOP.


    ENDIF.
* CS2021000183 Parte 1 - Classificação automática - US 74975 - BG - FIM
  ENDMETHOD.


  METHOD ZIF_CARGA~PESQUISAR.

    R_CARGA = ME.

    DATA: LC_FILTRO  TYPE ZDE_FILTRO_ZSDT0001CG,
          WA_RETORNO TYPE ZDE_ZSDT0001CG_ALV,
          LC_RETORNO TYPE ZDE_ZSDT0001CG_ALV_T.

    DATA: RG_TP_CARGA TYPE TABLE OF ZDE_TP_CARGA_R.

    RG_TP_CARGA = VALUE #( SIGN = 'I' OPTION = 'EQ'
                            ( HIGH = SPACE LOW = SPACE )
                            ( HIGH = ZIF_CARGA=>ST_TP_CARGA_ENTRADA_FOB LOW = ZIF_CARGA=>ST_TP_CARGA_ENTRADA_FOB )
                          ).

    MOVE I_FILTROS TO LC_FILTRO.

    DESCRIBE TABLE LC_FILTRO-INRSAFRA LINES DATA(LC_SAFRA).
    DESCRIBE TABLE LC_FILTRO-IIDBUKRS LINES DATA(LC_BUKRS).
    DESCRIBE TABLE LC_FILTRO-IIDBRANC LINES DATA(LC_BRANCH).

    IF LC_SAFRA IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_OBG_INF_SAFRA_PSQ-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_OBG_INF_SAFRA_PSQ-MSGNO )
          MSGTY  = 'E'
          MSGID  = ZCX_CARGA=>ZCX_OBG_INF_SAFRA_PSQ-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_OBG_INF_SAFRA_PSQ-MSGNO.
    ENDIF.

    IF LC_BUKRS IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_OBG_INF_EMPRESA_PSQ-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_OBG_INF_EMPRESA_PSQ-MSGNO )
          MSGTY  = 'E'
          MSGID  = ZCX_CARGA=>ZCX_OBG_INF_EMPRESA_PSQ-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_OBG_INF_EMPRESA_PSQ-MSGNO.
    ENDIF.

    IF LC_BRANCH IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_OBG_INF_FILIAL_PSQ-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_OBG_INF_FILIAL_PSQ-MSGNO )
          MSGTY  = 'E'
          MSGID  = ZCX_CARGA=>ZCX_OBG_INF_FILIAL_PSQ-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_OBG_INF_FILIAL_PSQ-MSGNO.
    ENDIF.

    CLEAR: E_PESQUISOU, E_REGISTROS.

    IF ( LC_FILTRO-IIDENTRA[] IS NOT INITIAL OR LC_FILTRO-IRROMENT[] IS NOT INITIAL ) AND
       ( LC_FILTRO-IRROMSAI[] IS NOT INITIAL ).

      SELECT CA~ID_CARGA           AS ID_CARGA,
             CA~ID_ORDEM           AS ID_ORDEM,
             OD~NR_ORDEM           AS NR_ORDEM,
             CA~ID_LOCAL_ENTREGA   AS ID_LOCAL_ENTREGA,
             LE~DS_LOCAL_ENTREGA   AS DS_LOCAL_ENTREGA,
             CA~DT_MOVIMENTO       AS DT_MOVIMENTO,
             CA~NR_SAFRA           AS NR_SAFRA,
             CA~ID_BUKRS           AS ID_BUKRS,
             EN~BUTXT              AS BUTXT,
             CA~ID_BRANCH          AS ID_BRANCH,
             FN~NAME               AS NAME,
             CA~ID_AGENT_FRETE     AS ID_AGENT_FRETE,
             AG~NAME1              AS DS_AGENT_FRETE,
             CA~ID_LOCAL_COLETA    AS ID_LOCAL_COLETA,
             LC~NAME1              AS DS_LOCAL_COLETA,
             CA~ID_LOCAL_DESTINO   AS ID_LOCAL_DESTINO,
             LD~NAME1              AS DS_LOCAL_DESTINO,
             CA~ID_LOCAL_DESCARGA  AS ID_LOCAL_DESCARGA,
             LA~NAME1              AS DS_LOCAL_DESCARGA,
             CA~TP_FRETE           AS TP_FRETE,
             CA~ID_PRODUTO         AS ID_PRODUTO,
             MA~MAKTX              AS DS_PRODUTO,
             CA~NM_PESO_BRUTO      AS NM_PESO_BRUTO,
             CA~NM_PESO_TARA       AS NM_PESO_TARA,
             CA~NM_PESO_SUBTOTAL   AS NM_PESO_SUBTOTAL,
             CA~NM_PESO_DESCONTOS  AS NM_PESO_DESCONTOS,
             CA~NM_PESO_LIQUIDO    AS NM_PESO_LIQUIDO,
             CA~DS_PLACA_TRATOR    AS DS_PLACA_TRATOR,
             CA~ID_PROPRIETARIO    AS ID_PROPRIETARIO,
             PP~NAME1              AS DS_PROPRIETARIO,
             CA~DS_PLACA_REBOQ_1   AS DS_PLACA_REBOQ_1,
             CA~DS_PLACA_REBOQ_2   AS DS_PLACA_REBOQ_2,
             CA~DS_PLACA_REBOQ_3   AS DS_PLACA_REBOQ_3,
             CA~ID_MOTORISTA       AS ID_MOTORISTA,
             MT~NAME1              AS DS_MOTORISTA,
             CA~NR_TICKET          AS NR_TICKET,
             CA~IN_TRANSFERENCIA   AS IN_TRANSFERENCIA,
             CA~TP_STATUS          AS TP_STATUS,
             CA~DT_ABERTURA        AS DT_ABERTURA,
             CA~HR_ABERTURA        AS HR_ABERTURA,
             CA~US_ABERTURA        AS US_ABERTURA,
             CA~DT_FECHAMENTO      AS DT_FECHAMENTO,
             CA~HR_FECHAMENTO      AS HR_FECHAMENTO,
             CA~US_FECHAMENTO      AS US_FECHAMENTO,
             CA~DT_CONFERENCIA     AS DT_CONFERENCIA,
             CA~HR_CONFERENCIA     AS HR_CONFERENCIA,
             CA~US_CONFERENCIA     AS US_CONFERENCIA,
             CA~DT_CANCELAMENTO    AS DT_CANCELAMENTO,
             CA~HR_CANCELAMENTO    AS HR_CANCELAMENTO,
             CA~US_CANCELAMENTO    AS US_CANCELAMENTO,

             CL~ID_CLASSIFICACAO      AS ID_CLASSIFICACAO,
             CL~IN_GMO                AS IN_GMO,
             CL~NR_RESULTADO_01       AS NR_RESULTADO_01,
             CL~NR_RESULTADO_02       AS NR_RESULTADO_02,
             CL~NR_RES_RR1_RR2        AS NR_RES_RR1_RR2,
             CL~IN_GMO_03             AS IN_GMO_03,
             CL~IN_SRR_ORIGEM_PARTIC  AS IN_SRR_ORIGEM_PARTIC,
             CL~ID_OUTRO_PARTIC       AS ID_OUTRO_PARTIC,
             OT~NAME1                 AS DS_OUTRO_PARTIC,
             CL~IN_SRR_DECLARADO      AS IN_SRR_DECLARADO,
             CL~IN_TESTE_SRR          AS IN_TESTE_SRR,
             CL~IN_SRR_DECLARADO_2    AS IN_SRR_DECLARADO_2,
             CL~IN_TESTE_SRR_2        AS IN_TESTE_SRR_2,
             CL~TP_TRANSGENIA         AS TP_TRANSGENIA,

             R1~NR_PERCENTUAL_COM     AS NR_PERC_UMI,
             R2~NR_PERCENTUAL_COM     AS NR_PERC_IMP,
             R3~NR_PERCENTUAL_COM     AS NR_PERC_AVA,
             R4~NR_PERCENTUAL_COM     AS NR_PERC_ARD,
             R5~NR_PERCENTUAL_COM     AS NR_PERC_QUE,
             R6~NR_PERCENTUAL_COM     AS NR_PERC_ESV,
             R7~NR_PERCENTUAL_COM     AS NR_PERC_CAR,

             R1~NR_QUANTIDADE_COM     AS NR_QTDE_UMI,
             R2~NR_QUANTIDADE_COM     AS NR_QTDE_IMP,
             R3~NR_QUANTIDADE_COM     AS NR_QTDE_AVA,
             R4~NR_QUANTIDADE_COM     AS NR_QTDE_ARD,
             R5~NR_QUANTIDADE_COM     AS NR_QTDE_QUE,
             R6~NR_QUANTIDADE_COM     AS NR_QTDE_ESV,
             R7~NR_QUANTIDADE_COM     AS NR_QTDE_CAR

        INTO TABLE @DATA(IT_TAB)
        FROM ZSDT0001CG AS CA
       INNER JOIN ZSDT0001LE   AS LE ON LE~ID_LOCAL_ENTREGA EQ CA~ID_LOCAL_ENTREGA
       INNER JOIN T001         AS EN ON EN~BUKRS EQ CA~ID_BUKRS
       INNER JOIN J_1BBRANCH   AS FN ON FN~BUKRS EQ CA~ID_BUKRS AND FN~BRANCH EQ CA~ID_BRANCH
       INNER JOIN LFA1         AS LC ON LC~LIFNR EQ CA~ID_LOCAL_COLETA
       INNER JOIN LFA1         AS LD ON LD~LIFNR EQ CA~ID_LOCAL_DESTINO
       INNER JOIN KNA1         AS LA ON LA~KUNNR EQ CA~ID_LOCAL_DESCARGA
        LEFT JOIN ZSDT0001OD   AS OD ON OD~ID_ORDEM EQ CA~ID_ORDEM
        LEFT JOIN ZSDT0001CL   AS CL ON CL~ID_CARGA EQ CA~ID_CARGA AND CL~ID_CLASSIFICACAO EQ CA~ID_CLASSIFICACAO
        LEFT JOIN ZSDT0001RS   AS R1 ON R1~ID_CARGA EQ CL~ID_CARGA AND R1~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R1~TP_CARACTERISTICA EQ '01'
        LEFT JOIN ZSDT0001RS   AS R2 ON R2~ID_CARGA EQ CL~ID_CARGA AND R2~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R2~TP_CARACTERISTICA EQ '02'
        LEFT JOIN ZSDT0001RS   AS R3 ON R3~ID_CARGA EQ CL~ID_CARGA AND R3~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R3~TP_CARACTERISTICA EQ '03'
        LEFT JOIN ZSDT0001RS   AS R4 ON R4~ID_CARGA EQ CL~ID_CARGA AND R4~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R4~TP_CARACTERISTICA EQ '04'
        LEFT JOIN ZSDT0001RS   AS R5 ON R5~ID_CARGA EQ CL~ID_CARGA AND R5~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R5~TP_CARACTERISTICA EQ '05'
        LEFT JOIN ZSDT0001RS   AS R6 ON R6~ID_CARGA EQ CL~ID_CARGA AND R6~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R6~TP_CARACTERISTICA EQ '06'
        LEFT JOIN ZSDT0001RS   AS R7 ON R7~ID_CARGA EQ CL~ID_CARGA AND R7~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R7~TP_CARACTERISTICA EQ '07'
        LEFT JOIN LFA1         AS AG ON AG~LIFNR EQ CA~ID_AGENT_FRETE
        LEFT JOIN LFA1         AS PP ON PP~LIFNR EQ CA~ID_PROPRIETARIO
        LEFT JOIN LFA1         AS MT ON MT~LIFNR EQ CA~ID_MOTORISTA
        LEFT JOIN LFA1         AS OT ON OT~LIFNR EQ CL~ID_OUTRO_PARTIC
       INNER JOIN MAKT         AS MA ON MA~SPRAS EQ @SY-LANGU AND MA~MATNR EQ CA~ID_PRODUTO
       WHERE CA~TP_CARGA           IN @RG_TP_CARGA
         AND CA~ID_CARGA           IN @LC_FILTRO-IIDCARGA
         AND CA~ID_ORDEM           IN @LC_FILTRO-IIDORDEM

         AND EXISTS ( SELECT * FROM ZSDT0001NT AS NT
                       WHERE NT~ID_CARGA        EQ CA~ID_CARGA
                         AND NT~ID_ENTRADA      IN @LC_FILTRO-IIDENTRA
                         AND NT~NR_ROMANEIO_ENT IN @LC_FILTRO-IRROMENT )

         AND EXISTS ( SELECT * FROM ZSDT0001OV AS OV
                       WHERE OV~ID_CARGA        EQ CA~ID_CARGA
                         AND OV~NR_ROMANEIO_SAI IN @LC_FILTRO-IRROMSAI )

         AND CA~ID_LOCAL_ENTREGA   IN @LC_FILTRO-IIDLOCAL
         AND CA~DT_MOVIMENTO       IN @LC_FILTRO-IDTMOVIM
         AND CA~NR_SAFRA           IN @LC_FILTRO-INRSAFRA
         AND CA~ID_BUKRS           IN @LC_FILTRO-IIDBUKRS
         AND CA~ID_BRANCH          IN @LC_FILTRO-IIDBRANC
         AND CA~ID_AGENT_FRETE     IN @LC_FILTRO-IIDAGENT
         AND CA~ID_LOCAL_COLETA    IN @LC_FILTRO-IIDCOLET
         AND CA~ID_LOCAL_DESTINO   IN @LC_FILTRO-IIDDESTI
         AND CA~ID_LOCAL_DESCARGA  IN @LC_FILTRO-IIDDESCA
         AND CA~ID_PRODUTO         IN @LC_FILTRO-IIDPRODU
         AND CA~DS_PLACA_TRATOR    IN @LC_FILTRO-IDSTRATO
         AND CA~DS_PLACA_REBOQ_1   IN @LC_FILTRO-IDSREBO1
         AND CA~DS_PLACA_REBOQ_2   IN @LC_FILTRO-IDSREBO2
         AND CA~DS_PLACA_REBOQ_3   IN @LC_FILTRO-IDSREBO3
         AND CA~ID_MOTORISTA       IN @LC_FILTRO-IIDMOTOR
         AND CA~NR_TICKET          IN @LC_FILTRO-INRTICKE
         AND CA~TP_STATUS          IN @LC_FILTRO-ITPSTATU
         AND CA~DT_ABERTURA        IN @LC_FILTRO-IDTABERT
         AND CA~HR_ABERTURA        IN @LC_FILTRO-IHRABERT
         AND CA~DT_FECHAMENTO      IN @LC_FILTRO-IDTFECHA
         AND CA~HR_FECHAMENTO      IN @LC_FILTRO-IHRFECHA
       ORDER BY CA~ID_CARGA.

    ELSEIF ( LC_FILTRO-IIDENTRA[] IS NOT INITIAL OR LC_FILTRO-IRROMENT[] IS NOT INITIAL ) AND
           ( LC_FILTRO-IRROMSAI[] IS INITIAL ).

      SELECT CA~ID_CARGA           AS ID_CARGA,
             CA~ID_ORDEM           AS ID_ORDEM,
             OD~NR_ORDEM           AS NR_ORDEM,
             CA~ID_LOCAL_ENTREGA   AS ID_LOCAL_ENTREGA,
             LE~DS_LOCAL_ENTREGA   AS DS_LOCAL_ENTREGA,
             CA~DT_MOVIMENTO       AS DT_MOVIMENTO,
             CA~NR_SAFRA           AS NR_SAFRA,
             CA~ID_BUKRS           AS ID_BUKRS,
             EN~BUTXT              AS BUTXT,
             CA~ID_BRANCH          AS ID_BRANCH,
             FN~NAME               AS NAME,
             CA~ID_AGENT_FRETE     AS ID_AGENT_FRETE,
             AG~NAME1              AS DS_AGENT_FRETE,
             CA~ID_LOCAL_COLETA    AS ID_LOCAL_COLETA,
             LC~NAME1              AS DS_LOCAL_COLETA,
             CA~ID_LOCAL_DESTINO   AS ID_LOCAL_DESTINO,
             LD~NAME1              AS DS_LOCAL_DESTINO,
             CA~ID_LOCAL_DESCARGA  AS ID_LOCAL_DESCARGA,
             LA~NAME1              AS DS_LOCAL_DESCARGA,
             CA~TP_FRETE           AS TP_FRETE,
             CA~ID_PRODUTO         AS ID_PRODUTO,
             MA~MAKTX              AS DS_PRODUTO,
             CA~NM_PESO_BRUTO      AS NM_PESO_BRUTO,
             CA~NM_PESO_TARA       AS NM_PESO_TARA,
             CA~NM_PESO_SUBTOTAL   AS NM_PESO_SUBTOTAL,
             CA~NM_PESO_DESCONTOS  AS NM_PESO_DESCONTOS,
             CA~NM_PESO_LIQUIDO    AS NM_PESO_LIQUIDO,
             CA~DS_PLACA_TRATOR    AS DS_PLACA_TRATOR,
             CA~ID_PROPRIETARIO    AS ID_PROPRIETARIO,
             PP~NAME1              AS DS_PROPRIETARIO,
             CA~DS_PLACA_REBOQ_1   AS DS_PLACA_REBOQ_1,
             CA~DS_PLACA_REBOQ_2   AS DS_PLACA_REBOQ_2,
             CA~DS_PLACA_REBOQ_3   AS DS_PLACA_REBOQ_3,
             CA~ID_MOTORISTA       AS ID_MOTORISTA,
             MT~NAME1              AS DS_MOTORISTA,
             CA~NR_TICKET          AS NR_TICKET,
             CA~IN_TRANSFERENCIA   AS IN_TRANSFERENCIA,
             CA~TP_STATUS          AS TP_STATUS,
             CA~DT_ABERTURA        AS DT_ABERTURA,
             CA~HR_ABERTURA        AS HR_ABERTURA,
             CA~US_ABERTURA        AS US_ABERTURA,
             CA~DT_FECHAMENTO      AS DT_FECHAMENTO,
             CA~HR_FECHAMENTO      AS HR_FECHAMENTO,
             CA~US_FECHAMENTO      AS US_FECHAMENTO,
             CA~DT_CONFERENCIA     AS DT_CONFERENCIA,
             CA~HR_CONFERENCIA     AS HR_CONFERENCIA,
             CA~US_CONFERENCIA     AS US_CONFERENCIA,
             CA~DT_CANCELAMENTO    AS DT_CANCELAMENTO,
             CA~HR_CANCELAMENTO    AS HR_CANCELAMENTO,
             CA~US_CANCELAMENTO    AS US_CANCELAMENTO,

             CL~ID_CLASSIFICACAO      AS ID_CLASSIFICACAO,
             CL~IN_GMO                AS IN_GMO,
             CL~NR_RESULTADO_01       AS NR_RESULTADO_01,
             CL~NR_RESULTADO_02       AS NR_RESULTADO_02,
             CL~NR_RES_RR1_RR2        AS NR_RES_RR1_RR2,
             CL~IN_GMO_03             AS IN_GMO_03,
             CL~IN_SRR_ORIGEM_PARTIC  AS IN_SRR_ORIGEM_PARTIC,
             CL~ID_OUTRO_PARTIC       AS ID_OUTRO_PARTIC,
             OT~NAME1                 AS DS_OUTRO_PARTIC,
             CL~IN_SRR_DECLARADO      AS IN_SRR_DECLARADO,
             CL~IN_TESTE_SRR          AS IN_TESTE_SRR,
             CL~IN_SRR_DECLARADO_2    AS IN_SRR_DECLARADO_2,
             CL~IN_TESTE_SRR_2        AS IN_TESTE_SRR_2,
             CL~TP_TRANSGENIA         AS TP_TRANSGENIA,

             R1~NR_PERCENTUAL_COM     AS NR_PERC_UMI,
             R2~NR_PERCENTUAL_COM     AS NR_PERC_IMP,
             R3~NR_PERCENTUAL_COM     AS NR_PERC_AVA,
             R4~NR_PERCENTUAL_COM     AS NR_PERC_ARD,
             R5~NR_PERCENTUAL_COM     AS NR_PERC_QUE,
             R6~NR_PERCENTUAL_COM     AS NR_PERC_ESV,
             R7~NR_PERCENTUAL_COM     AS NR_PERC_CAR,

             R1~NR_QUANTIDADE_COM     AS NR_QTDE_UMI,
             R2~NR_QUANTIDADE_COM     AS NR_QTDE_IMP,
             R3~NR_QUANTIDADE_COM     AS NR_QTDE_AVA,
             R4~NR_QUANTIDADE_COM     AS NR_QTDE_ARD,
             R5~NR_QUANTIDADE_COM     AS NR_QTDE_QUE,
             R6~NR_QUANTIDADE_COM     AS NR_QTDE_ESV,
             R7~NR_QUANTIDADE_COM     AS NR_QTDE_CAR

        INTO TABLE @IT_TAB
        FROM ZSDT0001CG AS CA
       INNER JOIN ZSDT0001LE   AS LE ON LE~ID_LOCAL_ENTREGA EQ CA~ID_LOCAL_ENTREGA
       INNER JOIN T001         AS EN ON EN~BUKRS EQ CA~ID_BUKRS
       INNER JOIN J_1BBRANCH   AS FN ON FN~BUKRS EQ CA~ID_BUKRS AND FN~BRANCH EQ CA~ID_BRANCH
       INNER JOIN LFA1         AS LC ON LC~LIFNR EQ CA~ID_LOCAL_COLETA
       INNER JOIN LFA1         AS LD ON LD~LIFNR EQ CA~ID_LOCAL_DESTINO
       INNER JOIN KNA1         AS LA ON LA~KUNNR EQ CA~ID_LOCAL_DESCARGA
        LEFT JOIN ZSDT0001OD   AS OD ON OD~ID_ORDEM EQ CA~ID_ORDEM
        LEFT JOIN ZSDT0001CL   AS CL ON CL~ID_CARGA EQ CA~ID_CARGA AND CL~ID_CLASSIFICACAO EQ CA~ID_CLASSIFICACAO
        LEFT JOIN ZSDT0001RS   AS R1 ON R1~ID_CARGA EQ CL~ID_CARGA AND R1~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R1~TP_CARACTERISTICA EQ '01'
        LEFT JOIN ZSDT0001RS   AS R2 ON R2~ID_CARGA EQ CL~ID_CARGA AND R2~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R2~TP_CARACTERISTICA EQ '02'
        LEFT JOIN ZSDT0001RS   AS R3 ON R3~ID_CARGA EQ CL~ID_CARGA AND R3~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R3~TP_CARACTERISTICA EQ '03'
        LEFT JOIN ZSDT0001RS   AS R4 ON R4~ID_CARGA EQ CL~ID_CARGA AND R4~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R4~TP_CARACTERISTICA EQ '04'
        LEFT JOIN ZSDT0001RS   AS R5 ON R5~ID_CARGA EQ CL~ID_CARGA AND R5~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R5~TP_CARACTERISTICA EQ '05'
        LEFT JOIN ZSDT0001RS   AS R6 ON R6~ID_CARGA EQ CL~ID_CARGA AND R6~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R6~TP_CARACTERISTICA EQ '06'
        LEFT JOIN ZSDT0001RS   AS R7 ON R7~ID_CARGA EQ CL~ID_CARGA AND R7~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R7~TP_CARACTERISTICA EQ '07'
        LEFT JOIN LFA1         AS AG ON AG~LIFNR EQ CA~ID_AGENT_FRETE
        LEFT JOIN LFA1         AS PP ON PP~LIFNR EQ CA~ID_PROPRIETARIO
        LEFT JOIN LFA1         AS MT ON MT~LIFNR EQ CA~ID_MOTORISTA
        LEFT JOIN LFA1         AS OT ON OT~LIFNR EQ CL~ID_OUTRO_PARTIC
       INNER JOIN MAKT         AS MA ON MA~SPRAS EQ @SY-LANGU AND MA~MATNR EQ CA~ID_PRODUTO
       WHERE CA~TP_CARGA           IN @RG_TP_CARGA
         AND CA~ID_CARGA           IN @LC_FILTRO-IIDCARGA
         AND CA~ID_ORDEM           IN @LC_FILTRO-IIDORDEM

         AND EXISTS ( SELECT * FROM ZSDT0001NT AS NT
                       WHERE NT~ID_CARGA        EQ CA~ID_CARGA
                         AND NT~ID_ENTRADA      IN @LC_FILTRO-IIDENTRA
                         AND NT~NR_ROMANEIO_ENT IN @LC_FILTRO-IRROMENT )

         AND CA~ID_LOCAL_ENTREGA   IN @LC_FILTRO-IIDLOCAL
         AND CA~DT_MOVIMENTO       IN @LC_FILTRO-IDTMOVIM
         AND CA~NR_SAFRA           IN @LC_FILTRO-INRSAFRA
         AND CA~ID_BUKRS           IN @LC_FILTRO-IIDBUKRS
         AND CA~ID_BRANCH          IN @LC_FILTRO-IIDBRANC
         AND CA~ID_AGENT_FRETE     IN @LC_FILTRO-IIDAGENT
         AND CA~ID_LOCAL_COLETA    IN @LC_FILTRO-IIDCOLET
         AND CA~ID_LOCAL_DESTINO   IN @LC_FILTRO-IIDDESTI
         AND CA~ID_LOCAL_DESCARGA  IN @LC_FILTRO-IIDDESCA
         AND CA~ID_PRODUTO         IN @LC_FILTRO-IIDPRODU
         AND CA~DS_PLACA_TRATOR    IN @LC_FILTRO-IDSTRATO
         AND CA~DS_PLACA_REBOQ_1   IN @LC_FILTRO-IDSREBO1
         AND CA~DS_PLACA_REBOQ_2   IN @LC_FILTRO-IDSREBO2
         AND CA~DS_PLACA_REBOQ_3   IN @LC_FILTRO-IDSREBO3
         AND CA~ID_MOTORISTA       IN @LC_FILTRO-IIDMOTOR
         AND CA~NR_TICKET          IN @LC_FILTRO-INRTICKE
         AND CA~TP_STATUS          IN @LC_FILTRO-ITPSTATU
         AND CA~DT_ABERTURA        IN @LC_FILTRO-IDTABERT
         AND CA~HR_ABERTURA        IN @LC_FILTRO-IHRABERT
         AND CA~DT_FECHAMENTO      IN @LC_FILTRO-IDTFECHA
         AND CA~HR_FECHAMENTO      IN @LC_FILTRO-IHRFECHA
       ORDER BY CA~ID_CARGA.

    ELSEIF ( LC_FILTRO-IIDENTRA[] IS INITIAL OR LC_FILTRO-IRROMENT[] IS INITIAL ) AND
           ( LC_FILTRO-IRROMSAI[] IS NOT INITIAL ).

      SELECT CA~ID_CARGA           AS ID_CARGA,
             CA~ID_ORDEM           AS ID_ORDEM,
             OD~NR_ORDEM           AS NR_ORDEM,
             CA~ID_LOCAL_ENTREGA   AS ID_LOCAL_ENTREGA,
             LE~DS_LOCAL_ENTREGA   AS DS_LOCAL_ENTREGA,
             CA~DT_MOVIMENTO       AS DT_MOVIMENTO,
             CA~NR_SAFRA           AS NR_SAFRA,
             CA~ID_BUKRS           AS ID_BUKRS,
             EN~BUTXT              AS BUTXT,
             CA~ID_BRANCH          AS ID_BRANCH,
             FN~NAME               AS NAME,
             CA~ID_AGENT_FRETE     AS ID_AGENT_FRETE,
             AG~NAME1              AS DS_AGENT_FRETE,
             CA~ID_LOCAL_COLETA    AS ID_LOCAL_COLETA,
             LC~NAME1              AS DS_LOCAL_COLETA,
             CA~ID_LOCAL_DESTINO   AS ID_LOCAL_DESTINO,
             LD~NAME1              AS DS_LOCAL_DESTINO,
             CA~ID_LOCAL_DESCARGA  AS ID_LOCAL_DESCARGA,
             LA~NAME1              AS DS_LOCAL_DESCARGA,
             CA~TP_FRETE           AS TP_FRETE,
             CA~ID_PRODUTO         AS ID_PRODUTO,
             MA~MAKTX              AS DS_PRODUTO,
             CA~NM_PESO_BRUTO      AS NM_PESO_BRUTO,
             CA~NM_PESO_TARA       AS NM_PESO_TARA,
             CA~NM_PESO_SUBTOTAL   AS NM_PESO_SUBTOTAL,
             CA~NM_PESO_DESCONTOS  AS NM_PESO_DESCONTOS,
             CA~NM_PESO_LIQUIDO    AS NM_PESO_LIQUIDO,
             CA~DS_PLACA_TRATOR    AS DS_PLACA_TRATOR,
             CA~ID_PROPRIETARIO    AS ID_PROPRIETARIO,
             PP~NAME1              AS DS_PROPRIETARIO,
             CA~DS_PLACA_REBOQ_1   AS DS_PLACA_REBOQ_1,
             CA~DS_PLACA_REBOQ_2   AS DS_PLACA_REBOQ_2,
             CA~DS_PLACA_REBOQ_3   AS DS_PLACA_REBOQ_3,
             CA~ID_MOTORISTA       AS ID_MOTORISTA,
             MT~NAME1              AS DS_MOTORISTA,
             CA~NR_TICKET          AS NR_TICKET,
             CA~IN_TRANSFERENCIA   AS IN_TRANSFERENCIA,
             CA~TP_STATUS          AS TP_STATUS,
             CA~DT_ABERTURA        AS DT_ABERTURA,
             CA~HR_ABERTURA        AS HR_ABERTURA,
             CA~US_ABERTURA        AS US_ABERTURA,
             CA~DT_FECHAMENTO      AS DT_FECHAMENTO,
             CA~HR_FECHAMENTO      AS HR_FECHAMENTO,
             CA~US_FECHAMENTO      AS US_FECHAMENTO,
             CA~DT_CONFERENCIA     AS DT_CONFERENCIA,
             CA~HR_CONFERENCIA     AS HR_CONFERENCIA,
             CA~US_CONFERENCIA     AS US_CONFERENCIA,
             CA~DT_CANCELAMENTO    AS DT_CANCELAMENTO,
             CA~HR_CANCELAMENTO    AS HR_CANCELAMENTO,
             CA~US_CANCELAMENTO    AS US_CANCELAMENTO,

             CL~ID_CLASSIFICACAO      AS ID_CLASSIFICACAO,
             CL~IN_GMO                AS IN_GMO,
             CL~NR_RESULTADO_01       AS NR_RESULTADO_01,
             CL~NR_RESULTADO_02       AS NR_RESULTADO_02,
             CL~NR_RES_RR1_RR2        AS NR_RES_RR1_RR2,
             CL~IN_GMO_03             AS IN_GMO_03,
             CL~IN_SRR_ORIGEM_PARTIC  AS IN_SRR_ORIGEM_PARTIC,
             CL~ID_OUTRO_PARTIC       AS ID_OUTRO_PARTIC,
             OT~NAME1                 AS DS_OUTRO_PARTIC,
             CL~IN_SRR_DECLARADO      AS IN_SRR_DECLARADO,
             CL~IN_TESTE_SRR          AS IN_TESTE_SRR,
             CL~IN_SRR_DECLARADO_2    AS IN_SRR_DECLARADO_2,
             CL~IN_TESTE_SRR_2        AS IN_TESTE_SRR_2,
             CL~TP_TRANSGENIA         AS TP_TRANSGENIA,

             R1~NR_PERCENTUAL_COM     AS NR_PERC_UMI,
             R2~NR_PERCENTUAL_COM     AS NR_PERC_IMP,
             R3~NR_PERCENTUAL_COM     AS NR_PERC_AVA,
             R4~NR_PERCENTUAL_COM     AS NR_PERC_ARD,
             R5~NR_PERCENTUAL_COM     AS NR_PERC_QUE,
             R6~NR_PERCENTUAL_COM     AS NR_PERC_ESV,
             R7~NR_PERCENTUAL_COM     AS NR_PERC_CAR,

             R1~NR_QUANTIDADE_COM     AS NR_QTDE_UMI,
             R2~NR_QUANTIDADE_COM     AS NR_QTDE_IMP,
             R3~NR_QUANTIDADE_COM     AS NR_QTDE_AVA,
             R4~NR_QUANTIDADE_COM     AS NR_QTDE_ARD,
             R5~NR_QUANTIDADE_COM     AS NR_QTDE_QUE,
             R6~NR_QUANTIDADE_COM     AS NR_QTDE_ESV,
             R7~NR_QUANTIDADE_COM     AS NR_QTDE_CAR

        INTO TABLE @IT_TAB
        FROM ZSDT0001CG AS CA
       INNER JOIN ZSDT0001LE   AS LE ON LE~ID_LOCAL_ENTREGA EQ CA~ID_LOCAL_ENTREGA
       INNER JOIN T001         AS EN ON EN~BUKRS EQ CA~ID_BUKRS
       INNER JOIN J_1BBRANCH   AS FN ON FN~BUKRS EQ CA~ID_BUKRS AND FN~BRANCH EQ CA~ID_BRANCH
       INNER JOIN LFA1         AS LC ON LC~LIFNR EQ CA~ID_LOCAL_COLETA
       INNER JOIN LFA1         AS LD ON LD~LIFNR EQ CA~ID_LOCAL_DESTINO
       INNER JOIN KNA1         AS LA ON LA~KUNNR EQ CA~ID_LOCAL_DESCARGA
        LEFT JOIN ZSDT0001OD   AS OD ON OD~ID_ORDEM EQ CA~ID_ORDEM
        LEFT JOIN ZSDT0001CL   AS CL ON CL~ID_CARGA EQ CA~ID_CARGA AND CL~ID_CLASSIFICACAO EQ CA~ID_CLASSIFICACAO
        LEFT JOIN ZSDT0001RS   AS R1 ON R1~ID_CARGA EQ CL~ID_CARGA AND R1~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R1~TP_CARACTERISTICA EQ '01'
        LEFT JOIN ZSDT0001RS   AS R2 ON R2~ID_CARGA EQ CL~ID_CARGA AND R2~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R2~TP_CARACTERISTICA EQ '02'
        LEFT JOIN ZSDT0001RS   AS R3 ON R3~ID_CARGA EQ CL~ID_CARGA AND R3~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R3~TP_CARACTERISTICA EQ '03'
        LEFT JOIN ZSDT0001RS   AS R4 ON R4~ID_CARGA EQ CL~ID_CARGA AND R4~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R4~TP_CARACTERISTICA EQ '04'
        LEFT JOIN ZSDT0001RS   AS R5 ON R5~ID_CARGA EQ CL~ID_CARGA AND R5~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R5~TP_CARACTERISTICA EQ '05'
        LEFT JOIN ZSDT0001RS   AS R6 ON R6~ID_CARGA EQ CL~ID_CARGA AND R6~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R6~TP_CARACTERISTICA EQ '06'
        LEFT JOIN ZSDT0001RS   AS R7 ON R7~ID_CARGA EQ CL~ID_CARGA AND R7~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R7~TP_CARACTERISTICA EQ '07'
        LEFT JOIN LFA1         AS AG ON AG~LIFNR EQ CA~ID_AGENT_FRETE
        LEFT JOIN LFA1         AS PP ON PP~LIFNR EQ CA~ID_PROPRIETARIO
        LEFT JOIN LFA1         AS MT ON MT~LIFNR EQ CA~ID_MOTORISTA
        LEFT JOIN LFA1         AS OT ON OT~LIFNR EQ CL~ID_OUTRO_PARTIC
       INNER JOIN MAKT         AS MA ON MA~SPRAS EQ @SY-LANGU AND MA~MATNR EQ CA~ID_PRODUTO
       WHERE CA~TP_CARGA           IN @RG_TP_CARGA
         AND CA~ID_CARGA           IN @LC_FILTRO-IIDCARGA
         AND CA~ID_ORDEM           IN @LC_FILTRO-IIDORDEM

         AND EXISTS ( SELECT * FROM ZSDT0001OV AS OV
                       WHERE OV~ID_CARGA        EQ CA~ID_CARGA
                         AND OV~NR_ROMANEIO_SAI IN @LC_FILTRO-IRROMSAI )

         AND CA~ID_LOCAL_ENTREGA   IN @LC_FILTRO-IIDLOCAL
         AND CA~DT_MOVIMENTO       IN @LC_FILTRO-IDTMOVIM
         AND CA~NR_SAFRA           IN @LC_FILTRO-INRSAFRA
         AND CA~ID_BUKRS           IN @LC_FILTRO-IIDBUKRS
         AND CA~ID_BRANCH          IN @LC_FILTRO-IIDBRANC
         AND CA~ID_AGENT_FRETE     IN @LC_FILTRO-IIDAGENT
         AND CA~ID_LOCAL_COLETA    IN @LC_FILTRO-IIDCOLET
         AND CA~ID_LOCAL_DESTINO   IN @LC_FILTRO-IIDDESTI
         AND CA~ID_LOCAL_DESCARGA  IN @LC_FILTRO-IIDDESCA
         AND CA~ID_PRODUTO         IN @LC_FILTRO-IIDPRODU
         AND CA~DS_PLACA_TRATOR    IN @LC_FILTRO-IDSTRATO
         AND CA~DS_PLACA_REBOQ_1   IN @LC_FILTRO-IDSREBO1
         AND CA~DS_PLACA_REBOQ_2   IN @LC_FILTRO-IDSREBO2
         AND CA~DS_PLACA_REBOQ_3   IN @LC_FILTRO-IDSREBO3
         AND CA~ID_MOTORISTA       IN @LC_FILTRO-IIDMOTOR
         AND CA~NR_TICKET          IN @LC_FILTRO-INRTICKE
         AND CA~TP_STATUS          IN @LC_FILTRO-ITPSTATU
         AND CA~DT_ABERTURA        IN @LC_FILTRO-IDTABERT
         AND CA~HR_ABERTURA        IN @LC_FILTRO-IHRABERT
         AND CA~DT_FECHAMENTO      IN @LC_FILTRO-IDTFECHA
         AND CA~HR_FECHAMENTO      IN @LC_FILTRO-IHRFECHA
       ORDER BY CA~ID_CARGA.

    ELSE.

      SELECT CA~ID_CARGA           AS ID_CARGA,
             CA~ID_ORDEM           AS ID_ORDEM,
             OD~NR_ORDEM           AS NR_ORDEM,
             CA~ID_LOCAL_ENTREGA   AS ID_LOCAL_ENTREGA,
             LE~DS_LOCAL_ENTREGA   AS DS_LOCAL_ENTREGA,
             CA~DT_MOVIMENTO       AS DT_MOVIMENTO,
             CA~NR_SAFRA           AS NR_SAFRA,
             CA~ID_BUKRS           AS ID_BUKRS,
             EN~BUTXT              AS BUTXT,
             CA~ID_BRANCH          AS ID_BRANCH,
             FN~NAME               AS NAME,
             CA~ID_AGENT_FRETE     AS ID_AGENT_FRETE,
             AG~NAME1              AS DS_AGENT_FRETE,
             CA~ID_LOCAL_COLETA    AS ID_LOCAL_COLETA,
             LC~NAME1              AS DS_LOCAL_COLETA,
             CA~ID_LOCAL_DESTINO   AS ID_LOCAL_DESTINO,
             LD~NAME1              AS DS_LOCAL_DESTINO,
             CA~ID_LOCAL_DESCARGA  AS ID_LOCAL_DESCARGA,
             LA~NAME1              AS DS_LOCAL_DESCARGA,
             CA~TP_FRETE           AS TP_FRETE,
             CA~ID_PRODUTO         AS ID_PRODUTO,
             MA~MAKTX              AS DS_PRODUTO,
             CA~NM_PESO_BRUTO      AS NM_PESO_BRUTO,
             CA~NM_PESO_TARA       AS NM_PESO_TARA,
             CA~NM_PESO_SUBTOTAL   AS NM_PESO_SUBTOTAL,
             CA~NM_PESO_DESCONTOS  AS NM_PESO_DESCONTOS,
             CA~NM_PESO_LIQUIDO    AS NM_PESO_LIQUIDO,
             CA~DS_PLACA_TRATOR    AS DS_PLACA_TRATOR,
             CA~ID_PROPRIETARIO    AS ID_PROPRIETARIO,
             PP~NAME1              AS DS_PROPRIETARIO,
             CA~DS_PLACA_REBOQ_1   AS DS_PLACA_REBOQ_1,
             CA~DS_PLACA_REBOQ_2   AS DS_PLACA_REBOQ_2,
             CA~DS_PLACA_REBOQ_3   AS DS_PLACA_REBOQ_3,
             CA~ID_MOTORISTA       AS ID_MOTORISTA,
             MT~NAME1              AS DS_MOTORISTA,
             CA~NR_TICKET          AS NR_TICKET,
             CA~IN_TRANSFERENCIA   AS IN_TRANSFERENCIA,
             CA~TP_STATUS          AS TP_STATUS,
             CA~DT_ABERTURA        AS DT_ABERTURA,
             CA~HR_ABERTURA        AS HR_ABERTURA,
             CA~US_ABERTURA        AS US_ABERTURA,
             CA~DT_FECHAMENTO      AS DT_FECHAMENTO,
             CA~HR_FECHAMENTO      AS HR_FECHAMENTO,
             CA~US_FECHAMENTO      AS US_FECHAMENTO,
             CA~DT_CONFERENCIA     AS DT_CONFERENCIA,
             CA~HR_CONFERENCIA     AS HR_CONFERENCIA,
             CA~US_CONFERENCIA     AS US_CONFERENCIA,
             CA~DT_CANCELAMENTO    AS DT_CANCELAMENTO,
             CA~HR_CANCELAMENTO    AS HR_CANCELAMENTO,
             CA~US_CANCELAMENTO    AS US_CANCELAMENTO,

             CL~ID_CLASSIFICACAO      AS ID_CLASSIFICACAO,
             CL~IN_GMO                AS IN_GMO,
             CL~NR_RESULTADO_01       AS NR_RESULTADO_01,
             CL~NR_RESULTADO_02       AS NR_RESULTADO_02,
             CL~NR_RES_RR1_RR2        AS NR_RES_RR1_RR2,
             CL~IN_GMO_03             AS IN_GMO_03,
             CL~IN_SRR_ORIGEM_PARTIC  AS IN_SRR_ORIGEM_PARTIC,
             CL~ID_OUTRO_PARTIC       AS ID_OUTRO_PARTIC,
             OT~NAME1                 AS DS_OUTRO_PARTIC,
             CL~IN_SRR_DECLARADO      AS IN_SRR_DECLARADO,
             CL~IN_TESTE_SRR          AS IN_TESTE_SRR,
             CL~IN_SRR_DECLARADO_2    AS IN_SRR_DECLARADO_2,
             CL~IN_TESTE_SRR_2        AS IN_TESTE_SRR_2,
             CL~TP_TRANSGENIA         AS TP_TRANSGENIA,

             R1~NR_PERCENTUAL_COM     AS NR_PERC_UMI,
             R2~NR_PERCENTUAL_COM     AS NR_PERC_IMP,
             R3~NR_PERCENTUAL_COM     AS NR_PERC_AVA,
             R4~NR_PERCENTUAL_COM     AS NR_PERC_ARD,
             R5~NR_PERCENTUAL_COM     AS NR_PERC_QUE,
             R6~NR_PERCENTUAL_COM     AS NR_PERC_ESV,
             R7~NR_PERCENTUAL_COM     AS NR_PERC_CAR,

             R1~NR_QUANTIDADE_COM     AS NR_QTDE_UMI,
             R2~NR_QUANTIDADE_COM     AS NR_QTDE_IMP,
             R3~NR_QUANTIDADE_COM     AS NR_QTDE_AVA,
             R4~NR_QUANTIDADE_COM     AS NR_QTDE_ARD,
             R5~NR_QUANTIDADE_COM     AS NR_QTDE_QUE,
             R6~NR_QUANTIDADE_COM     AS NR_QTDE_ESV,
             R7~NR_QUANTIDADE_COM     AS NR_QTDE_CAR

        INTO TABLE @IT_TAB
        FROM ZSDT0001CG AS CA
       INNER JOIN ZSDT0001LE   AS LE ON LE~ID_LOCAL_ENTREGA EQ CA~ID_LOCAL_ENTREGA
       INNER JOIN T001         AS EN ON EN~BUKRS EQ CA~ID_BUKRS
       INNER JOIN J_1BBRANCH   AS FN ON FN~BUKRS EQ CA~ID_BUKRS AND FN~BRANCH EQ CA~ID_BRANCH
       INNER JOIN LFA1         AS LC ON LC~LIFNR EQ CA~ID_LOCAL_COLETA
       INNER JOIN LFA1         AS LD ON LD~LIFNR EQ CA~ID_LOCAL_DESTINO
       INNER JOIN KNA1         AS LA ON LA~KUNNR EQ CA~ID_LOCAL_DESCARGA
        LEFT JOIN ZSDT0001OD   AS OD ON OD~ID_ORDEM EQ CA~ID_ORDEM
        LEFT JOIN ZSDT0001CL   AS CL ON CL~ID_CARGA EQ CA~ID_CARGA AND CL~ID_CLASSIFICACAO EQ CA~ID_CLASSIFICACAO
        LEFT JOIN ZSDT0001RS   AS R1 ON R1~ID_CARGA EQ CL~ID_CARGA AND R1~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R1~TP_CARACTERISTICA EQ '01'
        LEFT JOIN ZSDT0001RS   AS R2 ON R2~ID_CARGA EQ CL~ID_CARGA AND R2~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R2~TP_CARACTERISTICA EQ '02'
        LEFT JOIN ZSDT0001RS   AS R3 ON R3~ID_CARGA EQ CL~ID_CARGA AND R3~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R3~TP_CARACTERISTICA EQ '03'
        LEFT JOIN ZSDT0001RS   AS R4 ON R4~ID_CARGA EQ CL~ID_CARGA AND R4~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R4~TP_CARACTERISTICA EQ '04'
        LEFT JOIN ZSDT0001RS   AS R5 ON R5~ID_CARGA EQ CL~ID_CARGA AND R5~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R5~TP_CARACTERISTICA EQ '05'
        LEFT JOIN ZSDT0001RS   AS R6 ON R6~ID_CARGA EQ CL~ID_CARGA AND R6~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R6~TP_CARACTERISTICA EQ '06'
        LEFT JOIN ZSDT0001RS   AS R7 ON R7~ID_CARGA EQ CL~ID_CARGA AND R7~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R7~TP_CARACTERISTICA EQ '07'
        LEFT JOIN LFA1         AS AG ON AG~LIFNR EQ CA~ID_AGENT_FRETE
        LEFT JOIN LFA1         AS PP ON PP~LIFNR EQ CA~ID_PROPRIETARIO
        LEFT JOIN LFA1         AS MT ON MT~LIFNR EQ CA~ID_MOTORISTA
        LEFT JOIN LFA1         AS OT ON OT~LIFNR EQ CL~ID_OUTRO_PARTIC
       INNER JOIN MAKT         AS MA ON MA~SPRAS EQ @SY-LANGU AND MA~MATNR EQ CA~ID_PRODUTO
       WHERE CA~TP_CARGA           IN @RG_TP_CARGA
         AND CA~ID_CARGA           IN @LC_FILTRO-IIDCARGA
         AND CA~ID_ORDEM           IN @LC_FILTRO-IIDORDEM
         AND CA~ID_LOCAL_ENTREGA   IN @LC_FILTRO-IIDLOCAL
         AND CA~DT_MOVIMENTO       IN @LC_FILTRO-IDTMOVIM
         AND CA~NR_SAFRA           IN @LC_FILTRO-INRSAFRA
         AND CA~ID_BUKRS           IN @LC_FILTRO-IIDBUKRS
         AND CA~ID_BRANCH          IN @LC_FILTRO-IIDBRANC
         AND CA~ID_AGENT_FRETE     IN @LC_FILTRO-IIDAGENT
         AND CA~ID_LOCAL_COLETA    IN @LC_FILTRO-IIDCOLET
         AND CA~ID_LOCAL_DESTINO   IN @LC_FILTRO-IIDDESTI
         AND CA~ID_LOCAL_DESCARGA  IN @LC_FILTRO-IIDDESCA
         AND CA~ID_PRODUTO         IN @LC_FILTRO-IIDPRODU
         AND CA~DS_PLACA_TRATOR    IN @LC_FILTRO-IDSTRATO
         AND CA~DS_PLACA_REBOQ_1   IN @LC_FILTRO-IDSREBO1
         AND CA~DS_PLACA_REBOQ_2   IN @LC_FILTRO-IDSREBO2
         AND CA~DS_PLACA_REBOQ_3   IN @LC_FILTRO-IDSREBO3
         AND CA~ID_MOTORISTA       IN @LC_FILTRO-IIDMOTOR
         AND CA~NR_TICKET          IN @LC_FILTRO-INRTICKE
         AND CA~TP_STATUS          IN @LC_FILTRO-ITPSTATU
         AND CA~DT_ABERTURA        IN @LC_FILTRO-IDTABERT
         AND CA~HR_ABERTURA        IN @LC_FILTRO-IHRABERT
         AND CA~DT_FECHAMENTO      IN @LC_FILTRO-IDTFECHA
         AND CA~HR_FECHAMENTO      IN @LC_FILTRO-IHRFECHA
       ORDER BY CA~ID_CARGA.

    ENDIF.

    IF IT_TAB[] IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(IT_ZSDT0001RS_03)
        FROM ZSDT0001RS_03
         FOR ALL ENTRIES IN @IT_TAB
       WHERE ID_CARGA EQ @IT_TAB-ID_CARGA.
    ENDIF.

    LOOP AT IT_TAB INTO DATA(WA_TAB).
      CLEAR: WA_RETORNO.
      WA_RETORNO-ID_CARGA          = WA_TAB-ID_CARGA.
      WA_RETORNO-ID_ORDEM          = WA_TAB-ID_ORDEM.
      WA_RETORNO-NR_ORDEM          = WA_TAB-NR_ORDEM.
      WA_RETORNO-ID_LOCAL_ENTREGA  = WA_TAB-ID_LOCAL_ENTREGA.
      WA_RETORNO-DS_LOCAL_ENTREGA  = WA_TAB-DS_LOCAL_ENTREGA.
      WA_RETORNO-DT_MOVIMENTO      = WA_TAB-DT_MOVIMENTO.
      WA_RETORNO-NR_SAFRA          = WA_TAB-NR_SAFRA.
      WA_RETORNO-ID_BUKRS          = WA_TAB-ID_BUKRS.
      WA_RETORNO-BUTXT             = WA_TAB-BUTXT.
      WA_RETORNO-ID_BRANCH         = WA_TAB-ID_BRANCH.
      WA_RETORNO-NAME              = WA_TAB-NAME.
      WA_RETORNO-ID_AGENT_FRETE    = WA_TAB-ID_AGENT_FRETE.
      WA_RETORNO-DS_AGENT_FRETE    = WA_TAB-DS_AGENT_FRETE.
      WA_RETORNO-ID_LOCAL_COLETA   = WA_TAB-ID_LOCAL_COLETA.
      WA_RETORNO-DS_LOCAL_COLETA   = WA_TAB-DS_LOCAL_COLETA.
      WA_RETORNO-ID_LOCAL_DESTINO  = WA_TAB-ID_LOCAL_DESTINO.
      WA_RETORNO-DS_LOCAL_DESTINO  = WA_TAB-DS_LOCAL_DESTINO.
      WA_RETORNO-ID_LOCAL_DESCARGA = WA_TAB-ID_LOCAL_DESCARGA.
      WA_RETORNO-DS_LOCAL_DESCARGA = WA_TAB-DS_LOCAL_DESCARGA.
      WA_RETORNO-TP_FRETE          = WA_TAB-TP_FRETE.
      WA_RETORNO-ID_PRODUTO        = WA_TAB-ID_PRODUTO.
      WA_RETORNO-DS_PRODUTO        = WA_TAB-DS_PRODUTO.
      WA_RETORNO-NM_PESO_BRUTO     = WA_TAB-NM_PESO_BRUTO.
      WA_RETORNO-NM_PESO_TARA      = WA_TAB-NM_PESO_TARA.
      WA_RETORNO-NM_PESO_SUBTOTAL  = WA_TAB-NM_PESO_SUBTOTAL.
      WA_RETORNO-NM_PESO_DESCONTOS = WA_TAB-NM_PESO_DESCONTOS.
      WA_RETORNO-NM_PESO_LIQUIDO   = WA_TAB-NM_PESO_LIQUIDO.
      WA_RETORNO-DS_PLACA_TRATOR   = WA_TAB-DS_PLACA_TRATOR.
      WA_RETORNO-ID_PROPRIETARIO   = WA_TAB-ID_PROPRIETARIO.
      WA_RETORNO-DS_PROPRIETARIO   = WA_TAB-DS_PROPRIETARIO.
      WA_RETORNO-DS_PLACA_REBOQ_1  = WA_TAB-DS_PLACA_REBOQ_1.
      WA_RETORNO-DS_PLACA_REBOQ_2  = WA_TAB-DS_PLACA_REBOQ_2.
      WA_RETORNO-DS_PLACA_REBOQ_3  = WA_TAB-DS_PLACA_REBOQ_3.
      WA_RETORNO-ID_MOTORISTA      = WA_TAB-ID_MOTORISTA.
      WA_RETORNO-DS_MOTORISTA      = WA_TAB-DS_MOTORISTA.
      WA_RETORNO-NR_TICKET         = WA_TAB-NR_TICKET.
      WA_RETORNO-IN_TRANSFERENCIA  = WA_TAB-IN_TRANSFERENCIA.
      WA_RETORNO-TP_STATUS         = WA_TAB-TP_STATUS.
      WA_RETORNO-DT_ABERTURA       = WA_TAB-DT_ABERTURA.
      WA_RETORNO-HR_ABERTURA       = WA_TAB-HR_ABERTURA.
      WA_RETORNO-US_ABERTURA       = WA_TAB-US_ABERTURA.
      WA_RETORNO-DT_FECHAMENTO     = WA_TAB-DT_FECHAMENTO.
      WA_RETORNO-HR_FECHAMENTO     = WA_TAB-HR_FECHAMENTO.
      WA_RETORNO-US_FECHAMENTO     = WA_TAB-US_FECHAMENTO.
      WA_RETORNO-DT_CONFERENCIA    = WA_TAB-DT_CONFERENCIA.
      WA_RETORNO-HR_CONFERENCIA    = WA_TAB-HR_CONFERENCIA.
      WA_RETORNO-US_CONFERENCIA    = WA_TAB-US_CONFERENCIA.
      WA_RETORNO-DT_CANCELAMENTO   = WA_TAB-DT_CANCELAMENTO.
      WA_RETORNO-HR_CANCELAMENTO   = WA_TAB-HR_CANCELAMENTO.
      WA_RETORNO-US_CANCELAMENTO   = WA_TAB-US_CANCELAMENTO.

      WA_RETORNO-ID_CLASSIFICACAO       = WA_TAB-ID_CLASSIFICACAO.
      WA_RETORNO-IN_GMO                 = WA_TAB-IN_GMO.
      WA_RETORNO-NR_RESULTADO_01        = WA_TAB-NR_RESULTADO_01.
      WA_RETORNO-NR_RESULTADO_02        = WA_TAB-NR_RESULTADO_02.
      WA_RETORNO-NR_RES_RR1_RR2         = WA_TAB-NR_RES_RR1_RR2.
      WA_RETORNO-IN_GMO_03              = WA_TAB-IN_GMO_03.
      WA_RETORNO-IN_SRR_ORIGEM_PARTIC   = WA_TAB-IN_SRR_ORIGEM_PARTIC.
      WA_RETORNO-ID_OUTRO_PARTIC        = WA_TAB-ID_OUTRO_PARTIC.
      WA_RETORNO-DS_OUTRO_PARTIC        = WA_TAB-DS_OUTRO_PARTIC.
      WA_RETORNO-IN_SRR_DECLARADO       = WA_TAB-IN_SRR_DECLARADO.
      WA_RETORNO-IN_TESTE_SRR           = WA_TAB-IN_TESTE_SRR.
      WA_RETORNO-IN_SRR_DECLARADO_2     = WA_TAB-IN_SRR_DECLARADO_2.
      WA_RETORNO-IN_TESTE_SRR_2         = WA_TAB-IN_TESTE_SRR_2.
      WA_RETORNO-TP_TRANSGENIA          = WA_TAB-TP_TRANSGENIA.

      WA_RETORNO-NR_PERC_UMI   = WA_TAB-NR_PERC_UMI.
      WA_RETORNO-NR_PERC_IMP   = WA_TAB-NR_PERC_IMP.
      WA_RETORNO-NR_PERC_AVA   = WA_TAB-NR_PERC_AVA.
      WA_RETORNO-NR_PERC_ARD   = WA_TAB-NR_PERC_ARD.
      WA_RETORNO-NR_PERC_QUE   = WA_TAB-NR_PERC_QUE.
      WA_RETORNO-NR_PERC_ESV   = WA_TAB-NR_PERC_ESV.
      WA_RETORNO-NR_PERC_CAR   = WA_TAB-NR_PERC_CAR.
      WA_RETORNO-NR_QTDE_UMI   = WA_TAB-NR_QTDE_UMI.
      WA_RETORNO-NR_QTDE_IMP   = WA_TAB-NR_QTDE_IMP.
      WA_RETORNO-NR_QTDE_AVA   = WA_TAB-NR_QTDE_AVA.
      WA_RETORNO-NR_QTDE_ARD   = WA_TAB-NR_QTDE_ARD.
      WA_RETORNO-NR_QTDE_QUE   = WA_TAB-NR_QTDE_QUE.
      WA_RETORNO-NR_QTDE_ESV   = WA_TAB-NR_QTDE_ESV.
      WA_RETORNO-NR_QTDE_CAR   = WA_TAB-NR_QTDE_CAR.

      LOOP AT IT_ZSDT0001RS_03 INTO DATA(WA_ZSDT0001RS_03) WHERE ID_CARGA EQ WA_TAB-ID_CARGA AND ID_CLASSIFICACAO EQ WA_TAB-ID_CLASSIFICACAO.
        CASE WA_ZSDT0001RS_03-TP_SUB_CARAC_AVARIADO.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_ARQ.
            WA_RETORNO-NR_PERC_AVA_ARQ = WA_ZSDT0001RS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_QUE.
            WA_RETORNO-NR_PERC_AVA_QUE = WA_ZSDT0001RS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_MOF.
            WA_RETORNO-NR_PERC_AVA_MOF = WA_ZSDT0001RS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_PIC.
            WA_RETORNO-NR_PERC_AVA_PIC = WA_ZSDT0001RS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_FER.
            WA_RETORNO-NR_PERC_AVA_FER = WA_ZSDT0001RS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_GER.
            WA_RETORNO-NR_PERC_AVA_GER = WA_ZSDT0001RS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_ARD.
            WA_RETORNO-NR_PERC_AVA_ARD = WA_ZSDT0001RS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_GES.
            WA_RETORNO-NR_PERC_AVA_GES = WA_ZSDT0001RS_03-NR_PERCENTUAL_COM.
        ENDCASE.
      ENDLOOP.

      APPEND WA_RETORNO TO LC_RETORNO.
    ENDLOOP.

    CHECK LC_RETORNO[] IS NOT INITIAL.

    E_REGISTROS = LC_RETORNO.
    E_PESQUISOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_CARGA~PESQUISAR_SOLICITACAO_MANUT.

    R_CARGA = ME.

    DATA: LC_FILTRO  TYPE ZDE_FILTRO_ZSDT0001ACG,
          WA_RETORNO TYPE ZDE_ZSDT0001ACG_ALV,
          LC_RETORNO TYPE ZDE_ZSDT0001ACG_ALV_T.

    MOVE I_FILTROS TO LC_FILTRO.

    DESCRIBE TABLE LC_FILTRO-INRSAFRA LINES DATA(LC_SAFRA).
    DESCRIBE TABLE LC_FILTRO-IIDBUKRS LINES DATA(LC_BUKRS).
    DESCRIBE TABLE LC_FILTRO-IIDBRANC LINES DATA(LC_BRANCH).

    IF LC_SAFRA IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_OBG_INF_SAFRA_PSQ-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_OBG_INF_SAFRA_PSQ-MSGNO )
          MSGTY  = 'E'
          MSGID  = ZCX_CARGA=>ZCX_OBG_INF_SAFRA_PSQ-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_OBG_INF_SAFRA_PSQ-MSGNO.
    ENDIF.

    IF LC_BUKRS IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_OBG_INF_EMPRESA_PSQ-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_OBG_INF_EMPRESA_PSQ-MSGNO )
          MSGTY  = 'E'
          MSGID  = ZCX_CARGA=>ZCX_OBG_INF_EMPRESA_PSQ-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_OBG_INF_EMPRESA_PSQ-MSGNO.
    ENDIF.

    IF LC_BRANCH IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_OBG_INF_FILIAL_PSQ-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_OBG_INF_FILIAL_PSQ-MSGNO )
          MSGTY  = 'E'
          MSGID  = ZCX_CARGA=>ZCX_OBG_INF_FILIAL_PSQ-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_OBG_INF_FILIAL_PSQ-MSGNO.
    ENDIF.

    CLEAR: E_PESQUISOU, E_REGISTROS.

    SELECT SA~ID_SOLICITACAO      AS ID_SOLICITACAO,
           SA~DT_SOLICITACAO      AS DT_SOLICITACAO,
           SA~HR_SOLICITACAO      AS HR_SOLICITACAO,
           SA~US_SOLICITACAO      AS US_SOLICITACAO,
           SA~TP_STATUS           AS TP_SOLICITACAO_STATUS,
           SA~CK_ACEITE_FILIAL    AS CK_ACEITE_FILIAL,
           SA~CK_ACEITE_FISCAL    AS CK_ACEITE_FISCAL,
           SA~CK_ACEITE_COMERCIAL AS CK_ACEITE_COMERCIAL,
           SA~RS_ACEITE_FILIAL    AS RS_ACEITE_FILIAL,
           SA~RS_ACEITE_FISCAL    AS RS_ACEITE_FISCAL,
           SA~RS_ACEITE_COMERCIAL AS RS_ACEITE_COMERCIAL,
           SA~DS_ACEITE_FILIAL    AS DS_ACEITE_FILIAL,
           SA~DS_ACEITE_FISCAL    AS DS_ACEITE_FISCAL,
           SA~DS_ACEITE_COMERCIAL AS DS_ACEITE_COMERCIAL,
            "SE_STATUS
            "SE_CODE
            "SE_DETAIL
            "SE_RECORDKEY
           SA~SE_RECORDID    AS SE_RECORDID,

           CA~ID_CARGA_ORIGEM    AS ID_CARGA,
           CA~ID_ORDEM           AS ID_ORDEM,
           OD~NR_ORDEM           AS NR_ORDEM,
           "CA~ID_ENTRADA         AS ID_ENTRADA,
           "TE~DS_ENTRADA         AS DS_ENTRADA,
           CA~ID_LOCAL_ENTREGA   AS ID_LOCAL_ENTREGA,
           LE~DS_LOCAL_ENTREGA   AS DS_LOCAL_ENTREGA,
           CA~DT_MOVIMENTO       AS DT_MOVIMENTO,
           CA~NR_SAFRA           AS NR_SAFRA,
           CA~ID_BUKRS           AS ID_BUKRS,
           EN~BUTXT              AS BUTXT,
           CA~ID_BRANCH          AS ID_BRANCH,
           FN~NAME               AS NAME,
           CA~ID_AGENT_FRETE     AS ID_AGENT_FRETE,
           AG~NAME1              AS DS_AGENT_FRETE,
           CA~ID_LOCAL_COLETA    AS ID_LOCAL_COLETA,
           LC~NAME1              AS DS_LOCAL_COLETA,
           CA~ID_LOCAL_DESTINO   AS ID_LOCAL_DESTINO,
           LD~NAME1              AS DS_LOCAL_DESTINO,
           CA~ID_LOCAL_DESCARGA  AS ID_LOCAL_DESCARGA,
           LA~NAME1              AS DS_LOCAL_DESCARGA,
           CA~TP_FRETE           AS TP_FRETE,
           CA~ID_PRODUTO         AS ID_PRODUTO,
           MA~MAKTX              AS DS_PRODUTO,
           CA~NM_PESO_BRUTO      AS NM_PESO_BRUTO,
           CA~NM_PESO_TARA       AS NM_PESO_TARA,
           CA~NM_PESO_SUBTOTAL   AS NM_PESO_SUBTOTAL,
           CA~NM_PESO_DESCONTOS  AS NM_PESO_DESCONTOS,
           CA~NM_PESO_LIQUIDO    AS NM_PESO_LIQUIDO,
           CA~DS_PLACA_TRATOR    AS DS_PLACA_TRATOR,
           CA~ID_PROPRIETARIO    AS ID_PROPRIETARIO,
           PP~NAME1              AS DS_PROPRIETARIO,
           CA~DS_PLACA_REBOQ_1   AS DS_PLACA_REBOQ_1,
           CA~DS_PLACA_REBOQ_2   AS DS_PLACA_REBOQ_2,
           CA~DS_PLACA_REBOQ_3   AS DS_PLACA_REBOQ_3,
           CA~ID_MOTORISTA       AS ID_MOTORISTA,
           MT~NAME1              AS DS_MOTORISTA,
           CA~NR_TICKET          AS NR_TICKET,
           CA~IN_TRANSFERENCIA   AS IN_TRANSFERENCIA,
           CA~TP_STATUS          AS TP_STATUS,
           CA~DT_ABERTURA        AS DT_ABERTURA,
           CA~HR_ABERTURA        AS HR_ABERTURA,
           CA~US_ABERTURA        AS US_ABERTURA,
           CA~DT_FECHAMENTO      AS DT_FECHAMENTO,
           CA~HR_FECHAMENTO      AS HR_FECHAMENTO,
           CA~US_FECHAMENTO      AS US_FECHAMENTO,
           CA~DT_CONFERENCIA     AS DT_CONFERENCIA,
           CA~HR_CONFERENCIA     AS HR_CONFERENCIA,
           CA~US_CONFERENCIA     AS US_CONFERENCIA,
           CA~DT_CANCELAMENTO    AS DT_CANCELAMENTO,
           CA~HR_CANCELAMENTO    AS HR_CANCELAMENTO,
           CA~US_CANCELAMENTO    AS US_CANCELAMENTO,

           CL~ID_CLASSIFICACAO      AS ID_CLASSIFICACAO,
           CL~IN_GMO                AS IN_GMO,
           CL~NR_RESULTADO_01       AS NR_RESULTADO_01,
           CL~NR_RESULTADO_02       AS NR_RESULTADO_02,
           CL~NR_RES_RR1_RR2        AS NR_RES_RR1_RR2,
           CL~IN_GMO_03             AS IN_GMO_03,
           CL~IN_SRR_ORIGEM_PARTIC  AS IN_SRR_ORIGEM_PARTIC,
           CL~ID_OUTRO_PARTIC       AS ID_OUTRO_PARTIC,
           OT~NAME1                 AS DS_OUTRO_PARTIC,
           CL~IN_SRR_DECLARADO      AS IN_SRR_DECLARADO,
           CL~IN_TESTE_SRR          AS IN_TESTE_SRR,
           CL~IN_SRR_DECLARADO_2    AS IN_SRR_DECLARADO_2,
           CL~IN_TESTE_SRR_2        AS IN_TESTE_SRR_2,
           CL~TP_TRANSGENIA         AS TP_TRANSGENIA,

           R1~NR_PERCENTUAL_COM     AS NR_PERC_UMI,
           R2~NR_PERCENTUAL_COM     AS NR_PERC_IMP,
           R3~NR_PERCENTUAL_COM     AS NR_PERC_AVA,
           R4~NR_PERCENTUAL_COM     AS NR_PERC_ARD,
           R5~NR_PERCENTUAL_COM     AS NR_PERC_QUE,
           R6~NR_PERCENTUAL_COM     AS NR_PERC_ESV,
           R7~NR_PERCENTUAL_COM     AS NR_PERC_CAR,

           R1~NR_QUANTIDADE_COM     AS NR_QTDE_UMI,
           R2~NR_QUANTIDADE_COM     AS NR_QTDE_IMP,
           R3~NR_QUANTIDADE_COM     AS NR_QTDE_AVA,
           R4~NR_QUANTIDADE_COM     AS NR_QTDE_ARD,
           R5~NR_QUANTIDADE_COM     AS NR_QTDE_QUE,
           R6~NR_QUANTIDADE_COM     AS NR_QTDE_ESV,
           R7~NR_QUANTIDADE_COM     AS NR_QTDE_CAR

      INTO TABLE @DATA(IT_TAB)
      FROM ZSDT0001ACB        AS SA
     INNER JOIN ZSDT0001ACG   AS CA ON CA~ID_SOLICITACAO   EQ SA~ID_SOLICITACAO
     INNER JOIN ZSDT0001LE    AS LE ON LE~ID_LOCAL_ENTREGA EQ CA~ID_LOCAL_ENTREGA
     INNER JOIN T001          AS EN ON EN~BUKRS EQ CA~ID_BUKRS
     INNER JOIN J_1BBRANCH    AS FN ON FN~BUKRS EQ CA~ID_BUKRS AND FN~BRANCH EQ CA~ID_BRANCH
     INNER JOIN LFA1          AS LC ON LC~LIFNR EQ CA~ID_LOCAL_COLETA
     INNER JOIN LFA1          AS LD ON LD~LIFNR EQ CA~ID_LOCAL_DESTINO
     INNER JOIN KNA1          AS LA ON LA~KUNNR EQ CA~ID_LOCAL_DESCARGA
      LEFT JOIN ZSDT0001OD    AS OD ON OD~ID_ORDEM EQ CA~ID_ORDEM
      LEFT JOIN ZSDT0001ACL   AS CL ON CL~ID_SOLICITACAO EQ SA~ID_SOLICITACAO AND CL~ID_CARGA_ORIGEM EQ CA~ID_CARGA_ORIGEM AND CL~ID_CLASSIFICACAO EQ CA~ID_CLASSIFICACAO
      LEFT JOIN ZSDT0001ARS   AS R1 ON R1~ID_SOLICITACAO EQ SA~ID_SOLICITACAO AND R1~ID_CARGA_ORIGEM EQ CL~ID_CARGA_ORIGEM AND R1~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R1~TP_CARACTERISTICA EQ '01'
      LEFT JOIN ZSDT0001ARS   AS R2 ON R2~ID_SOLICITACAO EQ SA~ID_SOLICITACAO AND R2~ID_CARGA_ORIGEM EQ CL~ID_CARGA_ORIGEM AND R2~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R2~TP_CARACTERISTICA EQ '02'
      LEFT JOIN ZSDT0001ARS   AS R3 ON R3~ID_SOLICITACAO EQ SA~ID_SOLICITACAO AND R3~ID_CARGA_ORIGEM EQ CL~ID_CARGA_ORIGEM AND R3~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R3~TP_CARACTERISTICA EQ '03'
      LEFT JOIN ZSDT0001ARS   AS R4 ON R4~ID_SOLICITACAO EQ SA~ID_SOLICITACAO AND R4~ID_CARGA_ORIGEM EQ CL~ID_CARGA_ORIGEM AND R4~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R4~TP_CARACTERISTICA EQ '04'
      LEFT JOIN ZSDT0001ARS   AS R5 ON R5~ID_SOLICITACAO EQ SA~ID_SOLICITACAO AND R5~ID_CARGA_ORIGEM EQ CL~ID_CARGA_ORIGEM AND R5~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R5~TP_CARACTERISTICA EQ '05'
      LEFT JOIN ZSDT0001ARS   AS R6 ON R6~ID_SOLICITACAO EQ SA~ID_SOLICITACAO AND R6~ID_CARGA_ORIGEM EQ CL~ID_CARGA_ORIGEM AND R6~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R6~TP_CARACTERISTICA EQ '06'
      LEFT JOIN ZSDT0001ARS   AS R7 ON R7~ID_SOLICITACAO EQ SA~ID_SOLICITACAO AND R7~ID_CARGA_ORIGEM EQ CL~ID_CARGA_ORIGEM AND R7~ID_CLASSIFICACAO EQ CL~ID_CLASSIFICACAO AND R7~TP_CARACTERISTICA EQ '07'
      LEFT JOIN LFA1         AS AG ON AG~LIFNR EQ CA~ID_AGENT_FRETE
      LEFT JOIN LFA1         AS PP ON PP~LIFNR EQ CA~ID_PROPRIETARIO
      LEFT JOIN LFA1         AS MT ON MT~LIFNR EQ CA~ID_MOTORISTA
      LEFT JOIN LFA1         AS OT ON OT~LIFNR EQ CL~ID_OUTRO_PARTIC
     INNER JOIN MAKT         AS MA ON MA~SPRAS EQ @SY-LANGU AND MA~MATNR EQ CA~ID_PRODUTO
     WHERE SA~ID_SOLICITACAO IN @LC_FILTRO-IIDSOLIC
       AND SA~DT_SOLICITACAO IN @LC_FILTRO-IDTSOLIC
       AND SA~US_SOLICITACAO IN @LC_FILTRO-IUSSOLIC
       AND CA~ID_CARGA_ORIGEM    IN @LC_FILTRO-IIDCARGA
       AND EXISTS ( SELECT * FROM ZSDT0001ANT AS NT
                     WHERE NT~ID_CARGA_ORIGEM EQ CA~ID_CARGA_ORIGEM
                       AND NT~ID_SOLICITACAO  EQ CA~ID_SOLICITACAO
                       AND NT~ID_ENTRADA      IN @LC_FILTRO-IIDENTRA )
       AND CA~ID_LOCAL_ENTREGA   IN @LC_FILTRO-IIDLOCAL
       AND CA~DT_MOVIMENTO       IN @LC_FILTRO-IDTMOVIM
       AND CA~NR_SAFRA           IN @LC_FILTRO-INRSAFRA
       AND CA~ID_BUKRS           IN @LC_FILTRO-IIDBUKRS
       AND CA~ID_BRANCH          IN @LC_FILTRO-IIDBRANC
       AND CA~ID_AGENT_FRETE     IN @LC_FILTRO-IIDAGENT
       AND CA~ID_LOCAL_COLETA    IN @LC_FILTRO-IIDCOLET
       AND CA~ID_LOCAL_DESTINO   IN @LC_FILTRO-IIDDESTI
       AND CA~ID_LOCAL_DESCARGA  IN @LC_FILTRO-IIDDESCA
       AND CA~ID_PRODUTO         IN @LC_FILTRO-IIDPRODU
       AND CA~DS_PLACA_TRATOR    IN @LC_FILTRO-IDSTRATO
       AND CA~ID_MOTORISTA       IN @LC_FILTRO-IIDMOTOR
       AND CA~NR_TICKET          IN @LC_FILTRO-INRTICKE
       AND CA~DT_FECHAMENTO      IN @LC_FILTRO-IDTFECHA
       AND CA~HR_FECHAMENTO      IN @LC_FILTRO-IHRFECHA
     ORDER BY SA~ID_SOLICITACAO.

    IF IT_TAB[] IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(IT_ZSDT0001ARS_03)
        FROM ZSDT0001ARS_03
         FOR ALL ENTRIES IN @IT_TAB
       WHERE ID_SOLICITACAO  EQ @IT_TAB-ID_SOLICITACAO
         AND ID_CARGA_ORIGEM EQ @IT_TAB-ID_CARGA.
    ENDIF.

    LOOP AT IT_TAB INTO DATA(WA_TAB).
      CLEAR: WA_RETORNO.
      WA_RETORNO-ID_SOLICITACAO        = WA_TAB-ID_SOLICITACAO.
      WA_RETORNO-DT_SOLICITACAO        = WA_TAB-DT_SOLICITACAO.
      WA_RETORNO-HR_SOLICITACAO        = WA_TAB-HR_SOLICITACAO.
      WA_RETORNO-US_SOLICITACAO        = WA_TAB-US_SOLICITACAO.
      WA_RETORNO-TP_SOLICITACAO_STATUS = WA_TAB-TP_SOLICITACAO_STATUS.
      WA_RETORNO-SE_RECORDID           = WA_TAB-SE_RECORDID.
      WA_RETORNO-CK_ACEITE_FILIAL      = WA_TAB-CK_ACEITE_FILIAL.
      WA_RETORNO-CK_ACEITE_FISCAL      = WA_TAB-CK_ACEITE_FISCAL.
      WA_RETORNO-CK_ACEITE_COMERCIAL   = WA_TAB-CK_ACEITE_COMERCIAL.
      WA_RETORNO-RS_ACEITE_FILIAL      = WA_TAB-RS_ACEITE_FILIAL.
      WA_RETORNO-RS_ACEITE_FISCAL      = WA_TAB-RS_ACEITE_FISCAL.
      WA_RETORNO-RS_ACEITE_COMERCIAL   = WA_TAB-RS_ACEITE_COMERCIAL.
      WA_RETORNO-DS_ACEITE_FILIAL      = WA_TAB-DS_ACEITE_FILIAL.
      WA_RETORNO-DS_ACEITE_FISCAL      = WA_TAB-DS_ACEITE_FISCAL.
      WA_RETORNO-DS_ACEITE_COMERCIAL   = WA_TAB-DS_ACEITE_COMERCIAL.

      WA_RETORNO-ID_CARGA          = WA_TAB-ID_CARGA.
      WA_RETORNO-ID_ORDEM          = WA_TAB-ID_ORDEM.
      WA_RETORNO-NR_ORDEM          = WA_TAB-NR_ORDEM.
      WA_RETORNO-ID_LOCAL_ENTREGA  = WA_TAB-ID_LOCAL_ENTREGA.
      WA_RETORNO-DS_LOCAL_ENTREGA  = WA_TAB-DS_LOCAL_ENTREGA.
      WA_RETORNO-DT_MOVIMENTO      = WA_TAB-DT_MOVIMENTO.
      WA_RETORNO-NR_SAFRA          = WA_TAB-NR_SAFRA.
      WA_RETORNO-ID_BUKRS          = WA_TAB-ID_BUKRS.
      WA_RETORNO-BUTXT             = WA_TAB-BUTXT.
      WA_RETORNO-ID_BRANCH         = WA_TAB-ID_BRANCH.
      WA_RETORNO-NAME              = WA_TAB-NAME.
      WA_RETORNO-ID_AGENT_FRETE    = WA_TAB-ID_AGENT_FRETE.
      WA_RETORNO-DS_AGENT_FRETE    = WA_TAB-DS_AGENT_FRETE.
      WA_RETORNO-ID_LOCAL_COLETA   = WA_TAB-ID_LOCAL_COLETA.
      WA_RETORNO-DS_LOCAL_COLETA   = WA_TAB-DS_LOCAL_COLETA.
      WA_RETORNO-ID_LOCAL_DESTINO  = WA_TAB-ID_LOCAL_DESTINO.
      WA_RETORNO-DS_LOCAL_DESTINO  = WA_TAB-DS_LOCAL_DESTINO.
      WA_RETORNO-ID_LOCAL_DESCARGA = WA_TAB-ID_LOCAL_DESCARGA.
      WA_RETORNO-DS_LOCAL_DESCARGA = WA_TAB-DS_LOCAL_DESCARGA.
      WA_RETORNO-TP_FRETE          = WA_TAB-TP_FRETE.
      WA_RETORNO-ID_PRODUTO        = WA_TAB-ID_PRODUTO.
      WA_RETORNO-DS_PRODUTO        = WA_TAB-DS_PRODUTO.
      WA_RETORNO-NM_PESO_BRUTO     = WA_TAB-NM_PESO_BRUTO.
      WA_RETORNO-NM_PESO_TARA      = WA_TAB-NM_PESO_TARA.
      WA_RETORNO-NM_PESO_SUBTOTAL  = WA_TAB-NM_PESO_SUBTOTAL.
      WA_RETORNO-NM_PESO_DESCONTOS = WA_TAB-NM_PESO_DESCONTOS.
      WA_RETORNO-NM_PESO_LIQUIDO   = WA_TAB-NM_PESO_LIQUIDO.
      WA_RETORNO-DS_PLACA_TRATOR   = WA_TAB-DS_PLACA_TRATOR.
      WA_RETORNO-ID_PROPRIETARIO   = WA_TAB-ID_PROPRIETARIO.
      WA_RETORNO-DS_PROPRIETARIO   = WA_TAB-DS_PROPRIETARIO.
      WA_RETORNO-DS_PLACA_REBOQ_1  = WA_TAB-DS_PLACA_REBOQ_1.
      WA_RETORNO-DS_PLACA_REBOQ_2  = WA_TAB-DS_PLACA_REBOQ_2.
      WA_RETORNO-DS_PLACA_REBOQ_3  = WA_TAB-DS_PLACA_REBOQ_3.
      WA_RETORNO-ID_MOTORISTA      = WA_TAB-ID_MOTORISTA.
      WA_RETORNO-DS_MOTORISTA      = WA_TAB-DS_MOTORISTA.
      WA_RETORNO-NR_TICKET         = WA_TAB-NR_TICKET.
      WA_RETORNO-IN_TRANSFERENCIA  = WA_TAB-IN_TRANSFERENCIA.
      WA_RETORNO-TP_STATUS         = WA_TAB-TP_STATUS.
      WA_RETORNO-DT_ABERTURA       = WA_TAB-DT_ABERTURA.
      WA_RETORNO-HR_ABERTURA       = WA_TAB-HR_ABERTURA.
      WA_RETORNO-US_ABERTURA       = WA_TAB-US_ABERTURA.
      WA_RETORNO-DT_FECHAMENTO     = WA_TAB-DT_FECHAMENTO.
      WA_RETORNO-HR_FECHAMENTO     = WA_TAB-HR_FECHAMENTO.
      WA_RETORNO-US_FECHAMENTO     = WA_TAB-US_FECHAMENTO.
      WA_RETORNO-DT_CONFERENCIA    = WA_TAB-DT_CONFERENCIA.
      WA_RETORNO-HR_CONFERENCIA    = WA_TAB-HR_CONFERENCIA.
      WA_RETORNO-US_CONFERENCIA    = WA_TAB-US_CONFERENCIA.
      WA_RETORNO-DT_CANCELAMENTO   = WA_TAB-DT_CANCELAMENTO.
      WA_RETORNO-HR_CANCELAMENTO   = WA_TAB-HR_CANCELAMENTO.
      WA_RETORNO-US_CANCELAMENTO   = WA_TAB-US_CANCELAMENTO.

      WA_RETORNO-ID_CLASSIFICACAO       = WA_TAB-ID_CLASSIFICACAO.
      WA_RETORNO-IN_GMO                 = WA_TAB-IN_GMO.
      WA_RETORNO-NR_RESULTADO_01        = WA_TAB-NR_RESULTADO_01.
      WA_RETORNO-NR_RESULTADO_02        = WA_TAB-NR_RESULTADO_02.
      WA_RETORNO-NR_RES_RR1_RR2         = WA_TAB-NR_RES_RR1_RR2.
      WA_RETORNO-IN_GMO_03              = WA_TAB-IN_GMO_03.
      WA_RETORNO-IN_SRR_ORIGEM_PARTIC   = WA_TAB-IN_SRR_ORIGEM_PARTIC.
      WA_RETORNO-ID_OUTRO_PARTIC        = WA_TAB-ID_OUTRO_PARTIC.
      WA_RETORNO-DS_OUTRO_PARTIC        = WA_TAB-DS_OUTRO_PARTIC.
      WA_RETORNO-IN_SRR_DECLARADO       = WA_TAB-IN_SRR_DECLARADO.
      WA_RETORNO-IN_TESTE_SRR           = WA_TAB-IN_TESTE_SRR.
      WA_RETORNO-IN_SRR_DECLARADO_2     = WA_TAB-IN_SRR_DECLARADO_2.
      WA_RETORNO-IN_TESTE_SRR_2         = WA_TAB-IN_TESTE_SRR_2.
      WA_RETORNO-TP_TRANSGENIA          = WA_TAB-TP_TRANSGENIA.

      WA_RETORNO-NR_PERC_UMI   = WA_TAB-NR_PERC_UMI.
      WA_RETORNO-NR_PERC_IMP   = WA_TAB-NR_PERC_IMP.
      WA_RETORNO-NR_PERC_AVA   = WA_TAB-NR_PERC_AVA.
      WA_RETORNO-NR_PERC_ARD   = WA_TAB-NR_PERC_ARD.
      WA_RETORNO-NR_PERC_QUE   = WA_TAB-NR_PERC_QUE.
      WA_RETORNO-NR_PERC_ESV   = WA_TAB-NR_PERC_ESV.
      WA_RETORNO-NR_PERC_CAR   = WA_TAB-NR_PERC_CAR.

      WA_RETORNO-NR_QTDE_UMI   = WA_TAB-NR_QTDE_UMI.
      WA_RETORNO-NR_QTDE_IMP   = WA_TAB-NR_QTDE_IMP.
      WA_RETORNO-NR_QTDE_AVA   = WA_TAB-NR_QTDE_AVA.
      WA_RETORNO-NR_QTDE_ARD   = WA_TAB-NR_QTDE_ARD.
      WA_RETORNO-NR_QTDE_QUE   = WA_TAB-NR_QTDE_QUE.
      WA_RETORNO-NR_QTDE_ESV   = WA_TAB-NR_QTDE_ESV.
      WA_RETORNO-NR_QTDE_CAR   = WA_TAB-NR_QTDE_CAR.

      LOOP AT IT_ZSDT0001ARS_03 INTO DATA(WA_ZSDT0001ARS_03)
          WHERE ID_SOLICITACAO  EQ WA_TAB-ID_SOLICITACAO
            AND ID_CARGA_ORIGEM EQ WA_TAB-ID_CARGA
            AND ID_CLASSIFICACAO EQ WA_TAB-ID_CLASSIFICACAO.

        CASE WA_ZSDT0001ARS_03-TP_SUB_CARAC_AVARIADO.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_ARQ.
            WA_RETORNO-NR_PERC_AVA_ARQ = WA_ZSDT0001ARS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_QUE.
            WA_RETORNO-NR_PERC_AVA_QUE = WA_ZSDT0001ARS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_MOF.
            WA_RETORNO-NR_PERC_AVA_MOF = WA_ZSDT0001ARS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_PIC.
            WA_RETORNO-NR_PERC_AVA_PIC = WA_ZSDT0001ARS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_FER.
            WA_RETORNO-NR_PERC_AVA_FER = WA_ZSDT0001ARS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_GER.
            WA_RETORNO-NR_PERC_AVA_GER = WA_ZSDT0001ARS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_ARD.
            WA_RETORNO-NR_PERC_AVA_ARD = WA_ZSDT0001ARS_03-NR_PERCENTUAL_COM.
          WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_GES.
            WA_RETORNO-NR_PERC_AVA_GES = WA_ZSDT0001ARS_03-NR_PERCENTUAL_COM.
        ENDCASE.

      ENDLOOP.

      APPEND WA_RETORNO TO LC_RETORNO.
    ENDLOOP.

    CHECK LC_RETORNO[] IS NOT INITIAL.

    E_REGISTROS = LC_RETORNO.
    E_PESQUISOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD zif_carga~send_carga_to_opus.

    DATA: lc_json             TYPE string,
          lc_json_aux         TYPE string,
          lc_data             TYPE c LENGTH 10,
          lc_hora             TYPE c LENGTH 08,
          lc_data_hora        TYPE c LENGTH 19,
          lc_numero_rom       TYPE zsdt0001-nr_romaneio,
          lc_inteiro          TYPE i,
          lc_peso             TYPE c LENGTH 20,
          lc_numero_nf        TYPE c LENGTH 09,
          i_name_file         TYPE string,
          lc_retorno          TYPE zde_opus_carga,
          "lc_docnum            TYPE j_1bdocnum,
          lc_nr_romaneio_nt   TYPE zsdt0001nt-nr_romaneio_ent,
          lc_nr_romaneio_sai  TYPE zde_nr_romaneio_sai,
          lc_json_takeup      TYPE string,
          lc_chave_nf_propria TYPE zde_chave_doc_e,
          zcl_util            TYPE REF TO zcl_util,
          w_class_aux         TYPE zsdt0001cl,
          w_class_nota        TYPE zsdt0001cl.

    DEFINE add_tag.
      CONCATENATE lc_json '"' &1 '": "' &2 '"' &3 INTO lc_json.
    END-OF-DEFINITION.

    DEFINE add_tag_take.
      CONCATENATE lc_json_takeup '"' &1 '": "' &2 '"' &3 INTO lc_json_takeup.
    END-OF-DEFINITION.



    r_instancia = me.

    CHECK me->zif_carga~at_manutencao EQ abap_false.

    CLEAR: lc_json, lc_numero_rom.

    CREATE OBJECT zcl_util.

    CONCATENATE '{' lc_json INTO lc_json.

    add_tag 'idCargaSap'  me->carga-id_carga   ','.
    add_tag 'centro'      me->carga-id_branch  ','.
    add_tag 'materialSap' me->carga-id_produto ','.
    add_tag 'safra'       me->carga-nr_safra   ','.

    "wpp 22102024 US-153342 EUDR --->>>>
    IF me->carga-protocolo_eudr IS NOT INITIAL.
      add_tag 'InEudr'            me->carga-eudr ','.
      add_tag 'NrProtocoloEudr'   me->carga-protocolo_eudr ','.
      add_tag 'IdEudr'            me->carga-id_protocolo_eudr ','.
    ENDIF.
    "wpp 22102024 US-153342 <<<----

    me->zif_carga~get_romaneio_entrada(
            EXPORTING
              i_id_carga = me->carga-id_carga
              i_cancelados = abap_true
            IMPORTING
              e_romaneios = DATA(romaneio_entrada)
              e_romaneios_cancel = DATA(e_romaneios_cancel)
       )->get_romaneio_saida( EXPORTING i_id_carga  = me->carga-id_carga IMPORTING e_romaneios = DATA(romaneio_saidas)
       ).

    SORT e_romaneios_cancel BY ch_referencia.

    LOOP AT e_romaneios_cancel INTO DATA(wa_romaneios_cancel).
      APPEND wa_romaneios_cancel TO romaneio_entrada.
    ENDLOOP.

    DESCRIBE TABLE romaneio_entrada LINES DATA(lc_qtd_linhas).

    CONCATENATE lc_json '"romaneioEntrada": [' INTO lc_json.
    LOOP AT romaneio_entrada INTO DATA(wa_entrada).

      DATA(lc_cancelado) = '0'.
      DATA(lc_tabix) = sy-tabix.

      READ TABLE e_romaneios_cancel TRANSPORTING NO FIELDS WITH KEY ch_referencia = wa_entrada-ch_referencia BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        READ TABLE me->documento_fiscal WITH KEY ch_referencia_ent = wa_entrada-ch_referencia INTO DATA(nota_fiscal).
        IF sy-subrc IS NOT INITIAL.
          READ TABLE me->documento_fiscal WITH KEY id_carga = wa_entrada-id_carga
                                                   id_nota  = wa_entrada-id_nota
            ASSIGNING FIELD-SYMBOL(<fs_nota>).
          IF sy-subrc IS NOT INITIAL.
            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid = VALUE #( msgid  = zcx_carga=>zcx_nota_romaneio_entrada-msgid
                                  msgno  = zcx_carga=>zcx_nota_romaneio_entrada-msgno
                                  attr1  = CONV #( wa_entrada-nr_romaneio ) )
                msgid  = zcx_carga=>zcx_nota_romaneio_entrada-msgid
                msgno  = zcx_carga=>zcx_nota_romaneio_entrada-msgno
                msgty  = 'E'
                msgv1  = CONV #( wa_entrada-nr_romaneio ).
          ELSE.
            <fs_nota>-ch_referencia_ent = wa_entrada-ch_referencia.
            MOVE-CORRESPONDING <fs_nota> TO nota_fiscal.
          ENDIF.
        ENDIF.
      ELSE.
        CLEAR: nota_fiscal.
        lc_cancelado = '1'.
      ENDIF.

      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'dsUsuarioSap'  sy-uname ','. "BUG SOLTO 182008

      add_tag 'observacao'    wa_entrada-ds_obs ','.
      add_tag 'placa'         wa_entrada-placa_cav ','.
      add_tag 'cancelado'     lc_cancelado ','.

      CONCATENATE me->carga-dt_abertura+6(2) '/' me->carga-dt_abertura+4(2) '/' me->carga-dt_abertura(4)   INTO lc_data.
      CONCATENATE me->carga-hr_abertura(2)   ':' me->carga-hr_abertura+2(2) ':' me->carga-hr_abertura+4(2) INTO lc_hora.
      CONCATENATE lc_data lc_hora INTO lc_data_hora SEPARATED BY space.
      add_tag 'dataAbertura'  lc_data_hora ','.

      CONCATENATE me->carga-dt_fechamento+6(2) '/' me->carga-dt_fechamento+4(2) '/' me->carga-dt_fechamento(4)   INTO lc_data.
      CONCATENATE me->carga-hr_fechamento(2)   ':' me->carga-hr_fechamento+2(2) ':' me->carga-hr_fechamento+4(2) INTO lc_hora.
      CONCATENATE lc_data lc_hora INTO lc_data_hora SEPARATED BY space.
      add_tag 'dataFechamento'  lc_data_hora ','.

      CONCATENATE me->carga-dt_movimento+6(2) '/' me->carga-dt_movimento+4(2) '/' me->carga-dt_movimento(4)   INTO lc_data.
      add_tag 'dataMovimento'  lc_data ','.
      add_tag 'fornecedorSap'  wa_entrada-parid ','.
      add_tag 'localEntrega'   wa_entrada-local_descarga ','.
      IF nota_fiscal-id_entregue_por IS INITIAL.
        add_tag 'fornecedorSapEntregue'  wa_entrada-parid ','.
      ELSE.
        add_tag 'fornecedorSapEntregue'  nota_fiscal-id_entregue_por ','.
      ENDIF.
      add_tag 'tipoEntrada'  wa_entrada-tipo_entrada ','.

      IF nota_fiscal-id_mod_fiscal NE zif_carga~st_model_fiscal_eletronico.
        add_tag 'notaFiscalEletronica' 'N' ','.
      ELSE.
        add_tag 'notaFiscalEletronica' 'S' ','.
      ENDIF.

*      CASE WA_ENTRADA-NFE.
*        WHEN ABAP_TRUE.
*          ADD_TAG 'notaFiscalEletronica' 'S' ','.
*        WHEN ABAP_FALSE.
*          ADD_TAG 'notaFiscalEletronica' 'N' ','.
*      ENDCASE.

      lc_inteiro = wa_entrada-peso_bruto.
      WRITE lc_inteiro TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      REPLACE ALL OCCURRENCES OF '.' IN lc_peso WITH ''.
      add_tag 'pesoBruto'      lc_peso ','.

      lc_inteiro = wa_entrada-peso_liq.
      WRITE lc_inteiro TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      REPLACE ALL OCCURRENCES OF '.' IN lc_peso WITH ''.
      add_tag 'pesoLiquido'    lc_peso ','.

      lc_inteiro = wa_entrada-peso_subtotal.
      WRITE lc_inteiro TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      REPLACE ALL OCCURRENCES OF '.' IN lc_peso WITH ''.
      add_tag 'pesoSubTotal'   lc_peso ','.

      lc_inteiro = wa_entrada-peso_tara.
      WRITE lc_inteiro TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      REPLACE ALL OCCURRENCES OF '.' IN lc_peso WITH ''.
      add_tag 'pesoTara'       lc_peso ','.

      add_tag 'numeroRomaneio' wa_entrada-nr_romaneio ','.
      add_tag 'numeroTicket'   wa_entrada-nr_ticket ','.

      add_tag 'frete'          wa_entrada-tp_frete ','.
      add_tag 'motoristaSap'   wa_entrada-motorista ','.
      add_tag 'dataCapturaPesoTara'    lc_data_hora ','.
      add_tag 'dataCapturaPesoBruto'   lc_data_hora ','.
      add_tag 'dataPrimeiroFechamento' lc_data_hora ','.

      IF lc_numero_rom IS INITIAL.
        lc_numero_rom = wa_entrada-nr_romaneio.
      ENDIF.

      CASE me->carga-ck_gera_aviso.
        WHEN abap_true.
          add_tag 'InAvisoRecebimento' 'S' ','.
        WHEN abap_false.
          add_tag 'InAvisoRecebimento' 'N' ','.
      ENDCASE.

      IF lc_tabix LT 1.
        add_tag 'desmembrado' '1' ','.
      ELSE.
        add_tag 'desmembrado' '0' ','.
      ENDIF.

      IF nota_fiscal-id_mod_fiscal NE zif_carga~st_model_fiscal_eletronico.
        add_tag 'nfe' 'N' ','.
      ELSE.
        add_tag 'nfe' 'S' ','.
      ENDIF.

*      CASE WA_ENTRADA-NFE.
*        WHEN ABAP_TRUE.
*          ADD_TAG 'nfe' 'S' ','.
*        WHEN ABAP_FALSE.
*          ADD_TAG 'nfe' 'N' ','.
*      ENDCASE.

      IF lc_tabix LT 1.
        add_tag 'numeroRomaneioDesmembrado' lc_numero_rom ','.
      ELSE.
        add_tag 'numeroRomaneioDesmembrado' '' ','.
      ENDIF.

      "Classificação
      CONCATENATE lc_json '"classificacao": {' INTO lc_json.
      add_tag 'observacao' wa_entrada-ds_obs ','.
      add_tag 'placa'      wa_entrada-placa_cav ','.
      add_tag 'data'       lc_data_hora ','.

*    "US 143677 - transgenia por nota
      CLEAR w_class_aux.
      READ TABLE me->classificacao_notas INTO w_class_nota WITH KEY id_classificacao = nota_fiscal-id_classificacao.
      IF sy-subrc = 0.
        w_class_aux =  me->classificacao.
        MOVE-CORRESPONDING w_class_nota TO me->classificacao.
      ENDIF.
      "US 143677 - transgenia por nota
      IF me->classificacao-id_outro_partic IS INITIAL.
        CONCATENATE lc_json '"fornecedorParticipanteSap": null,' INTO lc_json.
      ELSE.
        add_tag 'fornecedorParticipanteSap' me->classificacao-id_outro_partic ','.
      ENDIF.

      add_tag 'entradaSaida' wa_entrada-tp_movimento ','.

      "Ajustado
      CASE me->classificacao-in_gmo.
        WHEN '0'.
          add_tag 'inGmo' '-1' ','.
        WHEN '1'.
          add_tag 'inGmo' '1' ','.
        WHEN '2'.
          add_tag 'inGmo' '0' ','.
      ENDCASE.

      CASE me->classificacao-in_srr_declarado.
        WHEN abap_true.
          add_tag 'inSrrDeclarado' '1' ','.
        WHEN abap_false.
          add_tag 'inSrrDeclarado' '0' ','.
      ENDCASE.

      CASE me->classificacao-in_srr_declarado_2.
        WHEN abap_true.
          add_tag 'inSrrDeclarado2' '1' ','.
        WHEN abap_false.
          add_tag 'inSrrDeclarado2' '0' ','.
      ENDCASE.

      CASE me->classificacao-in_teste_srr.
        WHEN '0'.
          add_tag 'inTesteSrr' '-1' ','.
        WHEN '1'.
          add_tag 'inTesteSrr' '1' ','.
        WHEN '2'.
          add_tag 'inTesteSrr' '0' ','.
      ENDCASE.

      CASE me->classificacao-in_teste_srr_2.
        WHEN '0'.
          add_tag 'inTesteSrr2' '-1' ','.
        WHEN '1'.
          add_tag 'inTesteSrr2' '1' ','.
        WHEN '2'.
          add_tag 'inTesteSrr2' '0' ','.
      ENDCASE.

      CASE me->classificacao-in_srr_origem_partic.
        WHEN abap_true.
          add_tag 'participante' '1' ','.
        WHEN abap_false.
          add_tag 'participante' '0' ','.

      ENDCASE.

      WRITE me->classificacao-nr_resultado_01 TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'numeroResultado01' lc_peso ','.

      WRITE me->classificacao-nr_resultado_02 TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'numeroResultado2' lc_peso ','.

      "US 143677 - transgenia por nota
      IF w_class_aux IS NOT INITIAL.
        MOVE-CORRESPONDING w_class_aux TO me->classificacao.
      ENDIF.
      "US 143677 - transgenia por nota

*-CS2022001166-27.12.2022-#98820-JT-inicio - Comentado para Publicar ajuste em PRD 23/08/2023
*      WRITE me->classificacao-nr_resultado_03 TO lc_peso.
*      CONDENSE lc_peso NO-GAPS.
*      add_tag 'numeroResultado3' lc_peso ','.
*
*      WRITE me->classificacao-nr_resultado_04 TO lc_peso.
*      CONDENSE lc_peso NO-GAPS.
*      add_tag 'numeroResultado4' lc_peso ','.
*
*      add_tag 'numeroFita' me->classificacao-nr_fita ','.
*-CS2022001166-27.12.2022-#98820-JT-fim

      add_tag 'ticket' wa_entrada-nr_ticket ','.

      IF me->classificacao-id_classificadora IS INITIAL.
        add_tag 'fornecedorClassificadoraSap' '' ','.
      ELSE.
        add_tag 'fornecedorClassificadoraSap' me->classificacao-id_classificadora ','.
      ENDIF.

***///CS2021000581 Classificação destino - Flag para marcação - ZMM0127 - Set 2021 - Inicio
      IF me->classificacao-ck_class_dest = 'X'.
        add_tag 'InClassificacaoDestino' '1' ','.
      ELSE.
        add_tag 'InClassificacaoDestino' '0' ','.
      ENDIF.
***///CS2021000581 Classificação destino - Flag para marcação - ZMM0127 - Set 2021 - Fim
***///

      CONCATENATE lc_json '"resultadoClassificacao": [' INTO lc_json.

      "Umidade
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '1' ','.
      WRITE wa_entrada-nr_perc_umidade TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_entrada-nr_qtd_umidade TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ''.
      CONCATENATE lc_json '},' INTO lc_json.

      "Impureza
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '2' ','.
      WRITE wa_entrada-nr_perc_impureza TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_entrada-nr_qtd_impureza TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ''.
      CONCATENATE lc_json '},' INTO lc_json.

      "Avariado
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '3' ','.
      WRITE wa_entrada-nr_perc_avaria TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_entrada-nr_qtd_avaria TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ','.

      "Sub Caacterísticas do Avariado"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Sub Caacterísticas do Avariado"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Sub Caacterísticas do Avariado"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      CLEAR: lc_json_aux.
      CONCATENATE lc_json '"subClassificacao": [' INTO lc_json.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_arq IMPORTING e_ck_carac = DATA(e_ck_carac) ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_arq TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_arq && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_que IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_que TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_que && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_mof IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_mof TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_mof && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_pic IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_pic TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_pic && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_fer IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_fer TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_fer && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_ger IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_ger TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_ger && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_ard IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_ard TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_ard && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_ges IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_ges TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_ges && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      CONCATENATE lc_json lc_json_aux ']' INTO lc_json.
      """""""""""""""""""""""""""""""""""""""""""""""
      CONCATENATE lc_json '},' INTO lc_json.

      "Ardido
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '4' ','.
      WRITE wa_entrada-nr_perc_ardido TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_entrada-nr_qtd_ardido TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ''.
      CONCATENATE lc_json '},' INTO lc_json.

      "Quebrado
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '5' ','.
      WRITE wa_entrada-nr_perc_quebra TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_entrada-nr_qtd_quebra TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ''.
      CONCATENATE lc_json '},' INTO lc_json.

      "Esverdeado
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '6' ','.
      WRITE wa_entrada-nr_perc_esverd TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_entrada-nr_qtd_esverd TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ''.
      CONCATENATE lc_json '},' INTO lc_json.

      "Carunchado
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '7' ','.
      WRITE wa_entrada-nr_perc_carunch TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_entrada-nr_qtd_carunch TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ''.
      CONCATENATE lc_json '}' INTO lc_json.

      "Final do Resultado
      CONCATENATE lc_json ']' INTO lc_json.
      "Final da Classificação
      CONCATENATE lc_json '},' INTO lc_json.

      "Processamento SIGAM
      CONCATENATE lc_json '"processamentoSigam": {' INTO lc_json.
      add_tag 'IdPedidoCompraSap'   nota_fiscal-po_number ','.
      add_tag 'NuAvisoRecebimento'  nota_fiscal-av_vbeln ','.
      add_tag 'NuDocumentoFatura'   nota_fiscal-ft_belnr ','.

      "Recuperar Documento Contabil Miro - Ini - WPP 10-08-2023
      IF nota_fiscal-ft_belnr IS NOT INITIAL.
        DATA: lwa_bkpf_miro TYPE bkpf.

        CLEAR: lwa_bkpf_miro.

        SELECT SINGLE *
           FROM rbkp INTO @DATA(_lwa_rbkp)
          WHERE belnr EQ @nota_fiscal-ft_belnr
            AND gjahr EQ @nota_fiscal-ft_gjahr.

        IF sy-subrc EQ 0.

          CONCATENATE  _lwa_rbkp-belnr _lwa_rbkp-gjahr INTO DATA(lwa_awkey_bkpf).

          SELECT SINGLE belnr bukrs gjahr budat kurs2 cpudt
            FROM bkpf INTO CORRESPONDING FIELDS OF lwa_bkpf_miro
           WHERE bukrs = _lwa_rbkp-bukrs
             AND gjahr = _lwa_rbkp-gjahr
             AND awkey = lwa_awkey_bkpf
             AND blart = 'ZG'.

          IF sy-subrc NE 0.
            SELECT SINGLE belnr bukrs gjahr budat kurs2 cpudt
              FROM bkpf INTO CORRESPONDING FIELDS OF lwa_bkpf_miro
             WHERE bukrs = _lwa_rbkp-bukrs
               AND gjahr = _lwa_rbkp-gjahr
               AND belnr = _lwa_rbkp-belnr.
          ENDIF.

          IF lwa_bkpf_miro IS NOT INITIAL.
            add_tag 'NuDocumentoContabilMiro'  lwa_bkpf_miro-belnr ','.
          ENDIF.
        ENDIF.
      ENDIF.

      IF nota_fiscal-mm_mblnr IS NOT INITIAL AND nota_fiscal-ft_belnr IS INITIAL.
        DATA: lwa_bkpf_migo TYPE bkpf.

        CLEAR: lwa_bkpf_migo.

        SELECT SINGLE *
           FROM mkpf INTO @DATA(_lwa_mkpf)
          WHERE mblnr EQ @nota_fiscal-mm_mblnr.

        IF sy-subrc EQ 0.

          CONCATENATE  _lwa_mkpf-mblnr _lwa_mkpf-mjahr INTO lwa_awkey_bkpf.

          SELECT SINGLE belnr bukrs gjahr budat kurs2 cpudt
            FROM bkpf INTO CORRESPONDING FIELDS OF lwa_bkpf_migo
           WHERE xblnr = lwa_awkey_bkpf.

          IF sy-subrc NE 0.
            SELECT SINGLE belnr bukrs gjahr budat kurs2 cpudt
              FROM bkpf INTO CORRESPONDING FIELDS OF lwa_bkpf_migo
             WHERE awkey = lwa_awkey_bkpf
               AND blart <> 'ML'.
          ENDIF.

          IF lwa_bkpf_migo IS NOT INITIAL.
            add_tag 'NuDocumentoContabilMiro'  lwa_bkpf_migo-belnr ','.
          ENDIF.
        ENDIF.
      ENDIF.
      "Recuperar Documento Contabil Miro - Fim - WPP 10-08-2023


      add_tag 'NuDocumentoMaterial' nota_fiscal-mm_mblnr ','.
      add_tag 'NuDocumentoSap'      nota_fiscal-docnum   ''.
      CONCATENATE lc_json '},' INTO lc_json.

      SELECT SINGLE * INTO @DATA(wa_j_1bnflin)
        FROM j_1bnflin
       WHERE docnum EQ @nota_fiscal-docnum.

      CASE lc_cancelado.
        WHEN '0'.

          "Nota Fiscal

          lc_numero_nf = nota_fiscal-nr_nota.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = lc_numero_nf
            IMPORTING
              output = lc_numero_nf.

          CONCATENATE lc_json '"notaFiscal": {' INTO lc_json.
          add_tag 'numero'      lc_numero_nf          ','.
          add_tag 'serie'       nota_fiscal-nm_serie  ','.
          add_tag 'cfop'        nota_fiscal-cfop      ','.
          CONCATENATE nota_fiscal-dt_emissao+6(2) '/' nota_fiscal-dt_emissao+4(2) '/' nota_fiscal-dt_emissao(4) INTO lc_data.
          add_tag 'dataEmissao' lc_data  ','.

          lc_inteiro = nota_fiscal-nr_quantidade.
          WRITE lc_inteiro TO lc_peso.
          CONDENSE lc_peso NO-GAPS.
          add_tag 'quantidade'  lc_peso  ','.

          WRITE nota_fiscal-nr_valor TO lc_peso.
          CONDENSE lc_peso NO-GAPS.
          add_tag 'valor' lc_peso  ','.

          "SELECT SINGLE * INTO @DATA(WA_ACTIVE)
          "  FROM J_1BNFE_ACTIVE
          " WHERE DOCNUM EQ @NOTA_FISCAL-DOCNUM.

          "CONCATENATE WA_ACTIVE-REGIO WA_ACTIVE-NFYEAR WA_ACTIVE-NFMONTH WA_ACTIVE-STCD1 WA_ACTIVE-MODEL WA_ACTIVE-SERIE WA_ACTIVE-NFNUM9 WA_ACTIVE-DOCNUM9
          "            WA_ACTIVE-CDV INTO DATA(LC_CHAVE).

          IF nota_fiscal-nr_chave_nfe IS INITIAL.
            CONCATENATE lc_json '"chaveNfe": null,' INTO lc_json.
          ELSE.
            add_tag 'chaveNfe' nota_fiscal-nr_chave_nfe ','.
          ENDIF.

          CONCATENATE nota_fiscal-dt_vencimento_form+6(2) '/' nota_fiscal-dt_vencimento_form+4(2) '/' nota_fiscal-dt_vencimento_form(4) INTO lc_data.
          add_tag 'dataVencimentoFormulario' lc_data  ''.

          "Incluir Dados de Take UP """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          CLEAR: lc_json_takeup.
          LOOP AT me->zif_carga~take_up INTO DATA(wa_takeup)
            WHERE id_carga EQ wa_entrada-id_carga
              AND id_nota  EQ wa_entrada-id_nota.
            IF lc_json_takeup IS NOT INITIAL.
              CONCATENATE ',' lc_json_takeup INTO lc_json_takeup.
            ENDIF.
            CONCATENATE lc_json_takeup '{' INTO lc_json_takeup.
            add_tag_take 'idTakeUp'    wa_takeup-id_takeup ','.
            add_tag_take 'numeroBloco' wa_takeup-nu_bloco  ','.

            lc_inteiro = wa_takeup-qt_vinculada.
            WRITE lc_inteiro TO lc_peso.
            CONDENSE lc_peso NO-GAPS.
            add_tag_take 'numeroQuantidadeVolume' lc_peso ''.
            CONCATENATE lc_json_takeup '}' INTO lc_json_takeup.
          ENDLOOP.

          IF lc_json_takeup IS NOT INITIAL.
            CONCATENATE lc_json ', "lotesCompraTakeUp": [' INTO lc_json.
            CONCATENATE lc_json lc_json_takeup INTO lc_json.
            CONCATENATE lc_json ']' INTO lc_json.
          ENDIF.
          "Incluir Dados de Take UP """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*-CS2021000183 - 17.05.2022 - JT - inicio
          IF nota_fiscal-docnum_np IS INITIAL.
            CONCATENATE lc_json '}'  INTO lc_json.
          ELSE.
            CONCATENATE lc_json '},' INTO lc_json.
          ENDIF.
*-CS2021000183 - 17.05.2022 - JT - fim

*-CS2021000183 - 17.05.2022 - JT - inicio
          IF nota_fiscal-docnum_np IS NOT INITIAL.
            SELECT *
              INTO @DATA(w_jdoc)
              FROM j_1bnfdoc
                UP TO 1 ROWS
             WHERE docnum = @nota_fiscal-docnum_np.
            ENDSELECT.

            SELECT *
              INTO @DATA(w_jlin)
              FROM j_1bnflin
                UP TO 1 ROWS
             WHERE docnum = @nota_fiscal-docnum_np.
            ENDSELECT.

            lc_chave_nf_propria = zcl_util->get_chave_nfe( nota_fiscal-docnum_np ).

            lc_numero_nf = w_jdoc-nfenum.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = lc_numero_nf
              IMPORTING
                output = lc_numero_nf.

            CONCATENATE lc_json '"notaFiscalPropria": {' INTO lc_json.
            add_tag 'numero'      lc_numero_nf          ','.
            add_tag 'serie'       w_jdoc-series         ','.
            add_tag 'cfop'        w_jlin-cfop(4)        ','.
            CONCATENATE w_jdoc-docdat+6(2) '/' w_jdoc-docdat+4(2) '/' w_jdoc-docdat(4) INTO lc_data.
            add_tag 'dataEmissao' lc_data  ','.

            lc_inteiro            = w_jlin-menge.
            WRITE lc_inteiro     TO lc_peso.
            CONDENSE lc_peso     NO-GAPS.
            add_tag 'quantidade' lc_peso  ','.

            WRITE w_jlin-netwr   TO lc_peso.
            CONDENSE lc_peso     NO-GAPS.
            add_tag 'valor'      lc_peso  ','.

            add_tag 'chaveNfe'   lc_chave_nf_propria ','.

*            CONCATENATE lc_json '"dataVencimentoFormulario": null,' INTO lc_json.
            add_tag 'dataVencimentoFormulario' lc_data  ','.

            add_tag 'NumeroDocumentoSap'  nota_fiscal-docnum_np ''.

            CONCATENATE lc_json '}' INTO lc_json.
          ENDIF.
*-CS2021000183 - 17.05.2022 - JT - fim

        WHEN '1'.
          "Nota Fiscal
          CONCATENATE lc_json '"notaFiscal": null' INTO lc_json.

*          CONCATENATE LC_JSON '"notaFiscal": {' INTO LC_JSON.
*          CONCATENATE LC_JSON '"numero": null,' INTO LC_JSON.
*          CONCATENATE LC_JSON '"serie": null,' INTO LC_JSON.
*          CONCATENATE LC_JSON '"cfop": null,' INTO LC_JSON.
*          CONCATENATE LC_JSON '"dataEmissao": null,' INTO LC_JSON.
*          CONCATENATE LC_JSON '"quantidade": null,' INTO LC_JSON.
*          CONCATENATE LC_JSON '"valor": null,' INTO LC_JSON.
*          CONCATENATE LC_JSON '"chaveNfe": null,' INTO LC_JSON.
*          CONCATENATE LC_JSON '"dataVencimentoFormulario": null ' INTO LC_JSON.
*          CONCATENATE LC_JSON '}' INTO LC_JSON.
      ENDCASE.

      "Final do Romaneio de Entrada
      IF lc_tabix NE lc_qtd_linhas.
        CONCATENATE lc_json '},' INTO lc_json.
      ELSE.
        CONCATENATE lc_json '}' INTO lc_json.
      ENDIF.
    ENDLOOP.
    "Fim Romaneio de Entrada
    CONCATENATE lc_json '],' INTO lc_json.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CLEAR: lc_numero_rom.

    DESCRIBE TABLE romaneio_saidas LINES lc_qtd_linhas.
    CONCATENATE lc_json '"romaneioSaida": [' INTO lc_json.
    LOOP AT romaneio_saidas INTO DATA(wa_saida).

      lc_tabix = sy-tabix.

      IF lc_numero_rom IS INITIAL.
        lc_numero_rom = wa_saida-nr_romaneio.
      ENDIF.

      CONCATENATE lc_json '{' INTO lc_json.

      add_tag 'dsUsuarioSap'  sy-uname ','. "BUG SOLTO 182008

      lc_inteiro = wa_saida-peso_bruto.
      WRITE lc_inteiro TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      REPLACE ALL OCCURRENCES OF '.' IN lc_peso WITH ''.
      add_tag 'pesoBruto' lc_peso ','.

      lc_inteiro = wa_saida-peso_liq.
      WRITE lc_inteiro TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      REPLACE ALL OCCURRENCES OF '.' IN lc_peso WITH ''.
      add_tag 'pesoLiquido' lc_peso ','.

      lc_inteiro = wa_saida-peso_subtotal.
      WRITE lc_inteiro TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      REPLACE ALL OCCURRENCES OF '.' IN lc_peso WITH ''.
      add_tag 'pesoSubTotal' lc_peso ','.

      lc_inteiro = wa_saida-peso_tara.
      WRITE lc_inteiro TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      REPLACE ALL OCCURRENCES OF '.' IN lc_peso WITH ''.
      add_tag 'pesoTara'       lc_peso ','.

      add_tag 'numeroRomaneio' wa_saida-nr_romaneio ','.
      add_tag 'numeroTicket'   wa_saida-nr_ticket ','.

      add_tag 'tipoFrete'      wa_saida-tp_frete ','.

      CASE wa_saida-tp_transgenia.
        WHEN 'CO'.
          add_tag 'tipoProduto'  'C' ','.
        WHEN OTHERS.
          add_tag 'tipoProduto'  'R' ','.
      ENDCASE.

      CONCATENATE wa_saida-dt_abertura+6(2) '/' wa_saida-dt_abertura+4(2) '/' wa_saida-dt_abertura(4)   INTO lc_data.
      CONCATENATE wa_saida-hr_abertura(2)   ':' wa_saida-hr_abertura+2(2) ':' wa_saida-hr_abertura+4(2) INTO lc_hora.
      CONCATENATE lc_data lc_hora INTO lc_data_hora SEPARATED BY space.
      add_tag 'dataAbertura'  lc_data_hora ','.

      CONCATENATE wa_saida-dt_fechamento+6(2) '/' wa_saida-dt_fechamento+4(2) '/' wa_saida-dt_fechamento(4)   INTO lc_data.
      CONCATENATE wa_saida-hr_fechamento(2)   ':' wa_saida-hr_fechamento+2(2) ':' wa_saida-hr_fechamento+4(2) INTO lc_hora.
      CONCATENATE lc_data lc_hora INTO lc_data_hora SEPARATED BY space.
      add_tag 'dataPrimeiroFechamento'  lc_data_hora ','.
      add_tag 'dataCapturaPesoTara'     lc_data_hora ','.
      add_tag 'dataCapturaPesoBruto'    lc_data_hora ','.
      add_tag 'dataFechamento'          lc_data_hora ','.

      add_tag 'observacao'  wa_saida-ds_obs ','.

      CONCATENATE wa_saida-dt_movimento+6(2) '/' wa_saida-dt_movimento+4(2) '/' wa_saida-dt_movimento(4)   INTO lc_data.
      add_tag 'dataMovimento' lc_data ','.
      add_tag 'placa'         wa_saida-placa_cav ','.
      add_tag 'motoristaSap'  wa_saida-motorista ','.

      IF me->carga-id_ordem IS NOT INITIAL.
        SELECT SINGLE * INTO @DATA(wa_ordem_carrega)
          FROM zsdt0001od
         WHERE id_ordem EQ @me->carga-id_ordem.

        add_tag 'ordemCarregamento' wa_ordem_carrega-nr_ordem ','.
      ELSE.
        CONCATENATE lc_json '"ordemCarregamento": null,' INTO lc_json.
      ENDIF.
      add_tag 'ordemVenda'        wa_saida-vbeln ','.

      CASE me->carga-ck_gera_aviso.
        WHEN abap_true.
          add_tag 'InAvisoRecebimento' 'S' ','.
        WHEN abap_false.
          add_tag 'InAvisoRecebimento' 'N' ','.
      ENDCASE.

      IF lc_tabix LT 1.
        add_tag 'desmembrado' 'S' ','.
      ELSE.
        add_tag 'desmembrado' 'N' ','.
      ENDIF.

      IF lc_tabix LT 1.
        add_tag 'numeroRomaneioDesmembrado' lc_numero_rom ','.
      ELSE.
        add_tag 'numeroRomaneioDesmembrado' '' ','.
      ENDIF.

      "Classificação
      CONCATENATE lc_json '"classificacao": {' INTO lc_json.
      add_tag 'observacao' wa_saida-ds_obs ','.
      add_tag 'placa'      wa_saida-placa_cav ','.
      add_tag 'data'       lc_data_hora ','.

      IF me->classificacao-id_outro_partic IS INITIAL.
        add_tag 'fornecedorParticipanteSap' '' ','.
      ELSE.
        add_tag 'fornecedorParticipanteSap' me->classificacao-id_outro_partic ','.
      ENDIF.

      add_tag 'entradaSaida' wa_saida-tp_movimento ','.

      CASE me->classificacao-in_gmo.
        WHEN '0'.
          add_tag 'inGmo' '-1' ','.
        WHEN '1'.
          add_tag 'inGmo' '1' ','.
        WHEN '2'.
          add_tag 'inGmo' '0' ','.
      ENDCASE.

      CASE me->classificacao-in_srr_declarado.
        WHEN abap_true.
          add_tag 'inSrrDeclarado' '1' ','.
        WHEN abap_false.
          add_tag 'inSrrDeclarado' '0' ','.
      ENDCASE.

      CASE me->classificacao-in_srr_declarado_2.
        WHEN abap_true.
          add_tag 'inSrrDeclarado2' '1' ','.
        WHEN abap_false.
          add_tag 'inSrrDeclarado2' '0' ','.
      ENDCASE.

      CASE me->classificacao-in_teste_srr.
        WHEN '0'.
          add_tag 'inTesteSrr' '-1' ','.
        WHEN '1'.
          add_tag 'inTesteSrr' '1' ','.
        WHEN '2'.
          add_tag 'inTesteSrr' '0' ','.
      ENDCASE.

      CASE me->classificacao-in_teste_srr_2.
        WHEN '0'.
          add_tag 'inTesteSrr2' '-1' ','.
        WHEN '1'.
          add_tag 'inTesteSrr2' '1' ','.
        WHEN '2'.
          add_tag 'inTesteSrr2' '0' ','.
      ENDCASE.

      CASE me->classificacao-in_srr_origem_partic.
        WHEN abap_true.
          add_tag 'participante' '1' ','.
        WHEN abap_false.
          add_tag 'participante' '0' ','.

      ENDCASE.

      WRITE me->classificacao-nr_resultado_01 TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'numeroResultado01' lc_peso ','.

      WRITE me->classificacao-nr_resultado_02 TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'numeroResultado2' lc_peso ','.

*-CS2022001166-27.12.2022-#98820-JT-inicio - Comentado para Publicar ajuste em PRD 23/08/2023
*      WRITE me->classificacao-nr_resultado_03 TO lc_peso.
*      CONDENSE lc_peso NO-GAPS.
*      add_tag 'numeroResultado3' lc_peso ','.
*
*      WRITE me->classificacao-nr_resultado_04 TO lc_peso.
*      CONDENSE lc_peso NO-GAPS.
*      add_tag 'numeroResultado4' lc_peso ','.
*
*      add_tag 'numeroFita' me->classificacao-nr_fita ','.
*-CS2022001166-27.12.2022-#98820-JT-fim

      add_tag 'ticket' wa_saida-nr_ticket ','.

      IF me->classificacao-id_classificadora IS INITIAL.
        add_tag 'fornecedorClassificadoraSap' '' ','.
      ELSE.
        add_tag 'fornecedorClassificadoraSap' me->classificacao-id_classificadora ','.
      ENDIF.

***///CS2021000581 Classificação destino - Flag para marcação - ZMM0127 - Set 2021 - Inicio
      IF me->classificacao-ck_class_dest = 'X'.
        add_tag 'InClassificacaoDestino' '1' ','.
      ELSE.
        add_tag 'InClassificacaoDestino' '0' ','.
      ENDIF.
***///CS2021000581 Classificação destino - Flag para marcação - ZMM0127 - Set 2021 - Fim
***///

      CONCATENATE lc_json '"resultadoClassificacao": [' INTO lc_json.

      "Umidade
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '1' ','.
      WRITE wa_saida-nr_perc_umidade TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_saida-nr_qtd_umidade TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ''.
      CONCATENATE lc_json '},' INTO lc_json.

      "Impureza
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '2' ','.
      WRITE wa_saida-nr_perc_impureza TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_saida-nr_qtd_impureza TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ''.
      CONCATENATE lc_json '},' INTO lc_json.

      "Avariado
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '3' ','.
      WRITE wa_saida-nr_perc_avaria TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_saida-nr_qtd_avaria TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ','.
      "Sub Caacterísticas do Avariado""""""""""""""""
      CLEAR: lc_json_aux.
      CONCATENATE lc_json '"subClassificacao": [' INTO lc_json.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_arq IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_arq TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_arq && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_que IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_que TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_que && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_mof IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_mof TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_mof && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_pic IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_pic TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_pic && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_fer IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_fer TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_fer && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_ger IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_ger TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_ger && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_ard IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_ard TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_ard && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      me->zif_carga~get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_ges IMPORTING e_ck_carac = e_ck_carac ).
      IF e_ck_carac EQ abap_true.
        IF lc_json_aux IS NOT INITIAL.
          lc_json_aux = lc_json_aux && ','.
        ENDIF.
        WRITE wa_entrada-nr_perc_ava_ges TO lc_peso.
        CONDENSE lc_peso NO-GAPS.
        lc_json_aux = lc_json_aux && '{ "subCaracteristica": "' && zif_carga=>st_tp_caract_sub_ges && '", "percentual" : "' && lc_peso && '" }'.
      ENDIF.

      CONCATENATE lc_json lc_json_aux ']' INTO lc_json.
      """""""""""""""""""""""""""""""""""""""""""""""
      CONCATENATE lc_json '},' INTO lc_json.

      "Ardido
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '4' ','.
      WRITE wa_saida-nr_perc_ardido TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_saida-nr_qtd_ardido TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ''.
      CONCATENATE lc_json '},' INTO lc_json.

      "Quebrado
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '5' ','.
      WRITE wa_saida-nr_perc_quebra TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_saida-nr_qtd_quebra TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ''.
      CONCATENATE lc_json '},' INTO lc_json.

      "Esverdeado
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '6' ','.
      WRITE wa_saida-nr_perc_esverd TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_saida-nr_qtd_esverd TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ''.
      CONCATENATE lc_json '},' INTO lc_json.

      "Carunchado
      CONCATENATE lc_json '{' INTO lc_json.
      add_tag 'caracteristica' '7' ','.
      WRITE wa_saida-nr_perc_carunch TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'percentual' lc_peso ','.
      add_tag 'percentualDescontado' '0' ','.
      WRITE wa_saida-nr_qtd_carunch TO lc_peso.
      CONDENSE lc_peso NO-GAPS.
      add_tag 'valorDesconto' lc_peso ''.
      CONCATENATE lc_json '}' INTO lc_json.

      "Final do Resultado
      CONCATENATE lc_json ']' INTO lc_json.
      "Final da Classificação
      CONCATENATE lc_json '},' INTO lc_json.

      CONCATENATE lc_json ' "notaFiscal": {' INTO lc_json.
      add_tag 'numero' '0' ','.
      add_tag 'serie' '0' ','.
      add_tag 'cfop' '0' ','.
      CONCATENATE me->carga-dt_movimento+6(2) '/' me->carga-dt_movimento+4(2) '/' me->carga-dt_movimento(4)   INTO lc_data.
      add_tag 'dataEmissao' lc_data ','.
      add_tag 'dataVencimentoFormulario' lc_data  ','.
      add_tag 'quantidade' '0' ','.
      add_tag 'valor' '0' ','.
      add_tag 'chaveNfe' '' ''.
      CONCATENATE lc_json ' }' INTO lc_json.

      "Final do Romaneio de Saída
      IF lc_tabix NE lc_qtd_linhas.
        CONCATENATE lc_json '},' INTO lc_json.
      ELSE.
        CONCATENATE lc_json '}' INTO lc_json.
      ENDIF.

    ENDLOOP.
    "Fim Romaneio de Saída
    CONCATENATE lc_json ']' INTO lc_json.

    "Fim Arquivo
    CONCATENATE lc_json '}' INTO lc_json.

    DATA: ob_web_service TYPE REF TO zcl_webservice.

    CREATE OBJECT ob_web_service.

    ob_web_service->zif_webservice~autentica_opus = abap_true.

    TRY .
        CASE me->zif_carga~ck_executar_manutencao_entrada.
          WHEN abap_false.
            ob_web_service->set_servico( i_servico = 'IT' ).
          WHEN abap_true.
            ob_web_service->set_servico( i_servico = 'IA' ).
        ENDCASE.
      CATCH zcx_webservice INTO DATA(lc_exception).
    ENDTRY.
    ob_web_service->set_tipo( i_tipo = 'O' ).

    TRY .
        DATA(var_http) = ob_web_service->url( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
        DATA(lc_uri) = ob_web_service->get_uri(  ).
      CATCH zcx_webservice INTO lc_exception.
    ENDTRY.

    ob_web_service->zif_webservice~abrir_conexao( i_http = var_http ).

    me->carga-ck_enviado_opus  = abap_true.
    me->carga-ck_recebido_opus = abap_false.
    UPDATE zsdt0001cg
       SET ck_enviado_opus  = abap_true
           ck_recebido_opus = abap_false
     WHERE id_carga = me->carga-id_carga.
    COMMIT WORK.

    ob_web_service->zif_webservice~consultar(
      EXPORTING
        i_http                     = var_http
        i_xml                      = lc_json
      IMPORTING
        e_code                     = e_code
        e_reason                   = DATA(e_reason)
      RECEIVING
        e_resultado                = DATA(json_retorno)
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgty  = 'E'
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.

    IF e_code NE 200.
      me->carga-ck_enviado_opus  = abap_false.
      me->carga-ck_recebido_opus = abap_false.

      UPDATE zsdt0001cg
         SET ck_enviado_opus  = abap_false
             ck_recebido_opus = abap_false
       WHERE id_carga = me->carga-id_carga.
      COMMIT WORK.

      CALL METHOD /ui2/cl_json=>deserialize
        EXPORTING
          json = json_retorno
        CHANGING
          data = lc_retorno.

      IF lc_retorno-message IS NOT INITIAL.
        CONCATENATE 'OPUS:' lc_retorno-message INTO DATA(lc_texto_code) SEPARATED BY space.
      ELSE.
        CONCATENATE 'OPUS:' e_reason INTO lc_texto_code SEPARATED BY space.
      ENDIF.
      me->zif_carga~gera_erro_geral( EXPORTING i_texto = lc_texto_code ).
    ENDIF.

    me->carga-ck_recebido_opus = abap_true.
    UPDATE zsdt0001cg
       SET ck_recebido_opus = abap_true
     WHERE id_carga = me->carga-id_carga.
    COMMIT WORK.

    DATA: arquivo TYPE REF TO zcl_arquivo.

    SELECT SINGLE * INTO @DATA(wa_zlest0007)
      FROM zlest0007
     WHERE id_interface = @zcl_romaneio=>interface_carga_sap
       AND id_ctg       = 'JSON'
       AND prefix       = 'JSON'.

    IF sy-subrc IS INITIAL.
      CREATE OBJECT arquivo.
      CONCATENATE wa_zlest0007-pathunix 'CargaIn' me->carga-id_carga '.json' INTO i_name_file.

      OPEN DATASET i_name_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
      IF sy-subrc IS INITIAL.
        TRANSFER lc_json TO i_name_file.
        CLOSE DATASET i_name_file.
      ENDIF.

      CONCATENATE wa_zlest0007-pathunix 'CargaOut' me->carga-id_carga '.json' INTO i_name_file.

      OPEN DATASET i_name_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
      IF sy-subrc IS INITIAL.
        TRANSFER json_retorno TO i_name_file.
        CLOSE DATASET i_name_file.
      ENDIF.

      CLEAR: arquivo.
    ENDIF.

    CONDENSE json_retorno.

    IF json_retorno(1) = '['.
      DATA(qtd_integer) =  strlen( json_retorno ).
      qtd_integer = qtd_integer - 2.
      json_retorno = json_retorno+1(qtd_integer).
    ENDIF.

    DATA(json_retorno_teste) = json_retorno.
    CONDENSE json_retorno_teste NO-GAPS.

    IF json_retorno_teste EQ '[]' OR json_retorno_teste IS INITIAL.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_sem_retorno_webservice-msgid
                            msgno = zcx_carga=>zcx_sem_retorno_webservice-msgno
                            attr1 = CONV #( lc_uri ) )
          msgid  = zcx_carga=>zcx_sem_retorno_webservice-msgid
          msgno  = zcx_carga=>zcx_sem_retorno_webservice-msgno
          msgv1  = CONV #( lc_uri ).
    ENDIF.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = json_retorno
      CHANGING
        data = lc_retorno.

    IF lc_retorno IS INITIAL.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_sem_retorno_webservice-msgid
                            msgno = zcx_carga=>zcx_sem_retorno_webservice-msgno
                            attr1 = CONV #( lc_uri ) )
          msgid  = zcx_carga=>zcx_sem_retorno_webservice-msgid
          msgno  = zcx_carga=>zcx_sem_retorno_webservice-msgno
          msgv1  = CONV #( lc_uri ).
    ENDIF.

    CONDENSE lc_retorno-message.
    IF lc_retorno-message IS NOT INITIAL.
      CONCATENATE 'OPUS: ' lc_retorno-message INTO lc_retorno-message SEPARATED BY space.
      me->zif_carga~gera_erro_geral( EXPORTING i_texto = lc_retorno-message ).
    ENDIF.

    "Atualiza Processo de Entrada
    LOOP AT lc_retorno-entrada INTO DATA(wa_entrada_ret).
      CONDENSE wa_entrada_ret-mensagem.
      IF wa_entrada_ret-mensagem IS NOT INITIAL.
        CONCATENATE 'OPUS: ' wa_entrada_ret-mensagem INTO wa_entrada_ret-mensagem SEPARATED BY space.
        me->zif_carga~gera_erro_geral( EXPORTING i_texto = wa_entrada_ret-mensagem ).
      ENDIF.
    ENDLOOP.

    "Atualiza Processo de Saída
    LOOP AT lc_retorno-saida INTO DATA(wa_saida_ret).
      CONDENSE wa_saida_ret-mensagem.
      IF wa_saida_ret-mensagem IS NOT INITIAL.
        CONCATENATE 'OPUS: ' wa_saida_ret-mensagem INTO wa_saida_ret-mensagem SEPARATED BY space.
        me->zif_carga~gera_erro_geral( EXPORTING i_texto = wa_saida_ret-mensagem ).
      ENDIF.
    ENDLOOP.

    IF me->zif_carga~ck_executar_manutencao_entrada EQ abap_false.
      "Atualiza Processo de Entrada
      LOOP AT lc_retorno-entrada INTO wa_entrada_ret.

        "Localiza Nota Fiscal de Entrada

*        lc_docnum = wa_entrada_ret-processamento-nudocumentosap.

*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = lc_docnum
*          IMPORTING
*            output = lc_docnum.



        lc_nr_romaneio_nt = wa_entrada_ret-numeroromaneio. "Localiza Nota Fiscal de Entrada pelo numero do romaneio

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lc_nr_romaneio_nt
          IMPORTING
            output = lc_nr_romaneio_nt.


        READ TABLE me->documento_fiscal WITH KEY nr_romaneio_ent = lc_nr_romaneio_nt ASSIGNING FIELD-SYMBOL(<documento_fiscal>).

        IF wa_entrada_ret-seqplafiscal IS NOT INITIAL AND wa_entrada_ret-seqplafiscal NE '0'.

          "Atualizar Movimento de Entrada
          UPDATE zmmt_ee_zgr
             SET obj_key = wa_entrada_ret-seqplafiscal
           WHERE obj_key  EQ <documento_fiscal>-obj_key_entrada
             AND id_carga EQ me->carga-id_carga.

          IF sy-subrc IS INITIAL.

            "Atualizar Movimento de Entrada Documentos
            UPDATE zmmt_eeimp_zgr
               SET obj_key = wa_entrada_ret-seqplafiscal
             WHERE obj_key = <documento_fiscal>-obj_key_entrada.

            "Atualizar Movimento de Entrada Documentos
            UPDATE zmmt_ee_zgr_docs
               SET obj_key = wa_entrada_ret-seqplafiscal
             WHERE obj_key = <documento_fiscal>-obj_key_entrada.

          ENDIF.

          "Atualizar Notas Fiscais da Carga
          <documento_fiscal>-obj_key_entrada = wa_entrada_ret-seqplafiscal.

        ENDIF.

        IF wa_entrada_ret-seqplaromaneio IS NOT INITIAL AND wa_entrada_ret-seqplaromaneio NE '0'.

          "Eliminar Romaneio de Entrada que foi gerado pelo ZMM0127
          SELECT SINGLE * INTO @DATA(wa_zsdt0001)
            FROM zsdt0001
           WHERE ch_referencia EQ @wa_entrada_ret-seqplaromaneio
             AND id_carga      NE @me->carga-id_carga.

          IF sy-subrc IS INITIAL.

            "Apaga Romaneio "Ficticio"
            DELETE FROM zsdt0001
             WHERE ch_referencia EQ <documento_fiscal>-ch_referencia_ent
               AND id_carga      EQ me->carga-id_carga.

            "Atualizar Romaneio de Entrada Gerado Pelo OPUS com Id Carga e Id Nota
            UPDATE zsdt0001
               SET id_carga = <documento_fiscal>-id_carga
                   id_nota  = <documento_fiscal>-id_nota
             WHERE ch_referencia EQ wa_entrada_ret-seqplaromaneio
               AND id_carga      NE me->carga-id_carga.

          ELSE.

            "Atualizar Movimento de Entrada com chave de referencia do romaneio
            UPDATE zmmt_ee_zgr
               SET ch_referencia = wa_entrada_ret-seqplaromaneio
             WHERE obj_key  EQ <documento_fiscal>-obj_key_entrada
               AND id_carga EQ me->carga-id_carga.

            "Atualizar Romaneio de Entrada "Ficticio" p/ Romaneio Real
            UPDATE zsdt0001
               SET ch_referencia = wa_entrada_ret-seqplaromaneio
             WHERE ch_referencia EQ <documento_fiscal>-ch_referencia_ent
               AND id_carga      EQ me->carga-id_carga.

            "Atualizar Romaneio de Entrada "Ficticio" p/ Romaneio Real
            UPDATE zsdt0001_item
               SET ch_referencia = wa_entrada_ret-seqplaromaneio
             WHERE ch_referencia EQ <documento_fiscal>-ch_referencia_ent
               AND id_carga      EQ me->carga-id_carga.

            UPDATE zsdt0001_item_fd
               SET ch_referencia = wa_entrada_ret-seqplaromaneio
             WHERE ch_referencia EQ <documento_fiscal>-ch_referencia_ent.

          ENDIF.
          "Atualizar Notas Fiscais da Carga
          <documento_fiscal>-ch_referencia_ent = wa_entrada_ret-seqplaromaneio.
        ENDIF.

      ENDLOOP.

      "
      DATA(erro_romaneio_gerar) = abap_false.

      "Atualiza Processo de Saída
      LOOP AT lc_retorno-saida INTO wa_saida_ret.

        IF wa_saida_ret-seqplaromaneio IS INITIAL OR wa_saida_ret-seqplaromaneio EQ '0'.
          erro_romaneio_gerar = abap_true.
        ELSEIF wa_saida_ret-seqplaromaneio IS NOT INITIAL AND wa_saida_ret-seqplaromaneio NE '0'.

          lc_nr_romaneio_sai = wa_saida_ret-numeroromaneio.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lc_nr_romaneio_sai
            IMPORTING
              output = lc_nr_romaneio_sai.

          LOOP AT me->zif_carga~ordem_venda ASSIGNING FIELD-SYMBOL(<ordem_venda>) WHERE nr_romaneio_sai EQ lc_nr_romaneio_sai.

            "Eliminar Romaneio de Saída que foi gerado pela ZMM0127
            SELECT SINGLE * INTO wa_zsdt0001
              FROM zsdt0001
             WHERE ch_referencia EQ wa_saida_ret-seqplaromaneio
               AND id_carga      NE me->carga-id_carga.

            IF sy-subrc IS INITIAL.
              "Deleta registro "Ficticio"
              DELETE FROM zsdt0001
               WHERE ch_referencia EQ <ordem_venda>-ch_referencia_sai
                 AND id_carga      EQ me->carga-id_carga.

              "Atualizar Romaneio de Saída Real com Id Carga
              UPDATE zsdt0001
                 SET id_carga = <ordem_venda>-id_carga
               WHERE ch_referencia EQ wa_saida_ret-seqplaromaneio
                 AND id_carga      NE me->carga-id_carga.

              UPDATE zsdt0001_item
                 SET id_carga = <ordem_venda>-id_carga
               WHERE ch_referencia EQ wa_saida_ret-seqplaromaneio
                 AND id_carga      NE me->carga-id_carga.

            ELSE.
              "Atualizar Romaneio de Saída
              UPDATE zsdt0001
                 SET ch_referencia = wa_saida_ret-seqplaromaneio
               WHERE ch_referencia EQ <ordem_venda>-ch_referencia_sai
                 AND id_carga      EQ me->carga-id_carga.

              UPDATE zsdt0001_item
                 SET ch_referencia = wa_saida_ret-seqplaromaneio
               WHERE ch_referencia EQ <ordem_venda>-ch_referencia_sai
                 AND id_carga      EQ me->carga-id_carga.

              UPDATE zsdt0001_item_fd
                 SET ch_referencia = wa_saida_ret-seqplaromaneio
               WHERE ch_referencia EQ <ordem_venda>-ch_referencia_sai.

            ENDIF.

            "Atualizar Notas Fiscais da Carga
            <ordem_venda>-ch_referencia_sai = wa_saida_ret-seqplaromaneio.
          ENDLOOP.

          LOOP AT me->zif_carga~pedido_compra ASSIGNING FIELD-SYMBOL(<pedido_compra>) WHERE nr_romaneio_sai EQ lc_nr_romaneio_sai.

            "Eliminar Romaneio de Saída que foi gerado pela ZMM0127
            SELECT SINGLE * INTO wa_zsdt0001
              FROM zsdt0001
             WHERE ch_referencia EQ wa_saida_ret-seqplaromaneio
               AND id_carga      NE me->carga-id_carga.

            IF sy-subrc IS INITIAL.
              "Apaga Romaneio "Ficticio"
              DELETE FROM zsdt0001
               WHERE ch_referencia EQ <pedido_compra>-ch_referencia_sai
                 AND id_carga      EQ me->carga-id_carga.

              "Atualiza Romaneio Real com a Carga
              UPDATE zsdt0001
                 SET id_carga = <pedido_compra>-id_carga
               WHERE ch_referencia EQ wa_saida_ret-seqplaromaneio
                 AND id_carga      NE me->carga-id_carga.

              "Atualiza Romaneio Real com a Carga
              UPDATE zsdt0001_item
                 SET id_carga = <pedido_compra>-id_carga
               WHERE ch_referencia EQ wa_saida_ret-seqplaromaneio
                 AND id_carga      NE me->carga-id_carga.

            ELSE.
              "Atualizar Romaneio de Saída "Ficticio" para real
              UPDATE zsdt0001
                 SET ch_referencia = wa_saida_ret-seqplaromaneio
               WHERE ch_referencia EQ <pedido_compra>-ch_referencia_sai
                 AND id_carga      EQ me->carga-id_carga.

              UPDATE zsdt0001_item
                 SET ch_referencia = wa_saida_ret-seqplaromaneio
               WHERE ch_referencia EQ <pedido_compra>-ch_referencia_sai
                 AND id_carga      EQ me->carga-id_carga.

              UPDATE zsdt0001_item_fd
                 SET ch_referencia = wa_saida_ret-seqplaromaneio
               WHERE ch_referencia EQ <pedido_compra>-ch_referencia_sai.
            ENDIF.

            "Atualizar Notas Fiscais da Carga
            <pedido_compra>-ch_referencia_sai = wa_saida_ret-seqplaromaneio.
          ENDLOOP.

        ENDIF.

      ENDLOOP.

      IF erro_romaneio_gerar EQ abap_true.
        me->zif_carga~gera_erro_geral( EXPORTING i_texto = 'OPUS: Erro ao gerar Romaneio de Saída' ).
      ENDIF.

    ELSE.

      LOOP AT lc_retorno-entrada INTO wa_entrada_ret.

        "Localiza Nota Fiscal de Entrada
*        lc_docnum = wa_entrada_ret-processamento-nudocumentosap.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = lc_docnum
*          IMPORTING
*            output = lc_docnum.

        lc_nr_romaneio_nt = wa_entrada_ret-numeroromaneio. "Localiza Nota Fiscal de Entrada pelo numero do romaneio

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lc_nr_romaneio_nt
          IMPORTING
            output = lc_nr_romaneio_nt.

        READ TABLE me->documento_fiscal WITH KEY nr_romaneio_ent = lc_nr_romaneio_nt ASSIGNING <documento_fiscal>.

        IF wa_entrada_ret-seqplafiscal IS NOT INITIAL AND wa_entrada_ret-seqplafiscal NE '0'.

          "Atualizar Movimento de Entrada
          UPDATE zmmt_ee_zgr
             SET obj_key       = wa_entrada_ret-seqplafiscal
                 ch_referencia = wa_entrada_ret-seqplaromaneio
           WHERE obj_key  EQ <documento_fiscal>-obj_key_entrada
             AND id_carga EQ me->carga-id_carga.

          IF sy-subrc IS INITIAL.

            "Atualizar Movimento de Entrada Documentos
            UPDATE zmmt_eeimp_zgr
               SET obj_key = wa_entrada_ret-seqplafiscal
             WHERE obj_key = <documento_fiscal>-obj_key_entrada.

            "Atualizar Movimento de Entrada Documentos
            UPDATE zmmt_ee_zgr_docs
               SET obj_key = wa_entrada_ret-seqplafiscal
             WHERE obj_key = <documento_fiscal>-obj_key_entrada.

          ENDIF.

          "Atualizar Notas Fiscais da Carga
          <documento_fiscal>-obj_key_entrada = wa_entrada_ret-seqplafiscal.

        ENDIF.

        IF wa_entrada_ret-seqplaromaneio IS NOT INITIAL AND wa_entrada_ret-seqplaromaneio NE '0'.

          "Eliminar Romaneio de Entrada que foi gerado pelo ZMM0127
          SELECT SINGLE * INTO @wa_zsdt0001
            FROM zsdt0001
           WHERE ch_referencia EQ @wa_entrada_ret-seqplaromaneio
             AND id_carga      NE @me->carga-id_carga.

          IF sy-subrc IS INITIAL.

            "Apaga Romaneio "Ficticio"
            DELETE FROM zsdt0001
             WHERE ch_referencia EQ <documento_fiscal>-ch_referencia_ent
               AND id_carga      EQ me->carga-id_carga.

            "Atualizar Romaneio de Entrada Gerado Pelo OPUS com Id Carga e Id Nota
            UPDATE zsdt0001
               SET id_carga = <documento_fiscal>-id_carga
                   id_nota  = <documento_fiscal>-id_nota
             WHERE ch_referencia EQ wa_entrada_ret-seqplaromaneio
               AND id_carga      NE me->carga-id_carga.

            UPDATE zsdt0001_item
               SET id_carga = <documento_fiscal>-id_carga
                   id_nota  = <documento_fiscal>-id_nota
             WHERE ch_referencia EQ wa_entrada_ret-seqplaromaneio
               AND id_carga      NE me->carga-id_carga.

          ELSE.

            "Atualizar Movimento de Entrada com chave de referencia do romaneio
            UPDATE zmmt_ee_zgr
               SET ch_referencia = wa_entrada_ret-seqplaromaneio
             WHERE obj_key  EQ <documento_fiscal>-obj_key_entrada
               AND id_carga EQ me->carga-id_carga.

            "Atualizar Romaneio de Entrada "Ficticio" p/ Romaneio Real
            UPDATE zsdt0001
               SET ch_referencia = wa_entrada_ret-seqplaromaneio
             WHERE ch_referencia EQ <documento_fiscal>-ch_referencia_ent
               AND id_carga      EQ me->carga-id_carga.

            "Atualizar Romaneio de Entrada "Ficticio" p/ Romaneio Real
            UPDATE zsdt0001_item
               SET ch_referencia = wa_entrada_ret-seqplaromaneio
             WHERE ch_referencia EQ <documento_fiscal>-ch_referencia_ent
               AND id_carga      EQ me->carga-id_carga.

            UPDATE zsdt0001_item_fd
               SET ch_referencia = wa_entrada_ret-seqplaromaneio
             WHERE ch_referencia EQ <documento_fiscal>-ch_referencia_ent.

          ENDIF.
          "Atualizar Notas Fiscais da Carga
          <documento_fiscal>-ch_referencia_ent = wa_entrada_ret-seqplaromaneio.
        ENDIF.

      ENDLOOP.

    ENDIF.

    COMMIT WORK.

    "Alimenta Fila Notas 1x1 - SD - Ajuste Insert Fila 1x1 - Processo Fob US 136393 - WPP - Ini
    LOOP AT me->documento_fiscal INTO DATA(lwa_doc_fiscal) WHERE docnum is NOT INITIAL.
      SUBMIT zsdr0012 WITH s_docnum-low = lwa_doc_fiscal-docnum AND RETURN.
    ENDLOOP.
    "Alimenta Fila Notas 1x1 - SD - Ajuste Insert Fila 1x1 - Processo Fob US 136393 - WPP - Fim


    me->zif_carga~ck_alterou = abap_true.

  ENDMETHOD.


  METHOD ZIF_CARGA~SEND_ESTORNO_CARGA_TO_OPUS.

    DATA: LC_JSON     TYPE STRING,
          I_NAME_FILE TYPE STRING,
          LC_RETORNO  TYPE ZDE_OPUS_CARGA.

    DEFINE ADD_TAG.
      CONCATENATE LC_JSON '"' &1 '": "' &2 '"' &3 INTO LC_JSON.
    END-OF-DEFINITION.

    R_INSTANCIA = ME.

    CHECK ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_FALSE.
    CHECK ME->ZIF_CARGA~CK_EXECUTAR_MANUTENCAO_ENTRADA EQ ABAP_FALSE.

    ME->ZIF_CARGA~GET_ROMANEIO_ENTRADA(
      EXPORTING
        I_ID_CARGA = ME->ZIF_CARGA~CARGA-ID_CARGA
      IMPORTING
       E_ROMANEIOS = DATA(E_ROMANEIOS) ).

    IF ( E_ROMANEIOS[] IS INITIAL ) AND ( ME->CARGA-TP_STATUS EQ 'CO' ). "Situações onde o Romaneio esta conferido, mas os romaneios já não existem mais no OPUS e SAP.
      ME->CARGA-TP_STATUS = ZIF_CARGA=>ST_STATUS_FECHADO.
      EXIT.
    ENDIF.

    DESCRIBE TABLE E_ROMANEIOS LINES DATA(LC_QTD_LINHAS).

    CONCATENATE '{' LC_JSON INTO LC_JSON.

    ADD_TAG 'centro'      ME->CARGA-ID_BRANCH  ','.
    ADD_TAG 'safra'       ME->CARGA-NR_SAFRA   ','.

    CONCATENATE LC_JSON '"romaneioEntrada": [' INTO LC_JSON.
    LOOP AT E_ROMANEIOS INTO DATA(WA_ENTRADA).

      DATA(LC_TABIX) = SY-TABIX.

      CONCATENATE LC_JSON '{' INTO LC_JSON.
      ADD_TAG 'numeroRomaneio' WA_ENTRADA-NR_ROMANEIO ''.

      "Final do Romaneio de Entrada
      IF LC_TABIX NE LC_QTD_LINHAS.
        CONCATENATE LC_JSON '},' INTO LC_JSON.
      ELSE.
        CONCATENATE LC_JSON '}' INTO LC_JSON.
      ENDIF.
    ENDLOOP.
    "Fim Romaneio de Entrada
    CONCATENATE LC_JSON ']' INTO LC_JSON.

    "Fim Arquivo
    CONCATENATE LC_JSON '}' INTO LC_JSON.

    DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.

    CREATE OBJECT OB_WEB_SERVICE.

    OB_WEB_SERVICE->ZIF_WEBSERVICE~AUTENTICA_OPUS = ABAP_TRUE.

    TRY .
        OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'IC' ).
      CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
    ENDTRY.

    OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'O' ).

    TRY .
        DATA(VAR_HTTP) = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
        DATA(LC_URI)   = OB_WEB_SERVICE->GET_URI( ).
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
    ENDTRY.

    OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP ).

    CALL METHOD OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR
      EXPORTING
        I_HTTP                     = VAR_HTTP
        I_XML                      = LC_JSON
      RECEIVING
        E_RESULTADO                = DATA(JSON_RETORNO)
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    DATA: ARQUIVO TYPE REF TO ZCL_ARQUIVO.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0007)
      FROM ZLEST0007
     WHERE ID_INTERFACE = @ZCL_ROMANEIO=>INTERFACE_CARGA_SAP
       AND ID_CTG       = 'JSON'
       AND PREFIX       = 'JSON'.

    IF SY-SUBRC IS INITIAL.
      CREATE OBJECT ARQUIVO.
      CONCATENATE WA_ZLEST0007-PATHUNIX 'CargaCancelIn' ME->CARGA-ID_CARGA '.json' INTO I_NAME_FILE.

      OPEN DATASET I_NAME_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
      IF SY-SUBRC IS INITIAL.
        TRANSFER LC_JSON TO I_NAME_FILE.
        CLOSE DATASET I_NAME_FILE.
      ENDIF.

      CONCATENATE WA_ZLEST0007-PATHUNIX 'CargaCancelOut' ME->CARGA-ID_CARGA '.json' INTO I_NAME_FILE.

      OPEN DATASET I_NAME_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
      IF SY-SUBRC IS INITIAL.
        TRANSFER JSON_RETORNO TO I_NAME_FILE.
        CLOSE DATASET I_NAME_FILE.
      ENDIF.

      CLEAR: ARQUIVO.
    ENDIF.

    CONDENSE JSON_RETORNO.

    IF JSON_RETORNO(1) = '['.
      DATA(QTD_INTEGER) =  STRLEN( JSON_RETORNO ).
      QTD_INTEGER = QTD_INTEGER - 2.
      JSON_RETORNO = JSON_RETORNO+1(QTD_INTEGER).
    ENDIF.

    DATA(JSON_RETORNO_TESTE) = JSON_RETORNO.
    CONDENSE JSON_RETORNO_TESTE NO-GAPS.

    IF JSON_RETORNO_TESTE EQ '[]' OR JSON_RETORNO_TESTE IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_SEM_RETORNO_WEBSERVICE-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_SEM_RETORNO_WEBSERVICE-MSGNO
                            ATTR1 = CONV #( LC_URI ) )
          MSGID  = ZCX_CARGA=>ZCX_SEM_RETORNO_WEBSERVICE-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_SEM_RETORNO_WEBSERVICE-MSGNO
          MSGV1  = CONV #( LC_URI ).
    ENDIF.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = JSON_RETORNO
      CHANGING
        DATA = LC_RETORNO.

    IF LC_RETORNO IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_SEM_RETORNO_WEBSERVICE-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_SEM_RETORNO_WEBSERVICE-MSGNO
                            ATTR1 = CONV #( LC_URI ) )
          MSGID  = ZCX_CARGA=>ZCX_SEM_RETORNO_WEBSERVICE-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_SEM_RETORNO_WEBSERVICE-MSGNO
          MSGV1  = CONV #( LC_URI ).
    ENDIF.

    CONDENSE LC_RETORNO-MESSAGE.
    IF LC_RETORNO-MESSAGE IS NOT INITIAL.
      CONCATENATE 'OPUS: ' LC_RETORNO-MESSAGE INTO LC_RETORNO-MESSAGE SEPARATED BY SPACE.
      ME->ZIF_CARGA~GERA_ERRO_GERAL( EXPORTING I_TEXTO = LC_RETORNO-MESSAGE ).
    ENDIF.

    "Atualiza Processo de Entrada
    LOOP AT LC_RETORNO-ENTRADA INTO DATA(WA_ENTRADA_RET).
      CONDENSE WA_ENTRADA_RET-MENSAGEM.
      IF WA_ENTRADA_RET-MENSAGEM IS NOT INITIAL.
        CONCATENATE 'OPUS: ' WA_ENTRADA_RET-MENSAGEM INTO WA_ENTRADA_RET-MENSAGEM SEPARATED BY SPACE.
        ME->ZIF_CARGA~GERA_ERRO_GERAL( EXPORTING I_TEXTO = WA_ENTRADA_RET-MENSAGEM ).
      ENDIF.
    ENDLOOP.

    ME->CARGA-TP_STATUS = ZIF_CARGA=>ST_STATUS_FECHADO.

  ENDMETHOD.


  METHOD zif_carga~set_abrir.

    DATA: i_texto     TYPE string,
          ob_romaneio TYPE REF TO zcl_romaneio.

    r_carga = me.

    me->carga-tp_produto_carga                 = i_tipo_produto. "*-CS2022000332-#78064-07.06.2022-JT
    me->zif_carga~ck_executar_reversao_entrada = abap_true. "BUG 34154

    CHECK NOT ( me->carga-ck_enviado_opus EQ abap_true AND me->carga-ck_recebido_opus EQ abap_false ).

    IF me->carga-id_carga IS NOT INITIAL AND me->zif_carga~at_manutencao EQ abap_false.

*-CS2021000183-#83755-19.07.2022-JT -inicio-Comentado
*-CS2021000183 - 05.04.2022 - JT - inicio
*     TRY.
*         me->zif_carga~set_cancelar_nota_propria( ).
*       CATCH zcx_error INTO DATA(ex_error).
*     ENDTRY.
*-CS2021000183 - 05.04.2022 - JT - fim
*-CS2021000183-#83755-19.07.2022-JT -fim-Comentado

      " Verificar se romaneios excluidos estão CANCELADOS 01/03/2024
      me->zif_carga~check_romaneios_carga_opus( EXPORTING i_id_carga  = me->carga-id_carga ).

      "Exclui Frete Entrada
      me->zif_carga~set_estorna_frete_entrada( ).

      "Veririca se Pode Excluir Romaneio de Saída
      me->zif_carga~get_ck_excluir_romaneio_saida( i_ck_opus = abap_true ).

      me->zif_carga~get_romaneio_saida( EXPORTING i_id_carga  = me->carga-id_carga IMPORTING e_romaneios = DATA(it_romaneio) ).

      LOOP AT it_romaneio INTO DATA(wa_romaneio).
        CREATE OBJECT ob_romaneio.
        ob_romaneio->zif_cadastro~set_registro( i_id_registro = wa_romaneio-ch_referencia ).
        ob_romaneio->zif_cadastro~excluir_registro( ).
        CLEAR ob_romaneio.
      ENDLOOP.

      me->zif_carga~get_romaneio_entrada( EXPORTING i_id_carga  = me->carga-id_carga IMPORTING e_romaneios = DATA(it_romaneio_entrada) ).

      IF it_romaneio_entrada[] IS NOT INITIAL.
        "Verifica se existe entrada para estornar
        TRY .
            me->zif_carga~get_recuperar_entrada(
            )->set_processar_estorno_sobra(
            )->set_gerar_estorno_estoque(
            )->set_processar_estorno( IMPORTING e_estornou = DATA(e_estornou)
            )->set_cancelar_nota_propria(  "CS2021000183-#83755-19.07.2022-JT
            ).

          CATCH zcx_job INTO DATA(ex_job).

            MESSAGE ID ex_job->msgid TYPE 'S'
             NUMBER ex_job->msgno
               WITH ex_job->msgv1 ex_job->msgv2 ex_job->msgv3 ex_job->msgv4
               INTO i_texto.

            me->gera_erro_geral( i_texto = i_texto ).

          CATCH zcx_pedido_compra_exception INTO DATA(ex_pedido).

            MESSAGE ID ex_pedido->msgid TYPE 'S'
             NUMBER ex_pedido->msgno
               WITH ex_pedido->msgv1 ex_pedido->msgv2 ex_pedido->msgv3 ex_pedido->msgv4
               INTO i_texto.

            me->gera_erro_geral( i_texto = i_texto ).

          CATCH zcx_cadastro INTO DATA(ex_cadastro).

            MESSAGE ID ex_cadastro->msgid TYPE 'S'
             NUMBER ex_cadastro->msgno
               WITH ex_cadastro->msgv1 ex_cadastro->msgv2 ex_cadastro->msgv3 ex_cadastro->msgv4
               INTO i_texto.

            me->gera_erro_geral( i_texto = i_texto ).

        ENDTRY.
        CHECK e_estornou EQ abap_true.
      ENDIF.

    ENDIF.

    CASE me->carga-tp_status.
      WHEN zif_carga=>st_status_aberto.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_carga_aberta-msgid msgno = zcx_carga=>zcx_carga_aberta-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_aberta-msgid
            msgno  = zcx_carga=>zcx_carga_aberta-msgno.
      WHEN zif_carga=>st_status_conferido.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_carga_conferida-msgid msgno = zcx_carga=>zcx_carga_conferida-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_conferida-msgid
            msgno  = zcx_carga=>zcx_carga_conferida-msgno.
      WHEN zif_carga=>st_status_cancelada.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_carga_cancelada-msgid msgno = zcx_carga=>zcx_carga_cancelada-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_cancelada-msgid
            msgno  = zcx_carga=>zcx_carga_cancelada-msgno.
    ENDCASE.
*CS2021000183 Parte 1 - Classificação automática - US 74975 - BG - INICIO
    "    COMO O PARAMETRO E_CARGA_RECEBIMENTO É PREENCHIDO NO METODO NOVO_REGISTRO NÃO PODE LIMPAR AS INFORMAÇÕES.
    " PARAMETRO JA VEM COM AS INFORMAÇÕES DE CLASSIFICAÇÃO
    "CLEAR: E_CARGA_RECEBIMENTO.
*CS2021000183 Parte 1 - Classificação automática - US 74975 - BG - FIM

    IF me->carga-id_carga IS NOT INITIAL.
      "077  Carga Está Fechada deseja reabrir a carga?
      IF me->zif_carga~at_manutencao EQ abap_false.
        MESSAGE w077.
        "ELSE.
        "  MESSAGE W180.
      ENDIF.
      me->get_info_alv_apresentacao( IMPORTING e_apresentacao = DATA(e_carga) ).
      e_carga_recebimento = e_carga-carga.
    ENDIF.

    me->zif_carga~set_cancelar_solic_manut( ).

    me->carga-dt_abertura    = sy-datlo.
    me->carga-hr_abertura    = sy-timlo.
    me->carga-us_abertura    = sy-uname.
    me->carga-tp_status      = zif_carga=>st_status_aberto.

    CLEAR:
    me->carga-dt_movimento,
    me->carga-dt_fechamento,
    me->carga-hr_fechamento,
    me->carga-us_fechamento,
    me->carga-dt_conferencia,
    me->carga-hr_conferencia,
    me->carga-us_conferencia.

    e_carga_recebimento-dt_abertura    = me->carga-dt_abertura.
    e_carga_recebimento-hr_abertura    = me->carga-hr_abertura.
    e_carga_recebimento-us_abertura    = me->carga-us_abertura.
    e_carga_recebimento-tp_status      = me->carga-tp_status.
    e_carga_recebimento-dt_fechamento  = me->carga-dt_fechamento.
    e_carga_recebimento-hr_fechamento  = me->carga-hr_fechamento.
    e_carga_recebimento-us_fechamento  = me->carga-us_fechamento.
    e_carga_recebimento-dt_conferencia = me->carga-dt_conferencia.
    e_carga_recebimento-hr_conferencia = me->carga-hr_conferencia.
    e_carga_recebimento-us_conferencia = me->carga-us_conferencia.
    e_carga_recebimento-dt_movimento   = me->carga-dt_movimento.
    me->ck_alterou = abap_true.

*076  Carga Foi Reaberta com Sucesso!

    IF me->carga-id_carga IS NOT INITIAL AND me->zif_carga~at_manutencao EQ abap_false.

      me->validar_registro( IMPORTING e_validou = DATA(e_validou) ).

      IF me->carga-id_ordem IS NOT INITIAL AND e_validou EQ abap_true AND me->zif_carga~at_manutencao EQ abap_false.
        zcl_ordem_carregamento=>set_abrir( i_id_ordem = me->carga-id_ordem ).
      ENDIF.

      me->gravar_registro( IMPORTING e_gravou  = DATA(e_gravou) ).

      IF NOT e_gravou EQ abap_true.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = sy-msgid msgno = sy-msgno
                              attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
            msgty  = 'E'
            msgid  = sy-msgid
            msgno  = sy-msgno
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4.
      ELSE.
        MESSAGE s076.
      ENDIF.

    ENDIF.

    CHECK me->carga-id_carga IS INITIAL.

    me->carga-nr_safra  = i_nr_safra.
    me->carga-id_bukrs  = i_id_bukrs.
    me->carga-id_branch = i_id_branch.

    "Tipo do Teste GMO Amaggi
    me->classificacao-in_gmo         = zif_carga=>st_gmo_nao_testado.
    "Tipo do Teste GMO - Tira Amaggi 0,3%.
    me->classificacao-in_gmo_03      = zif_carga=>st_gmo_nao_testado.
    "Tipo do Teste GMO - Tira Monsanto 5%.
    me->classificacao-in_teste_srr   = zif_carga=>st_gmo_nao_testado.
    "Tipo do Teste RR2 - Monsanto.
    me->classificacao-in_teste_srr_2 = zif_carga=>st_gmo_nao_testado.

    me->classificacao-nr_resultado_01 = 0.
    me->classificacao-nr_resultado_02 = 0.
    me->classificacao-nr_res_rr1_rr2  = 0.

    e_carga_recebimento-nr_safra    = me->carga-nr_safra.
    e_carga_recebimento-id_bukrs    = me->carga-id_bukrs.
    e_carga_recebimento-id_branch   = me->carga-id_branch.

    SELECT SINGLE name INTO e_carga_recebimento-name
      FROM j_1bbranch
     WHERE bukrs  EQ e_carga_recebimento-id_bukrs
       AND branch EQ e_carga_recebimento-id_branch.

    e_carga_recebimento-in_gmo          = me->classificacao-in_gmo.
    e_carga_recebimento-in_gmo_03       = me->classificacao-in_gmo_03.
    e_carga_recebimento-in_teste_srr    = me->classificacao-in_teste_srr.
    e_carga_recebimento-in_teste_srr_2  = me->classificacao-in_teste_srr_2.
    e_carga_recebimento-nr_resultado_01 = me->classificacao-nr_resultado_01.
    e_carga_recebimento-nr_resultado_02 = me->classificacao-nr_resultado_02.
    e_carga_recebimento-nr_res_rr1_rr2  = me->classificacao-nr_res_rr1_rr2.

    CHECK me->carga-id_carga IS INITIAL.
    CHECK me->carga-id_carga IS INITIAL.
    e_carga_recebimento-tp_frete = me->carga-tp_frete.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_ACEITE_SOLI_MANUTENCAO.

    DATA: ROMANEIO TYPE REF TO ZCL_ROMANEIO.

    CLEAR: E_MENSSAGEM_RETORNO.

    R_CARGA = ME.

    CHECK ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_TRUE.

    IF ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_PROCESSOU_AJUSTE EQ ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_CARGA=>ZCX_SOL_MANUT_EXECUTADA-MSGID
                            MSGNO  = ZCX_CARGA=>ZCX_SOL_MANUT_EXECUTADA-MSGNO )
          MSGTY  = 'E'
          MSGID  = ZCX_CARGA=>ZCX_SOL_MANUT_EXECUTADA-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_SOL_MANUT_EXECUTADA-MSGNO.
    ENDIF.

    CASE I_SOMENTE_PROCESSAR.
      WHEN ABAP_FALSE.

        IF I_TP_RESPOSTA NE ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA AND
           I_TP_RESPOSTA NE ZIF_CARGA=>ST_RS_ACEITE_MANUT_RECUSADA AND
           I_TP_RESPOSTA NE 'X'.
          RAISE EXCEPTION TYPE ZCX_CARGA
            EXPORTING
              TEXTID = VALUE #( MSGID  = ZCX_CARGA=>ZCX_TIPO_RESPOSTA_MANUT_ROM-MSGID
                                MSGNO  = ZCX_CARGA=>ZCX_TIPO_RESPOSTA_MANUT_ROM-MSGNO
                                ATTR1  = CONV #( I_TP_RESPOSTA ) )
              MSGTY  = 'E'
              MSGID  = ZCX_CARGA=>ZCX_TIPO_RESPOSTA_MANUT_ROM-MSGID
              MSGNO  = ZCX_CARGA=>ZCX_TIPO_RESPOSTA_MANUT_ROM-MSGNO
              MSGV1  = CONV #( I_TP_RESPOSTA ).
        ENDIF.

        CASE I_TP_APROVACAO.
          WHEN 'XX'.
          WHEN ZIF_CARGA=>ST_TP_ACEITE_MANUT_FILIAL.
            ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL = I_TP_RESPOSTA.
            ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-DS_ACEITE_FILIAL = I_MOTIVO_RESPOSTA.
            ME->ZIF_CARGA~CK_ALTEROU = ABAP_TRUE.
          WHEN ZIF_CARGA=>ST_TP_ACEITE_MANUT_FISCAL.
            ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FISCAL = I_TP_RESPOSTA.
            ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-DS_ACEITE_FISCAL = I_MOTIVO_RESPOSTA.
            ME->ZIF_CARGA~CK_ALTEROU = ABAP_TRUE.
          WHEN ZIF_CARGA=>ST_TP_ACEITE_MANUT_COMERCIAL.
            ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_COMERCIAL = I_TP_RESPOSTA.
            ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-DS_ACEITE_COMERCIAL = I_MOTIVO_RESPOSTA.
            ME->ZIF_CARGA~CK_ALTEROU = ABAP_TRUE.
          WHEN OTHERS.
            RAISE EXCEPTION TYPE ZCX_CARGA
              EXPORTING
                TEXTID = VALUE #( MSGID  = ZCX_CARGA=>ZCX_TIPO_ACEITE_MANUT_ROM-MSGID
                                  MSGNO  = ZCX_CARGA=>ZCX_TIPO_ACEITE_MANUT_ROM-MSGNO
                                  ATTR1  = CONV #( I_TP_APROVACAO ) )
                MSGTY  = 'E'
                MSGID  = ZCX_CARGA=>ZCX_TIPO_ACEITE_MANUT_ROM-MSGID
                MSGNO  = ZCX_CARGA=>ZCX_TIPO_ACEITE_MANUT_ROM-MSGNO
                MSGV1  = CONV #( I_TP_APROVACAO ).
        ENDCASE.

        "Não grava neste ponte se foi recusado alguma aprovação
        IF NOT ( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_RECUSADA OR
                 ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_COMERCIAL EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_RECUSADA OR
                 ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FISCAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_RECUSADA ) AND
           I_TP_APROVACAO NE 'XX'.
          ME->ZIF_CARGA~CK_ALTEROU = ABAP_TRUE.
          ME->GRAVAR_REGISTRO( ).
        ENDIF.

        IF ( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA OR
             ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_NAO_GERA ) AND
           ( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_COMERCIAL EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA OR
             ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_COMERCIAL EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_NAO_GERA ) AND
           ( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FISCAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA OR
             ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FISCAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_NAO_GERA ).

          DATA: E_DATA_LIMITE TYPE DATUM.

          "Verificar Romaneios de Entrada se Podem ser alterados
          DATA(CK_PENDENCIA_COMERCIAL) = ABAP_FALSE.
          ME->ZIF_CARGA~GET_ROMANEIO_ENTRADA( EXPORTING I_ID_CARGA = ME->ZIF_CARGA~CARGA-ID_CARGA I_FORCE = ABAP_TRUE IMPORTING E_ROMANEIOS = DATA(E_ROMANEIOS) ).
          LOOP AT E_ROMANEIOS INTO DATA(WA_ROMANEIOS).
            TRY .
                ROMANEIO = ZCL_ROMANEIO=>GET_INSTANCE( ).
                ROMANEIO->SET_REGISTRO( I_ID_REGISTRO = WA_ROMANEIOS-CH_REFERENCIA ).
                ROMANEIO->GET_CONSULTA_STATUS_OPUS(
                  IMPORTING
                    E_STATUS            = DATA(E_STATUS)               " Status do Romaneio
                    E_BLOQUEADO         = DATA(E_BLOQUEADO)
                    E_MENSAGEM_BLOQUEIO = DATA(E_MENSAGEM_BLOQUEIO) ).

                IF E_BLOQUEADO = 'X'.
                  CASE I_TP_RESPOSTA.
                    WHEN 'X'.
                      ME->GERA_ERRO_GERAL( I_TEXTO = E_MENSAGEM_BLOQUEIO ).
                    WHEN OTHERS.
                      CK_PENDENCIA_COMERCIAL = ABAP_TRUE.
                  ENDCASE.
                ENDIF.

              CATCH ZCX_ROMANEIO INTO DATA(EX_ROMANEIO).

                IF EX_ROMANEIO->MSGID EQ ZCX_ROMANEIO=>ZCX_ERRO_ROM_NAO_ENCONTRADO-MSGID AND
                   EX_ROMANEIO->MSGNO EQ ZCX_ROMANEIO=>ZCX_ERRO_ROM_NAO_ENCONTRADO-MSGNO.
                  CONTINUE.
                ENDIF.

                RAISE EXCEPTION TYPE ZCX_CARGA
                  EXPORTING
                    TEXTID = VALUE #( MSGID = EX_ROMANEIO->MSGID
                                      MSGNO = EX_ROMANEIO->MSGNO
                                      ATTR1 = CONV #( EX_ROMANEIO->MSGV1 )
                                      ATTR2 = CONV #( EX_ROMANEIO->MSGV2 )
                                      ATTR3 = CONV #( EX_ROMANEIO->MSGV3 )
                                      ATTR4 = CONV #( EX_ROMANEIO->MSGV4 ) )
                    MSGID  = EX_ROMANEIO->MSGID
                    MSGNO  = EX_ROMANEIO->MSGNO
                    MSGTY  = 'E'
                    MSGV1  = EX_ROMANEIO->MSGV1
                    MSGV2  = EX_ROMANEIO->MSGV2
                    MSGV3  = EX_ROMANEIO->MSGV3
                    MSGV4  = EX_ROMANEIO->MSGV4.
            ENDTRY.
          ENDLOOP.

          IF CK_PENDENCIA_COMERCIAL EQ ABAP_FALSE.
            ME->ZIF_CARGA~SET_PROCESSAR_MANUTENCAO( ).
            "Se Processou os Documentos Aprova WorkFlow e marca como processado
            ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-TP_STATUS = ZIF_CARGA=>ST_STATUS_MANUT_APROVADO.
            ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_PROCESSOU_AJUSTE = ABAP_TRUE.
            ME->ZIF_CARGA~CK_ALTEROU = ABAP_TRUE.
            ME->GRAVAR_REGISTRO( ).
          ENDIF.

        ELSEIF ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_RECUSADA OR
               ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_COMERCIAL EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_RECUSADA OR
               ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FISCAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_RECUSADA.
          "Grava neste ponte se foi recusado alguma aprovação
          ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-TP_STATUS = ZIF_CARGA=>ST_STATUS_MANUT_RECUSADA.
          ME->ZIF_CARGA~CK_ALTEROU              = ABAP_TRUE.
          ME->GRAVAR_REGISTRO( ).
        ENDIF.

      WHEN ABAP_TRUE.

        IF ( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA OR
                     ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_NAO_GERA ) AND
                   ( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_COMERCIAL EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA OR
                     ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_COMERCIAL EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_NAO_GERA ) AND
                   ( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FISCAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA OR
                     ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FISCAL    EQ ZIF_CARGA=>ST_RS_ACEITE_MANUT_NAO_GERA ).
          ME->ZIF_CARGA~SET_PROCESSAR_MANUTENCAO( ).
          ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-TP_STATUS = ZIF_CARGA=>ST_STATUS_MANUT_APROVADO.
          ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_PROCESSOU_AJUSTE = ABAP_TRUE.
          ME->ZIF_CARGA~CK_ALTEROU = ABAP_TRUE.
          ME->GRAVAR_REGISTRO( ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_ADD_BLOCO.

    DATA: LC_BLOCO TYPE ZSDT0001FD.

    CLEAR: E_BLOCO.

    R_CARGA = ME.

    IF I_BLOCO-NR_ORDEM_VENDA IS NOT INITIAL.

      READ TABLE ME->ZIF_CARGA~ORDEM_VENDA WITH KEY NR_ORDEM_VENDA = I_BLOCO-NR_ORDEM_VENDA TRANSPORTING NO FIELDS.
      CHECK SY-SUBRC IS INITIAL.

    ELSEIF I_BLOCO-NR_PEDIDO_COMPRA IS NOT INITIAL.

      READ TABLE ME->ZIF_CARGA~PEDIDO_COMPRA WITH KEY NR_PEDIDO_COMPRA = I_BLOCO-NR_PEDIDO_COMPRA TRANSPORTING NO FIELDS.
      CHECK SY-SUBRC IS INITIAL.

    ENDIF.

    READ TABLE ME->ZIF_CARGA~BLOCOS ASSIGNING FIELD-SYMBOL(<FS_BLOCO>)
    WITH KEY NR_ORDEM_VENDA   = I_BLOCO-NR_ORDEM_VENDA
             NR_PEDIDO_COMPRA = I_BLOCO-NR_PEDIDO_COMPRA
             ZSEQ_INST        = I_BLOCO-ZSEQ_INST
             OBJEK            = I_BLOCO-OBJEK
             OBJECTTABLE      = I_BLOCO-OBJECTTABLE.

    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING I_BLOCO TO <FS_BLOCO>.
    ELSE.
      MOVE-CORRESPONDING I_BLOCO TO LC_BLOCO.
      APPEND LC_BLOCO TO ME->ZIF_CARGA~BLOCOS.
    ENDIF.

    MOVE-CORRESPONDING I_BLOCO TO E_BLOCO.

    SELECT SINGLE NAME1 INTO @E_BLOCO-DS_PONTO_C
      FROM LFA1
     WHERE LIFNR EQ @E_BLOCO-CD_PONTO_C.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_ADD_FORN_FILIAL_SAFRA.

    DATA: WA_ZSDT0001PD TYPE ZSDT0001PD.

    WA_ZSDT0001PD-ID_PRODUTOR = I_ID_PRODUTOR.
    WA_ZSDT0001PD-NR_SAFRA    = I_ID_NR_SAFRA.
    WA_ZSDT0001PD-ID_BUKRS    = I_ID_BUKRS.
    WA_ZSDT0001PD-ID_BRANCH   = I_ID_BRANCH.

    TRY .
        ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
          )->SET_PARCEIRO( I_PARCEIRO = I_ID_PRODUTOR
          )->GET_ID_PARCEIRO( IMPORTING E_PARCEIRO = WA_ZSDT0001PD-ID_PRODUTOR
          )->CK_ATIVO(
          )->CK_ATIVO_EMPRESA( I_EMPRESA = WA_ZSDT0001PD-ID_BUKRS
          )->CK_RESTRICAO_EMBARGO(
          ).

        INSERT INTO ZSDT0001PD VALUES WA_ZSDT0001PD.
        COMMIT WORK.

      CATCH ZCX_PARCEIROS INTO DATA(EX_PARCEIROS).

        RAISE EXCEPTION TYPE ZCX_PARCEIROS
          EXPORTING
            TEXTID = VALUE #( MSGID = EX_PARCEIROS->MSGID
                              MSGNO = EX_PARCEIROS->MSGNO
                              ATTR1 = EX_PARCEIROS->MSGV1
                              ATTR2 = EX_PARCEIROS->MSGV2
                              ATTR3 = EX_PARCEIROS->MSGV3
                              ATTR4 = EX_PARCEIROS->MSGV4 )
            MSGID  = EX_PARCEIROS->MSGID
            MSGNO  = EX_PARCEIROS->MSGNO
            MSGTY  = 'E'
            MSGV1  = EX_PARCEIROS->MSGV1
            MSGV2  = EX_PARCEIROS->MSGV2
            MSGV3  = EX_PARCEIROS->MSGV3
            MSGV4  = EX_PARCEIROS->MSGV4.

    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_ADD_TAKE_UP.

    DATA: LC_TAKEUP TYPE ZSDT0001TK.

    CLEAR: E_TAKEUP.

    R_CARGA = ME.

    READ TABLE ME->DOCUMENTO_FISCAL WITH KEY ID_NOTA = I_TAKEUP-ID_NOTA INTO DATA(WA_NOTA).

    CHECK SY-SUBRC IS INITIAL.

    READ TABLE ME->ZIF_CARGA~TAKE_UP ASSIGNING FIELD-SYMBOL(<FS_TAKEUP>)
    WITH KEY ID_NOTA  = I_TAKEUP-ID_NOTA ID_TAKEUP = I_TAKEUP-ID_TAKEUP NU_BLOCO  = I_TAKEUP-NU_BLOCO.

    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING I_TAKEUP TO <FS_TAKEUP>.
    ELSE.
      MOVE-CORRESPONDING I_TAKEUP TO LC_TAKEUP.
      APPEND LC_TAKEUP TO ME->ZIF_CARGA~TAKE_UP.
    ENDIF.

    MOVE-CORRESPONDING I_TAKEUP TO E_TAKEUP.

    E_TAKEUP-NR_NOTA = WA_NOTA-NR_NOTA.
    E_TAKEUP-DT_EMISSAO = WA_NOTA-DT_EMISSAO.
    E_TAKEUP-NM_SERIE = WA_NOTA-NM_SERIE.
    E_TAKEUP-ID_MOD_FISCAL = WA_NOTA-ID_MOD_FISCAL.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_AGENTE_FRETE.

    R_CARGA = ME.

    CHECK ME->CARGA-TP_STATUS EQ ZIF_CARGA=>ST_STATUS_ABERTO.

    ME->CARGA-ID_AGENT_FRETE  = I_ID_AGENT_FRETE.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_AJUSTAR_RAT_DESC_GERAL.

    R_CARGA = ME.

    DESCRIBE TABLE ME->DOCUMENTO_FISCAL LINES DATA(QTD_NOTAS).

    DATA(LC_PESO_LIQUIDO) = ME->CARGA-NM_PESO_LIQUIDO.

    IF QTD_NOTAS GT 1.

      SORT ME->DOCUMENTO_FISCAL BY NR_QUANTIDADE.

      LOOP AT ME->DOCUMENTO_FISCAL INTO DATA(WA_FISCAL).
        WA_FISCAL-NM_PESO_SUBTOTAL = WA_FISCAL-NR_QUANTIDADE.

        IF LC_PESO_LIQUIDO LE WA_FISCAL-NM_PESO_SUBTOTAL.
          WA_FISCAL-NM_PESO_LIQUIDO = LC_PESO_LIQUIDO.
          LC_PESO_LIQUIDO = 0.
        ELSE.
          WA_FISCAL-NM_PESO_LIQUIDO = WA_FISCAL-NM_PESO_SUBTOTAL.
          IF ( LC_PESO_LIQUIDO - WA_FISCAL-NM_PESO_SUBTOTAL ) LE 0.
            LC_PESO_LIQUIDO = 0.
          ELSE.
            LC_PESO_LIQUIDO = LC_PESO_LIQUIDO - WA_FISCAL-NM_PESO_SUBTOTAL.
          ENDIF.
        ENDIF.

        TRY .
            ME->SET_PESOS_NOTAS(
              EXPORTING
                I_ID_CARGA      = WA_FISCAL-ID_CARGA
                I_ID_NOTA       = WA_FISCAL-ID_NOTA
                I_PESO_SUBTOTAL = WA_FISCAL-NM_PESO_SUBTOTAL
                I_PESO_LIQUIDO  = WA_FISCAL-NM_PESO_LIQUIDO ).
          CATCH ZCX_CARGA.    "
        ENDTRY.
      ENDLOOP.
    ELSE.

      LOOP AT ME->DOCUMENTO_FISCAL INTO WA_FISCAL.

        WA_FISCAL-NM_PESO_SUBTOTAL = ME->CARGA-NM_PESO_SUBTOTAL.
        WA_FISCAL-NM_PESO_LIQUIDO  = ME->CARGA-NM_PESO_LIQUIDO.

        TRY .
            ME->SET_PESOS_NOTAS(
              EXPORTING
                I_ID_CARGA      = WA_FISCAL-ID_CARGA
                I_ID_NOTA       = WA_FISCAL-ID_NOTA
                I_PESO_SUBTOTAL = WA_FISCAL-NM_PESO_SUBTOTAL
                I_PESO_LIQUIDO  = WA_FISCAL-NM_PESO_LIQUIDO  ).
          CATCH ZCX_CARGA.    "
        ENDTRY.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_AJUSTAR_RAT_ORDEM_VENDA.

    R_CARGA = ME.

    READ TABLE ME->ZIF_CARGA~ORDEM_VENDA INDEX 1 ASSIGNING FIELD-SYMBOL(<FS_ORDEM>).
    IF SY-SUBRC IS INITIAL.
      <FS_ORDEM>-ID_CARGA         = ME->CARGA-ID_CARGA.
      <FS_ORDEM>-NM_PESO_BRUTO    = ME->CARGA-NM_PESO_BRUTO.
      <FS_ORDEM>-NM_PESO_TARA     = ME->CARGA-NM_PESO_TARA.
      <FS_ORDEM>-NM_PESO_SUBTOTAL = ME->CARGA-NM_PESO_SUBTOTAL.
    ENDIF.

    READ TABLE ME->ZIF_CARGA~PEDIDO_COMPRA INDEX 1 ASSIGNING FIELD-SYMBOL(<FS_PEDIDO>).
    IF SY-SUBRC IS INITIAL.
      <FS_PEDIDO>-ID_CARGA         = ME->CARGA-ID_CARGA.
      <FS_PEDIDO>-NM_PESO_BRUTO    = ME->CARGA-NM_PESO_BRUTO.
      <FS_PEDIDO>-NM_PESO_TARA     = ME->CARGA-NM_PESO_TARA.
      <FS_PEDIDO>-NM_PESO_SUBTOTAL = ME->CARGA-NM_PESO_SUBTOTAL.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_BLOQUEAR_ROMANEIOS_SAIDA.
    R_CARGA = ME.
  ENDMETHOD.


  METHOD zif_carga~set_cancelar.

    DATA: ob_romaneio TYPE REF TO zcl_romaneio,
          wa_entrada  TYPE awkey,
          it_entradas TYPE TABLE OF awkey.

    r_carga = me.

    e_cancelou = abap_false.

    me->zif_carga~ck_executar_reversao_entrada = abap_true. "BUG 34154

    CHECK NOT ( me->carga-ck_enviado_opus EQ abap_true AND me->carga-ck_recebido_opus EQ abap_false ).

*-CS2021000183-#83755-19.07.2022-JT -inicio-Comentado
*-CS2021000183 - 05.04.2022 - JT - inicio
*   TRY.
*       me->zif_carga~set_cancelar_nota_propria( ).
*     CATCH zcx_error INTO DATA(ex_error).
*   ENDTRY.
*-CS2021000183 - 05.04.2022 - JT - fim
*-CS2021000183-#83755-19.07.2022-JT -fim-Comentado

    "Verificar Entrada
    "Verificar Solicitação
    "Verificar Cancelamento

    IF me->zif_carga~at_manutencao EQ abap_false.
      " Verificar se romaneios excluidos estão CANCELADOS 01/03/2024
      me->zif_carga~check_romaneios_carga_opus( EXPORTING i_id_carga  = me->carga-id_carga ).
      "
      LOOP AT me->documento_fiscal INTO DATA(wa_nota) WHERE obj_key_entrada IS NOT INITIAL.

        SELECT SINGLE * INTO @DATA(entrada)
          FROM zmmt_ee_zgr
         WHERE id_carga   EQ @wa_nota-id_carga
           AND id_nota    EQ @wa_nota-id_nota
           AND obj_key    EQ @wa_nota-obj_key_entrada.

        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        CASE entrada-st_estorno.
          WHEN zif_carga=>st_status_estorno_sem.

            SELECT SINGLE * INTO @DATA(documentos)
              FROM zmmt_ee_zgr_docs
             WHERE obj_key EQ @entrada-obj_key.

            IF "DOCUMENTOS-PO_NUMBER IS NOT INITIAL OR
               "DOCUMENTOS-AV_VBELN  IS NOT INITIAL OR
               documentos-mm_mblnr  IS NOT INITIAL OR
               documentos-mm_mjahr  IS NOT INITIAL OR
               documentos-ft_belnr  IS NOT INITIAL OR
               documentos-ft_gjahr  IS NOT INITIAL OR
               documentos-docnum    IS NOT INITIAL.
              "Solicitar Estorno
              RAISE EXCEPTION TYPE zcx_carga
                EXPORTING
                  textid = VALUE #( msgid = zcx_carga=>zcx_estornar_entrada-msgid msgno = zcx_carga=>zcx_estornar_entrada-msgno )
                  msgty  = 'E'
                  msgid  = zcx_carga=>zcx_estornar_entrada-msgid
                  msgno  = zcx_carga=>zcx_estornar_entrada-msgno.
            ENDIF.

            "ME->GET_CHECK_JOB_EXECUCAO( ).

            wa_entrada = entrada-obj_key.
            APPEND wa_entrada TO it_entradas.

            me->bloquear_entrada( i_obj_key = entrada-obj_key ).

          WHEN zif_carga=>st_status_estorno_solicitado.

            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid = VALUE #( msgid = zcx_carga=>zcx_existe_estorno_pendente-msgid msgno = zcx_carga=>zcx_existe_estorno_pendente-msgno )
                msgty  = 'E'
                msgid  = zcx_carga=>zcx_existe_estorno_pendente-msgid
                msgno  = zcx_carga=>zcx_existe_estorno_pendente-msgno.

          WHEN zif_carga=>st_status_estorno_executado.
            "Deixa Cancelar
          WHEN zif_carga=>st_status_estorno_erro.
            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid = VALUE #( msgid = zcx_carga=>zcx_existe_estorno_erro-msgid msgno = zcx_carga=>zcx_existe_estorno_erro-msgno )
                msgty  = 'E'
                msgid  = zcx_carga=>zcx_existe_estorno_erro-msgid
                msgno  = zcx_carga=>zcx_existe_estorno_erro-msgno.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    "Verificar Saida

    "Verificar Solicitação
    "Verificar Cancelamento

    e_cancelou = abap_false.

    CASE me->carga-tp_status.
      WHEN zif_carga=>st_status_cancelada.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_carga_cancelada-msgid msgno = zcx_carga=>zcx_carga_cancelada-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_cancelada-msgid
            msgno  = zcx_carga=>zcx_carga_cancelada-msgno.
      WHEN zif_carga=>st_status_conferido.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_carga_conferida-msgid msgno = zcx_carga=>zcx_carga_conferida-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_conferida-msgid
            msgno  = zcx_carga=>zcx_carga_conferida-msgno.
    ENDCASE.

    "Exclui Frete Entrada
    me->zif_carga~set_estorna_frete_entrada( ).

    "Veririca se Pode Excluir Romaneio de Saída
    me->zif_carga~get_ck_excluir_romaneio_saida( i_ck_opus = abap_true ).

    me->zif_carga~get_romaneio_saida( EXPORTING i_id_carga  = me->carga-id_carga IMPORTING e_romaneios = DATA(it_romaneio) ).

    LOOP AT it_romaneio INTO DATA(wa_romaneio).
      CREATE OBJECT ob_romaneio.
      ob_romaneio->zif_cadastro~set_registro( i_id_registro = wa_romaneio-ch_referencia ).
      ob_romaneio->zif_cadastro~excluir_registro( ).
      CLEAR ob_romaneio.
    ENDLOOP.

    DATA(lc_status)     = me->carga-tp_status.
    me->carga-tp_status = zif_carga=>st_status_cancelada.
    me->validar_registro( IMPORTING e_validou = DATA(e_validou)  ).

    IF NOT e_validou EQ abap_true.
      me->carga-tp_status = lc_status.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = sy-msgid msgno = sy-msgno
                            attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
          msgty  = 'E'
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.

    me->ck_alterou              = abap_true.
    me->carga-dt_cancelamento   = sy-datlo.
    me->carga-hr_cancelamento   = sy-timlo.
    me->carga-us_cancelamento   = sy-uname.

    IF me->zif_carga~at_manutencao EQ abap_false.

      SELECT * INTO TABLE @DATA(it_romaneios)
        FROM zsdt0001
       WHERE id_carga EQ @me->carga-id_carga
         AND id_carga NE @space.

      CREATE OBJECT ob_romaneio.

      TRY .
          LOOP AT it_romaneios INTO wa_romaneio.
            ob_romaneio->zif_cadastro~set_registro( i_id_registro = wa_romaneio-ch_referencia ).
            ob_romaneio->zif_cadastro~excluir_registro( ).
          ENDLOOP.
        CATCH zcx_cadastro INTO DATA(ex_cadastro).
          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = ex_cadastro->if_t100_message~t100key
              msgty  = 'E'
              msgno  = ex_cadastro->if_t100_message~t100key-msgno
              msgv1  = CONV #( ex_cadastro->if_t100_message~t100key-attr1 )
              msgv2  = CONV #( ex_cadastro->if_t100_message~t100key-attr2 )
              msgv3  = CONV #( ex_cadastro->if_t100_message~t100key-attr3 )
              msgv4  = CONV #( ex_cadastro->if_t100_message~t100key-attr4 )
              msgid  = ex_cadastro->if_t100_message~t100key-msgid.
      ENDTRY.

      "Excluir Entradas
      LOOP AT it_entradas INTO wa_entrada.
        DELETE FROM zmmt_ee_zgr
          WHERE id_carga EQ me->carga-id_carga
            AND obj_key  EQ wa_entrada.
        me->desbloquear_entrada( i_obj_key = wa_entrada ).
      ENDLOOP.

    ENDIF.

    me->zif_carga~solicitacao_manutencao-tp_status = zif_carga=>st_status_manut_cancelada.
    me->gravar_registro( IMPORTING e_gravou  = e_cancelou ).

    IF me->carga-id_ordem IS NOT INITIAL AND e_cancelou EQ abap_true AND me->zif_carga~at_manutencao EQ abap_false.
      CHECK zcl_ordem_carregamento=>set_abrir( i_id_ordem = me->carga-id_ordem ) EQ abap_true.
    ENDIF.

    MESSAGE s119.

  ENDMETHOD.


  METHOD zif_carga~set_cancelar_nota_propria.

    DATA: at_carga_original TYPE REF TO zif_carga,
          l_tabix           TYPE sy-tabix.

*----------------------------------------
*-- Quando é feito manutencao, recuperar dados originais
*----------------------------------------
    at_carga_original = zcl_factory_carga=>zif_factory_carga~get_instance(
                         )->set_factory_objeto( EXPORTING i_tp_carga      = me->carga-tp_carga
                                                          i_tp_produto    = me->carga-tp_produto_carga
                         )->get_factory_objeto(
                         ).

    at_carga_original->set_registro( i_id_carga = me->carga-id_carga i_no_enqueue = abap_true ).

*----------------------------------------
*-- Cancelar Nota propria
*----------------------------------------
    LOOP AT me->documento_fiscal INTO DATA(w_doc_fiscal) WHERE docnum_np IS NOT INITIAL
                                                           AND ent_np     = abap_true.
      l_tabix = sy-tabix.

*----------------------------------------
*---- Quando é feita manutencao
*----------------------------------------
      READ TABLE at_carga_original->documento_fiscal INTO DATA(w_carga_original) WITH KEY id_carga = w_doc_fiscal-id_carga
                                                                                          id_nota  = w_doc_fiscal-id_nota.
      IF sy-subrc = 0 AND ( w_carga_original-nr_chave_nfe <> w_doc_fiscal-nr_chave_nfe AND
                            w_carga_original-nr_chave_nfe IS NOT INITIAL ).

        MOVE-CORRESPONDING w_carga_original  TO w_doc_fiscal.
      ENDIF.

*----------------------------------------
*---- Ajustar referencias
*----------------------------------------
      UPDATE j_1bnfdoc SET belnr  = abap_off
                           gjahr  = 0
                     WHERE docnum = w_doc_fiscal-docnum_np.

      UPDATE j_1bnflin SET reftyp = abap_off
                           refkey = abap_off
                           refitm = 0
                     WHERE docnum = w_doc_fiscal-docnum_np.

*-------------------------------------------
*------ Atualiza ZIB_NFE_FORN
*-------------------------------------------
      IF w_doc_fiscal-nr_chave_nfe IS NOT INITIAL.
        UPDATE zib_nfe_forn SET docnum_ref      = 0
                                objkey_np       = abap_off
                          WHERE nu_chave        = w_doc_fiscal-nr_chave_nfe.
      ELSE.
        SELECT stcd2
          INTO @DATA(l_stcd2)
          FROM lfa1
            UP TO 1 ROWS
         WHERE lifnr = @w_doc_fiscal-id_fornecedor.
        ENDSELECT.

        UPDATE zib_nfe_forn SET docnum_ref      = 0
                                objkey_np       = abap_off
                          WHERE nu_chave_cnpj   = l_stcd2
                            AND dt_emissao      = w_doc_fiscal-dt_emissao
                            AND nu_chave_numero = w_doc_fiscal-nr_nota
                            AND nu_ie           = w_doc_fiscal-nr_fornecedor_ie.
      ENDIF.

      COMMIT WORK.

*----------------------------------------
*---- CAncela
*----------------------------------------
      DO 5 TIMES.
        TRY.
            zcl_nfe=>zif_doc_eletronico~get_instance( EXPORTING i_docnum    = w_doc_fiscal-docnum_np
              )->set_registro(                                  i_docnum    = w_doc_fiscal-docnum_np
              )->set_cancelar(                        EXPORTING i_motivo    = '01'       " Motivo para estorno/não utilização
                                                                i_aguardar  = abap_true  " Aguardar Cancelamento
                                                                i_ciclos    = 50
                                                                i_segundos  = 10
*             )->get_ck_autorizado_uso(
              )->get_registro(                        IMPORTING e_documento = DATA(e_documento)
              )->set_liberar_registro(
              )->set_clear( ).

            SELECT SINGLE *
              INTO @DATA(w_active)
              FROM j_1bnfe_active
             WHERE docnum = @w_doc_fiscal-docnum_np.

            IF ( w_active-docsta = 1 AND w_active-scssta = 2 ) OR
               ( w_active-docsta = 2 AND w_active-scssta = 4 ) OR
               ( w_active-cancel = abap_true ).
              EXIT.
            ELSE.
              WAIT UP TO 5 SECONDS.
            ENDIF.

          CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).
        ENDTRY.
      ENDDO.

      SELECT SINGLE *
        INTO w_active
        FROM j_1bnfe_active
       WHERE docnum = w_doc_fiscal-docnum_np.

      IF NOT ( ( w_active-docsta = 1 AND w_active-scssta = 2 ) OR
               ( w_active-docsta = 2 AND w_active-scssta = 4 ) OR
               ( w_active-cancel = abap_true ) ).
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            titel        = 'Cancelamento da Nota Própria'
            textline1    = 'Deve ser Cancelada/Inutilizada Nota própria:'
            textline2    = w_doc_fiscal-docnum_np
            start_column = 25
            start_row    = 6.
        EXIT.
      ENDIF.

*-----------------------------------
*---- dessassocia NProrpia
*-----------------------------------
      CLEAR: w_doc_fiscal-docnum_np,
             w_doc_fiscal-objkey_np.

      MODIFY me->documento_fiscal FROM w_doc_fiscal INDEX l_tabix TRANSPORTING docnum_np objkey_np.
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_CANCELAR_SOLIC_MANUT.

    R_INSTANCE = ME.
    CHECK ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_TRUE.
    CHECK ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-TP_STATUS IS NOT INITIAL.
    CHECK ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-TP_STATUS NE ZIF_CARGA=>ST_STATUS_MANUT_ABERTO.

    "ZCX_WORKFLOW_APROVADO  WorkFlow Manutenção de Romaneio &MSGV1& Está Aprovado!
    "ZCX_WORKFLOW_RECUSADO  WorkFlow Manutenção de Romaneio &MSGV1& Está Recusado!

    CASE ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-TP_STATUS.
      WHEN ZIF_CARGA=>ST_STATUS_MANUT_APROVADO.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_WORKFLOW_APROVADO-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_WORKFLOW_APROVADO-MSGNO
                              ATTR1 = CONV #( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_RECORDID ) )
            MSGID  = ZCX_CARGA=>ZCX_WORKFLOW_APROVADO-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_WORKFLOW_APROVADO-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_RECORDID ).
      WHEN ZIF_CARGA=>ST_STATUS_MANUT_RECUSADA.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_WORKFLOW_RECUSADO-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_WORKFLOW_RECUSADO-MSGNO
                              ATTR1 = CONV #( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_RECORDID ) )
            MSGID  = ZCX_CARGA=>ZCX_WORKFLOW_RECUSADO-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_WORKFLOW_RECUSADO-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_RECORDID ).
    ENDCASE.

    CHECK ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_RECORDID IS NOT INITIAL.

    CONCATENATE 'Cancelado pelo Usuário' SY-UNAME 'via SAP' INTO DATA(LC_MOTIVO) SEPARATED BY SPACE.

    ZCL_SOFT_EXPERT_WORKFLOW=>ZIF_SOFT_EXPERT_WORKFLOW~GET_INSTANCE(
      )->CANCEL_WORKFLOW(
      EXPORTING
        I_WORKFLOWID             = CONV #( ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_RECORDID )
        I_EXPLANATION            = LC_MOTIVO
      IMPORTING
        E_CANCEL_WORFLOW_RET     = DATA(E_CANCEL_WORFLOW_RET) ).

    ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-TP_STATUS    = ZIF_CARGA=>ST_STATUS_MANUT_ABERTO.

    CLEAR:
    ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_STATUS,
    ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_CODE,
    ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_DETAIL,
    ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_RECORDKEY,
    ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_RECORDID.
    ME->ZIF_CARGA~CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD zif_carga~set_carga.

    DATA: lc_resultado            LIKE LINE OF me->resultado,
          lc_resultado_sub_ardido LIKE LINE OF me->zif_carga~resultado_avariado,
          w_vbap                  TYPE vbap, "*-CS2021000253-26.04.2024-#59941-JT
          w_vbkd                  TYPE vbkd. "*-CS2021000253-26.04.2024-#59941-JT

    r_carga = me.

    CASE me->carga-tp_status.
      WHEN zif_carga=>st_status_fechado.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_carga_fechada-msgid msgno = zcx_carga=>zcx_carga_fechada-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_fechada-msgid
            msgno  = zcx_carga=>zcx_carga_fechada-msgno.
      WHEN zif_carga=>st_status_conferido.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_carga_conferida-msgid msgno = zcx_carga=>zcx_carga_conferida-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_conferida-msgid
            msgno  = zcx_carga=>zcx_carga_conferida-msgno.
      WHEN zif_carga=>st_status_cancelada.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_carga_cancelada-msgid msgno = zcx_carga=>zcx_carga_cancelada-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_cancelada-msgid
            msgno  = zcx_carga=>zcx_carga_cancelada-msgno.
    ENDCASE.

    IF me->carga-tp_status NE zif_carga=>st_status_aberto.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_carga_nao_aberta-msgid msgno = zcx_carga=>zcx_carga_nao_aberta-msgno )
          msgty  = 'E'
          msgid  = zcx_carga=>zcx_carga_nao_aberta-msgid
          msgno  = zcx_carga=>zcx_carga_nao_aberta-msgno.
    ENDIF.

    DATA(wa_carga) = i_carga.
    DATA(cp_me_carga) = me->carga.
    DATA(cp_me_classificacao) = me->classificacao.

    wa_carga-id_carga         = me->carga-id_carga.
    wa_carga-id_classificacao = me->classificacao-id_classificacao.
    MOVE-CORRESPONDING wa_carga TO me->carga.
    MOVE-CORRESPONDING wa_carga TO me->classificacao.

    IF me->carga NE cp_me_carga.
      me->ck_alterou = abap_true.
    ENDIF.

    IF me->classificacao NE cp_me_classificacao.
      me->ck_alterou = abap_true.
    ENDIF.

    e_carga_recebimento = i_carga.

    "Informações alteradas na classificação

*    DATA(LC_PESO) = E_CARGA_RECEBIMENTO-NM_PESO_SUBTOTAL.
*
*    ME->GET_CALCULAR_SUBTOTAL(
*      EXPORTING
*        I_PESO_BRUTO    = E_CARGA_RECEBIMENTO-NM_PESO_BRUTO
*        I_PESO_TARA     = E_CARGA_RECEBIMENTO-NM_PESO_TARA
*      IMPORTING
*        E_PESO_SUBTOTAL = E_CARGA_RECEBIMENTO-NM_PESO_SUBTOTAL ).
*
*    E_CARGA_RECEBIMENTO-NM_PESO_SUBTOTAL = LC_PESO.

    "CLEAR: ME->RESULTADO.

    "Umidade
    lc_resultado-tp_caracteristica = zif_carga=>st_tp_caract_class_umidade.
    lc_resultado-nr_percentual_com = e_carga_recebimento-nr_perc_umi.
    lc_resultado-nr_percentual_fis = e_carga_recebimento-nr_perc_umi.
    lc_resultado-nr_quantidade_com = e_carga_recebimento-nr_qtde_umi.
    lc_resultado-nr_quantidade_fis = e_carga_recebimento-nr_qtde_umi.
    lc_resultado-id_carga          = me->carga-id_carga.
    lc_resultado-id_classificacao  = me->carga-id_classificacao.

    READ TABLE me->resultado WITH KEY id_carga          = me->carga-id_carga
                                      id_classificacao  = me->carga-id_classificacao
                                      tp_caracteristica = zif_carga=>st_tp_caract_class_umidade
                             ASSIGNING FIELD-SYMBOL(<fs_resultado>).
    IF sy-subrc IS INITIAL.
      LOOP AT me->resultado ASSIGNING <fs_resultado>
           WHERE id_carga          = me->carga-id_carga
             AND id_classificacao  = me->carga-id_classificacao
             AND tp_caracteristica = zif_carga=>st_tp_caract_class_umidade.
        MOVE-CORRESPONDING lc_resultado TO <fs_resultado>.
      ENDLOOP.
    ELSE.
      APPEND lc_resultado TO me->resultado.
    ENDIF.

    "Impureza
    lc_resultado-tp_caracteristica = zif_carga=>st_tp_caract_class_impureza.
    lc_resultado-nr_percentual_com = e_carga_recebimento-nr_perc_imp.
    lc_resultado-nr_percentual_fis = e_carga_recebimento-nr_perc_imp.
    lc_resultado-nr_quantidade_com = e_carga_recebimento-nr_qtde_imp.
    lc_resultado-nr_quantidade_fis = e_carga_recebimento-nr_qtde_imp.
    lc_resultado-id_carga          = me->carga-id_carga.
    lc_resultado-id_classificacao  = me->carga-id_classificacao.
    READ TABLE me->resultado WITH KEY id_carga          = me->carga-id_carga
                                      id_classificacao  = me->carga-id_classificacao
                                      tp_caracteristica = zif_carga=>st_tp_caract_class_impureza
                             ASSIGNING <fs_resultado>.
    IF sy-subrc IS INITIAL.
      LOOP AT me->resultado ASSIGNING <fs_resultado>
           WHERE id_carga          = me->carga-id_carga
             AND id_classificacao  = me->carga-id_classificacao
             AND tp_caracteristica = zif_carga=>st_tp_caract_class_impureza.
        MOVE-CORRESPONDING lc_resultado TO <fs_resultado>.
      ENDLOOP.
    ELSE.
      APPEND lc_resultado TO me->resultado.
    ENDIF.


    "Avariado """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Avariado """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    lc_resultado-tp_caracteristica = zif_carga=>st_tp_caract_class_avariado.
    lc_resultado-nr_percentual_com = e_carga_recebimento-nr_perc_ava.
    lc_resultado-nr_percentual_fis = e_carga_recebimento-nr_perc_ava.
    lc_resultado-nr_quantidade_com = e_carga_recebimento-nr_qtde_ava.
    lc_resultado-nr_quantidade_fis = e_carga_recebimento-nr_qtde_ava.
    lc_resultado-id_carga          = me->carga-id_carga.
    lc_resultado-id_classificacao  = me->carga-id_classificacao.
    READ TABLE me->resultado WITH KEY id_carga          = me->carga-id_carga
                                      id_classificacao  = me->carga-id_classificacao
                                      tp_caracteristica = zif_carga=>st_tp_caract_class_avariado
                             ASSIGNING <fs_resultado>.
    IF sy-subrc IS INITIAL.
      LOOP AT me->resultado ASSIGNING <fs_resultado>
           WHERE id_carga          = me->carga-id_carga
             AND id_classificacao  = me->carga-id_classificacao
             AND tp_caracteristica = zif_carga=>st_tp_caract_class_avariado.
        MOVE-CORRESPONDING lc_resultado TO <fs_resultado>.
      ENDLOOP.
    ELSE.
      APPEND lc_resultado TO me->resultado.
    ENDIF.

    "Avariado 1	Ardido/Queimado
    CLEAR: lc_resultado_sub_ardido.
    lc_resultado_sub_ardido-tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_arq.
    lc_resultado_sub_ardido-nr_percentual_com     = e_carga_recebimento-nr_perc_ava_arq.
    lc_resultado_sub_ardido-id_carga              = me->carga-id_carga.
    lc_resultado_sub_ardido-id_classificacao      = me->carga-id_classificacao.

    READ TABLE me->zif_carga~resultado_avariado
    WITH KEY id_carga              = me->carga-id_carga
             id_classificacao      = me->carga-id_classificacao
             tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_arq
    ASSIGNING FIELD-SYMBOL(<fs_res_ardido>).

    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING lc_resultado_sub_ardido TO <fs_res_ardido>.
    ELSE.
      APPEND lc_resultado_sub_ardido TO me->zif_carga~resultado_avariado.
    ENDIF.

    "Avariado 2	Queimados
    CLEAR: lc_resultado_sub_ardido.
    lc_resultado_sub_ardido-tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_que.
    lc_resultado_sub_ardido-nr_percentual_com     = e_carga_recebimento-nr_perc_ava_que.
    lc_resultado_sub_ardido-id_carga              = me->carga-id_carga.
    lc_resultado_sub_ardido-id_classificacao      = me->carga-id_classificacao.

    READ TABLE me->zif_carga~resultado_avariado
    WITH KEY id_carga              = me->carga-id_carga
             id_classificacao      = me->carga-id_classificacao
             tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_que
    ASSIGNING <fs_res_ardido>.

    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING lc_resultado_sub_ardido TO <fs_res_ardido>.
    ELSE.
      APPEND lc_resultado_sub_ardido TO me->zif_carga~resultado_avariado.
    ENDIF.

    "Avariado 3	Mofados
    CLEAR: lc_resultado_sub_ardido.
    lc_resultado_sub_ardido-tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_mof.
    lc_resultado_sub_ardido-nr_percentual_com     = e_carga_recebimento-nr_perc_ava_mof.
    lc_resultado_sub_ardido-id_carga              = me->carga-id_carga.
    lc_resultado_sub_ardido-id_classificacao      = me->carga-id_classificacao.

    READ TABLE me->zif_carga~resultado_avariado
    WITH KEY id_carga              = me->carga-id_carga
             id_classificacao      = me->carga-id_classificacao
             tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_mof
    ASSIGNING <fs_res_ardido>.

    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING lc_resultado_sub_ardido TO <fs_res_ardido>.
    ELSE.
      APPEND lc_resultado_sub_ardido TO me->zif_carga~resultado_avariado.
    ENDIF.

    "Avariado 4	Picados
    CLEAR: lc_resultado_sub_ardido.
    lc_resultado_sub_ardido-tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_pic.
    lc_resultado_sub_ardido-nr_percentual_com     = e_carga_recebimento-nr_perc_ava_pic.
    lc_resultado_sub_ardido-id_carga              = me->carga-id_carga.
    lc_resultado_sub_ardido-id_classificacao      = me->carga-id_classificacao.

    READ TABLE me->zif_carga~resultado_avariado
    WITH KEY id_carga              = me->carga-id_carga
             id_classificacao      = me->carga-id_classificacao
             tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_pic
    ASSIGNING <fs_res_ardido>.

    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING lc_resultado_sub_ardido TO <fs_res_ardido>.
    ELSE.
      APPEND lc_resultado_sub_ardido TO me->zif_carga~resultado_avariado.
    ENDIF.

    "Avariado 5	Fermentados
    CLEAR: lc_resultado_sub_ardido.
    lc_resultado_sub_ardido-tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_fer.
    lc_resultado_sub_ardido-nr_percentual_com     = e_carga_recebimento-nr_perc_ava_fer.
    lc_resultado_sub_ardido-id_carga              = me->carga-id_carga.
    lc_resultado_sub_ardido-id_classificacao      = me->carga-id_classificacao.

    READ TABLE me->zif_carga~resultado_avariado
    WITH KEY id_carga              = me->carga-id_carga
             id_classificacao      = me->carga-id_classificacao
             tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_fer
    ASSIGNING <fs_res_ardido>.

    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING lc_resultado_sub_ardido TO <fs_res_ardido>.
    ELSE.
      APPEND lc_resultado_sub_ardido TO me->zif_carga~resultado_avariado.
    ENDIF.

    "Avariado 6	Germinados/Imaturos/Chochos
    CLEAR: lc_resultado_sub_ardido.
    lc_resultado_sub_ardido-tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_ger.
    lc_resultado_sub_ardido-nr_percentual_com     = e_carga_recebimento-nr_perc_ava_ger.
    lc_resultado_sub_ardido-id_carga              = me->carga-id_carga.
    lc_resultado_sub_ardido-id_classificacao      = me->carga-id_classificacao.

    READ TABLE me->zif_carga~resultado_avariado
    WITH KEY id_carga              = me->carga-id_carga
             id_classificacao      = me->carga-id_classificacao
             tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_ger
    ASSIGNING <fs_res_ardido>.

    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING lc_resultado_sub_ardido TO <fs_res_ardido>.
    ELSE.
      APPEND lc_resultado_sub_ardido TO me->zif_carga~resultado_avariado.
    ENDIF.

    "Avariado 7	Ardidos
    CLEAR: lc_resultado_sub_ardido.
    lc_resultado_sub_ardido-tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_ard.
    lc_resultado_sub_ardido-nr_percentual_com     = e_carga_recebimento-nr_perc_ava_ard.
    lc_resultado_sub_ardido-id_carga              = me->carga-id_carga.
    lc_resultado_sub_ardido-id_classificacao      = me->carga-id_classificacao.

    READ TABLE me->zif_carga~resultado_avariado
    WITH KEY id_carga              = me->carga-id_carga
             id_classificacao      = me->carga-id_classificacao
             tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_ard
    ASSIGNING <fs_res_ardido>.

    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING lc_resultado_sub_ardido TO <fs_res_ardido>.
    ELSE.
      APPEND lc_resultado_sub_ardido TO me->zif_carga~resultado_avariado.
    ENDIF.

    "Avariado 8	Gessados
    CLEAR: lc_resultado_sub_ardido.
    lc_resultado_sub_ardido-tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_ges.
    lc_resultado_sub_ardido-nr_percentual_com     = e_carga_recebimento-nr_perc_ava_ges.
    lc_resultado_sub_ardido-id_carga              = me->carga-id_carga.
    lc_resultado_sub_ardido-id_classificacao      = me->carga-id_classificacao.

    READ TABLE me->zif_carga~resultado_avariado
    WITH KEY id_carga              = me->carga-id_carga
             id_classificacao      = me->carga-id_classificacao
             tp_sub_carac_avariado = zif_carga=>st_tp_caract_sub_ges
    ASSIGNING <fs_res_ardido>.

    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING lc_resultado_sub_ardido TO <fs_res_ardido>.
    ELSE.
      APPEND lc_resultado_sub_ardido TO me->zif_carga~resultado_avariado.
    ENDIF.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "Ardido
    lc_resultado-tp_caracteristica = zif_carga=>st_tp_caract_class_ardido.
    lc_resultado-nr_percentual_com = e_carga_recebimento-nr_perc_ard.
    lc_resultado-nr_percentual_fis = e_carga_recebimento-nr_perc_ard.
    lc_resultado-nr_quantidade_com = e_carga_recebimento-nr_qtde_ard.
    lc_resultado-nr_quantidade_fis = e_carga_recebimento-nr_qtde_ard.
    lc_resultado-id_carga          = me->carga-id_carga.
    lc_resultado-id_classificacao  = me->carga-id_classificacao.
    READ TABLE me->resultado WITH KEY id_carga          = me->carga-id_carga
                                      id_classificacao  = me->carga-id_classificacao
                                      tp_caracteristica = zif_carga=>st_tp_caract_class_ardido
                             ASSIGNING <fs_resultado>.
    IF sy-subrc IS INITIAL.
      LOOP AT me->resultado ASSIGNING <fs_resultado>
           WHERE id_carga          = me->carga-id_carga
             AND id_classificacao  = me->carga-id_classificacao
             AND tp_caracteristica = zif_carga=>st_tp_caract_class_ardido.
        MOVE-CORRESPONDING lc_resultado TO <fs_resultado>.
      ENDLOOP.
    ELSE.
      APPEND lc_resultado TO me->resultado.
    ENDIF.


    "Quebrado
    lc_resultado-tp_caracteristica = zif_carga=>st_tp_caract_class_quebrado.
    lc_resultado-nr_percentual_com = e_carga_recebimento-nr_perc_que.
    lc_resultado-nr_percentual_fis = e_carga_recebimento-nr_perc_que.
    lc_resultado-nr_quantidade_com = e_carga_recebimento-nr_qtde_que.
    lc_resultado-nr_quantidade_fis = e_carga_recebimento-nr_qtde_que.
    lc_resultado-id_carga          = me->carga-id_carga.
    lc_resultado-id_classificacao  = me->carga-id_classificacao.
    READ TABLE me->resultado WITH KEY id_carga          = me->carga-id_carga
                                      id_classificacao  = me->carga-id_classificacao
                                      tp_caracteristica = zif_carga=>st_tp_caract_class_quebrado
                             ASSIGNING <fs_resultado>.
    IF sy-subrc IS INITIAL.
      LOOP AT me->resultado ASSIGNING <fs_resultado>
           WHERE id_carga          = me->carga-id_carga
             AND id_classificacao  = me->carga-id_classificacao
             AND tp_caracteristica = zif_carga=>st_tp_caract_class_quebrado.
        MOVE-CORRESPONDING lc_resultado TO <fs_resultado>.
      ENDLOOP.
    ELSE.
      APPEND lc_resultado TO me->resultado.
    ENDIF.

    "Esverdeado
    lc_resultado-tp_caracteristica = zif_carga=>st_tp_caract_class_esverdeado.
    lc_resultado-nr_percentual_com = e_carga_recebimento-nr_perc_esv.
    lc_resultado-nr_percentual_fis = e_carga_recebimento-nr_perc_esv.
    lc_resultado-nr_quantidade_com = e_carga_recebimento-nr_qtde_esv.
    lc_resultado-nr_quantidade_fis = e_carga_recebimento-nr_qtde_esv.
    lc_resultado-id_carga          = me->carga-id_carga.
    lc_resultado-id_classificacao  = me->carga-id_classificacao.
    READ TABLE me->resultado WITH KEY id_carga          = me->carga-id_carga
                                      id_classificacao  = me->carga-id_classificacao
                                      tp_caracteristica = zif_carga=>st_tp_caract_class_esverdeado
                             ASSIGNING <fs_resultado>.
    IF sy-subrc IS INITIAL.
      LOOP AT me->resultado ASSIGNING <fs_resultado>
           WHERE id_carga          = me->carga-id_carga
             AND id_classificacao  = me->carga-id_classificacao
             AND tp_caracteristica = zif_carga=>st_tp_caract_class_esverdeado.
        MOVE-CORRESPONDING lc_resultado TO <fs_resultado>.
      ENDLOOP.
    ELSE.
      APPEND lc_resultado TO me->resultado.
    ENDIF.

    "Carunchado
    lc_resultado-tp_caracteristica = zif_carga=>st_tp_caract_class_carunchado.
    lc_resultado-nr_percentual_com = e_carga_recebimento-nr_perc_car.
    lc_resultado-nr_percentual_fis = e_carga_recebimento-nr_perc_car.
    lc_resultado-nr_quantidade_com = e_carga_recebimento-nr_qtde_car.
    lc_resultado-nr_quantidade_fis = e_carga_recebimento-nr_qtde_car.
    lc_resultado-id_carga          = me->carga-id_carga.
    lc_resultado-id_classificacao  = me->carga-id_classificacao.
    READ TABLE me->resultado WITH KEY id_carga          = me->carga-id_carga
                                      id_classificacao  = me->carga-id_classificacao
                                      tp_caracteristica = zif_carga=>st_tp_caract_class_carunchado
                             ASSIGNING <fs_resultado>.
    IF sy-subrc IS INITIAL.
      LOOP AT me->resultado ASSIGNING <fs_resultado>
           WHERE id_carga          = me->carga-id_carga
             AND id_classificacao  = me->carga-id_classificacao
             AND tp_caracteristica = zif_carga=>st_tp_caract_class_carunchado.
        MOVE-CORRESPONDING lc_resultado TO <fs_resultado>.
      ENDLOOP.
    ELSE.
      APPEND lc_resultado TO me->resultado.
    ENDIF.

    e_carga_recebimento-nm_peso_descontos = e_carga_recebimento-nr_qtde_umi + e_carga_recebimento-nr_qtde_imp +
                                            e_carga_recebimento-nr_qtde_ava + e_carga_recebimento-nr_qtde_ard +
                                            e_carga_recebimento-nr_qtde_que + e_carga_recebimento-nr_qtde_esv +
                                            e_carga_recebimento-nr_qtde_car.

    e_carga_recebimento-nm_peso_liquido   = e_carga_recebimento-nm_peso_subtotal - e_carga_recebimento-nm_peso_descontos.

    me->carga-nm_peso_descontos = e_carga_recebimento-nm_peso_descontos.
    me->carga-nm_peso_liquido   = e_carga_recebimento-nm_peso_liquido.

    "Local de Coleta
    IF ( me->carga-id_local_coleta NE cp_me_carga-id_local_coleta ) OR
       ( me->carga-id_local_coleta IS NOT INITIAL AND e_carga_recebimento-ds_local_coleta IS INITIAL ).
      CLEAR: e_carga_recebimento-ds_local_coleta.
      SELECT SINGLE name1
        INTO e_carga_recebimento-ds_local_coleta
        FROM lfa1 WHERE lifnr EQ me->carga-id_local_coleta.
    ELSEIF me->carga-id_local_coleta IS INITIAL.
      CLEAR: e_carga_recebimento-ds_local_coleta.
    ENDIF.

    "Local de Descarga
    IF ( me->carga-id_local_descarga NE cp_me_carga-id_local_descarga ) OR
       ( me->carga-id_local_descarga IS NOT INITIAL AND e_carga_recebimento-ds_local_descarga IS INITIAL ) .
      CLEAR: e_carga_recebimento-ds_local_descarga.
      SELECT SINGLE name1
        INTO e_carga_recebimento-ds_local_descarga
        FROM kna1 WHERE kunnr EQ me->carga-id_local_descarga.
    ELSEIF me->carga-id_local_descarga IS INITIAL.
      CLEAR: e_carga_recebimento-ds_local_descarga.
    ENDIF.

    "Local de Destino
    IF ( me->carga-id_local_destino NE cp_me_carga-id_local_destino ) OR
       ( me->carga-id_local_destino IS NOT INITIAL AND e_carga_recebimento-ds_local_destino IS INITIAL ).
      CLEAR: e_carga_recebimento-ds_local_destino.
      SELECT SINGLE name1
        INTO e_carga_recebimento-ds_local_destino
        FROM lfa1 WHERE lifnr EQ me->carga-id_local_destino.
    ELSEIF me->carga-id_local_destino IS INITIAL.
      CLEAR: e_carga_recebimento-ds_local_destino.
    ENDIF.

*    "Tipo de Entrada
*    IF ( ME->CARGA-ID_ENTRADA NE CP_ME_CARGA-ID_ENTRADA ) OR ( ME->CARGA-ID_ENTRADA IS NOT INITIAL AND E_CARGA_RECEBIMENTO-DS_ENTRADA IS INITIAL ).
*      SELECT SINGLE DS_ENTRADA
*        INTO E_CARGA_RECEBIMENTO-DS_ENTRADA
*        FROM ZSDT0001TETX WHERE ID_ENTRADA EQ ME->CARGA-ID_ENTRADA.
*    ELSEIF ME->CARGA-ID_ENTRADA IS INITIAL.
*      CLEAR: E_CARGA_RECEBIMENTO-DS_ENTRADA.
*    ENDIF.

    "Local de Entrega
    IF ( me->carga-id_local_entrega NE cp_me_carga-id_local_entrega ) OR
       ( me->carga-id_local_entrega IS NOT INITIAL AND e_carga_recebimento-ds_local_entrega IS INITIAL ).
      CLEAR: e_carga_recebimento-ds_local_entrega.
      SELECT SINGLE ds_local_entrega
        INTO e_carga_recebimento-ds_local_entrega
        FROM zsdt0001le WHERE id_local_entrega EQ me->carga-id_local_entrega.
    ELSEIF me->carga-id_local_entrega IS INITIAL.
      CLEAR: e_carga_recebimento-ds_local_entrega.
    ENDIF.

    "Motorista
    IF ( me->carga-id_motorista NE cp_me_carga-id_motorista ) OR
       ( me->carga-id_motorista IS NOT INITIAL AND e_carga_recebimento-ds_motorista IS INITIAL ).
      CLEAR: e_carga_recebimento-ds_motorista.
      SELECT SINGLE name1
        INTO e_carga_recebimento-ds_motorista
        FROM lfa1 WHERE lifnr EQ me->carga-id_motorista.
    ELSEIF me->carga-id_motorista IS INITIAL.
      CLEAR: e_carga_recebimento-ds_motorista.
    ENDIF.

    "Proprietário do Veículo
    IF ( me->carga-id_proprietario NE cp_me_carga-id_proprietario ) OR
       ( me->carga-id_proprietario IS NOT INITIAL AND e_carga_recebimento-ds_proprietario IS INITIAL ).
      CLEAR: e_carga_recebimento-ds_proprietario.
      SELECT SINGLE name1
        INTO e_carga_recebimento-ds_proprietario
        FROM lfa1 WHERE lifnr EQ me->carga-id_proprietario.
    ELSEIF me->carga-id_proprietario IS INITIAL.
      CLEAR: e_carga_recebimento-ds_proprietario.
    ENDIF.

*-CS2021000253-26.04.2024-#59941-JT-inicio ==============================================
*-Ordem Carregamento terceiros - CPT
    IF me->carga-id_ordem IS NOT INITIAL.
      READ TABLE me->zif_carga~ordem_venda INTO DATA(ws_ordem_venda) INDEX 1.
      TRY.
          DATA(ordem_venda) = zcl_ordem_venda=>zif_ordem_venda~get_instance(
                            )->get_dados_comerciais( IMPORTING e_vbkd       = w_vbkd ).
                            ")->ck_grupo_mercadoria(  EXPORTING i_tipo_frete = w_vbkd-inco1

          IF w_vbkd-inco1 = zif_carga=>st_tp_frete_cpt.
            SELECT SINGLE   agente_frete
              INTO @DATA(lc_agente_frete)
              FROM zlest0185
             WHERE id_ordem = @me->carga-id_ordem
               AND vbeln    = @ws_ordem_venda-nr_ordem_venda.

            IF sy-subrc = 0 AND lc_agente_frete IS NOT INITIAL.
              e_carga_recebimento-id_agent_frete = lc_agente_frete.
              me->carga-id_agent_frete           = lc_agente_frete.
            ENDIF.
          ENDIF.
        CATCH zcx_ordem_venda INTO DATA(lc_ordem_venda).
      ENDTRY.
    ENDIF.
*-CS2021000253-26.04.2024-#59941-JT-fim  ================================================

    "Agente de Frete
    IF ( me->carga-id_agent_frete NE cp_me_carga-id_agent_frete ) OR
       ( me->carga-id_agent_frete IS NOT INITIAL AND e_carga_recebimento-ds_agent_frete IS INITIAL ).
      CLEAR: e_carga_recebimento-ds_agent_frete.
      SELECT SINGLE name1
        INTO e_carga_recebimento-ds_agent_frete
        FROM lfa1 WHERE lifnr EQ me->carga-id_agent_frete.
    ELSEIF me->carga-id_local_coleta IS INITIAL.
      CLEAR: e_carga_recebimento-ds_agent_frete.
    ENDIF.

    "Informações alteradas na classificação
    e_carga_recebimento-nr_res_rr1_rr2 = e_carga_recebimento-nr_resultado_01 + e_carga_recebimento-nr_resultado_02.

    IF me->classificacao-in_srr_origem_partic EQ abap_false.
      CLEAR: e_carga_recebimento-ds_outro_partic,
             e_carga_recebimento-id_outro_partic.
    ENDIF.

    IF ( me->classificacao-id_outro_partic NE cp_me_classificacao-id_outro_partic ) OR
       ( me->classificacao-id_outro_partic IS NOT INITIAL AND e_carga_recebimento-ds_outro_partic IS INITIAL ).
      CLEAR: e_carga_recebimento-ds_outro_partic.
      SELECT SINGLE name1
        INTO e_carga_recebimento-ds_outro_partic
        FROM lfa1 WHERE lifnr EQ me->classificacao-id_outro_partic.
    ELSEIF me->classificacao-id_outro_partic IS INITIAL.
      CLEAR: e_carga_recebimento-ds_outro_partic.
    ENDIF.

    "Classificadora
    IF ( me->classificacao-id_classificadora NE cp_me_classificacao-id_classificadora ) OR
       ( me->classificacao-id_classificadora IS NOT INITIAL AND e_carga_recebimento-ds_classificadora IS INITIAL ).
      CLEAR: e_carga_recebimento-ds_classificadora.
      SELECT SINGLE name1
        INTO e_carga_recebimento-ds_classificadora
        FROM lfa1 WHERE lifnr EQ me->classificacao-id_classificadora.
    ELSEIF me->classificacao-id_classificadora IS INITIAL.
      CLEAR: e_carga_recebimento-ds_classificadora.
    ENDIF.

    IF me->carga-nm_peso_subtotal NE cp_me_carga-nm_peso_subtotal OR
       me->carga-nm_peso_liquido  NE cp_me_carga-nm_peso_liquido.
      me->zif_carga~set_ajustar_rat_desc_geral( ).
    ENDIF.

    me->zif_carga~set_ajustar_rat_ordem_venda( ).

  ENDMETHOD.


  method zif_carga~set_conferido.

    data: "OB_ROMANEIO          TYPE REF TO ZCL_ROMANEIO,
      lc_resultado_rateios type zde_zsdt0001rs_t,
      lc_nm_peso_subtotal	 type zde_nm_peso_subtotal,
      lc_nm_peso_liquido   type zde_nm_peso_liquido,
      lc_nr_qtde_umi       type zde_nr_qtde_umidade,
      lc_nr_qtde_imp       type zde_nr_qtde_impureza,
      lc_nr_qtde_ava       type zde_nr_qtde_avariado,
      lc_nr_qtde_ard       type zde_nr_qtde_ardido,
      lc_nr_qtde_que       type zde_nr_qtde_quebrado,
      lc_nr_qtde_esv       type zde_nr_qtde_esverdeado,
      lc_nr_qtde_car       type zde_nr_qtde_carunchado,
      lc_peso_utilizado    type lips-lfimg,
      lc_peso_ordem_venda  type vbep-wmeng,
      lc_dados             type zde_processo,
      lc_docnum_np         type j_1bnfdoc,
      wa_zmmt0017          type zmmt0017. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940

    data: lc_qtd_fiscal_gerada  type zde_nr_quantidade,
          lc_qtd_fiscal_a_gerar type zde_nr_quantidade.

    data: wa_zmm0023 type zmm0023.

    clear: me->zif_carga~ck_executar_reversao_entrada.      "BUG 34154

    "Somente Deixa Conferir se Exitir Documento Fiscal de Entrada
    describe table me->documento_fiscal lines data(qtd_linhas).

    if qtd_linhas eq 0.
      raise exception type zcx_carga
        exporting
          textid = value #( msgid  = zcx_carga=>zcx_sem_doc_fiscal-msgid
                            msgno  = zcx_carga=>zcx_sem_doc_fiscal-msgno )
          msgty  = 'E'
          msgid  = zcx_carga=>zcx_sem_doc_fiscal-msgid
          msgno  = zcx_carga=>zcx_sem_doc_fiscal-msgno.
    endif.

    case me->carga-tp_status.
      when zif_carga=>st_status_aberto.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_carga_aberta-msgid msgno = zcx_carga=>zcx_carga_aberta-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_aberta-msgid
            msgno  = zcx_carga=>zcx_carga_aberta-msgno.
      when zif_carga=>st_status_conferido.
        if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_carga_conferida-msgid msgno = zcx_carga=>zcx_carga_conferida-msgno )
              msgty  = 'E'
              msgid  = zcx_carga=>zcx_carga_conferida-msgid
              msgno  = zcx_carga=>zcx_carga_conferida-msgno.
        endif.
      when zif_carga=>st_status_cancelada.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_carga_cancelada-msgid msgno = zcx_carga=>zcx_carga_cancelada-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_cancelada-msgid
            msgno  = zcx_carga=>zcx_carga_cancelada-msgno.
    endcase.

    describe table me->documento_fiscal lines data(qt_notas).

    me->zif_carga~check_ob_valida_entrada_fob( ). "// EUDR wbarbosa 22102024 US-153342 - Comentando 16-01-25

    me->carga-tp_status = zif_carga=>st_status_conferido.

    me->zif_carga~get_recuperar_entrada( ).

    data(ck_nota_gerada) = abap_false.

    loop at me->documento_fiscal into data(wa_nn) where mm_mblnr_sobra is not initial.
      ck_nota_gerada = abap_true.
    endloop.

*BUG 179188
    if ck_nota_gerada ne abap_true or me->zif_carga~at_manutencao eq abap_true.
      if qt_notas eq 1.
        "Unico
        read table me->documento_fiscal index 1 assigning field-symbol(<fs_nota>).
        <fs_nota>-id_classificacao = me->classificacao-id_classificacao.
        <fs_nota>-nm_peso_subtotal = me->carga-nm_peso_subtotal.
        <fs_nota>-nm_peso_liquido  = me->carga-nm_peso_liquido.
      else.
        "Desmembrado
        lc_nm_peso_subtotal = 0.
        lc_nm_peso_liquido  = 0.
        lc_nr_qtde_umi      = 0.
        lc_nr_qtde_imp      = 0.
        lc_nr_qtde_ava      = 0.
        lc_nr_qtde_ard      = 0.
        lc_nr_qtde_que      = 0.
        lc_nr_qtde_esv      = 0.
        "LC_NR_QTDE_CAR      = 0.

        "Duplicar Classificação
        loop at me->documento_fiscal assigning <fs_nota>.
          "Gerar Rateio
          me->get_rateia_descontos( exporting i_descontos     = me->resultado
                                              i_peso_liquido  = <fs_nota>-nm_peso_liquido
                                    importing e_peso_subtotal = <fs_nota>-nm_peso_subtotal
                                              e_rateio        = data(resultado) ).

          if <fs_nota>-id_classificacao is not initial.
            read table me->classificacao_notas with key id_classificacao = <fs_nota>-id_classificacao transporting no fields.
            if sy-subrc is initial.
              data(ck_new) = abap_false.
            else.
              ck_new = abap_true.
            endif.
          else.
            ck_new = abap_true.
          endif.

          data(wa_classifica) = me->classificacao.

          "Comentado US 143677 - para manter o ID_CLASSIFICACAO
          if ck_new eq abap_true.
            if <fs_nota>-id_classificacao is initial.
              me->zif_carga~get_new_id_classificao( importing e_id_classificacao = wa_classifica-id_classificacao ).
              <fs_nota>-id_classificacao = wa_classifica-id_classificacao.
            endif.
            append wa_classifica to me->classificacao_notas.
          else.
*            READ TABLE me->classificacao_notas WITH KEY id_classificacao = <fs_nota>-id_classificacao ASSIGNING FIELD-SYMBOL(<fs_ajuste_class>).
*            MOVE-CORRESPONDING me->classificacao TO <fs_ajuste_class>.
*            <fs_ajuste_class>-id_classificacao = <fs_nota>-id_classificacao.
          endif.
          "Comentado US 143677 - para manter o ID_CLASSIFICACAO

          loop at resultado into data(wa_resultado).
            wa_resultado-id_carga         = me->carga-id_carga.
            wa_resultado-id_classificacao = <fs_nota>-id_classificacao.
            append wa_resultado to lc_resultado_rateios.
            case wa_resultado-tp_caracteristica.
              when zif_carga=>st_tp_caract_class_umidade.
                add wa_resultado-nr_quantidade_com to lc_nr_qtde_umi.
              when zif_carga=>st_tp_caract_class_impureza.
                add wa_resultado-nr_quantidade_com to lc_nr_qtde_imp.
              when zif_carga=>st_tp_caract_class_avariado.
                add wa_resultado-nr_quantidade_com to lc_nr_qtde_ava.
              when zif_carga=>st_tp_caract_class_ardido.
                add wa_resultado-nr_quantidade_com to lc_nr_qtde_ard.
              when zif_carga=>st_tp_caract_class_quebrado.
                add wa_resultado-nr_quantidade_com to lc_nr_qtde_que.
              when zif_carga=>st_tp_caract_class_esverdeado.
                add wa_resultado-nr_quantidade_com to lc_nr_qtde_esv.
              when zif_carga=>st_tp_caract_class_carunchado.
                add wa_resultado-nr_quantidade_com to lc_nr_qtde_car.
            endcase.
          endloop.

          add <fs_nota>-nm_peso_subtotal to lc_nm_peso_subtotal.
          add <fs_nota>-nm_peso_liquido  to lc_nm_peso_liquido.

        endloop.

        if lc_nm_peso_subtotal ne me->carga-nm_peso_subtotal.
          if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
            me->carga-tp_status = zif_carga=>st_status_fechado.
          endif.
          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_peso_subtotal-msgid msgno = zcx_carga=>zcx_peso_subtotal-msgno )
              msgty  = 'E'
              msgid  = zcx_carga=>zcx_peso_subtotal-msgid
              msgno  = zcx_carga=>zcx_peso_subtotal-msgno.
        endif.

        if lc_nm_peso_liquido ne me->carga-nm_peso_liquido.
          if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
            me->carga-tp_status = zif_carga=>st_status_fechado.
          endif.
          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_peso_liquido-msgid msgno = zcx_carga=>zcx_peso_liquido-msgno )
              msgty  = 'E'
              msgid  = zcx_carga=>zcx_peso_liquido-msgid
              msgno  = zcx_carga=>zcx_peso_liquido-msgno.
        endif.

        if qt_notas eq 1. "*BUG 179188
          loop at me->resultado into wa_resultado where id_classificacao = me->carga-id_classificacao.
            case wa_resultado-tp_caracteristica.
              when zif_carga=>st_tp_caract_class_umidade.
                if wa_resultado-nr_quantidade_com ne lc_nr_qtde_umi.
                  if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
                    me->carga-tp_status = zif_carga=>st_status_fechado.
                  endif.
                  raise exception type zcx_carga
                    exporting
                      textid = value #( msgid = zcx_carga=>zcx_peso_desc_umidade-msgid msgno = zcx_carga=>zcx_peso_desc_umidade-msgno )
                      msgty  = 'E'
                      msgid  = zcx_carga=>zcx_peso_desc_umidade-msgid
                      msgno  = zcx_carga=>zcx_peso_desc_umidade-msgno.
                endif.
              when zif_carga=>st_tp_caract_class_impureza.
                if wa_resultado-nr_quantidade_com ne lc_nr_qtde_imp.
                  if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
                    me->carga-tp_status = zif_carga=>st_status_fechado.
                  endif.
                  raise exception type zcx_carga
                    exporting
                      textid = value #( msgid = zcx_carga=>zcx_peso_desc_impureza-msgid msgno = zcx_carga=>zcx_peso_desc_impureza-msgno )
                      msgty  = 'E'
                      msgid  = zcx_carga=>zcx_peso_desc_impureza-msgid
                      msgno  = zcx_carga=>zcx_peso_desc_impureza-msgno.
                endif.
              when zif_carga=>st_tp_caract_class_avariado.
                if wa_resultado-nr_quantidade_com ne lc_nr_qtde_ava.
                  if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
                    me->carga-tp_status = zif_carga=>st_status_fechado.
                  endif.
                  raise exception type zcx_carga
                    exporting
                      textid = value #( msgid = zcx_carga=>zcx_peso_desc_avariado-msgid msgno = zcx_carga=>zcx_peso_desc_avariado-msgno )
                      msgty  = 'E'
                      msgid  = zcx_carga=>zcx_peso_desc_avariado-msgid
                      msgno  = zcx_carga=>zcx_peso_desc_avariado-msgno.
                endif.
              when zif_carga=>st_tp_caract_class_ardido.
                if wa_resultado-nr_quantidade_com ne lc_nr_qtde_ard.
                  if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
                    me->carga-tp_status = zif_carga=>st_status_fechado.
                  endif.
                  raise exception type zcx_carga
                    exporting
                      textid = value #( msgid = zcx_carga=>zcx_peso_desc_ardido-msgid msgno = zcx_carga=>zcx_peso_desc_ardido-msgno )
                      msgty  = 'E'
                      msgid  = zcx_carga=>zcx_peso_desc_ardido-msgid
                      msgno  = zcx_carga=>zcx_peso_desc_ardido-msgno.
                endif.
              when zif_carga=>st_tp_caract_class_quebrado.
                if wa_resultado-nr_quantidade_com ne lc_nr_qtde_que.
                  if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
                    me->carga-tp_status = zif_carga=>st_status_fechado.
                  endif.
                  raise exception type zcx_carga
                    exporting
                      textid = value #( msgid = zcx_carga=>zcx_peso_desc_quebrado-msgid msgno = zcx_carga=>zcx_peso_desc_quebrado-msgno )
                      msgty  = 'E'
                      msgid  = zcx_carga=>zcx_peso_desc_quebrado-msgid
                      msgno  = zcx_carga=>zcx_peso_desc_quebrado-msgno.
                endif.
              when zif_carga=>st_tp_caract_class_esverdeado.
                if wa_resultado-nr_quantidade_com ne lc_nr_qtde_esv.
                  if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
                    me->carga-tp_status = zif_carga=>st_status_fechado.
                  endif.
                  raise exception type zcx_carga
                    exporting
                      textid = value #( msgid = zcx_carga=>zcx_peso_desc_esverdeado-msgid msgno = zcx_carga=>zcx_peso_desc_esverdeado-msgno )
                      msgty  = 'E'
                      msgid  = zcx_carga=>zcx_peso_desc_esverdeado-msgid
                      msgno  = zcx_carga=>zcx_peso_desc_esverdeado-msgno.
                endif.
              when zif_carga=>st_tp_caract_class_carunchado.
                if wa_resultado-nr_quantidade_com ne lc_nr_qtde_car.
                  if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
                    me->carga-tp_status = zif_carga=>st_status_fechado.
                  endif.
                  raise exception type zcx_carga
                    exporting
                      textid = value #( msgid = zcx_carga=>zcx_peso_desc_carunchado-msgid msgno = zcx_carga=>zcx_peso_desc_carunchado-msgno )
                      msgty  = 'E'
                      msgid  = zcx_carga=>zcx_peso_desc_carunchado-msgid
                      msgno  = zcx_carga=>zcx_peso_desc_carunchado-msgno.
                endif.
            endcase.
          endloop.
        endif.
      endif.
    endif.


    if me->zif_carga~at_manutencao eq abap_false.

      "Verificar Saldo de Ordem de Venda """"""""""""""""""""""""""""""""""""""""""""""""""""
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      if me->zif_carga~ordem_venda[] is not initial and me->zif_carga~ck_executar_manutencao_entrada eq abap_false.

        loop at me->zif_carga~ordem_venda into data(wa_ordem).

          clear: lc_peso_utilizado, lc_peso_ordem_venda.

          "Busca Item da Ordem de Venda
          select single * into @data(wa_vbap)
            from vbap
           where vbeln eq @wa_ordem-nr_ordem_venda
             and matnr eq @me->carga-id_produto.

          "Busca Remessa Emitidas
          select single sum( lfimg ) into lc_peso_utilizado
            from lips
           where vgbel eq wa_vbap-vbeln
             and matnr eq wa_vbap-matnr.

          "Busca Volume de Material da Ordem de Venda
          select single sum( wmeng ) into lc_peso_ordem_venda
            from vbep
           where vbeln eq wa_vbap-vbeln
             and posnr eq wa_vbap-posnr.

          if wa_ordem-nm_peso_liquido is not initial.
            lc_peso_utilizado = lc_peso_utilizado + wa_ordem-nm_peso_liquido.
          else.
            lc_peso_utilizado = lc_peso_utilizado + me->carga-nm_peso_liquido.
          endif.

          if lc_peso_utilizado gt lc_peso_ordem_venda.

            data(vg_saldo) = lc_peso_ordem_venda - lc_peso_utilizado.

            sy-msgv1 = wa_ordem-nr_ordem_venda.

            write vg_saldo to sy-msgv2.
            condense sy-msgv2 no-gaps.

            if wa_ordem-nm_peso_liquido is not initial.
              write wa_ordem-nm_peso_liquido to sy-msgv3.
              condense sy-msgv3 no-gaps.
            else.
              write me->carga-nm_peso_liquido to sy-msgv3.
              condense sy-msgv3 no-gaps.
            endif.

            if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
              me->carga-tp_status = zif_carga=>st_status_fechado.
            endif.

            raise exception type zcx_carga
              exporting
                textid = value #( msgid = zcx_carga=>zcx_sem_saldo_ord_venda-msgid
                                  msgno = zcx_carga=>zcx_sem_saldo_ord_venda-msgno
                                  attr1 = conv #( sy-msgv1 )
                                  attr2 = conv #( sy-msgv2 )
                                  attr3 = conv #( sy-msgv3 )
                                  )
                msgty  = 'E'
                msgid  = zcx_carga=>zcx_sem_saldo_ord_venda-msgid
                msgno  = zcx_carga=>zcx_sem_saldo_ord_venda-msgno
                msgv1  = sy-msgv1
                msgv2  = sy-msgv2
                msgv3  = sy-msgv3.

          elseif wa_vbap-kwmeng ne wa_vbap-ntgew.

            if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
              me->carga-tp_status = zif_carga=>st_status_fechado.
            endif.

            sy-msgv1 = wa_ordem-nr_ordem_venda.

            raise exception type zcx_carga
              exporting
                textid = value #( msgid = zcx_carga=>zcx_erro_peso_ord_venda-msgid
                                  msgno = zcx_carga=>zcx_erro_peso_ord_venda-msgno
                                  attr1 = conv #( sy-msgv1 )
                                  )
                msgty  = 'E'
                msgid  = zcx_carga=>zcx_erro_peso_ord_venda-msgid
                msgno  = zcx_carga=>zcx_erro_peso_ord_venda-msgno
                msgv1  = sy-msgv1.

          endif.

        endloop.

      endif.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      "Verificar Saldo de Estoque da Filial """"""""""""""""""""""""""""""""""""""""""""""""""
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      "Buscar Centro a Fixar
      select single * into @data(wa_afixar)
        from zsdt_depara_cen
       where vkorg             eq @me->carga-id_bukrs
         and centro_real       eq @me->carga-id_branch
         and tp_centro_virtual eq @zcl_pedido_compra=>st_tp_centro_a_fixar.

      if sy-subrc is not initial.
        raise exception type zcx_carga
          exporting
            textid    = value #( msgid = zcx_carga=>zcx_centro_a_fixar-msgid
                                 msgno = zcx_carga=>zcx_centro_a_fixar-msgno
                                 attr1 = conv #( me->carga-id_branch )
                                 attr2 = 'ZSDT0036' )
            msgid     = zcx_carga=>zcx_centro_a_fixar-msgid
            msgno     = zcx_carga=>zcx_centro_a_fixar-msgno
            msgty     = 'E'
            msgv1     = conv #( me->carga-id_branch )
            msgv2     = 'ZSDT0036'
            transacao = 'ZSDT0036'.
      endif.

      "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - inicio
      clear: wa_zmmt0017.

      if me->classificacao-tp_transgenia eq zif_carga=>st_tp_transgeniase_co.

*        SELECT SINGLE * INTO @WA_ZMMT0017
*          FROM ZMMT0017
*         WHERE MATNR          EQ @ME->CARGA-ID_PRODUTO
*           AND CENTRO_FIXO    EQ @ME->CARGA-ID_BRANCH
*           AND CENTRO_A_FIXAR EQ @WA_AFIXAR-CENTROV_1
*           AND TP_PRODUTO     EQ @ZIF_CARGA=>ST_TP_TRANSGENIASE_CO.

        zcl_depara_centro_fixo_afixar=>zif_depara_centro_fixo_afixar~get_dados_depara(
          exporting
            i_material      = me->carga-id_produto
            i_centro_fixo   = me->carga-id_branch
            i_centro_afixar = wa_afixar-centrov_1
            i_tipo_produto  = zif_carga=>st_tp_transgeniase_co
            i_eudr          = me->carga-eudr
          importing
            e_single_depara = wa_zmmt0017 ).

      else.

*        SELECT SINGLE * INTO @WA_ZMMT0017
*          FROM ZMMT0017
*         WHERE MATNR          EQ @ME->CARGA-ID_PRODUTO
*           AND CENTRO_FIXO    EQ @ME->CARGA-ID_BRANCH
*           AND CENTRO_A_FIXAR EQ @WA_AFIXAR-CENTROV_1
*           AND TP_PRODUTO     EQ 'RR'.

        zcl_depara_centro_fixo_afixar=>zif_depara_centro_fixo_afixar~get_dados_depara(
          exporting
            i_material      = me->carga-id_produto
            i_centro_fixo   = me->carga-id_branch
            i_centro_afixar = wa_afixar-centrov_1
            i_tipo_produto  = 'RR'
            i_eudr          = me->carga-eudr
          importing
            e_single_depara = wa_zmmt0017 ).
      endif.

      if wa_zmmt0017 is initial.

*        SELECT SINGLE * INTO @WA_ZMMT0017
*          FROM ZMMT0017
*         WHERE MATNR          EQ @ME->CARGA-ID_PRODUTO
*           AND CENTRO_FIXO    EQ @ME->CARGA-ID_BRANCH
*           AND CENTRO_A_FIXAR EQ @WA_AFIXAR-CENTROV_1
*           AND TP_PRODUTO     EQ @SPACE.

        zcl_depara_centro_fixo_afixar=>zif_depara_centro_fixo_afixar~get_dados_depara(
          exporting
            i_material      = me->carga-id_produto
            i_centro_fixo   = me->carga-id_branch
            i_centro_afixar = wa_afixar-centrov_1
            i_tipo_produto  = ' '
            i_eudr          = me->carga-eudr
          importing
            e_single_depara = wa_zmmt0017 ).
      endif.

      "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - FIM

      if wa_zmmt0017 is initial.
        "Centro &1 Centro a Fixar &2 e Material &3 sem Depósito! Transação &4!
        raise exception type zcx_carga
          exporting
            textid    = value #( msgid = zcx_carga=>zcx_centro_a_fixar_deposito-msgid
                                 msgno = zcx_carga=>zcx_centro_a_fixar_deposito-msgno
                                 attr1 = conv #( me->carga-id_branch )
                                 attr2 = conv #( wa_afixar-centrov_1 )
                                 attr3 = conv #( me->carga-id_produto )
                                 attr4 = 'ZMM0017' )
            msgid     = zcx_carga=>zcx_centro_a_fixar_deposito-msgid
            msgno     = zcx_carga=>zcx_centro_a_fixar_deposito-msgno
            msgty     = 'E'
            msgv1     = conv #( me->carga-id_branch )
            msgv2     = conv #( wa_afixar-centrov_1 )
            msgv3     = conv #( me->carga-id_produto )
            msgv4     = 'ZMM0017'
            transacao = 'ZMM0017'.
      endif.

      select single * into wa_zmm0023
      from zmm0023 as mm
      where mm~werks  eq me->carga-id_branch
      and mm~matnr = me->carga-id_produto
      and mm~matnr ne ''  "SMC
      and mm~cwerks eq ( select max( mm2~cwerks ) from zmm0023 as mm2 where mm2~werks eq mm~werks  and mm2~matnr eq mm~matnr ).

*SMC
      if sy-subrc ne 0.
        select single matkl into @data(_matkl)
            from  mara where matnr = @carga-id_produto.

        select single * into wa_zmm0023
           from zmm0023 as mm
            where mm~werks  eq me->carga-id_branch
            and mm~matkl = _matkl
            and mm~matkl <> ''
            and mm~cwerks eq ( select max( mm2~cwerks ) from zmm0023 as mm2 where mm2~werks eq mm~werks  and mm2~matkl eq _matkl ).
      endif.
*SMC

      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
      if
        sy-subrc ne 0.
        message e897(sd) with  'Falta parâmetros na ZMM0029. '
                                  'Favor entrar em contato com '
                                   'a área de controladoria e estoque. '.
      endif.
      ""141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC

      if wa_zmm0023-status ne 'A' and me->zif_carga~ck_executar_manutencao_entrada eq abap_false.

        "Buscar Saldo Estoque Atual
        select single * into @data(wa_mchb_filial)
          from mchb
         where werks eq @me->carga-id_branch
           and lgort eq @wa_zmmt0017-lgort
           and charg eq @me->carga-nr_safra
           and matnr eq @me->carga-id_produto.

        if me->carga-nr_safra <= '2019'.

          "Buscar Saldo Estoque Atual
          select single * into @data(wa_mchb)
            from mchb
           where werks eq @wa_zmmt0017-centro_a_fixar
             and lgort eq @wa_zmmt0017-lgort
             and charg eq @me->carga-nr_safra
             and matnr eq @me->carga-id_produto.

        else.

          data(new_charg) = |{ me->carga-nr_safra }_{ me->carga-id_branch }|.

          "Buscar Saldo Estoque Atual
          select single * into @wa_mchb
            from mchb
           where werks eq @wa_zmmt0017-centro_a_fixar
             and lgort eq @wa_zmmt0017-lgort
             and charg eq @new_charg
             and matnr eq @me->carga-id_produto.

        endif.

        me->zif_carga~get_recuperar_entrada( ).

        lc_qtd_fiscal_gerada = lc_qtd_fiscal_a_gerar = 0.

*        LOOP AT ME->DOCUMENTO_FISCAL INTO DATA(WA_NOTA) WHERE MM_MBLNR IS NOT INITIAL.
*          "Não vai gerar estoque já gerou
*          LC_QTD_FISCAL_GERADA = LC_QTD_FISCAL_GERADA + WA_NOTA-NR_QUANTIDADE.
*        ENDLOOP.

        loop at me->documento_fiscal into data(wa_nota) where mm_mblnr is initial.
          "Vai gerar estoque
          lc_qtd_fiscal_a_gerar = lc_qtd_fiscal_a_gerar + wa_nota-nr_quantidade.
        endloop.

        data(lc_saldo) = wa_mchb_filial-clabs + wa_mchb-clabs.

*        "Quantodade Já Gerada de Enatrada
*        LC_SALDO = LC_SALDO - LC_QTD_FISCAL_GERADA.

        "Quantidade Será Gerada de Saída
        lc_saldo = lc_saldo - me->carga-nm_peso_subtotal.

*-CS2022000332-#78064-07.06.2022-JT-inicio
        select single matkl
          into @data(l_matkl)
          from mara
         where matnr = @me->carga-id_produto.

        if sy-subrc = 0.
          select single * into @data(wa_setleaf1)
            from setleaf
           where setname eq 'EMBALAGEM'
             and valfrom eq @l_matkl.
          data(l_subrc1) = sy-subrc.
        else.
          l_subrc1 = sy-subrc.
        endif.
*-CS2022000332-#78064-07.06.2022-JT-fim

        select single * into @data(wa_setleaf2)
          from setleaf
         where setname eq 'RESIDUO'
           and valfrom eq @me->carga-id_produto.
        data(l_subrc2) = sy-subrc.

*       IF sy-subrc IS INITIAL.
        if l_subrc1 is initial or l_subrc2 is initial.  "*-CS2022000332-#78064-07.06.2022-JT-inicio
          "Quantidade Será Gerada de Entrada de Sobra
          lc_saldo = lc_saldo + ( me->carga-nm_peso_subtotal - me->carga-nm_peso_liquido ).
        endif.

        "Quantidade Será Gerada de Entrada de Estoque
        lc_saldo = lc_saldo + lc_qtd_fiscal_a_gerar.

        if lc_saldo lt 0.

          write lc_saldo to sy-msgv1.
          condense sy-msgv1 no-gaps.

          "Saldo Estoque Insuficiente em &MSGV1& - Centro &MSGV2& Deposito &MSGV3& Lote &MSGV4&!

          concatenate me->carga-id_branch '/' wa_zmmt0017-centro_a_fixar into sy-msgv2.

          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_saldo_estoque-msgid
                                msgno = zcx_carga=>zcx_saldo_estoque-msgno
                                attr1 = conv #( sy-msgv1 )
                                attr2 = conv #( sy-msgv2 )
                                attr3 = conv #( wa_zmmt0017-lgort )
                                attr4 = conv #( me->carga-nr_safra ) )
              msgty  = 'E'
              msgid  = zcx_carga=>zcx_saldo_estoque-msgid
              msgno  = zcx_carga=>zcx_saldo_estoque-msgno
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = conv #( wa_zmmt0017-lgort )
              msgv4  = conv #( me->carga-nr_safra ).

        endif.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      endif.

    endif.

    if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
      if not ( me->carga-ck_enviado_opus eq abap_true and me->carga-ck_recebido_opus eq abap_false ).
        me->carga-dt_conferencia = sy-datlo.
        me->carga-hr_conferencia = sy-timlo.
        me->carga-us_conferencia = sy-uname.
      endif.
    endif.

    me->zif_carga~validar_registro( EXPORTING i_processo = 'CONFERENCIA' importing  e_validou  = data(e_validou) ).

    if not e_validou eq abap_true.
      if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
        me->carga-tp_status = zif_carga=>st_status_fechado.
        if me->carga-id_carga is not initial.
          delete from zsdt0001 where id_carga eq me->carga-id_carga.
          commit work.
        endif.
      endif.
      raise exception type zcx_carga
        exporting
          textid = value #( msgid = sy-msgid msgno = sy-msgno
                            attr1 = conv #( sy-msgv1 ) attr2 = conv #( sy-msgv2 ) attr3 = conv #( sy-msgv3 ) attr4 = conv #( sy-msgv4 ) )
          msgty  = 'E'
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    endif.

    loop at lc_resultado_rateios into wa_resultado.
      read table me->resultado assigning field-symbol(<fs_resultado>)
      with key id_classificacao  = wa_resultado-id_classificacao
               tp_caracteristica = wa_resultado-tp_caracteristica.
      if sy-subrc is initial.
        move-corresponding wa_resultado to <fs_resultado>.
      else.
        append wa_resultado to me->resultado.
      endif.
    endloop.

    try .

        if zif_carga~solicitacao_manutencao is not initial.
          select single * into @data(wa_zsdt0001acb)
            from zsdt0001acb
           where id_solicitacao eq @me->zif_carga~solicitacao_manutencao-id_solicitacao.

          if wa_zsdt0001acb-ck_entrada_gerou eq abap_false.

*-CS2021000183 - 31.03.2022 - JT - inicio
            if me->zif_carga~at_manutencao eq abap_false.
              me->zif_carga~set_gerar_nota_propria( ).
            endif.
*-CS2021000183 - 31.03.2022 - JT - fim

            "Gerar Movimento de Entrada na manutenção
            me->zif_carga~set_gerar_romaneio_entrada(
              )->set_gerar_entrada_estoque(
              )->set_processar_entrada( importing e_gerou_entrada = data(e_gerou_entrada)
              ).

            if e_gerou_entrada eq abap_true.
              update zsdt0001acb
                 set ck_entrada_gerou = abap_true
               where id_solicitacao eq me->zif_carga~solicitacao_manutencao-id_solicitacao.
              commit work and wait.
            endif.
          else.
            e_gerou_entrada = abap_true.
          endif.

        else.
*-CS2021000183 - 31.03.2022 - JT - inicio
          if me->zif_carga~at_manutencao eq abap_false.
            me->zif_carga~set_gerar_nota_propria( ).
          endif.
*-CS2021000183 - 31.03.2022 - JT - fim

          "Gerar Movimento de Entrada
          me->zif_carga~set_gerar_romaneio_entrada(
          )->set_gerar_entrada_estoque(
          )->set_processar_entrada( importing e_gerou_entrada = e_gerou_entrada
          ).
        endif.

*        ME->ZIF_CARGA~SET_GERAR_ROMANEIO_ENTRADA(
*        )->SET_GERAR_ENTRADA_ESTOQUE(
*        )->SET_PROCESSAR_ENTRADA( IMPORTING E_GEROU_ENTRADA = DATA(E_GEROU_ENTRADA)
*        ).

        if me->zif_carga~ck_executar_manutencao_entrada eq abap_true.
          "Gravar para Processamento da Sobre ter a informação
          me->zif_carga~gravar_registro( ).
        endif.

        me->zif_carga~set_processar_sobra_fob(
        )->set_gerar_romaneio_saida(
        )->send_carga_to_opus( importing e_code = data(e_code)
        )->set_conferir_solic_manut( importing e_aprovacao_automatica = data(e_aprovacao_automatica)
        ).

        if e_gerou_entrada eq abap_false and me->zif_carga~at_manutencao eq abap_false.
          me->carga-tp_status = zif_carga=>st_status_fechado.
        endif.

      catch zcx_carga into data(ex_carga).

        if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
          me->carga-tp_status = zif_carga=>st_status_fechado.
        endif.

        me->zif_carga~gravar_registro( importing e_gravou = data(e_gravou) ).
        ex_carga->published_erro( exporting i_msgty = 'S' i_msgty_display = 'E' ).
        exit.

      catch zcx_cadastro into data(ex_cadastro).

*        ME->ZIF_CARGA~SET_LIMPA_ROMANEIO_SAIDA( ).
        if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
          me->carga-tp_status = zif_carga=>st_status_fechado.
        endif.
        me->zif_carga~gravar_registro( importing e_gravou = e_gravou ).
        ex_cadastro->published_erro( exporting i_msgty = 'S' i_msgty_display = 'E' ).
        exit.

      catch zcx_pedido_compra_exception into data(ex_pedido).

*        ME->ZIF_CARGA~SET_LIMPA_ROMANEIO_SAIDA( ).
        if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
          me->carga-tp_status = zif_carga=>st_status_fechado.
        endif.
        me->zif_carga~gravar_registro( importing e_gravou = e_gravou ).
        ex_pedido->published_erro( exporting i_msgty = 'S' i_msgty_display = 'E' ).
        exit.

      catch zcx_parceiros into data(ex_parceiros).

*        ME->ZIF_CARGA~SET_LIMPA_ROMANEIO_SAIDA( ).
        if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
          me->carga-tp_status = zif_carga=>st_status_fechado.
        endif.
        me->zif_carga~gravar_registro( importing e_gravou = e_gravou ).
        ex_parceiros->published_erro( exporting i_msgty = 'S' i_msgty_display = 'E' ).
        exit.

      catch zcx_miro_exception into data(ex_miro).

*        ME->ZIF_CARGA~SET_LIMPA_ROMANEIO_SAIDA( ).
        if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
          me->carga-tp_status = zif_carga=>st_status_fechado.
        endif.
        me->zif_carga~gravar_registro( importing e_gravou = e_gravou ).
        ex_miro->published_erro( exporting i_msgty = 'S' i_msgty_display = 'E' ).
        exit.

      catch zcx_soft_expert_workflow into data(ex_se_worlflow).

        if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
          me->carga-tp_status = zif_carga=>st_status_fechado.
        endif.
        me->zif_carga~gravar_registro( importing e_gravou = e_gravou ).
        ex_se_worlflow->published_erro( exporting i_msgty = 'S' i_msgty_display = 'E' ).
        exit.

      catch zcx_job into data(ex_job).

        if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
          me->carga-tp_status = zif_carga=>st_status_fechado.
        endif.
        me->zif_carga~gravar_registro( importing e_gravou = e_gravou ).
        ex_job->published_erro( exporting i_msgty = 'S' i_msgty_display = 'E' ).
        exit.

    endtry.

    me->zif_carga~gravar_registro( importing e_gravou = e_gravou ).

    if e_gravou eq abap_true.

      e_conferiu = abap_true.

      message s098.

      "Emitir Documentos Saída
      if me->zif_carga~at_manutencao eq abap_false and i_proximo_passo_automatico eq abap_true.
        me->zif_carga~set_emitir_doc_saidas( ).
      endif.

      if me->zif_carga~at_manutencao eq abap_true and
         me->zif_carga~ck_executar_manutencao_entrada eq abap_false and
         e_aprovacao_automatica eq abap_true.

        "LN	Tipo Aceite de Solicitação de Manutenção - Local de Negócio
        "FI	Tipo Aceite de Solicitação de Manutenção - Fiscal
        "CO	Tipo Aceite de Solicitação de Manutenção - Comercial

        "A  Solicitação Aprovada
        "R  Solicitação Recusada
        "W  Solicitação Em Espera de Aprovação
        "S  Solicitação não gera Aprovação

        if me->zif_carga~solicitacao_manutencao-rs_aceite_filial eq zif_carga=>st_rs_aceite_manut_espera.
          me->zif_carga~set_aceite_soli_manutencao(
            exporting
              i_tp_aprovacao = zif_carga=>st_tp_aceite_manut_filial   " Tipo de Aceite de Solicitação de Manutenção
              i_tp_resposta  = zif_carga=>st_rs_aceite_manut_aprovada " Tipo de Resposta
              "I_MOTIVO_RESPOSTA           = ''    " Motivo Informado na Resposta
          ).
        endif.

        if me->zif_carga~solicitacao_manutencao-rs_aceite_fiscal eq zif_carga=>st_rs_aceite_manut_espera.
          me->zif_carga~set_aceite_soli_manutencao(
            exporting
              i_tp_aprovacao = zif_carga=>st_tp_aceite_manut_fiscal   " Tipo de Aceite de Solicitação de Manutenção
              i_tp_resposta  = zif_carga=>st_rs_aceite_manut_aprovada " Tipo de Resposta
              "I_MOTIVO_RESPOSTA           = ''    " Motivo Informado na Resposta
          ).
        endif.

        if me->zif_carga~solicitacao_manutencao-rs_aceite_comercial eq zif_carga=>st_rs_aceite_manut_espera.
          me->zif_carga~set_aceite_soli_manutencao(
            exporting
              i_tp_aprovacao = zif_carga=>st_tp_aceite_manut_comercial " Tipo de Aceite de Solicitação de Manutenção
              i_tp_resposta  = zif_carga=>st_rs_aceite_manut_aprovada  " Tipo de Resposta
              "I_MOTIVO_RESPOSTA           = ''    " Motivo Informado na Resposta
          ).
        endif.
      endif.
    endif.

  endmethod.


  METHOD ZIF_CARGA~SET_CONFERIR_SOLIC_MANUT.

    DATA: E_NEW_WORKFLOW TYPE ZDE_SE_NEW_WORFLOW_RET.
    DATA: LC_INJECT      TYPE REF TO ZCL_SE_WL_MANUTENCAO_ROMANEIO.
    DATA: E_ALTERACOES   TYPE ZDE_ALTERACOES_CARGA.
    DATA: ROMANEIO       TYPE REF TO ZCL_ROMANEIO.

    R_INSTANCE = ME.

    CHECK ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_TRUE.
    CHECK ME->ZIF_CARGA~CK_EXECUTAR_MANUTENCAO_ENTRADA EQ ABAP_FALSE.

    "Verificar Romaneios de Entrada se Podem ser alterados
    ME->ZIF_CARGA~GET_ROMANEIO_ENTRADA( EXPORTING I_ID_CARGA = ME->ZIF_CARGA~CARGA-ID_CARGA  I_FORCE = ABAP_TRUE IMPORTING E_ROMANEIOS = DATA(E_ROMANEIOS) ).

    LOOP AT E_ROMANEIOS INTO DATA(WA_ROMANEIOS).
      TRY .

          ROMANEIO = ZCL_ROMANEIO=>GET_INSTANCE( ).
          ROMANEIO->SET_REGISTRO( I_ID_REGISTRO = WA_ROMANEIOS-CH_REFERENCIA ).
          ROMANEIO->GET_CONSULTA_STATUS_OPUS(
            IMPORTING
              E_STATUS            = DATA(E_STATUS)               " Status do Romaneio
              E_BLOQUEADO         = DATA(E_BLOQUEADO)
              E_MENSAGEM_BLOQUEIO = DATA(E_MENSAGEM_BLOQUEIO) ).

          IF E_BLOQUEADO = 'X'.
            ME->GERA_ERRO_GERAL( I_TEXTO = E_MENSAGEM_BLOQUEIO ).
          ENDIF.

        CATCH ZCX_ROMANEIO INTO DATA(EX_ROMANEIO).

          RAISE EXCEPTION TYPE ZCX_CARGA
            EXPORTING
              TEXTID = VALUE #( MSGID = EX_ROMANEIO->MSGID
                                MSGNO = EX_ROMANEIO->MSGNO
                                ATTR1 = CONV #( EX_ROMANEIO->MSGV1 )
                                ATTR2 = CONV #( EX_ROMANEIO->MSGV2 )
                                ATTR3 = CONV #( EX_ROMANEIO->MSGV3 )
                                ATTR4 = CONV #( EX_ROMANEIO->MSGV4 ) )
              MSGID  = EX_ROMANEIO->MSGID
              MSGNO  = EX_ROMANEIO->MSGNO
              MSGTY  = 'E'
              MSGV1  = EX_ROMANEIO->MSGV1
              MSGV2  = EX_ROMANEIO->MSGV2
              MSGV3  = EX_ROMANEIO->MSGV3
              MSGV4  = EX_ROMANEIO->MSGV4.

      ENDTRY.

    ENDLOOP.

    LC_INJECT = CAST #( ZCL_SE_WL_MANUTENCAO_ROMANEIO=>ZIF_SOFT_EXPERT_WS_INJECT~GET_INSTANCE( ) ).

    ME->ZIF_CARGA~VERIF_ALTERACAO_MANUT_ROMANEIO(
      IMPORTING
        E_CK_ACEITE_FILIAL    = ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_ACEITE_FILIAL       " Aceite Filial
        E_CK_ACEITE_COMERCIAL = ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_ACEITE_COMERCIAL    " Aceite Fiscal
        E_CK_ACEITE_FISCAL    = ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_ACEITE_FISCAL       " Aceite Comercial
        E_CARGA_ORIGINAL      = LC_INJECT->AT_CARGA_ORIGINAL                                " Interface para Carga
        E_ALTERACOES          = E_ALTERACOES
        E_OBS_ALTERACAO       = LC_INJECT->OBS_ALTERACAO
    ).

    MOVE-CORRESPONDING E_ALTERACOES TO ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO.

    LC_INJECT->AT_CARGA = ME.

    TRY .
        ME->ZIF_CARGA~GET_CK_EXCLUIR_ROMANEIO_SAIDA( ).
        E_APROVACAO_AUTOMATICA = ABAP_TRUE.
      CATCH ZCX_CARGA.    "
        ZCL_SOFT_EXPERT_WORKFLOW=>ZIF_SOFT_EXPERT_WORKFLOW~GET_INSTANCE(
          )->SET_PROCESS_WORKFLOW_INJECT( EXPORTING I_INJECT = LC_INJECT
          )->CREATE_NEW_WORKFLOW( IMPORTING E_NEW_WORKFLOW = E_NEW_WORKFLOW
          ).
        E_APROVACAO_AUTOMATICA = ABAP_FALSE.
    ENDTRY.

    IF ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_ACEITE_FILIAL EQ ABAP_TRUE.
      ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL = ZIF_CARGA=>ST_RS_ACEITE_MANUT_ESPERA.
    ELSE.
      ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL = ZIF_CARGA=>ST_RS_ACEITE_MANUT_NAO_GERA.
    ENDIF.

    IF ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_ACEITE_COMERCIAL EQ ABAP_TRUE.
      ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_COMERCIAL = ZIF_CARGA=>ST_RS_ACEITE_MANUT_ESPERA.
    ELSE.
      ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_COMERCIAL = ZIF_CARGA=>ST_RS_ACEITE_MANUT_NAO_GERA.
    ENDIF.

    IF ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-CK_ACEITE_FISCAL EQ ABAP_TRUE.
      ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FISCAL = ZIF_CARGA=>ST_RS_ACEITE_MANUT_ESPERA.
    ELSE.
      ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FISCAL = ZIF_CARGA=>ST_RS_ACEITE_MANUT_NAO_GERA.
    ENDIF.

    CASE E_APROVACAO_AUTOMATICA.
      WHEN ABAP_FALSE.
        ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL = ZIF_CARGA=>ST_RS_ACEITE_MANUT_ESPERA.
        ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-TP_STATUS        = ZIF_CARGA=>ST_STATUS_MANUT_ENVIADO.
        ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_STATUS        = E_NEW_WORKFLOW-STATUS.
        ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_CODE          = E_NEW_WORKFLOW-CODE.
        ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_DETAIL        = E_NEW_WORKFLOW-DETAIL.
        ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_RECORDKEY     = E_NEW_WORKFLOW-RECORDKEY.
        ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-SE_RECORDID      = E_NEW_WORKFLOW-RECORDID.
        ME->ZIF_CARGA~CK_ALTEROU = ABAP_TRUE.
      WHEN ABAP_TRUE.
        ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-RS_ACEITE_FILIAL = ZIF_CARGA=>ST_RS_ACEITE_MANUT_ESPERA.
        ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-TP_STATUS        = ZIF_CARGA=>ST_STATUS_MANUT_ENVIADO.
        ME->ZIF_CARGA~CK_ALTEROU = ABAP_TRUE.
    ENDCASE.

    CLEAR: LC_INJECT.

  ENDMETHOD.


  METHOD zif_carga~set_cria_manutencao.

    r_instance = me.

    IF i_id_carga IS NOT INITIAL.

      SELECT SINGLE *
        FROM zsdt0001rd INTO @DATA(lwa_zsdt0001)
       WHERE id_carga EQ @i_id_carga.

      IF sy-subrc EQ 0.
        MESSAGE s293 INTO DATA(lva_msg).
        me->zif_carga~gera_erro_geral( i_texto = CONV #( lva_msg ) ).
      ENDIF.

    ENDIF.

    me->set_registro( i_id_carga  = i_id_carga i_no_enqueue = abap_true ).

*    DATA(NOTAS) = ME->ZIF_CARGA~DOCUMENTO_FISCAL[].
*    DELETE NOTAS WHERE DOCNUM IS INITIAL.
*    IF NOTAS[] IS NOT INITIAL.
*      SELECT * INTO TABLE @DATA(IT_EXPORTACAO)
*        FROM ZDOC_NF_PRODUTOR
*         FOR ALL ENTRIES IN @NOTAS
*       WHERE DOCNUM_PROD EQ @NOTAS-DOCNUM.
*    ENDIF.
*
*    IF IT_EXPORTACAO[] IS NOT INITIAL.
*      READ TABLE IT_EXPORTACAO INTO DATA(WA_EXPORTACAO) INDEX 1.
*
*      RAISE EXCEPTION TYPE ZCX_CARGA
*        EXPORTING
*          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NF_EXPORTADA-MSGID
*                            MSGNO = ZCX_CARGA=>ZCX_NF_EXPORTADA-MSGNO
*                            ATTR1 = WA_EXPORTACAO-DOCNUM_PROD
*                            ATTR2 = WA_EXPORTACAO-VBELN )
*          MSGTY  = 'E'
*          MSGID  = ZCX_CARGA=>ZCX_NF_EXPORTADA-MSGID
*          MSGNO  = ZCX_CARGA=>ZCX_NF_EXPORTADA-MSGNO
*          MSGV1  = CONV #( WA_EXPORTACAO-DOCNUM_PROD )
*          MSGV2  = CONV #( WA_EXPORTACAO-VBELN ).
*    ENDIF.

    me->zif_carga~at_manutencao   = abap_true.
    CLEAR: me->zif_carga~carga-tp_status.

    me->zif_carga~set_abrir(
      EXPORTING i_nr_safra     = me->carga-nr_safra
                i_id_bukrs     = me->carga-id_bukrs
                i_id_branch    = me->carga-id_branch
                i_tipo_produto = me->carga-tp_produto_carga
    ).

    DELETE me->resultado WHERE id_classificacao NE me->carga-id_classificacao.

*    LOOP AT ME->RESULTADO ASSIGNING FIELD-SYMBOL(<FS_RESULTADO>).
*      CLEAR: <FS_RESULTADO>-ID_CLASSIFICACAO.
*    ENDLOOP.
*
*    LOOP AT ME->DOCUMENTO_FISCAL ASSIGNING FIELD-SYMBOL(<FS_NOTA>).
*      CLEAR: <FS_NOTA>-ID_CLASSIFICACAO.
*    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM me->resultado COMPARING ALL FIELDS.

    CLEAR:
    "ME->CARGA-ID_CLASSIFICACAO,
    "ME->CLASSIFICACAO-ID_CLASSIFICACAO,
*    me->classificacao_notas[],   BUG174959
    me->zif_carga~solicitacao_manutencao.

    me->zif_carga~solicitacao_manutencao-nr_safra            = me->carga-nr_safra.
    me->zif_carga~solicitacao_manutencao-id_bukrs            = me->carga-id_bukrs.
    me->zif_carga~solicitacao_manutencao-id_branch           = me->carga-id_branch.
    me->zif_carga~solicitacao_manutencao-tp_status           = zif_carga=>st_status_manut_aberto.
    me->zif_carga~solicitacao_manutencao-ck_aceite_filial    = abap_false.
    me->zif_carga~solicitacao_manutencao-ck_aceite_fiscal    = abap_false.
    me->zif_carga~solicitacao_manutencao-ck_aceite_comercial = abap_false.
    me->zif_carga~solicitacao_manutencao-rs_aceite_filial    = zif_carga=>st_rs_aceite_manut_nao_gera.
    me->zif_carga~solicitacao_manutencao-rs_aceite_fiscal    = zif_carga=>st_rs_aceite_manut_nao_gera.
    me->zif_carga~solicitacao_manutencao-rs_aceite_comercial = zif_carga=>st_rs_aceite_manut_nao_gera.

    me->zif_carga~ck_digitado_umidade    = abap_true.
    me->zif_carga~ck_digitado_impureza   = abap_true.
    me->zif_carga~ck_digitado_ardido     = abap_true.
    me->zif_carga~ck_digitado_avariado   = abap_true.
    me->zif_carga~ck_digitado_quebrado   = abap_true.
    me->zif_carga~ck_digitado_esverdeado = abap_true.
    me->zif_carga~ck_digitado_carunchado = abap_true.

  ENDMETHOD.


  METHOD zif_carga~set_dados_nota_propria.

    DATA: l_vcfop          TYPE j_1bcfop,
          l_valor_nf       TYPE ZTX_PAR_DOLAR, "zde_nr_valor, BUG171257
          l_valor_prop     TYPE zde_nr_valor,
          w_nfref          TYPE zrefnfe,
          w_det            TYPE zdet_nitem,
          w_pag            TYPE zdetpag,
          w_detpag         TYPE zdetpag_simetrya,
          w_branch_address TYPE bapi0002_3.

    r_instancia = me.

    FREE: e_dados, e_objkey_np,
          w_nfref, w_det, w_pag, w_detpag.

    SELECT *
      INTO @DATA(w_zsdt0001tetn)
        UP TO 1 ROWS
      FROM zsdt0001tetn
     WHERE id_entrada = @i_doc_fiscal-id_entrada.
    ENDSELECT.

    SELECT ct_nota, tp_pessoa
      FROM zsdt0001te
      INTO @DATA(w_zsdt0001te)
        UP TO 1 ROWS
     WHERE id_entrada = @i_doc_fiscal-id_entrada
       AND id_empresa = @me->carga-id_bukrs.
    ENDSELECT.

    SELECT *
      INTO @DATA(w_j_1bbranch)
      FROM j_1bbranch
        UP TO 1 ROWS
     WHERE bukrs  = @me->carga-id_bukrs
       AND branch = @me->carga-id_branch.
    ENDSELECT.

    SELECT *
      INTO @DATA(w_adrc)
      FROM adrc
        UP TO 1 ROWS
     WHERE addrnumber = @w_j_1bbranch-adrnr.
    ENDSELECT.

    SELECT *
      INTO @DATA(w_lfa1)
      FROM lfa1
        UP TO 1 ROWS
     WHERE lifnr = @i_doc_fiscal-id_fornecedor.
    ENDSELECT.

    SELECT *
      INTO @DATA(w_adrc_lfa1)
      FROM adrc
        UP TO 1 ROWS
     WHERE addrnumber = @w_lfa1-adrnr.
    ENDSELECT.

    SELECT *
      INTO @DATA(w_makt)
      FROM makt
        UP TO 1 ROWS
     WHERE matnr  = @me->carga-id_produto
       AND spras  = @sy-langu.
    ENDSELECT.

    SELECT *
      INTO @DATA(w_marc)
      FROM marc
        UP TO 1 ROWS
     WHERE matnr  = @me->carga-id_produto
       AND werks  = @me->carga-id_branch.
    ENDSELECT.

    IF w_adrc-region = w_lfa1-regio.
      l_vcfop = w_zsdt0001tetn-cfop_dentro_uf.
    ELSE.
      l_vcfop = w_zsdt0001tetn-cfop_fora_uf.
    ENDIF.

    IF l_vcfop IS NOT INITIAL.
      l_vcfop = l_vcfop && 'AA'.
    ENDIF.

*--------------------------------------------
*-- valida CFOP
*--------------------------------------------
    IF l_vcfop IS INITIAL.
      RAISE EXCEPTION TYPE zcx_error
        EXPORTING
          textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                            msgno = zcx_error=>zcx_erro_geral-msgno
                            attr1 = CONV #( 'CFOP não encontrado,' )
                            attr2 = CONV #( 'na tabela ZSDT0001TETN.' ) )
          msgty  = 'E'
          msgid  = zcx_error=>zcx_erro_geral-msgid
          msgno  = zcx_error=>zcx_erro_geral-msgno
          msgv1  = CONV #( 'CFOP não encontrado,' )
          msgv2  = CONV #( 'na tabela ZSDT0001TETN.' ).
    ENDIF.

    SELECT *
      INTO @DATA(w_j_1bagnt)
      FROM j_1bagnt
        UP TO 1 ROWS
     WHERE spras = @sy-langu
       AND cfop  = @l_vcfop.
    ENDSELECT.

*--------------------------------------------
*-- Gerar Impostos Retidos
*--------------------------------------------
    SELECT SINGLE *
      INTO @DATA(w_zsdt0001mt)
      FROM zsdt0001mt
     WHERE matnr = @me->carga-id_produto.

    CALL FUNCTION 'BAPI_BRANCH_GETDETAIL'
      EXPORTING
        company        = me->carga-id_bukrs
        branch         = me->carga-id_branch
      IMPORTING
        branch_address = w_branch_address.

    IF me->carga-dt_movimento IS INITIAL.
      me->carga-dt_movimento = sy-datlo.
    ENDIF.

    SELECT *
      INTO TABLE @DATA(t_zsdt0001im)
      FROM zsdt0001im
     WHERE id_tipo_nota  = @w_zsdt0001tetn-id_tipo_nota
       AND tp_grupo_ctb  = @w_zsdt0001mt-tp_grupo_ctb
       AND regio         = @w_branch_address-region
       AND dt_inicial   <= @me->carga-dt_movimento
       AND dt_final     >= @me->carga-dt_movimento.

*--------------------------------------------
*-- SEQUENCIADOR
*--------------------------------------------
    me->zif_carga~get_new_id_nota_propria( IMPORTING e_id_nota_propria = DATA(e_id_np) ).

    e_dados-processo-infosistemaorigem-id           = 'CGNP' && e_id_np.

*--------------------------------------------
*-- INTGNFE
*--------------------------------------------
    e_dados-processo-intgnfe-ide-natop              = w_j_1bagnt-cfotxt.
    e_dados-processo-intgnfe-ide-indpag             = '2'. "FORMA DE PAGAMENTO 0-A VISTA, 1-A PRAZO, 2-OUTROS
    e_dados-processo-intgnfe-ide-natop              = w_j_1bagnt-cfotxt.
    e_dados-processo-intgnfe-ide-serie              = ''.
    e_dados-processo-intgnfe-ide-nnf                = ''.
    e_dados-processo-intgnfe-ide-demi               = sy-datum(4) && '.' && sy-datum+4(2) && '.' && sy-datum+6(2).
    e_dados-processo-intgnfe-ide-tpnf               = '0'. "Entrada

    CASE i_doc_fiscal-cfop(1).
      WHEN '1' OR '5'.
        e_dados-processo-intgnfe-ide-iddest         = '1'.
      WHEN '2' OR '6'.
        e_dados-processo-intgnfe-ide-iddest         = '2'.
      WHEN OTHERS.
        e_dados-processo-intgnfe-ide-iddest         = '3'.
    ENDCASE.

    e_dados-processo-intgnfe-ide-cmunfg             = w_adrc-taxjurcode+4(10).
    e_dados-processo-intgnfe-ide-finnfe             = '1'.
    e_dados-processo-intgnfe-ide-indfinal           = '0'.  " INDICA OPERACÃO COM CONSUMIDOR FINAL -->
    e_dados-processo-intgnfe-ide-indpres            = '9'.  " INDICACÃO DE ATENDIMENTO PRESENCIAL -->
    e_dados-processo-intgnfe-ide-dhemi              = sy-datum(4) && '.' && sy-datum+4(2) && '.' && sy-datum+6(2) &&
                                                      sy-uzeit(2) && ':' && sy-uzeit+2(2) && ':' && sy-uzeit+4(2).
    e_dados-processo-intgnfe-ide-sap_categoria_nota = w_zsdt0001te-ct_nota.

*--------------------------------------------
*-- NFREF
*--------------------------------------------
    w_nfref-refnfe                                  = i_doc_fiscal-nr_chave_nfe.

    IF i_doc_fiscal-nr_chave_nfe IS INITIAL.
      w_nfref-refnfp-siglauf                        = w_adrc_lfa1-region.
      w_nfref-refnfp-cuf                            = ''.
      w_nfref-refnfp-aamm                           = i_doc_fiscal-dt_emissao(4) && i_doc_fiscal-dt_emissao+4(2).
      w_nfref-refnfp-cnpj                           = ''.
      w_nfref-refnfp-cpf                            = w_lfa1-stcd2.
      w_nfref-refnfp-ie                             = i_doc_fiscal-nr_fornecedor_ie.
      w_nfref-refnfp-mod                            = i_doc_fiscal-id_mod_fiscal.
      ""BUG IMPEDITIVO 82103
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = i_doc_fiscal-nm_serie
        IMPORTING
          output = i_doc_fiscal-nm_serie.
      w_nfref-refnfp-serie                          = i_doc_fiscal-nm_serie.
      w_nfref-refnfp-nnf                            = i_doc_fiscal-nr_nota.
    ENDIF.
    APPEND w_nfref                                 TO e_dados-processo-intgnfe-ide-nfref.

*--------------------------------------------
*-- EMIT
*--------------------------------------------
    e_dados-processo-intgnfe-emit-cnpj              = w_j_1bbranch-stcd1.
    e_dados-processo-intgnfe-emit-ie                = w_j_1bbranch-state_insc.

*--------------------------------------------
*-- DEST
*--------------------------------------------
    IF w_lfa1-stcd1 IS NOT INITIAL.
      e_dados-processo-intgnfe-dest-cnpj            = w_lfa1-stcd1.
    ELSE.
      e_dados-processo-intgnfe-dest-cpf             = w_lfa1-stcd2.
    ENDIF.

    e_dados-processo-intgnfe-dest-xnome             = w_lfa1-name1.
    e_dados-processo-intgnfe-dest-enderdest-xlgr    = w_adrc_lfa1-street.
    e_dados-processo-intgnfe-dest-enderdest-nro     = w_adrc_lfa1-house_num1.
    e_dados-processo-intgnfe-dest-enderdest-xbairro = w_adrc_lfa1-city2.
    e_dados-processo-intgnfe-dest-enderdest-cmun    = w_adrc_lfa1-taxjurcode+4(10).
    e_dados-processo-intgnfe-dest-enderdest-cep     = w_adrc_lfa1-post_code1.

    IF     w_lfa1-stcd3 IS INITIAL.
      e_dados-processo-intgnfe-dest-indiedest       = '9'.
    ELSEIF w_lfa1-stcd3 = 'ISENTO'.
      e_dados-processo-intgnfe-dest-indiedest       = '2'.
    ELSE.
      e_dados-processo-intgnfe-dest-indiedest       = '1'.
    ENDIF.

    e_dados-processo-intgnfe-dest-ie                = w_lfa1-stcd3.
    e_dados-processo-intgnfe-dest-sap_id_parceiro   = w_lfa1-lifnr.

*--------------------------------------------
*---CFOP
*--------------------------------------------
    IF l_vcfop IS INITIAL.
      l_vcfop = i_doc_fiscal-cfop && 'AA'.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_CFOBR_OUTPUT'
      EXPORTING
        input  = l_vcfop
      IMPORTING
        output = l_vcfop.

    SPLIT l_vcfop AT '/' INTO DATA(l_cfop1)
                              DATA(l_cfop2).

*--------------------------------------------
*-- DET
*--------------------------------------------
    l_valor_nf                                      = ( i_doc_fiscal-nr_valor / i_doc_fiscal-nr_quantidade ).
    l_valor_prop                                    = l_valor_nf            * i_doc_fiscal-nm_peso_liquido.

    w_det-a_nitem                                   = '1'.
    w_det-prod-cprod                                = '1'. "1-Soja 2-Milho
    w_det-prod-cean                                 = 'SEM GTIN'.
    w_det-prod-xprod                                = w_makt-maktx.
    w_det-prod-ncm                                  = w_marc-steuc.
    w_det-prod-cfop                                 = l_cfop1.
    w_det-prod-ucom                                 = 'KG'.
    w_det-prod-qcom                                 = i_doc_fiscal-nm_peso_liquido. "me->carga-nm_peso_liquido.
    w_det-prod-vuncom                               = '1'.
    w_det-prod-vprod                                = l_valor_prop.
    w_det-prod-ceantrib                             = 'SEM GTIN'.
    w_det-prod-utrib                                = 'KG'.
    w_det-prod-qtrib                                = i_doc_fiscal-nm_peso_liquido. "me->carga-nm_peso_liquido.
    w_det-prod-vuntrib                              = '1'.
    w_det-prod-indtot                               = '1'. "não usado no metodo
    w_det-prod-sap_matnr                            = me->carga-id_produto.
    w_det-prod-sap_incltx                           = 'S'. "Valor e preço incluíndo ICMS/ISS
    "ALRS 07.03.2024
    w_det-prod-xped                                 = i_doc_fiscal-po_number.
    w_det-prod-nitemped                             = i_doc_fiscal-po_item.
    "ALRS 07.03.2024

*--------------------------------------------
*-- ICM
*--------------------------------------------
    READ TABLE t_zsdt0001im INTO DATA(w_zsdt0001im) WITH KEY tp_tributo = 'ICM'.
    IF sy-subrc = 0.
      w_det-imposto-sap_lei_fiscal_icm              = w_zsdt0001im-cd_lei_fiscal.
      w_det-imposto-sap_icm_tp_imposto              = 'ICM3'.
      w_det-imposto-sap_icm_mn_basico               = '0'.
      w_det-imposto-sap_icm_tx_imposto              = '0'.
      w_det-imposto-sap_icm_vr_fiscal               = '0'.
      w_det-imposto-sap_icm_mn_excluido             = l_valor_prop. "i_doc_fiscal-nr_valor.
      w_det-imposto-sap_icm_ot_montante             = '0'.
    ENDIF.

*--------------------------------------------
*-- PNC
*--------------------------------------------
    READ TABLE t_zsdt0001im INTO w_zsdt0001im       WITH KEY tp_tributo = 'PNC'.
    IF sy-subrc = 0.
      w_det-imposto-sap_lei_fiscal_pis              = w_zsdt0001im-cd_lei_fiscal.
      w_det-imposto-sap_pis_tp_imposto              = 'IPIS'.
      w_det-imposto-sap_pis_mn_basico               = '0'.
      w_det-imposto-sap_pis_tx_imposto              = '0'.
      w_det-imposto-sap_pis_vr_fiscal               = '0'.
      w_det-imposto-sap_pis_mn_excluido             = l_valor_prop. "i_doc_fiscal-nr_valor.
      w_det-imposto-sap_pis_ot_montante             = '0'.
    ENDIF.

*--------------------------------------------
*-- CNC
*--------------------------------------------
    READ TABLE t_zsdt0001im INTO w_zsdt0001im       WITH KEY tp_tributo = 'CNC'.
    IF sy-subrc = 0.
      w_det-imposto-sap_lei_fiscal_cof              = w_zsdt0001im-cd_lei_fiscal.
      w_det-imposto-sap_cof_tp_imposto              = 'ICOF'.
      w_det-imposto-sap_cof_mn_basico               = '0'.
      w_det-imposto-sap_cof_tx_imposto              = '0'.
      w_det-imposto-sap_cof_vr_fiscal               = '0'.
      w_det-imposto-sap_cof_mn_excluido             = l_valor_prop. "i_doc_fiscal-nr_valor.
      w_det-imposto-sap_cof_ot_montante             = '0'.
    ENDIF.

*--------------------------------------------
*-- IPI
*--------------------------------------------
    w_det-imposto-sap_lei_fiscal_ipi                = 'I53'.
    w_det-imposto-sap_ipi_tp_imposto                = 'IPI3'.
    w_det-imposto-sap_ipi_mn_basico                 = '0'.
    w_det-imposto-sap_ipi_tx_imposto                = '0'.
    w_det-imposto-sap_ipi_vr_fiscal                 = '0'.
    w_det-imposto-sap_ipi_mn_excluido               = l_valor_prop. "i_doc_fiscal-nr_valor.
    w_det-imposto-sap_ipi_ot_montante               = '0'.

    APPEND w_det                                   TO e_dados-processo-intgnfe-det.

*--------------------------------------------
*-- TOTAL
*--------------------------------------------
    e_dados-processo-intgnfe-total-icmstot-vbc      = i_doc_fiscal-nr_valor.
    e_dados-processo-intgnfe-total-icmstot-vicms    = '0'.
    e_dados-processo-intgnfe-total-icmstot-vicmsdeson = '0'.
    e_dados-processo-intgnfe-total-icmstot-vbcst    = '0'.
    e_dados-processo-intgnfe-total-icmstot-vst      = '0'.
    e_dados-processo-intgnfe-total-icmstot-vprod    = i_doc_fiscal-nr_valor.
    e_dados-processo-intgnfe-total-icmstot-vfrete   = '0'.
    e_dados-processo-intgnfe-total-icmstot-vseg     = '0'.
    e_dados-processo-intgnfe-total-icmstot-vdesc    = '0'.
    e_dados-processo-intgnfe-total-icmstot-vii      = '0'.
    e_dados-processo-intgnfe-total-icmstot-vipi     = '0'.
    e_dados-processo-intgnfe-total-icmstot-vpis     = '0'.
    e_dados-processo-intgnfe-total-icmstot-vcofins  = '0'.
    e_dados-processo-intgnfe-total-icmstot-voutro   = '0'.
    e_dados-processo-intgnfe-total-icmstot-vnf      = i_doc_fiscal-nr_valor.
    e_dados-processo-intgnfe-total-icmstot-vfcp     = '0'.
    e_dados-processo-intgnfe-total-icmstot-vfcpst   = '0'.
    e_dados-processo-intgnfe-total-icmstot-vfcpstret = '0'.
    e_dados-processo-intgnfe-total-icmstot-vfcp     = '0'.
    e_dados-processo-intgnfe-total-icmstot-vipidevol = '0'.

*--------------------------------------------
*-- TRANSP
*--------------------------------------------
    e_dados-processo-intgnfe-transp-modfrete        = '0'.

*--------------------------------------------
*-- PAG
*--------------------------------------------
    w_detpag-tpag                                   = '90'.
    w_detpag-vpag                                   = '0'.
    APPEND w_detpag                                TO w_pag-detpag.
    APPEND w_pag                                   TO e_dados-processo-intgnfe-pag.

*--------------------------------------------
*-- INFADIC
*--------------------------------------------
    READ TABLE me->documento_fiscal_imp_ret INTO DATA(w_imp_ret) WITH KEY tp_tributo = 'OUT'.
    IF sy-subrc = 0.
      DATA(v_senar)    = w_imp_ret-vlr_imposto.
    ENDIF.

    READ TABLE me->documento_fiscal_imp_ret INTO w_imp_ret       WITH KEY tp_tributo = 'FUN'.
    IF sy-subrc = 0.
      DATA(v_funrural) = w_imp_ret-vlr_imposto.
    ENDIF.

    e_dados-processo-intgnfe-infadic-infcpl         = 'Safra:'              && me->carga-nr_safra   && ',' &&
                                                      'Valor do Funrural:'  && v_funrural           && ',' &&
                                                      'Valor do Senar:'     && v_senar              && ',' &&
                                                      'NF do Produtor:'     && i_doc_fiscal-nr_nota.

*--------------------------------------------
*-- OBJKEY ferado
*--------------------------------------------
    e_objkey_np                                     = e_dados-processo-infosistemaorigem-id.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_DENQUEUE.

    R_CARGA = ME.

    CHECK ME->ZIF_CARGA~AT_NAO_GERAR_BLOQUEIOS EQ ABAP_FALSE.

    CALL FUNCTION 'ZDENQUEUE_CARGA'
      EXPORTING
        ID_CARGA = I_CARGA.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_DENQUEUE_ROMANEIO.
    R_CARGA = ME.
  ENDMETHOD.


  METHOD ZIF_CARGA~SET_DESBLOQUEAR_ROMANEIOS_SAID.
    R_CARGA = ME.
  ENDMETHOD.


  METHOD ZIF_CARGA~SET_EMITIR_DOC_SAIDAS.

    DATA: NUMBER           TYPE TBTCJOB-JOBCOUNT,
          PRINT_PARAMETERS TYPE PRI_PARAMS.

    R_CARGA = ME.

    TRY .
        ME->ZIF_CARGA~GET_CK_SAIDA_AUTOMATICA( ).
      CATCH ZCX_CARGA.
        EXIT.
    ENDTRY.

    ZCL_CARGA_SAIDA=>ZIF_CARGA~GERAR_CARGA_SAIDA( I_CARGA_ENTRADA = ME
      )->GET_INFO_ALV_APRESENTACAO(
      IMPORTING
        E_APRESENTACAO = DATA(E_APRESENTACAO)
      ).

    IF 1 = 2.
      SUBMIT ZMMR126_JOB WITH PIDCARGA EQ E_APRESENTACAO-CARGA-ID_CARGA AND RETURN.
    ELSE.

      DATA(LC_USER_JOB) = ZCL_JOB=>GET_USER_JOB( ).

      ME->ZIF_CARGA~GET_NAME_JOB_SAIDA_AUTOMATICA( IMPORTING E_NAME = DATA(E_NAME) ).

      TRY .
          ZCL_JOB=>GET_JOB_EXECUCAO( I_JOB_NAME = E_NAME ).
          EXIT.
        CATCH ZCX_JOB.
      ENDTRY.

      TRY .
          ZCL_JOB=>GET_JOB_ESCALONADO( I_JOB_NAME = E_NAME ).
          EXIT.
        CATCH ZCX_JOB.
      ENDTRY.

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          JOBNAME          = E_NAME
        IMPORTING
          JOBCOUNT         = NUMBER
        EXCEPTIONS
          CANT_CREATE_JOB  = 1
          INVALID_JOB_DATA = 2
          JOBNAME_MISSING  = 3
          OTHERS           = 4.

      IF SY-SUBRC IS INITIAL.
        SUBMIT ZMMR126_JOB TO SAP-SPOOL SPOOL PARAMETERS PRINT_PARAMETERS
        WITHOUT SPOOL DYNPRO VIA JOB E_NAME NUMBER NUMBER
          WITH PIDCARGA EQ E_APRESENTACAO-CARGA-ID_CARGA
          USER LC_USER_JOB
           AND RETURN.

        IF SY-SUBRC IS INITIAL.
          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              JOBCOUNT             = NUMBER
              JOBNAME              = E_NAME
              STRTIMMED            = 'X'
            EXCEPTIONS
              CANT_START_IMMEDIATE = 1
              INVALID_STARTDATE    = 2
              JOBNAME_MISSING      = 3
              JOB_CLOSE_FAILED     = 4
              JOB_NOSTEPS          = 5
              JOB_NOTEX            = 6
              LOCK_FAILED          = 7
              OTHERS               = 8.

          IF SY-SUBRC IS NOT INITIAL.
            DATA(CK_ERRO) = ABAP_TRUE.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            CALL FUNCTION 'BP_JOB_DELETE'
              EXPORTING
                JOBCOUNT                 = NUMBER
                JOBNAME                  = E_NAME
              EXCEPTIONS
                CANT_DELETE_EVENT_ENTRY  = 1
                CANT_DELETE_JOB          = 2
                CANT_DELETE_JOBLOG       = 3
                CANT_DELETE_STEPS        = 4
                CANT_DELETE_TIME_ENTRY   = 5
                CANT_DERELEASE_SUCCESSOR = 6
                CANT_ENQ_PREDECESSOR     = 7
                CANT_ENQ_SUCCESSOR       = 8
                CANT_ENQ_TBTCO_ENTRY     = 9
                CANT_UPDATE_PREDECESSOR  = 10
                CANT_UPDATE_SUCCESSOR    = 11
                COMMIT_FAILED            = 12
                JOBCOUNT_MISSING         = 13
                JOBNAME_MISSING          = 14
                JOB_DOES_NOT_EXIST       = 15
                JOB_IS_ALREADY_RUNNING   = 16
                NO_DELETE_AUTHORITY      = 17
                OTHERS                   = 18.
            IF SY-SUBRC IS NOT INITIAL.
              CK_ERRO = ABAP_FALSE.
            ENDIF.
          ENDIF.
        ELSE.
          CK_ERRO = ABAP_TRUE.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO MTEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          CALL FUNCTION 'BP_JOB_DELETE'
            EXPORTING
              JOBCOUNT                 = NUMBER
              JOBNAME                  = E_NAME
            EXCEPTIONS
              CANT_DELETE_EVENT_ENTRY  = 1
              CANT_DELETE_JOB          = 2
              CANT_DELETE_JOBLOG       = 3
              CANT_DELETE_STEPS        = 4
              CANT_DELETE_TIME_ENTRY   = 5
              CANT_DERELEASE_SUCCESSOR = 6
              CANT_ENQ_PREDECESSOR     = 7
              CANT_ENQ_SUCCESSOR       = 8
              CANT_ENQ_TBTCO_ENTRY     = 9
              CANT_UPDATE_PREDECESSOR  = 10
              CANT_UPDATE_SUCCESSOR    = 11
              COMMIT_FAILED            = 12
              JOBCOUNT_MISSING         = 13
              JOBNAME_MISSING          = 14
              JOB_DOES_NOT_EXIST       = 15
              JOB_IS_ALREADY_RUNNING   = 16
              NO_DELETE_AUTHORITY      = 17
              OTHERS                   = 18.
          IF SY-SUBRC IS NOT INITIAL.
            CK_ERRO = ABAP_FALSE.
          ENDIF.
        ENDIF.
      ENDIF.

*      "Aguardar execução do job
*      ZCL_JOB=>GET_INSTANCE(
*       )->SET_KEY_JOB( I_JOBNAME = NAME I_JOBCOUNT = NUMBER
*       )->GET_WAIT_JOB_EXEC(
*       ).

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_ENQUEUE.

    CHECK ME->ZIF_CARGA~AT_NAO_GERAR_BLOQUEIOS EQ ABAP_FALSE.

    CALL FUNCTION 'ZENQUEUE_CARGA'
      EXPORTING
        ID_CARGA       = I_CARGA
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_ENQUEUE_ROMANEIO.
    R_CARGA = ME.
  ENDMETHOD.


  METHOD ZIF_CARGA~SET_ESTORNA_FRETE_ENTRADA.

    R_CARGA = ME.

    CHECK ME->CARGA-CK_FRETE_ENTRADA EQ ABAP_TRUE.

    LOOP AT ME->DOCUMENTO_FISCAL ASSIGNING FIELD-SYMBOL(<ENTRADA_NOTA>) WHERE AV_VBELN IS NOT INITIAL.

      TRY.
          SELECT SINGLE * INTO @DATA(LWA_ZLEST0108)
            FROM ZLEST0108
           WHERE VBELN EQ @<ENTRADA_NOTA>-AV_VBELN.

          IF SY-SUBRC IS INITIAL. "Quando o Aviso foi gerado pela propria Carga, não gera a ZLEST0108
            ZCL_DOC_FISCAL_FT_ENTRADA=>ZIF_DOC_FISCAL_FT_ENTRADA~GET_INSTANCE(
              )->SET_REGISTRO(
                EXPORTING
                  I_EBELN                   = <ENTRADA_NOTA>-PO_NUMBER    " Nº do documento de compras
                  I_EBELP                   = <ENTRADA_NOTA>-PO_ITEM      " Nº item do documento de compra
                  I_VBELN                   = <ENTRADA_NOTA>-AV_VBELN
              )->SET_ESTORNAR_DOCUMENTOS(
              ).
          ENDIF.

          CLEAR: <ENTRADA_NOTA>-PO_NUMBER,
                 <ENTRADA_NOTA>-PO_ITEM,
                 <ENTRADA_NOTA>-AV_VBELN,
                 <ENTRADA_NOTA>-FTE_DOCNUM,
                 <ENTRADA_NOTA>-FTE_FKNUM,
                 <ENTRADA_NOTA>-FTE_TKNUM,
                 <ENTRADA_NOTA>-FTE_VBELN_VA,
                 <ENTRADA_NOTA>-FTE_VBELN_VF.

        CATCH ZCX_DOC_FISCAL_FT_ENTRADA INTO DATA(ERRO_FT_ENTRADA).    "

          RAISE EXCEPTION TYPE ZCX_CARGA
            EXPORTING
              TEXTID = VALUE #( MSGID = ERRO_FT_ENTRADA->MSGID
                                MSGNO = ERRO_FT_ENTRADA->MSGNO
                                ATTR1 = CONV #( ERRO_FT_ENTRADA->MSGV1 )
                                ATTR2 = CONV #( ERRO_FT_ENTRADA->MSGV2 )
                                ATTR3 = CONV #( ERRO_FT_ENTRADA->MSGV3 )
                                ATTR4 = CONV #( ERRO_FT_ENTRADA->MSGV4 ) )
              MSGTY  = 'E'
              MSGID  = ERRO_FT_ENTRADA->MSGID
              MSGNO  = ERRO_FT_ENTRADA->MSGNO
              MSGV1  = ERRO_FT_ENTRADA->MSGV1
              MSGV2  = ERRO_FT_ENTRADA->MSGV2
              MSGV3  = ERRO_FT_ENTRADA->MSGV3
              MSGV4  = ERRO_FT_ENTRADA->MSGV4.

      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_EXCLUIR_BLOCO.

    R_CARGA = ME.

    DELETE ME->ZIF_CARGA~BLOCOS
     WHERE NR_ORDEM_VENDA EQ I_BLOCO-NR_ORDEM_VENDA
       AND NR_PEDIDO_COMPRA EQ I_BLOCO-NR_PEDIDO_COMPRA
       AND ZSEQ_INST EQ I_BLOCO-ZSEQ_INST
       AND OBJEK EQ I_BLOCO-OBJEK
       AND OBJECTTABLE EQ I_BLOCO-OBJECTTABLE.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_EXCLUIR_ORDEM_VENDA.

    R_CARGA = ME.

    READ TABLE ME->ZIF_CARGA~ORDEM_VENDA WITH KEY NR_ORDEM_VENDA = I_VBELN INTO DATA(WA_ORDEM_VENDA).

    CHECK SY-SUBRC IS INITIAL.

    "Elimina Ordem de Venda
    DELETE ME->ZIF_CARGA~ORDEM_VENDA WHERE NR_ORDEM_VENDA = I_VBELN.

    "Elimina Romaneios de Saída da Ordem de Venda
    DELETE ME->ZIF_CARGA~ORDEM_VENDA_ROMANEIOS WHERE NR_ORDEM_VENDA = I_VBELN.

    "Elimina Blocos da Ordem de Saída
    DELETE ME->ZIF_CARGA~BLOCOS WHERE NR_ORDEM_VENDA = I_VBELN.

    IF ME->CARGA-ID_ORDEM IS NOT INITIAL AND WA_ORDEM_VENDA-ID_ORDEM EQ ME->CARGA-ID_ORDEM.
      CLEAR: ME->CARGA-ID_ORDEM.
    ENDIF.

    READ TABLE ME->ZIF_CARGA~ORDEM_VENDA INDEX 1 INTO DATA(WA_ORDEM_VENDA_NEW).

    CHECK SY-SUBRC IS INITIAL.

    ME->CARGA-ID_ORDEM = WA_ORDEM_VENDA_NEW-ID_ORDEM.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_EXC_TAKE_UP.

    R_CARGA = ME.

    DELETE ME->ZIF_CARGA~TAKE_UP
     WHERE ID_NOTA = I_TAKEUP-ID_NOTA
       AND ID_TAKEUP = I_TAKEUP-ID_TAKEUP
       AND NU_BLOCO = I_TAKEUP-NU_BLOCO.

  ENDMETHOD.


  METHOD zif_carga~set_fechar.

    DATA: wa_entrada  TYPE awkey,
          it_entradas TYPE TABLE OF awkey,
          i_nota      TYPE zde_info_nota.

    e_fechou = abap_false.

    r_carga = me.

    CLEAR: me->zif_carga~ck_executar_reversao_entrada.      "BUG 34154

    CHECK NOT ( me->carga-ck_enviado_opus EQ abap_true AND me->carga-ck_recebido_opus EQ abap_false ).

    CASE me->carga-tp_status.
      WHEN zif_carga=>st_status_conferido.

        TRY .
            me->zif_carga~get_ck_saida_automatica( ).
            DATA(lc_ck_saida_auto) = abap_true.
          CATCH zcx_carga.
            lc_ck_saida_auto = abap_false.
        ENDTRY.

        IF lc_ck_saida_auto EQ abap_true.
          me->zif_carga~get_name_job_saida_automatica( IMPORTING e_name = DATA(e_name) ).

          TRY .
              zcl_job=>get_job_execucao( i_job_name = e_name ).
              RAISE EXCEPTION TYPE zcx_carga
                EXPORTING
                  textid = VALUE #( msgid = zcx_carga=>zcx_saida_automatica_exec-msgid msgno = zcx_carga=>zcx_saida_automatica_exec-msgno )
                  msgty  = 'E'
                  msgid  = zcx_carga=>zcx_saida_automatica_exec-msgid
                  msgno  = zcx_carga=>zcx_saida_automatica_exec-msgno.
            CATCH zcx_job.
          ENDTRY.

          TRY .
              zcl_job=>get_job_escalonado( i_job_name = e_name ).
              RAISE EXCEPTION TYPE zcx_carga
                EXPORTING
                  textid = VALUE #( msgid = zcx_carga=>zcx_saida_automatica_exec-msgid msgno = zcx_carga=>zcx_saida_automatica_exec-msgno )
                  msgty  = 'E'
                  msgid  = zcx_carga=>zcx_saida_automatica_exec-msgid
                  msgno  = zcx_carga=>zcx_saida_automatica_exec-msgno.
            CATCH zcx_job.
          ENDTRY.
        ENDIF.

      WHEN zif_carga=>st_status_fechado.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_carga_fechada-msgid msgno = zcx_carga=>zcx_carga_fechada-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_fechada-msgid
            msgno  = zcx_carga=>zcx_carga_fechada-msgno.
      WHEN zif_carga=>st_status_cancelada.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_carga_cancelada-msgid msgno = zcx_carga=>zcx_carga_cancelada-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_cancelada-msgid
            msgno  = zcx_carga=>zcx_carga_cancelada-msgno.
    ENDCASE.

    CASE me->carga-tp_status.
      WHEN zif_carga=>st_status_aberto.

        me->validar_registro( IMPORTING e_validou = DATA(e_validou) ).

        IF NOT e_validou EQ abap_true.
          me->carga-tp_status = zif_carga=>st_status_aberto.

          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = VALUE #( msgid = sy-msgid msgno = sy-msgno
                                attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
              msgty  = 'E'
              msgid  = sy-msgid
              msgno  = sy-msgno
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = sy-msgv3
              msgv4  = sy-msgv4.
        ENDIF.

        me->verif_ordem_carregamento( i_ck_verificar_carga = abap_true ).

        "Verificar Entradas Própria


        "Verificar Pagamento de Frete na Entrada
        IF me->carga-id_ordem IS NOT INITIAL AND me->zif_carga~at_manutencao EQ abap_false.

          IF me->carga-ck_frete_entrada EQ abap_true.

*            LOOP AT ME->DOCUMENTO_FISCAL ASSIGNING FIELD-SYMBOL(<FS_ENTRADA>).
*
*              CLEAR: I_NOTA.
*              I_NOTA-NFE            = COND STRING( WHEN <FS_ENTRADA>-ID_MOD_FISCAL EQ ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO THEN ABAP_TRUE ELSE ABAP_FALSE ).
*              I_NOTA-MODELO         = <FS_ENTRADA>-ID_MOD_FISCAL.
*              I_NOTA-SERIE          = <FS_ENTRADA>-NM_SERIE.
*              I_NOTA-NUMERO         = <FS_ENTRADA>-NR_NOTA.
*              I_NOTA-FORNECEDOR     = <FS_ENTRADA>-ID_FORNECEDOR.
*              I_NOTA-DTEMISSAO      = <FS_ENTRADA>-DT_EMISSAO.
*              I_NOTA-VL_BC          = <FS_ENTRADA>-NR_VALOR.
*              I_NOTA-VL_PRODUTOS    = <FS_ENTRADA>-NR_VALOR.
*              I_NOTA-VL_NOTA_FISCAL = <FS_ENTRADA>-NR_VALOR.
*              I_NOTA-MATERIAL       = ME->CARGA-ID_PRODUTO.
*              I_NOTA-QUANTIDADE     = <FS_ENTRADA>-NR_QUANTIDADE.
*              I_NOTA-UNIDADE        = 'KG'.
*              I_NOTA-CFOP           = <FS_ENTRADA>-CFOP && '/AA'.
*              I_NOTA-LC_RETIRADA    = <FS_ENTRADA>-ID_ENTREGUE_POR.
*              I_NOTA-ID_CARGA       = <FS_ENTRADA>-ID_CARGA.
*              I_NOTA-ID_NOTA        = <FS_ENTRADA>-ID_NOTA.
*
*              TRY .
*                  "Gerar Frete de Entrada
*                  ZCL_DOC_FISCAL_FT_ENTRADA=>ZIF_DOC_FISCAL_FT_ENTRADA~GET_INSTANCE(
*                     )->SET_NEW_DOCUMENTO_ORDEM_CARREG(
*                          EXPORTING
*                            I_ID_ORDEM        = ME->CARGA-ID_ORDEM
*                            I_ID_LOCAL_COLETA = CONV #( COND STRING( WHEN <FS_ENTRADA>-ID_ENTREGUE_POR IS NOT INITIAL THEN <FS_ENTRADA>-ID_ENTREGUE_POR ELSE <FS_ENTRADA>-ID_FORNECEDOR ) )
*                            I_PESO_LIQ        = CONV #( <FS_ENTRADA>-NR_QUANTIDADE )
*                            I_NOTA_FISCAL     = I_NOTA    " Inf. de Nota Fiscal de Mercadoria
*                            "I_ID_ROTA_REPOM           =     " Identificador de Rota REPOM
*                            "I_ID_PERCURSO_REPOM       =     " Identificador de Percurso REPOM
*                            "I_NR_CARTAO_REPOM         =     " Cartão REPOM
*                           IMPORTING
*                             E_EBELN = <FS_ENTRADA>-PO_NUMBER
*                             E_EBELP = <FS_ENTRADA>-PO_ITEM
*                             E_VBELN = <FS_ENTRADA>-AV_VBELN
*                             E_DOC_TRANSP = <FS_ENTRADA>-FTE_TKNUM
*                             E_FKNUM = <FS_ENTRADA>-FTE_FKNUM
*                             E_OV_FRETE = <FS_ENTRADA>-FTE_VBELN_VA
*                             E_FATURA_FRETE = <FS_ENTRADA>-FTE_VBELN_VF
*                             E_NRO_NF_FRETE = <FS_ENTRADA>-FTE_DOCNUM
*                     ).
*
*                CATCH ZCX_DOC_FISCAL_FT_ENTRADA INTO DATA(ERRO_FT_ENTRADA).    "
*                  "Volta Carga para Aberta
*                  ME->CARGA-TP_STATUS = ZIF_CARGA=>ST_STATUS_ABERTO.
*                  ME->GRAVAR_REGISTRO( ).
*
*                  RAISE EXCEPTION TYPE ZCX_CARGA
*                    EXPORTING
*                      TEXTID = VALUE #( MSGID = ERRO_FT_ENTRADA->MSGID
*                                        MSGNO = ERRO_FT_ENTRADA->MSGNO
*                                        ATTR1 = CONV #( ERRO_FT_ENTRADA->MSGV1 )
*                                        ATTR2 = CONV #( ERRO_FT_ENTRADA->MSGV2 )
*                                        ATTR3 = CONV #( ERRO_FT_ENTRADA->MSGV3 )
*                                        ATTR4 = CONV #( ERRO_FT_ENTRADA->MSGV4 ) )
*                      MSGTY  = 'E'
*                      MSGID  = ERRO_FT_ENTRADA->MSGID
*                      MSGNO  = ERRO_FT_ENTRADA->MSGNO
*                      MSGV1  = ERRO_FT_ENTRADA->MSGV1
*                      MSGV2  = ERRO_FT_ENTRADA->MSGV2
*                      MSGV3  = ERRO_FT_ENTRADA->MSGV3
*                      MSGV4  = ERRO_FT_ENTRADA->MSGV4.
*              ENDTRY.
*
*            ENDLOOP.

          ENDIF.

          zcl_ordem_carregamento=>set_fechar( i_id_ordem = me->carga-id_ordem ).
        ENDIF.

        me->zif_carga~set_ajustar_rat_desc_geral( ).

        me->carga-dt_movimento  = sy-datlo.
        me->carga-tp_status     = zif_carga=>st_status_fechado.
        me->carga-dt_fechamento = sy-datlo.
        me->carga-hr_fechamento = sy-timlo.
        me->carga-us_fechamento = sy-uname.
        me->gravar_registro( IMPORTING e_gravou  = e_fechou ).

      WHEN zif_carga=>st_status_conferido.

        "Reabrir para Ajustes
        me->ck_alterou = abap_true.

        " Verificar se romaneios excluidos estão CANCELADOS 01/03/2024
*        me->zif_carga~check_romaneios_carga_opus( EXPORTING i_id_carga  = me->carga-id_carga ).

        me->zif_carga~ck_executar_reversao_entrada = abap_true. "BUG 34154

        me->validar_registro( IMPORTING e_validou = e_validou ).

        IF NOT e_validou EQ abap_true.
          me->carga-tp_status = zif_carga=>st_status_conferido.
          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = VALUE #( msgid = sy-msgid msgno = sy-msgno
                                attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
              msgty  = 'E'
              msgid  = sy-msgid
              msgno  = sy-msgno
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = sy-msgv3
              msgv4  = sy-msgv4.
        ENDIF.

        TRY .
            me->verif_ordem_carregamento( i_ck_verificar_carga = abap_true ).
          CATCH zcx_carga INTO DATA(ex_carga).
            ex_carga->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
            MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          CATCH zcx_ordem_carregamento INTO DATA(ex_ordem_carregamento).
            ex_ordem_carregamento->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
            MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDTRY.

        IF me->zif_carga~at_manutencao EQ abap_false.
          me->zif_carga~get_ck_excluir_romaneio_saida( EXPORTING i_ck_opus = abap_false
           )->send_estorno_carga_to_opus(
           )->gravar_registro(
           )->get_recuperar_entrada(
           )->set_processar_estorno_sobra(
           )->set_gerar_estorno_estoque(
           )->set_processar_estorno( IMPORTING e_estornou = DATA(e_estornou)
           )->set_cancelar_nota_propria(   "*-CS2021000183-#83755-19.07.2022-JT
           ).
          CHECK e_estornou EQ abap_true.
        ELSE.
          me->zif_carga~set_cancelar_solic_manut( ).
          me->ck_alterou = abap_true.
        ENDIF.

        me->carga-tp_status         = zif_carga=>st_status_fechado.
        me->carga-dt_fechamento     = sy-datlo.
        me->carga-hr_fechamento     = sy-timlo.
        me->carga-us_fechamento     = sy-uname.

        LOOP AT me->documento_fiscal ASSIGNING FIELD-SYMBOL(<fs_nota>).
          CLEAR: <fs_nota>-ch_referencia_ent, <fs_nota>-nr_romaneio_ent.
        ENDLOOP.

        me->gravar_registro( IMPORTING e_gravou  = e_fechou ).

        LOOP AT it_entradas INTO wa_entrada.
          me->desbloquear_entrada( i_obj_key = wa_entrada ).
        ENDLOOP.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_carga~set_gerar_entrada_estoque.

    DATA: entradas        TYPE TABLE OF zmmt_ee_zgr,
          entradas_imp    TYPE TABLE OF zmmt_eeimp_zgr,
          lr_bwart        TYPE RANGE OF bwart, "**#128299 -  ITSOUZA - Inicio
          wa_entradas_imp TYPE zmmt_eeimp_zgr,
          lc_ck_nfe	      TYPE j_1bnfe,
          i_nf_number	    TYPE j_1bnfdoc-nfnum,
          obj_zcl_util_sd TYPE REF TO zcl_util_sd,
          i_data          TYPE gdatu_inv,
          ck_gerar_nova   TYPE char01,
          wa_nota         TYPE j_1bnfdoc,
          lc_menge        TYPE bstmg,
          e_lgort         TYPE lgort_d. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940


    "**#128299 -  ITSOUZA - Inicio --->>
    lr_bwart = VALUE #( sign = 'I' option = 'EQ' ( low = '004' )
                                                 ( low = '008' )
                                                 ( low = '236' ) ).
    "**#128299 -  ITSOUZA - Inicio <<----

    r_carga = me.

    CHECK me->zif_carga~at_manutencao EQ abap_false.

    CHECK NOT ( me->carga-ck_enviado_opus  EQ abap_true AND
                me->carga-ck_recebido_opus EQ abap_false ).

    i_data = me->carga-dt_movimento.

    CHECK me->carga-tp_status EQ zif_carga=>st_status_conferido.

    CREATE OBJECT obj_zcl_util_sd.
    obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data ).
    obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
    obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'BRL' ).
    obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'USD' ).
    DATA(e_ukurs) = obj_zcl_util_sd->taxa_cambio( ).

    CLEAR: entradas, entradas[], entradas_imp, entradas_imp[].

    me->zif_carga~get_recuperar_entrada( ).

    LOOP AT me->documento_fiscal ASSIGNING FIELD-SYMBOL(<fs_nota>).

      me->zif_carga~set_gera_imposto_nota( EXPORTING i_nota = <fs_nota> ).

      IF <fs_nota>-obj_key_entrada IS NOT INITIAL.

        SELECT SINGLE * INTO @DATA(entrada)
          FROM zmmt_ee_zgr
         WHERE id_carga   EQ @<fs_nota>-id_carga
           AND id_nota    EQ @<fs_nota>-id_nota
           AND obj_key    EQ @<fs_nota>-obj_key_entrada
           AND obj_key    NE @space .

        IF sy-subrc IS INITIAL.
          me->zif_carga~get_documento_ent_estornado(
            EXPORTING
              i_obj_key   = <fs_nota>-obj_key_entrada
             IMPORTING
               r_estornado = DATA(r_estornado) ).

          ck_gerar_nova = r_estornado.

          IF r_estornado EQ abap_false.
            CASE entrada-st_estorno.
              WHEN zif_carga=>st_status_estorno_sem.
              WHEN zif_carga=>st_status_estorno_solicitado.
                RAISE EXCEPTION TYPE zcx_carga
                  EXPORTING
                    textid = VALUE #( msgid = zcx_carga=>zcx_existe_estorno_pendente-msgid msgno = zcx_carga=>zcx_existe_estorno_pendente-msgno )
                    msgty  = 'E'
                    msgid  = zcx_carga=>zcx_existe_estorno_pendente-msgid
                    msgno  = zcx_carga=>zcx_existe_estorno_pendente-msgno.
              WHEN zif_carga=>st_status_estorno_executado.
                ck_gerar_nova = abap_true.
              WHEN zif_carga=>st_status_estorno_erro.
                RAISE EXCEPTION TYPE zcx_carga
                  EXPORTING
                    textid = VALUE #( msgid = zcx_carga=>zcx_existe_estorno_erro-msgid msgno = zcx_carga=>zcx_existe_estorno_erro-msgno )
                    msgty  = 'E'
                    msgid  = zcx_carga=>zcx_existe_estorno_erro-msgid
                    msgno  = zcx_carga=>zcx_existe_estorno_erro-msgno.
            ENDCASE.
          ELSE.
            CLEAR: <fs_nota>-obj_key_entrada.
          ENDIF.
        ENDIF.

        IF ck_gerar_nova NE abap_true.
          SELECT SINGLE * INTO @DATA(documentos)
            FROM zmmt_ee_zgr_docs
           WHERE obj_key EQ @entrada-obj_key
             AND obj_key NE @space.

          IF "DOCUMENTOS-PO_NUMBER IS NOT INITIAL OR
             "DOCUMENTOS-AV_VBELN  IS NOT INITIAL OR
             documentos-mm_mblnr  IS NOT INITIAL OR
             documentos-mm_mjahr  IS NOT INITIAL OR
             documentos-ft_belnr  IS NOT INITIAL OR
             documentos-ft_gjahr  IS NOT INITIAL OR
             documentos-docnum    IS NOT INITIAL.
            CONTINUE.
          ENDIF.
        ENDIF.

      ELSE.
        me->ck_alterou = abap_true.
        ck_gerar_nova = abap_true.
      ENDIF.

      CASE <fs_nota>-id_mod_fiscal.
        WHEN zif_carga=>st_model_fiscal_papel.
          lc_ck_nfe = abap_false.
        WHEN zif_carga=>st_model_fiscal_eletronico.
          lc_ck_nfe = abap_true.
      ENDCASE.

      SELECT SINGLE * INTO @DATA(wa_entrada)
        FROM zsdt0001te
       WHERE id_entrada EQ @<fs_nota>-id_entrada
         AND id_empresa EQ @me->carga-id_bukrs
         AND ck_nfe     EQ @lc_ck_nfe.

      SELECT SINGLE * INTO @DATA(wa_j_1baa)
        FROM j_1baa
       WHERE nftype EQ @wa_entrada-ct_nota.

      IF ck_gerar_nova EQ abap_true.

        CASE <fs_nota>-id_mod_fiscal.
          WHEN zif_carga=>st_model_fiscal_papel.
            lc_ck_nfe = abap_false.
          WHEN zif_carga=>st_model_fiscal_eletronico.
            lc_ck_nfe = abap_true.
        ENDCASE.

        IF wa_j_1baa-form IS INITIAL.

          "Verifica se Documento já foi lançado (MIRO)
          CASE <fs_nota>-id_mod_fiscal .
            WHEN zif_carga=>st_model_fiscal_eletronico.

              wa_nota-model	 = zif_carga=>st_model_fiscal_eletronico.
              wa_nota-parid  = <fs_nota>-id_fornecedor.
              wa_nota-series = <fs_nota>-nm_serie.
              wa_nota-form   = space.
              wa_nota-cancel = space.
              wa_nota-nfe    = abap_true.
              wa_nota-nfenum = <fs_nota>-nr_nota.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wa_nota-nfenum
                IMPORTING
                  output = wa_nota-nfenum.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wa_nota-series
                IMPORTING
                  output = wa_nota-series.

              SELECT SINGLE * INTO wa_nota
                FROM j_1bnfdoc
               WHERE doctyp NE '5'
                 AND model  EQ wa_nota-model
                 AND parid  EQ wa_nota-parid
                 AND series EQ wa_nota-series
                 AND form   EQ wa_nota-form
                 AND cancel EQ wa_nota-cancel
                 AND nfe    EQ wa_nota-nfe
                 AND nfenum EQ wa_nota-nfenum.

              IF sy-subrc IS NOT INITIAL.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = wa_nota-series
                  IMPORTING
                    output = wa_nota-series.

                SELECT SINGLE * INTO wa_nota
                  FROM j_1bnfdoc
                 WHERE doctyp NE '5'
                   AND model  EQ wa_nota-model
                   AND parid  EQ wa_nota-parid
                   AND series EQ wa_nota-series
                   AND form   EQ wa_nota-form
                   AND cancel EQ wa_nota-cancel
                   AND nfe    EQ wa_nota-nfe
                   AND nfenum EQ wa_nota-nfenum.

              ENDIF.

              IF sy-subrc IS INITIAL.
                RAISE EXCEPTION TYPE zcx_carga
                  EXPORTING
                    textid = VALUE #( msgid = zcx_carga=>zcx_nota_fiscal_lancada-msgid
                                      msgno = zcx_carga=>zcx_nota_fiscal_lancada-msgno
                                      attr1 = CONV #( zif_carga=>st_model_fiscal_eletronico )
                                      attr2 = CONV #( <fs_nota>-id_fornecedor )
                                      attr3 = CONV #( <fs_nota>-nr_nota )
                                      attr4 = CONV #( <fs_nota>-nm_serie )  )
                    msgty  = 'E'
                    msgno  = zcx_carga=>zcx_nota_fiscal_lancada-msgno
                    msgid  = zcx_carga=>zcx_nota_fiscal_lancada-msgid
                    msgv1  = CONV #( zif_carga=>st_model_fiscal_eletronico )
                    msgv2  = CONV #( <fs_nota>-id_fornecedor )
                    msgv3  = CONV #( <fs_nota>-nr_nota )
                    msgv4  = CONV #( <fs_nota>-nm_serie ).
              ENDIF.

              zcl_miro=>verificar_forn_doc_fiscal(
                EXPORTING
                  i_lifnr            = <fs_nota>-id_fornecedor
                  i_nftype           = wa_entrada-ct_nota
                  i_xblnr            = zcl_miro=>get_chave_referencia( i_series = <fs_nota>-nm_serie  i_nf_number9 = CONV #( <fs_nota>-nr_nota ) )
                  i_data             = <fs_nota>-dt_emissao
                  i_werks            = me->carga-id_branch
                IMPORTING
                  e_notas            = DATA(e_notas)
              ).

              READ TABLE e_notas INDEX 1 INTO DATA(wa_chave).

              SELECT SINGLE * INTO @DATA(wa_nfe_inbound)
                FROM zib_nfe_dist_ter
               WHERE chave_nfe EQ @wa_chave-nu_chave.

*              IF WA_NFE_INBOUND-CD_FINA_EMISSAO NE '1'.
*                RAISE EXCEPTION TYPE ZCX_CARGA
*                  EXPORTING
*                    TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NOTA_DEVE_SER_NORMAL-MSGID
*                                      MSGNO = ZCX_CARGA=>ZCX_NOTA_DEVE_SER_NORMAL-MSGNO )
*                    MSGTY  = 'E'
*                    MSGNO  = ZCX_CARGA=>ZCX_NOTA_DEVE_SER_NORMAL-MSGNO
*                    MSGID  = ZCX_CARGA=>ZCX_NOTA_DEVE_SER_NORMAL-MSGID.
*              ENDIF.

            WHEN zif_carga=>st_model_fiscal_papel.

              wa_nota-model	 = zif_carga=>st_model_fiscal_papel.
              wa_nota-parid  = <fs_nota>-id_fornecedor.
              wa_nota-series = <fs_nota>-nm_serie.
              wa_nota-form   = space.
              wa_nota-cancel = space.
              wa_nota-nfe    = abap_false.
              wa_nota-nfnum  = <fs_nota>-nr_nota.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wa_nota-series
                IMPORTING
                  output = wa_nota-series.

              SELECT SINGLE * INTO wa_nota
                FROM j_1bnfdoc
               WHERE doctyp NE '5'
                 AND model  EQ wa_nota-model
                 AND parid  EQ wa_nota-parid
                 AND series EQ wa_nota-series
                 AND nfnum  EQ wa_nota-nfnum
                 AND form   EQ wa_nota-form
                 AND cancel EQ wa_nota-cancel
                 AND nfe    EQ wa_nota-nfe.

              IF sy-subrc IS NOT INITIAL.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = wa_nota-series
                  IMPORTING
                    output = wa_nota-series.

                SELECT SINGLE * INTO wa_nota
                  FROM j_1bnfdoc
                 WHERE doctyp NE '5'
                   AND model  EQ wa_nota-model
                   AND parid  EQ wa_nota-parid
                   AND series EQ wa_nota-series
                   AND nfnum  EQ wa_nota-nfnum
                   AND form   EQ wa_nota-form
                   AND cancel EQ wa_nota-cancel
                   AND nfe    EQ wa_nota-nfe.
              ENDIF.

              IF sy-subrc IS INITIAL.
                RAISE EXCEPTION TYPE zcx_carga
                  EXPORTING
                    textid = VALUE #( msgid = zcx_carga=>zcx_nota_fiscal_lancada-msgid
                                      msgno = zcx_carga=>zcx_nota_fiscal_lancada-msgno
                                      attr1 = CONV #( zif_carga=>st_model_fiscal_papel )
                                      attr2 = CONV #( <fs_nota>-id_fornecedor )
                                      attr3 = CONV #( <fs_nota>-nr_nota )
                                      attr4 = CONV #( <fs_nota>-nm_serie )  )
                    msgty  = 'E'
                    msgno  = zcx_carga=>zcx_nota_fiscal_lancada-msgno
                    msgid  = zcx_carga=>zcx_nota_fiscal_lancada-msgid
                    msgv1  = CONV #( zif_carga=>st_model_fiscal_papel )
                    msgv2  = CONV #( <fs_nota>-id_fornecedor )
                    msgv3  = CONV #( <fs_nota>-nr_nota )
                    msgv4  = CONV #( <fs_nota>-nm_serie ).
              ENDIF.

          ENDCASE.
        ELSEIF <fs_nota>-id_mod_fiscal = zif_carga=>st_model_fiscal_eletronico AND <fs_nota>-docnum_np IS NOT INITIAL.
          zcl_miro=>verificar_forn_doc_fiscal(
          EXPORTING
                            i_lifnr            = <fs_nota>-id_fornecedor
                            i_nftype           = 'ZF' "forçar tipo NFE terceiro
                            i_xblnr            = zcl_miro=>get_chave_referencia( i_series = <fs_nota>-nm_serie  i_nf_number9 = CONV #( <fs_nota>-nr_nota ) )
                            i_data             = <fs_nota>-dt_emissao
                            i_werks            = me->carga-id_branch
          IMPORTING
                            e_notas            = DATA(e_nota)
          ).
        ENDIF.

        me->get_new_id_entrada_estoque( IMPORTING e_id_carga = DATA(lc_sequencia) ).
        CONCATENATE 'CG' lc_sequencia INTO entrada-obj_key.
        me->ck_alterou = abap_true.
      ENDIF.

*-CS2021000183 - 05.04.2022 - JT - inicio
*     IF <fs_nota>-objkey_np IS NOT INITIAL.
*       <fs_nota>-obj_key_entrada = <fs_nota>-objkey_np.
*     ELSE.
*       <fs_nota>-obj_key_entrada = entrada-obj_key.
*     ENDIF.
*-CS2021000183 - 05.04.2022 - JT - fim

      <fs_nota>-obj_key_entrada = entrada-obj_key.

      CASE <fs_nota>-id_mod_fiscal.
        WHEN zif_carga=>st_model_fiscal_papel.
          lc_ck_nfe = abap_false.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = <fs_nota>-nr_nota
            IMPORTING
              output = i_nf_number.

          entrada-nt_remessa = zcl_miro=>get_chave_referencia( i_nf_number  = i_nf_number i_series = <fs_nota>-nm_serie ).

        WHEN zif_carga=>st_model_fiscal_eletronico.
          lc_ck_nfe = abap_true.
          entrada-nt_remessa = zcl_miro=>get_chave_referencia( i_nf_number9  = CONV #( <fs_nota>-nr_nota ) i_series = <fs_nota>-nm_serie ).
      ENDCASE.

      SELECT SINGLE * INTO @DATA(wa_tipo_entrada)
        FROM zsdt0001te
       WHERE id_entrada EQ @<fs_nota>-id_entrada
         AND id_empresa EQ @me->carga-id_bukrs
         AND ck_nfe     EQ @lc_ck_nfe.

      IF zcl_cfop=>get_ck_cfop_retorno_amazem( i_cfop = <fs_nota>-cfop ) EQ abap_true.
        me->get_pedido_compra( EXPORTING i_id_nota = <fs_nota>-id_nota IMPORTING e_info_pedido = DATA(pedido) ).
      ELSE.
        TRY .
            me->get_pedido_compra( EXPORTING i_id_nota = <fs_nota>-id_nota IMPORTING e_info_pedido = pedido ).
          CATCH zcx_carga INTO DATA(ex_carga).
            IF ex_carga->msgid EQ zcx_carga=>zcx_sem_pedido_compra-msgid AND
               ex_carga->msgno EQ zcx_carga=>zcx_sem_pedido_compra-msgno AND
               wa_tipo_entrada-id_tp_operacao NE zif_tp_operacao_carga_entrada=>st_compra_entrega_futura.

              "Buscar Tipo de Nota Fiscal
              SELECT SINGLE * INTO @DATA(wa_zsdt0001tetn)
                FROM zsdt0001tetn
               WHERE id_entrada EQ @wa_tipo_entrada-id_entrada.

              IF sy-subrc IS INITIAL.

                "Buscar Grupo de Mercadoria
                SELECT SINGLE * INTO @DATA(wa_zsdt0001mt)
                  FROM zsdt0001mt
                 WHERE matnr EQ @me->carga-id_produto.

                IF sy-subrc IS INITIAL.

                  "Buscar Iva por Tipo de Nota Fiscal / Grupo de Mercadoria / Empresa
                  SELECT SINGLE * INTO @DATA(wa_zsdt0001tetniva)
                    FROM zsdt0001tetniva
                   WHERE id_empresa   EQ @me->carga-id_bukrs
                     AND id_tipo_nota EQ @wa_zsdt0001tetn-id_tipo_nota
                     AND tp_grupo_ctb EQ @wa_zsdt0001mt-tp_grupo_ctb.

                  IF sy-subrc IS INITIAL.
                    wa_tipo_entrada-id_iva = wa_zsdt0001tetniva-id_iva.
                  ENDIF.
                ENDIF.

              ENDIF.

*-CS2022000332-#78064-07.06.2022-JT-inicio
              IF me->zif_carga~carga-id_contrato IS INITIAL.
                lc_menge = 400000000.    " Quantidade do pedido
              ELSE.
                SELECT SINGLE quatidade
                  INTO lc_menge
                  FROM zsdt0143
                 WHERE id_contrato = me->zif_carga~carga-id_contrato.
                IF sy-subrc <> 0.
                  lc_menge = 400000000.    " Quantidade do pedido
                ENDIF.
              ENDIF.
*-CS2022000332-#78064-07.06.2022-JT-fim

              zcl_pedido_compra=>set_criar_pedido_compra(
                EXPORTING
                  i_bedat      = <fs_nota>-dt_emissao    " Data do documento de compra
                  i_ekorg      = 'OC01'    " Organização de compras
                  i_ekgrp      = 'G01'    " Grupo de compradores
                  i_waers      = 'BRL'    " Código da moeda
                  i_bsart      = 'ZGR '   " Tipo de documento de compras
                  i_zterm      = 'Z001'    " Chave de condições de pagamento
                  i_lifnr      = <fs_nota>-id_fornecedor " Nº conta do fornecedor
                  i_bukrs      = me->carga-id_bukrs    " Empresa
                  i_charg      = CONV #( me->carga-nr_safra )    " Número do lote
                  i_eindt      = sy-datum    " Data de remessa do item
                  i_mwskz      = wa_tipo_entrada-id_iva    " Código do IVA
                  i_menge      = lc_menge  "400000000    " Quantidade do pedido *-CS2022000332-#78064-07.06.2022-JT-inicio
                  i_matnr      = me->carga-id_produto   " Nº do material
                  i_meins      = 'KG'    " Unidade de medida do pedido
                  i_werks      = me->carga-id_branch    " Centro
                  i_tp_centro  = zcl_pedido_compra=>st_tp_centro_a_fixar
                  i_collect_no = me->zif_carga~carga-id_contrato      "*-CS2022000332-#78064-07.06.2022-JT-inicio
              ).
              WAIT UP TO 5 SECONDS.
              me->get_pedido_compra( EXPORTING i_id_nota = <fs_nota>-id_nota IMPORTING e_info_pedido = pedido ).
            ELSE.
              RAISE EXCEPTION TYPE zcx_carga
                EXPORTING
                  textid = VALUE #( msgid = ex_carga->msgid msgno = ex_carga->msgno
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
            ENDIF.
        ENDTRY.
      ENDIF.

      SELECT SINGLE * INTO @DATA(wa_afixar)
        FROM zsdt_depara_cen
       WHERE vkorg             EQ @me->carga-id_bukrs
         AND centro_real       EQ @me->carga-id_branch
         AND tp_centro_virtual EQ @zcl_pedido_compra=>st_tp_centro_a_fixar.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_pedido_compra_exception
          EXPORTING
            textid    = VALUE #( msgid = zcx_pedido_compra_exception=>zcx_centro_a_fixar-msgid
                                 msgno = zcx_pedido_compra_exception=>zcx_centro_a_fixar-msgno
                                 attr1 = CONV #( me->carga-id_branch )
                                 attr2 = 'ZSDT0036' )
            msgid     = zcx_pedido_compra_exception=>zcx_centro_a_fixar-msgid
            msgno     = zcx_pedido_compra_exception=>zcx_centro_a_fixar-msgno
            msgty     = 'E'
            msgv1     = CONV #( me->carga-id_branch )
            msgv2     = 'ZSDT0036'
            transacao = 'ZSDT0036'.
      ENDIF.

      IF me->classificacao-tp_transgenia EQ 'CO'.

        "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - inicio
*        SELECT SINGLE * INTO @DATA(wa_zmmt0017)
*          FROM zmmt0017
*         WHERE matnr          EQ @me->carga-id_produto
*           AND centro_fixo    EQ @me->carga-id_branch
*           AND centro_a_fixar EQ @wa_afixar-centrov_1
*           AND tp_produto     EQ 'CO'.
*
*        IF sy-subrc is INITIAL.
*          entrada-lgort = wa_zmmt0017-lgort.
*        ENDIF.

        zcl_deposito=>zif_deposito~get_instance(
                    )->get_deposito_material_filial(
                    EXPORTING
                      i_matnr          = me->carga-id_produto
                      i_tp_produto     = 'CO'
                      i_bukrs          = me->carga-id_bukrs
                      i_branch         = me->carga-id_branch
                      i_eudr           = me->carga-eudr
                    IMPORTING
                      e_lgort          = e_lgort  ).

        IF e_lgort IS NOT INITIAL.
          entrada-lgort = e_lgort.
        ENDIF.
        "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 -  fim

      ELSE.

        "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INICIO
*        SELECT SINGLE * INTO @wa_zmmt0017
*          FROM zmmt0017
*         WHERE matnr          EQ @me->carga-id_produto
*           AND centro_fixo    EQ @me->carga-id_branch
*           AND centro_a_fixar EQ @wa_afixar-centrov_1
*           AND tp_produto     EQ 'RR'.
*
*        IF sy-subrc is INITIAL.
*          entrada-lgort = wa_zmmt0017-lgort.
*        ENDIF.

        zcl_deposito=>zif_deposito~get_instance(
                    )->get_deposito_material_filial(
                    EXPORTING
                      i_matnr          = me->carga-id_produto
                      i_tp_produto     = 'RR'
                      i_bukrs          = me->carga-id_bukrs
                      i_branch         = me->carga-id_branch
                      i_eudr           = me->carga-eudr
                    IMPORTING
                      e_lgort          = e_lgort  ).

        IF e_lgort IS NOT INITIAL.
          entrada-lgort = e_lgort.
        ENDIF.
        "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 -  fim

      ENDIF.

      entrada-po_number      = pedido-ekko-ebeln.
      entrada-ch_referencia  = <fs_nota>-ch_referencia_ent.
      entrada-batch          = me->carga-nr_safra.
      entrada-move_type      = wa_tipo_entrada-tp_mov_mercadoria.
      IF wa_j_1baa-form IS INITIAL.
        entrada-doc_date       = <fs_nota>-dt_emissao.
      ELSE.
        entrada-doc_date             = me->carga-dt_fechamento.
        <fs_nota>-dt_emissao         = me->carga-dt_fechamento.
        <fs_nota>-dt_vencimento_form = me->carga-dt_fechamento.
      ENDIF.
      entrada-pstng_date     = me->carga-dt_movimento.
      entrada-po_item        = pedido-ekpo-ebelp.
      entrada-nu_item        = pedido-ekpo-ebelp.
      entrada-entry_qnt	     = <fs_nota>-nr_quantidade.
      entrada-meins          = 'KG'.
      entrada-peso_bruto     = <fs_nota>-nr_quantidade.
      entrada-comp_code	     = me->carga-id_bukrs.
      CASE me->carga-ck_gera_aviso.
        WHEN abap_true.
          entrada-in_aviso_receb = 'S'.
        WHEN abap_false.
          entrada-in_aviso_receb = 'N'.
      ENDCASE.
      <fs_nota>-tp_operacao  = wa_tipo_entrada-id_tp_operacao.
      entrada-tp_operacao    = wa_tipo_entrada-id_tp_operacao.
      entrada-ref_doc_no     = entrada-nt_remessa.
      entrada-vr_bruto       = <fs_nota>-nr_valor.

      CLEAR: entrada-del_costs_taxc.

      "Buscar Tipo de Nota Fiscal
      SELECT SINGLE * INTO @wa_zsdt0001tetn
        FROM zsdt0001tetn
       WHERE id_entrada EQ @wa_tipo_entrada-id_entrada.

      "Buscar Grupo de Mercadoria
      SELECT SINGLE * INTO @wa_zsdt0001mt
        FROM zsdt0001mt
       WHERE matnr EQ @me->carga-id_produto.

      "Buscar Iva por Tipo de Nota Fiscal / Grupo de Mercadoria / Empresa
      SELECT SINGLE * INTO @wa_zsdt0001tetniva
        FROM zsdt0001tetniva
       WHERE id_empresa   EQ @me->carga-id_bukrs
         AND id_tipo_nota EQ @wa_zsdt0001tetn-id_tipo_nota
         AND tp_grupo_ctb EQ @wa_zsdt0001mt-tp_grupo_ctb.

      IF sy-subrc IS INITIAL.
        wa_tipo_entrada-id_iva = wa_zsdt0001tetniva-id_iva.
      ENDIF.

      entrada-del_costs_taxc = wa_tipo_entrada-id_iva.
      entrada-tax_code       = wa_tipo_entrada-id_iva.
      entrada-pmnt_block     = wa_tipo_entrada-bl_pagamento.
      entrada-pmnttrms       = wa_tipo_entrada-ch_pagamento.
      entrada-scbank_ind     = wa_tipo_entrada-bn_empresa.
      entrada-pymt_meth	     = wa_tipo_entrada-fr_pagamento.
      entrada-plant	         = me->carga-id_branch.
      entrada-j_1bnftype     = wa_tipo_entrada-ct_nota.
      entrada-alloc_nmbr     = pedido-ekko-ebeln.

*-CS2021000183 - 05.04.2022 - JT - inicio
      IF <fs_nota>-docnum_np IS NOT INITIAL.
        SELECT nftot, ntgew, brgew
          INTO @DATA(w_jdoc)
          FROM j_1bnfdoc
            UP TO 1 ROWS
         WHERE docnum = @<fs_nota>-docnum_np.
        ENDSELECT.

        SELECT nfnum9, serie
          INTO @DATA(w_active)
          FROM j_1bnfe_active
            UP TO 1 ROWS
         WHERE docnum = @<fs_nota>-docnum_np.
        ENDSELECT.

        entrada-nt_remessa   = w_active-nfnum9 && '-' && w_active-serie.
        entrada-ref_doc_no   = entrada-nt_remessa.
        entrada-quantity     = w_jdoc-ntgew.
        entrada-gross_amount = w_jdoc-nftot.
        entrada-item_amount	 = w_jdoc-nftot.
        entrada-vr_bruto     = w_jdoc-nftot.
        entrada-entry_qnt	   = w_jdoc-ntgew.
        entrada-peso_bruto   = w_jdoc-brgew.
        entrada-amount_lc    = abs( w_jdoc-nftot       / e_ukurs ).
      ELSE.
        entrada-gross_amount = <fs_nota>-nr_valor.
        entrada-item_amount	 = <fs_nota>-nr_valor.
        entrada-amount_lc    = abs( <fs_nota>-nr_valor / e_ukurs ).
        entrada-quantity     = <fs_nota>-nr_quantidade.
      ENDIF.
*-CS2021000183 - 05.04.2022 - JT - fim

**#128299 -  ITSOUZA - Inicio
      IF <fs_nota>-id_entrada IN lr_bwart.
        entrada-material       = SWITCH #( me->carga-id_produto
                                           WHEN '000000000000119892' THEN '000000000000153733'
                                           WHEN '000000000000119895' THEN '000000000000120102'
                                           ELSE me->carga-id_produto ).
      ELSE.
        entrada-material       = me->carga-id_produto.
      ENDIF.
*      entrada-material       = me->carga-id_produto.
*#128299 -  ITSOUZA - Fim
      entrada-bus_area       = me->carga-id_branch.
*     entrada-quantity       = <fs_nota>-nr_quantidade.
      entrada-zdt_atlz       = sy-datlo.
      entrada-zhr_atlz       = sy-timlo.
      entrada-zrg_atlz       = '1'.
      entrada-id_carga       = <fs_nota>-id_carga.
      entrada-id_nota        = <fs_nota>-id_nota.
*-CS2021000183 - 05.04.2022 - JT - inicio
      entrada-objkey_np      = <fs_nota>-objkey_np.
*-CS2021000183 - 05.04.2022 - JT - fim
      entrada-st_estorno     = zif_carga=>st_status_estorno_sem.
      CONCATENATE 'Safra:' me->carga-nr_safra INTO entrada-text1.

      SELECT SINGLE txjcd INTO  entrada-taxjurcode
        FROM lfa1
       WHERE lifnr EQ <fs_nota>-id_fornecedor.

*--------------------------------------IR184069 / aoenning ---------
*      IF  entrada-tp_operacao EQ '06'.
*        entrada-lifnr  = <fs_nota>-id_fornecedor.
*      ENDIF.
*--------------------------------------IR184069 / aoenning ---------

      entrada-vr_impostos = 0.
      "ALRS grava valor de imposto normal 25/03/2025
*      LOOP AT me->documento_fiscal_imp_ret INTO DATA(wa_impostos)
*          WHERE id_nota EQ <fs_nota>-id_nota
*            AND cd_imposto IS INITIAL.
*        ADD wa_impostos-vlr_imposto TO entrada-vr_impostos.
*      ENDLOOP.

      IF entrada-obj_key IS NOT INITIAL.
        APPEND entrada TO entradas.
      ENDIF.

      "Copiar Impostos Retidos
      LOOP AT me->documento_fiscal_imp_ret INTO DATA(wa_impostos)
          WHERE id_nota EQ <fs_nota>-id_nota
            AND cd_imposto IS NOT INITIAL.
        wa_entradas_imp-obj_key     = entrada-obj_key.
        wa_entradas_imp-po_number   = entrada-po_number.
        wa_entradas_imp-wi_tax_code = wa_impostos-cd_imposto.
        wa_entradas_imp-wt_withcd   = wa_impostos-cd_categoria.
        wa_entradas_imp-zdt_atlz    = entrada-zdt_atlz.
        wa_entradas_imp-zhr_atlz    = entrada-zhr_atlz.
        wa_entradas_imp-zrg_atlz    = entrada-zrg_atlz.
        wa_entradas_imp-wi_tax_base = wa_impostos-base_imposto.
        wa_entradas_imp-wi_tax_amt  = wa_impostos-vlr_imposto.
        APPEND wa_entradas_imp TO entradas_imp.
      ENDLOOP.

    ENDLOOP.

    CHECK entradas[] IS NOT INITIAL.

    MODIFY zmmt_ee_zgr FROM TABLE entradas.

    LOOP AT entradas INTO entrada.
      DELETE FROM zmmt_eeimp_zgr WHERE obj_key EQ entrada-obj_key.
    ENDLOOP.

    IF entradas_imp[] IS NOT INITIAL.
      MODIFY zmmt_eeimp_zgr FROM TABLE entradas_imp.
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_carga~set_gerar_estorno_estoque.

    DATA: rg_st_estorno   TYPE RANGE OF zde_status_estorno,
          ck_solicitar    TYPE char01,
          wa_zmmt_eee_zgr TYPE zmmt_eee_zgr,
          it_zmmt_eee_zgr TYPE TABLE OF zmmt_eee_zgr.

    r_carga = me.

    CHECK me->zif_carga~at_manutencao EQ abap_false.

    CASE me->carga-tp_status.
      WHEN zif_carga=>st_status_aberto.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_carga_aberta-msgid msgno = zcx_carga=>zcx_carga_aberta-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_aberta-msgid
            msgno  = zcx_carga=>zcx_carga_aberta-msgno.
      WHEN zif_carga=>st_status_cancelada.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_carga_cancelada-msgid msgno = zcx_carga=>zcx_carga_cancelada-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_cancelada-msgid
            msgno  = zcx_carga=>zcx_carga_cancelada-msgno.
    ENDCASE.

    IF me->zif_carga~ck_executar_manutencao_entrada EQ abap_false AND
       me->zif_carga~at_manutencao EQ abap_false.
      me->zif_carga~get_ck_excluir_romaneio_saida( ).
    ENDIF.

    TYPES BEGIN OF ty_entrada_estoque.
    TYPES: docnum TYPE j_1bnfe_active-docnum,
           cancel TYPE j_1bnfe_active-cancel,
           docsta TYPE j_1bnfe_active-docsta,
           scssta TYPE j_1bnfe_active-scssta,
           form   TYPE j_1bnfdoc-form.
    TYPES END OF ty_entrada_estoque.

    DATA: it_tab TYPE TABLE OF ty_entrada_estoque,
          wa_tab TYPE ty_entrada_estoque.

    TRY.
        EXEC SQL.
          OPEN SQL_ZMMT_EE_ZGR FOR
            SELECT DOC.DOCNUM,
                   NFE.CANCEL,
                   NFE.DOCSTA,
                   NFE.SCSSTA,
                   NF.FORM
* ---> S4 Migration - 26/07/2023 - MG-5875 - RF
*              FROM SAPHANADB.ZMMT_EE_ZGR ENT,
*                   SAPHANADB.ZMMT_EE_ZGR_DOCS DOC,
*                   SAPHANADB.J_1BNFDOC        NF,
*                   SAPHANADB.J_1BNFE_ACTIVE   NFE
              FROM SAPHANADB.ZMMT_EE_ZGR ENT,
                   SAPHANADB.ZMMT_EE_ZGR_DOCS DOC,
                   SAPHANADB.J_1BNFDOC        NF,
                   SAPHANADB.J_1BNFE_ACTIVE   NFE
* <--- S4 Migration - 26/07/2023 - MG-5875 - RF
             WHERE ENT.MANDT    = :SY-MANDT
               AND ENT.ID_CARGA = :ME->CARGA-ID_CARGA
               AND ENT.OBJ_KEY  <> ' '
               AND ENT.MANDT    = DOC.MANDT
               AND ENT.OBJ_KEY  = DOC.OBJ_KEY
               AND DOC.MANDT    = NF.MANDT
               AND DOC.DOCNUM   = NF.DOCNUM
               AND DOC.MANDT    = NFE.MANDT
               AND DOC.DOCNUM   = NFE.DOCNUM
        ENDEXEC.
      CATCH cx_sy_native_sql_error INTO DATA(exc_ref).
        DATA(error_text) = exc_ref->get_text( ).
        MESSAGE error_text TYPE 'E'.
    ENDTRY.

    DO.
      EXEC SQL.
        FETCH NEXT SQL_ZMMT_EE_ZGR INTO
        :WA_TAB-DOCNUM,
        :WA_TAB-CANCEL,
        :WA_TAB-DOCSTA,
        :WA_TAB-SCSSTA,
        :WA_TAB-FORM
      ENDEXEC.
      IF sy-subrc <> 0.
        EXIT.
      ELSE.
        APPEND wa_tab TO it_tab.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE SQL_ZMMT_EE_ZGR
    ENDEXEC.

*    "Verifica Nota Fiscal Própria não Cancelada/Estornada
*    SELECT DOC~DOCNUM,
*           NFE~CANCEL,
*           NFE~DOCSTA,
*           NFE~SCSSTA,
*           NF~FORM
*      INTO TABLE @DATA(IT_TAB)
*      FROM ZMMT_EE_ZGR AS ENT
*     INNER JOIN ZMMT_EE_ZGR_DOCS AS DOC ON DOC~OBJ_KEY EQ ENT~OBJ_KEY
*     INNER JOIN J_1BNFDOC      AS NF  ON NF~DOCNUM EQ DOC~DOCNUM
*     INNER JOIN J_1BNFE_ACTIVE AS NFE ON NFE~DOCNUM EQ DOC~DOCNUM
*     WHERE ENT~ID_CARGA EQ @ME->CARGA-ID_CARGA
*       AND ENT~OBJ_KEY  NE @SPACE.

    LOOP AT it_tab INTO wa_tab WHERE docnum IS NOT INITIAL AND form NE space.

*-CS2021000183 - 05.04.2022 - JT - inicio
*-----------------------------------------
*---- verifica se nota propria. Case seja, nao efetuar validacao abaixo
*-----------------------------------------
      READ TABLE me->documento_fiscal INTO DATA(w_doc_fiscal) WITH KEY docnum_np = wa_tab-docnum.

      IF sy-subrc = 0.
        SELECT SINGLE *
          INTO @DATA(w_active)
          FROM j_1bnfe_active
         WHERE docnum = @wa_tab-docnum.

        IF ( w_active-docsta = 1 AND w_active-scssta = 2 ) OR
           ( w_active-docsta = 2 AND w_active-scssta = 4 ) OR
           ( w_active-cancel = abap_true ).
          sy-subrc = 1.
        ELSE.
          sy-subrc = 1.  "*-CS2021000183-#83755-19.07.2022-JT
*         sy-subrc = 4.  "*-CS2021000183-#83755-19.07.2022-JT
        ENDIF.
      ELSE.
        CALL FUNCTION 'Z_NFE_CTE_AUTORIZADO'
          EXPORTING
            p_docnum       = wa_tab-docnum
            p_uso          = 'N'
          EXCEPTIONS
            cancelado      = 1
            nao_cancelado  = 2
            pendente       = 3
            nao_concluido  = 4
            nao_existe     = 5
            autorizado_uso = 6
            denegado       = 7
            OTHERS         = 8.
      ENDIF.
*-CS2021000183 - 05.04.2022 - JT - fim

      IF sy-subrc IS NOT INITIAL AND sy-subrc NE 1.
        RAISE EXCEPTION TYPE zcx_carga
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

    ENDLOOP.

    rg_st_estorno = VALUE #( sign = 'I' option = 'EQ'
                             ( low = zif_carga=>st_status_estorno_sem  high = zif_carga=>st_status_estorno_sem  )
                             ( low = zif_carga=>st_status_estorno_erro high = zif_carga=>st_status_estorno_erro ) ).

    SELECT * INTO TABLE @DATA(it_entrada)
      FROM zmmt_ee_zgr
     WHERE id_carga   EQ @me->carga-id_carga
       AND st_estorno IN @rg_st_estorno
       AND obj_key    NE @space.

    IF it_entrada[] IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(it_entrada_estorno)
        FROM zmmt_eee_zgr
         FOR ALL ENTRIES IN @it_entrada
       WHERE obj_key EQ @it_entrada-obj_key.

      SELECT * INTO TABLE @DATA(it_zmmt_ee_zgr_docs)
        FROM zmmt_ee_zgr_docs
         FOR ALL ENTRIES IN @it_entrada
       WHERE obj_key EQ @it_entrada-obj_key.
    ENDIF.

    DATA(it_entrada_gravar) = it_entrada[].

    LOOP AT it_entrada INTO DATA(wa_entrada).
      me->bloquear_entrada( i_obj_key = wa_entrada-obj_key ).
    ENDLOOP.

    LOOP AT it_entrada ASSIGNING FIELD-SYMBOL(<entrada>).

      READ TABLE it_zmmt_ee_zgr_docs INTO DATA(wa_zmmt_ee_zgr_docs) WITH KEY obj_key = <entrada>-obj_key.
      IF sy-subrc IS NOT INITIAL.
        "Não Existe documento para ser estornado
        CONTINUE.
      ENDIF.

      IF NOT ( "WA_ZMMT_EE_ZGR_DOCS-AV_VBELN IS NOT INITIAL OR
               wa_zmmt_ee_zgr_docs-ft_belnr IS NOT INITIAL OR
               wa_zmmt_ee_zgr_docs-mm_mblnr IS NOT INITIAL ).
        "Não Existe nenhum documento para ser estornado
        CONTINUE.
      ENDIF.

      ck_solicitar = abap_false.

      READ TABLE it_entrada_estorno INTO DATA(wa_estorno) WITH KEY obj_key = <entrada>-obj_key.
      IF ( sy-subrc IS INITIAL ) AND ( wa_estorno-rg_atualizado EQ '0' ).

        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_solic_estorno_andamento-msgid
                              msgno = zcx_carga=>zcx_solic_estorno_andamento-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_solic_estorno_andamento-msgid
            msgno  = zcx_carga=>zcx_solic_estorno_andamento-msgno.

      ELSEIF ( sy-subrc IS INITIAL ) AND ( wa_estorno-rg_atualizado EQ '1' ).
        "Verificar se já foi estornado
        "IF WA_ESTORNO-VBELN_VL IS  NOT INITIAL.
        "  TRY.
        "      ME->GET_INFO_MESSAGEM_ESTORNO( I_OBJ_KEY = WA_ESTORNO-OBJ_KEY I_INTERFACE  = ZIF_CARGA=>ST_INTERFACE_AVISO ).
        "      CLEAR : WA_ZMMT_EE_ZGR_DOCS-AV_VBELN.
        "    CATCH ZCX_CARGA.
        "  ENDTRY.
        "ENDIF.

        IF wa_estorno-mblnr IS  NOT INITIAL.
          TRY.
              me->get_info_messagem_estorno( i_obj_key = wa_estorno-obj_key i_interface  = zif_carga=>st_interface_migo ).
              CLEAR : wa_zmmt_ee_zgr_docs-mm_mjahr,
                      wa_zmmt_ee_zgr_docs-mm_mblnr.
            CATCH zcx_carga.
          ENDTRY.
        ENDIF.

        IF wa_estorno-re_belnr IS  NOT INITIAL.
          TRY.
              me->get_info_messagem_estorno( i_obj_key = wa_estorno-obj_key i_interface  = zif_carga=>st_interface_miro ).
              CLEAR : wa_zmmt_ee_zgr_docs-ft_belnr,
                      wa_zmmt_ee_zgr_docs-ft_gjahr.
            CATCH zcx_carga.
          ENDTRY.
        ENDIF.

      ELSE.
        ck_solicitar = abap_true.
      ENDIF.

      IF ck_solicitar EQ abap_false.
        CONTINUE.
      ENDIF.

      IF NOT ( "WA_ZMMT_EE_ZGR_DOCS-AV_VBELN IS NOT INITIAL OR
               wa_zmmt_ee_zgr_docs-ft_belnr IS NOT INITIAL OR
               wa_zmmt_ee_zgr_docs-mm_mblnr IS NOT INITIAL ).
        CONTINUE.
      ELSE.
        <entrada>-st_estorno             = zif_carga=>st_status_estorno_solicitado.
        wa_zmmt_eee_zgr-obj_key          = <entrada>-obj_key.
        wa_zmmt_eee_zgr-mjahr            = wa_zmmt_ee_zgr_docs-mm_mjahr.
        wa_zmmt_eee_zgr-gjahr            = wa_zmmt_ee_zgr_docs-ft_gjahr.
        wa_zmmt_eee_zgr-vbeln_vl         = wa_zmmt_ee_zgr_docs-av_vbeln.
        wa_zmmt_eee_zgr-mblnr            = wa_zmmt_ee_zgr_docs-mm_mblnr.
        wa_zmmt_eee_zgr-re_belnr         = wa_zmmt_ee_zgr_docs-ft_belnr.
        wa_zmmt_eee_zgr-stgrd            = 'Z1'.
        wa_zmmt_eee_zgr-budat            = sy-datlo.
        wa_zmmt_eee_zgr-dt_estorno       = i_dt_estorno.
        wa_zmmt_eee_zgr-id_interface_avi = '19'.
        wa_zmmt_eee_zgr-id_interface_mig = '20'.
        wa_zmmt_eee_zgr-id_interface_mir = '21'.
        wa_zmmt_eee_zgr-dt_atualizacao   = sy-datlo.
        wa_zmmt_eee_zgr-hr_atualizacao   = sy-timlo.
        wa_zmmt_eee_zgr-rg_atualizado    = '1'.
        APPEND wa_zmmt_eee_zgr TO it_zmmt_eee_zgr.
      ENDIF.

    ENDLOOP.

    IF it_zmmt_eee_zgr[] IS NOT INITIAL.
      MODIFY zmmt_eee_zgr FROM TABLE it_zmmt_eee_zgr.
      MODIFY zmmt_ee_zgr  FROM TABLE it_entrada.
      COMMIT WORK.
    ENDIF.

    LOOP AT it_entrada INTO wa_entrada.
      me->desbloquear_entrada( i_obj_key = wa_entrada-obj_key ).
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_GERAR_FATURA.
    R_CARGA = ME.
  ENDMETHOD.


  METHOD zif_carga~set_gerar_nota_propria.

    DATA: lc_dados     TYPE zde_processo,
          lc_docnum_np TYPE j_1bdocnum,
          lc_objkey_np TYPE awkey,
          lc_gera_np   TYPE char1.

    LOOP AT me->documento_fiscal ASSIGNING FIELD-SYMBOL(<fs_nf>).

      FREE: lc_docnum_np,
            lc_gera_np.

      SELECT ct_nota, tp_pessoa
        FROM zsdt0001te
        INTO @DATA(w_zsdt0001te)
          UP TO 1 ROWS
       WHERE id_entrada = @<fs_nf>-id_entrada
         AND id_empresa = @me->carga-id_bukrs.
      ENDSELECT.

      CHECK sy-subrc = 0.

      SELECT form
        INTO @DATA(l_form)
        FROM j_1baa
          UP TO 1 ROWS
       WHERE nftype = @w_zsdt0001te-ct_nota.
      ENDSELECT.

      CHECK l_form IS NOT INITIAL.

*-------------------------------------------
*-----verifica se NP ja foi criada
*-------------------------------------------
      IF <fs_nf>-nr_chave_nfe IS NOT INITIAL.
        SELECT     docnum_ref    objkey_np
          INTO (lc_docnum_np, lc_objkey_np)
          FROM zib_nfe_forn
            UP TO 1 ROWS
         WHERE nu_chave  = <fs_nf>-nr_chave_nfe.
        ENDSELECT.
      ELSE.
        SELECT stcd2
          INTO @DATA(l_stcd2)
          FROM lfa1
            UP TO 1 ROWS
         WHERE lifnr = @<fs_nf>-id_fornecedor.
        ENDSELECT.

        SELECT     docnum_ref    objkey_np
          INTO (lc_docnum_np, lc_objkey_np)
          FROM zib_nfe_forn
            UP TO 1 ROWS
         WHERE nu_chave_cnpj   = l_stcd2
           AND dt_emissao      = <fs_nf>-dt_emissao
           AND nu_chave_numero = <fs_nf>-nr_nota
           AND nu_ie           = <fs_nf>-nr_fornecedor_ie.
        ENDSELECT.

        lc_docnum_np  = <fs_nf>-docnum_np.  "BUG IMPEDITIVO 82103
        lc_objkey_np  = <fs_nf>-objkey_np   .
      ENDIF.

      IF lc_docnum_np IS INITIAL.
        lc_gera_np = abap_true.
      ELSE.
        SELECT SINGLE *
          INTO @DATA(w_active)
          FROM j_1bnfe_active
         WHERE docnum = @lc_docnum_np.

        IF ( w_active-docsta = 1 AND w_active-scssta = 2 ) OR
           ( w_active-docsta = 2 AND w_active-scssta = 4 ) OR
           ( w_active-cancel = abap_true ).
          lc_gera_np = abap_true.
        ENDIF.
      ENDIF.

*-------------------------------------------
*-----so gera NP se nao foi gerada ainda
*-------------------------------------------
      IF lc_gera_np = abap_true.
        TRY.
            me->zif_carga~set_dados_nota_propria( EXPORTING i_doc_fiscal = <fs_nf>
                                                  IMPORTING e_dados      = lc_dados
                                                            e_objkey_np  = lc_objkey_np ).

          CATCH zcx_error INTO DATA(ex_error).
            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid = VALUE #( msgid = ex_error->msgid msgno = ex_error->msgno
                                  attr1 = ex_error->msgv1 attr2 = ex_error->msgv2
                                  attr3 = ex_error->msgv3 attr4 = ex_error->msgv4 )
                msgty  = 'E'
                msgid  = ex_error->msgid
                msgno  = ex_error->msgno
                msgv1  = ex_error->msgv1
                msgv2  = ex_error->msgv2
                msgv3  = ex_error->msgv3
                msgv4  = ex_error->msgv4.
        ENDTRY.

        TRY.
            zcl_integracao_grc_new_nfe=>zif_integracao_grc_new_nfe~get_instance(
               )->set_criar_nf_propria(           EXPORTING i_dados      = lc_dados
                                                  IMPORTING e_documento  = lc_docnum_np
               ).

          CATCH zcx_error INTO ex_error.
            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid = VALUE #( msgid = ex_error->msgid msgno = ex_error->msgno
                                  attr1 = ex_error->msgv1 attr2 = ex_error->msgv2
                                  attr3 = ex_error->msgv3 attr4 = ex_error->msgv4 )
                msgty  = 'E'
                msgid  = ex_error->msgid
                msgno  = ex_error->msgno
                msgv1  = ex_error->msgv1
                msgv2  = ex_error->msgv2
                msgv3  = ex_error->msgv3
                msgv4  = ex_error->msgv4.
        ENDTRY.

*-------------------------------------------
*------ Atualiza ZIB_NFE_FORN
*-------------------------------------------
        IF <fs_nf>-nr_chave_nfe IS NOT INITIAL.
          UPDATE zib_nfe_forn SET docnum_ref      = lc_docnum_np
                                  objkey_np       = lc_objkey_np
                            WHERE nu_chave        = <fs_nf>-nr_chave_nfe.
        ELSE.
          SELECT stcd2
            INTO l_stcd2
            FROM lfa1
              UP TO 1 ROWS
           WHERE lifnr = <fs_nf>-id_fornecedor.
          ENDSELECT.

          UPDATE zib_nfe_forn SET docnum_ref      = lc_docnum_np
                                  objkey_np       = lc_objkey_np
                            WHERE nu_chave_cnpj   = l_stcd2
                              AND dt_emissao      = <fs_nf>-dt_emissao
                              AND nu_chave_numero = <fs_nf>-nr_nota
                              AND nu_ie           = <fs_nf>-nr_fornecedor_ie.
        ENDIF.
      ENDIF.

*-------------------------------------------
*-----Atualiza NP
*-------------------------------------------
      <fs_nf>-ent_np       = abap_true.
      <fs_nf>-docnum_np    = lc_docnum_np.
      <fs_nf>-objkey_np    = lc_objkey_np.

*-------------------------------------------
*-----verifica se NP foi autorizada
*-------------------------------------------
      SELECT SINGLE *
        INTO w_active
        FROM j_1bnfe_active
       WHERE docnum = lc_docnum_np.

      IF NOT ( w_active-docsta = 1 AND w_active-scssta = 0 ).

*       DATA(l_mesg1) = 'Nota Propria gerada: ' && lc_docnum_np.
*       DATA(l_mesg2) = 'Não foi Autorizada! Verifique!'.
*
*       CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
*         EXPORTING
*           titel        = 'Nota Própria'
*           textline1    = l_mesg1
*           textline2    = l_mesg2
*           start_column = 25
*           start_row    = 6.

        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Nota Propria gerada: ' )
                              attr2 = CONV #( lc_docnum_np )
                              attr3 = CONV #( 'Não foi Autorizada! Verifique!' ) )
            msgty  = 'E'
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgv1  = CONV #( 'Nota Propria gerada: ' )
            msgv2  = CONV #( lc_docnum_np )
            msgv3  = CONV #( 'Não foi Autorizada! Verifique!' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_GERAR_REMESSAS.
    R_CARGA = ME.
  ENDMETHOD.


  METHOD ZIF_CARGA~SET_GERAR_ROMANEIO_ENTRADA.

    DATA: OBJ_ROMANEIO   TYPE REF TO ZCL_ROMANEIO,
          LC_ROMANEIO    TYPE ZSDT0001,
          LC_NOTA        TYPE ZSDT0001NT,
          CK_GERAR_SAIDA TYPE CHAR01.

    R_CARGA = ME.

    CHECK ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_FALSE.

    CHECK NOT ( ME->CARGA-CK_ENVIADO_OPUS EQ ABAP_TRUE AND ME->CARGA-CK_RECEBIDO_OPUS EQ ABAP_FALSE ).

    CREATE OBJECT OBJ_ROMANEIO.

    IF ME->CARGA-TP_STATUS EQ ZIF_CARGA=>ST_STATUS_CONFERIDO.
      ME->GET_ROMANEIO_ENTRADA( EXPORTING I_ID_CARGA  =  ME->CARGA-ID_CARGA IMPORTING E_ROMANEIOS = DATA(ROMANEIOS) ).

      LOOP AT ME->DOCUMENTO_FISCAL ASSIGNING FIELD-SYMBOL(<FS_DOCUMENTO>).

        IF <FS_DOCUMENTO>-DOCNUM IS NOT INITIAL.
          SELECT SINGLE * INTO @DATA(WA_J_1BNFDOC)
            FROM J_1BNFDOC
           WHERE DOCNUM EQ @<FS_DOCUMENTO>-DOCNUM.

          "Nota Fiscal Própria
          IF WA_J_1BNFDOC-FORM IS NOT INITIAL AND WA_J_1BNFDOC-NFENUM IS NOT INITIAL. " AND
*-CS2021000183 - 05.04.2022 - JT - inicio
*            <fs_documento>-docnum_np IS INITIAL.
*-CS2021000183 - 05.04.2022 - JT - fim

            SELECT SINGLE * INTO @DATA(WA_J_1BNFE_ACTIVE)
              FROM J_1BNFE_ACTIVE
             WHERE DOCNUM EQ @<FS_DOCUMENTO>-DOCNUM.

            IF SY-SUBRC IS INITIAL.
              <FS_DOCUMENTO>-NR_NOTA  = WA_J_1BNFE_ACTIVE-NFNUM9.
              <FS_DOCUMENTO>-NM_SERIE = WA_J_1BNFE_ACTIVE-SERIE.
              <FS_DOCUMENTO>-DT_EMISSAO = WA_J_1BNFDOC-DOCDAT.
              <FS_DOCUMENTO>-NR_CHAVE_NFE = WA_J_1BNFE_ACTIVE-REGIO && WA_J_1BNFE_ACTIVE-NFYEAR && WA_J_1BNFE_ACTIVE-NFMONTH && WA_J_1BNFE_ACTIVE-STCD1 && WA_J_1BNFE_ACTIVE-MODEL &&
                                            WA_J_1BNFE_ACTIVE-SERIE && WA_J_1BNFE_ACTIVE-NFNUM9 && WA_J_1BNFE_ACTIVE-DOCNUM9 && WA_J_1BNFE_ACTIVE-CDV.
            ENDIF.
          ENDIF.
        ENDIF.

        IF <FS_DOCUMENTO>-CH_REFERENCIA_ENT IS NOT INITIAL.
          "Não existe Romaneio
          SELECT SINGLE * INTO @DATA(WA_ROMNEIO)
            FROM ZSDT0001
           WHERE CH_REFERENCIA EQ @<FS_DOCUMENTO>-CH_REFERENCIA_ENT.

          IF SY-SUBRC IS INITIAL AND ME->ZIF_CARGA~CK_EXECUTAR_MANUTENCAO_ENTRADA EQ ABAP_FALSE.

            IF <FS_DOCUMENTO>-CH_REFERENCIA_ENT IS INITIAL OR
               <FS_DOCUMENTO>-NR_ROMANEIO_ENT   IS INITIAL.

              <FS_DOCUMENTO>-NR_ROMANEIO_ENT   = WA_ROMNEIO-NR_ROMANEIO.
              <FS_DOCUMENTO>-CH_REFERENCIA_ENT = WA_ROMNEIO-CH_REFERENCIA.
            ENDIF.

            CONTINUE.
          ELSEIF SY-SUBRC IS NOT INITIAL.
            CLEAR: <FS_DOCUMENTO>-CH_REFERENCIA_ENT, <FS_DOCUMENTO>-NR_ROMANEIO_ENT.
          ENDIF.
        ENDIF.

        READ TABLE ROMANEIOS INTO DATA(WA_ROMANEIO) WITH KEY ID_CARGA = <FS_DOCUMENTO>-ID_CARGA ID_NOTA = <FS_DOCUMENTO>-ID_NOTA.
        IF SY-SUBRC IS INITIAL.
          OBJ_ROMANEIO->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = WA_ROMANEIO-CH_REFERENCIA ).

          IF <FS_DOCUMENTO>-CH_REFERENCIA_ENT IS INITIAL OR
             <FS_DOCUMENTO>-NR_ROMANEIO_ENT   IS INITIAL.

            <FS_DOCUMENTO>-NR_ROMANEIO_ENT   = WA_ROMANEIO-NR_ROMANEIO.
            <FS_DOCUMENTO>-CH_REFERENCIA_ENT = WA_ROMANEIO-CH_REFERENCIA.
          ENDIF.

        ELSE.
          OBJ_ROMANEIO->NOVO_REGISTRO( ).
          OBJ_ROMANEIO->CK_PERMITE_DATA_RETROATIVA = ME->ZIF_CARGA~CK_EXECUTAR_MANUTENCAO_ENTRADA.
        ENDIF.

        OBJ_ROMANEIO->SET_TP_MOVIMENTO( I_TP_MOVIMENTO = ZCL_ROMANEIO=>ST_TP_MOVIMENTO_ENTRADA ).
        "OBJ_ROMANEIO->SET_VBELN( I_VBELN = <FS_DOCUMENTO>-NR_ORDEM_VENDA ).
        OBJ_ROMANEIO->SET_DT_MOVIMENTO( I_DT_MOVIMENTO = ME->CARGA-DT_MOVIMENTO ).
        OBJ_ROMANEIO->SET_NR_SAFRA( I_NR_SAFRA = CONV #( ME->CARGA-NR_SAFRA ) ).
        OBJ_ROMANEIO->SET_BUKRS( I_BUKRS = ME->CARGA-ID_BUKRS ).
        OBJ_ROMANEIO->SET_BRANCH( I_BRANCH = ME->CARGA-ID_BRANCH ).
        OBJ_ROMANEIO->SET_PARID( I_PARID = <FS_DOCUMENTO>-ID_FORNECEDOR ).
        OBJ_ROMANEIO->SET_ID_CLI_DEST( I_ID_CLI_DEST = CONV #( ME->CARGA-ID_BRANCH ) ).

        CASE ME->CARGA-TP_FRETE.
          WHEN ZIF_CARGA=>ST_TP_FRETE_CIF.
            OBJ_ROMANEIO->SET_TP_FRETE( I_TP_FRETE = ZCL_ROMANEIO=>ST_TP_FRETE_CIF ).
          WHEN ZIF_CARGA=>ST_TP_FRETE_FOB.
            OBJ_ROMANEIO->SET_TP_FRETE( I_TP_FRETE = ZCL_ROMANEIO=>ST_TP_FRETE_FOB ).
        ENDCASE.

        OBJ_ROMANEIO->SET_MATNR( I_MATNR = ME->CARGA-ID_PRODUTO ).
        OBJ_ROMANEIO->SET_PLACA_CAV( I_PLACA_CAV = ME->CARGA-DS_PLACA_TRATOR ).
        OBJ_ROMANEIO->SET_PLACA_CAR1( I_PLACA_CAR1 = ME->CARGA-DS_PLACA_REBOQ_1 ).
        OBJ_ROMANEIO->SET_PLACA_CAR2( I_PLACA_CAR2 = ME->CARGA-DS_PLACA_REBOQ_2 ).
        OBJ_ROMANEIO->SET_PLACA_CAR3( I_PLACA_CAR3 = ME->CARGA-DS_PLACA_REBOQ_3 ).
        OBJ_ROMANEIO->SET_ID_ORDEM( I_ID_ORDEM = ME->CARGA-ID_ORDEM ).
        OBJ_ROMANEIO->SET_MOTORISTA( I_MOTORISTA = ME->CARGA-ID_MOTORISTA ).
        OBJ_ROMANEIO->SET_NR_TICKET( I_NR_TICKET = ME->CARGA-NR_TICKET ).
        OBJ_ROMANEIO->SET_DT_FECHAMENTO( I_DT_FECHAMENTO = ME->CARGA-DT_FECHAMENTO ).
        OBJ_ROMANEIO->SET_HR_FECHAMENTO( I_HR_FECHAMENTO = ME->CARGA-HR_FECHAMENTO ).
        OBJ_ROMANEIO->SET_DT_ABERTURA( I_DT_ABERTURA = ME->CARGA-DT_ABERTURA ).
        OBJ_ROMANEIO->SET_HR_ABERTURA( I_HR_ABERTURA = ME->CARGA-HR_ABERTURA ).
        OBJ_ROMANEIO->SET_AGENTE_FRETE( I_AGENTE_FRETE = ME->CARGA-ID_AGENT_FRETE ).
        OBJ_ROMANEIO->SET_PESO_FISCAL( I_PESO_FISCAL = CONV #( <FS_DOCUMENTO>-NR_QUANTIDADE ) ).
        OBJ_ROMANEIO->SET_DS_OBS( I_DS_OBS = CONV #( <FS_DOCUMENTO>-DS_OBSERVACAO ) ).

        OBJ_ROMANEIO->SET_PESO_LIQ( I_PESO_LIQ = CONV #( <FS_DOCUMENTO>-NM_PESO_LIQUIDO ) ).
        OBJ_ROMANEIO->SET_PESO_SUBTOTAL( I_PESO_SUBTOTAL = CONV #( <FS_DOCUMENTO>-NM_PESO_SUBTOTAL ) ).
        OBJ_ROMANEIO->SET_PESO_TARA( I_PESO_TARA = CONV #( ME->CARGA-NM_PESO_TARA ) ).
        OBJ_ROMANEIO->SET_PESO_BRUTO( I_PESO_BRUTO = CONV #( ME->CARGA-NM_PESO_TARA + <FS_DOCUMENTO>-NM_PESO_SUBTOTAL ) ).

        LOOP AT ME->RESULTADO INTO DATA(WA_RESULTADO) WHERE ID_CLASSIFICACAO = <FS_DOCUMENTO>-ID_CLASSIFICACAO.
          CASE WA_RESULTADO-TP_CARACTERISTICA.
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_UMIDADE.
              OBJ_ROMANEIO->SET_NR_PERC_UMIDADE( I_NR_PERC_UMIDADE = WA_RESULTADO-NR_PERCENTUAL_COM ).
              OBJ_ROMANEIO->SET_NR_QTD_UMIDADE(  I_NR_QTD_UMIDADE = CONV #( WA_RESULTADO-NR_QUANTIDADE_COM ) ).
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_IMPUREZA.
              OBJ_ROMANEIO->SET_NR_PERC_IMPUREZA( I_NR_PERC_IMPUREZA = WA_RESULTADO-NR_PERCENTUAL_COM ).
              OBJ_ROMANEIO->SET_NR_QTD_IMPUREZA(  I_NR_QTD_IMPUREZA = CONV #( WA_RESULTADO-NR_QUANTIDADE_COM ) ).
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_AVARIADO.
              OBJ_ROMANEIO->SET_NR_PERC_AVARIA( I_NR_PERC_AVARIA = WA_RESULTADO-NR_PERCENTUAL_COM ).
              OBJ_ROMANEIO->SET_NR_QTD_AVARIA(  I_NR_QTD_AVARIA = CONV #( WA_RESULTADO-NR_QUANTIDADE_COM ) ).
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_ARDIDO.
              OBJ_ROMANEIO->SET_NR_PERC_ARDIDO( I_NR_PERC_ARDIDO = WA_RESULTADO-NR_PERCENTUAL_COM ).
              OBJ_ROMANEIO->SET_NR_QTD_ARDIDO(  I_NR_QTD_ARDIDO = CONV #( WA_RESULTADO-NR_QUANTIDADE_COM ) ).
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_QUEBRADO.
              OBJ_ROMANEIO->SET_NR_PERC_QUEBRA( I_NR_PERC_QUEBRA = CONV #( WA_RESULTADO-NR_PERCENTUAL_COM ) ).
              OBJ_ROMANEIO->SET_NR_QTD_QUEBRA(  I_NR_QTD_QUEBRA = CONV #( WA_RESULTADO-NR_QUANTIDADE_COM ) ).
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_ESVERDEADO.
              OBJ_ROMANEIO->SET_NR_PERC_ESVERD( I_NR_PERC_ESVERD = WA_RESULTADO-NR_PERCENTUAL_COM ).
              OBJ_ROMANEIO->SET_NR_QTD_ESVERD(  I_NR_QTD_ESVERD = CONV #( WA_RESULTADO-NR_QUANTIDADE_COM ) ).
            WHEN ZIF_CARGA=>ST_TP_CARACT_CLASS_CARUNCHADO.
              OBJ_ROMANEIO->SET_NR_PERC_CARUNCHADO( I_NR_PERC_CARUNCHADO = WA_RESULTADO-NR_PERCENTUAL_COM ).
              OBJ_ROMANEIO->SET_NR_QTD_CARUNCHADO(  I_NR_QTD_CARUNCHADO = CONV #( WA_RESULTADO-NR_QUANTIDADE_COM ) ).
          ENDCASE.
        ENDLOOP.

        LOOP AT ME->ZIF_CARGA~RESULTADO_AVARIADO INTO DATA(WA_RES_AVA) WHERE ID_CLASSIFICACAO = <FS_DOCUMENTO>-ID_CLASSIFICACAO.
          CASE WA_RES_AVA-TP_SUB_CARAC_AVARIADO.
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_ARQ.
              OBJ_ROMANEIO->SET_NR_PERC_AVARIA_ARQ( I_NR_PERC_AVARIA_ARQ = WA_RES_AVA-NR_PERCENTUAL_COM ).
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_QUE.
              OBJ_ROMANEIO->SET_NR_PERC_AVARIA_QUE( I_NR_PERC_AVARIA_QUE = WA_RES_AVA-NR_PERCENTUAL_COM ).
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_MOF.
              OBJ_ROMANEIO->SET_NR_PERC_AVARIA_MOF( I_NR_PERC_AVARIA_MOF = WA_RES_AVA-NR_PERCENTUAL_COM ).
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_PIC.
              OBJ_ROMANEIO->SET_NR_PERC_AVARIA_PIC( I_NR_PERC_AVARIA_PIC = WA_RES_AVA-NR_PERCENTUAL_COM ).
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_FER.
              OBJ_ROMANEIO->SET_NR_PERC_AVARIA_FER( I_NR_PERC_AVARIA_FER = WA_RES_AVA-NR_PERCENTUAL_COM ).
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_GER.
              OBJ_ROMANEIO->SET_NR_PERC_AVARIA_GER( I_NR_PERC_AVARIA_GER = WA_RES_AVA-NR_PERCENTUAL_COM ).
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_ARD.
              OBJ_ROMANEIO->SET_NR_PERC_AVARIA_ARD( I_NR_PERC_AVARIA_ARD = WA_RES_AVA-NR_PERCENTUAL_COM ).
            WHEN ZIF_CARGA=>ST_TP_CARACT_SUB_GES.
              OBJ_ROMANEIO->SET_NR_PERC_AVARIA_GES( I_NR_PERC_AVARIA_GES = WA_RES_AVA-NR_PERCENTUAL_COM ).
          ENDCASE.
        ENDLOOP.

        OBJ_ROMANEIO->SET_ID_CARGA( I_ID_CARGA = <FS_DOCUMENTO>-ID_CARGA ).
        OBJ_ROMANEIO->SET_ID_NOTA( I_ID_NOTA = <FS_DOCUMENTO>-ID_NOTA ).

        ME->GET_FACTORY_TP_TRANSGENIA( EXPORTING I_CLASSIFICACAO = ME->CLASSIFICACAO IMPORTING E_TP_TRANSGENIA = DATA(E_TP_TRANSGENIA) ).
        OBJ_ROMANEIO->SET_TP_TRANSGENIA( I_TP_TRANSGENIA = E_TP_TRANSGENIA ).

        OBJ_ROMANEIO->SET_ID_INTERFACE( I_ID_INTERFACE = ZCL_ROMANEIO=>INTERFACE_CARGA_SAP ).
        OBJ_ROMANEIO->SET_LOCAL_DESCARGA( I_LOCAL_DESCARGA = ME->CARGA-ID_LOCAL_ENTREGA ).
        OBJ_ROMANEIO->SET_TIPO_ENTRADA( I_TIPO_ENTRADA = <FS_DOCUMENTO>-ID_ENTRADA ).

        IF LC_ROMANEIO IS NOT INITIAL.
          OBJ_ROMANEIO->SET_CH_REFER_ENT( I_CH_REFER_ENT = LC_ROMANEIO-CH_REFERENCIA ).
          OBJ_ROMANEIO->SET_ID_REFERENCIA( I_ID_REFERENCIA = LC_ROMANEIO-CH_REFERENCIA ).
        ENDIF.

*-CS2021000183 - 05.04.2022 - JT - inicio
        IF <FS_DOCUMENTO>-DOCNUM_NP IS NOT INITIAL.
          SELECT NFENUM, SERIES, DOCDAT, NFTOT
            FROM J_1BNFDOC
            INTO @DATA(W_JDOC)
              UP TO 1 ROWS
           WHERE DOCNUM = @<FS_DOCUMENTO>-DOCNUM_NP.
          ENDSELECT.

          OBJ_ROMANEIO->SET_NFNUM(  I_NFNUM  = CONV #( W_JDOC-NFENUM ) ).
          OBJ_ROMANEIO->SET_SERIES( I_SERIES = W_JDOC-SERIES ).
          OBJ_ROMANEIO->SET_DOCDAT( I_DOCDAT = W_JDOC-DOCDAT ).
          OBJ_ROMANEIO->SET_NETWR(  I_NETWR  = CONV #( W_JDOC-NFTOT ) ).
        ELSE.
          OBJ_ROMANEIO->SET_NFNUM(  I_NFNUM  = <FS_DOCUMENTO>-NR_NOTA ).
          OBJ_ROMANEIO->SET_SERIES( I_SERIES = <FS_DOCUMENTO>-NM_SERIE ).
          OBJ_ROMANEIO->SET_DOCDAT( I_DOCDAT = <FS_DOCUMENTO>-DT_EMISSAO ).
          OBJ_ROMANEIO->SET_NETWR(  I_NETWR  = CONV #( <FS_DOCUMENTO>-NR_VALOR ) ).
        ENDIF.
*-CS2021000183 - 05.04.2022 - JT - fim

        CASE <FS_DOCUMENTO>-ID_MOD_FISCAL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_PAPEL.
            OBJ_ROMANEIO->SET_NFE( I_NFE = ABAP_FALSE ).
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO.
            OBJ_ROMANEIO->SET_NFE( I_NFE = ABAP_TRUE ).
        ENDCASE.

        "wpp 22102024 US-153342 --->>>>
        DATA(LWA_DADOS_EUDR) = VALUE ZSTRUCT_DADOS_EUDR( EUDR              = ME->ZIF_CARGA~CARGA-EUDR
                                                         PROTOCOLO_EUDR    = ME->ZIF_CARGA~CARGA-PROTOCOLO_EUDR
                                                         ID_PROTOCOLO_EUDR = ME->ZIF_CARGA~CARGA-ID_PROTOCOLO_EUDR ).
        OBJ_ROMANEIO->SET_DADOS_EUDR( I_DADOS_EUDR = LWA_DADOS_EUDR  ). "wpp 22102024 US-153342 --->>>>
        "wpp 22102024 US-153342 <<<----

        OBJ_ROMANEIO->SET_ROMANEIO_COMPLETO( ABAP_TRUE ). "// US-162952 - 09012025 WBARBOSA

        " Ajustatar Itens """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        SELECT SINGLE * INTO @DATA(WA_MARA)
          FROM MARA
         WHERE MATNR EQ @ME->CARGA-ID_PRODUTO.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = WA_MARA-MATKL
          IMPORTING
            OUTPUT = WA_MARA-MATKL.

        IF WA_MARA-MATKL EQ ZIF_CARGA=>ST_GRUPO_ALGODAO_PLUMA. "Algodão

          DATA(ITEMS_ROMANEIO) = OBJ_ROMANEIO->GET_ITENS( ).

          LOOP AT ITEMS_ROMANEIO INTO DATA(WA_ITEMS_ROMANEIO).
            OBJ_ROMANEIO->DEL_ITEM( I_ITEM = WA_ITEMS_ROMANEIO ).
          ENDLOOP.

          DATA: I_ITEM  TYPE ZSDT0001_ITEM.
          DATA: I_FARDO TYPE ZSDT0001_ITEM_FD.

          CLEAR: I_ITEM.
          I_ITEM-CD_ITEM  = '0000000001'.
          I_ITEM-MATNR    = ME->CARGA-ID_PRODUTO.
          I_ITEM-CHARG    = ME->CARGA-NR_SAFRA.
          I_ITEM-LFIMG    = <FS_DOCUMENTO>-NM_PESO_LIQUIDO.
          I_ITEM-MEINS    = 'KG'.
          I_ITEM-BRGEW    = <FS_DOCUMENTO>-NM_PESO_SUBTOTAL.
          I_ITEM-NTGEW    = <FS_DOCUMENTO>-NM_PESO_LIQUIDO.
          I_ITEM-GEWEI    = 'KG'.
          I_ITEM-VOLUM    = <FS_DOCUMENTO>-NR_FARDO.
          I_ITEM-VOLEH    = 'FD'.
          I_ITEM-ID_CARGA = <FS_DOCUMENTO>-ID_CARGA.
          I_ITEM-ID_NOTA  = <FS_DOCUMENTO>-ID_NOTA.
          OBJ_ROMANEIO->ADD_ITEM( CHANGING I_ITEM = I_ITEM ).

          DATA(LC_BLOCOS) = ME->ZIF_CARGA~BLOCOS[].
          SORT LC_BLOCOS BY ZSEQ_INST OBJEK OBJECTTABLE.
          DELETE ADJACENT DUPLICATES FROM LC_BLOCOS COMPARING ZSEQ_INST OBJEK OBJECTTABLE.
          LOOP AT LC_BLOCOS INTO DATA(BLOCOS).

            CLEAR: I_FARDO.
            I_FARDO-CD_ITEM         = I_ITEM-CD_ITEM.
            I_FARDO-CD_ITEM_FARDO   = SY-TABIX.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = I_FARDO-CD_ITEM_FARDO
              IMPORTING
                OUTPUT = I_FARDO-CD_ITEM_FARDO.

            BLOCOS-QT_FARDOS       = 0.
            BLOCOS-PS_FARDOS_BRUTO = 0.
            BLOCOS-PS_FARDOS_LIQUI = 0.

            LOOP AT ME->ZIF_CARGA~BLOCOS INTO DATA(WA_BLOCOS)
              WHERE ZSEQ_INST   EQ BLOCOS-ZSEQ_INST
                AND OBJEK       EQ BLOCOS-OBJEK
                AND OBJECTTABLE EQ BLOCOS-OBJECTTABLE.

              ADD WA_BLOCOS-QT_FARDOS       TO BLOCOS-QT_FARDOS.
              ADD WA_BLOCOS-PS_FARDOS_BRUTO TO BLOCOS-PS_FARDOS_BRUTO.
              ADD WA_BLOCOS-PS_FARDOS_LIQUI TO BLOCOS-PS_FARDOS_LIQUI.

            ENDLOOP.

            I_FARDO-ZSEQ_INST       = BLOCOS-ZSEQ_INST.
            I_FARDO-OBJEK           = BLOCOS-OBJEK.
            I_FARDO-OBJECTTABLE     = BLOCOS-OBJECTTABLE.
            I_FARDO-NM_BLOCO        = BLOCOS-NM_BLOCO.
            I_FARDO-QT_FARDOS       = BLOCOS-QT_FARDOS.
            I_FARDO-MEINS           = BLOCOS-MEINS.
            I_FARDO-PS_FARDOS_LIQUI = BLOCOS-PS_FARDOS_LIQUI.
            I_FARDO-PS_FARDOS_BRUTO = BLOCOS-PS_FARDOS_BRUTO.
            OBJ_ROMANEIO->ADD_FARDOS( I_FARDOS = I_FARDO ).
            CLEAR: I_FARDO.

          ENDLOOP.
        ENDIF.
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        DATA(R_GEROU) = OBJ_ROMANEIO->ZIF_CADASTRO~GRAVAR_REGISTRO( ).

        IF R_GEROU EQ ABAP_TRUE.
          OBJ_ROMANEIO->ZIF_CADASTRO~GET_REGISTRO( IMPORTING E_REGISTRO = LC_ROMANEIO ).
          <FS_DOCUMENTO>-NR_ROMANEIO_ENT   = LC_ROMANEIO-NR_ROMANEIO.
          <FS_DOCUMENTO>-CH_REFERENCIA_ENT = LC_ROMANEIO-CH_REFERENCIA.
          ME->CK_ALTEROU = ABAP_TRUE.
        ELSE.
          IF OBJ_ROMANEIO->CK_ALTEROU EQ ABAP_TRUE.
            ME->CARGA-TP_STATUS = ZIF_CARGA=>ST_STATUS_FECHADO.
            IF ME->CARGA-ID_CARGA IS NOT INITIAL.
              DELETE FROM ZSDT0001 WHERE TP_MOVIMENTO = ZCL_ROMANEIO=>ST_TP_MOVIMENTO_ENTRADA AND ID_CARGA EQ ME->CARGA-ID_CARGA.
              COMMIT WORK.
            ENDIF.
            RAISE EXCEPTION TYPE ZCX_CARGA
              EXPORTING
                TEXTID = VALUE #( MSGNO = SY-MSGNO MSGID = SY-MSGID ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
                MSGNO  = SY-MSGNO
                MSGID  = SY-MSGID
                MSGTY  = 'E'
                MSGV1  = SY-MSGV1
                MSGV2  = SY-MSGV2
                MSGV3  = SY-MSGV3
                MSGV4  = SY-MSGV4.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CLEAR: OBJ_ROMANEIO.

  ENDMETHOD.


  METHOD zif_carga~set_gerar_romaneio_saida.

    DATA: obj_romaneio   TYPE REF TO zcl_romaneio,
          lc_romaneio    TYPE zsdt0001,
          ck_gerar_saida TYPE char01,
          wa_acttab      TYPE j_1bnfe_active.


    DATA: lc_nr_qtd_umidade  TYPE znr_qtd_umidade,
          lc_nr_qtd_impureza TYPE znr_qtd_impureza,
          lc_nr_qtd_avaria   TYPE znr_qtd_avaria,
          lc_nr_qtd_ardido   TYPE znr_qtd_ardido,
          lc_nr_qtd_quebra   TYPE znr_qtd_quebra,
          lc_nr_qtd_esverd   TYPE znr_qtd_esverd,
          lc_nr_qtd_carunch  TYPE znr_qtd_carunchado,
          lc_nr_quantidade   TYPE zde_nr_quantidade,
          lc_peso_liq	       TYPE ntgew,
          lc_peso_subtotal   TYPE brgew,
          lc_docnum          TYPE j_1bnfdoc-docnum.

    r_carga = me.

    CHECK me->zif_carga~at_manutencao EQ abap_false.
    "CHECK ME->ZIF_CARGA~CK_EXECUTAR_MANUTENCAO_ENTRADA EQ ABAP_FALSE.
    CHECK NOT ( me->carga-ck_enviado_opus EQ abap_true AND me->carga-ck_recebido_opus EQ abap_false ).

    CREATE OBJECT obj_romaneio.

    me->get_romaneio_entrada( EXPORTING i_id_carga  = me->carga-id_carga IMPORTING e_romaneios = DATA(romaneios_entrada) ).

    me->get_romaneio_saida( EXPORTING i_id_carga  = me->carga-id_carga IMPORTING e_romaneios = DATA(romaneios) ).

    CLEAR: lc_romaneio.

    ck_gerar_saida = abap_true.

    "Somar Quantidades de Desconto por ordem de Venda
    lc_nr_qtd_umidade  = 0.
    lc_nr_qtd_impureza = 0.
    lc_nr_qtd_avaria   = 0.
    lc_nr_qtd_ardido   = 0.
    lc_nr_qtd_quebra   = 0.
    lc_nr_qtd_esverd   = 0.
    lc_nr_qtd_carunch  = 0.
    lc_peso_liq        = 0.
    lc_peso_subtotal   = 0.
    lc_nr_quantidade   = 0.

    SELECT SINGLE * INTO @DATA(wa_mara)
      FROM mara
     WHERE matnr EQ @me->carga-id_produto.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_mara-matkl
      IMPORTING
        output = wa_mara-matkl.

    "Verificar se todas as notas de entrada estão geradas
    LOOP AT me->documento_fiscal INTO DATA(wa_entrada).

      CASE wa_entrada-tp_operacao.

        WHEN '06' OR '07'.

          IF wa_entrada-mm_mblnr IS INITIAL.
            ck_gerar_saida = abap_false.
          ENDIF.

        WHEN OTHERS.

          IF wa_entrada-docnum IS NOT INITIAL.
            lc_docnum = wa_entrada-docnum.
          ELSE.
            lc_docnum = wa_entrada-docnum_np.
          ENDIF.

          IF lc_docnum IS INITIAL.
            ck_gerar_saida = abap_false.
          ELSE.

            SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
              FROM j_1bnfdoc
             WHERE docnum EQ @lc_docnum.

            IF wa_j_1bnfdoc-form IS NOT INITIAL AND wa_j_1bnfdoc-cancel EQ abap_false.

              "Verificar NF-e Autorizada
              CALL FUNCTION 'J_1B_NFE_XML_RAED_ACTIVE_TAB'
                EXPORTING
                  i_docnum = lc_docnum
                IMPORTING
                  e_acttab = wa_acttab
                EXCEPTIONS
                  no_entry = 1
                  OTHERS   = 2.

              IF sy-subrc IS NOT INITIAL.
                ck_gerar_saida = abap_false.
              ELSE.
                IF wa_acttab-nfnum9 IS INITIAL.
                  ck_gerar_saida = abap_false.
                ELSEIF wa_acttab-docsta NE '1'.
                  ck_gerar_saida = abap_false.
                ELSEIF wa_acttab-cancel EQ abap_true.
                  ck_gerar_saida = abap_false.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

      ENDCASE.

      "Somar Quantidades de Desconto por ordem de Venda
      LOOP AT romaneios_entrada INTO DATA(wa_romaneios_entrada) WHERE id_carga EQ wa_entrada-id_carga AND id_nota EQ wa_entrada-id_nota.
        ADD wa_romaneios_entrada-nr_qtd_umidade  TO lc_nr_qtd_umidade.
        ADD wa_romaneios_entrada-nr_qtd_impureza TO lc_nr_qtd_impureza.
        ADD wa_romaneios_entrada-nr_qtd_avaria   TO lc_nr_qtd_avaria.
        ADD wa_romaneios_entrada-nr_qtd_ardido   TO lc_nr_qtd_ardido.
        ADD wa_romaneios_entrada-nr_qtd_quebra   TO lc_nr_qtd_quebra.
        ADD wa_romaneios_entrada-nr_qtd_esverd   TO lc_nr_qtd_esverd.
        ADD wa_romaneios_entrada-nr_qtd_carunch  TO lc_nr_qtd_carunch.
        ADD wa_romaneios_entrada-peso_liq        TO lc_peso_liq.
        ADD wa_romaneios_entrada-peso_subtotal   TO lc_peso_subtotal.
      ENDLOOP.

      ADD wa_entrada-nr_quantidade TO lc_nr_quantidade.

    ENDLOOP.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF ck_gerar_saida EQ abap_false.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid  = zcx_carga=>zcx_nao_gerou_romaneio_saida-msgid
                            msgno  = zcx_carga=>zcx_nao_gerou_romaneio_saida-msgno )
          msgty  = 'E'
          msgid  = zcx_carga=>zcx_nao_gerou_romaneio_saida-msgid
          msgno  = zcx_carga=>zcx_nao_gerou_romaneio_saida-msgno.
    ENDIF.

    LOOP AT me->zif_carga~pedido_compra INTO DATA(wa_pedido).

      READ TABLE romaneios INTO DATA(wa_romaneio) WITH KEY id_carga = wa_pedido-id_carga vbeln = wa_pedido-nr_pedido_compra.
      IF sy-subrc IS INITIAL.
        obj_romaneio->zif_cadastro~set_registro( i_id_registro = wa_romaneio-ch_referencia ).
      ELSE.
        obj_romaneio->novo_registro( ).
      ENDIF.

      "Somatória dos Pesos de Descontos
      obj_romaneio->set_nr_qtd_umidade(  i_nr_qtd_umidade  = lc_nr_qtd_umidade  ).
      obj_romaneio->set_nr_qtd_impureza( i_nr_qtd_impureza = lc_nr_qtd_impureza ).
      obj_romaneio->set_nr_qtd_avaria(   i_nr_qtd_avaria   = lc_nr_qtd_avaria   ).
      obj_romaneio->set_nr_qtd_ardido(   i_nr_qtd_ardido   = lc_nr_qtd_ardido   ).
      obj_romaneio->set_nr_qtd_quebra(   i_nr_qtd_quebra   = lc_nr_qtd_quebra   ).
      obj_romaneio->set_nr_qtd_esverd(   i_nr_qtd_esverd   = lc_nr_qtd_esverd   ).
      obj_romaneio->set_nr_qtd_carunchado( i_nr_qtd_carunchado = lc_nr_qtd_carunch  ).

      "Somatória dos Pesos Liquido/SubTotal
      IF wa_romaneio-st_proc IS INITIAL.

        CASE wa_mara-matkl.
          WHEN zif_carga=>st_grupo_algodao_pluma. "Algodão.
            obj_romaneio->set_peso_liq( i_peso_liq = CONV #( lc_nr_quantidade ) ).
          WHEN OTHERS.
            obj_romaneio->set_peso_liq( i_peso_liq = CONV #( wa_pedido-nm_peso_subtotal ) ).
        ENDCASE.

        obj_romaneio->set_peso_subtotal( i_peso_subtotal = CONV #( wa_pedido-nm_peso_subtotal ) ). "LC_PESO_SUBTOTAL   ).
        obj_romaneio->set_peso_tara( i_peso_tara = CONV #( wa_pedido-nm_peso_tara ) ).
        obj_romaneio->set_peso_bruto( i_peso_bruto = CONV #( wa_pedido-nm_peso_tara + wa_pedido-nm_peso_subtotal ) ). "LC_PESO_SUBTOTAL ) ).
        obj_romaneio->set_peso_fiscal( i_peso_fiscal = CONV #( lc_nr_quantidade ) ).
      ENDIF.

      "Cópia de % de Qualidade
      LOOP AT me->resultado INTO DATA(wa_resultado) WHERE id_classificacao = me->carga-id_classificacao.
        CASE wa_resultado-tp_caracteristica.
          WHEN zif_carga=>st_tp_caract_class_umidade.
            obj_romaneio->set_nr_perc_umidade( i_nr_perc_umidade = wa_resultado-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_class_impureza.
            obj_romaneio->set_nr_perc_impureza( i_nr_perc_impureza = wa_resultado-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_class_avariado.
            obj_romaneio->set_nr_perc_avaria( i_nr_perc_avaria = wa_resultado-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_class_ardido.
            obj_romaneio->set_nr_perc_ardido( i_nr_perc_ardido = wa_resultado-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_class_quebrado.
            obj_romaneio->set_nr_perc_quebra( i_nr_perc_quebra = CONV #( wa_resultado-nr_percentual_com ) ).
          WHEN zif_carga=>st_tp_caract_class_esverdeado.
            obj_romaneio->set_nr_perc_esverd( i_nr_perc_esverd = wa_resultado-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_class_carunchado.
            obj_romaneio->set_nr_perc_carunchado( i_nr_perc_carunchado = wa_resultado-nr_percentual_com ).
        ENDCASE.
      ENDLOOP.

      LOOP AT me->zif_carga~resultado_avariado INTO DATA(wa_res_ava) WHERE id_classificacao = me->carga-id_classificacao.
        CASE wa_res_ava-tp_sub_carac_avariado.
          WHEN zif_carga=>st_tp_caract_sub_arq.
            obj_romaneio->set_nr_perc_avaria_arq( i_nr_perc_avaria_arq = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_que.
            obj_romaneio->set_nr_perc_avaria_que( i_nr_perc_avaria_que = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_mof.
            obj_romaneio->set_nr_perc_avaria_mof( i_nr_perc_avaria_mof = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_pic.
            obj_romaneio->set_nr_perc_avaria_pic( i_nr_perc_avaria_pic = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_fer.
            obj_romaneio->set_nr_perc_avaria_fer( i_nr_perc_avaria_fer = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_ger.
            obj_romaneio->set_nr_perc_avaria_ger( i_nr_perc_avaria_ger = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_ard.
            obj_romaneio->set_nr_perc_avaria_ard( i_nr_perc_avaria_ard = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_ges.
            obj_romaneio->set_nr_perc_avaria_ges( i_nr_perc_avaria_ges = wa_res_ava-nr_percentual_com ).
        ENDCASE.
      ENDLOOP.

      IF wa_romaneio-st_proc IS INITIAL.
        me->get_factory_tp_transgenia( EXPORTING i_classificacao = me->classificacao IMPORTING e_tp_transgenia = DATA(e_tp_transgenia) ).
        me->zif_carga~get_info_pedido_compra( EXPORTING i_pedido_compra = wa_pedido-nr_pedido_compra IMPORTING e_pedido = DATA(e_pedido) ).
        obj_romaneio->set_tp_movimento( i_tp_movimento = zcl_romaneio=>st_tp_movimento_saida ).
        obj_romaneio->set_dt_movimento( i_dt_movimento = me->carga-dt_movimento ).
        obj_romaneio->set_nr_safra( i_nr_safra = CONV #( me->carga-nr_safra ) ).
        obj_romaneio->set_bukrs( i_bukrs = me->carga-id_bukrs ).
        obj_romaneio->set_branch( i_branch = me->carga-id_branch ).
        obj_romaneio->set_id_cli_dest( i_id_cli_dest = me->carga-id_local_destino ).
        obj_romaneio->set_matnr( i_matnr = me->carga-id_produto ).
        obj_romaneio->set_placa_cav( i_placa_cav = me->carga-ds_placa_trator ).
        obj_romaneio->set_placa_car1( i_placa_car1 = me->carga-ds_placa_reboq_1 ).
        obj_romaneio->set_placa_car2( i_placa_car2 = me->carga-ds_placa_reboq_2 ).
        obj_romaneio->set_placa_car3( i_placa_car3 = me->carga-ds_placa_reboq_3 ).
        obj_romaneio->set_motorista( i_motorista = me->carga-id_motorista ).
        obj_romaneio->set_dt_fechamento( i_dt_fechamento = me->carga-dt_fechamento ).
        obj_romaneio->set_hr_fechamento( i_hr_fechamento = me->carga-hr_fechamento ).
        obj_romaneio->set_dt_abertura( i_dt_abertura = me->carga-dt_abertura ).
        obj_romaneio->set_hr_abertura( i_hr_abertura = me->carga-hr_abertura ).
        obj_romaneio->set_agente_frete( i_agente_frete = me->carga-id_agent_frete ).
        obj_romaneio->set_id_interface( i_id_interface = zcl_romaneio=>interface_carga_sap ).

        IF e_pedido-id_local_coleta IS NOT INITIAL.
          obj_romaneio->set_parid( i_parid = e_pedido-id_local_coleta ).
        ELSE.
          obj_romaneio->set_parid( i_parid = CONV #( me->carga-id_branch ) ).
        ENDIF.

        obj_romaneio->set_id_carga( i_id_carga = me->carga-id_carga ).
        obj_romaneio->set_id_ordem( i_id_ordem = me->carga-id_ordem ).
        obj_romaneio->set_tp_transgenia( i_tp_transgenia = e_tp_transgenia ).
        obj_romaneio->set_vbeln( i_vbeln = wa_pedido-nr_pedido_compra ).
        obj_romaneio->set_tp_frete( i_tp_frete = CONV #( e_pedido-ds_tipo_frete ) ).
      ENDIF.
      obj_romaneio->set_nr_ticket( i_nr_ticket = me->carga-nr_ticket ).

      "wpp 22102024 US-153342 --->>>>
      DATA(lwa_dados_eudr) = VALUE zstruct_dados_eudr( eudr              = me->zif_carga~carga-eudr
                                                       protocolo_eudr    = me->zif_carga~carga-protocolo_eudr
                                                       id_protocolo_eudr = me->zif_carga~carga-id_protocolo_eudr ).
      obj_romaneio->set_dados_eudr( i_dados_eudr = lwa_dados_eudr  ). "wpp 22102024 US-153342 --->>>>
      "wpp 22102024 US-153342 <<<----

      READ TABLE romaneios_entrada INDEX 1 INTO wa_romaneios_entrada.
      IF sy-subrc IS INITIAL.
        obj_romaneio->set_id_referencia( i_id_referencia = CONV #( wa_romaneios_entrada-nr_romaneio ) ).
      ENDIF.

      DATA(r_gerou) = obj_romaneio->zif_cadastro~gravar_registro( ).

      IF r_gerou EQ abap_true OR obj_romaneio->ck_alterou EQ abap_false.
        obj_romaneio->zif_cadastro~get_registro( IMPORTING e_registro = lc_romaneio ).
        READ TABLE me->zif_carga~pedido_compra ASSIGNING FIELD-SYMBOL(<fs_pedido>) WITH KEY nr_pedido_compra = wa_pedido-nr_pedido_compra.
        <fs_pedido>-nr_romaneio_sai   = lc_romaneio-nr_romaneio.
        <fs_pedido>-ch_referencia_sai = lc_romaneio-ch_referencia.
        me->ck_alterou = abap_true.
        "retirado o gravar
        "BREAK-POINT.
        "ME->GRAVAR_REGISTRO( ).
      ELSE.

        TRY .
            "Se não der erro pode excluir o Romaneio
            me->zif_carga~get_ck_excluir_romaneio_saida( EXPORTING i_ck_opus = abap_true ).
            IF me->carga-id_carga IS NOT INITIAL.
              DELETE FROM zsdt0001 WHERE tp_movimento = zcl_romaneio=>st_tp_movimento_saida AND id_carga EQ me->carga-id_carga.
              COMMIT WORK.
            ENDIF.
            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid = VALUE #( msgno = sy-msgno msgid = sy-msgid attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
                msgno  = sy-msgno
                msgid  = sy-msgid
                msgty  = 'E'
                msgv1  = sy-msgv1
                msgv2  = sy-msgv2
                msgv3  = sy-msgv3
                msgv4  = sy-msgv4.

          CATCH zcx_carga.
            "Não pode Excluir o Romaneio Continua com Ela
            obj_romaneio->zif_cadastro~get_registro( IMPORTING e_registro = lc_romaneio ).
            READ TABLE me->zif_carga~pedido_compra ASSIGNING <fs_pedido> WITH KEY nr_pedido_compra = wa_pedido-nr_pedido_compra.
            <fs_pedido>-nr_romaneio_sai   = lc_romaneio-nr_romaneio.
            <fs_pedido>-ch_referencia_sai = lc_romaneio-ch_referencia.
            me->ck_alterou = abap_true.
        ENDTRY.

      ENDIF.

    ENDLOOP.

    LOOP AT me->zif_carga~ordem_venda INTO DATA(wa_ordem).

      READ TABLE romaneios INTO wa_romaneio WITH KEY id_carga = wa_ordem-id_carga vbeln = wa_ordem-nr_ordem_venda.
      IF sy-subrc IS INITIAL.
        obj_romaneio->zif_cadastro~set_registro( i_id_registro = wa_romaneio-ch_referencia ).
      ELSE.
        obj_romaneio->novo_registro( ).
      ENDIF.

      "Somatória dos Pesos de Descontos
      obj_romaneio->set_nr_qtd_umidade(  i_nr_qtd_umidade  = lc_nr_qtd_umidade  ).
      obj_romaneio->set_nr_qtd_impureza( i_nr_qtd_impureza = lc_nr_qtd_impureza ).
      obj_romaneio->set_nr_qtd_avaria(   i_nr_qtd_avaria   = lc_nr_qtd_avaria   ).
      obj_romaneio->set_nr_qtd_ardido(   i_nr_qtd_ardido   = lc_nr_qtd_ardido   ).
      obj_romaneio->set_nr_qtd_quebra(   i_nr_qtd_quebra   = lc_nr_qtd_quebra   ).
      obj_romaneio->set_nr_qtd_esverd(   i_nr_qtd_esverd   = lc_nr_qtd_esverd   ).
      obj_romaneio->set_nr_qtd_carunchado( i_nr_qtd_carunchado = lc_nr_qtd_carunch ).

      IF wa_romaneio-st_proc IS INITIAL.
        "Somatória dos Pesos Liquido/SubTotal

        CASE wa_mara-matkl.
          WHEN zif_carga=>st_grupo_algodao_pluma. "Algodão.
            obj_romaneio->set_peso_liq( i_peso_liq = CONV #( wa_ordem-nm_peso_liquido ) ).
          WHEN OTHERS.
            obj_romaneio->set_peso_liq( i_peso_liq = CONV #( wa_ordem-nm_peso_subtotal ) ).
        ENDCASE.

        obj_romaneio->set_peso_subtotal(   i_peso_subtotal   = CONV #( wa_ordem-nm_peso_subtotal ) ). "LC_PESO_SUBTOTAL   ).
        obj_romaneio->set_peso_tara(       i_peso_tara       = CONV #( wa_ordem-nm_peso_tara ) ). " ME->CARGA-NM_PESO_TARA ) ).
        obj_romaneio->set_peso_bruto(      i_peso_bruto = CONV #( wa_ordem-nm_peso_tara + wa_ordem-nm_peso_subtotal ) ). "ME->CARGA-NM_PESO_TARA + LC_PESO_SUBTOTAL ) ).
        obj_romaneio->set_peso_fiscal(     i_peso_fiscal = CONV #( wa_ordem-nm_peso_liquido ) ). " LC_NR_QUANTIDADE ) ).
      ENDIF.

      "Cópia de % de Qualidade
      LOOP AT me->resultado INTO wa_resultado WHERE id_classificacao = me->carga-id_classificacao.
        CASE wa_resultado-tp_caracteristica.
          WHEN zif_carga=>st_tp_caract_class_umidade.
            obj_romaneio->set_nr_perc_umidade( i_nr_perc_umidade = wa_resultado-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_class_impureza.
            obj_romaneio->set_nr_perc_impureza( i_nr_perc_impureza = wa_resultado-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_class_avariado.
            obj_romaneio->set_nr_perc_avaria( i_nr_perc_avaria = wa_resultado-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_class_ardido.
            obj_romaneio->set_nr_perc_ardido( i_nr_perc_ardido = wa_resultado-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_class_quebrado.
            obj_romaneio->set_nr_perc_quebra( i_nr_perc_quebra = CONV #( wa_resultado-nr_percentual_com ) ).
          WHEN zif_carga=>st_tp_caract_class_esverdeado.
            obj_romaneio->set_nr_perc_esverd( i_nr_perc_esverd = wa_resultado-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_class_carunchado.
            obj_romaneio->set_nr_perc_carunchado( i_nr_perc_carunchado = wa_resultado-nr_percentual_com ).
        ENDCASE.
      ENDLOOP.

      LOOP AT me->zif_carga~resultado_avariado INTO wa_res_ava WHERE id_classificacao = me->carga-id_classificacao.
        CASE wa_res_ava-tp_sub_carac_avariado.
          WHEN zif_carga=>st_tp_caract_sub_arq.
            obj_romaneio->set_nr_perc_avaria_arq( i_nr_perc_avaria_arq = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_que.
            obj_romaneio->set_nr_perc_avaria_que( i_nr_perc_avaria_que = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_mof.
            obj_romaneio->set_nr_perc_avaria_mof( i_nr_perc_avaria_mof = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_pic.
            obj_romaneio->set_nr_perc_avaria_pic( i_nr_perc_avaria_pic = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_fer.
            obj_romaneio->set_nr_perc_avaria_fer( i_nr_perc_avaria_fer = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_ger.
            obj_romaneio->set_nr_perc_avaria_ger( i_nr_perc_avaria_ger = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_ard.
            obj_romaneio->set_nr_perc_avaria_ard( i_nr_perc_avaria_ard = wa_res_ava-nr_percentual_com ).
          WHEN zif_carga=>st_tp_caract_sub_ges.
            obj_romaneio->set_nr_perc_avaria_ges( i_nr_perc_avaria_ges = wa_res_ava-nr_percentual_com ).
        ENDCASE.
      ENDLOOP.

      IF wa_romaneio-st_proc IS INITIAL.
        me->get_factory_tp_transgenia( EXPORTING i_classificacao = me->classificacao IMPORTING e_tp_transgenia = e_tp_transgenia ).
        me->get_info_ordem_venda( EXPORTING i_ordem_venda = wa_ordem-nr_ordem_venda IMPORTING e_ordem = DATA(e_ordem) ).
        obj_romaneio->set_tp_movimento( i_tp_movimento = zcl_romaneio=>st_tp_movimento_saida ).
        obj_romaneio->set_dt_movimento( i_dt_movimento = me->carga-dt_movimento ).
        obj_romaneio->set_nr_safra( i_nr_safra = CONV #( me->carga-nr_safra ) ).
        obj_romaneio->set_bukrs( i_bukrs = me->carga-id_bukrs ).
        obj_romaneio->set_branch( i_branch = me->carga-id_branch ).
        obj_romaneio->set_id_cli_dest( i_id_cli_dest = me->carga-id_local_destino ).
        obj_romaneio->set_matnr( i_matnr = me->carga-id_produto ).
        obj_romaneio->set_placa_cav( i_placa_cav = me->carga-ds_placa_trator ).
        obj_romaneio->set_placa_car1( i_placa_car1 = me->carga-ds_placa_reboq_1 ).
        obj_romaneio->set_placa_car2( i_placa_car2 = me->carga-ds_placa_reboq_2 ).
        obj_romaneio->set_placa_car3( i_placa_car3 = me->carga-ds_placa_reboq_3 ).
        obj_romaneio->set_motorista( i_motorista = me->carga-id_motorista ).
        obj_romaneio->set_dt_fechamento( i_dt_fechamento = me->carga-dt_fechamento ).
        obj_romaneio->set_hr_fechamento( i_hr_fechamento = me->carga-hr_fechamento ).
        obj_romaneio->set_dt_abertura( i_dt_abertura = me->carga-dt_abertura ).
        obj_romaneio->set_hr_abertura( i_hr_abertura = me->carga-hr_abertura ).
        obj_romaneio->set_agente_frete( i_agente_frete = me->carga-id_agent_frete ).
        obj_romaneio->set_id_interface( i_id_interface = zcl_romaneio=>interface_carga_sap ).
        obj_romaneio->set_parid( i_parid = CONV #( me->carga-id_branch ) ).
        obj_romaneio->set_id_carga( i_id_carga = me->carga-id_carga ).
        obj_romaneio->set_tp_transgenia( i_tp_transgenia = e_tp_transgenia ).
        obj_romaneio->set_id_ordem( i_id_ordem = me->carga-id_ordem ).

        zcl_ordem_venda=>zif_ordem_venda~get_instance(
                      )->set_ordem_venda( i_vbeln = wa_ordem-nr_ordem_venda
                      )->get_tipo_frete( IMPORTING e_tipo_frete = DATA(e_tipo_frete)
                      ).

        obj_romaneio->set_vbeln( i_vbeln = wa_ordem-nr_ordem_venda ).
        obj_romaneio->set_tp_frete( i_tp_frete = CONV #( e_tipo_frete ) ).

        " Ajustatar Itens """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        IF wa_mara-matkl EQ zif_carga=>st_grupo_algodao_pluma. "Algodão

          DATA(items_romaneio) = obj_romaneio->get_itens( ).

          LOOP AT items_romaneio INTO DATA(wa_items_romaneio).
            obj_romaneio->del_item( i_item = wa_items_romaneio ).
          ENDLOOP.

          DATA: i_item  TYPE zsdt0001_item.
          DATA: i_fardo TYPE zsdt0001_item_fd.

          CLEAR: i_item.
          i_item-cd_item  = '0000000001'.
          i_item-matnr    = me->carga-id_produto.
          i_item-vbeln    = wa_ordem-nr_ordem_venda.
          i_item-charg    = me->carga-nr_safra.
          i_item-lfimg    = wa_ordem-nm_peso_liquido.
          i_item-meins    = 'KG'.
          i_item-brgew    = wa_ordem-nm_peso_subtotal.
          i_item-ntgew    = wa_ordem-nm_peso_liquido.
          i_item-gewei    = 'KG'.
          i_item-volum    = wa_ordem-qt_fardos.
          i_item-voleh    = 'FD'.
          i_item-id_carga = me->carga-id_carga.
          obj_romaneio->add_item( CHANGING i_item = i_item ).

          DATA(lc_blocos) = me->zif_carga~blocos[].
          DELETE lc_blocos WHERE nr_ordem_venda NE wa_ordem-nr_ordem_venda.
          SORT lc_blocos BY zseq_inst objek objecttable.
          DELETE ADJACENT DUPLICATES FROM lc_blocos COMPARING zseq_inst objek objecttable.
          LOOP AT lc_blocos INTO DATA(blocos).

            CLEAR: i_fardo.
            i_fardo-cd_item         = i_item-cd_item.
            i_fardo-cd_item_fardo   = sy-tabix.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = i_fardo-cd_item_fardo
              IMPORTING
                output = i_fardo-cd_item_fardo.

            blocos-qt_fardos       = 0.
            blocos-ps_fardos_bruto = 0.
            blocos-ps_fardos_liqui = 0.

            LOOP AT me->zif_carga~blocos INTO DATA(wa_blocos)
              WHERE zseq_inst      EQ blocos-zseq_inst
                AND objek          EQ blocos-objek
                AND objecttable    EQ blocos-objecttable
                AND nr_ordem_venda EQ blocos-nr_ordem_venda.

              ADD wa_blocos-qt_fardos       TO blocos-qt_fardos.
              ADD wa_blocos-ps_fardos_bruto TO blocos-ps_fardos_bruto.
              ADD wa_blocos-ps_fardos_liqui TO blocos-ps_fardos_liqui.
            ENDLOOP.

            i_fardo-zseq_inst       = blocos-zseq_inst.
            i_fardo-objek           = blocos-objek.
            i_fardo-objecttable     = blocos-objecttable.
            i_fardo-nm_bloco        = blocos-nm_bloco.
            i_fardo-qt_fardos       = blocos-qt_fardos.
            i_fardo-meins           = blocos-meins.
            i_fardo-ps_fardos_liqui = blocos-ps_fardos_liqui.
            i_fardo-ps_fardos_bruto = blocos-ps_fardos_bruto.
            obj_romaneio->add_fardos( i_fardos = i_fardo ).
            CLEAR: i_fardo.

          ENDLOOP.
        ENDIF.
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      ENDIF.

      obj_romaneio->set_nr_ticket( i_nr_ticket = me->carga-nr_ticket ).

      "wpp 22102024 US-153342 --->>>>
      lwa_dados_eudr = VALUE zstruct_dados_eudr( eudr              = me->zif_carga~carga-eudr
                                                 protocolo_eudr    = me->zif_carga~carga-protocolo_eudr
                                                 id_protocolo_eudr = me->zif_carga~carga-id_protocolo_eudr ).
      obj_romaneio->set_dados_eudr( i_dados_eudr = lwa_dados_eudr  ). "wpp 22102024 US-153342 --->>>>
      "wpp 22102024 US-153342 <<<----

      READ TABLE romaneios_entrada INDEX 1 INTO wa_romaneios_entrada.
      IF sy-subrc IS INITIAL.
        obj_romaneio->set_id_referencia( i_id_referencia = CONV #( wa_romaneios_entrada-nr_romaneio ) ).
      ENDIF.

      r_gerou = obj_romaneio->zif_cadastro~gravar_registro( ).

      IF r_gerou EQ abap_true OR obj_romaneio->ck_alterou EQ abap_false.
        obj_romaneio->zif_cadastro~get_registro( IMPORTING e_registro = lc_romaneio ).
        READ TABLE me->zif_carga~ordem_venda ASSIGNING FIELD-SYMBOL(<fs_ordem>) WITH KEY nr_ordem_venda = wa_ordem-nr_ordem_venda.
        <fs_ordem>-nr_romaneio_sai   = lc_romaneio-nr_romaneio.
        <fs_ordem>-ch_referencia_sai = lc_romaneio-ch_referencia.
        me->ck_alterou = abap_true.
      ELSE.
        IF me->carga-id_carga IS NOT INITIAL.

          TRY .
              "Se não der erro pode excluir o Romaneio
              me->zif_carga~get_ck_excluir_romaneio_saida( EXPORTING i_ck_opus = abap_true ).
              IF me->carga-id_carga IS NOT INITIAL.
                DELETE FROM zsdt0001
                 WHERE tp_movimento EQ zcl_romaneio=>st_tp_movimento_saida
                   AND id_carga     EQ me->carga-id_carga
                   AND id_carga     NE space.

                IF sy-subrc IS INITIAL.

                  SELECT * INTO TABLE @DATA(it_zsdt0001_item)
                    FROM zsdt0001_item
                   WHERE id_carga EQ @me->carga-id_carga
                     AND id_carga NE @space.

                  LOOP AT it_zsdt0001_item INTO DATA(wa_zsdt0001_item).
                    DELETE FROM zsdt0001_item_fd
                     WHERE ch_referencia EQ wa_zsdt0001_item-ch_referencia
                       AND cd_item EQ wa_zsdt0001_item-cd_item.
                  ENDLOOP.

                  DELETE FROM zsdt0001_item WHERE id_carga EQ me->carga-id_carga.
                ENDIF.

                COMMIT WORK.
              ENDIF.
              RAISE EXCEPTION TYPE zcx_carga
                EXPORTING
                  textid = VALUE #( msgno = sy-msgno msgid = sy-msgid attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
                  msgno  = sy-msgno
                  msgid  = sy-msgid
                  msgty  = 'E'
                  msgv1  = sy-msgv1
                  msgv2  = sy-msgv2
                  msgv3  = sy-msgv3
                  msgv4  = sy-msgv4.

            CATCH zcx_carga.
              "Não pode Excluir o Romaneio Continua com Ela
              obj_romaneio->zif_cadastro~get_registro( IMPORTING e_registro = lc_romaneio ).
              READ TABLE me->zif_carga~ordem_venda ASSIGNING <fs_ordem> WITH KEY nr_ordem_venda = wa_ordem-nr_ordem_venda.
              <fs_ordem>-nr_romaneio_sai   = lc_romaneio-nr_romaneio.
              <fs_ordem>-ch_referencia_sai = lc_romaneio-ch_referencia.
              me->ck_alterou = abap_true.
          ENDTRY.


        ENDIF.
        IF sy-msgid IS NOT INITIAL AND sy-msgno IS NOT INITIAL.
          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = VALUE #( msgno = sy-msgno msgid = sy-msgid attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
              msgno  = sy-msgno
              msgid  = sy-msgid
              msgty  = 'E'
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = sy-msgv3
              msgv4  = sy-msgv4.
        ELSE.
          me->zif_carga~gera_erro_geral( i_texto = 'SAP: Erro ao tentar gerar Romaneio de Saída!' ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    FREE obj_romaneio.
    CLEAR: obj_romaneio.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_GERAR_TRANSPORTE.
    R_CARGA = ME.
  ENDMETHOD.


  METHOD zif_carga~set_gera_imposto_nota.

    DATA: wa_branch_address TYPE bapi0002_3,
          vg_netwrt         TYPE j_1bnflin-netwrt,
          wa_imposto        LIKE LINE OF me->documento_fiscal_imp_ret.

    DATA:
      it_impost_ret_mt     TYPE TABLE OF tvarvc,
      it_impost_ret_dif_mt TYPE TABLE OF tvarvc,
      rg_imposto_mt        TYPE RANGE OF witht,
      rg_imposto_dif_mt    TYPE RANGE OF witht,
      lc_retorno           TYPE zde_zmme0004,
      wa_zde_zmme0003      TYPE zde_zmme0003,
      zvg_message          TYPE scx_attrname,
      it_zsdt0001im        TYPE TABLE OF zsdt0001im, "BUG SOLTO 145463 / IR184715 / AOENNING
      zvg_lines_api        TYPE i,
      zvg_lines_table      TYPE i.



    DELETE me->documento_fiscal_imp_ret WHERE id_nota EQ i_nota-id_nota.

    "Gerar Impostos Retidos """""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE * INTO @DATA(wa_zsdt0001tetn)
      FROM zsdt0001tetn
     WHERE id_entrada EQ @i_nota-id_entrada.

*    CHECK sy-subrc IS INITIAL.
    IF sy-subrc NE 0. "22/05/2024 ALRS
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_err_te-msgid
                            msgno = zcx_carga=>zcx_err_te-msgno
                            attr1 = CONV #( i_nota-id_entrada )
                            attr2 = CONV #( i_nota-id_entrada ) )
          msgid  = zcx_carga=>zcx_err_te-msgid
          msgno  = zcx_carga=>zcx_err_te-msgno
          msgty  = 'E'
          msgv1  = CONV #( i_nota-id_entrada )
          msgv2  = CONV #( i_nota-id_entrada ).
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_zsdt0001mt)
      FROM zsdt0001mt
     WHERE matnr EQ @me->carga-id_produto.

*    CHECK sy-subrc IS INITIAL.
    IF sy-subrc NE 0. "22/05/2024 ALRS
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_err_te-msgid
                            msgno = zcx_carga=>zcx_err_te-msgno
                            attr1 = CONV #( i_nota-id_entrada )
                            attr2 = CONV #( me->carga-id_produto ) )
          msgid  = zcx_carga=>zcx_err_te-msgid
          msgno  = zcx_carga=>zcx_err_te-msgno
          msgty  = 'E'
          msgv1  = CONV #( i_nota-id_entrada )
          msgv2  = CONV #( me->carga-id_produto ).
    ENDIF.

    CALL FUNCTION 'BAPI_BRANCH_GETDETAIL'
      EXPORTING
        company        = me->carga-id_bukrs
        branch         = me->carga-id_branch
      IMPORTING
        branch_address = wa_branch_address.

    IF wa_branch_address-region IS INITIAL.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_err_regio-msgid
                            msgno = zcx_carga=>zcx_err_regio-msgno
                            attr1 = CONV #( me->carga-id_branch ) )
          msgid  = zcx_carga=>zcx_err_regio-msgid
          msgno  = zcx_carga=>zcx_err_regio-msgno
          msgty  = 'E'
          msgv1  = CONV #( me->carga-id_branch ).
    ENDIF.

**&============Inicio BUG SOLTO 145463 / IR184715 / AOENNING.
    "Estrutura do json body da api.
    CLEAR: wa_zde_zmme0003, lc_retorno.
    wa_zde_zmme0003 = VALUE #( pageindex     = 0
                               pagesize      = 0
                               idtiponota    = wa_zsdt0001tetn-id_tipo_nota
                               uf            = wa_branch_address-region
                               grupocontabil = wa_zsdt0001mt-tp_grupo_ctb ).

    TRY .
        zcl_int_ob_cons_imposto_nf=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = wa_zde_zmme0003 IMPORTING e_integracao = DATA(r_response) ).

        /ui2/cl_json=>deserialize( EXPORTING json = r_response-ds_data_retorno CHANGING data = lc_retorno ).

        IF ( r_response-ds_data_retorno IS INITIAL ) OR ( lc_retorno-data[] IS INITIAL ).
          CLEAR: zvg_message.
          zvg_message = |{ wa_branch_address-region } Tipo Nota: {
                           wa_zsdt0001tetn-id_tipo_nota } Grp.Contabil: {
                           wa_zsdt0001mt-tp_grupo_ctb }|.

          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = VALUE #( msgid = zcx_carga=>zcx_erro_geral-msgid
                                msgno = zcx_carga=>zcx_erro_geral-msgno
                                attr1 = 'Impostos não encontrados no SIGAM para região:'
                                attr2 = zvg_message )
              msgid  = zcx_carga=>zcx_erro_geral-msgid
              msgno  = zcx_carga=>zcx_erro_geral-msgno
              msgty  = 'E'
              msgv1  = CONV #( 'Impostos não encontrados no SIGAM para região:' )
              msgv2  = CONV #( zvg_message ).
        ENDIF.

        LOOP AT lc_retorno-data INTO DATA(ws_data).

          APPEND INITIAL LINE TO it_zsdt0001im ASSIGNING FIELD-SYMBOL(<fs_zsdt0001im>).

          <fs_zsdt0001im>-id_lanc_imposto    = ws_data-idlancimposto.
          <fs_zsdt0001im>-id_tipo_nota       = ws_data-idtiponota.
          <fs_zsdt0001im>-tp_grupo_ctb       = ws_data-tpgrupoctb.
          <fs_zsdt0001im>-regio              = ws_data-regio.
          <fs_zsdt0001im>-dt_inicial         = ws_data-dtini.
          <fs_zsdt0001im>-dt_final           = ws_data-dtfim.
          <fs_zsdt0001im>-lifnr              = ws_data-lifnr.
          <fs_zsdt0001im>-ds_imposto         = ws_data-dsimposto.
          <fs_zsdt0001im>-tp_tributo         = ws_data-tptributo.
          <fs_zsdt0001im>-id_situacao_estado = ws_data-idsituacaoestado.
          <fs_zsdt0001im>-id_situacao_fora   = ws_data-idsituacaofora.
          <fs_zsdt0001im>-cd_imposto         = ws_data-cdimpostos.
          <fs_zsdt0001im>-cd_categoria       = ws_data-cdcategoria.
          <fs_zsdt0001im>-tp_pessoa          = ws_data-tppessoa.
          <fs_zsdt0001im>-cd_lei_fiscal      = ws_data-cdleifiscal.

        ENDLOOP.

      CATCH zcx_integracao.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_erro_geral-msgid
                              msgno = zcx_carga=>zcx_erro_geral-msgno
                              attr1 = 'Não foi possível consultar'
                              attr2 = 'os impostos no Sistema Sigam!'
                              )
            msgid  = zcx_carga=>zcx_erro_geral-msgid
            msgno  = zcx_carga=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Não foi possível consultar' )
            msgv2  = CONV #( 'os impostos no Sistema Sigam!' ).
      CATCH zcx_error.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_erro_geral-msgid
                              msgno = zcx_carga=>zcx_erro_geral-msgno
                              attr1 = 'Não foi possível consultar'
                              attr2 = 'os impostos no Sistema Sigam!'
                              )
            msgid  = zcx_carga=>zcx_erro_geral-msgid
            msgno  = zcx_carga=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Não foi possível consultar' )
            msgv2  = CONV #( 'os impostos no Sistema Sigam!' ).
    ENDTRY.
**&============Fim BUG SOLTO 145463 / IR184715 / AOENNING.

    IF me->carga-dt_movimento IS INITIAL.
      me->carga-dt_movimento = sy-datlo.
    ENDIF.

    "BUG SOLTO 145463 / IR184715 / AOENNING
    DELETE it_zsdt0001im WHERE NOT ( dt_inicial LE me->carga-dt_movimento
                                 AND dt_final   GE me->carga-dt_movimento ).

*    SELECT * INTO TABLE @DATA(it_zsdt0001im)
*      FROM zsdt0001im
*     WHERE id_tipo_nota EQ @wa_zsdt0001tetn-id_tipo_nota
*       AND tp_grupo_ctb EQ @wa_zsdt0001mt-tp_grupo_ctb
*       AND regio        EQ @wa_branch_address-region
*       AND dt_inicial   LE @me->carga-dt_movimento
*       AND dt_final     GE @me->carga-dt_movimento.
    "BUG SOLTO 145463 / IR184715 / AOENNING

*    CHECK sy-subrc IS INITIAL.
    "IF sy-subrc NE 0. "22/05/2024 ALRS
    IF  it_zsdt0001im[] IS INITIAL. "BUG SOLTO 145463 / IR184715 / AOENNING
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_err_te-msgid
                            msgno = zcx_carga=>zcx_err_te-msgno
                            attr1 = CONV #( i_nota-id_entrada )
                            attr2 = CONV #( wa_branch_address-region ) )
          msgid  = zcx_carga=>zcx_err_te-msgid
          msgno  = zcx_carga=>zcx_err_te-msgno
          msgty  = 'E'
          msgv1  = CONV #( i_nota-id_entrada )
          msgv2  = CONV #( wa_branch_address-region ).
    ENDIF.

    zcl_fornecedores=>zif_parceiros~get_instance(
      )->set_parceiro( i_parceiro = i_nota-id_fornecedor
      )->get_regio( IMPORTING e_regio = DATA(e_regio)
      )->get_tipo_parceiro( IMPORTING e_tipo = DATA(e_tipo_pessoa)
      ).

    "Exceção do FUNRUAL
    DATA(lc_fun_exception) = abap_false.

    SELECT SINGLE * INTO @DATA(wa_excecao_fun)
      FROM zsdt0001funex
     WHERE lifnr EQ @i_nota-id_fornecedor
       AND dt_inicio LE @i_nota-dt_emissao
       AND dt_final  GE @i_nota-dt_emissao.

    IF sy-subrc IS INITIAL.
      lc_fun_exception = abap_true.
    ENDIF.

    LOOP AT it_zsdt0001im INTO DATA(wa_zsdt0001im).

      CLEAR: wa_imposto.

*-CS2021000183 - 29.03.2022 - JT - inicio
      CHECK wa_zsdt0001im-cd_lei_fiscal IS INITIAL.
*-CS2021000183 - 29.03.2022 - JT - fim

      "Verifica se tem exceção de fun rural
      IF lc_fun_exception EQ abap_true AND wa_zsdt0001im-tp_tributo EQ 'FUN'.
        CONTINUE.
      ENDIF.

      "Verifica se o imposto deve ser gerado para o tipo do parceiro físico/jurídico
      IF NOT ( ( wa_zsdt0001im-tp_pessoa EQ e_tipo_pessoa ) OR ( wa_zsdt0001im-tp_pessoa EQ space ) ).
        CONTINUE.
      ENDIF.

      IF e_regio EQ wa_branch_address-region.
        DATA(lc_id_situacao) = wa_zsdt0001im-id_situacao_estado.
      ELSE.
        lc_id_situacao = wa_zsdt0001im-id_situacao_fora.
      ENDIF.

      "VERIFICA SE IMPOSTO RETIDO EXISTE NO CADASTRO BP 22/05/2024 ALRS
      IF wa_zsdt0001im-cd_imposto IS NOT INITIAL.
        SELECT COUNT(*)
          FROM lfbw
          WHERE bukrs     = me->carga-id_bukrs
          AND   lifnr     = i_nota-id_fornecedor
          AND   wt_withcd = wa_zsdt0001im-cd_imposto.
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = VALUE #( msgid = zcx_carga=>zcx_err_irf-msgid
                                msgno = zcx_carga=>zcx_err_irf-msgno
                                attr1 = CONV #( wa_zsdt0001im-cd_imposto ) )
              msgid  = zcx_carga=>zcx_err_irf-msgid
              msgno  = zcx_carga=>zcx_err_irf-msgno
              msgty  = 'E'
              msgv1  = CONV #( wa_zsdt0001im-cd_imposto ).
        ENDIF.
      ENDIF.
      "VERIFICA SE IMPOSTO RETIDO EXISTE NO CADASTRO BP

      SELECT SINGLE * INTO @DATA(wa_zsdt0001st)
        FROM zsdt0001st
       WHERE id_situacao_trib EQ @lc_id_situacao.

      CHECK wa_zsdt0001st-ck_subst_tributaria EQ abap_false.

      wa_imposto-id_carga         = i_nota-id_carga.
      wa_imposto-id_nota          = i_nota-id_nota.
      wa_imposto-id_lanc_imposto  = wa_zsdt0001im-id_lanc_imposto.
      wa_imposto-tp_tributo       = wa_zsdt0001im-tp_tributo.
      wa_imposto-cd_imposto       = wa_zsdt0001im-cd_imposto.
      wa_imposto-cd_categoria     = wa_zsdt0001im-cd_categoria.
      wa_imposto-id_tipo_nota     = wa_zsdt0001im-id_tipo_nota.
      wa_imposto-tp_grupo_ctb     = wa_zsdt0001im-tp_grupo_ctb.
      wa_imposto-regio            = wa_zsdt0001im-regio.
      wa_imposto-lifnr            = i_nota-id_fornecedor.
      wa_imposto-regio_forn       = e_regio.
      wa_imposto-tp_pessoa_forn   = e_tipo_pessoa.
      wa_imposto-id_situacao_trib = lc_id_situacao.
      wa_imposto-matnr            = me->carga-id_produto.

      wa_imposto-nm_aliquota      = 0.
      wa_imposto-nm_quantidade    = 0.
      wa_imposto-base_imposto     = 0.
      wa_imposto-vlr_imposto      = 0.
      wa_imposto-vlr_isento       = 0.
      wa_imposto-outros_icms      = 0.
      wa_imposto-percent_reducao  = 0.

      IF wa_zsdt0001im-tp_tributo EQ 'FET' OR wa_zsdt0001im-tp_tributo EQ 'FAC'.
        wa_imposto-dt_base = me->carga-dt_movimento.
      ELSE.
        wa_imposto-dt_base = i_nota-dt_emissao.
      ENDIF.

      "Busca Aliquota do Imposto
      IF wa_zsdt0001st-tp_tributacao EQ 'T' OR wa_zsdt0001st-tp_tributacao EQ 'R'.
        SELECT * INTO @DATA(wa_zsdt0001imtx)
          FROM zsdt0001imtx
         WHERE id_lanc_imposto EQ @wa_zsdt0001im-id_lanc_imposto
           AND dt_inicial      LE @wa_imposto-dt_base
           AND dt_final        GE @wa_imposto-dt_base.
          IF wa_zsdt0001imtx-tp_pessoa EQ e_tipo_pessoa OR wa_zsdt0001imtx-tp_pessoa EQ space.
            DATA(lc_taxavlr) = wa_zsdt0001imtx-nm_aliquota.
          ENDIF.
        ENDSELECT.
      ENDIF.

      CASE wa_zsdt0001st-tp_tributacao.
        WHEN 'T'.
          IF wa_zsdt0001st-nm_percentual IS NOT INITIAL.

            IF wa_zsdt0001im-tp_tributo EQ 'FET' OR
               wa_zsdt0001im-tp_tributo EQ 'FAC' OR
               wa_zsdt0001im-tp_tributo EQ 'FAB' OR
               wa_zsdt0001im-tp_tributo EQ 'IMA'.
              wa_imposto-base_imposto = i_nota-nr_quantidade.

              SELECT * INTO @DATA(wa_zsdt0001imqt)
                FROM zsdt0001imqt
               WHERE id_lanc_imposto EQ @wa_zsdt0001im-id_lanc_imposto
                 AND dt_inicial      LE @wa_imposto-dt_base
                 AND dt_final        GE @wa_imposto-dt_base
                 AND matnr           EQ @wa_imposto-matnr.
                wa_imposto-nm_aliquota   = wa_zsdt0001imqt-nm_aliquota.
                wa_imposto-nm_quantidade = wa_zsdt0001imqt-nm_quantidade.
                wa_imposto-vlr_imposto   = ( wa_imposto-base_imposto / wa_zsdt0001imqt-nm_quantidade ) * wa_imposto-nm_aliquota.
              ENDSELECT.

            ELSE.
              CLEAR vg_netwrt.
              IF i_nota-docnum_np IS NOT INITIAL.
                SELECT SINGLE netwrt
                  INTO vg_netwrt
                  FROM j_1bnflin
                  WHERE docnum = i_nota-docnum_np.
              ENDIF.
              IF vg_netwrt GT 0.
                wa_imposto-base_imposto = vg_netwrt.
              ELSE.
                wa_imposto-base_imposto = i_nota-nr_valor.
              ENDIF.

              IF lc_taxavlr IS NOT INITIAL.
                wa_imposto-nm_aliquota = lc_taxavlr.
              ELSE.
                wa_imposto-nm_aliquota = wa_zsdt0001st-nm_percentual.
              ENDIF.
              wa_imposto-vlr_imposto = wa_imposto-base_imposto * ( wa_imposto-nm_aliquota / 100 ).
            ENDIF.
          ENDIF.

        WHEN 'I'.
          wa_imposto-vlr_isento = i_nota-nr_valor.
        WHEN 'O'.
          wa_imposto-outros_icms = i_nota-nr_valor.
        WHEN 'S'.
          wa_imposto-outros_icms = i_nota-nr_valor.
        WHEN 'R'.

          wa_imposto-percent_reducao = wa_zsdt0001st-nm_percentual_red.

          IF lc_taxavlr IS NOT INITIAL.
            wa_imposto-nm_aliquota = lc_taxavlr.
          ELSE.
            wa_imposto-nm_aliquota = wa_zsdt0001st-nm_percentual.
          ENDIF.

          wa_imposto-base_imposto = i_nota-nr_valor - ( i_nota-nr_valor * ( wa_imposto-percent_reducao / 100 ) ).
          wa_imposto-vlr_imposto  = wa_imposto-base_imposto * ( wa_imposto-nm_aliquota / 100 ).
          wa_imposto-vlr_isento   = i_nota-nr_valor - wa_imposto-base_imposto.

      ENDCASE.

      APPEND wa_imposto TO me->documento_fiscal_imp_ret.

    ENDLOOP.


**&============Inicio BUG SOLTO 145463 / IR184715 / AOENNING.
**Valida imposto cadastrado no BP x Impostos configurado enviado pelo SIGAM para pessoal fisica.

    DATA: it_zmmt0182 TYPE TABLE OF zmmt0182.

    SELECT *
       FROM lfbw INTO TABLE @DATA(it_lfbw)
      WHERE bukrs  = @me->carga-id_bukrs
        AND lifnr  = @i_nota-id_fornecedor.

    CLEAR: it_zmmt0182[].

    SELECT *
      FROM zmmt0182 INTO TABLE it_zmmt0182
     WHERE regio = e_regio
      AND  tipo_pessoa EQ e_tipo_pessoa .

    IF it_zmmt0182[] IS INITIAL. "Se não achou parametros especifico para o estado do fornecedor, devemos buscar os parametros que servem para todos os demais estados
      SELECT *
        FROM zmmt0182 INTO TABLE it_zmmt0182
        WHERE regio = space
        AND  tipo_pessoa EQ e_tipo_pessoa.
    ENDIF.

    IF it_zmmt0182[] IS INITIAL AND e_tipo_pessoa = 'F'.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_erro_geral-msgid
                            msgno = zcx_carga=>zcx_erro_geral-msgno
                            attr1 = |Imposto retido para a UF { e_regio }|
                            attr2 = |não cadastrado na transação ZMM0227| )
          msgid  = zcx_carga=>zcx_erro_geral-msgid
          msgno  = zcx_carga=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = |Imposto retido para a UF { e_regio }|
          msgv2  = |não cadastrado na transação ZMM0227|.
    ENDIF.

    LOOP AT it_zmmt0182 INTO DATA(wa_zmmt0182).

      READ TABLE it_lfbw INTO DATA(wa_lfbw) WITH KEY witht     = wa_zmmt0182-witht
                                                     wt_withcd = wa_zmmt0182-wt_withcd
                                                     lifnr     = i_nota-id_fornecedor.

      IF sy-subrc NE 0.
        "Verifica se o imposto cadastrado é FUNRURAL e se esta na excecao, caso estiver não validar
        IF lc_fun_exception NE abap_true.
          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = VALUE #( msgid = zcx_carga=>zcx_erro_geral-msgid
                                msgno = zcx_carga=>zcx_erro_geral-msgno
                                attr1 = |Imposto retido {  wa_zmmt0182-witht }->{  wa_zmmt0182-wt_withcd } |
                                attr2 = |não cadastrado no BP.| )
              msgid  = zcx_carga=>zcx_erro_geral-msgid
              msgno  = zcx_carga=>zcx_erro_geral-msgno
              msgty  = 'E'
              msgv1  = |Imposto retido {  wa_zmmt0182-witht }->{  wa_zmmt0182-wt_withcd } |
              msgv2  = |não cadastrado no BP|.
        ENDIF.
      ENDIF.

    ENDLOOP.
**&============Fim BUG SOLTO 145463 / IR184715 / AOENNING.


  ENDMETHOD.


  METHOD ZIF_CARGA~SET_ID_CARGA.

    R_CARGA = ME.

    ME->CARGA-ID_CARGA         = I_ID_CARGA.
    ME->CLASSIFICACAO-ID_CARGA = I_ID_CARGA.

    ME->SET_ENQUEUE( I_CARGA = ME->CARGA-ID_CARGA ).

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_ID_CLASSIFICACAO.

    R_CARGA = ME.

    ME->CLASSIFICACAO-ID_CLASSIFICACAO = I_ID_CLASSIFICACAO.
    ME->CARGA-ID_CLASSIFICACAO         = I_ID_CLASSIFICACAO.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_ID_SOLIC_MANUT.

    R_CARGA = ME.

    ME->ZIF_CARGA~SOLICITACAO_MANUTENCAO-ID_SOLICITACAO = I_ID_SOLICITACAO.

  ENDMETHOD.


  method ZIF_CARGA~SET_INFO_FRETE_WITHOUT_OC.

    R_CARGA = ME.

    CHECK ME->CARGA-ID_ORDEM IS INITIAL.

    CHECK ME->CARGA-TP_STATUS EQ ZIF_CARGA=>ST_STATUS_ABERTO.

    ME->CARGA-ID_PROPRIETARIO  = I_ID_PROPRIETARIO.
    ME->CARGA-DS_PLACA_TRATOR  = I_DS_PLACA_TRATOR.
    ME->CARGA-DS_PLACA_REBOQ_1 = I_DS_PLACA_REBOQ_1.
    ME->CARGA-DS_PLACA_REBOQ_2 = I_DS_PLACA_REBOQ_2.
    ME->CARGA-DS_PLACA_REBOQ_3 = I_DS_PLACA_REBOQ_3.
    ME->CARGA-ID_MOTORISTA     = I_ID_MOTORISTA.
    ME->CARGA-DS_MOTORISTA     = I_DS_MOTORISTA.

  endmethod.


  METHOD ZIF_CARGA~SET_INFO_FRETE_WITHOUT_OV.

    R_CARGA = ME.

    CHECK ME->CARGA-TP_FRETE EQ ZIF_CARGA=>ST_TP_FRETE_CPT.

    CHECK ME->CARGA-TP_STATUS EQ ZIF_CARGA=>ST_STATUS_ABERTO.

    ME->CARGA-ID_PROPRIETARIO  = I_ID_PROPRIETARIO.
    ME->CARGA-DS_PLACA_TRATOR  = I_DS_PLACA_TRATOR.
    ME->CARGA-DS_PLACA_REBOQ_1 = I_DS_PLACA_REBOQ_1.
    ME->CARGA-DS_PLACA_REBOQ_2 = I_DS_PLACA_REBOQ_2.
    ME->CARGA-DS_PLACA_REBOQ_3 = I_DS_PLACA_REBOQ_3.
    ME->CARGA-ID_MOTORISTA     = I_ID_MOTORISTA.
    ME->CARGA-DS_MOTORISTA     = I_DS_MOTORISTA.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_LIMPA_ROMANEIO_SAIDA.

    DATA: I_TEXTO TYPE STRING.

    DATA: OB_ROMANEIO TYPE REF TO ZCL_ROMANEIO.

    CHECK ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_FALSE.
    CHECK ME->ZIF_CARGA~CK_EXECUTAR_MANUTENCAO_ENTRADA EQ ABAP_FALSE.

    ME->GET_ROMANEIO_SAIDA( EXPORTING I_ID_CARGA  = ME->CARGA-ID_CARGA IMPORTING E_ROMANEIOS = DATA(ROMANEIOS) ).

    LOOP AT ROMANEIOS INTO DATA(WA_ROMANEIO).
      IF WA_ROMANEIO-ST_PROC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ROMANEIO_SAIDA_DOC-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_ROMANEIO_SAIDA_DOC-MSGNO )
            MSGTY  = 'E'
            MSGID  = ZCX_CARGA=>ZCX_ROMANEIO_SAIDA_DOC-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_ROMANEIO_SAIDA_DOC-MSGNO.
      ENDIF.
    ENDLOOP.

    LOOP AT ME->ZIF_CARGA~ORDEM_VENDA ASSIGNING FIELD-SYMBOL(<ORDEM_VENDA>).
      CLEAR: <ORDEM_VENDA>-CH_REFERENCIA_SAI, <ORDEM_VENDA>-NR_ROMANEIO_SAI.
    ENDLOOP.

    TRY .

        LOOP AT ROMANEIOS INTO WA_ROMANEIO.
          CREATE OBJECT OB_ROMANEIO.
          OB_ROMANEIO->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = WA_ROMANEIO-CH_REFERENCIA ).
          OB_ROMANEIO->ZIF_CADASTRO~EXCLUIR_REGISTRO( ).
          CLEAR OB_ROMANEIO.
          ME->CK_ALTEROU = ABAP_TRUE.
        ENDLOOP.

      CATCH ZCX_CADASTRO INTO DATA(EX_CADASTRO).

        MESSAGE ID EX_CADASTRO->MSGID TYPE 'S'
         NUMBER EX_CADASTRO->MSGNO
           WITH EX_CADASTRO->MSGV1 EX_CADASTRO->MSGV2 EX_CADASTRO->MSGV3 EX_CADASTRO->MSGV4
           INTO I_TEXTO.

        ME->GERA_ERRO_GERAL( I_TEXTO = I_TEXTO ).

    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_LOCAL_ENTREGA.

    R_CARGA = ME.

    SELECT * INTO TABLE @DATA(IT_LOCAIS)
      FROM ZSDT0001LE_VW
     WHERE ID_LOCAL_ENTREGA EQ @I_ID_LOCAL_ENTREGA
       AND ID_BRANCH   EQ @ME->CARGA-ID_BRANCH.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_LE_SEM_PARAM_LC_NEGOCIO-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_LE_SEM_PARAM_LC_NEGOCIO-MSGNO
                            ATTR1 = CONV #( ME->CARGA-ID_BRANCH ) )
          MSGID  = ZCX_CARGA=>ZCX_LE_SEM_PARAM_LC_NEGOCIO-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_LE_SEM_PARAM_LC_NEGOCIO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->CARGA-ID_BRANCH ).
    ENDIF.

    READ TABLE IT_LOCAIS WITH KEY ID_MATERIAL = ME->CARGA-ID_PRODUTO TRANSPORTING NO FIELDS.
    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_LE_SEM_PARAM_MATERIAL-MSGID
                            MSGNO = ZCX_CARGA=>ZCX_LE_SEM_PARAM_MATERIAL-MSGNO
                            ATTR1 = CONV #( ME->CARGA-ID_PRODUTO ) )
          MSGID  = ZCX_CARGA=>ZCX_LE_SEM_PARAM_MATERIAL-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_LE_SEM_PARAM_MATERIAL-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->CARGA-ID_PRODUTO ).
    ENDIF.

    ME->CARGA-ID_LOCAL_ENTREGA = I_ID_LOCAL_ENTREGA.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_LOGS_ALTERACAO.

    DATA: LC_DT_REGISTRO       TYPE ZDE_DT_REGISTRO,
          LC_HR_REGISTRO       TYPE ZDE_HR_REGISTRO,
          LC_US_REGISTRO       TYPE ZDE_US_REGISTRO,
          LC_TP_REGISTRO       TYPE ZDE_TP_REGISTRO,
          WA_LOG_ZSDT0001CG    TYPE ZSDT0001CGLG,
          IT_LOG_ZSDT0001NT    TYPE TABLE OF ZSDT0001NTLG,
          WA_LOG_ZSDT0001NT    TYPE ZSDT0001NTLG,
          IT_LOG_ZSDT0001TK    TYPE TABLE OF ZSDT0001TKLG,
          WA_LOG_ZSDT0001TK    TYPE ZSDT0001TKLG,
          IT_LOG_ZSDT0001CL    TYPE TABLE OF ZSDT0001CLLG,
          WA_LOG_ZSDT0001CL    TYPE ZSDT0001CLLG,
          IT_LOG_ZSDT0001RS    TYPE TABLE OF ZSDT0001RSLG,
          WA_LOG_ZSDT0001RS    TYPE ZSDT0001RSLG,
          IT_LOG_ZSDT0001OV    TYPE TABLE OF ZSDT0001OVLG,
          WA_LOG_ZSDT0001OV    TYPE ZSDT0001OVLG,
          IT_LOG_ZSDT0001EK    TYPE TABLE OF ZSDT0001EKLG,
          WA_LOG_ZSDT0001EK    TYPE ZSDT0001EKLG,
          IT_LOG_ZSDT0001RS_03 TYPE TABLE OF ZSDT0001RSLG_03,
          WA_LOG_ZSDT0001RS_03 TYPE ZSDT0001RSLG_03,
          IT_LOG_ZSDT0001FD    TYPE TABLE OF ZSDT0001FDLG,
          WA_LOG_ZSDT0001FD    TYPE ZSDT0001FDLG.

    LC_DT_REGISTRO = SY-DATLO.
    LC_HR_REGISTRO = SY-TIMLO.
    LC_US_REGISTRO = SY-UNAME.

    R_CARGA = ME.

    CLEAR: WA_LOG_ZSDT0001CG,
           WA_LOG_ZSDT0001NT,
           WA_LOG_ZSDT0001CL,
           WA_LOG_ZSDT0001RS,
           WA_LOG_ZSDT0001OV,
           WA_LOG_ZSDT0001EK.

    SELECT SINGLE * INTO @DATA(WA_ZSDT0001CG)
      FROM ZSDT0001CG
     WHERE ID_CARGA EQ @ME->CARGA-ID_CARGA.

    SELECT * INTO TABLE @DATA(IT_ZSDT0001NT)
      FROM ZSDT0001NT
     WHERE ID_CARGA EQ @ME->CARGA-ID_CARGA.

    SELECT * INTO TABLE @DATA(IT_ZSDT0001TK)
      FROM ZSDT0001TK
     WHERE ID_CARGA EQ @ME->CARGA-ID_CARGA.

    "Todas as Classificações
    SELECT * INTO TABLE @DATA(IT_ZSDT0001CL)
      FROM ZSDT0001CL
     WHERE ID_CARGA EQ @ME->CARGA-ID_CARGA.

    SELECT * INTO TABLE @DATA(IT_ZSDT0001RS)
      FROM ZSDT0001RS
     WHERE ID_CARGA EQ @ME->CARGA-ID_CARGA.

    SELECT * INTO TABLE @DATA(IT_ZSDT0001RS_03)
      FROM ZSDT0001RS_03
     WHERE ID_CARGA EQ @ME->CARGA-ID_CARGA.

    SELECT * INTO TABLE @DATA(IT_ZSDT0001OV)
      FROM ZSDT0001OV
     WHERE ID_CARGA EQ @ME->CARGA-ID_CARGA.

    SELECT * INTO TABLE @DATA(IT_ZSDT0001EK)
      FROM ZSDT0001EK
     WHERE ID_CARGA EQ @ME->CARGA-ID_CARGA.

    SELECT * INTO TABLE @DATA(IT_ZSDT0001FD)
      FROM ZSDT0001FD
     WHERE ID_CARGA EQ @ME->CARGA-ID_CARGA.

    IF WA_ZSDT0001CG IS INITIAL.
      LC_TP_REGISTRO = 'I'.
      MOVE-CORRESPONDING ME->CARGA TO WA_LOG_ZSDT0001CG.
      WA_LOG_ZSDT0001CG-DT_REGISTRO = LC_DT_REGISTRO.
      WA_LOG_ZSDT0001CG-HR_REGISTRO = LC_HR_REGISTRO.
      WA_LOG_ZSDT0001CG-US_REGISTRO = LC_US_REGISTRO.
      WA_LOG_ZSDT0001CG-TP_REGISTRO = LC_TP_REGISTRO.
    ELSE.
      LC_TP_REGISTRO = 'A'.
      MOVE-CORRESPONDING ME->CARGA TO WA_LOG_ZSDT0001CG.
      WA_LOG_ZSDT0001CG-DT_REGISTRO = LC_DT_REGISTRO.
      WA_LOG_ZSDT0001CG-HR_REGISTRO = LC_HR_REGISTRO.
      WA_LOG_ZSDT0001CG-US_REGISTRO = LC_US_REGISTRO.
      WA_LOG_ZSDT0001CG-TP_REGISTRO = LC_TP_REGISTRO.
    ENDIF.

    IF IT_ZSDT0001NT[] IS INITIAL.
      LC_TP_REGISTRO = 'I'.
      LOOP AT ME->DOCUMENTO_FISCAL INTO DATA(WA_DOCUMENTO_FISCAL).
        MOVE-CORRESPONDING WA_DOCUMENTO_FISCAL TO WA_LOG_ZSDT0001NT.
        WA_LOG_ZSDT0001NT-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001NT-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001NT-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001NT-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001NT TO IT_LOG_ZSDT0001NT.
      ENDLOOP.
    ELSE.

      "Verifica Exclusão
      LOOP AT IT_ZSDT0001NT INTO DATA(WA_ZSDT0001NT).
        READ TABLE ME->DOCUMENTO_FISCAL WITH KEY ID_CARGA = WA_ZSDT0001NT-ID_CARGA ID_NOTA = WA_ZSDT0001NT-ID_NOTA INTO WA_DOCUMENTO_FISCAL.
        IF SY-SUBRC IS NOT INITIAL.
          LC_TP_REGISTRO = 'E'.
          MOVE-CORRESPONDING WA_ZSDT0001NT TO WA_LOG_ZSDT0001NT.
          WA_LOG_ZSDT0001NT-DT_REGISTRO = LC_DT_REGISTRO.
          WA_LOG_ZSDT0001NT-HR_REGISTRO = LC_HR_REGISTRO.
          WA_LOG_ZSDT0001NT-US_REGISTRO = LC_US_REGISTRO.
          WA_LOG_ZSDT0001NT-TP_REGISTRO = LC_TP_REGISTRO.
          APPEND WA_LOG_ZSDT0001NT TO IT_LOG_ZSDT0001NT.
        ENDIF.
      ENDLOOP.

      "Verifica Alteração
      LOOP AT ME->DOCUMENTO_FISCAL INTO WA_DOCUMENTO_FISCAL.
        READ TABLE IT_ZSDT0001NT INTO WA_ZSDT0001NT WITH KEY ID_CARGA = WA_DOCUMENTO_FISCAL-ID_CARGA ID_NOTA = WA_DOCUMENTO_FISCAL-ID_NOTA.
        IF SY-SUBRC IS INITIAL.
          LC_TP_REGISTRO = 'A'.
        ELSE.
          LC_TP_REGISTRO = 'I'.
        ENDIF.
        MOVE-CORRESPONDING WA_DOCUMENTO_FISCAL TO WA_LOG_ZSDT0001NT.
        WA_LOG_ZSDT0001NT-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001NT-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001NT-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001NT-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001NT TO IT_LOG_ZSDT0001NT.
        "ENDIF.
      ENDLOOP.

    ENDIF.

    IF IT_ZSDT0001TK[] IS INITIAL.
      LC_TP_REGISTRO = 'I'.
      LOOP AT ME->ZIF_CARGA~TAKE_UP INTO DATA(WA_TAKEUP).
        MOVE-CORRESPONDING WA_TAKEUP TO WA_LOG_ZSDT0001TK.
        WA_LOG_ZSDT0001TK-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001TK-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001TK-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001TK-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001TK TO IT_LOG_ZSDT0001TK.
      ENDLOOP.
    ELSE.

      "Verifica Exclusão
      LOOP AT IT_ZSDT0001TK INTO DATA(WA_ZSDT0001TK).
        READ TABLE ME->ZIF_CARGA~TAKE_UP
         WITH KEY ID_CARGA = WA_ZSDT0001TK-ID_CARGA
                  ID_NOTA  = WA_ZSDT0001TK-ID_NOTA
                  ID_TAKEUP = WA_ZSDT0001TK-ID_TAKEUP
                  NU_BLOCO =  WA_ZSDT0001TK-NU_BLOCO
             INTO WA_TAKEUP.
        IF SY-SUBRC IS NOT INITIAL.
          LC_TP_REGISTRO = 'E'.
          MOVE-CORRESPONDING WA_ZSDT0001TK TO WA_LOG_ZSDT0001TK.
          WA_LOG_ZSDT0001TK-DT_REGISTRO = LC_DT_REGISTRO.
          WA_LOG_ZSDT0001TK-HR_REGISTRO = LC_HR_REGISTRO.
          WA_LOG_ZSDT0001TK-US_REGISTRO = LC_US_REGISTRO.
          WA_LOG_ZSDT0001TK-TP_REGISTRO = LC_TP_REGISTRO.
          APPEND WA_LOG_ZSDT0001TK TO IT_LOG_ZSDT0001TK.
        ENDIF.
      ENDLOOP.

      "Verifica Alteração
      LOOP AT ME->ZIF_CARGA~TAKE_UP INTO WA_TAKEUP.
        READ TABLE IT_ZSDT0001TK INTO WA_ZSDT0001TK
        WITH KEY ID_CARGA = WA_TAKEUP-ID_CARGA
                 ID_NOTA = WA_TAKEUP-ID_NOTA
                 ID_TAKEUP = WA_TAKEUP-ID_TAKEUP
                 NU_BLOCO = WA_TAKEUP-NU_BLOCO.

        IF SY-SUBRC IS INITIAL.
          LC_TP_REGISTRO = 'A'.
        ELSE.
          LC_TP_REGISTRO = 'I'.
        ENDIF.

        MOVE-CORRESPONDING WA_TAKEUP TO WA_LOG_ZSDT0001TK.
        WA_LOG_ZSDT0001TK-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001TK-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001TK-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001TK-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001TK TO IT_LOG_ZSDT0001TK.
      ENDLOOP.

    ENDIF.

    IF IT_ZSDT0001CL[] IS INITIAL.
      LC_TP_REGISTRO = 'I'.

      LOOP AT ME->CLASSIFICACAO_NOTAS INTO DATA(WA_CLASSIFICACAO).
        MOVE-CORRESPONDING WA_CLASSIFICACAO TO WA_LOG_ZSDT0001CL.
        WA_LOG_ZSDT0001CL-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001CL-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001CL-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001CL-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001CL TO IT_LOG_ZSDT0001CL.
      ENDLOOP.

      MOVE-CORRESPONDING ME->CLASSIFICACAO TO WA_LOG_ZSDT0001CL.
      WA_LOG_ZSDT0001CL-DT_REGISTRO = LC_DT_REGISTRO.
      WA_LOG_ZSDT0001CL-HR_REGISTRO = LC_HR_REGISTRO.
      WA_LOG_ZSDT0001CL-US_REGISTRO = LC_US_REGISTRO.
      WA_LOG_ZSDT0001CL-TP_REGISTRO = LC_TP_REGISTRO.
      APPEND WA_LOG_ZSDT0001CL TO IT_LOG_ZSDT0001CL.

    ELSE.

      "Classificação da Carga
      "Verifica Alteração
      LOOP AT IT_ZSDT0001CL INTO DATA(WA_ZSDT0001CL) WHERE ID_CLASSIFICACAO EQ ME->CLASSIFICACAO-ID_CLASSIFICACAO.
        LC_TP_REGISTRO = 'A'.
        MOVE-CORRESPONDING ME->CLASSIFICACAO TO WA_LOG_ZSDT0001CL.
        WA_LOG_ZSDT0001CL-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001CL-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001CL-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001CL-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001CL TO IT_LOG_ZSDT0001CL.
      ENDLOOP.

      "Classificação da Nota
      "Verifica Exclusão
      LOOP AT IT_ZSDT0001CL INTO WA_ZSDT0001CL WHERE ID_CLASSIFICACAO NE ME->CLASSIFICACAO-ID_CLASSIFICACAO.
        READ TABLE ME->CLASSIFICACAO_NOTAS WITH KEY ID_CLASSIFICACAO = WA_ZSDT0001CL-ID_CLASSIFICACAO INTO DATA(WA_CLASSIFICACAO_NOTAS).
        IF SY-SUBRC IS NOT INITIAL.
          "Registro Excluido
          LC_TP_REGISTRO = 'E'.
          MOVE-CORRESPONDING WA_ZSDT0001CL TO WA_LOG_ZSDT0001NT.
          WA_LOG_ZSDT0001NT-DT_REGISTRO = LC_DT_REGISTRO.
          WA_LOG_ZSDT0001NT-HR_REGISTRO = LC_HR_REGISTRO.
          WA_LOG_ZSDT0001NT-US_REGISTRO = LC_US_REGISTRO.
          WA_LOG_ZSDT0001NT-TP_REGISTRO = LC_TP_REGISTRO.
          APPEND WA_LOG_ZSDT0001NT TO IT_LOG_ZSDT0001NT.
        ENDIF.
      ENDLOOP.

      "Classificação da Nota
      "Verifica Alteração
      LOOP AT ME->CLASSIFICACAO_NOTAS INTO WA_CLASSIFICACAO_NOTAS.
        READ TABLE IT_ZSDT0001CL INTO WA_ZSDT0001CL WITH KEY ID_CLASSIFICACAO = WA_CLASSIFICACAO_NOTAS-ID_CLASSIFICACAO.
        IF SY-SUBRC IS INITIAL.
          LC_TP_REGISTRO = 'A'.
        ELSE.
          LC_TP_REGISTRO = 'I'.
        ENDIF.
        MOVE-CORRESPONDING WA_CLASSIFICACAO_NOTAS TO WA_LOG_ZSDT0001CL.
        WA_LOG_ZSDT0001CL-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001CL-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001CL-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001CL-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001CL TO IT_LOG_ZSDT0001CL.
      ENDLOOP.

    ENDIF.

    IF IT_ZSDT0001RS[] IS INITIAL.
      LC_TP_REGISTRO = 'I'.
      LOOP AT ME->RESULTADO INTO DATA(WA_RESULTADO).
        MOVE-CORRESPONDING WA_RESULTADO TO WA_LOG_ZSDT0001RS.
        WA_LOG_ZSDT0001RS-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001RS-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001RS-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001RS-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001RS TO IT_LOG_ZSDT0001RS.
      ENDLOOP.
    ELSE.

      "Verifica Exclusão
      LOOP AT IT_ZSDT0001RS INTO DATA(WA_ZSDT0001RS).
        READ TABLE ME->RESULTADO WITH KEY ID_CARGA = WA_ZSDT0001RS-ID_CARGA ID_CLASSIFICACAO  = WA_ZSDT0001RS-ID_CLASSIFICACAO TP_CARACTERISTICA = WA_ZSDT0001RS-TP_CARACTERISTICA INTO WA_RESULTADO.
        IF SY-SUBRC IS NOT INITIAL.
          LC_TP_REGISTRO = 'E'.
          MOVE-CORRESPONDING WA_ZSDT0001RS TO WA_LOG_ZSDT0001RS.
          WA_LOG_ZSDT0001RS-DT_REGISTRO = LC_DT_REGISTRO.
          WA_LOG_ZSDT0001RS-HR_REGISTRO = LC_HR_REGISTRO.
          WA_LOG_ZSDT0001RS-US_REGISTRO = LC_US_REGISTRO.
          WA_LOG_ZSDT0001RS-TP_REGISTRO = LC_TP_REGISTRO.
          APPEND WA_LOG_ZSDT0001RS TO IT_LOG_ZSDT0001RS.
        ENDIF.
      ENDLOOP.

      "Verifica Alteração
      LOOP AT ME->RESULTADO INTO WA_RESULTADO.
        READ TABLE IT_ZSDT0001RS WITH KEY ID_CARGA = WA_RESULTADO-ID_CARGA ID_CLASSIFICACAO = WA_RESULTADO-ID_CLASSIFICACAO TP_CARACTERISTICA = WA_RESULTADO-TP_CARACTERISTICA INTO WA_ZSDT0001RS.
        IF SY-SUBRC IS INITIAL.
          LC_TP_REGISTRO = 'A'.
        ELSE.
          LC_TP_REGISTRO = 'I'.
        ENDIF.
        MOVE-CORRESPONDING WA_RESULTADO TO WA_LOG_ZSDT0001RS.
        WA_LOG_ZSDT0001RS-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001RS-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001RS-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001RS-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001RS TO IT_LOG_ZSDT0001RS.
      ENDLOOP.
    ENDIF.

    IF IT_ZSDT0001OV[] IS INITIAL.
      LC_TP_REGISTRO = 'I'.
      LOOP AT ME->ZIF_CARGA~ORDEM_VENDA INTO DATA(WA_ORDEM_VENDA).
        MOVE-CORRESPONDING WA_ORDEM_VENDA TO WA_LOG_ZSDT0001OV.
        WA_LOG_ZSDT0001OV-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001OV-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001OV-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001OV-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001OV TO IT_LOG_ZSDT0001OV.
      ENDLOOP.
    ELSE.

      "Verifica Exclusão
      LOOP AT IT_ZSDT0001OV INTO DATA(WA_ZSDT0001OV).
        READ TABLE ME->ZIF_CARGA~ORDEM_VENDA
        WITH KEY ID_CARGA       = WA_ZSDT0001OV-ID_CARGA
                 NR_ORDEM_VENDA = WA_ZSDT0001OV-NR_ORDEM_VENDA
            INTO WA_ORDEM_VENDA.
        IF SY-SUBRC IS NOT INITIAL.
          LC_TP_REGISTRO = 'E'.
          MOVE-CORRESPONDING WA_ORDEM_VENDA TO WA_LOG_ZSDT0001OV.
          WA_LOG_ZSDT0001OV-DT_REGISTRO = LC_DT_REGISTRO.
          WA_LOG_ZSDT0001OV-HR_REGISTRO = LC_HR_REGISTRO.
          WA_LOG_ZSDT0001OV-US_REGISTRO = LC_US_REGISTRO.
          WA_LOG_ZSDT0001OV-TP_REGISTRO = LC_TP_REGISTRO.
          APPEND WA_LOG_ZSDT0001OV TO IT_LOG_ZSDT0001OV.
        ENDIF.
      ENDLOOP.

      "Verifica Alteração
      LOOP AT ME->ZIF_CARGA~ORDEM_VENDA INTO WA_ORDEM_VENDA.
        READ TABLE IT_ZSDT0001OV INTO WA_ZSDT0001OV
        WITH KEY ID_CARGA       = WA_ORDEM_VENDA-ID_CARGA
                 NR_ORDEM_VENDA = WA_ORDEM_VENDA-NR_ORDEM_VENDA.
        IF SY-SUBRC IS INITIAL.
          LC_TP_REGISTRO = 'A'.
        ELSE.
          LC_TP_REGISTRO = 'I'.
        ENDIF.

        MOVE-CORRESPONDING WA_ORDEM_VENDA TO WA_LOG_ZSDT0001OV.
        WA_LOG_ZSDT0001OV-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001OV-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001OV-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001OV-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001OV TO IT_LOG_ZSDT0001OV.
        "ENDIF.
      ENDLOOP.

    ENDIF.

    IF IT_ZSDT0001EK[] IS INITIAL.
      LC_TP_REGISTRO = 'I'.
      LOOP AT ME->ZIF_CARGA~PEDIDO_COMPRA INTO DATA(WA_PEDIDO_COMPRA).
        MOVE-CORRESPONDING WA_PEDIDO_COMPRA TO WA_LOG_ZSDT0001EK.
        WA_LOG_ZSDT0001EK-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001EK-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001EK-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001EK-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001EK TO IT_LOG_ZSDT0001EK.
      ENDLOOP.
    ELSE.

      "Verifica Exclusão
      LOOP AT IT_ZSDT0001EK INTO DATA(WA_ZSDT0001EK).
        READ TABLE ME->ZIF_CARGA~PEDIDO_COMPRA
        WITH KEY ID_CARGA         = WA_ZSDT0001EK-ID_CARGA
                 NR_PEDIDO_COMPRA = WA_ZSDT0001EK-NR_PEDIDO_COMPRA
            INTO WA_PEDIDO_COMPRA.
        IF SY-SUBRC IS NOT INITIAL.
          LC_TP_REGISTRO = 'E'.
          MOVE-CORRESPONDING WA_PEDIDO_COMPRA TO WA_LOG_ZSDT0001EK.
          WA_LOG_ZSDT0001EK-DT_REGISTRO = LC_DT_REGISTRO.
          WA_LOG_ZSDT0001EK-HR_REGISTRO = LC_HR_REGISTRO.
          WA_LOG_ZSDT0001EK-US_REGISTRO = LC_US_REGISTRO.
          WA_LOG_ZSDT0001EK-TP_REGISTRO = LC_TP_REGISTRO.
          APPEND WA_LOG_ZSDT0001EK TO IT_LOG_ZSDT0001EK.
        ENDIF.
      ENDLOOP.

      "Verifica Alteração
      LOOP AT ME->ZIF_CARGA~PEDIDO_COMPRA INTO WA_PEDIDO_COMPRA.
        READ TABLE IT_ZSDT0001EK INTO WA_ZSDT0001EK
        WITH KEY ID_CARGA         = WA_PEDIDO_COMPRA-ID_CARGA
                 NR_PEDIDO_COMPRA = WA_PEDIDO_COMPRA-NR_PEDIDO_COMPRA.
        IF SY-SUBRC IS INITIAL.
          LC_TP_REGISTRO = 'A'.
        ELSE.
          LC_TP_REGISTRO = 'I'.
        ENDIF.

        MOVE-CORRESPONDING WA_PEDIDO_COMPRA TO WA_LOG_ZSDT0001EK.
        WA_LOG_ZSDT0001EK-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001EK-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001EK-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001EK-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001EK TO IT_LOG_ZSDT0001EK.
      ENDLOOP.

    ENDIF.

    IF IT_ZSDT0001RS_03[] IS INITIAL.
      LC_TP_REGISTRO = 'I'.
      LOOP AT ME->ZIF_CARGA~RESULTADO_AVARIADO INTO DATA(WA_RESULTADO_AVARIADO).
        MOVE-CORRESPONDING WA_RESULTADO_AVARIADO TO WA_LOG_ZSDT0001RS_03.
        WA_LOG_ZSDT0001RS_03-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001RS_03-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001RS_03-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001RS_03-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001RS_03 TO IT_LOG_ZSDT0001RS_03.
      ENDLOOP.
    ELSE.

      "Verifica Exclusão
      LOOP AT IT_ZSDT0001RS_03 INTO DATA(WA_ZSDT0001RS_03).
        READ TABLE ME->ZIF_CARGA~RESULTADO_AVARIADO

        WITH KEY ID_CARGA              = WA_ZSDT0001RS_03-ID_CARGA
                 ID_CLASSIFICACAO      = WA_ZSDT0001RS_03-ID_CLASSIFICACAO
                 TP_SUB_CARAC_AVARIADO = WA_ZSDT0001RS_03-TP_SUB_CARAC_AVARIADO
            INTO WA_RESULTADO_AVARIADO.

        IF SY-SUBRC IS NOT INITIAL.
          LC_TP_REGISTRO = 'E'.
          MOVE-CORRESPONDING WA_ZSDT0001RS_03 TO WA_LOG_ZSDT0001RS_03.
          WA_LOG_ZSDT0001RS_03-DT_REGISTRO = LC_DT_REGISTRO.
          WA_LOG_ZSDT0001RS_03-HR_REGISTRO = LC_HR_REGISTRO.
          WA_LOG_ZSDT0001RS_03-US_REGISTRO = LC_US_REGISTRO.
          WA_LOG_ZSDT0001RS_03-TP_REGISTRO = LC_TP_REGISTRO.
          APPEND WA_LOG_ZSDT0001RS_03 TO IT_LOG_ZSDT0001RS_03.
        ENDIF.
      ENDLOOP.

      "Verifica Alteração
      LOOP AT ME->ZIF_CARGA~RESULTADO_AVARIADO INTO WA_RESULTADO_AVARIADO.
        READ TABLE IT_ZSDT0001RS_03
        WITH KEY ID_CARGA = WA_RESULTADO-ID_CARGA
                 ID_CLASSIFICACAO = WA_RESULTADO-ID_CLASSIFICACAO
                 TP_SUB_CARAC_AVARIADO = WA_RESULTADO_AVARIADO-TP_SUB_CARAC_AVARIADO
            INTO WA_ZSDT0001RS_03.

        IF SY-SUBRC IS INITIAL.
          LC_TP_REGISTRO = 'A'.
        ELSE.
          LC_TP_REGISTRO = 'I'.
        ENDIF.
        MOVE-CORRESPONDING WA_RESULTADO_AVARIADO TO WA_LOG_ZSDT0001RS_03.
        WA_LOG_ZSDT0001RS_03-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001RS_03-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001RS_03-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001RS_03-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001RS_03 TO IT_LOG_ZSDT0001RS_03.
      ENDLOOP.
    ENDIF.

    IF IT_ZSDT0001FD[] IS INITIAL.
      LC_TP_REGISTRO = 'I'.
      LOOP AT ME->ZIF_CARGA~BLOCOS INTO DATA(WA_BLOCOS).
        MOVE-CORRESPONDING WA_BLOCOS TO WA_LOG_ZSDT0001FD.
        WA_LOG_ZSDT0001FD-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001FD-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001FD-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001FD-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001FD TO IT_LOG_ZSDT0001FD.
      ENDLOOP.
    ELSE.

      "Verifica Exclusão
      LOOP AT IT_ZSDT0001FD INTO DATA(WA_ZSDT0001FD).
        READ TABLE ME->ZIF_CARGA~BLOCOS
        WITH KEY ID_CARGA = WA_ZSDT0001FD-ID_CARGA
                 NR_ORDEM_VENDA = WA_ZSDT0001FD-NR_ORDEM_VENDA
                 NR_PEDIDO_COMPRA = WA_ZSDT0001FD-NR_PEDIDO_COMPRA
                 ZSEQ_INST = WA_ZSDT0001FD-ZSEQ_INST
                 OBJEK = WA_ZSDT0001FD-OBJEK
                 OBJECTTABLE = WA_ZSDT0001FD-OBJECTTABLE
            INTO WA_BLOCOS.
        IF SY-SUBRC IS NOT INITIAL.
          LC_TP_REGISTRO = 'E'.
          MOVE-CORRESPONDING WA_BLOCOS TO WA_LOG_ZSDT0001FD.
          WA_LOG_ZSDT0001FD-DT_REGISTRO = LC_DT_REGISTRO.
          WA_LOG_ZSDT0001FD-HR_REGISTRO = LC_HR_REGISTRO.
          WA_LOG_ZSDT0001FD-US_REGISTRO = LC_US_REGISTRO.
          WA_LOG_ZSDT0001FD-TP_REGISTRO = LC_TP_REGISTRO.
          APPEND WA_LOG_ZSDT0001FD TO IT_LOG_ZSDT0001FD.
        ENDIF.
      ENDLOOP.

      "Verifica Alteração
      LOOP AT ME->ZIF_CARGA~BLOCOS INTO WA_BLOCOS.
        READ TABLE IT_ZSDT0001FD INTO WA_ZSDT0001FD
        WITH KEY ID_CARGA = WA_BLOCOS-ID_CARGA
                 NR_ORDEM_VENDA = WA_BLOCOS-NR_ORDEM_VENDA
                 NR_PEDIDO_COMPRA = WA_BLOCOS-NR_PEDIDO_COMPRA
                 ZSEQ_INST = WA_BLOCOS-ZSEQ_INST
                 OBJEK = WA_BLOCOS-OBJEK
                 OBJECTTABLE = WA_BLOCOS-OBJECTTABLE.
        IF SY-SUBRC IS INITIAL.
          LC_TP_REGISTRO = 'A'.
        ELSE.
          LC_TP_REGISTRO = 'I'.
        ENDIF.
        MOVE-CORRESPONDING WA_BLOCOS TO WA_LOG_ZSDT0001FD.
        WA_LOG_ZSDT0001FD-DT_REGISTRO = LC_DT_REGISTRO.
        WA_LOG_ZSDT0001FD-HR_REGISTRO = LC_HR_REGISTRO.
        WA_LOG_ZSDT0001FD-US_REGISTRO = LC_US_REGISTRO.
        WA_LOG_ZSDT0001FD-TP_REGISTRO = LC_TP_REGISTRO.
        APPEND WA_LOG_ZSDT0001FD TO IT_LOG_ZSDT0001FD.
        "ENDIF.
      ENDLOOP.

    ENDIF.

    "Tabelas de Log
    MODIFY ZSDT0001CGLG    FROM WA_LOG_ZSDT0001CG.
    MODIFY ZSDT0001NTLG    FROM TABLE IT_LOG_ZSDT0001NT.
    MODIFY ZSDT0001CLLG    FROM TABLE IT_LOG_ZSDT0001CL.
    MODIFY ZSDT0001RSLG    FROM TABLE IT_LOG_ZSDT0001RS.
    MODIFY ZSDT0001RSLG_03 FROM TABLE IT_LOG_ZSDT0001RS_03.
    MODIFY ZSDT0001OVLG    FROM TABLE IT_LOG_ZSDT0001OV.
    MODIFY ZSDT0001EKLG    FROM TABLE IT_LOG_ZSDT0001EK.
    MODIFY ZSDT0001FDLG    FROM TABLE IT_LOG_ZSDT0001FD.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_NOTA_ENTRADA_PROPRIA.

    DATA: ENTRADA_CARGA TYPE REF TO ZIF_CARGA.

    SELECT SINGLE * INTO @DATA(WA_ZSDT0001NT)
      FROM ZSDT0001NT
     WHERE DOCNUM EQ @I_DOCNUM.

    CHECK SY-SUBRC IS INITIAL.

    ENTRADA_CARGA =
    ZCL_FACTORY_CARGA=>ZIF_FACTORY_CARGA~GET_INSTANCE(
      )->SET_FACTORY_OBJETO_ID( I_ID_CARGA = WA_ZSDT0001NT-ID_CARGA
      )->GET_FACTORY_OBJETO(
      ).

    ENTRADA_CARGA->AT_NAO_GERAR_BLOQUEIOS = ABAP_TRUE.

    ENTRADA_CARGA->SET_REGISTRO( I_ID_CARGA = WA_ZSDT0001NT-ID_CARGA ).

    "Marcar Como Conferido
    "ENTRADA_CARGA->CARGA-TP_STATUS = ZIF_CARGA=>ST_STATUS_CONFERIDO.

    ENTRADA_CARGA->SET_PROCESSAR_ENTRADA( I_SOMENTE_ATUALIZAR = ABAP_TRUE
      )->SET_GERAR_ROMANEIO_SAIDA(
      )->SEND_CARGA_TO_OPUS(
      )->GRAVAR_REGISTRO(
      ).

    ENTRADA_CARGA->FREE( ).

    CLEAR: ENTRADA_CARGA.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_OBSERVACAO_NOTA.

    R_CARGA = ME.

    CASE ME->CARGA-TP_STATUS.
      WHEN ZIF_CARGA=>ST_STATUS_ABERTO.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_CARGA_ABERTA-MSGID MSGNO = ZCX_CARGA=>ZCX_CARGA_ABERTA-MSGNO )
            MSGTY  = 'E'
            MSGID  = ZCX_CARGA=>ZCX_CARGA_ABERTA-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_CARGA_ABERTA-MSGNO.
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

    READ TABLE ME->DOCUMENTO_FISCAL ASSIGNING FIELD-SYMBOL(<FS_NOTA>) WITH KEY ID_CARGA = I_ID_CARGA ID_NOTA = I_ID_NOTA.

    CHECK SY-SUBRC IS INITIAL.

    ME->CK_ALTEROU = ABAP_TRUE.

    <FS_NOTA>-DS_OBSERVACAO = I_DS_OBSERVACAO.

    MOVE-CORRESPONDING <FS_NOTA> TO E_NOTA.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_ORDEM_CARREGAMENTO.

    R_CARGA = ME.

    IF I_NR_ORDEM IS INITIAL.
      CLEAR: ME->CARGA-ID_ORDEM,
             I_CARGA_ALV-ID_ORDEM,
             E_ORDEM_CARRGAMENTO.
    ENDIF.

    CHECK I_NR_ORDEM IS NOT INITIAL.

    CASE ME->CARGA-TP_STATUS.
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

    IF ME->CARGA-TP_STATUS NE ZIF_CARGA=>ST_STATUS_ABERTO.
      RAISE EXCEPTION TYPE ZCX_CARGA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_CARGA_NAO_ABERTA-MSGID MSGNO = ZCX_CARGA=>ZCX_CARGA_NAO_ABERTA-MSGNO )
          MSGTY  = 'E'
          MSGID  = ZCX_CARGA=>ZCX_CARGA_NAO_ABERTA-MSGID
          MSGNO  = ZCX_CARGA=>ZCX_CARGA_NAO_ABERTA-MSGNO.
    ENDIF.

    E_ORDEM_CARRGAMENTO = ZCL_ORDEM_CARREGAMENTO=>BUSCA_ORDEM_CARREGAMENTO_NR(
    I_NR_SAFRA  = I_NR_SAFRA
    I_ID_BUKRS  = I_ID_BUKRS
    I_ID_BRANCH = I_ID_BRANCH
    I_NR_ORDEM  = I_NR_ORDEM ).

    IF I_VBELN IS NOT INITIAL.
      E_ORDEM_CARRGAMENTO-NR_ORDEM_VENDA = I_VBELN.
    ELSE.
      READ TABLE ME->ZIF_CARGA~ORDEM_VENDA INDEX 1 INTO DATA(WA_ORDEM_VENDA).
      IF SY-SUBRC IS INITIAL.
        E_ORDEM_CARRGAMENTO-NR_ORDEM_VENDA = WA_ORDEM_VENDA-NR_ORDEM_VENDA.
      ENDIF.
    ENDIF.

    CHECK E_ORDEM_CARRGAMENTO IS NOT INITIAL.

    ME->VERIF_ORDEM_CARREGAMENTO( EXPORTING I_ORDEM = E_ORDEM_CARRGAMENTO ).

    ME->CK_ALTEROU = ABAP_TRUE.

    IF ME->DOCUMENTO_FISCAL IS NOT INITIAL.
      READ TABLE ME->ZIF_CARGA~ORDEM_VENDA INDEX 1 INTO WA_ORDEM_VENDA.
      IF WA_ORDEM_VENDA-NR_ORDEM_VENDA IS NOT INITIAL.
        ME->GET_INFO_ORDEM_VENDA( EXPORTING I_ORDEM_VENDA = WA_ORDEM_VENDA-NR_ORDEM_VENDA IMPORTING E_ORDEM = DATA(R_ORDEM) ).
      ENDIF.
    ENDIF.

    IF ME->ZIF_CARGA~ORDEM_VENDA IS INITIAL AND ME->ZIF_CARGA~PEDIDO_COMPRA IS INITIAL.

      I_CARGA_ALV-DS_LOCAL_COLETA   = E_ORDEM_CARRGAMENTO-DS_LOCAL_COLETA.
      I_CARGA_ALV-DS_LOCAL_DESTINO  = E_ORDEM_CARRGAMENTO-DS_LOCAL_DESTINO.
      I_CARGA_ALV-DS_LOCAL_DESCARGA = E_ORDEM_CARRGAMENTO-DS_LOCAL_DESCARGA.

      ME->CARGA-ID_PRODUTO          = E_ORDEM_CARRGAMENTO-ID_PRODUTO.

      I_CARGA_ALV-ID_LOCAL_COLETA   = E_ORDEM_CARRGAMENTO-ID_LOCAL_COLETA.
      I_CARGA_ALV-ID_LOCAL_DESTINO  = E_ORDEM_CARRGAMENTO-ID_LOCAL_DESTINO.
      I_CARGA_ALV-ID_LOCAL_DESCARGA = E_ORDEM_CARRGAMENTO-ID_LOCAL_DESCARGA.
      I_CARGA_ALV-ID_PRODUTO        = E_ORDEM_CARRGAMENTO-ID_PRODUTO.
      I_CARGA_ALV-DS_PRODUTO        = E_ORDEM_CARRGAMENTO-DS_PRODUTO.

    ELSEIF I_CARGA_ALV-ID_LOCAL_COLETA IS INITIAL.
      READ TABLE ME->DOCUMENTO_FISCAL INDEX 1 INTO DATA(WA_DOCUMENTO).
      I_CARGA_ALV-ID_LOCAL_COLETA = WA_DOCUMENTO-ID_FORNECEDOR.
      SELECT SINGLE NAME1 INTO I_CARGA_ALV-DS_LOCAL_COLETA
        FROM KNA1
       WHERE KUNNR EQ I_CARGA_ALV-ID_LOCAL_COLETA.
    ENDIF.

    "Vincular Ordem de Venda a Ordem de Carregamento """""""""""""""""""""""""""
    READ TABLE ME->ZIF_CARGA~ORDEM_VENDA WITH KEY NR_ORDEM_VENDA = I_VBELN ASSIGNING FIELD-SYMBOL(<FS_ORDEM>).
    IF SY-SUBRC IS INITIAL.

      IF SY-TABIX EQ 1.
        DATA(CK_AJUSTA_READER) = ABAP_TRUE.
      ELSE.
        CK_AJUSTA_READER = ABAP_FALSE.
      ENDIF.

      <FS_ORDEM>-ID_ORDEM = E_ORDEM_CARRGAMENTO-ID_ORDEM.
      <FS_ORDEM>-NR_ORDEM = E_ORDEM_CARRGAMENTO-NR_ORDEM.
    ELSE.
      CK_AJUSTA_READER = ABAP_TRUE.
    ENDIF.

    IF CK_AJUSTA_READER EQ ABAP_TRUE.
      "Setando Propriedade do Objeto
      ME->CARGA-ID_ORDEM = E_ORDEM_CARRGAMENTO-ID_ORDEM.

      IF R_ORDEM-DS_TIPO_FRETE NE 'CIF'.
        ME->CARGA-ID_AGENT_FRETE = E_ORDEM_CARRGAMENTO-ID_BRANCH_AG.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = ME->CARGA-ID_AGENT_FRETE
        IMPORTING
          OUTPUT = ME->CARGA-ID_AGENT_FRETE.

      ME->CARGA-ID_LOCAL_COLETA     = E_ORDEM_CARRGAMENTO-ID_LOCAL_COLETA.
      ME->CARGA-ID_LOCAL_DESTINO    = E_ORDEM_CARRGAMENTO-ID_LOCAL_DESTINO.
      ME->CARGA-ID_LOCAL_DESCARGA   = E_ORDEM_CARRGAMENTO-ID_LOCAL_DESCARGA.
      ME->CARGA-DS_PLACA_TRATOR     = E_ORDEM_CARRGAMENTO-DS_PLACA_TRATOR.
      ME->CARGA-ID_PROPRIETARIO     = E_ORDEM_CARRGAMENTO-ID_PROPRIETARIO.
      ME->CARGA-DS_PLACA_REBOQ_1    = E_ORDEM_CARRGAMENTO-DS_PLACA_REBOQ_1.
      ME->CARGA-DS_PLACA_REBOQ_2    = E_ORDEM_CARRGAMENTO-DS_PLACA_REBOQ_2.
      ME->CARGA-DS_PLACA_REBOQ_3    = E_ORDEM_CARRGAMENTO-DS_PLACA_REBOQ_3.
      ME->CARGA-ID_MOTORISTA        = E_ORDEM_CARRGAMENTO-ID_MOTORISTA.

      "Saída para Tela com Valoes Preenchidos
      IF R_ORDEM-DS_TIPO_FRETE NE 'CIF'.
        I_CARGA_ALV-ID_AGENT_FRETE = E_ORDEM_CARRGAMENTO-ID_BRANCH_AG.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = I_CARGA_ALV-ID_AGENT_FRETE
          IMPORTING
            OUTPUT = I_CARGA_ALV-ID_AGENT_FRETE.
        I_CARGA_ALV-DS_AGENT_FRETE     = E_ORDEM_CARRGAMENTO-NAME_AG.
      ENDIF.

      I_CARGA_ALV-ID_ORDEM           = E_ORDEM_CARRGAMENTO-ID_ORDEM.
      I_CARGA_ALV-NR_ORDEM           = E_ORDEM_CARRGAMENTO-NR_ORDEM.
      I_CARGA_ALV-DS_PLACA_TRATOR    = E_ORDEM_CARRGAMENTO-DS_PLACA_TRATOR.
      I_CARGA_ALV-ID_PROPRIETARIO    = E_ORDEM_CARRGAMENTO-ID_PROPRIETARIO.
      I_CARGA_ALV-DS_PLACA_REBOQ_1   = E_ORDEM_CARRGAMENTO-DS_PLACA_REBOQ_1.
      I_CARGA_ALV-DS_PLACA_REBOQ_2   = E_ORDEM_CARRGAMENTO-DS_PLACA_REBOQ_2.
      I_CARGA_ALV-DS_PLACA_REBOQ_3   = E_ORDEM_CARRGAMENTO-DS_PLACA_REBOQ_3.
      I_CARGA_ALV-ID_MOTORISTA       = E_ORDEM_CARRGAMENTO-ID_MOTORISTA.
      I_CARGA_ALV-NR_ORDEM           = E_ORDEM_CARRGAMENTO-NR_ORDEM.
      I_CARGA_ALV-BUTXT              = E_ORDEM_CARRGAMENTO-BUTXT.
      I_CARGA_ALV-NAME               = E_ORDEM_CARRGAMENTO-NAME.
      I_CARGA_ALV-DS_MOTORISTA       = E_ORDEM_CARRGAMENTO-DS_MOTORISTA.
      I_CARGA_ALV-DS_PROPRIETARIO    = E_ORDEM_CARRGAMENTO-DS_PROPRIETARIO.

    ENDIF.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "Informações que não são da rodem de carregamento
    I_CARGA_ALV-TP_FRETE           = ME->CARGA-TP_FRETE.
    I_CARGA_ALV-NM_PESO_BRUTO      = ME->CARGA-NM_PESO_BRUTO.
    I_CARGA_ALV-NM_PESO_TARA       = ME->CARGA-NM_PESO_TARA.
    I_CARGA_ALV-NM_PESO_SUBTOTAL   = ME->CARGA-NM_PESO_SUBTOTAL.
    I_CARGA_ALV-NR_TICKET          = ME->CARGA-NR_TICKET.
    I_CARGA_ALV-IN_TRANSFERENCIA   = ME->CARGA-IN_TRANSFERENCIA.
    I_CARGA_ALV-TP_STATUS          = ME->CARGA-TP_STATUS.
    I_CARGA_ALV-DT_ABERTURA        = ME->CARGA-DT_ABERTURA.
    I_CARGA_ALV-HR_ABERTURA        = ME->CARGA-HR_ABERTURA.
    I_CARGA_ALV-DT_FECHAMENTO      = ME->CARGA-DT_FECHAMENTO.
    I_CARGA_ALV-HR_FECHAMENTO      = ME->CARGA-HR_FECHAMENTO.

  ENDMETHOD.


  METHOD zif_carga~set_ordem_venda.

    DATA: obj_cliente    TYPE REF TO zcl_clientes,
          obj_fornecedor TYPE REF TO zcl_fornecedores,
          lc_zsdt0001ov  TYPE zsdt0001ov,
          lc_zsdt0001ek  TYPE zsdt0001ek,
          e_tipo_frete   TYPE zde_tp_frete,
          i_texto        TYPE string.

    "Ordem de Venda
    "Pedido de Compra
    CLEAR: me->zif_carga~ordem_venda[],
           me->zif_carga~pedido_compra[].

    CHECK i_ordem_venda IS NOT INITIAL.

    TRY.
        me->zif_carga~get_info_pedido_compra( EXPORTING i_pedido_compra = i_ordem_venda IMPORTING e_pedido = DATA(e_pedido) ).
        DATA(ck_pedido) = abap_true.
      CATCH zcx_carga .
        me->get_info_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda IMPORTING e_ordem = DATA(r_retorno_ord) ).
        ck_pedido = abap_false.
    ENDTRY.

    CASE ck_pedido.
      WHEN abap_false.

        me->carga-in_transferencia = abap_false.
        e_carga-in_transferencia   = abap_false.

        me->carga-ck_frete_entrada = COND string( WHEN r_retorno_ord-kvgr5 EQ '002' THEN abap_true ELSE abap_false ).
        e_carga-ck_frete_entrada   = me->carga-ck_frete_entrada.
        me->carga-ck_gera_aviso    = me->carga-ck_frete_entrada.
        e_carga-ck_gera_aviso      = me->carga-ck_frete_entrada.

        lc_zsdt0001ov-id_carga       = me->carga-id_carga.
        lc_zsdt0001ov-nr_ordem_venda = r_retorno_ord-nr_ordem_venda.
        APPEND lc_zsdt0001ov TO me->zif_carga~ordem_venda.

        me->carga-id_produto  = r_retorno_ord-id_produto.

*-CS2022000332-#78064-07.06.2022-JT-inicio
        TRY.
            me->zif_carga~get_contrato_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda
                                                    IMPORTING e_id_contrato = DATA(r_id_contrato) ).
            me->carga-id_contrato = r_id_contrato.

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
*-CS2022000332-#78064-07.06.2022-JT-fim

        "Ponto de Coleta
        TRY.
            me->get_partiner_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda i_funcao_partiner = 'PC' IMPORTING e_partiner = DATA(r_partiner_pc) ).
            me->carga-id_local_coleta = r_partiner_pc-lifnr.
          CATCH zcx_carga .
        ENDTRY.

        "Local de Descarga
        TRY.
            me->get_partiner_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda i_funcao_partiner = 'LR' IMPORTING e_partiner = DATA(r_partiner_lr) ).
            me->carga-id_local_descarga = r_partiner_lr-kunnr.
          CATCH zcx_carga .
        ENDTRY.

        "Local de Entrega
        TRY.
            me->get_partiner_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda i_funcao_partiner = 'Z1' IMPORTING e_partiner = DATA(r_partiner_z1) ).
            me->carga-id_local_destino = r_partiner_z1-lifnr.
          CATCH zcx_carga .
        ENDTRY.

        "Caso Seja Frete CIF buscar SP
        TRY.
            me->get_partiner_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda i_funcao_partiner = 'SP' IMPORTING e_partiner = DATA(r_partiner_sp) ).
            me->carga-id_agent_frete = r_partiner_sp-lifnr.
          CATCH zcx_carga .
        ENDTRY.
        "ENDIF.
        MOVE-CORRESPONDING me->carga TO e_carga.
        e_carga-ds_produto = r_retorno_ord-ds_produto.

        CREATE OBJECT obj_cliente.
        CREATE OBJECT obj_fornecedor.

        IF r_partiner_pc-lifnr IS NOT INITIAL.
          obj_fornecedor->zif_parceiros~set_parceiro( i_parceiro = r_partiner_pc-lifnr )->get_name( IMPORTING e_name = e_carga-ds_local_coleta ).
        ELSE.
          CLEAR: e_carga-ds_local_coleta.
        ENDIF.

        IF r_partiner_lr-kunnr IS NOT INITIAL.
          obj_cliente->zif_parceiros~set_parceiro( i_parceiro = r_partiner_lr-kunnr )->get_name( IMPORTING e_name = e_carga-ds_local_descarga ).
        ELSE.
          CLEAR: e_carga-ds_local_descarga.
        ENDIF.

        IF r_partiner_z1-lifnr IS NOT INITIAL.
          obj_fornecedor->zif_parceiros~set_parceiro( i_parceiro = r_partiner_z1-lifnr )->get_name( IMPORTING e_name = e_carga-ds_local_destino ).
        ELSE.
          CLEAR: e_carga-ds_local_destino.
        ENDIF.

        IF r_partiner_sp-lifnr IS NOT INITIAL.
          obj_fornecedor->zif_parceiros~set_parceiro( i_parceiro = r_partiner_sp-lifnr )->get_name( IMPORTING e_name = e_carga-ds_agent_frete ).
        ELSE.
          CLEAR: e_carga-ds_agent_frete.
        ENDIF.

        me->zif_carga~set_tipo_frete_ordem_venda( ).

        TRY .
            zcl_ordem_venda=>zif_ordem_venda~get_ordem_venda_saldo( EXPORTING i_vbeln = lc_zsdt0001ov-nr_ordem_venda IMPORTING e_saldo = DATA(e_saldo) ).
            c_zde_zsdt0001ov_alv-nm_saldo_ov = e_saldo.
          CATCH zcx_ordem_venda.
            c_zde_zsdt0001ov_alv-nm_saldo_ov = 0.
        ENDTRY.

        CLEAR: obj_cliente, obj_fornecedor.

      WHEN abap_true.

        lc_zsdt0001ek-id_carga         = me->carga-id_carga.
        lc_zsdt0001ek-nr_pedido_compra = e_pedido-nr_pedido_compra.
        APPEND lc_zsdt0001ek TO me->zif_carga~pedido_compra.
        me->carga-id_produto  = e_pedido-id_produto.

        CASE e_pedido-tp_tipo_pedido.
          WHEN 'ZUB'.

            me->carga-in_transferencia = abap_true.
            e_carga-in_transferencia   = abap_true.

            me->carga-ck_frete_entrada = abap_true.
            e_carga-ck_frete_entrada   = e_pedido-zckfreteent.

            me->carga-ck_gera_aviso    = me->carga-ck_frete_entrada.
            e_carga-ck_gera_aviso      = me->carga-ck_frete_entrada.

            e_carga-ds_produto         = e_pedido-ds_produto.
            e_carga-id_produto         = e_pedido-id_produto.

            CREATE OBJECT obj_fornecedor.

            IF e_pedido-id_centro_saida IS NOT INITIAL.
              IF e_pedido-id_local_coleta IS NOT INITIAL.
                e_carga-id_local_coleta = e_pedido-id_local_coleta.
              ELSE.
                e_carga-id_local_coleta = e_pedido-id_centro_saida.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = e_carga-id_local_coleta
                  IMPORTING
                    output = e_carga-id_local_coleta.
              ENDIF.
              obj_fornecedor->zif_parceiros~set_parceiro( i_parceiro = e_carga-id_local_coleta )->get_name( IMPORTING e_name = e_carga-ds_local_coleta ).

            ENDIF.

            IF e_pedido-id_centro_recebedor IS NOT INITIAL.
              e_carga-id_local_descarga = e_pedido-id_centro_recebedor.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = e_carga-id_local_descarga
                IMPORTING
                  output = e_carga-id_local_descarga.

              obj_fornecedor->zif_parceiros~set_parceiro( i_parceiro = e_carga-id_local_descarga )->get_name( IMPORTING e_name = e_carga-ds_local_descarga ).
              e_carga-id_local_destino = e_carga-id_local_descarga.
              e_carga-ds_local_destino = e_carga-ds_local_descarga.
            ENDIF.

            me->zif_carga~set_tipo_frete_ordem_venda( ).

            CLEAR: obj_fornecedor.

          WHEN OTHERS.
            me->carga-in_transferencia = abap_false.
            e_carga-in_transferencia   = abap_false.

            me->carga-ck_frete_entrada = abap_false.
            e_carga-ck_frete_entrada   = abap_false.
        ENDCASE.

        TRY .
            c_zde_zsdt0001ov_alv-nm_saldo_ov = zcl_pedido_compra=>get_saldo_pedido_item( EXPORTING i_ebeln = lc_zsdt0001ov-nr_ordem_venda ).
          CATCH zcx_ordem_venda.
            c_zde_zsdt0001ov_alv-nm_saldo_ov = 0.
        ENDTRY.

    ENDCASE.

  ENDMETHOD.


  method zif_carga~set_pesos_notas.

    r_carga = me.

    case me->carga-tp_status.
      when zif_carga=>st_status_conferido.
        if me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_carga_conferida-msgid msgno = zcx_carga=>zcx_carga_conferida-msgno )
              msgty  = 'E'
              msgid  = zcx_carga=>zcx_carga_conferida-msgid
              msgno  = zcx_carga=>zcx_carga_conferida-msgno.
        endif.
      when zif_carga=>st_status_cancelada.
        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_carga_cancelada-msgid msgno = zcx_carga=>zcx_carga_cancelada-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_carga_cancelada-msgid
            msgno  = zcx_carga=>zcx_carga_cancelada-msgno.
    endcase.

    read table me->documento_fiscal assigning field-symbol(<fs_nota>) with key id_carga = i_id_carga id_nota = i_id_nota.

    check sy-subrc is initial.
*    BUG179188

    me->ck_alterou = abap_true.

    me->get_rateia_descontos(
      exporting
        i_descontos     = me->resultado
        i_peso_liquido  = i_peso_liquido
      importing
        e_peso_subtotal = data(e_peso_subtotal)
        e_rateio        = data(r_rateio) ).

    <fs_nota>-nm_peso_subtotal = e_peso_subtotal.
    <fs_nota>-nm_peso_liquido  = i_peso_liquido.

    move-corresponding <fs_nota> to e_nota.

    if i_sem_consulta eq abap_false.
      select single name1
        into e_nota-ds_fornecedor
        from lfa1 where lifnr eq <fs_nota>-id_fornecedor.

      select single maktx
        into e_nota-ds_produto
        from makt
       where matnr eq me->carga-id_produto
         and spras eq sy-langu.

      select single ds_entrada
        into e_nota-ds_entrada
        from zsdt0001tetx where id_entrada eq e_nota-id_entrada.

      if e_nota-id_entregue_por is not initial.
        select single name1
          into e_nota-ds_entregue_por
          from lfa1 where lifnr eq e_nota-id_entregue_por.
      endif.
    endif.

    "Classificação
    loop at r_rateio into data(wa_resultado).
      case wa_resultado-tp_caracteristica.
        when zif_carga=>st_tp_caract_class_umidade.
          e_nota-nr_perc_umi = wa_resultado-nr_percentual_com.
          e_nota-nr_qtde_umi = wa_resultado-nr_quantidade_com.
        when zif_carga=>st_tp_caract_class_impureza.
          e_nota-nr_perc_imp = wa_resultado-nr_percentual_com.
          e_nota-nr_qtde_imp = wa_resultado-nr_quantidade_com.
        when zif_carga=>st_tp_caract_class_avariado.
          e_nota-nr_perc_ava = wa_resultado-nr_percentual_com.
          e_nota-nr_qtde_ava = wa_resultado-nr_quantidade_com.
        when zif_carga=>st_tp_caract_class_ardido.
          e_nota-nr_perc_ard = wa_resultado-nr_percentual_com.
          e_nota-nr_qtde_ard = wa_resultado-nr_quantidade_com.
        when zif_carga=>st_tp_caract_class_quebrado.
          e_nota-nr_perc_que = wa_resultado-nr_percentual_com.
          e_nota-nr_qtde_que = wa_resultado-nr_quantidade_com.
        when zif_carga=>st_tp_caract_class_esverdeado.
          e_nota-nr_perc_esv = wa_resultado-nr_percentual_com.
          e_nota-nr_qtde_esv = wa_resultado-nr_quantidade_com.
        when zif_carga=>st_tp_caract_class_carunchado.
          e_nota-nr_perc_car = wa_resultado-nr_percentual_com.
          e_nota-nr_qtde_car = wa_resultado-nr_quantidade_com.
      endcase.
    endloop.

  endmethod.


  METHOD zif_carga~set_processar_entrada.

    DATA: wa_acttab    TYPE j_1bnfe_active,
          obj_romaneio TYPE REF TO zcl_repository_classes,
          i_texto      TYPE string.

    r_carga = me.

    CHECK me->zif_carga~at_manutencao EQ abap_false.

    CHECK NOT ( me->carga-ck_enviado_opus EQ abap_true AND me->carga-ck_recebido_opus EQ abap_false ).

    e_gerou_entrada = abap_false.
    DATA: ck_erro TYPE char01 VALUE abap_false.

    CHECK me->carga-tp_status EQ zif_carga=>st_status_conferido OR i_somente_atualizar EQ abap_true.

    LOOP AT me->documento_fiscal ASSIGNING FIELD-SYMBOL(<fs_documento>) WHERE nr_nota   IS INITIAL AND docnum IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
        FROM j_1bnfdoc
       WHERE docnum EQ @<fs_documento>-docnum.

      IF wa_j_1bnfdoc-form IS NOT INITIAL AND wa_j_1bnfdoc-cancel EQ abap_false.

        "Verificar CT-e Autorizada
        CALL FUNCTION 'J_1B_NFE_XML_RAED_ACTIVE_TAB'
          EXPORTING
            i_docnum = wa_j_1bnfdoc-docnum
          IMPORTING
            e_acttab = wa_acttab
          EXCEPTIONS
            no_entry = 1
            OTHERS   = 2.

        "Atualizar Romaneio de Entrada """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF sy-subrc IS INITIAL AND wa_acttab-nfnum9 IS NOT INITIAL AND wa_acttab-docsta EQ '1' AND wa_acttab-cancel NE abap_true.
          "Ajusta Nota Fiscal
          <fs_documento>-nr_nota            = wa_acttab-nfnum9.
          <fs_documento>-nm_serie           = wa_acttab-serie.
          <fs_documento>-dt_emissao         = wa_j_1bnfdoc-docdat.
          <fs_documento>-dt_vencimento_form = wa_j_1bnfdoc-docdat.
          CONCATENATE wa_acttab-regio wa_acttab-nfyear wa_acttab-nfmonth wa_acttab-stcd1 wa_acttab-model wa_acttab-serie wa_acttab-nfnum9 wa_acttab-docnum9 wa_acttab-cdv INTO <fs_documento>-nr_chave_nfe.

          SELECT SINGLE * INTO @DATA(wa_item)
            FROM j_1bnflin
           WHERE docnum EQ @wa_j_1bnfdoc-docnum.

          <fs_documento>-cfop = wa_item-cfop(4).

          "Ajusta Romaneio de Entrada
          IF <fs_documento>-ch_referencia_ent IS NOT INITIAL.
            CREATE OBJECT obj_romaneio.
            obj_romaneio->romaneio( )->zif_cadastro~set_registro( i_id_registro = <fs_documento>-ch_referencia_ent ).
            obj_romaneio->at_romaneio->set_nfe( i_nfe = abap_true ).
            obj_romaneio->at_romaneio->set_nfnum( i_nfnum = CONV #( wa_acttab-nfnum9 ) ).
            obj_romaneio->at_romaneio->set_series( i_series = wa_acttab-serie ).
            obj_romaneio->at_romaneio->set_docdat( i_docdat = wa_j_1bnfdoc-docdat ).
            obj_romaneio->at_romaneio->zif_cadastro~gravar_registro( ).
            obj_romaneio->free( ).
            CLEAR obj_romaneio.
          ENDIF.
          me->ck_alterou = abap_true.
        ENDIF.
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      ENDIF.

    ENDLOOP.

    CHECK i_somente_atualizar EQ abap_false.

    SELECT * INTO TABLE @DATA(it_entrada)
      FROM zmmt_ee_zgr
       FOR ALL ENTRIES IN @me->documento_fiscal
     WHERE id_carga    EQ @me->carga-id_carga
       AND st_estorno  EQ @zif_carga=>st_status_estorno_sem
       AND obj_key     EQ @me->documento_fiscal-obj_key_entrada.

    DATA(it_entrada_gravar) = it_entrada[].

    "Verifica Período de MM/FI
    TRY .
        zcl_miro=>verificar_criar( i_data = me->carga-dt_fechamento i_bukrs = me->carga-id_bukrs ).
      CATCH zcx_miro_exception INTO DATA(ex_miro).
        MESSAGE ID ex_miro->msgid TYPE 'S'
         NUMBER ex_miro->msgno
           WITH ex_miro->msgv1 ex_miro->msgv2 ex_miro->msgv3 ex_miro->msgv4
           INTO i_texto.
        me->gera_erro_geral( i_texto = i_texto ).
    ENDTRY.

    "Verifica XML do Documento Fiscal
    LOOP AT me->documento_fiscal INTO DATA(wa_fiscal) WHERE id_mod_fiscal EQ zif_carga=>st_model_fiscal_eletronico.
      READ TABLE it_entrada INTO DATA(wa_entrada) WITH KEY id_nota = wa_fiscal-id_nota.
      IF sy-subrc IS INITIAL.
        TRY .
            CALL METHOD zcl_miro=>verificar_forn_doc_fiscal
              EXPORTING
                i_lifnr  = wa_entrada-lifnr
                i_nftype = wa_entrada-j_1bnftype
                i_xblnr  = wa_entrada-ref_doc_no
                i_data   = wa_entrada-doc_date
                i_werks  = wa_entrada-bus_area.
          CATCH zcx_miro_exception INTO ex_miro.
            MESSAGE ID ex_miro->msgid TYPE 'S'
             NUMBER ex_miro->msgno
               WITH ex_miro->msgv1 ex_miro->msgv2 ex_miro->msgv3 ex_miro->msgv4
               INTO i_texto.
            me->gera_erro_geral( i_texto = i_texto ).
        ENDTRY.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'J_1B_NF_CANCEL_CHECK_PERIOD'
      EXPORTING
        i_budat      = me->carga-dt_movimento
        i_bukrs      = me->carga-id_bukrs
      EXCEPTIONS
        wrong_period = 1
        OTHERS       = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO DATA(mtext) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      me->gera_erro_geral( i_texto = mtext ).
    ENDIF.

    LOOP AT it_entrada ASSIGNING FIELD-SYMBOL(<entrada>).

      IF ck_erro NE abap_false.
        CONTINUE.
      ENDIF.

*      IF 1 = 2.
      SELECT SINGLE * INTO @DATA(documentos)
        FROM zmmt_ee_zgr_docs
       WHERE obj_key EQ @<entrada>-obj_key.

      "Se existe documento gerado ajusta Nota Fiscal
      IF sy-subrc IS INITIAL AND (
         "DOCUMENTOS-AV_VBELN  IS NOT INITIAL OR
         documentos-mm_mblnr  IS NOT INITIAL OR
         documentos-mm_mjahr  IS NOT INITIAL OR
         documentos-ft_belnr  IS NOT INITIAL OR
         documentos-ft_gjahr  IS NOT INITIAL OR
         documentos-docnum    IS NOT INITIAL ).
        DELETE it_entrada_gravar WHERE obj_key EQ <entrada>-obj_key.

        READ TABLE me->documento_fiscal ASSIGNING <fs_documento>
        WITH KEY obj_key_entrada = <entrada>-obj_key.
        IF sy-subrc IS INITIAL.
          me->ck_alterou = abap_true.
          <fs_documento>-po_number = <entrada>-po_number.
          <fs_documento>-po_item   = <entrada>-po_item.
          "<FS_DOCUMENTO>-AV_VBELN  = DOCUMENTOS-AV_VBELN.
          <fs_documento>-mm_mblnr  = documentos-mm_mblnr.
          <fs_documento>-mm_mjahr  = documentos-mm_mjahr.
          <fs_documento>-ft_belnr  = documentos-ft_belnr.
          <fs_documento>-ft_gjahr  = documentos-ft_gjahr.
          IF <fs_documento>-docnum_np IS INITIAL. "*-CS2021000183 - 29.03.2022 - JT - inicio
            <fs_documento>-docnum  = documentos-docnum.
          ENDIF.
          e_gerou_entrada = abap_true.
        ENDIF.
      ELSE.

        DATA: number           TYPE tbtcjob-jobcount,
              name             TYPE tbtcjob-jobname,
              print_parameters TYPE pri_params.

        me->zif_carga~verif_bloq_lote_material_wait( ).

        IF <entrada>-tp_operacao EQ '01' OR
           <entrada>-tp_operacao EQ '02' OR
           <entrada>-tp_operacao EQ '03' OR
           <entrada>-tp_operacao EQ '04' OR
           <entrada>-tp_operacao EQ '08' OR
           <entrada>-tp_operacao EQ '09' OR
           <entrada>-tp_operacao EQ '10'.

          IF 1 = 2.
            SUBMIT zmmr019 WITH pobjkey EQ <entrada>-obj_key AND RETURN.
          ELSE.

            DATA(lc_user_job) = zcl_job=>get_user_job( ).

            CONCATENATE 'JOB_ENTRADA' me->carga-id_carga INTO name SEPARATED BY '_'.

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

            IF sy-subrc IS INITIAL.
              SUBMIT zmmr019 TO SAP-SPOOL SPOOL PARAMETERS print_parameters WITHOUT SPOOL DYNPRO VIA JOB name NUMBER number
                WITH pobjkey EQ <entrada>-obj_key
                USER lc_user_job
                 AND RETURN.

              IF sy-subrc IS INITIAL.
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

                IF sy-subrc IS NOT INITIAL.
                  ck_erro = abap_true.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                  CALL FUNCTION 'BP_JOB_DELETE'
                    EXPORTING
                      jobcount                 = number
                      jobname                  = name
                    EXCEPTIONS
                      cant_delete_event_entry  = 1
                      cant_delete_job          = 2
                      cant_delete_joblog       = 3
                      cant_delete_steps        = 4
                      cant_delete_time_entry   = 5
                      cant_derelease_successor = 6
                      cant_enq_predecessor     = 7
                      cant_enq_successor       = 8
                      cant_enq_tbtco_entry     = 9
                      cant_update_predecessor  = 10
                      cant_update_successor    = 11
                      commit_failed            = 12
                      jobcount_missing         = 13
                      jobname_missing          = 14
                      job_does_not_exist       = 15
                      job_is_already_running   = 16
                      no_delete_authority      = 17
                      OTHERS                   = 18.
                  IF sy-subrc IS NOT INITIAL.
                    ck_erro = abap_false.
                  ENDIF.
                ENDIF.
              ELSE.
                ck_erro = abap_true.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                CALL FUNCTION 'BP_JOB_DELETE'
                  EXPORTING
                    jobcount                 = number
                    jobname                  = name
                  EXCEPTIONS
                    cant_delete_event_entry  = 1
                    cant_delete_job          = 2
                    cant_delete_joblog       = 3
                    cant_delete_steps        = 4
                    cant_delete_time_entry   = 5
                    cant_derelease_successor = 6
                    cant_enq_predecessor     = 7
                    cant_enq_successor       = 8
                    cant_enq_tbtco_entry     = 9
                    cant_update_predecessor  = 10
                    cant_update_successor    = 11
                    commit_failed            = 12
                    jobcount_missing         = 13
                    jobname_missing          = 14
                    job_does_not_exist       = 15
                    job_is_already_running   = 16
                    no_delete_authority      = 17
                    OTHERS                   = 18.
                IF sy-subrc IS NOT INITIAL.
                  ck_erro = abap_false.
                ENDIF.
              ENDIF.
            ENDIF.

            "Aguardar execução do job
            zcl_job=>get_instance(
             )->set_key_job( i_jobname = name i_jobcount = number
             )->get_wait_job_exec(
             ).

          ENDIF.
        ELSEIF <entrada>-tp_operacao EQ '11'.

          IF 1 = 2.
            SUBMIT zmmr154 WITH pobjkey EQ <entrada>-obj_key AND RETURN.
          ELSE.

            lc_user_job = zcl_job=>get_user_job( ).

            CONCATENATE 'JOB_ENTRADA' me->carga-id_carga INTO name SEPARATED BY '_'.

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

            IF sy-subrc IS INITIAL.
              SUBMIT zmmr154 TO SAP-SPOOL SPOOL PARAMETERS print_parameters WITHOUT SPOOL DYNPRO VIA JOB name NUMBER number
                WITH pobjkey EQ <entrada>-obj_key
                USER lc_user_job
                 AND RETURN.

              IF sy-subrc IS INITIAL.
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

                IF sy-subrc IS NOT INITIAL.
                  ck_erro = abap_true.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                  CALL FUNCTION 'BP_JOB_DELETE'
                    EXPORTING
                      jobcount                 = number
                      jobname                  = name
                    EXCEPTIONS
                      cant_delete_event_entry  = 1
                      cant_delete_job          = 2
                      cant_delete_joblog       = 3
                      cant_delete_steps        = 4
                      cant_delete_time_entry   = 5
                      cant_derelease_successor = 6
                      cant_enq_predecessor     = 7
                      cant_enq_successor       = 8
                      cant_enq_tbtco_entry     = 9
                      cant_update_predecessor  = 10
                      cant_update_successor    = 11
                      commit_failed            = 12
                      jobcount_missing         = 13
                      jobname_missing          = 14
                      job_does_not_exist       = 15
                      job_is_already_running   = 16
                      no_delete_authority      = 17
                      OTHERS                   = 18.
                  IF sy-subrc IS NOT INITIAL.
                    ck_erro = abap_false.
                  ENDIF.
                ENDIF.
              ELSE.
                ck_erro = abap_true.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                CALL FUNCTION 'BP_JOB_DELETE'
                  EXPORTING
                    jobcount                 = number
                    jobname                  = name
                  EXCEPTIONS
                    cant_delete_event_entry  = 1
                    cant_delete_job          = 2
                    cant_delete_joblog       = 3
                    cant_delete_steps        = 4
                    cant_delete_time_entry   = 5
                    cant_derelease_successor = 6
                    cant_enq_predecessor     = 7
                    cant_enq_successor       = 8
                    cant_enq_tbtco_entry     = 9
                    cant_update_predecessor  = 10
                    cant_update_successor    = 11
                    commit_failed            = 12
                    jobcount_missing         = 13
                    jobname_missing          = 14
                    job_does_not_exist       = 15
                    job_is_already_running   = 16
                    no_delete_authority      = 17
                    OTHERS                   = 18.
                IF sy-subrc IS NOT INITIAL.
                  ck_erro = abap_false.
                ENDIF.
              ENDIF.
            ENDIF.

            "Aguardar execução do job
            zcl_job=>get_instance(
             )->set_key_job( i_jobname = name i_jobcount = number
             )->get_wait_job_exec(
             ).

          ENDIF.

        ELSEIF <entrada>-tp_operacao EQ '05' OR
               <entrada>-tp_operacao EQ '06' OR
               <entrada>-tp_operacao EQ '07'.

          IF 1 = 2.
            SUBMIT zmmr021 WITH pobjkey EQ <entrada>-obj_key AND RETURN.
          ELSE.

            lc_user_job = zcl_job=>get_user_job( ).

            CONCATENATE 'JOB_ENTRADA' me->carga-id_carga INTO name SEPARATED BY '_'.

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

            IF sy-subrc IS INITIAL.
              SUBMIT zmmr021 TO SAP-SPOOL SPOOL PARAMETERS print_parameters WITHOUT SPOOL DYNPRO VIA JOB name NUMBER number
                WITH pobjkey EQ <entrada>-obj_key
                USER lc_user_job
                 AND RETURN.

              IF sy-subrc IS INITIAL.
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

                IF sy-subrc IS NOT INITIAL.
                  ck_erro = abap_true.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                  CALL FUNCTION 'BP_JOB_DELETE'
                    EXPORTING
                      jobcount                 = number
                      jobname                  = name
                    EXCEPTIONS
                      cant_delete_event_entry  = 1
                      cant_delete_job          = 2
                      cant_delete_joblog       = 3
                      cant_delete_steps        = 4
                      cant_delete_time_entry   = 5
                      cant_derelease_successor = 6
                      cant_enq_predecessor     = 7
                      cant_enq_successor       = 8
                      cant_enq_tbtco_entry     = 9
                      cant_update_predecessor  = 10
                      cant_update_successor    = 11
                      commit_failed            = 12
                      jobcount_missing         = 13
                      jobname_missing          = 14
                      job_does_not_exist       = 15
                      job_is_already_running   = 16
                      no_delete_authority      = 17
                      OTHERS                   = 18.
                  IF sy-subrc IS NOT INITIAL.
                    ck_erro = abap_false.
                  ENDIF.
                ENDIF.
              ELSE.
                ck_erro = abap_true.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                CALL FUNCTION 'BP_JOB_DELETE'
                  EXPORTING
                    jobcount                 = number
                    jobname                  = name
                  EXCEPTIONS
                    cant_delete_event_entry  = 1
                    cant_delete_job          = 2
                    cant_delete_joblog       = 3
                    cant_delete_steps        = 4
                    cant_delete_time_entry   = 5
                    cant_derelease_successor = 6
                    cant_enq_predecessor     = 7
                    cant_enq_successor       = 8
                    cant_enq_tbtco_entry     = 9
                    cant_update_predecessor  = 10
                    cant_update_successor    = 11
                    commit_failed            = 12
                    jobcount_missing         = 13
                    jobname_missing          = 14
                    job_does_not_exist       = 15
                    job_is_already_running   = 16
                    no_delete_authority      = 17
                    OTHERS                   = 18.
                IF sy-subrc IS NOT INITIAL.
                  ck_erro = abap_false.
                ENDIF.
              ENDIF.
            ENDIF.

            "Aguardar execução do job
            zcl_job=>get_instance(
             )->set_key_job( i_jobname = name i_jobcount = number
             )->get_wait_job_exec(
             ).

          ENDIF.

        ENDIF.

        SELECT SINGLE * INTO documentos
          FROM zmmt_ee_zgr_docs
         WHERE obj_key EQ <entrada>-obj_key.

        IF sy-subrc IS INITIAL AND (
                 "DOCUMENTOS-AV_VBELN  IS NOT INITIAL OR
                 documentos-mm_mblnr  IS NOT INITIAL OR
                 documentos-mm_mjahr  IS NOT INITIAL OR
                 documentos-ft_belnr  IS NOT INITIAL OR
                 documentos-ft_gjahr  IS NOT INITIAL OR
                 documentos-docnum    IS NOT INITIAL ).
          DELETE it_entrada_gravar WHERE obj_key EQ <entrada>-obj_key.

          READ TABLE me->documento_fiscal ASSIGNING <fs_documento>
          WITH KEY obj_key_entrada = <entrada>-obj_key.
          IF sy-subrc IS INITIAL.
            me->ck_alterou = abap_true.
            <fs_documento>-po_number = <entrada>-po_number.
            <fs_documento>-po_item   = <entrada>-po_item.
            "<FS_DOCUMENTO>-AV_VBELN  = DOCUMENTOS-AV_VBELN.
            <fs_documento>-mm_mblnr  = documentos-mm_mblnr.
            <fs_documento>-mm_mjahr  = documentos-mm_mjahr.
            <fs_documento>-ft_belnr  = documentos-ft_belnr.
            <fs_documento>-ft_gjahr  = documentos-ft_gjahr.
            IF <fs_documento>-docnum_np IS INITIAL.
              <fs_documento>-docnum  = documentos-docnum. "*-CS2021000183 - 29.03.2022 - JT - inicio
            ENDIF.
            e_gerou_entrada = abap_true.

            IF documentos-docnum IS NOT INITIAL.
              CALL FUNCTION 'Z_INFO_NFE_FORNECEDOR_GERAL_01'
                EXPORTING
                  i_docnum = documentos-docnum.
            ENDIF.
          ENDIF.
        ELSE.
          "Erro.
          ck_erro = abap_true.
        ENDIF.

      ENDIF.

    ENDLOOP.

    "Estornar
    IF ck_erro EQ abap_true.
      me->zif_carga~set_gerar_estorno_estoque(
      )->set_processar_estorno(
      )->set_cancelar_nota_propria(  "CS2021000183-#83755-19.07.2022-JT
      ).
      e_gerou_entrada = abap_false.
      e_msg_erro      = mtext.
    ENDIF.

  ENDMETHOD.


  METHOD zif_carga~set_processar_estorno.

    DATA: ob_romaneio   TYPE REF TO zcl_romaneio,
          rg_st_estorno TYPE RANGE OF zde_status_estorno.

    r_carga = me.

    CHECK me->zif_carga~at_manutencao EQ abap_false.

*      CHECK ME->CARGA-TP_STATUS EQ ZIF_CARGA=>ST_STATUS_CONFERIDO.

*      LOOP AT ME->DOCUMENTO_FISCAL ASSIGNING FIELD-SYMBOL(<FS_DOCUMENTO>) WHERE CH_REFERENCIA_ENT IS NOT INITIAL.
*
*        SELECT * INTO TABLE @DATA(IT_ENTRADA)
*          FROM ZMMT_EE_ZGR
*         WHERE ID_CARGA   EQ @<FS_DOCUMENTO>-ID_CARGA
*           AND ID_NOTA    EQ @<FS_DOCUMENTO>-ID_NOTA.
*
*        IF IT_ENTRADA[] IS INITIAL.
*          "Não Tem Solicitação de Entrada de Estoque
*          "Excluir Romaneio de Entrada
*          CREATE OBJECT OB_ROMANEIO.
*          OB_ROMANEIO->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = <FS_DOCUMENTO>-CH_REFERENCIA_ENT ).
*          OB_ROMANEIO->ZIF_CADASTRO~EXCLUIR_REGISTRO( ).
*          CLEAR OB_ROMANEIO.
*          ME->CK_ALTEROU = ABAP_TRUE.
*          CLEAR: <FS_DOCUMENTO>-CH_REFERENCIA_ENT, <FS_DOCUMENTO>-NR_ROMANEIO_ENT.
*        ENDIF.
*
*      ENDLOOP.

    rg_st_estorno = VALUE #( sign = 'I' option = 'EQ'
                             ( low = zif_carga=>st_status_estorno_solicitado  high = zif_carga=>st_status_estorno_solicitado )
                             ( low = zif_carga=>st_status_estorno_bloqueio    high = zif_carga=>st_status_estorno_bloqueio ) ).

    SELECT * INTO TABLE @DATA(it_entrada)
      FROM zmmt_ee_zgr
     WHERE id_carga   EQ @me->carga-id_carga
       AND st_estorno IN @rg_st_estorno.

    IF it_entrada[] IS NOT INITIAL.

      SELECT * INTO TABLE @DATA(it_estorno)
        FROM zmmt_eee_zgr
         FOR ALL ENTRIES IN @it_entrada
       WHERE obj_key EQ @it_entrada-obj_key.

      SELECT * INTO TABLE @DATA(it_entrada_docs)
        FROM zmmt_ee_zgr_docs
         FOR ALL ENTRIES IN @it_entrada
       WHERE obj_key EQ @it_entrada-obj_key.

      "ME->GET_CHECK_JOB_EXECUCAO( ).

      LOOP AT it_entrada INTO DATA(wa_entrada).
        me->bloquear_entrada( i_obj_key = wa_entrada-obj_key ).
      ENDLOOP.

      LOOP AT it_entrada ASSIGNING FIELD-SYMBOL(<entrada>).

        READ TABLE it_estorno ASSIGNING FIELD-SYMBOL(<estorno>) WITH KEY  obj_key = <entrada>-obj_key.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        IF <estorno>-rg_atualizado EQ '0'.

          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = VALUE #( msgid = zcx_carga=>zcx_solic_estorno_andamento-msgid
                                msgno = zcx_carga=>zcx_solic_estorno_andamento-msgno )
              msgty  = 'E'
              msgid  = zcx_carga=>zcx_solic_estorno_andamento-msgid
              msgno  = zcx_carga=>zcx_solic_estorno_andamento-msgno.

        ELSEIF <estorno>-rg_atualizado EQ '1'.

          READ TABLE it_entrada_docs INTO DATA(wa_entrada_docs) WITH KEY obj_key = <estorno>-obj_key.
          IF sy-subrc IS INITIAL.
            me->zif_carga~get_documento_ent_estornado( EXPORTING i_obj_key = <entrada>-obj_key IMPORTING r_estornado = DATA(r_estornado) ).
          ELSE.
            r_estornado = abap_true. "Documentos já estornados
          ENDIF.

          IF r_estornado EQ abap_true.
            "Não Existe nenhum documento para ser estornado
            <entrada>-st_estorno = zif_carga=>st_status_estorno_executado.

            "Limpar Registro de Nota Fiscal
            READ TABLE me->documento_fiscal ASSIGNING FIELD-SYMBOL(<fs_documento>) WITH KEY obj_key_entrada = <entrada>-obj_key.
            IF sy-subrc IS INITIAL.

              IF <fs_documento>-ch_referencia_ent IS NOT INITIAL AND
                 me->zif_carga~ck_executar_manutencao_entrada EQ abap_false.

                SELECT SINGLE * INTO @DATA(wa_romaneio_ent)
                  FROM zsdt0001
                 WHERE id_carga      EQ @me->carga-id_carga
                   AND ch_referencia EQ @<fs_documento>-ch_referencia_ent.

                IF wa_romaneio_ent IS NOT INITIAL.
                  CREATE OBJECT ob_romaneio.
                  ob_romaneio->zif_cadastro~set_registro( i_id_registro = wa_romaneio_ent-ch_referencia ).
                  ob_romaneio->zif_cadastro~excluir_registro( ).
                  CLEAR ob_romaneio.
                ENDIF.

              ENDIF.

              IF me->zif_carga~ck_executar_manutencao_entrada EQ abap_false.
                CLEAR: <fs_documento>-ch_referencia_ent, <fs_documento>-nr_romaneio_ent.
              ENDIF.

              me->ck_alterou = abap_true.

              CLEAR: <fs_documento>-mm_mblnr,
                     <fs_documento>-mm_mjahr,
                     <fs_documento>-ft_belnr,
                     <fs_documento>-ft_gjahr,
                     <fs_documento>-docnum,
                     <fs_documento>-obj_key_entrada.

              IF <fs_documento>-av_vbeln IS NOT INITIAL.
                "Verifica Aviso
                SELECT SINGLE * INTO @DATA(wa_likp)
                  FROM likp
                 WHERE vbeln EQ @<fs_documento>-av_vbeln.

                IF sy-subrc IS NOT INITIAL.
                  CLEAR: <fs_documento>-av_vbeln.
                ENDIF.
              ENDIF.

            ENDIF.

            e_estornou = abap_true.

          ELSE.

            "Valida Estorno """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            IF wa_entrada_docs-docnum IS NOT INITIAL.
              SELECT SINGLE * INTO @DATA(wa_nota)
                FROM j_1bnfdoc
               WHERE docnum EQ @wa_entrada_docs-docnum.

              IF sy-subrc IS INITIAL.

                IF <estorno>-dt_estorno IS NOT INITIAL.
                  DATA(lc_dt_estorno) = <estorno>-dt_estorno.
                ELSE.
                  lc_dt_estorno = wa_nota-pstdat.
                ENDIF.

                CALL FUNCTION 'J_1B_NF_CANCEL_CHECK_PERIOD'
                  EXPORTING
                    i_budat      = lc_dt_estorno
                    i_bukrs      = wa_nota-bukrs
                  EXCEPTIONS
                    wrong_period = 1
                    OTHERS       = 2.

                IF sy-subrc IS NOT INITIAL.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO DATA(mtext) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                  me->zif_carga~gera_erro_geral( i_texto = mtext ).
                ENDIF.
              ENDIF.
            ENDIF.
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            "Excluir Romaneio de Saída
            IF me->zif_carga~ck_executar_manutencao_entrada EQ abap_false.

              LOOP AT me->zif_carga~ordem_venda ASSIGNING FIELD-SYMBOL(<fs_ordem>).

                IF me->zif_carga~at_manutencao EQ abap_false.
                  me->zif_carga~get_ck_excluir_romaneio_saida( ).
                ENDIF.

                IF <fs_ordem>-ch_referencia_sai IS NOT INITIAL.
                  SELECT SINGLE * INTO @DATA(wa_romaneio)
                    FROM zsdt0001
                   WHERE id_carga      EQ @me->carga-id_carga
                     AND ch_referencia EQ @<fs_ordem>-ch_referencia_sai.

                  IF sy-subrc eq 0 and wa_romaneio IS NOT INITIAL .
                    CREATE OBJECT ob_romaneio.
                    ob_romaneio->zif_cadastro~set_registro( i_id_registro = wa_romaneio-ch_referencia ).
                    ob_romaneio->zif_cadastro~excluir_registro( ).
                    CLEAR ob_romaneio.
                  ENDIF.
                  me->ck_alterou = abap_true.
                  CLEAR: <fs_ordem>-ch_referencia_sai, <fs_ordem>-nr_romaneio_sai.
                ENDIF.

              ENDLOOP.

              LOOP AT me->zif_carga~pedido_compra ASSIGNING FIELD-SYMBOL(<fs_pedido>).

                IF me->zif_carga~at_manutencao EQ abap_false.
                  me->zif_carga~get_ck_excluir_romaneio_saida( ).
                ENDIF.

                IF <fs_pedido>-ch_referencia_sai IS NOT INITIAL.
                  SELECT SINGLE * INTO wa_romaneio
                    FROM zsdt0001
                   WHERE id_carga      EQ me->carga-id_carga
                     AND ch_referencia EQ <fs_pedido>-ch_referencia_sai.

                  IF sy-subrc eq 0 and wa_romaneio IS NOT INITIAL.
                    CREATE OBJECT ob_romaneio.
                    ob_romaneio->zif_cadastro~set_registro( i_id_registro = wa_romaneio-ch_referencia ).
                    ob_romaneio->zif_cadastro~excluir_registro( ).
                    CLEAR ob_romaneio.
                  ENDIF.
                  me->ck_alterou = abap_true.
                  CLEAR: <fs_pedido>-ch_referencia_sai, <fs_pedido>-nr_romaneio_sai.
                ENDIF.

              ENDLOOP.

            ENDIF.

            DATA: number           TYPE tbtcjob-jobcount,
                  name             TYPE tbtcjob-jobname,
                  print_parameters TYPE pri_params.

            DATA(lc_user_job) = zcl_job=>get_user_job( ).

            CONCATENATE 'JOB_ENTRADA_ESTORNO' me->carga-id_carga INTO name.

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

              SUBMIT zmmr020 TO SAP-SPOOL SPOOL PARAMETERS print_parameters WITHOUT SPOOL DYNPRO VIA JOB name NUMBER number
                WITH pobjkey EQ <entrada>-obj_key
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

            me->zif_carga~set_processar_estorno( IMPORTING e_estornou = e_estornou ).

          ENDIF.

        ENDIF.
      ENDLOOP.
    ELSE.
      e_estornou = abap_true.
    ENDIF.

    "Comentado
    "BREAK-POINT.
*    IF IT_ESTORNO[] IS NOT INITIAL.
*      MODIFY ZMMT_EEE_ZGR FROM TABLE IT_ESTORNO.
*    ENDIF.

*    IF IT_ENTRADA[] IS NOT INITIAL.
*      MODIFY ZMMT_EE_ZGR  FROM TABLE IT_ENTRADA.
*    ENDIF.

*    IF ME->CK_ALTEROU EQ ABAP_TRUE.
*      ME->GRAVAR_REGISTRO( ).
*    ELSE.
*      COMMIT WORK.
*    ENDIF.

    LOOP AT it_entrada INTO wa_entrada.
      me->desbloquear_entrada( i_obj_key = wa_entrada-obj_key ).
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_PROCESSAR_ESTORNO_SOBRA.

    DATA: NUMBER           TYPE TBTCJOB-JOBCOUNT,
          NAME             TYPE TBTCJOB-JOBNAME,
          PRINT_PARAMETERS TYPE PRI_PARAMS.


    R_INSTANCE = ME.

    CHECK ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_FALSE.

    "Verificar se é entrada FOB
    CHECK ME->CARGA-TP_FRETE EQ ZIF_CARGA=>ST_TP_FRETE_FOB.

    ME->ZIF_CARGA~GET_RECUPERAR_ENTRADA( ).

    DATA(LC_USER_JOB) = ZCL_JOB=>GET_USER_JOB( ).

    LOOP AT ME->DOCUMENTO_FISCAL ASSIGNING FIELD-SYMBOL(<FS_ENTRADA>)  WHERE MM_MBLNR_SOBRA IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(WA_DOCUMENTOS)
        FROM ZMMT_EE_ZGR_DOCS
       WHERE OBJ_KEY EQ @<FS_ENTRADA>-OBJ_KEY_ENTRADA.

      IF SY-SUBRC IS INITIAL AND
         WA_DOCUMENTOS-MM_MBLNR_SOBRA IS INITIAL AND
         WA_DOCUMENTOS-MM_MJAHR_SOBRA IS INITIAL.
        CLEAR:
        <FS_ENTRADA>-MM_MBLNR_SOBRA,
        <FS_ENTRADA>-MM_MJAHR_SOBRA.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * INTO @DATA(WA_MKPF_SOBRA_EST)
        FROM MSEG
       WHERE SMBLN EQ @<FS_ENTRADA>-MM_MBLNR_SOBRA.

      IF SY-SUBRC IS INITIAL.
        CONTINUE.
      ENDIF.

      CONCATENATE 'JOB_SOBRA_ESTORNO' ME->CARGA-ID_CARGA INTO NAME SEPARATED BY '_'.

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          JOBNAME          = NAME
        IMPORTING
          JOBCOUNT         = NUMBER
        EXCEPTIONS
          CANT_CREATE_JOB  = 1
          INVALID_JOB_DATA = 2
          JOBNAME_MISSING  = 3
          OTHERS           = 4.

      IF SY-SUBRC = 0.

        IF I_DT_ESTORNO IS NOT INITIAL.
          DATA(LC_DT_ESTORNO) = ZCL_MIRO=>VERIFICAR_CRIAR( I_DATA = I_DT_ESTORNO I_BUKRS = ME->CARGA-ID_BUKRS ).

          DATA(LC_DT_RETORNO) = ZCL_MIRO=>GET_MES_PERMITIDO_FECHAMENTO( EXPORTING I_DATA = LC_DT_ESTORNO I_USER =  LC_USER_JOB ).
          IF LC_DT_ESTORNO NE LC_DT_RETORNO.
            LC_DT_ESTORNO = LC_DT_RETORNO(6) && '01'.
          ENDIF.

        ELSE.
          LC_DT_ESTORNO = SY-DATUM.
        ENDIF.

        SUBMIT ZMMR130 TO SAP-SPOOL SPOOL PARAMETERS PRINT_PARAMETERS WITHOUT SPOOL DYNPRO VIA JOB NAME NUMBER NUMBER
          WITH POBJKEY  EQ <FS_ENTRADA>-OBJ_KEY_ENTRADA
          WITH PESTORN  EQ ABAP_TRUE
          WITH DESTORNO EQ LC_DT_ESTORNO
          USER LC_USER_JOB
           AND RETURN.

        IF SY-SUBRC = 0.
          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              JOBCOUNT             = NUMBER
              JOBNAME              = NAME
              STRTIMMED            = 'X'
            EXCEPTIONS
              CANT_START_IMMEDIATE = 1
              INVALID_STARTDATE    = 2
              JOBNAME_MISSING      = 3
              JOB_CLOSE_FAILED     = 4
              JOB_NOSTEPS          = 5
              JOB_NOTEX            = 6
              LOCK_FAILED          = 7
              OTHERS               = 8.
        ENDIF.
      ENDIF.

      "Aguardar execução do job
      ZCL_JOB=>GET_INSTANCE(
       )->SET_KEY_JOB( I_JOBNAME = NAME I_JOBCOUNT = NUMBER
       )->GET_WAIT_JOB_EXEC(
       ).

      SELECT SINGLE * INTO WA_DOCUMENTOS
        FROM ZMMT_EE_ZGR_DOCS
       WHERE OBJ_KEY EQ <FS_ENTRADA>-OBJ_KEY_ENTRADA.

      IF SY-SUBRC IS INITIAL AND
         WA_DOCUMENTOS-MM_MBLNR_SOBRA IS INITIAL AND
         WA_DOCUMENTOS-MM_MJAHR_SOBRA IS INITIAL.
        CLEAR:
        <FS_ENTRADA>-MM_MBLNR_SOBRA,
        <FS_ENTRADA>-MM_MJAHR_SOBRA.
        CONTINUE.
      ELSE.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NAO_ESTORNOU_SOBRA-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_NAO_ESTORNOU_SOBRA-MSGNO
                              ATTR1 = CONV #( WA_DOCUMENTOS-DOCNUM ) )
            MSGTY  = 'E'
            MSGID  = ZCX_CARGA=>ZCX_NAO_ESTORNOU_SOBRA-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_NAO_ESTORNOU_SOBRA-MSGNO
            MSGV1  = CONV #( WA_DOCUMENTOS-DOCNUM ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  method zif_carga~set_processar_manutencao.

    data: ck_altera_romaneio    type char01 value abap_false,
          ck_estorna_miro       type char01 value abap_false,
          ck_estorna_migo       type char01 value abap_false,
          ck_estorna_migo_sobra type char01 value abap_false,
          ck_gera_carta_cte     type char01 value abap_false,
          ck_gera_carta_nfe     type char01 value abap_false,
          at_carga_original     type ref to zif_carga,
          at_romaneio           type ref to zcl_romaneio,
          lc_notas_removidas    type zde_zsdt0001nt_t.

    select single * into @data(wa_zsdt0001acb)
      from zsdt0001acb
     where id_solicitacao eq @me->zif_carga~solicitacao_manutencao-id_solicitacao.

    at_carga_original =
    zcl_factory_carga=>zif_factory_carga~get_instance(
      )->set_factory_objeto(
      exporting
        i_tp_carga      = me->carga-tp_carga
        i_tp_produto    = me->carga-tp_produto_carga
      )->get_factory_objeto(
      ).

    at_carga_original->set_registro( i_id_carga = me->carga-id_carga i_no_enqueue = abap_true ).
    at_carga_original->solicitacao_manutencao = me->zif_carga~solicitacao_manutencao.

    at_carga_original->ck_executar_manutencao_entrada = abap_true.

***///CS2021000581 Classificação destino - Flag para marcação - ZMM0127 - Set 2021 - Inicio
    select single * into @data(wa_mara)
         from mara
        where matnr eq @me->carga-id_produto.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_mara-matkl
      importing
        output = wa_mara-matkl.
    if me->classificacao-id_classificadora is initial and zif_carga=>st_grupo_algodao_pluma ne wa_mara-matkl.
      at_carga_original->classificacao-ck_class_dest = 'X'.
    endif.
***///CS2021000581 Classificação destino - Flag para marcação - ZMM0127 - Set 2021 - Fim

    "Deve Ser Emitido uma Carta de Correção para Alteração de Placa
    if me->carga-ds_placa_trator <> at_carga_original->carga-ds_placa_trator.
      ck_altera_romaneio = abap_true.
      ck_gera_carta_cte  = abap_true.
      ck_gera_carta_nfe  = abap_true.
    endif.

    "Deve Ser Emitido uma Carta de Correção para Alteração de Proprietário (NF-e/CT-e)
    "Se For CIF o RNTRC é do Terceiro
    if me->carga-id_proprietario <> at_carga_original->carga-id_proprietario
      and at_carga_original->at_tipo_frete_ordem_venda eq zif_carga=>st_tp_frete_cif.
      ck_gera_carta_cte  = abap_true.
    endif.

    "Deve Ser Emitido uma Carta de Correção para Alteração de Motorista (CT-e)
    "Se For CIF o RNTRC é do Terceiro
    if me->carga-id_motorista <> at_carga_original->carga-id_motorista.
      ck_altera_romaneio = abap_true.
      ck_gera_carta_cte  = abap_true.
    endif.

    "Deve Ser Verificação do Fluxo de Pagamento de Classificação
    if me->classificacao-id_classificadora <> at_carga_original->classificacao-id_classificadora.
      ck_altera_romaneio = abap_true.
    endif.

    if me->classificacao-nr_resultado_01 <> at_carga_original->classificacao-nr_resultado_01.
      ck_altera_romaneio = abap_true.
    endif.

    if me->classificacao-nr_resultado_02 <> at_carga_original->classificacao-nr_resultado_02.
      ck_altera_romaneio = abap_true.
    endif.

    if me->classificacao-nr_res_rr1_rr2 <> at_carga_original->classificacao-nr_res_rr1_rr2.
      ck_altera_romaneio = abap_true.
    endif.

    if me->classificacao-tp_transgenia <> at_carga_original->classificacao-tp_transgenia.
      ck_altera_romaneio = abap_true.
    endif.

    if me->classificacao-id_outro_partic <> at_carga_original->classificacao-id_outro_partic.
      ck_altera_romaneio = abap_true.
    endif.

    "US 143677 - transgenia por nota
    loop at me->classificacao_notas into data(wclass).
      read table at_carga_original->classificacao_notas into data(wclass_ori) with key id_classificacao = wclass-id_classificacao.
      if sy-subrc = 0.
        if wclass-nr_resultado_01 <> wclass_ori-nr_resultado_01.
          ck_altera_romaneio = abap_true.
        endif.

        if wclass-nr_resultado_02 <> wclass_ori-nr_resultado_02.
          ck_altera_romaneio = abap_true.
        endif.

        if wclass-nr_res_rr1_rr2 <> wclass_ori-nr_res_rr1_rr2.
          ck_altera_romaneio = abap_true.
        endif.

        if wclass-tp_transgenia <> wclass_ori-tp_transgenia.
          ck_altera_romaneio = abap_true.
        endif.

        if wclass-id_outro_partic <> wclass_ori-id_outro_partic.
          ck_altera_romaneio = abap_true.
        endif.
      endif.
    endloop.

    "Se Alterou Ticket
    if me->carga-nr_ticket <> at_carga_original->carga-nr_ticket.
      ck_altera_romaneio = abap_true.
    endif.

    "Alterou a SOBRA
    if me->carga-nm_peso_subtotal ne at_carga_original->carga-nm_peso_subtotal.
      ck_gera_carta_cte     = abap_true.
      ck_estorna_migo_sobra = abap_true.
    endif.

    "Verifica Alteração de Classificação
    loop at me->resultado into data(wa_resultado).
      read table at_carga_original->resultado into data(wa_res_original)
      with key tp_caracteristica = wa_resultado-tp_caracteristica.
      if sy-subrc is initial and wa_resultado-nr_percentual_com ne wa_res_original-nr_percentual_com.
        ck_altera_romaneio = abap_true.
      endif.
    endloop.

    "Verifica Alteração de Sub Característica Avariado
    loop at me->zif_carga~resultado_avariado into data(wa_resultado_ava).
      read table at_carga_original->resultado_avariado into data(wa_res_original_ava)
      with key tp_sub_carac_avariado = wa_resultado_ava-tp_sub_carac_avariado.
      if sy-subrc is initial and wa_resultado_ava-nr_percentual_com ne wa_res_original_ava-nr_percentual_com.
        ck_altera_romaneio = abap_true.
      endif.
    endloop.

    loop at me->documento_fiscal into data(wa_documento_fiscal).
      read table at_carga_original->documento_fiscal with key id_carga = wa_documento_fiscal-id_carga
                                                                         id_nota  = wa_documento_fiscal-id_nota
                                                               into data(wa_doc_original).
      if sy-subrc is initial.
        if wa_documento_fiscal-id_entrada ne wa_doc_original-id_entrada.
          ck_altera_romaneio = abap_true.
          ck_estorna_miro    = abap_true.
          ck_estorna_migo    = abap_true.
          ck_estorna_migo_sobra = abap_true.
        endif.
        if wa_documento_fiscal-id_fornecedor ne wa_doc_original-id_fornecedor.
          ck_altera_romaneio = abap_true.
          ck_estorna_miro    = abap_true.
          ck_estorna_migo    = abap_true.
          ck_estorna_migo_sobra = abap_true.
        endif.
        if wa_documento_fiscal-id_mod_fiscal ne wa_doc_original-id_mod_fiscal.
          ck_altera_romaneio = abap_true.
          ck_estorna_miro    = abap_true.
          ck_estorna_migo    = abap_true.
          ck_estorna_migo_sobra = abap_true.
        endif.
        if wa_documento_fiscal-nr_nota ne wa_doc_original-nr_nota.
          ck_altera_romaneio = abap_true.
          ck_estorna_miro    = abap_true.
          ck_estorna_migo    = abap_true.
          ck_estorna_migo_sobra = abap_true.
        endif.
        if wa_documento_fiscal-nm_serie ne wa_doc_original-nm_serie.
          ck_altera_romaneio = abap_true.
          ck_estorna_miro    = abap_true.
          ck_estorna_migo    = abap_true.
          ck_estorna_migo_sobra = abap_true.
        endif.
        if wa_documento_fiscal-nr_quantidade ne wa_doc_original-nr_quantidade.
          ck_altera_romaneio = abap_true.
          ck_estorna_miro    = abap_true.
          ck_estorna_migo    = abap_true.
          ck_estorna_migo_sobra = abap_true.
        endif.
        if wa_documento_fiscal-nr_valor ne wa_doc_original-nr_valor.
          ck_altera_romaneio = abap_true.
          ck_estorna_miro    = abap_true.
          ck_estorna_migo    = abap_true.
          ck_estorna_migo_sobra = abap_true.
        endif.
        if wa_documento_fiscal-dt_emissao ne wa_doc_original-dt_emissao.
          ck_altera_romaneio = abap_true.
          ck_estorna_miro    = abap_true.
          ck_estorna_migo    = abap_true.
          ck_estorna_migo_sobra = abap_true.
        endif.
        if wa_documento_fiscal-cfop ne wa_doc_original-cfop.
          ck_altera_romaneio = abap_true.
          ck_estorna_miro    = abap_true.
          ck_estorna_migo    = abap_true.
          ck_estorna_migo_sobra = abap_true.
        endif.
        if wa_documento_fiscal-nr_chave_nfe ne wa_doc_original-nr_chave_nfe.
          ck_altera_romaneio = abap_true.
          ck_estorna_miro    = abap_true.
          ck_estorna_migo    = abap_true.
          ck_estorna_migo_sobra = abap_true.
        endif.
        if wa_documento_fiscal-id_entregue_por ne wa_doc_original-id_entregue_por.
          ck_altera_romaneio = abap_true.
        endif.

        if wa_documento_fiscal-docnum is not initial. "Documento Estornado e não gerado
          select single *
            from j_1bnfdoc into @data(lwa_j_1bnfdoc_new)
           where docnum eq @wa_documento_fiscal-docnum.

          if ( sy-subrc eq 0 ) and ( lwa_j_1bnfdoc_new-candat is not initial ).
            ck_altera_romaneio = abap_true.
            ck_estorna_miro    = abap_true.
            ck_estorna_migo    = abap_true.
            ck_estorna_migo_sobra = abap_true.
          endif.
        endif.

      endif.
    endloop.

    "Altera Algum Peso de Desconto
    if me->carga-nm_peso_liquido ne at_carga_original->carga-nm_peso_liquido.
      ck_altera_romaneio    = abap_true.
      ck_estorna_migo_sobra = abap_true.
    endif.

    data: i_nota type zde_zsdt0001nt_alv.

    data: lc_dt_estorno type datum.
    lc_dt_estorno = at_carga_original->carga-dt_movimento.

    describe table at_carga_original->documento_fiscal lines data(qtd_fiscal_original).
    describe table me->documento_fiscal lines data(qtd_fiscal_corre).

    if qtd_fiscal_original ne qtd_fiscal_corre.
      ck_altera_romaneio    = abap_true.
      ck_estorna_migo       = abap_true.
      ck_estorna_migo_sobra = abap_true.
      ck_estorna_miro       = abap_true.
    endif.

    if ck_altera_romaneio    eq abap_true or
       ck_estorna_migo       eq abap_true or
       ck_estorna_migo_sobra eq abap_true or
       ck_estorna_miro       eq abap_true.

      if ck_estorna_migo eq abap_true or
         ck_estorna_miro eq abap_true.

        "Verificar se as Notas podem ser estornadas """""""""""""""""""""""""""""""""""""""""""""""""
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        data(notas) = at_carga_original->documento_fiscal[].
        delete notas where docnum is initial.
        if notas[] is not initial.
          select * into table @data(it_exportacao)
            from zdoc_nf_produtor
             for all entries in @notas
           where docnum_prod eq @notas-docnum.
        endif.

        if it_exportacao[] is not initial.
          read table it_exportacao into data(wa_exportacao) index 1.

          raise exception type zcx_carga
            exporting
              textid = value #( msgid = zcx_carga=>zcx_nf_exportada-msgid
                                msgno = zcx_carga=>zcx_nf_exportada-msgno
                                attr1 = wa_exportacao-docnum_prod
                                attr2 = wa_exportacao-vbeln )
              msgty  = 'E'
              msgid  = zcx_carga=>zcx_nf_exportada-msgid
              msgno  = zcx_carga=>zcx_nf_exportada-msgno
              msgv1  = conv #( wa_exportacao-docnum_prod )
              msgv2  = conv #( wa_exportacao-vbeln ).
        endif.

        "Verificar Data do Estorno """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        try.
            data(lc_user_job) = zcl_job=>get_user_job( ).
            lc_dt_estorno = zcl_miro=>verificar_criar( i_data = lc_dt_estorno i_bukrs = at_carga_original->carga-id_bukrs ).
          catch zcx_miro_exception into data(ex_miro).
        endtry.

        data(lc_dt_retorno) = zcl_miro=>get_mes_permitido_fechamento( exporting i_data = lc_dt_estorno i_user =  lc_user_job ).
        if lc_dt_estorno ne lc_dt_retorno.
          lc_dt_estorno = lc_dt_retorno(6) && '01'.
        endif.

        if lc_dt_estorno(6) ne sy-datum(6). "Ano/Mes estorno diferente do Ano/Mes Atual
          lc_dt_estorno = sy-datum(6) && '01'. "Estorna no primeiro dia do mes atual
        endif.

        loop at me->documento_fiscal into wa_documento_fiscal.
          if lc_dt_estorno lt wa_documento_fiscal-dt_emissao.
            lc_dt_estorno = wa_documento_fiscal-dt_emissao.
          endif.
        endloop.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      endif.

      if ck_estorna_migo_sobra eq abap_true.
        at_carga_original->set_processar_estorno_sobra( i_dt_estorno = lc_dt_estorno ).
      endif.

      if ( ck_estorna_migo eq abap_true or ck_estorna_miro eq abap_true ) and ( wa_zsdt0001acb-ck_entrada_estornada eq abap_false ).

*-CS2021000183-#83755-19.07.2022-JT -inicio-Comentado
*-CS2021000183 - 05.04.2022 - JT - inicio
*       TRY.
*           me->zif_carga~set_cancelar_nota_propria( ).
*         CATCH zcx_error INTO DATA(ex_error).
*       ENDTRY.
*-CS2021000183 - 05.04.2022 - JT - fim
*-CS2021000183-#83755-19.07.2022-JT -inicio-fim

        at_carga_original->set_gerar_estorno_estoque( i_dt_estorno = lc_dt_estorno
           )->set_processar_estorno( importing e_estornou = data(e_estornou)
           )->set_cancelar_nota_propria(  "CS2021000183-#83755-19.07.2022-JT
           ).

        if e_estornou eq abap_true.
          update zsdt0001acb
             set ck_entrada_estornada = abap_true
           where id_solicitacao = me->zif_carga~solicitacao_manutencao-id_solicitacao.

          commit work and wait.
        endif.

      endif.

      "Alterar Carga Original
      at_carga_original->carga-id_ordem	                    = me->carga-id_ordem         . "Ordem de Carregamento
      at_carga_original->carga-id_local_entrega	            = me->carga-id_local_entrega . "Id. Local de Entrega
      at_carga_original->carga-dt_movimento                 = lc_dt_estorno.
      at_carga_original->carga-nr_safra                     = me->carga-nr_safra         . "Safra
      at_carga_original->carga-id_bukrs	                    = me->carga-id_bukrs         . "Empresa Recebimento
      at_carga_original->carga-id_branch                    = me->carga-id_branch        . "Filial de Recebimento
      at_carga_original->carga-id_agent_frete               = me->carga-id_agent_frete   . "Agente de Frete
      at_carga_original->carga-id_local_coleta              = me->carga-id_local_coleta  . "Local de Coleta
      at_carga_original->carga-id_local_destino             = me->carga-id_local_destino . "Local de Destino
      at_carga_original->carga-id_local_descarga            = me->carga-id_local_descarga. "Local de Descarga/Transbordo
      at_carga_original->carga-tp_frete                     = me->carga-tp_frete         . "Incoterms
      at_carga_original->carga-id_produto                   = me->carga-id_produto       . "Nº do material
      at_carga_original->carga-nm_peso_bruto                = me->carga-nm_peso_bruto    . "Peso Bruto do Caminhão
      at_carga_original->carga-nm_peso_tara                 = me->carga-nm_peso_tara     . "Peso Tara do Caminhão
      at_carga_original->carga-nm_peso_subtotal             = me->carga-nm_peso_subtotal . "Peso SubTotal do Caminhão
      at_carga_original->carga-nm_peso_descontos            = me->carga-nm_peso_descontos. "Peso Descontos
      at_carga_original->carga-ds_placa_trator              = me->carga-ds_placa_trator  . "Placa Veículo Tranção
      at_carga_original->carga-ds_placa_reboq_1             = me->carga-ds_placa_reboq_1 . "Placa Veículo Reboque 1
      at_carga_original->carga-ds_placa_reboq_2             = me->carga-ds_placa_reboq_2 . "Placa Veículo Reboque 2
      at_carga_original->carga-ds_placa_reboq_3             = me->carga-ds_placa_reboq_3 . "Placa Veículo Reboque 3
      at_carga_original->carga-id_proprietario              = me->carga-id_proprietario  . "Proprietário do Veículo
      at_carga_original->carga-id_motorista                 = me->carga-id_motorista     . "Motorista
      at_carga_original->carga-nr_ticket                    = me->carga-nr_ticket        . "Número do Ticket
      at_carga_original->carga-in_transferencia             = me->carga-in_transferencia . "Indicador de Transferência
      at_carga_original->carga-tp_status                    = me->carga-tp_status        . "Status da Carga
      at_carga_original->carga-nm_peso_liquido              = me->carga-nm_peso_liquido  . "Peso Liquido
      at_carga_original->carga-ck_gera_aviso                = me->carga-ck_gera_aviso    . "Gera Aviso de Recebimento

      "Alterar Classificação Original
      at_carga_original->classificacao-nr_resultado_01      = me->classificacao-nr_resultado_01.
      at_carga_original->classificacao-nr_resultado_02      = me->classificacao-nr_resultado_02.
      at_carga_original->classificacao-nr_res_rr1_rr2       = me->classificacao-nr_res_rr1_rr2.
      at_carga_original->classificacao-in_gmo_03            = me->classificacao-in_gmo_03.
      at_carga_original->classificacao-in_gmo               = me->classificacao-in_gmo.
      at_carga_original->classificacao-in_srr_origem_partic = me->classificacao-in_srr_origem_partic.
      at_carga_original->classificacao-id_outro_partic      = me->classificacao-id_outro_partic.
      at_carga_original->classificacao-in_srr_declarado     = me->classificacao-in_srr_declarado.
      at_carga_original->classificacao-in_teste_srr         = me->classificacao-in_teste_srr.
      at_carga_original->classificacao-in_srr_declarado_2   = me->classificacao-in_srr_declarado_2.
      at_carga_original->classificacao-in_teste_srr_2       = me->classificacao-in_teste_srr_2.
      at_carga_original->classificacao-id_classificadora    = me->classificacao-id_classificadora.
      at_carga_original->classificacao-ck_class_dest        = me->classificacao-ck_class_dest.
      at_carga_original->classificacao-tp_transgenia        = me->classificacao-tp_transgenia.

      "US 143677 - transgenia por nota
      loop at me->classificacao_notas into wclass.
        read table at_carga_original->classificacao_notas assigning field-symbol(<wclass_origem>) with key id_classificacao = wclass-id_classificacao.
        if sy-subrc = 0.
          "Alterar Classificação Original
          <wclass_origem>-nr_resultado_01      = wclass-nr_resultado_01.
          <wclass_origem>-nr_resultado_02      = wclass-nr_resultado_02.
          <wclass_origem>-nr_res_rr1_rr2       = wclass-nr_res_rr1_rr2.
          <wclass_origem>-in_gmo_03            = wclass-in_gmo_03.
          <wclass_origem>-in_gmo               = wclass-in_gmo.
          <wclass_origem>-in_srr_origem_partic = wclass-in_srr_origem_partic.
          <wclass_origem>-id_outro_partic      = wclass-id_outro_partic.
          <wclass_origem>-in_srr_declarado     = wclass-in_srr_declarado.
          <wclass_origem>-in_teste_srr         = wclass-in_teste_srr.
          <wclass_origem>-in_srr_declarado_2   = wclass-in_srr_declarado_2.
          <wclass_origem>-in_teste_srr_2       = wclass-in_teste_srr_2.
          <wclass_origem>-id_classificadora    = wclass-id_classificadora.
          <wclass_origem>-ck_class_dest        = wclass-ck_class_dest.
          <wclass_origem>-tp_transgenia        = wclass-tp_transgenia.
        else.
                                                            "BUG174959
          append wclass to at_carga_original->classificacao_notas.
                                                            "BUG174959
        endif.
      endloop.
      "Alterar Ordem de Venda Original
      loop at me->zif_carga~ordem_venda into data(wa_ordem_venda).

        read table at_carga_original->ordem_venda
         assigning field-symbol(<fs_ordem>)
          with key id_carga = wa_ordem_venda-id_carga
                   nr_ordem_venda = wa_ordem_venda-nr_ordem_venda.

        move-corresponding wa_ordem_venda to <fs_ordem>.
      endloop.

      clear: at_carga_original->resultado[].

      "Atribui Classificação Aprovada a Carga Original
      at_carga_original->resultado[] = me->resultado[].
      loop at at_carga_original->resultado assigning field-symbol(<fs_resultado>) where id_classificacao = me->classificacao-id_classificacao.
        "Manter Id Classificação do Registro Antigo
        <fs_resultado>-id_classificacao = at_carga_original->classificacao-id_classificacao.
      endloop.

      clear: at_carga_original->resultado_avariado[].
      "Atribui Classificação Aprovada a Carga Original
      at_carga_original->resultado_avariado[] = me->zif_carga~resultado_avariado[].
      loop at at_carga_original->resultado_avariado assigning field-symbol(<fs_resultado_ava>) where id_classificacao = me->classificacao-id_classificacao.
        "Manter Id Classificação do Registro Antigo
        <fs_resultado_ava>-id_classificacao = at_carga_original->classificacao-id_classificacao.
      endloop.

      "Percorre notas da manutenção
      loop at me->documento_fiscal into data(doc_fiscal).

        "Percorre os resultados das notas da manutenção
        loop at at_carga_original->resultado assigning field-symbol(<fs_resultado_nota>) where id_classificacao = doc_fiscal-id_classificacao.

          "Busca o Documento do Original Equivalente
          read table at_carga_original->documento_fiscal assigning field-symbol(<fs_nota>)
            with key id_carga = doc_fiscal-id_carga
                     id_nota  = doc_fiscal-id_nota.
          if sy-subrc is initial.
            <fs_nota>-id_classificacao = <fs_resultado_nota>-id_classificacao.
          endif.
        endloop.

      endloop.

      "Alterar Nota Fiscal Original """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      "Nota fiscal da Manutenção, se não existir adiciona
      loop at me->documento_fiscal into wa_documento_fiscal.
        "Nota Fiscal Original
        read table at_carga_original->documento_fiscal into data(wa_nota_original)
         with key id_carga = wa_documento_fiscal-id_carga
                  id_nota  = wa_documento_fiscal-id_nota.

        clear: i_nota.
        if sy-subrc is not initial.
          move-corresponding wa_documento_fiscal to i_nota.
          clear: i_nota-id_nota.
          ck_estorna_migo = abap_true.
          ck_estorna_miro = abap_true.
        else.
          move-corresponding wa_nota_original to i_nota.
          i_nota-id_entrada          = wa_documento_fiscal-id_entrada.
          i_nota-id_mod_fiscal       = wa_documento_fiscal-id_mod_fiscal.
          i_nota-id_fornecedor       = wa_documento_fiscal-id_fornecedor.
          i_nota-nr_fornecedor_ie    = wa_documento_fiscal-nr_fornecedor_ie.
          i_nota-nr_nota             = wa_documento_fiscal-nr_nota.
          i_nota-nm_serie            = wa_documento_fiscal-nm_serie.
          i_nota-dt_emissao          = wa_documento_fiscal-dt_emissao.
          i_nota-nr_chave_nfe        = wa_documento_fiscal-nr_chave_nfe.
          i_nota-nr_quantidade       = wa_documento_fiscal-nr_quantidade.
          i_nota-nr_valor            = wa_documento_fiscal-nr_valor.
          i_nota-dt_vencimento_form  = wa_documento_fiscal-dt_vencimento_form.
          i_nota-nr_preco_saca_60    = wa_documento_fiscal-nr_preco_saca_60.
          i_nota-obj_key_entrada     = wa_documento_fiscal-obj_key_entrada.
          i_nota-nm_peso_subtotal    = wa_documento_fiscal-nm_peso_subtotal.
          i_nota-nm_peso_liquido     = wa_documento_fiscal-nm_peso_liquido.
          i_nota-id_classificacao    = wa_nota_original-id_classificacao.
          i_nota-cfop                = wa_documento_fiscal-cfop.
          i_nota-id_entregue_por     = wa_documento_fiscal-id_entregue_por.
          i_nota-ds_observacao       = wa_documento_fiscal-ds_observacao.
        endif.
        at_carga_original->add_nota_fiscal( exporting i_nota = i_nota ).
      endloop.

      "Remover Nota Fiscal e Cancelar Romaneio de Entrada 'Avulso'

      loop at at_carga_original->documento_fiscal into wa_nota_original.
        read table me->documento_fiscal transporting no fields
          with key id_carga = wa_nota_original-id_carga
                   id_nota  = wa_nota_original-id_nota.

        if sy-subrc is not initial.
          append wa_nota_original to lc_notas_removidas.
        endif.
      endloop.

      loop at lc_notas_removidas into wa_nota_original.
        if wa_nota_original-ch_referencia_ent is not initial.
          zcl_romaneio=>set_cancelar_romaneio( wa_nota_original-ch_referencia_ent ).
        endif.
        clear: i_nota.
        move-corresponding wa_nota_original to i_nota.
        at_carga_original->excluir_nota_fiscal( exporting i_nota = i_nota ).
      endloop.

      "Alterar Peso Conferido da Nota Fiscal Original
      loop at me->documento_fiscal into wa_documento_fiscal.

        read table at_carga_original->documento_fiscal into wa_nota_original
         with key id_carga = wa_documento_fiscal-id_carga
                  id_nota  = wa_documento_fiscal-id_nota.

        at_carga_original->set_pesos_notas(
          exporting
            i_id_carga      = wa_nota_original-id_carga            " Id. da Carga
            i_id_nota       = wa_nota_original-id_nota             " Id. Nota Fiscal
            i_peso_subtotal = wa_documento_fiscal-nm_peso_subtotal " Peso SubTotal do Caminhão
            i_peso_liquido  = wa_documento_fiscal-nm_peso_liquido  " Peso Líquido
        ).
      endloop.

    endif.

    if ck_estorna_migo       eq abap_true or
       ck_estorna_migo_sobra eq abap_true or
       ck_estorna_miro       eq abap_true.

      at_carga_original->set_conferido( importing e_conferiu = data(e_conferiu) ).

      if e_conferiu eq abap_false.
        message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into data(i_texto).
        me->zif_carga~gera_erro_geral( exporting i_texto = i_texto ).
      endif.

    else.

      at_carga_original->set_gerar_romaneio_entrada(
       )->set_gerar_romaneio_saida(
       )->send_carga_to_opus(
       ).

      at_carga_original->ck_alterou = abap_true.
      at_carga_original->gravar_registro( ).
    endif.

  endmethod.


  METHOD ZIF_CARGA~SET_PROCESSAR_SOBRA_FOB.

    R_INSTANCE = ME.

    CHECK ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_FALSE.

    "Verificar se é entrada FOB
    CHECK ME->CARGA-TP_FRETE EQ ZIF_CARGA=>ST_TP_FRETE_FOB.

    CHECK NOT ( ME->CARGA-CK_ENVIADO_OPUS EQ ABAP_TRUE AND ME->CARGA-CK_RECEBIDO_OPUS EQ ABAP_FALSE ).

    ME->ZIF_CARGA~GET_CK_GERA_SOBRA( IMPORTING E_ZMMT0074 = DATA(E_ZMMT0074) ).

    CHECK E_ZMMT0074 IS NOT INITIAL.

    DATA: NUMBER           TYPE TBTCJOB-JOBCOUNT,
          NAME             TYPE TBTCJOB-JOBNAME,
          PRINT_PARAMETERS TYPE PRI_PARAMS.

    DATA(LC_USER_JOB) = ZCL_JOB=>GET_USER_JOB( ).

    LOOP AT ME->DOCUMENTO_FISCAL ASSIGNING FIELD-SYMBOL(<ENTRADA>) WHERE OBJ_KEY_ENTRADA IS NOT INITIAL.

      CHECK <ENTRADA>-NM_PESO_LIQUIDO <> <ENTRADA>-NM_PESO_SUBTOTAL.

      SELECT SINGLE * INTO @DATA(WA_DOCUMENTOS)
        FROM ZMMT_EE_ZGR_DOCS
       WHERE OBJ_KEY EQ @<ENTRADA>-OBJ_KEY_ENTRADA.

      IF SY-SUBRC IS INITIAL AND
         WA_DOCUMENTOS-MM_MBLNR_SOBRA IS NOT INITIAL AND
         WA_DOCUMENTOS-MM_MJAHR_SOBRA IS NOT INITIAL.
        <ENTRADA>-MM_MBLNR_SOBRA = WA_DOCUMENTOS-MM_MBLNR_SOBRA.
        <ENTRADA>-MM_MJAHR_SOBRA = WA_DOCUMENTOS-MM_MJAHR_SOBRA.
        CONTINUE.
      ENDIF.

      IF 1 EQ 2.
        SUBMIT ZMMR130 WITH POBJKEY EQ <ENTRADA>-OBJ_KEY_ENTRADA AND RETURN.
      ELSE.
        CONCATENATE 'JOB_SOBRA' ME->CARGA-ID_CARGA INTO NAME SEPARATED BY '_'.

        CALL FUNCTION 'JOB_OPEN'
          EXPORTING
            JOBNAME          = NAME
          IMPORTING
            JOBCOUNT         = NUMBER
          EXCEPTIONS
            CANT_CREATE_JOB  = 1
            INVALID_JOB_DATA = 2
            JOBNAME_MISSING  = 3
            OTHERS           = 4.

        IF SY-SUBRC = 0.

          DATA(LC_QTD_SOBRA) = <ENTRADA>-NM_PESO_SUBTOTAL - <ENTRADA>-NM_PESO_LIQUIDO.

          DATA(LC_DT_ENTRADA) = ZCL_MIRO=>VERIFICAR_CRIAR( I_DATA = ME->CARGA-DT_MOVIMENTO I_BUKRS = ME->CARGA-ID_BUKRS ).

          DATA(LC_DT_RETORNO) = ZCL_MIRO=>GET_MES_PERMITIDO_FECHAMENTO( EXPORTING I_DATA = LC_DT_ENTRADA I_USER =  LC_USER_JOB ).
          IF LC_DT_ENTRADA NE LC_DT_RETORNO.
            LC_DT_ENTRADA = LC_DT_RETORNO(6) && '01'.
          ENDIF.

          SUBMIT ZMMR130 TO SAP-SPOOL SPOOL PARAMETERS PRINT_PARAMETERS WITHOUT SPOOL DYNPRO VIA JOB NAME NUMBER NUMBER
            WITH POBJKEY  EQ <ENTRADA>-OBJ_KEY_ENTRADA
            WITH PQUANTI  EQ LC_QTD_SOBRA
            WITH DTENTRAD EQ LC_DT_ENTRADA
            USER LC_USER_JOB
             AND RETURN.

          IF SY-SUBRC = 0.
            CALL FUNCTION 'JOB_CLOSE'
              EXPORTING
                JOBCOUNT             = NUMBER
                JOBNAME              = NAME
                STRTIMMED            = 'X'
              EXCEPTIONS
                CANT_START_IMMEDIATE = 1
                INVALID_STARTDATE    = 2
                JOBNAME_MISSING      = 3
                JOB_CLOSE_FAILED     = 4
                JOB_NOSTEPS          = 5
                JOB_NOTEX            = 6
                LOCK_FAILED          = 7
                OTHERS               = 8.
          ENDIF.
        ENDIF.

        "Aguardar execução do job
        ZCL_JOB=>GET_INSTANCE(
         )->SET_KEY_JOB( I_JOBNAME = NAME I_JOBCOUNT = NUMBER
         )->GET_WAIT_JOB_EXEC(
         )->GET_LOG_JOB( IMPORTING E_LOGS = DATA(E_LOGS)
         ).
      ENDIF.
      SELECT SINGLE * INTO WA_DOCUMENTOS
        FROM ZMMT_EE_ZGR_DOCS
       WHERE OBJ_KEY EQ <ENTRADA>-OBJ_KEY_ENTRADA.

      IF SY-SUBRC IS INITIAL AND
         WA_DOCUMENTOS-MM_MBLNR_SOBRA IS NOT INITIAL AND
         WA_DOCUMENTOS-MM_MJAHR_SOBRA IS NOT INITIAL.
        <ENTRADA>-MM_MBLNR_SOBRA = WA_DOCUMENTOS-MM_MBLNR_SOBRA.
        <ENTRADA>-MM_MJAHR_SOBRA = WA_DOCUMENTOS-MM_MJAHR_SOBRA.
        ME->CK_ALTEROU = ABAP_TRUE.
      ELSE.

        READ TABLE E_LOGS WITH KEY MSGTYPE = 'E' INTO DATA(WA_LOGS).
        IF SY-SUBRC IS INITIAL.
          MESSAGE ID WA_LOGS-MSGID TYPE 'S' NUMBER WA_LOGS-MSGNO WITH WA_LOGS-MSGV1 WA_LOGS-MSGV2 WA_LOGS-MSGV3 WA_LOGS-MSGV4 INTO DATA(I_TEXTO).
          ME->GERA_ERRO_GERAL( I_TEXTO = I_TEXTO ).
        ENDIF.

        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NAO_GEROU_SOBRA-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_NAO_GEROU_SOBRA-MSGNO
                              ATTR1 = CONV #( WA_DOCUMENTOS-DOCNUM ) )
            MSGTY  = 'E'
            MSGID  = ZCX_CARGA=>ZCX_NAO_GEROU_SOBRA-MSGID
            MSGNO  = ZCX_CARGA=>ZCX_NAO_GEROU_SOBRA-MSGNO
            MSGV1  = CONV #( WA_DOCUMENTOS-DOCNUM ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  method zif_carga~set_registro.

    r_carga = me.

    me->limpar_registro( ).

    if me->carga-id_carga is not initial and i_no_enqueue eq abap_false.
      me->set_denqueue( i_carga = me->carga-id_carga ).
    endif.

    me->zif_carga~get_cabecalho_carga( exporting i_id_carga = i_id_carga importing e_zsdt0001cg = me->carga ).
    "US 143677 - transgenia por nota pega a classificacao da nota 1
    select single * into me->classificacao from zsdt0001cl where id_carga eq i_id_carga and id_classificacao = me->carga-id_classificacao.
    if sy-subrc ne 0.
      select single * into me->classificacao from zsdt0001cl where id_carga eq i_id_carga.
    endif.
    select * into table me->zif_carga~ordem_venda from zsdt0001ov where id_carga eq i_id_carga.
    select * into table me->zif_carga~pedido_compra from zsdt0001ek where id_carga eq i_id_carga.
    select * into table me->resultado from zsdt0001rs where id_carga eq i_id_carga and id_classificacao eq me->carga-id_classificacao.
    select * into table me->zif_carga~resultado_avariado from zsdt0001rs_03 where id_carga eq i_id_carga and id_classificacao eq me->carga-id_classificacao.
    select * into table me->documento_fiscal from zsdt0001nt where id_carga eq i_id_carga.
    select * into table me->documento_fiscal_imp_ret from zsdt0001nt_ret where id_carga eq i_id_carga.
    select * into table me->zif_carga~take_up from zsdt0001tk where id_carga eq i_id_carga.
    select * into table me->zif_carga~blocos from zsdt0001fd where id_carga eq i_id_carga.

    select * into table @data(it_zsdt0001acg)
      from zsdt0001acg
     where id_carga_origem eq @me->carga-id_carga.

    if sy-subrc is initial.
      select * into table me->zif_carga~solicitacoes
        from zsdt0001acb
         for all entries in it_zsdt0001acg
       where id_solicitacao eq it_zsdt0001acg-id_solicitacao.
    endif.

    select * appending table me->resultado
      from zsdt0001rs
       for all entries in me->documento_fiscal
     where id_carga         eq me->documento_fiscal-id_carga
       and id_classificacao eq me->documento_fiscal-id_classificacao.

    sort me->resultado by id_carga id_classificacao tp_caracteristica.
    delete adjacent duplicates from me->resultado comparing id_carga id_classificacao tp_caracteristica.

    select * appending table me->zif_carga~resultado_avariado
      from zsdt0001rs_03
       for all entries in me->documento_fiscal
     where id_carga         eq me->documento_fiscal-id_carga
       and id_classificacao eq me->documento_fiscal-id_classificacao.

    sort me->zif_carga~resultado_avariado by id_carga id_classificacao tp_sub_carac_avariado.
    delete adjacent duplicates from me->zif_carga~resultado_avariado comparing id_carga id_classificacao tp_sub_carac_avariado.

    select * into table me->classificacao_notas
      from zsdt0001cl
      for all entries in me->documento_fiscal
     where id_carga         eq i_id_carga
       and id_classificacao eq me->documento_fiscal-id_classificacao
       and id_classificacao ne me->carga-id_classificacao.

    sort me->classificacao_notas by id_carga id_classificacao.
    delete adjacent duplicates from me->classificacao_notas comparing id_carga id_classificacao.

    if i_no_enqueue eq abap_false.
      me->set_enqueue( i_carga = me->carga-id_carga ).
    endif.

    me->zif_carga~set_tipo_frete_ordem_venda( ).

    me->ck_alterou = abap_false.
    me->zif_carga~ck_digitado_umidade    = abap_true.
    me->zif_carga~ck_digitado_impureza   = abap_true.
    me->zif_carga~ck_digitado_ardido     = abap_true.
    me->zif_carga~ck_digitado_avariado   = abap_true.
    me->zif_carga~ck_digitado_quebrado   = abap_true.
    me->zif_carga~ck_digitado_esverdeado = abap_true.
    me->zif_carga~ck_digitado_carunchado = abap_true.

  endmethod.


  method zif_carga~set_registro_manutencao.

    data: wa_ordem         like line of me->zif_carga~ordem_venda,
          wa_pedido        like line of me->zif_carga~pedido_compra,
          wa_resultado     like line of me->zif_carga~resultado,
          wa_resultado_03  like line of me->zif_carga~resultado_avariado,
          wa_documento     like line of me->documento_fiscal,
          wa_takeup        like line of me->zif_carga~take_up,
          wa_bloco         like line of me->zif_carga~blocos,
          wa_documento_ret like line of me->documento_fiscal_imp_ret,
          wa_class         like line of me->classificacao_notas.

    r_carga = me.

    me->limpar_registro( ).

    "Busca Solicitação
    select single *
      into me->zif_carga~solicitacao_manutencao
      from zsdt0001acb
     where id_solicitacao eq i_id_solicitacao.

    "Busca Carga da Solicitação
    select single *
      into @data(wa_zsdt0001acg)
      from zsdt0001acg
     where id_solicitacao eq @i_id_solicitacao.

    if wa_zsdt0001acg-id_carga_origem is not initial.
      me->set_denqueue( i_carga = wa_zsdt0001acg-id_carga_origem ).
    endif.

    "Busca Classificação da Carga
    select single * into @data(wa_zsdt0001acl)
      from zsdt0001acl
     where id_solicitacao   eq @wa_zsdt0001acg-id_solicitacao
       and id_carga_origem  eq @wa_zsdt0001acg-id_carga_origem
       and id_classificacao eq @wa_zsdt0001acg-id_classificacao. "BUG174959

    "Busca Ordens de Venda da Carga
    select * into table @data(it_zsdt0001aov)
      from zsdt0001aov
     where id_solicitacao  eq @wa_zsdt0001acg-id_solicitacao
       and id_carga_origem eq @wa_zsdt0001acg-id_carga_origem.

    "Busca Pedidos de Compra da Carga
    select * into table @data(it_zsdt0001aek)
      from zsdt0001aek
     where id_solicitacao  eq @wa_zsdt0001acg-id_solicitacao
       and id_carga_origem eq @wa_zsdt0001acg-id_carga_origem.

    "Busca Resultado de Classificação da Carga
    select * into table @data(it_zsdt0001ars)
      from zsdt0001ars
     where id_solicitacao   eq @i_id_solicitacao
       and id_carga_origem  eq @wa_zsdt0001acg-id_carga_origem
       and id_classificacao eq @wa_zsdt0001acg-id_classificacao.

    "Busca Resultado da Sub Classificação do Avariado
    select * into table @data(it_zsdt0001ars_03)
      from zsdt0001ars_03
     where id_solicitacao   eq @i_id_solicitacao
       and id_carga_origem  eq @wa_zsdt0001acg-id_carga_origem
       and id_classificacao eq @wa_zsdt0001acg-id_classificacao.

    "Busca Notas da Carga
    select * into table @data(it_zsdt0001ant)
      from zsdt0001ant
     where id_solicitacao  eq @wa_zsdt0001acg-id_solicitacao
       and id_carga_origem eq @wa_zsdt0001acg-id_carga_origem.

    "Busca Take UP's da Notas da Carga
    select * into table @data(it_zsdt0001atk)
      from zsdt0001atk
     where id_solicitacao  eq @wa_zsdt0001acg-id_solicitacao
       and id_carga_origem eq @wa_zsdt0001acg-id_carga_origem.

    "Busca Notas da Carga - Impostos Retidos
    select * into table @data(it_zsdt0001ant_ret)
      from zsdt0001ant
     where id_solicitacao  eq @wa_zsdt0001acg-id_solicitacao
       and id_carga_origem eq @wa_zsdt0001acg-id_carga_origem.

    "Busca Resultado de Classificação das Notas
    select * appending table it_zsdt0001ars
      from zsdt0001ars
       for all entries in it_zsdt0001ant
     where id_solicitacao   eq it_zsdt0001ant-id_solicitacao
       and id_carga_origem  eq it_zsdt0001ant-id_carga_origem
       and id_classificacao eq it_zsdt0001ant-id_classificacao
       and id_classificacao ne wa_zsdt0001acg-id_classificacao.

    select * into table @data(it_zsdt0001acl)
      from zsdt0001acl
      for all entries in @it_zsdt0001ant
     where id_solicitacao   eq @wa_zsdt0001acg-id_solicitacao "BUG174959
       and id_carga_origem  eq @it_zsdt0001ant-id_carga_origem
       and id_classificacao eq @it_zsdt0001ant-id_classificacao
       and id_classificacao ne @wa_zsdt0001acg-id_classificacao.

    "no caso de gravar a classificacao sem o numero definitivo
    select * appending table it_zsdt0001acl
     from zsdt0001acl
    where id_solicitacao   eq wa_zsdt0001acg-id_solicitacao "BUG174959
      and id_carga_origem  eq wa_zsdt0001acg-id_carga_origem
      and exists ( select * from zsdt0001ant where nr_nota = zsdt0001acl~id_classificacao ).


    "Busca Blocos da Ordem de Venda
    select * into table @data(it_zsdt0001afd)
      from zsdt0001afd
     where id_solicitacao  eq @wa_zsdt0001acg-id_solicitacao
       and id_carga_origem eq @wa_zsdt0001acg-id_carga_origem.

    "Copia Carga
    move-corresponding wa_zsdt0001acg to me->carga.
    me->carga-id_carga = wa_zsdt0001acg-id_carga_origem.

    "Copia Classificação
    move-corresponding wa_zsdt0001acl to me->classificacao.
    me->classificacao-id_carga = wa_zsdt0001acg-id_carga_origem.

    "Copia Ordem de Venda
    loop at it_zsdt0001aov into data(wa_zsdt0001aov).
      clear wa_ordem.
      move-corresponding wa_zsdt0001aov to wa_ordem.
      wa_ordem-id_carga = wa_zsdt0001acg-id_carga_origem.
      append wa_ordem to me->zif_carga~ordem_venda.
    endloop.

    "Copia Pedido de Compra
    loop at it_zsdt0001aek into data(wa_zsdt0001aek).
      clear wa_pedido.
      move-corresponding wa_zsdt0001aek to wa_pedido.
      wa_pedido-id_carga = wa_zsdt0001acg-id_carga_origem.
      append wa_pedido to me->zif_carga~pedido_compra.
    endloop.

    "Copia de Resultados de Classificação
    loop at it_zsdt0001ars into data(wa_zsdt0001ars).
      clear wa_resultado.
      move-corresponding wa_zsdt0001ars to wa_resultado.
      wa_resultado-id_carga = wa_zsdt0001acg-id_carga_origem.
      append wa_resultado to me->zif_carga~resultado.
    endloop.

    "Copia de Resultados de Sub Classificação Avariado
    loop at it_zsdt0001ars_03 into data(wa_zsdt0001ars_03).
      clear wa_resultado_03.
      move-corresponding wa_zsdt0001ars_03 to wa_resultado_03.
      wa_resultado_03-id_carga = wa_zsdt0001acg-id_carga_origem.
      append wa_resultado_03 to me->zif_carga~resultado_avariado.
    endloop.

    "Copia de Notas
    loop at it_zsdt0001ant into data(wa_zsdt0001ant).
      clear wa_documento.
      move-corresponding wa_zsdt0001ant to wa_documento.
      wa_documento-id_carga = wa_zsdt0001ant-id_carga_origem.
      wa_documento-id_nota  = wa_zsdt0001ant-id_nota_origem.
      append wa_documento to me->documento_fiscal.
    endloop.

    "Copia Taku UP's das Notas
    loop at it_zsdt0001atk into data(wa_zsdt0001atk).
      clear: wa_takeup.
      move-corresponding wa_zsdt0001atk to wa_takeup.
      wa_takeup-id_carga  = wa_zsdt0001atk-id_carga_origem.
      wa_takeup-id_nota   = wa_zsdt0001atk-id_nota_origem.
      wa_takeup-id_takeup = wa_zsdt0001atk-id_takeup_origem.
      append wa_takeup to me->zif_carga~take_up.
    endloop.

    "Copia Blocos das Ordem de Venda / Pedidos de Compra
    loop at it_zsdt0001afd into data(wa_zsdt0001afd).
      clear: wa_bloco.
      move-corresponding wa_zsdt0001afd to wa_bloco.
      wa_bloco-id_carga  = wa_zsdt0001afd-id_carga_origem.
      append wa_bloco to me->zif_carga~blocos.
    endloop.

    "Copia Impostos da Notas
    loop at it_zsdt0001ant_ret into data(wa_zsdt0001ant_ret).
      clear: wa_documento_ret.
      move-corresponding wa_zsdt0001ant_ret to wa_documento_ret.
      wa_documento_ret-id_carga = wa_zsdt0001ant_ret-id_carga_origem.
      wa_documento_ret-id_nota  = wa_zsdt0001ant_ret-id_nota_origem.
      append wa_documento_ret to me->documento_fiscal_imp_ret.
    endloop.

    "Copia de Classificação
    loop at it_zsdt0001acl into wa_zsdt0001acl.
      clear: wa_class.
      move-corresponding wa_zsdt0001acl to wa_class.
      wa_class-id_carga = wa_zsdt0001acg-id_carga_origem.
      append wa_class to me->classificacao_notas.
    endloop.

    me->zif_carga~at_manutencao = abap_true.

    me->zif_carga~set_tipo_frete_ordem_venda( ).

    me->ck_alterou = abap_false.
    me->zif_carga~ck_digitado_umidade    = abap_true.
    me->zif_carga~ck_digitado_impureza   = abap_true.
    me->zif_carga~ck_digitado_ardido     = abap_true.
    me->zif_carga~ck_digitado_avariado   = abap_true.
    me->zif_carga~ck_digitado_quebrado   = abap_true.
    me->zif_carga~ck_digitado_esverdeado = abap_true.
    me->zif_carga~ck_digitado_carunchado = abap_true.

  endmethod.


  METHOD ZIF_CARGA~SET_TIPO_FRETE_ORDEM_VENDA.

    DATA: I_TEXTO TYPE STRING.

    DESCRIBE TABLE ME->ZIF_CARGA~ORDEM_VENDA LINES DATA(QT_ORDEM).

    "Pedido de Compra
    LOOP AT ME->ZIF_CARGA~PEDIDO_COMPRA INTO DATA(WA_PEDIDO).
      TRY .
          ME->ZIF_CARGA~GET_INFO_PEDIDO_COMPRA( EXPORTING I_PEDIDO_COMPRA = WA_PEDIDO-NR_PEDIDO_COMPRA IMPORTING E_PEDIDO = DATA(E_PEDIDO) ).
          ME->ZIF_CARGA~AT_TIPO_FRETE_ORDEM_VENDA = E_PEDIDO-DS_TIPO_FRETE.
        CATCH ZCX_CARGA INTO DATA(ZCX_CARGA).
          IF QT_ORDEM EQ 0.
            MESSAGE ID ZCX_CARGA->MSGID TYPE 'S' NUMBER ZCX_CARGA->MSGNO WITH ZCX_CARGA->MSGV1 ZCX_CARGA->MSGV2 ZCX_CARGA->MSGV3 ZCX_CARGA->MSGV4 INTO I_TEXTO.
            ME->GERA_ERRO_GERAL( I_TEXTO = I_TEXTO ).
          ENDIF.
      ENDTRY.
    ENDLOOP.

    "Ordem de Venda
    LOOP AT ME->ZIF_CARGA~ORDEM_VENDA INTO DATA(WA_ORDEM_VENDA).
      TRY .
          ZCL_ORDEM_VENDA=>ZIF_ORDEM_VENDA~GET_INSTANCE(
                   )->SET_ORDEM_VENDA( I_VBELN = WA_ORDEM_VENDA-NR_ORDEM_VENDA
                   )->GET_TIPO_FRETE( IMPORTING  E_TIPO_FRETE = ME->ZIF_CARGA~AT_TIPO_FRETE_ORDEM_VENDA
                   ).

        CATCH ZCX_ORDEM_VENDA INTO DATA(EX_ORDEM_VENDA).
          MESSAGE ID EX_ORDEM_VENDA->MSGID TYPE 'S'
           NUMBER EX_ORDEM_VENDA->MSGNO
             WITH EX_ORDEM_VENDA->MSGV1 EX_ORDEM_VENDA->MSGV2 EX_ORDEM_VENDA->MSGV3 EX_ORDEM_VENDA->MSGV4
             INTO I_TEXTO.
          ME->GERA_ERRO_GERAL( I_TEXTO = I_TEXTO ).
      ENDTRY.
    ENDLOOP.

    CASE ME->ZIF_CARGA~AT_TIPO_FRETE_ORDEM_VENDA.
      WHEN ZIF_CARGA=>ST_TP_FRETE_CIF OR ZIF_CARGA=>ST_TP_FRETE_CPT.
        ME->CARGA-TP_FRETE = ZIF_CARGA=>ST_TP_FRETE_FOB.
      WHEN ZIF_CARGA=>ST_TP_FRETE_FOB.
        ME->CARGA-TP_FRETE = ZIF_CARGA=>ST_TP_FRETE_CIF.
      WHEN OTHERS.
        ME->CARGA-TP_FRETE = ZIF_CARGA=>ST_TP_FRETE_FOB.
    ENDCASE.

  ENDMETHOD.


  method zif_carga~set_transgenia.

    data:  vid_classificacao type zde_id_classificacao.
    data:  vclassificacao type zsdt0001cl.
    define retorna.
      I_IN_GMO                = ME->CLASSIFICACAO-IN_GMO.
      I_NR_RESULTADO_01       = ME->CLASSIFICACAO-NR_RESULTADO_01.
      I_NR_RESULTADO_02       = ME->CLASSIFICACAO-NR_RESULTADO_02.
      I_NR_RES_RR1_RR2        = ME->CLASSIFICACAO-NR_RES_RR1_RR2.
      I_IN_GMO_03             = ME->CLASSIFICACAO-IN_GMO_03.
      I_IN_SRR_ORIGEM_PARTIC  = ME->CLASSIFICACAO-IN_SRR_ORIGEM_PARTIC.
      I_ID_OUTRO_PARTIC       = ME->CLASSIFICACAO-ID_OUTRO_PARTIC.
      I_IN_SRR_DECLARADO      = ME->CLASSIFICACAO-IN_SRR_DECLARADO.
      I_IN_TESTE_SRR          = ME->CLASSIFICACAO-IN_TESTE_SRR.
      I_IN_SRR_DECLARADO_2    = ME->CLASSIFICACAO-IN_SRR_DECLARADO_2.
      I_IN_TESTE_SRR_2        = ME->CLASSIFICACAO-IN_TESTE_SRR_2.
      I_ID_CLASSIFICADORA     = ME->CLASSIFICACAO-ID_CLASSIFICADORA.
      I_ID_CK_CLASS_DEST      = ME->CLASSIFICACAO-CK_CLASS_DEST.

      TRY .
        CLEAR: ME->CLASSIFICACAO-TP_TRANSGENIA.

        ME->ZIF_CARGA~GET_FACTORY_TP_TRANSGENIA(
          EXPORTING
            I_CLASSIFICACAO = ME->CLASSIFICACAO
          IMPORTING
            E_TP_TRANSGENIA = ME->CLASSIFICACAO-TP_TRANSGENIA ).
       CATCH ZCX_CARGA .
      ENDTRY.

      I_TP_TRANSGENIA = ME->CLASSIFICACAO-TP_TRANSGENIA.

    end-of-definition.

*(Campo 1) IN_GMO
*(Campo 2) IN_SRR_DECLARADO
*(Campo 3) IN_SRR_DECLARADO_2
*(Campo 4) IN_TESTE_SRR_2
*(campo 5) IN_SRR_ORIGEM_PARTIC
    vid_classificacao = me->classificacao-id_classificacao.
    move-corresponding me->classificacao to vclassificacao.
    me->classificacao-in_gmo                = i_in_gmo.
    me->classificacao-nr_resultado_01       = i_nr_resultado_01.
    me->classificacao-nr_resultado_02       = i_nr_resultado_02.
    me->classificacao-nr_res_rr1_rr2        = i_nr_res_rr1_rr2.
    me->classificacao-in_gmo_03             = i_in_gmo_03.
    me->classificacao-in_srr_origem_partic  = i_in_srr_origem_partic.
    me->classificacao-id_outro_partic       = i_id_outro_partic.
    me->classificacao-in_srr_declarado      = i_in_srr_declarado.
    me->classificacao-in_teste_srr          = i_in_teste_srr.
    me->classificacao-in_srr_declarado_2    = i_in_srr_declarado_2.
    me->classificacao-in_teste_srr_2        = i_in_teste_srr_2.
    me->classificacao-id_classificadora     = i_id_classificadora.
    me->classificacao-ck_class_dest         = i_id_ck_class_dest.

    select single * into @data(wa_mara)
      from mara
     where matnr eq @me->carga-id_produto.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_mara-matkl
      importing
        output = wa_mara-matkl.

    "Campo Não Altera pela Tela
    me->classificacao-in_srr_declarado = abap_false.

    case wa_mara-matkl.

      when zif_carga=>st_grupo_algodao_pluma. "Algodão

        "(Campo 1)
        me->classificacao-in_gmo              = zif_carga=>st_gmo_nao_testado.
        "(Campo 2)
        me->classificacao-in_srr_declarado    = abap_true.
        "(Campo 3)
        me->classificacao-in_srr_declarado_2   = abap_false.
        "(Campo 4)
        me->classificacao-in_teste_srr_2       = zif_carga=>st_gmo_nao_testado.
        "(Campo 5)
        me->classificacao-in_srr_origem_partic = abap_false.

        me->classificacao-nr_resultado_01 = 0.
        me->classificacao-nr_resultado_02 = 0.
        me->classificacao-nr_res_rr1_rr2  = 0.

        clear: me->classificacao-id_outro_partic.

      when '700170'. "Milho

        "(Campo 1)
        me->classificacao-in_gmo              = zif_carga=>st_gmo_nao_testado.
        "(Campo 2)
        me->classificacao-in_srr_declarado    = abap_true.
        "(Campo 3)
        me->classificacao-in_srr_declarado_2   = abap_false.
        "(Campo 4)
        me->classificacao-in_teste_srr_2       = zif_carga=>st_gmo_nao_testado.
        "(Campo 5)
        me->classificacao-in_srr_origem_partic = abap_false.

        me->classificacao-nr_resultado_01 = 0.
        me->classificacao-nr_resultado_02 = 0.
        me->classificacao-nr_res_rr1_rr2  = 0.

        clear: me->classificacao-id_outro_partic.

      when '700110'. "Soja


        case me->classificacao-in_gmo.

*REGRA                      TIPO DE PRODUTO PARA "RESETAR"
*1: Se campo 1 = NEGATIVO, demais campos devem ser marcados com bloqueio para alteração conforme abaixo:                      CONVENCIONAL (CO) Desmarcar "Negativo"
*	Campo 2 = Não
*	Campo 3 = Não
*	Campo 4 = Não Testado
*	Campo 5 = Não
*	OBS.: o campo "Soma RR1 + RR2" deve trazer automaticamente a soma dos valores dos campos "Resultado"

          when zif_carga=>st_gmo_negativo.

*3: O campo 2 deve ser bloqueado para marcação na empresa AMAGGI, exceto para as filiais Lucas (55), Porto Velho (19) e Portochuelo (61)
*0119	AMAGGI PORTO VELHO
*0155	AMAGGI LUCAS - FABRICA
*0161	AMAGGI PORTOCHUELO

            "(Campo 2)
            if me->carga-id_branch ne '0155' and me->carga-id_branch ne '0119' and me->carga-id_branch ne '0161'.
              me->classificacao-in_srr_declarado    = abap_false.
            endif.
            "(Campo 3)
            me->classificacao-in_srr_declarado_2  = abap_false.
            "(Campo 4)
            me->classificacao-in_teste_srr_2      = zif_carga=>st_gmo_nao_testado.
            "(campo 5)
            me->classificacao-in_srr_origem_partic = abap_false.
            clear: me->classificacao-id_outro_partic.
            if i_nr_nota is not initial.                    "US143677
              if i_classificacao is  initial.
                me->classificacao-id_classificacao = i_nr_nota.
                read table me->classificacao_notas assigning field-symbol(<fs_class>) with key id_classificacao = i_nr_nota.
              else.
                me->classificacao-id_classificacao = i_classificacao.
                read table me->classificacao_notas assigning <fs_class> with key id_classificacao = i_classificacao.
              endif.

              if sy-subrc = 0.
                move-corresponding me->classificacao to <fs_class>.
              else.
                append me->classificacao to me->classificacao_notas.
*                me->classificacao-id_classificacao =  vid_classificacao.
              endif.
            endif.
*            move-corresponding vclassificacao to me->classificacao.
            retorna.
            exit.

*2: Se campo 1 = POSITIVO, demais campos devem ser marcados com bloqueio para alteração conforme abaixo:                      RR TESTADO (T1) Desmarcar "Positivo"
*	Campo 2 = Não
*	Campo 3 = Não
*	Campo 4 = Não Testado
*	Campo 5 = Não
*	OBS.: ao menos 1 dos campos "Resultado" deve ser preenchido com valor > 0, e o campo "Soma RR1 + RR2" deve trazer automaticamente a soma dos valores dos campos "Resultado"

          when zif_carga=>st_gmo_positivo.

*3: O campo 2 deve ser bloqueado para marcação na empresa AMAGGI, exceto para as filiais Lucas (55), Porto Velho (19) e Portochuelo (61)
*0119	AMAGGI PORTO VELHO
*0155	AMAGGI LUCAS - FABRICA
*0161	AMAGGI PORTOCHUELO

            "(Campo 2)
            if me->carga-id_branch ne '0155' and me->carga-id_branch ne '0119' and me->carga-id_branch ne '0161'.
              me->classificacao-in_srr_declarado    = abap_false.
            endif.
            "(Campo 3)
            me->classificacao-in_srr_declarado_2  = abap_false.
            "(Campo 4)
            me->classificacao-in_teste_srr_2      = zif_carga=>st_gmo_nao_testado.
            "(campo 5)
            me->classificacao-in_srr_origem_partic = abap_false.
            clear: me->classificacao-id_outro_partic.

            me->classificacao-nr_resultado_01 = 0.
            me->classificacao-nr_resultado_02 = 0.
            me->classificacao-nr_res_rr1_rr2  = 0.
            if i_nr_nota is not initial.                    "US143677
              if i_classificacao is  initial.
                me->classificacao-id_classificacao = i_nr_nota.
                read table me->classificacao_notas assigning <fs_class> with key id_classificacao = i_nr_nota.
              else.
                me->classificacao-id_classificacao = i_classificacao.
                read table me->classificacao_notas assigning <fs_class> with key id_classificacao = i_classificacao.
              endif.
              if sy-subrc = 0.
                move-corresponding me->classificacao to <fs_class>.
              else.
                append me->classificacao to me->classificacao_notas.
*                me->classificacao-id_classificacao =  vid_classificacao.
              endif.
            endif.
*            move-corresponding vclassificacao to me->classificacao.

            retorna.
            exit.

        endcase.

*4: Se campo 3 = SIM, demais campos devem ser marcados com bloqueio para alteração conforme abaixo:                     RR2 DECLARADO (D2)  Desmarcar "Sim"
*	Campo 1 = Sem marcação
*	Campo 2 = Não
*	Campo 4 = Não Testado
*	Campo 5 = Não

        if me->classificacao-in_srr_declarado_2 eq abap_true.
          "(Campo 1)
          me->classificacao-in_gmo              = zif_carga=>st_gmo_nao_testado.
          "(Campo 2)
          me->classificacao-in_srr_declarado    = abap_false.
          "(Campo 4)
          me->classificacao-in_teste_srr_2      = zif_carga=>st_gmo_nao_testado.
          "(campo 5)
          me->classificacao-in_srr_origem_partic = abap_false.
          clear: me->classificacao-id_outro_partic.

          me->classificacao-nr_resultado_01 = 0.
          me->classificacao-nr_resultado_02 = 0.
          me->classificacao-nr_res_rr1_rr2  = 0.
          if i_nr_nota is not initial.                      "US143677
            if i_classificacao is  initial.
              me->classificacao-id_classificacao = i_nr_nota.
              read table me->classificacao_notas assigning <fs_class> with key id_classificacao = i_nr_nota.
            else.
              me->classificacao-id_classificacao = i_classificacao.
              read table me->classificacao_notas assigning <fs_class> with key id_classificacao = i_classificacao.
            endif.
            if sy-subrc = 0.
              move-corresponding me->classificacao to <fs_class>.
            else.
              append me->classificacao to me->classificacao_notas.
*              me->classificacao-id_classificacao =  vid_classificacao.
            endif.
          endif.
*          move-corresponding vclassificacao to me->classificacao.
          retorna.
          exit.
        endif.

        case me->classificacao-in_teste_srr_2.

*5: Se campo 4 = POSITIVO, demais campos devem ser marcados com bloqueio para alteração conforme abaixo:                      RR2 TESTADO (T2)  Desmarcar "Positivo"
*	Campo 1 = Sem marcação
*	Campo 2 = Não
*	Campo 3 = Não
*	Campo 5 = Não

          when zif_carga=>st_gmo_positivo.

            "(Campo 1)
            me->classificacao-in_gmo               = zif_carga=>st_gmo_nao_testado.
            "(Campo 2)
            me->classificacao-in_srr_declarado     = abap_false.
            "(Campo 3)
            me->classificacao-in_srr_declarado_2   = abap_false.
            "(campo 5)
            me->classificacao-in_srr_origem_partic = abap_false.
            clear: me->classificacao-id_outro_partic.

            me->classificacao-nr_resultado_01 = 0.
            me->classificacao-nr_resultado_02 = 0.
            me->classificacao-nr_res_rr1_rr2  = 0.
            if i_nr_nota is not initial.                    "US143677
              if i_classificacao is  initial.
                me->classificacao-id_classificacao = i_nr_nota.
                read table me->classificacao_notas assigning <fs_class> with key id_classificacao = i_nr_nota.
              else.
                me->classificacao-id_classificacao = i_classificacao.
                read table me->classificacao_notas assigning <fs_class> with key id_classificacao = i_classificacao.
              endif.
              if sy-subrc = 0.
                move-corresponding me->classificacao to <fs_class>.
              else.
                append me->classificacao to me->classificacao_notas.
*                me->classificacao-id_classificacao =  vid_classificacao.
              endif.
            endif.
*            move-corresponding vclassificacao to me->classificacao.
            retorna.
            exit.

*6: Se campo 4 = NEGATIVO, demais campos devem ser marcados com bloqueio para alteração conforme abaixo:                      RR1 DECLARADO (D1)  Desmarcar "Negativo"
*	Campo 1 = Sem marcação
*	Campo 2 = Sim
*	Campo 3 = Não
*	Campo 5 = Não

          when zif_carga=>st_gmo_negativo.

            "(Campo 1)
            me->classificacao-in_gmo               = zif_carga=>st_gmo_nao_testado.
            "(Campo 2)
            me->classificacao-in_srr_declarado     = abap_true.
            "(Campo 3)
            me->classificacao-in_srr_declarado_2   = abap_false.
            "(campo 5)
            me->classificacao-in_srr_origem_partic = abap_false.
            clear: me->classificacao-id_outro_partic.

            me->classificacao-nr_resultado_01 = 0.
            me->classificacao-nr_resultado_02 = 0.
            me->classificacao-nr_res_rr1_rr2  = 0.
            if i_nr_nota is not initial.                    "US143677
              if i_classificacao is  initial.
                me->classificacao-id_classificacao = i_nr_nota.
                read table me->classificacao_notas assigning <fs_class> with key id_classificacao = i_nr_nota.
              else.
                me->classificacao-id_classificacao = i_classificacao.
                read table me->classificacao_notas assigning <fs_class> with key id_classificacao = i_classificacao.
              endif.
              if sy-subrc = 0.
                move-corresponding me->classificacao to <fs_class>.
              else.
                append me->classificacao to me->classificacao_notas.
*                me->classificacao-id_classificacao =  vid_classificacao.
              endif.
            endif.
*            move-corresponding vclassificacao to me->classificacao.
            retorna.
            exit.

        endcase.

*5: Se campo 5 = SIM, demais campos devem ser marcados com bloqueio para alteração conforme abaixo:                     PARTICIPANTE (PA) Desmarcar "Negativo"
*	Campo 1 = Sem marcação
*	Campo 2 = Não
*	Campo 3 = Não
*	Campo 4 = Não Testado
*	OBS.: Habilitar os campos "Participante" e "Endereço Parcipante" para que o usuário lance as informações. Não permitir a gravação com os campos em branco.

        if me->classificacao-in_srr_origem_partic eq abap_true.
          "(Campo 1)
          me->classificacao-in_gmo              = zif_carga=>st_gmo_nao_testado.
          "(Campo 2)
          me->classificacao-in_srr_declarado    = abap_false.
          "(Campo 3)
          me->classificacao-in_srr_declarado_2   = abap_false.
          "(Campo 4)
          me->classificacao-in_teste_srr_2      = zif_carga=>st_gmo_nao_testado.

          me->classificacao-nr_resultado_01 = 0.
          me->classificacao-nr_resultado_02 = 0.
          me->classificacao-nr_res_rr1_rr2  = 0.
          if i_nr_nota is not initial.                      "US143677
            if i_classificacao is  initial.
              me->classificacao-id_classificacao = i_nr_nota.
              read table me->classificacao_notas assigning <fs_class> with key id_classificacao = i_nr_nota.
            else.
              me->classificacao-id_classificacao = i_classificacao.
              read table me->classificacao_notas assigning <fs_class> with key id_classificacao = i_classificacao.
            endif.
            if sy-subrc = 0.
              move-corresponding me->classificacao to <fs_class>.
            else.
              append me->classificacao to me->classificacao_notas.
*              me->classificacao-id_classificacao =  vid_classificacao.
            endif.
          endif.
*          move-corresponding vclassificacao to me->classificacao.
          retorna.
          exit.
        endif.

      when others.

        "(Campo 1)
        me->classificacao-in_gmo               = zif_carga=>st_gmo_nao_testado.
        "(Campo 2)
        me->classificacao-in_srr_declarado     = abap_false.
        "(Campo 3)
        me->classificacao-in_srr_declarado_2   = abap_false.
        "(Campo 4)
        me->classificacao-in_teste_srr_2       = zif_carga=>st_gmo_nao_testado.
        "(Campo 5)
        me->classificacao-in_srr_origem_partic = abap_false.
        clear: me->classificacao-id_outro_partic.

        me->classificacao-nr_resultado_01 = 0.
        me->classificacao-nr_resultado_02 = 0.
        me->classificacao-nr_res_rr1_rr2  = 0.

    endcase.
    if i_nr_nota is not initial.                            "US143677
      if i_classificacao is  initial.
        me->classificacao-id_classificacao = i_nr_nota.
        read table me->classificacao_notas assigning <fs_class> with key id_classificacao = i_nr_nota.
      else.
        me->classificacao-id_classificacao = i_classificacao.
        read table me->classificacao_notas assigning <fs_class> with key id_classificacao = i_classificacao.
      endif.
      if sy-subrc = 0.
        move-corresponding me->classificacao to <fs_class>.
      else.
        append me->classificacao to me->classificacao_notas.
*        me->classificacao-id_classificacao =  vid_classificacao.
      endif.
    endif.

*    move-corresponding vclassificacao to me->classificacao.

    retorna.

  endmethod.


  METHOD zif_carga~set_validar_safra.

    DATA: l_pno_ini TYPE i,
          l_pno_fim TYPE i.

    r_carga   = me.

    l_pno_ini = sy-datum(4).
    l_pno_fim = sy-datum(4).

    ADD -1 TO l_pno_ini.
    ADD  1 TO l_pno_fim.

    CASE i_acao.
      WHEN zif_carga=>st_action_grao.
        IF i_safra < l_pno_ini OR
           i_safra > l_pno_fim.
          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = VALUE #( msgid = zcx_carga=>zcx_valida_safra-msgid
                                msgno = zcx_carga=>zcx_valida_safra-msgno
                                attr1 = CONV #( i_safra )
                                attr2 = CONV #( 'para a opção Granel' ) )
              msgty  = 'E'
              msgid  = zcx_carga=>zcx_valida_safra-msgid
              msgno  = zcx_carga=>zcx_valida_safra-msgno
              msgv1  = CONV #( i_safra )
              msgv2  = CONV #( 'para a opção Granel' ).
        ENDIF.

      WHEN zif_carga=>st_action_algo.
        IF i_safra > sy-datum(4).
          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = VALUE #( msgid = zcx_carga=>zcx_valida_safra-msgid
                                msgno = zcx_carga=>zcx_valida_safra-msgno
                                attr1 = CONV #( i_safra )
                                attr2 = CONV #( 'para a opção Algodão' ) )
              msgty  = 'E'
              msgid  = zcx_carga=>zcx_valida_safra-msgid
              msgno  = zcx_carga=>zcx_valida_safra-msgno
              msgv1  = CONV #( i_safra )
              msgv2  = CONV #( 'para a opção Algodão' ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_CARGA~SET_VOLUME_ORDEM_VENDA.
    R_CARGA = ME.
  ENDMETHOD.


  METHOD ZIF_CARGA~VALIDAR_EXCLUSAO.

    E_VALIDOU = ABAP_FALSE.

    R_CARGA = ME.

  ENDMETHOD.


  METHOD zif_carga~validar_registro.

    DATA: "RG_ID_CARGA  TYPE RANGE OF ZDE_ID_CARGA,
      e_tipo    TYPE char01,
      lc_texto  TYPE string,
      lc_branch TYPE lifnr.

    r_carga = me.

    e_validou = abap_false.

    DATA: lc_ck_nfe TYPE zsdt0001te-ck_nfe.

    me->zif_carga~get_ck_carga_sem_solic_manut( ).

    IF me->zif_carga~ordem_venda[]   IS INITIAL AND
       me->zif_carga~pedido_compra[] IS INITIAL AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_ordem_venda-msgid
                            msgno = zcx_carga=>zcx_obg_inf_ordem_venda-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_ordem_venda-msgno
          msgid  = zcx_carga=>zcx_obg_inf_ordem_venda-msgid.
    ENDIF.

    IF me->carga-id_local_entrega IS INITIAL AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_lc_entrega-msgid
                            msgno = zcx_carga=>zcx_obg_inf_lc_entrega-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_lc_entrega-msgno
          msgid  = zcx_carga=>zcx_obg_inf_lc_entrega-msgid.
    ENDIF.

    SELECT * INTO TABLE @DATA(it_local)
      FROM zsdt0001lp
     WHERE id_local_entrega EQ @me->carga-id_local_entrega
       AND id_bukrs         EQ @me->carga-id_bukrs
       AND id_branch        EQ @me->carga-id_branch.

    "053  Local de Entrega não parametrizado para Local de Negócio &1!
    "054  Local de Entrega não parametrizado para Material &1!
    IF sy-subrc IS NOT INITIAL  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_le_sem_param_lc_negocio-msgid
                            msgno = zcx_carga=>zcx_le_sem_param_lc_negocio-msgno
                            attr1 = CONV #( me->carga-id_branch ) )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_le_sem_param_lc_negocio-msgno
          msgid  = zcx_carga=>zcx_le_sem_param_lc_negocio-msgid
          msgv1  = CONV #( me->carga-id_branch ).
    ENDIF.

    SORT it_local BY id_material.
    READ TABLE it_local WITH KEY id_material = me->carga-id_produto TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_le_sem_param_material-msgid
                            msgno = zcx_carga=>zcx_le_sem_param_material-msgno
                            attr1 = CONV #( me->carga-id_produto ) )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_le_sem_param_material-msgno
          msgid  = zcx_carga=>zcx_le_sem_param_material-msgid
          msgv1  = CONV #( me->carga-id_produto ).
    ENDIF.

    IF me->carga-id_local_coleta IS INITIAL  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_lc_coleta-msgid msgno = zcx_carga=>zcx_obg_inf_lc_coleta-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_lc_coleta-msgno
          msgid  = zcx_carga=>zcx_obg_inf_lc_coleta-msgid.
    ELSEIF me->carga-tp_status NE zif_carga=>st_status_cancelada.
      DATA(ob_fornecedores) = zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = me->carga-id_local_coleta
        )->ck_ativo(
        )->ck_ativo_empresa( i_empresa = me->carga-id_bukrs
        ).
    ENDIF.

    IF me->carga-id_local_destino IS INITIAL  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_lc_destino-msgid msgno = zcx_carga=>zcx_obg_inf_lc_destino-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_lc_destino-msgno
          msgid  = zcx_carga=>zcx_obg_inf_lc_destino-msgid.
    ELSEIF me->carga-tp_status NE zif_carga=>st_status_cancelada.
*      OB_FORNECEDORES = ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
*        )->SET_PARCEIRO( I_PARCEIRO = ME->CARGA-ID_LOCAL_DESTINO
*        )->CK_ATIVO(
*        )->CK_ATIVO_EMPRESA( I_EMPRESA = ME->CARGA-ID_BUKRS
*        ).
    ENDIF.

    IF me->carga-id_local_descarga IS INITIAL  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_lc_descarga-msgid msgno = zcx_carga=>zcx_obg_inf_lc_descarga-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_lc_descarga-msgno
          msgid  = zcx_carga=>zcx_obg_inf_lc_descarga-msgid.
    ELSEIF me->carga-tp_status NE zif_carga=>st_status_cancelada.
      DATA(ob_cliente) = zcl_clientes=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = me->carga-id_local_descarga
        )->ck_ativo(
        )->ck_ativo_empresa( i_empresa = me->carga-id_bukrs
        ).
    ENDIF.

    IF me->carga-id_motorista IS INITIAL AND
       me->zif_carga~at_tipo_frete_ordem_venda EQ zif_carga=>st_tp_frete_cif  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_motorista-msgid
                            msgno = zcx_carga=>zcx_obg_inf_motorista-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_motorista-msgno
          msgid  = zcx_carga=>zcx_obg_inf_motorista-msgid.
    ELSEIF me->carga-tp_status NE zif_carga=>st_status_cancelada AND me->carga-id_motorista IS NOT INITIAL.
      ob_fornecedores = zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = me->carga-id_motorista
        )->ck_ativo(
        ).
    ENDIF.

    IF me->carga-id_proprietario IS INITIAL AND
       me->zif_carga~at_tipo_frete_ordem_venda EQ zif_carga=>st_tp_frete_cif AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_proprietario-msgid
                            msgno = zcx_carga=>zcx_obg_inf_proprietario-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_proprietario-msgno
          msgid  = zcx_carga=>zcx_obg_inf_proprietario-msgid.
    ELSEIF me->carga-tp_status NE zif_carga=>st_status_cancelada AND me->carga-id_proprietario IS NOT INITIAL.
      ob_fornecedores = zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = me->carga-id_proprietario
        )->ck_ativo(
        )->ck_ativo_empresa( i_empresa = me->carga-id_bukrs
        ).
    ENDIF.

    IF me->carga-ds_placa_trator IS INITIAL AND
       me->zif_carga~at_tipo_frete_ordem_venda EQ zif_carga=>st_tp_frete_cif AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_trator-msgid
                            msgno = zcx_carga=>zcx_obg_inf_trator-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_trator-msgno
          msgid  = zcx_carga=>zcx_obg_inf_trator-msgid.
    ENDIF.

    IF me->carga-nr_ticket IS INITIAL AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_ticket-msgid msgno = zcx_carga=>zcx_obg_inf_ticket-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_ticket-msgno
          msgid  = zcx_carga=>zcx_obg_inf_ticket-msgid.
    ENDIF.

    DATA: lc_nr_ticket TYPE zde_nr_ticket.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = me->carga-nr_ticket
      IMPORTING
        output = lc_nr_ticket.

    IF zcl_string=>length( text = CONV #( lc_nr_ticket ) ) GT 7  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid  = zcx_carga=>zcx_nr_ticket_grande-msgid
                            msgno  = zcx_carga=>zcx_nr_ticket_grande-msgno )
          msgid  = zcx_carga=>zcx_nr_ticket_grande-msgid
          msgno  = zcx_carga=>zcx_nr_ticket_grande-msgno
          msgty  = 'E'.
    ENDIF.

    IF me->carga-nm_peso_bruto IS INITIAL  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_ps_bruto-msgid
                            msgno = zcx_carga=>zcx_obg_inf_ps_bruto-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_ps_bruto-msgno
          msgid  = zcx_carga=>zcx_obg_inf_ps_bruto-msgid.
    ENDIF.

    IF me->carga-nm_peso_tara IS INITIAL  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_ps_tara-msgid
                            msgno = zcx_carga=>zcx_obg_inf_ps_tara-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_ps_tara-msgno
          msgid  = zcx_carga=>zcx_obg_inf_ps_tara-msgid.
    ENDIF.

    IF me->carga-nm_peso_subtotal IS INITIAL  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_ps_subtotal-msgid
                            msgno = zcx_carga=>zcx_obg_inf_ps_subtotal-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_ps_subtotal-msgno
          msgid  = zcx_carga=>zcx_obg_inf_ps_subtotal-msgid.
    ENDIF.

    IF me->carga-nm_peso_tara GE me->carga-nm_peso_bruto  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_tara_maior_bruto-msgid msgno = zcx_carga=>zcx_tara_maior_bruto-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_tara_maior_bruto-msgno
          msgid  = zcx_carga=>zcx_tara_maior_bruto-msgid.
    ENDIF.

    me->get_calcular_subtotal(
      EXPORTING
        i_peso_bruto    = me->carga-nm_peso_bruto
        i_peso_tara     = me->carga-nm_peso_tara
      IMPORTING
        e_peso_subtotal = DATA(e_peso_subtotal) ).

    IF me->carga-nm_peso_subtotal NE e_peso_subtotal  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_errp_ps_subtotal-msgid msgno = zcx_carga=>zcx_errp_ps_subtotal-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_errp_ps_subtotal-msgno
          msgid  = zcx_carga=>zcx_errp_ps_subtotal-msgid.
    ENDIF.

    IF me->carga-nm_peso_liquido GT me->carga-nm_peso_subtotal  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_peso_liq_subtotal-msgid msgno = zcx_carga=>zcx_peso_liq_subtotal-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_peso_liq_subtotal-msgno
          msgid  = zcx_carga=>zcx_peso_liq_subtotal-msgid.
    ENDIF.

    IF me->carga-nm_peso_liquido IS INITIAL  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_ps_liquido-msgid
                            msgno = zcx_carga=>zcx_obg_inf_ps_liquido-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_ps_liquido-msgno
          msgid  = zcx_carga=>zcx_obg_inf_ps_liquido-msgid.
    ENDIF.

    IF me->carga-nr_safra IS INITIAL  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_safra-msgid msgno = zcx_carga=>zcx_obg_inf_safra-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_safra-msgno
          msgid  = zcx_carga=>zcx_obg_inf_safra-msgid.
    ENDIF.

    IF ( me->carga-id_bukrs IS INITIAL OR me->carga-id_branch IS INITIAL )  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_lc_negocio-msgid msgno = zcx_carga=>zcx_obg_inf_lc_negocio-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_lc_negocio-msgno
          msgid  = zcx_carga=>zcx_obg_inf_lc_negocio-msgid.
    ENDIF.

    IF me->carga-id_produto IS INITIAL  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_produto-msgid
                            msgno = zcx_carga=>zcx_obg_inf_produto-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_produto-msgno
          msgid  = zcx_carga=>zcx_obg_inf_produto-msgid.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_mara)
      FROM mara
     WHERE matnr EQ @me->carga-id_produto.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_mara-matkl
      IMPORTING
        output = wa_mara-matkl.


    IF me->classificacao-ck_class_dest IS INITIAL AND "RMB ***///CS2021000581 Classificação destino - Flag para marcação - ZMM0127 - Set 2021 - Inicio
      ( me->classificacao-id_classificadora IS INITIAL AND
       zif_carga=>st_grupo_algodao_pluma NE wa_mara-matkl )  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_emp_classificadora-msgid msgno = zcx_carga=>zcx_obg_emp_classificadora-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_emp_classificadora-msgno
          msgid  = zcx_carga=>zcx_obg_emp_classificadora-msgid.
    ENDIF.

    IF me->classificacao-id_classificadora IS NOT INITIAL AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.

      SELECT SINGLE * INTO @DATA(wa_zsdt0001ce)
        FROM zsdt0001ce
       WHERE lifnr EQ @me->classificacao-id_classificadora.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_fornecedor_nao_classifica-msgid
                              msgno = zcx_carga=>zcx_fornecedor_nao_classifica-msgno
                              attr1 = CONV #( me->classificacao-id_classificadora ) )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_fornecedor_nao_classifica-msgno
            msgid  = zcx_carga=>zcx_fornecedor_nao_classifica-msgid
            msgv1  = CONV #( me->classificacao-id_classificadora ).
      ENDIF.

      ob_fornecedores = zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = me->classificacao-id_classificadora
        )->ck_ativo(
        )->ck_ativo_empresa( i_empresa = me->carga-id_bukrs
        ).

    ENDIF.

***///CS2021000581 Classificação destino - Flag para marcação - ZMM0127 - Set 2021 - Inicio
    IF me->classificacao-ck_class_dest IS NOT INITIAL AND me->classificacao-id_classificadora IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_ck_revisao-msgid
                            msgno = zcx_carga=>zcx_ck_revisao-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_ck_revisao-msgno
          msgid  = zcx_carga=>zcx_ck_revisao-msgid.
    ENDIF.
***///CS2021000581 Classificação destino - Flag para marcação - ZMM0127 - Set 2021 - Fim

    IF me->classificacao-id_outro_partic IS NOT INITIAL.

      ob_fornecedores = zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = me->classificacao-id_outro_partic
        )->ck_ativo(
        ).

    ENDIF.

    IF wa_mara-matkl NE zif_carga=>st_grupo_algodao_pluma. "Algodão

      LOOP AT me->resultado INTO DATA(wa_resultado).
        IF wa_resultado-nr_percentual_com IS INITIAL.
          CASE wa_resultado-tp_caracteristica.
            WHEN zif_carga=>st_tp_caract_class_umidade.
              IF me->zif_carga~ck_digitado_umidade NE abap_true.
                RAISE EXCEPTION TYPE zcx_carga
                  EXPORTING
                    textid = VALUE #( msgid = zcx_carga=>zcx_obg_class_umidade-msgid msgno = zcx_carga=>zcx_obg_class_umidade-msgno )
                    msgty  = 'E'
                    msgno  = zcx_carga=>zcx_obg_class_umidade-msgno
                    msgid  = zcx_carga=>zcx_obg_class_umidade-msgid.
              ENDIF.
            WHEN zif_carga=>st_tp_caract_class_impureza.
              IF me->zif_carga~ck_digitado_impureza NE abap_true.
                RAISE EXCEPTION TYPE zcx_carga
                  EXPORTING
                    textid = VALUE #( msgid = zcx_carga=>zcx_obg_class_impureza-msgid msgno = zcx_carga=>zcx_obg_class_impureza-msgno )
                    msgty  = 'E'
                    msgno  = zcx_carga=>zcx_obg_class_impureza-msgno
                    msgid  = zcx_carga=>zcx_obg_class_impureza-msgid.
              ENDIF.
            WHEN zif_carga=>st_tp_caract_class_avariado.
              IF me->zif_carga~ck_digitado_avariado NE abap_true.
                RAISE EXCEPTION TYPE zcx_carga
                  EXPORTING
                    textid = VALUE #( msgid = zcx_carga=>zcx_obg_class_avariado-msgid msgno = zcx_carga=>zcx_obg_class_avariado-msgno )
                    msgty  = 'E'
                    msgno  = zcx_carga=>zcx_obg_class_avariado-msgno
                    msgid  = zcx_carga=>zcx_obg_class_avariado-msgid.
              ENDIF.
            WHEN zif_carga=>st_tp_caract_class_ardido.
              "US 156300 - ALRS
*              IF me->zif_carga~ck_digitado_ardido NE abap_true.
*                RAISE EXCEPTION TYPE zcx_carga
*                  EXPORTING
*                    textid = VALUE #( msgid = zcx_carga=>zcx_obg_class_ardido-msgid msgno = zcx_carga=>zcx_obg_class_ardido-msgno )
*                    msgty  = 'E'
*                    msgno  = zcx_carga=>zcx_obg_class_ardido-msgno
*                    msgid  = zcx_carga=>zcx_obg_class_ardido-msgid.
*              ENDIF.
              "US 156300 - ALRS
            WHEN zif_carga=>st_tp_caract_class_quebrado.

              IF me->zif_carga~ck_digitado_quebrado NE abap_true.
                RAISE EXCEPTION TYPE zcx_carga
                  EXPORTING
                    textid = VALUE #( msgid = zcx_carga=>zcx_obg_class_quebrado-msgid msgno = zcx_carga=>zcx_obg_class_quebrado-msgno )
                    msgty  = 'E'
                    msgno  = zcx_carga=>zcx_obg_class_quebrado-msgno
                    msgid  = zcx_carga=>zcx_obg_class_quebrado-msgid.
              ENDIF.

            WHEN zif_carga=>st_tp_caract_class_esverdeado.

              IF me->zif_carga~ck_digitado_esverdeado NE abap_true.
                RAISE EXCEPTION TYPE zcx_carga
                  EXPORTING
                    textid = VALUE #( msgid = zcx_carga=>zcx_obg_class_esverdeado-msgid msgno = zcx_carga=>zcx_obg_class_esverdeado-msgno )
                    msgty  = 'E'
                    msgno  = zcx_carga=>zcx_obg_class_esverdeado-msgno
                    msgid  = zcx_carga=>zcx_obg_class_esverdeado-msgid.
              ENDIF.

            WHEN zif_carga=>st_tp_caract_class_carunchado.

              IF wa_mara-matkl EQ '700170'. "Milho
                IF me->zif_carga~ck_digitado_carunchado NE abap_true.
                  RAISE EXCEPTION TYPE zcx_carga
                    EXPORTING
                      textid = VALUE #( msgid = zcx_carga=>zcx_obg_class_carunchado-msgid msgno = zcx_carga=>zcx_obg_class_carunchado-msgno )
                      msgty  = 'E'
                      msgno  = zcx_carga=>zcx_obg_class_carunchado-msgno
                      msgid  = zcx_carga=>zcx_obg_class_carunchado-msgid.
                ENDIF.
              ENDIF.

          ENDCASE.
        ENDIF.
      ENDLOOP.

      LOOP AT me->resultado INTO wa_resultado.
        CASE wa_resultado-tp_caracteristica.
          WHEN zif_carga=>st_tp_caract_class_umidade.
          WHEN zif_carga=>st_tp_caract_class_impureza.
          WHEN zif_carga=>st_tp_caract_class_avariado.

            DATA: lc_total TYPE zde_nr_perc_ava_arq.

            lc_total = 0.
            DATA(lva_count_result_avariado) = 0.
            LOOP AT me->zif_carga~resultado_avariado INTO DATA(wa_resultado_avariado) WHERE id_classificacao EQ wa_resultado-id_classificacao.
              ADD wa_resultado_avariado-nr_percentual_com TO lc_total.
              ADD 1 TO lva_count_result_avariado.
            ENDLOOP.

            IF ( lc_total NE wa_resultado-nr_percentual_com ) AND ( lva_count_result_avariado > 0 ).
              RAISE EXCEPTION TYPE zcx_carga
                EXPORTING
                  textid = VALUE #( msgid = zcx_carga=>zcx_erro_soma_avariado-msgid msgno = zcx_carga=>zcx_erro_soma_avariado-msgno )
                  msgty  = 'E'
                  msgno  = zcx_carga=>zcx_erro_soma_avariado-msgno
                  msgid  = zcx_carga=>zcx_erro_soma_avariado-msgid.
            ENDIF.

          WHEN zif_carga=>st_tp_caract_class_ardido.
          WHEN zif_carga=>st_tp_caract_class_quebrado.
          WHEN zif_carga=>st_tp_caract_class_esverdeado.
          WHEN zif_carga=>st_tp_caract_class_carunchado.
        ENDCASE.
      ENDLOOP.

    ENDIF.

    IF me->classificacao-in_gmo EQ zif_carga=>st_gmo_positivo AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_nao_teste_amaggi_positivo-msgid msgno = zcx_carga=>zcx_nao_teste_amaggi_positivo-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_nao_teste_amaggi_positivo-msgno
          msgid  = zcx_carga=>zcx_nao_teste_amaggi_positivo-msgid.
    ENDIF.

    IF me->classificacao-in_srr_origem_partic EQ abap_true AND
       me->classificacao-id_outro_partic IS INITIAL AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_outro_part-msgid msgno = zcx_carga=>zcx_obg_inf_outro_part-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_outro_part-msgno
          msgid  = zcx_carga=>zcx_obg_inf_outro_part-msgid.
    ELSEIF me->classificacao-in_srr_origem_partic EQ abap_false AND
           me->classificacao-id_outro_partic IS NOT INITIAL  AND
           me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_nao_inf_outro_part-msgid msgno = zcx_carga=>zcx_nao_inf_outro_part-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_nao_inf_outro_part-msgno
          msgid  = zcx_carga=>zcx_nao_inf_outro_part-msgid.
    ENDIF.

    DESCRIBE TABLE me->documento_fiscal LINES DATA(lc_linhas).

    IF lc_linhas IS INITIAL  AND
       me->zif_carga~carga-tp_status NE zif_carga=>st_status_cancelada.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_obg_inf_doc_fiscal-msgid msgno = zcx_carga=>zcx_obg_inf_doc_fiscal-msgno )
          msgty  = 'E'
          msgno  = zcx_carga=>zcx_obg_inf_doc_fiscal-msgno
          msgid  = zcx_carga=>zcx_obg_inf_doc_fiscal-msgid.
    ENDIF.

    LOOP AT me->documento_fiscal ASSIGNING FIELD-SYMBOL(<fs_nota>).
      me->zif_carga~get_validar_nota_fiscal( CHANGING i_nota_fiscal = <fs_nota> ).
    ENDLOOP.

    "Validar Quantidade de Algodão Vinculado por Nota Fiscal """"""""""""""""""""""""""""""""""""""""""
    me->zif_carga~verif_ck_pluma_nota_fiscal( ).

    me->zif_carga~verif_ck_saldo_takeup_bloco( ).

    me->zif_carga~get_factory_tp_transgenia(
      EXPORTING
        i_classificacao = me->classificacao
      IMPORTING
        e_tp_transgenia = me->classificacao-tp_transgenia
    ).

    "US 143677 - transgenia por nota
    LOOP AT me->classificacao_notas INTO DATA(wclass).
      me->zif_carga~get_factory_tp_transgenia(
        EXPORTING
          i_classificacao = wclass
        IMPORTING
          e_tp_transgenia = wclass-tp_transgenia
      ).
    ENDLOOP.
    "US 143677 - transgenia por nota


    me->zif_carga~validar_registro_pedido_compra( ).

    me->zif_carga~validar_registro_ordem_venda( i_processo = i_processo ).

    IF me->carga-tp_status NE zif_carga=>st_status_cancelada.
      me->zif_carga~get_info_placa( EXPORTING i_tipo_frete = me->zif_carga~at_tipo_frete_ordem_venda i_placa = me->carga-ds_placa_trator  i_validar = abap_true i_tracao = abap_true ).
      me->zif_carga~get_info_placa( EXPORTING i_tipo_frete = me->zif_carga~at_tipo_frete_ordem_venda i_placa = me->carga-ds_placa_reboq_1 i_validar = abap_true i_tracao = abap_false ).
      me->zif_carga~get_info_placa( EXPORTING i_tipo_frete = me->zif_carga~at_tipo_frete_ordem_venda i_placa = me->carga-ds_placa_reboq_2 i_validar = abap_true i_tracao = abap_false ).
      me->zif_carga~get_info_placa( EXPORTING i_tipo_frete = me->zif_carga~at_tipo_frete_ordem_venda i_placa = me->carga-ds_placa_reboq_3 i_validar = abap_true i_tracao = abap_false ).
    ENDIF.

    CLEAR: ob_cliente, ob_fornecedores.

    "Validar Entrada de Sobra """""""""""""""""""""""""""""""""""""""""""
    me->zif_carga~get_ck_gera_sobra( ).

    "Verifica se Existe Alteraçao no Romaneio
    me->zif_carga~verif_alteracao_manut_romaneio( ).

*-CS2021000183 - 29.03.2022 - JT - inicio
*validar o Tp. Ent for  "Propria", o CFOP deverá estar adequado
    DATA(l_erro1) = abap_false.
    DATA(l_erro2) = abap_false.

    LOOP AT me->documento_fiscal ASSIGNING FIELD-SYMBOL(<fs_nf>).

      SELECT ct_nota, tp_pessoa
        FROM zsdt0001te
        INTO @DATA(w_zsdt0001te)
          UP TO 1 ROWS
       WHERE id_entrada = @<fs_nf>-id_entrada
         AND id_empresa = @me->carga-id_bukrs.
      ENDSELECT.

      IF sy-subrc <> 0.
        l_erro1 = abap_true.
        EXIT.
      ENDIF.

      SELECT form
        INTO @DATA(l_form)
        FROM j_1baa
          UP TO 1 ROWS
       WHERE nftype = @w_zsdt0001te-ct_nota.
      ENDSELECT.

      CHECK l_form IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->carga-id_branch
        IMPORTING
          output = lc_branch.

      SELECT regio
        INTO @DATA(l_regio)
        FROM lfa1
          UP TO 1 ROWS
       WHERE lifnr = @lc_branch.
      ENDSELECT.

      SELECT *
        FROM zsdt0001tecfop
        INTO @DATA(w_zsdt0001tecfop)
          UP TO 1 ROWS
       WHERE id_entrada = @<fs_nf>-id_entrada
         AND cfop       = @<fs_nf>-cfop
         AND regio      = @l_regio
         AND tp_pessoa  = @w_zsdt0001te-tp_pessoa.
      ENDSELECT.

      IF sy-subrc <> 0.
        l_erro2 = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    CASE abap_true.
      WHEN l_erro1.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_tpentrada_empresa_err-msgid
                              msgno = zcx_carga=>zcx_tpentrada_empresa_err-msgno
                              attr1 = CONV #( me->carga-id_bukrs  )
                              attr2 = CONV #( me->carga-id_branch )
                              attr3 = CONV #( me->carga-nr_safra ) )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_tpentrada_empresa_err-msgno
            msgid  = zcx_carga=>zcx_tpentrada_empresa_err-msgid
            msgv1  = CONV #( me->carga-id_bukrs )
            msgv2  = CONV #( me->carga-id_branch )
            msgv3  = CONV #( me->carga-nr_safra ).

      WHEN l_erro2.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_cfop_nao_permitido_te-msgid
                              msgno = zcx_carga=>zcx_cfop_nao_permitido_te-msgno
                              attr1 = CONV #( <fs_nf>-cfop )
                              attr2 = CONV #( 'Nota própria!' ) )
            msgty  = 'E'
            msgno  = zcx_carga=>zcx_cfop_nao_permitido_te-msgno
            msgid  = zcx_carga=>zcx_cfop_nao_permitido_te-msgid
            msgv1  = CONV #( <fs_nf>-cfop )
            msgv2  = CONV #( 'Nota própria!' ).
    ENDCASE.
*-CS2021000183 - 29.03.2022 - JT - fim

    e_validou = abap_true.

  ENDMETHOD.


  METHOD zif_carga~validar_registro_ordem_venda.

    DATA: e_tipo_frete TYPE zde_tp_frete.

    CHECK me->zif_carga~ck_executar_reversao_entrada = abap_false. " Ajuste Validaçao OV Canc.ZMM0127 Issue 180415 - WPP

    LOOP AT me->zif_carga~ordem_venda INTO DATA(wa_ordem_venda).


      CASE me->classificacao-tp_transgenia .
        WHEN 'CO'.
          DATA(ordem_venda) =
          zcl_ordem_venda=>zif_ordem_venda~get_instance(
                   )->set_ordem_venda( i_vbeln = wa_ordem_venda-nr_ordem_venda
                   ")->ck_ordem_venda_convencional( i_eudr = me->carga-eudr "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940
                   )->ck_filial_emissora_romaneio( i_branch = me->carga-id_branch
                   )->ck_safra( i_safra = CONV #( me->carga-nr_safra )
                   )->ck_ordem_venda_dco(  i_branch = me->carga-id_branch i_matnr = me->carga-id_produto
                   )->get_tipo_frete( IMPORTING  e_tipo_frete = e_tipo_frete
                   ).

          IF i_processo = 'CONFERENCIA'.
            ordem_venda->ck_ordem_venda_convencional( i_eudr = me->carga-eudr ). "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940
          ENDIF.

        WHEN OTHERS.
          ordem_venda =
          zcl_ordem_venda=>zif_ordem_venda~get_instance(
                   )->set_ordem_venda( i_vbeln = wa_ordem_venda-nr_ordem_venda
                   ")->ck_ordem_venda_transgenica( i_eudr = me->carga-eudr "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940
                   )->ck_filial_emissora_romaneio( i_branch = me->carga-id_branch
                   )->ck_safra( i_safra = CONV #( me->carga-nr_safra )
                   )->ck_ordem_venda_dco(  i_branch = me->carga-id_branch i_matnr = me->carga-id_produto
                   )->get_tipo_frete( IMPORTING  e_tipo_frete = e_tipo_frete
                   ).

          IF i_processo = 'CONFERENCIA'.
            ordem_venda->ck_ordem_venda_transgenica( i_eudr = me->carga-eudr ). "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940
          ENDIF.

      ENDCASE.

      "Somente Permitido Ordem de Venda CIF, CPT,  CFR, FOB
      TRY .
          ordem_venda->ck_tipo_frete( i_tipo_frete = zif_carga=>st_tp_frete_cif ).
        CATCH zcx_ordem_venda.
          TRY .
              ordem_venda->ck_tipo_frete( i_tipo_frete = zif_carga=>st_tp_frete_cpt ).
            CATCH zcx_ordem_venda.
              TRY .
                  ordem_venda->ck_tipo_frete( i_tipo_frete = zif_carga=>st_tp_frete_cfr ).
                CATCH zcx_ordem_venda.
                  ordem_venda->ck_tipo_frete( i_tipo_frete = zif_carga=>st_tp_frete_fob ). "U.S 155370 / AOENNING.
              ENDTRY.
          ENDTRY.
      ENDTRY.

      TRY .
          CASE e_tipo_frete.
            WHEN zif_carga=>st_tp_frete_cif.

              IF me->carga-id_ordem IS INITIAL.
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

              IF me->carga-id_agent_frete IS NOT INITIAL AND me->carga-tp_status NE zif_carga=>st_status_cancelada.
                DATA(ob_fornecedores) = zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = me->carga-id_agent_frete
                  )->ck_ativo(
                  )->ck_ativo_empresa( i_empresa = me->carga-id_bukrs
                  )->ck_parceiro_local_negocio(
                  ).
              ENDIF.

            WHEN zif_carga=>st_tp_frete_cpt.

              IF me->carga-id_agent_frete IS NOT INITIAL AND me->carga-tp_status NE zif_carga=>st_status_cancelada.
                ob_fornecedores = zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = me->carga-id_agent_frete
                  )->ck_ativo(
                  )->ck_ativo_empresa( i_empresa = me->carga-id_bukrs
                  )->ck_parceiro_terceiro(
                  )->ck_servico_frete(
                  )->ck_servico_frete_rodo(
                  ).
              ENDIF.

*-CS2021000253-26.04.2024-#59941-JT-inicio
              IF me->carga-id_ordem IS INITIAL.
                ordem_venda->ck_grupo_mercadoria( e_tipo_frete ).
              ENDIF.
*-CS2021000253-26.04.2024-#59941-JT-fim

            WHEN zif_carga=>st_tp_frete_cfr.

              "Não Valida Agente de Frete
              IF me->carga-id_agent_frete IS NOT INITIAL AND me->carga-tp_status NE zif_carga=>st_status_cancelada.
                ob_fornecedores = zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = me->carga-id_agent_frete
                  )->ck_ativo(
                  )->ck_ativo_empresa( i_empresa = me->carga-id_bukrs
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
          i_id_ordem       = me->carga-id_ordem    " Ordem de Carregamento
      ).

    ENDLOOP.
    CLEAR: ordem_venda.

  ENDMETHOD.


  METHOD ZIF_CARGA~VALIDAR_REGISTRO_PEDIDO_COMPRA.

    LOOP AT ME->ZIF_CARGA~PEDIDO_COMPRA INTO DATA(WA_PEDIDO_COMPRA).

      DATA: LC_CLIENTE TYPE ZDE_ID_LOCAL_DESTINO.

      ME->ZIF_CARGA~GET_INFO_PEDIDO_COMPRA( EXPORTING I_PEDIDO_COMPRA = WA_PEDIDO_COMPRA-NR_PEDIDO_COMPRA IMPORTING E_PEDIDO = DATA(E_PEDIDO) ).

      IF E_PEDIDO-ID_CENTRO_SAIDA NE ME->CARGA-ID_BRANCH.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_EF_SAIDA-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_EF_SAIDA-MSGNO
                              ATTR1 = CONV #( ME->CARGA-ID_BRANCH ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_EF_SAIDA-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_EF_SAIDA-MSGID
            MSGV1  = CONV #( ME->CARGA-ID_BRANCH ).
      ENDIF.

      LC_CLIENTE = E_PEDIDO-ID_CENTRO_RECEBEDOR.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LC_CLIENTE
        IMPORTING
          OUTPUT = LC_CLIENTE.

      ZCL_CLIENTES=>ZIF_PARCEIROS~GET_INSTANCE(
       )->SET_PARCEIRO( I_PARCEIRO = LC_CLIENTE
       )->CK_PARCEIRO_INTERCOMPANY( I_EMPRESA = ME->CARGA-ID_BUKRS
       ).

      IF LC_CLIENTE NE ME->CARGA-ID_LOCAL_DESTINO.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_EF_ENTRA-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_EF_ENTRA-MSGNO
                              ATTR1 = CONV #( E_PEDIDO-ID_CENTRO_RECEBEDOR ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_EF_ENTRA-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_EF_ENTRA-MSGID
            MSGV1  = CONV #( E_PEDIDO-ID_CENTRO_RECEBEDOR ).
      ENDIF.

      IF LC_CLIENTE NE ME->CARGA-ID_LOCAL_DESCARGA.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_EF_ENTRA-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_EF_ENTRA-MSGNO
                              ATTR1 = CONV #( E_PEDIDO-ID_CENTRO_RECEBEDOR ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_EF_ENTRA-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_EF_ENTRA-MSGID
            MSGV1  = CONV #( E_PEDIDO-ID_CENTRO_RECEBEDOR ).
      ENDIF.

      "Verificar E_PEDIDO-ID_CENTRO_RECEBEDOR se é intercompany

      IF E_PEDIDO-NR_SAFRA NE ME->CARGA-NR_SAFRA.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_SAFRA-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_SAFRA-MSGNO
                              ATTR1 = CONV #( ME->CARGA-NR_SAFRA ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_SAFRA-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_PEDIDO_COMPRA_SAFRA-MSGID
            MSGV1  = CONV #( ME->CARGA-NR_SAFRA ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  method zif_carga~valida_atributo_alteravel.

    r_carga = me.

    e_permitido = abap_false.

    check me->carga-tp_status eq zif_carga=>st_status_aberto.

    data(lc_emitiu) = abap_false.
    loop at me->documento_fiscal into data(wa_documento_fiscal).
      if "WA_DOCUMENTO_FISCAL-AV_VBELN IS NOT INITIAL OR
         wa_documento_fiscal-fte_docnum is not initial or
         wa_documento_fiscal-fte_fknum is not initial or
         wa_documento_fiscal-fte_tknum is not initial or
         wa_documento_fiscal-fte_vbeln_va is not initial or
         wa_documento_fiscal-fte_vbeln_vf is not initial.
        lc_emitiu = abap_true.
      endif.
    endloop.

    check lc_emitiu eq abap_false.

    check not ( me->carga-ck_enviado_opus eq abap_true and me->carga-ck_recebido_opus eq abap_false ).

    "Campos Não Alteráveis da Carga
    case i_campo.
      when 'IN_SRR_DECLARADO'. exit.
      when 'ID_CARGA'. exit.
        "WHEN 'NR_ORDEM_VENDA'. EXIT.
      when 'DT_MOVIMENTO'. exit.
      when 'NR_SAFRA'. exit.
      when 'ID_BUKRS'. exit.
      when 'ID_BRANCH'. exit.
      when 'ID_BUKRS_AG'. exit.
      when 'ID_BRANCH_AG'. exit.
      when 'ID_PRODUTO'. exit.
      when 'TP_STATUS'. exit.
      when 'DT_ABERTURA'. exit.
      when 'HR_ABERTURA'. exit.
      when 'DT_FECHAMENTO'. exit.
      when 'HR_FECHAMENTO'. exit.
      when 'NM_PESO_SUBTOTAL'. exit.
      when 'NR_RES_RR1_RR2'. exit.
      when 'NR_QTDE_UMI'. exit.
      when 'NR_QTDE_IMP'. exit.
      when 'NR_QTDE_AVA'. exit.
      when 'NR_QTDE_ARD'. exit.
      when 'NR_QTDE_QUE'. exit.
      when 'NR_QTDE_ESV'. exit.
      when 'NR_QTDE_CAR'. exit.
      when 'ID_LOCAL_COLETA'. exit.
      when 'ID_LOCAL_DESCARGA'. exit.
      when 'ID_LOCAL_DESTINO'. exit.
      when 'NM_PESO_DESCONTOS'. exit.
      when 'NM_PESO_LIQUIDO'. exit.
        "Nota Fiscal
      when 'ID_NOTA'. exit.
      when 'ID_FORNECEDOR'. exit.
      when 'NR_PRECO_SACA_60'. exit.
      when 'NR_PEDIDO'. exit.
      when 'NR_PEDIDO'. exit.
      when 'NR_PEDIDO_ITEM'. exit.
      when 'TP_TIPO_ORDEM'. exit.
      when 'DS_TIPO_ORDEM'. exit.
      when 'DS_TIPO_FRETE'. exit.
      when 'NR_ROMANEIO_ENT'. exit.
      when 'NR_ROMANEIO_SAI'. exit.
    endcase.

    "Campos Não Alteráveis da Carga Entrada
    case i_campo.
      when 'TP_FRETE'. exit.
      when 'IN_TRANSFERENCIA'. exit.
        "WHEN 'CK_GERA_AVISO'. EXIT.
    endcase.

    if me->carga-id_ordem is not initial.
      if i_campo = 'ID_MOTORISTA'.
        exit.
      endif.
    endif.

    if i_campo eq 'NR_PERC_CAR'.

      select single * into @data(wa_mara)
        from mara
       where matnr eq @me->carga-id_produto.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_mara-matkl
        importing
          output = wa_mara-matkl.

      "Somente Milho possui Carunchado
      if wa_mara-matkl ne '700170'.
        exit.
      endif.

    endif.

    if i_campo eq 'NR_PERC_UMI' or
       i_campo eq 'NR_PERC_IMP' or
       i_campo eq 'NR_PERC_AVA' or
       i_campo eq 'NR_PERC_ARD' or
       i_campo eq 'NR_PERC_QUE' or
       i_campo eq 'NR_PERC_ESV' or
       i_campo eq 'NR_PERC_CAR'.

      select single * into @wa_mara
        from mara
       where matnr eq @me->carga-id_produto.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_mara-matkl
        importing
          output = wa_mara-matkl.

      case wa_mara-matkl.

        when zif_carga=>st_grupo_algodao_pluma. "Algodão.
          exit.
      endcase.

    endif.

*(Campo 1) IN_GMO
*(Campo 2) IN_SRR_DECLARADO
*(Campo 3) IN_SRR_DECLARADO_2
*(Campo 4) IN_TESTE_SRR_2
*(campo 5) IN_SRR_ORIGEM_PARTIC

    if i_campo eq 'IN_GMO' or
       i_campo eq 'IN_SRR_DECLARADO' or
       i_campo eq 'IN_SRR_DECLARADO_2' or
       i_campo eq 'IN_TESTE_SRR_2' or
       i_campo eq 'IN_SRR_ORIGEM_PARTIC'.

      select single * into @wa_mara
        from mara
       where matnr eq @me->carga-id_produto.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_mara-matkl
        importing
          output = wa_mara-matkl.

      case wa_mara-matkl.

        when '700330'. "Algodão.
          exit.

        when '700170'. "Milho.
          exit.

        when '700110'. "Soja.

          if me->classificacao-in_gmo ne zif_carga=>st_gmo_nao_testado and i_campo ne 'IN_GMO'.
            if not
              ( ( me->carga-id_branch eq '0155' or me->carga-id_branch eq '0119' or me->carga-id_branch eq '0161' ) and i_campo eq 'IN_SRR_DECLARADO' ).
              exit.
            endif.
          endif.

          if me->classificacao-in_srr_declarado_2 eq abap_true and i_campo ne 'IN_SRR_DECLARADO_2'.
            exit.
          endif.

          if me->classificacao-in_teste_srr_2 ne zif_carga=>st_gmo_nao_testado and i_campo ne 'IN_TESTE_SRR_2'.
            exit.
          endif.

          if me->classificacao-in_srr_origem_partic eq abap_true and ( i_campo ne 'IN_SRR_ORIGEM_PARTIC' and i_campo ne 'ID_OUTRO_PARTIC' ).
            exit.
          endif.

        when others.
          exit.
      endcase.

    endif.

    "Se Ordem de Venda CIF
    if ( i_campo eq 'DS_PLACA_TRATOR'   or
         i_campo eq 'DS_PLACA_REBOQ_1'  or
         i_campo eq 'DS_PLACA_REBOQ_2'  or
         i_campo eq 'DS_PLACA_REBOQ_3'  or
         i_campo eq 'ID_MOTORISTA'      or
         i_campo eq 'ID_PROPRIETARIO' ) and
       ( me->zif_carga~at_tipo_frete_ordem_venda eq zif_carga=>st_tp_frete_cif or me->zif_carga~at_tipo_frete_ordem_venda is initial ).
      if not ( i_campo eq 'ID_MOTORISTA' and me->zif_carga~at_manutencao eq abap_true ).
        exit.
      endif.
    endif.

    if i_id_entrada is not initial and i_id_empresa is not initial.

      case i_modelo_fiscal.
        when zif_carga=>st_model_fiscal_papel.
          data(lc_ck_nfe)  = abap_false.
          data(lc_ck_form) = abap_false.
        when zif_carga=>st_model_fiscal_eletronico.
          lc_ck_nfe = abap_true.
      endcase.

      select single * into @data(wa_entrada)
        from zsdt0001te
       where id_entrada eq @i_id_entrada
         and id_empresa eq @i_id_empresa
         and ck_nfe     eq @lc_ck_nfe.

      if sy-subrc is initial.
        select single * into @data(wa_j_1baa)
          from j_1baa
         where nftype eq @wa_entrada-ct_nota.

        if wa_j_1baa-form is not initial.
          lc_ck_form = abap_true.
        endif.
      endif.

    else.
      lc_ck_nfe  = abap_false.
      lc_ck_form = abap_false.
    endif.

    if lc_ck_nfe eq abap_true and
       ( i_campo eq 'NR_FORNECEDOR_IE' or
         i_campo eq 'NR_NOTA' or
         i_campo eq 'NM_SERIE' or
         i_campo eq 'DT_EMISSAO' or
         i_campo eq 'DT_VENCIMENTO_FORM' or
         i_campo eq 'CFOP' ).
      exit.
    endif.

    if lc_ck_nfe eq abap_true and lc_ck_form eq abap_false and ( i_campo eq 'NR_QUANTIDADE' or i_campo eq 'NR_VALOR' ).
      exit.
    endif.

    if ( i_campo eq 'NR_RESULTADO_01' or
         i_campo eq 'NR_RESULTADO_02' ) and
       me->classificacao-in_gmo ne zif_carga=>st_gmo_negativo.
      exit.
    endif.

    if me->classificacao-in_srr_origem_partic eq abap_false and
       i_campo eq 'ID_OUTRO_PARTIC'.
      exit.
    endif.

    if me->zif_carga~at_manutencao eq abap_true.
      check i_campo eq 'ID_PROPRIETARIO'   or
            i_campo eq 'DS_PLACA_TRATOR'   or
            i_campo eq 'DS_PLACA_REBOQ_1'  or
            i_campo eq 'DS_PLACA_REBOQ_2'  or
            i_campo eq 'DS_PLACA_REBOQ_3'  or
            i_campo eq 'ID_MOTORISTA'      or
            i_campo eq 'NR_TICKET'         or
            i_campo eq 'ID_CLASSIFICADORA' or
            i_campo eq 'ID_LOCAL_ENTREGA'  or
            i_campo eq 'NM_PESO_BRUTO'     or
            i_campo eq 'NM_PESO_TARA'      or
            i_campo eq 'NM_PESO_SUBTOTAL'  or
            i_campo eq 'ID_ENTRADA'        or
            i_campo eq 'ID_FORNECEDOR'     or
            i_campo eq 'NR_FORNECEDOR_IE'  or
            i_campo eq 'ID_MOD_FISCAL'     or
            i_campo eq 'NR_NOTA'           or
            i_campo eq 'NR_QUANTIDADE'     or
            i_campo eq 'NR_VALOR' or
            i_campo eq 'DT_EMISSAO' or
            i_campo eq 'DT_VENCIMENTO_FORM' or
            i_campo eq 'CFOP' or
            i_campo eq 'NM_SERIE' or
            i_campo eq 'NR_CHAVE_NFE' or
            i_campo eq 'ID_ENTREGUE_POR' or
            i_campo eq 'NM_PESO_SUBTOTAL' or
            i_campo eq 'ID_OUTRO_PARTIC' or
            i_campo eq 'TP_TRANSGENIA' or
            i_campo eq 'IN_GMO' or
            i_campo eq 'NR_RESULTADO_01' or
            i_campo eq 'NR_RESULTADO_02' or
            i_campo eq 'NR_RES_RR1_RR2' or
            i_campo eq 'IN_GMO_03' or
            i_campo eq 'IN_SRR_ORIGEM_PARTIC' or
            i_campo eq 'IN_SRR_DECLARADO' or
            i_campo eq 'IN_TESTE_SRR' or
            i_campo eq 'IN_SRR_DECLARADO_2' or
            i_campo eq 'IN_TESTE_SRR_2' or
            i_campo eq 'NR_PERC_UMI' or
            i_campo eq 'NR_PERC_IMP' or
            i_campo eq 'NR_PERC_AVA' or
*            I_CAMPO EQ 'NR_PERC_ARD' OR "US156300 bloquear campo ardido
            i_campo eq 'NR_PERC_QUE' or
            i_campo eq 'NR_PERC_ESV' or
            i_campo eq 'CK_CLASS_DEST' or
            i_campo eq 'NR_PERC_CAR'.

      e_permitido = abap_true.

    else.
      check  i_campo ne 'NR_PERC_ARD'. "US156300 bloquear campo ardido
      e_permitido = abap_true.
    endif.

  endmethod.


  method zif_carga~verif_alteracao_manut_romaneio.

    data: lc_alterou_mes         type char01,
          lc_peso_old            type c length 09,
          lc_peso                type c length 09,
          lc_inteiro             type i,
          ck_alterou_takeup      type char01,
          ck_alterar_nota_fiscal type char01.

    r_carga = me.

    ck_alterou_takeup = abap_false.

    check me->zif_carga~at_manutencao eq abap_true.
    check me->zif_carga~ck_executar_manutencao_entrada eq abap_false.
    check me->zif_carga~solicitacao_manutencao-ck_processou_ajuste eq abap_false.

    clear: e_alteracoes.

    create object e_carga_original type zcl_carga_recebimento.

    e_carga_original->set_registro( i_id_carga = me->carga-id_carga i_no_enqueue = abap_true ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Carregar Atributos do WorkFlow """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "Deve Ser Emitido uma Carta de Correção para Alteração de Placa
    if me->carga-ds_placa_trator <> e_carga_original->carga-ds_placa_trator.
      e_ck_aceite_filial    = abap_true.
      e_alteracoes-al_ds_placa_trator = abap_true.
      e_obs_alteracao = e_obs_alteracao && ' Alterado Placa do Veículo de Tração'.
    endif.

    if me->carga-ds_placa_reboq_1 <> e_carga_original->carga-ds_placa_reboq_1.
      e_ck_aceite_filial    = abap_true.
      e_alteracoes-al_ds_placa_reboq_1 = abap_true.
      e_obs_alteracao = e_obs_alteracao && ' Alterado Placa do Reboque 1'.
    endif.

    if me->carga-ds_placa_reboq_2 <> e_carga_original->carga-ds_placa_reboq_2.
      e_ck_aceite_filial    = abap_true.
      e_alteracoes-al_ds_placa_reboq_2 = abap_true.
      e_obs_alteracao = e_obs_alteracao && ' Alterado Placa do Reboque 2'.
    endif.

    if me->carga-ds_placa_reboq_3 <> e_carga_original->carga-ds_placa_reboq_3.
      e_ck_aceite_filial    = abap_true.
      e_alteracoes-al_ds_placa_reboq_3 = abap_true.
      e_obs_alteracao = e_obs_alteracao && ' Alterado Placa do Reboque 3'.
    endif.

    "Deve Ser Emitido uma Carta de Correção para Alteração de Proprietário (NF-e/CT-e)
    "Se For CIF o RNTRC é do Terceiro
    if me->carga-id_proprietario <> e_carga_original->carga-id_proprietario
      and e_carga_original->at_tipo_frete_ordem_venda eq zif_carga=>st_tp_frete_cif.
      e_ck_aceite_filial = abap_true.
      e_alteracoes-al_id_proprietario = abap_true.
      e_obs_alteracao = e_obs_alteracao && ' Alterado Proprietário do Conjunto Veicular'.
    endif.

    "Deve Ser Emitido uma Carta de Correção para Alteração de Motorista (CT-e)
    "Se For CIF o RNTRC é do Terceiro
    if me->carga-id_motorista <> e_carga_original->carga-id_motorista.
      e_ck_aceite_filial = abap_true.
      e_alteracoes-al_id_motorista = abap_true.
      e_obs_alteracao = e_obs_alteracao && ' Alterado Motorista'.
    endif.

    "Deve Ser Verificação do Fluxo de Pagamento de Classificação
    if me->classificacao-id_classificadora <> e_carga_original->classificacao-id_classificadora.
      e_ck_aceite_filial = abap_true.
      e_alteracoes-al_id_classificadora = abap_true.
***///CS2021000581 Classificação destino - Flag para marcação - ZMM0127 - Set 2021 - Inicio
      if me->classificacao-id_classificadora  is initial.
        e_obs_alteracao = e_obs_alteracao && ' Classificação no destino'.
      endif.
***///CS2021000581 Classificação destino - Flag para marcação - ZMM0127 - Set 2021 - Fim
    endif.

    "Tipo de Trangeníase da Carga
    if me->classificacao-tp_transgenia ne e_carga_original->classificacao-tp_transgenia.
      e_alteracoes-al_tp_transgenia = abap_true.
      e_ck_aceite_filial = abap_true.
      e_obs_alteracao = e_obs_alteracao && ' Alterado a Transgenia'.
    endif.

    "US 143677 - transgenia por nota
    loop at me->classificacao_notas into data(wclass).
      read table e_carga_original->classificacao_notas into data(wclass_ori) with key id_classificacao = wclass-id_classificacao.
      if sy-subrc = 0.
        if wclass-tp_transgenia ne wclass_ori-tp_transgenia.
          e_alteracoes-al_tp_transgenia = abap_true.
          e_ck_aceite_filial = abap_true.
          e_obs_alteracao = e_obs_alteracao && ' Alterado a Transgenia'.
        endif.
        "
        "Alteração de Trangeníase
        if wclass-in_gmo               ne wclass_ori-in_gmo or
           wclass-nr_resultado_01      ne wclass_ori-nr_resultado_01 or
           wclass-nr_resultado_02      ne wclass_ori-nr_resultado_02 or
           wclass-nr_res_rr1_rr2       ne wclass_ori-nr_res_rr1_rr2 or
           wclass-in_gmo_03            ne wclass_ori-in_gmo_03 or
           wclass-in_srr_origem_partic ne wclass_ori-in_srr_origem_partic or
           wclass-id_outro_partic      ne wclass_ori-id_outro_partic or
           wclass-in_srr_declarado     ne wclass_ori-in_srr_declarado or
           wclass-in_teste_srr         ne wclass_ori-in_teste_srr or
           wclass-in_srr_declarado_2   ne wclass_ori-in_srr_declarado_2 or
           wclass-in_teste_srr_2       ne wclass_ori-in_teste_srr_2.

          e_obs_alteracao = e_obs_alteracao && ' Alterado a Transgeníase'.

          e_ck_aceite_filial = abap_true.
          if lc_alterou_mes eq abap_true.
            e_ck_aceite_comercial = abap_true.
          endif.

          if wclass-in_gmo ne wclass_ori-in_gmo.
            e_alteracoes-al_in_gmo = abap_true.
          endif.
          if wclass-nr_resultado_01 ne wclass_ori-nr_resultado_01.
            e_alteracoes-al_nr_resultado_01 = abap_true.
          endif.
          if wclass-nr_resultado_02 ne wclass_ori-nr_resultado_02.
            e_alteracoes-al_nr_resultado_02 = abap_true.
          endif.

          if wclass-nr_res_rr1_rr2 ne wclass_ori-nr_res_rr1_rr2.
            e_alteracoes-al_nr_res_rr1_rr2 = abap_true.
          endif.

          if wclass-in_gmo_03 ne wclass_ori-in_gmo_03.
            e_alteracoes-al_in_gmo_03 = abap_true.
          endif.

          if wclass-in_srr_origem_partic ne wclass_ori-in_srr_origem_partic.
            e_alteracoes-al_in_srr_origem_partic = abap_true.
          endif.

          if wclass-id_outro_partic ne wclass_ori-id_outro_partic.
            e_alteracoes-al_id_outro_partic = abap_true.
          endif.

          if wclass-in_srr_declarado ne wclass_ori-in_srr_declarado.
            e_alteracoes-al_in_srr_declarado = abap_true.
          endif.

          if wclass-in_teste_srr ne wclass_ori-in_teste_srr.
            e_alteracoes-al_in_teste_srr = abap_true.
          endif.

          if wclass-in_srr_declarado_2 ne wclass_ori-in_srr_declarado_2.
            e_alteracoes-al_in_srr_declarado_2 = abap_true.
          endif.

          if wclass-in_teste_srr_2 ne wclass_ori-in_teste_srr_2.
            e_alteracoes-al_in_teste_srr_2 = abap_true.
          endif.

        endif.
      endif.
    endloop.
    "US 143677 - transgenia por nota


    "Se Alterou Ticket
    if me->carga-nr_ticket <> e_carga_original->carga-nr_ticket.
      e_ck_aceite_filial = abap_true.
      e_alteracoes-al_nr_ticket = abap_true.
      e_obs_alteracao = e_obs_alteracao && ' Alterado o Ticket'.
    endif.

    "Altera a Entrada
    if me->carga-id_local_entrega <> e_carga_original->carga-id_local_entrega .
      "AND ME->AT_CARGA_ORIGINAL->AT_TIPO_FRETE_ORDEM_VENDA EQ ZIF_CARGA=>ST_TP_FRETE_CIF.
      e_ck_aceite_filial = abap_true.
      e_alteracoes-al_id_local_entrega = abap_true.
      e_obs_alteracao = e_obs_alteracao && ' Alterado o Local de Entrega'.
    endif.

    "Verificar Alteração de Classificação
    loop at me->resultado into data(wa_resultado).
      read table e_carga_original->resultado into data(wa_res_original)
      with key tp_caracteristica = wa_resultado-tp_caracteristica.
      if sy-subrc is initial.

        if wa_res_original-nr_percentual_com ne wa_resultado-nr_percentual_com.
          e_ck_aceite_filial = abap_true.
          case wa_resultado-tp_caracteristica.
            when zif_carga=>st_tp_caract_class_umidade.
              e_alteracoes-al_nr_perc_umi = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado o Percentual de Umidade'.
            when zif_carga=>st_tp_caract_class_impureza.
              e_alteracoes-al_nr_perc_imp = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado o Percentual de Impureza'.
            when zif_carga=>st_tp_caract_class_avariado.
              e_alteracoes-al_nr_perc_ava = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado o Percentual de Avariado'.
            when zif_carga=>st_tp_caract_class_ardido.
              e_alteracoes-al_nr_perc_ard = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado o Percentual de Ardido'.
            when zif_carga=>st_tp_caract_class_quebrado.
              e_alteracoes-al_nr_perc_que = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado o Percentual de Quebrado'.
            when zif_carga=>st_tp_caract_class_esverdeado.
              e_alteracoes-al_nr_perc_esv = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado o Percentual de Esverdeado'.
            when zif_carga=>st_tp_caract_class_carunchado.
              e_alteracoes-al_nr_perc_car = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado o Percentual de Carunchado'.
          endcase.
        endif.

        if wa_res_original-nr_quantidade_com ne wa_resultado-nr_quantidade_com.
          e_ck_aceite_filial = abap_true.
          case wa_resultado-tp_caracteristica.
            when zif_carga=>st_tp_caract_class_umidade.
              e_alteracoes-al_nr_qtda_umi = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado a Quantidade de Desconto de Umidade'.
            when zif_carga=>st_tp_caract_class_impureza.
              e_alteracoes-al_nr_qtda_imp = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado a Quantidade de Desconto de Impureza'.
            when zif_carga=>st_tp_caract_class_avariado.
              e_alteracoes-al_nr_qtda_ava = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado a Quantidade de Desconto de Avariado'.
            when zif_carga=>st_tp_caract_class_ardido.
              e_alteracoes-al_nr_qtda_ard = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado a Quantidade de Desconto de Ardido'.
            when zif_carga=>st_tp_caract_class_quebrado.
              e_alteracoes-al_nr_qtda_que = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado a Quantidade de Desconto de Quebrado'.
            when zif_carga=>st_tp_caract_class_esverdeado.
              e_alteracoes-al_nr_qtda_esv = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado a Quantidade de Desconto de Esverdeado'.
            when zif_carga=>st_tp_caract_class_carunchado.
              e_alteracoes-al_nr_qtda_car = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado a Quantidade de Desconto de Carunchado'.
          endcase.
        endif.

      endif.
    endloop.

    "IF E_CK_ACEITE_FISCAL EQ ABAP_TRUE.
    if e_carga_original->carga-dt_movimento(6) eq sy-datum(6).
      lc_alterou_mes = abap_false.
    else.
      lc_alterou_mes = abap_true.
    endif.
    "ENDIF.

    data(ck_alterado_imposto) = abap_false.

    loop at me->documento_fiscal into data(wa_documento_fiscal).

      ck_alterar_nota_fiscal = abap_false.

      read table e_carga_original->documento_fiscal with key id_carga = wa_documento_fiscal-id_carga
                                                             id_nota  = wa_documento_fiscal-id_nota
                                                    into data(wa_doc_original).
      if sy-subrc is initial.

        if wa_documento_fiscal-id_entrada ne wa_doc_original-id_entrada.
          ck_alterar_nota_fiscal = abap_true.
          e_alteracoes-al_id_entrada = abap_true.
          e_ck_aceite_filial      = abap_true.
          e_obs_alteracao = e_obs_alteracao && ' Alterado o Tipo de Entrada da Nota Fiscal ' && wa_doc_original-nr_nota.
          if lc_alterou_mes eq abap_true.
            e_ck_aceite_fiscal = abap_true.
          endif.
        endif.

        if wa_documento_fiscal-id_fornecedor ne wa_doc_original-id_fornecedor.
          ck_alterar_nota_fiscal = abap_true.
          e_alteracoes-al_id_fornecedor = abap_true.
          e_ck_aceite_filial = abap_true.
          e_obs_alteracao = e_obs_alteracao && ' Alterado o Fornecedor da Nota Fiscal ' && wa_doc_original-nr_nota.
          if lc_alterou_mes eq abap_true.
            e_ck_aceite_fiscal = abap_true.
          endif.
        endif.

        if wa_documento_fiscal-id_mod_fiscal ne wa_doc_original-id_mod_fiscal.
          ck_alterar_nota_fiscal = abap_true.
          e_alteracoes-al_id_mod_fiscal = abap_true.
          e_ck_aceite_filial = abap_true.
          e_obs_alteracao = e_obs_alteracao && ' Alterado o Modelo do Documento Fiscal da Nota Fiscal ' && wa_doc_original-nr_nota.
          if lc_alterou_mes eq abap_true.
            e_ck_aceite_fiscal = abap_true.
          endif.
        endif.

        if wa_documento_fiscal-nr_nota ne wa_doc_original-nr_nota.
          ck_alterar_nota_fiscal = abap_true.
          e_alteracoes-al_nr_nota = abap_true.
          e_ck_aceite_filial = abap_true.
          e_obs_alteracao = e_obs_alteracao && ' Alterado o Número da Nota Fiscal ' && wa_doc_original-nr_nota.
          if lc_alterou_mes eq abap_true.
            e_ck_aceite_fiscal = abap_true.
          endif.
        endif.

        if wa_documento_fiscal-nm_serie ne wa_doc_original-nm_serie.
          ck_alterar_nota_fiscal = abap_true.
          e_alteracoes-al_nm_serie = abap_true.
          e_ck_aceite_filial = abap_true.
          e_obs_alteracao = e_obs_alteracao && ' Alterado a Série da Nota Fiscal ' && wa_doc_original-nr_nota.
          if lc_alterou_mes eq abap_true.
            e_ck_aceite_fiscal = abap_true.
          endif.
        endif.

        if wa_documento_fiscal-nr_quantidade ne wa_doc_original-nr_quantidade.
          ck_alterar_nota_fiscal = abap_true.
          e_alteracoes-al_nr_quantidade = abap_true.
          e_ck_aceite_filial = abap_true.
          e_obs_alteracao = e_obs_alteracao && ' Alterado a Quantidade da Nota Fiscal ' && wa_doc_original-nr_nota.
          if lc_alterou_mes eq abap_true.
            e_ck_aceite_fiscal = abap_true.
          endif.
        endif.

        if wa_documento_fiscal-nr_valor ne wa_doc_original-nr_valor.
          ck_alterar_nota_fiscal = abap_true.
          e_alteracoes-al_nr_valor = abap_true.
          e_ck_aceite_filial = abap_true.
          e_obs_alteracao = e_obs_alteracao && ' Alterado o Valor da Nota Fiscal ' && wa_doc_original-nr_nota.
          if lc_alterou_mes eq abap_true.
            e_ck_aceite_fiscal = abap_true.
          endif.
        endif.

        if wa_documento_fiscal-dt_emissao ne wa_doc_original-dt_emissao.
          ck_alterar_nota_fiscal = abap_true.
          e_alteracoes-al_dt_emissao = abap_true.
          e_ck_aceite_filial = abap_true.
          e_obs_alteracao = e_obs_alteracao && ' Alterado a Data de Emissão da Nota Fiscal ' && wa_doc_original-nr_nota.
          if lc_alterou_mes eq abap_true.
            e_ck_aceite_fiscal = abap_true.
          endif.
        endif.

        if wa_documento_fiscal-dt_vencimento_form ne wa_doc_original-dt_vencimento_form.
          e_alteracoes-al_dt_vencimento_form = abap_true.
          e_ck_aceite_filial = abap_true.
          e_obs_alteracao = e_obs_alteracao && ' Alterado o Vencimento do Formulário da Nota Fiscal ' && wa_doc_original-nr_nota.
          if lc_alterou_mes eq abap_true.
            e_ck_aceite_fiscal = abap_true.
          endif.
        endif.

        if wa_documento_fiscal-cfop ne wa_doc_original-cfop.
          ck_alterar_nota_fiscal = abap_true.
          e_alteracoes-al_cfop = abap_true.
          e_ck_aceite_filial = abap_true.
          e_obs_alteracao = e_obs_alteracao && ' Alterado o CFOP da Nota Fiscal ' && wa_doc_original-nr_nota.
          if lc_alterou_mes eq abap_true.
            e_ck_aceite_fiscal = abap_true.
          endif.
        endif.

        if wa_documento_fiscal-nr_chave_nfe ne wa_doc_original-nr_chave_nfe.
          ck_alterar_nota_fiscal = abap_true.
          e_alteracoes-al_nr_chave_nfe = abap_true.
          e_ck_aceite_filial = abap_true.
          e_obs_alteracao = e_obs_alteracao && ' Alterado a Chave da NF-e da Nota Fiscal ' && wa_doc_original-nr_nota.
          if lc_alterou_mes eq abap_true.
            e_ck_aceite_fiscal = abap_true.
          endif.
        endif.

        if wa_documento_fiscal-id_entregue_por ne wa_doc_original-id_entregue_por.
          e_alteracoes-al_id_entregue_por = abap_true.
          e_ck_aceite_filial = abap_true.
          e_obs_alteracao = e_obs_alteracao && ' Alterado o Entregue por da Nota Fiscal ' && wa_doc_original-nr_nota.
        endif.

        "Verifica alterações de Impostos """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " Imposto só altera se alterar a nota fiscal """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        if ck_alterar_nota_fiscal eq abap_true.
          data(it_importo_atual) = me->documento_fiscal_imp_ret[].
          data(it_importo_origi) = e_carga_original->documento_fiscal_imp_ret[].
          delete it_importo_atual where id_nota ne wa_documento_fiscal-id_nota.
          delete it_importo_origi where id_nota ne wa_documento_fiscal-id_nota.

          describe table it_importo_atual lines data(qtd_imp_1).
          describe table it_importo_origi lines data(qtd_imp_2).

          if ( it_importo_atual[] is initial and it_importo_origi[] is not initial ) or
             ( it_importo_atual[] is not initial and it_importo_origi[] is initial ) or
             ( qtd_imp_1 ne qtd_imp_2 ) or
            ck_alterado_imposto = abap_true.
            e_obs_alteracao = e_obs_alteracao && ' Alterado Imposto(s) da(s) nota(s) fiscal(is) '.
          endif.
        endif.

        if ck_alterado_imposto eq abap_false and ck_alterar_nota_fiscal eq abap_true.
          loop at me->documento_fiscal_imp_ret into data(wa_imp_ret) where id_carga eq wa_documento_fiscal-id_carga
                                                                       and id_nota  eq wa_documento_fiscal-id_nota.

            read table e_carga_original->documento_fiscal_imp_ret
            into data(wa_imp_ret_origem)
            with key id_carga = wa_imp_ret-id_carga
                     id_nota  = wa_imp_ret-id_nota
                     id_lanc_imposto = wa_imp_ret-id_lanc_imposto.
            if sy-subrc is initial.
              if wa_imp_ret ne wa_imp_ret_origem.
                ck_alterar_nota_fiscal = abap_true.
                ck_alterado_imposto = abap_true.
                e_obs_alteracao = e_obs_alteracao && ' Alterado Imposto da nota fiscal ' &&  wa_documento_fiscal-nr_nota.
              endif.
            else.
              ck_alterar_nota_fiscal = abap_true.
              ck_alterado_imposto = abap_true.
              e_obs_alteracao = e_obs_alteracao && ' Alterado Imposto da nota fiscal ' &&  wa_documento_fiscal-nr_nota.
            endif.

          endloop.
        endif.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      else.
        e_ck_aceite_filial = abap_true.
        data(ds_desembramento) = | INCLUIDO documento fiscal em desembramento ( { wa_documento_fiscal-nr_nota }/{ wa_documento_fiscal-nm_serie } ) |.
        e_obs_alteracao = e_obs_alteracao && ds_desembramento.
      endif.

    endloop.

    loop at e_carga_original->documento_fiscal into wa_documento_fiscal.
      read table me->documento_fiscal with key id_carga = wa_documento_fiscal-id_carga
                                               id_nota  = wa_documento_fiscal-id_nota
                                               transporting no fields.
      if sy-subrc is not initial.
        e_ck_aceite_filial = abap_true.
        ds_desembramento = | REMOVIDO documento fiscal em desembramento ( { wa_documento_fiscal-nr_nota }/{ wa_documento_fiscal-nm_serie } ) |.
        e_obs_alteracao = e_obs_alteracao && ds_desembramento.
      endif.
    endloop.

    if ck_alterar_nota_fiscal eq abap_true.

      "Verificar se as Notas podem ser estornadas """""""""""""""""""""""""""""""""""""""""""""""""
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      data(notas) = e_carga_original->documento_fiscal[].
      delete notas where docnum is initial.
      if notas[] is not initial.
        select * into table @data(it_exportacao)
          from zdoc_nf_produtor
           for all entries in @notas
         where docnum_prod eq @notas-docnum.
      endif.

      if it_exportacao[] is not initial.
        read table it_exportacao into data(wa_exportacao) index 1.

        raise exception type zcx_carga
          exporting
            textid = value #( msgid = zcx_carga=>zcx_nf_exportada-msgid
                              msgno = zcx_carga=>zcx_nf_exportada-msgno
                              attr1 = wa_exportacao-docnum_prod
                              attr2 = wa_exportacao-vbeln )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_nf_exportada-msgid
            msgno  = zcx_carga=>zcx_nf_exportada-msgno
            msgv1  = conv #( wa_exportacao-docnum_prod )
            msgv2  = conv #( wa_exportacao-vbeln ).
      endif.

    endif.

    loop at me->zif_carga~take_up into data(wa_take_up_manut).

      read table e_carga_original->take_up
        into data(wa_take_up_original)
        with key id_carga  = wa_take_up_manut-id_carga
                 id_nota   = wa_take_up_manut-id_nota
                 id_takeup = wa_take_up_manut-id_takeup
                 nu_bloco  = wa_take_up_manut-nu_bloco.

      if sy-subrc is not initial.

        lc_inteiro = wa_take_up_manut-qt_vinculada.
        write lc_inteiro to lc_peso.
        condense lc_peso no-gaps.

        e_obs_alteracao =
        e_obs_alteracao && ' Incluído Take UP ' &&
        wa_take_up_manut-id_takeup && ' Bloco ' && wa_take_up_manut-nu_bloco &&
        'Quantidade de ' && lc_peso.
        ck_alterou_takeup = abap_true.

      elseif wa_take_up_manut-qt_vinculada ne wa_take_up_original-qt_vinculada.

        lc_inteiro = wa_take_up_original-qt_vinculada.
        write lc_inteiro to lc_peso_old.
        condense lc_peso_old no-gaps.

        lc_inteiro = wa_take_up_manut-qt_vinculada.
        write lc_inteiro to lc_peso.
        condense lc_peso no-gaps.

        e_obs_alteracao =
        e_obs_alteracao && ' Alterado Take UP ' &&
        wa_take_up_manut-id_takeup && ' Bloco ' && wa_take_up_manut-nu_bloco &&
        'Quantidade de ' && lc_peso_old && ' para ' && lc_peso.
        ck_alterou_takeup = abap_true.

      endif.

    endloop.

    loop at e_carga_original->take_up into wa_take_up_original.

      read table me->zif_carga~take_up
        into wa_take_up_manut
        with key id_carga  = wa_take_up_original-id_carga
                 id_nota   = wa_take_up_original-id_nota
                 id_takeup = wa_take_up_original-id_takeup
                 nu_bloco  = wa_take_up_original-nu_bloco.

      if sy-subrc is not initial.
        lc_inteiro = wa_take_up_original-qt_vinculada.
        write lc_inteiro to lc_peso.
        condense lc_peso no-gaps.

        e_obs_alteracao =
        e_obs_alteracao && ' Removido Take UP ' &&
        wa_take_up_original-id_takeup && ' Bloco ' && wa_take_up_original-nu_bloco &&
        'Quantidade de ' && lc_peso.
        ck_alterou_takeup = abap_true.
      endif.

    endloop.

    if ck_alterado_imposto eq abap_true.
      e_ck_aceite_filial = abap_true.
    endif.

    "Alterar Peso Bruto
    if me->carga-nm_peso_bruto ne e_carga_original->carga-nm_peso_bruto.
      e_alteracoes-al_nm_peso_bruto = abap_true.
      e_ck_aceite_filial = abap_true.
      e_obs_alteracao = e_obs_alteracao && ' Alterado o Peso Bruto'.
    endif.

    "Alterar Peso tara
    if me->carga-nm_peso_tara ne e_carga_original->carga-nm_peso_tara.
      e_alteracoes-al_nm_peso_tara = abap_true.
      e_ck_aceite_filial = abap_true.
      e_obs_alteracao = e_obs_alteracao && ' Alterado o Peso Tara'.
    endif.

    "Alterou SubTotal
    if me->carga-nm_peso_subtotal ne e_carga_original->carga-nm_peso_subtotal.
      e_alteracoes-al_nm_peso_subtotal = abap_true.
      e_ck_aceite_filial = abap_true.
      e_obs_alteracao = e_obs_alteracao && ' Alterado o Peso SubTotal'.
    endif.

    "Altera Algum Peso de Desconto
    if me->carga-nm_peso_descontos ne e_carga_original->carga-nm_peso_descontos.
      e_alteracoes-al_nm_peso_descontos = abap_true.
      e_ck_aceite_filial = abap_true.
      e_obs_alteracao = e_obs_alteracao && ' Alterado o Peso de Desconto'.
      if lc_alterou_mes eq abap_true.
        e_ck_aceite_comercial = abap_true.
      endif.
    endif.

    "Alteração de Trangeníase
    if me->classificacao-in_gmo               ne e_carga_original->classificacao-in_gmo or
       me->classificacao-nr_resultado_01      ne e_carga_original->classificacao-nr_resultado_01 or
       me->classificacao-nr_resultado_02      ne e_carga_original->classificacao-nr_resultado_02 or
       me->classificacao-nr_res_rr1_rr2       ne e_carga_original->classificacao-nr_res_rr1_rr2 or
       me->classificacao-in_gmo_03            ne e_carga_original->classificacao-in_gmo_03 or
       me->classificacao-in_srr_origem_partic ne e_carga_original->classificacao-in_srr_origem_partic or
       me->classificacao-id_outro_partic      ne e_carga_original->classificacao-id_outro_partic or
       me->classificacao-in_srr_declarado     ne e_carga_original->classificacao-in_srr_declarado or
       me->classificacao-in_teste_srr         ne e_carga_original->classificacao-in_teste_srr or
       me->classificacao-in_srr_declarado_2   ne e_carga_original->classificacao-in_srr_declarado_2 or
       me->classificacao-in_teste_srr_2       ne e_carga_original->classificacao-in_teste_srr_2.

      e_obs_alteracao = e_obs_alteracao && ' Alterado a Transgeníase'.

      e_ck_aceite_filial = abap_true.
      if lc_alterou_mes eq abap_true.
        e_ck_aceite_comercial = abap_true.
      endif.

      if me->classificacao-in_gmo ne e_carga_original->classificacao-in_gmo.
        e_alteracoes-al_in_gmo = abap_true.
      endif.
      if me->classificacao-nr_resultado_01 ne e_carga_original->classificacao-nr_resultado_01.
        e_alteracoes-al_nr_resultado_01 = abap_true.
      endif.
      if me->classificacao-nr_resultado_02 ne e_carga_original->classificacao-nr_resultado_02.
        e_alteracoes-al_nr_resultado_02 = abap_true.
      endif.

      if me->classificacao-nr_res_rr1_rr2 ne e_carga_original->classificacao-nr_res_rr1_rr2.
        e_alteracoes-al_nr_res_rr1_rr2 = abap_true.
      endif.

      if me->classificacao-in_gmo_03 ne e_carga_original->classificacao-in_gmo_03.
        e_alteracoes-al_in_gmo_03 = abap_true.
      endif.

      if me->classificacao-in_srr_origem_partic ne e_carga_original->classificacao-in_srr_origem_partic.
        e_alteracoes-al_in_srr_origem_partic = abap_true.
      endif.

      if me->classificacao-id_outro_partic ne e_carga_original->classificacao-id_outro_partic.
        e_alteracoes-al_id_outro_partic = abap_true.
      endif.

      if me->classificacao-in_srr_declarado ne e_carga_original->classificacao-in_srr_declarado.
        e_alteracoes-al_in_srr_declarado = abap_true.
      endif.

      if me->classificacao-in_teste_srr ne e_carga_original->classificacao-in_teste_srr.
        e_alteracoes-al_in_teste_srr = abap_true.
      endif.

      if me->classificacao-in_srr_declarado_2 ne e_carga_original->classificacao-in_srr_declarado_2.
        e_alteracoes-al_in_srr_declarado_2 = abap_true.
      endif.

      if me->classificacao-in_teste_srr_2 ne e_carga_original->classificacao-in_teste_srr_2.
        e_alteracoes-al_in_teste_srr_2 = abap_true.
      endif.

    endif.

    if e_ck_aceite_comercial eq abap_false and
       e_ck_aceite_filial    eq abap_false and
       e_ck_aceite_fiscal    eq abap_false and
       ck_alterou_takeup     eq abap_false.
      raise exception type zcx_carga
        exporting
          textid = value #( msgid  = zcx_carga=>zcx_nao_altera_original-msgid
                            msgno  = zcx_carga=>zcx_nao_altera_original-msgno )
          msgty  = 'E'
          msgid  = zcx_carga=>zcx_nao_altera_original-msgid
          msgno  = zcx_carga=>zcx_nao_altera_original-msgno.
    endif.

    e_ck_aceite_filial = abap_true.

    "Verificar se Alterar TakeUp é para enviar para comercial
    "CK_ALTEROU_TAKEUP

    if e_ck_aceite_fiscal eq abap_true and lc_alterou_mes eq abap_true.
      concatenate 'Movimento terá alteração de período de' e_carga_original->carga-dt_movimento(6) 'para' sy-datum(6) into e_obs_alteracao separated by space.
    endif.

    condense e_obs_alteracao.

  endmethod.


  METHOD ZIF_CARGA~VERIF_BLOQ_LOTE_MATERIAL_WAIT.

    DATA: I_GNAME TYPE SEQG3-GNAME,
          I_GARG  TYPE SEQG3-GARG,
          IT_ENQ  TYPE TABLE OF SEQG3.

    DATA: I_TEXTO       TYPE STRING,
          I_TEMPO       TYPE I,
          LC_TEXTO      TYPE STRING,
          LC_NUMERO     TYPE CHAR30,
          I_TEXT_WAIT   TYPE STRING,
          LC_ID_PRODUTO TYPE MATNR.

    R_CARGA = ME.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = ME->CARGA-ID_PRODUTO
      IMPORTING
        OUTPUT = LC_ID_PRODUTO.

    I_GNAME = 'MCH1'.
    CONCATENATE SY-MANDT ME->CARGA-ID_PRODUTO ME->CARGA-NR_SAFRA INTO I_GARG.

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
      IF SY-BATCH NE ABAP_TRUE.
        CONCATENATE 'Aguarde, Lote:' ME->CARGA-NR_SAFRA 'Material:' LC_ID_PRODUTO 'bloqueados!' WA_ENQ-GUNAME WA_ENQ-GTCODE INTO I_TEXT_WAIT SEPARATED BY SPACE.

        LC_NUMERO = 0.
        LC_TEXTO  = I_TEXT_WAIT && ' Segundos: ' && LC_NUMERO.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            PERCENTAGE = 0
            TEXT       = LC_TEXTO.
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

      IF SY-BATCH NE ABAP_TRUE.
        READ TABLE IT_ENQ INTO WA_ENQ INDEX 1.
        CONCATENATE 'Aguarde, Lote:' ME->CARGA-NR_SAFRA 'Material:' LC_ID_PRODUTO 'bloqueados!' WA_ENQ-GUNAME WA_ENQ-GTCODE INTO I_TEXT_WAIT SEPARATED BY SPACE.
        LC_TEXTO = I_TEXT_WAIT && ' Segundos: ' && LC_NUMERO.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            PERCENTAGE = 0
            TEXT       = LC_TEXTO.
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


  METHOD ZIF_CARGA~VERIF_CK_PLUMA_NOTA_FISCAL.

    DATA: LC_QUANTIDADE TYPE LFIMG.
    DATA: LC_FARDOS     TYPE ZDE_NM_FARDOS.
    DATA: LV_TEXTO TYPE C LENGTH 17.
    DATA: PS_VALOR TYPE P LENGTH 16.

    R_CARGA = ME.

    SELECT SINGLE * INTO @DATA(WA_MARA)
      FROM MARA
     WHERE MATNR EQ @ME->CARGA-ID_PRODUTO.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_MARA-MATKL
      IMPORTING
        OUTPUT = WA_MARA-MATKL.

    CHECK WA_MARA-MATKL EQ ZIF_CARGA=>ST_GRUPO_ALGODAO_PLUMA. "Algodão
    CHECK ME->CARGA-TP_STATUS NE ZIF_CARGA=>ST_STATUS_CANCELADA.

    LOOP AT ME->ZIF_CARGA~DOCUMENTO_FISCAL INTO DATA(WA_NOTA_FISCAL).

      LC_QUANTIDADE = REDUCE NTGEW_15( INIT I TYPE NTGEW_15 FOR LS IN ME->ZIF_CARGA~BLOCOS NEXT I = I + LS-PS_FARDOS_LIQUI ).
      LC_FARDOS     = REDUCE ZDE_NM_FARDOS( INIT B TYPE ZDE_NM_FARDOS FOR LS IN ME->ZIF_CARGA~BLOCOS NEXT B = B + LS-QT_FARDOS ).

      IF LC_QUANTIDADE IS INITIAL.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NOTA_WITHOUT_TAKE_BLOCO-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_NOTA_WITHOUT_TAKE_BLOCO-MSGNO
                              ATTR1 = CONV #( WA_NOTA_FISCAL-NR_NOTA ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_NOTA_WITHOUT_TAKE_BLOCO-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_NOTA_WITHOUT_TAKE_BLOCO-MSGID
            MSGV1  = CONV #( WA_NOTA_FISCAL-NR_NOTA ).
      ENDIF.

      IF LC_FARDOS IS INITIAL.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NOTA_WITHOUT_FARDO-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_NOTA_WITHOUT_FARDO-MSGNO
                              ATTR1 = CONV #( WA_NOTA_FISCAL-NR_NOTA ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_NOTA_WITHOUT_FARDO-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_NOTA_WITHOUT_FARDO-MSGID
            MSGV1  = CONV #( WA_NOTA_FISCAL-NR_NOTA ).
      ENDIF.

      WRITE WA_NOTA_FISCAL-NR_QUANTIDADE TO LV_TEXTO.
      CONDENSE LV_TEXTO NO-GAPS.

      IF LC_QUANTIDADE GT WA_NOTA_FISCAL-NR_QUANTIDADE.

        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_ACIMA-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_ACIMA-MSGNO
                              ATTR1 = CONV #( WA_NOTA_FISCAL-NR_NOTA )
                              ATTR2 = LV_TEXTO )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_ACIMA-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_ACIMA-MSGID
            MSGV1  = CONV #( WA_NOTA_FISCAL-NR_NOTA )
            MSGV2  = CONV #( LV_TEXTO ).

      ELSEIF LC_QUANTIDADE NE WA_NOTA_FISCAL-NR_QUANTIDADE.

        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_DIFER-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_DIFER-MSGNO
                              ATTR1 = CONV #( WA_NOTA_FISCAL-NR_NOTA )
                              ATTR2 = LV_TEXTO )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_DIFER-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_DIFER-MSGID
            MSGV1  = CONV #( WA_NOTA_FISCAL-NR_NOTA )
            MSGV2  = CONV #( LV_TEXTO ).

      ENDIF.

      WRITE WA_NOTA_FISCAL-NR_FARDO TO LV_TEXTO.
      CONDENSE LV_TEXTO NO-GAPS.

      IF LC_FARDOS GT WA_NOTA_FISCAL-NR_FARDO.

        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NOTA_FARDOS_ACIMA-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_NOTA_FARDOS_ACIMA-MSGNO
                              ATTR1 = CONV #( WA_NOTA_FISCAL-NR_NOTA )
                              ATTR2 = LV_TEXTO )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_NOTA_FARDOS_ACIMA-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_NOTA_FARDOS_ACIMA-MSGID
            MSGV1  = CONV #( WA_NOTA_FISCAL-NR_NOTA )
            MSGV2  = CONV #( LV_TEXTO ).

      ELSEIF LC_FARDOS NE WA_NOTA_FISCAL-NR_FARDO.

        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NOTA_FARDOS_DIFER-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_NOTA_FARDOS_DIFER-MSGNO
                              ATTR1 = CONV #( WA_NOTA_FISCAL-NR_NOTA )
                              ATTR2 = LV_TEXTO )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_NOTA_FARDOS_DIFER-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_NOTA_FARDOS_DIFER-MSGID
            MSGV1  = CONV #( WA_NOTA_FISCAL-NR_NOTA )
            MSGV2  = CONV #( LV_TEXTO ).

      ENDIF.

      LC_QUANTIDADE = 0.
      LC_FARDOS = 0.

      LC_QUANTIDADE = REDUCE NTGEW_15( INIT I TYPE NTGEW_15 FOR OV IN ME->ZIF_CARGA~ORDEM_VENDA NEXT I = I + OV-NM_PESO_LIQUIDO ).
      LC_FARDOS = REDUCE ZDE_NM_FARDOS( INIT B TYPE ZDE_NM_FARDOS FOR OF IN ME->ZIF_CARGA~ORDEM_VENDA NEXT B = B + OF-QT_FARDOS ).

      WRITE WA_NOTA_FISCAL-NR_QUANTIDADE TO LV_TEXTO.
      CONDENSE LV_TEXTO NO-GAPS.

      IF LC_QUANTIDADE GT WA_NOTA_FISCAL-NR_QUANTIDADE.

        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_ACIMA-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_ACIMA-MSGNO
                              ATTR1 = CONV #( WA_NOTA_FISCAL-NR_NOTA )
                              ATTR2 = LV_TEXTO )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_ACIMA-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_ACIMA-MSGID
            MSGV1  = CONV #( WA_NOTA_FISCAL-NR_NOTA )
            MSGV2  = CONV #( LV_TEXTO ).

      ELSEIF LC_QUANTIDADE NE WA_NOTA_FISCAL-NR_QUANTIDADE.

        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_DIFER-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_DIFER-MSGNO
                              ATTR1 = CONV #( WA_NOTA_FISCAL-NR_NOTA )
                              ATTR2 = LV_TEXTO )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_DIFER-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_NOTA_TAKE_BLOCO_DIFER-MSGID
            MSGV1  = CONV #( WA_NOTA_FISCAL-NR_NOTA )
            MSGV2  = CONV #( LV_TEXTO ).

      ENDIF.

      WRITE WA_NOTA_FISCAL-NR_FARDO TO LV_TEXTO.
      CONDENSE LV_TEXTO NO-GAPS.

      IF LC_FARDOS GT WA_NOTA_FISCAL-NR_FARDO.

        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NOTA_FARDOS_ACIMA-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_NOTA_FARDOS_ACIMA-MSGNO
                              ATTR1 = CONV #( WA_NOTA_FISCAL-NR_NOTA )
                              ATTR2 = LV_TEXTO )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_NOTA_FARDOS_ACIMA-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_NOTA_FARDOS_ACIMA-MSGID
            MSGV1  = CONV #( WA_NOTA_FISCAL-NR_NOTA )
            MSGV2  = CONV #( LV_TEXTO ).

      ELSEIF LC_FARDOS NE WA_NOTA_FISCAL-NR_FARDO.

        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_NOTA_FARDOS_DIFER-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_NOTA_FARDOS_DIFER-MSGNO
                              ATTR1 = CONV #( WA_NOTA_FISCAL-NR_NOTA )
                              ATTR2 = LV_TEXTO )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_NOTA_FARDOS_DIFER-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_NOTA_FARDOS_DIFER-MSGID
            MSGV1  = CONV #( WA_NOTA_FISCAL-NR_NOTA )
            MSGV2  = CONV #( LV_TEXTO ).

      ENDIF.

    ENDLOOP.

    "Verificar Saldo Bloco """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Verificar Saldo Bloco """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(LC_BLOCOS) = ME->ZIF_CARGA~BLOCOS[].
    SORT LC_BLOCOS BY ZSEQ_INST OBJEK OBJECTTABLE.
    DELETE ADJACENT DUPLICATES FROM LC_BLOCOS COMPARING ZSEQ_INST OBJEK OBJECTTABLE.

    LOOP AT LC_BLOCOS INTO DATA(WA_BLOCOS).

      WA_BLOCOS-QT_FARDOS       = 0.
      WA_BLOCOS-PS_FARDOS_LIQUI = 0.
      WA_BLOCOS-PS_FARDOS_BRUTO = 0.

      LOOP AT ME->ZIF_CARGA~BLOCOS INTO DATA(WA_BLOCO_CARGA)
        WHERE ZSEQ_INST EQ WA_BLOCOS-ZSEQ_INST
          AND OBJEK EQ WA_BLOCOS-OBJEK
          AND OBJECTTABLE EQ WA_BLOCOS-OBJECTTABLE.

        ADD WA_BLOCO_CARGA-QT_FARDOS       TO WA_BLOCOS-QT_FARDOS.
        ADD WA_BLOCO_CARGA-PS_FARDOS_LIQUI TO WA_BLOCOS-PS_FARDOS_LIQUI.
        ADD WA_BLOCO_CARGA-PS_FARDOS_BRUTO TO WA_BLOCOS-PS_FARDOS_BRUTO.
      ENDLOOP.

      "Busca Utilização de Fardos
      SELECT FD~QT_FARDOS, FD~PS_FARDOS_LIQUI, FD~PS_FARDOS_BRUTO
        INTO TABLE @DATA(IT_ZSDT0001FD)
        FROM ZSDT0001FD AS FD
        INNER JOIN ZSDT0001CG AS CG ON CG~ID_CARGA EQ FD~ID_CARGA
       WHERE FD~ZSEQ_INST    EQ @WA_BLOCOS-ZSEQ_INST
         AND FD~OBJEK        EQ @WA_BLOCOS-OBJEK
         AND FD~OBJECTTABLE  EQ @WA_BLOCOS-OBJECTTABLE
         AND FD~ID_CARGA     NE @ME->CARGA-ID_CARGA
         AND CG~TP_STATUS    NE @ZIF_CARGA=>ST_STATUS_CANCELADA.

      LOOP AT IT_ZSDT0001FD INTO DATA(WA_UTILIZADO).
        WA_BLOCOS-QT_FARDOS       = WA_BLOCOS-QT_FARDOS       + WA_UTILIZADO-QT_FARDOS.
        WA_BLOCOS-PS_FARDOS_LIQUI = WA_BLOCOS-PS_FARDOS_LIQUI + WA_UTILIZADO-PS_FARDOS_LIQUI.
        WA_BLOCOS-PS_FARDOS_BRUTO = WA_BLOCOS-PS_FARDOS_BRUTO + WA_UTILIZADO-PS_FARDOS_BRUTO.
      ENDLOOP.

      "Cadastro do Bloco/Fardos
      SELECT SINGLE * INTO @DATA(WA_ZSDT0045)
        FROM ZSDT0045
       WHERE ZSEQ_INST    EQ @WA_BLOCOS-ZSEQ_INST
         AND OBJEK        EQ @WA_BLOCOS-OBJEK
         AND OBJECTTABLE  EQ @WA_BLOCOS-OBJECTTABLE.

      IF SY-SUBRC IS NOT INITIAL.
        "Bloco não encontrado
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_BLOCO_NAO_ENCONTRADO-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_BLOCO_NAO_ENCONTRADO-MSGNO )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_BLOCO_NAO_ENCONTRADO-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_BLOCO_NAO_ENCONTRADO-MSGID.
      ENDIF.

      IF WA_BLOCOS-QT_FARDOS GT WA_ZSDT0045-QUANTIDADE.

        "Saldo Fardos do Bloco
        PS_VALOR = WA_ZSDT0045-QUANTIDADE.
        WRITE PS_VALOR TO SY-MSGV1.
        CONDENSE SY-MSGV1 NO-GAPS.

        "Fardos a Utilizar
        WRITE WA_BLOCOS-QT_FARDOS TO SY-MSGV2.
        CONDENSE SY-MSGV2 NO-GAPS.

        "Quantidade de Blocos passou da quantidade negociada
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_BLOCO_SALDO_FARDO-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_BLOCO_SALDO_FARDO-MSGNO
                              ATTR1 = SY-MSGV1
                              ATTR2 = SY-MSGV2 )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_BLOCO_SALDO_FARDO-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_BLOCO_SALDO_FARDO-MSGID
            MSGV1  = SY-MSGV1
            MSGV2  = SY-MSGV2.

      ENDIF.

      IF WA_BLOCOS-PS_FARDOS_LIQUI GT WA_ZSDT0045-BTGEW.

        "Saldo Peso do Bloco
        PS_VALOR = WA_ZSDT0045-BTGEW.
        WRITE PS_VALOR TO SY-MSGV1.
        CONDENSE SY-MSGV1 NO-GAPS.

        "Peso a Utilizar
        PS_VALOR = WA_BLOCOS-PS_FARDOS_LIQUI.
        WRITE PS_VALOR TO SY-MSGV2.
        CONDENSE SY-MSGV2 NO-GAPS.

*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_BLOCO_SALDO_PESO-MSGID
*                              MSGNO = ZCX_CARGA=>ZCX_BLOCO_SALDO_PESO-MSGNO
*                              ATTR1 = SY-MSGV1
*                              ATTR2 = SY-MSGV2 )
*            MSGTY  = 'W'
*            MSGNO  = ZCX_CARGA=>ZCX_BLOCO_SALDO_PESO-MSGNO
*            MSGID  = ZCX_CARGA=>ZCX_BLOCO_SALDO_PESO-MSGID
*            MSGV1  = SY-MSGV1
*            MSGV2  = SY-MSGV2.

      ENDIF.

    ENDLOOP.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  ENDMETHOD.


  METHOD ZIF_CARGA~VERIF_CK_SALDO_TAKEUP_BLOCO.

    R_CARGA = ME.

*    TYPES BEGIN OF TY_TOTAL.
*    TYPES: ID_TAKEUP TYPE ZDE_ID_TAKEUP.
*    TYPES: NU_BLOCO TYPE ZDE_NM_BLOCO_ALGODAO.
*    TYPES: QT_VINCULADA TYPE ZDE_QTD_VINC_ALGODAO.
*    TYPES END OF TY_TOTAL.
*
*    DATA: IT_TOTAL TYPE TABLE OF TY_TOTAL.
*
*    SELECT SINGLE * INTO @DATA(WA_MARA)
*      FROM MARA
*     WHERE MATNR EQ @ME->CARGA-ID_PRODUTO.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        INPUT  = WA_MARA-MATKL
*      IMPORTING
*        OUTPUT = WA_MARA-MATKL.
*
*    CHECK WA_MARA-MATKL EQ ZIF_CARGA=>ST_GRUPO_ALGODAO_PLUMA. "Algodão
*
*    LOOP AT ME->ZIF_CARGA~TAKE_UP INTO DATA(WA_TAKE_UP).
*      READ TABLE IT_TOTAL ASSIGNING FIELD-SYMBOL(<FS_TAKEUP>)
*      WITH KEY ID_TAKEUP = WA_TAKE_UP-ID_TAKEUP
*               NU_BLOCO  = WA_TAKE_UP-NU_BLOCO.
*      IF SY-SUBRC IS INITIAL.
*        ADD WA_TAKE_UP-QT_VINCULADA TO <FS_TAKEUP>-QT_VINCULADA.
*      ELSE.
*        APPEND VALUE #(
*                 ID_TAKEUP = WA_TAKE_UP-ID_TAKEUP
*                 NU_BLOCO = WA_TAKE_UP-NU_BLOCO
*                 QT_VINCULADA = WA_TAKE_UP-QT_VINCULADA
*        ) TO IT_TOTAL.
*      ENDIF.
*    ENDLOOP.
*
*    IF ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_TRUE. "Etapa 1
*      "Momento Solicitando uma Manutenção
*      "SALDO_FINAL = QTD_OLD - QTD_NEW + SALDO_NOW
*      "SALDO_FINAL <= SALDO_NOW OK!
*
*
*    ELSEIF ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_FALSE AND ME->ZIF_CARGA~CK_EXECUTAR_MANUTENCAO_ENTRADA = ABAP_FALSE. "Etapa 2
*      "Primeiro Lançamento
*      "SALDO_FINAL = SALDO_NOW - QTD_NEW
*      "SALDO_FINAL >= 0 SALDO OK
*
*    ELSEIF ME->ZIF_CARGA~AT_MANUTENCAO EQ ABAP_FALSE AND ME->ZIF_CARGA~CK_EXECUTAR_MANUTENCAO_ENTRADA = ABAP_TRUE.
*      "Executando Manutenção (Validou o Saldo na Etapa 1) não precisar fazer novamente
*
*    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~VERIF_ESTORNO_PENDENTE.

    DATA: RG_ST_ESTORNO  TYPE RANGE OF ZDE_STATUS_ESTORNO.

    R_CARGA = ME.

    RG_ST_ESTORNO = VALUE #( SIGN = 'I' OPTION = 'EQ'
                             ( LOW = ZIF_CARGA=>ST_STATUS_ESTORNO_SOLICITADO  HIGH = ZIF_CARGA=>ST_STATUS_ESTORNO_SOLICITADO )
                             ( LOW = ZIF_CARGA=>ST_STATUS_ESTORNO_ERRO        HIGH = ZIF_CARGA=>ST_STATUS_ESTORNO_ERRO ) ).

    SELECT * INTO TABLE @DATA(IT_ENTRADA)
      FROM ZMMT_EE_ZGR
     WHERE ID_CARGA   EQ @ME->CARGA-ID_CARGA
       AND ST_ESTORNO IN @RG_ST_ESTORNO.

    IF IT_ENTRADA[] IS NOT INITIAL.
      LOOP AT IT_ENTRADA INTO DATA(WA_ENTRADA_ESTORNO).
        CASE WA_ENTRADA_ESTORNO-ST_ESTORNO.
          WHEN ZIF_CARGA=>ST_STATUS_ESTORNO_SOLICITADO.
            RAISE EXCEPTION TYPE ZCX_CARGA
              EXPORTING
                TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_EXISTE_ESTORNO_PENDENTE-MSGID
                                  MSGNO = ZCX_CARGA=>ZCX_EXISTE_ESTORNO_PENDENTE-MSGNO )
                MSGTY  = 'E'
                MSGID  = ZCX_CARGA=>ZCX_EXISTE_ESTORNO_PENDENTE-MSGID
                MSGNO  = ZCX_CARGA=>ZCX_EXISTE_ESTORNO_PENDENTE-MSGNO.
          WHEN ZIF_CARGA=>ST_STATUS_ESTORNO_ERRO.
            RAISE EXCEPTION TYPE ZCX_CARGA
              EXPORTING
                TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_EXISTE_ESTORNO_ERRO-MSGID
                                  MSGNO = ZCX_CARGA=>ZCX_EXISTE_ESTORNO_ERRO-MSGNO )
                MSGTY  = 'E'
                MSGID  = ZCX_CARGA=>ZCX_EXISTE_ESTORNO_ERRO-MSGID
                MSGNO  = ZCX_CARGA=>ZCX_EXISTE_ESTORNO_ERRO-MSGNO.
        ENDCASE.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD zif_carga~verif_ordem_carregamento.

    DATA: tb_ordens TYPE TABLE OF zde_id_ordem.

    r_carga = me.

    LOOP AT me->zif_carga~ordem_venda INTO DATA(wa_ordem) WHERE id_ordem IS NOT INITIAL .
      APPEND wa_ordem-id_ordem TO tb_ordens.
    ENDLOOP.

    IF i_ordem IS INITIAL AND me->carga-id_ordem IS NOT INITIAL.
      APPEND me->carga-id_ordem TO tb_ordens.
    ELSEIF i_ordem IS NOT INITIAL.
      APPEND i_ordem-id_ordem TO tb_ordens.
    ENDIF.

    LOOP AT tb_ordens INTO DATA(wa_ordem_verif).

      DATA(lc_ordem) = zcl_ordem_carregamento=>busca_ordem_carregamento( i_id_ordem  = wa_ordem_verif ).

      IF lc_ordem-nr_safra NE me->carga-nr_safra.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_ordem_carga_safra-msgid msgno = zcx_carga=>zcx_ordem_carga_safra-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_ordem_carga_safra-msgid
            msgno  = zcx_carga=>zcx_ordem_carga_safra-msgno.
      ENDIF.

      IF lc_ordem-id_bukrs NE me->carga-id_bukrs.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_ordem_carga_empresa-msgid msgno = zcx_carga=>zcx_ordem_carga_empresa-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_ordem_carga_empresa-msgid
            msgno  = zcx_carga=>zcx_ordem_carga_empresa-msgno.
      ENDIF.

      IF lc_ordem-id_branch NE me->carga-id_branch.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_ordem_carga_filial-msgid msgno = zcx_carga=>zcx_ordem_carga_filial-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_ordem_carga_filial-msgid
            msgno  = zcx_carga=>zcx_ordem_carga_filial-msgno.
      ENDIF.

      IF lc_ordem-dt_validade LT sy-datlo AND lc_ordem-dt_validade LT sy-datum
          AND me->zif_carga~at_manutencao EQ abap_false
          AND me->zif_carga~ck_executar_manutencao_entrada EQ abap_false.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_ordem_vencida-msgid msgno = zcx_carga=>zcx_ordem_vencida-msgno attr1 = CONV #( lc_ordem-dt_validade ) )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_ordem_vencida-msgid
            msgno  = zcx_carga=>zcx_ordem_vencida-msgno
            msgv1  = CONV #( lc_ordem-dt_validade ).
      ENDIF.

      CHECK i_ck_verificar_carga EQ abap_true.

      DATA: lc_id_agent_frete TYPE zde_id_agent_frete.

      lc_id_agent_frete = lc_ordem-id_branch_ag.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lc_id_agent_frete
        IMPORTING
          output = lc_id_agent_frete.

      IF lc_id_agent_frete            NE me->carga-id_agent_frete    AND
         me->carga-id_agent_frete     NE me->carga-id_proprietario   AND
         me->zif_carga~at_manutencao  EQ abap_false.
*-CS2021000253-26.04.2024-#59941-JT-inicio
        IF lc_ordem-id_agent_frete_terceiro IS NOT INITIAL           AND
           lc_ordem-id_agent_frete_terceiro NE me->carga-id_agent_frete.
*-CS2021000253-26.04.2024-#59941-JT-fim
          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = VALUE #( msgid = zcx_carga=>zcx_ordem_empresaag-msgid msgno = zcx_carga=>zcx_ordem_empresaag-msgno )
              msgty  = 'E'
              msgid  = zcx_carga=>zcx_ordem_empresaag-msgid
              msgno  = zcx_carga=>zcx_ordem_empresaag-msgno.
        ENDIF.
      ENDIF.

      IF lc_ordem-id_produto NE me->carga-id_produto.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_ordem_produto-msgid msgno = zcx_carga=>zcx_ordem_produto-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_ordem_produto-msgid
            msgno  = zcx_carga=>zcx_ordem_produto-msgno.
      ENDIF.

      IF lc_ordem-ds_placa_trator NE me->carga-ds_placa_trator AND me->zif_carga~at_manutencao EQ abap_false AND me->carga-ds_placa_trator IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_placa_trator-msgid msgno = zcx_carga=>zcx_placa_trator-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_placa_trator-msgid
            msgno  = zcx_carga=>zcx_placa_trator-msgno.
      ENDIF.

      IF lc_ordem-ds_placa_reboq_1 NE me->carga-ds_placa_reboq_1 AND me->zif_carga~at_manutencao EQ abap_false AND me->carga-ds_placa_reboq_1 IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_placa_reboque1-msgid msgno = zcx_carga=>zcx_placa_reboque1-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_placa_reboque1-msgid
            msgno  = zcx_carga=>zcx_placa_reboque1-msgno.
      ENDIF.

      IF lc_ordem-ds_placa_reboq_2 NE me->carga-ds_placa_reboq_2 AND me->zif_carga~at_manutencao EQ abap_false AND me->carga-ds_placa_reboq_2 IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_placa_reboque2-msgid msgno = zcx_carga=>zcx_placa_reboque2-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_placa_reboque2-msgid
            msgno  = zcx_carga=>zcx_placa_reboque2-msgno.
      ENDIF.

      IF lc_ordem-ds_placa_reboq_3 NE me->carga-ds_placa_reboq_3 AND me->zif_carga~at_manutencao EQ abap_false AND me->carga-ds_placa_reboq_3 IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_placa_reboque3-msgid msgno = zcx_carga=>zcx_placa_reboque3-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_placa_reboque3-msgid
            msgno  = zcx_carga=>zcx_placa_reboque3-msgno.
      ENDIF.

      IF lc_ordem-id_motorista NE me->carga-id_motorista AND me->zif_carga~at_manutencao EQ abap_false AND me->carga-id_motorista IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_ordem_motorista-msgid msgno = zcx_carga=>zcx_ordem_motorista-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_ordem_motorista-msgid
            msgno  = zcx_carga=>zcx_ordem_motorista-msgno.
      ENDIF.

      IF lc_ordem-id_proprietario NE me->carga-id_proprietario AND me->zif_carga~at_manutencao EQ abap_false AND me->carga-id_proprietario IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_ordem_proprietario-msgid msgno = zcx_carga=>zcx_ordem_proprietario-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_ordem_proprietario-msgid
            msgno  = zcx_carga=>zcx_ordem_proprietario-msgno.
      ENDIF.

      "Fornecedor
      IF lc_ordem-id_local_destino NE me->carga-id_local_destino.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_ordem_destino-msgid msgno = zcx_carga=>zcx_ordem_destino-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_ordem_destino-msgid
            msgno  = zcx_carga=>zcx_ordem_destino-msgno.
      ENDIF.

      "Destino
      IF lc_ordem-id_local_descarga NE me->carga-id_local_descarga.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_ordem_descarga-msgid msgno = zcx_carga=>zcx_ordem_descarga-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_ordem_descarga-msgid
            msgno  = zcx_carga=>zcx_ordem_descarga-msgno.
      ENDIF.

      "Ponto de Coleta
      IF lc_ordem-id_local_coleta NE me->carga-id_local_coleta.
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_ordem_coleta-msgid msgno = zcx_carga=>zcx_ordem_coleta-msgno )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_ordem_coleta-msgid
            msgno  = zcx_carga=>zcx_ordem_coleta-msgno.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_CARGA~VERIF_PESO_NOTAS.

    DATA: RG_ID_CARGA  TYPE RANGE OF ZDE_ID_CARGA,
          TX_QTD_CARGA TYPE C LENGTH 15,
          TX_QTD_NOTA  TYPE C LENGTH 15.

    DATA: LC_PESO_NOTAS TYPE ZSDT0001NT-NR_QUANTIDADE.

    LC_PESO_NOTAS = 0.
    LOOP AT ME->ZIF_CARGA~DOCUMENTO_FISCAL INTO DATA(WA_NOTA).
      ADD WA_NOTA-NR_QUANTIDADE TO LC_PESO_NOTAS.
    ENDLOOP.

    IF ME->CARGA-NM_PESO_SUBTOTAL NE LC_PESO_NOTAS.
      WRITE ME->CARGA-NM_PESO_SUBTOTAL TO TX_QTD_CARGA.
      WRITE LC_PESO_NOTAS TO TX_QTD_NOTA.
      CONDENSE TX_QTD_CARGA NO-GAPS.
      CONDENSE TX_QTD_NOTA NO-GAPS.
      "218 Ticket Pesagem &1 já Utilizado para o Produtor &2 e Safra &3!
      MESSAGE W227 WITH TX_QTD_CARGA TX_QTD_NOTA.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~VERIF_SALDO_ORDEM_VENDA.

    DATA: LC_SALDO_OV  TYPE KWMENG,
          TX_QTD_SALDO TYPE C LENGTH 20,
          TX_QTD_LIQUI TYPE C LENGTH 20.

    CHECK ME->ZIF_CARGA~ORDEM_VENDA[] IS NOT INITIAL.

    LC_SALDO_OV = 0.

    LOOP AT ME->ZIF_CARGA~ORDEM_VENDA INTO DATA(WA_ORDEM).
      TRY .
          ZCL_ORDEM_VENDA=>ZIF_ORDEM_VENDA~GET_ORDEM_VENDA_SALDO(
            EXPORTING
              I_VBELN = WA_ORDEM-NR_ORDEM_VENDA
            IMPORTING
              E_SALDO = DATA(E_SALDO) ).

          ADD E_SALDO TO LC_SALDO_OV.
        CATCH ZCX_ORDEM_VENDA .
      ENDTRY.
    ENDLOOP.

    IF LC_SALDO_OV LT ME->CARGA-NM_PESO_LIQUIDO.
      WRITE LC_SALDO_OV TO TX_QTD_SALDO.
      WRITE ME->CARGA-NM_PESO_LIQUIDO TO TX_QTD_LIQUI.
      CONDENSE TX_QTD_SALDO NO-GAPS.
      CONDENSE TX_QTD_LIQUI NO-GAPS.
      MESSAGE W228 WITH TX_QTD_LIQUI TX_QTD_SALDO.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CARGA~VERIF_TICKET_PESAGEM.

    DATA: RG_ID_CARGA  TYPE RANGE OF ZDE_ID_CARGA.

    IF ME->CARGA-ID_CARGA IS NOT INITIAL.
      RG_ID_CARGA = VALUE #( SIGN = 'I' OPTION = 'NE' ( HIGH = ME->CARGA-ID_CARGA LOW = ME->CARGA-ID_CARGA ) ).
    ENDIF.

    LOOP AT ME->ZIF_CARGA~DOCUMENTO_FISCAL INTO DATA(WA_NFISCAL).

      SELECT SINGLE * INTO @DATA(WA_TICKET)
        FROM ZSDT0001CG AS D
       WHERE D~ID_CARGA  IN @RG_ID_CARGA
         AND D~NR_SAFRA  EQ @ME->CARGA-NR_SAFRA
         AND D~NR_TICKET EQ @ME->CARGA-NR_TICKET
         AND D~TP_STATUS NE @ZIF_CARGA~ST_STATUS_CANCELADA
         AND EXISTS ( SELECT * FROM ZSDT0001NT AS N WHERE N~ID_CARGA EQ D~ID_CARGA AND N~ID_FORNECEDOR EQ @WA_NFISCAL-ID_FORNECEDOR ).

      IF SY-SUBRC IS INITIAL.
        "218 Ticket Pesagem &1 já Utilizado para o Produtor &2 e Safra &3!
        MESSAGE W218 WITH ME->CARGA-NR_TICKET WA_NFISCAL-ID_FORNECEDOR ME->CARGA-NR_SAFRA.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  method zif_carga~add_classificacao.

    check i_nota-nr_chave_nfe is not initial.



  endmethod.


  METHOD zif_carga~check_romaneios_carga_opus.

    DATA: wa_zsdt0001 TYPE zsdt0001.
    "Romaneios excluidos da carga
    SELECT *
      FROM zsdt0001ex
      INTO TABLE @DATA(t_zsdt0001ex)
      WHERE id_carga = @i_id_carga.

    "Romaneios ativos
    SELECT *
      FROM zsdt0001
      APPENDING  CORRESPONDING FIELDS OF TABLE t_zsdt0001ex
      WHERE id_carga = i_id_carga.

    IF t_zsdt0001ex[] IS NOT INITIAL.
      READ TABLE t_zsdt0001ex INTO DATA(w_zsdt0001ex) INDEX 1.
      SELECT COUNT(*)
        FROM tvarvc
         WHERE name = 'MAGGI_OPUS_EX'  "Vai checar os romaneios cancelados se estiver parametrizado
         AND   low  = w_zsdt0001ex-branch.
      IF sy-subrc NE 0.
        SELECT COUNT(*)
       FROM tvarvc
        WHERE name = 'MAGGI_OPUS_EX'  "Vai checar os romaneios cancelados se estiver parametrizado
        AND   low  = '9999'.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT t_zsdt0001ex INTO w_zsdt0001ex.
*      IF w_zsdt0001ex-ch_referencia+0(2) = 'RO'.
*        CONTINUE.
*      ENDIF.
      TRY .
          DATA(lc_romaneio) = zcl_romaneio=>get_instance( ).
          lc_romaneio->zif_cadastro~set_registroex( i_id_registro = w_zsdt0001ex-ch_referencia ).
          lc_romaneio->get_registro( IMPORTING e_registro = wa_zsdt0001 ).
          lc_romaneio->get_consulta_status_opus( IMPORTING e_status = DATA(e_status) ).
        CATCH zcx_cadastro INTO DATA(ex_cadastro).
          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = VALUE #( msgid = ex_cadastro->msgid
                                msgno = ex_cadastro->msgno
                                attr1 = ex_cadastro->msgv1
                                attr2 = ex_cadastro->msgv2
                                attr3 = ex_cadastro->msgv3
                                attr4 = ex_cadastro->msgv4 )
              msgty  = 'E'
              msgid  = ex_cadastro->msgid
              msgno  = ex_cadastro->msgno
              msgv1  = ex_cadastro->msgv1
              msgv2  = ex_cadastro->msgv2
              msgv3  = ex_cadastro->msgv3
              msgv4  = ex_cadastro->msgv4.

        CATCH zcx_romaneio INTO DATA(ex_romaneio).
          IF ex_romaneio->msgid = zcx_romaneio=>zcx_erro_rom_nao_encontrado-msgid AND
             ex_romaneio->msgno = zcx_romaneio=>zcx_erro_rom_nao_encontrado-msgno.
            CLEAR e_status.
          ELSE.
            RAISE EXCEPTION TYPE zcx_carga
              EXPORTING
                textid = VALUE #( msgid = ex_romaneio->msgid
                                  msgno = ex_romaneio->msgno
                                  attr1 = ex_romaneio->msgv1
                                  attr2 = ex_romaneio->msgv2
                                  attr3 = ex_romaneio->msgv3
                                  attr4 = ex_romaneio->msgv4 )
                msgty  = 'E'
                msgid  = ex_romaneio->msgid
                msgno  = ex_romaneio->msgno
                msgv1  = ex_romaneio->msgv1
                msgv2  = ex_romaneio->msgv2
                msgv3  = ex_romaneio->msgv3
                msgv4  = ex_romaneio->msgv4.
          ENDIF.
      ENDTRY.
      IF e_status NE st_status_cancelada AND  e_status IS NOT INITIAL.

        DATA(tx_status) = COND string( WHEN e_status EQ zif_carga=>st_status_conferido THEN 'Conferido' ELSE 'Fechado' ).
        data(tx_compl) = '->Conferir a carga novamente na ZMM0127'.
        "Romaneio de Saída &MSGV1& está &MSGV2& no OPUS!
        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_rom_status_opus-msgid
                              msgno = zcx_carga=>zcx_rom_status_opus-msgno
                              attr1 = wa_zsdt0001-nr_romaneio
                              attr2 = tx_status
                              attr3 = tx_compl )
            msgty  = 'E'
            msgid  = zcx_carga=>zcx_rom_status_opus-msgid
            msgno  = zcx_carga=>zcx_rom_status_opus-msgno
            msgv1  = CONV #( wa_zsdt0001-nr_romaneio )
            msgv2  = CONV #( tx_status )
            msgv3  = CONV #( tx_compl ).

      ENDIF.



    ENDLOOP.


  ENDMETHOD.


  method ZIF_CARGA~SET_ORDEM_CARREGAMENTO_EXT.

* CS2021000978 Validação da ordem de carregamento de terceiros para embarques na modalidade CPT
    r_carga = me.

    IF i_nr_ordem IS INITIAL.
      CLEAR: me->carga-id_ordem.
    ENDIF.

    CHECK i_nr_ordem IS NOT INITIAL.

    SELECT id_ordem_ext
      UP TO 1 ROWS
      FROM zsdt0001cg
      INTO @DATA(lv_ord_ext)
    WHERE nr_safra     EQ @i_nr_safra
      AND id_bukrs     EQ @i_id_bukrs
      AND id_branch    EQ @i_id_branch
      AND id_ordem_ext EQ @i_nr_ordem.
    ENDSELECT.

    IF sy-subrc IS INITIAL AND lv_ord_ext IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_err_ordext-msgid msgno = zcx_carga=>zcx_err_ordext-msgno )
          msgty  = 'E'
          msgid  = zcx_carga=>zcx_err_ordext-msgid
          msgno  = zcx_carga=>zcx_err_ordext-msgno.
    ELSE.
      me->carga-id_ordem_ext = i_nr_ordem.
    ENDIF.

  endmethod.


  METHOD zif_carga~check_ob_valida_entrada_fob.

    DATA: lv_msg TYPE c LENGTH 250.
    DATA: lv_msg_string TYPE string.

    FIELD-SYMBOLS: <fs_has_deposity_regulation> TYPE abap_bool.

    DATA: lwa_return_erro TYPE zstruct_return_api_eudr.

    CLEAR: me->zif_carga~st_valida_entrada_fob,
           me->zif_carga~carga-eudr,
           me->zif_carga~carga-protocolo_eudr,
           me->zif_carga~carga-id_protocolo_eudr.


    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = |Realizando validações FOB no OPUS...|.

    DATA(lit_documentos_fiscais) = me->documento_fiscal[].

*    IF i_nota IS NOT INITIAL.
*      APPEND i_nota TO lit_documentos_fiscais[].
*    ENDIF.

    CHECK lit_documentos_fiscais[] IS NOT INITIAL.

    me->zif_carga~st_valida_entrada_fob-code_company       = me->carga-id_bukrs.
    me->zif_carga~st_valida_entrada_fob-code_branch        = me->carga-id_branch.
    me->zif_carga~st_valida_entrada_fob-boarding_place     = me->carga-id_local_coleta.
    me->zif_carga~st_valida_entrada_fob-identifier         = |IC{ me->carga-id_carga }|.
    me->zif_carga~st_valida_entrada_fob-year               = me->carga-nr_safra.

    IF me->zif_carga~ordem_venda[] IS NOT INITIAL.

      me->zif_carga~st_valida_entrada_fob-document_type    = me->zif_carga~st_document_type_ov.

      READ TABLE me->zif_carga~ordem_venda INTO DATA(lwa_ordem_venda) INDEX 1.
      CHECK sy-subrc EQ 0.

      SELECT SINGLE vbeln, auart
        FROM vbak INTO @DATA(lwa_vbak)
       WHERE vbeln EQ @lwa_ordem_venda-nr_ordem_venda.

      CHECK sy-subrc EQ 0.

      me->zif_carga~st_valida_entrada_fob-document_sub_type  = lwa_vbak-auart.

      zcl_eudr_utils=>check_ov_pedido_eudr(
        EXPORTING
          i_vbeln = lwa_ordem_venda-nr_ordem_venda
        RECEIVING
          r_eudr  = DATA(lva_ov_pedido_eudr) ).

    ELSEIF me->zif_carga~pedido_compra[] IS NOT INITIAL.

      me->zif_carga~st_valida_entrada_fob-document_type    = me->zif_carga~st_document_type_pd.

      READ TABLE me->zif_carga~pedido_compra INTO DATA(lwa_pedido_compra) INDEX 1.
      CHECK sy-subrc EQ 0.

      SELECT SINGLE ebeln, bsart
        FROM ekko INTO @DATA(lwa_ekko)
       WHERE ebeln EQ @lwa_pedido_compra-nr_pedido_compra.

      CHECK sy-subrc EQ 0.

      me->zif_carga~st_valida_entrada_fob-document_sub_type  = lwa_ekko-bsart.

      zcl_eudr_utils=>check_ov_pedido_eudr(
        EXPORTING
          i_ebeln = lwa_pedido_compra-nr_pedido_compra
        RECEIVING
          r_eudr  = lva_ov_pedido_eudr ).

    ENDIF.

    me->zif_carga~st_valida_entrada_fob-transshipment_code  = me->carga-id_local_descarga.
    me->zif_carga~st_valida_entrada_fob-terminal_code       = me->carga-id_local_destino.
    me->zif_carga~st_valida_entrada_fob-code_product        = me->carga-id_produto.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = me->zif_carga~st_valida_entrada_fob-code_product
      IMPORTING
        output = me->zif_carga~st_valida_entrada_fob-code_product.

    CASE lva_ov_pedido_eudr.
      WHEN 'S' OR 'N'.

        CREATE DATA me->zif_carga~st_valida_entrada_fob-has_deposity_regulation TYPE abap_bool.
        ASSIGN me->zif_carga~st_valida_entrada_fob-has_deposity_regulation->* TO <fs_has_deposity_regulation>.

        CASE lva_ov_pedido_eudr.
          WHEN 'S'.
            <fs_has_deposity_regulation> = abap_true.
          WHEN 'N'.
            <fs_has_deposity_regulation> = abap_false.
        ENDCASE.
      WHEN 'A'.
    ENDCASE.

    DATA(lit_documentos_fiscais_aux) = lit_documentos_fiscais[]. "MM- Ajuste Validação ZMM0127 Issue 180882 - WPP --->>>

    LOOP AT lit_documentos_fiscais[] INTO DATA(ls_notas).
      APPEND INITIAL LINE TO me->zif_carga~st_valida_entrada_fob-documents ASSIGNING FIELD-SYMBOL(<fs_documents>).

      SELECT SINGLE *
        FROM lfa1 INTO @DATA(lwa_lfa1)
       WHERE lifnr EQ @ls_notas-id_fornecedor.
      IF sy-subrc EQ 0.
        IF lwa_lfa1-stcd1 IS NOT INITIAL.
          <fs_documents>-farmer_document = lwa_lfa1-stcd1.
        ELSEIF lwa_lfa1-stcd2 IS NOT INITIAL..
          <fs_documents>-farmer_document = lwa_lfa1-stcd2.
        ENDIF.
      ENDIF.

      "MM- Ajuste Validação ZMM0127 Issue 180882 - WPP --->>>
      LOOP AT lit_documentos_fiscais_aux INTO DATA(lwa_doc_fiscal_aux) WHERE id_fornecedor = ls_notas-id_fornecedor.
        APPEND INITIAL LINE TO <fs_documents>-state_registrations ASSIGNING FIELD-SYMBOL(<fs_state_registrations>).
        <fs_state_registrations>-registration = lwa_doc_fiscal_aux-nr_fornecedor_ie.
        <fs_state_registrations>-capacity     = lwa_doc_fiscal_aux-nr_quantidade.
      ENDLOOP.
      "MM- Ajuste Validação ZMM0127 Issue 180882 - WPP <<<---

    ENDLOOP.

    SORT me->zif_carga~st_valida_entrada_fob-documents BY farmer_document.
    DELETE ADJACENT DUPLICATES FROM me->zif_carga~st_valida_entrada_fob-documents COMPARING ALL FIELDS.

    "MM- Ajuste Validação ZMM0127 Issue 180882 - WPP --->>>
*    LOOP AT me->zif_carga~st_valida_entrada_fob-documents ASSIGNING <fs_documents>.
*
*      "Busca para CNPJ dos dados da NOTA
*      LOOP AT lit_documentos_fiscais[] INTO ls_notas WHERE nr_chave_nfe+6(14) EQ <fs_documents>-farmer_document.
*        APPEND INITIAL LINE TO <fs_documents>-state_registrations ASSIGNING FIELD-SYMBOL(<fs_state_registrations>).
*        <fs_state_registrations>-registration = ls_notas-nr_fornecedor_ie.
*        <fs_state_registrations>-capacity     = ls_notas-nr_quantidade.
*      ENDLOOP.
*
*      "Busca para CPF dos dados da NOTA
*      LOOP AT lit_documentos_fiscais[] INTO ls_notas WHERE nr_chave_nfe+9(11) EQ <fs_documents>-farmer_document.
*        APPEND INITIAL LINE TO <fs_documents>-state_registrations ASSIGNING <fs_state_registrations>.
*        <fs_state_registrations>-registration = ls_notas-nr_fornecedor_ie.
*        <fs_state_registrations>-capacity     = ls_notas-nr_quantidade.
*      ENDLOOP.
*
*    ENDLOOP.
    "MM- Ajuste Validação ZMM0127 Issue 180882 - WPP <<<---

    TRY.
        "Verifica se a Entrada é EUDR
        zcl_int_ob_valida_entrada_fob=>zif_integracao_outbound~get_instance( )->execute_request(
        EXPORTING
          i_info_request           = me->zif_carga~st_valida_entrada_fob
        IMPORTING
          e_id_integracao          = DATA(resul_id)
          e_integracao             = DATA(result_json)
        ).

      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).

        lv_msg = |Não foi possivel realizar a validação da entrada FOB no OPUS!|.

        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_erro_geral-msgid
                              msgno = zcx_carga=>zcx_erro_geral-msgno
                              attr1 = CONV #( lv_msg+000(50) )
                              attr2 = CONV #( lv_msg+050(50) )
                              attr3 = CONV #( lv_msg+100(50) )
                              attr4 = CONV #( lv_msg+150(50) ) )
            msgid  = zcx_carga=>zcx_erro_geral-msgid
            msgno  = zcx_carga=>zcx_erro_geral-msgno
            msgty  = cl_abap_aab_utilities=>category_error
            msgv1  = CONV #( lv_msg+000(50) )
            msgv2  = CONV #( lv_msg+050(50) )
            msgv3  = CONV #( lv_msg+100(50) )
            msgv4  = CONV #( lv_msg+150(50) ).
      CATCH zcx_error INTO DATA(zcx_error).

        lv_msg = |Não foi possivel realizar a validação da entrada FOB no OPUS!|.

        IF resul_id IS NOT INITIAL.
          SELECT SINGLE ds_data_retorno
            FROM zintegracao_log INTO @DATA(lva_data_return_erro)
            WHERE id_integracao EQ @resul_id.

          IF sy-subrc EQ 0 AND lva_data_return_erro IS NOT INITIAL.
            /ui2/cl_json=>deserialize( EXPORTING json = lva_data_return_erro CHANGING data = lwa_return_erro ).

            IF lwa_return_erro-data IS NOT INITIAL.
              lv_msg = |(OPUS) - { lwa_return_erro-data }|.
            ENDIF.
          ENDIF.
        ENDIF.

        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = zcx_carga=>zcx_erro_geral-msgid
                              msgno = zcx_carga=>zcx_erro_geral-msgno
                              attr1 = CONV #( lv_msg+000(50) )
                              attr2 = CONV #( lv_msg+050(50) )
                              attr3 = CONV #( lv_msg+100(50) )
                              attr4 = CONV #( lv_msg+150(50) ) )
            msgid  = zcx_carga=>zcx_erro_geral-msgid
            msgno  = zcx_carga=>zcx_erro_geral-msgno
            msgty  = cl_abap_aab_utilities=>category_error
            msgv1  = CONV #( lv_msg+000(50) )
            msgv2  = CONV #( lv_msg+050(50) )
            msgv3  = CONV #( lv_msg+100(50) )
            msgv4  = CONV #( lv_msg+150(50) ).
    ENDTRY.

    "Recupera os dados do JSON
    IF result_json IS NOT INITIAL.
      /ui2/cl_json=>deserialize(
        EXPORTING
          json = result_json-ds_data_retorno
        CHANGING
          data = me->zif_carga~st_ret_entrada_fob ).
    ENDIF.

    IF me->zif_carga~st_ret_entrada_fob-is_blocked IS NOT INITIAL.

      IF lines( me->zif_carga~st_ret_entrada_fob-validations ) > 1.
        LOOP AT me->zif_carga~st_ret_entrada_fob-validations INTO DATA(lwa_validation).
          DATA(tabix) = sy-tabix.
          lv_msg_string = |(OPUS) Mensagem: { tabix } - { lwa_validation-description } |.
          MESSAGE lv_msg_string TYPE 'I'.
        ENDLOOP.
      ENDIF.

      READ TABLE me->zif_carga~st_ret_entrada_fob-validations INTO lwa_validation WITH KEY is_block = abap_true.
      IF sy-subrc EQ 0.
        lv_msg = lwa_validation-description.
      ELSE.
        lv_msg = me->zif_carga~st_ret_entrada_fob-validations[ 1 ]-description.
      ENDIF.

      lv_msg = |(OPUS) - { lv_msg } |.

      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = zcx_carga=>zcx_erro_geral-msgid
                            msgno = zcx_carga=>zcx_erro_geral-msgno
                            attr1 = CONV #( lv_msg+000(50) )
                            attr2 = CONV #( lv_msg+050(50) )
                            attr3 = CONV #( lv_msg+100(50) )
                            attr4 = CONV #( lv_msg+150(50) ) )
          msgid  = zcx_carga=>zcx_erro_geral-msgid
          msgno  = zcx_carga=>zcx_erro_geral-msgno
          msgty  = cl_abap_aab_utilities=>category_error
          msgv1  = CONV #( lv_msg+000(50) )
          msgv2  = CONV #( lv_msg+050(50) )
          msgv3  = CONV #( lv_msg+100(50) )
          msgv4  = CONV #( lv_msg+150(50) ).

    ENDIF.

    IF me->zif_carga~st_ret_entrada_fob-branch_is_regulamentation            EQ abap_true AND
       me->zif_carga~st_ret_entrada_fob-product_parameter-is_regulamentation EQ abap_true.

      IF me->zif_carga~st_ret_entrada_fob-is_regulamentation EQ abap_true.
        me->zif_carga~carga-eudr = zcl_eudr_utils=>lc_s_eudr.
      ELSE.
        me->zif_carga~carga-eudr = zcl_eudr_utils=>lc_n_eudr.
      ENDIF.

      me->zif_carga~carga-protocolo_eudr    = me->zif_carga~st_ret_entrada_fob-code.
      me->zif_carga~carga-id_protocolo_eudr = me->zif_carga~st_ret_entrada_fob-id.

    ENDIF.

  ENDMETHOD.


  method zif_carga~get_classificacao.
    r_carga = me.
    move-corresponding me->classificacao to e_registro.
  endmethod.


  method zif_carga~get_classificao_notas.
    read table me->classificacao_notas into e_classificacao
     with key id_carga         = i_id_carga
              id_classificacao = i_id_classificacao.
    if sy-subrc ne 0.
      if me->zif_carga~at_manutencao eq abap_true.
        clear e_classificacao.
      else.
        select single *
          from zsdt0001cl
          into e_classificacao
          where id_carga         = i_id_carga
          and   id_classificacao = i_id_classificacao.
        if sy-subrc ne 0.
          clear e_classificacao.
        endif.
      endif.
    endif.

  endmethod.


  method zif_carga~set_classificacao.
    r_carga = me.

    me->classificacao      = i_classificacao.
  endmethod.
ENDCLASS.
