FUNCTION z_les_adto_pedagio.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(START_COLUMN) TYPE  SY-CUCOL DEFAULT 90
*"     VALUE(START_ROW) TYPE  SY-CUROW DEFAULT 14
*"     VALUE(END_COLUMN) TYPE  SY-CUCOL DEFAULT 130
*"     VALUE(END_ROW) TYPE  SY-CUROW DEFAULT 20
*"     VALUE(TKNUM) TYPE  TKNUM OPTIONAL
*"     REFERENCE(I_ZLEST0108) TYPE  ZLEST0108 OPTIONAL
*"     REFERENCE(I_FATURA_AUT) TYPE  ZLESE0220 OPTIONAL
*"     REFERENCE(I_ZLEST0240_IN) TYPE  ZLEST0240 OPTIONAL
*"  EXPORTING
*"     VALUE(E_FATURA_AUT) TYPE  ZLESE0221
*"  TABLES
*"      C_XVTTK STRUCTURE  VTTKVB OPTIONAL
*"      C_XVTTS STRUCTURE  VTTSVB OPTIONAL
*"  EXCEPTIONS
*"      ERRO
*"      PEDAGIO
*"----------------------------------------------------------------------

  CLEAR: l_shipment_data,
         vl_tknum,
         wk_netwr_all,
         wk_waers_all,
         wk_errors,
         vl_route,
         vl_tplst,
         vl_tdlnr,
         vl_bukrs,
         vl_adto,
         wk_0026,
         vl_fknum,
         vl_chvadto,
         ti_xvttk[],
         ti_xvtts[],
         ti_xvttp[],
         ti_placa,
         v_region,
         v_country,
         v_placa1,
         v_placa2,
         v_placa3,
         v_cidade1,
         v_cidade2,
         v_cidade3,
         nm_fornecedor,
         t_dynpfields[],
         vg_cartao_pedagio,
         vg_pedi_pedagio,
         vg_pedi_informado,
         vg_ck_ped_param,
         vg_ck_alterou_cidade,
         vg_ck_admim_ped,
         vg_ck_credito_ped,
         vg_ck_credito_pedt,
         vg_pedi_mesmo_veicul,
         vg_pedi_mesma_carga,
         vg_ck_admim_frete,
         parceiro_pv,
         lc_tx_cid_origem,
         lc_tx_cid_destino,
         vg_solicita_pedagio,
         w_zlest0241,   "*-#133089-12.02.2024-JT-inicio
         e_fatura_aut.  "*-#133089-12.02.2024-JT-inicio

  DATA: wa_zlest0027 TYPE zlest0027,
        wk_xvttk     TYPE vttkvb.

  DATA: vg_branch        TYPE j_1bbranc_,
        lc_repom_roteiro TYPE REF TO zcl_repom_roteiro_vlr_vpr,
        e_erros          TYPE zde_repom_erros_t.

  "Verificar se o Pedágio/Adiantamento é Automático
*  IF I_ZLEST0108 IS INITIAL.
*    SELECT SINGLE *
*      INTO @DATA(WA_ZLEST0108)
*      FROM ZLEST0108
*     WHERE DOC_TRANSP EQ @TKNUM.
*  ELSE.
*    WA_ZLEST0108 = I_ZLEST0108.
*  ENDIF.

  REFRESH: ti_xvttk, ti_xvtts. CLEAR: ti_xvttk, ti_xvtts.

  IF NOT c_xvttk[] IS INITIAL.

    ti_xvttk[] = c_xvttk[].
    ti_xvtts[] = c_xvtts[].

    READ TABLE ti_xvttk INTO wk_xvttk INDEX 1.

    SELECT SINGLE * INTO wa_zlest0027
      FROM zlest0027
     WHERE route = wk_xvttk-route.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e092(zles) RAISING pedagio.
    ENDIF.

    DATA(lc_ck_pedagio) = wa_zlest0027-ck_pedagio.

  ENDIF.

*-#133089-12.02.2024-JT-inicio
  vg_faturamento_autom = abap_false.
  vg_faturauto_in      = i_fatura_aut.
  vg_zlest0240_in      = i_zlest0240_in.

  IF vg_faturauto_in IS INITIAL.
    SELECT SINGLE *
      FROM zlest0241
      INTO w_zlest0241
     WHERE ch_referencia = wk_xvttk-id_romaneio
       AND cancelado     = abap_false.
    IF sy-subrc = 0.
      vg_faturamento_autom = abap_true.
    ENDIF.

    IF ( sy-tcode = 'ZLES0106' ) OR
       ( sy-tcode = 'ZLES0113' ) OR
       ( sy-tcode = 'ZLES0115' ) OR
       ( sy-tcode = 'ZLES0136' ) OR
       ( sy-tcode = 'ZMM0127'  ) OR
       ( sy-tcode = 'ZNFE'     ) OR  "*-CS2024000086-26.09.2024-#151423-JT-inicio
       ( sy-tcode = 'ZLES0200' ).
      vg_faturamento_autom = abap_false.
    ENDIF.
  ENDIF.
*-#133089-12.02.2024-JT-fim

  DATA false(1).

  vl_tknum = tknum.

  IF ( sy-tcode = 'ZLES0106' ) OR
     ( sy-tcode = 'ZLES0113' ) OR
     ( sy-tcode = 'ZLES0115' ) OR
     ( sy-tcode = 'ZLES0136' ) OR
     ( sy-tcode = 'ZMM0127'  ) OR
     ( sy-tcode = 'ZNFE'     ) OR  "*-CS2024000086-26.09.2024-#151423-JT-inicio
*-CS2021001045 - 03.02.2022 - JT - inicio
     ( sy-tcode = 'ZLES0200' ) OR
*-CS2021001045 - 03.02.2022 - JT - fim
     ( vg_faturamento_autom = abap_true ) OR  "*-#133089-12.02.2024-JT
     ( wk_xvttk-id_carga IS NOT INITIAL AND sy-tcode(2) NE 'VT' ).
    PERFORM transport_lesen(sapmv56a) USING vl_tknum false.
  ENDIF.

*-#133089-12.02.2024-JT-inicio
  IF vg_faturauto_in IS NOT INITIAL.   "*-#133089-12.02.2024-JT-inicio
    wk_netwr_all = vg_faturauto_in-valor_frete.
    wk_waers_all = vg_faturauto_in-moeda_frete.
  ELSE.
*   Valor do adiantamento
    PERFORM scd_cost_info_interface_fill(sapmv56a) USING vl_tknum CHANGING l_shipment_data.

    l_shipment_data-xvttk[] = c_xvttk[].

    CALL FUNCTION 'SD_SCD_COST_INFO_SHIPMENT'
      EXPORTING
        i_shipment_data  = l_shipment_data
        i_opt_dialog     = space
        i_opt_batch_mode = sy-batch
        i_opt_sim        = '3'
      IMPORTING
        e_result         = wk_netwr_all
        e_result_waers   = wk_waers_all
        e_errors_occured = wk_errors.
  ENDIF.
*-#133089-12.02.2024-JT-fim

  IF NOT wk_netwr_all IS INITIAL.

    READ TABLE c_xvttk INDEX 1 INTO DATA(lc_vttk).

    DATA(lc_ck_automatico) = abap_false.

    IF lc_vttk-id_carga IS NOT INITIAL.

      "1  Transporte de partida com carga
      "2  Transporte de chegada com carga

      CASE lc_vttk-abfer.
        WHEN '1'.

          SELECT SINGLE * INTO @DATA(wa_zsdt0001ft)
            FROM zsdt0001ft
           WHERE id_carga EQ @lc_vttk-id_carga.

          IF sy-subrc IS INITIAL.
            lc_ck_automatico = abap_true.
          ENDIF.

        WHEN '2'.

          SELECT SINGLE * INTO @DATA(wa_zsdt0001fe)
            FROM zsdt0001fe
           WHERE id_carga EQ @lc_vttk-id_carga.

          IF sy-subrc IS INITIAL.
            lc_ck_automatico = abap_true.
          ENDIF.

      ENDCASE.


    ENDIF.

    CASE lc_ck_automatico.
      WHEN abap_true.

        CLEAR wk_0026.

        " Local de organizaçao de transporte
        SELECT SINGLE bukrs INTO vl_bukrs FROM ttds WHERE tplst = lc_vttk-tplst.
        vg_branch = lc_vttk-tplst.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = vg_branch
          IMPORTING
            output = vg_branch.

        CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
          EXPORTING
            centro               = vg_branch
          IMPORTING
            centro_out           = vg_branch
          EXCEPTIONS
            informar_centro      = 1
            nao_centro_r_virtual = 2
            informar_centro_out  = 3
            informar_centro_v    = 4
            OTHERS               = 5.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro.
        ENDIF.

        "SELECT SINGLE CK_PEDAGIO INTO @DATA(LC_CK_PED) FROM ZLEST0027 WHERE ROUTE = @LC_VTTK-ROUTE.

        "Consulta Situação da Placa
        CALL METHOD zcl_webservice_tipcard=>cons_situacao_transportador
          EXPORTING
            i_placa     = lc_vttk-text1(7)
          RECEIVING
            e_consultas = DATA(e_consultas)
          EXCEPTIONS
            erro        = 1
            webservice  = 2
            OTHERS      = 3.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE e000(zles) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro.
        ENDIF.

        READ TABLE e_consultas INDEX 1 INTO DATA(wa_zlest0135).
        IF wa_zlest0135-ck_rntrc_ativo EQ abap_false.
          sy-msgv1 = wa_zlest0135-ds_msg_transportador+000(50).
          sy-msgv2 = wa_zlest0135-ds_msg_transportador+050(50).
          sy-msgv3 = wa_zlest0135-ds_msg_transportador+100(50).
          sy-msgv4 = wa_zlest0135-ds_msg_transportador+150(50)..
          MESSAGE e000(zles) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4  RAISING erro.
        ENDIF.

        DATA(ck_repom_eletronico) = abap_false.

        IF wa_zsdt0001ft IS NOT INITIAL.
          DATA(i_ag_bukrs)          = wa_zsdt0001ft-ag_bukrs.
          DATA(i_ag_branch)         = wa_zsdt0001ft-ag_branch.
          DATA(i_id_bukrs)          = wa_zsdt0001ft-id_bukrs.
          DATA(i_id_branch)         = wa_zsdt0001ft-id_branch.
          DATA(i_id_rota_repom)     = wa_zsdt0001ft-id_rota_repom.
          DATA(i_id_percurso_repom) = wa_zsdt0001ft-id_percurso_repom.
          DATA(i_veiculo_eixos)     = wa_zsdt0001ft-nm_qtd_eixos.
          DATA(i_nr_cartao)         = wa_zsdt0001ft-nr_cartao_ped_repom.
          DATA(id_proprietario)     = wa_zsdt0001ft-id_proprietario.
          DATA(vl_adiantamento)     = wa_zsdt0001ft-vl_adiantamento.
          DATA(i_adm_frete)         = wa_zsdt0001ft-tp_admim_frete.
          DATA(i_adm_pedag)         = wa_zsdt0001ft-tp_admim_ped.
          DATA(i_ck_pedagio)        = wa_zsdt0001ft-ck_credita_ped.
          DATA(i_cd_cid_origem)     = wa_zsdt0001ft-cd_cid_origem.
          DATA(i_cd_cid_destino)    = wa_zsdt0001ft-cd_cid_destino.

          IF wa_zsdt0001ft-tp_adm_uso EQ 'E'.
            ck_repom_eletronico = abap_true.
          ENDIF.
        ELSEIF wa_zsdt0001fe IS NOT INITIAL.

          i_ag_bukrs          = wa_zsdt0001fe-ag_bukrs.
          i_ag_branch         = wa_zsdt0001fe-ag_branch.
          i_id_bukrs          = wa_zsdt0001fe-id_bukrs.
          i_id_branch         = wa_zsdt0001fe-id_branch.
          i_id_rota_repom     = wa_zsdt0001fe-id_rota_repom.
          i_id_percurso_repom = wa_zsdt0001fe-id_percurso_repom.
          i_veiculo_eixos     = wa_zsdt0001fe-nm_qtd_eixos.
          i_nr_cartao         = wa_zsdt0001fe-nr_cartao_ped_repom.
          id_proprietario     = wa_zsdt0001fe-id_proprietario.
          vl_adiantamento     = wa_zsdt0001fe-vl_adiantamento.
          i_adm_frete         = wa_zsdt0001fe-tp_admim_frete.
          i_adm_pedag         = wa_zsdt0001fe-tp_admim_ped.
          i_ck_pedagio        = wa_zsdt0001fe-ck_credita_ped.
          i_cd_cid_origem     = wa_zsdt0001fe-cd_cid_origem.
          i_cd_cid_destino    = wa_zsdt0001fe-cd_cid_destino.

          IF wa_zsdt0001fe-tp_adm_uso EQ 'E'.
            ck_repom_eletronico = abap_true.
          ENDIF.

        ENDIF.

        IF i_ck_pedagio EQ abap_true.

          "Pedágio REPOM/TIPFRETE
          IF ck_repom_eletronico EQ abap_true.

            CASE wa_zsdt0001ft-tp_admim_ped.
              WHEN '03'. "REPOM

                CREATE OBJECT lc_repom_roteiro.
                lc_repom_roteiro->set_bukrs( EXPORTING i_bukrs = i_ag_bukrs ).
                lc_repom_roteiro->set_branch( EXPORTING i_branch = i_ag_branch ).
                lc_repom_roteiro->set_id_rota_repom( EXPORTING i_id_rota_repom = i_id_rota_repom ).
                lc_repom_roteiro->set_id_percurso_repom( EXPORTING i_id_percurso_repom = i_id_percurso_repom ).
                lc_repom_roteiro->set_veiculo_eixos( EXPORTING i_veiculo_eixos = i_veiculo_eixos ).
                lc_repom_roteiro->set_qtd_eixos_suspensos_ida( EXPORTING i_qtd_eixos_suspensos_ida = 0 ).
                lc_repom_roteiro->set_qtd_eixos_suspensos_volta( EXPORTING i_qtd_eixos_suspensos_volta = 0 ).

                CALL METHOD lc_repom_roteiro->consultar_valor
                  IMPORTING
                    e_erros                    = e_erros
                  RECEIVING
                    i_retornou                 = DATA(p_retornou)
                  EXCEPTIONS
                    servico_nao_encontrado     = 1
                    http_communication_failure = 2
                    http_invalid_state         = 3
                    http_processing_failed     = 4
                    http_invalid_timeout       = 5
                    erro                       = 6
                    OTHERS                     = 7.

                IF sy-subrc IS NOT INITIAL.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro.
                ENDIF.

                IF p_retornou EQ abap_false.
                  CLEAR: lc_repom_roteiro.
                  LOOP AT e_erros INTO DATA(lc_erro).
                    MESSAGE e017(zrepom) WITH lc_erro-erro_codigo lc_erro-erro_descricao RAISING erro.
                  ENDLOOP.
                ENDIF.

                IF lc_repom_roteiro->get_valor_total_vpr( ) GT 2000.
                  CLEAR: lc_repom_roteiro.
                  MESSAGE e079(zles) RAISING erro.
                ENDIF.
                CLEAR: lc_repom_roteiro.

                TRY.
                    CALL METHOD zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~set_gerar_pedagio
                      EXPORTING
                        i_tknum             = lc_vttk-tknum
                        i_nr_cartao         = i_nr_cartao
                        i_id_rota_repom     = i_id_rota_repom
                        i_id_percurso_repom = i_id_percurso_repom
                        i_cd_cid_origem     = i_cd_cid_origem
                        i_cd_cid_destino    = i_cd_cid_destino
                      RECEIVING
                        e_zlest0123         = DATA(e_zlest0123).
                  CATCH zcx_doc_fiscal_ft_entrada INTO DATA(ex_erro).
                    ex_erro->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
                    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro.
                ENDTRY.
              WHEN '09'. "TipFRETE
                """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                "" PEDÁGIO AUTOMÁTICO DA TIPFRETE """"""""""""""""""""""""""""""""""""""""""""

                """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            ENDCASE.
          ELSE.
            "Pedágio MANUAL
            CASE lc_vttk-abfer.
              WHEN '1'.
                e_zlest0123-vlr_total_pedagio = wa_zsdt0001ft-vl_pedagio.
              WHEN '2'.
                e_zlest0123-vlr_total_pedagio = wa_zsdt0001fe-vl_pedagio.
            ENDCASE.

          ENDIF.
        ELSE.
          CLEAR:
          e_zlest0123-vlr_total_pedagio,
          e_zlest0123-id_proc_cliente.
        ENDIF.

        IF vl_adiantamento GT 0.

          TRY .
              DATA(r_margadto) =
              zcl_calc_frete=>get_valor_adiantamento(
                i_bukrs  = i_id_bukrs
                i_branch = i_id_branch
                i_lifnr  = id_proprietario ).

              DATA(vl_adiantamento_local) = wk_netwr_all * ( r_margadto / 100 ).

              IF vl_adiantamento GT vl_adiantamento_local.
                MESSAGE e022(zles) WITH r_margadto RAISING erro.
              ENDIF.

            CATCH zcx_calc_frete INTO DATA(ex_calc_frete).
              ex_calc_frete->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro.
          ENDTRY.

        ENDIF.

        DATA(ck_gera_chave) = abap_true.
        IF wa_zsdt0001ft IS NOT INITIAL.
          SELECT SINGLE * INTO wk_0026 FROM zlest0026 WHERE id_carga EQ wa_zsdt0001ft-id_carga.
          IF sy-subrc IS INITIAL.
            ck_gera_chave = abap_false.
            wk_0026-seq_controle = wk_0026-seq_controle.
          ENDIF.
        ELSEIF wa_zsdt0001fe IS NOT INITIAL.
          SELECT SINGLE * INTO wk_0026 FROM zlest0026 WHERE id_carga EQ wa_zsdt0001fe-id_carga.
          IF sy-subrc IS INITIAL.
            ck_gera_chave = abap_false.
            wk_0026-seq_controle = wk_0026-seq_controle.
          ENDIF.
        ENDIF.

        CLEAR: wk_0026-seq_controle.
        IF ck_gera_chave EQ abap_true.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = 'ZTRP01'
            IMPORTING
              number                  = wk_0026-seq_controle
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.
        ENDIF.

        IF wk_0026-seq_controle IS NOT INITIAL.
          wk_0026-mandt           = sy-mandt.
          wk_0026-tknum           = wk_0026-seq_controle.
          wk_0026-pedagio         = e_zlest0123-vlr_total_pedagio.
          wk_0026-erdat           = sy-datum.
          wk_0026-uzeit           = sy-uzeit.
          wk_0026-uname           = sy-uname.
          wk_0026-adto            = vl_adiantamento.
          wk_0026-tp_admim_ped    = i_adm_pedag.
          wk_0026-tp_card_ped     = space.
          wk_0026-nr_card_ped     = space.
          wk_0026-id_rota         = space.
          wk_0026-qtd_eixo        = i_veiculo_eixos.
          wk_0026-nr_vr_xml_tipf  = '1.17'.
          wk_0026-ck_credita_ped  = i_ck_pedagio.
          wk_0026-tp_admim_frete  = i_adm_frete.
          wk_0026-id_proc_cliente = e_zlest0123-id_proc_cliente.
          wk_0026-placa_cav       = lc_vttk-text1(7).
          wk_0026-id_carga        = lc_vttk-id_carga.
          wk_0026-id_romaneio     = lc_vttk-id_romaneio.
          wk_0026-shtyp           = lc_vttk-shtyp.
          wk_0026-abfer           = lc_vttk-abfer.
          MODIFY zlest0026 FROM wk_0026.
          EXPORT wk_0026-seq_controle TO MEMORY ID 'ZADTOPED'.
        ENDIF.

      WHEN abap_false.

        CLEAR: zlest0026, wk_0026, zlest0101.
        EXPORT '' TO MEMORY ID 'ZADTOPED'.
        CLEAR: zlest0026, zlest0101.
        vg_pedi_pedagio       = space.
        vg_pedi_informado     = space.
        vg_ck_adiantamento    = abap_false.
        vg_pedi_mesmo_veicul  = abap_false.
        vg_ck_alterou_cidade  = abap_false.

*-#133089-12.02.2024-JT-inicio
        PERFORM f_ajustar_fatur_auto_tela.
*-#133089-12.02.2024-JT-fim

        PERFORM ajusta_tela USING abap_false.

        IF lc_ck_pedagio EQ abap_true OR vg_ck_adiantamento EQ abap_true.

*-#133089-12.02.2024-JT-inicio
          IF vg_faturauto_in IS NOT INITIAL.
            e_fatura_aut = vg_faturauto_out.
            RETURN.
          ELSEIF vg_faturamento_autom = abap_true.
            PERFORM f_tratar_faturamento.
            RETURN.
          ENDIF.
*-#133089-12.02.2024-JT-fim

          CALL SCREEN 100 STARTING AT start_column start_row.

          IF ctl_con_0100 IS NOT INITIAL.
            CALL METHOD ctl_alv_0100->free.
            CALL METHOD ctl_con_0100->free.
            CLEAR: ctl_con_0100.
          ENDIF.

          IF ( NOT vg_pedi_pedagio IS INITIAL ) AND ( vg_pedi_informado IS INITIAL ) AND ( vg_cartao_pedagio EQ abap_true AND zlest0026-ck_credita_ped EQ abap_true ).
            MESSAGE e070(zles) RAISING pedagio.
          ENDIF.
        ELSE.
          IF vg_faturauto_in IS NOT INITIAL.
            e_fatura_aut = vg_faturauto_out.
            RETURN.
          ELSE.
            DATA(lc_gravou) = abap_false.
            PERFORM gravar_registro USING lc_gravou.
          ENDIF.
        ENDIF.

    ENDCASE.

*    ELSEIF ( WA_ZLEST0108 IS INITIAL ) OR ( WA_ZLEST0108-AUTOMATICO EQ ABAP_FALSE ).

*      CLEAR WK_0026.
*
*      " Local de organizaçao de transporte
*      SELECT SINGLE BUKRS INTO VL_BUKRS FROM TTDS WHERE TPLST = WK_XVTTK-TPLST.
*      VG_BRANCH = WK_XVTTK-TPLST.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = VG_BRANCH
*        IMPORTING
*          OUTPUT = VG_BRANCH.
*
*      CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
*        EXPORTING
*          CENTRO               = VG_BRANCH
*        IMPORTING
*          CENTRO_OUT           = VG_BRANCH
*        EXCEPTIONS
*          INFORMAR_CENTRO      = 1
*          NAO_CENTRO_R_VIRTUAL = 2
*          INFORMAR_CENTRO_OUT  = 3
*          INFORMAR_CENTRO_V    = 4
*          OTHERS               = 5.
*
*      IF SY-SUBRC IS NOT INITIAL.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
*      ENDIF.
*
*      SELECT SINGLE CK_PEDAGIO INTO @DATA(LC_CK_PED) FROM ZLEST0027 WHERE ROUTE = @WK_XVTTK-ROUTE.
*
*      "Consulta Situação da Placa
*
*      CALL METHOD ZCL_WEBSERVICE_TIPCARD=>CONS_SITUACAO_TRANSPORTADOR
*        EXPORTING
*          I_PLACA     = WK_XVTTK-TEXT1(7)
*        RECEIVING
*          E_CONSULTAS = DATA(E_CONSULTAS)
*        EXCEPTIONS
*          ERRO        = 1
*          WEBSERVICE  = 2
*          OTHERS      = 3.
*
*      IF SY-SUBRC IS NOT INITIAL.
*        MESSAGE E000(ZLES) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
*      ENDIF.
*
*      READ TABLE E_CONSULTAS INDEX 1 INTO DATA(WA_ZLEST0135).
*      IF WA_ZLEST0135-CK_RNTRC_ATIVO EQ ABAP_FALSE.
*        SY-MSGV1 = WA_ZLEST0135-DS_MSG_TRANSPORTADOR+000(50).
*        SY-MSGV2 = WA_ZLEST0135-DS_MSG_TRANSPORTADOR+050(50).
*        SY-MSGV3 = WA_ZLEST0135-DS_MSG_TRANSPORTADOR+100(50).
*        SY-MSGV4 = WA_ZLEST0135-DS_MSG_TRANSPORTADOR+150(50)..
*        MESSAGE E000(ZLES) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4  RAISING ERRO.
*      ENDIF.
*
*      IF LC_CK_PED EQ ABAP_TRUE.
*
*        "pesuisar valor do pedágio
*        SELECT * FROM ZLEST0002
*          INTO TABLE @DATA(IT_ZLEST0002)
*        WHERE PC_VEICULO IN (@WK_XVTTK-TEXT1(7),@WK_XVTTK-TEXT2(7),@WK_XVTTK-TEXT3(7),@WK_XVTTK-TEXT4(7) ).
*
*        I_VEICULO_EIXOS = 0.
*        LOOP AT IT_ZLEST0002 INTO DATA(WA_ZLEST0002).
*          I_VEICULO_EIXOS = I_VEICULO_EIXOS + WA_ZLEST0002-QT_EIXO.
*        ENDLOOP.
*
*        CREATE OBJECT LC_REPOM_ROTEIRO.
*        LC_REPOM_ROTEIRO->SET_BUKRS( EXPORTING I_BUKRS = VL_BUKRS ).
*        LC_REPOM_ROTEIRO->SET_BRANCH( EXPORTING I_BRANCH = VG_BRANCH ).
*        LC_REPOM_ROTEIRO->SET_ID_ROTA_REPOM( EXPORTING I_ID_ROTA_REPOM = WA_ZLEST0108-ID_ROTA_REPOM ).
*        LC_REPOM_ROTEIRO->SET_ID_PERCURSO_REPOM( EXPORTING I_ID_PERCURSO_REPOM = WA_ZLEST0108-ID_PERCURSO_REPOM ).
*        LC_REPOM_ROTEIRO->SET_VEICULO_EIXOS( EXPORTING I_VEICULO_EIXOS = I_VEICULO_EIXOS ).
*        LC_REPOM_ROTEIRO->SET_QTD_EIXOS_SUSPENSOS_IDA( EXPORTING I_QTD_EIXOS_SUSPENSOS_IDA = 0 ).
*        LC_REPOM_ROTEIRO->SET_QTD_EIXOS_SUSPENSOS_VOLTA( EXPORTING I_QTD_EIXOS_SUSPENSOS_VOLTA = 0 ).
*
*        CALL METHOD LC_REPOM_ROTEIRO->CONSULTAR_VALOR
*          IMPORTING
*            E_ERROS                    = E_ERROS
*          RECEIVING
*            I_RETORNOU                 = DATA(P_RETORNOU)
*          EXCEPTIONS
*            SERVICO_NAO_ENCONTRADO     = 1
*            HTTP_COMMUNICATION_FAILURE = 2
*            HTTP_INVALID_STATE         = 3
*            HTTP_PROCESSING_FAILED     = 4
*            HTTP_INVALID_TIMEOUT       = 5
*            ERRO                       = 6
*            OTHERS                     = 7.
*
*        IF SY-SUBRC IS NOT INITIAL.
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
*        ENDIF.
*
*        IF P_RETORNOU EQ ABAP_FALSE.
*          CLEAR: LC_REPOM_ROTEIRO.
*          LOOP AT E_ERROS INTO DATA(LC_ERRO).
*            MESSAGE E017(ZREPOM) WITH LC_ERRO-ERRO_CODIGO LC_ERRO-ERRO_DESCRICAO RAISING ERRO.
*          ENDLOOP.
*        ENDIF.
*
*        IF LC_REPOM_ROTEIRO->GET_VALOR_TOTAL_VPR( ) GT 2000.
*          CLEAR: LC_REPOM_ROTEIRO.
*          MESSAGE E079(ZLES) RAISING ERRO.
*        ENDIF.
*        CLEAR: LC_REPOM_ROTEIRO.
*
*        TRY.
*            CALL METHOD ZCL_DOC_FISCAL_FT_ENTRADA=>ZIF_DOC_FISCAL_FT_ENTRADA~SET_GERAR_PEDAGIO
*              EXPORTING
*                I_TKNUM             = WA_ZLEST0108-DOC_TRANSP
*                I_NR_CARTAO         = WA_ZLEST0108-NR_CARTAO_REPOM
*                I_ID_ROTA_REPOM     = WA_ZLEST0108-ID_ROTA_REPOM
*                I_ID_PERCURSO_REPOM = WA_ZLEST0108-ID_PERCURSO_REPOM
*              RECEIVING
*                E_ZLEST0123         = DATA(E_ZLEST0123).
*          CATCH ZCX_DOC_FISCAL_FT_ENTRADA INTO DATA(EX_ERRO).
*            EX_ERRO->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
*            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
*        ENDTRY.
*      ENDIF.
*
*      "Calcula Adiantamento
*      IF WK_XVTTK-TEXT1 IS NOT INITIAL.
*        SELECT SINGLE * INTO @DATA(WK_ZLEST0002)
*          FROM ZLEST0002
*         WHERE PC_VEICULO EQ @WK_XVTTK-TEXT1(7).
*
*        IF SY-SUBRC IS INITIAL.
*          SELECT SINGLE MARGADTO INTO VL_MARGEM
*            FROM ZLEST0103
*           WHERE BUKRS  = VL_BUKRS
*             AND BRANCH = VG_BRANCH
*             AND TDLNR  = WK_ZLEST0002-PROPRIETARIO.
*        ENDIF.
*      ELSE.
*        SY-SUBRC = 4.
*      ENDIF.
*
*      IF SY-SUBRC IS NOT INITIAL.
*        SELECT SINGLE MARGADTO INTO VL_MARGEM
*          FROM ZLEST0103
*         WHERE BUKRS  = VL_BUKRS
*           AND BRANCH = VG_BRANCH
*           AND TDLNR  = SPACE.
*      ENDIF.
*
*      IF SY-SUBRC IS INITIAL.
*        WK_0026-ADTO = ( WK_NETWR_ALL * VL_MARGEM ) / 100.
*      ELSE.
*        MESSAGE I031(ZLES) WITH VL_BUKRS VL_TPLST RAISING ERRO.
*      ENDIF.
*
*      CALL FUNCTION 'NUMBER_GET_NEXT'
*        EXPORTING
*          NR_RANGE_NR             = '01'
*          OBJECT                  = 'ZTRP01'
*        IMPORTING
*          NUMBER                  = WK_0026-SEQ_CONTROLE
*        EXCEPTIONS
*          INTERVAL_NOT_FOUND      = 1
*          NUMBER_RANGE_NOT_INTERN = 2
*          OBJECT_NOT_FOUND        = 3
*          QUANTITY_IS_0           = 4
*          QUANTITY_IS_NOT_1       = 5
*          INTERVAL_OVERFLOW       = 6
*          BUFFER_OVERFLOW         = 7
*          OTHERS                  = 8.
*
*      IF SY-SUBRC IS INITIAL.
*        WK_0026-MANDT           = SY-MANDT.
*        WK_0026-TKNUM           = WK_0026-SEQ_CONTROLE.
*        WK_0026-PEDAGIO         = E_ZLEST0123-VLR_TOTAL_PEDAGIO.
*        WK_0026-ERDAT           = SY-DATUM.
*        WK_0026-UZEIT           = SY-UZEIT.
*        WK_0026-UNAME           = SY-UNAME.
*        WK_0026-TP_ADMIM_PED    = '03'.
*        WK_0026-TP_CARD_PED     = SPACE.
*        WK_0026-NR_CARD_PED     = SPACE.
*        WK_0026-ID_ROTA         = SPACE.
*        WK_0026-QTD_EIXO        = I_VEICULO_EIXOS.
*        WK_0026-NR_VR_XML_TIPF  = '1.17'.
*        WK_0026-CK_CREDITA_PED  = ABAP_TRUE.
*        WK_0026-TP_ADMIM_FRETE  = '09'.
*        WK_0026-ID_PROC_CLIENTE = E_ZLEST0123-ID_PROC_CLIENTE.
*        WK_0026-PLACA_CAV       = WK_XVTTK-TEXT1(7).
*        MODIFY ZLEST0026 FROM WK_0026.
*        EXPORT WK_0026-SEQ_CONTROLE TO MEMORY ID 'ZADTOPED'.
*      ENDIF.

  ELSE.
    RAISE erro.
  ENDIF.

ENDFUNCTION.
