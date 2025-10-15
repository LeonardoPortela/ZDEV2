*----------------------------------------------------------------------*
***INCLUDE LZNFE_INBOUNDO04.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& 26/09/2024  |DEVK9A27D3  |NSEGANTIN      |Melhoria ZPP0014 NF       *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_1600  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1600 OUTPUT.

  DATA: it_pf1600 TYPE TABLE OF sy-ucomm.

  IF sb_tela_1600 IS INITIAL.
    sb_tela_1600 = tl_tela_1601.
  ENDIF.

  CLEAR: it_pf1600.

  DATA(lc_nota) = obj_nfe_inbound->get_cabecalho_nota( ).

  DATA(lc_retorno) = obj_nfe_inbound->get_ck_cfop_retorno_armazena( ).

  IF lc_retorno EQ abap_true.
    APPEND ok_arma_fis  TO it_pf1600. "Armazenagem
    APPEND ok_arma_fisp TO it_pf1600. "Armazenagem Parcial
    APPEND ok_arma_est  TO it_pf1600. "Armazenagem Estorno
    APPEND ok_devo_fis  TO it_pf1600. "Devolução
    APPEND ok_devo_fisp TO it_pf1600. "Devolução Parceial
    APPEND ok_devo_est  TO it_pf1600. "Devolução Estorno
  ENDIF.

  IF lc_nota-mblnr_arm IS NOT INITIAL OR lc_nota-mblnr_dev IS NOT INITIAL.
    "Se Existe Devolução ou Armazenagem não faz mais total
    APPEND ok_arma_fis TO it_pf1600.
    APPEND ok_devo_fis TO it_pf1600.
  ENDIF.

  IF lc_nota-mblnr_dev IS INITIAL.
    APPEND ok_devo_est  TO it_pf1600. "Devolução Estorno
  ENDIF.

  IF lc_nota-mblnr_arm IS INITIAL AND lc_nota-docnum_dev IS INITIAL.
    APPEND ok_destinacao TO it_pf1600. "Destinação
  ENDIF.


  IF lc_nota-mjahr IS INITIAL.

    "Se foi saída total de armazenagem
    APPEND ok_arma_fis  TO it_pf1600. "Armazenagem
    APPEND ok_arma_fisp TO it_pf1600. "Armazenagem Parcial
    APPEND ok_arma_est  TO it_pf1600. "Armazenagem Estorno

    APPEND ok_devo_fis  TO it_pf1600. "Devolução
    APPEND ok_devo_fisp TO it_pf1600. "Devolução Parceial
    APPEND ok_devo_est  TO it_pf1600. "Devolução Estorno

  ELSE.

    "Procura se existiu alguma operação com volume total
    SELECT * INTO TABLE @DATA(it_zmmt0115)
      FROM zmmt0115
     WHERE orig_mjahr      EQ @lc_nota-mjahr
       AND orig_mblnr      EQ @lc_nota-mblnr
       AND ck_total_origem EQ @abap_true.

    IF sy-subrc IS INITIAL.

      SELECT * INTO TABLE @DATA(it_zmmt0114)
        FROM zmmt0114 AS m
         FOR ALL ENTRIES IN @it_zmmt0115
       WHERE id_destinacao EQ @it_zmmt0115-id_destinacao
         AND NOT EXISTS ( SELECT * FROM mseg AS e WHERE e~sjahr EQ m~mjahr AND e~smbln EQ m~mblnr ).

      IF sy-subrc IS INITIAL.

        READ TABLE it_zmmt0114 INTO DATA(wa_zmmt0114) INDEX 1.

        CASE wa_zmmt0114-tp_destinacao.
          WHEN zif_material_destinacao=>st_tp_destinacao_armazenar.

            "Se foi saída total de armazenagem
            APPEND ok_arma_fis  TO it_pf1600. "Armazenagem
            APPEND ok_arma_fisp TO it_pf1600. "Armazenagem Parcial
            APPEND ok_devo_fis  TO it_pf1600. "Devolução
            APPEND ok_devo_est  TO it_pf1600. "Devolução Estorno
            APPEND ok_devo_fisp TO it_pf1600. "Devolução Parceial

          WHEN zif_material_destinacao=>st_tp_destinacao_devolucao.

            "Se foi saída total de Devolução
            APPEND ok_arma_fis  TO it_pf1600. "Armazenagem
            APPEND ok_arma_fisp TO it_pf1600. "Armazenagem Parcial
            APPEND ok_arma_est  TO it_pf1600. "Armazenagem Estorno
            APPEND ok_devo_fis  TO it_pf1600. "Devolução
            APPEND ok_devo_fisp TO it_pf1600. "Devolução Parceial

        ENDCASE.

      ENDIF.

    ELSE.
      APPEND ok_arma_est TO it_pf1600. "Devolução Estorno
      APPEND ok_devo_est TO it_pf1600. "Devolução Estorno
    ENDIF.

  ENDIF.

  IF obj_nfe_inbound->get_ck_gerar_saida_armazem( ) EQ abap_false AND lc_nota-mblnr_arm IS INITIAL.
    "Não existe Armazenagem
    APPEND ok_arma_fis  TO it_pf1600.
    APPEND ok_arma_fisp TO it_pf1600.
    APPEND ok_arma_est  TO it_pf1600.
  ELSEIF lc_nota-mblnr_arm IS INITIAL.
    APPEND ok_arma_est TO it_pf1600.
  ENDIF.

  IF wa_nfe_inbound-nfe_base-st_fiscal = zcl_nfe_inbound=>st_fiscal_sem_aceite_fiscal OR "Finalizado Sem Aceite Fiscal
     wa_nfe_inbound-nfe_base-st_fisico = zcl_nfe_inbound=>st_fisico_98.   "Finalizado Sem Aceite Físico
    APPEND ok_fisico_can TO it_pf1600.
    APPEND ok_fisico_act TO it_pf1600.
  ELSE.

    DATA(ck_tem_aviso) = abap_false.

    IF wa_nfe_inbound-nfe_base-ebeln IS NOT INITIAL.
      LOOP AT wa_nfe_inbound-nfe_base-itens INTO wa_itens.
        IF wa_itens-ebeln IS INITIAL OR wa_itens-ebelp IS INITIAL.
          CONTINUE.
        ENDIF.
        TRY .
            DATA(r_bstae) = zcl_pedido_compra=>get_chave_controle_conf_item( EXPORTING i_ebeln = wa_itens-ebeln i_ebelp = wa_itens-ebelp ).
          CATCH zcx_pedido_compra_exception.    "
        ENDTRY.
        IF r_bstae EQ '0004' OR r_bstae EQ '0003'.
          ck_tem_aviso = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF ck_tem_aviso EQ abap_false.
      APPEND ok_aviso_act TO it_pf1600.
      APPEND ok_aviso_can TO it_pf1600.
    ELSE.
      IF wa_nfe_inbound-nfe_base-vbeln IS INITIAL AND lc_nota-vbeln IS INITIAL.
        APPEND ok_aviso_can TO it_pf1600.
      ELSE.
        APPEND ok_aviso_act TO it_pf1600.
      ENDIF.
    ENDIF.

    IF wa_nfe_inbound-nfe_base-ck_fisico EQ abap_true.
      APPEND ok_fisico_act TO it_pf1600.
      APPEND ok_fatura_inf TO it_pf1600.
      APPEND ok_reini_lote TO it_pf1600. "*-CS2025000249-07.05.2025-#174157-JT
    ENDIF.

    IF wa_nfe_inbound-nfe_base-st_fisico EQ zcl_nfe_inbound=>st_fisico_00 OR wa_nfe_inbound-nfe_base-st_fisico EQ zcl_nfe_inbound=>st_fisico_aviso_gerado.
      APPEND ok_fisico_can TO it_pf1600.
    ELSE.
      APPEND ok_fatura_inf TO it_pf1600.
      APPEND ok_fisico_act TO it_pf1600.
      APPEND ok_reini_lote TO it_pf1600. "*-CS2025000249-07.05.2025-#174157-JT
    ENDIF.

  ENDIF.

  SELECT SINGLE vbeln
    FROM zib_nfe_dist_ter
    INTO @DATA(lv_vbeln)
    WHERE chave_nfe = @lc_nota-chave_nfe.
  IF sy-subrc IS NOT INITIAL OR lv_vbeln IS INITIAL.
    APPEND ok_gerar_trasp TO it_pf1600.
  ENDIF.

  SET PF-STATUS 'PF1600' EXCLUDING it_pf1600.
  SET TITLEBAR 'TL1600'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1601  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1601 OUTPUT.

  IF sb_tela_1601 IS INITIAL.
    sb_tela_1601 = tl_tela_1602.
  ENDIF.

  IF ck_alterou_ck_possui_frete EQ abap_true.
    obj_nfe_inbound->set_ck_possui_frete( i_ck_possui_frete = zde_nfe_dist_alv-ck_possui_frete ).
    ck_alterou_ck_possui_frete = abap_false.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name(16) EQ 'ZDE_NFE_DIST_ALV'.
      SPLIT screen-name AT '-' INTO DATA(str1_1601) DATA(str2_1601).
      i_campo = str2_1601.
      IF obj_nfe_inbound->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ) EQ abap_true.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF ( screen-name EQ 'ZDE_NFE_DIST_ALV-CK_POSSUI_FRETE' AND zde_nfe_dist_alv-ck_possui_frete IS NOT INITIAL ) OR
       ( screen-name EQ 'ZDE_NFE_DIST_ALV-F_TRANSPORTE' AND zde_nfe_dist_alv-f_transporte IS NOT INITIAL ) .
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF zde_nfe_dist_alv-ck_possui_frete IS INITIAL.
    CLEAR: zde_nfe_dist_alv-f_transporte.
  ENDIF.

  IF zde_nfe_dist_alv-f_transporte IS INITIAL.
    CLEAR:
    lc_transportador_cnpj,
    lc_transportador_ie,
    lc_transportador_razao,
    lc_transportador_regio.
  ENDIF.

  IF zde_nfe_dist_alv-f_transporte IS NOT INITIAL AND ck_alterou_transportador EQ abap_true.
    DATA(wa_trans_2) = obj_nfe_inbound->set_transportadora( i_f_transporte = zde_nfe_dist_alv-f_transporte ).
    lc_transportador_cnpj  = wa_trans_2-stcd1.
    lc_transportador_ie    = wa_trans_2-stcd3.
    lc_transportador_razao = wa_trans_2-name1.
    lc_transportador_regio = wa_trans_2-regio.
    ck_alterou_transportador = abap_false.
  ELSEIF zde_nfe_dist_alv-f_transporte IS INITIAL AND ck_alterou_transportador EQ abap_true.
    obj_nfe_inbound->set_transportadora( i_f_transporte = zde_nfe_dist_alv-f_transporte ).
    CLEAR:
    lc_transportador_cnpj,
    lc_transportador_ie,
    lc_transportador_razao,
    lc_transportador_regio.
    ck_alterou_transportador = abap_false.
  ELSEIF zde_nfe_dist_alv-f_transporte IS NOT INITIAL AND ck_alterou_transportador EQ abap_false.
    DATA(wa_lfa1_2) = obj_nfe_inbound->get_info_fornecedor( i_lifnr = zde_nfe_dist_alv-f_transporte ).
    lc_transportador_cnpj  = wa_lfa1_2-stcd1.
    lc_transportador_ie    = wa_lfa1_2-stcd3.
    lc_transportador_razao = wa_lfa1_2-name1.
    lc_transportador_regio = wa_lfa1_2-regio.
  ENDIF.

  IF zde_nfe_dist_alv-f_armazem IS NOT INITIAL AND ck_alterou_armazem EQ abap_true.
    DATA(armazem_2) = obj_nfe_inbound->set_armazem( i_f_armazem = zde_nfe_dist_alv-f_armazem ).
    zde_nfe_dist_alv-armazem_cnpj  = armazem_2-stcd1.
    zde_nfe_dist_alv-armazem_ie    = armazem_2-stcd3.
    zde_nfe_dist_alv-armazem_razao = armazem_2-name1.
    lc_armazem_regio               = armazem_2-regio.
    ck_alterou_armazem = abap_false.
  ELSEIF  zde_nfe_dist_alv-f_armazem IS INITIAL AND ck_alterou_armazem EQ abap_true.
    CLEAR:
    zde_nfe_dist_alv-armazem_cnpj,
    zde_nfe_dist_alv-armazem_ie,
    zde_nfe_dist_alv-armazem_razao,
    lc_armazem_regio.
    ck_alterou_armazem = abap_false.
  ENDIF.

  "Criar ALV Itens
  IF container_1601 IS INITIAL.

    CLEAR wa_layout_1601.
    wa_layout_1601-zebra      = abap_true.
    wa_stable_1601-row        = abap_true.
    wa_stable_1601-col        = abap_true.
    wa_layout_1601-stylefname = 'STYLE'.
    wa_layout_1601-sel_mode   = 'A'.
*   wa_layout_1601-cwidth_opt = 'X'.  "*-CS2025000249-08.04.2025-#173180-JT-inicio
*   wa_layout_1601-col_opt    = 'X'.  "*-CS2025000249-08.04.2025-#173180-JT-inicio
    wa_layout_1601-info_fname = 'LINE_COLOR'.
    wa_layout_1601-ctab_fname = 'COLOR_CELL'.
    wa_layout_1601-no_toolbar = abap_true.

    CREATE OBJECT container_1601
      EXPORTING
        container_name = 'ALV_ITENS'.

    PERFORM fill_it_fieldcatalog_1601.

*   Fill info for layout variant
    PERFORM fill_gs_variant_1601.

    CREATE OBJECT cl_grid_1601
      EXPORTING
        i_parent = container_1601.

    cl_grid_1601->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    cl_grid_1601->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_function_1601.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_function_1601.
    APPEND cl_gui_alv_grid=>mc_fc_loc_move_row      TO it_function_1601.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_function_1601.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_function_1601.
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_function_1601.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_function_1601.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_function_1601.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_function_1601.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_1601.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_1601.
    APPEND cl_gui_alv_grid=>mc_fc_check             TO it_function_1601.
    APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_function_1601.

    CREATE OBJECT event_receiver_1601.
    SET HANDLER:  event_receiver_1601->handle_double_click_1601  FOR cl_grid_1601.


    CALL METHOD cl_grid_1601->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_variant_1601
        i_save                        = 'A'
        is_layout                     = wa_layout_1601
        it_toolbar_excluding          = it_function_1601
      CHANGING
        it_outtab                     = it_itens_alv[]
        it_fieldcatalog               = it_fieldcat_1601
        it_sort                       = it_sort_1601
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  CALL METHOD cl_grid_1601->refresh_table_display
    EXPORTING
      is_stable = wa_stable_1601.

  IF lc_fornecedor_regio IS INITIAL.
    SELECT SINGLE regio INTO lc_fornecedor_regio
      FROM lfa1
     WHERE lifnr EQ zde_nfe_dist_alv-p_emissor.
  ENDIF.

  IF lc_destinatario_regio IS INITIAL.
    armazem_2-lifnr = zde_nfe_dist_alv-f_tomadora.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = armazem_2-lifnr
      IMPORTING
        output = armazem_2-lifnr.

    SELECT SINGLE regio INTO lc_fornecedor_regio
      FROM kna1
     WHERE kunnr EQ armazem_2-lifnr.
  ENDIF.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1600 INPUT.

  DATA: e_gerou      TYPE  char01,
        e_mblnr      TYPE  mblnr,
        e_mjahr      TYPE  mjahr,
        e_docnum     TYPE  j_1bdocnum,
        e_mblnr_dev  TYPE  mblnr,
        e_mjahr_dev  TYPE  mjahr,
        e_docnum_dev TYPE  j_1bdocnum,
        e_belnr_dev  TYPE  re_belnr,
        e_gjahr_dev  TYPE  gjahr,
        ls_opt       TYPE ctu_params.

  CLEAR: e_gerou, e_mblnr, e_mjahr, e_docnum, e_mblnr_dev, e_mjahr_dev, e_docnum_dev, e_belnr_dev, e_gjahr_dev.

  CASE ok_code.

    WHEN ok_addtrans.

      CLEAR: ok_code.
      PERFORM add_info_trasnporte.
      PERFORM limpar_variaveis.
      PERFORM get_info_tela.

    WHEN ok_arma_est.

      CLEAR: ok_code.
      TRY .
          obj_nfe_inbound->nfe_inbound_can_saida_armazem( ).
          obj_nfe_inbound->refresh( ).
          PERFORM limpar_variaveis.
          PERFORM get_info_tela.

        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN ok_devo_est.

      CLEAR: ok_code.
      TRY .
          obj_nfe_inbound->nfe_inbound_can_saida_devolu( ).
          obj_nfe_inbound->refresh( ).
          PERFORM limpar_variaveis.
          PERFORM get_info_tela.

        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN ok_arma_fis.

      CLEAR: ok_code.
      TRY .
          obj_nfe_inbound->nfe_inbound_saida_armazenagem( IMPORTING e_zmmt0114 = DATA(e_zmmt0114) ).
          obj_nfe_inbound->refresh( ).
          PERFORM limpar_variaveis.
          PERFORM get_info_tela.
        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.

          IF ex_nfe_inbound->msgid EQ zcx_doc_eletronico=>zcx_nao_autorizado_uso-msgid AND
             ex_nfe_inbound->msgno EQ zcx_doc_eletronico=>zcx_nao_autorizado_uso-msgno AND
             e_zmmt0114-id_destinacao IS NOT INITIAL.

            CALL FUNCTION 'ZMF_DESTINACAO_MERCADORIA'
              EXPORTING
                i_id_destinacao = e_zmmt0114-id_destinacao
                ck_total_origem = abap_false.

            obj_nfe_inbound->refresh( ).
            PERFORM limpar_variaveis.
            PERFORM get_info_tela.

          ELSE.
            ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
          ENDIF.

      ENDTRY.

    WHEN ok_devo_fis.

      CLEAR: ok_code.

*-CS2025000249-19.05.2025-#175013-JT-inicio
      IF obj_nfe_inbound->get_config_tipo_pedido( )-ck_nao_vinc_miro_migo_ped = abap_true.
        MESSAGE s024(sd) WITH 'Devolução nao Permitida! Pedido Bonificação!' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
*-CS2025000249-19.05.2025-#175013-JT-fim

      TRY .
          obj_nfe_inbound->nfe_inbound_saida_devolucao(
            IMPORTING
              e_mblnr_dev  = e_mblnr_dev
              e_mjahr_dev  = e_mjahr_dev
              e_belnr_dev  = e_belnr_dev
              e_gjahr_dev  = e_gjahr_dev
              e_docnum_dev = e_docnum_dev
              e_zmmt0114   = e_zmmt0114
            RECEIVING
              r_gerou      = DATA(r_gerou)
          ).
          obj_nfe_inbound->refresh( ).
          PERFORM limpar_variaveis.
          PERFORM get_info_tela.

        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.

          IF ex_nfe_inbound->msgid EQ zcx_doc_eletronico=>zcx_nao_autorizado_uso-msgid AND
             ex_nfe_inbound->msgno EQ zcx_doc_eletronico=>zcx_nao_autorizado_uso-msgno AND
             e_zmmt0114-id_destinacao IS NOT INITIAL.

            CALL FUNCTION 'ZMF_DESTINACAO_MERCADORIA'
              EXPORTING
                i_id_destinacao = e_zmmt0114-id_destinacao
                ck_total_origem = abap_false.

            obj_nfe_inbound->refresh( ).
            PERFORM limpar_variaveis.
            PERFORM get_info_tela.

          ELSE.
            ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
          ENDIF.

      ENDTRY.

    WHEN ok_aviso_act.

      CLEAR: ok_code.
      obj_nfe_inbound->set_aceitar_fisico( ).
      obj_nfe_inbound->set_ck_gerar_somente_aviso( ).
      TRY.
          IF obj_nfe_inbound->zif_cadastro~gravar_registro( ) EQ abap_true.
*            LEAVE TO SCREEN 0.
          ENDIF.
        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN ok_aviso_can.

      TRY.
          CLEAR: ok_code.
          obj_nfe_inbound->nfe_inbound_cancela_fisico( i_estornar_somente_aviso = abap_true ).
          LEAVE TO SCREEN 0.
        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.    "
          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        CATCH zcx_pedido_compra_exception INTO ex_pedido_compra.    "
          ex_pedido_compra->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN ok_fatura_inf.
      CLEAR: ok_code.
      ck_entrada_estoque = abap_true.
      PERFORM get_info_banco_parceiro.
      PERFORM get_info_valores.
      CALL SCREEN 1700 STARTING AT 50 03.
      ck_entrada_estoque = abap_false.

    WHEN ok_fisico_act.
      CLEAR: ok_code.
      CHECK ck_alterou_armazem EQ abap_false.
      CHECK ck_alterou_lote EQ abap_false.
      CHECK ck_alterou_lote_info EQ abap_false.
      CHECK ck_alterou_ck_possui_frete EQ abap_false.
      CHECK ck_alterou_transportador EQ abap_false.
      CLEAR: ok_code.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      SELECT SINGLE *
        FROM zsdt0410 AS a INTO @DATA(lwa_chave_nfe)
       WHERE chave_nfe EQ @zde_nfe_dist_alv-chave_nfe
         AND cancel    EQ @abap_false
         AND EXISTS ( SELECT nro_cg
                        FROM zsdt0133 AS b
                       WHERE b~nro_cg EQ a~nro_cg
                         AND status   NE 'X' ).

      IF sy-subrc EQ 0.
        _troca_nota = zcl_carga_saida_insumos=>check_carga_troca_nota( EXPORTING i_nro_cg  = lwa_chave_nfe-nro_cg ).
        IF _troca_nota EQ abap_true.
          MESSAGE |Nota vinculada a Carga de Saida { lwa_chave_nfe-nro_cg }! Aceite Fisico só pode ser realizado na transação ZSDT0112!| TYPE 'I'.
          RETURN.
        ENDIF.
      ENDIF.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

      SELECT SINGLE inco1
        FROM ekko
        INTO @DATA(lv_inco)
        WHERE ebeln = @zde_nfe_dist_alv-ebeln.
      IF sy-subrc IS INITIAL.
        IF lv_inco EQ 'FOB'.
          SELECT SINGLE *
            FROM zib_nfe_dist_ter
            INTO @DATA(ls_dist_ter)
            WHERE chave_nfe = @zde_nfe_dist_alv-chave_nfe.
          IF sy-subrc IS NOT INITIAL OR sy-subrc IS INITIAL AND ls_dist_ter-vbeln IS INITIAL.
            MESSAGE 'Necessário gerar aviso, antes do aceite físico!' TYPE 'I' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.

*      TRY .
*          obj_nfe_inbound->check_obligatory_charg( ).
*        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
*          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
*          EXIT.
*      ENDTRY.


* PBI - 64541 - Inicio - CBRAND
      IF zde_nfe_dist_alv-ctr_waers = 'USD'.
        TRY .
            IF obj_nfe_inbound->zif_cadastro~check_env_aprov_taxa( ) EQ abap_true.
              " lva_migo = 'N'.
            ENDIF.
          CATCH zcx_cadastro INTO ex_cadastro.
            ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        ENDTRY.
      ENDIF.
* PBI - 64541 - Fim - CBRAND

      IF obj_nfe_inbound->get_ck_cfop_retorno_armazena( ) EQ abap_true.
        TRY .
            obj_nfe_inbound->zif_cadastro~gravar_registro( ).

            PERFORM limpar_tela_9004.
            CALL SCREEN 9004.
            IF ck_informado_vinc_retorno EQ abap_false.
              PERFORM limpar_tela_9004.
              EXIT.
            ENDIF.
            PERFORM limpar_tela_9004.

          CATCH zcx_cadastro INTO ex_cadastro.
            ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
            EXIT.
        ENDTRY.

      ENDIF.

      TRY.
          obj_nfe_inbound->set_aceitar_fisico( ).
          obj_nfe_inbound->set_ck_miro_automatica( ).  "*-CS2025000249-17.04.2025-#173311-JT
          obj_nfe_inbound->set_aceitar_faturar( i_ck_somente_validar = abap_true ).

          TRY.
              IF obj_nfe_inbound->zif_cadastro~gravar_registro( ) EQ abap_true.
                LEAVE TO SCREEN 0.
              ENDIF.
            CATCH zcx_cadastro INTO ex_cadastro.
              ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
          ENDTRY.

        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        CATCH zcx_pedido_compra_exception INTO ex_pedido_compra.
          ex_pedido_compra->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN ok_fisico_can.
      CLEAR: ok_code.
      CLEAR: ok_code.

      TRY.
          obj_nfe_inbound->nfe_inbound_cancela_fisico( i_nao_estornar_aviso = abap_true ).
          obj_nfe_inbound->refresh( ).
          PERFORM limpar_variaveis.
          PERFORM get_info_tela.

        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN ok_arma_fisp.

      CLEAR: ok_code.
      TRY .
          CALL FUNCTION 'ZMF_DESTINACAO_MERCADORIA'
            EXPORTING
              i_orig_nfe_inbound = zib_nfe_dist_ter-chave_nfe
              i_tp_destinacao    = zif_material_destinacao=>st_tp_destinacao_armazenar
              ck_total_origem    = abap_false
            IMPORTING
              e_gerou            = e_gerou
              e_mblnr            = e_mblnr
              e_mjahr            = e_mjahr
              e_docnum           = e_docnum
              e_mblnr_dev        = e_mblnr_dev
              e_mjahr_dev        = e_mjahr_dev
              e_docnum_dev       = e_docnum_dev
              e_belnr_dev        = e_belnr_dev
              e_gjahr_dev        = e_gjahr_dev.

          obj_nfe_inbound->refresh( ).
          PERFORM limpar_variaveis.
          PERFORM get_info_tela.

        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN ok_devo_fisp.

      CLEAR: ok_code.

*-CS2025000249-19.05.2025-#175013-JT-inicio
      IF obj_nfe_inbound->get_config_tipo_pedido( )-ck_nao_vinc_miro_migo_ped = abap_true.
        MESSAGE s024(sd) WITH 'Devolução nao Permitida! Pedido Bonificação!' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
*-CS2025000249-19.05.2025-#175013-JT-fim

      TRY .
          CALL FUNCTION 'ZMF_DESTINACAO_MERCADORIA'
            EXPORTING
              i_orig_nfe_inbound = zib_nfe_dist_ter-chave_nfe
              i_tp_destinacao    = zif_material_destinacao=>st_tp_destinacao_devolucao
              ck_total_origem    = abap_false
            IMPORTING
              e_gerou            = e_gerou
              e_mblnr            = e_mblnr
              e_mjahr            = e_mjahr
              e_docnum           = e_docnum
              e_mblnr_dev        = e_mblnr_dev
              e_mjahr_dev        = e_mjahr_dev
              e_docnum_dev       = e_docnum_dev
              e_belnr_dev        = e_belnr_dev
              e_gjahr_dev        = e_gjahr_dev.

          obj_nfe_inbound->refresh( ).
          PERFORM limpar_variaveis.
          PERFORM get_info_tela.

        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
*         ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).  "*-CS2025000249-19.05.2025-#175013-JT-inicio
      ENDTRY.

    WHEN ok_destinacao.

      CLEAR: ok_code.

      TRY .

          IF zib_nfe_dist_ter-mblnr_arm IS NOT INITIAL.
            DATA(i_tp_destinacao) = zif_material_destinacao=>st_tp_destinacao_armazenar.
          ELSEIF zib_nfe_dist_ter-docnum_dev IS NOT INITIAL.
            i_tp_destinacao = zif_material_destinacao=>st_tp_destinacao_devolucao.
          ENDIF.

          CALL FUNCTION 'ZMF_DESTINACAO_MERCADORIA'
            EXPORTING
              i_orig_nfe_inbound = zib_nfe_dist_ter-chave_nfe
              i_tp_destinacao    = i_tp_destinacao
              ck_total_origem    = abap_false.

          obj_nfe_inbound->refresh( ).
          PERFORM limpar_variaveis.
          PERFORM get_info_tela.

        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN ok_gerar_trasp.

      PERFORM get_info_tela.

      CLEAR: t_bdcdata. "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP --->>

      PERFORM f_bdcdata USING 'ZLESR0092' '0100' 'X' '' ''.
      PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=SEA'.
      PERFORM f_bdcdata USING '' '' '' 'P_BUKRS-LOW' zib_nfe_dist_ter-bukrs.
      PERFORM f_bdcdata USING '' '' '' 'P_WERKS-LOW' zib_nfe_dist_ter-branch.
      PERFORM f_bdcdata USING '' '' '' 'P_EBELN-LOW' zib_nfe_dist_ter-ebeln.
      PERFORM f_bdcdata USING '' '' '' 'P_XBLNR-LOW' zib_nfe_dist_ter-numero. "*-IR 185421-03.07.2024-#144903-JT
      PERFORM f_bdcdata USING '' '' '' 'R_ATRIB' 'X'.

      ls_opt-racommit = abap_true.
      ls_opt-dismode  = 'E'.
      CALL TRANSACTION 'ZLES0113' USING t_bdcdata OPTIONS FROM ls_opt.

*-CS2025000249-07.05.2025-#174157-JT-inicio
    WHEN ok_reini_lote.
      obj_nfe_inbound->set_reinicia_carac_lote( ).
      obj_nfe_inbound->refresh( ).
      PERFORM limpar_variaveis.
      PERFORM get_info_tela.
*-CS2025000249-07.05.2025-#174157-JT-fim

  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1601
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1601 .

  wa_variant_1601-report      = sy-repid.
  wa_variant_1601-handle      = '1601'.
  wa_variant_1601-log_group   = abap_false.
  wa_variant_1601-username    = abap_false.
  wa_variant_1601-variant     = abap_false.
  wa_variant_1601-text        = abap_false.
  wa_variant_1601-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1601
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1601 .

  DATA: i_contador_2 TYPE lvc_colpos.

  CLEAR: it_fieldcat_1601[], it_fieldcat_1601.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_NFE_DIST_ITM_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcat_1601.

  i_contador_2 = 17.

  LOOP AT it_fieldcat_1601 ASSIGNING FIELD-SYMBOL(<fs_1601>).
    <fs_1601>-edit = abap_false.
    CASE <fs_1601>-fieldname.
      WHEN 'PROD_ITEM'.
        <fs_1601>-col_pos = 1.
      WHEN 'PROD_CODIGO'.
        <fs_1601>-outputlen = 10.
        <fs_1601>-col_pos   = 1.
      WHEN 'PROD_DESCRICAO'.
        <fs_1601>-outputlen = 25. "28. "*-CS2025000249-08.04.2025-#173180-JT-inicio
        <fs_1601>-col_pos   = 2.
      WHEN 'PROD_NCM'.
        <fs_1601>-outputlen = 11. "*-CS2025000249-08.04.2025-#173180-JT-inicio
        <fs_1601>-col_pos = 3.
      WHEN 'PROD_UND_COMERCI'.
        <fs_1601>-outputlen = 7.  "*-CS2025000249-08.04.2025-#173180-JT-inicio
        <fs_1601>-col_pos = 4.
      WHEN 'PROD_QTD_COMERCI'.
        <fs_1601>-outputlen = 11.  "*-CS2025000249-08.04.2025-#173180-JT-inicio
        <fs_1601>-col_pos = 5.
      WHEN 'PROD_VLR_UND_COM'.
        <fs_1601>-outputlen = 14.  "*-CS2025000249-08.04.2025-#173180-JT-inicio
        <fs_1601>-col_pos = 6.
      WHEN 'PROD_VLR_TOTAL_B'.
        <fs_1601>-outputlen = 14.  "*-CS2025000249-08.04.2025-#173180-JT-inicio
        <fs_1601>-col_pos = 7.
      WHEN 'COF_CST'.
        <fs_1601>-outputlen = 05.  "*-CS2025000249-08.04.2025-#173180-JT-inicio
        <fs_1601>-no_out  = abap_true.
        <fs_1601>-style     = cl_gui_alv_grid=>mc_style_button.
        <fs_1601>-scrtext_l = 'COFINS'.
        <fs_1601>-scrtext_m = 'CO'.
        <fs_1601>-scrtext_s = 'CO'.
        <fs_1601>-col_pos   = 8.
      WHEN 'PIS_CST'.
        <fs_1601>-no_out    = abap_true.
        <fs_1601>-style     = cl_gui_alv_grid=>mc_style_button.
        <fs_1601>-scrtext_l = 'PIS'.
        <fs_1601>-scrtext_m = 'PI'.
        <fs_1601>-scrtext_s = 'PI'.
        <fs_1601>-col_pos   = 9.
      WHEN 'IPI_CST'.
        <fs_1601>-no_out    = abap_true.
        <fs_1601>-style     = cl_gui_alv_grid=>mc_style_button.
        <fs_1601>-scrtext_l = 'IPI'.
        <fs_1601>-scrtext_m = 'IP'.
        <fs_1601>-scrtext_s = 'IP'.
        <fs_1601>-col_pos   = 10.
      WHEN 'ICMS_CST'.
        <fs_1601>-style     = cl_gui_alv_grid=>mc_style_button.
        <fs_1601>-outputlen = 06.  "*-CS2025000249-08.04.2025-#173180-JT-inicio
        <fs_1601>-scrtext_l = 'ICMS'.
        <fs_1601>-scrtext_m = 'IC'.
        <fs_1601>-scrtext_s = 'IC'.
        <fs_1601>-col_pos   = 11.
      WHEN 'MATNR'.
        i_campo = <fs_1601>-fieldname.
        <fs_1601>-outputlen = 08.  "*-CS2025000249-08.04.2025-#173180-JT-inicio
        <fs_1601>-edit  = obj_nfe_inbound->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ).
        <fs_1601>-col_pos = 12.
      WHEN 'MEINS'.
        i_campo = <fs_1601>-fieldname.
        <fs_1601>-edit  = obj_nfe_inbound->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ).
        <fs_1601>-col_pos = 13.
      WHEN 'MENGE'.
        i_campo = <fs_1601>-fieldname.
        <fs_1601>-outputlen = 10.  "*-CS2025000249-08.04.2025-#173180-JT-inicio
        <fs_1601>-edit    = abap_false.
        <fs_1601>-col_pos = 14.
      WHEN 'EBELN'.
        i_campo = <fs_1601>-fieldname.
        <fs_1601>-edit    = abap_false.
        <fs_1601>-col_pos = 15.
      WHEN 'EBELP'.
        i_campo = <fs_1601>-fieldname.
        <fs_1601>-edit    = abap_false.
        <fs_1601>-col_pos = 16.
      WHEN OTHERS.
        <fs_1601>-no_out  = abap_true.
        <fs_1601>-col_pos = i_contador_2.
        ADD 1 TO i_contador_2.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK_1601
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM double_click_1601  USING    p_e_row     TYPE lvc_s_row
                                 p_e_column  TYPE lvc_s_col
                                 p_es_row_no TYPE lvc_s_roid.

  IF p_e_row-rowtype IS INITIAL.

    READ TABLE it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX p_e_row-index.

    LOOP AT it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_limpar>) WHERE prod_item NE <fs_item>-prod_item.
      CLEAR: <fs_limpar>-line_color.
    ENDLOOP.

    READ TABLE it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_item2>) INDEX p_e_row-index.
    IF ( sy-subrc IS INITIAL ) AND ( <fs_item2>-line_color NE cs_line_color_selecionada ).
      <fs_item2>-line_color = cs_line_color_selecionada.
      PERFORM setar_lotes_linha USING <fs_item2>.
      "LEAVE TO SCREEN 1600.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK_1601
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM double_click_1603  USING    p_e_row     TYPE lvc_s_row
                                 p_e_column  TYPE lvc_s_col
                                 p_es_row_no TYPE lvc_s_roid.
  IF p_e_row-rowtype IS INITIAL.
    READ TABLE it_lotes_alv_u INDEX p_e_row-index INTO DATA(wa_lotes_alv_u).
    PERFORM selecionar_lote USING wa_itens_sel_lote-prod_item wa_lotes_alv_u-cd_lote_item abap_true.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SETAR_LOTES_LINHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ITEM>  text
*----------------------------------------------------------------------*
FORM setar_lotes_linha  USING  p_item TYPE ty_itens_alv.

  MOVE p_item TO wa_itens_sel_lote.

  "Seleciona o primeiro lote do item
  READ TABLE it_lotes_alv_t WITH KEY chave_nfe = p_item-chave_nfe
                                     prod_item = p_item-prod_item
                          INTO DATA(wa_lotes_alv).
  IF sy-subrc IS INITIAL.
    PERFORM selecionar_lote USING p_item-prod_item wa_lotes_alv-cd_lote_item abap_true.
  ELSE.
    "Não tem um Lote

    PERFORM incluir_lote.

*    TRY.
*        DATA(R_LOTE) = OBJ_NFE_INBOUND->ADD_LOTE_ITEM( EXPORTING I_PROD_ITEM = P_ITEM-PROD_ITEM
*                                                       IMPORTING E_CARACTERISTICAS = E_CARACTERISTICAS ).
*        "Alimenta Tabelas com dados base
*        CLEAR: WA_LOTES_ALV.
*        MOVE-CORRESPONDING R_LOTE TO WA_LOTES_ALV.
*        APPEND WA_LOTES_ALV TO IT_LOTES_ALV.
*
*        LOOP AT E_CARACTERISTICAS INTO DATA(WA_CARACTERISTICAS) WHERE CD_LOTE_ITEM EQ R_LOTE-CD_LOTE_ITEM.
*          CLEAR: WA_CARAC_ALV.
*          MOVE-CORRESPONDING WA_CARACTERISTICAS TO WA_CARAC_ALV.
*          APPEND WA_CARAC_ALV TO IT_CARAC_ALV.
*        ENDLOOP.
*
*        PERFORM SELECIONAR_LOTE USING 1 ABAP_FALSE.
*
*      CATCH ZCX_NFE_INBOUND_EXCEPTION INTO EX_NFE_INBOUND.
*        EX_NFE_INBOUND->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E').
*        EXIT.
*      CATCH ZCX_CHARG_EXCEPTION INTO EX_CHARG.
*        EX_CHARG->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E').
*        EXIT.
*      CATCH ZCX_CADASTRO INTO EX_CADASTRO.
*        EX_CADASTRO->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E').
*        EXIT.
*    ENDTRY.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1602  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1602 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
  DATA: it_lote_caract TYPE zib_nfe_dist_lca_t,
        wa_lote_caract TYPE zib_nfe_dist_lca.

  CLEAR: it_lote_caract, wa_lote_caract.

  IF ck_alterou_lote_info EQ abap_true OR ck_alterou_lote EQ abap_true.
    CLEAR: it_lote_caract.
    LOOP AT it_carac_alv_u.
      MOVE-CORRESPONDING it_carac_alv_u TO wa_lote_caract.
      APPEND wa_lote_caract TO it_lote_caract.
    ENDLOOP.

    obj_nfe_inbound->set_lote_item( CHANGING i_lote = zib_nfe_dist_lot i_lote_caract = it_lote_caract ).

    READ TABLE it_lotes_alv_u ASSIGNING FIELD-SYMBOL(<fs_lote>) WITH KEY cd_lote_item = zib_nfe_dist_lot-cd_lote_item.
    IF sy-subrc = 0.   "*-CS2025000249-07.05.2025-#174157-JT
      MOVE-CORRESPONDING zib_nfe_dist_lot TO <fs_lote>.
    ENDIF.

    READ TABLE it_lotes_alv_t ASSIGNING FIELD-SYMBOL(<fs_loteu>) WITH KEY cd_lote_item = zib_nfe_dist_lot-cd_lote_item.
    IF sy-subrc = 0.   "*-CS2025000249-07.05.2025-#174157-JT
      MOVE-CORRESPONDING zib_nfe_dist_lot TO <fs_loteu>.
    ENDIF.

    CLEAR: it_carac_alv_u[].

    LOOP AT it_lote_caract INTO DATA(rt_metodo).
      MOVE-CORRESPONDING rt_metodo TO it_carac_alv_u.
      READ TABLE it_carac_alv ASSIGNING FIELD-SYMBOL(<fs_carac>) WITH KEY cd_lote_item = rt_metodo-cd_lote_item
                                                                          atinn        = rt_metodo-atinn.
      CHECK sy-subrc = 0.   "*-CS2025000249-07.05.2025-#174157-JT
      MOVE-CORRESPONDING rt_metodo TO <fs_carac>.
      APPEND it_carac_alv_u.
    ENDLOOP.
    ck_alterou_lote_info = abap_false.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name(16) EQ 'ZIB_NFE_DIST_LOT'.
      SPLIT screen-name AT '-' INTO DATA(str1_1602) DATA(str2_1602).
      i_campo = str2_1602.
*-CS2025000249-07.05.2025-#174157-JT-inicio
      IF obj_nfe_inbound->valida_atributo_alteravel_lote( i_campo        = i_campo
                                                          i_prod_item    = wa_itens_sel_lote-prod_item
                                                          i_cd_lote_item = zib_nfe_dist_lot-cd_lote_item   "*-CS2025000249-07.05.2025-#174157-JT
                                                          i_licha        = zib_nfe_dist_lot-licha          "*-CS2025000249-07.05.2025-#174157-JT
                                                          i_lote         = zib_nfe_dist_lot-charg ) EQ abap_true.
*-CS2025000249-07.05.2025-#174157-JT-fim
        screen-input = 1.
**<<<------"138699 - NMS - INI------>>>
* Validação do campo classe para determinar obrigatoriedade na Data de Vencimento.
        IF i_campo                EQ 'VFDAT' AND
           zib_nfe_dist_lot-class EQ 'DEFENSIVOS'.
          screen-required = 1. "Marcação de Obrigatório com Validação Standard.

        ENDIF.
**<<<------"138699 - NMS - FIM------>>>
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF ck_alterou_lote EQ abap_true.

    IF cl_grid_1602 IS NOT INITIAL.
      cl_grid_1602->free( ).
    ENDIF.
    CLEAR: cl_grid_1602.

    IF cl_grid_1603 IS NOT INITIAL.
      cl_grid_1603->free( ).
    ENDIF.
    CLEAR: cl_grid_1603.

    IF container_1602 IS NOT INITIAL.
      container_1602->free( ).
    ENDIF.
    CLEAR: container_1602.

    IF container_1603 IS NOT INITIAL.
      container_1603->free( ).
    ENDIF.
    CLEAR: container_1603.

    ck_alterou_lote = abap_false.
  ENDIF.

  "Lotes
  IF container_1603 IS INITIAL.
    CLEAR wa_layout_1603.
    wa_layout_1603-zebra      = abap_true.
    wa_stable_1603-row        = abap_true.
    wa_stable_1603-col        = abap_true.
    wa_layout_1603-stylefname = 'STYLE'.
    wa_layout_1603-sel_mode   = 'A'.
*   WA_LAYOUT_1603-CWIDTH_OPT = 'X'.
*   wa_layout_1603-col_opt    = 'X'.   "*-CS2025000249-08.04.2025-#173180-JT-inicio
    wa_layout_1603-info_fname = 'LINE_COLOR'.
    wa_layout_1603-ctab_fname = 'COLOR_CELL'.
    wa_layout_1603-no_toolbar = abap_false.

    IF zib_nfe_dist_ter-st_fisico NE zcl_nfe_inbound=>st_fisico_00.
      wa_layout_1603-no_toolbar = abap_true.
    ENDIF.

    CREATE OBJECT container_1603
      EXPORTING
        container_name = 'ALV_LOTES'.

    PERFORM fill_it_fieldcatalog_1603.

*   Fill info for layout variant
    PERFORM fill_gs_variant_1603.

    CREATE OBJECT cl_grid_1603
      EXPORTING
        i_parent = container_1603.

    CREATE OBJECT obg_toolbar_1603
      EXPORTING
        io_alv_grid = cl_grid_1603.

    SET HANDLER obg_toolbar_1603->on_toolbar FOR cl_grid_1603.
    SET HANDLER obg_toolbar_1603->handle_user_command FOR cl_grid_1603.

    CREATE OBJECT event_receiver_1603.
    SET HANDLER: event_receiver_1603->handle_double_click_1603  FOR cl_grid_1603,
                 event_receiver_1603->handle_hotspot_click_1603 FOR cl_grid_1603.

    CALL METHOD cl_grid_1603->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_variant_1603
        i_save                        = 'A'
        is_layout                     = wa_layout_1603
        it_toolbar_excluding          = it_function_1603
      CHANGING
        it_outtab                     = it_lotes_alv_u[]
        it_fieldcatalog               = it_fieldcat_1603
        it_sort                       = it_sort_1603
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  "Características
  IF container_1602 IS INITIAL.
    CLEAR wa_layout_1602.
    wa_layout_1602-zebra      = abap_true.
    wa_stable_1602-row        = abap_true.
    wa_stable_1602-col        = abap_true.
    wa_layout_1602-stylefname = 'STYLE'.
    wa_layout_1602-sel_mode   = 'A'.
    wa_layout_1602-cwidth_opt = abap_off.  "'X'. "*-CS2025000249-08.04.2025-#173180-JT
    wa_layout_1602-col_opt    = abap_off.  "'X'. "*-CS2025000249-08.04.2025-#173180-JT
    wa_layout_1602-info_fname = 'LINE_COLOR'.
    wa_layout_1602-ctab_fname = 'COLOR_CELL'.
    wa_layout_1602-no_toolbar = abap_true.
    wa_layout_1602-no_f4      = abap_false.      "*-CS2025000249-08.04.2025-#173180-JT

    CREATE OBJECT container_1602
      EXPORTING
        container_name = 'ALV_CARACTERISTICAS'.

    PERFORM fill_it_fieldcatalog_1602.

*   Fill info for layout variant
    PERFORM fill_gs_variant_1602.

    CREATE OBJECT cl_grid_1602
      EXPORTING
        i_parent = container_1602.

    cl_grid_1602->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    cl_grid_1602->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_move_row      TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_check             TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_function_1602.

*-CS2025000249-08.04.2025-#173180-JT-inicio
    wa_f4_1602-fieldname = 'ATWRT'.
    wa_f4_1602-register  = 'X'.
    wa_f4_1602-getbefore = 'X'.
    INSERT wa_f4_1602 INTO TABLE it_f4_1602.
*-CS2025000249-08.04.2025-#173180-JT-fim

    CREATE OBJECT event_receiver_1602.
    SET HANDLER event_receiver_1602->handle_data_changed_1602 FOR cl_grid_1602.
    SET HANDLER event_receiver_1602->on_f4_1602               FOR cl_grid_1602.    "*-CS2025000249-08.04.2025-#173180-JT

    CALL METHOD cl_grid_1602->register_f4_for_fields EXPORTING it_f4 = it_f4_1602. "*-CS2025000249-08.04.2025-#173180-JT

    CALL METHOD cl_grid_1602->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_variant_1602
        i_save                        = 'A'
        is_layout                     = wa_layout_1602
        it_toolbar_excluding          = it_function_1602
      CHANGING
        it_outtab                     = it_carac_alv_u[]
        it_fieldcatalog               = it_fieldcat_1602
        it_sort                       = it_sort_1602
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  CALL METHOD cl_grid_1603->refresh_table_display
    EXPORTING
      is_stable = wa_stable_1603.

  CALL METHOD cl_grid_1602->refresh_table_display
    EXPORTING
      is_stable = wa_stable_1602.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1602
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1602 .

  wa_variant_1602-report      = sy-repid.
  wa_variant_1602-handle      = '1602'.
  wa_variant_1602-log_group   = abap_false.
  wa_variant_1602-username    = abap_false.
  wa_variant_1602-variant     = abap_false.
  wa_variant_1602-text        = abap_false.
  wa_variant_1602-dependvars  = abap_false.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1603
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1603 .

  wa_variant_1603-report      = sy-repid.
  wa_variant_1603-handle      = '1603'.
  wa_variant_1603-log_group   = abap_false.
  wa_variant_1603-username    = abap_false.
  wa_variant_1603-variant     = abap_false.
  wa_variant_1603-text        = abap_false.
  wa_variant_1603-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1602
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1602 .

  DATA: i_contador_2 TYPE lvc_colpos,
        i_campo	     TYPE name_feld,
        i_prod_item	 TYPE j_1bitmnum,
        i_lote       TYPE charg_d.

  CLEAR: it_fieldcat_1602[], it_fieldcat_1602.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZIB_NFE_DIST_LCA'
    CHANGING
      ct_fieldcat      = it_fieldcat_1602.

  i_contador_2 = 3.
  i_campo     = 'VFDAT'.
  i_prod_item = wa_itens_sel_lote-prod_item.
  i_lote      = zib_nfe_dist_lot-charg.

  DATA(ck_altera) = obj_nfe_inbound->valida_atributo_alteravel_lote( i_campo = i_campo i_prod_item = wa_itens_sel_lote-prod_item  i_lote = zib_nfe_dist_lot-charg ).

  LOOP AT it_fieldcat_1602 ASSIGNING FIELD-SYMBOL(<fs_1602>).
    <fs_1602>-edit = abap_false.
    CASE <fs_1602>-fieldname.
      WHEN 'SMBEZ'.
        <fs_1602>-col_pos   = 1.
        <fs_1602>-outputlen = 20.
      WHEN 'ATWRT'.
        <fs_1602>-edit      = ck_altera.
        <fs_1602>-outputlen = 35.
        <fs_1602>-col_pos   = 2.
*       <fs_1602>-outputlen = 30.                            "*-CS2025000249-08.04.2025-#173180-JT
        <fs_1602>-domname   = abap_off.                      "*-CS2025000249-08.04.2025-#173180-JT
        <fs_1602>-ref_table = abap_off.                      "*-CS2025000249-08.04.2025-#173180-JT
        <fs_1602>-style     = cl_gui_alv_grid=>mc_style_f4.  "*-CS2025000249-08.04.2025-#173180-JT
      WHEN OTHERS.
        <fs_1602>-no_out  = abap_true.
        <fs_1602>-col_pos = i_contador_2.
        ADD 1 TO i_contador_2.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1602
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1603 .

  DATA: i_contador_2 TYPE lvc_colpos,
        wa_coluna    TYPE lvc_s_fcat.

  CLEAR: it_fieldcat_1603[], it_fieldcat_1603.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZIB_NFE_DIST_LOT'
    CHANGING
      ct_fieldcat      = it_fieldcat_1603.

  wa_coluna-fieldname = 'INDEA'.
  APPEND wa_coluna TO it_fieldcat_1603.

  i_contador_2 = 3.

  LOOP AT it_fieldcat_1603 ASSIGNING FIELD-SYMBOL(<fs_1603>).
    <fs_1603>-edit = abap_false.
    CASE <fs_1603>-fieldname.
      WHEN 'CHARG'.
        <fs_1603>-col_pos   = 1.
        <fs_1603>-outputlen = 30. "20.  "*-CS2025000249-08.04.2025-#173180-JT-inicio
      WHEN 'PROD_QTD_COMERCI'.
        <fs_1603>-edit      = abap_false.
        <fs_1603>-col_pos   = 2.
        <fs_1603>-outputlen = 30.
      WHEN 'INDEA'.
*        <FS_1603>-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
        <fs_1603>-scrtext_l = 'Indea'.
        <fs_1603>-scrtext_m = 'Indea'.
        <fs_1603>-scrtext_s = 'Indea'.
        <fs_1603>-icon      = abap_true.
        <fs_1603>-hotspot   = abap_true.
        <fs_1603>-just      = ' '.
        <fs_1603>-col_pos   = 23.
        <fs_1603>-outputlen = 06.
      WHEN OTHERS.
        <fs_1603>-no_out  = abap_true.
        <fs_1603>-col_pos = i_contador_2.
        ADD 1 TO i_contador_2.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1600_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1600_exit INPUT.

*  DATA: l_chave_e      TYPE char25. " RIM CS1029457 ANB 30.09.2022
  DATA: l_chave_e       TYPE char29. " RIM CS1029457 ANB 30.09.2022

*-IR063194 - 22.07.2021 - JT - inicio
  SELECT * FROM zsdt0082
           INTO TABLE @DATA(t_0082)
          WHERE nro_sol   = @ck_nrosol
            AND seq       = @ck_seq.

  DELETE t_0082 WHERE NOT ( dt_canc  IS     INITIAL
                      AND   dt_liber IS NOT INITIAL ).

  READ TABLE t_0082 INTO DATA(w_0082) INDEX 1.

  IF sy-subrc = 0.
    "//Dequeue register;
    CONCATENATE w_0082-nro_sol
                w_0082-seq
                w_0082-vbeln
                w_0082-posnr INTO l_chave_e.

    CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        chave = l_chave_e.
  ENDIF.
*-IR063194 - 22.07.2021 - JT - fim

  obj_nfe_inbound->desbloquear_objeto( ).  "*-CS2025000249-19.05.2025-#175013-JT

  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA_1602
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM atualiza_tela_1602 .

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

  gs_alv_refres_cond-row = abap_true.
  gs_alv_refres_cond-col = abap_true.

  IF cl_grid_1601 IS NOT INITIAL.
    CALL METHOD cl_grid_1601->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = abap_true.
  ENDIF.

  IF cl_grid_1602 IS NOT INITIAL.
    CALL METHOD cl_grid_1602->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = abap_true.
  ENDIF.

  IF cl_grid_1603 IS NOT INITIAL.
    CALL METHOD cl_grid_1603->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = abap_true.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " ATUALIZA_TELA

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_lote INPUT.
  ck_alterou_lote = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_LOTE_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_lote_info INPUT.
  ck_alterou_lote_info = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_1602
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_changed_1602  USING  e_onf4           TYPE char01
                               e_onf4_after     TYPE char01
                               e_onf4_before    TYPE char01
                               e_ucomm          TYPE sy-ucomm
                               er_data_changed  TYPE REF TO cl_alv_changed_data_protocol.

  DATA: lc_atwrt  TYPE atwrt,
        i_caract  TYPE zib_nfe_dist_lca_t,
        wa_caract TYPE zib_nfe_dist_lca.

  CLEAR: i_caract.

  LOOP AT er_data_changed->mt_good_cells INTO DATA(ls_mod_cells).

    READ TABLE it_carac_alv_u ASSIGNING FIELD-SYMBOL(<fs_caracu>) INDEX ls_mod_cells-row_id.
    READ TABLE it_carac_alv   ASSIGNING FIELD-SYMBOL(<fs_carac>) WITH KEY cd_lote_item = <fs_caracu>-cd_lote_item
                                                                          atinn        = <fs_caracu>-atinn.
    CASE ls_mod_cells-fieldname.
      WHEN  'ATWRT'.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = lc_atwrt.

        IF lc_atwrt NE <fs_caracu>-atwrt.
          <fs_caracu>-atwrt = lc_atwrt.
          <fs_carac>-atwrt  = lc_atwrt.
          MOVE-CORRESPONDING <fs_carac> TO wa_caract.
          APPEND wa_caract TO i_caract.
        ENDIF.
    ENDCASE.

    IF i_caract IS NOT INITIAL.
      obj_nfe_inbound->set_lote_item( CHANGING i_lote = zib_nfe_dist_lot i_lote_caract = i_caract ).
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incluir_item .
*
*071  Deve ser Selecionado um Item "BASE"!
*072  Deve ser Selecionado um Item "FAKE"!

  IF it_itens_alv_sel[] IS INITIAL.
    MESSAGE s071.
    EXIT.
  ENDIF.

  READ TABLE it_itens_alv_sel INDEX 1.

  "Informar Quantidade do Novo Item
  cl_informado_new_item = abap_false.
  MOVE-CORRESPONDING it_itens_alv_sel TO zde_nfe_dist_itm_alv.
  CALL SCREEN 9001 STARTING AT 02 02.
  "Se informado Quantidade
  CHECK cl_informado_new_item EQ abap_true.

  TRY.
      obj_nfe_inbound->add_item( i_prod_item_base = it_itens_alv_sel-prod_item  i_prod_qtd_comerci = zde_nfe_dist_itm_alv-prod_qtd_comerci ).

      CLEAR: it_itens, it_lotes, it_lotes_c, it_itens_alv[], it_itens_alv, it_lotes_alv_t, it_lotes_alv_t[], it_lotes_c, it_lotes_c[].
      DATA(lc_info_nfe) = obj_nfe_inbound->get_info_nota( ).
      MOVE-CORRESPONDING lc_info_nfe-nfe_base TO zib_nfe_dist_ter.
      MOVE-CORRESPONDING lc_info_nfe-nfe_alv  TO zde_nfe_inbound_alv.
      MOVE-CORRESPONDING lc_info_nfe-nfe_alv  TO zde_nfe_dist_alv.
      MOVE lc_info_nfe-nfe_base-itens   TO it_itens.
      MOVE lc_info_nfe-nfe_base-lotes   TO it_lotes.
      MOVE lc_info_nfe-nfe_base-lotes_c TO it_lotes_c.

      LOOP AT it_itens INTO DATA(wa_itens).
        MOVE-CORRESPONDING wa_itens TO it_itens_alv.
        APPEND it_itens_alv.
      ENDLOOP.

      LOOP AT it_lotes INTO DATA(wa_lotes).
        MOVE-CORRESPONDING wa_lotes TO it_lotes_alv_t.
        APPEND it_lotes_alv_t.
      ENDLOOP.

      LOOP AT it_lotes_c INTO DATA(wa_lotes_c).
        MOVE-CORRESPONDING wa_lotes_c TO it_carac_alv.
        APPEND it_carac_alv.
      ENDLOOP.

      LEAVE TO SCREEN 1500.

    CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ex_nfe_inbound->published_erro( i_msgty = 'S'  i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_item .

*071  Deve ser Selecionado um Item "BASE"!
*072  Deve ser Selecionado um Item "FAKE"!

  IF it_itens_alv_sel[] IS INITIAL.
    MESSAGE s072.
    EXIT.
  ENDIF.

  READ TABLE it_itens_alv_sel INDEX 1.

  TRY.
      obj_nfe_inbound->excluir_item( i_prod_item_base = it_itens_alv_sel-prod_item ).

      CLEAR: it_itens, it_lotes, it_lotes_c, it_itens_alv[], it_itens_alv, it_lotes_alv_t, it_lotes_alv_t[], it_lotes_c, it_lotes_c[],
             it_carac_alv_u[], zib_nfe_dist_lot, it_carac_alv_u.

      DATA(lc_info_nfe) = obj_nfe_inbound->get_info_nota( ).
      MOVE-CORRESPONDING lc_info_nfe-nfe_base TO zib_nfe_dist_ter.
      MOVE-CORRESPONDING lc_info_nfe-nfe_alv  TO zde_nfe_inbound_alv.
      MOVE-CORRESPONDING lc_info_nfe-nfe_alv  TO zde_nfe_dist_alv.
      MOVE lc_info_nfe-nfe_base-itens   TO it_itens.
      MOVE lc_info_nfe-nfe_base-lotes   TO it_lotes.
      MOVE lc_info_nfe-nfe_base-lotes_c TO it_lotes_c.

      LOOP AT it_itens INTO DATA(wa_itens).
        MOVE-CORRESPONDING wa_itens TO it_itens_alv.
        APPEND it_itens_alv.
      ENDLOOP.

      LOOP AT it_lotes INTO DATA(wa_lotes).
        MOVE-CORRESPONDING wa_lotes TO it_lotes_alv_t.
        APPEND it_lotes_alv_t.
      ENDLOOP.

      LOOP AT it_lotes_c INTO DATA(wa_lotes_c).
        MOVE-CORRESPONDING wa_lotes_c TO it_carac_alv.
        APPEND it_carac_alv.
      ENDLOOP.

      LEAVE TO SCREEN 1500.

    CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ex_nfe_inbound->published_erro( i_msgty = 'S'  i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_E_ROW_INDEX  text
*      -->P_1002   text
*----------------------------------------------------------------------*
FORM selecionar_lote  USING p_prod_item	    TYPE j_1bitmnum
                            p_cd_lote_item  TYPE zde_cd_lote_item
                            p_atualiza_tela TYPE char01.

  CLEAR: it_carac_alv_u[], zib_nfe_dist_lot, it_carac_alv_u, it_lotes_alv_u, it_lotes_alv_u[].

  LOOP AT it_lotes_alv_t WHERE prod_item EQ p_prod_item.
    it_lotes_alv_t-indea = icon_display_more.
    APPEND it_lotes_alv_t TO it_lotes_alv_u.
  ENDLOOP.

  READ TABLE it_lotes_alv_u ASSIGNING FIELD-SYMBOL(<fs_lotes>) WITH KEY cd_lote_item = p_cd_lote_item.
  CHECK sy-subrc IS INITIAL.

  LOOP AT it_lotes_alv_u ASSIGNING FIELD-SYMBOL(<fs_limpar>) WHERE prod_item NE p_prod_item.
    CLEAR: <fs_limpar>-line_color.
  ENDLOOP.

  IF <fs_lotes>-line_color NE cs_line_color_selecionada.

    <fs_lotes>-line_color = cs_line_color_selecionada.

    "Tem um Lote
    MOVE-CORRESPONDING <fs_lotes> TO zib_nfe_dist_lot.

    LOOP AT it_carac_alv INTO DATA(wa_carac_alv) WHERE cd_lote_item = zib_nfe_dist_lot-cd_lote_item.

*-CS2025000249-08.04.2025-#173180-JT-inicio
      IF wa_carac_alv-atinn = '0000000841' OR  "Safra
         wa_carac_alv-atinn = '0000000835' OR  "MARCA
         wa_carac_alv-atinn = '0000036992'.    "CLASSE
        CONTINUE.
      ENDIF.
*-CS2025000249-08.04.2025-#173180-JT-fim

      APPEND wa_carac_alv TO it_carac_alv_u.
    ENDLOOP.

  ENDIF.

  PERFORM atualiza_tela_1602.

  IF p_atualiza_tela EQ abap_true.
    LEAVE TO SCREEN 1600.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incluir_lote .

  DATA: e_caracteristicas	TYPE zib_nfe_dist_lca_t,
        wa_carac_alv      TYPE ty_itens_carac_alv,
        wa_lotes_alv      TYPE ty_itens_lotes_alv,
        p_index           TYPE lvc_index.

  IF wa_itens_sel_lote IS INITIAL.
    MESSAGE s083.
    EXIT.
  ENDIF.

  CHECK zib_nfe_dist_ter-st_fisico EQ zcl_nfe_inbound=>st_fisico_00.

  TRY.
      DATA(r_lote) = obj_nfe_inbound->add_lote_item( EXPORTING i_prod_item       = wa_itens_sel_lote-prod_item
                                                     IMPORTING e_caracteristicas = e_caracteristicas ).
      "Alimenta Tabelas com dados base
      CLEAR: wa_lotes_alv.
      MOVE-CORRESPONDING r_lote TO wa_lotes_alv.
      APPEND wa_lotes_alv TO it_lotes_alv_t.

      LOOP AT e_caracteristicas INTO DATA(wa_caracteristicas) WHERE cd_lote_item EQ r_lote-cd_lote_item.
        CLEAR: wa_carac_alv.
        MOVE-CORRESPONDING wa_caracteristicas TO wa_carac_alv.
        APPEND wa_carac_alv TO it_carac_alv.
      ENDLOOP.

      READ TABLE it_lotes_alv_t WITH KEY cd_lote_item = wa_lotes_alv-cd_lote_item.
      MOVE sy-tabix TO p_index.
      PERFORM selecionar_lote USING wa_itens_sel_lote-prod_item r_lote-cd_lote_item abap_true.

    CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E').
      EXIT.
    CATCH zcx_charg_exception INTO ex_charg.
      ex_charg->published_erro( i_msgty = 'S' i_msgty_display = 'E').
      EXIT.
    CATCH zcx_cadastro INTO ex_cadastro.
      ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E').
      EXIT.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_lote .

  DATA: p_index  TYPE lvc_index.

  IF it_lotes_alv_sel[] IS INITIAL.
    MESSAGE s082.
    EXIT.
  ENDIF.

  LOOP AT it_lotes_alv_sel.
    READ TABLE it_itens_alv INTO DATA(wa_item_lote) WITH KEY chave_nfe = it_lotes_alv_sel-chave_nfe
                                                             prod_item = it_lotes_alv_sel-prod_item.
    IF sy-subrc = 0.
      DELETE FROM zmmt0102
               WHERE ebeln   = wa_item_lote-ebeln
               AND   ebelp   = wa_item_lote-ebelp
               AND   mblnr   =  ' '
               AND   charg   = it_lotes_alv_sel-charg.
    ENDIF.

    obj_nfe_inbound->excluir_lote_item( i_cd_lote_item = it_lotes_alv_sel-cd_lote_item ).
  ENDLOOP.

  CLEAR: it_lotes, it_lotes_c, it_lotes_alv_t, it_lotes_alv_t[], it_lotes_c, it_lotes_c[], it_carac_alv_u[], zib_nfe_dist_lot, it_carac_alv_u.
  DATA(lc_info_nfe) = obj_nfe_inbound->get_info_nota( ).
  MOVE-CORRESPONDING lc_info_nfe-nfe_base TO zib_nfe_dist_ter.
  MOVE-CORRESPONDING lc_info_nfe-nfe_alv  TO zde_nfe_inbound_alv.
  MOVE-CORRESPONDING lc_info_nfe-nfe_alv  TO zde_nfe_dist_alv.
  MOVE lc_info_nfe-nfe_base-lotes   TO it_lotes.
  MOVE lc_info_nfe-nfe_base-lotes_c TO it_lotes_c.

  LOOP AT it_lotes INTO DATA(wa_lotes).
    MOVE-CORRESPONDING wa_lotes TO it_lotes_alv_t.
    APPEND it_lotes_alv_t.
  ENDLOOP.

  LOOP AT it_lotes_c INTO DATA(wa_lotes_c).
    MOVE-CORRESPONDING wa_lotes_c TO it_carac_alv.
    APPEND it_carac_alv.
  ENDLOOP.

  READ TABLE it_itens_alv INTO DATA(wa_item_selecionado) WITH KEY line_color = cs_line_color_selecionada.
  IF sy-subrc IS INITIAL.
    READ TABLE it_lotes_alv_t WITH KEY prod_item = wa_item_selecionado-prod_item INTO DATA(wa_lc_lote).
    IF sy-subrc IS INITIAL.
      p_index = sy-tabix.
      PERFORM selecionar_lote USING wa_item_selecionado-prod_item wa_lc_lote-cd_lote_item abap_false.
    ENDIF.
  ENDIF.

  LEAVE TO SCREEN 1600.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_INFO_TRASNPORTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_info_trasnporte .

  CLEAR: zib_nfe_dist_frt.
  obj_nfe_inbound->get_info_transporte( IMPORTING e_zib_nfe_dist_frt = zib_nfe_dist_frt ).

  CALL SCREEN 1603 STARTING AT 40 05.

ENDFORM.
