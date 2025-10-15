*----------------------------------------------------------------------*
***INCLUDE LZNFE_INBOUNDO02.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_1500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1500 OUTPUT.

  DATA: it_pf1500 TYPE TABLE OF sy-ucomm.

  sb_tela_1501 = tl_tela_1501.
  sb_tela_1502 = tl_tela_1502.
  sb_tela_1503 = tl_tela_1503.

  CLEAR: it_pf1500.

  IF wa_nfe_inbound-nfe_base-st_fiscal = zcl_nfe_inbound=>st_fiscal_sem_aceite_fiscal.
    APPEND ok_aceite_doc TO it_pf1500.
    APPEND ok_aceite_nao TO it_pf1500.
    APPEND ok_save       TO it_pf1500.
    APPEND ok_aceite_can TO it_pf1500.
  ELSEIF wa_nfe_inbound-nfe_base-st_fiscal = zcl_nfe_inbound=>st_fiscal_nao_aceito_fiscal.
    APPEND ok_aceite_doc TO it_pf1500.
    APPEND ok_aceite_can TO it_pf1500.
    APPEND ok_save       TO it_pf1500.
  ELSE.
    IF wa_nfe_inbound-nfe_base-ck_fiscal EQ abap_true.
      APPEND ok_aceite_doc TO it_pf1500.
      APPEND ok_aceite_nao TO it_pf1500.
      APPEND ok_save       TO it_pf1500.
    ELSE.
      APPEND ok_aceite_can TO it_pf1500.
    ENDIF.
  ENDIF.

*-CS2025000249-08.04.2025-#173180-JT-inicio
  IF obj_nfe_inbound->get_pedido_massa( )-confirma_ped_massa = abap_true.
    APPEND ok_aceite_nao TO it_pf1500.
    APPEND ok_aceite_can TO it_pf1500.
  ENDIF.
*-CS2025000249-08.04.2025-#173180-JT-fim

  SET PF-STATUS 'PF1500' EXCLUDING it_pf1500.
  SET TITLEBAR 'TL1500'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1500_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1500_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1500 INPUT.

  DATA: e_motivo         TYPE  zde_motivo_rejeicao_fiscal,
        e_tp_autorizacao TYPE  zde_tp_autorizacao,
        e_tline          TYPE  tline_t.

  CASE ok_code.
    WHEN ok_aceite_nao .
      "Selecionar Motivos p/ Não Aceitar
      CLEAR: ok_code.
      CALL FUNCTION 'ZNFE_SEL_MOTIVO_REJEICAO'
        IMPORTING
          e_motivo = e_motivo.

      CHECK e_motivo IS NOT INITIAL.

      CALL FUNCTION 'ZNFE_INFO_MOTIVO_ESTRATEGIA'
        EXPORTING
          i_motivo         = e_motivo
          i_chave_nfe      = wa_nfe_inbound-nfe_base-chave_nfe
        IMPORTING
          e_tp_autorizacao = e_tp_autorizacao
          e_tline          = e_tline.

      CHECK e_tp_autorizacao IS NOT INITIAL.

      TRY.
          obj_nfe_inbound->nfe_inbound_rejeita_aceite( i_motivo = e_motivo i_tp_autorizacao = e_tp_autorizacao i_tline = e_tline ).
          PERFORM limpar_variaveis.
          PERFORM get_info_tela.
        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN ok_aceite_can .
      CLEAR: ok_code.
      TRY.
          obj_nfe_inbound->nfe_inbound_cancela_aceite( ).
          PERFORM limpar_variaveis.
          PERFORM get_info_tela.

        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN ok_aceite_doc.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      SELECT SINGLE *
        FROM zmmt0203 AS a INTO @DATA(lwa_zmmt0203)
       WHERE chave_nfe EQ @wa_nfe_inbound-nfe_base-chave_nfe
         AND cancel    EQ @abap_false
         AND EXISTS ( SELECT nro_cg
                        FROM zmmt0201 AS b
                       WHERE b~nro_cg = a~nro_cg
                         AND b~cancel = @abap_false ).

      IF sy-subrc EQ 0.
        MESSAGE |Nota vinculada a Carga de Entrada { lwa_zmmt0203-nro_cg }! Aceite Fiscal só pode ser realizado na transação ZSDT0112!| TYPE 'I'.
        RETURN.
      ELSE.
        SELECT SINGLE *
          FROM zsdt0410 AS a INTO @DATA(lwa_zsdt0410)
         WHERE chave_nfe EQ @wa_nfe_inbound-nfe_base-chave_nfe
           AND cancel    EQ @abap_false
           AND EXISTS ( SELECT nro_cg
                          FROM zsdt0133 AS b
                         WHERE b~nro_cg EQ a~nro_cg
                           AND status   NE 'X' ).

        IF sy-subrc EQ 0.
          DATA(_troca_nota) = zcl_carga_saida_insumos=>check_carga_troca_nota( EXPORTING i_nro_cg  = lwa_zsdt0410-nro_cg ).
          IF _troca_nota EQ abap_true.
            MESSAGE |Nota vinculada a Carga de Saida { lwa_zsdt0410-nro_cg }! Aceite Fiscal só pode ser realizado na transação ZSDT0112!| TYPE 'I'.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

*-CS2025000249-08.04.2025-#173180-JT-inicio
*      CLEAR: ok_code.
*      obj_nfe_inbound->zif_cadastro~ck_alterou = abap_true.
*      CHECK ck_alterou_armazem EQ abap_false.
*      CHECK ck_alterou_departamento EQ abap_false.
*      CHECK ck_alterou_ck_possui_frete EQ abap_false.
*      CHECK ck_alterou_transportador EQ abap_false.
*      CHECK ck_alterou_nr_fase EQ abap_false.
*      CHECK ck_alterou_dt_vencimento EQ abap_false.
*
*      "Aceite Fiscal
*      TRY .
*          obj_nfe_inbound->set_aceitar_documento( ).
*
*          TRY .
*              IF obj_nfe_inbound->zif_cadastro~gravar_registro( ) EQ abap_true.
*                LEAVE TO SCREEN 0.
*              ENDIF.
*            CATCH zcx_cadastro INTO ex_cadastro.
*              ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
*          ENDTRY.
*
*        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
*          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
*        CATCH zcx_pedido_compra_exception INTO ex_pedido_compra.
*          ex_pedido_compra->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
*      ENDTRY.

      PERFORM f_aceitar_registro.
*-CS2025000249-08.04.2025-#173180-JT-inicio
    WHEN ok_obs_fiscal.
      CLEAR: ok_code.
      TRY .
          CALL FUNCTION 'ZNFE_INBOUND_OBS'
            EXPORTING
              i_nota = obj_nfe_inbound
              i_tdid = 'ZFIS'.

        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN ok_obs_contri.
      CLEAR: ok_code.
      TRY.
          CALL FUNCTION 'ZNFE_INBOUND_OBS'
            EXPORTING
              i_nota = obj_nfe_inbound
              i_tdid = 'ZCON'.

        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN ok_save.
*-CS2025000249-08.04.2025-#173180-JT-inicio
*      CLEAR: ok_code.
*      CHECK ck_alterou_armazem EQ abap_false.
*      CHECK ck_alterou_departamento EQ abap_false.
*      CHECK ck_alterou_ck_possui_frete EQ abap_false.
*      CHECK ck_alterou_transportador EQ abap_false.
*
*      TRY .
*          IF obj_nfe_inbound->zif_cadastro~gravar_registro( ) EQ abap_true.
*            LEAVE TO SCREEN 0.
*          ENDIF.
*        CATCH zcx_cadastro INTO ex_cadastro.
*          ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
*      ENDTRY.

      PERFORM f_salvar_registro.
*-CS2025000249-08.04.2025-#173180-JT-fim

*-CS2025000249-04.06.2025-#168929-JT-inicio
    WHEN 'VIEW_IRF'.
      CALL FUNCTION 'ZNFE_EXIBIR_IRF_FORNECEDOR'
        EXPORTING
          i_bukrs   = wa_nfe_inbound-nfe_base-e_tomadora
          i_lifnr   = wa_nfe_inbound-nfe_base-p_emissor
        EXCEPTIONS
          sem_dados = 1.

      IF sy-subrc <> 0.
        MESSAGE s024(sd) WITH 'Não há dados de IRF.' DISPLAY LIKE 'E'.
      ENDIF.
*-CS2025000249-04.06.2025-#168929-JT-fim

  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1502  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1502 OUTPUT.

  IF zde_nfe_dist_alv-e_tomadora IS NOT INITIAL.
    SELECT SINGLE butxt INTO lc_txt_butxt
      FROM t001
     WHERE bukrs EQ zde_nfe_dist_alv-e_tomadora.
  ENDIF.

  IF ck_alterou_ck_possui_frete EQ abap_true.
    obj_nfe_inbound->set_ck_possui_frete( i_ck_possui_frete = zde_nfe_dist_alv-ck_possui_frete ).
    ck_alterou_ck_possui_frete = abap_false.
  ENDIF.

  IF zde_nfe_dist_alv-cd_departamento IS NOT INITIAL AND ck_alterou_departamento EQ abap_true.
    TRY .
        obj_nfe_inbound->set_departamento( i_cd_departamento = zde_nfe_dist_alv-cd_departamento ).

        CALL METHOD obj_nfe_inbound->get_info_departamento
          EXPORTING
            i_cd_departamento = zde_nfe_dist_alv-cd_departamento
          RECEIVING
            r_ds_departamento = zde_nfe_dist_alv-ds_departamento.

        ck_alterou_departamento = abap_false.

      CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.    "
        ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_cadastro INTO ex_cadastro.    "
        ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.

  ELSEIF zde_nfe_dist_alv-cd_departamento IS INITIAL AND ck_alterou_departamento EQ abap_true.

    TRY .
        obj_nfe_inbound->set_departamento( i_cd_departamento = zde_nfe_dist_alv-cd_departamento ).
        CLEAR: zde_nfe_dist_alv-ds_departamento.
        ck_alterou_departamento = abap_false.
      CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.    "
        ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_cadastro INTO ex_cadastro.    "
        ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.

  ENDIF.

  LOOP AT SCREEN.
    IF screen-name(16) EQ 'ZDE_NFE_DIST_ALV'.
      SPLIT screen-name AT '-' INTO DATA(str1_1502) DATA(str2_1502).
      i_campo = str2_1502.
      IF obj_nfe_inbound->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ) EQ abap_true.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
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
    DATA(wa_trans) = obj_nfe_inbound->set_transportadora( i_f_transporte = zde_nfe_dist_alv-f_transporte ).
    lc_transportador_cnpj  = wa_trans-stcd1.
    lc_transportador_ie    = wa_trans-stcd3.
    lc_transportador_razao = wa_trans-name1.
    lc_transportador_regio = wa_trans-regio.
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
    DATA(wa_lfa1) = obj_nfe_inbound->get_info_fornecedor( i_lifnr = zde_nfe_dist_alv-f_transporte ).
    lc_transportador_cnpj  = wa_lfa1-stcd1.
    lc_transportador_ie    = wa_lfa1-stcd3.
    lc_transportador_razao = wa_lfa1-name1.
    lc_transportador_regio = wa_lfa1-regio.
  ENDIF.

  IF zde_nfe_dist_alv-f_armazem IS NOT INITIAL AND ck_alterou_armazem EQ abap_true.
    DATA(armazem) = obj_nfe_inbound->set_armazem( i_f_armazem = zde_nfe_dist_alv-f_armazem ).
    zde_nfe_dist_alv-armazem_cnpj  = armazem-stcd1.
    zde_nfe_dist_alv-armazem_ie    = armazem-stcd3.
    zde_nfe_dist_alv-armazem_razao = armazem-name1.
    lc_armazem_regio               = armazem-regio.
    ck_alterou_armazem = abap_false.
  ELSEIF  zde_nfe_dist_alv-f_armazem IS INITIAL AND ck_alterou_armazem EQ abap_true.
    CLEAR:
    zde_nfe_dist_alv-armazem_cnpj,
    zde_nfe_dist_alv-armazem_ie,
    zde_nfe_dist_alv-armazem_razao,
    lc_armazem_regio.
    ck_alterou_armazem = abap_false.
  ENDIF.

  IF ck_alterou_nr_fase EQ abap_true.
    obj_nfe_inbound->set_nr_fase( i_nr_fase = zde_nfe_dist_alv-nr_fase ).
    ck_alterou_nr_fase = abap_false.
  ENDIF.

  IF ck_alterou_dt_vencimento EQ abap_true.
    obj_nfe_inbound->set_dt_vencimento( i_dt_vencimento = zde_nfe_dist_alv-dt_vencimento ).
    ck_alterou_dt_vencimento = abap_false.
  ENDIF.

  IF container_1503 IS INITIAL.
    CLEAR wa_layout_1503.
    wa_layout_1503-zebra      = abap_true.
    wa_stable_1503-row        = abap_true.
    wa_stable_1503-col        = abap_true.
    wa_layout_1503-stylefname = 'STYLE'.
    wa_layout_1503-sel_mode   = 'A'.
*   wa_layout_1503-cwidth_opt = 'X'.  "*-CS2025000249-04.06.2025-#168929-JT
*   wa_layout_1503-col_opt    = 'X'.  "*-CS2025000249-04.06.2025-#168929-JT
    wa_layout_1503-info_fname = 'LINE_COLOR'.
    wa_layout_1503-ctab_fname = 'COLOR_CELL'.

*    CREATE OBJECT CL_CONTAINER_1503
*      EXPORTING
*        "REPID = SY-REPID
*        "DYNNR = VG_TELA_1503
*        SIDE      = CL_CONTAINER_1503->DOCK_AT_BOTTOM
*        RATIO     = 20
*        EXTENSION = 20.
    CREATE OBJECT container_1503
      EXPORTING
        container_name = 'ALV_ITENS'.

    PERFORM fill_it_fieldcatalog_1503.

*   Fill info for layout variant
    PERFORM fill_gs_variant_1503.

    CREATE OBJECT cl_grid_1503
      EXPORTING
        i_parent = container_1503.

    CREATE OBJECT obg_toolbar_1503
      EXPORTING
        io_alv_grid = cl_grid_1503.

    SET HANDLER obg_toolbar_1503->on_toolbar FOR cl_grid_1503.
    SET HANDLER obg_toolbar_1503->handle_user_command FOR cl_grid_1503.

    CREATE OBJECT event_handler.
    SET HANDLER event_handler->handle_hotspot_click FOR cl_grid_1503.

    cl_grid_1503->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    cl_grid_1503->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_function_1503.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_function_1503.
    APPEND cl_gui_alv_grid=>mc_fc_loc_move_row      TO it_function_1503.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_function_1503.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_function_1503.
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_function_1503.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_function_1503.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_function_1503.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_function_1503.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_1503.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_1503.
    APPEND cl_gui_alv_grid=>mc_fc_check             TO it_function_1503.
    APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_function_1503.

*    GS_F4-FIELDNAME = 'PLANETYPE'.
*    IF P_F4 = 'X'.
*      GS_F4-REGISTER = 'X'.
*    ENDIF.
*    IF P_BFORF4 = 'X'.
*      GS_F4-GETBEFORE = 'X'.
*    ENDIF.
*    IF P_AFTRF4 = 'X'.
*      GS_F4-CHNGEAFTER = 'X'.
*    ENDIF.

    wa_f4_1503-fieldname = 'EBELN'.
    wa_f4_1503-register  = 'X'.
    wa_f4_1503-getbefore = 'X'.
    INSERT wa_f4_1503 INTO TABLE it_f4_1503.
    wa_f4_1503-fieldname = 'EBELP'.
    INSERT wa_f4_1503 INTO TABLE it_f4_1503.
    wa_f4_1503-fieldname = 'MATNR'.
    INSERT wa_f4_1503 INTO TABLE it_f4_1503.
    wa_f4_1503-fieldname = 'LGORT'.          "*-CS2025000249-04.06.2025-#168929-JT
    INSERT wa_f4_1503 INTO TABLE it_f4_1503. "*-CS2025000249-04.06.2025-#168929-JT

    CREATE OBJECT event_receiver_1502.
    SET HANDLER event_receiver_1502->handle_data_changed_1502 FOR cl_grid_1503.
    SET HANDLER event_receiver_1502->handle_f4_1502 FOR cl_grid_1503.

    CALL METHOD cl_grid_1503->register_f4_for_fields EXPORTING it_f4 = it_f4_1503.

    CALL METHOD cl_grid_1503->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_variant_1503
        i_save                        = 'A'
        is_layout                     = wa_layout_1503
        it_toolbar_excluding          = it_function_1503
      CHANGING
        it_outtab                     = it_itens_alv[]
        it_fieldcatalog               = it_fieldcat_1503
        it_sort                       = it_sort_1503
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  CALL METHOD cl_grid_1503->refresh_table_display
    EXPORTING
      is_stable = wa_stable_1503.

  IF lc_fornecedor_regio IS INITIAL.
    SELECT SINGLE regio INTO lc_fornecedor_regio
      FROM lfa1
     WHERE lifnr EQ zde_nfe_dist_alv-p_emissor.
  ENDIF.

  IF lc_destinatario_regio IS INITIAL.

    wa_lfa1-lifnr = zde_nfe_dist_alv-f_tomadora.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_lfa1-lifnr
      IMPORTING
        output = wa_lfa1-lifnr.

    SELECT SINGLE regio INTO lc_fornecedor_regio
      FROM kna1
     WHERE kunnr EQ wa_lfa1-lifnr.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1503
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1503 .

  wa_variant_1503-report      = sy-repid.
  wa_variant_1503-handle      = '1503'.
  wa_variant_1503-log_group   = abap_false.
  wa_variant_1503-username    = abap_false.
  wa_variant_1503-variant     = abap_false.
  wa_variant_1503-text        = abap_false.
  wa_variant_1503-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1503
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1503 .

  DATA: i_contador_2 TYPE lvc_colpos.

  CLEAR: it_fieldcat_1503[], it_fieldcat_1503.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_NFE_DIST_ITM_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcat_1503.

  i_contador_2 = 20. "17.  "*-CS2025000249-04.06.2025-#168929-JT

  APPEND VALUE #(
  fieldname = 'PRECOS'
  datatype  = 'CHAR'
  inttype   = 'C'
  intlen    = '000004'
  lowercase = 'X'
  domname   = 'CHAR04'
  scrtext_l = '$Pedido'
  scrtext_m = '$Pedido'
  scrtext_s = '$Pedido'
  hotspot   = abap_true ) TO it_fieldcat_1503.

  LOOP AT it_fieldcat_1503 ASSIGNING FIELD-SYMBOL(<fs_1503>).
    <fs_1503>-edit = abap_false.
    CASE <fs_1503>-fieldname.
      WHEN 'PRECOS'.
        <fs_1503>-outputlen = 8.     "4.    "*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-col_pos   = 18.    "16.   "*-CS2025000249-04.06.2025-#168929-JT
      WHEN 'PROD_ITEM'.
        <fs_1503>-col_pos = 1.
      WHEN 'PROD_CODIGO'.
        <fs_1503>-outputlen = 6.
        <fs_1503>-col_pos   = 1.
      WHEN 'PROD_DESCRICAO'.
        <fs_1503>-outputlen = 20. "28.  ""*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-col_pos   = 2.
      WHEN 'PROD_NCM'.
        <fs_1503>-outputlen = 9.        ""*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-col_pos = 3.
      WHEN 'PROD_UND_COMERCI'.
        <fs_1503>-outputlen = 7.        ""*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-col_pos = 4.
      WHEN 'PROD_QTD_COMERCI'.
        <fs_1503>-outputlen = 8.        ""*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-col_pos = 5.
      WHEN 'PROD_VLR_UND_COM'.
        <fs_1503>-outputlen = 10.        ""*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-col_pos = 6.
      WHEN 'PROD_VLR_TOTAL_B'.
        <fs_1503>-outputlen = 10.        ""*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-col_pos = 7.
      WHEN 'COF_CST'.
        <fs_1503>-no_out  = abap_true.
        <fs_1503>-style     = cl_gui_alv_grid=>mc_style_button.
        <fs_1503>-scrtext_l = 'COFINS'.
        <fs_1503>-scrtext_m = 'CO'.
        <fs_1503>-scrtext_s = 'CO'.
        <fs_1503>-col_pos = 8.
      WHEN 'PIS_CST'.
        <fs_1503>-no_out    = abap_true.
        <fs_1503>-style     = cl_gui_alv_grid=>mc_style_button.
        <fs_1503>-scrtext_l = 'PIS'.
        <fs_1503>-scrtext_m = 'PI'.
        <fs_1503>-scrtext_s = 'PI'.
        <fs_1503>-col_pos = 9.
      WHEN 'IPI_CST'.
        <fs_1503>-no_out    = abap_true.
        <fs_1503>-style     = cl_gui_alv_grid=>mc_style_button.
        <fs_1503>-scrtext_l = 'IPI'.
        <fs_1503>-scrtext_m = 'IP'.
        <fs_1503>-scrtext_s = 'IP'.
        <fs_1503>-col_pos = 10.
      WHEN 'ICMS_CST'.
        <fs_1503>-style     = cl_gui_alv_grid=>mc_style_button.
        <fs_1503>-scrtext_l = 'ICMS'.
        <fs_1503>-scrtext_m = 'IC'.
        <fs_1503>-scrtext_s = 'IC'.
        <fs_1503>-col_pos = 11.
      WHEN 'MATNR'.
        i_campo = <fs_1503>-fieldname.
        <fs_1503>-outputlen = 8.        ""*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-edit  = obj_nfe_inbound->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ).
        <fs_1503>-col_pos = 12.
      WHEN 'MEINS'.
        i_campo = <fs_1503>-fieldname.
        <fs_1503>-outputlen = 3.        ""*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-edit  = obj_nfe_inbound->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ).
        <fs_1503>-col_pos = 13.
      WHEN 'MENGE'.
        i_campo = <fs_1503>-fieldname.
        <fs_1503>-outputlen = 5.        ""*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-edit  = obj_nfe_inbound->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ).
        <fs_1503>-col_pos = 14.
      WHEN 'EBELN'.
        i_campo = <fs_1503>-fieldname.
        <fs_1503>-outputlen = 10.        ""*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-edit  = obj_nfe_inbound->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ).
        <fs_1503>-col_pos = 15.
      WHEN 'EBELP'.
        i_campo = <fs_1503>-fieldname.
        <fs_1503>-outputlen = 4.        ""*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-edit  = obj_nfe_inbound->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ).
        <fs_1503>-col_pos = 16.
*-CS2025000249-04.06.2025-#168929-JT-inicio
      WHEN 'LGORT'.
        i_campo = <fs_1503>-fieldname.
        <fs_1503>-outputlen  = 8.           ""*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-f4availabl = abap_true.   ""*-CS2025000249-04.06.2025-#168929-JT
        <fs_1503>-edit  = obj_nfe_inbound->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ).
        <fs_1503>-col_pos = 17.
*-CS2025000249-04.06.2025-#168929-JT-fim
      WHEN OTHERS.
        <fs_1503>-no_out  = abap_true.
        <fs_1503>-col_pos = i_contador_2.
        ADD 1 TO i_contador_2.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_ARMAZEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_armazem INPUT.
  ck_alterou_armazem = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  F4_1502
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_1502 USING r_fieldname TYPE lvc_fname rs_row_no TYPE lvc_s_roid rr_event_data TYPE REF TO cl_alv_event_data rt_bad_cells TYPE lvc_t_modi.

  FIELD-SYMBOLS: <lt_f4> TYPE lvc_t_modi.

*-CS2025000249-04.06.2025-#168929-JT-inicio
  TYPES: BEGIN OF ty_f4_lgort,
           werks TYPE t001l-werks,
           lgort TYPE t001l-lgort,
           lgobe TYPE t001l-lgobe.
  TYPES: END OF ty_f4_lgort.
*-CS2025000249-04.06.2025-#168929-JT-fim

  DATA: ls_f4  TYPE lvc_s_modi,
        i_nota TYPE zib_nfe_dist_ter,
        i_item TYPE zib_nfe_dist_itm.

*-CS2025000249-04.06.2025-#168929-JT-inicio
  DATA: it_ret      TYPE STANDARD TABLE OF ddshretval,
        it_f4_lgort TYPE STANDARD TABLE OF ty_f4_lgort,
        wa_ret      TYPE ddshretval,
        ls_modi     TYPE lvc_s_modi.
*-CS2025000249-04.06.2025-#168929-JT-fim

  CASE r_fieldname.
    WHEN 'MATNR'.
      rr_event_data->m_event_handled = 'X'.
      ASSIGN rr_event_data->m_data->* TO <lt_f4>.
      MESSAGE 'Fazer Pesquisa material' TYPE 'I'.

    WHEN 'EBELN' OR 'EBELP'.

      "ZDE_NFE_DIST_ALV
      MOVE-CORRESPONDING zde_nfe_dist_alv TO i_nota.
      READ TABLE it_itens_alv INDEX rs_row_no-row_id INTO DATA(wa_itens_alv).
      MOVE-CORRESPONDING wa_itens_alv TO i_item.

      rr_event_data->m_event_handled = 'X'.
      ASSIGN rr_event_data->m_data->* TO <lt_f4>.

      CALL METHOD obj_nfe_inbound->get_pedido_compra_chave
        EXPORTING
          i_nota = i_nota
          i_item = i_item
        RECEIVING
          r_ekpo = DATA(wa_expo)
        EXCEPTIONS
          erro   = 1
          OTHERS = 2.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      ELSE.
        ls_f4-row_id    = rs_row_no-row_id.
        ls_f4-fieldname = 'EBELN'.
        ls_f4-value     = wa_expo-ebeln.
        APPEND ls_f4 TO <lt_f4>.
        ls_f4-row_id    = rs_row_no-row_id.
        ls_f4-fieldname = 'EBELP'.
        ls_f4-value     = wa_expo-ebelp.
        APPEND ls_f4 TO <lt_f4>.
      ENDIF.

    WHEN 'MEINS'.

*-CS2025000249-04.06.2025-#168929-JT-inicio
    WHEN 'LGORT'.
      READ TABLE it_fieldcat_1503 INTO DATA(w_fieldcat_1503) WITH KEY fieldname = 'LGORT'.
      READ TABLE it_itens_alv INDEX rs_row_no-row_id INTO wa_itens_alv.

      SELECT *
        FROM t001l
        INTO CORRESPONDING FIELDS OF TABLE it_f4_lgort
       WHERE werks = zde_nfe_dist_alv-branch.

      SORT it_f4_lgort BY lgort.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'LGORT'
          window_title    = 'Depósitos'
          value_org       = 'S'
        TABLES
          value_tab       = it_f4_lgort
          return_tab      = it_ret
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.

      READ TABLE it_ret INTO wa_ret INDEX 1.
      IF sy-subrc = 0 AND wa_ret-fieldval IS NOT INITIAL AND w_fieldcat_1503-edit = abap_true.
        rr_event_data->m_event_handled = abap_true.
        ASSIGN rr_event_data->m_data->* TO <lt_f4>.
        ls_modi-row_id    = rs_row_no-row_id.
        ls_modi-fieldname = 'LGORT'.
        ls_modi-value     =  wa_ret-fieldval.
        APPEND ls_modi   TO <lt_f4>.
      ENDIF.
*-CS2025000249-04.06.2025-#168929-JT-fim

  ENDCASE.

  "ls_f4-fieldname = r_fieldname.
  "ls_f4-row_id = rs_row_no-row_id.
  "ls_f4-value = 'BLUBBER'.
  "append ls_f4 to <lt_f4>.

ENDFORM.                                                    " F4

*&---------------------------------------------------------------------*
*&      Form  data_changed_1502
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_changed_1502 USING  rr_data_changed TYPE REF TO
                                         cl_alv_changed_data_protocol.

  DATA: ls_mod_cells TYPE lvc_s_modi.
  DATA: ls_cells     TYPE lvc_s_modi.
  DATA: l_matnr      TYPE matnr.
  DATA: l_ebeln	TYPE ebeln.
  DATA: l_ebelp	TYPE ebelp.
  DATA: l_lgort	TYPE lgort_d.   "*-CS2025000249-04.06.2025-#168929-JT
  DATA: l_menge	TYPE j_1bnetqty.
  DATA: l_meins	TYPE j_1bnetunt.
  DATA: l_netpr	TYPE j_1bnetpri.
  DATA: l_netwr	TYPE j_1bnetval.
  DATA: lv_value TYPE lvc_value. "*-CS2025000249-04.06.2025-#168929-JT

  LOOP AT rr_data_changed->mt_good_cells INTO ls_mod_cells.

    READ TABLE it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX ls_mod_cells-row_id.

    CASE ls_mod_cells-fieldname.
      WHEN  'MATNR'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_matnr.

        IF l_matnr IS NOT INITIAL.
          SELECT SINGLE * INTO @DATA(wa_mara) FROM mara WHERE matnr EQ @l_matnr.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE s014 WITH l_matnr.

            CALL METHOD rr_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = sy-msgid
                i_msgno     = sy-msgno
                i_msgty     = 'E'
                i_msgv1     = sy-msgv1
                i_msgv2     = sy-msgv2
                i_msgv3     = sy-msgv3
                i_msgv4     = sy-msgv4
                i_fieldname = ls_mod_cells-fieldname
                i_row_id    = ls_mod_cells-row_id.
          ELSE.
            SELECT SINGLE * INTO @DATA(wa_makt)
              FROM makt
             WHERE spras EQ @sy-langu
               AND matnr EQ @l_matnr.

            MESSAGE s015 WITH wa_makt-maktg.

            <fs_item>-matnr = wa_makt-matnr.

            CALL METHOD obj_nfe_inbound->set_item_material
              EXPORTING
                i_prod_item = <fs_item>-prod_item
                i_matnr     = <fs_item>-matnr
                i_ebeln     = <fs_item>-ebeln
                i_ebelp     = <fs_item>-ebelp
                i_lgort     = <fs_item>-lgort  "*-CS2025000249-04.06.2025-#168929-JT-inicio
                i_menge     = <fs_item>-menge
                i_meins     = <fs_item>-meins
                i_netpr     = <fs_item>-netpr
                i_netwr     = <fs_item>-netwr.
          ENDIF.
        ELSE.
          CLEAR: <fs_item>-matnr.

          CALL METHOD obj_nfe_inbound->set_item_material
            EXPORTING
              i_prod_item = <fs_item>-prod_item
              i_matnr     = <fs_item>-matnr
              i_ebeln     = <fs_item>-ebeln
              i_ebelp     = <fs_item>-ebelp
              i_lgort     = <fs_item>-lgort  "*-CS2025000249-04.06.2025-#168929-JT-inicio
              i_menge     = <fs_item>-menge
              i_meins     = <fs_item>-meins
              i_netpr     = <fs_item>-netpr
              i_netwr     = <fs_item>-netwr.
        ENDIF.
      WHEN 'MENGE'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_menge.

        <fs_item>-menge = l_menge.

        CALL METHOD obj_nfe_inbound->set_item_material
          EXPORTING
            i_prod_item = <fs_item>-prod_item
            i_matnr     = <fs_item>-matnr
            i_ebeln     = <fs_item>-ebeln
            i_ebelp     = <fs_item>-ebelp
            i_lgort     = <fs_item>-lgort  "*-CS2025000249-04.06.2025-#168929-JT-inicio
            i_menge     = <fs_item>-menge
            i_meins     = <fs_item>-meins
            i_netpr     = <fs_item>-netpr
            i_netwr     = <fs_item>-netwr.

      WHEN 'MEINS'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_meins.

        <fs_item>-meins = l_meins.

        CALL METHOD obj_nfe_inbound->set_item_material
          EXPORTING
            i_prod_item = <fs_item>-prod_item
            i_matnr     = <fs_item>-matnr
            i_ebeln     = <fs_item>-ebeln
            i_ebelp     = <fs_item>-ebelp
            i_lgort     = <fs_item>-lgort  "*-CS2025000249-04.06.2025-#168929-JT-inicio
            i_menge     = <fs_item>-menge
            i_meins     = <fs_item>-meins
            i_netpr     = <fs_item>-netpr
            i_netwr     = <fs_item>-netwr.

      WHEN 'EBELN'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_ebeln.

        <fs_item>-ebeln = l_ebeln.

*-CS2025000249-04.06.2025-#168929-JT-inicio
        IF <fs_item>-lgort IS INITIAL.
          SELECT SINGLE lgort
            INTO @DATA(_lgort)
            FROM ekpo
           WHERE ebeln = @l_ebeln
             AND ebelp = @<fs_item>-ebelp.

          <fs_item>-lgort = _lgort.
          lv_value        = _lgort.

          CALL METHOD rr_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_mod_cells-row_id
              i_fieldname = 'LGORT'
              i_value     = lv_value.
        ENDIF.
*-CS2025000249-04.06.2025-#168929-JT-fim

        CALL METHOD obj_nfe_inbound->set_item_material
          EXPORTING
            i_prod_item = <fs_item>-prod_item
            i_matnr     = <fs_item>-matnr
            i_ebeln     = <fs_item>-ebeln
            i_ebelp     = <fs_item>-ebelp
            i_lgort     = <fs_item>-lgort  "*-CS2025000249-04.06.2025-#168929-JT-inicio
            i_menge     = <fs_item>-menge
            i_meins     = <fs_item>-meins
            i_netpr     = <fs_item>-netpr
            i_netwr     = <fs_item>-netwr.

      WHEN 'EBELP'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_ebelp.

        <fs_item>-ebelp = l_ebelp.

*-CS2025000249-04.06.2025-#168929-JT-inicio
        IF <fs_item>-lgort IS INITIAL.
          SELECT SINGLE lgort
            INTO @_lgort
            FROM ekpo
           WHERE ebeln = @<fs_item>-ebeln
             AND ebelp = @l_ebelp.

          <fs_item>-lgort = _lgort.
          lv_value        = _lgort.

          CALL METHOD rr_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_mod_cells-row_id
              i_fieldname = 'LGORT'
              i_value     = lv_value.
        ENDIF.
*-CS2025000249-04.06.2025-#168929-JT-fim

        CALL METHOD obj_nfe_inbound->set_item_material
          EXPORTING
            i_prod_item = <fs_item>-prod_item
            i_matnr     = <fs_item>-matnr
            i_ebeln     = <fs_item>-ebeln
            i_ebelp     = <fs_item>-ebelp
            i_lgort     = <fs_item>-lgort  "*-CS2025000249-04.06.2025-#168929-JT-inicio
            i_menge     = <fs_item>-menge
            i_meins     = <fs_item>-meins
            i_netpr     = <fs_item>-netpr
            i_netwr     = <fs_item>-netwr.

*-CS2025000249-04.06.2025-#168929-JT-inicio
      WHEN 'LGORT'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_lgort.

        <fs_item>-lgort = l_lgort.

        CALL METHOD obj_nfe_inbound->set_item_material
          EXPORTING
            i_prod_item = <fs_item>-prod_item
            i_matnr     = <fs_item>-matnr
            i_ebeln     = <fs_item>-ebeln
            i_ebelp     = <fs_item>-ebelp
            i_lgort     = <fs_item>-lgort  "*-CS2025000249-04.06.2025-#168929-JT-inicio
            i_menge     = <fs_item>-menge
            i_meins     = <fs_item>-meins
            i_netpr     = <fs_item>-netpr
            i_netwr     = <fs_item>-netwr.
*-CS2025000249-04.06.2025-#168929-JT-fim

      WHEN 'NETPR'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_netpr.

        <fs_item>-netpr = l_netpr.

        CALL METHOD obj_nfe_inbound->set_item_material
          EXPORTING
            i_prod_item = <fs_item>-prod_item
            i_matnr     = <fs_item>-matnr
            i_ebeln     = <fs_item>-ebeln
            i_ebelp     = <fs_item>-ebelp
            i_lgort     = <fs_item>-lgort  "*-CS2025000249-04.06.2025-#168929-JT-inicio
            i_menge     = <fs_item>-menge
            i_meins     = <fs_item>-meins
            i_netpr     = <fs_item>-netpr
            i_netwr     = <fs_item>-netwr.


      WHEN 'NETWR'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_netwr.

        <fs_item>-netwr = l_netwr.

        CALL METHOD obj_nfe_inbound->set_item_material
          EXPORTING
            i_prod_item = <fs_item>-prod_item
            i_matnr     = <fs_item>-matnr
            i_ebeln     = <fs_item>-ebeln
            i_ebelp     = <fs_item>-ebelp
            i_lgort     = <fs_item>-lgort  "*-CS2025000249-04.06.2025-#168929-JT-inicio
            i_menge     = <fs_item>-menge
            i_meins     = <fs_item>-meins
            i_netpr     = <fs_item>-netpr
            i_netwr     = <fs_item>-netwr.

    ENDCASE.
  ENDLOOP.
ENDFORM.                               " data_changed_1502

*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_TEXTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_TLINES  text
*      -->P_WA_AUTORIZACAO  text
*----------------------------------------------------------------------*
FORM visualizar_texto  TABLES p_tl_tlines STRUCTURE tline.

  MOVE p_tl_tlines[] TO tl_tlines[].

  CALL SCREEN 1503 STARTING AT 5 5.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1503  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1503 OUTPUT.
  SET PF-STATUS 'PF1503'.
  SET TITLEBAR 'TL1503' WITH gb_texto_obs.

  IF ( editor IS INITIAL ).

    CREATE OBJECT container
      EXPORTING
        container_name              = 'LONGTEXT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc IS INITIAL.

    CREATE OBJECT editor
      EXPORTING
        parent                 = container
        wordwrap_mode          = '2'
        wordwrap_position      = '132'
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.

    CHECK sy-subrc IS INITIAL.

    CALL METHOD editor->set_readonly_mode
      EXPORTING
        readonly_mode = editor->true.

    CLEAR: longtext_tab.

    LOOP AT tl_tlines.
      longtext = tl_tlines-tdline.
      APPEND longtext TO longtext_tab.
    ENDLOOP.

    CALL METHOD editor->set_text_as_r3table
      EXPORTING
        table           = longtext_tab
      EXCEPTIONS
        error_dp        = 1
        error_dp_create = 2
        OTHERS          = 3.

  ENDIF.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1503  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1503 INPUT.

  IF editor IS NOT INITIAL.
    editor->free( ).
  ENDIF.
  CLEAR: editor.

  IF container IS NOT INITIAL.
    container->free( ).
  ENDIF.
  CLEAR: container.

  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_CK_POSSUI_FRETE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_ck_possui_frete INPUT.
  ck_alterou_ck_possui_frete = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_TRANSPORTADOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_transportador INPUT.
  ck_alterou_transportador = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_NR_FASE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_nr_fase INPUT.
  ck_alterou_nr_fase = abap_true.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ALTEROU_DT_VENCIMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_dt_vencimento INPUT.
  ck_alterou_dt_vencimento = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_ZBVTYP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_zbvtyp INPUT.
  ck_alterou_zbvtyp = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1700  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1700 INPUT.

  DATA: it_zmmt0149    TYPE TABLE OF zmmt0149 WITH HEADER LINE.

  CASE ok_code.
    WHEN ok_fatura_act.

*-CS2025000249-17.04.2025-#173311-JT-inicio
      PERFORM f_gerar_fatura_act.

*-US 96438-17-09-2024-#96438-RJF-inicio
*      IF gv_open_text EQ abap_on AND zde_nfe_dist_alv-aut_embarque IS NOT INITIAL.
*        IF zde_nfe_dist_alv-chave_nfe IS NOT INITIAL.
*
*          DATA: ls_xml_nfe TYPE znfe_xml_sefaz_auth.
*
*          CALL FUNCTION 'Z_DETALHAMENTO_NFE'
*            EXPORTING
*              i_chave_nfe = zde_nfe_dist_alv-chave_nfe
*            IMPORTING
*              e_xml_nfe   = ls_xml_nfe
*            EXCEPTIONS
*              no_found    = 1
*              OTHERS      = 2.
*          IF sy-subrc <> 0.
** Implement suitable error handling here
*            MESSAGE 'Detalhamento da NFe não encontrado!' TYPE 'I'.
*          ELSE.
*
*            FREE: it_det,
*                  lv_infcpl,
*                  lv_infadfisco.
*
*            it_det = ls_xml_nfe-nfeproc-nfe-infnfe-det[]. "-INFADPROD
*            lv_infcpl = ls_xml_nfe-nfeproc-nfe-infnfe-infadic-infcpl.
*            lv_infadfisco = ls_xml_nfe-nfeproc-nfe-infnfe-infadic-infadfisco.
*
*            "Chamar Tela
*            CALL SCREEN 1800 STARTING AT 30 01.
*            IF sy-ucomm EQ 'CANCELAR'.
**              FREE gv_open_text.
*              EXIT.
*            ELSEIF sy-ucomm EQ 'SALVAR'.
*              IF lv_error IS NOT INITIAL.
*                DATA: lv_ans.
*                CALL FUNCTION 'POPUP_TO_CONFIRM'
*                  EXPORTING
*                    titlebar              = 'Continuar Programar Pagamento?'
*                    text_question         = 'Error: textos não foram salvos!'
*                    text_button_1         = 'Sim'
*                    icon_button_1         = 'ICON_CHECKED'
*                    text_button_2         = 'Não'
*                    icon_button_2         = 'ICON_CANCEL'
*                    display_cancel_button = ' '
*                    popup_type            = 'ICON_MESSAGE_ERROR'
*                  IMPORTING
*                    answer                = lv_ans.
*                IF lv_ans = 2.
**                  FREE gv_open_text.
*                  EXIT.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
**      FREE gv_open_text.
*
*      BREAK rfreitas.
**-US 96438-17-09-2024-#96438-RJF-fim
*
*      CHECK ck_alterou_zbvtyp         EQ abap_false.
*      CHECK ck_alterou_zlspr          EQ abap_false.
*      CHECK ck_alterou_pymt_meth      EQ abap_false.
*      CHECK ck_alterou_housebankid    EQ abap_false.
*      CHECK ck_alterou_vlr_desconto   EQ abap_false.
*      CHECK ck_alterou_obs_financeira EQ abap_false.
*      CHECK ck_alterou_boleto         EQ abap_false.
*
*      CASE ck_entrada_estoque.
*        WHEN abap_false.
*          "Aceite Fatura
*          IF zde_nfe_dist_alv-ctr_waers = 'USD'.
*            TRY .
*                IF obj_nfe_inbound->zif_cadastro~check_env_aprov_taxa( ) EQ abap_true.
*                  " lva_migo = 'N'.
*                ENDIF.
*              CATCH zcx_cadastro INTO ex_cadastro.
*                ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
*            ENDTRY.
*          ENDIF.
*
*          TRY .
*              obj_nfe_inbound->set_aceitar_faturar( ).
*              TRY .
*                  IF obj_nfe_inbound->zif_cadastro~gravar_registro( ) EQ abap_true.
*                    LEAVE TO SCREEN 0.
*                  ENDIF.
*                CATCH zcx_cadastro INTO ex_cadastro.
*                  ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
*              ENDTRY.
*            CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
*              ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
*            CATCH zcx_cadastro INTO ex_cadastro.
*              ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
*            CATCH zcx_pedido_compra_exception INTO ex_pedido_compra.
*              ex_pedido_compra->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
*          ENDTRY.
*
*        WHEN abap_true.
*
** PBI - 64541 - Inicio - CBRAND
*          IF zde_nfe_dist_alv-ctr_waers = 'USD'.
*
**            SELECT * INTO TABLE it_zmmt0149
**              FROM zmmt0149
**             WHERE chave_nfe EQ zde_nfe_dist_alv-chave_nfe
**              AND status = 'L'.
**
**            IF it_zmmt0149[] IS NOT INITIAL.
**              MESSAGE 'Taxa do USD em aprovação' TYPE 'I'.
**            ELSE.
*            TRY .
*                IF obj_nfe_inbound->zif_cadastro~check_env_aprov_taxa( ) EQ abap_true.
*                  " lva_migo = 'N'.
*                ENDIF.
*              CATCH zcx_cadastro INTO ex_cadastro.
*                ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
*            ENDTRY.
**            ENDIF.
*          ENDIF.
** PBI - 64541 - Fim - CBRAND
*
*          CLEAR: ok_code.
*          "Verifica Informações para pagamento
*          obj_nfe_inbound->set_aceitar_faturar( i_ck_somente_validar = abap_true ).
*          IF obj_nfe_inbound->zif_cadastro~validar_registro( ) EQ abap_true.
*            LEAVE TO SCREEN 0.
*          ENDIF.
*
*      ENDCASE.
*
*      CLEAR: ok_code.
*-CS2025000249-17.04.2025-#173311-JT-fim

    WHEN ok_fatura_can.

      CLEAR: ok_code.

      FREE gv_open_text. "US #96438 RJF

      "Cancelar Fatura
      TRY.
          obj_nfe_inbound->nfe_inbound_cancela_fatura( ).

          PERFORM limpar_variaveis.
          PERFORM get_info_tela.
          PERFORM get_info_banco_parceiro.
          PERFORM get_info_valores.

        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        CATCH zcx_miro_exception INTO ex_miro.
          ex_miro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

  ENDCASE.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VERIFICAR_DATA_VENCIMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verificar_data_vencimento INPUT.

  TRY.
      CALL METHOD zcl_miro=>verificar_vencimento_fatura
        EXPORTING
          i_data_vencimento = zde_nfe_dist_alv-dt_vencimento
          i_pymt_meth       = zde_nfe_dist_alv-pymt_meth.
    CATCH zcx_miro_exception INTO ex_miro.
      ex_miro->published_erro( ).
  ENDTRY.

  obj_nfe_inbound->set_dt_vencimento( i_dt_vencimento = zde_nfe_dist_alv-dt_vencimento ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_BANCO_PARCEIRO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribuir_banco_parceiro INPUT.

  obj_nfe_inbound->set_banco_parceiro( i_bvtyp = zde_nfe_dist_alv-zbvtyp ).

  CLEAR: lc_info_forne.

  PERFORM get_info_banco_parceiro.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GET_INFO_BANCO_PARCEIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_banco_parceiro .

  DATA: lc_lfbk  TYPE lfbk,
        lc_bnka  TYPE bnka,
        lc_cte   TYPE zib_cte_dist_ter,
        lc_valor TYPE netwr_fp.

  IF zde_nfe_dist_alv-zbvtyp IS NOT INITIAL.

    lc_cte-p_emissor = zde_nfe_dist_alv-p_emissor.
    lc_cte-zbvtyp    = zde_nfe_dist_alv-zbvtyp.

    CALL METHOD zcl_cte_dist_g=>busca_banco_parceiro
      IMPORTING
        e_lfbk     = lc_lfbk
        e_bnka     = lc_bnka
      CHANGING
        p_cte      = lc_cte
      EXCEPTIONS
        erro_banco = 1
        OTHERS     = 2.

    IF sy-subrc IS INITIAL.
      lc_info_forne-bankl = lc_bnka-bankl(3).
      lc_info_forne-banka = lc_bnka-banka.
      lc_info_forne-bankn = lc_lfbk-bankn.

      IF NOT lc_lfbk-bkont IS INITIAL.
        CONCATENATE lc_lfbk-bankl+4(11) '-' lc_lfbk-bkont INTO lc_info_forne-agenc.
      ELSE.
        lc_info_forne-agenc = lc_lfbk-bankl+4(11).
      ENDIF.

      IF zde_nfe_dist_alv-pymt_meth IS INITIAL AND zde_nfe_dist_alv-housebankid IS INITIAL.

        MOVE zde_nfe_dist_alv-ctr_valor_total TO lc_valor.

        CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
          EXPORTING
            p_bukrs           = zde_nfe_dist_alv-e_tomadora
            p_lifnr           = zde_nfe_dist_alv-p_emissor
            p_zlsch           = zde_nfe_dist_alv-pymt_meth
            p_valor           = lc_valor
            p_bvtyp           = zde_nfe_dist_alv-zbvtyp
            p_waers           = zde_nfe_dist_alv-ctr_waers
          IMPORTING
            p_forma_pagamento = zde_nfe_dist_alv-pymt_meth
            p_princ_bnc_emp   = zde_nfe_dist_alv-housebankid
          EXCEPTIONS
            nao_fornecedor    = 1
            fornecedor_conta  = 2
            fornecedor_banco  = 3
            faixa_valor       = 4
            banco_empresa     = 5
            OTHERS            = 6.

        IF sy-subrc IS INITIAL.
          obj_nfe_inbound->set_meio_de_pagamento( EXPORTING i_meio_pagamento = zde_nfe_dist_alv-pymt_meth ).
          obj_nfe_inbound->set_banco_empresa( EXPORTING i_banco_empresa = zde_nfe_dist_alv-housebankid ).

          SELECT SINGLE text1 INTO lt_texto_forma_de_pagmento
            FROM t042z
           WHERE land1 EQ zde_nfe_dist_alv-land1
             AND zlsch EQ zde_nfe_dist_alv-pymt_meth.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1700  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1700 OUTPUT.

  CLEAR: it_ucomm.

*-US 154879-11-10-2024-#154879-RJF-inicio
  IF sy-ucomm EQ 'PROGRAMAR' AND gv_open_text IS INITIAL.
    gv_open_text = abap_on.
  ENDIF.
*-US 154879-11-10-2024-#154879-RJF-fim

  IF ck_entrada_estoque EQ abap_false.
    IF zde_nfe_dist_alv-st_fisico EQ zcl_nfe_inbound=>st_fisico_99.
      "Somente pode Cancelar
      APPEND ok_fatura_act TO it_ucomm.
    ELSE.
      APPEND ok_fatura_can TO it_ucomm.
      IF ( zde_nfe_dist_alv-st_fisico NE zcl_nfe_inbound=>st_fisico_migo_gerada ) AND
         ( zde_nfe_dist_alv-tp_compra_futura NE zcl_nfe_inbound=>tp_compra_futura_fatura OR zde_nfe_dist_alv-ck_fiscal EQ abap_false ).
        "Se MIGO não Gerada não pode Programar
        APPEND ok_fatura_act TO it_ucomm.
      ENDIF.
    ENDIF.
  ELSE.
    APPEND ok_fatura_can TO it_ucomm.
  ENDIF.

*-CS2025000249-19.05.2025-#175013-JT-inicio
  IF obj_nfe_inbound->get_ck_migo_cat_fiscal( ) = abap_true.
    APPEND ok_fatura_act TO it_ucomm.
    APPEND ok_fatura_can TO it_ucomm.
  ENDIF.
*-CS2025000249-19.05.2025-#175013-JT-fim

  LOOP AT SCREEN.
    IF screen-name(16) EQ 'ZDE_NFE_DIST_ALV'.
      SPLIT screen-name AT '-' INTO DATA(str1_1700) DATA(str2_1700).
      i_campo = str2_1700.
      "IF CK_ENTRADA_ESTOQUE EQ ABAP_TRUE.
      IF obj_nfe_inbound->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ) EQ abap_true.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      "ELSE.
      "  SCREEN-INPUT = 0.
      "ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*---------------------------------------------------------------------
  "Tratativa campo embarque
*---------------------------------------------------------------------

  LOOP AT SCREEN.
    IF screen-group4 = 'EMB'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'ZDE_NFE_DIST_ALV-DT_VENCIMENTO'.
      DATA(wa_screen) = screen.
    ENDIF.
  ENDLOOP.

  IF zde_nfe_dist_alv-ebeln IS NOT INITIAL.

    SELECT ebeln, bsart FROM ekko
      INTO TABLE @DATA(it_ekko)
*      FOR ALL ENTRIES IN @it_itens
       WHERE ebeln EQ @zde_nfe_dist_alv-ebeln "@it_itens-ebeln
       AND bsart IN ('ZFTE', 'ZEFI', 'ZSON').

    READ TABLE it_ekko INTO DATA(wa_ekko) WITH KEY bsart = 'ZFTE'.

    IF sy-subrc IS INITIAL.

      LOOP AT SCREEN.
        IF screen-group4 = 'EMB'.
          screen-invisible = '0'.
          screen-input = wa_screen-input.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    ENDIF.

    READ TABLE it_ekko INTO wa_ekko WITH KEY bsart = 'ZEFI'.

    IF sy-subrc IS INITIAL.

      LOOP AT SCREEN.
        IF screen-group4 = 'EMB'.
          screen-invisible = '0'.
          screen-input = wa_screen-input.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    ENDIF.

    READ TABLE it_ekko INTO wa_ekko WITH KEY bsart = 'ZSON'.

    IF sy-subrc IS INITIAL.

      SELECT * FROM zmmt0037
        INTO TABLE @DATA(it_zmmt0037)
*        FOR ALL ENTRIES IN @it_itens
         WHERE ebeln EQ @zde_nfe_dist_alv-ebeln "@it_itens-ebeln
*           AND ebelp EQ @it_itens-ebelp
           AND nro_sol_cp <> @space.

      IF it_zmmt0037[] IS NOT INITIAL.

        SELECT nro_sol_cp, ebeln FROM zmmt0035
          INTO TABLE @DATA(it_zmmt0035)
          FOR ALL ENTRIES IN @it_zmmt0037
          WHERE nro_sol_cp EQ @it_zmmt0037-nro_sol_cp.

      ENDIF.

      IF it_zmmt0035[] IS NOT INITIAL.

        SELECT ebeln bsart FROM ekko
          INTO TABLE it_ekko
          FOR ALL ENTRIES IN it_zmmt0035
          WHERE ebeln = it_zmmt0035-ebeln
           AND bsart IN ('ZFTE', 'ZEFI').

        IF it_ekko[] IS NOT INITIAL.

          LOOP AT SCREEN.
            IF screen-group4 = 'EMB'.
              screen-invisible = '0'.
              screen-input = wa_screen-input.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  IF ck_alterou_zlspr EQ abap_true AND zde_nfe_dist_alv-zlspr IS NOT INITIAL.

    SELECT SINGLE textl INTO lt_texto_bloqueio_pagmento
      FROM t008t
     WHERE spras EQ sy-langu
       AND zahls EQ zde_nfe_dist_alv-zlspr.

  ELSEIF zde_nfe_dist_alv-zlspr IS INITIAL.
    CLEAR: lt_texto_bloqueio_pagmento.
  ENDIF.

  IF ck_alterou_pymt_meth EQ abap_true AND zde_nfe_dist_alv-pymt_meth IS NOT INITIAL.

    SELECT SINGLE text1 INTO lt_texto_forma_de_pagmento
      FROM t042z
     WHERE land1 EQ zde_nfe_dist_alv-land1
       AND zlsch EQ zde_nfe_dist_alv-pymt_meth.

  ELSEIF zde_nfe_dist_alv-pymt_meth IS INITIAL.
    CLEAR: lt_texto_forma_de_pagmento.
  ENDIF.

  CLEAR: ck_alterou_zbvtyp,
         ck_alterou_zlspr,
         ck_alterou_pymt_meth,
         ck_alterou_housebankid,
         ck_alterou_vlr_desconto,
         ck_alterou_obs_financeira,
         ck_alterou_boleto.

  SET PF-STATUS 'PF1700' EXCLUDING it_ucomm.
  SET TITLEBAR 'TL1700'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GET_INFO_VALORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_valores .

  TRY .

      DATA: e_wkurs	      TYPE wkurs,
            e_valor_total	TYPE bapi_rmwwr.

      DATA(r_zmmt0075) = obj_nfe_inbound->get_config_tipo_pedido( ).

      obj_nfe_inbound->get_valor_nota_fiscal_fatura(
        IMPORTING
          e_waers       = zde_nfe_dist_alv-ctr_waers
          e_wkurs       = e_wkurs
          e_kufix       = zde_nfe_dist_alv-ctr_kufix
          e_sinal       = zde_nfe_dist_alv-ctr_sinal
          e_valor_total = e_valor_total
          e_zterm       = zde_nfe_dist_alv-ctr_zterm ).

      IF r_zmmt0075-ck_altera_valor EQ abap_false OR zde_nfe_dist_alv-ctr_wkurs IS INITIAL.
        zde_nfe_dist_alv-ctr_wkurs       = e_wkurs.
        zde_nfe_dist_alv-ctr_valor_total = e_valor_total.
        obj_nfe_inbound->set_ctr_valor_total( i_ctr_valor_total = zde_nfe_dist_alv-ctr_valor_total ).
      ENDIF.

      IF r_zmmt0075-zlspr IS NOT INITIAL AND zde_nfe_dist_alv-zlspr IS INITIAL.
        zde_nfe_dist_alv-zlspr = r_zmmt0075-zlspr.
        obj_nfe_inbound->set_bloqueio_pagamento( i_zlspr = r_zmmt0075-zlspr ).
      ENDIF.

      IF zde_nfe_dist_alv-zlspr IS NOT INITIAL.
        SELECT SINGLE textl INTO lt_texto_bloqueio_pagmento
          FROM t008t
         WHERE spras EQ sy-langu
           AND zahls EQ zde_nfe_dist_alv-zlspr.
      ENDIF.

      IF zde_nfe_dist_alv-pymt_meth IS NOT INITIAL.
        SELECT SINGLE text1 INTO lt_texto_forma_de_pagmento
          FROM t042z
         WHERE land1 EQ zde_nfe_dist_alv-land1
           AND zlsch EQ zde_nfe_dist_alv-pymt_meth.
      ENDIF.
    CATCH zcx_cadastro.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1700_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1700_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTERAR_CTR_VALOR_TOTAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterar_ctr_valor_total INPUT.
  zde_nfe_dist_alv-ctr_wkurs = obj_nfe_inbound->set_ctr_valor_total( EXPORTING i_ctr_valor_total = zde_nfe_dist_alv-ctr_valor_total ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_ZLSPR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_zlspr INPUT.
  ck_alterou_zlspr = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTERAR_BLOQUEIO_PAGAMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterar_bloqueio_pagamento INPUT.

  IF zde_nfe_dist_alv-zlspr IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(wa_t008)
      FROM t008
     WHERE zahls EQ @zde_nfe_dist_alv-zlspr.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e079 WITH zde_nfe_dist_alv-zlspr.
    ENDIF.
  ENDIF.

  obj_nfe_inbound->set_bloqueio_pagamento( i_zlspr = zde_nfe_dist_alv-zlspr ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_PYMT_METH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_pymt_meth INPUT.
  ck_alterou_pymt_meth = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_HOUSEBANKID  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_housebankid INPUT.
  ck_alterou_housebankid = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_VLR_DESCONTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_vlr_desconto INPUT.
  ck_alterou_vlr_desconto = abap_true.
  obj_nfe_inbound->set_vlr_desconto( EXPORTING i_vlr_desconto = zde_nfe_dist_alv-vlr_desconto ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_OBS_FINANCEIRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_obs_financeira INPUT.
  ck_alterou_obs_financeira = abap_true.
  obj_nfe_inbound->set_obs_financeira( EXPORTING i_obs_financeira = zde_nfe_dist_alv-obs_financeira ).
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  ALTEROU_BOLETO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_boleto INPUT.
  ck_alterou_boleto = abap_true.
  obj_nfe_inbound->set_boleto( i_boleto = zde_nfe_dist_alv-boleto ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_MEIO_PAGAMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribuir_meio_pagamento INPUT.
  obj_nfe_inbound->set_meio_de_pagamento( EXPORTING i_meio_pagamento = zde_nfe_dist_alv-pymt_meth ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_BANCO_EMPRESA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribuir_banco_empresa INPUT.
  obj_nfe_inbound->set_banco_empresa( EXPORTING i_banco_empresa = zde_nfe_dist_alv-housebankid ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_AUT_EMBARQUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_aut_embarque INPUT.

*-CS2019001896 - 05.01.2021 - inicio
  DATA: l_aut_embarque TYPE zde_aut_embarque,
        l_seqcam       TYPE numc3,
*        l_nrosol       TYPE numc5,      " RIM CS1029457 ANB 30.09.2022
        l_nrosol       TYPE zde_nro_sol, " RIM CS1029457 ANB 30.09.2022
        l_ebeln        TYPE ebeln,
        l_seq          TYPE numc3,
        l_filial_resp  TYPE vkbur,
        t_tab_emb      TYPE zsds060_t,
        l_chave        TYPE zde_chave_sol.

  l_aut_embarque                = obj_nfe_inbound->set_validar_aut_embarque( i_aut_embarque = zde_nfe_dist_alv-aut_embarque ).
  zde_nfe_dist_alv-aut_embarque = l_aut_embarque.

  obj_nfe_inbound->set_aut_embarque( i_aut_embarque = zde_nfe_dist_alv-aut_embarque ).

  IF l_aut_embarque IS INITIAL.
    EXIT.
*   MESSAGE e157.
  ENDIF.

  obj_nfe_inbound->set_validar_aut_embarque(
    EXPORTING
      i_aut_embarque = l_aut_embarque
    IMPORTING
      e_seq_cam      = l_seqcam
      e_nro_sol      = l_nrosol
      e_seq          = l_seq
      e_filial_resp  = l_filial_resp
      t_tab_embarque = t_tab_emb ).

  IF t_tab_emb[] IS NOT INITIAL.
    SELECT SINGLE nro_sol
             INTO l_nrosol
             FROM zsdt0138
            WHERE seq_cam     = l_seqcam
              AND nro_sol     = l_nrosol
              AND seq         = l_seq
              AND filial_resp = l_filial_resp.

    IF sy-subrc <> 0.
*      MESSAGE e159 WITH l_aut_embarque.
    ENDIF.

    SELECT * FROM zsdt0082
             INTO TABLE @DATA(t_zsdt0082)
            WHERE nro_sol   = @l_nrosol
              AND seq       = @l_seq.

    DELETE t_zsdt0082 WHERE NOT ( dt_canc  IS     INITIAL
                            AND   dt_liber IS NOT INITIAL ).

    READ TABLE t_zsdt0082 INTO DATA(w_zsdt0082) INDEX 1.

    IF sy-subrc = 0.
      "//Enqueue register;
      ck_nrosol  = w_zsdt0082-nro_sol.
      ck_seq     = w_zsdt0082-seq.

      CONCATENATE w_zsdt0082-nro_sol
                  w_zsdt0082-seq
                  w_zsdt0082-vbeln
                  w_zsdt0082-posnr INTO l_chave.

      CALL FUNCTION 'ZENQUEUE_SD_SOL_INSUMOS'
        EXPORTING
          chave          = l_chave
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4." DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.

*-Vinvulo pedido com OV
  IF l_nrosol IS NOT INITIAL.
    SELECT SINGLE ebeln
             INTO l_ebeln
             FROM zsdt0062
            WHERE nro_sol = l_nrosol
              AND ebeln   = zde_nfe_dist_alv-ebeln.
    IF sy-subrc <> 0.
      MESSAGE e160.
    ENDIF.
  ENDIF.
*-CS2019001896 - 05.01.2021 - fim
*
*select ebeln, bsart from ekko
*  into TABLE @data(it_ekko)
*  FOR ALL ENTRIES IN it_itens
*  where ebeln =
*    and ebelp =
*
*READ TABLE it_ekko INTO wa_ekko WITH KEY bsart = 'ZSON'.
*
*    IF sy-subrc IS INITIAL.
*
*      SELECT * FROM zmmt0037
*        INTO TABLE @DATA(it_zmmt0037)
*        FOR ALL ENTRIES IN @it_itens
*         WHERE ebeln EQ @it_itens-ebeln
*           AND ebelp EQ @it_itens-ebelp
*           AND nro_sol_cp <> @space.
*
*      IF it_zmmt0037[] IS NOT INITIAL.
*
*        SELECT nro_sol_cp, ebeln FROM zmmt0035
*          INTO TABLE @DATA(it_zmmt0035)
*          FOR ALL ENTRIES IN @it_zmmt0037
*          WHERE nro_sol_cp EQ @it_zmmt0037-nro_sol_cp.
*
*      ENDIF.
*
*      IF it_zmmt0035[] IS NOT INITIAL.
*
*        SELECT ebeln bsart FROM ekko
*          INTO TABLE it_ekko
*          FOR ALL ENTRIES IN it_zmmt0035
*          WHERE ebeln = it_zmmt0035-ebeln
*           AND bsart IN ('ZFTE', 'ZEFI').
*
*        IF it_ekko[] IS NOT INITIAL.
*
*          LOOP AT SCREEN.
*            IF screen-group4 = 'EMB'.
*              screen-invisible = '0'.
*              screen-input = wa_screen-input.
*              MODIFY SCREEN.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_PLACA_CAV  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_placa_cav INPUT.

*-CS2019001896 - 05.01.2021 - inicio
  obj_nfe_inbound->set_placa_cav( i_placa_cav = zde_nfe_dist_alv-placa_cav ).
*-CS2019001896 - 05.01.2021 - fim

ENDMODULE.

*-CS2025000249-08.04.2025-#173180-JT-inicio
*****************************************************
* salvar regisstro
*****************************************************
FORM f_salvar_registro.

  DATA: lv_ped_massa  TYPE zde_nfe_ped_massa,
        t_chaves      TYPE zde_chave_doc_e_t,
        lv_resp       TYPE c,
        w_nfe_inbound TYPE znfe_inbound.

  CLEAR: ok_code.
  CHECK ck_alterou_armazem EQ abap_false.
  CHECK ck_alterou_departamento EQ abap_false.
  CHECK ck_alterou_ck_possui_frete EQ abap_false.
  CHECK ck_alterou_transportador EQ abap_false.

  IF obj_nfe_inbound->get_pedido_massa( )-confirma_ped_massa = abap_true.
    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
        defaultoption  = 'N'
        diagnosetext1  = 'Serão Salvas TODAS as NFs selecionadas!'
        textline1      = 'Deseja Continuar?'
        titel          = 'Atenção'
        start_column   = 50
        start_row      = 10
        cancel_display = abap_false
      IMPORTING
        answer         = lv_resp
      EXCEPTIONS
        OTHERS         = 1.
    IF lv_resp = 'N'.
      RETURN.
    ENDIF.
  ENDIF.

  TRY .
*     IF obj_nfe_inbound->zif_cadastro~gravar_registro( ) EQ abap_true. "*-CS2025000249-08.04.2025-#173180-JT-inicio
*       LEAVE TO SCREEN 0.                                              "*-CS2025000249-08.04.2025-#173180-JT-inicio
*     ENDIF.                                                            "*-CS2025000249-08.04.2025-#173180-JT-inicio

      obj_nfe_inbound->zif_cadastro~gravar_registro( ).                 "*-CS2025000249-08.04.2025-#173180-JT-inicio
    CATCH zcx_cadastro INTO ex_cadastro.
      ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
  ENDTRY.

*---------------------------------------------
* quando for determinacao pedidos em massa
*---------------------------------------------
  IF obj_nfe_inbound->get_pedido_massa( )-confirma_ped_massa = abap_false.
    LEAVE TO SCREEN 0.
  ENDIF.

  t_chaves      = obj_nfe_inbound->get_chaves_lock( ).
  w_nfe_inbound = obj_nfe_inbound->get_info_nota( ).

  obj_nfe_inbound->set_pedido_massa( i_set_header = abap_true ).

  LOOP AT t_chaves    INTO DATA(lv_chave).
    IF lv_chave = w_nfe_inbound-nfe_base-chave_nfe.
      DELETE t_chaves INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  LOOP AT t_chaves INTO lv_chave.
    TRY .
        obj_nfe_inbound->desbloquear_objeto( ).
        obj_nfe_inbound->zif_cadastro~set_registro( i_id_registro = lv_chave i_nao_limpa = abap_true ).
        obj_nfe_inbound->get_pedido_massa( i_get_header = abap_true ).
        obj_nfe_inbound->zif_cadastro~gravar_registro( ).

      CATCH zcx_nfe_inbound_exception INTO DATA(nfe_inbound_exception).
        nfe_inbound_exception->published_erro( ).
      CATCH zcx_cadastro              INTO DATA(nfe_cadastro_exception).
        nfe_cadastro_exception->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
    ENDTRY.
  ENDLOOP.

  LEAVE TO SCREEN 0.

ENDFORM.

*****************************************************
* aceitar documento
*****************************************************
FORM f_aceitar_registro.

  DATA: lv_ped_massa  TYPE zde_nfe_ped_massa,
        t_chaves      TYPE zde_chave_doc_e_t,
        lv_resp       TYPE c,
        w_nfe_inbound TYPE znfe_inbound.

  CLEAR: ok_code.
  obj_nfe_inbound->zif_cadastro~ck_alterou = abap_true.
  CHECK ck_alterou_armazem EQ abap_false.
  CHECK ck_alterou_departamento EQ abap_false.
  CHECK ck_alterou_ck_possui_frete EQ abap_false.
  CHECK ck_alterou_transportador EQ abap_false.
  CHECK ck_alterou_nr_fase EQ abap_false.
  CHECK ck_alterou_dt_vencimento EQ abap_false.

  IF obj_nfe_inbound->get_pedido_massa( )-confirma_ped_massa = abap_true.
    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
        defaultoption  = 'N'
        diagnosetext1  = 'O Aceite será feito para TODAS as NFs selecionadas!'
        textline1      = 'Deseja Continuar?'
        titel          = 'Atenção'
        start_column   = 50
        start_row      = 10
        cancel_display = abap_false
      IMPORTING
        answer         = lv_resp
      EXCEPTIONS
        OTHERS         = 1.
    IF lv_resp = 'N'.
      RETURN.
    ENDIF.
  ENDIF.

  w_nfe_inbound = obj_nfe_inbound->get_info_nota( ).

  "Aceite Fiscal
  TRY .
      obj_nfe_inbound->set_aceitar_documento( ).
      TRY .
*         IF obj_nfe_inbound->zif_cadastro~gravar_registro( ) EQ abap_true. "*-CS2025000249-08.04.2025-#173180-JT-inicio
*           LEAVE TO SCREEN 0.                                              "*-CS2025000249-08.04.2025-#173180-JT-inicio
*         ENDIF.                                                            "*-CS2025000249-08.04.2025-#173180-JT-inicio

          obj_nfe_inbound->set_fiscal_taxas( ).                             "*-CS2025000249-08.04.2025-#173180-JT-inicio
          obj_nfe_inbound->zif_cadastro~gravar_registro( ).                 "*-CS2025000249-08.04.2025-#173180-JT-inicio
*         PERFORM f_calcular_taxas USING w_nfe_inbound-nfe_base-chave_nfe.  "*-CS2025000249-08.04.2025-#173180-JT-inicio

        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.

    CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ex_nfe_inbound->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
    CATCH zcx_pedido_compra_exception INTO ex_pedido_compra.
      ex_pedido_compra->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
  ENDTRY.

*---------------------------------------------
* quando for determinacao pedidos em massa
*---------------------------------------------
  IF obj_nfe_inbound->get_pedido_massa( )-confirma_ped_massa = abap_false.
    LEAVE TO SCREEN 0.
  ENDIF.

  t_chaves      = obj_nfe_inbound->get_chaves_lock( ).

  obj_nfe_inbound->set_pedido_massa( i_set_header = abap_true ).

  LOOP AT t_chaves    INTO DATA(lv_chave).
    IF lv_chave = w_nfe_inbound-nfe_base-chave_nfe.
      DELETE t_chaves INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  LOOP AT t_chaves INTO lv_chave.
    TRY .
        obj_nfe_inbound->desbloquear_objeto( ).
        obj_nfe_inbound->zif_cadastro~set_registro( i_id_registro = lv_chave i_nao_limpa = abap_true ).
        w_nfe_inbound = obj_nfe_inbound->get_info_nota( ).
        obj_nfe_inbound->get_pedido_massa( i_get_header = abap_true ).
        obj_nfe_inbound->set_aceitar_documento( ).
        obj_nfe_inbound->set_fiscal_taxas( ).                             "*-CS2025000249-08.04.2025-#173180-JT-inicio
        obj_nfe_inbound->zif_cadastro~gravar_registro( ).
*       PERFORM f_calcular_taxas USING w_nfe_inbound-nfe_base-chave_nfe.

      CATCH zcx_nfe_inbound_exception INTO DATA(nfe_inbound_exception).
        nfe_inbound_exception->published_erro( ).
      CATCH zcx_cadastro              INTO DATA(nfe_cadastro_exception).
        nfe_cadastro_exception->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
    ENDTRY.
  ENDLOOP.

  LEAVE TO SCREEN 0.

ENDFORM.

***************************************************************************************
* calcular taxas
***************************************************************************************
FORM f_calcular_taxas USING p_chave_nfe.

  DATA: t_itens TYPE TABLE OF ty_itens_alv WITH HEADER LINE,
        wa_item TYPE ty_itens_alv.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE t_itens
    FROM zib_nfe_dist_itm
   WHERE chave_nfe = p_chave_nfe.

  LOOP AT t_itens INTO wa_item.

    SELECT SINGLE * INTO @DATA(wa_ekko)
      FROM ekko
     WHERE ebeln EQ @wa_item-ebeln.

    IF wa_ekko-waers EQ 'BRL'.
      CONTINUE.
    ENDIF.

    ck_informado_preco = abap_false.

    IF wa_item-brtwr IS INITIAL.
      SELECT SINGLE * INTO @DATA(wa_zmmt0035)
        FROM zmmt0035
       WHERE ebeln EQ @wa_item-ebeln.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE nro_sol_cp, ebelp, brtwr, netpr, bicms, picms INTO @DATA(wa_zmmt0037)
          FROM zmmt0037
         WHERE nro_sol_cp EQ @wa_zmmt0035-nro_sol_cp
           AND ebelp      EQ @wa_item-ebelp.

        IF sy-subrc IS INITIAL.
          wa_item-brtwr   = wa_zmmt0037-brtwr.
          wa_item-bicms   = wa_zmmt0037-bicms.
          wa_item-picms   = wa_zmmt0037-picms.
        ENDIF.
      ELSE.
        SELECT SINGLE * INTO @DATA(wa_item_informado)
          FROM zib_nfe_dist_itm
         WHERE ebeln EQ @wa_item-ebeln
           AND ebelp EQ @wa_item-ebelp
           AND brtwr LT 0.

        IF sy-subrc IS INITIAL.
          wa_item-brtwr   = wa_item_informado-brtwr.
          wa_item-bicms   = wa_item_informado-bicms.
          wa_item-picms   = wa_item_informado-picms.
          wa_item-bpis    = wa_item_informado-bpis.
          wa_item-ppis    = wa_item_informado-ppis.
          wa_item-bcofins = wa_item_informado-bcofins.
          wa_item-pcofins = wa_item_informado-pcofins.
        ENDIF.
      ENDIF.
    ENDIF.

    zde_nfe_dist_itm_preco_ped-brtwr    = wa_item-brtwr.
    zde_nfe_dist_itm_preco_ped-bicms    = wa_item-bicms.
    zde_nfe_dist_itm_preco_ped-picms    = wa_item-picms.
    zde_nfe_dist_itm_preco_ped-bpis     = wa_item-bpis.
    zde_nfe_dist_itm_preco_ped-ppis     = wa_item-ppis.
    zde_nfe_dist_itm_preco_ped-bcofins  = wa_item-bcofins.
    zde_nfe_dist_itm_preco_ped-pcofins  = wa_item-pcofins.

    zde_nfe_dist_itm_preco_ped-liquido = ( ( zde_nfe_dist_itm_preco_ped-brtwr -
                            (
                              ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bicms   / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-picms   / 100 ) )  +
                              ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bpis    / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-ppis    / 100 ) )  +
                              ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bcofins / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-pcofins / 100 ) )
                            )
                          ) ).

    "Busca valor do pedido 04/04/2025 ALRS
    SELECT SINGLE *
             INTO @DATA(wa_item_ter)
             FROM zib_nfe_dist_ter
            WHERE ebeln EQ @wa_item_informado-chave_nfe.

    IF zde_nfe_dist_itm_preco_ped-brtwr  IS INITIAL.
      SELECT SINGLE netpr mwskz werks matnr
        INTO ( zde_nfe_dist_itm_preco_ped-brtwr, wa_item_ter-mwskz, wa_item_ter-f_tomadora, wa_item_informado-matnr )
        FROM ekpo
       WHERE ebeln  EQ wa_item-ebeln
         AND ebelp  EQ wa_item-ebelp.

      IF wa_item_ter-p_emissor IS INITIAL.
        SELECT SINGLE lifnr
          FROM lfa1
          INTO wa_item_ter-p_emissor
         WHERE stcd1 = wa_item_ter-forne_cnpj.
      ENDIF.
      IF wa_item_ter-p_emissor IS NOT INITIAL.
        SELECT SINGLE * INTO @DATA(wa_lfa1)
          FROM lfa1
         WHERE lifnr EQ @wa_item_ter-p_emissor.

        DATA wa_kna1             TYPE kna1.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_item_ter-f_tomadora
          IMPORTING
            output = wa_kna1-kunnr.

        SELECT SINGLE * INTO wa_kna1
          FROM kna1
         WHERE kunnr EQ wa_kna1-kunnr.

        CALL METHOD zcl_miro=>get_taxas_iva
          EXPORTING
            i_iva                   = wa_item_ter-mwskz
            i_data                  = wa_item_ter-dt_emissao
            i_bbranc                = wa_item_ter-f_tomadora
            i_shipfrom              = wa_lfa1-regio
            i_shipto                = wa_kna1-regio
            i_matnr                 = wa_item_informado-matnr
            i_icms_base             = wa_item_ter-vl_icms_base
            i_valor_icms            = wa_item_ter-vl_icms_total
            i_werks                 = wa_item_ter-f_tomadora
          IMPORTING
            e_rate_icms             = DATA(e_rate_icms)
            e_base_icms             = DATA(e_base_icms)
            e_rate_pis              = DATA(e_rate_pis)
            e_rate_cofins           = DATA(e_rate_cofins)
            e_rate_icms_diferencial = DATA(e_rate_icms_df).

        zde_nfe_dist_itm_preco_ped-bicms    = e_base_icms.
        zde_nfe_dist_itm_preco_ped-picms    = e_rate_icms.
        zde_nfe_dist_itm_preco_ped-bpis     = 100.
        zde_nfe_dist_itm_preco_ped-ppis     = e_rate_pis.
        zde_nfe_dist_itm_preco_ped-bcofins  = 100.
        zde_nfe_dist_itm_preco_ped-pcofins  = e_rate_cofins.
        zde_nfe_dist_itm_preco_ped-liquido  = ( ( zde_nfe_dist_itm_preco_ped-brtwr -
                                  (
                                  ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bicms   / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-picms   / 100 ) )  +
                                  ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bpis    / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-ppis    / 100 ) )  +
                                  ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bcofins / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-pcofins / 100 ) )
                                )
                              ) ).
      ENDIF.
    ENDIF.

    CLEAR: ck_informado_preco.

    LOOP AT it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_item>) WHERE ebeln EQ wa_item-ebeln
                                                             AND ebelp EQ wa_item-ebelp.

      <fs_item>-brtwr    = zde_nfe_dist_itm_preco_ped-brtwr.
      <fs_item>-bicms    = zde_nfe_dist_itm_preco_ped-bicms.
      <fs_item>-picms    = zde_nfe_dist_itm_preco_ped-picms.
      <fs_item>-bpis     = zde_nfe_dist_itm_preco_ped-bpis.
      <fs_item>-ppis     = zde_nfe_dist_itm_preco_ped-ppis.
      <fs_item>-bcofins  = zde_nfe_dist_itm_preco_ped-bcofins.
      <fs_item>-pcofins  = zde_nfe_dist_itm_preco_ped-pcofins.
      <fs_item>-liquido  = ( ( zde_nfe_dist_itm_preco_ped-brtwr -
                          (
                            ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bicms   / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-picms   / 100 ) )  +
                            ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bpis    / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-ppis    / 100 ) )  +
                            ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bcofins / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-pcofins / 100 ) )
                            )
                           ) ).

      CALL METHOD obj_nfe_inbound->set_item_material_preco
        EXPORTING
          i_prod_item    = <fs_item>-prod_item    " Nº item do documento
          i_preco_pedido = zde_nfe_dist_itm_preco_ped. " Inf. Pedido em Moeda Estrangeira

      UPDATE zib_nfe_dist_itm
         SET brtwr      = zde_nfe_dist_itm_preco_ped-brtwr
             bicms      = zde_nfe_dist_itm_preco_ped-bicms
             picms      = zde_nfe_dist_itm_preco_ped-picms
             bpis       = zde_nfe_dist_itm_preco_ped-bpis
             ppis       = zde_nfe_dist_itm_preco_ped-ppis
             bcofins    = zde_nfe_dist_itm_preco_ped-bcofins
             pcofins    = zde_nfe_dist_itm_preco_ped-pcofins
             liquido    = zde_nfe_dist_itm_preco_ped-liquido
       WHERE chave_nfe EQ wa_item-chave_nfe
         AND prod_item EQ wa_item-prod_item.

    ENDLOOP.
  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.
*-CS2025000249-08.04.2025-#173180-JT-fim

*-CS2025000249-17.04.2025-#173311-JT-inicio
**********************************************************************
* GERAR FATURA
**********************************************************************
FORM f_gerar_fatura_act.

*-US 96438-17-09-2024-#96438-RJF-inicio
  IF gv_open_text EQ abap_on AND zde_nfe_dist_alv-aut_embarque IS NOT INITIAL.
    IF zde_nfe_dist_alv-chave_nfe IS NOT INITIAL.

      DATA: ls_xml_nfe TYPE znfe_xml_sefaz_auth.

      CALL FUNCTION 'Z_DETALHAMENTO_NFE'
        EXPORTING
          i_chave_nfe = zde_nfe_dist_alv-chave_nfe
        IMPORTING
          e_xml_nfe   = ls_xml_nfe
        EXCEPTIONS
          no_found    = 1
          OTHERS      = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
        MESSAGE 'Detalhamento da NFe não encontrado!' TYPE 'I'.
      ELSE.

        FREE: it_det,
              lv_infcpl,
              lv_infadfisco.

        it_det = ls_xml_nfe-nfeproc-nfe-infnfe-det[]. "-INFADPROD
        lv_infcpl = ls_xml_nfe-nfeproc-nfe-infnfe-infadic-infcpl.
        lv_infadfisco = ls_xml_nfe-nfeproc-nfe-infnfe-infadic-infadfisco.

        "Chamar Tela
        CALL SCREEN 1800 STARTING AT 30 01.
        IF sy-ucomm EQ 'CANCELAR'.
*              FREE gv_open_text.
          EXIT.
        ELSEIF sy-ucomm EQ 'SALVAR'.
          IF lv_error IS NOT INITIAL.
            DATA: lv_ans.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = 'Continuar Programar Pagamento?'
                text_question         = 'Error: textos não foram salvos!'
                text_button_1         = 'Sim'
                icon_button_1         = 'ICON_CHECKED'
                text_button_2         = 'Não'
                icon_button_2         = 'ICON_CANCEL'
                display_cancel_button = ' '
                popup_type            = 'ICON_MESSAGE_ERROR'
              IMPORTING
                answer                = lv_ans.
            IF lv_ans = 2.
*                  FREE gv_open_text.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*      FREE gv_open_text.

  BREAK rfreitas.
*-US 96438-17-09-2024-#96438-RJF-fim

  CHECK ck_alterou_zbvtyp         EQ abap_false.
  CHECK ck_alterou_zlspr          EQ abap_false.
  CHECK ck_alterou_pymt_meth      EQ abap_false.
  CHECK ck_alterou_housebankid    EQ abap_false.
  CHECK ck_alterou_vlr_desconto   EQ abap_false.
  CHECK ck_alterou_obs_financeira EQ abap_false.
  CHECK ck_alterou_boleto         EQ abap_false.

  CASE ck_entrada_estoque.
    WHEN abap_false.
      "Aceite Fatura
      IF zde_nfe_dist_alv-ctr_waers = 'USD'.
        TRY .
            IF obj_nfe_inbound->zif_cadastro~check_env_aprov_taxa( ) EQ abap_true.
              " lva_migo = 'N'.
            ENDIF.
          CATCH zcx_cadastro INTO ex_cadastro.
            ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        ENDTRY.
      ENDIF.

      TRY .
          obj_nfe_inbound->set_aceitar_faturar( ).
          TRY .
              IF obj_nfe_inbound->zif_cadastro~gravar_registro( ) EQ abap_true.
                IF obj_nfe_inbound->get_ck_miro_automatica( ) = abap_false.  "*-CS2025000249-17.04.2025-#173311-JT
                  LEAVE TO SCREEN 0.
                ENDIF.
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

    WHEN abap_true.

* PBI - 64541 - Inicio - CBRAND
      IF zde_nfe_dist_alv-ctr_waers = 'USD'.

*            SELECT * INTO TABLE it_zmmt0149
*              FROM zmmt0149
*             WHERE chave_nfe EQ zde_nfe_dist_alv-chave_nfe
*              AND status = 'L'.
*
*            IF it_zmmt0149[] IS NOT INITIAL.
*              MESSAGE 'Taxa do USD em aprovação' TYPE 'I'.
*            ELSE.
        TRY .
            IF obj_nfe_inbound->zif_cadastro~check_env_aprov_taxa( ) EQ abap_true.
              " lva_migo = 'N'.
            ENDIF.
          CATCH zcx_cadastro INTO ex_cadastro.
            ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        ENDTRY.
*            ENDIF.
      ENDIF.
* PBI - 64541 - Fim - CBRAND

      CLEAR: ok_code.
      "Verifica Informações para pagamento
      obj_nfe_inbound->set_aceitar_faturar( i_ck_somente_validar = abap_true ).
      IF obj_nfe_inbound->zif_cadastro~validar_registro( ) EQ abap_true.
        LEAVE TO SCREEN 0.
      ENDIF.

  ENDCASE.

  CLEAR: ok_code.

ENDFORM.

**********************************************************************
* efetuar faturamento
**********************************************************************
FORM f_efetuar_faturamento.

*-CS2025000249-19.05.2025-#175013-JT-inicio
  IF obj_nfe_inbound->get_ck_migo_cat_fiscal( ) = abap_true.
    RETURN.
  ENDIF.
*-CS2025000249-19.05.2025-#175013-JT-fim

  IF ck_alterou_zlspr EQ abap_true AND zde_nfe_dist_alv-zlspr IS NOT INITIAL.
    SELECT SINGLE textl
      INTO lt_texto_bloqueio_pagmento
      FROM t008t
     WHERE spras EQ sy-langu
       AND zahls EQ zde_nfe_dist_alv-zlspr.
  ELSEIF zde_nfe_dist_alv-zlspr IS INITIAL.
    CLEAR: lt_texto_bloqueio_pagmento.
  ENDIF.

  IF ck_alterou_pymt_meth EQ abap_true AND zde_nfe_dist_alv-pymt_meth IS NOT INITIAL.
    SELECT SINGLE text1
      INTO lt_texto_forma_de_pagmento
      FROM t042z
     WHERE land1 EQ zde_nfe_dist_alv-land1
       AND zlsch EQ zde_nfe_dist_alv-pymt_meth.
  ELSEIF zde_nfe_dist_alv-pymt_meth IS INITIAL.
    CLEAR: lt_texto_forma_de_pagmento.
  ENDIF.

  CLEAR: ck_alterou_zbvtyp,
         ck_alterou_zlspr,
         ck_alterou_pymt_meth,
         ck_alterou_housebankid,
         ck_alterou_vlr_desconto,
         ck_alterou_obs_financeira,
         ck_alterou_boleto.

*---------------------------------------------------
* alterar_bloqueio_pagamento INPUT.
*---------------------------------------------------
  IF zde_nfe_dist_alv-zlspr IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(wa_t008)
      FROM t008
     WHERE zahls EQ @zde_nfe_dist_alv-zlspr.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s079 WITH zde_nfe_dist_alv-zlspr DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDIF.

  obj_nfe_inbound->set_bloqueio_pagamento( i_zlspr = zde_nfe_dist_alv-zlspr ).

*---------------------------------------------------
* alterar_ctr_valor_total
*---------------------------------------------------
  zde_nfe_dist_alv-ctr_wkurs = obj_nfe_inbound->set_ctr_valor_total( EXPORTING i_ctr_valor_total = zde_nfe_dist_alv-ctr_valor_total ).

*---------------------------------------------------
* verificar_data_vencimento
*---------------------------------------------------
  TRY.
      CALL METHOD zcl_miro=>verificar_vencimento_fatura
        EXPORTING
          i_data_vencimento = zde_nfe_dist_alv-dt_vencimento
          i_pymt_meth       = zde_nfe_dist_alv-pymt_meth.
    CATCH zcx_miro_exception INTO ex_miro.
      ex_miro->published_erro( ).
  ENDTRY.

  obj_nfe_inbound->set_dt_vencimento( i_dt_vencimento = zde_nfe_dist_alv-dt_vencimento ).

*---------------------------------------------------
* atribuir_banco_parceiro
*---------------------------------------------------
  obj_nfe_inbound->set_banco_parceiro( i_bvtyp = zde_nfe_dist_alv-zbvtyp ).

  CLEAR: lc_info_forne.
  PERFORM get_info_banco_parceiro.

*---------------------------------------------------
* atribuicao valores na classe
*---------------------------------------------------
  obj_nfe_inbound->set_meio_de_pagamento( EXPORTING i_meio_pagamento = zde_nfe_dist_alv-pymt_meth ).
  obj_nfe_inbound->set_banco_empresa( EXPORTING i_banco_empresa = zde_nfe_dist_alv-housebankid ).
  obj_nfe_inbound->set_vlr_desconto( EXPORTING i_vlr_desconto = zde_nfe_dist_alv-vlr_desconto ).
  obj_nfe_inbound->set_obs_financeira( EXPORTING i_obs_financeira = zde_nfe_dist_alv-obs_financeira ).
  obj_nfe_inbound->set_boleto( i_boleto = zde_nfe_dist_alv-boleto ).
  obj_nfe_inbound->set_aut_embarque( i_aut_embarque = zde_nfe_dist_alv-aut_embarque ).
  obj_nfe_inbound->set_placa_cav( i_placa_cav = zde_nfe_dist_alv-placa_cav ).

*---------------------------------------------------
* gerar fatura
*---------------------------------------------------
  PERFORM f_gerar_fatura_act.

ENDFORM.
*-CS2025000249-17.04.2025-#173311-JT-fim

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
