*----------------------------------------------------------------------*
***INCLUDE Z_1BNFE_MONITOR_STATUS_0103O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0103 OUTPUT.
  DATA: vl_parvw TYPE c LENGTH 2.

  SET PF-STATUS 'SCREEN_103'.
  SET TITLEBAR  'TITLE_103'.

  vl_parvw = wg_corr_parc-parvw(2).

  CASE vl_parvw.
    WHEN 'LR' OR 'PC' OR 'Z1'.

      LOOP AT SCREEN.
        CASE screen-name.
          WHEN 'WG_CORR_PARC-NEW_PARID_V'.

            IF ( vl_parvw = 'PC' ) OR
               ( vl_parvw = 'Z1' ).
              screen-input = 1.
            ELSE.
              screen-input = 0.
              CLEAR: wg_corr_parc-new_parid_v.
            ENDIF.

            MODIFY SCREEN.

          WHEN 'WG_CORR_PARC-NEW_PARID_C'.

            IF ( vl_parvw = 'LR' ).
              screen-input = 1.
            ELSE.
              screen-input = 0.
              CLEAR: wg_corr_parc-new_parid_c.
            ENDIF.

            MODIFY SCREEN.
        ENDCASE.

      ENDLOOP.

  ENDCASE.


ENDMODULE.                 " STATUS_0103  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.

  IF g_custom_cont_desc IS INITIAL.

    CREATE OBJECT g_custom_cont_desc
      EXPORTING
        container_name = g_descbox.


    CREATE OBJECT obg_descbox
      EXPORTING
        parent            = g_custom_cont_desc
        wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position = 72
        max_number_chars  = 1000.

    CALL METHOD obg_descbox->set_toolbar_mode
      EXPORTING
        toolbar_mode = '0'.
  ENDIF.

  PERFORM atualiza_txt_corr_parc.

ENDMODULE.                    "cria_objetos OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0103 INPUT.
  CASE sy-ucomm .
    WHEN 'BTNCANC'.
      LEAVE TO SCREEN 0.
    WHEN 'BTNOK'.
      PERFORM lanc_carta_correcao.
      LEAVE TO SCREEN 0.
    WHEN 'ADD_CORR_PARC'.
      PERFORM add_corr_parc.
    WHEN 'DEL_CORR_PARC'.
      PERFORM del_corr_parc.
    WHEN 'SRC_CORR_PARC'.
      PERFORM get_dados_parc_nf_cce.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0103  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0104 INPUT.
  CASE sy-ucomm .
    WHEN 'BTNSAIR'.
      DATA(_erro) = 'N'.
      LOOP AT tl_carta_correc INTO sl_carta_correc.
        IF sl_carta_correc-dt_authcod = sy-datum AND
           sl_carta_correc-authcode IS NOT INITIAL.
          SELECT SINGLE *
            FROM zcarta_correcao
            INTO @DATA(_carta)
            WHERE docnum = @sl_carta_correc-docnum
            AND   id_cc  = @sl_carta_correc-id_cc.
          IF _carta-novo_terminal IS NOT INITIAL AND
             _carta-doc_material IS INITIAL.
            _erro = 'S'.
            MESSAGE 'Para troca de terminal faça a transf.de estoque!!' TYPE 'E'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF _erro NE 'S'.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'BTNNOVO'.

      PERFORM load_parc_cce_nf.

      CALL SCREEN 0103 STARTING AT 04 02 ENDING AT 90 25.
      "leave to screen 0.
    WHEN 'BTNATUALIZAR'.
      PERFORM: zsel_carta_correc,
               zrefresh_alv_cc.
    WHEN 'BTNIMPRIMIR'  .
      PERFORM: zimp_carta_correc.
    WHEN: 'BTN_TRANS_ESTOQUE'.
      PERFORM: trans_estoque_carta_correc.
    WHEN: 'BTN_CHECAR_DISP'.
      DATA(_disponivel) = ''.
      PERFORM: verificar_disp_transf CHANGING _disponivel.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0104 OUTPUT.
  SET PF-STATUS 'SCREEN_104'.
  SET TITLEBAR  'TITLE_104'.

ENDMODULE.                 " STATUS_0104  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  STATUS_0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows_cc.

  CLEAR it_selected_rows_cc.
  CALL METHOD wa_alv_cc->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows_cc.

  CLEAR tl_carta_correc_selection.
  LOOP AT it_selected_rows_cc INTO wa_selected_rows_cc.

    READ TABLE tl_carta_correc INTO sl_carta_correc INDEX wa_selected_rows_cc-index.

    "move-corresponding sl_carta_correc to wa_alv_selection_cc.
    APPEND sl_carta_correc TO tl_carta_correc_selection.
  ENDLOOP.

ENDMODULE.                    "get_selected_rows_cc

*----------------------------------------------------------------------*
*  MODULE GET_SELECTED_ROWS_CC_C
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE get_selected_rows_cc_c.

  CLEAR it_selected_rows_cc.
  CALL METHOD ctl_alv_cte_resu->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows_cc.

  CLEAR tl_carta_correc_selection.
  LOOP AT it_selected_rows_cc INTO wa_selected_rows_cc.

    READ TABLE tl_carta_correc INTO sl_carta_correc INDEX wa_selected_rows_cc-index.

    "move-corresponding sl_carta_correc to wa_alv_selection_cc.
    APPEND sl_carta_correc TO tl_carta_correc_selection.
  ENDLOOP.

ENDMODULE.                    "get_selected_rows_cc
*&---------------------------------------------------------------------*
*&      Module  PBO_0105  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0105 OUTPUT.
  SET PF-STATUS 'SCREEN_105'.
  SET TITLEBAR  'TITLE_105'.
  LOOP AT SCREEN.
    IF gf_opt_dep = 'X'.
      IF screen-name EQ 'TXT_CENTRO_DEST' OR
         screen-name EQ 'GF_WERKS_D'.
        screen-input     = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " PBO_0105  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0105  INPUT
*&---------------------------------------------------------------------*

MODULE pai_0105 INPUT.
  CASE sy-ucomm.
    WHEN: 'OK'.

      IF gf_inf_dados_transf_cce IS NOT INITIAL. "Somente Informação de Dados
        CLEAR: gf_inf_dados_transf_cce.
        LEAVE TO SCREEN 0.
      ENDIF.

      PERFORM: transf_estoque.

    WHEN: 'DEPCEN'.

    WHEN: 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " PAI_0105  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0106  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0106 OUTPUT.
*  DATA: FCODE TYPE TABLE OF SY-UCOMM.
*  "REFRESH: FCODE.
*  IF WL_DESACTIVE = 'X'.
*    APPEND 'OK' TO FCODE.
*  ENDIF.
  "SET PF-STATUS 'SCREEN_106' EXCLUDING FCODE.
  SET PF-STATUS 'SCREEN_106'.
  SET TITLEBAR 'TITLE_106'.
ENDMODULE.                 " STATUS_0106  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0106  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0106 INPUT.

  DATA: t_fieldcatalog TYPE lvc_t_fcat,
        t_fieldcat_edt TYPE lvc_t_fcat,
        w_fieldcatalog TYPE lvc_s_fcat.

  IF ( sy-ucomm IS INITIAL ).
    sy-ucomm = ok_code.
  ENDIF.

  CASE sy-ucomm .
    WHEN 'CANC'.
      "CALL METHOD OBG_DESCCTE->SET_READONLY_MODE
      "  EXPORTING
      "    READONLY_MODE = 1.
      "REFRESH TG_EDITOR.
      wl_desactive = 'X'.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.
      CALL METHOD ctl_alv_cte_corr->check_changed_data.
      PERFORM grava_cte_correcao.
    WHEN 'NOVO'.
      wl_desactive = ''.
      REFRESH tg_editor.
      REFRESH it_cte_corr.
      "CALL METHOD OBG_DESCCTE->SET_TEXT_AS_R3TABLE
      "  EXPORTING
      "    TABLE = TG_EDITOR.
      "CALL METHOD OBG_DESCCTE->SET_READONLY_MODE
      "  EXPORTING
      "    READONLY_MODE = 0.
      CALL METHOD ctl_alv_cte_corr->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.
    WHEN 'RENOVA'.
      PERFORM: zsel_carta_correc.
    WHEN 'EXIBIR'  .
      PERFORM busca_tags_cte.
    WHEN 'IMPRIMIR'.
      PERFORM: zimp_carta_correc.

  ENDCASE.

*  CASE SY-UCOMM.
*
*    WHEN: 'CCORR_CTE' OR 'RENOVA'.
*
*      CALL METHOD CTL_ALV_CTE_CORR->GET_FRONTEND_FIELDCATALOG
*        IMPORTING
*          ET_FIELDCATALOG = T_FIELDCATALOG.
*
*
*      IF ( WA_CTE_CORR-GRUPO EQ 'veic' ).
*
*        LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
*          IF W_FIELDCATALOG-FIELDNAME EQ 'PC_VEICULO'.
*            W_FIELDCATALOG-NO_OUT = SPACE.
*          ENDIF.
*
*          MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG TRANSPORTING NO_OUT.
*        ENDLOOP.
*
*
*
*      ELSEIF ( WA_CTE_CORR-GRUPO EQ 'infNFe' ).
*
*        LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
*          IF W_FIELDCATALOG-FIELDNAME EQ 'CHAVE'.
*            W_FIELDCATALOG-NO_OUT = SPACE.
*          ENDIF.
*          MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG TRANSPORTING NO_OUT.
*        ENDLOOP.
*
*
*      ENDIF.
*
*      CALL METHOD CTL_ALV_CTE_CORR->SET_FRONTEND_FIELDCATALOG
*        EXPORTING
*          IT_FIELDCATALOG = T_FIELDCATALOG.
*
*
*  ENDCASE.





ENDMODULE.                 " USER_COMMAND_0106  INPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECTS_0106  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_objects_0106 OUTPUT.

*  IF G_CUSTOM_CONT_CTE IS INITIAL.
*
*    CREATE OBJECT G_CUSTOM_CONT_CTE
*      EXPORTING
*        CONTAINER_NAME = G_DESCBOX.
*
*
*    CREATE OBJECT OBG_DESCCTE
*      EXPORTING
*        PARENT            = G_CUSTOM_CONT_CTE
*        WORDWRAP_MODE     = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
*        WORDWRAP_POSITION = 72
*        MAX_NUMBER_CHARS  = 1000.
*
*    CALL METHOD OBG_DESCCTE->SET_TOOLBAR_MODE
*      EXPORTING
*        TOOLBAR_MODE = '0'.
*
*    CALL METHOD OBG_DESCCTE->SET_READONLY_MODE
*      EXPORTING
*        READONLY_MODE = 1.
*
*  ENDIF.



  IF ctl_cccontainer3 IS INITIAL.

    CREATE OBJECT ctl_cccontainer3
      EXPORTING
        container_name = 'CC_CORRECAO'.
*   Create object for ALV grid inside container
    CREATE OBJECT ctl_alv_cte_corr
      EXPORTING
        i_parent = ctl_cccontainer3.


*   Fill field catalog 3
    PERFORM fill_it_fieldcatalog3.

*    "Capturar as placas que estão vinculadas no CTE.
*    PERFORM: CAPTURAR_PLACAS.
*    PERFORM: CAPTURAR_CHAVES.

*   Set layout parameters for ALV grid3
    gs_layout3-grid_title = 'Carta de Correção - CTE'.
    gs_layout3-stylefname = 'FIELD_STYLE'.
    gs_layout3-zebra      = 'X'.

    gs_variant_cte-report      = sy-repid.
*   Send data to ALV grid

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = ctl_alv_cte_corr.

*      * Register event handler
    wl_desactive = 'X'.
    SET HANDLER obg_toolbar->on_toolbar FOR ctl_alv_cte_corr.
    SET HANDLER obg_toolbar->handle_user_command FOR ctl_alv_cte_corr.

    REFRESH: it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    CALL METHOD ctl_alv_cte_corr->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout3
        is_variant           = gs_variant_cte
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_fieldcatalog      = it_fieldcatalog3
        it_outtab            = it_cte_corr.


    CALL METHOD ctl_alv_cte_corr->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    REFRESH gt_f4.

    gt_f4-fieldname = 'CAMPO'.
    gt_f4-register = 'X'.
    gt_f4-getbefore = 'X'.
    gt_f4-chngeafter ='X'.
    APPEND gt_f4.

    gt_f4-fieldname = 'GRUPO'.
    gt_f4-register = 'X'.
    gt_f4-getbefore = 'X'.
    gt_f4-chngeafter ='X'.
    APPEND gt_f4.

    CALL METHOD ctl_alv_cte_corr->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4[].

    SET HANDLER: lcl_event_handler_0106=>on_f4 FOR ctl_alv_cte_corr,
                 lcl_event_handler_0106=>on_data_changed FOR ctl_alv_cte_corr.


    CLEAR gf_first_display_0106.
  ELSE.
    CALL METHOD ctl_alv_cte_corr->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  "Capturar as placas que estão vinculadas no CTE.
  PERFORM: capturar_placas.
  PERFORM: capturar_chaves.

  " Resumo das Cartas de Correções
  IF ctl_cccontainer4 IS INITIAL.
    CREATE OBJECT ctl_cccontainer4
      EXPORTING
        container_name = 'CC_HIST'.
*   Create object for ALV grid inside container
    CREATE OBJECT ctl_alv_cte_resu
      EXPORTING
        i_parent = ctl_cccontainer4.


    gs_layout4-grid_title = 'Cartas de Correção criadas'.
    gs_layout4-no_toolbar = 'X'.

    gs_variant_cte-report      = sy-repid.
*   Send data to ALV grid

    REFRESH: it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    CALL METHOD ctl_alv_cte_resu->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout4
        is_variant           = gs_variant_cte
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_outtab            = tl_carta_correc
        it_fieldcatalog      = it_fcat.

    CALL METHOD ctl_alv_cte_resu->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD ctl_alv_cte_corr->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.
    CALL METHOD ctl_alv_cte_resu->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
ENDMODULE.                 " CREATE_OBJECTS_0106  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_RESU  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows_resu INPUT.
  CLEAR it_selected_rows_cc.
  CALL METHOD ctl_alv_cte_resu->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows_cc.

  CLEAR tl_carta_correc_selection.
  LOOP AT it_selected_rows_cc INTO wa_selected_rows_cc.

    READ TABLE tl_carta_correc INTO sl_carta_correc INDEX wa_selected_rows_cc-index.

    "move-corresponding sl_carta_correc to wa_alv_selection_cc.
    APPEND sl_carta_correc TO tl_carta_correc_selection.
  ENDLOOP.

ENDMODULE.                 " GET_SELECTED_ROWS_RESU  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0107  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0107 OUTPUT.
  DATA: tg_fcode TYPE TABLE OF sy-ucomm WITH HEADER LINE.
  DATA: tl_parametros TYPE ustyp_t_parameters.

  REFRESH: tg_fcode, tl_parametros.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = tl_parametros
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.

  READ TABLE tl_parametros INTO DATA(wl_parametros) WITH KEY parid = 'ZENC_MDFE'.
  IF sy-subrc NE 0.
    tg_fcode = 'ENC_MDFE'.
    APPEND tg_fcode.
  ENDIF.

  CASE sy-tcode.
    WHEN 'ZCTE'.
      SET PF-STATUS 'PF0107' EXCLUDING tg_fcode.
    WHEN 'ZNFE'.
      SET PF-STATUS 'PF0107' EXCLUDING tg_fcode.
    WHEN 'ZMDFE'.
      SET PF-STATUS 'PF0107' EXCLUDING tg_fcode.
  ENDCASE.

  SET TITLEBAR  'TB0107'.
ENDMODULE.                 " STATUS_0107  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0107  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0107 INPUT.

  CASE sy-ucomm.
    WHEN: 'FECHAR'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN: 'INF_TRANSP'.
      CLEAR: sy-ucomm.
      PERFORM inf_transp_mdfe.
    WHEN: 'GRAVAR'.
      CLEAR: sy-ucomm.
      PERFORM: gravar_mdfe.
    WHEN: 'EMITIR'.
      CLEAR: sy-ucomm.
      PERFORM: emitir_mdfe.
    WHEN: 'CANC_MDFE'.
      CLEAR: sy-ucomm.
      PERFORM: cancelar_mdfe.
    WHEN: 'ENC_MDFE'.
      CLEAR: sy-ucomm.
      PERFORM: encerrar_mdfe.
    WHEN: 'ATUAL_MDFE'.
      CLEAR: sy-ucomm.
      PERFORM: atualizar_mdfe.
    WHEN: 'PRINT_MDFE'.
      CLEAR: sy-ucomm.
      PERFORM: print_mdfe.
    WHEN: 'NOVO'.
      CLEAR: sy-ucomm.
      PERFORM: novo_mdfe.
    WHEN: 'ESTORNAR'.
      CLEAR: sy-ucomm.
      PERFORM: estornar_mdfe.
    WHEN 'ENC_POR'.

      CLEAR: sy-ucomm.

      IF gw_mdfe_status-docnum IS NOT INITIAL.
        SELECT SINGLE *
          FROM zsdt0102 INTO @DATA(_wl_zsdt0102)
         WHERE docnum EQ @gw_mdfe_status-docnum.

        IF sy-subrc NE 0.
          MESSAGE 'Registro MDF-e não encontrado!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF _wl_zsdt0102-encerrado IS INITIAL.
          MESSAGE 'Registro MDF-e não esta encerrado!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF _wl_zsdt0102-motivo_enc IS NOT INITIAL.
          CONCATENATE 'Motivo encerramento:' _wl_zsdt0102-motivo_enc INTO DATA(LVA_MOTIVO_ENC) SEPARATED BY SPACE.
          MESSAGE LVA_MOTIVO_ENC TYPE 'I'.
        ENDIF.

        IF _wl_zsdt0102-sol_enc_aut IS NOT INITIAL.
          MESSAGE 'Registro MDF-e encerrado automaticamente !' TYPE 'S'.
          EXIT.
        ENDIF.

        IF _wl_zsdt0102-doc_mdfe_sol_enc IS INITIAL.
          MESSAGE 'Não encontrado documento de encerramento!' TYPE 'S'.
          EXIT.
        ENDIF.

        SELECT SINGLE *
          FROM zsdt0105 INTO @DATA(_wl_doc_enc)
         WHERE docnum_ref = @_wl_zsdt0102-doc_mdfe_sol_enc.

        IF sy-subrc NE 0.
          MESSAGE 'Não encontrado documento de encerramento!' TYPE 'S'.
          EXIT.
        ENDIF.

        CONCATENATE 'MDF-e foi encerrado pelo MDF-e Documento:' _wl_doc_enc-docnum_ref ' e CTe Documento: ' _wl_doc_enc-docnum
               INTO DATA(_msg_enc) SEPARATED BY space.

        MESSAGE _msg_enc TYPE 'I'.

      ENDIF.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0107  INPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_0107  OUTPUT
*&---------------------------------------------------------------------*
MODULE create_alv_0107 OUTPUT.

  DATA: ls_layout_mdfe      TYPE lvc_s_layo,
        ls_layout_uf_perc   TYPE lvc_s_layo,
        ls_layout_hist_mdfe TYPE lvc_s_layo,
        ls_layout_mdfe_enc  TYPE lvc_s_layo,
        gs_variant_up       TYPE disvariant,
        var_len             TYPE i,
        wa_zsdt0105         TYPE zsdt0105.

  IF ( gc_container_mdfe IS INITIAL ).

    "-----------------------------------------------------------------
    " Documentos
    "-----------------------------------------------------------------
    CREATE OBJECT gc_container_mdfe
      EXPORTING
        container_name = 'CONTAINER_MDFE'.

    CREATE OBJECT gc_alv_mdfe
      EXPORTING
        i_parent = gc_container_mdfe.

    PERFORM: create_catalog_mdfe.

    ls_layout_mdfe-no_toolbar = 'X'.

    CALL METHOD gc_alv_mdfe->set_table_for_first_display
      EXPORTING
        is_layout       = ls_layout_mdfe
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = gt_catalog_mdfe
        it_outtab       = gt_mdfe.

    CALL METHOD gc_alv_mdfe->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    "---------------------------------------------------------------
    " UF Percurso
    "---------------------------------------------------------------

    CREATE OBJECT gc_container_uf_perc
      EXPORTING
        container_name = 'CC_UF_PERC'.

    CREATE OBJECT gc_alv_uf_perc
      EXPORTING
        i_parent = gc_container_uf_perc.

    CREATE OBJECT obj_toolbar_up
      EXPORTING
        io_alv_grid = gc_alv_uf_perc.

    PERFORM: create_catalog_uf_perc.

    gs_layout-zebra      = 'X'.
    gs_variant_up-report  = sy-repid.

    SET HANDLER: obj_toolbar_up->on_toolbar          FOR gc_alv_uf_perc,
                 obj_toolbar_up->handle_user_command FOR gc_alv_uf_perc,
                 obj_toolbar_up->on_data_changer     FOR gc_alv_uf_perc.

    REFRESH: it_exclude_fcode.

    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    CALL METHOD gc_alv_uf_perc->set_table_for_first_display
      EXPORTING
        is_layout            = ls_layout_uf_perc
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant_up
      CHANGING
        it_fieldcatalog      = gt_catalog_uf_perc
        it_outtab            = gt_uf_perc.

    CALL METHOD gc_alv_uf_perc->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD gc_alv_uf_perc->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    "---------------------------------------------------------------------
    " Descrição Motivo Cancelamento
    "---------------------------------------------------------------------

    IF g_custom_mdfe_canc IS INITIAL.

      CREATE OBJECT g_custom_mdfe_canc
        EXPORTING
          container_name = 'CC_JUST_CANC'.

      CREATE OBJECT obg_desc_mdfe_canc
        EXPORTING
          parent            = g_custom_mdfe_canc
          wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position = 110
          max_number_chars  = 255.

      CALL METHOD obg_desc_mdfe_canc->set_toolbar_mode
        EXPORTING
          toolbar_mode = '0'.

      CALL METHOD obg_desc_mdfe_canc->set_readonly_mode
        EXPORTING
          readonly_mode = 1.

    ENDIF.

    "---------------------------------------------------------------
    " Historico MDF-e
    "---------------------------------------------------------------

    CREATE OBJECT gc_container_hist_mdfe
      EXPORTING
        container_name = 'CC_HISTORICO_MDFE'.

    CREATE OBJECT gc_alv_hist_mdfe
      EXPORTING
        i_parent = gc_container_hist_mdfe.

    PERFORM: create_catalog_hist_mdfe.

    ls_layout_hist_mdfe-no_toolbar = 'X'.

    CALL METHOD gc_alv_hist_mdfe->set_table_for_first_display
      EXPORTING
        is_layout       = ls_layout_hist_mdfe
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = gt_catalog_hist_mdfe
        it_outtab       = gt_hist_mdfe.

    CALL METHOD gc_alv_hist_mdfe->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    "---------------------------------------------------------------
    " MDF-e em processo de Encerramento
    "---------------------------------------------------------------
    IF gc_container_mdfe_enc IS INITIAL.

      CREATE OBJECT gc_container_mdfe_enc
        EXPORTING
          container_name = 'CC_MDFE_ENC'.

      CREATE OBJECT gc_alv_mdfe_enc
        EXPORTING
          i_parent = gc_container_mdfe_enc.

      PERFORM: create_catalog_mdfe_enc.

      ls_layout_mdfe_enc-no_toolbar = 'X'.

      CALL METHOD gc_alv_mdfe_enc->set_table_for_first_display
        EXPORTING
          is_layout       = ls_layout_mdfe_enc
          i_save          = 'A'
        CHANGING
          it_fieldcatalog = gt_catalog_mdfe_enc
          it_outtab       = gt_mdfe_enc.

      CALL METHOD gc_alv_mdfe_enc->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    ENDIF.

  ENDIF.


  IF ( vg_init_mdfe IS INITIAL ).

    REFRESH: gt_mdfe_enc, gt_mdfe_enc_aux, gt_transp_mdfe.
    CLEAR: gw_transp_mdfe.

    var_len = lines( it_alv_selection ).

    IF ( var_len EQ 1 ).

      CASE sy-tcode.
        WHEN 'ZMDFE'.

          "CT-e ou NF-e Vinculada
          SELECT SINGLE *
            FROM zsdt0105 INTO wa_zsdt0105
           WHERE docnum_ref = wa_alv_selection-docnum.

          PERFORM carrega_docs_mdfe USING wa_zsdt0105-docnum.
          PERFORM atualizar_mdfe.

        WHEN OTHERS.

          "CT-e ou NF-e Vinculada
          SELECT SINGLE *
            FROM zsdt0105 INTO wa_zsdt0105
           WHERE docnum = wa_alv_selection-docnum.

          IF sy-subrc = 0.
            READ TABLE it_alv_selection INTO wa_alv_selection INDEX 1.
            PERFORM carrega_docs_mdfe USING wa_alv_selection-docnum.
            PERFORM atualizar_mdfe.
          ELSE.
            PERFORM: new_mdfe_selected.
          ENDIF.

      ENDCASE.

    ELSE.
      PERFORM: new_mdfe_selected.
    ENDIF.

    vg_init_mdfe = 'X'.

  ENDIF.

  CALL METHOD gc_alv_mdfe->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD gc_alv_uf_perc->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD gc_alv_hist_mdfe->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD gc_alv_mdfe_enc->refresh_table_display
    EXPORTING
      is_stable = wa_stable.


ENDMODULE.                 " CREATE_ALV_0107  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0108  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0108 OUTPUT.

  SET PF-STATUS 'SCREEN_108'.
*  SET TITLEBAR 'xxx'.

  PERFORM atualiza_transp_mdfe.

  gw_transp_mdfe-lib_tot_carga = ''.

  "Procura algum documento vinculado ao MDF-e, onde a unidade seja diferente de TO e KG
  LOOP AT gt_mdfe INTO gw_mdfe.

    SELECT SINGLE gewei
      FROM j_1bnfdoc INTO @DATA(v_unid)
     WHERE docnum EQ @gw_mdfe-docnum.

    IF ( sy-subrc EQ 0          ) AND
       ( v_unid  IS NOT INITIAL ) AND
       ( v_unid  NE 'TO'        ) AND
       ( v_unid  NE 'KG'        ).

      gw_transp_mdfe-lib_tot_carga = 'X'.
    ENDIF.

    SELECT SINGLE meins
      FROM j_1bnflin INTO v_unid
     WHERE docnum EQ gw_mdfe-docnum.

    IF ( sy-subrc EQ 0          ) AND
       ( v_unid  IS NOT INITIAL ) AND
       ( v_unid  NE 'TO'        ) AND
       ( v_unid  NE 'KG'        ).
      gw_transp_mdfe-lib_tot_carga = 'X'.
    ENDIF.

  ENDLOOP.

  LOOP AT SCREEN.
    CASE sy-tcode.
      WHEN 'ZNFE'.
        CASE screen-name.
          WHEN 'GW_TRANSP_MDFE-QCARGA' OR
               'GW_TRANSP_MDFE-CUNID'.
            IF gw_transp_mdfe-lib_tot_carga IS NOT INITIAL.
              screen-input = 1.
            ELSE.
              screen-input = 0.
            ENDIF.
        ENDCASE.

        MODIFY SCREEN.
      WHEN OTHERS.
        screen-input = 0.
        MODIFY SCREEN.
    ENDCASE.

  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0108  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0108 INPUT.

  DATA: vl_invalid       TYPE c,
        wa_zlest0002_aux TYPE zlest0002,
        wa_lfa1_aux      TYPE lfa1,
        tg_placas_inf    TYPE TABLE OF zplaca WITH HEADER LINE,
        vl_tot_placas    TYPE i.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      CLEAR: tg_placas_inf[], vl_tot_placas.

      IF gw_transp_mdfe-lib_tot_carga IS NOT INITIAL.

        IF gw_transp_mdfe-cunid IS INITIAL.
          MESSAGE 'Informe a Unidade de Medida!' TYPE 'W'.
          RETURN.
        ENDIF.

        IF gw_transp_mdfe-qcarga IS INITIAL.
          MESSAGE 'Informe o Peso Bruto Total!' TYPE 'W'.
          RETURN.
        ENDIF.

        IF gw_transp_mdfe-vcarga IS INITIAL.
          MESSAGE 'Informe o Valor da Carga!' TYPE 'W'.
          RETURN.
        ENDIF.

      ENDIF.

      IF ( gw_transp_mdfe-placa_cav IS INITIAL ).
        MESSAGE 'Placa Tração não informada!' TYPE 'S'.
        RETURN.
      ENDIF.

      PERFORM valida_placa_mdfe USING '0' "Tração
                             CHANGING gw_transp_mdfe-placa_cav
                                      vl_invalid
                                      wa_zlest0002_aux.
      CHECK vl_invalid IS INITIAL.

      tg_placas_inf = gw_transp_mdfe-placa_cav.
      APPEND tg_placas_inf.

*      IF ( GW_TRANSP_MDFE-PLACA_CAR1 IS INITIAL ).
*        MESSAGE 'Placa Reboque 1 não informada!' TYPE 'S'.
*        RETURN.
*      ENDIF.

      IF gw_transp_mdfe-placa_car1 IS NOT INITIAL.
        PERFORM valida_placa_mdfe USING '1' "Reboque
                               CHANGING gw_transp_mdfe-placa_car1
                                        vl_invalid
                                        wa_zlest0002_aux.
        CHECK vl_invalid IS INITIAL.
        tg_placas_inf = gw_transp_mdfe-placa_car1.
        APPEND tg_placas_inf.
      ENDIF.

      IF gw_transp_mdfe-placa_car2 IS NOT INITIAL.
        PERFORM valida_placa_mdfe USING '1' "Reboque
                               CHANGING gw_transp_mdfe-placa_car2
                                        vl_invalid
                                        wa_zlest0002_aux.
        CHECK vl_invalid IS INITIAL.
        tg_placas_inf = gw_transp_mdfe-placa_car2.
        APPEND tg_placas_inf.
      ENDIF.

      IF gw_transp_mdfe-placa_car3 IS NOT INITIAL.
        PERFORM valida_placa_mdfe USING '1' "Reboque
                               CHANGING gw_transp_mdfe-placa_car3
                                        vl_invalid
                                        wa_zlest0002_aux.
        CHECK vl_invalid IS INITIAL.

        tg_placas_inf = gw_transp_mdfe-placa_car3.
        APPEND tg_placas_inf.
      ENDIF.

      vl_tot_placas = lines( tg_placas_inf[] ).
      SORT tg_placas_inf.
      DELETE ADJACENT DUPLICATES FROM tg_placas_inf.
      IF vl_tot_placas NE lines( tg_placas_inf[] ).
        MESSAGE 'Existem Placas duplicadas! Operação não permitida!' TYPE 'S'.
        RETURN.
      ENDIF.

      IF ( gw_transp_mdfe-motorista IS INITIAL ).
        MESSAGE 'Motorista não informado!' TYPE 'S'.
        RETURN.
      ENDIF.

      PERFORM valida_motorista_mdfe USING gw_transp_mdfe-motorista
                                 CHANGING vl_invalid
                                          wa_lfa1_aux.
      CHECK vl_invalid IS INITIAL.

      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
