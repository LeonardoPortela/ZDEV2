*&---------------------------------------------------------------------*
*& Report  ZSDR0153
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0153.

INCLUDE zsdr0153_top.

INCLUDE zsdr0153_tool_event_receiver.

INCLUDE zsdr0153_tree_event_receiver.

DATA: toolbar_event_receiver TYPE REF TO lcl_toolbar_event_receiver,
      m_event_handler        TYPE REF TO lcl_event,
      m_event_handler_0102   TYPE REF TO lcl_event_0102,
      m_event_handler_0103   TYPE REF TO lcl_event_0103,
      m_event_handler_0103_1 TYPE REF TO lcl_event_0103_1,
      m_event_handler_0103_2 TYPE REF TO lcl_event_0103_2,
      m_event_handler_0103_3 TYPE REF TO lcl_event_0103_3,
      m_event_handler_0105   TYPE REF TO lcl_event_0105, " Rubenilson - 19.06.24 - #150257
      m_event_handler_0106   TYPE REF TO lcl_event_0106. " Rubenilson - 19.06.24 - #150257

*******************************************************************************************
* TABELAS
*******************************************************************************************
TABLES: zsdt0317.

*******************************************************************************************
* TELA SELECAO
*******************************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs FOR zsdt0317-bukrs NO INTERVALS NO-EXTENSION MODIF ID id1 OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

*******************************************************************************************
* START-OF-SELECRTION
*******************************************************************************************
START-OF-SELECTION.
  vg_bukrs = p_bukrs-low.
  SELECT * INTO TABLE @DATA(lit_t001)
     FROM t001
   WHERE bukrs EQ @vg_bukrs.
  IF sy-subrc = 0.
    PERFORM f_selecao_dados.
    CALL SCREEN 100.
  ELSE.
    MESSAGE 'Código Empresa Incorreto' TYPE 'I'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'Z0100'.
  SET TITLEBAR '0100'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TAB_STRIP_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tab_strip_set OUTPUT.
  tab_strip_cliente-activetab  = g_tab_strip_cli-pressed_tab.
  CASE g_tab_strip_cli-pressed_tab.
    WHEN c_tab_strip_cliente-tab1.
*      PERFORM create_object USING 'CC_0101' .
*      PERFORM create_fieldcat USING 'ZS1001_CARGO' .
      g_tab_strip_cli-subscreen = '0101'.
*      PERFORM display_output USING git_1001_cargo.
    WHEN c_tab_strip_cliente-tab2.
*      PERFORM create_object USING 'CC_0102' .
      g_tab_strip_cli-subscreen = '0102'.
*      PERFORM display_output USING git_1005_cargo.
    WHEN c_tab_strip_cliente-tab3.
*      PERFORM create_object USING 'CC_0103' .
*      PERFORM create_fieldcat USING 'ZS1010_CARGO' .
      g_tab_strip_cli-subscreen = '0103'.
*      PERFORM display_output USING git_1010_cargo.
*** Inicio - Rubenilson - 19.06.24 - #150257
    WHEN c_tab_strip_cliente-tab4.
      g_tab_strip_cli-subscreen = '0105'.
    WHEN c_tab_strip_cliente-tab5.
      g_tab_strip_cli-subscreen = '0106'.
*** Fim - Rubenilson - 19.06.24 - #150257
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA lv_ucomm TYPE sy-ucomm.
  lv_ucomm = sy-ucomm.
  CASE lv_ucomm.
    WHEN 'SAVE'.
      PERFORM f_verifica_erros.
      IF it_msg_ret[] IS NOT INITIAL.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '100'
            i_show        = 'X'
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_CLI-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = it_msg_ret.
      ELSE.
        PERFORM f_salva.
      ENDIF.

      "PERFORM f_selecao_dados.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      SET SCREEN '0'.
      LEAVE SCREEN.
    WHEN 'SHOW_MSGRE'.
      PERFORM f_verifica_erros.

      IF it_msg_ret[] IS NOT INITIAL.
        DATA(lva_show) = 'X'.
      ELSE.
        CLEAR: lva_show.
      ENDIF.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen      = '100'
          i_show        = c_x
          i_repid       = sy-repid
          i_pressed_tab = 'G_TAB_STRIP_CLI-PRESSED_TAB'
          i_set_field   = 'X_FIELD'
        IMPORTING
          e_messagem    = wg_mensagem
        TABLES
          it_msgs       = it_msg_ret.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TAB_STRIP_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tab_strip_get INPUT.
  CASE sy-ucomm.
    WHEN c_tab_strip_cliente-tab1.
      IF it_msg_ret[] IS INITIAL.
        PERFORM troca_aba USING vg_troca '0101'.
        IF vg_troca IS INITIAL.
          g_tab_strip_cli-pressed_tab = c_tab_strip_cliente-tab1.
          g_tab_strip_cli-subscreen = '0101'.
        ENDIF.
        PERFORM f_show_erro.
      ENDIF.
    WHEN c_tab_strip_cliente-tab2.
      IF it_msg_ret[] IS INITIAL.
        PERFORM troca_aba USING vg_troca '0102'.
        IF vg_troca IS INITIAL.
          g_tab_strip_cli-pressed_tab = c_tab_strip_cliente-tab2.
          g_tab_strip_cli-subscreen = '0102'.
        ENDIF.
      ELSE.
        PERFORM f_show_erro.
      ENDIF.
    WHEN c_tab_strip_cliente-tab3.
      IF it_msg_ret[] IS INITIAL.
        PERFORM troca_aba USING vg_troca '0103'.
        IF vg_troca IS INITIAL.
          g_tab_strip_cli-pressed_tab = c_tab_strip_cliente-tab3.
          g_tab_strip_cli-subscreen = '0103'.
        ENDIF.
      ELSE.
        PERFORM f_show_erro.
      ENDIF..
*** Inicio - Rubenilson - 19.06.24 - #150257
    WHEN c_tab_strip_cliente-tab4.
      IF it_msg_ret[] IS INITIAL.
        PERFORM troca_aba USING vg_troca '0105'.
        IF vg_troca IS INITIAL.
          g_tab_strip_cli-pressed_tab = c_tab_strip_cliente-tab3.
          g_tab_strip_cli-subscreen = '0105'.
        ENDIF.
      ELSE.
        PERFORM f_show_erro.
      ENDIF..

    WHEN c_tab_strip_cliente-tab5.
      IF it_msg_ret[] IS INITIAL.
        PERFORM troca_aba USING vg_troca '0106'.
        IF vg_troca IS INITIAL.
          g_tab_strip_cli-pressed_tab = c_tab_strip_cliente-tab3.
          g_tab_strip_cli-subscreen = '0106'.
        ENDIF.
      ELSE.
        PERFORM f_show_erro.
      ENDIF..
*** Fim - Rubenilson - 19.06.24 - #150257
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  PERFORM init_alv_0101.
  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_0101 .
  FREE: t_fieldcatalog.

  w_layout-zebra      = abap_false.
  "w_layout-edit       = abap_true. " Makes all Grid editable
  w_layout-no_totarr  = abap_true.
  w_layout-no_totexp  = abap_true.
  w_layout-no_totline = abap_true.
  w_layout-no_toolbar = abap_false.
  w_layout-sel_mode   = 'A'.
  w_layout-cwidth_opt = abap_false.
  w_layout-stylefname = 'CELLTAB'.

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container EXPORTING container_name = g_container.
    CREATE OBJECT g_grid_grupo_c EXPORTING i_parent = g_custom_container.

    PERFORM fill_it_fieldcatalog USING:
            01 'KTOKD' 'ZSDT0317'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' 'T077X'   'X'  'Grp.de Contas'   '15'.
    "T077X
    PERFORM toolbar_grupo_contas.

    IF m_event_handler IS INITIAL.
      CREATE OBJECT m_event_handler.
      SET HANDLER : m_event_handler->toolbar         FOR g_grid_grupo_c.
      SET HANDLER : m_event_handler->user_command    FOR g_grid_grupo_c.
      SET HANDLER : m_event_handler->on_double_click FOR g_grid_grupo_c.
    ENDIF.

    CALL METHOD g_grid_grupo_c->set_table_for_first_display
      EXPORTING
        is_layout            = w_layout
        it_toolbar_excluding = pt_exclude
        i_save               = 'U' "abap_true
      CHANGING
        it_fieldcatalog      = t_fieldcatalog
        it_outtab            = it_grupo_contas_alv[].

    CALL METHOD g_grid_grupo_c->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_grupo_c->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    SET HANDLER : lcl_event_handler=>on_data_changed          FOR g_grid_grupo_c.
    SET HANDLER : lcl_event_handler=>on_data_changed_finished FOR g_grid_grupo_c.
    SET HANDLER : lcl_event_handler=>handle_on_f1             FOR g_grid_grupo_c.
    SET HANDLER : lcl_event_handler=>on_f4                    FOR g_grid_grupo_c.

    DATA: lt_f4 TYPE lvc_t_f4.
    DATA: lw_f4 TYPE lvc_s_f4.

    CLEAR lw_f4.
    lw_f4-fieldname  = 'KTOKD'.
    lw_f4-register   = 'X'.
    APPEND lw_f4 TO lt_f4.

    CALL METHOD g_grid_grupo_c->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4.

  ELSE.
    CALL METHOD g_grid_grupo_c->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TOOLBAR_ALV1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM toolbar_grupo_contas .
  FREE: pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_separator              TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_more              TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant        TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_auf                    TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_view                   TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print                  TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_info                   TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_check                  TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_refresh                TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row         TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row         TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row         TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_chain             TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_crbatch           TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_crweb             TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_lineitems         TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_master_data       TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_more              TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_report            TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_xint              TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_xxl               TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_check                  TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_col_invisible          TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_col_optimize           TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_count                  TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant        TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_data_save              TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_delete_filter          TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_deselect_all           TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_detail                 TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_excl_all               TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_expcrdata              TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_expcrdesig             TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_expcrtempl             TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_expmdb                 TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_extend                 TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_f4                     TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_filter                 TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_find                   TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_fix_columns            TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_graph                  TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_help                   TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_html                   TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_info                   TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_load_variant           TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row         TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy               TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row           TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut                TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row         TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row         TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_move_row           TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste              TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row      TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo               TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_maintain_variant       TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_maximum                TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_minimum                TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_pc_file                TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print                  TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print_prev             TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_refresh                TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_reprep                 TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_save_variant           TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_select_all             TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_send                   TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_separator              TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort                   TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_asc               TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc               TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_subtot                 TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sum                    TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_to_office              TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_to_rep_tree            TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_unfix_columns          TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard  TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_variant_admin          TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_views                  TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_view_crystal           TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_paste                  TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_subtot                 TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_sum                    TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_variant                TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_view                   TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fg_sort                   TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fg_edit                   TO pt_exclude.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TROCA_ABA
*&---------------------------------------------------------------------*
FORM troca_aba  USING vg_troca TYPE sy-subrc
                      vg_tab TYPE c.

  IF vg_tab = '0101'..
    PERFORM verifica_selecao_grupo_ini  USING vg_verifica_selecao_gp.
  ELSE.
    IF vg_tab = '0102'.
      PERFORM verifica_selecao_grupo_cc  USING vg_verifica_selecao_gp.

      IF vg_verifica_selecao_gp IS INITIAL.
        MESSAGE 'Selecionar Grupo de Contas' TYPE 'I'.
        vg_troca = 1.
      ENDIF.
    ELSE.
      IF vg_tab = '0103'.
        PERFORM verifica_selecao_empresa  USING vg_verifica_selecao_ep.

        IF vg_verifica_selecao_ep IS INITIAL.
          MESSAGE 'Selecionar Dados da Empresa' TYPE 'I'.
          vg_troca = 1.
        ENDIF.
*** Inicio - Rubenilson - 19.06.24 - #150257
      ELSEIF vg_tab = '0105'.
        PERFORM verifica_selecao_grupo_cc2  USING vg_verifica_selecao_gp.

        IF vg_verifica_selecao_gp IS INITIAL.
          MESSAGE 'Selecionar Grupo de Contas' TYPE 'I'.
          vg_troca = 1.
        ENDIF.
      ELSEIF vg_tab = '0106'.
        PERFORM verifica_selecao_grupo_cc3  USING vg_verifica_selecao_gp.

        IF vg_verifica_selecao_gp IS INITIAL.
          MESSAGE 'Selecionar Grupo de Contas' TYPE 'I'.
          vg_troca = 1.
        ENDIF.
      ENDIF.
*** Inicio - Rubenilson - 19.06.24 - #150257
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_GRUPO_INI
*&---------------------------------------------------------------------*
FORM verifica_selecao_grupo_ini  USING  vg_verifica_selecao_gp TYPE sy-subrc.

  READ TABLE it_grupo_contas_alv_aux INTO wa_grupo_contas_alv INDEX 1.
  IF sy-subrc IS INITIAL."Rubenilson - 19.06.24 - #140377

    LOOP AT it_grupo_contas_alv INTO DATA(wa_grupo_contas_ini) WHERE id =  wa_grupo_contas_alv-id.

      MOVE-CORRESPONDING wa_grupo_contas_alv TO  wa_grupo_contas_ini.
      IF wa_grupo_contas_alv-criado <> 'X'.
        FREE wa_grupo_contas_ini-celltab.
        t_estilo =  VALUE #( ( fieldname =  'KTOKD'  style = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no ) ).

        INSERT LINES OF t_estilo INTO TABLE wa_grupo_contas_ini-celltab.
      ENDIF.
      MODIFY it_grupo_contas_alv FROM wa_grupo_contas_ini INDEX sy-tabix.
    ENDLOOP.

  ENDIF."Rubenilson - 19.06.24 - #140377

  PERFORM troca_aba_0101 USING space.
  LEAVE TO SCREEN 0100.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_GRUPO_CC
*&---------------------------------------------------------------------*
FORM verifica_selecao_grupo_cc  USING  vg_verifica_selecao_gp TYPE sy-subrc.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CLEAR: wa_grupo_contas, it_grupo_contas_alv_aux[].

  CALL METHOD g_grid_grupo_c->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_grupo_contas_alv INTO wa_grupo_contas_alv INDEX wa_selected_rows-index.

    "MOVE-CORRESPONDING wa_grupo_contas TO  wa_grupo_contas_alv.
    APPEND wa_grupo_contas_alv TO it_grupo_contas_alv_aux.
    PERFORM troca_aba_0102 USING space.
    LEAVE TO SCREEN 0100.

  ENDLOOP.

  IF NOT wa_grupo_contas IS INITIAL.
    vg_verifica_selecao_gp = 1.
  ELSE.
    vg_verifica_selecao_gp = 0.
  ENDIF.


ENDFORM.

*** Inicio - Rubenilson - 19.06.24 - #150257
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_GRUPO_CC
*&---------------------------------------------------------------------*
FORM verifica_selecao_grupo_cc2  USING  vg_verifica_selecao_gp TYPE sy-subrc.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CLEAR: wa_empresa_fornec, it_empresa_fornec_alv_aux[].

  CALL METHOD g_grid_grupo_c->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_grupo_contas_alv[] INTO wa_grupo_contas_alv INDEX wa_selected_rows-index.
    IF sy-subrc IS INITIAL.
      READ TABLE it_empresa_fornec_alv[] INTO wa_empresa_fornec_alv
      WITH KEY id = wa_grupo_contas_alv-id.
    ENDIF.

    APPEND wa_empresa_fornec_alv TO it_empresa_fornec_alv_aux.
    PERFORM troca_aba_0105 USING space.
    DELETE ADJACENT DUPLICATES FROM it_empresa_fornec_alv_aux.
    LEAVE TO SCREEN 0100.

  ENDLOOP.

  IF NOT wa_grupo_contas IS INITIAL.
    vg_verifica_selecao_gp = 1.
  ELSE.
    vg_verifica_selecao_gp = 0.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_GRUPO_CC
*&---------------------------------------------------------------------*
FORM verifica_selecao_grupo_cc3  USING  vg_verifica_selecao_gp TYPE sy-subrc.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CLEAR: wa_dados_compra, it_dados_compra_alv_aux[].

  CALL METHOD g_grid_grupo_c->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_dados_compra_alv[] INTO wa_dados_compra_alv INDEX wa_selected_rows-index.

    APPEND wa_dados_compra_alv TO it_dados_compra_alv_aux.
    PERFORM troca_aba_0106 USING space.
    DELETE ADJACENT DUPLICATES FROM it_dados_compra_alv_aux.
    LEAVE TO SCREEN 0100.

  ENDLOOP.

  IF NOT wa_grupo_contas IS INITIAL.
    vg_verifica_selecao_gp = 1.
  ELSE.
    vg_verifica_selecao_gp = 0.
  ENDIF.


ENDFORM.
*** Fim - Rubenilson - 19.06.24 - #150257
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_EMPRESA
*&---------------------------------------------------------------------*
FORM verifica_selecao_empresa  USING  vg_verifica_selecao_ep TYPE sy-subrc.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.


  CLEAR: wa_grupo_contas_alv_aux,
         wa_canal_dist,
         wa_canal_dist_alv.

  CALL METHOD g_grid_empresa->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_grupo_contas_alv_aux INTO wa_grupo_contas_alv_aux INDEX wa_selected_rows-index.
    MODIFY it_grupo_contas_alv FROM wa_grupo_contas_alv_aux INDEX wa_selected_rows-index.

    PERFORM troca_aba_0103 USING space.
    LEAVE TO SCREEN 0100.

  ENDLOOP.

  IF NOT wa_grupo_contas_alv_aux IS INITIAL.
    vg_verifica_selecao_ep = 1.
  ELSE.
    vg_verifica_selecao_ep = 0.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELECAO_DADOS
*&---------------------------------------------------------------------*
FORM f_selecao_dados .

  CLEAR:  it_grupo_contas[],
          it_canal_dist[],
          it_set_atv[],
          it_parceiros[],
          it_grupo_contas_alv[],
          it_canal_dist_alv[],
          it_set_atv_alv[],
          it_parceiros_alv[],
          it_canal_dist_alv_aux[],
          it_set_atv_alv_aux[],
          it_area_venda_alv_aux[],
*** Inicio - Rubenilson - 19.06.24 - #150257
          it_empresa_fornec[],
          it_empresa_fornec_alv[],
          it_grupo_contas_alv_aux[],
          it_dados_compra[],
          it_dados_compra_alv[].
*** Fim - Rubenilson - 19.06.24 - #150257

  CLEAR:
         wa_grupo_contas,
         wa_canal_dist,
         wa_set_atv,
         wa_parceiros,
         wa_grupo_contas_alv,
         wa_canal_dist_alv,
         wa_set_atv_alv,
         wa_parceiros_alv.

  SELECT *
     FROM zsdt0317
     INTO TABLE  it_grupo_contas
  WHERE  bukrs  =  p_bukrs-low
    AND cancelado <> 'X'.

  IF it_grupo_contas[] IS NOT INITIAL.

    SELECT *
      FROM zsdt0319
    INTO TABLE it_canal_dist
      FOR ALL ENTRIES IN it_grupo_contas
        WHERE id = it_grupo_contas-id
         AND cancelado <> 'X'.

    SELECT *
      FROM zsdt0320
    INTO TABLE it_set_atv
      FOR ALL ENTRIES IN it_canal_dist
        WHERE id = it_canal_dist-id
          AND seq_canal = it_canal_dist-seq_canal
          AND cancelado <> 'X'.
*** Inicio - Rubenilson - 19.06.24 - #150257
    SELECT *
      FROM zsdt0341
    INTO TABLE it_empresa_fornec
      FOR ALL ENTRIES IN it_grupo_contas
        WHERE id = it_grupo_contas-id
         AND cancelado <> 'X'.
    IF sy-subrc IS INITIAL.
      SORT it_empresa_fornec BY id.
      LOOP AT it_grupo_contas ASSIGNING FIELD-SYMBOL(<fs_grupo_contas>).
        READ TABLE it_empresa_fornec TRANSPORTING NO FIELDS
        WITH KEY id = <fs_grupo_contas>-id
        BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          APPEND INITIAL LINE TO it_empresa_fornec ASSIGNING FIELD-SYMBOL(<fs_fornec>).
          <fs_fornec>-bukrs = <fs_grupo_contas>-bukrs.
          <fs_fornec>-id    = <fs_grupo_contas>-id.
        ENDIF.

      ENDLOOP.
    ELSE.
      LOOP AT it_grupo_contas ASSIGNING <fs_grupo_contas>.
        APPEND INITIAL LINE TO it_empresa_fornec ASSIGNING <fs_fornec>.
        <fs_fornec>-bukrs = <fs_grupo_contas>-bukrs.
        <fs_fornec>-id    = <fs_grupo_contas>-id.
      ENDLOOP.
    ENDIF.
*** Fim - Rubenilson - 19.06.24 - #150257

*    SELECT *
*      FROM zsdt0321
*    INTO TABLE it_parceiros
*      FOR ALL ENTRIES IN it_set_atv
*        WHERE id = it_set_atv-id
*          AND seq_canal = it_set_atv-seq_canal
*          AND cancelado <> 'X'.

    SELECT *
       FROM zsdt0322
     INTO TABLE it_area_venda
       FOR ALL ENTRIES IN it_set_atv
         WHERE id = it_set_atv-id
           AND seq_canal = it_set_atv-seq_canal
           AND cancelado <> 'X'.


    LOOP AT it_grupo_contas INTO wa_grupo_contas.
      MOVE-CORRESPONDING wa_grupo_contas TO wa_grupo_contas_alv.
*03.04.2023 - CBRAND
      FREE wa_grupo_contas_alv-celltab.
      t_estilo =  VALUE #( ( fieldname =  'KTOKD'  style = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no ) ).

      INSERT LINES OF t_estilo INTO TABLE wa_grupo_contas_alv-celltab.

      APPEND wa_grupo_contas_alv TO it_grupo_contas_alv.

    ENDLOOP.

*** Inicio - Rubenilson - 19.06.24 - #150257
    LOOP AT it_empresa_fornec INTO wa_empresa_fornec.
      MOVE-CORRESPONDING wa_empresa_fornec TO wa_empresa_fornec_alv.

      FREE wa_empresa_fornec_alv-celltab.
      t_estilo =  VALUE #( ( fieldname =  'KTOKD'  style = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no ) ).

      INSERT LINES OF t_estilo INTO TABLE wa_empresa_fornec_alv-celltab.

      APPEND wa_empresa_fornec_alv TO it_empresa_fornec_alv.

    ENDLOOP.

    LOOP AT it_dados_compra INTO wa_dados_compra.
      MOVE-CORRESPONDING wa_dados_compra TO wa_dados_compra_alv.

      FREE wa_dados_compra_alv-celltab.
      t_estilo =  VALUE #( ( fieldname =  'KTOKD'  style = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no ) ).

      INSERT LINES OF t_estilo INTO TABLE wa_dados_compra_alv-celltab.

      APPEND wa_dados_compra_alv TO it_dados_compra_alv.

    ENDLOOP.
*** Fim - Rubenilson - 19.06.24 - #150257

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TROCA_ABA_0101
*&---------------------------------------------------------------------*
FORM troca_aba_0101  USING forcar TYPE c.
  g_tab_strip_cli-pressed_tab = 'TAB_STRIP_CLI_FC1'.
  g_tab_strip_cli-subscreen   = '0101'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TROCA_ABA_0102
*&---------------------------------------------------------------------*
FORM troca_aba_0102  USING forcar TYPE c.
  g_tab_strip_cli-pressed_tab = 'TAB_STRIP_CLI_FC2'.
  g_tab_strip_cli-subscreen   = '0102'.
ENDFORM.

*** Inicio - Rubenilson - 19.06.24 - #150257
*&---------------------------------------------------------------------*
*&      Form  TROCA_ABA_0105
*&---------------------------------------------------------------------*
FORM troca_aba_0105  USING forcar TYPE c.
  g_tab_strip_cli-pressed_tab = 'TAB_STRIP_CLI_FC4'.
  g_tab_strip_cli-subscreen   = '0105'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TROCA_ABA_0105
*&---------------------------------------------------------------------*
FORM troca_aba_0106  USING forcar TYPE c.
  g_tab_strip_cli-pressed_tab = 'TAB_STRIP_CLI_FC5'.
  g_tab_strip_cli-subscreen   = '0106'.
ENDFORM.
*** Fim - Rubenilson - 19.06.24 - #150257
*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0102 OUTPUT.
  PERFORM init_alv_0102.
  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.

*** Inicio - Rubenilson - 19.06.24 - #150257
*&---------------------------------------------------------------------*
*&      Module  STATUS_0105  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0105 OUTPUT.
  PERFORM init_alv_0105.
  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0106  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0106 OUTPUT.
  PERFORM init_alv_0106.
  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.
*** Fim - Rubenilson - 19.06.24 - #150257

*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_0102
*&---------------------------------------------------------------------*
FORM init_alv_0102 .
  FREE: t_fieldcatalog_0102.

  w_layout_0102-zebra      = abap_false.
  w_layout_0102-no_totarr  = abap_true.
  w_layout_0102-no_totexp  = abap_true.
  w_layout_0102-no_totline = abap_true.
  w_layout_0102-no_toolbar = abap_false.
  w_layout_0102-sel_mode   = 'A'.
  w_layout_0102-cwidth_opt = abap_false.
  w_layout_0102-stylefname = 'CELLTAB'.

  IF g_custom_container_0102 IS INITIAL.

    CREATE OBJECT g_custom_container_0102 EXPORTING container_name = g_container_0102.
    CREATE OBJECT g_grid_empresa EXPORTING i_parent = g_custom_container_0102.

    PERFORM fill_it_fieldcatalog_0102 USING:
            01 'AKONT' 'ZSDT0317'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     'X'     'Cta. Conciliação'     '18',
            02 'ZUAWA' 'ZSDT0317'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     'X'     'Chave de Ordenação'   '18',
            03 'FDGRV' 'ZSDT0317'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     'X'     'Grp.Adm. Tesour.'     '18',
            04 'ZWELS' 'LFB1'       ' '  'X'  ' '  ' '  ' '   ' '   ' ' 'LFB1'  'X'     'Formas Pagamento'     '18',
            05 'ZTERM' 'KNB1'       ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     'X'     'Cond. Pagamento'      '18'.

    PERFORM toolbar_grupo_contas.

    IF m_event_handler_0102 IS INITIAL.
      CREATE OBJECT m_event_handler_0102.
      SET HANDLER : m_event_handler_0102->toolbar              FOR g_grid_empresa.
      SET HANDLER : m_event_handler_0102->user_command         FOR g_grid_empresa.
      SET HANDLER : m_event_handler_0102->on_double_click      FOR g_grid_empresa.
    ENDIF.

    CALL METHOD g_grid_empresa->set_table_for_first_display
      EXPORTING
        is_layout            = w_layout_0102
        it_toolbar_excluding = pt_exclude
        i_save               = 'U' "abap_true
      CHANGING
        it_fieldcatalog      = t_fieldcatalog_0102
        it_outtab            = it_grupo_contas_alv_aux[].

    CALL METHOD g_grid_empresa->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_empresa->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER : lcl_event_handler_0102=>on_data_changed          FOR g_grid_empresa.
    SET HANDLER : lcl_event_handler_0102=>on_data_changed_finished FOR g_grid_empresa.
    SET HANDLER : lcl_event_handler_0102=>handle_on_f1             FOR g_grid_empresa.
    SET HANDLER : lcl_event_handler_0102=>on_f4                    FOR g_grid_empresa.

    DATA: lt_f4 TYPE lvc_t_f4.
    DATA: lw_f4 TYPE lvc_s_f4.

    CLEAR lw_f4.
    lw_f4-fieldname  = 'AKONT'.
    lw_f4-register   = 'X'.
    APPEND lw_f4 TO lt_f4[].

    CLEAR lw_f4.
    lw_f4-fieldname  = 'ZUAWA'.
    lw_f4-register   = 'X'.
    APPEND lw_f4 TO lt_f4[].

    CLEAR lw_f4.
    lw_f4-fieldname  = 'FDGRV'.
    lw_f4-register   = 'X'.
    lw_f4-getbefore = 'X' .
    INSERT lw_f4 INTO TABLE lt_f4.
    "APPEND lw_f4 TO lt_f4.

    CLEAR lw_f4.
    lw_f4-fieldname  = 'ZWELS'.
    lw_f4-register   = 'X'.
    APPEND lw_f4 TO lt_f4[].

    CLEAR lw_f4.
    lw_f4-fieldname  = 'ZTERM'.
    lw_f4-register   = 'X'.
    lw_f4-getbefore = 'X' .
    INSERT lw_f4 INTO TABLE lt_f4.
    "APPEND lw_f4 TO lt_f4.

    CALL METHOD g_grid_empresa->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4.

  ELSE.
    CALL METHOD g_grid_empresa->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.
*** Inicio - Rubenilson - 19.06.24 - #150257
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_0102
*&---------------------------------------------------------------------*
FORM init_alv_0105 .
  FREE: t_fieldcatalog_0105.

  IF it_empresa_fornec_alv_aux[] IS NOT INITIAL.

    LOOP AT it_empresa_fornec_alv_aux[] ASSIGNING FIELD-SYMBOL(<fs_empresa_fornec_alv>).
      APPEND INITIAL LINE TO it_empresa_fornec ASSIGNING FIELD-SYMBOL(<fs_empresa_fornec>).

      <fs_empresa_fornec>-id    = <fs_empresa_fornec_alv>-id.
      <fs_empresa_fornec>-bukrs = <fs_empresa_fornec_alv>-bukrs.

    ENDLOOP.

  ENDIF.
  w_layout_0105-zebra      = abap_false.
  w_layout_0105-no_totarr  = abap_true.
  w_layout_0105-no_totexp  = abap_true.
  w_layout_0105-no_totline = abap_true.
  w_layout_0105-no_toolbar = abap_false.
  w_layout_0105-sel_mode   = 'A'.
  w_layout_0105-cwidth_opt = abap_false.
  w_layout_0105-stylefname = 'CELLTAB'.

  IF g_custom_container_0105 IS INITIAL.

    CREATE OBJECT g_custom_container_0105 EXPORTING container_name = g_container_0105.
    CREATE OBJECT g_grid_empresa_fornec EXPORTING i_parent = g_custom_container_0105.

    PERFORM fill_it_fieldcatalog_0105 USING:
            01 'AKONT' 'ZSDT0341'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     'X'     'Cta. Conciliação'       '18',
            02 'FDGRV' 'ZSDT0341'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     'X'     'Grp.admin.tesouraria'   '18',
            03 'ZTERM' 'ZSDT0341'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     'X'     'Condições pgto.'        '18',
            04 'ZWELS' 'ZSDT0341'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' 'LFB1'  'X'     'Formas de pagamento'    '18',
            05 'QLAND' 'ZSDT0341'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     'X'     'Chv.país/região IRF'    '18'.

    PERFORM toolbar_grupo_contas.

    IF m_event_handler_0105 IS INITIAL.
      CREATE OBJECT m_event_handler_0105.
      SET HANDLER : m_event_handler_0105->toolbar              FOR g_grid_empresa_fornec.
      SET HANDLER : m_event_handler_0105->user_command         FOR g_grid_empresa_fornec.
*      SET HANDLER : m_event_handler_0105->on_double_click      FOR g_grid_empresa_fornec  .
    ENDIF.

    CALL METHOD g_grid_empresa_fornec->set_table_for_first_display
      EXPORTING
        is_layout            = w_layout_0105
        it_toolbar_excluding = pt_exclude
        i_save               = 'U' "abap_true
      CHANGING
        it_fieldcatalog      = t_fieldcatalog_0105
        it_outtab            = it_empresa_fornec_alv_aux[].

    CALL METHOD g_grid_empresa_fornec->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_empresa_fornec->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER : lcl_event_handler_0105=>on_data_changed          FOR g_grid_empresa_fornec.
    SET HANDLER : lcl_event_handler_0105=>on_data_changed_finished FOR g_grid_empresa_fornec.
    SET HANDLER : lcl_event_handler_0105=>handle_on_f1             FOR g_grid_empresa_fornec.
    SET HANDLER : lcl_event_handler_0105=>on_f4                    FOR g_grid_empresa_fornec.

    DATA: lt_f4 TYPE lvc_t_f4.
    DATA: lw_f4 TYPE lvc_s_f4.

    CLEAR lw_f4.
    lw_f4-fieldname  = 'AKONT'.
    lw_f4-register   = 'X'.
    APPEND lw_f4 TO lt_f4[].

    CLEAR lw_f4.
    lw_f4-fieldname  = 'ZUAWA'.
    lw_f4-register   = 'X'.
    APPEND lw_f4 TO lt_f4[].

    CLEAR lw_f4.
    lw_f4-fieldname  = 'FDGRV'.
    lw_f4-register   = 'X'.
    lw_f4-getbefore = 'X' .
    INSERT lw_f4 INTO TABLE lt_f4.
    "APPEND lw_f4 TO lt_f4.

    CLEAR lw_f4.
    lw_f4-fieldname  = 'ZWELS'.
    lw_f4-register   = 'X'.
    APPEND lw_f4 TO lt_f4[].

    CLEAR lw_f4.
    lw_f4-fieldname  = 'ZTERM'.
    lw_f4-register   = 'X'.
    lw_f4-getbefore = 'X' .
    INSERT lw_f4 INTO TABLE lt_f4.

    CLEAR lw_f4.
    lw_f4-fieldname  = 'QLAND'.
    lw_f4-register   = 'X'.
    lw_f4-getbefore = 'X' .
    INSERT lw_f4 INTO TABLE lt_f4.

    CALL METHOD g_grid_empresa_fornec->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4.

  ELSE.
    CALL METHOD g_grid_empresa_fornec->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_0102
*&---------------------------------------------------------------------*
FORM init_alv_0106 .
  FREE: t_fieldcatalog_0106.

  IF it_dados_compra_alv_aux[] IS NOT INITIAL.

*    LOOP AT it_dados_compra_alv_aux[] ASSIGNING FIELD-SYMBOL(<fs_dados_compra_alv>).
*      APPEND INITIAL LINE TO it_dados_compra ASSIGNING FIELD-SYMBOL(<fs_dados_compra>).
*
*      <fs_dados_compra>-id    = <fs_dados_compra_alv>-id.
**      <fs_dados_compra>-bukrs = <fs_dados_compra_alv>-bukrs.
*
*    ENDLOOP.

  ENDIF.
  w_layout_0105-zebra      = abap_false.
  w_layout_0105-no_totarr  = abap_true.
  w_layout_0105-no_totexp  = abap_true.
  w_layout_0105-no_totline = abap_true.
  w_layout_0105-no_toolbar = abap_false.
  w_layout_0105-sel_mode   = 'A'.
  w_layout_0105-cwidth_opt = abap_false.
  w_layout_0105-stylefname = 'CELLTAB'.

  IF g_custom_container_0106 IS INITIAL.

    CREATE OBJECT g_custom_container_0106 EXPORTING container_name = g_container_0106.
    CREATE OBJECT g_grid_dados_compra EXPORTING i_parent = g_custom_container_0106.

    PERFORM fill_it_fieldcatalog_0106 USING:
            01 'EKORG'   'ZSDT0342'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     ''     'Organização de compras'     '18',
            02 'WAERS'   'ZSDT0342'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     ''     'Moeda da ordem'             '18',
            03 'ZTERM'   'ZSDT0342'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     'X'     'Condições pgto.'            '18',
            04 'INCO1'   'ZSDT0342'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     ''     'Incoterms'                  '18',
            05 'INCO2_L' 'ZSDT0342'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     ''     'Local incoterms 1'          '18',
            06 'VSBED' '  ZSDT0342'   ' '  'X'  ' '  ' '  ' '   ' '   ' ' ' '     ''     'Condições de expedição'     '18',
            07 'WEBRE'   'ZSDT0342'   ' '  'X'  ' '  ' '  ' '   'X'   ' ' ' '     ''     'Revisao fats.baseada EM'    '18',
            08 'NRGEW'   'ZSDT0342'   ' '  'X'  ' '  ' '  ' '   'X'   ' ' ' '     ''     'Conceder bonif.mercadoria'  '18',
            09 'KZAUT'   'ZSDT0342'   ' '  'X'  ' '  ' '  ' '   'X'   ' ' ' '     ''     'Pedido automatico'          '18'.

    PERFORM toolbar_grupo_contas.

    IF m_event_handler_0106 IS INITIAL.
      CREATE OBJECT m_event_handler_0106.
      SET HANDLER : m_event_handler_0106->toolbar              FOR g_grid_dados_compra.
      SET HANDLER : m_event_handler_0106->user_command         FOR g_grid_dados_compra.
*      SET HANDLER : m_event_handler_0105->on_double_click      FOR g_grid_empresa_fornec  .
    ENDIF.

    CALL METHOD g_grid_dados_compra->set_table_for_first_display
      EXPORTING
        is_layout            = w_layout_0106
        it_toolbar_excluding = pt_exclude
        i_save               = 'U' "abap_true
      CHANGING
        it_fieldcatalog      = t_fieldcatalog_0106
        it_outtab            = it_dados_compra_alv_aux[].

    CALL METHOD g_grid_dados_compra->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_dados_compra->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER : lcl_event_handler_0106=>on_data_changed          FOR g_grid_dados_compra.
    SET HANDLER : lcl_event_handler_0106=>on_data_changed_finished FOR g_grid_dados_compra.
    SET HANDLER : lcl_event_handler_0106=>handle_on_f1             FOR g_grid_dados_compra.
    SET HANDLER : lcl_event_handler_0106=>on_f4                    FOR g_grid_dados_compra.

    DATA: lt_f4 TYPE lvc_t_f4.
    DATA: lw_f4 TYPE lvc_s_f4.

*
    CLEAR lw_f4.
    lw_f4-fieldname  = 'ZTERM'.
    lw_f4-register   = 'X'.
    lw_f4-getbefore = 'X' .
    INSERT lw_f4 INTO TABLE lt_f4.

    CALL METHOD g_grid_dados_compra->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4.

  ELSE.
    CALL METHOD g_grid_dados_compra->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.
*** Fim - Rubenilson - 19.06.24 - #150257
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog USING  VALUE(p_colnum)
                                 VALUE(p_fieldname)
                                 VALUE(p_tabname)
                                 VALUE(p_emphasize)
                                 VALUE(p_edit)
                                 VALUE(p_icon)
                                 VALUE(p_hotspot)
                                 VALUE(p_opt)
                                 VALUE(p_checkbox)
                                 VALUE(p_dosum)
                                 VALUE(p_check)
                                 VALUE(p_f4)
                                 VALUE(p_header)
                                 VALUE(p_outputlen).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-checktable = p_check.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.
  wa_fieldcatalog-no_convext = abap_false.

  APPEND wa_fieldcatalog TO t_fieldcatalog.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0102
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0102 USING  VALUE(p_colnum)
                                       VALUE(p_fieldname)
                                       VALUE(p_tabname)
                                       VALUE(p_emphasize)
                                       VALUE(p_edit)
                                       VALUE(p_icon)
                                       VALUE(p_hotspot)
                                       VALUE(p_opt)
                                       VALUE(p_checkbox)
                                       VALUE(p_dosum)
                                       VALUE(p_check)
                                       VALUE(p_f4)
                                       VALUE(p_header)
                                       VALUE(p_outputlen).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-checktable = p_check.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.
  wa_fieldcatalog-no_convext = abap_false.

  APPEND wa_fieldcatalog TO t_fieldcatalog_0102.

ENDFORM.

*** Inicio - Rubenilson - 19.06.24 - #150257
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0105
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0105 USING  VALUE(p_colnum)
                                       VALUE(p_fieldname)
                                       VALUE(p_tabname)
                                       VALUE(p_emphasize)
                                       VALUE(p_edit)
                                       VALUE(p_icon)
                                       VALUE(p_hotspot)
                                       VALUE(p_opt)
                                       VALUE(p_checkbox)
                                       VALUE(p_dosum)
                                       VALUE(p_check)
                                       VALUE(p_f4)
                                       VALUE(p_header)
                                       VALUE(p_outputlen).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-checktable = p_check.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.
  wa_fieldcatalog-no_convext = abap_false.

  APPEND wa_fieldcatalog TO t_fieldcatalog_0105.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0106
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0106 USING  VALUE(p_colnum)
                                       VALUE(p_fieldname)
                                       VALUE(p_tabname)
                                       VALUE(p_emphasize)
                                       VALUE(p_edit)
                                       VALUE(p_icon)
                                       VALUE(p_hotspot)
                                       VALUE(p_opt)
                                       VALUE(p_checkbox)
                                       VALUE(p_dosum)
                                       VALUE(p_check)
                                       VALUE(p_f4)
                                       VALUE(p_header)
                                       VALUE(p_outputlen).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = 'LFM1'.
  wa_fieldcatalog-ref_field  = p_fieldname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-checktable = p_check.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.
  wa_fieldcatalog-no_convext = abap_false.

  APPEND wa_fieldcatalog TO t_fieldcatalog_0106.

ENDFORM.
*** Fim - Rubenilson - 19.06.24 - #150257
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0103
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0103 USING   VALUE(p_colnum)
                                       VALUE(p_fieldname)
                                       VALUE(p_tabname)
                                       VALUE(p_emphasize)
                                       VALUE(p_edit)
                                       VALUE(p_icon)
                                       VALUE(p_hotspot)
                                       VALUE(p_opt)
                                       VALUE(p_checkbox)
                                       VALUE(p_dosum)
                                       VALUE(p_f4)
                                       VALUE(p_header)
                                       VALUE(p_outputlen).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.
  wa_fieldcatalog-no_convext = abap_false.

  APPEND wa_fieldcatalog TO t_fieldcatalog_0103.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0103_1
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0103_1 USING  VALUE(p_colnum)
                                        VALUE(p_fieldname)
                                        VALUE(p_tabname)
                                        VALUE(p_emphasize)
                                        VALUE(p_edit)
                                        VALUE(p_icon)
                                        VALUE(p_hotspot)
                                        VALUE(p_opt)
                                        VALUE(p_checkbox)
                                        VALUE(p_dosum)
                                        VALUE(p_check)
                                        VALUE(p_f4)
                                        VALUE(p_header)
                                        VALUE(p_outputlen).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-checktable = p_check.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.
  wa_fieldcatalog-no_convext = abap_false.

  APPEND wa_fieldcatalog TO t_fieldcatalog_0103_1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0103_2
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0103_2 USING   VALUE(p_colnum)
                                         VALUE(p_fieldname)
                                         VALUE(p_tabname)
                                         VALUE(p_emphasize)
                                         VALUE(p_edit)
                                         VALUE(p_icon)
                                         VALUE(p_hotspot)
                                         VALUE(p_opt)
                                         VALUE(p_checkbox)
                                         VALUE(p_dosum)
                                         VALUE(p_check)
                                         VALUE(p_f4)
                                         VALUE(p_header)
                                         VALUE(p_outputlen).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-checktable = p_check.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.
  wa_fieldcatalog-no_convext = abap_false.

  APPEND wa_fieldcatalog TO t_fieldcatalog_0103_2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0103_3
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0103_3 USING   VALUE(p_colnum)
                                         VALUE(p_fieldname)
                                         VALUE(p_tabname)
                                         VALUE(p_emphasize)
                                         VALUE(p_edit)
                                         VALUE(p_icon)
                                         VALUE(p_hotspot)
                                         VALUE(p_opt)
                                         VALUE(p_checkbox)
                                         VALUE(p_dosum)
                                         VALUE(p_check)
                                         VALUE(p_f4)
                                         VALUE(p_header)
                                         VALUE(p_outputlen).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-checktable = p_check.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.
  wa_fieldcatalog-no_convext = abap_false.

  APPEND wa_fieldcatalog TO t_fieldcatalog_0103_3.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TROCA_ABA_0103
*&---------------------------------------------------------------------*
FORM troca_aba_0103  USING    p_space.
  g_tab_strip_cli-pressed_tab = 'TAB_STRIP_CLI_FC3'.
  g_tab_strip_cli-subscreen   = '0103'.

  CLEAR: it_canal_dist_alv[],
         it_set_atv_alv[],
         it_parceiros_alv[],
         it_area_venda_alv[].

  IF wa_grupo_contas_alv IS NOT INITIAL.
    LOOP AT it_canal_dist INTO wa_canal_dist  WHERE id = wa_grupo_contas_alv-id .

      SELECT SINGLE vtext
        FROM tvtwt
        INTO @DATA(l_vtext)
         WHERE vtweg EQ @wa_canal_dist-vtweg
           AND spras EQ @sy-langu.

      wa_canal_dist_alv-vtext = l_vtext.

      MOVE-CORRESPONDING wa_canal_dist TO wa_canal_dist_alv.

      FREE wa_canal_dist_alv-celltab.
      t_estilo =  VALUE #( ( fieldname = 'VTWEG' style = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no  )
                           ( fieldname = 'VTEXT' style = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no ) ).

      INSERT LINES OF t_estilo INTO TABLE wa_canal_dist_alv-celltab.

      APPEND wa_canal_dist_alv TO it_canal_dist_alv.
      CLEAR: wa_canal_dist_alv, wa_canal_dist,l_vtext .
    ENDLOOP.

    CLEAR: wa_canal_dist_alv, l_vtext.
    LOOP AT it_canal_dist_alv_aux INTO wa_canal_dist_alv WHERE id  =  wa_grupo_contas_alv-id.

      SELECT SINGLE vtext
        FROM tvtwt
        INTO l_vtext
         WHERE vtweg EQ wa_canal_dist_alv-vtweg
           AND spras EQ sy-langu.

      wa_canal_dist_alv-vtext = l_vtext.

      APPEND wa_canal_dist_alv  TO it_canal_dist_alv.
      CLEAR: wa_canal_dist_alv , l_vtext.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0103 OUTPUT.
  PERFORM init_alv_0103.
  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_0103
*&---------------------------------------------------------------------*
FORM init_alv_0103 .

*** 0103 - CANAL DE DISTRIBUIÇÃO
  IF g_custom_container_0103 IS INITIAL.

    FREE: t_fieldcatalog_0103.

    w_layout_0103-zebra      = abap_false.
    w_layout_0103-no_totarr  = abap_true.
    w_layout_0103-no_totexp  = abap_true.
    w_layout_0103-no_totline = abap_true.
    w_layout_0103-no_toolbar = abap_false.
    w_layout_0103-sel_mode   = 'A'.
    w_layout_0103-cwidth_opt = abap_false.
    w_layout_0103-stylefname = 'CELLTAB'.


    CREATE OBJECT g_custom_container_0103 EXPORTING container_name = g_container_0103.
    CREATE OBJECT g_grid_distrib EXPORTING i_parent = g_custom_container_0103.

*    CREATE OBJECT obj_dyndoc_id
*      EXPORTING
*        no_margins = 'X'.

    PERFORM fill_it_fieldcatalog_0103 USING:
            01 'VTWEG' 'ZSDT0319'   ' '  'X'  ' '  ' '  ' '   ' '   ' '   'X'     'Canal de Distribuição' '18',
            02 'VTEXT' 'TVTWT'      ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '     'Descrição'             '18'.

    PERFORM toolbar_grupo_contas.

    IF m_event_handler_0103 IS INITIAL.
      CREATE OBJECT m_event_handler_0103.
      SET HANDLER : m_event_handler_0103->toolbar              FOR g_grid_distrib.
      SET HANDLER : m_event_handler_0103->user_command         FOR g_grid_distrib.
      SET HANDLER : m_event_handler_0103->on_double_click      FOR g_grid_distrib.
    ENDIF.

    CALL METHOD g_grid_distrib->set_table_for_first_display
      EXPORTING
        is_layout            = w_layout_0103
        it_toolbar_excluding = pt_exclude
        i_save               = 'U' "abap_true
      CHANGING
        it_fieldcatalog      = t_fieldcatalog_0103
        it_outtab            = it_canal_dist_alv[].

    CALL METHOD g_grid_distrib->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_distrib->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER : lcl_event_handler_0103=>on_data_changed          FOR g_grid_distrib.
    SET HANDLER : lcl_event_handler_0103=>on_data_changed_finished FOR g_grid_distrib.
    SET HANDLER : lcl_event_handler_0103=>handle_on_f1             FOR g_grid_distrib.

  ELSE.
    CALL METHOD g_grid_distrib->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

*** 0103_1  - SETOR ATIVIDADE
  IF g_custom_container_0103_1 IS INITIAL.


    FREE: t_fieldcatalog_0103_1.

    w_layout_0103_1-zebra      = abap_false.
    w_layout_0103_1-no_totarr  = abap_true.
    w_layout_0103_1-no_totexp  = abap_true.
    w_layout_0103_1-no_totline = abap_true.
    w_layout_0103_1-no_toolbar = abap_false.
    w_layout_0103_1-sel_mode   = 'A'.
    w_layout_0103_1-cwidth_opt = abap_false.
    w_layout_0103_1-stylefname = 'CELLTAB'.

    CREATE OBJECT g_custom_container_0103_1 EXPORTING container_name = g_container_0103_1.
    CREATE OBJECT g_grid_atividade EXPORTING i_parent = g_custom_container_0103_1.

    PERFORM fill_it_fieldcatalog_0103_1 USING:
            "01 'ID'         'ZSDT0320'       ''         'X'  ' '  ' '  ' '  ' '   ' '   ' '     'ID'                '18',
            "02 'SEQ_CANAL'  'ZSDT0320'       ''         ' '  'X'  ' '  ' '  ' '   ' '   ' '     'ID Canal'          '18',
            "03 'SEQ_SETOR'  'ZSDT0320'       ''         ' '  'X'  ' '  ' '  ' '   ' '   ' '     'ID Setor'          '18',
            "01 'BUKRS'      'ZSDT0320'       ''         'X'  ' '  ' '  ' '  ' '   ' '  ''        'X'     'Empresa'           '18',
            "02 'KTOKD'      'T077X'          ''         'X'  ' '  ' '  ' '  ' '   ' '  'T077X'   'X'     'Grupo Contas'      '18',
            03 'SPART'      'ZSDT0320'        ''         'X'  ' '  ' '  ' '  ' '   ' '  ''        'X'     'Setor Atividade'   '18',
            04 'VTEXT'      'TSPAT'           ''         ' '  ' '  ' '  ' '  ' '   ' '  ''        ' '     'Decrição'          '18'.


    PERFORM toolbar_grupo_contas.

    IF m_event_handler_0103_1 IS INITIAL.
      CREATE OBJECT m_event_handler_0103_1.
      SET HANDLER : m_event_handler_0103_1->toolbar              FOR g_grid_atividade.
      SET HANDLER : m_event_handler_0103_1->user_command         FOR g_grid_atividade.
      SET HANDLER : m_event_handler_0103_1->on_double_click      FOR g_grid_atividade.
    ENDIF.

    CALL METHOD g_grid_atividade->set_table_for_first_display
      EXPORTING
        is_layout            = w_layout_0103_1
        it_toolbar_excluding = pt_exclude
        i_save               = 'U' "abap_true
      CHANGING
        it_fieldcatalog      = t_fieldcatalog_0103_1
        it_outtab            = it_set_atv_alv[].

    CALL METHOD g_grid_atividade->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_atividade->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER : lcl_event_handler_0103_1=>on_data_changed            FOR g_grid_atividade.
    SET HANDLER : lcl_event_handler_0103_1=>on_data_changed_finished   FOR g_grid_atividade.
    SET HANDLER : lcl_event_handler_0103_1=>handle_on_f1               FOR g_grid_atividade.
    SET HANDLER : lcl_event_handler_0103_1=>on_f4                      FOR g_grid_atividade.

    DATA: lt_f4 TYPE lvc_t_f4.
    DATA: lw_f4 TYPE lvc_s_f4.

    CLEAR lw_f4.
    lw_f4-fieldname  = 'SPART'.
    lw_f4-register   = 'X'.
    APPEND lw_f4 TO lt_f4[].

    CALL METHOD g_grid_distrib->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4.


  ELSE.
    CALL METHOD g_grid_atividade->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.


*** 0103_2 - Removido a opção de parceiro no dia 10.04.2023
*  IF g_custom_container_0103_2 IS INITIAL.
*
*    FREE: t_fieldcatalog_0103_2.
*
*    w_layout_0103_2-zebra      = abap_false.
*    w_layout_0103_2-no_totarr  = abap_true.
*    w_layout_0103_2-no_totexp  = abap_true.
*    w_layout_0103_2-no_totline = abap_true.
*    w_layout_0103_2-no_toolbar = abap_false.
*    w_layout_0103_2-sel_mode   = 'A'.
*    w_layout_0103_2-cwidth_opt = abap_false.
*    w_layout_0103_2-stylefname = 'CELLTAB'.
*
*    CREATE OBJECT g_custom_container_0103_2 EXPORTING container_name = g_container_0103_2.
*    CREATE OBJECT g_grid_parceiro EXPORTING i_parent = g_custom_container_0103_2.
*
*    PERFORM fill_it_fieldcatalog_0103_2 USING:
*            "01 'ID'             'ZSDT0321'       ''         'X'  ' '  ' '  ' '  ' '   ' '   ' '   'ID'                  '18',
*            "02 'SEQ_CANAL'      'ZSDT0321'       ''         ' '  'X'  ' '  ' '  ' '   ' '   ' '   'ID Canal'            '18',
*            "03 'SEQ_PACEIROS'   'ZSDT0321'       ''         ' '  'X'  ' '  ' '  ' '   ' '   ' '   'ID Parceiro'         '18',
*            "01 'BUKRS'          'ZSDT0321'       ''         'X'  ' '  ' '  ' '  ' '   ' '  ' '       'X'   'Empresa'             '18',
*            "02 'KTOKD'          'T077X'          ''         'X'  ' '  ' '  ' '  ' '   ' '  'T077X'   'X'   'Grupo Contas'        '18',
*            03 'PARVW'          'TPAR'           ''         'X'  ' '  ' '  ' '  ' '   ' '  'TPAR'    'X'   'Função do parceiro'  '18',
*            04 'VTEXT'          'TAPART'         ''         ' '  ' '  ' '  ' '  ' '   ' '  'TPAR'    ' '   'Descrição'           '18'.
*
*
*
*    PERFORM toolbar_grupo_contas.
*
*    IF m_event_handler_0103_2 IS INITIAL.
*      CREATE OBJECT m_event_handler_0103_2.
*      SET HANDLER : m_event_handler_0103_2->toolbar              FOR g_grid_parceiro.
*      SET HANDLER : m_event_handler_0103_2->user_command         FOR g_grid_parceiro.
*      SET HANDLER : m_event_handler_0103_2->on_double_click      FOR g_grid_parceiro.
*    ENDIF.
*
*    CALL METHOD g_grid_parceiro->set_table_for_first_display
*      EXPORTING
*        is_layout            = w_layout_0103_2
*        it_toolbar_excluding = pt_exclude
*        i_save               = 'U' "abap_true
*      CHANGING
*        it_fieldcatalog      = t_fieldcatalog_0103_2
*        it_outtab            = it_parceiros_alv[].
*
*    CALL METHOD g_grid_parceiro->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*    CALL METHOD g_grid_parceiro->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
*    SET HANDLER : lcl_event_handler_0103_2=>on_data_changed            FOR g_grid_parceiro.
*    SET HANDLER : lcl_event_handler_0103_2=>on_data_changed_finished   FOR g_grid_parceiro.
*    SET HANDLER : lcl_event_handler_0103_2=>handle_on_f1               FOR g_grid_parceiro.
*    SET HANDLER : lcl_event_handler_0103_2=>on_f4                      FOR g_grid_parceiro.
*
*
*    DATA: lt_f4_0103_2 TYPE lvc_t_f4.
*    DATA: lw_f4_0103_2 TYPE lvc_s_f4.
*
*
*    CLEAR lw_f4_0103_2.
*    lw_f4_0103_2-fieldname  = 'PARVW'.
*    lw_f4_0103_2-register   = 'X'.
*    APPEND lw_f4_0103_2 TO lt_f4_0103_2[].
*
*    CALL METHOD g_grid_parceiro->register_f4_for_fields
*      EXPORTING
*        it_f4 = lt_f4_0103_2.
*
*  ELSE.
*    CALL METHOD g_grid_parceiro->refresh_table_display
*      EXPORTING
*        is_stable = w_stable.
*  ENDIF.


*** 0103_3 - AREA VENDA - ADICIONAIS
  IF g_custom_container_0103_3 IS INITIAL.

    FREE: t_fieldcatalog_0103_3.

    w_layout_0103_3-zebra      = abap_false.
    w_layout_0103_3-no_totarr  = abap_true.
    w_layout_0103_3-no_totexp  = abap_true.
    w_layout_0103_3-no_totline = abap_true.
    w_layout_0103_3-no_toolbar = abap_false.
    w_layout_0103_3-sel_mode   = 'A'.
    w_layout_0103_3-cwidth_opt = abap_false.
    w_layout_0103_3-stylefname = 'CELLTAB'.

    CREATE OBJECT g_custom_container_0103_3 EXPORTING container_name = g_container_0103_3.
    CREATE OBJECT g_grid_adicionais EXPORTING i_parent = g_custom_container_0103_3.

    PERFORM fill_it_fieldcatalog_0103_3 USING:
            "01 'ID'             'ZSDT0322'       ''         'X'  ' '  ' '  ' '  ' '   ' '   ' '  'ID'                       '18',
            "02 'SEQ_CANAL'      'ZSDT0322'       ''         ' '  'X'  ' '  ' '  ' '   ' '   ' '  'ID Canal'                 '18',
            "03 'SEQ_AREA_VENDA' 'ZSDT0322'       ''         ' '  'X'  ' '  ' '  ' '   ' '   ' '  'ID Are Venda'             '18',
            01 'WAERS'          'TCURC'          ''     'X' ' '  ' '  ' '  ' '   ' ' 'TCURC '  'X'  'Moeda'                   '18',
            02 'VERSG'          ' '              ''     'X  ' ' '  ' '  ' '  ' '   ' ' ' '     'X'  'GrupoEstatCliente'       '18',
            03 'KALKS'          ' '              ''     'X  ' ' '  ' '  ' '  ' '   ' ' ' '     'X'  'Esquema Cliente'         '18',
            04 'LPRIO'          'ZSDT0322'       ''     'X' ' '  ' '  ' '  ' '   ' ' ' '       'X'  'Prioridade de remessa'   '18',
            05 'VSBED'          'ZSDT0322'       ''     'X' ' '  ' '  ' '  ' '   ' ' ' '       'X'  'Condição de expedição'   '18',
            06 'KZAZU'          'ZSDT0322'       ''     'X' ' '  ' '  ' '  ' '   ' ' ' '       'X'  'Cod. Agr. Ordens'        '18',
            07 'KZTLF'          'ZSDT0322'       ''     'X' ' '  ' '  ' '  ' '   ' ' ' '       'X'  'Nº Fornecimento'         '18',
            08 'PERFK'          ''               ''     'X' ' '  ' '  ' '  ' '   ' ' ' '       'X'  'Datas do faturamento'    '18',
            09 'KTGRD'          'TVKTT'          ''     'X' ' '  ' '  ' '  ' '   ' ' 'TVKTT'   'X'  'Grp.Clas.Contabil'       '18',
            10 'TAXKD'          'TSKD'           'C310' ' ' ' '  ' '  'X'  ' '   ' ' 'TSKD'    'X'  'Class. Cliente'          '18',
            11 'TATYP'          'ZSDT0322'       ''     ' ' ' '  ' '  ' '  ' '   ' ' ' '       ''    'Ctg.imposto'            '18'.



    PERFORM toolbar_grupo_contas.

    IF m_event_handler_0103_3 IS INITIAL.
      CREATE OBJECT m_event_handler_0103_3.
      SET HANDLER : m_event_handler_0103_3->toolbar              FOR g_grid_adicionais.
      SET HANDLER : m_event_handler_0103_3->user_command         FOR g_grid_adicionais.
      SET HANDLER : m_event_handler_0103_3->on_double_click      FOR g_grid_adicionais.
    ENDIF.

    CALL METHOD g_grid_adicionais->set_table_for_first_display
      EXPORTING
        is_layout            = w_layout_0103_3
        it_toolbar_excluding = pt_exclude
        i_save               = 'U' "abap_true
      CHANGING
        it_fieldcatalog      = t_fieldcatalog_0103_3
        it_outtab            = it_area_venda_alv[].

    CALL METHOD g_grid_adicionais->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_adicionais->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER : lcl_event_handler_0103_3=>on_data_changed            FOR g_grid_adicionais.
    SET HANDLER : lcl_event_handler_0103_3=>on_data_changed_finished   FOR g_grid_adicionais.
    SET HANDLER : lcl_event_handler_0103_3=>handle_on_f1               FOR g_grid_adicionais.
    SET HANDLER : lcl_event_handler_0103_3=>on_f4                      FOR g_grid_adicionais.


    DATA: lt_f4_0103_3 TYPE lvc_t_f4.
    DATA: lw_f4_0103_3 TYPE lvc_s_f4.

    CLEAR lw_f4_0103_3.
    lw_f4_0103_3-fieldname  = 'WAERS'.
    lw_f4_0103_3-register   = 'X'.
    INSERT lw_f4_0103_3 INTO TABLE lt_f4_0103_3.

    CLEAR lw_f4_0103_3.
    lw_f4_0103_3-fieldname  = 'VERSG'.
    lw_f4_0103_3-register   = 'X'.
    INSERT lw_f4_0103_3 INTO TABLE lt_f4_0103_3.

    CLEAR lw_f4_0103_3.
    lw_f4_0103_3-fieldname  = 'KALKS'.
    lw_f4_0103_3-register   = 'X'.
    INSERT lw_f4_0103_3 INTO TABLE lt_f4_0103_3.

    CLEAR lw_f4_0103_3.
    lw_f4_0103_3-fieldname  = 'LPRIO'.
    lw_f4_0103_3-register   = 'X'.
    INSERT lw_f4_0103_3 INTO TABLE lt_f4_0103_3.

    CLEAR lw_f4_0103_3.
    lw_f4_0103_3-fieldname  = 'VSBED'.
    lw_f4_0103_3-register   = 'X'.
    INSERT lw_f4_0103_3 INTO TABLE lt_f4_0103_3.

    CLEAR lw_f4_0103_3.
    lw_f4_0103_3-fieldname  = 'KZAZU'.
    lw_f4_0103_3-register   = 'X'.
    INSERT lw_f4_0103_3 INTO TABLE lt_f4_0103_3.

    CLEAR lw_f4_0103_3.
    lw_f4_0103_3-fieldname  = 'ANTLF' .
    lw_f4_0103_3-register   = 'X'.
    INSERT lw_f4_0103_3 INTO TABLE lt_f4_0103_3.

    CLEAR lw_f4_0103_3.
    lw_f4_0103_3-fieldname  = 'PERFK'.
    lw_f4_0103_3-register   = 'X'.
    INSERT lw_f4_0103_3 INTO TABLE lt_f4_0103_3.

    CLEAR lw_f4_0103_3.
    lw_f4_0103_3-fieldname  = 'KTGRD'.
    lw_f4_0103_3-register   = 'X'.
    INSERT lw_f4_0103_3 INTO TABLE lt_f4_0103_3.

    CLEAR lw_f4_0103_3.
    lw_f4_0103_3-fieldname  = 'TATYP'.
    lw_f4_0103_3-register   = 'X'.
    lw_f4_0103_3-getbefore  = space.
    lw_f4_0103_3-chngeafter = space.
    INSERT lw_f4_0103_3 INTO TABLE lt_f4_0103_3.

    CLEAR lw_f4_0103_3.
    lw_f4_0103_3-fieldname  = 'TAXKD'.
    lw_f4_0103_3-register   = 'X'.
    lw_f4_0103_3-getbefore  = space.
    lw_f4_0103_3-chngeafter = space.
    INSERT lw_f4_0103_3 INTO TABLE lt_f4_0103_3.


    CALL METHOD g_grid_adicionais->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4_0103_3.

  ELSE.
    CALL METHOD g_grid_adicionais->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SALVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_salva .

  DATA: lva_user TYPE zsdt0317-usuario,
        lva_date TYPE zsdt0317-data,
        lva_hora TYPE zsdt0317-hora.


  lva_user = sy-uname.
  lva_date = sy-datum.
  lva_hora = sy-uzeit.

* 1º - ZSDT0317 - Cabeçalho dos Parâmetros para o cadastro de Cliente
*** Inicio - Rubenilson - 06.12.24 #159528

  IF it_grupo_contas_alv_aux[] IS NOT INITIAL.
    LOOP AT it_grupo_contas_alv_aux INTO wa_grupo_contas_alv  WHERE criado = 'X' OR modif = 'X'.
      MOVE-CORRESPONDING wa_grupo_contas_alv TO wa_grupo_contas.

      wa_grupo_contas-usuario = lva_user.
      wa_grupo_contas-data    = lva_date.
      wa_grupo_contas-hora    = lva_hora.

      MODIFY zsdt0317  FROM wa_grupo_contas.
      COMMIT WORK.
    ENDLOOP.
  ELSE.
    LOOP AT it_grupo_contas_alv INTO wa_grupo_contas_alv  WHERE criado = 'X' OR modif = 'X'.
      MOVE-CORRESPONDING wa_grupo_contas_alv TO wa_grupo_contas.

      wa_grupo_contas-usuario = lva_user.
      wa_grupo_contas-data    = lva_date.
      wa_grupo_contas-hora    = lva_hora.

      MODIFY zsdt0317  FROM wa_grupo_contas.
      COMMIT WORK.
    ENDLOOP.
  ENDIF.

* 2º - ZSDT0319 - Canal de Distribuição
  LOOP AT it_canal_dist_alv_aux  INTO wa_canal_dist_alv   WHERE criado = 'X'.
    MOVE-CORRESPONDING  wa_canal_dist_alv TO  wa_canal_dist.

    wa_canal_dist-usuario = lva_user.
    wa_canal_dist-data    = lva_date.
    wa_canal_dist-hora    = lva_hora.


    MODIFY zsdt0319  FROM wa_canal_dist.
    COMMIT WORK.
  ENDLOOP.

* 3º - ZSDT0320 - Setor de Atividade
  "LOOP AT it_set_atv_alv  INTO wa_set_atv_alv   WHERE criado = 'X'.
  LOOP AT   it_set_atv_alv_aux  INTO wa_set_atv_alv   WHERE criado = 'X'.
    MOVE-CORRESPONDING  wa_set_atv_alv TO  wa_set_atv.

    wa_set_atv-usuario = lva_user.
    wa_set_atv-data    = lva_date.
    wa_set_atv-hora    = lva_hora.


    MODIFY zsdt0320  FROM wa_set_atv.
    COMMIT WORK.
  ENDLOOP.

* 4º - ZSDT0321 - Funções de Parceiro
*  LOOP AT it_parceiros_alv  INTO wa_parceiros_alv   WHERE criado = 'X'.
*    MOVE-CORRESPONDING  wa_parceiros_alv TO  wa_parceiros.
*
*    wa_parceiros-usuario = lva_user.
*    wa_parceiros-data    = lva_date.
*    wa_parceiros-hora    = lva_hora.
*
*    MODIFY zsdt0321  FROM wa_parceiros.
*    "APPEND wa_parceiros TO it_parceiros.
*  ENDLOOP.

* 5º - ZSDT0322 - Área de Vendas
  "LOOP AT it_area_venda_alv  INTO wa_area_venda_alv   WHERE criado = 'X' OR modif = 'X'..
  LOOP AT it_area_venda_alv_aux  INTO wa_area_venda_alv   WHERE criado = 'X' OR modif = 'X'..
    MOVE-CORRESPONDING  wa_area_venda_alv TO wa_area_venda.

    wa_area_venda-usuario = lva_user.
    wa_area_venda-data    = lva_date.
    wa_area_venda-hora    = lva_hora.

    MODIFY zsdt0322  FROM wa_area_venda.
    COMMIT WORK.
  ENDLOOP.

*** Inicio - Rubenilson - 19.06.24 - #150257
* 6º - ZSDT0341 - Empresa para fornecedores
  LOOP AT it_empresa_fornec_alv ASSIGNING FIELD-SYMBOL(<fs_empresa_fornec>) WHERE criado = 'X' OR modif = 'X'.
    MOVE-CORRESPONDING <fs_empresa_fornec> TO wa_empresa_fornec.

    wa_empresa_fornec-usuario = lva_user.
    wa_empresa_fornec-data    = lva_date.
    wa_empresa_fornec-hora    = lva_hora.

    MODIFY zsdt0341  FROM wa_empresa_fornec.
    COMMIT WORK.

  ENDLOOP.
*** Fim - Rubenilson - 19.06.24 - #150257

  MESSAGE 'Dados Salvos com sucesso' TYPE 'I'.

  PERFORM init_aba.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_0103
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM fill_0103  USING    p_space.


  CLEAR: it_set_atv_alv[],
         it_area_venda_alv[].

  LOOP AT it_set_atv INTO wa_set_atv WHERE id = wa_canal_dist_alv-id AND
                                           seq_canal = wa_canal_dist_alv-seq_canal.

    MOVE-CORRESPONDING wa_set_atv TO wa_set_atv_alv.

    SELECT SINGLE vtext
         FROM tspat
         INTO @DATA(l_vtext)
          WHERE spart EQ @wa_set_atv_alv-spart
            AND spras EQ @sy-langu.


    wa_set_atv_alv-vtext = l_vtext.

    FREE wa_set_atv_alv-celltab.
    t_estilo =  VALUE #( ( fieldname = 'BUKRS' style = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no  )
                         ( fieldname = 'KTOKD' style = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no )
                         ( fieldname = 'SPART' style = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no ) ).

    INSERT LINES OF t_estilo INTO TABLE wa_set_atv_alv-celltab.
    APPEND wa_set_atv_alv TO it_set_atv_alv.
    CLEAR: wa_set_atv_alv , l_vtext.
  ENDLOOP.

  CLEAR: wa_set_atv_alv.
  LOOP AT it_set_atv_alv_aux INTO wa_set_atv_alv WHERE id        = wa_canal_dist_alv-id AND
                                                       seq_canal = wa_canal_dist_alv-seq_canal.

    SELECT SINGLE vtext
         FROM tspat
         INTO l_vtext
          WHERE spart EQ wa_set_atv_alv-spart
            AND spras EQ sy-langu.


    wa_set_atv_alv-vtext = l_vtext.

    APPEND wa_set_atv_alv TO it_set_atv_alv.
    CLEAR: wa_set_atv_alv , l_vtext.
  ENDLOOP.

  LOOP AT it_area_venda INTO wa_area_venda WHERE id = wa_canal_dist_alv-id AND
                                          seq_canal = wa_canal_dist_alv-seq_canal.
    MOVE-CORRESPONDING wa_area_venda TO wa_area_venda_alv.

    APPEND wa_area_venda_alv TO it_area_venda_alv.
    CLEAR: wa_area_venda_alv .
  ENDLOOP.

  LOOP AT it_area_venda_alv_aux INTO wa_area_venda_alv WHERE id = wa_canal_dist_alv-id AND
                                                         seq_canal = wa_canal_dist_alv-seq_canal.

    APPEND wa_area_venda_alv TO it_area_venda_alv.
    CLEAR: wa_area_venda_alv .
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_ABA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_aba .

  g_tab_strip_cli-pressed_tab = c_tab_strip_cliente-tab1.
  g_tab_strip_cli-subscreen = '0101'.

  PERFORM f_selecao_dados.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
FORM f_verifica_erros .

  CLEAR: it_msg_ret[].

  DATA: lva_count TYPE i.

* Valida Grupo de COntas Aba 1
  LOOP AT  it_grupo_contas_alv INTO DATA(lwa_grupo_contas_alv) WHERE criado = 'X' OR  modif = 'X'.
    IF lwa_grupo_contas_alv-ktokd IS NOT INITIAL.
      SELECT SINGLE ktokd
        INTO @DATA(lva_ktokd)
        FROM t077x
       WHERE ktokd = @lwa_grupo_contas_alv-ktokd
         AND spras = @sy-langu.

      IF sy-subrc <> 0.
*** Inicio - Rubenilson - 19.06.24 - #140377
        SELECT SINGLE ktokk
          INTO lva_ktokd
          FROM t077y
         WHERE ktokk = lwa_grupo_contas_alv-ktokd
           AND spras = sy-langu.
        IF sy-subrc IS NOT INITIAL.
*** Fim - Rubenilson - 19.06.24 - #140377
          MESSAGE s024(sd) WITH TEXT-102 DISPLAY LIKE 'E'.
          MOVE: TEXT-102    TO wa_msg_ret-msg,
          'KTOKD'           TO wa_msg_ret-field,
          sy-tabix          TO wa_msg_ret-tabix.
          APPEND wa_msg_ret TO it_msg_ret.
          CLEAR: wa_msg_ret.
        ENDIF."Rubenilson - 19.06.24 - #140377

      ENDIF.

      CLEAR: lva_count.
      LOOP AT it_grupo_contas_alv INTO DATA(lwa_grupo_contas) WHERE   bukrs = vg_bukrs AND
                                                                      ktokd = lwa_grupo_contas_alv-ktokd.
        lva_count = lva_count + 1.
      ENDLOOP.
      IF lva_count > 1.
        MESSAGE s024(sd) WITH TEXT-119 DISPLAY LIKE 'E'.
        MOVE: TEXT-119    TO wa_msg_ret-msg,
        'KTOKD'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.

    ENDIF.
  ENDLOOP.

* Valida Grupo de COntas Aba 2,
  CLEAR: lva_ktokd.
  LOOP AT  it_grupo_contas_alv_aux INTO DATA(lwa_grupo_contas_alv_aux) WHERE criado = 'X' OR  modif = 'X'.

    IF lwa_grupo_contas_alv_aux-akont IS NOT INITIAL.
      SELECT SINGLE saknr
        INTO @DATA(lva_akont)
        FROM skb1
       WHERE bukrs = @vg_bukrs
         AND saknr = @lwa_grupo_contas_alv_aux-akont.
      IF sy-subrc <> 0.
        MOVE: TEXT-105    TO wa_msg_ret-msg,
        'AKONT'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.

    IF lwa_grupo_contas_alv_aux-zuawa IS NOT INITIAL.
      SELECT SINGLE zuawa
        INTO @DATA(lva_zuawa)
      FROM  tzun
        WHERE zuawa = @lwa_grupo_contas_alv_aux-zuawa.

      IF sy-subrc <> 0.
        MOVE: TEXT-106    TO wa_msg_ret-msg,
        'ZUAWA'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.


    IF lwa_grupo_contas_alv_aux-fdgrv IS NOT INITIAL.
      SELECT SINGLE grupp
         INTO @DATA(lva_fdgrv)
         FROM  t035
        WHERE grupp = @lwa_grupo_contas_alv_aux-fdgrv.

      IF sy-subrc <> 0.
        MOVE: TEXT-107    TO wa_msg_ret-msg,
        'FDGRV'           TO wa_msg_ret-field,
         sy-tabix    TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.

    IF lwa_grupo_contas_alv_aux-zwels IS NOT INITIAL.
      DATA: it_t042e     TYPE TABLE OF t042e,
            lva_char(50) TYPE c.

      lva_char = lwa_grupo_contas_alv_aux-zwels.
      CONDENSE lva_char NO-GAPS.

      SELECT SINGLE zbukr FROM t042 INTO @DATA(lva_zbukr)
             WHERE bukrs = @vg_bukrs.

      SELECT * FROM t042e INTO TABLE it_t042e WHERE zbukr = lva_zbukr.

      WHILE lva_char NE space.
        SELECT * FROM t042z INTO TABLE @DATA(lit_t042z) WHERE land1 = 'BR'
                                         AND   zlsch = @lva_char(1).
        IF sy-subrc NE 0.

          CONCATENATE 'Forma de pagamento' lva_char(1) 'para o país BR não prevista' INTO wa_msg_ret-msg SEPARATED BY space.
          MESSAGE s024(sd) WITH wa_msg_ret-msg  DISPLAY LIKE 'E'.

          MOVE:
          'ZWELS'           TO wa_msg_ret-field,
           sy-tabix         TO wa_msg_ret-tabix.
          APPEND wa_msg_ret TO it_msg_ret.
          CLEAR: wa_msg_ret.


        ENDIF.

        READ TABLE it_t042e TRANSPORTING NO FIELDS WITH KEY zbukr  = lva_zbukr
                                                    zlsch  = lva_char(1).
        IF sy-subrc NE 0 AND lines( it_t042e ) NE 0.
          CONCATENATE 'Forma de pagamento' lva_char(1) 'para o país BR não prevista' INTO wa_msg_ret-msg SEPARATED BY space.
          MESSAGE s024(sd) WITH wa_msg_ret-msg  DISPLAY LIKE 'E'.

          MOVE: 'ZWELS' TO wa_msg_ret-field,
           sy-tabix          TO wa_msg_ret-tabix.
          APPEND wa_msg_ret TO it_msg_ret.
          CLEAR: wa_msg_ret.

        ENDIF.
        SHIFT lva_char.
      ENDWHILE.
    ENDIF.

    IF lwa_grupo_contas_alv_aux-zterm IS NOT INITIAL.
      SELECT SINGLE zterm
        INTO @DATA(lva_zterm)
        FROM  t052
       WHERE zterm = @lwa_grupo_contas_alv_aux-zterm.

      IF sy-subrc <> 0.
        MOVE: TEXT-126    TO wa_msg_ret-msg,
        'ZTERM'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.

  ENDLOOP.

* Valida Canal de distribuição
  CLEAR: lva_ktokd.
  LOOP AT  it_canal_dist_alv INTO DATA(lwa_canal_dist_alv) WHERE criado = 'X' OR  modif = 'X'..
    IF lwa_canal_dist_alv-vtweg IS NOT INITIAL.

      SELECT SINGLE vtweg
        INTO @DATA(lva_vtweg)
          FROM tvtwt
        WHERE  vtweg = @lwa_canal_dist_alv-vtweg
        AND spras = @sy-langu.

      IF sy-subrc <> 0.
        MESSAGE s024(sd) WITH TEXT-103 DISPLAY LIKE 'E'.
        MOVE: TEXT-103    TO wa_msg_ret-msg,
        'VTWEG'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
      "ENDIF.


*    READ TABLE it_canal_dist INTO DATA(lwa_it_canal_dist) WITH KEY id        = lwa_canal_dist_alv-id
*                                                                   seq_canal = lwa_canal_dist_alv-seq_canal
*                                                                   vtweg = lwa_canal_dist_alv-vtweg.
*
*    IF sy-subrc = 0.
*      MESSAGE s024(sd) WITH text-120 DISPLAY LIKE 'E'.
*      MOVE: text-120    TO wa_msg_ret-msg,
*      'VTWEG'           TO wa_msg_ret-field,
*      sy-tabix          TO wa_msg_ret-tabix.
*      APPEND wa_msg_ret TO it_msg_ret.
*      CLEAR: wa_msg_ret.
*    ENDIF.

      CLEAR: lva_count.
      LOOP AT it_canal_dist_alv INTO DATA(lwa_canal_dist) WHERE    id        = lwa_canal_dist_alv-id AND
                                                                   "seq_canal = lwa_canal_dist_alv-seq_canal AND
                                                                   vtweg = lwa_canal_dist_alv-vtweg.
        lva_count = lva_count + 1.
      ENDLOOP.
      IF lva_count > 1.
        MOVE: TEXT-120    TO wa_msg_ret-msg,
      'Canal'       TO wa_msg_ret-field,
      sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
        CLEAR: lva_count.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Valida Setor Atividade
  CLEAR: lva_ktokd.
  LOOP AT  it_set_atv_alv INTO DATA(lwa_set_atv_alv) WHERE criado = 'X' OR  modif = 'X'.
    IF lwa_set_atv_alv-spart IS NOT INITIAL.
      IF lwa_set_atv_alv-bukrs IS NOT INITIAL.
        SELECT SINGLE bukrs
          FROM t001
          INTO @DATA(lva_bukrs)
           WHERE bukrs EQ @lwa_set_atv_alv-bukrs
             AND spras EQ @sy-langu.

        IF sy-subrc <> 0.
          MOVE: TEXT-104    TO wa_msg_ret-msg,
          'BUKRS'           TO wa_msg_ret-field,
          sy-tabix          TO wa_msg_ret-tabix.
          APPEND wa_msg_ret TO it_msg_ret.
          CLEAR: wa_msg_ret.

        ENDIF.
      ENDIF.

      IF lwa_set_atv_alv-ktokd IS NOT INITIAL.
        SELECT SINGLE ktokd
          INTO lva_ktokd
          FROM t077x
         WHERE ktokd = lwa_set_atv_alv-ktokd
           AND spras = sy-langu.

        IF sy-subrc <> 0.
          MOVE: TEXT-102    TO wa_msg_ret-msg,
          'KTOKD'           TO wa_msg_ret-field,
          sy-tabix          TO wa_msg_ret-tabix.
          APPEND wa_msg_ret TO it_msg_ret.
          CLEAR: wa_msg_ret.
        ENDIF.

        CLEAR: lva_count.
        LOOP AT it_set_atv_alv INTO DATA(lwa_set_atv) WHERE    id        = lwa_set_atv_alv-id AND
                                                               seq_canal = lwa_set_atv_alv-seq_canal AND
                                                               bukrs = lwa_set_atv_alv-bukrs AND
                                                               ktokd = lwa_set_atv_alv-ktokd AND
                                                               spart = lwa_set_atv_alv-spart.
          lva_count = lva_count + 1.
        ENDLOOP.
        IF lva_count > 1.
          MOVE: TEXT-121    TO wa_msg_ret-msg,
          'ATIVIDADE'       TO wa_msg_ret-field,
          sy-tabix          TO wa_msg_ret-tabix.
          APPEND wa_msg_ret TO it_msg_ret.
          CLEAR: wa_msg_ret.
          CLEAR: lva_count.
        ENDIF.
      ENDIF.

      IF lwa_set_atv_alv-spart IS NOT INITIAL.
        SELECT SINGLE spart
          INTO @DATA(lva_spart)
          FROM tspa
         WHERE spart = @lwa_set_atv_alv-spart.

        IF sy-subrc <> 0.
          MOVE: TEXT-109    TO wa_msg_ret-msg,
          'SPART'           TO wa_msg_ret-field,
          sy-tabix          TO wa_msg_ret-tabix.
          APPEND wa_msg_ret TO it_msg_ret.
          CLEAR: wa_msg_ret.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Valida Area de Venda
  LOOP AT  it_area_venda_alv INTO DATA(lwa_area_venda_alv) WHERE criado = 'X' OR  modif = 'X'..

    IF lwa_area_venda_alv-waers IS NOT INITIAL.

      SELECT SINGLE waers
        INTO @DATA(lva_waers)
         FROM  tcurc
           WHERE waers = @lwa_area_venda_alv-waers.

      IF sy-subrc <> 0.
        MOVE: TEXT-116    TO wa_msg_ret-msg,
        'WAERS'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.

    IF lwa_area_venda_alv-versg IS NOT INITIAL.

      SELECT SINGLE stgku
        INTO @DATA(lva_versg)
         FROM  tvsdt
           WHERE stgku = @lwa_area_venda_alv-versg
            AND spras = @sy-langu.

      IF sy-subrc <> 0.
        MOVE: TEXT-127    TO wa_msg_ret-msg,
        'VERSG'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.

    IF lwa_area_venda_alv-kalks IS NOT INITIAL.

      SELECT SINGLE kalks
        INTO @DATA(lva_kalks)
         FROM  tvkdt
           WHERE kalks = @lwa_area_venda_alv-kalks
            AND spras = @sy-langu.

      IF sy-subrc <> 0.
        MOVE: TEXT-128     TO wa_msg_ret-msg,
        'KALKS'            TO wa_msg_ret-field,
        sy-tabix           TO wa_msg_ret-tabix.
        APPEND wa_msg_ret  TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.

    IF lwa_area_venda_alv-lprio IS NOT INITIAL.
      SELECT SINGLE lprio
        INTO @DATA(lva_lprio)
     FROM  tprio
      WHERE lprio = @lwa_area_venda_alv-lprio.

      IF sy-subrc <> 0.
        MOVE: TEXT-117    TO wa_msg_ret-msg,
        'LPRIO'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.

    IF lwa_area_venda_alv-vsbed IS NOT INITIAL.
      SELECT SINGLE vsbed
        INTO @DATA(lva_vsbed)
      FROM  tvsb
       WHERE vsbed = @lwa_area_venda_alv-vsbed.

      IF sy-subrc <> 0.
        MOVE: TEXT-118    TO wa_msg_ret-msg,
        'VSBED'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.

    IF lwa_area_venda_alv-kztlf IS NOT INITIAL.

      DATA: it_dd07v TYPE STANDARD TABLE OF dd07v.
      CALL FUNCTION 'GET_DOMAIN_VALUES'
        EXPORTING
          domname         = 'KZTLF'
        TABLES
          values_tab      = it_dd07v
        EXCEPTIONS
          no_values_found = 1
          OTHERS          = 2.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

      READ TABLE it_dd07v INTO DATA(lwa_dd07v) WITH KEY domvalue_l = lwa_area_venda_alv-kztlf.

      IF sy-subrc <> 0.
        MESSAGE s024(sd) WITH TEXT-125 DISPLAY LIKE 'E'.
        MOVE: TEXT-125    TO wa_msg_ret-msg,
        'KZTLF'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.


    IF lwa_area_venda_alv-perfk IS NOT INITIAL.
      SELECT SINGLE ident
        INTO @DATA(lva_perfk)
        FROM tfact
       WHERE ident = @lwa_area_venda_alv-perfk
         AND spras = @sy-langu.

      IF sy-subrc <> 0.
        MOVE: TEXT-123    TO wa_msg_ret-msg,
         'PERFK'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.

    IF lwa_area_venda_alv-ktgrd IS NOT INITIAL.
      SELECT SINGLE ktgrd
        INTO @DATA(lva_ktgrd)
      FROM tvkt
         WHERE ktgrd = @lwa_area_venda_alv-ktgrd .

      IF sy-subrc <> 0.
        MOVE: TEXT-114    TO wa_msg_ret-msg,
        'KTGRD'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.

    IF lwa_area_venda_alv-tatyp IS NOT INITIAL.
      SELECT SINGLE kschl
        INTO @DATA(lva_tatyp)
          FROM t685a
      WHERE kappl = 'V'
        AND kschl = @lwa_area_venda_alv-tatyp .

      IF sy-subrc <> 0.
        MOVE: TEXT-124    TO wa_msg_ret-msg,
        'TATYP'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.

    IF lwa_area_venda_alv-taxkd IS NOT INITIAL.
      SELECT SINGLE taxkd
        INTO @DATA(lva_taxkd)
        FROM  tskd
       WHERE taxkd = @lwa_area_venda_alv-taxkd .

      IF sy-subrc <> 0.
        MOVE: TEXT-115    TO wa_msg_ret-msg,
        'TAXKD'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.
  ENDLOOP.

*** Inicio - Rubenilson - 19.06.24 - #150257
* Valida Grupo de COntas Aba 2,
  CLEAR: lva_ktokd.
  LOOP AT  it_empresa_fornec_alv_aux INTO DATA(lwa_empresa_fornec_alv_aux) WHERE criado = 'X' OR  modif = 'X'.

    IF lwa_empresa_fornec_alv_aux-akont IS NOT INITIAL.
      SELECT SINGLE saknr
        INTO lva_akont
        FROM skb1
       WHERE bukrs = vg_bukrs
         AND saknr = lwa_empresa_fornec_alv_aux-akont.
      IF sy-subrc <> 0.
        MOVE: TEXT-105    TO wa_msg_ret-msg,
        'AKONT'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.

    IF lwa_empresa_fornec_alv_aux-fdgrv IS NOT INITIAL.
      SELECT SINGLE grupp
         INTO lva_fdgrv
         FROM  t035
        WHERE grupp = lwa_empresa_fornec_alv_aux-fdgrv.

      IF sy-subrc <> 0.
        MOVE: TEXT-107    TO wa_msg_ret-msg,
        'FDGRV'           TO wa_msg_ret-field,
         sy-tabix    TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.

    IF lwa_empresa_fornec_alv_aux-zwels IS NOT INITIAL.

      lva_char = lwa_empresa_fornec_alv_aux-zwels.
      CONDENSE lva_char NO-GAPS.

      SELECT SINGLE zbukr FROM t042 INTO lva_zbukr
             WHERE bukrs = vg_bukrs.

      SELECT * FROM t042e INTO TABLE it_t042e WHERE zbukr = lva_zbukr.

      WHILE lva_char NE space.
        SELECT * FROM t042z INTO TABLE lit_t042z WHERE land1 = 'BR'
                                         AND   zlsch = lva_char(1).
        IF sy-subrc NE 0.

          CONCATENATE 'Forma de pagamento' lva_char(1) 'para o país BR não prevista' INTO wa_msg_ret-msg SEPARATED BY space.
          MESSAGE s024(sd) WITH wa_msg_ret-msg  DISPLAY LIKE 'E'.

          MOVE:
          'ZWELS'           TO wa_msg_ret-field,
           sy-tabix         TO wa_msg_ret-tabix.
          APPEND wa_msg_ret TO it_msg_ret.
          CLEAR: wa_msg_ret.


        ENDIF.

        READ TABLE it_t042e TRANSPORTING NO FIELDS WITH KEY zbukr  = lva_zbukr
                                                    zlsch  = lva_char(1).
        IF sy-subrc NE 0 AND lines( it_t042e ) NE 0.
          CONCATENATE 'Forma de pagamento' lva_char(1) 'para o país BR não prevista' INTO wa_msg_ret-msg SEPARATED BY space.
          MESSAGE s024(sd) WITH wa_msg_ret-msg  DISPLAY LIKE 'E'.

          MOVE: 'ZWELS' TO wa_msg_ret-field,
           sy-tabix          TO wa_msg_ret-tabix.
          APPEND wa_msg_ret TO it_msg_ret.
          CLEAR: wa_msg_ret.

        ENDIF.
        SHIFT lva_char.
      ENDWHILE.
    ENDIF.

    IF lwa_empresa_fornec_alv_aux-zterm IS NOT INITIAL.
      SELECT SINGLE zterm
        INTO lva_zterm
        FROM  t052
       WHERE zterm = lwa_empresa_fornec_alv_aux-zterm.

      IF sy-subrc <> 0.
        MOVE: TEXT-126    TO wa_msg_ret-msg,
        'ZTERM'           TO wa_msg_ret-field,
        sy-tabix          TO wa_msg_ret-tabix.
        APPEND wa_msg_ret TO it_msg_ret.
        CLEAR: wa_msg_ret.
      ENDIF.
    ENDIF.
*** Fim - Rubenilson - 19.06.24 - #150257
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_ERRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_show_erro .
*  CALL FUNCTION 'Z_DOC_CHECK_NEW'
*    EXPORTING
*      i_screen      = '100'
*      i_show        = 'X'
*      i_repid       = sy-repid
*      i_pressed_tab = 'G_TAB_STRIP_CLI-PRESSED_TAB'
*      i_set_field   = 'X_FIELD'
*    IMPORTING
*      e_messagem    = wg_mensagem
*    TABLES
*      it_msgs       = it_msg_ret.

  IF it_msg_ret[] IS NOT INITIAL.
    DATA(lva_show) = 'X'.
  ELSE.
    CLEAR: lva_show.
  ENDIF.

  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen    = '100'
      i_show      = lva_show
      i_repid     = sy-repid
      i_popup     = 0
      i_set_field = 'X_FIELD'
    IMPORTING
      e_messagem  = wg_mensagem
    TABLES
      it_msgs     = it_msg_ret.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
FORM on_f4  USING f_fieldname    TYPE lvc_fname
                  f_fieldvalue   TYPE lvc_value
                  fw_row_no      TYPE lvc_s_roid
                  fcl_event_data TYPE REF TO cl_alv_event_data
                  ft_bad_cells   TYPE lvc_t_modi
                  f_display      TYPE char01.

  DATA: lw_modi  TYPE lvc_s_modi.
  DATA: lv_ktokd TYPE t077x-ktokd.

  FIELD-SYMBOLS: <lfst_modi> TYPE lvc_t_modi.

  ASSIGN fcl_event_data->m_data->* TO <lfst_modi>.
  CHECK sy-subrc = 0.

  READ TABLE  it_grupo_contas_alv INTO DATA(lwa_grupo_contas_alv) INDEX fw_row_no-row_id.

  IF lwa_grupo_contas_alv-criado = 'X'.

    CASE f_fieldname.
      WHEN 'KTOKD'.

        PERFORM f4_ktokd  USING lwa_grupo_contas_alv-ktokd
                          CHANGING lv_ktokd.

        f_fieldvalue = lv_ktokd.

        CLEAR lw_modi.
        lw_modi-row_id    = fw_row_no-row_id.
        lw_modi-fieldname = f_fieldname.
        lw_modi-value = f_fieldvalue.
        APPEND lw_modi TO <lfst_modi>.


        fcl_event_data->m_event_handled = 'X'.

    ENDCASE.
  ENDIF.
ENDFORM.                                                    " ON_F4
*&---------------------------------------------------------------------*
*&      Form  F4_KTOKD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_GRUPO_CONTAS_ALV_KTOKD  text
*      <--P_LV_KTOKD  text
*----------------------------------------------------------------------*
FORM f4_ktokd  USING  u_ktokd  TYPE t077x-ktokd
  CHANGING f_ktokd TYPE t077x-ktokd.

  TYPES : BEGIN OF ty_ktokd,
            ktokd TYPE t077x-ktokd,
            txt30 TYPE t077x-txt30,
          END OF ty_ktokd.

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_ktokd  TYPE TABLE OF ty_ktokd.

  DATA: ls_ktokd    TYPE ty_ktokd.
  DATA: lw_rettab TYPE ddshretval.

*** Inicio - Rubenilson - 19.06.24 - #140377
  SELECT  ktokk txt30
     FROM  t077y
     INTO TABLE lt_ktokd
      WHERE spras = sy-langu.
*** Fim - Rubenilson - 19.06.24 - #140377

  SELECT  ktokd txt30
     FROM  t077x
     APPENDING TABLE lt_ktokd
      WHERE spras = sy-langu.
  CHECK sy-subrc = 0.

  SORT lt_ktokd BY ktokd."Rubenilson - 19.06.24 - #140377

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'KTOKD'
      value_org       = 'S'
    TABLES
      value_tab       = lt_ktokd
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  f_ktokd = lw_rettab-fieldval.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ON_F4_0102
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
FORM on_f4_0102  USING f_fieldname    TYPE lvc_fname
                       f_fieldvalue   TYPE lvc_value
                       fw_row_no      TYPE lvc_s_roid
                       fcl_event_data TYPE REF TO cl_alv_event_data
                       ft_bad_cells   TYPE lvc_t_modi
                       f_display      TYPE char01.

  DATA: lw_modi  TYPE lvc_s_modi.
  DATA: lv_akont TYPE skb1-saknr,
        lv_zuawa TYPE tzun-zuawa,
        lv_fdgrv TYPE t035-grupp,
        lv_zwels TYPE knb1-zwels,
        lv_zterm TYPE t052-zterm.

  FIELD-SYMBOLS: <lfst_modi> TYPE lvc_t_modi.

  ASSIGN fcl_event_data->m_data->* TO <lfst_modi>.
  CHECK sy-subrc = 0.

  READ TABLE it_grupo_contas_alv_aux INTO DATA(lwa_grupo_contas_alv) INDEX fw_row_no-row_id.

  " IF lwa_grupo_contas_alv-criado = 'X'.

  CASE f_fieldname.
    WHEN 'AKONT'.

      PERFORM f4_akont  USING lwa_grupo_contas_alv-akont
                          CHANGING lv_akont.

      f_fieldvalue = lv_akont.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.


      fcl_event_data->m_event_handled = 'X'.
    WHEN 'ZUAWA'.

      PERFORM f4_zuawa  USING lwa_grupo_contas_alv-zuawa
                  CHANGING lv_zuawa.

      f_fieldvalue = lv_zuawa.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.

      fcl_event_data->m_event_handled = 'X'.


    WHEN 'FDGRV'.
      PERFORM f4_fdgrv  USING lwa_grupo_contas_alv-fdgrv
          CHANGING lv_fdgrv.

      f_fieldvalue = lv_fdgrv.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.

      fcl_event_data->m_event_handled = 'X'.

    WHEN 'ZWELS'.
      PERFORM f4_zwels USING lwa_grupo_contas_alv-zwels
          CHANGING lv_zwels.

      f_fieldvalue = lv_zwels.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.

      fcl_event_data->m_event_handled = 'X'.

    WHEN 'ZTERM'.
      PERFORM f4_zterm  USING lwa_grupo_contas_alv-zterm
                              'D'
          CHANGING lv_zterm.

      f_fieldvalue = lv_zterm.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.

      fcl_event_data->m_event_handled = 'X'.


  ENDCASE.
  " ENDIF.
ENDFORM.

*** Inicio - Rubenilson - 19.06.24 - #150257
*&---------------------------------------------------------------------*
*&      Form  ON_F4_0102
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
FORM on_f4_0105  USING f_fieldname    TYPE lvc_fname
                       f_fieldvalue   TYPE lvc_value
                       fw_row_no      TYPE lvc_s_roid
                       fcl_event_data TYPE REF TO cl_alv_event_data
                       ft_bad_cells   TYPE lvc_t_modi
                       f_display      TYPE char01.

  DATA: lw_modi  TYPE lvc_s_modi.
  DATA: lv_akont TYPE skb1-saknr,
        lv_zuawa TYPE tzun-zuawa,
        lv_fdgrv TYPE t035-grupp,
        lv_zwels TYPE knb1-zwels,
        lv_zterm TYPE t052-zterm,
        lv_qland TYPE lfb1-qland.

  FIELD-SYMBOLS: <lfst_modi> TYPE lvc_t_modi.

  ASSIGN fcl_event_data->m_data->* TO <lfst_modi>.
  CHECK sy-subrc = 0.

  READ TABLE it_empresa_fornec_alv_aux INTO DATA(lwa_empresa_fornec_alv) INDEX fw_row_no-row_id.

  " IF lwa_grupo_contas_alv-criado = 'X'.

  CASE f_fieldname.
    WHEN 'AKONT'.

      PERFORM f4_akont  USING lwa_empresa_fornec_alv-akont
                          CHANGING lv_akont.

      f_fieldvalue = lv_akont.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.


      fcl_event_data->m_event_handled = 'X'.
    WHEN 'ZUAWA'.
*
*      PERFORM f4_zuawa  USING lwa_empresa_fornec_alv-zuawa
*                  CHANGING lv_zuawa.
*
*      f_fieldvalue = lv_zuawa.
*
*      CLEAR lw_modi.
*      lw_modi-row_id    = fw_row_no-row_id.
*      lw_modi-fieldname = f_fieldname.
*      lw_modi-value = f_fieldvalue.
*      APPEND lw_modi TO <lfst_modi>.
*
*      fcl_event_data->m_event_handled = 'X'.


    WHEN 'FDGRV'.
      PERFORM f4_fdgrv  USING lwa_empresa_fornec_alv-fdgrv
          CHANGING lv_fdgrv.

      f_fieldvalue = lv_fdgrv.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.

      fcl_event_data->m_event_handled = 'X'.

    WHEN 'ZWELS'.
      PERFORM f4_zwels USING lwa_empresa_fornec_alv-zwels
          CHANGING lv_zwels.

      f_fieldvalue = lv_zwels.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.

      fcl_event_data->m_event_handled = 'X'.

    WHEN 'ZTERM'.
      PERFORM f4_zterm  USING lwa_empresa_fornec_alv-zterm
                              'K'
          CHANGING lv_zterm.

      f_fieldvalue = lv_zterm.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.

      fcl_event_data->m_event_handled = 'X'.
*** Inicio - Rubenilson - 19.06.24 - #140377
    WHEN 'QLAND'.
      PERFORM f4_qland  USING lwa_empresa_fornec_alv-qland
          CHANGING lv_qland.

      f_fieldvalue = lv_qland.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.

      fcl_event_data->m_event_handled = 'X'.
*** Fim - Rubenilson - 19.06.24 - #140377
  ENDCASE.
  " ENDIF.
ENDFORM.

*** Inicio - Rubenilson - 19.06.24 - #140377
*&---------------------------------------------------------------------*
*&      Form  ON_F4_0102
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
FORM on_f4_0106  USING f_fieldname    TYPE lvc_fname
                       f_fieldvalue   TYPE lvc_value
                       fw_row_no      TYPE lvc_s_roid
                       fcl_event_data TYPE REF TO cl_alv_event_data
                       ft_bad_cells   TYPE lvc_t_modi
                       f_display      TYPE char01.

  DATA: lw_modi  TYPE lvc_s_modi.
  DATA: lv_akont TYPE skb1-saknr,
        lv_zuawa TYPE tzun-zuawa,
        lv_fdgrv TYPE t035-grupp,
        lv_zwels TYPE knb1-zwels,
        lv_zterm TYPE t052-zterm,
        lv_qland TYPE lfb1-qland.

  FIELD-SYMBOLS: <lfst_modi> TYPE lvc_t_modi.

  ASSIGN fcl_event_data->m_data->* TO <lfst_modi>.
  CHECK sy-subrc = 0.

  READ TABLE it_dados_compra_alv_aux INTO DATA(lwa_dados_compra_alv) INDEX fw_row_no-row_id.

  " IF lwa_grupo_contas_alv-criado = 'X'.

  CASE f_fieldname.
*    WHEN 'AKONT'.
*
*      PERFORM f4_akont  USING lwa_empresa_fornec_alv-akont
*                          CHANGING lv_akont.
*
*      f_fieldvalue = lv_akont.
*
*      CLEAR lw_modi.
*      lw_modi-row_id    = fw_row_no-row_id.
*      lw_modi-fieldname = f_fieldname.
*      lw_modi-value = f_fieldvalue.
*      APPEND lw_modi TO <lfst_modi>.
*
*
*      fcl_event_data->m_event_handled = 'X'.
*    WHEN 'ZUAWA'.
**
**      PERFORM f4_zuawa  USING lwa_empresa_fornec_alv-zuawa
**                  CHANGING lv_zuawa.
**
**      f_fieldvalue = lv_zuawa.
**
**      CLEAR lw_modi.
**      lw_modi-row_id    = fw_row_no-row_id.
**      lw_modi-fieldname = f_fieldname.
**      lw_modi-value = f_fieldvalue.
**      APPEND lw_modi TO <lfst_modi>.
**
**      fcl_event_data->m_event_handled = 'X'.
*
*
*    WHEN 'FDGRV'.
*      PERFORM f4_fdgrv  USING lwa_dados_compra_alv-fdgrv
*          CHANGING lv_fdgrv.
*
*      f_fieldvalue = lv_fdgrv.
*
*      CLEAR lw_modi.
*      lw_modi-row_id    = fw_row_no-row_id.
*      lw_modi-fieldname = f_fieldname.
*      lw_modi-value = f_fieldvalue.
*      APPEND lw_modi TO <lfst_modi>.
*
*      fcl_event_data->m_event_handled = 'X'.
*
*    WHEN 'ZWELS'.
*      PERFORM f4_zwels USING lwa_empresa_fornec_alv-zwels
*          CHANGING lv_zwels.
*
*      f_fieldvalue = lv_zwels.
*
*      CLEAR lw_modi.
*      lw_modi-row_id    = fw_row_no-row_id.
*      lw_modi-fieldname = f_fieldname.
*      lw_modi-value = f_fieldvalue.
*      APPEND lw_modi TO <lfst_modi>.
*
*      fcl_event_data->m_event_handled = 'X'.
*
    WHEN 'ZTERM'.
*      PERFORM f4_zterm  USING lwa_dados_compra_alv-zterm
*          CHANGING lv_zterm.
*
*      f_fieldvalue = lv_zterm.
*
*      CLEAR lw_modi.
*      lw_modi-row_id    = fw_row_no-row_id.
*      lw_modi-fieldname = f_fieldname.
*      lw_modi-value = f_fieldvalue.
*      APPEND lw_modi TO <lfst_modi>.
*
*      fcl_event_data->m_event_handled = 'X'.

*    WHEN 'QLAND'.
*      PERFORM f4_qland  USING lwa_empresa_fornec_alv-qland
*          CHANGING lv_qland.
*
*      f_fieldvalue = lv_qland.
*
*      CLEAR lw_modi.
*      lw_modi-row_id    = fw_row_no-row_id.
*      lw_modi-fieldname = f_fieldname.
*      lw_modi-value = f_fieldvalue.
*      APPEND lw_modi TO <lfst_modi>.
*
*      fcl_event_data->m_event_handled = 'X'.
  ENDCASE.
  " ENDIF.
ENDFORM.

*** Fim - Rubenilson - 19.06.24 - #150257
*&---------------------------------------------------------------------*
*&      Form  F4_AKONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_GRUPO_CONTAS_ALV_AKONT  text
*      <--P_LV_AKONT  text
*----------------------------------------------------------------------*
FORM f4_akont  USING   u_akont  TYPE skb1-saknr
               CHANGING f_akont TYPE skb1-saknr.


  TYPES : BEGIN OF ty_akont,
            bukrs TYPE skb1-bukrs,
            saknr TYPE skb1-saknr,
          END OF ty_akont.

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_akont  TYPE TABLE OF ty_akont.

  DATA: ls_akont    TYPE ty_akont.
  DATA: lw_rettab TYPE ddshretval.

*** Inicio - Rubenilson - 19.06.24 - #140377
*  SELECT  bukrs saknr
*     FROM  skb1
*     INTO TABLE lt_akont
*      WHERE bukrs = vg_bukrs.
*  " AND saknr =  u_akont.
*
*  CHECK sy-subrc = 0.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'AKONT'
*      value_org       = 'S'
*    TABLES
*      value_tab       = lt_akont
*      return_tab      = lt_rettab
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*  CHECK sy-subrc = 0.
*
*  READ TABLE lt_rettab INTO lw_rettab INDEX 1.
*
*  CHECK sy-subrc = 0.
*
*  f_akont = lw_rettab-fieldval.
  CALL FUNCTION 'FI_F4_AKONT'
    EXPORTING
      i_bukrs        = vg_bukrs
      i_mitkz        = 'K'
    IMPORTING
      e_akont        = f_akont
    EXCEPTIONS
      invalid_call   = 1
      nothing_found  = 2
      internal_error = 3
      OTHERS         = 4.

*** Fim - Rubenilson - 19.06.24 - #140377
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_ZUAWA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_GRUPO_CONTAS_ALV_ZUAWA  text
*      <--P_LV_ZUAWA  text
*----------------------------------------------------------------------*
FORM f4_zuawa  USING    p_lwa_grupo_contas_alv_zuawa
               CHANGING p_lv_zuawa.



  TYPES : BEGIN OF ty_zuawa,
            zuawa TYPE tzun-zuawa,
            ttext TYPE tzunt-ttext,
          END OF ty_zuawa.

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_zuawa  TYPE TABLE OF ty_zuawa.

  DATA: ls_zuawa    TYPE ty_zuawa.
  DATA: lw_rettab TYPE ddshretval.

  SELECT zuawa ttext
      FROM  tzunt
         INTO TABLE lt_zuawa
    WHERE spras = sy-langu.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZUAWA'
      value_org       = 'S'
    TABLES
      value_tab       = lt_zuawa
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  p_lv_zuawa = lw_rettab-fieldval.




ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_FDGRV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_GRUPO_CONTAS_ALV_FDGRV  text
*      <--P_LV_FDGRV  text
*----------------------------------------------------------------------*
FORM f4_fdgrv  USING    p_lwa_grupo_contas_alv_fdgrv
               CHANGING p_lv_fdgrv.

  TYPES : BEGIN OF ty_fdgrv,
            grupp TYPE t035-grupp,
            textl TYPE t035t-textl,
          END OF ty_fdgrv.

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_fdgrv  TYPE TABLE OF ty_fdgrv.

  DATA: ls_fdgrv    TYPE ty_fdgrv.
  DATA: lw_rettab TYPE ddshretval.

  SELECT t1~grupp t2~textl
      FROM  t035 AS t1
     INNER JOIN t035t AS t2
    ON t1~grupp = t2~grupp
         INTO TABLE lt_fdgrv
    WHERE t1~grupp = t2~grupp
     AND t2~spras = sy-langu.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'GRUPP'
      value_org       = 'S'
    TABLES
      value_tab       = lt_fdgrv
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  p_lv_fdgrv = lw_rettab-fieldval.

ENDFORM.

*** Inicio - Rubenilson - 19.06.24 - #150257
FORM f4_QLAND  USING    p_lwa_EMPRESA_FORNEC_alv_qland
               CHANGING p_lv_QLAND.

  TYPES : BEGIN OF ty_qland,
            land1 TYPE t005r-land1,
            qland TYPE t005r-qland,
            qltxt TYPE t005r-qltxt,
          END OF ty_qland.

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_qland  TYPE TABLE OF ty_qland.

  DATA: ls_fdgrv    TYPE ty_qland.
  DATA: lw_rettab TYPE ddshretval.

  SELECT land1 qland qltxt
      FROM t005r
      INTO TABLE lt_qland
    WHERE spras = sy-langu.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'QLAND'
      value_org       = 'S'
    TABLES
      value_tab       = lt_qland
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  p_lv_qland = lw_rettab-fieldval.

ENDFORM.
*** Fim - Rubenilson - 19.06.24 - #150257

*&---------------------------------------------------------------------*
*&      Form  F4_ZWELS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_GRUPO_CONTAS_ALV_ZWELS  text
*      <--P_LV_ZWELS  text
*----------------------------------------------------------------------*
FORM f4_zwels  USING    p_lwa_grupo_contas_alv_zwels
               CHANGING p_lv_zwels.

  index = 1.
  CALL SCREEN 0104 STARTING AT 03 01 ENDING AT 80 25.

  p_lv_zwels = wa_grupo_contas_alv-zwels.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_ZTERM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_GRUPO_CONTAS_ALV_ZTERM  text
*      <--P_LV_ZTERM  text
*----------------------------------------------------------------------*
FORM f4_zterm  USING    p_lwa_grupo_contas_alv_zterm
                        p_koart TYPE koart"Rubenilson - 19.06.24 - #140377
               CHANGING p_lv_zterm.


  DATA zterm TYPE t052-zterm.

  CALL FUNCTION 'FI_F4_ZTERM'
    EXPORTING
      i_koart       = p_koart " Rubenilson - 19.06.24 - #140377
    IMPORTING
      e_zterm       = zterm
    EXCEPTIONS
      nothing_found = 01.

  IF zterm IS NOT INITIAL.
    p_lv_zterm = zterm.
  ENDIF.

*  TYPES : BEGIN OF ty_zterm,
*            zterm TYPE t052u-zterm,
*            text1 TYPE t052u-text1,
*          END OF ty_zterm.
*
*  DATA: lt_rettab TYPE TABLE OF ddshretval,
*        lt_zterm  TYPE TABLE OF ty_zterm.
*
*  DATA: ls_zuawa    TYPE ty_zterm.
*  DATA: lw_rettab TYPE ddshretval.
*
*  SELECT zterm text1
*      FROM  t052u
*         INTO TABLE lt_zterm
*    WHERE spras = sy-langu.
*
*  CHECK sy-subrc = 0.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'ZTERM'
*      value_org       = 'S'
*    TABLES
*      value_tab       = lt_zterm
*      return_tab      = lt_rettab
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*  CHECK sy-subrc = 0.
*
*  READ TABLE lt_rettab INTO lw_rettab INDEX 1.
*
*  CHECK sy-subrc = 0.




ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ON_F4_0103
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
FORM on_f4_0103  USING f_fieldname    TYPE lvc_fname
                       f_fieldvalue   TYPE lvc_value
                       fw_row_no      TYPE lvc_s_roid
                       fcl_event_data TYPE REF TO cl_alv_event_data
                       ft_bad_cells   TYPE lvc_t_modi
                       f_display      TYPE char01.

  DATA: lw_modi  TYPE lvc_s_modi.
  DATA: lv_vtweg TYPE tvtwt-vtweg.

  FIELD-SYMBOLS: <lfst_modi> TYPE lvc_t_modi.

  ASSIGN fcl_event_data->m_data->* TO <lfst_modi>.
  CHECK sy-subrc = 0.

  READ TABLE it_canal_dist_alv INTO DATA(lwa_canal_dist_alv) INDEX fw_row_no-row_id.

  IF lwa_canal_dist_alv-criado = 'X'.

    CASE f_fieldname.
      WHEN 'VTWEG'.

        PERFORM f4_vtweg  USING lwa_canal_dist_alv-vtweg
                            CHANGING lv_vtweg.

        f_fieldvalue = lv_vtweg.

        CLEAR lw_modi.
        lw_modi-row_id    = fw_row_no-row_id.
        lw_modi-fieldname = f_fieldname.
        lw_modi-value = f_fieldvalue.
        APPEND lw_modi TO <lfst_modi>.


        fcl_event_data->m_event_handled = 'X'.

    ENDCASE.
  ENDIF.
ENDFORM.
*
*&---------------------------------------------------------------------*
*&      Form  F4_VTWEG
*&---------------------------------------------------------------------*
FORM f4_vtweg  USING    p_lwa_grupo_contas_alv_vtweg
               CHANGING p_lv_vtweg.


  TYPES : BEGIN OF ty_vtweg ,
            vtweg TYPE tvtwt-vtweg,
            vtext TYPE tvtwt-vtext,
          END OF ty_vtweg .

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_vtweg  TYPE TABLE OF ty_vtweg.

  DATA: ls_vtweg    TYPE ty_vtweg.
  DATA: lw_rettab TYPE ddshretval.

  SELECT vtweg  vtext
      FROM tvtwt
         INTO TABLE lt_vtweg
    WHERE spras = sy-langu.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VTWEG'
      value_org       = 'S'
    TABLES
      value_tab       = lt_vtweg
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  p_lv_vtweg  = lw_rettab-fieldval.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ON_F4_0103_1
*&---------------------------------------------------------------------*
FORM on_f4_0103_1  USING f_fieldname    TYPE lvc_fname
                         f_fieldvalue   TYPE lvc_value
                         fw_row_no      TYPE lvc_s_roid
                         fcl_event_data TYPE REF TO cl_alv_event_data
                         ft_bad_cells   TYPE lvc_t_modi
                         f_display      TYPE char01.

  DATA: lw_modi  TYPE lvc_s_modi.
  DATA: lv_spart TYPE tspa-spart.

  FIELD-SYMBOLS: <lfst_modi> TYPE lvc_t_modi.

  ASSIGN fcl_event_data->m_data->* TO <lfst_modi>.
  CHECK sy-subrc = 0.

  READ TABLE it_set_atv_alv  INTO DATA(lwa_set_atv_alv) INDEX fw_row_no-row_id.

  IF lwa_set_atv_alv-criado = 'X'.

    CASE f_fieldname.
      WHEN 'SPART'.

        PERFORM f4_spart  USING lwa_set_atv_alv-spart
                            CHANGING lv_spart.

        f_fieldvalue = lv_spart.

        CLEAR lw_modi.
        lw_modi-row_id    = fw_row_no-row_id.
        lw_modi-fieldname = f_fieldname.
        lw_modi-value = f_fieldvalue.
        APPEND lw_modi TO <lfst_modi>.


        fcl_event_data->m_event_handled = 'X'.

    ENDCASE.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_SPART
*&---------------------------------------------------------------------*
FORM f4_spart  USING    p_lwa_set_atv_alv-spart
               CHANGING p_lv_spart.


  TYPES : BEGIN OF ty_spart ,
            spart TYPE tspa-spart,
            vtext TYPE tspat-vtext,
          END OF ty_spart .

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_spart  TYPE TABLE OF ty_spart.

  DATA: ls_spart    TYPE ty_spart.
  DATA: lw_rettab TYPE ddshretval.

*  SELECT spart
*      from  tspa

  SELECT spart vtext
       FROM tspat
       INTO TABLE lt_spart
        WHERE spras EQ sy-langu.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SPART'
      value_org       = 'S'
    TABLES
      value_tab       = lt_spart
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  p_lv_spart  = lw_rettab-fieldval.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ON_F4_0103_2
*&---------------------------------------------------------------------*
FORM on_f4_0103_2  USING f_fieldname    TYPE lvc_fname
                       f_fieldvalue   TYPE lvc_value
                       fw_row_no      TYPE lvc_s_roid
                       fcl_event_data TYPE REF TO cl_alv_event_data
                       ft_bad_cells   TYPE lvc_t_modi
                       f_display      TYPE char01.

*  DATA: lw_modi  TYPE lvc_s_modi.
*  DATA: lv_parvw TYPE tpar-parvw.
*
*  FIELD-SYMBOLS: <lfst_modi> TYPE lvc_t_modi.
*
*  ASSIGN fcl_event_data->m_data->* TO <lfst_modi>.
*  CHECK sy-subrc = 0.
*
*  READ TABLE it_parceiros_alv   INTO DATA(lwa_parceiros) INDEX fw_row_no-row_id.
*
*  IF lwa_parceiros-criado = 'X'.
*
*    CASE f_fieldname.
*      WHEN 'PARVW'.
*
*        PERFORM f4_parvw  USING lwa_parceiros-parvw
*                            CHANGING lv_parvw.
*
*        f_fieldvalue = lv_parvw.
*
*        CLEAR lw_modi.
*        lw_modi-row_id    = fw_row_no-row_id.
*        lw_modi-fieldname = f_fieldname.
*        lw_modi-value = f_fieldvalue.
*        APPEND lw_modi TO <lfst_modi>.
*
*
*        fcl_event_data->m_event_handled = 'X'.
*
*    ENDCASE.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_SPART
*&---------------------------------------------------------------------*
FORM f4_parvw  USING    p_lwa_parceiros-parvw
               CHANGING p_lv_parvw.


  TYPES : BEGIN OF ty_parvw ,
            parvw TYPE tpar-parvw,
            vtext TYPE tpart-vtext,
          END OF ty_parvw .

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_parvw  TYPE TABLE OF ty_parvw.

  DATA: ls_parvw    TYPE ty_parvw.
  DATA: lw_rettab TYPE ddshretval.

  SELECT parvw vtext
      FROM  tpart
         INTO TABLE lt_parvw.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PARVW'
      value_org       = 'S'
    TABLES
      value_tab       = lt_parvw
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  p_lv_parvw  = lw_rettab-fieldval.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ON_F4_0103_3
*&---------------------------------------------------------------------*
FORM on_f4_0103_3  USING f_fieldname    TYPE lvc_fname
                         f_fieldvalue   TYPE lvc_value
                         fw_row_no      TYPE lvc_s_roid
                         fcl_event_data TYPE REF TO cl_alv_event_data
                         ft_bad_cells   TYPE lvc_t_modi
                         f_display      TYPE char01.

  DATA: lw_modi  TYPE lvc_s_modi.
  DATA: lv_waers TYPE tcurc-waers,
        lv_versg TYPE knvv-versg,
        lv_kalks TYPE knvv-kalks,
        lv_lprio TYPE tprio-lprio,
        lv_vsbed TYPE tvsb-vsbed,
        lv_kztlf TYPE kztlf,
        lv_perfk TYPE knvv-perfk,
        lv_tatyp TYPE t685a-kschl,
        lv_taxkd TYPE tskd-taxkd.

  FIELD-SYMBOLS: <lfst_modi> TYPE lvc_t_modi.

  ASSIGN fcl_event_data->m_data->* TO <lfst_modi>.
  CHECK sy-subrc = 0.

  READ TABLE it_area_venda_alv   INTO DATA(lwa_area_venda_alv) INDEX fw_row_no-row_id.

*  IF lwa_area_venda_alv-criado = 'X'.

  CASE f_fieldname.
    WHEN  'WAERS'.
      PERFORM f4_waers  USING lwa_area_venda_alv-waers
                          CHANGING lv_waers.

      f_fieldvalue = lv_waers.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.
      fcl_event_data->m_event_handled = 'X'.

    WHEN  'VERSG'.
      PERFORM f4_versg  USING lwa_area_venda_alv-versg
                          CHANGING lv_versg.

      f_fieldvalue = lv_versg.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.
      fcl_event_data->m_event_handled = 'X'.


    WHEN  'KALKS'.
      PERFORM f4_kalks  USING lwa_area_venda_alv-kalks
                          CHANGING lv_kalks.

      f_fieldvalue = lv_kalks.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.
      fcl_event_data->m_event_handled = 'X'.

    WHEN  'LPRIO'.
      PERFORM f4_lprio USING lwa_area_venda_alv-lprio
                          CHANGING lv_lprio.

      f_fieldvalue = lv_lprio.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.
      fcl_event_data->m_event_handled = 'X'.

    WHEN  'VSBED'.
      PERFORM f4_vsbed USING lwa_area_venda_alv-vsbed
                          CHANGING lv_vsbed.

      f_fieldvalue = lv_vsbed.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.


      fcl_event_data->m_event_handled = 'X'.

*      WHEN  'KZAZU'. - é o X
*        PERFORM f4_parvw  USING lwa_parceiros-parvw
*                            CHANGING lv_parvw.
*
*        f_fieldvalue = lv_parvw.
*
*        CLEAR lw_modi.
*        lw_modi-row_id    = fw_row_no-row_id.
*        lw_modi-fieldname = f_fieldname.
*        lw_modi-value = f_fieldvalue.
*        APPEND lw_modi TO <lfst_modi>.
*
*
*        fcl_event_data->m_event_handled = 'X'.
    WHEN  'KZTLF'.
      PERFORM f4_kztlf  USING lwa_area_venda_alv-kztlf
                          CHANGING lv_kztlf.

      f_fieldvalue = lv_kztlf.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.


      fcl_event_data->m_event_handled = 'X'.
    WHEN  'PERFK'.
      PERFORM f4_perfk USING lwa_area_venda_alv-perfk
                          CHANGING lv_perfk.

      f_fieldvalue = lv_perfk.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.


      fcl_event_data->m_event_handled = 'X'.

    WHEN  'TATYP'.
      PERFORM f4_tatyp  USING lwa_area_venda_alv-tatyp
                          CHANGING lv_tatyp.

      f_fieldvalue = lv_tatyp.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.

      fcl_event_data->m_event_handled = 'X'.
    WHEN  'TAXKD'.
      PERFORM f4_taxkd  USING lwa_area_venda_alv-taxkd
                          CHANGING lv_taxkd lv_tatyp.

      f_fieldvalue = lv_taxkd.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.


      f_fieldvalue = lv_tatyp.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = 'TATYP'.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.

      fcl_event_data->m_event_handled = 'X'.



  ENDCASE.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_WAERS
*&---------------------------------------------------------------------*
FORM f4_waers  USING    p_lwa_area_venda_alv_waers
               CHANGING p_lv_waers.

  TYPES : BEGIN OF ty_waers ,
            waers TYPE tcurc-waers,
          END OF ty_waers .

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_waers  TYPE TABLE OF ty_waers.

  DATA: ls_waers    TYPE ty_waers.
  DATA: lw_rettab TYPE ddshretval.

  SELECT waers
      FROM  tcurc
         INTO TABLE lt_waers.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'WAERS'
      value_org       = 'S'
    TABLES
      value_tab       = lt_waers
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  p_lv_waers  = lw_rettab-fieldval.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_VERSG
*&---------------------------------------------------------------------*
FORM f4_versg  USING    p_lwa_area_venda_alv_versg
               CHANGING p_lv_versg.

  TYPES : BEGIN OF ty_versg ,
            stgku   TYPE tvsdt-stgku,
            bezei20 TYPE tvsdt-bezei20,
          END OF ty_versg .

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_versg  TYPE TABLE OF ty_versg.

  DATA: ls_versg    TYPE ty_versg.
  DATA: lw_rettab TYPE ddshretval.

  SELECT stgku
         bezei20
      FROM  tvsdt
         INTO TABLE lt_versg
       WHERE spras = sy-langu..

  CHECK sy-subrc = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'STGKU'
      value_org       = 'S'
    TABLES
      value_tab       = lt_versg
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  p_lv_versg  = lw_rettab-fieldval.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_KALKS
*&---------------------------------------------------------------------*
FORM f4_kalks  USING    p_lwa_area_venda_alv_kalks
               CHANGING p_lv_kalks.

  TYPES : BEGIN OF ty_kalks ,
            kalks TYPE tvkdt-kalks,
            vtext TYPE tvkdt-vtext,
          END OF ty_kalks .

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_kalks  TYPE TABLE OF ty_kalks.

  DATA: ls_kalks  TYPE ty_kalks.
  DATA: lw_rettab TYPE ddshretval.

  SELECT kalks
         vtext
      INTO TABLE lt_kalks
       FROM  tvkdt
         WHERE spras = sy-langu.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'KALKS'
      value_org       = 'S'
    TABLES
      value_tab       = lt_kalks
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  p_lv_kalks   = lw_rettab-fieldval.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_WAERS
*&---------------------------------------------------------------------*
FORM f4_lprio  USING    p_lwa_area_venda_alv_lprio
               CHANGING p_lv_lprio.

  TYPES : BEGIN OF ty_lprio ,
            lprio TYPE tprio-lprio,
          END OF ty_lprio .

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_lprio  TYPE TABLE OF ty_lprio.

  DATA: ls_lprio    TYPE ty_lprio.
  DATA: lw_rettab TYPE ddshretval.

  SELECT lprio
      FROM  tprio
         INTO TABLE lt_lprio.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LPRIO'
      value_org       = 'S'
    TABLES
      value_tab       = lt_lprio
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  p_lv_lprio  = lw_rettab-fieldval.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_VSBED
*&---------------------------------------------------------------------*
FORM f4_vsbed  USING    p_lwa_area_venda_alv_vsbed
               CHANGING p_lv_vsbed.

  TYPES : BEGIN OF ty_vsbed ,
            vsbed TYPE tvsb-vsbed,
          END OF ty_vsbed .

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_vsbed  TYPE TABLE OF ty_vsbed.

  DATA: ls_vsbed    TYPE ty_vsbed.
  DATA: lw_rettab TYPE ddshretval.

  SELECT vsbed
      FROM  tvsb
         INTO TABLE lt_vsbed.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VSBED'
      value_org       = 'S'
    TABLES
      value_tab       = lt_vsbed
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  p_lv_vsbed  = lw_rettab-fieldval.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_ANTLF
*&---------------------------------------------------------------------*
FORM f4_kztlf  USING    p_lwa_area_venda_alv_antlf
               CHANGING p_lv_kztlf.


  DATA: lt_rettab TYPE TABLE OF ddshretval.
  DATA: lw_rettab TYPE ddshretval.

  DATA: t_dd07v TYPE STANDARD TABLE OF dd07v,
        s_dd07v TYPE dd07v.

  DATA: BEGIN OF s_data,
          domvalue_l TYPE domvalue_l,
          ddtext     TYPE val_text,
        END OF s_data,
        t_data LIKE TABLE OF s_data.

* elemento de dados KZTLF.

* Get the domain values
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'KZTLF'   " Give your domain here
    TABLES
      values_tab      = t_dd07v
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

* Prepare the data.
  LOOP AT t_dd07v INTO s_dd07v.
    MOVE-CORRESPONDING s_dd07v TO s_data.
    APPEND s_data TO t_data.
  ENDLOOP.

  IF t_data IS NOT INITIAL.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'DOMVALUE_L'
        value_org       = 'S'
      TABLES
        value_tab       = t_data
        return_tab      = lt_rettab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    CHECK sy-subrc = 0.

    READ TABLE lt_rettab INTO lw_rettab INDEX 1.

    CHECK sy-subrc = 0.

    p_lv_kztlf  = lw_rettab-fieldval.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_PERFK
*&---------------------------------------------------------------------*
FORM f4_perfk  USING    p_lwa_area_venda_alv_perfk
               CHANGING p_lv_perfk.

  TYPES : BEGIN OF ty_perfk ,
            ident TYPE tfacd-ident,
            ltext TYPE tfact-ltext,
          END OF ty_perfk .

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_perfk  TYPE TABLE OF ty_perfk.

  DATA: ls_perfk    TYPE ty_perfk.
  DATA: lw_rettab TYPE ddshretval.

  SELECT ident ltext
      FROM tfact
         INTO TABLE lt_perfk
          WHERE spras = sy-langu.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'IDENT'
      value_org       = 'S'
    TABLES
      value_tab       = lt_perfk
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  p_lv_perfk = lw_rettab-fieldval.

ENDFORM.
*&      Form  F4_KTGRD
*&---------------------------------------------------------------------*
FORM f4_ktgrd   USING    p_lwa_area_venda_alv_ktgrd
               CHANGING p_lv_ktgrd .

  TYPES : BEGIN OF ty_ktgrd ,
            ktgrd TYPE tvkt-ktgrd,
          END OF ty_ktgrd .

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_ktgrd  TYPE TABLE OF ty_ktgrd.

  DATA: ls_ktgrd    TYPE ty_ktgrd.
  DATA: lw_rettab TYPE ddshretval.

  SELECT ktgrd
      FROM tvkt
         INTO TABLE lt_ktgrd.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'KTGRD'
      value_org       = 'S'
    TABLES
      value_tab       = lt_ktgrd
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  p_lv_ktgrd  = lw_rettab-fieldval.

ENDFORM.
*&      Form  F4_TATYP
*&---------------------------------------------------------------------*
FORM f4_tatyp   USING    p_lwa_area_venda_alv_tatyp
               CHANGING p_lv_tatyp .

*  TYPES : BEGIN OF ty_tatyp ,
*            tatyp TYPE t685a-kschl,
*          END OF ty_tatyp .
*
*  DATA: lt_rettab TYPE TABLE OF ddshretval,
*        lt_tatyp  TYPE TABLE OF ty_tatyp.
*
*  DATA: ls_tatyp    TYPE ty_tatyp.
*  DATA: lw_rettab TYPE ddshretval.
*
*  SELECT kschl
*      FROM t685a
*         INTO TABLE lt_tatyp
*    WHERE kappl = 'V'.
*
*  CHECK sy-subrc = 0.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'KSCHL'
*      value_org       = 'S'
*    TABLES
*      value_tab       = lt_tatyp
*      return_tab      = lt_rettab
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*  CHECK sy-subrc = 0.
*
*
*
*  READ TABLE lt_rettab INTO lw_rettab INDEX 1.
*
*  CHECK sy-subrc = 0.

  "p_lv_tatyp  = lw_rettab-fieldval.

ENDFORM.
*&      FORM  F4_TAXKD
*&---------------------------------------------------------------------*
FORM f4_taxkd   USING    p_lwa_area_venda_alv_taxkd
               CHANGING p_lv_taxkd p_lv_tatyp  .

  DATA:
    BEGIN OF tab OCCURS 0,
      tatyp TYPE tskd-tatyp,
      taxkd TYPE tskd-taxkd,
    END OF tab,
    wa              LIKE LINE OF tab,
    dynpfld_mapping TYPE STANDARD TABLE OF dselc,
    dyn_wa          TYPE dselc,
    lt_return       TYPE TABLE OF ddshretval,
    lwa_return      TYPE ddshretval.

  SELECT tatyp
         taxkd
          FROM tskd
         INTO TABLE tab.


  dyn_wa-fldname = 'FIELD1'.
  dyn_wa-dyfldname = 'A'.
  APPEND dyn_wa TO dynpfld_mapping.

  dyn_wa-fldname = 'FIELD2'.
  dyn_wa-dyfldname = 'B'.
  APPEND dyn_wa TO dynpfld_mapping.

  dyn_wa-fldname = 'FIELD3'.
  dyn_wa-dyfldname = 'C'.
  APPEND dyn_wa TO dynpfld_mapping.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield         = 'FIELD1'
      dynpprog         = sy-cprog
      dynpnr           = '1000'
      dynprofield      = 'A'
      value_org        = 'S'
      callback_program = sy-cprog
      callback_form    = 'CALLBACK_F4'
    TABLES
      value_tab        = tab
      return_tab       = lt_return.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  READ TABLE lt_return INTO lwa_return WITH KEY retfield  ='A'.

  p_lv_tatyp = lwa_return-fieldval.
  CLEAR: lwa_return.

  READ TABLE lt_return INTO lwa_return WITH KEY retfield  = 'B'.
  p_lv_taxkd = lwa_return-fieldval.

ENDFORM.

FORM callback_f4 TABLES record_tab STRUCTURE seahlpres
            CHANGING shlp TYPE shlp_descr
                     callcontrol LIKE ddshf4ctrl.
  DATA:
    ls_intf LIKE LINE OF shlp-interface,
    ls_prop LIKE LINE OF shlp-fieldprop.
*Hide unwanted fields
  CLEAR: ls_prop-shlpselpos,
         ls_prop-shlplispos.
*  MODIFY shlp-fieldprop FROM ls_prop
*    TRANSPORTING shlpselpos shlplispos
*  WHERE ( fieldname NE 'F0001'  AND
*          fieldname NE 'F0002'  AND
*          fieldname NE 'F0003' ).
*  " Overwrite selectable fields on search help
  REFRESH: shlp-interface.
  ls_intf-shlpfield = 'F0001'.
  ls_intf-valfield  = 'A'.
  ls_intf-f4field   = 'X'.
  APPEND ls_intf TO shlp-interface.
  ls_intf-shlpfield = 'F0002'.
  ls_intf-valfield  = 'B'.
  ls_intf-f4field   = 'X'.
  APPEND ls_intf TO shlp-interface.
  ls_intf-shlpfield = 'F0003'.
  ls_intf-valfield  = 'C'.
  ls_intf-f4field   = 'X'.
  APPEND ls_intf TO shlp-interface.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  D1215_MODIFIZIEREN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d1215_modifizieren OUTPUT.
*  LOOP AT SCREEN.
*    IF screen-name EQ 'GVA_XASEL'
*    OR screen-name EQ 'GVA_XESEL'.
*      screen-input = '0'.
*    ENDIF.
*    MODIFY SCREEN.
*  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  T042Z_LESEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE t042z_lesen OUTPUT.
  DATA: lt_t042zt TYPE TABLE OF t042zt,
        ls_t042zt TYPE t042zt,
        ls_t042e  TYPE t042e,
        lt_t042e  TYPE TABLE OF t042e,
        ld_zbukr  TYPE bukrs,
        lt_t042z  TYPE TABLE OF t042z,
        ls_t042z  TYPE t042z.

  DESCRIBE TABLE a042z LINES refe1.
  DESCRIBE TABLE e042z LINES refe2.

  SELECT SINGLE zbukr FROM t042 INTO ld_zbukr
          WHERE bukrs = vg_bukrs.

  SELECT SINGLE *
          FROM t001 INTO @DATA(wl_t001)
         WHERE bukrs = @vg_bukrs.


  IF sy-subrc NE 0 OR ld_zbukr = space.
    ld_zbukr = vg_bukrs.
  ENDIF.

  IF  refe1 = 0
  AND refe2 = 0.

    REFRESH: a042z, e042z, sorttab.

    SELECT * FROM t042zt INTO TABLE lt_t042zt
                        WHERE land1 = wl_t001-land1
                        AND   spras = sy-langu.

    SELECT * FROM t042e INTO TABLE lt_t042e WHERE zbukr = ld_zbukr.

    IF lines( lt_t042e ) NE 0.
      SELECT * FROM t042z INTO TABLE lt_t042z
                        FOR ALL ENTRIES IN lt_t042e
                        WHERE zlsch EQ lt_t042e-zlsch
                        AND land1 = wl_t001-land1.
    ELSE.
      SELECT * FROM t042z INTO TABLE lt_t042z
                       WHERE land1 = wl_t001-land1.
    ENDIF.

    LOOP AT lt_t042z INTO ls_t042z.
      LOOP AT lt_t042zt INTO ls_t042zt
              WHERE zlsch = ls_t042z-zlsch.
        IF ls_t042zt-text2 IS NOT INITIAL.
          ls_t042z-text1 = ls_t042zt-text2.
        ENDIF.
      ENDLOOP.

      IF g_ebpp_active = 'X' AND ls_t042z-zlsch CA g_ebpp_zahlwege.
        CONTINUE.
      ENDIF.

      IF ls_t042z-xeinz = 'X'.
        CLEAR e042z.
        e042z-zlsch = ls_t042z-zlsch.
        e042z-text1 = ls_t042z-text1.
        COLLECT e042z.
      ELSE.
        CLEAR a042z.
        a042z-zlsch = ls_t042z-zlsch.
        a042z-text1 = ls_t042z-text1.
        COLLECT a042z.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT a042z.
    CLEAR a042z-xselk.
    IF wa_grupo_contas_alv-zwels CS a042z-zlsch.
      a042z-xselk = 'X'.
      sorttab-arg = a042z-zlsch.
      APPEND sorttab.
    ELSE.
      a042z-xselk = space.
    ENDIF.
    MODIFY a042z.
  ENDLOOP.

  LOOP AT e042z.
    CLEAR e042z-xselk.
    IF wa_grupo_contas_alv-zwels CS e042z-zlsch.
      e042z-xselk = 'X'.
      sorttab-arg = e042z-zlsch.
      APPEND sorttab.
    ELSE.
      e042z-xselk = space.
    ENDIF.
    MODIFY e042z.
  ENDLOOP.

*------ Ausgangszustand bei den Zahlwegen merken -----------------------
*  IF  t020-aktyp <> 'A'
*  AND xmerken  = 'X'.
*    save_zwels = knb1-zwels.
*    CLEAR xmerken.
*  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TCTRL_ZAHLWEGE_INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tctrl_zahlwege_init OUTPUT.

  DESCRIBE TABLE a042z LINES refe1.
  DESCRIBE TABLE e042z LINES refe2.

  IF refe1 GE refe2.
    tfill = refe1.
  ELSE.
    tfill = refe2.
  ENDIF.

  tctrl_zahlwege-lines = tfill.

  IF index LE 0.
    index = 1.
  ENDIF.

  tctrl_zahlwege-top_line = index.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZAHLWEG_ANZEIGEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zahlweg_anzeigen OUTPUT.
  loopc = sy-loopc.
  lindex = index + sy-stepl - 1.

  READ TABLE a042z INDEX lindex.

  IF sy-subrc NE 0.
    LOOP AT SCREEN.
      IF screen-name = 'GVA_XASEL'
      OR screen-name = 'GVA_AZSCH'
      OR screen-name = 'GVA_AZTXT'.
        screen-input     = 0.
        screen-output    = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    gva_xasel = a042z-xselk.
    gva_azsch = a042z-zlsch.
    gva_aztxt = a042z-text1.
  ENDIF.

  READ TABLE e042z INDEX lindex.
  IF sy-subrc NE 0.
    LOOP AT SCREEN.
      IF screen-name = 'GVA_XESEL'
      OR screen-name = 'GVA_EZSCH'
      OR screen-name = 'GVA_EZTXT'.
        screen-input     = 0.
        screen-output    = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    gva_xesel = e042z-xselk.
    gva_ezsch = e042z-zlsch.
    gva_eztxt = e042z-text1.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ZAHLWEG_MARKIEREN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zahlweg_markieren INPUT.
  "CHECK t020-aktyp NE 'A'.
  "CHECK ok-code = 'MARK'.

*  GET CURSOR LINE crs_line FIELD crs_field.
*
*  IF crs_line = sy-stepl.
*    IF  crs_field(11) NE 'GVA_XASEL'
*    AND crs_field(11) NE 'GVA_XESEL'.
*      CLEAR: ok-code, crs_line, crs_field.
*      MESSAGE e017.
*    ENDIF.
*    ASSIGN (crs_field) TO <f1>.
*    IF <f1> NE 'X'.
*      <f1> = 'X'.
*    ELSE.
*      <f1> = space.
*    ENDIF.
*  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZAHLWEG_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zahlweg_update INPUT.

  lindex = index + sy-stepl - 1.

  IF gva_azsch NE space.
    a042z-xselk = gva_xasel.
    a042z-zlsch = gva_azsch.
    a042z-text1 = gva_aztxt.
    MODIFY a042z INDEX lindex.
    IF a042z-xselk = 'X'.
      sorttab-arg = a042z-zlsch.
      APPEND sorttab.
    ELSE.
      DELETE sorttab WHERE arg = a042z-zlsch.
    ENDIF.
  ENDIF.

  IF gva_ezsch NE space.
    e042z-xselk = gva_xesel.
    e042z-zlsch = gva_ezsch.
    e042z-text1 = gva_eztxt.
    MODIFY e042z INDEX lindex.
    IF e042z-xselk = 'X'.
      sorttab-arg = e042z-zlsch.
      APPEND sorttab.
    ELSE.
      DELETE sorttab WHERE arg = e042z-zlsch.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZAHLWEG_LEISTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zahlweg_leiste INPUT.

  CLEAR wa_grupo_contas_alv-zwels.
  zwcnt = 0.
  SORT sorttab DESCENDING.
  DELETE ADJACENT DUPLICATES FROM sorttab.
  LOOP AT sorttab.
    SHIFT wa_grupo_contas_alv-zwels RIGHT.
    wa_grupo_contas_alv-zwels(1) = sorttab-arg.
    zwcnt         = zwcnt + 1.
  ENDLOOP.

  IF zwcnt > 10.
    "MESSAGE s073.
    SET SCREEN sy-dynnr.
    LEAVE SCREEN.
  ELSE.
    zwcnt = 0.
  ENDIF.

*  IF ok-code = 'MARK'.
*    CLEAR: ok-code.
*    SET SCREEN sy-dynnr.
*    LEAVE SCREEN.
*  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TCTRL_ZAHLWEGE_BLAETTERN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tctrl_zahlwege_blaettern INPUT.
  IF index NE tctrl_zahlwege-top_line AND sy-ucomm IS INITIAL.
    index = tctrl_zahlwege-top_line.
    SET SCREEN sy-dynnr.
    LEAVE SCREEN.
  ENDIF.

  index = tctrl_zahlwege-top_line.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PFSTATUS_D1215  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pfstatus_d1215 OUTPUT.
  SET PF-STATUS 'Z0104'.
  "SET TITLEBAR '0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0104 INPUT.

  CASE sy-ucomm.
    WHEN 'EF12'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'ENTR'.
      SET SCREEN 0.
      LEAVE SCREEN.

*    WHEN 'F15'.
*      "PERFORM okcode_ende_debitor.
*    WHEN 'F17'.
*      "PERFORM okcode_anderes_konto.
  ENDCASE.

ENDMODULE.
