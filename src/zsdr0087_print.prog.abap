*&---------------------------------------------------------------------*
*&  Include           ZSDR0087_PRINT
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*      <--P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM create_and_init_alv  CHANGING pt_fieldcat TYPE lvc_t_fcat.

  DATA: lt_f4    TYPE lvc_t_f4 WITH HEADER LINE,
        l_tab(8) TYPE c.

  IF g_container IS INITIAL.

    wa_stable   = c_x.
    g_container =  'CONTAINER'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = g_custom_container.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = g_grid.

    SET HANDLER obg_toolbar->on_toolbar FOR g_grid.
    SET HANDLER obg_toolbar->handle_user_command FOR g_grid.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    PERFORM seleciona_dados.

* CONSTRÓI FIELDCAT E DEFINE AS COLUNAS EDITÁVEIS.
    PERFORM build_fieldcat CHANGING pt_fieldcat.

    CALL METHOD cl_gui_cfw=>flush.

***************************************
    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
      CHANGING
        it_fieldcatalog      = pt_fieldcat
        it_outtab            = tg_saida.

    SET HANDLER:
      lcl_event_handler=>on_button_click FOR g_grid,
      lcl_event_handler=>on_data_changed FOR g_grid,
      lcl_event_handler=>on_data_changed_finished FOR g_grid,
      lcl_event_handler=>on_double_click FOR g_grid.

* * *
    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.


ENDFORM.              "CREATE_AND_INIT_ALV

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM build_fieldcat  CHANGING pt_fieldcat TYPE lvc_t_fcat.

  DATA ls_fcat TYPE lvc_s_fcat.
  CLEAR pt_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZPARAM_CONT_FRET'
    CHANGING
      ct_fieldcat      = pt_fieldcat.

  LOOP AT pt_fieldcat INTO ls_fcat.

    IF ls_fcat-fieldname EQ 'BUKRS'.
      ls_fcat-coltext = 'Empresa'.
      ls_fcat-outputlen = 10.
      MODIFY pt_fieldcat FROM ls_fcat.
    ENDIF.
    IF ls_fcat-fieldname EQ 'WERKS'.
      ls_fcat-outputlen = 12.
      MODIFY pt_fieldcat FROM ls_fcat.
    ENDIF.
    IF ls_fcat-fieldname EQ 'TP_PRODUCAO'.
      ls_fcat-coltext = 'Tp.Prod'.
      ls_fcat-outputlen = 10.
      MODIFY pt_fieldcat FROM ls_fcat.
    ENDIF.
    IF ls_fcat-fieldname EQ 'DCO'.
      ls_fcat-coltext = 'DCO'.
      ls_fcat-outputlen = 06.
      MODIFY pt_fieldcat FROM ls_fcat.
    ENDIF.
    IF ls_fcat-fieldname EQ 'AUART'.
      ls_fcat-coltext = 'Tp.Doc.Vendas'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ENDIF.
    IF ls_fcat-fieldname EQ 'AUART'.
      ls_fcat-coltext = 'Tp.Doc.Vendas'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ENDIF.

    " 05.07.2022 - RAMON - 76636 -->
    IF ls_fcat-fieldname EQ 'INDUSTRIALIZACAO'.
      ls_fcat-coltext = 'Industr.'.
      ls_fcat-outputlen = 06.
      MODIFY pt_fieldcat FROM ls_fcat.
    ENDIF.
    " 05.07.2022 - RAMON - 76636 --<

    IF ls_fcat-fieldname EQ 'DATA_ATUAL'.
      ls_fcat-coltext = 'Data Mod.'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ENDIF.
    IF ls_fcat-fieldname EQ 'HORA_ATUAL'.
      ls_fcat-coltext = 'Hora Mod.'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ENDIF.

  ENDLOOP.


ENDFORM.
