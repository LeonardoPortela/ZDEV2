*&---------------------------------------------------------------------*
*&  Include           ZMMR_0041_N_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  FILL_ALV  OUTPUT
*&---------------------------------------------------------------------*
MODULE fill_alv OUTPUT.

  DATA lo_handler TYPE REF TO lcl_event_handler.

  IF go_cccontainer IS INITIAL.

*   Create object for container
    CREATE OBJECT go_cccontainer
      EXPORTING
        container_name = 'ALV_CONTAINER'.

    CREATE OBJECT go_splitter_container
      EXPORTING
        parent  = go_cccontainer
        rows    = 2
        columns = 1.

    CALL METHOD go_splitter_container->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = go_part1.

*    CALL METHOD go_splitter_container->get_container ALRS
*      EXPORTING
*        row       = 2
*        column    = 1
*      RECEIVING
*        container = go_part2.

*   Create object for ALV grid1 inside container
    CREATE OBJECT go_alv_grid1
      EXPORTING
        i_parent = go_part1.


*   Create object for ALV grid1 inside container ALRS
*    CREATE OBJECT go_alv_grid2
*      EXPORTING
*        i_parent = go_part2.

*   Fill field catalog
    PERFORM f_fill_fieldcat1.

    PERFORM f_coluna_edita2 USING 'BELNR_FRET' 'Doc.Frete' 'Doc.Frete' CHANGING gt_fieldcatalog1.
    PERFORM f_coluna_edita2 USING 'GJAHR_FRET' 'Ano.Frete' 'Ano.Frete' CHANGING gt_fieldcatalog1.

    "PERFORM f_fill_variant1.

*   Set layout parameters for ALV grid
    gw_layout1-grid_title  = TEXT-100. "Document Overview / Síntese do documento
    gw_layout1-sel_mode    = 'A'.

    gw_layout1-col_opt = 'X'.
    gw_layout1-cwidth_opt = 'X'.

    gw_variant1-report = sy-repid. "Enable users save own LAYOUTs
*   Send data to ALV grid
    CALL METHOD go_alv_grid1->set_table_for_first_display
      EXPORTING
        is_layout       = gw_layout1
        is_variant      = gw_variant1
        i_save          = 'X'
        i_default       = 'X'
      CHANGING
        it_fieldcatalog = gt_fieldcatalog1
        it_outtab       = gt_dados_alv.

    CREATE OBJECT lo_handler.
*
    SET HANDLER lo_handler->handle_double_click FOR go_alv_grid1.
*    SET HANDLER event_receiver->handle_toolbar      FOR go_alv_grid1.
*    SET HANDLER event_receiver->handle_menu_button  FOR go_alv_grid1.
*
    CALL METHOD go_alv_grid1->set_toolbar_interactive.
    CALL METHOD go_splitter_container->set_row_height
      EXPORTING
        id     = 1
        height = 100.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  DATA lt_exclude TYPE TABLE OF sy-ucomm.

  AUTHORITY-CHECK OBJECT 'ZACTNFPS' ID 'ZACTNFPS' FIELD '04'.

  IF sy-subrc <> 0.
    APPEND 'CANCEL' TO lt_exclude.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZACTNFPS' ID 'ZACTNFPS' FIELD '05'.

  IF sy-subrc <> 0.
    APPEND 'STATUS_DOC' TO lt_exclude.
  ENDIF.

  SET PF-STATUS 'ALV_NFSE' EXCLUDING lt_exclude.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9100 OUTPUT.
  SET PF-STATUS '9100'.
  SET TITLEBAR  '9100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos_9100 OUTPUT.
  REFRESH : events,
            tl_filter,
            tl_function.

  CLEAR: wl_function,
         wl_filter  ,
         event    .


  wa_stable-row = 'X'.
  wa_stable-col = 'X'.

  IF g_custom_conta0200 IS INITIAL.
    wa_layout-cwidth_opt = c_x.
    wa_layout-zebra       = c_x.
    wa_layout-no_rowmark  = space.
    wa_layout-sel_mode    = 'B'.
    wa_layout-box_fname   = ''.

    CREATE OBJECT g_custom_conta0200
      EXPORTING
        container_name = g_container2.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_conta0200
        rows    = 2
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_2.

    CREATE OBJECT grid2
      EXPORTING
        i_parent = container_2.

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

    CLEAR wa_layout-cwidth_opt.
    gs_variant_2-report = sy-repid.

    PERFORM f_monta_layout2.

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant_2
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
        i_default            = 'X'
      CHANGING
        it_fieldcatalog      = lt_fcat_lvc2[]
        it_outtab            = it_docs_alv[].

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT lo_handler.
    SET HANDLER:
              lo_handler->catch_hotspot FOR grid2.

*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.

    CALL METHOD grid2->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = lt_fcat_lvc2[].

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ELSE.

    CALL METHOD grid2->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = lt_fcat_lvc2[].

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
