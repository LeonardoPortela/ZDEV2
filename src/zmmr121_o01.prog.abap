*----------------------------------------------------------------------*
***INCLUDE ZMMR121_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  IF wg_acao IS INITIAL.
    REFRESH: tg_fields.
    PERFORM f_trata_campos USING  space
                                 'GR1'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
  ENDIF.

  SET PF-STATUS '0100'.
  CASE sy-tcode.
    WHEN 'ZMM0120'.
      SET TITLEBAR '0100'.
    WHEN 'ZMM0177'.
      SET TITLEBAR '0102'.
    WHEN OTHERS.
      SET TITLEBAR '0101'.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: event       TYPE cntl_simple_event,
        events      TYPE cntl_simple_events,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function  WITH HEADER LINE,

        lt_f4       TYPE lvc_t_f4     WITH HEADER LINE.

  DATA: waref TYPE REF TO data.

  wa_stable-row = 'X'.
  wa_stable-col = 'X'.

  IF g_custom_container IS INITIAL.
    wa_layout-cwidth_opt = c_x.
    wa_layout-zebra       = c_x.
*    WA_LAYOUT-NO_TOOLBAR  = C_X.
    wa_layout-no_rowmark  = space.
    wa_layout-col_opt     = c_x.
    wa_stable-row         = c_x.
    wa_layout-sel_mode    = 'A'.
    wa_layout-box_fname   = 'MARK'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_1.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container_1.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

*      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid1.
    SET HANDLER obg_toolbar->handle_user_command FOR grid1.

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

    PERFORM f_montar_layout USING space.


    CLEAR wa_layout-cwidth_opt.
    gs_variant_c-report = sy-repid. "Enable users save own LAYOUTs

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant_c
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_saida[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


*    CALL METHOD GRID1->REGISTER_F4_FOR_FIELDS
*      EXPORTING
*        IT_F4 = LT_F4[].

    SET HANDLER:
      lcl_event_handler=>on_double_click          FOR grid1,
      lcl_event_handler=>on_data_changed_finished FOR grid1,
      lcl_event_handler=>on_data_changed          FOR grid1,
      lcl_event_handler=>on_onf4                  FOR grid1.

*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.
  ELSE.
    IF wg_acao = c_modif OR wg_acao = c_add.
      PERFORM f_montar_layout USING c_x.
    ELSE.
      PERFORM f_montar_layout USING space.
    ENDIF.

    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE trata_fields OUTPUT.
  LOOP AT tg_fields.
    LOOP AT SCREEN.
      IF screen-name EQ tg_fields-campo
      OR screen-group1 EQ tg_fields-group1.
        screen-input     = tg_fields-value.
        screen-invisible = tg_fields-invisible.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF x_field IS NOT INITIAL.
    SET CURSOR FIELD x_field. "'WG_DESC_OPERACAO'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.
  APPEND 'AGRUPA' TO fcode.
  SET PF-STATUS '0100' EXCLUDING fcode.
    CASE sy-tcode.
    WHEN 'ZMM0120'.
      SET TITLEBAR '0100'.
    WHEN 'ZMM0177'.
      SET TITLEBAR '0102'.
    WHEN OTHERS.
      SET TITLEBAR '0101'.
  ENDCASE.
  "SET TITLEBAR '0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS2  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos2 OUTPUT.
*  DATA: EVENT       TYPE CNTL_SIMPLE_EVENT,
*        EVENTS      TYPE CNTL_SIMPLE_EVENTS,
*        TL_FILTER   TYPE LVC_T_FILT,
*        WL_FILTER   TYPE LVC_S_FILT,
*        TL_FUNCTION TYPE UI_FUNCTIONS,
*        WL_FUNCTION LIKE TL_FUNCTION  WITH HEADER LINE,
*
*        LT_F4       TYPE LVC_T_F4     WITH HEADER LINE.

*  DATA: WAREF TYPE REF TO DATA.

  wa_stable-row = 'X'.
  wa_stable-col = 'X'.

  IF g_custom_container2 IS INITIAL.
    wa_layout-cwidth_opt = c_x.
    wa_layout-zebra       = c_x.
*    WA_LAYOUT-NO_TOOLBAR  = C_X.
    wa_layout-no_rowmark  = space.
    wa_layout-col_opt     = c_x.
    wa_stable-row         = c_x.
    wa_layout-sel_mode    = 'A'.
    wa_layout-box_fname   = 'MARK'.

    CREATE OBJECT g_custom_container2
      EXPORTING
        container_name = g_container2.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_container2
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

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid2.

*      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid2.
    SET HANDLER obg_toolbar->handle_user_command FOR grid2.

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

    PERFORM f_montar_layout2 USING space.


    CLEAR wa_layout-cwidth_opt.
    gs_variant_c-report = sy-repid. "Enable users save own LAYOUTs

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant_c
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_saida2[].

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    SET HANDLER: lcl_event_handler=>on_data_changed_finished FOR grid2,
                 lcl_event_handler=>on_data_changed          FOR grid2.


*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.
  ELSE.
    IF wg_acao = c_modif OR wg_acao = c_add.
      PERFORM f_montar_layout2 USING c_x.
    ELSE.
      PERFORM f_montar_layout2 USING space.
    ENDIF.

    CALL METHOD grid2->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
