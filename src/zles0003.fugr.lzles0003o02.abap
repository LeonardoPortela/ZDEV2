*----------------------------------------------------------------------*
***INCLUDE LZLES0003O02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA:   wl_repid     TYPE sy-repid,
          tl_function  TYPE ui_functions,
          wl_function  LIKE tl_function WITH HEADER LINE.
*          is_variant                   TYPE disvariant.
  IF container1 IS INITIAL.
    wa_layout-zebra      = c_x.
    wa_layout-sel_mode   = 'A'.
    wa_layout-box_fname  = 'MARK'.
*    wa_layout-edit_mode = 'X'.
*    WA_LAYOUT-BOX_TABNAME  = 'TG_SAIDA_ARQ'.
    wa_stable-row        = c_x.

    CREATE OBJECT container1
      EXPORTING
        container_name = 'CC_01'.
*BREAK-POINT.
    CREATE OBJECT grid1
      EXPORTING
        i_parent = container1.

*    CALL METHOD grid1->register_delayed_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_delayed_change_select
*      EXCEPTIONS
*        OTHERS     = 1.

*    CREATE OBJECT OBG_TOOLBAR
*      EXPORTING
*        IO_ALV_GRID = GRID1.
*
**      * Register event handler
*    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID1.
*    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID1.

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
    is_variant-report = sy-repid.
    is_variant-handle = '1002'.

    PERFORM montar_layout USING 'ARQUIVOS'.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*        i_save               = 'A'
*        is_variant           = is_variant
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_saida_arq[].



*    CALL METHOD GRID1->REGISTER_EDIT_EVENT
*      EXPORTING
*        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
*    CALL METHOD GRID1->REGISTER_EDIT_EVENT
*      EXPORTING
*        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
*
*    SET HANDLER lcl_event_handler=>on_delayed_changed_sel FOR ALL
*    INSTANCES.
*    SET HANDLER:
*              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
*              LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR GRID1.

  ELSE.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0020  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0020 OUTPUT.
  SET PF-STATUS 'PF001'.
  SET TITLEBAR '001'.

ENDMODULE.                 " STATUS_0020  OUTPUT
