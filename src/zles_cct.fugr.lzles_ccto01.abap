*----------------------------------------------------------------------*
***INCLUDE LZLES_CCTO01.
*----------------------------------------------------------------------*

MODULE status_0122 OUTPUT.
  SET PF-STATUS 'PF0122'.
  SET TITLEBAR 'T0122'.
ENDMODULE.

MODULE status_0100 OUTPUT.

  DATA: tg_fcode TYPE TABLE OF sy-ucomm WITH HEADER LINE.

  CLEAR: tg_fcode[].

  IF entrega_control-modo = c_entrega_view.
    tg_fcode = c_save.
    APPEND tg_fcode.
  ENDIF.

  SET PF-STATUS 'PF0100' EXCLUDING tg_fcode.

  SET TITLEBAR 'T0100'.


  IF entrega_dynnr_000 IS INITIAL.
    entrega_dynnr_000          = entrega_0110.
    info_entrega_tab-activetab = entrega_tb01.
  ENDIF.

ENDMODULE.

MODULE status_0110 OUTPUT.

  "PERFORM F_COMPLETA_CAMPOS_0110.
  "PERFORM F_ATUALIZA_TEXTOS_0110.
  "PERFORM F_DEFINE_LISTBOX_0110.
  PERFORM f_control_fields_0110.

ENDMODULE.


MODULE pbo_0120 OUTPUT.

  IF obj_alv_0120 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0120'.

    IF obj_container_0120 IS INITIAL.
      CREATE OBJECT obj_container_0120
        EXPORTING
          container_name = 'CC_ALV_0120'.
    ENDIF.

    CREATE OBJECT obj_alv_0120
      EXPORTING
        i_parent = obj_container_0120.

    CREATE OBJECT obj_toolbar_0120
      EXPORTING
        io_alv_grid = obj_alv_0120.

    gs_layout-zebra       = 'X'.
    gs_layout-sel_mode    = 'A'.
    gs_layout-stylefname  = 'ESTILO'.
    gs_variant-report     = sy-repid.

    SET HANDLER: obj_toolbar_0120->on_toolbar          FOR obj_alv_0120,
                 obj_toolbar_0120->handle_user_command FOR obj_alv_0120.

    PERFORM f_exclude_fcode USING '0120'.

    CALL METHOD obj_alv_0120->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0120.

    CALL METHOD obj_alv_0120->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

**** CS2019001041 - Ajustes na ZLES0147 - Inicio - CBRAND
    CALL METHOD obj_alv_0120->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
**** CS2019001041 - Ajustes na ZLES0147 - Fim - CBRAND

    SET HANDLER: lcl_event_handler_0120=>on_data_changed_finished FOR obj_alv_0120,
                 lcl_event_handler_0120=>on_data_changed          FOR obj_alv_0120.


  ELSE.
    CALL METHOD obj_alv_0120->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  CALL METHOD obj_alv_0120->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

ENDMODULE.


MODULE pbo_0122 OUTPUT.

  IF obj_alv_0122 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0122'.

    IF obj_container_0122 IS INITIAL.
      CREATE OBJECT obj_container_0122
        EXPORTING
          container_name = 'CC_ALV_0122'.
    ENDIF.

    CREATE OBJECT obj_alv_0122
      EXPORTING
        i_parent = obj_container_0122.

    CREATE OBJECT obj_toolbar_0122
      EXPORTING
        io_alv_grid = obj_alv_0122.

    gs_layout-zebra       = 'X'.
    gs_layout-sel_mode    = 'A'.
    gs_layout-stylefname  = 'ESTILO'.
    gs_variant-report     = sy-repid.

    SET HANDLER: obj_toolbar_0122->on_toolbar          FOR obj_alv_0122,
                 obj_toolbar_0122->handle_user_command FOR obj_alv_0122.

    PERFORM f_exclude_fcode USING '0122'.

    CALL METHOD obj_alv_0122->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0122_itm.

    CALL METHOD obj_alv_0122->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler_0122=>on_data_changed_finished FOR obj_alv_0122,
                 lcl_event_handler_0122=>on_data_changed          FOR obj_alv_0122.

  ELSE.
    CALL METHOD obj_alv_0122->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  CALL METHOD obj_alv_0122->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

ENDMODULE.
