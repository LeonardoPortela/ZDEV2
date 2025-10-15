*&---------------------------------------------------------------------*
*&  Include           ZSDR0112_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'T0100'.
ENDMODULE.

MODULE status_0101 OUTPUT.
  SET PF-STATUS 'PF0101'.
  SET TITLEBAR 'T0101'.

ENDMODULE.

MODULE pbo_0100 OUTPUT.

  IF obj_alv_0100 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0100'.

    IF obj_container_0100 IS NOT INITIAL.
      CREATE OBJECT obj_container_0100
        EXPORTING
          container_name = 'CC_ALV_0100'.
    ENDIF.

    CREATE OBJECT obj_alv_0100
      EXPORTING
        i_parent = obj_container_0100.

    CREATE OBJECT obj_toolbar_0100
      EXPORTING
        io_alv_grid = obj_alv_0100.

    gs_layout-sel_mode   = 'A'.
    gs_layout-ctab_fname = 'COLOR'.
    gs_layout-col_opt = 'X'.
    gs_variant-report  = sy-repid.
*------------------------------
    wa_stable-row      = 'X'.
    wa_stable-col      = 'X'.
*------------------------------

    SET HANDLER: obj_toolbar_0100->on_toolbar          FOR obj_alv_0100,
                 obj_toolbar_0100->handle_user_command FOR obj_alv_0100,
                 lcl_event_handler_0100=>catch_hotspot FOR obj_alv_0100.

    PERFORM f_exclude_fcode USING '0100'.

    CALL METHOD cl_gui_cfw=>flush.

    CALL METHOD obj_alv_0100->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0100->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    CALL METHOD obj_alv_0100->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.


    CALL METHOD obj_alv_0100->set_table_for_first_display
      EXPORTING
        i_default            = abap_true
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0100.

  ELSE.

    CALL METHOD obj_alv_0100->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    IF lines( it_row ) > 0.
      CALL METHOD obj_alv_0100->set_selected_rows
        EXPORTING
          it_index_rows = it_row.
    ENDIF.

  ENDIF.


ENDMODULE.
