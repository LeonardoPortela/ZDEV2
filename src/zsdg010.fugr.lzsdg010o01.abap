*----------------------------------------------------------------------*
***INCLUDE LZSDG010O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: v_title TYPE string.

  SET PF-STATUS 'PF_MAIN'.

  SET TITLEBAR 'T0001'.

  IF obj_container IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING ''.

    CREATE OBJECT obj_container
      EXPORTING
        container_name = 'CC_ALV_FNC'.

    CREATE OBJECT obj_alv
      EXPORTING
        i_parent = obj_container.

    CREATE OBJECT obj_toolbar
      EXPORTING
        io_alv_grid = obj_alv.

    gs_layout-sel_mode   = 'A'.
    gs_variant-report  = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

*   SET HANDLER: obj_toolbar->on_toolbar          FOR obj_alv,
*                obj_toolbar->handle_user_command FOR obj_alv,
*                lcl_event_handler=>catch_hotspot FOR obj_alv.

    PERFORM f_exclude_fcode USING ''.

    CALL METHOD obj_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = tg_dados.

*   CALL METHOD obj_alv->register_edit_event
*     EXPORTING
*       i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*   CALL METHOD obj_alv->register_edit_event
*     EXPORTING
*       i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD obj_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.
