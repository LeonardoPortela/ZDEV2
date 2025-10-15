*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_PBO
*&---------------------------------------------------------------------*



MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'T0100'.
ENDMODULE.

MODULE pbo_0100 OUTPUT.

  IF obj_alv_0100 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0100'.

    PERFORM f_color_sequencia.

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

    gs_layout-sel_mode    = 'A'.
    gs_layout-ctab_fname  = 'COLOR'.
    gs_variant-report     = sy-repid.
    gs_variant-variant    = p_layout. "US #179396 - MMSILVA - 20.05.2025
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

    SET HANDLER: obj_toolbar_0100->on_toolbar          FOR obj_alv_0100,
                 obj_toolbar_0100->handle_user_command FOR obj_alv_0100,
                 lcl_event_handler_0100=>catch_hotspot FOR obj_alv_0100.

    PERFORM f_exclude_fcode USING '0100'.

    IF p_report EQ 'ZSDR0124'.

      CALL METHOD obj_alv_0100->set_table_for_first_display
        EXPORTING
          is_layout            = gs_layout
          i_save               = 'A'
          it_toolbar_excluding = it_exclude_fcode
          is_variant           = gs_variant
        CHANGING
          it_fieldcatalog      = it_fcat
          it_outtab            = it_saida_0200.
    ELSE.
      CALL METHOD obj_alv_0100->set_table_for_first_display
        EXPORTING
          is_layout            = gs_layout
          i_save               = 'A'
          it_toolbar_excluding = it_exclude_fcode
          is_variant           = gs_variant
        CHANGING
          it_fieldcatalog      = it_fcat
          it_outtab            = it_saida_0100.

    ENDIF.

    CALL METHOD obj_alv_0100->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0100->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*    PERFORM f_ajusta_totais.

  ELSE.

    CALL METHOD obj_alv_0100->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

ENDMODULE.
