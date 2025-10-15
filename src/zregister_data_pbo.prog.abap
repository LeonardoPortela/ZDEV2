*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_PBO
*&---------------------------------------------------------------------*

MODULE pbo OUTPUT.

  DATA: v_title TYPE string.

  SET PF-STATUS 'PF_MAIN'.

  SET TITLEBAR 'T0001' WITH p_title.

  IF obj_container IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING ''.

    CREATE OBJECT obj_container
      EXPORTING
        container_name = 'CC_ALV'.

    CREATE OBJECT obj_alv
      EXPORTING
        i_parent = obj_container.

    CREATE OBJECT obj_toolbar
      EXPORTING
        io_alv_grid = obj_alv.

    gs_layout-sel_mode   = 'A'.
*-CS2025000249-17.04.2025-#173311-JT-inicio
*   gs_layout-cwidth_opt = abap_true. "US "163322 - MMSILVA - 14.02.2025 - Ajustar Tamanho Coluna
    gs_layout-cwidth_opt = COND #( WHEN p_nocwid = abap_true THEN abap_false
                                                             ELSE abap_true ).
*-CS2025000249-17.04.2025-#173311-JT-fim
    gs_variant-report  = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

    SET HANDLER: obj_toolbar->on_toolbar          FOR obj_alv,
                 obj_toolbar->handle_user_command FOR obj_alv,
                 lcl_event_handler=>catch_hotspot FOR obj_alv.

    PERFORM f_exclude_fcode USING ''.

    CALL METHOD obj_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = <fs_it_saida>.

    CALL METHOD obj_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD obj_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    CALL METHOD obj_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ELSE.
    CALL METHOD obj_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDMODULE.

MODULE pbo_manter OUTPUT.

  SET PF-STATUS 'PF_MANTER'.

  IF vg_operacao = c_change.

    LOOP AT SCREEN.

      READ TABLE tg_field_screen_key WITH KEY field_screen = screen-name.
      IF sy-subrc EQ 0.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ENDIF.

  PERFORM f_exit_0005 CHANGING <fs_wa_registro_manter>.

  MOVE-CORRESPONDING <fs_wa_registro_manter> TO <fs_wa_saida>.

  PERFORM f_exit_0004 CHANGING <fs_wa_saida>.

ENDMODULE.


MODULE pbo_manter_search OUTPUT.

  SET PF-STATUS 'PF_SEARCH'.

  DATA(lva_title) = |Parâmetros Pequisa - { p_title }|.

  SET TITLEBAR 'T0002' WITH lva_title.

ENDMODULE.
