*----------------------------------------------------------------------*
***INCLUDE ZSDR0133_STATUS_0100O01.
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'PF0133'.
  SET TITLEBAR 'T0133'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
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
    gs_variant-report  = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

    SET HANDLER: obj_toolbar_0100->on_toolbar          FOR obj_alv_0100,
                 obj_toolbar_0100->handle_user_command FOR obj_alv_0100,
                 lcl_event_handler_0100=>catch_hotspot FOR obj_alv_0100.
    SET HANDLER: lcl_event_handler=>on_data_changed4   FOR obj_alv_0100.

    PERFORM f_exclude_fcode USING '0100'.

    CALL METHOD obj_alv_0100->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = t_saida_0100.

    CALL METHOD obj_alv_0100->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0100->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD obj_alv_0100->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN '&REFRESH'.
      PERFORM: f_selecionar_dados,
               f_processa_dados,
               f_refresh_alv USING '0100'.

    WHEN '&BAIXA'.
      CLEAR: l_erro.
      PERFORM f_efetua_baixa CHANGING l_erro.

      IF l_erro = abap_false.
        PERFORM:  f_selecionar_dados,
                  f_processa_dados,
                  f_refresh_alv USING '0100'.
      ENDIF.
  ENDCASE.

ENDMODULE.
