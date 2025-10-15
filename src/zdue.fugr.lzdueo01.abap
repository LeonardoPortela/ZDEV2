*----------------------------------------------------------------------*
***INCLUDE LZDUEO01.
*----------------------------------------------------------------------*

MODULE status_0100 OUTPUT.

  DATA: tg_fcode TYPE TABLE OF sy-ucomm WITH HEADER LINE.

  CLEAR: tg_fcode[].

  IF due_control-modo = c_due_view.
    tg_fcode = c_save.
    APPEND tg_fcode.
  ENDIF.

  SET PF-STATUS 'PF0100' EXCLUDING tg_fcode.

  IF due_control-retificar IS NOT INITIAL.
    SET TITLEBAR 'T0100_R'.
  ELSE.
    SET TITLEBAR 'T0100'.
  ENDIF.

  IF due_dynnr_000 IS INITIAL.
    due_dynnr_000          = due_0110.
    info_due_tab-activetab = due_tb01.
  ENDIF.

ENDMODULE.

MODULE status_0110 OUTPUT.

  PERFORM f_completa_campos_0110.
  PERFORM f_atualiza_textos_0110.
  PERFORM f_define_listbox_0110.
  PERFORM f_control_fields_0110.

  SET CURSOR FIELD vg_field_sel_0110 OFFSET vg_field_pos_0110.

ENDMODULE.

MODULE status_0010 OUTPUT.
  SET PF-STATUS 'PF0010'.
  SET TITLEBAR 'T0010'.
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

    gs_layout-sel_mode   = 'A'.
    gs_variant-report  = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

    SET HANDLER: obj_toolbar_0120->on_toolbar          FOR obj_alv_0120,
                 obj_toolbar_0120->handle_user_command FOR obj_alv_0120,
                 lcl_event_handler_0120=>catch_hotspot FOR obj_alv_0120.

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
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0120->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD obj_alv_0120->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDMODULE.

MODULE status_0121 OUTPUT.

  SET PF-STATUS 'PF0121'.
*  SET TITLEBAR 'xxx'.

  PERFORM f_completa_campos_0121.
  PERFORM f_control_fields_0121.

  SET CURSOR FIELD vg_field_sel_0121 OFFSET vg_field_pos_0121.

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

MODULE status_0122 OUTPUT.
  SET PF-STATUS 'PF0122'.
  SET TITLEBAR 'T0122'.
ENDMODULE.

MODULE status_0123 OUTPUT.
  SET PF-STATUS 'PF0123'.
  SET TITLEBAR 'T0123'.
ENDMODULE.


MODULE pbo_0123 OUTPUT.

  IF obj_alv_0123 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0123'.

    IF obj_container_0123 IS INITIAL.
      CREATE OBJECT obj_container_0123
        EXPORTING
          container_name = 'CC_ALV_0123'.
    ENDIF.

    CREATE OBJECT obj_alv_0123
      EXPORTING
        i_parent = obj_container_0123.

    CREATE OBJECT obj_toolbar_0123
      EXPORTING
        io_alv_grid = obj_alv_0123.

    gs_layout-zebra       = 'X'.
    gs_layout-sel_mode    = 'A'.
    gs_layout-stylefname  = 'ESTILO'.
    gs_variant-report     = sy-repid.

    SET HANDLER: obj_toolbar_0123->on_toolbar          FOR obj_alv_0123,
                 obj_toolbar_0123->handle_user_command FOR obj_alv_0123.

    PERFORM f_exclude_fcode USING '0123'.

    CALL METHOD obj_alv_0123->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0123_itm.

    CALL METHOD obj_alv_0123->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler_0123=>on_data_changed_finished FOR obj_alv_0123,
                 lcl_event_handler_0123=>on_data_changed          FOR obj_alv_0123.

  ELSE.
    CALL METHOD obj_alv_0123->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  CALL METHOD obj_alv_0123->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

ENDMODULE.

MODULE status_0124 OUTPUT.
  SET PF-STATUS 'PF0124'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.

MODULE status_0125 OUTPUT.
  SET PF-STATUS 'PF0125'.
  SET TITLEBAR 'T0125'.
ENDMODULE.

MODULE pbo_0125 OUTPUT.

  IF obj_alv_0125 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0125'.

    IF obj_container_0125 IS INITIAL.
      CREATE OBJECT obj_container_0125
        EXPORTING
          container_name = 'CC_ALV_0125'.
    ENDIF.

    CREATE OBJECT obj_alv_0125
      EXPORTING
        i_parent = obj_container_0125.

    CREATE OBJECT obj_toolbar_0125
      EXPORTING
        io_alv_grid = obj_alv_0125.

    gs_layout-zebra       = 'X'.
    gs_layout-sel_mode    = 'A'.
    gs_layout-stylefname  = 'ESTILO'.
    gs_variant-report     = sy-repid.

    SET HANDLER: obj_toolbar_0125->on_toolbar          FOR obj_alv_0125,
                 obj_toolbar_0125->handle_user_command FOR obj_alv_0125.

    PERFORM f_exclude_fcode USING '0125'.

    CALL METHOD obj_alv_0125->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0125_itm.

    CALL METHOD obj_alv_0125->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler_0125=>on_data_changed_finished FOR obj_alv_0125,
                 lcl_event_handler_0125=>on_data_changed          FOR obj_alv_0125.

  ELSE.
    CALL METHOD obj_alv_0125->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  CALL METHOD obj_alv_0125->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.


ENDMODULE.

MODULE status_0111 OUTPUT.
  SET PF-STATUS 'PF0111'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.

MODULE pbo_1000 OUTPUT.

  SET PF-STATUS 'PF1000'.

  IF obj_alv_1000 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '1000'.

    IF obj_container_1000 IS INITIAL.
      CREATE OBJECT obj_container_1000
        EXPORTING
          container_name = 'CC_ALV_1000'.
    ENDIF.

    CREATE OBJECT obj_alv_1000
      EXPORTING
        i_parent = obj_container_1000.

    gs_layout-sel_mode   = 'A'.
    gs_variant-report  = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

    PERFORM f_exclude_fcode USING '1000'.

    CALL METHOD obj_alv_1000->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_1000.

    CALL METHOD obj_alv_1000->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_1000->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD obj_alv_1000->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.

MODULE pbo_0200 OUTPUT.

  SET PF-STATUS 'PF0200'.

  LOOP AT SCREEN.

    IF gwa_nomeacao_sol_ov-id_nomeacao_tran IS INITIAL.

      CASE screen-group1.
        WHEN 'A1'.
          screen-input     = 1.
        WHEN 'B1'.
          screen-invisible = 1.
          screen-active    = 0.
      ENDCASE.

    ELSE.

      CASE screen-group1.
        WHEN 'A1'.
          screen-input = 0.
        WHEN 'B1'.
          screen-input = 1.
      ENDCASE.

    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.


ENDMODULE.


MODULE pbo_0201 OUTPUT.

  PERFORM f_fill_screen_fields_0201.

  SET TITLEBAR 'T0201'.
  SET PF-STATUS 'PF0201'.

  LOOP AT SCREEN.

    CASE screen-name.
      WHEN 'BTN_GERAR_RUC'.
        IF rb_gerar_ruc_0201 EQ abap_true.
          screen-active    = 1.
          screen-input     = 1.
        ELSE.
          screen-active    = 0.
          screen-input     = 0.
        ENDIF.

        MODIFY SCREEN.
    ENDCASE.

    CASE screen-group1.
      WHEN 'A1'.

        IF gwa_dados_geracao_ruc-edit_fields EQ abap_true.
          screen-input     = 1.
        ELSE.
          screen-input     = 0.
        ENDIF.

        MODIFY SCREEN.
    ENDCASE.

  ENDLOOP.


  IF obj_alv_0201 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0201'.

    IF obj_container_0201 IS INITIAL.
      CREATE OBJECT obj_container_0201
        EXPORTING
          container_name = 'CC_ALV_0201'.
    ENDIF.

    CREATE OBJECT obj_alv_0201
      EXPORTING
        i_parent = obj_container_0201.

    "CREATE OBJECT OBJ_TOOLBAR_0201
    "  EXPORTING
    "    IO_ALV_GRID = OBJ_ALV_0201.

    gs_layout-sel_mode   = 'A'.
    gs_variant-report  = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

    "SET HANDLER: OBJ_TOOLBAR_0201->ON_TOOLBAR          FOR OBJ_ALV_0201,
    "             OBJ_TOOLBAR_0201->HANDLE_USER_COMMAND FOR OBJ_ALV_0201,
    "             LCL_EVENT_HANDLER_0201=>CATCH_HOTSPOT FOR OBJ_ALV_0201.

    PERFORM f_exclude_fcode USING '0201'.

    CALL METHOD obj_alv_0201->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0201.

    CALL METHOD obj_alv_0201->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0201->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD obj_alv_0201->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0126  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0126 OUTPUT.
  SET PF-STATUS '0126'.
  SET TITLEBAR '0126'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CARREGA_DADOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE carrega_dados OUTPUT.

  DATA: vg_ucomm_drawn TYPE sy-ucomm.
  IMPORT vg_ucomm_drawn  TO vg_ucomm_drawn
  FROM MEMORY ID 'M_DRAWN'.

  LOOP AT SCREEN.
    IF vg_ucomm_drawn = 'VIEW'.
      IF screen-name = 'ZSDT0305-COD_ENQUADRAMENTO' OR screen-name = 'ZSDT0305-NR_DRAWBACK'
        OR screen-name = 'ZSDT0305-TP_ATO'.

        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT it_sel_rows INTO DATA(ls_sel_rows).
    READ TABLE it_saida_0120 INTO DATA(ls_saida_0120) WITH KEY id_due_item = ls_sel_rows-index+6(4).
    IF sy-subrc IS INITIAL.

      SELECT *
        FROM zsdt0305
        INTO TABLE t_zsdt0305
        WHERE "id_due      = ls_saida_0120-id_due    AND
              id_due_item = ls_saida_0120-id_due_item AND
               FATURA_ID = ls_saida_0120-fatura_id.
      IF sy-subrc IS INITIAL.
*---> 05/07/2023 - Migração S4 - DL
        SORT t_zsdt0305 BY id_due id_due_item fatura_id.
*<--- 05/07/2023 - Migração S4 - DL

        READ TABLE t_zsdt0305 INTO ls_zsdt0305 WITH KEY id_due      = ls_saida_0120-id_due
                                                              id_due_item = ls_saida_0120-id_due_item
                                                              FATURA_ID   = ls_saida_0120-fatura_id
                                                              BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          zsdt0305-cod_enquadramento = ls_zsdt0305-cod_enquadramento.
          zsdt0305-nr_drawback       = ls_zsdt0305-nr_drawback .
          zsdt0305-tp_ato            = ls_zsdt0305-tp_ato .
        ENDIF.
      ELSE.

        CLEAR: zsdt0305-cod_enquadramento,
               zsdt0305-nr_drawback      ,
               zsdt0305-tp_ato           .
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMODULE.
