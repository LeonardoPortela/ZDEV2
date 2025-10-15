*&---------------------------------------------------------------------*
*&  Include           ZFIR058_PBO
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'T0100'.
ENDMODULE.

MODULE pbo_0100 OUTPUT.

  PERFORM f_refresh_objetos.
  IF obj_container_0100 IS INITIAL.

    PERFORM f_criar_catalog USING '0100'.

    CREATE OBJECT obj_container_0100
      EXPORTING
        container_name = 'CC_ALV_0100'.

    CREATE OBJECT obj_alv_0100
      EXPORTING
        i_parent = obj_container_0100.

    CREATE OBJECT obj_toolbar_0100
      EXPORTING
        io_alv_grid = obj_alv_0100.

    gs_layout-sel_mode   = 'A'.
    gs_variant-report  = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

    SET HANDLER: obj_toolbar_0100->on_toolbar          FOR obj_alv_0100,
                 obj_toolbar_0100->handle_user_command FOR obj_alv_0100.

    PERFORM f_exclude_fcode USING '0100'.

    CALL METHOD obj_alv_0100->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0100.

    SET HANDLER: lcl_event_handler_0100=>handle_hotspot_click FOR obj_alv_0100.

    CALL METHOD obj_alv_0100->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0100->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    PERFORM f_criar_catalog USING '0100'.
    CALL METHOD obj_alv_0100->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.

MODULE status_0110 OUTPUT.
  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = tl_parametros
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
  ENDIF.

  clear vg_parausd.
  READ TABLE tl_parametros INTO wl_parametros
   WITH KEY parid = 'Z105USD'.

  IF sy-subrc EQ 0. "Exibe USD
    vg_parausd = 'X'.
    LOOP AT SCREEN.
      IF screen-name EQ 'TXT_US' OR
         screen-name EQ 'WA_CABECALHO_0110-ADT_DMBE2' OR
         screen-name EQ 'WA_CABECALHO_0110-SEL_DMBE2' OR
         screen-name EQ 'WA_CABECALHO_0110-SLD_DMBE2'.
        screen-invisible = 0.
        screen-active    = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  SET PF-STATUS 'PF0110'.
  SET TITLEBAR 'T0110'.

ENDMODULE.

MODULE pbo_0110 OUTPUT.

  IF obj_container_0110 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0110'.

    CREATE OBJECT obj_container_0110
      EXPORTING
        container_name = 'CC_ALV_0110'.

    CREATE OBJECT obj_alv_0110
      EXPORTING
        i_parent = obj_container_0110.

    CREATE OBJECT obj_toolbar_0110
      EXPORTING
        io_alv_grid = obj_alv_0110.

    gs_layout-sel_mode    = 'A'.
    gs_layout-stylefname  = 'ESTILO'.
    gs_variant-report     = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

    PERFORM f_exclude_fcode USING '0110'.

    SET HANDLER: obj_toolbar_0110->on_toolbar          FOR obj_alv_0110,
                 obj_toolbar_0110->handle_user_command FOR obj_alv_0110.

    CALL METHOD obj_alv_0110->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0110.


    CALL METHOD obj_alv_0110->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0110->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler_0110=>on_data_changed_finished FOR obj_alv_0110,
                 lcl_event_handler_0110=>handle_hotspot_click     FOR obj_alv_0110.


  ELSE.
    CALL METHOD obj_alv_0110->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    PERFORM f_atualiza_saldo.
  ENDIF.

ENDMODULE.

MODULE status_0120 OUTPUT.
  SET PF-STATUS 'PF0120'.
  SET TITLEBAR 'T0120'.
ENDMODULE.

MODULE pbo_0120 OUTPUT.

  IF obj_container_0120 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0120'.

    CREATE OBJECT obj_container_0120
      EXPORTING
        container_name = 'CC_ALV_0120'.

    CREATE OBJECT obj_alv_0120
      EXPORTING
        i_parent = obj_container_0120.

    CREATE OBJECT obj_toolbar_0120
      EXPORTING
        io_alv_grid = obj_alv_0120.

    gs_layout-sel_mode    = 'A'.
    gs_layout-stylefname  = 'ESTILO'.
    gs_variant-report     = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

    PERFORM f_exclude_fcode USING '0120'.

    SET HANDLER: obj_toolbar_0120->on_toolbar          FOR obj_alv_0120,
                 obj_toolbar_0120->handle_user_command FOR obj_alv_0120.

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

    SET HANDLER: lcl_event_handler_0120=>handle_hotspot_click     FOR obj_alv_0120.


  ELSE.
    CALL METHOD obj_alv_0120->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0130  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0130 OUTPUT.

  DATA :BEGIN OF t_extab OCCURS 0,
          fcode LIKE rsmpe-func,
        END OF t_extab.

  REFRESH t_extab.

*  MOVE 'CANCEL' TO t_extab-fcode.
*  APPEND t_extab.

  CLEAR t_extab.
  SET PF-STATUS 'PF0130' EXCLUDING t_extab.

  SET TITLEBAR 'T0130'.
ENDMODULE.
