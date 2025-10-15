*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_PBO
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.

  DATA: tg_tcode TYPE TABLE OF sy-tcode.

  append 'OBS_BRANCH' to tg_tcode.

  SET PF-STATUS 'PF0100' EXCLUDING tg_tcode.
  SET TITLEBAR 'T0100'.
ENDMODULE.

MODULE pbo_0100 OUTPUT.

  IF obj_container_0100 IS INITIAL.

    PERFORM f_refresh_objetos.
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

    gs_layout-zebra       = 'X'.
    gs_layout-sel_mode    = 'A'.
    gs_layout-stylefname  = 'ESTILO'.
    gs_variant-report     = sy-repid.

    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

    SET HANDLER: obj_toolbar_0100->on_toolbar          FOR obj_alv_0100,
                 obj_toolbar_0100->handle_user_command FOR obj_alv_0100,
                 lcl_event_handler_0100=>catch_hotspot FOR obj_alv_0100.

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

    CALL METHOD obj_alv_0100->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0100->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler_0100=>on_data_changed_finished FOR obj_alv_0100,
                 lcl_event_handler_0100=>on_data_changed          FOR obj_alv_0100.

  ELSE.
    CALL METHOD obj_alv_0100->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDMODULE.

MODULE status_0110 OUTPUT.
*  SET PF-STATUS 'PF0110'.
*  SET TITLEBAR 'T0110'.
ENDMODULE.

MODULE status_0120 OUTPUT.
  SET PF-STATUS 'PF0120'.
  SET TITLEBAR 'T0120'.
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
*&---------------------------------------------------------------------*
*&      Module  STATUS_0130  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0130 OUTPUT.
  SET PF-STATUS 'PF0130'.
  SET TITLEBAR 'T0130'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0130  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0130 OUTPUT.

  IF obj_alv_0130 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0130'.

    IF obj_container_0130 IS INITIAL.
      CREATE OBJECT obj_container_0130
        EXPORTING
          container_name = 'CC_ALV_0130'.
    ENDIF.

    CREATE OBJECT obj_alv_0130
      EXPORTING
        i_parent = obj_container_0130.

    CREATE OBJECT obj_toolbar_0130
      EXPORTING
        io_alv_grid = obj_alv_0130.

    gs_layout-zebra       = 'X'.
    gs_layout-sel_mode    = 'A'.
    gs_layout-stylefname  = 'ESTILO'.
    gs_variant-report     = sy-repid.

    SET HANDLER: obj_toolbar_0130->on_toolbar          FOR obj_alv_0130,
                 obj_toolbar_0130->handle_user_command FOR obj_alv_0130.

    PERFORM f_exclude_fcode USING '0130'.

    CALL METHOD obj_alv_0130->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0130.

    CALL METHOD obj_alv_0130->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler_0130=>on_data_changed_finished FOR obj_alv_0130,
                 lcl_event_handler_0130=>on_data_changed          FOR obj_alv_0130.

  ELSE.
    CALL METHOD obj_alv_0130->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  CALL METHOD obj_alv_0130->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0140  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0140 OUTPUT.
  SET PF-STATUS 'PF0140'.
  SET TITLEBAR 'T0140'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0140  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0140 OUTPUT.

  IF obj_alv_0140 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0140'.

    IF obj_container_0140 IS INITIAL.
      CREATE OBJECT obj_container_0140
        EXPORTING
          container_name = 'CC_ALV_0140'.
    ENDIF.

    CREATE OBJECT obj_alv_0140
      EXPORTING
        i_parent = obj_container_0140.

    CREATE OBJECT obj_toolbar_0140
      EXPORTING
        io_alv_grid = obj_alv_0140.

    gs_layout-zebra       = 'X'.
    gs_layout-sel_mode    = 'A'.
    gs_layout-stylefname  = 'ESTILO'.
    gs_variant-report     = sy-repid.

    SET HANDLER: obj_toolbar_0140->on_toolbar          FOR obj_alv_0140,
                 obj_toolbar_0140->handle_user_command FOR obj_alv_0140.

*    SET HANDLER: LCL_EVENT_HANDLER_0140=>ON_DATA_CHANGED_FINISHED FOR OBJ_ALV_0140,
*                 LCL_EVENT_HANDLER_0140=>ON_DATA_CHANGED          FOR OBJ_ALV_0140.

    PERFORM f_exclude_fcode USING '0140'.

    CALL METHOD obj_alv_0140->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0140.

    CALL METHOD obj_alv_0140->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler_0140=>on_data_changed_finished FOR obj_alv_0140,
                 lcl_event_handler_0140=>on_data_changed          FOR obj_alv_0140.

  ELSE.
    CALL METHOD obj_alv_0140->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  CALL METHOD obj_alv_0140->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

ENDMODULE.

MODULE status_0150 OUTPUT.
  SET PF-STATUS 'PF0140'.
  SET TITLEBAR 'T0150'.
ENDMODULE.

MODULE pbo_0150 OUTPUT.

  IF obj_alv_0150 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0150'.

    IF obj_container_0150 IS INITIAL.
      CREATE OBJECT obj_container_0150
        EXPORTING
          container_name = 'CC_ALV_0150'.
    ENDIF.

    CREATE OBJECT obj_alv_0150
      EXPORTING
        i_parent = obj_container_0150.

    CREATE OBJECT obj_toolbar_0150
      EXPORTING
        io_alv_grid = obj_alv_0150.

    gs_layout-zebra       = 'X'.
    gs_layout-sel_mode    = 'A'.
    gs_layout-stylefname  = 'ESTILO'.
    gs_variant-report     = sy-repid.

    SET HANDLER: obj_toolbar_0150->on_toolbar          FOR obj_alv_0150,
                 obj_toolbar_0150->handle_user_command FOR obj_alv_0150.

*    SET HANDLER: LCL_EVENT_HANDLER_0140=>ON_DATA_CHANGED_FINISHED FOR OBJ_ALV_0140,
*                 LCL_EVENT_HANDLER_0140=>ON_DATA_CHANGED          FOR OBJ_ALV_0140.

    PERFORM f_exclude_fcode USING '0150'.

    CALL METHOD obj_alv_0150->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0150.

    CALL METHOD obj_alv_0150->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler_0150=>on_data_changed_finished FOR obj_alv_0150,
                 lcl_event_handler_0150=>on_data_changed          FOR obj_alv_0150.

  ELSE.
    CALL METHOD obj_alv_0150->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  CALL METHOD obj_alv_0150->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

ENDMODULE.
