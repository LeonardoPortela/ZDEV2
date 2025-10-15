*----------------------------------------------------------------------*
***INCLUDE LZGFS_TRACE_COTTONO02.
*----------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed       FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      toolbar               FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.

    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good
                             WHERE fieldname = 'MARK'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      READ TABLE t_lotes  INTO w_lotes  INDEX ls_good-row_id.

      IF lv_value = abap_off.
        w_lotes-quantidade      = 0.
        w_lotes-quantidade_disp = w_lotes-quantidade_disp_orig - w_lotes-quantidade.
        w_lotes-quantidade_util = w_lotes-quantidade_util_orig + w_lotes-quantidade.
        MODIFY t_lotes       FROM w_lotes INDEX ls_good-row_id.
      ENDIF.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good
                             WHERE fieldname = 'QUANTIDADE'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      READ TABLE t_lotes  INTO w_lotes  INDEX ls_good-row_id.

      w_lotes-quantidade           = lv_value.
      w_lotes-quantidade_disp      = w_lotes-quantidade_disp_orig - w_lotes-quantidade.
      w_lotes-quantidade_util      = w_lotes-quantidade_util_orig + w_lotes-quantidade.
      w_lotes-mark                 = COND #( WHEN lv_value = '0000000000' THEN abap_off
                                                                     ELSE abap_true ).
      MODIFY t_lotes            FROM w_lotes INDEX ls_good-row_id.

    ENDLOOP.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

  METHOD on_hotspot_click.
  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

  METHOD user_command.

    CASE e_ucomm.

      WHEN 'MARCAR_ALL'.
        LOOP AT t_lotes INTO w_lotes.
          w_lotes-mark = abap_on.
          MODIFY t_lotes FROM w_lotes.
        ENDLOOP.

      WHEN 'DESMARCAR_ALL'.
        LOOP AT t_lotes INTO w_lotes.
          w_lotes-mark = abap_off.
          MODIFY t_lotes FROM w_lotes.
        ENDLOOP.
    ENDCASE.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

  METHOD toolbar.

    DATA wa_tool TYPE stb_button.

*-US 119847 - 01.08.2023 - JT - inicio
*    FREE: e_object->mt_toolbar.
*-US 119847 - 01.08.2023 - JT - fim

    CHECK g_somente_exibe = abap_false.

*   MOVE 3 TO wa_tool-butn_type.
*   APPEND wa_tool TO e_object->mt_toolbar.
*   CLEAR wa_tool.

    wa_tool-function  = 'MARCAR_ALL'.
    wa_tool-icon      = '@4B@'.
    wa_tool-text      = 'Marcar Todos'.
    wa_tool-quickinfo = 'Marcar Todos'.
    APPEND wa_tool    TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function  = 'DESMARCAR_ALL'.
    wa_tool-icon      = '@4D@'.
    wa_tool-text      = 'Desmarcar Todos'.
    wa_tool-quickinfo = 'Desmarcar Todos'.
    APPEND wa_tool   TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
