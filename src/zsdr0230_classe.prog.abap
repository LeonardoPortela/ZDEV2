*&---------------------------------------------------------------------*
*& Include          ZMMR0045_CLASSE
*&---------------------------------------------------------------------*

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
        IMPORTING e_object,

      toolbar_item          FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_hotspot_click.
  ENDMETHOD.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

  METHOD user_command.

    CALL METHOD g_grid->get_selected_rows
      IMPORTING
        et_index_rows = t_rows_head.

    CASE e_ucomm.
      WHEN 'REENVIAR'.
        PERFORM f_reenviar_produto.

      WHEN OTHERS.
    ENDCASE.

    IF lines( t_rows_head ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows_head.
    ENDIF.

  ENDMETHOD.

  METHOD toolbar.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool     TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function    = 'REENVIAR'.
    wa_tool-icon        = icon_transfer.
    wa_tool-disabled    = abap_false.
    wa_tool-quickinfo   = 'Reenviar'.
    wa_tool-text        = 'Reenviar'.
    APPEND wa_tool     TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD toolbar_item.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
