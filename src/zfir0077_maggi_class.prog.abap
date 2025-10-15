*&---------------------------------------------------------------------*
*& Include          ZFIR0077_MAGGI_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_handle_events_r DEFINITION DEFERRED.
DATA: gr_events_r TYPE REF TO lcl_handle_events_r.
CLASS lcl_handle_events_r DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_user_command_r FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function,
      on_before_user_command_r FOR EVENT before_salv_function OF cl_salv_events IMPORTING e_salv_function,
      on_after_user_command_r FOR EVENT after_salv_function OF cl_salv_events IMPORTING e_salv_function,
      "on_hotspot_click FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING e_row_id e_column_id es_row_no,
      on_hotspot_click_r FOR EVENT link_click OF cl_salv_events_table IMPORTING column row sender,
      make_toolbar_r FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_interactive e_object,
*      on_handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
*        IMPORTING
*          er_data_changed
*          e_onf4
*          e_onf4_before
*          e_onf4_after
*          e_ucomm
*          sender,
      on_data_changed_finished_r FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.

ENDCLASS.
CLASS lcl_handle_events_r IMPLEMENTATION.
  METHOD on_user_command_r.
    PERFORM show_function_info USING e_salv_function TEXT-i08.
  ENDMETHOD.
  METHOD on_before_user_command_r.
    PERFORM show_function_info USING e_salv_function TEXT-i09.
  ENDMETHOD.
  METHOD on_after_user_command_r.
    PERFORM show_function_info USING e_salv_function TEXT-i10.
  ENDMETHOD.                 " on_link_click2

  METHOD on_hotspot_click_r.
    PERFORM hotspot USING row column.
  ENDMETHOD.
  METHOD make_toolbar_r.
    DATA mt_toolbar TYPE stb_button.
    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   " separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
      " 3 DESABILITA E 0 HABILITA
      IF <fs_tollbar>-function = '&LOCAL&COPY_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&LOCAL&CREATE_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&LOCAL&APPEND'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&LOCAL&INSERT_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&LOCAL&DELETE_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&REFRESH'.
        <fs_tollbar>-butn_type = '3'.
      ENDIF.
*      IF <fs_tollbar>-function EQ '&LOCAL&INSERT_ROW'.
*        <fs_tollbar>-function = 'INSERT_ROW'.
*      ELSEIF <fs_tollbar>-function EQ '&LOCAL&DELETE_ROW'.
*        <fs_tollbar>-function = 'DELETE_ROW'.
*      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_data_changed_finished_r.

    CHECK e_modified = 'X'.

  ENDMETHOD.
ENDCLASS.
*CLASS lcl_alv DEFINITION.
*  PUBLIC SECTION.
*    CLASS-METHODS:
*      simple_alv
*        IMPORTING
*          iv_start_col  TYPE i         DEFAULT 25
*          iv_start_line TYPE i         DEFAULT 6
*          iv_end_col    TYPE i         DEFAULT 100
*          iv_end_line   TYPE i         DEFAULT 10
*          iv_popup      TYPE abap_bool DEFAULT abap_false
*        CHANGING
*          ct_table      TYPE ANY TABLE.
*ENDCLASS.

*CLASS lcl_alv IMPLEMENTATION.
*  METHOD simple_alv.
*
*  ENDMETHOD.
*ENDCLASS.
