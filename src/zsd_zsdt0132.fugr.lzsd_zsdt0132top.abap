FUNCTION-POOL zsd_zsdt0132.                 "MESSAGE-ID ..

* INCLUDE LZSD_ZSDT0132D...                  " Local class definition

TABLES: zsde0003, zsde0004, zsde0006.

CONTROLS grd_zsde0005 TYPE TABLEVIEW USING SCREEN 9000.
CONTROLS grd_zsde0006 TYPE TABLEVIEW USING SCREEN 9000.
DATA gw_bapiret TYPE bapiret2.

DATA gv_error_num TYPE i.
DATA gv_renew TYPE c.
DATA gv_cursor TYPE c LENGTH 40.
DATA gv_ucomm TYPE sy-ucomm.
DATA gv_9100_ucomm TYPE sy-ucomm.
DATA gv_9100_hide TYPE c.
DATA gt_0005 TYPE TABLE OF zsde0005.
DATA gt_0006 TYPE TABLE OF zsde0006.
DATA gt_0006_global TYPE TABLE OF zsde0006.
DATA gt_lfa1 TYPE TABLE OF lfa1.
DATA gt_kna1 TYPE TABLE OF kna1.
DATA gt_makt TYPE TABLE OF makt.
DATA gt_branch TYPE TABLE OF j_1bbranch.

DATA gv_bukrs TYPE bukrs.
DATA gv_matnr TYPE matnr.

DATA go_cc005_container TYPE REF TO cl_gui_custom_container.
DATA go_005_alv TYPE REF TO cl_gui_alv_grid.

DATA go_cc006_container TYPE REF TO cl_gui_custom_container.
DATA go_006_alv TYPE REF TO cl_gui_alv_grid.

data gv_title TYPE sy-title.

CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no,

      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD  handle_double_click.
    PERFORM f_on_click USING e_row e_column es_row_no.
  ENDMETHOD.                    "on_user_command

  METHOD handle_data_changed.
    PERFORM f_on_data_changed USING er_data_changed.
  ENDMETHOD.

ENDCLASS.                    "lcl_handle_events DEFINITION
