FUNCTION-POOL zgfsd006.                     "MESSAGE-ID ..

* INCLUDE LZGFSD006D...                      " Local class definition

TABLES: zsds379,zsds0006.
DATA gv_popup_processa.
DATA gv_checkid TYPE zsdcheckid.
DATA go_alv TYPE REF TO cl_salv_table.
DATA gt_rows TYPE salv_t_row.
DATA gt_0378 TYPE TABLE OF zsdt0378.
DATA gt_0378_r TYPE TABLE OF zsdt0378_r.
DATA gt_perguntas TYPE zsdc380.


" ---------TELA CHECKLIST
DATA gt_zsdt0380 TYPE TABLE OF zsdt0380.
DATA gt_zsdt0380_r TYPE TABLE OF zsdt0380_r.
DATA g_container_8001 TYPE scrfname VALUE 'CC_CUST_8001'.
DATA go_container_8001 TYPE REF TO cl_gui_custom_container.
DATA go_cc_alv_01 TYPE REF TO cl_gui_alv_grid.
DATA gt_fcat_8001 TYPE lvc_t_fcat.
DATA gt_alv_8001 TYPE TABLE OF zsds378.
DATA gv_ucomm_8001 TYPE sy-ucomm.

"----------TELA PREENCHIMENTO CHECKLIST X SIMULADOR
DATA gt_filter_7001 TYPE lvc_t_filt.
DATA g_container_7001 TYPE scrfname VALUE 'CC_CUST_7001'.
DATA go_container_7001 TYPE REF TO cl_gui_custom_container.
DATA go_cc_alv_02 TYPE REF TO cl_gui_alv_grid.
DATA gt_fcat_7001 TYPE lvc_t_fcat.
DATA gt_alv_7001_full TYPE TABLE OF zsds0006.
DATA gt_alv_7001 TYPE TABLE OF zsds0006.
DATA gv_ucomm_7001 TYPE sy-ucomm.

DATA gr_custom_cont TYPE REF TO cl_gui_custom_container.
DATA gr_text_edit   TYPE REF TO cl_gui_textedit.

*&---------------------------------------------------------------------*
*& Include          LZGRWM_006F02
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.

  PUBLIC SECTION.
    METHODS : on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function,

      before_salv_function FOR EVENT before_salv_function OF cl_salv_events
        IMPORTING sender e_salv_function .


ENDCLASS.                    "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD before_salv_function.
    BREAK-POINT.
  ENDMETHOD.

  METHOD on_user_command.
    PERFORM f_user_command_popup USING e_salv_function.
  ENDMETHOD.                    "on_user_command

ENDCLASS.                    "lcl_handle_events DEFINITION

CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,

      handle_double_click for event DOUBLE_CLICK of cl_gui_alv_grid
        IMPORTING e_row  e_column es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_hotspot_click.
    PERFORM f_hotspot_click USING e_row_id e_column_id es_row_no.
  ENDMETHOD.

  METHOD  handle_toolbar.
    PERFORM f_handle_toolbar USING e_object e_interactive.
  ENDMETHOD.                    "on_user_command

  METHOD handle_user_command.
    PERFORM handle_user_command USING e_ucomm.
  ENDMETHOD.

  METHOD handle_double_click.
    PERFORM handle_double_click_8001 USING e_row e_column es_row_no.
  ENDMETHOD.

ENDCLASS.                    "lcl_handle_events DEFINITION
CLASS lcl_event_handler_7001 DEFINITION .
  PUBLIC SECTION .
    METHODS:
      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler_7001 IMPLEMENTATION.

  METHOD handle_double_click.
    PERFORM handle_double_click_7001 USING e_row e_column es_row_no.
  ENDMETHOD.

ENDCLASS.                    "lcl_handle_events DEFINITION
