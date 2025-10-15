FUNCTION-POOL zsd_sigam_interface.          "MESSAGE-ID ..

TABLES: zsde0003, zsde0004.", zsde0005, zsde0006.

DATA go_alv TYPE REF TO cl_salv_table.
DATA gt_rows TYPE salv_t_row.
DATA gv_popup_processa TYPE flag.

DATA st_btn_01 TYPE smp_dyntxt.
DATA st_btn_02 TYPE smp_dyntxt.
DATA gv_param TYPE zsde0011.
DATA gv_screen_status TYPE c.

DATA gt_187 TYPE TABLE OF zsdt0187.
DATA gt_066 TYPE TABLE OF zsdt0066.

DATA gt_lfa1 TYPE TABLE OF lfa1.
DATA gt_kna1 TYPE TABLE OF kna1.
DATA gt_makt TYPE TABLE OF makt.
DATA gt_branch TYPE TABLE OF j_1bbranch.
DATA gt_branch_lr TYPE TABLE OF j_1bbranch.
DATA gt_branch_x TYPE TABLE OF j_1bbranch.
DATA gt_0005_global TYPE TABLE OF zsde0005.
DATA gt_0005 TYPE TABLE OF zsde0005.
DATA gt_0006 TYPE TABLE OF zsde0006.
DATA gt_0006_global TYPE TABLE OF zsde0006.

DATA gv_9000_ucomm TYPE sy-ucomm.
DATA gv_9100_ucomm TYPE sy-ucomm.
DATA gv_9200_ucomm TYPE sy-ucomm.
DATA gv_9100_hide TYPE c.

DATA gv_cursor TYPE c LENGTH 40.
DATA gv_text_9100 TYPE msgtx.
DATA gv_qdr_003 TYPE msgtx.

DATA go_cc005_container TYPE REF TO cl_gui_custom_container.
DATA go_005_alv TYPE REF TO cl_gui_alv_grid.

DATA go_cc006_container TYPE REF TO cl_gui_custom_container.
DATA go_006_alv TYPE REF TO cl_gui_alv_grid.

DATA gw_bapiret TYPE bapiret2.

" DADOS DE MEMORIA PARA ME21N
DATA gv_process TYPE  zmme_me21n_proc.
DATA gs_lote    TYPE  zsde0005.
DATA gs_screen  TYPE  zsde0004.

CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

      "handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid.

*      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
*        IMPORTING er_data_changed.

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD  handle_double_click.
    PERFORM f_on_click USING e_row e_column es_row_no.
  ENDMETHOD.                    "on_user_command

*  METHOD handle_top_of_page.
*    PERFORM f_top_of_page.
*  ENDMETHOD.

*  METHOD handle_data_changed.
*    PERFORM f_on_data_changed USING er_data_changed.
*  ENDMETHOD.

ENDCLASS.                    "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_handle_events_popup DEFINITION.

  PUBLIC SECTION.
    METHODS : on_user_command
                FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

ENDCLASS.                    "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_handle_events_popup IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM f_user_command_popup USING e_salv_function.
  ENDMETHOD.                    "on_user_command

ENDCLASS.                    "lcl_handle_events DEFINITION
