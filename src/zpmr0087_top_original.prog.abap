*&---------------------------------------------------------------------*
*& Include          ZPMR0087_TOP
*&---------------------------------------------------------------------*

TABLES coep.

TYPES: BEGIN OF lin_data,
         bukrs TYPE bukrs,
         perio TYPE perio,
         gjahr TYPE gjahr,
         icon  TYPE icon_d,
         desc(12)  TYPE c,
       END OF lin_data,

       tab_data TYPE TABLE OF lin_data.


DATA: gt_coep TYPE TABLE OF coep,
      gt_job  TYPE TABLE OF tvarvc.


DATA: gr_container TYPE REF TO cl_gui_custom_container.

DATA:
  gr_alv       TYPE REF TO cl_salv_table,
  gr_functions TYPE REF TO cl_salv_functions,
  gr_columns   TYPE REF TO cl_salv_columns,
  gr_layout    TYPE REF TO cl_salv_layout.

DATA: s_bukrs TYPE RANGE OF bukrs,
      p_kokrs TYPE coep-kokrs VALUE 'MAGI',
      p_perio TYPE coep-perio,
      p_gjahr TYPE coep-gjahr.

DATA: g_okcode TYPE syucomm,
      gv_status(12).

DATA: gt_data TYPE tab_data.

*----------------------------------------------------------------------*
* CONSTANTS                                                            *
*----------------------------------------------------------------------*
CONSTANTS:

  gc_light_inactive     TYPE   icon_d    VALUE '@EB@',
  gc_light_red          TYPE   icon_d    VALUE '@0A@',
  gc_light_yellow       TYPE   icon_d    VALUE '@09@',
  gc_light_green        TYPE   icon_d    VALUE '@08@',
  gc_btn_start_job      TYPE   string    VALUE 'bt_start_job',
  gc_btn_close_job      TYPE   string    VALUE 'bt_close_job',
  gc_btn_start_job_ant  TYPE   string    VALUE 'bt_start_job_ant',
  gc_btn_refresh_job    TYPE   string    VALUE 'bt_refresh_job',
  gc_icon_close         TYPE   icon_d    VALUE '@DF@',
  gc_icon_refresh       TYPE   icon_d    VALUE '@42@',
  gc_background_job     TYPE   icon_d    VALUE '@M4@',
  gc_background_job_ant TYPE   icon_d    VALUE '@EP@',
  gc_terminated_job     TYPE   icon_d    VALUE '@LQ@',
  gc_prog_exec          TYPE   sy-repid  VALUE 'RKO7KO8G',
  gc_prog_variant       type   sy-repid  VALUE 'RKOSEL00',
  gc_liquida(3)         TYPE   c         VALUE 'LIQ'.
