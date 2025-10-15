*&---------------------------------------------------------------------*
*& Include          ZPMR0087_TOP
*&---------------------------------------------------------------------*

tables coep.

types: begin of lin_data,
         bukrs    type bukrs,
         perio    type perio,
         gjahr    type gjahr,
         icon     type icon_d,
         desc(12) type c,
       end of lin_data,

       tab_data type table of lin_data.

types: begin of ty_saldo_ordem,
         aufnr           type coep-aufnr,
         gjahr           type coep-gjahr,
         perio           type coep-perio,
         total_custos    type coep-wtgbtr,
         total_liquidado type coep-wtgbtr,
       end of ty_saldo_ordem.

types: begin of ty_pendente,
         aufnr type coep-aufnr,
         gjahr type coep-gjahr,
         perio type coep-perio,
       end of ty_pendente.


DATA: lt_saldos    TYPE TABLE OF ty_saldo_ordem,
      lt_pendentes TYPE TABLE OF ty_pendente.


data: gt_coep type table of coep,
      gt_job  type table of tvarvc.




data: gr_container type ref to cl_gui_custom_container.

data:
  gr_alv       type ref to cl_salv_table,
  gr_functions type ref to cl_salv_functions,
  gr_columns   type ref to cl_salv_columns,
  gr_layout    type ref to cl_salv_layout.

data: s_bukrs type range of bukrs,
      p_kokrs type range of coep-kokrs,
      p_perio type coep-perio,
      p_gjahr type coep-gjahr.

data: g_okcode        type syucomm,
      gv_status(12),
      gv_set_erro(01).

data: gt_data type tab_data.

*----------------------------------------------------------------------*
* CONSTANTS                                                            *
*----------------------------------------------------------------------*
constants:

  gc_light_inactive     type   icon_d    value '@EB@',
  gc_light_red          type   icon_d    value '@0A@',
  gc_light_yellow       type   icon_d    value '@09@',
  gc_light_green        type   icon_d    value '@08@',
  gc_btn_start_job      type   string    value 'bt_start_job',
  gc_btn_close_job      type   string    value 'bt_close_job',
  gc_btn_start_job_ant  type   string    value 'bt_start_job_ant',
  gc_btn_refresh_job    type   string    value 'bt_refresh_job',
  gc_icon_close         type   icon_d    value '@DF@',
  gc_icon_refresh       type   icon_d    value '@42@',
  gc_background_job     type   icon_d    value '@M4@',
  gc_background_job_ant type   icon_d    value '@EP@',
  gc_terminated_job     type   icon_d    value '@LQ@',
  gc_prog_exec          type   sy-repid  value 'RKO7KO8G',
  gc_prog_variant       type   sy-repid  value 'RKOSEL00',
  gc_liquida(3)         type   c         value 'LIQ'.

data: lv_variant       type raldb-variant,
      lv_jobname       type tbtcjob-jobname,  " Nome do job
      "lv_jobcount      TYPE tbtcjob-jobcount,
      lt_variant       type disvariant,
      lt_vari_contents type standard table of rsparams,
      ls_vari_desc     type varid,
      lt_vari_text     type standard table of varit.
