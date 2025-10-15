*&---------------------------------------------------------------------*
*&  Include  ZRD_ZLEST0244_EXIT
*&---------------------------------------------------------------------*
report zrd_zsdt0348_exit.

data: t_return  type standard table of ddshretval.

form f_exit_zsdt0348_0001 changing p_registro_manter type any. "zsdt0348

  data: wl_zsdt0348 type zsdt0348.

  clear: wl_zsdt0348.

  wl_zsdt0348-dt_criacao    = sy-datum.
  wl_zsdt0348-hr_criacao    = sy-uzeit.
  wl_zsdt0348-us_criacao    = sy-uname.

  move-corresponding wl_zsdt0348 to p_registro_manter.

endform.

form f_exit_zsdt0348_0003 changing p_registro_manter type any.

  data: wl_zsdt0348    type zsdt0348.

  move-corresponding p_registro_manter  to wl_zsdt0348.

  wl_zsdt0348-dt_criacao    = sy-datum.
  wl_zsdt0348-hr_criacao    = sy-uzeit.
  wl_zsdt0348-us_criacao    = sy-uname.

  move-corresponding wl_zsdt0348 to p_registro_manter.

endform.
