*&---------------------------------------------------------------------*
*&  Include  ZRD_ZMMT0183_EXIT
*&---------------------------------------------------------------------*
report zrd_ZMMT0183_exit.

data: t_return  type standard table of ddshretval.

form f_exit_ZMMT0183_0001 changing p_registro_manter type any.

  data: wl_ZMMT0183 type zmmt0182.

  clear: wl_ZMMT0183.

  wl_ZMMT0183-dt_criacao    = sy-datum.
  wl_ZMMT0183-hr_criacao    = sy-uzeit.
  wl_ZMMT0183-us_criacao    = sy-uname.

  move-corresponding wl_ZMMT0183 to p_registro_manter.

endform.


form f_exit_ZMMT0183_0003 changing p_registro_manter type any.

  data: wl_ZMMT0183    type ZMMT0183.

  move-corresponding p_registro_manter  to wl_ZMMT0183.

  wl_ZMMT0183-dt_criacao    = sy-datum.
  wl_ZMMT0183-hr_criacao    = sy-uzeit.
  wl_ZMMT0183-us_criacao    = sy-uname.

  move-corresponding wl_ZMMT0183 to p_registro_manter.

endform.


form f_exit_ZMMT0183_0005 changing p_registro_manter type any.

  data: wl_ZMMT0183    type ZMMT0183.

  clear: wl_ZMMT0183.

  move-corresponding p_registro_manter  to wl_ZMMT0183.
  move-corresponding wl_ZMMT0183 to p_registro_manter.

endform.
