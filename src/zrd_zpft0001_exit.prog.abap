*&---------------------------------------------------------------------*
*& Report  ZRD_zpmt0078_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zrd_zpft0001_exit.

data: t_return  type standard table of ddshretval,
      v_emp     type bukrs,
      acesso(1) type c.

form f_exit_zpft0001_0001 changing p_registro_manter type any.

  data: wl_zpft0001 type zpft0001.

  clear: wl_zpft0001.

  wl_zpft0001-dt_criacao = sy-datum.
  wl_zpft0001-hr_criacao = sy-uzeit.
  wl_zpft0001-us_criacao = sy-uname.

  move-corresponding wl_zpft0001 to p_registro_manter.

endform.
