*&---------------------------------------------------------------------*
*&  Include  zrd_ZFIT258_exit
*&---------------------------------------------------------------------*
report zrd_ZFIT258_exit.

data: t_return  type standard table of ddshretval.

form f_exit_ZFIT258_0001 changing p_registro_manter type any.

  data: wl_ZFIT258 type ZFIT258.

  clear: wl_ZFIT258.

  wl_ZFIT258-dt_criacao    = sy-datum.
  wl_ZFIT258-hr_criacao    = sy-uzeit.
  wl_ZFIT258-us_criacao    = sy-uname.

  move-corresponding wl_ZFIT258 to p_registro_manter.

endform.

form f_exit_ZFIT258_0002    using p_registro_manter type any
                           changing p_erro.

  data: wl_ZFIT258   type ZFIT258_OUT.
*
*  move-corresponding p_registro_manter  to wl_ZFIT258.
*
*  if wl_ZFIT258-witht is not initial.
*
*    select single text40
*    from t059u
*    into wl_ZFIT258-text_witht
*    where spras = sy-langu
*    and land1 = 'BR'
*    and witht = wl_ZFIT258-witht.
*    if sy-subrc ne 0.
*      p_erro = abap_true.
*      message s024(sd) with 'Código para categoria de imposto' 'retido na fonte não existe!' display like 'E'.
*      exit.
*    endif.
*  endif.
*
*
*  if wl_ZFIT258-wt_withcd is not initial.
*    select single text40
*    from t059zt
*    into wl_ZFIT258-text_wt_withcd
*    where spras = sy-langu
*    and land1 = 'BR'
*    and witht = wl_ZFIT258-witht
*    and wt_withcd = wl_ZFIT258-wt_withcd.
*    if sy-subrc ne 0.
*      p_erro = abap_true.
*      message s024(sd) with 'Código de imposto retido ' 'na fonte não existe!' display like 'E'.
*      exit.
*    endif.
*  endif.
*
**  if wl_ZFIT258-tipo_pessoa is not initial.
**    if wl_ZFIT258-tipo_pessoa ne 'J' or wl_ZFIT258-tipo_pessoa ne 'F'.
**      p_erro = abap_true.
**      message s024(sd) with 'Tipo de pessoa não permitido!'
**                       display like 'E'.
**      exit.
**    endif.
**  endif.
*
*  if wl_ZFIT258-regio is not initial.
*    select single bezei
*    from t005u
*    into wl_ZFIT258-bezei
*    where spras = sy-langu
*    and land1 = 'BR'
*    and bland = wl_ZFIT258-regio.
*    if sy-subrc ne 0.
*      p_erro = abap_true.
*      message s024(sd) with 'Região informada não existe!'
*                         display like 'E'.
*      exit.
*    endif.
*
*  endif.
*
*  move-corresponding wl_ZFIT258 to p_registro_manter.

endform.

form f_exit_ZFIT258_0003 changing p_registro_manter type any.

  data: wl_ZFIT258    type ZFIT258.

  move-corresponding p_registro_manter  to wl_ZFIT258.

  wl_ZFIT258-dt_criacao    = sy-datum.
  wl_ZFIT258-hr_criacao    = sy-uzeit.
  wl_ZFIT258-us_criacao    = sy-uname.

  move-corresponding wl_ZFIT258 to p_registro_manter.

endform.

form f_exit_ZFIT258_0004 changing p_saida type any.

  data: wl_ZFIT258  type ZFIT258_OUT.

  clear: wl_ZFIT258.

  move-corresponding p_saida  to wl_ZFIT258.

*  clear: wl_ZFIT258-bezei.
*  select single bezei
*  from t005u
*  into wl_ZFIT258-bezei
*  where spras = sy-langu
*  and land1 = 'BR'
*  and bland = wl_ZFIT258-regio.
*
*  if wl_ZFIT258-bezei is initial.
*    wl_ZFIT258-bezei = 'Outros Estados'.
*  endif.
*
*  clear: wl_ZFIT258-text_witht.
*  select single text40
*  from t059u
*  into wl_ZFIT258-text_witht
*  where spras = sy-langu
*  and land1 = 'BR'
*  and witht = wl_ZFIT258-witht.
*
*  clear: wl_ZFIT258-text_wt_withcd.
*  select single text40
*  from t059zt
*  into wl_ZFIT258-text_wt_withcd
*  where spras = sy-langu
*  and land1 = 'BR'
*  and witht = wl_ZFIT258-witht.
*
*  move-corresponding wl_ZFIT258 to p_saida.

endform.

form f_exit_ZFIT258_0005 changing p_registro_manter type any.

  data: wl_ZFIT258    type ZFIT258.

  clear: wl_ZFIT258.

  move-corresponding p_registro_manter  to wl_ZFIT258.
  move-corresponding wl_ZFIT258 to p_registro_manter.

endform.

form f_exit_ZFIT258_0006 using p_registro_manter type any
                       changing p_erro.

endform.

form f_exit_ZFIT258_0007 using p_registro_manter type any
                       changing p_erro.

endform.

form f_exit_ZFIT258_0008 changing p_col_pos
                                  p_ref_tabname
                                  p_ref_fieldname
                                  p_tabname
                                  p_field
                                  p_scrtext_l
                                  p_outputlen
                                  p_edit
                                  p_sum
                                  p_emphasize
                                  p_just
                                  p_hotspot
                                  p_f4
                                  p_check.

*
*  if p_ref_tabname = 'ZFIT258_OUT' and
*     p_field       = 'MATKL'.
*    p_scrtext_l = 'Grupo Mercadoria'.
*    p_outputlen = 20.
*    p_f4           = abap_true.
*  endif.
*
*  if p_ref_tabname = 'ZFIT258_OUT' and
*     p_field       = 'DESCRICAO'.
*    p_scrtext_l = 'Descrição'.
*    p_outputlen = 60.
*    p_f4           = abap_true.
*  endif.
*
*  if p_ref_tabname = 'ZFIT258_OUT' and
*     p_field       = 'USER_REG'.
*    p_scrtext_l = 'Usuário'.
*    p_outputlen = 20.
*    p_f4           = abap_true.
*  endif.
*
*  if p_ref_tabname = 'ZFIT258_OUT' and
*     p_field       = 'DATA_REG'.
*    p_scrtext_l = 'Data'.
*    p_outputlen = 20.
*    p_f4           = abap_true.
*  endif.
*
*  if p_ref_tabname = 'ZFIT258_OUT' and
*     p_field       = 'HORA_REG'.
*    p_scrtext_l = 'Hora'.
*    p_outputlen = 20.
*    p_f4           = abap_true.
*  endif.

endform.

form f_exit_ZFIT258_0009  tables it_excl_toolbar
                           using p_db_tab.

*  append 'Modificar'    to it_excl_toolbar.

endform.


form f_exit_ZFIT258_0013  tables p_tables.

  call function 'Z_ANALISE_LOGS_TABLE'
    exporting
      cusobj   = 'ZFIT258'
      tabfirst = 'X'.

endform.
