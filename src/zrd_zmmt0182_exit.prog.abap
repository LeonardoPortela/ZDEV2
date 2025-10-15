*&---------------------------------------------------------------------*
*&  Include  ZRD_ZLEST0244_EXIT
*&---------------------------------------------------------------------*
report zrd_zmmt0182_exit.

data: t_return  type standard table of ddshretval.

form f_exit_ZMMT0182_0001 changing p_registro_manter type any.

  data: wl_ZMMT0182 type zmmt0182.

  clear: wl_ZMMT0182.

  wl_ZMMT0182-dt_criacao    = sy-datum.
  wl_ZMMT0182-hr_criacao    = sy-uzeit.
  wl_ZMMT0182-us_criacao    = sy-uname.

  move-corresponding wl_ZMMT0182 to p_registro_manter.

endform.

form f_exit_zmmt0182_0002    using p_registro_manter type any
                           changing p_erro.

  data: wl_ZMMT0182   type zmme0182_out.
*
  move-corresponding p_registro_manter  to wl_ZMMT0182.

  if wl_zmmt0182-witht is not initial.

    select single text40
    from t059u
    into wl_zmmt0182-text_witht
    where spras = sy-langu
    and land1 = 'BR'
    and witht = wl_zmmt0182-witht.
    if sy-subrc ne 0.
      p_erro = abap_true.
      message s024(sd) with 'Código para categoria de imposto' 'retido na fonte não existe!' display like 'E'.
      exit.
    endif.
  endif.


  if wl_zmmt0182-wt_withcd is not initial.
    select single text40
    from t059zt
    into wl_zmmt0182-text_wt_withcd
    where spras = sy-langu
    and land1 = 'BR'
    and witht = wl_zmmt0182-witht
    and wt_withcd = wl_zmmt0182-wt_withcd.
    if sy-subrc ne 0.
      p_erro = abap_true.
      message s024(sd) with 'Código de imposto retido ' 'na fonte não existe!' display like 'E'.
      exit.
    endif.
  endif.

*  if wl_ZMMT0182-tipo_pessoa is not initial.
*    if wl_ZMMT0182-tipo_pessoa ne 'J' or wl_ZMMT0182-tipo_pessoa ne 'F'.
*      p_erro = abap_true.
*      message s024(sd) with 'Tipo de pessoa não permitido!'
*                       display like 'E'.
*      exit.
*    endif.
*  endif.

  if wl_zmmt0182-regio is not initial.
    select single bezei
    from t005u
    into wl_ZMMT0182-bezei
    where spras = sy-langu
    and land1 = 'BR'
    and bland = wl_zmmt0182-regio.
    if sy-subrc ne 0.
      p_erro = abap_true.
      message s024(sd) with 'Região informada não existe!'
                         display like 'E'.
      exit.
    endif.

  endif.

  move-corresponding wl_ZMMT0182 to p_registro_manter.

endform.

form f_exit_ZMMT0182_0003 changing p_registro_manter type any.

  data: wl_ZMMT0182    type zmmt0182.

  move-corresponding p_registro_manter  to wl_ZMMT0182.

  wl_ZMMT0182-dt_criacao    = sy-datum.
  wl_ZMMT0182-hr_criacao    = sy-uzeit.
  wl_ZMMT0182-us_criacao    = sy-uname.

  move-corresponding wl_ZMMT0182 to p_registro_manter.

endform.

form f_exit_ZMMT0182_0004 changing p_saida type any.

  data: wl_zmmt0182  type ZMME0182_out.

  clear: wl_zmmt0182.

  move-corresponding p_saida  to wl_zmmt0182.

  clear: wl_zmmt0182-bezei.
  select single bezei
  from t005u
  into wl_ZMMT0182-bezei
  where spras = sy-langu
  and land1 = 'BR'
  and bland = wl_zmmt0182-regio.

  if wl_ZMMT0182-bezei is initial.
    wl_ZMMT0182-bezei = 'Outros Estados'.
  endif.

  clear: wl_zmmt0182-text_witht.
  select single text40
  from t059u
  into wl_zmmt0182-text_witht
  where spras = sy-langu
  and land1 = 'BR'
  and witht = wl_zmmt0182-witht.

  clear: wl_zmmt0182-text_wt_withcd.
  select single text40
  from t059zt
  into wl_zmmt0182-text_wt_withcd
  where spras = sy-langu
  and land1 = 'BR'
  and witht = wl_zmmt0182-witht.

  move-corresponding wl_zmmt0182 to p_saida.

endform.

form f_exit_ZMMT0182_0005 changing p_registro_manter type any.

  data: wl_ZMMT0182    type zmmt0182.

  clear: wl_ZMMT0182.

  move-corresponding p_registro_manter  to wl_ZMMT0182.
  move-corresponding wl_ZMMT0182 to p_registro_manter.

endform.

form f_exit_zlest0244_0006 using p_registro_manter type any
                       changing p_erro.

endform.

form f_exit_ZMMT0182_0008 changing p_col_pos
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
*  if p_ref_tabname = 'ZMME0182_OUT' and
*     p_field       = 'MATKL'.
*    p_scrtext_l = 'Grupo Mercadoria'.
*    p_outputlen = 20.
*    p_f4           = abap_true.
*  endif.
*
*  if p_ref_tabname = 'ZMME0182_OUT' and
*     p_field       = 'DESCRICAO'.
*    p_scrtext_l = 'Descrição'.
*    p_outputlen = 60.
*    p_f4           = abap_true.
*  endif.
*
*  if p_ref_tabname = 'ZMME0182_OUT' and
*     p_field       = 'USER_REG'.
*    p_scrtext_l = 'Usuário'.
*    p_outputlen = 20.
*    p_f4           = abap_true.
*  endif.
*
*  if p_ref_tabname = 'ZMME0182_OUT' and
*     p_field       = 'DATA_REG'.
*    p_scrtext_l = 'Data'.
*    p_outputlen = 20.
*    p_f4           = abap_true.
*  endif.
*
*  if p_ref_tabname = 'ZMME0182_OUT' and
*     p_field       = 'HORA_REG'.
*    p_scrtext_l = 'Hora'.
*    p_outputlen = 20.
*    p_f4           = abap_true.
*  endif.

endform.

form f_exit_ZMMT0182_0009  tables it_excl_toolbar
                           using p_db_tab.

*  append 'Modificar'    to it_excl_toolbar.

endform.


form f_exit_ZMMT0182_0013  tables p_tables.

  call function 'Z_ANALISE_LOGS_TABLE'
    exporting
      cusobj   = 'ZMMT0182'
      tabfirst = 'X'.

endform.

form f_exit_zlest0244_0017 using p_tipo.

endform.

form f4_val_valida using p_cod type help_info-dynprofld.

endform.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
