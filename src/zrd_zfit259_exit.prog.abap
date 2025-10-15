*&---------------------------------------------------------------------*
*& Report  ZRD_zfit259_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zrd_zfit259_exit.



form f_exit_zfit259_0002    using p_registro_manter type any
                          changing p_erro.

  data: w_zfit259 type zfit259_out.

  move-corresponding p_registro_manter  to w_zfit259.

  select single *
           from t005s
           into @data(w_t005)
          where bland = @w_zfit259-uf_orig.
  if sy-subrc <> 0.
    p_erro = abap_true.
    message s024(sd) with 'Região origem  não existe.'
                     display like 'E'.
    exit.
  endif.

  select single *
           from t005s
           into w_t005
          where bland = w_zfit259-uf_dest.
  if sy-subrc <> 0.
    p_erro = abap_true.
    message s024(sd) with 'Região destino não existe.'
                     display like 'E'.
    exit.
  endif.


endform.

form f_exit_zfit259_0003 changing p_registro_manter type any.

  data: wl_zfit259    type zfit259.

  clear: wl_zfit259.

  move-corresponding p_registro_manter  to wl_zfit259.

  wl_zfit259-data_atual = sy-datum.
  wl_zfit259-hora_atual = sy-uzeit.
  wl_zfit259-usuario    = sy-uname.

  move-corresponding wl_zfit259 to p_registro_manter.

endform.

form f_exit_zfit259_0004 changing p_registro_manter type any.
endform.

form f_exit_zfit259_0005 changing p_registro_manter type any.
endform.

form f_exit_zfit259_0006 using p_registro_manter type any
                       changing p_erro.
endform.

form f_exit_zfit259_0008 changing p_col_pos
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

  if p_ref_tabname = 'zfit259_OUT' and
     p_field       = 'USUARIO'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 25.
    p_f4           = abap_true.
  endif.

  if p_ref_tabname = 'zfit259_OUT' and
     p_field       = 'DATA_ATUAL'.
    p_scrtext_l = 'Data Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  endif.

  if p_ref_tabname = 'zfit259_OUT' and
     p_field       = 'Hora Atual'.
    p_scrtext_l = 'Hora Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  endif.



endform.

form f_exit_zfit259_0009  tables pt_excl_toolbar
                            using p_db_tab.
endform.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
