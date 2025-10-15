function z_pfe_tipo_contab.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_DT_POSICAO) TYPE  ZDT_POSICAO
*"  EXPORTING
*"     REFERENCE(P_TIPCONTABIL) TYPE  ZTIPCONTABIL
*"----------------------------------------------------------------------

  data: it_setleaf    type table of setleaf with header line,
        vg_data_corte type sy-datum.

  p_tipcontabil = 'FC'.

  select * into table it_setleaf
    from setleaf
   where setname eq 'MAGI_DT_INI_CONF_ADM'.

  check sy-subrc is initial.

  read table it_setleaf index 1.

  vg_data_corte = it_setleaf-valfrom.
  if vg_data_corte le p_dt_posicao.
    p_tipcontabil = 'FS'.
  endif.

endfunction.
