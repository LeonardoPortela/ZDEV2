*&---------------------------------------------------------------------*
*& Report ZIM21
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zim21.

data: t_resultado  type table of zsys_ksb1.

data: p_ano      type bkpf-gjahr,
      p_mes      type bkpf-monat,
      p_kokrs    type tka01-kokrs,
      vdatai     type sy-datum,
      t_area     type standard table of  rgsb4 with header line,
      wa_custo   type rsparams,
      t_custo    type table of  rsparams,
      v_sys_memo type char1.
parameters: p_ano1 type bkpf-gjahr,
            p_mes1 type bkpf-monat.

select-options p_data for sy-datum.

call function 'G_SET_GET_ALL_VALUES'
  exporting
    class         = '0000'
    setnr         = 'MAGGI_ZIM15_AREA'
  tables
    set_values    = t_area
  exceptions
    set_not_found = 1
    others        = 2.
if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
endif.
sort t_area by from.

if p_ano1 is initial.
  p_ano = sy-datum+0(4).
  p_mes = sy-datum+4(2).
elseif p_ano1 = 9999.
  p_ano = sy-datum+0(4).
  p_mes = sy-datum+4(2).
  if p_mes = '01'.
    p_ano = p_ano - 1.
    p_mes = '12'.
  else.
    p_mes = p_mes - 1.
  endif.
else.
  p_ano = p_ano1.
  p_mes = p_mes1.
endif.

vdatai = |{ p_ano }{ p_mes }01|.

if p_data-low is initial or p_data-low = vdatai.
  delete from zim14 where ano = p_ano and mes = p_mes.
  commit work.
endif.
"
v_sys_memo = 'X'.
export v_sys_memo            to memory id 'ZIM_MEMO'.
loop at t_area.
  p_kokrs = t_area-from.
  refresh t_resultado.
  call function 'Z_KSB1_SYSPHERA'
    exporting
      i_ano     = p_ano
      i_mes     = p_mes
      i_area    = p_kokrs
      i_datai   = p_data-low
      i_dataf   = p_data-high
      i_off     = 'X'
    tables
      t_custo   = t_custo
      resultado = t_resultado.
endloop.

delete from memory id 'ZIM_MEMO'.
