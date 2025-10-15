function z_fbl3n_sysphera.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_PERIODO) TYPE  MONAT OPTIONAL
*"     VALUE(I_ANO) TYPE  GJAHR OPTIONAL
*"  TABLES
*"      RESULTADO STRUCTURE  ZSYS_FBL3N
*"      IT_EMPRESA STRUCTURE  FIN_CFIN_S_APAR_BURKS
*"----------------------------------------------------------------------

  data: lr_bukrs      type range of bukrs,
        lr_saknr_real type range of saknr,
        lr_budat      type range of budat,
        w_resultado   like line of resultado,
        lv_datai      type dats,
        lv_dataf      type dats.

  if i_periodo is initial.
    i_periodo = sy-datum+4(2).
  endif.
  if i_ano is initial.
    i_ano = sy-datum(4).
  endif.

  lv_datai = |{ i_ano }{ i_periodo }01|.
  call function 'RP_LAST_DAY_OF_MONTHS'
    exporting
      day_in            = lv_datai
    importing
      last_day_of_month = lv_dataf.

  lr_budat = value #( sign = 'I' option = 'BT' ( low = lv_datai high = lv_dataf ) ).

  cl_salv_bs_runtime_info=>set(
    exporting display  = abap_false
              metadata = abap_false
              data     = abap_true ).

  select * from tvarvc
    into table @data(lt_saknr_real)
    where name eq 'SYSPHERA_CONTAS_REAL'
      and type eq 'S'.

  lr_saknr_real = value #( for ls_real in lt_saknr_real ( sign   = ls_real-sign
                                                          option = ls_real-opti
                                                          low    = ls_real-low
                                                          high   = ls_real-high ) ).

* USER STORY 160299 - MMSILVA - 04.12.2024 - INICIO
  if it_empresa[] is not initial.
    loop at it_empresa into data(wa_bukrs).
      append value #( sign = 'I' option = 'EQ' low = wa_bukrs-bukrs ) to lr_bukrs.
    endloop.
  else.
    lr_bukrs = value #( sign = 'I' option = 'BT' ( low = '0001' high = '0055' ) ).
  endif.
* USER STORY 160299 - MMSILVA - 04.12.2024 - FIM

  submit rfitemgl with sd_saknr in lr_saknr_real
                  with sd_bukrs in lr_bukrs
                  with x_aisel  eq abap_true
                  with xnorm    eq abap_true
                  with so_budat in lr_budat
*                  with pa_vari  eq '/ABAP'
                  exporting list to memory
                  and return.


  try.
      cl_salv_bs_runtime_info=>get_data_ref(
          importing r_data_descr      = lr_data_descr
                    r_data_line_descr = lr_data_line_descr ).

      if lr_data_descr is not initial.
        create data lr_data type handle lr_data_descr.


        assign lr_data->* to <lt_data>.

        cl_salv_bs_runtime_info=>get_data(
          importing t_data      = <lt_data> ).
      endif.

    catch cx_salv_bs_sc_runtime_info.
*      MESSAGE 'Não é possível recuperar os dados ALV' TYPE 'E'.
  endtry.

  check <lt_data> is assigned.

  loop at <lt_data> assigning <ls_data>.
    move-corresponding <ls_data> to w_resultado.
    if w_resultado-dmshb eq 0 and
       w_resultado-dmbe2 eq 0.
      continue.
    endif.

    if 'AA_DR_DA' cs w_resultado-blart.
      continue.
    endif.
    if w_resultado-u_anln1 is initial.
      select single anln1
        into w_resultado-u_anln1
        from bseg
        where bukrs = w_resultado-bukrs
        and   belnr = w_resultado-belnr
        and   gjahr = w_resultado-gjahr
        and   buzei = w_resultado-buzei.

    endif.
    if w_resultado-u_anln1 is initial.
      continue.
    endif.
    assign component 'JAMON' of structure <ls_data> to field-symbol(<fs_ano_mes>).
    split <fs_ano_mes> at '/' into w_resultado-ano w_resultado-mes.



    select single txt50 into w_resultado-ds_imobilizado
      from anla
      where bukrs eq w_resultado-bukrs
        and anln1 eq w_resultado-u_anln1.

*
    select single txt50 into w_resultado-ds_conta
      from skat
      where saknr eq w_resultado-konto
        and ktopl eq '0050'
        and spras eq sy-langu.

*    "BUG 165804
    if lv_dataf le '20241231'.
      select single posnr into w_resultado-solic_invest
        from imaka
        where bukrs eq w_resultado-bukrs
          and anln1 eq w_resultado-u_anln1.

      select single txt50 into w_resultado-ds_solic_invest
        from imakt
        where posnr eq w_resultado-solic_invest
          and spras eq sy-langu.

      select single solicitacao_invest into w_resultado-cod_sysphera
        from zim01_sol_ap_inv
        where posnr eq w_resultado-solic_invest.
    else.
      select single leanz  into @data(_leanz)
        from anla
         where bukrs eq @w_resultado-bukrs
         and   anln1 eq @w_resultado-u_anln1.
      w_resultado-cod_sysphera = _leanz.
      clear: w_resultado-solic_invest, w_resultado-ds_solic_invest.
    endif.
*     "BUG 165804

    select single kostl into w_resultado-kostl
      from anlz
      where bukrs eq w_resultado-bukrs
        and anln1 eq w_resultado-u_anln1
        and bdatu ge sy-datum.

    select single ktext into w_resultado-ds_centro_custo
      from cskt
      where kostl eq w_resultado-kostl
        and spras eq sy-langu.

    append w_resultado to resultado.
    clear w_resultado.
  endloop.


endfunction.
