function z_fbl3na_sysphera.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_PERIODO) TYPE  MONAT OPTIONAL
*"     VALUE(I_ANO) TYPE  GJAHR OPTIONAL
*"  TABLES
*"      RESULTADO STRUCTURE  ZSYS_FBL3N_ADT
*"      IT_EMPRESA STRUCTURE  FIN_CFIN_S_APAR_BURKS
*"----------------------------------------------------------------------

  data: lr_bukrs     type range of bukrs,
        lr_saknr_adt type range of saknr,
        lr_budat     type range of budat,
        w_resultado  like line of resultado,
        lv_datai     type dats,
        lv_dataf     type dats.

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
    exporting
      display  = abap_false
      metadata = abap_false
      data     = abap_true ).

  select * from tvarvc
    into table @data(lt_saknr_adt)
    where name eq 'SYSPHERA_CONTAS_ADT'
      and type eq 'S'.

  lr_saknr_adt = value #( for ls_adt  in lt_saknr_adt ( sign   = ls_adt-sign
                                                        option = ls_adt-opti
                                                        low    = ls_adt-low
                                                        high   = ls_adt-high ) ).

* USER STORY 160299 - MMSILVA - 04.12.2024 - INICIO
  if it_empresa[] is not initial.
    loop at it_empresa into data(wa_bukrs).
      append value #( sign = 'I' option = 'EQ' low = wa_bukrs-bukrs ) to lr_bukrs.
    endloop.
  else.
    lr_bukrs = value #( sign = 'I' option = 'BT' ( low = '0001' high = '0055' ) ).
  endif.
* USER STORY 160299 - MMSILVA - 04.12.2024 - FIM

* STF - IR250330 - Ajuste API SYSPHERA - 31/07/2025 - Inicio
  select SINGLE low
    from tvarvc
    into @data(lv_layout)
    where name eq 'Z_FBL3NA_SYSPHERA_VARIAVEL'.

  IF sy-subrc = 0.
    submit rfitemgl with sd_saknr in lr_saknr_adt
                  with sd_bukrs in lr_bukrs
                  with x_aisel  eq abap_true
                  with so_budat in lr_budat
                  with pa_vari  eq lv_layout
                  exporting list to memory
                  and return.

  ELSE.
    submit rfitemgl with sd_saknr in lr_saknr_adt
                  with sd_bukrs in lr_bukrs
                  with x_aisel  eq abap_true
                  with so_budat in lr_budat
                  with pa_vari  eq '/ABAP2'
                  exporting list to memory
                  and return.
  ENDIF.

**  submit rfitemgl with sd_saknr in lr_saknr_adt
**                  with sd_bukrs in lr_bukrs
**                  with x_aisel  eq abap_true
**                  with so_budat in lr_budat
**                  with pa_vari  eq '/ABAP'
**                  exporting list to memory
**                  and return.

* STF - IR250330 - Ajuste API SYSPHERA - 31/07/2025 - Fim

  try.
      cl_salv_bs_runtime_info=>get_data_ref(
        importing
          r_data_descr      = lr_data_descr
          r_data_line_descr = lr_data_line_descr ).

      if lr_data_descr is not initial.
        create data lr_data type handle lr_data_descr.


        assign lr_data->* to <lt_data>.

        cl_salv_bs_runtime_info=>get_data(
          importing
            t_data = <lt_data> ).
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
*comentao pra subir BUG  165804
**-US 74935-05-09-2024-#74935-RJF-início
*    ASSIGN COMPONENT 'XBLNR' OF STRUCTURE <LS_DATA> TO FIELD-SYMBOL(<FS_XBLNR>).
*    W_RESULTADO-XBLNR = <FS_XBLNR>.
*    ASSIGN COMPONENT 'EBELN' OF STRUCTURE <LS_DATA> TO FIELD-SYMBOL(<FS_EBELN>).
*    W_RESULTADO-EBELN = <FS_EBELN>.
**-US 74935-05-09-2024-#74935-RJF-fim

    if 'AA_DR_DA' cs w_resultado-blart.
      continue.
    endif.
* STF - IR250330 - Ajuste API SYSPHERA - 31/07/2025 - Inicio
    ASSIGN COMPONENT 'U_ANLN1' OF STRUCTURE <ls_data> to FIELD-SYMBOL(<fs_u_anln1>).

    IF sy-subrc = 0.
      w_resultado-u_anln1 = <fs_u_anln1>.
    ENDIF.

* STF - IR250330 - Ajuste API SYSPHERA - 31/07/2025 - Fim
    if w_resultado-u_anln1 is initial.
      select single anln1
        into w_resultado-u_anln1
        from bseg
        where bukrs = w_resultado-bukrs
        and   belnr = w_resultado-belnr
        and   gjahr = w_resultado-gjahr
        and   buzei = w_resultado-buzei.

    endif.

    assign component 'BELNR' of structure <ls_data> to field-symbol(<fs_belnr>).
    assign component 'JAMON' of structure <ls_data> to field-symbol(<fs_ano_mes>).
    split <fs_ano_mes> at '/' into w_resultado-ano w_resultado-mes.

    select single txt50 into w_resultado-ds_imobilizado
      from anla
      where bukrs eq w_resultado-bukrs
        and anln1 eq w_resultado-u_anln1.

*    "BUG 165804
    if lv_dataf le '20241231'.
      select single posnr into w_resultado-id_solicitacao
        from imaka
        where bukrs eq w_resultado-bukrs
          and anln1 eq w_resultado-u_anln1.

      select single txt50 into w_resultado-ds_solicitacao
        from imakt
        where posnr eq w_resultado-id_solicitacao
          and spras eq sy-langu.

      select single objnr into w_resultado-id_ordem
        from imakz
        where posnr eq w_resultado-id_solicitacao.
    else.
      select single leanz  into @data(_leanz)
      from anla
       where bukrs eq @w_resultado-bukrs
       and   anln1 eq @w_resultado-u_anln1.

      w_resultado-id_solicitacao = _leanz.
      clear: w_resultado-ds_solicitacao, w_resultado-id_ordem.
    endif.

    select single lifnr into w_resultado-cod_fornecedor
      from bseg
      where bukrs eq w_resultado-bukrs
        and anln1 eq w_resultado-u_anln1
        and belnr eq <fs_belnr>
        and lifnr ne space.

    select single name1 into w_resultado-ds_fornecedor
      from lfa1
      where lifnr eq w_resultado-cod_fornecedor.

* STF - IR250330 - Ajuste API SYSPHERA - 31/07/2025 - Inicio
    IF <fs_u_anln1> IS ASSIGNED AND <fs_u_anln1> IS NOT INITIAL.

      DATA lv_alnl1 TYPE anln1.

      lv_alnl1 = <fs_u_anln1>.

      SELECT SINGLE objnr
        FROM cobrb
        INTO @DATA(lv_id_ordem)
        WHERE konty = 'AN'
        AND bukrs = @w_resultado-bukrs
        AND anln1 = @lv_alnl1.

      IF sy-subrc = 0.
        data(lv_id_ordem_aux) = lv_id_ordem+2.

        w_resultado-id_ordem = | { lv_id_ordem_aux ALPHA = OUT } |.
      ENDIF.

    ENDIF.

    UNASSIGN <fs_u_anln1>.
    CLEAR lv_alnl1.
* STF - IR250330 - Ajuste API SYSPHERA - 31/07/2025 - Fim

    append w_resultado to resultado.
    clear w_resultado.
  endloop.


endfunction.
