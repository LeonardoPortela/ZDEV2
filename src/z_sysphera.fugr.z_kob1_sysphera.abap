function z_kob1_sysphera.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_AREA) TYPE  KOKRS DEFAULT 'MAGI'
*"     VALUE(IT_ORDENS) TYPE  ZTTSYS_AUFNR
*"     VALUE(I_PERIODO) TYPE  MONAT OPTIONAL
*"     VALUE(I_ANO) TYPE  GJAHR OPTIONAL
*"  TABLES
*"      RESULTADO STRUCTURE  ZSYS_KOB1
*"      PEDIDOS STRUCTURE  ZSYS_KOB1 OPTIONAL
*"----------------------------------------------------------------------

  data: lr_budat         type range of budat,
        lt_resultado_aux type table of zsys_kob1,
        lr_aufnr         type range of aufnr,
        w_pedidos        like line of pedidos, "RJF
        w_resultado      like line of resultado.

*-US 74935-05-09-2024-#74935-RJF-início
  if i_ano is not initial and i_periodo is not initial.
    data: lv_dat  type sy-datum,
          lv_udia type sy-datum.
    lv_dat = i_ano && i_periodo && '01'.

    call function 'RP_LAST_DAY_OF_MONTHS'
      exporting
        day_in            = lv_dat
      importing
        last_day_of_month = lv_udia.

    lr_budat = value #( sign = 'I' option = 'BT' ( low = lv_dat high = lv_udia ) ).
    lr_aufnr = value #( for ls_ordens in it_ordens ( sign = 'I' option = 'EQ' low = ls_ordens ) ).
  else.
*-US 74935-05-09-2024-#74935-RJF-fim

    lr_budat = value #( sign = 'I' option = 'BT' ( low = '20100101' high = '20991231' ) ).
    lr_aufnr = value #( for ls_ordens in it_ordens ( sign = 'I' option = 'EQ' low = ls_ordens ) ).

*-US 74935-05-09-2024-#74935-RJF-início
  endif.
*-US 74935-05-09-2024-#74935-RJF-fim

  cl_salv_bs_runtime_info=>set(
    exporting
      display  = abap_false
      metadata = abap_false
      data     = abap_true ).

  submit rkaep000 with p_tcode  eq 'KOB1'
                  with p_kokrs  eq i_area
                  with p_maxsel eq 999999999
                  with aufnr    in lr_aufnr
                  with r_budat  in lr_budat
                  exporting list to memory
                  and return.


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
    if w_resultado-wogbtr eq 0 and
       w_resultado-wkgbtr eq 0.
      continue.
    endif.
    "filtro da variante /J-INVEST_OR
    if w_resultado-kstar = '0000510142'.
      continue.
    endif.

*    "filtro da variante /L_INVES_ORD
*    if w_resultado-cel_ktxt =  'Liquid.Ord.Int. Inve'.
*      continue.
*    endif.
*    if w_resultado-kstar = '0000510141' or
*       w_resultado-kstar = '0000412516' or
*       w_resultado-kstar = '0000412527' or
*       w_resultado-kstar = '0000412561' or
*       w_resultado-kstar = '0000412563' or
*       w_resultado-kstar = '0000414301' or
*       w_resultado-kstar = '0000422203' or
*       w_resultado-kstar = '0000422207'.
*      continue.
*    endif.
*    "filtro da variante /L_INVES_ORD
    append w_resultado to lt_resultado_aux.
  endloop.

  clear w_resultado.
  loop at lt_resultado_aux into data(w_resultado_aux).
    w_resultado-wogbtr   = w_resultado-wogbtr + w_resultado_aux-wogbtr.
    w_resultado-wkgbtr   = w_resultado-wkgbtr + w_resultado_aux-wkgbtr.
    w_resultado-gjahr    =  w_resultado_aux-gjahr.
    w_resultado-perio    =  w_resultado_aux-perio.
    w_resultado-aufnr    = w_resultado_aux-aufnr .

*-US 74935-05-09-2024-#74935-RJF-início
    w_pedidos-ebeln      = w_resultado_aux-ebeln.
    append w_pedidos to pedidos.
    clear: w_pedidos.
*-US 74935-05-09-2024-#74935-RJF-fim

    at end of aufnr.
      data(lv_objnr) = |OR{ w_resultado-aufnr }|.
      select single posnr into @data(lv_posnr) from imakz
        where objnr eq @lv_objnr.
      if sy-subrc eq 0.
        select single anln1 into w_resultado-id_imobilizado from imaka
          where posnr eq lv_posnr.
      endif.

* STF - IR250598 - Ajuste API SYSPHERA - 13/08/2025 - Inicio
      IF w_resultado-id_imobilizado IS INITIAL.
        SELECT SINGLE anln1
          FROM cobrb
          INTO w_resultado-id_imobilizado
          WHERE objnr = lv_objnr
          AND gabja = i_ano
          AND gabpe = i_periodo.

          IF sy-subrc <> 0.
            SELECT SINGLE anln1
              FROM cobrb
              INTO w_resultado-id_imobilizado
              WHERE objnr = lv_objnr
              AND gabja = space
              AND gabpe = space.
          ENDIF.
      ENDIF.
* STF - IR250598 - Ajuste API SYSPHERA - 13/08/2025 - Fim

      append w_resultado to resultado.
      clear: w_resultado.
    endat.
  endloop.
  sort pedidos by ebeln.
  delete adjacent duplicates from pedidos comparing ebeln.

endfunction.
