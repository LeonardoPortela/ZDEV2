function z_ksb1_sysphera.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ANO) TYPE  GJAHR
*"     REFERENCE(I_MES) TYPE  MONAT
*"     REFERENCE(I_AREA) TYPE  KOKRS
*"     REFERENCE(I_OFF) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_DATAI) TYPE  SY-DATUM OPTIONAL
*"     REFERENCE(I_DATAF) TYPE  SY-DATUM OPTIONAL
*"  TABLES
*"      RESULTADO STRUCTURE  ZSYS_KSB1
*"      T_CUSTO STRUCTURE  RSPARAMS
*"----------------------------------------------------------------------


  refresh resultado.


  types: begin of ty_objnr,
           objnr_n1 type objnr_n1,
           wogbtr   type wogxxx,
           twaer    type twaer,
         end of ty_objnr.

  types: begin of ty_ksb1.
           include structure  zsys_ksb1.
  types:   wogbtr type zsys_ksb1-wtgbtr,
         end of ty_ksb1.

  data: so_data      type range of mkpf-budat,
        wa_data      like line of so_data,
        s_custo      type range of bsis-kostl,
        r_aufnr      type range of aufnr_neu,
        p_kagru      type kagru,
        wa_custo     like line of s_custo,
        it_selection type table of rsparams,
        wa_selection like line of it_selection,
        w_resultado  type zsys_ksb1,
        w_resultado2 type ty_ksb1,
        vdatai       type sy-datum,
        vdataf       type sy-datum,
        vcusto_ate   type kostl,
        v_variante   type slis_vari,
        v_objnr      type ty_objnr.

  data: lr_data            type ref to data,
        lr_data_line       type ref to data,
        lr_data_descr      type ref to cl_abap_datadescr,
        lr_data_line_descr type ref to cl_abap_datadescr.

  data: it_zim14 type table of zim14,
        wa_zim14 type zim14,
        vid      type zim14-id.

  cl_salv_bs_runtime_info=>set(
    exporting display  = abap_false
              metadata = abap_false
              data     = abap_true ).


  field-symbols: <lt_data>      type any table,
                 <lt_data_line> type any table,
                 <ls_data>      type any,
                 <ls_data_line> type any.

  if i_datai is not initial.
    vdatai = i_datai.
    vdataf = i_dataf.
  else.
    vdatai = |{ i_ano }{ i_mes }01|.
    call function 'RP_LAST_DAY_OF_MONTHS'
      exporting
        day_in            = vdatai
      importing
        last_day_of_month = vdataf.
  endif.

  wa_data-sign = 'I'.
  wa_data-option = 'BT'.
  wa_data-low = vdatai.
  wa_data-high = vdataf.
  append wa_data  to so_data.


  loop at t_custo into data(w_custo) where selname = 'KOSTL'.
    wa_custo-sign   = w_custo-sign.
    wa_custo-option = w_custo-option.
    wa_custo-low    =  w_custo-low.
    wa_custo-high   =  w_custo-high.
    append wa_custo  to s_custo.
  endloop.

  if i_off = 'X'.

    p_kagru = 'ORCAMENTO'.

    submit rkaep000   with p_tcode  eq 'KSB1'
                      with p_kokrs  eq i_area
                      with kostl    in s_custo
                      with r_budat  in so_data
                      with aufnr    in r_aufnr
                      "WITH KAGRU    EQ P_KAGRU
                      with koagr    eq p_kagru
                      with p_maxsel eq 999999
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

    cl_salv_bs_runtime_info=>clear_all( ).
    if lr_data_descr is initial.
      exit.
    endif.
    assign lr_data->* to <ls_data>.

    loop at <lt_data> assigning <ls_data>.

      move-corresponding <ls_data> to w_resultado.
      move-corresponding <ls_data> to w_resultado2.
      if i_area = 'MGPY'.
        multiply w_resultado2-wogbtr by 100.
      endif.
      w_resultado-wtgbtr = w_resultado2-wogbtr.
      move-corresponding <ls_data> to v_objnr.

      if  w_resultado-refbt ne 'R'.
        continue.
      endif.

      if w_resultado-wrttp ne '04'.
        continue.
      endif.

      if v_objnr-twaer eq 'USD'.
        if i_area = 'MGPY'   .
          multiply v_objnr-wogbtr by 100.
        endif.
        w_resultado-wtgbtr = v_objnr-wogbtr.
      endif.

      if v_objnr-objnr_n1+0(3) = 'ORD'.
        w_resultado-aufnr = v_objnr-objnr_n1+4(12).
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = w_resultado-aufnr
          importing
            output = w_resultado-aufnr.
        select single auart
          into @data(v_auart)
          from aufk
          where aufnr = @w_resultado-aufnr.
        if v_auart ne 'ZSTA' and
           v_auart ne 'ZSIN'.
          continue.
        endif.
      else.
*      CONTINUE.
      endif.

      select single ltext from cskt into w_resultado-nome_centro
           where spras eq sy-langu and
                 kokrs eq i_area  and
                 kostl eq w_resultado-kostl and
                 datbi ge sy-datum .

      select single maktx from makt into w_resultado-maktx
        where spras = sy-langu
        and   matnr = w_resultado-matnr.

      select single lifnr into w_resultado-lifnr
        from coep
        inner join ekko
        on ekko~ebeln = coep~ebeln
        where coep~kokrs eq i_area
        and   coep~belnr eq w_resultado-belnr
        and   coep~ebeln ne ''.

      if sy-subrc = 0.
        select single name1 into w_resultado-name1
          from lfa1
          where lifnr = w_resultado-lifnr.
      endif.
      append w_resultado to resultado.
      clear w_resultado.
    endloop.

    free: <lt_data>.
    free: lr_data.
    select max( id ) as id
      into vid
      from zim14.
    if vid is initial.
      vid = 0.
    else.
      add 1 to vid.
    endif.

    loop at resultado into w_resultado.
      add 1 to vid.
      move-corresponding w_resultado to wa_zim14.
      wa_zim14-id = vid.
      wa_zim14-ano = i_ano.
      wa_zim14-mes = i_mes.
      wa_zim14-area = i_area.
      append wa_zim14 to it_zim14.
    endloop.
*    DELETE FROM zim14 WHERE area  = i_area
*                      AND   ano   = i_ano
*                      AND   mes   = i_mes.
    "
    modify  zim14 from table it_zim14.
    commit work.
  else.
    select *
      from zim14
      into table it_zim14
      where area  = i_area
      and   ano   = i_ano
      and   mes   = i_mes.
    loop at it_zim14 into wa_zim14.
      move-corresponding wa_zim14 to w_resultado.
      append w_resultado to resultado.

    endloop.
  endif.


endfunction.
