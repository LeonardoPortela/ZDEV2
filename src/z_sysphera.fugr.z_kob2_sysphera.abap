function z_kob2_sysphera.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_AREA) TYPE  KOKRS DEFAULT 'MAGI'
*"     VALUE(IT_ORDENS) TYPE  ZTTSYS_AUFNR
*"  TABLES
*"      RESULTADO STRUCTURE  ZSYS_KOB2
*"----------------------------------------------------------------------

  data: lr_budat    type range of budat,
        w_resultado like line of resultado,
        lr_aufnr    type range of aufnr.

  lr_budat = value #( sign = 'I' option = 'BT' ( low = '20100101' high = '20991231' ) ).
  lr_aufnr = value #( for ls_ordens in it_ordens ( sign = 'I' option = 'EQ' low = ls_ordens ) ).

  cl_salv_bs_runtime_info=>set(
    exporting
      display  = abap_false
      metadata = abap_false
      data     = abap_true ).

  submit rkaep000 with p_tcode  eq 'KOB2'
                  with p_kokrs  eq i_area
                  with p_maxsel eq 999999999
                  with p_open   eq 'X'
                  with aufnr    in lr_aufnr
                  with r_obdat  in lr_budat
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

    assign component 'LIFNR' of structure <ls_data> to field-symbol(<fs_lifnr>).
    if <fs_lifnr> is assigned.
      select single lifnr name1 from lfa1 into ( w_resultado-lifnr, w_resultado-nome_forn )
        where lifnr eq <fs_lifnr>.
    endif.
    append w_resultado to resultado.
    clear <fs_lifnr>.
  endloop.

endfunction.
