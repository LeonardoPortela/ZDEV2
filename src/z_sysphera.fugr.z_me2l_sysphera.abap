function z_me2l_sysphera.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ANO) TYPE  GJAHR OPTIONAL
*"  TABLES
*"      RESULTADO STRUCTURE  ZSYS_ME2L
*"----------------------------------------------------------------------
  data: lr_ebdat         type range of ebdat,
        lr_selpa         type range of selpa,
        lr_knttp         type range of knttp,
        lt_resultado_aux type table of zsys_me2l,
        w_resultado      like line of resultado,
        lv_gdatu         type scurr-gdatu,
        lv_datat         type char8,
* Inicio - RRIBEIRO - 2000052093 - 04/09/2025 - Stefanini
        ls_ekko TYPE ekko,
        lv_rate TYPE ukurs,
        lv_valor_pedido TYPE ekpo-netwr, " ou o mesmo tipo de WTLIEF
        lv_date TYPE sydatum,
        lv_local_curr TYPE waers.
* Fim - RRIBEIRO - 2000052093 - 04/09/2025 - Stefanini

  if i_ano is initial.
    i_ano = sy-datum(4).
  endif.

  data(lv_ano_inicio) = i_ano - 5.
  data(lv_ano_fim)    = i_ano + 5.

  lr_selpa = value #( sign = 'I' option = 'EQ' ( low  = 'WE101' ) ).
  lr_knttp = value #( sign = 'I' option = 'EQ' ( low  = 'A' ) ).
  lr_ebdat = value #( sign = 'I' option = 'BT' ( low  = |{ lv_ano_inicio }0101|
                                                 high = |{ lv_ano_fim }1231| ) ).

  cl_salv_bs_runtime_info=>set(
    exporting display  = abap_false
              metadata = abap_false
              data     = abap_true ).


  submit rm06el00 with s_bedat in lr_ebdat
                  with s_knttp in lr_knttp
                  with listu   eq 'ALV'
                  with selpa   in lr_selpa
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
    if w_resultado-wtlief eq 0 and
       w_resultado-wolief eq 0.
      continue.
    endif.
    assign component 'EBELN'       of structure <ls_data> to field-symbol(<fs_ebeln>).
    assign component 'EBELP'       of structure <ls_data> to field-symbol(<fs_ebelp>).
    assign component 'VENDOR_NAME' of structure <ls_data> to field-symbol(<fs_vendor_name>).
    assign component 'WERKS'       of structure <ls_data> to field-symbol(<fs_werks>).

    select single bukrs into @data(lv_bukrs) from j_1bbranch
      where branch eq @<fs_werks>.

    select single anln1 into w_resultado-id_imobilizado
      from ekkn
      where ebeln eq <fs_ebeln>
        and ebelp eq <fs_ebelp>.

*    "BUG 165804
    if w_resultado-id_imobilizado is not initial.
      select single txt50 into w_resultado-ds_imobilizado
           from anla
            where bukrs eq lv_bukrs
           and    anln1 eq w_resultado-id_imobilizado.

      if w_resultado-bedat le '20241231'.
        select single posnr into w_resultado-id_solicitacao
          from imaka
          where anln1 eq w_resultado-id_imobilizado
            and bukrs eq lv_bukrs.
      else.
        select single leanz  into @data(_leanz)
        from anla
         where bukrs eq @lv_bukrs
         and   anln1 eq @w_resultado-id_imobilizado.
        w_resultado-id_solicitacao = _leanz.
      endif.
    endif.

    w_resultado-nome_forn = <fs_vendor_name>+11.
    w_resultado-lifnr     = <fs_vendor_name>(10).

    concatenate w_resultado-bedat+6(2) w_resultado-bedat+4(2) w_resultado-bedat(4) into lv_datat.

    call function 'CONVERSION_EXIT_INVDT_INPUT'
      exporting
        input  = lv_datat
      importing
        output = lv_gdatu.

* Inicio - RRIBEIRO - 2000052093 - 04/09/2025 - Stefanini

    SELECT SINGLE ebeln,
                  aedat,
                  waers
      FROM ekko
      INTO CORRESPONDING FIELDS OF @ls_ekko
      WHERE ebeln = @<fs_ebeln>.

      IF sy-subrc = 0.

        lv_valor_pedido = w_resultado-wtlief. " valor original
        lv_date         = ls_ekko-aedat.

        " Definir moeda local inversa
        IF ls_ekko-waers = 'BRL'.
          lv_local_curr = 'USD'.
        ELSEIF ls_ekko-waers = 'USD'.
          lv_local_curr = 'BRL'.
        ENDIF.

        CALL FUNCTION 'READ_EXCHANGE_RATE'
          EXPORTING
            client           = sy-mandt
            date             = lv_date
            foreign_currency = ls_ekko-waers
            local_currency   = lv_local_curr
            type_of_rate     = 'M'
          IMPORTING
            exchange_rate    = lv_rate
          EXCEPTIONS
            no_rate_found    = 1
            OTHERS           = 2.

        SHIFT lv_rate-kname LEFT DELETING LEADING space.

        IF lv_rate-kname < 0.
          lv_rate-kname = lv_rate-kname * -1.
        ENDIF.

        IF sy-subrc = 0 AND lv_rate-kname > 0.

          IF ls_ekko-waers = 'BRL'.
            " BRL -> USD
            w_resultado-wtlief = lv_valor_pedido.
            w_resultado-wolief = lv_valor_pedido / lv_rate-kname.
          ELSE.
            " USD -> BRL
            w_resultado-wolief = lv_valor_pedido.
            w_resultado-wtlief = lv_valor_pedido * lv_rate-kname.
          ENDIF.
        ENDIF.

      ENDIF.

*    select single *
*        from tcurr
*        into @data(wl_tcurr_usd)
*        where     kurst eq 'B'
*              and fcurr eq 'USD'
*              and tcurr eq 'BRL'
*              and gdatu eq @lv_gdatu.
*
*    w_resultado-wolief = w_resultado-wtlief * wl_tcurr_usd-ukurs.

* FIM - RRIBEIRO - 2000052093 - 04/09/2025 - Stefanini

    append w_resultado to resultado.

* Inicio - RRIBEIRO - 2000052093 - 04/09/2025 - Stefanini

    clear: w_resultado,lv_bukrs,<fs_ebeln>,<fs_ebelp>.
*    clear: w_resultado,lv_bukrs,wl_tcurr_usd,<fs_ebeln>,<fs_ebelp>.

* Fim - RRIBEIRO - 2000052093 - 04/09/2025 - Stefanini

  endloop.

endfunction.
