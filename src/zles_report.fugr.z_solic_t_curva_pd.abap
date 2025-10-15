function z_solic_t_curva_pd.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_OPCAO) TYPE  CHAR03 OPTIONAL
*"  EXPORTING
*"     REFERENCE(IT_RESULTADO) TYPE  STANDARD TABLE
*"  TABLES
*"      IT_NR_OV STRUCTURE  ZSDT0094 OPTIONAL
*"      IT_DATA_VENC STRUCTURE  ZSDT0094 OPTIONAL
*"      IT_DATA_LIB STRUCTURE  ZSDT0094 OPTIONAL
*"      IT_VKORG STRUCTURE  ZSDT0051 OPTIONAL
*"----------------------------------------------------------------------
  types: begin of ty_saida,
           data_registro           type zsdt0094-data_registro,
           hora_registro           type zsdt0094-hora_registro,
           matnr                   type mara-matnr,
           maktx                   type makt-maktx,
           tpsim                   type zsdt0040-tpsim,
           tp_venda                type zsdt0051-tp_venda,
           zterm                   type zsdt0052-zterm,
           data_progr              type zsdt0055-data_progr,
           dtde_logist             type zsdt0051-dtde_logist,
           formula                 type kurrf,
           dmbtr                   type zsdt0053-dmbtr,
           pmein                   type zsdt0053-pmein,
           programa                type zsdt0094-programa,
           nro_sol_ov              type zsdt0094-nro_sol_ov,
           data_venc               type zsdt0094-data_venc,
           data_lib                type zsdt0094-data_lib,
           inco1                   type zsdt0051-inco1,
           cadencia_qte            type zsdt0094-cadencia_qte,
           zieme                   type zsdt0094-zieme,
           total_proporc           type zsdt0094-total_proporc,
           total_proporc_usd       type zsdt0094-total_proporc,
           total_proporc_usd_curva type zsdt0094-total_proporc,
           taxa_curva              type zsdt0094-taxa_curva,
           frete_cif               type zsdt0094-total_proporc,
           frete_porto             type zsdt0094-total_proporc,
           tipo                    type zsdt0094-tipo,
           tipo_taxa               type zsdt0094-tipo_taxa,
           fixacao                 type zsdt0094-fixacao,
           bezei                   type zsdt0094-bezei,
           charg                   type zsdt0053-charg,
           waerk                   type zsdt0051-waerk,
           vbeln                   type zsdt0094-vbeln,
           vkorg                   type vkorg,
           kunnr                   type kunnr,
           cli_forn                type name1,
           ktokd                   type ktokd,
           intercompany            type zsdt0094-intercompany,
         end of ty_saida.

  data: gt_zsdt0094  type table of zsdt0094,
        gw_zsdt0094  type zsdt0094,
        gt_vkorg     type table of zsdt0051,
        gw_vkorg     type zsdt0051,
        gt_mara      type table of mara,
        gw_mara      type mara,
        gt_makt      type table of makt,
        gw_makt      type makt,
        gt_saida     type table of ty_saida,
        gw_saida     type ty_saida,
        gw_nro_sol   type zsdt0094,
        gw_data_venc type zsdt0094,
        gw_data_lib  type zsdt0094.

  data: var_taxa type kurrf.
  data: var_len type i.
  data: var_id      type cind,
        var_true    type c,
        var_feriado type c.

  data: gobj_zcl_webservice_tx_curva type ref to zcl_webservice_tx_curva.

  ranges r_nro_sol    for zsdt0094-nro_sol_ov.
  ranges r_data_venc  for zsdt0094-data_venc.
  ranges r_data_lib   for zsdt0094-data_lib.
  ranges r_vkorg      for zsdt0051-vkorg.
  ranges r_bezei      for zsdt0059-bezei.

  var_linhas = lines( it_nr_ov ).
  if var_linhas eq 1.
    clear: gw_nro_sol.
    read table it_nr_ov into gw_nro_sol index 1.
    r_nro_sol-sign   = 'I'.
    r_nro_sol-option = 'EQ'.
    r_nro_sol-low    = gw_nro_sol-nro_sol_ov.
    r_nro_sol-high   = gw_nro_sol-nro_sol_ov.
    append r_nro_sol.
  elseif var_linhas > 1.
    loop at it_nr_ov into gw_nro_sol.
      r_nro_sol-sign   = 'I'.
      r_nro_sol-option = 'EQ'.
      r_nro_sol-low    = gw_nro_sol-nro_sol_ov.
      append r_nro_sol.
    endloop.
  endif.

  clear: var_linhas.
  describe table it_data_venc lines var_linhas.
  case var_linhas.
    when: '1'.
      clear: gw_data_venc.
      read table it_data_venc into gw_data_venc index 1.
      r_data_venc-sign   = 'I'.
      r_data_venc-option = 'EQ'.
      r_data_venc-low    = gw_data_venc-data_venc.
      r_data_venc-high   = gw_data_venc-data_venc.
      append r_data_venc.

    when: '2'.
      clear: gw_data_venc.
      read table it_data_venc into gw_data_venc index 1.
      r_data_venc-sign   = 'I'.
      r_data_venc-option = 'BT'.
      r_data_venc-low    = gw_data_venc-data_venc.
      read table it_data_venc into gw_data_venc index 2.
      r_data_venc-high    = gw_data_venc-data_venc.
      append r_data_venc.
  endcase.


  clear: var_linhas.
  describe table it_data_lib lines var_linhas.
  case var_linhas.
    when: '1'.
      clear: gw_data_lib.
      read table it_data_lib into gw_data_lib index 1.
      r_data_lib-sign   = 'I'.
      r_data_lib-option = 'EQ'.
      r_data_lib-low    = gw_data_lib-data_lib.
      r_data_lib-high   = gw_data_lib-data_lib.
      append r_data_lib.

    when: '2'.
      clear: gw_data_lib.
      read table it_data_lib into gw_data_lib index 1.
      r_data_lib-sign   = 'I'.
      r_data_lib-option = 'BT'.
      r_data_lib-low    = gw_data_lib-data_lib.
      read table it_data_lib into gw_data_lib index 2.
      r_data_lib-high    = gw_data_lib-data_lib.
      append r_data_lib.
  endcase.


  clear: var_linhas.
  var_linhas = lines( it_vkorg ).

  if var_linhas eq 1.
    clear: gw_vkorg.
    read table it_vkorg into gw_vkorg index 1.
    r_vkorg-sign   = 'I'.
    r_vkorg-option = 'EQ'.
    r_vkorg-low    = gw_vkorg-vkorg.
    r_vkorg-high   = gw_vkorg-vkorg.
    append r_vkorg.
  elseif var_linhas > 1.

    loop at it_vkorg into gw_vkorg.
      r_vkorg-sign   = 'I'.
      r_vkorg-option = 'EQ'.
      r_vkorg-low    = gw_vkorg-vkorg.
      append r_vkorg.
    endloop.

  endif.

  "Atualizar as Taxas que foram feitas antes das 08:00 e que estão com a taxa curva com o valor 0,01. (INICIO)
  select * from zsdt0094
    into table gt_zsdt0094
   where taxa_curva  eq '0.01000'
      or taxa_cambio eq '0.01000'.

  select * from zsdt0094
      appending table gt_zsdt0094
     where taxa_curva  eq space.

  if ( sy-subrc eq 0 ).

    loop at gt_zsdt0094 into gw_zsdt0094.

      free: gobj_zcl_webservice_tx_curva.
      create object gobj_zcl_webservice_tx_curva.

      if ( gw_zsdt0094-taxa_curva eq '0.01000' ) or ( gw_zsdt0094-taxa_curva eq '0.00000' ) or ( gw_zsdt0094-taxa_curva is initial ).

        "Regara para validar data de finais de semana e feriados.
        "Recuperar a taxa curva com a data util.
        while var_true is initial.

          call function 'DATE_COMPUTE_DAY'
            exporting
              date = gw_zsdt0094-data_lib
            importing
              day  = var_id.

          case var_id.
            when: 6.

              add 2 to gw_zsdt0094-data_lib.

              call function 'HOLIDAY_CHECK_AND_GET_INFO'
                exporting
                  date                         = gw_zsdt0094-data_lib
                  holiday_calendar_id          = 'BR'
                importing
                  holiday_found                = var_feriado
                exceptions
                  calendar_buffer_not_loadable = 1
                  date_after_range             = 2
                  date_before_range            = 3
                  date_invalid                 = 4
                  holiday_calendar_id_missing  = 5
                  holiday_calendar_not_found   = 6
                  others                       = 7.

              if not var_feriado is initial.
                continue.
              else.
                var_true = 'X'.
              endif.

            when: 7.

              add 1 to gw_zsdt0094-data_lib.

              call function 'HOLIDAY_CHECK_AND_GET_INFO'
                exporting
                  date                         = gw_zsdt0094-data_lib
                  holiday_calendar_id          = 'BR'
                importing
                  holiday_found                = var_feriado
                exceptions
                  calendar_buffer_not_loadable = 1
                  date_after_range             = 2
                  date_before_range            = 3
                  date_invalid                 = 4
                  holiday_calendar_id_missing  = 5
                  holiday_calendar_not_found   = 6
                  others                       = 7.

              if not var_feriado is initial.
                continue.
              else.
                var_true = 'X'.
              endif.

            when others.

              call function 'HOLIDAY_CHECK_AND_GET_INFO'
                exporting
                  date                         = gw_zsdt0094-data_lib
                  holiday_calendar_id          = 'BR'
                importing
                  holiday_found                = var_feriado
                exceptions
                  calendar_buffer_not_loadable = 1
                  date_after_range             = 2
                  date_before_range            = 3
                  date_invalid                 = 4
                  holiday_calendar_id_missing  = 5
                  holiday_calendar_not_found   = 6
                  others                       = 7.

              if not var_feriado is initial.
                add 1 to gw_zsdt0094-data_lib.
                continue.
              else.
                var_true = 'X'.
              endif.
          endcase.
        endwhile.

        var_taxa = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = gw_zsdt0094-data_venc
                                                              i_data_lib = gw_zsdt0094-data_lib ).

        if ( var_taxa ne '0.01000' ) or ( var_taxa eq '0.00000') or ( var_taxa is initial ).

          update zsdt0094 set taxa_curva = var_taxa
                     where data_registro = gw_zsdt0094-data_registro
                       and hora_registro = gw_zsdt0094-hora_registro
                       and programa      = gw_zsdt0094-programa
                       and nro_sol_ov    = gw_zsdt0094-nro_sol_ov.


          commit work.

          if ( gw_zsdt0094-taxa_cambio eq '0.01000' ) or ( var_taxa eq '0.00000').

            update zsdt0094 set taxa_cambio = var_taxa
                       where data_registro = gw_zsdt0094-data_registro
                         and hora_registro = gw_zsdt0094-hora_registro
                         and programa      = gw_zsdt0094-programa
                         and nro_sol_ov    = gw_zsdt0094-nro_sol_ov.

            commit work.

          endif.

        endif.

      else.

        if ( gw_zsdt0094-taxa_cambio eq '0.01000' ) or ( var_taxa eq '0.00000').

          update zsdt0094 set taxa_cambio = gw_zsdt0094-taxa_curva
                     where data_registro = gw_zsdt0094-data_registro
                       and hora_registro = gw_zsdt0094-hora_registro
                       and programa      = gw_zsdt0094-programa
                       and nro_sol_ov    = gw_zsdt0094-nro_sol_ov.

          commit work.

        endif.

      endif.

      clear: gw_zsdt0094, var_taxa, var_true.
    endloop.
  endif.

  refresh: gt_zsdt0094[].
  "Atualizar as Taxas que foram feitas antes das 08:00 e que estão com a taxa curva com o valor 0,01. (FIM)

  select * from zsdt0094
    into table gt_zsdt0094
  where nro_sol_ov in r_nro_sol
    and data_venc  in r_data_venc
    and data_lib   in r_data_lib
    and tipo       eq i_opcao.

  check not gt_zsdt0094[] is initial.

  select * from ekko
    into table @data(gt_ekko)
    for all entries in @gt_zsdt0094
  where ebeln eq @gt_zsdt0094-nro_sol_ov.

  select * from ekpo
    into table @data(gt_ekpo)
    for all entries in @gt_zsdt0094
  where ebeln eq @gt_zsdt0094-nro_sol_ov
    and bukrs in @r_vkorg.

  sort: gt_zsdt0094 by data_registro hora_registro descending.

  loop at gt_zsdt0094 into gw_zsdt0094.

    try .
        data(gw_ekko) = gt_ekko[ ebeln = gw_zsdt0094-nro_sol_ov ].
      catch cx_root.
    endtry.

    try .
        data(gw_ekpo) = gt_ekpo[ ebeln = gw_zsdt0094-nro_sol_ov ].
      catch cx_root.
    endtry.

    if not ( r_vkorg is initial ) and ( gw_ekpo-bukrs not in r_vkorg  ).
      continue.
    endif.

    gw_saida-vkorg           = gw_ekko-bukrs.
    gw_saida-tpsim           = gw_ekko-bsart.
    gw_saida-zterm           = gw_ekko-zterm.

    gw_saida-inco1           = gw_zsdt0094-inco1.
    gw_saida-formula         = gw_zsdt0094-taxa_cambio  .
    gw_saida-data_registro   = gw_zsdt0094-data_registro.
    gw_saida-hora_registro   = gw_zsdt0094-hora_registro.
    gw_saida-programa        = gw_zsdt0094-programa     .
    gw_saida-nro_sol_ov      = gw_zsdt0094-nro_sol_ov   .
    gw_saida-data_venc       = gw_zsdt0094-data_venc    .
    gw_saida-data_lib        = gw_zsdt0094-data_lib     .
    gw_saida-cadencia_qte    = gw_zsdt0094-cadencia_qte .
    gw_saida-zieme           = gw_zsdt0094-zieme        .
    gw_saida-total_proporc   = gw_zsdt0094-total_proporc.

    try.
        gw_saida-total_proporc_usd       = gw_zsdt0094-total_proporc / gw_zsdt0094-taxa_cambio.
        gw_saida-total_proporc_usd_curva = gw_zsdt0094-total_proporc / gw_zsdt0094-taxa_curva.
      catch cx_sy_zerodivide.
    endtry.

    gw_saida-taxa_curva      = gw_zsdt0094-taxa_curva   .
    gw_saida-frete_cif       = gw_zsdt0094-frete_cif    .
    gw_saida-frete_porto     = gw_zsdt0094-frete_porto  .
    gw_saida-tipo            = gw_zsdt0094-tipo         .



    case gw_ekpo-matkl.
      when '658445'.
        gw_saida-maktx       = 'DEFENSIVOS'.
      when '700150' or '658440'.
        gw_saida-maktx       = 'FERTILIZANTES'.
      when '700240'.
        gw_saida-maktx       = 'SEMESTE DE MILHO'.
      when '700230'.
        gw_saida-maktx       = 'SEMESTE DE SOJA'.
      when '700130'.
        gw_saida-maktx       = 'SEMESTES'.
    endcase.

    gw_saida-dmbtr           = ''.
    gw_saida-pmein           = ''.
    gw_saida-dtde_logist     = ''.
    gw_saida-charg           = ''.
    gw_saida-tp_venda        = ''.
    gw_saida-data_progr      = ''.

    gw_saida-tipo_taxa = gw_zsdt0094-tipo_taxa.
    gw_saida-fixacao   = gw_zsdt0094-fixacao.
    gw_saida-bezei     = gw_zsdt0094-bezei.

    append gw_saida to gt_saida .

    clear: gw_saida, gw_ekpo, gw_ekko.

  endloop.

  it_resultado[] = gt_saida[].


endfunction.
