function z_solic_t_curva_aq.
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
           waerk                   type zsdt0040-waerk,
           vbeln                   type zsdt0094-vbeln,
           vkorg                   type vkorg,
           kunnr                   type kunnr,
           cli_forn                type name1,
           ktokd                   type ktokd,
           intercompany            type zsdt0094-intercompany,
         end of ty_saida.

  types: 	begin of ty_0094,
            data_registro type zsdt0094-data_registro,
            hora_registro type zsdt0094-hora_registro,
            programa      type zsdt0094-programa,
            nro_sol_ov    type zsdt0094-nro_sol_ov,
            fixacao       type zsdt0094-fixacao,
            data_venc     type zsdt0094-data_venc,
            data_lib      type zsdt0094-data_lib,
            cadencia_qte  type zsdt0094-cadencia_qte,
            zieme         type zsdt0094-zieme,
            total_proporc type zsdt0094-total_proporc,
            taxa_curva    type zsdt0094-taxa_curva,
            tipo_taxa     type zsdt0094-tipo_taxa,
            taxa_cambio   type zsdt0094-taxa_cambio,
            frete_cif     type zsdt0094-frete_cif,
            frete_porto   type zsdt0094-frete_porto,
            tipo          type zsdt0094-tipo,
            bezei         type zsdt0094-bezei,
            estorno       type zsdt0094-estorno,
            edicao        type zsdt0094-edicao,
            vbeln         type zsdt0094-vbeln,
            inco1         type zsdt0094-inco1,
            safra         type zsdt0094-safra,
            intercompany  type zsdt0094-intercompany,
            bukrs         type t001-bukrs,
          end of ty_0094.

  data: gt_zsdt0094  type table of zsdt0094,
        gt_0094      type table of ty_0094,
        gw_zsdt0094  type zsdt0094,
        gw_0094      type ty_0094,
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

  free: gt_zsdt0094, gt_0094.
  "Atualizar as Taxas que foram feitas antes das 08:00 e que estão com a taxa curva com o valor 0,01. (FIM)

*  SELECT *
*    FROM ZSDT0094 AS A
*    INNER JOIN VBRK AS B ON B~VBELN EQ A~NRO_SOL_OV
*    INTO CORRESPONDING FIELDS OF TABLE GT_ZSDT0094
*    WHERE A~NRO_SOL_OV IN R_NRO_SOL
*      AND A~DATA_VENC  IN R_DATA_VENC
*      AND A~DATA_LIB   IN R_DATA_LIB
*      AND A~TIPO       EQ I_OPCAO
*      AND B~VKORG      IN R_VKORG.

  if i_opcao eq 'AQV'.

    select *
     from zsdt0094
*    INTO CORRESPONDING FIELDS OF TABLE GT_ZSDT0094
      into corresponding fields of table gt_0094
     where nro_sol_ov in r_nro_sol
       and data_venc  in r_data_venc
       and data_lib   in r_data_lib
       and tipo       in ('AQV', 'TBO', 'SPT')
       and bezei      in r_vkorg.
  else.
    select *
      from zsdt0094
*    INTO CORRESPONDING FIELDS OF TABLE GT_ZSDT0094
       into corresponding fields of table gt_0094
      where nro_sol_ov in r_nro_sol
        and data_venc  in r_data_venc
        and data_lib   in r_data_lib
        and tipo       eq i_opcao
        and bezei      in r_vkorg.

  endif.

  loop at gt_0094 into gw_0094.
    gw_0094-bukrs = gw_0094-bezei.
    modify gt_0094 from gw_0094 index sy-tabix.
  endloop.

  check not gt_0094 is initial.

  select *
    from vbrk
     into table @data(it_vbrk)
      for all entries in @gt_0094
       where vbeln eq @gt_0094-nro_sol_ov.

  if not it_vbrk is initial.

    select *
     from t001
      into table @data(it_t001)
       for all entries in @gt_0094
       where bukrs eq @gt_0094-bukrs.

    select *
     from kna1
      into table @data(it_kna1)
       for all entries in @it_vbrk
       where kunnr eq @it_vbrk-kunag.

    select *
     from vbrp
      into table @data(it_vbrp)
       for all entries in @it_vbrk
       where vbeln eq @it_vbrk-vbeln.

    if it_vbrp is not initial.

      select *
        from makt
         into table @data(it_makt)
          for all entries in @it_vbrp
          where matnr eq @it_vbrp-matnr
            and spras eq @sy-langu.
    endif.
  endif.

  sort: gt_0094 by data_registro hora_registro descending.

  loop at gt_0094 into gw_0094.

    read table it_vbrk into data(wa_vbrk) with key vbeln = gw_0094-nro_sol_ov.
    if sy-subrc is initial.
      read table it_vbrp into data(wa_vbrp) with key vbeln = wa_vbrk-vbeln.
      if sy-subrc is initial.
        read table it_makt into data(wa_makt) with key matnr = wa_vbrp-matnr.
        gw_saida-matnr = wa_makt-matnr.
        gw_saida-maktx = wa_makt-maktx.
      endif.
    endif.

    move:
    gw_0094-data_registro to gw_saida-data_registro,
    gw_0094-hora_registro to gw_saida-hora_registro,
    wa_vbrk-zterm         to gw_saida-zterm,
    gw_0094-taxa_cambio   to gw_saida-formula,
    gw_0094-programa      to gw_saida-programa,
    gw_0094-nro_sol_ov    to gw_saida-nro_sol_ov,
    gw_0094-data_venc     to gw_saida-data_venc,
    gw_0094-data_lib      to gw_saida-data_lib,
    gw_0094-inco1         to gw_saida-inco1,
    gw_0094-cadencia_qte  to gw_saida-cadencia_qte,
    gw_0094-zieme         to gw_saida-zieme,
    gw_0094-total_proporc to gw_saida-total_proporc,
    gw_0094-taxa_curva    to gw_saida-taxa_curva,
    gw_0094-frete_cif     to gw_saida-frete_cif,
    gw_0094-frete_porto   to gw_saida-frete_porto,
    gw_0094-tipo          to gw_saida-tipo,
    gw_0094-tipo_taxa     to gw_saida-tipo_taxa,
    gw_0094-fixacao       to gw_saida-fixacao,
    gw_0094-bezei         to gw_saida-bezei,
    wa_vbrk-waerk         to gw_saida-waerk,
    gw_0094-intercompany  to gw_saida-intercompany.

*    GW_SAIDA-KUNNR = WA_VBRK-KUNAG.
    gw_saida-vkorg = gw_0094-bukrs.
    if gw_saida-vkorg is initial.
      gw_saida-vkorg = wa_vbrk-bukrs.
    endif.

    try .
        gw_saida-cli_forn = it_t001[ bukrs = gw_saida-vkorg ]-butxt.
      catch cx_sy_itab_line_not_found.
    endtry.
*    CASE GW_SAIDA-BEZEI.
*      WHEN GW_SAIDA-KUNNR.
*
*        TRY .
*            DATA(WA_KNA1) = IT_KNA1[ KUNNR = GW_SAIDA-KUNNR ].
*          CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*        ENDTRY.
*
*        GW_SAIDA-CLI_FORN = WA_KNA1-NAME1.
*        GW_SAIDA-KTOKD    = WA_KNA1-KTOKD.
*
*      WHEN GW_SAIDA-VKORG.

*        TRY .
*            GW_SAIDA-CLI_FORN = IT_T001[ BUKRS = GW_SAIDA-VKORG ]-BUTXT.
*          CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*        ENDTRY.

*    ENDCASE.

*    GW_SAIDA-TPSIM
*    GW_SAIDA-TP_VENDA
*    GW_SAIDA-DATA_PROGR
*    GW_SAIDA-DTDE_LOGIST
*    GW_SAIDA-DMBTR
*    GW_SAIDA-PMEIN

    try.
        gw_saida-total_proporc_usd       = gw_0094-total_proporc / gw_0094-taxa_cambio.
        gw_saida-total_proporc_usd_curva = gw_0094-total_proporc / gw_0094-taxa_curva.
      catch cx_sy_zerodivide.
    endtry.

*    GW_SAIDA-CHARG


    append gw_saida to gt_saida .

    clear: gw_saida, gw_0094, gw_zsdt0094, gw_mara, gw_makt.

  endloop.

  it_resultado[] = gt_saida[].


endfunction.
