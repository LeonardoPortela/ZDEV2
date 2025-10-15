*&---------------------------------------------------------------------*
*&  Include           ZFIR064_FORM
*&---------------------------------------------------------------------*

form f_refresh_alv using p_alv.

  case p_alv.
    when '0100'.
      call method obj_alv_0100->refresh_table_display
        exporting
          is_stable = wa_stable.
    when '0110'.

  endcase.

endform.

form f_refresh_objetos .

  clear: gs_layout,
         gs_variant.

  refresh: it_exclude_fcode.

endform.

form f_criar_catalog using p_screen.

  free: wa_fcat, it_fcat.

  case p_screen.
    when '0100'.

      perform f_estrutura_alv using:

       01  'MARA'          'MATNR'              'IT_SAIDA_0100'  'MATNR'               'Material'               '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'T001L'         'WERKS'              'IT_SAIDA_0100'  'WERKS'               'Centro'                 '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       03  'T001L'         'LGORT'              'IT_SAIDA_0100'  'LGORT'               'Depósito'               '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       04  'MCHB'          'CHARG'              'IT_SAIDA_0100'  'CHARG'               'Lote'                   '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       05  'MARD'          'LABST'              'IT_SAIDA_0100'  'SLD_E'               'Saldo Estoque'          '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       06  'MARD'          'LABST'              'IT_SAIDA_0100'  'SALDO_NF'            'Saldo Fiscal'           '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       07  'MARD'          'LABST'              'IT_SAIDA_0100'  'SLD_D'               'Diferença'              '13'   ' '    ''  ' ' ' ' ' ' ' ' '' .

    when '0110'.


  endcase.

endform.

form f_estrutura_alv using value(p_col_pos)       type i
                           value(p_ref_tabname)   like dd02d-tabname
                           value(p_ref_fieldname) like dd03d-fieldname
                           value(p_tabname)       like dd02d-tabname
                           value(p_field)         like dd03d-fieldname
                           value(p_scrtext_l)     like dd03p-scrtext_l
                           value(p_outputlen)
                           value(p_edit)
                           value(p_sum)
                           value(p_emphasize)
                           value(p_just)
                           value(p_hotspot)
                           value(p_f4)
                           value(p_check).

  clear wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = ' '.
  wa_fcat-do_sum      = p_sum.
  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-f4availabl  = p_f4.
  wa_fcat-checkbox    = p_check.

  append wa_fcat to it_fcat.

endform.                    " ESTRUTURA_ALV

form f_exclude_fcode using p_screen.

  append cl_gui_alv_grid=>mc_fc_refresh           to it_exclude_fcode.
  append cl_gui_alv_grid=>mc_fc_loc_delete_row    to it_exclude_fcode.
  append cl_gui_alv_grid=>mc_fc_loc_insert_row    to it_exclude_fcode.
  append cl_gui_alv_grid=>mc_fc_loc_append_row    to it_exclude_fcode.
  append cl_gui_alv_grid=>mc_fc_loc_copy          to it_exclude_fcode.
  append cl_gui_alv_grid=>mc_fc_loc_copy_row      to it_exclude_fcode.
  append cl_gui_alv_grid=>mc_fc_loc_cut           to it_exclude_fcode.
  append cl_gui_alv_grid=>mc_fc_loc_undo          to it_exclude_fcode.
  append cl_gui_alv_grid=>mc_fc_loc_paste         to it_exclude_fcode.
  append cl_gui_alv_grid=>mc_fc_loc_paste_new_row to it_exclude_fcode.
  append cl_gui_alv_grid=>mc_fc_check             to it_exclude_fcode.

endform.

form f_limpa_variaveis .

  clear: wa_saida_0100,
         it_saida_0100[],
         tg_estoque_zsdt0034[],
*         tg_estoque_zsdt0163[], " USER STORY 142076 // MMSILVA - 31.10.2024
         tg_estoque_mb52[].

endform.

form f_selecionar_dados.

  data: it_rsparams type table of rsparams,
        wa_rsparams type rsparams.

  perform f_limpa_variaveis.

  free: tg_zsdt_depara_cen.
  select *
    from zsdt_depara_cen into table tg_zsdt_depara_cen
   where vkorg     eq p_bukrs
     and centro_real like p_werks.

  delete adjacent duplicates from tg_zsdt_depara_cen comparing centrov_1.

  if tg_zsdt_depara_cen[] is initial.
    message 'Depara Centro virtual não encontrado!' type 'S'.
    stop.
  endif.

  free: tg_zsdt_depara_depo.
  select *
    from zsdt_depara_depo into table tg_zsdt_depara_depo
     for all entries in tg_zsdt_depara_cen
   where werks_v eq tg_zsdt_depara_cen-centrov_1
     and werks   eq tg_zsdt_depara_cen-centro_real.



*------------------------------------------------------------------------------*
* Selecionar Dados MB52
*------------------------------------------------------------------------------*
  data: lv_werks type char4.

  lv_werks = p_werks.

  if strlen( lv_werks ) = 4.
    lv_werks = '*' && lv_werks+2(2).
  endif.

  perform f_prepare_run_time_info using abap_false.

  submit rm07mlbs with matnr = p_matnr
                  with werks cp lv_werks
                  with charg eq p_charg
                  with lgort cp 'PO*'
                  with negativ = ''
                  with xmchb = 'X'
                  with nozero = 'X'
                  with novalues = 'X'
                  with pa_flt = 'X'
                  and return.

  perform f_get_runtime_info.

  if <lt_data> is assigned.
    loop at <lt_data> assigning <ls_data>.
      move-corresponding <ls_data> to tg_estoque_mb52.
      append tg_estoque_mb52.
    endloop.
  endif.

endform.

form f_processa_dados .

  loop at tg_estoque_mb52.

    clear: wa_saida_0100.

    wa_saida_0100-matnr = tg_estoque_mb52-matnr.
    wa_saida_0100-werks = tg_estoque_mb52-werks.
    wa_saida_0100-lgort = tg_estoque_mb52-lgort.
    wa_saida_0100-charg = tg_estoque_mb52-charg.
    wa_saida_0100-sld_e = tg_estoque_mb52-labst.

* USER STORY 142076 // MMSILVA - 31.10.2024 - Inicio
*    loop at tg_estoque_zsdt0034 where matnr       = tg_estoque_mb52-matnr
*                                  and werks_v     = tg_estoque_mb52-werks
*                                  and ( ( lgort   = tg_estoque_mb52-lgort ) or
*                                        ( lgort_t = tg_estoque_mb52-lgort ) ).
*      add tg_estoque_zsdt0034-saldo to wa_saida_0100-saldo_nf.
*    endloop.

*    loop at tg_estoque_zsdt0163 where matnr       = tg_estoque_mb52-matnr
*                                  and werks_d     = tg_estoque_mb52-werks
*                                  and ( ( lgort   = tg_estoque_mb52-lgort ) or
*                                        ( lgort_d = tg_estoque_mb52-lgort ) ).
*      add tg_estoque_zsdt0163-saldo_nf to wa_saida_0100-saldo_nf.
*    endloop.
* USER STORY 142076 // MMSILVA - 31.10.2024 - Fim

*    wa_saida_0100-sld_d = wa_saida_0100-sld_e - wa_saida_0100-saldo_nf.

    append wa_saida_0100 to it_saida_0100.

  endloop.

  sort it_saida_0100.

endform.

form f_prepare_run_time_info using p_display type c.

  if <lt_data> is assigned.
    clear: <lt_data>[].
  endif.

  if <lt_data_line> is assigned.
    clear: <lt_data_line>[].
  endif.

  if <ls_data> is assigned .
    clear: <ls_data>.
  endif.

  if <ls_data_line> is assigned .
    clear: <ls_data_line>.
  endif.

  free: lr_data, lr_data_line, lr_data_descr, lr_data_line_descr.

  cl_salv_bs_runtime_info=>set( exporting display = p_display metadata = abap_false data = abap_true ).

endform.

form f_get_runtime_info.

  try.
      cl_salv_bs_runtime_info=>get_data_ref(
        importing
          r_data_descr      = lr_data_descr
          r_data_line_descr = lr_data_line_descr ).

      check ( lr_data_descr is not initial ) or ( lr_data_line_descr is not initial ).

      create data lr_data      type handle lr_data_descr.
      create data lr_data_line type handle lr_data_line_descr.

      assign lr_data->*      to <lt_data>.
      assign lr_data_line->* to <lt_data_line>.

      cl_salv_bs_runtime_info=>get_data( importing t_data      = <lt_data>
                                                   t_data_line = <lt_data_line> ).

    catch cx_salv_bs_sc_runtime_info into data(ws_reutime_info).
      if ws_reutime_info is not initial.

      endif.
  endtry.

  cl_salv_bs_runtime_info=>clear_all( ).

  assign lr_data->*      to <ls_data>.
  assign lr_data_line->* to <ls_data_line>.


endform.






* US 142076 // MMSILVA - 27.11.2024 - Inicio " INSERIDO FORMS DA ZSDR0112 PARA ENCONTRAR RESULTADOS
*------------------------------------------------------------------------------*
* Selecionar Dados ZSDT0163
*------------------------------------------------------------------------------*
form f_selecionar_dados_zsdt0163 using p_refresh_alv type char1.
  data: r_reftyp type range of j_1breftyp.
  append value #( sign = 'I' option = 'EQ' low = 'BI' ) to r_reftyp.
  append value #( sign = 'I' option = 'EQ' low = 'ZW' ) to r_reftyp.


  perform f_limpa_variaveis_zsdt0163.

  if p_refresh_alv is initial.
    clear: tg_notas_rfl[].

    if  p_dtem-high is initial.
      p_dtem-high = p_dtem-low.
    endif.


    zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->selecionar_notas(
      exporting
        i_bukrs      =  p_bukrs
        i_fkart      =  p_fkart
        i_werks      =  p_werks
        i_lgort      =  p_lgort
        i_charg      =  p_charg
        i_kunnr      =  p_kunnr
        i_matnr      =  p_matnr
        i_status_cct =  ' '
        i_dt_ini_emi =  p_dtem-low
        i_dt_fim_emi =  p_dtem-high
      importing
        e_notas    =  tg_notas_rfl ).

  endif.

  check tg_notas_rfl[] is not initial.

  loop at tg_notas_rfl into data(wl_nota_rfl).
    clear: tg_notas.

    move-corresponding wl_nota_rfl to tg_notas.
    append tg_notas.
  endloop.

  tg_notas_aux[] = tg_notas[].


  if tg_notas_aux[] is not initial.
    select *
      from zsdt0023 into table tg_zsdt0023
       for all entries in tg_notas_aux
     where vbeln eq tg_notas_aux-vbeln_vl.

    loop at tg_zsdt0023.
      if ( tg_zsdt0023-mblnr_s is not initial ) and ( tg_zsdt0023-es_mblnr_s is not initial ).
        clear: tg_zsdt0023-mblnr_s, tg_zsdt0023-es_mblnr_s.
      endif.

      if ( tg_zsdt0023-mblnr_e is not initial ) and ( tg_zsdt0023-es_mblnr_e is not initial ).
        clear: tg_zsdt0023-mblnr_e, tg_zsdt0023-es_mblnr_e.
      endif.

      modify tg_zsdt0023.
    endloop.

    tg_zsdt0023_aux[] = tg_zsdt0023[].
    delete tg_zsdt0023_aux where mblnr_e is initial.

    if tg_zsdt0023_aux[] is not initial.
      select mblnr mjahr
        from mkpf appending table tg_mkpf
         for all entries in tg_zsdt0023_aux
       where mblnr  eq tg_zsdt0023_aux-mblnr_e
         and mjahr  eq tg_zsdt0023_aux-mjahr_e.
    endif.

    refresh tg_zsdt0023_aux[].
    tg_zsdt0023_aux[] = tg_zsdt0023[].
    delete tg_zsdt0023_aux where mblnr_s is initial.

    if tg_zsdt0023_aux[] is not initial.
      select mblnr mjahr
        from mkpf appending table tg_mkpf
         for all entries in tg_zsdt0023_aux
       where mblnr  eq tg_zsdt0023_aux-mblnr_s
         and mjahr  eq tg_zsdt0023_aux-mjahr_s.

    endif.
  endif.

  refresh tg_notas_aux[].
  tg_notas_aux[] = tg_notas[].
  delete tg_notas_aux where mblnr_cce is initial.

  if tg_notas_aux[] is not initial.
    select mblnr mjahr bktxt
     from mkpf appending table tg_mkpf
       for all entries in tg_notas_aux
    where mblnr eq tg_notas_aux-mblnr_cce
      and mjahr eq tg_notas_aux-mjahr_cce.
  endif.

  select *
    from zsdt_retlote into table tg_zsdt_retlote
     for all entries in tg_notas
   where docnum eq tg_notas-docnum.

  refresh tg_notas_aux[].
  tg_notas_aux[] = tg_notas[].
  delete tg_notas_aux where ra_cct is initial.

  if tg_notas_aux[] is not initial.
    select *
      from zsdt0168 into table tg_zsdt0168
       for all entries in tg_notas_aux
     where codigo_ra eq tg_notas_aux-ra_cct.
  endif.

  if tg_zsdt_retlote[] is not initial.
    select *
      from zsdt_export into table tg_zsdt_export
       for all entries in tg_zsdt_retlote
     where docnum eq tg_zsdt_retlote-docnum_ret.

    if tg_zsdt_export[] is not initial.
      select *
        from j_1bnfdoc into table tg_doc_ret
         for all entries in tg_zsdt_export
       where docnum eq tg_zsdt_export-docnum.

      select *
        from zfiwrt0008 into table tg_zfiwrt0008
         for all entries in tg_zsdt_export
       where docnum_retorno  eq tg_zsdt_export-docnum.

      delete tg_zfiwrt0008 where ( docs_estornados eq abap_true ) or ( loekz eq abap_true ).

      select *
        from j_1bnfe_active appending table tg_active
         for all entries in tg_zsdt_export
       where docnum eq tg_zsdt_export-docnum
         and docsta eq '1'
         and cancel eq abap_false.

      if tg_zfiwrt0008[] is not initial.
        select *
          from j_1bnfe_active appending table tg_active
           for all entries in tg_zfiwrt0008
         where docnum = tg_zfiwrt0008-docnum.
      endif.
    endif.

  endif.

  if tg_mkpf[] is not initial.

    select mblnr mjahr werks lgort
      from mseg appending table tg_mseg
       for all entries in tg_mkpf
     where mblnr eq tg_mkpf-mblnr
       and mjahr eq tg_mkpf-mjahr
       and shkzg eq 'S'.

  endif.

  select * from zsdt0283 into table tg_zsdt0283
    where bukrs      eq p_bukrs.

  call function 'GET_DOMAIN_VALUES'
    exporting
      domname         = 'ZFIN_EXPORT_D'
    tables
      values_tab      = t_dd07v
    exceptions
      no_values_found = 1
      others          = 2.

  data(lt_notas) = tg_notas[].
  sort lt_notas by matnr.
  delete adjacent duplicates from lt_notas comparing matnr.

  if lt_notas[] is not initial.
    select matnr matkl
      from mara
      into table tg_mara
      for all entries in lt_notas
      where matnr = lt_notas-matnr.
    sort tg_mara by matnr.
  endif.

  "Buscar os dados de descarga.
  select * from zlest0039 into table tg_zlest0039 for all entries in tg_notas where docnum eq tg_notas-docnum.

  "Dados deposito.
  if tg_notas is not initial.
    free: t_zfiwrt0008, t_zfiwrt0009.
    select * from zfiwrt0008
    into table t_zfiwrt0008
      for all entries in tg_notas
      where docnum eq tg_notas-docnum.
    if sy-subrc eq 0.
      select * from zfiwrt0009
          into table t_zfiwrt0009
            for all entries in t_zfiwrt0009
            where seq_lcto eq t_zfiwrt0009-seq_lcto.
    endif.
  endif.

endform.



form z_busca_dados_atualizado.
  data tl_docest    type table of zfiwrs0003 with header line.
  data vl_xblnr     type mkpf-xblnr.
  data vl_bktxt     type mkpf-bktxt.
  data wl_color type kkblo_specialcol.
  data w_final type zsdt_export-finalidade.
  refresh tl_docest.
  loop at it_saida_0100_zsdt0163 into wa_saida_0100_zsdt0163.
    if wa_saida_0100_zsdt0163-seq_lcto_znfw is not initial.
      move: wa_saida_0100_zsdt0163-seq_lcto_znfw to tl_docest-seq_lcto.

      append tl_docest.
      clear: tl_docest.
    endif.
  endloop.

  sort  tl_docest by seq_lcto.

  call function 'ZNFW_ESTORNA_SEQ_LCTO'
    tables
      t_docs = tl_docest.

  loop at it_saida_0100_zsdt0163 into wa_saida_0100_zsdt0163.
    data(tabix) = sy-tabix.
    refresh: wa_saida_0100_zsdt0163-color.
    "
    wa_saida_0100_zsdt0163-docnum_ret_flag = icon_warning.
    if wa_saida_0100_zsdt0163-docnum_retorno is not initial.
      shift wa_saida_0100_zsdt0163-docnum_znfw left deleting leading '0'.
      concatenate  icon_checked wa_saida_0100_zsdt0163-docnum_retorno into wa_saida_0100_zsdt0163-docnum_ret_flag separated by ' - '.

      select single *
      from j_1bnfe_active
       into @data(wa_active)
        where docnum eq @wa_saida_0100_zsdt0163-docnum_retorno.
      if sy-subrc = 0.
        select single *
        from j_1bnfdoc
        into @data(wa_nfdoc)
        where docnum eq @wa_saida_0100_zsdt0163-docnum_retorno.

        wa_saida_0100_zsdt0163-nferet_quebra = wa_nfdoc-nfenum.

        if wa_saida_0100_zsdt0163-nferet_quebra is initial.
          wa_saida_0100_zsdt0163-nferet_flag = icon_warning.
        else.
          wa_saida_0100_zsdt0163-nferet_flag = wa_nfdoc-nfenum.
          condense wa_saida_0100_zsdt0163-nferet_flag no-gaps.
          "
          shift wa_saida_0100_zsdt0163-nferet_flag left deleting leading '0'.
          if wa_nfdoc-candat is not initial.
            concatenate  icon_storno wa_saida_0100_zsdt0163-nferet_flag into wa_saida_0100_zsdt0163-nferet_flag separated by ' - '.
          elseif wa_active-cancel = 'X'. "Cancelado SEFAZ
            concatenate  icon_cancel wa_saida_0100_zsdt0163-nferet_flag into wa_saida_0100_zsdt0163-nferet_flag separated by ' - '.
          elseif wa_active-docsta eq space or   wa_active-action_requ eq space.
            concatenate  icon_activity wa_saida_0100_zsdt0163-nferet_flag into wa_saida_0100_zsdt0163-nferet_flag separated by ' - '.
          elseif wa_active-docsta eq '1'.
            concatenate  icon_complete wa_saida_0100_zsdt0163-nferet_flag into wa_saida_0100_zsdt0163-nferet_flag separated by ' - '.
          else.
            concatenate  icon_status_critical wa_saida_0100_zsdt0163-nferet_flag into wa_saida_0100_zsdt0163-nferet_flag separated by ' - '.
          endif.
        endif.
      endif.

      w_final = wa_saida_0100_zsdt0163-finalidade+0(1).
      "Transferencia
      select single * into wg_zsdt0283 from zsdt0283  where finalidade = w_final.
      if sy-subrc eq 0 and wg_zsdt0283-transf_saldo = 'X'.
        select single *
          into @data(w_doc)
          from j_1bnfdoc where docnum = @wa_saida_0100_zsdt0163-docnum_retorno.
        vl_xblnr = conv #( wa_saida_0100_zsdt0163-nfenum ).                                       "US140390-Regra atribuição finalidade-ALRS
        concatenate wa_saida_0100_zsdt0163-vbeln_vl wa_saida_0100_zsdt0163-docnum_retorno  into vl_bktxt separated by '/'. "US140390-Regra atribuição finalidade-ALRS
        select single c~mblnr
                      c~mjahr
                      c~xblnr
                      c~budat
                      from mkpf as c
                      inner join mseg as i
                      on   i~mblnr = c~mblnr
                      and  i~mjahr = c~mjahr
                      and  i~bwart = '301'
                      into tg_mkpf_2
                      where c~xblnr eq vl_xblnr
                      and   c~bktxt eq vl_bktxt.

        if sy-subrc eq 0.
          select single mblnr
                        mjahr
                        smbln from mseg into tg_mseg_2 where smbln eq tg_mkpf_2-mblnr.
          if sy-subrc ne 0.
            wa_saida_0100_zsdt0163-mblnr = tg_mkpf_2-mblnr.
            wa_saida_0100_zsdt0163-mjahr = tg_mkpf_2-mjahr.
            wa_saida_0100_zsdt0163-budat = tg_mkpf_2-budat.
          endif.
        else.
          vl_bktxt = conv #( wa_saida_0100_zsdt0163-vbeln_vl ).        "US140390-Regra atribuição finalidade-ALRS
          select single c~mblnr
                      c~mjahr
                      c~xblnr
                      c~budat
                      from mkpf as c
                      inner join mseg as i
                      on   i~mblnr = c~mblnr
                      and  i~mjahr = c~mjahr
                      and  i~bwart = '301'
           into tg_mkpf_2
           where xblnr eq vl_xblnr                          "US140390-Regra atribuição finalidade-ALRS
           and   bktxt eq vl_bktxt.                         "US140390-Regra atribuição finalidade-ALRS

          if sy-subrc eq 0.

            select single mblnr
                          mjahr
                          smbln from mseg
            into tg_mseg_2 where smbln eq tg_mkpf_2-mblnr.
            if sy-subrc ne 0.
              wa_saida_0100_zsdt0163-mblnr = tg_mkpf_2-mblnr.
              wa_saida_0100_zsdt0163-mjahr = tg_mkpf_2-mjahr.
              wa_saida_0100_zsdt0163-budat = tg_mkpf_2-budat.
            endif.
          endif.
        endif.
        if wa_saida_0100_zsdt0163-mblnr is initial.
          wa_saida_0100_zsdt0163-mblnr_flag = icon_warning.
        else.
          shift wa_saida_0100_zsdt0163-mblnr left deleting leading '0'.
          concatenate  icon_checked wa_saida_0100_zsdt0163-mblnr into wa_saida_0100_zsdt0163-mblnr_flag separated by ' - '.
        endif.
      else.
        wa_saida_0100_zsdt0163-mblnr_flag = icon_negative.
      endif.
      "Transferencia

      "ZNFW
      select single * into wg_zsdt0283 from zsdt0283  where finalidade = w_final.
      if sy-subrc eq 0 and wg_zsdt0283-operacao_b is not initial.
        select single *
         from zfiwrt0008
         into @data(wa_zfiwrt0008)
         where  docnum_retorno = @wa_saida_0100_zsdt0163-docnum_retorno
         and    loekz = ' '.
        if sy-subrc eq 0.
          wa_saida_0100_zsdt0163-seq_lcto_znfw = wa_zfiwrt0008-seq_lcto.
          wa_saida_0100_zsdt0163-docnum_znfw   = wa_zfiwrt0008-docnum.
          wa_saida_0100_zsdt0163-docdat_znfw   = wa_zfiwrt0008-bldat.
          wa_saida_0100_zsdt0163-mblnr_znfw    = wa_zfiwrt0008-mblnr.
          if wa_saida_0100_zsdt0163-mblnr_znfw is initial.
            wa_saida_0100_zsdt0163-mblnr_znfw_flag = icon_warning.
          else.
            shift wa_saida_0100_zsdt0163-mblnr_znfw left deleting leading '0'.
            concatenate  icon_checked wa_saida_0100_zsdt0163-mblnr_znfw into wa_saida_0100_zsdt0163-mblnr_znfw_flag separated by ' - '.
          endif.

          if wa_saida_0100_zsdt0163-docnum_znfw is not initial.
            select single *
              from j_1bnfe_active
               into wa_active
                where docnum eq wa_saida_0100_zsdt0163-docnum_znfw.

            if sy-subrc eq 0.
              wa_saida_0100_zsdt0163-nfenum_znfw = wa_active-nfnum9.
              select single *
                 from j_1bnfdoc
                 into wa_nfdoc
                 where docnum eq wa_saida_0100_zsdt0163-docnum_znfw.

              if wa_saida_0100_zsdt0163-nfenum_znfw is initial.
                wa_saida_0100_zsdt0163-nfenum_flag = icon_warning.
              else.
                wa_saida_0100_zsdt0163-nfenum_flag = wa_nfdoc-nfenum.
                condense wa_saida_0100_zsdt0163-nfenum_flag no-gaps.
                "
                shift wa_saida_0100_zsdt0163-nfenum_flag left deleting leading '0'.
                if wa_nfdoc-candat is not initial.
                  concatenate  icon_storno wa_saida_0100_zsdt0163-nfenum_flag into wa_saida_0100_zsdt0163-nfenum_flag separated by ' - '.
                elseif wa_active-cancel = 'X'. "Cancelado SEFAZ
                  concatenate  icon_cancel wa_saida_0100_zsdt0163-nfenum_flag into wa_saida_0100_zsdt0163-nfenum_flag separated by ' - '.
                elseif wa_active-docsta eq space or   wa_active-action_requ eq space.
                  concatenate  icon_activity wa_saida_0100_zsdt0163-nfenum_flag into wa_saida_0100_zsdt0163-nfenum_flag separated by ' - '.
                elseif wa_active-docsta eq '1'.
                  concatenate  icon_complete wa_saida_0100_zsdt0163-nfenum_flag into wa_saida_0100_zsdt0163-nfenum_flag separated by ' - '.
                else.
                  concatenate  icon_status_critical wa_saida_0100_zsdt0163-nfenum_flag into wa_saida_0100_zsdt0163-nfenum_flag separated by ' - '.
                endif.
              endif.
              "
              clear: tl_docest,  wa_saida_0100_zsdt0163-color.

              read table tl_docest
              with key seq_lcto = wa_zfiwrt0008-seq_lcto
                  binary search.

              if ( tl_docest-docnum_est is not initial
                 and tl_docest-docnum_est ne '0000000000' ) or wa_zfiwrt0008-loekz = 'X'.
                clear: wl_color.
                wl_color-fieldname = 'DOCNUM_FLAG'.
                wl_color-color-col = 6.
                wl_color-color-inv = 6.
                append wl_color to wa_saida_0100_zsdt0163-color.

                clear: wl_color.
                wl_color-fieldname = 'SEQ_LCTO_FLAG'.
                wl_color-color-col = 6.
                wl_color-color-inv = 6.
                append wl_color to wa_saida_0100_zsdt0163-color.
              endif.
            else.
              wa_saida_0100_zsdt0163-nfenum_flag = icon_warning.
            endif.
          endif.
        endif.
        if wa_saida_0100_zsdt0163-seq_lcto_znfw is initial.
          wa_saida_0100_zsdt0163-seq_lcto_flag = icon_warning.
        else.
          shift wa_saida_0100_zsdt0163-seq_lcto_znfw left deleting leading '0'.
          concatenate  icon_checked wa_saida_0100_zsdt0163-seq_lcto_znfw into wa_saida_0100_zsdt0163-seq_lcto_flag separated by ' - '.
        endif.
        if wa_saida_0100_zsdt0163-docnum_znfw is initial.
          wa_saida_0100_zsdt0163-docnum_flag = icon_warning.
        else.
          shift wa_saida_0100_zsdt0163-docnum_znfw left deleting leading '0'.
          concatenate  icon_checked wa_saida_0100_zsdt0163-docnum_znfw into wa_saida_0100_zsdt0163-docnum_flag separated by ' - '.
        endif.
      else.
        wa_saida_0100_zsdt0163-seq_lcto_flag   = icon_negative.
        wa_saida_0100_zsdt0163-docnum_flag     = icon_negative.
        wa_saida_0100_zsdt0163-nfenum_flag     = icon_negative.
        wa_saida_0100_zsdt0163-mblnr_znfw_flag = icon_negative.
      endif.
    endif.

    perform f_set_status_registro changing wa_saida_0100_zsdt0163.

    modify it_saida_0100_zsdt0163 from wa_saida_0100_zsdt0163 index tabix.
  endloop.

  "Agrupar os valores do mesmo depósito
  loop at it_saida_0100_zsdt0163 into data(wa_saida_agrp).
    READ TABLE it_result WITH KEY lgort_d = wa_saida_agrp-lgort_d
                                  werks_d = wa_saida_agrp-werks_d  TRANSPORTING NO FIELDS. "USER STORY 165680 - MMSILVA - 05.02.2025

    if sy-subrc = 0.
      loop at it_result into data(wa_result) where lgort_d = wa_saida_agrp-lgort_d.
        wa_result-saldo_nf = wa_result-saldo_nf + wa_saida_agrp-saldo_nf.
        modify it_result from wa_result.
        exit.
      endloop.
    else.
      wa_result-lgort_d = wa_saida_agrp-lgort_d.
      wa_result-saldo_nf = wa_saida_agrp-saldo_nf.
      wa_result-werks_d = wa_saida_agrp-werks_d.
      APPEND wa_result TO it_result.
    endif.

    CLEAR: wa_saida_agrp, wa_result. "USER STORY 165680 - MMSILVA - 05.02.2025
  endloop.

  "Incluir os valores da coluna saldo_nf na it_saida_0100
  loop at it_saida_0100 assigning field-symbol(<fs_saida>).
    loop at it_result assigning field-symbol(<fs_result>).
      if ( <fs_saida>-lgort = <fs_result>-lgort_d ) AND ( <fs_saida>-werks = <fs_result>-werks_d ). "USER STORY 165680 - MMSILVA - 05.02.2025
        <fs_saida>-saldo_nf = <fs_result>-saldo_nf.
        exit.
      endif.
    endloop.
    <fs_saida>-sld_d = <fs_saida>-sld_e - <fs_saida>-saldo_nf.

    data wa_t001w type t001w.

    select single J_1BBRANCH
      from t001w
      where werks = @<fs_saida>-werks
      into corresponding fields of @wa_t001w.

    if ( wa_t001w-j_1bbranch ne p_werks ).
      DELETE it_saida_0100 WHERE werks = <fs_saida>-werks.
    elseif ( <fs_saida>-werks CP 'AX*' ). "USER STORY 165680 - MMSILVA - 05.02.2025
      DELETE it_saida_0100 WHERE werks = <fs_saida>-werks. "USER STORY 165680 - MMSILVA - 05.02.2025
    endif.
  endloop.
endform.



form f_processa_dados_zsdt0163.

  data v_domvalue   type char10.
  data: lv_erro     type c.
  data vl_xblnr     type mkpf-xblnr.
  data vl_bktxt     type mkpf-bktxt.
  data vl_qtde_vinc type zde_nota_retorno_rfl-qtde_vinc.

  loop at tg_notas into data(wl_nota).

    clear: wa_saida_0100, v_domvalue, vl_qtde_vinc, vl_xblnr, tg_zsdt0023.

    wa_saida_0100_zsdt0163-docnum       =   wl_nota-docnum.
    wa_saida_0100_zsdt0163-reftyp       =   wl_nota-reftyp.
    wa_saida_0100_zsdt0163-vbeln_vl     =   wl_nota-vbeln_vl.

    "Buscar os dados de descarga.
    read table tg_zlest0039 into data(wl_zlest0039) with key docnum = wl_nota-docnum.
    if sy-subrc eq 0.
      wa_saida_0100_zsdt0163-datatransb =  wl_zlest0039-datatransb.
      wa_saida_0100_zsdt0163-pesotransb =  wl_zlest0039-pesotransb.
    endif.

    if ( wl_nota-reftyp eq 'BI' ) and ( wl_nota-vbeln_vl is not initial ).
      read table tg_zsdt0023 with key vbeln = wl_nota-vbeln_vl.
      if ( sy-subrc eq 0 ) and ( tg_zsdt0023-mblnr_s is not initial ).
        read table tg_mkpf with key mblnr = tg_zsdt0023-mblnr_s
                                    mjahr = tg_zsdt0023-mjahr_s.
        if sy-subrc eq 0.
          wa_saida_0100_zsdt0163-mblnr_s = tg_mkpf-mblnr.
          wa_saida_0100_zsdt0163-mjahr_s = tg_mkpf-mjahr.
          wa_saida_0100_zsdt0163-werks_d = tg_zsdt0023-werks_v.
          wa_saida_0100_zsdt0163-lgort_d = tg_zsdt0023-lgort_v.
        endif.
      endif.
    else.

**Inicio CS2022000880 Ajustes de erros e problemas ZSDT0163 / ZSDT0034 / Anderson
      if ( wl_nota-reftyp eq 'ZW' ).

        read table t_zfiwrt0008 into data(ws_zfiwrt0008) with key docnum = wl_nota-docnum.
        if sy-subrc eq 0.
*&******************************************************************************************************
*&    Inicio do ajuste seleção do centro responsavel pegando dos dados gerais e não dos itens / IR123815 / AOENNING.
*&******************************************************************************************************
          wa_saida_0100_zsdt0163-werks_d = ws_zfiwrt0008-move_plant. "Destino
          wa_saida_0100_zsdt0163-lgort_d = ws_zfiwrt0008-move_stloc. "Destino

          wa_saida_0100_zsdt0163-werks = ''. "Origem
          wa_saida_0100_zsdt0163-lgort = ''. "Origem

*&******************************************************************************************************
*&    Fim do ajuste seleção do centro responsavel pegando dos dados gerais e não dos itens / IR123815 / AOENNING.
*&******************************************************************************************************
        endif.

        wa_saida_0100_zsdt0163-mblnr_s = ''.
        wa_saida_0100_zsdt0163-mjahr_s = ''.

      endif.
**Inicio CS2022000880 Ajustes de erros e problemas ZSDT0163 / ZSDT0034 / Anderson
    endif.

    "SEMPRE VERIFICAR SE TEM CARTA CORRECAO ALRS 17.01.2023
    "Busca Centro Deposito Destino da Carta de correção
    if ( wa_saida_0100_zsdt0163-werks_d is not initial ) and ( wl_nota-mblnr_cce is not initial ).
      read table tg_mkpf with key mblnr = wl_nota-mblnr_cce
                                  mjahr = wl_nota-mjahr_cce.
      if sy-subrc eq 0.
        loop at tg_mseg where mblnr eq tg_mkpf-mblnr
                          and mjahr eq tg_mkpf-mjahr.

          if ( tg_mseg-werks eq wa_saida_0100_zsdt0163-werks_d ) and
             ( tg_mseg-lgort eq wa_saida_0100_zsdt0163-lgort_d ).
            continue.
          endif.

          wa_saida_0100_zsdt0163-werks_d = tg_mseg-werks.
          wa_saida_0100_zsdt0163-lgort_d = tg_mseg-lgort.
          "
          wa_saida_0100_zsdt0163-mblnr_cce = tg_mseg-mblnr.
          wa_saida_0100_zsdt0163-mjahr_cce = tg_mseg-mjahr.
          select single authcode
           into wa_saida_0100_zsdt0163-authcode
           from zcarta_correcao
           where doc_material = tg_mseg-mblnr
           and   ano_material = tg_mseg-mjahr.
          exit.
        endloop.
      endif.
    endif.

    wa_saida_0100_zsdt0163-credat           =   wl_nota-credat.
    wa_saida_0100_zsdt0163-pstdat           =   wl_nota-pstdat.
    wa_saida_0100_zsdt0163-docdat           =   wl_nota-docdat.
    wa_saida_0100_zsdt0163-nfenum           =   |{ wl_nota-nfenum alpha = out }|.
    wa_saida_0100_zsdt0163-bukrs            =   wl_nota-bukrs.
    wa_saida_0100_zsdt0163-branch           =   wl_nota-branch.
    wa_saida_0100_zsdt0163-werks            =   wl_nota-werks.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wl_nota-matnr
      importing
        output = wa_saida_0100_zsdt0163-matnr.

    wa_saida_0100_zsdt0163-menge            =   wl_nota-menge.
    wa_saida_0100_zsdt0163-meins            =   wl_nota-meins.
    wa_saida_0100_zsdt0163-netpr            =   wl_nota-netpr.
    wa_saida_0100_zsdt0163-netwrt           =   wl_nota-netwrt.
    wa_saida_0100_zsdt0163-charg            =   wl_nota-charg.
    wa_saida_0100_zsdt0163-lgort            =   wl_nota-lgort.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wl_nota-lifnr_z1
      importing
        output = wa_saida_0100_zsdt0163-lifnr_z1.

    wa_saida_0100_zsdt0163-chave_nfe        =   wl_nota-chave_nfe.
    wa_saida_0100_zsdt0163-parid            =   wl_nota-parid.
    wa_saida_0100_zsdt0163-partyp           =   wl_nota-partyp.
    wa_saida_0100_zsdt0163-qtde_vinc        =   wl_nota-qtde_vinc.
    wa_saida_0100_zsdt0163-qtde_cct         =   wl_nota-qtde_cct.
    wa_saida_0100_zsdt0163-dt_recepcao_cct  =   wl_nota-dt_recepcao_cct.

    wa_saida_0100_zsdt0163-qtde_quebra      =   wl_nota-qtde_quebra.
    wa_saida_0100_zsdt0163-qtde_sobra       =   wl_nota-qtde_sobra.
    wa_saida_0100_zsdt0163-saldo_nf         =   wl_nota-saldo_nf.
    wa_saida_0100_zsdt0163-saldo_cct        =   wl_nota-saldo_cct.

    read table tg_mara into data(lwa_mara)
                       with key matnr = wl_nota-matnr binary search.
    if sy-subrc is initial.
      wa_saida_0100_zsdt0163-matkl = lwa_mara-matkl.
    endif.

    read table t_dd07v into s_dd07v with key domvalue_l = v_domvalue.
    if sy-subrc eq 0.
    endif.

    if wl_nota-ra_cct is not initial.
      read table tg_zsdt0168 with key codigo_ra = wl_nota-ra_cct.
      if sy-subrc eq 0.
        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = tg_zsdt0168-lifnr
          importing
            output = wa_saida_0100_zsdt0163-lifnr_z1_cct.
      endif.
    endif.

*----------------------------------------------------------------------------*
*   Adicionar Linhas no ALV de Documentos de Retorno para a nota fiscal
*----------------------------------------------------------------------------*
    loop at tg_zsdt_retlote where docnum = wa_saida_0100_zsdt0163-docnum.

      read table tg_zsdt_export with key docnum = tg_zsdt_retlote-docnum_ret.

      read table tg_doc_ret with key docnum = tg_zsdt_retlote-docnum_ret.
      check sy-subrc eq 0.

      wa_saida_0100_zsdt0163-saldo_nf       = 0.
      wa_saida_0100_zsdt0163-qtde_atribuida = tg_zsdt_retlote-quant_vinc.
      wa_saida_0100_zsdt0163-docnum_retorno = tg_zsdt_retlote-docnum_ret.
      wa_saida_0100_zsdt0163-docdat_quebra  = tg_doc_ret-docdat.

      if wa_saida_0100_zsdt0163-docdat_quebra is not initial.
        wa_saida_0100_zsdt0163-dias = wa_saida_0100_zsdt0163-docdat_quebra - wl_nota-docdat.
      else.
        wa_saida_0100_zsdt0163-dias = sy-datum - wl_nota-docdat.
      endif.

      if ( wa_saida_0100_zsdt0163-nferet_quebra is not initial ) and ( wa_saida_0100_zsdt0163-nferet_quebra(1) ne '@' ).
        wa_saida_0100_zsdt0163-nferet_quebra_in = wa_saida_0100_zsdt0163-nferet_quebra.
      endif.

      if ( wa_saida_0100_zsdt0163-nfenum_znfw is not initial ) and ( wa_saida_0100_zsdt0163-nfenum_znfw(1) ne '@' ).
        wa_saida_0100_zsdt0163-nfenum_znfw_in = wa_saida_0100_zsdt0163-nfenum_znfw.
      endif.

      append wa_saida_0100_zsdt0163 to it_saida_0100_zsdt0163.


    endloop.

*-------------------------------------------------------------------------------------------------------------------------------------*
*   Caso a nota fiscal possua Saldo, adicionar uma linha no ALV disponibilizando a saldo para geração de um novo Retorno
*-------------------------------------------------------------------------------------------------------------------------------------*

    if wl_nota-saldo_nf > 0.
      move-corresponding wa_saida_0100_zsdt0163 to wa_saida_lc.

      wa_saida_lc-saldo_nf  = wl_nota-saldo_nf.

      clear: wa_saida_lc-qtde_atribuida,
             wa_saida_lc-docdat_quebra,
             wa_saida_lc-docnum_retorno,
             wa_saida_lc-nferet_quebra,
             wa_saida_lc-seq_lcto_znfw,
             wa_saida_lc-docnum_znfw,
             wa_saida_lc-nfenum_znfw,
             wa_saida_lc-docdat_znfw,
             wa_saida_lc-mblnr_znfw ,
             wa_saida_lc-mblnr.

      if wa_saida_lc-docdat_quebra is not initial.
        wa_saida_lc-dias = wa_saida_lc-docdat_quebra - wl_nota-docdat.
      else.
        wa_saida_lc-dias = sy-datum - wl_nota-docdat.
      endif.

      perform f_set_status_registro changing wa_saida_lc.

      append wa_saida_lc to it_saida_0100_zsdt0163.

      wa_saida_lc-saldo_nf = wa_saida_0100-saldo_nf. "US 142076 - MMSILVA - 27.11.2024

      clear: wl_zlest0039, tg_mkpf, ws_zfiwrt0008.", ws_zfiwrt0009.

    endif.

  endloop.

  perform z_busca_dados_atualizado.

endform.



form f_limpa_variaveis_zsdt0163.

  clear: tg_notas[],
         tg_notas_aux[],
         tg_zsdt0023[],
         tg_zsdt0023_aux[],
         tg_mkpf[],
         tg_mseg[],
         tg_zsdt_retlote[],
         tg_zsdt_export[],
         tg_doc_ret[],
         it_saida_0100_zsdt0163[],
         tg_active[].

endform.



form f_set_status_registro changing c_saida type ty_saida_0100_zsdt0163.


  case c_saida-finalidade+0(1).
    when 'Q'.
      c_saida-lcto_conc     = cond #( when c_saida-docnum_retorno is not initial and
                                           c_saida-seq_lcto_znfw  is not initial and
                                           c_saida-docnum_znfw    is not initial and
                                           c_saida-nfenum_znfw    is not initial and
                                           c_saida-mblnr_znfw     is not initial and
                                           c_saida-nferet_flag+0(4) = icon_complete and
                                           c_saida-nfenum_flag+0(4) = icon_complete  then icon_led_green else icon_led_yellow ).

    when 'R'.
      c_saida-lcto_conc     = cond #( when c_saida-docnum_retorno is not initial and
                                           c_saida-nferet_flag+0(4) = icon_complete and
                                           c_saida-mblnr          is not initial  then icon_led_green else icon_led_yellow ).

    when 'X'.
      c_saida-lcto_conc     = cond #( when c_saida-docnum_retorno is not initial and
                                           c_saida-nferet_flag+0(4) = icon_complete and
                                           c_saida-mblnr          is not initial  then icon_led_green else icon_led_yellow ).

    when 'N'.
      c_saida-lcto_conc     = cond #( when c_saida-docnum_retorno is not initial and
                                           c_saida-nferet_flag+0(4) = icon_complete and
                                           c_saida-mblnr          is not initial  then icon_led_green else icon_led_yellow ).

    when 'S'.
      c_saida-lcto_conc     = cond #( when c_saida-docnum_retorno is not initial and
                                           c_saida-nferet_flag+0(4) = icon_complete and
                                           c_saida-mblnr          is not initial and
                                           c_saida-seq_lcto_znfw  is not initial and
                                           c_saida-docnum_znfw    is not initial and
                                           c_saida-nfenum_znfw    is not initial and
                                           c_saida-nfenum_flag+0(4) = icon_complete and
                                           c_saida-mblnr_znfw     is not initial  then icon_led_green else icon_led_yellow ).

    when 'O'.
      c_saida-lcto_conc     = cond #( when c_saida-docnum_retorno is not initial and
                                           c_saida-nferet_flag+0(4) = icon_complete and
                                           c_saida-mblnr          is not initial  then icon_led_green else icon_led_yellow ).

    when 'Y'.
      c_saida-lcto_conc     = cond #( when c_saida-docnum_retorno is not initial and
                                           c_saida-nferet_flag+0(4) = icon_complete and
                                           c_saida-mblnr          is not initial and
                                           c_saida-seq_lcto_znfw  is not initial and
                                           c_saida-docnum_znfw    is not initial and
                                           c_saida-nfenum_znfw    is not initial and
                                           c_saida-nfenum_flag+0(4) = icon_complete and
                                           c_saida-mblnr_znfw     is not initial  then icon_led_green else icon_led_yellow ).
  endcase.

endform.

* US 142076 // MMSILVA - 27.11.2024 - Fim
