*&---------------------------------------------------------------------*
*&  Include           ZSDR0112_FORM
*&---------------------------------------------------------------------*

form f_selecionar_dados using p_refresh_alv type char1.
  data: r_reftyp type range of j_1breftyp.
  append value #( sign = 'I' option = 'EQ' low = 'BI' ) to r_reftyp.
  append value #( sign = 'I' option = 'EQ' low = 'ZW' ) to r_reftyp.


  perform f_limpa_variaveis.

  if p_refresh_alv is initial.
    clear: tg_notas_rfl[].

    if p_dtrec-high is initial.
      p_dtrec-high = p_dtrec-low.
    endif.

    if  p_dtem-high is initial.
      p_dtem-high = p_dtem-low.
    endif.


    zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->selecionar_notas(
      exporting
        i_bukrs      =  p_bukrs
        i_fkart      =  p_fkart
        i_werks      =  p_werks
        i_lgort      =  p_lgort
        i_charg      =  p_safra
        i_kunnr      =  p_kunnr
        i_terminal   =  p_termi
        i_matnr      =  p_matnr
        i_status_cct =  ' '
        i_dt_ini_rec =  p_dtrec-low
        i_dt_fim_rec =  p_dtrec-high
        i_dt_ini_emi =  p_dtem-low
        i_dt_fim_emi =  p_dtem-high
      importing
        e_notas    =  tg_notas_rfl ).

*    DELETE tg_notas_rfl WHERE qtde_quebra IS INITIAL AND reftyp <> 'ZW'.
  endif.

  delete tg_notas_rfl where docnum not in p_docnum[].

  check tg_notas_rfl[] is not initial.

  loop at tg_notas_rfl into data(wl_nota_rfl).
    clear: tg_notas.

    move-corresponding wl_nota_rfl to tg_notas.
    append tg_notas.
  endloop.

  tg_notas_aux[] = tg_notas[].
*  DELETE tg_notas_aux WHERE vbeln_vl IS INITIAL AND reftyp NOT IN r_reftyp OR reftyp NOT IN r_reftyp.


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
    where bukrs      eq p_bukrs
      and finalidade eq p_final.

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
  loop at it_saida_0100 into wa_saida_0100.
    if wa_saida_0100-seq_lcto_znfw is not initial.
      move: wa_saida_0100-seq_lcto_znfw to tl_docest-seq_lcto.

      append tl_docest.
      clear: tl_docest.
    endif.
  endloop.

  sort  tl_docest by seq_lcto.

  call function 'ZNFW_ESTORNA_SEQ_LCTO'
    tables
      t_docs = tl_docest.

  loop at it_saida_0100 into wa_saida_0100.
    data(tabix) = sy-tabix.
    refresh: wa_saida_0100-color.
    "
    wa_saida_0100-docnum_ret_flag = icon_warning.
    if wa_saida_0100-docnum_retorno is not initial.
      shift wa_saida_0100-docnum_znfw left deleting leading '0'.
      concatenate  icon_checked wa_saida_0100-docnum_retorno into wa_saida_0100-docnum_ret_flag separated by ' - '.

      select single *
      from j_1bnfe_active
       into @data(wa_active)
        where docnum eq @wa_saida_0100-docnum_retorno.
      if sy-subrc = 0.
        select single *
        from j_1bnfdoc
        into @data(wa_nfdoc)
        where docnum eq @wa_saida_0100-docnum_retorno.

        wa_saida_0100-nferet_quebra = wa_nfdoc-nfenum.

        if wa_saida_0100-nferet_quebra is initial.
          wa_saida_0100-nferet_flag = icon_warning.
        else.
          wa_saida_0100-nferet_flag = wa_nfdoc-nfenum.
          condense wa_saida_0100-nferet_flag no-gaps.
          "
          shift wa_saida_0100-nferet_flag left deleting leading '0'.
          if wa_nfdoc-candat is not initial.
            concatenate  icon_storno wa_saida_0100-nferet_flag into wa_saida_0100-nferet_flag separated by ' - '.
          elseif wa_active-cancel = 'X'. "Cancelado SEFAZ
            concatenate  icon_cancel wa_saida_0100-nferet_flag into wa_saida_0100-nferet_flag separated by ' - '.
          elseif wa_active-docsta eq space or   wa_active-action_requ eq space.
            concatenate  icon_activity wa_saida_0100-nferet_flag into wa_saida_0100-nferet_flag separated by ' - '.
          elseif wa_active-docsta eq '1'.
            concatenate  icon_complete wa_saida_0100-nferet_flag into wa_saida_0100-nferet_flag separated by ' - '.
          else.
            concatenate  icon_status_critical wa_saida_0100-nferet_flag into wa_saida_0100-nferet_flag separated by ' - '.
          endif.
        endif.
      endif.

      w_final = wa_saida_0100-finalidade+0(1).
      "Transferencia
      select single * into wg_zsdt0283 from zsdt0283  where finalidade = w_final.
      if sy-subrc eq 0 and wg_zsdt0283-transf_saldo = 'X'.
        select single *
          into @data(w_doc)
          from j_1bnfdoc where docnum = @wa_saida_0100-docnum_retorno.
        vl_xblnr = conv #( wa_saida_0100-nfenum ).
*        vl_bktxt = CONV #( wa_saida_0100-vbeln_vl ).                                                    "US140390-Regra atribuição finalidade-ALRS
        concatenate wa_saida_0100-vbeln_vl wa_saida_0100-docnum_retorno  into vl_bktxt separated by '/'. "US140390-Regra atribuição finalidade-ALRS
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
            wa_saida_0100-mblnr = tg_mkpf_2-mblnr.
            wa_saida_0100-mjahr = tg_mkpf_2-mjahr.
            wa_saida_0100-budat = tg_mkpf_2-budat.
          endif.
        else.
*          vl_xblnr = conv #( wa_saida_0100-docnum_retorno ). "US140390-Regra atribuição finalidade-ALRS
          vl_bktxt = conv #( wa_saida_0100-vbeln_vl ).        "US140390-Regra atribuição finalidade-ALRS
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
              wa_saida_0100-mblnr = tg_mkpf_2-mblnr.
              wa_saida_0100-mjahr = tg_mkpf_2-mjahr.
              wa_saida_0100-budat = tg_mkpf_2-budat.
            endif.
          endif.
        endif.
        if wa_saida_0100-mblnr is initial.
          wa_saida_0100-mblnr_flag = icon_warning.
        else.
          shift wa_saida_0100-mblnr left deleting leading '0'.
          concatenate  icon_checked wa_saida_0100-mblnr into wa_saida_0100-mblnr_flag separated by ' - '.
        endif.
      else.
        wa_saida_0100-mblnr_flag = icon_negative.
      endif.
      "Transferencia

      "ZNFW
      select single * into wg_zsdt0283 from zsdt0283  where finalidade = w_final.
      if sy-subrc eq 0 and wg_zsdt0283-operacao_b is not initial.
        select single *
         from zfiwrt0008
         into @data(wa_zfiwrt0008)
         where  docnum_retorno = @wa_saida_0100-docnum_retorno
         and    loekz = ' '.
        if sy-subrc eq 0.
          wa_saida_0100-seq_lcto_znfw = wa_zfiwrt0008-seq_lcto.
          wa_saida_0100-docnum_znfw   = wa_zfiwrt0008-docnum.
          wa_saida_0100-docdat_znfw   = wa_zfiwrt0008-bldat.
          wa_saida_0100-mblnr_znfw    = wa_zfiwrt0008-mblnr.
          if wa_saida_0100-mblnr_znfw is initial.
            wa_saida_0100-mblnr_znfw_flag = icon_warning.
          else.
            shift wa_saida_0100-mblnr_znfw left deleting leading '0'.
            concatenate  icon_checked wa_saida_0100-mblnr_znfw into wa_saida_0100-mblnr_znfw_flag separated by ' - '.
          endif.

          if wa_saida_0100-docnum_znfw is not initial.
            select single *
              from j_1bnfe_active
               into wa_active
                where docnum eq wa_saida_0100-docnum_znfw.

            if sy-subrc eq 0.
              wa_saida_0100-nfenum_znfw = wa_active-nfnum9.
              select single *
                 from j_1bnfdoc
                 into wa_nfdoc
                 where docnum eq wa_saida_0100-docnum_znfw.

              if wa_saida_0100-nfenum_znfw is initial.
                wa_saida_0100-nfenum_flag = icon_warning.
              else.
                wa_saida_0100-nfenum_flag = wa_nfdoc-nfenum.
                condense wa_saida_0100-nfenum_flag no-gaps.
                "
                shift wa_saida_0100-nfenum_flag left deleting leading '0'.
                if wa_nfdoc-candat is not initial.
                  concatenate  icon_storno wa_saida_0100-nfenum_flag into wa_saida_0100-nfenum_flag separated by ' - '.
                elseif wa_active-cancel = 'X'. "Cancelado SEFAZ
                  concatenate  icon_cancel wa_saida_0100-nfenum_flag into wa_saida_0100-nfenum_flag separated by ' - '.
                elseif wa_active-docsta eq space or   wa_active-action_requ eq space.
                  concatenate  icon_activity wa_saida_0100-nfenum_flag into wa_saida_0100-nfenum_flag separated by ' - '.
                elseif wa_active-docsta eq '1'.
                  concatenate  icon_complete wa_saida_0100-nfenum_flag into wa_saida_0100-nfenum_flag separated by ' - '.
                else.
                  concatenate  icon_status_critical wa_saida_0100-nfenum_flag into wa_saida_0100-nfenum_flag separated by ' - '.
                endif.
              endif.
              "
              clear: tl_docest,  wa_saida_0100-color.

              read table tl_docest
              with key seq_lcto = wa_zfiwrt0008-seq_lcto
                  binary search.

              if ( tl_docest-docnum_est is not initial
                 and tl_docest-docnum_est ne '0000000000' ) or wa_zfiwrt0008-loekz = 'X'.
                clear: wl_color.
                wl_color-fieldname = 'DOCNUM_FLAG'.
                wl_color-color-col = 6.
                wl_color-color-inv = 6.
                append wl_color to wa_saida_0100-color.

                clear: wl_color.
                wl_color-fieldname = 'SEQ_LCTO_FLAG'.
                wl_color-color-col = 6.
                wl_color-color-inv = 6.
                append wl_color to wa_saida_0100-color.
              endif.
            else.
              wa_saida_0100-nfenum_flag = icon_warning.
            endif.
          endif.
        endif.
        if wa_saida_0100-seq_lcto_znfw is initial.
          wa_saida_0100-seq_lcto_flag = icon_warning.
        else.
          shift wa_saida_0100-seq_lcto_znfw left deleting leading '0'.
          concatenate  icon_checked wa_saida_0100-seq_lcto_znfw into wa_saida_0100-seq_lcto_flag separated by ' - '.
        endif.
        if wa_saida_0100-docnum_znfw is initial.
          wa_saida_0100-docnum_flag = icon_warning.
        else.
          shift wa_saida_0100-docnum_znfw left deleting leading '0'.
          concatenate  icon_checked wa_saida_0100-docnum_znfw into wa_saida_0100-docnum_flag separated by ' - '.
        endif.
      else.
        wa_saida_0100-seq_lcto_flag   = icon_negative.
        wa_saida_0100-docnum_flag     = icon_negative.
        wa_saida_0100-nfenum_flag     = icon_negative.
        wa_saida_0100-mblnr_znfw_flag = icon_negative.
      endif.
    endif.

    perform f_set_status_registro changing wa_saida_0100.

    modify it_saida_0100 from wa_saida_0100 index tabix.
  endloop.
endform.

form f_processa_dados.

  data v_domvalue   type char10.
  data: lv_erro     type c.
  data vl_xblnr     type mkpf-xblnr.
  data vl_bktxt     type mkpf-bktxt.
  data vl_qtde_vinc type zde_nota_retorno_rfl-qtde_vinc.

  loop at tg_notas into data(wl_nota).

    clear: wa_saida_0100, v_domvalue, vl_qtde_vinc, vl_xblnr, tg_zsdt0023.

    wa_saida_0100-docnum       =   wl_nota-docnum.
    wa_saida_0100-reftyp       =   wl_nota-reftyp.
    wa_saida_0100-vbeln_vl     =   wl_nota-vbeln_vl.

    "Buscar os dados de descarga.
    read table tg_zlest0039 into data(wl_zlest0039) with key docnum = wl_nota-docnum.
    if sy-subrc eq 0.
      wa_saida_0100-datatransb =  wl_zlest0039-datatransb.
      wa_saida_0100-pesotransb =  wl_zlest0039-pesotransb.
    endif.

    if ( wl_nota-reftyp eq 'BI' ) and ( wl_nota-vbeln_vl is not initial ).
      read table tg_zsdt0023 with key vbeln = wl_nota-vbeln_vl.
      if ( sy-subrc eq 0 ) and ( tg_zsdt0023-mblnr_s is not initial ).
        read table tg_mkpf with key mblnr = tg_zsdt0023-mblnr_s
                                    mjahr = tg_zsdt0023-mjahr_s.
        if sy-subrc eq 0.
          wa_saida_0100-mblnr_s = tg_mkpf-mblnr.
          wa_saida_0100-mjahr_s = tg_mkpf-mjahr.
          wa_saida_0100-werks_d = tg_zsdt0023-werks_v.
          wa_saida_0100-lgort_d = tg_zsdt0023-lgort_v.
        endif.
      endif.
    else.
*      "Busca Centro Deposito Destino da Carta de correção
*      IF ( wa_saida_0100-werks_d IS NOT INITIAL ) AND ( wl_nota-mblnr_cce IS NOT INITIAL ).
*        READ TABLE tg_mkpf WITH KEY mblnr = wl_nota-mblnr_cce
*                                    mjahr = wl_nota-mjahr_cce.
*        IF sy-subrc EQ 0.
*          LOOP AT tg_mseg WHERE mblnr EQ tg_mkpf-mblnr
*                            AND mjahr EQ tg_mkpf-mjahr.
*
*            IF ( tg_mseg-werks EQ wa_saida_0100-werks_d ) AND
*               ( tg_mseg-lgort EQ wa_saida_0100-lgort_d ).
*              CONTINUE.
*            ENDIF.
*
*            wa_saida_0100-werks_d = tg_mseg-werks.
*            wa_saida_0100-lgort_d = tg_mseg-lgort.
*            "
*            wa_saida_0100-mblnr_cce = tg_mseg-mblnr.
*            wa_saida_0100-mjahr_cce = tg_mseg-mjahr.
*            SELECT SINGLE authcode
*             INTO wa_saida_0100-authcode
*             FROM zcarta_correcao
*             WHERE doc_material = tg_mseg-mblnr
*             AND   ano_material = tg_mseg-mjahr.
*            EXIT.
*          ENDLOOP.
*        ENDIF.
*      ENDIF.


**Inicio CS2022000880 Ajustes de erros e problemas ZSDT0163 / ZSDT0034 / Anderson
      if ( wl_nota-reftyp eq 'ZW' ).

        read table t_zfiwrt0008 into data(ws_zfiwrt0008) with key docnum = wl_nota-docnum.
        if sy-subrc eq 0.
*&******************************************************************************************************
*&    Inicio do ajuste seleção do centro responsavel pegando dos dados gerais e não dos itens / IR123815 / AOENNING.
*&******************************************************************************************************
          wa_saida_0100-werks_d = ws_zfiwrt0008-move_plant. "Destino
          wa_saida_0100-lgort_d = ws_zfiwrt0008-move_stloc. "Destino

          wa_saida_0100-werks = ''. "Origem
          wa_saida_0100-lgort = ''. "Origem

*&******************************************************************************************************
*&    Fim do ajuste seleção do centro responsavel pegando dos dados gerais e não dos itens / IR123815 / AOENNING.
*&******************************************************************************************************

*&******************************************************************************************************
*&    Comentado essa parte do processo para ajustar o centro responsavel, pegando dos dados gerais e não dos itens / IR123815 / AOENNING.
*&******************************************************************************************************
*          READ TABLE t_zfiwrt0009 INTO DATA(ws_zfiwrt0009) WITH KEY seq_lcto = ws_zfiwrt0008-seq_lcto.
*          wa_saida_0100-werks_d = ws_zfiwrt0009-bwkey. "Destino
*          wa_saida_0100-lgort_d = ws_zfiwrt0009-lgort. "Destino
*
*          wa_saida_0100-werks = ''. "Origem
*          wa_saida_0100-lgort = ''. "Origem
*&******************************************************************************************************
*&   Comentado essa parte do processo para ajustar o centro responsavel, pegando dos dados gerais e não dos itens / IR123815 / AOENNING.
*&******************************************************************************************************
        endif.

        wa_saida_0100-mblnr_s = ''.
        wa_saida_0100-mjahr_s = ''.

      endif.
**Inicio CS2022000880 Ajustes de erros e problemas ZSDT0163 / ZSDT0034 / Anderson
    endif.

    "SEMPRE VERIFICAR SE TEM CARTA CORRECAO ALRS 17.01.2023
    "Busca Centro Deposito Destino da Carta de correção
    if ( wa_saida_0100-werks_d is not initial ) and ( wl_nota-mblnr_cce is not initial ).
      read table tg_mkpf with key mblnr = wl_nota-mblnr_cce
                                  mjahr = wl_nota-mjahr_cce.
      if sy-subrc eq 0.
        loop at tg_mseg where mblnr eq tg_mkpf-mblnr
                          and mjahr eq tg_mkpf-mjahr.

          if ( tg_mseg-werks eq wa_saida_0100-werks_d ) and
             ( tg_mseg-lgort eq wa_saida_0100-lgort_d ).
            continue.
          endif.

          wa_saida_0100-werks_d = tg_mseg-werks.
          wa_saida_0100-lgort_d = tg_mseg-lgort.
          "
          wa_saida_0100-mblnr_cce = tg_mseg-mblnr.
          wa_saida_0100-mjahr_cce = tg_mseg-mjahr.
          select single authcode
           into wa_saida_0100-authcode
           from zcarta_correcao
           where doc_material = tg_mseg-mblnr
           and   ano_material = tg_mseg-mjahr.
          exit.
        endloop.
      endif.
    endif.

    wa_saida_0100-credat           =   wl_nota-credat.
    wa_saida_0100-pstdat           =   wl_nota-pstdat.
    wa_saida_0100-docdat           =   wl_nota-docdat.
    wa_saida_0100-nfenum           =   |{ wl_nota-nfenum alpha = out }|.
    wa_saida_0100-bukrs            =   wl_nota-bukrs.
    wa_saida_0100-branch           =   wl_nota-branch.
    wa_saida_0100-werks            =   wl_nota-werks.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wl_nota-matnr
      importing
        output = wa_saida_0100-matnr.

    wa_saida_0100-menge            =   wl_nota-menge.
    wa_saida_0100-meins            =   wl_nota-meins.
    wa_saida_0100-netpr            =   wl_nota-netpr.
    wa_saida_0100-netwrt           =   wl_nota-netwrt.
    wa_saida_0100-charg            =   wl_nota-charg.
    wa_saida_0100-lgort            =   wl_nota-lgort.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wl_nota-lifnr_z1
      importing
        output = wa_saida_0100-lifnr_z1.

    wa_saida_0100-chave_nfe        =   wl_nota-chave_nfe.
    wa_saida_0100-parid            =   wl_nota-parid.
    wa_saida_0100-partyp           =   wl_nota-partyp.
    wa_saida_0100-qtde_vinc        =   wl_nota-qtde_vinc.
    wa_saida_0100-qtde_cct         =   wl_nota-qtde_cct.
    wa_saida_0100-dt_recepcao_cct  =   wl_nota-dt_recepcao_cct.

    wa_saida_0100-qtde_quebra      =   wl_nota-qtde_quebra.
    wa_saida_0100-qtde_sobra       =   wl_nota-qtde_sobra.
    wa_saida_0100-saldo_nf         =   wl_nota-saldo_nf.
    wa_saida_0100-saldo_cct        =   wl_nota-saldo_cct.

    read table tg_mara into data(lwa_mara)
                       with key matnr = wl_nota-matnr binary search.
    if sy-subrc is initial.
      wa_saida_0100-matkl = lwa_mara-matkl.
    endif.

    v_domvalue   = p_final.

    read table t_dd07v into s_dd07v with key domvalue_l = v_domvalue.
    if sy-subrc eq 0.
      concatenate p_final '-' s_dd07v-ddtext into wa_saida_0100-finalidade.
    endif.

    if wl_nota-ra_cct is not initial.
      read table tg_zsdt0168 with key codigo_ra = wl_nota-ra_cct.
      if sy-subrc eq 0.
        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = tg_zsdt0168-lifnr
          importing
            output = wa_saida_0100-lifnr_z1_cct.
      endif.
    endif.

*----------------------------------------------------------------------------*
*   Adicionar Linhas no ALV de Documentos de Retorno para a nota fiscal
*----------------------------------------------------------------------------*
    loop at tg_zsdt_retlote where docnum = wa_saida_0100-docnum.

      read table tg_zsdt_export with key docnum = tg_zsdt_retlote-docnum_ret.

      check ( sy-subrc eq 0 ) and ( tg_zsdt_export-finalidade eq p_final or p_final is initial ). "Documento Retorno da Finalidade Selecionada no Filtro

      read table tg_doc_ret with key docnum = tg_zsdt_retlote-docnum_ret.
      check sy-subrc eq 0.

      if p_final is initial.
        v_domvalue   = tg_zsdt_export-finalidade.

        read table t_dd07v into s_dd07v with key domvalue_l = v_domvalue.
        if sy-subrc eq 0.
          concatenate tg_zsdt_export-finalidade '-' s_dd07v-ddtext into wa_saida_0100-finalidade.
        endif.
      endif.


      "wa_saida_0100-docret_quebra  = tg_zsdt_retlote-docnum_ret.
      wa_saida_0100-saldo_nf       = 0.
      wa_saida_0100-qtde_atribuida = tg_zsdt_retlote-quant_vinc.
      wa_saida_0100-docnum_retorno = tg_zsdt_retlote-docnum_ret.
      "
      wa_saida_0100-docdat_quebra  = tg_doc_ret-docdat.

      if wa_saida_0100-docdat_quebra is not initial.
        wa_saida_0100-dias = wa_saida_0100-docdat_quebra - wl_nota-docdat.
      else.
        wa_saida_0100-dias = sy-datum - wl_nota-docdat.
      endif.

      if ( wa_saida_0100-nferet_quebra is not initial ) and ( wa_saida_0100-nferet_quebra(1) ne '@' ).
        wa_saida_0100-nferet_quebra_in = wa_saida_0100-nferet_quebra.
      endif.

      if ( wa_saida_0100-nfenum_znfw is not initial ) and ( wa_saida_0100-nfenum_znfw(1) ne '@' ).
        wa_saida_0100-nfenum_znfw_in = wa_saida_0100-nfenum_znfw.
      endif.

      append wa_saida_0100 to it_saida_0100.

    endloop.

*-------------------------------------------------------------------------------------------------------------------------------------*
*   Caso a nota fiscal possua Saldo, adicionar uma linha no ALV disponibilizando a saldo para geração de um novo Retorno
*-------------------------------------------------------------------------------------------------------------------------------------*

    if wl_nota-saldo_nf > 0.
      move-corresponding wa_saida_0100 to wa_saida_lc.

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
*             wa_saida_lc-finalidade,
             wa_saida_lc-mblnr.

      if wa_saida_lc-docdat_quebra is not initial.
        wa_saida_lc-dias = wa_saida_lc-docdat_quebra - wl_nota-docdat.
      else.
        wa_saida_lc-dias = sy-datum - wl_nota-docdat.
      endif.

      if p_final is initial.
        clear:  wa_saida_lc-finalidade.
      endif.

      perform f_set_status_registro changing wa_saida_lc.

      append wa_saida_lc to it_saida_0100.
      clear: wl_zlest0039, tg_mkpf, ws_zfiwrt0008.", ws_zfiwrt0009.

    endif.

  endloop.

  perform z_busca_dados_atualizado.

  case abap_true.
    when p_ger. "notas com saldo
      delete  it_saida_0100 where saldo_nf eq 0 and lcto_conc = icon_led_green.
      delete  it_saida_0100 where lcto_conc = icon_led_green.
    when p_pen. "notas utilizadas
*      DELETE  it_saida_0100 WHERE saldo_nf NE 0 AND lcto_conc <> icon_led_green.
      delete  it_saida_0100 where lcto_conc <> icon_led_green.
  endcase.


endform.

form f_call_alv.

* CALL SCREEN 0100.

  if obj_alv_0100 is initial.
    call screen 0100.
  else.
    call method obj_alv_0100->refresh_table_display
      exporting
        is_stable = wa_stable.

    if lines( it_row ) > 0.
      call method obj_alv_0100->set_selected_rows
        exporting
          it_index_rows = it_row.
    endif.
  endif.

endform.

form f_refresh_objetos .

  clear: gs_layout,
         gs_variant.

  refresh: it_exclude_fcode.

endform.



form f_criar_catalog using p_screen.

  free: wa_fcat, it_fcat.


  perform f_estrutura_alv using:

     'REFTYP'               'Tp.Ref.'                     '07'       ' '   'C' ' ',
     'BUKRS'                'Empresa'                     '07'       ' '   ' ' ' ',
     'WERKS'                'Centro'                      '06'       ' '   ' ' ' ',
     'BRANCH'               'Filial'                      '06'       ' '   ' ' ' ',
     'WERKS_D'              'Centro Dest.'                '14'       ' '   'C' ' ',
     'LGORT_D'              'Deposito Dest.'              '14'       ' '   'C' ' ',
     'NFENUM'               'Nr.NF'                       '09'       ' '   ' ' ' ',
     'CHARG'                'Lote '                       '10'       ' '   ' ' ' ',
     'DOCNUM'               'Doc.Fiscal'                  '10'       ' '   ' ' ' ',
     'VBELN_VL'             'Remessa'                     '10'       ' '   ' ' ' ',
     'MBLNR_S'              'Doc.Estoque'                 '11'       ' '   ' ' ' ',
     'PSTDAT'               'Dt.Lcto'                     '10'       ' '   ' ' ' ',
     'DOCDAT'               'Dt.Emissão'                  '10'       ' '   ' ' ' ',
     'CREDAT'               'Dt.Criação'                  '10'       ' '   ' ' ' ',
     'DT_RECEPCAO_CCT'      'Dt.Rec.CCT'                  '10'       ' '   ' ' ' ',
     'DATATRANSB     '      'Dt.Chegada'                  '10'       ' '   ' ' ' ',
     'PESOTRANSB     '      'Peso chegada'                '10'       ' '   ' ' ' ',
     'MATNR'                'Material'                    '10'       ' '   ' ' ' ',
     'LIFNR_Z1'             'Terminal'                    '10'       ' '   ' ' ' ',
     'LIFNR_Z1_CCT'         'Term.CCT'                    '10'       ' '   ' ' ' ',
     'LGORT'                'Depósito'                    '08'       ' '   'C' ' ',
     'NETWRT'               'Valor NF'                    '10'       'X'   ' ' ' ',
     'NETPR'                'Preço'                       '09'       ' '   ' ' ' ',
     'MEINS'                'Unid.'                       '05'       ' '   ' ' ' ',
     'MENGE'                'Qtde.Fiscal(KG)'             '18'       'X'   ' ' ' ',
     'QTDE_CCT'             'Qtde CCT(KG)'                '18'       'X'   ' ' ' ',
     'QTDE_QUEBRA'          'Qtde Quebra(KG)'             '18'       'X'   ' ' ' ',
     'SALDO_NF'             'Saldo NF'                    '13'       'X'   ' ' ' ',
     'QTDE_ATRIBUIDA'       'Qtde Atribuida'              '13'       ' '   ' ' ' ',
     'FINALIDADE'           'Finalidade'                  '12'       ' '   ' ' ' ',

     'DOCNUM_RET_FLAG'      'Doc.Retorno'                 '11'       ' '   ' ' 'X',
     'NFERET_FLAG'          'NF.Retorno'                  '10'       ' '   ' ' 'X',
     'DOCDAT_QUEBRA'        'Dt.Retorno'                  '10'       ' '   ' ' ' ',

     'MBLNR_FLAG'           'Doc.TransfSaldo'             '15'       ' '   ' ' 'X',

     'DOCDAT_ZNFW'          'Data.ZNFW'                   '10'       ' '   ' ' ' ',
     'SEQ_LCTO_FLAG'        'Seq.Lcto ZNFW'               '13'       ' '   ' ' 'X',
     'DOCNUM_FLAG'          'Docnum ZNFW'                 '10'       ' '   ' ' 'X',
     'NFENUM_FLAG'          'NF.ZNFW'                     '13'       ' '   ' ' 'X',
     'MBLNR_ZNFW_FLAG'      'Doc.Estoque ZNFW'            '13'       ' '   ' ' 'X',
     'LCTO_CONC'            'Lcto Concluido'              '13'       ' '   'C' ' ',
     'CHAVE_NFE'            'Chave NF-e'                  '44'       ' '   ' ' ' ',
     'DIAS'                 'Dias'                        '10'       ' '   ' ' ' ',
     'MBLNR_CCE'            'Doc. CCE'                    '10'       ' '   ' ' 'X',
     'AUTHCODE'             'ID. CCE'                     '20'       ' '   ' ' ' '.


endform.

form f_estrutura_alv using value(p_field)
                           value(p_scrtext_l)
                           value(p_outputlen)
                           value(p_sum)
                           value(p_just)
                           value(p_hotspot).

  clear wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-do_sum      = p_sum.
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.
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

form f_limpa_variaveis.

  clear: tg_notas[],
         tg_notas_aux[],
         tg_zsdt0023[],
         tg_zsdt0023_aux[],
         tg_mkpf[],
         tg_mseg[],
         tg_zsdt_retlote[],
         tg_zsdt_export[],
         tg_doc_ret[],
         it_saida_0100[],
         tg_active[].

endform.

form f_refresh_alv using p_alv.

  check obj_alv_0100 is not initial.

  call method obj_alv_0100->refresh_table_display
    exporting
      is_stable = wa_stable.

endform.

form f_gerar_lcto.

  data: it_nota_sel	type zde_nota_retorno_rfl_sel_t,
        wa_nota_sel	type zde_nota_retorno_rfl_sel.
  data: vl_seq_lcto type zfiwrt0008-seq_lcto,
        vl_mblnr    type zfiwrt0008-mblnr,
        ws_zsdt0283 type zsdt0283,
        vl_erro     type c.

  clear: it_sel_rows[], wa_sel_rows, vl_seq_lcto, vl_mblnr,  wa_nota_sel, wa_saida_copy.
  refresh it_saida_copy.

  call method obj_alv_0100->get_selected_rows
    importing
      et_index_rows = it_sel_rows.

  if it_sel_rows[] is initial.
    message 'Selecione pelo menos uma linha!' type 'S'.
    exit.
  endif.

  free: it_row.
  loop at it_sel_rows into wa_sel_rows.
    append wa_sel_rows to it_row.
  endloop.

  clear: vl_erro, ws_zsdt0283.
  perform f_valida_regras_zsdt0283 changing vl_erro ws_zsdt0283.

  check vl_erro is initial.

*=========================================INICIO ANDERSON / IR172448 ==================================
  read table tg_zsdt0283 into wg_zsdt0283 with key bukrs      = ws_zsdt0283-bukrs
                                                   matkl      = ws_zsdt0283-matkl
                                                   finalidade = ws_zsdt0283-finalidade
                                                   prazo      = ws_zsdt0283-prazo
                                                   dias       = ws_zsdt0283-dias
                                                   retorno_nf = ws_zsdt0283-retorno_nf.
*=========================================FIM ANDERSON / IR172448 ==================================
  if sy-subrc eq 0 .

    case wg_zsdt0283-retorno_nf.
      when 'S'.
        loop at it_sel_rows into wa_sel_rows.

          read table it_saida_0100 assigning field-symbol(<fs_saida_0100>) index wa_sel_rows-index.

          check sy-subrc = 0.

          if ( <fs_saida_0100>-werks_d is initial ) or ( <fs_saida_0100>-lgort_d is initial ).
            message |Centro/Deposito destino não determinado!| type 'S' display like 'E'.
            return.
          endif.

*          IF ( <fs_saida_0100>-lifnr_z1 NE <fs_saida_0100>-lifnr_z1_cct ).
*            MESSAGE |Terminal RFL { <fs_saida_0100>-lifnr_z1 } diferente do terminal do CCT { <fs_saida_0100>-lifnr_z1_cct }!| TYPE 'S' DISPLAY LIKE 'E'.
*            RETURN.
*          ENDIF.

          "Gerando DocRetorno

          if <fs_saida_0100>-docnum_retorno is initial.

            zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->novo_lancamento( ).

            wa_nota_sel-docnum    = <fs_saida_0100>-docnum.

            if <fs_saida_0100>-qtde_atribuida is not initial.
              wa_nota_sel-qtde_vinc = <fs_saida_0100>-qtde_atribuida.
            else.
              wa_nota_sel-qtde_vinc = <fs_saida_0100>-saldo_nf.
            endif.

            try.
                zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance(
                  )->set_qtde_vinc_nf( i_nota_vinc = wa_nota_sel ).
              catch zcx_controle_retorno_rfl into data(zcx_controle_rfl).
                zcx_controle_rfl->published_erro( exporting i_msgty = 'S' i_msgty_display = 'W' ).
            endtry.

            try.
                <fs_saida_0100>-docnum_retorno =
                zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance(
                                          )->gerar_retorno( exporting i_check_exists_ret_finalidade = abap_false       "US140390-Regra atribuição finalidade-ALRS
                                                                      i_dt_retorno = sy-datum
                                                                      i_finalidade = p_final
                                                                      i_parceiro = <fs_saida_0100>-lifnr_z1 ).
                if <fs_saida_0100>-docnum_retorno is not initial.

                  perform f_enviar_sefaz using <fs_saida_0100>-docnum_retorno.

                endif.
              catch zcx_controle_retorno_rfl into zcx_controle_rfl.
                zcx_controle_rfl->published_erro( exporting i_msgty = 'S' i_msgty_display = 'W' ).
              catch zcx_nf_writer into data(zcx_nf_writer).
                zcx_nf_writer->published_erro( exporting i_msgty = 'S' i_msgty_display = 'W' ).
            endtry.
          elseif <fs_saida_0100>-nferet_quebra is initial.
            select single * from j_1bnfe_active into @data(wl_active_doc)
                    where docnum eq @<fs_saida_0100>-docnum_retorno
                      and docsta eq '1'
                      and cancel eq @abap_false.
            if sy-subrc ne 0.
              perform f_enviar_sefaz using <fs_saida_0100>-docnum_retorno.
            else.
              <fs_saida_0100>-nferet_quebra  = wl_active_doc-nfnum9.
            endif.
          endif.

          if <fs_saida_0100>-docnum_retorno is not initial and <fs_saida_0100>-nferet_quebra is not initial.

**Inicio CS2022000880 Ajustes de erros e problemas ZSDT0163 / ZSDT0034 / Anderson
*
*            CASE wg_zsdt0283-finalidade.
*              WHEN 'Q'.
*                PERFORM f_lancar_znfw USING <fs_saida_0100> wg_zsdt0283-finalidade CHANGING vl_seq_lcto.
*              WHEN 'R'.
*                IF wg_zsdt0283-transf_saldo = 'X'.
*                  PERFORM f_lancar_mb1b USING <fs_saida_0100> CHANGING vl_mblnr.
*                ENDIF.
*              WHEN 'S' OR 'O'.
*                IF wg_zsdt0283-transf_saldo = 'X'.
*                  PERFORM f_lancar_mb1b USING <fs_saida_0100> CHANGING vl_mblnr.
*                ENDIF.
*                PERFORM f_lancar_znfw USING <fs_saida_0100> wg_zsdt0283-finalidade CHANGING vl_seq_lcto.
*            ENDCASE.

            if wg_zsdt0283-finalidade eq p_final. "Tela de parametro.
              if wg_zsdt0283-transf_saldo eq  abap_true.
                perform f_lancar_mb1b using <fs_saida_0100> changing vl_mblnr.
              endif.

              if wg_zsdt0283-operacao_b is not initial.
                perform f_lancar_znfw using <fs_saida_0100> wg_zsdt0283-finalidade wg_zsdt0283-transf_saldo wg_zsdt0283-operacao_b  changing vl_seq_lcto. "CS2022000880 / Anderson Oeninng
              endif.

            endif.


            if vl_seq_lcto is not initial.
              loop at it_sel_rows into wa_sel_rows.
                read table it_saida_0100 assigning <fs_saida_0100> index wa_sel_rows-index.
                check sy-subrc = 0.
                <fs_saida_0100>-seq_lcto_znfw  = vl_seq_lcto.
              endloop.
            endif.

            if vl_mblnr is not initial.
*              "// Comentado para fazer somente a linha corrente no Loop
*              LOOP AT it_sel_rows INTO wa_sel_rows.
*                READ TABLE it_saida_0100 ASSIGNING <fs_saida_0100> INDEX wl_sel_rows-index.
*                CHECK sy-subrc = 0.
              <fs_saida_0100>-mblnr          = vl_mblnr.
*              ENDLOOP.
            endif.

            perform f_refresh_alv using '0100'.

          endif.

        endloop.

      when 'N'.
        clear wl_active_doc.

        loop at it_sel_rows into wa_sel_rows.

          read table it_saida_0100 assigning <fs_saida_0100> index wa_sel_rows-index.
          check sy-subrc = 0.

          if ( <fs_saida_0100>-werks_d is initial ) or ( <fs_saida_0100>-lgort_d is initial ).
            message |Centro/Deposito destino não determinado!| type 'S' display like 'E'.
            return.
          endif.

*          IF ( <fs_saida_0100>-lifnr_z1 NE <fs_saida_0100>-lifnr_z1_cct ).
*            MESSAGE |Terminal RFL { wa_saida_0100-lifnr_z1 } diferente do terminal do CCT { wa_saida_0100-lifnr_z1_cct }!|  TYPE 'S' DISPLAY LIKE 'E'.
*            RETURN.
*          ENDIF.

          if <fs_saida_0100>-docdat_quebra  is     initial or
             <fs_saida_0100>-docnum_retorno is not initial.

            move-corresponding  <fs_saida_0100> to wa_saida_copy.
            wa_saida_copy-gerar_lcto = abap_true.

            append wa_saida_copy to it_saida_copy.
            clear wa_saida_copy.
            <fs_saida_0100>-gerar_lcto = abap_true.
            modify it_saida_0100 from <fs_saida_0100> index wa_sel_rows-index.
          endif.
          clear wa_sel_rows.
        endloop.


        read table it_saida_copy  into wa_saida_copy with key gerar_lcto = abap_true.

        check sy-subrc eq 0.

        if wa_saida_copy-docnum_retorno is initial.

          zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->novo_lancamento( ).

          "Atribuir Quantidades
          loop at it_saida_copy into data(w_saida) where gerar_lcto = abap_true.
            wa_nota_sel-docnum    = w_saida-docnum.

            if w_saida-qtde_atribuida is not initial.
              wa_nota_sel-qtde_vinc = w_saida-qtde_atribuida.
            else.
              wa_nota_sel-qtde_vinc = w_saida-saldo_nf.
            endif.

            try.
                zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance(
                   )->set_qtde_vinc_nf( i_nota_vinc = wa_nota_sel ).
              catch zcx_controle_retorno_rfl into zcx_controle_rfl.
                zcx_controle_rfl->published_erro( exporting i_msgty = 'S' i_msgty_display = 'W' ).
            endtry.

          endloop.


          try.
              data(r_docnum_retorno) =
              zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance(
                                        )->gerar_retorno( exporting i_check_exists_ret_finalidade = abap_true
                                                                    i_dt_retorno = sy-datum
                                                                    i_finalidade = p_final
                                                                    i_parceiro = <fs_saida_0100>-lifnr_z1 ).
              if r_docnum_retorno is not initial.
                perform f_enviar_sefaz using r_docnum_retorno.
              endif.
            catch zcx_controle_retorno_rfl into zcx_controle_rfl.
              zcx_controle_rfl->published_erro( exporting i_msgty = 'S' i_msgty_display = 'W' ).
          endtry.

        elseif <fs_saida_0100>-nferet_quebra is initial.

          select single * from j_1bnfe_active into wl_active_doc
                  where docnum eq <fs_saida_0100>-docnum_retorno
                    and docsta eq '1'
                    and cancel eq abap_false.
          if sy-subrc ne 0.
            perform f_enviar_sefaz using <fs_saida_0100>-docnum_retorno.
          else.

            loop at it_saida_0100 assigning <fs_saida_0100>  where gerar_lcto = abap_true.
              <fs_saida_0100>-docdat_quebra  = r_docnum_retorno.
              <fs_saida_0100>-nferet_quebra  = wl_active_doc-nfnum9.
            endloop.

          endif.
        else.
          r_docnum_retorno = wa_saida_copy-docnum_retorno.
        endif.

        if r_docnum_retorno is not initial .
          loop at it_saida_copy into wa_saida_copy  where gerar_lcto = abap_true.

            wa_saida_copy-docnum_retorno = r_docnum_retorno.
            modify it_saida_copy from wa_saida_copy index sy-tabix.

            clear: vl_seq_lcto, vl_mblnr.
            "Inicio "CS2022000880 / Anderson Oeninng
*            CASE wg_zsdt0283-finalidade.
*              WHEN 'Q'.
*                PERFORM f_lancar_znfw USING wa_saida_copy wg_zsdt0283-finalidade wg_zsdt0283-transf_saldo wg_zsdt0283-operacao_b CHANGING vl_seq_lcto.
*              WHEN 'R'.
*                PERFORM f_lancar_mb1b USING wa_saida_copy CHANGING vl_mblnr.
*              WHEN 'S' OR 'O'.
*                PERFORM f_lancar_mb1b USING wa_saida_copy CHANGING vl_mblnr.
*                PERFORM f_lancar_znfw USING wa_saida_copy wg_zsdt0283-finalidade wg_zsdt0283-transf_saldo wg_zsdt0283-operacao_b CHANGING vl_seq_lcto.
*            ENDCASE.


            if wg_zsdt0283-finalidade eq p_final.
              if wg_zsdt0283-transf_saldo is not initial.
                perform f_lancar_mb1b using wa_saida_copy changing vl_mblnr.
              endif.

              if wg_zsdt0283-operacao_b is not initial.
                perform f_lancar_znfw using wa_saida_copy wg_zsdt0283-finalidade wg_zsdt0283-transf_saldo wg_zsdt0283-operacao_b changing vl_seq_lcto.
              endif.
            endif.
            "Fim "CS2022000880 / Anderson Oeninng


            unassign <fs_saida_0100>.
            read table it_saida_0100 assigning <fs_saida_0100> with key docnum = wa_saida_copy-docnum.

            if ( r_docnum_retorno is not initial
            or   vl_seq_lcto is not initial
            or   vl_mblnr is not initial ) and <fs_saida_0100> is assigned.
              <fs_saida_0100>-mblnr          = vl_mblnr.
              <fs_saida_0100>-seq_lcto_znfw  = vl_seq_lcto.
              <fs_saida_0100>-docnum_retorno = r_docnum_retorno.
            endif.
          endloop.
        endif.
        perform f_refresh_alv using '0100'.

    endcase.

  else.
    message |Favor realizar a parametrização para geração lançamento ZNFW!| type 'E' display like 'S'.
    exit.
  endif.


endform.

form f_estornar_retorno.

  data tl_docs   type table of zfiwrs0003.
  data: vg_mat_doc         type bapi2017_gm_head_ret-mat_doc,
        vg_pstng_date      type bapi2017_gm_head_02-pstng_date,
        vg_matdocumentyear type bapi2017_gm_head_ret-doc_year.
  data: wa_return       type bapiret2,
        wa_goodsmvt_ret type bapi2017_gm_head_ret,
        t_return        type table of bapiret2.



  clear: it_sel_rows[], wa_sel_rows.

  call method obj_alv_0100->get_selected_rows
    importing
      et_index_rows = it_sel_rows.

  if ( it_sel_rows[] is initial ).
    message 'Nenhuma linha foi selecionada!' type 'S'.
    exit.
  endif.

  if ( lines( it_sel_rows[] ) ne 1 ) .
    message 'Selecione apenas uma linha!' type 'S'.
    exit.
  endif.

  read table it_sel_rows into wa_sel_rows index 1.

  check sy-subrc eq 0.

  read table it_saida_0100 assigning field-symbol(<fs_saida_0100>) index wa_sel_rows-index.

  check sy-subrc eq 0.

  refresh tl_docs.

  case p_final.
    when 'Q'.
      if <fs_saida_0100>-seq_lcto_znfw is not initial.

        call function 'ZNFW_ESTORNA_SEQ_LCTO'
          exporting
            i_seq_lcto = <fs_saida_0100>-seq_lcto_znfw
            i_estorno  = 'X'
          tables
            t_docs     = tl_docs.

        loop at it_saida_0100 assigning <fs_saida_0100> where docnum_retorno = wa_saida_0100-docnum_retorno.
          check sy-subrc = 0.

          clear: <fs_saida_0100>-seq_lcto_znfw , <fs_saida_0100>-docdat_znfw, <fs_saida_0100>-docnum_znfw.
        endloop.
      endif.

      if <fs_saida_0100>-seq_lcto_znfw is initial and <fs_saida_0100>-docnum_znfw is initial.

        if <fs_saida_0100>-docnum_retorno is initial.
          message 'Docnumento Retorno ainda não gerado!' type 'S'.
          exit.
        endif.

        try.
            data(r_docnum_estorno) =
            zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance(
                                      )->cancelar_retorno( exporting i_docnum = <fs_saida_0100>-docnum_retorno ).
            if r_docnum_estorno is not initial.
              message 'Documento Retorno cancelado com sucesso!' type 'S'.

              clear: <fs_saida_0100>-docnum_retorno , <fs_saida_0100>-nferet_quebra, <fs_saida_0100>-docdat_quebra.

            endif.

            perform f_refresh_alv using '0100'.

          catch zcx_controle_retorno_rfl into data(zcx_controle_rfl).
            zcx_controle_rfl->published_erro( exporting i_msgty = 'S' i_msgty_display = 'W' ).
        endtry.

      endif.


    when 'S'.

      if <fs_saida_0100>-seq_lcto_znfw is not initial and <fs_saida_0100>-docnum_znfw is not initial.

        call function 'ZNFW_ESTORNA_SEQ_LCTO'
          exporting
            i_seq_lcto = <fs_saida_0100>-seq_lcto_znfw
            i_estorno  = 'X'
          tables
            t_docs     = tl_docs.

        if sy-subrc eq 0.
          clear: <fs_saida_0100>-seq_lcto_znfw , <fs_saida_0100>-docdat_znfw, <fs_saida_0100>-docnum_znfw.
        endif.
      else.
        message | Erro ao gerar estorno ZNFW! | type 'S' display like 'E'.
      endif.

      if <fs_saida_0100>-seq_lcto_znfw is initial.

        if <fs_saida_0100>-mblnr is not initial.

          clear: vg_mat_doc, vg_matdocumentyear, vg_pstng_date.

          vg_mat_doc          = <fs_saida_0100>-mblnr.
          vg_matdocumentyear  = <fs_saida_0100>-mjahr.
          vg_pstng_date       = <fs_saida_0100>-budat.


          call function 'BAPI_GOODSMVT_CANCEL'
            exporting
              materialdocument    = vg_mat_doc
              matdocumentyear     = vg_matdocumentyear
              goodsmvt_pstng_date = vg_pstng_date
            importing
              goodsmvt_headret    = wa_goodsmvt_ret
            tables
              return              = t_return.

          read table t_return into wa_return with key type = 'E'.
          if sy-subrc ne 0.

            call function 'BAPI_TRANSACTION_COMMIT'
              exporting
                wait = 'X'.

            clear: <fs_saida_0100>-mblnr, <fs_saida_0100>-mjahr, <fs_saida_0100>-budat.

          else.
            message wa_return-message type 'S'.
            exit.
          endif.
        endif.

        if <fs_saida_0100>-mblnr is initial.

          if <fs_saida_0100>-docnum_retorno is initial.
            message 'Docnumento Retorno ainda não gerado!' type 'S'.
            exit.
          endif.

          try.
              r_docnum_estorno =
              zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance(
                                        )->cancelar_retorno( exporting i_docnum = <fs_saida_0100>-docnum_retorno ).
              if r_docnum_estorno is not initial.
                message 'Documento Retorno cancelado com sucesso!' type 'S'.

                clear: <fs_saida_0100>-docnum_retorno , <fs_saida_0100>-nferet_quebra, <fs_saida_0100>-docdat_quebra.
              endif.

              perform f_refresh_alv using '0100'.

            catch zcx_controle_retorno_rfl into zcx_controle_rfl.
              zcx_controle_rfl->published_erro( exporting i_msgty = 'S' i_msgty_display = 'W' ).
          endtry.
        endif.
      endif.


    when 'R'.

      if <fs_saida_0100>-mblnr is not initial.

        clear: vg_mat_doc, vg_matdocumentyear, vg_pstng_date.

        vg_mat_doc          = <fs_saida_0100>-mblnr.
        vg_matdocumentyear  = <fs_saida_0100>-mjahr.
        vg_pstng_date       = <fs_saida_0100>-budat.


        call function 'BAPI_GOODSMVT_CANCEL'
          exporting
            materialdocument    = vg_mat_doc
            matdocumentyear     = vg_matdocumentyear
            goodsmvt_pstng_date = vg_pstng_date
          importing
            goodsmvt_headret    = wa_goodsmvt_ret
          tables
            return              = t_return.

        read table t_return into wa_return with key type = 'E'.
        if sy-subrc ne 0.

          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.

          clear: <fs_saida_0100>-mblnr, <fs_saida_0100>-mjahr, <fs_saida_0100>-budat.

        else.
          message wa_return-message type 'S'.
          exit.
        endif.
      endif.


      if <fs_saida_0100>-mblnr is initial.

        if <fs_saida_0100>-docnum_retorno is initial.
          message 'Docnumento Retorno ainda não gerado!' type 'S'.
          exit.
        endif.

        try.
            r_docnum_estorno =
            zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance(
                                      )->cancelar_retorno( exporting i_docnum = <fs_saida_0100>-docnum_retorno ).
            if r_docnum_estorno is not initial.
              message 'Documento Retorno cancelado com sucesso!' type 'S'.

              clear: <fs_saida_0100>-docnum_retorno , <fs_saida_0100>-nferet_quebra, <fs_saida_0100>-docdat_quebra.
            endif.

            perform f_refresh_alv using '0100'.

          catch zcx_controle_retorno_rfl into zcx_controle_rfl.
            zcx_controle_rfl->published_erro( exporting i_msgty = 'S' i_msgty_display = 'W' ).
        endtry.
      endif.
  endcase.

endform.


form f_lancar_znfw using wa_saida type ty_saida_0100
                         p_finalidade
                         p_transf
                         p_operacao_b
                 changing p_seq_lcto_znfw.

  data: it_nota_sel	type zde_nota_retorno_rfl_sel_t,
        wa_nota_sel	type zde_nota_retorno_rfl_sel.


  data: vl_matnr_18 type matnr_lo18.

  data: wl_zfiwrt0008   type zfiwrt0008,
        wl_zfiwrt0009   type zfiwrt0009,
        it_zsdt_retlote type table of zsdt_retlote with header line.

  check wg_zsdt0283-operacao_b is not initial.

  if ( wa_saida-docnum_retorno is initial ).
    message 'Doc.Retorno de Quebra não gerado!' type 'S'.
    exit.
  endif.

  check wa_saida-seq_lcto_znfw is initial.

  select single *
    from zsdt_export into @data(wl_export)
   where docnum eq @wa_saida-docnum_retorno.

  if sy-subrc ne 0.
    message |Dados exportação não encontrados(ZSDT_EXPORT)!| type 'S'.
    exit.
  endif.

  select *
    from zsdt_retlote into table it_zsdt_retlote
   where docnum_ret eq wa_saida-docnum_retorno.

  if it_zsdt_retlote[] is initial.
    message |Notas RFL documento retorno { wa_saida-docnum_retorno } não encontradas!| type 'S'.
    exit.
  endif.

  select single *
         into @data(w_active)
         from j_1bnfe_active
        where docnum = @wa_saida-docnum_retorno..

  if not ( w_active-docsta = 1 and w_active-scssta = 0 ).
    message |Notas RFL documento retorno { wa_saida-docnum_retorno } não autorizado!| type 'S'.
    exit.
  endif.

  clear: wl_zfiwrt0008.
  wl_zfiwrt0008-bukrs           = wa_saida-bukrs.
  wl_zfiwrt0008-branch          = wa_saida-branch.
  wl_zfiwrt0008-parid           = wa_saida-branch.

  call function 'CONVERSION_EXIT_ALPHA_INPUT' exporting input = wl_zfiwrt0008-parid importing output = wl_zfiwrt0008-parid.

  wl_zfiwrt0008-move_plant      = wa_saida_0100-werks_d.   "Centro Destino
  wl_zfiwrt0008-move_stloc      = wa_saida_0100-lgort_d.   "Deposito Destino
  wl_zfiwrt0008-budat           = sy-datum.
  wl_zfiwrt0008-bldat           = sy-datum.
  wl_zfiwrt0008-inco1           = 'CIF'.
  wl_zfiwrt0008-inco2           = 'CIF'.
  wl_zfiwrt0008-konto           = '0000341004'."'0000463007'. "Conta Razão
  wl_zfiwrt0008-move_batch      = wa_saida_0100-charg.
  wl_zfiwrt0008-move_mat        = wa_saida_0100-matnr.

  case p_final.
    when 'Q'.
      concatenate 'QUEBRA PORTO SAFRA' wa_saida-charg into wl_zfiwrt0008-bktxt separated by space.
      wl_zfiwrt0008-mtsnr  = 'QUEBRA PORTO'.
    when 'S'.
      concatenate 'SINISTRO NR Fiscal' wa_saida-nfenum into wl_zfiwrt0008-bktxt separated by space.
      wl_zfiwrt0008-mtsnr   = 'SINISTRO'.
  endcase.

  wl_zfiwrt0008-docnum_retorno   = wa_saida-docnum_retorno.

  clear: wl_zfiwrt0009.

  wl_zfiwrt0009-itmnum = 10.

                                                            "US119555
  call function 'CONVERSION_EXIT_ALPHA_INPUT' exporting input = wa_saida-matnr importing output = vl_matnr_18. "wl_zfiwrt0009-matnr.
  wl_zfiwrt0009-matnr = vl_matnr_18.

  "Inicio "CS2022000880 / Anderson Oeninng
*  IF p_finalidade = 'S' OR p_finalidade = 'O'.
*    wl_zfiwrt0009-bwkey  = wa_saida-werks. "wa_saida-werks_d.
*    wl_zfiwrt0009-lgort  = wa_saida-lgort. "wa_saida-lgort_d.
*  ELSE.
*    wl_zfiwrt0009-bwkey  = wa_saida-werks_d. "wa_saida-werks_d.
*    wl_zfiwrt0009-lgort  = wa_saida-lgort_d. "wa_saida-lgort_d.
*  ENDIF.

  if p_transf is not initial and p_operacao_b is not initial.
    wl_zfiwrt0009-bwkey  = wa_saida-werks. "wa_saida-werks_d.
    wl_zfiwrt0009-lgort  = wa_saida-lgort. "wa_saida-lgort_d.
  else.
    wl_zfiwrt0009-bwkey  = wa_saida-werks_d. "wa_saida-werks_d.
    wl_zfiwrt0009-lgort  = wa_saida-lgort_d. "wa_saida-lgort_d.
  endif.
  "Fim "CS2022000880 / Anderson Oeninng

  wl_zfiwrt0009-menge  = wl_export-quant.
  wl_zfiwrt0009-netwr  = wl_export-valor_total.

  if wl_zfiwrt0009-menge > 0.
    wl_zfiwrt0009-netpr  = wl_zfiwrt0009-netwr / wl_zfiwrt0009-menge.
  endif.

  select single *
    from mara into @data(wl_mara)
   where matnr eq @wl_zfiwrt0009-matnr.

  if ( sy-subrc eq 0 ).
    wl_zfiwrt0009-meins = wl_mara-meins.

    if ( wl_mara-xchpf = abap_true ).
      wl_zfiwrt0009-charg = wa_saida-charg.
    endif.
  endif.

  wl_zfiwrt0008-operacao  = wg_zsdt0283-operacao_b.

  try.
      zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                                 )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                                 )->add_item( i_item = wl_zfiwrt0009 ).
      "Documentos Referenciados.
      loop at it_zsdt_retlote into data(wl_zsdt_retlote).
        zcl_nf_writer=>zif_nf_writer~get_instance( )->add_doc_ref( i_docnum =  wl_zsdt_retlote-docnum ).
      endloop.

      "Add. Doc. Quebra
      zcl_nf_writer=>zif_nf_writer~get_instance( )->add_doc_ref( i_docnum =  wa_saida-docnum_retorno ).

      zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( exporting i_processar_lcto = abap_true
                                                                      importing e_seq_lcto       = data(_seq_lcto_gerado) ).

      if _seq_lcto_gerado is not initial.

        select single * from zfiwrt0008 into @data(wl_0008) where seq_lcto eq @_seq_lcto_gerado.

        perform f_enviar_sefaz using wl_0008-docnum.

        p_seq_lcto_znfw = _seq_lcto_gerado.

      else.
        message |Houve um erro ao gravar o lançamento!| type 'S'.
      endif.

    catch zcx_nf_writer into data(zcx_nf_writer).
      zcx_nf_writer->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  endtry.

endform.

form f_preencher_dynpro using l_start type c l_name type c l_value.

  move l_start to wl_bdc-dynbegin.
  if l_start = 'X'.
    move:
  l_name  to wl_bdc-program,
  l_value to wl_bdc-dynpro.
  else.
    move:
      l_name  to wl_bdc-fnam,
      l_value to wl_bdc-fval.
  endif.
  append wl_bdc to tl_bdc.
  clear: wl_bdc.

endform.

form f_count_nf_selecionadas.

  data: it_nota_sel	type zde_nota_retorno_rfl_sel_t,
        wa_nota_sel	type zde_nota_retorno_rfl_sel.

  clear: it_sel_rows[], wa_sel_rows.

  call method obj_alv_0100->get_selected_rows
    importing
      et_index_rows = it_sel_rows.

  data(_count_nf_sel) = lines( it_sel_rows[] ).

  message | { _count_nf_sel } notas selecionadas! | type 'S'.

endform.

***FORM c_atribuir_qrd.
***
***  DATA: it_saida_2 TYPE TABLE OF ty_saida_0100,
***        wa_saida_2 TYPE ty_saida_0100.
***
***  DATA: vl_saldo_total TYPE zde_nota_retorno_rfl-saldo_nf.
***
***  IF vg_atrib_qtde IS NOT INITIAL.
***
***    REFRESH it_saida_2.
***
***    DESCRIBE TABLE it_sel_rows LINES DATA(lines).
******
******    LOOP AT it_sel_rows INTO wa_sel_rows.
******      READ TABLE it_saida_0100 INTO DATA(wa_saida) INDEX wa_sel_rows-index.
******      IF sy-subrc EQ 0.
******        wa_saida-marc = abap_true.
******        MODIFY it_saida_0100 FROM wa_saida INDEX sy-tabix TRANSPORTING marc.
******        vl_saldo_total = vl_saldo_total + wa_saida_0100-saldo_nf.
******      ENDIF.
******      CLEAR wa_sel_rows.
******    ENDLOOP.
***
***
***    LOOP AT it_saida_0100  INTO wa_saida_0100.
******      WHERE marc EQ abap_true.
***
***      IF wa_saida_0100-saldo_nf LT vg_atrib_qtde.
***        wa_saida_0100-qtde_atribuida = wa_saida_0100-saldo_nf.
***        SUBTRACT wa_saida_0100-saldo_nf  FROM vg_atrib_qtde.
***      ELSEIF vg_atrib_qtde > 0.
***        DATA(vl_sobra) = wa_saida_0100-saldo_nf - vg_atrib_qtde.
***        wa_saida_0100-qtde_atribuida = vg_atrib_qtde.
***        vg_atrib_qtde = vl_sobra.
***      ENDIF.
***
******      wa_saida_0100-marc = abap_false.
***      MODIFY it_saida_0100 FROM wa_saida_0100 INDEX sy-tabix.
***
***      lines = lines - 1.
***
***      IF lines EQ 0 AND vg_atrib_qtde > 0.
***        wa_saida_0100-saldo_nf = vg_atrib_qtde.
***        wa_saida_0100-qtde_atribuida = 0.
***        APPEND wa_saida_0100 TO it_saida_0100.
***      ENDIF.
***
***      CLEAR wa_saida_0100.
***      IF vl_saldo_total > vg_atrib_qtde.
***        EXIT.
***      ENDIF.
***    ENDLOOP.
***
***    CLEAR vg_atrib_qtde.
***
***    CALL METHOD obj_alv_0100->refresh_table_display
***      EXPORTING
***        is_stable = wa_stable.
***  ENDIF.
***
***ENDFORM.

form f_enviar_sefaz using p_documento.

  data: v_docnum type j_1bnfdoc-docnum.

  if p_documento is initial.
    exit.
  endif.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = p_documento
    importing
      output = p_documento.

  select single *
   from j_1bnfe_active into @data(wl_active_doc)
  where docnum eq @p_documento.

  check ( sy-subrc eq 0 ) and ( wl_active_doc-action_requ is not initial ).

  check ( sy-subrc eq 0 ) and ( wl_active_doc-docsta ne '1' ).

  v_docnum = wl_active_doc-docnum.

  try.
      "Verificar Número Não Determinado
      zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = conv #( v_docnum )
        )->set_registro(
             exporting
               i_docnum = conv #( v_docnum )
               i_sem_bloqueio = abap_true
        )->get_ck_determinar_numero(
        )->set_det_numero(
        )->get_registro(
             importing
               e_documento           = data(wl_doc)
               e_info_doc_eletronico = data(wl_active)
        ).

    catch zcx_doc_eletronico into data(ex_doc_eletronico).

      try.

          zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = conv #( v_docnum )
            )->set_registro(
                 exporting
                   i_docnum = conv #( v_docnum )
                   i_sem_bloqueio = abap_true
            )->set_autorizar(
                   exporting
                           i_aguardar = abap_true
                           i_ciclos   = 50
                           i_segundos = 10
            )->get_registro(
                 importing
                   e_documento           = wl_doc
                   e_info_doc_eletronico = wl_active
            ).

        catch zcx_doc_eletronico.
        catch cx_root.
      endtry.

    catch cx_root.
  endtry.
endform.

form f_lancar_mb1b using wa_saida type ty_saida_0100 changing p_mblnr.

  data:
    sl_header   type bapi2017_gm_head_01,
    vl_code     type bapi2017_gm_code,
    vl_material type bapi2017_gm_head_ret-mat_doc,
    vl_year     type bapi2017_gm_head_ret-doc_year,
    tl_item     type table of bapi2017_gm_item_create,
    tl_return   type table of bapiret2,
    sl_return   type bapiret2,
    sl_item     type bapi2017_gm_item_create,
    vl_index    type i,
    vl_matnr18  type matnr18.


  refresh: tl_item, tl_return.
  clear: sl_header, sl_item, vl_material, vl_year.

  select single *
         into @data(w_active)
         from j_1bnfe_active
        where docnum = @wa_saida-docnum_retorno..

  if not ( w_active-docsta = 1 and w_active-scssta = 0 ).
    message |Notas RFL documento retorno { wa_saida-docnum_retorno } não autorizado!| type 'S'.
    exit.
  endif.

  vl_code = '04'.
  sl_header-pstng_date = sy-datum.
  sl_header-doc_date   = sy-datum.

  check wa_saida-mblnr is initial.

  sl_header-ref_doc_no = wa_saida-nfenum.

*  sl_header-header_txt = wa_saida-vbeln_vl.
  concatenate wa_saida-vbeln_vl wa_saida-docnum_retorno into sl_header-header_txt separated by '/'.     "US140390-Regra atribuição finalidade-ALRS

  sl_item-move_type  = '301'.



*---> 19/06/2023 - Migração S4 - DG
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wa_saida-matnr
*    IMPORTING
*      output = sl_item-material.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = wa_saida-matnr
    importing
      output = vl_matnr18.

  wa_saida-matnr = vl_matnr18.

  data(v_len) = strlen( wa_saida-matnr ).

  if v_len > 18.
    sl_item-material_long = wa_saida-matnr.
  else.
    sl_item-material      = wa_saida-matnr.
  endif.
*<--- 19/06/2023 - Migração S4 - DG




  sl_item-plant      = wa_saida-werks_d. "wa_saida-werks.
  sl_item-stge_loc   = wa_saida-lgort_d. "wa_saida-lgort.
  sl_item-batch      = wa_saida-charg.

  if wa_saida-qtde_atribuida is not initial.
    sl_item-entry_qnt  = wa_saida-qtde_atribuida.
  else.
    sl_item-entry_qnt  = wa_saida-saldo_nf.
  endif.

  sl_item-move_plant = wa_saida-werks. "wa_saida-werks_d.
  sl_item-move_stloc = wa_saida-lgort. "wa_saida-lgort_d.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = wa_saida-matnr
    importing
      output = sl_item-move_mat.

  sl_item-move_batch = wa_saida-charg.
  append sl_item to tl_item.

  call function 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
    exporting
      goodsmvt_header  = sl_header
      goodsmvt_code    = vl_code
    importing
      materialdocument = vl_material
      matdocumentyear  = vl_year
    tables
      goodsmvt_item    = tl_item
      return           = tl_return.

  if  vl_material is not initial.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.

    p_mblnr = vl_material.
  else.
*     Retorna Erro
    read table tl_return into sl_return with key type = 'E'.
    if sy-subrc eq 0.
      message sl_return-message  type  'S' display like 'E'.
      exit.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_LINHA
*&---------------------------------------------------------------------*
form f_verifica_linha tables  it_sel_rows type lvc_t_row
                       using vl_status.

  clear: vl_status.

  check lines( it_sel_rows ) > 0.

  loop at it_sel_rows into wa_sel_rows.
    read table it_saida_0100 into data(wa_saida) index wa_sel_rows-index.
    if sy-subrc eq 0.
      if wa_saida-docnum_retorno is not initial.
        vl_status = 1.
      endif.
    endif.
    clear wa_sel_rows.
  endloop.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_ATRIBUI_QRD
*&---------------------------------------------------------------------*
form f_atribui_qrd tables  it_sel_rows type lvc_t_row.

  data: vl_saldo_atender type zde_nota_retorno_rfl-saldo_nf.
  data: vl_saldo_linha   type zde_nota_retorno_rfl-saldo_nf.

  if vg_atrib_qtde is not initial.

    free it_row.

    describe table it_sel_rows lines data(lines).

    loop at it_sel_rows into wa_sel_rows.
      read table it_saida_0100 into data(wa_saida) index wa_sel_rows-index.
      if sy-subrc eq 0.
        vl_saldo_atender = vl_saldo_atender + wa_saida-saldo_nf.
        append wa_sel_rows to it_row.
      endif.
      clear wa_sel_rows.
    endloop.

    clear: wa_saida_0100, wa_sel_rows.

    if vg_atrib_qtde > vl_saldo_atender.
      message |Valor QTD Maior que o Saldo Disponivel!| type 'S' display like 'E'.
      return.
    endif.

    loop at it_sel_rows into wa_sel_rows.

      read table it_saida_0100 assigning field-symbol(<fs_saida>) index wa_sel_rows-index.

      if sy-subrc eq 0 and <fs_saida> is assigned.

        if <fs_saida>-qtde_atribuida is initial and vg_atrib_qtde > 0 .

          <fs_saida>-qtde_atribuida = cond #( when ( <fs_saida>-saldo_nf - vg_atrib_qtde ) < 0 then <fs_saida>-saldo_nf
                                              when ( <fs_saida>-saldo_nf - vg_atrib_qtde ) >= 0 then  vg_atrib_qtde  ).

          vl_saldo_atender = vl_saldo_atender - <fs_saida>-saldo_nf.
          vg_atrib_qtde    = vg_atrib_qtde    - abs( <fs_saida>-saldo_nf ).

        endif.

        lines = lines - 1.

        if lines eq 0 and abs( vg_atrib_qtde ) > 0.
          clear: wa_saida_0100.
          move-corresponding <fs_saida> to wa_saida_0100.
          wa_saida_0100-saldo_nf = abs( vg_atrib_qtde ).
          wa_saida_0100-qtde_atribuida = 0.
          wa_saida_0100-docnum_retorno = space.
          wa_saida_0100-nferet_quebra = space.
          wa_saida_0100-docdat_quebra = space.
          append wa_saida_0100 to it_saida_0100.
        elseif lines eq 0 and abs( vg_atrib_qtde ) < ( <fs_saida>-saldo_nf ) .
          exit.
        endif.

      endif.

    endloop.

    clear: vg_atrib_qtde.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_REGRAS_ZSDT0283
*&---------------------------------------------------------------------*
form f_valida_regras_zsdt0283 changing iv_erro type c ws_zsdt0283 type zsdt0283.

  data: lit_zsdt0283     type table of zsdt0283,
        lit_zsdt0283_aux type table of zsdt0283,
        lwa_zsdt0283     type zsdt0283.

  clear: iv_erro, ws_zsdt0283.

  select *
    from zsdt0283
    into table lit_zsdt0283_aux
    where bukrs      eq p_bukrs
      and finalidade eq p_final.
  if sy-subrc is not initial.
    iv_erro = abap_true.
    concatenate 'Erro: não existe regra cadastrada para a empresa:'
                 p_bukrs
                 'finalidade:'
                 p_final into data(lv_mensagem) separated by space.
    message lv_mensagem type 'S' display like 'E'.
  else.

    sort: lit_zsdt0283_aux by bukrs matkl finalidade prazo.

    loop at it_sel_rows into data(lwa_rows).

      read table it_saida_0100 assigning field-symbol(<fs_saida_0100>) index lwa_rows-index.
      if sy-subrc is initial.
        lwa_zsdt0283-bukrs      = <fs_saida_0100>-bukrs.
        lwa_zsdt0283-finalidade = p_final.
        lwa_zsdt0283-matkl      = <fs_saida_0100>-matkl.

        read table lit_zsdt0283_aux with key bukrs      = <fs_saida_0100>-bukrs
                                             matkl      = <fs_saida_0100>-matkl
                                             finalidade = p_final
                                             transporting no fields
                                             binary search.
        if sy-subrc is initial.
          loop at lit_zsdt0283_aux into data(lwa_0283_aux) from sy-tabix.
            if lwa_0283_aux-bukrs      <> <fs_saida_0100>-bukrs or
               lwa_0283_aux-matkl      <> <fs_saida_0100>-matkl or
               lwa_0283_aux-finalidade <> p_final.
              exit.
            endif.
            "Verifica qual linha da tabela encontrou
            if lwa_0283_aux-prazo = 'D'.
              "Verifica se o registro está dentro do prazo
              if <fs_saida_0100>-dias <= lwa_0283_aux-dias.
                move-corresponding lwa_0283_aux to lwa_zsdt0283.
              endif.
            elseif lwa_0283_aux-prazo = 'F'.
              "Verifica se o registro está fora do prazo
              if <fs_saida_0100>-dias > lwa_0283_aux-dias.
                move-corresponding lwa_0283_aux to lwa_zsdt0283.
              endif.
            endif.
          endloop.
        else.
          iv_erro = abap_true.
          concatenate 'Erro: não existe regra cadastrada para a empresa:'
                      <fs_saida_0100>-bukrs
                      'grupo de mercadoria:'
                      <fs_saida_0100>-matkl
                      'finalidade:'
                      p_final into lv_mensagem separated by space.
          message lv_mensagem type 'S' display like 'E'.
        endif.
        append lwa_zsdt0283 to lit_zsdt0283.
      endif.
    endloop.

    sort: lit_zsdt0283.
    delete adjacent duplicates from lit_zsdt0283 comparing all fields.

    "Se exister mais de um registro na tabela quer dizer que existem
    "mais de 1 regra, o que faz com que o processamento não possa ser
    "executado.
    describe table lit_zsdt0283 lines data(lv_lines).
    if lv_lines > 1.
      loop at lit_zsdt0283 into data(lwa_0283).

        loop at lit_zsdt0283 into data(lwa_0283_matkl) where matkl <> lwa_0283-matkl.
          concatenate 'Erro: Duas regras de parametrização. Foram selecionados grupos de mercadorias diferentes:'
                      lwa_0283-matkl
                      'e:'
                      lwa_0283_matkl-matkl
                      into lv_mensagem separated by space.
          message lv_mensagem type 'S' display like 'E'.
          exit.
        endloop.

        loop at lit_zsdt0283 into data(lwa_0283_prazo) where prazo <> lwa_0283-prazo.
          message 'Erro: Duas regras de parametrização. Existem linhas "Dentro" e "Fora" do prazo.'type 'S' display like 'E'.
          exit.
        endloop.

      endloop.


      iv_erro = abap_true.

    else.
      read table lit_zsdt0283 into ws_zsdt0283 index 1.
      if sy-subrc is initial.
        call function 'ZSDF_SET_DENTRO_FORA_PRAZO'
          exporting
            i_prazo = lwa_0283-prazo.
      endif.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  F_GERAR_SEFAZ
*&---------------------------------------------------------------------*
form f_gerar_sefaz .

  call method obj_alv_0100->get_selected_rows
    importing
      et_index_rows = it_sel_rows.

  if it_sel_rows[] is initial.
    message 'Selecione pelo menos uma linha!' type 'S'.
    exit.
  endif.

  free: it_row.
  loop at it_sel_rows into wa_sel_rows.
    append wa_sel_rows to it_row.
  endloop.

  loop at it_sel_rows into wa_sel_rows.
    read table it_saida_0100 assigning field-symbol(<fs_saida_0100>) index wa_sel_rows-index.
    if sy-subrc is initial.
      if <fs_saida_0100>-docnum_znfw is initial.
        message 'O campo DOCNUM ZNF não foi gerado, verificar seu status e atualizar o relatório' type 'I'.
      else.
        select single * from j_1bnfe_active into @data(wl_active_doc)
                          where docnum eq @<fs_saida_0100>-docnum_znfw
                            and docsta eq '1'
                            and cancel eq @abap_false.
        if sy-subrc ne 0.
          perform f_enviar_sefaz using <fs_saida_0100>-docnum_znfw.
        else.
          <fs_saida_0100>-nfenum_znfw  = wl_active_doc-nfnum9.
        endif.
      endif.
    endif.
  endloop.
endform.

form f_set_status_registro changing c_saida type ty_saida_0100.


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
