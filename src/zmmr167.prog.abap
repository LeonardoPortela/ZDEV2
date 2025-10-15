*&---------------------------------------------------------------------*
*& Report  ZMMR167
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zmmr167.

tables: mseg.

data: git_zmmt0145         type table of zmmt0145,
      git_mseg             type table of mseg,
      git_zsdt_depara_depo type table of zsdt_depara_depo,
      git_zsdt0001         type table of zsdt0001,
      git_zsdt0001_aux     type table of zsdt0001,
      git_zsdt0023         type table of zsdt0023,
      git_zfiwrt0008       type table of zfiwrt0008,
      git_mkpf             type table of mkpf,
      git_lfa1             type table of lfa1,
      git_kna1             type table of kna1,
*      git_j_1bbranch       type table of j_1bbranch,
      git_makt             type table of makt,
      git_zlest0147        type table of zlest0147,
      git_zlest0168        type table of zlest0168.

data: gwa_zmmt0145         type zmmt0145,
      gwa_mseg             type mseg,
      gwa_zsdt_depara_depo type zsdt_depara_depo,
      gwa_zsdt0001         type zsdt0001,
      gwa_zsdt0023         type zsdt0023,
      gwa_zfiwrt0008       type zfiwrt0008,
      gwa_mkpf             type mkpf,
      gwa_kna1             type kna1,
      gwa_lfa1             type lfa1,
      gwa_lfa1_01          type lfa1,
*      gwa_j_1bbranch       type j_1bbranch,
      gwa_vbfa             type vbfa,
      gwa_makt             type makt,
      gwa_vbrp             type vbrp,
      gwa_j_1bnflin        type j_1bnflin,
      ws_active            type j_1bnfe_active,
      gwa_j_1bnflin_01     type j_1bnflin,
      gwa_zsdt_retlote     type zsdt_retlote,
      gwa_zsdt_export      type zsdt_export,
      gwa_zlest0146        type zlest0146,
      gwa_zfiwrt0015       type zfiwrt0015,
      gwa_zmmt_ee_zgr_docs type zmmt_ee_zgr_docs,
      gwa_zmmt_ee_zgr      type zmmt_ee_zgr,
      gw_zlest0039         type zlest0039,
      gw_zlest0041         type zlest0041,
      gw_zsdt0168          type zsdt0168,
      gw_zlest0186         type zlest0186,
      gv_doc_rateio        type char1.


data: gr_werks type range of mseg-werks,
      gw_werks like line of gr_werks,
      gr_matnr type range of mseg-matnr,
      gw_matnr like line of gr_matnr.


data: vg_job         type i.
data: lva_data_cons  type sy-datum.
data: lt_values      like rgsb4 occurs 0 with header line.
data: vg_date       type sy-datum,
      vg_budat_mkpf type sy-datum,
      vg_cpudt_mkpf type sy-datum.

data: vg_series3     type zsdt0001-series,
      vg_ref_number3 like j_1binterf-xblnr,
      vg_nfnum3      like  j_1bnfdoc-nfenum,
      vg_id_cli_dest type zsdt0001-id_cli_dest,
      vg_chave       type zib_nfe_dist_ter-chave_nfe,
      v_len          type i.

selection-screen begin of block b1.
  select-options: s_werks for mseg-werks no intervals,
                  s_budat for mseg-budat_mkpf.

  parameters:     p_roma  type char1 default 'X'.
  parameters:     p_porto type char1 default 'X'.
  parameters:     p_ajuste type char1 default ' '.
  parameters:     p_emp type char1 default ' '.
selection-screen end of block b1.


start-of-selection.
  if p_ajuste = 'X'.
    "Ajuste esoque especial
*    select *
*    from zmmt0145
*    inner join mseg
*    on  mseg~mblnr = zmmt0145~mb_mblnr
*    and mseg~mjahr = zmmt0145~mb_gjahr
*    and mseg~zeile = zmmt0145~mb_zeile
*    and mseg~sobkz <> ''
*    into corresponding fields of table git_mseg
*    where zmmt0145~mb_werks in s_werks
*    and   zmmt0145~mb_budat in s_budat.
*
*    select *
*      from zmmt0145
*      into table @data(it_zmmt0145_esp)
*      for all entries in @git_mseg
*      where mb_mblnr = @git_mseg-mblnr
*      and   mb_gjahr = @git_mseg-mjahr
*      and   mb_zeile = @git_mseg-zeile.
*
*    sort git_mseg by mblnr mjahr zeile.
*    loop at it_zmmt0145_esp into gwa_zmmt0145.
*      read table git_mseg into gwa_mseg with key mblnr = gwa_zmmt0145-mb_mblnr
*                                                 mjahr = gwa_zmmt0145-mb_gjahr
*                                                 zeile = gwa_zmmt0145-mb_zeile binary search.
*      if sy-subrc = 0.
*        gwa_zmmt0145-mb_sobkz = gwa_mseg-sobkz.
*        append gwa_zmmt0145 to git_zmmt0145.
*      endif.
*    endloop.
*
*    if git_zmmt0145[] is not initial.
*      modify zmmt0145 from table git_zmmt0145.
*      commit work.
*    endif.
                                                            ""IR085921
    select *
      from zmmt0145
      into table @data(it_zmmt0145_aj)
      where mb_werks in @s_werks
      and   mb_budat in @s_budat
      and   mb_porto eq @abap_true.

    check it_zmmt0145_aj[] is not initial.

    refresh  git_zmmt0145.
    loop at it_zmmt0145_aj into gwa_zmmt0145.

      if gwa_zmmt0145-mb_lgort <> ''.

        select single *
        from zsdt0023
          into gwa_zsdt0023
          where mblnr_e = gwa_zmmt0145-mb_mblnr.
        if sy-subrc = 0.
          select  single *
            from vbrp into gwa_vbrp
            where vgbel eq gwa_zsdt0023-vbeln
             and  vgtyp eq 'J' and draft = space .
          clear gwa_j_1bnflin.
          if sy-subrc eq 0.
            select single *
              from j_1bnflin into gwa_j_1bnflin
              where refkey eq gwa_vbrp-vbeln
              and   reftyp eq 'BI'.
            if sy-subrc = 0.
              select single *
              from  zlest0039 into gw_zlest0039
              where docnum eq  gwa_j_1bnflin-docnum.
              if sy-subrc = 0.
                select single *
                  from zlest0186
                  into gw_zlest0186
                  where chave = gw_zlest0039-chave_nfe.
                if sy-subrc = 0.
                  select single *
                    from zsdt0168
                    into gw_zsdt0168
                    where codigo_ra = gw_zlest0186-codigo_ra.
                  gwa_zmmt0145-dt_recepcao_portal = gw_zlest0186-dt_recepcao.
                  gwa_zmmt0145-term_cct_portal    = gw_zsdt0168-lifnr.
                  append gwa_zmmt0145 to git_zmmt0145.
                endif.
              endif.
            endif.
          endif.
        else.

          select single *
              from  zlest0039 into gw_zlest0039
            where docnum eq  gwa_zmmt0145-ro_docnum.

          select single *
              from  zlest0041 into gw_zlest0041
            where docnum eq  gwa_zmmt0145-ro_docnum.

          select single *
             into @data(w_j_1bnfdoc2)
             from j_1bnfdoc
            where  branch = @gw_zlest0041-centro_comprador
            and    parid  = @gw_zlest0041-cod_cliente
            and    nfenum = @gw_zlest0041-nr_nf.

          if sy-subrc = 0.
            select single *
              from  j_1bnfe_active
              into ws_active
              where docnum = w_j_1bnfdoc2-docnum.

            if sy-subrc = 0.
              gw_zlest0039-chave_nfe =  ws_active-regio
                                        && ws_active-nfyear
                                        && ws_active-nfmonth
                                        && ws_active-stcd1
                                        && ws_active-model
                                        && ws_active-serie
                                        && ws_active-nfnum9
                                        && ws_active-docnum9
                                        && ws_active-cdv.


              select single *
                from zlest0186
                into gw_zlest0186
                where chave = gw_zlest0039-chave_nfe.
              if sy-subrc = 0.
                select single *
                  from zsdt0168
                  into gw_zsdt0168
                  where codigo_ra = gw_zlest0186-codigo_ra.
                gwa_zmmt0145-dt_recepcao_portal_p = gw_zlest0186-dt_recepcao.
                gwa_zmmt0145-term_cct_portal_p    = gw_zsdt0168-lifnr.
                append gwa_zmmt0145 to git_zmmt0145.
              endif.
            endif.

          endif.
        endif.
      endif.
    endloop.
    check git_zmmt0145 is not initial.
    modify zmmt0145 from table git_zmmt0145.
    commit work.

  else.
    select single count( * ) into vg_job
      from tbtco
     where jobname eq 'MAGGI_ZMMR167'
       and status eq 'R'.

    if p_emp is not initial.
      "AGRO/OTELHAR
      select *
        from  t001w into table @data(tg_t001w)
        where werks like '15%'.

      select *
        from  t001w appending
        table tg_t001w
        where werks like '50%'.

      s_werks-sign   = 'I'.
      s_werks-option = 'EQ'.
      loop at tg_t001w into data(wg_t001w).
        s_werks-low    = wg_t001w-werks.
        append s_werks.
      endloop.
    else.
      if vg_job eq 1.
        clear vg_date.
        call function 'RP_CALC_DATE_IN_INTERVAL'
          exporting
            date      = sy-datum
            days      = '00'
            months    = '01'
            signum    = '-'
            years     = '00'
          importing
            calc_date = vg_date.

        s_budat-sign   = 'I'.
        s_budat-option = 'BT'.
        concatenate vg_date(6) '01' into s_budat-low.
        s_budat-high   = sy-datum.
        append s_budat.

        "AMAGGI
        select *
          from  t001w into table tg_t001w
          where werks like '01%'.

        select *
          from  t001w appending
          table tg_t001w
          where werks like '11%'.

        "AGRO/OTELHAR
        select *
          from  t001w appending
          table tg_t001w
          where werks like '15%'.

        select *
          from  t001w appending
          table tg_t001w
          where werks like '50%'.
        "
        s_werks-sign   = 'I'.
        s_werks-option = 'EQ'.
        loop at tg_t001w into wg_t001w.
          s_werks-low    = wg_t001w-werks.
          append s_werks.
        endloop.

      endif.
    endif.


    perform f_preenche_range.

    perform f_seleciona_dados.

    perform f_trata_dados.

*  PERFORM f_romaneios_s_docmaterial.

  endif.


form f_seleciona_dados.

  clear: vg_budat_mkpf,  vg_cpudt_mkpf.

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255
  select *
    from zsdt_depara_depo into table git_zsdt_depara_depo
    where werks in s_werks.

*  ZCL_DEPARA_CENTRO_FIXO_VIRTUAL=>GET_DADOS_DEPARA(
*      EXPORTING
*        I_WERKS         = CONV #( S_WERKS )
*      IMPORTING
*        E_TABLE_DEPARA   = GIT_ZSDT_DEPARA_DEPO  ).

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  - FIM

  sort git_zsdt_depara_depo by werks_v.
  delete adjacent duplicates from git_zsdt_depara_depo comparing werks_v.
  loop at git_zsdt_depara_depo into gwa_zsdt_depara_depo.
    clear: gw_werks.
    gw_werks-sign   = 'I'.
    gw_werks-option = 'EQ'.
    gw_werks-low    = gwa_zsdt_depara_depo-werks_v.
    append gw_werks to gr_werks.
    clear: gw_werks, gwa_zsdt_depara_depo.
  endloop.

*
*  select *
*    from j_1bbranch into table git_j_1bbranch
*    where branch = '0101'. "EMPRESA 0001  FIXA

  select *
    from makt into table git_makt
    where spras eq sy-langu
    and   matnr in gr_matnr.

  select *
    from zsdt0001 into table git_zsdt0001
    where dt_movimento in s_budat
    and   bukrs  in ( '0001', '0015', '0050' )
    and   branch in s_werks.

  if vg_job  eq 1 and  p_emp is initial.

    vg_budat_mkpf  = sy-datum - 1.
    vg_cpudt_mkpf  = sy-datum - 10.

    select *
     from mseg into table   git_mseg
     where budat_mkpf in s_budat
     and   cpudt_mkpf  > vg_cpudt_mkpf
     and   werks      in gr_werks
     and   matnr      in gr_matnr.

    loop at git_mseg into gwa_mseg.
      if gwa_mseg-budat_mkpf = gwa_mseg-cpudt_mkpf.
        gwa_mseg-charg = 'APAGAR'.
        modify git_mseg from gwa_mseg index sy-tabix transporting charg.
      endif.
    endloop.
    delete git_mseg where charg = 'APAGAR'.

    select *
      from mseg appending table git_mseg
      where budat_mkpf eq vg_budat_mkpf
      and   werks      in gr_werks
      and   matnr      in gr_matnr.

  else.

    select *
      from mseg into table git_mseg
      where budat_mkpf in s_budat
      and   werks      in gr_werks
      and   matnr      in gr_matnr.

  endif.

  if sy-subrc eq 0.
    select *
      from mkpf into table git_mkpf
      for all entries in git_mseg
     where mblnr eq  git_mseg-mblnr
      and  mjahr eq  git_mseg-mjahr.

    select  *
        from zsdt0023 into table git_zsdt0023
        for all entries in git_mseg
        where mblnr_e eq git_mseg-mblnr.

    select  *
        from zfiwrt0008 into table git_zfiwrt0008
        for all entries in git_mseg
        where mblnr eq git_mseg-mblnr.

    select *
    from lfa1 into table git_lfa1
    for all entries in   git_mseg
   where lifnr eq git_mseg-lifnr.

    select *
       from kna1 into table git_kna1
       for all entries in   git_mseg
      where kunnr eq git_mseg-kunnr.

  endif.

endform.

form f_trata_dados.

  data: vg_branch     type zsdt0001-branch,
        vg_parid      type zsdt0001-parid,
        vg_parid2     type zsdt0001-parid,
        vg_vbeln      type zmmt_ee_zgr_docs-av_vbeln,
        vg_nfnum      like  j_1bnfdoc-nfenum,
        vg_series     type zsdt0001-series,
        vg_series2    type zsdt0001-series,
        vg_ref_number like j_1binterf-xblnr.

  git_zsdt0001_aux[] = git_zsdt0001[].
  sort: git_zsdt0001_aux by ch_referencia,
        git_zsdt0001 by bukrs branch parid nfnum peso_fiscal series,
        git_mkpf     by mblnr mjahr,
        git_lfa1     by lifnr,
        git_kna1     by kunnr,
        git_makt     by matnr,
        git_zfiwrt0008   by mblnr,
        git_zsdt0023 by mblnr_e.

  "Estoque 3 na filial
  select bwart
    from zmmt0083
    into table @data(it_083)
    where code = 'ZMM0177' and cd_agru = '0017'
    order by bwart.

  select bwart
      from zmmt0083
      into table @data(it_083_fiscal)
      where code = 'ZMM0177' and ck_fiscal = 'X'
      order by bwart.

  loop at git_mseg into gwa_mseg.
    gwa_zmmt0145-mb_mblnr = gwa_mseg-mblnr.
    gwa_zmmt0145-mb_gjahr = gwa_mseg-mjahr.
    gwa_zmmt0145-mb_zeile = gwa_mseg-zeile.
    gwa_zmmt0145-mb_werks = gwa_mseg-werks.
    gwa_zmmt0145-mb_gsber = gwa_mseg-gsber.
    gwa_zmmt0145-mb_kunnr = gwa_mseg-kunnr.
    gwa_zmmt0145-mb_lifnr = gwa_mseg-lifnr.
    gwa_zmmt0145-mb_bwart = gwa_mseg-bwart.
    gwa_zmmt0145-mb_lgort = gwa_mseg-lgort.
    gwa_zmmt0145-mb_matnr = gwa_mseg-matnr.
    gwa_zmmt0145-mb_budat = gwa_mseg-budat_mkpf.
    gwa_zmmt0145-mb_xblnr = gwa_mseg-xblnr_mkpf.
    gwa_zmmt0145-mb_menge = gwa_mseg-menge.
    gwa_zmmt0145-mb_charg = gwa_mseg-charg.
    gwa_zmmt0145-mb_ebeln = gwa_mseg-ebeln.
    gwa_zmmt0145-mb_sobkz = gwa_mseg-sobkz.

    read table git_makt into gwa_makt with key matnr = gwa_mseg-matnr binary search.
    if sy-subrc eq 0.
      gwa_zmmt0145-mb_maktx = gwa_makt-maktx.
    endif.

    read table git_mkpf into gwa_mkpf with key mblnr = gwa_mseg-mblnr
                                               mjahr = gwa_mseg-mjahr binary search.
    if sy-subrc = 0.
      gwa_zmmt0145-mb_bktxt = gwa_mkpf-bktxt.
      gwa_zmmt0145-mb_bldat = gwa_mkpf-bldat.
      gwa_zmmt0145-mb_xblnr = gwa_mkpf-xblnr.
    endif.

    read table git_lfa1 into gwa_lfa1 with key lifnr = gwa_mseg-lifnr binary search.
    if sy-subrc eq 0.
      gwa_zmmt0145-mb_name1 = gwa_lfa1-name1.
    endif.
    read table git_kna1 into gwa_kna1 with key kunnr = gwa_mseg-kunnr binary search.
    if sy-subrc eq 0.
      gwa_zmmt0145-mb_name1 = gwa_kna1-name1.
    endif.

    if p_roma  = 'X' and
       not ( 'ZX1_ZX2_ZP2_301_302' cs gwa_mseg-bwart ). "ZX sobra porto,desconsiderar romaneio
      if gwa_mseg-bwart+0(1) = 'Y'. "Exportação sem romaneio
        gwa_zmmt0145-ro_peso_liq       = gwa_mseg-menge.
        gwa_zmmt0145-ro_peso_fiscal    = gwa_mseg-menge.
      endif.
*      read table git_j_1bbranch into gwa_j_1bbranch with key branch = '0101'.
      v_len = strlen( gwa_mkpf-xblnr ).
      if gwa_mseg-lifnr ne ' ' and  gwa_mkpf-xblnr ne ' ' and gwa_mkpf-xblnr ca '-' and v_len gt 2.

        clear: vg_branch, vg_parid, vg_series,vg_parid2, vg_series2, vg_nfnum, vg_ref_number.

*        concatenate gwa_j_1bbranch-bukrs+2(2) gwa_mseg-werks+2(2) into vg_branch.
        concatenate gwa_mseg-bukrs+2(2) gwa_mseg-werks+2(2) into vg_branch.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = vg_branch
          importing
            output = vg_parid2.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = gwa_mseg-lifnr
          importing
            output = vg_parid.

        vg_ref_number = gwa_mkpf-xblnr.

        call function 'J_1B_NF_NUMBER_SEPARATE'
          exporting
            ref_number   = vg_ref_number
            i_nfeflag    = 'X'
          importing
            series       = vg_series
            nf_number9   = vg_nfnum
          exceptions
            number_error = 1
            others       = 2.

        gwa_zmmt0145-mb_nfnum  = vg_nfnum.
        gwa_zmmt0145-mb_series = vg_series.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = vg_series
          importing
            output = vg_series2.

        read table git_zsdt0001 into gwa_zsdt0001 with key bukrs       = gwa_mseg-bukrs
                                                           branch      = vg_branch
                                                           parid       = vg_parid "fornecedor
                                                           nfnum       = vg_nfnum
                                                           peso_fiscal = gwa_mseg-menge binary search.
        if sy-subrc ne 0.
          read table git_zsdt0001 into gwa_zsdt0001 with key bukrs     = gwa_mseg-bukrs
                                                           branch      = vg_branch
                                                           parid       = vg_parid2 "filial
                                                           nfnum       = vg_nfnum
                                                           peso_fiscal = gwa_mseg-menge binary search.
        endif.

        if sy-subrc eq 0.
          perform f_preenche_zsdt0001 using gwa_zsdt0001.
        endif.

        if gwa_zmmt0145-ro_nr_romaneio is initial.
          select single *
           from zmmt_ee_zgr_docs into gwa_zmmt_ee_zgr_docs
           where mm_mblnr eq gwa_mseg-mblnr.

          if sy-subrc eq 0.
            select single *
              from zmmt_ee_zgr into gwa_zmmt_ee_zgr
              where obj_key eq gwa_zmmt_ee_zgr_docs-obj_key.

            if gwa_zmmt_ee_zgr-entry_qnt = gwa_mseg-menge and  gwa_zmmt_ee_zgr-ch_referencia is not initial.
              read table git_zsdt0001_aux into gwa_zsdt0001 with key ch_referencia = gwa_zmmt_ee_zgr-ch_referencia binary search.
              if sy-subrc ne 0.
                select single *
                  from zsdt0001 into gwa_zsdt0001
                  where ch_referencia eq gwa_zmmt_ee_zgr-ch_referencia.
              endif.

              if sy-subrc eq 0.
                perform f_preenche_zsdt0001 using gwa_zsdt0001.
              endif.
            endif.
          endif.
        endif.
        "tabela física
        if gwa_zmmt0145-ro_nr_romaneio is initial. " AND '101_102' CS gwa_mseg-bwart.
          select single *
           from zsdt0001 into gwa_zsdt0001
          where bukrs       eq gwa_mseg-bukrs
           and  branch      eq vg_branch
           and  parid       in ( vg_parid , vg_parid2 )
           and  nfnum       eq vg_nfnum
           and  series      in ( vg_series, vg_series2 )
           and  peso_fiscal eq gwa_mseg-menge.
          if sy-subrc eq 0.
            perform f_preenche_zsdt0001 using gwa_zsdt0001.
          endif.
        endif.
      endif.

      if gwa_zmmt0145-ro_nr_romaneio is initial.

        if gwa_mseg-lifnr ne ' '  and gwa_mkpf-xblnr ca '0123456789'.

          select single *
            from zmmt_ee_zgr_docs into gwa_zmmt_ee_zgr_docs
            where mm_mblnr eq gwa_mseg-mblnr.

          if sy-subrc eq 0.
            select single *
              from zmmt_ee_zgr into gwa_zmmt_ee_zgr
              where obj_key eq gwa_zmmt_ee_zgr_docs-obj_key.

            if gwa_zmmt_ee_zgr-entry_qnt = gwa_mseg-menge and  gwa_zmmt_ee_zgr-ch_referencia is not initial.
              read table git_zsdt0001_aux into gwa_zsdt0001 with key ch_referencia = gwa_zmmt_ee_zgr-ch_referencia binary search.
              if sy-subrc ne 0.
                select single *
                  from zsdt0001 into gwa_zsdt0001
                  where ch_referencia eq gwa_zmmt_ee_zgr-ch_referencia.
              endif.

              if sy-subrc eq 0.
                perform f_preenche_zsdt0001 using gwa_zsdt0001.
              endif.

              clear: vg_series,vg_nfnum.
              vg_ref_number = gwa_zmmt_ee_zgr-ref_doc_no.

              call function 'J_1B_NF_NUMBER_SEPARATE'
                exporting
                  ref_number   = vg_ref_number
                  i_nfeflag    = 'X'
                importing
                  series       = vg_series
                  nf_number9   = vg_nfnum
                exceptions
                  number_error = 1
                  others       = 2.

              gwa_zmmt0145-mb_nfnum  = vg_nfnum.
              gwa_zmmt0145-mb_series = vg_series.
            endif.
          endif.
        endif.
      endif.

      if gwa_zmmt0145-ro_nr_romaneio is initial.
        if gwa_mseg-lifnr ne ' '  and gwa_mkpf-xblnr ca '0123456789'.
          vg_vbeln = gwa_mkpf-xblnr+0(10).
          select single *
            from zmmt_ee_zgr_docs into gwa_zmmt_ee_zgr_docs
            where av_vbeln eq vg_vbeln.

          if sy-subrc eq 0.
            select single *
              from zmmt_ee_zgr into gwa_zmmt_ee_zgr
              where obj_key eq gwa_zmmt_ee_zgr_docs-obj_key.

            if gwa_zmmt_ee_zgr-entry_qnt = gwa_mseg-menge and  gwa_zmmt_ee_zgr-ch_referencia is not initial.
              read table git_zsdt0001_aux into gwa_zsdt0001 with key ch_referencia = gwa_zmmt_ee_zgr-ch_referencia binary search.
              if sy-subrc ne 0.
                select single *
                  from zsdt0001 into gwa_zsdt0001
                  where ch_referencia eq gwa_zmmt_ee_zgr-ch_referencia.
              endif.

              if sy-subrc eq 0.
                perform f_preenche_zsdt0001 using gwa_zsdt0001.
              endif.
              "
              clear: vg_series,vg_nfnum.
              vg_ref_number = gwa_zmmt_ee_zgr-ref_doc_no.

              call function 'J_1B_NF_NUMBER_SEPARATE'
                exporting
                  ref_number   = vg_ref_number
                  i_nfeflag    = 'X'
                importing
                  series       = vg_series
                  nf_number9   = vg_nfnum
                exceptions
                  number_error = 1
                  others       = 2.

              gwa_zmmt0145-mb_nfnum  = vg_nfnum.
              gwa_zmmt0145-mb_series = vg_series.
            endif.
          endif.
        endif.
      endif.

      if gwa_zmmt0145-ro_nr_romaneio is initial.
        "SAIDA FORMAÇÃO DE LOTE
        if gwa_mkpf-bktxt ne '' and gwa_mkpf-bktxt ca '0123456789'.
          vg_vbeln = gwa_mkpf-bktxt+0(10).
          select single *  from zsdt0023 into gwa_zsdt0023
           where vbeln eq vg_vbeln.

          if sy-subrc eq 0.
            clear  gwa_zsdt0001.
            read table git_zsdt0001_aux into gwa_zsdt0001 with key ch_referencia = gwa_zsdt0023-ch_referencia binary search.
            if sy-subrc ne 0.
              select single *
                from zsdt0001 into gwa_zsdt0001
                where ch_referencia eq gwa_zsdt0023-ch_referencia.
            endif.

            if sy-subrc eq 0.
              perform f_preenche_zsdt0001 using gwa_zsdt0001.
            endif.
          endif.
        endif.
      endif.

      if gwa_zmmt0145-ro_nr_romaneio is initial.
        clear  gwa_zsdt0001.

        select single *
          from zsdt0001 into gwa_zsdt0001
          where doc_material eq gwa_mseg-mblnr
          and   ano_material eq gwa_mseg-mjahr.

        if sy-subrc eq 0.
          perform f_preenche_zsdt0001 using gwa_zsdt0001.
        endif.
      endif.

      if gwa_zmmt0145-ro_nr_romaneio is initial and gwa_mkpf-bktxt+0(7) eq 'CHV_ROM'.
        clear  gwa_zsdt0001.
        data vg_ch_referencia type zsdt0001-ch_referencia.
        vg_ch_referencia = gwa_mkpf-bktxt+8(15).
        select single *
          from zsdt0001 into gwa_zsdt0001
          where ch_referencia = vg_ch_referencia.

        if sy-subrc eq 0.
          perform f_preenche_zsdt0001 using gwa_zsdt0001.
        endif.
      endif.

      if gwa_zmmt0145-ro_nr_romaneio is initial.
        clear  gwa_zsdt0001.
*        concatenate gwa_j_1bbranch-bukrs+2(2) gwa_mseg-werks+2(2) into vg_branch.
        concatenate gwa_mseg-bukrs+2(2) gwa_mseg-werks+2(2) into vg_branch.

        case gwa_mseg-bwart.
          when '862'.
            if gwa_mkpf-xblnr ca '-'.
              vg_ref_number = gwa_mkpf-xblnr.
              call function 'J_1B_NF_NUMBER_SEPARATE'
                exporting
                  ref_number   = vg_ref_number
                  i_nfeflag    = 'X'
                importing
                  series       = vg_series
                  nf_number9   = vg_nfnum
                exceptions
                  number_error = 1
                  others       = 2.

              gwa_zmmt0145-mb_nfnum  = vg_nfnum.
              gwa_zmmt0145-mb_series = vg_series.
            endif.

            select single *
              from zsdt0001 into gwa_zsdt0001
              where fatura_prod eq gwa_mseg-mblnr
              and   branch      eq vg_branch.

            if sy-subrc eq 0.
              gwa_zsdt0001-ds_obs = '862'.
              perform f_preenche_zsdt0001 using gwa_zsdt0001.
            endif.

          when '861'.
            select single *
              from vbfa into gwa_vbfa
              where vbelv   eq gwa_mseg-vbeln_im
              and   vbtyp_n eq 'R'
              and   vbtyp_v eq 'J'.

            if sy-subrc eq 0.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = vg_branch
                importing
                  output = vg_id_cli_dest.
              gwa_zmmt0145-mb_xblnr = gwa_mseg-vbeln_im.
              select single *
               from zsdt0001 into gwa_zsdt0001
               where fatura_prod eq gwa_vbfa-vbeln
               and  id_cli_dest  eq vg_id_cli_dest.

              if sy-subrc eq 0.
                perform f_preenche_zsdt0001 using gwa_zsdt0001.
              endif.
            endif.
        endcase.
      endif.
    endif.

    perform f_porto.

    gwa_zmmt0145-mb_es = 'E'.
    if gwa_mseg-shkzg eq 'H'.
      gwa_zmmt0145-mb_es = 'S'.
      gwa_zmmt0145-mb_menge       = gwa_zmmt0145-mb_menge       * -1.
      gwa_zmmt0145-ro_peso_liq    = gwa_zmmt0145-ro_peso_liq    * -1.
      gwa_zmmt0145-ro_peso_fiscal = gwa_zmmt0145-ro_peso_fiscal * -1.
      "
      read table it_083 into data(w_083) with key bwart = gwa_zmmt0145-mb_bwart binary search.
      if sy-subrc = 0.
        gwa_zmmt0145-ro_peso_liq = gwa_zmmt0145-mb_menge.
      endif.

    endif.
                                                            "IR085921
    read table it_083_fiscal into data(w_083_fiscal) with key bwart = gwa_zmmt0145-mb_bwart binary search.
    if sy-subrc = 0.
      gwa_zmmt0145-ro_peso_liq = gwa_zmmt0145-mb_menge.
    endif.

*    IF ( 'Z43_Z44_701_702_301_302' CS gwa_zmmt0145-mb_bwart ).
*      gwa_zmmt0145-ro_peso_liq = gwa_zmmt0145-mb_menge.
*    ENDIF.
                                                            "IR085921
    append gwa_zmmt0145 to git_zmmt0145.
*    clear: gwa_zmmt0145, gwa_zsdt0001, gwa_mseg, gwa_mkpf, gwa_lfa1, gwa_j_1bbranch, vg_branch, vg_parid, vg_series, vg_nfnum.
    clear: gwa_zmmt0145, gwa_zsdt0001, gwa_mseg, gwa_mkpf, gwa_lfa1, vg_branch, vg_parid, vg_series, vg_nfnum.
  endloop.

  check git_zmmt0145 is not initial.
  modify zmmt0145 from table git_zmmt0145.
  commit work.

  if p_emp is initial.
    "Checar  Transito porto
    refresh: git_zmmt0145, it_zmmt0145_aj.
    select *
      from zmmt0145
      into table it_zmmt0145_aj
      where mb_transito eq  abap_true
      and   mb_budat ge '20210101'.

    check it_zmmt0145_aj[] is not initial.

    select  *
      from zsdt0023 into table git_zsdt0023
      for all entries in it_zmmt0145_aj
      where mblnr_e eq it_zmmt0145_aj-mb_mblnr.

    sort git_zsdt0023 by mblnr_e.

    loop at it_zmmt0145_aj into gwa_zmmt0145.
      gwa_mseg-werks = gwa_zmmt0145-mb_werks.
      gwa_mseg-mblnr = gwa_zmmt0145-mb_mblnr.
      gwa_mseg-lgort = gwa_zmmt0145-mb_lgort.
      "
      perform f_porto.
      append gwa_zmmt0145 to git_zmmt0145.
    endloop.
    check git_zmmt0145 is not initial.
    modify zmmt0145 from table git_zmmt0145.
    commit work.
  endif.


endform.


form f_porto.
  if p_porto = 'X'.
    clear: gwa_zmmt0145-mb_disponivel,gwa_zmmt0145-mb_transito.
    read table git_zsdt_depara_depo into gwa_zsdt_depara_depo with key werks_v = gwa_mseg-werks binary search.
    if sy-subrc eq 0.
      gwa_zmmt0145-mb_porto = abap_true.
      if gwa_mseg-lgort <> ''.
        read table git_zsdt0023 into gwa_zsdt0023 with key mblnr_e = gwa_mseg-mblnr binary search.
        if sy-subrc eq 0.
          select  single *
            from vbrp into gwa_vbrp
            where vgbel eq gwa_zsdt0023-vbeln
             and  vgtyp eq 'J' and draft = space .

          clear gwa_j_1bnflin.
          if sy-subrc eq 0.
            select single *
              from j_1bnflin into gwa_j_1bnflin
              where refkey eq gwa_vbrp-vbeln
              and   reftyp eq 'BI'.
          endif.

          if sy-subrc eq 0 and gwa_j_1bnflin-docnum is not initial.
            clear: gw_zlest0039, gwa_zlest0146, gwa_zsdt_retlote, gwa_zsdt_export.
            call function 'ZCCT_DADOS_RECEPCAO_CARGA'
              exporting
                i_docnum     = gwa_j_1bnflin-docnum
                i_itmnum     = gwa_j_1bnflin-itmnum
              importing
                e_zlest0146  = gwa_zlest0146
                e_zlest0147  = git_zlest0147
                e_zlest0168  = git_zlest0168
                e_doc_rateio = gv_doc_rateio.


            select single *
              from  zlest0039 into gw_zlest0039
              where docnum eq  gwa_j_1bnflin-docnum.

            if sy-subrc = 0.
              select single *
                from zlest0186
                into gw_zlest0186
                where chave = gw_zlest0039-chave_nfe.
              if sy-subrc = 0.
                select single *
                  from zsdt0168
                  into gw_zsdt0168
                  where codigo_ra = gw_zlest0186-codigo_ra.
                gwa_zmmt0145-dt_recepcao_portal = gw_zlest0186-dt_recepcao.
                gwa_zmmt0145-term_cct_portal    = gw_zsdt0168-lifnr.
              endif.
            endif.

            if gwa_zlest0146-dt_recepcao is initial.
              gwa_zmmt0145-mb_transito = abap_true.
            else.
              gwa_zmmt0145-ro_dt_descarga    = gw_zlest0039-datatransb.
              gwa_zmmt0145-mb_dt_recep_cct   = gwa_zlest0146-dt_recepcao.

              select single *
                from zsdt_retlote into gwa_zsdt_retlote
                where docnum eq gwa_j_1bnflin-docnum.

              select single *
                from zsdt_export into gwa_zsdt_export
                where docnum eq gwa_zsdt_retlote-docnum_ret.

              select single *
                from  vbfa as a
                into gwa_vbfa
                where a~vbelv  eq gwa_zsdt_export-ordem
                and   a~vbtyp_n eq 'M'
                and  a~vbtyp_v eq 'C'
               and not exists ( select *
                                from vbfa as b
                               where b~vbelv   = a~vbeln
                                 and b~vbtyp_n = 'N' ).

              select single *
                from j_1bnflin into gwa_j_1bnflin_01
                where refkey eq gwa_vbfa-vbeln.

              if sy-subrc ne 0.
                gwa_zmmt0145-mb_disponivel = abap_true.
              endif.
            endif.
          endif.

        else.
          select single *
             from  zlest0039 into gw_zlest0039
           where docnum eq  gwa_zmmt0145-ro_docnum.

          select single *
              from  zlest0041 into gw_zlest0041
            where docnum eq  gwa_zmmt0145-ro_docnum.

          select single *
             into @data(w_j_1bnfdoc3)
             from j_1bnfdoc
            where  branch = @gw_zlest0041-centro_comprador
            and    parid  = @gw_zlest0041-cod_cliente
            and    nfenum = @gw_zlest0041-nr_nf.

          if sy-subrc = 0.
            select single *
              from  j_1bnfe_active
              into ws_active
              where docnum = w_j_1bnfdoc3-docnum.

            if sy-subrc = 0.
              gw_zlest0039-chave_nfe =  ws_active-regio
                                        && ws_active-nfyear
                                        && ws_active-nfmonth
                                        && ws_active-stcd1
                                        && ws_active-model
                                        && ws_active-serie
                                        && ws_active-nfnum9
                                        && ws_active-docnum9
                                        && ws_active-cdv.
              select single *
                   from zlest0186
                   into gw_zlest0186
                   where chave = gw_zlest0039-chave_nfe.
              if sy-subrc = 0.
                select single *
                  from zsdt0168
                  into gw_zsdt0168
                  where codigo_ra = gw_zlest0186-codigo_ra.
                gwa_zmmt0145-dt_recepcao_portal_p = gw_zlest0186-dt_recepcao.
                gwa_zmmt0145-term_cct_portal_p    = gw_zsdt0168-lifnr.
              endif.
            endif.
          endif.

          clear gwa_zfiwrt0008.
          read table git_zfiwrt0008 into gwa_zfiwrt0008 with key mblnr = gwa_mseg-mblnr binary search.

          clear gwa_j_1bnflin.
          select single *
            from j_1bnflin into gwa_j_1bnflin
            where docnum eq gwa_zfiwrt0008-docnum.

          if sy-subrc eq 0 and gwa_j_1bnflin-docnum is not initial.
            if sy-subrc eq 0 and gwa_zfiwrt0008-docnum is not initial.
              clear: gw_zlest0039, gwa_zlest0146, gwa_zsdt_retlote, gwa_zsdt_export.
              call function 'ZCCT_DADOS_RECEPCAO_CARGA'
                exporting
                  i_docnum     = gwa_j_1bnflin-docnum
                  i_itmnum     = gwa_j_1bnflin-itmnum
                importing
                  e_zlest0146  = gwa_zlest0146
                  e_zlest0147  = git_zlest0147
                  e_zlest0168  = git_zlest0168
                  e_doc_rateio = gv_doc_rateio.

              select single *
                from  zlest0039 into gw_zlest0039
              where docnum eq  gwa_j_1bnflin-docnum.

              if sy-subrc = 0.
                select single *
                  from zlest0186
                  into gw_zlest0186
                  where chave = gw_zlest0039-chave_nfe.
                if sy-subrc = 0.
                  select single *
                    from zsdt0168
                    into gw_zsdt0168
                    where codigo_ra = gw_zlest0186-codigo_ra.
                  gwa_zmmt0145-dt_recepcao_portal_p = gw_zlest0186-dt_recepcao.
                  gwa_zmmt0145-term_cct_portal_p    = gw_zsdt0168-lifnr.
                endif.
              endif.


              if gwa_zlest0146-dt_recepcao is initial.
                gwa_zmmt0145-mb_transito = abap_true.
              else.
                gwa_zmmt0145-ro_dt_descarga  = gw_zlest0039-datatransb.
                gwa_zmmt0145-mb_dt_recep_cct = gwa_zlest0146-dt_recepcao.

                select single *
                  from zfiwrt0015 into gwa_zfiwrt0015
                  where seq_lcto eq gwa_zfiwrt0008-seq_lcto
                  and   parvw    eq 'Z1'.

                select single *
                  from zsdt_retlote into gwa_zsdt_retlote
                  where docnum eq  gwa_zfiwrt0008-docnum.

                select single *
                  from zsdt_export into gwa_zsdt_export
                  where docnum eq gwa_zsdt_retlote-docnum_ret.

                select single *
                  from vbfa as a
                  into gwa_vbfa
                 where a~vbelv   eq gwa_zsdt_export-ordem
                  and  a~vbtyp_n eq 'M'
                  and  a~vbtyp_v eq 'C'
                  and not exists ( select * from vbfa as b
                                   where  b~vbelv  = a~vbeln
                                      and b~vbtyp_n eq 'N' ).
                select single *
                  from j_1bnflin into gwa_j_1bnflin_01
                  where refkey eq  gwa_vbfa-vbeln.

                if sy-subrc ne 0.
                  gwa_zmmt0145-mb_disponivel = abap_true.
                endif.
              endif.
            endif.
          endif.
        endif.
      endif.
    endif.
  endif.


endform.

form f_preenche_range.

  loop at s_werks into data(w_werks).
    gw_werks-sign   = w_werks-sign.
    gw_werks-option = w_werks-option.
    gw_werks-low    = w_werks-low.
    append gw_werks to gr_werks.
    clear: gw_werks.

    if w_werks-low+0(2) = '15' or
       w_werks-low+0(2) = '50'.
      continue.
    endif.

    gw_werks-sign   = w_werks-sign.
    gw_werks-option = w_werks-option.
    if w_werks-low+0(2) = '01'.
      concatenate 'AF' w_werks-low+2(2) into gw_werks-low.
    else.
      concatenate 'MF' w_werks-low+2(2) into gw_werks-low.
    endif.
    append gw_werks to gr_werks.
    clear: gw_werks.
  endloop.


  gw_matnr-sign   = 'I'.
  gw_matnr-option = 'EQ'.
  gw_matnr-low    = '000000000000119892'.
  append gw_matnr to gr_matnr.

  gw_matnr-sign   = 'I'.
  gw_matnr-option = 'EQ'.
  gw_matnr-low    = '000000000000119891'.
  append gw_matnr to gr_matnr.

  gw_matnr-low    = '000000000000153733'.
  append gw_matnr to gr_matnr.

  gw_matnr-low    = '000000000000119894'.
  append gw_matnr to gr_matnr.

  gw_matnr-low    = '000000000000119895'.
  append gw_matnr to gr_matnr.

  gw_matnr-low    = '000000000000120102'.
  append gw_matnr to gr_matnr.
  clear gw_matnr.
endform.

form f_romaneios_s_docmaterial.

*  refresh: git_j_1bbranch, git_zmmt0145, git_zsdt0001.
  refresh: git_zmmt0145, git_zsdt0001.
  clear: gwa_zsdt0001, gwa_zmmt0145.

*  select *
*    from j_1bbranch into table git_j_1bbranch
*    where branch eq '0101'.

*  select *
*    from zsdt0001 into table git_zsdt0001
*    for all entries in git_j_1bbranch
*    where dt_movimento in s_budat
*    and   bukrs  eq git_j_1bbranch-bukrs
*    and   branch in s_werks.

  select *
    from zsdt0001 into table git_zsdt0001
    where dt_movimento in s_budat
    and   bukrs  in ( '0001', '0015', '0050' )
    and   branch in s_werks.

  select *
    from zmmt0145 into table git_zmmt0145
    for all entries in git_zsdt0001
   where ro_ch_referencia eq git_zsdt0001-ch_referencia.

  write:/   'ch_referencia' color col_heading,
            'tp_movimento' color col_heading,
            'nr_romaneio' color col_heading,
            'vbeln' color col_heading,
            'nr_ticket' color col_heading,
            'id_referencia' color col_heading,
            'id_carga' color col_heading,
            'id_ordem' color col_heading,
            'dt_movimento' color col_heading,
            'nr_safra' color col_heading,
            'bukrs' color col_heading,
            'branch' color col_heading,
            'parid' color col_heading,
            'matnr' color col_heading,
            'peso_liq' color col_heading,
            'peso_fiscal' color col_heading,
            'nfnum' color col_heading,
            'series' color col_heading.

  new-line.

  loop at git_zsdt0001 into gwa_zsdt0001.
    read table git_zmmt0145 into gwa_zmmt0145 with key ro_ch_referencia = gwa_zsdt0001-ch_referencia.
    if sy-subrc ne 0.
      write:/ gwa_zsdt0001-ch_referencia,
              gwa_zsdt0001-tp_movimento,
              gwa_zsdt0001-nr_romaneio,
              gwa_zsdt0001-vbeln,
              gwa_zsdt0001-nr_ticket,
              gwa_zsdt0001-id_referencia,
              gwa_zsdt0001-id_carga,
              gwa_zsdt0001-id_ordem,
              gwa_zsdt0001-dt_movimento,
              gwa_zsdt0001-nr_safra,
              gwa_zsdt0001-bukrs,
              gwa_zsdt0001-branch,
              gwa_zsdt0001-parid,
              gwa_zsdt0001-matnr,
              gwa_zsdt0001-peso_liq,
              gwa_zsdt0001-peso_fiscal,
              gwa_zsdt0001-nfnum,
              gwa_zsdt0001-series.
    endif.
    clear: gwa_zsdt0001, gwa_zmmt0145.
  endloop.

endform.

form f_preenche_zsdt0001 using w_zsdt0001 type zsdt0001.

  gwa_zmmt0145-ro_ch_referencia  = w_zsdt0001-ch_referencia.
  gwa_zmmt0145-ro_tp_movimento   = w_zsdt0001-tp_movimento.
  gwa_zmmt0145-ro_nr_romaneio    = w_zsdt0001-nr_romaneio.
  gwa_zmmt0145-ro_vbeln          = w_zsdt0001-vbeln.
  gwa_zmmt0145-ro_nr_ticket      = w_zsdt0001-nr_ticket.
  gwa_zmmt0145-ro_id_referencia  = w_zsdt0001-id_referencia.
  gwa_zmmt0145-ro_id_carga       = w_zsdt0001-id_carga.
  gwa_zmmt0145-ro_id_ordem       = w_zsdt0001-id_ordem.
  gwa_zmmt0145-ro_dt_movimento   = w_zsdt0001-dt_movimento.
  gwa_zmmt0145-ro_nr_safra       = w_zsdt0001-nr_safra.

  gwa_zmmt0145-ro_parid          = w_zsdt0001-parid.
  gwa_zmmt0145-ro_tp_frete       = w_zsdt0001-tp_frete.

  gwa_zmmt0145-ro_peso_liq       = w_zsdt0001-peso_liq.
  gwa_zmmt0145-ro_peso_fiscal    = w_zsdt0001-peso_fiscal.
  gwa_zmmt0145-ro_nfnum          = w_zsdt0001-nfnum.
  gwa_zmmt0145-ro_series         = w_zsdt0001-series.
  gwa_zmmt0145-ro_docdat         = w_zsdt0001-docdat.
  gwa_zmmt0145-ro_netwr          = w_zsdt0001-netwr.
  gwa_zmmt0145-ro_docnum         = w_zsdt0001-nro_nf_prod.
  if w_zsdt0001-ds_obs = '862'.
    gwa_zmmt0145-mb_xblnr = w_zsdt0001-doc_rem.
  endif.
  if gwa_zmmt0145-ro_nfnum  is initial  and w_zsdt0001-nro_nf_prod is not initial.
    select single *
      into @data(w_j_1bnfdoc)
      from j_1bnfdoc
      where docnum = @w_zsdt0001-nro_nf_prod.
    if sy-subrc = 0.
      gwa_zmmt0145-ro_nfnum          = w_j_1bnfdoc-nfenum.
      gwa_zmmt0145-ro_series         = w_j_1bnfdoc-series.
    endif.
  endif.


endform.
