*&---------------------------------------------------------------------*
*&  Include           ZFIR058_FORM
*&---------------------------------------------------------------------*

form f_selecionar_dados .

  data: wl_x001 type x001.
  data: g_vbeln type range of vbeln.
  data: r_vbeln2 type range of vbeln.
  data: w_vbeln2 like line of r_vbeln2.


  perform: f_limpa_variaveis,
           f_config_ranges.

  "Seleciona adiantamentos.
  if rb_cli is not initial.

    perform f_ranges_tp_partida using 'D'.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    select distinct *
      from bsid_view into corresponding fields of table @tg_bsid_adt
     where bukrs in @r_bukrs
       and kunnr in @r_parid
*      AND gsber IN p_gsber
       and umsks in @r_umsks_p
       and umskz in @r_umskz_p
       and vbel2 in @r_ovped
       and sgtxt in @r_sgtxt
       and zuonr in @r_zuonr.


    delete tg_bsid_adt where not ( blart ne 'VC'
                             and ( vbel2 ne '' or sgtxt ne '' or zuonr ne '' )
                             and   shkzg in r_shkzg_p ).
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    if tg_bsid_adt[] is initial.
      message 'Nenhum registro encontrado!' type 'S'.
      return.
    endif.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsid_adt_aux[] =  tg_bsid_adt[].
    sort tg_bsid_adt_aux by bukrs belnr.
    delete adjacent duplicates from tg_bsid_adt_aux
                          comparing bukrs belnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    data etl51c4r2656 type table of bseg.
    call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      exporting
        it_for_all_entries = tg_bsid_adt_aux[]
        i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR|
      importing
        et_bseg            = etl51c4r2656
      exceptions
        not_found          = 1.
    if sy-subrc = 0 and lines( etl51c4r2656 ) > 0.
      move-corresponding etl51c4r2656 to tg_bseg[] keeping target lines.
      sy-dbcnt = lines( etl51c4r2656 ).
    else.
      sy-subrc = 4.
      sy-dbcnt = 0.
    endif.

* PANF - performance
***-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
**    SELECT DISTINCT *
**      FROM bsis_VIEW INNER JOIN skb1 ON bsis_VIEW~bukrs = skb1~bukrs "#EC CI_DB_OPERATION_OK[2431747]
**                               AND bsis_VIEW~hkont = skb1~saknr
**       FOR ALL ENTRIES IN @tg_bsid_adt
**     WHERE bsis_VIEW~bukrs EQ @tg_bsid_adt-bukrs
**       AND bsis_VIEW~gjahr EQ @tg_bsid_adt-gjahr
**       AND bsis_VIEW~belnr EQ @tg_bsid_adt-belnr
**    APPENDING CORRESPONDING FIELDS OF TABLE @tg_bsis_cbanco.
***      AND skb1~fdlev IN r_fdlev_banco.
**
**    DELETE tg_bsis_cbanco WHERE NOT ( fdlev IN r_fdlev_banco[] ).
***-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio

    select bsis~bukrs bsis~belnr bsis~gjahr bsis~hkont bsis~dmbtr bsis~dmbe2
           bsis~waers bsis~wrbtr
           appending corresponding fields of table tg_bsis_cbanco
      from bsis
       for all entries in tg_bsid_adt
      where bsis~bukrs eq tg_bsid_adt-bukrs
       and bsis~gjahr eq tg_bsid_adt-gjahr
       and bsis~belnr eq tg_bsid_adt-belnr.

    select bukrs, saknr, fdlev
    from skb1
      into table @data(lt_skb1)
      for all entries in @tg_bsis_cbanco
      where skb1~bukrs = @tg_bsis_cbanco-bukrs
        and skb1~saknr = @tg_bsis_cbanco-hkont.

    delete lt_skb1 where not ( fdlev in r_fdlev_banco[] ).
    sort: lt_skb1 by bukrs saknr,
          tg_bsis_cbanco by bukrs belnr gjahr hkont.

    loop at tg_bsis_cbanco assigning field-symbol(<fs_bsis_cbanco>).
      read table lt_skb1 assigning field-symbol(<fs_skb1>)
                         with key bukrs = <fs_bsis_cbanco>-bukrs
                                  saknr = <fs_bsis_cbanco>-hkont
                                  binary search.
      if sy-subrc = 0.
        <fs_bsis_cbanco>-fdlev = <fs_skb1>-fdlev.
      endif.
    endloop.

    delete tg_bsis_cbanco where fdlev is initial.

    select distinct *
      from t001 into corresponding fields of table tg_t001
       for all entries in tg_bsid_adt
     where bukrs = tg_bsid_adt-bukrs.

    loop at tg_bsid_adt assigning field-symbol(<fs_bsid_adt>).

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = <fs_bsid_adt>-vbel2
        importing
          output = <fs_bsid_adt>-vbel2.

**      MODIFY tg_bsid_adt.
    endloop.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsid_adt_aux[] =  tg_bsid_adt[].
    sort tg_bsid_adt_aux by vbel2.
    delete adjacent duplicates from tg_bsid_adt_aux
                          comparing vbel2.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    select distinct *
      from vbak into corresponding fields of table tg_vbak
      for all entries in tg_bsid_adt_aux
     where vbeln = tg_bsid_adt_aux-vbel2
     and   spart in p_spart.

    if tg_vbak[] is not initial.
      select distinct *
        from tspat into corresponding fields of table tg_tspat
         for all entries in tg_vbak
       where spras = sy-langu
         and spart = tg_vbak-spart.
    endif.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsid_adt_aux[] =  tg_bsid_adt[].
    sort tg_bsid_adt_aux by bukrs belnr gjahr.
    delete adjacent duplicates from tg_bsid_adt_aux
                          comparing bukrs belnr gjahr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    "Seleciona Cabeçalhos
    select distinct *
     from bkpf into corresponding fields of table tg_bkpf
     for all entries in tg_bsid_adt_aux
    where bukrs eq tg_bsid_adt_aux-bukrs
      and belnr eq tg_bsid_adt_aux-belnr
      and gjahr eq tg_bsid_adt_aux-gjahr.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsid_adt_aux[] =  tg_bsid_adt[].
    sort tg_bsid_adt_aux by kunnr.
    delete adjacent duplicates from tg_bsid_adt_aux
                          comparing kunnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    select distinct *
      from kna1 into corresponding fields of table tg_kna1
       for all entries in tg_bsid_adt_aux
     where kunnr = tg_bsid_adt_aux-kunnr.

    "Seleciona Partidas Compensação.
    perform f_sel_part_comp using 'D'.

    if tg_bsid_comp[] is not initial.
      "Seleciona Cabeçalhos

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsid_comp_aux2[] =  tg_bsid_comp[].
      sort tg_bsid_comp_aux2 by bukrs belnr gjahr.
      delete adjacent duplicates from tg_bsid_comp_aux2
                            comparing bukrs belnr gjahr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      select distinct *
       from bkpf appending corresponding fields of table tg_bkpf
       for all entries in tg_bsid_comp_aux2
      where bukrs eq tg_bsid_comp_aux2-bukrs
        and belnr eq tg_bsid_comp_aux2-belnr
        and gjahr eq tg_bsid_comp_aux2-gjahr.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsid_comp_aux2[] =  tg_bsid_comp[].
      sort tg_bsid_comp_aux2 by kunnr.
      delete adjacent duplicates from tg_bsid_comp_aux2
                            comparing kunnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      select distinct *
        from kna1 appending corresponding fields of table tg_kna1
         for all entries in tg_bsid_comp_aux2
       where kunnr = tg_bsid_comp_aux2-kunnr.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsid_comp_aux2[] =  tg_bsid_comp[].
      sort tg_bsid_comp_aux2 by bukrs belnr.
      delete adjacent duplicates from tg_bsid_comp_aux2
                            comparing bukrs belnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      data etl174c6r4445 type table of bseg.
      call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
        exporting
          it_for_all_entries = tg_bsid_comp_aux2[]
          i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR|
        importing
          et_bseg            = etl174c6r4445
        exceptions
          not_found          = 1.
      if sy-subrc = 0 and lines( etl174c6r4445 ) > 0.
        move-corresponding etl174c6r4445 to tg_bseg[] keeping target lines.
        sy-dbcnt = lines( etl174c6r4445 ).
      else.
        sy-subrc = 4.
        sy-dbcnt = 0.
      endif.


*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsid_comp_aux2[] =  tg_bsid_comp[].
      sort tg_bsid_comp_aux2 by vbel2.
      delete adjacent duplicates from tg_bsid_comp_aux2
                            comparing vbel2.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      select distinct *
        from vbak appending corresponding fields of table tg_vbak
         for all entries in tg_bsid_comp_aux2
       where vbeln eq tg_bsid_comp_aux2-vbel2.

    endif.

    if tg_vbak[] is not initial.

      "Recusa e Devolução
      select distinct *
        from vbfa appending corresponding fields of table tg_vbfa_rd
         for all entries in tg_vbak
       where vbeln    eq tg_vbak-vbeln
         and vbtyp_n  in ('H','L', 'C')
         and vbtyp_v  eq 'C'.

      if tg_vbfa_rd[] is not initial.
        select distinct *
          from vbak appending corresponding fields of table tg_vbak
           for all entries in tg_vbfa_rd
         where vbeln eq tg_vbfa_rd-vbelv.
      endif.

      select distinct *
        from zsdt0041 appending corresponding fields of table tg_zsdt0041
         for all entries in tg_vbak
       where vbeln         eq tg_vbak-vbeln
         and doc_simulacao ne '0000000000'.

      select distinct *
        from zsdt0090 appending corresponding fields of table tg_zsdt0090
         for all entries in tg_vbak
       where vbeln         eq tg_vbak-vbeln
         and doc_simulacao ne '0000000000'
         and estorno       ne 'X'.

    endif.
**********************************************************************Melhoria na Performance!
    data(tg_bseg2) = tg_bseg[]. "PSA
    delete tg_bseg2 where anln1 is initial.
    sort tg_bseg2 by bukrs ascending belnr ascending.

    loop at tg_bsid_adt assigning field-symbol(<_adt>).
      "Atualização Dados Imobilizado
      if tg_bsid_adt-anln1 is initial.
        read table tg_bseg2 assigning field-symbol(<_adt_get>) with key bukrs = tg_bsid_adt-bukrs belnr = tg_bsid_adt-belnr binary search.
        if sy-subrc = 0.
          <_adt>-anln1  = <_adt_get>-anln1.
          <_adt>-anln2  = <_adt_get>-anln2.
        endif.
      endif.
      "Atribuir Documento Simulador
      perform f_atrib_doc_simulador using <_adt>-vbel2 changing <_adt>-dcsim.
    endloop.

    loop at tg_bsid_comp assigning field-symbol(<_comp>).
      "Atualização Dados Imobilizado
      if tg_bsid_comp-anln1 is initial.
        read table tg_bseg2 assigning field-symbol(<_comp_get>) with key bukrs = tg_bsid_adt-bukrs belnr = tg_bsid_adt-belnr binary search.
        if sy-subrc = 0.
          <_comp>-anln1  = <_comp_get>-anln1.
          <_comp>-anln2  = <_comp_get>-anln2.
        endif.
      endif.
      "Atribuir Documento Simulador
      perform f_atrib_doc_simulador using <_comp>-vbel2 changing <_comp>-dcsim.
    endloop.

    clear: tg_bseg2, tg_bseg2[].
**********************************************************************
*    LOOP AT tg_bsid_adt. "PSA Lentidão!
*
*      "Atualização Dados Imobilizado
*      IF tg_bsid_adt-anln1 IS INITIAL.
*        LOOP AT tg_bseg WHERE bukrs = tg_bsid_adt-bukrs
*                          AND belnr = tg_bsid_adt-belnr
*                          AND anln1 IS NOT INITIAL.
*          tg_bsid_adt-anln1  = tg_bseg-anln1.
*          tg_bsid_adt-anln2  = tg_bseg-anln2.
*          EXIT.
*        ENDLOOP.
*      ENDIF.
*
*      "Atribuir Documento Simulador
*      PERFORM f_atrib_doc_simulador USING tg_bsid_adt-vbel2
*                                 CHANGING tg_bsid_adt-dcsim.
*
*      MODIFY tg_bsid_adt.
*
*    ENDLOOP.
*
*    LOOP AT tg_bsid_comp.
*
*      "Atualização Dados Imobilizado
*      IF tg_bsid_comp-anln1 IS INITIAL.
*        LOOP AT tg_bseg WHERE bukrs = tg_bsid_comp-bukrs
*                          AND belnr = tg_bsid_comp-belnr
*                          AND anln1 IS NOT INITIAL.
*          tg_bsid_comp-anln1  = tg_bseg-anln1.
*          tg_bsid_comp-anln2  = tg_bseg-anln2.
*          EXIT.
*        ENDLOOP.
*      ENDIF.
*
*      "Atribuir Documento Simulador
*      PERFORM f_atrib_doc_simulador USING tg_bsid_comp-vbel2
*                                 CHANGING tg_bsid_comp-dcsim.
*
*      MODIFY tg_bsid_comp.
*    ENDLOOP.

    if ( p_dcsimj eq abap_true ).
      "OV Primária
      if r_dcsim[] is initial.
        loop at tg_bsid_adt.
          r_dcsim-sign   = 'I'.
          r_dcsim-option = 'EQ'.
          r_dcsim-low    = tg_bsid_adt-vbel2.
          append r_dcsim.
        endloop.
      endif.
      "Seleção ZSDT0060
      free: r_vbeln2,
            it_vbel.
      loop at r_dcsim.
        w_vbeln2-sign   = 'I'.
        w_vbeln2-option = 'EQ'.
        w_vbeln2-low    = r_dcsim-low.
        append w_vbeln2 to r_vbeln2.
      endloop.


      delete r_vbeln2 where low is initial.
      sort r_vbeln2 by low.
      delete adjacent duplicates from r_vbeln2 comparing low.

      select vbeln vbelv
        from zsdt0090
          appending table it_vbel
              where ( vbeln in r_vbeln2 or
                      vbelv in r_vbeln2 )
                and estorno ne abap_true.


      move it_vbel to it_vbelx.

      if it_vbelx is not initial.

        free: r_vbeln, g_vbeln.
        r_vbeln2 = value #( for ls_vbelx in it_vbelx ( sign = 'I' option = 'EQ' low = ls_vbelx-vbeln ) ).
        append lines of r_vbeln2 to g_vbeln.

        r_vbeln2 = value #( for ls_vbelx in it_vbelx ( sign = 'I' option = 'EQ' low = ls_vbelx-vbelv ) ).
        append lines of r_vbeln2 to g_vbeln.

        delete g_vbeln where low is initial.
        sort g_vbeln by low.
        delete adjacent duplicates from g_vbeln comparing low.

        select vbeln vbelv
          from zsdt0090
            appending table it_vbel
                where ( vbelv in g_vbeln or vbeln in g_vbeln )
                  and estorno ne abap_true.

        sort it_vbel by vbeln vbelv.
        delete adjacent duplicates from it_vbel comparing vbeln vbelv.

        free: r_vbeln2, g_vbeln, it_vbelx.

        r_vbeln2 = value #( for ls_vbel_aux in it_vbel ( sign = 'I' option = 'EQ' low = ls_vbel_aux-vbeln ) ).
        append lines of r_vbeln2 to g_vbeln.

        r_vbeln2 = value #( for ls_vbel_aux in it_vbel ( sign = 'I' option = 'EQ' low = ls_vbel_aux-vbelv ) ).
        append lines of r_vbeln2 to g_vbeln.

        delete g_vbeln where low is initial.
        sort g_vbeln by low.
        delete adjacent duplicates from g_vbeln comparing low.

        if g_vbeln is not initial.

          select vbeln vbelv
            from zsdt0090
          appending table it_vbel
          where ( vbelv in g_vbeln or vbeln in g_vbeln )
            and estorno ne abap_true.

          free: r_vbeln2, g_vbeln.
          r_vbeln2 = value #( for ls_vbel_aux in it_vbel ( sign = 'I' option = 'EQ' low = ls_vbel_aux-vbeln ) ).
          append lines of r_vbeln2 to g_vbeln.

          r_vbeln2 = value #( for ls_vbel_aux in it_vbel ( sign = 'I' option = 'EQ' low = ls_vbel_aux-vbelv ) ).
          append lines of r_vbeln2 to g_vbeln.

          delete g_vbeln where low is initial.
          sort g_vbeln by low.
          delete adjacent duplicates from g_vbeln comparing low.

        endif.
      endif.

      "Seleção ZSDT0060
*      "
*      SELECT *
*      FROM zsdt0090 INTO CORRESPONDING FIELDS OF TABLE tg_zsdt0090_aux
*      WHERE vbelv IN r_dcsim
*      AND estorno NE abap_true.
*
*      LOOP AT r_dcsim.
*        r_vbeln-sign   = 'I'.
*        r_vbeln-option = 'EQ'.
*        r_vbeln-low    = r_dcsim-low.
*        APPEND r_vbeln.
*      ENDLOOP.
*      LOOP AT tg_zsdt0090_aux.
*        r_vbeln-sign   = 'I'.
*        r_vbeln-option = 'EQ'.
*        r_vbeln-low    = tg_zsdt0090_aux-vbeln.
*        APPEND r_vbeln.
*      ENDLOOP.

* INICIO - STEFANINI - FT - 11.09.2024 - IR177849
      select doc_simulacao from zsdt0090
        into table @data(lt_doc)
        where vbelv   in @r_dcsim
          and estorno ne @abap_true.
      if sy-subrc eq 0.
        select vbeln, vbelv from zsdt0090
          into table @data(lt_ovs)
          for all entries in @lt_doc
          where doc_simulacao eq @lt_doc-doc_simulacao
            and estorno       ne @abap_true.
        if sy-subrc eq 0.
          r_vbeln2 = value #( for ls_ovs in lt_ovs ( sign = 'I' option = 'EQ' low = ls_ovs-vbeln ) ).
          append lines of r_vbeln2 to g_vbeln.

          r_vbeln2 = value #( for ls_ovs in lt_ovs ( sign = 'I' option = 'EQ' low = ls_ovs-vbelv ) ).
          append lines of r_vbeln2 to g_vbeln.

          delete g_vbeln where low is initial.
          sort g_vbeln by low.
          delete adjacent duplicates from g_vbeln comparing low.

        endif.
      endif.
* FIM - STEFANINI - FT - 11.09.2024 - IR177849

      if g_vbeln is not initial.
        delete tg_bsid_adt  where vbel2 not in g_vbeln.
        delete tg_bsid_comp where vbel2 not in g_vbeln.
      endif.

* PBI - 70679 - Fim - CBRAND
    endif.

  endif.

  if rb_forn is not initial. "OR rb_prod IS NOT INITIAL. PBI - 70679 - CBRAND

    perform f_ranges_tp_partida using 'K'.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
* PBI - 70679 - CBRAND - Inicio
*    IF rb_prod IS NOT INITIAL.
*      SELECT *
*          FROM bsik INTO CORRESPONDING FIELDS OF TABLE tg_bsik_adt
*         WHERE bukrs IN r_bukrs
*           AND gsber IN p_gsber
*           AND lifnr IN r_parid
*           AND ebeln IN r_ovped
*           AND sgtxt IN r_sgtxt
*           AND zuonr IN r_zuonr
*           AND xref1 IN p_safra
*           AND umskz NE 'Z'
*           AND blart IN ( 'MA', 'MB' )
*           AND ( ebeln NE '' OR sgtxt NE '' OR zuonr NE '' ).
*
*      DELETE tg_bsik_adt WHERE NOT ( xref1 IN p_safra[]
*                          AND   xref3 IN r_xref3[] ).
*
*
*    ELSE.
* PBI - 70679 - Fim - CBRAND
    select distinct *
      from bsik_view into corresponding fields of table @tg_bsik_adt
     where bukrs in @r_bukrs
       and lifnr in @r_parid
*      AND gsber IN p_gsber
       and umsks in @r_umsks_p
       and umskz in @r_umskz_p
       and ebeln in @r_ovped
       and sgtxt in @r_sgtxt
       and zuonr in @r_zuonr.
*      AND XREF1 IN P_SAFRA.
*      AND XREF3 LIKE P_PRODU-LOW
*      AND BLART NE 'VC'
*      AND ( EBELN NE '' OR SGTXT NE '' OR ZUONR NE '' )
*      AND SHKZG IN R_SHKZG_P.

    delete tg_bsik_adt where not ( "xref1 IN p_safra[] AND
                                   "xref3 IN r_xref3[]  AND
                                   blart ne 'VC'
                             and ( ebeln ne '' or sgtxt ne '' or zuonr ne '' )
                             and   shkzg in r_shkzg_p[] ).
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim
*    ENDIF.

    if tg_bsik_adt[] is initial.
      message 'Nenhum registro encontrado!' type 'S'.
      return.
    endif.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsik_adt_aux[] =  tg_bsik_adt[].
    sort tg_bsik_adt_aux by bukrs belnr.
    delete adjacent duplicates from tg_bsik_adt_aux
                          comparing bukrs belnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    data etl450c4r8073 type table of bseg.
    call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      exporting
        it_for_all_entries = tg_bsik_adt_aux[]
        i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR|
      importing
        et_bseg            = etl450c4r8073
      exceptions
        not_found          = 1.
    if sy-subrc = 0 and lines( etl450c4r8073 ) > 0.
      move-corresponding etl450c4r8073 to tg_bseg[] keeping target lines.
      sy-dbcnt = lines( etl450c4r8073 ).
    else.
      sy-subrc = 4.
      sy-dbcnt = 0.
    endif.


    loop at tg_bsik_adt where ebeln is not initial
                          and ebelp is initial.
      clear: tg_ekpo_aux[].
      loop at tg_bseg where bukrs eq tg_bsik_adt-bukrs
                        and belnr eq tg_bsik_adt-belnr
                        and ebeln eq tg_bsik_adt-ebeln
                        and ebelp is not initial.
        tg_ekpo_aux-ebeln = tg_bseg-ebeln.
        tg_ekpo_aux-ebelp = tg_bseg-ebelp.
        append tg_ekpo_aux.
      endloop.

      sort tg_ekpo_aux by ebeln ebelp.
      delete adjacent duplicates from tg_ekpo_aux comparing ebeln ebelp.
      read table tg_ekpo_aux index 1.

      if ( lines( tg_ekpo_aux[] ) = 1 ).
        tg_bsik_adt-ebelp = tg_ekpo_aux-ebelp.
      elseif ( lines( tg_ekpo_aux[] ) = 0 ).
        tg_bsik_adt-ebelp = '00010'.
      endif.
      modify tg_bsik_adt.
    endloop.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsik_adt_aux[] =  tg_bsik_adt[].
    sort tg_bsik_adt_aux by ebeln ebelp.
    delete adjacent duplicates from tg_bsik_adt_aux
                          comparing ebeln ebelp.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    select distinct *
      from ekkn appending corresponding fields of table tg_ekkn
       for all entries in tg_bsik_adt_aux
     where ebeln = tg_bsik_adt_aux-ebeln
       and ebelp = tg_bsik_adt_aux-ebelp.

* PANF - performance
***-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
**    SELECT DISTINCT *
**      FROM bsis_view INNER JOIN skb1 ON bsis_view~bukrs = skb1~bukrs "#EC CI_DB_OPERATION_OK[2431747]
**                               AND bsis_view~hkont = skb1~saknr
**       FOR ALL ENTRIES IN @tg_bsik_adt
**     WHERE bsis_view~bukrs EQ @tg_bsik_adt-bukrs
**       AND bsis_view~gjahr EQ @tg_bsik_adt-gjahr
**       AND bsis_view~belnr EQ @tg_bsik_adt-belnr
**      APPENDING CORRESPONDING FIELDS OF TABLE @tg_bsis_cbanco.
***      AND skb1~fdlev IN r_fdlev_banco.
**
**    DELETE tg_bsis_cbanco WHERE NOT ( fdlev IN r_fdlev_banco[] ).
***-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

* PANF - performance
    select bsis~bukrs bsis~belnr bsis~gjahr bsis~hkont bsis~dmbtr bsis~dmbe2
           bsis~waers bsis~wrbtr
           appending corresponding fields of table tg_bsis_cbanco
      from bsis
       for all entries in tg_bsik_adt
      where bsis~bukrs eq tg_bsik_adt-bukrs
       and bsis~gjahr eq tg_bsik_adt-gjahr
       and bsis~belnr eq tg_bsik_adt-belnr.

    select bukrs, saknr, fdlev
    from skb1
      into table @data(lt_skb1_a)
      for all entries in @tg_bsis_cbanco
      where skb1~bukrs = @tg_bsis_cbanco-bukrs
        and skb1~saknr = @tg_bsis_cbanco-hkont.

    delete lt_skb1_a where not ( fdlev in r_fdlev_banco[] ).
    sort: lt_skb1_a by bukrs saknr,
          tg_bsis_cbanco by bukrs belnr gjahr hkont.

    loop at tg_bsis_cbanco assigning field-symbol(<fs_bsis_cbanco_a>).
      read table lt_skb1_a assigning field-symbol(<fs_skb1_a>)
                         with key bukrs = <fs_bsis_cbanco_a>-bukrs
                                  saknr = <fs_bsis_cbanco_a>-hkont
                                  binary search.
      if sy-subrc = 0.
        <fs_bsis_cbanco_a>-fdlev = <fs_skb1_a>-fdlev.
      endif.
    endloop.

    delete tg_bsis_cbanco where fdlev is initial.
* PANF - performance

    select distinct *
      from t001 into corresponding fields of table tg_t001
       for all entries in tg_bsik_adt
     where bukrs = tg_bsik_adt-bukrs.

    loop at tg_bsik_adt assigning field-symbol(<fs_bsik_adt2>).
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = <fs_bsik_adt2>-ebeln
        importing
          output = <fs_bsik_adt2>-ebeln.

**      MODIFY tg_bsik_adt.
    endloop.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsik_adt_aux[] =  tg_bsik_adt[].
    sort tg_bsik_adt_aux by ebeln.
    delete adjacent duplicates from tg_bsik_adt_aux
                          comparing ebeln.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    select distinct *
      from ekko into corresponding fields of table tg_ekko
       for all entries in tg_bsik_adt_aux
     where ebeln = tg_bsik_adt_aux-ebeln.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsik_adt_aux[] =  tg_bsik_adt[].
    sort tg_bsik_adt_aux by bukrs belnr gjahr.
    delete adjacent duplicates from tg_bsik_adt_aux
                          comparing bukrs belnr gjahr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    "Seleciona Cabeçalhos
    select distinct *
     from bkpf into corresponding fields of table tg_bkpf
     for all entries in tg_bsik_adt_aux
    where bukrs eq tg_bsik_adt_aux-bukrs
      and belnr eq tg_bsik_adt_aux-belnr
      and gjahr eq tg_bsik_adt_aux-gjahr.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsik_adt_aux[] =  tg_bsik_adt[].
    sort tg_bsik_adt_aux by lifnr.
    delete adjacent duplicates from tg_bsik_adt_aux
                          comparing lifnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    select distinct *
      from lfa1 into corresponding fields of table tg_lfa1
       for all entries in tg_bsik_adt_aux
     where lifnr = tg_bsik_adt_aux-lifnr.

    "Seleciona Partidas Compensação.
    perform f_sel_part_comp using 'K'.

    if tg_bsik_comp[] is not initial.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsik_comp_aux2[] =  tg_bsik_comp[].
      sort tg_bsik_comp_aux2 by bukrs belnr gjahr.
      delete adjacent duplicates from tg_bsik_comp_aux2
                            comparing bukrs belnr gjahr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      "Seleciona Cabeçalhos
      select distinct *
       from bkpf appending corresponding fields of table tg_bkpf
       for all entries in tg_bsik_comp_aux2
      where bukrs eq tg_bsik_comp_aux2-bukrs
        and belnr eq tg_bsik_comp_aux2-belnr
        and gjahr eq tg_bsik_comp_aux2-gjahr.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsik_comp_aux2[] =  tg_bsik_comp[].
      sort tg_bsik_comp_aux2 by lifnr.
      delete adjacent duplicates from tg_bsik_comp_aux2
                            comparing lifnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      select distinct *
        from lfa1 appending corresponding fields of table tg_lfa1
         for all entries in tg_bsik_comp_aux2
       where lifnr = tg_bsik_comp_aux2-lifnr.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsik_comp_aux2[] =  tg_bsik_comp[].
      sort tg_bsik_comp_aux2 by bukrs belnr.
      delete adjacent duplicates from tg_bsik_comp_aux2
                            comparing bukrs belnr..
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      data etl601c6r8720 type table of bseg.
      call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
        exporting
          it_for_all_entries = tg_bsik_comp_aux2[]
          i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR|
        importing
          et_bseg            = etl601c6r8720
        exceptions
          not_found          = 1.
      if sy-subrc = 0 and lines( etl601c6r8720 ) > 0.
        move-corresponding etl601c6r8720 to tg_bseg[] keeping target lines.
        sy-dbcnt = lines( etl601c6r8720 ).
      else.
        sy-subrc = 4.
        sy-dbcnt = 0.
      endif.


*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsik_comp_aux2[] =  tg_bsik_comp[].
      sort tg_bsik_comp_aux2 by ebeln ebelp.
      delete adjacent duplicates from tg_bsik_comp_aux2
                            comparing ebeln ebelp.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      select distinct *
        from ekkn appending corresponding fields of table tg_ekkn
         for all entries in tg_bsik_comp_aux2
       where ebeln = tg_bsik_comp_aux2-ebeln
         and ebelp = tg_bsik_comp_aux2-ebelp.

      loop at tg_bsik_comp where ebeln is not initial
                             and ebelp is initial.
        clear: tg_ekpo_aux[].
        loop at tg_bseg where bukrs eq tg_bsik_comp-bukrs
                          and belnr eq tg_bsik_comp-belnr
                          and ebeln eq tg_bsik_comp-ebeln
                          and ebelp is not initial.
          tg_ekpo_aux-ebeln = tg_bseg-ebeln.
          tg_ekpo_aux-ebelp = tg_bseg-ebelp.
          append tg_ekpo_aux.
        endloop.

        sort tg_ekpo_aux by ebeln ebelp.
        delete adjacent duplicates from tg_ekpo_aux comparing ebeln ebelp.
        read table tg_ekpo_aux index 1.

        if ( lines( tg_ekpo_aux[] ) = 1 ).
          tg_bsik_comp-ebelp = tg_ekpo_aux-ebelp.
        elseif ( lines( tg_ekpo_aux[] ) = 0 ).
          tg_bsik_comp-ebelp = '00010'.
        endif.
        modify tg_bsik_comp.
      endloop.
    endif.

    "Atualização Dados Imobilizado
    loop at tg_bsik_adt.
      if tg_bsik_adt-anln1 is initial.
        loop at tg_bseg where bukrs = tg_bsik_adt-bukrs
                          and belnr = tg_bsik_adt-belnr
                          and anln1 is not initial.
          tg_bsik_adt-anln1  = tg_bseg-anln1.
          tg_bsik_adt-anln2  = tg_bseg-anln2.
          modify tg_bsik_adt.
          exit.
        endloop.
      endif.

      "Busca Classificação Contabil
      if tg_bsik_adt-anln1 is initial.
        loop at tg_ekkn where ebeln = tg_bsik_adt-ebeln
                          and ebelp = tg_bsik_adt-ebelp
                          and anln1 is not initial.
          tg_bsik_adt-anln1  = tg_ekkn-anln1.
          tg_bsik_adt-anln2  = tg_ekkn-anln2.
          modify tg_bsik_adt.
          exit.
        endloop.
      endif.
    endloop.

    loop at tg_bsik_comp.
      if tg_bsik_comp-anln1 is initial.
        loop at tg_bseg where bukrs = tg_bsik_comp-bukrs
                          and belnr = tg_bsik_comp-belnr
                          and anln1 is not initial.
          tg_bsik_comp-anln1  = tg_bseg-anln1.
          tg_bsik_comp-anln2  = tg_bseg-anln2.
          modify tg_bsik_comp.
          exit.
        endloop.
      endif.

      "Busca Classificação Contabil
      if tg_bsik_comp-anln1 is initial.
        loop at tg_ekkn where ebeln = tg_bsik_comp-ebeln
                          and ebelp = tg_bsik_comp-ebelp
                          and anln1 is not initial.
          tg_bsik_comp-anln1  = tg_ekkn-anln1.
          tg_bsik_comp-anln2  = tg_ekkn-anln2.
          modify tg_bsik_comp.
          exit.
        endloop.
      endif.

    endloop.

  endif.

  loop at tg_t001.
    clear: wl_x001.

    call function 'FI_CURRENCY_INFORMATION'
      exporting
        i_bukrs = tg_t001-bukrs
      importing
        e_x001  = wl_x001.

    tg_t001-waers2 = wl_x001-hwae2.

    modify tg_t001.
  endloop.

  sort tg_bsis_cbanco by bukrs gjahr belnr.

endform.

form f_processa_dados .
  data: wa_zfit0154 type zfit0154,
        vdif_int    type zfit0154-vlr_toler,
        vdif_for    type zfit0154-vlr_toler.

  select single *
      into wa_zfit0154
      from zfit0154
      where tipo = 'C'
      and   fg_soc = ''.

  loop at tg_bsid_adt.
    clear: wa_saida_0100, tg_kna1, tg_vbak, tg_tspat.

    perform f_moeda_empresa using tg_bsid_adt-bukrs
                                  'X'.
    if ( sy-subrc ne 0 ).
      return.
    endif.

    read table tg_bkpf with key bukrs = tg_bsid_adt-bukrs
                                belnr = tg_bsid_adt-belnr
                                gjahr = tg_bsid_adt-gjahr.
    check sy-subrc = 0.

    read table tg_kna1 with key kunnr = tg_bsid_adt-kunnr.
    check sy-subrc = 0.

    read table tg_vbak with key vbeln = tg_bsid_adt-vbel2.
    if sy-subrc = 0.
      read table tg_tspat with key spart = tg_vbak-spart.
      if sy-subrc = 0.
        wa_saida_0100-spart = tg_vbak-spart.  "Setor Atividade
        wa_saida_0100-vtext = tg_tspat-vtext. "Descr. Setor Atividade
      endif.
    endif.

    if wa_saida_0100-spart not in p_spart and p_spart[] is not initial.
      continue.
    endif.


    wa_saida_0100-bukrs     = tg_bsid_adt-bukrs.
    wa_saida_0100-parid     = tg_kna1-kunnr.
    wa_saida_0100-name1     = tg_kna1-name1.
    wa_saida_0100-belnr     = tg_bsid_adt-belnr.
    wa_saida_0100-buzei     = tg_bsid_adt-buzei.
    wa_saida_0100-gjahr     = tg_bsid_adt-gjahr.
    wa_saida_0100-bldat     = tg_bsid_adt-bldat.
    wa_saida_0100-budat     = tg_bsid_adt-budat.
    wa_saida_0100-waers     = tg_bsid_adt-waers.
    wa_saida_0100-wrbtr     = tg_bsid_adt-wrbtr.
    wa_saida_0100-dmbtr     = tg_bsid_adt-dmbtr.
    wa_saida_0100-dmbe2     = tg_bsid_adt-dmbe2.
    wa_saida_0100-dmbtr_au2 = tg_bsid_adt-dmbtr.
    wa_saida_0100-dmbtr_aux = tg_bsid_adt-wrbtr.
    wa_saida_0100-dmbe2_aux = tg_bsid_adt-dmbe2.
    wa_saida_0100-hkont     = tg_bsid_adt-hkont.
    wa_saida_0100-bschl     = tg_bsid_adt-bschl.
    wa_saida_0100-umsks     = tg_bsid_adt-umsks.
    wa_saida_0100-umskz     = tg_bsid_adt-umskz.
    wa_saida_0100-shkzg     = tg_bsid_adt-shkzg.
    wa_saida_0100-gsber     = tg_bsid_adt-gsber.
    wa_saida_0100-sgtxt     = tg_bsid_adt-sgtxt.

*** - PBI - 70679 - Inicio - CBRAND
    read table tg_bseg with key bukrs = wa_saida_0100-bukrs
                                belnr = wa_saida_0100-belnr.

    wa_saida_0100-zfbdt     = tg_bsid_adt-zfbdt + tg_bseg-zbd1t .
*    wa_saida_0100-zfbdt     = tg_bsid_adt-zfbdt
*** - PBI - 70679 - Fim - CBRAND
    wa_saida_0100-zbd1t     = tg_bsid_adt-zbd1t.
    wa_saida_0100-kidno     = tg_bsid_adt-kidno.
    wa_saida_0100-xref1     = tg_bsid_adt-xref1.
    wa_saida_0100-xref3     = tg_bsid_adt-xref3.
    wa_saida_0100-zuonr     = tg_bsid_adt-zuonr.
    wa_saida_0100-blart     = tg_bsid_adt-blart.
    wa_saida_0100-zterm     = tg_bsid_adt-zterm.
    wa_saida_0100-anln1     = tg_bsid_adt-anln1.
    wa_saida_0100-anln2     = tg_bsid_adt-anln2.
    wa_saida_0100-xblnr     = tg_bkpf-xblnr.
    wa_saida_0100-ovped     = tg_bsid_adt-vbel2.
    wa_saida_0100-posn2     = tg_bsid_adt-posn2.
    wa_saida_0100-itmop     = tg_bsid_adt-posn2.
    wa_saida_0100-dcsim     = tg_bsid_adt-dcsim.
    wa_saida_0100-count     = 1.
    wa_saida_0100-koart     = 'D'.

    perform f_get_taxa using tg_bkpf
                             wa_saida_0100-dmbtr
                             wa_saida_0100-dmbe2
                    changing wa_saida_0100-kursf.

    perform f_get_bsid_comp tables tg_bsid_comp_aux
                             using tg_bsid_adt-bukrs
                                   tg_bsid_adt-belnr
                                   tg_bsid_adt-buzei
                                   tg_bsid_adt-gjahr
                                   tg_bsid_adt-kunnr
                                   tg_bsid_adt-vbel2
                                   tg_bsid_adt-sgtxt
                                   tg_bsid_adt-zuonr
                                   tg_bsid_adt-anln1
                                   tg_bsid_adt-anln2
                                   tg_bsid_adt-dcsim.

    loop at tg_bsid_comp_aux.
      add 1 to wa_saida_0100-qtde_ft.
      if ( tg_bsid_comp_aux-shkzg ne wa_saida_0100-shkzg ). "Se for operação(Cred.Deb.) diferente da partida principal
        add tg_bsid_comp_aux-wrbtr to wa_saida_0100-ft_dmbtr.
        add tg_bsid_comp_aux-dmbe2 to wa_saida_0100-ft_dmbe2.
      else.
        subtract tg_bsid_comp_aux-wrbtr from wa_saida_0100-ft_dmbtr.
        subtract tg_bsid_comp_aux-dmbe2 from wa_saida_0100-ft_dmbe2.
      endif.
    endloop.

    wa_saida_0100-df_dmbe2 = wa_saida_0100-ft_dmbtr - wa_saida_0100-wrbtr.

    wa_saida_0100-comp = icon_light_out.
    vdif_int = abs( wa_saida_0100-ft_dmbtr - wa_saida_0100-wrbtr ).
    clear wa_saida_0100-st_comp.

    if ( vdif_int le  wa_zfit0154-vlr_toler ) and wa_saida_0100-ft_dmbtr  > 0 and wa_saida_0100-wrbtr > 0.
      wa_saida_0100-st_comp = '3'.
      wa_saida_0100-vlr_rsd  = wa_saida_0100-ft_dmbtr - wa_saida_0100-wrbtr.
      wa_saida_0100-comp     = icon_execute_object.
    endif.

    if wa_saida_0100-st_comp ne '3'.

      if  ( wa_saida_0100-ft_dmbtr = wa_saida_0100-wrbtr ).
        wa_saida_0100-st_comp  = '1'.
        wa_saida_0100-comp     = icon_execute_object.
      else.
        wa_saida_0100-st_comp  = '2'.
        wa_saida_0100-comp     = icon_system_mark.
      endif.
    endif.

    wa_saida_0100-view_cp  = icon_display.
    wa_saida_0100-view_ad  = icon_display.

    append wa_saida_0100 to it_saida_0100.
  endloop.

  select single *
      into wa_zfit0154
      from zfit0154
      where tipo = 'F'
      and   fg_soc = ''.

  loop at tg_bsik_adt.
    clear: wa_saida_0100, tg_lfa1, tg_ekko.

    perform f_moeda_empresa using tg_bsik_adt-bukrs
                                  'X'.
    if ( sy-subrc ne 0 ).
      return.
    endif.

    read table tg_bkpf with key bukrs = tg_bsik_adt-bukrs
                                belnr = tg_bsik_adt-belnr
                                gjahr = tg_bsik_adt-gjahr.
    "CHECK sy-subrc = 0.
    if sy-subrc ne 0.
      tg_bkpf-bukrs = tg_bsik_adt-bukrs.                    "PBI 58318
    endif.

    read table tg_lfa1 with key lifnr = tg_bsik_adt-lifnr.
    check sy-subrc = 0.

    read table tg_ekko with key ebeln = tg_bsik_adt-ebeln.
    if sy-subrc = 0.

    endif.

    wa_saida_0100-bukrs     = tg_bsik_adt-bukrs.
    wa_saida_0100-parid     = tg_lfa1-lifnr.
    wa_saida_0100-name1     = tg_lfa1-name1.
    wa_saida_0100-belnr     = tg_bsik_adt-belnr.
    wa_saida_0100-buzei     = tg_bsik_adt-buzei.
    wa_saida_0100-gjahr     = tg_bsik_adt-gjahr.
    wa_saida_0100-bldat     = tg_bsik_adt-bldat.
    wa_saida_0100-budat     = tg_bsik_adt-budat.
    wa_saida_0100-waers     = tg_bsik_adt-waers.
    wa_saida_0100-wrbtr     = tg_bsik_adt-wrbtr.
    wa_saida_0100-dmbtr     = tg_bsik_adt-dmbtr.
    wa_saida_0100-dmbe2     = tg_bsik_adt-dmbe2.
    wa_saida_0100-dmbtr_au2 = tg_bsik_adt-dmbtr.
    wa_saida_0100-dmbtr_aux = tg_bsik_adt-wrbtr.
    wa_saida_0100-dmbe2_aux = tg_bsik_adt-dmbe2.
    wa_saida_0100-hkont     = tg_bsik_adt-hkont.
    wa_saida_0100-bschl     = tg_bsik_adt-bschl.
    wa_saida_0100-umsks     = tg_bsik_adt-umsks.
    wa_saida_0100-umskz     = tg_bsik_adt-umskz.
    wa_saida_0100-shkzg     = tg_bsik_adt-shkzg.
    wa_saida_0100-gsber     = tg_bsik_adt-gsber.
    wa_saida_0100-sgtxt     = tg_bsik_adt-sgtxt.

*** - PBI - 70679 - Inicio - CBRAND
    read table tg_bseg with key bukrs = wa_saida_0100-bukrs
                                belnr = wa_saida_0100-belnr.

    wa_saida_0100-zfbdt     = tg_bsik_adt-zfbdt + tg_bseg-zbd1t .
    "wa_saida_0100-zfbdt     = tg_bsik_adt-zfbdt.
*** - PBI - 70679 - Fim - CBRAND

    wa_saida_0100-zbd1t     = tg_bsik_adt-zbd1t.
    wa_saida_0100-kidno     = tg_bsik_adt-kidno.
    wa_saida_0100-xref1     = tg_bsik_adt-xref1.
    wa_saida_0100-xref3     = tg_bsik_adt-xref3.
    wa_saida_0100-zuonr     = tg_bsik_adt-zuonr.
    wa_saida_0100-blart     = tg_bsik_adt-blart.
    wa_saida_0100-zterm     = tg_bsik_adt-zterm.
    wa_saida_0100-anln1     = tg_bsik_adt-anln1.
    wa_saida_0100-anln2     = tg_bsik_adt-anln2.
    wa_saida_0100-xblnr     = tg_bkpf-xblnr.
    wa_saida_0100-ovped     = tg_bsik_adt-ebeln.
    wa_saida_0100-ebelp     = tg_bsik_adt-ebelp.
    wa_saida_0100-itmop     = tg_bsik_adt-ebelp.
    wa_saida_0100-count     = 1.
    wa_saida_0100-qtde_ad   = tg_bsik_adt-qtde_ad.
    wa_saida_0100-koart     = 'K'.

    perform f_get_taxa using tg_bkpf
                             wa_saida_0100-dmbtr
                             wa_saida_0100-dmbe2
                    changing wa_saida_0100-kursf.

    perform f_get_bsik_comp tables tg_bsik_comp_aux
                             using tg_bsik_adt-bukrs
                                   tg_bsik_adt-belnr
                                   tg_bsik_adt-buzei
                                   tg_bsik_adt-gjahr
                                   tg_bsik_adt-lifnr
                                   tg_bsik_adt-ebeln
                                   tg_bsik_adt-sgtxt
                                   tg_bsik_adt-zuonr
                                   tg_bsik_adt-anln1
                                   tg_bsik_adt-anln2.

    loop at tg_bsik_comp_aux.
      add 1 to wa_saida_0100-qtde_ft.
      if ( tg_bsik_comp_aux-shkzg ne wa_saida_0100-shkzg ). "Se for operação(Cred.Deb.) diferente da partida principal
        add tg_bsik_comp_aux-wrbtr to wa_saida_0100-ft_dmbtr.
        add tg_bsik_comp_aux-dmbe2 to wa_saida_0100-ft_dmbe2.
      else.
        subtract tg_bsik_comp_aux-wrbtr from wa_saida_0100-ft_dmbtr.
        subtract tg_bsik_comp_aux-dmbe2 from wa_saida_0100-ft_dmbe2.
      endif.
    endloop.

    wa_saida_0100-comp = icon_light_out.
    "
    clear:  wa_saida_0100-st_comp.


    vdif_int = abs( wa_saida_0100-ft_dmbtr - wa_saida_0100-wrbtr ).
    wa_saida_0100-df_dmbe2 = wa_saida_0100-ft_dmbtr - wa_saida_0100-wrbtr.  "*-BUG 78517-11.05.2022-JT-inicio
    "
    if ( vdif_int le  wa_zfit0154-vlr_toler ) and wa_saida_0100-ft_dmbtr  > 0 and wa_saida_0100-wrbtr > 0.
      wa_saida_0100-st_comp = '3'.
      wa_saida_0100-vlr_rsd  = wa_saida_0100-ft_dmbtr - wa_saida_0100-wrbtr.
      wa_saida_0100-comp     = icon_execute_object.
    endif.

    if wa_saida_0100-st_comp ne '3'.
      if  ( wa_saida_0100-ft_dmbtr = wa_saida_0100-wrbtr ).
        wa_saida_0100-st_comp  = '1'.
        wa_saida_0100-comp      = icon_execute_object.
      else.
        "Selecionar Partidas para compensar
        wa_saida_0100-st_comp  = '2'.
        wa_saida_0100-comp      = icon_system_mark.
      endif.
    endif.


    wa_saida_0100-view_cp  = icon_display.
    wa_saida_0100-view_ad  = icon_display.

    append wa_saida_0100 to it_saida_0100.
  endloop.


endform.

form f_refresh_alv using p_alv.

  case p_alv.
    when '0100'.
      if obj_alv_0100 is not initial.
        call method obj_alv_0100->refresh_table_display
          exporting
            is_stable = wa_stable.
      endif.
    when '0110'.
      call method obj_alv_0110->refresh_table_display
        exporting
          is_stable = wa_stable.
  endcase.

endform.

form f_refresh_objetos .

  clear: gs_layout,
         gs_variant.

  refresh: it_exclude_fcode.
* PBI - 70679 - Inicio -  CBRAND
*  IF rb_prod = 'X'.
*    IF sy-dynnr = '0100'.
*      IF obj_alv_0100 IS NOT INITIAL.
*        CALL METHOD obj_alv_0100->free.
*
*        IF obj_container_0100 IS NOT INITIAL.
*          CALL METHOD obj_container_0100->free.
*        ENDIF.
*        FREE:  obj_alv_0100,obj_toolbar_0100.
*        FREE: obj_container_0100.
*      ENDIF.
*      MOVE '/PRODUTORES' TO gs_variant-variant.
*    ELSEIF sy-dynnr = '0110'.
*      IF obj_alv_0110 IS NOT INITIAL.
*        CALL METHOD obj_alv_0110->free.
*
*        IF obj_container_0110 IS NOT INITIAL.
*          CALL METHOD obj_container_0110->free.
*        ENDIF.
*        FREE:  obj_alv_0100,obj_toolbar_0110.
*        FREE: obj_container_0110.
*      ENDIF.
*      MOVE '/PRODUT_COMP' TO gs_variant-variant.
*    ENDIF.
*    "
*
*  ENDIF.
* PBI - 70679 - Fim - CBRAND
endform.

form f_criar_catalog using p_screen.

  free: wa_fcat, it_fcat.

  case p_screen.
    when '0100'.

      perform f_estrutura_alv using:

       01  'KNA1'      'KUNNR'            'IT_SAIDA_0100' 'PARID'    'Cliente/Fornecedor'           '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'KNA1'      'NAME1'            'IT_SAIDA_0100' 'NAME1'    'Nome Cliente/Fornecedor'      '27'   ' '    ''  ' ' ' ' ' ' ' ' '' .

* PBI - 70679 - Inicio - CBRAND
*      IF rb_prod IS NOT INITIAL.
*        PERFORM f_estrutura_alv USING:
*        03  ''          ''                 'IT_SAIDA_0100' 'ZUONR'    'Atribuição'                   '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*        04  'BSID'      'WAERS'            'IT_SAIDA_0100' 'WAERS'    'Moeda'                        '05'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*        05  'BSID'      'BSCHL'            'IT_SAIDA_0100' 'BSCHL'     'CL'                          '02'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*        06  'BSID'      'WRBTR'            'IT_SAIDA_0100' 'DMBTR'    'Valor R$'                     '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*        06  'BSID'      'WRBTR'            'IT_SAIDA_0100' 'WRBTR'    'Valor Doc'                    '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*        07  'BSID'      'DMBE2'            'IT_SAIDA_0100' 'DMBE2'    'Valor U$'                     '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*        08  'BSID'      'DMBTR'            'IT_SAIDA_0100' 'FT_DMBTR' 'Tot.Fat.Doc'                  '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*        09  'BSID'      'DMBE2'            'IT_SAIDA_0100' 'DF_DMBE2' 'Dif.FatXVlr Doc'              '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*        10  'BSID'      'DMBE2'            'IT_SAIDA_0100' 'FT_DMBE2' 'Tot.Fat.U$'                   '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*        11  ''          ''                 'IT_SAIDA_0100' 'QTDE_AD'  'Qtde.Adt.'                    '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*        11  ''          ''                 'IT_SAIDA_0100' 'QTDE_FT'  'Qtde.Fat.'                    '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*        12  ''          ''                 'IT_SAIDA_0100' 'COMP'     'Compensar'                    '10'   ' '    ''  ' ' 'C' 'X' ' ' '' ,
*        13  ''          ''                 'IT_SAIDA_0100' 'VIEW_CP'  'Ctr.Part.'                    '09'   ' '    ''  ' ' 'C' 'X' ' ' '' ,
*        14  ''          ''                 'IT_SAIDA_0100' 'VIEW_AD'  'Partidas'                     '09'   ' '    ''  ' ' 'C' 'X' ' ' '' ,
*        15  ''          ''                 'IT_SAIDA_0100' 'GSBER'    'DIV'                          '06'   ' '    ' ' ' ' ' ' ' ' ' ' '' ,
*        16  ''          ''                 'IT_SAIDA_0100' 'XREF3'    'Ref 3'                        '20'   ' '    ' ' ' ' ' ' ' ' ' ' '' ,
*        17  ''          ''                 'IT_SAIDA_0100' 'XREF1'    'Ref 1'                        '12'   ' '    ' ' ' ' ' ' ' ' ' ' '' ,
*        18  ''          ''                 'IT_SAIDA_0100' 'HKONT'    'Razão'                        '12'   ' '    ' ' ' ' ' ' ' ' ' ' '' ,
*        19  ''          ''                 'IT_SAIDA_0100' 'SGTXT'    'Texto Item'                   '15'   ' '    ''  ' ' ' ' ' ' ' ' '' .
*      ELSE.
* PBI - 70679 - Fim  CBRAND
      perform f_estrutura_alv using:
      03  'BSID'      'BELNR'            'IT_SAIDA_0100' 'BELNR'    'Doc. Contábil'                '12'   ' '    ''  ' ' ' ' 'X' ' ' '' ,
      04  'BSID'      'BLDAT'            'IT_SAIDA_0100' 'BLDAT'    'Dt.Doc.'                      '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      05  'BSID'      'BUDAT'            'IT_SAIDA_0100' 'BUDAT'    'Dt.Lcto'                      '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      06  'BSID'      'WAERS'            'IT_SAIDA_0100' 'WAERS'    'Moeda'                        '05'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      07  'BSID'      'DMBTR'            'IT_SAIDA_0100' 'DMBTR'    'Valor R$'                     '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      07  'BSID'      'DMBTR'            'IT_SAIDA_0100' 'WRBTR'    'Valor Doc'                    '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      08  'BSID'      'DMBE2'            'IT_SAIDA_0100' 'DMBE2'    'Valor U$'                     '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      09  'BKPF'      'KURSF'            'IT_SAIDA_0100' 'KURSF'    'Tx.Câmbio'                    '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      10  'BSID'      'VBEL2'            'IT_SAIDA_0100' 'OVPED'    'Nro. OV/Ped.'                 '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      10  'BSID'      'POSN2'            'IT_SAIDA_0100' 'ITMOP'    'Itm.OV/Ped.'                  '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      10  'ZSDT0041'  'DOC_SIMULACAO'    'IT_SAIDA_0100' 'DCSIM'    'Simulador'                    '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      11  'VBAK'      'SPART'            'IT_SAIDA_0100' 'SPART'    'S.A'                          '03'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      12  'TSPAT'     'VTEXT'            'IT_SAIDA_0100' 'VTEXT'    'Setor Atividade'              '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      13  'BSID'      'DMBTR'            'IT_SAIDA_0100' 'FT_DMBTR' 'Tot.Fat.Doc'                  '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      14  'BSID'      'DMBE2'            'IT_SAIDA_0100' 'DF_DMBE2' 'Dif.FatXVlr Doc'              '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      14  'BSID'      'DMBE2'            'IT_SAIDA_0100' 'FT_DMBE2' 'Tot.Fat.U$'                   '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      15  ''          ''                 'IT_SAIDA_0100' 'QTDE_FT'  'Qtde.Fat.'                    '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      16  ''          ''                 'IT_SAIDA_0100' 'COMP'     'Compensar'                    '10'   ' '    ''  ' ' 'C' 'X' ' ' '' ,
      16  ''          ''                 'IT_SAIDA_0100' 'VIEW_CP'  'Ctr.Part.'                    '09'   ' '    ''  ' ' 'C' 'X' ' ' '' ,
      17  ''          ''                 'IT_SAIDA_0100' 'ZUONR'    'Atribuição'                   '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      17  ''          ''                 'IT_SAIDA_0100' 'XBLNR'    'Referência'                   '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      18  ''          ''                 'IT_SAIDA_0100' 'SGTXT'    'Texto Item'                   '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      19  ''          ''                 'IT_SAIDA_0100' 'COUNT'    'Count'                        '06'   ' '    'X' ' ' ' ' ' ' ' ' '' ,

      19  ''          ''                 'IT_SAIDA_0100' 'KIDNO'    'Ref. Pgto'                    '15'   ' '    ' ' ' ' ' ' ' ' ' ' '' ,
      19  ''          ''                 'IT_SAIDA_0100' 'XREF1'    'Ref 1'                        '12'   ' '    ' ' ' ' ' ' ' ' ' ' '' ,
      19  ''          ''                 'IT_SAIDA_0100' 'XREF3'    'Ref 3'                        '20'   ' '    ' ' ' ' ' ' ' ' ' ' '' ,
      19  ''          ''                 'IT_SAIDA_0100' 'HKONT'    'Razão'                        '12'   ' '    ' ' ' ' ' ' ' ' ' ' '' ,
      19  ''          ''                 'IT_SAIDA_0100' 'GSBER'    'DIV'                          '06'   ' '    ' ' ' ' ' ' ' ' ' ' '' .
*      ENDIF.

    when '0110'.

      perform f_estrutura_alv using:

       01  ''          ''                 'IT_SAIDA_0110' 'IC_MANUAL'   'LM'                  '04'   ' '    ' '  ' ' 'C' ' ' ' ' ' ' ,
       01  ''          ''                 'IT_SAIDA_0110' 'CHECK'      'Check'                '05'   'X'    ' '  ' ' ' ' ' ' ' ' 'X' ,
       "01  'KNA1'      'KUNNR'            'IT_SAIDA_0110' 'KUNNR'      'Cliente'              '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       "02  'KNA1'      'NAME1'            'IT_SAIDA_0110' 'NAME1'      'Nome Cliente'         '20'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       03  'BSID'      'BELNR'            'IT_SAIDA_0110' 'BELNR'      'Doc.Ctb.'             '10'   ' '    ' '  ' ' ' ' 'X' ' ' ' ' ,
       04  'BSID'      'BLDAT'            'IT_SAIDA_0110' 'BLDAT'      'Dt.Doc.'              '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       05  'BSID'      'BUDAT'            'IT_SAIDA_0110' 'BUDAT'      'Dt.Lcto'              '10'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       05  ''          ''            'IT_SAIDA_0110' 'HKONT'      'Conta'                '10'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       05  'BSID'      'BSCHL'            'IT_SAIDA_0110' 'BSCHL'      'CL'                   '02'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       06  'BSID'      'UMSKS'            'IT_SAIDA_0110' 'UMSKS'      'Cód.Razão Especial'   '01'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       06  'BSID'      'WAERS'            'IT_SAIDA_0110' 'WAERS'      'Moeda'                '05'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       07  'BSID'      'DMBTR'            'IT_SAIDA_0110' 'DMBTR_AUX'  'Valor Doc'            '12'   ' '    'X'  ' ' ' ' ' ' ' ' ' ' ,
* PBI - 70679 CBRAND
"       08  'BSID'      'DMBE2'            'IT_SAIDA_0110' 'DMBE2_AUX'  'Valor U$'             '12'   ' '    'X'  ' ' ' ' ' ' ' ' ' ' ,
       09  'BSID'      'DMBTR'            'IT_SAIDA_0110' 'VLR_RSD'    'Vlr.Residual'         '12'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
* PBI - 70679 - Inicio - CBRAND
"       09  'BSID'      'DMBTR'            'IT_SAIDA_0110' 'DMBTR'      'Vlr.Comp.R$'          '12'   ' '    'X'  ' ' ' ' ' ' ' ' ' ' ,
       09  'BSID'      'DMBTR'            'IT_SAIDA_0110' 'WRBTR'      'Vlr.Comp.doc'         '12'   'X'    'X'  ' ' ' ' ' ' ' ' ' ' ,
* PBI - 70679 - Inicio - CBRAND
"       10  'BSID'      'DMBE2'            'IT_SAIDA_0110' 'DMBE2'      'Vlr.Comp.U$'          '12'   'X'    'X'  ' ' ' ' ' ' ' ' ' ' ,
       10  'BSID'      'ZFBDT'            'IT_SAIDA_0110' 'ZFBDT'      'Dt.Venc.'             '10'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       11  'BSID'      'VBEL2'            'IT_SAIDA_0110' 'OVPED'      'Nro. OV/Ped.'         '13'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       11  'BSID'      'POSN2'            'IT_SAIDA_0110' 'ITMOP'      'Itm.OV/Ped.'          '11'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       11  ''          ''                 'IT_SAIDA_0110' 'ZUONR'      'Atribuição'           '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       10  'ZSDT0041'  'DOC_SIMULACAO'    'IT_SAIDA_0110' 'DCSIM'      'Simulador'            '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       12  ''          ''                 'IT_SAIDA_0110' 'PART_PRINC' 'Aplic.Part.Princ.'    '18'   'X'    ' '  ' ' ' ' ' ' ' ' 'X' ,
       13  'BSID'      'SGTXT'            'IT_SAIDA_0110' 'SGTXT_RSD ' 'Txt. Residual'        '35'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       19  ''          ''                 'IT_SAIDA_0110' 'KIDNO'      'Ref. Pgto'            '15'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       19  ''          ''                 'IT_SAIDA_0110' 'XREF1'      'Ref 1'                '12'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       19  ''          ''                 'IT_SAIDA_0110' 'XREF3'      'Ref 3'                '20'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       19  ''          ''                 'IT_SAIDA_0110' 'GSBER'      'DIV'                  '06'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       19  ''          ''                 'IT_SAIDA_0110' 'XBLNR'      'Referencia'           '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' .

    when '0120'.

      perform f_estrutura_alv using:

       01  'ZFIT0139'      'BUKRS'         'IT_SAIDA_0110' 'BUKRS'        'Empresa'             '07'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       "02  'ZFIT0139'      'GJAHR'         'IT_SAIDA_0110' 'GJAHR'        'Ano'                 '04'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       "03  'ZFIT0139'      'BELNR'         'IT_SAIDA_0110' 'BELNR'        'Doc.Ctb.'            '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       "04  'ZFIT0139'      'BUZEI'         'IT_SAIDA_0110' 'BUZEI'        'Item'                '04'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       05  'ZFIT0139'      'AUGBL'         'IT_SAIDA_0110' 'AUGBL'        'Doc.Comp.'           '10'   ' '    ' '  ' ' ' ' 'X' ' ' ' ' ,
       07  'ZFIT0139'      'OBJ_KEY'       'IT_SAIDA_0110' 'OBJ_KEY'      'Chv.Ref.'            '20'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       08  'ZFIT0139'      'AUGDT'         'IT_SAIDA_0110' 'AUGDT'        'Dt.Comp.'            '12'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       09  'ZFIT0139'      'KUNNR'         'IT_SAIDA_0110' 'KUNNR'        'Cliente'             '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       10  'ZFIT0139'      'LIFNR'         'IT_SAIDA_0110' 'LIFNR'        'Fornecedor'          '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       12  'ZFIT0139'      'BELNR_GER'     'IT_SAIDA_0110' 'BELNR_GER'    'Doc.Recls.'          '10'   ' '    ' '  ' ' ' ' 'X' ' ' ' ' ,
       13  'ZFIT0139'      'STBLG_GER'     'IT_SAIDA_0110' 'STBLG_GER'    'Doc.Estorno.Recls.'  '18'   ' '    ' '  ' ' ' ' 'X' ' ' ' ' ,
       14  ''              ''              'IT_SAIDA_0110' 'ST_CTB'       'St.Ctb'              '06'   ' '    ' '  ' ' 'C' 'X' ' ' ' ' ,
       15  'ZFIT0139'      'ANULADO'       'IT_SAIDA_0110' 'ANULADO'      'Anulado'             '06'   ' '    ' '  ' ' 'C' ' ' ' ' ' ' ,
       17  'ZFIT0139'      'DT_REGISTRO'   'IT_SAIDA_0110' 'DT_REGISTRO'  'Dt.Registro'         '12'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       18  'ZFIT0139'      'HR_REGISTRO'   'IT_SAIDA_0110' 'HR_REGISTRO'  'Hr.Registro'         '12'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' .

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

  "CASE P_SCREEN.
  "  WHEN '0112'.
  append cl_gui_alv_grid=>mc_fc_loc_delete_row    to it_exclude_fcode.
  append cl_gui_alv_grid=>mc_fc_loc_insert_row    to it_exclude_fcode.
  "ENDCASE.

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
         wa_saida_0110,
         it_saida_0110[],
         tg_bsid_adt[],
         tg_bsid_comp[],
         tg_vbak[],
         tg_bsik_adt[],
         tg_bsik_comp[],
         tg_ekko[],
         tg_kna1[],
         tg_lfa1[],
         tg_bseg[],
         tg_t001[],
         tg_ekkn[],
         tg_bsis_cbanco[],
         tg_zsdt0041[],
         tg_zsdt0090[],
         tg_vbfa_rd[],
         r_vbeln[],
         tg_tspat.

  clear: vg_not_found.


  clear: lva_bseg-zlspr,
         lva_bseg-zlsch,
         lva_bseg-hbkid,
         lva_bseg-bvtyp,
         lva_bseg-zfbdt.

endform.

form f_config_ranges.

  clear: r_fdlev_banco, r_fdlev_banco[],
         r_bukrs,       r_bukrs[],
         r_parid,       r_parid[],
         r_ovped,       r_ovped[],
         r_augdt,       r_augdt[],
         r_sgtxt,       r_sgtxt[],
         r_zuonr,       r_zuonr[],
         r_dcsim,       r_dcsim[].

  r_bukrs[] = p_bukrs[].
  r_parid[] = p_parid[].
  r_ovped[] = p_ovped[].
  r_augdt[] = p_augdt[].
  r_sgtxt[] = p_sgtxt[].
  r_zuonr[] = p_zuonr[].
  r_dcsim[] = p_dcsim[].

  r_fdlev_banco-sign   = 'I'.
  r_fdlev_banco-option = 'EQ'.
  r_fdlev_banco-low    = 'F0'.
  append r_fdlev_banco.

  r_fdlev_banco-low    = 'B2'.
  append r_fdlev_banco.

endform.

form f_hotspot_click  using  p_alv
                             i_row_id     type lvc_s_row
                             i_column_id  type lvc_s_col
                             is_row_no    type lvc_s_roid.

  data: it_rsparams type table of rsparams,
        wa_rsparams type rsparams.

  data: opt         type ctu_params,
        vl_error    type c,
        vl_doc_comp type bsad-belnr.


  case p_alv.
    when '0100'.
      clear: wa_saida_0100, wa_saida_0110.
      case i_column_id.
        when 'BELNR'.
          read table it_saida_0100 into wa_saida_0100 index i_row_id.
          check ( sy-subrc = 0 ) and ( wa_saida_0100-belnr is not initial ).

          set parameter id 'BLN' field wa_saida_0100-belnr.
          set parameter id 'BUK' field wa_saida_0100-bukrs.
          set parameter id 'GJR' field wa_saida_0100-budat(4).

          call transaction 'FB03' and skip first screen.

        when 'COMP'.

          if p_blt_cp is initial.
            message 'Informe um Tp.Documento para Compensação!' type 'S'.
            exit.
          endif.

          read table it_saida_0100 into wa_saida_0100 index i_row_id.
          check ( sy-subrc = 0 ).

          case wa_saida_0100-st_comp.
            when '1' or '3' . "Liberado Compensação ( Total Adiamento = Total Partidas Compensar )
              perform f_get_part_comp using wa_saida_0100.
              perform f_bapi_f51 using abap_false
                              changing wa_saida_0100
                                       vl_error
                                       vl_doc_comp.
              perform f_renovar_cons.
            when '2'. "Selecionar Partidas para compensar
              perform f_get_part_comp using wa_saida_0100.
              "CHECK IT_SAIDA_0110[] IS NOT INITIAL. "Ajuste p/ Compensar com Descontos/Juros 02.05.2018
              call screen 0110 starting at 01 01 ending at 165 20 .
              perform f_renovar_cons.
          endcase.
        when 'VIEW_AD'.

          read table it_saida_0100 into wa_saida_0100 index i_row_id.
          check ( sy-subrc = 0 ).
          refresh it_display.
          loop at tg_bsik_copy where lifnr = wa_saida_0100-parid
                               and   zuonr = wa_saida_0100-zuonr.
            move-corresponding tg_bsik_copy to wa_display.
            append wa_display to it_display.
          endloop.


          try.
              call method cl_salv_table=>factory
                importing
                  r_salv_table = gr_table
                changing
                  t_table      = it_display.


              gr_table->set_screen_popup(
                start_column = 10
                end_column   = 150
                start_line   = 1
                end_line     = 6 ).

              call method gr_table->display.
            catch cx_root.

          endtry.



        when 'VIEW_CP'.

          if p_blt_cp is initial.
            message 'Informe um Tp.Documento para Compensação!' type 'S'.
            exit.
          endif.

          read table it_saida_0100 into wa_saida_0100 index i_row_id.
          check ( sy-subrc = 0 ).

          wa_saida_0100-st_comp = '2'.

          perform f_get_part_comp using wa_saida_0100.
          check it_saida_0110[] is not initial.
          call screen 0110 starting at 01 01 ending at 165 20 .
          perform f_renovar_cons.

      endcase.
    when '0110'.
      case i_column_id.
        when 'BELNR'.
          read table it_saida_0110 into wa_saida_0110 index i_row_id.
          check ( sy-subrc = 0 ) and ( wa_saida_0110-belnr is not initial ).

          set parameter id 'BLN' field wa_saida_0110-belnr.
          set parameter id 'BUK' field wa_saida_0110-bukrs.
          set parameter id 'GJR' field wa_saida_0110-budat(4).

          call transaction 'FB03' and skip first screen.
      endcase.
    when '0120'.
      case i_column_id.
        when 'ST_CTB'.
          clear: tg_zib_err[], wa_saida_0120.
          read table it_saida_0120 into wa_saida_0120 index i_row_id.

          check ( sy-subrc = 0 ) and ( wa_saida_0120-obj_key is not initial ).

          select distinct *
            from zib_contabil_err as a into corresponding fields of table tg_zib_err
           where obj_key  eq wa_saida_0120-obj_key.

          check tg_zib_err[] is not initial.

          perform f_montar_layout_log_erro.

          call function 'REUSE_ALV_GRID_DISPLAY'
            exporting
              it_fieldcat           = estrutura[]
              i_save                = 'A'
              i_screen_start_column = 3
              i_screen_start_line   = 3
              i_screen_end_column   = 100
              i_screen_end_line     = 13
            tables
              t_outtab              = tg_zib_err.
        when 'AUGBL'.
          read table it_saida_0120 into wa_saida_0120 index i_row_id.
          check ( sy-subrc = 0 ) and ( wa_saida_0120-augbl is not initial ).

          set parameter id 'BLN' field wa_saida_0120-augbl.
          set parameter id 'BUK' field wa_saida_0120-bukrs.
          set parameter id 'GJR' field wa_saida_0120-augdt(4).

          call transaction 'FB03' and skip first screen.
        when 'BELNR_GER'.
          read table it_saida_0120 into wa_saida_0120 index i_row_id.
          check ( sy-subrc = 0 ) and ( wa_saida_0120-belnr_ger is not initial ).

          set parameter id 'BLN' field wa_saida_0120-belnr_ger.
          set parameter id 'BUK' field wa_saida_0120-bukrs.
          set parameter id 'GJR' field wa_saida_0120-gjahr_ger.

          call transaction 'FB03' and skip first screen.
        when 'STBLG_GER'.
          read table it_saida_0120 into wa_saida_0120 index i_row_id.
          check ( sy-subrc = 0 ) and ( wa_saida_0120-stblg_ger is not initial ).

          set parameter id 'BLN' field wa_saida_0120-stblg_ger.
          set parameter id 'BUK' field wa_saida_0120-bukrs.
          set parameter id 'GJR' field wa_saida_0120-gjahr_ger.

          call transaction 'FB03' and skip first screen.
      endcase.
  endcase.


endform.

form f_get_part_comp  using  p_saida_0100 type ty_saida_0100.

  refresh tl_parametros.
  call function 'SUSR_USER_PARAMETERS_GET'
    exporting
      user_name           = sy-uname
    tables
      user_parameters     = tl_parametros
    exceptions
      user_name_not_exist = 1
      others              = 2.
  if sy-subrc <> 0.
  endif.

  clear vg_parausd.
  read table tl_parametros into wl_parametros
   with key parid = 'Z105USD'.

  if sy-subrc eq 0. "Exibe USD
    vg_parausd = 'X'.
  endif.
  clear: it_saida_0110[].

  check p_saida_0100-kursf ne 0.

  case p_saida_0100-koart.
    when 'D'. "Cliente

*--------------------------------------------------------------------*
*  Carrega Partidas Compensar Cliente
*--------------------------------------------------------------------*

      perform f_get_bsid_comp tables tg_bsid_comp_aux
                               using p_saida_0100-bukrs
                                     p_saida_0100-belnr
                                     p_saida_0100-buzei
                                     p_saida_0100-gjahr
                                     p_saida_0100-parid
                                     p_saida_0100-ovped
                                     p_saida_0100-sgtxt
                                     p_saida_0100-zuonr
                                     p_saida_0100-anln1
                                     p_saida_0100-anln2
                                     p_saida_0100-dcsim.

      loop at tg_bsid_comp_aux into tg_bsid_comp.

        clear: wa_saida_0110, tg_kna1, gt_estilo[].

        perform f_moeda_empresa using tg_bsid_comp-bukrs
                                      'X'.
        if ( sy-subrc ne 0 ).
          return.
        endif.

        read table tg_bkpf with key bukrs = tg_bsid_comp-bukrs
                                    belnr = tg_bsid_comp-belnr
                                    gjahr = tg_bsid_comp-gjahr.
        check sy-subrc = 0.

        read table tg_kna1 with key kunnr = tg_bsid_comp-kunnr.
        check sy-subrc = 0.

        wa_saida_0110-bukrs     = tg_bsid_comp-bukrs.
        wa_saida_0110-parid     = tg_kna1-kunnr.
        wa_saida_0110-name1     = tg_kna1-name1.
        wa_saida_0110-belnr     = tg_bsid_comp-belnr.
        wa_saida_0110-buzei     = tg_bsid_comp-buzei.
        wa_saida_0110-gjahr     = tg_bsid_comp-gjahr.
        wa_saida_0110-bldat     = tg_bsid_comp-bldat.
        wa_saida_0110-budat     = tg_bsid_comp-budat.
        wa_saida_0110-waers     = tg_bsid_comp-waers.
        wa_saida_0110-wrbtr     = tg_bsid_comp-wrbtr.
        wa_saida_0110-dmbtr     = tg_bsid_comp-dmbtr.
        wa_saida_0110-dmbe2     = tg_bsid_comp-dmbe2.
        wa_saida_0110-dmbtr_au2 = tg_bsid_comp-dmbtr.
        wa_saida_0110-dmbtr_aux = tg_bsid_comp-wrbtr.
        wa_saida_0110-dmbe2_aux = tg_bsid_comp-dmbe2.
        wa_saida_0110-hkont     = tg_bsid_comp-hkont.
        wa_saida_0110-bschl     = tg_bsid_comp-bschl.
        wa_saida_0110-umsks     = tg_bsid_comp-umsks.
        wa_saida_0110-umskz     = tg_bsid_comp-umskz.
        wa_saida_0110-gsber     = tg_bsid_comp-gsber.
        wa_saida_0110-sgtxt     = tg_bsid_comp-sgtxt.
        wa_saida_0110-shkzg     = tg_bsid_comp-shkzg.

*** - PBI - 70679 - Inicio - CBRAND
        read table tg_bseg with key bukrs = wa_saida_0110-bukrs
                                    belnr = wa_saida_0110-belnr.

        wa_saida_0110-zfbdt     = tg_bsid_comp-zfbdt + tg_bseg-zbd1t .
* wa_saida_0110-zfbdt     = tg_bsid_comp-zfbdt.
*** - PBI - 70679 - Fim - CBRAND

        wa_saida_0110-zbd1t     = tg_bsid_comp-zbd1t.
        wa_saida_0110-ovped     = tg_bsid_comp-vbel2.
        wa_saida_0110-posn2     = tg_bsid_comp-posn2.
        wa_saida_0110-itmop     = tg_bsid_comp-posn2.
        wa_saida_0110-dcsim     = tg_bsid_comp-dcsim.
        wa_saida_0110-kidno     = tg_bsid_comp-kidno.
        wa_saida_0110-kidno     = tg_bsid_comp-kidno.
        wa_saida_0110-xref1     = tg_bsid_comp-xref1.
        wa_saida_0110-xref3     = tg_bsid_comp-xref3.
        wa_saida_0110-blart     = tg_bsid_comp-blart.
        wa_saida_0110-zuonr     = tg_bsid_comp-zuonr.
        wa_saida_0110-zterm     = tg_bsid_comp-zterm.
        wa_saida_0110-anln1     = tg_bsid_comp-anln1.
        wa_saida_0110-anln2     = tg_bsid_comp-anln2.
        wa_saida_0110-xblnr     = tg_bkpf-xblnr.
        wa_saida_0110-koart     = p_saida_0100-koart.
        wa_saida_0110-sgtxt_rsd = wa_saida_0110-sgtxt.

        perform f_get_taxa using tg_bkpf
                                 wa_saida_0110-dmbtr
                                 wa_saida_0110-dmbe2
                        changing wa_saida_0110-kursf.

        if wa_saida_0100-st_comp = '1'. "Total Partidas = Valor Adiantamento.
          wa_saida_0110-check = 'X'.
        endif.

        if p_saida_0100-waers = tg_t001-waers.
          wa_saida_0110-dmbe2 = wa_saida_0110-dmbtr / wa_saida_0110-kursf.
        else.
          wa_saida_0110-dmbtr = wa_saida_0110-dmbe2 * wa_saida_0110-kursf.
        endif.

        wl_estilo-fieldname    = 'BSCHL'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.

        wl_estilo-fieldname    = 'DMBE2'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.

        wl_estilo-fieldname    = 'DMBTR'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.

        wl_estilo-fieldname    = 'HKONT'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.

        if wa_saida_0110-manual is initial.
          wl_estilo-fieldname    = 'PART_PRINC'.
          wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
          append wl_estilo to gt_estilo.
        endif.

        wl_estilo-fieldname    = 'UMSKS'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.


        wl_estilo-fieldname    = 'WRBTR'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.

        wl_estilo-fieldname    = 'ZFBDT'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.

        insert lines of gt_estilo into table wa_saida_0110-estilo.

        append wa_saida_0110 to it_saida_0110.

      endloop.

    when 'K'. "Fornecedor

*--------------------------------------------------------------------*
*  Carrega Partidas Compensar Fornecedor
*--------------------------------------------------------------------*

      perform f_get_bsik_comp tables tg_bsik_comp_aux
                               using p_saida_0100-bukrs
                                     p_saida_0100-belnr
                                     p_saida_0100-buzei
                                     p_saida_0100-gjahr
                                     p_saida_0100-parid
                                     p_saida_0100-ovped
                                     p_saida_0100-sgtxt
                                     p_saida_0100-zuonr
                                     p_saida_0100-anln1
                                     p_saida_0100-anln2.

      loop at tg_bsik_comp_aux into tg_bsik_comp.

        clear: wa_saida_0110, tg_lfa1, gt_estilo[].

        perform f_moeda_empresa using tg_bsik_comp-bukrs
                                      'X'.
        if ( sy-subrc ne 0 ).
          return.
        endif.

        read table tg_bkpf with key bukrs = tg_bsik_comp-bukrs
                                    belnr = tg_bsik_comp-belnr
                                    gjahr = tg_bsik_comp-gjahr.
*        CHECK sy-subrc = 0.

        read table tg_lfa1 with key lifnr = tg_bsik_comp-lifnr.

        check sy-subrc = 0.

        wa_saida_0110-bukrs     = tg_bsik_comp-bukrs.
        wa_saida_0110-parid     = tg_lfa1-lifnr.
        wa_saida_0110-name1     = tg_lfa1-name1.
        wa_saida_0110-belnr     = tg_bsik_comp-belnr.
        wa_saida_0110-buzei     = tg_bsik_comp-buzei.
        wa_saida_0110-gjahr     = tg_bsik_comp-gjahr.
        wa_saida_0110-bldat     = tg_bsik_comp-bldat.
        wa_saida_0110-budat     = tg_bsik_comp-budat.
        wa_saida_0110-waers     = tg_bsik_comp-waers.
        wa_saida_0110-wrbtr     = tg_bsik_comp-wrbtr.
        wa_saida_0110-dmbtr     = tg_bsik_comp-dmbtr.
        wa_saida_0110-dmbe2     = tg_bsik_comp-dmbe2.
        wa_saida_0110-dmbtr_au2 = tg_bsik_comp-dmbtr.
        wa_saida_0110-dmbtr_aux = tg_bsik_comp-wrbtr.
        wa_saida_0110-dmbe2_aux = tg_bsik_comp-dmbe2.
        wa_saida_0110-hkont     = tg_bsik_comp-hkont.
        wa_saida_0110-bschl     = tg_bsik_comp-bschl.
        wa_saida_0110-umsks     = tg_bsik_comp-umsks.
        wa_saida_0110-umskz     = tg_bsik_comp-umskz.
        wa_saida_0110-gsber     = tg_bsik_comp-gsber.
        wa_saida_0110-sgtxt     = tg_bsik_comp-sgtxt.
        wa_saida_0110-shkzg     = tg_bsik_comp-shkzg.

*** - PBI - 70679 - Inicio - CBRAND
        read table tg_bseg with key bukrs = wa_saida_0110-bukrs
                                    belnr = wa_saida_0110-belnr.

        wa_saida_0110-zfbdt     = tg_bsik_comp-zfbdt + tg_bseg-zbd1t.
* wa_saida_0110-zfbdt     = tg_bsik_comp-zfbdt
*** - PBI - 70679 - Fim - CBRAND
        wa_saida_0110-zbd1t     = tg_bsik_comp-zbd1t.
        wa_saida_0110-ovped     = tg_bsik_comp-ebeln.
        wa_saida_0110-ebelp     = tg_bsik_comp-ebelp.
        wa_saida_0110-itmop     = tg_bsik_comp-ebelp.
        wa_saida_0110-kidno     = tg_bsik_comp-kidno.
        wa_saida_0110-xref1     = tg_bsik_comp-xref1.
        wa_saida_0110-xref3     = tg_bsik_comp-xref3.
        wa_saida_0110-blart     = tg_bsik_comp-blart.
        wa_saida_0110-zuonr     = tg_bsik_comp-zuonr.
        wa_saida_0110-zterm     = tg_bsik_comp-zterm.
        wa_saida_0110-anln1     = tg_bsik_comp-anln1.
        wa_saida_0110-anln2     = tg_bsik_comp-anln2.
        wa_saida_0110-xblnr     = tg_bkpf-xblnr.
        wa_saida_0110-koart     = p_saida_0100-koart.
        wa_saida_0110-sgtxt_rsd = wa_saida_0110-sgtxt.

        perform f_get_taxa using tg_bkpf
                                 wa_saida_0110-dmbtr
                                 wa_saida_0110-dmbe2
                        changing wa_saida_0110-kursf.

        if wa_saida_0100-st_comp = '1'. "Total Partidas = Valor Adiantamento.
          wa_saida_0110-check = 'X'.
        endif.

        if p_saida_0100-waers = tg_t001-waers.
          wa_saida_0110-dmbe2 = wa_saida_0110-dmbtr / wa_saida_0110-kursf.
        else.
          wa_saida_0110-dmbtr = wa_saida_0110-dmbe2 * wa_saida_0110-kursf.
        endif.

        wl_estilo-fieldname    = 'BSCHL'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.

        wl_estilo-fieldname    = 'DMBE2'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.

        wl_estilo-fieldname    = 'DMBTR'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.

        wl_estilo-fieldname    = 'HKONT'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.

        if wa_saida_0110-manual is initial.
          wl_estilo-fieldname    = 'PART_PRINC'.
          wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
          append wl_estilo to gt_estilo.
        endif.

        wl_estilo-fieldname    = 'UMSKS'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.

        wl_estilo-fieldname    = 'WRBTR'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.

        wl_estilo-fieldname    = 'ZFBDT'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        append wl_estilo to gt_estilo.

        insert lines of gt_estilo into table wa_saida_0110-estilo.

        append wa_saida_0110 to it_saida_0110.

      endloop.

  endcase.

  clear: wa_cabecalho_0110.
  if vg_parausd = 'X'.
    wa_cabecalho_0110-adt_dmbtr2 = p_saida_0100-dmbtr.
  else.
    wa_cabecalho_0110-adt_dmbtr2 = p_saida_0100-wrbtr.
  endif.
  wa_cabecalho_0110-adt_dmbtr = p_saida_0100-wrbtr.
  wa_cabecalho_0110-adt_dmbe2 = p_saida_0100-dmbe2.
  wa_cabecalho_0110-waers     = p_saida_0100-waers.
  wa_cabecalho_0110-sgtxt_rsd = 'Desmembramento Adiantamento'.

  if p_saida_0100-koart = 'D'.
    read table tg_kna1 with key kunnr = p_saida_0100-parid.
    if sy-subrc = 0.
      concatenate p_saida_0100-parid '-' tg_kna1-name1
             into wa_cabecalho_0110-ds_cliente separated by space.
    endif.
  else.
    read table tg_lfa1 with key lifnr = p_saida_0100-parid.
    if sy-subrc = 0.
      concatenate p_saida_0100-parid '-' tg_lfa1-name1
             into wa_cabecalho_0110-ds_cliente separated by space.
    endif.
  endif.

endform.

form f_bapi_f51 using p_residual_comp  type c  "Opção para deixar residual na Compensação
             changing p_saida_0100     type ty_saida_0100
                      p_erro
                      p_doc_comp       type bsad-belnr.


  data: it_retorno_rfc like zgl002_comp_f44 occurs 0 with header line,
        wa_retorno     like line of it_retorno_rfc.

  data: wl_0122      type zfit0122,
        vl_name1     type kna1-name1,
        it_0110_comp type table of ty_saida_0110.

  data: l_auglv   type t041a-auglv   value 'UMBUCHNG', "Posting with Clearing
        l_tcode   type sy-tcode      value 'FB05',     "You get an error with any other value
        l_sgfunct type rfipi-sgfunct value 'C'.        "Post immediately

  data: lt_blntab  type standard table of blntab  with header line,
        lt_ftclear type standard table of ftclear with header line,
        lt_ftpost  type standard table of ftpost  with header line,
        lt_fttax   type standard table of fttax   with header line,
        lds_return type bapiret2.

  data: wa_sai_0110_tmp type ty_saida_0110.

  data: vdata(10),
        vdata_venc(10),
        cnum_seq(2),
        wl_vlr(16),
        wl_taxa(16),
        wl_vlrc(16),
        wl_vlrn        type p decimals 2,
        vcampo(15),
        v_kur          type bkpf-kursf,
        vvalor_bax     type zfit0042-dmbe2,
        msg_no         type t100-msgnr,
        msg_text       type string,
        p_mode         like rfpdo-allgazmd,
        vl_dt_mov      type sy-datum,
        count_ft       type ftpost-count,
        v_xsimu        type char1.

  clear: p_doc_comp.

  vl_dt_mov = p_augdt-low.

  p_mode = 'N'.

  call function 'POSTING_INTERFACE_START'
    exporting
      i_client           = sy-mandt
      i_function         = 'C'
      i_mode             = p_mode
      i_update           = 'S'
      i_user             = sy-uname
    exceptions
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      others             = 6.

  if sy-subrc ne 0.
    p_erro = 'X'.
    rollback work.
    message 'Houve ao efeturar a compensação' type 'S'.
    return.
  endif.

  concatenate  vl_dt_mov+6(2) vl_dt_mov+4(2) vl_dt_mov(4) into vdata separated by '.'.

  if p_ksfcp > 0.
    write: p_ksfcp to wl_taxa.
  else.
    write: p_saida_0100-kursf to wl_taxa.
  endif.

  condense wl_taxa no-gaps.

  clear: lt_blntab,   lt_blntab[],
         lt_ftclear,  lt_ftclear[],
         lt_ftpost,   lt_ftpost[],
         lt_fttax,    lt_fttax[],
         it_0110_comp[],
         lds_return, p_erro.

  count_ft = 1.

  lt_ftpost-stype = 'K'."Header
  lt_ftpost-count = count_ft.  "number of Dynpro

  lt_ftpost-fnam = 'BKPF-BUKRS'.
  lt_ftpost-fval = p_saida_0100-bukrs.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-WAERS'.
  lt_ftpost-fval = p_saida_0100-waers.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-KURSF'.
  lt_ftpost-fval = wl_taxa.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLDAT'.
  lt_ftpost-fval = vdata.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BUDAT'.
  lt_ftpost-fval = vdata.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-MONAT'.
  lt_ftpost-fval =  vl_dt_mov+4(2).
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLART'.
  lt_ftpost-fval = p_blt_cp.
  append lt_ftpost.

*-BUG 78517-11.05.2022-JT-inicio
  lt_ftpost-fnam = 'BKPF-XBLNR'.
  lt_ftpost-fval = p_saida_0100-ovped.
  append lt_ftpost.
*-BUG 78517-11.05.2022-JT-fim

  it_0110_comp[] = it_saida_0110[].
  sort it_0110_comp by belnr buzei.
  delete adjacent duplicates from it_0110_comp comparing belnr buzei.
  if p_saida_0100-st_comp  = '3'.
    loop at it_0110_comp into wa_saida_0110 where manual is initial.
      wa_saida_0110-check = 'X'.
      modify it_0110_comp from wa_saida_0110 index sy-tabix transporting check.
    endloop.
  endif.
  loop at it_0110_comp into wa_saida_0110 where manual is initial
                                            and check  is not initial.

    lt_ftclear-agkoa  = wa_saida_0110-koart.
    lt_ftclear-agkon  = wa_saida_0110-parid.
    lt_ftclear-agums  = wa_saida_0110-umskz.
    lt_ftclear-agbuk  = wa_saida_0110-bukrs.
    lt_ftclear-xnops  = 'X'.
    lt_ftclear-selfd  = 'BELNR'.
    concatenate wa_saida_0110-belnr wa_saida_0110-budat(4) wa_saida_0110-buzei into lt_ftclear-selvon.
    append lt_ftclear.

    "Opção para deixar residual na Compensação
    if ( p_residual_comp is not initial ) and ( wa_saida_0110-anln1 is not initial ) and ( wa_saida_0110-vlr_rsd > 0 ).
      clear: wa_sai_0110_tmp.
      move-corresponding wa_saida_0110 to wa_sai_0110_tmp.

      perform f_add_part_residual tables lt_ftpost
                                   using wa_sai_0110_tmp
                                changing count_ft
                                         p_erro.
      if p_erro is not initial.
        return.
      endif.
    endif.
  endloop.

  "Adiantamento
  lt_ftclear-agkoa  = p_saida_0100-koart.
  lt_ftclear-agkon  = p_saida_0100-parid.
  lt_ftclear-agums  = p_saida_0100-umskz.
  lt_ftclear-agbuk  = p_saida_0100-bukrs.
  lt_ftclear-xnops  = 'X'.
  lt_ftclear-selfd  = 'BELNR'.
  concatenate p_saida_0100-belnr p_saida_0100-budat(4) p_saida_0100-buzei into lt_ftclear-selvon.
  append lt_ftclear.


  if p_saida_0100-st_comp  = '3' and p_saida_0100-vlr_rsd ne 0.
    clear: wa_sai_0110_tmp.
    move-corresponding p_saida_0100 to wa_sai_0110_tmp.
    wa_sai_0110_tmp-vlr_rsd  = 0.
    wa_sai_0110_tmp-vlr_rsdp = p_saida_0100-vlr_rsd.

    perform f_add_part_residual tables lt_ftpost
                                 using wa_sai_0110_tmp
                              changing count_ft
                                       p_erro.
    if p_erro is not initial.
      return.
    endif.
  endif.

  "Opção para deixar residual na Compensação
  if ( p_residual_comp is not initial ) and ( p_saida_0100-anln1 is not initial ) and ( p_saida_0100-vlr_rsd > 0 ).
    clear: wa_sai_0110_tmp.
    move-corresponding p_saida_0100 to wa_sai_0110_tmp.

    perform f_add_part_residual tables lt_ftpost
                                 using wa_sai_0110_tmp
                              changing count_ft
                                       p_erro.
    if p_erro is not initial.
      return.
    endif.
  endif.

  "Lançamentos Manuais
  loop at it_0110_comp into wa_saida_0110 where manual is not initial
                                            and check  is not initial.

    clear: wl_vlrn, wl_vlrc, vl_name1.

    perform f_moeda_empresa using wa_saida_0110-bukrs
                                  'X'.
    if ( sy-subrc ne 0 ).
      p_erro = 'X'.
      return.
    endif.

    concatenate wa_saida_0110-zfbdt+6(2) wa_saida_0110-zfbdt+4(2) wa_saida_0110-zfbdt(4) into vdata_venc separated by '.'.

    add 1 to count_ft.

    if ( wa_saida_0110-wrbtr = 0 ).
      p_erro = 'X'.
      message 'Existem lançamentos com valores zerados!' type 'S'.
      return.
    endif.

    select single koart
      from tbsl into wa_saida_0110-koart
     where bschl = wa_saida_0110-bschl.

    if ( sy-subrc ne 0 ) or ( wa_saida_0110-koart is initial ).
      p_erro = 'X'.
      message 'Tipo de Conta não encontrado!' type 'S'.
      return.
    endif.

    lt_ftpost-stype = 'P'.
    lt_ftpost-count = count_ft .

    lt_ftpost-fnam = 'RF05A-NEWBS'.
    lt_ftpost-fval =  wa_saida_0110-bschl.
    append lt_ftpost.

    lt_ftpost-fnam = 'BSEG-HKONT'.
    lt_ftpost-fval = wa_saida_0110-hkont.
    append lt_ftpost.

    wl_vlrn = abs( wa_saida_0110-wrbtr ).
    write: wl_vlrn to wl_vlrc.

    lt_ftpost-fnam = 'BSEG-WRBTR'.
    lt_ftpost-fval =  wl_vlrc.
    append lt_ftpost.

    if wa_saida_0110-waers ne 'USD'.
      wl_vlrn = wl_vlrn / abs( wa_saida_0110-kursf ).
      if wl_vlrn < '0.01'.
        wl_vlrn = '0.01'.
        write: wl_vlrn to wl_vlrc.
        lt_ftpost-fnam = 'BSEG-DMBE2'.
        lt_ftpost-fval =  wl_vlrc.
        append lt_ftpost.
      endif.
    endif.

*    IF p_saida_0100-waers NE tg_t001-waers.
*      wl_vlrn      = abs( wa_saida_0110-dmbtr ).
*      WRITE: wl_vlrn TO wl_vlrc.
*      lt_ftpost-fnam = 'BSEG-DMBTR'.
*      lt_ftpost-fval =  wl_vlrc.
*      APPEND lt_ftpost.
*    ELSE.
*      wl_vlrn      = abs( wa_saida_0110-dmbe2 ).
*      WRITE: wl_vlrn TO wl_vlrc.
*      lt_ftpost-fnam = 'BSEG-DMBE2'.
*      lt_ftpost-fval =  wl_vlrc.
*      APPEND lt_ftpost.
*    ENDIF.

    if wa_saida_0110-umsks is not initial.
      lt_ftpost-fnam = 'RF05A-NEWUM'.
      lt_ftpost-fval = wa_saida_0110-umsks.
      append lt_ftpost.
    endif.

    case wa_saida_0110-koart.
      when 'D' or 'K'. "Cliente ou Fornecedor
        if wa_saida_0110-zfbdt is not initial.
          lt_ftpost-fnam = 'BSEG-ZFBDT'.
          lt_ftpost-fval = vdata_venc.
          append lt_ftpost.
        endif.

        lt_ftpost-fnam = 'BSEG-KIDNO'.
        lt_ftpost-fval =  p_saida_0100-kidno.
        append lt_ftpost.


        lt_ftpost-fnam = 'BSEG-GSBER'.
        lt_ftpost-fval =  p_saida_0100-gsber.
        append lt_ftpost.

        lt_ftpost-fnam = 'BSEG-HZUON'.
        lt_ftpost-fval =  p_saida_0100-ovped.
        append lt_ftpost.
      when 'S'. "Razão
        lt_ftpost-fnam = 'BSEG-BUPLA'.
        lt_ftpost-fval =  p_saida_0100-gsber.
        append lt_ftpost.
    endcase.

    lt_ftpost-fnam = 'BSEG-SGTXT'.
    if wa_saida_0110-manual = 'X'.
      lt_ftpost-fval = wa_saida_0110-sgtxt.
    else.
      select single name1
        from kna1 into vl_name1
       where kunnr = wa_saida_0110-hkont.
      if sy-subrc = 0.
        concatenate 'Saldo Residual' vl_name1 into  lt_ftpost-fval separated by space.
      else.
        lt_ftpost-fval = 'Saldo Residual'.
      endif.
    endif.

    append lt_ftpost.

  endloop.

  call function 'POSTING_INTERFACE_CLEARING'
    exporting
      i_auglv                    = l_auglv
      i_tcode                    = l_tcode
      i_sgfunct                  = l_sgfunct
      i_no_auth                  = 'X'
      i_xsimu                    = v_xsimu
    importing
      e_msgid                    = lds_return-id
      e_msgno                    = lds_return-number
      e_msgty                    = lds_return-type
      e_msgv1                    = lds_return-message_v1
      e_msgv2                    = lds_return-message_v2
      e_msgv3                    = lds_return-message_v3
      e_msgv4                    = lds_return-message_v4
    tables
      t_blntab                   = lt_blntab
      t_ftclear                  = lt_ftclear
      t_ftpost                   = lt_ftpost
      t_fttax                    = lt_fttax
    exceptions
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      others                     = 10.

  refresh it_retorno_rfc.
  if lt_blntab[] is initial.
    p_erro = 'X'.
    write lds_return-number to msg_no.
    call function 'MESSAGE_PREPARE'
      exporting
        msg_id                 = lds_return-id
        msg_no                 = msg_no
        msg_var1               = lds_return-message_v1
        msg_var2               = lds_return-message_v2
        msg_var3               = lds_return-message_v3
        msg_var4               = lds_return-message_v4
      importing
        msg_text               = msg_text
      exceptions
        function_not_completed = 1
        message_not_found      = 2
        others                 = 3.
    message msg_text type 'S'.
  else.
    read table lt_blntab index 1.
    clear: wl_0122.
    wl_0122-usnam         = sy-uname.
    wl_0122-data          = sy-datum.
    wl_0122-hora          = sy-uzeit.
    wl_0122-tipo          = 'C'. "Compensação
    wl_0122-bukrs         = p_saida_0100-bukrs.
    wl_0122-gjahr         = vl_dt_mov(4).
    wl_0122-belnr         = lt_blntab-belnr.

    if p_ksfcp > 0.
      wl_0122-kursf = p_ksfcp.
    endif.

    modify zfit0122 from wl_0122.
    if sy-subrc ne 0.
      p_erro = 'X'.
      rollback work.
      message 'Houve ao efeturar a compensação' type 'S'.
      return.
    endif.

    "envia SIGAM
    if p_saida_0100-st_comp  = '3'.
      call function 'Z_FI_RETURN_PAYMENT_AP_AR'
        exporting
          i_bukrs = wl_0122-bukrs
          i_augbl = wl_0122-belnr
          i_gjahr = wl_0122-gjahr
          i_tcode = 'ZFI0105'.
    endif.
    clear: wl_0122.
    wl_0122-usnam         = sy-uname.
    wl_0122-data          = sy-datum.
    wl_0122-hora          = sy-uzeit.
    wl_0122-tipo          = 'A'. "Adiantamento
    wl_0122-bukrs         = p_saida_0100-bukrs.
    wl_0122-gjahr         = p_saida_0100-budat(4).
    wl_0122-belnr         = p_saida_0100-belnr.
    modify zfit0122 from wl_0122.
    if sy-subrc ne 0.
      p_erro = 'X'.
      rollback work.
      message 'Houve ao efeturar a compensação' type 'S'.
      return.
    endif.

    loop at it_0110_comp into wa_saida_0110 where check is not initial.

      clear: wl_0122.
      wl_0122-usnam         = sy-uname.
      wl_0122-data          = sy-datum.
      wl_0122-hora          = sy-uzeit.
      wl_0122-tipo          = 'P'. "Cta.Partidas Adiantamento.
      wl_0122-bukrs         = wa_saida_0110-bukrs.
      wl_0122-gjahr         = wa_saida_0110-budat(4).
      wl_0122-belnr         = wa_saida_0110-belnr.
      wl_0122-gjahr_cp      = p_saida_0100-budat(4).
      wl_0122-belnr_cp      = p_saida_0100-belnr.
      modify zfit0122 from wl_0122.
      if sy-subrc ne 0.
        p_erro = 'X'.
        rollback work.
        message 'Houve ao efeturar a compensação' type 'S'.
        return.
      endif.

    endloop.

    p_doc_comp = lt_blntab-belnr.

*   IF p_cplib IS INITIAL.  "*-BUG 78517-11.05.2022-JT-inicio
*     MESSAGE |Compensação gerada com sucesso: { p_doc_comp } | TYPE 'I'.
*   ENDIF.
  endif.

  "fim
  call function 'POSTING_INTERFACE_END'
    exporting
      i_bdcimmed              = 'X'
    exceptions
      session_not_processable = 1
      others                  = 2.

  if sy-subrc <> 0.
    exit.
  endif.

endform.

form f_bapi_f51_residual changing p_saida_0110 type ty_saida_0110
                                  p_erro.

  data: wl_0122 type zfit0122,
        wl_bsid type bsid_view,
        wl_bsik type bsik_view.

  data: l_auglv     type t041a-auglv   value 'UMBUCHNG', "Posting with Clearing
        l_tcode     type sy-tcode      value 'FB05',     "You get an error with any other value
        l_sgfunct   type rfipi-sgfunct value 'C',        "Post immediately
        l_flg_zfbdt type char1.

  data: lt_blntab  type standard table of blntab  with header line,
        lt_ftclear type standard table of ftclear with header line,
        lt_ftpost  type standard table of ftpost  with header line,
        lt_fttax   type standard table of fttax   with header line,
        lds_return type bapiret2.

  data: vdata(10),
        vdata_venc(10),
        vdata_zfbdt(10),
        cnum_seq(2),
        wl_vlr(16),
        wl_taxa(16),
        wl_vlrc(16),
        wl_vlrn         type p decimals 2,
        vcampo(15),
        v_kur           type bkpf-kursf,
        vvalor_bax      type zfit0042-dmbe2,
        msg_no          type t100-msgnr,
        msg_text        type string,
        p_mode          like rfpdo-allgazmd,
        count_ft        type ftpost-count,
        vl_dt_mov       type sy-datum.

  if p_saida_0110-vlr_rsd <= 0.
    message 'Valor Residual inconsistente!' type 'S'.
    p_erro = 'X'.
    return.
  endif.

  if p_saida_0110-kursf <= 0.
    message 'Taxa para gerar residual não encontrada!' type 'S'.
    p_erro = 'X'.
    return.
  endif.

  case p_saida_0110-koart.
    when 'D'. "Cliente
      if ( p_saida_0110-bschl ne '01' ) and
         ( p_saida_0110-bschl ne '11' ) and
         ( p_saida_0110-bschl ne '09' ) and
         ( p_saida_0110-bschl ne '19' ).
        data(_erro_chave) = 'X'.
      endif.
    when 'K'. "Fornecedor
      if ( p_saida_0110-bschl ne '21' ) and
         ( p_saida_0110-bschl ne '31' ) and
         ( p_saida_0110-bschl ne '29' ) and
         ( p_saida_0110-bschl ne '39' ).
        _erro_chave = 'X'.

      endif.
  endcase.

  if _erro_chave is not initial.
    message 'Chave Lançamento não configurada para deixar Saldo Residual!' type 'S'.
    p_erro = 'X'.
    return.
  endif.

  perform f_moeda_empresa using p_saida_0110-bukrs
                                'X'.
  if ( sy-subrc ne 0 ).
    p_erro = 'X'.
    return.
  endif.


  if ( p_saida_0110-wrbtr + p_saida_0110-vlr_rsd ) ne p_saida_0110-dmbtr_aux.
    message 'Saldo Residual inconsistente!' type 'S'.
    p_erro = 'X'.
    return.
  endif.


  vl_dt_mov = p_augdt-low.

  p_mode = 'N'.

* * PBI - 70679 - Inicio - CBRAND
  if p_saida_0110-vlr_rsd > 0 and rb_forn is not initial.

    clear: wa_bseg.
    data etl2374c4r5184 type table of bseg.
    data rldnr_l2374c4r4684 type rldnr.
    call function 'FAGL_GET_LEADING_LEDGER'
      importing
        e_rldnr       = rldnr_l2374c4r4684
      exceptions
        not_found     = 1
        more_than_one = 2.
    if sy-subrc = 0.
      call function 'FAGL_GET_GL_DOCUMENT'
        exporting
          i_rldnr   = rldnr_l2374c4r4684
          i_bukrs   = p_saida_0110-bukrs
          i_belnr   = p_saida_0110-belnr
          i_gjahr   = p_saida_0110-gjahr
          i_buzei   = p_saida_0110-buzei
        importing
          et_bseg   = etl2374c4r5184
        exceptions
          not_found = 1.
    endif.
    if sy-subrc = 0 and lines( etl2374c4r5184 ) = 1.
      wa_bseg = etl2374c4r5184[ 1 ].
      sy-dbcnt = 1.
    else.
      sy-subrc = 4.
      sy-dbcnt = 0.
    endif.


    if wa_cabecalho_0110-rsd_adt <= 0 and _automation = abap_false.  "*-BUG 78517-11.05.2022-JT-inicio "PSA
      call screen 0130 starting at 1 1.
      if var_answer = '0'.
        set screen 0110.
        leave to screen 0110.
      endif.
    endif.
  endif.

* * PBI - 70679 - Fim - CBRAND

  call function 'POSTING_INTERFACE_START'
    exporting
      i_client           = sy-mandt
      i_function         = 'C'
      i_mode             = p_mode
      i_update           = 'S'
      i_user             = sy-uname
    exceptions
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      others             = 6.

  if sy-subrc ne 0.
    p_erro = 'X'.
    message 'Houve um erro ao desmembrar um documento' type 'S'.
    return.
  endif.

  concatenate vl_dt_mov+6(2) vl_dt_mov+4(2) vl_dt_mov(4) into vdata separated by '.'.
  concatenate p_saida_0110-zfbdt+6(2) p_saida_0110-zfbdt+4(2) p_saida_0110-zfbdt(4) into vdata_venc separated by '.'.

  write: p_saida_0110-kursf to wl_taxa.
  condense wl_taxa no-gaps.

  clear: lt_blntab,   lt_blntab[],
         lt_ftclear,  lt_ftclear[],
         lt_ftpost,   lt_ftpost[],
         lt_fttax,    lt_fttax[],
         lds_return.

  count_ft = 1.

  lt_ftpost-stype = 'K'."Header
  lt_ftpost-count = count_ft.  "number of Dynpro

  lt_ftpost-fnam = 'BKPF-BUKRS'.
  lt_ftpost-fval = p_saida_0110-bukrs.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-WAERS'.
  lt_ftpost-fval = p_saida_0110-waers.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-KURSF'.
  lt_ftpost-fval = wl_taxa.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLDAT'.
  lt_ftpost-fval = vdata.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BUDAT'.
  lt_ftpost-fval = vdata.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-MONAT'.
  lt_ftpost-fval =  vl_dt_mov+4(2).
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLART'.
  if p_saida_0110-blart is initial.
    p_saida_0110-blart = 'MB'.
  endif.
  lt_ftpost-fval = p_saida_0110-blart.
  append lt_ftpost.

*-BUG 78517-11.05.2022-JT-inicio
  lt_ftpost-fnam = 'BKPF-XBLNR'.
*  lt_ftpost-fval = p_saida_0110-ovped.
  lt_ftpost-fval = p_saida_0110-xblnr.
  append lt_ftpost.

  lt_ftclear-agkoa  = p_saida_0110-koart.
  lt_ftclear-agkon  = p_saida_0110-parid.
  lt_ftclear-agums  = p_saida_0110-umskz.
  lt_ftclear-agbuk  = p_saida_0110-bukrs.
  lt_ftclear-xnops  = 'X'.
  lt_ftclear-selfd  = 'BELNR'.
  concatenate p_saida_0110-belnr p_saida_0110-budat(4) p_saida_0110-buzei into lt_ftclear-selvon.
  append lt_ftclear.

  clear: p_erro, l_flg_zfbdt.

  "Valor residual
  do 2 times.

    clear: wl_vlrn, wl_vlrc.

    add 1 to count_ft.

*-BUG 78517-11.05.2022-JT-inicio
    lt_ftpost-stype = 'P'.
    lt_ftpost-count = count_ft .
*-BUG 78517-11.05.2022-JT-fim

    case sy-index .
      when 1.
        wl_vlrn = abs( p_saida_0110-wrbtr ).
      when 2.
        wl_vlrn = abs( p_saida_0110-vlr_rsd ).

* * PBI - 70679 - Inicio - CBRAND
** Dados Bancários
        if rb_forn is not initial         and
           p_saida_0110-vlr_rsd > 0       and
           wa_cabecalho_0110-rsd_adt <= 0.  "*-BUG 78517-11.05.2022-JT-inicio
          lt_ftpost-fnam = 'BSEG-ZLSPR'.
          lt_ftpost-fval = wa_bseg-zlspr.
          append lt_ftpost.

          lt_ftpost-fnam = 'BSEG-ZLSCH'.
          lt_ftpost-fval = wa_bseg-zlsch.
          append lt_ftpost.

          lt_ftpost-fnam = 'BSEG-HBKID'.
          lt_ftpost-fval = wa_bseg-hbkid.
          append lt_ftpost.

          lt_ftpost-fnam = 'BSEG-BVTYP'.
          lt_ftpost-fval = wa_bseg-bvtyp.
          append lt_ftpost.

          lt_ftpost-fnam = 'BSEG-ZTERM'.
          clear lt_ftpost-fval.
          append lt_ftpost.

          lt_ftpost-fnam = 'BSEG-ZBD1T'.
          clear lt_ftpost-fval.
          append lt_ftpost.

          concatenate wa_bseg-zfbdt+6(2) wa_bseg-zfbdt+4(2) wa_bseg-zfbdt(4) into vdata_zfbdt separated by '.'.

          l_flg_zfbdt    = abap_true.
          lt_ftpost-fnam = 'BSEG-ZFBDT'.
          lt_ftpost-fval = vdata_zfbdt.
          append lt_ftpost.
        endif.
* * PBI - 70679 - Fim - CBRAND

    endcase.

    write: wl_vlrn to wl_vlrc.

*-BUG 78517-11.05.2022-JT-inicio
*   lt_ftpost-stype = 'P'.
*   lt_ftpost-count = count_ft .
*-BUG 78517-11.05.2022-JT-fim

    lt_ftpost-fnam = 'RF05A-NEWBS'.
    lt_ftpost-fval =  p_saida_0110-bschl.
    append lt_ftpost.

    lt_ftpost-fnam = 'BSEG-HKONT'.
    lt_ftpost-fval = p_saida_0110-parid.
    append lt_ftpost.

    lt_ftpost-fnam = 'BSEG-GSBER'.
    lt_ftpost-fval = p_saida_0110-gsber.
    append lt_ftpost.

    lt_ftpost-fnam = 'BSEG-SGTXT'.
    lt_ftpost-fval = p_saida_0110-sgtxt_rsd.
    append lt_ftpost.


    lt_ftpost-fnam = 'BSEG-ZUONR'.
    lt_ftpost-fval = p_saida_0110-zuonr.
    append lt_ftpost.

    lt_ftpost-fnam = 'BSEG-HZUON'.
    lt_ftpost-fval =  p_saida_0110-ovped.

    if p_saida_0110-ebelp is not initial.
      lt_ftpost-fval = lt_ftpost-fval && p_saida_0110-ebelp.
    endif.

    append lt_ftpost.

    if p_saida_0110-zfbdt is not initial and
       l_flg_zfbdt         = abap_false.    "*-BUG 78517-11.05.2022-JT-inicio
      lt_ftpost-fnam = 'BSEG-ZFBDT'.
      lt_ftpost-fval = vdata_venc.
      append lt_ftpost.

      if ( p_saida_0110-umsks is initial ).
        lt_ftpost-fnam = 'BSEG-ZBD1T'.
        lt_ftpost-fval = 0. "p_saida_0110-zbd1t.
        condense lt_ftpost-fval no-gaps.
        append lt_ftpost.
      endif.
    endif.

    if p_saida_0110-umsks is not initial. "Adiantamento

      lt_ftpost-fnam = 'RF05A-NEWUM'.
      lt_ftpost-fval = p_saida_0110-umskz.
      append lt_ftpost.

      if ( p_saida_0110-anln1 is not initial ).
        lt_ftpost-fnam = 'BSEG-ANLN1'.
        lt_ftpost-fval = p_saida_0110-anln1.
        append lt_ftpost.

        if p_saida_0110-anln2 is not initial.
          lt_ftpost-fnam = 'BSEG-ANLN2'.
          lt_ftpost-fval = p_saida_0110-anln2.
          append lt_ftpost.
        endif.
      endif.
    endif.

    lt_ftpost-fnam = 'BSEG-WRBTR'.
    lt_ftpost-fval =  wl_vlrc.
    append lt_ftpost.

    if p_saida_0110-waers ne tg_t001-waers.
      wl_vlrn = wl_vlrn * abs( p_saida_0110-kursf ).
      write: wl_vlrn to wl_vlrc.
      lt_ftpost-fnam = 'BSEG-DMBTR'.
      lt_ftpost-fval =  wl_vlrc.
      append lt_ftpost.
    else.
      wl_vlrn = wl_vlrn / abs( p_saida_0110-kursf ).
      write: wl_vlrn to wl_vlrc.
      lt_ftpost-fnam = 'BSEG-DMBE2'.
      lt_ftpost-fval =  wl_vlrc.
      append lt_ftpost.
    endif.

  enddo.

  call function 'POSTING_INTERFACE_CLEARING'
    exporting
      i_auglv                    = l_auglv
      i_tcode                    = l_tcode
      i_sgfunct                  = l_sgfunct
      i_no_auth                  = 'X'
    importing
      e_msgid                    = lds_return-id
      e_msgno                    = lds_return-number
      e_msgty                    = lds_return-type
      e_msgv1                    = lds_return-message_v1
      e_msgv2                    = lds_return-message_v2
      e_msgv3                    = lds_return-message_v3
      e_msgv4                    = lds_return-message_v4
    tables
      t_blntab                   = lt_blntab
      t_ftclear                  = lt_ftclear
      t_ftpost                   = lt_ftpost
      t_fttax                    = lt_fttax
    exceptions
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      others                     = 10.


  if lt_blntab[] is initial.
    p_erro = 'X'.
    write lds_return-number to msg_no.
    call function 'MESSAGE_PREPARE'
      exporting
        msg_id                 = lds_return-id
        msg_no                 = msg_no
        msg_var1               = lds_return-message_v1
        msg_var2               = lds_return-message_v2
        msg_var3               = lds_return-message_v3
        msg_var4               = lds_return-message_v4
      importing
        msg_text               = msg_text
      exceptions
        function_not_completed = 1
        message_not_found      = 2
        others                 = 3.
    message msg_text type 'S'.
  else.
    read table lt_blntab index 1.
*    IF rb_prod IS INITIAL.
    clear: wl_0122.
    wl_0122-usnam         = sy-uname.
    wl_0122-data          = sy-datum.
    wl_0122-hora          = sy-uzeit.
    wl_0122-tipo          = 'D'. "Desmembramento
    wl_0122-bukrs         = p_saida_0110-bukrs.
    wl_0122-gjahr         = p_saida_0110-gjahr.
    wl_0122-belnr         = p_saida_0110-belnr.
    wl_0122-gjahr_desmemb = vl_dt_mov(4).
    wl_0122-belnr_desmemb = lt_blntab-belnr.
    modify zfit0122 from wl_0122.
*PBI - 70679 - Inicio - CBRAND
*    ELSE.
*      LOOP AT tg_bsik_copy WHERE lifnr = p_saida_0110-parid
*                           AND   zuonr = p_saida_0110-zuonr.
*        CLEAR: wl_0122.
*        wl_0122-usnam         = sy-uname.
*        wl_0122-data          = sy-datum.
*        wl_0122-hora          = sy-uzeit.
*        wl_0122-tipo          = 'D'. "Desmembramento
*        wl_0122-bukrs         = tg_bsik_copy-bukrs.
*        wl_0122-gjahr         = tg_bsik_copy-gjahr.
*        wl_0122-belnr         = tg_bsik_copy-belnr.
*        wl_0122-gjahr_desmemb = vl_dt_mov(4).
*        wl_0122-belnr_desmemb = lt_blntab-belnr.
*        MODIFY zfit0122 FROM wl_0122.
*      ENDLOOP.
* ENDIF.
* PBI - 70679 - Fim - CBRAND
    if sy-subrc ne 0.
      p_erro = 'X'.
      message 'Houve um erro ao desmembrar um documento' type 'S'.
      return.
    endif.

    clear: wl_bsid, wl_bsik.

    case p_saida_0110-koart.
      when 'D'. "Cliente

        select single *
          from bsid_view into @wl_bsid
         where bukrs = @p_saida_0110-bukrs
           and gjahr = @vl_dt_mov(4)
           and belnr = @lt_blntab-belnr
           and wrbtr = @p_saida_0110-wrbtr.
      when 'K'. "Fornecedor
        select single *
          from bsik_view into @wl_bsik
         where bukrs = @p_saida_0110-bukrs
           and gjahr = @vl_dt_mov(4)
           and belnr = @lt_blntab-belnr
           and wrbtr = @p_saida_0110-wrbtr.
    endcase.

    if sy-subrc ne 0 .
      p_erro = 'X'.
      message 'Houve um erro ao desmembrar um documento' type 'S'.
      return.
    endif.

    "Faz a troca para o novo documento gerado com valor a ser baixado.
    p_saida_0110-bl_desmemb = p_saida_0110-belnr.
    p_saida_0110-bz_desmemb = p_saida_0110-buzei.
* PBI - 70679 - Inicio - CBRAND
*    IF rb_prod IS NOT INITIAL .
*      p_saida_0110-bl_desmemb = wl_bsik-belnr.
*      p_saida_0110-bz_desmemb = wl_bsik-buzei.
*    ENDIF.
* PBI - 70679 - Fim - CBRAND
    case p_saida_0110-koart.
      when 'D'. "Cliente
        p_saida_0110-belnr      = wl_bsid-belnr.
        p_saida_0110-buzei      = wl_bsid-buzei.
        p_saida_0110-gjahr      = wl_bsid-gjahr.
        p_saida_0110-bldat      = wl_bsid-bldat.
        p_saida_0110-budat      = wl_bsid-budat.
      when 'K'. "Fornecedor
        p_saida_0110-belnr      = wl_bsik-belnr.
        p_saida_0110-buzei      = wl_bsik-buzei.
        p_saida_0110-gjahr      = wl_bsik-gjahr.
        p_saida_0110-bldat      = wl_bsik-bldat.
        p_saida_0110-budat      = wl_bsik-budat.
    endcase.
* PBI - 70679 - Inicio - CBRAND
*    IF rb_prod = 'X'.
*      SELECT SINGLE *
*        FROM bseg
*        INTO @DATA(w_bseg_atribui)
*        WHERE bukrs = @wl_bsik-bukrs
*        AND   belnr = @wl_bsik-belnr
*        AND   buzei = @wl_bsik-buzei
*        AND   gjahr = @wl_bsik-gjahr.
*      PERFORM f_atribui USING w_bseg_atribui p_saida_0110-xref3.
*    ENDIF.
* PBI - 70679 - Fim - CBRAND
  endif.

  "fim
  call function 'POSTING_INTERFACE_END'
    exporting
      i_bdcimmed              = 'X'
    exceptions
      session_not_processable = 1
      others                  = 2.

  if sy-subrc <> 0.
    exit.
  endif.

endform.


form f_atribui using p_bseg  type bseg
                     p_xref3 type bseg-xref3.

  data: lt_bkdf  type table of bkdf,
        lt_bkpf  type table of bkpf,
        wa_bkpf  type bkpf,
        lt_bsec  type table of bsec,
        wa_bseg  type bseg,
        lt_bsed  type table of bsed,
        lt_bseg  type table of bseg,
        lt_bset  type table of bset,
        msg_text type string.

  clear wa_bkpf.
  refresh lt_bkpf.
  select single *
    from bkpf
    into wa_bkpf
  where bukrs = p_bseg-bukrs
  and   belnr = p_bseg-belnr
  and   gjahr = p_bseg-gjahr.

  append wa_bkpf to lt_bkpf.
  clear wa_bseg.
  refresh  lt_bseg.
  move-corresponding p_bseg to wa_bseg.
  wa_bseg-xref3 = p_xref3.
  append wa_bseg to lt_bseg.

  call function 'CHANGE_DOCUMENT'
    tables
      t_bkdf = lt_bkdf
      t_bkpf = lt_bkpf
      t_bsec = lt_bsec
      t_bsed = lt_bsed
      t_bseg = lt_bseg
      t_bset = lt_bset.

  if sy-subrc = 0.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
  endif.

endform.

form f_get_taxa  using p_bkpf  like tg_bkpf
                       p_dmbtr type bsid-dmbtr
                       p_dmbe2 type bsid-dmbe2
              changing p_kursf type bkpf-kursf.

  data: v_kursf_banco type bkpf-kursf.

  clear: p_kursf.

  perform f_moeda_empresa using p_bkpf-bukrs
                                'X'.
  if ( sy-subrc ne 0 ).
    return.
  endif.

*  IF ( P_BKPF-WAERS = TG_T001-WAERS ).
*    P_KURSF = ABS( P_BKPF-KURS2 ).
*  ELSE.
*    P_KURSF = ABS( P_BKPF-KURSF ).
*  ENDIF.

  if ( p_dmbtr > 0 ) and  ( p_dmbe2 > 0 ).

    try.
        p_kursf = p_dmbtr / p_dmbe2.
      catch cx_sy_arithmetic_overflow.
    endtry.

  endif.

  perform f_get_bsis_cbanco using p_bkpf-bukrs
                                  p_bkpf-belnr
                                  p_bkpf-gjahr
                         changing v_kursf_banco.

  if v_kursf_banco > 0.
    p_kursf = abs( v_kursf_banco ).
  endif.

endform.

form f_atualiza_saldo.

  data: vl_tabix type sy-tabix.
  refresh tl_parametros.
  call function 'SUSR_USER_PARAMETERS_GET'
    exporting
      user_name           = sy-uname
    tables
      user_parameters     = tl_parametros
    exceptions
      user_name_not_exist = 1
      others              = 2.
  if sy-subrc <> 0.
  endif.

  clear vg_parausd.
  read table tl_parametros into wl_parametros
   with key parid = 'Z105USD'.

  if sy-subrc eq 0. "Exibe USD
    vg_parausd = 'X'.
  endif.

  clear: wa_cabecalho_0110-sel_dmbtr,
         wa_cabecalho_0110-sel_dmbtr2,
         wa_cabecalho_0110-sel_dmbe2,
         wa_cabecalho_0110-sld_dmbtr,
         wa_cabecalho_0110-sld_dmbtr2,
         wa_cabecalho_0110-sld_dmbe2.

  perform f_moeda_empresa using wa_saida_0100-bukrs
                                'X'.
  if ( sy-subrc ne 0 ).
    return.
  endif.

  if vg_parausd = 'X'.
    wa_cabecalho_0110-adt_dmbtr2 = wa_saida_0100-dmbtr.
  else.
    wa_cabecalho_0110-adt_dmbtr2 = wa_saida_0100-wrbtr..
  endif.

  wa_cabecalho_0110-adt_dmbtr = wa_saida_0100-wrbtr.
  wa_cabecalho_0110-adt_dmbe2 = wa_saida_0100-dmbe2.

*------------------------------------------------------------------------*
*  Recalcula valor caso atribuido um valor residual para a partida.
*------------------------------------------------------------------------*
  if wa_cabecalho_0110-rsd_adt > 0.
    wa_cabecalho_0110-adt_dmbtr  = wa_saida_0100-wrbtr - wa_cabecalho_0110-rsd_adt.

    if vg_parausd = 'X'.
      if wa_saida_0100-waers = 'USD'.
        wa_cabecalho_0110-adt_dmbe2    = wa_saida_0100-wrbtr - wa_cabecalho_0110-rsd_adt.
        wa_cabecalho_0110-adt_dmbtr2   = wa_cabecalho_0110-adt_dmbe2 * wa_saida_0100-kursf.
      else.
        wa_cabecalho_0110-adt_dmbtr2 = wa_saida_0100-wrbtr - wa_cabecalho_0110-rsd_adt.
        if wa_cabecalho_0110-adt_dmbtr2 > 0.
          wa_cabecalho_0110-adt_dmbe2 = wa_cabecalho_0110-adt_dmbtr2 / wa_saida_0100-kursf .
        endif.
      endif.
    else.
      wa_cabecalho_0110-adt_dmbtr2 = wa_saida_0100-wrbtr - wa_cabecalho_0110-rsd_adt.
    endif.

    if wa_cabecalho_0110-adt_dmbtr2 > 0.
*      wa_cabecalho_0110-adt_dmbe2 = wa_cabecalho_0110-adt_dmbtr / wa_saida_0100-kursf .
    else.
      if vg_parausd = 'X'.
        wa_cabecalho_0110-adt_dmbtr = wa_saida_0100-dmbtr.
      else.
        wa_cabecalho_0110-adt_dmbtr2 = wa_saida_0100-wrbtr.
      endif.
      wa_cabecalho_0110-adt_dmbtr = wa_saida_0100-wrbtr.
      wa_cabecalho_0110-adt_dmbe2 = wa_saida_0100-dmbe2.
      clear: wa_cabecalho_0110-rsd_adt.
    endif.

  endif.

*------------------------------------------------------------------------*
*  Processamento contra partidas.
*------------------------------------------------------------------------*

  loop at it_saida_0110 into wa_saida_0110.

    vl_tabix = sy-tabix.

    select single shkzg
      from tbsl into wa_saida_0110-shkzg
     where bschl = wa_saida_0110-bschl.

    if wa_saida_0110-manual is not initial. "Tratamento partida manual

      if ( wa_saida_0110-dmbe2 is initial     ) and
         ( wa_saida_0110-wrbtr is not initial ).
        wa_saida_0110-dmbe2 = wa_saida_0110-wrbtr / wa_saida_0110-kursf.
      endif.

    else. "Tratamento outras partidas.
      wa_saida_0110-wrbtr = wa_saida_0110-dmbtr_aux.
      wa_saida_0110-dmbtr = wa_saida_0110-dmbtr_au2.

      if ( wa_saida_0110-check   is not initial ) and
         ( wa_saida_0110-vlr_rsd is initial ).
      else.
        if ( wa_saida_0110-vlr_rsd <= 0 ) or
           ( wa_saida_0110-vlr_rsd >= wa_saida_0110-wrbtr ).
          clear: wa_saida_0110-vlr_rsd, wa_saida_0110-check.
        else.
          subtract wa_saida_0110-vlr_rsd from wa_saida_0110-wrbtr.
          if vg_parausd = 'X'.
            if wa_saida_0110-waers = 'USD'.
              wa_saida_0110-dmbe2 = wa_saida_0110-vlr_rsd * wa_saida_0110-kursf.
              subtract wa_saida_0110-dmbe2 from wa_saida_0110-dmbtr.
            else.
              subtract wa_saida_0110-vlr_rsd from wa_saida_0110-dmbtr.
            endif.
          else.

          endif.
          wa_saida_0110-check = 'X'.
        endif.
      endif.

      wa_saida_0110-dmbe2 = wa_saida_0110-dmbtr / wa_saida_0110-kursf.

    endif.

    if wa_saida_0110-check is not initial.

      if  ( wa_saida_0110-manual     is not initial ) and
          ( wa_saida_0110-part_princ is not initial ).

        if ( wa_saida_0110-shkzg ne wa_saida_0100-shkzg ).
          if vg_parausd = 'X'.
            add wa_saida_0110-dmbtr to wa_cabecalho_0110-adt_dmbtr2.
          else.
            add wa_saida_0110-wrbtr  to wa_cabecalho_0110-adt_dmbtr2.
          endif.
          add wa_saida_0110-wrbtr to wa_cabecalho_0110-adt_dmbtr.
          add wa_saida_0110-dmbe2 to wa_cabecalho_0110-adt_dmbe2.
        else.
          if vg_parausd = 'X'.
            subtract wa_saida_0110-dmbtr from wa_cabecalho_0110-adt_dmbtr2.
          else.
            subtract wa_saida_0110-wrbtr from wa_cabecalho_0110-adt_dmbtr2.
          endif.
          subtract wa_saida_0110-wrbtr from wa_cabecalho_0110-adt_dmbtr.
          subtract wa_saida_0110-dmbe2 from wa_cabecalho_0110-adt_dmbe2.
        endif.

      else.

        if  ( wa_saida_0110-manual is not initial ).
          if ( wa_saida_0110-shkzg eq wa_saida_0100-shkzg ).
            if vg_parausd = 'X'.
              add wa_saida_0110-dmbtr to wa_cabecalho_0110-sel_dmbtr2.
            else.
              add wa_saida_0110-wrbtr to wa_cabecalho_0110-sel_dmbtr2.
            endif.
            add wa_saida_0110-wrbtr to wa_cabecalho_0110-sel_dmbtr.
            add wa_saida_0110-dmbe2 to wa_cabecalho_0110-sel_dmbe2.
          else.
            if vg_parausd = 'X'.
              subtract wa_saida_0110-dmbtr from wa_cabecalho_0110-sel_dmbtr2.
            else.
              subtract wa_saida_0110-wrbtr from wa_cabecalho_0110-sel_dmbtr2.
            endif.
            subtract wa_saida_0110-wrbtr from wa_cabecalho_0110-sel_dmbtr.
            subtract wa_saida_0110-dmbe2 from wa_cabecalho_0110-sel_dmbe2.
          endif.
        else.
          if ( wa_saida_0110-shkzg ne wa_saida_0100-shkzg ).
            if vg_parausd = 'X'.
              add wa_saida_0110-dmbtr to wa_cabecalho_0110-sel_dmbtr2.
            else.
              add wa_saida_0110-wrbtr to wa_cabecalho_0110-sel_dmbtr2.
            endif.
            add wa_saida_0110-wrbtr to wa_cabecalho_0110-sel_dmbtr.
            add wa_saida_0110-dmbe2 to wa_cabecalho_0110-sel_dmbe2.
          else.
            if vg_parausd = 'X'.
              subtract wa_saida_0110-dmbtr from wa_cabecalho_0110-sel_dmbtr2.
            else.
              subtract wa_saida_0110-wrbtr from wa_cabecalho_0110-sel_dmbtr2.
            endif.
            subtract wa_saida_0110-wrbtr from wa_cabecalho_0110-sel_dmbtr.
            subtract wa_saida_0110-dmbe2 from wa_cabecalho_0110-sel_dmbe2.
          endif.
        endif.

      endif.

    endif.

    modify it_saida_0110 from wa_saida_0110 index vl_tabix.

  endloop.

  wa_cabecalho_0110-sld_dmbtr2 = wa_cabecalho_0110-adt_dmbtr2 - wa_cabecalho_0110-sel_dmbtr2.

  wa_cabecalho_0110-sld_dmbtr = wa_cabecalho_0110-adt_dmbtr - wa_cabecalho_0110-sel_dmbtr.
  wa_cabecalho_0110-sld_dmbe2 = wa_cabecalho_0110-adt_dmbe2 - wa_cabecalho_0110-sel_dmbe2.

  if _automation = abap_true and _step = 'Menor' or _step = 'Maior'.
    wa_cabecalho_0110-sld_dmbtr2 = 0.
    wa_cabecalho_0110-sld_dmbtr = 0.
  endif.


endform.

form f_compensar_adt using p_saida_0100 type ty_saida_0100.

  data: vl_error    type c,
        vl_doc_comp type bsad-belnr,
        vl_msg      type string.

  data: wa_sai_0110_tmp type ty_saida_0110.

  field-symbols <saida_0110> type ty_saida_0110.

  perform f_valida_alv_0110 changing vl_error.

  check vl_error is initial.

  perform f_atualiza_saldo.

  perform f_moeda_empresa using p_saida_0100-bukrs
                                'X'.
  if ( sy-subrc ne 0 ).
    return.
  endif.

  if wa_cabecalho_0110-sld_dmbtr ne 0.
    message 'Saldo diferente de 0' type 'W'.
    return.
  endif.

  "Check Compensação Imobilizado com Residual.
  data(_comp_imob_residual) = ''.
  loop at it_saida_0110 assigning <saida_0110> where vlr_rsd  ne 0
                                                 and check is not initial
                                                 and anln1 is not initial.
    _comp_imob_residual = 'X'.
  endloop.

  if ( wa_cabecalho_0110-rsd_adt > 0 ) and ( wa_saida_0100-anln1 is not initial ).
    _comp_imob_residual = 'X'.
  endif.
  "Fim Check Compensação Imobilizado com Residual

  "Verifica Lançamentos que devem ser desmembrados.
  if _comp_imob_residual is initial.
    loop at it_saida_0110 assigning <saida_0110> where vlr_rsd ne 0
                                                   and check is not initial.
      vl_error = '1'.
*      IF vg_parausd IS INITIAL.
*        <saida_0110>-sgtxt_rsd = 'Desmembramento Fatura'.
*      ENDIF.
      perform f_bapi_f51_residual changing <saida_0110>
                                           vl_error.
      if vl_error is not initial and _automation = abap_false. "PSA
        leave to screen 0.
        return.
      else.
        return.
      endif.
    endloop.

    commit work.

    "Verifica se Adiantamento deve ser desmembrado.
    if ( wa_cabecalho_0110-rsd_adt > 0 ).
      clear: wa_sai_0110_tmp.

      move-corresponding wa_saida_0100 to wa_sai_0110_tmp.

      if wa_cabecalho_0110-manter_tp_dc is initial. "AND rb_prod IS INITIAL. * PBI - 70679 -  CBRAND
        wa_sai_0110_tmp-blart     = 'AB'.
      endif.
      wa_sai_0110_tmp-vlr_rsd   = wa_cabecalho_0110-rsd_adt.
      wa_sai_0110_tmp-wrbtr     = wa_cabecalho_0110-adt_dmbtr.
      wa_sai_0110_tmp-dmbe2     = wa_cabecalho_0110-adt_dmbe2.
      wa_sai_0110_tmp-sgtxt_rsd = wa_cabecalho_0110-sgtxt_rsd.

      wa_sai_0110_tmp-dmbtr_aux = wa_saida_0100-wrbtr. "Valor Original Adiantamento
      wa_sai_0110_tmp-dmbe2_aux = wa_saida_0100-dmbe2. "Valor Original Adiantamento

      perform f_bapi_f51_residual changing wa_sai_0110_tmp
                                           vl_error.
      if vl_error is not initial and _automation = abap_false."PSA
        leave to screen 0.
        return.
      else.
        return.
      endif.

      if wa_sai_0110_tmp-bl_desmemb is initial and _automation = abap_false. "Documento Desmembrado PSA
        leave to screen 0.
        return.
      endif.

      wa_saida_0100-belnr = wa_sai_0110_tmp-belnr.
      wa_saida_0100-buzei = wa_sai_0110_tmp-buzei.
      wa_saida_0100-gjahr = wa_sai_0110_tmp-gjahr.
      wa_saida_0100-bldat = wa_sai_0110_tmp-bldat.
      wa_saida_0100-budat = wa_sai_0110_tmp-budat.
      wa_saida_0100-dmbtr = wa_sai_0110_tmp-dmbtr.
      wa_saida_0100-dmbe2 = wa_sai_0110_tmp-dmbe2.
    endif.

  endif.

  wa_saida_0100-vlr_rsd   = wa_cabecalho_0110-rsd_adt.   "Atribuição Saldo Residual
  wa_saida_0100-wrbtr     = wa_cabecalho_0110-adt_dmbtr.
  wa_saida_0100-dmbe2     = wa_cabecalho_0110-adt_dmbe2.
  wa_saida_0100-sgtxt_rsd = wa_cabecalho_0110-sgtxt_rsd.

  perform f_bapi_f51 using _comp_imob_residual
                  changing wa_saida_0100
                           vl_error
                           vl_doc_comp.

  if vl_error is initial and _automation = abap_false."PSA
    concatenate 'Compensação efetuada com sucesso! Doc.Compensação:' vl_doc_comp
           into vl_msg separated by space.
    message vl_msg type 'S'.
  endif.

  if _automation = abap_false."PSA
    leave to screen 0.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_ALV_0110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VL_ERROR  text
*----------------------------------------------------------------------*
form f_valida_alv_0110  changing p_error.

  clear: p_error.

  "Lançamentos Manuais
  loop at it_saida_0110 into wa_saida_0110 where manual is not initial.

    if wa_saida_0110-hkont is initial.
      p_error = 'X'.
      message 'Conta é um campo obrigatório!' type 'S'.
      return.
    endif.

    if wa_saida_0110-bschl is initial.
      p_error = 'X'.
      message 'Chave Lcto. é um campo obrigatório!' type 'S'.
      return.
    endif.

    if wa_saida_0110-wrbtr is initial.
      p_error = 'X'.
      message 'Valor R$ é um campo obrigatório!' type 'S'.
      return.
    endif.

*    IF wa_saida_0110-dmbe2 IS INITIAL AND
*       wa_saida_0110-waers <> 'BRL'.  "*-BUG 78517-11.05.2022-JT-inicio
*      p_error = 'X'.
*      MESSAGE 'Valor U$ é um campo obrigatório!' TYPE 'S'.
*      RETURN.
*    ENDIF.

    if ( wa_saida_0110-umsks is not initial ) and
       ( wa_saida_0110-zfbdt is initial ).
      p_error = 'X'.
      message 'Data Venc. é um campo obrigatório!' type 'S'.
      return.
    endif.

  endloop.

endform.

form f_renovar_cons.
  if vg_cancel = 'X'.
    clear vg_cancel.
    perform f_refresh_alv using '0100'..
    exit.
  endif.
  perform f_selecionar_dados.
  check vg_not_found is initial.
  perform: f_processa_dados,
           f_refresh_alv using '0100'.

endform.

form f_flag_documentos  using p_mark.

  field-symbols: <saida_0110> type ty_saida_0110.
  data: vl_msg    type string,
        vl_msg_01 type string.

  clear: it_sel_rows[], wa_sel_rows.

  call method obj_alv_0110->get_selected_rows
    importing
      et_index_rows = it_sel_rows.

  check it_sel_rows[] is not initial.

  loop at it_sel_rows into wa_sel_rows.
    read table it_saida_0110 assigning <saida_0110> index wa_sel_rows-index.
    if sy-subrc ne 0 .
      return.
    endif.

    <saida_0110>-check = p_mark.
  endloop.

  perform f_atualiza_saldo.

  perform f_moeda_empresa using <saida_0110>-bukrs
                                'X'.
  if ( sy-subrc ne 0 ).
    return.
  endif.

  if p_mark is not initial.
    if wa_cabecalho_0110-sld_dmbtr  > 0.
      vl_msg_01 = wa_cabecalho_0110-sld_dmbtr.

      concatenate 'Ainda existe um saldo à compensar( Moeda Documento ) no valor de:'
                   vl_msg_01 '! Desejar atribuir esse valor como Saldo Residual p/ o Adiantamento?'
              into vl_msg separated by space.

      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = 'Confirmação'
          text_question         = vl_msg
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        importing
          answer                = var_answer
        exceptions
          text_not_found        = 1
          others                = 2.

      if var_answer eq '1'.
        wa_cabecalho_0110-rsd_adt = wa_cabecalho_0110-sld_dmbtr.
      endif.

    endif.
  endif.

  perform f_atualiza_saldo.
  leave to screen 0110.

endform.

form f_get_bsid_comp  tables p_tg_bsid_comp structure tg_bsid_comp
                       using i_bukrs type bsid-bukrs
                             i_belnr type bsid-belnr
                             i_buzei type bsid-buzei
                             i_gjahr type bsid-gjahr
                             i_kunnr
                             i_vbel2
                             i_sgtxt
                             i_zuonr
                             i_anln1 type bsid-anln1
                             i_anln2 type bsid-anln2
                             i_dcsim.

  data: vl_partida_comp type c.

  clear: p_tg_bsid_comp[].


  loop at tg_bsid_comp where bukrs = i_bukrs
                         and ( ( kunnr = i_kunnr  and kunnr is not initial ) or
                               ( vbel2 = i_vbel2  and vbel2 is not initial ) or
                               ( dcsim = i_dcsim  and dcsim is not initial ) or
                               ( zuonr = i_zuonr  and zuonr is not initial )
                             ).

    clear: vl_partida_comp.

    "Se não for do mesmo cliente ou
    "           da mesma O.V ou
    "           da mesmo Doc.Simulador
    if ( tg_bsid_comp-kunnr ne i_kunnr ) and
       ( tg_bsid_comp-vbel2 ne i_vbel2 ) and
       ( tg_bsid_comp-dcsim ne i_dcsim ).
      "Verificar se esta unificando por uma atribuição especifica
      check ( p_zuonrj eq 'X' ) and ( p_zuonr-low is not initial ).
    endif.

    if ( p_ovped-low is not initial ) or
       ( p_dcsim-low is not initial ) or
       ( p_sgtxt-low is not initial ) or
       ( p_zuonr-low is not initial ).
      if ( ( tg_bsid_comp-vbel2 eq i_vbel2 and i_vbel2     is not initial and p_ovpedj eq 'X' and
             tg_bsid_comp-anln1 eq i_anln1 and tg_bsid_comp-anln2 eq i_anln2                  ) or
           ( tg_bsid_comp-sgtxt in r_sgtxt and p_sgtxt-low is not initial and p_sgtxtj eq 'X' ) or
           ( tg_bsid_comp-dcsim eq i_dcsim and i_dcsim     is not initial and p_dcsimj eq 'X' ) or
           ( tg_bsid_comp-zuonr in r_zuonr and p_zuonr-low is not initial and p_zuonrj eq 'X' ) ).
        vl_partida_comp = 'X'.
      endif.
    else.
      if ( ( tg_bsid_comp-vbel2 eq i_vbel2 and i_vbel2 is not initial and p_ovpedj eq 'X' and
             tg_bsid_comp-anln1 eq i_anln1 and tg_bsid_comp-anln2 eq i_anln2              ) or
           ( tg_bsid_comp-sgtxt eq i_sgtxt and i_sgtxt is not initial and p_sgtxtj eq 'X' ) or
           ( tg_bsid_comp-dcsim eq i_dcsim and i_dcsim is not initial and p_dcsimj eq 'X' ) or
           ( tg_bsid_comp-zuonr eq i_zuonr and i_zuonr is not initial and p_zuonrj eq 'X' ) ).
        vl_partida_comp = 'X'.
      endif.
    endif.

    check vl_partida_comp is not initial.

    append tg_bsid_comp to p_tg_bsid_comp.

  endloop.

  delete p_tg_bsid_comp where bukrs = i_bukrs
                          and belnr = i_belnr
                          and gjahr = i_gjahr
                          and buzei = i_buzei.


endform.

form f_get_bsik_comp  tables p_tg_bsik_comp structure tg_bsik_comp
                       using i_bukrs type bsik-bukrs
                             i_belnr type bsik-belnr
                             i_buzei type bsik-buzei
                             i_gjahr type bsik-gjahr
                             i_lifnr
                             i_ebeln
                             i_sgtxt
                             i_zuonr
                             i_anln1 type bsik-anln1
                             i_anln2 type bsik-anln2.

  data: vl_partida_comp type c.

  clear: p_tg_bsik_comp[].

*  "IR156502 ALRS 14.03.2024
*  IF i_anln1+0(3) = 'OBR' or
*     i_anln1+0(3) = 'EQP'.
*     clear: i_anln1, i_anln2.
*  ENDIF.
*  "IR156502 ALRS 14.03.2024

*  LOOP AT tg_bsik_comp WHERE bukrs = i_bukrs
*                         AND ( ( lifnr = i_lifnr  AND lifnr IS NOT INITIAL ) OR
*                               ( ebeln = i_ebeln  AND ebeln IS NOT INITIAL ) OR
*                               ( zuonr = i_zuonr  AND zuonr IS NOT INITIAL )
*                             ).
  loop at tg_bsik_comp where bukrs = i_bukrs.

* PBI - 70679 - Inicio - CBRAND
*    IF rb_prod IS INITIAL.
* PBI - 70679 - Fim - CBRAND
    if tg_bsik_comp-ebeln ne i_ebeln and tg_bsik_comp-ebeln is not initial and
       tg_bsik_comp-lifnr ne i_lifnr and tg_bsik_comp-lifnr is not initial and
       tg_bsik_comp-zuonr ne i_zuonr and tg_bsik_comp-zuonr is not initial.
      continue.
    endif.
*    ELSE.
*      IF tg_bsik_comp-lifnr NE i_lifnr OR
*         tg_bsik_comp-zuonr NE i_zuonr.
*        CONTINUE.
*      ENDIF.
* PBI - 70679 - Inicio - CBRAND
*      IF tg_bsik_comp-zuonr NE i_zuonr. "agrupa somente   por atribuição as FATURAS
*        CONTINUE.
*      ENDIF.
* PBI - 70679 - Fim - CBRAND
*    ENDIF.

    clear: vl_partida_comp.

    "Se não for do mesmo fornecedor ou do mesmo pedido
    if ( tg_bsik_comp-lifnr ne i_lifnr ) and
       ( tg_bsik_comp-ebeln ne i_ebeln ).
      "Verificar se esta unificando por uma atribuição especifica
      check ( p_zuonrj eq 'X' ) and ( p_zuonr-low is not initial ).
    endif.

    if ( p_ovped-low is not initial ) or
       ( p_sgtxt-low is not initial ) or
       ( p_zuonr-low is not initial ).
      if ( ( tg_bsik_comp-ebeln eq i_ebeln and i_ebeln     is not initial and p_ovpedj eq 'X' and
             tg_bsik_comp-anln1 eq i_anln1 and tg_bsik_comp-anln2 eq i_anln2                  ) or
           ( tg_bsik_comp-sgtxt in r_sgtxt and p_sgtxt-low is not initial and p_sgtxtj eq 'X' ) or
           ( tg_bsik_comp-zuonr in r_zuonr and p_zuonr-low is not initial and p_zuonrj eq 'X' ) ).
        vl_partida_comp = 'X'.
      endif.
    else.
      if ( ( tg_bsik_comp-ebeln eq i_ebeln and i_ebeln is not initial and p_ovpedj eq 'X' and
             tg_bsik_comp-anln1 eq i_anln1 and tg_bsik_comp-anln2 eq i_anln2              ) or
           ( tg_bsik_comp-sgtxt eq i_sgtxt and i_sgtxt is not initial and p_sgtxtj eq 'X' ) or
           ( tg_bsik_comp-zuonr eq i_zuonr and i_zuonr is not initial and p_zuonrj eq 'X' ) ).
        vl_partida_comp = 'X'.
      endif.
    endif.

    check vl_partida_comp is not initial.

    append tg_bsik_comp to p_tg_bsik_comp.

  endloop.

* PBI - 70679 - Inicio - CBRAND
*  IF rb_prod IS INITIAL.
  delete p_tg_bsik_comp where bukrs = i_bukrs
                          and belnr = i_belnr
                          and gjahr = i_gjahr
                          and buzei = i_buzei.
*  ELSE.
*    LOOP AT tg_bsik_copy WHERE zuonr = i_zuonr.
*      DELETE p_tg_bsik_comp WHERE bukrs = tg_bsik_copy-bukrs
*                            AND   belnr = tg_bsik_copy-belnr
*                            AND   gjahr = tg_bsik_copy-gjahr
*                            AND   buzei = tg_bsik_copy-buzei.
*    ENDLOOP.
*  ENDIF.
* PBI - 70679 - Fim - CBRAND
endform.

form f_ranges_tp_partida  using p_tp_conta.


  clear: r_umsks_p, r_umsks_p[],
         r_umskz_p, r_umskz_p[],
         r_shkzg_p, r_shkzg_p[],
         r_umsks_c, r_umsks_c[],
         r_umskz_c, r_umskz_c[].

  case p_tp_conta.
    when 'K'. "Fornecedor

      if ( p_cta_rz is initial ) and ( p_cta_nr is initial ).

        "Partida Principal
        r_umsks_p-sign   = 'I'.
        r_umsks_p-option = 'NE'.
        r_umsks_p-low    = ''.
        append r_umsks_p.

        r_umskz_p-sign   = 'I'.
        r_umskz_p-option = 'NE'.
        r_umskz_p-low    = 'F'.
        append r_umskz_p.

        r_shkzg_p-sign   = 'I'.
        r_shkzg_p-option = 'EQ'.
        r_shkzg_p-low    = 'S'.
        append r_shkzg_p.

        "Outras Partidas
        r_umsks_c-sign   = 'I'.
        r_umsks_c-option = 'EQ'.
        r_umsks_c-low    = ''.
        append r_umsks_c.

        r_umskz_c-sign   = 'I'.
        r_umskz_c-option = 'EQ'.
        r_umskz_c-low    = ''.
        append r_umskz_c.

        "Ranges Unificação Partida
        if p_ovpedj eq abap_true.
          if r_ovped[] is initial.
            r_ovped-sign   = 'I'.
            r_ovped-option = 'NE'.
            r_ovped-low    = ''.
            append r_ovped.
          endif.
        endif.

        if p_sgtxtj eq abap_true.
          if r_sgtxt[] is initial.
            r_sgtxt-sign   = 'I'.
            r_sgtxt-option = 'NE'.
            r_sgtxt-low    = ''.
            append r_sgtxt.
          endif.
        endif.

        if p_zuonrj eq abap_true.
          if r_zuonr[] is initial.
            r_zuonr-sign   = 'I'.
            r_zuonr-option = 'NE'.
            r_zuonr-low    = ''.
            append r_zuonr.
          endif.
        endif.

      else.

        if ( p_cta_rz is not initial ) and ( p_cta_nr is initial ).

          "Partida Principal
          r_umsks_p-sign   = 'I'.
          r_umsks_p-option = 'NE'.
          r_umsks_p-low    = ''.
          append r_umsks_p.

          r_umskz_p-sign   = 'I'.
          r_umskz_p-option = 'NE'.
          r_umskz_p-low    = 'F'.
          append r_umskz_p.

        elseif ( p_cta_nr is not initial ) and ( p_cta_rz is initial ).

          "Partida Principal
          r_umsks_p-sign   = 'I'.
          r_umsks_p-option = 'EQ'.
          r_umsks_p-low    = ''.
          append r_umsks_p.

          r_umskz_p-sign   = 'I'.
          r_umskz_p-option = 'EQ'.
          r_umskz_p-low    = ''.
          append r_umskz_p.

        endif.

      endif.


    when 'D'. "Ciente

      if ( p_cta_rz is initial ) and ( p_cta_nr is initial ).

        "Partida Principal
        r_umsks_p-sign   = 'I'.
        r_umsks_p-option = 'NE'.
        r_umsks_p-low    = ''.
        append r_umsks_p.

        r_umskz_p-sign   = 'I'.
        r_umskz_p-option = 'NE'.
        r_umskz_p-low    = 'F'.
        append r_umskz_p.

        r_shkzg_p-sign   = 'I'.
        r_shkzg_p-option = 'EQ'.
        r_shkzg_p-low    = 'H'.
        append r_shkzg_p.

        "Outras Partidas
        r_umsks_c-sign   = 'I'.
        r_umsks_c-option = 'EQ'.
        r_umsks_c-low    = ''.
        append r_umsks_c.

        r_umskz_c-sign   = 'I'.
        r_umskz_c-option = 'EQ'.
        r_umskz_c-low    = ''.
        append r_umskz_c.

        "Ranges Unificação Partida
        if ( p_ovpedj eq abap_true ) or
           ( p_dcsimj eq abap_true ).
          if r_ovped[] is initial.
            r_ovped-sign   = 'I'.
            r_ovped-option = 'NE'.
            r_ovped-low    = ''.
            append r_ovped.
          endif.
        endif.

        if p_sgtxtj eq abap_true.
          if r_sgtxt[] is initial.
            r_sgtxt-sign   = 'I'.
            r_sgtxt-option = 'NE'.
            r_sgtxt-low    = ''.
            append r_sgtxt.
          endif.
        endif.

        if p_zuonrj eq abap_true.
          if r_zuonr[] is initial.
            r_zuonr-sign   = 'I'.
            r_zuonr-option = 'NE'.
            r_zuonr-low    = ''.
            append r_zuonr.
          endif.
        endif.

*        IF ( p_dcsimj EQ abap_true ).
*          IF r_dcsim[] IS INITIAL.
*            r_dcsim-sign   = 'I'.
*            r_dcsim-option = 'NE'.
*            r_dcsim-low    = ''.
*            APPEND r_dcsim.
*          ENDIF.
*        ENDIF.

      else.

        if ( p_cta_rz is not initial ) and ( p_cta_nr is initial ).

          "Partida Principal
          r_umsks_p-sign   = 'I'.
          r_umsks_p-option = 'NE'.
          r_umsks_p-low    = ''.
          append r_umsks_p.

          r_umskz_p-sign   = 'I'.
          r_umskz_p-option = 'NE'.
          r_umskz_p-low    = 'F'.
          append r_umskz_p.

        elseif ( p_cta_nr is not initial ) and ( p_cta_rz is initial ).

          "Partida Principal
          r_umsks_p-sign   = 'I'.
          r_umsks_p-option = 'EQ'.
          r_umsks_p-low    = ''.
          append r_umsks_p.

          r_umskz_p-sign   = 'I'.
          r_umskz_p-option = 'EQ'.
          r_umskz_p-low    = ''.
          append r_umskz_p.

        endif.

      endif.



  endcase.

endform.

form f_moeda_empresa using p_bukrs type t001-bukrs
                           p_msg   type c.

  clear: tg_t001.

  read table tg_t001 with key bukrs = p_bukrs.

  if ( sy-subrc ne 0 ) or ( tg_t001-waers is initial ) or
     ( tg_t001-waers2 is initial ) or ( p_bukrs is initial ).
    clear: tg_t001.
    if p_msg = 'X'.
      message |Informações referente a moeda da empresa: { p_bukrs }, não encontrado ou incompleto!| type 'S'.
    endif.
    sy-subrc = 4.
  endif.

endform.

form f_aplic_text_def .

  wa_cabecalho_0110-sgtxt_rsd = wa_saida_0100-sgtxt.
  loop at it_saida_0110 assigning field-symbol(<saida_0110>).
    if vg_parausd is initial.
      <saida_0110>-sgtxt_rsd = 'Desmembramento Fatura'.
    else.
      <saida_0110>-sgtxt_rsd =  <saida_0110>-sgtxt.
    endif.
  endloop.

  leave to screen 0110.

endform.


form f_get_bsis_cbanco using p_bukrs     type bsas-bukrs
                             p_belnr     type bsas-belnr
                             p_gjahr     type bsas-gjahr
                    changing p_kursf     like bkpf-kursf.

  clear: tg_bsis_cbanco, p_kursf.

  read table tg_bsis_cbanco with key bukrs = p_bukrs
                                     gjahr = p_gjahr
                                     belnr = p_belnr binary search.
  if ( sy-subrc = 0 ) and
     ( tg_bsis_cbanco-dmbtr > 0 ) and
     ( tg_bsis_cbanco-dmbe2 > 0 ).

    try.
        p_kursf = tg_bsis_cbanco-dmbtr / tg_bsis_cbanco-dmbe2.
      catch cx_sy_arithmetic_overflow.
    endtry.

    if ( tg_bsis_cbanco-waers is not initial ) and
       ( tg_bsis_cbanco-waers ne 'BRL'       ) and
       ( tg_bsis_cbanco-waers ne 'USD'       ) and
       ( tg_bsis_cbanco-wrbtr > 0            ).

      try.
          p_kursf = tg_bsis_cbanco-dmbtr / tg_bsis_cbanco-wrbtr.
          "P_KURS2 = TG_BSIS_CBANCO-DMBE2 / TG_BSIS_CBANCO-WRBTR.
        catch cx_sy_arithmetic_overflow.
      endtry.
    endif.
  endif.

endform.

form f_call_screen_0120.

  perform f_seleciona_dados_0120.

  call screen 0120 starting at 2 2 ending at 178 25.

endform.

form f_montar_layout_log_erro.
  refresh estrutura.
  perform f_montar_estrutura using:
     01  ''   ''            'TG_ZIB_ERR' 'DT_ATUALIZACAO'     'Data'         '10' '' '' ,
     02  ''   ''            'TG_ZIB_ERR' 'HR_ATUALIZACAO'     'Hora'         '10' '' '' ,
     03  ''   ''            'TG_ZIB_ERR' 'TYPE'               'Tipo'         '10' '' '' ,
     04  ''   ''            'TG_ZIB_ERR' 'NUM'                'Num.'         '10' '' '' ,
     05  ''   ''            'TG_ZIB_ERR' 'MESSAGE'            'Mensagem'     '10' '' '' ,
     06  ''   ''            'TG_ZIB_ERR' 'MESSAGE_V1'         'Msg.1'        '10' '' '' ,
     07  ''   ''            'TG_ZIB_ERR' 'MESSAGE_V2'         'Msg.2'        '10' '' '' ,
     08  ''   ''            'TG_ZIB_ERR' 'MESSAGE_V3'         'Msg.3'        '10' '' '' ,
     09  ''   ''            'TG_ZIB_ERR' 'MESSAGE_V4'         'Msg.4'        '10' '' '' .

endform.                    " MONTAR_LAYOUT


form f_montar_estrutura using value(p_col_pos)       type i
                              value(p_ref_tabname)   like dd02d-tabname
                              value(p_ref_fieldname) like dd03d-fieldname
                              value(p_tabname)       like dd02d-tabname
                              value(p_field)         like dd03d-fieldname
                              value(p_scrtext_l)     like dd03p-scrtext_l
                              value(p_outputlen)
                              value(p_hotspot)
                              value(p_just).

  clear wa_estrutura.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-hotspot       = p_hotspot.
  wa_estrutura-just          = p_just.
  wa_estrutura-ddictxt       = 'L'.
  wa_estrutura-outputlen     = p_outputlen.


  if p_scrtext_l is not initial.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  endif.

  translate  wa_estrutura-fieldname     to upper case.
  translate  wa_estrutura-tabname       to upper case.
  translate  wa_estrutura-ref_tabname   to upper case.
  translate  wa_estrutura-ref_fieldname to upper case.

  append wa_estrutura to estrutura.

endform.                    " MONTAR_ESTRUTURA

form f_seleciona_dados_0120 .

  clear: it_saida_0120[].

  if p_augdt-low is initial.
    message 'Selecione um data de compensação!' type 'S'.
    exit.
  endif.

  select distinct *
    from zfit0139 into table @data(tg_0139)
   where bukrs in @p_bukrs
     and augdt in @p_augdt.

  loop at tg_0139 into data(_wl_0139).
    clear: wa_saida_0120.
    move-corresponding _wl_0139 to wa_saida_0120.

    if ( _wl_0139-belnr_ger is not initial ) and
       ( _wl_0139-stblg_ger is initial     ) and "Não foi estornado
       ( _wl_0139-anulado   is initial     ).    "Não foi anulado
      wa_saida_0120-st_ctb = icon_led_green.
    elseif ( _wl_0139-belnr_ger is initial ) and
           ( _wl_0139-stblg_ger is initial ) and "Não foi estornado
           ( _wl_0139-anulado   is initial ) and "Não foi anulado.
           ( _wl_0139-st_proc   eq '2'     ).    "Com erro
      wa_saida_0120-st_ctb = icon_led_red.
    elseif ( _wl_0139-belnr_ger  is initial ) and
           ( _wl_0139-stblg_ger  is initial ) and "Não foi estornado
           ( _wl_0139-anulado    is initial ) and "Não foi anulado.
           ( _wl_0139-st_proc    is initial ).    "Com erro
      wa_saida_0120-st_ctb = icon_led_yellow.
    elseif ( _wl_0139-belnr_ger is not initial ) and
           ( _wl_0139-stblg_ger is not initial ) and "Não foi estornado
           ( _wl_0139-anulado   is initial     ).    "Não foi anulado
      wa_saida_0120-st_ctb = icon_led_inactive.
    endif.

    append wa_saida_0120 to it_saida_0120.
  endloop.

  sort it_saida_0120 by anulado.

endform.

form f_add_part_residual tables p_ftpost      structure ftpost
                          using p_saida_0110  type ty_saida_0110
                       changing p_count_ft    type ftpost-count
                                p_error       type c.

  data: wl_vlrc(16),
        vdata_venc(10),
        wl_vlrn        type p decimals 2,
        wa_zfit0154    type zfit0154.

  clear: p_error.

  if p_saida_0110-st_comp  ne '3'.
    if p_saida_0110-vlr_rsd <= 0.
      message 'Valor Residual inconsistente!' type 'S'.
      p_error = 'X'.
      return.
    endif.
  endif.

  if p_saida_0110-kursf <= 0.
    message 'Taxa para gerar residual não encontrada!' type 'S'.
    p_error = 'X'.
    return.
  endif.

  if p_saida_0110-st_comp  ne '3'.
    case p_saida_0110-koart.
      when 'D'. "Cliente
        if ( p_saida_0110-bschl ne '01' ) and
           ( p_saida_0110-bschl ne '11' ) and
           ( p_saida_0110-bschl ne '09' ) and
           ( p_saida_0110-bschl ne '19' ).
          data(_erro_chave) = 'X'.
        endif.
      when 'K'. "Fornecedor
        if ( p_saida_0110-bschl ne '21' ) and
           ( p_saida_0110-bschl ne '31' ) and
           ( p_saida_0110-bschl ne '29' ) and
           ( p_saida_0110-bschl ne '39' ).
          _erro_chave = 'X'.


        endif.
    endcase.
  else.
    case p_saida_0110-koart.
      when 'D'. "Cliente
        select single *
           into wa_zfit0154
           from zfit0154
           where tipo = 'C'
           and   fg_soc = ''.

*-BUG 78517-11.05.2022-JT-inicio
        if p_saida_0110-vlr_rsdp < 0.
          p_saida_0110-bschl = '50'.
          p_saida_0110-parid = wa_zfit0154-cta_desc_obtido. "331102
        else.
          p_saida_0110-bschl = '40'.
          p_saida_0110-parid = wa_zfit0154-cta_desc_conc.   "431101
        endif.
*-BUG 78517-11.05.2022-JT-fim
      when 'K'. "Fornecedor
        select single *
          into wa_zfit0154
          from zfit0154
          where tipo = 'F'
          and   fg_soc = ''.

*-BUG 78517-11.05.2022-JT-inicio
        if p_saida_0110-vlr_rsdp < 0.
          p_saida_0110-bschl = '40'.
          p_saida_0110-parid = wa_zfit0154-cta_desc_conc.
        else.
          p_saida_0110-bschl = '50'.
          p_saida_0110-parid = wa_zfit0154-cta_desc_obtido.
        endif.
*-BUG 78517-11.05.2022-JT-fim
    endcase.


  endif.

  if _erro_chave is not initial.
    message 'Chave Lançamento não configurada para deixar Saldo Residual!' type 'S'.
    p_error = 'X'.
    return.
  endif.

  perform f_moeda_empresa using p_saida_0110-bukrs
                              'X'.
  if ( sy-subrc ne 0 ).
    p_error = 'X'.
    return.
  endif.

  if p_saida_0110-st_comp  ne '3'.
    if ( p_saida_0110-wrbtr + p_saida_0110-vlr_rsd ) ne p_saida_0110-dmbtr_aux.
      message 'Saldo Residual inconsistente!' type 'S'.
      p_error = 'X'.
      return.
    endif.
  endif.

  clear: wl_vlrn, wl_vlrc, vdata_venc.

  concatenate p_saida_0110-zfbdt+6(2) p_saida_0110-zfbdt+4(2) p_saida_0110-zfbdt(4) into vdata_venc separated by '.'.

  add 1 to p_count_ft.

  if p_saida_0110-st_comp  ne '3'.
    wl_vlrn = abs( p_saida_0110-vlr_rsd ).
  else.
    wl_vlrn = abs( p_saida_0110-vlr_rsdp ).
  endif.

  write: wl_vlrn to wl_vlrc.

  p_ftpost-stype = 'P'.
  p_ftpost-count = p_count_ft .

  p_ftpost-fnam = 'RF05A-NEWBS'.
  p_ftpost-fval =  p_saida_0110-bschl.
  append p_ftpost.

  p_ftpost-fnam = 'BSEG-HKONT'.
  p_ftpost-fval = p_saida_0110-parid.
  append p_ftpost.

  p_ftpost-fnam = 'BSEG-SGTXT'.
  if p_saida_0110-st_comp  ne '3'.
    p_ftpost-fval = p_saida_0110-sgtxt_rsd.
  else.
    p_ftpost-fval = 'Desconto Tolerância'.
  endif.
  append p_ftpost.

  p_ftpost-fnam = 'BSEG-ZUONR'.
  p_ftpost-fval = p_saida_0110-zuonr.
  if p_saida_0110-ebelp is not initial.
    p_ftpost-fval = p_ftpost-fval && p_saida_0110-ebelp.
  endif.
  append p_ftpost.

  if p_saida_0110-st_comp  ne '3'.
    p_ftpost-fnam = 'BSEG-GSBER'.
    p_ftpost-fval = p_saida_0110-gsber.
    append p_ftpost.

    p_ftpost-fnam = 'BSEG-HZUON'.
    p_ftpost-fval =  p_saida_0110-ovped.
    append p_ftpost.

    if p_saida_0110-zfbdt is not initial.
      p_ftpost-fnam = 'BSEG-ZFBDT'.
      p_ftpost-fval = vdata_venc.
      append p_ftpost.

      if ( p_saida_0110-umsks is initial ).
        p_ftpost-fnam = 'BSEG-ZBD1T'.
        p_ftpost-fval = p_saida_0110-zbd1t.
        condense p_ftpost-fval no-gaps.
        append p_ftpost.
      endif.
    endif.
  else.
    p_ftpost-fnam = 'BSEG-BUPLA'.
    p_ftpost-fval = p_saida_0110-gsber.
    append p_ftpost.
  endif.

  if p_saida_0110-umsks is not initial. "Adiantamento
    if p_saida_0110-st_comp  ne '3'.
      p_ftpost-fnam = 'RF05A-NEWUM'.
      p_ftpost-fval = p_saida_0110-umskz.
      append p_ftpost.
    endif.

*    IF ( p_saida_0110-anln1 IS NOT INITIAL ).
*      p_ftpost-fnam = 'BSEG-ANLN1'.
*      p_ftpost-fval = p_saida_0110-anln1.
*      APPEND p_ftpost.
*
*      IF p_saida_0110-anln2 IS NOT INITIAL.
*        p_ftpost-fnam = 'BSEG-ANLN2'.
*        p_ftpost-fval = p_saida_0110-anln2.
*        APPEND p_ftpost.
*      ENDIF.
*    ENDIF.
  endif.

  p_ftpost-fnam = 'BSEG-WRBTR'.
  p_ftpost-fval =  wl_vlrc.
  append p_ftpost.

  if p_saida_0110-waers ne 'USD'.
    wl_vlrn = wl_vlrn / abs( p_saida_0110-kursf ).
    if wl_vlrn < '0.01'.
      wl_vlrn = '0.01'.
      write: wl_vlrn to wl_vlrc.
      p_ftpost-fnam = 'BSEG-DMBE2'.
      p_ftpost-fval =  wl_vlrc.
      append p_ftpost.
    endif.
  endif.

*  IF p_saida_0110-waers NE tg_t001-waers.
*    wl_vlrn = wl_vlrn * abs( p_saida_0110-kursf ).
*    WRITE: wl_vlrn TO wl_vlrc.
*    p_ftpost-fnam = 'BSEG-DMBTR'.
*    p_ftpost-fval =  wl_vlrc.
*    APPEND p_ftpost.
*  ELSE.
*    wl_vlrn = wl_vlrn / abs( p_saida_0110-kursf ).
*    WRITE: wl_vlrn TO wl_vlrc.
*    p_ftpost-fnam = 'BSEG-DMBE2'.
*    p_ftpost-fval =  wl_vlrc.
*    APPEND p_ftpost.
*  ENDIF.

endform.

form f_atrib_doc_simulador  using p_vbeln
                         changing p_dcsim.

  clear: p_dcsim.

  check p_vbeln is not initial.

  read table tg_zsdt0041 with key vbeln = p_vbeln.
  if ( sy-subrc eq 0 ) and ( tg_zsdt0041-doc_simulacao is not initial ).
    p_dcsim = tg_zsdt0041-doc_simulacao.
    exit.
  endif.

  read table tg_zsdt0090 with key vbeln = p_vbeln.
  if ( sy-subrc eq 0 ) and ( tg_zsdt0090-doc_simulacao is not initial ).
*** PBI - 70679 - Inicio - Cbrand
    p_dcsim = tg_zsdt0090-doc_simulacao.
    exit.
*** PBI - 70679 - Fim - Cbrand
  endif.

  "Check se é uma Devolução/Recusa
  read table tg_vbfa_rd with key vbeln = p_vbeln.

  check sy-subrc eq 0.

  read table tg_zsdt0041 with key vbeln = tg_vbfa_rd-vbelv.
  if ( sy-subrc eq 0 ) and ( tg_zsdt0041-doc_simulacao is not initial ).
    p_dcsim = tg_zsdt0041-doc_simulacao.
    exit.
  endif.

  read table tg_zsdt0090 with key vbeln = tg_vbfa_rd-vbelv.
  if ( sy-subrc eq 0 ) and ( tg_zsdt0090-doc_simulacao is not initial ).
    p_dcsim = tg_zsdt0090-doc_simulacao.
    exit.
  endif.

endform.

form f_sel_part_comp using p_tp_conta.

  case p_tp_conta.
    when 'K'. "Fornecedor

      if ( p_ovped-low is not initial ) or
         ( p_sgtxt-low is not initial ) or
         ( p_zuonr-low is not initial ).

        select distinct *
          from bsik_view into corresponding fields of table @tg_bsik_comp
         where bukrs in @r_bukrs
           and umsks in @r_umsks_c
           and umskz in @r_umskz_c
           and ebeln in @r_ovped
           and sgtxt in @r_sgtxt
           and zuonr in @r_zuonr
           and blart ne 'VC'
           and dmbtr > 0
           and dmbe2 > 0.

      else.

        select distinct *
          from bsik_view into corresponding fields of table @tg_bsik_comp
          for all entries in @tg_bsik_adt
         where bukrs eq @tg_bsik_adt-bukrs
           and ( ( lifnr eq @tg_bsik_adt-lifnr ) or
                 ( ebeln eq @tg_bsik_adt-ebeln and ebeln ne '' ) )
           and umsks in @r_umsks_c
           and umskz in @r_umskz_c
           and ebeln in @r_ovped
           and sgtxt in @r_sgtxt
           and zuonr in @r_zuonr
           and blart ne 'VC'
           and ( ( ebeln = @tg_bsik_adt-ebeln and ebeln ne '' ) or
                 ( sgtxt = @tg_bsik_adt-sgtxt and sgtxt ne '' ) or
                 ( zuonr = @tg_bsik_adt-zuonr and zuonr ne '' ) )
           and dmbtr > 0
           and dmbe2 > 0.
      endif.

    when 'D'. "Cliente

      if ( p_ovped-low is not initial ) or
         ( p_sgtxt-low is not initial ) or
         ( p_zuonr-low is not initial ) or
         ( p_dcsimj    eq abap_true   ).

        select distinct *
          from bsid_view into corresponding fields of table @tg_bsid_comp
         where bukrs in @r_bukrs
           and umsks in @r_umsks_c
           and umskz in @r_umskz_c
           and vbel2 in @r_ovped
           and sgtxt in @r_sgtxt
           and zuonr in @r_zuonr
           and blart ne 'VC'
           and dmbtr > 0
           and dmbe2 > 0.

      else.

        select distinct *
          from bsid_view into corresponding fields of table @tg_bsid_comp
          for all entries in @tg_bsid_adt
         where bukrs eq @tg_bsid_adt-bukrs
           and ( ( kunnr eq @tg_bsid_adt-kunnr ) or
                 ( vbel2 eq @tg_bsid_adt-vbel2 and vbel2 ne '' ) )
           and umsks in @r_umsks_c
           and umskz in @r_umskz_c
           and vbel2 in @r_ovped
           and sgtxt in @r_sgtxt
           and zuonr in @r_zuonr
           and blart ne 'VC'
           and ( ( vbel2 = @tg_bsid_adt-vbel2 and vbel2 ne '' ) or
                 ( sgtxt = @tg_bsid_adt-sgtxt and sgtxt ne '' ) or
                 ( zuonr = @tg_bsid_adt-zuonr and zuonr ne '' ) )
           and dmbtr > 0
           and dmbe2 > 0.

      endif.

  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_desc using p_saida_0100 type ty_saida_0100..
  data wa_zfit0154 type zfit0154.
  "
  loop at it_saida_0110 into data(w110) where check is not initial and belnr is not initial.

  endloop.

  if w110-parid is initial.
    message 'Selecione partidas a compensar!' type 'I'.
    exit.
  endif.


  clear w110.
  loop at it_saida_0110 into w110 where manual is not initial.

  endloop.
  if w110-manual = 'X'.
    message 'Desconto já incluido!' type 'I'.
    exit.
  endif.



  clear: wa_saida_0110, gt_estilo[].

  loop at it_saida_0110 into w110 where manual is initial.

  endloop.

  case w110-koart.
    when 'D'. "Cliente
      select single *
        into wa_zfit0154
        from zfit0154
        where tipo = 'C'
        and   fg_soc = ''.
*-BUG 78517-11.05.2022-JT-inicio
      if wa_cabecalho_0110-sld_dmbtr > 0.
        wa_saida_0110-bschl = '50'.
        wa_saida_0110-hkont = wa_zfit0154-cta_desc_obtido.
      else.
        wa_saida_0110-bschl = '40'.
        wa_saida_0110-hkont = wa_zfit0154-cta_desc_conc.
      endif.
*-BUG 78517-11.05.2022-JT-fim
    when 'K'.
      select single *
      into wa_zfit0154
      from zfit0154
      where tipo = 'F'
      and   fg_soc = ''.

*-BUG 78517-11.05.2022-JT-inicio
      if wa_cabecalho_0110-sld_dmbtr > 0.
        wa_saida_0110-bschl = '40'.
        wa_saida_0110-hkont = wa_zfit0154-cta_desc_conc.
      else.
        wa_saida_0110-bschl = '50'.
        wa_saida_0110-hkont = wa_zfit0154-cta_desc_obtido.
      endif.
*-BUG 78517-11.05.2022-JT-fim
  endcase.


  wa_saida_0110-manual = 'X'.
  wa_saida_0110-ic_manual = icon_checked.
  wa_saida_0110-kursf  = wa_saida_0100-kursf.
  wa_saida_0110-koart  = wa_saida_0100-koart.
  wa_saida_0110-bukrs  = wa_saida_0100-bukrs.
  wa_saida_0110-waers  = wa_saida_0100-waers.
  wa_saida_0110-wrbtr  = abs( wa_cabecalho_0110-sld_dmbtr ).
  wa_saida_0110-dmbe2  = abs( wa_cabecalho_0110-sld_dmbtr / wa_saida_0100-kursf ).

*-BUG 78517-11.05.2022-JT-inicio
* wa_saida_0110-sgtxt_rsd  = 'Desconto Tolerância de fechamento de lote'.
  wa_saida_0110-sgtxt_rsd  = 'Desconto Tolerância'.
*-BUG 78517-11.05.2022-JT-inicio

  wa_saida_0110-sgtxt      = 'Desconto Tolerância de fechamento de lote'.

  wl_estilo-fieldname    = 'VLR_RSD'.
  wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
  append wl_estilo to gt_estilo.

  insert lines of gt_estilo into table wa_saida_0110-estilo.

  append wa_saida_0110 to it_saida_0110.

endform.
*&---------------------------------------------------------------------*
*& Form proc_doc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form proc_doc .
  clear: vl_comp.
  data  verro(1).
  clear verro.
  loop at p_bukrs .
    authority-check object 'F_BKPF_BUK'
       id 'ACTVT' field '03'
       id 'BUKRS' field p_bukrs-low.
    if sy-subrc ne 0.
      message | Sem acesso a empresa: { p_bukrs-low } | type 'I'.
      verro = 'X'.
      exit.
    endif.
  endloop.
  if verro is initial.
    perform f_selecionar_dados.
    check vg_not_found is initial.
    perform: f_processa_dados,
             f_refresh_alv using '0100'.

    if p_cplib is not initial. "Compensar Adiantamentos com Saldo Zerado
      loop at it_saida_0100 into wa_saida_0100 where st_comp = '1' or st_comp = '3'.
        perform f_get_part_comp using wa_saida_0100.
        perform f_bapi_f51 using abap_false
                        changing wa_saida_0100
                                 vl_error
                                 vl_doc_comp.
        vl_comp = 'X'.
      endloop.

      if vl_comp is not initial.
        perform f_renovar_cons.
      endif.
    endif.
  endif.
endform.
