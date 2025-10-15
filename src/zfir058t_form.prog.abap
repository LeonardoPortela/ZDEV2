*&---------------------------------------------------------------------*
*&  Include           ZFIR058_FORM
*&---------------------------------------------------------------------*

form F_SELECIONAR_DADOS .

  data: WL_X001 type X001.

  perform: F_LIMPA_VARIAVEIS,
           F_CONFIG_RANGES.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
  free R_XREF3.
  R_XREF3-SIGN = 'I'.
  R_XREF3-OPTION = 'CP'.
  concatenate '*' P_PRODU-LOW
         into R_XREF3-LOW.
  append R_XREF3.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

  "Seleciona adiantamentos.
  if RB_CLI is not initial.

    perform F_RANGES_TP_PARTIDA using 'D'.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    select distinct *
      from BSID_view
     where BUKRS in @R_BUKRS
       and KUNNR in @R_PARID
       and GSBER in @P_GSBER
       and UMSKS in @R_UMSKS_P
       and UMSKZ in @R_UMSKZ_P
       and VBEL2 in @R_OVPED
       and SGTXT in @R_SGTXT
       and ZUONR in @R_ZUONR
      into corresponding fields of table @TG_BSID_ADT.
*      AND blart NE 'VC'
*      AND ( vbel2 NE '' OR sgtxt NE '' OR zuonr NE '' )
*      AND shkzg IN r_shkzg_p.

    delete TG_BSID_ADT where not ( BLART ne 'VC'
                             and ( VBEL2 ne '' or SGTXT ne '' or ZUONR ne '' )
                             and   SHKZG in R_SHKZG_P ).
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    if TG_BSID_ADT[] is initial.
      message 'Nenhum registro encontrado!' type 'S'.
      return.
    endif.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    TG_BSID_ADT_AUX[] =  TG_BSID_ADT[].
    sort TG_BSID_ADT_AUX by BUKRS BELNR.
    delete adjacent duplicates from TG_BSID_ADT_AUX
                          comparing BUKRS BELNR.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    data ETL58C4R5439 type table of BSEG.
    call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      exporting
        IT_FOR_ALL_ENTRIES = TG_BSID_ADT_AUX[]
        I_WHERE_CLAUSE     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR|
      importing
        ET_BSEG            = ETL58C4R5439
      exceptions
        NOT_FOUND          = 1.
    if SY-SUBRC = 0 and LINES( ETL58C4R5439 ) > 0.
      move-corresponding ETL58C4R5439 to TG_BSEG[] keeping target lines.
      SY-DBCNT = LINES( ETL58C4R5439 ).
    else.
      SY-SUBRC = 4.
      SY-DBCNT = 0.
    endif.


*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    select distinct *
      from bsis_view
      inner join SKB1 on bsis_view~BUKRS = SKB1~BUKRS "#EC CI_DB_OPERATION_OK[2431747]
                               and bsis_view~HKONT = SKB1~SAKNR
       for all entries in @TG_BSID_ADT
     where bsis_view~BUKRS eq @TG_BSID_ADT-BUKRS
       and bsis_view~GJAHR eq @TG_BSID_ADT-GJAHR
       and bsis_view~BELNR eq @TG_BSID_ADT-BELNR
      appending corresponding fields of table @TG_BSIS_CBANCO.
*      AND skb1~fdlev IN r_fdlev_banco.

    delete TG_BSIS_CBANCO where not ( FDLEV in R_FDLEV_BANCO[] ).
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio

    select distinct *
      from T001 into corresponding fields of table TG_T001
       for all entries in TG_BSID_ADT
     where BUKRS = TG_BSID_ADT-BUKRS.

    loop at TG_BSID_ADT.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          INPUT  = TG_BSID_ADT-VBEL2
        importing
          OUTPUT = TG_BSID_ADT-VBEL2.

      modify TG_BSID_ADT.
    endloop.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    TG_BSID_ADT_AUX[] =  TG_BSID_ADT[].
    sort TG_BSID_ADT_AUX by VBEL2.
    delete adjacent duplicates from TG_BSID_ADT_AUX
                          comparing VBEL2.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    select distinct *
      from VBAK into corresponding fields of table TG_VBAK
      for all entries in TG_BSID_ADT_AUX
     where VBELN = TG_BSID_ADT_AUX-VBEL2.

    if TG_VBAK[] is not initial.
      select distinct *
        from TSPAT into corresponding fields of table TG_TSPAT
         for all entries in TG_VBAK
       where SPRAS = SY-LANGU
         and SPART = TG_VBAK-SPART.
    endif.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    TG_BSID_ADT_AUX[] =  TG_BSID_ADT[].
    sort TG_BSID_ADT_AUX by BUKRS BELNR GJAHR.
    delete adjacent duplicates from TG_BSID_ADT_AUX
                          comparing BUKRS BELNR GJAHR.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    "Seleciona Cabeçalhos
    select distinct *
     from BKPF into corresponding fields of table TG_BKPF
     for all entries in TG_BSID_ADT_AUX
    where BUKRS eq TG_BSID_ADT_AUX-BUKRS
      and BELNR eq TG_BSID_ADT_AUX-BELNR
      and GJAHR eq TG_BSID_ADT_AUX-GJAHR.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    TG_BSID_ADT_AUX[] =  TG_BSID_ADT[].
    sort TG_BSID_ADT_AUX by KUNNR.
    delete adjacent duplicates from TG_BSID_ADT_AUX
                          comparing KUNNR.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    select distinct *
      from KNA1 into corresponding fields of table TG_KNA1
       for all entries in TG_BSID_ADT_AUX
     where KUNNR = TG_BSID_ADT_AUX-KUNNR.

    "Seleciona Partidas Compensação.
    perform F_SEL_PART_COMP using 'D'.

    if TG_BSID_COMP[] is not initial.
      "Seleciona Cabeçalhos

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      TG_BSID_COMP_AUX2[] =  TG_BSID_COMP[].
      sort TG_BSID_COMP_AUX2 by BUKRS BELNR GJAHR.
      delete adjacent duplicates from TG_BSID_COMP_AUX2
                            comparing BUKRS BELNR GJAHR.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      select distinct *
       from BKPF appending corresponding fields of table TG_BKPF
       for all entries in TG_BSID_COMP_AUX2
      where BUKRS eq TG_BSID_COMP_AUX2-BUKRS
        and BELNR eq TG_BSID_COMP_AUX2-BELNR
        and GJAHR eq TG_BSID_COMP_AUX2-GJAHR.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      TG_BSID_COMP_AUX2[] =  TG_BSID_COMP[].
      sort TG_BSID_COMP_AUX2 by KUNNR.
      delete adjacent duplicates from TG_BSID_COMP_AUX2
                            comparing KUNNR.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      select distinct *
        from KNA1 appending corresponding fields of table TG_KNA1
         for all entries in TG_BSID_COMP_AUX2
       where KUNNR = TG_BSID_COMP_AUX2-KUNNR.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      TG_BSID_COMP_AUX2[] =  TG_BSID_COMP[].
      sort TG_BSID_COMP_AUX2 by BUKRS BELNR.
      delete adjacent duplicates from TG_BSID_COMP_AUX2
                            comparing BUKRS BELNR.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      data ETL181C6R188 type table of BSEG.
      call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
        exporting
          IT_FOR_ALL_ENTRIES = TG_BSID_COMP_AUX2[]
          I_WHERE_CLAUSE     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR|
        importing
          ET_BSEG            = ETL181C6R188
        exceptions
          NOT_FOUND          = 1.
      if SY-SUBRC = 0 and LINES( ETL181C6R188 ) > 0.
        move-corresponding ETL181C6R188 to TG_BSEG[] keeping target lines.
        SY-DBCNT = LINES( ETL181C6R188 ).
      else.
        SY-SUBRC = 4.
        SY-DBCNT = 0.
      endif.


*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      TG_BSID_COMP_AUX2[] =  TG_BSID_COMP[].
      sort TG_BSID_COMP_AUX2 by VBEL2.
      delete adjacent duplicates from TG_BSID_COMP_AUX2
                            comparing VBEL2.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      select distinct *
        from VBAK appending corresponding fields of table TG_VBAK
         for all entries in TG_BSID_COMP_AUX2
       where VBELN eq TG_BSID_COMP_AUX2-VBEL2.

    endif.

    if TG_VBAK[] is not initial.

      "Recusa e Devolução
      select distinct *
        from VBFA appending corresponding fields of table TG_VBFA_RD
         for all entries in TG_VBAK
       where VBELN    eq TG_VBAK-VBELN
         and VBTYP_N  in ('H','L', 'C')
         and VBTYP_V  eq 'C'.

      if TG_VBFA_RD[] is not initial.
        select distinct *
          from VBAK appending corresponding fields of table TG_VBAK
           for all entries in TG_VBFA_RD
         where VBELN eq TG_VBFA_RD-VBELV.
      endif.

      select DISTINCT *
        from ZSDT0041 appending corresponding fields of table TG_ZSDT0041
         for all entries in TG_VBAK
       where VBELN         eq TG_VBAK-VBELN
         and DOC_SIMULACAO ne '0000000000'.

      select distinct *
        from ZSDT0090 appending corresponding fields of table TG_ZSDT0090
         for all entries in TG_VBAK
       where VBELN         eq TG_VBAK-VBELN
         and DOC_SIMULACAO ne '0000000000'
         and ESTORNO       ne 'X'.

    endif.

    loop at TG_BSID_ADT.

      "Atualização Dados Imobilizado
      if TG_BSID_ADT-ANLN1 is initial.
        loop at TG_BSEG where BUKRS = TG_BSID_ADT-BUKRS
                          and BELNR = TG_BSID_ADT-BELNR
                          and ANLN1 is not initial.
          TG_BSID_ADT-ANLN1  = TG_BSEG-ANLN1.
          TG_BSID_ADT-ANLN2  = TG_BSEG-ANLN2.
          exit.
        endloop.
      endif.

      "Atribuir Documento Simulador
      perform F_ATRIB_DOC_SIMULADOR using TG_BSID_ADT-VBEL2
                                 changing TG_BSID_ADT-DCSIM.

      modify TG_BSID_ADT.

    endloop.

    loop at TG_BSID_COMP.

      "Atualização Dados Imobilizado
      if TG_BSID_COMP-ANLN1 is initial.
        loop at TG_BSEG where BUKRS = TG_BSID_COMP-BUKRS
                          and BELNR = TG_BSID_COMP-BELNR
                          and ANLN1 is not initial.
          TG_BSID_COMP-ANLN1  = TG_BSEG-ANLN1.
          TG_BSID_COMP-ANLN2  = TG_BSEG-ANLN2.
          exit.
        endloop.
      endif.

      "Atribuir Documento Simulador
      perform F_ATRIB_DOC_SIMULADOR using TG_BSID_COMP-VBEL2
                                 changing TG_BSID_COMP-DCSIM.

      modify TG_BSID_COMP.
    endloop.

    if R_DCSIM[] is not initial.
      delete TG_BSID_ADT  where DCSIM not in R_DCSIM.
      delete TG_BSID_COMP where DCSIM not in R_DCSIM.
    endif.

  endif.

  if RB_FORN is not initial or RB_PROD is not initial.

    perform F_RANGES_TP_PARTIDA using 'K'.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    if RB_PROD is not initial.
      select distinct *
          from bsik_view
         where BUKRS in @R_BUKRS
           and GSBER in @P_GSBER
           and LIFNR in @R_PARID
           and EBELN in @R_OVPED
           and SGTXT in @R_SGTXT
           and ZUONR in @R_ZUONR
           and XREF1 in @P_SAFRA
           and UMSKZ ne 'Z'
           and BLART in ( 'MA', 'MB' )
           and ( EBELN ne '' or SGTXT ne '' or ZUONR ne '' )
        into corresponding fields of table @TG_BSIK_ADT.

      delete TG_BSIK_ADT where not ( XREF1 in P_SAFRA[]
                          and   XREF3 in R_XREF3[] ).


    else.
      select distinct *
        from bsik_view
       where BUKRS in @R_BUKRS
         and LIFNR in @R_PARID
         and GSBER in @P_GSBER
         and UMSKS in @R_UMSKS_P
         and UMSKZ in @R_UMSKZ_P
         and EBELN in @R_OVPED
         and SGTXT in @R_SGTXT
         and ZUONR in @R_ZUONR
        into corresponding fields of table @TG_BSIK_ADT.
*      AND XREF1 IN P_SAFRA.
*      AND XREF3 LIKE P_PRODU-LOW
*      AND BLART NE 'VC'
*      AND ( EBELN NE '' OR SGTXT NE '' OR ZUONR NE '' )
*      AND SHKZG IN R_SHKZG_P.

      delete TG_BSIK_ADT where not ( XREF1 in P_SAFRA[]
                               and   XREF3 in R_XREF3[]
                               and   BLART ne 'VC'
                               and ( EBELN ne '' or SGTXT ne '' or ZUONR ne '' )
                               and   SHKZG in R_SHKZG_P[] ).
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim
    endif.

    if TG_BSIK_ADT[] is initial.
      message 'Nenhum registro encontrado!' type 'S'.
      return.
    endif.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    TG_BSIK_ADT_AUX[] =  TG_BSIK_ADT[].
    sort TG_BSIK_ADT_AUX by BUKRS BELNR.
    delete adjacent duplicates from TG_BSIK_ADT_AUX
                          comparing BUKRS BELNR.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    data ETL341C4R7781 type table of BSEG.
    call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      exporting
        IT_FOR_ALL_ENTRIES = TG_BSIK_ADT_AUX[]
        I_WHERE_CLAUSE     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR|
      importing
        ET_BSEG            = ETL341C4R7781
      exceptions
        NOT_FOUND          = 1.
    if SY-SUBRC = 0 and LINES( ETL341C4R7781 ) > 0.
      move-corresponding ETL341C4R7781 to TG_BSEG[] keeping target lines.
      SY-DBCNT = LINES( ETL341C4R7781 ).
    else.
      SY-SUBRC = 4.
      SY-DBCNT = 0.
    endif.


    loop at TG_BSIK_ADT where EBELN is not initial
                          and EBELP is initial.
      clear: TG_EKPO_AUX[].
      loop at TG_BSEG where BUKRS eq TG_BSIK_ADT-BUKRS
                        and BELNR eq TG_BSIK_ADT-BELNR
                        and EBELN eq TG_BSIK_ADT-EBELN
                        and EBELP is not initial.
        TG_EKPO_AUX-EBELN = TG_BSEG-EBELN.
        TG_EKPO_AUX-EBELP = TG_BSEG-EBELP.
        append TG_EKPO_AUX.
      endloop.

      sort TG_EKPO_AUX by EBELN EBELP.
      delete adjacent duplicates from TG_EKPO_AUX comparing EBELN EBELP.
      read table TG_EKPO_AUX index 1.

      if ( LINES( TG_EKPO_AUX[] ) = 1 ).
        TG_BSIK_ADT-EBELP = TG_EKPO_AUX-EBELP.
      elseif ( LINES( TG_EKPO_AUX[] ) = 0 ).
        TG_BSIK_ADT-EBELP = '00010'.
      endif.
      modify TG_BSIK_ADT.
    endloop.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    TG_BSIK_ADT_AUX[] =  TG_BSIK_ADT[].
    sort TG_BSIK_ADT_AUX by EBELN EBELP.
    delete adjacent duplicates from TG_BSIK_ADT_AUX
                          comparing EBELN EBELP.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    select distinct *
      from EKKN appending corresponding fields of table TG_EKKN
       for all entries in TG_BSIK_ADT_AUX
     where EBELN = TG_BSIK_ADT_AUX-EBELN
       and EBELP = TG_BSIK_ADT_AUX-EBELP.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    select distinct *
      from bsis_view
      inner join SKB1 on bsis_view~BUKRS = SKB1~BUKRS "#EC CI_DB_OPERATION_OK[2431747]
                         and bsis_view~HKONT = SKB1~SAKNR
       for all entries in @TG_BSIK_ADT
     where bsis_view~BUKRS eq @TG_BSIK_ADT-BUKRS
       and bsis_view~GJAHR eq @TG_BSIK_ADT-GJAHR
       and bsis_view~BELNR eq @TG_BSIK_ADT-BELNR
      appending corresponding fields of table @TG_BSIS_CBANCO.
*      AND skb1~fdlev IN r_fdlev_banco.

    delete TG_BSIS_CBANCO where not ( FDLEV in R_FDLEV_BANCO[] ).
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    select distinct *
      from T001 into corresponding fields of table TG_T001
       for all entries in TG_BSIK_ADT
     where BUKRS = TG_BSIK_ADT-BUKRS.

    loop at TG_BSIK_ADT.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          INPUT  = TG_BSIK_ADT-EBELN
        importing
          OUTPUT = TG_BSIK_ADT-EBELN.

      modify TG_BSIK_ADT.
    endloop.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    TG_BSIK_ADT_AUX[] =  TG_BSIK_ADT[].
    sort TG_BSIK_ADT_AUX by EBELN.
    delete adjacent duplicates from TG_BSIK_ADT_AUX
                          comparing EBELN.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    select distinct *
      from EKKO into corresponding fields of table TG_EKKO
       for all entries in TG_BSIK_ADT_AUX
     where EBELN = TG_BSIK_ADT_AUX-EBELN.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    TG_BSIK_ADT_AUX[] =  TG_BSIK_ADT[].
    sort TG_BSIK_ADT_AUX by BUKRS BELNR GJAHR.
    delete adjacent duplicates from TG_BSIK_ADT_AUX
                          comparing BUKRS BELNR GJAHR.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    "Seleciona Cabeçalhos
    select distinct *
     from BKPF into corresponding fields of table TG_BKPF
     for all entries in TG_BSIK_ADT_AUX
    where BUKRS eq TG_BSIK_ADT_AUX-BUKRS
      and BELNR eq TG_BSIK_ADT_AUX-BELNR
      and GJAHR eq TG_BSIK_ADT_AUX-GJAHR.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    TG_BSIK_ADT_AUX[] =  TG_BSIK_ADT[].
    sort TG_BSIK_ADT_AUX by LIFNR.
    delete adjacent duplicates from TG_BSIK_ADT_AUX
                          comparing LIFNR.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    select distinct *
      from LFA1 into corresponding fields of table TG_LFA1
       for all entries in TG_BSIK_ADT_AUX
     where LIFNR = TG_BSIK_ADT_AUX-LIFNR.

    "Seleciona Partidas Compensação.
    perform F_SEL_PART_COMP using 'K'.

    if TG_BSIK_COMP[] is not initial.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      TG_BSIK_COMP_AUX2[] =  TG_BSIK_COMP[].
      sort TG_BSIK_COMP_AUX2 by BUKRS BELNR GJAHR.
      delete adjacent duplicates from TG_BSIK_COMP_AUX2
                            comparing BUKRS BELNR GJAHR.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      "Seleciona Cabeçalhos
      select distinct *
       from BKPF appending corresponding fields of table TG_BKPF
       for all entries in TG_BSIK_COMP_AUX2
      where BUKRS eq TG_BSIK_COMP_AUX2-BUKRS
        and BELNR eq TG_BSIK_COMP_AUX2-BELNR
        and GJAHR eq TG_BSIK_COMP_AUX2-GJAHR.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      TG_BSIK_COMP_AUX2[] =  TG_BSIK_COMP[].
      sort TG_BSIK_COMP_AUX2 by LIFNR.
      delete adjacent duplicates from TG_BSIK_COMP_AUX2
                            comparing LIFNR.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      select distinct *
        from LFA1 appending corresponding fields of table TG_LFA1
         for all entries in TG_BSIK_COMP_AUX2
       where LIFNR = TG_BSIK_COMP_AUX2-LIFNR.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      TG_BSIK_COMP_AUX2[] =  TG_BSIK_COMP[].
      sort TG_BSIK_COMP_AUX2 by BUKRS BELNR.
      delete adjacent duplicates from TG_BSIK_COMP_AUX2
                            comparing BUKRS BELNR..
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      data ETL492C6R6628 type table of BSEG.
      call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
        exporting
          IT_FOR_ALL_ENTRIES = TG_BSIK_COMP_AUX2[]
          I_WHERE_CLAUSE     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR|
        importing
          ET_BSEG            = ETL492C6R6628
        exceptions
          NOT_FOUND          = 1.
      if SY-SUBRC = 0 and LINES( ETL492C6R6628 ) > 0.
        move-corresponding ETL492C6R6628 to TG_BSEG[] keeping target lines.
        SY-DBCNT = LINES( ETL492C6R6628 ).
      else.
        SY-SUBRC = 4.
        SY-DBCNT = 0.
      endif.


*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      TG_BSIK_COMP_AUX2[] =  TG_BSIK_COMP[].
      sort TG_BSIK_COMP_AUX2 by EBELN EBELP.
      delete adjacent duplicates from TG_BSIK_COMP_AUX2
                            comparing EBELN EBELP.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      select distinct *
        from EKKN appending corresponding fields of table TG_EKKN
         for all entries in TG_BSIK_COMP_AUX2
       where EBELN = TG_BSIK_COMP_AUX2-EBELN
         and EBELP = TG_BSIK_COMP_AUX2-EBELP.

      loop at TG_BSIK_COMP where EBELN is not initial
                             and EBELP is initial.
        clear: TG_EKPO_AUX[].
        loop at TG_BSEG where BUKRS eq TG_BSIK_COMP-BUKRS
                          and BELNR eq TG_BSIK_COMP-BELNR
                          and EBELN eq TG_BSIK_COMP-EBELN
                          and EBELP is not initial.
          TG_EKPO_AUX-EBELN = TG_BSEG-EBELN.
          TG_EKPO_AUX-EBELP = TG_BSEG-EBELP.
          append TG_EKPO_AUX.
        endloop.

        sort TG_EKPO_AUX by EBELN EBELP.
        delete adjacent duplicates from TG_EKPO_AUX comparing EBELN EBELP.
        read table TG_EKPO_AUX index 1.

        if ( LINES( TG_EKPO_AUX[] ) = 1 ).
          TG_BSIK_COMP-EBELP = TG_EKPO_AUX-EBELP.
        elseif ( LINES( TG_EKPO_AUX[] ) = 0 ).
          TG_BSIK_COMP-EBELP = '00010'.
        endif.
        modify TG_BSIK_COMP.
      endloop.
    endif.

    "Atualização Dados Imobilizado
    loop at TG_BSIK_ADT.
      if TG_BSIK_ADT-ANLN1 is initial.
        loop at TG_BSEG where BUKRS = TG_BSIK_ADT-BUKRS
                          and BELNR = TG_BSIK_ADT-BELNR
                          and ANLN1 is not initial.
          TG_BSIK_ADT-ANLN1  = TG_BSEG-ANLN1.
          TG_BSIK_ADT-ANLN2  = TG_BSEG-ANLN2.
          modify TG_BSIK_ADT.
          exit.
        endloop.
      endif.

      "Busca Classificação Contabil
      if TG_BSIK_ADT-ANLN1 is initial.
        loop at TG_EKKN where EBELN = TG_BSIK_ADT-EBELN
                          and EBELP = TG_BSIK_ADT-EBELP
                          and ANLN1 is not initial.
          TG_BSIK_ADT-ANLN1  = TG_EKKN-ANLN1.
          TG_BSIK_ADT-ANLN2  = TG_EKKN-ANLN2.
          modify TG_BSIK_ADT.
          exit.
        endloop.
      endif.
    endloop.

    loop at TG_BSIK_COMP.
      if TG_BSIK_COMP-ANLN1 is initial.
        loop at TG_BSEG where BUKRS = TG_BSIK_COMP-BUKRS
                          and BELNR = TG_BSIK_COMP-BELNR
                          and ANLN1 is not initial.
          TG_BSIK_COMP-ANLN1  = TG_BSEG-ANLN1.
          TG_BSIK_COMP-ANLN2  = TG_BSEG-ANLN2.
          modify TG_BSIK_COMP.
          exit.
        endloop.
      endif.

      "Busca Classificação Contabil
      if TG_BSIK_COMP-ANLN1 is initial.
        loop at TG_EKKN where EBELN = TG_BSIK_COMP-EBELN
                          and EBELP = TG_BSIK_COMP-EBELP
                          and ANLN1 is not initial.
          TG_BSIK_COMP-ANLN1  = TG_EKKN-ANLN1.
          TG_BSIK_COMP-ANLN2  = TG_EKKN-ANLN2.
          modify TG_BSIK_COMP.
          exit.
        endloop.
      endif.

    endloop.

  endif.

  loop at TG_T001.
    clear: WL_X001.

    call function 'FI_CURRENCY_INFORMATION'
      exporting
        I_BUKRS = TG_T001-BUKRS
      importing
        E_X001  = WL_X001.

    TG_T001-WAERS2 = WL_X001-HWAE2.

    modify TG_T001.
  endloop.

  sort TG_BSIS_CBANCO by BUKRS GJAHR BELNR.

endform.

form F_PROCESSA_DADOS .
  data: WA_ZFIT0154 type ZFIT0154,
        VDIF_INT    type ZFIT0154-VLR_TOLER,
        VDIF_FOR    type ZFIT0154-VLR_TOLER.

  loop at TG_BSID_ADT.
    clear: WA_SAIDA_0100, TG_KNA1, TG_VBAK, TG_TSPAT.

    perform F_MOEDA_EMPRESA using TG_BSID_ADT-BUKRS
                                  'X'.
    if ( SY-SUBRC ne 0 ).
      return.
    endif.

    read table TG_BKPF with key BUKRS = TG_BSID_ADT-BUKRS
                                BELNR = TG_BSID_ADT-BELNR
                                GJAHR = TG_BSID_ADT-GJAHR.
    check SY-SUBRC = 0.

    read table TG_KNA1 with key KUNNR = TG_BSID_ADT-KUNNR.
    check SY-SUBRC = 0.

    read table TG_VBAK with key VBELN = TG_BSID_ADT-VBEL2.
    if SY-SUBRC = 0.
      read table TG_TSPAT with key SPART = TG_VBAK-SPART.
      if SY-SUBRC = 0.
        WA_SAIDA_0100-SPART = TG_VBAK-SPART.  "Setor Atividade
        WA_SAIDA_0100-VTEXT = TG_TSPAT-VTEXT. "Descr. Setor Atividade
      endif.
    endif.


    WA_SAIDA_0100-BUKRS     = TG_BSID_ADT-BUKRS.
    WA_SAIDA_0100-PARID     = TG_KNA1-KUNNR.
    WA_SAIDA_0100-NAME1     = TG_KNA1-NAME1.
    WA_SAIDA_0100-BELNR     = TG_BSID_ADT-BELNR.
    WA_SAIDA_0100-BUZEI     = TG_BSID_ADT-BUZEI.
    WA_SAIDA_0100-GJAHR     = TG_BSID_ADT-GJAHR.
    WA_SAIDA_0100-BLDAT     = TG_BSID_ADT-BLDAT.
    WA_SAIDA_0100-BUDAT     = TG_BSID_ADT-BUDAT.
    WA_SAIDA_0100-WAERS     = TG_BSID_ADT-WAERS.
    WA_SAIDA_0100-WRBTR     = TG_BSID_ADT-WRBTR.
    WA_SAIDA_0100-DMBTR     = TG_BSID_ADT-DMBTR.
    WA_SAIDA_0100-DMBE2     = TG_BSID_ADT-DMBE2.
*    wa_saida_0100-dmbtr_aux = tg_bsid_adt-dmbtr.
    WA_SAIDA_0100-DMBTR_AUX = TG_BSID_ADT-WRBTR.
    WA_SAIDA_0100-DMBE2_AUX = TG_BSID_ADT-DMBE2.
    WA_SAIDA_0100-HKONT     = TG_BSID_ADT-HKONT.
    WA_SAIDA_0100-BSCHL     = TG_BSID_ADT-BSCHL.
    WA_SAIDA_0100-UMSKS     = TG_BSID_ADT-UMSKS.
    WA_SAIDA_0100-UMSKZ     = TG_BSID_ADT-UMSKZ.
    WA_SAIDA_0100-SHKZG     = TG_BSID_ADT-SHKZG.
    WA_SAIDA_0100-GSBER     = TG_BSID_ADT-GSBER.
    WA_SAIDA_0100-SGTXT     = TG_BSID_ADT-SGTXT.
    WA_SAIDA_0100-ZFBDT     = TG_BSID_ADT-ZFBDT.
    WA_SAIDA_0100-ZBD1T     = TG_BSID_ADT-ZBD1T.
    WA_SAIDA_0100-KIDNO     = TG_BSID_ADT-KIDNO.
    WA_SAIDA_0100-XREF1     = TG_BSID_ADT-XREF1.
    WA_SAIDA_0100-XREF3     = TG_BSID_ADT-XREF3.
    WA_SAIDA_0100-ZUONR     = TG_BSID_ADT-ZUONR.
    WA_SAIDA_0100-BLART     = TG_BSID_ADT-BLART.
    WA_SAIDA_0100-ZTERM     = TG_BSID_ADT-ZTERM.
    WA_SAIDA_0100-ANLN1     = TG_BSID_ADT-ANLN1.
    WA_SAIDA_0100-ANLN2     = TG_BSID_ADT-ANLN2.
    WA_SAIDA_0100-XBLNR     = TG_BKPF-XBLNR.
    WA_SAIDA_0100-OVPED     = TG_BSID_ADT-VBEL2.
    WA_SAIDA_0100-POSN2     = TG_BSID_ADT-POSN2.
    WA_SAIDA_0100-ITMOP     = TG_BSID_ADT-POSN2.
    WA_SAIDA_0100-DCSIM     = TG_BSID_ADT-DCSIM.
    WA_SAIDA_0100-COUNT     = 1.
    WA_SAIDA_0100-KOART     = 'D'.

    perform F_GET_TAXA using TG_BKPF
                             WA_SAIDA_0100-DMBTR
                             WA_SAIDA_0100-DMBE2
                    changing WA_SAIDA_0100-KURSF.

    perform F_GET_BSID_COMP tables TG_BSID_COMP_AUX
                             using TG_BSID_ADT-BUKRS
                                   TG_BSID_ADT-BELNR
                                   TG_BSID_ADT-BUZEI
                                   TG_BSID_ADT-GJAHR
                                   TG_BSID_ADT-KUNNR
                                   TG_BSID_ADT-VBEL2
                                   TG_BSID_ADT-SGTXT
                                   TG_BSID_ADT-ZUONR
                                   TG_BSID_ADT-ANLN1
                                   TG_BSID_ADT-ANLN2
                                   TG_BSID_ADT-DCSIM.

    loop at TG_BSID_COMP_AUX.
      add 1 to WA_SAIDA_0100-QTDE_FT.
      if ( TG_BSID_COMP_AUX-SHKZG ne WA_SAIDA_0100-SHKZG ). "Se for operação(Cred.Deb.) diferente da partida principal
*        ADD tg_bsid_comp_aux-dmbtr TO wa_saida_0100-ft_dmbtr.
        add TG_BSID_COMP_AUX-WRBTR to WA_SAIDA_0100-FT_DMBTR.
        add TG_BSID_COMP_AUX-DMBE2 to WA_SAIDA_0100-FT_DMBE2.
      else.
*        SUBTRACT tg_bsid_comp_aux-dmbtr FROM wa_saida_0100-ft_dmbtr.
        subtract TG_BSID_COMP_AUX-WRBTR from WA_SAIDA_0100-FT_DMBTR.
        subtract TG_BSID_COMP_AUX-DMBE2 from WA_SAIDA_0100-FT_DMBE2.
      endif.
    endloop.

    WA_SAIDA_0100-DF_DMBE2 = WA_SAIDA_0100-FT_DMBTR - WA_SAIDA_0100-WRBTR.

    WA_SAIDA_0100-COMP = ICON_LIGHT_OUT.

    if ( ( WA_SAIDA_0100-FT_DMBTR = WA_SAIDA_0100-WRBTR ) and "PBI 58318
         ( WA_SAIDA_0100-WAERS    = TG_T001-WAERS       ) )
          or
       ( ( WA_SAIDA_0100-FT_DMBE2 = WA_SAIDA_0100-DMBE2 ) and
         ( WA_SAIDA_0100-WAERS    = TG_T001-WAERS2      ) ).
      "Liberado Compensação ( Total Adiamento = Total Partidas Compensar )
      WA_SAIDA_0100-ST_COMP  = '1'.
      WA_SAIDA_0100-COMP     = ICON_EXECUTE_OBJECT.
    else."IF ( WA_SAIDA_0100-QTDE_FT > 0 ). "Ajuste p/ Compensar com Descontos/Juros 02.05.2018
      "Selecionar Partidas para compensar
      WA_SAIDA_0100-ST_COMP  = '2'.
      WA_SAIDA_0100-COMP     = ICON_SYSTEM_MARK.
    endif.

    WA_SAIDA_0100-VIEW_CP  = ICON_DISPLAY.
    WA_SAIDA_0100-VIEW_AD  = ICON_DISPLAY.

    append WA_SAIDA_0100 to IT_SAIDA_0100.
  endloop.

  if RB_PROD is not initial.
    refresh: TG_BSIK_PROD[].
    loop at TG_BSIK_ADT.
      move-corresponding TG_BSIK_ADT to TG_BSIK_PROD.
      clear TG_BSIK_PROD-BLART. "junta BLART
      TG_BSIK_PROD-QTDE_AD = 1.
      collect TG_BSIK_PROD.
    endloop.
    "Blart Unico
    loop at TG_BSIK_PROD.
      data(TABIX) = SY-TABIX.
      read table TG_BSIK_ADT with key BUKRS  = TG_BSIK_PROD-BUKRS
                                      LIFNR  = TG_BSIK_PROD-LIFNR
                                      BSCHL  = TG_BSIK_PROD-BSCHL
                                      GSBER  = TG_BSIK_PROD-GSBER
                                      ZUONR  = TG_BSIK_PROD-ZUONR
                                      WAERS  = TG_BSIK_PROD-WAERS
                                      XREF3  = TG_BSIK_PROD-XREF3
                                      XREF1  = TG_BSIK_PROD-XREF1
                                      HKONT  = TG_BSIK_PROD-HKONT.
      if SY-SUBRC = 0.
        TG_BSIK_PROD-BLART = TG_BSIK_ADT-BLART.
        modify TG_BSIK_PROD index TABIX transporting BLART.
      endif.
    endloop.
    TG_BSIK_COPY[] = TG_BSIK_ADT[].
    refresh TG_BSIK_ADT.
    clear TG_BSIK_ADT.
    loop at TG_BSIK_PROD.
      move-corresponding TG_BSIK_PROD to TG_BSIK_ADT.
      append TG_BSIK_ADT.
      clear TG_BSIK_ADT.
    endloop.
  endif.

  loop at TG_BSIK_ADT.
    clear: WA_SAIDA_0100, TG_LFA1, TG_EKKO.

    perform F_MOEDA_EMPRESA using TG_BSIK_ADT-BUKRS
                                  'X'.
    if ( SY-SUBRC ne 0 ).
      return.
    endif.

    read table TG_BKPF with key BUKRS = TG_BSIK_ADT-BUKRS
                                BELNR = TG_BSIK_ADT-BELNR
                                GJAHR = TG_BSIK_ADT-GJAHR.
    "CHECK sy-subrc = 0.
    if SY-SUBRC ne 0.
      TG_BKPF-BUKRS = TG_BSIK_ADT-BUKRS.                    "PBI 58318
    endif.

    read table TG_LFA1 with key LIFNR = TG_BSIK_ADT-LIFNR.
    check SY-SUBRC = 0.

    read table TG_EKKO with key EBELN = TG_BSIK_ADT-EBELN.
    if SY-SUBRC = 0.

    endif.

    WA_SAIDA_0100-BUKRS     = TG_BSIK_ADT-BUKRS.
    WA_SAIDA_0100-PARID     = TG_LFA1-LIFNR.
    WA_SAIDA_0100-NAME1     = TG_LFA1-NAME1.
    WA_SAIDA_0100-BELNR     = TG_BSIK_ADT-BELNR.
    WA_SAIDA_0100-BUZEI     = TG_BSIK_ADT-BUZEI.
    WA_SAIDA_0100-GJAHR     = TG_BSIK_ADT-GJAHR.
    WA_SAIDA_0100-BLDAT     = TG_BSIK_ADT-BLDAT.
    WA_SAIDA_0100-BUDAT     = TG_BSIK_ADT-BUDAT.
    WA_SAIDA_0100-WAERS     = TG_BSIK_ADT-WAERS.
    WA_SAIDA_0100-WRBTR     = TG_BSIK_ADT-WRBTR.
    WA_SAIDA_0100-DMBTR     = TG_BSIK_ADT-DMBTR.
    WA_SAIDA_0100-DMBE2     = TG_BSIK_ADT-DMBE2.
*    wa_saida_0100-dmbtr_aux = tg_bsik_adt-dmbtr.
    WA_SAIDA_0100-DMBTR_AUX = TG_BSIK_ADT-WRBTR.
    WA_SAIDA_0100-DMBE2_AUX = TG_BSIK_ADT-DMBE2.
    WA_SAIDA_0100-HKONT     = TG_BSIK_ADT-HKONT.
    WA_SAIDA_0100-BSCHL     = TG_BSIK_ADT-BSCHL.
    WA_SAIDA_0100-UMSKS     = TG_BSIK_ADT-UMSKS.
    WA_SAIDA_0100-UMSKZ     = TG_BSIK_ADT-UMSKZ.
    WA_SAIDA_0100-SHKZG     = TG_BSIK_ADT-SHKZG.
    WA_SAIDA_0100-GSBER     = TG_BSIK_ADT-GSBER.
    WA_SAIDA_0100-SGTXT     = TG_BSIK_ADT-SGTXT.
    WA_SAIDA_0100-ZFBDT     = TG_BSIK_ADT-ZFBDT.
    WA_SAIDA_0100-ZBD1T     = TG_BSIK_ADT-ZBD1T.
    WA_SAIDA_0100-KIDNO     = TG_BSIK_ADT-KIDNO.
    WA_SAIDA_0100-XREF1     = TG_BSIK_ADT-XREF1.
    WA_SAIDA_0100-XREF3     = TG_BSIK_ADT-XREF3.
    WA_SAIDA_0100-ZUONR     = TG_BSIK_ADT-ZUONR.
    WA_SAIDA_0100-BLART     = TG_BSIK_ADT-BLART.
    WA_SAIDA_0100-ZTERM     = TG_BSIK_ADT-ZTERM.
    WA_SAIDA_0100-ANLN1     = TG_BSIK_ADT-ANLN1.
    WA_SAIDA_0100-ANLN2     = TG_BSIK_ADT-ANLN2.
    WA_SAIDA_0100-XBLNR     = TG_BKPF-XBLNR.
    WA_SAIDA_0100-OVPED     = TG_BSIK_ADT-EBELN.
    WA_SAIDA_0100-EBELP     = TG_BSIK_ADT-EBELP.
    WA_SAIDA_0100-ITMOP     = TG_BSIK_ADT-EBELP.
    WA_SAIDA_0100-COUNT     = 1.
    WA_SAIDA_0100-QTDE_AD   = TG_BSIK_ADT-QTDE_AD.
    WA_SAIDA_0100-KOART     = 'K'.

    perform F_GET_TAXA using TG_BKPF
                             WA_SAIDA_0100-DMBTR
                             WA_SAIDA_0100-DMBE2
                    changing WA_SAIDA_0100-KURSF.

    perform F_GET_BSIK_COMP tables TG_BSIK_COMP_AUX
                             using TG_BSIK_ADT-BUKRS
                                   TG_BSIK_ADT-BELNR
                                   TG_BSIK_ADT-BUZEI
                                   TG_BSIK_ADT-GJAHR
                                   TG_BSIK_ADT-LIFNR
                                   TG_BSIK_ADT-EBELN
                                   TG_BSIK_ADT-SGTXT
                                   TG_BSIK_ADT-ZUONR
                                   TG_BSIK_ADT-ANLN1
                                   TG_BSIK_ADT-ANLN2.

    loop at TG_BSIK_COMP_AUX.
      add 1 to WA_SAIDA_0100-QTDE_FT.
      if ( TG_BSIK_COMP_AUX-SHKZG ne WA_SAIDA_0100-SHKZG ). "Se for operação(Cred.Deb.) diferente da partida principal
*          ADD tg_bsik_comp_aux-dmbtr TO wa_saida_0100-ft_dmbtr.
        add TG_BSIK_COMP_AUX-WRBTR to WA_SAIDA_0100-FT_DMBTR.
        add TG_BSIK_COMP_AUX-DMBE2 to WA_SAIDA_0100-FT_DMBE2.
      else.
*          SUBTRACT tg_bsik_comp_aux-dmbtr FROM wa_saida_0100-ft_dmbtr.
        subtract TG_BSIK_COMP_AUX-WRBTR from WA_SAIDA_0100-FT_DMBTR.
        subtract TG_BSIK_COMP_AUX-DMBE2 from WA_SAIDA_0100-FT_DMBE2.
      endif.
    endloop.

    WA_SAIDA_0100-COMP = ICON_LIGHT_OUT.
    "
    clear: WA_ZFIT0154, WA_SAIDA_0100-ST_COMP.
    if RB_PROD is not initial.
      select single *
        from LFA1
        into @data(W_LFA1)
        where LIFNR = @WA_SAIDA_0100-PARID
        and   VBUND = 'SOCIOS'.
      if SY-SUBRC = 0.
        select single *
                 into WA_ZFIT0154
                 from ZFIT0154
                 where TIPO = 'P'
                 and   FG_SOC = 'X'.
      else.
        select single *
           into WA_ZFIT0154
           from ZFIT0154
           where TIPO = 'P'
           and   FG_SOC = ''.
      endif.
      "
      VDIF_INT = ABS( WA_SAIDA_0100-FT_DMBTR - WA_SAIDA_0100-WRBTR ).
      if ( VDIF_INT le  WA_ZFIT0154-VLR_TOLER ) and ( WA_SAIDA_0100-WAERS    = TG_T001-WAERS ) .
        WA_SAIDA_0100-ST_COMP  = '3'.
*        wa_saida_0100-vlr_rsd  = wa_saida_0100-ft_dmbtr - wa_saida_0100-dmbtr.
        WA_SAIDA_0100-VLR_RSD  = WA_SAIDA_0100-FT_DMBTR - WA_SAIDA_0100-WRBTR.
        WA_SAIDA_0100-COMP     = ICON_EXECUTE_OBJECT.
      endif.

      VDIF_FOR = ABS( WA_SAIDA_0100-FT_DMBE2 - WA_SAIDA_0100-DMBE2 ).
      if ( VDIF_INT lt  WA_ZFIT0154-VLR_TOLER ) and ( WA_SAIDA_0100-WAERS    = TG_T001-WAERS2 ) .
        WA_SAIDA_0100-ST_COMP  = '3'.
        WA_SAIDA_0100-VLR_RSD  = WA_SAIDA_0100-FT_DMBE2 - WA_SAIDA_0100-DMBE2 .
        WA_SAIDA_0100-COMP     = ICON_EXECUTE_OBJECT.
      endif.
    endif.
    "
    WA_SAIDA_0100-DF_DMBE2 = WA_SAIDA_0100-FT_DMBTR - WA_SAIDA_0100-DMBTR.
    "
    if WA_SAIDA_0100-ST_COMP ne '3'.
      if ( ( WA_SAIDA_0100-FT_DMBTR = WA_SAIDA_0100-WRBTR ) and
           ( WA_SAIDA_0100-WAERS    = TG_T001-WAERS       ) )
            or
         ( ( WA_SAIDA_0100-FT_DMBE2 = WA_SAIDA_0100-DMBE2 ) and
           ( WA_SAIDA_0100-WAERS    = TG_T001-WAERS2      ) ).
        "Liberado Compensação ( Total Adiamento = Total Partidas Compensar )
        WA_SAIDA_0100-ST_COMP  = '1'.
        WA_SAIDA_0100-COMP      = ICON_EXECUTE_OBJECT.
      else."IF ( WA_SAIDA_0100-QTDE_FT > 0 ). "Ajuste p/ Compensar com Descontos/Juros 02.05.2018
        "Selecionar Partidas para compensar
        WA_SAIDA_0100-ST_COMP  = '2'.
        WA_SAIDA_0100-COMP      = ICON_SYSTEM_MARK.
      endif.
    endif.

    WA_SAIDA_0100-VIEW_CP  = ICON_DISPLAY.
    WA_SAIDA_0100-VIEW_AD  = ICON_DISPLAY.

    append WA_SAIDA_0100 to IT_SAIDA_0100.
  endloop.


endform.

form F_REFRESH_ALV using P_ALV.

  case P_ALV.
    when '0100'.
      if OBJ_ALV_0100 is not initial.
        call method OBJ_ALV_0100->REFRESH_TABLE_DISPLAY
          exporting
            IS_STABLE = WA_STABLE.
      endif.
    when '0110'.
      call method OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
        exporting
          IS_STABLE = WA_STABLE.
  endcase.

endform.

form F_REFRESH_OBJETOS .

  clear: GS_LAYOUT,
         GS_VARIANT.

  refresh: IT_EXCLUDE_FCODE.

  if RB_PROD = 'X'.
    if SY-DYNNR = '0100'.
      if OBJ_ALV_0100 is not initial.
        call method OBJ_ALV_0100->FREE.

        if OBJ_CONTAINER_0100 is not initial.
          call method OBJ_CONTAINER_0100->FREE.
        endif.
        free:  OBJ_ALV_0100,OBJ_TOOLBAR_0100.
        free: OBJ_CONTAINER_0100.
      endif.
      move '/PRODUTORES' to GS_VARIANT-VARIANT.
    elseif SY-DYNNR = '0110'.
      if OBJ_ALV_0110 is not initial.
        call method OBJ_ALV_0110->FREE.

        if OBJ_CONTAINER_0110 is not initial.
          call method OBJ_CONTAINER_0110->FREE.
        endif.
        free:  OBJ_ALV_0100,OBJ_TOOLBAR_0110.
        free: OBJ_CONTAINER_0110.
      endif.
      move '/PRODDETALHE' to GS_VARIANT-VARIANT.
    endif.
    "

  endif.

endform.

form F_CRIAR_CATALOG using P_SCREEN.

  free: WA_FCAT, IT_FCAT.

  case P_SCREEN.
    when '0100'.

      perform F_ESTRUTURA_ALV using:

       01  'KNA1'      'KUNNR'            'IT_SAIDA_0100' 'PARID'    'Cliente/Fornecedor'           '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'KNA1'      'NAME1'            'IT_SAIDA_0100' 'NAME1'    'Nome Cliente/Fornecedor'      '27'   ' '    ''  ' ' ' ' ' ' ' ' '' .

      if RB_PROD is not initial.
        perform F_ESTRUTURA_ALV using:
        03  ''          ''                 'IT_SAIDA_0100' 'ZUONR'    'Atribuição'                   '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        04  'BSID'      'WAERS'            'IT_SAIDA_0100' 'WAERS'    'Moeda'                        '05'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        05  'BSID'      'BSCHL'            'IT_SAIDA_0100' 'BSCHL'     'CL'                          '02'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        06  'BSID'      'WRBTR'            'IT_SAIDA_0100' 'DMBTR'    'Valor R$'                     '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        06  'BSID'      'WRBTR'            'IT_SAIDA_0100' 'WRBTR'    'Valor Doc'                    '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        07  'BSID'      'DMBE2'            'IT_SAIDA_0100' 'DMBE2'    'Valor U$'                     '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        08  'BSID'      'DMBTR'            'IT_SAIDA_0100' 'FT_DMBTR' 'Tot.Fat.Doc'                  '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        09  'BSID'      'DMBE2'            'IT_SAIDA_0100' 'DF_DMBE2' 'Dif.FatXVlr Doc'              '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        10  'BSID'      'DMBE2'            'IT_SAIDA_0100' 'FT_DMBE2' 'Tot.Fat.U$'                   '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        11  ''          ''                 'IT_SAIDA_0100' 'QTDE_AD'  'Qtde.Adt.'                    '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        11  ''          ''                 'IT_SAIDA_0100' 'QTDE_FT'  'Qtde.Fat.'                    '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        12  ''          ''                 'IT_SAIDA_0100' 'COMP'     'Compensar'                    '10'   ' '    ''  ' ' 'C' 'X' ' ' '' ,
        13  ''          ''                 'IT_SAIDA_0100' 'VIEW_CP'  'Ctr.Part.'                    '09'   ' '    ''  ' ' 'C' 'X' ' ' '' ,
        14  ''          ''                 'IT_SAIDA_0100' 'VIEW_AD'  'Partidas'                     '09'   ' '    ''  ' ' 'C' 'X' ' ' '' ,
        15  ''          ''                 'IT_SAIDA_0100' 'GSBER'    'DIV'                          '06'   ' '    ' ' ' ' ' ' ' ' ' ' '' ,
        16  ''          ''                 'IT_SAIDA_0100' 'XREF3'    'Ref 3'                        '20'   ' '    ' ' ' ' ' ' ' ' ' ' '' ,
        17  ''          ''                 'IT_SAIDA_0100' 'XREF1'    'Ref 1'                        '12'   ' '    ' ' ' ' ' ' ' ' ' ' '' ,
        18  ''          ''                 'IT_SAIDA_0100' 'HKONT'    'Razão'                        '12'   ' '    ' ' ' ' ' ' ' ' ' ' '' ,
        19  ''          ''                 'IT_SAIDA_0100' 'SGTXT'    'Texto Item'                   '15'   ' '    ''  ' ' ' ' ' ' ' ' '' .
      else.
        perform F_ESTRUTURA_ALV using:
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
      endif.

    when '0110'.

      perform F_ESTRUTURA_ALV using:

       01  ''          ''                 'IT_SAIDA_0110' 'IC_MANUAL'   'LM'                  '04'   ' '    ' '  ' ' 'C' ' ' ' ' ' ' ,
       01  ''          ''                 'IT_SAIDA_0110' 'CHECK'      'Check'                '05'   'X'    ' '  ' ' ' ' ' ' ' ' 'X' ,
       "01  'KNA1'      'KUNNR'            'IT_SAIDA_0110' 'KUNNR'      'Cliente'              '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       "02  'KNA1'      'NAME1'            'IT_SAIDA_0110' 'NAME1'      'Nome Cliente'         '20'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       03  'BSID'      'BELNR'            'IT_SAIDA_0110' 'BELNR'      'Doc.Ctb.'             '10'   ' '    ' '  ' ' ' ' 'X' ' ' ' ' ,
       04  'BSID'      'BLDAT'            'IT_SAIDA_0110' 'BLDAT'      'Dt.Doc.'              '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       05  'BSID'      'BUDAT'            'IT_SAIDA_0110' 'BUDAT'      'Dt.Lcto'              '10'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       05  'BSID'      'HKONT'            'IT_SAIDA_0110' 'HKONT'      'Conta'                '10'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       05  'BSID'      'BSCHL'            'IT_SAIDA_0110' 'BSCHL'      'CL'                   '02'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       06  'BSID'      'UMSKS'            'IT_SAIDA_0110' 'UMSKS'      'Cód.Razão Especial'   '01'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       06  'BSID'      'WAERS'            'IT_SAIDA_0110' 'WAERS'      'Moeda'                '05'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       07  'BSID'      'DMBTR'            'IT_SAIDA_0110' 'DMBTR_AUX'  'Valor Doc'            '12'   ' '    'X'  ' ' ' ' ' ' ' ' ' ' ,
       08  'BSID'      'DMBE2'            'IT_SAIDA_0110' 'DMBE2_AUX'  'Valor U$'             '12'   ' '    'X'  ' ' ' ' ' ' ' ' ' ' ,
       09  'BSID'      'DMBTR'            'IT_SAIDA_0110' 'VLR_RSD'    'Vlr.Residual'         '12'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       09  'BSID'      'DMBTR'            'IT_SAIDA_0110' 'DMBTR'      'Vlr.Comp.R$'          '12'   ' '    'X'  ' ' ' ' ' ' ' ' ' ' ,
       09  'BSID'      'DMBTR'            'IT_SAIDA_0110' 'WRBTR'      'Vlr.Comp.doc'         '12'   'X'    'X'  ' ' ' ' ' ' ' ' ' ' ,
       10  'BSID'      'DMBE2'            'IT_SAIDA_0110' 'DMBE2'      'Vlr.Comp.U$'          '12'   'X'    'X'  ' ' ' ' ' ' ' ' ' ' ,
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

      perform F_ESTRUTURA_ALV using:

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

form F_ESTRUTURA_ALV using value(P_COL_POS)       type I
                           value(P_REF_TABNAME)   like DD02D-TABNAME
                           value(P_REF_FIELDNAME) like DD03D-FIELDNAME
                           value(P_TABNAME)       like DD02D-TABNAME
                           value(P_FIELD)         like DD03D-FIELDNAME
                           value(P_SCRTEXT_L)     like DD03P-SCRTEXT_L
                           value(P_OUTPUTLEN)
                           value(P_EDIT)
                           value(P_SUM)
                           value(P_EMPHASIZE)
                           value(P_JUST)
                           value(P_HOTSPOT)
                           value(P_F4)
                           value(P_CHECK).

  clear WA_FCAT.

  WA_FCAT-FIELDNAME   = P_FIELD.
  WA_FCAT-TABNAME     = P_TABNAME.
  WA_FCAT-REF_TABLE   = P_REF_TABNAME.
  WA_FCAT-REF_FIELD   = P_REF_FIELDNAME.
  WA_FCAT-KEY         = ' '.
  WA_FCAT-EDIT        = P_EDIT.
  WA_FCAT-COL_POS     = P_COL_POS.
  WA_FCAT-OUTPUTLEN   = P_OUTPUTLEN.
  WA_FCAT-NO_OUT      = ' '.
  WA_FCAT-DO_SUM      = P_SUM.
  WA_FCAT-REPTEXT     = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_S   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_M   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_L   = P_SCRTEXT_L.
  WA_FCAT-EMPHASIZE   = P_EMPHASIZE.
  WA_FCAT-STYLE       =
  WA_FCAT-JUST        = P_JUST.
  WA_FCAT-HOTSPOT     = P_HOTSPOT.
  WA_FCAT-F4AVAILABL  = P_F4.
  WA_FCAT-CHECKBOX    = P_CHECK.

  append WA_FCAT to IT_FCAT.

endform.                    " ESTRUTURA_ALV

form F_EXCLUDE_FCODE using P_SCREEN.

  append CL_GUI_ALV_GRID=>MC_FC_REFRESH           to IT_EXCLUDE_FCODE.


  append CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    to IT_EXCLUDE_FCODE.
  append CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    to IT_EXCLUDE_FCODE.
  append CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    to IT_EXCLUDE_FCODE.
  append CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          to IT_EXCLUDE_FCODE.
  append CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      to IT_EXCLUDE_FCODE.
  append CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           to IT_EXCLUDE_FCODE.
  append CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          to IT_EXCLUDE_FCODE.
  append CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         to IT_EXCLUDE_FCODE.
  append CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW to IT_EXCLUDE_FCODE.
  append CL_GUI_ALV_GRID=>MC_FC_CHECK             to IT_EXCLUDE_FCODE.

endform.

form F_LIMPA_VARIAVEIS .

  clear: WA_SAIDA_0100,
         IT_SAIDA_0100[],
         WA_SAIDA_0110,
         IT_SAIDA_0110[],
         TG_BSID_ADT[],
         TG_BSID_COMP[],
         TG_VBAK[],
         TG_BSIK_ADT[],
         TG_BSIK_COMP[],
         TG_EKKO[],
         TG_KNA1[],
         TG_LFA1[],
         TG_BSEG[],
         TG_T001[],
         TG_EKKN[],
         TG_BSIS_CBANCO[],
         TG_ZSDT0041[],
         TG_ZSDT0090[],
         TG_VBFA_RD[],
         TG_TSPAT.

  clear: VG_NOT_FOUND.

endform.

form F_CONFIG_RANGES.

  clear: R_FDLEV_BANCO, R_FDLEV_BANCO[],
         R_BUKRS,       R_BUKRS[],
         R_PARID,       R_PARID[],
         R_OVPED,       R_OVPED[],
         R_AUGDT,       R_AUGDT[],
         R_SGTXT,       R_SGTXT[],
         R_ZUONR,       R_ZUONR[],
         R_DCSIM,       R_DCSIM[].

  R_BUKRS[] = P_BUKRS[].
  R_PARID[] = P_PARID[].
  R_OVPED[] = P_OVPED[].
  R_AUGDT[] = P_AUGDT[].
  R_SGTXT[] = P_SGTXT[].
  R_ZUONR[] = P_ZUONR[].
  R_DCSIM[] = P_DCSIM[].

  R_FDLEV_BANCO-SIGN   = 'I'.
  R_FDLEV_BANCO-OPTION = 'EQ'.
  R_FDLEV_BANCO-LOW    = 'F0'.
  append R_FDLEV_BANCO.

  R_FDLEV_BANCO-LOW    = 'B2'.
  append R_FDLEV_BANCO.

  "----------------------------------------------------
  " Empresa
  "----------------------------------------------------
*  IF P_BUKRS-LOW IS NOT INITIAL.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = P_BUKRS-LOW
*      IMPORTING
*        OUTPUT = P_BUKRS-LOW.
*
*    R_BUKRS-SIGN   = 'I'.
*    R_BUKRS-OPTION = 'EQ'.
*    R_BUKRS-LOW  = P_BUKRS-LOW.
*    R_BUKRS-HIGH = P_BUKRS-LOW.
*
*    IF ( P_BUKRS-HIGH IS NOT INITIAL ).
*      R_BUKRS-OPTION = 'BT'.
*      R_BUKRS-HIGH = P_BUKRS-HIGH .
*    ENDIF.
*
*    APPEND R_BUKRS.
*  ENDIF.

  "----------------------------------------------------
  " Cliente/Fornecedor
  "----------------------------------------------------
*  IF P_PARID-LOW IS NOT INITIAL.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = P_PARID-LOW
*      IMPORTING
*        OUTPUT = P_PARID-LOW.
*
*    R_PARID-SIGN   = 'I'.
*    R_PARID-OPTION = 'EQ'.
*    R_PARID-LOW  = P_PARID-LOW.
*    R_PARID-HIGH = P_PARID-LOW.
*
*    IF ( P_PARID-HIGH IS NOT INITIAL ).
*      R_PARID-OPTION = 'BT'.
*      R_PARID-HIGH = P_PARID-HIGH .
*    ENDIF.
*    APPEND R_PARID.
*  ENDIF.

  "----------------------------------------------------
  " O.V / Pedido
  "----------------------------------------------------
*  IF P_OVPED-LOW IS NOT INITIAL.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = P_OVPED-LOW
*      IMPORTING
*        OUTPUT = P_OVPED-LOW.
*
*    R_OVPED-SIGN   = 'I'.
*    R_OVPED-OPTION = 'EQ'.
*    R_OVPED-LOW  = P_OVPED-LOW.
*    R_OVPED-HIGH = P_OVPED-LOW.
*
*    IF ( P_OVPED-HIGH IS NOT INITIAL ).
*      R_OVPED-OPTION = 'BT'.
*      R_OVPED-HIGH = P_OVPED-HIGH .
*    ENDIF.
*    APPEND R_OVPED.
*  ENDIF.

  "----------------------------------------------------
  " Texto Item
  "----------------------------------------------------
*  IF P_SGTXT-LOW IS NOT INITIAL.
*    R_SGTXT-SIGN   = 'I'.
*    IF P_SGTXT-LOW CS '*'.
*      R_SGTXT-OPTION = 'CP'.
*    ELSE.
*      R_SGTXT-OPTION = 'EQ'.
*    ENDIF.
*    R_SGTXT-LOW  = P_SGTXT-LOW.
*    APPEND R_SGTXT.
*  ENDIF.

  "----------------------------------------------------
  " Atribuição
  "----------------------------------------------------
*  IF P_ZUONR-LOW IS NOT INITIAL.
*    R_ZUONR-SIGN   = 'I'.
*    IF P_ZUONR-LOW CS '*'.
*      R_ZUONR-OPTION = 'CP'.
*    ELSE.
*      R_ZUONR-OPTION = 'EQ'.
*    ENDIF.
*    R_ZUONR-LOW  = P_ZUONR-LOW.
*    APPEND R_ZUONR.
*  ENDIF.


  "----------------------------------------------------
  " Data Compensação.
  "----------------------------------------------------
*  IF P_AUGDT-LOW IS NOT INITIAL.
*    R_AUGDT-SIGN   = 'I'.
*    R_AUGDT-OPTION = 'EQ'.
*    R_AUGDT-LOW  = P_AUGDT-LOW.
*    R_AUGDT-HIGH = P_AUGDT-LOW.
*
*    IF ( P_AUGDT-HIGH IS NOT INITIAL ).
*      R_AUGDT-OPTION = 'BT'.
*      R_AUGDT-HIGH = P_AUGDT-HIGH .
*    ENDIF.
*    APPEND R_AUGDT.
*  ENDIF.



endform.

form F_HOTSPOT_CLICK  using  P_ALV
                             I_ROW_ID     type LVC_S_ROW
                             I_COLUMN_ID  type LVC_S_COL
                             IS_ROW_NO    type LVC_S_ROID.

  data: IT_RSPARAMS type table of RSPARAMS,
        WA_RSPARAMS type RSPARAMS.

  data: OPT         type CTU_PARAMS,
        VL_ERROR    type C,
        VL_DOC_COMP type BSAD-BELNR.


  case P_ALV.
    when '0100'.
      clear: WA_SAIDA_0100, WA_SAIDA_0110.
      case I_COLUMN_ID.
        when 'BELNR'.
          read table IT_SAIDA_0100 into WA_SAIDA_0100 index I_ROW_ID.
          check ( SY-SUBRC = 0 ) and ( WA_SAIDA_0100-BELNR is not initial ).

          set parameter id 'BLN' field WA_SAIDA_0100-BELNR.
          set parameter id 'BUK' field WA_SAIDA_0100-BUKRS.
          set parameter id 'GJR' field WA_SAIDA_0100-BUDAT(4).

          call transaction 'FB03' and skip first screen.

        when 'COMP'.

          if P_BLT_CP is initial.
            message 'Informe um Tp.Documento para Compensação!' type 'S'.
            exit.
          endif.

          read table IT_SAIDA_0100 into WA_SAIDA_0100 index I_ROW_ID.
          check ( SY-SUBRC = 0 ).

          case WA_SAIDA_0100-ST_COMP.
            when '1' or '3' . "Liberado Compensação ( Total Adiamento = Total Partidas Compensar )
              perform F_GET_PART_COMP using WA_SAIDA_0100.
              perform F_BAPI_F51 using ABAP_FALSE
                              changing WA_SAIDA_0100
                                       VL_ERROR
                                       VL_DOC_COMP.
              perform F_RENOVAR_CONS.
            when '2'. "Selecionar Partidas para compensar
              perform F_GET_PART_COMP using WA_SAIDA_0100.
              "CHECK IT_SAIDA_0110[] IS NOT INITIAL. "Ajuste p/ Compensar com Descontos/Juros 02.05.2018
              call screen 0110 starting at 01 01 ending at 250 30 .
              perform F_RENOVAR_CONS.
          endcase.
        when 'VIEW_AD'.

          read table IT_SAIDA_0100 into WA_SAIDA_0100 index I_ROW_ID.
          check ( SY-SUBRC = 0 ).
          refresh IT_DISPLAY.
          loop at TG_BSIK_COPY where LIFNR = WA_SAIDA_0100-PARID
                               and   ZUONR = WA_SAIDA_0100-ZUONR.
            move-corresponding TG_BSIK_COPY to WA_DISPLAY.
            append WA_DISPLAY to IT_DISPLAY.
          endloop.
          call method CL_SALV_TABLE=>FACTORY
            importing
              R_SALV_TABLE = GR_TABLE
            changing
              T_TABLE      = IT_DISPLAY.


          GR_TABLE->SET_SCREEN_POPUP(
                 START_COLUMN = 10
                 END_COLUMN  = 150
                 START_LINE  = 1
                 END_LINE    = 6 ).

          call method GR_TABLE->DISPLAY.

        when 'VIEW_CP'.

          if P_BLT_CP is initial.
            message 'Informe um Tp.Documento para Compensação!' type 'S'.
            exit.
          endif.

          read table IT_SAIDA_0100 into WA_SAIDA_0100 index I_ROW_ID.
          check ( SY-SUBRC = 0 ).

          WA_SAIDA_0100-ST_COMP = '2'.

          perform F_GET_PART_COMP using WA_SAIDA_0100.
          check IT_SAIDA_0110[] is not initial.
          call screen 0110 starting at 01 01 ending at 250 30 .
          perform F_RENOVAR_CONS.

      endcase.
    when '0110'.
      case I_COLUMN_ID.
        when 'BELNR'.
          read table IT_SAIDA_0110 into WA_SAIDA_0110 index I_ROW_ID.
          check ( SY-SUBRC = 0 ) and ( WA_SAIDA_0110-BELNR is not initial ).

          set parameter id 'BLN' field WA_SAIDA_0110-BELNR.
          set parameter id 'BUK' field WA_SAIDA_0110-BUKRS.
          set parameter id 'GJR' field WA_SAIDA_0110-BUDAT(4).

          call transaction 'FB03' and skip first screen.
      endcase.
    when '0120'.
      case I_COLUMN_ID.
        when 'ST_CTB'.
          clear: TG_ZIB_ERR[], WA_SAIDA_0120.
          read table IT_SAIDA_0120 into WA_SAIDA_0120 index I_ROW_ID.

          check ( SY-SUBRC = 0 ) and ( WA_SAIDA_0120-OBJ_KEY is not initial ).

          select distinct *
            from ZIB_CONTABIL_ERR as A into corresponding fields of table TG_ZIB_ERR
           where OBJ_KEY  eq WA_SAIDA_0120-OBJ_KEY.

          check TG_ZIB_ERR[] is not initial.

          perform F_MONTAR_LAYOUT_LOG_ERRO.

          call function 'REUSE_ALV_GRID_DISPLAY'
            exporting
              IT_FIELDCAT           = ESTRUTURA[]
              I_SAVE                = 'A'
              I_SCREEN_START_COLUMN = 3
              I_SCREEN_START_LINE   = 3
              I_SCREEN_END_COLUMN   = 100
              I_SCREEN_END_LINE     = 13
            tables
              T_OUTTAB              = TG_ZIB_ERR.
        when 'AUGBL'.
          read table IT_SAIDA_0120 into WA_SAIDA_0120 index I_ROW_ID.
          check ( SY-SUBRC = 0 ) and ( WA_SAIDA_0120-AUGBL is not initial ).

          set parameter id 'BLN' field WA_SAIDA_0120-AUGBL.
          set parameter id 'BUK' field WA_SAIDA_0120-BUKRS.
          set parameter id 'GJR' field WA_SAIDA_0120-AUGDT(4).

          call transaction 'FB03' and skip first screen.
        when 'BELNR_GER'.
          read table IT_SAIDA_0120 into WA_SAIDA_0120 index I_ROW_ID.
          check ( SY-SUBRC = 0 ) and ( WA_SAIDA_0120-BELNR_GER is not initial ).

          set parameter id 'BLN' field WA_SAIDA_0120-BELNR_GER.
          set parameter id 'BUK' field WA_SAIDA_0120-BUKRS.
          set parameter id 'GJR' field WA_SAIDA_0120-GJAHR_GER.

          call transaction 'FB03' and skip first screen.
        when 'STBLG_GER'.
          read table IT_SAIDA_0120 into WA_SAIDA_0120 index I_ROW_ID.
          check ( SY-SUBRC = 0 ) and ( WA_SAIDA_0120-STBLG_GER is not initial ).

          set parameter id 'BLN' field WA_SAIDA_0120-STBLG_GER.
          set parameter id 'BUK' field WA_SAIDA_0120-BUKRS.
          set parameter id 'GJR' field WA_SAIDA_0120-GJAHR_GER.

          call transaction 'FB03' and skip first screen.
      endcase.
  endcase.


endform.

form F_GET_PART_COMP  using  P_SAIDA_0100 type TY_SAIDA_0100.

  clear: IT_SAIDA_0110[].

  check P_SAIDA_0100-KURSF ne 0.

  case P_SAIDA_0100-KOART.
    when 'D'. "Cliente

*--------------------------------------------------------------------*
*  Carrega Partidas Compensar Cliente
*--------------------------------------------------------------------*

      perform F_GET_BSID_COMP tables TG_BSID_COMP_AUX
                               using P_SAIDA_0100-BUKRS
                                     P_SAIDA_0100-BELNR
                                     P_SAIDA_0100-BUZEI
                                     P_SAIDA_0100-GJAHR
                                     P_SAIDA_0100-PARID
                                     P_SAIDA_0100-OVPED
                                     P_SAIDA_0100-SGTXT
                                     P_SAIDA_0100-ZUONR
                                     P_SAIDA_0100-ANLN1
                                     P_SAIDA_0100-ANLN2
                                     P_SAIDA_0100-DCSIM.

      loop at TG_BSID_COMP_AUX into TG_BSID_COMP.

        clear: WA_SAIDA_0110, TG_KNA1, GT_ESTILO[].

        perform F_MOEDA_EMPRESA using TG_BSID_COMP-BUKRS
                                      'X'.
        if ( SY-SUBRC ne 0 ).
          return.
        endif.

        read table TG_BKPF with key BUKRS = TG_BSID_COMP-BUKRS
                                    BELNR = TG_BSID_COMP-BELNR
                                    GJAHR = TG_BSID_COMP-GJAHR.
        check SY-SUBRC = 0.

        read table TG_KNA1 with key KUNNR = TG_BSID_COMP-KUNNR.
        check SY-SUBRC = 0.

        WA_SAIDA_0110-BUKRS     = TG_BSID_COMP-BUKRS.
        WA_SAIDA_0110-PARID     = TG_KNA1-KUNNR.
        WA_SAIDA_0110-NAME1     = TG_KNA1-NAME1.
        WA_SAIDA_0110-BELNR     = TG_BSID_COMP-BELNR.
        WA_SAIDA_0110-BUZEI     = TG_BSID_COMP-BUZEI.
        WA_SAIDA_0110-GJAHR     = TG_BSID_COMP-GJAHR.
        WA_SAIDA_0110-BLDAT     = TG_BSID_COMP-BLDAT.
        WA_SAIDA_0110-BUDAT     = TG_BSID_COMP-BUDAT.
        WA_SAIDA_0110-WAERS     = TG_BSID_COMP-WAERS.
        WA_SAIDA_0110-WRBTR     = TG_BSID_COMP-WRBTR.
        WA_SAIDA_0110-DMBTR     = TG_BSID_COMP-DMBTR.
        WA_SAIDA_0110-DMBE2     = TG_BSID_COMP-DMBE2.
*        wa_saida_0110-dmbtr_aux = tg_bsid_comp-dmbtr.
        WA_SAIDA_0110-DMBTR_AUX = TG_BSID_COMP-WRBTR.
        WA_SAIDA_0110-DMBE2_AUX = TG_BSID_COMP-DMBE2.
        WA_SAIDA_0110-HKONT     = TG_BSID_COMP-HKONT.
        WA_SAIDA_0110-BSCHL     = TG_BSID_COMP-BSCHL.
        WA_SAIDA_0110-UMSKS     = TG_BSID_COMP-UMSKS.
        WA_SAIDA_0110-UMSKZ     = TG_BSID_COMP-UMSKZ.
        WA_SAIDA_0110-GSBER     = TG_BSID_COMP-GSBER.
        WA_SAIDA_0110-SGTXT     = TG_BSID_COMP-SGTXT.
        WA_SAIDA_0110-ZFBDT     = TG_BSID_COMP-ZFBDT.
        WA_SAIDA_0110-ZBD1T     = TG_BSID_COMP-ZBD1T.
        WA_SAIDA_0110-OVPED     = TG_BSID_COMP-VBEL2.
        WA_SAIDA_0110-POSN2     = TG_BSID_COMP-POSN2.
        WA_SAIDA_0110-ITMOP     = TG_BSID_COMP-POSN2.
        WA_SAIDA_0110-DCSIM     = TG_BSID_COMP-DCSIM.
        WA_SAIDA_0110-KIDNO     = TG_BSID_COMP-KIDNO.
        WA_SAIDA_0110-KIDNO     = TG_BSID_COMP-KIDNO.
        WA_SAIDA_0110-XREF1     = TG_BSID_COMP-XREF1.
        WA_SAIDA_0110-XREF3     = TG_BSID_COMP-XREF3.
        WA_SAIDA_0110-BLART     = TG_BSID_COMP-BLART.
        WA_SAIDA_0110-ZUONR     = TG_BSID_COMP-ZUONR.
        WA_SAIDA_0110-ZTERM     = TG_BSID_COMP-ZTERM.
        WA_SAIDA_0110-ANLN1     = TG_BSID_COMP-ANLN1.
        WA_SAIDA_0110-ANLN2     = TG_BSID_COMP-ANLN2.
        WA_SAIDA_0110-XBLNR     = TG_BKPF-XBLNR.
        WA_SAIDA_0110-KOART     = P_SAIDA_0100-KOART.
        WA_SAIDA_0110-SGTXT_RSD = 'Desmembramento Fatura'.
        WA_SAIDA_0110-SGTXT_RSD = WA_SAIDA_0110-SGTXT.

        perform F_GET_TAXA using TG_BKPF
                                 WA_SAIDA_0110-DMBTR
                                 WA_SAIDA_0110-DMBE2
                        changing WA_SAIDA_0110-KURSF.

        if WA_SAIDA_0100-ST_COMP = '1'. "Total Partidas = Valor Adiantamento.
          WA_SAIDA_0110-CHECK = 'X'.
        endif.

        if P_SAIDA_0100-WAERS = TG_T001-WAERS.
          WA_SAIDA_0110-DMBE2 = WA_SAIDA_0110-DMBTR / WA_SAIDA_0110-KURSF.
        else.
          WA_SAIDA_0110-DMBTR = WA_SAIDA_0110-DMBE2 * WA_SAIDA_0110-KURSF.
        endif.

        WL_ESTILO-FIELDNAME    = 'BSCHL'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.

        WL_ESTILO-FIELDNAME    = 'DMBE2'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.

        WL_ESTILO-FIELDNAME    = 'DMBTR'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.

        WL_ESTILO-FIELDNAME    = 'HKONT'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.

        if WA_SAIDA_0110-MANUAL is initial.
          WL_ESTILO-FIELDNAME    = 'PART_PRINC'.
          WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
          append WL_ESTILO to GT_ESTILO.
        endif.

        WL_ESTILO-FIELDNAME    = 'UMSKS'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.


        WL_ESTILO-FIELDNAME    = 'WRBTR'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.

        WL_ESTILO-FIELDNAME    = 'ZFBDT'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.

        insert lines of GT_ESTILO into table WA_SAIDA_0110-ESTILO.

        append WA_SAIDA_0110 to IT_SAIDA_0110.

      endloop.

    when 'K'. "Fornecedor

*--------------------------------------------------------------------*
*  Carrega Partidas Compensar Fornecedor
*--------------------------------------------------------------------*

      perform F_GET_BSIK_COMP tables TG_BSIK_COMP_AUX
                               using P_SAIDA_0100-BUKRS
                                     P_SAIDA_0100-BELNR
                                     P_SAIDA_0100-BUZEI
                                     P_SAIDA_0100-GJAHR
                                     P_SAIDA_0100-PARID
                                     P_SAIDA_0100-OVPED
                                     P_SAIDA_0100-SGTXT
                                     P_SAIDA_0100-ZUONR
                                     P_SAIDA_0100-ANLN1
                                     P_SAIDA_0100-ANLN2.

      loop at TG_BSIK_COMP_AUX into TG_BSIK_COMP.

        clear: WA_SAIDA_0110, TG_LFA1, GT_ESTILO[].

        perform F_MOEDA_EMPRESA using TG_BSIK_COMP-BUKRS
                                      'X'.
        if ( SY-SUBRC ne 0 ).
          return.
        endif.

        read table TG_BKPF with key BUKRS = TG_BSIK_COMP-BUKRS
                                    BELNR = TG_BSIK_COMP-BELNR
                                    GJAHR = TG_BSIK_COMP-GJAHR.
*        CHECK sy-subrc = 0.

        read table TG_LFA1 with key LIFNR = TG_BSIK_COMP-LIFNR.

        check SY-SUBRC = 0.

        WA_SAIDA_0110-BUKRS     = TG_BSIK_COMP-BUKRS.
        WA_SAIDA_0110-PARID     = TG_LFA1-LIFNR.
        WA_SAIDA_0110-NAME1     = TG_LFA1-NAME1.
        WA_SAIDA_0110-BELNR     = TG_BSIK_COMP-BELNR.
        WA_SAIDA_0110-BUZEI     = TG_BSIK_COMP-BUZEI.
        WA_SAIDA_0110-GJAHR     = TG_BSIK_COMP-GJAHR.
        WA_SAIDA_0110-BLDAT     = TG_BSIK_COMP-BLDAT.
        WA_SAIDA_0110-BUDAT     = TG_BSIK_COMP-BUDAT.
        WA_SAIDA_0110-WAERS     = TG_BSIK_COMP-WAERS.
        WA_SAIDA_0110-WRBTR     = TG_BSIK_COMP-WRBTR.
        WA_SAIDA_0110-DMBTR     = TG_BSIK_COMP-DMBTR.
        WA_SAIDA_0110-DMBE2     = TG_BSIK_COMP-DMBE2.
*        wa_saida_0110-dmbtr_aux = tg_bsik_comp-dmbtr.
        WA_SAIDA_0110-DMBTR_AUX = TG_BSIK_COMP-WRBTR.
        WA_SAIDA_0110-DMBE2_AUX = TG_BSIK_COMP-DMBE2.
        WA_SAIDA_0110-HKONT     = TG_BSIK_COMP-HKONT.
        WA_SAIDA_0110-BSCHL     = TG_BSIK_COMP-BSCHL.
        WA_SAIDA_0110-UMSKS     = TG_BSIK_COMP-UMSKS.
        WA_SAIDA_0110-UMSKZ     = TG_BSIK_COMP-UMSKZ.
        WA_SAIDA_0110-GSBER     = TG_BSIK_COMP-GSBER.
        WA_SAIDA_0110-SGTXT     = TG_BSIK_COMP-SGTXT.
        WA_SAIDA_0110-ZFBDT     = TG_BSIK_COMP-ZFBDT.
        WA_SAIDA_0110-ZBD1T     = TG_BSIK_COMP-ZBD1T.
        WA_SAIDA_0110-OVPED     = TG_BSIK_COMP-EBELN.
        WA_SAIDA_0110-EBELP     = TG_BSIK_COMP-EBELP.
        WA_SAIDA_0110-ITMOP     = TG_BSIK_COMP-EBELP.
        WA_SAIDA_0110-KIDNO     = TG_BSIK_COMP-KIDNO.
        WA_SAIDA_0110-XREF1     = TG_BSIK_COMP-XREF1.
        WA_SAIDA_0110-XREF3     = TG_BSIK_COMP-XREF3.
        WA_SAIDA_0110-BLART     = TG_BSIK_COMP-BLART.
        WA_SAIDA_0110-ZUONR     = TG_BSIK_COMP-ZUONR.
        WA_SAIDA_0110-ZTERM     = TG_BSIK_COMP-ZTERM.
        WA_SAIDA_0110-ANLN1     = TG_BSIK_COMP-ANLN1.
        WA_SAIDA_0110-ANLN2     = TG_BSIK_COMP-ANLN2.
        WA_SAIDA_0110-XBLNR     = TG_BKPF-XBLNR.
        WA_SAIDA_0110-KOART     = P_SAIDA_0100-KOART.
        WA_SAIDA_0110-SGTXT_RSD = 'Desmembramento Fatura'.
        WA_SAIDA_0110-SGTXT_RSD = WA_SAIDA_0110-SGTXT.

        perform F_GET_TAXA using TG_BKPF
                                 WA_SAIDA_0110-DMBTR
                                 WA_SAIDA_0110-DMBE2
                        changing WA_SAIDA_0110-KURSF.

        if WA_SAIDA_0100-ST_COMP = '1'. "Total Partidas = Valor Adiantamento.
          WA_SAIDA_0110-CHECK = 'X'.
        endif.

        if P_SAIDA_0100-WAERS = TG_T001-WAERS.
          WA_SAIDA_0110-DMBE2 = WA_SAIDA_0110-DMBTR / WA_SAIDA_0110-KURSF.
        else.
          WA_SAIDA_0110-DMBTR = WA_SAIDA_0110-DMBE2 * WA_SAIDA_0110-KURSF.
        endif.

        WL_ESTILO-FIELDNAME    = 'BSCHL'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.

        WL_ESTILO-FIELDNAME    = 'DMBE2'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.

        WL_ESTILO-FIELDNAME    = 'DMBTR'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.

        WL_ESTILO-FIELDNAME    = 'HKONT'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.

        if WA_SAIDA_0110-MANUAL is initial.
          WL_ESTILO-FIELDNAME    = 'PART_PRINC'.
          WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
          append WL_ESTILO to GT_ESTILO.
        endif.

        WL_ESTILO-FIELDNAME    = 'UMSKS'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.

        WL_ESTILO-FIELDNAME    = 'WRBTR'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.

        WL_ESTILO-FIELDNAME    = 'ZFBDT'.
        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        append WL_ESTILO to GT_ESTILO.

        insert lines of GT_ESTILO into table WA_SAIDA_0110-ESTILO.

        append WA_SAIDA_0110 to IT_SAIDA_0110.

      endloop.

  endcase.

  clear: WA_CABECALHO_0110.
*  wa_cabecalho_0110-adt_dmbtr = p_saida_0100-dmbtr.
  WA_CABECALHO_0110-ADT_DMBTR = P_SAIDA_0100-WRBTR.
  WA_CABECALHO_0110-ADT_DMBE2 = P_SAIDA_0100-DMBE2.
  WA_CABECALHO_0110-WAERS     = P_SAIDA_0100-WAERS.
  if RB_PROD is initial.
    WA_CABECALHO_0110-SGTXT_RSD = 'Desmembramento Adiantamento'.
  else.
    WA_CABECALHO_0110-SGTXT_RSD = P_SAIDA_0100-SGTXT.
  endif.

  if P_SAIDA_0100-KOART = 'D'.
    read table TG_KNA1 with key KUNNR = P_SAIDA_0100-PARID.
    if SY-SUBRC = 0.
      concatenate P_SAIDA_0100-PARID '-' TG_KNA1-NAME1
             into WA_CABECALHO_0110-DS_CLIENTE separated by SPACE.
    endif.
  else.
    read table TG_LFA1 with key LIFNR = P_SAIDA_0100-PARID.
    if SY-SUBRC = 0.
      concatenate P_SAIDA_0100-PARID '-' TG_LFA1-NAME1
             into WA_CABECALHO_0110-DS_CLIENTE separated by SPACE.
    endif.
  endif.

endform.

form F_BAPI_F51 using P_RESIDUAL_COMP  type C  "Opção para deixar residual na Compensação
             changing P_SAIDA_0100     type TY_SAIDA_0100
                      P_ERRO
                      P_DOC_COMP       type BSAD-BELNR.

*  DATA: BEGIN OF wa_retorno,
*          mandt        LIKE zgl002_comp_f44-mandt,
*          bukrs        LIKE zgl002_comp_f44-bukrs,
*          lote         LIKE zgl002_comp_f44-lote,
*          status       LIKE zgl002_comp_f44-status,
*          sgtxt        LIKE zgl002_comp_f44-sgtxt,
*          data         LIKE zgl002_comp_f44-data,
*          hora         LIKE zgl002_comp_f44-hora,
*          data_comp    LIKE zgl002_comp_f44-data,
*          interface(1) TYPE c,
*        END OF wa_retorno.
*
*  DATA: it_retorno_rfc     LIKE STANDARD TABLE OF wa_retorno.

  data: IT_RETORNO_RFC like ZGL002_COMP_F44 occurs 0 with header line,
        WA_RETORNO     like line of IT_RETORNO_RFC.

  data: WL_0122      type ZFIT0122,
        VL_NAME1     type KNA1-NAME1,
        IT_0110_COMP type table of TY_SAIDA_0110.

  data: L_AUGLV   type T041A-AUGLV   value 'UMBUCHNG', "Posting with Clearing
        L_TCODE   type SY-TCODE      value 'FB05',     "You get an error with any other value
        L_SGFUNCT type RFIPI-SGFUNCT value 'C'.        "Post immediately

  data: LT_BLNTAB  type standard table of BLNTAB  with header line,
        LT_FTCLEAR type standard table of FTCLEAR with header line,
        LT_FTPOST  type standard table of FTPOST  with header line,
        LT_FTTAX   type standard table of FTTAX   with header line,
        LDS_RETURN type BAPIRET2.

  data: WA_SAI_0110_TMP type TY_SAIDA_0110.

  data: VDATA(10),
        VDATA_VENC(10),
        CNUM_SEQ(2),
        WL_VLR(16),
        WL_TAXA(16),
        WL_VLRC(16),
        WL_VLRN        type P decimals 2,
        VCAMPO(15),
        V_KUR          type BKPF-KURSF,
        VVALOR_BAX     type ZFIT0042-DMBE2,
        MSG_NO         type T100-MSGNR,
        MSG_TEXT       type STRING,
        P_MODE         like RFPDO-ALLGAZMD,
        VL_DT_MOV      type SY-DATUM,
        COUNT_FT       type FTPOST-COUNT,
        V_XSIMU        type CHAR1.

  clear: P_DOC_COMP.

  VL_DT_MOV = P_AUGDT-LOW.

  P_MODE = 'N'.

  call function 'POSTING_INTERFACE_START'
    exporting
      I_CLIENT           = SY-MANDT
      I_FUNCTION         = 'C'
      I_MODE             = P_MODE
      I_UPDATE           = 'S'
      I_USER             = SY-UNAME
    exceptions
      CLIENT_INCORRECT   = 1
      FUNCTION_INVALID   = 2
      GROUP_NAME_MISSING = 3
      MODE_INVALID       = 4
      UPDATE_INVALID     = 5
      others             = 6.

  if SY-SUBRC ne 0.
    P_ERRO = 'X'.
    rollback work.
    message 'Houve ao efeturar a compensação' type 'S'.
    return.
  endif.

  concatenate  VL_DT_MOV+6(2) VL_DT_MOV+4(2) VL_DT_MOV(4) into VDATA separated by '.'.

  if P_KSFCP > 0.
    write: P_KSFCP to WL_TAXA.
  else.
    write: P_SAIDA_0100-KURSF to WL_TAXA.
  endif.

  condense WL_TAXA no-gaps.

  clear: LT_BLNTAB,   LT_BLNTAB[],
         LT_FTCLEAR,  LT_FTCLEAR[],
         LT_FTPOST,   LT_FTPOST[],
         LT_FTTAX,    LT_FTTAX[],
         IT_0110_COMP[],
         LDS_RETURN, P_ERRO.

  COUNT_FT = 1.

  LT_FTPOST-STYPE = 'K'."Header
  LT_FTPOST-COUNT = COUNT_FT.  "number of Dynpro

  LT_FTPOST-FNAM = 'BKPF-BUKRS'.
  LT_FTPOST-FVAL = P_SAIDA_0100-BUKRS.
  append LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-WAERS'.
  LT_FTPOST-FVAL = P_SAIDA_0100-WAERS.
  append LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-KURSF'.
  LT_FTPOST-FVAL = WL_TAXA.
  append LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-BLDAT'.
  LT_FTPOST-FVAL = VDATA.
  append LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-BUDAT'.
  LT_FTPOST-FVAL = VDATA.
  append LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-MONAT'.
  LT_FTPOST-FVAL =  VL_DT_MOV+4(2).
  append LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-BLART'.
  LT_FTPOST-FVAL = P_BLT_CP.
  append LT_FTPOST.

  IT_0110_COMP[] = IT_SAIDA_0110[].
  sort IT_0110_COMP by BELNR BUZEI.
  delete adjacent duplicates from IT_0110_COMP comparing BELNR BUZEI.
  if P_SAIDA_0100-ST_COMP  = '3'.
    loop at IT_0110_COMP into WA_SAIDA_0110 where MANUAL is initial.
      WA_SAIDA_0110-CHECK = 'X'.
      modify IT_0110_COMP from WA_SAIDA_0110 index SY-TABIX transporting CHECK.
    endloop.
  endif.
  loop at IT_0110_COMP into WA_SAIDA_0110 where MANUAL is initial
                                            and CHECK  is not initial.

    LT_FTCLEAR-AGKOA  = WA_SAIDA_0110-KOART.
    LT_FTCLEAR-AGKON  = WA_SAIDA_0110-PARID.
    LT_FTCLEAR-AGUMS  = WA_SAIDA_0110-UMSKZ.
    LT_FTCLEAR-AGBUK  = WA_SAIDA_0110-BUKRS.
    LT_FTCLEAR-XNOPS  = 'X'.
    LT_FTCLEAR-SELFD  = 'BELNR'.
    concatenate WA_SAIDA_0110-BELNR WA_SAIDA_0110-BUDAT(4) WA_SAIDA_0110-BUZEI into LT_FTCLEAR-SELVON.
    append LT_FTCLEAR.

    "Opção para deixar residual na Compensação
    if ( P_RESIDUAL_COMP is not initial ) and ( WA_SAIDA_0110-ANLN1 is not initial ) and ( WA_SAIDA_0110-VLR_RSD > 0 ).
      clear: WA_SAI_0110_TMP.
      move-corresponding WA_SAIDA_0110 to WA_SAI_0110_TMP.

      perform F_ADD_PART_RESIDUAL tables LT_FTPOST
                                   using WA_SAI_0110_TMP
                                changing COUNT_FT
                                         P_ERRO.
      if P_ERRO is not initial.
        return.
      endif.
    endif.
  endloop.

  "Adiantamento
  if RB_PROD is initial or P_SAIDA_0100-BELNR is not initial.
    LT_FTCLEAR-AGKOA  = P_SAIDA_0100-KOART.
    LT_FTCLEAR-AGKON  = P_SAIDA_0100-PARID.
    LT_FTCLEAR-AGUMS  = P_SAIDA_0100-UMSKZ.
    LT_FTCLEAR-AGBUK  = P_SAIDA_0100-BUKRS.
    LT_FTCLEAR-XNOPS  = 'X'.
    LT_FTCLEAR-SELFD  = 'BELNR'.
    concatenate P_SAIDA_0100-BELNR P_SAIDA_0100-BUDAT(4) P_SAIDA_0100-BUZEI into LT_FTCLEAR-SELVON.
    append LT_FTCLEAR.
  else.                                                     "PBI 58318
    loop at TG_BSIK_COPY where LIFNR = P_SAIDA_0100-PARID
                          and  ZUONR = P_SAIDA_0100-ZUONR.
      LT_FTCLEAR-AGKOA  = P_SAIDA_0100-KOART.
      LT_FTCLEAR-AGKON  = P_SAIDA_0100-PARID.
      LT_FTCLEAR-AGUMS  = TG_BSIK_COPY-UMSKZ.
      LT_FTCLEAR-AGBUK  = P_SAIDA_0100-BUKRS.
      LT_FTCLEAR-XNOPS  = 'X'.
      LT_FTCLEAR-SELFD  = 'BELNR'.
      concatenate TG_BSIK_COPY-BELNR TG_BSIK_COPY-BUDAT(4) TG_BSIK_COPY-BUZEI into LT_FTCLEAR-SELVON.
      append LT_FTCLEAR.
    endloop.
  endif.

  if P_SAIDA_0100-ST_COMP  = '3' and P_SAIDA_0100-VLR_RSD ne 0.
    clear: WA_SAI_0110_TMP.
    move-corresponding P_SAIDA_0100 to WA_SAI_0110_TMP.
    WA_SAI_0110_TMP-VLR_RSD  = 0.
    WA_SAI_0110_TMP-VLR_RSDP = P_SAIDA_0100-VLR_RSD.

    perform F_ADD_PART_RESIDUAL tables LT_FTPOST
                                 using WA_SAI_0110_TMP
                              changing COUNT_FT
                                       P_ERRO.
    if P_ERRO is not initial.
      return.
    endif.
  endif.

  "Opção para deixar residual na Compensação
  if ( P_RESIDUAL_COMP is not initial ) and ( P_SAIDA_0100-ANLN1 is not initial ) and ( P_SAIDA_0100-VLR_RSD > 0 ).
    clear: WA_SAI_0110_TMP.
    move-corresponding P_SAIDA_0100 to WA_SAI_0110_TMP.

    perform F_ADD_PART_RESIDUAL tables LT_FTPOST
                                 using WA_SAI_0110_TMP
                              changing COUNT_FT
                                       P_ERRO.
    if P_ERRO is not initial.
      return.
    endif.
  endif.

  "Lançamentos Manuais
  loop at IT_0110_COMP into WA_SAIDA_0110 where MANUAL is not initial
                                            and CHECK  is not initial.

    clear: WL_VLRN, WL_VLRC, VL_NAME1.

    perform F_MOEDA_EMPRESA using WA_SAIDA_0110-BUKRS
                                  'X'.
    if ( SY-SUBRC ne 0 ).
      P_ERRO = 'X'.
      return.
    endif.

    concatenate WA_SAIDA_0110-ZFBDT+6(2) WA_SAIDA_0110-ZFBDT+4(2) WA_SAIDA_0110-ZFBDT(4) into VDATA_VENC separated by '.'.

    add 1 to COUNT_FT.

    if ( WA_SAIDA_0110-WRBTR = 0 ) or ( WA_SAIDA_0110-DMBE2 = 0 ).
      P_ERRO = 'X'.
      message 'Existem lançamentos com valores zerados!' type 'S'.
      return.
    endif.

    select single KOART
      from TBSL into WA_SAIDA_0110-KOART
     where BSCHL = WA_SAIDA_0110-BSCHL.

    if ( SY-SUBRC ne 0 ) or ( WA_SAIDA_0110-KOART is initial ).
      P_ERRO = 'X'.
      message 'Tipo de Conta não encontrado!' type 'S'.
      return.
    endif.

    LT_FTPOST-STYPE = 'P'.
    LT_FTPOST-COUNT = COUNT_FT .

    LT_FTPOST-FNAM = 'RF05A-NEWBS'.
    LT_FTPOST-FVAL =  WA_SAIDA_0110-BSCHL.
    append LT_FTPOST.

    LT_FTPOST-FNAM = 'BSEG-HKONT'.
    LT_FTPOST-FVAL = WA_SAIDA_0110-HKONT.
    append LT_FTPOST.

    if P_SAIDA_0100-WAERS = TG_T001-WAERS.
      WL_VLRN = ABS( WA_SAIDA_0110-WRBTR ).
    else.
      WL_VLRN = ABS( WA_SAIDA_0110-DMBE2 ).
    endif.

    write: WL_VLRN to WL_VLRC.

    LT_FTPOST-FNAM = 'BSEG-WRBTR'.
    LT_FTPOST-FVAL =  WL_VLRC.
    append LT_FTPOST.

    if P_SAIDA_0100-WAERS ne TG_T001-WAERS.
      WL_VLRN      = ABS( WA_SAIDA_0110-DMBTR ).
      write: WL_VLRN to WL_VLRC.
      LT_FTPOST-FNAM = 'BSEG-DMBTR'.
      LT_FTPOST-FVAL =  WL_VLRC.
      append LT_FTPOST.
    else.
      WL_VLRN      = ABS( WA_SAIDA_0110-DMBE2 ).
      write: WL_VLRN to WL_VLRC.
      LT_FTPOST-FNAM = 'BSEG-DMBE2'.
      LT_FTPOST-FVAL =  WL_VLRC.
      append LT_FTPOST.
    endif.

    if WA_SAIDA_0110-UMSKS is not initial.
      LT_FTPOST-FNAM = 'RF05A-NEWUM'.
      LT_FTPOST-FVAL = WA_SAIDA_0110-UMSKS.
      append LT_FTPOST.
    endif.

    case WA_SAIDA_0110-KOART.
      when 'D' or 'K'. "Cliente ou Fornecedor
        if WA_SAIDA_0110-ZFBDT is not initial.
          LT_FTPOST-FNAM = 'BSEG-ZFBDT'.
          LT_FTPOST-FVAL = VDATA_VENC.
          append LT_FTPOST.
        endif.

        LT_FTPOST-FNAM = 'BSEG-KIDNO'.
        LT_FTPOST-FVAL =  P_SAIDA_0100-KIDNO.
        append LT_FTPOST.


        LT_FTPOST-FNAM = 'BSEG-GSBER'.
        LT_FTPOST-FVAL =  P_SAIDA_0100-GSBER.
        append LT_FTPOST.

        LT_FTPOST-FNAM = 'BSEG-HZUON'.
        LT_FTPOST-FVAL =  P_SAIDA_0100-OVPED.
        append LT_FTPOST.
      when 'S'. "Razão
        LT_FTPOST-FNAM = 'BSEG-BUPLA'.
        LT_FTPOST-FVAL =  P_SAIDA_0100-GSBER.
        append LT_FTPOST.
    endcase.

    LT_FTPOST-FNAM = 'BSEG-SGTXT'.
    if WA_SAIDA_0110-MANUAL = 'X'.
      LT_FTPOST-FVAL = WA_SAIDA_0110-SGTXT.
    else.
      select single NAME1
        from KNA1 into VL_NAME1
       where KUNNR = WA_SAIDA_0110-HKONT.
      if SY-SUBRC = 0.
        concatenate 'Saldo Residual' VL_NAME1 into  LT_FTPOST-FVAL separated by SPACE.
      else.
        LT_FTPOST-FVAL = 'Saldo Residual'.
      endif.
    endif.

    append LT_FTPOST.

  endloop.

  call function 'POSTING_INTERFACE_CLEARING'
    exporting
      I_AUGLV                    = L_AUGLV
      I_TCODE                    = L_TCODE
      I_SGFUNCT                  = L_SGFUNCT
      I_NO_AUTH                  = 'X'
      I_XSIMU                    = V_XSIMU
    importing
      E_MSGID                    = LDS_RETURN-ID
      E_MSGNO                    = LDS_RETURN-NUMBER
      E_MSGTY                    = LDS_RETURN-TYPE
      E_MSGV1                    = LDS_RETURN-MESSAGE_V1
      E_MSGV2                    = LDS_RETURN-MESSAGE_V2
      E_MSGV3                    = LDS_RETURN-MESSAGE_V3
      E_MSGV4                    = LDS_RETURN-MESSAGE_V4
    tables
      T_BLNTAB                   = LT_BLNTAB
      T_FTCLEAR                  = LT_FTCLEAR
      T_FTPOST                   = LT_FTPOST
      T_FTTAX                    = LT_FTTAX
    exceptions
      CLEARING_PROCEDURE_INVALID = 1
      CLEARING_PROCEDURE_MISSING = 2
      TABLE_T041A_EMPTY          = 3
      TRANSACTION_CODE_INVALID   = 4
      AMOUNT_FORMAT_ERROR        = 5
      TOO_MANY_LINE_ITEMS        = 6
      COMPANY_CODE_INVALID       = 7
      SCREEN_NOT_FOUND           = 8
      NO_AUTHORIZATION           = 9
      others                     = 10.

  refresh IT_RETORNO_RFC.
  if LT_BLNTAB[] is initial.
    P_ERRO = 'X'.
    write LDS_RETURN-NUMBER to MSG_NO.
    call function 'MESSAGE_PREPARE'
      exporting
        MSG_ID                 = LDS_RETURN-ID
        MSG_NO                 = MSG_NO
        MSG_VAR1               = LDS_RETURN-MESSAGE_V1
        MSG_VAR2               = LDS_RETURN-MESSAGE_V2
        MSG_VAR3               = LDS_RETURN-MESSAGE_V3
        MSG_VAR4               = LDS_RETURN-MESSAGE_V4
      importing
        MSG_TEXT               = MSG_TEXT
      exceptions
        FUNCTION_NOT_COMPLETED = 1
        MESSAGE_NOT_FOUND      = 2
        others                 = 3.
    message MSG_TEXT type 'S'.
    if RB_PROD is not initial.
      WA_RETORNO-MANDT  = SY-MANDT.
      WA_RETORNO-BUKRS  = P_SAIDA_0100-BUKRS.
      WA_RETORNO-LOTE   = P_SAIDA_0100-ZUONR.
      WA_RETORNO-STATUS = 'E'.
      WA_RETORNO-SGTXT  = MSG_TEXT.
      WA_RETORNO-DATA   = SY-DATUM.
      WA_RETORNO-HORA   = SY-UZEIT.
      WA_RETORNO-DATA_COMP   = P_AUGDT-LOW.
      WA_RETORNO-INTERFACE   = 'Z'.
      append WA_RETORNO to IT_RETORNO_RFC.

      call function 'Z_FI_OUTBOUND_LOTE_COMPRA_ADD'
        tables
          RETURN = IT_RETORNO_RFC.

      commit work.
    endif.
  else.
    read table LT_BLNTAB index 1.
    clear: WL_0122.
    WL_0122-USNAM         = SY-UNAME.
    WL_0122-DATA          = SY-DATUM.
    WL_0122-HORA          = SY-UZEIT.
    WL_0122-TIPO          = 'C'. "Compensação
    WL_0122-BUKRS         = P_SAIDA_0100-BUKRS.
    WL_0122-GJAHR         = VL_DT_MOV(4).
    WL_0122-BELNR         = LT_BLNTAB-BELNR.
    WL_0122-LOTE          = P_SAIDA_0100-ZUONR.

    if P_KSFCP > 0.
      WL_0122-KURSF = P_KSFCP.
    endif.

    modify ZFIT0122 from WL_0122.
    if SY-SUBRC ne 0.
      P_ERRO = 'X'.
      rollback work.
      message 'Houve ao efeturar a compensação' type 'S'.
      return.
    endif.

    "envia SIGAM
    if P_SAIDA_0100-ST_COMP  = '3'.
      call function 'Z_FI_RETURN_PAYMENT_AP_AR'
        exporting
          I_BUKRS = WL_0122-BUKRS
          I_AUGBL = WL_0122-BELNR
          I_GJAHR = WL_0122-GJAHR
          I_TCODE = 'ZFI0105'.
    endif.
    "
    if RB_PROD is not initial.
      WA_RETORNO-MANDT  = SY-MANDT.
      WA_RETORNO-BUKRS  = P_SAIDA_0100-BUKRS.
      WA_RETORNO-LOTE   = P_SAIDA_0100-ZUONR.
      WA_RETORNO-STATUS = 'S'.
      WA_RETORNO-SGTXT  = |Documento { WL_0122-BELNR } registrado na empresa { P_SAIDA_0100-BUKRS } |.
      WA_RETORNO-DATA  = SY-DATUM.
      WA_RETORNO-HORA  = SY-UZEIT.
      WA_RETORNO-DATA_COMP   = P_AUGDT-LOW.
      WA_RETORNO-INTERFACE   = 'Z'.
      append WA_RETORNO to IT_RETORNO_RFC.

      call function 'Z_FI_OUTBOUND_LOTE_COMPRA_ADD'
        tables
          RETURN = IT_RETORNO_RFC.

      commit work.
    endif.

    clear: WL_0122.
    WL_0122-USNAM         = SY-UNAME.
    WL_0122-DATA          = SY-DATUM.
    WL_0122-HORA          = SY-UZEIT.
    WL_0122-TIPO          = 'A'. "Adiantamento
    WL_0122-BUKRS         = P_SAIDA_0100-BUKRS.
    WL_0122-GJAHR         = P_SAIDA_0100-BUDAT(4).
    WL_0122-BELNR         = P_SAIDA_0100-BELNR.
    WL_0122-LOTE          = P_SAIDA_0100-ZUONR.
    modify ZFIT0122 from WL_0122.
    if SY-SUBRC ne 0.
      P_ERRO = 'X'.
      rollback work.
      message 'Houve ao efeturar a compensação' type 'S'.
      return.
    endif.

    loop at IT_0110_COMP into WA_SAIDA_0110 where CHECK is not initial.

      clear: WL_0122.
      WL_0122-USNAM         = SY-UNAME.
      WL_0122-DATA          = SY-DATUM.
      WL_0122-HORA          = SY-UZEIT.
      WL_0122-TIPO          = 'P'. "Cta.Partidas Adiantamento.
      WL_0122-BUKRS         = WA_SAIDA_0110-BUKRS.
      WL_0122-GJAHR         = WA_SAIDA_0110-BUDAT(4).
      WL_0122-BELNR         = WA_SAIDA_0110-BELNR.
      WL_0122-GJAHR_CP      = P_SAIDA_0100-BUDAT(4).
      WL_0122-BELNR_CP      = P_SAIDA_0100-BELNR.
      WL_0122-LOTE          = P_SAIDA_0100-ZUONR.
      modify ZFIT0122 from WL_0122.
      if SY-SUBRC ne 0.
        P_ERRO = 'X'.
        rollback work.
        message 'Houve ao efeturar a compensação' type 'S'.
        return.
      endif.

    endloop.

    P_DOC_COMP = LT_BLNTAB-BELNR.
    if P_CPLIB is initial.
      message |Compensação gerada com sucesso: { P_DOC_COMP } | type 'I'.
    endif.
  endif.

  "fim
  call function 'POSTING_INTERFACE_END'
    exporting
      I_BDCIMMED              = 'X'
    exceptions
      SESSION_NOT_PROCESSABLE = 1
      others                  = 2.

  if SY-SUBRC <> 0.
    exit.
  endif.

endform.

form F_BAPI_F51_RESIDUAL changing P_SAIDA_0110 type TY_SAIDA_0110
                                  P_ERRO.

  data: WL_0122 type ZFIT0122,
        WL_BSID type bsid_view,
        WL_BSIK type bsik_view.

  data: L_AUGLV   type T041A-AUGLV   value 'UMBUCHNG', "Posting with Clearing
        L_TCODE   type SY-TCODE      value 'FB05',     "You get an error with any other value
        L_SGFUNCT type RFIPI-SGFUNCT value 'C'.        "Post immediately

  data: LT_BLNTAB  type standard table of BLNTAB  with header line,
        LT_FTCLEAR type standard table of FTCLEAR with header line,
        LT_FTPOST  type standard table of FTPOST  with header line,
        LT_FTTAX   type standard table of FTTAX   with header line,
        LDS_RETURN type BAPIRET2.

  data: VDATA(10),
        VDATA_VENC(10),
        CNUM_SEQ(2),
        WL_VLR(16),
        WL_TAXA(16),
        WL_VLRC(16),
        WL_VLRN        type P decimals 2,
        VCAMPO(15),
        V_KUR          type BKPF-KURSF,
        VVALOR_BAX     type ZFIT0042-DMBE2,
        MSG_NO         type T100-MSGNR,
        MSG_TEXT       type STRING,
        P_MODE         like RFPDO-ALLGAZMD,
        COUNT_FT       type FTPOST-COUNT,
        VL_DT_MOV      type SY-DATUM.

  if P_SAIDA_0110-VLR_RSD <= 0.
    message 'Valor Residual inconsistente!' type 'S'.
    P_ERRO = 'X'.
    return.
  endif.

  if P_SAIDA_0110-KURSF <= 0.
    message 'Taxa para gerar residual não encontrada!' type 'S'.
    P_ERRO = 'X'.
    return.
  endif.

  case P_SAIDA_0110-KOART.
    when 'D'. "Cliente
      if ( P_SAIDA_0110-BSCHL ne '01' ) and
         ( P_SAIDA_0110-BSCHL ne '11' ) and
         ( P_SAIDA_0110-BSCHL ne '09' ) and
         ( P_SAIDA_0110-BSCHL ne '19' ).
        data(_ERRO_CHAVE) = 'X'.
      endif.
    when 'K'. "Fornecedor
      if ( P_SAIDA_0110-BSCHL ne '21' ) and
         ( P_SAIDA_0110-BSCHL ne '31' ) and
         ( P_SAIDA_0110-BSCHL ne '29' ) and
         ( P_SAIDA_0110-BSCHL ne '39' ).
        _ERRO_CHAVE = 'X'.
      endif.
  endcase.

  if _ERRO_CHAVE is not initial.
    message 'Chave Lançamento não configurada para deixar Saldo Residual!' type 'S'.
    P_ERRO = 'X'.
    return.
  endif.

  perform F_MOEDA_EMPRESA using P_SAIDA_0110-BUKRS
                                'X'.
  if ( SY-SUBRC ne 0 ).
    P_ERRO = 'X'.
    return.
  endif.

  if P_SAIDA_0110-WAERS = TG_T001-WAERS.
*    IF ( p_saida_0110-dmbtr + p_saida_0110-vlr_rsd ) NE p_saida_0110-dmbtr_aux.
    if ( P_SAIDA_0110-WRBTR + P_SAIDA_0110-VLR_RSD ) ne P_SAIDA_0110-DMBTR_AUX.
      message 'Saldo Residual inconsistente!' type 'S'.
      P_ERRO = 'X'.
      return.
    endif.
  else.
    if ( P_SAIDA_0110-DMBE2 + P_SAIDA_0110-VLR_RSD ) ne P_SAIDA_0110-DMBE2_AUX.
      message 'Saldo Residual inconsistente!' type 'S'.
      P_ERRO = 'X'.
      return.
    endif.
  endif.

  VL_DT_MOV = P_AUGDT-LOW.

  P_MODE = 'N'.

  call function 'POSTING_INTERFACE_START'
    exporting
      I_CLIENT           = SY-MANDT
      I_FUNCTION         = 'C'
      I_MODE             = P_MODE
      I_UPDATE           = 'S'
      I_USER             = SY-UNAME
    exceptions
      CLIENT_INCORRECT   = 1
      FUNCTION_INVALID   = 2
      GROUP_NAME_MISSING = 3
      MODE_INVALID       = 4
      UPDATE_INVALID     = 5
      others             = 6.

  if SY-SUBRC ne 0.
    P_ERRO = 'X'.
    message 'Houve um erro ao desmembrar um documento' type 'S'.
    return.
  endif.

  concatenate VL_DT_MOV+6(2) VL_DT_MOV+4(2) VL_DT_MOV(4) into VDATA separated by '.'.
  concatenate P_SAIDA_0110-ZFBDT+6(2) P_SAIDA_0110-ZFBDT+4(2) P_SAIDA_0110-ZFBDT(4) into VDATA_VENC separated by '.'.

  write: P_SAIDA_0110-KURSF to WL_TAXA.
  condense WL_TAXA no-gaps.

  clear: LT_BLNTAB,   LT_BLNTAB[],
         LT_FTCLEAR,  LT_FTCLEAR[],
         LT_FTPOST,   LT_FTPOST[],
         LT_FTTAX,    LT_FTTAX[],
         LDS_RETURN.

  COUNT_FT = 1.

  LT_FTPOST-STYPE = 'K'."Header
  LT_FTPOST-COUNT = COUNT_FT.  "number of Dynpro

  LT_FTPOST-FNAM = 'BKPF-BUKRS'.
  LT_FTPOST-FVAL = P_SAIDA_0110-BUKRS.
  append LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-WAERS'.
  LT_FTPOST-FVAL = P_SAIDA_0110-WAERS.
  append LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-KURSF'.
  LT_FTPOST-FVAL = WL_TAXA.
  append LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-BLDAT'.
  LT_FTPOST-FVAL = VDATA.
  append LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-BUDAT'.
  LT_FTPOST-FVAL = VDATA.
  append LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-MONAT'.
  LT_FTPOST-FVAL =  VL_DT_MOV+4(2).
  append LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-BLART'.
  if P_SAIDA_0110-BLART is initial.
    P_SAIDA_0110-BLART = 'MB'.
  endif.
  LT_FTPOST-FVAL = P_SAIDA_0110-BLART.
  append LT_FTPOST.

  if RB_PROD = 'X'.
    loop at TG_BSIK_COPY where LIFNR = P_SAIDA_0110-PARID
                         and   ZUONR = P_SAIDA_0110-ZUONR.
      P_SAIDA_0110-XBLNR = TG_BSIK_COPY-XBLNR.
    endloop.
    LT_FTPOST-FNAM = 'BKPF-XBLNR'.
    LT_FTPOST-FVAL = P_SAIDA_0110-XBLNR.
    append LT_FTPOST.
  endif.

  if RB_PROD is initial or P_ERRO eq '1'.
    LT_FTCLEAR-AGKOA  = P_SAIDA_0110-KOART.
    LT_FTCLEAR-AGKON  = P_SAIDA_0110-PARID.
    LT_FTCLEAR-AGUMS  = P_SAIDA_0110-UMSKZ.
    LT_FTCLEAR-AGBUK  = P_SAIDA_0110-BUKRS.
    LT_FTCLEAR-XNOPS  = 'X'.
    LT_FTCLEAR-SELFD  = 'BELNR'.
    concatenate P_SAIDA_0110-BELNR P_SAIDA_0110-BUDAT(4) P_SAIDA_0110-BUZEI into LT_FTCLEAR-SELVON.
    append LT_FTCLEAR.
  else.
    loop at TG_BSIK_COPY where LIFNR = P_SAIDA_0110-PARID
                         and   ZUONR = P_SAIDA_0110-ZUONR.
      LT_FTCLEAR-AGKOA  = P_SAIDA_0110-KOART.
      LT_FTCLEAR-AGKON  = P_SAIDA_0110-PARID.
      LT_FTCLEAR-AGUMS  = TG_BSIK_COPY-UMSKZ.
      LT_FTCLEAR-AGBUK  = P_SAIDA_0110-BUKRS.
      LT_FTCLEAR-XNOPS  = 'X'.
      LT_FTCLEAR-SELFD  = 'BELNR'.
      concatenate TG_BSIK_COPY-BELNR TG_BSIK_COPY-BUDAT(4) TG_BSIK_COPY-BUZEI into LT_FTCLEAR-SELVON.
      append LT_FTCLEAR.
    endloop.
  endif.

  clear P_ERRO.

  "Valor residual
  do 2 times.

    clear: WL_VLRN, WL_VLRC.

    add 1 to COUNT_FT.

    case SY-INDEX .
      when 1.
        if P_SAIDA_0110-WAERS = TG_T001-WAERS.
*          wl_vlrn = abs( p_saida_0110-dmbtr ).
          WL_VLRN = ABS( P_SAIDA_0110-WRBTR ).
        else.
          WL_VLRN = ABS( P_SAIDA_0110-DMBE2 ).
        endif.
      when 2.
        WL_VLRN = ABS( P_SAIDA_0110-VLR_RSD ).
    endcase.

    write: WL_VLRN to WL_VLRC.

    LT_FTPOST-STYPE = 'P'.
    LT_FTPOST-COUNT = COUNT_FT .

    LT_FTPOST-FNAM = 'RF05A-NEWBS'.
    LT_FTPOST-FVAL =  P_SAIDA_0110-BSCHL.
    append LT_FTPOST.

    LT_FTPOST-FNAM = 'BSEG-HKONT'.
    LT_FTPOST-FVAL = P_SAIDA_0110-PARID.
    append LT_FTPOST.

    LT_FTPOST-FNAM = 'BSEG-GSBER'.
    LT_FTPOST-FVAL = P_SAIDA_0110-GSBER.
    append LT_FTPOST.

    LT_FTPOST-FNAM = 'BSEG-SGTXT'.
    LT_FTPOST-FVAL = P_SAIDA_0110-SGTXT_RSD.
    append LT_FTPOST.

    if RB_PROD = 'X'.
      loop at TG_BSIK_COPY where LIFNR = P_SAIDA_0110-PARID
                           and   ZUONR = P_SAIDA_0110-ZUONR.
        P_SAIDA_0110-KIDNO  = TG_BSIK_COPY-KIDNO.
        P_SAIDA_0110-XREF1  = TG_BSIK_COPY-XREF1.
        P_SAIDA_0110-XREF3  = TG_BSIK_COPY-XREF3.
      endloop.

      if not ( '21_29' cs P_SAIDA_0110-BSCHL ).
        LT_FTPOST-FNAM = 'BSEG-KIDNO'.
        LT_FTPOST-FVAL = P_SAIDA_0110-KIDNO.
        if P_SAIDA_0110-KIDNO is initial.
          LT_FTPOST-FVAL = WA_SAIDA_0100-KIDNO.
        endif.
        append LT_FTPOST.
      endif.
      "

    endif.

    LT_FTPOST-FNAM = 'BSEG-ZUONR'.
    LT_FTPOST-FVAL = P_SAIDA_0110-ZUONR.
    append LT_FTPOST.

    LT_FTPOST-FNAM = 'BSEG-HZUON'.
    LT_FTPOST-FVAL =  P_SAIDA_0110-OVPED.

    if P_SAIDA_0110-EBELP is not initial.
      LT_FTPOST-FVAL = LT_FTPOST-FVAL && P_SAIDA_0110-EBELP.
    endif.

    append LT_FTPOST.

    if P_SAIDA_0110-ZFBDT is not initial.
      LT_FTPOST-FNAM = 'BSEG-ZFBDT'.
      LT_FTPOST-FVAL = VDATA_VENC.
      append LT_FTPOST.

      if ( P_SAIDA_0110-UMSKS is initial ).
        LT_FTPOST-FNAM = 'BSEG-ZBD1T'.
        LT_FTPOST-FVAL = P_SAIDA_0110-ZBD1T.
        condense LT_FTPOST-FVAL no-gaps.
        append LT_FTPOST.
      endif.
    endif.

    if P_SAIDA_0110-UMSKS is not initial. "Adiantamento

      LT_FTPOST-FNAM = 'RF05A-NEWUM'.
      LT_FTPOST-FVAL = P_SAIDA_0110-UMSKZ.
      append LT_FTPOST.

      if ( P_SAIDA_0110-ANLN1 is not initial ).
        LT_FTPOST-FNAM = 'BSEG-ANLN1'.
        LT_FTPOST-FVAL = P_SAIDA_0110-ANLN1.
        append LT_FTPOST.

        if P_SAIDA_0110-ANLN2 is not initial.
          LT_FTPOST-FNAM = 'BSEG-ANLN2'.
          LT_FTPOST-FVAL = P_SAIDA_0110-ANLN2.
          append LT_FTPOST.
        endif.
      endif.
    endif.

    LT_FTPOST-FNAM = 'BSEG-WRBTR'.
    LT_FTPOST-FVAL =  WL_VLRC.
    append LT_FTPOST.

    if P_SAIDA_0110-WAERS ne TG_T001-WAERS.
      WL_VLRN = WL_VLRN * ABS( P_SAIDA_0110-KURSF ).
      write: WL_VLRN to WL_VLRC.
      LT_FTPOST-FNAM = 'BSEG-DMBTR'.
      LT_FTPOST-FVAL =  WL_VLRC.
      append LT_FTPOST.
    else.
      WL_VLRN = WL_VLRN / ABS( P_SAIDA_0110-KURSF ).
      write: WL_VLRN to WL_VLRC.
      LT_FTPOST-FNAM = 'BSEG-DMBE2'.
      LT_FTPOST-FVAL =  WL_VLRC.
      append LT_FTPOST.
    endif.

  enddo.

  call function 'POSTING_INTERFACE_CLEARING'
    exporting
      I_AUGLV                    = L_AUGLV
      I_TCODE                    = L_TCODE
      I_SGFUNCT                  = L_SGFUNCT
      I_NO_AUTH                  = 'X'
    importing
      E_MSGID                    = LDS_RETURN-ID
      E_MSGNO                    = LDS_RETURN-NUMBER
      E_MSGTY                    = LDS_RETURN-TYPE
      E_MSGV1                    = LDS_RETURN-MESSAGE_V1
      E_MSGV2                    = LDS_RETURN-MESSAGE_V2
      E_MSGV3                    = LDS_RETURN-MESSAGE_V3
      E_MSGV4                    = LDS_RETURN-MESSAGE_V4
    tables
      T_BLNTAB                   = LT_BLNTAB
      T_FTCLEAR                  = LT_FTCLEAR
      T_FTPOST                   = LT_FTPOST
      T_FTTAX                    = LT_FTTAX
    exceptions
      CLEARING_PROCEDURE_INVALID = 1
      CLEARING_PROCEDURE_MISSING = 2
      TABLE_T041A_EMPTY          = 3
      TRANSACTION_CODE_INVALID   = 4
      AMOUNT_FORMAT_ERROR        = 5
      TOO_MANY_LINE_ITEMS        = 6
      COMPANY_CODE_INVALID       = 7
      SCREEN_NOT_FOUND           = 8
      NO_AUTHORIZATION           = 9
      others                     = 10.


  if LT_BLNTAB[] is initial.
    P_ERRO = 'X'.
    write LDS_RETURN-NUMBER to MSG_NO.
    call function 'MESSAGE_PREPARE'
      exporting
        MSG_ID                 = LDS_RETURN-ID
        MSG_NO                 = MSG_NO
        MSG_VAR1               = LDS_RETURN-MESSAGE_V1
        MSG_VAR2               = LDS_RETURN-MESSAGE_V2
        MSG_VAR3               = LDS_RETURN-MESSAGE_V3
        MSG_VAR4               = LDS_RETURN-MESSAGE_V4
      importing
        MSG_TEXT               = MSG_TEXT
      exceptions
        FUNCTION_NOT_COMPLETED = 1
        MESSAGE_NOT_FOUND      = 2
        others                 = 3.
    message MSG_TEXT type 'S'.
  else.
    read table LT_BLNTAB index 1.
    if RB_PROD is initial.
      clear: WL_0122.
      WL_0122-USNAM         = SY-UNAME.
      WL_0122-DATA          = SY-DATUM.
      WL_0122-HORA          = SY-UZEIT.
      WL_0122-TIPO          = 'D'. "Desmembramento
      WL_0122-BUKRS         = P_SAIDA_0110-BUKRS.
      WL_0122-GJAHR         = P_SAIDA_0110-GJAHR.
      WL_0122-BELNR         = P_SAIDA_0110-BELNR.
      WL_0122-GJAHR_DESMEMB = VL_DT_MOV(4).
      WL_0122-BELNR_DESMEMB = LT_BLNTAB-BELNR.
      modify ZFIT0122 from WL_0122.
    else.
      loop at TG_BSIK_COPY where LIFNR = P_SAIDA_0110-PARID
                           and   ZUONR = P_SAIDA_0110-ZUONR.
        clear: WL_0122.
        WL_0122-USNAM         = SY-UNAME.
        WL_0122-DATA          = SY-DATUM.
        WL_0122-HORA          = SY-UZEIT.
        WL_0122-TIPO          = 'D'. "Desmembramento
        WL_0122-BUKRS         = TG_BSIK_COPY-BUKRS.
        WL_0122-GJAHR         = TG_BSIK_COPY-GJAHR.
        WL_0122-BELNR         = TG_BSIK_COPY-BELNR.
        WL_0122-GJAHR_DESMEMB = VL_DT_MOV(4).
        WL_0122-BELNR_DESMEMB = LT_BLNTAB-BELNR.
        modify ZFIT0122 from WL_0122.
      endloop.
    endif.
    if SY-SUBRC ne 0.
      P_ERRO = 'X'.
      message 'Houve um erro ao desmembrar um documento' type 'S'.
      return.
    endif.

    clear: WL_BSID, WL_BSIK.

    case P_SAIDA_0110-KOART.
      when 'D'. "Cliente
        if P_SAIDA_0110-WAERS = TG_T001-WAERS.
          select single *
            from bsid_view into @WL_BSID
           where BUKRS = @P_SAIDA_0110-BUKRS
             and GJAHR = @VL_DT_MOV(4)
             and BELNR = @LT_BLNTAB-BELNR
             and WRBTR = @P_SAIDA_0110-WRBTR.
*             AND dmbtr = p_saida_0110-dmbtr.
        else.
          select single *
            from BSID_view
           where BUKRS = @P_SAIDA_0110-BUKRS
             and GJAHR = @VL_DT_MOV(4)
             and BELNR = @LT_BLNTAB-BELNR
             and DMBE2 = @P_SAIDA_0110-DMBE2
            into @WL_BSID.
        endif.
      when 'K'. "Fornecedor
        if P_SAIDA_0110-WAERS = TG_T001-WAERS.
          select single *
            from bsik_view
           where BUKRS = @P_SAIDA_0110-BUKRS
             and GJAHR = @VL_DT_MOV(4)
             and BELNR = @LT_BLNTAB-BELNR
             and WRBTR = @P_SAIDA_0110-WRBTR
            into @WL_BSIK.
*             AND dmbtr = p_saida_0110-dmbtr.
        else.
          select single *
            from BSIK_view
           where BUKRS = @P_SAIDA_0110-BUKRS
             and GJAHR = @VL_DT_MOV(4)
             and BELNR = @LT_BLNTAB-BELNR
             and DMBE2 = @P_SAIDA_0110-DMBE2
             into @WL_BSIK.
        endif.
    endcase.

    if SY-SUBRC ne 0 .
      P_ERRO = 'X'.
      message 'Houve um erro ao desmembrar um documento' type 'S'.
      return.
    endif.

    "Faz a troca para o novo documento gerado com valor a ser baixado.
    P_SAIDA_0110-BL_DESMEMB = P_SAIDA_0110-BELNR.
    P_SAIDA_0110-BZ_DESMEMB = P_SAIDA_0110-BUZEI.
    if RB_PROD is not initial .
      P_SAIDA_0110-BL_DESMEMB = WL_BSIK-BELNR.
      P_SAIDA_0110-BZ_DESMEMB = WL_BSIK-BUZEI.
    endif.
    case P_SAIDA_0110-KOART.
      when 'D'. "Cliente
        P_SAIDA_0110-BELNR      = WL_BSID-BELNR.
        P_SAIDA_0110-BUZEI      = WL_BSID-BUZEI.
        P_SAIDA_0110-GJAHR      = WL_BSID-GJAHR.
        P_SAIDA_0110-BLDAT      = WL_BSID-BLDAT.
        P_SAIDA_0110-BUDAT      = WL_BSID-BUDAT.
      when 'K'. "Fornecedor
        P_SAIDA_0110-BELNR      = WL_BSIK-BELNR.
        P_SAIDA_0110-BUZEI      = WL_BSIK-BUZEI.
        P_SAIDA_0110-GJAHR      = WL_BSIK-GJAHR.
        P_SAIDA_0110-BLDAT      = WL_BSIK-BLDAT.
        P_SAIDA_0110-BUDAT      = WL_BSIK-BUDAT.
    endcase.
    if RB_PROD = 'X'.
* ---> S4 Migration - 15/06/2023 - MA
*      SELECT *
*        FROM bseg
*        INTO TABLE  @DATA(t_bseg_atribui)
*        WHERE bukrs = @wl_bsik-bukrs
*        AND   belnr = @wl_bsik-belnr
*        AND   bschl = @wl_bsik-bschl
*        AND   gjahr = @wl_bsik-gjahr.

      data: LT_BSEG        type table of BSEG,
            T_BSEG_ATRIBUI type table of BSEG.

      call function 'FAGL_GET_BSEG'
        exporting
          I_BUKRS   = WL_BSIK-BUKRS
          I_BELNR   = WL_BSIK-BELNR
          I_GJAHR   = WL_BSIK-GJAHR
        importing
          ET_BSEG   = LT_BSEG
        exceptions
          NOT_FOUND = 1
          others    = 2.

      delete LT_BSEG where BSCHL ne WL_BSIK-BSCHL.

      if SY-SUBRC = 0 and LINES( LT_BSEG ) > 0.
        MOVE-CORRESPONDING LT_BSEG to T_BSEG_ATRIBUI.
        SY-DBCNT = LINES( LT_BSEG ).
      else.
        SY-SUBRC = 4.
        SY-DBCNT = 0.
      endif.
* <--- S4 Migration - 15/06/2023 - MA

      loop at T_BSEG_ATRIBUI into data(W_BSEG_ATRIBUI).
        perform F_ATRIBUI using W_BSEG_ATRIBUI P_SAIDA_0110-XREF1 P_SAIDA_0110-XREF3.
      endloop.

    endif.

  endif.

  "fim
  call function 'POSTING_INTERFACE_END'
    exporting
      I_BDCIMMED              = 'X'
    exceptions
      SESSION_NOT_PROCESSABLE = 1
      others                  = 2.

  if SY-SUBRC <> 0.
    exit.
  endif.

endform.


form F_ATRIBUI using P_BSEG  type BSEG
                     P_XREF1 type BSEG-XREF1
                     P_XREF3 type BSEG-XREF3.

  data: LT_BKDF  type table of BKDF,
        LT_BKPF  type table of BKPF,
        WA_BKPF  type BKPF,
        LT_BSEC  type table of BSEC,
        WA_BSEG  type BSEG,
        LT_BSED  type table of BSED,
        LT_BSEG  type table of BSEG,
        LT_BSET  type table of BSET,
        MSG_TEXT type STRING.

  clear WA_BKPF.
  refresh LT_BKPF.
  select single *
    from BKPF
    into WA_BKPF
  where BUKRS = P_BSEG-BUKRS
  and   BELNR = P_BSEG-BELNR
  and   GJAHR = P_BSEG-GJAHR.

  append WA_BKPF to LT_BKPF.
  clear WA_BSEG.
  refresh  LT_BSEG.
  move-corresponding P_BSEG to WA_BSEG.
  WA_BSEG-XREF1 = P_XREF1.
  WA_BSEG-XREF3 = P_XREF3.
  append WA_BSEG to LT_BSEG.

  call function 'CHANGE_DOCUMENT'
    tables
      T_BKDF = LT_BKDF
      T_BKPF = LT_BKPF
      T_BSEC = LT_BSEC
      T_BSED = LT_BSED
      T_BSEG = LT_BSEG
      T_BSET = LT_BSET.

  if SY-SUBRC = 0.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        WAIT = 'X'.
  endif.

endform.

form F_GET_TAXA  using P_BKPF  like TG_BKPF
                       P_DMBTR type BSID-DMBTR
                       P_DMBE2 type BSID-DMBE2
              changing P_KURSF type BKPF-KURSF.

  data: V_KURSF_BANCO type BKPF-KURSF.

  clear: P_KURSF.

  perform F_MOEDA_EMPRESA using P_BKPF-BUKRS
                                'X'.
  if ( SY-SUBRC ne 0 ).
    return.
  endif.

*  IF ( P_BKPF-WAERS = TG_T001-WAERS ).
*    P_KURSF = ABS( P_BKPF-KURS2 ).
*  ELSE.
*    P_KURSF = ABS( P_BKPF-KURSF ).
*  ENDIF.

  if ( P_DMBTR > 0 ) and  ( P_DMBE2 > 0 ).

    try.
        P_KURSF = P_DMBTR / P_DMBE2.
      catch CX_SY_ARITHMETIC_OVERFLOW.
    endtry.

  endif.

  perform F_GET_BSIS_CBANCO using P_BKPF-BUKRS
                                  P_BKPF-BELNR
                                  P_BKPF-GJAHR
                         changing V_KURSF_BANCO.

  if V_KURSF_BANCO > 0.
    P_KURSF = ABS( V_KURSF_BANCO ).
  endif.

endform.

form F_ATUALIZA_SALDO.

  data: VL_TABIX type SY-TABIX.

  clear: WA_CABECALHO_0110-SEL_DMBTR,
         WA_CABECALHO_0110-SEL_DMBE2,
         WA_CABECALHO_0110-SLD_DMBTR,
         WA_CABECALHO_0110-SLD_DMBE2.

  perform F_MOEDA_EMPRESA using WA_SAIDA_0100-BUKRS
                                'X'.
  if ( SY-SUBRC ne 0 ).
    return.
  endif.

*  wa_cabecalho_0110-adt_dmbtr = wa_saida_0100-dmbtr.
  WA_CABECALHO_0110-ADT_DMBTR = WA_SAIDA_0100-WRBTR.
  WA_CABECALHO_0110-ADT_DMBE2 = WA_SAIDA_0100-DMBE2.

*------------------------------------------------------------------------*
*  Recalcula valor caso atribuido um valor residual para a partida.
*------------------------------------------------------------------------*
  if WA_CABECALHO_0110-RSD_ADT > 0.
    case WA_SAIDA_0100-WAERS.

      when TG_T001-WAERS.
*        wa_cabecalho_0110-adt_dmbtr = wa_saida_0100-dmbtr - wa_cabecalho_0110-rsd_adt.
        WA_CABECALHO_0110-ADT_DMBTR = WA_SAIDA_0100-WRBTR - WA_CABECALHO_0110-RSD_ADT.

        if WA_CABECALHO_0110-ADT_DMBTR > 0.
          WA_CABECALHO_0110-ADT_DMBE2 = WA_CABECALHO_0110-ADT_DMBTR / WA_SAIDA_0100-KURSF .
        else.
*          wa_cabecalho_0110-adt_dmbtr = wa_saida_0100-dmbtr.
          WA_CABECALHO_0110-ADT_DMBTR = WA_SAIDA_0100-WRBTR.
          WA_CABECALHO_0110-ADT_DMBE2 = WA_SAIDA_0100-DMBE2.
          clear: WA_CABECALHO_0110-RSD_ADT.
        endif.

      when others.

        WA_CABECALHO_0110-ADT_DMBE2 = WA_SAIDA_0100-DMBE2 - WA_CABECALHO_0110-RSD_ADT.

        if WA_CABECALHO_0110-ADT_DMBE2 > 0.
*          wa_cabecalho_0110-adt_dmbtr = wa_cabecalho_0110-adt_dmbe2 * wa_saida_0100-kursf .
          WA_CABECALHO_0110-ADT_DMBTR = WA_SAIDA_0100-WRBTR.
        else.
*          wa_cabecalho_0110-adt_dmbtr = wa_saida_0100-dmbtr.
          WA_CABECALHO_0110-ADT_DMBTR = WA_SAIDA_0100-WRBTR.
          WA_CABECALHO_0110-ADT_DMBE2 = WA_SAIDA_0100-DMBE2.
          clear: WA_CABECALHO_0110-RSD_ADT.
        endif.

    endcase.

  endif.

*------------------------------------------------------------------------*
*  Processamento contra partidas.
*------------------------------------------------------------------------*

  loop at IT_SAIDA_0110 into WA_SAIDA_0110.

    VL_TABIX = SY-TABIX.

    select single SHKZG
      from TBSL into WA_SAIDA_0110-SHKZG
     where BSCHL = WA_SAIDA_0110-BSCHL.

    case WA_SAIDA_0100-WAERS.
      when TG_T001-WAERS.

        if WA_SAIDA_0110-MANUAL is not initial. "Tratamento partida manual

          if ( WA_SAIDA_0110-DMBE2 is initial     ) and
             ( WA_SAIDA_0110-WRBTR is not initial ).
*            wa_saida_0110-dmbe2 = wa_saida_0110-dmbtr / wa_saida_0110-kursf.
            WA_SAIDA_0110-DMBE2 = WA_SAIDA_0110-WRBTR / WA_SAIDA_0110-KURSF.
          endif.

        else. "Tratamento outras partidas.

*          wa_saida_0110-dmbtr = wa_saida_0110-dmbtr_aux.
          WA_SAIDA_0110-WRBTR = WA_SAIDA_0110-DMBTR_AUX.

          if ( WA_SAIDA_0110-CHECK   is not initial ) and
             ( WA_SAIDA_0110-VLR_RSD is initial ).
          else.
            if ( WA_SAIDA_0110-VLR_RSD <= 0 ) or
               ( WA_SAIDA_0110-VLR_RSD >= WA_SAIDA_0110-WRBTR ).
              clear: WA_SAIDA_0110-VLR_RSD, WA_SAIDA_0110-CHECK.
            else.
*              SUBTRACT wa_saida_0110-vlr_rsd FROM wa_saida_0110-dmbtr.
              subtract WA_SAIDA_0110-VLR_RSD from WA_SAIDA_0110-WRBTR.
              WA_SAIDA_0110-CHECK = 'X'.
            endif.
          endif.

          WA_SAIDA_0110-DMBE2 = WA_SAIDA_0110-DMBTR / WA_SAIDA_0110-KURSF.

        endif.

      when others.

        if WA_SAIDA_0110-MANUAL is not initial. "Tratamento partida manual

          if ( WA_SAIDA_0110-WRBTR is initial     ) and
             ( WA_SAIDA_0110-DMBE2 is not initial ).
            WA_SAIDA_0110-WRBTR = WA_SAIDA_0110-DMBE2 * WA_SAIDA_0110-KURSF.
          endif.

        else. "Tratamento outras partidas.

          WA_SAIDA_0110-DMBE2 = WA_SAIDA_0110-DMBE2_AUX.

          if ( WA_SAIDA_0110-CHECK   is not initial ) and
             ( WA_SAIDA_0110-VLR_RSD is initial ).
          else.
            if ( WA_SAIDA_0110-VLR_RSD <= 0 ) or
               ( WA_SAIDA_0110-VLR_RSD >= WA_SAIDA_0110-DMBE2 ).
              clear: WA_SAIDA_0110-VLR_RSD, WA_SAIDA_0110-CHECK.
            else.
              subtract WA_SAIDA_0110-VLR_RSD from WA_SAIDA_0110-DMBE2.
              WA_SAIDA_0110-CHECK = 'X'.
            endif.
          endif.

          WA_SAIDA_0110-DMBTR = WA_SAIDA_0110-DMBE2 * WA_SAIDA_0110-KURSF.

        endif.

    endcase.

    if WA_SAIDA_0110-CHECK is not initial.

      "IF WA_SAIDA_0110-MANUAL IS NOT INITIAL.

      if  ( WA_SAIDA_0110-MANUAL     is not initial ) and
          ( WA_SAIDA_0110-PART_PRINC is not initial ). "Aplicar diferença na partida principal

        if ( WA_SAIDA_0110-SHKZG ne WA_SAIDA_0100-SHKZG ). "Se for diferente operação(Cred.Deb.)
*          ADD wa_saida_0110-dmbtr TO wa_cabecalho_0110-adt_dmbtr.
          add WA_SAIDA_0110-WRBTR to WA_CABECALHO_0110-ADT_DMBTR.
          add WA_SAIDA_0110-DMBE2 to WA_CABECALHO_0110-ADT_DMBE2.
        else.
*          SUBTRACT wa_saida_0110-dmbtr FROM wa_cabecalho_0110-adt_dmbtr.
          subtract WA_SAIDA_0110-WRBTR from WA_CABECALHO_0110-ADT_DMBTR.
          subtract WA_SAIDA_0110-DMBE2 from WA_CABECALHO_0110-ADT_DMBE2.
        endif.

      else.

        if  ( WA_SAIDA_0110-MANUAL is not initial ).
          if ( WA_SAIDA_0110-SHKZG eq WA_SAIDA_0100-SHKZG ). "Se for operação(Cred.Deb.) igual da partida principal
*            ADD wa_saida_0110-dmbtr TO wa_cabecalho_0110-sel_dmbtr.
            add WA_SAIDA_0110-WRBTR to WA_CABECALHO_0110-SEL_DMBTR.
            add WA_SAIDA_0110-DMBE2 to WA_CABECALHO_0110-SEL_DMBE2.
          else.
*            SUBTRACT wa_saida_0110-dmbtr FROM wa_cabecalho_0110-sel_dmbtr.
            subtract WA_SAIDA_0110-WRBTR from WA_CABECALHO_0110-SEL_DMBTR.
            subtract WA_SAIDA_0110-DMBE2 from WA_CABECALHO_0110-SEL_DMBE2.
          endif.
        else.
          if ( WA_SAIDA_0110-SHKZG ne WA_SAIDA_0100-SHKZG ). "Se for operação(Cred.Deb.) diferente da partida principal
*            ADD wa_saida_0110-dmbtr TO wa_cabecalho_0110-sel_dmbtr.
            add WA_SAIDA_0110-WRBTR to WA_CABECALHO_0110-SEL_DMBTR.
            add WA_SAIDA_0110-DMBE2 to WA_CABECALHO_0110-SEL_DMBE2.
          else.
*            SUBTRACT wa_saida_0110-dmbtr FROM wa_cabecalho_0110-sel_dmbtr.
            subtract WA_SAIDA_0110-WRBTR from WA_CABECALHO_0110-SEL_DMBTR.
            subtract WA_SAIDA_0110-DMBE2 from WA_CABECALHO_0110-SEL_DMBE2.
          endif.
        endif.

      endif.

      "ELSE.
      "  ADD WA_SAIDA_0110-DMBTR TO WA_CABECALHO_0110-SEL_DMBTR.
      "  ADD WA_SAIDA_0110-DMBE2 TO WA_CABECALHO_0110-SEL_DMBE2.
      "ENDIF.

    endif.

    modify IT_SAIDA_0110 from WA_SAIDA_0110 index VL_TABIX.

  endloop.

  WA_CABECALHO_0110-SLD_DMBTR = WA_CABECALHO_0110-ADT_DMBTR - WA_CABECALHO_0110-SEL_DMBTR.
  WA_CABECALHO_0110-SLD_DMBE2 = WA_CABECALHO_0110-ADT_DMBE2 - WA_CABECALHO_0110-SEL_DMBE2.




endform.

form F_COMPENSAR_ADT using P_SAIDA_0100 type TY_SAIDA_0100.

  data: VL_ERROR    type C,
        VL_DOC_COMP type BSAD-BELNR,
        VL_MSG      type STRING.

  data: WA_SAI_0110_TMP type TY_SAIDA_0110.

  field-symbols <SAIDA_0110> type TY_SAIDA_0110.

  perform F_VALIDA_ALV_0110 changing VL_ERROR.

  check VL_ERROR is initial.

  perform F_ATUALIZA_SALDO.

  perform F_MOEDA_EMPRESA using P_SAIDA_0100-BUKRS
                                'X'.
  if ( SY-SUBRC ne 0 ).
    return.
  endif.

  if WA_SAIDA_0100-WAERS = TG_T001-WAERS.
    if WA_CABECALHO_0110-SLD_DMBTR ne 0.
      message 'Saldo diferente de 0' type 'W'.
      return.
    endif.
  else.
    if WA_CABECALHO_0110-SLD_DMBE2 ne 0.
      message 'Saldo diferente de 0' type 'W'.
      return.
    endif.
  endif.

  call function 'POPUP_TO_CONFIRM'
    exporting
      TITLEBAR              = 'Confirmação'
      TEXT_QUESTION         = 'Confirma compensaçao dos registros?'
      TEXT_BUTTON_1         = 'Sim'
      TEXT_BUTTON_2         = 'Não'
      DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = ''
    importing
      ANSWER                = VAR_ANSWER
    exceptions
      TEXT_NOT_FOUND        = 1
      others                = 2.

  check VAR_ANSWER eq '1'.

  "Check Compensação Imobilizado com Residual.
  data(_COMP_IMOB_RESIDUAL) = ''.
  loop at IT_SAIDA_0110 assigning <SAIDA_0110> where VLR_RSD  ne 0
                                                 and CHECK is not initial
                                                 and ANLN1 is not initial.
    _COMP_IMOB_RESIDUAL = 'X'.
  endloop.

  if ( WA_CABECALHO_0110-RSD_ADT > 0 ) and ( WA_SAIDA_0100-ANLN1 is not initial ).
    _COMP_IMOB_RESIDUAL = 'X'.
  endif.
  "Fim Check Compensação Imobilizado com Residual

  "Verifica Lançamentos que devem ser desmembrados.
  if _COMP_IMOB_RESIDUAL is initial.
    loop at IT_SAIDA_0110 assigning <SAIDA_0110> where VLR_RSD ne 0
                                                   and CHECK is not initial.
      VL_ERROR = '1'.
      perform F_BAPI_F51_RESIDUAL changing <SAIDA_0110>
                                           VL_ERROR.
      if VL_ERROR is not initial.
        leave to screen 0.
        return.
      endif.
    endloop.

    commit work.

    "Verifica se Adiantamento deve ser desmembrado.
    if ( WA_CABECALHO_0110-RSD_ADT > 0 ).
      clear: WA_SAI_0110_TMP.

      move-corresponding WA_SAIDA_0100 to WA_SAI_0110_TMP.

      if WA_CABECALHO_0110-MANTER_TP_DC is initial and RB_PROD is initial.
        WA_SAI_0110_TMP-BLART     = 'AB'.
      endif.
      WA_SAI_0110_TMP-VLR_RSD   = WA_CABECALHO_0110-RSD_ADT.
*      wa_sai_0110_tmp-dmbtr     = wa_cabecalho_0110-adt_dmbtr.
      WA_SAI_0110_TMP-WRBTR     = WA_CABECALHO_0110-ADT_DMBTR.
      WA_SAI_0110_TMP-DMBE2     = WA_CABECALHO_0110-ADT_DMBE2.
      WA_SAI_0110_TMP-SGTXT_RSD = WA_CABECALHO_0110-SGTXT_RSD.

*      wa_sai_0110_tmp-dmbtr_aux = wa_saida_0100-dmbtr. "Valor Original Adiantamento
      WA_SAI_0110_TMP-DMBTR_AUX = WA_SAIDA_0100-WRBTR. "Valor Original Adiantamento
      WA_SAI_0110_TMP-DMBE2_AUX = WA_SAIDA_0100-DMBE2. "Valor Original Adiantamento

      perform F_BAPI_F51_RESIDUAL changing WA_SAI_0110_TMP
                                           VL_ERROR.
      if VL_ERROR is not initial.
        leave to screen 0.
        return.
      endif.

      if WA_SAI_0110_TMP-BL_DESMEMB is initial. "Documento Desmembrado
        leave to screen 0.
        return.
      endif.

      WA_SAIDA_0100-BELNR = WA_SAI_0110_TMP-BELNR.
      WA_SAIDA_0100-BUZEI = WA_SAI_0110_TMP-BUZEI.
      WA_SAIDA_0100-GJAHR = WA_SAI_0110_TMP-GJAHR.
      WA_SAIDA_0100-BLDAT = WA_SAI_0110_TMP-BLDAT.
      WA_SAIDA_0100-BUDAT = WA_SAI_0110_TMP-BUDAT.
      WA_SAIDA_0100-DMBTR = WA_SAI_0110_TMP-DMBTR.
      WA_SAIDA_0100-DMBE2 = WA_SAI_0110_TMP-DMBE2.
    endif.
  endif.

  WA_SAIDA_0100-VLR_RSD   = WA_CABECALHO_0110-RSD_ADT.   "Atribuição Saldo Residual
  WA_SAIDA_0100-WRBTR     = WA_CABECALHO_0110-ADT_DMBTR.
  WA_SAIDA_0100-DMBE2     = WA_CABECALHO_0110-ADT_DMBE2.
  WA_SAIDA_0100-SGTXT_RSD = WA_CABECALHO_0110-SGTXT_RSD.

  perform F_BAPI_F51 using _COMP_IMOB_RESIDUAL
                  changing WA_SAIDA_0100
                           VL_ERROR
                           VL_DOC_COMP.

  if VL_ERROR is initial.
    concatenate 'Compensação efetuada com sucesso! Doc.Compensação:' VL_DOC_COMP
           into VL_MSG separated by SPACE.
    message VL_MSG type 'S'.
  endif.

  leave to screen 0.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_ALV_0110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VL_ERROR  text
*----------------------------------------------------------------------*
form F_VALIDA_ALV_0110  changing P_ERROR.

  clear: P_ERROR.

  "Lançamentos Manuais
  loop at IT_SAIDA_0110 into WA_SAIDA_0110 where MANUAL is not initial.

    if WA_SAIDA_0110-HKONT is initial.
      P_ERROR = 'X'.
      message 'Conta é um campo obrigatório!' type 'S'.
      return.
    endif.

    if WA_SAIDA_0110-BSCHL is initial.
      P_ERROR = 'X'.
      message 'Chave Lcto. é um campo obrigatório!' type 'S'.
      return.
    endif.

*    IF wa_saida_0110-dmbtr IS INITIAL.
    if WA_SAIDA_0110-WRBTR is initial.
      P_ERROR = 'X'.
      message 'Valor R$ é um campo obrigatório!' type 'S'.
      return.
    endif.

    if WA_SAIDA_0110-DMBE2 is initial.
      P_ERROR = 'X'.
      message 'Valor U$ é um campo obrigatório!' type 'S'.
      return.
    endif.

    if ( WA_SAIDA_0110-UMSKS is not initial ) and
       ( WA_SAIDA_0110-ZFBDT is initial ).
      P_ERROR = 'X'.
      message 'Data Venc. é um campo obrigatório!' type 'S'.
      return.
    endif.

  endloop.

endform.

form F_RENOVAR_CONS.
  if VG_CANCEL = 'X'.
    clear VG_CANCEL.
    perform F_REFRESH_ALV using '0100'..
    exit.
  endif.
  perform F_SELECIONAR_DADOS.
  check VG_NOT_FOUND is initial.
  perform: F_PROCESSA_DADOS,
           F_REFRESH_ALV using '0100'.

endform.

form F_FLAG_DOCUMENTOS  using P_MARK.

  field-symbols: <SAIDA_0110> type TY_SAIDA_0110.
  data: VL_MSG    type STRING,
        VL_MSG_01 type STRING.

  clear: IT_SEL_ROWS[], WA_SEL_ROWS.

  call method OBJ_ALV_0110->GET_SELECTED_ROWS
    importing
      ET_INDEX_ROWS = IT_SEL_ROWS.

  check IT_SEL_ROWS[] is not initial.

  loop at IT_SEL_ROWS into WA_SEL_ROWS.
    read table IT_SAIDA_0110 assigning <SAIDA_0110> index WA_SEL_ROWS-INDEX.
    if SY-SUBRC ne 0 .
      return.
    endif.

    <SAIDA_0110>-CHECK = P_MARK.
  endloop.

  perform F_ATUALIZA_SALDO.

  perform F_MOEDA_EMPRESA using <SAIDA_0110>-BUKRS
                                'X'.
  if ( SY-SUBRC ne 0 ).
    return.
  endif.

  if P_MARK is not initial.
    if ( ( WA_CABECALHO_0110-SLD_DMBTR > 0             ) and
         ( WA_CABECALHO_0110-WAERS     = TG_T001-WAERS ) )
        or
         ( ( WA_CABECALHO_0110-SLD_DMBE2 > 0     ) and
           ( WA_CABECALHO_0110-WAERS  ne TG_T001-WAERS ) ).

      if WA_SAIDA_0110-WAERS = TG_T001-WAERS.
        VL_MSG_01 = WA_CABECALHO_0110-SLD_DMBTR.
      else.
        VL_MSG_01 = WA_CABECALHO_0110-SLD_DMBE2.
      endif.

      concatenate 'Ainda existe um saldo à compensar( Moeda Documento ) no valor de:'
                   VL_MSG_01 '! Desejar atribuir esse valor como Saldo Residual p/ o Adiantamento?'
              into VL_MSG separated by SPACE.

      call function 'POPUP_TO_CONFIRM'
        exporting
          TITLEBAR              = 'Confirmação'
          TEXT_QUESTION         = VL_MSG
          TEXT_BUTTON_1         = 'Sim'
          TEXT_BUTTON_2         = 'Não'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ''
        importing
          ANSWER                = VAR_ANSWER
        exceptions
          TEXT_NOT_FOUND        = 1
          others                = 2.

      if VAR_ANSWER eq '1'.
        if WA_SAIDA_0110-WAERS = TG_T001-WAERS.
          WA_CABECALHO_0110-RSD_ADT = WA_CABECALHO_0110-SLD_DMBTR.
        else.
          WA_CABECALHO_0110-RSD_ADT = WA_CABECALHO_0110-SLD_DMBE2.
        endif.
      endif.

    endif.
  endif.

  perform F_ATUALIZA_SALDO.
  leave to screen 0110.

endform.

form F_GET_BSID_COMP  tables P_TG_BSID_COMP structure TG_BSID_COMP
                       using I_BUKRS type BSID-BUKRS
                             I_BELNR type BSID-BELNR
                             I_BUZEI type BSID-BUZEI
                             I_GJAHR type BSID-GJAHR
                             I_KUNNR
                             I_VBEL2
                             I_SGTXT
                             I_ZUONR
                             I_ANLN1 type BSID-ANLN1
                             I_ANLN2 type BSID-ANLN2
                             I_DCSIM.

  data: VL_PARTIDA_COMP type C.

  clear: P_TG_BSID_COMP[].

  loop at TG_BSID_COMP where BUKRS = I_BUKRS
                         and ( ( KUNNR = I_KUNNR  and KUNNR is not initial ) or
                               ( VBEL2 = I_VBEL2  and VBEL2 is not initial ) or
                               ( DCSIM = I_DCSIM  and DCSIM is not initial ) or
                               ( ZUONR = I_ZUONR  and ZUONR is not initial )
                             ).

    clear: VL_PARTIDA_COMP.

    "Se não for do mesmo cliente ou
    "           da mesma O.V ou
    "           da mesmo Doc.Simulador
    if ( TG_BSID_COMP-KUNNR ne I_KUNNR ) and
       ( TG_BSID_COMP-VBEL2 ne I_VBEL2 ) and
       ( TG_BSID_COMP-DCSIM ne I_DCSIM ).
      "Verificar se esta unificando por uma atribuição especifica
      check ( P_ZUONRJ eq 'X' ) and ( P_ZUONR-LOW is not initial ).
    endif.

    if ( P_OVPED-LOW is not initial ) or
       ( P_DCSIM-LOW is not initial ) or
       ( P_SGTXT-LOW is not initial ) or
       ( P_ZUONR-LOW is not initial ).
      if ( ( TG_BSID_COMP-VBEL2 eq I_VBEL2 and I_VBEL2     is not initial and P_OVPEDJ eq 'X' and
             TG_BSID_COMP-ANLN1 eq I_ANLN1 and TG_BSID_COMP-ANLN2 eq I_ANLN2                  ) or
           ( TG_BSID_COMP-SGTXT in R_SGTXT and P_SGTXT-LOW is not initial and P_SGTXTJ eq 'X' ) or
           ( TG_BSID_COMP-DCSIM eq I_DCSIM and I_DCSIM     is not initial and P_DCSIMJ eq 'X' ) or
           ( TG_BSID_COMP-ZUONR in R_ZUONR and P_ZUONR-LOW is not initial and P_ZUONRJ eq 'X' ) ).
        VL_PARTIDA_COMP = 'X'.
      endif.
    else.
      if ( ( TG_BSID_COMP-VBEL2 eq I_VBEL2 and I_VBEL2 is not initial and P_OVPEDJ eq 'X' and
             TG_BSID_COMP-ANLN1 eq I_ANLN1 and TG_BSID_COMP-ANLN2 eq I_ANLN2              ) or
           ( TG_BSID_COMP-SGTXT eq I_SGTXT and I_SGTXT is not initial and P_SGTXTJ eq 'X' ) or
           ( TG_BSID_COMP-DCSIM eq I_DCSIM and I_DCSIM is not initial and P_DCSIMJ eq 'X' ) or
           ( TG_BSID_COMP-ZUONR eq I_ZUONR and I_ZUONR is not initial and P_ZUONRJ eq 'X' ) ).
        VL_PARTIDA_COMP = 'X'.
      endif.
    endif.

    check VL_PARTIDA_COMP is not initial.

    append TG_BSID_COMP to P_TG_BSID_COMP.

  endloop.

  delete P_TG_BSID_COMP where BUKRS = I_BUKRS
                          and BELNR = I_BELNR
                          and GJAHR = I_GJAHR
                          and BUZEI = I_BUZEI.


endform.

form F_GET_BSIK_COMP  tables P_TG_BSIK_COMP structure TG_BSIK_COMP
                       using I_BUKRS type BSIK-BUKRS
                             I_BELNR type BSIK-BELNR
                             I_BUZEI type BSIK-BUZEI
                             I_GJAHR type BSIK-GJAHR
                             I_LIFNR
                             I_EBELN
                             I_SGTXT
                             I_ZUONR
                             I_ANLN1 type BSIK-ANLN1
                             I_ANLN2 type BSIK-ANLN2.

  data: VL_PARTIDA_COMP type C.

  clear: P_TG_BSIK_COMP[].

*  LOOP AT tg_bsik_comp WHERE bukrs = i_bukrs
*                         AND ( ( lifnr = i_lifnr  AND lifnr IS NOT INITIAL ) OR
*                               ( ebeln = i_ebeln  AND ebeln IS NOT INITIAL ) OR
*                               ( zuonr = i_zuonr  AND zuonr IS NOT INITIAL )
*                             ).
  loop at TG_BSIK_COMP where BUKRS = I_BUKRS.

    if RB_PROD is initial.
      if TG_BSIK_COMP-EBELN ne I_EBELN and TG_BSIK_COMP-EBELN is not initial and
         TG_BSIK_COMP-LIFNR ne I_LIFNR and TG_BSIK_COMP-LIFNR is not initial and
         TG_BSIK_COMP-ZUONR ne I_ZUONR and TG_BSIK_COMP-ZUONR is not initial.
        continue.
      endif.
    else.
*      IF tg_bsik_comp-lifnr NE i_lifnr OR
*         tg_bsik_comp-zuonr NE i_zuonr.
*        CONTINUE.
*      ENDIF.
      if TG_BSIK_COMP-ZUONR ne I_ZUONR. "agrupa somente   por atribuição as FATURAS
        continue.
      endif.
    endif.

    clear: VL_PARTIDA_COMP.

    "Se não for do mesmo fornecedor ou do mesmo pedido
    if ( TG_BSIK_COMP-LIFNR ne I_LIFNR ) and
       ( TG_BSIK_COMP-EBELN ne I_EBELN ).
      "Verificar se esta unificando por uma atribuição especifica
      check ( P_ZUONRJ eq 'X' ) and ( P_ZUONR-LOW is not initial ).
    endif.

    if ( P_OVPED-LOW is not initial ) or
       ( P_SGTXT-LOW is not initial ) or
       ( P_ZUONR-LOW is not initial ).
      if ( ( TG_BSIK_COMP-EBELN eq I_EBELN and I_EBELN     is not initial and P_OVPEDJ eq 'X' and
             TG_BSIK_COMP-ANLN1 eq I_ANLN1 and TG_BSIK_COMP-ANLN2 eq I_ANLN2                  ) or
           ( TG_BSIK_COMP-SGTXT in R_SGTXT and P_SGTXT-LOW is not initial and P_SGTXTJ eq 'X' ) or
           ( TG_BSIK_COMP-ZUONR in R_ZUONR and P_ZUONR-LOW is not initial and P_ZUONRJ eq 'X' ) ).
        VL_PARTIDA_COMP = 'X'.
      endif.
    else.
      if ( ( TG_BSIK_COMP-EBELN eq I_EBELN and I_EBELN is not initial and P_OVPEDJ eq 'X' and
             TG_BSIK_COMP-ANLN1 eq I_ANLN1 and TG_BSIK_COMP-ANLN2 eq I_ANLN2              ) or
           ( TG_BSIK_COMP-SGTXT eq I_SGTXT and I_SGTXT is not initial and P_SGTXTJ eq 'X' ) or
           ( TG_BSIK_COMP-ZUONR eq I_ZUONR and I_ZUONR is not initial and P_ZUONRJ eq 'X' ) ).
        VL_PARTIDA_COMP = 'X'.
      endif.
    endif.

    check VL_PARTIDA_COMP is not initial.

    append TG_BSIK_COMP to P_TG_BSIK_COMP.

  endloop.

  if RB_PROD is initial.
    delete P_TG_BSIK_COMP where BUKRS = I_BUKRS
                            and BELNR = I_BELNR
                            and GJAHR = I_GJAHR
                            and BUZEI = I_BUZEI.
  else.
    loop at TG_BSIK_COPY where ZUONR = I_ZUONR.
      delete P_TG_BSIK_COMP where BUKRS = TG_BSIK_COPY-BUKRS
                            and   BELNR = TG_BSIK_COPY-BELNR
                            and   GJAHR = TG_BSIK_COPY-GJAHR
                            and   BUZEI = TG_BSIK_COPY-BUZEI.
    endloop.
  endif.

endform.

form F_RANGES_TP_PARTIDA  using P_TP_CONTA.


  clear: R_UMSKS_P, R_UMSKS_P[],
         R_UMSKZ_P, R_UMSKZ_P[],
         R_SHKZG_P, R_SHKZG_P[],
         R_UMSKS_C, R_UMSKS_C[],
         R_UMSKZ_C, R_UMSKZ_C[].

  case P_TP_CONTA.
    when 'K'. "Fornecedor

      if ( P_CTA_RZ is initial ) and ( P_CTA_NR is initial ).

        "Partida Principal
        R_UMSKS_P-SIGN   = 'I'.
        R_UMSKS_P-OPTION = 'NE'.
        R_UMSKS_P-LOW    = ''.
        append R_UMSKS_P.

        R_UMSKZ_P-SIGN   = 'I'.
        R_UMSKZ_P-OPTION = 'NE'.
        R_UMSKZ_P-LOW    = 'F'.
        append R_UMSKZ_P.

        R_SHKZG_P-SIGN   = 'I'.
        R_SHKZG_P-OPTION = 'EQ'.
        R_SHKZG_P-LOW    = 'S'.
        append R_SHKZG_P.

        "Outras Partidas
        R_UMSKS_C-SIGN   = 'I'.
        R_UMSKS_C-OPTION = 'EQ'.
        R_UMSKS_C-LOW    = ''.
        append R_UMSKS_C.

        R_UMSKZ_C-SIGN   = 'I'.
        R_UMSKZ_C-OPTION = 'EQ'.
        R_UMSKZ_C-LOW    = ''.
        append R_UMSKZ_C.

        "Ranges Unificação Partida
        if P_OVPEDJ eq ABAP_TRUE.
          if R_OVPED[] is initial.
            R_OVPED-SIGN   = 'I'.
            R_OVPED-OPTION = 'NE'.
            R_OVPED-LOW    = ''.
            append R_OVPED.
          endif.
        endif.

        if P_SGTXTJ eq ABAP_TRUE.
          if R_SGTXT[] is initial.
            R_SGTXT-SIGN   = 'I'.
            R_SGTXT-OPTION = 'NE'.
            R_SGTXT-LOW    = ''.
            append R_SGTXT.
          endif.
        endif.

        if P_ZUONRJ eq ABAP_TRUE.
          if R_ZUONR[] is initial.
            R_ZUONR-SIGN   = 'I'.
            R_ZUONR-OPTION = 'NE'.
            R_ZUONR-LOW    = ''.
            append R_ZUONR.
          endif.
        endif.

      else.

        if ( P_CTA_RZ is not initial ) and ( P_CTA_NR is initial ).

          "Partida Principal
          R_UMSKS_P-SIGN   = 'I'.
          R_UMSKS_P-OPTION = 'NE'.
          R_UMSKS_P-LOW    = ''.
          append R_UMSKS_P.

          R_UMSKZ_P-SIGN   = 'I'.
          R_UMSKZ_P-OPTION = 'NE'.
          R_UMSKZ_P-LOW    = 'F'.
          append R_UMSKZ_P.

        elseif ( P_CTA_NR is not initial ) and ( P_CTA_RZ is initial ).

          "Partida Principal
          R_UMSKS_P-SIGN   = 'I'.
          R_UMSKS_P-OPTION = 'EQ'.
          R_UMSKS_P-LOW    = ''.
          append R_UMSKS_P.

          R_UMSKZ_P-SIGN   = 'I'.
          R_UMSKZ_P-OPTION = 'EQ'.
          R_UMSKZ_P-LOW    = ''.
          append R_UMSKZ_P.

        endif.

      endif.


    when 'D'. "Ciente

      if ( P_CTA_RZ is initial ) and ( P_CTA_NR is initial ).

        "Partida Principal
        R_UMSKS_P-SIGN   = 'I'.
        R_UMSKS_P-OPTION = 'NE'.
        R_UMSKS_P-LOW    = ''.
        append R_UMSKS_P.

        R_UMSKZ_P-SIGN   = 'I'.
        R_UMSKZ_P-OPTION = 'NE'.
        R_UMSKZ_P-LOW    = 'F'.
        append R_UMSKZ_P.

        R_SHKZG_P-SIGN   = 'I'.
        R_SHKZG_P-OPTION = 'EQ'.
        R_SHKZG_P-LOW    = 'H'.
        append R_SHKZG_P.

        "Outras Partidas
        R_UMSKS_C-SIGN   = 'I'.
        R_UMSKS_C-OPTION = 'EQ'.
        R_UMSKS_C-LOW    = ''.
        append R_UMSKS_C.

        R_UMSKZ_C-SIGN   = 'I'.
        R_UMSKZ_C-OPTION = 'EQ'.
        R_UMSKZ_C-LOW    = ''.
        append R_UMSKZ_C.

        "Ranges Unificação Partida
        if ( P_OVPEDJ eq ABAP_TRUE ) or
           ( P_DCSIMJ eq ABAP_TRUE ).
          if R_OVPED[] is initial.
            R_OVPED-SIGN   = 'I'.
            R_OVPED-OPTION = 'NE'.
            R_OVPED-LOW    = ''.
            append R_OVPED.
          endif.
        endif.

        if P_SGTXTJ eq ABAP_TRUE.
          if R_SGTXT[] is initial.
            R_SGTXT-SIGN   = 'I'.
            R_SGTXT-OPTION = 'NE'.
            R_SGTXT-LOW    = ''.
            append R_SGTXT.
          endif.
        endif.

        if P_ZUONRJ eq ABAP_TRUE.
          if R_ZUONR[] is initial.
            R_ZUONR-SIGN   = 'I'.
            R_ZUONR-OPTION = 'NE'.
            R_ZUONR-LOW    = ''.
            append R_ZUONR.
          endif.
        endif.

        if ( P_DCSIMJ eq ABAP_TRUE ).
          if R_DCSIM[] is initial.
            R_DCSIM-SIGN   = 'I'.
            R_DCSIM-OPTION = 'NE'.
            R_DCSIM-LOW    = ''.
            append R_DCSIM.
          endif.
        endif.

      else.

        if ( P_CTA_RZ is not initial ) and ( P_CTA_NR is initial ).

          "Partida Principal
          R_UMSKS_P-SIGN   = 'I'.
          R_UMSKS_P-OPTION = 'NE'.
          R_UMSKS_P-LOW    = ''.
          append R_UMSKS_P.

          R_UMSKZ_P-SIGN   = 'I'.
          R_UMSKZ_P-OPTION = 'NE'.
          R_UMSKZ_P-LOW    = 'F'.
          append R_UMSKZ_P.

        elseif ( P_CTA_NR is not initial ) and ( P_CTA_RZ is initial ).

          "Partida Principal
          R_UMSKS_P-SIGN   = 'I'.
          R_UMSKS_P-OPTION = 'EQ'.
          R_UMSKS_P-LOW    = ''.
          append R_UMSKS_P.

          R_UMSKZ_P-SIGN   = 'I'.
          R_UMSKZ_P-OPTION = 'EQ'.
          R_UMSKZ_P-LOW    = ''.
          append R_UMSKZ_P.

        endif.

      endif.



  endcase.

endform.

form F_MOEDA_EMPRESA using P_BUKRS type T001-BUKRS
                           P_MSG   type C.

  clear: TG_T001.

  read table TG_T001 with key BUKRS = P_BUKRS.

  if ( SY-SUBRC ne 0 ) or ( TG_T001-WAERS is initial ) or
     ( TG_T001-WAERS2 is initial ) or ( P_BUKRS is initial ).
    clear: TG_T001.
    if P_MSG = 'X'.
      message |Informações referente a moeda da empresa: { P_BUKRS }, não encontrado ou incompleto!| type 'S'.
    endif.
    SY-SUBRC = 4.
  endif.

endform.

form F_APLIC_TEXT_DEF .

  loop at IT_SAIDA_0110 assigning field-symbol(<SAIDA_0110>).
    if <SAIDA_0110>-SGTXT_RSD ne 'Desmembramento Fatura'.
      <SAIDA_0110>-SGTXT_RSD = 'Desmembramento Fatura'.
    else.
      <SAIDA_0110>-SGTXT_RSD = <SAIDA_0110>-SGTXT.
    endif.
  endloop.

  WA_CABECALHO_0110-SGTXT_RSD = WA_SAIDA_0100-SGTXT.

  leave to screen 0110.

endform.


form F_GET_BSIS_CBANCO using P_BUKRS     type bsas_view-BUKRS
                             P_BELNR     type BSAS_view-BELNR
                             P_GJAHR     type BSAS_view-GJAHR
                    changing P_KURSF     like BKPF-KURSF.

  clear: TG_BSIS_CBANCO, P_KURSF.

  read table TG_BSIS_CBANCO with key BUKRS = P_BUKRS
                                     GJAHR = P_GJAHR
                                     BELNR = P_BELNR binary search.
  if ( SY-SUBRC = 0 ) and
     ( TG_BSIS_CBANCO-DMBTR > 0 ) and
     ( TG_BSIS_CBANCO-DMBE2 > 0 ).

    try.
        P_KURSF = TG_BSIS_CBANCO-DMBTR / TG_BSIS_CBANCO-DMBE2.
      catch CX_SY_ARITHMETIC_OVERFLOW.
    endtry.

    if ( TG_BSIS_CBANCO-WAERS is not initial ) and
       ( TG_BSIS_CBANCO-WAERS ne 'BRL'       ) and
       ( TG_BSIS_CBANCO-WAERS ne 'USD'       ) and
       ( TG_BSIS_CBANCO-WRBTR > 0            ).

      try.
          P_KURSF = TG_BSIS_CBANCO-DMBTR / TG_BSIS_CBANCO-WRBTR.
          "P_KURS2 = TG_BSIS_CBANCO-DMBE2 / TG_BSIS_CBANCO-WRBTR.
        catch CX_SY_ARITHMETIC_OVERFLOW.
      endtry.
    endif.
  endif.

endform.

form F_CALL_SCREEN_0120.

  perform F_SELECIONA_DADOS_0120.

  call screen 0120 starting at 2 2 ending at 178 25.

endform.

form F_MONTAR_LAYOUT_LOG_ERRO.
  refresh ESTRUTURA.
  perform F_MONTAR_ESTRUTURA using:
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


form F_MONTAR_ESTRUTURA using value(P_COL_POS)       type I
                              value(P_REF_TABNAME)   like DD02D-TABNAME
                              value(P_REF_FIELDNAME) like DD03D-FIELDNAME
                              value(P_TABNAME)       like DD02D-TABNAME
                              value(P_FIELD)         like DD03D-FIELDNAME
                              value(P_SCRTEXT_L)     like DD03P-SCRTEXT_L
                              value(P_OUTPUTLEN)
                              value(P_HOTSPOT)
                              value(P_JUST).

  clear WA_ESTRUTURA.

  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.
  WA_ESTRUTURA-HOTSPOT       = P_HOTSPOT.
  WA_ESTRUTURA-JUST          = P_JUST.
  WA_ESTRUTURA-DDICTXT       = 'L'.
  WA_ESTRUTURA-OUTPUTLEN     = P_OUTPUTLEN.


  if P_SCRTEXT_L is not initial.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  endif.

  translate  WA_ESTRUTURA-FIELDNAME     to upper case.
  translate  WA_ESTRUTURA-TABNAME       to upper case.
  translate  WA_ESTRUTURA-REF_TABNAME   to upper case.
  translate  WA_ESTRUTURA-REF_FIELDNAME to upper case.

  append WA_ESTRUTURA to ESTRUTURA.

endform.                    " MONTAR_ESTRUTURA

form F_SELECIONA_DADOS_0120 .

  clear: IT_SAIDA_0120[].

  if P_AUGDT-LOW is initial.
    message 'Selecione um data de compensação!' type 'S'.
    exit.
  endif.

  select distinct *
    from ZFIT0139 into table @data(TG_0139)
   where BUKRS in @P_BUKRS
     and AUGDT in @P_AUGDT.

  loop at TG_0139 into data(_WL_0139).
    clear: WA_SAIDA_0120.
    move-corresponding _WL_0139 to WA_SAIDA_0120.

    if ( _WL_0139-BELNR_GER is not initial ) and
       ( _WL_0139-STBLG_GER is initial     ) and "Não foi estornado
       ( _WL_0139-ANULADO   is initial     ).    "Não foi anulado
      WA_SAIDA_0120-ST_CTB = ICON_LED_GREEN.
    elseif ( _WL_0139-BELNR_GER is initial ) and
           ( _WL_0139-STBLG_GER is initial ) and "Não foi estornado
           ( _WL_0139-ANULADO   is initial ) and "Não foi anulado.
           ( _WL_0139-ST_PROC   eq '2'     ).    "Com erro
      WA_SAIDA_0120-ST_CTB = ICON_LED_RED.
    elseif ( _WL_0139-BELNR_GER  is initial ) and
           ( _WL_0139-STBLG_GER  is initial ) and "Não foi estornado
           ( _WL_0139-ANULADO    is initial ) and "Não foi anulado.
           ( _WL_0139-ST_PROC    is initial ).    "Com erro
      WA_SAIDA_0120-ST_CTB = ICON_LED_YELLOW.
    elseif ( _WL_0139-BELNR_GER is not initial ) and
           ( _WL_0139-STBLG_GER is not initial ) and "Não foi estornado
           ( _WL_0139-ANULADO   is initial     ).    "Não foi anulado
      WA_SAIDA_0120-ST_CTB = ICON_LED_INACTIVE.
    endif.

    append WA_SAIDA_0120 to IT_SAIDA_0120.
  endloop.

  sort IT_SAIDA_0120 by ANULADO.

endform.

form F_ADD_PART_RESIDUAL tables P_FTPOST      structure FTPOST
                          using P_SAIDA_0110  type TY_SAIDA_0110
                       changing P_COUNT_FT    type FTPOST-COUNT
                                P_ERROR       type C.

  data: WL_VLRC(16),
        VDATA_VENC(10),
        WL_VLRN        type P decimals 2,
        WA_ZFIT0154    type ZFIT0154.

  clear: P_ERROR.

  if P_SAIDA_0110-ST_COMP  ne '3'.
    if P_SAIDA_0110-VLR_RSD <= 0.
      message 'Valor Residual inconsistente!' type 'S'.
      P_ERROR = 'X'.
      return.
    endif.
  endif.

  if P_SAIDA_0110-KURSF <= 0.
    message 'Taxa para gerar residual não encontrada!' type 'S'.
    P_ERROR = 'X'.
    return.
  endif.

  if P_SAIDA_0110-ST_COMP  ne '3'.
    case P_SAIDA_0110-KOART.
      when 'D'. "Cliente
        if ( P_SAIDA_0110-BSCHL ne '01' ) and
           ( P_SAIDA_0110-BSCHL ne '11' ) and
           ( P_SAIDA_0110-BSCHL ne '09' ) and
           ( P_SAIDA_0110-BSCHL ne '19' ).
          data(_ERRO_CHAVE) = 'X'.
        endif.
      when 'K'. "Fornecedor
        if ( P_SAIDA_0110-BSCHL ne '21' ) and
           ( P_SAIDA_0110-BSCHL ne '31' ) and
           ( P_SAIDA_0110-BSCHL ne '29' ) and
           ( P_SAIDA_0110-BSCHL ne '39' ).
          _ERRO_CHAVE = 'X'.
        endif.
    endcase.
  else.
    select single *
      from LFA1
      into @data(W_LFA1)
      where LIFNR = @P_SAIDA_0110-PARID
      and   VBUND = 'SOCIOS'.

    if SY-SUBRC = 0.
      select single *
               into WA_ZFIT0154
               from ZFIT0154
               where TIPO = 'P'
               and   FG_SOC = 'X'.
    else.
      select single *
         into WA_ZFIT0154
         from ZFIT0154
         where TIPO = 'P'
         and   FG_SOC = ''.
    endif.

    if P_SAIDA_0110-VLR_RSDP < 0.
      P_SAIDA_0110-BSCHL = '40'.
      P_SAIDA_0110-PARID = WA_ZFIT0154-CTA_DESC_CONC.
    else.
      P_SAIDA_0110-BSCHL = '50'.
      P_SAIDA_0110-PARID = WA_ZFIT0154-CTA_DESC_OBTIDO.
    endif.

  endif.

  if _ERRO_CHAVE is not initial.
    message 'Chave Lançamento não configurada para deixar Saldo Residual!' type 'S'.
    P_ERROR = 'X'.
    return.
  endif.

  perform F_MOEDA_EMPRESA using P_SAIDA_0110-BUKRS
                                'X'.
  if ( SY-SUBRC ne 0 ).
    P_ERROR = 'X'.
    return.
  endif.

  if P_SAIDA_0110-ST_COMP  ne '3'.
    if P_SAIDA_0110-WAERS = TG_T001-WAERS.
*      IF ( p_saida_0110-dmbtr + p_saida_0110-vlr_rsd ) NE p_saida_0110-dmbtr_aux.
      if ( P_SAIDA_0110-WRBTR + P_SAIDA_0110-VLR_RSD ) ne P_SAIDA_0110-DMBTR_AUX.
        message 'Saldo Residual inconsistente!' type 'S'.
        P_ERROR = 'X'.
        return.
      endif.
    else.
      if ( P_SAIDA_0110-DMBE2 + P_SAIDA_0110-VLR_RSD ) ne P_SAIDA_0110-DMBE2_AUX.
        message 'Saldo Residual inconsistente!' type 'S'.
        P_ERROR = 'X'.
        return.
      endif.
    endif.
  endif.

  clear: WL_VLRN, WL_VLRC, VDATA_VENC.

  concatenate P_SAIDA_0110-ZFBDT+6(2) P_SAIDA_0110-ZFBDT+4(2) P_SAIDA_0110-ZFBDT(4) into VDATA_VENC separated by '.'.

  add 1 to P_COUNT_FT.

  if P_SAIDA_0110-ST_COMP  ne '3'.
    WL_VLRN = ABS( P_SAIDA_0110-VLR_RSD ).
  else.
    WL_VLRN = ABS( P_SAIDA_0110-VLR_RSDP ).
  endif.

  write: WL_VLRN to WL_VLRC.

  P_FTPOST-STYPE = 'P'.
  P_FTPOST-COUNT = P_COUNT_FT .

  P_FTPOST-FNAM = 'RF05A-NEWBS'.
  P_FTPOST-FVAL =  P_SAIDA_0110-BSCHL.
  append P_FTPOST.

  P_FTPOST-FNAM = 'BSEG-HKONT'.
  P_FTPOST-FVAL = P_SAIDA_0110-PARID.
  append P_FTPOST.

  P_FTPOST-FNAM = 'BSEG-SGTXT'.
  P_FTPOST-FVAL = P_SAIDA_0110-SGTXT_RSD.
  append P_FTPOST.

  P_FTPOST-FNAM = 'BSEG-ZUONR'.
  P_FTPOST-FVAL = P_SAIDA_0110-ZUONR.
  append P_FTPOST.

  if P_SAIDA_0110-EBELP is not initial.
    P_FTPOST-FVAL = P_FTPOST-FVAL && P_SAIDA_0110-EBELP.
  endif.
  append P_FTPOST.

  if P_SAIDA_0110-ST_COMP  ne '3'.
    P_FTPOST-FNAM = 'BSEG-GSBER'.
    P_FTPOST-FVAL = P_SAIDA_0110-GSBER.
    append P_FTPOST.

    P_FTPOST-FNAM = 'BSEG-HZUON'.
    P_FTPOST-FVAL =  P_SAIDA_0110-OVPED.
    append P_FTPOST.

    if P_SAIDA_0110-ZFBDT is not initial.
      P_FTPOST-FNAM = 'BSEG-ZFBDT'.
      P_FTPOST-FVAL = VDATA_VENC.
      append P_FTPOST.

      if ( P_SAIDA_0110-UMSKS is initial ).
        P_FTPOST-FNAM = 'BSEG-ZBD1T'.
        P_FTPOST-FVAL = P_SAIDA_0110-ZBD1T.
        condense P_FTPOST-FVAL no-gaps.
        append P_FTPOST.
      endif.
    endif.
  else.
    P_FTPOST-FNAM = 'BSEG-BUPLA'.
    P_FTPOST-FVAL = P_SAIDA_0110-GSBER.
    append P_FTPOST.
  endif.

  if P_SAIDA_0110-UMSKS is not initial. "Adiantamento
    P_FTPOST-FNAM = 'RF05A-NEWUM'.
    P_FTPOST-FVAL = P_SAIDA_0110-UMSKZ.
    append P_FTPOST.

    if ( P_SAIDA_0110-ANLN1 is not initial ).
      P_FTPOST-FNAM = 'BSEG-ANLN1'.
      P_FTPOST-FVAL = P_SAIDA_0110-ANLN1.
      append P_FTPOST.

      if P_SAIDA_0110-ANLN2 is not initial.
        P_FTPOST-FNAM = 'BSEG-ANLN2'.
        P_FTPOST-FVAL = P_SAIDA_0110-ANLN2.
        append P_FTPOST.
      endif.
    endif.
  endif.

  P_FTPOST-FNAM = 'BSEG-WRBTR'.
  P_FTPOST-FVAL =  WL_VLRC.
  append P_FTPOST.

  if P_SAIDA_0110-WAERS ne TG_T001-WAERS.
    WL_VLRN = WL_VLRN * ABS( P_SAIDA_0110-KURSF ).
    write: WL_VLRN to WL_VLRC.
    P_FTPOST-FNAM = 'BSEG-DMBTR'.
    P_FTPOST-FVAL =  WL_VLRC.
    append P_FTPOST.
  else.
    WL_VLRN = WL_VLRN / ABS( P_SAIDA_0110-KURSF ).
    write: WL_VLRN to WL_VLRC.
    P_FTPOST-FNAM = 'BSEG-DMBE2'.
    P_FTPOST-FVAL =  WL_VLRC.
    append P_FTPOST.
  endif.

endform.

form F_ATRIB_DOC_SIMULADOR  using P_VBELN
                         changing P_DCSIM.

  clear: P_DCSIM.

  check P_VBELN is not initial.

  read table TG_ZSDT0041 with key VBELN = P_VBELN.
  if ( SY-SUBRC eq 0 ) and ( TG_ZSDT0041-DOC_SIMULACAO is not initial ).
    P_DCSIM = TG_ZSDT0041-DOC_SIMULACAO.
    exit.
  endif.

  read table TG_ZSDT0090 with key VBELN = P_VBELN.
  if ( SY-SUBRC eq 0 ) and ( TG_ZSDT0090-DOC_SIMULACAO is not initial ).
    P_DCSIM = TG_ZSDT0090-DOC_SIMULACAO.
    exit.
  endif.

  "Check se é uma Devolução/Recusa
  read table TG_VBFA_RD with key VBELN = P_VBELN.

  check SY-SUBRC eq 0.

  read table TG_ZSDT0041 with key VBELN = TG_VBFA_RD-VBELV.
  if ( SY-SUBRC eq 0 ) and ( TG_ZSDT0041-DOC_SIMULACAO is not initial ).
    P_DCSIM = TG_ZSDT0041-DOC_SIMULACAO.
    exit.
  endif.

  read table TG_ZSDT0090 with key VBELN = TG_VBFA_RD-VBELV.
  if ( SY-SUBRC eq 0 ) and ( TG_ZSDT0090-DOC_SIMULACAO is not initial ).
    P_DCSIM = TG_ZSDT0090-DOC_SIMULACAO.
    exit.
  endif.

endform.

form F_SEL_PART_COMP using P_TP_CONTA.

  case P_TP_CONTA.
    when 'K'. "Fornecedor

      if ( P_OVPED-LOW is not initial ) or
         ( P_SGTXT-LOW is not initial ) or
         ( P_ZUONR-LOW is not initial ).

        select distinct *
          from bsik_view
         where BUKRS in @R_BUKRS
           and UMSKS in @R_UMSKS_C
           and UMSKZ in @R_UMSKZ_C
           and EBELN in @R_OVPED
           and SGTXT in @R_SGTXT
           and ZUONR in @R_ZUONR
           and BLART ne 'VC'
           and DMBTR > 0
           and DMBE2 > 0
          into corresponding fields of table @TG_BSIK_COMP.

      else.

        select distinct *
          from BSIK_view
          for all entries in @TG_BSIK_ADT
         where BUKRS eq @TG_BSIK_ADT-BUKRS
           and ( ( LIFNR eq @TG_BSIK_ADT-LIFNR ) or
                 ( EBELN eq @TG_BSIK_ADT-EBELN and EBELN ne '' ) )
           and UMSKS in @R_UMSKS_C
           and UMSKZ in @R_UMSKZ_C
           and EBELN in @R_OVPED
           and SGTXT in @R_SGTXT
           and ZUONR in @R_ZUONR
           and BLART ne 'VC'
           and ( ( EBELN = @TG_BSIK_ADT-EBELN and EBELN ne '' ) or
                 ( SGTXT = @TG_BSIK_ADT-SGTXT and SGTXT ne '' ) or
                 ( ZUONR = @TG_BSIK_ADT-ZUONR and ZUONR ne '' ) )
           and DMBTR > 0
           and DMBE2 > 0
          into corresponding fields of table @TG_BSIK_COMP.
      endif.

    when 'D'. "Cliente

      if ( P_OVPED-LOW is not initial ) or
         ( P_SGTXT-LOW is not initial ) or
         ( P_ZUONR-LOW is not initial ) or
         ( P_DCSIMJ    eq ABAP_TRUE   ).

        select distinct *
          from bsid_view
         where BUKRS in @R_BUKRS
           and UMSKS in @R_UMSKS_C
           and UMSKZ in @R_UMSKZ_C
           and VBEL2 in @R_OVPED
           and SGTXT in @R_SGTXT
           and ZUONR in @R_ZUONR
           and BLART ne 'VC'
           and DMBTR > 0
           and DMBE2 > 0
          into corresponding fields of table @TG_BSID_COMP.

      else.

        select distinct *
          from bsid_view
          for all entries in @TG_BSID_ADT
         where BUKRS eq @TG_BSID_ADT-BUKRS
           and ( ( KUNNR eq @TG_BSID_ADT-KUNNR ) or
                 ( VBEL2 eq @TG_BSID_ADT-VBEL2 and VBEL2 ne '' ) )
           and UMSKS in @R_UMSKS_C
           and UMSKZ in @R_UMSKZ_C
           and VBEL2 in @R_OVPED
           and SGTXT in @R_SGTXT
           and ZUONR in @R_ZUONR
           and BLART ne 'VC'
           and ( ( VBEL2 = @TG_BSID_ADT-VBEL2 and VBEL2 ne '' ) or
                 ( SGTXT = @TG_BSID_ADT-SGTXT and SGTXT ne '' ) or
                 ( ZUONR = @TG_BSID_ADT-ZUONR and ZUONR ne '' ) )
           and DMBTR > 0
           and DMBE2 > 0
          into corresponding fields of table @TG_BSID_COMP.

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
form F_DESC using P_SAIDA_0100 type TY_SAIDA_0100..
  data WA_ZFIT0154 type ZFIT0154.
  "
  loop at IT_SAIDA_0110 into data(W110) where CHECK is not initial and BELNR is not initial.

  endloop.

  if W110-PARID is initial.
    message 'Selecione partidas a compensar!' type 'I'.
    exit.
  endif.

  select single *
       from LFA1
       into @data(W_LFA1)
       where LIFNR = @W110-PARID
       and   VBUND = 'SOCIOS'.

  if SY-SUBRC = 0.
    select single *
             into WA_ZFIT0154
             from ZFIT0154
             where TIPO = 'P'
             and   FG_SOC = 'X'.
  else.
    select single *
       into WA_ZFIT0154
       from ZFIT0154
       where TIPO = 'P'
       and   FG_SOC = ''.
  endif.

  clear W110.
  loop at IT_SAIDA_0110 into W110 where MANUAL is not initial.

  endloop.
  if W110-MANUAL = 'X'.
    message 'Desconto já incluido!' type 'I'.
    exit.
  endif.



  clear: WA_SAIDA_0110, GT_ESTILO[].

  if WA_CABECALHO_0110-SLD_DMBTR > 0.
    WA_SAIDA_0110-BSCHL = '40'.
    WA_SAIDA_0110-HKONT = WA_ZFIT0154-CTA_DESC_CONC.
  else.
    WA_SAIDA_0110-BSCHL = '50'.
    WA_SAIDA_0110-HKONT = WA_ZFIT0154-CTA_DESC_OBTIDO.
  endif.
  WA_SAIDA_0110-MANUAL = 'X'.
  WA_SAIDA_0110-IC_MANUAL = ICON_CHECKED.
  WA_SAIDA_0110-KURSF  = WA_SAIDA_0100-KURSF.
  WA_SAIDA_0110-KOART  = WA_SAIDA_0100-KOART.
  WA_SAIDA_0110-BUKRS  = WA_SAIDA_0100-BUKRS.
  WA_SAIDA_0110-WAERS  = WA_SAIDA_0100-WAERS.

  WA_SAIDA_0110-DMBTR_AUX  = ABS( WA_CABECALHO_0110-SLD_DMBTR ).
  WA_SAIDA_0110-DMBE2_AUX  = ABS( WA_CABECALHO_0110-SLD_DMBTR / WA_SAIDA_0100-KURSF ).

  WA_SAIDA_0110-DMBTR  = ABS( WA_CABECALHO_0110-SLD_DMBTR ).
  WA_SAIDA_0110-WRBTR  = ABS( WA_CABECALHO_0110-SLD_DMBTR ).
  WA_SAIDA_0110-DMBE2  = ABS( WA_CABECALHO_0110-SLD_DMBTR / WA_SAIDA_0100-KURSF ).
  WA_SAIDA_0110-SGTXT_RSD  = 'Desconto Tolerância de fechamento de lote'.
  WA_SAIDA_0110-SGTXT      = 'Desconto Tolerância de fechamento de lote'.

  WL_ESTILO-FIELDNAME    = 'VLR_RSD'.
  WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
  append WL_ESTILO to GT_ESTILO.

  insert lines of GT_ESTILO into table WA_SAIDA_0110-ESTILO.

  append WA_SAIDA_0110 to IT_SAIDA_0110.

endform.
