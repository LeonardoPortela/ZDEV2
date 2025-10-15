*&---------------------------------------------------------------------*
*&  Include           ZFIR058_FORM
*&---------------------------------------------------------------------*

FORM f_selecionar_dados .

  DATA: wl_x001 TYPE x001.

  PERFORM: f_limpa_variaveis,
           f_config_ranges.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
  FREE: r_xref3.
  r_xref3-sign = 'I'.
  r_xref3-option = 'CP'.
  CONCATENATE '*' p_produ-low
         INTO r_xref3-low.
  APPEND r_xref3.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

  "Seleciona adiantamentos.
  IF rb_cli IS NOT INITIAL.

    PERFORM f_ranges_tp_partida USING 'D'.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    SELECT *
      FROM bsid INTO CORRESPONDING FIELDS OF TABLE tg_bsid_adt
     WHERE bukrs IN r_bukrs
       AND kunnr IN r_parid
       AND umsks IN r_umsks_p
       AND umskz IN r_umskz_p
       AND vbel2 IN r_ovped
       AND sgtxt IN r_sgtxt
       AND zuonr IN r_zuonr.
*      AND blart NE 'VC'
*      AND ( vbel2 NE '' OR sgtxt NE '' OR zuonr NE '' )
*      AND shkzg IN r_shkzg_p.

    DELETE tg_bsid_adt WHERE NOT ( blart NE 'VC'
                             AND ( vbel2 NE '' OR sgtxt NE '' OR zuonr NE '' )
                             AND   shkzg IN r_shkzg_p ).
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    IF tg_bsid_adt[] IS INITIAL.
      MESSAGE 'Nenhum registro encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsid_adt_aux[] =  tg_bsid_adt[].
    SORT tg_bsid_adt_aux BY bukrs belnr.
    DELETE ADJACENT DUPLICATES FROM tg_bsid_adt_aux
                          COMPARING bukrs belnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    DATA etl57c4r657 TYPE TABLE OF bseg.
    CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      EXPORTING
        it_for_all_entries = tg_bsid_adt_aux[]
        i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR|
      IMPORTING
        et_bseg            = etl57c4r657
      EXCEPTIONS
        not_found          = 1.
    IF sy-subrc = 0 AND lines( etl57c4r657 ) > 0.
      MOVE-CORRESPONDING etl57c4r657 TO tg_bseg[] KEEPING TARGET LINES.
      sy-dbcnt = lines( etl57c4r657 ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.


* PANF - performance
****-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
***    SELECT bsis~bukrs bsis~belnr bsis~gjahr bsis~hkont bsis~dmbtr bsis~dmbe2
***           bsis~waers bsis~wrbtr skb1~fdlev
***           APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_cbanco
***      FROM bsis INNER JOIN skb1 ON bsis~bukrs = skb1~bukrs "#EC CI_DB_OPERATION_OK[2431747]
***                               AND bsis~hkont = skb1~saknr
***       FOR ALL ENTRIES IN tg_bsid_adt
***     WHERE bsis~bukrs EQ tg_bsid_adt-bukrs
***       AND bsis~gjahr EQ tg_bsid_adt-gjahr
***       AND bsis~belnr EQ tg_bsid_adt-belnr.
****      AND skb1~fdlev IN r_fdlev_banco.
***
***    DELETE tg_bsis_cbanco WHERE NOT ( fdlev IN r_fdlev_banco[] ).
****-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio

    SELECT bsis~bukrs bsis~belnr bsis~gjahr bsis~hkont bsis~dmbtr bsis~dmbe2
           bsis~waers bsis~wrbtr
           APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_cbanco
      FROM bsis
       FOR ALL ENTRIES IN tg_bsid_adt
      WHERE bsis~bukrs EQ tg_bsid_adt-bukrs
       AND bsis~gjahr EQ tg_bsid_adt-gjahr
       AND bsis~belnr EQ tg_bsid_adt-belnr.

    SELECT bukrs, saknr, fdlev
    FROM skb1
      INTO TABLE @DATA(lt_skb1)
      FOR ALL ENTRIES IN @tg_bsis_cbanco
      WHERE skb1~bukrs = @tg_bsis_cbanco-bukrs
        AND skb1~saknr = @tg_bsis_cbanco-hkont.

    DELETE lt_skb1 WHERE NOT ( fdlev IN r_fdlev_banco[] ).
    SORT: lt_skb1 BY bukrs saknr,
          tg_bsis_cbanco BY bukrs belnr gjahr hkont.

    LOOP AT tg_bsis_cbanco ASSIGNING FIELD-SYMBOL(<fs_bsis_cbanco>).
      READ TABLE lt_skb1 ASSIGNING FIELD-SYMBOL(<fs_skb1>)
                         WITH KEY bukrs = <fs_bsis_cbanco>-bukrs
                                  saknr = <fs_bsis_cbanco>-hkont
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_bsis_cbanco>-fdlev = <fs_skb1>-fdlev.
      ENDIF.
    ENDLOOP.

    DELETE tg_bsis_cbanco WHERE fdlev IS INITIAL.
* PANF - performance


    SELECT *
      FROM t001 INTO CORRESPONDING FIELDS OF TABLE tg_t001
       FOR ALL ENTRIES IN tg_bsid_adt
     WHERE bukrs = tg_bsid_adt-bukrs.

    LOOP AT tg_bsid_adt.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = tg_bsid_adt-vbel2
        IMPORTING
          output = tg_bsid_adt-vbel2.

      MODIFY tg_bsid_adt.
    ENDLOOP.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsid_adt_aux[] =  tg_bsid_adt[].
    SORT tg_bsid_adt_aux BY vbel2.
    DELETE ADJACENT DUPLICATES FROM tg_bsid_adt_aux
                          COMPARING vbel2.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    SELECT *
      FROM vbak INTO CORRESPONDING FIELDS OF TABLE tg_vbak
      FOR ALL ENTRIES IN tg_bsid_adt_aux
     WHERE vbeln = tg_bsid_adt_aux-vbel2.

    IF tg_vbak[] IS NOT INITIAL.
      SELECT *
        FROM tspat INTO CORRESPONDING FIELDS OF TABLE tg_tspat
         FOR ALL ENTRIES IN tg_vbak
       WHERE spras = sy-langu
         AND spart = tg_vbak-spart.
    ENDIF.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsid_adt_aux[] =  tg_bsid_adt[].
    SORT tg_bsid_adt_aux BY bukrs belnr gjahr.
    DELETE ADJACENT DUPLICATES FROM tg_bsid_adt_aux
                          COMPARING bukrs belnr gjahr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    "Seleciona Cabeçalhos
    SELECT *
     FROM bkpf INTO CORRESPONDING FIELDS OF TABLE tg_bkpf
     FOR ALL ENTRIES IN tg_bsid_adt_aux
    WHERE bukrs EQ tg_bsid_adt_aux-bukrs
      AND belnr EQ tg_bsid_adt_aux-belnr
      AND gjahr EQ tg_bsid_adt_aux-gjahr.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsid_adt_aux[] =  tg_bsid_adt[].
    SORT tg_bsid_adt_aux BY kunnr.
    DELETE ADJACENT DUPLICATES FROM tg_bsid_adt_aux
                          COMPARING kunnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    SELECT *
      FROM kna1 INTO CORRESPONDING FIELDS OF TABLE tg_kna1
       FOR ALL ENTRIES IN tg_bsid_adt_aux
     WHERE kunnr = tg_bsid_adt_aux-kunnr.

    "Seleciona Partidas Compensação.
    PERFORM f_sel_part_comp USING 'D'.

    IF tg_bsid_comp[] IS NOT INITIAL.
      "Seleciona Cabeçalhos

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsid_comp_aux2[] =  tg_bsid_comp[].
      SORT tg_bsid_comp_aux2 BY bukrs belnr gjahr.
      DELETE ADJACENT DUPLICATES FROM tg_bsid_comp_aux2
                            COMPARING bukrs belnr gjahr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      SELECT *
       FROM bkpf APPENDING CORRESPONDING FIELDS OF TABLE tg_bkpf
       FOR ALL ENTRIES IN tg_bsid_comp_aux2
      WHERE bukrs EQ tg_bsid_comp_aux2-bukrs
        AND belnr EQ tg_bsid_comp_aux2-belnr
        AND gjahr EQ tg_bsid_comp_aux2-gjahr.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsid_comp_aux2[] =  tg_bsid_comp[].
      SORT tg_bsid_comp_aux2 BY kunnr.
      DELETE ADJACENT DUPLICATES FROM tg_bsid_comp_aux2
                            COMPARING kunnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      SELECT *
        FROM kna1 APPENDING CORRESPONDING FIELDS OF TABLE tg_kna1
         FOR ALL ENTRIES IN tg_bsid_comp_aux2
       WHERE kunnr = tg_bsid_comp_aux2-kunnr.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsid_comp_aux2[] =  tg_bsid_comp[].
      SORT tg_bsid_comp_aux2 BY bukrs belnr.
      DELETE ADJACENT DUPLICATES FROM tg_bsid_comp_aux2
                            COMPARING bukrs belnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      DATA etl180c6r6595 TYPE TABLE OF bseg.
      CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
        EXPORTING
          it_for_all_entries = tg_bsid_comp_aux2[]
          i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR|
        IMPORTING
          et_bseg            = etl180c6r6595
        EXCEPTIONS
          not_found          = 1.
      IF sy-subrc = 0 AND lines( etl180c6r6595 ) > 0.
        MOVE-CORRESPONDING etl180c6r6595 TO tg_bseg[] KEEPING TARGET LINES.
        sy-dbcnt = lines( etl180c6r6595 ).
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.


*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsid_comp_aux2[] =  tg_bsid_comp[].
      SORT tg_bsid_comp_aux2 BY vbel2.
      DELETE ADJACENT DUPLICATES FROM tg_bsid_comp_aux2
                            COMPARING vbel2.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      SELECT *
        FROM vbak APPENDING CORRESPONDING FIELDS OF TABLE tg_vbak
         FOR ALL ENTRIES IN tg_bsid_comp_aux2
       WHERE vbeln EQ tg_bsid_comp_aux2-vbel2.

    ENDIF.

    IF tg_vbak[] IS NOT INITIAL.

      "Recusa e Devolução
      SELECT *
        FROM vbfa APPENDING CORRESPONDING FIELDS OF TABLE tg_vbfa_rd
         FOR ALL ENTRIES IN tg_vbak
       WHERE vbeln    EQ tg_vbak-vbeln
         AND vbtyp_n  IN ('H','L', 'C')
         AND vbtyp_v  EQ 'C'.

      IF tg_vbfa_rd[] IS NOT INITIAL.
        SELECT *
          FROM vbak APPENDING CORRESPONDING FIELDS OF TABLE tg_vbak
           FOR ALL ENTRIES IN tg_vbfa_rd
         WHERE vbeln EQ tg_vbfa_rd-vbelv.
      ENDIF.

      SELECT *
        FROM zsdt0041 APPENDING CORRESPONDING FIELDS OF TABLE tg_zsdt0041
         FOR ALL ENTRIES IN tg_vbak
       WHERE vbeln         EQ tg_vbak-vbeln
         AND doc_simulacao NE '0000000000'.

      SELECT *
        FROM zsdt0090 APPENDING CORRESPONDING FIELDS OF TABLE tg_zsdt0090
         FOR ALL ENTRIES IN tg_vbak
       WHERE vbeln         EQ tg_vbak-vbeln
         AND doc_simulacao NE '0000000000'
         AND estorno       NE 'X'.

    ENDIF.

    LOOP AT tg_bsid_adt.

      "Atualização Dados Imobilizado
      IF tg_bsid_adt-anln1 IS INITIAL.
        LOOP AT tg_bseg WHERE bukrs = tg_bsid_adt-bukrs
                          AND belnr = tg_bsid_adt-belnr
                          AND anln1 IS NOT INITIAL.
          tg_bsid_adt-anln1  = tg_bseg-anln1.
          tg_bsid_adt-anln2  = tg_bseg-anln2.
          EXIT.
        ENDLOOP.
      ENDIF.

      "Atribuir Documento Simulador
      PERFORM f_atrib_doc_simulador USING tg_bsid_adt-vbel2
                                 CHANGING tg_bsid_adt-dcsim.

      MODIFY tg_bsid_adt.

    ENDLOOP.

    LOOP AT tg_bsid_comp.

      "Atualização Dados Imobilizado
      IF tg_bsid_comp-anln1 IS INITIAL.
        LOOP AT tg_bseg WHERE bukrs = tg_bsid_comp-bukrs
                          AND belnr = tg_bsid_comp-belnr
                          AND anln1 IS NOT INITIAL.
          tg_bsid_comp-anln1  = tg_bseg-anln1.
          tg_bsid_comp-anln2  = tg_bseg-anln2.
          EXIT.
        ENDLOOP.
      ENDIF.

      "Atribuir Documento Simulador
      PERFORM f_atrib_doc_simulador USING tg_bsid_comp-vbel2
                                 CHANGING tg_bsid_comp-dcsim.

      MODIFY tg_bsid_comp.
    ENDLOOP.

    IF r_dcsim[] IS NOT INITIAL.
      DELETE tg_bsid_adt  WHERE dcsim NOT IN r_dcsim.
      DELETE tg_bsid_comp WHERE dcsim NOT IN r_dcsim.
    ENDIF.

  ENDIF.

  IF rb_forn IS NOT INITIAL OR rb_prod IS NOT INITIAL.

    PERFORM f_ranges_tp_partida USING 'K'.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    SELECT *
      FROM bsik INTO CORRESPONDING FIELDS OF TABLE tg_bsik_adt
     WHERE bukrs IN r_bukrs
       AND lifnr IN r_parid
       AND umsks IN r_umsks_p
       AND umskz IN r_umskz_p
       AND ebeln IN r_ovped
       AND sgtxt IN r_sgtxt
       AND zuonr IN r_zuonr.
*      AND XREF1 IN P_SAFRA.
*      AND XREF3 LIKE P_PRODU-LOW
*      AND BLART NE 'VC'
*      AND ( EBELN NE '' OR SGTXT NE '' OR ZUONR NE '' )
*      AND SHKZG IN R_SHKZG_P.

    DELETE tg_bsik_adt WHERE NOT ( xref1 IN p_safra[]
                             AND   xref3 IN r_xref3[]
                             AND   blart NE 'VC'
                             AND ( ebeln NE '' OR sgtxt NE '' OR zuonr NE '' )
                             AND   shkzg IN r_shkzg_p[] ).
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    IF rb_prod IS NOT INITIAL.
      SELECT *
          FROM bsik INTO CORRESPONDING FIELDS OF TABLE tg_bsik_adt
         WHERE bukrs IN r_bukrs
           AND lifnr IN r_parid
           AND ebeln IN r_ovped
           AND sgtxt IN r_sgtxt
           AND zuonr IN r_zuonr
           AND xref1 IN p_safra
*           AND xref3 in r_xref3
           AND umskz NE 'Z'
           AND blart IN ( 'MA', 'MB' )
           AND ( ebeln NE '' OR sgtxt NE '' OR zuonr NE '' ).

      DELETE tg_bsik_adt WHERE NOT ( xref1 IN p_safra[]
                          AND   xref3 IN r_xref3[] ).

    ENDIF.

    IF tg_bsik_adt[] IS INITIAL.
      MESSAGE 'Nenhum registro encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsik_adt_aux[] =  tg_bsik_adt[].
    SORT tg_bsik_adt_aux BY bukrs belnr.
    DELETE ADJACENT DUPLICATES FROM tg_bsik_adt_aux
                          COMPARING bukrs belnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    DATA etl338c4r2664 TYPE TABLE OF bseg.
    CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      EXPORTING
        it_for_all_entries = tg_bsik_adt_aux[]
        i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR|
      IMPORTING
        et_bseg            = etl338c4r2664
      EXCEPTIONS
        not_found          = 1.
    IF sy-subrc = 0 AND lines( etl338c4r2664 ) > 0.
      MOVE-CORRESPONDING etl338c4r2664 TO tg_bseg[] KEEPING TARGET LINES.
      sy-dbcnt = lines( etl338c4r2664 ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.


    LOOP AT tg_bsik_adt WHERE ebeln IS NOT INITIAL
                          AND ebelp IS INITIAL.
      CLEAR: tg_ekpo_aux[].
      LOOP AT tg_bseg WHERE bukrs EQ tg_bsik_adt-bukrs
                        AND belnr EQ tg_bsik_adt-belnr
                        AND ebeln EQ tg_bsik_adt-ebeln
                        AND ebelp IS NOT INITIAL.
        tg_ekpo_aux-ebeln = tg_bseg-ebeln.
        tg_ekpo_aux-ebelp = tg_bseg-ebelp.
        APPEND tg_ekpo_aux.
      ENDLOOP.

      SORT tg_ekpo_aux BY ebeln ebelp.
      DELETE ADJACENT DUPLICATES FROM tg_ekpo_aux COMPARING ebeln ebelp.
      READ TABLE tg_ekpo_aux INDEX 1.

      IF ( lines( tg_ekpo_aux[] ) = 1 ).
        tg_bsik_adt-ebelp = tg_ekpo_aux-ebelp.
      ELSEIF ( lines( tg_ekpo_aux[] ) = 0 ).
        tg_bsik_adt-ebelp = '00010'.
      ENDIF.
      MODIFY tg_bsik_adt.
    ENDLOOP.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsik_adt_aux[] =  tg_bsik_adt[].
    SORT tg_bsik_adt_aux BY ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM tg_bsik_adt_aux
                          COMPARING ebeln ebelp.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    SELECT *
      FROM ekkn APPENDING CORRESPONDING FIELDS OF TABLE tg_ekkn
       FOR ALL ENTRIES IN tg_bsik_adt_aux
     WHERE ebeln = tg_bsik_adt_aux-ebeln
       AND ebelp = tg_bsik_adt_aux-ebelp.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
***    SELECT bsis~bukrs bsis~belnr bsis~gjahr bsis~hkont bsis~dmbtr bsis~dmbe2
***           bsis~waers bsis~wrbtr skb1~fdlev
***           APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_cbanco
***      FROM bsis INNER JOIN skb1 ON bsis~bukrs = skb1~bukrs "#EC CI_DB_OPERATION_OK[2431747]
***                               AND bsis~hkont = skb1~saknr
***       FOR ALL ENTRIES IN tg_bsik_adt
***     WHERE bsis~bukrs EQ tg_bsik_adt-bukrs
***       AND bsis~gjahr EQ tg_bsik_adt-gjahr
***       AND bsis~belnr EQ tg_bsik_adt-belnr.
****      AND skb1~fdlev IN r_fdlev_banco.

**       DELETE tg_bsis_cbanco WHERE NOT ( fdlev IN r_fdlev_banco[] ).

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

* PANF - performance
    SELECT bsis~bukrs bsis~belnr bsis~gjahr bsis~hkont bsis~dmbtr bsis~dmbe2
           bsis~waers bsis~wrbtr
           APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_cbanco
      FROM bsis
       FOR ALL ENTRIES IN tg_bsik_adt
      WHERE bsis~bukrs EQ tg_bsik_adt-bukrs
       AND bsis~gjahr EQ tg_bsik_adt-gjahr
       AND bsis~belnr EQ tg_bsik_adt-belnr.

    SELECT bukrs, saknr, fdlev
    FROM skb1
      INTO TABLE @DATA(lt_skb1_a)
      FOR ALL ENTRIES IN @tg_bsis_cbanco
      WHERE skb1~bukrs = @tg_bsis_cbanco-bukrs
        AND skb1~saknr = @tg_bsis_cbanco-hkont.

    DELETE lt_skb1_a WHERE NOT ( fdlev IN r_fdlev_banco[] ).
    SORT: lt_skb1_a BY bukrs saknr,
          tg_bsis_cbanco BY bukrs belnr gjahr hkont.

    LOOP AT tg_bsis_cbanco ASSIGNING FIELD-SYMBOL(<fs_bsis_cbanco_a>).
      READ TABLE lt_skb1_a ASSIGNING FIELD-SYMBOL(<fs_skb1_a>)
                         WITH KEY bukrs = <fs_bsis_cbanco_a>-bukrs
                                  saknr = <fs_bsis_cbanco_a>-hkont
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_bsis_cbanco_a>-fdlev = <fs_skb1_a>-fdlev.
      ENDIF.
    ENDLOOP.

    DELETE tg_bsis_cbanco WHERE fdlev IS INITIAL.
* PANF - performance


    SELECT *
      FROM t001 INTO CORRESPONDING FIELDS OF TABLE tg_t001
       FOR ALL ENTRIES IN tg_bsik_adt
     WHERE bukrs = tg_bsik_adt-bukrs.

    LOOP AT tg_bsik_adt ASSIGNING FIELD-SYMBOL(<fs_bsik_adt>).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_bsik_adt>-ebeln
        IMPORTING
          output = <fs_bsik_adt>-ebeln.
*      MODIFY tg_bsik_adt.
    ENDLOOP.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsik_adt_aux[] =  tg_bsik_adt[].
    SORT tg_bsik_adt_aux BY ebeln.
    DELETE ADJACENT DUPLICATES FROM tg_bsik_adt_aux
                          COMPARING ebeln.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    SELECT *
      FROM ekko INTO CORRESPONDING FIELDS OF TABLE tg_ekko
       FOR ALL ENTRIES IN tg_bsik_adt_aux
     WHERE ebeln = tg_bsik_adt_aux-ebeln.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsik_adt_aux[] =  tg_bsik_adt[].
    SORT tg_bsik_adt_aux BY bukrs belnr gjahr.
    DELETE ADJACENT DUPLICATES FROM tg_bsik_adt_aux
                          COMPARING bukrs belnr gjahr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    "Seleciona Cabeçalhos
    SELECT *
     FROM bkpf INTO CORRESPONDING FIELDS OF TABLE tg_bkpf
     FOR ALL ENTRIES IN tg_bsik_adt_aux
    WHERE bukrs EQ tg_bsik_adt_aux-bukrs
      AND belnr EQ tg_bsik_adt_aux-belnr
      AND gjahr EQ tg_bsik_adt_aux-gjahr.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
    tg_bsik_adt_aux[] =  tg_bsik_adt[].
    SORT tg_bsik_adt_aux BY lifnr.
    DELETE ADJACENT DUPLICATES FROM tg_bsik_adt_aux
                          COMPARING lifnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

    SELECT *
      FROM lfa1 INTO CORRESPONDING FIELDS OF TABLE tg_lfa1
       FOR ALL ENTRIES IN tg_bsik_adt_aux
     WHERE lifnr = tg_bsik_adt_aux-lifnr.

    "Seleciona Partidas Compensação.
    PERFORM f_sel_part_comp USING 'K'.

    IF tg_bsik_comp[] IS NOT INITIAL.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsik_comp_aux2[] =  tg_bsik_comp[].
      SORT tg_bsik_comp_aux2 BY bukrs belnr gjahr.
      DELETE ADJACENT DUPLICATES FROM tg_bsik_comp_aux2
                            COMPARING bukrs belnr gjahr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      "Seleciona Cabeçalhos
      SELECT *
       FROM bkpf APPENDING CORRESPONDING FIELDS OF TABLE tg_bkpf
       FOR ALL ENTRIES IN tg_bsik_comp_aux2
      WHERE bukrs EQ tg_bsik_comp_aux2-bukrs
        AND belnr EQ tg_bsik_comp_aux2-belnr
        AND gjahr EQ tg_bsik_comp_aux2-gjahr.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsik_comp_aux2[] =  tg_bsik_comp[].
      SORT tg_bsik_comp_aux2 BY lifnr.
      DELETE ADJACENT DUPLICATES FROM tg_bsik_comp_aux2
                            COMPARING lifnr.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      SELECT *
        FROM lfa1 APPENDING CORRESPONDING FIELDS OF TABLE tg_lfa1
         FOR ALL ENTRIES IN tg_bsik_comp_aux2
       WHERE lifnr = tg_bsik_comp_aux2-lifnr.

*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsik_comp_aux2[] =  tg_bsik_comp[].
      SORT tg_bsik_comp_aux2 BY bukrs belnr.
      DELETE ADJACENT DUPLICATES FROM tg_bsik_comp_aux2
                            COMPARING bukrs belnr..
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      DATA etl489c6r950 TYPE TABLE OF bseg.
      CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
        EXPORTING
          it_for_all_entries = tg_bsik_comp_aux2[]
          i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR|
        IMPORTING
          et_bseg            = etl489c6r950
        EXCEPTIONS
          not_found          = 1.
      IF sy-subrc = 0 AND lines( etl489c6r950 ) > 0.
        MOVE-CORRESPONDING etl489c6r950 TO tg_bseg[] KEEPING TARGET LINES.
        sy-dbcnt = lines( etl489c6r950 ).
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.


*-CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
      tg_bsik_comp_aux2[] =  tg_bsik_comp[].
      SORT tg_bsik_comp_aux2 BY ebeln ebelp.
      DELETE ADJACENT DUPLICATES FROM tg_bsik_comp_aux2
                            COMPARING ebeln ebelp.
*-CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

      SELECT *
        FROM ekkn APPENDING CORRESPONDING FIELDS OF TABLE tg_ekkn
         FOR ALL ENTRIES IN tg_bsik_comp_aux2
       WHERE ebeln = tg_bsik_comp_aux2-ebeln
         AND ebelp = tg_bsik_comp_aux2-ebelp.

      LOOP AT tg_bsik_comp WHERE ebeln IS NOT INITIAL
                             AND ebelp IS INITIAL.
        CLEAR: tg_ekpo_aux[].
        LOOP AT tg_bseg WHERE bukrs EQ tg_bsik_comp-bukrs
                          AND belnr EQ tg_bsik_comp-belnr
                          AND ebeln EQ tg_bsik_comp-ebeln
                          AND ebelp IS NOT INITIAL.
          tg_ekpo_aux-ebeln = tg_bseg-ebeln.
          tg_ekpo_aux-ebelp = tg_bseg-ebelp.
          APPEND tg_ekpo_aux.
        ENDLOOP.

        SORT tg_ekpo_aux BY ebeln ebelp.
        DELETE ADJACENT DUPLICATES FROM tg_ekpo_aux COMPARING ebeln ebelp.
        READ TABLE tg_ekpo_aux INDEX 1.

        IF ( lines( tg_ekpo_aux[] ) = 1 ).
          tg_bsik_comp-ebelp = tg_ekpo_aux-ebelp.
        ELSEIF ( lines( tg_ekpo_aux[] ) = 0 ).
          tg_bsik_comp-ebelp = '00010'.
        ENDIF.
        MODIFY tg_bsik_comp.
      ENDLOOP.
    ENDIF.

    "Atualização Dados Imobilizado
    LOOP AT tg_bsik_adt.
      IF tg_bsik_adt-anln1 IS INITIAL.
        LOOP AT tg_bseg WHERE bukrs = tg_bsik_adt-bukrs
                          AND belnr = tg_bsik_adt-belnr
                          AND anln1 IS NOT INITIAL.
          tg_bsik_adt-anln1  = tg_bseg-anln1.
          tg_bsik_adt-anln2  = tg_bseg-anln2.
          MODIFY tg_bsik_adt.
          EXIT.
        ENDLOOP.
      ENDIF.

      "Busca Classificação Contabil
      IF tg_bsik_adt-anln1 IS INITIAL.
        LOOP AT tg_ekkn WHERE ebeln = tg_bsik_adt-ebeln
                          AND ebelp = tg_bsik_adt-ebelp
                          AND anln1 IS NOT INITIAL.
          tg_bsik_adt-anln1  = tg_ekkn-anln1.
          tg_bsik_adt-anln2  = tg_ekkn-anln2.
          MODIFY tg_bsik_adt.
          EXIT.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    LOOP AT tg_bsik_comp.
      IF tg_bsik_comp-anln1 IS INITIAL.
        LOOP AT tg_bseg WHERE bukrs = tg_bsik_comp-bukrs
                          AND belnr = tg_bsik_comp-belnr
                          AND anln1 IS NOT INITIAL.
          tg_bsik_comp-anln1  = tg_bseg-anln1.
          tg_bsik_comp-anln2  = tg_bseg-anln2.
          MODIFY tg_bsik_comp.
          EXIT.
        ENDLOOP.
      ENDIF.

      "Busca Classificação Contabil
      IF tg_bsik_comp-anln1 IS INITIAL.
        LOOP AT tg_ekkn WHERE ebeln = tg_bsik_comp-ebeln
                          AND ebelp = tg_bsik_comp-ebelp
                          AND anln1 IS NOT INITIAL.
          tg_bsik_comp-anln1  = tg_ekkn-anln1.
          tg_bsik_comp-anln2  = tg_ekkn-anln2.
          MODIFY tg_bsik_comp.
          EXIT.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDIF.

  LOOP AT tg_t001.
    CLEAR: wl_x001.

    CALL FUNCTION 'FI_CURRENCY_INFORMATION'
      EXPORTING
        i_bukrs = tg_t001-bukrs
      IMPORTING
        e_x001  = wl_x001.

    tg_t001-waers2 = wl_x001-hwae2.

    MODIFY tg_t001.
  ENDLOOP.

  SORT tg_bsis_cbanco BY bukrs gjahr belnr.

ENDFORM.

FORM f_processa_dados .
  DATA wa_zfit0154 TYPE zfit0154.
  LOOP AT tg_bsid_adt.
    CLEAR: wa_saida_0100, tg_kna1, tg_vbak, tg_tspat.

    PERFORM f_moeda_empresa USING tg_bsid_adt-bukrs
                                  'X'.
    IF ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.

    READ TABLE tg_bkpf WITH KEY bukrs = tg_bsid_adt-bukrs
                                belnr = tg_bsid_adt-belnr
                                gjahr = tg_bsid_adt-gjahr.
    CHECK sy-subrc = 0.

    READ TABLE tg_kna1 WITH KEY kunnr = tg_bsid_adt-kunnr.
    CHECK sy-subrc = 0.

    READ TABLE tg_vbak WITH KEY vbeln = tg_bsid_adt-vbel2.
    IF sy-subrc = 0.
      READ TABLE tg_tspat WITH KEY spart = tg_vbak-spart.
      IF sy-subrc = 0.
        wa_saida_0100-spart = tg_vbak-spart.  "Setor Atividade
        wa_saida_0100-vtext = tg_tspat-vtext. "Descr. Setor Atividade
      ENDIF.
    ENDIF.


    wa_saida_0100-bukrs     = tg_bsid_adt-bukrs.
    wa_saida_0100-parid     = tg_kna1-kunnr.
    wa_saida_0100-name1     = tg_kna1-name1.
    wa_saida_0100-belnr     = tg_bsid_adt-belnr.
    wa_saida_0100-buzei     = tg_bsid_adt-buzei.
    wa_saida_0100-gjahr     = tg_bsid_adt-gjahr.
    wa_saida_0100-bldat     = tg_bsid_adt-bldat.
    wa_saida_0100-budat     = tg_bsid_adt-budat.
    wa_saida_0100-waers     = tg_bsid_adt-waers.
    wa_saida_0100-dmbtr     = tg_bsid_adt-dmbtr.
    wa_saida_0100-dmbe2     = tg_bsid_adt-dmbe2.
    wa_saida_0100-dmbtr_aux = tg_bsid_adt-dmbtr.
    wa_saida_0100-dmbe2_aux = tg_bsid_adt-dmbe2.
    wa_saida_0100-hkont     = tg_bsid_adt-hkont.
    wa_saida_0100-bschl     = tg_bsid_adt-bschl.
    wa_saida_0100-umsks     = tg_bsid_adt-umsks.
    wa_saida_0100-umskz     = tg_bsid_adt-umskz.
    wa_saida_0100-shkzg     = tg_bsid_adt-shkzg.
    wa_saida_0100-gsber     = tg_bsid_adt-gsber.
    wa_saida_0100-sgtxt     = tg_bsid_adt-sgtxt.
    wa_saida_0100-zfbdt     = tg_bsid_adt-zfbdt.
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

    PERFORM f_get_taxa USING tg_bkpf
                             wa_saida_0100-dmbtr
                             wa_saida_0100-dmbe2
                    CHANGING wa_saida_0100-kursf.

    PERFORM f_get_bsid_comp TABLES tg_bsid_comp_aux
                             USING tg_bsid_adt-bukrs
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

    LOOP AT tg_bsid_comp_aux.
      ADD 1 TO wa_saida_0100-qtde_ft.
      IF ( tg_bsid_comp_aux-shkzg NE wa_saida_0100-shkzg ). "Se for operação(Cred.Deb.) diferente da partida principal
        ADD tg_bsid_comp_aux-dmbtr TO wa_saida_0100-ft_dmbtr.
        ADD tg_bsid_comp_aux-dmbe2 TO wa_saida_0100-ft_dmbe2.
      ELSE.
        SUBTRACT tg_bsid_comp_aux-dmbtr FROM wa_saida_0100-ft_dmbtr.
        SUBTRACT tg_bsid_comp_aux-dmbe2 FROM wa_saida_0100-ft_dmbe2.
      ENDIF.
    ENDLOOP.

    wa_saida_0100-df_dmbe2 = wa_saida_0100-ft_dmbtr - wa_saida_0100-dmbtr.

    wa_saida_0100-comp = icon_light_out.

    IF ( ( wa_saida_0100-ft_dmbtr = wa_saida_0100-dmbtr ) AND
         ( wa_saida_0100-waers    = tg_t001-waers       ) )
          OR
       ( ( wa_saida_0100-ft_dmbe2 = wa_saida_0100-dmbe2 ) AND
         ( wa_saida_0100-waers    = tg_t001-waers2      ) ).
      "Liberado Compensação ( Total Adiamento = Total Partidas Compensar )
      wa_saida_0100-st_comp  = '1'.
      wa_saida_0100-comp     = icon_execute_object.
    ELSE."IF ( WA_SAIDA_0100-QTDE_FT > 0 ). "Ajuste p/ Compensar com Descontos/Juros 02.05.2018
      "Selecionar Partidas para compensar
      wa_saida_0100-st_comp  = '2'.
      wa_saida_0100-comp     = icon_system_mark.
    ENDIF.

    wa_saida_0100-view_cp  = icon_display.

    APPEND wa_saida_0100 TO it_saida_0100.
  ENDLOOP.

  LOOP AT tg_bsik_adt.
    CLEAR: wa_saida_0100, tg_lfa1, tg_ekko.

    PERFORM f_moeda_empresa USING tg_bsik_adt-bukrs
                                  'X'.
    IF ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.

    READ TABLE tg_bkpf WITH KEY bukrs = tg_bsik_adt-bukrs
                                belnr = tg_bsik_adt-belnr
                                gjahr = tg_bsik_adt-gjahr.
    CHECK sy-subrc = 0.

    READ TABLE tg_lfa1 WITH KEY lifnr = tg_bsik_adt-lifnr.
    CHECK sy-subrc = 0.

    READ TABLE tg_ekko WITH KEY ebeln = tg_bsik_adt-ebeln.
    IF sy-subrc = 0.

    ENDIF.

    wa_saida_0100-bukrs     = tg_bsik_adt-bukrs.
    wa_saida_0100-parid     = tg_lfa1-lifnr.
    wa_saida_0100-name1     = tg_lfa1-name1.
    wa_saida_0100-belnr     = tg_bsik_adt-belnr.
    wa_saida_0100-buzei     = tg_bsik_adt-buzei.
    wa_saida_0100-gjahr     = tg_bsik_adt-gjahr.
    wa_saida_0100-bldat     = tg_bsik_adt-bldat.
    wa_saida_0100-budat     = tg_bsik_adt-budat.
    wa_saida_0100-waers     = tg_bsik_adt-waers.
    wa_saida_0100-dmbtr     = tg_bsik_adt-dmbtr.
    wa_saida_0100-dmbe2     = tg_bsik_adt-dmbe2.
    wa_saida_0100-dmbtr_aux = tg_bsik_adt-dmbtr.
    wa_saida_0100-dmbe2_aux = tg_bsik_adt-dmbe2.
    wa_saida_0100-hkont     = tg_bsik_adt-hkont.
    wa_saida_0100-bschl     = tg_bsik_adt-bschl.
    wa_saida_0100-umsks     = tg_bsik_adt-umsks.
    wa_saida_0100-umskz     = tg_bsik_adt-umskz.
    wa_saida_0100-shkzg     = tg_bsik_adt-shkzg.
    wa_saida_0100-gsber     = tg_bsik_adt-gsber.
    wa_saida_0100-sgtxt     = tg_bsik_adt-sgtxt.
    wa_saida_0100-zfbdt     = tg_bsik_adt-zfbdt.
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
    wa_saida_0100-koart     = 'K'.

    PERFORM f_get_taxa USING tg_bkpf
                             wa_saida_0100-dmbtr
                             wa_saida_0100-dmbe2
                    CHANGING wa_saida_0100-kursf.

    PERFORM f_get_bsik_comp TABLES tg_bsik_comp_aux
                             USING tg_bsik_adt-bukrs
                                   tg_bsik_adt-belnr
                                   tg_bsik_adt-buzei
                                   tg_bsik_adt-gjahr
                                   tg_bsik_adt-lifnr
                                   tg_bsik_adt-ebeln
                                   tg_bsik_adt-sgtxt
                                   tg_bsik_adt-zuonr
                                   tg_bsik_adt-anln1
                                   tg_bsik_adt-anln2.

    LOOP AT tg_bsik_comp_aux.
      ADD 1 TO wa_saida_0100-qtde_ft.
      IF ( tg_bsik_comp_aux-shkzg NE wa_saida_0100-shkzg ). "Se for operação(Cred.Deb.) diferente da partida principal
        ADD tg_bsik_comp_aux-dmbtr TO wa_saida_0100-ft_dmbtr.
        ADD tg_bsik_comp_aux-dmbe2 TO wa_saida_0100-ft_dmbe2.
      ELSE.
        SUBTRACT tg_bsik_comp_aux-dmbtr FROM wa_saida_0100-ft_dmbtr.
        SUBTRACT tg_bsik_comp_aux-dmbe2 FROM wa_saida_0100-ft_dmbe2.
      ENDIF.
    ENDLOOP.

    wa_saida_0100-comp = icon_light_out.
    "
    CLEAR: wa_zfit0154, wa_saida_0100-st_comp.
    IF rb_prod IS NOT INITIAL.
      SELECT SINGLE *
        FROM lfa1
        INTO @DATA(w_lfa1)
        WHERE lifnr = @wa_saida_0100-parid
        AND   vbund = 'SOCIOS'.
      IF sy-subrc = 0.
        SELECT SINGLE *
                 INTO wa_zfit0154
                 FROM zfit0154
                 WHERE tipo = 'P'
                 AND   fg_soc = 'X'.
      ELSE.
        SELECT SINGLE *
           INTO wa_zfit0154
           FROM zfit0154
           WHERE tipo = 'P'
           AND   fg_soc = ''.
      ENDIF.
      "
      DATA(vdif_int) = abs( wa_saida_0100-ft_dmbtr - wa_saida_0100-dmbtr ).
      IF ( vdif_int LE  wa_zfit0154-vlr_toler ) AND ( wa_saida_0100-waers    = tg_t001-waers ) .
        wa_saida_0100-st_comp  = '3'.
        wa_saida_0100-vlr_rsd  = wa_saida_0100-ft_dmbtr - wa_saida_0100-dmbtr.
        wa_saida_0100-comp     = icon_execute_object.
      ENDIF.

      DATA(vdif_for) = abs( wa_saida_0100-ft_dmbe2 - wa_saida_0100-dmbe2 ).
      IF ( vdif_int LT  wa_zfit0154-vlr_toler ) AND ( wa_saida_0100-waers    = tg_t001-waers2 ) .
        wa_saida_0100-st_comp  = '3'.
        wa_saida_0100-vlr_rsd  = wa_saida_0100-ft_dmbe2 - wa_saida_0100-dmbe2 .
        wa_saida_0100-comp     = icon_execute_object.
      ENDIF.
    ENDIF.
    "
    wa_saida_0100-df_dmbe2 = wa_saida_0100-ft_dmbtr - wa_saida_0100-dmbtr.
    "
    IF wa_saida_0100-st_comp NE '3'.
      IF ( ( wa_saida_0100-ft_dmbtr = wa_saida_0100-dmbtr ) AND
           ( wa_saida_0100-waers    = tg_t001-waers       ) )
            OR
         ( ( wa_saida_0100-ft_dmbe2 = wa_saida_0100-dmbe2 ) AND
           ( wa_saida_0100-waers    = tg_t001-waers2      ) ).
        "Liberado Compensação ( Total Adiamento = Total Partidas Compensar )
        wa_saida_0100-st_comp  = '1'.
        wa_saida_0100-comp      = icon_execute_object.
      ELSE."IF ( WA_SAIDA_0100-QTDE_FT > 0 ). "Ajuste p/ Compensar com Descontos/Juros 02.05.2018
        "Selecionar Partidas para compensar
        wa_saida_0100-st_comp  = '2'.
        wa_saida_0100-comp      = icon_system_mark.
      ENDIF.
    ENDIF.

    wa_saida_0100-view_cp  = icon_display.

    APPEND wa_saida_0100 TO it_saida_0100.
  ENDLOOP.


ENDFORM.

FORM f_refresh_alv USING p_alv.

  CASE p_alv.
    WHEN '0100'.
      IF obj_alv_0100 IS NOT INITIAL.
        CALL METHOD obj_alv_0100->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      ENDIF.
    WHEN '0110'.
      CALL METHOD obj_alv_0110->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
  ENDCASE.

ENDFORM.

FORM f_refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

  IF rb_prod = 'X'.
    IF sy-dynnr = '0100'.
      IF obj_alv_0100 IS NOT INITIAL.
        CALL METHOD obj_alv_0100->free.

        IF obj_container_0100 IS NOT INITIAL.
          CALL METHOD obj_container_0100->free.
        ENDIF.
        FREE:  obj_alv_0100,obj_toolbar_0100.
        FREE: obj_container_0100.
      ENDIF.
      MOVE '/PRODUTORES' TO gs_variant-variant.
    ELSEIF sy-dynnr = '0110'.
      IF obj_alv_0110 IS NOT INITIAL.
        CALL METHOD obj_alv_0110->free.

        IF obj_container_0110 IS NOT INITIAL.
          CALL METHOD obj_container_0110->free.
        ENDIF.
        FREE:  obj_alv_0100,obj_toolbar_0110.
        FREE: obj_container_0110.
      ENDIF.
      MOVE '/PRODUT_COMP' TO gs_variant-variant.
    ENDIF.
    "

  ENDIF.

ENDFORM.

FORM f_criar_catalog USING p_screen.

  FREE: wa_fcat, it_fcat.

  CASE p_screen.
    WHEN '0100'.

      PERFORM f_estrutura_alv USING:

       01  'KNA1'      'KUNNR'            'IT_SAIDA_0100' 'PARID'    'Cliente/Fornecedor'           '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'KNA1'      'NAME1'            'IT_SAIDA_0100' 'NAME1'    'Nome Cliente/Fornecedor'      '27'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       03  'BSID'      'BELNR'            'IT_SAIDA_0100' 'BELNR'    'Doc. Contábil'                '12'   ' '    ''  ' ' ' ' 'X' ' ' '' ,
       04  'BSID'      'BLDAT'            'IT_SAIDA_0100' 'BLDAT'    'Dt.Doc.'                      '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       05  'BSID'      'BUDAT'            'IT_SAIDA_0100' 'BUDAT'    'Dt.Lcto'                      '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       06  'BSID'      'WAERS'            'IT_SAIDA_0100' 'WAERS'    'Moeda'                        '05'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       07  'BSID'      'DMBTR'            'IT_SAIDA_0100' 'DMBTR'    'Valor R$'                     '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       08  'BSID'      'DMBE2'            'IT_SAIDA_0100' 'DMBE2'    'Valor U$'                     '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       09  'BKPF'      'KURSF'            'IT_SAIDA_0100' 'KURSF'    'Tx.Câmbio'                    '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       10  'BSID'      'VBEL2'            'IT_SAIDA_0100' 'OVPED'    'Nro. OV/Ped.'                 '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       10  'BSID'      'POSN2'            'IT_SAIDA_0100' 'ITMOP'    'Itm.OV/Ped.'                  '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       10  'ZSDT0041'  'DOC_SIMULACAO'    'IT_SAIDA_0100' 'DCSIM'    'Simulador'                    '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       11  'VBAK'      'SPART'            'IT_SAIDA_0100' 'SPART'    'S.A'                          '03'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       12  'TSPAT'     'VTEXT'            'IT_SAIDA_0100' 'VTEXT'    'Setor Atividade'              '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       13  'BSID'      'DMBTR'            'IT_SAIDA_0100' 'FT_DMBTR' 'Tot.Fat.R$'                   '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       14  'BSID'      'DMBE2'            'IT_SAIDA_0100' 'DF_DMBE2' 'Dif.FatXVlr R$'               '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
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


    WHEN '0110'.

      PERFORM f_estrutura_alv USING:

       01  ''          ''                 'IT_SAIDA_0110' 'IC_MANUAL'   'LM'                  '04'   ' '    ' '  ' ' 'C' ' ' ' ' ' ' ,
       01  ''          ''                 'IT_SAIDA_0110' 'CHECK'      'Check'                '05'   'X'    ' '  ' ' ' ' ' ' ' ' 'X' ,
       "01  'KNA1'      'KUNNR'            'IT_SAIDA_0110' 'KUNNR'      'Cliente'              '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       "02  'KNA1'      'NAME1'            'IT_SAIDA_0110' 'NAME1'      'Nome Cliente'         '20'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       03  'BSID'      'BELNR'            'IT_SAIDA_0110' 'BELNR'      'Doc.Ctb.'             '10'   ' '    ' '  ' ' ' ' 'X' ' ' ' ' ,
       04  'BSID'      'BLDAT'            'IT_SAIDA_0110' 'BLDAT'      'Dt.Doc.'              '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       05  'BSID'      'BUDAT'            'IT_SAIDA_0110' 'BUDAT'      'Dt.Lcto'              '10'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       05  ''          ''                 'IT_SAIDA_0110' 'HKONT'      'Conta'                '10'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       05  'BSID'      'BSCHL'            'IT_SAIDA_0110' 'BSCHL'      'CL'                   '02'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       06  'BSID'      'UMSKS'            'IT_SAIDA_0110' 'UMSKS'      'Cód.Razão Especial'   '01'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       06  'BSID'      'WAERS'            'IT_SAIDA_0110' 'WAERS'      'Moeda'                '05'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       07  'BSID'      'DMBTR'            'IT_SAIDA_0110' 'DMBTR_AUX'  'Valor R$'             '12'   ' '    'X'  ' ' ' ' ' ' ' ' ' ' ,
       08  'BSID'      'DMBE2'            'IT_SAIDA_0110' 'DMBE2_AUX'  'Valor U$'             '12'   ' '    'X'  ' ' ' ' ' ' ' ' ' ' ,
       09  'BSID'      'DMBTR'            'IT_SAIDA_0110' 'VLR_RSD'    'Vlr.Residual'         '12'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       09  'BSID'      'DMBTR'            'IT_SAIDA_0110' 'DMBTR'      'Vlr.Comp.R$'          '12'   'X'    'X'  ' ' ' ' ' ' ' ' ' ' ,
       10  'BSID'      'DMBE2'            'IT_SAIDA_0110' 'DMBE2'      'Vlr.Comp.U$'          '12'   'X'    'X'  ' ' ' ' ' ' ' ' ' ' ,
       10  'BSID'      'ZFBDT'            'IT_SAIDA_0110' 'ZFBDT'      'Dt.Venc.'             '10'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' ,
       11  'BSID'      'VBEL2'            'IT_SAIDA_0110' 'OVPED'      'Nro. OV/Ped.'         '13'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       11  'BSID'      'POSN2'            'IT_SAIDA_0110' 'ITMOP'      'Itm.OV/Ped.'          '11'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       11  ''          ''                 'IT_SAIDA_0110' 'ZUONR'      'Atribuição'           '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       10  'ZSDT0041'  'DOC_SIMULACAO'    'IT_SAIDA_0110' 'DCSIM'      'Simulador'            '10'   ' '    ' '  ' ' ' ' ' ' ' ' ' ' ,
       12  ''          ''                 'IT_SAIDA_0110' 'PART_PRINC' 'Aplic.Part.Princ.'    '18'   'X'    ' '  ' ' ' ' ' ' ' ' 'X' ,
       13  'BSID'      'SGTXT'            'IT_SAIDA_0110' 'SGTXT_RSD ' 'Txt. Residual'        '35'   'X'    ' '  ' ' ' ' ' ' ' ' ' ' .

    WHEN '0120'.

      PERFORM f_estrutura_alv USING:

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

  ENDCASE.

ENDFORM.

FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                           VALUE(p_tabname)       LIKE dd02d-tabname
                           VALUE(p_field)         LIKE dd03d-fieldname
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                           VALUE(p_outputlen)
                           VALUE(p_edit)
                           VALUE(p_sum)
                           VALUE(p_emphasize)
                           VALUE(p_just)
                           VALUE(p_hotspot)
                           VALUE(p_f4)
                           VALUE(p_check).

  CLEAR wa_fcat.

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

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM f_exclude_fcode USING p_screen.

  APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_exclude_fcode.

  "CASE P_SCREEN.
  "  WHEN '0112'.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_exclude_fcode.
  "ENDCASE.

  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_check             TO it_exclude_fcode.

ENDFORM.

FORM f_limpa_variaveis .

  CLEAR: wa_saida_0100,
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
         tg_tspat.

  CLEAR: vg_not_found.

ENDFORM.

FORM f_config_ranges.

  CLEAR: r_fdlev_banco, r_fdlev_banco[],
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
  APPEND r_fdlev_banco.

  r_fdlev_banco-low    = 'B2'.
  APPEND r_fdlev_banco.

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



ENDFORM.

FORM f_hotspot_click  USING  p_alv
                             i_row_id     TYPE lvc_s_row
                             i_column_id  TYPE lvc_s_col
                             is_row_no    TYPE lvc_s_roid.

  DATA: it_rsparams TYPE TABLE OF rsparams,
        wa_rsparams TYPE rsparams.

  DATA: opt         TYPE ctu_params,
        vl_error    TYPE c,
        vl_doc_comp TYPE bsad-belnr.


  CASE p_alv.
    WHEN '0100'.
      CLEAR: wa_saida_0100, wa_saida_0110.
      CASE i_column_id.
        WHEN 'BELNR'.
          READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX i_row_id.
          CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-belnr IS NOT INITIAL ).

          SET PARAMETER ID 'BLN' FIELD wa_saida_0100-belnr.
          SET PARAMETER ID 'BUK' FIELD wa_saida_0100-bukrs.
          SET PARAMETER ID 'GJR' FIELD wa_saida_0100-budat(4).

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

        WHEN 'COMP'.

          IF p_blt_cp IS INITIAL.
            MESSAGE 'Informe um Tp.Documento para Compensação!' TYPE 'S'.
            EXIT.
          ENDIF.

          READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX i_row_id.
          CHECK ( sy-subrc = 0 ).

          CASE wa_saida_0100-st_comp.
            WHEN '1' OR '3' . "Liberado Compensação ( Total Adiamento = Total Partidas Compensar )
              PERFORM f_get_part_comp USING wa_saida_0100.
              PERFORM f_bapi_f51 USING abap_false
                              CHANGING wa_saida_0100
                                       vl_error
                                       vl_doc_comp.
              PERFORM f_renovar_cons.
            WHEN '2'. "Selecionar Partidas para compensar
              PERFORM f_get_part_comp USING wa_saida_0100.
              "CHECK IT_SAIDA_0110[] IS NOT INITIAL. "Ajuste p/ Compensar com Descontos/Juros 02.05.2018
              CALL SCREEN 0110 STARTING AT 01 01 ENDING AT 165 20 .
              PERFORM f_renovar_cons.
          ENDCASE.
        WHEN 'VIEW_CP'.

          IF p_blt_cp IS INITIAL.
            MESSAGE 'Informe um Tp.Documento para Compensação!' TYPE 'S'.
            EXIT.
          ENDIF.

          READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX i_row_id.
          CHECK ( sy-subrc = 0 ).

          wa_saida_0100-st_comp = '2'.

          PERFORM f_get_part_comp USING wa_saida_0100.
          CHECK it_saida_0110[] IS NOT INITIAL.
          CALL SCREEN 0110 STARTING AT 01 01 ENDING AT 165 20 .
          PERFORM f_renovar_cons.

      ENDCASE.
    WHEN '0110'.
      CASE i_column_id.
        WHEN 'BELNR'.
          READ TABLE it_saida_0110 INTO wa_saida_0110 INDEX i_row_id.
          CHECK ( sy-subrc = 0 ) AND ( wa_saida_0110-belnr IS NOT INITIAL ).

          SET PARAMETER ID 'BLN' FIELD wa_saida_0110-belnr.
          SET PARAMETER ID 'BUK' FIELD wa_saida_0110-bukrs.
          SET PARAMETER ID 'GJR' FIELD wa_saida_0110-budat(4).

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDCASE.
    WHEN '0120'.
      CASE i_column_id.
        WHEN 'ST_CTB'.
          CLEAR: tg_zib_err[], wa_saida_0120.
          READ TABLE it_saida_0120 INTO wa_saida_0120 INDEX i_row_id.

          CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120-obj_key IS NOT INITIAL ).

          SELECT *
            FROM zib_contabil_err AS a INTO CORRESPONDING FIELDS OF TABLE tg_zib_err
           WHERE obj_key  EQ wa_saida_0120-obj_key.

          CHECK tg_zib_err[] IS NOT INITIAL.

          PERFORM f_montar_layout_log_erro.

          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              it_fieldcat           = estrutura[]
              i_save                = 'A'
              i_screen_start_column = 3
              i_screen_start_line   = 3
              i_screen_end_column   = 100
              i_screen_end_line     = 13
            TABLES
              t_outtab              = tg_zib_err.
        WHEN 'AUGBL'.
          READ TABLE it_saida_0120 INTO wa_saida_0120 INDEX i_row_id.
          CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120-augbl IS NOT INITIAL ).

          SET PARAMETER ID 'BLN' FIELD wa_saida_0120-augbl.
          SET PARAMETER ID 'BUK' FIELD wa_saida_0120-bukrs.
          SET PARAMETER ID 'GJR' FIELD wa_saida_0120-augdt(4).

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        WHEN 'BELNR_GER'.
          READ TABLE it_saida_0120 INTO wa_saida_0120 INDEX i_row_id.
          CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120-belnr_ger IS NOT INITIAL ).

          SET PARAMETER ID 'BLN' FIELD wa_saida_0120-belnr_ger.
          SET PARAMETER ID 'BUK' FIELD wa_saida_0120-bukrs.
          SET PARAMETER ID 'GJR' FIELD wa_saida_0120-gjahr_ger.

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        WHEN 'STBLG_GER'.
          READ TABLE it_saida_0120 INTO wa_saida_0120 INDEX i_row_id.
          CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120-stblg_ger IS NOT INITIAL ).

          SET PARAMETER ID 'BLN' FIELD wa_saida_0120-stblg_ger.
          SET PARAMETER ID 'BUK' FIELD wa_saida_0120-bukrs.
          SET PARAMETER ID 'GJR' FIELD wa_saida_0120-gjahr_ger.

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDCASE.
  ENDCASE.


ENDFORM.

FORM f_get_part_comp  USING  p_saida_0100 TYPE ty_saida_0100.

  CLEAR: it_saida_0110[].

  CHECK p_saida_0100-kursf NE 0.

  CASE p_saida_0100-koart.
    WHEN 'D'. "Cliente

*--------------------------------------------------------------------*
*  Carrega Partidas Compensar Cliente
*--------------------------------------------------------------------*

      PERFORM f_get_bsid_comp TABLES tg_bsid_comp_aux
                               USING p_saida_0100-bukrs
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

      LOOP AT tg_bsid_comp_aux INTO tg_bsid_comp.

        CLEAR: wa_saida_0110, tg_kna1, gt_estilo[].

        PERFORM f_moeda_empresa USING tg_bsid_comp-bukrs
                                      'X'.
        IF ( sy-subrc NE 0 ).
          RETURN.
        ENDIF.

        READ TABLE tg_bkpf WITH KEY bukrs = tg_bsid_comp-bukrs
                                    belnr = tg_bsid_comp-belnr
                                    gjahr = tg_bsid_comp-gjahr.
        CHECK sy-subrc = 0.

        READ TABLE tg_kna1 WITH KEY kunnr = tg_bsid_comp-kunnr.
        CHECK sy-subrc = 0.

        wa_saida_0110-bukrs     = tg_bsid_comp-bukrs.
        wa_saida_0110-parid     = tg_kna1-kunnr.
        wa_saida_0110-name1     = tg_kna1-name1.
        wa_saida_0110-belnr     = tg_bsid_comp-belnr.
        wa_saida_0110-buzei     = tg_bsid_comp-buzei.
        wa_saida_0110-gjahr     = tg_bsid_comp-gjahr.
        wa_saida_0110-bldat     = tg_bsid_comp-bldat.
        wa_saida_0110-budat     = tg_bsid_comp-budat.
        wa_saida_0110-waers     = tg_bsid_comp-waers.
        wa_saida_0110-dmbtr     = tg_bsid_comp-dmbtr.
        wa_saida_0110-dmbe2     = tg_bsid_comp-dmbe2.
        wa_saida_0110-dmbtr_aux = tg_bsid_comp-dmbtr.
        wa_saida_0110-dmbe2_aux = tg_bsid_comp-dmbe2.
        wa_saida_0110-hkont     = tg_bsid_comp-hkont.
        wa_saida_0110-bschl     = tg_bsid_comp-bschl.
        wa_saida_0110-umsks     = tg_bsid_comp-umsks.
        wa_saida_0110-umskz     = tg_bsid_comp-umskz.
        wa_saida_0110-gsber     = tg_bsid_comp-gsber.
        wa_saida_0110-sgtxt     = tg_bsid_comp-sgtxt.
        wa_saida_0110-zfbdt     = tg_bsid_comp-zfbdt.
        wa_saida_0110-zbd1t     = tg_bsid_comp-zbd1t.
        wa_saida_0110-ovped     = tg_bsid_comp-vbel2.
        wa_saida_0110-posn2     = tg_bsid_comp-posn2.
        wa_saida_0110-itmop     = tg_bsid_comp-posn2.
        wa_saida_0110-dcsim     = tg_bsid_comp-dcsim.
        wa_saida_0110-kidno     = tg_bsid_comp-kidno.
        wa_saida_0110-blart     = tg_bsid_comp-blart.
        wa_saida_0110-zuonr     = tg_bsid_comp-zuonr.
        wa_saida_0110-zterm     = tg_bsid_comp-zterm.
        wa_saida_0110-anln1     = tg_bsid_comp-anln1.
        wa_saida_0110-anln2     = tg_bsid_comp-anln2.
        wa_saida_0110-xblnr     = tg_bkpf-xblnr.
        wa_saida_0110-koart     = p_saida_0100-koart.
        wa_saida_0110-sgtxt_rsd = 'Desmembramento Fatura'.
        wa_saida_0110-sgtxt_rsd = wa_saida_0110-sgtxt.

        PERFORM f_get_taxa USING tg_bkpf
                                 wa_saida_0110-dmbtr
                                 wa_saida_0110-dmbe2
                        CHANGING wa_saida_0110-kursf.

        IF wa_saida_0100-st_comp = '1'. "Total Partidas = Valor Adiantamento.
          wa_saida_0110-check = 'X'.
        ENDIF.

        IF p_saida_0100-waers = tg_t001-waers.
          wa_saida_0110-dmbe2 = wa_saida_0110-dmbtr / wa_saida_0110-kursf.
        ELSE.
          wa_saida_0110-dmbtr = wa_saida_0110-dmbe2 * wa_saida_0110-kursf.
        ENDIF.

        wl_estilo-fieldname    = 'BSCHL'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO gt_estilo.

        wl_estilo-fieldname    = 'DMBE2'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO gt_estilo.

        wl_estilo-fieldname    = 'DMBTR'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO gt_estilo.

        wl_estilo-fieldname    = 'HKONT'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO gt_estilo.

        IF wa_saida_0110-manual IS INITIAL.
          wl_estilo-fieldname    = 'PART_PRINC'.
          wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
          APPEND wl_estilo TO gt_estilo.
        ENDIF.

        wl_estilo-fieldname    = 'UMSKS'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO gt_estilo.

        wl_estilo-fieldname    = 'ZFBDT'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO gt_estilo.

        INSERT LINES OF gt_estilo INTO TABLE wa_saida_0110-estilo.

        APPEND wa_saida_0110 TO it_saida_0110.

      ENDLOOP.

    WHEN 'K'. "Fornecedor

*--------------------------------------------------------------------*
*  Carrega Partidas Compensar Fornecedor
*--------------------------------------------------------------------*

      PERFORM f_get_bsik_comp TABLES tg_bsik_comp_aux
                               USING p_saida_0100-bukrs
                                     p_saida_0100-belnr
                                     p_saida_0100-buzei
                                     p_saida_0100-gjahr
                                     p_saida_0100-parid
                                     p_saida_0100-ovped
                                     p_saida_0100-sgtxt
                                     p_saida_0100-zuonr
                                     p_saida_0100-anln1
                                     p_saida_0100-anln2.

      LOOP AT tg_bsik_comp_aux INTO tg_bsik_comp.

        CLEAR: wa_saida_0110, tg_lfa1, gt_estilo[].

        PERFORM f_moeda_empresa USING tg_bsik_comp-bukrs
                                      'X'.
        IF ( sy-subrc NE 0 ).
          RETURN.
        ENDIF.

        READ TABLE tg_bkpf WITH KEY bukrs = tg_bsik_comp-bukrs
                                    belnr = tg_bsik_comp-belnr
                                    gjahr = tg_bsik_comp-gjahr.
        CHECK sy-subrc = 0.

        READ TABLE tg_lfa1 WITH KEY lifnr = tg_bsik_comp-lifnr.

        CHECK sy-subrc = 0.

        wa_saida_0110-bukrs     = tg_bsik_comp-bukrs.
        wa_saida_0110-parid     = tg_lfa1-lifnr.
        wa_saida_0110-name1     = tg_lfa1-name1.
        wa_saida_0110-belnr     = tg_bsik_comp-belnr.
        wa_saida_0110-buzei     = tg_bsik_comp-buzei.
        wa_saida_0110-gjahr     = tg_bsik_comp-gjahr.
        wa_saida_0110-bldat     = tg_bsik_comp-bldat.
        wa_saida_0110-budat     = tg_bsik_comp-budat.
        wa_saida_0110-waers     = tg_bsik_comp-waers.
        wa_saida_0110-dmbtr     = tg_bsik_comp-dmbtr.
        wa_saida_0110-dmbe2     = tg_bsik_comp-dmbe2.
        wa_saida_0110-dmbtr_aux = tg_bsik_comp-dmbtr.
        wa_saida_0110-dmbe2_aux = tg_bsik_comp-dmbe2.
        wa_saida_0110-hkont     = tg_bsik_comp-hkont.
        wa_saida_0110-bschl     = tg_bsik_comp-bschl.
        wa_saida_0110-umsks     = tg_bsik_comp-umsks.
        wa_saida_0110-umskz     = tg_bsik_comp-umskz.
        wa_saida_0110-gsber     = tg_bsik_comp-gsber.
        wa_saida_0110-sgtxt     = tg_bsik_comp-sgtxt.
        wa_saida_0110-zfbdt     = tg_bsik_comp-zfbdt.
        wa_saida_0110-zbd1t     = tg_bsik_comp-zbd1t.
        wa_saida_0110-ovped     = tg_bsik_comp-ebeln.
        wa_saida_0110-ebelp     = tg_bsik_comp-ebelp.
        wa_saida_0110-itmop     = tg_bsik_comp-ebelp.
        wa_saida_0110-kidno     = tg_bsik_comp-kidno.
        wa_saida_0110-blart     = tg_bsik_comp-blart.
        wa_saida_0110-zuonr     = tg_bsik_comp-zuonr.
        wa_saida_0110-zterm     = tg_bsik_comp-zterm.
        wa_saida_0110-anln1     = tg_bsik_comp-anln1.
        wa_saida_0110-anln2     = tg_bsik_comp-anln2.
        wa_saida_0110-xblnr     = tg_bkpf-xblnr.
        wa_saida_0110-koart     = p_saida_0100-koart.
        wa_saida_0110-sgtxt_rsd = 'Desmembramento Fatura'.
        wa_saida_0110-sgtxt_rsd = wa_saida_0110-sgtxt.

        PERFORM f_get_taxa USING tg_bkpf
                                 wa_saida_0110-dmbtr
                                 wa_saida_0110-dmbe2
                        CHANGING wa_saida_0110-kursf.

        IF wa_saida_0100-st_comp = '1'. "Total Partidas = Valor Adiantamento.
          wa_saida_0110-check = 'X'.
        ENDIF.

        IF p_saida_0100-waers = tg_t001-waers.
          wa_saida_0110-dmbe2 = wa_saida_0110-dmbtr / wa_saida_0110-kursf.
        ELSE.
          wa_saida_0110-dmbtr = wa_saida_0110-dmbe2 * wa_saida_0110-kursf.
        ENDIF.

        wl_estilo-fieldname    = 'BSCHL'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO gt_estilo.

        wl_estilo-fieldname    = 'DMBE2'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO gt_estilo.

        wl_estilo-fieldname    = 'DMBTR'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO gt_estilo.

        wl_estilo-fieldname    = 'HKONT'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO gt_estilo.

        IF wa_saida_0110-manual IS INITIAL.
          wl_estilo-fieldname    = 'PART_PRINC'.
          wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
          APPEND wl_estilo TO gt_estilo.
        ENDIF.

        wl_estilo-fieldname    = 'UMSKS'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO gt_estilo.

        wl_estilo-fieldname    = 'ZFBDT'.
        wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO gt_estilo.

        INSERT LINES OF gt_estilo INTO TABLE wa_saida_0110-estilo.

        APPEND wa_saida_0110 TO it_saida_0110.

      ENDLOOP.

  ENDCASE.

  CLEAR: wa_cabecalho_0110.
  wa_cabecalho_0110-adt_dmbtr = p_saida_0100-dmbtr.
  wa_cabecalho_0110-adt_dmbe2 = p_saida_0100-dmbe2.
  wa_cabecalho_0110-waers     = p_saida_0100-waers.
  IF rb_prod IS INITIAL.
    wa_cabecalho_0110-sgtxt_rsd = 'Desmembramento Adiantamento'.
  ELSE.
    wa_cabecalho_0110-sgtxt_rsd = p_saida_0100-sgtxt.
  ENDIF.

  IF p_saida_0100-koart = 'D'.
    READ TABLE tg_kna1 WITH KEY kunnr = p_saida_0100-parid.
    IF sy-subrc = 0.
      CONCATENATE p_saida_0100-parid '-' tg_kna1-name1
             INTO wa_cabecalho_0110-ds_cliente SEPARATED BY space.
    ENDIF.
  ELSE.
    READ TABLE tg_lfa1 WITH KEY lifnr = p_saida_0100-parid.
    IF sy-subrc = 0.
      CONCATENATE p_saida_0100-parid '-' tg_lfa1-name1
             INTO wa_cabecalho_0110-ds_cliente SEPARATED BY space.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_bapi_f51 USING p_residual_comp  TYPE c  "Opção para deixar residual na Compensação
             CHANGING p_saida_0100     TYPE ty_saida_0100
                      p_erro
                      p_doc_comp       TYPE bsad-belnr.

  DATA: BEGIN OF wa_retorno,
          mandt  LIKE zgl002_comp_f44-mandt,
          bukrs  LIKE zgl002_comp_f44-bukrs,
          lote   LIKE zgl002_comp_f44-lote,
          status LIKE zgl002_comp_f44-status,
          sgtxt  LIKE zgl002_comp_f44-sgtxt,
          data   LIKE zgl002_comp_f44-data,
          hora   LIKE zgl002_comp_f44-hora,
        END OF wa_retorno.

  DATA: it_retorno_rfc     LIKE STANDARD TABLE OF wa_retorno.


  DATA: wl_0122      TYPE zfit0122,
        vl_name1     TYPE kna1-name1,
        it_0110_comp TYPE TABLE OF ty_saida_0110.

  DATA: l_auglv   TYPE t041a-auglv   VALUE 'UMBUCHNG', "Posting with Clearing
        l_tcode   TYPE sy-tcode      VALUE 'FB05',     "You get an error with any other value
        l_sgfunct TYPE rfipi-sgfunct VALUE 'C'.        "Post immediately

  DATA: lt_blntab  TYPE STANDARD TABLE OF blntab  WITH HEADER LINE,
        lt_ftclear TYPE STANDARD TABLE OF ftclear WITH HEADER LINE,
        lt_ftpost  TYPE STANDARD TABLE OF ftpost  WITH HEADER LINE,
        lt_fttax   TYPE STANDARD TABLE OF fttax   WITH HEADER LINE,
        lds_return TYPE bapiret2.

  DATA: wa_sai_0110_tmp TYPE ty_saida_0110.

  DATA: vdata(10),
        vdata_venc(10),
        cnum_seq(2),
        wl_vlr(16),
        wl_taxa(16),
        wl_vlrc(16),
        wl_vlrn        TYPE p DECIMALS 2,
        vcampo(15),
        v_kur          TYPE bkpf-kursf,
        vvalor_bax     TYPE zfit0042-dmbe2,
        msg_no         TYPE t100-msgnr,
        msg_text       TYPE string,
        p_mode         LIKE rfpdo-allgazmd,
        vl_dt_mov      TYPE sy-datum,
        count_ft       TYPE ftpost-count,
        v_xsimu        TYPE char1.

  CLEAR: p_doc_comp.

  vl_dt_mov = p_augdt-low.

  p_mode = 'N'.

  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_client           = sy-mandt
      i_function         = 'C'
      i_mode             = p_mode
      i_update           = 'S'
      i_user             = sy-uname
    EXCEPTIONS
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      OTHERS             = 6.

  IF sy-subrc NE 0.
    p_erro = 'X'.
    ROLLBACK WORK.
    MESSAGE 'Houve ao efeturar a compensação' TYPE 'S'.
    RETURN.
  ENDIF.

  CONCATENATE  vl_dt_mov+6(2) vl_dt_mov+4(2) vl_dt_mov(4) INTO vdata SEPARATED BY '.'.

  IF p_ksfcp > 0.
    WRITE: p_ksfcp TO wl_taxa.
  ELSE.
    WRITE: p_saida_0100-kursf TO wl_taxa.
  ENDIF.

  CONDENSE wl_taxa NO-GAPS.

  CLEAR: lt_blntab,   lt_blntab[],
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
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-WAERS'.
  lt_ftpost-fval = p_saida_0100-waers.
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-KURSF'.
  lt_ftpost-fval = wl_taxa.
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLDAT'.
  lt_ftpost-fval = vdata.
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BUDAT'.
  lt_ftpost-fval = vdata.
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-MONAT'.
  lt_ftpost-fval =  vl_dt_mov+4(2).
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLART'.
  lt_ftpost-fval = p_blt_cp.
  APPEND lt_ftpost.

  it_0110_comp[] = it_saida_0110[].
  SORT it_0110_comp BY belnr buzei.
  DELETE ADJACENT DUPLICATES FROM it_0110_comp COMPARING belnr buzei.
  IF p_saida_0100-st_comp  = '3'.
    LOOP AT it_0110_comp INTO wa_saida_0110 WHERE manual IS INITIAL.
      wa_saida_0110-check = 'X'.
      MODIFY it_0110_comp FROM wa_saida_0110 INDEX sy-tabix TRANSPORTING check.
    ENDLOOP.
  ENDIF.
  LOOP AT it_0110_comp INTO wa_saida_0110 WHERE manual IS INITIAL
                                            AND check  IS NOT INITIAL.

    lt_ftclear-agkoa  = wa_saida_0110-koart.
    lt_ftclear-agkon  = wa_saida_0110-parid.
    lt_ftclear-agums  = wa_saida_0110-umskz.
    lt_ftclear-agbuk  = wa_saida_0110-bukrs.
    lt_ftclear-xnops  = 'X'.
    lt_ftclear-selfd  = 'BELNR'.
    CONCATENATE wa_saida_0110-belnr wa_saida_0110-budat(4) wa_saida_0110-buzei INTO lt_ftclear-selvon.
    APPEND lt_ftclear.

    "Opção para deixar residual na Compensação
    IF ( p_residual_comp IS NOT INITIAL ) AND ( wa_saida_0110-anln1 IS NOT INITIAL ) AND ( wa_saida_0110-vlr_rsd > 0 ).
      CLEAR: wa_sai_0110_tmp.
      MOVE-CORRESPONDING wa_saida_0110 TO wa_sai_0110_tmp.

      PERFORM f_add_part_residual TABLES lt_ftpost
                                   USING wa_sai_0110_tmp
                                CHANGING count_ft
                                         p_erro.
      IF p_erro IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  "Adiantamento
  lt_ftclear-agkoa  = p_saida_0100-koart.
  lt_ftclear-agkon  = p_saida_0100-parid.
  lt_ftclear-agums  = p_saida_0100-umskz.
  lt_ftclear-agbuk  = p_saida_0100-bukrs.
  lt_ftclear-xnops  = 'X'.
  lt_ftclear-selfd  = 'BELNR'.
  CONCATENATE p_saida_0100-belnr p_saida_0100-budat(4) p_saida_0100-buzei INTO lt_ftclear-selvon.
  APPEND lt_ftclear.

  IF p_saida_0100-st_comp  = '3' AND p_saida_0100-vlr_rsd NE 0.
    CLEAR: wa_sai_0110_tmp.
    MOVE-CORRESPONDING p_saida_0100 TO wa_sai_0110_tmp.
    wa_sai_0110_tmp-vlr_rsd  = 0.
    wa_sai_0110_tmp-vlr_rsdp = p_saida_0100-vlr_rsd.

    PERFORM f_add_part_residual TABLES lt_ftpost
                                 USING wa_sai_0110_tmp
                              CHANGING count_ft
                                       p_erro.
    IF p_erro IS NOT INITIAL.
      RETURN.
    ENDIF.
  ENDIF.

  "Opção para deixar residual na Compensação
  IF ( p_residual_comp IS NOT INITIAL ) AND ( p_saida_0100-anln1 IS NOT INITIAL ) AND ( p_saida_0100-vlr_rsd > 0 ).
    CLEAR: wa_sai_0110_tmp.
    MOVE-CORRESPONDING p_saida_0100 TO wa_sai_0110_tmp.

    PERFORM f_add_part_residual TABLES lt_ftpost
                                 USING wa_sai_0110_tmp
                              CHANGING count_ft
                                       p_erro.
    IF p_erro IS NOT INITIAL.
      RETURN.
    ENDIF.
  ENDIF.

  "Lançamentos Manuais
  LOOP AT it_0110_comp INTO wa_saida_0110 WHERE manual IS NOT INITIAL
                                            AND check  IS NOT INITIAL.

    CLEAR: wl_vlrn, wl_vlrc, vl_name1.

    PERFORM f_moeda_empresa USING wa_saida_0110-bukrs
                                  'X'.
    IF ( sy-subrc NE 0 ).
      p_erro = 'X'.
      RETURN.
    ENDIF.

    CONCATENATE wa_saida_0110-zfbdt+6(2) wa_saida_0110-zfbdt+4(2) wa_saida_0110-zfbdt(4) INTO vdata_venc SEPARATED BY '.'.

    ADD 1 TO count_ft.

    IF ( wa_saida_0110-dmbtr = 0 ) OR ( wa_saida_0110-dmbe2 = 0 ).
      p_erro = 'X'.
      MESSAGE 'Existem lançamentos com valores zerados!' TYPE 'S'.
      RETURN.
    ENDIF.

    SELECT SINGLE koart
      FROM tbsl INTO wa_saida_0110-koart
     WHERE bschl = wa_saida_0110-bschl.

    IF ( sy-subrc NE 0 ) OR ( wa_saida_0110-koart IS INITIAL ).
      p_erro = 'X'.
      MESSAGE 'Tipo de Conta não encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

    lt_ftpost-stype = 'P'.
    lt_ftpost-count = count_ft .

    lt_ftpost-fnam = 'RF05A-NEWBS'.
    lt_ftpost-fval =  wa_saida_0110-bschl.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-HKONT'.
    lt_ftpost-fval = wa_saida_0110-hkont.
    APPEND lt_ftpost.

    IF p_saida_0100-waers = tg_t001-waers.
      wl_vlrn = abs( wa_saida_0110-dmbtr ).
    ELSE.
      wl_vlrn = abs( wa_saida_0110-dmbe2 ).
    ENDIF.

    WRITE: wl_vlrn TO wl_vlrc.

    lt_ftpost-fnam = 'BSEG-WRBTR'.
    lt_ftpost-fval =  wl_vlrc.
    APPEND lt_ftpost.

    IF p_saida_0100-waers NE tg_t001-waers.
      wl_vlrn      = abs( wa_saida_0110-dmbtr ).
      WRITE: wl_vlrn TO wl_vlrc.
      lt_ftpost-fnam = 'BSEG-DMBTR'.
      lt_ftpost-fval =  wl_vlrc.
      APPEND lt_ftpost.
    ELSE.
      wl_vlrn      = abs( wa_saida_0110-dmbe2 ).
      WRITE: wl_vlrn TO wl_vlrc.
      lt_ftpost-fnam = 'BSEG-DMBE2'.
      lt_ftpost-fval =  wl_vlrc.
      APPEND lt_ftpost.
    ENDIF.

    IF wa_saida_0110-umsks IS NOT INITIAL.
      lt_ftpost-fnam = 'RF05A-NEWUM'.
      lt_ftpost-fval = wa_saida_0110-umsks.
      APPEND lt_ftpost.
    ENDIF.

    CASE wa_saida_0110-koart.
      WHEN 'D' OR 'K'. "Cliente ou Fornecedor
        IF wa_saida_0110-zfbdt IS NOT INITIAL.
          lt_ftpost-fnam = 'BSEG-ZFBDT'.
          lt_ftpost-fval = vdata_venc.
          APPEND lt_ftpost.
        ENDIF.

        lt_ftpost-fnam = 'BSEG-KIDNO'.
        lt_ftpost-fval =  p_saida_0100-kidno.
        APPEND lt_ftpost.


        lt_ftpost-fnam = 'BSEG-GSBER'.
        lt_ftpost-fval =  p_saida_0100-gsber.
        APPEND lt_ftpost.

        lt_ftpost-fnam = 'BSEG-HZUON'.
        lt_ftpost-fval =  p_saida_0100-ovped.
        APPEND lt_ftpost.
      WHEN 'S'. "Razão
        lt_ftpost-fnam = 'BSEG-BUPLA'.
        lt_ftpost-fval =  p_saida_0100-gsber.
        APPEND lt_ftpost.
    ENDCASE.

    lt_ftpost-fnam = 'BSEG-SGTXT'.
    IF wa_saida_0110-manual = 'X'.
      lt_ftpost-fval = wa_saida_0110-sgtxt.
    ELSE.
      SELECT SINGLE name1
        FROM kna1 INTO vl_name1
       WHERE kunnr = wa_saida_0110-hkont.
      IF sy-subrc = 0.
        CONCATENATE 'Saldo Residual' vl_name1 INTO  lt_ftpost-fval SEPARATED BY space.
      ELSE.
        lt_ftpost-fval = 'Saldo Residual'.
      ENDIF.
    ENDIF.

    APPEND lt_ftpost.

  ENDLOOP.

  CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
    EXPORTING
      i_auglv                    = l_auglv
      i_tcode                    = l_tcode
      i_sgfunct                  = l_sgfunct
      i_no_auth                  = 'X'
      i_xsimu                    = v_xsimu
    IMPORTING
      e_msgid                    = lds_return-id
      e_msgno                    = lds_return-number
      e_msgty                    = lds_return-type
      e_msgv1                    = lds_return-message_v1
      e_msgv2                    = lds_return-message_v2
      e_msgv3                    = lds_return-message_v3
      e_msgv4                    = lds_return-message_v4
    TABLES
      t_blntab                   = lt_blntab
      t_ftclear                  = lt_ftclear
      t_ftpost                   = lt_ftpost
      t_fttax                    = lt_fttax
    EXCEPTIONS
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      OTHERS                     = 10.

  REFRESH it_retorno_rfc.
  IF lt_blntab[] IS INITIAL.
    p_erro = 'X'.
    WRITE lds_return-number TO msg_no.
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        msg_id                 = lds_return-id
        msg_no                 = msg_no
        msg_var1               = lds_return-message_v1
        msg_var2               = lds_return-message_v2
        msg_var3               = lds_return-message_v3
        msg_var4               = lds_return-message_v4
      IMPORTING
        msg_text               = msg_text
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.
    MESSAGE msg_text TYPE 'S'.
    IF rb_prod IS NOT INITIAL.
      wa_retorno-mandt  = sy-mandt.
      wa_retorno-bukrs  = p_saida_0100-bukrs.
      wa_retorno-lote   = p_saida_0100-zuonr.
      wa_retorno-status = 'E'.
      wa_retorno-sgtxt  = msg_text.
      wa_retorno-data   = sy-datum.
      wa_retorno-hora   = sy-uzeit.
      APPEND wa_retorno TO it_retorno_rfc.

      CALL FUNCTION 'Z_FI_OUTBOUND_LOTE_COMPRA_ADD'
        TABLES
          return = it_retorno_rfc.

      COMMIT WORK.
    ENDIF.
  ELSE.
    READ TABLE lt_blntab INDEX 1.
    CLEAR: wl_0122.
    wl_0122-usnam         = sy-uname.
    wl_0122-data          = sy-datum.
    wl_0122-hora          = sy-uzeit.
    wl_0122-tipo          = 'C'. "Compensação
    wl_0122-bukrs         = p_saida_0100-bukrs.
    wl_0122-gjahr         = vl_dt_mov(4).
    wl_0122-belnr         = lt_blntab-belnr.

    IF p_ksfcp > 0.
      wl_0122-kursf = p_ksfcp.
    ENDIF.

    MODIFY zfit0122 FROM wl_0122.
    IF sy-subrc NE 0.
      p_erro = 'X'.
      ROLLBACK WORK.
      MESSAGE 'Houve ao efeturar a compensação' TYPE 'S'.
      RETURN.
    ENDIF.

    "envia SIGAM
    IF p_saida_0100-st_comp  = '3'.
      CALL FUNCTION 'Z_FI_RETURN_PAYMENT_AP_AR'
        EXPORTING
          i_bukrs = wl_0122-bukrs
          i_augbl = wl_0122-belnr
          i_gjahr = wl_0122-gjahr
          i_tcode = 'ZFI0105'.
    ENDIF.
    "
    IF rb_prod IS NOT INITIAL.
      wa_retorno-mandt  = sy-mandt.
      wa_retorno-bukrs  = p_saida_0100-bukrs.
      wa_retorno-lote   = p_saida_0100-zuonr.
      wa_retorno-status = 'S'.
      wa_retorno-sgtxt  = |Documento { wl_0122-belnr } registrado na empresa { p_saida_0100-bukrs } |.
      wa_retorno-data  = sy-datum.
      wa_retorno-hora  = sy-uzeit.
      APPEND wa_retorno TO it_retorno_rfc.

      CALL FUNCTION 'Z_FI_OUTBOUND_LOTE_COMPRA_ADD'
        TABLES
          return = it_retorno_rfc.

      COMMIT WORK.
    ENDIF.

    CLEAR: wl_0122.
    wl_0122-usnam         = sy-uname.
    wl_0122-data          = sy-datum.
    wl_0122-hora          = sy-uzeit.
    wl_0122-tipo          = 'A'. "Adiantamento
    wl_0122-bukrs         = p_saida_0100-bukrs.
    wl_0122-gjahr         = p_saida_0100-budat(4).
    wl_0122-belnr         = p_saida_0100-belnr.
    MODIFY zfit0122 FROM wl_0122.
    IF sy-subrc NE 0.
      p_erro = 'X'.
      ROLLBACK WORK.
      MESSAGE 'Houve ao efeturar a compensação' TYPE 'S'.
      RETURN.
    ENDIF.

    LOOP AT it_0110_comp INTO wa_saida_0110 WHERE check IS NOT INITIAL.

      CLEAR: wl_0122.
      wl_0122-usnam         = sy-uname.
      wl_0122-data          = sy-datum.
      wl_0122-hora          = sy-uzeit.
      wl_0122-tipo          = 'P'. "Cta.Partidas Adiantamento.
      wl_0122-bukrs         = wa_saida_0110-bukrs.
      wl_0122-gjahr         = wa_saida_0110-budat(4).
      wl_0122-belnr         = wa_saida_0110-belnr.
      wl_0122-gjahr_cp      = p_saida_0100-budat(4).
      wl_0122-belnr_cp      = p_saida_0100-belnr.
      MODIFY zfit0122 FROM wl_0122.
      IF sy-subrc NE 0.
        p_erro = 'X'.
        ROLLBACK WORK.
        MESSAGE 'Houve ao efeturar a compensação' TYPE 'S'.
        RETURN.
      ENDIF.

    ENDLOOP.

    p_doc_comp = lt_blntab-belnr.
    MESSAGE |Compensação gerada com sucesso: { p_doc_comp } | TYPE 'I'.
  ENDIF.

  "fim
  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXPORTING
      i_bdcimmed              = 'X'
    EXCEPTIONS
      session_not_processable = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_bapi_f51_residual CHANGING p_saida_0110 TYPE ty_saida_0110
                                  p_erro.

  DATA: wl_0122 TYPE zfit0122,
        wl_bsid TYPE bsid,
        wl_bsik TYPE bsik.

  DATA: l_auglv   TYPE t041a-auglv   VALUE 'UMBUCHNG', "Posting with Clearing
        l_tcode   TYPE sy-tcode      VALUE 'FB05',     "You get an error with any other value
        l_sgfunct TYPE rfipi-sgfunct VALUE 'C'.        "Post immediately

  DATA: lt_blntab  TYPE STANDARD TABLE OF blntab  WITH HEADER LINE,
        lt_ftclear TYPE STANDARD TABLE OF ftclear WITH HEADER LINE,
        lt_ftpost  TYPE STANDARD TABLE OF ftpost  WITH HEADER LINE,
        lt_fttax   TYPE STANDARD TABLE OF fttax   WITH HEADER LINE,
        lds_return TYPE bapiret2.

  DATA: vdata(10),
        vdata_venc(10),
        cnum_seq(2),
        wl_vlr(16),
        wl_taxa(16),
        wl_vlrc(16),
        wl_vlrn        TYPE p DECIMALS 2,
        vcampo(15),
        v_kur          TYPE bkpf-kursf,
        vvalor_bax     TYPE zfit0042-dmbe2,
        msg_no         TYPE t100-msgnr,
        msg_text       TYPE string,
        p_mode         LIKE rfpdo-allgazmd,
        count_ft       TYPE ftpost-count,
        vl_dt_mov      TYPE sy-datum.

  IF p_saida_0110-vlr_rsd <= 0.
    MESSAGE 'Valor Residual inconsistente!' TYPE 'S'.
    p_erro = 'X'.
    RETURN.
  ENDIF.

  IF p_saida_0110-kursf <= 0.
    MESSAGE 'Taxa para gerar residual não encontrada!' TYPE 'S'.
    p_erro = 'X'.
    RETURN.
  ENDIF.

  CASE p_saida_0110-koart.
    WHEN 'D'. "Cliente
      IF ( p_saida_0110-bschl NE '01' ) AND
         ( p_saida_0110-bschl NE '11' ) AND
         ( p_saida_0110-bschl NE '09' ) AND
         ( p_saida_0110-bschl NE '19' ).
        DATA(_erro_chave) = 'X'.
      ENDIF.
    WHEN 'K'. "Fornecedor
      IF ( p_saida_0110-bschl NE '21' ) AND
         ( p_saida_0110-bschl NE '31' ) AND
         ( p_saida_0110-bschl NE '29' ) AND
         ( p_saida_0110-bschl NE '39' ).
        _erro_chave = 'X'.
      ENDIF.
  ENDCASE.

  IF _erro_chave IS NOT INITIAL.
    MESSAGE 'Chave Lançamento não configurada para deixar Saldo Residual!' TYPE 'S'.
    p_erro = 'X'.
    RETURN.
  ENDIF.

  PERFORM f_moeda_empresa USING p_saida_0110-bukrs
                                'X'.
  IF ( sy-subrc NE 0 ).
    p_erro = 'X'.
    RETURN.
  ENDIF.

  IF p_saida_0110-waers = tg_t001-waers.
    IF ( p_saida_0110-dmbtr + p_saida_0110-vlr_rsd ) NE p_saida_0110-dmbtr_aux.
      MESSAGE 'Saldo Residual inconsistente!' TYPE 'S'.
      p_erro = 'X'.
      RETURN.
    ENDIF.
  ELSE.
    IF ( p_saida_0110-dmbe2 + p_saida_0110-vlr_rsd ) NE p_saida_0110-dmbe2_aux.
      MESSAGE 'Saldo Residual inconsistente!' TYPE 'S'.
      p_erro = 'X'.
      RETURN.
    ENDIF.
  ENDIF.

  vl_dt_mov = p_augdt-low.

  p_mode = 'N'.

  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_client           = sy-mandt
      i_function         = 'C'
      i_mode             = p_mode
      i_update           = 'S'
      i_user             = sy-uname
    EXCEPTIONS
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      OTHERS             = 6.

  IF sy-subrc NE 0.
    p_erro = 'X'.
    MESSAGE 'Houve um erro ao desmembrar um documento' TYPE 'S'.
    RETURN.
  ENDIF.

  CONCATENATE vl_dt_mov+6(2) vl_dt_mov+4(2) vl_dt_mov(4) INTO vdata SEPARATED BY '.'.
  CONCATENATE p_saida_0110-zfbdt+6(2) p_saida_0110-zfbdt+4(2) p_saida_0110-zfbdt(4) INTO vdata_venc SEPARATED BY '.'.

  WRITE: p_saida_0110-kursf TO wl_taxa.
  CONDENSE wl_taxa NO-GAPS.

  CLEAR: lt_blntab,   lt_blntab[],
         lt_ftclear,  lt_ftclear[],
         lt_ftpost,   lt_ftpost[],
         lt_fttax,    lt_fttax[],
         lds_return, p_erro.

  count_ft = 1.

  lt_ftpost-stype = 'K'."Header
  lt_ftpost-count = count_ft.  "number of Dynpro

  lt_ftpost-fnam = 'BKPF-BUKRS'.
  lt_ftpost-fval = p_saida_0110-bukrs.
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-WAERS'.
  lt_ftpost-fval = p_saida_0110-waers.
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-KURSF'.
  lt_ftpost-fval = wl_taxa.
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLDAT'.
  lt_ftpost-fval = vdata.
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BUDAT'.
  lt_ftpost-fval = vdata.
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-MONAT'.
  lt_ftpost-fval =  vl_dt_mov+4(2).
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLART'.
  lt_ftpost-fval = p_saida_0110-blart.
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-XBLNR'.
  lt_ftpost-fval = p_saida_0110-xblnr.
  APPEND lt_ftpost.

  lt_ftclear-agkoa  = p_saida_0110-koart.
  lt_ftclear-agkon  = p_saida_0110-parid.
  lt_ftclear-agums  = p_saida_0110-umskz.
  lt_ftclear-agbuk  = p_saida_0110-bukrs.
  lt_ftclear-xnops  = 'X'.
  lt_ftclear-selfd  = 'BELNR'.
  CONCATENATE p_saida_0110-belnr p_saida_0110-budat(4) p_saida_0110-buzei INTO lt_ftclear-selvon.
  APPEND lt_ftclear.

  "Valor residual
  DO 2 TIMES.

    CLEAR: wl_vlrn, wl_vlrc.

    ADD 1 TO count_ft.

    CASE sy-index .
      WHEN 1.
        IF p_saida_0110-waers = tg_t001-waers.
          wl_vlrn = abs( p_saida_0110-dmbtr ).
        ELSE.
          wl_vlrn = abs( p_saida_0110-dmbe2 ).
        ENDIF.
      WHEN 2.
        wl_vlrn = abs( p_saida_0110-vlr_rsd ).
    ENDCASE.

    WRITE: wl_vlrn TO wl_vlrc.

    lt_ftpost-stype = 'P'.
    lt_ftpost-count = count_ft .

    lt_ftpost-fnam = 'RF05A-NEWBS'.
    lt_ftpost-fval =  p_saida_0110-bschl.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-HKONT'.
    lt_ftpost-fval = p_saida_0110-parid.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-GSBER'.
    lt_ftpost-fval = p_saida_0110-gsber.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-SGTXT'.
    lt_ftpost-fval = p_saida_0110-sgtxt_rsd.
*    IF P_SAIDA_0110-UMSKS IS NOT INITIAL. "Adiantamento
*      LT_FTPOST-FVAL = P_SGTXT_RZ-LOW.
*    ELSE.
*      LT_FTPOST-FVAL = P_SGTXT_NR-LOW.
*    ENDIF.

    APPEND lt_ftpost.

* ---> S4 Migration - 15/06/2023 - MA
*    SELECT SINGLE *
*      INTO @DATA(w__bseg)
*      FROM bseg
*      WHERE bukrs = @wa_saida_0100-bukrs
*      AND belnr   = @wa_saida_0100-belnr
**      AND BUZEI   = @WA_SAIDA_0100-BUZEI
*      AND gjahr   = @wa_saida_0100-gjahr
*      AND bschl   = '31'.

    DATA: lt_bseg TYPE fagl_t_bseg,
          w__bseg TYPE bseg.

    CALL FUNCTION 'FAGL_GET_BSEG'
      EXPORTING
        i_bukrs   = wa_saida_0100-bukrs
        i_belnr   = wa_saida_0100-belnr
        i_gjahr   = wa_saida_0100-gjahr
      IMPORTING
        et_bseg   = lt_bseg
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    DELETE lt_bseg WHERE bschl NE '31' AND buzei NE wa_saida_0100-buzei.

    READ TABLE lt_bseg INTO DATA(ls_bseg) INDEX 1.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_bseg TO w__bseg.
    ENDIF.
*<--- S4 Migration - 15/06/2023 - MA

    IF  ( w__bseg-bschl = '31' OR p_saida_0110-bschl = '31' ) AND wa_saida_0100-ft_dmbtr GT wa_saida_0100-dmbtr_aux.
      lt_ftpost-fnam = 'BSEG-KIDNO'.
      lt_ftpost-fval = p_saida_0110-kidno.
      IF p_saida_0110-kidno IS INITIAL.
        lt_ftpost-fval = wa_saida_0100-kidno.
      ENDIF.
      APPEND lt_ftpost.
    ENDIF.


    lt_ftpost-fnam = 'BSEG-ZUONR'.
    lt_ftpost-fval = p_saida_0110-zuonr.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-HZUON'.
    lt_ftpost-fval =  p_saida_0110-ovped.

    IF p_saida_0110-ebelp IS NOT INITIAL.
      lt_ftpost-fval = lt_ftpost-fval && p_saida_0110-ebelp.
    ENDIF.

    APPEND lt_ftpost.

    IF p_saida_0110-zfbdt IS NOT INITIAL.
      lt_ftpost-fnam = 'BSEG-ZFBDT'.
      lt_ftpost-fval = vdata_venc.
      APPEND lt_ftpost.

      IF ( p_saida_0110-umsks IS INITIAL ).
        lt_ftpost-fnam = 'BSEG-ZBD1T'.
        lt_ftpost-fval = p_saida_0110-zbd1t.
        CONDENSE lt_ftpost-fval NO-GAPS.
        APPEND lt_ftpost.
      ENDIF.
    ENDIF.

*    IF ( P_SAIDA_0110-ZTERM IS NOT INITIAL ) AND ( P_SAIDA_0110-UMSKS IS INITIAL ).
*      LT_FTPOST-FNAM = 'BSEG-ZTERM'.
*      LT_FTPOST-FVAL = P_SAIDA_0110-ZTERM.
*      APPEND LT_FTPOST.
*    ENDIF.

    IF p_saida_0110-umsks IS NOT INITIAL. "Adiantamento

      lt_ftpost-fnam = 'RF05A-NEWUM'.
      lt_ftpost-fval = p_saida_0110-umskz.
      APPEND lt_ftpost.

      IF ( p_saida_0110-anln1 IS NOT INITIAL ).
        lt_ftpost-fnam = 'BSEG-ANLN1'.
        lt_ftpost-fval = p_saida_0110-anln1.
        APPEND lt_ftpost.

        IF p_saida_0110-anln2 IS NOT INITIAL.
          lt_ftpost-fnam = 'BSEG-ANLN2'.
          lt_ftpost-fval = p_saida_0110-anln2.
          APPEND lt_ftpost.
        ENDIF.
      ENDIF.
    ENDIF.

    lt_ftpost-fnam = 'BSEG-WRBTR'.
    lt_ftpost-fval =  wl_vlrc.
    APPEND lt_ftpost.

    IF p_saida_0110-waers NE tg_t001-waers.
      wl_vlrn = wl_vlrn * abs( p_saida_0110-kursf ).
      WRITE: wl_vlrn TO wl_vlrc.
      lt_ftpost-fnam = 'BSEG-DMBTR'.
      lt_ftpost-fval =  wl_vlrc.
      APPEND lt_ftpost.
    ELSE.
      wl_vlrn = wl_vlrn / abs( p_saida_0110-kursf ).
      WRITE: wl_vlrn TO wl_vlrc.
      lt_ftpost-fnam = 'BSEG-DMBE2'.
      lt_ftpost-fval =  wl_vlrc.
      APPEND lt_ftpost.
    ENDIF.

  ENDDO.

  CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
    EXPORTING
      i_auglv                    = l_auglv
      i_tcode                    = l_tcode
      i_sgfunct                  = l_sgfunct
      i_no_auth                  = 'X'
    IMPORTING
      e_msgid                    = lds_return-id
      e_msgno                    = lds_return-number
      e_msgty                    = lds_return-type
      e_msgv1                    = lds_return-message_v1
      e_msgv2                    = lds_return-message_v2
      e_msgv3                    = lds_return-message_v3
      e_msgv4                    = lds_return-message_v4
    TABLES
      t_blntab                   = lt_blntab
      t_ftclear                  = lt_ftclear
      t_ftpost                   = lt_ftpost
      t_fttax                    = lt_fttax
    EXCEPTIONS
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      OTHERS                     = 10.


  IF lt_blntab[] IS INITIAL.
    p_erro = 'X'.
    WRITE lds_return-number TO msg_no.
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        msg_id                 = lds_return-id
        msg_no                 = msg_no
        msg_var1               = lds_return-message_v1
        msg_var2               = lds_return-message_v2
        msg_var3               = lds_return-message_v3
        msg_var4               = lds_return-message_v4
      IMPORTING
        msg_text               = msg_text
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.
    MESSAGE msg_text TYPE 'S'.
  ELSE.
    READ TABLE lt_blntab INDEX 1.
    CLEAR: wl_0122.
    wl_0122-usnam         = sy-uname.
    wl_0122-data          = sy-datum.
    wl_0122-hora          = sy-uzeit.
    wl_0122-tipo          = 'D'. "Desmembramento
    wl_0122-bukrs         = p_saida_0110-bukrs.
    wl_0122-gjahr         = p_saida_0110-gjahr.
    wl_0122-belnr         = p_saida_0110-belnr.
    wl_0122-gjahr_desmemb = vl_dt_mov(4).
    wl_0122-belnr_desmemb = lt_blntab-belnr.
    MODIFY zfit0122 FROM wl_0122.
    IF sy-subrc NE 0.
      p_erro = 'X'.
      MESSAGE 'Houve um erro ao desmembrar um documento' TYPE 'S'.
      RETURN.
    ENDIF.

    CLEAR: wl_bsid, wl_bsik.

    CASE p_saida_0110-koart.
      WHEN 'D'. "Cliente
        IF p_saida_0110-waers = tg_t001-waers.
          SELECT SINGLE *
            FROM bsid INTO wl_bsid
           WHERE bukrs = p_saida_0110-bukrs
             AND gjahr = vl_dt_mov(4)
             AND belnr = lt_blntab-belnr
             AND dmbtr = p_saida_0110-dmbtr.
        ELSE.
          SELECT SINGLE *
            FROM bsid INTO wl_bsid
           WHERE bukrs = p_saida_0110-bukrs
             AND gjahr = vl_dt_mov(4)
             AND belnr = lt_blntab-belnr
             AND dmbe2 = p_saida_0110-dmbe2.
        ENDIF.
      WHEN 'K'. "Fornecedor
        IF p_saida_0110-waers = tg_t001-waers.
          SELECT SINGLE *
            FROM bsik INTO wl_bsik
           WHERE bukrs = p_saida_0110-bukrs
             AND gjahr = vl_dt_mov(4)
             AND belnr = lt_blntab-belnr
             AND dmbtr = p_saida_0110-dmbtr.
        ELSE.
          SELECT SINGLE *
            FROM bsik INTO wl_bsik
           WHERE bukrs = p_saida_0110-bukrs
             AND gjahr = vl_dt_mov(4)
             AND belnr = lt_blntab-belnr
             AND dmbe2 = p_saida_0110-dmbe2.
        ENDIF.
    ENDCASE.

    IF sy-subrc NE 0 .
      p_erro = 'X'.
      MESSAGE 'Houve um erro ao desmembrar um documento' TYPE 'S'.
      RETURN.
    ENDIF.

    "Faz a troca para o novo documento gerado com valor a ser baixado.
    p_saida_0110-bl_desmemb = p_saida_0110-belnr.
    p_saida_0110-bz_desmemb = p_saida_0110-buzei.

    CASE p_saida_0110-koart.
      WHEN 'D'. "Cliente
        p_saida_0110-belnr      = wl_bsid-belnr.
        p_saida_0110-buzei      = wl_bsid-buzei.
        p_saida_0110-gjahr      = wl_bsid-gjahr.
        p_saida_0110-bldat      = wl_bsid-bldat.
        p_saida_0110-budat      = wl_bsid-budat.
      WHEN 'K'. "Fornecedor
        p_saida_0110-belnr      = wl_bsik-belnr.
        p_saida_0110-buzei      = wl_bsik-buzei.
        p_saida_0110-gjahr      = wl_bsik-gjahr.
        p_saida_0110-bldat      = wl_bsik-bldat.
        p_saida_0110-budat      = wl_bsik-budat.
    ENDCASE.

  ENDIF.

  "fim
  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXPORTING
      i_bdcimmed              = 'X'
    EXCEPTIONS
      session_not_processable = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_get_taxa  USING p_bkpf  LIKE tg_bkpf
                       p_dmbtr TYPE bsid-dmbtr
                       p_dmbe2 TYPE bsid-dmbe2
              CHANGING p_kursf TYPE bkpf-kursf.

  DATA: v_kursf_banco TYPE bkpf-kursf.

  CLEAR: p_kursf.

  PERFORM f_moeda_empresa USING p_bkpf-bukrs
                                'X'.
  IF ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

*  IF ( P_BKPF-WAERS = TG_T001-WAERS ).
*    P_KURSF = ABS( P_BKPF-KURS2 ).
*  ELSE.
*    P_KURSF = ABS( P_BKPF-KURSF ).
*  ENDIF.

  IF ( p_dmbtr > 0 ) AND  ( p_dmbe2 > 0 ).

    TRY.
        p_kursf = p_dmbtr / p_dmbe2.
      CATCH cx_sy_arithmetic_overflow.
    ENDTRY.

  ENDIF.

  PERFORM f_get_bsis_cbanco USING p_bkpf-bukrs
                                  p_bkpf-belnr
                                  p_bkpf-gjahr
                         CHANGING v_kursf_banco.

  IF v_kursf_banco > 0.
    p_kursf = abs( v_kursf_banco ).
  ENDIF.

ENDFORM.

FORM f_atualiza_saldo.

  DATA: vl_tabix TYPE sy-tabix.

  CLEAR: wa_cabecalho_0110-sel_dmbtr,
         wa_cabecalho_0110-sel_dmbe2,
         wa_cabecalho_0110-sld_dmbtr,
         wa_cabecalho_0110-sld_dmbe2.

  PERFORM f_moeda_empresa USING wa_saida_0100-bukrs
                                'X'.
  IF ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  wa_cabecalho_0110-adt_dmbtr = wa_saida_0100-dmbtr.
  wa_cabecalho_0110-adt_dmbe2 = wa_saida_0100-dmbe2.

*------------------------------------------------------------------------*
*  Recalcula valor caso atribuido um valor residual para a partida.
*------------------------------------------------------------------------*
  IF wa_cabecalho_0110-rsd_adt > 0.
    CASE wa_saida_0100-waers.

      WHEN tg_t001-waers.
        wa_cabecalho_0110-adt_dmbtr = wa_saida_0100-dmbtr - wa_cabecalho_0110-rsd_adt.

        IF wa_cabecalho_0110-adt_dmbtr > 0.
          wa_cabecalho_0110-adt_dmbe2 = wa_cabecalho_0110-adt_dmbtr / wa_saida_0100-kursf .
        ELSE.
          wa_cabecalho_0110-adt_dmbtr = wa_saida_0100-dmbtr.
          wa_cabecalho_0110-adt_dmbe2 = wa_saida_0100-dmbe2.
          CLEAR: wa_cabecalho_0110-rsd_adt.
        ENDIF.

      WHEN OTHERS.

        wa_cabecalho_0110-adt_dmbe2 = wa_saida_0100-dmbe2 - wa_cabecalho_0110-rsd_adt.

        IF wa_cabecalho_0110-adt_dmbe2 > 0.
          wa_cabecalho_0110-adt_dmbtr = wa_cabecalho_0110-adt_dmbe2 * wa_saida_0100-kursf .
        ELSE.
          wa_cabecalho_0110-adt_dmbtr = wa_saida_0100-dmbtr.
          wa_cabecalho_0110-adt_dmbe2 = wa_saida_0100-dmbe2.
          CLEAR: wa_cabecalho_0110-rsd_adt.
        ENDIF.

    ENDCASE.

  ENDIF.

*------------------------------------------------------------------------*
*  Processamento contra partidas.
*------------------------------------------------------------------------*

  LOOP AT it_saida_0110 INTO wa_saida_0110.

    vl_tabix = sy-tabix.

    SELECT SINGLE shkzg
      FROM tbsl INTO wa_saida_0110-shkzg
     WHERE bschl = wa_saida_0110-bschl.

    CASE wa_saida_0100-waers.
      WHEN tg_t001-waers.

        IF wa_saida_0110-manual IS NOT INITIAL. "Tratamento partida manual

          IF ( wa_saida_0110-dmbe2 IS INITIAL     ) AND
             ( wa_saida_0110-dmbtr IS NOT INITIAL ).
            wa_saida_0110-dmbe2 = wa_saida_0110-dmbtr / wa_saida_0110-kursf.
          ENDIF.

        ELSE. "Tratamento outras partidas.

          wa_saida_0110-dmbtr = wa_saida_0110-dmbtr_aux.

          IF ( wa_saida_0110-check   IS NOT INITIAL ) AND
             ( wa_saida_0110-vlr_rsd IS INITIAL ).
          ELSE.
            IF ( wa_saida_0110-vlr_rsd <= 0 ) OR
               ( wa_saida_0110-vlr_rsd >= wa_saida_0110-dmbtr ).
              CLEAR: wa_saida_0110-vlr_rsd, wa_saida_0110-check.
            ELSE.
              SUBTRACT wa_saida_0110-vlr_rsd FROM wa_saida_0110-dmbtr.
              wa_saida_0110-check = 'X'.
            ENDIF.
          ENDIF.

          wa_saida_0110-dmbe2 = wa_saida_0110-dmbtr / wa_saida_0110-kursf.

        ENDIF.

      WHEN OTHERS.

        IF wa_saida_0110-manual IS NOT INITIAL. "Tratamento partida manual

          IF ( wa_saida_0110-dmbtr IS INITIAL     ) AND
             ( wa_saida_0110-dmbe2 IS NOT INITIAL ).
            wa_saida_0110-dmbtr = wa_saida_0110-dmbe2 * wa_saida_0110-kursf.
          ENDIF.

        ELSE. "Tratamento outras partidas.

          wa_saida_0110-dmbe2 = wa_saida_0110-dmbe2_aux.

          IF ( wa_saida_0110-check   IS NOT INITIAL ) AND
             ( wa_saida_0110-vlr_rsd IS INITIAL ).
          ELSE.
            IF ( wa_saida_0110-vlr_rsd <= 0 ) OR
               ( wa_saida_0110-vlr_rsd >= wa_saida_0110-dmbe2 ).
              CLEAR: wa_saida_0110-vlr_rsd, wa_saida_0110-check.
            ELSE.
              SUBTRACT wa_saida_0110-vlr_rsd FROM wa_saida_0110-dmbe2.
              wa_saida_0110-check = 'X'.
            ENDIF.
          ENDIF.

          wa_saida_0110-dmbtr = wa_saida_0110-dmbe2 * wa_saida_0110-kursf.

        ENDIF.

    ENDCASE.

    IF wa_saida_0110-check IS NOT INITIAL.

      "IF WA_SAIDA_0110-MANUAL IS NOT INITIAL.

      IF  ( wa_saida_0110-manual     IS NOT INITIAL ) AND
          ( wa_saida_0110-part_princ IS NOT INITIAL ). "Aplicar diferença na partida principal

        IF ( wa_saida_0110-shkzg NE wa_saida_0100-shkzg ). "Se for diferente operação(Cred.Deb.)
          ADD wa_saida_0110-dmbtr TO wa_cabecalho_0110-adt_dmbtr.
          ADD wa_saida_0110-dmbe2 TO wa_cabecalho_0110-adt_dmbe2.
        ELSE.
          SUBTRACT wa_saida_0110-dmbtr FROM wa_cabecalho_0110-adt_dmbtr.
          SUBTRACT wa_saida_0110-dmbe2 FROM wa_cabecalho_0110-adt_dmbe2.
        ENDIF.

      ELSE.

        IF  ( wa_saida_0110-manual IS NOT INITIAL ).
          IF ( wa_saida_0110-shkzg EQ wa_saida_0100-shkzg ). "Se for operação(Cred.Deb.) igual da partida principal
            ADD wa_saida_0110-dmbtr TO wa_cabecalho_0110-sel_dmbtr.
            ADD wa_saida_0110-dmbe2 TO wa_cabecalho_0110-sel_dmbe2.
          ELSE.
            SUBTRACT wa_saida_0110-dmbtr FROM wa_cabecalho_0110-sel_dmbtr.
            SUBTRACT wa_saida_0110-dmbe2 FROM wa_cabecalho_0110-sel_dmbe2.
          ENDIF.
        ELSE.
          IF ( wa_saida_0110-shkzg NE wa_saida_0100-shkzg ). "Se for operação(Cred.Deb.) diferente da partida principal
            ADD wa_saida_0110-dmbtr TO wa_cabecalho_0110-sel_dmbtr.
            ADD wa_saida_0110-dmbe2 TO wa_cabecalho_0110-sel_dmbe2.
          ELSE.
            SUBTRACT wa_saida_0110-dmbtr FROM wa_cabecalho_0110-sel_dmbtr.
            SUBTRACT wa_saida_0110-dmbe2 FROM wa_cabecalho_0110-sel_dmbe2.
          ENDIF.
        ENDIF.

      ENDIF.

      "ELSE.
      "  ADD WA_SAIDA_0110-DMBTR TO WA_CABECALHO_0110-SEL_DMBTR.
      "  ADD WA_SAIDA_0110-DMBE2 TO WA_CABECALHO_0110-SEL_DMBE2.
      "ENDIF.

    ENDIF.

    MODIFY it_saida_0110 FROM wa_saida_0110 INDEX vl_tabix.

  ENDLOOP.

  wa_cabecalho_0110-sld_dmbtr = wa_cabecalho_0110-adt_dmbtr - wa_cabecalho_0110-sel_dmbtr.
  wa_cabecalho_0110-sld_dmbe2 = wa_cabecalho_0110-adt_dmbe2 - wa_cabecalho_0110-sel_dmbe2.




ENDFORM.

FORM f_compensar_adt USING p_saida_0100 TYPE ty_saida_0100.

  DATA: vl_error    TYPE c,
        vl_doc_comp TYPE bsad-belnr,
        vl_msg      TYPE string.

  DATA: wa_sai_0110_tmp TYPE ty_saida_0110.

  FIELD-SYMBOLS <saida_0110> TYPE ty_saida_0110.

  PERFORM f_valida_alv_0110 CHANGING vl_error.

  CHECK vl_error IS INITIAL.

  PERFORM f_atualiza_saldo.

  PERFORM f_moeda_empresa USING p_saida_0100-bukrs
                                'X'.
  IF ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  IF wa_saida_0100-waers = tg_t001-waers.
    IF wa_cabecalho_0110-sld_dmbtr NE 0.
      MESSAGE 'Saldo diferente de 0' TYPE 'W'.
      RETURN.
    ENDIF.
  ELSE.
    IF wa_cabecalho_0110-sld_dmbe2 NE 0.
      MESSAGE 'Saldo diferente de 0' TYPE 'W'.
      RETURN.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Confirma compensaçao dos registros?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  "Check Compensação Imobilizado com Residual.
  DATA(_comp_imob_residual) = ''.
  LOOP AT it_saida_0110 ASSIGNING <saida_0110> WHERE vlr_rsd  NE 0
                                                 AND check IS NOT INITIAL
                                                 AND anln1 IS NOT INITIAL.
    _comp_imob_residual = 'X'.
  ENDLOOP.

  IF ( wa_cabecalho_0110-rsd_adt > 0 ) AND ( wa_saida_0100-anln1 IS NOT INITIAL ).
    _comp_imob_residual = 'X'.
  ENDIF.
  "Fim Check Compensação Imobilizado com Residual

  "Verifica Lançamentos que devem ser desmembrados.
  IF _comp_imob_residual IS INITIAL.
    LOOP AT it_saida_0110 ASSIGNING <saida_0110> WHERE vlr_rsd NE 0
                                                   AND check IS NOT INITIAL.
      PERFORM f_bapi_f51_residual CHANGING <saida_0110>
                                           vl_error.
      IF vl_error IS NOT INITIAL.
        LEAVE TO SCREEN 0.
        RETURN.
      ENDIF.
    ENDLOOP.

    COMMIT WORK.

    "Verifica se Adiantamento deve ser desmembrado.
    IF ( wa_cabecalho_0110-rsd_adt > 0 ).
      CLEAR: wa_sai_0110_tmp.

      MOVE-CORRESPONDING wa_saida_0100 TO wa_sai_0110_tmp.

      IF wa_cabecalho_0110-manter_tp_dc IS INITIAL AND rb_prod IS INITIAL.
        wa_sai_0110_tmp-blart     = 'AB'.
      ENDIF.
      wa_sai_0110_tmp-vlr_rsd   = wa_cabecalho_0110-rsd_adt.
      wa_sai_0110_tmp-dmbtr     = wa_cabecalho_0110-adt_dmbtr.
      wa_sai_0110_tmp-dmbe2     = wa_cabecalho_0110-adt_dmbe2.
      wa_sai_0110_tmp-sgtxt_rsd = wa_cabecalho_0110-sgtxt_rsd.

      wa_sai_0110_tmp-dmbtr_aux = wa_saida_0100-dmbtr. "Valor Original Adiantamento
      wa_sai_0110_tmp-dmbe2_aux = wa_saida_0100-dmbe2. "Valor Original Adiantamento

      PERFORM f_bapi_f51_residual CHANGING wa_sai_0110_tmp
                                           vl_error.
      IF vl_error IS NOT INITIAL.
        LEAVE TO SCREEN 0.
        RETURN.
      ENDIF.

      IF wa_sai_0110_tmp-bl_desmemb IS INITIAL. "Documento Desmembrado
        LEAVE TO SCREEN 0.
        RETURN.
      ENDIF.

      wa_saida_0100-belnr = wa_sai_0110_tmp-belnr.
      wa_saida_0100-buzei = wa_sai_0110_tmp-buzei.
      wa_saida_0100-gjahr = wa_sai_0110_tmp-gjahr.
      wa_saida_0100-bldat = wa_sai_0110_tmp-bldat.
      wa_saida_0100-budat = wa_sai_0110_tmp-budat.
      wa_saida_0100-dmbtr = wa_sai_0110_tmp-dmbtr.
      wa_saida_0100-dmbe2 = wa_sai_0110_tmp-dmbe2.
    ENDIF.
  ENDIF.

  wa_saida_0100-vlr_rsd   = wa_cabecalho_0110-rsd_adt.   "Atribuição Saldo Residual
  wa_saida_0100-dmbtr     = wa_cabecalho_0110-adt_dmbtr.
  wa_saida_0100-dmbe2     = wa_cabecalho_0110-adt_dmbe2.
  wa_saida_0100-sgtxt_rsd = wa_cabecalho_0110-sgtxt_rsd.

  PERFORM f_bapi_f51 USING _comp_imob_residual
                  CHANGING wa_saida_0100
                           vl_error
                           vl_doc_comp.

  IF vl_error IS INITIAL.
    CONCATENATE 'Compensação efetuada com sucesso! Doc.Compensação:' vl_doc_comp
           INTO vl_msg SEPARATED BY space.
    MESSAGE vl_msg TYPE 'S'.
  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_ALV_0110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VL_ERROR  text
*----------------------------------------------------------------------*
FORM f_valida_alv_0110  CHANGING p_error.

  CLEAR: p_error.

  "Lançamentos Manuais
  LOOP AT it_saida_0110 INTO wa_saida_0110 WHERE manual IS NOT INITIAL.

    IF wa_saida_0110-hkont IS INITIAL.
      p_error = 'X'.
      MESSAGE 'Conta é um campo obrigatório!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF wa_saida_0110-bschl IS INITIAL.
      p_error = 'X'.
      MESSAGE 'Chave Lcto. é um campo obrigatório!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF wa_saida_0110-dmbtr IS INITIAL.
      p_error = 'X'.
      MESSAGE 'Valor R$ é um campo obrigatório!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF wa_saida_0110-dmbe2 IS INITIAL.
      p_error = 'X'.
      MESSAGE 'Valor U$ é um campo obrigatório!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF ( wa_saida_0110-umsks IS NOT INITIAL ) AND
       ( wa_saida_0110-zfbdt IS INITIAL ).
      p_error = 'X'.
      MESSAGE 'Data Venc. é um campo obrigatório!' TYPE 'S'.
      RETURN.
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM f_renovar_cons.

  PERFORM f_selecionar_dados.
  CHECK vg_not_found IS INITIAL.
  PERFORM: f_processa_dados,
           f_refresh_alv USING '0100'.

ENDFORM.

FORM f_flag_documentos  USING p_mark.

  FIELD-SYMBOLS: <saida_0110> TYPE ty_saida_0110.
  DATA: vl_msg    TYPE string,
        vl_msg_01 TYPE string.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0110->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  LOOP AT it_sel_rows INTO wa_sel_rows.
    READ TABLE it_saida_0110 ASSIGNING <saida_0110> INDEX wa_sel_rows-index.
    IF sy-subrc NE 0 .
      RETURN.
    ENDIF.

    <saida_0110>-check = p_mark.
  ENDLOOP.

  PERFORM f_atualiza_saldo.

  PERFORM f_moeda_empresa USING <saida_0110>-bukrs
                                'X'.
  IF ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  IF p_mark IS NOT INITIAL.
    IF ( ( wa_cabecalho_0110-sld_dmbtr > 0             ) AND
         ( wa_cabecalho_0110-waers     = tg_t001-waers ) )
        OR
         ( ( wa_cabecalho_0110-sld_dmbe2 > 0     ) AND
           ( wa_cabecalho_0110-waers  NE tg_t001-waers ) ).

      IF wa_saida_0110-waers = tg_t001-waers.
        vl_msg_01 = wa_cabecalho_0110-sld_dmbtr.
      ELSE.
        vl_msg_01 = wa_cabecalho_0110-sld_dmbe2.
      ENDIF.

      CONCATENATE 'Ainda existe um saldo à compensar( Moeda Documento ) no valor de:'
                   vl_msg_01 '! Desejar atribuir esse valor como Saldo Residual p/ o Adiantamento?'
              INTO vl_msg SEPARATED BY space.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = vl_msg
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = var_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF var_answer EQ '1'.
        IF wa_saida_0110-waers = tg_t001-waers.
          wa_cabecalho_0110-rsd_adt = wa_cabecalho_0110-sld_dmbtr.
        ELSE.
          wa_cabecalho_0110-rsd_adt = wa_cabecalho_0110-sld_dmbe2.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.

  PERFORM f_atualiza_saldo.
  LEAVE TO SCREEN 0110.

ENDFORM.

FORM f_get_bsid_comp  TABLES p_tg_bsid_comp STRUCTURE tg_bsid_comp
                       USING i_bukrs TYPE bsid-bukrs
                             i_belnr TYPE bsid-belnr
                             i_buzei TYPE bsid-buzei
                             i_gjahr TYPE bsid-gjahr
                             i_kunnr
                             i_vbel2
                             i_sgtxt
                             i_zuonr
                             i_anln1 TYPE bsid-anln1
                             i_anln2 TYPE bsid-anln2
                             i_dcsim.

  DATA: vl_partida_comp TYPE c.

  CLEAR: p_tg_bsid_comp[].

  LOOP AT tg_bsid_comp WHERE bukrs = i_bukrs
                         AND ( ( kunnr = i_kunnr  AND kunnr IS NOT INITIAL ) OR
                               ( vbel2 = i_vbel2  AND vbel2 IS NOT INITIAL ) OR
                               ( dcsim = i_dcsim  AND dcsim IS NOT INITIAL ) OR
                               ( zuonr = i_zuonr  AND zuonr IS NOT INITIAL )
                             ).

    CLEAR: vl_partida_comp.

    "Se não for do mesmo cliente ou
    "           da mesma O.V ou
    "           da mesmo Doc.Simulador
    IF ( tg_bsid_comp-kunnr NE i_kunnr ) AND
       ( tg_bsid_comp-vbel2 NE i_vbel2 ) AND
       ( tg_bsid_comp-dcsim NE i_dcsim ).
      "Verificar se esta unificando por uma atribuição especifica
      CHECK ( p_zuonrj EQ 'X' ) AND ( p_zuonr-low IS NOT INITIAL ).
    ENDIF.

    IF ( p_ovped-low IS NOT INITIAL ) OR
       ( p_dcsim-low IS NOT INITIAL ) OR
       ( p_sgtxt-low IS NOT INITIAL ) OR
       ( p_zuonr-low IS NOT INITIAL ).
      IF ( ( tg_bsid_comp-vbel2 EQ i_vbel2 AND i_vbel2     IS NOT INITIAL AND p_ovpedj EQ 'X' AND
             tg_bsid_comp-anln1 EQ i_anln1 AND tg_bsid_comp-anln2 EQ i_anln2                  ) OR
           ( tg_bsid_comp-sgtxt IN r_sgtxt AND p_sgtxt-low IS NOT INITIAL AND p_sgtxtj EQ 'X' ) OR
           ( tg_bsid_comp-dcsim EQ i_dcsim AND i_dcsim     IS NOT INITIAL AND p_dcsimj EQ 'X' ) OR
           ( tg_bsid_comp-zuonr IN r_zuonr AND p_zuonr-low IS NOT INITIAL AND p_zuonrj EQ 'X' ) ).
        vl_partida_comp = 'X'.
      ENDIF.
    ELSE.
      IF ( ( tg_bsid_comp-vbel2 EQ i_vbel2 AND i_vbel2 IS NOT INITIAL AND p_ovpedj EQ 'X' AND
             tg_bsid_comp-anln1 EQ i_anln1 AND tg_bsid_comp-anln2 EQ i_anln2              ) OR
           ( tg_bsid_comp-sgtxt EQ i_sgtxt AND i_sgtxt IS NOT INITIAL AND p_sgtxtj EQ 'X' ) OR
           ( tg_bsid_comp-dcsim EQ i_dcsim AND i_dcsim IS NOT INITIAL AND p_dcsimj EQ 'X' ) OR
           ( tg_bsid_comp-zuonr EQ i_zuonr AND i_zuonr IS NOT INITIAL AND p_zuonrj EQ 'X' ) ).
        vl_partida_comp = 'X'.
      ENDIF.
    ENDIF.

    CHECK vl_partida_comp IS NOT INITIAL.

    APPEND tg_bsid_comp TO p_tg_bsid_comp.

  ENDLOOP.

  DELETE p_tg_bsid_comp WHERE bukrs = i_bukrs
                          AND belnr = i_belnr
                          AND gjahr = i_gjahr
                          AND buzei = i_buzei.


ENDFORM.

FORM f_get_bsik_comp  TABLES p_tg_bsik_comp STRUCTURE tg_bsik_comp
                       USING i_bukrs TYPE bsik-bukrs
                             i_belnr TYPE bsik-belnr
                             i_buzei TYPE bsik-buzei
                             i_gjahr TYPE bsik-gjahr
                             i_lifnr
                             i_ebeln
                             i_sgtxt
                             i_zuonr
                             i_anln1 TYPE bsik-anln1
                             i_anln2 TYPE bsik-anln2.

  DATA: vl_partida_comp TYPE c.

  CLEAR: p_tg_bsik_comp[].

  LOOP AT tg_bsik_comp WHERE bukrs = i_bukrs
                         AND ( ( lifnr = i_lifnr  AND lifnr IS NOT INITIAL ) OR
                               ( ebeln = i_ebeln  AND ebeln IS NOT INITIAL ) OR
                               ( zuonr = i_zuonr  AND zuonr IS NOT INITIAL )
                             ).

    CLEAR: vl_partida_comp.

    "Se não for do mesmo fornecedor ou do mesmo pedido
    IF ( tg_bsik_comp-lifnr NE i_lifnr ) AND
       ( tg_bsik_comp-ebeln NE i_ebeln ).
      "Verificar se esta unificando por uma atribuição especifica
      CHECK ( p_zuonrj EQ 'X' ) AND ( p_zuonr-low IS NOT INITIAL ).
    ENDIF.

    IF ( p_ovped-low IS NOT INITIAL ) OR
       ( p_sgtxt-low IS NOT INITIAL ) OR
       ( p_zuonr-low IS NOT INITIAL ).
      IF ( ( tg_bsik_comp-ebeln EQ i_ebeln AND i_ebeln     IS NOT INITIAL AND p_ovpedj EQ 'X' AND
             tg_bsik_comp-anln1 EQ i_anln1 AND tg_bsik_comp-anln2 EQ i_anln2                  ) OR
           ( tg_bsik_comp-sgtxt IN r_sgtxt AND p_sgtxt-low IS NOT INITIAL AND p_sgtxtj EQ 'X' ) OR
           ( tg_bsik_comp-zuonr IN r_zuonr AND p_zuonr-low IS NOT INITIAL AND p_zuonrj EQ 'X' ) ).
        vl_partida_comp = 'X'.
      ENDIF.
    ELSE.
      IF ( ( tg_bsik_comp-ebeln EQ i_ebeln AND i_ebeln IS NOT INITIAL AND p_ovpedj EQ 'X' AND
             tg_bsik_comp-anln1 EQ i_anln1 AND tg_bsik_comp-anln2 EQ i_anln2              ) OR
           ( tg_bsik_comp-sgtxt EQ i_sgtxt AND i_sgtxt IS NOT INITIAL AND p_sgtxtj EQ 'X' ) OR
           ( tg_bsik_comp-zuonr EQ i_zuonr AND i_zuonr IS NOT INITIAL AND p_zuonrj EQ 'X' ) ).
        vl_partida_comp = 'X'.
      ENDIF.
    ENDIF.

    CHECK vl_partida_comp IS NOT INITIAL.

    APPEND tg_bsik_comp TO p_tg_bsik_comp.

  ENDLOOP.

  DELETE p_tg_bsik_comp WHERE bukrs = i_bukrs
                          AND belnr = i_belnr
                          AND gjahr = i_gjahr
                          AND buzei = i_buzei.

ENDFORM.

FORM f_ranges_tp_partida  USING p_tp_conta.


  CLEAR: r_umsks_p, r_umsks_p[],
         r_umskz_p, r_umskz_p[],
         r_shkzg_p, r_shkzg_p[],
         r_umsks_c, r_umsks_c[],
         r_umskz_c, r_umskz_c[].

  CASE p_tp_conta.
    WHEN 'K'. "Fornecedor

      IF ( p_cta_rz IS INITIAL ) AND ( p_cta_nr IS INITIAL ).

        "Partida Principal
        r_umsks_p-sign   = 'I'.
        r_umsks_p-option = 'NE'.
        r_umsks_p-low    = ''.
        APPEND r_umsks_p.

        r_umskz_p-sign   = 'I'.
        r_umskz_p-option = 'NE'.
        r_umskz_p-low    = 'F'.
        APPEND r_umskz_p.

        r_shkzg_p-sign   = 'I'.
        r_shkzg_p-option = 'EQ'.
        r_shkzg_p-low    = 'S'.
        APPEND r_shkzg_p.

        "Outras Partidas
        r_umsks_c-sign   = 'I'.
        r_umsks_c-option = 'EQ'.
        r_umsks_c-low    = ''.
        APPEND r_umsks_c.

        r_umskz_c-sign   = 'I'.
        r_umskz_c-option = 'EQ'.
        r_umskz_c-low    = ''.
        APPEND r_umskz_c.

        "Ranges Unificação Partida
        IF p_ovpedj EQ abap_true.
          IF r_ovped[] IS INITIAL.
            r_ovped-sign   = 'I'.
            r_ovped-option = 'NE'.
            r_ovped-low    = ''.
            APPEND r_ovped.
          ENDIF.
        ENDIF.

        IF p_sgtxtj EQ abap_true.
          IF r_sgtxt[] IS INITIAL.
            r_sgtxt-sign   = 'I'.
            r_sgtxt-option = 'NE'.
            r_sgtxt-low    = ''.
            APPEND r_sgtxt.
          ENDIF.
        ENDIF.

        IF p_zuonrj EQ abap_true.
          IF r_zuonr[] IS INITIAL.
            r_zuonr-sign   = 'I'.
            r_zuonr-option = 'NE'.
            r_zuonr-low    = ''.
            APPEND r_zuonr.
          ENDIF.
        ENDIF.

      ELSE.

        IF ( p_cta_rz IS NOT INITIAL ) AND ( p_cta_nr IS INITIAL ).

          "Partida Principal
          r_umsks_p-sign   = 'I'.
          r_umsks_p-option = 'NE'.
          r_umsks_p-low    = ''.
          APPEND r_umsks_p.

          r_umskz_p-sign   = 'I'.
          r_umskz_p-option = 'NE'.
          r_umskz_p-low    = 'F'.
          APPEND r_umskz_p.

        ELSEIF ( p_cta_nr IS NOT INITIAL ) AND ( p_cta_rz IS INITIAL ).

          "Partida Principal
          r_umsks_p-sign   = 'I'.
          r_umsks_p-option = 'EQ'.
          r_umsks_p-low    = ''.
          APPEND r_umsks_p.

          r_umskz_p-sign   = 'I'.
          r_umskz_p-option = 'EQ'.
          r_umskz_p-low    = ''.
          APPEND r_umskz_p.

        ENDIF.

      ENDIF.


    WHEN 'D'. "Ciente

      IF ( p_cta_rz IS INITIAL ) AND ( p_cta_nr IS INITIAL ).

        "Partida Principal
        r_umsks_p-sign   = 'I'.
        r_umsks_p-option = 'NE'.
        r_umsks_p-low    = ''.
        APPEND r_umsks_p.

        r_umskz_p-sign   = 'I'.
        r_umskz_p-option = 'NE'.
        r_umskz_p-low    = 'F'.
        APPEND r_umskz_p.

        r_shkzg_p-sign   = 'I'.
        r_shkzg_p-option = 'EQ'.
        r_shkzg_p-low    = 'H'.
        APPEND r_shkzg_p.

        "Outras Partidas
        r_umsks_c-sign   = 'I'.
        r_umsks_c-option = 'EQ'.
        r_umsks_c-low    = ''.
        APPEND r_umsks_c.

        r_umskz_c-sign   = 'I'.
        r_umskz_c-option = 'EQ'.
        r_umskz_c-low    = ''.
        APPEND r_umskz_c.

        "Ranges Unificação Partida
        IF ( p_ovpedj EQ abap_true ) OR
           ( p_dcsimj EQ abap_true ).
          IF r_ovped[] IS INITIAL.
            r_ovped-sign   = 'I'.
            r_ovped-option = 'NE'.
            r_ovped-low    = ''.
            APPEND r_ovped.
          ENDIF.
        ENDIF.

        IF p_sgtxtj EQ abap_true.
          IF r_sgtxt[] IS INITIAL.
            r_sgtxt-sign   = 'I'.
            r_sgtxt-option = 'NE'.
            r_sgtxt-low    = ''.
            APPEND r_sgtxt.
          ENDIF.
        ENDIF.

        IF p_zuonrj EQ abap_true.
          IF r_zuonr[] IS INITIAL.
            r_zuonr-sign   = 'I'.
            r_zuonr-option = 'NE'.
            r_zuonr-low    = ''.
            APPEND r_zuonr.
          ENDIF.
        ENDIF.

        IF ( p_dcsimj EQ abap_true ).
          IF r_dcsim[] IS INITIAL.
            r_dcsim-sign   = 'I'.
            r_dcsim-option = 'NE'.
            r_dcsim-low    = ''.
            APPEND r_dcsim.
          ENDIF.
        ENDIF.

      ELSE.

        IF ( p_cta_rz IS NOT INITIAL ) AND ( p_cta_nr IS INITIAL ).

          "Partida Principal
          r_umsks_p-sign   = 'I'.
          r_umsks_p-option = 'NE'.
          r_umsks_p-low    = ''.
          APPEND r_umsks_p.

          r_umskz_p-sign   = 'I'.
          r_umskz_p-option = 'NE'.
          r_umskz_p-low    = 'F'.
          APPEND r_umskz_p.

        ELSEIF ( p_cta_nr IS NOT INITIAL ) AND ( p_cta_rz IS INITIAL ).

          "Partida Principal
          r_umsks_p-sign   = 'I'.
          r_umsks_p-option = 'EQ'.
          r_umsks_p-low    = ''.
          APPEND r_umsks_p.

          r_umskz_p-sign   = 'I'.
          r_umskz_p-option = 'EQ'.
          r_umskz_p-low    = ''.
          APPEND r_umskz_p.

        ENDIF.

      ENDIF.



  ENDCASE.

ENDFORM.

FORM f_moeda_empresa USING p_bukrs TYPE t001-bukrs
                           p_msg   TYPE c.

  CLEAR: tg_t001.

  READ TABLE tg_t001 WITH KEY bukrs = p_bukrs.

  IF ( sy-subrc NE 0 ) OR ( tg_t001-waers IS INITIAL ) OR
     ( tg_t001-waers2 IS INITIAL ) OR ( p_bukrs IS INITIAL ).
    CLEAR: tg_t001.
    IF p_msg = 'X'.
      MESSAGE |Informações referente a moeda da empresa: { p_bukrs }, não encontrado ou incompleto!| TYPE 'S'.
    ENDIF.
    sy-subrc = 4.
  ENDIF.

ENDFORM.

FORM f_aplic_text_def .

  LOOP AT it_saida_0110 ASSIGNING FIELD-SYMBOL(<saida_0110>).
    IF <saida_0110>-sgtxt_rsd NE 'Desmembramento Fatura'.
      <saida_0110>-sgtxt_rsd = 'Desmembramento Fatura'.
    ELSE.
      <saida_0110>-sgtxt_rsd = <saida_0110>-sgtxt.
    ENDIF.
  ENDLOOP.

  wa_cabecalho_0110-sgtxt_rsd = wa_saida_0100-sgtxt.

  LEAVE TO SCREEN 0110.

ENDFORM.


FORM f_get_bsis_cbanco USING p_bukrs     TYPE bsas-bukrs
                             p_belnr     TYPE bsas-belnr
                             p_gjahr     TYPE bsas-gjahr
                    CHANGING p_kursf     LIKE bkpf-kursf.

  CLEAR: tg_bsis_cbanco, p_kursf.

  READ TABLE tg_bsis_cbanco WITH KEY bukrs = p_bukrs
                                     gjahr = p_gjahr
                                     belnr = p_belnr BINARY SEARCH.
  IF ( sy-subrc = 0 ) AND
     ( tg_bsis_cbanco-dmbtr > 0 ) AND
     ( tg_bsis_cbanco-dmbe2 > 0 ).

    TRY.
        p_kursf = tg_bsis_cbanco-dmbtr / tg_bsis_cbanco-dmbe2.
      CATCH cx_sy_arithmetic_overflow.
    ENDTRY.

    IF ( tg_bsis_cbanco-waers IS NOT INITIAL ) AND
       ( tg_bsis_cbanco-waers NE 'BRL'       ) AND
       ( tg_bsis_cbanco-waers NE 'USD'       ) AND
       ( tg_bsis_cbanco-wrbtr > 0            ).

      TRY.
          p_kursf = tg_bsis_cbanco-dmbtr / tg_bsis_cbanco-wrbtr.
          "P_KURS2 = TG_BSIS_CBANCO-DMBE2 / TG_BSIS_CBANCO-WRBTR.
        CATCH cx_sy_arithmetic_overflow.
      ENDTRY.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_call_screen_0120.

  PERFORM f_seleciona_dados_0120.

  CALL SCREEN 0120 STARTING AT 2 2 ENDING AT 178 25.

ENDFORM.

FORM f_montar_layout_log_erro.
  REFRESH estrutura.
  PERFORM f_montar_estrutura USING:
     01  ''   ''            'TG_ZIB_ERR' 'DT_ATUALIZACAO'     'Data'         '10' '' '' ,
     02  ''   ''            'TG_ZIB_ERR' 'HR_ATUALIZACAO'     'Hora'         '10' '' '' ,
     03  ''   ''            'TG_ZIB_ERR' 'TYPE'               'Tipo'         '10' '' '' ,
     04  ''   ''            'TG_ZIB_ERR' 'NUM'                'Num.'         '10' '' '' ,
     05  ''   ''            'TG_ZIB_ERR' 'MESSAGE'            'Mensagem'     '10' '' '' ,
     06  ''   ''            'TG_ZIB_ERR' 'MESSAGE_V1'         'Msg.1'        '10' '' '' ,
     07  ''   ''            'TG_ZIB_ERR' 'MESSAGE_V2'         'Msg.2'        '10' '' '' ,
     08  ''   ''            'TG_ZIB_ERR' 'MESSAGE_V3'         'Msg.3'        '10' '' '' ,
     09  ''   ''            'TG_ZIB_ERR' 'MESSAGE_V4'         'Msg.4'        '10' '' '' .

ENDFORM.                    " MONTAR_LAYOUT


FORM f_montar_estrutura USING VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_hotspot)
                              VALUE(p_just).

  CLEAR wa_estrutura.

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


  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM f_seleciona_dados_0120 .

  CLEAR: it_saida_0120[].

  IF p_augdt-low IS INITIAL.
    MESSAGE 'Selecione um data de compensação!' TYPE 'S'.
    EXIT.
  ENDIF.

  SELECT *
    FROM zfit0139 INTO TABLE @DATA(tg_0139)
   WHERE bukrs IN @p_bukrs
     AND augdt IN @p_augdt.

  LOOP AT tg_0139 INTO DATA(_wl_0139).
    CLEAR: wa_saida_0120.
    MOVE-CORRESPONDING _wl_0139 TO wa_saida_0120.

    IF ( _wl_0139-belnr_ger IS NOT INITIAL ) AND
       ( _wl_0139-stblg_ger IS INITIAL     ) AND "Não foi estornado
       ( _wl_0139-anulado   IS INITIAL     ).    "Não foi anulado
      wa_saida_0120-st_ctb = icon_led_green.
    ELSEIF ( _wl_0139-belnr_ger IS INITIAL ) AND
           ( _wl_0139-stblg_ger IS INITIAL ) AND "Não foi estornado
           ( _wl_0139-anulado   IS INITIAL ) AND "Não foi anulado.
           ( _wl_0139-st_proc   EQ '2'     ).    "Com erro
      wa_saida_0120-st_ctb = icon_led_red.
    ELSEIF ( _wl_0139-belnr_ger  IS INITIAL ) AND
           ( _wl_0139-stblg_ger  IS INITIAL ) AND "Não foi estornado
           ( _wl_0139-anulado    IS INITIAL ) AND "Não foi anulado.
           ( _wl_0139-st_proc    IS INITIAL ).    "Com erro
      wa_saida_0120-st_ctb = icon_led_yellow.
    ELSEIF ( _wl_0139-belnr_ger IS NOT INITIAL ) AND
           ( _wl_0139-stblg_ger IS NOT INITIAL ) AND "Não foi estornado
           ( _wl_0139-anulado   IS INITIAL     ).    "Não foi anulado
      wa_saida_0120-st_ctb = icon_led_inactive.
    ENDIF.

    APPEND wa_saida_0120 TO it_saida_0120.
  ENDLOOP.

  SORT it_saida_0120 BY anulado.

ENDFORM.

FORM f_add_part_residual TABLES p_ftpost      STRUCTURE ftpost
                          USING p_saida_0110  TYPE ty_saida_0110
                       CHANGING p_count_ft    TYPE ftpost-count
                                p_error       TYPE c.

  DATA: wl_vlrc(16),
        vdata_venc(10),
        wl_vlrn        TYPE p DECIMALS 2,
        wa_zfit0154    TYPE zfit0154.

  CLEAR: p_error.

  IF p_saida_0110-st_comp  NE '3'.
    IF p_saida_0110-vlr_rsd <= 0.
      MESSAGE 'Valor Residual inconsistente!' TYPE 'S'.
      p_error = 'X'.
      RETURN.
    ENDIF.
  ENDIF.

  IF p_saida_0110-kursf <= 0.
    MESSAGE 'Taxa para gerar residual não encontrada!' TYPE 'S'.
    p_error = 'X'.
    RETURN.
  ENDIF.

  IF p_saida_0110-st_comp  NE '3'.
    CASE p_saida_0110-koart.
      WHEN 'D'. "Cliente
        IF ( p_saida_0110-bschl NE '01' ) AND
           ( p_saida_0110-bschl NE '11' ) AND
           ( p_saida_0110-bschl NE '09' ) AND
           ( p_saida_0110-bschl NE '19' ).
          DATA(_erro_chave) = 'X'.
        ENDIF.
      WHEN 'K'. "Fornecedor
        IF ( p_saida_0110-bschl NE '21' ) AND
           ( p_saida_0110-bschl NE '31' ) AND
           ( p_saida_0110-bschl NE '29' ) AND
           ( p_saida_0110-bschl NE '39' ).
          _erro_chave = 'X'.
        ENDIF.
    ENDCASE.
  ELSE.
    SELECT SINGLE *
      FROM lfa1
      INTO @DATA(w_lfa1)
      WHERE lifnr = @p_saida_0110-parid
      AND   vbund = 'SOCIOS'.

    IF sy-subrc = 0.
      SELECT SINGLE *
               INTO wa_zfit0154
               FROM zfit0154
               WHERE tipo = 'P'
               AND   fg_soc = 'X'.
    ELSE.
      SELECT SINGLE *
         INTO wa_zfit0154
         FROM zfit0154
         WHERE tipo = 'P'
         AND   fg_soc = ''.
    ENDIF.

    IF p_saida_0110-vlr_rsdp < 0.
      p_saida_0110-bschl = '40'.
      p_saida_0110-parid = wa_zfit0154-cta_desc_conc.
    ELSE.
      p_saida_0110-bschl = '50'.
      p_saida_0110-parid = wa_zfit0154-cta_desc_obtido.
    ENDIF.

  ENDIF.

  IF _erro_chave IS NOT INITIAL.
    MESSAGE 'Chave Lançamento não configurada para deixar Saldo Residual!' TYPE 'S'.
    p_error = 'X'.
    RETURN.
  ENDIF.

  PERFORM f_moeda_empresa USING p_saida_0110-bukrs
                                'X'.
  IF ( sy-subrc NE 0 ).
    p_error = 'X'.
    RETURN.
  ENDIF.

  IF p_saida_0110-st_comp  NE '3'.
    IF p_saida_0110-waers = tg_t001-waers.
      IF ( p_saida_0110-dmbtr + p_saida_0110-vlr_rsd ) NE p_saida_0110-dmbtr_aux.
        MESSAGE 'Saldo Residual inconsistente!' TYPE 'S'.
        p_error = 'X'.
        RETURN.
      ENDIF.
    ELSE.
      IF ( p_saida_0110-dmbe2 + p_saida_0110-vlr_rsd ) NE p_saida_0110-dmbe2_aux.
        MESSAGE 'Saldo Residual inconsistente!' TYPE 'S'.
        p_error = 'X'.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR: wl_vlrn, wl_vlrc, vdata_venc.

  CONCATENATE p_saida_0110-zfbdt+6(2) p_saida_0110-zfbdt+4(2) p_saida_0110-zfbdt(4) INTO vdata_venc SEPARATED BY '.'.

  ADD 1 TO p_count_ft.

  IF p_saida_0110-st_comp  NE '3'.
    wl_vlrn = abs( p_saida_0110-vlr_rsd ).
  ELSE.
    wl_vlrn = abs( p_saida_0110-vlr_rsdp ).
  ENDIF.

  WRITE: wl_vlrn TO wl_vlrc.

  p_ftpost-stype = 'P'.
  p_ftpost-count = p_count_ft .

  p_ftpost-fnam = 'RF05A-NEWBS'.
  p_ftpost-fval =  p_saida_0110-bschl.
  APPEND p_ftpost.

  p_ftpost-fnam = 'BSEG-HKONT'.
  p_ftpost-fval = p_saida_0110-parid.
  APPEND p_ftpost.

  p_ftpost-fnam = 'BSEG-SGTXT'.
  p_ftpost-fval = p_saida_0110-sgtxt_rsd.
  APPEND p_ftpost.

  p_ftpost-fnam = 'BSEG-ZUONR'.
  p_ftpost-fval = p_saida_0110-zuonr.
  APPEND p_ftpost.

  IF p_saida_0110-ebelp IS NOT INITIAL.
    p_ftpost-fval = p_ftpost-fval && p_saida_0110-ebelp.
  ENDIF.
  APPEND p_ftpost.

  IF p_saida_0110-st_comp  NE '3'.
    p_ftpost-fnam = 'BSEG-GSBER'.
    p_ftpost-fval = p_saida_0110-gsber.
    APPEND p_ftpost.

    p_ftpost-fnam = 'BSEG-HZUON'.
    p_ftpost-fval =  p_saida_0110-ovped.
    APPEND p_ftpost.

    IF p_saida_0110-zfbdt IS NOT INITIAL.
      p_ftpost-fnam = 'BSEG-ZFBDT'.
      p_ftpost-fval = vdata_venc.
      APPEND p_ftpost.

      IF ( p_saida_0110-umsks IS INITIAL ).
        p_ftpost-fnam = 'BSEG-ZBD1T'.
        p_ftpost-fval = p_saida_0110-zbd1t.
        CONDENSE p_ftpost-fval NO-GAPS.
        APPEND p_ftpost.
      ENDIF.
    ENDIF.
  ELSE.
    p_ftpost-fnam = 'BSEG-BUPLA'.
    p_ftpost-fval = p_saida_0110-gsber.
    APPEND p_ftpost.
  ENDIF.

  IF p_saida_0110-umsks IS NOT INITIAL. "Adiantamento
    p_ftpost-fnam = 'RF05A-NEWUM'.
    p_ftpost-fval = p_saida_0110-umskz.
    APPEND p_ftpost.

    IF ( p_saida_0110-anln1 IS NOT INITIAL ).
      p_ftpost-fnam = 'BSEG-ANLN1'.
      p_ftpost-fval = p_saida_0110-anln1.
      APPEND p_ftpost.

      IF p_saida_0110-anln2 IS NOT INITIAL.
        p_ftpost-fnam = 'BSEG-ANLN2'.
        p_ftpost-fval = p_saida_0110-anln2.
        APPEND p_ftpost.
      ENDIF.
    ENDIF.
  ENDIF.

  p_ftpost-fnam = 'BSEG-WRBTR'.
  p_ftpost-fval =  wl_vlrc.
  APPEND p_ftpost.

  IF p_saida_0110-waers NE tg_t001-waers.
    wl_vlrn = wl_vlrn * abs( p_saida_0110-kursf ).
    WRITE: wl_vlrn TO wl_vlrc.
    p_ftpost-fnam = 'BSEG-DMBTR'.
    p_ftpost-fval =  wl_vlrc.
    APPEND p_ftpost.
  ELSE.
    wl_vlrn = wl_vlrn / abs( p_saida_0110-kursf ).
    WRITE: wl_vlrn TO wl_vlrc.
    p_ftpost-fnam = 'BSEG-DMBE2'.
    p_ftpost-fval =  wl_vlrc.
    APPEND p_ftpost.
  ENDIF.

ENDFORM.

FORM f_atrib_doc_simulador  USING p_vbeln
                         CHANGING p_dcsim.

  CLEAR: p_dcsim.

  CHECK p_vbeln IS NOT INITIAL.

  READ TABLE tg_zsdt0041 WITH KEY vbeln = p_vbeln.
  IF ( sy-subrc EQ 0 ) AND ( tg_zsdt0041-doc_simulacao IS NOT INITIAL ).
    p_dcsim = tg_zsdt0041-doc_simulacao.
    EXIT.
  ENDIF.

  READ TABLE tg_zsdt0090 WITH KEY vbeln = p_vbeln.
  IF ( sy-subrc EQ 0 ) AND ( tg_zsdt0090-doc_simulacao IS NOT INITIAL ).
    p_dcsim = tg_zsdt0090-doc_simulacao.
    EXIT.
  ENDIF.

  "Check se é uma Devolução/Recusa
  READ TABLE tg_vbfa_rd WITH KEY vbeln = p_vbeln.

  CHECK sy-subrc EQ 0.

  READ TABLE tg_zsdt0041 WITH KEY vbeln = tg_vbfa_rd-vbelv.
  IF ( sy-subrc EQ 0 ) AND ( tg_zsdt0041-doc_simulacao IS NOT INITIAL ).
    p_dcsim = tg_zsdt0041-doc_simulacao.
    EXIT.
  ENDIF.

  READ TABLE tg_zsdt0090 WITH KEY vbeln = tg_vbfa_rd-vbelv.
  IF ( sy-subrc EQ 0 ) AND ( tg_zsdt0090-doc_simulacao IS NOT INITIAL ).
    p_dcsim = tg_zsdt0090-doc_simulacao.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_sel_part_comp USING p_tp_conta.

  CASE p_tp_conta.
    WHEN 'K'. "Fornecedor

      IF ( p_ovped-low IS NOT INITIAL ) OR
         ( p_sgtxt-low IS NOT INITIAL ) OR
         ( p_zuonr-low IS NOT INITIAL ).

        SELECT *
          FROM bsik INTO CORRESPONDING FIELDS OF TABLE tg_bsik_comp
         WHERE bukrs IN r_bukrs
           AND umsks IN r_umsks_c
           AND umskz IN r_umskz_c
           AND ebeln IN r_ovped
           AND sgtxt IN r_sgtxt
           AND zuonr IN r_zuonr
           AND blart NE 'VC'
           AND dmbtr > 0
           AND dmbe2 > 0.

      ELSE.

        SELECT *
          FROM bsik INTO CORRESPONDING FIELDS OF TABLE tg_bsik_comp
          FOR ALL ENTRIES IN tg_bsik_adt
         WHERE bukrs EQ tg_bsik_adt-bukrs
           AND ( ( lifnr EQ tg_bsik_adt-lifnr ) OR
                 ( ebeln EQ tg_bsik_adt-ebeln AND ebeln NE '' ) )
           AND umsks IN r_umsks_c
           AND umskz IN r_umskz_c
           AND ebeln IN r_ovped
           AND sgtxt IN r_sgtxt
           AND zuonr IN r_zuonr
           AND blart NE 'VC'
           AND ( ( ebeln = tg_bsik_adt-ebeln AND ebeln NE '' ) OR
                 ( sgtxt = tg_bsik_adt-sgtxt AND sgtxt NE '' ) OR
                 ( zuonr = tg_bsik_adt-zuonr AND zuonr NE '' ) )
           AND dmbtr > 0
           AND dmbe2 > 0.
      ENDIF.

    WHEN 'D'. "Cliente

      IF ( p_ovped-low IS NOT INITIAL ) OR
         ( p_sgtxt-low IS NOT INITIAL ) OR
         ( p_zuonr-low IS NOT INITIAL ) OR
         ( p_dcsimj    EQ abap_true   ).

        SELECT *
          FROM bsid INTO CORRESPONDING FIELDS OF TABLE tg_bsid_comp
         WHERE bukrs IN r_bukrs
           AND umsks IN r_umsks_c
           AND umskz IN r_umskz_c
           AND vbel2 IN r_ovped
           AND sgtxt IN r_sgtxt
           AND zuonr IN r_zuonr
           AND blart NE 'VC'
           AND dmbtr > 0
           AND dmbe2 > 0.

      ELSE.

        SELECT *
          FROM bsid INTO CORRESPONDING FIELDS OF TABLE tg_bsid_comp
          FOR ALL ENTRIES IN tg_bsid_adt
         WHERE bukrs EQ tg_bsid_adt-bukrs
           AND ( ( kunnr EQ tg_bsid_adt-kunnr ) OR
                 ( vbel2 EQ tg_bsid_adt-vbel2 AND vbel2 NE '' ) )
           AND umsks IN r_umsks_c
           AND umskz IN r_umskz_c
           AND vbel2 IN r_ovped
           AND sgtxt IN r_sgtxt
           AND zuonr IN r_zuonr
           AND blart NE 'VC'
           AND ( ( vbel2 = tg_bsid_adt-vbel2 AND vbel2 NE '' ) OR
                 ( sgtxt = tg_bsid_adt-sgtxt AND sgtxt NE '' ) OR
                 ( zuonr = tg_bsid_adt-zuonr AND zuonr NE '' ) )
           AND dmbtr > 0
           AND dmbe2 > 0.

      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_desc USING p_saida_0100 TYPE ty_saida_0100..
  DATA wa_zfit0154 TYPE zfit0154.
  "
  LOOP AT it_saida_0110 INTO DATA(w110) WHERE check IS NOT INITIAL AND belnr IS NOT INITIAL.

  ENDLOOP.

  IF w110-parid IS INITIAL.
    MESSAGE 'Selecione partidas a compensar!' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
       FROM lfa1
       INTO @DATA(w_lfa1)
       WHERE lifnr = @w110-parid
       AND   vbund = 'SOCIOS'.

  IF sy-subrc = 0.
    SELECT SINGLE *
             INTO wa_zfit0154
             FROM zfit0154
             WHERE tipo = 'P'
             AND   fg_soc = 'X'.
  ELSE.
    SELECT SINGLE *
       INTO wa_zfit0154
       FROM zfit0154
       WHERE tipo = 'P'
       AND   fg_soc = ''.
  ENDIF.

  CLEAR w110.
  LOOP AT it_saida_0110 INTO w110 WHERE manual IS NOT INITIAL.

  ENDLOOP.
  IF w110-manual = 'X'.
    MESSAGE 'Desconto já incluido!' TYPE 'I'.
    EXIT.
  ENDIF.



  CLEAR: wa_saida_0110, gt_estilo[].

  IF wa_cabecalho_0110-sld_dmbtr > 0.
    wa_saida_0110-bschl = '40'.
    wa_saida_0110-hkont = wa_zfit0154-cta_desc_conc.
  ELSE.
    wa_saida_0110-bschl = '50'.
    wa_saida_0110-hkont = wa_zfit0154-cta_desc_obtido.
  ENDIF.
  wa_saida_0110-manual = 'X'.
  wa_saida_0110-ic_manual = icon_checked.
  wa_saida_0110-kursf  = wa_saida_0100-kursf.
  wa_saida_0110-koart  = wa_saida_0100-koart.
  wa_saida_0110-bukrs  = wa_saida_0100-bukrs.
  wa_saida_0110-waers  = wa_saida_0100-waers.
  wa_saida_0110-dmbtr  = abs( wa_cabecalho_0110-sld_dmbtr ).
  wa_saida_0110-dmbe2  = abs( wa_cabecalho_0110-sld_dmbtr / wa_saida_0100-kursf ).
  wa_saida_0110-sgtxt_rsd  = 'Desconto Tolerância de fechamento de lote'.
  wa_saida_0110-sgtxt      = 'Desconto Tolerância de fechamento de lote'.

  wl_estilo-fieldname    = 'VLR_RSD'.
  wl_estilo-style        = cl_gui_alv_grid=>mc_style_disabled.
  APPEND wl_estilo TO gt_estilo.

  INSERT LINES OF gt_estilo INTO TABLE wa_saida_0110-estilo.

  APPEND wa_saida_0110 TO it_saida_0110.

ENDFORM.
