FUNCTION z_01_dre_saldos_incluir.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LC_DATA_INI) TYPE  TIMESTAMP
*"     REFERENCE(LC_DATA_FIM) TYPE  TIMESTAMP
*"     REFERENCE(P_BUKRS) TYPE  BUKRS OPTIONAL
*"  TABLES
*"      IT_DRE04 STRUCTURE  ZGLT_DRE_04
*"  EXCEPTIONS
*"      ERRO_SQL
*"----------------------------------------------------------------------
  DATA: exc_ref    TYPE REF TO cx_sy_native_sql_error,
        error_text TYPE string,
        "IT_DRE01   TYPE TABLE OF ZGLT_DRE_01,
        it_dre02   TYPE TABLE OF zglt_dre_02,
        "IT_DRE03   TYPE TABLE OF ZGLT_DRE_03,
        "WA_DRE01   TYPE ZGLT_DRE_01,
        wa_dre02   TYPE zglt_dre_02,
        "WA_DRE03   TYPE ZGLT_DRE_03,
        wa_dre04   TYPE zglt_dre_04,
        vg_tabix   TYPE sy-tabix.

  DATA: lva_gjahr_min      TYPE zglt_dre_04-gjahr,
        lva_poper_min      TYPE zglt_dre_04-poper,
        lva_anomes_min     TYPE c LENGTH 7,
        lva_anomes_min_reg TYPE c LENGTH 7.

  DATA: rg_bukrs TYPE RANGE OF bukrs,
        wa_bukrs LIKE LINE OF rg_bukrs.

  IF p_bukrs IS NOT INITIAL.
    wa_bukrs-sign   = 'I'.
    wa_bukrs-option = 'EQ'.
    wa_bukrs-low    = p_bukrs.
    wa_bukrs-high   = p_bukrs.
    APPEND wa_bukrs TO rg_bukrs.
  ENDIF.

  CHECK it_dre04[] IS NOT INITIAL.

*  SELECT * INTO TABLE IT_DRE01
*    FROM ZGLT_DRE_01
*     FOR ALL ENTRIES IN IT_DRE04
*   WHERE BUKRS EQ IT_DRE04-BUKRS
*     AND GJAHR EQ IT_DRE04-GJAHR
*     AND SAKNR EQ IT_DRE04-SAKNR
*     AND KTOPL EQ IT_DRE04-KTOPL
*     AND KOKRS EQ IT_DRE04-KOKRS
*     AND KOSTL EQ IT_DRE04-KOSTL
*     AND KOSAR EQ IT_DRE04-KOSAR
*     AND PRCTR EQ IT_DRE04-PRCTR
*     AND MATNR EQ IT_DRE04-MATNR
*     AND MATKL EQ IT_DRE04-MATKL
*     AND AUFNR EQ IT_DRE04-AUFNR
*     AND VBUND EQ IT_DRE04-VBUND.

  SELECT * INTO TABLE it_dre02
    FROM zglt_dre_02
     FOR ALL ENTRIES IN it_dre04
   WHERE bukrs IN rg_bukrs
     AND bukrs EQ it_dre04-bukrs
     AND gjahr EQ it_dre04-gjahr
     AND poper EQ it_dre04-poper
     AND saknr EQ it_dre04-saknr
     AND ktopl EQ it_dre04-ktopl
     AND kokrs EQ it_dre04-kokrs
     AND kostl EQ it_dre04-kostl
     AND kosar EQ it_dre04-kosar
     AND prctr EQ it_dre04-prctr
     AND matnr EQ it_dre04-matnr
     AND matkl EQ it_dre04-matkl
     AND aufnr EQ it_dre04-aufnr
     AND vbund EQ it_dre04-vbund.

*  SELECT * INTO TABLE IT_DRE03
*    FROM ZGLT_DRE_03
*     FOR ALL ENTRIES IN IT_DRE04
*   WHERE BUKRS EQ IT_DRE04-BUKRS
*     AND GJAHR EQ IT_DRE04-GJAHR
*     AND POPER EQ IT_DRE04-POPER
*     AND BUDAT EQ IT_DRE04-BUDAT
*     AND SAKNR EQ IT_DRE04-SAKNR
*     AND KTOPL EQ IT_DRE04-KTOPL
*     AND KOKRS EQ IT_DRE04-KOKRS
*     AND SHKZG EQ IT_DRE04-SHKZG
*     AND KOSTL EQ IT_DRE04-KOSTL
*     AND KOSAR EQ IT_DRE04-KOSAR
*     AND PRCTR EQ IT_DRE04-PRCTR
*     AND MATNR EQ IT_DRE04-MATNR
*     AND MATKL EQ IT_DRE04-MATKL
*     AND AUFNR EQ IT_DRE04-AUFNR
*     AND VBUND EQ IT_DRE04-VBUND.

*  SORT IT_DRE01 BY BUKRS GJAHR SAKNR KTOPL KOKRS KOSTL KOSAR PRCTR MATNR MATKL AUFNR VBUND.
  SORT it_dre02 BY bukrs gjahr poper saknr ktopl kokrs kostl kosar prctr matnr matkl aufnr vbund.
*  SORT IT_DRE03 BY BUKRS GJAHR POPER BUDAT SAKNR KTOPL KOKRS SHKZG KOSTL KOSAR PRCTR MATNR MATKL AUFNR VBUND.

  CLEAR: lva_anomes_min.

  LOOP AT it_dre04 INTO wa_dre04.

    CLEAR: "WA_DRE01,
           wa_dre02.
    "WA_DRE03.

    lva_anomes_min_reg = wa_dre04-gjahr && wa_dre04-poper.

    IF lva_anomes_min IS INITIAL.
      lva_anomes_min = lva_anomes_min_reg.
    ELSEIF lva_anomes_min_reg < lva_anomes_min.
      lva_anomes_min = lva_anomes_min_reg.
    ENDIF.

    "**************************************************************************************
*    READ TABLE IT_DRE01 INTO WA_DRE01
*                        WITH KEY BUKRS = WA_DRE04-BUKRS
*                                 GJAHR = WA_DRE04-GJAHR
*                                 SAKNR = WA_DRE04-SAKNR
*                                 KTOPL = WA_DRE04-KTOPL
*                                 KOKRS = WA_DRE04-KOKRS
*                                 KOSTL = WA_DRE04-KOSTL
*                                 KOSAR = WA_DRE04-KOSAR
*                                 PRCTR = WA_DRE04-PRCTR
*                                 MATNR = WA_DRE04-MATNR
*                                 MATKL = WA_DRE04-MATKL
*                                 AUFNR = WA_DRE04-AUFNR
*                                 VBUND = WA_DRE04-VBUND
*                                 BINARY SEARCH.
*    IF SY-SUBRC IS INITIAL.
*      VG_TABIX = SY-TABIX.
*      WA_DRE01-VLTSL = WA_DRE01-VLTSL + WA_DRE04-VLTSL.
*      WA_DRE01-VLHSL = WA_DRE01-VLHSL + WA_DRE04-VLHSL.
*      WA_DRE01-VLKSL = WA_DRE01-VLKSL + WA_DRE04-VLKSL.
*      WA_DRE01-VLOSL = WA_DRE01-VLOSL + WA_DRE04-VLOSL.
*      WA_DRE01-QTMSL = WA_DRE01-QTMSL + WA_DRE04-QTMSL.
*      MODIFY IT_DRE01 INDEX VG_TABIX FROM WA_DRE01 TRANSPORTING VLTSL VLHSL VLKSL VLOSL QTMSL.
*    ELSE.
*      MOVE-CORRESPONDING WA_DRE04 TO WA_DRE01.
*      WA_DRE01-VLTSL = WA_DRE04-VLTSL.
*      WA_DRE01-VLHSL = WA_DRE04-VLHSL.
*      WA_DRE01-VLKSL = WA_DRE04-VLKSL.
*      WA_DRE01-VLOSL = WA_DRE04-VLOSL.
*      WA_DRE01-QTMSL = WA_DRE04-QTMSL.
*      APPEND WA_DRE01 TO IT_DRE01.
*      SORT IT_DRE01 BY BUKRS GJAHR SAKNR KTOPL KOKRS KOSTL KOSAR PRCTR MATNR MATKL AUFNR VBUND.
*    ENDIF.
    "**************************************************************************************
    "**************************************************************************************


    "**************************************************************************************
    READ TABLE it_dre02 INTO wa_dre02
                        WITH KEY bukrs = wa_dre04-bukrs
                                 gjahr = wa_dre04-gjahr
                                 poper = wa_dre04-poper
                                 saknr = wa_dre04-saknr
                                 ktopl = wa_dre04-ktopl
                                 kokrs = wa_dre04-kokrs
                                 kostl = wa_dre04-kostl
                                 kosar = wa_dre04-kosar
                                 prctr = wa_dre04-prctr
                                 matnr = wa_dre04-matnr
                                 matkl = wa_dre04-matkl
                                 aufnr = wa_dre04-aufnr
                                 vbund = wa_dre04-vbund
                                 BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      vg_tabix = sy-tabix.
      wa_dre02-vltsl = wa_dre02-vltsl + wa_dre04-vltsl.
      wa_dre02-vlhsl = wa_dre02-vlhsl + wa_dre04-vlhsl.
      wa_dre02-vlksl = wa_dre02-vlksl + wa_dre04-vlksl.
      wa_dre02-vlosl = wa_dre02-vlosl + wa_dre04-vlosl.
      wa_dre02-qtmsl = wa_dre02-qtmsl + wa_dre04-qtmsl.
      MODIFY it_dre02 INDEX vg_tabix FROM wa_dre02 TRANSPORTING vltsl vlhsl vlksl vlosl qtmsl.
    ELSE.
      MOVE-CORRESPONDING wa_dre04 TO wa_dre02.
      wa_dre02-vltsl = wa_dre04-vltsl.
      wa_dre02-vlhsl = wa_dre04-vlhsl.
      wa_dre02-vlksl = wa_dre04-vlksl.
      wa_dre02-vlosl = wa_dre04-vlosl.
      wa_dre02-qtmsl = wa_dre04-qtmsl.
      APPEND wa_dre02 TO it_dre02.
      SORT it_dre02 BY bukrs gjahr poper saknr ktopl kokrs kostl kosar prctr matnr matkl aufnr vbund.
    ENDIF.
    "**************************************************************************************
    "**************************************************************************************


    "**************************************************************************************
*    READ TABLE IT_DRE03 INTO WA_DRE03
*                        WITH KEY BUKRS = WA_DRE04-BUKRS
*                                 GJAHR = WA_DRE04-GJAHR
*                                 POPER = WA_DRE04-POPER
*                                 BUDAT = WA_DRE04-BUDAT
*                                 SAKNR = WA_DRE04-SAKNR
*                                 KTOPL = WA_DRE04-KTOPL
*                                 KOKRS = WA_DRE04-KOKRS
*                                 SHKZG = WA_DRE04-SHKZG
*                                 KOSTL = WA_DRE04-KOSTL
*                                 KOSAR = WA_DRE04-KOSAR
*                                 PRCTR = WA_DRE04-PRCTR
*                                 MATNR = WA_DRE04-MATNR
*                                 MATKL = WA_DRE04-MATKL
*                                 AUFNR = WA_DRE04-AUFNR
*                                 VBUND = WA_DRE04-VBUND
*                                 BINARY SEARCH.
*    IF SY-SUBRC IS INITIAL.
*      VG_TABIX = SY-TABIX.
*      WA_DRE03-VLTSL = WA_DRE03-VLTSL + WA_DRE04-VLTSL.
*      WA_DRE03-VLHSL = WA_DRE03-VLHSL + WA_DRE04-VLHSL.
*      WA_DRE03-VLKSL = WA_DRE03-VLKSL + WA_DRE04-VLKSL.
*      WA_DRE03-VLOSL = WA_DRE03-VLOSL + WA_DRE04-VLOSL.
*      WA_DRE03-QTMSL = WA_DRE03-QTMSL + WA_DRE04-QTMSL.
*      MODIFY IT_DRE03 INDEX VG_TABIX FROM WA_DRE03 TRANSPORTING VLTSL VLHSL VLKSL VLOSL QTMSL.
*    ELSE.
*      MOVE-CORRESPONDING WA_DRE04 TO WA_DRE03.
*      WA_DRE03-VLTSL = WA_DRE04-VLTSL.
*      WA_DRE03-VLHSL = WA_DRE04-VLHSL.
*      WA_DRE03-VLKSL = WA_DRE04-VLKSL.
*      WA_DRE03-VLOSL = WA_DRE04-VLOSL.
*      WA_DRE03-QTMSL = WA_DRE04-QTMSL.
*      APPEND WA_DRE03 TO IT_DRE03.
*      SORT IT_DRE03 BY BUKRS GJAHR POPER BUDAT SAKNR KTOPL KOKRS SHKZG KOSTL KOSAR PRCTR MATNR MATKL AUFNR VBUND.
*    ENDIF.
    "**************************************************************************************
    "**************************************************************************************
  ENDLOOP.

  lva_gjahr_min = lva_anomes_min(4).
  lva_poper_min = lva_anomes_min+4(3).

*  IF IT_DRE01 IS NOT INITIAL.
*    MODIFY ZGLT_DRE_01 FROM TABLE IT_DRE01.
*  ENDIF.

  IF it_dre02 IS NOT INITIAL.
    MODIFY zglt_dre_02 FROM TABLE it_dre02.
  ENDIF.

*  IF IT_DRE03 IS NOT INITIAL.
*    MODIFY ZGLT_DRE_03 FROM TABLE IT_DRE03.
*  ENDIF.

  COMMIT WORK.

  "Exclui todos os registros integrados neste dia
  IF p_bukrs IS INITIAL.
    EXEC SQL.
      UPDATE ZGLT_DRE_04
         SET CK_INTEGRADO = 'S'
       WHERE TIMESTAMP BETWEEN :LC_DATA_INI AND :LC_DATA_FIM
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
         AND COALESCE(CK_INTEGRADO,'N') = 'N'
*         AND NVL(CK_INTEGRADO,'N') = 'N'
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
    ENDEXEC.

    TRY.
        EXEC SQL.
          OPEN DOCUMENTOS_INCLUIR FOR
             SELECT * FROM (
             SELECT TT.MANDT,
                    TT.BUKRS,
                    TT.GJAHR,
                    TT.POPER,
                    SUM(VLTSL_1) - SUM(VLTSL_2) AS VLTSL,
                    SUM(VLHSL_1) - SUM(VLHSL_2) AS VLHSL,
                    SUM(VLKSL_1) - SUM(VLKSL_2) AS VLKSL,
                    SUM(VLOSL_1) - SUM(VLOSL_2) AS VLOSL,
                    SUM(QTMSL_1) - SUM(QTMSL_2) AS QTMSL
               FROM (
             SELECT D2.MANDT, D2.BUKRS, D2.GJAHR, D2.POPER,
                    SUM(D2.VLTSL) AS VLTSL_1,
                    SUM(D2.VLHSL) AS VLHSL_1,
                    SUM(D2.VLKSL) AS VLKSL_1,
                    SUM(D2.VLOSL) AS VLOSL_1,
                    SUM(D2.QTMSL) AS QTMSL_1,
                    0 AS VLTSL_2, 0 AS VLHSL_2, 0 AS VLKSL_2, 0 AS VLOSL_2, 0 AS QTMSL_2
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
               FROM SAPHANADB.ZGLT_DRE_02 D2
*               FROM SAPSR3.ZGLT_DRE_02 D2
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
              WHERE D2.MANDT = '300'
                AND D2.GJAHR >= :LVA_GJAHR_MIN
                AND D2.POPER >= :LVA_POPER_MIN
              GROUP BY D2.MANDT, D2.BUKRS, D2.GJAHR, D2.POPER
              UNION ALL
             SELECT D2.MANDT, D2.BUKRS, D2.GJAHR, D2.POPER, 0 AS VLTSL_1, 0 AS VLHSL_1, 0 AS VLKSL_1, 0 AS VLOSL_1, 0 AS QTMSL_1,
                    SUM(D2.VLTSL) AS VLTSL_2,
                    SUM(D2.VLHSL) AS VLHSL_2,
                    SUM(D2.VLKSL) AS VLKSL_2,
                    SUM(D2.VLOSL) AS VLOSL_2,
                    SUM(D2.QTMSL_BASE) AS QTMSL_2
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
               FROM SAPHANADB.ZGLT_DRE_04 D2
*               FROM SAPSR3.ZGLT_DRE_04 D2
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
              WHERE D2.MANDT = '300'
                AND D2.GJAHR >= :LVA_GJAHR_MIN
                AND D2.POPER >= :LVA_POPER_MIN
              GROUP BY D2.MANDT, D2.BUKRS, D2.GJAHR, D2.POPER ) TT
              GROUP BY TT.MANDT, TT.BUKRS, TT.GJAHR, TT.POPER ) TT
              WHERE ABS(VLTSL) + ABS(VLHSL) + ABS(VLKSL) + ABS(VLOSL) + ABS(QTMSL) <> 0
        ENDEXEC.
      CATCH cx_sy_native_sql_error INTO exc_ref.
        error_text = exc_ref->get_text( ).
        MESSAGE error_text TYPE 'E' RAISING erro_sql.
    ENDTRY.

  ELSE.
    EXEC SQL.
      UPDATE ZGLT_DRE_04
         SET CK_INTEGRADO = 'S'
       WHERE BUKRS = :P_BUKRS
         AND TIMESTAMP BETWEEN :LC_DATA_INI AND :LC_DATA_FIM
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
         AND COALESCE(CK_INTEGRADO,'N') = 'N'
*         AND NVL(CK_INTEGRADO,'N') = 'N'
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
    ENDEXEC.

    TRY.
        EXEC SQL.
          OPEN DOCUMENTOS_INCLUIR FOR
             SELECT * FROM (
             SELECT TT.MANDT,
                    TT.BUKRS,
                    TT.GJAHR,
                    TT.POPER,
                    SUM(VLTSL_1) - SUM(VLTSL_2) AS VLTSL,
                    SUM(VLHSL_1) - SUM(VLHSL_2) AS VLHSL,
                    SUM(VLKSL_1) - SUM(VLKSL_2) AS VLKSL,
                    SUM(VLOSL_1) - SUM(VLOSL_2) AS VLOSL,
                    SUM(QTMSL_1) - SUM(QTMSL_2) AS QTMSL
               FROM (
             SELECT D2.MANDT, D2.BUKRS, D2.GJAHR, D2.POPER,
                    SUM(D2.VLTSL) AS VLTSL_1,
                    SUM(D2.VLHSL) AS VLHSL_1,
                    SUM(D2.VLKSL) AS VLKSL_1,
                    SUM(D2.VLOSL) AS VLOSL_1,
                    SUM(D2.QTMSL) AS QTMSL_1,
                    0 AS VLTSL_2, 0 AS VLHSL_2, 0 AS VLKSL_2, 0 AS VLOSL_2, 0 AS QTMSL_2
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
               FROM SAPHANADB.ZGLT_DRE_02 D2
*               FROM SAPSR3.ZGLT_DRE_02 D2
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
              WHERE D2.MANDT = '300'
                AND D2.BUKRS = :P_BUKRS
                AND D2.GJAHR >= :LVA_GJAHR_MIN
                AND D2.POPER >= :LVA_POPER_MIN
              GROUP BY D2.MANDT, D2.BUKRS, D2.GJAHR, D2.POPER
              UNION ALL
             SELECT D2.MANDT, D2.BUKRS, D2.GJAHR, D2.POPER, 0 AS VLTSL_1, 0 AS VLHSL_1, 0 AS VLKSL_1, 0 AS VLOSL_1, 0 AS QTMSL_1,
                    SUM(D2.VLTSL) AS VLTSL_2,
                    SUM(D2.VLHSL) AS VLHSL_2,
                    SUM(D2.VLKSL) AS VLKSL_2,
                    SUM(D2.VLOSL) AS VLOSL_2,
                    SUM(D2.QTMSL_BASE) AS QTMSL_2
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
               FROM SAPHANADB.ZGLT_DRE_04 D2
*               FROM SAPSR3.ZGLT_DRE_04 D2
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
              WHERE D2.MANDT = '300'
                AND D2.BUKRS = :P_BUKRS
                AND D2.GJAHR >= :LVA_GJAHR_MIN
                AND D2.POPER >= :LVA_POPER_MIN
              GROUP BY D2.MANDT, D2.BUKRS, D2.GJAHR, D2.POPER ) TT
              GROUP BY TT.MANDT, TT.BUKRS, TT.GJAHR, TT.POPER ) TT
              WHERE ABS(VLTSL) + ABS(VLHSL) + ABS(VLKSL) + ABS(VLOSL) + ABS(QTMSL) <> 0
        ENDEXEC.
      CATCH cx_sy_native_sql_error INTO exc_ref.
        error_text = exc_ref->get_text( ).
        MESSAGE error_text TYPE 'E' RAISING erro_sql.
    ENDTRY.

  ENDIF.

  CLEAR: it_dre02[], it_dre02.

  DO.
    EXEC SQL.
      FETCH NEXT DOCUMENTOS_INCLUIR INTO
      :WA_DRE02-MANDT,
      :WA_DRE02-BUKRS,
      :WA_DRE02-GJAHR,
      :WA_DRE02-POPER
    ENDEXEC.
    IF sy-subrc <> 0.
      EXIT.
    ELSE.
      APPEND wa_dre02 TO it_dre02.
    ENDIF.
  ENDDO.

  EXEC SQL.
    CLOSE DOCUMENTOS_INCLUIR
  ENDEXEC.

  LOOP AT it_dre02 INTO wa_dre02.
    CALL FUNCTION 'Z_01_DRE_AJUSTA_TABELAS'
      EXPORTING
        i_bukrs  = wa_dre02-bukrs
        i_gjahr  = wa_dre02-gjahr
        i_poper  = wa_dre02-poper
      EXCEPTIONS
        erro_sql = 1
        OTHERS   = 2.
  ENDLOOP.


ENDFUNCTION.
