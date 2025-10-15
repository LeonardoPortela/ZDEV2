FUNCTION z_01_dre_saldos_retira .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LC_DATA_INI) TYPE  TIMESTAMP
*"     REFERENCE(LC_DATA_FIM) TYPE  TIMESTAMP
*"     REFERENCE(P_BUKRS) TYPE  BUKRS OPTIONAL
*"  EXCEPTIONS
*"      ERRO_SQL
*"----------------------------------------------------------------------
  DATA: exc_ref    TYPE REF TO cx_sy_native_sql_error,
        error_text TYPE string,
        it_dre04   TYPE TABLE OF zglt_dre_04 WITH HEADER LINE.

  DATA: rg_bukrs TYPE RANGE OF bukrs,
        wa_bukrs LIKE LINE OF rg_bukrs.

  CLEAR: it_dre04[].

  IF p_bukrs IS NOT INITIAL.
    wa_bukrs-sign   = 'I'.
    wa_bukrs-option = 'EQ'.
    wa_bukrs-low    = p_bukrs.
    wa_bukrs-high   = p_bukrs.
    APPEND wa_bukrs TO rg_bukrs.
  ENDIF.

  SELECT * INTO TABLE it_dre04
    FROM zglt_dre_04
   WHERE bukrs IN rg_bukrs
     AND ck_integrado EQ 'N'.

  CHECK it_dre04[] IS NOT INITIAL.

  DATA: "IT_DRE01 TYPE TABLE OF ZGLT_DRE_01 WITH HEADER LINE,
    it_dre02 TYPE TABLE OF zglt_dre_02 WITH HEADER LINE,
    "IT_DRE03 TYPE TABLE OF ZGLT_DRE_03 WITH HEADER LINE,
    "WA_DRE01 TYPE ZGLT_DRE_01,
    wa_dre02 TYPE zglt_dre_02,
    "WA_DRE03 TYPE ZGLT_DRE_03,
    wa_dre04 TYPE zglt_dre_04.

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

  LOOP AT it_dre04 INTO wa_dre04.

    CLEAR: "WA_DRE01,
           wa_dre02.
    "WA_DRE03.

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
*      WA_DRE01-VLTSL = WA_DRE01-VLTSL - WA_DRE04-VLTSL.
*      WA_DRE01-VLHSL = WA_DRE01-VLHSL - WA_DRE04-VLHSL.
*      WA_DRE01-VLKSL = WA_DRE01-VLKSL - WA_DRE04-VLKSL.
*      WA_DRE01-VLOSL = WA_DRE01-VLOSL - WA_DRE04-VLOSL.
*      WA_DRE01-QTMSL = WA_DRE01-QTMSL - WA_DRE04-QTMSL.
*      MODIFY IT_DRE01 INDEX SY-TABIX FROM WA_DRE01 TRANSPORTING VLTSL VLHSL VLKSL VLOSL QTMSL.
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
      wa_dre02-vltsl = wa_dre02-vltsl - wa_dre04-vltsl.
      wa_dre02-vlhsl = wa_dre02-vlhsl - wa_dre04-vlhsl.
      wa_dre02-vlksl = wa_dre02-vlksl - wa_dre04-vlksl.
      wa_dre02-vlosl = wa_dre02-vlosl - wa_dre04-vlosl.
      wa_dre02-qtmsl = wa_dre02-qtmsl - wa_dre04-qtmsl.
      MODIFY it_dre02 INDEX sy-tabix FROM wa_dre02 TRANSPORTING vltsl vlhsl vlksl vlosl qtmsl.
    ENDIF.
    "**************************************************************************************
    "**************************************************************************************


    "**************************************************************************************
*    READ TABLE IT_DRE03 INTO WA_DRE03
*                        WITH KEY BUKRS = IT_DRE04-BUKRS
*                                 GJAHR = IT_DRE04-GJAHR
*                                 POPER = IT_DRE04-POPER
*                                 BUDAT = IT_DRE04-BUDAT
*                                 SAKNR = IT_DRE04-SAKNR
*                                 KTOPL = IT_DRE04-KTOPL
*                                 KOKRS = IT_DRE04-KOKRS
*                                 SHKZG = IT_DRE04-SHKZG
*                                 KOSTL = IT_DRE04-KOSTL
*                                 KOSAR = IT_DRE04-KOSAR
*                                 PRCTR = IT_DRE04-PRCTR
*                                 MATNR = IT_DRE04-MATNR
*                                 MATKL = IT_DRE04-MATKL
*                                 AUFNR = IT_DRE04-AUFNR
*                                 VBUND = IT_DRE04-VBUND
*                                 BINARY SEARCH.
*    IF SY-SUBRC IS INITIAL.
*      WA_DRE03-VLTSL = WA_DRE03-VLTSL - WA_DRE04-VLTSL.
*      WA_DRE03-VLHSL = WA_DRE03-VLHSL - WA_DRE04-VLHSL.
*      WA_DRE03-VLKSL = WA_DRE03-VLKSL - WA_DRE04-VLKSL.
*      WA_DRE03-VLOSL = WA_DRE03-VLOSL - WA_DRE04-VLOSL.
*      WA_DRE03-QTMSL = WA_DRE03-QTMSL - WA_DRE04-QTMSL.
*      MODIFY IT_DRE03 INDEX SY-TABIX FROM WA_DRE03 TRANSPORTING VLTSL VLHSL VLKSL VLOSL QTMSL.
*    ENDIF.
    "**************************************************************************************
    "**************************************************************************************
  ENDLOOP.

*  IF IT_DRE01[] IS NOT INITIAL.
*    MODIFY ZGLT_DRE_01 FROM TABLE IT_DRE01.
*  ENDIF.

  IF it_dre02[] IS NOT INITIAL.
    MODIFY zglt_dre_02 FROM TABLE it_dre02.
  ENDIF.

*  IF IT_DRE03[] IS NOT INITIAL.
*    MODIFY ZGLT_DRE_03 FROM TABLE IT_DRE03.
*  ENDIF.

  COMMIT WORK.

  "Exclui todos os registros integrados neste dia
  IF p_bukrs IS INITIAL.
    EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
      DELETE FROM ZGLT_DRE_04
       WHERE TIMESTAMP    BETWEEN :LC_DATA_INI AND :LC_DATA_FIM
         AND COALESCE(CK_INTEGRADO,'N') = 'N'

*      DELETE ZGLT_DRE_04
*       WHERE TIMESTAMP    BETWEEN :LC_DATA_INI AND :LC_DATA_FIM
*         AND NVL(CK_INTEGRADO,'N') = 'N'
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
    ENDEXEC.
  ELSE.
    EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
      DELETE FROM ZGLT_DRE_04
       WHERE BUKRS = :P_BUKRS
         AND TIMESTAMP BETWEEN :LC_DATA_INI AND :LC_DATA_FIM
         AND COALESCE(CK_INTEGRADO,'N') = 'N'

*      DELETE ZGLT_DRE_04
*       WHERE BUKRS = :P_BUKRS
*         AND TIMESTAMP BETWEEN :LC_DATA_INI AND :LC_DATA_FIM
*         AND NVL(CK_INTEGRADO,'N') = 'N'
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
    ENDEXEC.
  ENDIF.

ENDFUNCTION.
