FUNCTION z_01_dre_carga.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------
  DATA: lc_aux     TYPE c LENGTH 16,
        exc_ref    TYPE REF TO cx_sy_native_sql_error,
        error_text TYPE string.

  CALL FUNCTION 'Z_01_DRE_LIMPAR'.

  DATA: pdata_ini TYPE datum,
        p_total   TYPE i,
        p_posicao TYPE i,
        p_posica2 TYPE i,
        texto     TYPE string,
        textop    TYPE string,
        p_parcial TYPE char01.

  pdata_ini = '20150101'.

  p_total   = 0.
  p_posicao = 0.
  WHILE pdata_ini LE sy-datum.
    p_total   = p_total + 1.
    pdata_ini = pdata_ini + 1.
  ENDWHILE.

  pdata_ini = '20150101'.
  p_parcial = abap_false.

  CALL FUNCTION 'Z_01_DRE_STATUS'
    EXPORTING
      texto     = 'Processo de Carga (DRE)'
      p_total   = p_total
      p_posicao = p_posicao.

  WHILE pdata_ini LE sy-datum.

    CONCATENATE pdata_ini+6(2) '/' pdata_ini+4(2) '/' pdata_ini(4) INTO texto.
    CONCATENATE 'Processo de Carga (DRE)' texto INTO texto SEPARATED BY space.
    IF p_parcial EQ abap_false.
      p_posicao = p_posicao + 1.
      p_posica2 = 1.
    ELSE.
      p_posica2 = p_posica2 + 1.
      MOVE p_posica2 TO textop.
      CONCATENATE texto 'Etapa:' textop INTO texto SEPARATED BY space.
    ENDIF.

    CALL FUNCTION 'Z_01_DRE_STATUS'
      EXPORTING
        texto     = texto
        p_total   = p_total
        p_posicao = p_posicao.

    CALL FUNCTION 'Z_01_DRE_PROC_DIARIO'
      EXPORTING
        pdata    = pdata_ini
        pcarga   = 'X'
      IMPORTING
        parcial  = p_parcial
      EXCEPTIONS
        erro_sql = 1
        OTHERS   = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF p_parcial EQ abap_false.
      pdata_ini = pdata_ini + 1.
    ENDIF.

  ENDWHILE.

  "Inclui Saldo DRE01
  TRY.
      EXEC SQL.
* ---> S4 Migration - 22/07/2023 - MG-5609 - CA
        INSERT INTO SAPHANADB.ZGLT_DRE_01 RE1
*        INSERT INTO SAPSR3.ZGLT_DRE_01 RE1
* <--- S4 Migration - 22/07/2023 - MG-5609 - CA
             ( MANDT, BUKRS, GJAHR, SAKNR, KTOPL, KOKRS,
               KOSTL, KOSAR, PRCTR, MATNR, MATKL, AUFNR, VBUND,
               VLTSL, VLHSL, VLKSL, VLOSL, QTMSL )
        SELECT RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.SAKNR, RE4.KTOPL, RE4.KOKRS,
               RE4.KOSTL, RE4.KOSAR, RE4.PRCTR, RE4.MATNR, RE4.MATKL, RE4.AUFNR, RE4.VBUND,
* ---> S4 Migration - 22/07/2023 - MG-5609 - CA
               COALESCE(SUM(RE4.VLTSL),0) AS VLTSL, COALESCE(SUM(RE4.VLHSL),0) AS VLHSL, COALESCE(SUM(RE4.VLKSL),0) AS VLKSL,
               COALESCE(SUM(RE4.VLOSL),0) AS VLOSL, COALESCE(SUM(RE4.QTMSL_BASE),0) AS QTMSL
          FROM SAPHANADB.ZGLT_DRE_04 RE4
*               NVL(SUM(RE4.VLTSL),0) AS VLTSL, NVL(SUM(RE4.VLHSL),0) AS VLHSL, NVL(SUM(RE4.VLKSL),0) AS VLKSL,
*               NVL(SUM(RE4.VLOSL),0) AS VLOSL, NVL(SUM(RE4.QTMSL_BASE),0) AS QTMSL
*          FROM SAPSR3.ZGLT_DRE_04 RE4
* <--- S4 Migration - 22/07/2023 - MG-5609 - CA
          GROUP BY RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.SAKNR,
                   RE4.KTOPL, RE4.KOKRS, RE4.KOSTL, RE4.KOSAR, RE4.PRCTR,
                   RE4.MATNR, RE4.MATKL, RE4.AUFNR, RE4.VBUND
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      CONCATENATE 'Inclui Saldo DRE01 (Insert)' error_text INTO error_text SEPARATED BY space.
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  "Inclui Saldo DRE02
  TRY.
      EXEC SQL.
* ---> S4 Migration - 22/07/2023 - MG-5609 - CA
        INSERT INTO SAPHANADB.ZGLT_DRE_02 RE2
*        INSERT INTO SAPSR3.ZGLT_DRE_02 RE2
* <--- S4 Migration - 22/07/2023 - MG-5609 - CA
             ( MANDT, BUKRS, GJAHR, POPER, SAKNR, KTOPL, KOKRS,
               KOSTL, KOSAR, PRCTR, MATNR, MATKL, AUFNR, VBUND,
               VLTSL, VLHSL, VLKSL, VLOSL, QTMSL )
        SELECT RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.POPER, RE4.SAKNR, RE4.KTOPL, RE4.KOKRS,
               RE4.KOSTL, RE4.KOSAR, RE4.PRCTR, RE4.MATNR, RE4.MATKL, RE4.AUFNR, RE4.VBUND,
* ---> S4 Migration - 22/07/2023 - MG-5609 - CA
               COALESCE(SUM(RE4.VLTSL),0) AS VLTSL, COALESCE(SUM(RE4.VLHSL),0) AS VLHSL, COALESCE(SUM(RE4.VLKSL),0) AS VLKSL,
               COALESCE(SUM(RE4.VLOSL),0) AS VLOSL, COALESCE(SUM(RE4.QTMSL_BASE),0) AS QTMSL
          FROM SAPHANADB.ZGLT_DRE_04 RE4
*               NVL(SUM(RE4.VLTSL),0) AS VLTSL, NVL(SUM(RE4.VLHSL),0) AS VLHSL, NVL(SUM(RE4.VLKSL),0) AS VLKSL,
*               NVL(SUM(RE4.VLOSL),0) AS VLOSL, NVL(SUM(RE4.QTMSL_BASE),0) AS QTMSL
*          FROM SAPSR3.ZGLT_DRE_04 RE4
* <--- S4 Migration - 22/07/2023 - MG-5609 - CA
          GROUP BY RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.POPER, RE4.SAKNR,
                   RE4.KTOPL, RE4.KOKRS, RE4.KOSTL, RE4.KOSAR, RE4.PRCTR,
                   RE4.MATNR, RE4.MATKL, RE4.AUFNR, RE4.VBUND
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      CONCATENATE 'Inclui Saldo DRE02 (Insert)' error_text INTO error_text SEPARATED BY space.
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  "Inclui Saldo DRE03
  TRY.
      EXEC SQL.
* ---> S4 Migration - 22/07/2023 - MG-5609 - CA
        INSERT INTO SAPHANADB.ZGLT_DRE_03 RE3
*        INSERT INTO SAPSR3.ZGLT_DRE_03 RE3
* <--- S4 Migration - 22/07/2023 - MG-5609 - CA
             ( MANDT, BUKRS, GJAHR, POPER, BUDAT, SAKNR, KTOPL, KOKRS,
               SHKZG, KOSTL, KOSAR, PRCTR, MATNR, MATKL, AUFNR, VBUND,
               VLTSL, VLHSL, VLKSL, VLOSL, QTMSL )
        SELECT RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.POPER, RE4.BUDAT, RE4.SAKNR, RE4.KTOPL, RE4.KOKRS,
               RE4.SHKZG, RE4.KOSTL, RE4.KOSAR, RE4.PRCTR, RE4.MATNR, RE4.MATKL, RE4.AUFNR, RE4.VBUND,
* ---> S4 Migration - 22/07/2023 - MG-5609 - CA
               COALESCE(SUM(RE4.VLTSL),0) AS VLTSL, COALESCE(SUM(RE4.VLHSL),0) AS VLHSL, COALESCE(SUM(RE4.VLKSL),0) AS VLKSL,
               COALESCE(SUM(RE4.VLOSL),0) AS VLOSL, COALESCE(SUM(RE4.QTMSL_BASE),0) AS QTMSL
          FROM SAPHANADB.ZGLT_DRE_04 RE4
*               NVL(SUM(RE4.VLTSL),0) AS VLTSL, NVL(SUM(RE4.VLHSL),0) AS VLHSL, NVL(SUM(RE4.VLKSL),0) AS VLKSL,
*               NVL(SUM(RE4.VLOSL),0) AS VLOSL, NVL(SUM(RE4.QTMSL_BASE),0) AS QTMSL
*          FROM SAPSR3.ZGLT_DRE_04 RE4
* <--- S4 Migration - 22/07/2023 - MG-5609 - CA
          GROUP BY RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.POPER, RE4.BUDAT,
                   RE4.SAKNR, RE4.KTOPL, RE4.KOKRS, RE4.SHKZG, RE4.KOSTL,
                   RE4.KOSAR, RE4.PRCTR, RE4.MATNR, RE4.MATKL, RE4.AUFNR,
                   RE4.VBUND
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      CONCATENATE 'Inclui Saldo DRE03 (Insert)' error_text INTO error_text SEPARATED BY space.
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  "Exclui todos os registros integrados neste dia
  EXEC SQL.
    UPDATE ZGLT_DRE_04
       SET CK_INTEGRADO = 'S'
* ---> S4 Migration - 22/07/2023 - MG-5609 - CA
     WHERE COALESCE(CK_INTEGRADO,'N') = 'N'
*     WHERE NVL(CK_INTEGRADO,'N') = 'N'
* <--- S4 Migration - 22/07/2023 - MG-5609 - CA
  ENDEXEC.

ENDFUNCTION.
