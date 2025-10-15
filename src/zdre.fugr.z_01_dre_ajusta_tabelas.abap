FUNCTION z_01_dre_ajusta_tabelas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_GJAHR) TYPE  GJAHR
*"     REFERENCE(I_POPER) TYPE  POPER
*"     REFERENCE(I_EXCLUIR_04) TYPE  CHAR01 DEFAULT ' '
*"     REFERENCE(I_REFAZER) TYPE  CHAR01 DEFAULT ' '
*"  EXCEPTIONS
*"      ERRO_SQL
*"----------------------------------------------------------------------

  DATA: lc_aux     TYPE c LENGTH 16,
        exc_ref    TYPE REF TO cx_sy_native_sql_error,
        error_text TYPE string.

  DATA: lc_dt_inicio   TYPE sy-datum,
        lc_dt_final    TYPE sy-datum,
        lc_dt_atual    TYPE sy-datum,
        lc_parcial     TYPE char01,
        lc_quantidade  TYPE i,
        lc_quant_atual TYPE i.

  "Exclui todos os registros integrados Empresa/Ano
  "EXEC SQL.
  "  DELETE SAPSR3.ZGLT_DRE_01 WHERE BUKRS = :I_BUKRS AND GJAHR = :I_GJAHR
  "ENDEXEC.
  "COMMIT WORK.

  "Exclui todos os registros integrados Empresa/Ano/Mês
  EXEC SQL.
    DELETE FROM SAPHANADB.ZGLT_DRE_02 WHERE BUKRS = :I_BUKRS AND GJAHR = :I_GJAHR AND POPER = :I_POPER
  ENDEXEC.
*  COMMIT WORK.

  "Exclui todos os registros integrados Empresa/Ano/Mês
  EXEC SQL.
    DELETE FROM SAPHANADB.ZGLT_DRE_03 WHERE BUKRS = :I_BUKRS AND GJAHR = :I_GJAHR AND POPER = :I_POPER
  ENDEXEC.
*  COMMIT WORK.

  "Exclui todos os registros integrados Empresa/Ano/Mês
  IF i_excluir_04 EQ abap_true.
    EXEC SQL.
      DELETE FROM SAPHANADB.ZGLT_DRE_04 WHERE BUKRS = :I_BUKRS AND GJAHR = :I_GJAHR AND POPER = :I_POPER
    ENDEXEC.
  ENDIF.
*  COMMIT WORK.

  IF i_excluir_04 EQ abap_false AND i_refazer EQ abap_false.
    "Inclui Saldo DRE01
*    TRY.
*        EXEC SQL.
*          INSERT INTO SAPSR3.ZGLT_DRE_01 RE1
*               ( MANDT, BUKRS, GJAHR, SAKNR, KTOPL, KOKRS,
*                 KOSTL, KOSAR, PRCTR, MATNR, MATKL, AUFNR, VBUND,
*                 VLTSL, VLHSL, VLKSL, VLOSL, QTMSL )
*          SELECT RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.SAKNR, RE4.KTOPL, RE4.KOKRS,
*                 RE4.KOSTL, RE4.KOSAR, RE4.PRCTR, RE4.MATNR, RE4.MATKL, RE4.AUFNR, RE4.VBUND,
*                 NVL(SUM(RE4.VLTSL),0) AS VLTSL, NVL(SUM(RE4.VLHSL),0) AS VLHSL, NVL(SUM(RE4.VLKSL),0) AS VLKSL,
*                 NVL(SUM(RE4.VLOSL),0) AS VLOSL, NVL(SUM(RE4.QTMSL_BASE),0) AS QTMSL
*            FROM SAPSR3.ZGLT_DRE_04 RE4
*           WHERE RE4.BUKRS = :I_BUKRS
*             AND RE4.GJAHR = :I_GJAHR
*            GROUP BY RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.SAKNR,
*                     RE4.KTOPL, RE4.KOKRS, RE4.KOSTL, RE4.KOSAR, RE4.PRCTR,
*                     RE4.MATNR, RE4.MATKL, RE4.AUFNR, RE4.VBUND
*        ENDEXEC.
*      CATCH CX_SY_NATIVE_SQL_ERROR INTO EXC_REF.
*        ERROR_TEXT = EXC_REF->GET_TEXT( ).
*        CONCATENATE 'Inclui Saldo DRE01 (Insert)' ERROR_TEXT INTO ERROR_TEXT SEPARATED BY SPACE.
*        MESSAGE ERROR_TEXT TYPE 'E' RAISING ERRO_SQL.
*    ENDTRY.

    "Inclui Saldo DRE02
    TRY.
        EXEC SQL.
* ---> S4 Migration - 21/07/2023 - MG-5609 - CA
*          INSERT INTO SAPSR3.ZGLT_DRE_02 RE2
*               ( MANDT, BUKRS, GJAHR, POPER, SAKNR, KTOPL, KOKRS,
*                 KOSTL, KOSAR, PRCTR, MATNR, MATKL, AUFNR, VBUND,
*                 VLTSL, VLHSL, VLKSL, VLOSL, QTMSL )
*          SELECT RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.POPER, RE4.SAKNR, RE4.KTOPL, RE4.KOKRS,
*                 RE4.KOSTL, RE4.KOSAR, RE4.PRCTR, RE4.MATNR, RE4.MATKL, RE4.AUFNR, RE4.VBUND,
*                 NVL(SUM(RE4.VLTSL),0) AS VLTSL, NVL(SUM(RE4.VLHSL),0) AS VLHSL, NVL(SUM(RE4.VLKSL),0) AS VLKSL,
*                 NVL(SUM(RE4.VLOSL),0) AS VLOSL, NVL(SUM(RE4.QTMSL_BASE),0) AS QTMSL
*            FROM SAPSR3.ZGLT_DRE_04 RE4
*           WHERE RE4.BUKRS = :I_BUKRS
*             AND RE4.GJAHR = :I_GJAHR
*             AND RE4.POPER = :I_POPER
*            GROUP BY RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.POPER, RE4.SAKNR,
*                     RE4.KTOPL, RE4.KOKRS, RE4.KOSTL, RE4.KOSAR, RE4.PRCTR,
*                     RE4.MATNR, RE4.MATKL, RE4.AUFNR, RE4.VBUND

          INSERT INTO SAPHANADB.ZGLT_DRE_02 RE2
               ( MANDT, BUKRS, GJAHR, POPER, SAKNR, KTOPL, KOKRS,
                 KOSTL, KOSAR, PRCTR, MATNR, MATKL, AUFNR, VBUND,
                 VLTSL, VLHSL, VLKSL, VLOSL, QTMSL )
          SELECT RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.POPER, RE4.SAKNR, RE4.KTOPL, RE4.KOKRS,
                 RE4.KOSTL, RE4.KOSAR, RE4.PRCTR, RE4.MATNR, RE4.MATKL, RE4.AUFNR, RE4.VBUND,
                 COALESCE(SUM(RE4.VLTSL),0) AS VLTSL, COALESCE(SUM(RE4.VLHSL),0) AS VLHSL, COALESCE(SUM(RE4.VLKSL),0) AS VLKSL,
                 COALESCE(SUM(RE4.VLOSL),0) AS VLOSL, COALESCE(SUM(RE4.QTMSL_BASE),0) AS QTMSL
            FROM SAPHANADB.ZGLT_DRE_04 RE4
           WHERE RE4.BUKRS = :I_BUKRS
             AND RE4.GJAHR = :I_GJAHR
             AND RE4.POPER = :I_POPER
            GROUP BY RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.POPER, RE4.SAKNR,
                     RE4.KTOPL, RE4.KOKRS, RE4.KOSTL, RE4.KOSAR, RE4.PRCTR,
                     RE4.MATNR, RE4.MATKL, RE4.AUFNR, RE4.VBUND
* <--- S4 Migration - 21/07/2023 - MG-5609 - CA
        ENDEXEC.
*        COMMIT WORK.
      CATCH cx_sy_native_sql_error INTO exc_ref.
        error_text = exc_ref->get_text( ).
        CONCATENATE 'Inclui Saldo DRE02 (Insert)' error_text INTO error_text SEPARATED BY space.
        MESSAGE error_text TYPE 'E' RAISING erro_sql.
    ENDTRY.

*    "Inclui Saldo DRE03
*    TRY.
*        EXEC SQL.
*          INSERT INTO SAPSR3.ZGLT_DRE_03 RE3
*               ( MANDT, BUKRS, GJAHR, POPER, BUDAT, SAKNR, KTOPL, KOKRS,
*                 SHKZG, KOSTL, KOSAR, PRCTR, MATNR, MATKL, AUFNR, VBUND,
*                 VLTSL, VLHSL, VLKSL, VLOSL, QTMSL )
*          SELECT RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.POPER, RE4.BUDAT, RE4.SAKNR, RE4.KTOPL, RE4.KOKRS,
*                 RE4.SHKZG, RE4.KOSTL, RE4.KOSAR, RE4.PRCTR, RE4.MATNR, RE4.MATKL, RE4.AUFNR, RE4.VBUND,
*                 NVL(SUM(RE4.VLTSL),0) AS VLTSL, NVL(SUM(RE4.VLHSL),0) AS VLHSL, NVL(SUM(RE4.VLKSL),0) AS VLKSL,
*                 NVL(SUM(RE4.VLOSL),0) AS VLOSL, NVL(SUM(RE4.QTMSL_BASE),0) AS QTMSL
*            FROM SAPSR3.ZGLT_DRE_04 RE4
*           WHERE RE4.BUKRS = :I_BUKRS
*             AND RE4.GJAHR = :I_GJAHR
*             AND RE4.POPER = :I_POPER
*            GROUP BY RE4.MANDT, RE4.BUKRS, RE4.GJAHR, RE4.POPER, RE4.BUDAT,
*                     RE4.SAKNR, RE4.KTOPL, RE4.KOKRS, RE4.SHKZG, RE4.KOSTL,
*                     RE4.KOSAR, RE4.PRCTR, RE4.MATNR, RE4.MATKL, RE4.AUFNR,
*                     RE4.VBUND
*        ENDEXEC.
*      CATCH CX_SY_NATIVE_SQL_ERROR INTO EXC_REF.
*        ERROR_TEXT = EXC_REF->GET_TEXT( ).
*        CONCATENATE 'Inclui Saldo DRE03 (Insert)' ERROR_TEXT INTO ERROR_TEXT SEPARATED BY SPACE.
*        MESSAGE ERROR_TEXT TYPE 'E' RAISING ERRO_SQL.
*    ENDTRY.

  ELSEIF i_refazer EQ abap_true.

    TRY.
        EXEC SQL.
          OPEN DATAS FOR
             SELECT SUBSTR(MIN(FI.TIMESTAMP),1,8) AS DT_INICIO,
                    SUBSTR(MAX(FI.TIMESTAMP),1,8) AS DT_FINAL
               FROM SAPHANADB.FAGLFLEXA FI
*               FROM SAPSR3.FAGLFLEXA FI
              WHERE FI.RCLNT  = :SY-MANDT
                AND FI.RLDNR  = '0L'
                AND FI.RBUKRS = :I_BUKRS
                AND FI.RYEAR  = :I_GJAHR
                AND FI.POPER  = :I_POPER
        ENDEXEC.
*        COMMIT WORK.
      CATCH cx_sy_native_sql_error INTO exc_ref.
        error_text = exc_ref->get_text( ).
        MESSAGE error_text TYPE 'E' RAISING erro_sql.
    ENDTRY.

    DO.
      EXEC SQL.
        FETCH NEXT DATAS INTO
        :LC_DT_INICIO,
        :LC_DT_FINAL
      ENDEXEC.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE DATAS
    ENDEXEC.
    COMMIT WORK.

    IF lc_dt_inicio IS NOT INITIAL AND lc_dt_final IS NOT INITIAL.

      lc_dt_atual = lc_dt_inicio.

      lc_quantidade = 0.
      WHILE lc_dt_atual LE lc_dt_final.
        ADD 1 TO lc_quantidade.
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = lc_dt_atual
            days      = 1
            months    = 0
            years     = 0
          IMPORTING
            calc_date = lc_dt_atual.
      ENDWHILE.

      PERFORM mostra_texto_p USING 'Refazendo Exercício' lc_quantidade 0.

      lc_dt_atual = lc_dt_inicio.
      lc_quant_atual = 0.

      WHILE lc_dt_atual LE lc_dt_final.

        lc_parcial = abap_true.

        ADD 1 TO lc_quant_atual.

        WHILE lc_parcial EQ abap_true.
          CALL FUNCTION 'Z_01_DRE_PROC_DIARIO'
            EXPORTING
              pdata    = lc_dt_atual
            IMPORTING
              parcial  = lc_parcial
            EXCEPTIONS
              erro_sql = 1
              OTHERS   = 2.

          IF sy-subrc IS NOT INITIAL.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_sql.
          ENDIF.

          PERFORM mostra_texto_p USING 'Refazendo Exercício' lc_quantidade lc_quant_atual.

        ENDWHILE.

        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = lc_dt_atual
            days      = 1
            months    = 0
            years     = 0
          IMPORTING
            calc_date = lc_dt_atual.

        PERFORM mostra_texto_p USING 'Refazendo Exercício' lc_quantidade lc_quant_atual.

      ENDWHILE.

      CALL FUNCTION 'Z_01_DRE_AJUSTA_TABELAS'
        EXPORTING
          i_bukrs  = i_bukrs
          i_gjahr  = i_gjahr
          i_poper  = i_poper
        EXCEPTIONS
          erro_sql = 1
          OTHERS   = 2.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_sql.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFUNCTION.
