FUNCTION z_01_dre_totaliza_mes.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(PBUKRS) TYPE  BUKRS
*"     REFERENCE(PGJAHR) TYPE  GJAHR
*"     REFERENCE(PPOPER) TYPE  POPER
*"     REFERENCE(PVERSN) TYPE  VERSN_011
*"  EXCEPTIONS
*"      ERRO_SQL
*"----------------------------------------------------------------------

  DATA: lc_aux     TYPE c LENGTH 16,
        exc_ref    TYPE REF TO cx_sy_native_sql_error,
        error_text TYPE string,
        pmonat     TYPE zgl025_dre_acm-monat,
        pmonata    TYPE zgl025_dre_acm-monat.

  MOVE ppoper TO pmonat.
  pmonata = pmonat - 1.

  "Exclui todos os registros integrados neste dia
  EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
    DELETE FROM SAPHANADB.ZGL025_DRE_ACM
*    DELETE SAPSR3.ZGL025_DRE_ACM
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
     WHERE BUKRS = :PBUKRS
       AND VERSN = :PVERSN
       AND MONAT = :PMONAT
       AND GJAHR = :PGJAHR
  ENDEXEC.

  "Exclui todos os registros integrados neste dia
  EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
    DELETE FROM SAPHANADB.ZGL026_DRE_ACM
*    DELETE SAPSR3.ZGL026_DRE_ACM
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
     WHERE BUKRS = :PBUKRS
       AND VERSN = :PVERSN
       AND MONAT = :PMONAT
       AND GJAHR = :PGJAHR
  ENDEXEC.

  "Exclui todos os registros integrados neste dia
  EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
    DELETE FROM SAPHANADB.ZGL027_DRE_ACM
*    DELETE SAPSR3.ZGL027_DRE_ACM
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
     WHERE BUKRS = :PBUKRS
       AND VERSN = :PVERSN
       AND MONAT = :PMONAT
       AND GJAHR = :PGJAHR
  ENDEXEC.

  "Exclui todos os registros integrados neste dia
  EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
    DELETE FROM SAPHANADB.ZGL028_DRE_ACM
*    DELETE SAPSR3.ZGL028_DRE_ACM
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
     WHERE BUKRS = :PBUKRS
       AND VERSN = :PVERSN
       AND MONAT = :PMONAT
       AND GJAHR = :PGJAHR
  ENDEXEC.

  EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
    DELETE FROM SAPHANADB.ZGL030_DRE_ACM
*    DELETE SAPSR3.ZGL030_DRE_ACM
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
     WHERE BUKRS = :PBUKRS
       AND VERSN = :PVERSN
       AND MONAT = :PMONAT
       AND GJAHR = :PGJAHR
  ENDEXEC.

  "Acumulador de Conta Razão
  TRY.
      EXEC SQL.
        INSERT INTO ZGL025_DRE_ACM ( MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO )
        SELECT * FROM (
        SELECT MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR,
               SUM(VLR_REA)        AS VLR_REA,
               SUM(VLR_DOLAR)      AS VLR_DOLAR,
               SUM(VLR_DOLAR_CONV) AS VLR_DOLAR_CONV,
               SUM(VLR_GRUPO)      AS VLR_GRUPO
          FROM (SELECT MANDT, BUKRS, VERSN, :PMONAT AS MONAT, GJAHR, NIVEL, SAKNR, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
                  FROM SAPHANADB.ZGL025_DRE_ACM A
*                  FROM SAPSR3.ZGL025_DRE_ACM A
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
                 WHERE A.BUKRS = :PBUKRS
                   AND A.VERSN = :PVERSN
                   AND A.MONAT = :PMONATA
                   AND A.GJAHR = :PGJAHR
                 UNION ALL
                SELECT MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
                  FROM SAPHANADB.ZGL021_DRE_DADOS A
*                  FROM SAPSR3.ZGL021_DRE_DADOS A
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
                 WHERE A.BUKRS = :PBUKRS
                   AND A.VERSN = :PVERSN
                   AND A.MONAT = :PMONAT
                   AND A.GJAHR = :PGJAHR ) TT
         GROUP BY MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR ) TT
         WHERE NOT ( TT.VLR_REA	= 0 AND TT.VLR_DOLAR = 0 AND TT.VLR_DOLAR_CONV = 0 AND TT.VLR_GRUPO	= 0 )
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      CONCATENATE 'Inclui Saldo ZGL025_DRE_ACM (Insert)' error_text INTO error_text SEPARATED BY space.
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  "Acumulador de Centro de Custo
  TRY.
      EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
        INSERT INTO SAPHANADB.ZGL026_DRE_ACM ( MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, KOSTL, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO )
*        INSERT INTO SAPSR3.ZGL026_DRE_ACM ( MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, KOSTL, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO )
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
        SELECT * FROM (
        SELECT MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, KOSTL,
               SUM(VLR_REA)        AS VLR_REA,
               SUM(VLR_DOLAR)      AS VLR_DOLAR,
               SUM(VLR_DOLAR_CONV) AS VLR_DOLAR_CONV,
               SUM(VLR_GRUPO)      AS VLR_GRUPO
          FROM (SELECT MANDT, BUKRS, VERSN, :PMONAT AS MONAT, GJAHR, NIVEL, SAKNR, KOSTL, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
                  FROM SAPHANADB.ZGL026_DRE_ACM A
*                  FROM SAPSR3.ZGL026_DRE_ACM A
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
                 WHERE A.BUKRS = :PBUKRS
                   AND A.VERSN = :PVERSN
                   AND A.MONAT = :PMONATA
                   AND A.GJAHR = :PGJAHR
                 UNION ALL
                SELECT MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, KOSTL, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
                  FROM SAPHANADB.ZGL022_DRE_DADOS A
*                  FROM SAPSR3.ZGL022_DRE_DADOS A
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
                 WHERE A.BUKRS = :PBUKRS
                   AND A.VERSN = :PVERSN
                   AND A.MONAT = :PMONAT
                   AND A.GJAHR = :PGJAHR ) TT
         GROUP BY MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, KOSTL ) TT
         WHERE NOT ( TT.VLR_REA	= 0 AND TT.VLR_DOLAR = 0 AND TT.VLR_DOLAR_CONV = 0 AND TT.VLR_GRUPO	= 0 )
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      CONCATENATE 'Inclui Saldo ZGL026_DRE_ACM (Insert)' error_text INTO error_text SEPARATED BY space.
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  "Acumulador de Centro de Lucro
  TRY.
      EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
        INSERT INTO SAPHANADB.ZGL027_DRE_ACM ( MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, PRCTR, QTD_TON, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO )
*        INSERT INTO SAPSR3.ZGL027_DRE_ACM ( MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, PRCTR, QTD_TON, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO )
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
        SELECT * FROM (
        SELECT MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, PRCTR,
               SUM(QTD_TON)        AS QTD_TON,
               SUM(VLR_REA)        AS VLR_REA,
               SUM(VLR_DOLAR)      AS VLR_DOLAR,
               SUM(VLR_DOLAR_CONV) AS VLR_DOLAR_CONV,
               SUM(VLR_GRUPO)      AS VLR_GRUPO
          FROM (SELECT MANDT, BUKRS, VERSN, :PMONAT AS MONAT, GJAHR, NIVEL, SAKNR, PRCTR, QTD_TON, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
                  FROM SAPHANADB.ZGL027_DRE_ACM A
*                  FROM SAPSR3.ZGL027_DRE_ACM A
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
                 WHERE A.BUKRS = :PBUKRS
                   AND A.VERSN = :PVERSN
                   AND A.MONAT = :PMONATA
                   AND A.GJAHR = :PGJAHR
                 UNION ALL
                SELECT MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, PRCTR, QTD_TON, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
                  FROM SAPHANADB.ZGL023_DRE_DADOS A
*                  FROM SAPSR3.ZGL023_DRE_DADOS A
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
                 WHERE A.BUKRS = :PBUKRS
                   AND A.VERSN = :PVERSN
                   AND A.MONAT = :PMONAT
                   AND A.GJAHR = :PGJAHR ) TT
         GROUP BY MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, PRCTR ) TT
         WHERE NOT ( TT.VLR_REA	= 0 AND TT.VLR_DOLAR = 0 AND TT.VLR_DOLAR_CONV = 0 AND TT.VLR_GRUPO	= 0 )
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      CONCATENATE 'Inclui Saldo ZGL027_DRE_ACM (Insert)' error_text INTO error_text SEPARATED BY space.
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  "Acumulador de Grupo de Mercadoria
  TRY.
      EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
        INSERT INTO SAPHANADB.ZGL028_DRE_ACM ( MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, MATKL, QTD_TON, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO )
*        INSERT INTO SAPSR3.ZGL028_DRE_ACM ( MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, MATKL, QTD_TON, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO )
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
        SELECT * FROM (
        SELECT MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, MATKL,
               SUM(QTD_TON)        AS QTD_TON,
               SUM(VLR_REA)        AS VLR_REA,
               SUM(VLR_DOLAR)      AS VLR_DOLAR,
               SUM(VLR_DOLAR_CONV) AS VLR_DOLAR_CONV,
               SUM(VLR_GRUPO)      AS VLR_GRUPO
          FROM (SELECT MANDT, BUKRS, VERSN, :PMONAT AS MONAT, GJAHR, NIVEL, SAKNR, MATKL, QTD_TON, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
                  FROM SAPHANADB.ZGL028_DRE_ACM A
*                  FROM SAPSR3.ZGL028_DRE_ACM A
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
                 WHERE A.BUKRS = :PBUKRS
                   AND A.VERSN = :PVERSN
                   AND A.MONAT = :PMONATA
                   AND A.GJAHR = :PGJAHR
                 UNION ALL
                SELECT MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, MATKL, QTD_TON, VLR_REA, VLR_DOLAR, VLR_DOLAR_CONV, VLR_GRUPO
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
                  FROM SAPHANADB.ZGL024_DRE_DADOS A
*                  FROM SAPSR3.ZGL024_DRE_DADOS A
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
                 WHERE A.BUKRS = :PBUKRS
                   AND A.VERSN = :PVERSN
                   AND A.MONAT = :PMONAT
                   AND A.GJAHR = :PGJAHR ) TT
         GROUP BY MANDT, BUKRS, VERSN, MONAT, GJAHR, NIVEL, SAKNR, MATKL ) TT
         WHERE NOT ( TT.VLR_REA	= 0 AND TT.VLR_DOLAR = 0 AND TT.VLR_DOLAR_CONV = 0 AND TT.VLR_GRUPO	= 0 AND TT.QTD_TON = 0 )
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      CONCATENATE 'Inclui Saldo ZGL028_DRE_ACM (Insert)' error_text INTO error_text SEPARATED BY space.
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  "Acumulador de Conta Razão / Centro de Custo / Centro de Lucro / Grupo de Mercadoria / Sociedade Parceira
  TRY.
      EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
        INSERT INTO SAPHANADB.ZGL030_DRE_ACM ( MANDT, BUKRS, VERSN, MONAT, GJAHR, SAKNR, KOSTL, PRCTR, MATKL, VBUND, QTD_TON, VLR_REA, VLR_DOLAR, VLR_GRUPO )
*        INSERT INTO SAPSR3.ZGL030_DRE_ACM ( MANDT, BUKRS, VERSN, MONAT, GJAHR, SAKNR, KOSTL, PRCTR, MATKL, VBUND, QTD_TON, VLR_REA, VLR_DOLAR, VLR_GRUPO )
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
        SELECT * FROM (
        SELECT MANDT, BUKRS, VERSN, MONAT, GJAHR, SAKNR, KOSTL, PRCTR, MATKL, VBUND,
               SUM(QTD_TON)        AS QTD_TON,
               SUM(VLR_REA)        AS VLR_REA,
               SUM(VLR_DOLAR)      AS VLR_DOLAR,
               SUM(VLR_GRUPO)      AS VLR_GRUPO
          FROM (SELECT MANDT, BUKRS, VERSN, :PMONAT AS MONAT, GJAHR, SAKNR, KOSTL, PRCTR, MATKL, VBUND, QTD_TON, VLR_REA, VLR_DOLAR, VLR_GRUPO
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
                  FROM SAPHANADB.ZGL030_DRE_ACM A
*                  FROM SAPSR3.ZGL030_DRE_ACM A
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
                 WHERE A.BUKRS = :PBUKRS
                   AND A.VERSN = :PVERSN
                   AND A.MONAT = :PMONATA
                   AND A.GJAHR = :PGJAHR
                 UNION ALL
                SELECT MANDT, BUKRS, VERSN, MONAT, GJAHR, SAKNR, KOSTL, PRCTR, MATKL, VBUND, QTD_TON, VLR_REA, VLR_DOLAR, VLR_GRUPO
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
                  FROM SAPHANADB.ZGL029_DRE_DADOS A
*                  FROM SAPSR3.ZGL029_DRE_DADOS A
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
                 WHERE A.BUKRS = :PBUKRS
                   AND A.VERSN = :PVERSN
                   AND A.MONAT = :PMONAT
                   AND A.GJAHR = :PGJAHR ) TT
         GROUP BY MANDT, BUKRS, VERSN, MONAT, GJAHR, SAKNR, KOSTL, PRCTR, MATKL, VBUND ) TT
         WHERE NOT ( TT.VLR_REA	= 0 AND TT.VLR_DOLAR = 0 AND TT.VLR_GRUPO	= 0 AND TT.QTD_TON = 0 )
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      CONCATENATE 'Inclui Saldo ZGL030_DRE_ACM (Insert)' error_text INTO error_text SEPARATED BY space.
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

ENDFUNCTION.
