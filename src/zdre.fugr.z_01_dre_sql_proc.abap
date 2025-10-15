FUNCTION z_01_dre_sql_proc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(PBUKRS) TYPE  BUKRS DEFAULT '0001'
*"     REFERENCE(PGJAHR) TYPE  GJAHR DEFAULT '2015'
*"     REFERENCE(PPOPER) TYPE  POPER DEFAULT '001'
*"     REFERENCE(PVERSN) TYPE  VERSN_011 DEFAULT 'AM15'
*"  TABLES
*"      IT_REGISTRO STRUCTURE  ZDRE_SQL_PROC
*"  EXCEPTIONS
*"      ERRO_SQL
*"      ERRO_DRE
*"----------------------------------------------------------------------

  DATA: exc_ref             TYPE REF TO cx_sy_native_sql_error,
        error_text          TYPE string,
        wa_registro         TYPE zdre_sql_proc,
        pbukrs_01           TYPE bukrs,
        pbukrs_0b           TYPE bukrs,
        wa_zgl015_dre_est08 TYPE TABLE OF zgl015_dre_est08 WITH HEADER LINE,
        lc_kokrs            TYPE kokrs.

  SELECT SINGLE kokrs INTO lc_kokrs FROM tka02 WHERE bukrs EQ pbukrs.

  "Ajuste de empresa do parâmetro"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "Empresa Base Consulta
  MOVE pbukrs TO pbukrs_0b.

  "Empresa Base Estrutura
  SELECT SINGLE * INTO wa_zgl015_dre_est08 FROM zgl015_dre_est08
   WHERE versn   EQ pversn
     AND bukrs_b EQ pbukrs_0b.

  IF sy-subrc IS INITIAL.
    pbukrs_01 = wa_zgl015_dre_est08-bukrs.
  ELSE.
    pbukrs_01 = pbukrs.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  TRY.
      EXEC SQL.
        OPEN REGISTROS FOR
          SELECT TT.BUKRS,
                 TT.VERSN,
                 TT.GJAHR,
                 TT.POPER,
                 TT.SAKNR,

                 TT.SOC_PARCEIRA,
                 TT.TIPO_CUSTO,
                 TT.KOSTL,
                 TT.CENTRO_LUCRO,
                 TT.GP_MERCADORIA,
                 TT.MOEDA_01 * CASE WHEN TT.BUKRS = '0101' THEN 100 ELSE 1 END AS MOEDA_01,
                 TT.MOEDA_02 AS MOEDA_02,
                 TT.MOEDA_03 AS MOEDA_03,
                 TT.QTDE
            FROM (
          SELECT A1.MANDT, DR2.BUKRS, DR2.GJAHR, CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END AS POPER,
                 A1.VERSN, A1.SAKNR, A1.NIVEL, DR2.VBUND AS SOC_PARCEIRA, '' AS TIPO_CUSTO, '' AS KOSTL, '' AS CENTRO_LUCRO, '' AS GP_MERCADORIA,
                 SUM(DR2.VLHSL) AS MOEDA_01, SUM(DR2.VLKSL) AS MOEDA_02, SUM(DR2.VLOSL) AS MOEDA_03,
                 SUM( CASE WHEN EXISTS ( SELECT *
                                           FROM SAPHANADB.ZGL015_DRE_EST02 A2
                                          WHERE A2.MANDT = A1.MANDT
                                            AND A2.BUKRS = A1.BUKRS
                                            AND A2.VERSN = A1.VERSN
                                            AND A2.NIVEL LIKE SUBSTR(A1.NIVEL,1,2)||'%'
                                            AND A2.NIVEL <= A1.NIVEL
                                            AND A2.LEVAR_QTDE = 'X' ) OR ( A1.LEVAR_QTDE = 'X' ) THEN DR2.QTMSL
                           ELSE 0 END ) AS QTDE
            FROM SAPHANADB.ZGL015_DRE_EST03 A1,
                 SAPHANADB.ZGLT_DRE_02      DR2
           WHERE 1 = 1
             AND A1.BUKRS  = :PBUKRS_01
             AND A1.VERSN  = :PVERSN
             AND A1.MANDT  = DR2.MANDT
             AND DR2.BUKRS = :PBUKRS_0B
             AND A1.SAKNR  = DR2.SAKNR
             AND DR2.GJAHR = :PGJAHR
             AND CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END = :PPOPER
             AND NOT EXISTS ( SELECT * FROM SAPHANADB.ZGL015_DRE_EST04 A2
                               WHERE A2.BUKRS = A1.BUKRS
                                 AND A2.VERSN = A1.VERSN
                                 AND A2.KTOPL = A1.KTOPL
                                 AND A2.SAKNR = A1.SAKNR )
             AND NOT EXISTS ( SELECT * FROM SAPHANADB.ZGL015_DRE_EST05 A2
                               WHERE A2.VERSN = A1.VERSN
                                 AND A2.KTOPL = A1.KTOPL
                                 AND A2.SAKNR = A1.SAKNR
                                 AND A2.KOKRS = :LC_KOKRS )
             AND NOT EXISTS ( SELECT * FROM SAPHANADB.ZGL015_DRE_EST06 A2
                               WHERE A2.BUKRS = A1.BUKRS
                                 AND A2.VERSN = A1.VERSN
                                 AND A2.KTOPL = A1.KTOPL
                                 AND A2.SAKNR = A1.SAKNR )
          GROUP BY A1.MANDT, DR2.BUKRS, DR2.GJAHR, CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END, A1.VERSN, A1.SAKNR, A1.NIVEL, DR2.VBUND

          UNION ALL

          SELECT A1.MANDT, DR2.BUKRS, DR2.GJAHR, CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END AS POPER,
                 A1.VERSN, A1.SAKNR, A1.NIVEL, DR2.VBUND AS SOC_PARCEIRA, A2.KOSAR AS TIPO_CUSTO, DR2.KOSTL, '' AS CENTRO_LUCRO, '' AS GP_MERCADORIA,
                 SUM(DR2.VLHSL) AS MOEDA_01, SUM(DR2.VLKSL) AS MOEDA_02, SUM(DR2.VLOSL) AS MOEDA_03,
                 SUM( CASE WHEN EXISTS ( SELECT *
                                           FROM SAPHANADB.ZGL015_DRE_EST02 A2
                                          WHERE A2.MANDT = A1.MANDT
                                            AND A2.BUKRS = A1.BUKRS
                                            AND A2.VERSN = A1.VERSN
                                            AND A2.NIVEL LIKE SUBSTR(A1.NIVEL,1,2)||'%'
                                            AND A2.NIVEL <= A1.NIVEL
                                            AND A2.LEVAR_QTDE = 'X' ) OR ( A1.LEVAR_QTDE = 'X' )  THEN DR2.QTMSL
                           ELSE 0 END ) AS QTDE
            FROM SAPHANADB.ZGL015_DRE_EST03 A1,
                 SAPHANADB.ZGL015_DRE_EST04 A2,
                 SAPHANADB.ZGLT_DRE_02      DR2

           WHERE 1 = 1
             AND A1.BUKRS  = :PBUKRS_01
             AND A1.VERSN  = :PVERSN
             AND A1.BUKRS  = A2.BUKRS
             AND A1.VERSN  = A2.VERSN
             AND A1.KTOPL  = A2.KTOPL
             AND A1.SAKNR  = A2.SAKNR
             AND A1.NIVEL  = A2.NIVEL
             AND A1.MANDT  = DR2.MANDT
             AND DR2.BUKRS = :PBUKRS_0B
             AND A1.SAKNR  = DR2.SAKNR
             AND A2.KOSAR  = DR2.KOSAR
             AND DR2.GJAHR = :PGJAHR
             AND CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END = :PPOPER
             AND CASE WHEN TRIM(DR2.AUFNR) = '' THEN 'S'
                      WHEN EXISTS ( SELECT * FROM SAPHANADB.AUFK C WHERE C.MANDT = A1.MANDT AND C.AUFNR = DR2.AUFNR AND C.AUART IN ('ZSTA','ZSIN') ) THEN 'S'
                 END = 'S'
          GROUP BY A1.MANDT, DR2.BUKRS, DR2.GJAHR, CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END, A1.VERSN, A1.SAKNR, A1.NIVEL, DR2.VBUND, A2.KOSAR, DR2.KOSTL

          UNION ALL

          SELECT A1.MANDT, DR2.BUKRS, DR2.GJAHR, CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END AS POPER, A1.VERSN, A1.SAKNR, A1.NIVEL, DR2.VBUND AS SOC_PARCEIRA, '' AS TIPO_CUSTO, '' AS KOSTL, DR2.PRCTR AS
                 CENTRO_LUCRO, '' AS GP_MERCADORIA,
                 SUM(DR2.VLHSL) AS MOEDA_01, SUM(DR2.VLKSL) AS MOEDA_02, SUM(DR2.VLOSL) AS MOEDA_03,
                 SUM( CASE WHEN EXISTS ( SELECT *
                                           FROM SAPHANADB.ZGL015_DRE_EST02 A2
                                          WHERE A2.MANDT = A1.MANDT
                                            AND A2.BUKRS = A1.BUKRS
                                            AND A2.VERSN = A1.VERSN
                                            AND A2.NIVEL LIKE SUBSTR(A1.NIVEL,1,2)||'%'
                                            AND A2.NIVEL <= A1.NIVEL
                                            AND A2.LEVAR_QTDE = 'X' ) OR ( A1.LEVAR_QTDE = 'X' )  THEN DR2.QTMSL
                           ELSE 0 END ) AS QTDE
            FROM SAPHANADB.ZGL015_DRE_EST03 A1,
                 SAPHANADB.ZGL015_DRE_EST05 A2,
                 SAPHANADB.ZGLT_DRE_02      DR2

           WHERE 1 = 1
             AND A1.BUKRS  = :PBUKRS_01
             AND A1.VERSN  = :PVERSN
             AND A1.MANDT  = A2.MANDT
             AND A1.VERSN  = A2.VERSN
             AND A1.KTOPL  = A2.KTOPL
             AND A1.SAKNR  = A2.SAKNR
             AND A1.NIVEL  = A2.NIVEL
             AND A2.KOKRS  = :LC_KOKRS
             AND DR2.BUKRS = :PBUKRS_0B
             AND A2.SAKNR  = DR2.SAKNR
             AND A2.KOKRS  = DR2.KOKRS
             AND A2.PRCTR  = DR2.PRCTR
             AND DR2.GJAHR = :PGJAHR
             AND CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END = :PPOPER
             AND TRIM(DR2.MATKL) = ''
             AND CASE WHEN TRIM(DR2.AUFNR) = '' THEN 'S'
                      WHEN EXISTS ( SELECT * FROM SAPHANADB.AUFK C WHERE C.MANDT = A1.MANDT AND C.AUFNR = DR2.AUFNR AND C.AUART IN ('ZSTA','ZSIN') ) THEN 'S'
                 END = 'S'
          GROUP BY A1.MANDT, DR2.BUKRS, DR2.GJAHR, CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END, A1.VERSN, A1.SAKNR, A1.NIVEL, DR2.VBUND, DR2.PRCTR

          UNION ALL

          SELECT A1.MANDT, DR2.BUKRS, DR2.GJAHR, CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END AS POPER, A1.VERSN, A1.SAKNR, A1.NIVEL, DR2.VBUND AS SOC_PARCEIRA, '' AS TIPO_CUSTO, '' AS KOSTL, '' AS CENTRO_LUCRO,
                 DR2.MATKL AS GP_MERCADORIA,
                 SUM(DR2.VLHSL) AS MOEDA_01, SUM(DR2.VLKSL) AS MOEDA_02, SUM(DR2.VLOSL) AS MOEDA_03,
                 SUM( CASE WHEN EXISTS ( SELECT *
                                           FROM SAPHANADB.ZGL015_DRE_EST02 A2
                                          WHERE A2.MANDT = A1.MANDT
                                            AND A2.BUKRS = A1.BUKRS
                                            AND A2.VERSN = A1.VERSN
                                            AND A2.NIVEL LIKE SUBSTR(A1.NIVEL,1,2)||'%'
                                            AND A2.NIVEL <= A1.NIVEL
                                            AND A2.LEVAR_QTDE = 'X' ) OR ( A1.LEVAR_QTDE = 'X' ) THEN DR2.QTMSL
                           ELSE 0 END ) AS QTDE
            FROM SAPHANADB.ZGL015_DRE_EST03 A1,
                 SAPHANADB.ZGL015_DRE_EST06 A2,
                 SAPHANADB.ZGLT_DRE_02      DR2
           WHERE 1 = 1
             AND A1.BUKRS  = :PBUKRS_01
             AND A1.VERSN  = :PVERSN
             AND A1.BUKRS  = A2.BUKRS
             AND A1.VERSN  = A2.VERSN
             AND A1.KTOPL  = A2.KTOPL
             AND A1.SAKNR  = A2.SAKNR
             AND A1.NIVEL  = A2.NIVEL
             AND A2.MANDT  = DR2.MANDT
             AND DR2.BUKRS = :PBUKRS_0B
             AND A2.SAKNR  = DR2.SAKNR
             AND A2.MATKL  = DR2.MATKL
             AND DR2.GJAHR = :PGJAHR
             AND CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END = :PPOPER
             AND TRIM(DR2.MATKL) <> ''
             AND NOT EXISTS ( SELECT * FROM SAPHANADB.ZGL015_DRE_CVEND DEP2 WHERE DEP2.BUKRS = DR2.BUKRS AND DEP2.HKONT = A1.SAKNR AND DEP2.MATKL = A2.MATKL AND TRIM(DR2.KOSTL) <> '' )
             AND CASE WHEN TRIM(DR2.AUFNR) = '' THEN 'S'
                      WHEN EXISTS ( SELECT * FROM SAPHANADB.AUFK C WHERE C.MANDT = A1.MANDT AND C.AUFNR = DR2.AUFNR AND C.AUART IN ('ZSTA','ZSIN') ) THEN 'S'
                 END = 'S'
          GROUP BY A1.MANDT, DR2.BUKRS, DR2.GJAHR, CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END, A1.VERSN, A1.SAKNR, A1.NIVEL, DR2.VBUND, DR2.MATKL

          UNION ALL

          SELECT A1.MANDT, DR2.BUKRS, DR2.GJAHR, CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END AS POPER,
                 A1.VERSN, A1.SAKNR, A1.NIVEL, DR2.VBUND AS SOC_PARCEIRA, '' AS TIPO_CUSTO, '' AS KOSTL, '' AS CENTRO_LUCRO, DEP.MATKL AS GP_MERCADORIA,
                 SUM(DR2.VLHSL) AS MOEDA_01, SUM(DR2.VLKSL) AS MOEDA_02, SUM(DR2.VLOSL) AS MOEDA_03,
                           SUM(CASE WHEN EXISTS ( SELECT *
                                                     FROM SAPHANADB.ZGL015_DRE_EST02 A2
                                                    WHERE A2.MANDT = A1.MANDT
                                                      AND A2.BUKRS = A1.BUKRS
                                                      AND A2.VERSN = A1.VERSN
                                                      AND A2.NIVEL LIKE SUBSTR(A1.NIVEL,1,2)||'%'
                                                      AND A2.NIVEL <= A1.NIVEL
                                                      AND A2.LEVAR_QTDE = 'X' ) OR ( A1.LEVAR_QTDE = 'X' ) THEN DR2.QTMSL
                                     ELSE 0 END) AS QTDE
            FROM SAPHANADB.ZGL015_DRE_EST03 A1,
                 SAPHANADB.ZGL015_DRE_EST06 A2,
                 SAPHANADB.ZGLT_DRE_02      DR2,
                 SAPHANADB.ZGL015_DRE_CVEND DEP

           WHERE 1 = 1
             AND A1.BUKRS  = :PBUKRS_01
             AND A1.VERSN  = :PVERSN
             AND A1.BUKRS  = A2.BUKRS
             AND A1.VERSN  = A2.VERSN
             AND A1.KTOPL  = A2.KTOPL
             AND A1.SAKNR  = A2.SAKNR
             AND A1.NIVEL  = A2.NIVEL
             AND A2.MANDT  = DR2.MANDT
             AND DR2.BUKRS = :PBUKRS_0B
             AND A2.SAKNR  = DR2.SAKNR
             AND DR2.BUKRS = DEP.BUKRS
             AND A1.SAKNR  = DEP.HKONT
             AND DR2.KOSTL = DEP.KOSTL
             AND A2.MATKL  = DEP.MATKL
             AND DR2.GJAHR = :PGJAHR
             AND CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END = :PPOPER
             AND EXISTS ( SELECT * FROM SAPHANADB.ZGL015_DRE_CVEND DEP2 WHERE DEP2.BUKRS = DR2.BUKRS AND DEP2.HKONT = A1.SAKNR AND DEP2.MATKL = A2.MATKL AND TRIM(DR2.KOSTL) <> '' )
             AND CASE WHEN TRIM(DR2.AUFNR) = '' THEN 'S'
                      WHEN EXISTS ( SELECT * FROM SAPHANADB.COAS C WHERE C.MANDT = A1.MANDT AND C.AUFNR = DR2.AUFNR AND C.AUART IN ('ZSTA','ZSIN') ) THEN 'S'
                 END = 'S'
          GROUP BY A1.MANDT, DR2.BUKRS, DR2.GJAHR, CASE WHEN DR2.POPER >= '013' AND DR2.POPER <= '015' THEN '012' ELSE DR2.POPER END,
                A1.VERSN, A1.SAKNR, A1.NIVEL, DR2.VBUND, DEP.MATKL ) TT

      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  DO.
    EXEC SQL.
      FETCH NEXT REGISTROS INTO
      :WA_REGISTRO-BUKRS,
      :WA_REGISTRO-VERSN,
      :WA_REGISTRO-GJAHR,
      :WA_REGISTRO-POPER,
      :WA_REGISTRO-SAKNR,
      :WA_REGISTRO-VBUND,
      :WA_REGISTRO-KOSAR,
      :WA_REGISTRO-KOSTL,
      :WA_REGISTRO-PRCTR,
      :WA_REGISTRO-MATKL,
      :WA_REGISTRO-VLHSL,
      :WA_REGISTRO-VLKSL,
      :WA_REGISTRO-VLOSL,
      :WA_REGISTRO-QTMSL.
    ENDEXEC.
    IF sy-subrc <> 0.
      EXIT.
    ELSE.
      APPEND wa_registro TO it_registro.
    ENDIF.
  ENDDO.

  EXEC SQL.
    CLOSE REGISTROS
  ENDEXEC.

ENDFUNCTION.
