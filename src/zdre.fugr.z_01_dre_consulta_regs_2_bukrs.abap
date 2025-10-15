FUNCTION z_01_dre_consulta_regs_2_bukrs.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LC_DATA_INI) TYPE  TIMESTAMP
*"     REFERENCE(LC_DATA_FIM) TYPE  TIMESTAMP
*"     REFERENCE(PCARGA) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_BUKRS) TYPE  BUKRS
*"  EXPORTING
*"     REFERENCE(PARCIAL) TYPE  CHAR01
*"  TABLES
*"      IT_LANC STRUCTURE  ZGLT_DRE_04
*"  EXCEPTIONS
*"      ERRO_SQL
*"----------------------------------------------------------------------

  DATA: exc_ref    TYPE REF TO cx_sy_native_sql_error,
        error_text TYPE string,
        wa_lanc    TYPE zglt_dre_04,
        lc_linhas  TYPE i,
        lc_matkl   TYPE matkl.

  DATA: lc_data_fimc TYPE  timestamp,
        lc_data_inic TYPE  timestamp,
        i_menge      LIKE  ekpo-menge,
        e_menge      LIKE  ekpo-menge.

  DATA: lva_limite_reg TYPE i.

  lva_limite_reg = 10000.

  SELECT SINGLE *
    FROM setleaf INTO @DATA(lwa_setleaf_limite_reg)
   WHERE setname EQ 'DRE_LIMITE_REG'.

  IF sy-subrc EQ 0 AND lwa_setleaf_limite_reg-valfrom IS NOT INITIAL.
    lva_limite_reg = lwa_setleaf_limite_reg-valfrom.
  ENDIF.

  lc_data_inic = lc_data_ini.
  lc_data_fimc = lc_data_fim.
  lc_data_inic = lc_data_ini - 770000.
  lc_data_fimc = lc_data_fim + 40001.
  "Consultando Registros SAP
  TRY.
      EXEC SQL.
        OPEN DOCUMENTOS_2 FOR
          SELECT *
            FROM (
          SELECT TT.*,
                 EM.KTOPL,
                 CO.KOSAR
            FROM (
          SELECT DISTINCT
                 FI.BUDAT,
                 FI.TIMESTAMP,
                 FI.DOCNR,
                 FI.DOCLN,
                 FI.BELNR,
                 FI.BUZEI,
                 FI.RCLNT,
                 FI.RYEAR,
                 FI.POPER,
                 FI.RBUKRS,
                 FI.RACCT,
                 FI.KOKRS,
                 FI.AWTYP,
                 FI.DRCRK,
                 FI.RTCUR,
                 FI.TSL,
                 FI.HSL,
                 FI.KSL,
                 FI.OSL,

                  CASE WHEN FI.RUNIT <> ' ' THEN FI.RUNIT
                       WHEN CC.MEINH <> ' ' THEN CC.MEINH
                       ELSE
                  CASE WHEN FI.AWTYP = 'VBRK' THEN
                         CASE WHEN CC.KOKRS = 'MAGI' THEN
                                coalesce((SELECT VR.GEWEI FROM SAPHANADB.VBRP VR WHERE VR.VBELN = FI.AWREF AND VR.POSNR = MGB.KDPOS AND VR.SPART <> '08'),FI.RUNIT)
                              WHEN CC.KOKRS = 'MGLD' THEN
                                coalesce((SELECT VR.GEWEI FROM SAPHANADB.VBRP VR WHERE VR.VBELN = FI.AWREF AND VR.POSNR = MGH.KDPOS AND VR.SPART <> '08'),FI.RUNIT)
                              WHEN CC.KOKRS = 'MGAR' THEN
                                coalesce((SELECT VR.GEWEI FROM SAPHANADB.VBRP VR WHERE VR.VBELN = FI.AWREF AND VR.POSNR = MGA.KDPOS AND VR.SPART <> '08'),FI.RUNIT)
                              WHEN CC.KOKRS = 'MGBG' THEN
                                coalesce((SELECT VR.GEWEI FROM SAPHANADB.VBRP VR WHERE VR.VBELN = FI.AWREF AND VR.POSNR = MGG.KDPOS AND VR.SPART <> '08'),FI.RUNIT)
                              WHEN CC.KOKRS = 'MGTF' THEN
                                coalesce((SELECT VR.GEWEI FROM SAPHANADB.VBRP VR WHERE VR.VBELN = FI.AWREF AND VR.POSNR = MGT.KDPOS AND VR.SPART <> '08'),FI.RUNIT)
                              ELSE FI.RUNIT
                         END
                       ELSE FI.RUNIT
                  END END AS RUNIT,

                  CASE WHEN coalesce(FI.MSL,0) <> 0    THEN FI.MSL
                       WHEN coalesce(CC.MEGBTR,0) <> 0 THEN CC.MEGBTR
                       ELSE
                  CASE WHEN FI.AWTYP = 'VBRK' THEN
                         CASE WHEN CC.KOKRS = 'MAGI' THEN
                                coalesce((SELECT VR.NTGEW FROM SAPHANADB.VBRP VR WHERE VR.VBELN = FI.AWREF AND VR.POSNR = MGB.KDPOS AND VR.SPART <> '08') * CASE WHEN FI.DRCRK = 'S' THEN 1 ELSE -1 END ,FI.MSL)
                              WHEN CC.KOKRS = 'MGLD' THEN
                                coalesce((SELECT VR.NTGEW FROM SAPHANADB.VBRP VR WHERE VR.VBELN = FI.AWREF AND VR.POSNR = MGH.KDPOS AND VR.SPART <> '08') * CASE WHEN FI.DRCRK = 'S' THEN 1 ELSE -1 END ,FI.MSL)
                              WHEN CC.KOKRS = 'MGAR' THEN
                                coalesce((SELECT VR.NTGEW FROM SAPHANADB.VBRP VR WHERE VR.VBELN = FI.AWREF AND VR.POSNR = MGA.KDPOS AND VR.SPART <> '08') * CASE WHEN FI.DRCRK = 'S' THEN 1 ELSE -1 END ,FI.MSL)
                              WHEN CC.KOKRS = 'MGBG' THEN
                                coalesce((SELECT VR.NTGEW FROM SAPHANADB.VBRP VR WHERE VR.VBELN = FI.AWREF AND VR.POSNR = MGG.KDPOS AND VR.SPART <> '08') * CASE WHEN FI.DRCRK = 'S' THEN 1 ELSE -1 END ,FI.MSL)
                              WHEN CC.KOKRS = 'MGTF' THEN
                                coalesce((SELECT VR.NTGEW FROM SAPHANADB.VBRP VR WHERE VR.VBELN = FI.AWREF AND VR.POSNR = MGT.KDPOS AND VR.SPART <> '08') * CASE WHEN FI.DRCRK = 'S' THEN 1 ELSE -1 END ,FI.MSL)
                              ELSE FI.MSL
                         END
                       ELSE FI.MSL
                  END
                  END AS MSL,

                 ( SELECT TRIM(BB.AUFNR) FROM SAPHANADB.BSIS BB WHERE BB.MANDT = FI.RCLNT AND BB.BUKRS = FI.RBUKRS AND BB.BELNR = FI.BELNR AND BB.BUZEI = FI.BUZEI ) AS AUFNR,

                 CASE WHEN CC.KOKRS = 'MAGI' THEN
                        CASE WHEN TRIM(MGB.KMMAKL) = '' AND SUBSTR(FI.AWREF,1,2) = 'ZG' AND SUBSTR(FI.AWREF,1,3) <> 'ZGF' THEN
                               (SELECT MAX( TRIM(J1.MATKL) )
                                  FROM SAPHANADB.J_1BNFLIN J1
                                 WHERE J1.REFTYP = 'ZW'
                                   AND SUBSTR(J1.REFKEY,1,10) = SUBSTR(FI.AWREF||FI.AWORG,4,10)
                                    )
                             WHEN TRIM(FI.RUNIT) = '' AND SUBSTR(FI.AWREF,1,2) = 'ZG' AND SUBSTR(FI.AWREF,1,3) = 'ZGF' THEN
                               (SELECT MAX( TRIM(J1.MATKL) )
                                  FROM SAPHANADB.J_1BNFLIN    J1,
                                       SAPHANADB.FAGL_SPLINFO CO2
                                 WHERE CO2.GJAHR = SUBSTR(FI.AWORG,4,4)
                                   AND CO2.BUKRS = FI.BUKRS
                                   AND CO2.BELNR = SUBSTR(FI.AWREF||FI.AWORG,4,10)
                                   AND J1.REFTYP = 'ZW'
                                   AND SUBSTR(J1.REFKEY,1,10) = SUBSTR(CO2.AWREF||CO2.AWORG,4,10)
                                    )
                             ELSE TRIM(MGB.KMMAKL)
                        END
                      WHEN CC.KOKRS = 'MGLD' THEN
                        TRIM(MGH.KMMAKL)
                      WHEN CC.KOKRS = 'MGAR' THEN
                        TRIM(MGA.KMMAKL)
                      WHEN CC.KOKRS = 'MGBG' THEN
                        TRIM(MGG.KMMAKL)
                      WHEN CC.KOKRS = 'MGTF' THEN
                        TRIM(MGT.KMMAKL)
                      ELSE
                        FI.KMMAKL
                 END AS KMMAKL,

                 CASE WHEN CC.KOKRS = 'MAGI' THEN
                        TRIM(MGB.ARTNR)
                      WHEN CC.KOKRS = 'MGLD' THEN
                        TRIM(MGH.ARTNR)
                      WHEN CC.KOKRS = 'MGAR' THEN
                        TRIM(MGA.ARTNR)
                      WHEN CC.KOKRS = 'MGBG' THEN
                        TRIM(MGG.ARTNR)
                      WHEN CC.KOKRS = 'MGTF' THEN
                        TRIM(MGT.ARTNR)
                      ELSE
                        FI.ARTNR
                 END AS ARTNR,

                 CASE WHEN TRIM(FI.RCNTR) = '' THEN ( SELECT TRIM(BB.KOSTL) FROM SAPHANADB.BSIS BB WHERE BB.MANDT = FI.RCLNT AND BB.BUKRS = FI.RBUKRS AND BB.BELNR = FI.BELNR AND BB.BUZEI = FI.BUZEI )
                      ELSE TRIM(FI.RCNTR)
                 END AS RCNTR,
                 CASE WHEN FI.PRCTR = '0000009900' THEN '' ELSE TRIM(FI.PRCTR) END PRCTR,
                 TRIM(FI.RASSC) AS RASSC

            FROM (SELECT DISTINCT
                         FI.TIMESTAMP,
                         FI.BUDAT,
                         FI.BELNR AS DOCNR,
                         FI.DOCLN,
                         FI.RLDNR,
                         '001' AS RVERS,
                         FI.BELNR,
                         FI.RCLNT,
                         FI.RYEAR,
                         FI.AWTYP,
                         FI.BUZEI,
                         FI.KOKRS,
                         coalesce(CO.GJAHR,BK.GJAHR) AS GJAHR,
                         coalesce(CO.BUKRS,FI.RBUKRS) AS BUKRS,
                         coalesce(CO.AWORG, SUBSTR( BK.AWKEY,11,10) ) AS AWORG,
                         coalesce(CO.AWREF, SUBSTR( BK.AWKEY,01,10) ) AS AWREF,
                         FI.RASSC,
                         FI.PRCTR,
                         FI.RCNTR,
                         FI.TSL,
                         FI.HSL,
                         FI.KSL,
                         FI.OSL,
                         FI.RUNIT,
                         FI.MSL,
                         FI.RTCUR,
                         FI.DRCRK,
                         FI.RACCT,
                         FI.RBUKRS,
                         FI.POPER,
                         FI.KMMAKL_PA AS KMMAKL,
                         FI.MATNR AS ARTNR
                    FROM SAPHANADB.ACDOCA    FI LEFT JOIN (SELECT * FROM SAPHANADB.FAGL_SPLINFO CO WHERE TRIM(CO.AWREF) <> '' ) CO
                                        ON FI.RCLNT    = CO.MANDT
                         AND FI.RYEAR    = CO.GJAHR
                         AND FI.BELNR    = CO.BELNR
                         AND FI.RBUKRS   = CO.BUKRS
                                                   LEFT JOIN SAPHANADB.BKPF  BK ON FI.RCLNT    = BK.MANDT
                                         AND FI.RBUKRS   = BK.BUKRS
                                         AND FI.BELNR    = BK.BELNR

           ) FI LEFT JOIN ( SELECT * FROM SAPHANADB.COVP CC WHERE CC.SCOPE = 'PA' ) CC

                        ON FI.RCLNT    = CC.MANDT
                 AND FI.AWREF    = CC.REFBN
                 AND FI.AWTYP    = CC.AWTYP
                 AND FI.AWORG    = CC.AWORG
                 AND FI.BUKRS    = CC.REFBK
                 AND FI.GJAHR    = CC.REFGJ
                 AND FI.KOKRS    = CC.KOKRS
                 AND FI.BUZEI    = CC.REFBZ_FI


            LEFT JOIN  ( SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MAGI_ACCT T1 WHERE AKTBO   = 'X' AND PASUBNR = '0001'
                UNION ALL
                 SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MAGI T2 WHERE AKTBO   = 'X' AND PASUBNR = '0001' ) MGB

                 ON CC.PAOBJNR  = MGB.PAOBJNR
                AND CC.KOKRS    = MGB.KOKRS

            LEFT JOIN  (SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGLD_ACCT T1 WHERE AKTBO   = 'X' AND PASUBNR = '0001'
                 UNION ALL
                SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGLD T2 WHERE AKTBO   = 'X' AND PASUBNR = '0001' ) MGH

                ON CC.PAOBJNR  = MGH.PAOBJNR
               AND CC.KOKRS    = MGH.KOKRS

            LEFT JOIN  (SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGAR_ACCT T1 WHERE AKTBO   = 'X' AND PASUBNR = '0001'
                 UNION ALL
                SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGAR T2 WHERE AKTBO   = 'X' AND PASUBNR = '0001' ) MGA

                ON CC.PAOBJNR  = MGA.PAOBJNR
               AND CC.KOKRS    = MGA.KOKRS

            LEFT JOIN  (SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGBG_ACCT T1 WHERE AKTBO   = 'X' AND PASUBNR = '0001'
                 UNION ALL
                SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGBG T2 WHERE AKTBO   = 'X' AND PASUBNR = '0001' ) MGG

                ON CC.PAOBJNR  = MGG.PAOBJNR
               AND CC.KOKRS    = MGG.KOKRS

            LEFT JOIN  (SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGTF_ACCT T1 WHERE AKTBO   = 'X' AND PASUBNR = '0001'
                 UNION ALL
                SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGTF T2 WHERE AKTBO   = 'X' AND PASUBNR = '0001' ) MGT

                ON  CC.PAOBJNR  = MGT.PAOBJNR
                AND CC.KOKRS    = MGT.KOKRS


           WHERE FI.RCLNT   = '300'
             AND FI.TIMESTAMP BETWEEN :LC_DATA_INIC AND  :LC_DATA_FIMC
             AND FI.RBUKRS  = :P_BUKRS
             AND FI.RLDNR   = '0L'
             AND FI.RYEAR   >= '2015'
             AND FI.AWTYP IN ('VBRK','MKPF','IDOC','MLHD','MKPF','RMRP','BKPF','IDOC','CAJO','RMRP','TRAVL','AMBU','PRCHG','AUAK','AMDP','AIBU','AFRU','COBK','HRPAY')
             AND NOT EXISTS ( SELECT *
                                FROM SAPHANADB.FAGL_SPLINFO CO
                               WHERE CO.MANDT  = FI.RCLNT
                                 AND CO.GJAHR  = FI.RYEAR
                                 AND CO.BELNR  = FI.BELNR
                                 AND CO.BUKRS  = FI.RBUKRS
                                 AND CO.BUZEI  = FI.BUZEI )
           UNION ALL
          SELECT DISTINCT
                 FI.BUDAT,
                 FI.TIMESTAMP,
                 FI.BELNR AS DOCNR,
                 FI.DOCLN,
                 FI.BELNR,
                 FI.BUZEI,
                 FI.RCLNT,
                 FI.RYEAR,
                 FI.POPER,
                 FI.RBUKRS,
                 FI.RACCT,
                 FI.KOKRS,
                 FI.AWTYP,
                 FI.DRCRK,
                 FI.RTCUR,
                 FI.TSL,
                 FI.HSL,
                 FI.KSL,
                 FI.OSL,
                 FI.RUNIT,
                 FI.MSL,
                 CO.AUFNR,
                 '' AS KMMAKL,
                 '' AS ARTNR,
                 CASE WHEN TRIM(FI.RCNTR) <> '' THEN TRIM(FI.RCNTR) ELSE TRIM(CO.KOSTL) END AS RCNTR,
                 CASE WHEN FI.PRCTR = '0000009900' THEN '' ELSE TRIM(FI.PRCTR) END PRCTR,
                 TRIM(FI.RASSC) AS RASSC
            FROM SAPHANADB.ACDOCA    FI LEFT JOIN SAPHANADB.FAGL_SPLINFO CO
                                    ON FI.RCLNT   = CO.MANDT
                     AND FI.BELNR   = CO.BELNR
                     AND FI.RYEAR   = CO.GJAHR
                     AND FI.BUZEI   = CO.BUZEI
                     AND FI.RBUKRS  = CO.BUKRS


           WHERE FI.RCLNT   = '300'
             AND FI.TIMESTAMP BETWEEN :LC_DATA_INIC AND  :LC_DATA_FIMC
             AND FI.RBUKRS  = :P_BUKRS
             AND FI.RLDNR   = '0L'
             AND FI.RYEAR    >= '2015'
             AND FI.AWTYP IN ('VBRK','MKPF','IDOC','MLHD','MKPF','RMRP','BKPF','IDOC','CAJO','RMRP','TRAVL','AMBU','PRCHG','AUAK','AMDP','AIBU','AFRU','COBK','HRPAY')



             AND SUBSTR(CO.PAOBJNR,1,10) = '0000000000'
             AND NOT EXISTS ( SELECT *
                                FROM SAPHANADB.COVP CC
                               WHERE CC.MANDT   = CO.MANDT
                                 AND CC.REFBN   = CO.AWREF
                                 AND CC.AWTYP   = CO.AWTYP
                                 AND CC.AWORG   = CO.AWORG
                                 AND CC.REFBK   = CO.BUKRS
                                 AND CC.REFGJ   = CO.GJAHR
                                 AND CC.REFBZ   = CO.BUZEI
                                 AND TRIM(CO.AWORG) <> ''
                                 AND CC.PAOBJNR  <> '0000000000' )
           UNION ALL
          SELECT DISTINCT
                 FI.BUDAT,
                 FI.TIMESTAMP,
                 FI.BELNR AS DOCNR,
                 FI.DOCLN,
                 FI.BELNR,
                 FI.BUZEI,
                 FI.RCLNT,
                 FI.RYEAR,
                 FI.POPER,
                 FI.RBUKRS,
                 FI.RACCT,
                 FI.KOKRS,
                 FI.AWTYP,
                 FI.DRCRK,
                 FI.RTCUR,
                 FI.TSL,
                 FI.HSL,
                 FI.KSL,
                 FI.OSL,
                 FI.RUNIT,
                 FI.MSL,
                 CO.AUFNR,
                 FI.KMMAKL_PA AS KMMAKL,
                 FI.MATNR AS ARTNR,
                 CASE WHEN TRIM(FI.RCNTR) <> '' THEN TRIM(FI.RCNTR) ELSE TRIM(CO.KOSTL) END AS RCNTR,
                 CASE WHEN FI.PRCTR = '0000009900' THEN '' ELSE TRIM(FI.PRCTR) END PRCTR,
                 TRIM(FI.RASSC) AS RASSC

            FROM SAPHANADB.ACDOCA    FI,
                 SAPHANADB.FAGL_SPLINFO CO,
                 SAPHANADB.COBK         CC
           WHERE FI.RCLNT    = '300'
             AND FI.TIMESTAMP BETWEEN :LC_DATA_INIC AND  :LC_DATA_FIMC
             AND FI.RBUKRS   = :P_BUKRS
             AND FI.RLDNR    = '0L'
             AND FI.RYEAR    >= '2015'
             AND FI.AWTYP IN ('VBRK','MKPF','IDOC','MLHD','MKPF','RMRP','BKPF','IDOC','CAJO','RMRP','TRAVL','AMBU','PRCHG','AUAK','AMDP','AIBU','AFRU','COBK','HRPAY')
             AND FI.RCLNT    = CO.MANDT
             AND FI.BELNR    = CO.BELNR
             AND FI.RYEAR    = CO.GJAHR
             AND FI.RBUKRS   = CO.BUKRS
             AND FI.BUZEI    = CO.BUZEI
             AND SUBSTR(CO.PAOBJNR,1,10) = '0000000000'

             AND TRIM(CO.AWORG) <> ''
             AND CO.MANDT    = CC.MANDT
             AND CO.AWREF    = CC.REFBN
             AND CO.AWTYP    = CC.AWTYP
             AND CO.AWORG    = CC.AWORG
             AND CO.BUKRS    = CC.REFBK
             AND CO.GJAHR    = CC.REFGJ


           UNION ALL
          SELECT DISTINCT
                 FI.BUDAT,
                 FI.TIMESTAMP,
                 FI.BELNR AS DOCNR,
                 FI.DOCLN,
                 FI.BELNR,
                 FI.BUZEI,
                 FI.RCLNT,
                 FI.RYEAR,
                 FI.POPER,
                 FI.RBUKRS,
                 FI.RACCT,
                 FI.KOKRS,
                 FI.AWTYP,
                 FI.DRCRK,
                 FI.RTCUR,
                 FI.TSL,
                 FI.HSL,
                 FI.KSL,
                 FI.OSL,
                 FI.RUNIT,
                 FI.MSL,
                 CO.AUFNR,
                 FI.KMMAKL_PA AS KMMAKL,
                 FI.MATNR AS ARTNR,
                 CASE WHEN TRIM(FI.RCNTR) <> '' THEN TRIM(FI.RCNTR) ELSE TRIM(CO.KOSTL) END AS RCNTR,
                 CASE WHEN FI.PRCTR = '0000009900' THEN '' ELSE TRIM(FI.PRCTR) END PRCTR,
                 TRIM(FI.RASSC) AS RASSC

            FROM SAPHANADB.ACDOCA    FI,
                 SAPHANADB.FAGL_SPLINFO CO,
                 SAPHANADB.COBK         CC
           WHERE FI.RCLNT    = '300'
             AND FI.TIMESTAMP BETWEEN :LC_DATA_INIC AND  :LC_DATA_FIMC
             AND FI.RBUKRS   = :P_BUKRS
             AND FI.RLDNR    = '0L'
             AND FI.RYEAR    >= '2015'
             AND FI.AWTYP IN ('VBRK','MKPF','IDOC','MLHD','MKPF','RMRP','BKPF','IDOC','CAJO','RMRP','TRAVL','AMBU','PRCHG','AUAK','AMDP','AIBU','AFRU','COBK','HRPAY')
             AND FI.RCLNT    = CO.MANDT
             AND FI.BELNR    = CO.BELNR
             AND FI.RYEAR    = CO.GJAHR
             AND FI.RBUKRS   = CO.BUKRS
             AND FI.BUZEI    = CO.BUZEI
             AND SUBSTR(CO.PAOBJNR,1,10) = '0000000000'

             AND TRIM(CO.AWORG) <> ''
             AND CO.MANDT    = CC.MANDT
             AND CO.AWREF    = CC.REFBN
             AND CO.AWTYP    = CC.AWTYP
             AND CO.AWORG    = CC.AWORG
             AND CO.BUKRS    = CC.REFBK
             AND CO.GJAHR    = CC.REFGJ

           UNION ALL
          SELECT FI.BUDAT,
                 FI.TIMESTAMP,
                 FI.BELNR AS DOCNR,
                 FI.DOCLN,
                 FI.BELNR,
                 FI.BUZEI,
                 FI.RCLNT,
                 FI.RYEAR,
                 FI.POPER,
                 FI.RBUKRS,
                 FI.RACCT,
                 FI.KOKRS,
                 FI.AWTYP,
                 FI.DRCRK,
                 FI.RTCUR,
                 FI.TSL,
                 FI.HSL,
                 FI.KSL,
                 FI.OSL,
                 CASE WHEN FI.AWTYP = 'VBRK' THEN
                        coalesce((SELECT VR.GEWEI FROM SAPHANADB.VBRP VR WHERE VR.VBELN = CO.AWREF AND VR.POSNR = MG.KDPOS AND VR.SPART <> '08'),FI.RUNIT)
                      ELSE FI.RUNIT
                 END AS RUNIT,
                 CASE WHEN FI.AWTYP = 'VBRK' THEN
                        coalesce((SELECT VR.NTGEW FROM SAPHANADB.VBRP VR WHERE VR.VBELN = CO.AWREF AND VR.POSNR = MG.KDPOS AND VR.SPART <> '08') * CASE WHEN FI.DRCRK = 'S' THEN 1 ELSE -1 END ,FI.MSL)
                      ELSE FI.MSL
                 END AS MSL,
                 CO.AUFNR,
                 TRIM(MG.KMMAKL) AS KMMAKL,
                 TRIM(MG.ARTNR)  AS ARTNR,
                 CASE WHEN TRIM(FI.RCNTR) <> '' THEN  TRIM(FI.RCNTR) ELSE TRIM(CO.KOSTL) END AS RCNTR,
                 CASE WHEN FI.PRCTR = '0000009900' THEN '' ELSE FI.PRCTR END PRCTR,
                 TRIM(FI.RASSC) AS RASSC
            FROM SAPHANADB.ACDOCA    FI LEFT JOIN SAPHANADB.FAGL_SPLINFO CO

                                    ON FI.RCLNT    = CO.MANDT
                     AND FI.BELNR    = CO.BELNR
                     AND FI.RYEAR    = CO.GJAHR
                     AND FI.BUZEI    = CO.BUZEI
                     AND FI.RBUKRS   = CO.BUKRS
                 ,
                 ( SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS
                     FROM SAPHANADB.CE4MGTF_ACCT T1
                    UNION ALL
                   SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS
                     FROM SAPHANADB.CE4MGTF T2 ) MG
           WHERE FI.RCLNT    = '300'
             AND FI.TIMESTAMP BETWEEN :LC_DATA_INIC AND  :LC_DATA_FIMC
             AND FI.RBUKRS   = :P_BUKRS
             AND FI.RLDNR    = '0L'
             AND FI.RYEAR    >= '2015'
             AND FI.AWTYP IN ('VBRK','MKPF','IDOC','MLHD','MKPF','RMRP','BKPF','IDOC','CAJO','RMRP','TRAVL','AMBU','PRCHG','AUAK','AMDP','AIBU','AFRU','COBK','HRPAY')


             AND SUBSTR(CO.PAOBJNR,1,10) <> '0000000000'
             AND CO.MANDT    = MG.MANDT
             AND CO.PAOBJNR  = MG.PAOBJNR
             AND FI.KOKRS    = MG.KOKRS
             AND MG.PASUBNR  = '0001'
             AND MG.AKTBO    = 'X'
           UNION ALL
          SELECT FI.BUDAT,
                 FI.TIMESTAMP,
                 FI.BELNR AS DOCNR,
                 FI.DOCLN,
                 FI.BELNR,
                 FI.BUZEI,
                 FI.RCLNT,
                 FI.RYEAR,
                 FI.POPER,
                 FI.RBUKRS,
                 FI.RACCT,
                 FI.KOKRS,
                 FI.AWTYP,
                 FI.DRCRK,
                 FI.RTCUR,
                 FI.TSL,
                 FI.HSL,
                 FI.KSL,
                 FI.OSL,
                 CASE WHEN FI.AWTYP = 'VBRK' THEN
                        coalesce((SELECT VR.GEWEI FROM SAPHANADB.VBRP VR WHERE VR.VBELN = CO.AWREF AND VR.POSNR = MG.KDPOS AND VR.SPART <> '08'),FI.RUNIT)
                      ELSE FI.RUNIT
                 END AS RUNIT,
                 CASE WHEN FI.AWTYP = 'VBRK' THEN
                        coalesce((SELECT VR.NTGEW FROM SAPHANADB.VBRP VR WHERE VR.VBELN = CO.AWREF AND VR.POSNR = MG.KDPOS AND VR.SPART <> '08') * CASE WHEN FI.DRCRK = 'S' THEN 1 ELSE -1 END ,FI.MSL)
                      ELSE FI.MSL
                 END AS MSL,
                 CO.AUFNR,
                 TRIM(MG.KMMAKL) AS KMMAKL,
                 TRIM(MG.ARTNR)  AS ARTNR,
                 CASE WHEN TRIM(FI.RCNTR) <> '' THEN  TRIM(FI.RCNTR) ELSE TRIM(CO.KOSTL) END AS RCNTR,
                 CASE WHEN FI.PRCTR = '0000009900' THEN '' ELSE FI.PRCTR END PRCTR,
                 TRIM(FI.RASSC) AS RASSC
            FROM SAPHANADB.ACDOCA    FI LEFT JOIN SAPHANADB.FAGL_SPLINFO CO
                                ON FI.RCLNT    = CO.MANDT
                   AND FI.BELNR    = CO.BELNR
                   AND FI.RYEAR    = CO.GJAHR
                   AND FI.BUZEI    = CO.BUZEI
                   AND FI.RBUKRS   = CO.BUKRS
                 ,
                 ( SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS
                     FROM SAPHANADB.CE4MGBG_ACCT T1
                    UNION ALL
                   SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS
                     FROM SAPHANADB.CE4MGBG T2 ) MG
           WHERE FI.RCLNT    = '300'
             AND FI.TIMESTAMP BETWEEN :LC_DATA_INIC AND  :LC_DATA_FIMC
             AND FI.RBUKRS   = :P_BUKRS
             AND FI.RLDNR    = '0L'
             AND FI.RYEAR    >= '2015'
             AND FI.AWTYP IN ('VBRK','MKPF','IDOC','MLHD','MKPF','RMRP','BKPF','IDOC','CAJO','RMRP','TRAVL','AMBU','PRCHG','AUAK','AMDP','AIBU','AFRU','COBK','HRPAY')


             AND SUBSTR(CO.PAOBJNR,1,10) <> '0000000000'
             AND CO.MANDT    = MG.MANDT
             AND CO.PAOBJNR  = MG.PAOBJNR
             AND FI.KOKRS    = MG.KOKRS
             AND MG.PASUBNR  = '0001'
             AND MG.AKTBO    = 'X'
              ) TT LEFT JOIN
                 (SELECT CO.KOKRS, CO.KOSTL, CO.KOSAR
                    FROM SAPHANADB.CSKS CO
                   WHERE CURRENT_DATE BETWEEN CO.DATAB AND CO.DATBI
                     AND CO.MANDT = '300' ) CO
           ON TRIM(TT.RCNTR) = CO.KOSTL
                     AND TT.KOKRS       = CO.KOKRS
           ,
                 SAPHANADB.T001 EM
           WHERE TT.RBUKRS      = EM.BUKRS
             AND EM.MANDT       = '300'
             AND NOT EXISTS ( SELECT * FROM SAPHANADB.SKA1 K WHERE K.MANDT = EM.MANDT AND K.KTOPL = EM.KTOPL AND K.SAKNR = TT.RACCT AND K.KTOKS IN ('YB01','YB02','YB03','YB04' ) )
             AND NOT EXISTS ( SELECT * FROM SAPHANADB.ZGLT_DRE_04 FF
                               WHERE FF.MANDT = '300'
                                 AND FF.BUKRS = TT.RBUKRS
                                 AND FF.GJAHR = TT.RYEAR
                                 AND FF.DOCNR = TT.DOCNR
                                 AND FF.DOCLN = TT.DOCLN ) )
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  lc_linhas = 1.

  DO.
    EXEC SQL.
      FETCH NEXT DOCUMENTOS_2 INTO
      :WA_LANC-BUDAT,
      :WA_LANC-TIMESTAMP,
      :WA_LANC-DOCNR,
      :WA_LANC-DOCLN,
      :WA_LANC-BELNR,
      :WA_LANC-BUZEI,
      :WA_LANC-MANDT,
      :WA_LANC-GJAHR,
      :WA_LANC-POPER,
      :WA_LANC-BUKRS,
      :WA_LANC-SAKNR,
      :WA_LANC-KOKRS,
      :WA_LANC-AWTYP,
      :WA_LANC-SHKZG,
      :WA_LANC-RTCUR,
      :WA_LANC-VLTSL,
      :WA_LANC-VLHSL,
      :WA_LANC-VLKSL,
      :WA_LANC-VLOSL,
      :WA_LANC-RUNIT,
      :WA_LANC-QTMSL,
      :WA_LANC-AUFNR,
      :WA_LANC-MATKL,
      :WA_LANC-MATNR,
      :WA_LANC-KOSTL,
      :WA_LANC-PRCTR,
      :WA_LANC-VBUND,
      :WA_LANC-KTOPL,
      :WA_LANC-KOSAR
    ENDEXEC.
    IF sy-subrc <> 0.
      EXIT.
    ELSE.
      IF ( ( wa_lanc-gjahr GE 2015 ) AND ( lc_linhas LE lva_limite_reg ) ) OR ( pcarga IS NOT INITIAL ).
        wa_lanc-ck_integrado = 'N'.

        IF wa_lanc-runit IS NOT INITIAL.

          CASE wa_lanc-runit.
            WHEN 'UN'. "Unidade
              wa_lanc-runit_base = wa_lanc-runit.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = wa_lanc-matkl
                IMPORTING
                  output = lc_matkl.

              CASE lc_matkl.
                WHEN '750110'. "BOVINOS
                  wa_lanc-qtmsl_base = wa_lanc-qtmsl.
                WHEN '750120'. "EQUINOS E MUARES
                  wa_lanc-qtmsl_base = wa_lanc-qtmsl.
                WHEN '750130'. "SUINOS
                  wa_lanc-qtmsl_base = wa_lanc-qtmsl.
                WHEN '750140'. "AVES
                  wa_lanc-qtmsl_base = wa_lanc-qtmsl.
                WHEN '750150'. "CAPRINOS
                  wa_lanc-qtmsl_base = wa_lanc-qtmsl.
                WHEN '750160'. "OUTROS ANIMAIS
                  wa_lanc-qtmsl_base = wa_lanc-qtmsl.
                WHEN OTHERS.
                  wa_lanc-qtmsl_base = 0.
              ENDCASE.

            WHEN 'KG' OR 'L' OR 'MWH'. "Quilograma/Litro/Megawatt hora/Microampère
              wa_lanc-runit_base = wa_lanc-runit.
              wa_lanc-qtmsl_base = wa_lanc-qtmsl.
            WHEN 'G' OR 'TON' OR 'TO' OR 'BAG' OR 'SC'. "Outras Unidades de Medida -- Grama/Toneladas EUA/Tonelada/Saco/Saca

              wa_lanc-runit_base = 'KG'. "Quilograma

              MOVE wa_lanc-qtmsl TO i_menge.

              CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
                EXPORTING
                  i_matnr              = wa_lanc-matnr
                  i_in_me              = wa_lanc-runit
                  i_out_me             = 'KG'
                  i_menge              = i_menge
                IMPORTING
                  e_menge              = e_menge
                EXCEPTIONS
                  error_in_application = 1
                  error                = 2
                  OTHERS               = 3.

              IF sy-subrc IS NOT INITIAL.
                CLEAR: wa_lanc-runit_base, wa_lanc-qtmsl_base.
              ELSE.
                MOVE e_menge TO wa_lanc-qtmsl_base.
              ENDIF.

            WHEN OTHERS.
              wa_lanc-runit_base = wa_lanc-runit.
              wa_lanc-qtmsl_base = wa_lanc-qtmsl.
          ENDCASE.

        ELSE.
          CLEAR: wa_lanc-runit_base.
          wa_lanc-qtmsl_base = 0.
        ENDIF.

        APPEND wa_lanc TO it_lanc.
        lc_linhas = lc_linhas + 1.
      ENDIF.
    ENDIF.
  ENDDO.

  EXEC SQL.
    CLOSE DOCUMENTOS_2
  ENDEXEC.

  lc_linhas = 0.

  DELETE it_lanc WHERE gjahr LT 2015.

  IF pcarga IS INITIAL.

    DESCRIBE TABLE it_lanc LINES lc_linhas.

    IF lc_linhas GE lva_limite_reg.
      parcial = abap_true.
    ELSE.
      parcial = abap_false.
    ENDIF.

  ELSE.
    parcial = abap_false.
  ENDIF.

ENDFUNCTION.
