FUNCTION z_01_dre_consulta_regs_2.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LC_DATA_INI) TYPE  TIMESTAMP
*"     REFERENCE(LC_DATA_FIM) TYPE  TIMESTAMP
*"     REFERENCE(PCARGA) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_BUKRS) TYPE  BUKRS OPTIONAL
*"  EXPORTING
*"     REFERENCE(PARCIAL) TYPE  CHAR01
*"  TABLES
*"      IT_LANC STRUCTURE  ZGLT_DRE_04
*"  EXCEPTIONS
*"      ERRO_SQL
*"----------------------------------------------------------------------

  DATA: EXC_REF    TYPE REF TO CX_SY_NATIVE_SQL_ERROR,
        ERROR_TEXT TYPE STRING,
        WA_LANC    TYPE ZGLT_DRE_04,
        LC_LINHAS  TYPE I,
        LC_MATKL   TYPE MATKL.

  DATA: LC_DATA_FIMC TYPE  TIMESTAMP,
        LC_DATA_INIC TYPE  TIMESTAMP,
        I_MENGE      LIKE  EKPO-MENGE,
        E_MENGE      LIKE  EKPO-MENGE.

  DATA: lva_limite_reg TYPE i.

  lva_limite_reg = 10000.

  SELECT SINGLE *
    FROM SETLEAF INTO @DATA(LWA_SETLEAF_LIMITE_REG)
   WHERE SETNAME EQ 'DRE_LIMITE_REG'.

  IF SY-SUBRC EQ 0 AND LWA_SETLEAF_LIMITE_REG-valfrom IS NOT INITIAL.
    lva_limite_reg = LWA_SETLEAF_LIMITE_REG-valfrom.
  ENDIF.

  IF P_BUKRS IS NOT INITIAL.

    CALL FUNCTION 'Z_01_DRE_CONSULTA_REGS_2_BUKRS'
      EXPORTING
        LC_DATA_INI = LC_DATA_INI
        LC_DATA_FIM = LC_DATA_FIM
        PCARGA      = PCARGA
        P_BUKRS     = P_BUKRS
      IMPORTING
        PARCIAL     = PARCIAL
      TABLES
        IT_LANC     = IT_LANC
      EXCEPTIONS
        ERRO_SQL    = 1
        OTHERS      = 2.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO ERROR_TEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      MESSAGE ERROR_TEXT TYPE 'E' RAISING ERRO_SQL.
    ENDIF.

  ELSE.

    LC_DATA_INIC = LC_DATA_INI.
    LC_DATA_FIMC = LC_DATA_FIM.
    LC_DATA_INIC = LC_DATA_INI - 770000.
    LC_DATA_FIMC = LC_DATA_FIM + 40001.
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
                                 (SELECT max( TRIM(J1.MATKL) )
                                    FROM SAPHANADB.J_1BNFLIN J1
                                   WHERE J1.REFTYP = 'ZW'
                                     AND SUBSTR(J1.REFKEY,1,10) = SUBSTR(FI.AWREF||FI.AWORG,4,10)
                                      )
                               WHEN TRIM(FI.RUNIT) = '' AND SUBSTR(FI.AWREF,1,2) = 'ZG' AND SUBSTR(FI.AWREF,1,3) = 'ZGF' THEN
                                 (SELECT max( TRIM(J1.MATKL) )
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
                           '001' as RVERS,
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
                      FROM SAPHANADB.ACDOCA    FI left join (SELECT * FROM SAPHANADB.FAGL_SPLINFO CO WHERE TRIM(CO.AWREF) <> '' ) CO
                                           on    FI.RCLNT    = CO.MANDT
                             AND FI.BELNR    = CO.BELNR
                             AND FI.RYEAR    = CO.GJAHR
                             AND FI.RBUKRS   = CO.BUKRS
                           left join SAPHANADB.BKPF         BK
                           on  FI.RCLNT    = BK.MANDT
                           AND FI.RBUKRS   = BK.BUKRS
                           AND FI.BELNR    = BK.BELNR






             ) FI left join ( SELECT * FROM SAPHANADB.COVP CC WHERE CC.SCOPE = 'PA' ) CC

                                  on FI.RCLNT    = CC.MANDT
                       AND FI.AWREF    = CC.REFBN
                       AND FI.AWTYP    = CC.AWTYP
                       AND FI.AWORG    = CC.AWORG
                       AND FI.BUKRS    = CC.REFBK
                       AND FI.GJAHR    = CC.REFGJ
                       AND FI.KOKRS    = CC.KOKRS
                       AND FI.BUZEI    = CC.REFBZ_FI


              left join    ( SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MAGI_ACCT T1 WHERE MANDT = '300' AND AKTBO   = 'X' AND PASUBNR = '0001'
                      UNION ALL
                     SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MAGI T2 WHERE MANDT = '300' AND AKTBO   = 'X' AND PASUBNR = '0001' ) MGB


                     on CC.PAOBJNR  = MGB.PAOBJNR
                                        AND CC.KOKRS    = MGB.KOKRS

              left join   (SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGLD_ACCT T1 WHERE MANDT = '300' AND AKTBO   = 'X' AND PASUBNR = '0001'
                     UNION ALL
                    SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGLD T2 WHERE MANDT = '300' AND AKTBO   = 'X' AND PASUBNR = '0001' ) MGH


                    on CC.PAOBJNR  = MGH.PAOBJNR
                                       AND CC.KOKRS    = MGH.KOKRS

              left join   (SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGAR_ACCT T1 WHERE MANDT = '300' AND AKTBO   = 'X' AND PASUBNR = '0001'
                     UNION ALL
                    SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGAR T2 WHERE MANDT = '300' AND AKTBO   = 'X' AND PASUBNR = '0001' ) MGA


                    on CC.PAOBJNR  = MGA.PAOBJNR
                                       AND CC.KOKRS    = MGA.KOKRS

              left join   (SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGBG_ACCT T1 WHERE MANDT = '300' AND AKTBO   = 'X' AND PASUBNR = '0001'
                     UNION ALL
                    SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGBG T2 WHERE MANDT = '300' AND AKTBO   = 'X' AND PASUBNR = '0001' ) MGG


                    on CC.PAOBJNR  = MGG.PAOBJNR
                                       AND CC.KOKRS    = MGG.KOKRS

              left join   (SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGTF_ACCT T1 WHERE MANDT = '300' AND AKTBO   = 'X' AND PASUBNR = '0001'
                     UNION ALL
                    SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS FROM SAPHANADB.CE4MGTF T2 WHERE MANDT = '300' AND AKTBO   = 'X' AND PASUBNR = '0001' ) MGT

                    on CC.PAOBJNR  = MGT.PAOBJNR
                                       AND CC.KOKRS    = MGT.KOKRS






             WHERE FI.RCLNT   = '300'
               AND FI.TIMESTAMP BETWEEN :LC_DATA_INIC AND  :LC_DATA_FIMC
               AND FI.RBUKRS   IN ('0041','0039','0101')
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
              FROM SAPHANADB.ACDOCA    FI left join SAPHANADB.FAGL_SPLINFO CO
                          on FI.RCLNT   = CO.MANDT
                 AND FI.BELNR   = CO.BELNR
                 AND FI.RYEAR   = CO.GJAHR
                 AND FI.RBUKRS  = CO.BUKRS
                 AND FI.BUZEI   = CO.BUZEI

             WHERE FI.RCLNT   = '300'
               AND FI.TIMESTAMP BETWEEN :LC_DATA_INIC AND  :LC_DATA_FIMC
               AND FI.RBUKRS   IN ('0041','0039','0101')
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
               AND FI.RBUKRS   IN ('0041')
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
               AND FI.RBUKRS   IN ('0039')
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
              FROM SAPHANADB.ACDOCA    FI left join SAPHANADB.FAGL_SPLINFO CO on
                                     FI.RCLNT    = CO.MANDT
                     AND FI.BELNR    = CO.BELNR
                     AND FI.RYEAR    = CO.GJAHR
                     AND FI.RBUKRS   = CO.BUKRS
                     AND FI.BUZEI    = CO.BUZEI ,

                   ( SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS
                       FROM SAPHANADB.CE4MGTF_ACCT T1
                      UNION ALL
                     SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS
                       FROM SAPHANADB.CE4MGTF T2 ) MG
             WHERE FI.RCLNT    = '300'
               AND FI.TIMESTAMP BETWEEN :LC_DATA_INIC AND  :LC_DATA_FIMC
               AND FI.RBUKRS   IN ('0041')
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
              FROM SAPHANADB.ACDOCA    FI left join SAPHANADB.FAGL_SPLINFO CO

                                  on FI.RCLNT    = CO.MANDT
                     AND FI.BELNR    = CO.BELNR
                     AND FI.RYEAR    = CO.GJAHR
                     AND FI.RBUKRS   = CO.BUKRS
                     AND FI.BUZEI    = CO.BUZEI ,

                   ( SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS
                       FROM SAPHANADB.CE4MGBG_ACCT T1
                      UNION ALL
                     SELECT MANDT, AKTBO, PAOBJNR, PASUBNR, KNDNR, ARTNR, AUFNR, BUKRS, KOKRS, PRCTR, PPRCTR, MEINH, KMMAKL, AUART, KDPOS
                       FROM SAPHANADB.CE4MGBG T2 ) MG
             WHERE FI.RCLNT    = '300'
               AND FI.TIMESTAMP BETWEEN :LC_DATA_INIC AND  :LC_DATA_FIMC
               AND FI.RBUKRS   IN ('0039')
               AND FI.RLDNR    = '0L'
               AND FI.RYEAR    >= '2015'
               AND FI.AWTYP IN ('VBRK','MKPF','IDOC','MLHD','MKPF','RMRP','BKPF','IDOC','CAJO','RMRP','TRAVL','AMBU','PRCHG','AUAK','AMDP','AIBU','AFRU','COBK','HRPAY')




               AND SUBSTR(CO.PAOBJNR,1,10) <> '0000000000'
               AND CO.MANDT    = MG.MANDT
               AND CO.PAOBJNR  = MG.PAOBJNR
               AND FI.KOKRS    = MG.KOKRS
               AND MG.PASUBNR  = '0001'
               AND MG.AKTBO    = 'X'
                ) TT left join (SELECT CO.KOKRS, CO.KOSTL, CO.KOSAR
                      FROM SAPHANADB.CSKS CO
                     WHERE current_date BETWEEN CO.DATAB AND CO.DATBI
                       AND CO.MANDT = '300' ) CO
             on TRIM(TT.RCNTR) = CO.KOSTL
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
      CATCH CX_SY_NATIVE_SQL_ERROR INTO EXC_REF.
        ERROR_TEXT = EXC_REF->GET_TEXT( ).
        MESSAGE ERROR_TEXT TYPE 'E' RAISING ERRO_SQL.
    ENDTRY.

    LC_LINHAS = 1.

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
      IF SY-SUBRC <> 0.
        EXIT.
      ELSE.
        IF ( ( WA_LANC-GJAHR GE 2015 ) AND ( LC_LINHAS LE lva_limite_reg ) ) OR ( PCARGA IS NOT INITIAL ).
          WA_LANC-CK_INTEGRADO = 'N'.

          IF WA_LANC-RUNIT IS NOT INITIAL.

            CASE WA_LANC-RUNIT.
              WHEN 'UN'. "Unidade
                WA_LANC-RUNIT_BASE = WA_LANC-RUNIT.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    INPUT  = WA_LANC-MATKL
                  IMPORTING
                    OUTPUT = LC_MATKL.

                CASE LC_MATKL.
                  WHEN '750110'. "BOVINOS
                    WA_LANC-QTMSL_BASE = WA_LANC-QTMSL.
                  WHEN '750120'. "EQUINOS E MUARES
                    WA_LANC-QTMSL_BASE = WA_LANC-QTMSL.
                  WHEN '750130'. "SUINOS
                    WA_LANC-QTMSL_BASE = WA_LANC-QTMSL.
                  WHEN '750140'. "AVES
                    WA_LANC-QTMSL_BASE = WA_LANC-QTMSL.
                  WHEN '750150'. "CAPRINOS
                    WA_LANC-QTMSL_BASE = WA_LANC-QTMSL.
                  WHEN '750160'. "OUTROS ANIMAIS
                    WA_LANC-QTMSL_BASE = WA_LANC-QTMSL.
                  WHEN OTHERS.
                    WA_LANC-QTMSL_BASE = 0.
                ENDCASE.

              WHEN 'KG' OR 'L' OR 'MWH'. "Quilograma/Litro/Megawatt hora/Microampère
                WA_LANC-RUNIT_BASE = WA_LANC-RUNIT.
                WA_LANC-QTMSL_BASE = WA_LANC-QTMSL.
              WHEN 'G' OR 'TON' OR 'TO' OR 'BAG' OR 'SC'. "Outras Unidades de Medida -- Grama/Toneladas EUA/Tonelada/Saco/Saca

                WA_LANC-RUNIT_BASE = 'KG'. "Quilograma

                MOVE WA_LANC-QTMSL TO I_MENGE.

                CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
                  EXPORTING
                    I_MATNR              = WA_LANC-MATNR
                    I_IN_ME              = WA_LANC-RUNIT
                    I_OUT_ME             = 'KG'
                    I_MENGE              = I_MENGE
                  IMPORTING
                    E_MENGE              = E_MENGE
                  EXCEPTIONS
                    ERROR_IN_APPLICATION = 1
                    ERROR                = 2
                    OTHERS               = 3.

                IF SY-SUBRC IS NOT INITIAL.
                  CLEAR: WA_LANC-RUNIT_BASE, WA_LANC-QTMSL_BASE.
                ELSE.
                  MOVE E_MENGE TO WA_LANC-QTMSL_BASE.
                ENDIF.

              WHEN OTHERS.
                WA_LANC-RUNIT_BASE = WA_LANC-RUNIT.
                WA_LANC-QTMSL_BASE = WA_LANC-QTMSL.
            ENDCASE.

          ELSE.
            CLEAR: WA_LANC-RUNIT_BASE.
            WA_LANC-QTMSL_BASE = 0.
          ENDIF.

          APPEND WA_LANC TO IT_LANC.
          LC_LINHAS = LC_LINHAS + 1.
        ENDIF.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE DOCUMENTOS_2
    ENDEXEC.

    LC_LINHAS = 0.

    DELETE IT_LANC WHERE GJAHR LT 2015.

    IF PCARGA IS INITIAL.

      DESCRIBE TABLE IT_LANC LINES LC_LINHAS.

      IF LC_LINHAS GE lva_limite_reg.
        PARCIAL = ABAP_TRUE.
      ELSE.
        PARCIAL = ABAP_FALSE.
      ENDIF.

    ELSE.
      PARCIAL = ABAP_FALSE.
    ENDIF.
  ENDIF.

ENDFUNCTION.
