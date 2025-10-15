FUNCTION ZLES_MONITORA_003.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------

  DATA:
    LC_SAFRA TYPE ZSDT_RETLOTE-SAFRA,
    LC_WERKS TYPE ZSDT_RETLOTE-WERKS.

  TYPES: BEGIN OF TY_REGISTRO.
  TYPES: LC_SAFRA TYPE ZSDT_RETLOTE-SAFRA,
         LC_WERKS TYPE ZSDT_RETLOTE-WERKS.
  TYPES: END OF TY_REGISTRO.

  DATA: IT_REGISTRO TYPE TABLE OF TY_REGISTRO,
        WA_REGISTRO TYPE TY_REGISTRO.

  TRY.
      EXEC SQL.
        OPEN FILIAIS FOR
          SELECT UNIQUE SAFRA, WERKS
            FROM SAPSR3.ZSDT_RETLOTE
           WHERE SAFRA <> ' '
             AND WERKS <> ' '
             AND SAFRA >= '2018'
           ORDER BY SAFRA, WERKS
      ENDEXEC.

      DO.
        EXEC SQL.
          FETCH NEXT FILIAIS INTO
            :WA_REGISTRO-LC_SAFRA,
            :WA_REGISTRO-LC_WERKS
        ENDEXEC.
        IF SY-SUBRC <> 0.
          EXIT.
        ELSE.
          APPEND WA_REGISTRO TO IT_REGISTRO.
        ENDIF.
      ENDDO.

      EXEC SQL.
        CLOSE FILIAIS
      ENDEXEC.

    CATCH CX_SY_NATIVE_SQL_ERROR.
  ENDTRY.

  LOOP AT IT_REGISTRO INTO WA_REGISTRO
    WHERE LC_SAFRA IS NOT INITIAL AND LC_WERKS IS NOT INITIAL.

    LC_SAFRA = WA_REGISTRO-LC_SAFRA.
    LC_WERKS = WA_REGISTRO-LC_WERKS.

    CONCATENATE 'Processando Safra' WA_REGISTRO-LC_SAFRA 'Filial' WA_REGISTRO-LC_WERKS INTO DATA(LC_TEXTO) SEPARATED BY SPACE.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = 0
        TEXT       = LC_TEXTO.

    EXEC SQL.
      INSERT INTO SAPSR3.ZLEST0172 ( MANDT, ID_NOMEACAO_TRAN, DOCNUM, CHARG, BRANCH, CREDAT, DOCDAT, NFENUM, MATNR, MENGE, MEINS, NETPR, NETWRT, TERMINAL, LGORT, CFOP, REFTYP )
      WITH CARTAS AS ( SELECT * FROM SAPSR3.ZCARTA_CORRECAO CC )
      SELECT TT.MANDT,
             TT.ID_NOMEACAO_TRAN,
             TT.DOCNUM,
             TT.CHARG,
             TT.BRANCH,
             TT.CREDAT,
             TT.DOCDAT,
             TT.NFENUM,
             TT.MATNR,
             SUM(TT.MENGE) AS MENGE,
             TT.MEINS,
             TT.NETPR,
             SUM(TT.NETWRT) AS NETWRT,
             TT.TERMINAL,
             TT.LGORT,
             TT.CFOP,
             TT.REFTYP
        FROM (
        SELECT :SY-MANDT AS MANDT,
               NVL((SELECT DISTINCT EE.ID_NOMEACAO_TRAN
                  FROM SAPSR3.ZSDT_EXPORT    TR,
                       SAPSR3.ZNOM_REMETENTE EE
                 WHERE TR.MANDT  = L.MANDT
                   AND TR.DOCNUM = L.DOCNUM_RET
                   AND TR.MANDT  = EE.MANDT
                   AND TR.ORDEM  = EE.NR_ORDEM
                   AND TR.DOCNUM = EE.DOCNUM_RT
                   AND TR.ORDEM <> ' '  ), (SELECT MIN(E.ID_NOMEACAO_TRAN) FROM SAPSR3.ZNOM_TRANSPORTE E WHERE E.NR_ANO = L.SAFRA ) ) AS ID_NOMEACAO_TRAN,
               L.DOCNUM AS DOCNUM,
               L.SAFRA  AS CHARG,
               L.WERKS  AS BRANCH,
               D.CREDAT AS CREDAT,
               D.DOCDAT AS DOCDAT,
               D.NFENUM AS NFENUM,
               L.MATNR  AS MATNR,
               L.QUANT_VINC AS MENGE,
               'KG'         AS MEINS,
               L.UNITARIO   AS NETPR,
               L.VLR_TOTAL  AS NETWRT,
               NVL(
               DECODE(TRIM(C.NOVO_TERMINAL),NULL,
               CASE WHEN I.REFTYP = 'ZW' THEN
                      ( SELECT P.PARID FROM SAPSR3.ZFIWRT0008 Z, SAPSR3.ZFIWRT0015 P WHERE Z.MANDT = L.MANDT AND Z.DOCNUM = L.DOCNUM AND Z.MANDT = P.MANDT AND Z.SEQ_LCTO = P.SEQ_LCTO AND P.PARVW = 'Z1' )
                    WHEN I.REFTYP = 'BI' THEN
                      NVL(( SELECT N.PARID FROM SAPSR3.J_1BNFNAD N WHERE N.MANDT = L.MANDT AND N.DOCNUM = L.DOCNUM AND N.PARVW = 'Z1' ),
                          ( SELECT PA.LIFNR FROM SAPSR3.VBRP FT, SAPSR3.VBPA PA WHERE FT.MANDT = I.MANDT AND FT.VBELN = I.REFKEY AND FT.POSNR = I.REFITM AND FT.MANDT = PA.MANDT AND FT.VGBEL = PA.VBELN AND PA.PARVW = 'Z1')
                         )
               END, C.NOVO_TERMINAL),
               ' ') AS TERMINAL,
               CASE WHEN L.LGORT NOT IN ('*',' ') THEN L.LGORT
                    ELSE DECODE(TRIM(DD.LGORT_D),NULL,
                         CASE WHEN I.REFTYP = 'ZW' THEN
                                ( SELECT P.LGORT FROM SAPSR3.ZFIWRT0008 Z, SAPSR3.ZFIWRT0009 P WHERE Z.MANDT = L.MANDT AND Z.DOCNUM = L.DOCNUM AND Z.MANDT = P.MANDT AND Z.SEQ_LCTO = P.SEQ_LCTO )
                              WHEN I.REFTYP = 'BI' THEN
                                ( SELECT PA.LGORT FROM SAPSR3.VBRP FT, SAPSR3.LIPS PA WHERE FT.MANDT = I.MANDT AND FT.VBELN = I.REFKEY AND FT.POSNR = I.REFITM AND FT.MANDT = PA.MANDT AND FT.VGBEL = PA.VBELN )
                         END,DD.LGORT_D)
               END AS LGORT,
               I.CFOP       AS CFOP,
               I.REFTYP     AS REFTYP
          from SAPSR3.ZSDT_RETLOTE L,
               SAPSR3.J_1BNFLIN    I,
               SAPSR3.J_1BNFDOC    D,
               ( SELECT DOCNUM, NOVO_TERMINAL
                   FROM CARTAS CC
                  WHERE CC.MANDT = :SY-MANDT
                    AND CC.NOVO_TERMINAL <> ' '
                    AND CC.AUTHCODE      <> ' '
                    AND CC.ID_CC = ( SELECT MAX(ID_CC)
                                       FROM CARTAS CCM
                                      WHERE CCM.MANDT         = :SY-MANDT
                                        AND CCM.DOCNUM        = CC.DOCNUM
                                        AND CCM.NOVO_TERMINAL <> ' '
                                        AND CCM.AUTHCODE      <> ' ' ) ) C,
               ( SELECT DOCNUM, CC.LGORT_D
                   FROM CARTAS CC
                  WHERE CC.MANDT = :SY-MANDT
                    AND CC.LGORT_D       <> ' '
                    AND CC.AUTHCODE      <> ' '
                    AND CC.ID_CC = ( SELECT MAX(ID_CC)
                                       FROM CARTAS CCM
                                      WHERE CCM.MANDT         = :SY-MANDT
                                        AND CCM.DOCNUM        = CC.DOCNUM
                                        AND CCM.LGORT_D       <> ' '
                                        AND CCM.AUTHCODE      <> ' ' ) ) DD
         WHERE 1 = 1
           AND L.MANDT  = I.MANDT
           AND L.DOCNUM = I.DOCNUM
           AND L.SAFRA  = :LC_SAFRA
           AND L.WERKS  = :LC_WERKS
           AND L.MANDT  = D.MANDT
           AND L.DOCNUM = D.DOCNUM
           AND L.DOCNUM = C.DOCNUM(+)
           AND L.DOCNUM = DD.DOCNUM(+)
            ) TT
        WHERE NOT EXISTS ( SELECT * FROM SAPSR3.ZLEST0172 FF
                            WHERE FF.MANDT            = TT.MANDT
                              AND FF.ID_NOMEACAO_TRAN = TT.ID_NOMEACAO_TRAN
                              AND FF.DOCNUM           = TT.DOCNUM )
       GROUP BY MANDT, ID_NOMEACAO_TRAN, DOCNUM, CHARG, BRANCH, CREDAT, DOCDAT, NFENUM, MATNR, MEINS, NETPR, TERMINAL, LGORT, CFOP, REFTYP
    ENDEXEC.

    COMMIT WORK.

  ENDLOOP.

ENDFUNCTION.
