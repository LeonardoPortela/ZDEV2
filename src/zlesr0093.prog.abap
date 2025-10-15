*&---------------------------------------------------------------------*
*& Report  ZLESR0093
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0093.

TYPES: ty_rg_vbeln TYPE RANGE OF vttp-vbeln.

DATA: exc_ref      TYPE REF TO cx_sy_native_sql_error,
      error_text   TYPE string,
      lc_erdat_aux TYPE vbfa-erdat,
      lc_erdat     TYPE c LENGTH 9,
      lc_erdat_fim TYPE c LENGTH 9,
      wa_linha     TYPE zde_zlest0098_sql,
      lc_carga     TYPE char01,
      it_linha     TYPE TABLE OF zde_zlest0098_sql WITH HEADER LINE,
      it_zlest0112 TYPE TABLE OF zlest0112 WITH HEADER LINE,
      it_sort_0112 TYPE SORTED TABLE OF zlest0112 WITH NON-UNIQUE DEFAULT KEY, "WITH HEADER LINE,
      it_vttp      TYPE TABLE OF vttp WITH HEADER LINE,
      it_sort_vttp TYPE SORTED TABLE OF vttp WITH NON-UNIQUE KEY vbeln, "WITH HEADER LINE,
      it_vttk      TYPE TABLE OF vttk WITH HEADER LINE,
      it_vfkp      TYPE TABLE OF vfkp WITH HEADER LINE,
      it_sort_vfkp TYPE SORTED TABLE OF vfkp WITH NON-UNIQUE KEY rebel, "WITH HEADER LINE,
      it_konv      TYPE TABLE OF konv WITH HEADER LINE,
      it_sort_konv TYPE SORTED TABLE OF konv WITH UNIQUE DEFAULT KEY. " HEADER LINE.

FIELD-SYMBOLS: <fs_0112> TYPE zlest0112.

DATA: lit_likp TYPE TABLE OF  likp.

"Para Execução em backgound (jobs) """"""""""""""""""""""""""""
IF sy-batch EQ abap_true.
  TRY .
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
    CATCH zcx_job.
      e_qtd = 1.
  ENDTRY.

  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.
ENDIF.

lc_carga = abap_false.

IF lc_carga EQ abap_false.
  lc_erdat_aux = sy-datum - 1. "Alterar para pegar apenas 2 dias ISUE 189281 - BG
  MOVE lc_erdat_aux TO lc_erdat.
  lc_erdat_aux = sy-datum.
  MOVE lc_erdat_aux TO lc_erdat_fim.
ENDIF.


TRY. "174296 IR234471  - job SPL_ZLESR0093  sendo cancelado PSA
    EXEC SQL.
      OPEN DOCUMENTOS FOR
             SELECT TT.ERDAT,
               TT.VBELN,
               TT.RFMNG,
               TT.VBELV,
               TT.MATNR,
               LPAD(TT.MATKL,9,'0') AS MATKL,
               TT.WERKS,
               TT.CHARG,
               TT.NR_ROMANEIO,
               TT.TP_TRANSGENIA,
               TT.COD_COLETA,
               TT.COD_TRANSBORDO,
               TT.COD_PORTO,
               TT.CENTRO_VIRTUAL,
               (SELECT DESCR_CENTRO FROM SAPHANADB.ZLEST0097 FF WHERE FF.CENTRO_VIRTUAL = SUBSTR(TT.CENTRO_VIRTUAL,1,2) ) AS NOME_DESTINO,
               TT.AUART,
               TT.COD_CLIENTE,
               TT.INCO1,
               CASE WHEN TT.VALOR1 <> '' THEN SUBSTR(TT.VALOR1,1,3)
                    WHEN TT.VALOR2 <> '' THEN SUBSTR(TT.VALOR2,1,3)
                    WHEN TT.VALOR3 <> '' THEN SUBSTR(TT.VALOR3,1,3)
                    ELSE null
               END AS MOEDA_AQUA,
               cast(SUBSTR(TT.VALOR1,24,20) as decimal(15,5)) AS TAXAS_AQUA,
               TO_NUMBER(CASE WHEN TT.VALOR1 <> '' THEN SUBSTR(TT.VALOR1,4,20)
                    WHEN TT.VALOR2 <> '' THEN SUBSTR(TT.VALOR2,4,20)
                    WHEN TT.VALOR3 <> '' THEN SUBSTR(TT.VALOR3,4,20)
                    ELSE null
               END) AS VALOR_AQUA,
               UKURS,
               COALESCE(VALOR_FERRO,0)           AS VALOR_FERRO,
               COALESCE(VALOR_FERRO_GERENCIAL,0) AS VALOR_FERRO_GERENCIAL,
               COALESCE(VALOR_AQUAVIARIO,0)      AS VALOR_AQUAVIARIO,
               VALOR_FERRO_GERENCIAL        AS MOEDA_FERRO_GERENCIAL,
               VALOR_AQUAVIARIO             AS MOEDA_AQUAVIARIO
          FROM (
         SELECT VF.ERDAT,
                VF.VBELN,
                LP.LFIMG AS RFMNG,
                VF.VBELV,
                LP.MATNR,
                LP.MATKL,
                LP.WERKS,
                LP.CHARG,
                RO.NR_ROMANEIO,
                CASE WHEN BK.AUART NOT IN ('ZRDC','ZRFL','ZIND') THEN
                       CASE WHEN RO.TP_TRANSGENIA IN ('CO','C ') THEN 'NAO' ELSE 'SIM' END
                     ELSE
                       CASE WHEN BK.KVGR3 = 'C' THEN 'NAO'
                            WHEN BK.KVGR3 = 'R' THEN 'SIM'
                            ELSE CASE WHEN RO.TP_TRANSGENIA IN ('CO','C ') THEN 'NAO' ELSE 'SIM' END
                       END
                END TP_TRANSGENIA,
                PC.LIFNR AS COD_COLETA,

                CASE WHEN BK.AUART IN ('ZRDC','ZRFL','ZIND') THEN LR.KUNNR ELSE '' END AS COD_TRANSBORDO,
                CASE WHEN BK.AUART IN ('ZRDC','ZRFL','ZIND') THEN Z1.LIFNR ELSE '' END AS COD_PORTO,
                (SELECT DISTINCT DT.WERKS_V FROM SAPHANADB.ZSDT_DEPARA_DEPO DT WHERE DT.MANDT = '300'  AND DT.WERKS = LP.WERKS AND DT.LIFNR = Z1.LIFNR AND DT.OPERACAO =
                     CASE WHEN BK.AUART = 'ZRFL' THEN 'RF'
                          WHEN BK.AUART = 'ZIND' THEN 'RI'
                          ELSE DT.OPERACAO
                     END ) AS CENTRO_VIRTUAL,
                BK.AUART,
                BK.KUNNR AS COD_CLIENTE,

                CASE WHEN BK.AUART IN ('ZRDC','ZRFL','ZIND') THEN

                      ( SELECT  max( VALOR ) as valor
                            FROM ( SELECT AQ.DOC_REM,
trim(AD.WAERK) ||
lpad(substr( SUM(AD.NETPR)/COUNT(*),1,instr( to_char(SUM(AD.NETPR)/COUNT(*), '999999999999999.99999'),'.' )-1),14,'0') ||'.'||
rpad(substr( SUM(AD.NETPR)/COUNT(*),instr( to_char(SUM(AD.NETPR)/COUNT(*), '999999999999999.99999'),'.' )+1, length(SUM(AD.NETPR)/COUNT(*))),5,'0') ||
lpad(substr( SUM(AD.TAX_DOLAR)/COUNT(*),1,instr( to_char(SUM(AD.TAX_DOLAR)/COUNT(*), '999999999999999.99999'),'.' )-1),14,'0') ||'.'||
rpad(substr( SUM(AD.TAX_DOLAR)/COUNT(*),instr( to_char(SUM(AD.TAX_DOLAR)/COUNT(*), '999999999999999.99999'),'.' )+1, length(SUM(AD.TAX_DOLAR)/COUNT(*))),5,'0') AS VALOR
                                     FROM SAPHANADB.ZLEST0060 AQ,
                                          SAPHANADB.ZLEST0061 AD
                                    WHERE AQ.MANDT   = '300'
                                      AND AQ.DOC_REM <> '0000000000'
                                      AND AQ.DOC_REM <> ' '
                                      AND AQ.DOCNUM  <> '0000000000'
                                      AND AQ.MANDT   = AD.MANDT
                                      AND AQ.DOCNUM  = AD.DOCNUM
                                   GROUP BY AQ.DOC_REM, AD.WAERK ) XX
                          WHERE VF.VBELN = XX.DOC_REM
                             )

                     ELSE null END AS VALOR1,

                CASE WHEN BK.AUART IN ('ZRDC','ZRFL','ZIND') AND TRANBORDOS.KUNNR <> '' THEN
                       (SELECT  max(
trim(G5.WAERK) ||
lpad(substr( G5.NETPR,1,instr( to_char(G5.NETPR, '999999999999999.99999'),'.' )-1),14,'0') ||'.'||
rpad(substr( G5.NETPR,instr( to_char(G5.NETPR, '999999999999999.99999'),'.' )+1, length(G5.NETPR)),5,'0')

) AS VALOR
                          FROM SAPHANADB.ZLEST0055 G5
                         WHERE G5.MANDT  = '300'
                           AND G5.MATKL  = LPAD(LP.MATKL,9,'0')
                           AND G5.VTWEG  = '10'
                           AND G5.SPART  = '08'
                           AND G5.STATUS = '1'
                           AND G5.AUART  IN ('ZTAB','ZTAM','ZTAG','ZTAF')
                           AND G5.PO_EMBARQUE = TRANBORDOS.KUNNR
                           AND G5.KUNNR       IN ( SELECT N1.KUNNR
                                                     FROM SAPHANADB.KNA1 NT,
                                                          SAPHANADB.KNA1 N1
                                                    WHERE NT.KUNNR = BK.KUNNR
                                                      AND SUBSTR(NT.STCD1,1,8) = SUBSTR(N1.STCD1,1,8) )
                           AND VF.ERDAT BETWEEN G5.DT_INICIO AND G5.DT_FIM
                            )
                     ELSE null
                END AS VALOR2,

                CASE WHEN BK.AUART IN ('ZRDC','ZRFL','ZIND') AND TRANBORDOS.KUNNR <> '' THEN
                        (SELECT  max(

trim(G5.WAERK) ||
lpad(substr( G5.NETPR,1,instr( to_char(G5.NETPR, '999999999999999.99999'),'.' )-1),14,'0') ||'.'||
rpad(substr( G5.NETPR,instr( to_char(G5.NETPR, '999999999999999.99999'),'.' )+1, length(G5.NETPR)),5,'0')

) AS VALOR
                          FROM SAPHANADB.ZLEST0055 G5,
                               SAPHANADB.KNA1      NJ
                         WHERE G5.MANDT  = '300'
                           AND G5.MATKL  = LPAD(LP.MATKL,9,'0')
                           AND G5.VTWEG  = '10'
                           AND G5.SPART  = '08'
                           AND G5.STATUS = '1'
                           AND G5.AUART  IN ('ZTAB','ZTAM','ZTAG','ZTAF')
                           AND NJ.MANDT  = '300'
                           AND NJ.STCD1  LIKE SUBSTR(KL.STCD1,1,8)||'%'
                           AND SUBSTR(NJ.STCD1,9,4) = '0001'
                           AND G5.PO_EMBARQUE  = NJ.KUNNR
                           AND G5.KUNNR       IN ( SELECT N1.KUNNR
                                                     FROM SAPHANADB.KNA1 NT,
                                                          SAPHANADB.KNA1 N1
                                                    WHERE NT.KUNNR = BK.KUNNR
                                                      AND SUBSTR(NT.STCD1,1,8) = SUBSTR(N1.STCD1,1,8) )
                           AND VF.ERDAT BETWEEN G5.DT_INICIO AND G5.DT_FIM
                            )
                     ELSE null
                END AS VALOR3,

                (SELECT max( INCO1 ) as INCO1 FROM SAPHANADB.VBKD DR WHERE DR.MANDT = '300'  AND DR.VBELN = VF.VBELV  ) AS INCO1,

                COALESCE((SELECT UK.UKURS FROM SAPHANADB.TCURR UK
                  WHERE UK.MANDT  = '300'
                    AND UK.KURST  = 'B'
                    AND UK.FCURR  = 'USD'
                    AND UK.TCURR  = 'BRL'
                    AND UK.GDATU  = 99999999 - VF.ERDAT ),
                    (SELECT UK.UKURS FROM SAPHANADB.TCURR UK
                  WHERE UK.MANDT  = '300'
                    AND UK.KURST  = 'B'
                    AND UK.FCURR  = 'USD'
                    AND UK.TCURR  = 'BRL'
                    AND UK.GDATU  = ( SELECT MIN(UK2.GDATU)
                                        FROM SAPHANADB.TCURR UK2
                                       WHERE UK2.MANDT = UK.MANDT
                                         AND UK2.KURST = UK.KURST
                                         AND UK2.FCURR = UK.FCURR
                                         AND UK2.TCURR = UK.TCURR
                                         AND UK2.GDATU > 99999999 - VF.ERDAT ))) AS UKURS,

                CASE WHEN BK.AUART IN ('ZRDC','ZRFL','ZIND') AND TRIM(LR.KUNNR) <> '' AND TRIM(Z1.LIFNR) <> '' THEN
                       (SELECT max( FR.PRECO )  as PRECO
                          FROM SAPHANADB.ZLEST0119 FR
                         WHERE FR.MANDT = '300'
                           AND FR.TP_PRECO = ' '
                           AND FR.TP_MODAL = '4'
                           AND VF.ERDAT BETWEEN FR.DT_INICIO AND FR.DT_FIM
                           AND FR.DOMICILIO_ORIGEM = ( SELECT GG.TXJCD FROM SAPHANADB.KNA1 GG WHERE GG.MANDT = '300'  AND GG.KUNNR = LR.KUNNR )
                           AND FR.DOMICILIO_DESTIN = ( SELECT GG.TXJCD FROM SAPHANADB.LFA1 GG WHERE GG.MANDT = '300'  AND GG.LIFNR = Z1.LIFNR )
                           AND FR.TIPO IN ('P','C')
                           AND EXISTS ( SELECT * FROM SAPHANADB.ZLEST0128 PP WHERE PP.MANDT = FR.MANDT AND PP.CD_SEQ_LANC = FR.CD_SEQ_LANC AND PP.MATNR = LP.MATNR )
                           AND EXISTS ( SELECT * FROM SAPHANADB.ZLEST0120 MM WHERE MM.MANDT = FR.MANDT AND MM.CD_SEQ_LANC = FR.CD_SEQ_LANC AND MM.BUKRS = BK.VKORG )
                            )
                     ELSE 0 END AS VALOR_FERRO,

                CASE WHEN BK.AUART IN ('ZRDC','ZRFL','ZIND') AND TRIM(LR.KUNNR) <> '' AND TRIM(Z1.LIFNR) <> '' THEN
                       (SELECT max( FR.PRECO ) as PRECO
                          FROM SAPHANADB.ZLEST0119 FR
                         WHERE FR.MANDT = '300'
                           AND FR.TP_PRECO = 'G'
                           AND FR.TP_MODAL = '4'
                           AND VF.ERDAT BETWEEN FR.DT_INICIO AND FR.DT_FIM
                           AND FR.DOMICILIO_ORIGEM = ( SELECT GG.TXJCD FROM SAPHANADB.KNA1 GG WHERE GG.MANDT = '300'  AND GG.KUNNR = LR.KUNNR )
                           AND FR.DOMICILIO_DESTIN = ( SELECT GG.TXJCD FROM SAPHANADB.LFA1 GG WHERE GG.MANDT = '300'  AND GG.LIFNR = Z1.LIFNR )
                           AND FR.TIPO IN ('P','C')
                           AND EXISTS ( SELECT * FROM SAPHANADB.ZLEST0128 PP WHERE PP.MANDT = FR.MANDT AND PP.CD_SEQ_LANC = FR.CD_SEQ_LANC AND PP.MATNR = LP.MATNR )
                           AND EXISTS ( SELECT * FROM SAPHANADB.ZLEST0120 MM WHERE MM.MANDT = FR.MANDT AND MM.CD_SEQ_LANC = FR.CD_SEQ_LANC AND MM.BUKRS = BK.VKORG )
                           )
                     ELSE 0 END AS VALOR_FERRO_GERENCIAL,

                CASE WHEN BK.AUART IN ('ZRDC','ZRFL','ZIND') AND TRIM(LR.KUNNR) <> '' AND TRIM(Z1.LIFNR) <> '' THEN
                       (SELECT max( FR.PRECO ) as preco
                          FROM SAPHANADB.ZLEST0119 FR
                         WHERE FR.MANDT = '300'
                           AND FR.TP_PRECO = 'G'
                           AND FR.TP_MODAL = '3'
                           AND VF.ERDAT BETWEEN FR.DT_INICIO AND FR.DT_FIM
                           AND FR.DOMICILIO_ORIGEM = ( SELECT GG.TXJCD FROM SAPHANADB.KNA1 GG WHERE GG.MANDT = '300'  AND GG.KUNNR = LR.KUNNR )
                           AND FR.DOMICILIO_DESTIN = ( SELECT GG.TXJCD FROM SAPHANADB.LFA1 GG WHERE GG.MANDT = '300'  AND GG.LIFNR = Z1.LIFNR )
                           AND FR.TIPO IN ('P','C')
                           AND EXISTS ( SELECT * FROM SAPHANADB.ZLEST0128 PP WHERE PP.MANDT = FR.MANDT AND PP.CD_SEQ_LANC = FR.CD_SEQ_LANC AND PP.MATNR = LP.MATNR )
                           AND EXISTS ( SELECT * FROM SAPHANADB.ZLEST0120 MM WHERE MM.MANDT = FR.MANDT AND MM.CD_SEQ_LANC = FR.CD_SEQ_LANC AND MM.BUKRS = BK.VKORG )
                            )
                     ELSE 0 END AS VALOR_AQUAVIARIO,

                CASE WHEN BK.AUART IN ('ZRDC','ZRFL','ZIND') AND TRIM(LR.KUNNR) <> '' AND TRIM(Z1.LIFNR) <> '' THEN
                       (SELECT max( FR.WAERK ) as WAERK
                          FROM SAPHANADB.ZLEST0119 FR
                         WHERE FR.MANDT = '300'
                           AND FR.TP_PRECO = 'G'
                           AND FR.TP_MODAL = '4'
                           AND VF.ERDAT BETWEEN FR.DT_INICIO AND FR.DT_FIM
                           AND FR.DOMICILIO_ORIGEM = ( SELECT GG.TXJCD FROM SAPHANADB.KNA1 GG WHERE GG.MANDT = '300'  AND GG.KUNNR = LR.KUNNR )
                           AND FR.DOMICILIO_DESTIN = ( SELECT GG.TXJCD FROM SAPHANADB.LFA1 GG WHERE GG.MANDT = '300'  AND GG.LIFNR = Z1.LIFNR )
                           AND FR.TIPO IN ('P','C')
                           AND EXISTS ( SELECT * FROM SAPHANADB.ZLEST0128 PP WHERE PP.MANDT = FR.MANDT AND PP.CD_SEQ_LANC = FR.CD_SEQ_LANC AND PP.MATNR = LP.MATNR )
                           AND EXISTS ( SELECT * FROM SAPHANADB.ZLEST0120 MM WHERE MM.MANDT = FR.MANDT AND MM.CD_SEQ_LANC = FR.CD_SEQ_LANC AND MM.BUKRS = BK.VKORG )
                            )
                     ELSE '' END AS MOEDA_FERRO_GERENCIAL,

                CASE WHEN BK.AUART IN ('ZRDC','ZRFL','ZIND') AND TRIM(LR.KUNNR) <> '' AND TRIM(Z1.LIFNR) <> '' THEN
                       (SELECT max( FR.WAERK ) as WAERK
                          FROM SAPHANADB.ZLEST0119 FR
                         WHERE FR.MANDT = '300'
                           AND FR.TP_PRECO = 'G'
                           AND FR.TP_MODAL = '3'
                           AND VF.ERDAT BETWEEN FR.DT_INICIO AND FR.DT_FIM
                           AND FR.DOMICILIO_ORIGEM = ( SELECT GG.TXJCD FROM SAPHANADB.KNA1 GG WHERE GG.MANDT = '300'  AND GG.KUNNR = LR.KUNNR )
                           AND FR.DOMICILIO_DESTIN = ( SELECT GG.TXJCD FROM SAPHANADB.LFA1 GG WHERE GG.MANDT = '300'  AND GG.LIFNR = Z1.LIFNR )
                           AND FR.TIPO IN ('P','C')
                           AND EXISTS ( SELECT * FROM SAPHANADB.ZLEST0128 PP WHERE PP.MANDT = FR.MANDT AND PP.CD_SEQ_LANC = FR.CD_SEQ_LANC AND PP.MATNR = LP.MATNR )
                           AND EXISTS ( SELECT * FROM SAPHANADB.ZLEST0120 MM WHERE MM.MANDT = FR.MANDT AND MM.CD_SEQ_LANC = FR.CD_SEQ_LANC AND MM.BUKRS = BK.VKORG )
                            )
                     ELSE '' END AS MOEDA_AQUAVIARIO

           FROM SAPHANADB.VBFA     VF LEFT JOIN (SELECT * FROM SAPHANADB.ZSDT0001 RO WHERE RO.MANDT = '300' AND RO.TP_MOVIMENTO = 'S') RO
                                     ON VF.MANDT   = RO.MANDT
                                        AND VF.VBELN   = RO.DOC_REM


                              LEFT JOIN (SELECT * FROM SAPHANADB.VBPA     LR WHERE LR.MANDT = '300' AND LR.PARVW = 'LR') LR
                     ON VF.MANDT   = LR.MANDT
                                        AND VF.VBELV   = LR.VBELN

                      LEFT JOIN ( SELECT GG.PO_EMBARQUE AS KUNNR
                          FROM SAPHANADB.ZLEST0055 GG,
                             SAPHANADB.KNA1      NJ1,
                             SAPHANADB.KNA1      NJ2
                           WHERE GG.MANDT   = '300'
                           AND '20230923' BETWEEN GG.DT_INICIO AND GG.DT_FIM
                           AND GG.MANDT       = NJ1.MANDT
                           AND GG.PO_EMBARQUE = NJ1.KUNNR
                           AND GG.MANDT       = NJ2.MANDT
                           AND SUBSTR(NJ1.STCD1,1,8) = SUBSTR(NJ2.STCD1,1,8)
                           UNION
                          SELECT GG.PO_EMBARQUE AS KUNNR
                          FROM SAPHANADB.ZLEST0055 GG,
                             SAPHANADB.KNA1      NJ1,
                             SAPHANADB.KNA1      NJ2
                           WHERE GG.MANDT   = '300'
                           AND '20231023' BETWEEN GG.DT_INICIO AND GG.DT_FIM
                           AND GG.MANDT       = NJ1.MANDT
                           AND GG.PO_EMBARQUE = NJ1.KUNNR
                           AND GG.MANDT       = NJ2.MANDT
                           AND SUBSTR(NJ1.STCD1,1,8) = SUBSTR(NJ2.STCD1,1,8) ) TRANBORDOS

                                       ON LR.KUNNR   = TRANBORDOS.KUNNR

                    LEFT JOIN (SELECT * FROM SAPHANADB.VBPA     LR WHERE LR.MANDT = '300' AND LR.PARVW = 'Z1') Z1
                     ON VF.MANDT   = Z1.MANDT
                                        AND VF.VBELV   = Z1.VBELN

                    LEFT JOIN (SELECT * FROM SAPHANADB.VBPA     LR WHERE LR.MANDT = '300' AND LR.PARVW = 'AG') AG
                     ON VF.MANDT   = AG.MANDT
                                        AND VF.VBELV   = AG.VBELN


                    LEFT JOIN   (SELECT * FROM SAPHANADB.VBPA     LR WHERE LR.MANDT = '300' AND LR.PARVW = 'PC') PC
                      ON VF.MANDT   = PC.MANDT
                                       AND VF.VBELV   = PC.VBELN,

                SAPHANADB.VBAK     BK,
                SAPHANADB.LIPS     LP,
                SAPHANADB.KNA1     KL


          WHERE VF.MANDT    = :SY-MANDT
            AND VF.ERDAT   >= :LC_ERDAT
            AND VF.ERDAT   <= :LC_ERDAT_FIM
            AND VF.VBTYP_N  = 'J'
            AND VF.VBTYP_V  = 'C'
            AND VF.MANDT   = BK.MANDT
            AND VF.VBELV   = BK.VBELN
            AND BK.VKORG   IN ('0001','0015','0018', '0050')
            AND BK.AUART   NOT IN ('ZEXI','ZEXP','ZPER')
            AND VF.MANDT   = LP.MANDT
            AND VF.VBELN   = LP.VBELN
            AND BK.MANDT   = KL.MANDT
            AND BK.KUNNR   = KL.KUNNR
                ) TT


    ENDEXEC.
  CATCH cx_sy_native_sql_error INTO exc_ref.
    error_text = exc_ref->get_text( ).
    MESSAGE error_text TYPE 'E' RAISING erro_sql.
ENDTRY.

DO.
  EXEC SQL.
    FETCH NEXT DOCUMENTOS INTO
    :WA_LINHA-ERDAT,
    :WA_LINHA-VBELN,
    :WA_LINHA-RFMNG,
    :WA_LINHA-VBELV,
    :WA_LINHA-MATNR,
    :WA_LINHA-MATKL,
    :WA_LINHA-WERKS,
    :WA_LINHA-CHARG,
    :WA_LINHA-NR_ROMANEIO,
    :WA_LINHA-TP_TRANSGENIA,
    :WA_LINHA-COD_COLETA,
    :WA_LINHA-COD_TRANSBORDO,
    :WA_LINHA-COD_PORTO,
    :WA_LINHA-CENTRO_VIRTUAL,
    :WA_LINHA-NOME_DESTINO,
    :WA_LINHA-AUART,
    :WA_LINHA-COD_CLIENTE,
    :WA_LINHA-INCO1,
    :WA_LINHA-MOEDA_AQUA,
    :WA_LINHA-TAXAS_AQUA,
    :WA_LINHA-VALOR_AQUA,
    :WA_LINHA-UKURS,
    :WA_LINHA-VALOR_FERRO,
    :WA_LINHA-VALOR_FERRO_GERENCIAL,
    :WA_LINHA-VALOR_AQUAVIARIO,
    :WA_LINHA-MOEDA_FERRO_GERENCIAL,
    :WA_LINHA-MOEDA_AQUAVIARIO.
  ENDEXEC.
  IF sy-subrc <> 0.
    EXIT.
  ELSE.
    APPEND wa_linha TO it_linha.
  ENDIF.
ENDDO.

EXEC SQL.
  CLOSE DOCUMENTOS
ENDEXEC.


LOOP AT it_linha.
  CLEAR: it_zlest0112.
  it_zlest0112-vbeln           = it_linha-vbeln.
  it_zlest0112-erdat           = it_linha-erdat.
  it_zlest0112-matnr           = it_linha-matnr.
  it_zlest0112-matkl           = it_linha-matkl.
  it_zlest0112-werks           = it_linha-werks.
  it_zlest0112-lfimg           = it_linha-rfmng.
  it_zlest0112-vbelv           = it_linha-vbelv.
  it_zlest0112-safra           = it_linha-charg.
  it_zlest0112-auart           = it_linha-auart.
  it_zlest0112-inco1           = it_linha-inco1.
  it_zlest0112-cod_cliente     = it_linha-cod_cliente.
  it_zlest0112-cod_transbordo  = it_linha-cod_transbordo.
  it_zlest0112-cod_porto       = it_linha-cod_porto.
  it_zlest0112-nr_romaneio     = it_linha-nr_romaneio.
  it_zlest0112-centro_virtual  = it_linha-centro_virtual.
  it_zlest0112-nome_destino    = it_linha-nome_destino.
  it_zlest0112-tp_transgenia   = it_linha-tp_transgenia.
  it_zlest0112-cod_coleta      = it_linha-cod_coleta.
  it_zlest0112-dt_atual        = sy-datum.
  it_zlest0112-hr_atual        = sy-uzeit.
  it_zlest0112-ukurs           = it_linha-ukurs.
  it_zlest0112-preco_aqua_brl  = 0.
  it_zlest0112-preco_aqua_us   = 0.
  it_zlest0112-preco_ferro_brl = 0.
  it_zlest0112-preco_ferro_us  = 0.

  it_zlest0112-preco_aqua_brl_g  = 0.
  it_zlest0112-preco_aqua_us_g   = 0.
  it_zlest0112-preco_ferro_brl_g = 0.
  it_zlest0112-preco_ferro_us_g  = 0.

  it_zlest0112-vlr_frete_brl     = 0.
  it_zlest0112-vlr_frete_us      = 0.
  it_zlest0112-vlr_ped_r         = 0.
  it_zlest0112-vlr_ped_us        = 0.
  it_zlest0112-vlr_est_r         = 0.
  it_zlest0112-vlr_est_us        = 0.

  CASE it_linha-moeda_aqua.
    WHEN 'BRL'.
      it_zlest0112-preco_aqua_brl = ( it_zlest0112-lfimg * it_linha-valor_aqua ) / 1000.
      IF it_linha-taxas_aqua NE 0.
        it_zlest0112-preco_aqua_us  = it_zlest0112-preco_aqua_brl / it_linha-taxas_aqua.
      ELSE.
        it_zlest0112-preco_aqua_us  = it_zlest0112-preco_aqua_brl / it_linha-ukurs.
      ENDIF.
    WHEN 'USD'.
      it_zlest0112-preco_aqua_us  = ( it_zlest0112-lfimg * it_linha-valor_aqua ) / 1000.
      IF it_linha-taxas_aqua NE 0.
        it_zlest0112-preco_aqua_brl = it_zlest0112-preco_aqua_us * it_linha-taxas_aqua.
      ELSE.
        it_zlest0112-preco_aqua_brl = it_zlest0112-preco_aqua_us * it_linha-ukurs.
      ENDIF.
  ENDCASE.

  IF it_linha-valor_ferro NE 0.
    it_zlest0112-preco_ferro_brl = ( it_zlest0112-lfimg * it_linha-valor_ferro ) / 1000.
    IF it_linha-ukurs NE 0.
      it_zlest0112-preco_ferro_us  = it_zlest0112-preco_ferro_brl / it_linha-ukurs.
    ENDIF.
  ENDIF.

  IF it_linha-ukurs IS NOT INITIAL.
    IF it_linha-moeda_aquaviario EQ 'USD'.
      it_zlest0112-preco_aqua_us_g   = it_linha-valor_aquaviario.
      it_zlest0112-preco_aqua_brl_g  = it_zlest0112-preco_aqua_us_g * it_linha-ukurs.
    ELSE.
      it_zlest0112-preco_aqua_brl_g  = it_linha-valor_aquaviario.
      it_zlest0112-preco_aqua_us_g   = it_zlest0112-preco_aqua_brl_g / it_linha-ukurs.
    ENDIF.

    IF it_linha-moeda_ferro_gerencial EQ 'USD'.
      it_zlest0112-preco_ferro_us_g  = it_linha-valor_ferro_gerencial.
      it_zlest0112-preco_ferro_brl_g = it_zlest0112-preco_ferro_us_g * it_linha-ukurs.
    ELSE.
      it_zlest0112-preco_ferro_brl_g = it_linha-valor_ferro_gerencial.
      it_zlest0112-preco_ferro_us_g  = it_zlest0112-preco_ferro_brl_g / it_linha-ukurs.
    ENDIF.
  ENDIF.

  "BSART
  APPEND it_zlest0112.
ENDLOOP.

CLEAR: it_linha[].

TRY.
    EXEC SQL.
      OPEN DOCUMENTOS FOR
        SELECT KO.BSART,
               EO.INCO1,
               EK.BUDAT AS ERDAT,
               EK.EBELN AS VBELV,
               EK.BELNR AS VBELN,
               CASE WHEN TRIM(PA.LIFN2) = '' THEN LPAD(KO.RESWK,10,'0') ELSE PA.LIFN2 END COD_COLETA,
               LPAD(EO.WERKS,10,'0') AS COD_CLIENTE,
               ( SELECT KK.ORT01||' - '||KK.REGIO FROM SAPHANADB.KNA1 KK WHERE KK.MANDT = EK.MANDT AND KK.KUNNR = LPAD(EO.WERKS,10,'0')) AS NOME_DESTINO,
               TR.LIFN2 AS COD_TRANSBORDO,
               RO.NR_ROMANEIO,
               CASE WHEN RO.TP_TRANSGENIA IN ('CO','C ') THEN 'NAO' ELSE CASE WHEN RO.NR_ROMANEIO = '' THEN '' ELSE 'SIM' END END AS TP_TRANSGENIA,
               LS.LFIMG AS RFMNG,
               LS.MATNR,
               LPAD(LS.MATKL,9,'0') AS MATKL,
               LS.WERKS,
               LS.CHARG,
               COALESCE((SELECT UK.UKURS FROM SAPHANADB.TCURR UK
                 WHERE UK.MANDT  = :SY-MANDT
                   AND UK.KURST  = 'B'
                   AND UK.FCURR  = 'USD'
                   AND UK.TCURR  = 'BRL'
                   AND UK.GDATU  = 99999999 - EK.BUDAT ),
                   (SELECT UK.UKURS FROM SAPHANADB.TCURR UK
                  WHERE UK.MANDT  = :SY-MANDT
                   AND UK.KURST  = 'B'
                   AND UK.FCURR  = 'USD'
                   AND UK.TCURR  = 'BRL'
                   AND UK.GDATU  = ( SELECT MIN(UK2.GDATU)
                                       FROM SAPHANADB.TCURR UK2
                                      WHERE UK2.MANDT = UK.MANDT
                                        AND UK2.KURST = UK.KURST
                                        AND UK2.FCURR = UK.FCURR
                                        AND UK2.TCURR = UK.TCURR
                                        AND UK2.GDATU > 99999999 - EK.BUDAT ))) AS UKURS

      FROM SAPHANADB.EKBE EK LEFT JOIN (SELECT * FROM SAPHANADB.EKPA PA WHERE PA.PARVW = 'PR' ) PA
                                        ON EK.MANDT  = PA.MANDT
                                           AND EK.EBELN  = PA.EBELN

                             LEFT JOIN (SELECT * FROM SAPHANADB.EKPA PA WHERE PA.PARVW = 'ZT' ) TR
                           ON EK.MANDT  = TR.MANDT
                                          AND EK.EBELN  = TR.EBELN,

               SAPHANADB.EKKO KO,
               SAPHANADB.EKPO EO,

               SAPHANADB.LIPS LS LEFT JOIN (SELECT * FROM SAPHANADB.ZSDT0001 RO WHERE RO.MANDT = :SY-MANDT ) RO
                                          ON LS.MANDT   = RO.MANDT
                                         AND LS.VBELN   = RO.DOC_REM

         WHERE EK.MANDT  = :SY-MANDT
           AND EK.BUDAT >= :LC_ERDAT
           AND EK.BUDAT <= :LC_ERDAT_FIM
           AND EK.VGABE  = '8'
           AND EK.MANDT  = KO.MANDT
           AND EK.EBELN  = KO.EBELN
           AND KO.BSART  = 'ZUB'
           AND KO.BUKRS  IN ('0001','0015','0018', '0050')
           AND EK.MANDT  = EO.MANDT
           AND EK.EBELN  = EO.EBELN
           AND EK.EBELP  = EO.EBELP
           AND EK.MANDT  = LS.MANDT
           AND EK.BELNR  = LS.VBELN
           AND LPAD(EK.BUZEI,6,'0') = LS.POSNR
    ENDEXEC.
  CATCH cx_sy_native_sql_error INTO exc_ref.
    error_text = exc_ref->get_text( ).
    MESSAGE error_text TYPE 'E' RAISING erro_sql.
ENDTRY.

DO.
  EXEC SQL.
    FETCH NEXT DOCUMENTOS INTO
    :WA_LINHA-BSART,
    :WA_LINHA-INCO1,
    :WA_LINHA-ERDAT,
    :WA_LINHA-VBELV,
    :WA_LINHA-VBELN,
    :WA_LINHA-COD_COLETA,
    :WA_LINHA-COD_CLIENTE,
    :WA_LINHA-NOME_DESTINO,
    :WA_LINHA-COD_TRANSBORDO,
    :WA_LINHA-NR_ROMANEIO,
    :WA_LINHA-TP_TRANSGENIA,
    :WA_LINHA-RFMNG,
    :WA_LINHA-MATNR,
    :WA_LINHA-MATKL,
    :WA_LINHA-WERKS,
    :WA_LINHA-CHARG,
    :WA_LINHA-UKURS.
  ENDEXEC.

  IF sy-subrc <> 0.
    EXIT.
  ELSE.
    APPEND wa_linha TO it_linha.
  ENDIF.
ENDDO.

EXEC SQL.
  CLOSE DOCUMENTOS
ENDEXEC.

LOOP AT it_linha.
  CLEAR: it_zlest0112.
  it_zlest0112-vbeln           = it_linha-vbeln.
  it_zlest0112-erdat           = it_linha-erdat.
  it_zlest0112-matnr           = it_linha-matnr.
  it_zlest0112-matkl           = it_linha-matkl.
  it_zlest0112-werks           = it_linha-werks.
  it_zlest0112-safra           = it_linha-charg.
  it_zlest0112-lfimg           = it_linha-rfmng.
  it_zlest0112-vbelv           = it_linha-vbelv.
  it_zlest0112-bsart           = it_linha-bsart.
  it_zlest0112-inco1           = it_linha-inco1.
  it_zlest0112-cod_coleta      = it_linha-cod_coleta.
  it_zlest0112-cod_transbordo  = it_linha-cod_transbordo.
  it_zlest0112-cod_cliente     = it_linha-cod_cliente.
  it_zlest0112-nome_destino    = it_linha-nome_destino.
  it_zlest0112-nr_romaneio     = it_linha-nr_romaneio.
  it_zlest0112-tp_transgenia   = it_linha-tp_transgenia.
  it_zlest0112-dt_atual        = sy-datum.
  it_zlest0112-hr_atual        = sy-uzeit.
  it_zlest0112-ukurs           = it_linha-ukurs.
  it_zlest0112-preco_aqua_brl  = 0.
  it_zlest0112-preco_aqua_us   = 0.
  it_zlest0112-preco_ferro_brl = 0.
  it_zlest0112-preco_ferro_us  = 0.
  it_zlest0112-vlr_frete_brl   = 0.
  it_zlest0112-vlr_frete_us    = 0.
  it_zlest0112-vlr_ped_r       = 0.
  it_zlest0112-vlr_ped_us      = 0.
  it_zlest0112-vlr_est_r       = 0.
  it_zlest0112-vlr_est_us      = 0.
  APPEND it_zlest0112.
ENDLOOP.

SORT it_zlest0112 BY werks.

CHECK it_zlest0112[] IS NOT INITIAL.

"Busca de Dados Custo de transporte
SELECT *
  FROM vttp
  INTO TABLE it_vttp
   FOR ALL ENTRIES IN it_zlest0112
 WHERE vbeln EQ it_zlest0112-vbeln.

SORT it_vttp BY vbeln ASCENDING.

IF it_vttp[] IS NOT INITIAL.

  "Cabeçalho transporte
  SELECT *
    FROM vttk
    INTO TABLE it_vttk
     FOR ALL ENTRIES IN it_vttp
   WHERE tknum EQ it_vttp-tknum
     AND vsart EQ '01'.

  SORT it_vttk BY tknum.

  "Custos de frete: dados do item
  SELECT *
    FROM vfkp
    INTO TABLE it_vfkp
     FOR ALL ENTRIES IN it_vttk
   WHERE rebel EQ it_vttk-tknum.

  SORT it_vfkp BY rebel.

  IF it_vfkp[] IS NOT INITIAL.
    "Condições (dados de operação)
    SELECT FROM v_konv FIELDS * FOR ALL ENTRIES IN @it_vfkp WHERE knumv EQ @it_vfkp-knumv AND kschl IN ( 'ZFRE' , 'ZLOT' , 'ZPED' ) INTO CORRESPONDING FIELDS OF TABLE @it_konv .

    SORT it_konv BY knumv.
  ENDIF.
ENDIF.

SORT it_zlest0112[] BY vbeln ASCENDING.
it_sort_vttp[] = it_vttp[].
it_sort_vfkp[] = it_vfkp[].
it_sort_konv[] = it_konv[].
it_sort_0112[] = it_zlest0112[].

"Valor do Frete Rodoviário
LOOP AT it_zlest0112 ASSIGNING <fs_0112>.

  "LOOP AT it_vttp WHERE vbeln EQ <fs_0112>-vbeln.
  LOOP AT it_sort_vttp[] ASSIGNING FIELD-SYMBOL(<fs_vttp>) WHERE vbeln EQ <fs_0112>-vbeln.
    "READ TABLE it_vttk WITH KEY tknum = it_vttp-tknum BINARY SEARCH.
    READ TABLE it_vttk WITH KEY tknum = <fs_vttp>-tknum BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      "LOOP AT it_vfkp WHERE rebel EQ it_vttk-tknum.
      LOOP AT it_sort_vfkp[] ASSIGNING FIELD-SYMBOL(<fs_vfkp>) WHERE rebel EQ it_vttk-tknum.
        "LOOP AT it_konv WHERE knumv EQ it_vfkp-knumv.
        LOOP AT it_sort_konv ASSIGNING FIELD-SYMBOL(<fs_konv>) WHERE knumv EQ it_vfkp-knumv.
          CASE it_konv-kschl.
            WHEN 'ZPED'.
              ADD it_konv-kwert TO <fs_0112>-vlr_ped_r.
            WHEN OTHERS.
              ADD it_konv-kwert TO <fs_0112>-vlr_frete_brl.
          ENDCASE.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  IF <fs_0112>-ukurs IS NOT INITIAL.
    IF <fs_0112>-vlr_ped_r IS NOT INITIAL.
      <fs_0112>-vlr_ped_us = <fs_0112>-vlr_ped_r / <fs_0112>-ukurs.
    ENDIF.

    IF <fs_0112>-vlr_frete_brl IS NOT INITIAL.
      <fs_0112>-vlr_frete_us = <fs_0112>-vlr_frete_brl / <fs_0112>-ukurs.
    ENDIF.
  ENDIF.

ENDLOOP.


IF it_zlest0112[] IS NOT INITIAL.

  CLEAR: lit_likp[].
  SELECT *
    FROM likp INTO TABLE lit_likp
    FOR ALL ENTRIES IN it_zlest0112
   WHERE vbeln = it_zlest0112-vbeln.

  SORT lit_likp BY vbeln.

  LOOP AT it_zlest0112 ASSIGNING FIELD-SYMBOL(<fs_zlest0112>).
    READ TABLE lit_likp INTO DATA(lwa_likp) WITH KEY vbeln = <fs_zlest0112>-vbeln BINARY SEARCH.
    IF sy-subrc EQ 0 AND lwa_likp-fkdat IS NOT INITIAL.
      <fs_zlest0112>-erdat = lwa_likp-fkdat.
    ENDIF.
  ENDLOOP.

  MODIFY zlest0112 FROM TABLE it_zlest0112.
ENDIF.

IF lc_carga EQ abap_false.
  "Exclui todos os registros integrados neste dia
  EXEC SQL.
    DELETE FROM SAPHANADB.ZLEST0112 O2 WHERE NOT EXISTS ( SELECT * FROM SAPHANADB.LIKP P WHERE P.MANDT = O2.MANDT AND P.VBELN = O2.VBELN )
  ENDEXEC.

  "Exclui todos os registros integrados neste dia
  EXEC SQL.
    DELETE FROM SAPHANADB.ZLEST0113 WHERE ERDAT >= :LC_ERDAT
  ENDEXEC.
ENDIF.

TRY.
    EXEC SQL.

      INSERT INTO SAPHANADB.ZLEST0113 ( MANDT, ERDAT, SAFRA, WERKS, MATNR, COD_TRANSBORDO, COD_DESTINO, TP_TRANSGENIA, NM_TRANSBORDO, NM_DESTINO, NM_FILIAL, WGBEZ, LFIMG, QUANT_TRANSP )
      SELECT COALESCE(O2.MANDT,' '),
                       COALESCE(O2.ERDAT,' '),
                       COALESCE(O2.SAFRA,' '),
                       COALESCE(O2.WERKS,' '),
                       COALESCE(O2.MATNR,' '),
                       COALESCE(O2.COD_TRANSBORDO,' '),
                       COALESCE(CASE WHEN TRIM(O2.COD_PORTO) <> '' THEN O2.COD_PORTO ELSE O2.COD_CLIENTE END,' '),
                       COALESCE(CASE WHEN O2.TP_TRANSGENIA = 'SIM' THEN 'TR' WHEN O2.TP_TRANSGENIA = 'NAO' THEN 'CO' ELSE '' END,' '),
                       COALESCE(TR.NAME1,' ') AS NM_TRANSBORDO,
                       COALESCE(CASE WHEN TRIM(O2.COD_PORTO) <> '' THEN
                              ( SELECT PT.NAME1 FROM SAPHANADB.LFA1 PT WHERE PT.MANDT = O2.MANDT AND PT.LIFNR = O2.COD_PORTO )
                            ELSE
                              CASE WHEN TRIM(O2.BSART) = '' THEN
                               (SELECT PT.NAME1 FROM SAPHANADB.KNA1 PT WHERE PT.MANDT = O2.MANDT AND PT.KUNNR = O2.COD_CLIENTE)
                              ELSE
                               (SELECT NAME1 FROM SAPHANADB.T001W FL  WHERE FL.MANDT = O2.MANDT AND FL.WERKS = SUBSTR(O2.COD_CLIENTE,7,4))
                              END
                       END,' ') NM_DESTINO,
                       COALESCE(FL.NAME1,' '),
                       COALESCE(GB.WGBEZ,' '),
                       COALESCE(SUM(O2.LFIMG),0)/1000 AS LFIMG,
                       COALESCE(COUNT(*),0) AS QUANT_TRANSP
                  FROM SAPHANADB.ZLEST0112 O2 LEFT JOIN SAPHANADB.KNA1      TR

                                             ON O2.MANDT          = TR.MANDT
                                            AND O2.COD_TRANSBORDO = TR.KUNNR

                          LEFT JOIN SAPHANADB.T001W     FL

                                                  ON O2.MANDT          = FL.MANDT
                                                 AND O2.WERKS          = FL.WERKS

                                        LEFT JOIN  (SELECT * FROM SAPHANADB.T023T T WHERE T.MANDT = '300' AND T.SPRAS = 'P' ) GB

                                             ON SUBSTR(O2.MATKL,4,6) = GB.MATKL


                 WHERE O2.MANDT          = :SY-MANDT
                   AND O2.ERDAT         >= :LC_ERDAT
                   AND O2.ERDAT         <= :LC_ERDAT_FIM

                 GROUP BY O2.MANDT,
                          O2.ERDAT,
                          O2.SAFRA,
                          O2.WERKS,
                          FL.NAME1,
                          O2.MATNR,
                          O2.COD_TRANSBORDO,
                          TR.NAME1, O2.COD_PORTO, O2.COD_CLIENTE,
                          GB.WGBEZ,
                          O2.BSART,
                          CASE WHEN O2.TP_TRANSGENIA = 'SIM' THEN 'TR' WHEN O2.TP_TRANSGENIA = 'NAO' THEN 'CO' ELSE '' END

    ENDEXEC.
  CATCH cx_sy_native_sql_error INTO exc_ref.
    error_text = exc_ref->get_text( ).
    MESSAGE error_text TYPE 'E' RAISING erro_sql.
ENDTRY.

COMMIT WORK.
