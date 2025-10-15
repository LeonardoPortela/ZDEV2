*&---------------------------------------------------------------------*
*& Report  ZFIY0034
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfiy0034.

TABLES: FAGLFLEXA.

DATA: "IT_FAGLFLEXA    TYPE TABLE OF FAGLFLEXA,
  "IT_BSXS         TYPE TABLE OF BSIS,
  "IT_BSXS_AUX     TYPE TABLE OF BSIS,
  "IT_SALDO_CONTAS TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT,
  it_ope_per  TYPE TABLE OF zde_ope_per WITH HEADER LINE,
  "IT_BKPF         TYPE TABLE OF BKPF WITH HEADER LINE,
  "IT_BKPF_VALIDOS TYPE TABLE OF BKPF WITH HEADER LINE,
  "IT_PARTIDAS     TYPE TABLE OF ZDE_FI_GL_PARTIDAS_CLI_FOR WITH HEADER LINE,
  "IT_PARTIDAS_FOR TYPE TABLE OF ZDE_FI_GL_PARTIDAS_CLI_FOR WITH HEADER LINE,
  "IT_PARTIDAS_AUX TYPE TABLE OF ZDE_FI_GL_PARTIDAS_CLI_FOR WITH HEADER LINE,
  "IT_KNA1         TYPE TABLE OF KNA1 WITH HEADER LINE,
  "IT_LFA1         TYPE TABLE OF LFA1 WITH HEADER LINE,
  e_first_day TYPE sy-datum,
  e_last_day  TYPE sy-datum.
"IT_WITH_ITEM    TYPE TABLE OF WITH_ITEM WITH HEADER LINE.

SELECTION-SCREEN: BEGIN OF BLOCK ba WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: pbukrs FOR faglflexa-rbukrs NO-EXTENSION NO INTERVALS OBLIGATORY DEFAULT '0100',
                  pgjahr FOR faglflexa-gjahr  NO-EXTENSION NO INTERVALS OBLIGATORY,
                  ppoper FOR faglflexa-poper  NO-EXTENSION NO INTERVALS OBLIGATORY.
  PARAMETERS: prldnr TYPE fagl_rldnr DEFAULT '0L' NO-DISPLAY.
SELECTION-SCREEN: END OF BLOCK ba.

INCLUDE zfiy0034_0100.

INITIALIZATION.


START-OF-SELECTION.
  "1. Conta contabil 213052: retencao
  "2. Conta contabil 213051: percepcao

  DATA: i_monat  TYPE am_monat,
        e_contas TYPE zct_emp_contas,
        wa_conta TYPE zlc_emp_contas.

  i_monat = ppoper-low.

  CALL FUNCTION 'OIUREP_MONTH_FIRST_LAST'
    EXPORTING
      i_month     = i_monat
      i_year      = pgjahr-low
    IMPORTING
      e_first_day = e_first_day
      e_last_day  = e_last_day
    EXCEPTIONS
      wrong_date  = 1
      OTHERS      = 2.

  wa_conta-bukrs = pbukrs-low.
  "WA_CONTA-SAKNR = '0000213052'.
  "APPEND WA_CONTA TO E_CONTAS.
  wa_conta-saknr = '0000213051'.
  APPEND wa_conta TO e_contas.

*  CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
*    EXPORTING
*      P_DT_INICIAL    = E_FIRST_DAY
*      P_DT_POSICAO    = E_LAST_DAY
*      CONTAS          = E_CONTAS
*    TABLES
*      IT_BSXS         = IT_BSXS
*      IT_SALDO_CONTAS = IT_SALDO_CONTAS.
*
*  CLEAR: IT_BKPF[], IT_BKPF_VALIDOS[].
*
*  LOOP AT IT_BSXS INTO DATA(WA_BSXS).
*    CLEAR: IT_BKPF.
*    IT_BKPF-GJAHR = WA_BSXS-GJAHR.
*    IT_BKPF-BELNR = WA_BSXS-BELNR.
*    IT_BKPF-BUKRS = WA_BSXS-BUKRS.
*    APPEND IT_BKPF.
*  ENDLOOP.
*
*  IF IT_BKPF[] IS NOT INITIAL.
*    SELECT * INTO TABLE IT_BKPF_VALIDOS
*       FROM BKPF
*       FOR ALL ENTRIES IN IT_BKPF
*      WHERE GJAHR EQ IT_BKPF-GJAHR
*        AND BELNR EQ IT_BKPF-BELNR
*        AND BUKRS EQ IT_BKPF-BUKRS
*        AND STBLG EQ SPACE.
*
*    IF IT_BKPF_VALIDOS[] IS NOT INITIAL.
*      SELECT * INTO TABLE IT_WITH_ITEM
*        FROM WITH_ITEM
*         FOR ALL ENTRIES IN IT_BKPF_VALIDOS
*       WHERE BUKRS EQ IT_BKPF_VALIDOS-BUKRS
*         AND BELNR EQ IT_BKPF_VALIDOS-BELNR
*         AND GJAHR EQ IT_BKPF_VALIDOS-GJAHR
*         AND WITHT EQ 'GB'.
*    ENDIF.
*  ENDIF.
*
*  CLEAR: IT_BKPF[].
*  MOVE IT_BKPF_VALIDOS[] TO IT_BKPF[].
*  SORT IT_BKPF_VALIDOS BY BUKRS GJAHR BELNR.
*  SORT IT_BKPF BY BUKRS GJAHR BELNR.
*  DELETE ADJACENT DUPLICATES FROM IT_BKPF COMPARING BUKRS GJAHR BELNR.
*  DELETE ADJACENT DUPLICATES FROM IT_BKPF_VALIDOS COMPARING BUKRS GJAHR BELNR.
*
*  CALL FUNCTION 'Z_FI_GL_PARTIDAS'
*    TABLES
*      IT_BKPF     = IT_BKPF
*      IT_PARTIDAS = IT_PARTIDAS.
*
*  CLEAR: IT_PARTIDAS_AUX.
*  MOVE IT_PARTIDAS[] TO IT_PARTIDAS_AUX[].
*  DELETE IT_PARTIDAS_AUX WHERE PARTYP NE 'C'.
*  SORT IT_PARTIDAS_AUX BY PARID.
*  DELETE ADJACENT DUPLICATES FROM IT_PARTIDAS_AUX COMPARING PARID.
*  IF IT_PARTIDAS_AUX[] IS NOT INITIAL.
*    SELECT * INTO TABLE IT_KNA1
*      FROM KNA1
*       FOR ALL ENTRIES IN IT_PARTIDAS_AUX
*     WHERE KUNNR EQ IT_PARTIDAS_AUX-PARID.
*    SORT IT_KNA1 BY KUNNR.
*  ENDIF.
*
*  CLEAR: IT_PARTIDAS_AUX.
*  MOVE IT_PARTIDAS[] TO IT_PARTIDAS_AUX[].
*  DELETE IT_PARTIDAS_AUX WHERE PARTYP NE 'V'.
*  SORT IT_PARTIDAS_AUX BY PARID.
*  DELETE ADJACENT DUPLICATES FROM IT_PARTIDAS_AUX COMPARING PARID.
*  IF IT_PARTIDAS_AUX[] IS NOT INITIAL.
*    SELECT * INTO TABLE IT_LFA1
*      FROM LFA1
*       FOR ALL ENTRIES IN IT_PARTIDAS_AUX
*     WHERE LIFNR EQ IT_PARTIDAS_AUX-PARID.
*    SORT IT_LFA1 BY LIFNR.
*  ENDIF.
*
*  MOVE IT_BSXS[] TO IT_BSXS_AUX[].
*  DELETE IT_BSXS_AUX WHERE HKONT NE '0000213052'.
*
*  CLEAR: IT_BKPF[].
*  LOOP AT IT_BSXS_AUX INTO DATA(WA_BSXS_R).
*    CLEAR: IT_BKPF.
*    IT_BKPF-GJAHR = WA_BSXS_R-GJAHR.
*    IT_BKPF-BELNR = WA_BSXS_R-BELNR.
*    IT_BKPF-BUKRS = WA_BSXS_R-BUKRS.
*    APPEND IT_BKPF.
*  ENDLOOP.
*
*  IF IT_BKPF[] IS NOT INITIAL.
*    SELECT * INTO TABLE IT_BKPF
*       FROM BKPF
*       FOR ALL ENTRIES IN IT_BKPF
*      WHERE GJAHR EQ IT_BKPF-GJAHR
*        AND BELNR EQ IT_BKPF-BELNR
*        AND BUKRS EQ IT_BKPF-BUKRS
*        AND STBLG EQ SPACE.
*  ENDIF.

*  IF IT_BKPF[] IS NOT INITIAL.
*    "Procurar Ordens de Pago
*    SELECT * INTO TABLE @DATA(IT_BSAK)
*      FROM BSAK AS K
*      FOR ALL ENTRIES IN @IT_BKPF
*     WHERE K~BUKRS EQ @IT_BKPF-BUKRS
*       AND K~AUGBL EQ @IT_BKPF-BELNR
*       AND K~AUGGJ EQ @IT_BKPF-GJAHR
*       AND K~AUGBL NE K~BELNR.
*  ENDIF.
*
*  CLEAR: IT_BKPF[].
*  LOOP AT IT_BSAK INTO DATA(WA_BSAK).
*    CLEAR: IT_BKPF.
*    IT_BKPF-GJAHR = WA_BSAK-GJAHR.
*    IT_BKPF-BELNR = WA_BSAK-BELNR.
*    IT_BKPF-BUKRS = WA_BSAK-BUKRS.
*    APPEND IT_BKPF.
*  ENDLOOP.
*
*  IF IT_BKPF[] IS NOT INITIAL.
*    SELECT * INTO TABLE IT_BKPF
*       FROM BKPF
*       FOR ALL ENTRIES IN IT_BKPF
*      WHERE GJAHR EQ IT_BKPF-GJAHR
*        AND BELNR EQ IT_BKPF-BELNR
*        AND BUKRS EQ IT_BKPF-BUKRS
*        AND STBLG EQ SPACE.
*  ENDIF.

*  CALL FUNCTION 'Z_FI_GL_PARTIDAS'
*    TABLES
*      IT_BKPF     = IT_BKPF
*      IT_PARTIDAS = IT_PARTIDAS_FOR.

END-OF-SELECTION.

  PERFORM  organiza_saida.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organiza_saida .

  DATA:  valor TYPE i.

  DATA: exc_ref    TYPE REF TO cx_sy_native_sql_error,
        error_text TYPE string.


*TP_OPERACION       1 Type  ZDE_TP_OPERACION  CHAR  1 0 Tipo Operación
*CD_NORMA           1 Type  ZDE_CODIGO_NORMA  CHAR  2 0 Código de norma
*DT_BUDAT           1 Type  BUDAT DATS  8 0 Fecha de contabilización en el documento
*TP_COMBROB         1 Type  ZDE_TP_COMBROP  CHAR  2 0 Tipo Comprob Origen de Ret. Perc.
*LT_COMPROBANTE     1 Type  ZDE_LETRA_COMPROB CHAR  1 0 Letra Comprobante
*NR_COMPROBANTE     1 Type  BELNR_D CHAR  10  0 Número de un documento contable
*DT_COMPROBANTE     1 Type  BUDAT DATS  8 0 Fecha de contabilización en el documento
*NM_MONTO_COMPRO    1 Type  ZDE_MONTO_COMPRO  CURR  13  2 Monto Comprobante
*TP_DOC             1 Type  ZDE_TP_DOC  CHAR  1 0 Tipo Doc
*NR_CUIT            1 Type  ZDE_CUIT  CHAR  11  0 CUIT
*TP_SITUACION_IIBB  1 Type  ZDE_TP_SITUACION_IIBB CHAR  1 0 Situacion IIBB Retenido
*NR_INSCRIP_IIBB    1 Type  ZDE_NR_INSCRIP_IIBB CHAR  11  0 N° Inscrip IIBB
*TP_SITUACION_IVA   1 Type  ZDE_TP_SITUACION_IVA  CHAR  1 0 Situación iva
*NM_RAZON_SOCIAL    1 Type  ZDE_RAZON_SOCIAL  CHAR  35  0 Razón Social
*NM_OTROS_CONCEPTOS 1 Type  ZDE_OTROS_CONCEP  CURR  13  2 Imp Otros conceptos
*NM_IMPORTE_IVA     1 Type  ZDE_IMPORTE_IVA CURR  13  2 Importe IVA
*NM_MONTO_SUJ_RET   1 Type  ZDE_MONTO_SUJ_RET CURR  13  2 Monto Sujeto Ret. Perc.
*NM_ALICUOTA        1 Type  ZDE_ALICUOTA  DEC 4 2 Alícuota
*NM_RET_PRACTICADA  1 Type  ZDE_RET_PRACTICADA  CURR  13  2 Ret. / Perc. Practicada
*NM_MONTO_TOTAL     1 Type  ZDE_MONTO_TOTAL_RET CURR  13  2 Monto Total Retenido

  CLEAR: it_ope_per[], it_ope_per.

*  "1: Para os casos de RETENCOES
*  LOOP AT IT_BSXS INTO DATA(WA_BSXS) WHERE HKONT EQ '0000213052'.
*
*    READ TABLE IT_BKPF_VALIDOS WITH KEY BUKRS = WA_BSXS-BUKRS GJAHR = WA_BSXS-GJAHR BELNR = WA_BSXS-BELNR TRANSPORTING NO FIELDS BINARY SEARCH.
*    IF SY-SUBRC IS NOT INITIAL.
*      CONTINUE.
*    ENDIF.
*
*    CLEAR: IT_OPE_PER.
*    IT_OPE_PER-TP_OPERACION      = '1'.
*    IT_OPE_PER-CD_NORMA          = '29'.
*    IT_OPE_PER-DT_BUDAT          = WA_BSXS-BUDAT.
*    IT_OPE_PER-TP_COMBROB        = '03'.
*    IT_OPE_PER-LT_COMPROBANTE    = ' '.
*    IT_OPE_PER-NR_COMPROBANTE    = WA_BSXS-BELNR.
*    IT_OPE_PER-DT_COMPROBANTE    = WA_BSXS-BUDAT.
*    IT_OPE_PER-NM_CERT_PROPIO    = '1'.
*    IT_OPE_PER-TP_DOC            = '3'.
*    IT_OPE_PER-TP_SITUACION_IIBB = '2'.
*    IT_OPE_PER-TP_SITUACION_IVA  = '1'.
*
*    IT_OPE_PER-NM_MONTO_COMPRO    = 0.
*    IT_OPE_PER-NM_OTROS_CONCEPTOS = 0.
*    IT_OPE_PER-NM_IMPORTE_IVA     = 0.
*    IT_OPE_PER-NM_MONTO_SUJ_RET   = 0.
*    IT_OPE_PER-NM_ALICUOTA        = 0. "FAZER FAZER FAZER FAZER FAZER FAZER FAZER
*    IT_OPE_PER-NM_RET_PRACTICADA  = 0.
*    IT_OPE_PER-NM_MONTO_TOTAL     = 0.
*
*    IT_OPE_PER-NM_RET_PRACTICADA  = WA_BSXS-DMBTR.
*    IT_OPE_PER-NM_MONTO_TOTAL     = WA_BSXS-DMBTR.
*
*    LOOP AT IT_PARTIDAS INTO DATA(WA_PARTIDAS)
*                        WHERE BUKRS  EQ WA_BSXS-BUKRS
*                          AND GJAHR  EQ WA_BSXS-GJAHR
*                          AND BELNR  EQ WA_BSXS-BELNR
*                          AND PARTYP EQ 'V'.
*
*      LOOP AT IT_WITH_ITEM INTO DATA(WA_WITH_ITEM)
*                      WHERE BUKRS EQ WA_PARTIDAS-BUKRS
*                        AND BELNR EQ WA_PARTIDAS-BELNR
*                        AND GJAHR EQ WA_PARTIDAS-GJAHR
*                        AND BUZEI EQ WA_PARTIDAS-BUZEI.
*        ADD WA_WITH_ITEM-WT_QSSHH TO IT_OPE_PER-NM_MONTO_SUJ_RET.
*        ADD WA_WITH_ITEM-WT_QSSHH TO IT_OPE_PER-NM_MONTO_COMPRO.
*      ENDLOOP.
*
*      IF IT_OPE_PER-NR_CUIT IS INITIAL.
*        READ TABLE IT_LFA1 WITH KEY LIFNR = WA_PARTIDAS-PARID BINARY SEARCH.
*        IF SY-SUBRC IS INITIAL.
*          IT_OPE_PER-NR_CUIT         = IT_LFA1-STCD1.
*          IT_OPE_PER-NR_INSCRIP_IIBB = IT_LFA1-STCD2.
*          IT_OPE_PER-NM_RAZON_SOCIAL = IT_LFA1-NAME1.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    IF IT_OPE_PER-NM_MONTO_SUJ_RET IS NOT INITIAL.
*      VALOR = ( ( IT_OPE_PER-NM_RET_PRACTICADA / IT_OPE_PER-NM_MONTO_SUJ_RET ) * 100 ) * 100.
*      TRY.
*          IT_OPE_PER-NM_ALICUOTA = VALOR / 100.
*        CATCH CX_SY_ZERODIVIDE.
*      ENDTRY.
*    ENDIF.
*
*    APPEND IT_OPE_PER.
*  ENDLOOP.

  TRY.
      EXEC SQL.
        OPEN DOCUMENTOS FOR
          SELECT TP_OPERACION,
                 CD_NORMA,
                 DT_BUDAT,
                 TP_COMBROB,
                 LT_COMPROBANTE,
                 NR_COMPROBANTE,
                 DT_COMPROBANTE,
                 SUM(NM_MONTO_COMPRO)     AS NM_MONTO_COMPRO,
                 NM_CERT_PROPIO,
                 TP_DOC,
                 NR_CUIT,
                 TP_SITUACION_IIBB,
                 NR_INSCRIP_IIBB,
                 TP_SITUACION_IVA,
                 NM_RAZON_SOCIAL,
                 SUM(NM_OTROS_CONCEPTOS) AS NM_OTROS_CONCEPTOS,
                 SUM(NM_IMPORTE_IVA)     AS NM_IMPORTE_IVA,
                 SUM(NM_MONTO_SUJ_RET)   AS NM_MONTO_SUJ_RET,
                 CASE WHEN COALESCE(SUM(NM_MONTO_SUJ_RET),0) = 0 THEN 0 ELSE COALESCE(NM_ALICUOTA,ROUND((SUM(NM_RET_PRACTICADA)*100)/SUM(NM_MONTO_SUJ_RET),2)) END AS NM_ALICUOTA,
                 SUM(NM_RET_PRACTICADA)  AS NM_RET_PRACTICADA,
                 SUM(NM_MONTO_TOTAL)     AS NM_MONTO_TOTAL
           FROM (
          SELECT '1'          AS TP_OPERACION,
                 '29'         AS CD_NORMA,
                 O.AUGDT      AS DT_BUDAT,
                 '03'         AS TP_COMBROB,
                 ''           AS LT_COMPROBANTE,
                 O.AUGBL      AS NR_COMPROBANTE,
                 O.AUGDT      AS DT_COMPROBANTE,
                 O.AUGDT      AS DT_FECHA,
                 TT.BRNCH||'-'||SUBSTR(WI.CTNUMBER,3,8) AS NM_CERT_PROPIO,
                 '3'          AS TP_DOC,
                 CASE FN.STKZN
                              WHEN 'X' THEN FN.STCD2
                              ELSE FN.STCD1 END AS NR_CUIT,
                 CASE WHEN SUBSTR(FN.STCD2,1,1) = '9' THEN '2' ELSE '1' END AS TP_SITUACION_IIBB,
                 CASE WHEN SUBSTR(FN.STCD2,1,1) = '9' THEN SUBSTR(FN.STCD2,1,3)||'0'||SUBSTR(FN.STCD2,4,11) ELSE LPAD(TRIM(FN.STCD2),11,'0') END AS NR_INSCRIP_IIBB,
                 '1'          AS TP_SITUACION_IVA,
                 FN.NAME1     AS NM_RAZON_SOCIAL,
                 WI.WT_QSSHH  AS NM_MONTO_COMPRO,
                 0            AS NM_OTROS_CONCEPTOS,
                 0            AS NM_IMPORTE_IVA,
                 WI.WT_QSSHH  AS NM_MONTO_SUJ_RET,
                 WI.WT_QBSHHA AS NM_RET_PRACTICADA,
                 WI.WT_QBSHHA AS NM_MONTO_TOTAL,
                 CASE WHEN TO_NUMBER(WI.QSATZ) = 0 THEN (SELECT DISTINCT WI2.QSATZ FROM SAPHANADB.WITH_ITEM WI2 WHERE WI2.MANDT = O.MANDT AND WI2.BUKRS = O.BUKRS AND WI2.BELNR = O.BELNR AND TO_NUMBER(WI2.QSATZ) <> 0 AND WI2.WITHT = 'CB' )
                      ELSE TO_NUMBER(WI.QSATZ)
                 END AS NM_ALICUOTA
            FROM ( SELECT DISTINCT A.MANDT, A.BUKRS, A.BELNR, K.BRNCH
                     FROM SAPHANADB.BSIS  A
                    INNER JOIN SAPHANADB.FGLV_FAGLFLEXA  B ON ( B.RBUKRS = A.BUKRS AND B.BELNR  = A.BELNR AND B.GJAHR  = A.GJAHR AND B.BUZEI  = A.BUZEI AND B.RCLNT  = A.MANDT )
                    INNER JOIN SAPHANADB.BKPF K ON ( K.MANDT = A.MANDT AND K.BUKRS = A.BUKRS AND K.BELNR  = A.BELNR AND K.GJAHR  = A.GJAHR AND K.STBLG = ' ' )
                    WHERE A.MANDT  = :SY-MANDT
                      AND A.BUKRS  = :PBUKRS-LOW
                      AND A.BUDAT >= :E_FIRST_DAY
                      AND A.BUDAT <= :E_LAST_DAY
                      AND B.BUDAT >= :E_FIRST_DAY
                      AND B.BUDAT <= :E_LAST_DAY
                      AND A.HKONT IN ('0000213052','0000213055')
                      AND B.RLDNR = '0L'
                      AND B.RACCT IN ('0000213052','0000213055')
                      AND ( B.BSTAT = ' ' OR B.BSTAT = 'L' ) ) TT,
                 SAPHANADB.BSAK      O,
                 SAPHANADB.WITH_ITEM WI,
                 SAPHANADB.LFA1      FN
           WHERE TT.MANDT = WI.MANDT
             AND TT.BUKRS = WI.BUKRS
             AND TT.BELNR = WI.BELNR
             AND WI.MANDT = O.MANDT
             AND WI.BUKRS = O.BUKRS
             AND WI.BELNR = O.BELNR
             AND WI.BUZEI = O.BUZEI
             AND WI.WITHT = 'CB'
             AND WI.WT_QSSHH <> 0
             AND O.LIFNR  = FN.LIFNR ) TT
      GROUP BY TP_OPERACION,
             CD_NORMA,
             DT_BUDAT,
             TP_COMBROB,
             LT_COMPROBANTE,
             NR_COMPROBANTE,
             DT_COMPROBANTE,
             NM_CERT_PROPIO,
             TP_DOC,
             NR_CUIT,
             TP_SITUACION_IIBB,
             NR_INSCRIP_IIBB,
             TP_SITUACION_IVA,
             NM_RAZON_SOCIAL,
             NM_ALICUOTA
       ORDER BY 3, 6
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  CLEAR: it_ope_per.

  DO.
    EXEC SQL.
      FETCH NEXT DOCUMENTOS INTO
        :IT_OPE_PER-TP_OPERACION,
        :IT_OPE_PER-CD_NORMA,
        :IT_OPE_PER-DT_BUDAT,
        :IT_OPE_PER-TP_COMBROB,
        :IT_OPE_PER-LT_COMPROBANTE,
        :IT_OPE_PER-NR_COMPROBANTE,
        :IT_OPE_PER-DT_COMPROBANTE,
        :IT_OPE_PER-NM_MONTO_COMPRO,
        :IT_OPE_PER-NM_CERT_PROPIO,
        :IT_OPE_PER-TP_DOC,
        :IT_OPE_PER-NR_CUIT,
        :IT_OPE_PER-TP_SITUACION_IIBB,
        :IT_OPE_PER-NR_INSCRIP_IIBB,
        :IT_OPE_PER-TP_SITUACION_IVA,
        :IT_OPE_PER-NM_RAZON_SOCIAL,
        :IT_OPE_PER-NM_OTROS_CONCEPTOS,
        :IT_OPE_PER-NM_IMPORTE_IVA,
        :IT_OPE_PER-NM_MONTO_SUJ_RET,
        :IT_OPE_PER-NM_ALICUOTA,
        :IT_OPE_PER-NM_RET_PRACTICADA,
        :IT_OPE_PER-NM_MONTO_TOTAL
    ENDEXEC.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    APPEND it_ope_per.
    CLEAR: it_ope_per.
  ENDDO.

  EXEC SQL.
    CLOSE DOCUMENTOS
  ENDEXEC.

  "2: Para os casos de PERCEPCOES
*  LOOP AT IT_BSXS INTO DATA(WA_BSXS_2) WHERE HKONT EQ '0000213051'.
*
*    READ TABLE IT_BKPF_VALIDOS WITH KEY BUKRS = WA_BSXS_2-BUKRS GJAHR = WA_BSXS_2-GJAHR BELNR = WA_BSXS_2-BELNR TRANSPORTING NO FIELDS BINARY SEARCH.
*    IF SY-SUBRC IS NOT INITIAL.
*      CONTINUE.
*    ENDIF.
*
*    CLEAR: IT_OPE_PER.
*
*    IT_OPE_PER-TP_OPERACION      = '2'.
*    IT_OPE_PER-CD_NORMA          = '29'.
*    IT_OPE_PER-DT_BUDAT          = WA_BSXS_2-BUDAT.
*    IT_OPE_PER-TP_COMBROB        = '01'.
*    IT_OPE_PER-LT_COMPROBANTE    = 'A'.
*
*    SPLIT WA_BSXS_2-XBLNR AT 'A' INTO: DATA(STR1_2) DATA(STR2_2).
*    CONCATENATE STR1_2 STR2_2 INTO IT_OPE_PER-NR_COMPROBANTE.
*
*    IT_OPE_PER-DT_COMPROBANTE    = WA_BSXS_2-BUDAT.
*    IT_OPE_PER-NM_CERT_PROPIO    = ' '.
*    IT_OPE_PER-TP_DOC            = '3'.
*    IT_OPE_PER-TP_SITUACION_IIBB = '2'.
*    IT_OPE_PER-TP_SITUACION_IVA  = '1'.
*
*    IT_OPE_PER-NM_MONTO_COMPRO    = 0.
*    IT_OPE_PER-NM_OTROS_CONCEPTOS = 0.
*    IT_OPE_PER-NM_IMPORTE_IVA     = 0.
*    IT_OPE_PER-NM_MONTO_SUJ_RET   = 0.
*    IT_OPE_PER-NM_ALICUOTA        = 0. "FAZER FAZER FAZER FAZER FAZER FAZER FAZER
*    IT_OPE_PER-NM_RET_PRACTICADA  = 0.
*    IT_OPE_PER-NM_MONTO_TOTAL     = 0.
*
*    LOOP AT IT_PARTIDAS INTO DATA(WA_PARTIDAS_2)
*                        WHERE BUKRS EQ WA_BSXS_2-BUKRS
*                          AND GJAHR EQ WA_BSXS_2-GJAHR
*                          AND BELNR EQ WA_BSXS_2-BELNR.
*      CASE WA_PARTIDAS_2-PARTYP.
*        WHEN 'B'.
*          IF WA_PARTIDAS_2-HKONT EQ '0000213011'.
*            ADD WA_PARTIDAS_2-DMBTR TO IT_OPE_PER-NM_IMPORTE_IVA.
*          ENDIF.
*          IF WA_PARTIDAS_2-HKONT(5) EQ '00003'.
*            ADD WA_PARTIDAS_2-DMBTR TO IT_OPE_PER-NM_MONTO_SUJ_RET.
*          ENDIF.
*          IF WA_PARTIDAS_2-HKONT EQ '0000213051'.
*            ADD WA_PARTIDAS_2-DMBTR TO IT_OPE_PER-NM_RET_PRACTICADA.
*            ADD WA_PARTIDAS_2-DMBTR TO IT_OPE_PER-NM_MONTO_TOTAL.
*          ENDIF.
*        WHEN 'C'.
*          ADD WA_PARTIDAS_2-DMBTR TO IT_OPE_PER-NM_MONTO_COMPRO.
*
*          IF IT_OPE_PER-NR_CUIT IS INITIAL.
*            READ TABLE IT_KNA1 WITH KEY KUNNR = WA_PARTIDAS_2-PARID BINARY SEARCH.
*            IF SY-SUBRC IS INITIAL.
*              IT_OPE_PER-NR_CUIT         = IT_KNA1-STCD1.
*              IT_OPE_PER-NR_INSCRIP_IIBB = IT_KNA1-STCD1.
*              IT_OPE_PER-NM_RAZON_SOCIAL = IT_KNA1-NAME1.
*            ENDIF.
*          ENDIF.
*
*        WHEN 'V'.
*          ADD WA_PARTIDAS_2-DMBTR TO IT_OPE_PER-NM_MONTO_COMPRO.
*
*          IF IT_OPE_PER-NR_CUIT IS INITIAL.
*            READ TABLE IT_LFA1 WITH KEY LIFNR = WA_PARTIDAS_2-PARID BINARY SEARCH.
*            IF SY-SUBRC IS INITIAL.
*              IT_OPE_PER-NR_CUIT         = IT_LFA1-STCD1.
*              IT_OPE_PER-NR_INSCRIP_IIBB = IT_LFA1-STCD1.
*              IT_OPE_PER-NM_RAZON_SOCIAL = IT_LFA1-NAME1.
*            ENDIF.
*          ENDIF.
*      ENDCASE.
*    ENDLOOP.
*
*    IT_OPE_PER-NM_MONTO_COMPRO = IT_OPE_PER-NM_MONTO_COMPRO - IT_OPE_PER-NM_MONTO_TOTAL.
*
*    IF IT_OPE_PER-NM_MONTO_SUJ_RET IS NOT INITIAL.
*      VALOR = ( ( IT_OPE_PER-NM_RET_PRACTICADA / IT_OPE_PER-NM_MONTO_SUJ_RET ) * 100 ) * 100.
*      TRY.
*          IT_OPE_PER-NM_ALICUOTA = VALOR / 100.
*        CATCH CX_SY_ZERODIVIDE.
*      ENDTRY.
*    ENDIF.
*
*    APPEND IT_OPE_PER.
*  ENDLOOP.

  TRY.
      EXEC SQL.
        OPEN PERCEPCIONES FOR
          WITH DOCUMENTOS AS (
          SELECT A.RCLNT AS MANDT, A.RBUKRS AS BUKRS, A.BELNR, A.GJAHR, B.MWSKZ AS IVA
            FROM SAPHANADB.FGLV_FAGLFLEXA A,
                 SAPHANADB.BSAD      B
           WHERE A.RCLNT  = :SY-MANDT
             AND A.RYEAR  = :PGJAHR-LOW
             AND A.RLDNR  = '0L'
             AND A.RBUKRS = :PBUKRS-LOW
             AND A.POPER  = :PPOPER-LOW
             AND A.RACCT  IN ( '0000213051', '0000213054' )
             AND (A.BSTAT = ' ' OR A.BSTAT = 'L')
             AND A.RCLNT  = B.MANDT
             AND A.RBUKRS = B.BUKRS
             AND A.BELNR  = B.BELNR
             AND EXISTS ( SELECT * FROM SAPHANADB.BKPF  KK WHERE KK.MANDT = A.RCLNT AND KK.BUKRS = A.RBUKRS AND KK.BELNR = A.BELNR AND KK.GJAHR = A.GJAHR AND KK.STBLG = ' ' ) )

          SELECT TP_OPERACION,
                 CD_NORMA,
                 DT_BUDAT,
                 TP_COMBROB,
                 LT_COMPROBANTE,
                 NR_COMPROBANTE,
                 DT_COMPROBANTE,
                 NM_MONTO_COMPRO,
                 NM_CERT_PROPIO,
                 TP_DOC,
                 NR_CUIT,
                 TP_SITUACION_IIBB,
                 NR_INSCRIP_IIBB,
                 TP_SITUACION_IVA,
                 NM_RAZON_SOCIAL,
                 NM_OTROS_CONCEPTOS,
                 NM_IMPORTE_IVA,
                 NM_MONTO_SUJ_RET,
                 CASE WHEN NM_ALICUOTA is null AND NM_MONTO_SUJ_RET > 0 THEN ROUND((NM_RET_PRACTICADA*100)/NM_MONTO_SUJ_RET,2) ELSE NM_ALICUOTA END AS NM_ALICUOTA,
                 NM_RET_PRACTICADA,
                 NM_MONTO_TOTAL
          FROM (
          SELECT '2'      AS TP_OPERACION,
                 '29'     AS CD_NORMA,
                 TT.BUDAT AS DT_BUDAT,
                 '01'     AS TP_COMBROB,
                 'A'      AS LT_COMPROBANTE,
                 SUBSTR(TT.XBLNR,1,4)||SUBSTR(TT.XBLNR,6,8) AS NR_COMPROBANTE,
                 TT.BUDAT AS DT_COMPROBANTE,
                 '0' AS NM_CERT_PROPIO,
                 '3' AS TP_DOC,
                 '1' AS TP_SITUACION_IVA,
                 SUM( VALOR_DOC ) - SUM( NM_RET_PRACTICADA ) AS NM_MONTO_COMPRO,
                 0 AS NM_OTROS_CONCEPTOS,
                 SUM( NM_IMPORTE_IVA )    AS NM_IMPORTE_IVA,
                 SUM( NM_MONTO_SUJ_RET )  AS NM_MONTO_SUJ_RET,
                 SUM( NM_RET_PRACTICADA )  AS NM_RET_PRACTICADA,
                 SUM( NM_MONTO_TOTAL )     AS NM_MONTO_TOTAL,
                 SUBSTR(ID_PARCEIRO,12,30) AS NM_RAZON_SOCIAL,
                 SUBSTR(ID_PARCEIRO,42,20) AS NR_CUIT,
                 CASE WHEN SUBSTR(TRIM(SUBSTR(ID_PARCEIRO,61,20)),1,1) = '9' THEN '2' ELSE '1' END AS TP_SITUACION_IIBB,
                 CASE WHEN SUBSTR(TRIM(SUBSTR(ID_PARCEIRO,61,20)),1,1) = '9' THEN SUBSTR(TRIM(SUBSTR(ID_PARCEIRO,61,20)),1,3)||'0'||SUBSTR(TRIM(SUBSTR(ID_PARCEIRO,61,20)),4,11)
                      ELSE LPAD(TRIM(TRIM(SUBSTR(ID_PARCEIRO,61,20))),11,'0') END AS NR_INSCRIP_IIBB,
                 ( SELECT TO_NUMBER(REPLACE(TRIM(T.DESCRIPT),',',
                                                             '.')) AS NM_ALICUOTA
                     FROM SAPHANADB.SETLEAF  O ,
                          SAPHANADB.SETLINET T
                    WHERE O.MANDT    = '300'
                      AND O.SETNAME LIKE 'MAGGI_0100_IVA'
                      AND O.MANDT    = T.MANDT
                      AND O.SETCLASS = T.SETCLASS
                      AND O.SUBCLASS = T.SUBCLASS
                      AND O.SETNAME  = T.SETNAME
                      AND O.LINEID   = T.LINEID
                      AND T.LANGU    = 'P'
                      AND O.VALFROM  = TT.IVA ) AS NM_ALICUOTA
            FROM (
          SELECT AD.BUKRS, AD.BELNR, AD.BUDAT, AD.GJAHR, AD.HKONT, AD.XBLNR,
                 CASE WHEN AD.HKONT    IN ('0000213011','0000213012')     THEN AD.DMBTR ELSE 0 END AS NM_IMPORTE_IVA,
                 CASE WHEN SUBSTR(AD.HKONT,1,5) IN ('00003') THEN AD.DMBTR ELSE 0 END AS NM_MONTO_SUJ_RET,
                 CASE WHEN AD.HKONT IN ( '0000213051','0000213054','0000113251') THEN AD.DMBTR ELSE 0 END AS NM_RET_PRACTICADA,
                 CASE WHEN AD.HKONT IN ( '0000213051','0000213054','0000113251') THEN AD.DMBTR ELSE 0 END AS NM_MONTO_TOTAL,
                 CASE WHEN EXISTS ( SELECT * FROM SAPHANADB.BSAK AC WHERE AC.MANDT = AD.MANDT AND AC.BUKRS = AD.BUKRS AND AC.BELNR = AD.BELNR AND AC.GJAHR = AD.GJAHR ) THEN
                        ( SELECT DISTINCT
                              CASE LF.STKZN
                                    WHEN 'X'
                                        THEN 'F'||AC.LIFNR||RPAD(LF.NAME1,30)||RPAD(LF.STCD2,20)||RPAD(LF.STCD2,20)
                                    ELSE
                                      'F'||AC.LIFNR||RPAD(LF.NAME1,30)||RPAD(LF.STCD1,20)||RPAD(LF.STCD2,20)
                                  END AS ID_PARCEIRO
                            FROM SAPHANADB.BSAK AC,
                                 SAPHANADB.LFA1 LF
                           WHERE AC.MANDT = AD.MANDT AND AC.BUKRS = AD.BUKRS AND AC.BELNR = AD.BELNR AND AC.GJAHR = AD.GJAHR
                             AND AC.MANDT = LF.MANDT
                             AND AC.LIFNR = LF.LIFNR )
                      ELSE
                        ( SELECT DISTINCT
                              CASE KN.STKZN
                                    WHEN 'X'
                                        THEN 'C'||AC.KUNNR||RPAD(KN.NAME1,30)||RPAD(KN.STCD2,20)||RPAD(KN.STCD2,20)
                                    ELSE
                                        'C'||AC.KUNNR||RPAD(KN.NAME1,30)||RPAD(KN.STCD1,20)||RPAD(KN.STCD2,20)
                                  END AS ID_PARCEIRO
                            FROM SAPHANADB.BSAD AC ,
                                 SAPHANADB.KNA1 KN
                           WHERE AC.MANDT = AD.MANDT AND AC.BUKRS = AD.BUKRS AND AC.BELNR = AD.BELNR AND AC.GJAHR = AD.GJAHR
                             AND AC.MANDT = KN.MANDT
                             AND AC.KUNNR = KN.KUNNR )
                 END AS ID_PARCEIRO,
                 COALESCE(
                 CASE WHEN EXISTS ( SELECT * FROM SAPHANADB.BSAK AC WHERE AC.MANDT = AD.MANDT AND AC.BUKRS = AD.BUKRS AND AC.BELNR = AD.BELNR AND AC.GJAHR = AD.GJAHR AND AC.BUZEI = AD.BUZEI ) THEN
                        ( SELECT AC.DMBTR FROM SAPHANADB.BSAK AC WHERE AC.MANDT = AD.MANDT AND AC.BUKRS = AD.BUKRS AND AC.BELNR = AD.BELNR AND AC.GJAHR = AD.GJAHR AND AC.BUZEI = AD.BUZEI )
                      ELSE
                        ( SELECT AC.DMBTR FROM SAPHANADB.BSAD AC WHERE AC.MANDT = AD.MANDT AND AC.BUKRS = AD.BUKRS AND AC.BELNR = AD.BELNR AND AC.GJAHR = AD.GJAHR AND AC.BUZEI = AD.BUZEI )
                 END,0) AS VALOR_DOC,
                 DC.IVA
            FROM DOCUMENTOS  DC,
                 SAPHANADB.BSAS AD
           WHERE DC.MANDT = AD.MANDT
             AND DC.BUKRS = AD.BUKRS
             AND DC.BELNR = AD.BELNR
             AND DC.GJAHR = AD.GJAHR
           UNION ALL
          SELECT ID.BUKRS, ID.BELNR, ID.BUDAT, ID.GJAHR, ID.HKONT, ID.XBLNR,
                 CASE WHEN ID.HKONT IN ('0000213011','0000113243')  THEN ID.DMBTR ELSE 0 END AS NM_IMPORTE_IVA,
                 CASE WHEN SUBSTR(ID.HKONT,1,5) IN ('00003') THEN ID.DMBTR ELSE 0 END AS NM_MONTO_SUJ_RET,
                 CASE WHEN ID.HKONT IN ( '0000213051','0000213054', '0000113251')  THEN ID.DMBTR ELSE 0 END AS NM_RET_PRACTICADA,
                 CASE WHEN ID.HKONT IN ( '0000213051','0000213054', '0000113251')  THEN ID.DMBTR ELSE 0 END AS NM_MONTO_TOTAL,
                 CASE WHEN EXISTS ( SELECT * FROM SAPHANADB.BSIK AC WHERE AC.MANDT = ID.MANDT AND AC.BUKRS = ID.BUKRS AND AC.BELNR = ID.BELNR AND AC.GJAHR = ID.GJAHR ) THEN
                        ( SELECT DISTINCT
                                  CASE LF.STKZN
                                    WHEN 'X'
                                        THEN 'F'||AC.LIFNR||RPAD(LF.NAME1,30)||RPAD(LF.STCD2,20)||RPAD(LF.STCD2,20)
                                    ELSE
                                      'F'||AC.LIFNR||RPAD(LF.NAME1,30)||RPAD(LF.STCD1,20)||RPAD(LF.STCD2,20)
                                  END AS ID_PARCEIRO
                            FROM SAPHANADB.BSAK AC,
                                 SAPHANADB.LFA1 LF
                           WHERE AC.MANDT = ID.MANDT AND AC.BUKRS = ID.BUKRS AND AC.BELNR = ID.BELNR AND AC.GJAHR = ID.GJAHR
                             AND AC.MANDT = LF.MANDT
                             AND AC.LIFNR = LF.LIFNR )
                      ELSE
                        ( SELECT DISTINCT
                             CASE KN.STKZN
                                    WHEN 'X'
                                        THEN 'C'||AC.KUNNR||RPAD(KN.NAME1,30)||RPAD(KN.STCD2,20)||RPAD(KN.STCD2,20)
                                    ELSE
                                        'C'||AC.KUNNR||RPAD(KN.NAME1,30)||RPAD(KN.STCD1,20)||RPAD(KN.STCD2,20)
                                  END AS ID_PARCEIRO
                            FROM SAPHANADB.BSAD AC,
                                 SAPHANADB.KNA1 KN
                           WHERE AC.MANDT = ID.MANDT AND AC.BUKRS = ID.BUKRS AND AC.BELNR = ID.BELNR AND AC.GJAHR = ID.GJAHR
                             AND AC.MANDT = KN.MANDT
                             AND AC.KUNNR = KN.KUNNR )
                 END AS ID_PARCEIRO,
                 COALESCE(
                 CASE WHEN EXISTS ( SELECT * FROM SAPHANADB.BSIK AC WHERE AC.MANDT = ID.MANDT AND AC.BUKRS = ID.BUKRS AND AC.BELNR = ID.BELNR AND AC.GJAHR = ID.GJAHR AND AC.BUZEI = ID.BUZEI ) THEN
                        ( SELECT AC.DMBTR FROM SAPHANADB.BSAK AC WHERE AC.MANDT = ID.MANDT AND AC.BUKRS = ID.BUKRS AND AC.BELNR = ID.BELNR AND AC.GJAHR = ID.GJAHR AND AC.BUZEI = ID.BUZEI )
                      ELSE
                        ( SELECT AC.DMBTR FROM SAPHANADB.BSAD AC WHERE AC.MANDT = ID.MANDT AND AC.BUKRS = ID.BUKRS AND AC.BELNR = ID.BELNR AND AC.GJAHR = ID.GJAHR AND AC.BUZEI = ID.BUZEI )
                 END,0) AS VALOR_DOC,
                 DC.IVA
            FROM DOCUMENTOS  DC,
                 SAPHANADB.BSIS ID
           WHERE DC.MANDT = ID.MANDT
             AND DC.BUKRS = ID.BUKRS
             AND DC.BELNR = ID.BELNR
             AND DC.GJAHR = ID.GJAHR  ) TT
            WHERE 1 = 1
              AND TT.ID_PARCEIRO <> ''
              AND TRIM(SUBSTR(ID_PARCEIRO,42,20)) <> '30713700505'
          GROUP BY TT.BUDAT, TT.BELNR, TT.XBLNR, ID_PARCEIRO, TT.IVA
           ) TT
           ORDER BY 3, 6
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  CLEAR: it_ope_per.

  DO.

*** Stefanini - IR242165 - 06/06/2025 - GGARAUJO1 - Inicio de alteração
    TRY.
*** Stefanini - IR242165 - 06/06/2025 - GGARAUJO1 - Fim de alteração

    EXEC SQL.
      FETCH NEXT PERCEPCIONES INTO
        :IT_OPE_PER-TP_OPERACION,
        :IT_OPE_PER-CD_NORMA,
        :IT_OPE_PER-DT_BUDAT,
        :IT_OPE_PER-TP_COMBROB,
        :IT_OPE_PER-LT_COMPROBANTE,
        :IT_OPE_PER-NR_COMPROBANTE,
        :IT_OPE_PER-DT_COMPROBANTE,
        :IT_OPE_PER-NM_MONTO_COMPRO,
        :IT_OPE_PER-NM_CERT_PROPIO,
        :IT_OPE_PER-TP_DOC,
        :IT_OPE_PER-NR_CUIT,
        :IT_OPE_PER-TP_SITUACION_IIBB,
        :IT_OPE_PER-NR_INSCRIP_IIBB,
        :IT_OPE_PER-TP_SITUACION_IVA,
        :IT_OPE_PER-NM_RAZON_SOCIAL,
        :IT_OPE_PER-NM_OTROS_CONCEPTOS,
        :IT_OPE_PER-NM_IMPORTE_IVA,
        :IT_OPE_PER-NM_MONTO_SUJ_RET,
        :IT_OPE_PER-NM_ALICUOTA,
        :IT_OPE_PER-NM_RET_PRACTICADA,
        :IT_OPE_PER-NM_MONTO_TOTAL
    ENDEXEC.

*** Stefanini - IR242165 - 06/06/2025 - GGARAUJO1 - Inicio de alteração
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
    ENDTRY.
*** Stefanini - IR242165 - 06/06/2025 - GGARAUJO1 - Fim de alteração

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    APPEND it_ope_per.
    CLEAR: it_ope_per.
  ENDDO.

  EXEC SQL.
    CLOSE PERCEPCIONES
  ENDEXEC.

ENDFORM.
