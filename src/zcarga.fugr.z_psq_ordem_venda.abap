FUNCTION Z_PSQ_ORDEM_VENDA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ID_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_ID_BRANCH) TYPE  J_1BBRANC_
*"     REFERENCE(I_NR_SAFRA) TYPE  ZDE_NR_SAFRA
*"  EXPORTING
*"     REFERENCE(E_VBELN) TYPE  ZDE_ORDEM_VENDA_PSQ
*"  EXCEPTIONS
*"      ERRO
*"----------------------------------------------------------------------

  DATA: EXC_REF    TYPE REF TO CX_SY_NATIVE_SQL_ERROR,
        ERROR_TEXT TYPE STRING.

  CLEAR: IT_ORDENS[], IT_ORDENS, WA_ORDENS.

  TRY.
      EXEC SQL.
        OPEN DOCUMENTOS FOR
          SELECT TT.VBELN,
                 TT.ARKTX,
                 TT.MEINS,
                 TT.INCO1,
                 TT.ROUTE,
                 TT.QUANTIDADE,
                 TT.REMESSA,
                 TT.QUANTIDADE - TT.REMESSA AS SALDO,
                 TT.REMESSA_ERRO,
                 (select C.NAME1
                    from SAPHANADB.vbpa o,
                         SAPHANADB.kna1 C
                   where o.mandt = TT.MANDT
                     AND O.VBELN = TT.VBELN
                     AND O.PARVW = 'LR'
                     AND O.MANDT = C.MANDT
                     AND O.KUNNR = C.KUNNR ) AS LOCAL_DESCARGA_TRANSBORDO,
                 (select C.NAME1
                    from SAPHANADB.vbpa o,
                         SAPHANADB.LFA1 C
                   where o.mandt = TT.MANDT
                     AND O.VBELN = TT.VBELN
                     AND O.PARVW = 'Z1'
                     AND O.MANDT = C.MANDT
                     AND O.LIFNR = C.LIFNR ) AS LOCAL_DESTINO,
                 (select C.NAME1
                    from SAPHANADB.vbpa o,
                         SAPHANADB.lfa1 C
                   where o.mandt = TT.MANDT
                     AND O.VBELN = TT.VBELN
                     AND O.PARVW = 'PC'
                     AND O.MANDT = C.MANDT
                     AND O.LIFNR = C.LIFNR ) AS PONTO_COLETA
            FROM (
          SELECT OV.MANDT,
                 OV.VBELN,
                 OV.MATNR,
                 OV.ARKTX,
                 OV.PSTYV,
                 OV.MEINS,
                 OV.KWMENG,
                 VK.INCO1,
                 OV.LGORT,
                 OV.ROUTE,
                 SUM( WMENG ) AS QUANTIDADE,
                 COALESCE( SUM( (SELECT SUM( LP.LFIMG )
                              FROM SAPHANADB.LIPS LP
                             WHERE LP.MANDT = OV.MANDT
                               AND LP.VGBEL = OV.VBELN
                               AND LP.VGPOS = OV.POSNR ) ),0) AS REMESSA,

                 COALESCE( SUM( (SELECT SUM( LP.LFIMG )
                              FROM SAPHANADB.LIPS LP
                             WHERE LP.MANDT = OV.MANDT
                               AND LP.VGBEL = OV.VBELN
                               AND LP.VGPOS = OV.POSNR
                               AND EXISTS ( SELECT *
                                              FROM SAPHANADB.VBFA VF
                                             WHERE VF.MANDT = LP.MANDT
                                               AND VF.VBELV = LP.VBELN
                                               AND VF.POSNV = LP.POSNR
                                               AND VF.VBTYP_V = 'J'
                                               AND VF.VBTYP_N = 'h' ) ) ),0) AS REMESSA_ERRO
            FROM SAPHANADB.VBAP OV,
                 SAPHANADB.VBKD VK,
                 SAPHANADB.VBEP BE
           WHERE OV.MANDT = :SY-MANDT
             AND OV.CHARG = :I_NR_SAFRA
             AND OV.MANDT = VK.MANDT
             AND OV.VBELN = VK.VBELN
             AND OV.POSNR = VK.POSNR
             AND VK.INCO1 IN ('CIF','CPT','CFR')
             AND OV.MANDT = BE.MANDT
             AND OV.VBELN = BE.VBELN
             AND OV.POSNR = BE.POSNR
             AND OV.WERKS = :I_ID_BRANCH
          GROUP BY
                 OV.MANDT,
                 OV.VBELN,
                 OV.MATNR,
                 OV.ARKTX,
                 OV.PSTYV,
                 OV.MEINS,
                 OV.KWMENG,
                 VK.INCO1,
                 OV.LGORT,
                 OV.ROUTE ) TT
           WHERE ( TT.QUANTIDADE - TT.REMESSA <> 0 ) OR TT.REMESSA_ERRO > 0
      ENDEXEC.
    CATCH CX_SY_NATIVE_SQL_ERROR INTO EXC_REF.
      ERROR_TEXT = EXC_REF->GET_TEXT( ).
      MESSAGE ERROR_TEXT TYPE 'E' RAISING ERRO.
  ENDTRY.

  DO.
    EXEC SQL.
      FETCH NEXT DOCUMENTOS INTO
      :WA_ORDENS-VBELN,
      :WA_ORDENS-MAKTX,
      :WA_ORDENS-MEINS,
      :WA_ORDENS-INCO1,
      :WA_ORDENS-ROUTE,
      :WA_ORDENS-QUANTIDADE,
      :WA_ORDENS-UTILIZADO,
      :WA_ORDENS-SALDO,
      :WA_ORDENS-REMESSA_ESTORNADA,
      :WA_ORDENS-NM_LOCAL_DESCARGA,
      :WA_ORDENS-NM_LOCAL_DESTINO,
      :WA_ORDENS-NM_LOCAL_COLETA
    ENDEXEC.
    IF SY-SUBRC <> 0.
      EXIT.
    ELSE.
      APPEND WA_ORDENS TO IT_ORDENS.
    ENDIF.
  ENDDO.

  EXEC SQL.
    CLOSE DOCUMENTOS
  ENDEXEC.

  "146  Não Encontrado Ordens de Venda Com Saldo
  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE E166(ZCARGA) WITH I_ID_BUKRS I_ID_BRANCH I_NR_SAFRA RAISING ERRO.
  ENDIF.

  LOOP AT IT_ORDENS ASSIGNING FIELD-SYMBOL(<FS_LINE>).
    IF <FS_LINE>-REMESSA_ESTORNADA IS INITIAL.
      <FS_LINE>-ICO_ORDEM = ICON_LED_GREEN.
    ELSE.
      <FS_LINE>-ICO_ORDEM = ICON_ALERT.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_ORDENS LINES DATA(QTD_OVS).

  IF QTD_OVS EQ 1.
    READ TABLE IT_ORDENS INTO WA_ORDENS INDEX 1.
    IF E_VBELN-REMESSA_ESTORNADA IS NOT INITIAL.
      PID_BUKRS      = I_ID_BUKRS.
      PID_BRANCH     = I_ID_BRANCH.
      PNR_SAFRA      = I_NR_SAFRA.
      CK_SELECIONOU  = ABAP_FALSE.
      CALL SCREEN 9002 STARTING AT 10 05.
      IF CK_SELECIONOU EQ ABAP_TRUE.
        MOVE-CORRESPONDING WA_ORDENS TO E_VBELN.
      ENDIF.
    ELSE.
      MOVE-CORRESPONDING WA_ORDENS TO E_VBELN.
    ENDIF.
  ELSE.
    PID_BUKRS      = I_ID_BUKRS.
    PID_BRANCH     = I_ID_BRANCH.
    PNR_SAFRA      = I_NR_SAFRA.
    CK_SELECIONOU  = ABAP_FALSE.
    CALL SCREEN 9002 STARTING AT 10 05.
    IF CK_SELECIONOU EQ ABAP_TRUE.
      MOVE-CORRESPONDING WA_ORDENS TO E_VBELN.
    ENDIF.
  ENDIF.

ENDFUNCTION.
