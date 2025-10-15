*----------------------------------------------------------------------*
***INCLUDE ZFIR0055_PAI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.
  CASE SY-UCOMM.
    WHEN 'CONFIRM'.
      PERFORM ATRIB_CLASSIFICAO_FLX.
      PERFORM RENOVAR.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE USER_COMMAND_0102 INPUT.
  DATA: WL_0121      TYPE ZFIT0121,
        WL_0080      TYPE ZFIT0080,
        WL_BSAK      TYPE BSAK,
        WL_BSIK      TYPE BSIK,
        WL_BSAD      TYPE BSAD,
        WL_BSID      TYPE BSID,
        WL_BSAS      TYPE BSAS,
        WL_BSIS      TYPE BSIS,
        WL_BKPF      TYPE BKPF,
        WL_FLEXA     TYPE FAGLFLEXA,
        WL_0078      TYPE ZFIT0078,
        VL_VALIDA    TYPE C.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      CLEAR: WL_0121, WL_0080.

      MOVE-CORRESPONDING ZFIT0121 TO WL_0121.
      CLEAR: ZFIT0121.
      MOVE: WL_0121-BUKRS       TO ZFIT0121-BUKRS,
            WL_0121-BELNR       TO ZFIT0121-BELNR,
            WL_0121-GJAHR       TO ZFIT0121-GJAHR,
            WL_0121-BUZEI       TO ZFIT0121-BUZEI,
            WL_0121-KOART       TO ZFIT0121-KOART,
            WL_0121-LCTO_COMP   TO ZFIT0121-LCTO_COMP,
            WL_0121-COD_FLX     TO ZFIT0121-COD_FLX.

      CLEAR: WL_0121.

      PERFORM VALIDA_INFO_0102 USING VL_VALIDA 'I'.
      CHECK VL_VALIDA IS NOT INITIAL.

      SELECT SINGLE *
        FROM ZFIT0080 INTO WL_0080
       WHERE BUKRS = ZFIT0121-BUKRS
         AND BELNR = ZFIT0121-BELNR
         AND GJAHR = ZFIT0121-GJAHR
         AND BUZEI = ZFIT0121-BUZEI.

      IF SY-SUBRC EQ 0.
        MESSAGE 'Documento já classificado!' TYPE 'W'.
        RETURN.
      ENDIF.

      CLEAR: WL_0080.

      CASE ZFIT0121-KOART.
        WHEN 'D'. "Cliente

          SELECT SINGLE *
            FROM BSID INTO WL_BSID
           WHERE BUKRS = ZFIT0121-BUKRS
             AND BELNR = ZFIT0121-BELNR
             AND GJAHR = ZFIT0121-GJAHR
             AND BUZEI = ZFIT0121-BUZEI.

          IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING WL_BSID TO ZFIT0121.
            ZFIT0121-AUGBL = WL_BSID-BELNR.
            ZFIT0121-AUGDT = WL_BSID-BUDAT.
          ELSE.
            SELECT SINGLE *
              FROM BSAD INTO WL_BSAD
             WHERE BUKRS = ZFIT0121-BUKRS
               AND BELNR = ZFIT0121-BELNR
               AND GJAHR = ZFIT0121-GJAHR
               AND BUZEI = ZFIT0121-BUZEI.

            IF SY-SUBRC NE 0.
              MESSAGE 'Documento não encontrado!' TYPE 'W'.
              RETURN.
            ENDIF.

            MOVE-CORRESPONDING WL_BSAD TO ZFIT0121.
          ENDIF.

        WHEN 'K'. "Fornecedor

          SELECT SINGLE *
            FROM BSIK INTO WL_BSIK
           WHERE BUKRS = ZFIT0121-BUKRS
             AND BELNR = ZFIT0121-BELNR
             AND GJAHR = ZFIT0121-GJAHR
             AND BUZEI = ZFIT0121-BUZEI.

          IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING WL_BSIK TO ZFIT0121.
            ZFIT0121-AUGBL = WL_BSIK-BELNR.
            ZFIT0121-AUGDT = WL_BSIK-BUDAT.
          ELSE.
            SELECT SINGLE *
              FROM BSAK INTO WL_BSAK
             WHERE BUKRS = ZFIT0121-BUKRS
               AND BELNR = ZFIT0121-BELNR
               AND GJAHR = ZFIT0121-GJAHR
               AND BUZEI = ZFIT0121-BUZEI.

            IF SY-SUBRC NE 0.
              MESSAGE 'Documento não encontrado!' TYPE 'W'.
              RETURN.
            ENDIF.

            MOVE-CORRESPONDING WL_BSAK TO ZFIT0121.
          ENDIF.

        WHEN 'S'. "Razão

          SELECT SINGLE *
            FROM BSIS INTO WL_BSIS
           WHERE BUKRS = ZFIT0121-BUKRS
             AND BELNR = ZFIT0121-BELNR
             AND GJAHR = ZFIT0121-GJAHR
             AND BUZEI = ZFIT0121-BUZEI.

          IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING WL_BSIS TO ZFIT0121.
            ZFIT0121-AUGBL = WL_BSIS-BELNR.
            ZFIT0121-AUGDT = WL_BSIS-BUDAT.
          ELSE.
            SELECT SINGLE *
              FROM BSAS INTO WL_BSAS
             WHERE BUKRS = ZFIT0121-BUKRS
               AND BELNR = ZFIT0121-BELNR
               AND GJAHR = ZFIT0121-GJAHR
               AND BUZEI = ZFIT0121-BUZEI.

            IF SY-SUBRC NE 0.
              MESSAGE 'Documento não encontrado!' TYPE 'W'.
              RETURN.
            ENDIF.

            MOVE-CORRESPONDING WL_BSAS TO ZFIT0121.
          ENDIF.


        WHEN OTHERS.
          RETURN.
      ENDCASE.

      IF ZFIT0121-LCTO_COMP IS NOT INITIAL. "Utilizar Data de Lcto como Referencia.
        ZFIT0121-AUGDT = ZFIT0121-BUDAT.
      ELSE.
        SELECT SINGLE *
          FROM BKPF INTO WL_BKPF
         WHERE BUKRS = ZFIT0121-BUKRS
           AND BELNR = ZFIT0121-AUGBL
           AND GJAHR = ZFIT0121-AUGDT(4).

        IF SY-SUBRC NE 0.
          MESSAGE 'Houve um erro ao incluir o documento!' TYPE 'W'.
          RETURN.
        ENDIF.

        ZFIT0121-AUGDT = WL_BKPF-BUDAT.
      ENDIF.

      SELECT SINGLE *
        FROM FAGLFLEXA INTO WL_FLEXA
       WHERE RYEAR   EQ ZFIT0121-GJAHR
         AND DOCNR   EQ ZFIT0121-BELNR
         AND RLDNR   EQ '0L'
         AND RBUKRS  EQ ZFIT0121-BUKRS
         AND BUZEI   EQ ZFIT0121-BUZEI.

      IF SY-SUBRC = 0.
        ZFIT0121-RMVCT = WL_FLEXA-RMVCT.
      ENDIF.

      SELECT SINGLE *
        FROM ZFIT0078 INTO WL_0078
       WHERE COD_FLX = ZFIT0121-COD_FLX
         AND SHKZG NE ''.

      IF WL_0078-SHKZG = 'S'.
        ZFIT0121-DMBTR  = ABS( ZFIT0121-DMBTR ) * -1.
        ZFIT0121-DMBE2  = ABS( ZFIT0121-DMBE2 ) * -1.
      ELSEIF WL_0078-SHKZG = 'H'.
        ZFIT0121-DMBTR  = ABS( ZFIT0121-DMBTR ).
        ZFIT0121-DMBE2  = ABS( ZFIT0121-DMBE2 ).
      ENDIF.

      ZFIT0121-MANUAL   = 'X'.
      ZFIT0121-USNAM    = SY-UNAME.
      ZFIT0121-DT_ATUAL = SY-DATUM.
      ZFIT0121-HR_ATUAL = SY-UZEIT.

      MOVE-CORRESPONDING ZFIT0121 TO WL_0121.
      MODIFY ZFIT0121 FROM WL_0121.

      MOVE-CORRESPONDING ZFIT0121 TO WL_0080.
      MODIFY ZFIT0080 FROM WL_0080.

      MESSAGE 'Lançamento gravado com sucesso!' TYPE 'S'.
      LEAVE TO SCREEN 0.
    WHEN 'DEL_LCTO'.
      CLEAR: WL_0121.
      PERFORM VALIDA_INFO_0102 USING VL_VALIDA 'D'.
      CHECK VL_VALIDA IS NOT INITIAL.

      DELETE FROM ZFIT0121 WHERE BUKRS  = ZFIT0121-BUKRS
                             AND BELNR  = ZFIT0121-BELNR
                             AND GJAHR  = ZFIT0121-GJAHR
                             AND BUZEI  = ZFIT0121-BUZEI
                             AND MANUAL = 'X'.

      DELETE FROM ZFIT0080 WHERE BUKRS  = ZFIT0121-BUKRS
                             AND BELNR  = ZFIT0121-BELNR
                             AND GJAHR  = ZFIT0121-GJAHR
                             AND BUZEI  = ZFIT0121-BUZEI
                             AND MANUAL = 'X'.

      IF SY-SUBRC = 0.
        MESSAGE 'Lançamento manual excluído com sucesso!' TYPE 'S'.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

MODULE USER_COMMAND_0103 INPUT.

 CALL METHOD OBJ_ALV_0103->CHECK_CHANGED_DATA.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      IF ZFIT0080-BUKRS IS INITIAL.
        MESSAGE 'Empresa é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZFIT0080-GJAHR IS INITIAL.
        MESSAGE 'Ano é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZFIT0080-MONAT IS INITIAL.
        MESSAGE 'Mês é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZFIT0080-MONAT EQ 0 OR ZFIT0080-MONAT > 12.
        MESSAGE 'Mês informado, é inválido!' TYPE 'S'.
        EXIT.
      ENDIF.

      DELETE FROM ZFIT0080 WHERE BUKRS      = ZFIT0080-BUKRS
                             AND GJAHR      = ZFIT0080-GJAHR
                             AND MONAT      = ZFIT0080-MONAT
                             AND SLD_CONTAS = 'X'.

      DELETE FROM ZFIT0121 WHERE BUKRS      = ZFIT0080-BUKRS
                             AND GJAHR      = ZFIT0080-GJAHR
                             AND MONAT      = ZFIT0080-MONAT
                             AND SLD_CONTAS = 'X'.

      IF T_SAIDA_0103[] IS NOT INITIAL.

        CLEAR: WL_0121, WL_0080, WL_0121.

        LOOP AT T_SAIDA_0103 INTO WA_SAIDA_0103.

          WL_0080-BUKRS          = ZFIT0080-BUKRS.
          WL_0080-GJAHR          = ZFIT0080-GJAHR.
          WL_0080-MONAT          = ZFIT0080-MONAT.
          WL_0080-BUZEI          = '001'.
          WL_0080-BUDAT          = ZFIT0080-AUGDT.
          WL_0080-AUGDT          = ZFIT0080-AUGDT.
          WL_0080-BLDAT          = ZFIT0080-AUGDT.
          WL_0080-WAERS          = ''.
          WL_0080-XBLNR          = 'Saldo Contas'.
          WL_0080-MANUAL         = 'X'.
          WL_0080-SLD_CONTAS     = 'X'.
          WL_0080-USNAM          = SY-UNAME.
          WL_0080-DT_ATUAL       = SY-DATUM.
          WL_0080-HR_ATUAL       = SY-UZEIT.

          ADD WA_SAIDA_0103-DMBTR TO  WL_0080-DMBTR.
          ADD WA_SAIDA_0103-DMBE2 TO  WL_0080-DMBE2.
        ENDLOOP.

        MODIFY ZFIT0080 FROM WL_0080.

        IF SY-SUBRC NE 0.
          MESSAGE 'Houve um erro ao gravar o saldo!' TYPE 'W'.
          EXIT.
        ENDIF.

        MOVE-CORRESPONDING WL_0080 TO WL_0121.
        MODIFY ZFIT0121 FROM WL_0121.
        IF SY-SUBRC NE 0.
          MESSAGE 'Houve um erro ao gravar o saldo!' TYPE 'W'.
          EXIT.
        ENDIF.

      ENDIF.

      CALL FUNCTION 'ZFI_PROC_FLX_REAL'
        EXPORTING
          I_DATA_INI        = ZFIT0080-AUGDT
          I_BUKRS           = ZFIT0080-BUKRS
          I_REF_SALDO       = 'X'.

      MESSAGE 'Lançamento efetuado com sucesso!' TYPE 'S'.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE HELP_HKONT INPUT.

  "Parametros Contas FF7A.
  DATA: WATAB_RF40V     LIKE RF40V OCCURS 0 WITH HEADER LINE.

  DATA: P_FDREC  LIKE RFPDO1-TS65FDRC,
        P_TFREC  LIKE RFPDO1-TS65TFRC VALUE 'X',
        P_XSKWR  TYPE CMXGLCURR,
        L_XENTVW TYPE CMXENTVW.

  RANGES: S_BUKRS_T FOR FDSB-BUKRS,
          S_DISPW_T FOR FDSB-DISPW,
          S_GLIED_T FOR RF40T-GLIED,
          S_GSBER_T FOR FDSB-GSBER,
          S_SEGMT_T FOR FDSB2-SEGMENT,
          S_PRCTR_T FOR FDESDIST-PRCTR,
          S_GEBER_T FOR FDSB2-GEBER,
          S_GRANT_T FOR FDESDIST-GRANT_NBR.

  DATA: GT_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        GT_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE,
        VL_MARK_TAB   TYPE DDSHMARKS.

  DATA: BEGIN OF GT_SAKNR OCCURS 0,
          BUKRS     TYPE SKB1-BUKRS,
          SAKNR     TYPE SKAT-SAKNR,
          TXT50     TYPE SKAT-TXT50,
        END OF GT_SAKNR.

  DATA: VL_SAKNR TYPE ZFIT0080-HKONT.

  CALL METHOD OBJ_ALV_0103->CHECK_CHANGED_DATA.

  IF ZFIT0080-BUKRS IS INITIAL.
    MESSAGE 'Empresa é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF ZFIT0080-GJAHR IS INITIAL.
    MESSAGE 'Ano é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF ZFIT0080-MONAT IS INITIAL.
    MESSAGE 'Mês é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF ZFIT0080-MONAT EQ 0 OR ZFIT0080-MONAT > 12.
    MESSAGE 'Mês informado, é inválido!' TYPE 'S'.
    EXIT.
  ENDIF.

  CONCATENATE ZFIT0080-GJAHR ZFIT0080-MONAT '01' INTO ZFIT0080-AUGDT.

  "Busca Ultimo dia do mês
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = ZFIT0080-AUGDT
    IMPORTING
      LAST_DAY_OF_MONTH = ZFIT0080-AUGDT.


  "Busca Contas FF7A
  S_BUKRS_T-SIGN   = 'I'.
  S_BUKRS_T-OPTION = 'EQ'.
  S_BUKRS_T-LOW    = ZFIT0080-BUKRS.
  APPEND S_BUKRS_T.

  S_GLIED_T-SIGN   = 'I'.
  S_GLIED_T-OPTION = 'EQ'.
  S_GLIED_T-LOW    = 'BP01'.
  APPEND S_GLIED_T.


  CALL FUNCTION 'CASH_FORECAST_SELECT_AND_COMPR'
     EXPORTING
           X_CASH_FORECAST     = P_FDREC
           X_CASH_MANAGEMENT   = P_TFREC
           X_GROUP_COMPRESSION = 'X'
           X_LEVEL_COMPRESSION = 'X'
           X_GSBER_COMPRESSION = 'X'
           I_XGLCURR           = P_XSKWR
           I_XENTVW            = L_XENTVW
     TABLES
           S_BUKRS     = S_BUKRS_T
           S_DISPW     = S_DISPW_T
           S_GLIED     = S_GLIED_T
           S_GSBER     = S_GSBER_T
           "S_SEGMENT   = S_SEGMT_T
           "S_PRCTR     = S_PRCTR_T
           "S_GEBER     = S_GEBER_T
           "S_GRANT_NBR = S_GRANT_T
           TAB_RF40V   = WATAB_RF40V.

  "Fim FF7A
  SORT WATAB_RF40V BY GRUPP.
  DELETE ADJACENT DUPLICATES FROM WATAB_RF40V COMPARING GRUPP.

  CLEAR: GT_SAKNR[].

  IF WATAB_RF40V[] IS NOT INITIAL.
    SELECT A~BUKRS B~SAKNR B~TXT50 "#EC CI_DB_OPERATION_OK[2431747]
      FROM SKB1 AS A INNER JOIN SKAT AS B ON A~SAKNR = B~SAKNR
      INTO CORRESPONDING FIELDS OF TABLE GT_SAKNR
      FOR ALL ENTRIES IN WATAB_RF40V
     WHERE BUKRS   EQ ZFIT0080-BUKRS
       AND A~SAKNR EQ WATAB_RF40V-GRUPP
       AND B~SPRAS EQ SY-LANGU
       AND B~KTOPL EQ '0050'
       AND A~FDLEV IN ('F0', 'B2').
  ENDIF.

  "Buscar Contas
  SELECT *
    FROM ZSALDO_CTA_MOEDA INTO TABLE @DATA(TG_ZSALDO_CTA_MOEDA)
   WHERE BUKRS = @ZFIT0080-BUKRS.

  IF TG_ZSALDO_CTA_MOEDA[] IS NOT INITIAL.
    SELECT A~BUKRS B~SAKNR B~TXT50 "#EC CI_DB_OPERATION_OK[2431747]
      FROM SKB1 AS A INNER JOIN SKAT AS B ON A~SAKNR = B~SAKNR
      APPENDING CORRESPONDING FIELDS OF TABLE GT_SAKNR
      FOR ALL ENTRIES IN TG_ZSALDO_CTA_MOEDA
   WHERE A~BUKRS EQ ZFIT0080-BUKRS
     AND A~SAKNR EQ TG_ZSALDO_CTA_MOEDA-SAKNR
     AND B~SPRAS EQ SY-LANGU
     AND B~KTOPL EQ '0050'
     AND A~FDLEV IN ('F0', 'B2').
  ENDIF.

  IF GT_SAKNR[] IS INITIAL.
    MESSAGE 'Nenhuma conta encontrada!' TYPE 'S'.
    EXIT.
  ENDIF.

  SORT GT_SAKNR BY BUKRS SAKNR.
  DELETE ADJACENT DUPLICATES FROM GT_SAKNR COMPARING BUKRS SAKNR.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'SAKNR'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZFIT0080-HKONT'
      VALUE_ORG       = 'S'
      MULTIPLE_CHOICE = 'X'
      MARK_TAB        = VL_MARK_TAB
    TABLES
      VALUE_TAB       = GT_SAKNR
      RETURN_TAB      = GT_RETURN_TAB
      DYNPFLD_MAPPING = GT_DSELC.

  IF GT_RETURN_TAB[] IS NOT INITIAL.

    LOOP AT GT_RETURN_TAB.

      VL_SAKNR = GT_RETURN_TAB-FIELDVAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT         = VL_SAKNR
        IMPORTING
          OUTPUT        = VL_SAKNR.

      READ TABLE GT_SAKNR WITH KEY SAKNR = VL_SAKNR.
      IF SY-SUBRC = 0.

        READ TABLE T_SAIDA_0103 INTO WA_SAIDA_0103 WITH KEY SAKNR = VL_SAKNR.
        IF SY-SUBRC NE 0.
          CLEAR: WA_SAIDA_0103.
          WA_SAIDA_0103-SAKNR = GT_SAKNR-SAKNR.
          WA_SAIDA_0103-TXT50 = GT_SAKNR-TXT50.

          PERFORM GET_SALDO_CONTA CHANGING WA_SAIDA_0103.

          APPEND WA_SAIDA_0103 TO T_SAIDA_0103.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDIF.

  CLEAR: ZFIT0080-HKONT.

  LEAVE TO SCREEN 0103.



ENDMODULE.
