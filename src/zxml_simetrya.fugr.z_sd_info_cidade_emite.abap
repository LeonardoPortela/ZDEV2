FUNCTION Z_SD_INFO_CIDADE_EMITE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WA_J_1BNFDOC) TYPE  J_1BNFDOC
*"  EXPORTING
*"     REFERENCE(TAXJURCODE) TYPE  J_1BTXJCD
*"----------------------------------------------------------------------

  DATA: WA_J_1BNFLIN   TYPE J_1BNFLIN,
        WA_J_1BBRANCH  TYPE J_1BBRANCH,
        WA_ZSDT_DEPARA_CEN TYPE ZSDT_DEPARA_CEN,
        WA_INFO_PART   TYPE LFA1,
        P_BUKRS        TYPE BUKRS,
        P_PARID        TYPE J_1BPARID,
        P_TP_FORNE     TYPE ZTP_FORNECIMENTO,
        P_DC_FORNE     TYPE ZDC_FORNECIMENTO,
        WA_LIPS        TYPE LIPS,
        P_PARCEIRO     TYPE J_1BPARID,
        WA_EKKO        TYPE EKKO,
        WA_LFA1        TYPE LFA1,
        WA_VBPA        TYPE VBPA.

  SELECT SINGLE * INTO WA_J_1BNFLIN
    FROM J_1BNFLIN
   WHERE DOCNUM EQ WA_J_1BNFDOC-DOCNUM.

  CHECK SY-SUBRC IS INITIAL.

  IF WA_J_1BNFLIN-REFTYP EQ 'ZW'.
    CLEAR: WA_INFO_PART.
    P_PARCEIRO = WA_J_1BNFDOC-BRANCH.
    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        P_PARCEIRO   = P_PARCEIRO
        P_PARTYPE    = 'B'
      CHANGING
        WA_INFO_PART = WA_INFO_PART.

    IF NOT WA_INFO_PART IS INITIAL.
      TAXJURCODE = WA_INFO_PART-TXJCD.
    ENDIF.
  ENDIF.

  CHECK WA_J_1BNFLIN-REFTYP NE 'ZW'.

  CALL FUNCTION 'Z_REMETENTE_MERCADORIA_CTE'
    EXPORTING
      P_DOCNUM   = WA_J_1BNFDOC-DOCNUM
    CHANGING
      P_BUKRS    = P_BUKRS
      P_PARID    = P_PARID
      P_TP_FORNE = P_TP_FORNE
      P_DC_FORNE = P_DC_FORNE.

  CASE P_TP_FORNE.

    WHEN: 'A'. "Aviso

      SELECT SINGLE * INTO WA_LIPS
        FROM LIPS
       WHERE VBELN EQ P_DC_FORNE.

      IF ( SY-SUBRC EQ 0 ).

        SELECT SINGLE * FROM EKKO INTO WA_EKKO WHERE EBELN EQ WA_LIPS-VGBEL.
        SELECT SINGLE * FROM LFA1 INTO WA_LFA1 WHERE LIFNR EQ WA_EKKO-LIFNR.

        IF ( SY-SUBRC EQ 0 ) AND ( WA_LFA1-KTOKK EQ 'ZFEX') .

          SELECT SINGLE * FROM VBPA INTO WA_VBPA WHERE VBELN EQ WA_LIPS-VBELN
                                                   AND  PARVW EQ 'PC'.
          IF ( SY-SUBRC EQ 0 ).
            P_PARCEIRO = WA_VBPA-LIFNR.
          ENDIF.

        ELSE.

          SELECT SINGLE * INTO WA_J_1BBRANCH
            FROM J_1BBRANCH
            WHERE BRANCH EQ  WA_LIPS-WERKS.

          IF SY-SUBRC IS INITIAL.
            P_PARCEIRO = WA_J_1BBRANCH-BRANCH.
          ELSE.

            SELECT SINGLE * INTO WA_ZSDT_DEPARA_CEN
              FROM ZSDT_DEPARA_CEN
             WHERE CENTROV_1 EQ WA_LIPS-WERKS.

            IF SY-SUBRC IS INITIAL.
              SELECT SINGLE * INTO WA_J_1BBRANCH
                FROM J_1BBRANCH
                WHERE BRANCH EQ  WA_ZSDT_DEPARA_CEN-CENTRO_REAL.
              IF SY-SUBRC IS INITIAL.
                P_PARCEIRO = WA_J_1BBRANCH-BRANCH.
              ELSE.
                EXIT.
              ENDIF.
            ELSE.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          P_PARCEIRO   = P_PARCEIRO
          P_PARTYPE    = 'B'
        CHANGING
          WA_INFO_PART = WA_INFO_PART.

      IF NOT WA_INFO_PART IS INITIAL.
        TAXJURCODE = WA_INFO_PART-TXJCD.
      ENDIF.

    WHEN: 'R'. "Remessa

      SELECT SINGLE * INTO WA_LIPS
             FROM LIPS
            WHERE VBELN EQ P_DC_FORNE.

      IF ( SY-SUBRC EQ 0 ).

        SELECT SINGLE * FROM VBPA INTO WA_VBPA WHERE VBELN EQ WA_LIPS-VBELN
                                                 AND  PARVW EQ 'PC'.

        SELECT SINGLE * FROM LFA1 INTO WA_LFA1 WHERE LIFNR EQ WA_VBPA-LIFNR.
        P_PARCEIRO = WA_VBPA-LIFNR.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            P_PARCEIRO   = P_PARCEIRO
            P_PARTYPE    = 'V'
          CHANGING
            WA_INFO_PART = WA_INFO_PART.

        IF NOT WA_INFO_PART IS INITIAL.
          TAXJURCODE = WA_INFO_PART-TXJCD.
        ENDIF.



*        CASE WA_LFA1-KTOKK.
*          WHEN: 'ZFIC'.

*            SELECT SINGLE * INTO WA_J_1BBRANCH
*              FROM J_1BBRANCH
*              WHERE BRANCH EQ  WA_LIPS-WERKS.
*
*            IF SY-SUBRC IS INITIAL.
*              P_PARCEIRO = WA_J_1BBRANCH-BRANCH.
*            ELSE.
*
*              SELECT SINGLE * INTO WA_ZSDT_DEPARA_CEN
*                FROM ZSDT_DEPARA_CEN
*               WHERE CENTROV_1 EQ WA_LIPS-WERKS.
*
*              IF SY-SUBRC IS INITIAL.
*                SELECT SINGLE * INTO WA_J_1BBRANCH
*                  FROM J_1BBRANCH
*                  WHERE BRANCH EQ  WA_ZSDT_DEPARA_CEN-CENTRO_REAL.
*                IF SY-SUBRC IS INITIAL.
*                  P_PARCEIRO = WA_J_1BBRANCH-BRANCH.
*                ELSE.
*                  EXIT.
*                ENDIF.
*              ELSE.
*                EXIT.
*              ENDIF.
*            ENDIF.

*          WHEN OTHERS.
*            P_PARCEIRO = WA_VBPA-LIFNR.
*        ENDCASE.



      ENDIF.

    WHEN OTHERS.

      CLEAR: WA_INFO_PART.

      P_PARCEIRO = WA_J_1BNFDOC-BRANCH.
      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          P_PARCEIRO   = P_PARCEIRO
          P_PARTYPE    = 'B'
        CHANGING
          WA_INFO_PART = WA_INFO_PART.

      IF NOT WA_INFO_PART IS INITIAL.
        TAXJURCODE = WA_INFO_PART-TXJCD.
      ENDIF.

  ENDCASE.

ENDFUNCTION.
