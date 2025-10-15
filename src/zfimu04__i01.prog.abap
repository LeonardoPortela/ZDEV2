*----------------------------------------------------------------------*
***INCLUDE ZFIMU04__I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: CURSORFIELD(30)     TYPE C,
        CURSORLINE(30)      TYPE C,
        CURSORVALUE(30)     TYPE C,
        VG_LOTE             TYPE ZGLT034-LOTE,
        WA_ZIB_CONTABIL_CHV TYPE ZIB_CONTABIL_CHV.

  CASE OK-CODE.
    WHEN 'REFRESH'.
      IF WG_CADLAN-DOC_LCTOF IS NOT INITIAL.
        CONCATENATE 'ZGL17' WG_CADLAN-DOC_LCTOF WG_CADLAN-DT_LCTO+0(4) INTO WA_ZIB_CONTABIL_CHV-OBJ_KEY.
        SELECT SINGLE *
        FROM ZIB_CONTABIL_CHV
        INTO WA_ZIB_CONTABIL_CHV
        WHERE OBJ_KEY EQ WA_ZIB_CONTABIL_CHV-OBJ_KEY.

        IF SY-SUBRC = 0.
          WG_CADLAN-BELNRF = WA_ZIB_CONTABIL_CHV-BELNR.
          WG_CADLAN-GJAHRF = WA_ZIB_CONTABIL_CHV-GJAHR.
        ENDIF.
      ENDIF.

      IF WG_CADLAN-DOC_LCTOC IS NOT INITIAL.
        CONCATENATE 'ZGL17' WG_CADLAN-DOC_LCTOC WG_CADLAN-DT_LCTO+0(4) INTO WA_ZIB_CONTABIL_CHV-OBJ_KEY.
        SELECT SINGLE *
        FROM ZIB_CONTABIL_CHV
        INTO WA_ZIB_CONTABIL_CHV
        WHERE OBJ_KEY EQ WA_ZIB_CONTABIL_CHV-OBJ_KEY.

        IF SY-SUBRC = 0.
          WG_CADLAN-BELNRC = WA_ZIB_CONTABIL_CHV-BELNR.
          WG_CADLAN-GJAHRC = WA_ZIB_CONTABIL_CHV-GJAHR.
        ENDIF.
      ENDIF.
    WHEN 'PICK'.
      GET CURSOR FIELD CURSORFIELD LINE CURSORLINE VALUE CURSORVALUE.
      IF NOT WG_CADLAN-BELNRF IS INITIAL AND
         NOT WG_CADLAN-GJAHRF IS INITIAL AND
         NOT CURSORVALUE IS INITIAL AND CURSORFIELD = 'WG_CADLAN-BELNRF'.
        SET PARAMETER ID 'BLN' FIELD CURSORVALUE.
        SET PARAMETER ID 'BUK' FIELD WG_CADLAN-BUKRS_F.
        SET PARAMETER ID 'GJR' FIELD WG_CADLAN-GJAHRF.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
      IF NOT WG_CADLAN-BELNRC IS INITIAL AND
         NOT WG_CADLAN-GJAHRC IS INITIAL AND
         NOT CURSORVALUE IS INITIAL AND CURSORFIELD = 'WG_CADLAN-BELNRC'.
        SET PARAMETER ID 'BLN' FIELD CURSORVALUE.
        SET PARAMETER ID 'BUK' FIELD WG_CADLAN-BUKRS_C.
        SET PARAMETER ID 'GJR' FIELD WG_CADLAN-GJAHRC.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
      IF NOT WG_CADLAN-DOC_LCTOF IS INITIAL AND
         NOT CURSORVALUE IS INITIAL AND CURSORFIELD = 'WG_CADLAN-DOC_LCTOF'.
        CLEAR VG_LOTE.
        SET PARAMETER ID 'BLN' FIELD WG_CADLAN-DOC_LCTOF.
        SET PARAMETER ID 'LOT' FIELD  VG_LOTE.
        CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.
      ENDIF.
      IF NOT WG_CADLAN-DOC_LCTOC IS INITIAL AND
         NOT CURSORVALUE IS INITIAL AND CURSORFIELD = 'WG_CADLAN-DOC_LCTOC'.
        CLEAR VG_LOTE.
        SET PARAMETER ID 'BLN' FIELD WG_CADLAN-DOC_LCTOC.
        SET PARAMETER ID 'LOT' FIELD  VG_LOTE.
        CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN C_CANCEL.

    WHEN C_BACK.
      SET SCREEN 0.
    WHEN C_EXIT.
      LEAVE PROGRAM.
    WHEN C_SEARCH.
      PERFORM F_BUSCA_DADOS.
    WHEN C_DISPLA.
      WG_ACAO = C_DISPLA.
      PERFORM F_LIMPA_CAMPOS.
      REFRESH: TG_FIELDS.

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR4'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR2'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR1'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR3'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
    WHEN C_SAVE.
      CHECK WG_ACAO NE C_DISPLA.
      PERFORM F_VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        CLEAR WG_ACAO.
        IF WG_CADLAN-NRO_SOL IS INITIAL.
          PERFORM  F_OBTEM_PROXIMO.
          PERFORM F_GRAVA_DADOS.
        ENDIF.
        REFRESH TG_FIELDS.
        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR2'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR4'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.       "INPUT 1     NO INPUT 0

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR1'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR3'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
      ELSE.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E59.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = SPACE   "c_x
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'TS_100_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.
    WHEN C_ADD.
      CHECK WG_ACAO <> C_ADD.
      WG_ACAO = C_ADD.  "c_modif.
      PERFORM:  F_LIMPA_CAMPOS.
      REFRESH: TG_FIELDS.
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR2'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR4'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR1'
                                    C_1       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR3'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

    WHEN C_SHOW_MSGRE.
      PERFORM F_VERIFICA_ERROS.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '100'
          I_SHOW        = C_X
          I_REPID       = SY-REPID
          I_POPUP       = 0
          I_PRESSED_TAB = 'TS_100-PRESSED_TAB'
          I_SET_FIELD   = 'X_FIELD'
          I_SET_CELL    = 'WG_CELL'
          I_SET_OBJ     = 'WG_OBJ'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.
  ENDCASE.
ENDMODULE.
