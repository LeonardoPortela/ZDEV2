*&---------------------------------------------------------------------*
*&  Include           ZIM12_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE OK-CODE.
    WHEN C_SAVE.

      CALL METHOD GRID1->CHECK_CHANGED_DATA.
      PERFORM F_VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        CLEAR WG_ACAO.
        PERFORM F_GRAVA_DADOS.

      ELSE.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E59.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '0100'
            I_SHOW        = 'X'
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'TS_100_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.
    WHEN C_DISPLA.
      WG_ACAO = C_DISPLA.
      PERFORM F_LIMPA_CAMPOS.
      REFRESH: TG_FIELDS.
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR2'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR1'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

    WHEN C_ADD.
      CHECK WG_ACAO <> C_ADD.

      WG_ACAO = C_ADD.  "c_modif.

      PERFORM:  F_LIMPA_CAMPOS.

      REFRESH: TG_FIELDS.
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR2'
                                    C_1       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR1'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

    WHEN C_CANCEL.
      CLEAR WG_ACAO.

    WHEN C_EXIT OR C_BACK.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TS_100_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TS_100_ACTIVE_TAB_GET INPUT.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN C_TS_100-TAB1.
      G_TS_100-PRESSED_TAB = C_TS_100-TAB1.
    WHEN C_TS_100-TAB2.
      G_TS_100-PRESSED_TAB = C_TS_100-TAB2.

    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
