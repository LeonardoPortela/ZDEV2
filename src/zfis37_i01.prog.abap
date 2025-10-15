*----------------------------------------------------------------------*
***INCLUDE ZFIS37_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE OK-CODE.
    WHEN C_DISPLA.
      WG_ACAO = C_DISPLA.
      REFRESH: TG_SAIDA.
      PERFORM F_BUSCA_DADOS.
      REFRESH: TG_FIELDS.

    WHEN C_ADD.
      CHECK WG_ACAO <> C_ADD.

      WG_ACAO = C_ADD.  "c_modif.

*      REFRESH: TG_SAIDA2.
      REFRESH: TG_FIELDS.

    WHEN C_SEARCH.
      PERFORM F_BUSCA_DADOS.
    WHEN C_SAVE.

      CALL METHOD GRID1->CHECK_CHANGED_DATA.
      PERFORM F_VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        CLEAR WG_ACAO.

        PERFORM: F_GRAVA_DADOS.

      ELSE.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E35.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '200'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = ''
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.

    WHEN C_SHOW_MSGRE.
      PERFORM F_VERIFICA_ERROS.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '200'
          I_SHOW        = C_X
          I_REPID       = SY-REPID
          I_PRESSED_TAB = ''
          I_SET_FIELD   = 'X_FIELD'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.

    WHEN C_CANCEL.
      SET SCREEN 0.
    WHEN C_EXIT.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.
