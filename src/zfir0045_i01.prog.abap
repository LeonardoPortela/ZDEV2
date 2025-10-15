*----------------------------------------------------------------------*
***INCLUDE ZFIR0045_I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
DATA W_ANSWER.

  CASE OK-CODE.
    WHEN C_DELDOC.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
*         TITLEBAR                    = ' '
*         DIAGNOSE_OBJECT             = ' '
         TEXT_QUESTION               = 'Confirma a exclusão desta Data?'
         TEXT_BUTTON_1               = 'Sim'(001)
         ICON_BUTTON_1               = 'ICON_OKAY '
         TEXT_BUTTON_2               = 'Não'(002)
         ICON_BUTTON_2               = 'ICON_CANCEL'
         DEFAULT_BUTTON              = '1'
         DISPLAY_CANCEL_BUTTON       = ' '
*         USERDEFINED_F1_HELP         = ' '
         START_COLUMN                = 25
         START_ROW                   = 6
*         POPUP_TYPE                  =
*         IV_QUICKINFO_BUTTON_1       = ' '
*         IV_QUICKINFO_BUTTON_2       = ' '
      IMPORTING
        ANSWER                      = W_ANSWER
*       TABLES
*         PARAMETER                   =
      EXCEPTIONS
        TEXT_NOT_FOUND              = 1
        OTHERS                      = 2.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF W_ANSWER = '1'.
        PERFORM ELIMINAR_PAR.
      ENDIF.
    WHEN C_SEARCH.
      IF WG_ACAO NE C_ADD.
        PERFORM BUSCA_DADOS.
      ENDIF.
    WHEN C_SAVE.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        CLEAR WG_ACAO.
        PERFORM GRAVA_DADOS.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_1       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0
      ELSE.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.

    WHEN C_BACK.
      CLEAR WG_ACAO.
    WHEN C_DISPLA.
      WG_ACAO = C_DISPLA.
      PERFORM LIMPA_CAMPOS.
      REFRESH: TG_FIELDS.
      PERFORM TRATA_CAMPOS USING SPACE
                                'GR1'
                                 C_0       "INPUT 1     NO INPUT 0
                                 C_0.      "INVISIBLE 1 VISIBLE 0
    WHEN C_ADD.
      IF WG_ACAO NE C_DISPLA.
        CLEAR XMODIF.
        WG_ACAO = C_MODIF.
        PERFORM LIMPA_CAMPOS.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_0       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0

      ELSEIF WG_ACAO = C_DISPLA.
        WG_ACAO = C_ADD.
        PERFORM LIMPA_CAMPOS.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_1       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0
      ENDIF.
    WHEN C_CANCEL.
      CLEAR WG_ACAO.
    WHEN C_ATUALI.

    WHEN C_MODIF.
      IF WG_ACAO = C_MODIF.
        CLEAR WG_ACAO.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_1       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0

      ELSE.
        WG_ACAO = C_MODIF.
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_0       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0

      ENDIF.
    WHEN C_SHOW_MSGRE.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.

    WHEN C_EXIT.

      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_EXIT INPUT.
  CASE OK-CODE.
    WHEN C_BACK.
      SET SCREEN 0.

    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT
