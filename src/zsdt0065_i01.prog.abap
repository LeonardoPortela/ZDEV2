*----------------------------------------------------------------------*
***INCLUDE ZSDT0065_I01 .
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
          TEXT_QUESTION         = 'Confirma a exclusão da Instrução?'
          TEXT_BUTTON_1         = 'Sim'(001)
          ICON_BUTTON_1         = 'ICON_OKAY '
          TEXT_BUTTON_2         = 'Não'(002)
          ICON_BUTTON_2         = 'ICON_CANCEL'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ' '
*         USERDEFINED_F1_HELP   = ' '
          START_COLUMN          = 25
          START_ROW             = 6
*         POPUP_TYPE            =
*         IV_QUICKINFO_BUTTON_1 = ' '
*         IV_QUICKINFO_BUTTON_2 = ' '
        IMPORTING
          ANSWER                = W_ANSWER
*       TABLES
*         PARAMETER             =
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.
      .
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF W_ANSWER = '1'.
        DELETE FROM ZFIT0048
        WHERE INSTRUCAO  = WG_CADLAN-INSTRUCAO
        AND   BUKRS  = WG_CADLAN-BUKRS
        AND   WERKS  = WG_CADLAN-WERKS
        AND   KUNNR  = WG_CADLAN-KUNNR.
        IF SY-SUBRC IS INITIAL.
          MESSAGE S836(SD) WITH 'A Instrução foi excluida!'.
        ELSE.
          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Erro ao excluir a'
                                                 'Instrução!'.
        ENDIF.
      ENDIF.
    WHEN C_ADD.
      WG_ACAO = C_MODIF.
      PERFORM LIMPA_CAMPOS.

      REFRESH: TG_FIELDS.
      PERFORM TRATA_CAMPOS USING SPACE
                        'GR1'
                         C_1       "INPUT 1     NO INPUT 0
                         C_0.      "INVISIBLE 1 VISIBLE 0

      CALL METHOD OBG_DESCBOX->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE = TG_EDITOR.
      CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 0.

    WHEN C_SEARCH.
      PERFORM BUSCA_DADOS.
    WHEN C_SAVE.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        CLEAR WG_ACAO.
        PERFORM GRAVA_DADOS.

        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                          'GR1'
                           C_0       "INPUT 1     NO INPUT 0
                           C_0.      "INVISIBLE 1 VISIBLE 0

        CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 1.

      ELSE.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = ' '
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.
    WHEN C_MODIF.
      IF WG_ACAO = C_MODIF.
        CLEAR WG_ACAO.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_0       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0
        CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 1.
      ELSE.
        WG_ACAO = C_MODIF.
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_1       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0
        CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 0.
      ENDIF.
    WHEN C_SHOW_MSGRE.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = ''
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.
    WHEN C_BACK.
      SET SCREEN 0.
    WHEN C_EXIT.
      LEAVE PROGRAM.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_FILIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_FILIAL INPUT.
  DATA: BEGIN OF TL_BRANCH OCCURS 0,
          BRANCH TYPE J_1BBRANCH-BRANCH,
          NAME   TYPE J_1BBRANCH-NAME,
        END OF TL_BRANCH.

  SELECT BRANCH NAME
    FROM J_1BBRANCH
    INTO TABLE TL_BRANCH
    WHERE BUKRS = WG_CADLAN-BUKRS.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'BRANCH'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZFIT0048-WERKS'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_BRANCH
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_FILIAL  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_INSTRUCAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_INSTRUCAO INPUT.
  DATA: BEGIN OF TL_INSTR OCCURS 0,
          INSTRUCAO     TYPE ZFIT0048-INSTRUCAO,
          DES_INSTRUCAO TYPE T012T-TEXT1,
        END OF TL_INSTR.

  REFRESH TL_INSTR.

  TL_INSTR-INSTRUCAO = '01'.
  TL_INSTR-DES_INSTRUCAO = 'Fatura'.
  APPEND TL_INSTR.

  TL_INSTR-INSTRUCAO = '02'.
  TL_INSTR-DES_INSTRUCAO = 'Adiantamento'.
  APPEND TL_INSTR.

  TL_INSTR-INSTRUCAO = '03'.
  TL_INSTR-DES_INSTRUCAO = 'Insumos'.
  APPEND TL_INSTR.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'INSTRUCAO'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZFIT0048-INSTRUCAO'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_INSTR
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_INSTRUCAO  INPUT
