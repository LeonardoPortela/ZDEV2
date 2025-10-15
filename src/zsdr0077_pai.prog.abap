*&---------------------------------------------------------------------*
*&  Include           ZSDR0077_PAI
*&---------------------------------------------------------------------*


MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM: F_SELECIONAR_DADOS,
               F_PROCESSA_DADOS,
               F_REFRESH_ALV USING '0100'.
  ENDCASE.

ENDMODULE.

MODULE USER_COMMAND_0110 INPUT.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.
      IF ( ZSDT0149-REGION IS INITIAL ) AND ( ZSDT0149-BRANCH IS INITIAL ).
        MESSAGE 'Informe Estado ou Filial!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZSDT0149-REGION IS NOT INITIAL ) AND ( ZSDT0149-BRANCH IS NOT INITIAL ).
        MESSAGE 'Informe Estado ou Filial!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZSDT0149-TZONE IS INITIAL.
        MESSAGE 'Fuso Horário não informado!' TYPE 'S'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM TTZZ INTO @DATA(_WL_TTZZ)
       WHERE TZONE = @ZSDT0149-TZONE.

      IF SY-SUBRC NE 0.
        MESSAGE 'Fuso Horário inválido!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZSDT0149-BRANCH IS NOT INITIAL.
        SELECT SINGLE *
          FROM J_1BBRANCH INTO @DATA(_WL_J_1BBRANCH)
         WHERE BRANCH = @ZSDT0149-BRANCH.

        IF SY-SUBRC NE 0.
          MESSAGE |Filial inválida!| TYPE 'S'.
          EXIT.
        ENDIF.
      ENDIF.

      IF VG_OPERACAO = C_NOVO.
        SELECT SINGLE *
          FROM ZSDT0149 INTO @DATA(_WL_0049)
         WHERE REGION = @ZSDT0149-REGION
           AND BRANCH = @ZSDT0149-BRANCH.

        IF SY-SUBRC EQ 0.
          MESSAGE 'Já existe um registro para esse estado/filial!' TYPE 'S'.
          EXIT.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = 'Confirmação'
          TEXT_QUESTION         = 'Deseja realmente gravar o registro?'
          TEXT_BUTTON_1         = 'Sim'
          TEXT_BUTTON_2         = 'Não'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ''
        IMPORTING
          ANSWER                = VAR_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.

      CHECK VAR_ANSWER EQ '1'.

      ZSDT0149-DT_REGISTRO = SY-DATUM.
      ZSDT0149-HR_REGISTRO = SY-UZEIT.
      ZSDT0149-US_REGISTRO = SY-UNAME.

      MODIFY ZSDT0149 FROM ZSDT0149.

      IF SY-SUBRC = 0.
        MESSAGE 'Registro gravado com sucesso!' TYPE 'S'.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao gravar o registro!' TYPE 'S'.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE CARREGA_ARQUIVO INPUT.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_FILENAME     = ' '
      DEF_PATH         = P_FILE
      MASK             = ',*.xlsx.'
      MODE             = 'O'
      TITLE            = 'Arquivo a importar'
    IMPORTING
      FILENAME         = P_FILE
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

ENDMODULE.

MODULE USER_COMMAND_0111 INPUT.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      PERFORM F_IMPORT_ITENS.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
