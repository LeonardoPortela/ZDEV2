*&---------------------------------------------------------------------*
*&  Include           ZSDR0083_PAI
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

      IF ZSDT0080-GRUPO IS INITIAL.
        MESSAGE 'Grupo não informado!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZSDT0080-CAMPO IS INITIAL.
        MESSAGE 'Campo não informado!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF VG_OPERACAO = C_NOVO.
        SELECT SINGLE *
          FROM ZSDT0080 INTO @DATA(_WL_0080)
         WHERE GRUPO = @ZSDT0080-GRUPO
           AND CAMPO = @ZSDT0080-CAMPO.

        IF SY-SUBRC EQ 0.
          MESSAGE 'Já existe um registro com esses dados!' TYPE 'S'.
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

      MODIFY ZSDT0080 FROM ZSDT0080.

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

MODULE USER_COMMAND_0121 INPUT.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      IF ZSDT0079-GRUPO IS INITIAL.
        MESSAGE 'Grupo não informado!' TYPE 'S'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = 'Confirmação'
          TEXT_QUESTION         = 'Deseja realmente deletar o grupo?'
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

      DELETE FROM ZSDT0079 WHERE GRUPO = ZSDT0079-GRUPO.
      DELETE FROM ZSDT0080 WHERE GRUPO = ZSDT0079-GRUPO.

      MESSAGE 'Grupo removido com sucesso!' TYPE 'S'.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE USER_COMMAND_0120 INPUT.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      IF ZSDT0079-GRUPO IS INITIAL.
        MESSAGE 'Grupo não informado!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZSDT0079-DESCRICAO IS INITIAL.
        MESSAGE 'Descrição não informada!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF VG_OPERACAO = C_NOVO.
        SELECT SINGLE *
          FROM ZSDT0079 INTO @DATA(_WL_0079)
         WHERE GRUPO = @ZSDT0079-GRUPO.

        IF SY-SUBRC EQ 0.
          MESSAGE 'Já existe um registro com esse grupo!' TYPE 'S'.
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

      MODIFY ZSDT0079 FROM ZSDT0079.

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
