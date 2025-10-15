*&---------------------------------------------------------------------*
*&  Include           ZLESR0112_PAI
*&---------------------------------------------------------------------*

MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'FORN_FIS'.
      PERFORM F_CALL_SCREEN_0120.
    WHEN 'EXEC'.
      PERFORM: F_SELECIONAR_DADOS,
               F_PROCESSA_DADOS,
               F_REFRESH_ALV USING '0100'.
  ENDCASE.

ENDMODULE.

MODULE USER_COMMAND_0110 INPUT.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      IF ZLEST0149-BUKRS IS INITIAL.
        MESSAGE 'Empresa não informada!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZLEST0149-BRANCH IS INITIAL.
        MESSAGE 'Filial não informada!' TYPE 'S'.
        EXIT.
      ENDIF.

      ZLEST0149-DT_REGISTRO = SY-DATUM.
      ZLEST0149-HR_REGISTRO = SY-UZEIT.
      ZLEST0149-US_REGISTRO = SY-UNAME.

      MODIFY ZLEST0149 FROM ZLEST0149.

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

MODULE USER_COMMAND_0120 INPUT.

  DATA: IT_ZLEST0161 TYPE TABLE OF ZLEST0161,
        WL_ZLEST0161 TYPE ZLEST0161.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = 'Confirmação'
          TEXT_QUESTION         = 'Deseja realmente salvar os registros?'
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

      CLEAR: IT_ZLEST0161[].

      LOOP AT IT_SAIDA_0120 INTO WA_SAIDA_0120.
        CLEAR: WL_ZLEST0161.
        WL_ZLEST0161-LIFNR = WA_SAIDA_0120-LIFNR.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT      = WL_ZLEST0161-LIFNR
          IMPORTING
            OUTPUT     = WL_ZLEST0161-LIFNR.

        SELECT SINGLE *
          FROM LFA1 INTO @DATA(_LFA1)
         WHERE LIFNR = @WL_ZLEST0161-LIFNR.

        IF SY-SUBRC NE 0.
          MESSAGE |Fornecedor { WL_ZLEST0161-LIFNR } é inválido | TYPE 'S'.
          RETURN.
        ENDIF.

        APPEND WL_ZLEST0161 TO IT_ZLEST0161.
      ENDLOOP.

      DELETE FROM ZLEST0161.

      COMMIT WORK.

      MODIFY ZLEST0161 FROM TABLE IT_ZLEST0161.

      IF SY-SUBRC = 0.
        MESSAGE 'Registros salvos com sucesso!' TYPE 'S'.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao salvar os registros!' TYPE 'S'.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.
