*&---------------------------------------------------------------------*
*&  Include           ZFIR063_PAI
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
      IF ( ZFIT0140-TXJCD IS INITIAL ).
        MESSAGE 'Informe o Domicílio Fiscal!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZFIT0140-DIA_VENCIMENTO IS INITIAL ).
        MESSAGE 'Informe o dia de Vencimento!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZFIT0140-DIA_VENCIMENTO < 0 ) OR ( ZFIT0140-DIA_VENCIMENTO > 31 ).
        MESSAGE 'Dia inválido!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF VG_OPERACAO = C_NOVO.
        SELECT SINGLE *
          FROM ZFIT0140 INTO @DATA(_WL_0140)
         WHERE TXJCD = @ZFIT0140-TXJCD.

        IF SY-SUBRC EQ 0.
          MESSAGE 'Já existe um registro para esse Domicílio Fiscal!' TYPE 'S'.
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

      ZFIT0140-DT_REGISTRO = SY-DATUM.
      ZFIT0140-HR_REGISTRO = SY-UZEIT.
      ZFIT0140-US_REGISTRO = SY-UNAME.

      MODIFY ZFIT0140 FROM ZFIT0140.

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
