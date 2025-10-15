*&---------------------------------------------------------------------*
*&  Include           ZLESR0119_PAI
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

      IF ( ZLEST0158-MOTIVO IS INITIAL ).
        MESSAGE 'Informe um motivo!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( STRLEN( ZLEST0158-MOTIVO ) < 15 ).
        MESSAGE 'Informe no Minimo de 15 caracteres!' TYPE 'S'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = 'Confirmação'
          TEXT_QUESTION         = 'Deseja baixar as NF-es selecionadas?'
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

      ZLEST0158-US_REGISTRO = SY-UNAME.
      ZLEST0158-DT_REGISTRO = SY-DATUM.
      ZLEST0158-HR_REGISTRO = SY-UZEIT.

      LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.
        READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.
        CHECK SY-SUBRC = 0.

        ZLEST0158-CH_REFERENCIA = WA_SAIDA_0100-CH_REFERENCIA.
        MODIFY ZLEST0158 FROM ZLEST0158.
      ENDLOOP.

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
