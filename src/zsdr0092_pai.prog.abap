*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_PAI
*&---------------------------------------------------------------------*


MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CAD_URF'.
      PERFORM F_CALL_SCREEN_0120.
    WHEN 'CAD_RA'.
      PERFORM F_CALL_SCREEN_0130.
    WHEN 'EXEC'.
      PERFORM: F_SELECIONAR_DADOS,
               F_PROCESSA_DADOS,
               F_REFRESH_ALV USING '0100'.
  ENDCASE.

ENDMODULE.

MODULE USER_COMMAND_0110 INPUT.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      IF ZSDT0169-CODIGO_RA IS INITIAL.
        MESSAGE 'Código Recinto Alfandegaldo é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZSDT0169-CODIGO_URF IS INITIAL.
        MESSAGE 'Código Unid. Receita Federal é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM ZSDT0168 INTO @DATA(_WL_0168)
       WHERE CODIGO_RA = @ZSDT0169-CODIGO_RA.

      IF SY-SUBRC NE 0.
        MESSAGE 'Código RA inválido!' TYPE 'S'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM ZSDT0167 INTO @DATA(_WL_0167)
       WHERE CODIGO_URF = @ZSDT0169-CODIGO_URF.

      IF SY-SUBRC NE 0.
        MESSAGE 'Código URF inválido!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF VG_OPERACAO EQ C_NOVO.
        SELECT SINGLE *
          FROM ZSDT0169 INTO @DATA(_WL_0169)
         WHERE CODIGO_RA = @ZSDT0169-CODIGO_RA.

        IF SY-SUBRC = 0.
          MESSAGE 'Código RA já cadastrado!' TYPE 'S'.
          EXIT.
        ENDIF.
      ENDIF.

      ZSDT0169-DT_REGISTRO = SY-DATUM.
      ZSDT0169-HR_REGISTRO = SY-UZEIT.
      ZSDT0169-US_REGISTRO = SY-UNAME.

      MODIFY ZSDT0169 FROM ZSDT0169.

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

  DATA: IT_ZSDT0167 TYPE TABLE OF ZSDT0167,
        WL_ZSDT0167 TYPE ZSDT0167.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      CALL METHOD OBJ_ALV_0120->CHECK_CHANGED_DATA.

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

      CLEAR: IT_ZSDT0167[].

      LOOP AT IT_SAIDA_0120 INTO WA_SAIDA_0120 WHERE CK_MODIFY EQ ABAP_TRUE.
        CLEAR: WL_ZSDT0167.
        MOVE-CORRESPONDING WA_SAIDA_0120 TO WL_ZSDT0167.

        IF WL_ZSDT0167-CODIGO_URF IS INITIAL.
          MESSAGE 'Codigo URF é um campo obrigatório!' TYPE 'S'.
          RETURN.
        ENDIF.

        IF WL_ZSDT0167-DS_URF IS INITIAL.
          MESSAGE 'Descrição URF é um campo obrigatório!' TYPE 'S'.
          RETURN.
        ENDIF.

        IF WL_ZSDT0167-DS_URF_ABREV IS INITIAL.
          MESSAGE 'Descrição URF Abreviada é um campo obrigatório!' TYPE 'S'.
          RETURN.
        ENDIF.

        WL_ZSDT0167-DT_REGISTRO = SY-DATUM.
        WL_ZSDT0167-HR_REGISTRO = SY-UZEIT.
        WL_ZSDT0167-US_REGISTRO = SY-UNAME.

        APPEND WL_ZSDT0167 TO IT_ZSDT0167.
      ENDLOOP.

      MODIFY ZSDT0167 FROM TABLE IT_ZSDT0167.

      IF SY-SUBRC = 0.
        COMMIT WORK.
        MESSAGE 'Registros salvos com sucesso!' TYPE 'S'.
        PERFORM: F_SELECIONAR_DADOS,
                 F_PROCESSA_DADOS.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao salvar os registros!' TYPE 'S'.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.


MODULE USER_COMMAND_0130 INPUT.

  DATA: IT_ZSDT0168 TYPE TABLE OF ZSDT0168,
        WL_ZSDT0168 TYPE ZSDT0168.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      CALL METHOD OBJ_ALV_0130->CHECK_CHANGED_DATA.

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

      CLEAR: IT_ZSDT0168[].

      LOOP AT IT_SAIDA_0130 INTO WA_SAIDA_0130 WHERE CK_MODIFY EQ ABAP_TRUE.
        CLEAR: WL_ZSDT0168.
        MOVE-CORRESPONDING WA_SAIDA_0130 TO WL_ZSDT0168.

        IF WL_ZSDT0168-CODIGO_RA IS INITIAL.
          MESSAGE 'Codigo RA é um campo obrigatório!' TYPE 'S'.
          RETURN.
        ENDIF.

        IF WL_ZSDT0168-DS_RA IS INITIAL.
          MESSAGE 'Descrição RA é um campo obrigatório!' TYPE 'S'.
          RETURN.
        ENDIF.

        WL_ZSDT0168-DT_REGISTRO = SY-DATUM.
        WL_ZSDT0168-HR_REGISTRO = SY-UZEIT.
        WL_ZSDT0168-US_REGISTRO = SY-UNAME.

        APPEND WL_ZSDT0168 TO IT_ZSDT0168.
      ENDLOOP.

      MODIFY ZSDT0168 FROM TABLE IT_ZSDT0168.

      IF SY-SUBRC = 0.
        COMMIT WORK.
        MESSAGE 'Registros salvos com sucesso!' TYPE 'S'.
        PERFORM: F_SELECIONAR_DADOS,
                 F_PROCESSA_DADOS.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao salvar os registros!' TYPE 'S'.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.
