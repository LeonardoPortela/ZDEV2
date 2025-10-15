*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_PAI
*&---------------------------------------------------------------------*


MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'AUTH_EMP'.

      CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

      CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_SEL_ROWS.

      IF IT_SEL_ROWS[] IS INITIAL.
        MESSAGE 'Selecione uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF LINES( IT_SEL_ROWS ) NE 1.
        MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.
      CHECK SY-SUBRC = 0.

      READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.
      CHECK SY-SUBRC = 0.

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

      IF ZAUTH_WEBSERVICE-SERVICE IS INITIAL.
        MESSAGE 'Nome serviço um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZAUTH_WEBSERVICE-URL IS INITIAL.
        MESSAGE 'Endereço URL é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF VG_OPERACAO EQ C_NOVO.
        SELECT SINGLE *
          FROM ZAUTH_WEBSERVICE INTO @DATA(_WL_ZAUTH_WEBSERVICE)
         WHERE SERVICE = @ZAUTH_WEBSERVICE-SERVICE.

        IF SY-SUBRC = 0.
          MESSAGE 'Serviço já cadastrado!' TYPE 'S'.
          EXIT.
        ENDIF.
      ENDIF.

      MODIFY ZAUTH_WEBSERVICE FROM ZAUTH_WEBSERVICE.

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

  DATA: IT_ZAUTH_WS_0001 TYPE TABLE OF ZAUTH_WS_0001,
        WL_ZAUTH_WS_0001 TYPE ZAUTH_WS_0001.

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

      CLEAR: IT_ZAUTH_WS_0001[].

      LOOP AT IT_SAIDA_0120 INTO WA_SAIDA_0120 WHERE CK_MODIFY EQ ABAP_TRUE.
        CLEAR: WL_ZAUTH_WS_0001.
        MOVE-CORRESPONDING WA_SAIDA_0120 TO WL_ZAUTH_WS_0001.

        IF WL_ZAUTH_WS_0001-BUKRS IS INITIAL.
          MESSAGE 'Empresa é um campo obrigatório!' TYPE 'S'.
          RETURN.
        ENDIF.

        WL_ZAUTH_WS_0001-DT_REGISTRO = SY-DATUM.
        WL_ZAUTH_WS_0001-HR_REGISTRO = SY-UZEIT.
        WL_ZAUTH_WS_0001-US_REGISTRO = SY-UNAME.

        APPEND WL_ZAUTH_WS_0001 TO IT_ZAUTH_WS_0001.
      ENDLOOP.

      MODIFY ZAUTH_WS_0001 FROM TABLE IT_ZAUTH_WS_0001.

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
