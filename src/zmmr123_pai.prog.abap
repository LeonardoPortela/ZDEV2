*&---------------------------------------------------------------------*
*&  Include           ZMMR123_PAI
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
    WHEN 'GRAVAR'.

      CLEAR: ZMMT0086.

      MOVE-CORRESPONDING WA_SAIDA_0110 TO ZMMT0086.

      IF ZMMT0086-EBELN IS INITIAL.
        MESSAGE 'Pedido não informado!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZMMT0086-EBELP IS INITIAL.
        MESSAGE 'Item pedido não informado!' TYPE 'S'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM EKPO INTO WL_EKPO
       WHERE EBELN = WA_SAIDA_0110-EBELN
         AND EBELP = WA_SAIDA_0110-EBELP.

      IF SY-SUBRC NE 0.
        MESSAGE 'Nro Pedido/Item estão incorretos!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF VG_OPERACAO = C_NOVO.
        SELECT SINGLE *
          FROM ZMMT0086 INTO @DATA(WL_086)
         WHERE EBELN = @WA_SAIDA_0110-EBELN
           AND EBELP = @WA_SAIDA_0110-EBELP.

        IF SY-SUBRC = 0.
          MESSAGE |Sobra já cadastrada para Pedido { WA_SAIDA_0110-EBELN } e Item { WA_SAIDA_0110-EBELP }!| TYPE 'S'.
          EXIT.
        ENDIF.
      ENDIF.

      IF ZMMT0086-BUKRS IS INITIAL.
        MESSAGE 'Empresa não encontrada!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZMMT0086-LIFNR IS INITIAL.
        MESSAGE 'Código fornecedor não encontrado!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZMMT0086-MATNR IS INITIAL.
        MESSAGE 'Código material não encontrado!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZMMT0086-MENGE IS INITIAL.
        MESSAGE 'Quantidade do Pedido não encontrada!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZMMT0086-MEINS IS INITIAL.
        MESSAGE 'Unidade Item não encontrada!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZMMT0086-WERKS IS INITIAL.
        MESSAGE 'Centro não encontrado!' TYPE 'S'.
        EXIT.
      ENDIF.

*      IF ZMMT0086-LGORT IS INITIAL.
*        MESSAGE 'Depósito não encontrado!' TYPE 'S'.
*        EXIT.
*      ENDIF.
*
*      IF ZMMT0086-CHARG IS INITIAL.
*        MESSAGE 'Lote não encontrado!' TYPE 'S'.
*        EXIT.
*      ENDIF.

*      IF ZMMT0086-SOBRA IS INITIAL.
*        MESSAGE 'Quantidade Sobra não informada!' TYPE 'S'.
*        EXIT.
*      ENDIF.

      SELECT SUM( PESO_LIQ )
        FROM ZSDT0001 INTO @DATA(_PESO_LIQ_ROM)
       WHERE VBELN        = @WL_EKPO-EBELN
         AND MATNR        = @WL_EKPO-MATNR.
         "AND TP_MOVIMENTO = 'S'.

      SELECT SINGLE BRGEW
        FROM MARA INTO @DATA(_BRGEW)
       WHERE MATNR = @WL_EKPO-MATNR.

      IF ( SY-SUBRC NE 0 ) OR ( _BRGEW = 0 ).
        MESSAGE 'Qtde. de Conversão para Peso Bruto não encontrada! Verificar cadastro de Material!' TYPE 'S'.
        EXIT.
      ENDIF.

      DATA(_TOT_LIQ_PED) = ( ZMMT0086-MENGE * _BRGEW ) + ( ZMMT0086-SOBRA * _BRGEW ).
      IF _TOT_LIQ_PED < _PESO_LIQ_ROM.
        MESSAGE |Quantidade Pedido+Sobra: { _TOT_LIQ_PED } ficará menor que quantidade de Romaneios: { _PESO_LIQ_ROM } ! | TYPE 'S'.
        WA_SAIDA_0110-SOBRA = WA_SAIDA_0110_AUX-SOBRA.
        LEAVE TO SCREEN 0110.
        RETURN.
      ENDIF.

      ZMMT0086-DT_REGISTRO = SY-DATUM.
      ZMMT0086-HR_REGISTRO = SY-UZEIT.
      ZMMT0086-USUARIO     = SY-UNAME.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = 'Confirmação'
          TEXT_QUESTION         = 'Confirmar gravação do registro de Sobra de Material?'
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

      MODIFY ZMMT0086 FROM ZMMT0086.

      IF SY-SUBRC = 0.
        MESSAGE 'Registro gravado com sucesso!' TYPE 'S'.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao gravar o registro!' TYPE 'S'.
        EXIT.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
