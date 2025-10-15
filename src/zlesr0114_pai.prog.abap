*&---------------------------------------------------------------------*
*&  Include           ZLESR0114_PAI
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

  DATA: V_BUKRS TYPE T001-BUKRS.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      IF ZLEST0152-BUKRS IS INITIAL.
        MESSAGE 'Centro não informado!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZLEST0152-RM_CODIGO IS INITIAL.
        MESSAGE 'Fornecedor não informado!' TYPE 'S'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT     = ZLEST0152-RM_CODIGO
        IMPORTING
          OUTPUT    = ZLEST0152-RM_CODIGO.

      SELECT SINGLE *
        FROM T001 INTO @DATA(_T001)
       WHERE BUKRS = @ZLEST0152-BUKRS.

      IF SY-SUBRC NE 0.
        MESSAGE 'Centro informado é inválido!' TYPE 'S'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM LFA1 INTO @DATA(_WL_LFA1)
       WHERE LIFNR = @ZLEST0152-RM_CODIGO.

      IF SY-SUBRC NE 0.
        MESSAGE 'Fornecedor informado é inválido!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF VG_OPERACAO = C_NOVO.
        SELECT SINGLE *
          FROM ZLEST0152 INTO @DATA(_WL_0152)
         WHERE BUKRS     = @ZLEST0152-BUKRS
           AND RM_CODIGO = @ZLEST0152-RM_CODIGO
           AND LOEKZ     = ''.

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

      SELECT MAX( CODIGO )
        FROM ZLEST0152 INTO @DATA(_CODIGO).

      ADD 1 TO _CODIGO.

      ZLEST0152-CODIGO       = _CODIGO.
      ZLEST0152-DT_REGISTRO  = SY-DATUM.
      ZLEST0152-HR_REGISTRO  = SY-UZEIT.
      ZLEST0152-US_REGISTRO  = SY-UNAME.

      MODIFY ZLEST0152 FROM ZLEST0152.

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
