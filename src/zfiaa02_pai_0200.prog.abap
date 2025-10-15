*&---------------------------------------------------------------------*
*&  Include           ZFIAA02_PAI_0200
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_0200 INPUT.
  CLEAR LINES.
  CREATE OBJECT: R_TIPO_OPERACAO,
                 R_SELECIONA_DADOS,
                 R_UTILS.

  CASE SY-UCOMM.
    WHEN C_CANC.
      CLEAR: GT_MSG_RETURN, WL_MENSAGEM, MODO_OPERACAO.

      CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WL_STABLE.

      LEAVE TO SCREEN 0.

    WHEN C_SAVE.
      R_SELECIONA_DADOS->SELECIONA_DADOS_0200( ).

      IF ( GT_MSG_RETURN IS INITIAL ).
        R_TIPO_OPERACAO->SALVAR_DADOS_0200( ).
      ENDIF.

    WHEN C_EDIT.
      R_TIPO_OPERACAO->EDITAR_DADOS_0200( ).
    WHEN C_ENTER.
      R_SELECIONA_DADOS->SELECIONA_DADOS_0200( ).
    WHEN C_SHOW_MSG.
      R_UTILS->SHOW_SPLITTER_ERROR( I_SHOW  = X
                                    I_POPUP = 1 ).
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " PAI_0200  INPUT
