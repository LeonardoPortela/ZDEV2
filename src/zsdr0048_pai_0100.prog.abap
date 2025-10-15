*----------------------------------------------------------------------*
***INCLUDE ZSDR0048_PAI_0100 .
*----------------------------------------------------------------------*

MODULE PAI_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'OK'.

      CALL METHOD OBJ_GRID->CHECK_CHANGED_DATA.
      PERFORM GRAVA_REGISTRO.
      PERFORM CHANGE_ROWS USING 'DESACTIVE' ''.
      WL_DESACTIVE = ' '.

    WHEN 'CANC'.

      PERFORM SELECIONAR_DADOS.
      CALL METHOD OBJ_GRID->REFRESH_TABLE_DISPLAY.
      CALL METHOD OBJ_GRID->SET_READY_FOR_INPUT
        EXPORTING
          I_READY_FOR_INPUT = 1.

      WL_DESACTIVE = ' '.
      PERFORM CHANGE_ROWS USING 'DESACTIVE' ''.

  ENDCASE.

ENDMODULE.                 " PAI_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.

  DATA: BUSCA_TX TYPE REF TO LCL_ACAO.
  CREATE OBJECT BUSCA_TX.

  CASE SY-UCOMM.
    WHEN 'OK'.
      BUSCA_TX->VALIDA_CAMPOS( ).

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  PAI0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI0102 INPUT.

  CASE SY-UCOMM.

    WHEN 'SALVAR'.

      LCL_ACAO=>SALVAR_DADOS( ).

      CHECK VAR_EXIT IS INITIAL.

      PERFORM ATUALIZA.
      FREE: IT_ADD, WA_ADD, CONT, SL_ZSDT0094-FIELD_STYLE, VAR_DIR.
      LEAVE TO SCREEN  0.

    WHEN OTHERS.

      PERFORM ATUALIZA.
      FREE: IT_ADD, WA_ADD, CONT, SL_ZSDT0094-FIELD_STYLE, VAR_DIR.
      LEAVE TO SCREEN  0.

  ENDCASE.

ENDMODULE.
