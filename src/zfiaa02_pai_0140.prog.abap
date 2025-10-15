*&---------------------------------------------------------------------*
*&  Include           ZFIAA02_PAI_0140
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  PAI_0140  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_0140 INPUT.
  CREATE OBJECT: R_SELECIONA_DADOS.
*                 R_VALIDAR_INFO_ALV.

  CASE SY-UCOMM.
    WHEN C_ENTER.
      CLEAR: GT_MSG_RETURN, WL_MENSAGEM, GT_SAIDA_0140.

      IF ( S_EMPRES-LOW IS INITIAL ).
        CLEAR GT_SAIDA_0140.
        MESSAGE S836(SD) WITH TEXT-E03 DISPLAY LIKE 'E'.

      ELSE.
        R_SELECIONA_DADOS->SELECIONA_DADOS_0140( ).
      ENDIF.
  ENDCASE.
ENDMODULE.                 " PAI_0140  INPUT
