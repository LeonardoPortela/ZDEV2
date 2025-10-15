*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0304.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0304  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0304 OUTPUT.

  CLEAR: IT_EXCLUDE_FCODE.

  WA_EXCLUDE_FCODE = OK_CONFIRMAR.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = OK_VERIFICAR.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.

  SET PF-STATUS 'PFTELA' EXCLUDING IT_EXCLUDE_FCODE.
  SET TITLEBAR 'TL0304'.

  IF CK_ALTERACAO = 'A'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'A1'.
        SCREEN-INPUT   = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0304_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0304_EXIT INPUT.
  WA_ADD_NF_0104-CK_INCLUIR = ABAP_FALSE.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0304  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0304 INPUT.

  CASE OK_CODE.
    WHEN OK_SALVAR.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
