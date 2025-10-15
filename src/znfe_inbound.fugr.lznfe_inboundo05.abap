*----------------------------------------------------------------------*
***INCLUDE LZNFE_INBOUNDO05.
*----------------------------------------------------------------------*

DATA: CK_ALTEROU_QUANTIDADE_SPLIT TYPE C LENGTH 1.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  SET PF-STATUS 'PF9001'.
  SET TITLEBAR 'TL9001'.

  CK_ALTEROU_QUANTIDADE_SPLIT = ABAP_FALSE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001_EXIT INPUT.
  CLEAR: OK_CODE.
  CK_ALTEROU_QUANTIDADE_SPLIT = ABAP_FALSE.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.

  CHECK CK_ALTEROU_QUANTIDADE_SPLIT EQ ABAP_FALSE.

  CASE OK_CODE.
    WHEN OK_CONFIRMAR.
      CLEAR: OK_CODE.
      CL_INFORMADO_NEW_ITEM = ABAP_TRUE.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_QUANTIDADE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_QUANTIDADE INPUT.
  CK_ALTEROU_QUANTIDADE_SPLIT = ABAP_TRUE.
ENDMODULE.
