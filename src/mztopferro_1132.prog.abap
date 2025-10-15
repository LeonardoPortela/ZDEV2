*----------------------------------------------------------------------*
***INCLUDE MZTOPFERRO_1132.
*----------------------------------------------------------------------*

DATA: CK_ALTEROU_1132    TYPE CHAR01.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1132_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1132_EXIT INPUT.
  CLEAR OK_CODE.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_1132  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_1132 INPUT.
  CK_ALTEROU_1132 = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1132  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1132 OUTPUT.
  SET PF-STATUS 'PF1131'.
  SET TITLEBAR 'TL1131'.

  CK_ALTEROU_1132 = ABAP_FALSE.

  IF ZDE_ZLEST0128_ALV-MATNR IS NOT INITIAL.
    SELECT SINGLE MAKTX INTO ZDE_ZLEST0128_ALV-MAKTX
      FROM MAKT
     WHERE MATNR EQ ZDE_ZLEST0128_ALV-MATNR
      AND SPRAS EQ SY-LANGU.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1132  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1132 INPUT.

  CASE OK_CODE.
    WHEN OK_ADICIONAR.
      CLEAR OK_CODE.

      CHECK CK_ALTEROU_1132 EQ ABAP_FALSE.

      READ TABLE IT_ZLEST0128_ALV WITH KEY MATNR = ZDE_ZLEST0128_ALV-MATNR.
      IF SY-SUBRC IS INITIAL.
        MESSAGE S013.
        EXIT.
      ENDIF.

      CK_CONFIRMADO_1132 = ABAP_TRUE.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
