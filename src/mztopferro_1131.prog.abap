*----------------------------------------------------------------------*
***INCLUDE MZTOPFERRO_1131.
*----------------------------------------------------------------------*

DATA: CK_ALTEROU_1131    TYPE CHAR01.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1131_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1131_EXIT INPUT.
  CLEAR OK_CODE.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_1131  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_1131 INPUT.
  CK_ALTEROU_1131 = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1131  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1131 OUTPUT.
  SET PF-STATUS 'PF1131'.
  SET TITLEBAR 'TL1131'.

  CK_ALTEROU_1131 = ABAP_FALSE.

  IF ZDE_ZLEST0119_ALV-BUKRS IS NOT INITIAL.
    ZDE_ZLEST0120_ALV-BUKRS = ZDE_ZLEST0119_ALV-BUKRS.

    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'ZDE_ZLEST0120_ALV-BUKRS'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF ZDE_ZLEST0120_ALV-BUKRS IS NOT INITIAL.
    SELECT SINGLE BUTXT INTO ZDE_ZLEST0120_ALV-BUTXT
      FROM T001
     WHERE BUKRS EQ ZDE_ZLEST0120_ALV-BUKRS.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1131  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1131 INPUT.

  CASE OK_CODE.
    WHEN OK_ADICIONAR.
      CLEAR OK_CODE.

      CHECK CK_ALTEROU_1131 EQ ABAP_FALSE.

      READ TABLE IT_ZLEST0120_ALV WITH KEY BUKRS = ZDE_ZLEST0120_ALV-BUKRS.
      IF SY-SUBRC IS INITIAL.
        MESSAGE S005.
        EXIT.
      ENDIF.

      CK_CONFIRMADO_1131 = ABAP_TRUE.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
