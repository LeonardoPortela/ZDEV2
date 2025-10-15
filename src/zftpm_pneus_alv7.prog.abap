*&---------------------------------------------------------------------*
*&  Include           ZFTPM_PNEUS_ALV7
*&---------------------------------------------------------------------*
CONTROLS: TB_ORD_PNEU_ESQ TYPE TABLEVIEW USING SCREEN 1070,
          TB_ORD_PNEU_DIR TYPE TABLEVIEW USING SCREEN 1070.

*----------------------------------------------------------------------*
*  MODULE TB_MONT_PNEU_ESQ_CH_TC_ATTR OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE TB_ORD_PNEU_ESQ_CH_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_ORD_PNEUS_ESQ LINES TB_ORD_PNEU_ESQ-LINES.
ENDMODULE.                 " TB_PNEU_ESQ_CHANGE_TC_ATTR  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TB_PNEU_DIR_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TB_ORD_PNEU_DIR_CH_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_ORD_PNEUS_DIR LINES TB_ORD_PNEU_DIR-LINES.
ENDMODULE.                 " TB_PNEU_DIR_CHANGE_TC_ATTR  OUTPUT

*----------------------------------------------------------------------*
*  MODULE MO_desm_DIR_GET_LINES OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE MO_ORD_DIR_GET_LINES OUTPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'WA_PNEUS_DIR-ARBPL'.
      IF  WA_PNEUS_DIR-EQUIP IS INITIAL.
        SCREEN-INPUT = '0'.
      ELSE.
        SCREEN-INPUT = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " MO_DIR_GET_LINES  OUTPUT


*----------------------------------------------------------------------*
*  MODULE MO_desm_ESQ_GET_LINES OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE MO_ORD_ESQ_GET_LINES OUTPUT.
*
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'WA_PNEUS_ESQ-ARBPL'.
      IF  WA_PNEUS_ESQ-EQUIP IS INITIAL.
        SCREEN-INPUT = '0'.
      ELSE.
        SCREEN-INPUT = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " MO_DIR_GET_LINES  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TB_ALV_ORD_PNEU_DIR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TB_ALV_ORD_PNEU_DIR INPUT.

*  IF WA_PNEUS_ESQ-ARBPL IS NOT INITIAL.
*    WA_PNEUS_ESQ-CHECK = 'X'.
*  ELSE.
*    CLEAR WA_PNEUS_ESQ-CHECK.
*  ENDIF.

  MODIFY IT_ORD_PNEUS_DIR FROM WA_PNEUS_DIR INDEX TB_ORD_PNEU_DIR-CURRENT_LINE.

ENDMODULE.                 " TB_ALV_ORD_PNEU_DIR  INPUT


*----------------------------------------------------------------------*
*  MODULE TB_ALV_ORD_PNEU_ESQ INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE TB_ALV_ORD_PNEU_ESQ INPUT.

*  IF WA_PNEUS_ESQ-CHECK IS NOT INITIAL.
*    WA_PNEUS_ESQ-CHECK = 'X'.
*  ELSE.
*    CLEAR WA_PNEUS_ESQ-CHECK.
*  ENDIF.

  MODIFY IT_ORD_PNEUS_ESQ FROM WA_PNEUS_ESQ INDEX TB_ORD_PNEU_ESQ-CURRENT_LINE.

ENDMODULE.                 " TB_ALV_ORD_PNEU_DIR  INPUT
