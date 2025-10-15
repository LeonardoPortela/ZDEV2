*----------------------------------------------------------------------*
***INCLUDE ZFTPM_PNEUS_TB_ALV_RODI_PNEI01 .
*----------------------------------------------------------------------*

CONTROLS: TB_RODI_PNEU_ESQ TYPE TABLEVIEW USING SCREEN 1050,
          TB_RODI_PNEU_DIR TYPE TABLEVIEW USING SCREEN 1050.

*----------------------------------------------------------------------*
*  MODULE TB_MONT_PNEU_ESQ_CH_TC_ATTR OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE TB_RODI_PNEU_ESQ_CH_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_RODI_PNEUS_ESQ LINES TB_RODI_PNEU_ESQ-LINES.
ENDMODULE.                 " TB_PNEU_ESQ_CHANGE_TC_ATTR  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TB_PNEU_DIR_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TB_RODI_PNEU_DIR_CH_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_RODI_PNEUS_DIR LINES TB_RODI_PNEU_DIR-LINES.
ENDMODULE.                 " TB_PNEU_DIR_CHANGE_TC_ATTR  OUTPUT

*----------------------------------------------------------------------*
*  MODULE TB_ALV_PNEU_ESQ INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE TB_ALV_RODI_PNEU_ESQ INPUT.

  IF WA_PNEUS_ESQ-RODIZ IS NOT INITIAL.
    WA_PNEUS_ESQ-CHECK = 'X'.
  ELSE.
    CLEAR WA_PNEUS_ESQ-CHECK.
  ENDIF.

  MODIFY IT_RODI_PNEUS_ESQ
    FROM WA_PNEUS_ESQ
    INDEX TB_RODI_PNEU_ESQ-CURRENT_LINE.
ENDMODULE.                    "TB_ALV2_MARK INPUT

**----------------------------------------------------------------------*
**  MODULE TB_ALV_PNEU_DIR INPUT
**----------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
MODULE TB_ALV_RODI_PNEU_DIR INPUT.

  IF WA_PNEUS_DIR-RODIZ IS NOT INITIAL.
    WA_PNEUS_DIR-CHECK = 'X'.
  ELSE.
    CLEAR WA_PNEUS_DIR-CHECK.
  ENDIF.

  MODIFY IT_RODI_PNEUS_DIR
    FROM WA_PNEUS_DIR
    INDEX TB_RODI_PNEU_DIR-CURRENT_LINE.
ENDMODULE.                    "TB_ALV2_MARK INPUT

*----------------------------------------------------------------------*
*  MODULE MO_desm_DIR_GET_LINES OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE MO_RODI_DIR_GET_LINES OUTPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'WA_PNEUS_DIR-RODIZ'.
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
MODULE MO_RODI_ESQ_GET_LINES OUTPUT.
*
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'WA_PNEUS_ESQ-RODIZ'.
      IF  WA_PNEUS_ESQ-EQUIP IS INITIAL.
        SCREEN-INPUT = '0'.
      ELSE.
        SCREEN-INPUT = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " MO_DIR_GET_LINES  OUTPUT
