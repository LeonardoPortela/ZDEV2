*&---------------------------------------------------------------------*
*&  Include           ZFTPM_PNEUS_ALV6
*&---------------------------------------------------------------------*

* Constante
*----------------------------------------------------------------------*
CONTROLS: TB_ALV3 TYPE TABLEVIEW USING SCREEN 1060.

*----------------------------------------------------------------------*
*  MODULE TB_ALV3_CHANGE_TC_ATTR OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE TB_ALV3_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_ITEM_PNEU LINES TB_ALV3-LINES.
ENDMODULE.                    "TB_ALV3_CHANGE_TC_ATTR OUTPUT


MODULE TB_ALV3_MARK INPUT.
  DATA: G_TB_ALV3_WA2 LIKE LINE OF IT_ITEM_PNEU.
  IF TB_ALV3-LINE_SEL_MODE = 1
  AND WA_ITEM_PNEU-CHECK = 'X'.
    LOOP AT IT_ITEM_PNEU INTO G_TB_ALV3_WA2
      WHERE CHECK = 'X'.
      G_TB_ALV3_WA2-CHECK = ''.
      MODIFY IT_ITEM_PNEU
        FROM G_TB_ALV3_WA2
        TRANSPORTING CHECK.
    ENDLOOP.
  ENDIF.
  MODIFY IT_ITEM_PNEU
    FROM WA_ITEM_PNEU
    INDEX TB_ALV3-CURRENT_LINE
    TRANSPORTING CHECK.
ENDMODULE.                    "TB_ALV3_MARK INPUT

MODULE TB_dir_MARK INPUT.
*  DATA: G_TB_ALV_dir LIKE LINE OF IT_PNEUS_ESQ.
*  IF TB_PNEU_DIR-LINE_SEL_MODE = 1
*  AND WA_ITEM_PNEU-CHECK = 'X'.
*    LOOP AT IT_ITEM_PNEU INTO G_TB_ALV3_WA2
*      WHERE CHECK = 'X'.
*      G_TB_ALV3_WA2-CHECK = ''.
*      MODIFY IT_ITEM_PNEU
*        FROM G_TB_ALV3_WA2
*        TRANSPORTING CHECK.
*    ENDLOOP.
*  ENDIF.
  MODIFY IT_PNEUS_DIR FROM WA_PNEUS_DIR INDEX TB_PNEU_DIR-CURRENT_LINE TRANSPORTING CHECK.
ENDMODULE.                    "TB_ALV3_MARK INPUT

MODULE TB_ESQ_MARK INPUT.
*  DATA: G_TB_ALV_dir LIKE LINE OF IT_PNEUS_ESQ.
*  IF TB_PNEU_DIR-LINE_SEL_MODE = 1
*  AND WA_ITEM_PNEU-CHECK = 'X'.
*    LOOP AT IT_ITEM_PNEU INTO G_TB_ALV3_WA2
*      WHERE CHECK = 'X'.
*      G_TB_ALV3_WA2-CHECK = ''.
*      MODIFY IT_ITEM_PNEU
*        FROM G_TB_ALV3_WA2
*        TRANSPORTING CHECK.
*    ENDLOOP.
*  ENDIF.
  MODIFY IT_PNEUS_ESQ FROM WA_PNEUS_ESQ INDEX TB_PNEU_ESQ-CURRENT_LINE TRANSPORTING CHECK.
ENDMODULE.                    "TB_ALV3_MARK INPUT
