
TABLES: t001.

DATA: it_screen_status TYPE TABLE OF sy-ucomm.

*SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-001 .
*  SELECT-OPTIONS: p_bukrs   FOR t001-bukrs  NO-EXTENSION NO INTERVALS. "OBLIGATORY
*SELECTION-SCREEN END OF BLOCK part1.

*SELECTION-SCREEN BEGIN OF BLOCK part2 WITH FRAME TITLE TEXT-002 .
*  PARAMETERS: r1 RADIOBUTTON GROUP rad1,
*              r2 RADIOBUTTON GROUP rad1, "DEFAULT 'X',
*              r3 RADIOBUTTON GROUP rad1.
*SELECTION-SCREEN END OF BLOCK part2.

*SELECTION-SCREEN BEGIN OF BLOCK part3 WITH FRAME TITLE TEXT-003 .
*  PARAMETERS: p_vriant LIKE tline-tdline VISIBLE LENGTH 25.
*SELECTION-SCREEN END OF BLOCK part3.

*AT SELECTION-SCREEN OUTPUT.
*
*  it_screen_status = VALUE #( ( CONV sy-ucomm( '' ) ) ).
*
*  IF sy-dynnr = 1000.
*
*    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
*      EXPORTING
*        p_status  = 'STATUS_1000'
*        p_program = sy-repid
*      TABLES
*        p_exclude = it_screen_status.
*
*  ENDIF.
*
*AT SELECTION-SCREEN.
*  CASE sy-ucomm.
*    WHEN 'ONLI'.
*      LOOP AT SCREEN.
*        IF p_bukrs IS INITIAL.
**          IF screen-name(7) = 'P_BUKRS'.
**            screen-required = '2'.
**            MODIFY SCREEN.
**          ENDIF.
*        ENDIF.
*      ENDLOOP.
*      IF p_bukrs IS INITIAL.
*        MESSAGE 'Empresa é Obrigatorio!' TYPE 'E'.
*        STOP.
*      ENDIF.
*
*      CALL SELECTION-SCREEN '0100'.
*
*    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
*      SET SCREEN 0.
*      LEAVE SCREEN.
*  ENDCASE.


START-OF-SELECTION.

CALL SELECTION-SCREEN '0100'.
