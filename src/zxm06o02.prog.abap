*----------------------------------------------------------------------*
***INCLUDE ZXM06O02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0111  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0111 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.



  LOOP AT SCREEN.
    IF SY-TCODE NE 'ME21N'.
      IF SCREEN-NAME EQ 'EKPO_CI-ZKVGR3' OR
         SCREEN-NAME EQ 'EKPO_CI-ZKVGR4'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF ( SY-TCODE NE 'ME21N' ) AND ( SY-TCODE NE 'ME22N' ).
      IF SCREEN-NAME EQ 'EKPO_CI-ZCKFRETEENT'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDMODULE.
