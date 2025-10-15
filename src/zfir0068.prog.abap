*&---------------------------------------------------------------------*
*& Report  ZFIR0068
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZFIR0068.


 "IF SY-TCODE EQ 'ZFI0101'.
   "CALL SCREEN 0101 STARTING AT 02 02 ENDING AT 160 20 .
 "ELSE.
 "  CALL SCREEN 0100.
 "ENDIF.

 CALL SCREEN 0100.

INCLUDE ZFIR0068_TOP.
INCLUDE ZFIR0068_CLASS.
INCLUDE ZFIR0068_FORM.
INCLUDE ZFIR0068_PBO.
INCLUDE ZFIR0068_PAI.
