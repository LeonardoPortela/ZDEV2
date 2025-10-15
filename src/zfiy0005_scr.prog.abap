*&---------------------------------------------------------------------*
*&  Include           ZFIY0005_SCR
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------
*  Selection screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK bloque2 WITH FRAME TITLE text-002.

PARAMETERS: rb_view type c RADIOBUTTON GROUP gr1,
            rb_prnt  type c  RADIOBUTTON GROUP gr1,
            s_copy   LIKE j_1afpdo-copyno OBLIGATORY default 2.

PARAMETERS: p_impr type TSP03-PADEST OBLIGATORY default 'LOCL'.

SELECTION-SCREEN END OF BLOCK bloque2.
