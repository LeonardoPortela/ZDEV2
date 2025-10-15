*&---------------------------------------------------------------------*
*&  Include           ZFIY0004_SCR
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------
*  Selection screen
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-s01.
SELECT-OPTIONS:  s_witht      FOR  with_item-witht.
SELECTION-SCREEN END OF BLOCK 1.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE text-s02.
PARAMETERS:      s_pdate      LIKE j_1afpdo-prdate.

SELECTION-SCREEN END OF BLOCK 2.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK bloque3 WITH FRAME TITLE text-002.

PARAMETERS: s_copy   LIKE j_1afpdo-copyno OBLIGATORY DEFAULT '1'.
SKIP.
PARAMETERS: rb_view   TYPE c  RADIOBUTTON GROUP  gr1,
            rb_prnt   TYPE c  RADIOBUTTON GROUP gr1,
            p_impr    TYPE tsp03-padest OBLIGATORY DEFAULT 'LOCL',
            p_path    type string.
SELECTION-SCREEN END OF BLOCK bloque3.

*SELECTION-SCREEN BEGIN OF BLOCK MAIL WITH FRAME TITLE TEXT-001.
*PARAMETERS: PEMAIL TYPE CHAR120.
*SELECTION-SCREEN END OF BLOCK MAIL.
