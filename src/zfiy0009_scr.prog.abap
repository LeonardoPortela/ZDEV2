*&---------------------------------------------------------------------*
*&  Include           ZFIY0009_SCR
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------
*  Selection screen
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-006.
SELECT-OPTIONS: S_WITHT       FOR WITH_ITEM-WITHT.
SELECT-OPTIONS: S_KTOSL       FOR BSET-KTOSL.

DATA: P_LFILE.

* SELECTION-SCREEN ULINE.
   PARAMETERS:     S_NEWDAT      LIKE J_1AFPDO-NEWDAT NO-DISPLAY,
                   S_FILE        LIKE RFPDO1-ALLGUNIX NO-DISPLAY,
*                  P_LFILE       AS   CHECKBOX ,
                   P_LFNAME      LIKE SAPB-SAPPFAD NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK 1.

data: P_SFILE,
      P_LWFILE.
SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE TEXT-S02.
PARAMETERS: "    P_SFILE   AS   CHECKBOX ,
                 P_SFNAM   LIKE SAPB-SAPPFAD NO-DISPLAY,
*                P_LWFILE  AS   CHECKBOX ,
                 P_LWFNAM  LIKE SAPB-SAPPFAD NO-DISPLAY,
                 P_BAJA    AS   CHECKBOX ,
                 P_fila    LIKE rlgrap-filename.

SELECTION-SCREEN END OF BLOCK 2.

SELECTION-SCREEN BEGIN OF BLOCK 3 WITH FRAME TITLE text-304.
* ALV Changes starts
SELECTION-SCREEN:
BEGIN OF LINE,
 END OF LINE.
* first parameter line
SELECTION-SCREEN BEGIN OF LINE.
* checkbox
PARAMETERS par_lis1 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (10) text-003 FOR FIELD par_lis1.
* description
SELECTION-SCREEN: COMMENT (10) text-007
                  FOR FIELD par_var1.
*                 POSITION pos_high.
* variant field
PARAMETERS par_var1 TYPE disvariant-variant.
* push button
SELECTION-SCREEN: PUSHBUTTON 50(15) text-004
                  USER-COMMAND con1.
SELECTION-SCREEN END OF LINE.

* second parameter line
SELECTION-SCREEN BEGIN OF LINE.
* checkbox
PARAMETERS par_lis2 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (10) text-005 FOR FIELD par_lis2.
* description
SELECTION-SCREEN: COMMENT (10) text-007
                  FOR FIELD par_var2.
*                 POSITION pos_high.
* variant field
PARAMETERS par_var2 TYPE disvariant-variant.
* push button
SELECTION-SCREEN: PUSHBUTTON 50(15) text-004
                  USER-COMMAND con2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK 3.
