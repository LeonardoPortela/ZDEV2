*&---------------------------------------------------------------------*
*&  Include           ZFIY0014_SCR
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------
*  Selection screen
*----------------------------------------------------------------------
DATA: p_lfile.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-006.
PARAMETER: rb_001 LIKE ibipparms-pc DEFAULT 'X' RADIOBUTTON GROUP gr01 USER-COMMAND ent .

SELECT-OPTIONS: s_witht FOR with_item-witht.
PARAMETER: rb_002 LIKE ibipparms-pc RADIOBUTTON GROUP gr01 .
SELECT-OPTIONS: s_ktosl FOR bset-ktosl.
*SELECTION-SCREEN ULINE.
PARAMETERS: s_newdat LIKE j_1afpdo-newdat NO-DISPLAY,
            s_file        LIKE rfpdo1-allgunix NO-DISPLAY,
*           P_LFILE       AS   CHECKBOX ,
            p_lfname      LIKE sapb-sappfad NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK 1.

DATA: p_sfile,
      p_lwfile.
SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE text-s02.
PARAMETERS: " P_SFILE      AS CHECKBOX ,
              p_sfnam      LIKE sapb-sappfad NO-DISPLAY,
*             P_LWFILE     AS   CHECKBOX ,
              p_lwfnam     LIKE sapb-sappfad NO-DISPLAY,
              p_baja       AS   CHECKBOX ,
              p_fila       LIKE  rlgrap-filename.
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
*                  POSITION pos_high.
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
*                  POSITION pos_high.
* variant field
PARAMETERS par_var2 TYPE disvariant-variant.
* push button
SELECTION-SCREEN: PUSHBUTTON 50(15) text-004
                  USER-COMMAND con2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK 3.
