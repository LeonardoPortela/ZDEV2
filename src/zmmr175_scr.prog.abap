*&---------------------------------------------------------------------*
*&  Include           ZMMR175_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR gwa_screen-bukrs,
                s_augdt FOR gwa_screen-augdt,
                s_gjahr FOR gwa_screen-gjahr,
                s_blart FOR gwa_screen-blart NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_vgabe  FOR gwa_screen-vgabe NO-DISPLAY,
                s_budat  FOR gwa_screen-budat,
                s_gjahr2 FOR gwa_screen-gjahr.
SELECTION-SCREEN END OF BLOCK b2.
