*&---------------------------------------------------------------------*
*&  Include           ZFIY0013_SCR
*&---------------------------------------------------------------------*
*==========================================
*   *** PARAMETROS DE SELECCION ***
*==========================================
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: s_bukrs FOR bkpf-bukrs.
SELECT-OPTIONS: s_bldat FOR bkpf-bldat.
SELECT-OPTIONS: s_kschl FOR t685-kschl.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-002.
PARAMETERS: p_pc RADIOBUTTON GROUP r1 USER-COMMAND uc2.
PARAMETERS: p_serv RADIOBUTTON GROUP r1.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_file LIKE rlgrap-filename DEFAULT 'C:\IIBBStaFe.txt'.
SELECTION-SCREEN END OF BLOCK bl3.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK bl1.
