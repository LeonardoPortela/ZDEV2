*&---------------------------------------------------------------------*
*&  Include           ZFIY0011_SCR
*&---------------------------------------------------------------------*
*==========================================
*   *** PARAMETROS DE SELECCION ***
*==========================================

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
SELECTION-SCREEN SKIP 1.
PARAMETERS: s_bukrs TYPE bukrs.
SELECT-OPTIONS: s_bldat FOR bkpf-bldat.
SELECT-OPTIONS: s_ktosl FOR bset-ktosl.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-002.
PARAMETERS: p_pc RADIOBUTTON GROUP r1 USER-COMMAND uc2.
PARAMETERS: p_serv RADIOBUTTON GROUP r1.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_file LIKE rlgrap-filename DEFAULT 'C:\Percepcion_iva.txt'.
SELECTION-SCREEN END OF BLOCK bl3.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK bl1.
