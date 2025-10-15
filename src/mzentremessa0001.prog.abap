*&---------------------------------------------------------------------*
*&  Include           MZENTREMESSA0001
*&---------------------------------------------------------------------*

TABLES: vbrk, j_1bnfdoc, t001w.

SELECTION-SCREEN BEGIN OF SCREEN 0001 AS SUBSCREEN NESTING LEVEL 3.
SELECTION-SCREEN BEGIN OF BLOCK zb01 WITH FRAME TITLE text-s01.
SELECT-OPTIONS: p_docnum FOR j_1bnfdoc-docnum ,
                p_docdat FOR wa_notas-dt_chegada OBLIGATORY,
                p_nfenum FOR j_1bnfdoc-nfenum NO-EXTENSION NO INTERVALS,
                p_modelo FOR j_1bnfdoc-model  DEFAULT '55' NO-DISPLAY.
PARAMETERS:     p_vbeln  LIKE vbrk-vbeln,
                p_centro LIKE t001w-werks OBLIGATORY,
                p_todos  AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK zb01.
SELECTION-SCREEN END OF SCREEN 0001.
