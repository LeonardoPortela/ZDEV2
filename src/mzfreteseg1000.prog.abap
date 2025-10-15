*&---------------------------------------------------------------------*
*&  Include           MZFRETESEG1000
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF SCREEN 1100 AS SUBSCREEN NESTING LEVEL 3.

SELECTION-SCREEN BEGIN OF BLOCK zb01 WITH FRAME TITLE text-s01.
PARAMETERS: p_monat TYPE monat,
            p_gjahr TYPE gjahr,
            p_matnr TYPE matnr.
SELECT-OPTIONS: p_inicio FOR zvalor_seg_terc-dt_inicio,
                p_final  FOR zvalor_seg_terc-dt_final.
SELECTION-SCREEN END OF BLOCK zb01.

SELECTION-SCREEN END OF SCREEN 1100.
