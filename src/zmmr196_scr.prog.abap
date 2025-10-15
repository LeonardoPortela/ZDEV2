*&---------------------------------------------------------------------*
*& Include          ZMMR196_SCR
*&---------------------------------------------------------------------*



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_werks  FOR marc-werks OBLIGATORY,
                  s_matnr  FOR marc-matnr,
                  s_lgort  FOR eban-lgort,
                  s_plane  FOR marc-dispo,
                  s_resrv  FOR eban-rsnum,
                  s_tp_mrv FOR marc-dismm.

  PARAMETERS: p_ctr_at TYPE c  default ' ' no-display.
SELECTION-SCREEN END OF BLOCK b1.
