*&---------------------------------------------------------------------*
*& Include          ZMMR200_SSC
*&---------------------------------------------------------------------*
*--------------------------------------------------------------------*
*SELECTION-SCREEN
*--------------------------------------------------------------------*
TABLES j_1bbranch.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_bukrs TYPE j_1bbranch-bukrs OBLIGATORY,
              p_data  TYPE budat OBLIGATORY,
              p_ref   TYPE xblnr.

  PARAMETER: rb_mr21 RADIOBUTTON GROUP gp1,
             rb_mr22 RADIOBUTTON GROUP gp1.

SELECTION-SCREEN END OF BLOCK b1.
