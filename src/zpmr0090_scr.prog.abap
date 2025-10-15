*&---------------------------------------------------------------------*
*& Include          ZPMR0090_SCR
*&---------------------------------------------------------------------*


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_cla   TYPE c RADIOBUTTON GROUP rb1 USER-COMMAND tst DEFAULT 'X',
              p_tp_ob TYPE c RADIOBUTTON GROUP rb1.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_class TYPE char10.
  SELECT-OPTIONS: s_eqart FOR equi-eqart.

  PARAMETERS: p_dt_ini TYPE sy-datum,
              p_dt_fim TYPE sy-datum,
              p_ence   TYPE c AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b2.
