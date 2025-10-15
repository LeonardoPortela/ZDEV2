*&---------------------------------------------------------------------*
*& Report ZTREINOTESTE2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZTREINOTESTE2.

TABLES: ZTREINODIOGO2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS: rb_cons RADIOBUTTON GROUP 1,
              rb_lanc RADIOBUTTON GROUP 1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
  PARAMETERS: p_nome TYPE ZTREINODIOGO2-nome,
              p_prod TYPE ZTREINODIOGO2-Produto,
              p_valu TYPE ZTREINODIOGO2-valor_prod,
              p_ativ TYPE ZTREINODIOGO2-ativo.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
IF rb_cons IS NOT INITIAL.

ELSEIF rb_lanc IS NOT INITIAL.

ENDIF.


START-OF-SELECTION.
