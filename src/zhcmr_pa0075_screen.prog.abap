*&---------------------------------------------------------------------*
*&  Include           ZHCMR_PA0075_SCREEN
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK opsel WITH FRAME TITLE text-s01.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS so_bukrs FOR pa0001-bukrs.
SELECT-OPTIONS so_kostl FOR pa0001-kostl.
SELECT-OPTIONS so_pernr FOR pa0000-pernr.
PARAMETERS p_endda TYPE pa0001-endda.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK opsel.

SELECTION-SCREEN BEGIN OF BLOCK tipe WITH FRAME TITLE text-s02.
SELECTION-SCREEN SKIP.
PARAMETERS p_cargos AS CHECKBOX.
PARAMETERS p_carloc AS CHECKBOX.
PARAMETERS p_carati AS CHECKBOX.
PARAMETERS p_caratu AS CHECKBOX.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK tipe.

gr_bukrs[] = so_bukrs[].
gr_kostl[] = so_kostl[].
gr_pernr[] = so_pernr[].
IF p_endda IS INITIAL.
  p_endda = sy-datum.
ENDIF.

IF  p_carloc IS INITIAL
AND p_carati IS INITIAL
AND p_caratu IS INITIAL
AND p_cargos IS INITIAL.
  MESSAGE 'Selecionar ao menos um tipo de envio!'(i01) TYPE 'I' DISPLAY LIKE 'I'.
ENDIF.
