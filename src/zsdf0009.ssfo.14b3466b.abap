DATA: l_lin    TYPE i,
      l_linhas TYPE i.

DESCRIBE TABLE itens LINES l_lin.

IF w_lfa1_werks2-stras IS NOT INITIAL.
  l_lin = l_lin + 4.
ENDIF.
IF w_lfa1_werks3-stras IS NOT INITIAL.
  l_lin = l_lin + 4.
ENDIF.
IF w_lfa1_werks4-stras IS NOT INITIAL.
  l_lin = l_lin + 4.
ENDIF.

*IF sfsy-page > 1.
*  l_lin = l_lin - 54.
*ENDIF.

*IF l_lin >= 43.
*  l_salta = abap_true.
*ELSE.
l_salta = abap_false.
*ENDIF.








