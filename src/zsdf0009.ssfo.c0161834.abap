DATA: l_lin  TYPE i,
      l_pula TYPE i.

DESCRIBE TABLE itens LINES l_lin.

IF l_lin <= 18.
  l_salta = abap_true.
ELSEIF l_lin >= 38 AND l_lin <= 58.
  l_salta = abap_true.
ELSEIF l_lin >= 78 AND l_lin <= 98.
  l_salta = abap_true.
ELSEIF l_lin >= 118 AND l_lin <= 138.
  l_salta = abap_true.
ELSE.
  l_salta = abap_false.
ENDIF.













