DATA: l_lin TYPE i.
DESCRIBE TABLE itens LINES l_lin.
v_texto_geral_dinamico = lines( ta_texto_geral_dinamico ).

l_linhas = l_lin + v_texto_geral_dinamico.
IF sfsy-page > 1.
  l_linhas = l_linhas - 54.
ENDIF.

IF l_linhas > 37.
  l_salta = abap_true.
ELSE.
  l_salta = abap_false.
ENDIF.
















