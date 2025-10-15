
LOOP AT it_saida into wa_saida.
  IF wa_saida-menge > 0.
    titulo = 'COMPROVANTE DE RETIRADA DE MATERIAIS DO ESTOQUE'.
  else.
   titulo = 'LISTA PARA RETIRADA DE MATERIAIS DO ESTOQUE'.
  ENDIF.

ENDLOOP.






















