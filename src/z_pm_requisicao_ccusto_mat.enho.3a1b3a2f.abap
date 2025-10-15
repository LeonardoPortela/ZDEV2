"Name: \FU:CO_ZF_FILL_EBAN_FROM_RESBD\SE:END\EI
ENHANCEMENT 0 Z_PM_REQUISICAO_CCUSTO_MAT.

"Modificação solicitada pelo chamado CS2017000903
  "para possibilitar criar requisição de compra PM
  "com serviço s/ erro de C. Custo.

  IF sy-tcode eq 'IW21' OR
     sy-tcode eq 'IW22' OR
     sy-tcode eq 'IW31' OR
     sy-tcode eq 'IW32' OR
     sy-tcode eq 'IW34'.
    IF ebkn_exp-kostl is INITIAL.
      move caufvd-kostl to ebkn_exp-kostl.
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
