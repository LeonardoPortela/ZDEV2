*----------------------------------------------------------------------*
***INCLUDE LZLES_REPORTF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_ADT_CAIXA
*&---------------------------------------------------------------------*
FORM SELECIONAR_DADOS_ADT_CAIXA USING P_OPCAO.

  PERFORM SELECIONAR_DADOS_ADT USING P_OPCAO.

  PERFORM SELECIONAR_DADOS_ADT_ZLES0145 USING P_OPCAO.

ENDFORM.                    " SELECIONAR_DADOS_ADT_CAIXA
