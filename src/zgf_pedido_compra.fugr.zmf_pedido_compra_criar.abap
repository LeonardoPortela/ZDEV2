FUNCTION ZMF_PEDIDO_COMPRA_CRIAR.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_FUNDO) TYPE  CHAR01 DEFAULT 'X'
*"----------------------------------------------------------------------

  PERFORM PEDIDO_COMPRA_LANCAMENTO USING I_FUNDO.

ENDFUNCTION.
