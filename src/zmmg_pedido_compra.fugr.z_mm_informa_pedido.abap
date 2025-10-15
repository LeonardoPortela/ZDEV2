FUNCTION Z_MM_INFORMA_PEDIDO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(E_PEDIDO) TYPE  EBELN
*"----------------------------------------------------------------------

  PERFORM GET_NUMERO_PEDIDO CHANGING E_PEDIDO.

ENDFUNCTION.
