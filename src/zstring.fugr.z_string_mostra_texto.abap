FUNCTION Z_STRING_MOSTRA_TEXTO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_STRING) TYPE  STRING
*"     REFERENCE(I_TITULO) TYPE  STRING OPTIONAL
*"     REFERENCE(I_HTML) TYPE  CHAR01 OPTIONAL
*"----------------------------------------------------------------------

  PERFORM MOSTRA_STRING USING I_STRING I_TITULO I_HTML.

ENDFUNCTION.
