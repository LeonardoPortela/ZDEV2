FUNCTION CONVERSION_EXIT_ZDDEX_OUTPUT.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  CLIKE
*"--------------------------------------------------------------------

  WRITE input USING EDIT MASK '__________/_' TO output.

ENDFUNCTION.
