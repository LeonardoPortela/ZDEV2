FUNCTION CONVERSION_EXIT_ZDDEX_INPUT.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  CLIKE
*"--------------------------------------------------------------------

  REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN input WITH '' IGNORING CASE.
  output = input.

ENDFUNCTION.
