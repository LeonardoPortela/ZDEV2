FUNCTION CONVERSION_EXIT_ZBOL2_OUTPUT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  CLIKE
*"----------------------------------------------------------------------

  REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN INPUT WITH '' IGNORING CASE.
  OUTPUT = INPUT.


ENDFUNCTION.
