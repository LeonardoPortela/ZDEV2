FUNCTION CONVERSION_EXIT_ZBOL2_INPUT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  CLIKE
*"----------------------------------------------------------------------

  REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN INPUT  WITH ''.
  CONDENSE INPUT NO-GAPS.

  OUTPUT = INPUT.

ENDFUNCTION.
