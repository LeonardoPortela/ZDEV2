FUNCTION CONVERSION_EXIT_ZFASE_INPUT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"  EXCEPTIONS
*"      NOT_VALID
*"----------------------------------------------------------------------

  DATA: LC_ENTRADA TYPE C LENGTH 18.

  LC_ENTRADA = INPUT.

  REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN LC_ENTRADA WITH ''.

  OUTPUT = LC_ENTRADA.

ENDFUNCTION.
