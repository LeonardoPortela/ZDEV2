FUNCTION conversion_exit_zcnpj_input.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  CLIKE
*"--------------------------------------------------------------------
  DATA: lc_campo_max(18).

  lc_campo_max = input.
  REPLACE ALL OCCURRENCES OF REGEX '\D' IN lc_campo_max WITH ''.
  CONDENSE lc_campo_max.

  output = lc_campo_max.

ENDFUNCTION.
