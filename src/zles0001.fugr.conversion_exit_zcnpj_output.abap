FUNCTION conversion_exit_zcnpj_output .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  CLIKE
*"----------------------------------------------------------------------

  IF NOT input IS INITIAL.
    WRITE input TO output USING EDIT MASK '__.___.___/____-__'.
    REPLACE ALL OCCURRENCES OF REGEX '\D+$' IN output WITH ''.
  ELSE.
    output = input.
  ENDIF.

ENDFUNCTION.
