function conversion_exit_znrdi_input.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  CLIKE
*"----------------------------------------------------------------------

  replace all occurrences of regex '[^0-9]' in input with '' ignoring case.
  output = input.

endfunction.
