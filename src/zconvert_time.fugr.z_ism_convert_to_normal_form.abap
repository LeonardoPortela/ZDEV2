FUNCTION z_ism_convert_to_normal_form.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"----------------------------------------------------------------------

*
  CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
    EXPORTING
      intext  = input
    IMPORTING
      outtext = output.
*
  TRANSLATE output TO UPPER CASE.                         "#EC syntchar
*


ENDFUNCTION.
