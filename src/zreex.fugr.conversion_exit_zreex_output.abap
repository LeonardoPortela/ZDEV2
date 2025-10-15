FUNCTION CONVERSION_EXIT_ZREEX_OUTPUT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  CLIKE
*"----------------------------------------------------------------------

  DATA(_IS_DUE) = ABAP_FALSE.
  CALL FUNCTION 'ZDUE_CHECK_NR_RE'
    EXPORTING
      I_NUMERO  = INPUT
    IMPORTING
      E_IS_DUE  = _IS_DUE.

  IF _IS_DUE EQ ABAP_TRUE.
    WRITE INPUT to OUTPUT.
    RETURN.
  ENDIF.

write INPUT using edit mask '__/_______-___' to OUTPUT.


ENDFUNCTION.
