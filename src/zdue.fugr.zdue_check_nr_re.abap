FUNCTION ZDUE_CHECK_NR_RE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_NUMERO) TYPE  CLIKE
*"  EXPORTING
*"     REFERENCE(E_IS_DUE) TYPE  CHAR01
*"----------------------------------------------------------------------

  CLEAR: E_IS_DUE.

  CHECK I_NUMERO IS NOT INITIAL.

  IF STRLEN( I_NUMERO ) EQ 14.
    IF I_NUMERO+2(2) = 'BR'. "É uma DU-e
      E_IS_DUE = ABAP_TRUE.
      RETURN.
    ENDIF.
  ENDIF.

ENDFUNCTION.
