FUNCTION CONVERSION_EXIT_VBUND_INPUT .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  CLIKE
*"----------------------------------------------------------------------

  IF input IS INITIAL.
    MESSAGE 'operação intercompany - necessita preencher o campo. Em caso de dúvidas falar com contábil.' TYPE 'E'.
  ELSE.

    CALL 'CONVERSION_EXIT_ALPHA_INPUT'  ID 'INPUT'  FIELD INPUT
                                        ID 'OUTPUT' FIELD OUTPUT.

  ENDIF.

ENDFUNCTION.
