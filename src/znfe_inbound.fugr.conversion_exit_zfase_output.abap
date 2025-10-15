FUNCTION CONVERSION_EXIT_ZFASE_OUTPUT.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"--------------------------------------------------------------------
  IF NOT INPUT IS INITIAL.
    WRITE INPUT USING EDIT MASK 'RR__.___.___.____-__'
          TO OUTPUT.
*
    DO.
      REPLACE ' ' WITH '0' INTO OUTPUT.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.
*
ENDFUNCTION.
