FUNCTION ZPM_GET_DATA_MARD.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_YEAR) TYPE  GJAHR OPTIONAL
*"     VALUE(I_DATE) TYPE  DATS OPTIONAL
*"  TABLES
*"      MARD STRUCTURE  MARD OPTIONAL
*"----------------------------------------------------------------------

  IF i_date IS NOT INITIAL.
    r_date = VALUE #( sign = 'I' option = 'EQ' ( low = i_date ) ).
  ENDIF.
  IF i_year IS NOT INITIAL.
    r_year = VALUE #( sign = 'I' option = 'EQ' ( low = i_year ) ).
  ENDIF.

  SELECT * FROM mard INTO TABLE mard
    WHERE lfgja IN r_year
      AND ersda IN r_date
      AND labst NE space.


ENDFUNCTION.
