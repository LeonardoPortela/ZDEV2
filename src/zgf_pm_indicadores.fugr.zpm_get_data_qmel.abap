FUNCTION ZPM_GET_DATA_QMEL.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_START) TYPE  DATS
*"     VALUE(I_END) TYPE  DATS
*"  TABLES
*"      QMEL STRUCTURE  QMEL OPTIONAL
*"----------------------------------------------------------------------

  r_date = VALUE #( sign = 'I' option = 'BT' ( low  = i_start
                                               high = COND #( WHEN i_end IS INITIAL THEN i_start
                                                              ELSE i_end ) ) ).

SELECT * FROM qmel INTO TABLE qmel
  WHERE erdat IN r_date
     OR aedat IN r_date.


ENDFUNCTION.
