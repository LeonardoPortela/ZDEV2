FUNCTION ZPM_GET_DATA_COBK.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_START) TYPE  DATS
*"     VALUE(I_END) TYPE  DATS
*"  TABLES
*"      COBK STRUCTURE  COBK
*"----------------------------------------------------------------------
  r_date = VALUE #( sign = 'I' option = 'BT' ( low  = i_start
                                               high = COND #( WHEN i_end IS INITIAL THEN i_start
                                                              ELSE i_end ) ) ).

SELECT * FROM cobk
  INTO TABLE cobk
  WHERE bldat IN r_date.


ENDFUNCTION.
