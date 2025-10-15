FUNCTION ZPM_GET_DATA_IMPTT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_START) TYPE  DATS
*"     VALUE(I_END) TYPE  DATS
*"  TABLES
*"      IMPTT STRUCTURE  IMPTT OPTIONAL
*"----------------------------------------------------------------------
  r_date = VALUE #( sign = 'I' option = 'BT' ( low  = i_start
                                               high = COND #( WHEN i_end IS INITIAL THEN i_start
                                                              ELSE i_end ) ) ).

SELECT * FROM imptt INTO TABLE imptt
  WHERE erdat IN r_date
     OR aedat IN r_date.


ENDFUNCTION.
