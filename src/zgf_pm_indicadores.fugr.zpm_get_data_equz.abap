FUNCTION zpm_get_data_equz.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_START) TYPE  DATS
*"     VALUE(I_END) TYPE  DATS
*"  TABLES
*"      EQUZ STRUCTURE  EQUZ OPTIONAL
*"----------------------------------------------------------------------


  r_date = VALUE #( sign = 'I' option = 'BT' ( low  = i_start
                                               high = i_end ) ).

  SELECT * FROM equz INTO TABLE equz
    WHERE erdat IN r_date
       OR aedat IN r_date.



ENDFUNCTION.
