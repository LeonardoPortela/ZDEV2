FUNCTION zpm_get_data_equip_loc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_START) TYPE  DATS
*"     VALUE(I_END) TYPE  DATS
*"  TABLES
*"      ZPMT0078 STRUCTURE  ZPMT0078 OPTIONAL
*"----------------------------------------------------------------------
  DATA: r_dats     TYPE RANGE OF dats.

  r_dats = VALUE #( sign = 'I' option = 'BT' ( low  = i_start
                                               high = i_end ) ).


  SELECT * FROM zpmt0078
  INTO TABLE zpmt0078
  WHERE data_registro IN r_dats.


ENDFUNCTION.
