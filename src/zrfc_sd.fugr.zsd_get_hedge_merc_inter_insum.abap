FUNCTION zsd_get_hedge_merc_inter_insum.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  DATUM
*"     VALUE(I_DATA_FIM) TYPE  DATUM
*"  TABLES
*"      ET_ZSDT0094 STRUCTURE  ZSDT0094
*"----------------------------------------------------------------------

  DATA: lr_range_data TYPE RANGE OF datum.

  CHECK i_data_ini IS NOT INITIAL.

  APPEND VALUE #( sign = 'I'
                  option = 'BT'
                  low  = i_data_ini
                  high = i_data_fim ) TO lr_range_data.

  SELECT *
    FROM zsdt0094
    INTO TABLE et_zsdt0094
    WHERE data_registro IN lr_range_data.


ENDFUNCTION.
