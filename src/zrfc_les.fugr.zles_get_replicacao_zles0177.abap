FUNCTION zles_get_replicacao_zles0177.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  DATUM
*"     VALUE(I_DATA_FIM) TYPE  DATUM
*"  TABLES
*"      ET_ZLEST0227 STRUCTURE  ZLEST0227
*"----------------------------------------------------------------------
  DATA: lr_range_data TYPE RANGE OF datum.

  CHECK i_data_ini IS NOT INITIAL.

  APPEND VALUE #( sign = 'I'
                  option = 'BT'
                  low    = i_data_ini
                  high   = i_data_fim ) TO lr_range_data.

  SELECT *
    FROM zlest0227
    INTO TABLE et_zlest0227
    WHERE data_modificacao IN lr_range_data.

ENDFUNCTION.
