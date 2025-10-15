FUNCTION zles_get_controle_veiculos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  DATUM
*"     VALUE(I_DATA_FIM) TYPE  DATUM
*"  TABLES
*"      ET_ZLEST0002 STRUCTURE  ZLEST0002
*"----------------------------------------------------------------------

  DATA: lr_range_data TYPE RANGE OF datum.

  CHECK i_data_ini IS NOT INITIAL.

  APPEND VALUE #( sign = 'I'
                  option = 'BT'
                  low    = i_data_ini
                  high   = i_data_fim ) TO lr_range_data.
  SELECT *
    FROM zlest0002
    INTO TABLE et_zlest0002
    WHERE erdat IN lr_range_data.

  SELECT *
    FROM zlest0002
    APPENDING TABLE et_zlest0002
    WHERE dt_modificacao IN lr_range_data.

  SORT et_zlest0002 BY pc_veiculo.
  DELETE ADJACENT DUPLICATES FROM et_zlest0002 COMPARING pc_veiculo.



ENDFUNCTION.
