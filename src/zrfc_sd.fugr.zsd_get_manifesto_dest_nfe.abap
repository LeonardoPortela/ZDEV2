FUNCTION zsd_get_manifesto_dest_nfe.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  DATUM
*"     VALUE(I_DATA_FIM) TYPE  DATUM
*"  TABLES
*"      ET_ZSDT0127 STRUCTURE  ZSDT0127
*"----------------------------------------------------------------------

  DATA: lr_range_data TYPE RANGE OF datum.

  CHECK i_data_ini IS NOT INITIAL.

  APPEND VALUE #(  sign = 'I'
                   option = 'BT'
                   low  = i_data_ini
                   high = i_data_fim ) TO lr_range_data.

  SELECT *
    FROM zsdt0127
    INTO TABLE et_zsdt0127
    WHERE data_emi IN lr_range_data.

  SELECT *
   FROM zsdt0127
   APPENDING TABLE et_zsdt0127
   WHERE dt_atualizado IN lr_range_data.

  SORT et_zsdt0127 BY chave doc_manifesto.
  DELETE ADJACENT DUPLICATES FROM et_zsdt0127 COMPARING chave doc_manifesto.

ENDFUNCTION.
