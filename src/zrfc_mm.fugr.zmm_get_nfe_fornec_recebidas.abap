FUNCTION zmm_get_nfe_fornec_recebidas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  DATUM
*"     VALUE(I_DATA_FIM) TYPE  DATUM
*"  TABLES
*"      ET_ZIB_NFE_FORN STRUCTURE  ZIB_NFE_FORN
*"----------------------------------------------------------------------

  DATA: lr_range_data TYPE RANGE OF datum.

  CHECK i_data_ini IS NOT INITIAL.


  APPEND VALUE #(  sign = 'I'
                   option = 'BT'
                   low  = i_data_ini
                   high = i_data_fim ) TO lr_range_data.

  SELECT *
    FROM zib_nfe_forn
    INTO TABLE et_zib_nfe_forn
    WHERE dt_atualizacao IN lr_range_data.




ENDFUNCTION.
