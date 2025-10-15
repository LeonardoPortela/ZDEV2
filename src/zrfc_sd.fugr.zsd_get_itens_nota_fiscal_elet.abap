FUNCTION zsd_get_itens_nota_fiscal_elet.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  DATUM
*"     VALUE(I_DATA_FIM) TYPE  DATUM
*"  TABLES
*"      ET_ZIB_NFE_DIST_ITM STRUCTURE  ZIB_NFE_DIST_ITM
*"----------------------------------------------------------------------

  DATA: lr_range_data TYPE RANGE OF datum.

  CHECK i_data_ini IS NOT INITIAL.

  APPEND VALUE #( sign = 'I'
                  option = 'BT'
                  low  = i_data_ini
                  high = i_data_fim ) TO lr_range_data.

  SELECT *
    FROM zib_nfe_dist_itm
    INTO TABLE et_zib_nfe_dist_itm
    WHERE dt_atualizacao IN lr_range_data.


ENDFUNCTION.
