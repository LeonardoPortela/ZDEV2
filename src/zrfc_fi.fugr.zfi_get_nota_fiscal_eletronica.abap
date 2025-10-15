FUNCTION zfi_get_nota_fiscal_eletronica.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  J_1BCREDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  J_1BCREDAT OPTIONAL
*"  TABLES
*"      ET_J_1BNFE_ACTIVE STRUCTURE  J_1BNFE_ACTIVE
*"----------------------------------------------------------------------


  DATA: lt_range_data      TYPE RANGE OF j_1bnfe_active-credat.

  CHECK i_data_ini IS NOT INITIAL.

  IF i_data_ini IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I'
                             option = 'BT'
                             low  = i_data_ini
                             high = i_data_fim ) TO lt_range_data.
  ENDIF.

  SELECT *
    FROM j_1bnfe_active
    INTO TABLE et_j_1bnfe_active
    WHERE credat IN lt_range_data.


  SELECT *
    FROM j_1bnfe_active
    APPENDING TABLE et_j_1bnfe_active
    WHERE action_date IN lt_range_data.

  SORT et_j_1bnfe_active BY docnum.
  DELETE ADJACENT DUPLICATES FROM et_j_1bnfe_active COMPARING docnum.


ENDFUNCTION.
