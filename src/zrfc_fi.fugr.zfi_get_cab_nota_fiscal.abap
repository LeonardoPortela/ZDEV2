FUNCTION zfi_get_cab_nota_fiscal.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  J_1BCREDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  J_1BCREDAT OPTIONAL
*"  TABLES
*"      ET_J_1BNFDOC STRUCTURE  J_1BNFDOC
*"----------------------------------------------------------------------


  DATA: lt_range_data TYPE RANGE OF j_1bnfdoc-credat.

  CHECK i_data_ini IS NOT INITIAL.

  APPEND VALUE #(  sign = 'I'
                           option = 'BT'
                           low  = i_data_ini
                           high = i_data_fim ) TO lt_range_data.

  SELECT *
    FROM j_1bnfdoc
    INTO TABLE et_j_1bnfdoc
    WHERE credat IN lt_range_data.

  SELECT *
    FROM j_1bnfdoc
    APPENDING TABLE et_j_1bnfdoc
    WHERE chadat IN lt_range_data.

  SORT et_j_1bnfdoc BY docnum.
  DELETE ADJACENT DUPLICATES FROM et_j_1bnfdoc COMPARING docnum.
ENDFUNCTION.
