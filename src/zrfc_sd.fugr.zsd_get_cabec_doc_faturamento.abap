FUNCTION zsd_get_cabec_doc_faturamento .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      ET_VBRK STRUCTURE  VBRK
*"----------------------------------------------------------------------
  DATA:
    lt_range_data TYPE RANGE OF vbrk-erdat.

  CHECK i_data_ini IS NOT INITIAL.

  APPEND VALUE #(  sign = 'I'
                           option = 'BT'
                           low  = i_data_ini
                           high = i_data_fim ) TO lt_range_data.


  SELECT *
    FROM vbrk
    INTO TABLE et_vbrk
    WHERE erdat IN lt_range_data.

  SELECT *
    FROM vbrk
    APPENDING TABLE et_vbrk
    WHERE aedat IN lt_range_data.

  SORT et_vbrk BY vbeln.
  DELETE ADJACENT DUPLICATES FROM et_vbrk COMPARING vbeln.

ENDFUNCTION.
